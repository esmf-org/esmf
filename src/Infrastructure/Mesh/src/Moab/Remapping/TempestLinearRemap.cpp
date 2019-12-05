///////////////////////////////////////////////////////////////////////////////
///
/// \file    TempestLinearRemap.cpp
/// \author  Vijay Mahadevan
/// \version Mar 08, 2017
///

#include "Announce.h"
#include "DataMatrix3D.h"
#include "FiniteElementTools.h"
// #include "LinearRemapFV.h"
#include "GaussLobattoQuadrature.h"
#include "TriangularQuadrature.h"
#include "MeshUtilitiesFuzzy.h"
#include "MeshUtilitiesExact.h"
#include "MathHelper.h"
#include "SparseMatrix.h"
#include "OverlapMesh.h"

#include "moab/Remapping/TempestOfflineMap.hpp"
#include "DebugOutput.hpp"

#include "netcdfcpp.h"

#ifdef MOAB_HAVE_EIGEN
#include <Eigen/Dense>
#endif

#include <fstream>
#include <cmath>
#include <cstdlib>
#include <sstream>

// #define VERBOSE

// TODO: Replace these with the LAPACK wrappers once we have the Eigen-External-Dep branch merged in
extern "C" {
    /// General matrix solver from CLAPACK
    int dgesv_ (
        int * n,
        int * nrhs,
        double * a,
        int * lda,
        int * ipiv,
        double * b,
        int * ldb,
        int * info );

    /// Compute the QR decomposition from CLAPACK
    int dgeqrf_ (
        int * m,
        int * n,
        double * a,
        int * lda,
        double * tau,
        double * work,
        int * lwork,
        int * info );

    // Compute the matrix Q from DGEQRF from CLAPACK
    int dorgqr_ (
        int * m,
        int * n,
        int * k,
        double * a,
        int * lda,
        double * tau,
        double * work,
        int * lwork,
        int * info );

    /// Compute an LU Factorization from CLAPACK
    int dgetrf_ (
        int * m,
        int * n,
        double * a,
        int * lda,
        int * ipiv,
        int * info );

    /// Compute the matrix inverse from the LU factorization from CLAPACK
    int dgetri_ (
        int * n,
        double * a,
        int * lda,
        int * ipiv,
        double * work,
        int * lwork,
        int * info );

    /// Matrix vector multiply
    int dgemv_ (
        char * trans,
        int * m,
        int * n,
        double * alpha,
        double * a,
        int * lda,
        double * x,
        int * incx,
        double * beta,
        double * y,
        int * incy );

    /// Symmetric positive definite matrix solver from CLAPACK
    int dposv_ (
        char * uplo,
        int * n,
        int * nrhs,
        double * a,
        int * lda,
        double * b,
        int * ldb,
        int * info );

    /// Symmetric matrix solver from CLAPACK
    int dsysv_ (
        char * uplo,
        int * n,
        int * nrhs,
        double * a,
        int * lda,
        int * ipiv,
        double * b,
        int * ldb,
        double * work,
        int * lwork,
        int * info );

    /// Constrained least squares solver
    int dgglse_ (
        int * m,
        int * n,
        int * p,
        double * a,
        int * lda,
        double * b,
        int * ldb,
        double * c,
        double * d,
        double * x,
        double * work,
        int * lwork,
        int * info );

    // Unconstrained least squares solve (used for pseudoinverse)
    int dgelss_ (
        int * m,
        int * n,
        int * nrhs,
        double * a,
        int * lda,
        double * b,
        int * ldb,
        double * s,
        double * rcond,
        int * rank,
        double * work,
        int * lwork,
        int * info );
}

extern void BuildIntegrationArray (
    const Mesh & m_meshInput,
    const Mesh & m_meshOverlap,
    const TriangularQuadratureRule & triquadrule,
    int ixFirstFace,
    int ixOverlapBegin,
    int ixOverlapEnd,
    int nOrder,
    DataMatrix<double> & dIntArray
);

extern void InvertFitArray_Corrected (
    const DataVector<double> & dConstraint,
    DataMatrix<double> & dFitArray,
    DataVector<double> & dFitWeights,
    DataMatrix<double> & dFitArrayPlus
);

/// <summary>
///     Face index and distance metric pair.
/// </summary>
typedef std::pair<int, int> FaceDistancePair;

/// <summary>
///     Vector storing adjacent Faces.
/// </summary>
typedef std::vector<FaceDistancePair> AdjacentFaceVector;

extern void BuildFitArray (
    const Mesh & mesh,
    const TriangularQuadratureRule & triquadrule,
    int ixFirst,
    const AdjacentFaceVector & vecAdjFaces,
    int nOrder,
    int nFitWeightsExponent,
    const DataVector<double> & dConstraint,
    DataMatrix<double> & dFitArray,
    DataVector<double> & dFitWeights
);

extern void GetAdjacentFaceVectorByEdge (
    const Mesh & mesh,
    int iFaceInitial,
    int nRequiredFaceSetSize,
    AdjacentFaceVector & vecFaces
);

///////////////////////////////////////////////////////////////////////////////

// TODO: Can easily optimize this with a MOAB call
static Node GetReferenceNode_MOAB (
    const Face & face,
    const NodeVector & nodes
)
{
    Node nodeRef;

    //nodeRef = nodes[face[0]];

    nodeRef.x = 0.0;
    nodeRef.y = 0.0;
    nodeRef.z = 0.0;

    for ( size_t i = 0; i < face.edges.size(); i++ )
    {
        nodeRef.x += nodes[face[i]].x;
        nodeRef.y += nodes[face[i]].y;
        nodeRef.z += nodes[face[i]].z;
    }
    nodeRef.x /= static_cast<double> ( face.edges.size() );
    nodeRef.y /= static_cast<double> ( face.edges.size() );
    nodeRef.z /= static_cast<double> ( face.edges.size() );

    /*
        double dMag = sqrt(
              nodeRef.x * nodeRef.x
            + nodeRef.y * nodeRef.y
            + nodeRef.z * nodeRef.z);

        nodeRef.x /= dMag;
        nodeRef.y /= dMag;
        nodeRef.z /= dMag;
    */
    return nodeRef;
}

///////////////////////////////////////////////////////////////////////////////

void moab::TempestOfflineMap::LinearRemapFVtoFV_Tempest_MOAB (
    int nOrder
)
{
    // Order of triangular quadrature rule
    const int TriQuadRuleOrder = 4;

    // Verify ReverseNodeArray has been calculated
    if ( m_meshInputCov->revnodearray.size() == 0 )
    {
        _EXCEPTIONT ( "ReverseNodeArray has not been calculated for m_meshInput" );
    }

    // Triangular quadrature rule
    TriangularQuadratureRule triquadrule ( TriQuadRuleOrder );

    // Number of coefficients needed at this order
#ifdef RECTANGULAR_TRUNCATION
    int nCoefficients = nOrder * nOrder;
#endif
#ifdef TRIANGULAR_TRUNCATION
    int nCoefficients = nOrder * ( nOrder + 1 ) / 2;
#endif

    // Number of faces you need
    const int nRequiredFaceSetSize = nCoefficients;

    // Fit weight exponent
    const int nFitWeightsExponent = nOrder + 2;

    // Announcemnets
    if ( is_root )
    {
        Announce ( "[moab::TempestOfflineMap::LinearRemapFVtoFV_Tempest_MOAB] Finite Volume to Finite Volume Projection" );
        Announce ( "Triangular quadrature rule order %i", TriQuadRuleOrder );
        Announce ( "Number of coefficients: %i", nCoefficients );
        Announce ( "Required adjacency set size: %i", nRequiredFaceSetSize );
        Announce ( "Fit weights exponent: %i", nFitWeightsExponent );
    }

    // Current overlap face
    int ixOverlap = 0;

#ifdef VERBOSE
    const unsigned outputFrequency = (m_meshInputCov->faces.size()/10);
#endif
    // Loop through all faces on m_meshInput
    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
#ifdef VERBOSE
        // Output every 1000 elements
        if ( ixFirst % outputFrequency == 0 )
        {
            Announce ( "Element %i/%i", ixFirst, m_meshInputCov->faces.size() );
        }
#endif
        // This Face

        // Find the set of Faces that overlap faceFirst
        int ixOverlapBegin = ixOverlap;
        unsigned ixOverlapEnd = ixOverlapBegin;

        for ( ; ixOverlapEnd < m_meshOverlap->faces.size(); ixOverlapEnd++ )
        {
            if ( ixFirst - m_meshOverlap->vecSourceFaceIx[ixOverlapEnd] != 0 )
            {
                break;
            }
        }

        unsigned nOverlapFaces = ixOverlapEnd - ixOverlapBegin;
        // if ( is_root ) Announce ( "Element %i / %i :: [%i, %i]", ixFirst, m_meshInputCov->faces.size(), ixOverlapBegin, ixOverlapEnd );

        if ( nOverlapFaces == 0 ) continue;

        // Build integration array
        DataMatrix<double> dIntArray;

        BuildIntegrationArray (
            *m_meshInputCov,
            *m_meshOverlap,
            triquadrule,
            ixFirst,
            ixOverlapBegin,
            ixOverlapEnd,
            nOrder,
            dIntArray );

        // Set of Faces to use in building the reconstruction and associated
        // distance metric.
        AdjacentFaceVector vecAdjFaces;

        GetAdjacentFaceVectorByEdge (
            *m_meshInputCov,
            ixFirst,
            nRequiredFaceSetSize,
            vecAdjFaces );

        // Number of adjacent Faces
        int nAdjFaces = vecAdjFaces.size();

        // Determine the conservative constraint equation
        DataVector<double> dConstraint;

        dConstraint.Initialize ( nCoefficients );

        double dFirstArea = m_meshInputCov->vecFaceArea[ixFirst];

        for ( int p = 0; p < nCoefficients; p++ )
        {
            for ( unsigned j = 0; j < nOverlapFaces; j++ )
            {
                dConstraint[p] += dIntArray[p][j];
            }
            dConstraint[p] /= dFirstArea;
        }

        // Build the fit array from the integration operator
        DataMatrix<double> dFitArray;
        DataVector<double> dFitWeights;
        DataMatrix<double> dFitArrayPlus;

        BuildFitArray (
            *m_meshInputCov,
            triquadrule,
            ixFirst,
            vecAdjFaces,
            nOrder,
            nFitWeightsExponent,
            dConstraint,
            dFitArray,
            dFitWeights
        );

        // Compute the inverse fit array
        InvertFitArray_Corrected (
            dConstraint,
            dFitArray,
            dFitWeights,
            dFitArrayPlus
        );

        // Multiply integration array and fit array
        DataMatrix<double> dComposedArray;
        dComposedArray.Initialize ( nAdjFaces, nOverlapFaces );

        for ( int i = 0; i < nAdjFaces; i++ )
        {
            for ( unsigned j = 0; j < nOverlapFaces; j++ )
            {
                for ( int k = 0; k < nCoefficients; k++ )
                {
                    dComposedArray[i][j] += dIntArray[k][j] * dFitArrayPlus[i][k];
                }
            }
        }

        // Put composed array into map
        for ( unsigned i = 0; i < vecAdjFaces.size(); i++ )
        {
            for ( unsigned j = 0; j < nOverlapFaces; j++ )
            {
                int& ixFirstFaceLoc = vecAdjFaces[i].first;
                int& ixSecondFaceLoc = m_meshOverlap->vecTargetFaceIx[ixOverlap + j];
                // int ixFirstFaceGlob = m_remapper->GetGlobalID(moab::Remapper::SourceMesh, ixFirstFaceLoc);
                // int ixSecondFaceGlob = m_remapper->GetGlobalID(moab::Remapper::TargetMesh, ixSecondFaceLoc);

                m_mapRemap ( ixSecondFaceLoc, ixFirstFaceLoc ) +=
                    dComposedArray[i][j]
                    / m_meshOutput->vecFaceArea[ixSecondFaceLoc];
            }
        }

        // Increment the current overlap index
        ixOverlap += nOverlapFaces;
    }

    return;
}


#ifdef MOAB_HAVE_EIGEN
void moab::TempestOfflineMap::CopyTempestSparseMat_Eigen()
{
    m_weightMatrix.resize(m_nTotDofs_Dest, m_nTotDofs_SrcCov);
    InitVectors();

#ifdef VERBOSE
    int locrows = std::max(m_mapRemap.GetRows(), m_nTotDofs_Dest);
    int loccols = std::max(m_mapRemap.GetColumns(), m_nTotDofs_SrcCov);

    std::cout << m_weightMatrix.rows() << ", " <<  locrows << ", " <<  m_weightMatrix.cols() << ", " << loccols << "\n";
    // assert(m_weightMatrix.rows() == locrows && m_weightMatrix.cols() == loccols);
#endif

    DataVector<int> lrows;
    DataVector<int> lcols;
    DataVector<double> lvals;
    m_mapRemap.GetEntries(lrows, lcols, lvals);
    unsigned locvals = lvals.GetRows();

    m_weightMatrix.reserve(locvals);
    for (unsigned iv=0; iv < locvals; iv++) {
        m_weightMatrix.insert(lrows[iv], lcols[iv]) = lvals[iv];
    }

    m_weightMatrix.makeCompressed();

#ifdef VERBOSE
    std::stringstream sstr;
    sstr << "tempestmatrix.txt.0000" << rank;
    std::ofstream output_file ( sstr.str(), std::ios::out );
    output_file << "0 " << locrows << " 0 " << loccols << "\n";
    for (unsigned iv=0; iv < locvals; iv++) {
        // output_file << lrows[iv] << " " << row_ldofmap[lrows[iv]] << " " << row_gdofmap[row_ldofmap[lrows[iv]]] << " " << col_gdofmap[col_ldofmap[lcols[iv]]] << " " << lvals[iv] << "\n";
        output_file << row_gdofmap[row_ldofmap[lrows[iv]]] << " " << col_gdofmap[col_ldofmap[lcols[iv]]] << " " << lvals[iv] << "\n";
        
    }
    output_file.flush(); // required here
    output_file.close();
#endif

    return;
}

///////////////////////////////////////////////////////////////////////////////

// #define IO_USE_PARALLEL_NETCDF
void moab::TempestOfflineMap::WriteParallelWeightsToFile(std::string strFilename)
{
    // m_weightMatrix.Print(filename.c_str(), 0, 0);

#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiFile ncMap(MPI_COMM_WORLD, strFilename.c_str(), NcmpiFile::Replace, NcmpiFile::classic5);
#else
    NcFile ncMap(strFilename.c_str(), NcFile::Replace);
#endif

    if (!ncMap.is_valid()) {
        _EXCEPTION1("Unable to open output map file \"%s\"",
            strFilename.c_str());
    }

    // Attributes
    ncMap.add_att("Title", "MOAB-TempestRemap Online Regridding Weight Generator");

    // Map dimensions
    unsigned nA = (m_dSourceAreas.GetRows());
    unsigned nB = (m_dTargetAreas.GetRows());

    // Write output dimensions entries
    unsigned nSrcGridDims = (m_vecSourceDimSizes.size());
    unsigned nDstGridDims = (m_vecTargetDimSizes.size());

#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiDim * dimSrcGridRank = ncMap.addDim("src_grid_rank", nSrcGridDims);
    NcmpiDim * dimDstGridRank = ncMap.addDim("dst_grid_rank", nDstGridDims);
#else
    NcDim * dimSrcGridRank = ncMap.add_dim("src_grid_rank", nSrcGridDims);
    NcDim * dimDstGridRank = ncMap.add_dim("dst_grid_rank", nDstGridDims);
#endif

#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiVar * varSrcGridDims =
        ncMap.addVar("src_grid_dims", ncmpiInt, dimSrcGridRank);
    NcmpiVar * varDstGridDims =
        ncMap.addVar("dst_grid_dims", ncmpiInt, dimDstGridRank);
#else
    NcVar * varSrcGridDims =
        ncMap.add_var("src_grid_dims", ncInt, dimSrcGridRank);
    NcVar * varDstGridDims =
        ncMap.add_var("dst_grid_dims", ncInt, dimDstGridRank);
#endif

    char szDim[64];
    if ((nSrcGridDims == 1) && (m_vecSourceDimSizes[0] != (int)nA)) {
        int tmp = (int)(nA);
        varSrcGridDims->put(&tmp, 1);
        varSrcGridDims->add_att("name0", "num_dof");

    } else {
        for (unsigned i = 0; i < m_vecSourceDimSizes.size(); i++) {
            varSrcGridDims->set_cur(nSrcGridDims - i - 1);
            varSrcGridDims->put(&(m_vecSourceDimSizes[i]), 1);
        }

        for (unsigned i = 0; i < m_vecSourceDimSizes.size(); i++) {
            sprintf(szDim, "name%i", i);
            varSrcGridDims->add_att(szDim,
                m_vecSourceDimNames[nSrcGridDims - i - 1].c_str());
        }
    }

    if ((nDstGridDims == 1) && (m_vecTargetDimSizes[0] != (int)nB)) {
        int tmp = (int)(nB);
        varDstGridDims->put(&tmp, 1);
        varDstGridDims->add_att("name0", "num_dof");
    } else {
        for (unsigned i = 0; i < m_vecTargetDimSizes.size(); i++) {
            varDstGridDims->set_cur(nDstGridDims - i - 1);
            varDstGridDims->put(&(m_vecTargetDimSizes[i]), 1);
        }

        for (unsigned i = 0; i < m_vecTargetDimSizes.size(); i++) {
            sprintf(szDim, "name%i", i);
            varDstGridDims->add_att(szDim,
                m_vecTargetDimNames[nDstGridDims - i - 1].c_str());
        }
    }

    // Source and Target mesh resolutions
#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiDim * dimNA = ncMap.addDim("n_a", nA);
    NcmpiDim * dimNB = ncMap.addDim("n_b", nB);
#else
    NcDim * dimNA = ncMap.add_dim("n_a", nA);
    NcDim * dimNB = ncMap.add_dim("n_b", nB);
#endif

    // Number of nodes per Face
    int nSourceNodesPerFace = m_dSourceVertexLon.GetColumns();
    int nTargetNodesPerFace = m_dTargetVertexLon.GetColumns();

#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiDim * dimNVA = ncMap.addDim("nv_a", nSourceNodesPerFace);
    NcmpiDim * dimNVB = ncMap.addDim("nv_b", nTargetNodesPerFace);
#else
    NcDim * dimNVA = ncMap.add_dim("nv_a", nSourceNodesPerFace);
    NcDim * dimNVB = ncMap.add_dim("nv_b", nTargetNodesPerFace);
#endif

    // Write coordinates
#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiVar * varYCA = ncMap.addVar("yc_a", ncmpiDouble, dimNA);
    NcmpiVar * varYCB = ncMap.addVar("yc_b", ncmpiDouble, dimNB);

    NcmpiVar * varXCA = ncMap.addVar("xc_a", ncmpiDouble, dimNA);
    NcmpiVar * varXCB = ncMap.addVar("xc_b", ncmpiDouble, dimNB);

    NcmpiVar * varYVA = ncMap.addVar("yv_a", ncmpiDouble, dimNA, dimNVA);
    NcmpiVar * varYVB = ncMap.addVar("yv_b", ncmpiDouble, dimNB, dimNVB);

    NcmpiVar * varXVA = ncMap.addVar("xv_a", ncmpiDouble, dimNA, dimNVA);
    NcmpiVar * varXVB = ncMap.addVar("xv_b", ncmpiDouble, dimNB, dimNVB);
#else
    NcVar * varYCA = ncMap.add_var("yc_a", ncDouble, dimNA);
    NcVar * varYCB = ncMap.add_var("yc_b", ncDouble, dimNB);

    NcVar * varXCA = ncMap.add_var("xc_a", ncDouble, dimNA);
    NcVar * varXCB = ncMap.add_var("xc_b", ncDouble, dimNB);

    NcVar * varYVA = ncMap.add_var("yv_a", ncDouble, dimNA, dimNVA);
    NcVar * varYVB = ncMap.add_var("yv_b", ncDouble, dimNB, dimNVB);

    NcVar * varXVA = ncMap.add_var("xv_a", ncDouble, dimNA, dimNVA);
    NcVar * varXVB = ncMap.add_var("xv_b", ncDouble, dimNB, dimNVB);
#endif

    varYCA->add_att("units", "degrees");
    varYCB->add_att("units", "degrees");

    varXCA->add_att("units", "degrees");
    varXCB->add_att("units", "degrees");

    varYVA->add_att("units", "degrees");
    varYVB->add_att("units", "degrees");

    varXVA->add_att("units", "degrees");
    varXVB->add_att("units", "degrees");

    // Verify dimensionality
    if (m_dSourceCenterLon.GetRows() != nA) {
        _EXCEPTIONT("Mismatch between m_dSourceCenterLon and nA");
    }
    if (m_dSourceCenterLat.GetRows() != nA) {
        _EXCEPTIONT("Mismatch between m_dSourceCenterLat and nA");
    }
    if (m_dTargetCenterLon.GetRows() != nB) {
        _EXCEPTIONT("Mismatch between m_dTargetCenterLon and nB");
    }
    if (m_dTargetCenterLat.GetRows() != nB) {
        _EXCEPTIONT("Mismatch between m_dTargetCenterLat and nB");
    }
    if (m_dSourceVertexLon.GetRows() != nA) {
        _EXCEPTIONT("Mismatch between m_dSourceVertexLon and nA");
    }
    if (m_dSourceVertexLat.GetRows() != nA) {
        _EXCEPTIONT("Mismatch between m_dSourceVertexLat and nA");
    }
    if (m_dTargetVertexLon.GetRows() != nB) {
        _EXCEPTIONT("Mismatch between m_dTargetVertexLon and nB");
    }
    if (m_dTargetVertexLat.GetRows() != nB) {
        _EXCEPTIONT("Mismatch between m_dTargetVertexLat and nB");
    }

    varYCA->put(&(m_dSourceCenterLat[0]), nA);
    varYCB->put(&(m_dTargetCenterLat[0]), nB);

    varXCA->put(&(m_dSourceCenterLon[0]), nA);
    varXCB->put(&(m_dTargetCenterLon[0]), nB);

    varYVA->put(&(m_dSourceVertexLat[0][0]), nA, nSourceNodesPerFace);
    varYVB->put(&(m_dTargetVertexLat[0][0]), nB, nTargetNodesPerFace);

    varXVA->put(&(m_dSourceVertexLon[0][0]), nA, nSourceNodesPerFace);
    varXVB->put(&(m_dTargetVertexLon[0][0]), nB, nTargetNodesPerFace);

    // Write vector centers
    if ((m_dVectorTargetCenterLat.GetRows() != 0) &&
        (m_dVectorTargetCenterLon.GetRows() != 0)
    ) {
        NcDim * dimLatB =
            ncMap.add_dim("lat_b", m_dVectorTargetCenterLat.GetRows());
        NcDim * dimLonB =
            ncMap.add_dim("lon_b", m_dVectorTargetCenterLon.GetRows());

        NcVar * varLatCB = ncMap.add_var("latc_b", ncDouble, dimLatB);
        NcVar * varLonCB = ncMap.add_var("lonc_b", ncDouble, dimLonB);

        varLatCB->put(&(m_dVectorTargetCenterLat[0]), dimLatB->size());
        varLonCB->put(&(m_dVectorTargetCenterLon[0]), dimLonB->size());

        NcDim * dimBounds = ncMap.add_dim("bnds", 2);
        NcVar * varLatBounds =
            ncMap.add_var("lat_bnds", ncDouble, dimLatB, dimBounds);
        NcVar * varLonBounds =
            ncMap.add_var("lon_bnds", ncDouble, dimLonB, dimBounds);

        varLatBounds->put(&(m_dVectorTargetBoundsLat[0][0]),
            m_dVectorTargetBoundsLat.GetRows(), 2);
        varLonBounds->put(&(m_dVectorTargetBoundsLon[0][0]),
            m_dVectorTargetBoundsLon.GetRows(), 2);
    }

    // Write areas
#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiVar * varAreaA = ncMap.addVar("area_a", ncmpiDouble, dimNA);
#else
    NcVar * varAreaA = ncMap.add_var("area_a", ncDouble, dimNA);
#endif
    varAreaA->put(&(m_dSourceAreas[0]), nA);

#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiVar * varAreaB = ncMap.addVar("area_b", ncmpiDouble, dimNB);
#else
    NcVar * varAreaB = ncMap.add_var("area_b", ncDouble, dimNB);
#endif
    varAreaB->put(&(m_dTargetAreas[0]), nB);

    // Write frac
    DataVector<double> dFrac;

    dFrac.Initialize(nA);
    for (unsigned i = 0; i < nA; i++) {
        dFrac[i] = 1.0;
    }
#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiVar * varFracA = ncMap.addVar("frac_a", ncmpiDouble, dimNA);
#else
    NcVar * varFracA = ncMap.add_var("frac_a", ncDouble, dimNA);
#endif
    varFracA->put(&(dFrac[0]), nA);

    dFrac.Initialize(nB);
    for (unsigned i = 0; i < nB; i++) {
        dFrac[i] = 1.0;
    }
#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiVar * varFracB = ncMap.addVar("frac_b", ncmpiDouble, dimNB);
#else
    NcVar * varFracB = ncMap.add_var("frac_b", ncDouble, dimNB);
#endif
    varFracB->put(&(dFrac[0]), nB);

    // Write SparseMatrix entries
    int nS = m_weightMatrix.nonZeros();
    DataVector<int> vecRow(nS);
    DataVector<int> vecCol(nS);
    DataVector<double> vecS(nS);

    for (int i = 0; i < m_weightMatrix.outerSize(); i++) {
        for (WeightMatrix::InnerIterator it(m_weightMatrix,i); it; ++it) {
            vecRow[i] = 1+it.row(); // row index
            vecCol[i] = 1+it.col(); // col index
            vecS[i] = it.value();   // value
        }
    }

    // Load in data
#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiDim * dimNS = ncMap.add_dim("n_s", nS);
#else
    NcDim * dimNS = ncMap.add_dim("n_s", nS);
#endif

#ifdef IO_USE_PARALLEL_NETCDF
    NcmpiVar * varRow = ncMap.addVar("row", ncmpiInt, dimNS);
    NcmpiVar * varCol = ncMap.addVar("col", ncmpiInt, dimNS);
    NcmpiVar * varS = ncMap.addVar("S", ncmpiDouble, dimNS);
#else
    NcVar * varRow = ncMap.add_var("row", ncInt, dimNS);
    NcVar * varCol = ncMap.add_var("col", ncInt, dimNS);
    NcVar * varS = ncMap.add_var("S", ncDouble, dimNS);
#endif

    varRow->set_cur((long)0);
    varRow->put(&(vecRow[0]), nS);

    varCol->set_cur((long)0);
    varCol->put(&(vecCol[0]), nS);

    varS->set_cur((long)0);
    varS->put(&(vecS[0]), nS);

    // Add global attributes
    // std::map<std::string, std::string>::const_iterator iterAttributes =
    //     mapAttributes.begin();
    // for (; iterAttributes != mapAttributes.end(); iterAttributes++) {
    //     ncMap.add_att(
    //         iterAttributes->first.c_str(),
    //         iterAttributes->second.c_str());
    // }
}

///////////////////////////////////////////////////////////////////////////////

moab::ErrorCode moab::TempestOfflineMap::ApplyWeights (std::vector<double>& srcVals, std::vector<double>& tgtVals, bool transpose)
{
    // Reset the source and target data first
    m_rowVector.setZero();
    m_colVector.setZero();

#ifdef VERBOSE
    std::stringstream sstr;
    sstr << "projection_" << rank << ".txt";
    std::ofstream output_file ( sstr.str() );
#endif
    // Perform the actual projection of weights: application of weight matrix onto the source solution vector
    if (transpose) {
        // Permute the source data first
        for (unsigned i=0; i < srcVals.size(); ++i) {
            m_rowVector(row_dofmap[i]) = srcVals[i]; // permute and set the row (source) vector properly
        }
        
        m_colVector = m_weightMatrix.adjoint() * m_rowVector;

        // Permute the resulting target data back
        for (unsigned i=0; i < tgtVals.size(); ++i) {
            tgtVals[i] = m_colVector(col_dofmap[i]); // permute and set the row (source) vector properly
        }
    }
    else {
        // Permute the source data first
#ifdef VERBOSE
        output_file << "ColVector: " << m_colVector.size() << ", SrcVals: " << srcVals.size() << ", Sizes: " << m_nTotDofs_SrcCov << ", " << col_dofmap.size() << "\n";
#endif
        for (unsigned i=0; i < srcVals.size(); ++i) {
            assert(m_colVector.size()-col_dofmap[i]>0);
            m_colVector(col_dofmap[i]) = srcVals[i]; // permute and set the row (source) vector properly
#ifdef VERBOSE
            output_file << "Col: " << i << ", " << col_dofmap[i] << ", GID: " << col_gdofmap[i] << ", Data = " << srcVals[i]  << ", " << m_colVector(col_dofmap[i]) << "\n";
#endif
        }
        
        m_rowVector = m_weightMatrix * m_colVector;

        // Permute the resulting target data back
#ifdef VERBOSE
        output_file << "RowVector: " << m_rowVector.size() << ", TgtVals:" << tgtVals.size() << ", Sizes: " << m_nTotDofs_Dest << ", " << row_dofmap.size() << "\n";
#endif
        for (unsigned i=0; i < tgtVals.size(); ++i) {
            tgtVals[i] = m_rowVector(row_dofmap[i]); // permute and set the row (source) vector properly
#ifdef VERBOSE
            output_file << "Row: " << i << ", " << row_dofmap[i] << ", GID: " << row_gdofmap[i] << ", Data = " << m_rowVector(row_dofmap[i]) << "\n";
#endif
        }
    }

#ifdef VERBOSE
    output_file.flush(); // required here
    output_file.close();
#endif

    // All done with matvec application
    return moab::MB_SUCCESS;
}

#endif


///////////////////////////////////////////////////////////////////////////////

extern void ForceConsistencyConservation3(
    const DataVector<double> & vecSourceArea,
    const DataVector<double> & vecTargetArea,
    DataMatrix<double> & dCoeff,
    bool fMonotone
);

///////////////////////////////////////////////////////////////////////////////

extern void ForceIntArrayConsistencyConservation (
    const DataVector<double> & vecSourceArea,
    const DataVector<double> & vecTargetArea,
    DataMatrix<double> & dCoeff,
    bool fMonotone
);

///////////////////////////////////////////////////////////////////////////////

void moab::TempestOfflineMap::LinearRemapSE4_Tempest_MOAB (
    const DataMatrix3D<int> & dataGLLNodes,
    const DataMatrix3D<double> & dataGLLJacobian,
    int nMonotoneType,
    bool fContinuousIn,
    bool fNoConservation
)
{
    // Order of the polynomial interpolant
    int nP = dataGLLNodes.GetRows();

    // Order of triangular quadrature rule
    const int TriQuadRuleOrder = 4;

    // Triangular quadrature rule
    TriangularQuadratureRule triquadrule ( TriQuadRuleOrder );

    int TriQuadraturePoints = triquadrule.GetPoints();

    const DataMatrix<double> & TriQuadratureG = triquadrule.GetG();

    const DataVector<double> & TriQuadratureW = triquadrule.GetW();

    // Sample coefficients
    DataMatrix<double> dSampleCoeff;
    dSampleCoeff.Initialize ( nP, nP );

    // GLL Quadrature nodes on quadrilateral elements
    DataVector<double> dG;
    DataVector<double> dW;
    GaussLobattoQuadrature::GetPoints ( nP, 0.0, 1.0, dG, dW );

    // Announcemnets
    if ( is_root )
    {
        Announce ( "[moab::TempestOfflineMap::LinearRemapSE4_Tempest_MOAB] Finite Element to Finite Volume Projection" );
        Announce ( "Triangular quadrature rule order %i", TriQuadRuleOrder );
        Announce ( "Order of the FE polynomial interpolant: %i", nP );
    }

    // Get SparseMatrix represntation of the OfflineMap
    SparseMatrix<double> & smatMap = this->GetSparseMatrix();

    // NodeVector from m_meshOverlap
    const NodeVector & nodesOverlap = m_meshOverlap->nodes;
    const NodeVector & nodesFirst   = m_meshInputCov->nodes;

    // Vector of source areas
    DataVector<double> vecSourceArea;
    vecSourceArea.Initialize ( nP * nP );

    DataVector<double> vecTargetArea;
    DataMatrix<double> dCoeff;

#ifdef VERBOSE
    std::stringstream sstr;
    sstr << "remapdata_" << rank << ".txt";
    std::ofstream output_file ( sstr.str() );
#endif

    // Current Overlap Face
    int ixOverlap = 0;
#ifdef VERBOSE
    const unsigned outputFrequency = (m_meshInputCov->faces.size()/10);
#endif

    // Loop over all input Faces
    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
        const Face & faceFirst = m_meshInputCov->faces[ixFirst];

        if ( faceFirst.edges.size() != 4 )
        {
            _EXCEPTIONT ( "Only quadrilateral elements allowed for SE remapping" );
        }

#ifdef VERBOSE
        // Announce computation progress
        if ( ixFirst % outputFrequency == 0 )
        {
            Announce ( "Element %i/%i", ixFirst, m_meshInputCov->faces.size() );
        }
#endif

        // Need to re-number the overlap elements such that vecSourceFaceIx[a:b] = 0, then 1 and so on wrt the input mesh data
        // Then the overlap_end and overlap_begin will be correct. However, the relation with MOAB and Tempest will go out of the roof

        // Determine how many overlap Faces and triangles are present
        int nOverlapFaces = 0;
        size_t ixOverlapTemp = ixOverlap;
        for ( ; ixOverlapTemp < m_meshOverlap->faces.size(); ixOverlapTemp++ )
        {
            // const Face & faceOverlap = m_meshOverlap->faces[ixOverlapTemp];
            if ( ixFirst - m_meshOverlap->vecSourceFaceIx[ixOverlapTemp] != 0 )
            {
                break;
            }

            nOverlapFaces++;
        }

        // No overlaps
        if ( nOverlapFaces == 0 )
        {
            continue;
        }

        // Allocate remap coefficients array for meshFirst Face
        DataMatrix3D<double> dRemapCoeff;
        dRemapCoeff.Initialize ( nP, nP, nOverlapFaces );

        // Find the local remap coefficients
        for ( int j = 0; j < nOverlapFaces; j++ )
        {
            const Face & faceOverlap = m_meshOverlap->faces[ixOverlap + j];

// #ifdef VERBOSE
            // if ( is_root )
            //     Announce ( "\tLocal ID: %i/%i = %i, areas = %2.8e", j + ixOverlap, nOverlapFaces, m_remapper->lid_to_gid_covsrc[m_meshOverlap->vecSourceFaceIx[ixOverlap + j]], m_meshOverlap->vecFaceArea[ixOverlap + j] );
// #endif

            int nOverlapTriangles = faceOverlap.edges.size() - 2;

// #define USE_MININDEX

#ifdef USE_MININDEX
            // first find out the minimum node, start there the triangle decomposition
            int minIndex = 0;
            int nnodes = faceOverlap.edges.size();
            for (int j1=1; j1<nnodes; j1++)
            {
              if ( nodesOverlap[faceOverlap[j1]] < nodesOverlap[faceOverlap[minIndex]] )
              {
                minIndex = j1;
              }
            }
#endif

            // Loop over all sub-triangles of this Overlap Face
            for ( int k = 0; k < nOverlapTriangles; k++ )
            {
#ifdef USE_MININDEX
                // Cornerpoints of triangle, they start at the minimal Node, for consistency
                const Node & node0 = nodesOverlap[faceOverlap[minIndex]];
                const Node & node1 = nodesOverlap[faceOverlap[(minIndex + k + 1)%nnodes]];
                const Node & node2 = nodesOverlap[faceOverlap[(minIndex + k + 2)%nnodes]];

                // Calculate the area of the modified Face
                Face faceTri ( 3 );
                faceTri.SetNode ( 0, faceOverlap[minIndex] );
                faceTri.SetNode ( 1, faceOverlap[(minIndex + k + 1)%nnodes] );
                faceTri.SetNode ( 2, faceOverlap[(minIndex + k + 2)%nnodes] );
#else
                // Cornerpoints of triangle
                const Node & node0 = nodesOverlap[faceOverlap[0]];
                const Node & node1 = nodesOverlap[faceOverlap[k+1]];
                const Node & node2 = nodesOverlap[faceOverlap[k+2]];

                // Calculate the area of the modified Face
                Face faceTri(3);
                faceTri.SetNode(0, faceOverlap[0]);
                faceTri.SetNode(1, faceOverlap[k+1]);
                faceTri.SetNode(2, faceOverlap[k+2]);
#endif

                double dTriangleArea =
                    CalculateFaceArea ( faceTri, nodesOverlap );

                // Coordinates of quadrature Node
                for ( int l = 0; l < TriQuadraturePoints; l++ )
                {
                    Node nodeQuadrature;
                    nodeQuadrature.x =
                        TriQuadratureG[l][0] * node0.x
                        + TriQuadratureG[l][1] * node1.x
                        + TriQuadratureG[l][2] * node2.x;

                    nodeQuadrature.y =
                        TriQuadratureG[l][0] * node0.y
                        + TriQuadratureG[l][1] * node1.y
                        + TriQuadratureG[l][2] * node2.y;

                    nodeQuadrature.z =
                        TriQuadratureG[l][0] * node0.z
                        + TriQuadratureG[l][1] * node1.z
                        + TriQuadratureG[l][2] * node2.z;

                    double dMag = sqrt (
                                      nodeQuadrature.x * nodeQuadrature.x
                                      + nodeQuadrature.y * nodeQuadrature.y
                                      + nodeQuadrature.z * nodeQuadrature.z );

                    nodeQuadrature.x /= dMag;
                    nodeQuadrature.y /= dMag;
                    nodeQuadrature.z /= dMag;

                    // Find components of quadrature point in basis
                    // of the first Face
                    double dAlpha;
                    double dBeta;

                    ApplyInverseMap (
                        faceFirst,
                        nodesFirst,
                        nodeQuadrature,
                        dAlpha,
                        dBeta );

                    // Check inverse map value
                    if ( ( dAlpha < -1.0e-13 ) || ( dAlpha > 1.0 + 1.0e-13 ) ||
                            ( dBeta  < -1.0e-13 ) || ( dBeta  > 1.0 + 1.0e-13 )
                       )
                    {
                        _EXCEPTION2 ( "Inverse Map out of range (%1.5e %1.5e)",
                                      dAlpha, dBeta );
                    }

                    // Sample the finite element at this point
                    SampleGLLFiniteElement (
                        nMonotoneType,
                        nP,
                        dAlpha,
                        dBeta,
                        dSampleCoeff );

                    // Add sample coefficients to the map
                    for ( int p = 0; p < nP; p++ )
                    {
                        for ( int q = 0; q < nP; q++ )
                        {
                            dRemapCoeff[p][q][j] +=
                                TriQuadratureW[l]
                                * dTriangleArea
                                * dSampleCoeff[p][q]
                                / m_meshOverlap->vecFaceArea[ixOverlap + j];
                        }
                    }
                }
            }
        }

#ifdef VERBOSE
        output_file << "[" << m_remapper->lid_to_gid_covsrc[ixFirst] << "] \t";
        for ( int j = 0; j < nOverlapFaces; j++ )
        {
            for ( int p = 0; p < nP; p++ )
            {
                for ( int q = 0; q < nP; q++ )
                {
                    output_file << dRemapCoeff[p][q][j] << " ";
                }
            }
        }
        output_file << std::endl;
#endif

        // Force consistency and conservation
        if ( !fNoConservation )
        {
            double dTargetArea = 0.0;
            for ( int j = 0; j < nOverlapFaces; j++ )
            {
                dTargetArea += m_meshOverlap->vecFaceArea[ixOverlap + j];
            }

            for ( int p = 0; p < nP; p++ )
            {
                for ( int q = 0; q < nP; q++ )
                {
                    vecSourceArea[p * nP + q] = dataGLLJacobian[p][q][ixFirst];
                }
            }

            const double areaTolerance = 1e-10;
            // Source elements are completely covered by target volumes
            if ( fabs ( m_meshInputCov->vecFaceArea[ixFirst] - dTargetArea ) <= areaTolerance )
            {
                vecTargetArea.Initialize ( nOverlapFaces );
                for ( int j = 0; j < nOverlapFaces; j++ )
                {
                    vecTargetArea[j] = m_meshOverlap->vecFaceArea[ixOverlap + j];
                }

                dCoeff.Initialize ( nOverlapFaces, nP * nP );

                for ( int j = 0; j < nOverlapFaces; j++ )
                {
                    for ( int p = 0; p < nP; p++ )
                    {
                        for ( int q = 0; q < nP; q++ )
                        {
                            dCoeff[j][p * nP + q] = dRemapCoeff[p][q][j];
                        }
                    }
                }

                // Target volumes only partially cover source elements
            }
            else if ( m_meshInputCov->vecFaceArea[ixFirst] - dTargetArea > areaTolerance )
            {
                double dExtraneousArea = m_meshInputCov->vecFaceArea[ixFirst] - dTargetArea;

                vecTargetArea.Initialize ( nOverlapFaces + 1 );
                for ( int j = 0; j < nOverlapFaces; j++ )
                {
                    vecTargetArea[j] = m_meshOverlap->vecFaceArea[ixOverlap + j];
                }
                vecTargetArea[nOverlapFaces] = dExtraneousArea;

#ifdef VERBOSE
                Announce ( "Partial volume: %i (%1.10e / %1.10e)",
                           ixFirst, dTargetArea, m_meshInputCov->vecFaceArea[ixFirst] );
#endif
                if ( dTargetArea > m_meshInputCov->vecFaceArea[ixFirst] )
                {
                    _EXCEPTIONT ( "Partial element area exceeds total element area" );
                }

                dCoeff.Initialize ( nOverlapFaces + 1, nP * nP );

                for ( int j = 0; j < nOverlapFaces; j++ )
                {
                    for ( int p = 0; p < nP; p++ )
                    {
                        for ( int q = 0; q < nP; q++ )
                        {
                            dCoeff[j][p * nP + q] = dRemapCoeff[p][q][j];
                        }
                    }
                }
                for ( int p = 0; p < nP; p++ )
                {
                    for ( int q = 0; q < nP; q++ )
                    {
                        dCoeff[nOverlapFaces][p * nP + q] =
                            dataGLLJacobian[p][q][ixFirst];
                    }
                }
                for ( int j = 0; j < nOverlapFaces; j++ )
                {
                    for ( int p = 0; p < nP; p++ )
                    {
                        for ( int q = 0; q < nP; q++ )
                        {
                            dCoeff[nOverlapFaces][p * nP + q] -=
                                dRemapCoeff[p][q][j]
                                * m_meshOverlap->vecFaceArea[ixOverlap + j];
                        }
                    }
                }
                for ( int p = 0; p < nP; p++ )
                {
                    for ( int q = 0; q < nP; q++ )
                    {
                        dCoeff[nOverlapFaces][p * nP + q] /= dExtraneousArea;
                    }
                }

                // Source elements only partially cover target volumes
            }
            else
            {
                Announce ( "Coverage area: %1.10e, and target element area: %1.10e)",
                           ixFirst, m_meshInputCov->vecFaceArea[ixFirst], dTargetArea );
                _EXCEPTIONT ( "Target grid must be a subset of source grid" );
            }

            ForceConsistencyConservation3 (
                vecSourceArea,
                vecTargetArea,
                dCoeff,
                ( nMonotoneType > 0 )
                /*, m_remapper->lid_to_gid_covsrc[ixFirst]*/ );

            for ( int j = 0; j < nOverlapFaces; j++ )
            {
                for ( int p = 0; p < nP; p++ )
                {
                    for ( int q = 0; q < nP; q++ )
                    {
                        dRemapCoeff[p][q][j] = dCoeff[j][p * nP + q];
                    }
                }
            }
        }

#ifdef VERBOSE
        // output_file << "[" << m_remapper->lid_to_gid_covsrc[ixFirst] << "] \t";
        // for ( int j = 0; j < nOverlapFaces; j++ )
        // {
        //     for ( int p = 0; p < nP; p++ )
        //     {
        //         for ( int q = 0; q < nP; q++ )
        //         {
        //             output_file << dRemapCoeff[p][q][j] << " ";
        //         }
        //     }
        // }
        // output_file << std::endl;
#endif

        // Put these remap coefficients into the SparseMatrix map
        for ( int j = 0; j < nOverlapFaces; j++ )
        {
            int ixSecondFace = m_meshOverlap->vecTargetFaceIx[ixOverlap + j];
            if (ixSecondFace < 0) // signal to not participate, because it is a ghost target
              continue; // do not do anything
            for ( int p = 0; p < nP; p++ )
            {
                for ( int q = 0; q < nP; q++ )
                {
                    if ( fContinuousIn )
                    {
                        int ixFirstNode = dataGLLNodes[p][q][ixFirst] - 1;

                        smatMap ( ixSecondFace, ixFirstNode ) +=
                            dRemapCoeff[p][q][j]
                            * m_meshOverlap->vecFaceArea[ixOverlap + j]
                            / m_meshOutput->vecFaceArea[ixSecondFace];
                    }
                    else
                    {
                        int ixFirstNode = ixFirst * nP * nP + p * nP + q;

                        smatMap ( ixSecondFace, ixFirstNode ) +=
                            dRemapCoeff[p][q][j]
                            * m_meshOverlap->vecFaceArea[ixOverlap + j]
                            / m_meshOutput->vecFaceArea[ixSecondFace];
                    }
                }
            }
        }
        // Increment the current overlap index
        ixOverlap += nOverlapFaces;
    }
#ifdef VERBOSE
    output_file.flush(); // required here
    output_file.close();
#endif

    return;
}

///////////////////////////////////////////////////////////////////////////////

void moab::TempestOfflineMap::LinearRemapFVtoGLL_Simple_MOAB (
    const DataMatrix3D<int> & dataGLLNodes,
    const DataMatrix3D<double> & /*dataGLLJacobian*/,
    const DataVector<double> & /*dataGLLNodalArea*/,
    int nOrder,
    int /*nMonotoneType*/,
    bool fContinuous,
    bool /*fNoConservation*/
)
{
    // Order of triangular quadrature rule
    const int TriQuadRuleOrder = 8;

    // Verify ReverseNodeArray has been calculated
    if ( m_meshInputCov->revnodearray.size() == 0 )
    {
        _EXCEPTIONT ( "ReverseNodeArray has not been calculated for m_meshInput" );
    }
    if ( m_meshInputCov->edgemap.size() == 0 )
    {
        _EXCEPTIONT ( "EdgeMap has not been calculated for m_meshInput" );
    }

    // Get SparseMatrix represntation of the OfflineMap
    SparseMatrix<double> & smatMap = this->GetSparseMatrix();

    // Fit weight exponent
    int nFitWeightsExponent = nOrder + 2;

    // Order of the finite element method
    int nP = dataGLLNodes.GetRows();

    // Announcemnets
    if ( is_root )
    {
        Announce ( "[moab::TempestOfflineMap::LinearRemapFVtoGLL_Simple_MOAB] Finite Volume to Finite Element (Simple) Projection" );
        Announce ( "Triangular quadrature rule order %i", TriQuadRuleOrder );
        Announce ( "Order of the FE polynomial interpolant: %i", nP );
    }

    // Mesh utilities
    MeshUtilitiesFuzzy meshutil;

    // GLL nodes
    DataVector<double> dG;
    DataVector<double> dW;

    GaussLobattoQuadrature::GetPoints ( nP, 0.0, 1.0, dG, dW );

    // Triangular quadrature rule
    TriangularQuadratureRule triquadrule ( TriQuadRuleOrder );

    // Number of elements needed
#ifdef RECTANGULAR_TRUNCATION
    int nCoefficients = nOrder * nOrder;
#endif
#ifdef TRIANGULAR_TRUNCATION
    int nCoefficients = nOrder * ( nOrder + 1 ) / 2;
#endif

    int nRequiredFaceSetSize = nCoefficients;

    // Set of found nodes
    std::set<int> setFoundNodes;

    // Loop through all faces on m_meshInput
    int ixOverlap = 0;
#ifdef VERBOSE
    const unsigned outputFrequency = (m_meshInputCov->faces.size()/10);
#endif

    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
#ifdef VERBOSE
        // Announce computation progress
        if ( ixFirst % outputFrequency == 0 )
        {
            Announce ( "Element %i", ixFirst );
        }
#endif

        // This Face
        const Face & faceFirst = m_meshInputCov->faces[ixFirst];

        // Coordinate axes
        Node nodeRef = GetReferenceNode_MOAB ( faceFirst, m_meshInputCov->nodes );

        Node nodeA1 = m_meshInputCov->nodes[faceFirst[1]] - nodeRef;
        Node nodeA2 = m_meshInputCov->nodes[faceFirst[2]] - nodeRef;

        Node nodeC = CrossProduct ( nodeA1, nodeA2 );

        // Fit matrix
        DataMatrix<double> dFit;
        dFit.Initialize ( 3, 3 );

        dFit[0][0] = nodeA1.x; dFit[0][1] = nodeA1.y; dFit[0][2] = nodeA1.z;
        dFit[1][0] = nodeA2.x; dFit[1][1] = nodeA2.y; dFit[1][2] = nodeA2.z;
        dFit[2][0] = nodeC.x;  dFit[2][1] = nodeC.y;  dFit[2][2] = nodeC.z;

        // Set of Faces to use in building the reconstruction and associated
        // distance metric.
        AdjacentFaceVector vecAdjFaces;

        GetAdjacentFaceVectorByEdge (
            *m_meshInputCov,
            ixFirst,
            nRequiredFaceSetSize,
            vecAdjFaces );

        // Blank constraint
        DataVector<double> dConstraint;

        // Least squares arrays
        DataMatrix<double> dFitArray;
        DataVector<double> dFitWeights;
        DataMatrix<double> dFitArrayPlus;

        BuildFitArray (
            *m_meshInputCov,
            triquadrule,
            ixFirst,
            vecAdjFaces,
            nOrder,
            nFitWeightsExponent,
            dConstraint,
            dFitArray,
            dFitWeights
        );

        // Compute the inverse fit array
        InvertFitArray_Corrected (
            dConstraint,
            dFitArray,
            dFitWeights,
            dFitArrayPlus
        );

        // Number of overlapping Faces
        int nOverlapFaces = 0;
        size_t ixOverlapTemp = ixOverlap;
        for ( ; ixOverlapTemp < m_meshOverlap->faces.size(); ixOverlapTemp++ )
        {
            // const Face & faceOverlap = m_meshOverlap->faces[ixOverlapTemp];

            if ( ixFirst - m_meshOverlap->vecSourceFaceIx[ixOverlapTemp] != 0 )
            {
                break;
            }

            nOverlapFaces++;
        }

        // Loop through all Overlap Faces
        for ( int i = 0; i < nOverlapFaces; i++ )
        {
            // Quantities from the overlap Mesh
            // const Face & faceOverlap = m_meshOverlap->faces[ixOverlap + i];

            // Quantities from the Second Mesh
            int ixSecondFace = m_meshOverlap->vecTargetFaceIx[ixOverlap + i];

            // const NodeVector & nodesSecond = m_meshOutput->nodes;

            const Face & faceSecond = m_meshOutput->faces[ixSecondFace];

            for ( int s = 0; s < nP; s++ )
            {
                for ( int t = 0; t < nP; t++ )
                {
                    // Determine if this Node is in faceFirst
                    Node node;
                    Node dDx1G;
                    Node dDx2G;

                    ApplyLocalMap (
                        faceSecond,
                        m_meshOutput->nodes,
                        dG[s],
                        dG[t],
                        node,
                        dDx1G,
                        dDx2G );

                    Face::NodeLocation loc;
                    int ixLocation;

                    meshutil.ContainsNode (
                        faceFirst,
                        m_meshInputCov->nodes,
                        node,
                        loc,
                        ixLocation );

                    if ( loc == Face::NodeLocation_Exterior )
                    {
                        continue;
                    }

                    // Second node index
                    int ixSecondNode;

                    if ( fContinuous )
                    {
                        ixSecondNode = dataGLLNodes[t][s][ixSecondFace] - 1;
                    }
                    else
                    {
                        ixSecondNode = ixSecondFace * nP * nP + s * nP + t;
                    }

                    // Avoid duplicates
                    if ( setFoundNodes.find ( ixSecondNode ) != setFoundNodes.end() )
                    {
                        continue;
                    }

                    setFoundNodes.insert ( ixSecondNode );

                    // Find the coefficients for this point
                    double dX[3];
                    dX[0] = node.x - nodeRef.x;
                    dX[1] = node.y - nodeRef.y;
                    dX[2] = node.z - nodeRef.z;

                    // if (ixSecondNode < 10) {

                    //  double dLon = atan2(node.y, node.x);
                    //  if (dLon < 0.0) {
                    //      dLon += 2.0 * M_PI;
                    //  }
                    //  double dLat = asin(node.z);

                    //  dLon *= 180.0 / M_PI;
                    //  dLat *= 180.0 / M_PI;

                    //  printf("%i %1.15e %1.15e\n", ixSecondNode, dLon, dLat);
                    // }

                    int n = 3;
                    int nrhs = 1;
                    int lda = 3;
                    int ipiv[3];
                    int ldb = 3;
                    int info;

                    DataMatrix<double> dFitTemp;
                    dFitTemp = dFit;
                    dgesv_ (
                        &n, &nrhs, & ( dFitTemp[0][0] ), &lda, ipiv, dX, &ldb, &info );

                    // Sample the reconstruction at this point
                    int ixp = 0;

#ifdef RECTANGULAR_TRUNCATION
                    for ( int p = 0; p < nOrder; p++ )
                    {
                        for ( int q = 0; q < nOrder; q++ )
                        {
#endif
#ifdef TRIANGULAR_TRUNCATION
                    for ( int p = 0; p < nOrder; p++ )
                    {
                        for ( int q = 0; q < nOrder - p; q++ )
                        {
#endif

                            for ( size_t nx = 0; nx < vecAdjFaces.size(); nx++ )
                            {
                                int ixAdjFace = vecAdjFaces[nx].first;

                                smatMap ( ixSecondNode, ixAdjFace ) +=
                                    IPow ( dX[0], p )
                                    * IPow ( dX[1], q )
                                    * dFitArrayPlus[nx][ixp];
                            }

                            ixp++;
                        }
                    }
                }
            }
        }

        // Increment the current overlap index
        ixOverlap += nOverlapFaces;

    }

    return;
}

///////////////////////////////////////////////////////////////////////////////

void moab::TempestOfflineMap::LinearRemapFVtoGLL_Volumetric_MOAB (
    const DataMatrix3D<int> & dataGLLNodes,
    const DataMatrix3D<double> & dataGLLJacobian,
    const DataVector<double> & dataGLLNodalArea,
    int nOrder,
    int nMonotoneType,
    bool /*fContinuous*/,
    bool fNoConservation
)
{
    // Order of triangular quadrature rule
    const int TriQuadRuleOrder = 4;

    // Verify ReverseNodeArray has been calculated
    if ( m_meshInputCov->revnodearray.size() == 0 )
    {
        _EXCEPTIONT ( "ReverseNodeArray has not been calculated for m_meshInput" );
    }

    // Triangular quadrature rule
    TriangularQuadratureRule triquadrule ( TriQuadRuleOrder );

    // Fit weight exponent
    int nFitWeightsExponent = nOrder + 2;

    // Order of the finite element method
    int nP = dataGLLNodes.GetRows();

    // Announcemnets
    if ( is_root )
    {
        Announce ( "[moab::TempestOfflineMap::LinearRemapFVtoGLL_Volumetric_MOAB] Finite Volume to Finite Element (Volumetric) Projection" );
        Announce ( "Triangular quadrature rule order %i", TriQuadRuleOrder );
        Announce ( "Order of the FE polynomial interpolant: %i", nP );
    }

    // Gauss-Lobatto quadrature nodes and weights
    DataVector<double> dG;
    DataVector<double> dW;

    GaussLobattoQuadrature::GetPoints ( nP, 0.0, 1.0, dG, dW );

    // Get SparseMatrix represntation of the OfflineMap
    SparseMatrix<double> & smatMap = this->GetSparseMatrix();

    // Number of elements needed
#ifdef RECTANGULAR_TRUNCATION
    int nCoefficients = nOrder * nOrder;
#endif
#ifdef TRIANGULAR_TRUNCATION
    int nCoefficients = nOrder * ( nOrder + 1 ) / 2;
#endif

// #pragma message "This should be a command-line parameter"
    int nRequiredFaceSetSize = nCoefficients;

    // Accumulated weight vector
    DataVector<double> dAccumW ( nP + 1 );
    dAccumW[0] = 0.0;
    for ( int i = 1; i < nP + 1; i++ )
    {
        dAccumW[i] = dAccumW[i - 1] + dW[i - 1];
    }
    if ( fabs ( dAccumW[dAccumW.GetRows() - 1] - 1.0 ) > 1.0e-14 )
    {
        _EXCEPTIONT ( "Logic error in accumulated weight" );
    }

    // Create sub-element mesh and redistribution map
    Announce ( "Generating sub-element mesh" );
    Mesh meshTargetSubElement;

    DataVector<double> dFiniteVolumeArea ( nP * nP );
    DataVector<double> dQuadratureArea ( nP * nP );
    std::vector< DataMatrix<double> > dRedistributionMaps;
    dRedistributionMaps.resize ( m_meshOutput->faces.size() );

    for ( size_t ixSecond = 0; ixSecond < m_meshOutput->faces.size(); ixSecond++ )
    {

        const Face & faceSecond = m_meshOutput->faces[ixSecond];

        const Node & nodeOutput0 = m_meshOutput->nodes[faceSecond[0]];
        const Node & nodeOutput1 = m_meshOutput->nodes[faceSecond[1]];
        const Node & nodeOutput2 = m_meshOutput->nodes[faceSecond[2]];
        const Node & nodeOutput3 = m_meshOutput->nodes[faceSecond[3]];

        for ( int q = 0; q < nP; q++ )
        {
            for ( int p = 0; p < nP; p++ )
            {

                Node node0 =
                    InterpolateQuadrilateralNode (
                        nodeOutput0, nodeOutput1, nodeOutput2, nodeOutput3,
                        dAccumW[p], dAccumW[q] );

                Node node1 =
                    InterpolateQuadrilateralNode (
                        nodeOutput0, nodeOutput1, nodeOutput2, nodeOutput3,
                        dAccumW[p + 1], dAccumW[q] );

                Node node2 =
                    InterpolateQuadrilateralNode (
                        nodeOutput0, nodeOutput1, nodeOutput2, nodeOutput3,
                        dAccumW[p + 1], dAccumW[q + 1] );

                Node node3 =
                    InterpolateQuadrilateralNode (
                        nodeOutput0, nodeOutput1, nodeOutput2, nodeOutput3,
                        dAccumW[p], dAccumW[q + 1] );

                int nNodeStart = meshTargetSubElement.nodes.size();
                meshTargetSubElement.nodes.push_back ( node0 );
                meshTargetSubElement.nodes.push_back ( node1 );
                meshTargetSubElement.nodes.push_back ( node2 );
                meshTargetSubElement.nodes.push_back ( node3 );

                Face faceNew ( 4 );
                faceNew.SetNode ( 0, nNodeStart );
                faceNew.SetNode ( 1, nNodeStart + 1 );
                faceNew.SetNode ( 2, nNodeStart + 2 );
                faceNew.SetNode ( 3, nNodeStart + 3 );

                meshTargetSubElement.faces.push_back ( faceNew );

                dFiniteVolumeArea[q * nP + p] =
                    CalculateFaceArea (
                        faceNew, meshTargetSubElement.nodes );

                dQuadratureArea[q * nP + p] =
                    dataGLLJacobian[q][p][ixSecond];
            }
        }

        dRedistributionMaps[ixSecond].Initialize ( nP * nP, nP * nP );
        for ( int i = 0; i < nP * nP; i++ )
        {
            dRedistributionMaps[ixSecond][i][i] = 1.0;
        }

        if ( !fNoConservation )
        {
            ForceIntArrayConsistencyConservation (
                dFiniteVolumeArea,
                dQuadratureArea,
                dRedistributionMaps[ixSecond],
                ( nMonotoneType != 0 ) );
        }
        /*
                double dSumQuadArea = 0.0;
                double dSumFVArea = 0.0;
                for (int i = 0; i < nP * nP; i++) {
                    dSumQuadArea += dQuadratureArea[i];
                    dSumFVArea += dFiniteVolumeArea[i];
                }
                if (fabs(dSumQuadArea - dSumFVArea) > 1.0e-14) {
                    printf("%1.15e\n", dSumQuadArea - dSumFVArea);
                    _EXCEPTION();
                }
        */
        for ( int i = 0; i < nP * nP; i++ )
        {
            for ( int j = 0; j < nP * nP; j++ )
            {
                dRedistributionMaps[ixSecond][i][j] *=
                    dQuadratureArea[i] / dFiniteVolumeArea[j];
            }
        }
        /*
                for (int i = 0; i < nP * nP; i++) {
                    double dSum = 0.0;
                    for (int j = 0; j < nP * nP; j++) {
                        dSum += dRedistributionMaps[ixSecond][i][j] * dFiniteVolumeArea[j];
                    }
                    printf("%i %1.15e %1.15e\n", i, dSum, dQuadratureArea[i]);
                }
                _EXCEPTION();
        */
        /*
                for (int i = 0; i < nP * nP; i++) {
                for (int j = 0; j < nP * nP; j++) {
                    printf("%i %i %1.15e\n", i, j, dRedistributionMaps[ixSecond][i][j]);
                }
                }
                _EXCEPTION();
        */
    }

    // Current overlap face
    int ixOverlap = 0;
#ifdef VERBOSE
    const unsigned outputFrequency = (m_meshInputCov->faces.size()/10);
#endif

    // Loop through all faces on m_meshInput
    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
#ifdef VERBOSE
        // Announce computation progress
        if ( ixFirst % outputFrequency == 0 )
        {
            Announce ( "Element %i", ixFirst );
        }
#endif

        // This Face
        // const Face & faceFirst = m_meshInputCov->faces[ixFirst];

        // Find the set of Faces that overlap faceFirst
        size_t ixOverlapBegin = ixOverlap;
        size_t ixOverlapEnd = ixOverlapBegin;

        for ( ; ixOverlapEnd < m_meshOverlap->faces.size(); ixOverlapEnd++ )
        {
            if ( ixFirst - m_meshOverlap->vecSourceFaceIx[ixOverlapEnd] != 0 )
                break;
        }

        size_t nOverlapFaces = ixOverlapEnd - ixOverlapBegin;

        // Create a new Mesh representing the division of target finite
        // elements associated with this finite volume.
        Mesh meshThisElement;
        meshThisElement.faces.reserve ( nOverlapFaces * nP * nP );
        meshThisElement.vecTargetFaceIx.reserve ( nOverlapFaces * nP * nP );

        for ( size_t i = ixOverlapBegin; i < ixOverlapEnd; i++ )
        {

            int iTargetFace = m_meshOverlap->vecTargetFaceIx[i];

            int iSubElementBegin =  iTargetFace      * nP * nP;
            // int iSubElementEnd   = ( iTargetFace + 1 ) * nP * nP;

            int iSubElement = iSubElementBegin;
            for ( int p = 0; p < nP; p++ )
            {
                for ( int q = 0; q < nP; q++ )
                {

                    // Calculate overlap polygon between sub-element
                    // and finite volume
                    NodeVector nodevecOutput;

                    GenerateOverlapFace<MeshUtilitiesFuzzy, Node> (
                        *m_meshInputCov,
                        meshTargetSubElement,
                        ixFirst,
                        iSubElementBegin + p * nP + q,
                        nodevecOutput );
                    /*
                                    if (nodevecOutput.size() < 3) {
                                        continue;
                                    }

                                    Face faceNew(nodevecOutput.size());
                                    for (int n = 0; n < nodevecOutput.size(); n++) {
                                        faceNew.SetNode(n, n);
                                    }
                                    Real dArea = CalculateFaceArea(faceNew, nodevecOutput);

                                    if (dArea < 1.0e-13) {
                                        continue;
                                    }
                    */
                    /*
                                    if (dataGLLNodes[p][q][iTargetFace] - 1 == 3) {
                                        printf("%1.15e %1.15e %1.15e\n", dArea, dataGLLJacobian[p][q][iTargetFace], dataGLLNodalArea[dataGLLNodes[p][q][iTargetFace] - 1]);
                                    }
                    */
                    Face faceNew ( nodevecOutput.size() );
                    for ( size_t n = 0; n < nodevecOutput.size(); n++ )
                    {
                        meshThisElement.nodes.push_back ( nodevecOutput[n] );
                        faceNew.SetNode ( n, meshThisElement.nodes.size() - 1 );
                    }

                    meshThisElement.faces.push_back ( faceNew );

                    meshThisElement.vecTargetFaceIx.push_back (
                        dataGLLNodes[p][q][iTargetFace] - 1 );

                    iSubElement++;
                }
            }
        }

        if ( meshThisElement.faces.size() != nOverlapFaces * nP * nP )
        {
            _EXCEPTIONT ( "Logic error" );
        }

        // Build integration array
        DataMatrix<double> dIntArray;

        BuildIntegrationArray (
            *m_meshInputCov,
            meshThisElement,
            triquadrule,
            ixFirst,
            0,
            meshThisElement.faces.size(),
            nOrder,
            dIntArray );

        // Set of Faces to use in building the reconstruction and associated
        // distance metric.
        AdjacentFaceVector vecAdjFaces;

        GetAdjacentFaceVectorByEdge (
            *m_meshInputCov,
            ixFirst,
            nRequiredFaceSetSize,
            vecAdjFaces );

        // Number of adjacent Faces
        int nAdjFaces = vecAdjFaces.size();

        // Determine the conservative constraint equation
        DataVector<double> dConstraint;

        dConstraint.Initialize ( nCoefficients );

        double dFirstArea = m_meshInputCov->vecFaceArea[ixFirst];

        for ( int p = 0; p < nCoefficients; p++ )
        {
            for ( size_t j = 0; j < meshThisElement.faces.size(); j++ )
            {
                dConstraint[p] += dIntArray[p][j];
            }
            dConstraint[p] /= dFirstArea;
        }

        // Least squares arrays
        DataMatrix<double> dFitArray;
        DataVector<double> dFitWeights;
        DataMatrix<double> dFitArrayPlus;

        BuildFitArray (
            *m_meshInputCov,
            triquadrule,
            ixFirst,
            vecAdjFaces,
            nOrder,
            nFitWeightsExponent,
            dConstraint,
            dFitArray,
            dFitWeights
        );

        // Compute the inverse fit array
        InvertFitArray_Corrected (
            dConstraint,
            dFitArray,
            dFitWeights,
            dFitArrayPlus
        );

        // Multiply integration array and fit array
        DataMatrix<double> dComposedArray;
        dComposedArray.Initialize ( nAdjFaces, meshThisElement.faces.size() );

        for ( int i = 0; i < nAdjFaces; i++ )
        {
            for ( size_t j = 0; j < meshThisElement.faces.size(); j++ )
            {
                for ( int k = 0; k < nCoefficients; k++ )
                {
                    dComposedArray[i][j] += dIntArray[k][j] * dFitArrayPlus[i][k];
                }
            }
        }

        // Apply redistribution operator
        DataMatrix<double> dRedistributedArray;
        dRedistributedArray.Initialize ( nAdjFaces, meshThisElement.faces.size() );

        for ( int i = 0; i < nAdjFaces; i++ )
        {
            for ( size_t j = 0; j < meshThisElement.faces.size(); j++ )
            {
                int ixSubElement = j % ( nP * nP );
                int ixElement = j / ( nP * nP );

                int ixSecondFace =
                    m_meshOverlap->vecTargetFaceIx[ixOverlap + ixElement];

                for ( int k = 0; k < nP * nP; k++ )
                {
                    dRedistributedArray[i][j] +=
                        dComposedArray[i][ixElement * nP * nP + k]
                        * dRedistributionMaps[ixSecondFace][ixSubElement][k];
                }
            }
        }

        // Put composed array into map
        for ( size_t i = 0; i < vecAdjFaces.size(); i++ )
        {
            for ( size_t j = 0; j < meshThisElement.faces.size(); j++ )
            {
                int ixFirstFace = vecAdjFaces[i].first;
                int ixSecondNode = meshThisElement.vecTargetFaceIx[j];

                smatMap ( ixSecondNode, ixFirstFace ) +=
                    dRedistributedArray[i][j]
                    / dataGLLNodalArea[ixSecondNode];
                // meshThisElement.vecFaceArea[j];
                // dataGLLJacobian[ixS][ixT][ixSecondElement];
            }
        }

        // Increment the current overlap index
        ixOverlap += nOverlapFaces;

        //_EXCEPTION();
    }

    return;
}

///////////////////////////////////////////////////////////////////////////////

void moab::TempestOfflineMap::LinearRemapFVtoGLL_MOAB (
    const DataMatrix3D<int> & dataGLLNodes,
    const DataMatrix3D<double> & dataGLLJacobian,
    const DataVector<double> & dataGLLNodalArea,
    int nOrder,
    int nMonotoneType,
    bool fContinuous,
    bool fNoConservation
)
{
    // NOTE: Reducing this quadrature rule order greatly affects error norms
    // Order of triangular quadrature rule
    const int TriQuadRuleOrder = 8;

    // Verify ReverseNodeArray has been calculated
    if ( m_meshInputCov->revnodearray.size() == 0 )
    {
        _EXCEPTIONT ( "ReverseNodeArray has not been calculated for m_meshInput" );
    }
    if ( m_meshInputCov->edgemap.size() == 0 )
    {
        _EXCEPTIONT ( "EdgeMap has not been calculated for m_meshInput" );
    }

    // Triangular quadrature rule
    TriangularQuadratureRule triquadrule ( TriQuadRuleOrder );

    const DataMatrix<double> & dG = triquadrule.GetG();
    const DataVector<double> & dW = triquadrule.GetW();

    // Get SparseMatrix represntation of the OfflineMap
    SparseMatrix<double> & smatMap = this->GetSparseMatrix();

    // Fit weight exponent
    int nFitWeightsExponent = nOrder + 2;

    // Order of the finite element method
    int nP = dataGLLNodes.GetRows();

    // Sample coefficients
    DataMatrix<double> dSampleCoeff;
    dSampleCoeff.Initialize ( nP, nP );

    // Number of elements needed
#ifdef RECTANGULAR_TRUNCATION
    int nCoefficients = nOrder * nOrder;
#endif
#ifdef TRIANGULAR_TRUNCATION
    int nCoefficients = nOrder * ( nOrder + 1 ) / 2;
#endif

    int nRequiredFaceSetSize = nCoefficients;

    // Announcemnets
    if ( is_root )
    {
        Announce ( "[moab::TempestOfflineMap::LinearRemapFVtoGLL_MOAB] Finite Volume to Finite Element Projection" );
        Announce ( "Triangular quadrature rule order %i", TriQuadRuleOrder );
        Announce ( "Order of the FE polynomial interpolant: %i", nP );
        Announce ( "Number of coefficients: %i", nCoefficients );
        Announce ( "Required adjacency set size: %i", nRequiredFaceSetSize );
        Announce ( "Fit weights exponent: %i", nFitWeightsExponent );
    }

    // Current overlap face
    int ixOverlap = 0;

    // Build the integration array for each element on m_meshOverlap
    DataMatrix3D<double> dGlobalIntArray;
    dGlobalIntArray.Initialize (
        nCoefficients,
        m_meshOverlap->faces.size(),
        nP * nP );

    // Number of overlap Faces per source Face
    DataVector<int> nAllOverlapFaces;
    nAllOverlapFaces.Initialize ( m_meshInputCov->faces.size() );

    // DataVector<int> nAllTotalOverlapTriangles;
    // nAllTotalOverlapTriangles.Initialize ( m_meshInputCov->faces.size() );

    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
        size_t ixOverlapTemp = ixOverlap;
        for ( ; ixOverlapTemp < m_meshOverlap->faces.size(); ixOverlapTemp++ )
        {
            // const Face & faceOverlap = m_meshOverlap->faces[ixOverlapTemp];

            if ( ixFirst - m_meshOverlap->vecSourceFaceIx[ixOverlapTemp] != 0 )
            {
                break;
            }

            nAllOverlapFaces[ixFirst]++;
            // nAllTotalOverlapTriangles[ixFirst] += faceOverlap.edges.size() - 2;
        }

        // Increment the current overlap index
        ixOverlap += nAllOverlapFaces[ixFirst];
    }

    // Loop through all faces on m_meshInput
    ixOverlap = 0;
#ifdef VERBOSE
    const unsigned outputFrequency = (m_meshInputCov->faces.size()/10);
#endif

    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
#ifdef VERBOSE
        // Announce computation progress
        if ( ixFirst % outputFrequency == 0 && is_root )
        {
            Announce ( "Element %i/%i", ixFirst, m_meshInputCov->faces.size() );
        }
#endif

        // This Face
        const Face & faceFirst = m_meshInputCov->faces[ixFirst];

        // Area of the First Face
        // double dFirstArea = m_meshInputCov->vecFaceArea[ixFirst];

        // Coordinate axes
        Node nodeRef = GetReferenceNode_MOAB ( faceFirst, m_meshInputCov->nodes );

        Node nodeA1 = m_meshInputCov->nodes[faceFirst[1]] - nodeRef;
        Node nodeA2 = m_meshInputCov->nodes[faceFirst[2]] - nodeRef;

        Node nodeC = CrossProduct ( nodeA1, nodeA2 );

        // Fit matrix
        DataMatrix<double> dFit;
        dFit.Initialize ( 3, 3 );

        dFit[0][0] = nodeA1.x; dFit[0][1] = nodeA1.y; dFit[0][2] = nodeA1.z;
        dFit[1][0] = nodeA2.x; dFit[1][1] = nodeA2.y; dFit[1][2] = nodeA2.z;
        dFit[2][0] = nodeC.x;  dFit[2][1] = nodeC.y;  dFit[2][2] = nodeC.z;

        // Number of overlapping Faces and triangles
        int nOverlapFaces = nAllOverlapFaces[ixFirst];
        // int nTotalOverlapTriangles = nAllTotalOverlapTriangles[ixFirst];

        // Loop through all Overlap Faces
        for ( int i = 0; i < nOverlapFaces; i++ )
        {
            // Quantities from the overlap Mesh
            const Face & faceOverlap = m_meshOverlap->faces[ixOverlap + i];

            const NodeVector & nodesOverlap = m_meshOverlap->nodes;

            int nOverlapTriangles = faceOverlap.edges.size() - 2;

            // Quantities from the Second Mesh
            int ixSecond = m_meshOverlap->vecTargetFaceIx[ixOverlap + i];

            const NodeVector & nodesSecond = m_meshOutput->nodes;

            const Face & faceSecond = m_meshOutput->faces[ixSecond];

            // Loop over all sub-triangles of this Overlap Face
            for ( int j = 0; j < nOverlapTriangles; j++ )
            {
                // Cornerpoints of triangle
                const Node & node0 = nodesOverlap[faceOverlap[0]];
                const Node & node1 = nodesOverlap[faceOverlap[j + 1]];
                const Node & node2 = nodesOverlap[faceOverlap[j + 2]];

                // Calculate the area of the modified Face
                Face faceTri ( 3 );
                faceTri.SetNode ( 0, faceOverlap[0] );
                faceTri.SetNode ( 1, faceOverlap[j + 1] );
                faceTri.SetNode ( 2, faceOverlap[j + 2] );

                double dTriArea =
                    CalculateFaceArea ( faceTri, nodesOverlap );

                for ( int k = 0; k < triquadrule.GetPoints(); k++ )
                {
                    double * dGL = dG[k];

                    // Get the nodal location of this point
                    double dX[3];

                    dX[0] = dGL[0] * node0.x + dGL[1] * node1.x + dGL[2] * node2.x;
                    dX[1] = dGL[0] * node0.y + dGL[1] * node1.y + dGL[2] * node2.y;
                    dX[2] = dGL[0] * node0.z + dGL[1] * node1.z + dGL[2] * node2.z;

                    double dMag =
                        sqrt ( dX[0] * dX[0] + dX[1] * dX[1] + dX[2] * dX[2] );

                    dX[0] /= dMag;
                    dX[1] /= dMag;
                    dX[2] /= dMag;

                    Node nodeQuadrature ( dX[0], dX[1], dX[2] );

                    dX[0] -= nodeRef.x;
                    dX[1] -= nodeRef.y;
                    dX[2] -= nodeRef.z;

                    // Find the coefficients for this point of the polynomial
                    int n = 3;
                    int nrhs = 1;
                    int lda = 3;
                    int ipiv[3];
                    int ldb = 3;
                    int info;

                    DataMatrix<double> dFitTemp;
                    dFitTemp = dFit;
                    dgesv_ (
                        &n, &nrhs, & ( dFitTemp[0][0] ), &lda, ipiv, dX, &ldb, &info );

                    // Find the components of this quadrature point in the basis
                    // of the finite element.
                    double dAlpha;
                    double dBeta;

                    ApplyInverseMap (
                        faceSecond,
                        nodesSecond,
                        nodeQuadrature,
                        dAlpha,
                        dBeta );
                    /*
                        // Check inverse map value
                        if ((dAlpha < -1.0e-12) || (dAlpha > 1.0 + 1.0e-12) ||
                            (dBeta  < -1.0e-12) || (dBeta  > 1.0 + 1.0e-12)
                        ) {
                            _EXCEPTION2("Inverse Map out of range (%1.5e %1.5e)",
                                dAlpha, dBeta);
                        }
                    */
                    // Sample the finite element at this point
                    SampleGLLFiniteElement (
                        nMonotoneType,
                        nP,
                        dAlpha,
                        dBeta,
                        dSampleCoeff );

                    // Sample this point in the GLL element
                    int ixs = 0;
                    for ( int s = 0; s < nP; s++ )
                    {
                        for ( int t = 0; t < nP; t++ )
                        {
                            int ixp = 0;

                            for ( int p = 0; p < nOrder; p++ )
                            {
#ifdef TRIANGULAR_TRUNCATION
                                const int redOrder = p;
#else
                                const int redOrder = 0;
#endif
                                for ( int q = 0; q < nOrder - redOrder; q++ )
                                {
                                    dGlobalIntArray[ixp][ixOverlap + i][ixs] +=
                                        dSampleCoeff[s][t]
                                        * IPow ( dX[0], p )
                                        * IPow ( dX[1], q )
                                        * dW[k]
                                        * dTriArea
                                        / dataGLLJacobian[s][t][ixSecond];

                                    ixp++;
                                }
                            }
                            ixs++;
                        }
                    }
                }
            }
        }

        // Increment the current overlap index
        ixOverlap += nOverlapFaces;
    }

    // Reverse map
    std::vector< std::vector<int> > vecReverseFaceIx;
    vecReverseFaceIx.resize ( m_meshOutput->faces.size() );
    for ( size_t i = 0; i < m_meshOverlap->faces.size(); i++ )
    {
        int ixSecond = m_meshOverlap->vecTargetFaceIx[i];

        vecReverseFaceIx[ixSecond].push_back ( i );
    }

    // Force consistency and conservation
    for ( size_t ixSecond = 0; ixSecond < m_meshOutput->faces.size(); ixSecond++ )
    {
        if ( vecReverseFaceIx[ixSecond].size() == 0 )
            continue;

        DataMatrix<double> dCoeff;
        dCoeff.Initialize (
            nP * nP,
            vecReverseFaceIx[ixSecond].size() );

        for ( size_t i = 0; i < vecReverseFaceIx[ixSecond].size(); i++ )
        {
            int ijxOverlap = vecReverseFaceIx[ixSecond][i];

            for ( int s = 0; s < nP * nP; s++ )
            {
                dCoeff[s][i] = dGlobalIntArray[0][ijxOverlap][s];
            }
        }

        // Target areas
        DataVector<double> vecTargetArea;
        vecTargetArea.Initialize ( nP * nP );

        for ( int s = 0; s < nP * nP; s++ )
        {
            vecTargetArea[s] =
                dataGLLJacobian[s / nP][s % nP][ixSecond];
        }

        // Source areas
        DataVector<double> vecSourceArea;
        vecSourceArea.Initialize ( vecReverseFaceIx[ixSecond].size() );

        for ( size_t i = 0; i < vecReverseFaceIx[ixSecond].size(); i++ )
        {
            int ijxOverlap = vecReverseFaceIx[ixSecond][i];
            vecSourceArea[i] = m_meshOverlap->vecFaceArea[ijxOverlap];
        }

        if ( !fNoConservation )
        {
            ForceIntArrayConsistencyConservation (
                vecSourceArea,
                vecTargetArea,
                dCoeff,
                ( nMonotoneType != 0 ) );
        }

        for ( size_t i = 0; i < vecReverseFaceIx[ixSecond].size(); i++ )
        {
            int ijxOverlap = vecReverseFaceIx[ixSecond][i];

            for ( int s = 0; s < nP * nP; s++ )
            {
                //printf("%1.15e %1.15e\n", dGlobalIntArray[0][ixOverlap][s], dCoeff[s][i]);
                dGlobalIntArray[0][ijxOverlap][s] = dCoeff[s][i];
            }
        }

    }

    // Construct finite-volume fit matrix and compose with integration operator
    ixOverlap = 0;

    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
#ifdef VERBOSE
        // Announce computation progress
        if ( ixFirst % outputFrequency == 0 )
        {
            Announce ( "Element %i", ixFirst );
        }
#endif

        // Area of the First Face
        double dFirstArea = m_meshInputCov->vecFaceArea[ixFirst];

        // Number of overlapping Faces and triangles
        int nOverlapFaces = nAllOverlapFaces[ixFirst];

        // Determine the conservative constraint equation
        DataVector<double> dConstraint;

        dConstraint.Initialize ( nCoefficients );

        for ( int p = 0; p < nCoefficients; p++ )
        {
            for ( int i = 0; i < nOverlapFaces; i++ )
            {
                int ixSecond = m_meshOverlap->vecTargetFaceIx[ixOverlap + i];

                for ( int s = 0; s < nP * nP; s++ )
                {
                    dConstraint[p] += dGlobalIntArray[p][ixOverlap + i][s]
                                      * dataGLLJacobian[s / nP][s % nP][ixSecond]
                                      / dFirstArea;
                }
            }
        }

        // Set of Faces to use in building the reconstruction and associated
        // distance metric.
        AdjacentFaceVector vecAdjFaces;

        GetAdjacentFaceVectorByEdge (
            *m_meshInputCov,
            ixFirst,
            nRequiredFaceSetSize,
            vecAdjFaces );

        // Number of adjacent Faces
        int nAdjFaces = vecAdjFaces.size();

        for ( int x = 0; x < nAdjFaces; x++ )
        {
            if ( vecAdjFaces[x].first == ( -1 ) )
            {
                _EXCEPTION();
            }
        }

        // Build the fit operator
        DataMatrix<double> dFitArray;
        DataVector<double> dFitWeights;
        DataMatrix<double> dFitArrayPlus;

        BuildFitArray (
            *m_meshInputCov,
            triquadrule,
            ixFirst,
            vecAdjFaces,
            nOrder,
            nFitWeightsExponent,
            dConstraint,
            dFitArray,
            dFitWeights
        );

        // Compute the inverse fit array
        InvertFitArray_Corrected (
            dConstraint,
            dFitArray,
            dFitWeights,
            dFitArrayPlus
        );

        // Multiply integration array and fit array
        DataMatrix<double> dComposedArray;
        dComposedArray.Initialize ( nAdjFaces, nOverlapFaces * nP * nP );

        for ( int j = 0; j < nOverlapFaces; j++ )
        {
            // int ixSecond = m_meshOverlap->vecTargetFaceIx[ixOverlap + j];

            for ( int i = 0; i < nAdjFaces; i++ )
            {
                for ( int s = 0; s < nP * nP; s++ )
                {
                    for ( int k = 0; k < nCoefficients; k++ )
                    {
                        dComposedArray[i][j * nP * nP + s] +=
                            dGlobalIntArray[k][ixOverlap + j][s]
                            * dFitArrayPlus[i][k];
                    }
                }
            }
        }

        // Put composed array into map
        for ( size_t i = 0; i < vecAdjFaces.size(); i++ )
        {
            for ( int j = 0; j < nOverlapFaces; j++ )
            {
                int ixFirstFace = vecAdjFaces[i].first;
                int ixSecondFace = m_meshOverlap->vecTargetFaceIx[ixOverlap + j];

                for ( int s = 0; s < nP; s++ )
                {
                    for ( int t = 0; t < nP; t++ )
                    {

                        int jx = j * nP * nP + s * nP + t;

                        if ( fContinuous )
                        {
                            int ixSecondNode = dataGLLNodes[s][t][ixSecondFace] - 1;

                            smatMap ( ixSecondNode, ixFirstFace ) +=
                                dComposedArray[i][jx]
                                * dataGLLJacobian[s][t][ixSecondFace]
                                / dataGLLNodalArea[ixSecondNode];
                        }
                        else
                        {
                            int ixSecondNode = ixSecondFace * nP * nP + s * nP + t;

                            smatMap ( ixSecondNode, ixFirstFace ) +=
                                dComposedArray[i][jx];
                        }
                    }
                }
            }
        }

        ixOverlap += nOverlapFaces;
    }

    return;
}

///////////////////////////////////////////////////////////////////////////////

void moab::TempestOfflineMap::LinearRemapGLLtoGLL2_MOAB (
    const DataMatrix3D<int> & dataGLLNodesIn,
    const DataMatrix3D<double> & dataGLLJacobianIn,
    const DataMatrix3D<int> & dataGLLNodesOut,
    const DataMatrix3D<double> & dataGLLJacobianOut,
    const DataVector<double> & dataNodalAreaOut,
    int nPin,
    int nPout,
    int nMonotoneType,
    bool fContinuousIn,
    bool fContinuousOut,
    bool fNoConservation
)
{
    // Triangular quadrature rule
    TriangularQuadratureRule triquadrule ( 8 );

    const DataMatrix<double> & dG = triquadrule.GetG();
    const DataVector<double> & dW = triquadrule.GetW();

    // Get SparseMatrix represntation of the OfflineMap
    SparseMatrix<double> & smatMap = this->GetSparseMatrix();

    // Sample coefficients
    DataMatrix<double> dSampleCoeffIn;
    dSampleCoeffIn.Initialize ( nPin, nPin );

    DataMatrix<double> dSampleCoeffOut;
    dSampleCoeffOut.Initialize ( nPout, nPout );

    // Announcemnets
    if ( is_root )
    {
        Announce ( "[moab::TempestOfflineMap::LinearRemapGLLtoGLL2_MOAB] Finite Element to Finite Element Projection" );
        Announce ( "Order of the input FE polynomial interpolant: %i", nPin );
        Announce ( "Order of the output FE polynomial interpolant: %i", nPout );
    }

    // Build the integration array for each element on m_meshOverlap
    DataMatrix3D<double> dGlobalIntArray;
    dGlobalIntArray.Initialize (
        nPin * nPin,
        m_meshOverlap->faces.size(),
        nPout * nPout );

    // Number of overlap Faces per source Face
    DataVector<int> nAllOverlapFaces;
    nAllOverlapFaces.Initialize ( m_meshInputCov->faces.size() );

    int ixOverlap = 0;

    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
        size_t ixOverlapTemp = ixOverlap;
        for ( ; ixOverlapTemp < m_meshOverlap->faces.size(); ixOverlapTemp++ )
        {
            // const Face & faceOverlap = m_meshOverlap->faces[ixOverlapTemp];

            if ( ixFirst - m_meshOverlap->vecSourceFaceIx[ixOverlapTemp] != 0 )
            {
                break;
            }

            nAllOverlapFaces[ixFirst]++;
        }

        // Increment the current overlap index
        ixOverlap += nAllOverlapFaces[ixFirst];
    }

    // Geometric area of each output node
    DataMatrix<double> dGeometricOutputArea;
    dGeometricOutputArea.Initialize (
        m_meshOutput->faces.size(), nPout * nPout );

    // Area of each overlap element in the output basis
    DataMatrix<double> dOverlapOutputArea;
    dOverlapOutputArea.Initialize (
        m_meshOverlap->faces.size(), nPout * nPout );

    // Loop through all faces on m_meshInput
    ixOverlap = 0;
#ifdef VERBOSE
    const unsigned outputFrequency = (m_meshInputCov->faces.size()/10);
#endif

    if ( is_root )
        Announce ( "Building conservative distribution maps" );
    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
#ifdef VERBOSE
        // Announce computation progress
        if ( ixFirst % outputFrequency == 0 && is_root )
        {
            Announce ( "Element %i", ixFirst );
        }
#endif

        // Quantities from the First Mesh
        const Face & faceFirst = m_meshInputCov->faces[ixFirst];

        const NodeVector & nodesFirst = m_meshInputCov->nodes;

        // Number of overlapping Faces and triangles
        int nOverlapFaces = nAllOverlapFaces[ixFirst];

        if ( !nOverlapFaces ) continue;
        /*
                // Calculate total element Jacobian
                double dTotalJacobian = 0.0;
                for (int s = 0; s < nPin; s++) {
                for (int t = 0; t < nPin; t++) {
                    dTotalJacobian += dataGLLJacobianIn[s][t][ixFirst];
                }
                }
        */

        // Loop through all Overlap Faces
        for ( int i = 0; i < nOverlapFaces; i++ )
        {

            // Quantities from the overlap Mesh
            const Face & faceOverlap = m_meshOverlap->faces[ixOverlap + i];

            const NodeVector & nodesOverlap = m_meshOverlap->nodes;

            int nOverlapTriangles = faceOverlap.edges.size() - 2;

            // Quantities from the Second Mesh
            int ixSecond = m_meshOverlap->vecTargetFaceIx[ixOverlap + i];

            const NodeVector & nodesSecond = m_meshOutput->nodes;

            const Face & faceSecond = m_meshOutput->faces[ixSecond];

            // Loop over all sub-triangles of this Overlap Face
            for ( int j = 0; j < nOverlapTriangles; j++ )
            {

                // Cornerpoints of triangle
                const Node & node0 = nodesOverlap[faceOverlap[0]];
                const Node & node1 = nodesOverlap[faceOverlap[j + 1]];
                const Node & node2 = nodesOverlap[faceOverlap[j + 2]];

                // Calculate the area of the modified Face
                Face faceTri ( 3 );
                faceTri.SetNode ( 0, faceOverlap[0] );
                faceTri.SetNode ( 1, faceOverlap[j + 1] );
                faceTri.SetNode ( 2, faceOverlap[j + 2] );

                double dTriArea =
                    CalculateFaceArea ( faceTri, nodesOverlap );

                for ( int k = 0; k < triquadrule.GetPoints(); k++ )
                {
                    double * dGL = dG[k];

                    // Get the nodal location of this point
                    double dX[3];

                    dX[0] = dGL[0] * node0.x + dGL[1] * node1.x + dGL[2] * node2.x;
                    dX[1] = dGL[0] * node0.y + dGL[1] * node1.y + dGL[2] * node2.y;
                    dX[2] = dGL[0] * node0.z + dGL[1] * node1.z + dGL[2] * node2.z;

                    double dMag =
                        sqrt ( dX[0] * dX[0] + dX[1] * dX[1] + dX[2] * dX[2] );

                    dX[0] /= dMag;
                    dX[1] /= dMag;
                    dX[2] /= dMag;

                    Node nodeQuadrature ( dX[0], dX[1], dX[2] );

                    // Find the components of this quadrature point in the basis
                    // of the first Face.
                    double dAlphaIn;
                    double dBetaIn;

                    ApplyInverseMap (
                        faceFirst,
                        nodesFirst,
                        nodeQuadrature,
                        dAlphaIn,
                        dBetaIn );

                    // Find the components of this quadrature point in the basis
                    // of the second Face.
                    double dAlphaOut;
                    double dBetaOut;

                    ApplyInverseMap (
                        faceSecond,
                        nodesSecond,
                        nodeQuadrature,
                        dAlphaOut,
                        dBetaOut );

                    /*
                                        // Check inverse map value
                                        if ((dAlphaIn < 0.0) || (dAlphaIn > 1.0) ||
                                            (dBetaIn  < 0.0) || (dBetaIn  > 1.0)
                                        ) {
                                            _EXCEPTION2("Inverse Map out of range (%1.5e %1.5e)",
                                                dAlphaIn, dBetaIn);
                                        }

                                        // Check inverse map value
                                        if ((dAlphaOut < 0.0) || (dAlphaOut > 1.0) ||
                                            (dBetaOut  < 0.0) || (dBetaOut  > 1.0)
                                        ) {
                                            _EXCEPTION2("Inverse Map out of range (%1.5e %1.5e)",
                                                dAlphaOut, dBetaOut);
                                        }
                    */
                    // Sample the First finite element at this point
                    SampleGLLFiniteElement (
                        nMonotoneType,
                        nPin,
                        dAlphaIn,
                        dBetaIn,
                        dSampleCoeffIn );

                    // Sample the Second finite element at this point
                    SampleGLLFiniteElement (
                        nMonotoneType,
                        nPout,
                        dAlphaOut,
                        dBetaOut,
                        dSampleCoeffOut );

                    // Overlap output area
                    for ( int s = 0; s < nPout; s++ )
                    {
                        for ( int t = 0; t < nPout; t++ )
                        {
                            double dNodeArea =
                                dSampleCoeffOut[s][t]
                                * dW[k]
                                * dTriArea;

                            dOverlapOutputArea[ixOverlap + i][s * nPout + t] +=
                                dNodeArea;

                            dGeometricOutputArea[ixSecond][s * nPout + t] +=
                                dNodeArea;
                        }
                    }

                    // Compute overlap integral
                    int ixp = 0;
                    for ( int p = 0; p < nPin; p++ )
                    {
                        for ( int q = 0; q < nPin; q++ )
                        {

                            int ixs = 0;
                            for ( int s = 0; s < nPout; s++ )
                            {
                                for ( int t = 0; t < nPout; t++ )
                                {

                                    // Sample the Second finite element at this point
                                    dGlobalIntArray[ixp][ixOverlap + i][ixs] +=
                                        dSampleCoeffOut[s][t]
                                        * dSampleCoeffIn[p][q]
                                        * dW[k]
                                        * dTriArea;

                                    ixs++;
                                }
                            }

                            ixp++;
                        }
                    }
                }
            }
        }

        // Coefficients
        DataMatrix<double> dCoeff;
        dCoeff.Initialize ( nOverlapFaces * nPout * nPout, nPin * nPin );

        for ( int i = 0; i < nOverlapFaces; i++ )
        {
            // int ixSecondFace = m_meshOverlap->vecTargetFaceIx[ixOverlap + i];

            int ixp = 0;
            for ( int p = 0; p < nPin; p++ )
            {
                for ( int q = 0; q < nPin; q++ )
                {

                    int ixs = 0;
                    for ( int s = 0; s < nPout; s++ )
                    {
                        for ( int t = 0; t < nPout; t++ )
                        {
                            dCoeff[i * nPout * nPout + ixs][ixp] =
                                dGlobalIntArray[ixp][ixOverlap + i][ixs]
                                / dOverlapOutputArea[ixOverlap + i][s * nPout + t];

                            ixs++;
                        }
                    }

                    ixp++;
                }
            }
        }

        // Source areas
        DataVector<double> vecSourceArea ( nPin * nPin );

        for ( int p = 0; p < nPin; p++ )
        {
            for ( int q = 0; q < nPin; q++ )
            {
                vecSourceArea[p * nPin + q] =
                    dataGLLJacobianIn[p][q][ixFirst];
            }
        }

        // Target areas
        DataVector<double> vecTargetArea ( nOverlapFaces * nPout * nPout );

        for ( int i = 0; i < nOverlapFaces; i++ )
        {
            // int ixSecond = m_meshOverlap->vecTargetFaceIx[ixOverlap + i];

            int ixs = 0;
            for ( int s = 0; s < nPout; s++ )
            {
                for ( int t = 0; t < nPout; t++ )
                {

                    vecTargetArea[i * nPout * nPout + ixs] =
                        dOverlapOutputArea[ixOverlap + i][nPout * s + t];

                    ixs++;
                }
            }
        }

        // Force consistency and conservation
        if ( !fNoConservation )
        {
            ForceIntArrayConsistencyConservation (
                vecSourceArea,
                vecTargetArea,
                dCoeff,
                ( nMonotoneType != 0 ) );
        }

        // Update global coefficients
        for ( int i = 0; i < nOverlapFaces; i++ )
        {
            int ixp = 0;
            for ( int p = 0; p < nPin; p++ )
            {
                for ( int q = 0; q < nPin; q++ )
                {
                    int ixs = 0;
                    for ( int s = 0; s < nPout; s++ )
                    {
                        for ( int t = 0; t < nPout; t++ )
                        {

                            dGlobalIntArray[ixp][ixOverlap + i][ixs] =
                                dCoeff[i * nPout * nPout + ixs][ixp]
                                * dOverlapOutputArea[ixOverlap + i][s * nPout + t];

                            ixs++;
                        }
                    }

                    ixp++;
                }
            }
        }
        /*
                // Check column sums (conservation)
                for (int i = 0; i < nPin * nPin; i++) {
                    double dColSum = 0.0;
                    for (int j = 0; j < nOverlapFaces * nPout * nPout; j++) {
                        dColSum += dCoeff[j][i] * vecTargetArea[j];
                    }
                    printf("Col %i: %1.15e\n", i, dColSum / vecSourceArea[i]);
                }

                // Check row sums (consistency)
                for (int j = 0; j < nOverlapFaces * nPout * nPout; j++) {
                    double dRowSum = 0.0;
                    for (int i = 0; i < nPin * nPin; i++) {
                        dRowSum += dCoeff[j][i];
                    }
                    printf("Row %i: %1.15e\n", j, dRowSum);
                }
                _EXCEPTION();
        */

        // Increment the current overlap index
        ixOverlap += nOverlapFaces;
    }

    // Build redistribution map within target element
    Announce ( "Building redistribution maps on target mesh" );
    DataVector<double> dRedistSourceArea ( nPout * nPout );
    DataVector<double> dRedistTargetArea ( nPout * nPout );
    std::vector< DataMatrix<double> > dRedistributionMaps;
    dRedistributionMaps.resize ( m_meshOutput->faces.size() );

    for ( size_t ixSecond = 0; ixSecond < m_meshOutput->faces.size(); ixSecond++ )
    {
        dRedistributionMaps[ixSecond].Initialize (
            nPout * nPout, nPout * nPout );

        for ( int i = 0; i < nPout * nPout; i++ )
        {
            dRedistributionMaps[ixSecond][i][i] = 1.0;
        }

        for ( int s = 0; s < nPout * nPout; s++ )
        {
            dRedistSourceArea[s] =
                dGeometricOutputArea[ixSecond][s];
        }

        for ( int s = 0; s < nPout * nPout; s++ )
        {
            dRedistTargetArea[s] =
                dataGLLJacobianOut[s / nPout][s % nPout][ixSecond];
        }

        if ( !fNoConservation )
        {
            ForceIntArrayConsistencyConservation (
                dRedistSourceArea,
                dRedistTargetArea,
                dRedistributionMaps[ixSecond],
                ( nMonotoneType != 0 ) );

            for ( int s = 0; s < nPout * nPout; s++ )
            {
                for ( int t = 0; t < nPout * nPout; t++ )
                {
                    dRedistributionMaps[ixSecond][s][t] *=
                        dRedistTargetArea[s] / dRedistSourceArea[t];
                }
            }
        }
    }

    // Construct the total geometric area
    DataVector<double> dTotalGeometricArea ( dataNodalAreaOut.GetRows() );
    for ( size_t ixSecond = 0; ixSecond < m_meshOutput->faces.size(); ixSecond++ )
    {
        for ( int s = 0; s < nPout; s++ )
        {
            for ( int t = 0; t < nPout; t++ )
            {
                dTotalGeometricArea[dataGLLNodesOut[s][t][ixSecond] - 1]
                += dGeometricOutputArea[ixSecond][s * nPout + t];
            }
        }
    }

    // Compose the integration operator with the output map
    ixOverlap = 0;

    Announce ( "Assembling map" );

    // Map from source DOFs to target DOFs with redistribution applied
    DataMatrix<double> dRedistributedOp (
        nPin * nPin, nPout * nPout );

    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
#ifdef VERBOSE
        // Announce computation progress
        if ( ixFirst % outputFrequency == 0 )
        {
            Announce ( "Element %i", ixFirst );
        }
#endif

        // Number of overlapping Faces and triangles
        int nOverlapFaces = nAllOverlapFaces[ixFirst];

        // Put composed array into map
        for ( int j = 0; j < nOverlapFaces; j++ )
        {
            int ixSecondFace = m_meshOverlap->vecTargetFaceIx[ixOverlap + j];

            dRedistributedOp.Zero();
            for ( int p = 0; p < nPin * nPin; p++ )
            {
                for ( int s = 0; s < nPout * nPout; s++ )
                {
                    for ( int t = 0; t < nPout * nPout; t++ )
                    {
                        dRedistributedOp[p][s] +=
                            dRedistributionMaps[ixSecondFace][s][t]
                            * dGlobalIntArray[p][ixOverlap + j][t];
                    }
                }
            }

            int ixp = 0;
            for ( int p = 0; p < nPin; p++ )
            {
                for ( int q = 0; q < nPin; q++ )
                {

                    int ixFirstNode;
                    if ( fContinuousIn )
                    {
                        ixFirstNode = dataGLLNodesIn[p][q][ixFirst] - 1;
                    }
                    else
                    {
                        ixFirstNode = ixFirst * nPin * nPin + p * nPin + q;
                    }

                    int ixs = 0;
                    for ( int s = 0; s < nPout; s++ )
                    {
                        for ( int t = 0; t < nPout; t++ )
                        {

                            int ixSecondNode;
                            if ( fContinuousOut )
                            {
                                ixSecondNode = dataGLLNodesOut[s][t][ixSecondFace] - 1;

                                if ( !fNoConservation )
                                {
                                    smatMap ( ixSecondNode, ixFirstNode ) +=
                                        dRedistributedOp[ixp][ixs]
                                        / dataNodalAreaOut[ixSecondNode];
                                }
                                else
                                {
                                    smatMap ( ixSecondNode, ixFirstNode ) +=
                                        dRedistributedOp[ixp][ixs]
                                        / dTotalGeometricArea[ixSecondNode];
                                }

                            }
                            else
                            {
                                ixSecondNode =
                                    ixSecondFace * nPout * nPout + s * nPout + t;

                                if ( !fNoConservation )
                                {
                                    smatMap ( ixSecondNode, ixFirstNode ) +=
                                        dRedistributedOp[ixp][ixs]
                                        / dataGLLJacobianOut[s][t][ixSecondFace];
                                }
                                else
                                {
                                    smatMap ( ixSecondNode, ixFirstNode ) +=
                                        dRedistributedOp[ixp][ixs]
                                        / dGeometricOutputArea[ixSecondFace][s * nPout + t];
                                }
                            }

                            ixs++;
                        }
                    }

                    ixp++;
                }
            }
        }

        // Increment the current overlap index
        ixOverlap += nOverlapFaces;
    }

    return;
}

///////////////////////////////////////////////////////////////////////////////

void moab::TempestOfflineMap::LinearRemapGLLtoGLL2_Pointwise_MOAB (
    const DataMatrix3D<int> & dataGLLNodesIn,
    const DataMatrix3D<double> & /*dataGLLJacobianIn*/,
    const DataMatrix3D<int> & dataGLLNodesOut,
    const DataMatrix3D<double> & /*dataGLLJacobianOut*/,
    const DataVector<double> & dataNodalAreaOut,
    int nPin,
    int nPout,
    int nMonotoneType,
    bool fContinuousIn,
    bool fContinuousOut
)
{
    // Gauss-Lobatto quadrature within Faces
    DataVector<double> dGL;
    DataVector<double> dWL;

    GaussLobattoQuadrature::GetPoints ( nPout, 0.0, 1.0, dGL, dWL );

    // Utilities
    MeshUtilitiesFuzzy utils;

    // Get SparseMatrix represntation of the OfflineMap
    SparseMatrix<double> & smatMap = this->GetSparseMatrix();

    // Sample coefficients
    DataMatrix<double> dSampleCoeffIn;
    dSampleCoeffIn.Initialize ( nPin, nPin );

    // Announcemnets
    if ( is_root )
    {
        Announce ( "[moab::TempestOfflineMap::LinearRemapGLLtoGLL2_Pointwise_MOAB] Finite Element to Finite Element (Pointwise) Projection" );
        Announce ( "Order of the input FE polynomial interpolant: %i", nPin );
        Announce ( "Order of the output FE polynomial interpolant: %i", nPout );
    }

    // Number of overlap Faces per source Face
    DataVector<int> nAllOverlapFaces;
    nAllOverlapFaces.Initialize ( m_meshInputCov->faces.size() );

    int ixOverlap = 0;

    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
        size_t ixOverlapTemp = ixOverlap;
        for ( ; ixOverlapTemp < m_meshOverlap->faces.size(); ixOverlapTemp++ )
        {
            // const Face & faceOverlap = m_meshOverlap->faces[ixOverlapTemp];

            if ( ixFirst - m_meshOverlap->vecSourceFaceIx[ixOverlapTemp] != 0 )
                break;

            nAllOverlapFaces[ixFirst]++;
        }

        // Increment the current overlap index
        ixOverlap += nAllOverlapFaces[ixFirst];
    }

    // Number of times this point was found
    DataVector<bool> fSecondNodeFound ( dataNodalAreaOut.GetRows() );

    ixOverlap = 0;
#ifdef VERBOSE
    const unsigned outputFrequency = (m_meshInputCov->faces.size()/10);
#endif

    // Loop through all faces on m_meshInputCov
    for ( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
#ifdef VERBOSE
        // Announce computation progress
        if ( ixFirst % outputFrequency == 0 )
        {
            Announce ( "Element %i", ixFirst );
        }
#endif

        // Quantities from the First Mesh
        const Face & faceFirst = m_meshInputCov->faces[ixFirst];

        const NodeVector & nodesFirst = m_meshInputCov->nodes;

        // Number of overlapping Faces and triangles
        int nOverlapFaces = nAllOverlapFaces[ixFirst];

        // Loop through all Overlap Faces
        for ( int i = 0; i < nOverlapFaces; i++ )
        {
            // Quantities from the overlap Mesh
            // const Face & faceOverlap = m_meshOverlap->faces[ixOverlap + i];

            // const NodeVector & nodesOverlap = m_meshOverlap->nodes;

            // size_t nOverlapTriangles = faceOverlap.edges.size() - 2;

            // Quantities from the Second Mesh
            int ixSecond = m_meshOverlap->vecTargetFaceIx[ixOverlap + i];

            const NodeVector & nodesSecond = m_meshOutput->nodes;

            const Face & faceSecond = m_meshOutput->faces[ixSecond];

            // Loop through all nodes on the second face
            for ( int s = 0; s < nPout; s++ )
            {
                for ( int t = 0; t < nPout; t++ )
                {
                    size_t ixSecondNode;
                    if ( fContinuousOut )
                    {
                        ixSecondNode = dataGLLNodesOut[s][t][ixSecond] - 1;
                    }
                    else
                    {
                        ixSecondNode =
                            ixSecond * nPout * nPout + s * nPout + t;
                    }

                    if ( ixSecondNode >= fSecondNodeFound.GetRows() )
                    {
                        _EXCEPTIONT ( "Logic error" );
                    }

                    // Check if this node has been found already
                    if ( fSecondNodeFound[ixSecondNode] )
                    {
                        continue;
                    }

                    // Check this node
                    Node node;
                    Node dDx1G;
                    Node dDx2G;

                    ApplyLocalMap (
                        faceSecond,
                        nodesSecond,
                        dGL[t],
                        dGL[s],
                        node,
                        dDx1G,
                        dDx2G );

                    // Find the components of this quadrature point in the basis
                    // of the first Face.
                    double dAlphaIn;
                    double dBetaIn;

                    ApplyInverseMap (
                        faceFirst,
                        nodesFirst,
                        node,
                        dAlphaIn,
                        dBetaIn );

                    // Check if this node is within the first Face
                    if ( ( dAlphaIn < -1.0e-10 ) || ( dAlphaIn > 1.0 + 1.0e-10 ) ||
                            ( dBetaIn  < -1.0e-10 ) || ( dBetaIn  > 1.0 + 1.0e-10 )
                       )
                    {
                        continue;
                    }

                    // Node is within the overlap region, mark as found
                    fSecondNodeFound[ixSecondNode] = true;

                    // Sample the First finite element at this point
                    SampleGLLFiniteElement (
                        nMonotoneType,
                        nPin,
                        dAlphaIn,
                        dBetaIn,
                        dSampleCoeffIn );

                    // Add to map
                    for ( int p = 0; p < nPin; p++ )
                    {
                        for ( int q = 0; q < nPin; q++ )
                        {
                            int ixFirstNode;
                            if ( fContinuousIn )
                            {
                                ixFirstNode = dataGLLNodesIn[p][q][ixFirst] - 1;
                            }
                            else
                            {
                                ixFirstNode =
                                    ixFirst * nPin * nPin + p * nPin + q;
                            }

                            smatMap ( ixSecondNode, ixFirstNode ) +=
                                dSampleCoeffIn[p][q];
                        }
                    }
                }
            }
        }

        // Increment the current overlap index
        ixOverlap += nOverlapFaces;
    }

    // Check for missing samples
    for ( size_t i = 0; i < fSecondNodeFound.GetRows(); i++ )
    {
        if ( !fSecondNodeFound[i] )
        {
            _EXCEPTION1 ( "Can't sample point %i", i );
        }
    }

    return;
}

///////////////////////////////////////////////////////////////////////////////

