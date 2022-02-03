///////////////////////////////////////////////////////////////////////////////
///
/// \file    TempestLinearRemap.cpp
/// \author  Vijay Mahadevan
/// \version Mar 08, 2017
///

#ifdef WIN32               /* windows */
#define _USE_MATH_DEFINES  // For M_PI
#endif
#include "Announce.h"
#include "DataArray3D.h"
#include "FiniteElementTools.h"
// #include "LinearRemapFV.h"
#include "GaussLobattoQuadrature.h"
#include "TriangularQuadrature.h"
#include "MeshUtilitiesFuzzy.h"
#include "MeshUtilitiesExact.h"
#include "MathHelper.h"
#include "SparseMatrix.h"
#include "OverlapMesh.h"

#include "DebugOutput.hpp"
#include "moab/AdaptiveKDTree.hpp"

#include "moab/Remapping/TempestOnlineMap.hpp"
#include "moab/TupleList.hpp"

#ifdef MOAB_HAVE_EIGEN3
#include <Eigen/Dense>
#endif

#include <fstream>
#include <cmath>
#include <cstdlib>
#include <sstream>

// #define VERBOSE

extern void BuildIntegrationArray( const Mesh& m_meshInput, const Mesh& m_meshOverlap,
                                   const TriangularQuadratureRule& triquadrule, int ixFirstFace, int ixOverlapBegin,
                                   int ixOverlapEnd, int nOrder, DataArray2D< double >& dIntArray );

extern void InvertFitArray_Corrected( const DataArray1D< double >& dConstraint, DataArray2D< double >& dFitArray,
                                      DataArray1D< double >& dFitWeights, DataArray2D< double >& dFitArrayPlus );

/// <summary>
///     Face index and distance metric pair.
/// </summary>
typedef std::pair< int, int > FaceDistancePair;

/// <summary>
///     Vector storing adjacent Faces.
/// </summary>
typedef std::vector< FaceDistancePair > AdjacentFaceVector;

extern void BuildFitArray( const Mesh& mesh, const TriangularQuadratureRule& triquadrule, int ixFirst,
                           const AdjacentFaceVector& vecAdjFaces, int nOrder, int nFitWeightsExponent,
                           const DataArray1D< double >& dConstraint, DataArray2D< double >& dFitArray,
                           DataArray1D< double >& dFitWeights );

extern void GetAdjacentFaceVectorByEdge( const Mesh& mesh, int iFaceInitial, int nRequiredFaceSetSize,
                                         AdjacentFaceVector& vecFaces );

///////////////////////////////////////////////////////////////////////////////

moab::ErrorCode moab::TempestOnlineMap::LinearRemapNN_MOAB( bool use_GID_matching, bool strict_check )
{
    /* m_mapRemap size = (m_nTotDofs_Dest X m_nTotDofs_SrcCov)  */

#ifdef VVERBOSE
    {
        std::ofstream output_file( "rowcolindices.txt", std::ios::out );
        output_file << m_nTotDofs_Dest << " " << m_nTotDofs_SrcCov << " " << row_gdofmap.size() << " "
                    << row_ldofmap.size() << " " << col_gdofmap.size() << " " << col_ldofmap.size() << "\n";
        output_file << "Rows \n";
        for( unsigned iv = 0; iv < row_gdofmap.size(); iv++ )
            output_file << row_gdofmap[iv] << " " << row_dofmap[iv] << "\n";
        output_file << "Cols \n";
        for( unsigned iv = 0; iv < col_gdofmap.size(); iv++ )
            output_file << col_gdofmap[iv] << " " << col_dofmap[iv] << "\n";
        output_file.flush();  // required here
        output_file.close();
    }
#endif

    if( use_GID_matching )
    {
        std::map< unsigned, unsigned > src_gl;
        for( unsigned it = 0; it < col_gdofmap.size(); ++it )
            src_gl[col_gdofmap[it]] = it;

        std::map< unsigned, unsigned >::iterator iter;
        for( unsigned it = 0; it < row_gdofmap.size(); ++it )
        {
            unsigned row = row_gdofmap[it];
            iter         = src_gl.find( row );
            if( strict_check && iter == src_gl.end() )
            {
                std::cout << "Searching for global target DOF " << row
                          << " but could not find correspondence in source mesh.\n";
                assert( false );
            }
            else if( iter == src_gl.end() )
            {
                continue;
            }
            else
            {
                unsigned icol = src_gl[row];
                unsigned irow = it;

                // Set the permutation matrix in local space
                m_mapRemap( irow, icol ) = 1.0;
            }
        }

        return moab::MB_SUCCESS;
    }
    else
    {
        /* Create a Kd-tree to perform local queries to find nearest neighbors */

        return moab::MB_SUCCESS;
    }
}

///////////////////////////////////////////////////////////////////////////////

void moab::TempestOnlineMap::LinearRemapFVtoFV_Tempest_MOAB( int nOrder )
{
    // Order of triangular quadrature rule
    const int TriQuadRuleOrder = 4;

    // Verify ReverseNodeArray has been calculated
    if( m_meshInputCov->faces.size() > 0 && m_meshInputCov->revnodearray.size() == 0 )
    {
        _EXCEPTIONT( "ReverseNodeArray has not been calculated for m_meshInput" );
    }

    // Triangular quadrature rule
    TriangularQuadratureRule triquadrule( TriQuadRuleOrder );

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
    moab::DebugOutput dbgprint( std::cout, this->rank, 0 );
    dbgprint.set_prefix( "[LinearRemapFVtoFV_Tempest_MOAB]: " );
    if( is_root )
    {
        dbgprint.printf( 0, "Finite Volume to Finite Volume Projection\n" );
        dbgprint.printf( 0, "Triangular quadrature rule order %i\n", TriQuadRuleOrder );
        dbgprint.printf( 0, "Number of coefficients: %i\n", nCoefficients );
        dbgprint.printf( 0, "Required adjacency set size: %i\n", nRequiredFaceSetSize );
        dbgprint.printf( 0, "Fit weights exponent: %i\n", nFitWeightsExponent );
    }

    // Current overlap face
    int ixOverlap                  = 0;
    const unsigned outputFrequency = ( m_meshInputCov->faces.size() / 10 ) + 1;

    // Loop through all faces on m_meshInput
    for( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
        // Output every 1000 elements
        if( ixFirst % outputFrequency == 0 && is_root )
        {
            dbgprint.printf( 0, "Element %zu/%lu\n", ixFirst, m_meshInputCov->faces.size() );
        }

        // Find the set of Faces that overlap faceFirst
        int ixOverlapBegin    = ixOverlap;
        unsigned ixOverlapEnd = ixOverlapBegin;

        for( ; ixOverlapEnd < m_meshOverlap->faces.size(); ixOverlapEnd++ )
        {
            if( ixFirst - m_meshOverlap->vecSourceFaceIx[ixOverlapEnd] != 0 )
            {
                break;
            }
        }

        unsigned nOverlapFaces = ixOverlapEnd - ixOverlapBegin;
        // if ( is_root ) Announce ( "Element %i / %i :: [%i, %i]", ixFirst,
        // m_meshInputCov->faces.size(), ixOverlapBegin, ixOverlapEnd );

        if( nOverlapFaces == 0 ) continue;

        // Build integration array
        DataArray2D< double > dIntArray;

        BuildIntegrationArray( *m_meshInputCov, *m_meshOverlap, triquadrule, ixFirst, ixOverlapBegin, ixOverlapEnd,
                               nOrder, dIntArray );

        // Set of Faces to use in building the reconstruction and associated
        // distance metric.
        AdjacentFaceVector vecAdjFaces;

        GetAdjacentFaceVectorByEdge( *m_meshInputCov, ixFirst, nRequiredFaceSetSize, vecAdjFaces );

        // Number of adjacent Faces
        int nAdjFaces = vecAdjFaces.size();

        // Determine the conservative constraint equation
        DataArray1D< double > dConstraint( nCoefficients );

        double dFirstArea = m_meshInputCov->vecFaceArea[ixFirst];

        for( int p = 0; p < nCoefficients; p++ )
        {
            for( unsigned j = 0; j < nOverlapFaces; j++ )
            {
                dConstraint[p] += dIntArray[p][j];
            }
            dConstraint[p] /= dFirstArea;
        }

        // Build the fit array from the integration operator
        DataArray2D< double > dFitArray;
        DataArray1D< double > dFitWeights;
        DataArray2D< double > dFitArrayPlus;

        BuildFitArray( *m_meshInputCov, triquadrule, ixFirst, vecAdjFaces, nOrder, nFitWeightsExponent, dConstraint,
                       dFitArray, dFitWeights );

        // Compute the inverse fit array
        InvertFitArray_Corrected( dConstraint, dFitArray, dFitWeights, dFitArrayPlus );

        // Multiply integration array and fit array
        DataArray2D< double > dComposedArray( nAdjFaces, nOverlapFaces );

        for( int i = 0; i < nAdjFaces; i++ )
        {
            for( unsigned j = 0; j < nOverlapFaces; j++ )
            {
                for( int k = 0; k < nCoefficients; k++ )
                {
                    dComposedArray[i][j] += dIntArray[k][j] * dFitArrayPlus[i][k];
                }
            }
        }

        // Put composed array into map
        for( unsigned i = 0; i < vecAdjFaces.size(); i++ )
        {
            for( unsigned j = 0; j < nOverlapFaces; j++ )
            {
                int& ixFirstFaceLoc  = vecAdjFaces[i].first;
                int& ixSecondFaceLoc = m_meshOverlap->vecTargetFaceIx[ixOverlap + j];
                // int ixFirstFaceGlob = m_remapper->GetGlobalID(moab::Remapper::SourceMesh,
                // ixFirstFaceLoc); int ixSecondFaceGlob =
                // m_remapper->GetGlobalID(moab::Remapper::TargetMesh, ixSecondFaceLoc);

                // signal to not participate, because it is a ghost target
                if( ixSecondFaceLoc < 0 ) continue;  // do not do anything

                m_mapRemap( ixSecondFaceLoc, ixFirstFaceLoc ) +=
                    dComposedArray[i][j] / m_meshOutput->vecFaceArea[ixSecondFaceLoc];
            }
        }

        // Increment the current overlap index
        ixOverlap += nOverlapFaces;
    }

    return;
}

///////////////////////////////////////////////////////////////////////////////

#ifdef MOAB_HAVE_EIGEN3
void moab::TempestOnlineMap::copy_tempest_sparsemat_to_eigen3()
{
#ifndef VERBOSE
#define VERBOSE_ACTIVATED
// #define VERBOSE
#endif
    if( m_nTotDofs_Dest <= 0 || m_nTotDofs_SrcCov <= 0 )
    {
        // std::cout << rank << ": rowsize = " <<  m_nTotDofs_Dest << ", colsize = " <<
        // m_nTotDofs_SrcCov << "\n";
        return;  // No need to allocate if either rows or cols size are zero
    }

    /* Should the columns be the global size of the matrix ? */
    m_weightMatrix.resize( m_nTotDofs_Dest, m_nTotDofs_SrcCov );
    m_rowVector.resize( m_weightMatrix.rows() );
    m_colVector.resize( m_weightMatrix.cols() );

#ifdef VERBOSE
    int locrows = std::max( m_mapRemap.GetRows(), m_nTotDofs_Dest );
    int loccols = std::max( m_mapRemap.GetColumns(), m_nTotDofs_SrcCov );

    std::cout << m_weightMatrix.rows() << ", " << locrows << ", " << m_weightMatrix.cols() << ", " << loccols << "\n";
    // assert(m_weightMatrix.rows() == locrows && m_weightMatrix.cols() == loccols);
#endif

    DataArray1D< int > lrows;
    DataArray1D< int > lcols;
    DataArray1D< double > lvals;
    m_mapRemap.GetEntries( lrows, lcols, lvals );
    unsigned locvals = lvals.GetRows();

    // first matrix
    typedef Eigen::Triplet< double > Triplet;
    std::vector< Triplet > tripletList;
    tripletList.reserve( locvals );
    for( int iv = 0; iv < locvals; iv++ )
    {
        tripletList.push_back( Triplet( lrows[iv], lcols[iv], lvals[iv] ) );
    }

    m_weightMatrix.setFromTriplets( tripletList.begin(), tripletList.end() );
    m_weightMatrix.makeCompressed();

    int nrows = m_weightMatrix.rows();      // Number of rows
    int ncols = m_weightMatrix.cols();      // Number of columns
    int NNZ   = m_weightMatrix.nonZeros();  // Number of non zero values
#ifdef MOAB_HAVE_MPI
    // find out min/max for NNZ, ncols, nrows
    // should work on std c++ 11
    int arr3[6] = { NNZ, nrows, ncols, -NNZ, -nrows, -ncols };
    int rarr3[6];
    MPI_Reduce( arr3, rarr3, 6, MPI_INT, MPI_MIN, 0, m_pcomm->comm() );

    int total[2];
    MPI_Reduce( arr3, total, 2, MPI_INT, MPI_SUM, 0, m_pcomm->comm() );
    if( !rank )
        std::cout << " Rows:(" << rarr3[1] << ", " << -rarr3[4] << "), Cols:(" << rarr3[2] << ", " << -rarr3[5]
                  << "), NNZ:(" << rarr3[0] << ", " << -rarr3[3] << "),  total NNZ:" << total[0]
                  << " total rows:" << total[1] << "\n";
#else
    std::cout << "nr rows: " << nrows << " cols: " << ncols << " non-zeros: " << NNZ << "\n";
#endif

#ifdef VERBOSE
    std::stringstream sstr;
    sstr << "tempestmatrix.txt.0000" << rank;
    std::ofstream output_file( sstr.str(), std::ios::out );
    output_file << "0 " << locrows << " 0 " << loccols << "\n";
    for( unsigned iv = 0; iv < locvals; iv++ )
    {
        // output_file << lrows[iv] << " " << row_ldofmap[lrows[iv]] << " " <<
        // row_gdofmap[row_ldofmap[lrows[iv]]] << " " << col_gdofmap[col_ldofmap[lcols[iv]]] << " "
        // << lvals[iv] << "\n";
        output_file << row_gdofmap[row_ldofmap[lrows[iv]]] << " " << col_gdofmap[col_ldofmap[lcols[iv]]] << " "
                    << lvals[iv] << "\n";
    }
    output_file.flush();  // required here
    output_file.close();
#endif

#ifdef VERBOSE_ACTIVATED
#undef VERBOSE_ACTIVATED
#undef VERBOSE
#endif
    return;
}

///////////////////////////////////////////////////////////////////////////////
//#define VERBOSE
moab::ErrorCode moab::TempestOnlineMap::ApplyWeights( std::vector< double >& srcVals, std::vector< double >& tgtVals,
                                                      bool transpose )
{
    // Reset the source and target data first
    m_rowVector.setZero();
    m_colVector.setZero();

#ifdef VERBOSE
    std::stringstream sstr;
    static int callId=0;
    callId++;
    sstr << "projection_id_" <<callId<<"_s_"<< size << "_rk_" << rank << ".txt";
    std::ofstream output_file( sstr.str() );
#endif
    // Perform the actual projection of weights: application of weight matrix onto the source
    // solution vector
    if( transpose )
    {
        // Permute the source data first
        for( unsigned i = 0; i < srcVals.size(); ++i )
        {
            if( row_dtoc_dofmap[i] >= 0 )
                m_rowVector( row_dtoc_dofmap[i] ) = srcVals[i];  // permute and set the row (source) vector properly
        }

        m_colVector = m_weightMatrix.adjoint() * m_rowVector;

        // Permute the resulting target data back
        for( unsigned i = 0; i < tgtVals.size(); ++i )
        {
            if( col_dtoc_dofmap[i] >= 0 )
                tgtVals[i] = m_colVector( col_dtoc_dofmap[i] );  // permute and set the row (source) vector properly
        }
    }
    else
    {
        // Permute the source data first
#ifdef VERBOSE
        output_file << "ColVector: " << m_colVector.size() << ", SrcVals: " << srcVals.size()
                    << ", Sizes: " << m_nTotDofs_SrcCov << ", " << col_dtoc_dofmap.size() << "\n";
#endif
        for( unsigned i = 0; i < srcVals.size(); ++i )
        {
            if( col_dtoc_dofmap[i] >= 0 )
            {
                m_colVector( col_dtoc_dofmap[i] ) = srcVals[i];  // permute and set the row (source) vector properly
#ifdef VERBOSE
                output_file  << i << " " << col_gdofmap[col_dtoc_dofmap[i]] + 1 << "  " << srcVals[i] << "\n";
#endif
            }
        }

        m_rowVector = m_weightMatrix * m_colVector;

        // Permute the resulting target data back
#ifdef VERBOSE
        output_file << "RowVector: " << m_rowVector.size() << ", TgtVals:" << tgtVals.size()
                    << ", Sizes: " << m_nTotDofs_Dest << ", " << row_gdofmap.size() << "\n";
#endif
        for( unsigned i = 0; i < tgtVals.size(); ++i )
        {
            if( row_dtoc_dofmap[i] >= 0 )
            {
                tgtVals[i] = m_rowVector( row_dtoc_dofmap[i] );  // permute and set the row (source) vector properly
#ifdef VERBOSE
                 output_file << i << " " << row_gdofmap[row_dtoc_dofmap[i]]+1  << "  " << tgtVals[i] << "\n";
#endif
            }
        }
    }

#ifdef VERBOSE
    output_file.flush();  // required here
    output_file.close();
#endif

    // All done with matvec application
    return moab::MB_SUCCESS;
}

#endif
//#undef VERBOSE
///////////////////////////////////////////////////////////////////////////////

extern void ForceConsistencyConservation3( const DataArray1D< double >& vecSourceArea,
                                           const DataArray1D< double >& vecTargetArea, DataArray2D< double >& dCoeff,
                                           bool fMonotone, bool fSparseConstraints = false );

///////////////////////////////////////////////////////////////////////////////

extern void ForceIntArrayConsistencyConservation( const DataArray1D< double >& vecSourceArea,
                                                  const DataArray1D< double >& vecTargetArea,
                                                  DataArray2D< double >& dCoeff, bool fMonotone );

///////////////////////////////////////////////////////////////////////////////

void moab::TempestOnlineMap::LinearRemapSE4_Tempest_MOAB( const DataArray3D< int >& dataGLLNodes,
                                                          const DataArray3D< double >& dataGLLJacobian,
                                                          int nMonotoneType, bool fContinuousIn, bool fNoConservation )
{
    // Order of the polynomial interpolant
    int nP = dataGLLNodes.GetRows();

    // Order of triangular quadrature rule
    const int TriQuadRuleOrder = 4;

    // Triangular quadrature rule
    TriangularQuadratureRule triquadrule( TriQuadRuleOrder );

    int TriQuadraturePoints = triquadrule.GetPoints();

    const DataArray2D< double >& TriQuadratureG = triquadrule.GetG();

    const DataArray1D< double >& TriQuadratureW = triquadrule.GetW();

    // Sample coefficients
    DataArray2D< double > dSampleCoeff( nP, nP );

    // GLL Quadrature nodes on quadrilateral elements
    DataArray1D< double > dG;
    DataArray1D< double > dW;
    GaussLobattoQuadrature::GetPoints( nP, 0.0, 1.0, dG, dW );

    // Announcemnets
    moab::DebugOutput dbgprint( std::cout, this->rank, 0 );
    dbgprint.set_prefix( "[LinearRemapSE4_Tempest_MOAB]: " );
    if( is_root )
    {
        dbgprint.printf( 0, "Finite Element to Finite Volume Projection\n" );
        dbgprint.printf( 0, "Triangular quadrature rule order %i\n", TriQuadRuleOrder );
        dbgprint.printf( 0, "Order of the FE polynomial interpolant: %i\n", nP );
    }

    // Get SparseMatrix represntation of the OfflineMap
    SparseMatrix< double >& smatMap = this->GetSparseMatrix();

    // NodeVector from m_meshOverlap
    const NodeVector& nodesOverlap = m_meshOverlap->nodes;
    const NodeVector& nodesFirst   = m_meshInputCov->nodes;

    // Vector of source areas
    DataArray1D< double > vecSourceArea( nP * nP );

    DataArray1D< double > vecTargetArea;
    DataArray2D< double > dCoeff;

#ifdef VERBOSE
    std::stringstream sstr;
    sstr << "remapdata_" << rank << ".txt";
    std::ofstream output_file( sstr.str() );
#endif

    // Current Overlap Face
    int ixOverlap                  = 0;
    const unsigned outputFrequency = ( m_meshInputCov->faces.size() / 10 ) + 1;

    // generic triangle used for area computation, for triangles around the center of overlap face;
    // used for overlap faces with more than 4 edges;
    // nodes array will be set for each triangle;
    // these triangles are not part of the mesh structure, they are just temporary during
    //   aforementioned decomposition.
    Face faceTri( 3 );
    NodeVector nodes( 3 );
    faceTri.SetNode( 0, 0 );
    faceTri.SetNode( 1, 1 );
    faceTri.SetNode( 2, 2 );

    // Loop over all input Faces
    for( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
        const Face& faceFirst = m_meshInputCov->faces[ixFirst];

        if( faceFirst.edges.size() != 4 )
        {
            _EXCEPTIONT( "Only quadrilateral elements allowed for SE remapping" );
        }

        // Announce computation progress
        if( ixFirst % outputFrequency == 0 && is_root )
        {
            dbgprint.printf( 0, "Element %zu/%lu\n", ixFirst, m_meshInputCov->faces.size() );
        }

        // Need to re-number the overlap elements such that vecSourceFaceIx[a:b] = 0, then 1 and so
        // on wrt the input mesh data Then the overlap_end and overlap_begin will be correct.
        // However, the relation with MOAB and Tempest will go out of the roof

        // Determine how many overlap Faces and triangles are present
        int nOverlapFaces    = 0;
        size_t ixOverlapTemp = ixOverlap;
        for( ; ixOverlapTemp < m_meshOverlap->faces.size(); ixOverlapTemp++ )
        {
            // const Face & faceOverlap = m_meshOverlap->faces[ixOverlapTemp];
            if( ixFirst - m_meshOverlap->vecSourceFaceIx[ixOverlapTemp] != 0 )
            {
                break;
            }

            nOverlapFaces++;
        }

        // No overlaps
        if( nOverlapFaces == 0 )
        {
            continue;
        }

        // Allocate remap coefficients array for meshFirst Face
        DataArray3D< double > dRemapCoeff( nP, nP, nOverlapFaces );

        // Find the local remap coefficients
        for( int j = 0; j < nOverlapFaces; j++ )
        {
            const Face& faceOverlap = m_meshOverlap->faces[ixOverlap + j];
            if( m_meshOverlap->vecFaceArea[ixOverlap + j] < 1.e-16 )  // machine precision
            {
                Announce( "Very small overlap at index %i area polygon: (%1.10e )", ixOverlap + j,
                          m_meshOverlap->vecFaceArea[ixOverlap + j] );
                int n = faceOverlap.edges.size();
                Announce( "Number nodes: %d", n );
                for( int k = 0; k < n; k++ )
                {
                    Node nd = nodesOverlap[faceOverlap[k]];
                    Announce( "Node %d  %d  : %1.10e  %1.10e %1.10e ", k, faceOverlap[k], nd.x, nd.y, nd.z );
                }
                continue;
            }

            // #ifdef VERBOSE
            // if ( is_root )
            //     Announce ( "\tLocal ID: %i/%i = %i, areas = %2.8e", j + ixOverlap, nOverlapFaces,
            //     m_remapper->lid_to_gid_covsrc[m_meshOverlap->vecSourceFaceIx[ixOverlap + j]],
            //     m_meshOverlap->vecFaceArea[ixOverlap + j] );
            // #endif

            int nbEdges = faceOverlap.edges.size();
            int nOverlapTriangles = 1;
            Node center; // not used if nbEdges == 3
            if (nbEdges > 3) { // decompose from center in this case
                nOverlapTriangles = nbEdges;
                for (int k = 0; k < nbEdges; k++) {
                    const Node &node = nodesOverlap[faceOverlap[k]];
                    center = center + node;
                }
                center = center / nbEdges;
                center = center.Normalized();// project back on sphere of radius 1
            }

            Node node0, node1, node2;
            double dTriangleArea;

            // Loop over all sub-triangles of this Overlap Face
            for( int k = 0; k < nOverlapTriangles; k++ )
            {
                if (nbEdges == 3) // will come here only once, nOverlapTriangles == 1 in this case
                {
                    node0 = nodesOverlap[faceOverlap[0]];
                    node1 = nodesOverlap[faceOverlap[1]];
                    node2 = nodesOverlap[faceOverlap[2]];
                    dTriangleArea = CalculateFaceArea(faceOverlap, nodesOverlap);
                }
                else // decompose polygon in triangles around the center
                {
                    node0 = center;
                    node1 = nodesOverlap[faceOverlap[k]];
                    int k1 = (k + 1) % nbEdges;
                    node2 = nodesOverlap[faceOverlap[k1]];
                    nodes[0] = center;
                    nodes[1] = node1;
                    nodes[2] = node2;
                    dTriangleArea = CalculateFaceArea(faceTri, nodes);
                }
                // Coordinates of quadrature Node
                for( int l = 0; l < TriQuadraturePoints; l++ )
                {
                    Node nodeQuadrature;
                    nodeQuadrature.x = TriQuadratureG[l][0] * node0.x + TriQuadratureG[l][1] * node1.x +
                                       TriQuadratureG[l][2] * node2.x;

                    nodeQuadrature.y = TriQuadratureG[l][0] * node0.y + TriQuadratureG[l][1] * node1.y +
                                       TriQuadratureG[l][2] * node2.y;

                    nodeQuadrature.z = TriQuadratureG[l][0] * node0.z + TriQuadratureG[l][1] * node1.z +
                                       TriQuadratureG[l][2] * node2.z;

                    nodeQuadrature = nodeQuadrature.Normalized();

                    // Find components of quadrature point in basis
                    // of the first Face
                    double dAlpha;
                    double dBeta;

                    ApplyInverseMap( faceFirst, nodesFirst, nodeQuadrature, dAlpha, dBeta );

                    // Check inverse map value
                    if( ( dAlpha < -1.0e-13 ) || ( dAlpha > 1.0 + 1.0e-13 ) || ( dBeta < -1.0e-13 ) ||
                        ( dBeta > 1.0 + 1.0e-13 ) )
                    {
                        _EXCEPTION4( "Inverse Map for element %d and subtriangle %d out of range "
                                     "(%1.5e %1.5e)",
                                     j, l, dAlpha, dBeta );
                    }

                    // Sample the finite element at this point
                    SampleGLLFiniteElement( nMonotoneType, nP, dAlpha, dBeta, dSampleCoeff );

                    // Add sample coefficients to the map if m_meshOverlap->vecFaceArea[ixOverlap + j] > 0
                    for( int p = 0; p < nP; p++ )
                    {
                        for( int q = 0; q < nP; q++ )
                        {
                            dRemapCoeff[p][q][j] += TriQuadratureW[l] * dTriangleArea * dSampleCoeff[p][q] /
                                                    m_meshOverlap->vecFaceArea[ixOverlap + j];
                        }
                    }
                }
            }
        }

#ifdef VERBOSE
        output_file << "[" << m_remapper->lid_to_gid_covsrc[ixFirst] << "] \t";
        for( int j = 0; j < nOverlapFaces; j++ )
        {
            for( int p = 0; p < nP; p++ )
            {
                for( int q = 0; q < nP; q++ )
                {
                    output_file << dRemapCoeff[p][q][j] << " ";
                }
            }
        }
        output_file << std::endl;
#endif

        // Force consistency and conservation
        if( !fNoConservation )
        {
            double dTargetArea = 0.0;
            for( int j = 0; j < nOverlapFaces; j++ )
            {
                dTargetArea += m_meshOverlap->vecFaceArea[ixOverlap + j];
            }

            for( int p = 0; p < nP; p++ )
            {
                for( int q = 0; q < nP; q++ )
                {
                    vecSourceArea[p * nP + q] = dataGLLJacobian[p][q][ixFirst];
                }
            }

            const double areaTolerance = 1e-10;
            // Source elements are completely covered by target volumes
            if( fabs( m_meshInputCov->vecFaceArea[ixFirst] - dTargetArea ) <= areaTolerance )
            {
                vecTargetArea.Allocate( nOverlapFaces );
                for( int j = 0; j < nOverlapFaces; j++ )
                {
                    vecTargetArea[j] = m_meshOverlap->vecFaceArea[ixOverlap + j];
                }

                dCoeff.Allocate( nOverlapFaces, nP * nP );

                for( int j = 0; j < nOverlapFaces; j++ )
                {
                    for( int p = 0; p < nP; p++ )
                    {
                        for( int q = 0; q < nP; q++ )
                        {
                            dCoeff[j][p * nP + q] = dRemapCoeff[p][q][j];
                        }
                    }
                }

                // Target volumes only partially cover source elements
            }
            else if( m_meshInputCov->vecFaceArea[ixFirst] - dTargetArea > areaTolerance )
            {
                double dExtraneousArea = m_meshInputCov->vecFaceArea[ixFirst] - dTargetArea;

                vecTargetArea.Allocate( nOverlapFaces + 1 );
                for( int j = 0; j < nOverlapFaces; j++ )
                {
                    vecTargetArea[j] = m_meshOverlap->vecFaceArea[ixOverlap + j];
                }
                vecTargetArea[nOverlapFaces] = dExtraneousArea;

#ifdef VERBOSE
                Announce( "Partial volume: %i (%1.10e / %1.10e)", ixFirst, dTargetArea,
                          m_meshInputCov->vecFaceArea[ixFirst] );
#endif
                if( dTargetArea > m_meshInputCov->vecFaceArea[ixFirst] )
                {
                    _EXCEPTIONT( "Partial element area exceeds total element area" );
                }

                dCoeff.Allocate( nOverlapFaces + 1, nP * nP );

                for( int j = 0; j < nOverlapFaces; j++ )
                {
                    for( int p = 0; p < nP; p++ )
                    {
                        for( int q = 0; q < nP; q++ )
                        {
                            dCoeff[j][p * nP + q] = dRemapCoeff[p][q][j];
                        }
                    }
                }
                for( int p = 0; p < nP; p++ )
                {
                    for( int q = 0; q < nP; q++ )
                    {
                        dCoeff[nOverlapFaces][p * nP + q] = dataGLLJacobian[p][q][ixFirst];
                    }
                }
                for( int j = 0; j < nOverlapFaces; j++ )
                {
                    for( int p = 0; p < nP; p++ )
                    {
                        for( int q = 0; q < nP; q++ )
                        {
                            dCoeff[nOverlapFaces][p * nP + q] -=
                                dRemapCoeff[p][q][j] * m_meshOverlap->vecFaceArea[ixOverlap + j];
                        }
                    }
                }
                for( int p = 0; p < nP; p++ )
                {
                    for( int q = 0; q < nP; q++ )
                    {
                        dCoeff[nOverlapFaces][p * nP + q] /= dExtraneousArea;
                    }
                }

                // Source elements only partially cover target volumes
            }
            else
            {
                Announce( "Coverage area: %1.10e, and target element area: %1.10e)", ixFirst,
                          m_meshInputCov->vecFaceArea[ixFirst], dTargetArea );
                _EXCEPTIONT( "Target grid must be a subset of source grid" );
            }

            ForceConsistencyConservation3( vecSourceArea, vecTargetArea, dCoeff, ( nMonotoneType > 0 )
                                           /*, m_remapper->lid_to_gid_covsrc[ixFirst]*/ );

            for( int j = 0; j < nOverlapFaces; j++ )
            {
                for( int p = 0; p < nP; p++ )
                {
                    for( int q = 0; q < nP; q++ )
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
        for( int j = 0; j < nOverlapFaces; j++ )
        {
            int ixSecondFace = m_meshOverlap->vecTargetFaceIx[ixOverlap + j];

            // signal to not participate, because it is a ghost target
            if( ixSecondFace < 0 ) continue;  // do not do anything

            for( int p = 0; p < nP; p++ )
            {
                for( int q = 0; q < nP; q++ )
                {
                    if( fContinuousIn )
                    {
                        int ixFirstNode = dataGLLNodes[p][q][ixFirst] - 1;

                        smatMap( ixSecondFace, ixFirstNode ) += dRemapCoeff[p][q][j] *
                                                                m_meshOverlap->vecFaceArea[ixOverlap + j] /
                                                                m_meshOutput->vecFaceArea[ixSecondFace];
                    }
                    else
                    {
                        int ixFirstNode = ixFirst * nP * nP + p * nP + q;

                        smatMap( ixSecondFace, ixFirstNode ) += dRemapCoeff[p][q][j] *
                                                                m_meshOverlap->vecFaceArea[ixOverlap + j] /
                                                                m_meshOutput->vecFaceArea[ixSecondFace];
                    }
                }
            }
        }
        // Increment the current overlap index
        ixOverlap += nOverlapFaces;
    }
#ifdef VERBOSE
    output_file.flush();  // required here
    output_file.close();
#endif

    return;
}

///////////////////////////////////////////////////////////////////////////////

void moab::TempestOnlineMap::LinearRemapGLLtoGLL2_MOAB( const DataArray3D< int >& dataGLLNodesIn,
                                                        const DataArray3D< double >& dataGLLJacobianIn,
                                                        const DataArray3D< int >& dataGLLNodesOut,
                                                        const DataArray3D< double >& dataGLLJacobianOut,
                                                        const DataArray1D< double >& dataNodalAreaOut, int nPin,
                                                        int nPout, int nMonotoneType, bool fContinuousIn,
                                                        bool fContinuousOut, bool fNoConservation )
{
    // Triangular quadrature rule
    TriangularQuadratureRule triquadrule( 8 );

    const DataArray2D< double >& dG = triquadrule.GetG();
    const DataArray1D< double >& dW = triquadrule.GetW();

    // Get SparseMatrix represntation of the OfflineMap
    SparseMatrix< double >& smatMap = this->GetSparseMatrix();

    // Sample coefficients
    DataArray2D< double > dSampleCoeffIn( nPin, nPin );
    DataArray2D< double > dSampleCoeffOut( nPout, nPout );

    // Announcemnets
    moab::DebugOutput dbgprint( std::cout, this->rank, 0 );
    dbgprint.set_prefix( "[LinearRemapGLLtoGLL2_MOAB]: " );
    if( is_root )
    {
        dbgprint.printf( 0, "Finite Element to Finite Element Projection\n" );
        dbgprint.printf( 0, "Order of the input FE polynomial interpolant: %i\n", nPin );
        dbgprint.printf( 0, "Order of the output FE polynomial interpolant: %i\n", nPout );
    }

    // Build the integration array for each element on m_meshOverlap
    DataArray3D< double > dGlobalIntArray( nPin * nPin, m_meshOverlap->faces.size(), nPout * nPout );

    // Number of overlap Faces per source Face
    DataArray1D< int > nAllOverlapFaces( m_meshInputCov->faces.size() );

    int ixOverlap = 0;
    for( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
        // Determine how many overlap Faces and triangles are present
        int nOverlapFaces    = 0;
        size_t ixOverlapTemp = ixOverlap;
        for( ; ixOverlapTemp < m_meshOverlap->faces.size(); ixOverlapTemp++ )
        {
            // const Face & faceOverlap = m_meshOverlap->faces[ixOverlapTemp];
            if( ixFirst - m_meshOverlap->vecSourceFaceIx[ixOverlapTemp] != 0 )
            {
                break;
            }

            nOverlapFaces++;
        }

        nAllOverlapFaces[ixFirst] = nOverlapFaces;

        // Increment the current overlap index
        ixOverlap += nAllOverlapFaces[ixFirst];
    }

    // Geometric area of each output node
    DataArray2D< double > dGeometricOutputArea( m_meshOutput->faces.size(), nPout * nPout );

    // Area of each overlap element in the output basis
    DataArray2D< double > dOverlapOutputArea( m_meshOverlap->faces.size(), nPout * nPout );

    // Loop through all faces on m_meshInput
    ixOverlap                      = 0;
    const unsigned outputFrequency = ( m_meshInputCov->faces.size() / 10 ) + 1;

    if( is_root ) dbgprint.printf( 0, "Building conservative distribution maps\n" );

    // generic triangle used for area computation, for triangles around the center of overlap face;
    // used for overlap faces with more than 4 edges;
    // nodes array will be set for each triangle;
    // these triangles are not part of the mesh structure, they are just temporary during
    //   aforementioned decomposition.
    Face faceTri( 3 );
    NodeVector nodes( 3 );
    faceTri.SetNode( 0, 0 );
    faceTri.SetNode( 1, 1 );
    faceTri.SetNode( 2, 2 );

    for( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
        // Announce computation progress
        if( ixFirst % outputFrequency == 0 && is_root )
        {
            dbgprint.printf( 0, "Element %zu/%lu\n", ixFirst, m_meshInputCov->faces.size() );
        }

        // Quantities from the First Mesh
        const Face& faceFirst = m_meshInputCov->faces[ixFirst];

        const NodeVector& nodesFirst = m_meshInputCov->nodes;

        // Number of overlapping Faces and triangles
        int nOverlapFaces = nAllOverlapFaces[ixFirst];

        if( !nOverlapFaces ) continue;

        // // Calculate total element Jacobian
        // double dTotalJacobian = 0.0;
        // for (int s = 0; s < nPin; s++) {
        //     for (int t = 0; t < nPin; t++) {
        //         dTotalJacobian += dataGLLJacobianIn[s][t][ixFirst];
        //     }
        // }

        // Loop through all Overlap Faces
        for( int i = 0; i < nOverlapFaces; i++ )
        {
            // Quantities from the overlap Mesh
            const Face& faceOverlap = m_meshOverlap->faces[ixOverlap + i];

            const NodeVector& nodesOverlap = m_meshOverlap->nodes;

            // Quantities from the Second Mesh
            int ixSecond = m_meshOverlap->vecTargetFaceIx[ixOverlap + i];

            // signal to not participate, because it is a ghost target
            if( ixSecond < 0 ) continue;  // do not do anything

            const NodeVector& nodesSecond = m_meshOutput->nodes;

            const Face& faceSecond = m_meshOutput->faces[ixSecond];

            int nbEdges = faceOverlap.edges.size();
            int nOverlapTriangles = 1;
            Node center; // not used if nbEdges == 3
            if (nbEdges > 3) { // decompose from center in this case
                nOverlapTriangles = nbEdges;
                for (int k = 0; k < nbEdges; k++) {
                    const Node &node = nodesOverlap[faceOverlap[k]];
                    center = center + node;
                }
                center = center / nbEdges;
                center = center.Normalized();// project back on sphere of radius 1
            }

            Node node0, node1, node2;
            double dTriArea;

            // Loop over all sub-triangles of this Overlap Face
            for( int j = 0; j < nOverlapTriangles; j++ )
            {
                if (nbEdges == 3) // will come here only once, nOverlapTriangles == 1 in this case
                {
                    node0 = nodesOverlap[faceOverlap[0]];
                    node1 = nodesOverlap[faceOverlap[1]];
                    node2 = nodesOverlap[faceOverlap[2]];
                    dTriArea = CalculateFaceArea(faceOverlap, nodesOverlap);
                }
                else // decompose polygon in triangles around the center
                {
                    node0 = center;
                    node1 = nodesOverlap[faceOverlap[j]];
                    int j1 = (j + 1) % nbEdges;
                    node2 = nodesOverlap[faceOverlap[j1]];
                    nodes[0] = center;
                    nodes[1] = node1;
                    nodes[2] = node2;
                    dTriArea = CalculateFaceArea(faceTri, nodes);
                }

                for( int k = 0; k < triquadrule.GetPoints(); k++ )
                {
                    // Get the nodal location of this point
                    double dX[3];

                    dX[0] = dG( k, 0 ) * node0.x + dG( k, 1 ) * node1.x + dG( k, 2 ) * node2.x;
                    dX[1] = dG( k, 0 ) * node0.y + dG( k, 1 ) * node1.y + dG( k, 2 ) * node2.y;
                    dX[2] = dG( k, 0 ) * node0.z + dG( k, 1 ) * node1.z + dG( k, 2 ) * node2.z;

                    double dMag = sqrt( dX[0] * dX[0] + dX[1] * dX[1] + dX[2] * dX[2] );

                    dX[0] /= dMag;
                    dX[1] /= dMag;
                    dX[2] /= dMag;

                    Node nodeQuadrature( dX[0], dX[1], dX[2] );

                    // Find the components of this quadrature point in the basis
                    // of the first Face.
                    double dAlphaIn;
                    double dBetaIn;

                    ApplyInverseMap( faceFirst, nodesFirst, nodeQuadrature, dAlphaIn, dBetaIn );

                    // Find the components of this quadrature point in the basis
                    // of the second Face.
                    double dAlphaOut;
                    double dBetaOut;

                    ApplyInverseMap( faceSecond, nodesSecond, nodeQuadrature, dAlphaOut, dBetaOut );

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
                    SampleGLLFiniteElement( nMonotoneType, nPin, dAlphaIn, dBetaIn, dSampleCoeffIn );

                    // Sample the Second finite element at this point
                    SampleGLLFiniteElement( nMonotoneType, nPout, dAlphaOut, dBetaOut, dSampleCoeffOut );

                    // Overlap output area
                    for( int s = 0; s < nPout; s++ )
                    {
                        for( int t = 0; t < nPout; t++ )
                        {
                            double dNodeArea = dSampleCoeffOut[s][t] * dW[k] * dTriArea;

                            dOverlapOutputArea[ixOverlap + i][s * nPout + t] += dNodeArea;

                            dGeometricOutputArea[ixSecond][s * nPout + t] += dNodeArea;
                        }
                    }

                    // Compute overlap integral
                    int ixp = 0;
                    for( int p = 0; p < nPin; p++ )
                    {
                        for( int q = 0; q < nPin; q++ )
                        {
                            int ixs = 0;
                            for( int s = 0; s < nPout; s++ )
                            {
                                for( int t = 0; t < nPout; t++ )
                                {
                                    // Sample the Second finite element at this point
                                    dGlobalIntArray[ixp][ixOverlap + i][ixs] +=
                                        dSampleCoeffOut[s][t] * dSampleCoeffIn[p][q] * dW[k] * dTriArea;

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
        DataArray2D< double > dCoeff( nOverlapFaces * nPout * nPout, nPin * nPin );

        for( int i = 0; i < nOverlapFaces; i++ )
        {
            // int ixSecondFace = m_meshOverlap->vecTargetFaceIx[ixOverlap + i];

            int ixp = 0;
            for( int p = 0; p < nPin; p++ )
            {
                for( int q = 0; q < nPin; q++ )
                {
                    int ixs = 0;
                    for( int s = 0; s < nPout; s++ )
                    {
                        for( int t = 0; t < nPout; t++ )
                        {
                            dCoeff[i * nPout * nPout + ixs][ixp] = dGlobalIntArray[ixp][ixOverlap + i][ixs] /
                                                                   dOverlapOutputArea[ixOverlap + i][s * nPout + t];

                            ixs++;
                        }
                    }

                    ixp++;
                }
            }
        }

        // Source areas
        DataArray1D< double > vecSourceArea( nPin * nPin );

        for( int p = 0; p < nPin; p++ )
        {
            for( int q = 0; q < nPin; q++ )
            {
                vecSourceArea[p * nPin + q] = dataGLLJacobianIn[p][q][ixFirst];
            }
        }

        // Target areas
        DataArray1D< double > vecTargetArea( nOverlapFaces * nPout * nPout );

        for( int i = 0; i < nOverlapFaces; i++ )
        {
            // int ixSecond = m_meshOverlap->vecTargetFaceIx[ixOverlap + i];
            int ixs = 0;
            for( int s = 0; s < nPout; s++ )
            {
                for( int t = 0; t < nPout; t++ )
                {
                    vecTargetArea[i * nPout * nPout + ixs] = dOverlapOutputArea[ixOverlap + i][nPout * s + t];

                    ixs++;
                }
            }
        }

        // Force consistency and conservation
        if( !fNoConservation )
        {
            ForceIntArrayConsistencyConservation( vecSourceArea, vecTargetArea, dCoeff, ( nMonotoneType != 0 ) );
        }

        // Update global coefficients
        for( int i = 0; i < nOverlapFaces; i++ )
        {
            int ixp = 0;
            for( int p = 0; p < nPin; p++ )
            {
                for( int q = 0; q < nPin; q++ )
                {
                    int ixs = 0;
                    for( int s = 0; s < nPout; s++ )
                    {
                        for( int t = 0; t < nPout; t++ )
                        {
                            dGlobalIntArray[ixp][ixOverlap + i][ixs] =
                                dCoeff[i * nPout * nPout + ixs][ixp] * dOverlapOutputArea[ixOverlap + i][s * nPout + t];

                            ixs++;
                        }
                    }

                    ixp++;
                }
            }
        }

#ifdef VVERBOSE
        // Check column sums (conservation)
        for( int i = 0; i < nPin * nPin; i++ )
        {
            double dColSum = 0.0;
            for( int j = 0; j < nOverlapFaces * nPout * nPout; j++ )
            {
                dColSum += dCoeff[j][i] * vecTargetArea[j];
            }
            printf( "Col %i: %1.15e\n", i, dColSum / vecSourceArea[i] );
        }

        // Check row sums (consistency)
        for( int j = 0; j < nOverlapFaces * nPout * nPout; j++ )
        {
            double dRowSum = 0.0;
            for( int i = 0; i < nPin * nPin; i++ )
            {
                dRowSum += dCoeff[j][i];
            }
            printf( "Row %i: %1.15e\n", j, dRowSum );
        }
#endif

        // Increment the current overlap index
        ixOverlap += nOverlapFaces;
    }

    // Build redistribution map within target element
    if( is_root ) dbgprint.printf( 0, "Building redistribution maps on target mesh\n" );
    DataArray1D< double > dRedistSourceArea( nPout * nPout );
    DataArray1D< double > dRedistTargetArea( nPout * nPout );
    std::vector< DataArray2D< double > > dRedistributionMaps;
    dRedistributionMaps.resize( m_meshOutput->faces.size() );

    for( size_t ixSecond = 0; ixSecond < m_meshOutput->faces.size(); ixSecond++ )
    {
        dRedistributionMaps[ixSecond].Allocate( nPout * nPout, nPout * nPout );

        for( int i = 0; i < nPout * nPout; i++ )
        {
            dRedistributionMaps[ixSecond][i][i] = 1.0;
        }

        for( int s = 0; s < nPout * nPout; s++ )
        {
            dRedistSourceArea[s] = dGeometricOutputArea[ixSecond][s];
        }

        for( int s = 0; s < nPout * nPout; s++ )
        {
            dRedistTargetArea[s] = dataGLLJacobianOut[s / nPout][s % nPout][ixSecond];
        }

        if( !fNoConservation )
        {
            ForceIntArrayConsistencyConservation( dRedistSourceArea, dRedistTargetArea, dRedistributionMaps[ixSecond],
                                                  ( nMonotoneType != 0 ) );

            for( int s = 0; s < nPout * nPout; s++ )
            {
                for( int t = 0; t < nPout * nPout; t++ )
                {
                    dRedistributionMaps[ixSecond][s][t] *= dRedistTargetArea[s] / dRedistSourceArea[t];
                }
            }
        }
    }

    // Construct the total geometric area
    DataArray1D< double > dTotalGeometricArea( dataNodalAreaOut.GetRows() );
    for( size_t ixSecond = 0; ixSecond < m_meshOutput->faces.size(); ixSecond++ )
    {
        for( int s = 0; s < nPout; s++ )
        {
            for( int t = 0; t < nPout; t++ )
            {
                dTotalGeometricArea[dataGLLNodesOut[s][t][ixSecond] - 1] +=
                    dGeometricOutputArea[ixSecond][s * nPout + t];
            }
        }
    }

    // Compose the integration operator with the output map
    ixOverlap = 0;

    if( is_root ) dbgprint.printf( 0, "Assembling map\n" );

    // Map from source DOFs to target DOFs with redistribution applied
    DataArray2D< double > dRedistributedOp( nPin * nPin, nPout * nPout );

    for( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
        // Announce computation progress
        if( ixFirst % outputFrequency == 0 && is_root )
        {
            dbgprint.printf( 0, "Element %zu/%lu\n", ixFirst, m_meshInputCov->faces.size() );
        }

        // Number of overlapping Faces and triangles
        int nOverlapFaces = nAllOverlapFaces[ixFirst];

        if( !nOverlapFaces ) continue;

        // Put composed array into map
        for( int j = 0; j < nOverlapFaces; j++ )
        {
            int ixSecondFace = m_meshOverlap->vecTargetFaceIx[ixOverlap + j];

            // signal to not participate, because it is a ghost target
            if( ixSecondFace < 0 ) continue;  // do not do anything

            dRedistributedOp.Zero();
            for( int p = 0; p < nPin * nPin; p++ )
            {
                for( int s = 0; s < nPout * nPout; s++ )
                {
                    for( int t = 0; t < nPout * nPout; t++ )
                    {
                        dRedistributedOp[p][s] +=
                            dRedistributionMaps[ixSecondFace][s][t] * dGlobalIntArray[p][ixOverlap + j][t];
                    }
                }
            }

            int ixp = 0;
            for( int p = 0; p < nPin; p++ )
            {
                for( int q = 0; q < nPin; q++ )
                {
                    int ixFirstNode;
                    if( fContinuousIn )
                    {
                        ixFirstNode = dataGLLNodesIn[p][q][ixFirst] - 1;
                    }
                    else
                    {
                        ixFirstNode = ixFirst * nPin * nPin + p * nPin + q;
                    }

                    int ixs = 0;
                    for( int s = 0; s < nPout; s++ )
                    {
                        for( int t = 0; t < nPout; t++ )
                        {
                            int ixSecondNode;
                            if( fContinuousOut )
                            {
                                ixSecondNode = dataGLLNodesOut[s][t][ixSecondFace] - 1;

                                if( !fNoConservation )
                                {
                                    smatMap( ixSecondNode, ixFirstNode ) +=
                                        dRedistributedOp[ixp][ixs] / dataNodalAreaOut[ixSecondNode];
                                }
                                else
                                {
                                    smatMap( ixSecondNode, ixFirstNode ) +=
                                        dRedistributedOp[ixp][ixs] / dTotalGeometricArea[ixSecondNode];
                                }
                            }
                            else
                            {
                                ixSecondNode = ixSecondFace * nPout * nPout + s * nPout + t;

                                if( !fNoConservation )
                                {
                                    smatMap( ixSecondNode, ixFirstNode ) +=
                                        dRedistributedOp[ixp][ixs] / dataGLLJacobianOut[s][t][ixSecondFace];
                                }
                                else
                                {
                                    smatMap( ixSecondNode, ixFirstNode ) +=
                                        dRedistributedOp[ixp][ixs] / dGeometricOutputArea[ixSecondFace][s * nPout + t];
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

void moab::TempestOnlineMap::LinearRemapGLLtoGLL2_Pointwise_MOAB( const DataArray3D< int >& dataGLLNodesIn,
                                                                  const DataArray3D< double >& /*dataGLLJacobianIn*/,
                                                                  const DataArray3D< int >& dataGLLNodesOut,
                                                                  const DataArray3D< double >& /*dataGLLJacobianOut*/,
                                                                  const DataArray1D< double >& dataNodalAreaOut,
                                                                  int nPin, int nPout, int nMonotoneType,
                                                                  bool fContinuousIn, bool fContinuousOut )
{
    // Gauss-Lobatto quadrature within Faces
    DataArray1D< double > dGL;
    DataArray1D< double > dWL;

    GaussLobattoQuadrature::GetPoints( nPout, 0.0, 1.0, dGL, dWL );

    // Get SparseMatrix represntation of the OfflineMap
    SparseMatrix< double >& smatMap = this->GetSparseMatrix();

    // Sample coefficients
    DataArray2D< double > dSampleCoeffIn( nPin, nPin );

    // Announcemnets
    moab::DebugOutput dbgprint( std::cout, this->rank, 0 );
    dbgprint.set_prefix( "[LinearRemapGLLtoGLL2_Pointwise_MOAB]: " );
    if( is_root )
    {
        dbgprint.printf( 0, "Finite Element to Finite Element (Pointwise) Projection\n" );
        dbgprint.printf( 0, "Order of the input FE polynomial interpolant: %i\n", nPin );
        dbgprint.printf( 0, "Order of the output FE polynomial interpolant: %i\n", nPout );
    }

    // Number of overlap Faces per source Face
    DataArray1D< int > nAllOverlapFaces( m_meshInputCov->faces.size() );

    int ixOverlap = 0;

    for( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
        size_t ixOverlapTemp = ixOverlap;
        for( ; ixOverlapTemp < m_meshOverlap->faces.size(); ixOverlapTemp++ )
        {
            // const Face & faceOverlap = m_meshOverlap->faces[ixOverlapTemp];

            if( ixFirst - m_meshOverlap->vecSourceFaceIx[ixOverlapTemp] != 0 ) break;

            nAllOverlapFaces[ixFirst]++;
        }

        // Increment the current overlap index
        ixOverlap += nAllOverlapFaces[ixFirst];
    }

    // Number of times this point was found
    DataArray1D< bool > fSecondNodeFound( dataNodalAreaOut.GetRows() );

    ixOverlap                      = 0;
    const unsigned outputFrequency = ( m_meshInputCov->faces.size() / 10 ) + 1;

    // Loop through all faces on m_meshInputCov
    for( size_t ixFirst = 0; ixFirst < m_meshInputCov->faces.size(); ixFirst++ )
    {
        // Announce computation progress
        if( ixFirst % outputFrequency == 0 && is_root )
        {
            dbgprint.printf( 0, "Element %zu/%lu\n", ixFirst, m_meshInputCov->faces.size() );
        }

        // Quantities from the First Mesh
        const Face& faceFirst = m_meshInputCov->faces[ixFirst];

        const NodeVector& nodesFirst = m_meshInputCov->nodes;

        // Number of overlapping Faces and triangles
        int nOverlapFaces = nAllOverlapFaces[ixFirst];

        // Loop through all Overlap Faces
        for( int i = 0; i < nOverlapFaces; i++ )
        {

            // Quantities from the Second Mesh
            int ixSecond = m_meshOverlap->vecTargetFaceIx[ixOverlap + i];

            // signal to not participate, because it is a ghost target
            if( ixSecond < 0 ) continue;  // do not do anything

            const NodeVector& nodesSecond = m_meshOutput->nodes;

            const Face& faceSecond = m_meshOutput->faces[ixSecond];

            // Loop through all nodes on the second face
            for( int s = 0; s < nPout; s++ )
            {
                for( int t = 0; t < nPout; t++ )
                {
                    size_t ixSecondNode;
                    if( fContinuousOut )
                    {
                        ixSecondNode = dataGLLNodesOut[s][t][ixSecond] - 1;
                    }
                    else
                    {
                        ixSecondNode = ixSecond * nPout * nPout + s * nPout + t;
                    }

                    if( ixSecondNode >= fSecondNodeFound.GetRows() )
                    {
                        _EXCEPTIONT( "Logic error" );
                    }

                    // Check if this node has been found already
                    if( fSecondNodeFound[ixSecondNode] )
                    {
                        continue;
                    }

                    // Check this node
                    Node node;
                    Node dDx1G;
                    Node dDx2G;

                    ApplyLocalMap( faceSecond, nodesSecond, dGL[t], dGL[s], node, dDx1G, dDx2G );

                    // Find the components of this quadrature point in the basis
                    // of the first Face.
                    double dAlphaIn;
                    double dBetaIn;

                    ApplyInverseMap( faceFirst, nodesFirst, node, dAlphaIn, dBetaIn );

                    // Check if this node is within the first Face
                    if( ( dAlphaIn < -1.0e-10 ) || ( dAlphaIn > 1.0 + 1.0e-10 ) || ( dBetaIn < -1.0e-10 ) ||
                        ( dBetaIn > 1.0 + 1.0e-10 ) )
                    {
                        continue;
                    }

                    // Node is within the overlap region, mark as found
                    fSecondNodeFound[ixSecondNode] = true;

                    // Sample the First finite element at this point
                    SampleGLLFiniteElement( nMonotoneType, nPin, dAlphaIn, dBetaIn, dSampleCoeffIn );

                    // Add to map
                    for( int p = 0; p < nPin; p++ )
                    {
                        for( int q = 0; q < nPin; q++ )
                        {
                            int ixFirstNode;
                            if( fContinuousIn )
                            {
                                ixFirstNode = dataGLLNodesIn[p][q][ixFirst] - 1;
                            }
                            else
                            {
                                ixFirstNode = ixFirst * nPin * nPin + p * nPin + q;
                            }

                            smatMap( ixSecondNode, ixFirstNode ) += dSampleCoeffIn[p][q];
                        }
                    }
                }
            }
        }

        // Increment the current overlap index
        ixOverlap += nOverlapFaces;
    }

    // Check for missing samples
    for( size_t i = 0; i < fSecondNodeFound.GetRows(); i++ )
    {
        if( !fSecondNodeFound[i] )
        {
            _EXCEPTION1( "Can't sample point %i", i );
        }
    }

    return;
}

///////////////////////////////////////////////////////////////////////////////
