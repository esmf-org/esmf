/*
 * =====================================================================================
 *
 *       Filename:  TempestOfflineMap.hpp
 *
 *    Description:  Interface to the TempestRemap library to compute the consistent,
 *                  and accurate high-order conservative remapping weights for overlap
 *                  grids on the sphere in climate simulations.
 *
 *         Author:  Vijay S. Mahadevan (vijaysm), mahadevan@anl.gov
 *
 * =====================================================================================
 */

#include "Announce.h"
#include "DataMatrix3D.h"
#include "FiniteElementTools.h"
#include "SparseMatrix.h"
#include "STLStringHelper.h"

#include "moab/Remapping/TempestOfflineMap.hpp"
#include "DebugOutput.hpp"

#include <fstream>
#include <cmath>
#include <cstdlib>

///////////////////////////////////////////////////////////////////////////////

// #define VERBOSE
// #define VVERBOSE

///////////////////////////////////////////////////////////////////////////////

moab::TempestOfflineMap::TempestOfflineMap ( moab::TempestRemapper* remapper ) : OfflineMap(), m_remapper ( remapper )
{
    // Get the references for the MOAB core objects
    mbCore = m_remapper->get_interface();
#ifdef MOAB_HAVE_MPI
    pcomm = m_remapper->get_parallel_communicator();
#endif

    // Update the references to the meshes
    m_meshInput = remapper->GetMesh ( moab::Remapper::SourceMesh );
    m_meshInputCov = remapper->GetCoveringMesh();
    m_meshOutput = remapper->GetMesh ( moab::Remapper::TargetMesh );
    m_meshOverlap = remapper->GetMesh ( moab::Remapper::IntersectedMesh );
    m_globalMapAvailable = false;

    is_parallel = false;
    is_root = true;
    rank = 0;
    size = 1;
#ifdef MOAB_HAVE_MPI
    int flagInit;
    MPI_Initialized( &flagInit );
    if (flagInit) {
        is_parallel = true;
        assert(pcomm != NULL);
        rank = pcomm->rank();
        size = pcomm->size();
        is_root = (rank == 0);
    }
#endif

    moab::DebugOutput dbgprint ( std::cout, rank );

    // Compute and store the total number of source and target DoFs corresponding
    // to number of rows and columns in the mapping.

    // Initialize dimension information from file
    std::vector<std::string> dimNames(1);
    std::vector<int> dimSizes(1);
    dimNames[0] = "num_elem";

    dbgprint.printf ( 0, "Initializing dimensions of map\n" );
    dbgprint.printf ( 0, "Input mesh\n" );
    dimSizes[0] = m_meshInputCov->faces.size();
    this->InitializeSourceDimensions(dimNames, dimSizes);
    dbgprint.printf ( 0, "Output mesh\n" );
    dimSizes[0] = m_meshOutput->faces.size();
    this->InitializeTargetDimensions(dimNames, dimSizes);

    m_weightMapGlobal = NULL;

    // Build a matrix of source and target discretization so that we know how to assign
    // the global DoFs in parallel for the mapping weights
    // For example, FV->FV: rows X cols = faces_source X faces_target
}

///////////////////////////////////////////////////////////////////////////////

moab::TempestOfflineMap::~TempestOfflineMap()
{
    delete m_weightMapGlobal;
    mbCore = NULL;
#ifdef MOAB_HAVE_MPI
    pcomm = NULL;
#endif
    m_meshInput = NULL;
    m_meshOutput = NULL;
    m_meshOverlap = NULL;
}

///////////////////////////////////////////////////////////////////////////////

static void ParseVariableList (
    const std::string & strVariables,
    std::vector< std::string > & vecVariableStrings
)
{
    unsigned iVarBegin = 0;
    unsigned iVarCurrent = 0;

    // Parse variable name
    for ( ;; )
    {
        if ( ( iVarCurrent >= strVariables.length() ) ||
                ( strVariables[iVarCurrent] == ',' ) ||
                ( strVariables[iVarCurrent] == ' ' )
           )
        {
            if ( iVarCurrent == iVarBegin )
            {
                if ( iVarCurrent >= strVariables.length() )
                {
                    break;
                }
                continue;
            }

            vecVariableStrings.push_back (
                strVariables.substr ( iVarBegin, iVarCurrent - iVarBegin ) );

            iVarBegin = iVarCurrent + 1;
        }

        iVarCurrent++;
    }
}

///////////////////////////////////////////////////////////////////////////////

moab::ErrorCode moab::TempestOfflineMap::SetDofMapTags(const std::string srcDofTagName, const std::string tgtDofTagName)
{
    moab::ErrorCode rval;

    rval = mbCore->tag_get_handle ( srcDofTagName.c_str(), m_nDofsPEl_Src*m_nDofsPEl_Src, MB_TYPE_INTEGER,
                             this->m_dofTagSrc, MB_TAG_ANY );MB_CHK_ERR(rval);
    rval = mbCore->tag_get_handle ( tgtDofTagName.c_str(), m_nDofsPEl_Dest*m_nDofsPEl_Dest, MB_TYPE_INTEGER,
                             this->m_dofTagDest, MB_TAG_ANY );MB_CHK_ERR(rval);

    return moab::MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////

moab::ErrorCode moab::TempestOfflineMap::SetDofMapAssociation(DiscretizationType srcType, bool isSrcContinuous, DataMatrix3D<int>* srcdataGLLNodes, DataMatrix3D<int>* srcdataGLLNodesSrc,
    DiscretizationType destType, bool isTgtContinuous, DataMatrix3D<int>* tgtdataGLLNodes)
{
    moab::ErrorCode rval;
    std::vector<bool> dgll_cgll_row_ldofmap, dgll_cgll_col_ldofmap, dgll_cgll_covcol_ldofmap;
    std::vector<int> src_soln_gdofs, locsrc_soln_gdofs, tgt_soln_gdofs;

    // We are assuming that these are element based tags that are sized: np * np
    m_srcDiscType = srcType;
    m_destDiscType = destType;

    bool vprint = is_root && false;

#ifdef VVERBOSE
    {
        src_soln_gdofs.resize(m_remapper->m_covering_source_entities.size()*m_nDofsPEl_Src*m_nDofsPEl_Src, -1);
        rval = mbCore->tag_get_data ( m_dofTagSrc, m_remapper->m_covering_source_entities, &src_soln_gdofs[0] );MB_CHK_ERR(rval);
        locsrc_soln_gdofs.resize(m_remapper->m_source_entities.size()*m_nDofsPEl_Src*m_nDofsPEl_Src);
        rval = mbCore->tag_get_data ( m_dofTagSrc, m_remapper->m_source_entities, &locsrc_soln_gdofs[0] );MB_CHK_ERR(rval);
        tgt_soln_gdofs.resize(m_remapper->m_target_entities.size()*m_nDofsPEl_Dest*m_nDofsPEl_Dest);
        rval = mbCore->tag_get_data ( m_dofTagDest, m_remapper->m_target_entities, &tgt_soln_gdofs[0] );MB_CHK_ERR(rval);

        if (is_root)
        {
            {
                std::ofstream output_file ( "sourcecov-gids-0.txt" );
                output_file << "I, GDOF\n";
                for (unsigned i=0; i < src_soln_gdofs.size(); ++i)
                    output_file << i << ", " << src_soln_gdofs[i] << "\n";

                output_file << "ELEMID, IDOF, LDOF, GDOF, NDOF\n";
                m_nTotDofs_SrcCov=0;
                if (isSrcContinuous) dgll_cgll_covcol_ldofmap.resize (m_remapper->m_covering_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, false);
                for ( unsigned j = 0; j < m_remapper->m_covering_source_entities.size(); j++ )
                {
                    for ( int p = 0; p < m_nDofsPEl_Src; p++ )
                    {
                        for ( int q = 0; q < m_nDofsPEl_Src; q++)
                        {
                            const int ldof = (*srcdataGLLNodes)[p][q][j] - 1;
                            const int idof = j * m_nDofsPEl_Src * m_nDofsPEl_Src + p * m_nDofsPEl_Src + q;
                            if ( isSrcContinuous && !dgll_cgll_covcol_ldofmap[ldof] ) {
                                m_nTotDofs_SrcCov++;
                                dgll_cgll_covcol_ldofmap[ldof] = true;
                            }
                            output_file << m_remapper->lid_to_gid_covsrc[j] << ", " <<  idof << ", " << ldof << ", " << src_soln_gdofs[idof] << ", " << m_nTotDofs_SrcCov << "\n";
                        }
                    }
                }
                output_file.flush(); // required here
                output_file.close();
                dgll_cgll_covcol_ldofmap.clear();
            }

            {
                std::ofstream output_file ( "source-gids-0.txt" );
                output_file << "I, GDOF\n";
                for (unsigned i=0; i < locsrc_soln_gdofs.size(); ++i)
                    output_file << i << ", " << locsrc_soln_gdofs[i] << "\n";

                output_file << "ELEMID, IDOF, LDOF, GDOF, NDOF\n";
                m_nTotDofs_Src=0;
                if (isSrcContinuous) dgll_cgll_col_ldofmap.resize (m_remapper->m_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, false);
                for ( unsigned j = 0; j < m_remapper->m_source_entities.size(); j++ )
                {
                    for ( int p = 0; p < m_nDofsPEl_Src; p++ )
                    {
                        for ( int q = 0; q < m_nDofsPEl_Src; q++)
                        {
                            const int ldof = (*srcdataGLLNodesSrc)[p][q][j] - 1;
                            const int idof = j * m_nDofsPEl_Src * m_nDofsPEl_Src + p * m_nDofsPEl_Src + q;
                            if ( isSrcContinuous && !dgll_cgll_col_ldofmap[ldof] ) {
                                m_nTotDofs_Src++;
                                dgll_cgll_col_ldofmap[ldof] = true;
                            }
                            output_file << m_remapper->lid_to_gid_src[j] << ", " <<  idof << ", " << ldof << ", " << locsrc_soln_gdofs[idof] << ", " << m_nTotDofs_Src << "\n";
                        }
                    }
                }
                output_file.flush(); // required here
                output_file.close();
                dgll_cgll_col_ldofmap.clear();
            }

            {
                std::ofstream output_file ( "target-gids-0.txt" );
                output_file << "I, GDOF\n";
                for (unsigned i=0; i < tgt_soln_gdofs.size(); ++i)
                    output_file << i << ", " << tgt_soln_gdofs[i] << "\n";
        
                output_file << "ELEMID, IDOF, GDOF, NDOF\n";
                m_nTotDofs_Dest=0;
                
                for (unsigned i=0; i < tgt_soln_gdofs.size(); ++i) {
                    output_file << m_remapper->lid_to_gid_tgt[i] << ", " <<  i << ", " << tgt_soln_gdofs[i] << ", " << m_nTotDofs_Dest << "\n";
                    m_nTotDofs_Dest++;
                }
        
                output_file.flush(); // required here
                output_file.close();
            }
        }
        else 
        {
            {
                std::ofstream output_file ( "sourcecov-gids-1.txt" );
                output_file << "I, GDOF\n";
                for (unsigned i=0; i < src_soln_gdofs.size(); ++i)
                    output_file << i << ", " << src_soln_gdofs[i] << "\n";

                output_file << "ELEMID, IDOF, LDOF, GDOF, NDOF\n";
                m_nTotDofs_SrcCov=0;
                if (isSrcContinuous) dgll_cgll_covcol_ldofmap.resize (m_remapper->m_covering_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, false);
                for ( unsigned j = 0; j < m_remapper->m_covering_source_entities.size(); j++ )
                {
                    for ( int p = 0; p < m_nDofsPEl_Src; p++ )
                    {
                        for ( int q = 0; q < m_nDofsPEl_Src; q++)
                        {
                            const int ldof = (*srcdataGLLNodes)[p][q][j] - 1;
                            const int idof = j * m_nDofsPEl_Src * m_nDofsPEl_Src + p * m_nDofsPEl_Src + q;
                            if ( isSrcContinuous && !dgll_cgll_covcol_ldofmap[ldof] ) {
                                m_nTotDofs_SrcCov++;
                                dgll_cgll_covcol_ldofmap[ldof] = true;
                            }
                            output_file << m_remapper->lid_to_gid_covsrc[j] << ", " <<  idof << ", " << ldof << ", " << src_soln_gdofs[idof] << ", " << m_nTotDofs_SrcCov << "\n";
                        }
                    }
                }
                output_file.flush(); // required here
                output_file.close();
                dgll_cgll_covcol_ldofmap.clear();
            }

            {
                std::ofstream output_file ( "source-gids-1.txt" );
                output_file << "I, GDOF\n";
                for (unsigned i=0; i < locsrc_soln_gdofs.size(); ++i)
                    output_file << i << ", " << locsrc_soln_gdofs[i] << "\n";

                output_file << "ELEMID, IDOF, LDOF, GDOF, NDOF\n";
                m_nTotDofs_Src=0;
                if (isSrcContinuous) dgll_cgll_col_ldofmap.resize (m_remapper->m_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, false);
                for ( unsigned j = 0; j < m_remapper->m_source_entities.size(); j++ )
                {
                    for ( int p = 0; p < m_nDofsPEl_Src; p++ )
                    {
                        for ( int q = 0; q < m_nDofsPEl_Src; q++)
                        {
                            const int ldof = (*srcdataGLLNodesSrc)[p][q][j] - 1;
                            const int idof = j * m_nDofsPEl_Src * m_nDofsPEl_Src + p * m_nDofsPEl_Src + q;
                            if ( isSrcContinuous && !dgll_cgll_col_ldofmap[ldof] ) {
                                m_nTotDofs_Src++;
                                dgll_cgll_col_ldofmap[ldof] = true;
                            }
                            output_file << m_remapper->lid_to_gid_src[j] << ", " <<  idof << ", " << ldof << ", " << locsrc_soln_gdofs[idof] << ", " << m_nTotDofs_Src << "\n";
                        }
                    }
                }
                output_file.flush(); // required here
                output_file.close();
                dgll_cgll_col_ldofmap.clear();
            }

            {
                std::ofstream output_file ( "target-gids-1.txt" );
                output_file << "I, GDOF\n";
                for (unsigned i=0; i < tgt_soln_gdofs.size(); ++i)
                    output_file << i << ", " << tgt_soln_gdofs[i] << "\n";
        
                output_file << "ELEMID, IDOF, GDOF, NDOF\n";
                m_nTotDofs_Dest=0;
                
                for (unsigned i=0; i < tgt_soln_gdofs.size(); ++i) {
                    output_file << m_remapper->lid_to_gid_tgt[i] << ", " <<  i << ", " << tgt_soln_gdofs[i] << ", " << m_nTotDofs_Dest << "\n";
                    m_nTotDofs_Dest++;
                }
        
                output_file.flush(); // required here
                output_file.close();
            }
        }
    }
#endif

    // Now compute the mapping and store it for the covering mesh
    col_dofmap.resize (m_remapper->m_covering_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, ULONG_MAX);
    col_ldofmap.resize (m_remapper->m_covering_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, ULONG_MAX);
    col_gdofmap.resize (m_remapper->m_covering_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, ULONG_MAX);
    src_soln_gdofs.resize(m_remapper->m_covering_source_entities.size()*m_nDofsPEl_Src*m_nDofsPEl_Src, INT_MAX);
    rval = mbCore->tag_get_data ( m_dofTagSrc, m_remapper->m_covering_source_entities, &src_soln_gdofs[0] );MB_CHK_ERR(rval);
    m_nTotDofs_SrcCov = 0;
    if (srcdataGLLNodes == NULL || srcType == DiscretizationType_FV) { /* we only have a mapping for elements as DoFs */
        for (unsigned i=0; i < col_dofmap.size(); ++i) {
            col_dofmap[i] = i;
            col_ldofmap[i] = i;
            col_gdofmap[i] = src_soln_gdofs[i];
            if (vprint) std::cout << "Col: " << i << ", " << src_soln_gdofs[i] << "\n";
            m_nTotDofs_SrcCov++;
        }
    }
    else {
        if (isSrcContinuous) dgll_cgll_covcol_ldofmap.resize (m_remapper->m_covering_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, false);
        // Put these remap coefficients into the SparseMatrix map
        for ( unsigned j = 0; j < m_remapper->m_covering_source_entities.size(); j++ )
        {
            for ( int p = 0; p < m_nDofsPEl_Src; p++ )
            {
                for ( int q = 0; q < m_nDofsPEl_Src; q++)
                {
                    const int ldof = (*srcdataGLLNodes)[p][q][j] - 1;
                    const int idof = j * m_nDofsPEl_Src * m_nDofsPEl_Src + p * m_nDofsPEl_Src + q;
                    if ( isSrcContinuous && !dgll_cgll_covcol_ldofmap[ldof] ) {
                        m_nTotDofs_SrcCov++;
                        dgll_cgll_covcol_ldofmap[ldof] = true;
                    }
                    if ( !isSrcContinuous ) m_nTotDofs_SrcCov++;
                    col_dofmap[ idof ] = ldof;
                    col_ldofmap[ ldof ] = idof;
                    assert(src_soln_gdofs[idof] > 0);
                    col_gdofmap[ idof ] = src_soln_gdofs[idof] - 1;
                    if (vprint) std::cout << "Col: " << m_remapper->lid_to_gid_covsrc[j] << ", " <<  idof << ", " << ldof << ", " << col_gdofmap[idof] << ", " << m_nTotDofs_SrcCov << "\n";
                }
            }
        }
    }

    srccol_dofmap.resize (m_remapper->m_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, ULONG_MAX);
    srccol_ldofmap.resize (m_remapper->m_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, ULONG_MAX);
    srccol_gdofmap.resize (m_remapper->m_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, ULONG_MAX);
    locsrc_soln_gdofs.resize(m_remapper->m_source_entities.size()*m_nDofsPEl_Src*m_nDofsPEl_Src, INT_MAX);
    rval = mbCore->tag_get_data ( m_dofTagSrc, m_remapper->m_source_entities, &locsrc_soln_gdofs[0] );MB_CHK_ERR(rval);
    
    // Now compute the mapping and store it for the original source mesh
    m_nTotDofs_Src = 0;
    if (srcdataGLLNodesSrc == NULL || srcType == DiscretizationType_FV) { /* we only have a mapping for elements as DoFs */
        for (unsigned i=0; i < srccol_dofmap.size(); ++i) {
            srccol_dofmap[i] = i;
            srccol_ldofmap[i] = i;
            srccol_gdofmap[i] = locsrc_soln_gdofs[i];
            m_nTotDofs_Src++;
        }
    }
    else {
        if (isSrcContinuous) dgll_cgll_col_ldofmap.resize(m_remapper->m_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, false);
        // Put these remap coefficients into the SparseMatrix map
        for ( unsigned j = 0; j < m_remapper->m_source_entities.size(); j++ )
        {
            for ( int p = 0; p < m_nDofsPEl_Src; p++ )
            {
                for ( int q = 0; q < m_nDofsPEl_Src; q++ )
                {
                    const int ldof = (*srcdataGLLNodesSrc)[p][q][j] - 1;
                    const int idof = j * m_nDofsPEl_Src * m_nDofsPEl_Src + p * m_nDofsPEl_Src + q;
                    if ( isSrcContinuous && !dgll_cgll_col_ldofmap[ldof] ) {
                        m_nTotDofs_Src++;
                        dgll_cgll_col_ldofmap[ldof] = true;
                    }
                    if ( !isSrcContinuous ) m_nTotDofs_Src++;
                    srccol_dofmap[ idof ] = ldof;
                    srccol_ldofmap[ ldof ] = idof;
                    srccol_gdofmap[ idof ] = locsrc_soln_gdofs[idof] - 1;
                }
            }
        }
    }

    row_dofmap.resize (m_remapper->m_target_entities.size() * m_nDofsPEl_Dest * m_nDofsPEl_Dest, ULONG_MAX);
    row_ldofmap.resize (m_remapper->m_target_entities.size() * m_nDofsPEl_Dest * m_nDofsPEl_Dest, ULONG_MAX);
    row_gdofmap.resize (m_remapper->m_target_entities.size() * m_nDofsPEl_Dest * m_nDofsPEl_Dest, ULONG_MAX);
    tgt_soln_gdofs.resize(m_remapper->m_target_entities.size()*m_nDofsPEl_Dest*m_nDofsPEl_Dest, INT_MAX);
    rval = mbCore->tag_get_data ( m_dofTagDest, m_remapper->m_target_entities, &tgt_soln_gdofs[0] );MB_CHK_ERR(rval);

    // Now compute the mapping and store it for the target mesh
    m_nTotDofs_Dest = 0;
    if (tgtdataGLLNodes == NULL || destType == DiscretizationType_FV) { /* we only have a mapping for elements as DoFs */
        for (unsigned i=0; i < row_dofmap.size(); ++i) {
            row_dofmap[i] = i;
            row_ldofmap[i] = i;
            row_gdofmap[i] = tgt_soln_gdofs[i];
            // if (vprint) std::cout << "Row: " << m_remapper->lid_to_gid_tgt[i] << ", " <<  i << ", " << row_dofmap[i] << ", " << row_ldofmap[i] << ", " << tgt_soln_gdofs[i] << ", " << row_gdofmap[i] << "\n";
            m_nTotDofs_Dest++;
        }
    }
    else {
        if (isTgtContinuous) dgll_cgll_row_ldofmap.resize (m_remapper->m_target_entities.size() * m_nDofsPEl_Dest * m_nDofsPEl_Dest, false);
        // Put these remap coefficients into the SparseMatrix map
        for ( unsigned j = 0; j < m_remapper->m_target_entities.size(); j++ )
        {
            for ( int p = 0; p < m_nDofsPEl_Dest; p++ )
            {
                for ( int q = 0; q < m_nDofsPEl_Dest; q++ )
                {
                    const int ldof = (*tgtdataGLLNodes)[p][q][j] - 1;
                    const int idof = j * m_nDofsPEl_Dest * m_nDofsPEl_Dest + p * m_nDofsPEl_Dest + q;
                    if ( isTgtContinuous && !dgll_cgll_row_ldofmap[ldof] ) {
                        m_nTotDofs_Dest++;
                        dgll_cgll_row_ldofmap[ldof] = true;
                    }
                    if ( !isTgtContinuous ) m_nTotDofs_Dest++;
                    row_dofmap[ idof ] = ldof;
                    row_ldofmap[ ldof ] = idof;
                    row_gdofmap[ idof ] = tgt_soln_gdofs[idof] - 1;
                    if (vprint) std::cout << "Row: " << idof << ", " << ldof << ", " << tgt_soln_gdofs[idof] - 1 << "\n";
                }
            }
        }
    }

    // Let us also allocate the local representation of the sparse matrix
#ifdef MOAB_HAVE_EIGEN
    // if (vprint)
    {
        std::cout << "[" << rank << "]" << "DoFs: row = " << m_nTotDofs_Dest << ", " << row_dofmap.size() << ", col = " << m_nTotDofs_Src << ", " << m_nTotDofs_SrcCov << ", " << col_dofmap.size() << "\n";
        // std::cout << "Max col_dofmap: " << maxcol << ", Min col_dofmap" << mincol << "\n";
    }
#endif

    return moab::MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////

moab::ErrorCode moab::TempestOfflineMap::GenerateOfflineMap ( std::string strInputType, std::string strOutputType,
        const int nPin, const int nPout,
        bool fBubble, int fMonotoneTypeID,
        bool fVolumetric, bool fNoConservation, bool fNoCheck,
        const std::string srcDofTagName, const std::string tgtDofTagName,
        const std::string strVariables,
        const std::string strInputData, const std::string strOutputData,
        const std::string strNColName, const bool fOutputDouble,
        const std::string strPreserveVariables, const bool fPreserveAll, const double dFillValueOverride,
        const bool fInputConcave, const bool fOutputConcave )
{
    NcError error ( NcError::silent_nonfatal );

    moab::DebugOutput dbgprint ( std::cout, ( rank ) );
    moab::ErrorCode rval;

    try
    {
        // Check command line parameters (data arguments)
        if ( ( strInputData != "" ) && ( strOutputData == "" ) )
        {
            _EXCEPTIONT ( "--in_data specified without --out_data" );
        }
        if ( ( strInputData == "" ) && ( strOutputData != "" ) )
        {
            _EXCEPTIONT ( "--out_data specified without --in_data" );
        }

        // Check command line parameters (data type arguments)
        STLStringHelper::ToLower ( strInputType );
        STLStringHelper::ToLower ( strOutputType );

        DiscretizationType eInputType;
        DiscretizationType eOutputType;

        if ( strInputType == "fv" )
        {
            eInputType = DiscretizationType_FV;
        }
        else if ( strInputType == "cgll" )
        {
            eInputType = DiscretizationType_CGLL;
        }
        else if ( strInputType == "dgll" )
        {
            eInputType = DiscretizationType_DGLL;
        }
        else
        {
            _EXCEPTION1 ( "Invalid \"in_type\" value (%s), expected [fv|cgll|dgll]",
                          strInputType.c_str() );
        }

        if ( strOutputType == "fv" )
        {
            eOutputType = DiscretizationType_FV;
        }
        else if ( strOutputType == "cgll" )
        {
            eOutputType = DiscretizationType_CGLL;
        }
        else if ( strOutputType == "dgll" )
        {
            eOutputType = DiscretizationType_DGLL;
        }
        else
        {
            _EXCEPTION1 ( "Invalid \"out_type\" value (%s), expected [fv|cgll|dgll]",
                          strOutputType.c_str() );
        }

        // Monotonicity flags
        int nMonotoneType = fMonotoneTypeID;

        // Parse variable list
        std::vector< std::string > vecVariableStrings;
        ParseVariableList ( strVariables, vecVariableStrings );

        // Parse preserve variable list
        std::vector< std::string > vecPreserveVariableStrings;
        ParseVariableList ( strPreserveVariables, vecPreserveVariableStrings );

        if ( fPreserveAll && ( vecPreserveVariableStrings.size() != 0 ) )
        {
            _EXCEPTIONT ( "--preserveall and --preserve cannot both be specified" );
        }

        m_nDofsPEl_Src = nPin;
        m_nDofsPEl_Dest = nPout;

        rval = SetDofMapTags(srcDofTagName, tgtDofTagName);

        // Calculate Face areas
        if ( is_root ) dbgprint.printf ( 0, "Calculating input mesh Face areas\n" );
        double dTotalAreaInput_loc = m_meshInput->CalculateFaceAreas(fInputConcave);
        double dTotalAreaInput = dTotalAreaInput_loc;
#ifdef MOAB_HAVE_MPI
        if (pcomm) MPI_Allreduce ( &dTotalAreaInput_loc, &dTotalAreaInput, 1, MPI_DOUBLE, MPI_SUM, pcomm->comm() );
#endif
        if ( is_root ) dbgprint.printf ( 0, "Input Mesh Geometric Area: %1.15e\n", dTotalAreaInput );

        // Input mesh areas
        m_meshInputCov->CalculateFaceAreas(fInputConcave);
        if ( eInputType == DiscretizationType_FV )
        {
            this->SetSourceAreas ( m_meshInputCov->vecFaceArea );
        }

        // Calculate Face areas
        if ( is_root ) dbgprint.printf ( 0, "Calculating output mesh Face areas\n" );
        double dTotalAreaOutput_loc = m_meshOutput->CalculateFaceAreas(fOutputConcave);
        double dTotalAreaOutput = dTotalAreaOutput_loc;
#ifdef MOAB_HAVE_MPI
        if (pcomm) MPI_Allreduce ( &dTotalAreaOutput_loc, &dTotalAreaOutput, 1, MPI_DOUBLE, MPI_SUM, pcomm->comm() );
#endif
        if ( is_root ) dbgprint.printf ( 0, "Output Mesh Geometric Area: %1.15e\n", dTotalAreaOutput );

        // Output mesh areas
        if ( eOutputType == DiscretizationType_FV )
        {
            this->SetTargetAreas ( m_meshOutput->vecFaceArea );
        }

        // Verify that overlap mesh is in the correct order
        int ixSourceFaceMax = ( -1 );
        int ixTargetFaceMax = ( -1 );

        if ( m_meshOverlap->vecSourceFaceIx.size() !=
                m_meshOverlap->vecTargetFaceIx.size()
           )
        {
            _EXCEPTIONT ( "Invalid overlap mesh:\n"
                          "    Possible mesh file corruption?" );
        }

        for ( unsigned i = 0; i < m_meshOverlap->vecSourceFaceIx.size(); i++ )
        {
            if ( m_meshOverlap->vecSourceFaceIx[i] + 1 > ixSourceFaceMax )
            {
                ixSourceFaceMax = m_meshOverlap->vecSourceFaceIx[i] + 1;
            }
            if ( m_meshOverlap->vecTargetFaceIx[i] + 1 > ixTargetFaceMax )
            {
                ixTargetFaceMax = m_meshOverlap->vecTargetFaceIx[i] + 1;
            }
        }

        /*
        // Check for forward correspondence in overlap mesh
        if ( // m_meshInputCov->faces.size() - ixSourceFaceMax == 0 //&&
            ( m_meshOutput->faces.size() - ixTargetFaceMax == 0 )
        )
        {
            if ( is_root ) dbgprint.printf ( 0, "Overlap mesh forward correspondence found\n" );
        }
        else if (
            // m_meshOutput->faces.size() - ixSourceFaceMax == 0 //&&
            ( m_meshInputCov->faces.size() - ixTargetFaceMax == 0 )
        )
        {   // Check for reverse correspondence in overlap mesh
            if ( is_root ) dbgprint.printf ( 0, "Overlap mesh reverse correspondence found (reversing)\n" );

            // Reorder overlap mesh
            m_meshOverlap->ExchangeFirstAndSecondMesh();
        }
        else
        {   // No correspondence found
            _EXCEPTION4 ( "Invalid overlap mesh:\n"
                          "    No correspondence found with input and output meshes (%i,%i) vs (%i,%i)",
                          m_meshInputCov->faces.size(), m_meshOutput->faces.size(), ixSourceFaceMax, ixTargetFaceMax );
        }
        */

        // Calculate Face areas
        if ( is_root ) dbgprint.printf ( 0, "Calculating overlap mesh Face areas\n" );
        double dTotalAreaOverlap_loc = m_meshOverlap->CalculateFaceAreas(false);
        double dTotalAreaOverlap = dTotalAreaOverlap_loc;
#ifdef MOAB_HAVE_MPI
        if (pcomm) MPI_Allreduce ( &dTotalAreaOverlap_loc, &dTotalAreaOverlap, 1, MPI_DOUBLE, MPI_SUM, pcomm->comm() );
#endif
        if ( is_root ) dbgprint.printf ( 0, "Overlap Mesh Area: %1.15e\n", dTotalAreaOverlap );

        // Partial cover
        if ( fabs ( dTotalAreaOverlap - dTotalAreaInput ) > 1.0e-10 )
        {
            if ( !fNoCheck )
            {
                if ( is_root ) dbgprint.printf ( 0, "WARNING: Significant mismatch between overlap mesh area "
                                                            "and input mesh area.\n  Automatically enabling --nocheck\n" );
                fNoCheck = true;
            }
        }

        /*
            // Recalculate input mesh area from overlap mesh
            if (fabs(dTotalAreaOverlap - dTotalAreaInput) > 1.0e-10) {
                dbgprint.printf(0, "Overlap mesh only covers a sub-area of the sphere\n");
                dbgprint.printf(0, "Recalculating source mesh areas\n");
                dTotalAreaInput = m_meshInput->CalculateFaceAreasFromOverlap(m_meshOverlap);
                dbgprint.printf(0, "New Input Mesh Geometric Area: %1.15e\n", dTotalAreaInput);
            }
        */
        // Finite volume input / Finite volume output
        if ( ( eInputType  == DiscretizationType_FV ) &&
                ( eOutputType == DiscretizationType_FV )
           )
        {
            // Generate reverse node array and edge map
            m_meshInputCov->ConstructReverseNodeArray();
            m_meshInputCov->ConstructEdgeMap();

            // Initialize coordinates for map
            this->InitializeSourceCoordinatesFromMeshFV ( *m_meshInputCov );
            this->InitializeTargetCoordinatesFromMeshFV ( *m_meshOutput );

            // Finite volume input / Finite element output
            rval = this->SetDofMapAssociation(eInputType, false, NULL, NULL, eOutputType, false, NULL);MB_CHK_ERR(rval);

            // Construct OfflineMap
            if ( is_root ) dbgprint.printf ( 0, "Calculating offline map\n" );
            LinearRemapFVtoFV_Tempest_MOAB ( nPin );
        }
        else if ( eInputType == DiscretizationType_FV )
        {
            DataMatrix3D<double> dataGLLJacobian;

            if ( is_root ) dbgprint.printf ( 0, "Generating output mesh meta data\n" );
            double dNumericalArea_loc =
                GenerateMetaData (
                    *m_meshOutput,
                    nPout,
                    fBubble,
                    dataGLLNodesDest,
                    dataGLLJacobian );

            double dNumericalArea = dNumericalArea_loc;
#ifdef MOAB_HAVE_MPI
            if (pcomm) MPI_Allreduce ( &dNumericalArea_loc, &dNumericalArea, 1, MPI_DOUBLE, MPI_SUM, pcomm->comm() );
#endif
            if ( is_root ) dbgprint.printf ( 0, "Output Mesh Numerical Area: %1.15e\n", dNumericalArea );

            // Initialize coordinates for map
            this->InitializeSourceCoordinatesFromMeshFV ( *m_meshInputCov );
            this->InitializeTargetCoordinatesFromMeshFE (
                *m_meshOutput, nPout, dataGLLNodesDest );

            // Generate the continuous Jacobian
            bool fContinuous = ( eOutputType == DiscretizationType_CGLL );

            if ( eOutputType == DiscretizationType_CGLL )
            {
                GenerateUniqueJacobian (
                    dataGLLNodesDest,
                    dataGLLJacobian,
                    this->GetTargetAreas() );
            }
            else
            {
                GenerateDiscontinuousJacobian (
                    dataGLLJacobian,
                    this->GetTargetAreas() );
            }

            // Generate reverse node array and edge map
            m_meshInputCov->ConstructReverseNodeArray();
            m_meshInputCov->ConstructEdgeMap();

            // Finite volume input / Finite element output
            rval = this->SetDofMapAssociation(eInputType, false, NULL, NULL, 
                eOutputType, (eOutputType == DiscretizationType_CGLL), &dataGLLNodesDest);MB_CHK_ERR(rval);

            // Generate remap weights
            if ( is_root ) dbgprint.printf ( 0, "Calculating offline map\n" );

            if ( fVolumetric )
            {
                LinearRemapFVtoGLL_Volumetric_MOAB (
                    dataGLLNodesDest,
                    dataGLLJacobian,
                    this->GetTargetAreas(),
                    nPin,
                    nMonotoneType,
                    fContinuous,
                    fNoConservation );
            }
            else
            {
                LinearRemapFVtoGLL_MOAB (
                    dataGLLNodesDest,
                    dataGLLJacobian,
                    this->GetTargetAreas(),
                    nPin,
                    nMonotoneType,
                    fContinuous,
                    fNoConservation );
            }
        }
        else if (
            ( eInputType != DiscretizationType_FV ) &&
            ( eOutputType == DiscretizationType_FV )
        )
        {
            DataMatrix3D<double> dataGLLJacobianSrc, dataGLLJacobian;

            if ( is_root ) dbgprint.printf ( 0, "Generating input mesh meta data\n" );
            // double dNumericalAreaCov_loc =
                GenerateMetaData (
                    *m_meshInputCov,
                    nPin,
                    fBubble,
                    dataGLLNodesSrcCov,
                    dataGLLJacobian );

            double dNumericalArea_loc =
                GenerateMetaData (
                    *m_meshInput,
                    nPin,
                    fBubble,
                    dataGLLNodesSrc,
                    dataGLLJacobianSrc );

            // if ( is_root ) dbgprint.printf ( 0, "Input Mesh: Coverage Area: %1.15e, Output Area: %1.15e\n", dNumericalAreaCov_loc, dTotalAreaOutput_loc );
            // assert(dNumericalAreaCov_loc >= dTotalAreaOutput_loc);

            double dNumericalArea = dNumericalArea_loc;
#ifdef MOAB_HAVE_MPI
            if (pcomm) MPI_Allreduce ( &dNumericalArea_loc, &dNumericalArea, 1, MPI_DOUBLE, MPI_SUM, pcomm->comm() );
#endif
            if ( is_root ) dbgprint.printf ( 0, "Input Mesh Numerical Area: %1.15e\n", dNumericalArea );

            if ( fabs ( dNumericalArea - dTotalAreaInput ) > 1.0e-12 )
            {
                dbgprint.printf ( 0, "WARNING: Significant mismatch between input mesh "
                                  "numerical area and geometric area\n" );
            }

            if ( dataGLLNodesSrcCov.GetSubColumns() != m_meshInputCov->faces.size() )
            {
                _EXCEPTIONT ( "Number of element does not match between metadata and "
                              "input mesh" );
            }

            // Initialize coordinates for map
            this->InitializeSourceCoordinatesFromMeshFE (
                *m_meshInputCov, nPin, dataGLLNodesSrcCov );
            this->InitializeSourceCoordinatesFromMeshFE (
                *m_meshInput, nPin, dataGLLNodesSrc );
            this->InitializeTargetCoordinatesFromMeshFV ( *m_meshOutput );

            // Generate the continuous Jacobian for input mesh
            bool fContinuousIn = ( eInputType == DiscretizationType_CGLL );

            if ( eInputType == DiscretizationType_CGLL )
            {
                GenerateUniqueJacobian (
                    dataGLLNodesSrcCov,
                    dataGLLJacobian,
                    this->GetSourceAreas() );
            }
            else
            {
                GenerateDiscontinuousJacobian (
                    dataGLLJacobian,
                    this->GetSourceAreas() );
            }

            // Finite element input / Finite volume output
            rval = this->SetDofMapAssociation(eInputType, (eInputType == DiscretizationType_CGLL), &dataGLLNodesSrcCov, &dataGLLNodesSrc, 
                eOutputType, false, NULL);MB_CHK_ERR(rval);

            // Generate offline map
            if ( is_root ) dbgprint.printf ( 0, "Calculating offline map\n" );

            if ( fVolumetric )
            {
                _EXCEPTIONT ( "Unimplemented: Volumetric currently unavailable for"
                              "GLL input mesh" );
            }

            LinearRemapSE4_Tempest_MOAB (
                dataGLLNodesSrcCov,
                dataGLLJacobian,
                nMonotoneType,
                fContinuousIn,
                fNoConservation
            );

        }
        else if (
            ( eInputType  != DiscretizationType_FV ) &&
            ( eOutputType != DiscretizationType_FV )
        )
        {
            DataMatrix3D<double> dataGLLJacobianIn, dataGLLJacobianSrc;
            DataMatrix3D<double> dataGLLJacobianOut;

            // Input metadata
            if ( is_root ) dbgprint.printf ( 0, "Generating input mesh meta data" );
            double dNumericalAreaIn_loc =
                GenerateMetaData (
                    *m_meshInputCov,
                    nPin,
                    fBubble,
                    dataGLLNodesSrcCov,
                    dataGLLJacobianIn );

            double dNumericalAreaSrc_loc =
                GenerateMetaData (
                    *m_meshInput,
                    nPin,
                    fBubble,
                    dataGLLNodesSrc,
                    dataGLLJacobianSrc );

            assert(dNumericalAreaIn_loc >= dNumericalAreaSrc_loc);

            double dNumericalAreaIn = dNumericalAreaSrc_loc;
#ifdef MOAB_HAVE_MPI
            if (pcomm) MPI_Allreduce ( &dNumericalAreaSrc_loc, &dNumericalAreaIn, 1, MPI_DOUBLE, MPI_SUM, pcomm->comm() );
#endif
            if ( is_root ) dbgprint.printf ( 0, "Input Mesh Numerical Area: %1.15e", dNumericalAreaIn );

            if ( fabs ( dNumericalAreaIn - dTotalAreaInput ) > 1.0e-12 )
            {
                dbgprint.printf ( 0, "WARNING: Significant mismatch between input mesh "
                                  "numerical area and geometric area" );
            }

            // Output metadata
            if ( is_root ) dbgprint.printf ( 0, "Generating output mesh meta data" );
            double dNumericalAreaOut_loc =
                GenerateMetaData (
                    *m_meshOutput,
                    nPout,
                    fBubble,
                    dataGLLNodesDest,
                    dataGLLJacobianOut );

            double dNumericalAreaOut = dNumericalAreaOut_loc;
#ifdef MOAB_HAVE_MPI
            if (pcomm) MPI_Allreduce ( &dNumericalAreaOut_loc, &dNumericalAreaOut, 1, MPI_DOUBLE, MPI_SUM, pcomm->comm() );
#endif
            if ( is_root ) dbgprint.printf ( 0, "Output Mesh Numerical Area: %1.15e", dNumericalAreaOut );

            if ( fabs ( dNumericalAreaOut - dTotalAreaOutput ) > 1.0e-12 )
            {
                if ( is_root ) dbgprint.printf ( 0, "WARNING: Significant mismatch between output mesh "
                                                            "numerical area and geometric area" );
            }

            // Initialize coordinates for map
            this->InitializeSourceCoordinatesFromMeshFE (
                *m_meshInputCov, nPin, dataGLLNodesSrcCov );
            this->InitializeSourceCoordinatesFromMeshFE (
                *m_meshInput, nPin, dataGLLNodesSrc );
            this->InitializeTargetCoordinatesFromMeshFE (
                *m_meshOutput, nPout, dataGLLNodesDest );

            // Generate the continuous Jacobian for input mesh
            bool fContinuousIn = ( eInputType == DiscretizationType_CGLL );

            if ( eInputType == DiscretizationType_CGLL )
            {
                GenerateUniqueJacobian (
                    dataGLLNodesSrcCov,
                    dataGLLJacobianIn,
                    this->GetSourceAreas() );
            }
            else
            {
                GenerateDiscontinuousJacobian (
                    dataGLLJacobianIn,
                    this->GetSourceAreas() );
            }

            // Generate the continuous Jacobian for output mesh
            bool fContinuousOut = ( eOutputType == DiscretizationType_CGLL );

            if ( eOutputType == DiscretizationType_CGLL )
            {
                GenerateUniqueJacobian (
                    dataGLLNodesDest,
                    dataGLLJacobianOut,
                    this->GetTargetAreas() );
            }
            else
            {
                GenerateDiscontinuousJacobian (
                    dataGLLJacobianOut,
                    this->GetTargetAreas() );
            }

            // Input Finite Element to Output Finite Element
            rval = this->SetDofMapAssociation(eInputType, (eInputType == DiscretizationType_CGLL), &dataGLLNodesSrcCov, &dataGLLNodesSrc, 
                eOutputType, (eOutputType == DiscretizationType_CGLL), &dataGLLNodesDest);MB_CHK_ERR(rval);

            // Generate offline map
            if ( is_root ) dbgprint.printf ( 0, "Calculating offline map" );

            LinearRemapGLLtoGLL2_MOAB (
                dataGLLNodesSrcCov,
                dataGLLJacobianIn,
                dataGLLNodesDest,
                dataGLLJacobianOut,
                this->GetTargetAreas(),
                nPin,
                nPout,
                nMonotoneType,
                fContinuousIn,
                fContinuousOut,
                fNoConservation
            );

        }
        else
        {
            _EXCEPTIONT ( "Not implemented" );
        }

#ifdef MOAB_HAVE_EIGEN
        CopyTempestSparseMat_Eigen();
#endif

        // Verify consistency, conservation and monotonicity
        // gather weights to root process to perform consistency/conservation checks
        if ( !fNoCheck && false)
        {
            if ( !m_globalMapAvailable && size > 1 ) {
                // gather weights to root process to perform consistency/conservation checks
                rval = this->GatherAllToRoot();MB_CHK_ERR(rval);
            }

            if ( is_root ) dbgprint.printf ( 0, "Verifying map" );
            this->IsConsistent ( 1.0e-8 );
            if ( !fNoConservation ) this->IsConservative ( 1.0e-8 );

            if ( nMonotoneType != 0 )
            {
                this->IsMonotone ( 1.0e-12 );
            }
        }

        // Apply Offline Map to data
        if ( strInputData != "" )
        {
            if ( is_root ) dbgprint.printf ( 0, "Applying offline map to data\n" );

            this->SetFillValueOverride ( static_cast<float> ( dFillValueOverride ) );
            this->Apply (
                strInputData,
                strOutputData,
                vecVariableStrings,
                strNColName,
                fOutputDouble,
                false );
        }

        // Copy variables from input file to output file
        if ( ( strInputData != "" ) && ( strOutputData != "" ) )
        {
            if ( fPreserveAll )
            {
                if ( is_root ) dbgprint.printf ( 0, "Preserving variables" );
                this->PreserveAllVariables ( strInputData, strOutputData );

            }
            else if ( vecPreserveVariableStrings.size() != 0 )
            {
                if ( is_root ) dbgprint.printf ( 0, "Preserving variables" );
                this->PreserveVariables (
                    strInputData,
                    strOutputData,
                    vecPreserveVariableStrings );
            }
        }

    }
    catch ( Exception & e )
    {
        dbgprint.printf ( 0, "%s", e.ToString().c_str() );
        return ( moab::MB_FAILURE );

    }
    catch ( ... )
    {
        return ( moab::MB_FAILURE );
    }
    return moab::MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////
#ifdef MOAB_HAVE_EIGEN

int moab::TempestOfflineMap::GetSourceGlobalNDofs()
{
    return m_weightMatrix.cols(); // return the global number of rows from the weight matrix
}

// ///////////////////////////////////////////////////////////////////////////////

int moab::TempestOfflineMap::GetDestinationGlobalNDofs()
{
    return m_weightMatrix.rows(); // return the global number of columns from the weight matrix
}

///////////////////////////////////////////////////////////////////////////////

int moab::TempestOfflineMap::GetSourceLocalNDofs()
{
    return m_weightMatrix.cols(); // return the local number of rows from the weight matrix
}

///////////////////////////////////////////////////////////////////////////////

int moab::TempestOfflineMap::GetDestinationLocalNDofs()
{
    return m_weightMatrix.rows(); // return the local number of columns from the weight matrix
}

///////////////////////////////////////////////////////////////////////////////

int moab::TempestOfflineMap::GetSourceNDofsPerElement()
{
    return m_nDofsPEl_Src;
}

///////////////////////////////////////////////////////////////////////////////

int moab::TempestOfflineMap::GetDestinationNDofsPerElement()
{
    return m_nDofsPEl_Dest;
}

///////////////////////////////////////////////////////////////////////////////

void moab::TempestOfflineMap::InitVectors()
{
    assert(m_weightMatrix.rows() != 0 && m_weightMatrix.cols() != 0);
    m_rowVector.resize( m_weightMatrix.rows() );
    m_colVector.resize( m_weightMatrix.cols() );
}


moab::TempestOfflineMap::WeightMatrix& moab::TempestOfflineMap::GetWeightMatrix()
{
    assert(m_weightMatrix.rows() != 0 && m_weightMatrix.cols() != 0);
    return m_weightMatrix;
}

moab::TempestOfflineMap::WeightRowVector& moab::TempestOfflineMap::GetRowVector()
{
    assert(m_rowVector.size() != 0);
    return m_rowVector;
}

moab::TempestOfflineMap::WeightColVector& moab::TempestOfflineMap::GetColVector()
{
    assert(m_colVector.size() != 0);
    return m_colVector;
}

#endif

///////////////////////////////////////////////////////////////////////////////

bool moab::TempestOfflineMap::IsConsistent (
    double dTolerance
)
{
    // Get map entries
    DataVector<int> dataRows;
    DataVector<int> dataCols;
    DataVector<double> dataEntries;

    // Calculate row sums
    DataVector<double> dRowSums;
    if ( size > 1 )
    {
        if ( rank ) return true;
        SparseMatrix<double>& m_mapRemapGlobal = m_weightMapGlobal->GetSparseMatrix();
        m_mapRemapGlobal.GetEntries ( dataRows, dataCols, dataEntries );
        dRowSums.Initialize ( m_mapRemapGlobal.GetRows() );
    }
    else
    {
        m_mapRemap.GetEntries ( dataRows, dataCols, dataEntries );
        dRowSums.Initialize ( m_mapRemap.GetRows() );
    }

    for ( unsigned i = 0; i < dataRows.GetRows(); i++ )
    {
        dRowSums[dataRows[i]] += dataEntries[i];
    }

    // Verify all row sums are equal to 1
    bool fConsistent = true;
    for ( unsigned i = 0; i < dRowSums.GetRows(); i++ )
    {
        if ( fabs ( dRowSums[i] - 1.0 ) > dTolerance )
        {
            fConsistent = false;
            Announce ( "TempestOfflineMap is not consistent in row %i (%1.15e)",
                       i, dRowSums[i] );
        }
    }

    return fConsistent;
}

///////////////////////////////////////////////////////////////////////////////

bool moab::TempestOfflineMap::IsConservative (
    double dTolerance
)
{
    // Get map entries
    DataVector<int> dataRows;
    DataVector<int> dataCols;
    DataVector<double> dataEntries;
    const DataVector<double>& dTargetAreas = this->GetGlobalTargetAreas();
    const DataVector<double>& dSourceAreas = this->GetGlobalSourceAreas();

    // Calculate column sums
    DataVector<double> dColumnSums;
    if ( size > 1 )
    {
        if ( rank ) return true;
        SparseMatrix<double>& m_mapRemapGlobal = m_weightMapGlobal->GetSparseMatrix();
        m_mapRemapGlobal.GetEntries ( dataRows, dataCols, dataEntries );
        dColumnSums.Initialize ( m_mapRemapGlobal.GetColumns() );
    }
    else
    {
        m_mapRemap.GetEntries ( dataRows, dataCols, dataEntries );
        dColumnSums.Initialize ( m_mapRemap.GetColumns() );
    }

    for ( unsigned i = 0; i < dataRows.GetRows(); i++ )
    {
        dColumnSums[dataCols[i]] +=
            dataEntries[i] * dTargetAreas[dataRows[i]];
    }

    // Verify all column sums equal the input Jacobian
    bool fConservative = true;
    for ( unsigned i = 0; i < dColumnSums.GetRows(); i++ )
    {
        if ( fabs ( dColumnSums[i] - dSourceAreas[i] ) > dTolerance )
        {
            fConservative = false;
            Announce ( "TempestOfflineMap is not conservative in column "
                       "%i (%1.15e / %1.15e)",
                       i, dColumnSums[i], dSourceAreas[i] );
        }
    }

    return fConservative;
}

///////////////////////////////////////////////////////////////////////////////

bool moab::TempestOfflineMap::IsMonotone (
    double dTolerance
)
{
    // Get map entries
    DataVector<int> dataRows;
    DataVector<int> dataCols;
    DataVector<double> dataEntries;

    if ( size > 1 )
    {
        if ( rank ) return true;
        SparseMatrix<double>& m_mapRemapGlobal = m_weightMapGlobal->GetSparseMatrix();
        m_mapRemapGlobal.GetEntries ( dataRows, dataCols, dataEntries );
    }
    else
        m_mapRemap.GetEntries ( dataRows, dataCols, dataEntries );

    // Verify all entries are in the range [0,1]
    bool fMonotone = true;
    for ( unsigned i = 0; i < dataRows.GetRows(); i++ )
    {
        if ( ( dataEntries[i] < -dTolerance ) ||
                ( dataEntries[i] > 1.0 + dTolerance )
           )
        {
            fMonotone = false;

            Announce ( "TempestOfflineMap is not monotone in entry (%i): %1.15e",
                       i, dataEntries[i] );
        }
    }

    return fMonotone;
}


///////////////////////////////////////////////////////////////////////////////
#ifdef MOAB_HAVE_MPI
moab::ErrorCode moab::TempestOfflineMap::GatherAllToRoot()   // Collective
{
    Mesh globalMesh;
    int ierr, rootProc = 0, nprocs = size;
    moab::ErrorCode rval;

    // Write SparseMatrix entries
    DataVector<int> vecRow;
    DataVector<int> vecCol;
    DataVector<double> vecS;

    moab::DebugOutput dbgprint ( std::cout, ( rank ) );

    m_mapRemap.GetEntries ( vecRow, vecCol, vecS );
    const DataVector<double>& dOrigSourceAreas = m_meshInput->vecFaceArea;
    const DataVector<double>& dSourceAreas = m_meshInputCov->vecFaceArea;
    const DataVector<double>& dTargetAreas = m_meshOutput->vecFaceArea;

    // Translate the index in Row and Col to global_id and dump it out

    // Communicate the necessary data
    const int NDATA = 7;
    int gnnz = 0, gsrc = 0, gsrccov = 0, gtar = 0, gsrcdofs = 0, gsrccovdofs = 0, gtgtdofs = 0;
    std::vector<int> rootSizesData, rowcolsv;
    DataVector<int> rows, cols, srcelmindx, tgtelmindx;
    {
        // First, accumulate the sizes of rows and columns of the matrix
        if ( !rank ) rootSizesData.resize ( size * NDATA );

        int sendarray[NDATA];
        sendarray[0] = vecS.GetRows();
        sendarray[1] = dOrigSourceAreas.GetRows();
        sendarray[2] = dTargetAreas.GetRows();
        sendarray[3] = dSourceAreas.GetRows();
        sendarray[4] = m_nTotDofs_Src;
        sendarray[5] = m_nTotDofs_Dest;
        sendarray[6] = m_nTotDofs_SrcCov;
        
        ierr = MPI_Gather ( sendarray, NDATA, MPI_INTEGER, rootSizesData.data(), NDATA, MPI_INTEGER, rootProc, pcomm->comm() );
        if ( ierr != MPI_SUCCESS ) return moab::MB_FAILURE;

        if ( !rank )
        {
            for ( int i = 0; i < nprocs; ++i )
            {
                unsigned offset = NDATA * i;
                gnnz     += rootSizesData[offset];
                gsrc     += rootSizesData[offset + 1];
                gtar     += rootSizesData[offset + 2];
                gsrccov  += rootSizesData[offset + 3];
                gsrcdofs += rootSizesData[offset + 4];
                gtgtdofs += rootSizesData[offset + 5];
                gsrccovdofs  += rootSizesData[offset + 6];
            }
            rowcolsv.resize ( 2 * gnnz + gsrc + gtar );
            rows.Initialize ( gnnz ); // we are assuming rows = cols
            cols.Initialize ( gnnz ); // we are assuming rows = cols

            // Let us allocate our global offline map object
            m_weightMapGlobal = new OfflineMap();
            std::vector<std::string> dimNames(1);
            std::vector<int> dimSizes(1);
            dimNames[0] = "num_elem";
            dimSizes[0] = gsrc;
            m_weightMapGlobal->InitializeSourceDimensions(dimNames, dimSizes);
            dimSizes[0] = gtar;
            m_weightMapGlobal->InitializeTargetDimensions(dimNames, dimSizes);

            /* 
                VSM: do we need to gather the entire mesh ?!?! 
                The following initialization can't work correctly without the mesh on the root process
            */
            if (m_srcDiscType == DiscretizationType_FV) /* unsure if we actually care about the type for this */
                m_weightMapGlobal->InitializeSourceCoordinatesFromMeshFV ( *m_meshInput ); 
            else
                m_weightMapGlobal->InitializeSourceCoordinatesFromMeshFE ( *m_meshInput, m_nDofsPEl_Src, dataGLLNodesSrc );

            if (m_destDiscType == DiscretizationType_FV)  /* unsure if we actually care about the type for this */
                m_weightMapGlobal->InitializeTargetCoordinatesFromMeshFV ( *m_meshOutput );
            else
                m_weightMapGlobal->InitializeTargetCoordinatesFromMeshFE ( *m_meshOutput, m_nDofsPEl_Dest, dataGLLNodesDest );

            DataVector<double>& m_areasSrcGlobal = m_weightMapGlobal->GetSourceAreas();
            m_areasSrcGlobal.Initialize ( gsrcdofs ); srcelmindx.Initialize ( gsrc );
            DataVector<double>& m_areasTgtGlobal = m_weightMapGlobal->GetTargetAreas();
            m_areasTgtGlobal.Initialize ( gtgtdofs ); tgtelmindx.Initialize ( gtar );

#ifdef VERBOSE
            dbgprint.printf ( 0, "Received global dimensions: %d, %d\n", vecRow.GetRows(), rows.GetRows() );
            dbgprint.printf ( 0, "Global: n(source) = %d, n(srccov) = %d, and n(target) = %d\n", gsrc, gsrccov, gtar );
            dbgprint.printf ( 0, "Operator size = %d X %d and NNZ = %d\n", gsrcdofs, gtgtdofs, gnnz );
#endif
        }
    }

#ifdef VERBOSE
    {
        std::stringstream sstr;
        sstr << "rowscols_" << rank << ".txt";
        std::ofstream output_file ( sstr.str().c_str() );
        output_file << "VALUES\n";
        for ( unsigned ip = 0; ip < vecS.GetRows(); ++ip )
        {
            output_file << ip << " (" << row_gdofmap[row_ldofmap[vecRow[ip]]] << ", " << col_gdofmap[col_ldofmap[vecCol[ip]]] << ") = " << vecS[ip] << "\n";

        }
        output_file.flush(); // required here
        output_file.close();
    }
#endif

    {
        // Next, accumulate the row and column values for the matrices (in local indexing)
        // Let's gather the data in the following order:
        //      1) row indices,
        //      2) column indices, 
        //      3) GID for source elements, 
        //      4) GID for target elements
        //
        const int nR = 2 * vecS.GetRows() + dOrigSourceAreas.GetRows() + dTargetAreas.GetRows();
        std::vector<int> sendarray ( nR );
        for ( unsigned ix = 0; ix < vecS.GetRows(); ++ix )
        {
            sendarray[ix] = row_gdofmap[row_ldofmap[vecRow[ix]]];
        }
        for ( unsigned ix = 0, offset = vecS.GetRows(); ix < vecS.GetRows(); ++ix )
        {
            sendarray[offset + ix] = col_gdofmap[col_ldofmap[vecCol[ix]]];
        }

        {
            moab::Tag gidtag;
            rval = mbCore->tag_get_handle ( "GLOBAL_ID", gidtag ); MB_CHK_ERR ( rval );
            rval = mbCore->tag_get_data ( gidtag, m_remapper->m_source_entities, &sendarray[2 * vecS.GetRows()] ); MB_CHK_ERR ( rval );
            rval = mbCore->tag_get_data ( gidtag, m_remapper->m_target_entities, &sendarray[2 * vecS.GetRows() + dOrigSourceAreas.GetRows()] ); MB_CHK_ERR ( rval );
        }

        std::vector<int> displs, rcount;
        if ( !rank )
        {
            displs.resize ( size, 0 );
            rcount.resize ( size, 0 );
            int gsum = 0;
            for ( int i = 0; i < nprocs; ++i )
            {
                displs[i] = gsum;
                rcount[i] = 2 * rootSizesData[NDATA * i] + rootSizesData[NDATA * i + 1] + rootSizesData[NDATA * i + 2];
                /* + rootSizesData[NDATA * i + 3] + rootSizesData[NDATA * i + 4] */
                gsum += rcount[i];
            }
            assert ( rowcolsv.size() - gsum == 0 );
        }

        // Both rows and columns have a size of "rowsize"
        ierr = MPI_Gatherv ( &sendarray[0], nR, MPI_INTEGER, &rowcolsv[0], &rcount[0], &displs[0], MPI_INTEGER, rootProc, pcomm->comm() );

        if ( !rank )
        {
#ifdef VERBOSE
            std::ofstream output_file ( "rows-cols.txt", std::ios::out );
            output_file << "ROWS\n";
#endif
            for ( int ip = 0, offset = 0; ip < nprocs; ++ip )
            {
                int istart = displs[ip], iend = istart + rootSizesData[NDATA * ip];
                for ( int i = istart; i < iend; ++i, ++offset )
                {
                    rows[offset] = rowcolsv[i];
#ifdef VERBOSE
                    output_file << offset << " " << rows[offset] << "\n";
#endif
                }
            }
#ifdef VERBOSE
            output_file << "COLS\n";
#endif
            for ( int ip = 0, offset = 0; ip < nprocs; ++ip )
            {
                int istart = displs[ip] + rootSizesData[NDATA * ip], iend = istart + rootSizesData[NDATA * ip];
                for ( int i = istart; i < iend; ++i, ++offset )
                {
                    cols[offset] = rowcolsv[i];
#ifdef VERBOSE
                    output_file << offset << " " << cols[offset] << "\n";
#endif
                }
            }
#ifdef VERBOSE
            output_file.flush(); // required here
            output_file.close();
#endif
            for ( int ip = 0, offset = 0; ip < nprocs; ++ip )
            {
                int istart = displs[ip] + 2 * rootSizesData[NDATA * ip], iend = istart + rootSizesData[NDATA * ip + 1];
                for ( int i = istart; i < iend; ++i, ++offset )
                {
                    srcelmindx[offset] = rowcolsv[i];
                }
            }
            for ( int ip = 0, offset = 0; ip < nprocs; ++ip )
            {
                int istart = displs[ip] + 2 * rootSizesData[NDATA * ip] + rootSizesData[NDATA * ip + 1], iend = istart + rootSizesData[NDATA * ip + 2];
                for ( int i = istart; i < iend; ++i, ++offset )
                {
                    tgtelmindx[offset] = rowcolsv[i];
                }
            }
        } /* (!rank) */
        rowcolsv.clear();
    }

    int nSc = m_dSourceVertexLon.GetColumns();
    int nTc = m_dTargetVertexLon.GetColumns();
    {
        // Next, accumulate the row and column values for the matrices (in local indexing)
        // Let's gather the data in the following order:
        //      1) matrix weights,
        //      2) source element areas, 
        //      3) target element areas, 
        //      4) source element: m_dSourceCenterLon
        //      5) source element: m_dSourceCenterLat
        //      6) target element: m_dTargetCenterLon
        //      7) target element: m_dTargetCenterLat
        //
        const int nR = vecS.GetRows() + dOrigSourceAreas.GetRows() + dTargetAreas.GetRows() + 2*m_nTotDofs_Src*(1+nSc) + 2*m_nTotDofs_Dest*(1+nTc);

        std::vector<double> sendarray ( nR );
        int locoffset = 0;
        std::copy ( ( double* ) vecS, ( double* ) vecS + vecS.GetRows(), sendarray.begin() + locoffset); locoffset += vecS.GetRows();
        std::copy ( ( const double* ) dOrigSourceAreas, ( const double* ) dOrigSourceAreas + dOrigSourceAreas.GetRows(), sendarray.begin() + locoffset ); locoffset += dOrigSourceAreas.GetRows();
        std::copy ( ( const double* ) dTargetAreas, ( const double* ) dTargetAreas + dTargetAreas.GetRows(), sendarray.begin() + locoffset ); locoffset += dTargetAreas.GetRows();

        std::cout << "[" << rank << "] " <<  m_nTotDofs_Src << ", m_dSourceCenterLon.size() = " << m_dSourceCenterLon.GetRows() << " and " << m_nTotDofs_Dest << ", m_dTargetCenterLon.size() = " << m_dTargetCenterLon.GetRows() << "\n";

        std::copy ( ( const double* ) m_dSourceCenterLon, ( const double* ) m_dSourceCenterLon + m_dSourceCenterLon.GetRows(), sendarray.begin() + locoffset ); locoffset += m_dSourceCenterLon.GetRows();
        std::copy ( ( const double* ) m_dSourceCenterLat, ( const double* ) m_dSourceCenterLat + m_dSourceCenterLat.GetRows(), sendarray.begin() + locoffset ); locoffset += m_dSourceCenterLat.GetRows();

        double** dSCLon = m_dSourceVertexLon;
        std::copy ( &dSCLon[0][0], &dSCLon[0][0] + m_dSourceVertexLon.GetRows()*nSc, sendarray.begin() + locoffset ); locoffset += m_dSourceVertexLon.GetRows()*nSc;
        double** dSCLat = m_dSourceVertexLat;
        std::copy ( &dSCLat[0][0], &dSCLat[0][0] + m_dSourceVertexLat.GetRows()*nSc, sendarray.begin() + locoffset ); locoffset += m_dSourceVertexLat.GetRows()*nSc;

        std::copy ( ( const double* ) m_dTargetCenterLon, ( const double* ) m_dTargetCenterLon + m_dTargetCenterLon.GetRows(), sendarray.begin() + locoffset ); locoffset += m_dTargetCenterLon.GetRows();
        std::copy ( ( const double* ) m_dTargetCenterLat, ( const double* ) m_dTargetCenterLat + m_dTargetCenterLat.GetRows(), sendarray.begin() + locoffset ); locoffset += m_dTargetCenterLat.GetRows();

        double** dTCLon = m_dTargetVertexLon;
        std::copy ( &dTCLon[0][0], &dTCLon[0][0] + m_dTargetVertexLon.GetRows()*nTc, sendarray.begin() + locoffset ); locoffset += m_dTargetVertexLon.GetRows()*nTc;
        double** dTCLat = m_dTargetVertexLat;
        std::copy ( &dTCLat[0][0], &dTCLat[0][0] + m_dTargetVertexLat.GetRows()*nTc, sendarray.begin() + locoffset ); locoffset += m_dTargetVertexLat.GetRows()*nTc;

        std::vector<int> displs, rcount;
        int gsum = 0;
        if ( !rank )
        {
            displs.resize ( size, 0 );
            rcount.resize ( size, 0 );
            for ( int i = 0; i < nprocs; ++i )
            {
                displs[i] = gsum;
                rcount[i] = rootSizesData[NDATA * i] + rootSizesData[NDATA * i + 1] + rootSizesData[NDATA * i + 2] + 
                            2 * rootSizesData[NDATA * i + 4] * (1 + nSc) +
                            2 * rootSizesData[NDATA * i + 5] * (1 + nTc);
                gsum += rcount[i];
            }
        }

        std::vector<double> rowcolsvals ( (!rank ? gnnz + gsrc + gtar + 2*gsrcdofs*(1+nSc) + 2*gtgtdofs*(1+nTc) : 0 ) );
        // Both rows and columns have a size of "rowsize"
        ierr = MPI_Gatherv ( sendarray.data(), sendarray.size(), MPI_DOUBLE, rowcolsvals.data(), &rcount[0], &displs[0], MPI_DOUBLE, rootProc, pcomm->comm() );

        if ( !rank )
        {
            SparseMatrix<double>& spmat = m_weightMapGlobal->GetSparseMatrix();
            {
#ifdef VERBOSE
                std::ofstream output_file ( "rows-cols.txt", std::ios::app );
                output_file << "VALUES\n";
#endif
                for ( int ip = 0, offset = 0; ip < nprocs; ++ip )
                {
                    for ( int i = displs[ip]; i < displs[ip] + rootSizesData[NDATA * ip]; ++i, ++offset )
                    {
                        // globvalues[offset] = rowcolsvals[i];
                        spmat(rows[offset], cols[offset]) += rowcolsvals[i];
#ifdef VERBOSE
                        output_file << offset << " (" << rows[offset] << ", " << cols[offset] << ") = " << rowcolsvals[i] << "\n";
#endif
                    }
               }
#ifdef VERBOSE
                output_file.flush(); // required here
                output_file.close();
#endif

            }
            // m_mapRemapGlobal.SetEntries(rows, cols, globvalues);
            // m_weightMapGlobal->GetSparseMatrix().AddEntries ( rows, cols, globvalues );

            {
                DataVector<double>& m_areasSrcGlobal = m_weightMapGlobal->GetSourceAreas();
                DataVector<double>& m_areasTgtGlobal = m_weightMapGlobal->GetTargetAreas();
                // Store the global source and target elements areas
#ifdef VERBOSE
                std::ofstream output_file ( "source-target-areas.txt" );
                output_file << "Source areas (nelems = " << gsrc << ")\n";
#endif
                int offset = 0;
                for ( int ip = 0; ip < nprocs; ++ip )
                {
                    int istart = displs[ip] + rootSizesData[NDATA * ip], iend = istart + rootSizesData[NDATA * ip + 1];
                    for ( int i = istart; i < iend; ++i, ++offset )
                    {
                        // assert(offset < gsrcdofs && srcelmindx[offset] <= gsrcdofs);
                        m_areasSrcGlobal[srcelmindx[offset]] = rowcolsvals[i];
                    }
                }

#ifdef VERBOSE
                for ( unsigned i = 0; i < m_areasSrcGlobal.GetRows(); ++i )
                    output_file << srcelmindx[i] << " " << m_areasSrcGlobal[i] << "\n";
#endif

                offset = 0;
                for ( int ip = 0; ip < nprocs; ++ip )
                {
                    int istart = displs[ip] + rootSizesData[NDATA * ip] + rootSizesData[NDATA * ip + 1], iend = istart + rootSizesData[NDATA * ip + 2];
                    for ( int i = istart; i < iend; ++i, ++offset )
                    {
                        // assert(offset < gtgtdofs && tgtelmindx[offset] < gtgtdofs);
                        m_areasTgtGlobal[tgtelmindx[offset]] = rowcolsvals[i];
                    }
                }
#ifdef VERBOSE
                output_file << "Target areas (nelems = " << gtar << ")\n";
                for ( unsigned i = 0; i < m_areasTgtGlobal.GetRows(); ++i )
                    output_file << tgtelmindx[i] << " " << m_areasTgtGlobal[i] << "\n";
#endif
#ifdef VERBOSE
                output_file.flush(); // required here
                output_file.close();
#endif
            }

            DataVector<double>& dSourceCenterLon = m_weightMapGlobal->GetSourceCenterLon();
            DataVector<double>& dSourceCenterLat = m_weightMapGlobal->GetSourceCenterLat();
            dSourceCenterLon.Initialize(gsrcdofs);
            dSourceCenterLat.Initialize(gsrcdofs);
            {
                int offset = 0;
                for ( int ip = 0; ip < nprocs; ++ip )
                {
                    int ibase = rootSizesData[NDATA * ip] + rootSizesData[NDATA * ip + 1] + rootSizesData[NDATA * ip + 2];
                    int istart = displs[ip] + ibase, iend = istart + rootSizesData[NDATA * ip + 4];
                    for ( int i = istart; i < iend; ++i, ++offset )
                    {
                        // assert(offset < gsrcdofs && srcelmindx[offset] <= gsrcdofs);
                        // dSourceCenterLon[srcelmindx[offset]] = rowcolsvals[i];
                        dSourceCenterLon[offset] = rowcolsvals[i];
                    }
                }

                offset = 0;
                for ( int ip = 0; ip < nprocs; ++ip )
                {
                    int ibase = rootSizesData[NDATA * ip] + rootSizesData[NDATA * ip + 1] + rootSizesData[NDATA * ip + 2] + rootSizesData[NDATA * ip + 4];
                    int istart = displs[ip] + ibase, iend = istart + rootSizesData[NDATA * ip + 4];
                    for ( int i = istart; i < iend; ++i, ++offset )
                    {
                        // dSourceCenterLat[srcelmindx[offset]] = rowcolsvals[i];
                        dSourceCenterLat[offset] = rowcolsvals[i];
                    }
                }
            }


            DataMatrix<double>& dSourceVertexLon = m_weightMapGlobal->GetSourceVertexLon();
            DataMatrix<double>& dSourceVertexLat = m_weightMapGlobal->GetSourceVertexLat();
            dSourceVertexLon.Initialize(gsrcdofs, nSc);
            dSourceVertexLat.Initialize(gsrcdofs, nSc);
            {
                int ioffset = 0;
                for ( int ip = 0; ip < nprocs; ++ip )
                {
                    int ibase = rootSizesData[NDATA * ip] + rootSizesData[NDATA * ip + 1] + rootSizesData[NDATA * ip + 2] + 2 * rootSizesData[NDATA * ip + 4];
                    int istart = displs[ip] + ibase, iend = istart + rootSizesData[NDATA * ip + 4] * nSc;
                    for ( int i = istart; i < iend; ++i, ++ioffset )
                    {
                        for ( int joffset = 0; joffset < nSc; ++joffset)
                            dSourceVertexLon[ioffset][joffset] = rowcolsvals[i+joffset];
                        i += nSc;
                    }
                }

                ioffset = 0;
                for ( int ip = 0; ip < nprocs; ++ip )
                {
                    int ibase = rootSizesData[NDATA * ip] + rootSizesData[NDATA * ip + 1] + rootSizesData[NDATA * ip + 2] + 2 * rootSizesData[NDATA * ip + 4] + rootSizesData[NDATA * ip + 4] * nSc;
                    int istart = displs[ip] + ibase, iend = istart + rootSizesData[NDATA * ip + 4] * nSc;
                    for ( int i = istart; i < iend; ++i, ++ioffset )
                    {
                        for ( int joffset = 0; joffset < nSc; ++joffset)
                            dSourceVertexLat[ioffset][joffset] = rowcolsvals[i+joffset];
                        i += nSc;
                    }
                }
            }

            DataVector<double>& dTargetCenterLon = m_weightMapGlobal->GetTargetCenterLon();
            DataVector<double>& dTargetCenterLat = m_weightMapGlobal->GetTargetCenterLat();
            dTargetCenterLon.Initialize(gtgtdofs);
            dTargetCenterLat.Initialize(gtgtdofs);
            {
                int offset = 0;
                for ( int ip = 0; ip < nprocs; ++ip )
                {
                    int ibase = rootSizesData[NDATA * ip] + rootSizesData[NDATA * ip + 1] + rootSizesData[NDATA * ip + 2] + 2 * rootSizesData[NDATA * ip + 4] * (1+nSc);
                    int istart = displs[ip] + ibase, iend = istart + rootSizesData[NDATA * ip + 5];
                    for ( int i = istart; i < iend; ++i, ++offset )
                    {
                        // assert(offset < gsrcdofs && srcelmindx[offset] <= gsrcdofs);
                        // dTargetCenterLon[tgtelmindx[offset]] = rowcolsvals[i];
                        dTargetCenterLon[offset] = rowcolsvals[i];
                    }
                }

                offset = 0;
                for ( int ip = 0; ip < nprocs; ++ip )
                {
                    int ibase = rootSizesData[NDATA * ip] + rootSizesData[NDATA * ip + 1] + rootSizesData[NDATA * ip + 2] + 2 * rootSizesData[NDATA * ip + 4] * (1+nSc) + rootSizesData[NDATA * ip + 5];
                    int istart = displs[ip] + ibase, iend = istart + rootSizesData[NDATA * ip + 5];
                    for ( int i = istart; i < iend; ++i, ++offset )
                    {
                        // dTargetCenterLat[tgtelmindx[offset]] = rowcolsvals[i];
                        dTargetCenterLat[offset] = rowcolsvals[i];
                    }
                }
            }

            DataMatrix<double>& dTargetVertexLon = m_weightMapGlobal->GetTargetVertexLon();
            DataMatrix<double>& dTargetVertexLat = m_weightMapGlobal->GetTargetVertexLat();
            dTargetVertexLon.Initialize(gtgtdofs, nTc);
            dTargetVertexLat.Initialize(gtgtdofs, nTc);
            {
                int ioffset = 0;
                int nc = dTargetVertexLon.GetColumns();
                for ( int ip = 0; ip < nprocs; ++ip )
                {
                    int ibase = rootSizesData[NDATA * ip] + rootSizesData[NDATA * ip + 1] + rootSizesData[NDATA * ip + 2] + 2 * rootSizesData[NDATA * ip + 4] * (1+nSc) + 2 * rootSizesData[NDATA * ip + 5];
                    int istart = displs[ip] + ibase, iend = istart + rootSizesData[NDATA * ip + 5]*nTc;
                    for ( int i = istart; i < iend; ++i, ++ioffset )
                    {
                        for ( int joffset = 0; joffset < nc; ++joffset)
                            dTargetVertexLon[ioffset][joffset] = rowcolsvals[i+joffset];
                        i += nc;
                    }
                }

                ioffset = 0;
                for ( int ip = 0; ip < nprocs; ++ip )
                {
                    int ibase = rootSizesData[NDATA * ip] + rootSizesData[NDATA * ip + 1] + rootSizesData[NDATA * ip + 2] + 2 * rootSizesData[NDATA * ip + 4] * (1+nSc) + 2 * rootSizesData[NDATA * ip + 5] + rootSizesData[NDATA * ip + 5]*nTc;
                    int istart = displs[ip] + ibase, iend = istart + rootSizesData[NDATA * ip + 5]*nTc;
                    for ( int i = istart; i < iend; ++i, ++ioffset )
                    {
                        for ( int joffset = 0; joffset < nc; ++joffset)
                            dTargetVertexLat[ioffset][joffset] = rowcolsvals[i+joffset];
                        i += nc;
                    }
                }
            }
        }

    }
    rootSizesData.clear();

    // Update all processes that we have a global view of the map available
    // on the root process
    m_globalMapAvailable = true;

#ifdef VERBOSE
    if ( !rank )
    {
        dbgprint.printf ( 0, "Writing out file outGlobalView.nc\n" );
        m_weightMapGlobal->Write ( "outGlobalView.nc" );
    }
#endif
    return moab::MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////
#endif

