/*
 * =====================================================================================
 *
 *       Filename:  TempestOnlineMap.hpp
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
#include "DataArray3D.h"
#include "FiniteElementTools.h"
#include "TriangularQuadrature.h"
#include "GaussQuadrature.h"
#include "GaussLobattoQuadrature.h"
#include "SparseMatrix.h"
#include "STLStringHelper.h"

#include "moab/Remapping/TempestOnlineMap.hpp"
#include "DebugOutput.hpp"
#include "moab/TupleList.hpp"

#include <fstream>
#include <cmath>
#include <cstdlib>

#ifdef MOAB_HAVE_NETCDFPAR
#include "netcdfcpp_par.hpp"
#else
#include "netcdfcpp.h"
#endif

///////////////////////////////////////////////////////////////////////////////

// #define VERBOSE
// #define VVERBOSE
// #define CHECK_INCREASING_DOF

void LinearRemapFVtoGLL( const Mesh& meshInput, const Mesh& meshOutput, const Mesh& meshOverlap,
                         const DataArray3D< int >& dataGLLNodes, const DataArray3D< double >& dataGLLJacobian,
                         const DataArray1D< double >& dataGLLNodalArea, int nOrder, OfflineMap& mapRemap,
                         int nMonotoneType, bool fContinuous, bool fNoConservation );

void LinearRemapFVtoGLL_Volumetric( const Mesh& meshInput, const Mesh& meshOutput, const Mesh& meshOverlap,
                                    const DataArray3D< int >& dataGLLNodes,
                                    const DataArray3D< double >& dataGLLJacobian,
                                    const DataArray1D< double >& dataGLLNodalArea, int nOrder, OfflineMap& mapRemap,
                                    int nMonotoneType, bool fContinuous, bool fNoConservation );

///////////////////////////////////////////////////////////////////////////////

#define MPI_CHK_ERR( err )                                          \
    if( err )                                                       \
    {                                                               \
        std::cout << "MPI Failure. ErrorCode (" << ( err ) << ") "; \
        std::cout << "\nMPI Aborting... \n";                        \
        return moab::MB_FAILURE;                                    \
    }

moab::TempestOnlineMap::TempestOnlineMap( moab::TempestRemapper* remapper ) : OfflineMap(), m_remapper( remapper )
{
    // Get the references for the MOAB core objects
    m_interface = m_remapper->get_interface();
#ifdef MOAB_HAVE_MPI
    m_pcomm = m_remapper->get_parallel_communicator();
#endif

    // Update the references to the meshes
    m_meshInput    = remapper->GetMesh( moab::Remapper::SourceMesh );
    m_meshInputCov = remapper->GetCoveringMesh();
    m_meshOutput   = remapper->GetMesh( moab::Remapper::TargetMesh );
    m_meshOverlap  = remapper->GetMesh( moab::Remapper::OverlapMesh );

    is_parallel = false;
    is_root     = true;
    rank        = 0;
    root_proc   = rank;
    size        = 1;
#ifdef MOAB_HAVE_MPI
    int flagInit;
    MPI_Initialized( &flagInit );
    if( flagInit )
    {
        is_parallel = true;
        assert( m_pcomm != NULL );
        rank    = m_pcomm->rank();
        size    = m_pcomm->size();
        is_root = ( rank == 0 );
    }
#endif

    // Compute and store the total number of source and target DoFs corresponding
    // to number of rows and columns in the mapping.

    // Initialize dimension information from file
    this->setup_sizes_dimensions();

    // Build a matrix of source and target discretization so that we know how to assign
    // the global DoFs in parallel for the mapping weights
    // For example, FV->FV: rows X cols = faces_source X faces_target
}

void moab::TempestOnlineMap::setup_sizes_dimensions()
{
    if( m_meshInputCov )
    {
        std::vector< std::string > dimNames;
        std::vector< int > dimSizes;
        if( m_remapper->m_source_type == moab::TempestRemapper::RLL && m_remapper->m_source_metadata.size() )
        {
            dimNames.push_back( "lat" );
            dimNames.push_back( "lon" );
            dimSizes.resize( 2, 0 );
            dimSizes[0] = m_remapper->m_source_metadata[1];
            dimSizes[1] = m_remapper->m_source_metadata[2];
        }
        else
        {
            dimNames.push_back( "num_elem" );
            dimSizes.push_back( m_meshInputCov->faces.size() );
        }

        this->InitializeSourceDimensions( dimNames, dimSizes );
    }

    if( m_meshOutput )
    {
        std::vector< std::string > dimNames;
        std::vector< int > dimSizes;
        if( m_remapper->m_target_type == moab::TempestRemapper::RLL && m_remapper->m_target_metadata.size() )
        {
            dimNames.push_back( "lat" );
            dimNames.push_back( "lon" );
            dimSizes.resize( 2, 0 );
            dimSizes[0] = m_remapper->m_target_metadata[1];
            dimSizes[1] = m_remapper->m_target_metadata[2];
        }
        else
        {
            dimNames.push_back( "num_elem" );
            dimSizes.push_back( m_meshOutput->faces.size() );
        }

        this->InitializeTargetDimensions( dimNames, dimSizes );
    }
}

///////////////////////////////////////////////////////////////////////////////

moab::TempestOnlineMap::~TempestOnlineMap()
{
    m_interface = NULL;
#ifdef MOAB_HAVE_MPI
    m_pcomm = NULL;
#endif
    m_meshInput   = NULL;
    m_meshOutput  = NULL;
    m_meshOverlap = NULL;
}

///////////////////////////////////////////////////////////////////////////////

moab::ErrorCode moab::TempestOnlineMap::SetDOFmapTags( const std::string srcDofTagName,
                                                       const std::string tgtDofTagName )
{
    moab::ErrorCode rval;

    int tagSize = 0;
    tagSize     = ( m_eInputType == DiscretizationType_FV ? 1 : m_nDofsPEl_Src * m_nDofsPEl_Src );
    rval =
        m_interface->tag_get_handle( srcDofTagName.c_str(), tagSize, MB_TYPE_INTEGER, this->m_dofTagSrc, MB_TAG_ANY );

    if( rval == moab::MB_TAG_NOT_FOUND && m_eInputType != DiscretizationType_FV )
    {
        int ntot_elements = 0, nelements = m_remapper->m_source_entities.size();
#ifdef MOAB_HAVE_MPI
        int ierr = MPI_Allreduce( &nelements, &ntot_elements, 1, MPI_INT, MPI_SUM, m_pcomm->comm() );
        if( ierr != 0 ) MB_CHK_SET_ERR( MB_FAILURE, "MPI_Allreduce failed to get total source elements" );
#else
        ntot_elements = nelements;
#endif

        rval = m_remapper->GenerateCSMeshMetadata( ntot_elements, m_remapper->m_covering_source_entities,
                                                   &m_remapper->m_source_entities, srcDofTagName, m_nDofsPEl_Src );MB_CHK_ERR( rval );

        rval = m_interface->tag_get_handle( srcDofTagName.c_str(), m_nDofsPEl_Src * m_nDofsPEl_Src, MB_TYPE_INTEGER,
                                            this->m_dofTagSrc, MB_TAG_ANY );MB_CHK_ERR( rval );
    }
    else
        MB_CHK_ERR( rval );

    tagSize = ( m_eOutputType == DiscretizationType_FV ? 1 : m_nDofsPEl_Dest * m_nDofsPEl_Dest );
    rval =
        m_interface->tag_get_handle( tgtDofTagName.c_str(), tagSize, MB_TYPE_INTEGER, this->m_dofTagDest, MB_TAG_ANY );
    if( rval == moab::MB_TAG_NOT_FOUND && m_eOutputType != DiscretizationType_FV )
    {
        int ntot_elements = 0, nelements = m_remapper->m_target_entities.size();
#ifdef MOAB_HAVE_MPI
        int ierr = MPI_Allreduce( &nelements, &ntot_elements, 1, MPI_INT, MPI_SUM, m_pcomm->comm() );
        if( ierr != 0 ) MB_CHK_SET_ERR( MB_FAILURE, "MPI_Allreduce failed to get total source elements" );
#else
        ntot_elements = nelements;
#endif

        rval = m_remapper->GenerateCSMeshMetadata( ntot_elements, m_remapper->m_target_entities, NULL, tgtDofTagName,
                                                   m_nDofsPEl_Dest );MB_CHK_ERR( rval );

        rval = m_interface->tag_get_handle( tgtDofTagName.c_str(), m_nDofsPEl_Dest * m_nDofsPEl_Dest, MB_TYPE_INTEGER,
                                            this->m_dofTagDest, MB_TAG_ANY );MB_CHK_ERR( rval );
    }
    else
        MB_CHK_ERR( rval );

    return moab::MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////

moab::ErrorCode moab::TempestOnlineMap::SetDOFmapAssociation( DiscretizationType srcType, bool isSrcContinuous,
                                                              DataArray3D< int >* srcdataGLLNodes,
                                                              DataArray3D< int >* srcdataGLLNodesSrc,
                                                              DiscretizationType destType, bool isTgtContinuous,
                                                              DataArray3D< int >* tgtdataGLLNodes )
{
    moab::ErrorCode rval;
    std::vector< bool > dgll_cgll_row_ldofmap, dgll_cgll_col_ldofmap, dgll_cgll_covcol_ldofmap;
    std::vector< int > src_soln_gdofs, locsrc_soln_gdofs, tgt_soln_gdofs;

    // We are assuming that these are element based tags that are sized: np * np
    m_srcDiscType  = srcType;
    m_destDiscType = destType;

    bool vprint = is_root && false;

#ifdef VVERBOSE
    {
        src_soln_gdofs.resize( m_remapper->m_covering_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, -1 );
        rval = m_interface->tag_get_data( m_dofTagSrc, m_remapper->m_covering_source_entities, &src_soln_gdofs[0] );MB_CHK_ERR( rval );
        locsrc_soln_gdofs.resize( m_remapper->m_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src );
        rval = m_interface->tag_get_data( m_dofTagSrc, m_remapper->m_source_entities, &locsrc_soln_gdofs[0] );MB_CHK_ERR( rval );
        tgt_soln_gdofs.resize( m_remapper->m_target_entities.size() * m_nDofsPEl_Dest * m_nDofsPEl_Dest );
        rval = m_interface->tag_get_data( m_dofTagDest, m_remapper->m_target_entities, &tgt_soln_gdofs[0] );MB_CHK_ERR( rval );

        if( is_root )
        {
            {
                std::ofstream output_file( "sourcecov-gids-0.txt" );
                output_file << "I, GDOF\n";
                for( unsigned i = 0; i < src_soln_gdofs.size(); ++i )
                    output_file << i << ", " << src_soln_gdofs[i] << "\n";

                output_file << "ELEMID, IDOF, LDOF, GDOF, NDOF\n";
                m_nTotDofs_SrcCov = 0;
                if( isSrcContinuous )
                    dgll_cgll_covcol_ldofmap.resize(
                        m_remapper->m_covering_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, false );
                for( unsigned j = 0; j < m_remapper->m_covering_source_entities.size(); j++ )
                {
                    for( int p = 0; p < m_nDofsPEl_Src; p++ )
                    {
                        for( int q = 0; q < m_nDofsPEl_Src; q++ )
                        {
                            const int localDOF  = ( *srcdataGLLNodes )[p][q][j] - 1;
                            const int offsetDOF = j * m_nDofsPEl_Src * m_nDofsPEl_Src + p * m_nDofsPEl_Src + q;
                            if( isSrcContinuous && !dgll_cgll_covcol_ldofmap[localDOF] )
                            {
                                m_nTotDofs_SrcCov++;
                                dgll_cgll_covcol_ldofmap[localDOF] = true;
                            }
                            output_file << m_remapper->lid_to_gid_covsrc[j] << ", " << offsetDOF << ", " << localDOF
                                        << ", " << src_soln_gdofs[offsetDOF] << ", " << m_nTotDofs_SrcCov << "\n";
                        }
                    }
                }
                output_file.flush();  // required here
                output_file.close();
                dgll_cgll_covcol_ldofmap.clear();
            }

            {
                std::ofstream output_file( "source-gids-0.txt" );
                output_file << "I, GDOF\n";
                for( unsigned i = 0; i < locsrc_soln_gdofs.size(); ++i )
                    output_file << i << ", " << locsrc_soln_gdofs[i] << "\n";

                output_file << "ELEMID, IDOF, LDOF, GDOF, NDOF\n";
                m_nTotDofs_Src = 0;
                if( isSrcContinuous )
                    dgll_cgll_col_ldofmap.resize(
                        m_remapper->m_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, false );
                for( unsigned j = 0; j < m_remapper->m_source_entities.size(); j++ )
                {
                    for( int p = 0; p < m_nDofsPEl_Src; p++ )
                    {
                        for( int q = 0; q < m_nDofsPEl_Src; q++ )
                        {
                            const int localDOF  = ( *srcdataGLLNodesSrc )[p][q][j] - 1;
                            const int offsetDOF = j * m_nDofsPEl_Src * m_nDofsPEl_Src + p * m_nDofsPEl_Src + q;
                            if( isSrcContinuous && !dgll_cgll_col_ldofmap[localDOF] )
                            {
                                m_nTotDofs_Src++;
                                dgll_cgll_col_ldofmap[localDOF] = true;
                            }
                            output_file << m_remapper->lid_to_gid_src[j] << ", " << offsetDOF << ", " << localDOF
                                        << ", " << locsrc_soln_gdofs[offsetDOF] << ", " << m_nTotDofs_Src << "\n";
                        }
                    }
                }
                output_file.flush();  // required here
                output_file.close();
                dgll_cgll_col_ldofmap.clear();
            }

            {
                std::ofstream output_file( "target-gids-0.txt" );
                output_file << "I, GDOF\n";
                for( unsigned i = 0; i < tgt_soln_gdofs.size(); ++i )
                    output_file << i << ", " << tgt_soln_gdofs[i] << "\n";

                output_file << "ELEMID, IDOF, GDOF, NDOF\n";
                m_nTotDofs_Dest = 0;

                for( unsigned i = 0; i < tgt_soln_gdofs.size(); ++i )
                {
                    output_file << m_remapper->lid_to_gid_tgt[i] << ", " << i << ", " << tgt_soln_gdofs[i] << ", "
                                << m_nTotDofs_Dest << "\n";
                    m_nTotDofs_Dest++;
                }

                output_file.flush();  // required here
                output_file.close();
            }
        }
        else
        {
            {
                std::ofstream output_file( "sourcecov-gids-1.txt" );
                output_file << "I, GDOF\n";
                for( unsigned i = 0; i < src_soln_gdofs.size(); ++i )
                    output_file << i << ", " << src_soln_gdofs[i] << "\n";

                output_file << "ELEMID, IDOF, LDOF, GDOF, NDOF\n";
                m_nTotDofs_SrcCov = 0;
                if( isSrcContinuous )
                    dgll_cgll_covcol_ldofmap.resize(
                        m_remapper->m_covering_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, false );
                for( unsigned j = 0; j < m_remapper->m_covering_source_entities.size(); j++ )
                {
                    for( int p = 0; p < m_nDofsPEl_Src; p++ )
                    {
                        for( int q = 0; q < m_nDofsPEl_Src; q++ )
                        {
                            const int localDOF  = ( *srcdataGLLNodes )[p][q][j] - 1;
                            const int offsetDOF = j * m_nDofsPEl_Src * m_nDofsPEl_Src + p * m_nDofsPEl_Src + q;
                            if( isSrcContinuous && !dgll_cgll_covcol_ldofmap[localDOF] )
                            {
                                m_nTotDofs_SrcCov++;
                                dgll_cgll_covcol_ldofmap[localDOF] = true;
                            }
                            output_file << m_remapper->lid_to_gid_covsrc[j] << ", " << offsetDOF << ", " << localDOF
                                        << ", " << src_soln_gdofs[offsetDOF] << ", " << m_nTotDofs_SrcCov << "\n";
                        }
                    }
                }
                output_file.flush();  // required here
                output_file.close();
                dgll_cgll_covcol_ldofmap.clear();
            }

            {
                std::ofstream output_file( "source-gids-1.txt" );
                output_file << "I, GDOF\n";
                for( unsigned i = 0; i < locsrc_soln_gdofs.size(); ++i )
                    output_file << i << ", " << locsrc_soln_gdofs[i] << "\n";

                output_file << "ELEMID, IDOF, LDOF, GDOF, NDOF\n";
                m_nTotDofs_Src = 0;
                if( isSrcContinuous )
                    dgll_cgll_col_ldofmap.resize(
                        m_remapper->m_source_entities.size() * m_nDofsPEl_Src * m_nDofsPEl_Src, false );
                for( unsigned j = 0; j < m_remapper->m_source_entities.size(); j++ )
                {
                    for( int p = 0; p < m_nDofsPEl_Src; p++ )
                    {
                        for( int q = 0; q < m_nDofsPEl_Src; q++ )
                        {
                            const int localDOF  = ( *srcdataGLLNodesSrc )[p][q][j] - 1;
                            const int offsetDOF = j * m_nDofsPEl_Src * m_nDofsPEl_Src + p * m_nDofsPEl_Src + q;
                            if( isSrcContinuous && !dgll_cgll_col_ldofmap[localDOF] )
                            {
                                m_nTotDofs_Src++;
                                dgll_cgll_col_ldofmap[localDOF] = true;
                            }
                            output_file << m_remapper->lid_to_gid_src[j] << ", " << offsetDOF << ", " << localDOF
                                        << ", " << locsrc_soln_gdofs[offsetDOF] << ", " << m_nTotDofs_Src << "\n";
                        }
                    }
                }
                output_file.flush();  // required here
                output_file.close();
                dgll_cgll_col_ldofmap.clear();
            }

            {
                std::ofstream output_file( "target-gids-1.txt" );
                output_file << "I, GDOF\n";
                for( unsigned i = 0; i < tgt_soln_gdofs.size(); ++i )
                    output_file << i << ", " << tgt_soln_gdofs[i] << "\n";

                output_file << "ELEMID, IDOF, GDOF, NDOF\n";
                m_nTotDofs_Dest = 0;

                for( unsigned i = 0; i < tgt_soln_gdofs.size(); ++i )
                {
                    output_file << m_remapper->lid_to_gid_tgt[i] << ", " << i << ", " << tgt_soln_gdofs[i] << ", "
                                << m_nTotDofs_Dest << "\n";
                    m_nTotDofs_Dest++;
                }

                output_file.flush();  // required here
                output_file.close();
            }
        }
    }
#endif

    // Now compute the mapping and store it for the covering mesh
    int srcTagSize = ( m_eInputType == DiscretizationType_FV ? 1 : m_nDofsPEl_Src * m_nDofsPEl_Src );
    if( m_remapper->point_cloud_source )
    {
        assert( m_nDofsPEl_Src == 1 );
        col_gdofmap.resize( m_remapper->m_covering_source_vertices.size(), UINT_MAX );
        col_dtoc_dofmap.resize( m_remapper->m_covering_source_vertices.size(), UINT_MAX );
        src_soln_gdofs.resize( m_remapper->m_covering_source_vertices.size(), UINT_MAX );
        rval = m_interface->tag_get_data( m_dofTagSrc, m_remapper->m_covering_source_vertices, &src_soln_gdofs[0] );MB_CHK_ERR( rval );
        srcTagSize = 1;
    }
    else
    {
        col_gdofmap.resize( m_remapper->m_covering_source_entities.size() * srcTagSize, UINT_MAX );
        col_dtoc_dofmap.resize( m_remapper->m_covering_source_entities.size() * srcTagSize, UINT_MAX );
        src_soln_gdofs.resize( m_remapper->m_covering_source_entities.size() * srcTagSize, UINT_MAX );
        rval = m_interface->tag_get_data( m_dofTagSrc, m_remapper->m_covering_source_entities, &src_soln_gdofs[0] );MB_CHK_ERR( rval );
    }

    // std::cout << "TOnlineMap: Process: " << rank << " and covering entities = [" <<
    // col_dofmap.size() << ", " << src_soln_gdofs.size() << "]\n"; MPI_Barrier(MPI_COMM_WORLD);

#ifdef ALTERNATE_NUMBERING_IMPLEMENTATION
    unsigned maxSrcIndx = 0;

    // for ( unsigned j = 0; j < m_covering_source_entities.size(); j++ )
    std::vector< int > locdofs( srcTagSize );
    std::map< Node, moab::EntityHandle > mapLocalMBNodes;
    double elcoords[3];
    for( unsigned iel = 0; iel < m_remapper->m_covering_source_entities.size(); ++iel )
    {
        EntityHandle eh = m_remapper->m_covering_source_entities[iel];
        rval            = m_interface->get_coords( &eh, 1, elcoords );MB_CHK_ERR( rval );
        Node elCentroid( elcoords[0], elcoords[1], elcoords[2] );
        mapLocalMBNodes.insert( std::pair< Node, moab::EntityHandle >( elCentroid, eh ) );
    }

    const NodeVector& nodes = m_remapper->m_covering_source->nodes;
    for( unsigned j = 0; j < m_remapper->m_covering_source->faces.size(); j++ )
    {
        const Face& face = m_remapper->m_covering_source->faces[j];

        Node centroid;
        centroid.x = centroid.y = centroid.z = 0.0;
        for( unsigned l = 0; l < face.edges.size(); ++l )
        {
            centroid.x += nodes[face[l]].x;
            centroid.y += nodes[face[l]].y;
            centroid.z += nodes[face[l]].z;
        }
        const double factor = 1.0 / face.edges.size();
        centroid.x *= factor;
        centroid.y *= factor;
        centroid.z *= factor;

        EntityHandle current_eh;
        if( mapLocalMBNodes.find( centroid ) != mapLocalMBNodes.end() )
        {
            current_eh = mapLocalMBNodes[centroid];
        }

        rval = m_interface->tag_get_data( m_dofTagSrc, &current_eh, 1, &locdofs[0] );MB_CHK_ERR( rval );
        for( int p = 0; p < m_nDofsPEl_Src; p++ )
        {
            for( int q = 0; q < m_nDofsPEl_Src; q++ )
            {
                const int localDOF  = ( *srcdataGLLNodes )[p][q][j] - 1;
                const int offsetDOF = p * m_nDofsPEl_Src + q;
                maxSrcIndx          = ( localDOF > maxSrcIndx ? localDOF : maxSrcIndx );
                std::cout << "Col: " << current_eh << ", " << m_remapper->lid_to_gid_covsrc[j] << ", " << offsetDOF
                          << ", " << localDOF << ", " << locdofs[offsetDOF] - 1 << ", " << maxSrcIndx << "\n";
            }
        }
    }
#endif

    m_nTotDofs_SrcCov = 0;
    if( srcdataGLLNodes == NULL )
    { /* we only have a mapping for elements as DoFs */
        for( unsigned i = 0; i < col_gdofmap.size(); ++i )
        {
            assert( src_soln_gdofs[i] > 0 );
            col_gdofmap[i]     = src_soln_gdofs[i] - 1;
            col_dtoc_dofmap[i] = i;
            if( vprint ) std::cout << "Col: " << i << ", " << col_gdofmap[i] << "\n";
            m_nTotDofs_SrcCov++;
        }
    }
    else
    {
        if( isSrcContinuous )
            dgll_cgll_covcol_ldofmap.resize( m_remapper->m_covering_source_entities.size() * srcTagSize, false );
        // Put these remap coefficients into the SparseMatrix map
        for( unsigned j = 0; j < m_remapper->m_covering_source_entities.size(); j++ )
        {
            for( int p = 0; p < m_nDofsPEl_Src; p++ )
            {
                for( int q = 0; q < m_nDofsPEl_Src; q++ )
                {
                    const int localDOF  = ( *srcdataGLLNodes )[p][q][j] - 1;
                    const int offsetDOF = j * srcTagSize + p * m_nDofsPEl_Src + q;
                    if( isSrcContinuous && !dgll_cgll_covcol_ldofmap[localDOF] )
                    {
                        m_nTotDofs_SrcCov++;
                        dgll_cgll_covcol_ldofmap[localDOF] = true;
                    }
                    if( !isSrcContinuous ) m_nTotDofs_SrcCov++;
                    assert( src_soln_gdofs[offsetDOF] > 0 );
                    col_gdofmap[localDOF]      = src_soln_gdofs[offsetDOF] - 1;
                    col_dtoc_dofmap[offsetDOF] = localDOF;
                    if( vprint )
                        std::cout << "Col: " << m_remapper->lid_to_gid_covsrc[j] << ", " << offsetDOF << ", "
                                  << localDOF << ", " << col_gdofmap[offsetDOF] << ", " << m_nTotDofs_SrcCov << "\n";
                }
            }
        }
    }

    if( m_remapper->point_cloud_source )
    {
        assert( m_nDofsPEl_Src == 1 );
        srccol_gdofmap.resize( m_remapper->m_source_vertices.size(), UINT_MAX );
        srccol_dtoc_dofmap.resize( m_remapper->m_covering_source_vertices.size(), UINT_MAX );
        locsrc_soln_gdofs.resize( m_remapper->m_source_vertices.size(), UINT_MAX );
        rval = m_interface->tag_get_data( m_dofTagSrc, m_remapper->m_source_vertices, &locsrc_soln_gdofs[0] );MB_CHK_ERR( rval );
    }
    else
    {
        srccol_gdofmap.resize( m_remapper->m_source_entities.size() * srcTagSize, UINT_MAX );
        srccol_dtoc_dofmap.resize( m_remapper->m_source_entities.size() * srcTagSize, UINT_MAX );
        locsrc_soln_gdofs.resize( m_remapper->m_source_entities.size() * srcTagSize, UINT_MAX );
        rval = m_interface->tag_get_data( m_dofTagSrc, m_remapper->m_source_entities, &locsrc_soln_gdofs[0] );MB_CHK_ERR( rval );
    }

    // Now compute the mapping and store it for the original source mesh
    m_nTotDofs_Src = 0;
    if( srcdataGLLNodesSrc == NULL )
    { /* we only have a mapping for elements as DoFs */
        for( unsigned i = 0; i < srccol_gdofmap.size(); ++i )
        {
            assert( locsrc_soln_gdofs[i] > 0 );
            srccol_gdofmap[i]     = locsrc_soln_gdofs[i] - 1;
            srccol_dtoc_dofmap[i] = i;
            m_nTotDofs_Src++;
        }
    }
    else
    {
        if( isSrcContinuous ) dgll_cgll_col_ldofmap.resize( m_remapper->m_source_entities.size() * srcTagSize, false );
        // Put these remap coefficients into the SparseMatrix map
        for( unsigned j = 0; j < m_remapper->m_source_entities.size(); j++ )
        {
            for( int p = 0; p < m_nDofsPEl_Src; p++ )
            {
                for( int q = 0; q < m_nDofsPEl_Src; q++ )
                {
                    const int localDOF  = ( *srcdataGLLNodesSrc )[p][q][j] - 1;
                    const int offsetDOF = j * srcTagSize + p * m_nDofsPEl_Src + q;
                    if( isSrcContinuous && !dgll_cgll_col_ldofmap[localDOF] )
                    {
                        m_nTotDofs_Src++;
                        dgll_cgll_col_ldofmap[localDOF] = true;
                    }
                    if( !isSrcContinuous ) m_nTotDofs_Src++;
                    assert( locsrc_soln_gdofs[offsetDOF] > 0 );
                    srccol_gdofmap[localDOF]      = locsrc_soln_gdofs[offsetDOF] - 1;
                    srccol_dtoc_dofmap[offsetDOF] = localDOF;
                }
            }
        }
    }

    int tgtTagSize = ( m_eOutputType == DiscretizationType_FV ? 1 : m_nDofsPEl_Dest * m_nDofsPEl_Dest );
    if( m_remapper->point_cloud_target )
    {
        assert( m_nDofsPEl_Dest == 1 );
        row_gdofmap.resize( m_remapper->m_target_vertices.size(), UINT_MAX );
        row_dtoc_dofmap.resize( m_remapper->m_target_vertices.size(), UINT_MAX );
        tgt_soln_gdofs.resize( m_remapper->m_target_vertices.size(), UINT_MAX );
        rval = m_interface->tag_get_data( m_dofTagDest, m_remapper->m_target_vertices, &tgt_soln_gdofs[0] );MB_CHK_ERR( rval );
        tgtTagSize = 1;
    }
    else
    {
        row_gdofmap.resize( m_remapper->m_target_entities.size() * tgtTagSize, UINT_MAX );
        row_dtoc_dofmap.resize( m_remapper->m_target_entities.size() * tgtTagSize, UINT_MAX );
        tgt_soln_gdofs.resize( m_remapper->m_target_entities.size() * tgtTagSize, UINT_MAX );
        rval = m_interface->tag_get_data( m_dofTagDest, m_remapper->m_target_entities, &tgt_soln_gdofs[0] );MB_CHK_ERR( rval );
    }

    // Now compute the mapping and store it for the target mesh
    // To access the GID for each row: row_gdofmap [ row_ldofmap [ 0 : local_ndofs ] ] = GDOF
    m_nTotDofs_Dest = 0;
    if( tgtdataGLLNodes == NULL )
    { /* we only have a mapping for elements as DoFs */
        for( unsigned i = 0; i < row_gdofmap.size(); ++i )
        {
            assert( tgt_soln_gdofs[i] > 0 );
            row_gdofmap[i]     = tgt_soln_gdofs[i] - 1;
            row_dtoc_dofmap[i] = i;
            if( vprint ) std::cout << "Row: " << i << ", " << row_gdofmap[i] << "\n";
            m_nTotDofs_Dest++;
        }
    }
    else
    {
        if( isTgtContinuous ) dgll_cgll_row_ldofmap.resize( m_remapper->m_target_entities.size() * tgtTagSize, false );
        // Put these remap coefficients into the SparseMatrix map
        for( unsigned j = 0; j < m_remapper->m_target_entities.size(); j++ )
        {
            for( int p = 0; p < m_nDofsPEl_Dest; p++ )
            {
                for( int q = 0; q < m_nDofsPEl_Dest; q++ )
                {
                    const int localDOF  = ( *tgtdataGLLNodes )[p][q][j] - 1;
                    const int offsetDOF = j * tgtTagSize + p * m_nDofsPEl_Dest + q;
                    if( isTgtContinuous && !dgll_cgll_row_ldofmap[localDOF] )
                    {
                        m_nTotDofs_Dest++;
                        dgll_cgll_row_ldofmap[localDOF] = true;
                    }
                    if( !isTgtContinuous ) m_nTotDofs_Dest++;
                    assert( tgt_soln_gdofs[offsetDOF] > 0 );
                    row_gdofmap[localDOF]      = tgt_soln_gdofs[offsetDOF] - 1;
                    row_dtoc_dofmap[offsetDOF] = localDOF;
                    if( vprint )
                        std::cout << "Row: " << m_remapper->lid_to_gid_tgt[j] << ", " << offsetDOF << ", " << localDOF
                                  << ", " << row_gdofmap[offsetDOF] << ", " << m_nTotDofs_Dest << "\n";
                }
            }
        }
    }

    // Let us also allocate the local representation of the sparse matrix
#if defined( MOAB_HAVE_EIGEN3 ) && defined( VERBOSE )
    if( vprint )
    {
        std::cout << "[" << rank << "]"
                  << "DoFs: row = " << m_nTotDofs_Dest << ", " << row_gdofmap.size() << ", col = " << m_nTotDofs_Src
                  << ", " << m_nTotDofs_SrcCov << ", " << col_gdofmap.size() << "\n";
        // std::cout << "Max col_dofmap: " << maxcol << ", Min col_dofmap" << mincol << "\n";
    }
#endif

    // check monotonicity of row_gdofmap and col_gdofmap
#ifdef CHECK_INCREASING_DOF
    for( size_t i = 0; i < row_gdofmap.size() - 1; i++ )
    {
        if( row_gdofmap[i] > row_gdofmap[i + 1] )
            std::cout << " on rank " << rank << " in row_gdofmap[" << i << "]=" << row_gdofmap[i] << " > row_gdofmap["
                      << i + 1 << "]=" << row_gdofmap[i + 1] << " \n";
    }
    for( size_t i = 0; i < col_gdofmap.size() - 1; i++ )
    {
        if( col_gdofmap[i] > col_gdofmap[i + 1] )
            std::cout << " on rank " << rank << " in col_gdofmap[" << i << "]=" << col_gdofmap[i] << " > col_gdofmap["
                      << i + 1 << "]=" << col_gdofmap[i + 1] << " \n";
    }
#endif

    return moab::MB_SUCCESS;
}

moab::ErrorCode moab::TempestOnlineMap::set_col_dc_dofs( std::vector< int >& values_entities )
{
    // col_gdofmap has global dofs , that should be in the list of values, such that
    // row_dtoc_dofmap[offsetDOF] = localDOF;
    // //  we need to find col_dtoc_dofmap such that: col_gdofmap[ col_dtoc_dofmap[i] ] == values_entities [i];
    // we know that col_gdofmap[0..(nbcols-1)] = global_col_dofs -> in values_entities
    // form first inverse

    col_dtoc_dofmap.resize( values_entities.size() );
    for( int j = 0; j < (int)values_entities.size(); j++ )
    {
        if( colMap.find( values_entities[j] - 1 ) != colMap.end() )
            col_dtoc_dofmap[j] = colMap[values_entities[j] - 1];
        else
        {
            col_dtoc_dofmap[j] = -1;  // signal that this value should not be used in
            // std::cout <<"values_entities[j] -  1: " << values_entities[j] -  1 <<" at index j = " << j <<  " not
            // found in colMap \n";
        }
    }
    return moab::MB_SUCCESS;
}

moab::ErrorCode moab::TempestOnlineMap::set_row_dc_dofs( std::vector< int >& values_entities )
{
    // row_dtoc_dofmap = values_entities; // needs to point to local
    //  we need to find row_dtoc_dofmap such that: row_gdofmap[ row_dtoc_dofmap[i] ] == values_entities [i];

    row_dtoc_dofmap.resize( values_entities.size() );
    for( int j = 0; j < (int)values_entities.size(); j++ )
    {
        if( rowMap.find( values_entities[j] - 1 ) != rowMap.end() )
            row_dtoc_dofmap[j] = rowMap[values_entities[j] - 1];  // values are 1 based, but rowMap, colMap are not
        else
        {
            row_dtoc_dofmap[j] = -1;  // not all values are used
            // std::cout <<"values_entities[j] -  1: " << values_entities[j] -  1 <<" at index j = " << j <<  " not
            // found in rowMap \n";
        }
    }
    return moab::MB_SUCCESS;
}
///////////////////////////////////////////////////////////////////////////////

moab::ErrorCode moab::TempestOnlineMap::GenerateRemappingWeights(
    std::string strInputType, std::string strOutputType, const int nPin, const int nPout, bool fBubble,
    int fMonotoneTypeID, bool fVolumetric, bool fNoConservation, bool fNoCheck, const std::string srcDofTagName,
    const std::string tgtDofTagName, const bool fInputConcave, const bool fOutputConcave )
{
    NcError error( NcError::silent_nonfatal );

    moab::DebugOutput dbgprint( std::cout, rank, 0 );
    dbgprint.set_prefix( "[TempestOnlineMap]: " );
    moab::ErrorCode rval;

    const bool m_bPointCloudSource = ( m_remapper->point_cloud_source );
    const bool m_bPointCloudTarget = ( m_remapper->point_cloud_target );
    const bool m_bPointCloud       = m_bPointCloudSource || m_bPointCloudTarget;

    try
    {
        // Check command line parameters (data type arguments)
        STLStringHelper::ToLower( strInputType );
        STLStringHelper::ToLower( strOutputType );

        DiscretizationType eInputType;
        DiscretizationType eOutputType;
        int fNoCheckGlob = ( fNoCheck ? 1 : 0 );

        if( strInputType == "fv" )
        {
            eInputType = DiscretizationType_FV;
        }
        else if( strInputType == "cgll" )
        {
            eInputType = DiscretizationType_CGLL;
        }
        else if( strInputType == "dgll" )
        {
            eInputType = DiscretizationType_DGLL;
        }
        else if( strInputType == "pcloud" )
        {
            eInputType = DiscretizationType_PCLOUD;
        }
        else
        {
            _EXCEPTION1( "Invalid \"in_type\" value (%s), expected [fv|cgll|dgll]", strInputType.c_str() );
        }

        if( strOutputType == "fv" )
        {
            eOutputType = DiscretizationType_FV;
        }
        else if( strOutputType == "cgll" )
        {
            eOutputType = DiscretizationType_CGLL;
        }
        else if( strOutputType == "dgll" )
        {
            eOutputType = DiscretizationType_DGLL;
        }
        else if( strOutputType == "pcloud" )
        {
            eOutputType = DiscretizationType_PCLOUD;
        }
        else
        {
            _EXCEPTION1( "Invalid \"out_type\" value (%s), expected [fv|cgll|dgll]", strOutputType.c_str() );
        }

        // Monotonicity flags
        int nMonotoneType = fMonotoneTypeID;
        m_bConserved      = !fNoConservation;
        m_iMonotonicity   = fMonotoneTypeID;
        m_eInputType      = eInputType;
        m_eOutputType     = eOutputType;

        m_nDofsPEl_Src =
            ( m_eInputType == DiscretizationType_FV || m_eInputType == DiscretizationType_PCLOUD ? 1 : nPin );
        m_nDofsPEl_Dest =
            ( m_eOutputType == DiscretizationType_FV || m_eOutputType == DiscretizationType_PCLOUD ? 1 : nPout );

        rval = SetDOFmapTags( srcDofTagName, tgtDofTagName );MB_CHK_ERR( rval );

        double dTotalAreaInput = 0.0, dTotalAreaOutput = 0.0;
        if( !m_bPointCloudSource )
        {
            // Calculate Input Mesh Face areas
            if( is_root ) dbgprint.printf( 0, "Calculating input mesh Face areas\n" );
            double dTotalAreaInput_loc = m_meshInput->CalculateFaceAreas( fInputConcave );
            dTotalAreaInput            = dTotalAreaInput_loc;
#ifdef MOAB_HAVE_MPI
            if( m_pcomm )
                MPI_Reduce( &dTotalAreaInput_loc, &dTotalAreaInput, 1, MPI_DOUBLE, MPI_SUM, 0, m_pcomm->comm() );
#endif
            if( is_root ) dbgprint.printf( 0, "Input Mesh Geometric Area: %1.15e\n", dTotalAreaInput );

            // Input mesh areas
            m_meshInputCov->CalculateFaceAreas( fInputConcave );
        }

        if( !m_bPointCloudTarget )
        {
            // Calculate Output Mesh Face areas
            if( is_root ) dbgprint.printf( 0, "Calculating output mesh Face areas\n" );
            double dTotalAreaOutput_loc = m_meshOutput->CalculateFaceAreas( fOutputConcave );
            dTotalAreaOutput            = dTotalAreaOutput_loc;
#ifdef MOAB_HAVE_MPI
            if( m_pcomm )
                MPI_Reduce( &dTotalAreaOutput_loc, &dTotalAreaOutput, 1, MPI_DOUBLE, MPI_SUM, 0, m_pcomm->comm() );
#endif
            if( is_root ) dbgprint.printf( 0, "Output Mesh Geometric Area: %1.15e\n", dTotalAreaOutput );
        }

        if( !m_bPointCloud )
        {
            // Verify that overlap mesh is in the correct order
            int ixSourceFaceMax = ( -1 );
            int ixTargetFaceMax = ( -1 );

            if( m_meshOverlap->vecSourceFaceIx.size() != m_meshOverlap->vecTargetFaceIx.size() )
            {
                _EXCEPTIONT( "Invalid overlap mesh:\n"
                             "    Possible mesh file corruption?" );
            }

            for( unsigned i = 0; i < m_meshOverlap->faces.size(); i++ )
            {
                if( m_meshOverlap->vecSourceFaceIx[i] + 1 > ixSourceFaceMax )
                    ixSourceFaceMax = m_meshOverlap->vecSourceFaceIx[i] + 1;

                if( m_meshOverlap->vecTargetFaceIx[i] + 1 > ixTargetFaceMax )
                    ixTargetFaceMax = m_meshOverlap->vecTargetFaceIx[i] + 1;
            }

            // Check for forward correspondence in overlap mesh
            if( m_meshInput->faces.size() - ixSourceFaceMax == 0 )
            {
                if( is_root ) dbgprint.printf( 0, "Overlap mesh forward correspondence found\n" );
            }
            else if( m_meshOutput->faces.size() - ixSourceFaceMax == 0 )
            {  // Check for reverse correspondence in overlap mesh
                if( is_root ) dbgprint.printf( 0, "Overlap mesh reverse correspondence found (reversing)\n" );

                // Reorder overlap mesh
                m_meshOverlap->ExchangeFirstAndSecondMesh();
            }
            // else
            // {   // No correspondence found
            //     _EXCEPTION4 ( "Invalid overlap mesh:\n"
            //                   "    No correspondence found with input and output meshes (%i,%i)
            //                   vs (%i,%i)", m_meshInputCov->faces.size(),
            //                   m_meshOutput->faces.size(), ixSourceFaceMax, ixTargetFaceMax );
            // }

            // Calculate Face areas
            if( is_root ) dbgprint.printf( 0, "Calculating overlap mesh Face areas\n" );
            double dTotalAreaOverlap_loc = m_meshOverlap->CalculateFaceAreas( false );
            double dTotalAreaOverlap     = dTotalAreaOverlap_loc;
#ifdef MOAB_HAVE_MPI
            if( m_pcomm )
                MPI_Reduce( &dTotalAreaOverlap_loc, &dTotalAreaOverlap, 1, MPI_DOUBLE, MPI_SUM, 0, m_pcomm->comm() );
#endif
            if( is_root ) dbgprint.printf( 0, "Overlap Mesh Area: %1.15e\n", dTotalAreaOverlap );

            // Correct areas to match the areas calculated in the overlap mesh
            // if (fCorrectAreas)
            {
                if( is_root ) dbgprint.printf( 0, "Correcting source/target areas to overlap mesh areas\n" );
                DataArray1D< double > dSourceArea( m_meshInputCov->faces.size() );
                DataArray1D< double > dTargetArea( m_meshOutput->faces.size() );

                assert( m_meshOverlap->vecSourceFaceIx.size() == m_meshOverlap->faces.size() );
                assert( m_meshOverlap->vecTargetFaceIx.size() == m_meshOverlap->faces.size() );
                assert( m_meshOverlap->vecFaceArea.GetRows() == m_meshOverlap->faces.size() );

                assert( m_meshInputCov->vecFaceArea.GetRows() == m_meshInputCov->faces.size() );
                assert( m_meshOutput->vecFaceArea.GetRows() == m_meshOutput->faces.size() );

                for( size_t i = 0; i < m_meshOverlap->faces.size(); i++ )
                {
                    if( m_meshOverlap->vecSourceFaceIx[i] < 0 || m_meshOverlap->vecTargetFaceIx[i] < 0 )
                        continue;  // skip this cell since it is ghosted

                    // let us recompute the source/target areas based on overlap mesh areas
                    assert( static_cast< size_t >( m_meshOverlap->vecSourceFaceIx[i] ) < m_meshInputCov->faces.size() );
                    dSourceArea[m_meshOverlap->vecSourceFaceIx[i]] += m_meshOverlap->vecFaceArea[i];
                    assert( static_cast< size_t >( m_meshOverlap->vecTargetFaceIx[i] ) < m_meshOutput->faces.size() );
                    dTargetArea[m_meshOverlap->vecTargetFaceIx[i]] += m_meshOverlap->vecFaceArea[i];
                }

                for( size_t i = 0; i < m_meshInputCov->faces.size(); i++ )
                {
                    if( fabs( dSourceArea[i] - m_meshInputCov->vecFaceArea[i] ) < 1.0e-10 )
                    {
                        m_meshInputCov->vecFaceArea[i] = dSourceArea[i];
                    }
                }
                for( size_t i = 0; i < m_meshOutput->faces.size(); i++ )
                {
                    if( fabs( dTargetArea[i] - m_meshOutput->vecFaceArea[i] ) < 1.0e-10 )
                    {
                        m_meshOutput->vecFaceArea[i] = dTargetArea[i];
                    }
                }
            }

            // Set source mesh areas in map
            if( !m_bPointCloudSource && eInputType == DiscretizationType_FV )
            {
                this->SetSourceAreas( m_meshInputCov->vecFaceArea );
                if( m_meshInputCov->vecMask.IsAttached() )
                {
                    this->SetSourceMask( m_meshInputCov->vecMask );
                }
            }

            // Set target mesh areas in map
            if( !m_bPointCloudTarget && eOutputType == DiscretizationType_FV )
            {
                this->SetTargetAreas( m_meshOutput->vecFaceArea );
                if( m_meshOutput->vecMask.IsAttached() )
                {
                    this->SetTargetMask( m_meshOutput->vecMask );
                }
            }

            // Partial cover
            if( fabs( dTotalAreaOutput - dTotalAreaInput ) > 1.0e-10 &&
                fabs( dTotalAreaOverlap - dTotalAreaInput ) > 1.0e-10 )
            {
                if( !fNoCheckGlob )
                {
                    dbgprint.printf( rank, "WARNING: Significant mismatch between overlap mesh area "
                                           "and input mesh area.\n  Automatically enabling --nocheck\n" );
                    fNoCheckGlob = 1;
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
        }

        // Finite volume input / Finite volume output
        if( ( eInputType == DiscretizationType_FV ) && ( eOutputType == DiscretizationType_FV ) )
        {
            // Generate reverse node array and edge map
            if( m_meshInputCov->revnodearray.size() == 0 ) m_meshInputCov->ConstructReverseNodeArray();
            if( m_meshInputCov->edgemap.size() == 0 ) m_meshInputCov->ConstructEdgeMap( false );

            // Initialize coordinates for map
            this->InitializeSourceCoordinatesFromMeshFV( *m_meshInputCov );
            this->InitializeTargetCoordinatesFromMeshFV( *m_meshOutput );

            // Finite volume input / Finite element output
            rval = this->SetDOFmapAssociation( eInputType, false, NULL, NULL, eOutputType, false, NULL );MB_CHK_ERR( rval );

            // Construct remap
            if( is_root ) dbgprint.printf( 0, "Calculating remap weights\n" );
            LinearRemapFVtoFV_Tempest_MOAB( nPin );
        }
        else if( eInputType == DiscretizationType_FV )
        {
            DataArray3D< double > dataGLLJacobian;

            if( is_root ) dbgprint.printf( 0, "Generating output mesh meta data\n" );
            double dNumericalArea_loc =
                GenerateMetaData( *m_meshOutput, nPout, fBubble, dataGLLNodesDest, dataGLLJacobian );

            double dNumericalArea = dNumericalArea_loc;
#ifdef MOAB_HAVE_MPI
            if( m_pcomm )
                MPI_Allreduce( &dNumericalArea_loc, &dNumericalArea, 1, MPI_DOUBLE, MPI_SUM, m_pcomm->comm() );
#endif
            if( is_root ) dbgprint.printf( 0, "Output Mesh Numerical Area: %1.15e\n", dNumericalArea );

            // Initialize coordinates for map
            this->InitializeSourceCoordinatesFromMeshFV( *m_meshInputCov );
            this->InitializeTargetCoordinatesFromMeshFE( *m_meshOutput, nPout, dataGLLNodesDest );

            // Generate the continuous Jacobian
            bool fContinuous = ( eOutputType == DiscretizationType_CGLL );

            if( eOutputType == DiscretizationType_CGLL )
            {
                GenerateUniqueJacobian( dataGLLNodesDest, dataGLLJacobian, this->GetTargetAreas() );
            }
            else
            {
                GenerateDiscontinuousJacobian( dataGLLJacobian, this->GetTargetAreas() );
            }

            // Generate reverse node array and edge map
            if( m_meshInputCov->revnodearray.size() == 0 ) m_meshInputCov->ConstructReverseNodeArray();
            if( m_meshInputCov->edgemap.size() == 0 ) m_meshInputCov->ConstructEdgeMap( false );

            // Finite volume input / Finite element output
            rval = this->SetDOFmapAssociation( eInputType, false, NULL, NULL, eOutputType,
                                               ( eOutputType == DiscretizationType_CGLL ), &dataGLLNodesDest );MB_CHK_ERR( rval );

            // Generate remap weights
            if( is_root ) dbgprint.printf( 0, "Calculating remap weights\n" );

            if( fVolumetric )
            {
                LinearRemapFVtoGLL_Volumetric( *m_meshInputCov, *m_meshOutput, *m_meshOverlap, dataGLLNodesDest,
                                               dataGLLJacobian, this->GetTargetAreas(), nPin, *this, nMonotoneType,
                                               fContinuous, fNoConservation );
            }
            else
            {
                LinearRemapFVtoGLL( *m_meshInputCov, *m_meshOutput, *m_meshOverlap, dataGLLNodesDest, dataGLLJacobian,
                                    this->GetTargetAreas(), nPin, *this, nMonotoneType, fContinuous, fNoConservation );
            }
        }
        else if( ( eInputType == DiscretizationType_PCLOUD ) || ( eOutputType == DiscretizationType_PCLOUD ) )
        {
            DataArray3D< double > dataGLLJacobian;
            if( !m_bPointCloudSource )
            {
                // Generate reverse node array and edge map
                if( m_meshInputCov->revnodearray.size() == 0 ) m_meshInputCov->ConstructReverseNodeArray();
                if( m_meshInputCov->edgemap.size() == 0 ) m_meshInputCov->ConstructEdgeMap( false );

                // Initialize coordinates for map
                if( eInputType == DiscretizationType_FV )
                {
                    this->InitializeSourceCoordinatesFromMeshFV( *m_meshInputCov );
                }
                else
                {
                    if( is_root ) dbgprint.printf( 0, "Generating input mesh meta data\n" );
                    DataArray3D< double > dataGLLJacobianSrc;
                    GenerateMetaData( *m_meshInputCov, nPin, fBubble, dataGLLNodesSrcCov, dataGLLJacobian );
                    GenerateMetaData( *m_meshInput, nPin, fBubble, dataGLLNodesSrc, dataGLLJacobianSrc );
                }
            }
            // else { /* Source is a point cloud dataset */ }

            if( !m_bPointCloudTarget )
            {
                // Generate reverse node array and edge map
                if( m_meshOutput->revnodearray.size() == 0 ) m_meshOutput->ConstructReverseNodeArray();
                if( m_meshOutput->edgemap.size() == 0 ) m_meshOutput->ConstructEdgeMap( false );

                // Initialize coordinates for map
                if( eOutputType == DiscretizationType_FV )
                {
                    this->InitializeSourceCoordinatesFromMeshFV( *m_meshOutput );
                }
                else
                {
                    if( is_root ) dbgprint.printf( 0, "Generating output mesh meta data\n" );
                    GenerateMetaData( *m_meshOutput, nPout, fBubble, dataGLLNodesDest, dataGLLJacobian );
                }
            }
            // else { /* Target is a point cloud dataset */ }

            // Finite volume input / Finite element output
            rval = this->SetDOFmapAssociation(
                eInputType, ( eInputType == DiscretizationType_CGLL ),
                ( m_bPointCloudSource || eInputType == DiscretizationType_FV ? NULL : &dataGLLNodesSrcCov ),
                ( m_bPointCloudSource || eInputType == DiscretizationType_FV ? NULL : &dataGLLNodesSrc ), eOutputType,
                ( eOutputType == DiscretizationType_CGLL ), ( m_bPointCloudTarget ? NULL : &dataGLLNodesDest ) );MB_CHK_ERR( rval );

            // Construct remap
            if( is_root ) dbgprint.printf( 0, "Calculating remap weights with Nearest-Neighbor method\n" );
            rval = LinearRemapNN_MOAB( true /*use_GID_matching*/, false /*strict_check*/ );MB_CHK_ERR( rval );
        }
        else if( ( eInputType != DiscretizationType_FV ) && ( eOutputType == DiscretizationType_FV ) )
        {
            DataArray3D< double > dataGLLJacobianSrc, dataGLLJacobian;

            if( is_root ) dbgprint.printf( 0, "Generating input mesh meta data\n" );
            // double dNumericalAreaCov_loc =
            GenerateMetaData( *m_meshInputCov, nPin, fBubble, dataGLLNodesSrcCov, dataGLLJacobian );

            double dNumericalArea_loc =
                GenerateMetaData( *m_meshInput, nPin, fBubble, dataGLLNodesSrc, dataGLLJacobianSrc );

            // if ( is_root ) dbgprint.printf ( 0, "Input Mesh: Coverage Area: %1.15e, Output Area:
            // %1.15e\n", dNumericalAreaCov_loc, dTotalAreaOutput_loc );
            // assert(dNumericalAreaCov_loc >= dTotalAreaOutput_loc);

            double dNumericalArea = dNumericalArea_loc;
#ifdef MOAB_HAVE_MPI
            if( m_pcomm )
                MPI_Allreduce( &dNumericalArea_loc, &dNumericalArea, 1, MPI_DOUBLE, MPI_SUM, m_pcomm->comm() );
#endif
            if( is_root )
            {
                dbgprint.printf( 0, "Input Mesh Numerical Area: %1.15e\n", dNumericalArea );

                if( fabs( dNumericalArea - dTotalAreaInput ) > 1.0e-12 )
                {
                    dbgprint.printf( 0, "WARNING: Significant mismatch between input mesh "
                                        "numerical area and geometric area\n" );
                }
            }

            if( dataGLLNodesSrcCov.GetSubColumns() != m_meshInputCov->faces.size() )
            {
                _EXCEPTIONT( "Number of element does not match between metadata and "
                             "input mesh" );
            }

            // Initialize coordinates for map
            this->InitializeSourceCoordinatesFromMeshFE( *m_meshInputCov, nPin, dataGLLNodesSrcCov );
            this->InitializeTargetCoordinatesFromMeshFV( *m_meshOutput );

            // Generate the continuous Jacobian for input mesh
            bool fContinuousIn = ( eInputType == DiscretizationType_CGLL );

            if( eInputType == DiscretizationType_CGLL )
            {
                GenerateUniqueJacobian( dataGLLNodesSrcCov, dataGLLJacobian, this->GetSourceAreas() );
            }
            else
            {
                GenerateDiscontinuousJacobian( dataGLLJacobian, this->GetSourceAreas() );
            }

            // Finite element input / Finite volume output
            rval = this->SetDOFmapAssociation( eInputType, ( eInputType == DiscretizationType_CGLL ),
                                               &dataGLLNodesSrcCov, &dataGLLNodesSrc, eOutputType, false, NULL );MB_CHK_ERR( rval );

            // Generate remap
            if( is_root ) dbgprint.printf( 0, "Calculating remap weights\n" );

            if( fVolumetric )
            {
                _EXCEPTIONT( "Unimplemented: Volumetric currently unavailable for"
                             "GLL input mesh" );
            }

            LinearRemapSE4_Tempest_MOAB( dataGLLNodesSrcCov, dataGLLJacobian, nMonotoneType, fContinuousIn,
                                         fNoConservation );
        }
        else if( ( eInputType != DiscretizationType_FV ) && ( eOutputType != DiscretizationType_FV ) )
        {
            DataArray3D< double > dataGLLJacobianIn, dataGLLJacobianSrc;
            DataArray3D< double > dataGLLJacobianOut;

            // Input metadata
            if( is_root ) dbgprint.printf( 0, "Generating input mesh meta data\n" );
            double dNumericalAreaIn_loc =
                GenerateMetaData( *m_meshInputCov, nPin, fBubble, dataGLLNodesSrcCov, dataGLLJacobianIn );

            double dNumericalAreaSrc_loc =
                GenerateMetaData( *m_meshInput, nPin, fBubble, dataGLLNodesSrc, dataGLLJacobianSrc );

            assert( dNumericalAreaIn_loc >= dNumericalAreaSrc_loc );

            double dNumericalAreaIn = dNumericalAreaSrc_loc;
#ifdef MOAB_HAVE_MPI
            if( m_pcomm )
                MPI_Allreduce( &dNumericalAreaSrc_loc, &dNumericalAreaIn, 1, MPI_DOUBLE, MPI_SUM, m_pcomm->comm() );
#endif
            if( is_root ) dbgprint.printf( 0, "Input Mesh Numerical Area: %1.15e\n", dNumericalAreaIn );

            if( fabs( dNumericalAreaIn - dTotalAreaInput ) > 1.0e-12 )
            {
                dbgprint.printf( 0, "WARNING: Significant mismatch between input mesh "
                                    "numerical area and geometric area\n" );
            }

            // Output metadata
            if( is_root ) dbgprint.printf( 0, "Generating output mesh meta data\n" );
            double dNumericalAreaOut_loc =
                GenerateMetaData( *m_meshOutput, nPout, fBubble, dataGLLNodesDest, dataGLLJacobianOut );

            double dNumericalAreaOut = dNumericalAreaOut_loc;
#ifdef MOAB_HAVE_MPI
            if( m_pcomm )
                MPI_Allreduce( &dNumericalAreaOut_loc, &dNumericalAreaOut, 1, MPI_DOUBLE, MPI_SUM, m_pcomm->comm() );
#endif
            if( is_root ) dbgprint.printf( 0, "Output Mesh Numerical Area: %1.15e\n", dNumericalAreaOut );

            if( fabs( dNumericalAreaOut - dTotalAreaOutput ) > 1.0e-12 )
            {
                if( is_root )
                    dbgprint.printf( 0, "WARNING: Significant mismatch between output mesh "
                                        "numerical area and geometric area\n" );
            }

            // Initialize coordinates for map
            this->InitializeSourceCoordinatesFromMeshFE( *m_meshInputCov, nPin, dataGLLNodesSrcCov );
            this->InitializeTargetCoordinatesFromMeshFE( *m_meshOutput, nPout, dataGLLNodesDest );

            // Generate the continuous Jacobian for input mesh
            bool fContinuousIn = ( eInputType == DiscretizationType_CGLL );

            if( eInputType == DiscretizationType_CGLL )
            {
                GenerateUniqueJacobian( dataGLLNodesSrcCov, dataGLLJacobianIn, this->GetSourceAreas() );
            }
            else
            {
                GenerateDiscontinuousJacobian( dataGLLJacobianIn, this->GetSourceAreas() );
            }

            // Generate the continuous Jacobian for output mesh
            bool fContinuousOut = ( eOutputType == DiscretizationType_CGLL );

            if( eOutputType == DiscretizationType_CGLL )
            {
                GenerateUniqueJacobian( dataGLLNodesDest, dataGLLJacobianOut, this->GetTargetAreas() );
            }
            else
            {
                GenerateDiscontinuousJacobian( dataGLLJacobianOut, this->GetTargetAreas() );
            }

            // Input Finite Element to Output Finite Element
            rval = this->SetDOFmapAssociation( eInputType, ( eInputType == DiscretizationType_CGLL ),
                                               &dataGLLNodesSrcCov, &dataGLLNodesSrc, eOutputType,
                                               ( eOutputType == DiscretizationType_CGLL ), &dataGLLNodesDest );MB_CHK_ERR( rval );

            // Generate remap
            if( is_root ) dbgprint.printf( 0, "Calculating remap weights\n" );

            LinearRemapGLLtoGLL2_MOAB( dataGLLNodesSrcCov, dataGLLJacobianIn, dataGLLNodesDest, dataGLLJacobianOut,
                                       this->GetTargetAreas(), nPin, nPout, nMonotoneType, fContinuousIn,
                                       fContinuousOut, fNoConservation );
        }
        else
        {
            _EXCEPTIONT( "Not implemented" );
        }

#ifdef MOAB_HAVE_EIGEN3
        copy_tempest_sparsemat_to_eigen3();
#endif

        int fNoCheckLoc = fNoCheckGlob;
#ifdef MOAB_HAVE_MPI
        {
            // Remove ghosted entities from overlap set
            moab::Range ghostedEnts;
            rval = m_remapper->GetOverlapAugmentedEntities( ghostedEnts );MB_CHK_ERR( rval );
            moab::EntityHandle m_meshOverlapSet = m_remapper->GetMeshSet( moab::Remapper::OverlapMesh );
            rval                                = m_interface->remove_entities( m_meshOverlapSet, ghostedEnts );MB_CHK_SET_ERR( rval, "Deleting ghosted entities failed" );
        }

        // Let us see if we need to perform consistency/conservation checks
        if( m_pcomm ) MPI_Allreduce( &fNoCheckLoc, &fNoCheckGlob, 1, MPI_INT, MPI_MAX, m_pcomm->comm() );
#endif
        // Verify consistency, conservation and monotonicity, globally
        if( !fNoCheckLoc )
        {
            if( is_root ) dbgprint.printf( 0, "Verifying map" );
            this->IsConsistent( 1.0e-8 );
            if( !fNoConservation ) this->IsConservative( 1.0e-8 );

            if( nMonotoneType != 0 )
            {
                this->IsMonotone( 1.0e-12 );
            }
        }
    }
    catch( Exception& e )
    {
        dbgprint.printf( 0, "%s", e.ToString().c_str() );
        return ( moab::MB_FAILURE );
    }
    catch( ... )
    {
        return ( moab::MB_FAILURE );
    }
    return moab::MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////

int moab::TempestOnlineMap::IsConsistent( double dTolerance )
{
#ifndef MOAB_HAVE_MPI

    return OfflineMap::IsConsistent( dTolerance );

#else

    // Get map entries
    DataArray1D< int > dataRows;
    DataArray1D< int > dataCols;
    DataArray1D< double > dataEntries;

    // Calculate row sums
    DataArray1D< double > dRowSums;
    m_mapRemap.GetEntries( dataRows, dataCols, dataEntries );
    dRowSums.Allocate( m_mapRemap.GetRows() );

    for( unsigned i = 0; i < dataRows.GetRows(); i++ )
    {
        dRowSums[dataRows[i]] += dataEntries[i];
    }

    // Verify all row sums are equal to 1
    int fConsistent = 0;
    for( unsigned i = 0; i < dRowSums.GetRows(); i++ )
    {
        if( fabs( dRowSums[i] - 1.0 ) > dTolerance )
        {
            fConsistent++;
            int rowGID = row_gdofmap[i];
            Announce( "TempestOnlineMap is not consistent in row %i (%1.15e)", rowGID, dRowSums[i] );
        }
    }

    int ierr;
    int fConsistentGlobal = 0;
    ierr = MPI_Allreduce( &fConsistent, &fConsistentGlobal, 1, MPI_INT, MPI_SUM, m_pcomm->comm() );
    if( ierr != MPI_SUCCESS ) return -1;

    return fConsistentGlobal;
#endif
}

///////////////////////////////////////////////////////////////////////////////

int moab::TempestOnlineMap::IsConservative( double dTolerance )
{
#ifndef MOAB_HAVE_MPI

    return OfflineMap::IsConservative( dTolerance );

#else
    // return OfflineMap::IsConservative(dTolerance);

    int ierr;
    // Get map entries
    DataArray1D< int > dataRows;
    DataArray1D< int > dataCols;
    DataArray1D< double > dataEntries;
    const DataArray1D< double >& dTargetAreas = this->GetTargetAreas();
    const DataArray1D< double >& dSourceAreas = this->GetSourceAreas();

    // Calculate column sums
    std::vector< int > dColumnsUnique;
    std::vector< double > dColumnSums;

    int nColumns = m_mapRemap.GetColumns();
    m_mapRemap.GetEntries( dataRows, dataCols, dataEntries );
    dColumnSums.resize( m_nTotDofs_SrcCov, 0.0 );
    dColumnsUnique.resize( m_nTotDofs_SrcCov, -1 );

    for( unsigned i = 0; i < dataEntries.GetRows(); i++ )
    {
        dColumnSums[dataCols[i]] += dataEntries[i] * dTargetAreas[dataRows[i]] / dSourceAreas[dataCols[i]];

        assert( dataCols[i] < m_nTotDofs_SrcCov );

        // GID for column DoFs: col_gdofmap[ col_ldofmap [ dataCols[i] ] ]
        int colGID = this->GetColGlobalDoF( dataCols[i] );  // col_gdofmap[ col_ldofmap [ dataCols[i] ] ];
        // int colGID = col_gdofmap[ col_ldofmap [ dataCols[i] ] ];
        dColumnsUnique[dataCols[i]] = colGID;

        // std::cout << "Column dataCols[i]=" << dataCols[i] << " with GID = " << colGID <<
        // std::endl;
    }

    int rootProc = 0;
    std::vector< int > nElementsInProc;
    const int nDATA = 3;
    if( !rank ) nElementsInProc.resize( size * nDATA );
    int senddata[nDATA] = { nColumns, m_nTotDofs_SrcCov, m_nTotDofs_Src };
    ierr = MPI_Gather( senddata, nDATA, MPI_INT, nElementsInProc.data(), nDATA, MPI_INT, rootProc, m_pcomm->comm() );
    if( ierr != MPI_SUCCESS ) return -1;

    int nTotVals = 0, nTotColumns = 0, nTotColumnsUnq = 0;
    std::vector< int > dColumnIndices;
    std::vector< double > dColumnSourceAreas;
    std::vector< double > dColumnSumsTotal;
    std::vector< int > displs, rcount;
    if( rank == rootProc )
    {
        displs.resize( size + 1, 0 );
        rcount.resize( size, 0 );
        int gsum = 0;
        for( int ir = 0; ir < size; ++ir )
        {
            nTotVals += nElementsInProc[ir * nDATA];
            nTotColumns += nElementsInProc[ir * nDATA + 1];
            nTotColumnsUnq += nElementsInProc[ir * nDATA + 2];

            displs[ir] = gsum;
            rcount[ir] = nElementsInProc[ir * nDATA + 1];
            gsum += rcount[ir];

            // printf("%d: nTotColumns: %d, Displs: %d, rcount: %d, gsum = %d\n", ir, nTotColumns,
            // displs[ir], rcount[ir], gsum);
        }

        dColumnIndices.resize( nTotColumns, -1 );
        dColumnSumsTotal.resize( nTotColumns, 0.0 );
        // dColumnSourceAreas.resize ( nTotColumns, 0.0 );
    }

    // Gather all ColumnSums to root process and accumulate
    // We expect that the sums of all columns equate to 1.0 within user specified tolerance
    // Need to do a gatherv here since different processes have different number of elements
    // MPI_Reduce(&dColumnSums[0], &dColumnSumsTotal[0], m_mapRemap.GetColumns(), MPI_DOUBLE,
    // MPI_SUM, 0, m_pcomm->comm());
    ierr = MPI_Gatherv( &dColumnsUnique[0], m_nTotDofs_SrcCov, MPI_INT, &dColumnIndices[0], rcount.data(),
                        displs.data(), MPI_INT, rootProc, m_pcomm->comm() );
    if( ierr != MPI_SUCCESS ) return -1;
    ierr = MPI_Gatherv( &dColumnSums[0], m_nTotDofs_SrcCov, MPI_DOUBLE, &dColumnSumsTotal[0], rcount.data(),
                        displs.data(), MPI_DOUBLE, rootProc, m_pcomm->comm() );
    if( ierr != MPI_SUCCESS ) return -1;
    // ierr = MPI_Gatherv ( &dSourceAreas[0], m_nTotDofs_SrcCov, MPI_DOUBLE, &dColumnSourceAreas[0],
    // rcount.data(), displs.data(), MPI_DOUBLE, rootProc, m_pcomm->comm() ); if ( ierr !=
    // MPI_SUCCESS ) return -1;

    // Clean out unwanted arrays now
    dColumnSums.clear();
    dColumnsUnique.clear();

    // Verify all column sums equal the input Jacobian
    int fConservative = 0;
    if( rank == rootProc )
    {
        displs[size] = ( nTotColumns );
        // std::vector<double> dColumnSumsOnRoot(nTotColumnsUnq, 0.0);
        std::map< int, double > dColumnSumsOnRoot;
        // std::map<int, double> dColumnSourceAreasOnRoot;
        for( int ir = 0; ir < size; ir++ )
        {
            for( int ips = displs[ir]; ips < displs[ir + 1]; ips++ )
            {
                if( dColumnIndices[ips] < 0 ) continue;
                // printf("%d, %d: dColumnIndices[ips]: %d\n", ir, ips, dColumnIndices[ips]);
                assert( dColumnIndices[ips] < nTotColumnsUnq );
                dColumnSumsOnRoot[dColumnIndices[ips]] += dColumnSumsTotal[ips];  // / dColumnSourceAreas[ips];
                // dColumnSourceAreasOnRoot[ dColumnIndices[ips] ] = dColumnSourceAreas[ips];
                // dColumnSourceAreas[ dColumnIndices[ips] ]
            }
        }

        for( std::map< int, double >::iterator it = dColumnSumsOnRoot.begin(); it != dColumnSumsOnRoot.end(); ++it )
        {
            // if ( fabs ( it->second - dColumnSourceAreasOnRoot[it->first] ) > dTolerance )
            if( fabs( it->second - 1.0 ) > dTolerance )
            {
                fConservative++;
                Announce( "TempestOnlineMap is not conservative in column "
                          // "%i (%1.15e)", it->first, it->second );
                          "%i (%1.15e)",
                          it->first, it->second /* / dColumnSourceAreasOnRoot[it->first] */ );
            }
        }
    }

    // TODO: Just do a broadcast from root instead of a reduction
    ierr = MPI_Bcast( &fConservative, 1, MPI_INT, rootProc, m_pcomm->comm() );
    if( ierr != MPI_SUCCESS ) return -1;

    return fConservative;
#endif
}

///////////////////////////////////////////////////////////////////////////////

int moab::TempestOnlineMap::IsMonotone( double dTolerance )
{
#ifndef MOAB_HAVE_MPI

    return OfflineMap::IsMonotone( dTolerance );

#else

    // Get map entries
    DataArray1D< int > dataRows;
    DataArray1D< int > dataCols;
    DataArray1D< double > dataEntries;

    m_mapRemap.GetEntries( dataRows, dataCols, dataEntries );

    // Verify all entries are in the range [0,1]
    int fMonotone = 0;
    for( unsigned i = 0; i < dataRows.GetRows(); i++ )
    {
        if( ( dataEntries[i] < -dTolerance ) || ( dataEntries[i] > 1.0 + dTolerance ) )
        {
            fMonotone++;

            Announce( "TempestOnlineMap is not monotone in entry (%i): %1.15e", i, dataEntries[i] );
        }
    }

    int ierr;
    int fMonotoneGlobal = 0;
    ierr = MPI_Allreduce( &fMonotone, &fMonotoneGlobal, 1, MPI_INT, MPI_SUM, m_pcomm->comm() );
    if( ierr != MPI_SUCCESS ) return -1;

    return fMonotoneGlobal;
#endif
}

///////////////////////////////////////////////////////////////////////////////

moab::ErrorCode moab::TempestOnlineMap::ApplyWeights( moab::Tag srcSolutionTag, moab::Tag tgtSolutionTag,
                                                      bool transpose )
{
    moab::ErrorCode rval;

    std::vector< double > solSTagVals;
    std::vector< double > solTTagVals;

    moab::Range sents, tents;
    if( m_remapper->point_cloud_source || m_remapper->point_cloud_target )
    {
        if( m_remapper->point_cloud_source )
        {
            moab::Range& covSrcEnts = m_remapper->GetMeshVertices( moab::Remapper::CoveringMesh );
            solSTagVals.resize( covSrcEnts.size(), -1.0 );
            sents = covSrcEnts;
        }
        else
        {
            moab::Range& covSrcEnts = m_remapper->GetMeshEntities( moab::Remapper::CoveringMesh );
            solSTagVals.resize( covSrcEnts.size() * this->GetSourceNDofsPerElement() * this->GetSourceNDofsPerElement(),
                                -1.0 );
            sents = covSrcEnts;
        }
        if( m_remapper->point_cloud_target )
        {
            moab::Range& tgtEnts = m_remapper->GetMeshVertices( moab::Remapper::TargetMesh );
            solTTagVals.resize( tgtEnts.size(), -1.0 );
            tents = tgtEnts;
        }
        else
        {
            moab::Range& tgtEnts = m_remapper->GetMeshEntities( moab::Remapper::TargetMesh );
            solTTagVals.resize(
                tgtEnts.size() * this->GetDestinationNDofsPerElement() * this->GetDestinationNDofsPerElement(), -1.0 );
            tents = tgtEnts;
        }
    }
    else
    {
        moab::Range& covSrcEnts = m_remapper->GetMeshEntities( moab::Remapper::CoveringMesh );
        moab::Range& tgtEnts    = m_remapper->GetMeshEntities( moab::Remapper::TargetMesh );
        solSTagVals.resize( covSrcEnts.size() * this->GetSourceNDofsPerElement() * this->GetSourceNDofsPerElement(),
                            -1.0 );
        solTTagVals.resize(
            tgtEnts.size() * this->GetDestinationNDofsPerElement() * this->GetDestinationNDofsPerElement(), -1.0 );

        sents = covSrcEnts;
        tents = tgtEnts;
    }

    // The tag data is np*np*n_el_src
    rval = m_interface->tag_get_data( srcSolutionTag, sents, &solSTagVals[0] );MB_CHK_SET_ERR( rval, "Getting local tag data failed" );

    // Compute the application of weights on the suorce solution data and store it in the
    // destination solution vector data Optionally, can also perform the transpose application of
    // the weight matrix. Set the 3rd argument to true if this is needed
    rval = this->ApplyWeights( solSTagVals, solTTagVals, transpose );MB_CHK_SET_ERR( rval, "Applying remap operator onto source vector data failed" );

    // The tag data is np*np*n_el_dest
    rval = m_interface->tag_set_data( tgtSolutionTag, tents, &solTTagVals[0] );MB_CHK_SET_ERR( rval, "Setting local tag data failed" );

    return moab::MB_SUCCESS;
}

moab::ErrorCode moab::TempestOnlineMap::DefineAnalyticalSolution( moab::Tag& solnTag, const std::string& solnName,
                                                                  moab::Remapper::IntersectionContext ctx,
                                                                  sample_function testFunction,
                                                                  moab::Tag* clonedSolnTag, std::string cloneSolnName )
{
    moab::ErrorCode rval;
    const bool outputEnabled = ( is_root );
    int discOrder;
    DiscretizationType discMethod;
    moab::EntityHandle meshset;
    moab::Range entities;
    Mesh* trmesh;
    switch( ctx )
    {
        case Remapper::SourceMesh:
            meshset    = m_remapper->m_covering_source_set;
            trmesh     = m_remapper->m_covering_source;
            entities   = ( m_remapper->point_cloud_source ? m_remapper->m_covering_source_vertices
                                                        : m_remapper->m_covering_source_entities );
            discOrder  = m_nDofsPEl_Src;
            discMethod = m_eInputType;
            break;

        case Remapper::TargetMesh:
            meshset = m_remapper->m_target_set;
            trmesh  = m_remapper->m_target;
            entities =
                ( m_remapper->point_cloud_target ? m_remapper->m_target_vertices : m_remapper->m_target_entities );
            discOrder  = m_nDofsPEl_Dest;
            discMethod = m_eOutputType;
            break;

        default:
            if( outputEnabled )
                std::cout << "Invalid context specified for defining an analytical solution tag" << std::endl;
            return moab::MB_FAILURE;
    }

    // Let us create teh solution tag with appropriate information for name, discretization order
    // (DoF space)
    rval = m_interface->tag_get_handle( solnName.c_str(), discOrder * discOrder, MB_TYPE_DOUBLE, solnTag,
                                        MB_TAG_DENSE | MB_TAG_CREAT );MB_CHK_ERR( rval );
    if( clonedSolnTag != NULL )
    {
        if( cloneSolnName.size() == 0 )
        {
            cloneSolnName = solnName + std::string( "Cloned" );
        }
        rval = m_interface->tag_get_handle( cloneSolnName.c_str(), discOrder * discOrder, MB_TYPE_DOUBLE,
                                            *clonedSolnTag, MB_TAG_DENSE | MB_TAG_CREAT );MB_CHK_ERR( rval );
    }

    // Triangular quadrature rule
    const int TriQuadratureOrder = 10;

    if( outputEnabled ) std::cout << "Using triangular quadrature of order " << TriQuadratureOrder << std::endl;

    TriangularQuadratureRule triquadrule( TriQuadratureOrder );

    const int TriQuadraturePoints = triquadrule.GetPoints();

    const DataArray2D< double >& TriQuadratureG = triquadrule.GetG();
    const DataArray1D< double >& TriQuadratureW = triquadrule.GetW();

    // Output data
    DataArray1D< double > dVar;
    DataArray1D< double > dVarMB;  // re-arranged local MOAB vector

    // Nodal geometric area
    DataArray1D< double > dNodeArea;

    // Calculate element areas
    // trmesh->CalculateFaceAreas(fContainsConcaveFaces);

    if( discMethod == DiscretizationType_CGLL || discMethod == DiscretizationType_DGLL )
    {
        /* Get the spectral points and sample the functionals accordingly */
        const bool fGLL          = true;
        const bool fGLLIntegrate = false;

        // Generate grid metadata
        DataArray3D< int > dataGLLNodes;
        DataArray3D< double > dataGLLJacobian;

        GenerateMetaData( *trmesh, discOrder, false, dataGLLNodes, dataGLLJacobian );

        // Number of elements
        int nElements = trmesh->faces.size();

        // Verify all elements are quadrilaterals
        for( int k = 0; k < nElements; k++ )
        {
            const Face& face = trmesh->faces[k];

            if( face.edges.size() != 4 )
            {
                _EXCEPTIONT( "Non-quadrilateral face detected; "
                             "incompatible with --gll" );
            }
        }

        // Number of unique nodes
        int iMaxNode = 0;
        for( int i = 0; i < discOrder; i++ )
        {
            for( int j = 0; j < discOrder; j++ )
            {
                for( int k = 0; k < nElements; k++ )
                {
                    if( dataGLLNodes[i][j][k] > iMaxNode )
                    {
                        iMaxNode = dataGLLNodes[i][j][k];
                    }
                }
            }
        }

        // Get Gauss-Lobatto quadrature nodes
        DataArray1D< double > dG;
        DataArray1D< double > dW;

        GaussLobattoQuadrature::GetPoints( discOrder, 0.0, 1.0, dG, dW );

        // Get Gauss quadrature nodes
        const int nGaussP = 10;

        DataArray1D< double > dGaussG;
        DataArray1D< double > dGaussW;

        GaussQuadrature::GetPoints( nGaussP, 0.0, 1.0, dGaussG, dGaussW );

        // Allocate data
        dVar.Allocate( iMaxNode );
        dVarMB.Allocate( discOrder * discOrder * nElements );
        dNodeArea.Allocate( iMaxNode );

        // Sample data
        for( int k = 0; k < nElements; k++ )
        {
            const Face& face = trmesh->faces[k];

            // Sample data at GLL nodes
            if( fGLL )
            {
                for( int i = 0; i < discOrder; i++ )
                {
                    for( int j = 0; j < discOrder; j++ )
                    {

                        // Apply local map
                        Node node;
                        Node dDx1G;
                        Node dDx2G;

                        ApplyLocalMap( face, trmesh->nodes, dG[i], dG[j], node, dDx1G, dDx2G );

                        // Sample data at this point
                        double dNodeLon = atan2( node.y, node.x );
                        if( dNodeLon < 0.0 )
                        {
                            dNodeLon += 2.0 * M_PI;
                        }
                        double dNodeLat = asin( node.z );

                        double dSample = ( *testFunction )( dNodeLon, dNodeLat );

                        dVar[dataGLLNodes[j][i][k] - 1] = dSample;
                    }
                }
                // High-order Gaussian integration over basis function
            }
            else
            {
                DataArray2D< double > dCoeff( discOrder, discOrder );

                for( int p = 0; p < nGaussP; p++ )
                {
                    for( int q = 0; q < nGaussP; q++ )
                    {

                        // Apply local map
                        Node node;
                        Node dDx1G;
                        Node dDx2G;

                        ApplyLocalMap( face, trmesh->nodes, dGaussG[p], dGaussG[q], node, dDx1G, dDx2G );

                        // Cross product gives local Jacobian
                        Node nodeCross = CrossProduct( dDx1G, dDx2G );

                        double dJacobian =
                            sqrt( nodeCross.x * nodeCross.x + nodeCross.y * nodeCross.y + nodeCross.z * nodeCross.z );

                        // Find components of quadrature point in basis
                        // of the first Face
                        SampleGLLFiniteElement( 0, discOrder, dGaussG[p], dGaussG[q], dCoeff );

                        // Sample data at this point
                        double dNodeLon = atan2( node.y, node.x );
                        if( dNodeLon < 0.0 )
                        {
                            dNodeLon += 2.0 * M_PI;
                        }
                        double dNodeLat = asin( node.z );

                        double dSample = ( *testFunction )( dNodeLon, dNodeLat );

                        // Integrate
                        for( int i = 0; i < discOrder; i++ )
                        {
                            for( int j = 0; j < discOrder; j++ )
                            {

                                double dNodalArea = dCoeff[i][j] * dGaussW[p] * dGaussW[q] * dJacobian;

                                dVar[dataGLLNodes[i][j][k] - 1] += dSample * dNodalArea;

                                dNodeArea[dataGLLNodes[i][j][k] - 1] += dNodalArea;
                            }
                        }
                    }
                }
            }
        }

        // Divide by area
        if( fGLLIntegrate )
        {
            for( size_t i = 0; i < dVar.GetRows(); i++ )
            {
                dVar[i] /= dNodeArea[i];
            }
        }

        // Let us rearrange the data based on DoF ID specification
        if( ctx == Remapper::SourceMesh )
        {
            for( unsigned j = 0; j < entities.size(); j++ )
                for( int p = 0; p < discOrder; p++ )
                    for( int q = 0; q < discOrder; q++ )
                    {
                        const int offsetDOF = j * discOrder * discOrder + p * discOrder + q;
                        dVarMB[offsetDOF]   = dVar[col_dtoc_dofmap[offsetDOF]];
                    }
        }
        else
        {
            for( unsigned j = 0; j < entities.size(); j++ )
                for( int p = 0; p < discOrder; p++ )
                    for( int q = 0; q < discOrder; q++ )
                    {
                        const int offsetDOF = j * discOrder * discOrder + p * discOrder + q;
                        dVarMB[offsetDOF]   = dVar[row_dtoc_dofmap[offsetDOF]];
                    }
        }

        // Set the tag data
        rval = m_interface->tag_set_data( solnTag, entities, &dVarMB[0] );MB_CHK_ERR( rval );
    }
    else
    {
        // assert( discOrder == 1 );
        if( discMethod == DiscretizationType_FV )
        {
            /* Compute an element-wise integral to store the sampled solution based on Quadrature
             * rules */
            // Resize the array
            dVar.Allocate( trmesh->faces.size() );

            std::vector< Node >& nodes = trmesh->nodes;

            // Loop through all Faces
            for( size_t i = 0; i < trmesh->faces.size(); i++ )
            {
                const Face& face = trmesh->faces[i];

                // Loop through all sub-triangles
                for( size_t j = 0; j < face.edges.size() - 2; j++ )
                {

                    const Node& node0 = nodes[face[0]];
                    const Node& node1 = nodes[face[j + 1]];
                    const Node& node2 = nodes[face[j + 2]];

                    // Triangle area
                    Face faceTri( 3 );
                    faceTri.SetNode( 0, face[0] );
                    faceTri.SetNode( 1, face[j + 1] );
                    faceTri.SetNode( 2, face[j + 2] );

                    double dTriangleArea = CalculateFaceArea( faceTri, nodes );

                    // Calculate the element average
                    double dTotalSample = 0.0;

                    // Loop through all quadrature points
                    for( int k = 0; k < TriQuadraturePoints; k++ )
                    {
                        Node node( TriQuadratureG[k][0] * node0.x + TriQuadratureG[k][1] * node1.x +
                                       TriQuadratureG[k][2] * node2.x,
                                   TriQuadratureG[k][0] * node0.y + TriQuadratureG[k][1] * node1.y +
                                       TriQuadratureG[k][2] * node2.y,
                                   TriQuadratureG[k][0] * node0.z + TriQuadratureG[k][1] * node1.z +
                                       TriQuadratureG[k][2] * node2.z );

                        double dMagnitude = node.Magnitude();
                        node.x /= dMagnitude;
                        node.y /= dMagnitude;
                        node.z /= dMagnitude;

                        double dLon = atan2( node.y, node.x );
                        if( dLon < 0.0 )
                        {
                            dLon += 2.0 * M_PI;
                        }
                        double dLat = asin( node.z );

                        double dSample = ( *testFunction )( dLon, dLat );

                        dTotalSample += dSample * TriQuadratureW[k] * dTriangleArea;
                    }

                    dVar[i] += dTotalSample / trmesh->vecFaceArea[i];
                }
            }
            rval = m_interface->tag_set_data( solnTag, entities, &dVar[0] );MB_CHK_ERR( rval );
        }
        else /* discMethod == DiscretizationType_PCLOUD */
        {
            /* Get the coordinates of the vertices and sample the functionals accordingly */
            std::vector< Node >& nodes = trmesh->nodes;

            // Resize the array
            dVar.Allocate( nodes.size() );

            for( size_t j = 0; j < nodes.size(); j++ )
            {
                Node& node        = nodes[j];
                double dMagnitude = node.Magnitude();
                node.x /= dMagnitude;
                node.y /= dMagnitude;
                node.z /= dMagnitude;
                double dLon = atan2( node.y, node.x );
                if( dLon < 0.0 )
                {
                    dLon += 2.0 * M_PI;
                }
                double dLat = asin( node.z );

                double dSample = ( *testFunction )( dLon, dLat );
                dVar[j]        = dSample;
            }

            rval = m_interface->tag_set_data( solnTag, entities, &dVar[0] );MB_CHK_ERR( rval );
        }
    }

    return moab::MB_SUCCESS;
}

moab::ErrorCode moab::TempestOnlineMap::ComputeMetrics( moab::Remapper::IntersectionContext ctx, moab::Tag& exactTag,
                                                        moab::Tag& approxTag, std::map< std::string, double >& metrics,
                                                        bool verbose )
{
    moab::ErrorCode rval;
    const bool outputEnabled = ( is_root );
    int discOrder;
    DiscretizationType discMethod;
    moab::EntityHandle meshset;
    moab::Range entities;
    Mesh* trmesh;
    switch( ctx )
    {
        case Remapper::SourceMesh:
            meshset    = m_remapper->m_covering_source_set;
            trmesh     = m_remapper->m_covering_source;
            entities   = ( m_remapper->point_cloud_source ? m_remapper->m_covering_source_vertices
                                                        : m_remapper->m_covering_source_entities );
            discOrder  = m_nDofsPEl_Src;
            discMethod = m_eInputType;
            break;

        case Remapper::TargetMesh:
            meshset = m_remapper->m_target_set;
            trmesh  = m_remapper->m_target;
            entities =
                ( m_remapper->point_cloud_target ? m_remapper->m_target_vertices : m_remapper->m_target_entities );
            discOrder  = m_nDofsPEl_Dest;
            discMethod = m_eOutputType;
            break;

        default:
            if( outputEnabled )
                std::cout << "Invalid context specified for defining an analytical solution tag" << std::endl;
            return moab::MB_FAILURE;
    }

    // Let us create teh solution tag with appropriate information for name, discretization order
    // (DoF space)
    std::string exactTagName, projTagName;
    const int ntotsize = entities.size() * discOrder * discOrder;
    int ntotsize_glob  = 0;
    std::vector< double > exactSolution( ntotsize, 0.0 ), projSolution( ntotsize, 0.0 );
    rval = m_interface->tag_get_name( exactTag, exactTagName );MB_CHK_ERR( rval );
    rval = m_interface->tag_get_data( exactTag, entities, &exactSolution[0] );MB_CHK_ERR( rval );
    rval = m_interface->tag_get_name( approxTag, projTagName );MB_CHK_ERR( rval );
    rval = m_interface->tag_get_data( approxTag, entities, &projSolution[0] );MB_CHK_ERR( rval );

    std::vector< double > errnorms( 3, 0.0 ), globerrnorms( 3, 0.0 );  //  L1Err, L2Err, LinfErr
    for( int i = 0; i < ntotsize; ++i )
    {
        const double error = fabs( exactSolution[i] - projSolution[i] );
        errnorms[0] += error;
        errnorms[1] += error * error;
        errnorms[2] = ( error > errnorms[2] ? error : errnorms[2] );
    }
#ifdef MOAB_HAVE_MPI
    if( m_pcomm )
    {
        MPI_Reduce( &ntotsize, &ntotsize_glob, 1, MPI_INT, MPI_SUM, 0, m_pcomm->comm() );
        MPI_Reduce( &errnorms[0], &globerrnorms[0], 2, MPI_DOUBLE, MPI_SUM, 0, m_pcomm->comm() );
        MPI_Reduce( &errnorms[2], &globerrnorms[2], 1, MPI_DOUBLE, MPI_MAX, 0, m_pcomm->comm() );
    }
#else
    ntotsize_glob = ntotsize;
    globerrnorms = errnorms;
#endif
    globerrnorms[0] = ( globerrnorms[0] / ntotsize_glob );
    globerrnorms[1] = std::sqrt( globerrnorms[1] / ntotsize_glob );

    metrics.clear();
    metrics["L1Error"]   = globerrnorms[0];
    metrics["L2Error"]   = globerrnorms[1];
    metrics["LinfError"] = globerrnorms[2];

    if( verbose && is_root )
    {
        std::cout << "Error metrics when comparing " << projTagName << " against " << exactTagName << std::endl;
        std::cout << "\t L_1 error   = " << globerrnorms[0] << std::endl;
        std::cout << "\t L_2 error   = " << globerrnorms[1] << std::endl;
        std::cout << "\t L_inf error = " << globerrnorms[2] << std::endl;
    }

    return moab::MB_SUCCESS;
}
