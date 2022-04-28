/*
 * =====================================================================================
 *
 *       Filename:  TempestRemapper.hpp
 *
 *    Description:  Interface to the TempestRemap library to enable intersection and
 *                  high-order conservative remapping of climate solution from
 *                  arbitrary resolution of source and target grids on the sphere.
 *
 *         Author:  Vijay S. Mahadevan (vijaysm), mahadevan@anl.gov
 *
 * =====================================================================================
 */

#include <string>
#include <iostream>
#include <cassert>
#include "DebugOutput.hpp"
#include "moab/Remapping/TempestRemapper.hpp"
#include "moab/ReadUtilIface.hpp"

// Intersection includes
#include "moab/IntxMesh/Intx2MeshOnSphere.hpp"
#include "moab/IntxMesh/IntxUtils.hpp"

#include "moab/AdaptiveKDTree.hpp"
#include "moab/SpatialLocator.hpp"

// skinner for augmenting overlap mesh to complete coverage
#include "moab/Skinner.hpp"
#include "MBParallelConventions.h"

#ifdef MOAB_HAVE_TEMPESTREMAP
#include "GaussLobattoQuadrature.h"
#endif

// #define VERBOSE

namespace moab
{

///////////////////////////////////////////////////////////////////////////////////

ErrorCode TempestRemapper::initialize( bool initialize_fsets )
{
    ErrorCode rval;
    if( initialize_fsets )
    {
        rval = m_interface->create_meshset( moab::MESHSET_SET, m_source_set );MB_CHK_SET_ERR( rval, "Can't create new set" );
        rval = m_interface->create_meshset( moab::MESHSET_SET, m_target_set );MB_CHK_SET_ERR( rval, "Can't create new set" );
        rval = m_interface->create_meshset( moab::MESHSET_SET, m_overlap_set );MB_CHK_SET_ERR( rval, "Can't create new set" );
    }
    else
    {
        m_source_set  = 0;
        m_target_set  = 0;
        m_overlap_set = 0;
    }

    is_parallel = false;
    is_root     = true;
    rank        = 0;
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

    m_source          = NULL;
    m_target          = NULL;
    m_overlap         = NULL;
    m_covering_source = NULL;

    point_cloud_source = false;
    point_cloud_target = false;

    return MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////////

TempestRemapper::~TempestRemapper()
{
    this->clear();
}

ErrorCode TempestRemapper::clear()
{
    // destroy all meshes
    if( m_source )
    {
        delete m_source;
        m_source = NULL;
    }
    if( m_target )
    {
        delete m_target;
        m_target = NULL;
    }
    if( m_overlap )
    {
        delete m_overlap;
        m_overlap = NULL;
    }
    if( m_covering_source && size > 1 )
    {
        delete m_covering_source;
        m_covering_source = NULL;
    }

    point_cloud_source = false;
    point_cloud_target = false;

    m_source_entities.clear();
    m_source_vertices.clear();
    m_covering_source_entities.clear();
    m_covering_source_vertices.clear();
    m_target_entities.clear();
    m_target_vertices.clear();
    m_overlap_entities.clear();
    gid_to_lid_src.clear();
    gid_to_lid_tgt.clear();
    gid_to_lid_covsrc.clear();
    lid_to_gid_src.clear();
    lid_to_gid_tgt.clear();
    lid_to_gid_covsrc.clear();

    return MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////////

ErrorCode TempestRemapper::LoadMesh( Remapper::IntersectionContext ctx, std::string inputFilename,
                                     TempestMeshType type )
{
    if( ctx == Remapper::SourceMesh )
    {
        m_source_type = type;
        return load_tempest_mesh_private( inputFilename, &m_source );
    }
    else if( ctx == Remapper::TargetMesh )
    {
        m_target_type = type;
        return load_tempest_mesh_private( inputFilename, &m_target );
    }
    else if( ctx != Remapper::DEFAULT )
    {
        m_overlap_type = type;
        return load_tempest_mesh_private( inputFilename, &m_overlap );
    }
    else
    {
        MB_CHK_SET_ERR( MB_FAILURE, "Invalid IntersectionContext context provided" );
    }
}

ErrorCode TempestRemapper::load_tempest_mesh_private( std::string inputFilename, Mesh** tempest_mesh )
{
    const bool outputEnabled = ( TempestRemapper::verbose && is_root );
    if( outputEnabled ) std::cout << "\nLoading TempestRemap Mesh object from file = " << inputFilename << " ...\n";

    {
        NcError error( NcError::silent_nonfatal );

        try
        {
            // Load input mesh
            if( outputEnabled ) std::cout << "Loading mesh ...\n";
            Mesh* mesh = new Mesh( inputFilename );
            mesh->RemoveZeroEdges();
            if( outputEnabled ) std::cout << "----------------\n";

            // Validate mesh
            if( meshValidate )
            {
                if( outputEnabled ) std::cout << "Validating mesh ...\n";
                mesh->Validate();
                if( outputEnabled ) std::cout << "-------------------\n";
            }

            // Construct the edge map on the mesh
            if( constructEdgeMap )
            {
                if( outputEnabled ) std::cout << "Constructing edge map on mesh ...\n";
                mesh->ConstructEdgeMap( false );
                if( outputEnabled ) std::cout << "---------------------------------\n";
            }

            if( tempest_mesh ) *tempest_mesh = mesh;
        }
        catch( Exception& e )
        {
            std::cout << "TempestRemap ERROR: " << e.ToString() << "\n";
            return MB_FAILURE;
        }
        catch( ... )
        {
            return MB_FAILURE;
        }
    }
    return MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////////

ErrorCode TempestRemapper::ConvertTempestMesh( Remapper::IntersectionContext ctx )
{
    const bool outputEnabled = ( TempestRemapper::verbose && is_root );
    if( ctx == Remapper::SourceMesh )
    {
        if( outputEnabled ) std::cout << "Converting (source) TempestRemap Mesh object to MOAB representation ...\n";
        return convert_tempest_mesh_private( m_source_type, m_source, m_source_set, m_source_entities,
                                             &m_source_vertices );
    }
    else if( ctx == Remapper::TargetMesh )
    {
        if( outputEnabled ) std::cout << "Converting (target) TempestRemap Mesh object to MOAB representation ...\n";
        return convert_tempest_mesh_private( m_target_type, m_target, m_target_set, m_target_entities,
                                             &m_target_vertices );
    }
    else if( ctx != Remapper::DEFAULT )
    {
        if( outputEnabled ) std::cout << "Converting (overlap) TempestRemap Mesh object to MOAB representation ...\n";
        return convert_tempest_mesh_private( m_overlap_type, m_overlap, m_overlap_set, m_overlap_entities, NULL );
    }
    else
    {
        MB_CHK_SET_ERR( MB_FAILURE, "Invalid IntersectionContext context provided" );
    }
}

ErrorCode TempestRemapper::convert_tempest_mesh_private( TempestMeshType meshType, Mesh* mesh, EntityHandle& mesh_set,
                                                         Range& entities, Range* vertices )
{
    ErrorCode rval;

    const bool outputEnabled = ( TempestRemapper::verbose && is_root );
    const NodeVector& nodes  = mesh->nodes;
    const FaceVector& faces  = mesh->faces;

    moab::DebugOutput dbgprint( std::cout, this->rank, 0 );
    dbgprint.set_prefix( "[TempestToMOAB]: " );

    ReadUtilIface* iface;
    rval = m_interface->query_interface( iface );MB_CHK_SET_ERR( rval, "Can't get reader interface" );

    Tag gidTag = m_interface->globalId_tag();

    // Set the data for the vertices
    std::vector< double* > arrays;
    std::vector< int > gidsv( nodes.size() );
    EntityHandle startv;
    rval = iface->get_node_coords( 3, nodes.size(), 0, startv, arrays );MB_CHK_SET_ERR( rval, "Can't get node coords" );
    for( unsigned iverts = 0; iverts < nodes.size(); ++iverts )
    {
        const Node& node  = nodes[iverts];
        arrays[0][iverts] = node.x;
        arrays[1][iverts] = node.y;
        arrays[2][iverts] = node.z;
        gidsv[iverts]     = iverts + 1;
    }
    Range mbverts( startv, startv + nodes.size() - 1 );
    rval = m_interface->add_entities( mesh_set, mbverts );MB_CHK_SET_ERR( rval, "Can't add entities" );
    rval = m_interface->tag_set_data( gidTag, mbverts, &gidsv[0] );MB_CHK_SET_ERR( rval, "Can't set global_id tag" );

    gidsv.clear();
    entities.clear();

    Tag srcParentTag, tgtParentTag;
    std::vector< int > srcParent, tgtParent;
    bool storeParentInfo = ( mesh->vecSourceFaceIx.size() > 0 );

    if( storeParentInfo )
    {
        int defaultInt = -1;
        rval           = m_interface->tag_get_handle( "TargetParent", 1, MB_TYPE_INTEGER, tgtParentTag,
                                            MB_TAG_DENSE | MB_TAG_CREAT, &defaultInt );MB_CHK_SET_ERR( rval, "can't create positive tag" );

        rval = m_interface->tag_get_handle( "SourceParent", 1, MB_TYPE_INTEGER, srcParentTag,
                                            MB_TAG_DENSE | MB_TAG_CREAT, &defaultInt );MB_CHK_SET_ERR( rval, "can't create negative tag" );
    }

    // Let us first perform a full pass assuming arbitrary polygons. This is especially true for
    // overlap meshes.
    //   1. We do a first pass over faces, decipher edge size and group into categories based on
    //   element type
    //   2. Next we loop over type, and add blocks of elements into MOAB
    //   3. For each block within the loop, also update the connectivity of elements.
    {
        if( outputEnabled )
            dbgprint.printf( 0, "..Mesh size: Nodes [%zu]  Elements [%zu].\n", nodes.size(), faces.size() );
        const int NMAXPOLYEDGES = 15;
        std::vector< unsigned > nPolys( NMAXPOLYEDGES, 0 );
        std::vector< std::vector< int > > typeNSeqs( NMAXPOLYEDGES );
        for( unsigned ifaces = 0; ifaces < faces.size(); ++ifaces )
        {
            const int iType = faces[ifaces].edges.size();
            nPolys[iType]++;
            typeNSeqs[iType].push_back( ifaces );
        }
        int iBlock = 0;
        for( unsigned iType = 0; iType < NMAXPOLYEDGES; ++iType )
        {
            if( !nPolys[iType] ) continue;  // Nothing to do

            const unsigned num_v_per_elem = iType;
            EntityHandle starte;  // Connectivity
            EntityHandle* conn;

            // Allocate the connectivity array, depending on the element type
            switch( num_v_per_elem )
            {
                case 3:
                    if( outputEnabled )
                        dbgprint.printf( 0, "....Block %d: Triangular Elements [%u].\n", iBlock++, nPolys[iType] );
                    rval = iface->get_element_connect( nPolys[iType], num_v_per_elem, MBTRI, 0, starte, conn );MB_CHK_SET_ERR( rval, "Can't get element connectivity" );
                    break;
                case 4:
                    if( outputEnabled )
                        dbgprint.printf( 0, "....Block %d: Quadrilateral Elements [%u].\n", iBlock++, nPolys[iType] );
                    rval = iface->get_element_connect( nPolys[iType], num_v_per_elem, MBQUAD, 0, starte, conn );MB_CHK_SET_ERR( rval, "Can't get element connectivity" );
                    break;
                default:
                    if( outputEnabled )
                        dbgprint.printf( 0, "....Block %d: Polygonal [%u] Elements [%u].\n", iBlock++, iType,
                                         nPolys[iType] );
                    rval = iface->get_element_connect( nPolys[iType], num_v_per_elem, MBPOLYGON, 0, starte, conn );MB_CHK_SET_ERR( rval, "Can't get element connectivity" );
                    break;
            }

            Range mbcells( starte, starte + nPolys[iType] - 1 );
            m_interface->add_entities( mesh_set, mbcells );

            if( storeParentInfo )
            {
                srcParent.resize( mbcells.size(), -1 );
                tgtParent.resize( mbcells.size(), -1 );
            }

            std::vector< int > gids( typeNSeqs[iType].size() );
            for( unsigned ifaces = 0, offset = 0; ifaces < typeNSeqs[iType].size(); ++ifaces )
            {
                const int fIndex = typeNSeqs[iType][ifaces];
                const Face& face = faces[fIndex];
                // conn[offset++] = startv + face.edges[0].node[0];
                for( unsigned iedges = 0; iedges < face.edges.size(); ++iedges )
                {
                    conn[offset++] = startv + face.edges[iedges].node[0];
                }

                if( storeParentInfo )
                {
                    srcParent[ifaces] = mesh->vecSourceFaceIx[fIndex] + 1;
                    tgtParent[ifaces] = mesh->vecTargetFaceIx[fIndex] + 1;
                }

                gids[ifaces] = typeNSeqs[iType][ifaces] + 1;
            }
            rval = m_interface->tag_set_data( gidTag, mbcells, &gids[0] );MB_CHK_SET_ERR( rval, "Can't set global_id tag" );

            if( meshType == OVERLAP_FILES )
            {
                // Now let us update the adjacency data, because some elements are new
                rval = iface->update_adjacencies( starte, nPolys[iType], num_v_per_elem, conn );MB_CHK_SET_ERR( rval, "Can't update adjacencies" );
                // Generate all adj entities dimension 1 and 2 (edges and faces/ tri or qua)
                Range edges;
                rval = m_interface->get_adjacencies( mbcells, 1, true, edges, Interface::UNION );MB_CHK_SET_ERR( rval, "Can't get edges" );
            }

            if( storeParentInfo )
            {
                rval = m_interface->tag_set_data( srcParentTag, mbcells, &srcParent[0] );MB_CHK_SET_ERR( rval, "Can't set tag data" );
                rval = m_interface->tag_set_data( tgtParentTag, mbcells, &tgtParent[0] );MB_CHK_SET_ERR( rval, "Can't set tag data" );
            }
            entities.merge( mbcells );
        }
    }

    if( vertices ) *vertices = mbverts;

    return MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////////

ErrorCode TempestRemapper::ConvertMeshToTempest( Remapper::IntersectionContext ctx )
{
    ErrorCode rval;
    const bool outputEnabled = ( TempestRemapper::verbose && is_root );

    moab::DebugOutput dbgprint( std::cout, this->rank, 0 );
    dbgprint.set_prefix( "[MOABToTempest]: " );

    if( ctx == Remapper::SourceMesh )
    {
        if( !m_source ) m_source = new Mesh();
        if( outputEnabled ) dbgprint.printf( 0, "Converting (source) MOAB to TempestRemap Mesh representation ...\n" );
        rval = convert_mesh_to_tempest_private( m_source, m_source_set, m_source_entities, &m_source_vertices );MB_CHK_SET_ERR( rval, "Can't convert source mesh to Tempest" );
        if( m_source_entities.size() == 0 && m_source_vertices.size() != 0 )
        {
            this->point_cloud_source = true;
        }
    }
    else if( ctx == Remapper::TargetMesh )
    {
        if( !m_target ) m_target = new Mesh();
        if( outputEnabled ) dbgprint.printf( 0, "Converting (target) MOAB to TempestRemap Mesh representation ...\n" );
        rval = convert_mesh_to_tempest_private( m_target, m_target_set, m_target_entities, &m_target_vertices );MB_CHK_SET_ERR( rval, "Can't convert target mesh to Tempest" );
        if( m_target_entities.size() == 0 && m_target_vertices.size() != 0 ) this->point_cloud_target = true;
    }
    else if( ctx == Remapper::OverlapMesh )  // Overlap mesh
    {
        if( !m_overlap ) m_overlap = new Mesh();
        if( outputEnabled ) dbgprint.printf( 0, "Converting (overlap) MOAB to TempestRemap Mesh representation ...\n" );
        rval = convert_overlap_mesh_sorted_by_source();MB_CHK_SET_ERR( rval, "Can't convert overlap mesh to Tempest" );
    }
    else
    {
        MB_CHK_SET_ERR( MB_FAILURE, "Invalid IntersectionContext context provided" );
    }

    return rval;
}

ErrorCode TempestRemapper::convert_mesh_to_tempest_private( Mesh* mesh, EntityHandle mesh_set, moab::Range& elems,
                                                            moab::Range* pverts )
{
    ErrorCode rval;
    Range verts;

    NodeVector& nodes = mesh->nodes;
    FaceVector& faces = mesh->faces;

    elems.clear();
    rval = m_interface->get_entities_by_dimension( mesh_set, 2, elems );MB_CHK_ERR( rval );

    // resize the number of elements in Tempest mesh
    faces.resize( elems.size() );

    // let us now get the vertices from all the elements
    rval = m_interface->get_connectivity( elems, verts );MB_CHK_ERR( rval );
    if( verts.size() == 0 )
    {
        rval = m_interface->get_entities_by_dimension( mesh_set, 0, verts );MB_CHK_ERR( rval );
    }
    // assert(verts.size() > 0); // If not, this may be an invalid mesh ! possible for unbalanced
    // loads

    std::map< EntityHandle, int > indxMap;
    bool useRange = true;
    if( verts.compactness() > 0.01 )
    {
        int j = 0;
        for( Range::iterator it = verts.begin(); it != verts.end(); it++ )
            indxMap[*it] = j++;
        useRange = false;
    }

    for( unsigned iface = 0; iface < elems.size(); ++iface )
    {
        Face& face           = faces[iface];
        EntityHandle ehandle = elems[iface];

        // get the connectivity for each edge
        const EntityHandle* connectface;
        int nnodesf;
        rval = m_interface->get_connectivity( ehandle, connectface, nnodesf );MB_CHK_ERR( rval );

        face.edges.resize( nnodesf );
        for( int iverts = 0; iverts < nnodesf; ++iverts )
        {
            int indx = ( useRange ? verts.index( connectface[iverts] ) : indxMap[connectface[iverts]] );
            assert( indx >= 0 );
            face.SetNode( iverts, indx );
        }
    }

    unsigned nnodes = verts.size();
    nodes.resize( nnodes );

    // Set the data for the vertices
    std::vector< double > coordx( nnodes ), coordy( nnodes ), coordz( nnodes );
    rval = m_interface->get_coords( verts, &coordx[0], &coordy[0], &coordz[0] );MB_CHK_ERR( rval );
    for( unsigned inode = 0; inode < nnodes; ++inode )
    {
        Node& node = nodes[inode];
        node.x     = coordx[inode];
        node.y     = coordy[inode];
        node.z     = coordz[inode];
    }
    coordx.clear();
    coordy.clear();
    coordz.clear();

    mesh->RemoveZeroEdges();
    mesh->RemoveCoincidentNodes();

    // Generate reverse node array and edge map
    if( constructEdgeMap ) mesh->ConstructEdgeMap( false );
    // mesh->ConstructReverseNodeArray();

    // mesh->Validate();

    if( pverts )
    {
        pverts->clear();
        *pverts = verts;
    }
    verts.clear();

    return MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////////

bool IntPairComparator( const std::pair< int, int >& a, const std::pair< int, int >& b )
{
    if( a.first == b.first )
        return a.second < b.second;
    else
        return a.first < b.first;
}

moab::ErrorCode moab::TempestRemapper::GetOverlapAugmentedEntities( moab::Range& sharedGhostEntities )
{
    sharedGhostEntities.clear();
#ifdef MOAB_HAVE_MPI
    moab::ErrorCode rval;

    // Remove entities in the intersection mesh that are part of the ghosted overlap
    if( is_parallel && size > 1 )
    {
        moab::Range allents;
        rval = m_interface->get_entities_by_dimension( m_overlap_set, 2, allents );MB_CHK_SET_ERR( rval, "Getting entities dim 2 failed" );

        moab::Range sharedents;
        moab::Tag ghostTag;
        std::vector< int > ghFlags( allents.size() );
        rval = m_interface->tag_get_handle( "ORIG_PROC", ghostTag );MB_CHK_ERR( rval );
        rval = m_interface->tag_get_data( ghostTag, allents, &ghFlags[0] );MB_CHK_ERR( rval );
        for( unsigned i = 0; i < allents.size(); ++i )
            if( ghFlags[i] >= 0 )                 // it means it is a ghost overlap element
                sharedents.insert( allents[i] );  // this should not participate in smat!

        allents = subtract( allents, sharedents );

        // Get connectivity from all ghosted elements and filter out
        // the vertices that are not owned
        moab::Range ownedverts, sharedverts;
        rval = m_interface->get_connectivity( allents, ownedverts );MB_CHK_SET_ERR( rval, "Deleting entities dim 0 failed" );
        rval = m_interface->get_connectivity( sharedents, sharedverts );MB_CHK_SET_ERR( rval, "Deleting entities dim 0 failed" );
        sharedverts = subtract( sharedverts, ownedverts );
        // rval = m_interface->remove_entities(m_overlap_set, sharedents);MB_CHK_SET_ERR(rval,
        // "Deleting entities dim 2 failed"); rval = m_interface->remove_entities(m_overlap_set,
        // sharedverts);MB_CHK_SET_ERR(rval, "Deleting entities dim 0 failed");

        sharedGhostEntities.merge( sharedents );
        // sharedGhostEntities.merge(sharedverts);
    }
#endif
    return moab::MB_SUCCESS;
}

ErrorCode TempestRemapper::convert_overlap_mesh_sorted_by_source()
{
    ErrorCode rval;

    m_overlap_entities.clear();
    rval = m_interface->get_entities_by_dimension( m_overlap_set, 2, m_overlap_entities );MB_CHK_ERR( rval );

    // Allocate for the overlap mesh
    if( !m_overlap ) m_overlap = new Mesh();

    size_t n_overlap_entitites = m_overlap_entities.size();

    std::vector< std::pair< int, int > > sorted_overlap_order( n_overlap_entitites );
    {
        Tag srcParentTag, tgtParentTag;
        rval = m_interface->tag_get_handle( "SourceParent", srcParentTag );MB_CHK_ERR( rval );
        rval = m_interface->tag_get_handle( "TargetParent", tgtParentTag );MB_CHK_ERR( rval );
        // Overlap mesh: resize the source and target connection arrays
        m_overlap->vecTargetFaceIx.resize( n_overlap_entitites );
        m_overlap->vecSourceFaceIx.resize( n_overlap_entitites );

        // Overlap mesh: resize the source and target connection arrays
        std::vector< int > rbids_src( n_overlap_entitites ), rbids_tgt( n_overlap_entitites );
        rval = m_interface->tag_get_data( srcParentTag, m_overlap_entities, &rbids_src[0] );MB_CHK_ERR( rval );
        rval = m_interface->tag_get_data( tgtParentTag, m_overlap_entities, &rbids_tgt[0] );MB_CHK_ERR( rval );
        for( size_t ix = 0; ix < n_overlap_entitites; ++ix )
        {
            sorted_overlap_order[ix].first =
                ( gid_to_lid_covsrc.size() ? gid_to_lid_covsrc[rbids_src[ix]] : rbids_src[ix] );
            sorted_overlap_order[ix].second = ix;
        }
        std::sort( sorted_overlap_order.begin(), sorted_overlap_order.end(), IntPairComparator );
        // sorted_overlap_order[ie].second , ie=0,nOverlap-1 is the order such that overlap elems
        // are ordered by source parent

        std::vector< int > ghFlags;
        if( is_parallel && size > 1 )
        {
            Tag ghostTag;
            ghFlags.resize( n_overlap_entitites );
            rval = m_interface->tag_get_handle( "ORIG_PROC", ghostTag );MB_CHK_ERR( rval );
            rval = m_interface->tag_get_data( ghostTag, m_overlap_entities, &ghFlags[0] );MB_CHK_ERR( rval );
        }
        for( unsigned ie = 0; ie < n_overlap_entitites; ++ie )
        {
            int ix = sorted_overlap_order[ie].second;  // original index of the element
            m_overlap->vecSourceFaceIx[ie] =
                ( gid_to_lid_covsrc.size() ? gid_to_lid_covsrc[rbids_src[ix]] : rbids_src[ix] - 1 );
            if( is_parallel && size > 1 && ghFlags[ix] >= 0 )  // it means it is a ghost overlap element
                m_overlap->vecTargetFaceIx[ie] = -1;           // this should not participate in smat!
            else
                m_overlap->vecTargetFaceIx[ie] =
                    ( gid_to_lid_tgt.size() ? gid_to_lid_tgt[rbids_tgt[ix]] : rbids_tgt[ix] - 1 );
        }
    }

    FaceVector& faces = m_overlap->faces;
    faces.resize( n_overlap_entitites );

    Range verts;
    // let us now get the vertices from all the elements
    rval = m_interface->get_connectivity( m_overlap_entities, verts );MB_CHK_ERR( rval );
    // std::cout << "Vertices size = " << verts.size() << " , psize = " << verts.psize() << ",
    // compactness = " << verts.compactness() << std::endl;

    std::map< EntityHandle, int > indxMap;
    bool useRange = true;
    if( verts.compactness() > 0.01 )
    {
        int j = 0;
        for( Range::iterator it = verts.begin(); it != verts.end(); ++it )
            indxMap[*it] = j++;
        useRange = false;
    }

    for( unsigned ifac = 0; ifac < m_overlap_entities.size(); ++ifac )
    {
        const unsigned iface = sorted_overlap_order[ifac].second;
        Face& face           = faces[ifac];
        EntityHandle ehandle = m_overlap_entities[iface];

        // get the connectivity for each edge
        const EntityHandle* connectface;
        int nnodesf;
        rval = m_interface->get_connectivity( ehandle, connectface, nnodesf );MB_CHK_ERR( rval );

        face.edges.resize( nnodesf );
        for( int iverts = 0; iverts < nnodesf; ++iverts )
        {
            int indx = ( useRange ? verts.index( connectface[iverts] ) : indxMap[connectface[iverts]] );
            assert( indx >= 0 );
            face.SetNode( iverts, indx );
        }
    }

    unsigned nnodes   = verts.size();
    NodeVector& nodes = m_overlap->nodes;
    nodes.resize( nnodes );

    // Set the data for the vertices
    std::vector< double > coordx( nnodes ), coordy( nnodes ), coordz( nnodes );
    rval = m_interface->get_coords( verts, &coordx[0], &coordy[0], &coordz[0] );MB_CHK_ERR( rval );
    for( unsigned inode = 0; inode < nnodes; ++inode )
    {
        Node& node = nodes[inode];
        node.x     = coordx[inode];
        node.y     = coordy[inode];
        node.z     = coordz[inode];
    }
    coordx.clear();
    coordy.clear();
    coordz.clear();
    verts.clear();

    m_overlap->RemoveZeroEdges();
    m_overlap->RemoveCoincidentNodes( false );

    // Generate reverse node array and edge map
    // if ( constructEdgeMap ) m_overlap->ConstructEdgeMap(false);
    // m_overlap->ConstructReverseNodeArray();

    // m_overlap->Validate();
    return MB_SUCCESS;
}

// Should be ordered as Source, Target, Overlap
ErrorCode TempestRemapper::ComputeGlobalLocalMaps()
{
    ErrorCode rval;

    if( 0 == m_covering_source )
    {
        m_covering_source = new Mesh();
        rval = convert_mesh_to_tempest_private( m_covering_source, m_covering_source_set, m_covering_source_entities,
                                                &m_covering_source_vertices );MB_CHK_SET_ERR( rval, "Can't convert source Tempest mesh" );
        // std::cout << "ComputeGlobalLocalMaps: " << rank << ", " << " covering entities = [" <<
        // m_covering_source_vertices.size() << ", " << m_covering_source_entities.size() << "]\n";
    }
    gid_to_lid_src.clear();
    lid_to_gid_src.clear();
    gid_to_lid_covsrc.clear();
    lid_to_gid_covsrc.clear();
    gid_to_lid_tgt.clear();
    lid_to_gid_tgt.clear();
    {
        Tag gidtag = m_interface->globalId_tag();

        std::vector< int > gids;
        if( point_cloud_source )
        {
            gids.resize( m_covering_source_vertices.size(), -1 );
            rval = m_interface->tag_get_data( gidtag, m_covering_source_vertices, &gids[0] );MB_CHK_ERR( rval );
        }
        else
        {
            gids.resize( m_covering_source_entities.size(), -1 );
            rval = m_interface->tag_get_data( gidtag, m_covering_source_entities, &gids[0] );MB_CHK_ERR( rval );
        }
        for( unsigned ie = 0; ie < gids.size(); ++ie )
        {
            gid_to_lid_covsrc[gids[ie]] = ie;
            lid_to_gid_covsrc[ie]       = gids[ie];
        }

        if( point_cloud_source )
        {
            gids.resize( m_source_vertices.size(), -1 );
            rval = m_interface->tag_get_data( gidtag, m_source_vertices, &gids[0] );MB_CHK_ERR( rval );
        }
        else
        {
            gids.resize( m_source_entities.size(), -1 );
            rval = m_interface->tag_get_data( gidtag, m_source_entities, &gids[0] );MB_CHK_ERR( rval );
        }
        for( unsigned ie = 0; ie < gids.size(); ++ie )
        {
            gid_to_lid_src[gids[ie]] = ie;
            lid_to_gid_src[ie]       = gids[ie];
        }

        if( point_cloud_target )
        {
            gids.resize( m_target_vertices.size(), -1 );
            rval = m_interface->tag_get_data( gidtag, m_target_vertices, &gids[0] );MB_CHK_ERR( rval );
        }
        else
        {
            gids.resize( m_target_entities.size(), -1 );
            rval = m_interface->tag_get_data( gidtag, m_target_entities, &gids[0] );MB_CHK_ERR( rval );
        }
        for( unsigned ie = 0; ie < gids.size(); ++ie )
        {
            gid_to_lid_tgt[gids[ie]] = ie;
            lid_to_gid_tgt[ie]       = gids[ie];
        }
    }

    return MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////////

moab::ErrorCode moab::TempestRemapper::WriteTempestIntersectionMesh( std::string strOutputFileName,
                                                                     const bool fAllParallel, const bool fInputConcave,
                                                                     const bool fOutputConcave )
{
    // Let us alos write out the TempestRemap equivalent so that we can do some verification checks
    if( fAllParallel )
    {
        if( is_root && size == 1 )
        {
            this->m_source->CalculateFaceAreas( fInputConcave );
            this->m_target->CalculateFaceAreas( fOutputConcave );
            this->m_overlap->Write( strOutputFileName.c_str(), NcFile::Netcdf4 );
        }
        else
        {
            // Perform reduction and write from root processor
            // if ( is_root )
            //     std::cout << "--- PARALLEL IMPLEMENTATION is NOT AVAILABLE yet ---\n";

            this->m_source->CalculateFaceAreas( fInputConcave );
            this->m_covering_source->CalculateFaceAreas( fInputConcave );
            this->m_target->CalculateFaceAreas( fOutputConcave );
            this->m_overlap->Write( strOutputFileName.c_str(), NcFile::Netcdf4 );
        }
    }
    else
    {
        this->m_source->CalculateFaceAreas( fInputConcave );
        this->m_target->CalculateFaceAreas( fOutputConcave );
        this->m_overlap->Write( strOutputFileName.c_str(), NcFile::Netcdf4 );
    }

    return moab::MB_SUCCESS;
}
void TempestRemapper::SetMeshSet( Remapper::IntersectionContext ctx /* Remapper::CoveringMesh*/,
                                  moab::EntityHandle mset, moab::Range& entities )
{

    if( ctx == Remapper::SourceMesh )  // should not be used
    {
        m_source_entities = entities;
        m_source_set      = mset;
    }
    else if( ctx == Remapper::TargetMesh )
    {
        m_target_entities = entities;
        m_target_set      = mset;
    }
    else if( ctx == Remapper::CoveringMesh )
    {
        m_covering_source_entities = entities;
        m_covering_source_set      = mset;
    }
    else
    {
        // some error
    }
    return;
}
///////////////////////////////////////////////////////////////////////////////////

#ifndef MOAB_HAVE_MPI
ErrorCode TempestRemapper::assign_vertex_element_IDs( Tag idtag, EntityHandle this_set, const int dimension,
                                                      const int start_id )
{
    assert( idtag );

    ErrorCode rval;
    Range entities;
    rval = m_interface->get_entities_by_dimension( this_set, dimension, entities );MB_CHK_SET_ERR( rval, "Failed to get entities" );

    if( entities.size() == 0 ) return moab::MB_SUCCESS;

    int idoffset = start_id;
    std::vector< int > gid( entities.size() );
    for( unsigned i = 0; i < entities.size(); ++i )
        gid[i] = idoffset++;

    rval = m_interface->tag_set_data( idtag, entities, &gid[0] );MB_CHK_ERR( rval );

    return moab::MB_SUCCESS;
}
#endif

///////////////////////////////////////////////////////////////////////////////

// Create a custom comparator for Nodes
bool operator<( Node const& lhs, Node const& rhs )
{
    return std::pow( lhs.x - rhs.x, 2.0 ) + std::pow( lhs.y - rhs.y, 2.0 ) + std::pow( lhs.z - rhs.z, 2.0 );
}

ErrorCode TempestRemapper::GenerateCSMeshMetadata( const int ntot_elements, moab::Range& ents,
                                                   moab::Range* secondary_ents, const std::string dofTagName, int nP )
{
    Mesh csMesh;
    int err;
    moab::ErrorCode rval;

    const int res = std::sqrt( ntot_elements / 6 );

    // create a temporary CS mesh
    // NOTE: This will not work for RRM grids. Need to run HOMME for that case anyway
    err = GenerateCSMesh( csMesh, res, "", "NetCDF4" );
    if( err )
    {
        MB_CHK_SET_ERR( MB_FAILURE, "Failed to generate CS mesh through TempestRemap" );
        ;
    }

    rval = this->GenerateMeshMetadata( csMesh, ntot_elements, ents, secondary_ents, dofTagName, nP );MB_CHK_SET_ERR( rval, "Failed in call to GenerateMeshMetadata" );

    return moab::MB_SUCCESS;
}

ErrorCode TempestRemapper::GenerateMeshMetadata( Mesh& csMesh, const int ntot_elements, moab::Range& ents,
                                                 moab::Range* secondary_ents, const std::string dofTagName, int nP )
{
    moab::ErrorCode rval;

    Tag dofTag;
    bool created = false;
    rval         = m_interface->tag_get_handle( dofTagName.c_str(), nP * nP, MB_TYPE_INTEGER, dofTag,
                                        MB_TAG_DENSE | MB_TAG_CREAT, 0, &created );MB_CHK_SET_ERR( rval, "Failed creating DoF tag" );

    // Number of Faces
    int nElements = static_cast< int >( csMesh.faces.size() );

    assert( nElements == ntot_elements );

    // Initialize data structures
    DataArray3D< int > dataGLLnodes;
    dataGLLnodes.Allocate( nP, nP, nElements );

    std::map< Node, int > mapNodes;
    std::map< Node, moab::EntityHandle > mapLocalMBNodes;

    // GLL Quadrature nodes
    DataArray1D< double > dG;
    DataArray1D< double > dW;
    GaussLobattoQuadrature::GetPoints( nP, 0.0, 1.0, dG, dW );

    moab::Range entities( ents );
    if( secondary_ents ) entities.insert( secondary_ents->begin(), secondary_ents->end() );
    double elcoords[3];
    for( unsigned iel = 0; iel < entities.size(); ++iel )
    {
        EntityHandle eh = entities[iel];
        rval            = m_interface->get_coords( &eh, 1, elcoords );
        Node elCentroid( elcoords[0], elcoords[1], elcoords[2] );
        mapLocalMBNodes.insert( std::pair< Node, moab::EntityHandle >( elCentroid, eh ) );
    }

    // Build a Kd-tree for local mesh (nearest neighbor searches)
    // Loop over all elements in CS-Mesh
    // Then find if current centroid is in an element
    //     If yes - then let us compute the DoF numbering and set to tag data
    //     If no - then compute DoF numbering BUT DO NOT SET to tag data
    // continue
    int* dofIDs = new int[nP * nP];

    // Write metadata
    for( int k = 0; k < nElements; k++ )
    {
        const Face& face        = csMesh.faces[k];
        const NodeVector& nodes = csMesh.nodes;

        if( face.edges.size() != 4 )
        {
            _EXCEPTIONT( "Mesh must only contain quadrilateral elements" );
        }

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

        bool locElem = false;
        EntityHandle current_eh;
        if( mapLocalMBNodes.find( centroid ) != mapLocalMBNodes.end() )
        {
            locElem    = true;
            current_eh = mapLocalMBNodes[centroid];
        }

        for( int j = 0; j < nP; j++ )
        {
            for( int i = 0; i < nP; i++ )
            {

                // Get local map vectors
                Node nodeGLL;
                Node dDx1G;
                Node dDx2G;

                // ApplyLocalMap(
                //     face,
                //     nodevec,
                //     dG[i],
                //     dG[j],
                //     nodeGLL,
                //     dDx1G,
                //     dDx2G);
                const double& dAlpha = dG[i];
                const double& dBeta  = dG[j];

                // Calculate nodal locations on the plane
                double dXc = nodes[face[0]].x * ( 1.0 - dAlpha ) * ( 1.0 - dBeta ) +
                             nodes[face[1]].x * dAlpha * ( 1.0 - dBeta ) + nodes[face[2]].x * dAlpha * dBeta +
                             nodes[face[3]].x * ( 1.0 - dAlpha ) * dBeta;

                double dYc = nodes[face[0]].y * ( 1.0 - dAlpha ) * ( 1.0 - dBeta ) +
                             nodes[face[1]].y * dAlpha * ( 1.0 - dBeta ) + nodes[face[2]].y * dAlpha * dBeta +
                             nodes[face[3]].y * ( 1.0 - dAlpha ) * dBeta;

                double dZc = nodes[face[0]].z * ( 1.0 - dAlpha ) * ( 1.0 - dBeta ) +
                             nodes[face[1]].z * dAlpha * ( 1.0 - dBeta ) + nodes[face[2]].z * dAlpha * dBeta +
                             nodes[face[3]].z * ( 1.0 - dAlpha ) * dBeta;

                double dR = sqrt( dXc * dXc + dYc * dYc + dZc * dZc );

                // Mapped node location
                nodeGLL.x = dXc / dR;
                nodeGLL.y = dYc / dR;
                nodeGLL.z = dZc / dR;

                // Determine if this is a unique Node
                std::map< Node, int >::const_iterator iter = mapNodes.find( nodeGLL );
                if( iter == mapNodes.end() )
                {
                    // Insert new unique node into map
                    int ixNode = static_cast< int >( mapNodes.size() );
                    mapNodes.insert( std::pair< Node, int >( nodeGLL, ixNode ) );
                    dataGLLnodes[j][i][k] = ixNode + 1;
                }
                else
                {
                    dataGLLnodes[j][i][k] = iter->second + 1;
                }

                dofIDs[j * nP + i] = dataGLLnodes[j][i][k];
            }
        }

        if( locElem )
        {
            rval = m_interface->tag_set_data( dofTag, &current_eh, 1, dofIDs );MB_CHK_SET_ERR( rval, "Failed to tag_set_data for DoFs" );
        }
    }

    // clear memory
    delete[] dofIDs;
    mapLocalMBNodes.clear();
    mapNodes.clear();

    return moab::MB_SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////////

ErrorCode TempestRemapper::ConstructCoveringSet( double tolerance, double radius_src, double radius_tgt, double boxeps,
                                                 bool regional_mesh )
{
    ErrorCode rval;

    rrmgrids = regional_mesh;
    moab::Range local_verts;

    // Initialize intersection context
    mbintx = new moab::Intx2MeshOnSphere( m_interface );

    mbintx->set_error_tolerance( tolerance );
    mbintx->set_radius_source_mesh( radius_src );
    mbintx->set_radius_destination_mesh( radius_tgt );
    mbintx->set_box_error( boxeps );
#ifdef MOAB_HAVE_MPI
    mbintx->set_parallel_comm( m_pcomm );
#endif

    // compute the maxiumum edges in elements comprising source and target mesh
    rval = mbintx->FindMaxEdges( m_source_set, m_target_set );MB_CHK_ERR( rval );

    this->max_source_edges = mbintx->max_edges_1;
    this->max_target_edges = mbintx->max_edges_2;

    // Note: lots of communication possible, if mesh is distributed very differently
#ifdef MOAB_HAVE_MPI
    if( is_parallel )
    {
        rval = mbintx->build_processor_euler_boxes( m_target_set, local_verts );MB_CHK_ERR( rval );

        rval = m_interface->create_meshset( moab::MESHSET_SET, m_covering_source_set );MB_CHK_SET_ERR( rval, "Can't create new set" );

        rval = mbintx->construct_covering_set( m_source_set, m_covering_source_set );MB_CHK_ERR( rval );
        // if (rank == 1)
        // {
        //     moab::Range ents;
        //     m_interface->get_entities_by_dimension(m_covering_source_set, 2, ents);
        //     m_interface->remove_entities(m_covering_source_set, ents);
        // }
    }
    else
    {
#endif
        if( rrmgrids )
        {
            rval = m_interface->create_meshset( moab::MESHSET_SET, m_covering_source_set );MB_CHK_SET_ERR( rval, "Can't create new set" );

            double tolerance = 1e-6, btolerance = 1e-3;
            moab::AdaptiveKDTree tree( m_interface );
            moab::Range targetVerts;

            rval = m_interface->get_connectivity( m_target_entities, targetVerts, true );MB_CHK_ERR( rval );

            rval = tree.build_tree( m_source_entities, &m_source_set );MB_CHK_ERR( rval );

            for( unsigned ie = 0; ie < targetVerts.size(); ++ie )
            {
                EntityHandle el = targetVerts[ie], leaf;
                double point[3];

                // Get the element centroid to be queried
                rval = m_interface->get_coords( &el, 1, point );MB_CHK_ERR( rval );

                // Search for the closest source element in the master mesh corresponding
                // to the target element centroid in the slave mesh
                rval = tree.point_search( point, leaf, tolerance, btolerance );MB_CHK_ERR( rval );

                if( leaf == 0 )
                {
                    leaf = m_source_set;  // no hint
                }

                std::vector< moab::EntityHandle > leaf_elems;
                // We only care about the dimension that the user specified.
                // MOAB partitions are ordered by elements anyway.
                rval = m_interface->get_entities_by_dimension( leaf, 2, leaf_elems );MB_CHK_ERR( rval );

                if( !leaf_elems.size() )
                {
                    // std::cout << ie << ": " << " No leaf elements found." << std::endl;
                    continue;
                }

                // Now get the master element centroids so that we can compute
                // the minimum distance to the target point
                std::vector< double > centroids( leaf_elems.size() * 3 );
                rval = m_interface->get_coords( &leaf_elems[0], leaf_elems.size(), &centroids[0] );MB_CHK_ERR( rval );

                double dist = 1e5;
                int pinelem = -1;
                for( size_t il = 0; il < leaf_elems.size(); ++il )
                {
                    const double* centroid = &centroids[il * 3];
                    const double locdist   = std::pow( point[0] - centroid[0], 2 ) +
                                           std::pow( point[1] - centroid[1], 2 ) +
                                           std::pow( point[2] - centroid[2], 2 );

                    if( locdist < dist )
                    {
                        dist    = locdist;
                        pinelem = il;
                        m_covering_source_entities.insert( leaf_elems[il] );
                    }
                }

                if( pinelem < 0 )
                {
                    std::cout << ie
                              << ": [Error] - Could not find a minimum distance within the leaf "
                                 "nodes. Dist = "
                              << dist << std::endl;
                }
            }
            // rval = tree.reset_tree();MB_CHK_ERR(rval);
            std::cout << "[INFO] - Total covering source entities = " << m_covering_source_entities.size() << std::endl;
            rval = m_interface->add_entities( m_covering_source_set, m_covering_source_entities );MB_CHK_ERR( rval );
        }
        else
        {
            m_covering_source_set      = m_source_set;
            m_covering_source          = m_source;
            m_covering_source_entities = m_source_entities;  // this is a tempest mesh object; careful about
                                                             // incrementing the reference?
            m_covering_source_vertices = m_source_vertices;  // this is a tempest mesh object; careful about
                                                             // incrementing the reference?
        }
#ifdef MOAB_HAVE_MPI
    }
#endif

    return rval;
}

ErrorCode TempestRemapper::ComputeOverlapMesh( bool kdtree_search, bool use_tempest )
{
    ErrorCode rval;
    const bool outputEnabled = ( this->rank == 0 );
    moab::DebugOutput dbgprint( std::cout, this->rank, 0 );
    dbgprint.set_prefix( "[ComputeOverlapMesh]: " );

    // const double radius = 1.0 /*2.0*acos(-1.0)*/;
    // const double boxeps = 0.1;
    // Create the intersection on the sphere object and set up necessary parameters

    // First, split based on whether to use Tempest or MOAB
    // If Tempest
    //   1) Check for valid Mesh and pointers to objects for source/target
    //   2) Invoke GenerateOverlapWithMeshes routine from Tempest library
    // If MOAB
    //   1) Check for valid source and target meshsets (and entities)
    //   2) Build processor bounding boxes and construct a covering set
    //   3) Perform intersection between the source (covering) and target entities
    if( use_tempest )
    {
        // Now let us construct the overlap mesh, by calling TempestRemap interface directly
        // For the overlap method, choose between: "fuzzy", "exact" or "mixed"
        assert( m_source != NULL );
        assert( m_target != NULL );
        if( m_overlap != NULL ) delete m_overlap;
        m_overlap         = new Mesh();
        bool concaveMeshA = false, concaveMeshB = false;
        int err = GenerateOverlapWithMeshes( *m_covering_source, *m_target, *m_overlap, "" /*outFilename*/, "Netcdf4",
                                             "exact", concaveMeshA, concaveMeshB, false );
        if( err )
        {
            MB_CHK_SET_ERR( MB_FAILURE, "TempestRemap: Can't compute the intersection of meshes on the sphere" );
        }
    }
    else
    {
        Tag gidtag = m_interface->globalId_tag();
        moab::EntityHandle subrange[2];
        int gid[2];
        if( m_source_entities.size() > 1 )
        {  // Let us do some sanity checking to fix ID if they have are setup incorrectly
            subrange[0] = m_source_entities[0];
            subrange[1] = m_source_entities[1];
            rval        = m_interface->tag_get_data( gidtag, subrange, 2, gid );MB_CHK_ERR( rval );

            // Check if we need to impose Global ID numbering for vertices and elements. This may be
            // needed if we load the meshes from exodus or some other formats that may not have a
            // numbering forced.
            if( gid[0] + gid[1] == 0 )  // this implies first two elements have GID = 0
            {
#ifdef MOAB_HAVE_MPI
                rval = m_pcomm->assign_global_ids( m_source_set, 2, 1, false, true, false );MB_CHK_ERR( rval );
#else
                rval = this->assign_vertex_element_IDs( gidtag, m_source_set, 2, 1 );MB_CHK_ERR( rval );
#endif
            }
        }
        if( m_target_entities.size() > 1 )
        {
            subrange[0] = m_target_entities[0];
            subrange[1] = m_target_entities[1];
            rval        = m_interface->tag_get_data( gidtag, subrange, 2, gid );MB_CHK_ERR( rval );

            // Check if we need to impose Global ID numbering for vertices and elements. This may be
            // needed if we load the meshes from exodus or some other formats that may not have a
            // numbering forced.
            if( gid[0] + gid[1] == 0 )  // this implies first two elements have GID = 0
            {
#ifdef MOAB_HAVE_MPI
                rval = m_pcomm->assign_global_ids( m_target_set, 2, 1, false, true, false );MB_CHK_ERR( rval );
#else
                rval = this->assign_vertex_element_IDs( gidtag, m_target_set, 2, 1 );MB_CHK_ERR( rval );
#endif
            }
        }

        // Now perform the actual parallel intersection between the source and the target meshes
        if( kdtree_search )
        {
            if( outputEnabled ) dbgprint.printf( 0, "Computing intersection mesh with the Kd-tree search algorithm" );
            rval = mbintx->intersect_meshes_kdtree( m_covering_source_set, m_target_set, m_overlap_set );MB_CHK_SET_ERR( rval, "Can't compute the intersection of meshes on the sphere with brute-force" );
        }
        else
        {
            if( outputEnabled )
                dbgprint.printf( 0, "Computing intersection mesh with the advancing-front propagation algorithm" );
            rval = mbintx->intersect_meshes( m_covering_source_set, m_target_set, m_overlap_set );MB_CHK_SET_ERR( rval, "Can't compute the intersection of meshes on the sphere" );
        }

#ifdef MOAB_HAVE_MPI
        if( is_parallel || rrmgrids )
        {
#ifdef VERBOSE
            std::stringstream ffc, fft, ffo;
            ffc << "cover_" << rank << ".h5m";
            fft << "target_" << rank << ".h5m";
            ffo << "intx_" << rank << ".h5m";
            rval = m_interface->write_mesh( ffc.str().c_str(), &m_covering_source_set, 1 );MB_CHK_ERR( rval );
            rval = m_interface->write_mesh( fft.str().c_str(), &m_target_set, 1 );MB_CHK_ERR( rval );
            rval = m_interface->write_mesh( ffo.str().c_str(), &m_overlap_set, 1 );MB_CHK_ERR( rval );
#endif
            // because we do not want to work with elements in coverage set that do not participate
            // in intersection, remove them from the coverage set we will not delete them yet, just
            // remove from the set !
            if( !point_cloud_target )
            {
                Range covEnts;
                rval = m_interface->get_entities_by_dimension( m_covering_source_set, 2, covEnts );MB_CHK_ERR( rval );
                Tag gidtag = m_interface->globalId_tag();

                std::map< int, int > loc_gid_to_lid_covsrc;
                std::vector< int > gids( covEnts.size(), -1 );
                rval = m_interface->tag_get_data( gidtag, covEnts, &gids[0] );MB_CHK_ERR( rval );
                for( unsigned ie = 0; ie < gids.size(); ++ie )
                {
                    loc_gid_to_lid_covsrc[gids[ie]] = ie;
                }

                Range intxCov;
                Range intxCells;
                Tag srcParentTag;
                rval = m_interface->tag_get_handle( "SourceParent", srcParentTag );MB_CHK_ERR( rval );
                rval = m_interface->get_entities_by_dimension( m_overlap_set, 2, intxCells );MB_CHK_ERR( rval );
                for( Range::iterator it = intxCells.begin(); it != intxCells.end(); it++ )
                {
                    EntityHandle intxCell = *it;
                    int blueParent        = -1;
                    rval                  = m_interface->tag_get_data( srcParentTag, &intxCell, 1, &blueParent );MB_CHK_ERR( rval );
                    // if (is_root) std::cout << "Found intersecting element: " << blueParent << ",
                    // " << gid_to_lid_covsrc[blueParent] << "\n";
                    assert( blueParent >= 0 );
                    intxCov.insert( covEnts[loc_gid_to_lid_covsrc[blueParent]] );
                }

                Range notNeededCovCells = moab::subtract( covEnts, intxCov );
                    // remove now from coverage set the cells that are not needed
                rval = m_interface->remove_entities( m_covering_source_set, notNeededCovCells );MB_CHK_ERR( rval );
                covEnts = moab::subtract( covEnts, notNeededCovCells );
#ifdef VERBOSE
                std::cout << " total participating elements in the covering set: " << intxCov.size() << "\n";
                std::cout << " remove from coverage set elements that are not intersected: "
                              << notNeededCovCells.size() << "\n";
#endif
                if( size > 1 )
                {
                    // some source elements cover multiple target partitions; the conservation logic
                    // requires to know all overlap elements for a source element; they need to be
                    // communicated from the other target partitions
                    //
                    // so first we have to identify source (coverage) elements that cover multiple
                    // target partitions

                    // we will then mark the source, we will need to migrate the overlap elements
                    // that cover this to the original source for the source element; then
                    // distribute the overlap elements to all processors that have the coverage mesh
                    // used

                    rval = augment_overlap_set();MB_CHK_ERR( rval );
                }
            }

            // m_covering_source = new Mesh();
            // rval = convert_mesh_to_tempest_private ( m_covering_source, m_covering_source_set,
            // m_covering_source_entities, &m_covering_source_vertices ); MB_CHK_SET_ERR ( rval,
            // "Can't convert source Tempest mesh" );
        }
#endif

        // Fix any inconsistencies in the overlap mesh
        {
            IntxAreaUtils areaAdaptor;
            rval = IntxUtils::fix_degenerate_quads( m_interface, m_overlap_set );MB_CHK_ERR( rval );
            rval = areaAdaptor.positive_orientation( m_interface, m_overlap_set, 1.0 /*radius*/ );MB_CHK_ERR( rval );
        }

        // Now let us re-convert the MOAB mesh back to Tempest representation
        rval = this->ComputeGlobalLocalMaps();MB_CHK_ERR( rval );

        rval = this->convert_overlap_mesh_sorted_by_source();MB_CHK_ERR( rval );

        // free the memory
        delete mbintx;
    }

    return MB_SUCCESS;
}

#ifdef MOAB_HAVE_MPI
// this function is called only in parallel
///////////////////////////////////////////////////////////////////////////////////
ErrorCode TempestRemapper::augment_overlap_set()
{
    /*
     * overall strategy:
     *
     * 1) collect all boundary target cells on the current task, affected by the partition boundary;
     *    note: not only partition boundary, we need all boundary (all coastal lines) and partition
     * boundary targetBoundaryIds is the set of target boundary cell IDs
     *
     * 2) collect all source cells that are intersecting boundary cells (call them
     * affectedSourceCellsIds)
     *
     * 3) collect overlap, that is accumulate all overlap cells that have source target in
     * affectedSourceCellsIds
     */
    // first, get all edges on the partition boundary, on the target mesh, then all the target
    // elements that border the partition boundary
    ErrorCode rval;
    Skinner skinner( m_interface );
    Range targetCells, boundaryEdges;
    rval = m_interface->get_entities_by_dimension( m_target_set, 2, targetCells );MB_CHK_ERR( rval );
    /// find all boundary edges
    rval = skinner.find_skin( 0, targetCells, false, boundaryEdges );MB_CHK_ERR( rval );
    // filter boundary edges that are on partition boundary, not on boundary
    // find all cells adjacent to these boundary edges, from target set
    Range boundaryCells;  // these will be filtered from target_set
    rval = m_interface->get_adjacencies( boundaryEdges, 2, false, boundaryCells, Interface::UNION );MB_CHK_ERR( rval );
    boundaryCells = intersect( boundaryCells, targetCells );
#ifdef VERBOSE
    EntityHandle tmpSet;
    rval = m_interface->create_meshset( MESHSET_SET, tmpSet );MB_CHK_SET_ERR( rval, "Can't create temporary set" );
    // add the boundary set and edges, and save it to a file
    rval = m_interface->add_entities( tmpSet, boundaryCells );MB_CHK_SET_ERR( rval, "Can't add entities" );
    rval = m_interface->add_entities( tmpSet, boundaryEdges );MB_CHK_SET_ERR( rval, "Can't add edges" );
    std::stringstream ffs;
    ffs << "boundaryCells_0" << rank << ".h5m";
    rval = m_interface->write_mesh( ffs.str().c_str(), &tmpSet, 1 );MB_CHK_ERR( rval );
#endif

    // now that we have the boundary cells, see which overlap polys have have these as parents;
    //   find the ids of the boundary cells;
    Tag gid = m_interface->globalId_tag();
    std::set< int > targetBoundaryIds;
    for( Range::iterator it = boundaryCells.begin(); it != boundaryCells.end(); it++ )
    {
        int tid;
        EntityHandle targetCell = *it;
        rval                    = m_interface->tag_get_data( gid, &targetCell, 1, &tid );MB_CHK_SET_ERR( rval, "Can't get global id tag on target cell" );
        if( tid < 0 ) std::cout << " incorrect id for a target cell\n";
        targetBoundaryIds.insert( tid );
    }

    Range overlapCells;
    rval = m_interface->get_entities_by_dimension( m_overlap_set, 2, overlapCells );MB_CHK_ERR( rval );

    std::set< int > affectedSourceCellsIds;
    Tag targetParentTag, sourceParentTag;  // do not use blue/red, as it is more confusing
    rval = m_interface->tag_get_handle( "TargetParent", targetParentTag );MB_CHK_ERR( rval );
    rval = m_interface->tag_get_handle( "SourceParent", sourceParentTag );MB_CHK_ERR( rval );
    for( Range::iterator it = overlapCells.begin(); it != overlapCells.end(); it++ )
    {
        EntityHandle intxCell = *it;
        int targetParentID, sourceParentID;
        rval = m_interface->tag_get_data( targetParentTag, &intxCell, 1, &targetParentID );MB_CHK_ERR( rval );
        if( targetBoundaryIds.find( targetParentID ) != targetBoundaryIds.end() )
        {
            // this means that the source element is affected
            rval = m_interface->tag_get_data( sourceParentTag, &intxCell, 1, &sourceParentID );MB_CHK_ERR( rval );
            affectedSourceCellsIds.insert( sourceParentID );
        }
    }

    // now find all source cells affected, based on their id;
    //  (we do not have yet the mapping gid_to_lid_covsrc)
    std::map< int, EntityHandle > affectedCovCellFromID;  // map from source cell id to the eh; it is needed to find out
                                                          // the original processor
    // this one came from , so to know where to send the overlap elements

    // use std::set<EntityHandle> instead of moab::Range for collecting cells, either on coverage or
    // target or intx cells
    std::set< EntityHandle > affectedCovCells;  // their overlap cells will be sent to their
                                                // original task, then distributed to all
    // other processes that might need them to compute conservation

    Range covCells;
    rval = m_interface->get_entities_by_dimension( m_covering_source_set, 2, covCells );MB_CHK_ERR( rval );
    // loop thru all cov cells, to find the ones with global ids in affectedSourceCellsIds
    for( Range::iterator it = covCells.begin(); it != covCells.end(); it++ )
    {
        EntityHandle covCell = *it;  //
        int covID;
        rval = m_interface->tag_get_data( gid, &covCell, 1, &covID );
        if( affectedSourceCellsIds.find( covID ) != affectedSourceCellsIds.end() )
        {
            // this source cell is affected;
            affectedCovCellFromID[covID] = covCell;
            affectedCovCells.insert( covCell );
        }
    }

    // now loop again over all overlap cells, to see if their source parent is "affected"
    // store in ranges the overlap cells that need to be sent to original task of the source cell
    // from there, they will be redistributed to the tasks that need that coverage cell
    Tag sendProcTag;
    rval = m_interface->tag_get_handle( "sending_processor", 1, MB_TYPE_INTEGER, sendProcTag );

    // basically a map from original processor task to the set of overlap cells to be sent there
    std::map< int, std::set< EntityHandle > > overlapCellsForTask;
    // this set will contain all intx cells that will need to be sent ( a union of above sets ,
    //   that are organized per task on the above map )
    std::set< EntityHandle > overlapCellsToSend;

    for( Range::iterator it = overlapCells.begin(); it != overlapCells.end(); it++ )
    {
        EntityHandle intxCell = *it;
        int sourceParentID;
        rval = m_interface->tag_get_data( sourceParentTag, &intxCell, 1, &sourceParentID );MB_CHK_ERR( rval );
        if( affectedSourceCellsIds.find( sourceParentID ) != affectedSourceCellsIds.end() )
        {
            EntityHandle covCell = affectedCovCellFromID[sourceParentID];
            int orgTask;
            rval = m_interface->tag_get_data( sendProcTag, &covCell, 1, &orgTask );MB_CHK_ERR( rval );
            overlapCellsForTask[orgTask].insert(
                intxCell );                         // put the overlap cell in corresponding range (set<EntityHandle>)
            overlapCellsToSend.insert( intxCell );  // also put it in this range, for debugging mostly
        }
    }

    // now prepare to send; will use crystal router, as the buffers in ParComm are prepared only
    // for neighbors; coverage mesh was also migrated with crystal router, so here we go again :(

    // find out the maximum number of edges of the polygons needed to be sent
    // we could we conservative and use a big number, or the number from intx, if we store it then?
    int maxEdges = 0;
    for( std::set< EntityHandle >::iterator it = overlapCellsToSend.begin(); it != overlapCellsToSend.end(); it++ )
    {
        EntityHandle intxCell = *it;
        int nnodes;
        const EntityHandle* conn;
        rval = m_interface->get_connectivity( intxCell, conn, nnodes );MB_CHK_ERR( rval );
        if( maxEdges < nnodes ) maxEdges = nnodes;
    }

    // find the maximum among processes in intersection
    int globalMaxEdges;
    if( m_pcomm )
        MPI_Allreduce( &maxEdges, &globalMaxEdges, 1, MPI_INT, MPI_MAX, m_pcomm->comm() );
    else
        globalMaxEdges = maxEdges;

#ifdef VERBOSE
    if( is_root ) std::cout << "maximum number of edges for polygons to send is " << globalMaxEdges << "\n";
#endif

#ifdef VERBOSE
    EntityHandle tmpSet2;
    rval = m_interface->create_meshset( MESHSET_SET, tmpSet2 );MB_CHK_SET_ERR( rval, "Can't create temporary set2" );
    // add the affected source and overlap elements
    for( std::set< EntityHandle >::iterator it = overlapCellsToSend.begin(); it != overlapCellsToSend.end(); it++ )
    {
        EntityHandle intxCell = *it;
        rval                  = m_interface->add_entities( tmpSet2, &intxCell, 1 );MB_CHK_SET_ERR( rval, "Can't add entities" );
    }
    for( std::set< EntityHandle >::iterator it = affectedCovCells.begin(); it != affectedCovCells.end(); it++ )
    {
        EntityHandle covCell = *it;
        rval                 = m_interface->add_entities( tmpSet2, &covCell, 1 );MB_CHK_SET_ERR( rval, "Can't add entities" );
    }
    std::stringstream ffs2;
    // these will contain coverage cells and intx cells on the boundary
    ffs2 << "affectedCells_" << m_pcomm->rank() << ".h5m";
    rval = m_interface->write_mesh( ffs2.str().c_str(), &tmpSet2, 1 );MB_CHK_ERR( rval );
#endif
    // form tuple lists to send vertices and cells;
    // the problem is that the lists of vertices will need to have other information, like the
    // processor it comes from, and its index in that list; we may have to duplicate vertices, but
    // we do not care much; we will not duplicate overlap elements, just the vertices, as they may
    // come from different cells and different processes each vertex will have a local index and a
    // processor task it is coming from

    // look through the std::set's to be sent to other processes, and form the vertex tuples and
    // cell tuples
    //
    std::map< int, std::set< EntityHandle > > verticesOverlapForTask;
    // Range allVerticesToSend;
    std::set< EntityHandle > allVerticesToSend;
    std::map< EntityHandle, int > allVerticesToSendMap;
    int numVerts        = 0;
    int numOverlapCells = 0;
    for( std::map< int, std::set< EntityHandle > >::iterator it = overlapCellsForTask.begin();
         it != overlapCellsForTask.end(); it++ )
    {
        int sendToProc                                = it->first;
        std::set< EntityHandle >& overlapCellsToSend2 = it->second;  // organize vertices in std::set per processor
        // Range vertices;
        std::set< EntityHandle > vertices;  // collect all vertices connected to overlapCellsToSend2
        for( std::set< EntityHandle >::iterator set_it = overlapCellsToSend2.begin();
             set_it != overlapCellsToSend2.end(); ++set_it )
        {
            int nnodes_local          = 0;
            const EntityHandle* conn1 = NULL;
            rval                      = m_interface->get_connectivity( *set_it, conn1, nnodes_local );MB_CHK_ERR( rval );
            for( int k = 0; k < nnodes_local; k++ )
                vertices.insert( conn1[k] );
        }
        verticesOverlapForTask[sendToProc] = vertices;
        numVerts += (int)vertices.size();
        numOverlapCells += (int)overlapCellsToSend2.size();
        allVerticesToSend.insert( vertices.begin(), vertices.end() );
    }
    // build the index map, from entity handle to index in all vert set
    int j = 0;
    for( std::set< EntityHandle >::iterator vert_it = allVerticesToSend.begin(); vert_it != allVerticesToSend.end();
         vert_it++, j++ )
    {
        EntityHandle vert          = *vert_it;
        allVerticesToSendMap[vert] = j;
    }

    // first send vertices in a tuple list, then send overlap cells, according to requests
    // overlap cells need to send info about the blue and red parent tags, too
    TupleList TLv;                           //
    TLv.initialize( 2, 0, 0, 3, numVerts );  // to proc, index in all range, DP points
    TLv.enableWriteAccess();

    for( std::map< int, std::set< EntityHandle > >::iterator it = verticesOverlapForTask.begin();
         it != verticesOverlapForTask.end(); it++ )
    {
        int sendToProc                     = it->first;
        std::set< EntityHandle >& vertices = it->second;
        int i                              = 0;
        for( std::set< EntityHandle >::iterator it2 = vertices.begin(); it2 != vertices.end(); it2++, i++ )
        {
            int n                = TLv.get_n();
            TLv.vi_wr[2 * n]     = sendToProc;  // send to processor
            EntityHandle v       = *it2;
            int indexInAllVert   = allVerticesToSendMap[v];
            TLv.vi_wr[2 * n + 1] = indexInAllVert;  // will be orgProc, to differentiate indices
                                                    // of vertices sent to "sentToProc"
            double coords[3];
            rval = m_interface->get_coords( &v, 1, coords );MB_CHK_ERR( rval );
            TLv.vr_wr[3 * n]     = coords[0];  // departure position, of the node local_verts[i]
            TLv.vr_wr[3 * n + 1] = coords[1];
            TLv.vr_wr[3 * n + 2] = coords[2];
            TLv.inc_n();
        }
    }

    TupleList TLc;
    int sizeTuple = 4 + globalMaxEdges;
    // total number of overlap cells to send
    TLc.initialize( sizeTuple, 0, 0, 0,
                    numOverlapCells );  // to proc, blue parent ID, red parent ID, nvert,
                                        // connectivity[globalMaxEdges] (global ID v), local eh)
    TLc.enableWriteAccess();

    for( std::map< int, std::set< EntityHandle > >::iterator it = overlapCellsForTask.begin();
         it != overlapCellsForTask.end(); it++ )
    {
        int sendToProc                                = it->first;
        std::set< EntityHandle >& overlapCellsToSend2 = it->second;
        // send also the target and source parents for these overlap cells
        for( std::set< EntityHandle >::iterator it2 = overlapCellsToSend2.begin(); it2 != overlapCellsToSend2.end();
             it2++ )
        {
            EntityHandle intxCell = *it2;
            int sourceParentID, targetParentID;
            rval = m_interface->tag_get_data( targetParentTag, &intxCell, 1, &targetParentID );MB_CHK_ERR( rval );
            rval = m_interface->tag_get_data( sourceParentTag, &intxCell, 1, &sourceParentID );MB_CHK_ERR( rval );
            int n                        = TLc.get_n();
            TLc.vi_wr[sizeTuple * n]     = sendToProc;
            TLc.vi_wr[sizeTuple * n + 1] = sourceParentID;
            TLc.vi_wr[sizeTuple * n + 2] = targetParentID;
            int nnodes;
            const EntityHandle* conn = NULL;
            rval                     = m_interface->get_connectivity( intxCell, conn, nnodes );MB_CHK_ERR( rval );
            TLc.vi_wr[sizeTuple * n + 3] = nnodes;
            for( int i = 0; i < nnodes; i++ )
            {
                int indexVertex = allVerticesToSendMap[conn[i]];
                ;  // the vertex index will be now unique per original proc
                if( -1 == indexVertex ) MB_CHK_SET_ERR( MB_FAILURE, "Can't find vertex in range of vertices to send" );
                TLc.vi_wr[sizeTuple * n + 4 + i] = indexVertex;
            }
            // fill the rest with 0, just because we do not like uninitialized data
            for( int i = nnodes; i < globalMaxEdges; i++ )
                TLc.vi_wr[sizeTuple * n + 4 + i] = 0;

            TLc.inc_n();
        }
    }

    // send first the vertices and overlap cells to original task for coverage cells
    // now we are done populating the tuples; route them to the appropriate processors
#ifdef VERBOSE
    std::stringstream ff1;
    ff1 << "TLc_" << rank << ".txt";
    TLc.print_to_file( ff1.str().c_str() );
    std::stringstream ffv;
    ffv << "TLv_" << rank << ".txt";
    TLv.print_to_file( ffv.str().c_str() );
#endif
    ( m_pcomm->proc_config().crystal_router() )->gs_transfer( 1, TLv, 0 );
    ( m_pcomm->proc_config().crystal_router() )->gs_transfer( 1, TLc, 0 );

#ifdef VERBOSE
    TLc.print_to_file( ff1.str().c_str() );  // will append to existing file
    TLv.print_to_file( ffv.str().c_str() );
#endif
    // first phase of transfer complete
    // now look at TLc, and sort by the source parent (index 1)

    TupleList::buffer buffer;
    buffer.buffer_init( sizeTuple * TLc.get_n() * 2 );  // allocate memory for sorting !! double
    TLc.sort( 1, &buffer );
#ifdef VERBOSE
    TLc.print_to_file( ff1.str().c_str() );
#endif

    // will keep a map with vertices per processor that will need to be used in TLv2;
    // so, availVertexIndicesPerProcessor[proc] is a map from vertex indices that are available from
    // this processor to the index in the local TLv; the used vertices will have to be sent to the
    // tasks that need them

    // connectivity of a cell is given by sending proc and index in original list of vertices from
    // that proc

    std::map< int, std::map< int, int > > availVertexIndicesPerProcessor;
    int nv = TLv.get_n();
    for( int i = 0; i < nv; i++ )
    {
        // int proc=TLv.vi_rd[3*i]; // it is coming from this processor
        int orgProc   = TLv.vi_rd[2 * i];  // this is the original processor, for index vertex consideration
        int indexVert = TLv.vi_rd[2 * i + 1];
        availVertexIndicesPerProcessor[orgProc][indexVert] = i;
    }

    // now we have sorted the incoming overlap elements by the source element;
    // if we have overlap elements for one source coming from 2 or more processes, we need to send
    // back to the processes that do not have that overlap cell;

    // form new TLc2, TLv2, that will be distributed to necessary processes
    // first count source elements that are "spread" over multiple processes
    // TLc is ordered now by source ID; loop over them
    int n = TLc.get_n();  // total number of overlap elements received on current task;
    std::map< int, int > currentProcsCount;
    // form a map from proc to sets of vertex indices that will be sent using TLv2
    // will form a map between a source cell ID and tasks/targets that are partially overlapped by
    // these sources
    std::map< int, std::set< int > > sourcesForTasks;
    int sizeOfTLc2 = 0;  // only increase when we will have to send data
    if( n > 0 )
    {
        int currentSourceID      = TLc.vi_rd[sizeTuple * 0 + 1];  // we  have written sizeTuple*0 for "clarity"
        int proc0                = TLc.vi_rd[sizeTuple * 0];
        currentProcsCount[proc0] = 1;  //

        for( int i = 1; i < n; i++ )
        {
            int proc     = TLc.vi_rd[sizeTuple * i];
            int sourceID = TLc.vi_rd[sizeTuple * i + 1];
            if( sourceID == currentSourceID )
            {
                if( currentProcsCount.find( proc ) == currentProcsCount.end() )
                {
                    currentProcsCount[proc] = 1;
                }
                else
                    currentProcsCount[proc]++;
            }
            if( sourceID != currentSourceID || ( ( n - 1 ) == i ) )  // we study the current source if we reach the last
            {
                // we have found a new source id, need to reset the proc counts, and establish if we
                // need to send data
                if( currentProcsCount.size() > 1 )
                {
#ifdef VERBOSE
                    std::cout << " source element " << currentSourceID << " intersects with "
                              << currentProcsCount.size() << " target partitions\n";
                    for( std::map< int, int >::iterator it = currentProcsCount.begin(); it != currentProcsCount.end();
                         it++ )
                    {
                        int procID       = it->first;
                        int numOverCells = it->second;
                        std::cout << "   task:" << procID << " " << numOverCells << " cells\n";
                    }

#endif
                    // estimate what we need to send
                    for( std::map< int, int >::iterator it1 = currentProcsCount.begin(); it1 != currentProcsCount.end();
                         it1++ )
                    {
                        int proc1 = it1->first;
                        sourcesForTasks[currentSourceID].insert( proc1 );
                        for( std::map< int, int >::iterator it2 = currentProcsCount.begin();
                             it2 != currentProcsCount.end(); it2++ )
                        {
                            int proc2 = it2->first;
                            if( proc1 != proc2 ) sizeOfTLc2 += it2->second;
                        }
                    }
                    // mark vertices in TLv tuple that need to be sent
                }
                if( sourceID != currentSourceID )  // maybe we are not at the end, so continue on
                {
                    currentSourceID = sourceID;
                    currentProcsCount.clear();
                    currentProcsCount[proc] = 1;
                }
            }
        }
    }
    // begin a loop to send the needed cells to the processes; also mark the vertices that need to
    // be sent, put them in a set

#ifdef VERBOSE
    std::cout << " need to initialize TLc2 with " << sizeOfTLc2 << " cells\n ";
#endif

    TupleList TLc2;
    int sizeTuple2 = 5 + globalMaxEdges;  // send to, original proc for intx cell, source parent id,
                                          // target parent id,
    // number of vertices, then connectivity in terms of indices in vertex lists from original proc
    TLc2.initialize( sizeTuple2, 0, 0, 0, sizeOfTLc2 );
    TLc2.enableWriteAccess();
    // loop again through TLc, and select intx cells that have the problem sources;

    std::map< int, std::set< int > > verticesToSendForProc;  // will look at indices in the TLv list
    // will form for each processor, the index list from TLv
    for( int i = 0; i < n; i++ )
    {
        int sourceID = TLc.vi_rd[sizeTuple * i + 1];
        if( sourcesForTasks.find( sourceID ) != sourcesForTasks.end() )
        {
            // it means this intx cell needs to be sent to any proc that is not "original" to it
            std::set< int > procs = sourcesForTasks[sourceID];  // set of processors involved with this source
            if( procs.size() < 2 ) MB_CHK_SET_ERR( MB_FAILURE, " not enough processes involved with a sourceID cell" );

            int orgProc = TLc.vi_rd[sizeTuple * i];  // this intx cell was sent from this orgProc, originally
            // will need to be sent to all other procs from above set; also, need to mark the vertex
            // indices for that proc, and check that they are available to populate TLv2
            std::map< int, int >& availableVerticesFromThisProc = availVertexIndicesPerProcessor[orgProc];
            for( std::set< int >::iterator setIt = procs.begin(); setIt != procs.end(); setIt++ )
            {
                int procID = *setIt;
                // send this cell to the other processors, not to orgProc this cell is coming from

                if( procID != orgProc )
                {
                    // send the cell to this processor;
                    int n2 = TLc2.get_n();
                    if( n2 >= sizeOfTLc2 ) MB_CHK_SET_ERR( MB_FAILURE, " memory overflow" );
                    //
                    std::set< int >& indexVerticesInTLv = verticesToSendForProc[procID];
                    TLc2.vi_wr[n2 * sizeTuple2]         = procID;    // send to
                    TLc2.vi_wr[n2 * sizeTuple2 + 1]     = orgProc;   // this cell is coming from here
                    TLc2.vi_wr[n2 * sizeTuple2 + 2]     = sourceID;  // source parent of the intx cell
                    TLc2.vi_wr[n2 * sizeTuple2 + 3] =
                        TLc.vi_rd[sizeTuple * i + 2];  // target parent of the intx cell
                                                       // number of vertices of the intx cell
                    int nvert                       = TLc.vi_rd[sizeTuple * i + 3];
                    TLc2.vi_wr[n2 * sizeTuple2 + 4] = nvert;
                    // now loop through the connectivity, and make sure the vertices are available;
                    // mark them, to populate later the TLv2 tuple list

                    // just copy the vertices, including 0 ones
                    for( int j = 0; j < nvert; j++ )
                    {
                        int vertexIndex = TLc.vi_rd[i * sizeTuple + 4 + j];
                        // is this vertex available from org proc?
                        if( availableVerticesFromThisProc.find( vertexIndex ) == availableVerticesFromThisProc.end() )
                        {
                            MB_CHK_SET_ERR( MB_FAILURE, " vertex index not available from processor" );
                        }
                        TLc2.vi_wr[n2 * sizeTuple2 + 5 + j] = vertexIndex;
                        int indexInTLv                      = availVertexIndicesPerProcessor[orgProc][vertexIndex];
                        indexVerticesInTLv.insert( indexInTLv );
                    }

                    for( int j = nvert; j < globalMaxEdges; j++ )
                    {
                        TLc2.vi_wr[n2 * sizeTuple2 + 5 + j] = 0;  // or mark them 0
                    }
                    TLc2.inc_n();
                }
            }
        }
    }

    // now we have to populate TLv2, with original source proc, index of vertex, and coordinates
    // from TLv use the verticesToSendForProc sets from above, and the map from index in proc to the
    // index in TLv
    TupleList TLv2;
    int numVerts2 = 0;
    // how many vertices to send?
    for( std::map< int, std::set< int > >::iterator it = verticesToSendForProc.begin();
         it != verticesToSendForProc.end(); it++ )
    {
        std::set< int >& indexInTLvSet = it->second;
        numVerts2 += (int)indexInTLvSet.size();
    }
    TLv2.initialize( 3, 0, 0, 3,
                     numVerts2 );  // send to, original proc, index in original proc, and 3 coords
    TLv2.enableWriteAccess();
    for( std::map< int, std::set< int > >::iterator it = verticesToSendForProc.begin();
         it != verticesToSendForProc.end(); it++ )
    {
        int sendToProc                 = it->first;
        std::set< int >& indexInTLvSet = it->second;
        // now, look at indices in TLv, to find out the original proc, and the index in that list
        for( std::set< int >::iterator itSet = indexInTLvSet.begin(); itSet != indexInTLvSet.end(); itSet++ )
        {
            int indexInTLv           = *itSet;
            int orgProc              = TLv.vi_rd[2 * indexInTLv];
            int indexVertexInOrgProc = TLv.vi_rd[2 * indexInTLv + 1];
            int nv2                  = TLv2.get_n();
            TLv2.vi_wr[3 * nv2]      = sendToProc;
            TLv2.vi_wr[3 * nv2 + 1]  = orgProc;
            TLv2.vi_wr[3 * nv2 + 2]  = indexVertexInOrgProc;
            for( int j = 0; j < 3; j++ )
                TLv2.vr_wr[3 * nv2 + j] =
                    TLv.vr_rd[3 * indexInTLv + j];  // departure position, of the node local_verts[i]
            TLv2.inc_n();
        }
    }
    // now, finally, transfer the vertices and the intx cells;
    ( m_pcomm->proc_config().crystal_router() )->gs_transfer( 1, TLv2, 0 );
    ( m_pcomm->proc_config().crystal_router() )->gs_transfer( 1, TLc2, 0 );
    // now, look at vertices from TLv2, and create them
    // we should have in TLv2 only vertices with orgProc different from current task
#ifdef VERBOSE
    std::stringstream ff2;
    ff2 << "TLc2_" << rank << ".txt";
    TLc2.print_to_file( ff2.str().c_str() );
    std::stringstream ffv2;
    ffv2 << "TLv2_" << rank << ".txt";
    TLv2.print_to_file( ffv2.str().c_str() );
#endif
    // first create vertices, and make a map from origin processor, and index, to entity handle
    // (index in TLv2 )
    Tag ghostTag;
    int orig_proc = -1;
    rval          = m_interface->tag_get_handle( "ORIG_PROC", 1, MB_TYPE_INTEGER, ghostTag, MB_TAG_DENSE | MB_TAG_CREAT,
                                        &orig_proc );MB_CHK_ERR( rval );

    int nvNew = TLv2.get_n();
    // if number of vertices to be created is 0, it means there is no need of ghost intx cells,
    // because everything matched perfectly (it can happen in manufactured cases)
    if( 0 == nvNew ) return MB_SUCCESS;
    // create a vertex h for each coordinate
    Range newVerts;
    rval = m_interface->create_vertices( &( TLv2.vr_rd[0] ), nvNew, newVerts );MB_CHK_ERR( rval );
    // now create a map from index , org proc, to actual entity handle corresponding to it
    std::map< int, std::map< int, EntityHandle > > vertexPerProcAndIndex;
    for( int i = 0; i < nvNew; i++ )
    {
        int orgProc                                 = TLv2.vi_rd[3 * i + 1];
        int indexInVert                             = TLv2.vi_rd[3 * i + 2];
        vertexPerProcAndIndex[orgProc][indexInVert] = newVerts[i];
    }

    // new polygons will receive a dense tag, with default value -1, with the processor task they
    // originally belonged to

    // now form the needed cells, in order
    Range newPolygons;
    int ne = TLc2.get_n();
    for( int i = 0; i < ne; i++ )
    {
        int orgProc  = TLc2.vi_rd[i * sizeTuple2 + 1];  // this cell is coming from here, originally
        int sourceID = TLc2.vi_rd[i * sizeTuple2 + 2];  // source parent of the intx cell
        int targetID = TLc2.vi_wr[i * sizeTuple2 + 3];  // target parent of intx cell
        int nve      = TLc2.vi_wr[i * sizeTuple2 + 4];  // number of vertices for the polygon
        std::vector< EntityHandle > conn;
        conn.resize( nve );
        for( int j = 0; j < nve; j++ )
        {
            int indexV      = TLc2.vi_wr[i * sizeTuple2 + 5 + j];
            EntityHandle vh = vertexPerProcAndIndex[orgProc][indexV];
            conn[j]         = vh;
        }
        EntityHandle polyNew;
        rval = m_interface->create_element( MBPOLYGON, &conn[0], nve, polyNew );MB_CHK_ERR( rval );
        newPolygons.insert( polyNew );
        rval = m_interface->tag_set_data( targetParentTag, &polyNew, 1, &targetID );MB_CHK_ERR( rval );
        rval = m_interface->tag_set_data( sourceParentTag, &polyNew, 1, &sourceID );MB_CHK_ERR( rval );
        rval = m_interface->tag_set_data( ghostTag, &polyNew, 1, &orgProc );MB_CHK_ERR( rval );
    }

#ifdef VERBOSE
    EntityHandle tmpSet3;
    rval = m_interface->create_meshset( MESHSET_SET, tmpSet3 );MB_CHK_SET_ERR( rval, "Can't create temporary set3" );
    // add the boundary set and edges, and save it to a file
    rval = m_interface->add_entities( tmpSet3, newPolygons );MB_CHK_SET_ERR( rval, "Can't add entities" );

    std::stringstream ffs4;
    ffs4 << "extraIntxCells" << rank << ".h5m";
    rval = m_interface->write_mesh( ffs4.str().c_str(), &tmpSet3, 1 );MB_CHK_ERR( rval );
#endif

    // add the new polygons to the overlap set
    // these will be ghosted, so will participate in conservation only
    rval = m_interface->add_entities( m_overlap_set, newPolygons );MB_CHK_ERR( rval );
    return MB_SUCCESS;
}

#endif

}  // namespace moab
