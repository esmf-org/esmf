/*
 * Intx2Mesh.cpp
 *
 *  Created on: Oct 2, 2012
 */

#include "moab/IntxMesh/Intx2Mesh.hpp"
#ifdef MOAB_HAVE_MPI
#include "moab/ParallelComm.hpp"
#include "MBParallelConventions.h"
#include "moab/ParallelMergeMesh.hpp"
#endif /* MOAB_HAVE_MPI */
#include "MBTagConventions.hpp"
// this is for DBL_MAX
#include <cfloat>
#include <queue>
#include <sstream>
#include "moab/GeomUtil.hpp"
#include "moab/AdaptiveKDTree.hpp"

namespace moab
{

#ifdef ENABLE_DEBUG
int Intx2Mesh::dbg_1 = 0;
#endif

Intx2Mesh::Intx2Mesh( Interface* mbimpl )
    : mb( mbimpl ), mbs1( 0 ), mbs2( 0 ), outSet( 0 ), gid( 0 ), TgtFlagTag( 0 ), tgtParentTag( 0 ), srcParentTag( 0 ),
      countTag( 0 ), srcNeighTag( 0 ), tgtNeighTag( 0 ), neighTgtEdgeTag( 0 ), orgSendProcTag( 0 ), tgtConn( NULL ),
      srcConn( NULL ), epsilon_1( 0.0 ), epsilon_area( 0.0 ), box_error( 0.0 ), localRoot( 0 ), my_rank( 0 )
#ifdef MOAB_HAVE_MPI
      ,
      parcomm( NULL ), remote_cells( NULL ), remote_cells_with_tracers( NULL )
#endif
      ,
      max_edges_1( 0 ), max_edges_2( 0 ), counting( 0 )
{
    gid = mbimpl->globalId_tag();
}

Intx2Mesh::~Intx2Mesh()
{
    // TODO Auto-generated destructor stub
#ifdef MOAB_HAVE_MPI
    if( remote_cells )
    {
        delete remote_cells;
        remote_cells = NULL;
    }
#endif
}
ErrorCode Intx2Mesh::FindMaxEdgesInSet( EntityHandle eset, int& max_edges )
{
    Range cells;
    ErrorCode rval = mb->get_entities_by_dimension( eset, 2, cells );MB_CHK_ERR( rval );

    max_edges = 0;  // can be 0 for point clouds
    for( Range::iterator cit = cells.begin(); cit != cells.end(); cit++ )
    {
        EntityHandle cell = *cit;
        const EntityHandle* conn4;
        int nnodes = 3;
        rval       = mb->get_connectivity( cell, conn4, nnodes );MB_CHK_SET_ERR( rval, "can't get connectivity of a cell" );
        if( nnodes > max_edges ) max_edges = nnodes;
    }
    // if in parallel, communicate the actual max_edges; it is not needed for tgt mesh (to be
    // global) but it is better to be consistent
#ifdef MOAB_HAVE_MPI
    if( parcomm )
    {
        int local_max_edges = max_edges;
        // now reduce max_edges over all processors
        int mpi_err =
            MPI_Allreduce( &local_max_edges, &max_edges, 1, MPI_INT, MPI_MAX, parcomm->proc_config().proc_comm() );
        if( MPI_SUCCESS != mpi_err ) return MB_FAILURE;
    }
#endif

    return MB_SUCCESS;
}
ErrorCode Intx2Mesh::FindMaxEdges( EntityHandle set1, EntityHandle set2 )
{
    ErrorCode rval = FindMaxEdgesInSet( set1, max_edges_1 );MB_CHK_SET_ERR( rval, "can't determine max_edges in set 1" );
    rval = FindMaxEdgesInSet( set2, max_edges_2 );MB_CHK_SET_ERR( rval, "can't determine max_edges in set 2" );

    return MB_SUCCESS;
}

ErrorCode Intx2Mesh::createTags()
{
    if( tgtParentTag ) mb->tag_delete( tgtParentTag );
    if( srcParentTag ) mb->tag_delete( srcParentTag );
    if( countTag ) mb->tag_delete( countTag );

    unsigned char def_data_bit = 0;  // unused by default
    // maybe the tgt tag is better to be deleted every time, and recreated;
    // or is it easy to set all values to something again? like 0?
    ErrorCode rval = mb->tag_get_handle( "tgtFlag", 1, MB_TYPE_BIT, TgtFlagTag, MB_TAG_CREAT, &def_data_bit );MB_CHK_SET_ERR( rval, "can't get tgt flag tag" );

    // create tgt edges if they do not exist yet; so when they are looked upon, they are found
    // this is the only call that is potentially NlogN, in the whole method
    rval = mb->get_adjacencies( rs2, 1, true, TgtEdges, Interface::UNION );MB_CHK_SET_ERR( rval, "can't get adjacent tgt edges" );

    // now, create a map from each edge to a list of potential new nodes on a tgt edge
    // this memory has to be cleaned up
    // change it to a vector, and use the index in range of tgt edges
    int indx = 0;
    extraNodesVec.resize( TgtEdges.size() );
    for( Range::iterator eit = TgtEdges.begin(); eit != TgtEdges.end(); ++eit, indx++ )
    {
        std::vector< EntityHandle >* nv = new std::vector< EntityHandle >;
        extraNodesVec[indx]             = nv;
    }

    int defaultInt = -1;

    rval = mb->tag_get_handle( "TargetParent", 1, MB_TYPE_INTEGER, tgtParentTag, MB_TAG_DENSE | MB_TAG_CREAT,
                               &defaultInt );MB_CHK_SET_ERR( rval, "can't create positive tag" );

    rval = mb->tag_get_handle( "SourceParent", 1, MB_TYPE_INTEGER, srcParentTag, MB_TAG_DENSE | MB_TAG_CREAT,
                               &defaultInt );MB_CHK_SET_ERR( rval, "can't create negative tag" );

    rval = mb->tag_get_handle( "Counting", 1, MB_TYPE_INTEGER, countTag, MB_TAG_DENSE | MB_TAG_CREAT, &defaultInt );MB_CHK_SET_ERR( rval, "can't create Counting tag" );

    // for each cell in set 1, determine its neigh in set 1 (could be null too)
    // for each cell in set 2, determine its neigh in set 2 (if on boundary, could be 0)
    rval = DetermineOrderedNeighbors( mbs1, max_edges_1, srcNeighTag );MB_CHK_SET_ERR( rval, "can't determine neighbors for set 1" );
    rval = DetermineOrderedNeighbors( mbs2, max_edges_2, tgtNeighTag );MB_CHK_SET_ERR( rval, "can't determine neighbors for set 2" );

    // for tgt cells, save a dense tag with the bordering edges, so we do not have to search for
    // them each time edges were for sure created before (tgtEdges)
    std::vector< EntityHandle > zeroh( max_edges_2, 0 );
    // if we have a tag with this name, it could be of a different size, so delete it
    rval = mb->tag_get_handle( "__tgtEdgeNeighbors", neighTgtEdgeTag );
    if( rval == MB_SUCCESS && neighTgtEdgeTag ) mb->tag_delete( neighTgtEdgeTag );
    rval = mb->tag_get_handle( "__tgtEdgeNeighbors", max_edges_2, MB_TYPE_HANDLE, neighTgtEdgeTag,
                               MB_TAG_DENSE | MB_TAG_CREAT, &zeroh[0] );MB_CHK_SET_ERR( rval, "can't create tgt edge neighbors tag" );
    for( Range::iterator rit = rs2.begin(); rit != rs2.end(); rit++ )
    {
        EntityHandle tgtCell = *rit;
        int num_nodes        = 0;
        rval                 = mb->get_connectivity( tgtCell, tgtConn, num_nodes );MB_CHK_SET_ERR( rval, "can't get  tgt conn" );
        // account for padded polygons
        while( tgtConn[num_nodes - 2] == tgtConn[num_nodes - 1] && num_nodes > 3 )
            num_nodes--;

        int i = 0;
        for( i = 0; i < num_nodes; i++ )
        {
            EntityHandle v[2] = { tgtConn[i],
                                  tgtConn[( i + 1 ) % num_nodes] };  // this is fine even for padded polygons
            std::vector< EntityHandle > adj_entities;
            rval = mb->get_adjacencies( v, 2, 1, false, adj_entities, Interface::INTERSECT );
            if( rval != MB_SUCCESS || adj_entities.size() < 1 ) return rval;  // get out , big error
            zeroh[i] = adj_entities[0];                                       // should be only one edge between 2 nodes
            // also, even if number of edges is less than max_edges_2, they will be ignored, even if
            // the tag is dense
        }
        // now set the value of the tag
        rval = mb->tag_set_data( neighTgtEdgeTag, &tgtCell, 1, &( zeroh[0] ) );MB_CHK_SET_ERR( rval, "can't set edge tgt tag" );
    }
    return MB_SUCCESS;
}

ErrorCode Intx2Mesh::DetermineOrderedNeighbors( EntityHandle inputSet, int max_edges, Tag& neighTag )
{
    Range cells;
    ErrorCode rval = mb->get_entities_by_dimension( inputSet, 2, cells );MB_CHK_SET_ERR( rval, "can't get cells in set" );

    std::vector< EntityHandle > neighbors( max_edges );
    std::vector< EntityHandle > zeroh( max_edges, 0 );
    // nameless tag, as the name is not important; we will have 2 related tags, but one on tgt mesh,
    // one on src mesh
    rval = mb->tag_get_handle( "", max_edges, MB_TYPE_HANDLE, neighTag, MB_TAG_DENSE | MB_TAG_CREAT, &zeroh[0] );MB_CHK_SET_ERR( rval, "can't create neighbors tag" );

    for( Range::iterator cit = cells.begin(); cit != cells.end(); cit++ )
    {
        EntityHandle cell = *cit;
        int nnodes        = 3;
        // will get the nnodes ordered neighbors;
        // first cell is for nodes 0, 1, second to 1, 2, third to 2, 3, last to nnodes-1,
        const EntityHandle* conn4;
        rval = mb->get_connectivity( cell, conn4, nnodes );MB_CHK_SET_ERR( rval, "can't get connectivity of a cell" );
        int nsides = nnodes;
        // account for possible padded polygons
        while( conn4[nsides - 2] == conn4[nsides - 1] && nsides > 3 )
            nsides--;

        for( int i = 0; i < nsides; i++ )
        {
            EntityHandle v[2];
            v[0] = conn4[i];
            v[1] = conn4[( i + 1 ) % nsides];
            // get all cells adjacent to these 2 vertices on the edge
            std::vector< EntityHandle > adjcells;
            std::vector< EntityHandle > cellsInSet;
            rval = mb->get_adjacencies( v, 2, 2, false, adjcells, Interface::INTERSECT );MB_CHK_SET_ERR( rval, "can't adjacency to 2 verts" );
            // now look for the cells contained in the input set;
            // the input set should be a correct mesh, not overlapping cells, and manifold
            size_t siz = adjcells.size();
            for( size_t j = 0; j < siz; j++ )
                if( mb->contains_entities( inputSet, &( adjcells[j] ), 1 ) ) cellsInSet.push_back( adjcells[j] );
            siz = cellsInSet.size();

            if( siz > 2 )
            {
                std::cout << "non manifold mesh, error" << mb->list_entities( &( cellsInSet[0] ), cellsInSet.size() )
                          << "\n";MB_CHK_SET_ERR( MB_FAILURE, "non-manifold input mesh set" );  // non-manifold
            }
            if( siz == 1 )
            {
                // it must be the border of the input mesh;
                neighbors[i] = 0;  // we are guaranteed that ids are !=0; this is marking a border
                // borders do not appear for a sphere in serial, but they do appear for
                // parallel processing anyway
                continue;
            }
            // here siz ==2, it is either the first or second
            if( cell == cellsInSet[0] )
                neighbors[i] = cellsInSet[1];
            else
                neighbors[i] = cellsInSet[0];
        }
        // fill the rest with 0
        for( int i = nsides; i < max_edges; i++ )
            neighbors[i] = 0;
        // now simply set the neighbors tag; the last few positions will not be used, but for
        // simplicity will keep them all (MAXEDGES)
        rval = mb->tag_set_data( neighTag, &cell, 1, &neighbors[0] );MB_CHK_SET_ERR( rval, "can't set neigh tag" );
    }
    return MB_SUCCESS;
}

// slow interface; this will not do the advancing front trick
// some are triangles, some are quads, some are polygons ...
ErrorCode Intx2Mesh::intersect_meshes_kdtree( EntityHandle mbset1, EntityHandle mbset2, EntityHandle& outputSet )
{
    ErrorCode rval;
    mbs1   = mbset1;  // set 1 is departure, and it is completely covering the euler set on proc
    mbs2   = mbset2;
    outSet = outputSet;
    rval   = mb->get_entities_by_dimension( mbs1, 2, rs1 );MB_CHK_ERR( rval );
    rval = mb->get_entities_by_dimension( mbs2, 2, rs2 );MB_CHK_ERR( rval );
    // from create tags, copy relevant ones
    if( tgtParentTag ) mb->tag_delete( tgtParentTag );
    if( srcParentTag ) mb->tag_delete( srcParentTag );
    if( countTag ) mb->tag_delete( countTag );

    // create tgt edges if they do not exist yet; so when they are looked upon, they are found
    // this is the only call that is potentially NlogN, in the whole method
    rval = mb->get_adjacencies( rs2, 1, true, TgtEdges, Interface::UNION );MB_CHK_SET_ERR( rval, "can't get adjacent tgt edges" );

    int indx = 0;
    extraNodesVec.resize( TgtEdges.size() );
    for( Range::iterator eit = TgtEdges.begin(); eit != TgtEdges.end(); ++eit, indx++ )
    {
        std::vector< EntityHandle >* nv = new std::vector< EntityHandle >;
        extraNodesVec[indx]             = nv;
    }

    int defaultInt = -1;

    rval = mb->tag_get_handle( "TargetParent", 1, MB_TYPE_INTEGER, tgtParentTag, MB_TAG_DENSE | MB_TAG_CREAT,
                               &defaultInt );MB_CHK_SET_ERR( rval, "can't create positive tag" );

    rval = mb->tag_get_handle( "SourceParent", 1, MB_TYPE_INTEGER, srcParentTag, MB_TAG_DENSE | MB_TAG_CREAT,
                               &defaultInt );MB_CHK_SET_ERR( rval, "can't create negative tag" );

    rval = mb->tag_get_handle( "Counting", 1, MB_TYPE_INTEGER, countTag, MB_TAG_DENSE | MB_TAG_CREAT, &defaultInt );MB_CHK_SET_ERR( rval, "can't create Counting tag" );

    // for tgt cells, save a dense tag with the bordering edges, so we do not have to search for
    // them each time edges were for sure created before (tgtEdges)
    // if we have a tag with this name, it could be of a different size, so delete it
    rval = mb->tag_get_handle( "__tgtEdgeNeighbors", neighTgtEdgeTag );
    if( rval == MB_SUCCESS && neighTgtEdgeTag ) mb->tag_delete( neighTgtEdgeTag );
    std::vector< EntityHandle > zeroh( max_edges_2, 0 );
    rval = mb->tag_get_handle( "__tgtEdgeNeighbors", max_edges_2, MB_TYPE_HANDLE, neighTgtEdgeTag,
                               MB_TAG_DENSE | MB_TAG_CREAT, &zeroh[0] );MB_CHK_SET_ERR( rval, "can't create tgt edge neighbors tag" );
    for( Range::iterator rit = rs2.begin(); rit != rs2.end(); rit++ )
    {
        EntityHandle tgtCell = *rit;
        int num_nodes        = 0;
        rval                 = mb->get_connectivity( tgtCell, tgtConn, num_nodes );MB_CHK_SET_ERR( rval, "can't get  tgt conn" );
        // account for padded polygons
        while( tgtConn[num_nodes - 2] == tgtConn[num_nodes - 1] && num_nodes > 3 )
            num_nodes--;

        int i = 0;
        for( i = 0; i < num_nodes; i++ )
        {
            EntityHandle v[2] = { tgtConn[i],
                                  tgtConn[( i + 1 ) % num_nodes] };  // this is fine even for padded polygons
            std::vector< EntityHandle > adj_entities;
            rval = mb->get_adjacencies( v, 2, 1, false, adj_entities, Interface::INTERSECT );
            if( rval != MB_SUCCESS || adj_entities.size() < 1 ) return rval;  // get out , big error
            zeroh[i] = adj_entities[0];                                       // should be only one edge between 2 nodes
            // also, even if number of edges is less than max_edges_2, they will be ignored, even if
            // the tag is dense
        }
        // now set the value of the tag
        rval = mb->tag_set_data( neighTgtEdgeTag, &tgtCell, 1, &( zeroh[0] ) );MB_CHK_SET_ERR( rval, "can't set edge tgt tag" );
    }

    // create the kd tree on source cells, and intersect all targets in an expensive loop
    // build a kd tree with the rs1 (source) cells
    AdaptiveKDTree kd( mb );
    EntityHandle tree_root = 0;
    rval                   = kd.build_tree( rs1, &tree_root );MB_CHK_ERR( rval );

    // find out max edge on source mesh;
    double max_length = 0;
    {
        std::vector< double > coords;
        coords.resize( 3 * max_edges_1 );
        for( Range::iterator it = rs1.begin(); it != rs1.end(); it++ )
        {
            const EntityHandle* conn = NULL;
            int nnodes;
            rval = mb->get_connectivity( *it, conn, nnodes );MB_CHK_SET_ERR( rval, "can't get connectivity" );
            while( conn[nnodes - 2] == conn[nnodes - 1] && nnodes > 3 )
                nnodes--;
            rval = mb->get_coords( conn, nnodes, &coords[0] );MB_CHK_SET_ERR( rval, "can't get coordinates" );
            for( int j = 0; j < nnodes; j++ )
            {
                int next = ( j + 1 ) % nnodes;
                double leng;
                leng = ( coords[3 * j] - coords[3 * next] ) * ( coords[3 * j] - coords[3 * next] ) +
                       ( coords[3 * j + 1] - coords[3 * next + 1] ) * ( coords[3 * j + 1] - coords[3 * next + 1] ) +
                       ( coords[3 * j + 2] - coords[3 * next + 2] ) * ( coords[3 * j + 2] - coords[3 * next + 2] );
                leng = sqrt( leng );
                if( leng > max_length ) max_length = leng;
            }
        }
    }
    // maximum sag on a spherical mesh make sense only for intx on a sphere, with radius 1 :(
    double tolerance = 1.e-15;
    if( max_length < 1. )
    {
        // basically, the sag for an arc of length max_length on a circle of radius 1
        tolerance = 1. - sqrt( 1 - max_length * max_length / 4 );
        if( box_error < tolerance ) box_error = tolerance;
        tolerance = 3 * tolerance;  // we use it for gnomonic plane too, projected sag could be =* sqrt(2.)
        // be more generous, use 1.5 ~= sqrt(2.)

        if( !my_rank )
        {
            std::cout << " max edge length: " << max_length << "  tolerance for kd tree: " << tolerance << "\n";
            std::cout << " box overlap tolerance: " << box_error << "\n";
        }
    }
    for( Range::iterator it = rs2.begin(); it != rs2.end(); ++it )
    {
        EntityHandle tcell = *it;
        // find vertex positions
        const EntityHandle* conn = NULL;
        int nnodes               = 0;
        rval                     = mb->get_connectivity( tcell, conn, nnodes );MB_CHK_ERR( rval );
        // find leaves close to those positions
        double areaTgtCell   = setup_tgt_cell( tcell, nnodes );  // this is the area in the gnomonic plane
        double recoveredArea = 0;
        std::vector< double > positions;
        positions.resize( nnodes * 3 );
        rval = mb->get_coords( conn, nnodes, &positions[0] );MB_CHK_ERR( rval );

        // distance to search will be based on average edge length
        double av_len = 0;
        for( int k = 0; k < nnodes; k++ )
        {
            int ik      = ( k + 1 ) % nnodes;
            double len1 = 0;
            for( int j = 0; j < 3; j++ )
            {
                double len2 = positions[3 * k + j] - positions[3 * ik + j];
                len1 += len2 * len2;
            }
            av_len += sqrt( len1 );
        }
        if( nnodes > 0 ) av_len /= nnodes;
        // find leaves within a distance from each vertex of target
        // in those leaves, collect all cells; we will try for an intx in there
        Range close_source_cells;
        std::vector< EntityHandle > leaves;
        for( int i = 0; i < nnodes; i++ )
        {

            leaves.clear();
            rval = kd.distance_search( &positions[3 * i], av_len, leaves, tolerance, epsilon_1 );MB_CHK_ERR( rval );

            for( std::vector< EntityHandle >::iterator j = leaves.begin(); j != leaves.end(); ++j )
            {
                Range tmp;
                rval = mb->get_entities_by_dimension( *j, 2, tmp );MB_CHK_ERR( rval );

                close_source_cells.merge( tmp.begin(), tmp.end() );
            }
        }
#ifdef VERBOSE
        if( close_source_cells.empty() )
        {
            std::cout << " there are no close source cells to target cell " << tcell << " id from handle "
                      << mb->id_from_handle( tcell ) << "\n";
        }
#endif
        for( Range::iterator it2 = close_source_cells.begin(); it2 != close_source_cells.end(); ++it2 )
        {
            EntityHandle startSrc = *it2;
            double area           = 0;
            // if area is > 0 , we have intersections
            double P[10 * MAXEDGES];  // max 8 intx points + 8 more in the polygon
            //
            int nP = 0;
            int nb[MAXEDGES], nr[MAXEDGES];  // sides 3 or 4? also, check boxes first
            int nsTgt, nsSrc;
            rval = computeIntersectionBetweenTgtAndSrc( tcell, startSrc, P, nP, area, nb, nr, nsSrc, nsTgt, true );MB_CHK_ERR( rval );
            if( area > 0 )
            {
                if( nP > 1 )
                {  // this will also construct triangles/polygons in the new mesh, if needed
                    rval = findNodes( tcell, nnodes, startSrc, nsSrc, P, nP );MB_CHK_ERR( rval );
                }
                recoveredArea += area;
            }
        }
        recoveredArea = ( recoveredArea - areaTgtCell ) / areaTgtCell;  // replace now with recovery fract
    }
    // before cleaning up , we need to settle the position of the intersection points
    // on the boundary edges
    // this needs to be collective, so we should maybe wait something
#ifdef MOAB_HAVE_MPI
    rval = resolve_intersection_sharing();MB_CHK_SET_ERR( rval, "can't correct position, Intx2Mesh.cpp \n" );
#endif

    this->clean();
    return MB_SUCCESS;
}
// main interface; this will do the advancing front trick
// some are triangles, some are quads, some are polygons ...
ErrorCode Intx2Mesh::intersect_meshes( EntityHandle mbset1, EntityHandle mbset2, EntityHandle& outputSet )
{
    ErrorCode rval;
    mbs1   = mbset1;  // set 1 is departure, and it is completely covering the euler set on proc
    mbs2   = mbset2;
    outSet = outputSet;
#ifdef VERBOSE
    std::stringstream ffs, fft;
    ffs << "source_rank0" << my_rank << ".vtk";
    rval = mb->write_mesh( ffs.str().c_str(), &mbset1, 1 );MB_CHK_ERR( rval );
    fft << "target_rank0" << my_rank << ".vtk";
    rval = mb->write_mesh( fft.str().c_str(), &mbset2, 1 );MB_CHK_ERR( rval );

#endif
    // really, should be something from t1 and t2; src is 1 (lagrange), tgt is 2 (euler)

    EntityHandle startSrc = 0, startTgt = 0;

    rval = mb->get_entities_by_dimension( mbs1, 2, rs1 );MB_CHK_ERR( rval );
    rval = mb->get_entities_by_dimension( mbs2, 2, rs2 );MB_CHK_ERR( rval );
    // std::cout << "rs1.size() = " << rs1.size() << " and rs2.size() = "  << rs2.size() << "\n";
    // std::cout.flush();

    createTags();  // will also determine max_edges_1, max_edges_2 (for src and tgt meshes)

    Range rs22 = rs2;  // a copy of the initial range; we will remove from it elements as we
                       // advance ; rs2 is needed for marking the polygon to the tgt parent

    // create the local kdd tree with source elements; will use it to search
    // more efficiently for the seeds in advancing front;
    // some of the target cells will not be covered by source cells, and they need to be eliminated
    // early from contention

    // build a kd tree with the rs1 (source) cells
    AdaptiveKDTree kd( mb );
    EntityHandle tree_root = 0;
    rval                   = kd.build_tree( rs1, &tree_root );MB_CHK_ERR( rval );

    while( !rs22.empty() )
    {
#if defined( ENABLE_DEBUG ) || defined( VERBOSE )
        if( rs22.size() < rs2.size() )
        {
            std::cout << " possible not connected arrival mesh; my_rank: " << my_rank << " counting: " << counting
                      << "\n";
            std::stringstream ffo;
            ffo << "file0" << counting << "rank0" << my_rank << ".vtk";
            rval = mb->write_mesh( ffo.str().c_str(), &outSet, 1 );MB_CHK_ERR( rval );
        }
#endif
        bool seedFound = false;
        for( Range::iterator it = rs22.begin(); it != rs22.end(); ++it )
        {
            startTgt  = *it;
            int found = 0;
            // find vertex positions
            const EntityHandle* conn = NULL;
            int nnodes               = 0;
            rval                     = mb->get_connectivity( startTgt, conn, nnodes );MB_CHK_ERR( rval );
            // find leaves close to those positions
            std::vector< double > positions;
            positions.resize( nnodes * 3 );
            rval = mb->get_coords( conn, nnodes, &positions[0] );MB_CHK_ERR( rval );
            // find leaves within a distance from each vertex of target
            // in those leaves, collect all cells; we will try for an intx in there, instead of
            // looping over all rs1 cells, as before
            Range close_source_cells;
            std::vector< EntityHandle > leaves;
            for( int i = 0; i < nnodes; i++ )
            {

                leaves.clear();
                rval = kd.distance_search( &positions[3 * i], epsilon_1, leaves, epsilon_1, epsilon_1 );MB_CHK_ERR( rval );

                for( std::vector< EntityHandle >::iterator j = leaves.begin(); j != leaves.end(); ++j )
                {
                    Range tmp;
                    rval = mb->get_entities_by_dimension( *j, 2, tmp );MB_CHK_ERR( rval );

                    close_source_cells.merge( tmp.begin(), tmp.end() );
                }
            }

            for( Range::iterator it2 = close_source_cells.begin(); it2 != close_source_cells.end() && !found; ++it2 )
            {
                startSrc    = *it2;
                double area = 0;
                // if area is > 0 , we have intersections
                double P[10 * MAXEDGES];  // max 8 intx points + 8 more in the polygon
                //
                int nP = 0;
                int nb[MAXEDGES], nr[MAXEDGES];  // sides 3 or 4? also, check boxes first
                int nsTgt, nsSrc;
                rval =
                    computeIntersectionBetweenTgtAndSrc( startTgt, startSrc, P, nP, area, nb, nr, nsSrc, nsTgt, true );MB_CHK_ERR( rval );
                if( area > 0 )
                {
                    found     = 1;
                    seedFound = true;
                    break;  // found 2 elements that intersect; these will be the seeds
                }
            }
            if( found )
                break;
            else
            {
#if defined( VERBOSE )
                std::cout << " on rank " << my_rank << " target cell " << ID_FROM_HANDLE( startTgt )
                          << " not intx with any source\n";
#endif
                rs22.erase( startTgt );
            }
        }
        if( !seedFound ) continue;  // continue while(!rs22.empty())

        std::queue< EntityHandle > srcQueue;  // these are corresponding to Ta,
        srcQueue.push( startSrc );
        std::queue< EntityHandle > tgtQueue;
        tgtQueue.push( startTgt );

        Range toResetSrcs;  // will be used to reset src flags for every tgt quad
        // processed

        /*if (my_rank==0)
          dbg_1 = 1;*/
        unsigned char used = 1;
        // mark the start tgt quad as used, so it will not come back again
        rval = mb->tag_set_data( TgtFlagTag, &startTgt, 1, &used );MB_CHK_ERR( rval );
        while( !tgtQueue.empty() )
        {
            // flags for the side : 0 means a src cell not found on side
            // a paired src not found yet for the neighbors of tgt
            Range nextSrc[MAXEDGES];  // there are new ranges of possible next src cells for
                                      // seeding the side j of tgt cell

            EntityHandle currentTgt = tgtQueue.front();
            tgtQueue.pop();
            int nsidesTgt;                                                   // will be initialized now
            double areaTgtCell   = setup_tgt_cell( currentTgt, nsidesTgt );  // this is the area in the gnomonic plane
            double recoveredArea = 0;
            // get the neighbors of tgt, and if they are solved already, do not bother with that
            // side of tgt
            EntityHandle tgtNeighbors[MAXEDGES] = { 0 };
            rval                                = mb->tag_get_data( tgtNeighTag, &currentTgt, 1, tgtNeighbors );MB_CHK_SET_ERR( rval, "can't get neighbors of current tgt" );
#ifdef ENABLE_DEBUG
            if( dbg_1 )
            {
                std::cout << "Next: neighbors for current tgt ";
                for( int kk = 0; kk < nsidesTgt; kk++ )
                {
                    if( tgtNeighbors[kk] > 0 )
                        std::cout << mb->id_from_handle( tgtNeighbors[kk] ) << " ";
                    else
                        std::cout << 0 << " ";
                }
                std::cout << std::endl;
            }
#endif
            // now get the status of neighbors; if already solved, make them 0, so not to bother
            // anymore on that side of tgt
            for( int j = 0; j < nsidesTgt; j++ )
            {
                EntityHandle tgtNeigh = tgtNeighbors[j];
                unsigned char status  = 1;
                if( tgtNeigh == 0 ) continue;
                rval = mb->tag_get_data( TgtFlagTag, &tgtNeigh, 1, &status );MB_CHK_ERR( rval );                     // status 0 is unused
                if( 1 == status ) tgtNeighbors[j] = 0;  // so will not look anymore on this side of tgt
            }

#ifdef ENABLE_DEBUG
            if( dbg_1 )
            {
                std::cout << "reset sources: ";
                for( Range::iterator itr = toResetSrcs.begin(); itr != toResetSrcs.end(); ++itr )
                    std::cout << mb->id_from_handle( *itr ) << " ";
                std::cout << std::endl;
            }
#endif
            EntityHandle currentSrc = srcQueue.front();
            // tgt and src queues are parallel; for clarity we should have kept in the queue pairs
            // of entity handle std::pair<EntityHandle, EntityHandle>; so just one queue, with
            // pairs;
            //  at every moment, the queue contains pairs of cells that intersect, and they form the
            //  "advancing front"
            srcQueue.pop();
            toResetSrcs.clear();  // empty the range of used srcs, will have to be set unused again,
            // at the end of tgt element processing
            toResetSrcs.insert( currentSrc );
            // mb2->set_tag_data
            std::queue< EntityHandle > localSrc;
            localSrc.push( currentSrc );
#ifdef VERBOSE
            int countingStart = counting;
#endif
            // will advance-front search in the neighborhood of tgt cell, until we finish processing
            // all
            //   possible src cells; localSrc queue will contain all possible src cells that cover
            //   the current tgt cell
            while( !localSrc.empty() )
            {
                //
                EntityHandle srcT = localSrc.front();
                localSrc.pop();
                double P[10 * MAXEDGES], area;  //
                int nP           = 0;
                int nb[MAXEDGES] = { 0 };
                int nr[MAXEDGES] = { 0 };

                int nsidesSrc;  ///
                // area is in 2d, points are in 3d (on a sphere), back-projected, or in a plane
                // intersection points could include the vertices of initial elements
                // nb [j] = 0 means no intersection on the side j for element src (markers)
                // nb [j] = 1 means that the side j (from j to j+1) of src poly intersects the
                // tgt poly.  A potential next poly in the tgt queue is the tgt poly that is
                // adjacent to this side
                rval = computeIntersectionBetweenTgtAndSrc( /* tgt */ currentTgt, srcT, P, nP, area, nb, nr, nsidesSrc,
                                                            nsidesTgt );MB_CHK_ERR( rval );
                if( nP > 0 )
                {
#ifdef ENABLE_DEBUG
                    if( dbg_1 )
                    {
                        for( int k = 0; k < 3; k++ )
                        {
                            std::cout << " nb, nr: " << k << " " << nb[k] << " " << nr[k] << "\n";
                        }
                    }
#endif

                    // intersection found: output P and original triangles if nP > 2
                    EntityHandle neighbors[MAXEDGES] = { 0 };
                    rval                             = mb->tag_get_data( srcNeighTag, &srcT, 1, neighbors );
                    if( rval != MB_SUCCESS )
                    {
                        std::cout << " can't get the neighbors for src element " << mb->id_from_handle( srcT );
                        return MB_FAILURE;
                    }

                    // add neighbors to the localSrc queue, if they are not marked
                    for( int nn = 0; nn < nsidesSrc; nn++ )
                    {
                        EntityHandle neighbor = neighbors[nn];
                        if( neighbor > 0 && nb[nn] > 0 )  // advance across src boundary nn
                        {
                            if( toResetSrcs.find( neighbor ) == toResetSrcs.end() )
                            {
                                localSrc.push( neighbor );
#ifdef ENABLE_DEBUG
                                if( dbg_1 )
                                {
                                    std::cout << " local src elem " << mb->id_from_handle( neighbor )
                                              << " for tgt:" << mb->id_from_handle( currentTgt ) << "\n";
                                    mb->list_entities( &neighbor, 1 );
                                }
#endif
                                toResetSrcs.insert( neighbor );
                            }
                        }
                    }
                    // n(find(nc>0))=ac;        % ac is starting candidate for neighbor
                    for( int nn = 0; nn < nsidesTgt; nn++ )
                    {
                        if( nr[nn] > 0 && tgtNeighbors[nn] > 0 )
                            nextSrc[nn].insert( srcT );  // potential src cell that can intersect
                                                         // the tgt neighbor nn
                    }
                    if( nP > 1 )
                    {  // this will also construct triangles/polygons in the new mesh, if needed
                        rval = findNodes( currentTgt, nsidesTgt, srcT, nsidesSrc, P, nP );MB_CHK_ERR( rval );
                    }

                    recoveredArea += area;
                }
#ifdef ENABLE_DEBUG
                else if( dbg_1 )
                {
                    std::cout << " tgt, src, do not intersect: " << mb->id_from_handle( currentTgt ) << " "
                              << mb->id_from_handle( srcT ) << "\n";
                }
#endif
            }                                                               // end while (!localSrc.empty())
            recoveredArea = ( recoveredArea - areaTgtCell ) / areaTgtCell;  // replace now with recovery fraction
#if defined( ENABLE_DEBUG ) || defined( VERBOSE )
            if( fabs( recoveredArea ) > epsilon_1 )
            {
#ifdef VERBOSE
                std::cout << " tgt area: " << areaTgtCell << " recovered :" << recoveredArea * ( 1 + areaTgtCell )
                          << " fraction error recovery:" << recoveredArea
                          << " tgtID: " << mb->id_from_handle( currentTgt ) << " countingStart:" << countingStart
                          << "\n";
#endif
            }
#endif
            // here, we are finished with tgtCurrent, take it out of the rs22 range (tgt, arrival
            // mesh)
            rs22.erase( currentTgt );
            // also, look at its neighbors, and add to the seeds a next one

            for( int j = 0; j < nsidesTgt; j++ )
            {
                EntityHandle tgtNeigh = tgtNeighbors[j];
                if( tgtNeigh == 0 || nextSrc[j].size() == 0 )  // if tgt is bigger than src, there could be no src
                                                               // to advance on that side
                    continue;
                int nsidesTgt2 = 0;
                setup_tgt_cell( tgtNeigh,
                                nsidesTgt2 );  // find possible intersection with src cell from nextSrc
                for( Range::iterator nit = nextSrc[j].begin(); nit != nextSrc[j].end(); ++nit )
                {
                    EntityHandle nextB = *nit;
                    // we identified tgt quad n[j] as possibly intersecting with neighbor j of the
                    // src quad
                    double P[10 * MAXEDGES], area;  //
                    int nP           = 0;
                    int nb[MAXEDGES] = { 0 };
                    int nr[MAXEDGES] = { 0 };

                    int nsidesSrc;  ///
                    rval = computeIntersectionBetweenTgtAndSrc(
                        /* tgt */ tgtNeigh, nextB, P, nP, area, nb, nr, nsidesSrc, nsidesTgt2 );MB_CHK_ERR( rval );
                    if( area > 0 )
                    {
                        tgtQueue.push( tgtNeigh );
                        srcQueue.push( nextB );
#ifdef ENABLE_DEBUG
                        if( dbg_1 )
                            std::cout << "new polys pushed: src, tgt:" << mb->id_from_handle( tgtNeigh ) << " "
                                      << mb->id_from_handle( nextB ) << std::endl;
#endif
                        rval = mb->tag_set_data( TgtFlagTag, &tgtNeigh, 1, &used );MB_CHK_ERR( rval );
                        break;  // so we are done with this side of tgt, we have found a proper next
                                // seed
                    }
                }
            }

        }  // end while (!tgtQueue.empty())
    }
#ifdef ENABLE_DEBUG
    if( dbg_1 )
    {
        for( int k = 0; k < 6; k++ )
            mout_1[k].close();
    }
#endif
    // before cleaning up , we need to settle the position of the intersection points
    // on the boundary edges
    // this needs to be collective, so we should maybe wait something
#ifdef MOAB_HAVE_MPI
    rval = resolve_intersection_sharing();MB_CHK_SET_ERR( rval, "can't correct position, Intx2Mesh.cpp \n" );
#endif

    this->clean();
    return MB_SUCCESS;
}

// clean some memory allocated
void Intx2Mesh::clean()
{
    //
    int indx = 0;
    for( Range::iterator eit = TgtEdges.begin(); eit != TgtEdges.end(); ++eit, indx++ )
    {
        delete extraNodesVec[indx];
    }
    // extraNodesMap.clear();
    extraNodesVec.clear();
    // also, delete some bit tags, used to mark processed tgts and srcs
    mb->tag_delete( TgtFlagTag );
    counting = 0;  // reset counting to original value
}
// this method will reduce number of nodes, collapse edges that are of length 0
// so a polygon like 428 431 431 will become a line 428 431
// or something like 428 431 431 531 -> 428 431 531
void Intx2Mesh::correct_polygon( EntityHandle* nodes, int& nP )
{
    int i = 0;
    while( i < nP )
    {
        int nextIndex = ( i + 1 ) % nP;
        if( nodes[i] == nodes[nextIndex] )
        {
#ifdef ENABLE_DEBUG
            // we need to reduce nP, and collapse nodes
            if( dbg_1 )
            {
                std::cout << " nodes duplicated in list: ";
                for( int j = 0; j < nP; j++ )
                    std::cout << nodes[j] << " ";
                std::cout << "\n";
                std::cout << " node " << nodes[i] << " at index " << i << " is duplicated"
                          << "\n";
            }
#endif
            // this will work even if we start from 1 2 3 1; when i is 3, we find nextIndex is 0,
            // then next thing does nothing
            //  (nP-1 is 3, so k is already >= nP-1); it will result in nodes -> 1, 2, 3
            for( int k = i; k < nP - 1; k++ )
                nodes[k] = nodes[k + 1];
            nP--;  // decrease the number of nodes; also, decrease i, just if we may need to check
                   // again
            i--;
        }
        i++;
    }
    return;
}
#ifdef MOAB_HAVE_MPI
ErrorCode Intx2Mesh::build_processor_euler_boxes( EntityHandle euler_set, Range& local_verts )
{
    localEnts.clear();
    ErrorCode rval = mb->get_entities_by_dimension( euler_set, 2, localEnts );ERRORR( rval, "can't get ents by dimension" );

    rval                = mb->get_connectivity( localEnts, local_verts );
    int num_local_verts = (int)local_verts.size();ERRORR( rval, "can't get local vertices" );

    assert( parcomm != NULL );

    // get the position of local vertices, and decide local boxes (allBoxes...)
    double bmin[3] = { DBL_MAX, DBL_MAX, DBL_MAX };
    double bmax[3] = { -DBL_MAX, -DBL_MAX, -DBL_MAX };

    std::vector< double > coords( 3 * num_local_verts );
    rval = mb->get_coords( local_verts, &coords[0] );ERRORR( rval, "can't get coords of vertices " );

    for( int i = 0; i < num_local_verts; i++ )
    {
        for( int k = 0; k < 3; k++ )
        {
            double val = coords[3 * i + k];
            if( val < bmin[k] ) bmin[k] = val;
            if( val > bmax[k] ) bmax[k] = val;
        }
    }
    int numprocs = parcomm->proc_config().proc_size();
    allBoxes.resize( 6 * numprocs );

    my_rank = parcomm->proc_config().proc_rank();
    for( int k = 0; k < 3; k++ )
    {
        allBoxes[6 * my_rank + k]     = bmin[k];
        allBoxes[6 * my_rank + 3 + k] = bmax[k];
    }

    // now communicate to get all boxes
    int mpi_err;
#if( MPI_VERSION >= 2 )
    // use "in place" option
    mpi_err = MPI_Allgather( MPI_IN_PLACE, 0, MPI_DATATYPE_NULL, &allBoxes[0], 6, MPI_DOUBLE,
                             parcomm->proc_config().proc_comm() );
#else
    {
        std::vector< double > allBoxes_tmp( 6 * parcomm->proc_config().proc_size() );
        mpi_err  = MPI_Allgather( &allBoxes[6 * my_rank], 6, MPI_DOUBLE, &allBoxes_tmp[0], 6, MPI_DOUBLE,
                                 parcomm->proc_config().proc_comm() );
        allBoxes = allBoxes_tmp;
    }
#endif
    if( MPI_SUCCESS != mpi_err ) return MB_FAILURE;

    if( my_rank == 0 )
    {
        std::cout << " maximum number of vertices per cell are " << max_edges_1 << " on first mesh and " << max_edges_2
                  << " on second mesh \n";
        for( int i = 0; i < numprocs; i++ )
        {
            std::cout << "proc: " << i << " box min: " << allBoxes[6 * i] << " " << allBoxes[6 * i + 1] << " "
                      << allBoxes[6 * i + 2] << " \n";
            std::cout << "        box max: " << allBoxes[6 * i + 3] << " " << allBoxes[6 * i + 4] << " "
                      << allBoxes[6 * i + 5] << " \n";
        }
    }

    return MB_SUCCESS;
}
ErrorCode Intx2Mesh::create_departure_mesh_2nd_alg( EntityHandle& euler_set, EntityHandle& covering_lagr_set )
{
    // compute the bounding box on each proc
    assert( parcomm != NULL );

    localEnts.clear();
    ErrorCode rval = mb->get_entities_by_dimension( euler_set, 2, localEnts );ERRORR( rval, "can't get ents by dimension" );

    Tag dpTag = 0;
    std::string tag_name( "DP" );
    rval = mb->tag_get_handle( tag_name.c_str(), 3, MB_TYPE_DOUBLE, dpTag, MB_TAG_DENSE );ERRORR( rval, "can't get DP tag" );

    EntityHandle dum = 0;
    Tag corrTag;
    rval = mb->tag_get_handle( CORRTAGNAME, 1, MB_TYPE_HANDLE, corrTag, MB_TAG_DENSE | MB_TAG_CREAT, &dum );ERRORR( rval, "can't get CORR tag" );
    // get all local verts
    Range local_verts;
    rval                = mb->get_connectivity( localEnts, local_verts );
    int num_local_verts = (int)local_verts.size();ERRORR( rval, "can't get local vertices" );

    rval = Intx2Mesh::build_processor_euler_boxes( euler_set, local_verts );ERRORR( rval, "can't build processor boxes" );

    std::vector< int > gids( num_local_verts );
    rval = mb->tag_get_data( gid, local_verts, &gids[0] );ERRORR( rval, "can't get local vertices gids" );

    // now see the departure points; to what boxes should we send them?
    std::vector< double > dep_points( 3 * num_local_verts );
    rval = mb->tag_get_data( dpTag, local_verts, (void*)&dep_points[0] );ERRORR( rval, "can't get DP tag values" );
    // ranges to send to each processor; will hold vertices and elements (quads?)
    // will look if the box of the dep quad covers box of euler mesh on proc (with tolerances)
    std::map< int, Range > Rto;
    int numprocs = parcomm->proc_config().proc_size();

    for( Range::iterator eit = localEnts.begin(); eit != localEnts.end(); ++eit )
    {
        EntityHandle q = *eit;
        const EntityHandle* conn4;
        int num_nodes;
        rval = mb->get_connectivity( q, conn4, num_nodes );ERRORR( rval, "can't get DP tag values" );
        CartVect qbmin( DBL_MAX );
        CartVect qbmax( -DBL_MAX );
        for( int i = 0; i < num_nodes; i++ )
        {
            EntityHandle v = conn4[i];
            size_t index   = local_verts.find( v ) - local_verts.begin();
            CartVect dp( &dep_points[3 * index] );  // will use constructor
            for( int j = 0; j < 3; j++ )
            {
                if( qbmin[j] > dp[j] ) qbmin[j] = dp[j];
                if( qbmax[j] < dp[j] ) qbmax[j] = dp[j];
            }
        }
        for( int p = 0; p < numprocs; p++ )
        {
            CartVect bbmin( &allBoxes[6 * p] );
            CartVect bbmax( &allBoxes[6 * p + 3] );
            if( GeomUtil::boxes_overlap( bbmin, bbmax, qbmin, qbmax, box_error ) ) { Rto[p].insert( q ); }
        }
    }

    // now, build TLv and TLq, for each p
    size_t numq = 0;
    size_t numv = 0;
    for( int p = 0; p < numprocs; p++ )
    {
        if( p == (int)my_rank ) continue;  // do not "send" it, because it is already here
        Range& range_to_P = Rto[p];
        // add the vertices to it
        if( range_to_P.empty() ) continue;  // nothing to send to proc p
        Range vertsToP;
        rval = mb->get_connectivity( range_to_P, vertsToP );ERRORR( rval, "can't get connectivity" );
        numq = numq + range_to_P.size();
        numv = numv + vertsToP.size();
        range_to_P.merge( vertsToP );
    }
    TupleList TLv;
    TupleList TLq;
    TLv.initialize( 2, 0, 0, 3, numv );  // to proc, GLOBAL ID, DP points
    TLv.enableWriteAccess();

    int sizeTuple = 2 + max_edges_1;  // determined earlier, for src, first mesh
    TLq.initialize( 2 + max_edges_1, 0, 1, 0,
                    numq );  // to proc, elem GLOBAL ID, connectivity[10] (global ID v), local eh
    TLq.enableWriteAccess();
#ifdef VERBOSE
    std::cout << "from proc " << my_rank << " send " << numv << " vertices and " << numq << " elements\n";
#endif
    for( int to_proc = 0; to_proc < numprocs; to_proc++ )
    {
        if( to_proc == (int)my_rank ) continue;
        Range& range_to_P = Rto[to_proc];
        Range V           = range_to_P.subset_by_type( MBVERTEX );

        for( Range::iterator it = V.begin(); it != V.end(); ++it )
        {
            EntityHandle v       = *it;
            unsigned int index   = local_verts.find( v ) - local_verts.begin();
            int n                = TLv.get_n();
            TLv.vi_wr[2 * n]     = to_proc;                // send to processor
            TLv.vi_wr[2 * n + 1] = gids[index];            // global id needs index in the local_verts range
            TLv.vr_wr[3 * n]     = dep_points[3 * index];  // departure position, of the node local_verts[i]
            TLv.vr_wr[3 * n + 1] = dep_points[3 * index + 1];
            TLv.vr_wr[3 * n + 2] = dep_points[3 * index + 2];
            TLv.inc_n();
        }
        // also, prep the quad for sending ...
        Range Q = range_to_P.subset_by_dimension( 2 );
        for( Range::iterator it = Q.begin(); it != Q.end(); ++it )
        {
            EntityHandle q = *it;
            int global_id;
            rval = mb->tag_get_data( gid, &q, 1, &global_id );ERRORR( rval, "can't get gid for polygon" );
            int n                        = TLq.get_n();
            TLq.vi_wr[sizeTuple * n]     = to_proc;    //
            TLq.vi_wr[sizeTuple * n + 1] = global_id;  // global id of element, used to identify it ...
            const EntityHandle* conn4;
            int num_nodes;
            rval = mb->get_connectivity( q, conn4,
                                         num_nodes );  // could be up to MAXEDGES, but it is limited by max_edges_1
            ERRORR( rval, "can't get connectivity for cell" );
            if( num_nodes > MAXEDGES ) ERRORR( MB_FAILURE, "too many nodes in a polygon" );
            for( int i = 0; i < num_nodes; i++ )
            {
                EntityHandle v                   = conn4[i];
                unsigned int index               = local_verts.find( v ) - local_verts.begin();
                TLq.vi_wr[sizeTuple * n + 2 + i] = gids[index];
            }
            for( int k = num_nodes; k < max_edges_1; k++ )
            {
                TLq.vi_wr[sizeTuple * n + 2 + k] =
                    0;  // fill the rest of node ids with 0; we know that the node ids start from 1!
            }
            TLq.vul_wr[n] = q;  // save here the entity handle, it will be communicated back
            // maybe we should forget about global ID
            TLq.inc_n();
        }
    }

    // now we are done populating the tuples; route them to the appropriate processors
    ( parcomm->proc_config().crystal_router() )->gs_transfer( 1, TLv, 0 );
    ( parcomm->proc_config().crystal_router() )->gs_transfer( 1, TLq, 0 );
    // the elements are already in localEnts;

    // maps from global ids to new vertex and quad handles, that are added
    std::map< int, EntityHandle > globalID_to_handle;
    /*std::map<int, EntityHandle> globalID_to_eh;*/
    globalID_to_eh.clear();  // need for next iteration
    // now, look at every TLv, and see if we have to create a vertex there or not
    int n = TLv.get_n();  // the size of the points received
    for( int i = 0; i < n; i++ )
    {
        int globalId = TLv.vi_rd[2 * i + 1];
        if( globalID_to_handle.find( globalId ) == globalID_to_handle.end() )
        {
            EntityHandle new_vert;
            double dp_pos[3] = { TLv.vr_wr[3 * i], TLv.vr_wr[3 * i + 1], TLv.vr_wr[3 * i + 2] };
            rval             = mb->create_vertex( dp_pos, new_vert );ERRORR( rval, "can't create new vertex " );
            globalID_to_handle[globalId] = new_vert;
        }
    }

    // now, all dep points should be at their place
    // look in the local list of q for this proc, and create all those quads and vertices if needed
    // it may be an overkill, but because it does not involve communication, we do it anyway
    Range& local  = Rto[my_rank];
    Range local_q = local.subset_by_dimension( 2 );
    // the local should have all the vertices in local_verts
    for( Range::iterator it = local_q.begin(); it != local_q.end(); ++it )
    {
        EntityHandle q = *it;
        int nnodes;
        const EntityHandle* conn4;
        rval = mb->get_connectivity( q, conn4, nnodes );ERRORR( rval, "can't get connectivity of local q " );
        EntityHandle new_conn[MAXEDGES];
        for( int i = 0; i < nnodes; i++ )
        {
            EntityHandle v1    = conn4[i];
            unsigned int index = local_verts.find( v1 ) - local_verts.begin();
            int globalId       = gids[index];
            if( globalID_to_handle.find( globalId ) == globalID_to_handle.end() )
            {
                // we need to create that vertex, at this position dep_points
                double dp_pos[3] = { dep_points[3 * index], dep_points[3 * index + 1], dep_points[3 * index + 2] };
                EntityHandle new_vert;
                rval = mb->create_vertex( dp_pos, new_vert );ERRORR( rval, "can't create new vertex " );
                globalID_to_handle[globalId] = new_vert;
            }
            new_conn[i] = globalID_to_handle[gids[index]];
        }
        EntityHandle new_element;
        //
        EntityType entType = MBQUAD;
        if( nnodes > 4 ) entType = MBPOLYGON;
        if( nnodes < 4 ) entType = MBTRI;

        rval = mb->create_element( entType, new_conn, nnodes, new_element );ERRORR( rval, "can't create new quad " );
        rval = mb->add_entities( covering_lagr_set, &new_element, 1 );ERRORR( rval, "can't add new element to dep set" );
        int gid_el;
        // get the global ID of the initial quad
        rval = mb->tag_get_data( gid, &q, 1, &gid_el );ERRORR( rval, "can't get element global ID " );
        globalID_to_eh[gid_el] = new_element;
        // is this redundant or not?
        rval = mb->tag_set_data( corrTag, &new_element, 1, &q );ERRORR( rval, "can't set corr tag on new el" );
        // set the global id on new elem
        rval = mb->tag_set_data( gid, &new_element, 1, &gid_el );ERRORR( rval, "can't set global id tag on new el" );
    }
    // now look at all elements received through; we do not want to duplicate them
    n = TLq.get_n();  // number of elements received by this processor
    // form the remote cells, that will be used to send the tracer info back to the originating proc
    remote_cells = new TupleList();
    remote_cells->initialize( 2, 0, 1, 0, n );  // will not have tracer data anymore
    remote_cells->enableWriteAccess();
    for( int i = 0; i < n; i++ )
    {
        int globalIdEl = TLq.vi_rd[sizeTuple * i + 1];
        int from_proc  = TLq.vi_wr[sizeTuple * i];
        // do we already have a quad with this global ID, represented?
        if( globalID_to_eh.find( globalIdEl ) == globalID_to_eh.end() )
        {
            // construct the conn quad
            EntityHandle new_conn[MAXEDGES];
            int nnodes = -1;
            for( int j = 0; j < max_edges_1; j++ )
            {
                int vgid = TLq.vi_rd[sizeTuple * i + 2 + j];  // vertex global ID
                if( vgid == 0 )
                    new_conn[j] = 0;
                else
                {
                    assert( globalID_to_handle.find( vgid ) != globalID_to_handle.end() );
                    new_conn[j] = globalID_to_handle[vgid];
                    nnodes      = j + 1;  // nodes are at the beginning, and are variable number
                }
            }
            EntityHandle new_element;
            //
            EntityType entType = MBQUAD;
            if( nnodes > 4 ) entType = MBPOLYGON;
            if( nnodes < 4 ) entType = MBTRI;
            rval = mb->create_element( entType, new_conn, nnodes, new_element );ERRORR( rval, "can't create new element " );
            globalID_to_eh[globalIdEl] = new_element;
            rval                       = mb->add_entities( covering_lagr_set, &new_element, 1 );ERRORR( rval, "can't add new element to dep set" );
            /* rval = mb->tag_set_data(corrTag, &new_element, 1, &q);ERRORR(rval, "can't set corr tag on new el");*/
            remote_cells->vi_wr[2 * i]     = from_proc;
            remote_cells->vi_wr[2 * i + 1] = globalIdEl;
            //     remote_cells->vr_wr[i] = 0.; // no contribution yet sent back
            remote_cells->vul_wr[i] = TLq.vul_rd[i];  // this is the corresponding tgt cell (arrival)
            remote_cells->inc_n();
            // set the global id on new elem
            rval = mb->tag_set_data( gid, &new_element, 1, &globalIdEl );ERRORR( rval, "can't set global id tag on new el" );
        }
    }
    // order the remote cells tuple list, with the global id, because we will search in it
    // remote_cells->print("remote_cells before sorting");
    moab::TupleList::buffer sort_buffer;
    sort_buffer.buffer_init( n );
    remote_cells->sort( 1, &sort_buffer );
    sort_buffer.reset();
    return MB_SUCCESS;
}

// this algorithm assumes lagr set is already created, and some elements will be coming from
// other procs, and populate the covering_set
// we need to keep in a tuple list the remote cells from other procs, because we need to send back
// the intersection info (like area of the intx polygon, and the current concentration) maybe total
// mass in that intx
ErrorCode Intx2Mesh::create_departure_mesh_3rd_alg( EntityHandle& lagr_set, EntityHandle& covering_set )
{
    EntityHandle dum = 0;

    Tag corrTag;
    ErrorCode rval = mb->tag_get_handle( CORRTAGNAME, 1, MB_TYPE_HANDLE, corrTag, MB_TAG_DENSE | MB_TAG_CREAT, &dum );
    // start copy from 2nd alg
    // compute the bounding box on each proc
    assert( parcomm != NULL );
    if( 1 == parcomm->proc_config().proc_size() )
    {
        covering_set = lagr_set;  // nothing to communicate, it must be serial
        return MB_SUCCESS;
    }

    // get all local verts
    Range local_verts;
    rval                = mb->get_connectivity( localEnts, local_verts );
    int num_local_verts = (int)local_verts.size();ERRORR( rval, "can't get local vertices" );

    std::vector< int > gids( num_local_verts );
    rval = mb->tag_get_data( gid, local_verts, &gids[0] );ERRORR( rval, "can't get local vertices gids" );

    Range localDepCells;
    rval = mb->get_entities_by_dimension( lagr_set, 2, localDepCells );ERRORR( rval, "can't get ents by dimension from lagr set" );

    // get all lagr verts (departure vertices)
    Range lagr_verts;
    rval = mb->get_connectivity( localDepCells, lagr_verts );  // they should be created in
    // the same order as the euler vertices
    int num_lagr_verts = (int)lagr_verts.size();ERRORR( rval, "can't get local lagr vertices" );

    // now see the departure points position; to what boxes should we send them?
    std::vector< double > dep_points( 3 * num_lagr_verts );
    rval = mb->get_coords( lagr_verts, &dep_points[0] );ERRORR( rval, "can't get departure points position" );
    // ranges to send to each processor; will hold vertices and elements (quads?)
    // will look if the box of the dep quad covers box of euler mesh on proc (with tolerances)
    std::map< int, Range > Rto;
    int numprocs = parcomm->proc_config().proc_size();

    for( Range::iterator eit = localDepCells.begin(); eit != localDepCells.end(); ++eit )
    {
        EntityHandle q = *eit;
        const EntityHandle* conn4;
        int num_nodes;
        rval = mb->get_connectivity( q, conn4, num_nodes );ERRORR( rval, "can't get DP tag values" );
        CartVect qbmin( DBL_MAX );
        CartVect qbmax( -DBL_MAX );
        for( int i = 0; i < num_nodes; i++ )
        {
            EntityHandle v = conn4[i];
            int index      = lagr_verts.index( v );
            assert( -1 != index );
            CartVect dp( &dep_points[3 * index] );  // will use constructor
            for( int j = 0; j < 3; j++ )
            {
                if( qbmin[j] > dp[j] ) qbmin[j] = dp[j];
                if( qbmax[j] < dp[j] ) qbmax[j] = dp[j];
            }
        }
        for( int p = 0; p < numprocs; p++ )
        {
            CartVect bbmin( &allBoxes[6 * p] );
            CartVect bbmax( &allBoxes[6 * p + 3] );
            if( GeomUtil::boxes_overlap( bbmin, bbmax, qbmin, qbmax, box_error ) ) { Rto[p].insert( q ); }
        }
    }

    // now, build TLv and TLq, for each p
    size_t numq = 0;
    size_t numv = 0;
    for( int p = 0; p < numprocs; p++ )
    {
        if( p == (int)my_rank ) continue;  // do not "send" it, because it is already here
        Range& range_to_P = Rto[p];
        // add the vertices to it
        if( range_to_P.empty() ) continue;  // nothing to send to proc p
        Range vertsToP;
        rval = mb->get_connectivity( range_to_P, vertsToP );ERRORR( rval, "can't get connectivity" );
        numq = numq + range_to_P.size();
        numv = numv + vertsToP.size();
        range_to_P.merge( vertsToP );
    }
    TupleList TLv;
    TupleList TLq;
    TLv.initialize( 2, 0, 0, 3, numv );  // to proc, GLOBAL ID, DP points
    TLv.enableWriteAccess();

    int sizeTuple = 2 + max_edges_1;  // max edges could be up to MAXEDGES :) for polygons
    TLq.initialize( 2 + max_edges_1, 0, 1, 0,
                    numq );  // to proc, elem GLOBAL ID, connectivity[max_edges] (global ID v)
    // send also the corresponding tgt cell it will come to
    TLq.enableWriteAccess();
#ifdef VERBOSE
    std::cout << "from proc " << my_rank << " send " << numv << " vertices and " << numq << " elements\n";
#endif

    for( int to_proc = 0; to_proc < numprocs; to_proc++ )
    {
        if( to_proc == (int)my_rank ) continue;
        Range& range_to_P = Rto[to_proc];
        Range V           = range_to_P.subset_by_type( MBVERTEX );

        for( Range::iterator it = V.begin(); it != V.end(); ++it )
        {
            EntityHandle v = *it;
            int index = lagr_verts.index( v );  // will be the same index as the corresponding vertex in euler verts
            assert( -1 != index );
            int n                = TLv.get_n();
            TLv.vi_wr[2 * n]     = to_proc;                // send to processor
            TLv.vi_wr[2 * n + 1] = gids[index];            // global id needs index in the local_verts range
            TLv.vr_wr[3 * n]     = dep_points[3 * index];  // departure position, of the node local_verts[i]
            TLv.vr_wr[3 * n + 1] = dep_points[3 * index + 1];
            TLv.vr_wr[3 * n + 2] = dep_points[3 * index + 2];
            TLv.inc_n();
        }
        // also, prep the 2d cells for sending ...
        Range Q = range_to_P.subset_by_dimension( 2 );
        for( Range::iterator it = Q.begin(); it != Q.end(); ++it )
        {
            EntityHandle q = *it;  // this is a src cell
            int global_id;
            rval = mb->tag_get_data( gid, &q, 1, &global_id );ERRORR( rval, "can't get gid for polygon" );
            int n                        = TLq.get_n();
            TLq.vi_wr[sizeTuple * n]     = to_proc;    //
            TLq.vi_wr[sizeTuple * n + 1] = global_id;  // global id of element, used to identify it ...
            const EntityHandle* conn4;
            int num_nodes;
            rval = mb->get_connectivity(
                q, conn4, num_nodes );  // could be up to 10;ERRORR( rval, "can't get connectivity for quad" );
            if( num_nodes > MAXEDGES ) ERRORR( MB_FAILURE, "too many nodes in a polygon" );
            for( int i = 0; i < num_nodes; i++ )
            {
                EntityHandle v = conn4[i];
                int index      = lagr_verts.index( v );
                assert( -1 != index );
                TLq.vi_wr[sizeTuple * n + 2 + i] = gids[index];
            }
            for( int k = num_nodes; k < max_edges_1; k++ )
            {
                TLq.vi_wr[sizeTuple * n + 2 + k] =
                    0;  // fill the rest of node ids with 0; we know that the node ids start from 1!
            }
            EntityHandle tgtCell;
            rval = mb->tag_get_data( corrTag, &q, 1, &tgtCell );ERRORR( rval, "can't get corresponding tgt cell for dep cell" );
            TLq.vul_wr[n] = tgtCell;  // this will be sent to remote_cells, to be able to come back
            TLq.inc_n();
        }
    }
    // now we can route them to each processor
    // now we are done populating the tuples; route them to the appropriate processors
    ( parcomm->proc_config().crystal_router() )->gs_transfer( 1, TLv, 0 );
    ( parcomm->proc_config().crystal_router() )->gs_transfer( 1, TLq, 0 );
    // the elements are already in localEnts;

    // maps from global ids to new vertex and quad handles, that are added
    std::map< int, EntityHandle > globalID_to_handle;
    // we already have vertices from lagr set; they are already in the processor, even before
    // receiving other verts from neighbors
    int k = 0;
    for( Range::iterator vit = lagr_verts.begin(); vit != lagr_verts.end(); ++vit, k++ )
    {
        globalID_to_handle[gids[k]] = *vit;  // a little bit of overkill
        // we do know that the global ids between euler and lagr verts are parallel
    }
    /*std::map<int, EntityHandle> globalID_to_eh;*/  // do we need this one?
    globalID_to_eh.clear();
    // now, look at every TLv, and see if we have to create a vertex there or not
    int n = TLv.get_n();  // the size of the points received
    for( int i = 0; i < n; i++ )
    {
        int globalId = TLv.vi_rd[2 * i + 1];
        if( globalID_to_handle.find( globalId ) == globalID_to_handle.end() )
        {
            EntityHandle new_vert;
            double dp_pos[3] = { TLv.vr_wr[3 * i], TLv.vr_wr[3 * i + 1], TLv.vr_wr[3 * i + 2] };
            rval             = mb->create_vertex( dp_pos, new_vert );ERRORR( rval, "can't create new vertex " );
            globalID_to_handle[globalId] = new_vert;
        }
    }

    // now, all dep points should be at their place
    // look in the local list of 2d cells for this proc, and create all those cells if needed
    // it may be an overkill, but because it does not involve communication, we do it anyway
    Range& local  = Rto[my_rank];
    Range local_q = local.subset_by_dimension( 2 );
    // the local should have all the vertices in lagr_verts
    for( Range::iterator it = local_q.begin(); it != local_q.end(); ++it )
    {
        EntityHandle q = *it;  // these are from lagr cells, local
        int gid_el;
        rval = mb->tag_get_data( gid, &q, 1, &gid_el );ERRORR( rval, "can't get element global ID " );
        globalID_to_eh[gid_el] = q;  // do we need this? maybe to just mark the ones on this processor
        // maybe a range of global cell ids is fine?
    }
    // now look at all elements received through; we do not want to duplicate them
    n = TLq.get_n();  // number of elements received by this processor
    // a cell should be received from one proc only; so why are we so worried about duplicated
    // elements? a vertex can be received from multiple sources, that is fine

    remote_cells = new TupleList();
    remote_cells->initialize( 2, 0, 1, 0, n );  // no tracers anymore in these tuples
    remote_cells->enableWriteAccess();
    for( int i = 0; i < n; i++ )
    {
        int globalIdEl = TLq.vi_rd[sizeTuple * i + 1];
        int from_proc  = TLq.vi_rd[sizeTuple * i];
        // do we already have a quad with this global ID, represented?
        if( globalID_to_eh.find( globalIdEl ) == globalID_to_eh.end() )
        {
            // construct the conn quad
            EntityHandle new_conn[MAXEDGES];
            int nnodes = -1;
            for( int j = 0; j < max_edges_1; j++ )
            {
                int vgid = TLq.vi_rd[sizeTuple * i + 2 + j];  // vertex global ID
                if( vgid == 0 )
                    new_conn[j] = 0;
                else
                {
                    assert( globalID_to_handle.find( vgid ) != globalID_to_handle.end() );
                    new_conn[j] = globalID_to_handle[vgid];
                    nnodes      = j + 1;  // nodes are at the beginning, and are variable number
                }
            }
            EntityHandle new_element;
            //
            EntityType entType = MBQUAD;
            if( nnodes > 4 ) entType = MBPOLYGON;
            if( nnodes < 4 ) entType = MBTRI;
            rval = mb->create_element( entType, new_conn, nnodes, new_element );ERRORR( rval, "can't create new element " );
            globalID_to_eh[globalIdEl] = new_element;
            local_q.insert( new_element );
            rval = mb->tag_set_data( gid, &new_element, 1, &globalIdEl );ERRORR( rval, "can't set gid on new element " );
        }
        remote_cells->vi_wr[2 * i]     = from_proc;
        remote_cells->vi_wr[2 * i + 1] = globalIdEl;
        //    remote_cells->vr_wr[i] = 0.; will have a different tuple for communication
        remote_cells->vul_wr[i] = TLq.vul_rd[i];  // this is the corresponding tgt cell (arrival)
        remote_cells->inc_n();
    }
    // now, create a new set, covering_set
    rval = mb->create_meshset( MESHSET_SET, covering_set );ERRORR( rval, "can't create new mesh set " );
    rval = mb->add_entities( covering_set, local_q );ERRORR( rval, "can't add entities to new mesh set " );
    // order the remote cells tuple list, with the global id, because we will search in it
    // remote_cells->print("remote_cells before sorting");
    moab::TupleList::buffer sort_buffer;
    sort_buffer.buffer_init( n );
    remote_cells->sort( 1, &sort_buffer );
    sort_buffer.reset();
    return MB_SUCCESS;
    // end copy
}

ErrorCode Intx2Mesh::resolve_intersection_sharing()
{
    if( parcomm && parcomm->size() > 1 )
    {
        /*
            moab::ParallelMergeMesh pm(parcomm, epsilon_1);
            ErrorCode rval = pm.merge(outSet, false, 2); // resolve only the output set, do not skip
           local merge, use dim 2 ERRORR(rval, "can't merge intersection ");
        */
        // look at non-owned shared vertices, that could be part of original source set
        // they should be removed from intx set reference, because they might not have a
        // correspondent on the other task
        Range nonOwnedVerts;
        Range vertsInIntx;
        Range intxCells;
        ErrorCode rval = mb->get_entities_by_dimension( outSet, 2, intxCells );MB_CHK_ERR( rval );
        rval = mb->get_connectivity( intxCells, vertsInIntx );MB_CHK_ERR( rval );

        rval = parcomm->filter_pstatus( vertsInIntx, PSTATUS_NOT_OWNED, PSTATUS_AND, -1, &nonOwnedVerts );MB_CHK_ERR( rval );

        // some of these vertices can be in original set 1, which was covered, transported;
        // but they should not be "shared" from the intx point of view, because they are not shared
        // with another task they might have come from coverage as a plain vertex, so losing the
        // sharing property ?

        Range coverVerts;
        rval = mb->get_connectivity( rs1, coverVerts );MB_CHK_ERR( rval );
        // find out those that are on the interface
        Range vertsCovInterface;
        rval = parcomm->filter_pstatus( coverVerts, PSTATUS_INTERFACE, PSTATUS_AND, -1, &vertsCovInterface );MB_CHK_ERR( rval );
        // how many of these are in
        Range nodesToDuplicate = intersect( vertsCovInterface, nonOwnedVerts );
        // first, get all cells connected to these vertices, from intxCells

        Range connectedCells;
        rval = mb->get_adjacencies( nodesToDuplicate, 2, false, connectedCells, Interface::UNION );MB_CHK_ERR( rval );
        // only those in intx set:
        connectedCells = intersect( connectedCells, intxCells );
        // first duplicate vertices in question:
        std::map< EntityHandle, EntityHandle > duplicatedVerticesMap;
        for( Range::iterator vit = nodesToDuplicate.begin(); vit != nodesToDuplicate.end(); vit++ )
        {
            EntityHandle vertex = *vit;
            double coords[3];
            rval = mb->get_coords( &vertex, 1, coords );MB_CHK_ERR( rval );
            EntityHandle newVertex;
            rval = mb->create_vertex( coords, newVertex );MB_CHK_ERR( rval );
            duplicatedVerticesMap[vertex] = newVertex;
        }

        // look now at connectedCells, and change their connectivities:
        for( Range::iterator eit = connectedCells.begin(); eit != connectedCells.end(); eit++ )
        {
            EntityHandle intxCell = *eit;
            // replace connectivity
            std::vector< EntityHandle > connectivity;
            rval = mb->get_connectivity( &intxCell, 1, connectivity );MB_CHK_ERR( rval );
            for( size_t i = 0; i < connectivity.size(); i++ )
            {
                EntityHandle currentVertex                           = connectivity[i];
                std::map< EntityHandle, EntityHandle >::iterator mit = duplicatedVerticesMap.find( currentVertex );
                if( mit != duplicatedVerticesMap.end() )
                {
                    connectivity[i] = mit->second;  // replace connectivity directly
                }
            }
            int nnodes = (int)connectivity.size();
            rval       = mb->set_connectivity( intxCell, &connectivity[0], nnodes );MB_CHK_ERR( rval );
        }
    }
    return MB_SUCCESS;
}
#endif /* MOAB_HAVE_MPI */
} /* namespace moab */
