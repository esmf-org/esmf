/*
 * ParCommGraph.cpp
 *
 */

#include "moab/ParCommGraph.hpp"
// we need to recompute adjacencies for merging to work
#include "moab/Core.hpp"
#include "AEntityFactory.hpp"

#ifdef MOAB_HAVE_ZOLTAN
#include "moab/ZoltanPartitioner.hpp"
#endif

// #define VERBOSE
// #define GRAPH_INFO

namespace moab
{
ParCommGraph::ParCommGraph( MPI_Comm joincomm, MPI_Group group1, MPI_Group group2, int coid1, int coid2 )
    : comm( joincomm ), compid1( coid1 ), compid2( coid2 )
{
    // find out the tasks from each group, in the joint communicator
    find_group_ranks( group1, comm, senderTasks );
    find_group_ranks( group2, comm, receiverTasks );

    rootSender = rootReceiver = false;
    rankInGroup1 = rankInGroup2 = rankInJoin = -1;  // not initialized, or not part of the group

    int mpierr = MPI_Group_rank( group1, &rankInGroup1 );
    if( MPI_SUCCESS != mpierr || rankInGroup1 == MPI_UNDEFINED ) rankInGroup1 = -1;

    mpierr = MPI_Group_rank( group2, &rankInGroup2 );
    if( MPI_SUCCESS != mpierr || rankInGroup2 == MPI_UNDEFINED ) rankInGroup2 = -1;

    mpierr = MPI_Comm_rank( comm, &rankInJoin );
    if( MPI_SUCCESS != mpierr )  // it should be a fatal error
        rankInJoin = -1;

    mpierr = MPI_Comm_size( comm, &joinSize );
    if( MPI_SUCCESS != mpierr )  // it should be a fatal error
        joinSize = -1;

    if( 0 == rankInGroup1 ) rootSender = true;
    if( 0 == rankInGroup2 ) rootReceiver = true;
    graph_type = INITIAL_MIGRATE;  // 0
    comm_graph = NULL;
    context_id = -1;
    cover_set  = 0;  // refers to nothing yet
}

// copy constructor will copy only few basic things; split ranges will not be copied
ParCommGraph::ParCommGraph( const ParCommGraph& src )
{
    comm          = src.comm;
    senderTasks   = src.senderTasks;    // these are the sender tasks in joint comm
    receiverTasks = src.receiverTasks;  // these are all the receiver tasks in joint comm
    rootSender    = src.rootSender;
    rootReceiver  = src.rootReceiver;
    rankInGroup1  = src.rankInGroup1;
    rankInGroup2  = src.rankInGroup2;  // group 1 is sender, 2 is receiver
    rankInJoin    = src.rankInJoin;
    joinSize      = src.joinSize;
    compid1       = src.compid1;
    compid2       = src.compid2;
    comm_graph    = NULL;
    graph_type    = src.graph_type;
    context_id    = src.context_id;
    cover_set     = src.cover_set;
    return;
}

ParCommGraph::~ParCommGraph()
{
    // TODO Auto-generated destructor stub
}

// utility to find out the ranks of the processes of a group, with respect to a joint comm,
// which spans for sure the group
// it is used locally (in the constructor), but it can be used as a utility
void ParCommGraph::find_group_ranks( MPI_Group group, MPI_Comm joincomm, std::vector< int >& ranks )
{
    MPI_Group global_grp;
    MPI_Comm_group( joincomm, &global_grp );

    int grp_size;

    MPI_Group_size( group, &grp_size );
    std::vector< int > rks( grp_size );
    ranks.resize( grp_size );

    for( int i = 0; i < grp_size; i++ )
        rks[i] = i;

    MPI_Group_translate_ranks( group, grp_size, &rks[0], global_grp, &ranks[0] );
    MPI_Group_free( &global_grp );
    return;
}

ErrorCode ParCommGraph::compute_trivial_partition( std::vector< int >& numElemsPerTaskInGroup1 )
{

    recv_graph.clear();
    recv_sizes.clear();
    sender_graph.clear();
    sender_sizes.clear();

    if( numElemsPerTaskInGroup1.size() != senderTasks.size() )
        return MB_FAILURE;  // each sender has a number of elements that it owns

    // first find out total number of elements to be sent from all senders
    int total_elems = 0;
    std::vector< int > accum;
    accum.push_back( 0 );

    int num_senders = (int)senderTasks.size();

    for( size_t k = 0; k < numElemsPerTaskInGroup1.size(); k++ )
    {
        total_elems += numElemsPerTaskInGroup1[k];
        accum.push_back( total_elems );
    }

    int num_recv = ( (int)receiverTasks.size() );
    // in trivial partition, every receiver should get about total_elems/num_receivers elements
    int num_per_receiver = (int)( total_elems / num_recv );
    int leftover         = total_elems - num_per_receiver * num_recv;

    // so receiver k will receive  [starts[k], starts[k+1] ) interval
    std::vector< int > starts;
    starts.resize( num_recv + 1 );
    starts[0] = 0;
    for( int k = 0; k < num_recv; k++ )
    {
        starts[k + 1] = starts[k] + num_per_receiver;
        if( k < leftover ) starts[k + 1]++;
    }

    // each sender will send to a number of receivers, based on how the
    // arrays starts[0:num_recv] and accum[0:sendr] overlap
    int lastUsedReceiverRank = 0;  // first receiver was not treated yet
    for( int j = 0; j < num_senders; j++ )
    {
        // we could start the receiver loop with the latest receiver that received from previous
        // sender
        for( int k = lastUsedReceiverRank; k < num_recv; k++ )
        {
            // if overlap:
            if( starts[k] < accum[j + 1] && starts[k + 1] > accum[j] )
            {
                recv_graph[receiverTasks[k]].push_back( senderTasks[j] );
                sender_graph[senderTasks[j]].push_back( receiverTasks[k] );

                // we still need to decide what is the overlap
                int sizeOverlap = 1;  // at least 1, for sure
                // 1
                if( starts[k] >= accum[j] )  // one end is starts[k]
                {
                    if( starts[k + 1] >= accum[j + 1] )  // the other end is accum[j+1]
                        sizeOverlap = accum[j + 1] - starts[k];
                    else  //
                        sizeOverlap = starts[k + 1] - starts[k];
                }
                else  // one end is accum[j]
                {
                    if( starts[k + 1] >= accum[j + 1] )  // the other end is accum[j+1]
                        sizeOverlap = accum[j + 1] - accum[j];
                    else
                        sizeOverlap = starts[k + 1] - accum[j];
                }
                recv_sizes[receiverTasks[k]].push_back( sizeOverlap );  // basically, task k will receive from
                                                                        //   sender j, sizeOverlap elems
                sender_sizes[senderTasks[j]].push_back( sizeOverlap );
                if( starts[k] > accum[j + 1] )
                {
                    lastUsedReceiverRank = k - 1;  // so next k loop will start a little higher, we
                                                   // probably finished with first few receivers (up
                                                   // to receiver lastUsedReceiverRank)
                    break;  // break the k loop, we distributed all elements from sender j to some
                            // receivers
                }
            }
        }
    }

    return MB_SUCCESS;
}

ErrorCode ParCommGraph::pack_receivers_graph( std::vector< int >& packed_recv_array )
{
    // it will basically look at local data, to pack communication graph, each receiver task will
    // have to post receives for each sender task that will send data to it; the array will be
    // communicated to root receiver, and eventually distributed to receiver tasks

    /*
     * packed_array will have receiver, number of senders, then senders, etc
     */
    if( recv_graph.size() < receiverTasks.size() )
    {
        // big problem, we have empty partitions in receive
        std::cout << " WARNING: empty partitions, some receiver tasks will receive nothing.\n";
    }
    for( std::map< int, std::vector< int > >::iterator it = recv_graph.begin(); it != recv_graph.end(); it++ )
    {
        int recv                    = it->first;
        std::vector< int >& senders = it->second;
        packed_recv_array.push_back( recv );
        packed_recv_array.push_back( (int)senders.size() );

        for( int k = 0; k < (int)senders.size(); k++ )
            packed_recv_array.push_back( senders[k] );
    }

    return MB_SUCCESS;
}

ErrorCode ParCommGraph::split_owned_range( int sender_rank, Range& owned )
{
    int senderTask                   = senderTasks[sender_rank];
    std::vector< int >& distribution = sender_sizes[senderTask];
    std::vector< int >& receivers    = sender_graph[senderTask];
    if( distribution.size() != receivers.size() )  //
        return MB_FAILURE;

    Range current = owned;  // get the full range first, then we will subtract stuff, for
    // the following ranges

    Range rleftover = current;
    for( size_t k = 0; k < receivers.size(); k++ )
    {
        Range newr;
        newr.insert( current.begin(), current.begin() + distribution[k] );
        split_ranges[receivers[k]] = newr;

        rleftover = subtract( current, newr );
        current   = rleftover;
    }

    return MB_SUCCESS;
}

// use for this the corresponding tasks and sizes
ErrorCode ParCommGraph::split_owned_range( Range& owned )
{
    if( corr_tasks.size() != corr_sizes.size() )  //
        return MB_FAILURE;

    Range current = owned;  // get the full range first, then we will subtract stuff, for
    // the following ranges

    Range rleftover = current;
    for( size_t k = 0; k < corr_tasks.size(); k++ )
    {
        Range newr;
        newr.insert( current.begin(), current.begin() + corr_sizes[k] );
        split_ranges[corr_tasks[k]] = newr;

        rleftover = subtract( current, newr );
        current   = rleftover;
    }

    return MB_SUCCESS;
}

ErrorCode ParCommGraph::send_graph( MPI_Comm jcomm )
{
    if( is_root_sender() )
    {
        int ierr;
        // will need to build a communication graph, because each sender knows now to which receiver
        // to send data the receivers need to post receives for each sender that will send data to
        // them will need to gather on rank 0 on the sender comm, global ranks of sender with
        // receivers to send build communication matrix, each receiver will receive from what sender

        std::vector< int > packed_recv_array;
        ErrorCode rval = pack_receivers_graph( packed_recv_array );
        if( MB_SUCCESS != rval ) return rval;

        int size_pack_array = (int)packed_recv_array.size();
        comm_graph          = new int[size_pack_array + 1];
        comm_graph[0]       = size_pack_array;
        for( int k = 0; k < size_pack_array; k++ )
            comm_graph[k + 1] = packed_recv_array[k];
        // will add 2 requests
        /// use tag 10 to send size and tag 20 to send the packed array
        sendReqs.resize( 1 );
        // do not send the size in advance, because we use probe now
        /*ierr = MPI_Isend(&comm_graph[0], 1, MPI_INT, receiver(0), 10, jcomm, &sendReqs[0]); // we
        have to use global communicator if (ierr!=0) return MB_FAILURE;*/
        ierr = MPI_Isend( &comm_graph[1], size_pack_array, MPI_INT, receiver( 0 ), 20, jcomm,
                          &sendReqs[0] );  // we have to use global communicator
        if( ierr != 0 ) return MB_FAILURE;
    }
    return MB_SUCCESS;
}

// pco has MOAB too get_moab()
// do we need to store "method" as a member variable ?
ErrorCode ParCommGraph::send_mesh_parts( MPI_Comm jcomm, ParallelComm* pco, Range& owned )
{

    ErrorCode rval;
    if( split_ranges.empty() )  // in trivial partition
    {
        rval = split_owned_range( rankInGroup1, owned );
        if( rval != MB_SUCCESS ) return rval;
        // we know this on the sender side:
        corr_tasks = sender_graph[senderTasks[rankInGroup1]];  // copy
        corr_sizes = sender_sizes[senderTasks[rankInGroup1]];  // another copy
    }

    int indexReq = 0;
    int ierr;                             // MPI error
    if( is_root_sender() ) indexReq = 1;  // for sendReqs
    sendReqs.resize( indexReq + split_ranges.size() );
    for( std::map< int, Range >::iterator it = split_ranges.begin(); it != split_ranges.end(); it++ )
    {
        int receiver_proc = it->first;
        Range ents        = it->second;

        // add necessary vertices too
        Range verts;
        rval = pco->get_moab()->get_adjacencies( ents, 0, false, verts, Interface::UNION );
        if( rval != MB_SUCCESS )
        {
            std::cout << " can't get adjacencies. for entities to send\n";
            return rval;
        }
        ents.merge( verts );
        ParallelComm::Buffer* buffer = new ParallelComm::Buffer( ParallelComm::INITIAL_BUFF_SIZE );
        buffer->reset_ptr( sizeof( int ) );
        rval = pco->pack_buffer( ents, false, true, false, -1, buffer );
        if( rval != MB_SUCCESS )
        {
            std::cout << " can't pack buffer for entities to send\n";
            return rval;
        }
        int size_pack = buffer->get_current_size();

        // TODO there could be an issue with endian things; check !!!!!
        // we are sending the size of the buffer first as an int!!!
        /// not anymore !
        /* ierr = MPI_Isend(buffer->mem_ptr, 1, MPI_INT, receiver_proc, 1, jcomm,
         &sendReqs[indexReq]); // we have to use global communicator if (ierr!=0) return MB_FAILURE;
         indexReq++;*/

        ierr = MPI_Isend( buffer->mem_ptr, size_pack, MPI_CHAR, receiver_proc, 2, jcomm,
                          &sendReqs[indexReq] );  // we have to use global communicator
        if( ierr != 0 ) return MB_FAILURE;
        indexReq++;
        localSendBuffs.push_back( buffer );
    }
    return MB_SUCCESS;
}

// this is called on receiver side
ErrorCode ParCommGraph::receive_comm_graph( MPI_Comm jcomm, ParallelComm* pco, std::vector< int >& pack_array )
{
    // first, receive from sender_rank 0, the communication graph (matrix), so each receiver
    // knows what data to expect
    MPI_Comm receive = pco->comm();
    int size_pack_array, ierr;
    MPI_Status status;
    if( rootReceiver )
    {
        /*
         * MPI_Probe(
        int source,
        int tag,
        MPI_Comm comm,
        MPI_Status* status)
         *
         */
        ierr = MPI_Probe( sender( 0 ), 20, jcomm, &status );
        if( 0 != ierr )
        {
            std::cout << " MPI_Probe failure: " << ierr << "\n";
            return MB_FAILURE;
        }
        // get the count of data received from the MPI_Status structure
        ierr = MPI_Get_count( &status, MPI_INT, &size_pack_array );
        if( 0 != ierr )
        {
            std::cout << " MPI_Get_count failure: " << ierr << "\n";
            return MB_FAILURE;
        }
#ifdef VERBOSE
        std::cout << " receive comm graph size: " << size_pack_array << "\n";
#endif
        pack_array.resize( size_pack_array );
        ierr = MPI_Recv( &pack_array[0], size_pack_array, MPI_INT, sender( 0 ), 20, jcomm, &status );
        if( 0 != ierr ) return MB_FAILURE;
#ifdef VERBOSE
        std::cout << " receive comm graph ";
        for( int k = 0; k < (int)pack_array.size(); k++ )
            std::cout << " " << pack_array[k];
        std::cout << "\n";
#endif
    }

    // now broadcast this whole array to all receivers, so they know what to expect
    ierr = MPI_Bcast( &size_pack_array, 1, MPI_INT, 0, receive );
    if( 0 != ierr ) return MB_FAILURE;
    pack_array.resize( size_pack_array );
    ierr = MPI_Bcast( &pack_array[0], size_pack_array, MPI_INT, 0, receive );
    if( 0 != ierr ) return MB_FAILURE;
    return MB_SUCCESS;
}

ErrorCode ParCommGraph::receive_mesh( MPI_Comm jcomm, ParallelComm* pco, EntityHandle local_set,
                                      std::vector< int >& senders_local )
{
    ErrorCode rval;
    int ierr;
    MPI_Status status;
    // we also need to fill corresponding mesh info on the other side
    corr_tasks = senders_local;
    Range newEnts;

    Tag orgSendProcTag;  // this will be a tag set on the received mesh, with info about from what
                         // task / PE the
    // primary element came from, in the joint communicator ; this will be forwarded by coverage
    // mesh
    int defaultInt = -1;  // no processor, so it was not migrated from somewhere else
    rval           = pco->get_moab()->tag_get_handle( "orig_sending_processor", 1, MB_TYPE_INTEGER, orgSendProcTag,
                                            MB_TAG_DENSE | MB_TAG_CREAT, &defaultInt );MB_CHK_SET_ERR( rval, "can't create original sending processor tag" );
    if( !senders_local.empty() )
    {
        for( size_t k = 0; k < senders_local.size(); k++ )
        {
            int sender1 = senders_local[k];
            // first receive the size of the buffer using probe
            /*
                 * MPI_Probe(
                int source,
                int tag,
                MPI_Comm comm,
                MPI_Status* status)
                 *
                 */
            ierr = MPI_Probe( sender1, 2, jcomm, &status );
            if( 0 != ierr )
            {
                std::cout << " MPI_Probe failure in ParCommGraph::receive_mesh " << ierr << "\n";
                return MB_FAILURE;
            }
            // get the count of data received from the MPI_Status structure
            int size_pack;
            ierr = MPI_Get_count( &status, MPI_CHAR, &size_pack );
            if( 0 != ierr )
            {
                std::cout << " MPI_Get_count failure in ParCommGraph::receive_mesh  " << ierr << "\n";
                return MB_FAILURE;
            }

            /* ierr = MPI_Recv (&size_pack, 1, MPI_INT, sender1, 1, jcomm, &status);
             if (0!=ierr) return MB_FAILURE;*/
            // now resize the buffer, then receive it
            ParallelComm::Buffer* buffer = new ParallelComm::Buffer( size_pack );
            // buffer->reserve(size_pack);

            ierr = MPI_Recv( buffer->mem_ptr, size_pack, MPI_CHAR, sender1, 2, jcomm, &status );
            if( 0 != ierr )
            {
                std::cout << " MPI_Recv failure in ParCommGraph::receive_mesh " << ierr << "\n";
                return MB_FAILURE;
            }
            // now unpack the buffer we just received
            Range entities;
            std::vector< std::vector< EntityHandle > > L1hloc, L1hrem;
            std::vector< std::vector< int > > L1p;
            std::vector< EntityHandle > L2hloc, L2hrem;
            std::vector< unsigned int > L2p;

            buffer->reset_ptr( sizeof( int ) );
            std::vector< EntityHandle > entities_vec( entities.size() );
            std::copy( entities.begin(), entities.end(), entities_vec.begin() );
            rval = pco->unpack_buffer( buffer->buff_ptr, false, -1, -1, L1hloc, L1hrem, L1p, L2hloc, L2hrem, L2p,
                                       entities_vec );
            delete buffer;
            if( MB_SUCCESS != rval ) return rval;

            std::copy( entities_vec.begin(), entities_vec.end(), range_inserter( entities ) );
            // we have to add them to the local set
            rval = pco->get_moab()->add_entities( local_set, entities );
            if( MB_SUCCESS != rval ) return rval;
            // corr_sizes is the size of primary entities received
            Range verts              = entities.subset_by_dimension( 0 );
            Range local_primary_ents = subtract( entities, verts );
            if( local_primary_ents.empty() )
            {
                // it is possible that all ents sent were vertices (point cloud)
                // then consider primary entities the vertices
                local_primary_ents = verts;
            }
            else
            {
                // set a tag with the original sender for the primary entity
                // will be used later for coverage mesh
                std::vector< int > orig_senders( local_primary_ents.size(), sender1 );
                rval = pco->get_moab()->tag_set_data( orgSendProcTag, local_primary_ents, &orig_senders[0] );
            }
            corr_sizes.push_back( (int)local_primary_ents.size() );

            newEnts.merge( entities );
            // make these in split ranges
            split_ranges[sender1] = local_primary_ents;

#ifdef VERBOSE
            std::ostringstream partial_outFile;

            partial_outFile << "part_send_" << sender1 << "."
                            << "recv" << rankInJoin << ".vtk";

            // the mesh contains ghosts too, but they are not part of mat/neumann set
            // write in serial the file, to see what tags are missing
            std::cout << " writing from receiver " << rankInJoin << " from sender " << sender1
                      << " entities: " << entities.size() << std::endl;
            rval = pco->get_moab()->write_file( partial_outFile.str().c_str(), 0, 0, &local_set,
                                                1 );  // everything on local set received
            if( MB_SUCCESS != rval ) return rval;
#endif
        }
    }
    // make sure adjacencies are updated on the new elements

    if( newEnts.empty() )
    {
        std::cout << " WARNING: this task did not receive any entities \n";
    }
    // in order for the merging to work, we need to be sure that the adjacencies are updated
    // (created)
    Range local_verts        = newEnts.subset_by_type( MBVERTEX );
    newEnts                  = subtract( newEnts, local_verts );
    Core* mb                 = (Core*)pco->get_moab();
    AEntityFactory* adj_fact = mb->a_entity_factory();
    if( !adj_fact->vert_elem_adjacencies() )
        adj_fact->create_vert_elem_adjacencies();
    else
    {
        for( Range::iterator it = newEnts.begin(); it != newEnts.end(); it++ )
        {
            EntityHandle eh          = *it;
            const EntityHandle* conn = NULL;
            int num_nodes            = 0;
            rval                     = mb->get_connectivity( eh, conn, num_nodes );
            if( MB_SUCCESS != rval ) return rval;
            adj_fact->notify_create_entity( eh, conn, num_nodes );
        }
    }

    return MB_SUCCESS;
}

// VSM: Why is the communicator never used. Remove the argument ?
ErrorCode ParCommGraph::release_send_buffers()
{
    int ierr, nsize = (int)sendReqs.size();
    std::vector< MPI_Status > mult_status;
    mult_status.resize( sendReqs.size() );
    ierr = MPI_Waitall( nsize, &sendReqs[0], &mult_status[0] );

    if( ierr != 0 ) return MB_FAILURE;
    // now we can free all buffers
    delete[] comm_graph;
    comm_graph = NULL;
    std::vector< ParallelComm::Buffer* >::iterator vit;
    for( vit = localSendBuffs.begin(); vit != localSendBuffs.end(); ++vit )
        delete( *vit );
    localSendBuffs.clear();
    return MB_SUCCESS;
}

// again, will use the send buffers, for nonblocking sends;
// should be the receives non-blocking too?
ErrorCode ParCommGraph::send_tag_values( MPI_Comm jcomm, ParallelComm* pco, Range& owned,
                                         std::vector< Tag >& tag_handles )
{
    // basically, owned.size() needs to be equal to sum(corr_sizes)
    // get info about the tag size, type, etc
    int ierr;
    Core* mb = (Core*)pco->get_moab();
    // get info about the tag
    //! Get the size of the specified tag in bytes
    int total_bytes_per_entity = 0;  // we need to know, to allocate buffers
    ErrorCode rval;
    std::vector< int > vect_bytes_per_tag;
#ifdef VERBOSE
    std::vector< int > tag_sizes;
#endif
    for( size_t i = 0; i < tag_handles.size(); i++ )
    {
        int bytes_per_tag;
        rval = mb->tag_get_bytes( tag_handles[i], bytes_per_tag );MB_CHK_ERR( rval );
        int tag_size1;  // length
        rval = mb->tag_get_length( tag_handles[i], tag_size1 );MB_CHK_ERR( rval );
        if( graph_type == DOF_BASED )
            bytes_per_tag = bytes_per_tag / tag_size1;  // we know we have one double per tag , per ID sent;
                                                        // could be 8 for double, 4 for int, etc
        total_bytes_per_entity += bytes_per_tag;
        vect_bytes_per_tag.push_back( bytes_per_tag );
#ifdef VERBOSE
        int tag_size;
        rval = mb->tag_get_length( tag_handles[i], tag_size );MB_CHK_ERR( rval );
        tag_sizes.push_back( tag_size );
#endif
    }

    int indexReq = 0;
    if( graph_type == INITIAL_MIGRATE )  // original send
    {
        // use the buffers data structure to allocate memory for sending the tags
        sendReqs.resize( split_ranges.size() );

        for( std::map< int, Range >::iterator it = split_ranges.begin(); it != split_ranges.end(); it++ )
        {
            int receiver_proc = it->first;
            Range ents        = it->second;  // primary entities, with the tag data
            int size_buffer   = 4 + total_bytes_per_entity *
                                      (int)ents.size();  // hopefully, below 2B; if more, we have a big problem ...
            ParallelComm::Buffer* buffer = new ParallelComm::Buffer( size_buffer );

            buffer->reset_ptr( sizeof( int ) );
            for( size_t i = 0; i < tag_handles.size(); i++ )
            {
                // copy tag data to buffer->buff_ptr, and send the buffer (we could have used
                // regular char arrays)
                rval = mb->tag_get_data( tag_handles[i], ents, (void*)( buffer->buff_ptr ) );MB_CHK_ERR( rval );
                // advance the butter
                buffer->buff_ptr += vect_bytes_per_tag[i] * ents.size();
            }
            *( (int*)buffer->mem_ptr ) = size_buffer;
            // int size_pack = buffer->get_current_size(); // debug check
            ierr = MPI_Isend( buffer->mem_ptr, size_buffer, MPI_CHAR, receiver_proc, 222, jcomm,
                              &sendReqs[indexReq] );  // we have to use global communicator
            if( ierr != 0 ) return MB_FAILURE;
            indexReq++;
            localSendBuffs.push_back( buffer );  // we will release them after nonblocking sends are completed
        }
    }
    else if( graph_type == COVERAGE )
    {
        // we know that we will need to send some tag data in a specific order
        // first, get the ids of the local elements, from owned Range; arrange the buffer in order
        // of increasing global id
        Tag gidTag;
        rval = mb->tag_get_handle( "GLOBAL_ID", gidTag );MB_CHK_ERR( rval );
        std::vector< int > gids;
        gids.resize( owned.size() );
        rval = mb->tag_get_data( gidTag, owned, &gids[0] );MB_CHK_ERR( rval );
        std::map< int, EntityHandle > gidToHandle;
        size_t i = 0;
        for( Range::iterator it = owned.begin(); it != owned.end(); it++ )
        {
            EntityHandle eh        = *it;
            gidToHandle[gids[i++]] = eh;
        }
        // now, pack the data and send it
        sendReqs.resize( involved_IDs_map.size() );
        for( std::map< int, std::vector< int > >::iterator mit = involved_IDs_map.begin();
             mit != involved_IDs_map.end(); mit++ )
        {
            int receiver_proc        = mit->first;
            std::vector< int >& eids = mit->second;
            int size_buffer          = 4 + total_bytes_per_entity *
                                      (int)eids.size();  // hopefully, below 2B; if more, we have a big problem ...
            ParallelComm::Buffer* buffer = new ParallelComm::Buffer( size_buffer );
            buffer->reset_ptr( sizeof( int ) );
#ifdef VERBOSE
            std::ofstream dbfile;
            std::stringstream outf;
            outf << "from_" << rankInJoin << "_send_to_" << receiver_proc << ".txt";
            dbfile.open( outf.str().c_str() );
            dbfile << "from " << rankInJoin << " send to " << receiver_proc << "\n";
#endif
            // copy tag data to buffer->buff_ptr, and send the buffer (we could have used regular
            // char arrays) pack data by tag, to be consistent with above, even though we loop
            // through the entities for each tag

            for( std::vector< int >::iterator it = eids.begin(); it != eids.end(); it++ )
            {
                int eID         = *it;
                EntityHandle eh = gidToHandle[eID];
                for( i = 0; i < tag_handles.size(); i++ )
                {
                    rval = mb->tag_get_data( tag_handles[i], &eh, 1, (void*)( buffer->buff_ptr ) );MB_CHK_ERR( rval );
#ifdef VERBOSE
                    dbfile << "global ID " << eID << " local handle " << mb->id_from_handle( eh ) << " vals: ";
                    double* vals = (double*)( buffer->buff_ptr );
                    for( int kk = 0; kk < tag_sizes[i]; kk++ )
                    {
                        dbfile << " " << *vals;
                        vals++;
                    }
                    dbfile << "\n";
#endif
                    buffer->buff_ptr += vect_bytes_per_tag[i];
                }
            }

#ifdef VERBOSE
            dbfile.close();
#endif
            *( (int*)buffer->mem_ptr ) = size_buffer;
            // int size_pack = buffer->get_current_size(); // debug check
            ierr = MPI_Isend( buffer->mem_ptr, size_buffer, MPI_CHAR, receiver_proc, 222, jcomm,
                              &sendReqs[indexReq] );  // we have to use global communicator
            if( ierr != 0 ) return MB_FAILURE;
            indexReq++;
            localSendBuffs.push_back( buffer );  // we will release them after nonblocking sends are completed
        }
    }
    else if( graph_type == DOF_BASED )
    {
        // need to fill up the buffer, in the order desired, send it
        // get all the tags, for all owned entities, and pack the buffers accordingly
        // we do not want to get the tags by entity, it may be too expensive
        std::vector< std::vector< double > > valuesTags;
        valuesTags.resize( tag_handles.size() );
        for( size_t i = 0; i < tag_handles.size(); i++ )
        {

            int bytes_per_tag;
            rval = mb->tag_get_bytes( tag_handles[i], bytes_per_tag );MB_CHK_ERR( rval );
            valuesTags[i].resize( owned.size() * bytes_per_tag );
            // fill the whole array, we will pick up from here
            rval = mb->tag_get_data( tag_handles[i], owned, (void*)( &( valuesTags[i][0] ) ) );MB_CHK_ERR( rval );
        }
        // now, pack the data and send it
        sendReqs.resize( involved_IDs_map.size() );
        for( std::map< int, std::vector< int > >::iterator mit = involved_IDs_map.begin();
             mit != involved_IDs_map.end(); mit++ )
        {
            int receiver_proc                   = mit->first;
            std::vector< int >& eids            = mit->second;
            std::vector< int >& index_in_values = map_index[receiver_proc];
            std::vector< int >& index_ptr       = map_ptr[receiver_proc];  // this is eids.size()+1;
            int size_buffer                     = 4 + total_bytes_per_entity *
                                      (int)eids.size();  // hopefully, below 2B; if more, we have a big problem ...
            ParallelComm::Buffer* buffer = new ParallelComm::Buffer( size_buffer );
            buffer->reset_ptr( sizeof( int ) );
#ifdef VERBOSE
            std::ofstream dbfile;
            std::stringstream outf;
            outf << "from_" << rankInJoin << "_send_to_" << receiver_proc << ".txt";
            dbfile.open( outf.str().c_str() );
            dbfile << "from " << rankInJoin << " send to " << receiver_proc << "\n";
#endif
            // copy tag data to buffer->buff_ptr, and send the buffer
            // pack data by tag, to be consistent with above
            int j = 0;
            for( std::vector< int >::iterator it = eids.begin(); it != eids.end(); it++, j++ )
            {
                int index_in_v = index_in_values[index_ptr[j]];
                for( size_t i = 0; i < tag_handles.size(); i++ )
                {
                    // right now, move just doubles; but it could be any type of tag
                    *( (double*)( buffer->buff_ptr ) ) = valuesTags[i][index_in_v];
                    buffer->buff_ptr += 8;  // we know we are working with doubles only !!!
                }
            };
            *( (int*)buffer->mem_ptr ) = size_buffer;
            // int size_pack = buffer->get_current_size(); // debug check
            ierr = MPI_Isend( buffer->mem_ptr, size_buffer, MPI_CHAR, receiver_proc, 222, jcomm,
                              &sendReqs[indexReq] );  // we have to use global communicator
            if( ierr != 0 ) return MB_FAILURE;
            indexReq++;
            localSendBuffs.push_back( buffer );  // we will release them after nonblocking sends are completed
        }
    }
    return MB_SUCCESS;
}

ErrorCode ParCommGraph::receive_tag_values( MPI_Comm jcomm, ParallelComm* pco, Range& owned,
                                            std::vector< Tag >& tag_handles )
{
    // opposite to sending, we will use blocking receives
    int ierr;
    MPI_Status status;
    // basically, owned.size() needs to be equal to sum(corr_sizes)
    // get info about the tag size, type, etc
    Core* mb = (Core*)pco->get_moab();
    // get info about the tag
    //! Get the size of the specified tag in bytes
    ErrorCode rval;
    int total_bytes_per_entity = 0;
    std::vector< int > vect_bytes_per_tag;
#ifdef VERBOSE
    std::vector< int > tag_sizes;
#endif
    for( size_t i = 0; i < tag_handles.size(); i++ )
    {
        int bytes_per_tag;
        rval = mb->tag_get_bytes( tag_handles[i], bytes_per_tag );MB_CHK_ERR( rval );
        total_bytes_per_entity += bytes_per_tag;
        vect_bytes_per_tag.push_back( bytes_per_tag );
#ifdef VERBOSE
        int tag_size;
        rval = mb->tag_get_length( tag_handles[i], tag_size );MB_CHK_ERR( rval );
        tag_sizes.push_back( tag_size );
#endif
    }

    if( graph_type == INITIAL_MIGRATE )
    {
        // std::map<int, Range> split_ranges;
        // rval = split_owned_range ( owned);MB_CHK_ERR ( rval );

        // use the buffers data structure to allocate memory for receiving the tags
        for( std::map< int, Range >::iterator it = split_ranges.begin(); it != split_ranges.end(); it++ )
        {
            int sender_proc = it->first;
            Range ents      = it->second;  // primary entities, with the tag data, we will receive
            int size_buffer = 4 + total_bytes_per_entity *
                                      (int)ents.size();  // hopefully, below 2B; if more, we have a big problem ...
            ParallelComm::Buffer* buffer = new ParallelComm::Buffer( size_buffer );

            buffer->reset_ptr( sizeof( int ) );

            *( (int*)buffer->mem_ptr ) = size_buffer;
            // int size_pack = buffer->get_current_size(); // debug check

            ierr = MPI_Recv( buffer->mem_ptr, size_buffer, MPI_CHAR, sender_proc, 222, jcomm, &status );
            if( ierr != 0 ) return MB_FAILURE;
            // now set the tag
            // copy to tag

            for( size_t i = 0; i < tag_handles.size(); i++ )
            {
                rval = mb->tag_set_data( tag_handles[i], ents, (void*)( buffer->buff_ptr ) );
                buffer->buff_ptr += vect_bytes_per_tag[i] * ents.size();
            }
            delete buffer;  // no need for it afterwards
            MB_CHK_ERR( rval );
        }
    }
    else if( graph_type == COVERAGE )  // receive buffer, then extract tag data, in a loop
    {
        // we know that we will need to receive some tag data in a specific order (by ids stored)
        // first, get the ids of the local elements, from owned Range; unpack the buffer in order
        Tag gidTag;
        rval = mb->tag_get_handle( "GLOBAL_ID", gidTag );MB_CHK_ERR( rval );
        std::vector< int > gids;
        gids.resize( owned.size() );
        rval = mb->tag_get_data( gidTag, owned, &gids[0] );MB_CHK_ERR( rval );
        std::map< int, EntityHandle > gidToHandle;
        size_t i = 0;
        for( Range::iterator it = owned.begin(); it != owned.end(); it++ )
        {
            EntityHandle eh        = *it;
            gidToHandle[gids[i++]] = eh;
        }
        //
        // now, unpack the data and set it to the tag
        for( std::map< int, std::vector< int > >::iterator mit = involved_IDs_map.begin();
             mit != involved_IDs_map.end(); mit++ )
        {
            int sender_proc          = mit->first;
            std::vector< int >& eids = mit->second;
            int size_buffer          = 4 + total_bytes_per_entity *
                                      (int)eids.size();  // hopefully, below 2B; if more, we have a big problem ...
            ParallelComm::Buffer* buffer = new ParallelComm::Buffer( size_buffer );
            buffer->reset_ptr( sizeof( int ) );
            *( (int*)buffer->mem_ptr ) = size_buffer;  // this is really not necessary, it should receive this too

            // receive the buffer
            ierr = MPI_Recv( buffer->mem_ptr, size_buffer, MPI_CHAR, sender_proc, 222, jcomm, &status );
            if( ierr != 0 ) return MB_FAILURE;
// start copy
#ifdef VERBOSE
            std::ofstream dbfile;
            std::stringstream outf;
            outf << "recvFrom_" << sender_proc << "_on_proc_" << rankInJoin << ".txt";
            dbfile.open( outf.str().c_str() );
            dbfile << "recvFrom_" << sender_proc << " on proc  " << rankInJoin << "\n";
#endif

            // copy tag data from buffer->buff_ptr
            // data is arranged by tag , and repeat the loop for each entity ()
            // maybe it should be arranged by entity now, not by tag (so one loop for entities,
            // outside)

            for( std::vector< int >::iterator it = eids.begin(); it != eids.end(); it++ )
            {
                int eID                                     = *it;
                std::map< int, EntityHandle >::iterator mit = gidToHandle.find( eID );
                if( mit == gidToHandle.end() )
                {
                    std::cout << " on rank: " << rankInJoin << " cannot find entity handle with global ID " << eID
                              << "\n";
                    return MB_FAILURE;
                }
                EntityHandle eh = mit->second;
                for( i = 0; i < tag_handles.size(); i++ )
                {
                    rval = mb->tag_set_data( tag_handles[i], &eh, 1, (void*)( buffer->buff_ptr ) );MB_CHK_ERR( rval );
#ifdef VERBOSE
                    dbfile << "global ID " << eID << " local handle " << mb->id_from_handle( eh ) << " vals: ";
                    double* vals = (double*)( buffer->buff_ptr );
                    for( int kk = 0; kk < tag_sizes[i]; kk++ )
                    {
                        dbfile << " " << *vals;
                        vals++;
                    }
                    dbfile << "\n";
#endif
                    buffer->buff_ptr += vect_bytes_per_tag[i];
                }
            }

            // delete receive buffer
            delete buffer;
#ifdef VERBOSE
            dbfile.close();
#endif
        }
    }
    else if( graph_type == DOF_BASED )
    {
        // need to fill up the values for each tag, in the order desired, from the buffer received
        //
        // get all the tags, for all owned entities, and pack the buffers accordingly
        // we do not want to get the tags by entity, it may be too expensive
        std::vector< std::vector< double > > valuesTags;
        valuesTags.resize( tag_handles.size() );
        for( size_t i = 0; i < tag_handles.size(); i++ )
        {
            int bytes_per_tag;
            rval = mb->tag_get_bytes( tag_handles[i], bytes_per_tag );MB_CHK_ERR( rval );
            valuesTags[i].resize( owned.size() * bytes_per_tag );
            // fill the whole array, we will pick up from here
            // we will fill this array, using data from received buffer
            // rval = mb->tag_get_data(owned, (void*)( &(valuesTags[i][0])) );MB_CHK_ERR ( rval );
        }
        // now, unpack the data and set the tags
        sendReqs.resize( involved_IDs_map.size() );
        for( std::map< int, std::vector< int > >::iterator mit = involved_IDs_map.begin();
             mit != involved_IDs_map.end(); mit++ )
        {
            int sender_proc                     = mit->first;
            std::vector< int >& eids            = mit->second;
            std::vector< int >& index_in_values = map_index[sender_proc];
            std::vector< int >& index_ptr       = map_ptr[sender_proc];  // this is eids.size()+1;
            int size_buffer                     = 4 + total_bytes_per_entity *
                                      (int)eids.size();  // hopefully, below 2B; if more, we have a big problem ...
            ParallelComm::Buffer* buffer = new ParallelComm::Buffer( size_buffer );
            buffer->reset_ptr( sizeof( int ) );

            // receive the buffer
            ierr = MPI_Recv( buffer->mem_ptr, size_buffer, MPI_CHAR, sender_proc, 222, jcomm, &status );
            if( ierr != 0 ) return MB_FAILURE;
            // use the values in buffer to populate valuesTag arrays, fill it up!
            int j = 0;
            for( std::vector< int >::iterator it = eids.begin(); it != eids.end(); it++, j++ )
            {
                for( size_t i = 0; i < tag_handles.size(); i++ )
                {
                    // right now, move just doubles; but it could be any type of tag
                    double val = *( (double*)( buffer->buff_ptr ) );
                    buffer->buff_ptr += 8;  // we know we are working with doubles only !!!
                    for( int k = index_ptr[j]; k < index_ptr[j + 1]; k++ )
                        valuesTags[i][index_in_values[k]] = val;
                }
            }
            // we are done with the buffer in which we received tags, release / delete it
            delete buffer;
        }
        // now we populated the values for all tags; set now the tags!
        for( size_t i = 0; i < tag_handles.size(); i++ )
        {
            // we will fill this array, using data from received buffer
            rval = mb->tag_set_data( tag_handles[i], owned, (void*)( &( valuesTags[i][0] ) ) );MB_CHK_ERR( rval );
        }
    }
    return MB_SUCCESS;
}
/*
 * for example
 */
ErrorCode ParCommGraph::settle_send_graph( TupleList& TLcovIDs )
{
    // fill involved_IDs_map with data
    // will have "receiving proc" and global id of element
    int n      = TLcovIDs.get_n();
    graph_type = COVERAGE;  // do not rely only on involved_IDs_map.size(); this can be 0 in some cases
    for( int i = 0; i < n; i++ )
    {
        int to_proc      = TLcovIDs.vi_wr[2 * i];
        int globalIdElem = TLcovIDs.vi_wr[2 * i + 1];
        involved_IDs_map[to_proc].push_back( globalIdElem );
    }
#ifdef VERBOSE
    for( std::map< int, std::vector< int > >::iterator mit = involved_IDs_map.begin(); mit != involved_IDs_map.end();
         mit++ )
    {
        std::cout << " towards task " << mit->first << " send: " << mit->second.size() << " cells " << std::endl;
        for( size_t i = 0; i < mit->second.size(); i++ )
        {
            std::cout << " " << mit->second[i];
        }
        std::cout << std::endl;
    }
#endif
    return MB_SUCCESS;
}

// this will set involved_IDs_map will store all ids to be received from one sender task
void ParCommGraph::SetReceivingAfterCoverage(
    std::map< int, std::set< int > >& idsFromProcs )  // will make sense only on receivers, right now after cov
{
    for( std::map< int, std::set< int > >::iterator mt = idsFromProcs.begin(); mt != idsFromProcs.end(); mt++ )
    {
        int fromProc            = mt->first;
        std::set< int >& setIds = mt->second;
        involved_IDs_map[fromProc].resize( setIds.size() );
        std::vector< int >& listIDs = involved_IDs_map[fromProc];
        size_t indx                 = 0;
        for( std::set< int >::iterator st = setIds.begin(); st != setIds.end(); st++ )
        {
            int valueID     = *st;
            listIDs[indx++] = valueID;
        }
    }
    graph_type = COVERAGE;
    return;
}
//#define VERBOSE
void ParCommGraph::settle_comm_by_ids( int comp, TupleList& TLBackToComp, std::vector< int >& valuesComp )
{
    // settle comm graph on comp
    if( rootSender || rootReceiver ) std::cout << " settle comm graph by id on component " << comp << "\n";
    int n = TLBackToComp.get_n();
    // third_method = true; // do not rely only on involved_IDs_map.size(); this can be 0 in some
    // cases
    std::map< int, std::set< int > > uniqueIDs;

    for( int i = 0; i < n; i++ )
    {
        int to_proc  = TLBackToComp.vi_wr[3 * i + 2];
        int globalId = TLBackToComp.vi_wr[3 * i + 1];
        uniqueIDs[to_proc].insert( globalId );
    }

    // Vector to store element
    // with respective present index
    std::vector< std::pair< int, int > > vp;
    vp.reserve( valuesComp.size() );

    // Inserting element in pair vector
    // to keep track of previous indexes in valuesComp
    for( int i = 0; i < (int)valuesComp.size(); ++i )
    {
        vp.push_back( std::make_pair( valuesComp[i], i ) );
    }
    // Sorting pair vector
    sort( vp.begin(), vp.end() );

    // vp[i].first, second

    // count now how many times some value appears in ordered (so in valuesComp)
    for( std::map< int, std::set< int > >::iterator it = uniqueIDs.begin(); it != uniqueIDs.end(); it++ )
    {
        int procId                  = it->first;
        std::set< int >& nums       = it->second;
        std::vector< int >& indx    = map_ptr[procId];
        std::vector< int >& indices = map_index[procId];
        indx.resize( nums.size() + 1 );
        int indexInVp = 0;
        int indexVal  = 0;
        indx[0]       = 0;  // start from 0
        for( std::set< int >::iterator sst = nums.begin(); sst != nums.end(); sst++, indexVal++ )
        {
            int val = *sst;
            involved_IDs_map[procId].push_back( val );
            indx[indexVal + 1] = indx[indexVal];
            while( ( indexInVp < (int)valuesComp.size() ) && ( vp[indexInVp].first <= val ) )  // should be equal !
            {
                if( vp[indexInVp].first == val )
                {
                    indx[indexVal + 1]++;
                    indices.push_back( vp[indexInVp].second );
                }
                indexInVp++;
            }
        }
    }
#ifdef VERBOSE
    std::stringstream f1;
    std::ofstream dbfile;
    f1 << "Involve_" << comp << "_" << rankInJoin << ".txt";
    dbfile.open( f1.str().c_str() );
    for( std::map< int, std::vector< int > >::iterator mit = involved_IDs_map.begin(); mit != involved_IDs_map.end();
         mit++ )
    {
        int corrTask                = mit->first;
        std::vector< int >& corrIds = mit->second;
        std::vector< int >& indx    = map_ptr[corrTask];
        std::vector< int >& indices = map_index[corrTask];

        dbfile << " towards proc " << corrTask << " \n";
        for( int i = 0; i < (int)corrIds.size(); i++ )
        {
            dbfile << corrIds[i] << " [" << indx[i] << "," << indx[i + 1] << ")  : ";
            for( int j = indx[i]; j < indx[i + 1]; j++ )
                dbfile << indices[j] << " ";
            dbfile << "\n";
        }
        dbfile << " \n";
    }
    dbfile.close();
#endif

    graph_type = DOF_BASED;
    // now we need to fill back and forth information, needed to fill the arrays
    // for example, from spectral to involved_IDs_map, in case we want to send data from
    // spectral to phys
}
//#undef VERBOSE
// new partition calculation
ErrorCode ParCommGraph::compute_partition( ParallelComm* pco, Range& owned, int met )
{
    // we are on a task on sender, and need to compute a new partition;
    // primary cells need to be distributed to nb receivers tasks
    // first, we will use graph partitioner, with zoltan;
    // in the graph that we need to build, the first layer of ghosts is needed;
    // can we avoid that ? For example, we can find out from each boundary edge/face what is the
    // other cell (on the other side), then form the global graph, and call zoltan in parallel met 1
    // would be a geometric partitioner, and met 2 would be a graph partitioner for method 1 we do
    // not need any ghost exchange

    // find first edges that are shared
    if( owned.empty() )
        return MB_SUCCESS;  // nothing to do? empty partition is not allowed, maybe we should return
                            // error?
    Core* mb = (Core*)pco->get_moab();

    double t1, t2, t3;
    t1               = MPI_Wtime();
    int primaryDim   = mb->dimension_from_handle( *owned.rbegin() );
    int interfaceDim = primaryDim - 1;  // should be 1 or 2
    Range sharedEdges;
    ErrorCode rval;

    std::vector< int > shprocs( MAX_SHARING_PROCS );
    std::vector< EntityHandle > shhandles( MAX_SHARING_PROCS );

    Tag gidTag;  //
    rval = mb->tag_get_handle( "GLOBAL_ID", gidTag );MB_CHK_ERR( rval );
    int np;
    unsigned char pstatus;

    std::multimap< int, int > extraGraphEdges;
    // std::map<int, int> adjCellsId;
    std::map< int, int > extraCellsProc;
    // if method is 2, no need to do the exchange for adjacent cells across partition boundary
    // these maps above will be empty for method 2 (geometry)
    if( 1 == met )
    {
        rval = pco->get_shared_entities( /*int other_proc*/ -1, sharedEdges, interfaceDim,
                                         /*const bool iface*/ true );MB_CHK_ERR( rval );

#ifdef VERBOSE
        std::cout << " on sender task " << pco->rank() << " number of shared interface cells " << sharedEdges.size()
                  << "\n";
#endif
        // find to what processors we need to send the ghost info about the edge
        // first determine the local graph; what elements are adjacent to each cell in owned range
        // cells that are sharing a partition interface edge, are identified first, and form a map
        TupleList TLe;                                     // tuple list for cells
        TLe.initialize( 2, 0, 1, 0, sharedEdges.size() );  // send to, id of adj cell, remote edge
        TLe.enableWriteAccess();

        std::map< EntityHandle, int > edgeToCell;  // from local boundary edge to adjacent cell id
        // will be changed after
        for( Range::iterator eit = sharedEdges.begin(); eit != sharedEdges.end(); eit++ )
        {
            EntityHandle edge = *eit;
            // get the adjacent cell
            Range adjEnts;
            rval = mb->get_adjacencies( &edge, 1, primaryDim, false, adjEnts );MB_CHK_ERR( rval );
            if( adjEnts.size() > 0 )
            {
                EntityHandle adjCell = adjEnts[0];
                int gid;
                rval = mb->tag_get_data( gidTag, &adjCell, 1, &gid );MB_CHK_ERR( rval );
                rval = pco->get_sharing_data( edge, &shprocs[0], &shhandles[0], pstatus, np );MB_CHK_ERR( rval );
                int n                = TLe.get_n();
                TLe.vi_wr[2 * n]     = shprocs[0];
                TLe.vi_wr[2 * n + 1] = gid;
                TLe.vul_wr[n]        = shhandles[0];  // the remote edge corresponding to shared edge
                edgeToCell[edge]     = gid;           // store the map between edge and local id of adj cell
                TLe.inc_n();
            }
        }

#ifdef VERBOSE
        std::stringstream ff2;
        ff2 << "TLe_" << pco->rank() << ".txt";
        TLe.print_to_file( ff2.str().c_str() );
#endif
        // send the data to the other processors:
        ( pco->proc_config().crystal_router() )->gs_transfer( 1, TLe, 0 );
        // on receiver side, each local edge will have the remote cell adjacent to it!

        int ne = TLe.get_n();
        for( int i = 0; i < ne; i++ )
        {
            int sharedProc         = TLe.vi_rd[2 * i];       // this info is coming from here, originally
            int remoteCellID       = TLe.vi_rd[2 * i + 1];   // this is the id of the remote cell, on sharedProc
            EntityHandle localCell = TLe.vul_rd[i];          // this is now local edge/face on this proc
            int localCellId        = edgeToCell[localCell];  // this is the local cell  adjacent to edge/face
            // now, we will need to add to the graph the pair <localCellId, remoteCellID>
            std::pair< int, int > extraAdj = std::make_pair( localCellId, remoteCellID );
            extraGraphEdges.insert( extraAdj );
            // adjCellsId [edgeToCell[localCell]] = remoteCellID;
            extraCellsProc[remoteCellID] = sharedProc;
#ifdef VERBOSE
            std::cout << "local ID " << edgeToCell[localCell] << " remote cell ID: " << remoteCellID << "\n";
#endif
        }
    }
    t2 = MPI_Wtime();
    if( rootSender ) std::cout << " time preparing the input for Zoltan:" << t2 - t1 << " seconds. \n";
        // so adj cells ids; need to call zoltan for parallel partition
#ifdef MOAB_HAVE_ZOLTAN
    ZoltanPartitioner* mbZTool = new ZoltanPartitioner( mb );
    if( 1 <= met )  //  partition in zoltan, either graph or geometric partitioner
    {
        std::map< int, Range > distribution;  // how to distribute owned elements by processors in receiving groups
        // in how many tasks do we want to be distributed?
        int numNewPartitions = (int)receiverTasks.size();
        Range primaryCells   = owned.subset_by_dimension( primaryDim );
        rval = mbZTool->partition_owned_cells( primaryCells, pco, extraGraphEdges, extraCellsProc, numNewPartitions,
                                               distribution, met );MB_CHK_ERR( rval );
        for( std::map< int, Range >::iterator mit = distribution.begin(); mit != distribution.end(); mit++ )
        {
            int part_index = mit->first;
            assert( part_index < numNewPartitions );
            split_ranges[receiverTasks[part_index]] = mit->second;
        }
    }
    // delete now the partitioner
    delete mbZTool;
#endif
    t3 = MPI_Wtime();
    if( rootSender ) std::cout << " time spent by Zoltan " << t3 - t2 << " seconds. \n";
    return MB_SUCCESS;
}

// after map read, we need to know what entities we need to send to receiver
ErrorCode ParCommGraph::set_split_ranges( int comp, TupleList& TLBackToComp1, std::vector< int >& valuesComp1,
                                          int lenTag, Range& ents_of_interest, int type )
{
    // settle split_ranges // same role as partitioning
    if( rootSender ) std::cout << " find split_ranges on component " << comp << "  according to read map \n";
    // Vector to store element
    // with respective present index
    int n = TLBackToComp1.get_n();
    // third_method = true; // do not rely only on involved_IDs_map.size(); this can be 0 in some
    // cases
    std::map< int, std::set< int > > uniqueIDs;

    for( int i = 0; i < n; i++ )
    {
        int to_proc  = TLBackToComp1.vi_wr[3 * i + 2];
        int globalId = TLBackToComp1.vi_wr[3 * i + 1];
        uniqueIDs[to_proc].insert( globalId );
    }

    for( int i = 0; i < (int)ents_of_interest.size(); i++ )
    {
        EntityHandle ent = ents_of_interest[i];
        for( int j = 0; j < lenTag; j++ )
        {
            int marker = valuesComp1[i * lenTag + j];
            for( auto mit = uniqueIDs.begin(); mit != uniqueIDs.end(); mit++ )
            {
                int proc                = mit->first;
                std::set< int >& setIds = mit->second;
                if( setIds.find( marker ) != setIds.end() )
                {
                    split_ranges[proc].insert( ent );
                }
            }
        }
    }

    return MB_SUCCESS;
}

// new methods to migrate mesh after reading map
ErrorCode ParCommGraph::form_tuples_to_migrate_mesh( Interface* mb, TupleList& TLv, TupleList& TLc, int type,
                                                     int lenTagType1 )
{
    // we will always need GlobalID tag
    Tag gidtag = mb->globalId_tag();
    // for Type1, we need GLOBAL_DOFS tag;
    Tag gds;
    ErrorCode rval;
    if( type == 1 )
    {
        rval = mb->tag_get_handle( "GLOBAL_DOFS", gds );
    }
    // find vertices to be sent, for each of the split_ranges procs
    std::map< int, Range > verts_to_proc;
    int numv = 0, numc = 0;
    for( auto it = split_ranges.begin(); it != split_ranges.end(); it++ )
    {
        int to_proc = it->first;
        Range verts;
        if( type != 2 )
        {
            rval = mb->get_connectivity( it->second, verts );MB_CHK_ERR( rval );
            numc += (int)it->second.size();
        }
        else
            verts = it->second;
        verts_to_proc[to_proc] = verts;
        numv += (int)verts.size();
    }
    // first vertices:
    TLv.initialize( 2, 0, 0, 3, numv );  // to proc, GLOBAL ID, 3 real coordinates
    TLv.enableWriteAccess();
    // use the global id of vertices for connectivity
    for( auto it = verts_to_proc.begin(); it != verts_to_proc.end(); it++ )
    {
        int to_proc  = it->first;
        Range& verts = it->second;
        for( Range::iterator vit = verts.begin(); vit != verts.end(); ++vit )
        {
            EntityHandle v   = *vit;
            int n            = TLv.get_n();  // current size of tuple list
            TLv.vi_wr[2 * n] = to_proc;      // send to processor

            rval = mb->tag_get_data( gidtag, &v, 1, &( TLv.vi_wr[2 * n + 1] ) );MB_CHK_ERR( rval );
            rval = mb->get_coords( &v, 1, &( TLv.vr_wr[3 * n] ) );MB_CHK_ERR( rval );
            TLv.inc_n();  // increment tuple list size
        }
    }
    if( type == 2 ) return MB_SUCCESS;  // no need to fill TLc
    // to proc, ID cell, gdstag, nbv, id conn,
    int size_tuple = 2 + ( ( type != 1 ) ? 0 : lenTagType1 ) + 1 + 10;  // 10 is the max number of vertices in cell

    std::vector< int > gdvals;

    TLc.initialize( size_tuple, 0, 0, 0, numc );  // to proc, GLOBAL ID, 3 real coordinates
    TLc.enableWriteAccess();
    for( auto it = split_ranges.begin(); it != split_ranges.end(); it++ )
    {
        int to_proc  = it->first;
        Range& cells = it->second;
        for( Range::iterator cit = cells.begin(); cit != cells.end(); ++cit )
        {
            EntityHandle cell         = *cit;
            int n                     = TLc.get_n();  // current size of tuple list
            TLc.vi_wr[size_tuple * n] = to_proc;
            int current_index         = 2;
            rval                      = mb->tag_get_data( gidtag, &cell, 1, &( TLc.vi_wr[size_tuple * n + 1] ) );MB_CHK_ERR( rval );
            if( 1 == type )
            {
                rval = mb->tag_get_data( gds, &cell, 1, &( TLc.vi_wr[size_tuple * n + current_index] ) );MB_CHK_ERR( rval );
                current_index += lenTagType1;
            }
            // now get connectivity
            const EntityHandle* conn = NULL;
            int nnodes               = 0;
            rval                     = mb->get_connectivity( cell, conn, nnodes );MB_CHK_ERR( rval );
            // fill nnodes:
            TLc.vi_wr[size_tuple * n + current_index] = nnodes;
            rval = mb->tag_get_data( gidtag, conn, nnodes, &( TLc.vi_wr[size_tuple * n + current_index + 1] ) );MB_CHK_ERR( rval );
            TLc.inc_n();  // increment tuple list size
        }
    }
    return MB_SUCCESS;
}
ErrorCode ParCommGraph::form_mesh_from_tuples( Interface* mb, TupleList& TLv, TupleList& TLc, int type, int lenTagType1,
                                               EntityHandle fset, Range& primary_ents,
                                               std::vector< int >& values_entities )
{
    // might need to fill also the split_range things
    // we will always need GlobalID tag
    Tag gidtag = mb->globalId_tag();
    // for Type1, we need GLOBAL_DOFS tag;
    Tag gds;
    ErrorCode rval;
    std::vector< int > def_val( lenTagType1, 0 );
    if( type == 1 )
    {
        // we may need to create this tag
        rval = mb->tag_get_handle( "GLOBAL_DOFS", lenTagType1, MB_TYPE_INTEGER, gds, MB_TAG_CREAT | MB_TAG_DENSE,
                                   &def_val[0] );MB_CHK_ERR( rval );
    }

    std::map< int, EntityHandle > vertexMap;  //
    Range verts;
    // always form vertices and add them to the fset;
    int n = TLv.get_n();
    EntityHandle vertex;
    for( int i = 0; i < n; i++ )
    {
        int gid = TLv.vi_rd[2 * i + 1];
        if( vertexMap.find( gid ) == vertexMap.end() )
        {
            // need to form this vertex
            rval = mb->create_vertex( &( TLv.vr_rd[3 * i] ), vertex );MB_CHK_ERR( rval );
            vertexMap[gid] = vertex;
            verts.insert( vertex );
            rval = mb->tag_set_data( gidtag, &vertex, 1, &gid );MB_CHK_ERR( rval );
            // if point cloud,
        }
        if( 2 == type )  // point cloud, add it to the split_ranges ?
        {
            split_ranges[TLv.vi_rd[2 * i]].insert( vertexMap[gid] );
        }
        // todo : when to add the values_entities ?
    }
    rval = mb->add_entities( fset, verts );MB_CHK_ERR( rval );
    if( 2 == type )
    {
        primary_ents = verts;
        values_entities.resize( verts.size() );  // just get the ids of vertices
        rval = mb->tag_get_data( gidtag, verts, &values_entities[0] );MB_CHK_ERR( rval );
        return MB_SUCCESS;
    }

    n              = TLc.get_n();
    int size_tuple = 2 + ( ( type != 1 ) ? 0 : lenTagType1 ) + 1 + 10;  // 10 is the max number of vertices in cell

    EntityHandle new_element;
    Range cells;
    std::map< int, EntityHandle > cellMap;  // do no tcreate one if it already exists, maybe from other processes
    for( int i = 0; i < n; i++ )
    {
        int from_proc  = TLc.vi_rd[size_tuple * i];
        int globalIdEl = TLc.vi_rd[size_tuple * i + 1];
        if( cellMap.find( globalIdEl ) == cellMap.end() )  // need to create the cell
        {
            int current_index = 2;
            if( 1 == type ) current_index += lenTagType1;
            int nnodes = TLc.vi_rd[size_tuple * i + current_index];
            std::vector< EntityHandle > conn;
            conn.resize( nnodes );
            for( int j = 0; j < nnodes; j++ )
            {
                conn[j] = vertexMap[TLc.vi_rd[size_tuple * i + current_index + j + 1]];
            }
            //
            EntityType entType = MBQUAD;
            if( nnodes > 4 ) entType = MBPOLYGON;
            if( nnodes < 4 ) entType = MBTRI;
            rval = mb->create_element( entType, &conn[0], nnodes, new_element );MB_CHK_SET_ERR( rval, "can't create new element " );
            cells.insert( new_element );
            cellMap[globalIdEl] = new_element;
            rval                = mb->tag_set_data( gidtag, &new_element, 1, &globalIdEl );MB_CHK_SET_ERR( rval, "can't set global id tag on cell " );
            if( 1 == type )
            {
                // set the gds tag
                rval = mb->tag_set_data( gds, &new_element, 1, &( TLc.vi_rd[size_tuple * i + 2] ) );MB_CHK_SET_ERR( rval, "can't set gds tag on cell " );
            }
        }
        split_ranges[from_proc].insert( cellMap[globalIdEl] );
    }
    rval = mb->add_entities( fset, cells );MB_CHK_ERR( rval );
    primary_ents = cells;
    if( 1 == type )
    {
        values_entities.resize( lenTagType1 * primary_ents.size() );
        rval = mb->tag_get_data( gds, primary_ents, &values_entities[0] );MB_CHK_ERR( rval );
    }
    else  // type == 3
    {
        values_entities.resize( primary_ents.size() );  // just get the ids !
        rval = mb->tag_get_data( gidtag, primary_ents, &values_entities[0] );MB_CHK_ERR( rval );
    }
    return MB_SUCCESS;
}

// at this moment, each sender task has split_ranges formed;
// we need to aggregate that info and send it to receiver
ErrorCode ParCommGraph::send_graph_partition( ParallelComm* pco, MPI_Comm jcomm )
{
    // first, accumulate the info to root of sender; use gatherv
    // first, accumulate number of receivers from each sender, to the root receiver
    int numberReceivers =
        (int)split_ranges.size();            // these are ranges of elements to be sent to each receiver, from this task
    int nSenders = (int)senderTasks.size();  //
    // this sender will have to send to this many receivers
    std::vector< int > displs( 1 );  // displacements for gatherv
    std::vector< int > counts( 1 );
    if( is_root_sender() )
    {
        displs.resize( nSenders + 1 );
        counts.resize( nSenders );
    }

    int ierr = MPI_Gather( &numberReceivers, 1, MPI_INT, &counts[0], 1, MPI_INT, 0, pco->comm() );
    if( ierr != MPI_SUCCESS ) return MB_FAILURE;
    // compute now displacements
    if( is_root_sender() )
    {
        displs[0] = 0;
        for( int k = 0; k < nSenders; k++ )
        {
            displs[k + 1] = displs[k] + counts[k];
        }
    }
    std::vector< int > buffer;
    if( is_root_sender() ) buffer.resize( displs[nSenders] );  // the last one will have the total count now

    std::vector< int > recvs;
    for( std::map< int, Range >::iterator mit = split_ranges.begin(); mit != split_ranges.end(); mit++ )
    {
        recvs.push_back( mit->first );
    }
    ierr =
        MPI_Gatherv( &recvs[0], numberReceivers, MPI_INT, &buffer[0], &counts[0], &displs[0], MPI_INT, 0, pco->comm() );
    if( ierr != MPI_SUCCESS ) return MB_FAILURE;

    // now form recv_graph map; points from the
    // now form the graph to be sent to the other side; only on root
    if( is_root_sender() )
    {
#ifdef GRAPH_INFO
        std::ofstream dbfileSender;
        std::stringstream outf;
        outf << "S_" << compid1 << "_R_" << compid2 << "_SenderGraph.txt";
        dbfileSender.open( outf.str().c_str() );
        dbfileSender << " number senders: " << nSenders << "\n";
        dbfileSender << " senderRank \treceivers \n";
        for( int k = 0; k < nSenders; k++ )
        {
            int indexInBuff = displs[k];
            int senderTask  = senderTasks[k];
            dbfileSender << senderTask << "\t\t";
            for( int j = 0; j < counts[k]; j++ )
            {
                int recvTask = buffer[indexInBuff + j];
                dbfileSender << recvTask << " ";
            }
            dbfileSender << "\n";
        }
        dbfileSender.close();
#endif
        for( int k = 0; k < nSenders; k++ )
        {
            int indexInBuff = displs[k];
            int senderTask  = senderTasks[k];
            for( int j = 0; j < counts[k]; j++ )
            {
                int recvTask = buffer[indexInBuff + j];
                recv_graph[recvTask].push_back( senderTask );  // this will be packed and sent to root receiver, with
                                                               // nonblocking send
            }
        }
#ifdef GRAPH_INFO
        std::ofstream dbfile;
        std::stringstream outf2;
        outf2 << "S_" << compid1 << "_R_" << compid2 << "_RecvGraph.txt";
        dbfile.open( outf2.str().c_str() );
        dbfile << " number receivers: " << recv_graph.size() << "\n";
        dbfile << " receiverRank \tsenders \n";
        for( std::map< int, std::vector< int > >::iterator mit = recv_graph.begin(); mit != recv_graph.end(); mit++ )
        {
            int recvTask                = mit->first;
            std::vector< int >& senders = mit->second;
            dbfile << recvTask << "\t\t";
            for( std::vector< int >::iterator vit = senders.begin(); vit != senders.end(); vit++ )
                dbfile << *vit << " ";
            dbfile << "\n";
        }
        dbfile.close();
#endif
        // this is the same as trivial partition
        ErrorCode rval = send_graph( jcomm );MB_CHK_ERR( rval );
    }

    return MB_SUCCESS;
}
// method to expose local graph info: sender id, receiver id, sizes of elements to send, after or
// before intersection
ErrorCode ParCommGraph::dump_comm_information( std::string prefix, int is_send )
{
    //
    if( -1 != rankInGroup1 && 1 == is_send )  // it is a sender task
    {
        std::ofstream dbfile;
        std::stringstream outf;
        outf << prefix << "_sender_" << rankInGroup1 << "_joint" << rankInJoin << "_type_" << (int)graph_type << ".txt";
        dbfile.open( outf.str().c_str() );

        if( graph_type == COVERAGE )
        {
            for( std::map< int, std::vector< int > >::iterator mit = involved_IDs_map.begin();
                 mit != involved_IDs_map.end(); mit++ )
            {
                int receiver_proc        = mit->first;
                std::vector< int >& eids = mit->second;
                dbfile << "receiver: " << receiver_proc << " size:" << eids.size() << "\n";
            }
        }
        else if( graph_type == INITIAL_MIGRATE )  // just after migration
        {
            for( std::map< int, Range >::iterator mit = split_ranges.begin(); mit != split_ranges.end(); mit++ )
            {
                int receiver_proc = mit->first;
                Range& eids       = mit->second;
                dbfile << "receiver: " << receiver_proc << " size:" << eids.size() << "\n";
            }
        }
        else if( graph_type == DOF_BASED )  // just after migration, or from computeGraph
        {
            for( std::map< int, std::vector< int > >::iterator mit = involved_IDs_map.begin();
                 mit != involved_IDs_map.end(); mit++ )
            {
                int receiver_proc = mit->first;
                dbfile << "receiver: " << receiver_proc << " size:" << mit->second.size() << "\n";
            }
        }
        dbfile.close();
    }
    if( -1 != rankInGroup2 && 0 == is_send )  // it is a receiver task
    {
        std::ofstream dbfile;
        std::stringstream outf;
        outf << prefix << "_receiver_" << rankInGroup2 << "_joint" << rankInJoin << "_type_" << (int)graph_type
             << ".txt";
        dbfile.open( outf.str().c_str() );

        if( graph_type == COVERAGE )
        {
            for( std::map< int, std::vector< int > >::iterator mit = involved_IDs_map.begin();
                 mit != involved_IDs_map.end(); mit++ )
            {
                int sender_proc          = mit->first;
                std::vector< int >& eids = mit->second;
                dbfile << "sender: " << sender_proc << " size:" << eids.size() << "\n";
            }
        }
        else if( graph_type == INITIAL_MIGRATE )  // just after migration
        {
            for( std::map< int, Range >::iterator mit = split_ranges.begin(); mit != split_ranges.end(); mit++ )
            {
                int sender_proc = mit->first;
                Range& eids     = mit->second;
                dbfile << "sender: " << sender_proc << " size:" << eids.size() << "\n";
            }
        }
        else if( graph_type == DOF_BASED )  // just after migration
        {
            for( std::map< int, std::vector< int > >::iterator mit = involved_IDs_map.begin();
                 mit != involved_IDs_map.end(); mit++ )
            {
                int sender_proc = mit->first;
                dbfile << "receiver: " << sender_proc << " size:" << mit->second.size() << "\n";
            }
        }
        dbfile.close();
    }
    return MB_SUCCESS;
}
}  // namespace moab
