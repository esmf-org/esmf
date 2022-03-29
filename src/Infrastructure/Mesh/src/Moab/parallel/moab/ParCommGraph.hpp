/*
 * ParCommGraph.hpp
 *
 *  will be used to setup communication between 2 distributed meshes, in which one mesh was migrated
 * from the other. (one example is atmosphere mesh migrated to coupler pes)
 *
 *  there are 3 communicators in play, one for each mesh, and one for the joined
 *  communicator, that spans both sets of processes; to send mesh or tag data we need to use the
 * joint communicator, use nonblocking MPI_iSend and blocking MPI_Recv receives
 *
 *  various methods should be available to migrate meshes; trivial, using graph partitioner (Zoltan
 * PHG) and using a geometric partitioner  (Zoltan RCB)
 *
 *  communicators are represented by their MPI groups, not by their communicators, because
 *  the groups are always defined, irrespective of what tasks are they on. Communicators can be
 * MPI_NULL, while MPI_Groups are always defined
 *
 *  Some of the methods in here are executed over the sender communicator, some are over the
 * receiver communicator They can switch places, what was sender becomes the receiver and viceversa
 *
 *  The name "graph" is in the sense of a bipartite graph, in which we can separate senders and
 * receivers tasks
 *
 *  The info stored in the ParCommGraph helps in migrating fields (MOAB tags) from component to the
 * coupler and back
 *
 *  So initially the ParCommGraph is assisting in mesh migration (from component to coupler) and
 * then is used to migrate tag data from component to coupler and back from coupler to component.
 *
 *  The same class is used after intersection (which is done on the coupler pes between 2 different
 * component migrated meshes) and it alters communication pattern between the original component pes
 * and coupler pes;
 *
 *   We added a new way to send tags between 2 models; the first application of the new method is to
 * send tag from atm dynamics model (spectral elements, with np x np tags defined on each element,
 * according to the GLOBAL_DOFS tag associated to each element) towards the atm physics model, which
 * is just a point cloud of vertices distributed differently to the physics model pes; matching is
 * done using GLOBAL_ID tag on vertices; Right now, we assume that the models are on different pes,
 * but the joint communicator covers both and that the ids of the tasks are with respect to the
 * joint communicator
 *
 *
 */
#include "moab_mpi.h"
#include "moab/Interface.hpp"
#include "moab/ParallelComm.hpp"
#include <map>

#ifndef SRC_PARALLEL_MOAB_PARCOMMGRAPH_HPP_
#define SRC_PARALLEL_MOAB_PARCOMMGRAPH_HPP_

namespace moab
{

class ParCommGraph
{
  public:
    enum TypeGraph
    {
        INITIAL_MIGRATE,
        COVERAGE,
        DOF_BASED
    };
    virtual ~ParCommGraph();

    /**
     * \brief collective constructor, will be called on all sender tasks and receiver tasks
     * \param[in]  joincomm  joint MPI communicator that covers both sender and receiver MPI groups
     * \param[in]  group1   MPI group formed with sender tasks; (sender usually loads the mesh in a
     * migrate scenario) \param[in]  group2   MPI group formed with receiver tasks; (receiver
     * usually receives the mesh in a migrate scenario) \param[in]  coid1    sender component unique
     * identifier in a coupled application (in climate simulations could be the Atmosphere Comp id =
     * 5 or Ocean Comp ID , 17) \param[in]  coid2    receiver component unique identifier in a
     * coupled application (it is usually the coupler, 2, in E3SM)
     *
     * this graph will be formed on sender and receiver tasks, and, in principle, will hold info
     * about how the local entities are distributed on the other side
     *
     * Its major role is to help in migration of data, from component to the coupler and vice-versa;
     * Each local entity has a corresponding task (or tasks) on the other side, to where the data
     * needs to be sent
     *
     * important data stored in ParCommGraph, immediately it is created
     *   - all sender and receiver tasks ids, with respect to the joint communicator
     *   - local rank in sender and receiver group (-1 if not part of the respective group)
     *   - rank in the joint communicator (from 0)
     */
    ParCommGraph( MPI_Comm joincomm, MPI_Group group1, MPI_Group group2, int coid1, int coid2 );

    /**
     * \brief copy constructor will copy only the senders, receivers, compid1, etc
     */
    ParCommGraph( const ParCommGraph& );

    /**
      \brief  Based on the number of elements on each task in group 1, partition for group 2,
   trivially

   <B>Operations:</B> it is called on every receiver task; decides how are all elements distributed

    Note:  establish how many elements are sent from each task in group 1 to tasks in group 2
          This call is usually made on a root / master process, and will construct local maps that
   are member data, which contain the communication graph, in both directions Also, number of
   elements migrated/exchanged between each sender/receiver

     \param[in]  numElemsPerTaskInGroup1 (std::vector<int> &)  number of elements on each sender
   task
     */

    ErrorCode compute_trivial_partition( std::vector< int >& numElemsPerTaskInGroup1 );

    /**
       \brief  pack information about receivers view of the graph, for future sending to receiver
      root

      <B>Operations:</B> Local, called on root process of the senders group

       \param[out] packed_recv_array
         packed data will be sent to the root of receivers, and distributed from there, and
           will have this information, for each receiver, concatenated
         receiver 1 task, number of senders for receiver 1, then sender tasks for receiver 1,
      receiver 2 task, number of senders for receiver 2, sender tasks for receiver 2, etc Note: only
      the root of senders will compute this, and send it over to the receiver root, which will
        distribute it over each receiver; We do not pack the sizes of data to be sent, only the
      senders for each of the receivers (could be of size O(n^2) , where n is the number of tasks ;
      but in general, it should be more like O(n) ). Each sender sends to a "finite" number of
      receivers, and each receiver receives from a finite number of senders). We need this info to
      decide how to set up the send/receive waiting game for non-blocking communication )
     */
    ErrorCode pack_receivers_graph( std::vector< int >& packed_recv_array );

    // get methods for private data
    bool is_root_sender()
    {
        return rootSender;
    }

    bool is_root_receiver()
    {
        return rootReceiver;
    }

    int sender( int index )
    {
        return senderTasks[index];
    }

    int receiver( int index )
    {
        return receiverTasks[index];
    }

    int get_component_id1()
    {
        return compid1;
    }
    int get_component_id2()
    {
        return compid2;
    }

    int get_context_id()
    {
        return context_id;
    }
    void set_context_id( int other_id )
    {
        context_id = other_id;
    }

    EntityHandle get_cover_set()
    {
        return cover_set;
    }
    void set_cover_set( EntityHandle cover )
    {
        cover_set = cover;
    }

    // return local graph for a specific task
    ErrorCode split_owned_range( int sender_rank, Range& owned );

    ErrorCode split_owned_range( Range& owned );

    ErrorCode send_graph( MPI_Comm jcomm );

    ErrorCode send_graph_partition( ParallelComm* pco, MPI_Comm jcomm );

    ErrorCode send_mesh_parts( MPI_Comm jcomm, ParallelComm* pco, Range& owned );

    // this is called on receiver side
    ErrorCode receive_comm_graph( MPI_Comm jcomm, ParallelComm* pco, std::vector< int >& pack_array );

    ErrorCode receive_mesh( MPI_Comm jcomm, ParallelComm* pco, EntityHandle local_set,
                            std::vector< int >& senders_local );

    ErrorCode release_send_buffers();

    ErrorCode send_tag_values( MPI_Comm jcomm, ParallelComm* pco, Range& owned, std::vector< Tag >& tag_handles );

    ErrorCode receive_tag_values( MPI_Comm jcomm, ParallelComm* pco, Range& owned, std::vector< Tag >& tag_handles );

    // getter method
    const std::vector< int >& senders()
    {
        return senderTasks;
    }  // reference copy; refers to sender tasks in joint comm
    const std::vector< int >& receivers()
    {
        return receiverTasks;
    }

    ErrorCode settle_send_graph( TupleList& TLcovIDs );

    // this will set after_cov_rec_sizes
    void SetReceivingAfterCoverage(
        std::map< int, std::set< int > >& idsFromProcs );  // will make sense only on receivers, right now after cov

    // strideComp is np x np, or 1, in our cases
    // will fill up ordered lists for corresponding IDs on the other component
    // will form back and forth information, from ordered list of IDs, to valuesComp
    void settle_comm_by_ids( int comp, TupleList& TLBackToComp, std::vector< int >& valuesComp );

    // after map read, we need to know what entities we need to send to receiver
    ErrorCode set_split_ranges( int comp, TupleList& TLBackToComp1, std::vector< int >& valuesComp1, int lenTag,
                                Range& ents_of_interest, int type );

    // new methods to migrate mesh after reading map
    ErrorCode form_tuples_to_migrate_mesh( Interface* mb, TupleList& TLv, TupleList& TLc, int type, int lenTagType1 );
    ErrorCode form_mesh_from_tuples( Interface* mb, TupleList& TLv, TupleList& TLc, int type, int lenTagType1,
                                     EntityHandle fset, Range& primary_ents, std::vector< int >& values_entities );

    // new partition calculation
    ErrorCode compute_partition( ParallelComm* pco, Range& owned, int met );

    // dump local information about graph
    ErrorCode dump_comm_information( std::string prefix, int is_send );

  private:
    /**
    \brief find ranks of a group with respect to an encompassing communicator

    <B>Operations:</B> Local, usually called on root process of the group

    \param[in]  joincomm (MPI_Comm)
    \param[in]  group (MPI_Group)
    \param[out] ranks ( std::vector<int>)  ranks with respect to the joint communicator
  */
    void find_group_ranks( MPI_Group group, MPI_Comm join, std::vector< int >& ranks );

    MPI_Comm comm;
    std::vector< int > senderTasks;    // these are the sender tasks in joint comm
    std::vector< int > receiverTasks;  // these are all the receiver tasks in joint comm
    bool rootSender;
    bool rootReceiver;
    int rankInGroup1, rankInGroup2;  // group 1 is sender, 2 is receiver
    int rankInJoin, joinSize;
    int compid1, compid2;
    int context_id;          // used to identify the other comp for intersection
    EntityHandle cover_set;  // will be initialized only if it is the receiver parcomm graph, in
                             // CoverageGraph

    // communication graph from group1 to group2;
    //  graph[task1] = vec1; // vec1 is a stl vector of tasks in group2
    std::map< int, std::vector< int > > recv_graph;  // to what tasks from group2 to send  (actual communication graph)
    std::map< int, std::vector< int > >
        recv_sizes;  // how many elements to actually send from a sender task to receiver tasks
    std::map< int, std::vector< int > >
        sender_graph;  // to what tasks from group2 to send  (actual communication graph)
    std::map< int, std::vector< int > >
        sender_sizes;  // how many elements to actually send from a sender task to receiver tasks

    std::vector< ParallelComm::Buffer* > localSendBuffs;  // this will store the pointers to the Buffers
    //                                    will be  released only when all mpi requests are waited
    //                                    for
    int* comm_graph;  // this will store communication graph, on sender master, sent by nonblocking
                      // send to the master receiver first integer will be the size of the graph,
                      // the rest will be the packed graph, for trivial partition

    // these will be now used to store ranges to be sent from current sender to each receiver in
    // joint comm
    std::map< int, Range > split_ranges;

    std::vector< MPI_Request > sendReqs;  // there will be multiple requests, 2 for comm graph, 2 for each Buffer
    // there are as many buffers as sender_graph[rankInJoin].size()

    // active on both receiver and sender sides
    std::vector< int > corr_tasks;  // subset of the senderTasks, in the joint comm for sender;
                                    // subset of receiverTasks for receiver side
    std::vector< int > corr_sizes;  // how many primary entities corresponding to the other side
    // so what we know is that the local range corresponds to remote corr_sizes[i] size ranges on
    // tasks corr_tasks[i]

    // these will be used now after coverage, quick fix; they will also be populated by
    // iMOAB_CoverageGraph
    TypeGraph graph_type;  // this should be false , set to true in settle send graph, to use send_IDs_map
    std::map< int, std::vector< int > > involved_IDs_map;  // replace send and recv IDs_mapp with involved_IDs_map
    // used only for third method: DOF_BASED
    std::map< int, std::vector< int > >
        map_index;  // from index in involved[] to index in values[] of tag, for each corr task
    std::map< int, std::vector< int > > map_ptr;  //  lmap[ie], lmap[ie+1], pointer into map_index[corrTask]
};

}  // namespace moab
#endif /* SRC_PARALLEL_MOAB_PARCOMMGRAPH_HPP_ */
