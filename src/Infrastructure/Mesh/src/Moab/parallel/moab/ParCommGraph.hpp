/*
 * ParCommGraph.hpp
 *
 *  will be used to setup communication between 2 separate meshes, on
 *  different communicators;
 *  there are 3 communicators in play, one for each mesh, and one for the joined
 *  communicator, that spans both sets of processes
 *
 *  various methods should be available to partition meshes between the 2 communicators
 *  communicators are represented by their MPI groups, not by their communicators, because
 *  the groups are always defined, irrespective of what tasks are they on.
 *  Some of the methods in here are executed over the sender communicator, some are over the receiver communicator
 *
 */
#include "moab_mpi.h"
#include "moab/Interface.hpp"
#include "moab/ParallelComm.hpp"
#include <map>


#ifndef SRC_PARALLEL_MOAB_PARCOMMGRAPH_HPP_
#define SRC_PARALLEL_MOAB_PARCOMMGRAPH_HPP_


namespace moab {

	class ParCommGraph {
	public:
	  // ParCommGraph();
	  virtual ~ParCommGraph();

	  // collective constructor, will be called on all sender tasks and receiver tasks
	  ParCommGraph(MPI_Comm joincomm, MPI_Group group1, MPI_Group group2, int coid1, int coid2);

	  /**
	    \brief  Based on the number of elements on each task in group 1, partition for group 2, trivially

     <B>Operations:</B> Local, usually called on root process of the group

      Note:  establish how many elements are sent from each task in group 1 to tasks in group 2
            This call is usually made on a root / master process, and will construct local maps that are member data,
            which contain the communication graph, in both directions
            Also, number of elements migrated/exchanged between each sender/receiver

       \param[in]  numElemsPerTaskInGroup1 (std::vector<int> &)  number of elements on each sender task
	   */
	  ErrorCode compute_trivial_partition (std::vector<int> & numElemsPerTaskInGroup1);

	  /**
	     \brief  pack information about receivers view of the graph, for future sending to receiver root

        <B>Operations:</B> Local, usually called on root process of the group

	     \param[out] packed_recv_array
	       packed data will be sent eventually to the root of receivers, and distributed from there, and
	         will have this information, for each receiver, concatenated
	          receiver 1 task, number of senders for receiver 1, then sender tasks for receiver 1, receiver 2 task,
	            number of senders for receiver 2, sender tasks for receiver 2, etc
	   */
	  ErrorCode pack_receivers_graph(std::vector<int> & packed_recv_array );

	  /**
	   * \brief  distribute send information (as decided on root) to all senders in the group
	   * <B>Operations:</B> collective, needs to be called on all tasks on sender group
	   *
	   * sending info will contain the rank of the receivers and number of elements
	   *   (sizes) for each receiver  recv1, size1, recv2, size2, ...
	   *   size of the array will be the number of receivers times 2
	   * \param[in]      senderComm(MPI_Comm)
	   * \param[out]     sendingInfo(std::vector<int> & )
	   */
	  ErrorCode distribute_sender_graph_info(MPI_Comm senderComm, std::vector<int> &sendingInfo );

	  // get methods for private data
	  bool is_root_sender() { return rootSender;}

	  bool is_root_receiver () { return rootReceiver;}

	  int sender(int index) {return senderTasks[index];}

	  int receiver(int index) {return receiverTasks[index];}

	  int get_component_id1(){return compid1;}
	  int get_component_id2(){return compid2;}

	  // return local graph for a specific task
	  ErrorCode split_owned_range (int sender_rank, Range & owned);

	  ErrorCode split_owned_range (Range & owned);

	  ErrorCode send_graph(MPI_Comm jcomm);

	  ErrorCode send_graph_partition (ParallelComm *pco, MPI_Comm jcomm);

	  ErrorCode send_mesh_parts(MPI_Comm jcomm, ParallelComm * pco, Range & owned );

	  // this is called on receiver side
	  ErrorCode receive_comm_graph(MPI_Comm jcomm, ParallelComm *pco, std::vector<int> & pack_array);

    ErrorCode receive_mesh(MPI_Comm jcomm, ParallelComm *pco, EntityHandle local_set,
        std::vector<int> &senders_local);

	  ErrorCode release_send_buffers(MPI_Comm jcomm);

	  ErrorCode send_tag_values (MPI_Comm jcomm, ParallelComm *pco, Range & owned,
	      std::vector<Tag> & tag_handles );

	  ErrorCode receive_tag_values (MPI_Comm jcomm, ParallelComm *pco, Range & owned,
        std::vector<Tag> & tag_handles );

	  // getter method
	  const std::vector<int> & senders() { return senderTasks; } //reference copy; refers to sender tasks in joint comm
	  const std::vector<int> & receivers() { return receiverTasks; }

	  ErrorCode settle_send_graph(TupleList & TLcovIDs);

	  // this will set after_cov_rec_sizes
	  void SetReceivingAfterCoverage(std::map<int, std::set<int> > & idsFromProcs); // will make sense only on receivers, right now after cov

	  // new partition calculation
	  ErrorCode compute_partition (ParallelComm *pco, Range & owned, int met);
	private:
	  /**
      \brief find ranks of a group with respect to an encompassing communicator

      <B>Operations:</B> Local, usually called on root process of the group

      \param[in]  joincomm (MPI_Comm)
      \param[in]  group (MPI_Group)
      \param[out] ranks ( std::vector<int>)  ranks with respect to the joint communicator
    */
	  void find_group_ranks(MPI_Group group, MPI_Comm join, std::vector<int> & ranks);

	  MPI_Comm  comm;
	  std::vector<int>  senderTasks;  // these are the sender tasks in joint comm
	  std::vector<int>  receiverTasks; // these are all the receiver tasks in joint comm
	  bool rootSender;
	  bool rootReceiver;
	  int rankInGroup1, rankInGroup2; // group 1 is sender, 2 is receiver
	  int rankInJoin, joinSize;
	  int compid1, compid2;

	  // communication graph from group1 to group2;
	  //  graph[task1] = vec1; // vec1 is a stl vector of tasks in group2
	  std::map<int, std::vector<int> >  recv_graph; // to what tasks from group2 to send  (actual communication graph)
	  std::map<int, std::vector<int> >  recv_sizes; // how many elements to actually send from a sender task to receiver tasks
	  std::map<int, std::vector<int> >  sender_graph; // to what tasks from group2 to send  (actual communication graph)
	  std::map<int, std::vector<int> >  sender_sizes; // how many elements to actually send from a sender task to receiver tasks

	  std::vector<ParallelComm::Buffer*> localSendBuffs; // this will store the pointers to the Buffers
	  //                                    will be  released only when all mpi requests are waited for
	  int * comm_graph; // this will store communication graph, on sender master, sent by nonblocking send to
	                    // the master receiver
	                    // first integer will be the size of the graph, the rest will be the packed graph, for trivial partition

	  // these will be now used to store ranges to be sent from current sender to each receiver in joint comm
	  std::map<int, Range> split_ranges;

	  std::vector<MPI_Request> sendReqs; // there will be multiple requests, 2 for comm graph, 2 for each Buffer
	  // there are as many buffers as sender_graph[rankInJoin].size()

	  // active on both receiver and sender sides
	  std::vector<int> corr_tasks; // subset of the senderTasks, in the joint comm for sender;
	                                // subset of receiverTasks for receiver side
	  std::vector<int> corr_sizes ; // how many primary entities corresponding to the other side
	  // so what we know is that the local range corresponds to remote corr_sizes[i] size ranges on tasks corr_tasks[i]

	  // these will be used now after coverage, quick fix; they will also be populated by iMOAB_CoverageGraph
	  bool recomputed_send_graph; // this should be false , set to true in settle send graph, to use send_IDs_map
	  std::map<int ,std::vector<int> > send_IDs_map; // maybe moab::Range instead of std::vector<int> // these will be on sender side
	  std::map<int, std::vector<int> > recv_IDs_map; // receiver side, after coverage, how many elements need to be received from each sender process

};

} // namespace moab
#endif /* SRC_PARALLEL_MOAB_PARCOMMGRAPH_HPP_ */
