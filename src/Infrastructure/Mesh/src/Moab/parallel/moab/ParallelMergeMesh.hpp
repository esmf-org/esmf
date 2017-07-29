#ifndef PARALLELMERGEMESH_HPP
#define PARALLELMERGEMESH_HPP

#include "moab/Types.hpp"
#include <vector>
#include "moab/Range.hpp"
#include "moab/ParallelComm.hpp"

#include "moab/TupleList.hpp"
#include "moab/gs.hpp"

/*
  Class to merge meshes in parallel
  Requires a ParallelComm and tolerance epsilon
  Currently uses a 1 dimensional partition of the global box
*/

namespace moab {

  class ParallelComm;
  class TupleList;
  
  class ParallelMergeMesh {
  public:
    ParallelMergeMesh(ParallelComm *pc, 
		      const double epsilon);
    
    //Public Function to identify shared elements
    ErrorCode merge(EntityHandle levelset=0, bool skip_local_merge=false);
    
  private:
    ParallelComm *myPcomm;
    Interface *myMB;
    std::vector<Range> mySkinEnts;
    double myEps;
    TupleList myTup, myMatches;
    gs_data::crystal_data myCD;
    
    //Wrapper of merge() that performs the merge
    ErrorCode PerformMerge(EntityHandle levelset=0, bool skip_local_merge=false);
    //Determine the local skin entities (fills mySkinEnts)
    ErrorCode PopulateMySkinEnts(const EntityHandle meshset,int dim, bool skip_local_merge=false);
    //Get the global bounding box
    ErrorCode GetGlobalBox(double *gbox);
    //Fill out the local myTup before the first gather-scatter
    ErrorCode PopulateMyTup(double * gbox);
    //Once myTup is filled and gather scattered, figure out the matches
    ErrorCode PopulateMyMatches();
    //Sort the matching tuples
    ErrorCode SortMyMatches();
    //Tag the shared elements once the myMatches has been filled
    ErrorCode TagSharedElements(int dim);
    //Cleanup any data allocated by class members
    void CleanUp();
    //Partition the global box by the number of procs
    //Returns results in lengths and parts, which needs to be of length 3
    ErrorCode PartitionGlobalBox(double *gbox, double *lengths, int *parts);
    //A function for determining how many parts a side should be split into
    static int PartitionSide(double sideLeng, double restLen, unsigned numProcs, bool altRatio);
    
    //Swap 2 tuples
    static void SwapTuples(TupleList &tup, 
			   unsigned long a, 
			   unsigned long b);
    
    //Sort a tuple list by its real values
    static void SortTuplesByReal(TupleList &tup,
				 double eps2=0);
    
    //The recursive sorting function
    static void PerformRealSort(TupleList &tup, 
				unsigned long left, 
				unsigned long right,
				double eps2,
				uint tup_mr);
    
    //Determines whether tuple i is greater than tuple j
    static bool TupleGreaterThan(TupleList &tup, 
				 unsigned long vrI, 
				 unsigned long vrJ, 
				 double eps2, 
				 uint tup_mr);
  };
  
} // namespace moab

#endif
