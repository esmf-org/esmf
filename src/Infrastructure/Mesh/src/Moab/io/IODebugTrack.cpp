#include "IODebugTrack.hpp"
#include "moab/Range.hpp"
#include <iostream>
#include <vector>
#include <assert.h>

#ifdef MOAB_HAVE_MPI
#  include "moab_mpi.h"
#endif

const char PFX[] = ">>> ";

namespace moab {

IODebugTrack::IODebugTrack( bool enabled,
                            const std::string& name,
                            std::ostream& output_stream,
                            unsigned long table_size )
          : enableOutput(enabled),
            tableName(name),
            ostr(output_stream),
            maxSize(table_size),
            haveMPI(false)
{
#ifdef MOAB_HAVE_MPI
  MPI_Comm_rank( MPI_COMM_WORLD, &mpiRank );
#else
  mpiRank = 0;
#endif
}


IODebugTrack::IODebugTrack( bool enabled,
                            const std::string& name,
                            unsigned long table_size )
          : enableOutput(enabled),
            tableName(name),
            ostr(std::cerr),
            maxSize(table_size) 
{
  mpiRank = 0;
  haveMPI = false;
#ifdef MOAB_HAVE_MPI
  int have_init = 0;
  MPI_Initialized(&have_init);
  if (have_init) {
    haveMPI = true;
    MPI_Comm_rank( MPI_COMM_WORLD, &mpiRank );
  }
#endif
}

IODebugTrack::~IODebugTrack()
{
  if (!enableOutput || mpiRank) // only root prints gap summary
    return;
  
  if (dataSet.empty()) {
    ostr << PFX << tableName << " : No Data Written!!!!" << std::endl;
    return;
  }
  
  std::list<DRange>::const_iterator i;
  if (!maxSize) {
    for (i = dataSet.begin(); i != dataSet.end(); ++i)
      if (i->end >= maxSize)
        maxSize = i->end + 1;
  }
  Range processed;
  Range::iterator h = processed.begin();
  bool wrote_zero = false;
  for (i = dataSet.begin(); i != dataSet.end(); ++i) {
    // ranges cannot contain zero
    assert(i->begin <= i->end);
    if (i->begin)
      h = processed.insert( h, i->begin, i->end );
    else {
      wrote_zero = true;
      if (i->end)
        h = processed.insert( h, i->begin+1, i->end );
    }
  }
    
    // ranges cannot contain zero
  Range unprocessed;
  if (maxSize > 1) 
    unprocessed.insert( 1, maxSize - 1 );
  unprocessed = subtract( unprocessed, processed );
  if (unprocessed.empty())
    return;
  
  Range::const_pair_iterator j;
  for (j = unprocessed.const_pair_begin(); j != unprocessed.const_pair_end(); ++j) {
    unsigned long b = j->first;
    unsigned long e = j->second;
    if (b == 1 && !wrote_zero)
      b = 0;
    
    ostr << PFX << tableName << " : range not read/written: ["
         << b << "," << e << "]" << std::endl;
    ostr.flush();
  }
}

void IODebugTrack::record_io( unsigned long begin, unsigned long count )
{
  if (enableOutput && count) {
    DRange ins = { begin, begin+count-1, static_cast<long unsigned>(mpiRank) };
    record_io( ins );
  }
}

void IODebugTrack::record_io( DRange ins )
{
  if (!enableOutput)
    return;

    // only root should get non-local data
  assert(!mpiRank || ins.rank == (unsigned)mpiRank);
  assert( ins.begin <= ins.end );

    // test for out-of-bounds write
  if (maxSize && ins.end >= maxSize)
    ostr << ": Out of bounds write on rank " << mpiRank 
         << ": [" << ins.begin << "," << ins.end << "] >= " << maxSize
         << std::endl;

    // test for overlap with all existing ranges
  std::list<DRange>::iterator i;
  for (i = dataSet.begin(); i != dataSet.end(); ++i) {
    if (i->end >= ins.begin && i->begin <= ins.end) { // if overlap
      ostr << PFX << tableName;
      if (i->rank == ins.rank) {
        if (mpiRank == (int)ins.rank) 
          ostr << ": Local overwrite on rank " << mpiRank;
        
        // otherwise should have been logged on remote proc, do nothing here
      }
      else 
        ostr << ": Conflicting write for ranks " << i->rank << " and " << ins.rank;
      
      ostr << ": [" << i->begin << "," << i->end << "] and [" << ins.begin
           << "," << ins.end << "]" << std::endl;
      ostr.flush();
    }
  }

  dataSet.push_back( ins );
}

void IODebugTrack::all_reduce()
{
#ifdef MOAB_HAVE_MPI
  if (!enableOutput || !haveMPI)
    return;

  int commsize;
  MPI_Comm_size( MPI_COMM_WORLD, &commsize);
  int count = 3*dataSet.size();
  std::vector<int> displs(commsize), counts(commsize);
  MPI_Gather( &count, 1, MPI_INT, 
              &counts[0], 1, MPI_INT,
              0, MPI_COMM_WORLD );
  displs[0] = 0;
  for (int i = 1; i < commsize; ++i) 
    displs[i] = displs[i-1] + counts[i-1];
  int total = (displs.back() + counts.back()) / 3;
  count /= 3;
              
  std::vector<DRange> send(dataSet.size()), recv(total);
  std::copy( dataSet.begin(), dataSet.end(), send.begin() );
  MPI_Gatherv( &send[0], 3*send.size(), MPI_UNSIGNED_LONG,
               &recv[0], &counts[0], &displs[0], MPI_UNSIGNED_LONG,
               0, MPI_COMM_WORLD );
  
  if (0 == mpiRank) {
    for (int i = count; i < total; ++i)
      record_io( recv[i] );
  }
  else {
    dataSet.clear();
  }
#endif
}
    


} // namespace moab
