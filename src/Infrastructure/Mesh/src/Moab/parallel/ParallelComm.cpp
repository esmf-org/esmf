#include "moab/Interface.hpp"
#include "moab/ParallelComm.hpp"
#include "moab/WriteUtilIface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "SequenceManager.hpp"
#include "moab/Error.hpp"
#include "EntitySequence.hpp"
#include "MBTagConventions.hpp"
#include "moab/Skinner.hpp"
#include "MBParallelConventions.h"
#include "moab/Core.hpp"
#include "ElementSequence.hpp"
#include "moab/CN.hpp"
#include "moab/RangeMap.hpp"
#include "moab/MeshTopoUtil.hpp"
#include "TagInfo.hpp"
#include "DebugOutput.hpp"
#include "SharedSetData.hpp"
#include "moab/ScdInterface.hpp"
#include "moab/TupleList.hpp"
#include "moab/gs.hpp"

#include <iostream>
#include <sstream>
#include <algorithm>
#include <functional>
#include <numeric>

#include <math.h>
#include <assert.h>

#ifdef MOAB_HAVE_MPI
#include "moab_mpi.h"
#endif
#ifdef MOAB_HAVE_MPE
#include "mpe.h"
int IFACE_START, IFACE_END;
int GHOST_START, GHOST_END;
int SHAREDV_START, SHAREDV_END;
int RESOLVE_START, RESOLVE_END;
int ENTITIES_START, ENTITIES_END;
int RHANDLES_START, RHANDLES_END;
int OWNED_START, OWNED_END;
#endif

namespace moab {

  const unsigned int ParallelComm::INITIAL_BUFF_SIZE = 1024;

  const int MAX_BCAST_SIZE = (1 << 28);

  std::vector<ParallelComm::Buffer*> msgs;
  unsigned int __PACK_num = 0, __UNPACK_num = 0, __PACK_count = 0, __UNPACK_count = 0;
  std::string __PACK_string, __UNPACK_string;

#ifdef DEBUG_PACKING_TIMES
#define PC(n, m) { \
    if (__PACK_num == (unsigned int)n && __PACK_string == m) __PACK_count++; \
    else { \
      if (__PACK_count > 1) std::cerr << " (" << __PACK_count << "x)"; \
      __PACK_count = 1; __PACK_string = m; __PACK_num = n; \
      std::cerr << std::endl << "PACK: " << n << m; \
    }}
#define UPC(n, m) { \
    if (__UNPACK_num == (unsigned int)n && __UNPACK_string == m) __UNPACK_count++; \
    else { \
      if (__UNPACK_count > 1) std::cerr << "(" << __UNPACK_count << "x)"; \
      __UNPACK_count = 1; __UNPACK_string = m; __UNPACK_num = n; \
      std::cerr << std::endl << "UNPACK: " << n << m; \
    }}
#else
#define PC(n, m)
#define UPC(n, m)
#endif

  template <typename T> static inline
  void UNPACK(unsigned char*& buff, T* val, size_t count)
  {
    memcpy(val, buff, count*sizeof(T));
    buff += count*sizeof(T);
  }

  template <typename T> static inline
  void PACK(unsigned char*& buff, const T* val, size_t count)
  {
    memcpy(buff, val, count*sizeof(T));
    buff += count*sizeof(T);
  }

  static inline
  void PACK_INTS(unsigned char*& buff, const int* int_val, size_t num)
  { PACK(buff, int_val, num); PC(num, " ints"); }

  static inline
  void PACK_INT(unsigned char*& buff, int int_val)
  { PACK_INTS(buff, &int_val, 1); }

  static inline
  void PACK_DBLS(unsigned char*& buff, const double* dbl_val, size_t num)
  { PACK(buff, dbl_val, num); PC(num, " doubles"); }

  //static inline
  //void PACK_DBL(unsigned char*& buff, const double dbl_val)
  //{ PACK_DBLS(buff, &dbl_val, 1); }

  static inline
  void PACK_EH(unsigned char*& buff, const EntityHandle* eh_val, size_t num)
  { PACK(buff, eh_val, num); PC(num, " handles"); }

  //static inline
  //void PACK_CHAR_64(unsigned char*& buff, const char* str)
  //{
  //  memcpy(buff, str, 64);
  //  buff += 64;
  //  PC(64, " chars");
  //}

  static inline
  void PACK_VOID(unsigned char*& buff, const void* val, size_t num)
  {
    PACK(buff, reinterpret_cast<const unsigned char*>(val), num);
    PC(num, " void");
  }

  static inline
  void PACK_BYTES(unsigned char*& buff, const void* val, int num)
  { PACK_INT(buff, num); PACK_VOID(buff, val, num); }

  static inline
  void PACK_RANGE(unsigned char*& buff, const Range& rng)
  {
    PACK_INT(buff, rng.psize());
    Range::const_pair_iterator cit;
    for (cit = rng.const_pair_begin(); cit != rng.const_pair_end(); ++cit) {
      EntityHandle eh[2] = { cit->first, cit->second };
      PACK_EH(buff, eh, 2);
    }
    PC(rng.psize(), "-subranged range");
  }

  static inline
  void UNPACK_INTS(unsigned char*& buff, int* int_val, size_t num)
  { UNPACK(buff, int_val, num); UPC(num, " ints"); }

  static inline
  void UNPACK_INT(unsigned char*& buff, int& int_val)
  { UNPACK_INTS(buff, &int_val, 1); }

  static inline
  void UNPACK_DBLS(unsigned char*& buff, double* dbl_val, size_t num)
  { UNPACK(buff, dbl_val, num); UPC(num, " doubles"); }

  static inline
  void UNPACK_DBL(unsigned char*& buff, double &dbl_val)
  { UNPACK_DBLS(buff, &dbl_val, 1); }

  static inline
  void UNPACK_EH(unsigned char*& buff, EntityHandle* eh_val, size_t num)
  { UNPACK(buff, eh_val, num); UPC(num, " handles"); }

  //static inline
  //void UNPACK_CHAR_64(unsigned char*& buff, char* char_val)
  //{
  //  memcpy(buff, char_val, 64);
  //  buff += 64;
  //  UPC(64, " chars");
  //}

  static inline
  void UNPACK_VOID(unsigned char*& buff, void* val, size_t num)
  {
    UNPACK(buff, reinterpret_cast<unsigned char*>(val), num);
    UPC(num, " void");
  }

  static inline
  void UNPACK_TYPE(unsigned char*& buff, EntityType& type)
  {
    int int_type = MBMAXTYPE;
    UNPACK_INT(buff, int_type);
    type = static_cast<EntityType>(int_type);
    assert(type >= MBVERTEX && type <= MBMAXTYPE);
  }

  static inline
  void UNPACK_RANGE(unsigned char*& buff, Range& rng)
  {
    int num_subs;
    EntityHandle eh[2];
    UNPACK_INT(buff, num_subs);
    for (int i = 0; i < num_subs; i++) {
      UPC(num_subs, "-subranged range");
      UNPACK_EH(buff, eh, 2);
      rng.insert(eh[0], eh[1]);
    }
  }

  enum MBMessageTag {MB_MESG_ANY=MPI_ANY_TAG,
                     MB_MESG_ENTS_ACK,
                     MB_MESG_ENTS_SIZE,
                     MB_MESG_ENTS_LARGE,
                     MB_MESG_REMOTEH_ACK,
                     MB_MESG_REMOTEH_SIZE,
                     MB_MESG_REMOTEH_LARGE,
                     MB_MESG_TAGS_ACK,
                     MB_MESG_TAGS_SIZE,
                     MB_MESG_TAGS_LARGE
  };

  static inline size_t RANGE_SIZE(const Range& rng)
  { return 2*sizeof(EntityHandle)*rng.psize() + sizeof(int); }

#define PRINT_DEBUG_ISEND(A,B,C,D,E)   print_debug_isend((A),(B),(C),(D),(E))
#define PRINT_DEBUG_IRECV(A,B,C,D,E,F) print_debug_irecv((A),(B),(C),(D),(E),(F))
#define PRINT_DEBUG_RECD(A)            print_debug_recd((A))
#define PRINT_DEBUG_WAITANY(A,B,C)     print_debug_waitany((A),(B),(C))

  void ParallelComm::print_debug_isend(int from, int to, unsigned char *buff,
                                       int tag, int sz) 
  {
    myDebug->tprintf(3, "Isend, %d->%d, buffer ptr = %p, tag=%d, size=%d\n",
                     from, to, (void*)buff, tag, sz);
  }

  void ParallelComm::print_debug_irecv(int to, int from, unsigned char *buff, int sz,
                                       int tag, int incoming) 
  {
    myDebug->tprintf(3, "Irecv, %d<-%d, buffer ptr = %p, tag=%d, size=%d",
                     to, from, (void*)buff, tag, sz);
    if (tag < MB_MESG_REMOTEH_ACK)
      myDebug->printf(3, ", incoming1=%d\n", incoming);
    else if (tag < MB_MESG_TAGS_ACK)
      myDebug->printf(3, ", incoming2=%d\n", incoming);
    else
      myDebug->printf(3, ", incoming=%d\n", incoming);
  }

  void ParallelComm::print_debug_recd(MPI_Status status) 
  {
    if (myDebug->get_verbosity() == 3) {
      int this_count;
      int success = MPI_Get_count(&status, MPI_UNSIGNED_CHAR, &this_count);
      if (MPI_SUCCESS != success)
        this_count = -1;
      myDebug->tprintf(3, "Received from %d, count = %d, tag = %d\n",
                       status.MPI_SOURCE, this_count, status.MPI_TAG);
    }
  }

  void ParallelComm::print_debug_waitany(std::vector<MPI_Request> &reqs, int tag, int proc) 
  {
    if (myDebug->get_verbosity() == 3) {
      myDebug->tprintf(3, "Waitany, p=%d, ", proc);
      if (tag < MB_MESG_REMOTEH_ACK)
        myDebug->print(3, ", recv_ent_reqs=");
      else if (tag < MB_MESG_TAGS_ACK)
        myDebug->print(3, ", recv_remoteh_reqs=");
      else
        myDebug->print(3, ", recv_tag_reqs=");
      for (unsigned int i = 0; i < reqs.size(); i++)
        myDebug->printf(3, " %p", (void*)(intptr_t)reqs[i]);
      myDebug->print(3, "\n");
    }
  }

  /** Name of tag used to store ParallelComm Index on mesh paritioning sets */
  const char* PARTITIONING_PCOMM_TAG_NAME = "__PRTN_PCOMM";

  /** \brief Tag storing parallel communication objects
   *
   * This tag stores pointers to ParallelComm communication
   * objects; one of these is allocated for each different
   * communicator used to read mesh. ParallelComm stores
   * partition and interface sets corresponding to its parallel mesh.
   * By default, a parallel read uses the first ParallelComm object
   * on the interface instance; if instantiated with one, ReadParallel
   * adds this object to the interface instance too.
   *
   * Tag type: opaque
   * Tag size: MAX_SHARING_PROCS*sizeof(ParallelComm*)
   */
#define PARALLEL_COMM_TAG_NAME "__PARALLEL_COMM"

  ParallelComm::ParallelComm(Interface *impl, MPI_Comm cm, int* id)
    : mbImpl(impl), procConfig(cm),
      sharedpTag(0), sharedpsTag(0),
      sharedhTag(0), sharedhsTag(0), pstatusTag(0), ifaceSetsTag(0),
      partitionTag(0), globalPartCount(-1), partitioningSet(0),
      myDebug(NULL),
      sharedSetData(new SharedSetData(*impl, procConfig.proc_rank()))
  {
    initialize();

    if (id)
      *id = pcommID;
  }

  ParallelComm::ParallelComm(Interface *impl,
                             std::vector<unsigned char> &/*tmp_buff*/,
                             MPI_Comm cm,
                             int* id)
    : mbImpl(impl), procConfig(cm),
      sharedpTag(0), sharedpsTag(0),
      sharedhTag(0), sharedhsTag(0), pstatusTag(0), ifaceSetsTag(0),
      partitionTag(0), globalPartCount(-1), partitioningSet(0),
      myDebug(NULL),
      sharedSetData(new SharedSetData(*impl, procConfig.proc_rank()))
  {
    initialize();

    if (id)
      *id = pcommID;
  }

  ParallelComm::~ParallelComm() 
  {
    remove_pcomm(this);
    delete_all_buffers();
    delete myDebug;
    delete sharedSetData;
  }

  void ParallelComm::initialize() 
  {
    Core* core = dynamic_cast<Core*>(mbImpl);
    sequenceManager = core->sequence_manager();
    mbImpl->query_interface(errorHandler);

    // Initialize MPI, if necessary
    int flag = 1;
    int retval = MPI_Initialized(&flag);
    if (MPI_SUCCESS != retval || !flag) {
      int argc = 0;
      char **argv = NULL;

      // mpi not initialized yet - initialize here
      retval = MPI_Init(&argc, &argv);
      assert(MPI_SUCCESS == retval);
    }

    // Reserve space for vectors
    buffProcs.reserve(MAX_SHARING_PROCS);
    localOwnedBuffs.reserve(MAX_SHARING_PROCS);
    remoteOwnedBuffs.reserve(MAX_SHARING_PROCS);

    pcommID = add_pcomm(this);

    if (!myDebug)
    {
      myDebug = new DebugOutput("ParallelComm", std::cerr);
      myDebug->set_rank( procConfig.proc_rank());
    }
  }

  int ParallelComm::add_pcomm(ParallelComm *pc) 
  {
    // Add this pcomm to instance tag
    std::vector<ParallelComm *> pc_array(MAX_SHARING_PROCS,
                                         (ParallelComm*)NULL);
    Tag pc_tag = pcomm_tag(mbImpl, true);
    assert(0 != pc_tag);

    const EntityHandle root = 0;
    ErrorCode result = mbImpl->tag_get_data(pc_tag, &root, 1, (void*)&pc_array[0]);
    if (MB_SUCCESS != result && MB_TAG_NOT_FOUND != result)
      return -1;
    int index = 0;
    while (index < MAX_SHARING_PROCS && pc_array[index])
      index++;
    if (index == MAX_SHARING_PROCS) {
      index = -1;
      assert(false);
    }
    else {
      pc_array[index] = pc;
      mbImpl->tag_set_data(pc_tag, &root, 1, (void*)&pc_array[0]);
    }
    return index;
  }

  void ParallelComm::remove_pcomm(ParallelComm *pc)
  {
    // Remove this pcomm from instance tag
    std::vector<ParallelComm *> pc_array(MAX_SHARING_PROCS);
    Tag pc_tag = pcomm_tag(mbImpl, true);

    const EntityHandle root = 0;
    ErrorCode result = mbImpl->tag_get_data(pc_tag, &root, 1, (void*)&pc_array[0]);
    std::vector<ParallelComm*>::iterator pc_it = 
      std::find(pc_array.begin(), pc_array.end(), pc);
    assert(MB_SUCCESS == result &&
           pc_it != pc_array.end());
    // Empty if test to get around compiler warning about unused var
    if (MB_SUCCESS == result) {}

    *pc_it = NULL;
    mbImpl->tag_set_data(pc_tag, &root, 1, (void*)&pc_array[0]);
  }

  //! Assign a global id space, for largest-dimension or all entities (and
  //! in either case for vertices too)
  ErrorCode ParallelComm::assign_global_ids(EntityHandle this_set,
                                            const int dimension,
                                            const int start_id,
                                            const bool largest_dim_only,
                                            const bool parallel,
                                            const bool owned_only)
  {
    Range entities[4];
    ErrorCode result;
    std::vector<unsigned char> pstatus;
    for (int dim = 0; dim <= dimension; dim++) {
      if (dim == 0 || !largest_dim_only || dim == dimension) {
        result = mbImpl->get_entities_by_dimension(this_set, dim, entities[dim]);MB_CHK_SET_ERR(result, "Failed to get vertices in assign_global_ids");
      }

      // Need to filter out non-locally-owned entities!!!
      pstatus.resize(entities[dim].size());
      result = mbImpl->tag_get_data(pstatus_tag(), entities[dim], &pstatus[0]);MB_CHK_SET_ERR(result, "Failed to get pstatus in assign_global_ids");

      Range dum_range;
      Range::iterator rit;
      unsigned int i;
      for (rit = entities[dim].begin(), i = 0; rit != entities[dim].end(); ++rit, i++)
        if (pstatus[i] & PSTATUS_NOT_OWNED)
          dum_range.insert(*rit);
      entities[dim] = subtract(entities[dim], dum_range);
    }

    return assign_global_ids(entities, dimension, start_id, parallel, owned_only);
  }

  //! Assign a global id space, for largest-dimension or all entities (and
  //! in either case for vertices too)
  ErrorCode ParallelComm::assign_global_ids(Range entities[],
                                            const int dimension,
                                            const int start_id,
                                            const bool parallel,
                                            const bool owned_only)
  {
    int local_num_elements[4];
    ErrorCode result;
    for (int dim = 0; dim <= dimension; dim++) {
      local_num_elements[dim] = entities[dim].size();
    }

    // Communicate numbers
    std::vector<int> num_elements(procConfig.proc_size() * 4);
#ifdef MOAB_HAVE_MPI
    if (procConfig.proc_size() > 1 && parallel) {
      int retval = MPI_Allgather(local_num_elements, 4, MPI_INT,
                                 &num_elements[0], 4,
                                 MPI_INT, procConfig.proc_comm());
      if (0 != retval)
        return MB_FAILURE;
    }
    else
#endif
      for (int dim = 0; dim < 4; dim++)
        num_elements[dim] = local_num_elements[dim];

    // My entities start at one greater than total_elems[d]
    int total_elems[4] = {start_id, start_id, start_id, start_id};

    for (unsigned int proc = 0; proc < procConfig.proc_rank(); proc++) {
      for (int dim = 0; dim < 4; dim++)
        total_elems[dim] += num_elements[4*proc + dim];
    }

    // Assign global ids now
    Tag gid_tag;
    int zero = 0;
    result = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                    gid_tag, MB_TAG_DENSE | MB_TAG_CREAT, &zero);
    if (MB_SUCCESS != result) return result;

    for (int dim = 0; dim < 4; dim++) {
      if (entities[dim].empty())
        continue;
      num_elements.resize(entities[dim].size());
      int i = 0;
      for (Range::iterator rit = entities[dim].begin(); rit != entities[dim].end(); ++rit)
        num_elements[i++] = total_elems[dim]++;
    
      result = mbImpl->tag_set_data(gid_tag, entities[dim], &num_elements[0]);MB_CHK_SET_ERR(result, "Failed to set global id tag in assign_global_ids");
    }

    if (owned_only)
      return MB_SUCCESS;

    // Exchange tags
    for (int dim = 1; dim < 4; dim++) 
      entities[0].merge(entities[dim]);

    return exchange_tags(gid_tag, entities[0]);
  }

  int ParallelComm::get_buffers(int to_proc, bool *is_new)
  {
    int ind = -1;
    std::vector<unsigned int>::iterator vit =
      std::find(buffProcs.begin(), buffProcs.end(), to_proc);
    if (vit == buffProcs.end()) {
      assert("shouldn't need buffer to myself" && to_proc != (int)procConfig.proc_rank());
      ind = buffProcs.size();
      buffProcs.push_back((unsigned int)to_proc);
      localOwnedBuffs.push_back(new Buffer(INITIAL_BUFF_SIZE));
      remoteOwnedBuffs.push_back(new Buffer(INITIAL_BUFF_SIZE));
      if (is_new)
        *is_new = true;
    }
    else {
      ind = vit - buffProcs.begin();
      if (is_new)
        *is_new = false;
    }
    assert(ind < MAX_SHARING_PROCS);
    return ind;
  }

  ErrorCode ParallelComm::broadcast_entities(const int from_proc,
                                             Range &entities,
                                             const bool adjacencies,
                                             const bool tags)
  {
#ifndef MOAB_HAVE_MPI
    return MB_FAILURE;
#else

    ErrorCode result = MB_SUCCESS;
    int success;
    int buff_size;

    Buffer buff(INITIAL_BUFF_SIZE);
    buff.reset_ptr(sizeof(int));
    if ((int)procConfig.proc_rank() == from_proc) {
      result = add_verts(entities);MB_CHK_SET_ERR(result, "Failed to add adj vertices");

      buff.reset_ptr(sizeof(int));
      result = pack_buffer(entities, adjacencies, tags,
                           false, -1, &buff);MB_CHK_SET_ERR(result, "Failed to compute buffer size in broadcast_entities");
      buff.set_stored_size();
      buff_size = buff.buff_ptr - buff.mem_ptr;
    }

    success = MPI_Bcast(&buff_size, 1, MPI_INT, from_proc, procConfig.proc_comm());
    if (MPI_SUCCESS != success) {
      MB_SET_ERR(MB_FAILURE, "MPI_Bcast of buffer size failed");
    }

    if (!buff_size) // No data
      return MB_SUCCESS;

    if ((int)procConfig.proc_rank() != from_proc) 
      buff.reserve(buff_size);

    size_t offset = 0;
    while (buff_size) {
      int sz = std::min(buff_size, MAX_BCAST_SIZE);
      success = MPI_Bcast(buff.mem_ptr + offset, sz, MPI_UNSIGNED_CHAR, from_proc, procConfig.proc_comm());
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "MPI_Bcast of buffer failed");
      }

      offset += sz;
      buff_size -= sz;
    }

    if ((int)procConfig.proc_rank() != from_proc) {
      std::vector<std::vector<EntityHandle> > dum1a, dum1b;
      std::vector<std::vector<int> > dum1p;
      std::vector<EntityHandle> dum2, dum4;
      std::vector<unsigned int> dum3;
      buff.reset_ptr(sizeof(int));
      result = unpack_buffer(buff.buff_ptr, false, from_proc, -1,
                             dum1a, dum1b, dum1p, dum2, dum2, dum3, dum4);MB_CHK_SET_ERR(result, "Failed to unpack buffer in broadcast_entities");
      std::copy(dum4.begin(), dum4.end(), range_inserter(entities));
    }

    return MB_SUCCESS;
#endif
  }

  ErrorCode ParallelComm::scatter_entities(const int from_proc,
                                           std::vector<Range> &entities,
                                           const bool adjacencies,
                                           const bool tags)
  {
#ifndef MOAB_HAVE_MPI
    return MB_FAILURE;
#else
    ErrorCode result = MB_SUCCESS;
    int i, success, buff_size, prev_size;
    int nProcs = (int)procConfig.proc_size();
    int* sendCounts = new int[nProcs];
    int* displacements = new int[nProcs];
    sendCounts[0] = sizeof(int);
    displacements[0] = 0;
    Buffer buff(INITIAL_BUFF_SIZE);
    buff.reset_ptr(sizeof(int));
    buff.set_stored_size();
    unsigned int my_proc = procConfig.proc_rank();

    // Get buffer size array for each remote processor
    if (my_proc == (unsigned int) from_proc) {
      for (i = 1; i < nProcs; i++) {
        prev_size = buff.buff_ptr - buff.mem_ptr;
        buff.reset_ptr(prev_size + sizeof(int));
        result = add_verts(entities[i]);MB_CHK_SET_ERR(result, "Failed to add verts");

        result = pack_buffer(entities[i], adjacencies, tags, 
                             false, -1, &buff); 
        if (MB_SUCCESS != result) {
          delete[] sendCounts;
          delete[] displacements;
          MB_SET_ERR(result, "Failed to pack buffer in scatter_entities");
        }

        buff_size = buff.buff_ptr - buff.mem_ptr - prev_size;
        *((int*)(buff.mem_ptr + prev_size)) = buff_size;
        sendCounts[i] = buff_size;
      }
    }

    // Broadcast buffer size array
    success = MPI_Bcast(sendCounts, nProcs, MPI_INT, from_proc, procConfig.proc_comm());
    if (MPI_SUCCESS != success) {
      delete[] sendCounts;
      delete[] displacements;
      MB_SET_ERR(MB_FAILURE, "MPI_Bcast of buffer size failed");
    }

    for (i = 1; i < nProcs; i++) {
      displacements[i] = displacements[i-1] + sendCounts[i-1];
    }

    Buffer rec_buff;
    rec_buff.reserve(sendCounts[my_proc]);

    // Scatter actual geometry
    success = MPI_Scatterv(buff.mem_ptr, sendCounts, displacements,
                           MPI_UNSIGNED_CHAR, rec_buff.mem_ptr, sendCounts[my_proc],
                           MPI_UNSIGNED_CHAR, from_proc, procConfig.proc_comm());

    if (MPI_SUCCESS != success) {
      delete[] sendCounts;
      delete[] displacements;
      MB_SET_ERR(MB_FAILURE, "MPI_Scatterv of buffer failed");
    }

    // Unpack in remote processors
    if (my_proc != (unsigned int) from_proc) {
      std::vector<std::vector<EntityHandle> > dum1a, dum1b;
      std::vector<std::vector<int> > dum1p;
      std::vector<EntityHandle> dum2, dum4;
      std::vector<unsigned int> dum3;
      rec_buff.reset_ptr(sizeof(int));
      result = unpack_buffer(rec_buff.buff_ptr, false, from_proc, -1,
                             dum1a, dum1b, dum1p, dum2, dum2, dum3, dum4);
      if (MB_SUCCESS != result) {
        delete[] sendCounts;
        delete[] displacements;
        MB_SET_ERR(result, "Failed to unpack buffer in scatter_entities");
      }

      std::copy(dum4.begin(), dum4.end(), range_inserter(entities[my_proc]));
    }

    delete[] sendCounts;
    delete[] displacements;

    return MB_SUCCESS;
#endif
  }

  ErrorCode ParallelComm::send_entities(const int to_proc,
                                        Range &orig_ents,
                                        const bool adjs,
                                        const bool tags,
                                        const bool store_remote_handles,
                                        const bool is_iface,
                                        Range &/*final_ents*/,
                                        int &incoming1,
                                        int &incoming2,
                                        TupleList& entprocs,
                                        std::vector<MPI_Request> &recv_remoteh_reqs,
                                        bool /*wait_all*/)
  {
#ifndef MOAB_HAVE_MPI
    return MB_FAILURE;
#else
    // Pack entities to local buffer
    int ind = get_buffers(to_proc);
    localOwnedBuffs[ind]->reset_ptr(sizeof(int));

    // Add vertices
    ErrorCode result = add_verts(orig_ents);MB_CHK_SET_ERR(result, "Failed to add verts in send_entities");

    // Filter out entities already shared with destination
    Range tmp_range;
    result = filter_pstatus(orig_ents, PSTATUS_SHARED, PSTATUS_AND,
                            to_proc, &tmp_range);MB_CHK_SET_ERR(result, "Failed to filter on owner");
    if (!tmp_range.empty()) {
      orig_ents = subtract(orig_ents, tmp_range);
    }

    result = pack_buffer(orig_ents, adjs, tags, store_remote_handles,
                         to_proc, localOwnedBuffs[ind], &entprocs);MB_CHK_SET_ERR(result, "Failed to pack buffer in send_entities");

    // Send buffer
    result = send_buffer(to_proc, localOwnedBuffs[ind], MB_MESG_ENTS_SIZE,
                         sendReqs[2*ind], recvReqs[2*ind + 1],
                         (int*)(remoteOwnedBuffs[ind]->mem_ptr),
                         //&ackbuff,
                         incoming1,
                         MB_MESG_REMOTEH_SIZE,
                         (!is_iface && store_remote_handles ?
                          localOwnedBuffs[ind] : NULL),
                         &recv_remoteh_reqs[2*ind], &incoming2);MB_CHK_SET_ERR(result, "Failed to send buffer");

    return MB_SUCCESS;
#endif
  }

ErrorCode ParallelComm::send_entities(std::vector<unsigned int>& send_procs,
                                      std::vector<Range*>& send_ents,
                                      int& incoming1, int& incoming2,
                                      const bool store_remote_handles)
{
#ifdef MOAB_HAVE_MPE
  if (myDebug->get_verbosity() == 2) {
    MPE_Log_event(OWNED_START, procConfig.proc_rank(), "Starting send_entities.");
  }
#endif
  myDebug->tprintf(1, "Entering send_entities\n");
  if (myDebug->get_verbosity() == 4) {
    msgs.clear();
    msgs.reserve(MAX_SHARING_PROCS);
  }

  unsigned int i;
  int ind;
  ErrorCode result = MB_SUCCESS;

  // Set buffProcs with communicating procs
  unsigned int n_proc = send_procs.size();
  for (i = 0; i < n_proc; i++) {
    ind = get_buffers(send_procs[i]);
    result = add_verts(*send_ents[i]);MB_CHK_SET_ERR(result, "Failed to add verts");

    // Filter out entities already shared with destination
    Range tmp_range;
    result = filter_pstatus(*send_ents[i], PSTATUS_SHARED, PSTATUS_AND,
                            buffProcs[ind], &tmp_range);MB_CHK_SET_ERR(result, "Failed to filter on owner");
    if (!tmp_range.empty()) {
      *send_ents[i] = subtract(*send_ents[i], tmp_range);
    }
  }

  //===========================================
  // Get entities to be sent to neighbors
  // Need to get procs each entity is sent to
  //===========================================  
  Range allsent, tmp_range;
  int npairs = 0;
  TupleList entprocs;
  for (i = 0; i < n_proc; i++) {
    int n_ents = send_ents[i]->size();
    if (n_ents > 0) {
      npairs += n_ents; // Get the total # of proc/handle pairs
      allsent.merge(*send_ents[i]);
    }
  }

  // Allocate a TupleList of that size
  entprocs.initialize(1, 0, 1, 0, npairs); 
  entprocs.enableWriteAccess();

  // Put the proc/handle pairs in the list
  for (i = 0; i < n_proc; i++) {
    for (Range::iterator rit = send_ents[i]->begin(); rit != send_ents[i]->end(); ++rit) {
      entprocs.vi_wr[entprocs.get_n()] = send_procs[i];
      entprocs.vul_wr[entprocs.get_n()] = *rit; 
      entprocs.inc_n(); 
    }
  }

  // Sort by handle
  moab::TupleList::buffer sort_buffer; 
  sort_buffer.buffer_init(npairs); 
  entprocs.sort(1, &sort_buffer);
  entprocs.disableWriteAccess();
  sort_buffer.reset(); 

  myDebug->tprintf(1, "allsent ents compactness (size) = %f (%lu)\n", allsent.compactness(),
                  (unsigned long)allsent.size());

  //===========================================
  // Pack and send ents from this proc to others
  //===========================================
  for (i = 0; i < n_proc; i++) {
    if (send_ents[i]->size() > 0) {
      ind = get_buffers(send_procs[i]);
      myDebug->tprintf(1, "Sent ents compactness (size) = %f (%lu)\n", send_ents[i]->compactness(),
                       (unsigned long)send_ents[i]->size());
      // Reserve space on front for size and for initial buff size
      localOwnedBuffs[ind]->reset_buffer(sizeof(int));
      result = pack_buffer(*send_ents[i], false, true,
                           store_remote_handles, buffProcs[ind],
                           localOwnedBuffs[ind], &entprocs, &allsent);

      if (myDebug->get_verbosity() == 4) {
        msgs.resize(msgs.size() + 1);
        msgs.back() = new Buffer(*localOwnedBuffs[ind]);
      }

      // Send the buffer (size stored in front in send_buffer)
      result = send_buffer(send_procs[i], localOwnedBuffs[ind],
                           MB_MESG_ENTS_SIZE, sendReqs[2*ind],
                           recvReqs[2*ind + 1],
                           &ackbuff,
                           incoming1,
                           MB_MESG_REMOTEH_SIZE,
                           (store_remote_handles ?
                            localOwnedBuffs[ind] : NULL),
                           &recvRemotehReqs[2*ind], &incoming2);MB_CHK_SET_ERR(result, "Failed to Isend in ghost send");
    }
  }
  entprocs.reset();

#ifdef MOAB_HAVE_MPE
  if (myDebug->get_verbosity() == 2) {
    MPE_Log_event(ENTITIES_END, procConfig.proc_rank(), "Ending send_entities.");
  }
#endif

  return MB_SUCCESS;
}

/////////////////////////////////////////////////////////////////////////////////
// Send and Receive routines for a sequence of entities: use case UMR
/////////////////////////////////////////////////////////////////////////////////
void print_buff(unsigned char * ch, int size)
{
  for (int i=0; i<size; i++)
    std::cout<<ch[i];
  std::cout<<"\n";
}
ErrorCode ParallelComm::send_recv_entities(std::vector<int> &send_procs, std::vector<std::vector<int> > &msgsizes, std::vector<std::vector<EntityHandle> > &senddata, std::vector<std::vector<EntityHandle> > &recvdata)
{
#ifdef USE_MPE
  if (myDebug->get_verbosity() == 2) {
    MPE_Log_event(OWNED_START, procConfig.proc_rank(), "Starting send_recv_entities.");
  }
#endif
  myDebug->tprintf(1, "Entering send_recv_entities\n");
  if (myDebug->get_verbosity() == 4) {
    msgs.clear();
    msgs.reserve(MAX_SHARING_PROCS);
  }

  //unsigned int i;
  int i, ind, success;
  ErrorCode error = MB_SUCCESS;

  //===========================================
  // Pack and send ents from this proc to others
  //===========================================

 // std::cout<<"resetting all buffers"<<std::endl;

  reset_all_buffers();
  sendReqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);
  std::vector<MPI_Request> recv_ent_reqs(3*buffProcs.size(), MPI_REQUEST_NULL);
  int ack_buff;
  int incoming = 0;

  std::vector<unsigned int>::iterator sit;

  for (ind = 0, sit = buffProcs.begin(); sit != buffProcs.end(); ++sit, ind++) {
    incoming++;
    PRINT_DEBUG_IRECV(*sit, procConfig.proc_rank(), remoteOwnedBuffs[ind]->mem_ptr,
                      INITIAL_BUFF_SIZE, MB_MESG_ENTS_SIZE, incoming);

    success = MPI_Irecv(remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
                        MPI_UNSIGNED_CHAR, *sit,
                        MB_MESG_ENTS_SIZE, procConfig.proc_comm(),
                        &recv_ent_reqs[3*ind]);
    if (success != MPI_SUCCESS) {
      MB_SET_ERR(MB_FAILURE, "Failed to post irecv in send_recv_entities");
    }
  }



//  std::set<unsigned int>::iterator it;
  for ( i=0; i< (int) send_procs.size(); i++)
    {
      //Get index of the shared processor in the local buffer
      ind = get_buffers(send_procs[i]);
      localOwnedBuffs[ind]->reset_buffer(sizeof(int));

      int buff_size = msgsizes[i].size()*sizeof(int) + senddata[i].size()*sizeof(EntityHandle);
      localOwnedBuffs[ind]->check_space(buff_size);

      //Pack entities
      std::vector<int> msg;
      msg.insert(msg.end(), msgsizes[i].begin(), msgsizes[i].end());
      PACK_INTS(localOwnedBuffs[ind]->buff_ptr, &msg[0], msg.size());

      std::vector<EntityHandle> entities;
      entities.insert(entities.end(), senddata[i].begin(), senddata[i].end());
      PACK_EH(localOwnedBuffs[ind]->buff_ptr, &entities[0], entities.size());
      localOwnedBuffs[ind]->set_stored_size();

      if (myDebug->get_verbosity() == 4) {
          msgs.resize(msgs.size() + 1);
          msgs.back() = new Buffer(*localOwnedBuffs[ind]);
        }

      // Send the buffer (size stored in front in send_buffer)
      error = send_buffer(send_procs[i], localOwnedBuffs[ind],
                           MB_MESG_ENTS_SIZE, sendReqs[3*ind],
          recv_ent_reqs[3*ind+2],
          &ack_buff,
          incoming);MB_CHK_SET_ERR(error, "Failed to Isend in send_recv_entities");
    }


  //===========================================
  // Receive and unpack ents from received data
  //===========================================

  while (incoming) {

    MPI_Status status;
    int index_in_recv_requests;

    PRINT_DEBUG_WAITANY(recv_ent_reqs, MB_MESG_ENTS_SIZE, procConfig.proc_rank());
    success = MPI_Waitany(3*buffProcs.size(), &recv_ent_reqs[0], &index_in_recv_requests, &status);
    if (MPI_SUCCESS != success) {
      MB_SET_ERR(MB_FAILURE, "Failed in waitany in send_recv_entities");
    }

    // Processor index in the list is divided by 3
    ind = index_in_recv_requests / 3;

    PRINT_DEBUG_RECD(status);

    // OK, received something; decrement incoming counter
    incoming--;

    bool done = false;

    error = recv_buffer(MB_MESG_ENTS_SIZE,
                         status,
                         remoteOwnedBuffs[ind],
                         recv_ent_reqs[3*ind + 1], // This is for receiving the second message
                         recv_ent_reqs[3*ind + 2], // This would be for ack, but it is not used; consider removing it
                         incoming,
                         localOwnedBuffs[ind],
                         sendReqs[3*ind + 1], // Send request for sending the second message
                         sendReqs[3*ind + 2], // This is for sending the ack
                         done);MB_CHK_SET_ERR(error, "Failed to resize recv buffer");

    if (done) {
      remoteOwnedBuffs[ind]->reset_ptr(sizeof(int));

      int from_proc = status.MPI_SOURCE;
      int idx = std::find(send_procs.begin(), send_procs.end(), from_proc) - send_procs.begin();

      int msg = msgsizes[idx].size(); std::vector<int> recvmsg(msg);
      int ndata = senddata[idx].size(); std::vector<EntityHandle> dum_vec(ndata);

      UNPACK_INTS(remoteOwnedBuffs[ind]->buff_ptr, &recvmsg[0], msg);
      UNPACK_EH(remoteOwnedBuffs[ind]->buff_ptr, &dum_vec[0], ndata);

      recvdata[idx].insert(recvdata[idx].end(), dum_vec.begin(), dum_vec.end());
    }
  }

#ifdef USE_MPE
  if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(ENTITIES_END, procConfig.proc_rank(), "Ending send_recv_entities.");
    }
#endif


  return MB_SUCCESS;
}

ErrorCode ParallelComm::update_remote_data(EntityHandle entity, std::vector<int> &procs, std::vector<EntityHandle> &handles)
{
  ErrorCode error;
  unsigned char pstatus = PSTATUS_INTERFACE;

  int procmin = *std::min_element(procs.begin(), procs.end());

  if ((int)rank() > procmin)
    pstatus |= PSTATUS_NOT_OWNED;
  else
    procmin = rank();


  //DBG
 // std::cout<<"entity = "<<entity<<std::endl;
 // for (int j=0; j<procs.size(); j++)
  // std::cout<<"procs["<<j<<"] = "<<procs[j]<<", handles["<<j<<"] = "<<handles[j]<<std::endl;
  //DBG


  if ((int)procs.size() > 1)
    {
      procs.push_back(rank());
      handles.push_back(entity);

      int idx = std::find(procs.begin(), procs.end(), procmin) - procs.begin();

      std::iter_swap(procs.begin(), procs.begin()+idx);
      std::iter_swap(handles.begin(), handles.begin()+idx);


      //DBG
    //  std::cout<<"entity = "<<entity<<std::endl;
     // for (int j=0; j<procs.size(); j++)
      // std::cout<<"procs["<<j<<"] = "<<procs[j]<<", handles["<<j<<"] = "<<handles[j]<<std::endl;
      //DBG


    }

 // if ((entity == 10388) && (rank()==1))
//    std::cout<<"Here"<<std::endl;

  error = update_remote_data(entity, &procs[0], &handles[0], procs.size(), pstatus);MB_CHK_ERR(error);

  return MB_SUCCESS;
}

ErrorCode ParallelComm::get_remote_handles(EntityHandle *local_vec, EntityHandle *rem_vec, int num_ents, int to_proc)
{
  ErrorCode error;
 std::vector<EntityHandle> newents;
  error = get_remote_handles(true, local_vec, rem_vec, num_ents, to_proc, newents);MB_CHK_ERR(error);

  return MB_SUCCESS;
}



//////////////////////////////////////////////////////////////////

  ErrorCode ParallelComm::recv_entities(const int from_proc,
                                        const bool store_remote_handles,
                                        const bool is_iface,
                                        Range &final_ents,
                                        int& incoming1,
                                        int& incoming2,
                                        std::vector<std::vector<EntityHandle> > &L1hloc,
                                        std::vector<std::vector<EntityHandle> > &L1hrem,
                                        std::vector<std::vector<int> > &L1p,
                                        std::vector<EntityHandle> &L2hloc,
                                        std::vector<EntityHandle> &L2hrem,
                                        std::vector<unsigned int> &L2p,
                                        std::vector<MPI_Request> &recv_remoteh_reqs,
                                        bool /*wait_all*/)
  {
#ifndef MOAB_HAVE_MPI
    return MB_FAILURE;
#else
    // Non-blocking receive for the first message (having size info)
    int ind1 = get_buffers(from_proc);
    incoming1++;
    PRINT_DEBUG_IRECV(procConfig.proc_rank(), from_proc,
                      remoteOwnedBuffs[ind1]->mem_ptr, INITIAL_BUFF_SIZE,
                      MB_MESG_ENTS_SIZE, incoming1);
    int success = MPI_Irecv(remoteOwnedBuffs[ind1]->mem_ptr, INITIAL_BUFF_SIZE,
                            MPI_UNSIGNED_CHAR, from_proc,
                            MB_MESG_ENTS_SIZE, procConfig.proc_comm(),
                            &recvReqs[2*ind1]);
    if (success != MPI_SUCCESS) {
      MB_SET_ERR(MB_FAILURE, "Failed to post irecv in ghost exchange");
    }

    // Receive messages in while loop
    return recv_messages(from_proc, store_remote_handles, is_iface, final_ents,
                         incoming1, incoming2, L1hloc, L1hrem, L1p, L2hloc,
                         L2hrem, L2p, recv_remoteh_reqs);
#endif
  }

  ErrorCode ParallelComm::recv_entities(std::set<unsigned int>& recv_procs,
                                        int incoming1, int incoming2,
                                        const bool store_remote_handles,
                                        const bool migrate)
  {
    //===========================================
    // Receive/unpack new entities
    //===========================================
    // Number of incoming messages is the number of procs we communicate with
    int success, ind, i;
    ErrorCode result;
    MPI_Status status;
    std::vector<std::vector<EntityHandle> > recd_ents(buffProcs.size());
    std::vector<std::vector<EntityHandle> > L1hloc(buffProcs.size()), L1hrem(buffProcs.size());
    std::vector<std::vector<int> > L1p(buffProcs.size());
    std::vector<EntityHandle> L2hloc, L2hrem;
    std::vector<unsigned int> L2p;
    std::vector<EntityHandle> new_ents;

    while (incoming1) {
      // Wait for all recvs of ents before proceeding to sending remote handles,
      // b/c some procs may have sent to a 3rd proc ents owned by me;
      PRINT_DEBUG_WAITANY(recvReqs, MB_MESG_ENTS_SIZE, procConfig.proc_rank());

      success = MPI_Waitany(2*buffProcs.size(), &recvReqs[0], &ind, &status);
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitany in owned entity exchange");
      }

      PRINT_DEBUG_RECD(status);

      // OK, received something; decrement incoming counter
      incoming1--;
      bool done = false;

      // In case ind is for ack, we need index of one before it
      unsigned int base_ind = 2*(ind/2);
      result = recv_buffer(MB_MESG_ENTS_SIZE,
                           status,
                           remoteOwnedBuffs[ind/2],
                           recvReqs[ind], recvReqs[ind + 1],
                           incoming1,
                           localOwnedBuffs[ind/2], sendReqs[base_ind], sendReqs[base_ind + 1],
                           done,
                           (store_remote_handles ?
                            localOwnedBuffs[ind/2] : NULL),
                           MB_MESG_REMOTEH_SIZE,
                           &recvRemotehReqs[base_ind], &incoming2);MB_CHK_SET_ERR(result, "Failed to receive buffer");

      if (done) {
        if (myDebug->get_verbosity() == 4) {
          msgs.resize(msgs.size() + 1);
          msgs.back() = new Buffer(*remoteOwnedBuffs[ind/2]);
        }

        // Message completely received - process buffer that was sent
        remoteOwnedBuffs[ind/2]->reset_ptr(sizeof(int));
        result = unpack_buffer(remoteOwnedBuffs[ind/2]->buff_ptr,
                               store_remote_handles, buffProcs[ind/2], ind/2,
                               L1hloc, L1hrem, L1p, L2hloc, L2hrem, L2p,
                               new_ents, true);
        if (MB_SUCCESS != result) {
          std::cout << "Failed to unpack entities. Buffer contents:" << std::endl;
          print_buffer(remoteOwnedBuffs[ind/2]->mem_ptr, MB_MESG_ENTS_SIZE, buffProcs[ind/2], false);
          return result;
        }

        if (recvReqs.size() != 2*buffProcs.size()) {
          // Post irecv's for remote handles from new proc
          recvRemotehReqs.resize(2*buffProcs.size(), MPI_REQUEST_NULL);
          for (i = recvReqs.size(); i < (int)(2*buffProcs.size()); i += 2) {
            localOwnedBuffs[i/2]->reset_buffer();
            incoming2++;
            PRINT_DEBUG_IRECV(procConfig.proc_rank(), buffProcs[i/2],
                              localOwnedBuffs[i/2]->mem_ptr, INITIAL_BUFF_SIZE,
                              MB_MESG_REMOTEH_SIZE, incoming2);
            success = MPI_Irecv(localOwnedBuffs[i/2]->mem_ptr, INITIAL_BUFF_SIZE,
                                MPI_UNSIGNED_CHAR, buffProcs[i/2],
                                MB_MESG_REMOTEH_SIZE, procConfig.proc_comm(),
                                &recvRemotehReqs[i]);
            if (success != MPI_SUCCESS) {
              MB_SET_ERR(MB_FAILURE, "Failed to post irecv for remote handles in ghost exchange");
            }
          }
          recvReqs.resize(2*buffProcs.size(), MPI_REQUEST_NULL);
          sendReqs.resize(2*buffProcs.size(), MPI_REQUEST_NULL);
        }
      }
    }

    // Assign and remove newly created elements from/to receive processor
    result = assign_entities_part(new_ents, procConfig.proc_rank());MB_CHK_SET_ERR(result, "Failed to assign entities to part");
    if (migrate) {
      //result = remove_entities_part(allsent, procConfig.proc_rank());MB_CHK_SET_ERR(ressult, "Failed to remove entities to part");
    }

    // Add requests for any new addl procs
    if (recvReqs.size() != 2*buffProcs.size()) {
      // Shouldn't get here...
      MB_SET_ERR(MB_FAILURE, "Requests length doesn't match proc count in entity exchange");
    }

  #ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(ENTITIES_END, procConfig.proc_rank(), "Ending recv entities.");
    }
  #endif

    //===========================================
    // Send local handles for new entity to owner
    //===========================================
    std::set<unsigned int>::iterator it = recv_procs.begin();
    std::set<unsigned int>::iterator eit = recv_procs.end();
    for (; it != eit; ++it) {
      ind = get_buffers(*it);
      // Reserve space on front for size and for initial buff size
      remoteOwnedBuffs[ind]->reset_buffer(sizeof(int));

      result = pack_remote_handles(L1hloc[ind], L1hrem[ind], L1p[ind],
                                   buffProcs[ind], remoteOwnedBuffs[ind]);MB_CHK_SET_ERR(result, "Failed to pack remote handles");
      remoteOwnedBuffs[ind]->set_stored_size();

      if (myDebug->get_verbosity() == 4) {
        msgs.resize(msgs.size() + 1);
        msgs.back() = new Buffer(*remoteOwnedBuffs[ind]);
      }
      result = send_buffer(buffProcs[ind], remoteOwnedBuffs[ind],
                           MB_MESG_REMOTEH_SIZE,
                           sendReqs[2*ind], recvRemotehReqs[2*ind + 1],
                           &ackbuff,
                           incoming2);MB_CHK_SET_ERR(result, "Failed to send remote handles");
    }

    //===========================================
    // Process remote handles of my ghosteds
    //===========================================
    while (incoming2) {
      PRINT_DEBUG_WAITANY(recvRemotehReqs, MB_MESG_REMOTEH_SIZE, procConfig.proc_rank());
      success = MPI_Waitany(2*buffProcs.size(), &recvRemotehReqs[0], &ind, &status);
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitany in owned entity exchange");
      }

      // OK, received something; decrement incoming counter
      incoming2--;

      PRINT_DEBUG_RECD(status);
      bool done = false;
      unsigned int base_ind = 2*(ind/2);
      result = recv_buffer(MB_MESG_REMOTEH_SIZE, status,
                           localOwnedBuffs[ind/2],
                           recvRemotehReqs[ind], recvRemotehReqs[ind + 1], incoming2,
                           remoteOwnedBuffs[ind/2],
                           sendReqs[base_ind], sendReqs[base_ind + 1],
                           done);MB_CHK_SET_ERR(result, "Failed to receive remote handles");
      if (done) {
        // Incoming remote handles
        if (myDebug->get_verbosity() == 4) {
          msgs.resize(msgs.size() + 1);
          msgs.back() = new Buffer(*localOwnedBuffs[ind]);
        }

        localOwnedBuffs[ind/2]->reset_ptr(sizeof(int));
        result = unpack_remote_handles(buffProcs[ind/2],
                                       localOwnedBuffs[ind/2]->buff_ptr,
                                       L2hloc, L2hrem, L2p);MB_CHK_SET_ERR(result, "Failed to unpack remote handles");
      }
    }

  #ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(RHANDLES_END, procConfig.proc_rank(), "Ending remote handles.");
      MPE_Log_event(OWNED_END, procConfig.proc_rank(),
                    "Ending recv entities (still doing checks).");
    }
  #endif
    myDebug->tprintf(1, "Exiting recv_entities.\n");

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::recv_messages(const int from_proc,
                                        const bool store_remote_handles,
                                        const bool is_iface,
                                        Range &final_ents,
                                        int& incoming1,
                                        int& incoming2,
                                        std::vector<std::vector<EntityHandle> > &L1hloc,
                                        std::vector<std::vector<EntityHandle> > &L1hrem,
                                        std::vector<std::vector<int> > &L1p,
                                        std::vector<EntityHandle> &L2hloc,
                                        std::vector<EntityHandle> &L2hrem,
                                        std::vector<unsigned int> &L2p,
                                        std::vector<MPI_Request> &recv_remoteh_reqs)
  {
#ifndef MOAB_HAVE_MPI
    return MB_FAILURE;
#else
    MPI_Status status;
    ErrorCode result;
    int ind1 = get_buffers(from_proc);
    int success, ind2;
    std::vector<EntityHandle> new_ents;

    // Wait and receive messages
    while (incoming1) {
      PRINT_DEBUG_WAITANY(recvReqs, MB_MESG_TAGS_SIZE, procConfig.proc_rank());
      success = MPI_Waitany(2, &recvReqs[2*ind1], &ind2, &status);
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitany in recv_messages");
      }

      PRINT_DEBUG_RECD(status);

      // OK, received something; decrement incoming counter
      incoming1--;
      bool done = false;

      // In case ind is for ack, we need index of one before it
      ind2 += 2*ind1;
      unsigned int base_ind = 2*(ind2/2);

      result = recv_buffer(MB_MESG_ENTS_SIZE, status,
                           remoteOwnedBuffs[ind2/2],
                           //recvbuff,
                           recvReqs[ind2], recvReqs[ind2 + 1],
                           incoming1, localOwnedBuffs[ind2/2],
                           sendReqs[base_ind], sendReqs[base_ind + 1],
                           done,
                           (!is_iface && store_remote_handles ?
                            localOwnedBuffs[ind2/2] : NULL),
                           MB_MESG_REMOTEH_SIZE,
                           &recv_remoteh_reqs[base_ind], &incoming2);MB_CHK_SET_ERR(result, "Failed to receive buffer");

      if (done) {
        // If it is done, unpack buffer
        remoteOwnedBuffs[ind2/2]->reset_ptr(sizeof(int));
        result = unpack_buffer(remoteOwnedBuffs[ind2/2]->buff_ptr,
                               store_remote_handles, from_proc, ind2/2,
                               L1hloc, L1hrem, L1p, L2hloc, L2hrem,
                               L2p, new_ents);MB_CHK_SET_ERR(result, "Failed to unpack buffer in recev_messages");

        std::copy(new_ents.begin(), new_ents.end(), range_inserter(final_ents));

        // Send local handles for new elements to owner
        // Reserve space on front for size and for initial buff size
        remoteOwnedBuffs[ind2/2]->reset_buffer(sizeof(int));

        result = pack_remote_handles(L1hloc[ind2/2], L1hrem[ind2/2], L1p[ind2/2],
                                     from_proc, remoteOwnedBuffs[ind2/2]);MB_CHK_SET_ERR(result, "Failed to pack remote handles");
        remoteOwnedBuffs[ind2/2]->set_stored_size();

        result = send_buffer(buffProcs[ind2/2], remoteOwnedBuffs[ind2/2],
                             MB_MESG_REMOTEH_SIZE,
                             sendReqs[ind2], recv_remoteh_reqs[ind2 + 1],
                             (int*)(localOwnedBuffs[ind2/2]->mem_ptr),
                             //&ackbuff,
                             incoming2);MB_CHK_SET_ERR(result, "Failed to send remote handles");
      }
    }

    return MB_SUCCESS;
#endif
  }

  ErrorCode ParallelComm::recv_remote_handle_messages(const int from_proc,
                                                      int& incoming2,
                                                      std::vector<EntityHandle> &L2hloc,
                                                      std::vector<EntityHandle> &L2hrem,
                                                      std::vector<unsigned int> &L2p,
                                                      std::vector<MPI_Request> &recv_remoteh_reqs)
  {
#ifndef MOAB_HAVE_MPI
    return MB_FAILURE;
#else
    MPI_Status status;
    ErrorCode result;
    int ind1 = get_buffers(from_proc);
    int success, ind2;

    while (incoming2) {
      PRINT_DEBUG_WAITANY(recv_remoteh_reqs, MB_MESG_REMOTEH_SIZE,
                          procConfig.proc_rank());
      success = MPI_Waitany(2, &recv_remoteh_reqs[2*ind1],
                            &ind2, &status);
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitany in recv_remote_handle_messages");
      }

      // OK, received something; decrement incoming counter
      incoming2--;

      PRINT_DEBUG_RECD(status);

      bool done = false;
      ind2 += 2*ind1;
      unsigned int base_ind = 2*(ind2/2);
      result = recv_buffer(MB_MESG_REMOTEH_SIZE, status,
                           localOwnedBuffs[ind2/2],
                           recv_remoteh_reqs[ind2], recv_remoteh_reqs[ind2 + 1], incoming2,
                           remoteOwnedBuffs[ind2/2],
                           sendReqs[base_ind], sendReqs[base_ind + 1],
                           done);MB_CHK_SET_ERR(result, "Failed to receive remote handles");
      if (done) {
        // Incoming remote handles
        localOwnedBuffs[ind2/2]->reset_ptr(sizeof(int));
        result = unpack_remote_handles(buffProcs[ind2/2],
                                       localOwnedBuffs[ind2/2]->buff_ptr,
                                       L2hloc, L2hrem, L2p);MB_CHK_SET_ERR(result, "Failed to unpack remote handles");
      }
    }

    return MB_SUCCESS;
#endif
  }

  ErrorCode ParallelComm::pack_buffer(Range &orig_ents, 
                                      const bool /*adjacencies*/,
                                      const bool tags,
                                      const bool store_remote_handles,
                                      const int to_proc,
                                      Buffer *buff,
                                      TupleList *entprocs,
                                      Range *allsent)
  {
    // Pack the buffer with the entity ranges, adjacencies, and tags sections
    // 
    // Note: new entities used in subsequent connectivity lists, sets, or tags, 
    // are referred to as (MBMAXTYPE + index), where index is into vector
    // of new entities, 0-based
    ErrorCode result;

    Range set_range;
    std::vector<Tag> all_tags;
    std::vector<Range> tag_ranges;

    Range::const_iterator rit;

    // Entities
    result = pack_entities(orig_ents, buff,
                           store_remote_handles, to_proc, false,
                           entprocs, allsent);MB_CHK_SET_ERR(result, "Packing entities failed");

    // Sets
    result = pack_sets(orig_ents, buff,
                       store_remote_handles, to_proc);MB_CHK_SET_ERR(result, "Packing sets (count) failed");

    // Tags
    Range final_ents;
    if (tags) {
      result = get_tag_send_list(orig_ents, all_tags, tag_ranges);MB_CHK_SET_ERR(result, "Failed to get tagged entities");
      result = pack_tags(orig_ents, all_tags, all_tags, tag_ranges, 
                         buff, store_remote_handles, to_proc);MB_CHK_SET_ERR(result, "Packing tags (count) failed");
    }
    else { // Set tag size to 0
      buff->check_space(sizeof(int));
      PACK_INT(buff->buff_ptr, 0);
      buff->set_stored_size();
    }

    return result;
  }

  ErrorCode ParallelComm::unpack_buffer(unsigned char *buff_ptr,
                                        const bool store_remote_handles,
                                        const int from_proc,
                                        const int ind,
                                        std::vector<std::vector<EntityHandle> > &L1hloc,
                                        std::vector<std::vector<EntityHandle> > &L1hrem,
                                        std::vector<std::vector<int> > &L1p,
                                        std::vector<EntityHandle> &L2hloc, 
                                        std::vector<EntityHandle> &L2hrem,
                                        std::vector<unsigned int> &L2p,
                                        std::vector<EntityHandle> &new_ents,
                                        const bool created_iface)
  {
    unsigned char *tmp_buff = buff_ptr;
    ErrorCode result;
    result = unpack_entities(buff_ptr, store_remote_handles,
                             ind, false, L1hloc, L1hrem, L1p,
                             L2hloc, L2hrem, L2p, new_ents,
                             created_iface);MB_CHK_SET_ERR(result, "Unpacking entities failed");
    if (myDebug->get_verbosity() == 3) {
      myDebug->tprintf(4, "unpack_entities buffer space: %ld bytes.\n", (long int)(buff_ptr - tmp_buff));
      tmp_buff = buff_ptr;
    }
    result = unpack_sets(buff_ptr, new_ents, store_remote_handles, from_proc);MB_CHK_SET_ERR(result, "Unpacking sets failed");
    if (myDebug->get_verbosity() == 3) {
      myDebug->tprintf(4, "unpack_sets buffer space: %ld bytes.\n", (long int)(buff_ptr - tmp_buff));
      tmp_buff = buff_ptr;
    }
    result = unpack_tags(buff_ptr, new_ents, store_remote_handles, from_proc);MB_CHK_SET_ERR(result, "Unpacking tags failed");
    if (myDebug->get_verbosity() == 3) {
      myDebug->tprintf(4, "unpack_tags buffer space: %ld bytes.\n", (long int)(buff_ptr - tmp_buff));
      //tmp_buff = buff_ptr;
    }

    if (myDebug->get_verbosity() == 3)
      myDebug->print(4, "\n");

    return MB_SUCCESS;
  }

  int ParallelComm::estimate_ents_buffer_size(Range &entities,
                                              const bool store_remote_handles) 
  {
    int buff_size = 0;
    std::vector<EntityHandle> dum_connect_vec;
    const EntityHandle *connect;
    int num_connect;

    int num_verts = entities.num_of_type(MBVERTEX);
    // # verts + coords + handles
    buff_size += 2*sizeof(int) + 3*sizeof(double)*num_verts;
    if (store_remote_handles) buff_size += sizeof(EntityHandle)*num_verts;

    // Do a rough count by looking at first entity of each type
    for (EntityType t = MBEDGE; t < MBENTITYSET; t++) {
      const Range::iterator rit = entities.lower_bound(t);
      if (TYPE_FROM_HANDLE(*rit) != t)
        continue;

      ErrorCode result = mbImpl->get_connectivity(*rit, connect, num_connect, 
                                                  false, &dum_connect_vec);MB_CHK_SET_ERR_RET_VAL(result, "Failed to get connectivity to estimate buffer size", -1);

      // Number, type, nodes per entity
      buff_size += 3*sizeof(int);
      int num_ents = entities.num_of_type(t);
      // Connectivity, handle for each ent
      buff_size += (num_connect + 1)*sizeof(EntityHandle)*num_ents;
    }

    // Extra entity type at end, passed as int
    buff_size += sizeof(int);

    return buff_size;
  }

  int ParallelComm::estimate_sets_buffer_size(Range &entities,
                                              const bool /*store_remote_handles*/)
  {
    // Number of sets
    int buff_size = sizeof(int);

    // Do a rough count by looking at first entity of each type
    Range::iterator rit = entities.lower_bound(MBENTITYSET);
    ErrorCode result;

    for (; rit != entities.end(); ++rit) {
      unsigned int options;
      result = mbImpl->get_meshset_options(*rit, options);MB_CHK_SET_ERR_RET_VAL(result, "Failed to get meshset options", -1);

      buff_size += sizeof(int);

      Range set_range;
      if (options & MESHSET_SET) {
        // Range-based set; count the subranges
        result = mbImpl->get_entities_by_handle(*rit, set_range);MB_CHK_SET_ERR_RET_VAL(result, "Failed to get set entities", -1);

        // Set range
        buff_size += RANGE_SIZE(set_range);
      }
      else if (options & MESHSET_ORDERED) {
        // Just get the number of entities in the set
        int num_ents;
        result = mbImpl->get_number_entities_by_handle(*rit, num_ents);MB_CHK_SET_ERR_RET_VAL(result, "Failed to get number entities in ordered set", -1);

        // Set vec
        buff_size += sizeof(EntityHandle) * num_ents + sizeof(int);
      }

      // Get numbers of parents/children
      int num_par, num_ch;
      result = mbImpl->num_child_meshsets(*rit, &num_ch);MB_CHK_SET_ERR_RET_VAL(result, "Failed to get num children", -1);
      result = mbImpl->num_parent_meshsets(*rit, &num_par);MB_CHK_SET_ERR_RET_VAL(result, "Failed to get num parents", -1);

      buff_size += (num_ch + num_par) * sizeof(EntityHandle) + 2*sizeof(int);
    }

    return buff_size;
  }

  ErrorCode ParallelComm::pack_entities(Range &entities,
                                        Buffer *buff,
                                        const bool store_remote_handles,
                                        const int to_proc,
                                        const bool /*is_iface*/,
                                        TupleList *entprocs,
                                        Range */*allsent*/)
  {
    // Packed information:
    // 1. # entities = E
    // 2. for e in E
    //   a. # procs sharing e, incl. sender and receiver = P
    //   b. for p in P (procs sharing e)
    //   c. for p in P (handle for e on p) (Note1)
    // 3. vertex/entity info

    // Get an estimate of the buffer size & pre-allocate buffer size
    int buff_size = estimate_ents_buffer_size(entities, store_remote_handles);
    if (buff_size < 0)
      MB_SET_ERR(MB_FAILURE, "Failed to estimate ents buffer size");
    buff->check_space(buff_size);
    myDebug->tprintf(3, "estimate buffer size for %d entities: %d \n", (int)entities.size(), buff_size  );

    unsigned int num_ents;
    ErrorCode result;

    std::vector<EntityHandle> entities_vec(entities.size());
    std::copy(entities.begin(), entities.end(), entities_vec.begin());

    // First pack procs/handles sharing this ent, not including this dest but including
    // others (with zero handles)
    if (store_remote_handles) {
      // Buff space is at least proc + handle for each entity; use avg of 4 other procs
      // to estimate buff size, but check later
      buff->check_space(sizeof(int) + (5*sizeof(int) + sizeof(EntityHandle))*entities.size());

      // 1. # entities = E
      PACK_INT(buff->buff_ptr, entities.size());

      Range::iterator rit;

      // Pre-fetch sharedp and pstatus
      std::vector<int> sharedp_vals(entities.size());
      result = mbImpl->tag_get_data(sharedp_tag(), entities, &sharedp_vals[0]);MB_CHK_SET_ERR(result, "Failed to get sharedp tag data");
      std::vector<char> pstatus_vals(entities.size());
      result = mbImpl->tag_get_data(pstatus_tag(), entities, &pstatus_vals[0]);MB_CHK_SET_ERR(result, "Failed to get pstatus tag data");

      unsigned int i;
      int tmp_procs[MAX_SHARING_PROCS];
      EntityHandle tmp_handles[MAX_SHARING_PROCS];
      std::set<unsigned int> dumprocs;

      // 2. for e in E
      for (rit = entities.begin(), i = 0; 
           rit != entities.end(); ++rit, i++) {
        unsigned int ind = std::lower_bound(entprocs->vul_rd, entprocs->vul_rd + entprocs->get_n(), *rit) - entprocs->vul_rd;
        assert(ind < entprocs->get_n());

        while (ind < entprocs->get_n() && entprocs->vul_rd[ind] == *rit)
          dumprocs.insert(entprocs->vi_rd[ind++]);

        result = build_sharedhps_list(*rit, pstatus_vals[i], sharedp_vals[i],
                                      dumprocs, num_ents, tmp_procs, tmp_handles);MB_CHK_SET_ERR(result, "Failed to build sharedhps");

        dumprocs.clear();

        // Now pack them
        buff->check_space((num_ents + 1)*sizeof(int) +
                          num_ents*sizeof(EntityHandle));
        PACK_INT(buff->buff_ptr, num_ents);
        PACK_INTS(buff->buff_ptr, tmp_procs, num_ents);
        PACK_EH(buff->buff_ptr, tmp_handles, num_ents);

#ifndef NDEBUG
        // Check for duplicates in proc list
        unsigned int dp = 0;
        for (; dp < MAX_SHARING_PROCS && -1 != tmp_procs[dp]; dp++)
          dumprocs.insert(tmp_procs[dp]);
        assert(dumprocs.size() == dp);
        dumprocs.clear();
#endif
      }
    }

    // Pack vertices
    Range these_ents = entities.subset_by_type(MBVERTEX);
    num_ents = these_ents.size();

    if (num_ents) {
      buff_size = 2*sizeof(int) + 3*num_ents*sizeof(double);
      buff->check_space(buff_size);

      // Type, # ents
      PACK_INT(buff->buff_ptr, ((int) MBVERTEX));
      PACK_INT(buff->buff_ptr, ((int) num_ents));

      std::vector<double> tmp_coords(3*num_ents);
      result = mbImpl->get_coords(these_ents, &tmp_coords[0]);MB_CHK_SET_ERR(result, "Failed to get vertex coordinates");
      PACK_DBLS(buff->buff_ptr, &tmp_coords[0], 3*num_ents);

      myDebug->tprintf(4, "Packed %lu ents of type %s\n", (unsigned long)these_ents.size(),
                       CN::EntityTypeName(TYPE_FROM_HANDLE(*these_ents.begin())));
    }

    // Now entities; go through range, packing by type and equal # verts per element
    Range::iterator start_rit = entities.find(*these_ents.rbegin());
    ++start_rit;
    int last_nodes = -1;
    EntityType last_type = MBMAXTYPE;
    these_ents.clear();
    Range::iterator end_rit = start_rit;
    EntitySequence *seq;
    ElementSequence *eseq;

    while (start_rit != entities.end() || !these_ents.empty()) {
      // Cases:
      // A: !end, last_type == MBMAXTYPE, seq: save contig sequence in these_ents
      // B: !end, last type & nodes same, seq: save contig sequence in these_ents
      // C: !end, last type & nodes different: pack these_ents, then save contig sequence in these_ents
      // D: end: pack these_ents

      // Find the sequence holding current start entity, if we're not at end
      eseq = NULL;
      if (start_rit != entities.end()) {
        result = sequenceManager->find(*start_rit, seq);MB_CHK_SET_ERR(result, "Failed to find entity sequence");
        if (NULL == seq)
          return MB_FAILURE;
        eseq = dynamic_cast<ElementSequence*>(seq);
      }

      // Pack the last batch if at end or next one is different
      if (!these_ents.empty() &&
          (!eseq || eseq->type() != last_type ||
           last_nodes != (int) eseq->nodes_per_element())) {
        result = pack_entity_seq(last_nodes, store_remote_handles,
                                 to_proc, these_ents, entities_vec, buff);MB_CHK_SET_ERR(result, "Failed to pack entities from a sequence");
        these_ents.clear();
      }

      if (eseq) {
        // Continuation of current range, just save these entities
        // Get position in entities list one past end of this sequence
        end_rit = entities.lower_bound(start_rit, entities.end(), eseq->end_handle() + 1);

        // Put these entities in the range
        std::copy(start_rit, end_rit, range_inserter(these_ents));

        last_type = eseq->type();
        last_nodes = eseq->nodes_per_element();
      }
      else if (start_rit != entities.end() &&
               TYPE_FROM_HANDLE(*start_rit) == MBENTITYSET)
        break;

      start_rit = end_rit;
    }

    // Pack MBMAXTYPE to indicate end of ranges
    buff->check_space(sizeof(int));
    PACK_INT(buff->buff_ptr, ((int)MBMAXTYPE));

    buff->set_stored_size();
    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::build_sharedhps_list(const EntityHandle entity,
                                               const unsigned char pstatus,
                                               const int
#ifndef NDEBUG
                                               sharedp
#endif
                                               ,
                                               const std::set<unsigned int> &procs,
                                               unsigned int &num_ents,
                                               int *tmp_procs,
                                               EntityHandle *tmp_handles)
  {
    num_ents = 0;
    unsigned char pstat;
    ErrorCode result = get_sharing_data(entity, tmp_procs, tmp_handles,
                                        pstat, num_ents);MB_CHK_SET_ERR(result, "Failed to get sharing data");
    assert(pstat == pstatus);

    // Build shared proc/handle lists
    // Start with multi-shared, since if it is the owner will be first
    if (pstatus & PSTATUS_MULTISHARED) {
    }
    else if (pstatus & PSTATUS_NOT_OWNED) {
      // If not multishared and not owned, other sharing proc is owner, put that
      // one first
      assert("If not owned, I should be shared too" &&
             pstatus & PSTATUS_SHARED &&
             1 == num_ents);
      tmp_procs[1] = procConfig.proc_rank();
      tmp_handles[1] = entity;
      num_ents = 2;
    }
    else if (pstatus & PSTATUS_SHARED) {
      // If not multishared and owned, I'm owner
      assert("shared and owned, should be only 1 sharing proc" && 1 == num_ents);
      tmp_procs[1] = tmp_procs[0];
      tmp_procs[0] = procConfig.proc_rank();
      tmp_handles[1] = tmp_handles[0];
      tmp_handles[0] = entity;
      num_ents = 2;
    }
    else {
      // Not shared yet, just add owner (me)
      tmp_procs[0] = procConfig.proc_rank();
      tmp_handles[0] = entity;
      num_ents = 1;
    }

#ifndef NDEBUG
    int tmp_ps = num_ents;
#endif

    // Now add others, with zero handle for now
    for (std::set<unsigned int>::iterator sit = procs.begin();
         sit != procs.end(); ++sit) {
#ifndef NDEBUG
      if (tmp_ps && std::find(tmp_procs, tmp_procs + tmp_ps, *sit) != tmp_procs + tmp_ps) {
        std::cerr << "Trouble with something already in shared list on proc " << procConfig.proc_rank()
                  << ". Entity:" << std::endl;
        list_entities(&entity, 1);
        std::cerr << "pstatus = " << (int) pstatus << ", sharedp = " << sharedp << std::endl;
        std::cerr << "tmp_ps = ";
        for (int i = 0; i < tmp_ps; i++)
          std::cerr << tmp_procs[i] << " ";
        std::cerr << std::endl;
        std::cerr << "procs = ";
        for (std::set<unsigned int>::iterator sit2 = procs.begin(); sit2 != procs.end(); ++sit2)
          std::cerr << *sit2 << " ";
        assert(false);
      }
#endif
      tmp_procs[num_ents] = *sit;
      tmp_handles[num_ents] = 0;
      num_ents++;
    }

    // Put -1 after procs and 0 after handles
    if (MAX_SHARING_PROCS > num_ents) {
      tmp_procs[num_ents] = -1;
      tmp_handles[num_ents] = 0;
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::pack_entity_seq(const int nodes_per_entity,
                                          const bool store_remote_handles,
                                          const int to_proc,
                                          Range &these_ents,
                                          std::vector<EntityHandle> &entities_vec,
                                          Buffer *buff)
  {
    int tmp_space = 3*sizeof(int) + nodes_per_entity*these_ents.size()*sizeof(EntityHandle);
    buff->check_space(tmp_space);

    // Pack the entity type
    PACK_INT(buff->buff_ptr, ((int)TYPE_FROM_HANDLE(*these_ents.begin())));

    // Pack # ents
    PACK_INT(buff->buff_ptr, these_ents.size());

    // Pack the nodes per entity
    PACK_INT(buff->buff_ptr, nodes_per_entity);
    myDebug->tprintf(3, "after some pack int  %d \n", buff->get_current_size() );

    // Pack the connectivity
    std::vector<EntityHandle> connect;
    ErrorCode result = MB_SUCCESS;
    for (Range::const_iterator rit = these_ents.begin(); rit != these_ents.end(); ++rit) {
      connect.clear();
      result = mbImpl->get_connectivity(&(*rit), 1, connect, false);MB_CHK_SET_ERR(result, "Failed to get connectivity");
      assert((int)connect.size() == nodes_per_entity);
      result = get_remote_handles(store_remote_handles, &connect[0], &connect[0],
                                  connect.size(), to_proc, entities_vec);MB_CHK_SET_ERR(result, "Failed in get_remote_handles");
      PACK_EH(buff->buff_ptr, &connect[0], connect.size());
    }

    myDebug->tprintf(3, "Packed %lu ents of type %s\n", (unsigned long)these_ents.size(),
                     CN::EntityTypeName(TYPE_FROM_HANDLE(*these_ents.begin())));

    return result;
  }

  ErrorCode ParallelComm::get_remote_handles(const bool store_remote_handles,
                                             EntityHandle *from_vec,
                                             EntityHandle *to_vec_tmp,
                                             int num_ents, int to_proc,
                                             const std::vector<EntityHandle> &new_ents)
  {
    // NOTE: THIS IMPLEMENTATION IS JUST LIKE THE RANGE-BASED VERSION, NO REUSE
    // AT THIS TIME, SO IF YOU FIX A BUG IN THIS VERSION, IT MAY BE IN THE
    // OTHER VERSION TOO!!!
    if (0 == num_ents)
      return MB_SUCCESS;

    // Use a local destination ptr in case we're doing an in-place copy
    std::vector<EntityHandle> tmp_vector;
    EntityHandle *to_vec = to_vec_tmp;
    if (to_vec == from_vec) {
      tmp_vector.resize(num_ents);
      to_vec = &tmp_vector[0];
    }

    if (!store_remote_handles) {
      int err;
      // In this case, substitute position in new_ents list
      for (int i = 0; i < num_ents; i++) {
        int ind = std::lower_bound(new_ents.begin(), new_ents.end(), from_vec[i]) - new_ents.begin();
        assert(new_ents[ind] == from_vec[i]);
        to_vec[i] = CREATE_HANDLE(MBMAXTYPE, ind, err);
        assert(to_vec[i] != 0 && !err && -1 != ind);
      }
    }
    else {
      Tag shp_tag, shps_tag, shh_tag, shhs_tag, pstat_tag;
      ErrorCode result = get_shared_proc_tags(shp_tag, shps_tag,
                                              shh_tag, shhs_tag, pstat_tag);MB_CHK_SET_ERR(result, "Failed to get shared proc tags");

      // Get single-proc destination handles and shared procs
      std::vector<int> sharing_procs(num_ents);
      result = mbImpl->tag_get_data(shh_tag, from_vec, num_ents,
                                    to_vec);MB_CHK_SET_ERR(result, "Failed to get shared handle tag for remote_handles");
      result = mbImpl->tag_get_data(shp_tag, from_vec, num_ents, &sharing_procs[0]);MB_CHK_SET_ERR(result, "Failed to get sharing proc tag in remote_handles");
      for (int j = 0; j < num_ents; j++) {
        if (to_vec[j] && sharing_procs[j] != to_proc)
          to_vec[j] = 0;
      }

      EntityHandle tmp_handles[MAX_SHARING_PROCS];
      int tmp_procs[MAX_SHARING_PROCS];
      int i;
      // Go through results, and for 0-valued ones, look for multiple shared proc
      for (i = 0; i < num_ents; i++) {
        if (!to_vec[i]) {
          result = mbImpl->tag_get_data(shps_tag, from_vec + i, 1, tmp_procs);
          if (MB_SUCCESS == result) {
            for (int j = 0; j < MAX_SHARING_PROCS; j++) {
              if (-1 == tmp_procs[j])
                break;
              else if (tmp_procs[j] == to_proc) {
                result = mbImpl->tag_get_data(shhs_tag, from_vec + i, 1, tmp_handles);MB_CHK_SET_ERR(result, "Failed to get sharedhs tag data");
                to_vec[i] = tmp_handles[j];
                assert(to_vec[i]);
                break;
              }
            }
          }
          if (!to_vec[i]) {
            int j = std::lower_bound(new_ents.begin(), new_ents.end(), from_vec[i]) - new_ents.begin();
            if ((int)new_ents.size() == j) {
              std::cout << "Failed to find new entity in send list, proc "
                        << procConfig.proc_rank() << std::endl;
              for (int k = 0; k <= num_ents; k++)
                std::cout << k << ": " << from_vec[k] << " " << to_vec[k]
                          << std::endl;
              MB_SET_ERR(MB_FAILURE, "Failed to find new entity in send list");
            }
            int err;
            to_vec[i] = CREATE_HANDLE(MBMAXTYPE, j, err);
            if (err) {
              MB_SET_ERR(MB_FAILURE, "Failed to create handle in remote_handles");
            }
          }
        }
      }
    }

    // memcpy over results if from_vec and to_vec are the same
    if (to_vec_tmp == from_vec)
      memcpy(from_vec, to_vec, num_ents * sizeof(EntityHandle));

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_remote_handles(const bool store_remote_handles,
                                             const Range &from_range,
                                             EntityHandle *to_vec,
                                             int to_proc,
                                             const std::vector<EntityHandle> &new_ents)
  {
    // NOTE: THIS IMPLEMENTATION IS JUST LIKE THE VECTOR-BASED VERSION, NO REUSE
    // AT THIS TIME, SO IF YOU FIX A BUG IN THIS VERSION, IT MAY BE IN THE
    // OTHER VERSION TOO!!!
    if (from_range.empty())
      return MB_SUCCESS;

    if (!store_remote_handles) {
      int err;
      // In this case, substitute position in new_ents list
      Range::iterator rit;
      unsigned int i;
      for (rit = from_range.begin(), i = 0; rit != from_range.end(); ++rit, i++) {
        int ind = std::lower_bound(new_ents.begin(), new_ents.end(), *rit) - new_ents.begin();
        assert(new_ents[ind] == *rit);
        to_vec[i] = CREATE_HANDLE(MBMAXTYPE, ind, err);
        assert(to_vec[i] != 0 && !err && -1 != ind);
      }
    }
    else {
      Tag shp_tag, shps_tag, shh_tag, shhs_tag, pstat_tag;
      ErrorCode result = get_shared_proc_tags(shp_tag, shps_tag, 
                                              shh_tag, shhs_tag, pstat_tag);MB_CHK_SET_ERR(result, "Failed to get shared proc tags");

      // Get single-proc destination handles and shared procs
      std::vector<int> sharing_procs(from_range.size());
      result = mbImpl->tag_get_data(shh_tag, from_range, to_vec);MB_CHK_SET_ERR(result, "Failed to get shared handle tag for remote_handles");
      result = mbImpl->tag_get_data(shp_tag, from_range, &sharing_procs[0]);MB_CHK_SET_ERR(result, "Failed to get sharing proc tag in remote_handles");
      for (unsigned int j = 0; j < from_range.size(); j++) {
        if (to_vec[j] && sharing_procs[j] != to_proc)
          to_vec[j] = 0;
      }

      EntityHandle tmp_handles[MAX_SHARING_PROCS];
      int tmp_procs[MAX_SHARING_PROCS];
      // Go through results, and for 0-valued ones, look for multiple shared proc
      Range::iterator rit;
      unsigned int i;
      for (rit = from_range.begin(), i = 0; rit != from_range.end(); ++rit, i++) {
        if (!to_vec[i]) {
          result = mbImpl->tag_get_data(shhs_tag, &(*rit), 1, tmp_handles);
          if (MB_SUCCESS == result) {
            result = mbImpl->tag_get_data(shps_tag, &(*rit), 1, tmp_procs);MB_CHK_SET_ERR(result, "Failed to get sharedps tag data");
            for (int j = 0; j < MAX_SHARING_PROCS; j++)
              if (tmp_procs[j] == to_proc) {
                to_vec[i] = tmp_handles[j];
                break;
              }
          }

          if (!to_vec[i]) {
            int j = std::lower_bound(new_ents.begin(), new_ents.end(), *rit) - new_ents.begin();
            if ((int)new_ents.size() == j) {
              MB_SET_ERR(MB_FAILURE, "Failed to find new entity in send list");
            }
            int err;
            to_vec[i] = CREATE_HANDLE(MBMAXTYPE, j, err);
            if (err) {
              MB_SET_ERR(MB_FAILURE, "Failed to create handle in remote_handles");
            }
          }
        }
      }
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_remote_handles(const bool store_remote_handles,
                                             const Range &from_range,
                                             Range &to_range,
                                             int to_proc,
                                             const std::vector<EntityHandle> &new_ents)
  {
    std::vector<EntityHandle> to_vector(from_range.size());

    ErrorCode result =
      get_remote_handles(store_remote_handles, from_range, &to_vector[0],
                         to_proc, new_ents);MB_CHK_SET_ERR(result, "Failed to get remote handles");
    std::copy(to_vector.begin(), to_vector.end(), range_inserter(to_range));
    return result;
  }

  ErrorCode ParallelComm::unpack_entities(unsigned char *&buff_ptr,
                                          const bool store_remote_handles,
                                          const int /*from_ind*/,
                                          const bool is_iface,
                                          std::vector<std::vector<EntityHandle> > &L1hloc,
                                          std::vector<std::vector<EntityHandle> > &L1hrem,
                                          std::vector<std::vector<int> > &L1p,
                                          std::vector<EntityHandle> &L2hloc,
                                          std::vector<EntityHandle> &L2hrem,
                                          std::vector<unsigned int> &L2p,
                                          std::vector<EntityHandle> &new_ents,
                                          const bool created_iface)
  {
    // General algorithm:
    // - unpack # entities
    // - save start of remote handle info, then scan forward to entity definition data
    // - for all vertices or entities w/ same # verts:
    //   . get entity type, num ents, and (if !vert) # verts 
    //   . for each ent:
    //      o get # procs/handles in remote handle info
    //      o if # procs/handles > 2, check for already-created entity:
    //        x get index of owner proc (1st in proc list), resize L1 list if nec
    //        x look for already-arrived entity in L2 by owner handle
    //      o if no existing entity:
    //        x if iface, look for existing entity with same connect & type
    //        x if none found, create vertex or element
    //        x if !iface & multi-shared, save on L2
    //        x if !iface, put new entity on new_ents list
    //      o update proc/handle, pstatus tags, adjusting to put owner first if iface
    //      o if !iface, save new handle on L1 for all sharing procs

    // Lists of handles/procs to return to sending/other procs
    // L1hloc[p], L1hrem[p]: handle pairs [h, h'], where h is the local proc handle
    //         and h' is either the remote proc handle (if that is known) or
    //         the owner proc handle (otherwise);
    // L1p[p]: indicates whether h is remote handle (= -1) or owner (rank of owner)
    // L2hloc, L2hrem: local/remote handles for entities shared by > 2 procs;
    //         remote handles are on owning proc
    // L2p: owning procs for handles in L2hrem

    ErrorCode result;
    bool done = false;
    ReadUtilIface *ru = NULL;

    result = mbImpl->query_interface(ru);MB_CHK_SET_ERR(result, "Failed to get ReadUtilIface");

    // 1. # entities = E
    int num_ents = 0;
    unsigned char *buff_save = buff_ptr;
    int i, j;

    if (store_remote_handles) {
      UNPACK_INT(buff_ptr, num_ents);

      buff_save = buff_ptr;

      // Save place where remote handle info starts, then scan forward to ents
      for (i = 0; i < num_ents; i++) {
        UNPACK_INT(buff_ptr, j);
        if (j < 0) {
          std::cout << "Should be non-negative # proc/handles.";
          return MB_FAILURE;
        }

        buff_ptr += j * (sizeof(int) + sizeof(EntityHandle));
      }
    }

    std::vector<EntityHandle> msg_ents;

    while (!done) {
      EntityType this_type = MBMAXTYPE;
      UNPACK_TYPE(buff_ptr, this_type);
      assert(this_type != MBENTITYSET);

      // MBMAXTYPE signifies end of entities data
      if (MBMAXTYPE == this_type)
        break;

      // Get the number of ents
      int num_ents2, verts_per_entity = 0;
      UNPACK_INT(buff_ptr, num_ents2);

      // Unpack the nodes per entity
      if (MBVERTEX != this_type && num_ents2) {
        UNPACK_INT(buff_ptr, verts_per_entity);
      }

      std::vector<int> ps(MAX_SHARING_PROCS, -1);
      std::vector<EntityHandle> hs(MAX_SHARING_PROCS, 0);
      for (int e = 0; e < num_ents2; e++) {
        // Check for existing entity, otherwise make new one
        EntityHandle new_h = 0;
        EntityHandle connect[CN::MAX_NODES_PER_ELEMENT];
        double coords[3];
        int num_ps = -1;

        //=======================================
        // Unpack all the data at once, to make sure the buffer pointers
        // are tracked correctly
        //=======================================
        if (store_remote_handles) {
          // Pointers to other procs/handles
          UNPACK_INT(buff_save, num_ps);
          if (0 >= num_ps) {
            std::cout << "Shouldn't ever be fewer than 1 procs here." << std::endl;
            return MB_FAILURE;
          }

          UNPACK_INTS(buff_save, &ps[0], num_ps);
          UNPACK_EH(buff_save, &hs[0], num_ps);
        }

        if (MBVERTEX == this_type) {
          UNPACK_DBLS(buff_ptr, coords, 3);
        }
        else {
          assert(verts_per_entity <= CN::MAX_NODES_PER_ELEMENT);
          UNPACK_EH(buff_ptr, connect, verts_per_entity);

          // Update connectivity to local handles
          result = get_local_handles(connect, verts_per_entity, msg_ents);MB_CHK_SET_ERR(result, "Failed to get local handles");
        }

        //=======================================
        // Now, process that data; begin by finding an identical
        // entity, if there is one
        //=======================================
        if (store_remote_handles) {
          result = find_existing_entity(is_iface, ps[0], hs[0], num_ps, 
                                        connect, verts_per_entity,
                                        this_type,
                                        L2hloc, L2hrem, L2p,
                                        new_h);MB_CHK_SET_ERR(result, "Failed to get existing entity");
        }

        //=======================================
        // If we didn't find one, we'll have to create one
        //=======================================
        bool created_here = false;
        if (!new_h && !is_iface) {
          if (MBVERTEX == this_type) {
            // Create a vertex
            result = mbImpl->create_vertex(coords, new_h);MB_CHK_SET_ERR(result, "Failed to make new vertex");
          }
          else {
            // Create the element
            result = mbImpl->create_element(this_type, connect, verts_per_entity,
                                            new_h);MB_CHK_SET_ERR(result, "Failed to make new element");

            // Update adjacencies
            result = ru->update_adjacencies(new_h, 1, verts_per_entity,
                                            connect);MB_CHK_SET_ERR(result, "Failed to update adjacencies");
          }

          // Should have a new handle now
          assert(new_h);

          created_here = true;
        }

        //=======================================
        // Take care of sharing data
        //=======================================

        // Need to save entities found in order, for interpretation of
        // later parts of this message
        if (!is_iface) {
          assert(new_h);
          msg_ents.push_back(new_h);
        }

        if (created_here)
          new_ents.push_back(new_h);

        if (new_h && store_remote_handles) {
          unsigned char new_pstat = 0x0;
          if (is_iface) {
            new_pstat = PSTATUS_INTERFACE;
            // Here, lowest rank proc should be first
            int idx = std::min_element(&ps[0], &ps[0] + num_ps) - &ps[0];
            if (idx) {
              std::swap(ps[0], ps[idx]);
              std::swap(hs[0], hs[idx]);
            }
            // Set ownership based on lowest rank; can't be in update_remote_data, because
            // there we don't know whether it resulted from ghosting or not
            if ((num_ps > 1 && ps[0] != (int) rank()))
              new_pstat |= PSTATUS_NOT_OWNED;
          }
          else if (created_here) {
            if (created_iface)
              new_pstat = PSTATUS_NOT_OWNED;
            else
              new_pstat = PSTATUS_GHOST | PSTATUS_NOT_OWNED;
          }

          // Update sharing data and pstatus, adjusting order if iface
          result = update_remote_data(new_h, &ps[0], &hs[0], num_ps, new_pstat);MB_CHK_SET_ERR(result, "unpack_entities");

          // If a new multi-shared entity, save owner for subsequent lookup in L2 lists
          if (store_remote_handles && !is_iface && num_ps > 2) {
            L2hrem.push_back(hs[0]);
            L2hloc.push_back(new_h);
            L2p.push_back(ps[0]);
          }

          // Need to send this new handle to all sharing procs
          if (!is_iface) {
            for (j = 0; j < num_ps; j++) {
              if (ps[j] == (int)procConfig.proc_rank())
                continue;
              int idx = get_buffers(ps[j]);
              if (idx == (int)L1hloc.size()) {
                L1hloc.resize(idx + 1);
                L1hrem.resize(idx + 1);
                L1p.resize(idx + 1);
              }

              // Don't bother adding if it's already in the list
              std::vector<EntityHandle>::iterator vit = 
                std::find(L1hloc[idx].begin(), L1hloc[idx].end(), new_h);
              if (vit != L1hloc[idx].end()) {
                // If it's in the list but remote handle isn't known but we know
                // it, replace in the list
                if (L1p[idx][vit-L1hloc[idx].begin()] != -1 && hs[j]) {
                  L1hrem[idx][vit-L1hloc[idx].begin()] = hs[j];
                  L1p[idx][vit-L1hloc[idx].begin()] = -1;
                }
                else
                  continue;
              }
              else {
                if (!hs[j]) {
                  assert(-1 != ps[0] && num_ps > 2);
                  L1p[idx].push_back(ps[0]);
                  L1hrem[idx].push_back(hs[0]);
                }
                else {
                  assert("either this remote handle isn't in the remote list, or it's for another proc" &&
                         (std::find(L1hrem[idx].begin(), L1hrem[idx].end(), hs[j]) ==
                          L1hrem[idx].end() ||
                          L1p[idx][std::find(L1hrem[idx].begin(), L1hrem[idx].end(), hs[j]) -
                                   L1hrem[idx].begin()] != -1));
                  L1p[idx].push_back(-1);
                  L1hrem[idx].push_back(hs[j]);
                }
                L1hloc[idx].push_back(new_h);
              }
            }
          }

          assert("Shouldn't be here for non-shared entities" && -1 != num_ps);
          std::fill(&ps[0], &ps[num_ps], -1);
          std::fill(&hs[0], &hs[num_ps], 0);
        }
      }

      myDebug->tprintf(4, "Unpacked %d ents of type %s", num_ents2,
                       CN::EntityTypeName(this_type));
    }

    myDebug->tprintf(4, "Done unpacking entities.\n");

    // Need to sort here, to enable searching
    std::sort(new_ents.begin(), new_ents.end());

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::print_buffer(unsigned char *buff_ptr,
                                       int mesg_tag,
                                       int from_proc, bool sent)
  {
    std::cerr << procConfig.proc_rank();
    if (sent)
      std::cerr << " sent";
    else
      std::cerr << " received";
    std::cerr << " message type " << mesg_tag
              << " to/from proc " << from_proc << "; contents:" << std::endl;

    int msg_length, num_ents;
    unsigned char *orig_ptr = buff_ptr;
    UNPACK_INT(buff_ptr, msg_length);
    std::cerr << msg_length << " bytes..." << std::endl;

    if (MB_MESG_ENTS_SIZE == mesg_tag || MB_MESG_ENTS_LARGE == mesg_tag) {
      // 1. # entities = E
      int i, j, k;
      std::vector<int> ps;
      std::vector<EntityHandle> hs;

      UNPACK_INT(buff_ptr, num_ents);
      std::cerr << num_ents << " entities..." << std::endl;

      // Save place where remote handle info starts, then scan forward to ents
      for (i = 0; i < num_ents; i++) {
        UNPACK_INT(buff_ptr, j);
        if (0 > j)
          return MB_FAILURE;
        ps.resize(j);
        hs.resize(j);
        std::cerr << "Entity " << i << ", # procs = " << j << std::endl;
        UNPACK_INTS(buff_ptr, &ps[0], j);
        UNPACK_EH(buff_ptr, &hs[0], j);
        std::cerr << "   Procs: ";
        for (k = 0; k < j; k++)
          std::cerr << ps[k] << " ";
        std::cerr << std::endl;
        std::cerr << "   Handles: ";
        for (k = 0; k < j; k++)
          std::cerr << hs[k] << " ";
        std::cerr << std::endl;

        if (buff_ptr-orig_ptr > msg_length) {
          std::cerr << "End of buffer..." << std::endl;
          std::cerr.flush();
          return MB_FAILURE;
        }
      }

      while (true) {
        EntityType this_type = MBMAXTYPE;
        UNPACK_TYPE(buff_ptr, this_type);
        assert(this_type != MBENTITYSET);

        // MBMAXTYPE signifies end of entities data
        if (MBMAXTYPE == this_type)
          break;

        // Get the number of ents
        int num_ents2, verts_per_entity = 0;
        UNPACK_INT(buff_ptr, num_ents2);

        // Unpack the nodes per entity
        if (MBVERTEX != this_type && num_ents2) {
          UNPACK_INT(buff_ptr, verts_per_entity);
        }

        std::cerr << "Type: " << CN::EntityTypeName(this_type)
                  << "; num_ents = " << num_ents2;
        if (MBVERTEX != this_type)
          std::cerr << "; verts_per_ent = " << verts_per_entity;
        std::cerr << std::endl;
        if (num_ents2 < 0 || num_ents2 > msg_length) {
          std::cerr << "Wrong number of entities, returning." << std::endl;
          return MB_FAILURE;
        }

        for (int e = 0; e < num_ents2; e++) {
          // Check for existing entity, otherwise make new one
          if (MBVERTEX == this_type) {
            double coords[3];
            UNPACK_DBLS(buff_ptr, coords, 3);
            std::cerr << "xyz = " << coords[0] << ", " << coords[1] << ", "
                      << coords[2] << std::endl;
          }
          else {
            EntityHandle connect[CN::MAX_NODES_PER_ELEMENT];
            assert(verts_per_entity <= CN::MAX_NODES_PER_ELEMENT);
            UNPACK_EH(buff_ptr, connect, verts_per_entity);

            // Update connectivity to local handles
            std::cerr << "Connectivity: ";
            for (k = 0; k < verts_per_entity; k++)
              std::cerr << connect[k] << " ";
            std::cerr << std::endl;
          }

          if (buff_ptr-orig_ptr > msg_length) {
            std::cerr << "End of buffer..." << std::endl;
            std::cerr.flush();
            return MB_FAILURE;
          }
        }
      }
    }
    else if (MB_MESG_REMOTEH_SIZE == mesg_tag || MB_MESG_REMOTEH_LARGE == mesg_tag) {
      UNPACK_INT(buff_ptr, num_ents);
      std::cerr << num_ents << " entities..." << std::endl;
      if (0 > num_ents || num_ents > msg_length) {
        std::cerr << "Wrong number of entities, returning." << std::endl;
        return MB_FAILURE;
      }
      std::vector<EntityHandle> L1hloc(num_ents), L1hrem(num_ents);
      std::vector<int> L1p(num_ents);
      UNPACK_INTS(buff_ptr, &L1p[0], num_ents);
      UNPACK_EH(buff_ptr, &L1hrem[0], num_ents);
      UNPACK_EH(buff_ptr, &L1hloc[0], num_ents);
      std::cerr << num_ents << " Entity pairs; hremote/hlocal/proc: " << std::endl;
      for (int i = 0; i < num_ents; i++) {
        EntityType etype = TYPE_FROM_HANDLE(L1hloc[i]);
        std::cerr << CN::EntityTypeName(etype) << ID_FROM_HANDLE(L1hrem[i]) << ", "
                  << CN::EntityTypeName(etype) << ID_FROM_HANDLE(L1hloc[i]) << ", "
                  << L1p[i] << std::endl;
      }

      if (buff_ptr-orig_ptr > msg_length) {
        std::cerr << "End of buffer..." << std::endl;
        std::cerr.flush();
        return MB_FAILURE;
      }
    }
    else if (mesg_tag == MB_MESG_TAGS_SIZE || mesg_tag == MB_MESG_TAGS_LARGE) {
      int num_tags, dum1, data_type, tag_size;
      UNPACK_INT(buff_ptr, num_tags);
      std::cerr << "Number of tags = " << num_tags << std::endl;
      for (int i = 0; i < num_tags; i++) {
        std::cerr << "Tag " << i << ":" << std::endl;
        UNPACK_INT(buff_ptr, tag_size);
        UNPACK_INT(buff_ptr, dum1);
        UNPACK_INT(buff_ptr, data_type);
        std::cerr << "Tag size, type, data type = " << tag_size << ", " 
                  << dum1 << ", " << data_type << std::endl;
        UNPACK_INT(buff_ptr, dum1);
        std::cerr << "Default value size = " << dum1 << std::endl;
        buff_ptr += dum1;
        UNPACK_INT(buff_ptr, dum1);
        std::string name((char*)buff_ptr, dum1);
        std::cerr << "Tag name = " << name.c_str() << std::endl;
        buff_ptr += dum1;
        UNPACK_INT(buff_ptr, num_ents);
        std::cerr << "Number of ents = " << num_ents << std::endl;
        std::vector<EntityHandle> tmp_buff(num_ents);
        UNPACK_EH(buff_ptr, &tmp_buff[0], num_ents);
        int tot_length = 0;
        for (int j = 0; j < num_ents; j++) {
          EntityType etype = TYPE_FROM_HANDLE(tmp_buff[j]);
          std::cerr << CN::EntityTypeName(etype) << " " 
                    << ID_FROM_HANDLE(tmp_buff[j])
                    << ", tag = ";
          if (tag_size == MB_VARIABLE_LENGTH) {
            UNPACK_INT(buff_ptr, dum1);
            tot_length += dum1;
            std::cerr << "(variable, length = " << dum1 << ")" << std::endl;
          }
          else if (data_type == MB_TYPE_DOUBLE) {
            double dum_dbl;
            UNPACK_DBL(buff_ptr, dum_dbl);
            std::cerr << dum_dbl << std::endl;
          }
          else if (data_type == MB_TYPE_INTEGER) {
            int dum_int;
            UNPACK_INT(buff_ptr, dum_int);
            std::cerr << dum_int << std::endl;
          }
          else if (data_type == MB_TYPE_OPAQUE) {
            std::cerr << "(opaque)" << std::endl;
            buff_ptr += tag_size;
          }
          else if (data_type == MB_TYPE_HANDLE) {
            EntityHandle dum_eh;
            UNPACK_EH(buff_ptr, &dum_eh, 1);
            std::cerr <<  dum_eh << std::endl;
          }
          else if (data_type == MB_TYPE_BIT) {
            std::cerr << "(bit)" << std::endl;
            buff_ptr += tag_size;
          }
        }
        if (tag_size == MB_VARIABLE_LENGTH)
          buff_ptr += tot_length;
      }
    }
    else {
      assert(false);
      return MB_FAILURE;
    }

    std::cerr.flush();

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::list_entities(const EntityHandle *ents, int num_ents) 
  {
    if (NULL == ents && 0 == num_ents) {
      Range shared_ents;
      std::copy(sharedEnts.begin(), sharedEnts.end(), range_inserter(shared_ents));
      shared_ents.print("Shared entities:\n");
      return MB_SUCCESS;
    }
    else if (NULL == ents && 0 != num_ents) {
      return list_entities(&sharedEnts[0], sharedEnts.size());
    }

    unsigned char pstat;
    EntityHandle tmp_handles[MAX_SHARING_PROCS];
    int tmp_procs[MAX_SHARING_PROCS];
    unsigned int num_ps;
    ErrorCode result;

    for (int i = 0; i < num_ents; i++) {
      result = mbImpl->list_entities(ents + i, 1);MB_CHK_ERR(result);

      result = get_sharing_data(ents[i], tmp_procs, tmp_handles, pstat, num_ps);MB_CHK_SET_ERR(result, "Failed to get sharing data");

      std::cout << "Pstatus: ";
      if (!num_ps)
        std::cout << "local " << std::endl;
      else {
        if (pstat & PSTATUS_NOT_OWNED)
          std::cout << "NOT_OWNED; ";
        if (pstat & PSTATUS_SHARED)
          std::cout << "SHARED; ";
        if (pstat & PSTATUS_MULTISHARED)
          std::cout << "MULTISHARED; ";
        if (pstat & PSTATUS_INTERFACE)
          std::cout << "INTERFACE; ";
        if (pstat & PSTATUS_GHOST)
          std::cout << "GHOST; ";
        std::cout << std::endl;
        for (unsigned int j = 0; j < num_ps; j++) {
          std::cout << "  proc " << tmp_procs[j] << " id (handle) "
                    << mbImpl->id_from_handle(tmp_handles[j])
                    << "(" << tmp_handles[j] << ")" << std::endl;
        }
      }
      std::cout << std::endl;
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::list_entities(const Range &ents) 
  {
    for (Range::iterator rit = ents.begin(); rit != ents.end(); ++rit)
      list_entities(&(*rit), 1);

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::update_remote_data(Range &local_range,
                                             Range &remote_range,
                                             int other_proc,
                                             const unsigned char add_pstat)
  {
    Range::iterator rit, rit2;
    ErrorCode result = MB_SUCCESS;

    // For each pair of local/remote handles:
    for (rit = local_range.begin(), rit2 = remote_range.begin();
         rit != local_range.end(); ++rit, ++rit2) {
      result = update_remote_data(*rit, &other_proc, &(*rit2), 1, add_pstat);MB_CHK_ERR(result);
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::update_remote_data(const EntityHandle new_h,
                                             const int *ps,
                                             const EntityHandle *hs,
                                             const int num_ps,
                                             const unsigned char add_pstat
// The following lines left in for future debugging, at least until I trust this function; tjt, 10/4/2013
//                                           , int *new_ps,
//                                           EntityHandle *new_hs,
//                                           int &new_numps,
//                                           unsigned char &new_pstat
                                             )
  {
    // Get initial sharing data; tag_ps and tag_hs get terminated with -1 and 0
    // in this function, so no need to initialize; sharing data does not include
    // this proc if shared with only one other

    // Following variables declared here to avoid compiler errors
    int new_numps;
    unsigned char new_pstat;
    std::vector<int> new_ps(MAX_SHARING_PROCS, -1);
    std::vector<EntityHandle> new_hs(MAX_SHARING_PROCS, 0);

    new_numps = 0;
    ErrorCode result = get_sharing_data(new_h, &new_ps[0], &new_hs[0], new_pstat, new_numps);MB_CHK_SET_ERR(result, "Failed to get sharing data in update_remote_data");
    int num_exist = new_numps;

    // Add new pstat info to the flag
    new_pstat |= add_pstat;

/*
#define plist(str, lst, siz)                                          \
    std::cout << str << "(";                                          \
    for (int i = 0; i < (int)siz; i++) std::cout << lst[i] << " ";    \
    std::cout << ") ";                                                \
    
    std::cout << "update_remote_data: rank = " << rank() << ", new_h = " << new_h << std::endl;
    std::string ostr;
    plist("ps", ps, num_ps);
    plist("hs", hs, num_ps);
    print_pstatus(add_pstat, ostr);
    std::cout << ", add_pstat = " << ostr.c_str() << std::endl;
    plist("tag_ps", new_ps, new_numps);
    plist("tag_hs", new_hs, new_numps);
    assert(new_numps <= size());
    print_pstatus(new_pstat, ostr);
    std::cout << ", tag_pstat=" << ostr.c_str() << std::endl;
*/

#ifndef NDEBUG
    {
      // Check for duplicates in proc list
      std::set<unsigned int> dumprocs;
      unsigned int dp = 0;
      for (; (int) dp < num_ps && -1 != ps[dp]; dp++)
        dumprocs.insert(ps[dp]);
      assert(dp == dumprocs.size());
    }
#endif

    // If only one sharer and I'm the owner, insert myself in the list;
    // otherwise, my data is checked at the end
    if (1 == new_numps && !(new_pstat & PSTATUS_NOT_OWNED)) {
      new_hs[1] = new_hs[0];
      new_ps[1] = new_ps[0];
      new_hs[0] = new_h;
      new_ps[0] = rank();
      new_numps = 2;
    }

    // Now put passed-in data onto lists
    int idx;
    for (int i = 0; i < num_ps; i++) {
      idx = std::find(&new_ps[0], &new_ps[0] + new_numps, ps[i]) - &new_ps[0];
      if (idx < new_numps) {
        if (!new_hs[idx] && hs[i])
          // h on list is 0 and passed-in h is non-zero, replace it
          new_hs[idx] = hs[i];
        else
          assert(!hs[i] || new_hs[idx] == hs[i]);
      }
      else {
        if (new_numps + 1 == MAX_SHARING_PROCS) {
          MB_SET_ERR(MB_FAILURE, "Exceeded MAX_SHARING_PROCS for " << CN::EntityTypeName(TYPE_FROM_HANDLE(new_h))
              << ' ' << ID_FROM_HANDLE(new_h) << " in process " << rank());
        }
        new_ps[new_numps] = ps[i];
        new_hs[new_numps] = hs[i];
        new_numps++;
      }
    }

    // Add myself, if it isn't there already
    idx = std::find(&new_ps[0], &new_ps[0] + new_numps, rank()) - &new_ps[0];
    if (idx == new_numps) {
      new_ps[new_numps] = rank();
      new_hs[new_numps] = new_h;
      new_numps++;
    }
    else if (!new_hs[idx] && new_numps > 2)
      new_hs[idx] = new_h;

    // Proc list is complete; update for shared, multishared
    if (new_numps > 1) {
      if (new_numps > 2) new_pstat |= PSTATUS_MULTISHARED;
      new_pstat |= PSTATUS_SHARED;
    }

/*    
    plist("new_ps", new_ps, new_numps);
    plist("new_hs", new_hs, new_numps);
    print_pstatus(new_pstat, ostr);
    std::cout << ", new_pstat=" << ostr.c_str() << std::endl;
    std::cout << std::endl;
*/

    result = set_sharing_data(new_h, new_pstat, num_exist, new_numps, &new_ps[0], &new_hs[0]);MB_CHK_SET_ERR(result, "Failed to set sharing data in update_remote_data");

    if (new_pstat & PSTATUS_SHARED)
      sharedEnts.push_back(new_h);

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::update_remote_data_old(const EntityHandle new_h,
                                                 const int *ps,
                                                 const EntityHandle *hs,
                                                 const int num_ps,
                                                 const unsigned char add_pstat)
  {
    EntityHandle tag_hs[MAX_SHARING_PROCS];
    int tag_ps[MAX_SHARING_PROCS];
    unsigned char pstat;
    // Get initial sharing data; tag_ps and tag_hs get terminated with -1 and 0
    // in this function, so no need to initialize
    unsigned int num_exist;
    ErrorCode result = get_sharing_data(new_h, tag_ps, tag_hs, pstat, num_exist);MB_CHK_ERR(result);

#ifndef NDEBUG
    {
      // Check for duplicates in proc list
      std::set<unsigned int> dumprocs;
      unsigned int dp = 0;
      for (; (int) dp < num_ps && -1 != ps[dp]; dp++)
        dumprocs.insert(ps[dp]);
      assert(dp == dumprocs.size());
    }
#endif

    // Add any new sharing data
    bool changed = false;
    int idx;
    if (!num_exist) {
      // Just take what caller passed
      memcpy(tag_ps, ps, num_ps*sizeof(int));
      memcpy(tag_hs, hs, num_ps*sizeof(EntityHandle));
      num_exist = num_ps;
      // If it's only one, hopefully I'm not there yet...
      assert("I shouldn't be the only proc there." &&
             (1 != num_exist || ps[0] != (int)procConfig.proc_rank()));
      changed = true;
    }
    else {
      for (int i = 0; i < num_ps; i++) {
        idx = std::find(tag_ps, tag_ps + num_exist, ps[i]) - tag_ps;
        if (idx == (int) num_exist) {
          if (num_exist == MAX_SHARING_PROCS) {
            std::cerr << "Exceeded MAX_SHARING_PROCS for "
                      << CN::EntityTypeName(TYPE_FROM_HANDLE(new_h))
                      << ' ' << ID_FROM_HANDLE(new_h)
                      << " in process " << proc_config().proc_rank()
                      << std::endl;
            std::cerr.flush();
            MPI_Abort(proc_config().proc_comm(), 66);
          }

          // If there's only 1 sharing proc, and it's not me, then
          // we'll end up with 3; add me to the front
          if (!i && num_ps == 1 && num_exist == 1 &&
              ps[0] != (int)procConfig.proc_rank()) {
            int j = 1;
            // If I own this entity, put me at front, otherwise after first
            if (!(pstat & PSTATUS_NOT_OWNED)) {
              tag_ps[1] = tag_ps[0];
              tag_hs[1] = tag_hs[0];
              j = 0;
            }
            tag_ps[j] = procConfig.proc_rank();
            tag_hs[j] = new_h;
            num_exist++;
          }

          tag_ps[num_exist] = ps[i];
          tag_hs[num_exist] = hs[i];
          num_exist++;
          changed = true;
        }
        else if (0 == tag_hs[idx]) {
          tag_hs[idx] = hs[i];
          changed = true;
        }
        else if (0 != hs[i]) {
          assert(hs[i] == tag_hs[idx]);
        }
      }
    }

    // Adjust for interface layer if necessary
    if (add_pstat & PSTATUS_INTERFACE) {
      idx = std::min_element(tag_ps, tag_ps + num_exist) - tag_ps;
      if (idx) {
        int tag_proc = tag_ps[idx];
        tag_ps[idx] = tag_ps[0];
        tag_ps[0] = tag_proc;
        EntityHandle tag_h = tag_hs[idx];
        tag_hs[idx] = tag_hs[0];
        tag_hs[0] = tag_h;
        changed = true;
        if (tag_ps[0] != (int)procConfig.proc_rank())
          pstat |= PSTATUS_NOT_OWNED;
      }
    }

    if (!changed)
      return MB_SUCCESS;

    assert("interface entities should have > 1 proc" &&
           (!(add_pstat & PSTATUS_INTERFACE) || num_exist > 1));
    assert("ghost entities should have > 1 proc" &&
           (!(add_pstat & PSTATUS_GHOST) || num_exist > 1));

    // If it's multi-shared and we created the entity in this unpack,
    // local handle probably isn't in handle list yet
    if (num_exist > 2) {
      idx = std::find(tag_ps, tag_ps + num_exist, procConfig.proc_rank()) - tag_ps;
      assert(idx < (int) num_exist);
      if (!tag_hs[idx])
        tag_hs[idx] = new_h;
    }

    int tag_p;
    EntityHandle tag_h;

    // Update pstat
    pstat |= add_pstat;

    if (num_exist > 2) 
      pstat |= (PSTATUS_MULTISHARED | PSTATUS_SHARED);
    else if (num_exist > 0)
      pstat |= PSTATUS_SHARED;

//    compare_remote_data(new_h, num_ps, hs, ps, add_pstat,
//                        num_exist, tag_hs, tag_ps, pstat);

    // Reset single shared proc/handle if was shared and moving to multi-shared
    if (num_exist > 2 && !(pstat & PSTATUS_MULTISHARED) &&
        (pstat & PSTATUS_SHARED)) {
      // Must remove sharedp/h first, which really means set to default value
      tag_p = -1;
      result = mbImpl->tag_set_data(sharedp_tag(), &new_h, 1, &tag_p);MB_CHK_SET_ERR(result, "Failed to set sharedp tag data");
      tag_h = 0;
      result = mbImpl->tag_set_data(sharedh_tag(), &new_h, 1, &tag_h);MB_CHK_SET_ERR(result, "Failed to set sharedh tag data");
    }

    // Set sharing tags
    if (num_exist > 2) {
      std::fill(tag_ps + num_exist, tag_ps + MAX_SHARING_PROCS, -1);
      std::fill(tag_hs + num_exist, tag_hs + MAX_SHARING_PROCS, 0);
      result = mbImpl->tag_set_data(sharedps_tag(), &new_h, 1, tag_ps);MB_CHK_SET_ERR(result, "Failed to set sharedps tag data");
      result = mbImpl->tag_set_data(sharedhs_tag(), &new_h, 1, tag_hs);MB_CHK_SET_ERR(result, "Failed to set sharedhs tag data");

#ifndef NDEBUG
      {
        // Check for duplicates in proc list
        std::set<unsigned int> dumprocs;
        unsigned int dp = 0;
        for (; dp < num_exist && -1 != tag_ps[dp]; dp++)
          dumprocs.insert(tag_ps[dp]);
        assert(dp == dumprocs.size());
      }
#endif
    }
    else if (num_exist == 2 || num_exist == 1) {
      if (tag_ps[0] == (int) procConfig.proc_rank()) {
        assert(2 == num_exist && tag_ps[1] != (int) procConfig.proc_rank());
        tag_ps[0] = tag_ps[1];
        tag_hs[0] = tag_hs[1];
      }
      assert(tag_ps[0] != -1 && tag_hs[0] != 0);
      result = mbImpl->tag_set_data(sharedp_tag(), &new_h, 1, tag_ps);MB_CHK_SET_ERR(result, "Failed to set sharedp tag data");
      result = mbImpl->tag_set_data(sharedh_tag(), &new_h, 1, tag_hs);MB_CHK_SET_ERR(result, "Failed to set sharedh tag data");
    }

    // Now set new pstatus
    result = mbImpl->tag_set_data(pstatus_tag(), &new_h, 1, &pstat);MB_CHK_SET_ERR(result, "Failed to set pstatus tag data");

    if (pstat & PSTATUS_SHARED)
      sharedEnts.push_back(new_h);

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_sharing_data(const Range &entities,
                                           std::set<int> &procs,
                                           int operation)
  {
    // Get the union or intersection of sharing data for multiple entities
    ErrorCode result;
    int sp2[MAX_SHARING_PROCS];
    int num_ps;
    unsigned char pstat;
    std::set<int> tmp_procs;
    procs.clear();

    for (Range::const_iterator rit = entities.begin(); rit != entities.end(); ++rit) {
      // Get sharing procs
      result = get_sharing_data(*rit, sp2, NULL, pstat, num_ps);MB_CHK_SET_ERR(result, "Failed to get sharing data in get_sharing_data");
      if (!(pstat & PSTATUS_SHARED) && Interface::INTERSECT == operation) {
        procs.clear();
        return MB_SUCCESS;
      }

      if (rit == entities.begin()) {
        std::copy(sp2, sp2 + num_ps, std::inserter(procs, procs.begin()));
      }
      else {
        std::sort(sp2, sp2 + num_ps);
        tmp_procs.clear();
        if (Interface::UNION == operation)
          std::set_union(procs.begin(), procs.end(),
                         sp2, sp2 + num_ps, std::inserter(tmp_procs, tmp_procs.end()));
        else if (Interface::INTERSECT == operation)
          std::set_intersection(procs.begin(), procs.end(),
                                sp2, sp2 + num_ps, std::inserter(tmp_procs, tmp_procs.end()));
        else {
          assert("Unknown operation." && false);
          return MB_FAILURE;
        }
        procs.swap(tmp_procs);
      }
      if (Interface::INTERSECT == operation && procs.empty())
        return MB_SUCCESS;
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_sharing_data(const EntityHandle entity,
                                           int *ps,
                                           EntityHandle *hs,
                                           unsigned char &pstat,
                                           unsigned int &num_ps)
  {
    ErrorCode result = mbImpl->tag_get_data(pstatus_tag(), &entity, 1, &pstat);MB_CHK_SET_ERR(result, "Failed to get pstatus tag data");
    if (pstat & PSTATUS_MULTISHARED) {
      result = mbImpl->tag_get_data(sharedps_tag(), &entity, 1, ps);MB_CHK_SET_ERR(result, "Failed to get sharedps tag data");
      if (hs) {
        result = mbImpl->tag_get_data(sharedhs_tag(), &entity, 1, hs);MB_CHK_SET_ERR(result, "Failed to get sharedhs tag data");
      }
      num_ps = std::find(ps, ps + MAX_SHARING_PROCS, -1) - ps;
    }
    else if (pstat & PSTATUS_SHARED) {
      result = mbImpl->tag_get_data(sharedp_tag(), &entity, 1, ps);MB_CHK_SET_ERR(result, "Failed to get sharedp tag data");
      if (hs) {
        result = mbImpl->tag_get_data(sharedh_tag(), &entity, 1, hs);MB_CHK_SET_ERR(result, "Failed to get sharedh tag data");
        hs[1] = 0;
      }
      // Initialize past end of data
      ps[1] = -1;
      num_ps = 1;
    }
    else {
      ps[0] = -1;
      if (hs)
        hs[0] = 0;
      num_ps = 0;
    }

    assert(MAX_SHARING_PROCS >= num_ps);

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::find_existing_entity(const bool is_iface,
                                               const int owner_p,
                                               const EntityHandle owner_h,
                                               const int num_ps,
                                               const EntityHandle *connect,
                                               const int num_connect,
                                               const EntityType this_type,
                                               std::vector<EntityHandle> &L2hloc,
                                               std::vector<EntityHandle> &L2hrem,
                                               std::vector<unsigned int> &L2p,
                                               EntityHandle &new_h)
  {
    new_h = 0;
    if (!is_iface && num_ps > 2) {
      for (unsigned int i = 0; i < L2hrem.size(); i++) {
        if (L2hrem[i] == owner_h && owner_p == (int) L2p[i]) {
          new_h = L2hloc[i];
          return MB_SUCCESS;
        }
      }
    }

    // If we got here and it's a vertex, we don't need to look further
    if (MBVERTEX == this_type || !connect || !num_connect) return MB_SUCCESS;

    Range tmp_range;
    ErrorCode result = mbImpl->get_adjacencies(connect, num_connect,
                                               CN::Dimension(this_type), false,
                                               tmp_range);MB_CHK_SET_ERR(result, "Failed to get existing entity");
    if (!tmp_range.empty()) {
      // Found a corresponding entity - return target
      new_h = *tmp_range.begin();
    }
    else {
      new_h = 0;
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_local_handles(const Range &remote_handles,
                                            Range &local_handles,
                                            const std::vector<EntityHandle> &new_ents)
  {
    std::vector<EntityHandle> rh_vec;
    rh_vec.reserve(remote_handles.size());
    std::copy(remote_handles.begin(), remote_handles.end(), std::back_inserter(rh_vec));
    ErrorCode result = get_local_handles(&rh_vec[0], remote_handles.size(), new_ents);
    std::copy(rh_vec.begin(), rh_vec.end(), range_inserter(local_handles));
    return result;
  }

  ErrorCode ParallelComm::get_local_handles(EntityHandle *from_vec,
                                            int num_ents,
                                            const Range &new_ents)
  {
    std::vector<EntityHandle> tmp_ents;
    std::copy(new_ents.begin(), new_ents.end(), std::back_inserter(tmp_ents));
    return get_local_handles(from_vec, num_ents, tmp_ents);
  }

  ErrorCode ParallelComm::get_local_handles(EntityHandle *from_vec,
                                            int num_ents,
                                            const std::vector<EntityHandle> &new_ents)
  {
    for (int i = 0; i < num_ents; i++) {
      if (TYPE_FROM_HANDLE(from_vec[i]) == MBMAXTYPE) {
        assert(ID_FROM_HANDLE(from_vec[i]) < (int) new_ents.size());
        from_vec[i] = new_ents[ID_FROM_HANDLE(from_vec[i])];
      }
    }

    return MB_SUCCESS;
  }

  /*
  template <typename T> void
  insert_in_array(T* array, size_t array_size, size_t location, T value)
  {
    assert(location + 1 < array_size);
    for (size_t i = array_size - 1; i > location; i--)
      array[i] = array[i - 1];
    array[location] = value;
  }
  */

  ErrorCode ParallelComm::pack_range_map(Range &key_range, EntityHandle val_start,
                                         HandleMap &handle_map)
  {
    for (Range::const_pair_iterator key_it = key_range.const_pair_begin();
         key_it != key_range.const_pair_end(); ++key_it) {
      int tmp_num = (*key_it).second - (*key_it).first + 1;
      handle_map.insert((*key_it).first, val_start, tmp_num);
      val_start += tmp_num;
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::pack_sets(Range &entities,
                                    Buffer *buff,
                                    const bool store_remote_handles,
                                    const int to_proc)
  {
    // SETS:
    // . #sets
    // . for each set:
    //   - options[#sets] (unsigned int)
    //   - if (unordered) set range 
    //   - else if ordered
    //     . #ents in set
    //     . handles[#ents]
    //   - #parents
    //   - if (#parents) handles[#parents]
    //   - #children
    //   - if (#children) handles[#children]

    // Now the sets; assume any sets the application wants to pass are in the entities list
    ErrorCode result;
    Range all_sets = entities.subset_by_type(MBENTITYSET);

    int buff_size = estimate_sets_buffer_size(all_sets, store_remote_handles);
    if (buff_size < 0)
      MB_SET_ERR(MB_FAILURE, "Failed to estimate sets buffer size");
    buff->check_space(buff_size);

    // Number of sets
    PACK_INT(buff->buff_ptr, all_sets.size());

    // Options for all sets
    std::vector<unsigned int> options(all_sets.size());
    Range::iterator rit;
    std::vector<EntityHandle> members;
    int i;
    for (rit = all_sets.begin(), i = 0; rit != all_sets.end(); ++rit, i++) {
      result = mbImpl->get_meshset_options(*rit, options[i]);MB_CHK_SET_ERR(result, "Failed to get meshset options");
    }
    buff->check_space(all_sets.size()*sizeof(unsigned int));
    PACK_VOID(buff->buff_ptr, &options[0], all_sets.size()*sizeof(unsigned int));

    // Pack parallel geometry unique id
    if (!all_sets.empty()) {
      Tag uid_tag;
      int n_sets = all_sets.size();
      bool b_pack = false;
      std::vector<int> id_data(n_sets);
      result = mbImpl->tag_get_handle("PARALLEL_UNIQUE_ID", 1, MB_TYPE_INTEGER, 
                                      uid_tag, MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(result, "Failed to create parallel geometry unique id tag");

      result = mbImpl->tag_get_data(uid_tag, all_sets, &id_data[0]);
      if (MB_TAG_NOT_FOUND != result) {
        if (MB_SUCCESS != result)
          MB_SET_ERR(result, "Failed to get parallel geometry unique ids");
        for (i = 0; i < n_sets; i++) {
          if (id_data[i] != 0) {
            b_pack = true;
            break;
          }
        }
      }

      if (b_pack) { // If you find
        buff->check_space((n_sets + 1)*sizeof(int));
        PACK_INT(buff->buff_ptr, n_sets);
        PACK_INTS(buff->buff_ptr, &id_data[0], n_sets);
      }
      else {
        buff->check_space(sizeof(int));
        PACK_INT(buff->buff_ptr, 0);
      }
    }

    // Vectors/ranges
    std::vector<EntityHandle> entities_vec(entities.size());
    std::copy(entities.begin(), entities.end(), entities_vec.begin());
    for (rit = all_sets.begin(), i = 0; rit != all_sets.end(); ++rit, i++) {
      members.clear();
      result = mbImpl->get_entities_by_handle(*rit, members);MB_CHK_SET_ERR(result, "Failed to get entities in ordered set");
      result = get_remote_handles(store_remote_handles, &members[0],
                                  &members[0], members.size(),
                                  to_proc, entities_vec);MB_CHK_SET_ERR(result, "Failed in get_remote_handles");
      buff->check_space(members.size()*sizeof(EntityHandle) + sizeof(int));
      PACK_INT(buff->buff_ptr, members.size());
      PACK_EH(buff->buff_ptr, &members[0], members.size());
    }

    // Pack parent/child sets
    if (!store_remote_handles) { // Only works not store remote handles
      // Pack numbers of parents/children
      unsigned int tot_pch = 0;
      int num_pch;
      buff->check_space(2*all_sets.size()*sizeof(int));
      for (rit = all_sets.begin(), i = 0; rit != all_sets.end(); ++rit, i++) {
        // Pack parents
        result = mbImpl->num_parent_meshsets(*rit, &num_pch);MB_CHK_SET_ERR(result, "Failed to get num parents");
        PACK_INT(buff->buff_ptr, num_pch);
        tot_pch += num_pch;
        result = mbImpl->num_child_meshsets(*rit, &num_pch);MB_CHK_SET_ERR(result, "Failed to get num children");
        PACK_INT(buff->buff_ptr, num_pch);
        tot_pch += num_pch;
      }

      // Now pack actual parents/children
      members.clear();
      members.reserve(tot_pch);
      std::vector<EntityHandle> tmp_pch;
      for (rit = all_sets.begin(), i = 0; rit != all_sets.end(); ++rit, i++) {
        result = mbImpl->get_parent_meshsets(*rit, tmp_pch);MB_CHK_SET_ERR(result, "Failed to get parents");
        std::copy(tmp_pch.begin(), tmp_pch.end(), std::back_inserter(members));
        tmp_pch.clear();
        result = mbImpl->get_child_meshsets(*rit, tmp_pch);MB_CHK_SET_ERR(result, "Failed to get children");
        std::copy(tmp_pch.begin(), tmp_pch.end(), std::back_inserter(members));
        tmp_pch.clear();
      }
      assert(members.size() == tot_pch);
      if (!members.empty()) {
        result = get_remote_handles(store_remote_handles,
                                    &members[0], &members[0],
                                    members.size(), to_proc,
                                    entities_vec);MB_CHK_SET_ERR(result, "Failed to get remote handles for set parent/child sets");
#ifndef NDEBUG
        // Check that all handles are either sets or maxtype
        for (unsigned int __j = 0; __j < members.size(); __j++)
          assert((TYPE_FROM_HANDLE(members[__j]) == MBMAXTYPE &&
                  ID_FROM_HANDLE(members[__j]) < (int)entities.size()) ||
                 TYPE_FROM_HANDLE(members[__j]) == MBENTITYSET);
#endif
        buff->check_space(members.size()*sizeof(EntityHandle));
        PACK_EH(buff->buff_ptr, &members[0], members.size());
      }
    }
    else {
      buff->check_space(2*all_sets.size()*sizeof(int));
      for (rit = all_sets.begin(); rit != all_sets.end(); ++rit) {
        PACK_INT(buff->buff_ptr, 0);
        PACK_INT(buff->buff_ptr, 0);
      }
    }

    // Pack the handles
    if (store_remote_handles && !all_sets.empty()) {
      buff_size = RANGE_SIZE(all_sets);
      buff->check_space(buff_size);
      PACK_RANGE(buff->buff_ptr, all_sets);
    }

    myDebug->tprintf(4, "Done packing sets.\n");

    buff->set_stored_size();

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::unpack_sets(unsigned char *&buff_ptr,
                                      std::vector<EntityHandle> &entities,
                                      const bool store_remote_handles,
                                      const int from_proc)
  {
    // Now the sets; assume any sets the application wants to pass are in the entities list
    ErrorCode result;

    bool no_sets = (entities.empty() || (mbImpl->type_from_handle(*entities.rbegin()) == MBENTITYSET));

    Range new_sets;
    int num_sets;
    UNPACK_INT(buff_ptr, num_sets);

    if (!num_sets)
      return MB_SUCCESS;

    int i;
    Range::const_iterator rit;
    std::vector<EntityHandle> members;
    int num_ents;
    std::vector<unsigned int> options_vec(num_sets);
    // Option value
    if (num_sets)
      UNPACK_VOID(buff_ptr, &options_vec[0], num_sets*sizeof(unsigned int));

    // Unpack parallel geometry unique id
    int n_uid;
    UNPACK_INT(buff_ptr, n_uid);
    if (n_uid > 0 && n_uid != num_sets) {
      std::cerr << "The number of Parallel geometry unique ids should be same."
                << std::endl;
    }

    if (n_uid > 0) { // If parallel geometry unique id is packed
      std::vector<int> uids(n_uid);
      UNPACK_INTS(buff_ptr, &uids[0], n_uid);

      Tag uid_tag;
      result = mbImpl->tag_get_handle("PARALLEL_UNIQUE_ID", 1, MB_TYPE_INTEGER,
                                      uid_tag, MB_TAG_SPARSE | MB_TAG_CREAT);MB_CHK_SET_ERR(result, "Failed to create parallel geometry unique id tag");

      // Find existing sets
      for (i = 0; i < n_uid; i++) {
        EntityHandle set_handle;
        Range temp_sets;
        void* tag_vals[] = { &uids[i] };
        if (uids[i] > 0) {
          result = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET,
                                                        &uid_tag, tag_vals,
                                                        1, temp_sets);
        }
        if (!temp_sets.empty()) { // Existing set
          set_handle = *temp_sets.begin();
        }
        else { // Create a new set
          result = mbImpl->create_meshset(options_vec[i], set_handle);MB_CHK_SET_ERR(result, "Failed to create set in unpack");
          result = mbImpl->tag_set_data(uid_tag, &set_handle, 1, &uids[i]);MB_CHK_SET_ERR(result, "Failed to set parallel geometry unique ids");
        }
        new_sets.insert(set_handle);
      }
    }
    else {
      // Create sets
      for (i = 0; i < num_sets; i++) {
        EntityHandle set_handle;
        result = mbImpl->create_meshset(options_vec[i], set_handle);MB_CHK_SET_ERR(result, "Failed to create set in unpack");

        // Make sure new sets handles are monotonically increasing
        assert(set_handle > *new_sets.rbegin());
        new_sets.insert(set_handle);
      }
    }

    std::copy(new_sets.begin(), new_sets.end(), std::back_inserter(entities));
    // Only need to sort if we came in with no sets on the end
    if (!no_sets)
      std::sort(entities.begin(), entities.end());

    for (rit = new_sets.begin(), i = 0; rit != new_sets.end(); ++rit, i++) {
      // Unpack entities as vector, with length
      UNPACK_INT(buff_ptr, num_ents);
      members.resize(num_ents);
      if (num_ents)
        UNPACK_EH(buff_ptr, &members[0], num_ents);
      result = get_local_handles(&members[0], num_ents, entities);MB_CHK_SET_ERR(result, "Failed to get local handles for ordered set contents");
      result = mbImpl->add_entities(*rit, &members[0], num_ents);MB_CHK_SET_ERR(result, "Failed to add ents to ordered set in unpack");
    }

    std::vector<int> num_pch(2*new_sets.size());
    std::vector<int>::iterator vit;
    int tot_pch = 0;
    for (vit = num_pch.begin(); vit != num_pch.end(); ++vit) {
      UNPACK_INT(buff_ptr, *vit);
      tot_pch += *vit;
    }

    members.resize(tot_pch);
    UNPACK_EH(buff_ptr, &members[0], tot_pch);
    result = get_local_handles(&members[0], tot_pch, entities);MB_CHK_SET_ERR(result, "Failed to get local handle for parent/child sets");

    int num = 0;
    EntityHandle *mem_ptr = &members[0];
    for (rit = new_sets.begin(); rit != new_sets.end(); ++rit) {
      // Unpack parents/children
      int num_par = num_pch[num++], num_child = num_pch[num++];
      if (num_par + num_child) {
        for (i = 0; i < num_par; i++) {
          assert(0 != mem_ptr[i]);
          result = mbImpl->add_parent_meshset(*rit, mem_ptr[i]);MB_CHK_SET_ERR(result, "Failed to add parent to set in unpack");
        }
        mem_ptr += num_par;
        for (i = 0; i < num_child; i++) {
          assert(0 != mem_ptr[i]);
          result = mbImpl->add_child_meshset(*rit, mem_ptr[i]);MB_CHK_SET_ERR(result, "Failed to add child to set in unpack");
        }
        mem_ptr += num_child;
      }
    }

    // Unpack source handles
    Range dum_range;
    if (store_remote_handles && !new_sets.empty()) {
      UNPACK_RANGE(buff_ptr, dum_range);
      result = update_remote_data(new_sets, dum_range, from_proc, 0);MB_CHK_SET_ERR(result, "Failed to set sharing data for sets");
    }

    myDebug->tprintf(4, "Done unpacking sets.");

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::pack_adjacencies(Range& /*entities*/,
                                           Range::const_iterator& /*start_rit*/,
                                           Range& /*whole_range*/,
                                           unsigned char*& /*buff_ptr*/,
                                           int& /*count*/,
                                           const bool /*just_count*/,
                                           const bool /*store_handles*/,
                                           const int /*to_proc*/)
  {
    return MB_FAILURE;
  }

  ErrorCode ParallelComm::unpack_adjacencies(unsigned char*& /*buff_ptr*/,
                                             Range& /*entities*/,
                                             const bool /*store_handles*/,
                                             const int /*from_proc*/)
  {
    return MB_FAILURE;
  }

  ErrorCode ParallelComm::pack_tags(Range &entities,
                                    const std::vector<Tag> &src_tags,
                                    const std::vector<Tag> &dst_tags,
                                    const std::vector<Range> &tag_ranges,
                                    Buffer *buff,
                                    const bool store_remote_handles,
                                    const int to_proc)
  {
    ErrorCode result;
    std::vector<Tag>::const_iterator tag_it, dst_it;
    std::vector<Range>::const_iterator rit;
    int count = 0;

    for (tag_it = src_tags.begin(), rit = tag_ranges.begin();
         tag_it != src_tags.end(); ++tag_it, ++rit) {
      result = packed_tag_size(*tag_it, *rit, count);
      if (MB_SUCCESS != result)
        return result;
    }

    // Number of tags
    count += sizeof(int);

    buff->check_space(count);

    PACK_INT(buff->buff_ptr, src_tags.size());

    std::vector<EntityHandle> entities_vec(entities.size());
    std::copy(entities.begin(), entities.end(), entities_vec.begin());

    for (tag_it = src_tags.begin(), dst_it = dst_tags.begin(), rit = tag_ranges.begin(); 
         tag_it != src_tags.end(); ++tag_it, ++dst_it, ++rit) {
      result = pack_tag(*tag_it, *dst_it, *rit, entities_vec, buff,
                        store_remote_handles, to_proc);
      if (MB_SUCCESS != result)
        return result;
    }

    myDebug->tprintf(4, "Done packing tags.");

    buff->set_stored_size();

    return MB_SUCCESS;
  }
         
  ErrorCode ParallelComm::packed_tag_size(Tag tag,
                                          const Range &tagged_entities,
                                          int &count)
  {
    // For dense tags, compute size assuming all entities have that tag
    // For sparse tags, get number of entities w/ that tag to compute size

    std::vector<int> var_len_sizes;
    std::vector<const void*> var_len_values;

    // Default value
    count += sizeof(int);
    if (NULL != tag->get_default_value()) 
      count += tag->get_default_value_size();

    // Size, type, data type
    count += 3*sizeof(int);

    // Name
    count += sizeof(int);
    count += tag->get_name().size();

    // Range of tag
    count += sizeof(int) + tagged_entities.size() * sizeof(EntityHandle);

    if (tag->get_size() == MB_VARIABLE_LENGTH) {
      const int num_ent = tagged_entities.size();
      // Send a tag size for each entity
      count += num_ent * sizeof(int);
      // Send tag data for each entity
      var_len_sizes.resize(num_ent);
      var_len_values.resize(num_ent);
      ErrorCode result = tag->get_data(sequenceManager,
                                       errorHandler,
                                       tagged_entities,
                                       &var_len_values[0],
                                       &var_len_sizes[0]);MB_CHK_SET_ERR(result, "Failed to get lenghts of variable-length tag values");
      count += std::accumulate(var_len_sizes.begin(), var_len_sizes.end(), 0);
    }
    else {
      // Tag data values for range or vector
      count += tagged_entities.size() * tag->get_size();
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::pack_tag(Tag src_tag,
                                   Tag dst_tag,
                                   const Range &tagged_entities,
                                   const std::vector<EntityHandle> &whole_vec,
                                   Buffer *buff,
                                   const bool store_remote_handles,
                                   const int to_proc)
  {
    ErrorCode result;
    std::vector<int> var_len_sizes;
    std::vector<const void*> var_len_values;

    if (src_tag != dst_tag) {
      if (dst_tag->get_size() != src_tag->get_size())
        return MB_TYPE_OUT_OF_RANGE;
      if (dst_tag->get_data_type() != src_tag->get_data_type() && 
          dst_tag->get_data_type() != MB_TYPE_OPAQUE &&
          src_tag->get_data_type() != MB_TYPE_OPAQUE)
        return MB_TYPE_OUT_OF_RANGE;
    }

    // Size, type, data type
    buff->check_space(3*sizeof(int));
    PACK_INT(buff->buff_ptr, src_tag->get_size());
    TagType this_type;
    result = mbImpl->tag_get_type(dst_tag, this_type);
    PACK_INT(buff->buff_ptr, (int)this_type);
    DataType data_type = src_tag->get_data_type();
    PACK_INT(buff->buff_ptr, (int)data_type);
    int type_size = TagInfo::size_from_data_type(data_type);

    // Default value
    if (NULL == src_tag->get_default_value()) {
      buff->check_space(sizeof(int));
      PACK_INT(buff->buff_ptr, 0);
    }
    else {
      buff->check_space(src_tag->get_default_value_size());
      PACK_BYTES(buff->buff_ptr, src_tag->get_default_value(), src_tag->get_default_value_size());
    }

    // Name
    buff->check_space(src_tag->get_name().size());
    PACK_BYTES(buff->buff_ptr, dst_tag->get_name().c_str(), dst_tag->get_name().size());

    myDebug->tprintf(4, "Packing tag \"%s\"", src_tag->get_name().c_str());
    if (src_tag != dst_tag)
      myDebug->tprintf(4, " (as tag \"%s\")", dst_tag->get_name().c_str());
    myDebug->tprintf(4, "\n");

    // Pack entities
    buff->check_space(tagged_entities.size()*sizeof(EntityHandle) + sizeof(int));
    PACK_INT(buff->buff_ptr, tagged_entities.size());
    std::vector<EntityHandle> dum_tagged_entities(tagged_entities.size());
    result = get_remote_handles(store_remote_handles,
                                tagged_entities, &dum_tagged_entities[0], to_proc,
                                whole_vec);
    if (MB_SUCCESS != result) {
      if (myDebug->get_verbosity() == 3) {
        std::cerr << "Failed to get remote handles for tagged entities:" << std::endl;
        tagged_entities.print("  ");
      }
      MB_SET_ERR(result, "Failed to get remote handles for tagged entities");
    }

    PACK_EH(buff->buff_ptr, &dum_tagged_entities[0], dum_tagged_entities.size());

    const size_t num_ent = tagged_entities.size();
    if (src_tag->get_size() == MB_VARIABLE_LENGTH) {
      var_len_sizes.resize(num_ent, 0);
      var_len_values.resize(num_ent, 0);
      result = mbImpl->tag_get_by_ptr(src_tag, tagged_entities, &var_len_values[0], 
                                      &var_len_sizes[0]);MB_CHK_SET_ERR(result, "Failed to get variable-length tag data in pack_tags");
      buff->check_space(num_ent * sizeof(int));
      PACK_INTS(buff->buff_ptr, &var_len_sizes[0], num_ent);
      for (unsigned int i = 0; i < num_ent; i++) {
        buff->check_space(var_len_sizes[i]);
        PACK_VOID(buff->buff_ptr, var_len_values[i], type_size*var_len_sizes[i]);
      }
    }
    else {
      buff->check_space(num_ent * src_tag->get_size());
      // Should be OK to read directly into buffer, since tags are untyped and
      // handled by memcpy
      result = mbImpl->tag_get_data(src_tag, tagged_entities, buff->buff_ptr);MB_CHK_SET_ERR(result, "Failed to get tag data in pack_tags");
      buff->buff_ptr += num_ent * src_tag->get_size();
      PC(num_ent*src_tag->get_size(), " void");
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_tag_send_list(const Range& whole_range,
                                            std::vector<Tag>& all_tags,
                                            std::vector<Range>& tag_ranges)
  {
    std::vector<Tag> tmp_tags;
    ErrorCode result = mbImpl->tag_get_tags(tmp_tags);MB_CHK_SET_ERR(result, "Failed to get tags in pack_tags");

    std::vector<Tag>::iterator tag_it;
    for (tag_it = tmp_tags.begin(); tag_it != tmp_tags.end(); ++tag_it) {
      std::string tag_name;
      result = mbImpl->tag_get_name(*tag_it, tag_name);
      if (tag_name.c_str()[0] == '_' && tag_name.c_str()[1] == '_')
        continue;

      Range tmp_range;
      result = (*tag_it)->get_tagged_entities(sequenceManager, tmp_range);MB_CHK_SET_ERR(result, "Failed to get entities for tag in pack_tags");
      tmp_range = intersect(tmp_range, whole_range);

      if (tmp_range.empty())
        continue;

      // OK, we'll be sending this tag
      all_tags.push_back(*tag_it);
      tag_ranges.push_back(Range());
      tag_ranges.back().swap(tmp_range);
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::unpack_tags(unsigned char *&buff_ptr,
                                      std::vector<EntityHandle> &entities,
                                      const bool /*store_remote_handles*/,
                                      const int /*from_proc*/,
                                      const MPI_Op * const mpi_op)
  {
    // Tags
    // Get all the tags
    // For dense tags, compute size assuming all entities have that tag
    // For sparse tags, get number of entities w/ that tag to compute size

    ErrorCode result;

    int num_tags;
    UNPACK_INT(buff_ptr, num_tags);
    std::vector<const void*> var_len_vals;
    std::vector<unsigned char*> dum_vals;
    std::vector<EntityHandle> dum_ehvals;

    for (int i = 0; i < num_tags; i++) {
      // Tag handle
      Tag tag_handle;

      // Size, data type
      int tag_size, tag_data_type, tag_type;
      UNPACK_INT(buff_ptr, tag_size);
      UNPACK_INT(buff_ptr, tag_type);
      UNPACK_INT(buff_ptr, tag_data_type);

      // Default value
      int def_val_size;
      UNPACK_INT(buff_ptr, def_val_size);
      void *def_val_ptr = NULL;
      if (def_val_size) {
        def_val_ptr = buff_ptr;
        buff_ptr += def_val_size;
        UPC(tag_size, " void");
      }

      // Name
      int name_len;
      UNPACK_INT(buff_ptr, name_len);
      std::string tag_name(reinterpret_cast<char*>(buff_ptr), name_len);
      buff_ptr += name_len;
      UPC(64, " chars");

      myDebug->tprintf(4, "Unpacking tag %s\n", tag_name.c_str());

      // Create the tag
      if (tag_size == MB_VARIABLE_LENGTH) 
        result = mbImpl->tag_get_handle(tag_name.c_str(), def_val_size, (DataType)tag_data_type,
                                        tag_handle, MB_TAG_VARLEN | MB_TAG_CREAT | MB_TAG_BYTES | tag_type,
                                        def_val_ptr);
      else
        result = mbImpl->tag_get_handle(tag_name.c_str(), tag_size, (DataType) tag_data_type,
                                        tag_handle, MB_TAG_CREAT | MB_TAG_BYTES | tag_type,
                                        def_val_ptr);
      if (MB_SUCCESS != result) return result;

      // Get handles and convert to local handles
      int num_ents;
      UNPACK_INT(buff_ptr, num_ents);
      std::vector<EntityHandle> dum_ents(num_ents);
      UNPACK_EH(buff_ptr, &dum_ents[0], num_ents);

      // In this case handles are indices into new entity range; need to convert
      // to local handles
      result = get_local_handles(&dum_ents[0], num_ents, entities);MB_CHK_SET_ERR(result, "Unable to convert to local handles");

      // If it's a handle type, also convert tag vals in-place in buffer
      if (MB_TYPE_HANDLE == tag_type) {
        dum_ehvals.resize(num_ents);
        UNPACK_EH(buff_ptr, &dum_ehvals[0], num_ents);
        result = get_local_handles(&dum_ehvals[0], num_ents, entities);MB_CHK_SET_ERR(result, "Failed to get local handles for tag vals");
      }

      DataType data_type;
      mbImpl->tag_get_data_type(tag_handle, data_type);
      int type_size = TagInfo::size_from_data_type(data_type);

      if (!dum_ents.empty()) {
        if (tag_size == MB_VARIABLE_LENGTH) {
          // Be careful of alignment here. If the integers are aligned
          // in the buffer, we can use them directly. Otherwise we must
          // copy them.
          std::vector<int> var_lengths(num_ents);
          UNPACK_INTS(buff_ptr, &var_lengths[0], num_ents);
          UPC(sizeof(int) * num_ents, " void");

          // Get pointers into buffer for each tag value
          var_len_vals.resize(num_ents);
          for (std::vector<EntityHandle>::size_type j = 0; 
               j < (std::vector<EntityHandle>::size_type) num_ents; j++) {
            var_len_vals[j] = buff_ptr;
            buff_ptr += var_lengths[j]*type_size;
            UPC(var_lengths[j], " void");
          }
          result = mbImpl->tag_set_by_ptr(tag_handle, &dum_ents[0], num_ents,
                                          &var_len_vals[0], &var_lengths[0]);MB_CHK_SET_ERR(result, "Failed to set tag data when unpacking variable-length tag");
        }
        else {
              // Get existing values of dst tag
            dum_vals.resize(tag_size*num_ents);
            if (mpi_op) {
              int tag_length;
              result = mbImpl->tag_get_length(tag_handle, tag_length);MB_CHK_SET_ERR(result, "Failed to get tag length");
              result = mbImpl->tag_get_data(tag_handle, &dum_ents[0], num_ents, &dum_vals[0]);MB_CHK_SET_ERR(result, "Failed to get existing value of dst tag on entities");
              result = reduce_void(tag_data_type, *mpi_op, tag_length*num_ents, &dum_vals[0], buff_ptr);MB_CHK_SET_ERR(result, "Failed to perform mpi op on dst tags");
            }
          result = mbImpl->tag_set_data(tag_handle, &dum_ents[0],
                                        num_ents, buff_ptr);MB_CHK_SET_ERR(result, "Failed to set range-based tag data when unpacking tag");
          buff_ptr += num_ents * tag_size;
          UPC(num_ents * tag_size, " void");
        }
      }
    }

    myDebug->tprintf(4, "Done unpacking tags.\n");

    return MB_SUCCESS;
  }

  template<class T> T LAND(const T &arg1, const T &arg2) {return arg1 && arg2;}
  template<class T> T LOR(const T& arg1, const T& arg2) {return arg1 || arg2;}
  template<class T> T LXOR(const T& arg1, const T& arg2) {return ((arg1 && !arg2) || (!arg1 && arg2));}
  template<class T> T MAX(const T& arg1, const T& arg2) {return (arg1 > arg2 ? arg1 : arg2);}
  template<class T> T MIN(const T& arg1, const T& arg2) {return (arg1 < arg2 ? arg1 : arg2);}
  template<class T> T ADD(const T &arg1, const T &arg2) {return arg1 + arg2;}
  template<class T> T MULT(const T &arg1, const T &arg2) {return arg1 * arg2;}

  template <class T>
  ErrorCode ParallelComm::reduce(const MPI_Op mpi_op, int num_ents, void *old_vals, void *new_vals)
  {
    T *old_tmp = reinterpret_cast<T*>(old_vals);
    T *new_tmp = reinterpret_cast<T*>(new_vals);

    if (mpi_op == MPI_SUM)
      std::transform(old_tmp, old_tmp + num_ents, new_tmp, new_tmp, ADD<T>);
    else if (mpi_op == MPI_PROD)
      std::transform(old_tmp, old_tmp + num_ents, new_tmp, new_tmp, MULT<T>);
    else if (mpi_op == MPI_MAX)
      std::transform(old_tmp, old_tmp + num_ents, new_tmp, new_tmp, MAX<T>);
    else if (mpi_op == MPI_MIN)
      std::transform(old_tmp, old_tmp + num_ents, new_tmp, new_tmp, MIN<T>);
    else if (mpi_op == MPI_LAND)
      std::transform(old_tmp, old_tmp + num_ents, new_tmp, new_tmp, LAND<T>);
    else if (mpi_op == MPI_LOR)
      std::transform(old_tmp, old_tmp + num_ents, new_tmp, new_tmp, LOR<T>);
    else if (mpi_op == MPI_LXOR)
      std::transform(old_tmp, old_tmp + num_ents, new_tmp, new_tmp, LXOR<T>);
    else if (mpi_op == MPI_BAND || mpi_op == MPI_BOR || mpi_op == MPI_BXOR) {
      std::cerr << "Bitwise operations not allowed in tag reductions." << std::endl;
      return MB_FAILURE;
    }
    else if (mpi_op != MPI_OP_NULL) {
      std::cerr << "Unknown MPI operation type." << std::endl;
      return MB_TYPE_OUT_OF_RANGE;
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::reduce_void(int tag_data_type, const MPI_Op mpi_op, int num_ents, void *old_vals, void *new_vals)
  {
    ErrorCode result;
    switch (tag_data_type) {
      case MB_TYPE_INTEGER:
          result = reduce<int>(mpi_op, num_ents, old_vals, new_vals);
          break;
      case MB_TYPE_DOUBLE:
          result = reduce<double>(mpi_op, num_ents, old_vals, new_vals);
          break;
      case MB_TYPE_BIT:
          result = reduce<unsigned char>(mpi_op, num_ents, old_vals, new_vals);
          break;
      default:
          result = MB_SUCCESS;
          break;
    }

    return result;
  }

  ErrorCode ParallelComm::resolve_shared_ents(EntityHandle this_set,
                                              int resolve_dim,
                                              int shared_dim,
                                              const Tag* id_tag)
  {
    ErrorCode result;
    Range proc_ents;

    // Check for structured mesh, and do it differently if it is
    ScdInterface *scdi;
    result = mbImpl->query_interface(scdi);
    if (scdi) {
      result = scdi->tag_shared_vertices(this, this_set);
      if (MB_SUCCESS == result) {
        myDebug->tprintf(1, "Total number of shared entities = %lu.\n", (unsigned long)sharedEnts.size());
        return result;
      }
    }

    if (0 == this_set) {
        // Get the entities in the partition sets
      for (Range::iterator rit = partitionSets.begin(); rit != partitionSets.end(); ++rit) {
        Range tmp_ents;
        result = mbImpl->get_entities_by_handle(*rit, tmp_ents, true);
        if (MB_SUCCESS != result) return result;
        proc_ents.merge(tmp_ents);
      }
    }
    else {
      result = mbImpl->get_entities_by_handle(this_set, proc_ents, true);
      if (MB_SUCCESS != result) return result;
    }

    // Resolve dim is maximal dim of entities in proc_ents
    if (-1 == resolve_dim) {
      if (proc_ents.empty()) 
        return MB_ENTITY_NOT_FOUND;

      resolve_dim = mbImpl->dimension_from_handle(*proc_ents.rbegin());
    }

    // proc_ents should all be of same dimension
    if (resolve_dim > shared_dim &&
        mbImpl->dimension_from_handle(*proc_ents.rbegin()) !=
        mbImpl->dimension_from_handle(*proc_ents.begin())) {
      Range::iterator lower = proc_ents.lower_bound(CN::TypeDimensionMap[0].first),
        upper = proc_ents.upper_bound(CN::TypeDimensionMap[resolve_dim - 1].second);
      proc_ents.erase(lower, upper);
    }

    // Must call even if we don't have any entities, to make sure
    // collective comm'n works
    return resolve_shared_ents(this_set, proc_ents, resolve_dim, shared_dim, NULL, id_tag);
  }

  ErrorCode ParallelComm::resolve_shared_ents(EntityHandle this_set,
                                              Range &proc_ents,
                                              int resolve_dim,
                                              int shared_dim,
                                              Range *skin_ents,
                                              const Tag* id_tag)
  {
#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      define_mpe();
      MPE_Log_event(RESOLVE_START, procConfig.proc_rank(), "Entering resolve_shared_ents.");
    }
#endif

    ErrorCode result;
    myDebug->tprintf(1, "Resolving shared entities.\n");

    if (resolve_dim < shared_dim) {
      MB_SET_ERR(MB_FAILURE, "MOAB does not support vertex-based partitions, only element-based ones");
    }

    if (-1 == shared_dim) {
      if (!proc_ents.empty())
        shared_dim = mbImpl->dimension_from_handle(*proc_ents.begin()) - 1;
      else if (resolve_dim == 3)
        shared_dim = 2;
    }

    if (shared_dim < 0 || resolve_dim < 0) {
      MB_SET_ERR(MB_FAILURE, "Unable to guess shared_dim or resolve_dim");
    }

    // Get the skin entities by dimension
    Range tmp_skin_ents[4];

    // Get the entities to be skinned
    // Find the skin
    int skin_dim = resolve_dim - 1;
    if (!skin_ents) {
      skin_ents = tmp_skin_ents;
      skin_ents[resolve_dim] = proc_ents;
      Skinner skinner(mbImpl);
      result = skinner.find_skin(this_set, skin_ents[skin_dim + 1], false, skin_ents[skin_dim],
                                 NULL, true, true, true);MB_CHK_SET_ERR(result, "Failed to find skin");
      myDebug->tprintf(1, "Found skin, now resolving.\n");

      // Get entities adjacent to skin ents from shared_dim down to zero
      for (int this_dim = skin_dim - 1; this_dim >= 0; this_dim--) {
        result = mbImpl->get_adjacencies(skin_ents[skin_dim], this_dim,
                                         true, skin_ents[this_dim],
                                         Interface::UNION);MB_CHK_SET_ERR(result, "Failed to get skin adjacencies");

        if (this_set && skin_dim==2 && this_dim==1)
          {
            result = mbImpl->add_entities(this_set, skin_ents[this_dim]);MB_CHK_ERR(result);
          }
      }
    }
    else if (skin_ents[resolve_dim].empty())
      skin_ents[resolve_dim] = proc_ents;

    // Global id tag
    Tag gid_tag; 
    if (id_tag)
      gid_tag = *id_tag;
    else {
      bool tag_created = false;
      int def_val = -1;
      result = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                      gid_tag, MB_TAG_DENSE | MB_TAG_CREAT,
                                      &def_val, &tag_created);
      if (MB_ALREADY_ALLOCATED != result && MB_SUCCESS != result) {
        MB_SET_ERR(result, "Failed to create/get gid tag handle");
      }
      else if (tag_created) {
        // Just created it, so we need global ids
        result = assign_global_ids(this_set, skin_dim + 1, true, true, true);MB_CHK_SET_ERR(result, "Failed to assign global ids");
      }
    }

    DataType tag_type;
    result = mbImpl->tag_get_data_type(gid_tag, tag_type);MB_CHK_SET_ERR(result, "Failed to get tag data type");
    int bytes_per_tag;
    result = mbImpl->tag_get_bytes(gid_tag, bytes_per_tag);MB_CHK_SET_ERR(result, "Failed to get number of bytes per tag");
    // On 64 bits, long and int are different
    // On 32 bits, they are not; if size of long is 8, it is a 64 bit machine (really?)

    // Get gids for skin ents in a vector, to pass to gs
    std::vector<long> lgid_data(skin_ents[0].size());
    // Size is either long or int
    // On 64 bit is 8 or 4
    if (sizeof(long) == bytes_per_tag && ((MB_TYPE_HANDLE == tag_type) || (MB_TYPE_OPAQUE == tag_type))) { // It is a special id tag
      result = mbImpl->tag_get_data(gid_tag, skin_ents[0], &lgid_data[0]);MB_CHK_SET_ERR(result, "Couldn't get gid tag for skin vertices");
    }
    else if (4 == bytes_per_tag) { // Must be GLOBAL_ID tag or 32 bits ...
      std::vector<int> gid_data(lgid_data.size());
      result = mbImpl->tag_get_data(gid_tag, skin_ents[0], &gid_data[0]);MB_CHK_SET_ERR(result, "Failed to get gid tag for skin vertices");
      std::copy(gid_data.begin(), gid_data.end(), lgid_data.begin());
    }
    else {
      // Not supported flag
      MB_SET_ERR(MB_FAILURE, "Unsupported id tag");
    }

    // Put handles in vector for passing to gs setup
    std::vector<Ulong> handle_vec; // Assumes that we can do conversion from Ulong to EntityHandle
    std::copy(skin_ents[0].begin(), skin_ents[0].end(),
              std::back_inserter(handle_vec));

#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(SHAREDV_START, procConfig.proc_rank(), "Creating crystal router.");
    }
#endif

    // Get a crystal router
    gs_data::crystal_data *cd = procConfig.crystal_router();

    /*  
    // Get total number of entities; will overshoot highest global id, but
    // that's OK
    int num_total[2] = {0, 0}, num_local[2] = {0, 0};
    result = mbImpl->get_number_entities_by_dimension(this_set, 0, num_local);
    if (MB_SUCCESS != result)return result;
    int failure = MPI_Allreduce(num_local, num_total, 1,
    MPI_INTEGER, MPI_SUM, procConfig.proc_comm());
    if (failure) {
      MB_SET_ERR(MB_FAILURE, "Allreduce for total number of shared ents failed");
    }
    */
    // Call gather-scatter to get shared ids & procs
    gs_data *gsd = new gs_data();
   // assert(sizeof(ulong_) == sizeof(EntityHandle));
    result = gsd->initialize(skin_ents[0].size(), &lgid_data[0],
                              &handle_vec[0], 2, 1, 1, cd);MB_CHK_SET_ERR(result, "Failed to create gs data");

    // Get shared proc tags
    Tag shp_tag, shps_tag, shh_tag, shhs_tag, pstat_tag;
    result = get_shared_proc_tags(shp_tag, shps_tag,
                                  shh_tag, shhs_tag, pstat_tag);MB_CHK_SET_ERR(result, "Failed to get shared proc tags");

    // Load shared verts into a tuple, then sort by index
    TupleList shared_verts;
    shared_verts.initialize(2, 0, 1, 0, 
                            skin_ents[0].size()*(MAX_SHARING_PROCS + 1));
    shared_verts.enableWriteAccess();

    unsigned int i = 0, j = 0;
    for (unsigned int p = 0; p < gsd->nlinfo->_np; p++)
      for (unsigned int np = 0; np < gsd->nlinfo->_nshared[p]; np++) {
        shared_verts.vi_wr[i++] = gsd->nlinfo->_sh_ind[j];
        shared_verts.vi_wr[i++] = gsd->nlinfo->_target[p];
        shared_verts.vul_wr[j] = gsd->nlinfo->_ulabels[j];
        j++;
        shared_verts.inc_n();
      }

    myDebug->tprintf(3, " shared verts size %d \n", (int)shared_verts.get_n());


    int max_size = skin_ents[0].size()*(MAX_SHARING_PROCS + 1);
    moab::TupleList::buffer sort_buffer;
    sort_buffer.buffer_init(max_size);
    shared_verts.sort(0, &sort_buffer);
    sort_buffer.reset();

    // Set sharing procs and handles tags on skin ents
    int maxp = -1;
    std::vector<int> sharing_procs(MAX_SHARING_PROCS);
    std::fill(sharing_procs.begin(), sharing_procs.end(), maxp);
    j = 0;
    i = 0;

    // Get ents shared by 1 or n procs
    std::map<std::vector<int>, std::vector<EntityHandle> > proc_nvecs;
    Range proc_verts;
    result = mbImpl->get_adjacencies(proc_ents, 0, false, proc_verts,
                                     Interface::UNION);MB_CHK_SET_ERR(result, "Failed to get proc_verts");

    myDebug->print( 3, " resolve shared ents:  proc verts ", proc_verts );
    result = tag_shared_verts(shared_verts, skin_ents,
                              proc_nvecs, proc_verts);MB_CHK_SET_ERR(result, "Failed to tag shared verts");

#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(SHAREDV_END, procConfig.proc_rank(), "Finished tag_shared_verts.");
    }
#endif

    // Get entities shared by 1 or n procs
    result = get_proc_nvecs(resolve_dim, shared_dim, skin_ents, proc_nvecs);MB_CHK_SET_ERR(result, "Failed to tag shared entities");


    shared_verts.reset();

    if (myDebug->get_verbosity() > 0) {
      for (std::map<std::vector<int>, std::vector<EntityHandle> >::const_iterator mit = proc_nvecs.begin();
           mit != proc_nvecs.end(); ++mit) {
        myDebug->tprintf(1, "Iface: ");
        for (std::vector<int>::const_iterator vit = (mit->first).begin();
             vit != (mit->first).end(); ++vit) myDebug->printf(1, " %d", *vit);
        myDebug->print(1, "\n");
      }
    }

    // Create the sets for each interface; store them as tags on
    // the interface instance
    Range iface_sets;
    result = create_interface_sets(proc_nvecs);MB_CHK_SET_ERR(result, "Failed to create interface sets");

    // Establish comm procs and buffers for them
    std::set<unsigned int> procs;
    result = get_interface_procs(procs, true);MB_CHK_SET_ERR(result, "Failed to get interface procs");

#ifndef NDEBUG
    result = check_all_shared_handles(true);MB_CHK_SET_ERR(result, "Shared handle check failed after interface vertex exchange");
#endif  

    // Resolve shared entity remote handles; implemented in ghost cell exchange
    // code because it's so similar
    result = exchange_ghost_cells(-1, -1, 0, 0, true, true);MB_CHK_SET_ERR(result, "Failed to resolve shared entity remote handles");

    // Now build parent/child links for interface sets
    result = create_iface_pc_links();MB_CHK_SET_ERR(result, "Failed to create interface parent/child links");

    gsd->reset();
    delete gsd;

#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(RESOLVE_END, procConfig.proc_rank(), "Exiting resolve_shared_ents.");
    }
#endif

    //std::ostringstream ent_str;
    //ent_str << "mesh." << procConfig.proc_rank() << ".h5m";
    //mbImpl->write_mesh(ent_str.str().c_str());

    // Done
    return result;
  }

  void ParallelComm::define_mpe()
  {
#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      // Define mpe states used for logging
      int success;
      MPE_Log_get_state_eventIDs(&IFACE_START, &IFACE_END);
      MPE_Log_get_state_eventIDs(&GHOST_START, &GHOST_END);
      MPE_Log_get_state_eventIDs(&SHAREDV_START, &SHAREDV_END);
      MPE_Log_get_state_eventIDs(&RESOLVE_START, &RESOLVE_END);
      MPE_Log_get_state_eventIDs(&ENTITIES_START, &ENTITIES_END);
      MPE_Log_get_state_eventIDs(&RHANDLES_START, &RHANDLES_END);
      MPE_Log_get_state_eventIDs(&OWNED_START, &OWNED_END);
      success = MPE_Describe_state(IFACE_START, IFACE_END, "Resolve interface ents", "green");
      assert(MPE_LOG_OK == success);
      success = MPE_Describe_state(GHOST_START, GHOST_END, "Exchange ghost ents", "red");
      assert(MPE_LOG_OK == success);
      success = MPE_Describe_state(SHAREDV_START, SHAREDV_END, "Resolve interface vertices", "blue");
      assert(MPE_LOG_OK == success);
      success = MPE_Describe_state(RESOLVE_START, RESOLVE_END, "Resolve shared ents", "purple");
      assert(MPE_LOG_OK == success);
      success = MPE_Describe_state(ENTITIES_START, ENTITIES_END, "Exchange shared ents", "yellow");
      assert(MPE_LOG_OK == success);
      success = MPE_Describe_state(RHANDLES_START, RHANDLES_END, "Remote handles", "cyan");
      assert(MPE_LOG_OK == success);
      success = MPE_Describe_state(OWNED_START, OWNED_END, "Exchange owned ents", "black");
      assert(MPE_LOG_OK == success);
    }
#endif
  }

  ErrorCode ParallelComm::resolve_shared_ents(ParallelComm **pc, 
                                              const unsigned int np, 
                                              EntityHandle this_set,
                                              const int part_dim)
  {
    std::vector<Range> verts(np);
    int tot_verts = 0;
    unsigned int p, i, j, v;
    ErrorCode rval;
    for (p = 0; p < np; p++) {
      Skinner skinner(pc[p]->get_moab());
      Range part_ents, skin_ents;
      rval = pc[p]->get_moab()->get_entities_by_dimension(this_set, part_dim, part_ents);
      if (MB_SUCCESS != rval) return rval;
      rval = skinner.find_skin(this_set, part_ents, false, skin_ents, 0, true, true, true);
      if (MB_SUCCESS != rval) return rval;
      rval = pc[p]->get_moab()->get_adjacencies(skin_ents, 0, true, verts[p],
                                                Interface::UNION);
      if (MB_SUCCESS != rval) return rval;
      tot_verts += verts[p].size();
    }

    TupleList shared_ents;
    shared_ents.initialize(2, 0, 1, 0, tot_verts);
    shared_ents.enableWriteAccess();

    i = 0; j = 0;
    std::vector<int> gids;
    Range::iterator rit;
    Tag gid_tag;
    int dum_default = 0;
    for (p = 0; p < np; p++) {
      rval = pc[p]->get_moab()->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                               gid_tag, MB_TAG_DENSE | MB_TAG_CREAT,
                                               &dum_default);
      if (MB_SUCCESS != rval) return rval;
      gids.resize(verts[p].size());
      rval = pc[p]->get_moab()->tag_get_data(gid_tag, verts[p], &gids[0]);
      if (MB_SUCCESS != rval) return rval;

      for (v = 0, rit = verts[p].begin(); v < gids.size(); v++, ++rit) {
        shared_ents.vi_wr[i++] = gids[v];
        shared_ents.vi_wr[i++] = p;
        shared_ents.vul_wr[j] = *rit;
        j++;
        shared_ents.inc_n();
      }
    }

    moab::TupleList::buffer sort_buffer;
    sort_buffer.buffer_init(tot_verts);
    shared_ents.sort(0, &sort_buffer);
    sort_buffer.reset();

    j = 0; i = 0;
    std::vector<EntityHandle> handles;
    std::vector<int> procs;

    while (i < shared_ents.get_n()) {
      handles.clear();
      procs.clear();

      // Count & accumulate sharing procs
      int this_gid = shared_ents.vi_rd[j];
      while (i < shared_ents.get_n() && shared_ents.vi_rd[j] == this_gid) {
        j++;
        procs.push_back(shared_ents.vi_rd[j++]);
        handles.push_back(shared_ents.vul_rd[i++]);
      }
      if (1 == procs.size())
        continue;

      for (v = 0; v < procs.size(); v++) {
        rval = pc[procs[v]]->update_remote_data(handles[v],
                                                &procs[0], &handles[0], procs.size(),
                                                (procs[0] == (int)pc[procs[v]]->rank() ? PSTATUS_INTERFACE : (PSTATUS_NOT_OWNED|PSTATUS_INTERFACE)));
        if (MB_SUCCESS != rval) return rval;
      }
    }

    std::set<unsigned int> psets;
    for (p = 0; p < np; p++) {
      rval = pc[p]->create_interface_sets(this_set, part_dim, part_dim - 1);
      if (MB_SUCCESS != rval) return rval;
      // Establish comm procs and buffers for them
      psets.clear();
      rval = pc[p]->get_interface_procs(psets, true);
      if (MB_SUCCESS != rval) return rval;
    }

    shared_ents.reset();

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::tag_iface_entities() 
  {
    ErrorCode result = MB_SUCCESS;
    Range iface_ents, tmp_ents, rmv_ents;
    std::vector<unsigned char> pstat;
    unsigned char set_pstat;
    Range::iterator rit2;
    unsigned int i;

    for (Range::iterator rit = interfaceSets.begin(); rit != interfaceSets.end(); ++rit) {
      iface_ents.clear();

      result = mbImpl->get_entities_by_handle(*rit, iface_ents);MB_CHK_SET_ERR(result, "Failed to get interface set contents");
      pstat.resize(iface_ents.size());
      result = mbImpl->tag_get_data(pstatus_tag(), iface_ents, &pstat[0]);MB_CHK_SET_ERR(result, "Failed to get pstatus values for interface set entities");
      result = mbImpl->tag_get_data(pstatus_tag(), &(*rit), 1, &set_pstat);MB_CHK_SET_ERR(result, "Failed to get pstatus values for interface set");
      rmv_ents.clear();
      for (rit2 = iface_ents.begin(), i = 0; rit2 != iface_ents.end(); ++rit2, i++) {
        if (!(pstat[i] & PSTATUS_INTERFACE)) {
          rmv_ents.insert(*rit2);
          pstat[i] = 0x0;
        }
      }
      result = mbImpl->remove_entities(*rit, rmv_ents);MB_CHK_SET_ERR(result, "Failed to remove entities from interface set");

      if (!(set_pstat & PSTATUS_NOT_OWNED))
        continue;
      // If we're here, we need to set the notowned status on (remaining) set contents

      // Remove rmv_ents from the contents list
      iface_ents = subtract(iface_ents, rmv_ents);
      // Compress the pstat vector (removing 0x0's)
      std::remove_if(pstat.begin(), pstat.end(), 
                     std::bind2nd(std::equal_to<unsigned char>(), 0x0));
      // Fold the not_owned bit into remaining values
      unsigned int sz = iface_ents.size();
      for (i = 0; i < sz; i++)
        pstat[i] |= PSTATUS_NOT_OWNED;

      // Set the tag on the entities
      result = mbImpl->tag_set_data(pstatus_tag(), iface_ents, &pstat[0]);MB_CHK_SET_ERR(result, "Failed to set pstatus values for interface set entities");
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::set_pstatus_entities(Range &pstatus_ents,
                                               unsigned char pstatus_val,
                                               bool lower_dim_ents,
                                               bool verts_too,
                                               int operation)
  {
    std::vector<unsigned char> pstatus_vals(pstatus_ents.size());
    Range all_ents, *range_ptr = &pstatus_ents;
    ErrorCode result;
    if (lower_dim_ents || verts_too) {
      all_ents = pstatus_ents;
      range_ptr = &all_ents;
      int start_dim = (lower_dim_ents ? mbImpl->dimension_from_handle(*pstatus_ents.rbegin()) - 1 : 0);
      for (; start_dim >= 0; start_dim--) {
        result = mbImpl->get_adjacencies(all_ents, start_dim, true, all_ents,
                                         Interface::UNION);MB_CHK_SET_ERR(result, "Failed to get adjacencies for pstatus entities");
      }
    }
    if (Interface::UNION == operation) {
      result = mbImpl->tag_get_data(pstatus_tag(), *range_ptr, &pstatus_vals[0]);MB_CHK_SET_ERR(result, "Failed to get pstatus tag data");
      for (unsigned int i = 0; i < pstatus_vals.size(); i++)
        pstatus_vals[i] |= pstatus_val;
    }
    else {
      for (unsigned int i = 0; i < pstatus_vals.size(); i++)
        pstatus_vals[i] = pstatus_val;
    }
    result = mbImpl->tag_set_data(pstatus_tag(), *range_ptr, &pstatus_vals[0]);MB_CHK_SET_ERR(result, "Failed to set pstatus tag data");

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::set_pstatus_entities(EntityHandle *pstatus_ents,
                                               int num_ents,
                                               unsigned char pstatus_val,
                                               bool lower_dim_ents,
                                               bool verts_too,
                                               int operation)
  {
    std::vector<unsigned char> pstatus_vals(num_ents);
    ErrorCode result;
    if (lower_dim_ents || verts_too) {
      // In this case, call the range-based version
      Range tmp_range;
      std::copy(pstatus_ents, pstatus_ents + num_ents, range_inserter(tmp_range));
      return set_pstatus_entities(tmp_range, pstatus_val, lower_dim_ents, 
                                  verts_too, operation);
    }

    if (Interface::UNION == operation) {
      result = mbImpl->tag_get_data(pstatus_tag(), pstatus_ents, num_ents, &pstatus_vals[0]);MB_CHK_SET_ERR(result, "Failed to get pstatus tag data");
      for (unsigned int i = 0; i < (unsigned int) num_ents; i++)
        pstatus_vals[i] |= pstatus_val;
    }
    else {
      for (unsigned int i = 0; i < (unsigned int) num_ents; i++)
        pstatus_vals[i] = pstatus_val;
    }
    result = mbImpl->tag_set_data(pstatus_tag(), pstatus_ents, num_ents, &pstatus_vals[0]);MB_CHK_SET_ERR(result, "Failed to set pstatus tag data");

    return MB_SUCCESS;
  }

  static size_t choose_owner_idx(const std::vector<unsigned>& proc_list)
  {
    // Try to assign owners randomly so we get a good distribution,
    // (note: specifying the same seed on all procs is essential)
    unsigned val = 0;
    for (size_t i = 0; i < proc_list.size(); i++)
      val ^= proc_list[i];
    return rand_r(&val) % proc_list.size();
  }

  struct set_tuple
  {
    unsigned idx;
    unsigned proc;
    EntityHandle handle;
    inline bool operator<(set_tuple other) const
    { return (idx == other.idx) ? (proc < other.proc) : (idx < other.idx); }
  };

  ErrorCode ParallelComm::resolve_shared_sets(EntityHandle file, const Tag* idtag)
  {
    // Find all sets with any of the following tags:
    const char* const shared_set_tag_names[] = {GEOM_DIMENSION_TAG_NAME,
                                                MATERIAL_SET_TAG_NAME,
                                                DIRICHLET_SET_TAG_NAME,
                                                NEUMANN_SET_TAG_NAME,
                                                PARALLEL_PARTITION_TAG_NAME};
    int num_tags = sizeof(shared_set_tag_names) / sizeof(shared_set_tag_names[0]);
    Range candidate_sets;
    ErrorCode result;

    // If we're not given an ID tag to use to globally identify sets,
    // then fall back to using known tag values
    if (!idtag) {
      Tag gid, tag;
      result = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, gid);
      if (MB_SUCCESS == result) 
        result = mbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, tag);
      if (MB_SUCCESS == result) {
        for (int d = 0; d < 4; d++) {
          candidate_sets.clear();
          const void* vals[] = { &d };
          result = mbImpl->get_entities_by_type_and_tag(file, MBENTITYSET, &tag, vals, 1, candidate_sets);
          if (MB_SUCCESS == result)
            resolve_shared_sets(candidate_sets, gid);
        }
      }

      for (int i = 1; i < num_tags; i++) {
        result = mbImpl->tag_get_handle(shared_set_tag_names[i], 1, MB_TYPE_INTEGER, tag);
        if (MB_SUCCESS == result) {
          candidate_sets.clear();
          result = mbImpl->get_entities_by_type_and_tag(file, MBENTITYSET, &tag, 0, 1, candidate_sets);
          if (MB_SUCCESS == result)
            resolve_shared_sets(candidate_sets, tag);
        }
      }

      return MB_SUCCESS;
    }

    for (int i = 0; i < num_tags; i++) {
      Tag tag;
      result = mbImpl->tag_get_handle(shared_set_tag_names[i], 1, MB_TYPE_INTEGER,
                                      tag, MB_TAG_ANY);
      if (MB_SUCCESS != result)
        continue;

      mbImpl->get_entities_by_type_and_tag(file, MBENTITYSET, &tag, 0, 1, candidate_sets, Interface::UNION);
    }

    // Find any additional sets that contain shared entities
    Range::iterator hint = candidate_sets.begin();
    Range all_sets;
    mbImpl->get_entities_by_type(file, MBENTITYSET, all_sets);
    all_sets = subtract(all_sets, candidate_sets);
    Range::iterator it = all_sets.begin();
    while (it != all_sets.end()) {
      Range contents;
      mbImpl->get_entities_by_handle(*it, contents);
      contents.erase(contents.lower_bound(MBENTITYSET), contents.end());
      filter_pstatus(contents, PSTATUS_SHARED, PSTATUS_OR);
      if (contents.empty()) {
        ++it;
      }
      else {
        hint = candidate_sets.insert(hint, *it);
        it = all_sets.erase(it);
      }
    }

    // Find any additionl sets that contain or are parents of potential shared sets
    Range prev_list = candidate_sets;
    while (!prev_list.empty()) {
      it = all_sets.begin();
      Range new_list;
      hint = new_list.begin();
      while (it != all_sets.end()) {
        Range contents;
        mbImpl->get_entities_by_type(*it, MBENTITYSET, contents);
        if (!intersect(prev_list,contents).empty()) {
          hint = new_list.insert(hint, *it);
          it = all_sets.erase(it);
        }
        else {
          new_list.clear();
          mbImpl->get_child_meshsets(*it, contents);
          if (!intersect(prev_list,contents).empty()) {
            hint = new_list.insert(hint, *it);
            it = all_sets.erase(it);
          }
          else {
            ++it;
          }
        }
      }

      candidate_sets.merge(new_list);
      prev_list.swap(new_list);
    }

    return resolve_shared_sets(candidate_sets, *idtag);
  }

#ifndef NDEBUG
  bool is_sorted_unique(std::vector<unsigned>& v)
  {
    for (size_t i = 1; i < v.size(); i++)
      if (v[i - 1] >= v[i])
        return false;
    return true;
  }
#endif

  ErrorCode ParallelComm::resolve_shared_sets(Range& sets, Tag idtag)
  {
    ErrorCode result;
    const unsigned rk = proc_config().proc_rank();
    MPI_Comm cm = proc_config().proc_comm();

    // Build sharing list for all sets

    // Get ids for sets in a vector, to pass to gs
    std::vector<long> larray; // Allocate sufficient space for longs
    std::vector<Ulong> handles;
    Range tmp_sets;
    // The id tag can be size 4 or size 8
    // Based on that, convert to int or to long, similarly to what we do
    // for resolving shared vertices;
    // This code must work on 32 bit too, where long is 4 bytes, also
    // so test first size 4, then we should be fine
    DataType tag_type;
    result = mbImpl->tag_get_data_type(idtag, tag_type);MB_CHK_SET_ERR(result, "Failed getting tag data type");
    int bytes_per_tag;
    result = mbImpl->tag_get_bytes(idtag, bytes_per_tag);MB_CHK_SET_ERR(result, "Failed getting number of bytes per tag");
    // On 64 bits, long and int are different
    // On 32 bits, they are not; if size of long is 8, it is a 64 bit machine (really?)

    for (Range::iterator rit = sets.begin(); rit != sets.end(); ++rit) {
      if (sizeof(long) == bytes_per_tag && ((MB_TYPE_HANDLE == tag_type) || (MB_TYPE_OPAQUE == tag_type))) { // It is a special id tag
        long dum;
        result = mbImpl->tag_get_data(idtag, &(*rit), 1, &dum);
        if (MB_SUCCESS == result) {
          larray.push_back(dum);
          handles.push_back(*rit);
          tmp_sets.insert(tmp_sets.end(), *rit);
        }
      }
      else if (4 == bytes_per_tag) { // Must be GLOBAL_ID tag or MATERIAL_ID, etc
        int dum;
        result = mbImpl->tag_get_data(idtag, &(*rit), 1, &dum);
        if (MB_SUCCESS == result) {
          larray.push_back(dum);
          handles.push_back(*rit);
          tmp_sets.insert(tmp_sets.end(), *rit);
        }
      }
    }

    const size_t nsets = handles.size();

    // Get handle array for sets
    // This is not true on windows machine, 64 bits: entity handle is 64 bit, long is 32
    // assert(sizeof(EntityHandle) <= sizeof(unsigned long));

    // Do communication of data
    gs_data::crystal_data *cd = procConfig.crystal_router();
    gs_data *gsd = new gs_data();
    result = gsd->initialize(nsets, &larray[0], &handles[0], 2, 1, 1, cd);MB_CHK_SET_ERR(result, "Failed to create gs data");

    // Convert from global IDs grouped by process rank to list
    // of <idx, rank> pairs so that we can sort primarily
    // by idx and secondarily by rank (we want lists of procs for each
    // idx, not lists if indices for each proc).
    size_t ntuple = 0;
    for (unsigned p = 0; p < gsd->nlinfo->_np; p++)
      ntuple += gsd->nlinfo->_nshared[p];
    std::vector< set_tuple > tuples;
    tuples.reserve(ntuple);
    size_t j = 0;
    for (unsigned p = 0; p < gsd->nlinfo->_np; p++) {
      for (unsigned np = 0; np < gsd->nlinfo->_nshared[p]; np++) {
        set_tuple t;
        t.idx = gsd->nlinfo->_sh_ind[j];
        t.proc = gsd->nlinfo->_target[p];
        t.handle = gsd->nlinfo->_ulabels[j];
        tuples.push_back(t);
        j++;
      }
    }
    std::sort(tuples.begin(), tuples.end());

    // Release crystal router stuff
    gsd->reset();
    delete gsd;

    // Storing sharing data for each set
    size_t ti = 0;
    unsigned idx = 0;
    std::vector<unsigned> procs;
    Range::iterator si = tmp_sets.begin();
    while (si != tmp_sets.end() && ti < tuples.size()) {
      assert(idx <= tuples[ti].idx);
      if (idx < tuples[ti].idx) 
        si += (tuples[ti].idx - idx);
      idx = tuples[ti].idx;

      procs.clear();
      size_t ti_init = ti;
      while (ti < tuples.size() && tuples[ti].idx == idx) {
        procs.push_back(tuples[ti].proc);
        ++ti;
      }
      assert(is_sorted_unique(procs));

      result = sharedSetData->set_sharing_procs(*si, procs);
      if (MB_SUCCESS != result) {
        std::cerr << "Failure at " __FILE__ ":" << __LINE__ << std::endl;
        std::cerr.flush();
        MPI_Abort(cm, 1);
      }

      // Add this proc to list of sharing procs in correct position
      // so that all procs select owner based on same list
      std::vector<unsigned>::iterator it = std::lower_bound(procs.begin(), procs.end(), rk);
      assert(it == procs.end() || *it > rk);
      procs.insert(it, rk);
      size_t owner_idx = choose_owner_idx(procs);
      EntityHandle owner_handle;
      if (procs[owner_idx] == rk)
        owner_handle = *si;
      else if (procs[owner_idx] > rk)
        owner_handle = tuples[ti_init + owner_idx - 1].handle;
      else
        owner_handle = tuples[ti_init + owner_idx].handle;
      result = sharedSetData->set_owner(*si, procs[owner_idx], owner_handle);
      if (MB_SUCCESS != result) {
        std::cerr << "Failure at " __FILE__ ":" << __LINE__ << std::endl;
        std::cerr.flush();
        MPI_Abort(cm, 1);
      }

      ++si;
      ++idx;
    }

    return MB_SUCCESS;
  }
    // populate sets with ghost entities, if necessary
  ErrorCode ParallelComm::augment_default_sets_with_ghosts(EntityHandle file_set) {
    // gather all default sets we are interested in, material, neumann, etc
    // we will skip geometry sets, because they are not uniquely identified with their tag value
    // maybe we will add another tag, like category

    if (procConfig.proc_size() < 2)
      return MB_SUCCESS; // no reason to stop by
    const char* const shared_set_tag_names[] =
        { MATERIAL_SET_TAG_NAME, DIRICHLET_SET_TAG_NAME, NEUMANN_SET_TAG_NAME,
            PARALLEL_PARTITION_TAG_NAME };

    int num_tags = sizeof(shared_set_tag_names) / sizeof(shared_set_tag_names[0]);

    Range * rangeSets = new Range[num_tags];
    Tag * tags = new Tag[num_tags + 1]; // one extra for global id tag, which is an int, so far

    int my_rank = rank();
    int ** tagVals = new int*[num_tags];
    for (int i = 0; i < num_tags; i++)
      tagVals[i] = NULL;
    ErrorCode rval;

    // for each tag, we keep a local map, from the value to the actual set with that value
    // we assume that the tag values are unique, for a given set, otherwise we
    // do not know to which set to add the entity

    typedef std::map<int, EntityHandle> MVal;
    typedef std::map<int, EntityHandle>::iterator itMVal;
    MVal * localMaps = new MVal[num_tags];

    for (int i = 0; i < num_tags; i++) {

      rval = mbImpl->tag_get_handle(shared_set_tag_names[i], 1, MB_TYPE_INTEGER,
          tags[i], MB_TAG_ANY);
      if (MB_SUCCESS != rval)
        continue;
      rval = mbImpl->get_entities_by_type_and_tag(file_set, MBENTITYSET,
          &(tags[i]), 0, 1, rangeSets[i], Interface::UNION);
      MB_CHK_SET_ERR(rval, "can't get sets with a tag");

      if (rangeSets[i].size() > 0) {
        tagVals[i] = new int[rangeSets[i].size()];
        // fill up with the tag values
        rval = mbImpl->tag_get_data(tags[i], rangeSets[i], tagVals[i]);
        MB_CHK_SET_ERR(rval, "can't get set tag values");
        // now for inverse mapping:
        for (int j = 0; j < (int) rangeSets[i].size(); j++) {
          localMaps[i][tagVals[i][j]] = rangeSets[i][j];
        }
      }
    }
    // get the global id tag too
    rval = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
        tags[num_tags], MB_TAG_ANY);
    MB_CHK_SET_ERR(rval, "can't get global id tag");

    TupleList remoteEnts;
    // processor to send to, type of tag (0-mat,) tag value,     remote handle
    //                         1-diri
    //                         2-neum
    //                         3-part
    //
    int initialSize = (int)sharedEnts.size(); // estimate that on average, each shared ent
    // will be sent to one processor, for one tag
    // we will actually send only entities that are owned locally, and from those
    // only those that do have a special tag (material, neumann, etc)
    // if we exceed the capacity, we resize the tuple
    remoteEnts.initialize(3, 0, 1, 0, initialSize);
    remoteEnts.enableWriteAccess();

    // now, for each owned entity, get the remote handle(s) and Proc(s), and verify if it
    // belongs to one of the sets; if yes, create a tuple and append it

    std::set<EntityHandle> own_and_sha;
    int ir = 0, jr = 0;
    for (std::vector<EntityHandle>::iterator vit = sharedEnts.begin();
        vit != sharedEnts.end(); ++vit)
    {
      // ghosted eh
      EntityHandle geh = *vit;
      if (own_and_sha.find(geh)!=own_and_sha.end())// already encountered
        continue;
      int procs[MAX_SHARING_PROCS];
      EntityHandle handles[MAX_SHARING_PROCS];
      int nprocs;
      unsigned char pstat;
      rval = get_sharing_data(geh, procs, handles, pstat, nprocs);
      MB_CHK_SET_ERR(rval, "Failed to get sharing data");
      if (pstat & PSTATUS_NOT_OWNED)
        continue; // we will send info only for entities that we own
      own_and_sha.insert(geh);
      for (int i = 0; i < num_tags; i++) {
        for (int j = 0; j < (int) rangeSets[i].size(); j++) {
          EntityHandle specialSet = rangeSets[i][j]; // this set has tag i, value tagVals[i][j];
          if (mbImpl->contains_entities(specialSet, &geh, 1)) {
            // this ghosted entity is in a special set, so form the tuple
            // to send to the processors that do not own this
            for (int k = 0; k < nprocs; k++) {
              if (procs[k] != my_rank) {
                if (remoteEnts.get_n()>=remoteEnts.get_max()-1)
                {
                  // resize, so we do not overflow
                  int oldSize = remoteEnts.get_max();
                  // increase with 50% the capacity
                  remoteEnts.resize(oldSize+oldSize/2+1);
                }
                remoteEnts.vi_wr[ir++] = procs[k]; // send to proc
                remoteEnts.vi_wr[ir++] = i; // for the tags [i] (0-3)
                remoteEnts.vi_wr[ir++] = tagVals[i][j]; // actual value of the tag
                remoteEnts.vul_wr[jr++] = handles[k];
                remoteEnts.inc_n();
              }
            }
          }
        }
      }
      // if the local entity has a global id, send it too, so we avoid
      // another "exchange_tags" for global id
      int gid;
      rval = mbImpl->tag_get_data(tags[num_tags], &geh, 1, &gid);
      MB_CHK_SET_ERR(rval, "Failed to get global id");
      if (gid != 0) {
        for (int k = 0; k < nprocs; k++) {
          if (procs[k] != my_rank) {
            if (remoteEnts.get_n()>=remoteEnts.get_max()-1)
            {
              // resize, so we do not overflow
              int oldSize = remoteEnts.get_max();
              // increase with 50% the capacity
              remoteEnts.resize(oldSize+oldSize/2+1);
            }
            remoteEnts.vi_wr[ir++] = procs[k]; // send to proc
            remoteEnts.vi_wr[ir++] = num_tags; // for the tags [j] (4)
            remoteEnts.vi_wr[ir++] = gid; // actual value of the tag
            remoteEnts.vul_wr[jr++] = handles[k];
            remoteEnts.inc_n();
          }
        }
      }
    }

  #ifndef NDEBUG
    if (my_rank == 1 && 1 == get_debug_verbosity())
      remoteEnts.print(" on rank 1, before augment routing");
    MPI_Barrier(procConfig.proc_comm());
    int sentEnts = remoteEnts.get_n();
    assert((sentEnts == jr) && (3 * sentEnts == ir));
  #endif
    // exchange the info now, and send to
    gs_data::crystal_data *cd = this->procConfig.crystal_router();
    // All communication happens here; no other mpi calls
    // Also, this is a collective call
    rval = cd->gs_transfer(1, remoteEnts, 0);
    MB_CHK_SET_ERR(rval, "Error in tuple transfer");
  #ifndef NDEBUG
    if (my_rank == 0 && 1 == get_debug_verbosity())
      remoteEnts.print(" on rank 0, after augment routing");
    MPI_Barrier(procConfig.proc_comm());
  #endif

    // now process the data received from other processors
    int received = remoteEnts.get_n();
    for (int i = 0; i < received; i++) {
      //int from = ents_to_delete.vi_rd[i];
      EntityHandle geh = (EntityHandle) remoteEnts.vul_rd[i];
      int from_proc = remoteEnts.vi_rd[3 * i];
      if (my_rank == from_proc)
        std::cout << " unexpected receive from my rank " << my_rank
            << " during augmenting with ghosts\n ";
      int tag_type = remoteEnts.vi_rd[3 * i + 1];
      assert((0 <= tag_type) && (tag_type <= num_tags));
      int value = remoteEnts.vi_rd[3 * i + 2];
      if (tag_type == num_tags) {
        // it is global id
        rval = mbImpl->tag_set_data(tags[num_tags], &geh, 1, &value);
        MB_CHK_SET_ERR(rval, "Error in setting gid tag");
      } else {
        // now, based on value and tag type, see if we have that value in the map
        MVal & lmap = localMaps[tag_type];
        itMVal itm = lmap.find(value);
        if (itm == lmap.end()) {
          // the value was not found yet in the local map, so we have to create the set
          EntityHandle newSet;
          rval = mbImpl->create_meshset(MESHSET_SET, newSet);
          MB_CHK_SET_ERR(rval, "can't create new set");
          lmap[value] = newSet;
          // set the tag value
          rval = mbImpl->tag_set_data(tags[tag_type], &newSet, 1, &value);
          MB_CHK_SET_ERR(rval, "can't set tag for new set");

          // we also need to add the new created set to the file set, if not null
          if (file_set) {
            rval = mbImpl->add_entities(file_set, &newSet, 1);
            MB_CHK_SET_ERR(rval, "can't add new set to the file set");
          }
        }
        // add the entity to the set pointed to by the map
        rval = mbImpl->add_entities(lmap[value], &geh, 1);
        MB_CHK_SET_ERR(rval, "can't add ghost ent to the set");
      }
    }

    for (int i = 0; i < num_tags; i++)
      delete[] tagVals[i];
    delete[] tagVals;
    delete[] rangeSets;
    delete [] tags;
    delete[] localMaps;
    return MB_SUCCESS;
  }
  ErrorCode ParallelComm::create_interface_sets(EntityHandle this_set, int resolve_dim, int shared_dim)
  {
    std::map<std::vector<int>, std::vector<EntityHandle> > proc_nvecs;

    // Build up the list of shared entities
    int procs[MAX_SHARING_PROCS];
    EntityHandle handles[MAX_SHARING_PROCS];
    ErrorCode result;
    int nprocs;
    unsigned char pstat;
    for (std::vector<EntityHandle>::iterator vit = sharedEnts.begin(); vit != sharedEnts.end(); ++vit) {
      if (shared_dim != -1 && mbImpl->dimension_from_handle(*vit) > shared_dim)
        continue;
      result = get_sharing_data(*vit, procs, handles, pstat, nprocs);MB_CHK_SET_ERR(result, "Failed to get sharing data");
      std::sort(procs, procs + nprocs);
      std::vector<int> tmp_procs(procs, procs + nprocs);
      assert(tmp_procs.size() != 2);
      proc_nvecs[tmp_procs].push_back(*vit);
    }

    Skinner skinner(mbImpl);
    Range skin_ents[4];
    result = mbImpl->get_entities_by_dimension(this_set, resolve_dim, skin_ents[resolve_dim]);MB_CHK_SET_ERR(result, "Failed to get skin entities by dimension");
    result = skinner.find_skin(this_set, skin_ents[resolve_dim], false,
                               skin_ents[resolve_dim - 1], 0, true, true, true);MB_CHK_SET_ERR(result, "Failed to find skin");
    if (shared_dim > 1) {
      result = mbImpl->get_adjacencies(skin_ents[resolve_dim - 1], resolve_dim - 2, true,
                                       skin_ents[resolve_dim - 2], Interface::UNION);MB_CHK_SET_ERR(result, "Failed to get skin adjacencies");
    }

    result = get_proc_nvecs(resolve_dim, shared_dim, skin_ents, proc_nvecs);

    return create_interface_sets(proc_nvecs);
  }

  ErrorCode ParallelComm::create_interface_sets(std::map<std::vector<int>, std::vector<EntityHandle> > &proc_nvecs) 
  {
    if (proc_nvecs.empty())
      return MB_SUCCESS;

    int proc_ids[MAX_SHARING_PROCS];
    EntityHandle proc_handles[MAX_SHARING_PROCS];
    Tag shp_tag, shps_tag, shh_tag, shhs_tag, pstat_tag;
    ErrorCode result = get_shared_proc_tags(shp_tag, shps_tag,
                                            shh_tag, shhs_tag,
                                            pstat_tag);MB_CHK_SET_ERR(result, "Failed to get shared proc tags in create_interface_sets");
    Range::iterator rit;

    // Create interface sets, tag them, and tag their contents with iface set tag
    std::vector<unsigned char> pstatus;
    for (std::map<std::vector<int>,std::vector<EntityHandle> >::iterator vit = proc_nvecs.begin();
         vit != proc_nvecs.end(); ++vit) {
      // Create the set
      EntityHandle new_set;
      result = mbImpl->create_meshset(MESHSET_SET, new_set);MB_CHK_SET_ERR(result, "Failed to create interface set");
      interfaceSets.insert(new_set);

      // Add entities
      assert(!vit->second.empty());
      result = mbImpl->add_entities(new_set, &(vit->second)[0], (vit->second).size());MB_CHK_SET_ERR(result, "Failed to add entities to interface set");
      // Tag set with the proc rank(s)
      if (vit->first.size() == 1) {
        assert((vit->first)[0] != (int)procConfig.proc_rank());
        result = mbImpl->tag_set_data(shp_tag, &new_set, 1,
                                      &(vit->first)[0]);MB_CHK_SET_ERR(result, "Failed to tag interface set with procs");
        proc_handles[0] = 0;
        result = mbImpl->tag_set_data(shh_tag, &new_set, 1,
                                      proc_handles);MB_CHK_SET_ERR(result, "Failed to tag interface set with procs");
      }
      else {
        // Pad tag data out to MAX_SHARING_PROCS with -1
        if (vit->first.size() > MAX_SHARING_PROCS) {
          std::cerr << "Exceeded MAX_SHARING_PROCS for "
                    << CN::EntityTypeName(TYPE_FROM_HANDLE(new_set))
                    << ' ' << ID_FROM_HANDLE(new_set) 
                    << " on process " << proc_config().proc_rank()
                    << std::endl;
          std::cerr.flush();
          MPI_Abort(proc_config().proc_comm(), 66);
        }
        //assert(vit->first.size() <= MAX_SHARING_PROCS);
        std::copy(vit->first.begin(), vit->first.end(), proc_ids);
        std::fill(proc_ids + vit->first.size(), proc_ids + MAX_SHARING_PROCS, -1);
        result = mbImpl->tag_set_data(shps_tag, &new_set, 1, proc_ids);MB_CHK_SET_ERR(result, "Failed to tag interface set with procs");
        unsigned int ind = std::find(proc_ids, proc_ids + vit->first.size(), procConfig.proc_rank())
          - proc_ids;
        assert(ind < vit->first.size());
        std::fill(proc_handles, proc_handles + MAX_SHARING_PROCS, 0);
        proc_handles[ind] = new_set;
        result = mbImpl->tag_set_data(shhs_tag, &new_set, 1, proc_handles);MB_CHK_SET_ERR(result, "Failed to tag interface set with procs");
      }

      // Get the owning proc, then set the pstatus tag on iface set
      int min_proc = (vit->first)[0];
      unsigned char pval = (PSTATUS_SHARED | PSTATUS_INTERFACE);
      if (min_proc < (int) procConfig.proc_rank())
        pval |= PSTATUS_NOT_OWNED;
      if (vit->first.size() > 1)
        pval |= PSTATUS_MULTISHARED;
      result = mbImpl->tag_set_data(pstat_tag, &new_set, 1, &pval);MB_CHK_SET_ERR(result, "Failed to tag interface set with pstatus");

      // Tag the vertices with the same thing
      pstatus.clear();
      std::vector<EntityHandle> verts;
      for (std::vector<EntityHandle>::iterator v2it = (vit->second).begin(); v2it != (vit->second).end(); ++v2it)
        if (mbImpl->type_from_handle(*v2it) == MBVERTEX) verts.push_back(*v2it);
      pstatus.resize(verts.size(), pval);
      if (!verts.empty()) {
        result = mbImpl->tag_set_data(pstat_tag, &verts[0], verts.size(), &pstatus[0]);MB_CHK_SET_ERR(result, "Failed to tag interface set vertices with pstatus");
      }
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::create_iface_pc_links()
  {
    // Now that we've resolved the entities in the iface sets,
    // set parent/child links between the iface sets

    // First tag all entities in the iface sets
    Tag tmp_iface_tag;
    EntityHandle tmp_iface_set = 0;
    ErrorCode result = mbImpl->tag_get_handle("__tmp_iface", 1, MB_TYPE_HANDLE,
                                              tmp_iface_tag, MB_TAG_DENSE | MB_TAG_CREAT,
                                              &tmp_iface_set);MB_CHK_SET_ERR(result, "Failed to create temporary interface set tag");

    Range iface_ents;
    std::vector<EntityHandle> tag_vals;
    Range::iterator rit;

    for (rit = interfaceSets.begin(); rit != interfaceSets.end(); ++rit) {
      // tag entities with interface set
      iface_ents.clear();
      result = mbImpl->get_entities_by_handle(*rit, iface_ents);MB_CHK_SET_ERR(result, "Failed to get entities in interface set");

      if (iface_ents.empty())
        continue;

      tag_vals.resize(iface_ents.size());
      std::fill(tag_vals.begin(), tag_vals.end(), *rit);
      result = mbImpl->tag_set_data(tmp_iface_tag, iface_ents, &tag_vals[0]);MB_CHK_SET_ERR(result, "Failed to tag iface entities with interface set");
    }

    // Now go back through interface sets and add parent/child links
    Range tmp_ents2;
    for (int d = 2; d >= 0; d--) {
      for (rit = interfaceSets.begin(); rit != interfaceSets.end(); ++rit) {
        // Get entities on this interface
        iface_ents.clear();
        result = mbImpl->get_entities_by_handle(*rit, iface_ents, true);MB_CHK_SET_ERR(result, "Failed to get entities by handle");
        if (iface_ents.empty() || mbImpl->dimension_from_handle(*iface_ents.rbegin()) != d)
          continue;

        // Get higher-dimensional entities and their interface sets
        result = mbImpl->get_adjacencies(&(*iface_ents.begin()), 1, d + 1,
                                         false, tmp_ents2);MB_CHK_SET_ERR(result, "Failed to get adjacencies for interface sets");
        tag_vals.resize(tmp_ents2.size());
        result = mbImpl->tag_get_data(tmp_iface_tag, tmp_ents2, &tag_vals[0]);MB_CHK_SET_ERR(result, "Failed to get tmp iface tag for interface sets");

        // Go through and for any on interface make it a parent
        EntityHandle last_set = 0;
        for (unsigned int i = 0; i < tag_vals.size(); i++) {
          if (tag_vals[i] && tag_vals[i] != last_set) {
            result = mbImpl->add_parent_child(tag_vals[i], *rit);MB_CHK_SET_ERR(result, "Failed to add parent/child link for interface set");
            last_set = tag_vals[i];
          }
        }
      }
    }

    // Delete the temporary tag
    result = mbImpl->tag_delete(tmp_iface_tag);MB_CHK_SET_ERR(result, "Failed to delete tmp iface tag");

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_proc_nvecs(int resolve_dim,
                                         int shared_dim,
                                         Range *skin_ents,
                                         std::map<std::vector<int>, std::vector<EntityHandle> > &proc_nvecs)
  {
    // Set sharing procs tags on other skin ents
    ErrorCode result;
    const EntityHandle *connect; int num_connect;
    std::set<int> sharing_procs;
    std::vector<EntityHandle> dum_connect;
    std::vector<int> sp_vec;

    for (int d = 3; d > 0; d--) {
      if (resolve_dim == d)
        continue;

      for (Range::iterator rit = skin_ents[d].begin();
           rit != skin_ents[d].end(); ++rit) {
        // Get connectivity
        result = mbImpl->get_connectivity(*rit, connect, num_connect, false,
                                          &dum_connect);MB_CHK_SET_ERR(result, "Failed to get connectivity on non-vertex skin entities");

        int op = (resolve_dim < shared_dim ? Interface::UNION : Interface::INTERSECT);
        result = get_sharing_data(connect, num_connect, sharing_procs, op);MB_CHK_SET_ERR(result, "Failed to get sharing data in get_proc_nvecs");
        if (sharing_procs.empty() ||
            (sharing_procs.size() == 1 && *sharing_procs.begin() == (int)procConfig.proc_rank()))
          continue;

        // Need to specify sharing data correctly for entities or they will
        // end up in a different interface set than corresponding vertices
        if (sharing_procs.size() == 2) {
          std::set<int>::iterator it = sharing_procs.find(proc_config().proc_rank());
          assert(it != sharing_procs.end());
          sharing_procs.erase(it);
        }

        // Intersection is the owning proc(s) for this skin ent
        sp_vec.clear();
        std::copy(sharing_procs.begin(), sharing_procs.end(), std::back_inserter(sp_vec));
        assert(sp_vec.size() != 2);
        proc_nvecs[sp_vec].push_back(*rit);
      }
    }

#ifndef NDEBUG
    // Shouldn't be any repeated entities in any of the vectors in proc_nvecs
    for (std::map<std::vector<int>, std::vector<EntityHandle> >::iterator mit = proc_nvecs.begin();
         mit != proc_nvecs.end(); ++mit) {
      std::vector<EntityHandle> tmp_vec = (mit->second);
      std::sort(tmp_vec.begin(), tmp_vec.end());
      std::vector<EntityHandle>::iterator vit = std::unique(tmp_vec.begin(), tmp_vec.end());
      assert(vit == tmp_vec.end());
    }
#endif

    return MB_SUCCESS;
  }

  // Overloaded form of tag_shared_verts
  // Tuple coming in is of form (arbitrary value, remoteProc, localHandle, remoteHandle)
  // Also will check for doubles in the list if the list is sorted
  ErrorCode ParallelComm::tag_shared_verts(TupleList &shared_ents,
                                           std::map<std::vector<int>, std::vector<EntityHandle> > &proc_nvecs,
                                           Range& /*proc_verts*/,
                                           unsigned int i_extra)
  {
    Tag shp_tag, shps_tag, shh_tag, shhs_tag, pstat_tag;
    ErrorCode result = get_shared_proc_tags(shp_tag, shps_tag,
                                            shh_tag, shhs_tag,
                                            pstat_tag);MB_CHK_SET_ERR(result, "Failed to get shared proc tags in tag_shared_verts");

    unsigned int j = 0, i = 0;
    std::vector<int> sharing_procs, sharing_procs2, tag_procs;
    std::vector<EntityHandle> sharing_handles, sharing_handles2, tag_lhandles, tag_rhandles;
    std::vector<unsigned char> pstatus;

    // Were on tuple j/2
    if (i_extra)
      i += i_extra;
    while (j < 2*shared_ents.get_n()) {
      // Count & accumulate sharing procs
      EntityHandle this_ent = shared_ents.vul_rd[j], other_ent = 0;
      int other_proc = -1;
      while (j < 2*shared_ents.get_n() && shared_ents.vul_rd[j] == this_ent) {
        j++;
        // Shouldn't have same proc
        assert(shared_ents.vi_rd[i] != (int)procConfig.proc_rank());
        // Grab the remote data if its not a dublicate
        if (shared_ents.vul_rd[j] != other_ent || shared_ents.vi_rd[i] != other_proc) {
          assert(0 != shared_ents.vul_rd[j]);
          sharing_procs.push_back(shared_ents.vi_rd[i]);
          sharing_handles.push_back(shared_ents.vul_rd[j]);
        }
        other_proc = shared_ents.vi_rd[i];
        other_ent = shared_ents.vul_rd[j];
        j++;
        i += 1 + i_extra;
      }

      if (sharing_procs.size() > 1) {
        // Add current proc/handle to list
        sharing_procs.push_back(procConfig.proc_rank());
        sharing_handles.push_back(this_ent);

        // Sort sharing_procs and sharing_handles such that
        // sharing_procs is in ascending order. Use temporary
        // lists and binary search to re-order sharing_handles.
        sharing_procs2 = sharing_procs;
        std::sort(sharing_procs2.begin(), sharing_procs2.end());
        sharing_handles2.resize(sharing_handles.size());
        for (size_t k = 0; k < sharing_handles.size(); k++) {
          size_t idx = std::lower_bound(sharing_procs2.begin(),
                                        sharing_procs2.end(),
                                        sharing_procs[k]) - sharing_procs2.begin();
          sharing_handles2[idx] = sharing_handles[k];
        }
        sharing_procs.swap(sharing_procs2);
        sharing_handles.swap(sharing_handles2);
      }

      assert(sharing_procs.size() != 2);
      proc_nvecs[sharing_procs].push_back(this_ent);

      unsigned char share_flag = PSTATUS_SHARED, 
        ms_flag = (PSTATUS_SHARED | PSTATUS_MULTISHARED);
      if (sharing_procs.size() == 1) {
        tag_procs.push_back(sharing_procs[0]);
        tag_lhandles.push_back(this_ent);
        tag_rhandles.push_back(sharing_handles[0]);
        pstatus.push_back(share_flag);
      }
      else {
        // Pad lists
        //assert(sharing_procs.size() <= MAX_SHARING_PROCS);
        if (sharing_procs.size() > MAX_SHARING_PROCS) {
          std::cerr << "MAX_SHARING_PROCS exceeded for vertex " << this_ent <<
            " on process " << proc_config().proc_rank() << std::endl;
          std::cerr.flush();
          MPI_Abort(proc_config().proc_comm(), 66);
        }
        sharing_procs.resize(MAX_SHARING_PROCS, -1);
        sharing_handles.resize(MAX_SHARING_PROCS, 0);
        result = mbImpl->tag_set_data(shps_tag, &this_ent, 1,
                                      &sharing_procs[0]);MB_CHK_SET_ERR(result, "Failed to set sharedps tag on shared vertex");
        result = mbImpl->tag_set_data(shhs_tag, &this_ent, 1,
                                      &sharing_handles[0]);MB_CHK_SET_ERR(result, "Failed to set sharedhs tag on shared vertex");
        result = mbImpl->tag_set_data(pstat_tag, &this_ent, 1, &ms_flag);MB_CHK_SET_ERR(result, "Failed to set pstatus tag on shared vertex");
        sharedEnts.push_back(this_ent);
      }

      // Reset sharing proc(s) tags
      sharing_procs.clear();
      sharing_handles.clear();
    }

    if (!tag_procs.empty()) {
      result = mbImpl->tag_set_data(shp_tag, &tag_lhandles[0], tag_procs.size(),
                                    &tag_procs[0]);MB_CHK_SET_ERR(result, "Failed to set sharedp tag on shared vertex");
      result = mbImpl->tag_set_data(shh_tag, &tag_lhandles[0], tag_procs.size(),
                                    &tag_rhandles[0]);MB_CHK_SET_ERR(result, "Failed to set sharedh tag on shared vertex");
      result = mbImpl->tag_set_data(pstat_tag, &tag_lhandles[0], tag_procs.size(), &pstatus[0]);MB_CHK_SET_ERR(result, "Failed to set pstatus tag on shared vertex");
      std::copy(tag_lhandles.begin(), tag_lhandles.end(), std::back_inserter(sharedEnts));
    }

#ifndef NDEBUG
    // Shouldn't be any repeated entities in any of the vectors in proc_nvecs
    for (std::map<std::vector<int>, std::vector<EntityHandle> >::iterator mit = proc_nvecs.begin();
         mit != proc_nvecs.end(); ++mit) {
      std::vector<EntityHandle> tmp_vec = (mit->second);
      std::sort(tmp_vec.begin(), tmp_vec.end());
      std::vector<EntityHandle>::iterator vit = std::unique(tmp_vec.begin(), tmp_vec.end());
      assert(vit == tmp_vec.end());
    }
#endif

    return MB_SUCCESS;
  }
 
  ErrorCode ParallelComm::tag_shared_verts(TupleList &shared_ents,
                                           Range *skin_ents,
                                           std::map<std::vector<int>, std::vector<EntityHandle> > &proc_nvecs,
                                           Range& /*proc_verts*/)
  {
    Tag shp_tag, shps_tag, shh_tag, shhs_tag, pstat_tag;
    ErrorCode result = get_shared_proc_tags(shp_tag, shps_tag,
                                            shh_tag, shhs_tag, pstat_tag);MB_CHK_SET_ERR(result, "Failed to get shared proc tags in tag_shared_verts");

    unsigned int j = 0, i = 0;
    std::vector<int> sharing_procs, sharing_procs2;
    std::vector<EntityHandle> sharing_handles, sharing_handles2, skin_verts(skin_ents[0].size());
    for (Range::iterator rit = skin_ents[0].begin(); rit != skin_ents[0].end(); ++rit, i++)
      skin_verts[i] = *rit;
    i = 0;

    while (j < 2*shared_ents.get_n()) {
      // Count & accumulate sharing procs
      int this_idx = shared_ents.vi_rd[j];
      EntityHandle this_ent = skin_verts[this_idx];
      while (j < 2*shared_ents.get_n() && shared_ents.vi_rd[j] == this_idx) {
        j++;
        // Shouldn't have same proc
        assert(shared_ents.vi_rd[j] != (int)procConfig.proc_rank());
        sharing_procs.push_back(shared_ents.vi_rd[j++]);
        sharing_handles.push_back(shared_ents.vul_rd[i++]);
      }

      if (sharing_procs.size() > 1) {
        // Add current proc/handle to list
        sharing_procs.push_back(procConfig.proc_rank());
        sharing_handles.push_back(this_ent);
      }

      // Sort sharing_procs and sharing_handles such that
      // sharing_procs is in ascending order. Use temporary
      // lists and binary search to re-order sharing_handles.
      sharing_procs2 = sharing_procs;
      std::sort(sharing_procs2.begin(), sharing_procs2.end());
      sharing_handles2.resize(sharing_handles.size());
      for (size_t k = 0; k < sharing_handles.size(); k++) {
        size_t idx = std::lower_bound(sharing_procs2.begin(),
                                      sharing_procs2.end(),
                                      sharing_procs[k]) - sharing_procs2.begin();
        sharing_handles2[idx] = sharing_handles[k];
      }
      sharing_procs.swap(sharing_procs2);
      sharing_handles.swap(sharing_handles2);

      assert(sharing_procs.size() != 2);
      proc_nvecs[sharing_procs].push_back(this_ent);

      unsigned char share_flag = PSTATUS_SHARED,
        ms_flag = (PSTATUS_SHARED | PSTATUS_MULTISHARED);
      if (sharing_procs.size() == 1) {
        result = mbImpl->tag_set_data(shp_tag, &this_ent, 1,
                                      &sharing_procs[0]);MB_CHK_SET_ERR(result, "Failed to set sharedp tag on shared vertex");
        result = mbImpl->tag_set_data(shh_tag, &this_ent, 1,
                                      &sharing_handles[0]);MB_CHK_SET_ERR(result, "Failed to set sharedh tag on shared vertex");
        result = mbImpl->tag_set_data(pstat_tag, &this_ent, 1, &share_flag);MB_CHK_SET_ERR(result, "Failed to set pstatus tag on shared vertex");
        sharedEnts.push_back(this_ent);
      }
      else {
        // Pad lists
        //assert(sharing_procs.size() <= MAX_SHARING_PROCS);
        if (sharing_procs.size() > MAX_SHARING_PROCS) {
          std::cerr << "MAX_SHARING_PROCS exceeded for vertex " << this_ent <<
            " on process " << proc_config().proc_rank() <<  std::endl;
          std::cerr.flush();
          MPI_Abort(proc_config().proc_comm(), 66);
        }
        sharing_procs.resize(MAX_SHARING_PROCS, -1);
        sharing_handles.resize(MAX_SHARING_PROCS, 0);
        result = mbImpl->tag_set_data(shps_tag, &this_ent, 1,
                                      &sharing_procs[0]);MB_CHK_SET_ERR(result, "Failed to set sharedps tag on shared vertex");
        result = mbImpl->tag_set_data(shhs_tag, &this_ent, 1,
                                      &sharing_handles[0]);MB_CHK_SET_ERR(result, "Failed to set sharedhs tag on shared vertex");
        result = mbImpl->tag_set_data(pstat_tag, &this_ent, 1, &ms_flag);MB_CHK_SET_ERR(result, "Failed to set pstatus tag on shared vertex");
        sharedEnts.push_back(this_ent);
      }

      // Reset sharing proc(s) tags
      sharing_procs.clear();
      sharing_handles.clear();
    }

#ifndef NDEBUG
    // Shouldn't be any repeated entities in any of the vectors in proc_nvecs
    for (std::map<std::vector<int>, std::vector<EntityHandle> >::iterator mit = proc_nvecs.begin();
         mit != proc_nvecs.end(); ++mit) {
      std::vector<EntityHandle> tmp_vec = (mit->second);
      std::sort(tmp_vec.begin(), tmp_vec.end());
      std::vector<EntityHandle>::iterator vit = std::unique(tmp_vec.begin(), tmp_vec.end());
      assert(vit == tmp_vec.end());
    }
#endif

    return MB_SUCCESS;
  }
  
  //! Get processors with which this processor communicates; sets are sorted by processor
  ErrorCode ParallelComm::get_interface_procs(std::set<unsigned int> &procs_set,
                                              bool get_buffs)
  {
    // Make sure the sharing procs vector is empty
    procs_set.clear();

    // Pre-load vector of single-proc tag values
    unsigned int i, j;
    std::vector<int> iface_proc(interfaceSets.size());
    ErrorCode result = mbImpl->tag_get_data(sharedp_tag(), interfaceSets, &iface_proc[0]);MB_CHK_SET_ERR(result, "Failed to get iface_proc for iface sets");

    // Get sharing procs either from single-proc vector or by getting
    // multi-proc tag value
    int tmp_iface_procs[MAX_SHARING_PROCS];
    std::fill(tmp_iface_procs, tmp_iface_procs + MAX_SHARING_PROCS, -1);
    Range::iterator rit;
    for (rit = interfaceSets.begin(), i = 0; rit != interfaceSets.end(); ++rit, i++) {
      if (-1 != iface_proc[i]) {
        assert(iface_proc[i] != (int)procConfig.proc_rank());
        procs_set.insert((unsigned int) iface_proc[i]);
      }
      else {
        // Get the sharing_procs tag
        result = mbImpl->tag_get_data(sharedps_tag(), &(*rit), 1,
                                      tmp_iface_procs);MB_CHK_SET_ERR(result, "Failed to get iface_procs for iface set");
        for (j = 0; j < MAX_SHARING_PROCS; j++) {
          if (-1 != tmp_iface_procs[j] && tmp_iface_procs[j] != (int)procConfig.proc_rank()) 
            procs_set.insert((unsigned int) tmp_iface_procs[j]);
          else if (-1 == tmp_iface_procs[j]) {
            std::fill(tmp_iface_procs, tmp_iface_procs + j, -1);
            break;
          }
        }
      }
    }

    if (get_buffs) {
      for (std::set<unsigned int>::iterator sit = procs_set.begin(); sit != procs_set.end(); ++sit)
        get_buffers(*sit);
    }

    return MB_SUCCESS;
  }
  
  ErrorCode ParallelComm::get_pstatus(EntityHandle entity,
                                      unsigned char &pstatus_val)
  {
    ErrorCode result = mbImpl->tag_get_data(pstatus_tag(), &entity, 1, &pstatus_val);MB_CHK_SET_ERR(result, "Failed to get pastatus tag data");
    return result;
  }

  ErrorCode ParallelComm::get_pstatus_entities(int dim,
                                               unsigned char pstatus_val,
                                               Range &pstatus_ents)
  {
    Range ents;
    ErrorCode result;

    if (-1 == dim) {
      result = mbImpl->get_entities_by_handle(0, ents);MB_CHK_SET_ERR(result, "Failed to get all entities");
    }
    else {
      result = mbImpl->get_entities_by_dimension(0, dim, ents);MB_CHK_SET_ERR(result, "Failed to get entities of dimension " << dim);
    }

    std::vector<unsigned char> pstatus(ents.size());
    result = mbImpl->tag_get_data(pstatus_tag(), ents, &pstatus[0]);MB_CHK_SET_ERR(result, "Failed to get pastatus tag data");
    Range::iterator rit = ents.begin();
    int i = 0;
    if (pstatus_val) {
      for (; rit != ents.end(); i++, ++rit) {
        if (pstatus[i]&pstatus_val &&
            (-1 == dim || mbImpl->dimension_from_handle(*rit) == dim))
          pstatus_ents.insert(*rit);
      }
    }
    else {
      for (; rit != ents.end(); i++, ++rit) {
        if (!pstatus[i] &&
            (-1 == dim || mbImpl->dimension_from_handle(*rit) == dim))
          pstatus_ents.insert(*rit);
      }
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::check_global_ids(EntityHandle this_set,
                                           const int dimension,
                                           const int start_id,
                                           const bool largest_dim_only,
                                           const bool parallel,
                                           const bool owned_only)
  {
    // Global id tag
    Tag gid_tag; int def_val = -1;
    ErrorCode result = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                              gid_tag, MB_TAG_DENSE | MB_TAG_CREAT, &def_val);
    if (MB_ALREADY_ALLOCATED != result && MB_SUCCESS != result) {
      MB_SET_ERR(result, "Failed to create/get gid tag handle");
    }

    Range dum_range;
    if (MB_ALREADY_ALLOCATED == result) {
      void *tag_ptr = &def_val;
      ErrorCode tmp_result = mbImpl->get_entities_by_type_and_tag(this_set, MBVERTEX, 
                                                                  &gid_tag, &tag_ptr, 1,
                                                                  dum_range);MB_CHK_SET_ERR(tmp_result, "Failed to get entities by MBVERTEX type and gid tag");
    }

    if (MB_ALREADY_ALLOCATED != result || !dum_range.empty()) {
      // Just created it, so we need global ids
      result = assign_global_ids(this_set, dimension, start_id, largest_dim_only,
                                 parallel, owned_only);MB_CHK_SET_ERR(result, "Failed assigning global ids");
    }

    return MB_SUCCESS;
  }

  bool ParallelComm::is_iface_proc(EntityHandle this_set,
                                   int to_proc) 
  {
    int sharing_procs[MAX_SHARING_PROCS];
    std::fill(sharing_procs, sharing_procs + MAX_SHARING_PROCS, -1);
    ErrorCode result = mbImpl->tag_get_data(sharedp_tag(), &this_set, 1,
                                            sharing_procs);
    if (MB_SUCCESS == result && to_proc == sharing_procs[0])
      return true;

    result = mbImpl->tag_get_data(sharedps_tag(), &this_set, 1,
                                  sharing_procs);
    if (MB_SUCCESS != result)
      return false;

    for (int i = 0; i < MAX_SHARING_PROCS; i++) {
      if (to_proc == sharing_procs[i])
        return true;
      else if (-1 == sharing_procs[i])
        return false;
    }

    return false;
  }

  ErrorCode ParallelComm::filter_pstatus(Range &ents,
                                         unsigned char pstat,
                                         unsigned char op,
                                         int to_proc,
                                         Range *returned_ents)
  {
    Range tmp_ents;

    //assert(!ents.empty());
    if (ents.empty()) {
      if (returned_ents)
        returned_ents->clear();
      return MB_SUCCESS;
    }

    // Put into tmp_ents any entities which are not owned locally or
    // who are already shared with to_proc
    std::vector<unsigned char> shared_flags(ents.size()), shared_flags2;
    ErrorCode result = mbImpl->tag_get_data(pstatus_tag(), ents,
                                            &shared_flags[0]);MB_CHK_SET_ERR(result, "Failed to get pstatus flag");
    Range::const_iterator rit, hint = tmp_ents.begin();;
    int i;
    if (op == PSTATUS_OR) {
      for (rit = ents.begin(), i = 0; rit != ents.end(); ++rit, i++) {
        if (((shared_flags[i] & ~pstat) ^ shared_flags[i]) & pstat) {
          hint = tmp_ents.insert(hint, *rit);
          if (-1 != to_proc)
            shared_flags2.push_back(shared_flags[i]);
        }
      }
    }
    else if (op == PSTATUS_AND) {
      for (rit = ents.begin(), i = 0; rit != ents.end(); ++rit, i++) {
        if ((shared_flags[i] & pstat) == pstat) {
          hint = tmp_ents.insert(hint, *rit);
          if (-1 != to_proc)
            shared_flags2.push_back(shared_flags[i]);
        }
      }
    }
    else if (op == PSTATUS_NOT) {
      for (rit = ents.begin(), i = 0; rit != ents.end(); ++rit, i++) {
        if (!(shared_flags[i] & pstat)) {
          hint = tmp_ents.insert(hint, *rit);
          if (-1 != to_proc)
            shared_flags2.push_back(shared_flags[i]);
        }
      }
    }
    else {
      assert(false);
      return MB_FAILURE;
    }

    if (-1 != to_proc) {
      int sharing_procs[MAX_SHARING_PROCS];
      std::fill(sharing_procs, sharing_procs + MAX_SHARING_PROCS, -1);
      Range tmp_ents2;
      hint = tmp_ents2.begin();

      for (rit = tmp_ents.begin(), i = 0; rit != tmp_ents.end(); ++rit, i++) {
        // We need to check sharing procs
        if (shared_flags2[i] & PSTATUS_MULTISHARED) {
          result = mbImpl->tag_get_data(sharedps_tag(), &(*rit), 1,
                                        sharing_procs);MB_CHK_SET_ERR(result, "Failed to get sharedps tag");
          assert(-1 != sharing_procs[0]);
          for (unsigned int j = 0; j < MAX_SHARING_PROCS; j++) {
            // If to_proc shares this entity, add it to list
            if (sharing_procs[j] == to_proc) {
              hint = tmp_ents2.insert(hint, *rit);
            }
            else if (-1 == sharing_procs[j])
              break;

            sharing_procs[j] = -1;
          }
        }
        else if (shared_flags2[i] & PSTATUS_SHARED) {
          result = mbImpl->tag_get_data(sharedp_tag(), &(*rit), 1,
                                        sharing_procs);MB_CHK_SET_ERR(result, "Failed to get sharedp tag");
          assert(-1 != sharing_procs[0]);
          if (sharing_procs[0] == to_proc) 
            hint = tmp_ents2.insert(hint, *rit);
          sharing_procs[0] = -1;
        }
        else
          assert("should never get here" && false);
      }

      tmp_ents.swap(tmp_ents2);
    }

    if (returned_ents)
      returned_ents->swap(tmp_ents);
    else
      ents.swap(tmp_ents);

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::exchange_ghost_cells(int ghost_dim, int bridge_dim,
                                               int num_layers, int addl_ents,
                                               bool store_remote_handles,
                                               bool wait_all,
                                               EntityHandle *file_set)
  {
#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      if (!num_layers)
        MPE_Log_event(IFACE_START, procConfig.proc_rank(), "Starting interface exchange.");
      else
        MPE_Log_event(GHOST_START, procConfig.proc_rank(), "Starting ghost exchange.");
    }
#endif

    myDebug->tprintf(1, "Entering exchange_ghost_cells with num_layers = %d\n", num_layers);
    if (myDebug->get_verbosity() == 4) {
      msgs.clear();
      msgs.reserve(MAX_SHARING_PROCS);
    }

    // If we're only finding out about existing ents, we have to be storing
    // remote handles too
    assert(num_layers > 0 || store_remote_handles);

    const bool is_iface = !num_layers;

    // Get the b-dimensional interface(s) with with_proc, where b = bridge_dim

    int success;
    ErrorCode result = MB_SUCCESS;
    int incoming1 = 0, incoming2 = 0;

    reset_all_buffers();

    // When this function is called, buffProcs should already have any
    // communicating procs

    //===========================================
    // Post ghost irecv's for ghost entities from all communicating procs
    //===========================================
#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(ENTITIES_START, procConfig.proc_rank(), "Starting entity exchange.");
    }
#endif

    // Index reqs the same as buffer/sharing procs indices
    std::vector<MPI_Request> recv_ent_reqs(3*buffProcs.size(), MPI_REQUEST_NULL),
      recv_remoteh_reqs(3*buffProcs.size(), MPI_REQUEST_NULL);
    std::vector<unsigned int>::iterator proc_it;
    int ind, p;
    sendReqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);
    for (ind = 0, proc_it = buffProcs.begin(); 
         proc_it != buffProcs.end(); ++proc_it, ind++) {
      incoming1++;
      PRINT_DEBUG_IRECV(procConfig.proc_rank(), buffProcs[ind],
                        remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
                        MB_MESG_ENTS_SIZE, incoming1);
      success = MPI_Irecv(remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
                          MPI_UNSIGNED_CHAR, buffProcs[ind],
                          MB_MESG_ENTS_SIZE, procConfig.proc_comm(),
                          &recv_ent_reqs[3*ind]);
      if (success != MPI_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Failed to post irecv in ghost exchange");
      }
    }

    //===========================================
    // Get entities to be sent to neighbors
    //===========================================
    Range sent_ents[MAX_SHARING_PROCS], allsent, tmp_range;
    TupleList entprocs;
    int dum_ack_buff;
    result = get_sent_ents(is_iface, bridge_dim, ghost_dim, num_layers,
                           addl_ents, sent_ents, allsent, entprocs);MB_CHK_SET_ERR(result, "get_sent_ents failed");

    // augment file set with the entities to be sent
    // we might have created new entities if addl_ents>0, edges and/or faces
    if (addl_ents> 0 && file_set && !allsent.empty()) {
      result = mbImpl->add_entities(*file_set, allsent);
      MB_CHK_SET_ERR(result, "Failed to add new sub-entities to set");
    }
    myDebug->tprintf(1, "allsent ents compactness (size) = %f (%lu)\n", allsent.compactness(),
                     (unsigned long)allsent.size());

    //===========================================
    // Pack and send ents from this proc to others
    //===========================================
    for (p = 0, proc_it = buffProcs.begin(); 
         proc_it != buffProcs.end(); ++proc_it, p++) {
      myDebug->tprintf(1, "Sent ents compactness (size) = %f (%lu)\n", sent_ents[p].compactness(),
                       (unsigned long)sent_ents[p].size());
    
      // Reserve space on front for size and for initial buff size
      localOwnedBuffs[p]->reset_buffer(sizeof(int));

      // Entities
      result = pack_entities(sent_ents[p], localOwnedBuffs[p],
                             store_remote_handles, buffProcs[p], is_iface,
                             &entprocs, &allsent);MB_CHK_SET_ERR(result, "Packing entities failed");

      if (myDebug->get_verbosity() == 4) {
        msgs.resize(msgs.size() + 1);
        msgs.back() = new Buffer(*localOwnedBuffs[p]);
      }

      // Send the buffer (size stored in front in send_buffer)
      result = send_buffer(*proc_it, localOwnedBuffs[p],
                           MB_MESG_ENTS_SIZE, sendReqs[3*p],
                           recv_ent_reqs[3*p + 2], &dum_ack_buff,
                           incoming1,
                           MB_MESG_REMOTEH_SIZE,
                           (!is_iface && store_remote_handles ?  // this used for ghosting only
                            localOwnedBuffs[p] : NULL),
                           &recv_remoteh_reqs[3*p], &incoming2);MB_CHK_SET_ERR(result, "Failed to Isend in ghost exchange");
    }

    entprocs.reset();

    //===========================================
    // Receive/unpack new entities
    //===========================================
    // Number of incoming messages for ghosts is the number of procs we
    // communicate with; for iface, it's the number of those with lower rank
    MPI_Status status;
    std::vector<std::vector<EntityHandle> > recd_ents(buffProcs.size());
    std::vector<std::vector<EntityHandle> > L1hloc(buffProcs.size()), L1hrem(buffProcs.size());
    std::vector<std::vector<int> > L1p(buffProcs.size());
    std::vector<EntityHandle> L2hloc, L2hrem;
    std::vector<unsigned int> L2p;
    std::vector<EntityHandle> new_ents;

    while (incoming1) {
      // Wait for all recvs of ghost ents before proceeding to sending remote handles,
      // b/c some procs may have sent to a 3rd proc ents owned by me;
      PRINT_DEBUG_WAITANY(recv_ent_reqs, MB_MESG_ENTS_SIZE, procConfig.proc_rank());

      success = MPI_Waitany(3*buffProcs.size(), &recv_ent_reqs[0], &ind, &status);
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitany in ghost exchange");
      }

      PRINT_DEBUG_RECD(status);

      // OK, received something; decrement incoming counter
      incoming1--;
      bool done = false;

      // In case ind is for ack, we need index of one before it
      unsigned int base_ind = 3*(ind/3);
      result = recv_buffer(MB_MESG_ENTS_SIZE,
                           status,
                           remoteOwnedBuffs[ind/3],
                           recv_ent_reqs[base_ind + 1],
                           recv_ent_reqs[base_ind + 2],
                           incoming1,
                           localOwnedBuffs[ind/3],
                           sendReqs[base_ind + 1],
                           sendReqs[base_ind + 2],
                           done,
                           (!is_iface && store_remote_handles ?
                            localOwnedBuffs[ind/3] : NULL),
                           MB_MESG_REMOTEH_SIZE, // maybe base_ind+1?
                           &recv_remoteh_reqs[base_ind+1], &incoming2);MB_CHK_SET_ERR(result, "Failed to receive buffer");

      if (done) {
        if (myDebug->get_verbosity() == 4) {
          msgs.resize(msgs.size() + 1);
          msgs.back() = new Buffer(*remoteOwnedBuffs[ind/3]);
        }

        // Message completely received - process buffer that was sent
        remoteOwnedBuffs[ind/3]->reset_ptr(sizeof(int));
        result = unpack_entities(remoteOwnedBuffs[ind/3]->buff_ptr,
                                 store_remote_handles, ind/3, is_iface,
                                 L1hloc, L1hrem, L1p, L2hloc, L2hrem, L2p, new_ents);
        if (MB_SUCCESS != result) {
          std::cout << "Failed to unpack entities. Buffer contents:" << std::endl;
          print_buffer(remoteOwnedBuffs[ind/3]->mem_ptr, MB_MESG_ENTS_SIZE, buffProcs[ind/3], false);
          return result;
        }

        if (recv_ent_reqs.size() != 3*buffProcs.size()) {
          // Post irecv's for remote handles from new proc; shouldn't be iface,
          // since we know about all procs we share with
          assert(!is_iface);
          recv_remoteh_reqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);
          for (unsigned int i = recv_ent_reqs.size(); i < 3*buffProcs.size(); i += 3) {
            localOwnedBuffs[i/3]->reset_buffer();
            incoming2++;
            PRINT_DEBUG_IRECV(procConfig.proc_rank(), buffProcs[i/3],
                              localOwnedBuffs[i/3]->mem_ptr, INITIAL_BUFF_SIZE,
                              MB_MESG_REMOTEH_SIZE, incoming2);
            success = MPI_Irecv(localOwnedBuffs[i/3]->mem_ptr, INITIAL_BUFF_SIZE,
                                MPI_UNSIGNED_CHAR, buffProcs[i/3],
                                MB_MESG_REMOTEH_SIZE, procConfig.proc_comm(),
                                &recv_remoteh_reqs[i]);
            if (success != MPI_SUCCESS) {
              MB_SET_ERR(MB_FAILURE, "Failed to post irecv for remote handles in ghost exchange");
            }
          }
          recv_ent_reqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);
          sendReqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);
        }
      }
    }

    // Add requests for any new addl procs
    if (recv_ent_reqs.size() != 3*buffProcs.size()) {
      // Shouldn't get here...
      MB_SET_ERR(MB_FAILURE, "Requests length doesn't match proc count in ghost exchange");
    }

#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(ENTITIES_END, procConfig.proc_rank(), "Ending entity exchange.");
    }
#endif

    if (is_iface) {
      // Need to check over entities I sent and make sure I received
      // handles for them from all expected procs; if not, need to clean
      // them up
      result = check_clean_iface(allsent);
      if (MB_SUCCESS != result)
        std::cout << "Failed check." << std::endl;

      // Now set the shared/interface tag on non-vertex entities on interface
      result = tag_iface_entities();MB_CHK_SET_ERR(result, "Failed to tag iface entities");

#ifndef NDEBUG
      result = check_sent_ents(allsent);
      if (MB_SUCCESS != result) std::cout << "Failed check." << std::endl;
      result = check_all_shared_handles(true);
      if (MB_SUCCESS != result) std::cout << "Failed check." << std::endl;
#endif

#ifdef MOAB_HAVE_MPE
      if (myDebug->get_verbosity() == 2) {
        MPE_Log_event(IFACE_END, procConfig.proc_rank(), "Ending interface exchange.");
      }
#endif

      //===========================================
      // Wait if requested
      //===========================================
      if (wait_all) {
        if (myDebug->get_verbosity() == 5) {
          success = MPI_Barrier(procConfig.proc_comm());
        }
        else {
          MPI_Status mult_status[3*MAX_SHARING_PROCS];
          success = MPI_Waitall(3*buffProcs.size(), &recv_ent_reqs[0], mult_status);
          if (MPI_SUCCESS != success) {
            MB_SET_ERR(MB_FAILURE, "Failed in waitall in ghost exchange");
          }
          success = MPI_Waitall(3*buffProcs.size(), &sendReqs[0], mult_status);
          if (MPI_SUCCESS != success) {
            MB_SET_ERR(MB_FAILURE, "Failed in waitall in ghost exchange");
          }
          /*success = MPI_Waitall(3*buffProcs.size(), &recv_remoteh_reqs[0], mult_status);
          if (MPI_SUCCESS != success) {
            MB_SET_ERR(MB_FAILURE, "Failed in waitall in ghost exchange");
          }*/
        }
      }

      myDebug->tprintf(1, "Total number of shared entities = %lu.\n", (unsigned long)sharedEnts.size());
      myDebug->tprintf(1, "Exiting exchange_ghost_cells\n");

      return MB_SUCCESS;
    }

    // we still need to wait on sendReqs, if they are not fulfilled yet
    if (wait_all) {
      if (myDebug->get_verbosity() == 5) {
        success = MPI_Barrier(procConfig.proc_comm());
      }
      else {
        MPI_Status mult_status[3*MAX_SHARING_PROCS];
        success = MPI_Waitall(3*buffProcs.size(), &sendReqs[0], mult_status);
        if (MPI_SUCCESS != success) {
          MB_SET_ERR(MB_FAILURE, "Failed in waitall in ghost exchange");
        }
      }
    } 
    //===========================================
    // Send local handles for new ghosts to owner, then add
    // those to ghost list for that owner
    //===========================================
    for (p = 0, proc_it = buffProcs.begin(); 
         proc_it != buffProcs.end(); ++proc_it, p++) {

      // Reserve space on front for size and for initial buff size
      remoteOwnedBuffs[p]->reset_buffer(sizeof(int));

      result = pack_remote_handles(L1hloc[p], L1hrem[p], L1p[p], *proc_it,
                                   remoteOwnedBuffs[p]);MB_CHK_SET_ERR(result, "Failed to pack remote handles");
      remoteOwnedBuffs[p]->set_stored_size();

      if (myDebug->get_verbosity() == 4) {
        msgs.resize(msgs.size() + 1);
        msgs.back() = new Buffer(*remoteOwnedBuffs[p]);
      }
      result = send_buffer(buffProcs[p], remoteOwnedBuffs[p],
                           MB_MESG_REMOTEH_SIZE,
                           sendReqs[3*p],
                           recv_remoteh_reqs[3*p + 2],
                           &dum_ack_buff, incoming2);MB_CHK_SET_ERR(result, "Failed to send remote handles");
    }

    //===========================================
    // Process remote handles of my ghosteds
    //===========================================
    while (incoming2) {
      PRINT_DEBUG_WAITANY(recv_remoteh_reqs, MB_MESG_REMOTEH_SIZE, procConfig.proc_rank());
      success = MPI_Waitany(3*buffProcs.size(), &recv_remoteh_reqs[0], &ind, &status);
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitany in ghost exchange");
      }

      // OK, received something; decrement incoming counter
      incoming2--;

      PRINT_DEBUG_RECD(status);

      bool done = false;
      unsigned int base_ind = 3*(ind/3);
      result = recv_buffer(MB_MESG_REMOTEH_SIZE, status,
                           localOwnedBuffs[ind/3],
                           recv_remoteh_reqs[base_ind+1],
                           recv_remoteh_reqs[base_ind + 2], incoming2,
                           remoteOwnedBuffs[ind/3],
                           sendReqs[base_ind+1],
                           sendReqs[base_ind + 2],
                           done);MB_CHK_SET_ERR(result, "Failed to receive remote handles");
      if (done) {
        // Incoming remote handles
        if (myDebug->get_verbosity() == 4) {
          msgs.resize(msgs.size() + 1);
          msgs.back() = new Buffer(*localOwnedBuffs[ind/3]);
        }
        localOwnedBuffs[ind/3]->reset_ptr(sizeof(int));
        result = unpack_remote_handles(buffProcs[ind/3],
                                       localOwnedBuffs[ind/3]->buff_ptr,
                                       L2hloc, L2hrem, L2p);MB_CHK_SET_ERR(result, "Failed to unpack remote handles");
      }
    }

#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(RHANDLES_END, procConfig.proc_rank(), "Ending remote handles.");
      MPE_Log_event(GHOST_END, procConfig.proc_rank(),
                    "Ending ghost exchange (still doing checks).");
    }
#endif

    //===========================================
    // Wait if requested
    //===========================================
    if (wait_all) {
      if (myDebug->get_verbosity() == 5) {
        success = MPI_Barrier(procConfig.proc_comm());
      }
      else {
        MPI_Status mult_status[3*MAX_SHARING_PROCS];
        success = MPI_Waitall(3*buffProcs.size(), &recv_remoteh_reqs[0], mult_status);
        if (MPI_SUCCESS == success)
          success = MPI_Waitall(3*buffProcs.size(), &sendReqs[0], mult_status);
      }
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitall in ghost exchange");
      }
    }

#ifndef NDEBUG
    result = check_sent_ents(allsent);MB_CHK_SET_ERR(result, "Failed check on shared entities");
    result = check_all_shared_handles(true);MB_CHK_SET_ERR(result, "Failed check on all shared handles");
#endif

    if (file_set && !new_ents.empty()) {
      result = mbImpl->add_entities(*file_set, &new_ents[0], new_ents.size());MB_CHK_SET_ERR(result, "Failed to add new entities to set");
    }

    myDebug->tprintf(1, "Total number of shared entities = %lu.\n", (unsigned long)sharedEnts.size());
    myDebug->tprintf(1, "Exiting exchange_ghost_cells\n");

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::send_buffer(const unsigned int to_proc,
                                      Buffer *send_buff,
                                      int mesg_tag,
                                      MPI_Request &send_req,
                                      MPI_Request &ack_req,
                                      int *ack_buff,
                                      int &this_incoming,
                                      int next_mesg_tag,
                                      Buffer *next_recv_buff,
                                      MPI_Request *next_recv_req,
                                      int *next_incoming)
  {
    ErrorCode result = MB_SUCCESS;
    int success;

    // If small message, post recv for remote handle message
    if (send_buff->get_stored_size() <= (int)INITIAL_BUFF_SIZE && next_recv_buff) {
      (*next_incoming)++;
      PRINT_DEBUG_IRECV(procConfig.proc_rank(), to_proc, next_recv_buff->mem_ptr,
                        INITIAL_BUFF_SIZE, next_mesg_tag, *next_incoming);
      success = MPI_Irecv(next_recv_buff->mem_ptr, INITIAL_BUFF_SIZE, 
                          MPI_UNSIGNED_CHAR, to_proc,
                          next_mesg_tag, procConfig.proc_comm(), 
                          next_recv_req);
      if (success != MPI_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Failed to post irecv for next message in ghost exchange");
      }
    }
    // If large, we'll need an ack before sending the rest
    else if (send_buff->get_stored_size() > (int)INITIAL_BUFF_SIZE) {
      this_incoming++;
      PRINT_DEBUG_IRECV(procConfig.proc_rank(), to_proc, (unsigned char*)ack_buff,
                        sizeof(int), mesg_tag - 1, this_incoming);
      success = MPI_Irecv(ack_buff, sizeof(int),
                          MPI_UNSIGNED_CHAR, to_proc,
                          mesg_tag - 1, procConfig.proc_comm(),
                          &ack_req);
      if (success != MPI_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Failed to post irecv for entity ack in ghost exchange");
      }
    }

    // Send the buffer
    PRINT_DEBUG_ISEND(procConfig.proc_rank(), to_proc, send_buff->mem_ptr, mesg_tag,
                      std::min(send_buff->get_stored_size(), (int)INITIAL_BUFF_SIZE));
    assert(0 <= send_buff->get_stored_size() && 
           send_buff->get_stored_size() <= (int)send_buff->alloc_size);
    success = MPI_Isend(send_buff->mem_ptr, 
                        std::min(send_buff->get_stored_size(),
                                 (int)INITIAL_BUFF_SIZE),
                        MPI_UNSIGNED_CHAR, to_proc,
                        mesg_tag, procConfig.proc_comm(), &send_req);
    if (success != MPI_SUCCESS) return MB_FAILURE;

    return result;
  }

  ErrorCode ParallelComm::recv_buffer(int mesg_tag_expected,
                                      const MPI_Status &mpi_status,
                                      Buffer *recv_buff,
                                      MPI_Request &recv_req,
                                      MPI_Request & /*ack_recvd_req*/,
                                      int &this_incoming,
                                      Buffer *send_buff,
                                      MPI_Request &send_req,
                                      MPI_Request &sent_ack_req,
                                      bool &done,
                                      Buffer *next_buff,
                                      int next_tag,
                                      MPI_Request *next_req,
                                      int *next_incoming)
  {
    // Process a received message; if there will be more coming,
    // post a receive for 2nd part then send an ack message
    int from_proc = mpi_status.MPI_SOURCE;
    int success;

    // Set the buff_ptr on the recv_buffer; needs to point beyond any
    // valid data already in the buffer
    recv_buff->reset_ptr(std::min(recv_buff->get_stored_size(),
                                  (int)recv_buff->alloc_size));

    if (mpi_status.MPI_TAG == mesg_tag_expected &&
        recv_buff->get_stored_size() > (int)INITIAL_BUFF_SIZE) {
      // 1st message & large - allocate buffer, post irecv for 2nd message,
      // then send ack
      recv_buff->reserve(recv_buff->get_stored_size());
      assert(recv_buff->alloc_size > INITIAL_BUFF_SIZE);

      // Will expect a 2nd message
      this_incoming++;

      PRINT_DEBUG_IRECV(procConfig.proc_rank(), from_proc,
                        recv_buff->mem_ptr + INITIAL_BUFF_SIZE,
                        recv_buff->get_stored_size() - INITIAL_BUFF_SIZE,
                        mesg_tag_expected + 1, this_incoming);
      success = MPI_Irecv(recv_buff->mem_ptr + INITIAL_BUFF_SIZE,
                          recv_buff->get_stored_size() - INITIAL_BUFF_SIZE,
                          MPI_UNSIGNED_CHAR, from_proc,
                          mesg_tag_expected + 1, procConfig.proc_comm(),
                          &recv_req);
      if (success != MPI_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Failed to post 2nd iRecv in ghost exchange");
      }

      // Send ack, doesn't matter what data actually is
      PRINT_DEBUG_ISEND(procConfig.proc_rank(), from_proc, recv_buff->mem_ptr,
                        mesg_tag_expected - 1, sizeof(int));
      success = MPI_Isend(recv_buff->mem_ptr, sizeof(int),
                          MPI_UNSIGNED_CHAR, from_proc,
                          mesg_tag_expected - 1, procConfig.proc_comm(), &sent_ack_req);
      if (success != MPI_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Failed to send ack in ghost exchange");
      }
    }
    else if (mpi_status.MPI_TAG == mesg_tag_expected - 1) {
      // Got an ack back, send the 2nd half of message

      // Should be a large message if we got this
      assert(*((size_t*)send_buff->mem_ptr) > INITIAL_BUFF_SIZE);

      // Post irecv for next message, then send 2nd message
      if (next_buff) {
        // We'll expect a return message
        (*next_incoming)++;
        PRINT_DEBUG_IRECV(procConfig.proc_rank(), from_proc, next_buff->mem_ptr,
                          INITIAL_BUFF_SIZE, next_tag, *next_incoming);

        success = MPI_Irecv(next_buff->mem_ptr,
                            INITIAL_BUFF_SIZE,
                            MPI_UNSIGNED_CHAR, from_proc,
                            next_tag, procConfig.proc_comm(),
                            next_req);
        if (success != MPI_SUCCESS) {
          MB_SET_ERR(MB_FAILURE, "Failed to post next irecv in ghost exchange");
        }
      }

      // Send 2nd message
      PRINT_DEBUG_ISEND(procConfig.proc_rank(), from_proc,
                        send_buff->mem_ptr+INITIAL_BUFF_SIZE,
                        mesg_tag_expected + 1,
                        send_buff->get_stored_size() - INITIAL_BUFF_SIZE);
    
      assert(send_buff->get_stored_size()-INITIAL_BUFF_SIZE < send_buff->alloc_size &&
             0 <= send_buff->get_stored_size());
      success = MPI_Isend(send_buff->mem_ptr+INITIAL_BUFF_SIZE,
                          send_buff->get_stored_size() - INITIAL_BUFF_SIZE,
                          MPI_UNSIGNED_CHAR, from_proc, mesg_tag_expected + 1,
                          procConfig.proc_comm(), &send_req);
      if (success != MPI_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Failed to send 2nd message in ghost exchange");
      }
    }
    else if ((mpi_status.MPI_TAG == mesg_tag_expected &&
              recv_buff->get_stored_size() <= (int)INITIAL_BUFF_SIZE) ||
              mpi_status.MPI_TAG == mesg_tag_expected + 1) {
      // Message completely received - signal that we're done
      done = true;
    }

    return MB_SUCCESS;
  }

  struct ProcList {
    int procs[MAX_SHARING_PROCS];
  };
  static bool operator<(const ProcList& a, const ProcList& b) {
    for (int i = 0; i < MAX_SHARING_PROCS; i++) {
      if (a.procs[i] < b.procs[i]) 
        return true;
      else if (b.procs[i] < a.procs[i])
        return false;
      else if (a.procs[i] < 0)
        return false; 
    }
    return false;
  }

  ErrorCode ParallelComm::check_clean_iface(Range &allsent) 
  {
    // allsent is all entities I think are on interface; go over them, looking
    // for zero-valued handles, and fix any I find

    // Keep lists of entities for which teh sharing data changed, grouped
    // by set of sharing procs.
    typedef std::map< ProcList, Range > procmap_t;
    procmap_t old_procs, new_procs;

    ErrorCode result = MB_SUCCESS;
    Range::iterator rit;
    Range::reverse_iterator rvit;
    unsigned char pstatus;
    int nump;
    ProcList sharedp;
    EntityHandle sharedh[MAX_SHARING_PROCS];
    for (rvit = allsent.rbegin(); rvit != allsent.rend(); ++rvit) {
      result = get_sharing_data(*rvit, sharedp.procs, sharedh, pstatus, nump);MB_CHK_SET_ERR(result, "Failed to get sharing data");
      assert("Should be shared with at least one other proc" && 
             (nump > 1 || sharedp.procs[0] != (int)procConfig.proc_rank()));
      assert(nump == MAX_SHARING_PROCS || sharedp.procs[nump] == -1);

      // Look for first null handle in list
      int idx = std::find(sharedh, sharedh + nump, (EntityHandle)0) - sharedh;
      if (idx == nump)
        continue; // All handles are valid

      ProcList old_list(sharedp);
      std::sort(old_list.procs, old_list.procs + nump);
      old_procs[old_list].insert(*rvit);

      // Remove null handles and corresponding proc ranks from lists
      int new_nump = idx;
      bool removed_owner = !idx;
      for (++idx; idx < nump; ++idx) {
        if (sharedh[idx]) {
          sharedh[new_nump] = sharedh[idx];
          sharedp.procs[new_nump] = sharedp.procs[idx];
          ++new_nump;
        }
      }
      sharedp.procs[new_nump] = -1;

      if (removed_owner && new_nump > 1) {
        // The proc that we choose as the entity owner isn't sharing the
        // entity (doesn't have a copy of it). We need to pick a different
        // owner. Choose the proc with lowest rank.
        idx = std::min_element(sharedp.procs, sharedp.procs + new_nump) - sharedp.procs;
        std::swap(sharedp.procs[0], sharedp.procs[idx]);
        std::swap(sharedh[0], sharedh[idx]);
        if (sharedp.procs[0] == (int)proc_config().proc_rank())
          pstatus &= ~PSTATUS_NOT_OWNED;
      }

      result = set_sharing_data(*rvit, pstatus, nump, new_nump, sharedp.procs, sharedh);MB_CHK_SET_ERR(result, "Failed to set sharing data in check_clean_iface");

      if (new_nump > 1) {
        if (new_nump == 2) {
          if (sharedp.procs[1] != (int)proc_config().proc_rank()) {
            assert(sharedp.procs[0] == (int)proc_config().proc_rank());
            sharedp.procs[0] = sharedp.procs[1];
          }
          sharedp.procs[1] = -1;
        }
        else {
          std::sort(sharedp.procs, sharedp.procs + new_nump);
        }
        new_procs[sharedp].insert(*rvit);
      }
    }

    if (old_procs.empty()) {
      assert(new_procs.empty());
      return MB_SUCCESS;
    }

    // Update interface sets
    procmap_t::iterator pmit;
    //std::vector<unsigned char> pstatus_list;
    rit = interface_sets().begin();
    while (rit != interface_sets().end()) {
      result = get_sharing_data(*rit, sharedp.procs, sharedh, pstatus, nump);MB_CHK_SET_ERR(result, "Failed to get sharing data for interface set");
      assert(nump != 2);
      std::sort(sharedp.procs, sharedp.procs + nump);
      assert(nump == MAX_SHARING_PROCS || sharedp.procs[nump] == -1);
    
      pmit = old_procs.find(sharedp);
      if (pmit != old_procs.end()) {
        result = mbImpl->remove_entities(*rit, pmit->second);MB_CHK_SET_ERR(result, "Failed to remove entities from interface set");
      }

      pmit = new_procs.find(sharedp);
      if (pmit == new_procs.end()) {
        int count;
        result = mbImpl->get_number_entities_by_handle(*rit, count);MB_CHK_SET_ERR(result, "Failed to get number of entities in interface set");
        if (!count) {
          result = mbImpl->delete_entities(&*rit, 1);MB_CHK_SET_ERR(result, "Failed to delete entities from interface set");
          rit = interface_sets().erase(rit);
        }
        else {
          ++rit;
        }
      }
      else {
        result = mbImpl->add_entities(*rit, pmit->second);MB_CHK_SET_ERR(result, "Failed to add entities to interface set");

        // Remove those that we've processed so that we know which ones
        // are new.
        new_procs.erase(pmit);
        ++rit;
      }
    }

    // Create interface sets for new proc id combinations
    std::fill(sharedh, sharedh + MAX_SHARING_PROCS, 0);
    for (pmit = new_procs.begin(); pmit != new_procs.end(); ++pmit) {
      EntityHandle new_set;
      result = mbImpl->create_meshset(MESHSET_SET, new_set);MB_CHK_SET_ERR(result, "Failed to create interface set");
      interfaceSets.insert(new_set);

      // Add entities
      result = mbImpl->add_entities(new_set, pmit->second);MB_CHK_SET_ERR(result, "Failed to add entities to interface set");
      // Tag set with the proc rank(s)
      assert(pmit->first.procs[0] >= 0);
      pstatus = PSTATUS_SHARED|PSTATUS_INTERFACE;
      if (pmit->first.procs[1] == -1) {
        int other = pmit->first.procs[0];
        assert(other != (int)procConfig.proc_rank());
        result = mbImpl->tag_set_data(sharedp_tag(), &new_set, 1, pmit->first.procs);MB_CHK_SET_ERR(result, "Failed to tag interface set with procs");
        sharedh[0] = 0;
        result = mbImpl->tag_set_data(sharedh_tag(), &new_set, 1, sharedh);MB_CHK_SET_ERR(result, "Failed to tag interface set with procs");
        if (other < (int)proc_config().proc_rank())
          pstatus |= PSTATUS_NOT_OWNED;
      }
      else {
        result = mbImpl->tag_set_data(sharedps_tag(), &new_set, 1, pmit->first.procs);MB_CHK_SET_ERR(result, "Failed to tag interface set with procs");
        result = mbImpl->tag_set_data(sharedhs_tag(), &new_set, 1, sharedh);MB_CHK_SET_ERR(result, "Failed to tag interface set with procs");
        pstatus |= PSTATUS_MULTISHARED;
        if (pmit->first.procs[0] < (int)proc_config().proc_rank())
          pstatus |= PSTATUS_NOT_OWNED;
      }

      result = mbImpl->tag_set_data(pstatus_tag(), &new_set, 1, &pstatus);MB_CHK_SET_ERR(result, "Failed to tag interface set with pstatus");

      // Set pstatus on all interface entities in set
      result = mbImpl->tag_clear_data(pstatus_tag(), pmit->second, &pstatus);MB_CHK_SET_ERR(result, "Failed to tag interface entities with pstatus");
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::set_sharing_data(EntityHandle ent, unsigned char pstatus,
                                           int old_nump, int new_nump,
                                           int *ps, EntityHandle *hs)
  {
    // If new nump is less than 3, the entity is no longer mutishared
     if (old_nump > 2 && (pstatus & PSTATUS_MULTISHARED) && new_nump < 3) {
       // Unset multishared flag
       pstatus ^= PSTATUS_MULTISHARED;
     }

    // Check for consistency in input data
     //DBG
   /*  bool con1 = ((new_nump == 2 && pstatus&PSTATUS_SHARED && !(pstatus&PSTATUS_MULTISHARED)) || (new_nump > 2 && pstatus&PSTATUS_SHARED && pstatus&PSTATUS_MULTISHARED));
     bool con2 = (!(pstatus&PSTATUS_GHOST) || pstatus&PSTATUS_SHARED);
     bool con3 = (new_nump < 3 || (pstatus&PSTATUS_NOT_OWNED && ps[0] != (int)rank()) || (!(pstatus&PSTATUS_NOT_OWNED) && ps[0] == (int)rank()));
     std::cout<<"current rank = "<<rank()<<std::endl;
     std::cout<<"condition 1::"<<con1<<std::endl;
     std::cout<<"condition 2::"<<con2<<std::endl;
     std::cout<<"condition 3::"<<con3<<std::endl;*/

     //DBG

    assert(new_nump > 1 &&
           ((new_nump == 2 && pstatus&PSTATUS_SHARED && !(pstatus&PSTATUS_MULTISHARED)) || // If <= 2 must not be multishared
            (new_nump > 2 && pstatus&PSTATUS_SHARED && pstatus&PSTATUS_MULTISHARED)) && // If > 2 procs, must be multishared
           (!(pstatus&PSTATUS_GHOST) || pstatus&PSTATUS_SHARED) && // If ghost, it must also be shared
           (new_nump < 3 || (pstatus&PSTATUS_NOT_OWNED && ps[0] != (int)rank()) || // I'm not owner and first proc not me
            (!(pstatus&PSTATUS_NOT_OWNED) && ps[0] == (int)rank())) // I'm owner and first proc is me
           );

#ifndef NDEBUG
      {
        // Check for duplicates in proc list
        std::set<unsigned int> dumprocs;
        int dp = 0;
        for (; dp < old_nump && -1 != ps[dp]; dp++)
          dumprocs.insert(ps[dp]);
        assert(dp == (int)dumprocs.size());
      }
#endif

    ErrorCode result;
    // Reset any old data that needs to be
    if (old_nump > 2 && new_nump < 3) {
      // Need to remove multishared tags
      result = mbImpl->tag_delete_data(sharedps_tag(), &ent, 1);MB_CHK_SET_ERR(result, "set_sharing_data:1");
      result = mbImpl->tag_delete_data(sharedhs_tag(), &ent, 1);MB_CHK_SET_ERR(result, "set_sharing_data:2");
//    if (new_nump < 2)
//      pstatus = 0x0;
//    else if (ps[0] != (int)proc_config().proc_rank())
//      pstatus |= PSTATUS_NOT_OWNED;
    }
    else if ((old_nump < 3 && new_nump > 2) || (old_nump > 1 && new_nump == 1)) {
      // Reset sharedp and sharedh tags
      int tmp_p = -1;
      EntityHandle tmp_h = 0;
      result = mbImpl->tag_set_data(sharedp_tag(), &ent, 1, &tmp_p);MB_CHK_SET_ERR(result, "set_sharing_data:3");
      result = mbImpl->tag_set_data(sharedh_tag(), &ent, 1, &tmp_h);MB_CHK_SET_ERR(result, "set_sharing_data:4");
    }

    assert("check for multishared/owner I'm first proc" &&
           (!(pstatus & PSTATUS_MULTISHARED) || (pstatus & (PSTATUS_NOT_OWNED|PSTATUS_GHOST)) || (ps[0] == (int)rank())) &&
           "interface entities should have > 1 proc" &&
           (!(pstatus & PSTATUS_INTERFACE) || new_nump > 1) &&
           "ghost entities should have > 1 proc" &&
           (!(pstatus & PSTATUS_GHOST) || new_nump > 1)
           );

    // Now set new data
    if (new_nump > 2) {
      result = mbImpl->tag_set_data(sharedps_tag(), &ent, 1, ps);MB_CHK_SET_ERR(result, "set_sharing_data:5");
      result = mbImpl->tag_set_data(sharedhs_tag(), &ent, 1, hs);MB_CHK_SET_ERR(result, "set_sharing_data:6");
    }
    else {
      unsigned int j = (ps[0] == (int)procConfig.proc_rank() ? 1 : 0);
      assert(-1 != ps[j]);
      result = mbImpl->tag_set_data(sharedp_tag(), &ent, 1, ps + j);MB_CHK_SET_ERR(result, "set_sharing_data:7");
      result = mbImpl->tag_set_data(sharedh_tag(), &ent, 1, hs + j);MB_CHK_SET_ERR(result, "set_sharing_data:8");
    }

    result = mbImpl->tag_set_data(pstatus_tag(), &ent, 1, &pstatus);MB_CHK_SET_ERR(result, "set_sharing_data:9");

    if (old_nump > 1 && new_nump < 2)
      sharedEnts.erase(std::find(sharedEnts.begin(), sharedEnts.end(), ent));

    return result;
  }

  ErrorCode ParallelComm::get_sent_ents(const bool is_iface,
                                        const int bridge_dim, const int ghost_dim,
                                        const int num_layers, const int addl_ents,
                                        Range *sent_ents, Range &allsent,
                                        TupleList &entprocs)
  {
    ErrorCode result;
    unsigned int ind;
    std::vector<unsigned int>::iterator proc_it;
    Range tmp_range;

    // Done in a separate loop over procs because sometimes later procs
    // need to add info to earlier procs' messages
    for (ind = 0, proc_it = buffProcs.begin(); 
         proc_it != buffProcs.end(); ++proc_it, ind++) {
      if (!is_iface) {
        result = get_ghosted_entities(bridge_dim, ghost_dim, buffProcs[ind],
                                      num_layers, addl_ents, sent_ents[ind]);MB_CHK_SET_ERR(result, "Failed to get ghost layers");
      }
      else {
        result = get_iface_entities(buffProcs[ind], -1, sent_ents[ind]);MB_CHK_SET_ERR(result, "Failed to get interface layers");
      }

      // Filter out entities already shared with destination
      tmp_range.clear();
      result = filter_pstatus(sent_ents[ind], PSTATUS_SHARED, PSTATUS_AND,
                              buffProcs[ind], &tmp_range);MB_CHK_SET_ERR(result, "Failed to filter on owner");
      if (!tmp_range.empty())
        sent_ents[ind] = subtract(sent_ents[ind], tmp_range);

      allsent.merge(sent_ents[ind]);
    }

    //===========================================
    // Need to get procs each entity is sent to
    //===========================================

    // Get the total # of proc/handle pairs
    int npairs = 0;
    for (ind = 0; ind < buffProcs.size(); ind++)
      npairs += sent_ents[ind].size();

    // Allocate a TupleList of that size
    entprocs.initialize(1, 0, 1, 0, npairs);
    entprocs.enableWriteAccess();

    // Put the proc/handle pairs in the list
    for (ind = 0, proc_it = buffProcs.begin(); 
         proc_it != buffProcs.end(); ++proc_it, ind++) {
      for (Range::iterator rit = sent_ents[ind].begin();
          rit != sent_ents[ind].end(); ++rit) {
        entprocs.vi_wr[entprocs.get_n()] = *proc_it;
        entprocs.vul_wr[entprocs.get_n()] = *rit;
        entprocs.inc_n();
      }
    }
    // Sort by handle
    moab::TupleList::buffer sort_buffer;
    sort_buffer.buffer_init(npairs);
    entprocs.sort(1, &sort_buffer);

    entprocs.disableWriteAccess();
    sort_buffer.reset();

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::exchange_ghost_cells(ParallelComm **pcs,
                                               unsigned int num_procs,
                                               int ghost_dim, int bridge_dim,
                                               int num_layers, int addl_ents,
                                               bool store_remote_handles,
                                               EntityHandle *file_sets)
  {
    // Static version of function, exchanging info through buffers rather
    // than through messages

    // If we're only finding out about existing ents, we have to be storing
    // remote handles too
    assert(num_layers > 0 || store_remote_handles);

    const bool is_iface = !num_layers;

    unsigned int ind;
    ParallelComm *pc;
    ErrorCode result = MB_SUCCESS;

    std::vector<Error*> ehs(num_procs);
    for (unsigned int i = 0; i < num_procs; i++) {
      result = pcs[i]->get_moab()->query_interface(ehs[i]);
      assert (MB_SUCCESS == result);
    }

    // When this function is called, buffProcs should already have any
    // communicating procs

    //===========================================
    // Get entities to be sent to neighbors
    //===========================================

    // Done in a separate loop over procs because sometimes later procs
    // need to add info to earlier procs' messages
    Range sent_ents[MAX_SHARING_PROCS][MAX_SHARING_PROCS],
      allsent[MAX_SHARING_PROCS];

    //===========================================
    // Get entities to be sent to neighbors
    //===========================================
    TupleList entprocs[MAX_SHARING_PROCS];
    for (unsigned int p = 0; p < num_procs; p++) {
      pc = pcs[p];
      result = pc->get_sent_ents(is_iface, bridge_dim, ghost_dim, num_layers, addl_ents,
                                 sent_ents[p], allsent[p], entprocs[p]);MB_CHK_SET_ERR(result, "p = " << p << ", get_sent_ents failed");
  
      //===========================================
      // Pack entities into buffers
      //===========================================
      for (ind = 0; ind < pc->buffProcs.size(); ind++) {
        // Entities
        pc->localOwnedBuffs[ind]->reset_ptr(sizeof(int));
        result = pc->pack_entities(sent_ents[p][ind], pc->localOwnedBuffs[ind],
                                   store_remote_handles, pc->buffProcs[ind], is_iface,
                                   &entprocs[p], &allsent[p]);MB_CHK_SET_ERR(result, "p = " << p << ", packing entities failed");
      }

      entprocs[p].reset();
    }

    //===========================================
    // Receive/unpack new entities
    //===========================================
    // Number of incoming messages for ghosts is the number of procs we
    // communicate with; for iface, it's the number of those with lower rank
    std::vector<std::vector<EntityHandle> > L1hloc[MAX_SHARING_PROCS], L1hrem[MAX_SHARING_PROCS];
    std::vector<std::vector<int> > L1p[MAX_SHARING_PROCS];
    std::vector<EntityHandle> L2hloc[MAX_SHARING_PROCS], L2hrem[MAX_SHARING_PROCS];
    std::vector<unsigned int> L2p[MAX_SHARING_PROCS];
    std::vector<EntityHandle> new_ents[MAX_SHARING_PROCS];

    for (unsigned int p = 0; p < num_procs; p++) {
      L1hloc[p].resize(pcs[p]->buffProcs.size());
      L1hrem[p].resize(pcs[p]->buffProcs.size());
      L1p[p].resize(pcs[p]->buffProcs.size());
    }

    for (unsigned int p = 0; p < num_procs; p++) {
      pc = pcs[p];

      for (ind = 0; ind < pc->buffProcs.size(); ind++) {
        // Incoming ghost entities; unpack; returns entities received
        // both from sending proc and from owning proc (which may be different)

        // Buffer could be empty, which means there isn't any message to
        // unpack (due to this comm proc getting added as a result of indirect
        // communication); just skip this unpack
        if (pc->localOwnedBuffs[ind]->get_stored_size() == 0)
          continue;

        unsigned int to_p = pc->buffProcs[ind];
        pc->localOwnedBuffs[ind]->reset_ptr(sizeof(int));
        result = pcs[to_p]->unpack_entities(pc->localOwnedBuffs[ind]->buff_ptr,
                                            store_remote_handles, ind, is_iface,
                                            L1hloc[to_p], L1hrem[to_p], L1p[to_p], L2hloc[to_p],
                                            L2hrem[to_p], L2p[to_p], new_ents[to_p]);MB_CHK_SET_ERR(result, "p = " << p << ", failed to unpack entities");
      }
    }

    if (is_iface) {
      // Need to check over entities I sent and make sure I received
      // handles for them from all expected procs; if not, need to clean
      // them up
      for (unsigned int p = 0; p < num_procs; p++) {
        result = pcs[p]->check_clean_iface(allsent[p]);MB_CHK_SET_ERR(result, "p = " << p << ", failed to check on shared entities");
      }

#ifndef NDEBUG
      for (unsigned int p = 0; p < num_procs; p++) {
        result = pcs[p]->check_sent_ents(allsent[p]);MB_CHK_SET_ERR(result, "p = " << p << ", failed to check on shared entities");
      }
      result = check_all_shared_handles(pcs, num_procs);MB_CHK_SET_ERR(result, "Failed to check on all shared handles");
#endif
      return MB_SUCCESS;
    }

    //===========================================
    // Send local handles for new ghosts to owner, then add
    // those to ghost list for that owner
    //===========================================
    std::vector<unsigned int>::iterator proc_it;
    for (unsigned int p = 0; p < num_procs; p++) {
      pc = pcs[p];

      for (ind = 0, proc_it = pc->buffProcs.begin(); 
           proc_it != pc->buffProcs.end(); ++proc_it, ind++) {
        // Skip if iface layer and higher-rank proc
        pc->localOwnedBuffs[ind]->reset_ptr(sizeof(int));
        result = pc->pack_remote_handles(L1hloc[p][ind], L1hrem[p][ind], L1p[p][ind], *proc_it,
                                         pc->localOwnedBuffs[ind]);MB_CHK_SET_ERR(result, "p = " << p << ", failed to pack remote handles");
      }
    }

    //===========================================
    // Process remote handles of my ghosteds
    //===========================================
    for (unsigned int p = 0; p < num_procs; p++) {
      pc = pcs[p];

      for (ind = 0, proc_it = pc->buffProcs.begin(); 
           proc_it != pc->buffProcs.end(); ++proc_it, ind++) {
        // Incoming remote handles
        unsigned int to_p = pc->buffProcs[ind];
        pc->localOwnedBuffs[ind]->reset_ptr(sizeof(int));
        result = pcs[to_p]->unpack_remote_handles(p,
                                                  pc->localOwnedBuffs[ind]->buff_ptr,
                                                  L2hloc[to_p], L2hrem[to_p], L2p[to_p]);MB_CHK_SET_ERR(result, "p = " << p << ", failed to unpack remote handles");
      }
    }

#ifndef NDEBUG
    for (unsigned int p = 0; p < num_procs; p++) {
      result = pcs[p]->check_sent_ents(allsent[p]);MB_CHK_SET_ERR(result, "p = " << p << ", failed to check on shared entities");
    }

    result = ParallelComm::check_all_shared_handles(pcs, num_procs);MB_CHK_SET_ERR(result, "Failed to check on all shared handles");
#endif

    if (file_sets) {
      for (unsigned int p = 0; p < num_procs; p++) {
        if (new_ents[p].empty())
          continue;
        result = pcs[p]->get_moab()->add_entities(file_sets[p], &new_ents[p][0], new_ents[p].size());MB_CHK_SET_ERR(result, "p = " << p << ", failed to add new entities to set");
      }
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::post_irecv(std::vector<unsigned int>& exchange_procs)
  {
    // Set buffers
    int n_proc = exchange_procs.size();
    for (int i = 0; i < n_proc; i++)
      get_buffers(exchange_procs[i]);
    reset_all_buffers();

    // Post ghost irecv's for entities from all communicating procs
    // Index requests the same as buffer/sharing procs indices
    int success;
    recvReqs.resize(2*buffProcs.size(), MPI_REQUEST_NULL);
    recvRemotehReqs.resize(2*buffProcs.size(), MPI_REQUEST_NULL);
    sendReqs.resize(2*buffProcs.size(), MPI_REQUEST_NULL);

    int incoming = 0;
    for (int i = 0; i < n_proc; i++) {
      int ind = get_buffers(exchange_procs[i]);
      incoming++;
      PRINT_DEBUG_IRECV(procConfig.proc_rank(), buffProcs[ind],
                        remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
                        MB_MESG_ENTS_SIZE, incoming);
      success = MPI_Irecv(remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
                          MPI_UNSIGNED_CHAR, buffProcs[ind],
                          MB_MESG_ENTS_SIZE, procConfig.proc_comm(),
                          &recvReqs[2*ind]);
      if (success != MPI_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Failed to post irecv in owned entity exchange");
      }
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::post_irecv(std::vector<unsigned int>& shared_procs,
                                     std::set<unsigned int>& recv_procs)
  {
    // Set buffers
    int num = shared_procs.size();
    for (int i = 0; i < num; i++)
      get_buffers(shared_procs[i]);
    reset_all_buffers();
    num = remoteOwnedBuffs.size();
    for (int i = 0; i < num; i++)
      remoteOwnedBuffs[i]->set_stored_size();
    num = localOwnedBuffs.size();
    for (int i = 0; i < num; i++)
      localOwnedBuffs[i]->set_stored_size();

    // Post ghost irecv's for entities from all communicating procs
    // Index requests the same as buffer/sharing procs indices
    int success;
    recvReqs.resize(2*buffProcs.size(), MPI_REQUEST_NULL);
    recvRemotehReqs.resize(2*buffProcs.size(), MPI_REQUEST_NULL);
    sendReqs.resize(2*buffProcs.size(), MPI_REQUEST_NULL);

    int incoming = 0;
    std::set<unsigned int>::iterator it = recv_procs.begin();
    std::set<unsigned int>::iterator eit = recv_procs.end();
    for (; it != eit; ++it) {
      int ind = get_buffers(*it);
      incoming++;
      PRINT_DEBUG_IRECV(procConfig.proc_rank(), buffProcs[ind],
                        remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
                        MB_MESG_ENTS_SIZE, incoming);
      success = MPI_Irecv(remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
                          MPI_UNSIGNED_CHAR, buffProcs[ind],
                          MB_MESG_ENTS_SIZE, procConfig.proc_comm(),
                          &recvReqs[2*ind]);
      if (success != MPI_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Failed to post irecv in owned entity exchange");
      }
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::exchange_owned_meshs(std::vector<unsigned int>& exchange_procs,
                                               std::vector<Range*>& exchange_ents,
                                               std::vector<MPI_Request>& recv_ent_reqs,
                                               std::vector<MPI_Request>& recv_remoteh_reqs,
                                               bool store_remote_handles,
                                               bool wait_all,
                                               bool migrate,
                                               int dim)
  {
    // Filter out entities already shared with destination
    // Exchange twice for entities and sets
    ErrorCode result;
    std::vector<unsigned int> exchange_procs_sets;
    std::vector<Range*> exchange_sets;
    int n_proc = exchange_procs.size();
    for (int i = 0; i < n_proc; i++) {
      Range set_range = exchange_ents[i]->subset_by_type(MBENTITYSET);
      *exchange_ents[i] = subtract(*exchange_ents[i], set_range);
      Range* tmp_range = new Range(set_range);
      exchange_sets.push_back(tmp_range);
      exchange_procs_sets.push_back(exchange_procs[i]);
    }

    if (dim == 2) {
      // Exchange entities first
      result = exchange_owned_mesh(exchange_procs, exchange_ents,
                                   recvReqs, recvRemotehReqs, true,
                                   store_remote_handles, wait_all, migrate);MB_CHK_SET_ERR(result, "Failed to exchange owned mesh entities");

      // Exchange sets
      result = exchange_owned_mesh(exchange_procs_sets, exchange_sets,
                                   recvReqs, recvRemotehReqs, false,
                                   store_remote_handles, wait_all, migrate);
    }
    else {
      // Exchange entities first
      result = exchange_owned_mesh(exchange_procs, exchange_ents,
                                   recv_ent_reqs, recv_remoteh_reqs, false,
                                   store_remote_handles, wait_all, migrate);MB_CHK_SET_ERR(result, "Failed to exchange owned mesh entities");

      // Exchange sets
      result = exchange_owned_mesh(exchange_procs_sets, exchange_sets,
                                   recv_ent_reqs, recv_remoteh_reqs, false,
                                   store_remote_handles, wait_all, migrate);MB_CHK_SET_ERR(result, "Failed to exchange owned mesh sets");
    }

    for (int i = 0; i < n_proc; i++)
      delete exchange_sets[i];

    // Build up the list of shared entities
    std::map<std::vector<int>, std::vector<EntityHandle> > proc_nvecs;
    int procs[MAX_SHARING_PROCS];
    EntityHandle handles[MAX_SHARING_PROCS];
    int nprocs;
    unsigned char pstat;
    for (std::vector<EntityHandle>::iterator vit = sharedEnts.begin(); vit != sharedEnts.end(); ++vit) {
      if (mbImpl->dimension_from_handle(*vit) > 2)
        continue;
      result = get_sharing_data(*vit, procs, handles, pstat, nprocs);MB_CHK_SET_ERR(result, "Failed to get sharing data in exchange_owned_meshs");
      std::sort(procs, procs + nprocs);
      std::vector<int> tmp_procs(procs, procs + nprocs);
      assert(tmp_procs.size() != 2);
      proc_nvecs[tmp_procs].push_back(*vit);
    }

    // Create interface sets from shared entities
    result = create_interface_sets(proc_nvecs);MB_CHK_SET_ERR(result, "Failed to create interface sets");

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::exchange_owned_mesh(std::vector<unsigned int>& exchange_procs,
                                              std::vector<Range*>& exchange_ents,
                                              std::vector<MPI_Request>& recv_ent_reqs,
                                              std::vector<MPI_Request>& recv_remoteh_reqs,
                                              const bool recv_posted,
                                              bool store_remote_handles,
                                              bool wait_all,
                                              bool migrate)
  {
#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(OWNED_START, procConfig.proc_rank(), "Starting owned ents exchange.");
    }
#endif

    myDebug->tprintf(1, "Entering exchange_owned_mesh\n");
    if (myDebug->get_verbosity() == 4) {
      msgs.clear();
      msgs.reserve(MAX_SHARING_PROCS);
    }
    unsigned int i;
    int ind, success;
    ErrorCode result = MB_SUCCESS;
    int incoming1 = 0, incoming2 = 0;

    // Set buffProcs with communicating procs
    unsigned int n_proc = exchange_procs.size();
    for (i = 0; i < n_proc; i++) {
      ind = get_buffers(exchange_procs[i]);
      result = add_verts(*exchange_ents[i]);MB_CHK_SET_ERR(result, "Failed to add verts");

      // Filter out entities already shared with destination
      Range tmp_range;
      result = filter_pstatus(*exchange_ents[i], PSTATUS_SHARED, PSTATUS_AND,
                              buffProcs[ind], &tmp_range);MB_CHK_SET_ERR(result, "Failed to filter on owner");
      if (!tmp_range.empty()) {
        *exchange_ents[i] = subtract(*exchange_ents[i], tmp_range);
      }
    }

    //===========================================
    // Post ghost irecv's for entities from all communicating procs
    //===========================================
#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(ENTITIES_START, procConfig.proc_rank(), "Starting entity exchange.");
    }
#endif

    // Index reqs the same as buffer/sharing procs indices
    if (!recv_posted) {
      reset_all_buffers();
      recv_ent_reqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);
      recv_remoteh_reqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);
      sendReqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);

      for (i = 0; i < n_proc; i++) {
        ind = get_buffers(exchange_procs[i]);
        incoming1++;
        PRINT_DEBUG_IRECV(procConfig.proc_rank(), buffProcs[ind],
                          remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
                          MB_MESG_ENTS_SIZE, incoming1);
        success = MPI_Irecv(remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
                            MPI_UNSIGNED_CHAR, buffProcs[ind],
                            MB_MESG_ENTS_SIZE, procConfig.proc_comm(),
                            &recv_ent_reqs[3*ind]);
        if (success != MPI_SUCCESS) {
          MB_SET_ERR(MB_FAILURE, "Failed to post irecv in owned entity exchange");
        }
      }
    }
    else
      incoming1 += n_proc;

    //===========================================
    // Get entities to be sent to neighbors
    // Need to get procs each entity is sent to
    //===========================================  
    Range allsent, tmp_range;
    int dum_ack_buff;
    int npairs = 0;
    TupleList entprocs;
    for (i = 0; i < n_proc; i++) {
      int n_ents = exchange_ents[i]->size();
      if (n_ents > 0) {
        npairs += n_ents; // Get the total # of proc/handle pairs
        allsent.merge(*exchange_ents[i]);
      }
    }

    // Allocate a TupleList of that size
    entprocs.initialize(1, 0, 1, 0, npairs);
    entprocs.enableWriteAccess();

    // Put the proc/handle pairs in the list
    for (i = 0; i < n_proc; i++) {
      for (Range::iterator rit = exchange_ents[i]->begin(); rit != exchange_ents[i]->end(); ++rit) {
        entprocs.vi_wr[entprocs.get_n()] = exchange_procs[i];
        entprocs.vul_wr[entprocs.get_n()] = *rit;
        entprocs.inc_n();
      }
    }

    // Sort by handle
    moab::TupleList::buffer sort_buffer;
    sort_buffer.buffer_init(npairs);
    entprocs.sort(1, &sort_buffer);
    sort_buffer.reset();

    myDebug->tprintf(1, "allsent ents compactness (size) = %f (%lu)\n", allsent.compactness(),
                     (unsigned long)allsent.size());

    //===========================================
    // Pack and send ents from this proc to others
    //===========================================
    for (i = 0; i < n_proc; i++) {
      ind = get_buffers(exchange_procs[i]);
      myDebug->tprintf(1, "Sent ents compactness (size) = %f (%lu)\n", exchange_ents[i]->compactness(),
                       (unsigned long)exchange_ents[i]->size());
      // Reserve space on front for size and for initial buff size
      localOwnedBuffs[ind]->reset_buffer(sizeof(int));
      result = pack_buffer(*exchange_ents[i], false, true,
                           store_remote_handles, buffProcs[ind],
                           localOwnedBuffs[ind], &entprocs, &allsent);

      if (myDebug->get_verbosity() == 4) {
        msgs.resize(msgs.size() + 1);
        msgs.back() = new Buffer(*localOwnedBuffs[ind]);
      }

      // Send the buffer (size stored in front in send_buffer)
      result = send_buffer(exchange_procs[i], localOwnedBuffs[ind],
                           MB_MESG_ENTS_SIZE, sendReqs[3*ind],
                           recv_ent_reqs[3*ind + 2], &dum_ack_buff,
                           incoming1,
                           MB_MESG_REMOTEH_SIZE,
                           (store_remote_handles ?
                           localOwnedBuffs[ind] : NULL),
                           &recv_remoteh_reqs[3*ind], &incoming2);MB_CHK_SET_ERR(result, "Failed to Isend in ghost exchange");
    }

    entprocs.reset();

    //===========================================
    // Receive/unpack new entities
    //===========================================
    // Number of incoming messages is the number of procs we communicate with
    MPI_Status status;
    std::vector<std::vector<EntityHandle> > recd_ents(buffProcs.size());
    std::vector<std::vector<EntityHandle> > L1hloc(buffProcs.size()), L1hrem(buffProcs.size());
    std::vector<std::vector<int> > L1p(buffProcs.size());
    std::vector<EntityHandle> L2hloc, L2hrem;
    std::vector<unsigned int> L2p;
    std::vector<EntityHandle> new_ents;

    while (incoming1) {
      // Wait for all recvs of ents before proceeding to sending remote handles,
      // b/c some procs may have sent to a 3rd proc ents owned by me;
      PRINT_DEBUG_WAITANY(recv_ent_reqs, MB_MESG_ENTS_SIZE, procConfig.proc_rank());
    
      success = MPI_Waitany(3*buffProcs.size(), &recv_ent_reqs[0], &ind, &status);
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitany in owned entity exchange");
      }

      PRINT_DEBUG_RECD(status);

      // OK, received something; decrement incoming counter
      incoming1--;
      bool done = false;

      // In case ind is for ack, we need index of one before it
      unsigned int base_ind = 3*(ind/3);
      result = recv_buffer(MB_MESG_ENTS_SIZE,
                           status,
                           remoteOwnedBuffs[ind/3],
                           recv_ent_reqs[base_ind + 1],
                           recv_ent_reqs[base_ind + 2],
                           incoming1,
                           localOwnedBuffs[ind/3],
                           sendReqs[base_ind + 1],
                           sendReqs[base_ind + 2],
                           done,
                           (store_remote_handles ?
                            localOwnedBuffs[ind/3] : NULL),
                           MB_MESG_REMOTEH_SIZE,
                           &recv_remoteh_reqs[base_ind + 1], &incoming2);MB_CHK_SET_ERR(result, "Failed to receive buffer");

      if (done) {
        if (myDebug->get_verbosity() == 4) {
          msgs.resize(msgs.size() + 1);
          msgs.back() = new Buffer(*remoteOwnedBuffs[ind/3]);
        }

        // Message completely received - process buffer that was sent
        remoteOwnedBuffs[ind/3]->reset_ptr(sizeof(int));
        result = unpack_buffer(remoteOwnedBuffs[ind/3]->buff_ptr,
                               store_remote_handles, buffProcs[ind/3], ind/3,
                               L1hloc, L1hrem, L1p, L2hloc, L2hrem, L2p,
                               new_ents, true);
        if (MB_SUCCESS != result) {
          std::cout << "Failed to unpack entities. Buffer contents:" << std::endl;
          print_buffer(remoteOwnedBuffs[ind/3]->mem_ptr, MB_MESG_ENTS_SIZE, buffProcs[ind/3], false);
          return result;
        }

        if (recv_ent_reqs.size() != 3*buffProcs.size()) {
          // Post irecv's for remote handles from new proc
          recv_remoteh_reqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);
          for (i = recv_ent_reqs.size(); i < 3*buffProcs.size(); i += 3) {
            localOwnedBuffs[i/3]->reset_buffer();
            incoming2++;
            PRINT_DEBUG_IRECV(procConfig.proc_rank(), buffProcs[i/3],
                              localOwnedBuffs[i/3]->mem_ptr, INITIAL_BUFF_SIZE,
                              MB_MESG_REMOTEH_SIZE, incoming2);
            success = MPI_Irecv(localOwnedBuffs[i/3]->mem_ptr, INITIAL_BUFF_SIZE,
                                MPI_UNSIGNED_CHAR, buffProcs[i/3],
                                MB_MESG_REMOTEH_SIZE, procConfig.proc_comm(),
                                &recv_remoteh_reqs[i]);
            if (success != MPI_SUCCESS) {
              MB_SET_ERR(MB_FAILURE, "Failed to post irecv for remote handles in ghost exchange");
            }
          }
          recv_ent_reqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);
          sendReqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);
        }
      }
    }

    // Assign and remove newly created elements from/to receive processor
    result = assign_entities_part(new_ents, procConfig.proc_rank());MB_CHK_SET_ERR(result, "Failed to assign entities to part");
    if (migrate) {
      result = remove_entities_part(allsent, procConfig.proc_rank());MB_CHK_SET_ERR(result, "Failed to remove entities to part");
    }

    // Add requests for any new addl procs
    if (recv_ent_reqs.size() != 3*buffProcs.size()) {
      // Shouldn't get here...
      MB_SET_ERR(MB_FAILURE, "Requests length doesn't match proc count in entity exchange");
    }

#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(ENTITIES_END, procConfig.proc_rank(), "Ending entity exchange.");
    }
#endif

    // we still need to wait on sendReqs, if they are not fulfilled yet
    if (wait_all) {
      if (myDebug->get_verbosity() == 5) {
        success = MPI_Barrier(procConfig.proc_comm());
      }
      else {
        MPI_Status mult_status[3*MAX_SHARING_PROCS];
        success = MPI_Waitall(3*buffProcs.size(), &sendReqs[0], mult_status);
        if (MPI_SUCCESS != success) {
          MB_SET_ERR(MB_FAILURE, "Failed in waitall in exchange owned mesh");
        }
      }
    }

    //===========================================
    // Send local handles for new entity to owner
    //===========================================
    for (i = 0; i < n_proc; i++) {
      ind = get_buffers(exchange_procs[i]);
      // Reserve space on front for size and for initial buff size
      remoteOwnedBuffs[ind]->reset_buffer(sizeof(int));

      result = pack_remote_handles(L1hloc[ind], L1hrem[ind], L1p[ind],
                                   buffProcs[ind], remoteOwnedBuffs[ind]);MB_CHK_SET_ERR(result, "Failed to pack remote handles");
      remoteOwnedBuffs[ind]->set_stored_size();

      if (myDebug->get_verbosity() == 4) {
        msgs.resize(msgs.size() + 1);
        msgs.back() = new Buffer(*remoteOwnedBuffs[ind]);
      }
      result = send_buffer(buffProcs[ind], remoteOwnedBuffs[ind],
                           MB_MESG_REMOTEH_SIZE,
                           sendReqs[3*ind],
                           recv_remoteh_reqs[3*ind + 2],
                           &dum_ack_buff, incoming2);MB_CHK_SET_ERR(result, "Failed to send remote handles");
    }

    //===========================================
    // Process remote handles of my ghosteds
    //===========================================
    while (incoming2) {
      PRINT_DEBUG_WAITANY(recv_remoteh_reqs, MB_MESG_REMOTEH_SIZE, procConfig.proc_rank());
      success = MPI_Waitany(3*buffProcs.size(), &recv_remoteh_reqs[0], &ind, &status);
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitany in owned entity exchange");
      }

      // OK, received something; decrement incoming counter
      incoming2--;

      PRINT_DEBUG_RECD(status);

      bool done = false;
      unsigned int base_ind = 3*(ind/3);
      result = recv_buffer(MB_MESG_REMOTEH_SIZE, status,
                           localOwnedBuffs[ind/3],
                           recv_remoteh_reqs[base_ind + 1],
                           recv_remoteh_reqs[base_ind + 2],
                           incoming2,
                           remoteOwnedBuffs[ind/3],
                           sendReqs[base_ind + 1],
                           sendReqs[base_ind + 2],
                           done);MB_CHK_SET_ERR(result, "Failed to receive remote handles");

      if (done) {
        // Incoming remote handles
        if (myDebug->get_verbosity() == 4) {
          msgs.resize(msgs.size() + 1);
          msgs.back() = new Buffer(*localOwnedBuffs[ind/3]);
        }

        localOwnedBuffs[ind/3]->reset_ptr(sizeof(int));
        result = unpack_remote_handles(buffProcs[ind/3],
                                       localOwnedBuffs[ind/3]->buff_ptr,
                                       L2hloc, L2hrem, L2p);MB_CHK_SET_ERR(result, "Failed to unpack remote handles");
      }
    }

#ifdef MOAB_HAVE_MPE
    if (myDebug->get_verbosity() == 2) {
      MPE_Log_event(RHANDLES_END, procConfig.proc_rank(), "Ending remote handles.");
      MPE_Log_event(OWNED_END, procConfig.proc_rank(),
                    "Ending ghost exchange (still doing checks).");
    }
#endif

    //===========================================
    // Wait if requested
    //===========================================
    if (wait_all) {
      if (myDebug->get_verbosity() == 5) {
        success = MPI_Barrier(procConfig.proc_comm());
      }
      else {
        MPI_Status mult_status[3*MAX_SHARING_PROCS];
        success = MPI_Waitall(3*buffProcs.size(), &recv_remoteh_reqs[0], mult_status);
        if (MPI_SUCCESS == success)
          success = MPI_Waitall(3*buffProcs.size(), &sendReqs[0], mult_status);
      }
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitall in owned entity exchange");
      }
    }

#ifndef NDEBUG
    result = check_sent_ents(allsent);MB_CHK_SET_ERR(result, "Failed check on shared entities");
#endif
    myDebug->tprintf(1, "Exiting exchange_owned_mesh\n");

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_iface_entities(int other_proc,
                                             int dim,
                                             Range &iface_ents) 
  {
    Range iface_sets;
    ErrorCode result = MB_SUCCESS;

    for (Range::iterator rit = interfaceSets.begin(); rit != interfaceSets.end(); ++rit) {
      if (-1 != other_proc && !is_iface_proc(*rit, other_proc))
        continue;

      if (-1 == dim) {
        result = mbImpl->get_entities_by_handle(*rit, iface_ents);MB_CHK_SET_ERR(result, "Failed to get entities in iface set");
      }
      else {
        result = mbImpl->get_entities_by_dimension(*rit, dim, iface_ents);MB_CHK_SET_ERR(result, "Failed to get entities in iface set");
      }
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::assign_entities_part(std::vector<EntityHandle> &entities, const int proc)
  {
    EntityHandle part_set;
    ErrorCode result = get_part_handle(proc, part_set);MB_CHK_SET_ERR(result, "Failed to get part handle");

    if (part_set > 0) {
      result = mbImpl->add_entities(part_set, &entities[0], entities.size());MB_CHK_SET_ERR(result, "Failed to add entities to part set");
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::remove_entities_part(Range &entities, const int proc)
  {
    EntityHandle part_set;
    ErrorCode result = get_part_handle(proc, part_set);MB_CHK_SET_ERR(result, "Failed to get part handle");

    if (part_set > 0) {
      result = mbImpl->remove_entities(part_set, entities);MB_CHK_SET_ERR(result, "Failed to remove entities from part set");
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::check_sent_ents(Range &allsent)
  {
    // Check entities to make sure there are no zero-valued remote handles
    // where they shouldn't be
    std::vector<unsigned char> pstat(allsent.size());
    ErrorCode result = mbImpl->tag_get_data(pstatus_tag(), allsent, &pstat[0]);MB_CHK_SET_ERR(result, "Failed to get pstatus tag data");
    std::vector<EntityHandle> handles(allsent.size());
    result = mbImpl->tag_get_data(sharedh_tag(), allsent, &handles[0]);MB_CHK_SET_ERR(result, "Failed to get sharedh tag data");
    std::vector<int> procs(allsent.size());
    result = mbImpl->tag_get_data(sharedp_tag(), allsent, &procs[0]);MB_CHK_SET_ERR(result, "Failed to get sharedp tag data");

    Range bad_entities;

    Range::iterator rit;
    unsigned int i;
    EntityHandle dum_hs[MAX_SHARING_PROCS];
    int dum_ps[MAX_SHARING_PROCS];

    for (rit = allsent.begin(), i = 0; rit != allsent.end(); ++rit, i++) {
      if (-1 != procs[i] && 0 == handles[i])
        bad_entities.insert(*rit);
      else {
        // Might be multi-shared...
        result = mbImpl->tag_get_data(sharedps_tag(), &(*rit), 1, dum_ps);
        if (MB_TAG_NOT_FOUND == result)
          continue;
        else if (MB_SUCCESS != result)
          MB_SET_ERR(result, "Failed to get sharedps tag data");
        result = mbImpl->tag_get_data(sharedhs_tag(), &(*rit), 1, dum_hs);MB_CHK_SET_ERR(result, "Failed to get sharedhs tag data");

        // Find first non-set proc
        int *ns_proc = std::find(dum_ps, dum_ps + MAX_SHARING_PROCS, -1);
        int num_procs = ns_proc - dum_ps;
        assert(num_procs <= MAX_SHARING_PROCS);
        // Now look for zero handles in active part of dum_hs
        EntityHandle *ns_handle = std::find(dum_hs, dum_hs + num_procs, 0);
        int num_handles = ns_handle - dum_hs;
        assert(num_handles <= num_procs);
        if (num_handles != num_procs)
          bad_entities.insert(*rit);
      }
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::pack_remote_handles(std::vector<EntityHandle> &L1hloc,
                                              std::vector<EntityHandle> &L1hrem,
                                              std::vector<int> &L1p,
                                              unsigned int /*to_proc*/,
                                              Buffer *buff) 
  {
    assert(std::find(L1hloc.begin(), L1hloc.end(), (EntityHandle)0) == L1hloc.end());

    // 2 vectors of handles plus ints
    buff->check_space(((L1p.size() + 1)*sizeof(int) +
                       (L1hloc.size() + 1)*sizeof(EntityHandle) +
                       (L1hrem.size() + 1)*sizeof(EntityHandle)));

    // Should be in pairs of handles
    PACK_INT(buff->buff_ptr, L1hloc.size());
    PACK_INTS(buff->buff_ptr, &L1p[0], L1p.size());
    // Pack handles in reverse order, (remote, local), so on destination they
    // are ordered (local, remote)
    PACK_EH(buff->buff_ptr, &L1hrem[0], L1hrem.size());
    PACK_EH(buff->buff_ptr, &L1hloc[0], L1hloc.size());

    buff->set_stored_size();

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::unpack_remote_handles(unsigned int from_proc,
                                                unsigned char *&buff_ptr,
                                                std::vector<EntityHandle> &L2hloc,
                                                std::vector<EntityHandle> &L2hrem,
                                                std::vector<unsigned int> &L2p)
  {
    // Incoming remote handles; use to set remote handles
    int num_eh;
    UNPACK_INT(buff_ptr, num_eh);

    unsigned char *buff_proc = buff_ptr;
    buff_ptr += num_eh * sizeof(int);
    unsigned char *buff_rem = buff_ptr + num_eh * sizeof(EntityHandle);
    ErrorCode result;
    EntityHandle hpair[2], new_h;
    int proc;
    for (int i = 0; i < num_eh; i++) {
      UNPACK_INT(buff_proc, proc);
      // Handles packed (local, remote), though here local is either on this
      // proc or owner proc, depending on value of proc (-1 = here, otherwise owner);
      // this is decoded in find_existing_entity
      UNPACK_EH(buff_ptr, hpair, 1);
      UNPACK_EH(buff_rem, hpair + 1, 1);

      if (-1 != proc) {
        result = find_existing_entity(false, proc, hpair[0], 3, NULL, 0,
                                      mbImpl->type_from_handle(hpair[1]),
                                      L2hloc, L2hrem, L2p, new_h);MB_CHK_SET_ERR(result, "Didn't get existing entity");
        if (new_h)
          hpair[0] = new_h;
        else
          hpair[0] = 0;
      }
      if (!(hpair[0] && hpair[1]))
        return MB_FAILURE;
      int this_proc = from_proc;
      result = update_remote_data(hpair[0], &this_proc, hpair + 1, 1, 0);MB_CHK_SET_ERR(result, "Failed to set remote data range on sent entities in ghost exchange");
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_ghosted_entities(int bridge_dim,
                                               int ghost_dim,
                                               int to_proc,
                                               int num_layers,
                                               int addl_ents,
                                               Range &ghosted_ents)
  {
    // Get bridge ents on interface(s)
    Range from_ents;
    ErrorCode result = MB_SUCCESS;
    assert(0 < num_layers);
    for (Range::iterator rit = interfaceSets.begin(); rit != interfaceSets.end();
         ++rit) {
      if (!is_iface_proc(*rit, to_proc))
        continue;

      // Get starting "from" entities
      if (bridge_dim == -1) {
        result = mbImpl->get_entities_by_handle(*rit, from_ents);MB_CHK_SET_ERR(result, "Failed to get bridge ents in the set");
      }
      else {
        result = mbImpl->get_entities_by_dimension(*rit, bridge_dim, from_ents);MB_CHK_SET_ERR(result, "Failed to get bridge ents in the set");
      }

      // Need to get layers of bridge-adj entities
      if (from_ents.empty())
        continue;
      result = MeshTopoUtil(mbImpl).get_bridge_adjacencies(from_ents, bridge_dim,
                                                           ghost_dim, ghosted_ents,
                                                           num_layers);MB_CHK_SET_ERR(result, "Failed to get bridge adjacencies");
    }

    result = add_verts(ghosted_ents);MB_CHK_SET_ERR(result, "Failed to add verts");

    if (addl_ents) {
      // First get the ents of ghost_dim
      Range tmp_ents, tmp_owned, tmp_notowned;
      tmp_owned = ghosted_ents.subset_by_dimension(ghost_dim);
      if (tmp_owned.empty())
        return result;

      tmp_notowned = tmp_owned;

      // Next, filter by pstatus; can only create adj entities for entities I own
      result = filter_pstatus(tmp_owned, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &tmp_owned);MB_CHK_SET_ERR(result, "Failed to filter owned entities");

      tmp_notowned -= tmp_owned;

      // Get edges first
      if (1 == addl_ents || 3 == addl_ents) {
        result = mbImpl->get_adjacencies(tmp_owned, 1, true, tmp_ents, Interface::UNION);MB_CHK_SET_ERR(result, "Failed to get edge adjacencies for owned ghost entities");
        result = mbImpl->get_adjacencies(tmp_notowned, 1, false, tmp_ents, Interface::UNION);MB_CHK_SET_ERR(result, "Failed to get edge adjacencies for notowned ghost entities");
      }
      if (2 == addl_ents || 3 == addl_ents) {
        result = mbImpl->get_adjacencies(tmp_owned, 2, true, tmp_ents, Interface::UNION);MB_CHK_SET_ERR(result, "Failed to get face adjacencies for owned ghost entities");
        result = mbImpl->get_adjacencies(tmp_notowned, 2, false, tmp_ents, Interface::UNION);MB_CHK_SET_ERR(result, "Failed to get face adjacencies for notowned ghost entities");
      }

      ghosted_ents.merge(tmp_ents);
    }

    return result;
  }

  ErrorCode ParallelComm::add_verts(Range &sent_ents)
  {
    // Get the verts adj to these entities, since we'll have to send those too

    // First check sets
    std::pair<Range::const_iterator, Range::const_iterator>
      set_range = sent_ents.equal_range(MBENTITYSET);
    ErrorCode result = MB_SUCCESS, tmp_result;
    for (Range::const_iterator rit = set_range.first; rit != set_range.second; ++rit) {
      tmp_result = mbImpl->get_entities_by_type(*rit, MBVERTEX, sent_ents);MB_CHK_SET_ERR(tmp_result, "Failed to get contained verts");
    }

    // Now non-sets
    Range tmp_ents;
    std::copy(sent_ents.begin(), set_range.first, range_inserter(tmp_ents));
    result = mbImpl->get_adjacencies(tmp_ents, 0, false, sent_ents,
                                     Interface::UNION);MB_CHK_SET_ERR(result, "Failed to get vertices adj to ghosted ents");

    // if polyhedra, need to add all faces from there
    Range polyhedra=sent_ents.subset_by_type(MBPOLYHEDRON);
    // get all faces adjacent to every polyhedra
    result = mbImpl->get_connectivity(polyhedra, sent_ents);MB_CHK_SET_ERR(result, "Failed to get polyhedra faces");
    return result;
  }

  ErrorCode ParallelComm::exchange_tags(const std::vector<Tag> &src_tags,
                                        const std::vector<Tag> &dst_tags,
                                        const Range &entities_in)
  {
    ErrorCode result;
    int success;

    myDebug->tprintf(1, "Entering exchange_tags\n");

    // Get all procs interfacing to this proc
    std::set<unsigned int> exch_procs;
    result = get_comm_procs(exch_procs);

    // Post ghost irecv's for all interface procs
    // Index requests the same as buffer/sharing procs indices
    std::vector<MPI_Request> recv_tag_reqs(3*buffProcs.size(), MPI_REQUEST_NULL);
    // sent_ack_reqs(buffProcs.size(), MPI_REQUEST_NULL);
    std::vector<unsigned int>::iterator sit;
    int ind;

    reset_all_buffers();
    int incoming = 0;

    for (ind = 0, sit = buffProcs.begin(); sit != buffProcs.end(); ++sit, ind++) {
      incoming++;
      PRINT_DEBUG_IRECV(*sit, procConfig.proc_rank(), remoteOwnedBuffs[ind]->mem_ptr,
                        INITIAL_BUFF_SIZE, MB_MESG_TAGS_SIZE, incoming);

      success = MPI_Irecv(remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
                          MPI_UNSIGNED_CHAR, *sit,
                          MB_MESG_TAGS_SIZE, procConfig.proc_comm(),
                          &recv_tag_reqs[3*ind]);
      if (success != MPI_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Failed to post irecv in ghost exchange");
      }
    }

    // Pack and send tags from this proc to others
    // Make sendReqs vector to simplify initialization
    sendReqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);

    // Take all shared entities if incoming list is empty
    Range entities;
    if (entities_in.empty())
      std::copy(sharedEnts.begin(), sharedEnts.end(), range_inserter(entities));
    else
      entities = entities_in;

    int dum_ack_buff;

    for (ind = 0, sit = buffProcs.begin(); sit != buffProcs.end(); ++sit, ind++) {
      Range tag_ents = entities;

      // Get ents shared by proc *sit
      result = filter_pstatus(tag_ents, PSTATUS_SHARED, PSTATUS_AND, *sit);MB_CHK_SET_ERR(result, "Failed pstatus AND check");

      // Remote nonowned entities
      if (!tag_ents.empty()) {
        result = filter_pstatus(tag_ents, PSTATUS_NOT_OWNED, PSTATUS_NOT);MB_CHK_SET_ERR(result, "Failed pstatus NOT check");
      }

      // Pack-send; this also posts receives if store_remote_handles is true
      std::vector<Range> tag_ranges;
      for (std::vector<Tag>::const_iterator vit = src_tags.begin(); vit != src_tags.end(); ++vit) {
        const void* ptr;
        int sz;
        if (mbImpl->tag_get_default_value(*vit, ptr, sz) != MB_SUCCESS) {
          Range tagged_ents;
          mbImpl->get_entities_by_type_and_tag(0, MBMAXTYPE, &*vit, 0, 1, tagged_ents);
          tag_ranges.push_back(intersect(tag_ents, tagged_ents));
        }
        else {
          tag_ranges.push_back(tag_ents);
        }
      }

      // Pack the data
      // Reserve space on front for size and for initial buff size
      localOwnedBuffs[ind]->reset_ptr(sizeof(int));

      result = pack_tags(tag_ents,
                         src_tags, dst_tags, tag_ranges,
                         localOwnedBuffs[ind], true, *sit);MB_CHK_SET_ERR(result, "Failed to count buffer in pack_send_tag");

      // Now send it
      result = send_buffer(*sit, localOwnedBuffs[ind], MB_MESG_TAGS_SIZE, sendReqs[3*ind],
                           recv_tag_reqs[3*ind + 2], &dum_ack_buff, incoming);MB_CHK_SET_ERR(result, "Failed to send buffer");
    }

    // Receive/unpack tags
    while (incoming) {
      MPI_Status status;
      int index_in_recv_requests;
      PRINT_DEBUG_WAITANY(recv_tag_reqs, MB_MESG_TAGS_SIZE, procConfig.proc_rank());
      success = MPI_Waitany(3*buffProcs.size(), &recv_tag_reqs[0], &index_in_recv_requests, &status);
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitany in tag exchange");
      }
      // Processor index in the list is divided by 3
      ind = index_in_recv_requests / 3;

      PRINT_DEBUG_RECD(status);

      // OK, received something; decrement incoming counter
      incoming--;

      bool done = false;
      std::vector<EntityHandle> dum_vec;
      result = recv_buffer(MB_MESG_TAGS_SIZE,
                           status,
                           remoteOwnedBuffs[ind],
                           recv_tag_reqs[3*ind + 1], // This is for receiving the second message
                           recv_tag_reqs[3*ind + 2], // This would be for ack, but it is not used; consider removing it
                           incoming,
                           localOwnedBuffs[ind],
                           sendReqs[3*ind + 1], // Send request for sending the second message
                           sendReqs[3*ind + 2], // This is for sending the ack
                           done);MB_CHK_SET_ERR(result, "Failed to resize recv buffer");
      if (done) {
        remoteOwnedBuffs[ind]->reset_ptr(sizeof(int));
        result = unpack_tags(remoteOwnedBuffs[ind]->buff_ptr,
                             dum_vec, true, buffProcs[ind]);MB_CHK_SET_ERR(result, "Failed to recv-unpack-tag message");
      }
    }

    // OK, now wait
    if (myDebug->get_verbosity() == 5) {
      success = MPI_Barrier(procConfig.proc_comm());
    }
    else {
      MPI_Status status[3*MAX_SHARING_PROCS];
      success = MPI_Waitall(3*buffProcs.size(), &sendReqs[0], status);
    }
    if (MPI_SUCCESS != success) {
      MB_SET_ERR(MB_FAILURE, "Failure in waitall in tag exchange");
    }

    // If source tag is not equal to destination tag, then
    // do local copy for owned entities (communicate w/ self)
    assert(src_tags.size() == dst_tags.size());
    if (src_tags != dst_tags) {
      std::vector<unsigned char> data;
      Range owned_ents;
      if (entities_in.empty())
        std::copy(sharedEnts.begin(), sharedEnts.end(), range_inserter(entities));
      else
        owned_ents = entities_in;
      result = filter_pstatus(owned_ents, PSTATUS_NOT_OWNED, PSTATUS_NOT);MB_CHK_SET_ERR(result, "Failure to get subset of owned entities");

      if (!owned_ents.empty()) { // Check this here, otherwise we get
        // Unexpected results from get_entities_by_type_and_tag w/ Interface::INTERSECT
        for (size_t i = 0; i < src_tags.size(); i++) {
          if (src_tags[i] == dst_tags[i])
            continue;

          Range tagged_ents(owned_ents);
          result = mbImpl->get_entities_by_type_and_tag(0, MBMAXTYPE,
                                                        &src_tags[0],
                                                        0, 1, tagged_ents,
                                                        Interface::INTERSECT);MB_CHK_SET_ERR(result, "get_entities_by_type_and_tag(type == MBMAXTYPE) failed");

          int sz, size2;
          result = mbImpl->tag_get_bytes(src_tags[i], sz);MB_CHK_SET_ERR(result, "tag_get_size failed");
          result = mbImpl->tag_get_bytes(dst_tags[i], size2);MB_CHK_SET_ERR(result, "tag_get_size failed");
          if (sz != size2) {
            MB_SET_ERR(MB_FAILURE, "tag sizes don't match");
          }

          data.resize(sz * tagged_ents.size());
          result = mbImpl->tag_get_data(src_tags[i], tagged_ents, &data[0]);MB_CHK_SET_ERR(result, "tag_get_data failed");
          result = mbImpl->tag_set_data(dst_tags[i], tagged_ents, &data[0]);MB_CHK_SET_ERR(result, "tag_set_data failed");
        }
      }
    }

    myDebug->tprintf(1, "Exiting exchange_tags");

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::reduce_tags(const std::vector<Tag> &src_tags,
                                      const std::vector<Tag> &dst_tags,
                                      const MPI_Op mpi_op,
                                      const Range &entities_in)
  {
    ErrorCode result;
    int success;

    myDebug->tprintf(1, "Entering reduce_tags\n");

    // Check that restrictions are met: number of source/dst tags...
    if (src_tags.size() != dst_tags.size()) {
      MB_SET_ERR(MB_FAILURE, "Source and destination tag handles must be specified for reduce_tags");
    }

    // ... tag data types
    std::vector<Tag>::const_iterator vits, vitd;
    int tags_size, tagd_size;
    DataType tags_type, tagd_type;
    std::vector<unsigned char> vals;
    std::vector<int> tags_sizes;
    for (vits = src_tags.begin(), vitd = dst_tags.begin(); vits != src_tags.end(); ++vits, ++vitd) {
      // Checks on tag characteristics
      result = mbImpl->tag_get_data_type(*vits, tags_type);MB_CHK_SET_ERR(result, "Failed to get src tag data type");
      if (tags_type != MB_TYPE_INTEGER && tags_type != MB_TYPE_DOUBLE &&
          tags_type != MB_TYPE_BIT) {
        MB_SET_ERR(MB_FAILURE, "Src/dst tags must have integer, double, or bit data type");
      }

      result = mbImpl->tag_get_bytes(*vits, tags_size);MB_CHK_SET_ERR(result, "Failed to get src tag bytes");
      vals.resize(tags_size);
      result = mbImpl->tag_get_default_value(*vits, &vals[0]);MB_CHK_SET_ERR(result, "Src tag must have default value");

      tags_sizes.push_back(tags_size);

      // OK, those passed; now check whether dest tags, if specified, agree with src tags
      if (*vits == *vitd)
        continue;

      result = mbImpl->tag_get_bytes(*vitd, tagd_size);MB_CHK_SET_ERR(result, "Coudln't get dst tag bytes");
      if (tags_size != tagd_size) {
        MB_SET_ERR(MB_FAILURE, "Sizes between src and dst tags don't match");
      }
      result = mbImpl->tag_get_data_type(*vitd, tagd_type);MB_CHK_SET_ERR(result, "Coudln't get dst tag data type");
      if (tags_type != tagd_type) {
        MB_SET_ERR(MB_FAILURE, "Src and dst tags must be of same data type");
      }
    }

    // Get all procs interfacing to this proc
    std::set<unsigned int> exch_procs;
    result = get_comm_procs(exch_procs);

    // Post ghost irecv's for all interface procs
    // Index requests the same as buffer/sharing procs indices
    std::vector<MPI_Request> recv_tag_reqs(3*buffProcs.size(), MPI_REQUEST_NULL);

    std::vector<unsigned int>::iterator sit;
    int ind;

    reset_all_buffers();
    int incoming = 0;

    for (ind = 0, sit = buffProcs.begin(); sit != buffProcs.end(); ++sit, ind++) {
      incoming++;
      PRINT_DEBUG_IRECV(*sit, procConfig.proc_rank(), remoteOwnedBuffs[ind]->mem_ptr,
                        INITIAL_BUFF_SIZE, MB_MESG_TAGS_SIZE, incoming);

      success = MPI_Irecv(remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
                          MPI_UNSIGNED_CHAR, *sit,
                          MB_MESG_TAGS_SIZE, procConfig.proc_comm(), 
                          &recv_tag_reqs[3*ind]);
      if (success != MPI_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Failed to post irecv in ghost exchange");
      }
    }

    // Pack and send tags from this proc to others
    // Make sendReqs vector to simplify initialization
    sendReqs.resize(3*buffProcs.size(), MPI_REQUEST_NULL);

    // Take all shared entities if incoming list is empty
    Range entities;
    if (entities_in.empty())
      std::copy(sharedEnts.begin(), sharedEnts.end(), range_inserter(entities));
    else 
      entities = entities_in;

    // If the tags are different, copy the source to the dest tag locally
    std::vector<Tag>::const_iterator vit = src_tags.begin(), vit2 = dst_tags.begin();
    std::vector<int>::const_iterator vsizes = tags_sizes.begin();
    for (; vit != src_tags.end(); ++vit, ++vit2, ++vsizes) {
      if (*vit == *vit2)
        continue;
      vals.resize(entities.size()*(*vsizes));
      result = mbImpl->tag_get_data(*vit, entities, &vals[0]);MB_CHK_SET_ERR(result, "Didn't get data properly");
      result = mbImpl->tag_set_data(*vit2, entities, &vals[0]);MB_CHK_SET_ERR(result, "Didn't set data properly");
    }

    int dum_ack_buff;

    for (ind = 0, sit = buffProcs.begin(); sit != buffProcs.end(); ++sit, ind++) {
      Range tag_ents = entities;

      // Get ents shared by proc *sit
      result = filter_pstatus(tag_ents, PSTATUS_SHARED, PSTATUS_AND, *sit);MB_CHK_SET_ERR(result, "Failed pstatus AND check");

      // Pack-send
      std::vector<Range> tag_ranges;
      for (vit = src_tags.begin(); vit != src_tags.end(); ++vit) {
        const void* ptr;
        int sz;
        if (mbImpl->tag_get_default_value(*vit, ptr, sz) != MB_SUCCESS) {
          Range tagged_ents;
          mbImpl->get_entities_by_type_and_tag(0, MBMAXTYPE, &*vit, 0, 1, tagged_ents);
          tag_ranges.push_back(intersect(tag_ents, tagged_ents));
        }
        else
          tag_ranges.push_back(tag_ents);
      }

      // Pack the data
      // Reserve space on front for size and for initial buff size
      localOwnedBuffs[ind]->reset_ptr(sizeof(int));

      result = pack_tags(tag_ents,
                         src_tags, dst_tags, tag_ranges,
                         localOwnedBuffs[ind], true, *sit);MB_CHK_SET_ERR(result, "Failed to count buffer in pack_send_tag");

      // Now send it
      result = send_buffer(*sit, localOwnedBuffs[ind], MB_MESG_TAGS_SIZE, sendReqs[3*ind],
                           recv_tag_reqs[3*ind + 2], &dum_ack_buff, incoming);MB_CHK_SET_ERR(result, "Failed to send buffer");
    }

    // Receive/unpack tags
    while (incoming) {
      MPI_Status status;
      int index_in_recv_requests;
      PRINT_DEBUG_WAITANY(recv_tag_reqs, MB_MESG_TAGS_SIZE, procConfig.proc_rank());
      success = MPI_Waitany(3*buffProcs.size(), &recv_tag_reqs[0], &index_in_recv_requests, &status);
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitany in ghost exchange");
      }
      ind = index_in_recv_requests / 3;

      PRINT_DEBUG_RECD(status);

      // OK, received something; decrement incoming counter
      incoming--;

      bool done = false;
      std::vector<EntityHandle> dum_vec;
      result = recv_buffer(MB_MESG_TAGS_SIZE, status,
                        remoteOwnedBuffs[ind],
                        recv_tag_reqs[3*ind + 1], // This is for receiving the second message
                        recv_tag_reqs[3*ind + 2], // This would be for ack, but it is not used; consider removing it
                        incoming, localOwnedBuffs[ind],
                        sendReqs[3*ind + 1], // Send request for sending the second message
                        sendReqs[3*ind + 2], // This is for sending the ack
                        done);MB_CHK_SET_ERR(result, "Failed to resize recv buffer");
      if (done) {
        remoteOwnedBuffs[ind]->reset_ptr(sizeof(int));
        result = unpack_tags(remoteOwnedBuffs[ind]->buff_ptr,
                             dum_vec, true, buffProcs[ind], &mpi_op);MB_CHK_SET_ERR(result, "Failed to recv-unpack-tag message");
      }
    }

    // OK, now wait
    if (myDebug->get_verbosity() == 5) {
      success = MPI_Barrier(procConfig.proc_comm());
    }
    else {
      MPI_Status status[3*MAX_SHARING_PROCS];
      success = MPI_Waitall(3*buffProcs.size(), &sendReqs[0], status);
    }
    if (MPI_SUCCESS != success) {
      MB_SET_ERR(MB_FAILURE, "Failure in waitall in tag exchange");
    }

    myDebug->tprintf(1, "Exiting reduce_tags");

    return MB_SUCCESS;
  }

  //! return sharedp tag
  Tag ParallelComm::sharedp_tag()
  {
    if (!sharedpTag) {
      int def_val = -1;
      ErrorCode result = mbImpl->tag_get_handle(PARALLEL_SHARED_PROC_TAG_NAME,
                                                1, MB_TYPE_INTEGER, sharedpTag,
                                                MB_TAG_DENSE | MB_TAG_CREAT, &def_val);
      if (MB_SUCCESS != result)
        return 0;
    }

    return sharedpTag;
  }

  //! return sharedps tag
  Tag ParallelComm::sharedps_tag()
  {
    if (!sharedpsTag) {
      ErrorCode result = mbImpl->tag_get_handle(PARALLEL_SHARED_PROCS_TAG_NAME,
                                                MAX_SHARING_PROCS, MB_TYPE_INTEGER,
                                                sharedpsTag, MB_TAG_SPARSE | MB_TAG_CREAT);
      if (MB_SUCCESS != result)
        return 0;
    }

    return sharedpsTag;
  }

  //! return sharedh tag
  Tag ParallelComm::sharedh_tag()
  {
    if (!sharedhTag) {
      EntityHandle def_val = 0;
      ErrorCode result = mbImpl->tag_get_handle(PARALLEL_SHARED_HANDLE_TAG_NAME,
                                                1, MB_TYPE_HANDLE, sharedhTag,
                                                MB_TAG_DENSE | MB_TAG_CREAT, &def_val);
      if (MB_SUCCESS != result)
        return 0;
    }

    return sharedhTag;
  }

  //! return sharedhs tag
  Tag ParallelComm::sharedhs_tag()
  {
    if (!sharedhsTag) {
      ErrorCode result = mbImpl->tag_get_handle(PARALLEL_SHARED_HANDLES_TAG_NAME,
                                                MAX_SHARING_PROCS, MB_TYPE_HANDLE,
                                                sharedhsTag, MB_TAG_SPARSE | MB_TAG_CREAT);
      if (MB_SUCCESS != result) 
        return 0;
    }

    return sharedhsTag;
  }

  //! return pstatus tag
  Tag ParallelComm::pstatus_tag()
  {
    if (!pstatusTag) {
      unsigned char tmp_pstatus = 0;
      ErrorCode result = mbImpl->tag_get_handle(PARALLEL_STATUS_TAG_NAME,
                                                1, MB_TYPE_OPAQUE, pstatusTag,
                                                MB_TAG_DENSE | MB_TAG_CREAT,
                                                &tmp_pstatus);
      if (MB_SUCCESS != result)
        return 0;
    }

    return pstatusTag;
  }

  //! return partition set tag
  Tag ParallelComm::partition_tag()
  {
    if (!partitionTag) {
      int dum_id = -1;
      ErrorCode result = mbImpl->tag_get_handle(PARALLEL_PARTITION_TAG_NAME,
                                                1, MB_TYPE_INTEGER, partitionTag,
                                                MB_TAG_SPARSE | MB_TAG_CREAT, &dum_id);
      if (MB_SUCCESS != result)
        return 0;
    }

    return partitionTag;
  }

  //! return pcomm tag; passes in impl 'cuz this is a static function
  Tag ParallelComm::pcomm_tag(Interface *impl,
                              bool create_if_missing)
  {
    Tag this_tag = 0;
    ErrorCode result;
    if (create_if_missing) {
      result = impl->tag_get_handle(PARALLEL_COMM_TAG_NAME,
                                    MAX_SHARING_PROCS*sizeof(ParallelComm*),
                                    MB_TYPE_OPAQUE, this_tag, MB_TAG_SPARSE | MB_TAG_CREAT);
    }
    else {
      result = impl->tag_get_handle(PARALLEL_COMM_TAG_NAME,
                                    MAX_SHARING_PROCS*sizeof(ParallelComm*),
                                    MB_TYPE_OPAQUE, this_tag, MB_TAG_SPARSE);
    }

    if (MB_SUCCESS != result)
      return 0;

    return this_tag;
  }

  //! get the indexed pcomm object from the interface
  ParallelComm *ParallelComm::get_pcomm(Interface *impl, const int index) 
  {
    Tag pc_tag = pcomm_tag(impl, false);
    if (0 == pc_tag)
      return NULL;

    const EntityHandle root = 0;
    ParallelComm *pc_array[MAX_SHARING_PROCS];
    ErrorCode result = impl->tag_get_data(pc_tag, &root, 1, (void*)pc_array);
    if (MB_SUCCESS != result)
      return NULL;

    return pc_array[index];
  }

  ErrorCode ParallelComm::get_all_pcomm(Interface* impl, std::vector<ParallelComm*>& list)
  {
    Tag pc_tag = pcomm_tag(impl, false);
    if (0 == pc_tag)
      return MB_TAG_NOT_FOUND;

    const EntityHandle root = 0;
    ParallelComm *pc_array[MAX_SHARING_PROCS];
    ErrorCode rval = impl->tag_get_data(pc_tag, &root, 1, pc_array);
    if (MB_SUCCESS != rval)
      return rval;

    for (int i = 0; i < MAX_SHARING_PROCS; i++) {
      if (pc_array[i])
        list.push_back(pc_array[i]);
    }

    return MB_SUCCESS;
  }

  //! get the indexed pcomm object from the interface
  ParallelComm *ParallelComm::get_pcomm(Interface *impl,
                                        EntityHandle prtn,
                                        const MPI_Comm* comm)
  {
    ErrorCode rval;
    ParallelComm* result = 0;

    Tag prtn_tag;
    rval = impl->tag_get_handle(PARTITIONING_PCOMM_TAG_NAME,
                                1, MB_TYPE_INTEGER, prtn_tag,
                                MB_TAG_SPARSE | MB_TAG_CREAT);
    if (MB_SUCCESS != rval)
      return 0;

    int pcomm_id;
    rval = impl->tag_get_data(prtn_tag, &prtn, 1, &pcomm_id);
    if (MB_SUCCESS == rval) {
      result = get_pcomm(impl, pcomm_id);
    }
    else if (MB_TAG_NOT_FOUND == rval && comm) {
      result = new ParallelComm(impl, *comm, &pcomm_id);
      if (!result)
        return 0;
      result->set_partitioning(prtn);

      rval = impl->tag_set_data(prtn_tag, &prtn, 1, &pcomm_id);
      if (MB_SUCCESS != rval) {
        delete result;
        result = 0;
      }
    }

    return result;
  }

  ErrorCode ParallelComm::set_partitioning(EntityHandle set)
  {
    ErrorCode rval;
    Tag prtn_tag;
    rval = mbImpl->tag_get_handle(PARTITIONING_PCOMM_TAG_NAME,
                                  1, MB_TYPE_INTEGER, prtn_tag,
                                  MB_TAG_SPARSE | MB_TAG_CREAT);
    if (MB_SUCCESS != rval)
      return rval;

    // Get my id
    ParallelComm* pcomm_arr[MAX_SHARING_PROCS];
    Tag pc_tag = pcomm_tag(mbImpl, false);
    if (0 == pc_tag) 
      return MB_FAILURE;
    const EntityHandle root = 0;
    ErrorCode result = mbImpl->tag_get_data(pc_tag, &root, 1, pcomm_arr);
    if (MB_SUCCESS != result) 
      return MB_FAILURE;
    int id = std::find(pcomm_arr, pcomm_arr + MAX_SHARING_PROCS, this) - pcomm_arr;
    if (id == MAX_SHARING_PROCS)
      return MB_FAILURE;

    EntityHandle old = partitioningSet;
    if (old) {
      rval = mbImpl->tag_delete_data(prtn_tag, &old, 1);
      if (MB_SUCCESS != rval)
        return rval;
      partitioningSet = 0;
    }

    if (!set)
      return MB_SUCCESS;

    Range contents;
    if (old) {
      rval = mbImpl->get_entities_by_handle(old, contents);
      if (MB_SUCCESS != rval)
        return rval;
    }
    else {
      contents = partition_sets();
    }

    rval = mbImpl->add_entities(set, contents);
    if (MB_SUCCESS != rval)
      return rval;

    // Store pcomm id on new partition set
    rval = mbImpl->tag_set_data(prtn_tag, &set, 1, &id);
    if (MB_SUCCESS != rval)
      return rval;

    partitioningSet = set;
    return MB_SUCCESS;
  }

  //! return all the entities in parts owned locally
  ErrorCode ParallelComm::get_part_entities(Range &ents, int dim)
  {
    ErrorCode result;

    for (Range::iterator rit = partitionSets.begin(); 
         rit != partitionSets.end(); ++rit) {
      Range tmp_ents;
      if (-1 == dim) 
        result = mbImpl->get_entities_by_handle(*rit, tmp_ents, true);
      else
        result = mbImpl->get_entities_by_dimension(*rit, dim, tmp_ents, true);

      if (MB_SUCCESS != result) return result;
      ents.merge(tmp_ents);
    }

    return MB_SUCCESS;
  }

  /** \brief Return the rank of the entity owner
   */
  ErrorCode ParallelComm::get_owner_handle(EntityHandle entity,
                                           int &owner,
                                           EntityHandle &handle)
  {
    unsigned char pstat;
    int sharing_procs[MAX_SHARING_PROCS];
    EntityHandle sharing_handles[MAX_SHARING_PROCS];

    ErrorCode result = mbImpl->tag_get_data(pstatus_tag(), &entity, 1,
                                            &pstat);MB_CHK_SET_ERR(result, "Failed to get pstatus tag data");
    if (!(pstat & PSTATUS_NOT_OWNED)) {
      owner = proc_config().proc_rank();
      handle = entity;
    }
    else if (pstat & PSTATUS_MULTISHARED) {
      result = mbImpl->tag_get_data(sharedps_tag(), &entity, 1,
                                    sharing_procs);MB_CHK_SET_ERR(result, "Failed to get sharedps tag data");
      owner = sharing_procs[0];
      result = mbImpl->tag_get_data(sharedhs_tag(), &entity, 1,
                                    sharing_handles);MB_CHK_SET_ERR(result, "Failed to get sharedhs tag data");
      handle = sharing_handles[0];
    }
    else if (pstat & PSTATUS_SHARED) {
      result = mbImpl->tag_get_data(sharedp_tag(), &entity, 1,
                                    sharing_procs);MB_CHK_SET_ERR(result, "Failed to get sharedp tag data");
      owner = sharing_procs[0];
      result = mbImpl->tag_get_data(sharedh_tag(), &entity, 1,
                                    sharing_handles);MB_CHK_SET_ERR(result, "Failed to get sharedh tag data");
      handle = sharing_handles[0];
    }
    else {
      owner = -1;
      handle = 0;
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_global_part_count(int& count_out) const
  {
    count_out = globalPartCount;
    return count_out < 0 ? MB_FAILURE : MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_part_owner(int part_id, int& owner) const
  {
    // FIXME: assumes only 1 local part
    owner = part_id;
    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_part_id(EntityHandle /*part*/, int& id_out) const
  {
    // FIXME: assumes only 1 local part
    id_out = proc_config().proc_rank();
    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_part_handle(int id, EntityHandle& handle_out) const
  {
    // FIXME: assumes only 1 local part
    if ((unsigned)id != proc_config().proc_rank())
      return MB_ENTITY_NOT_FOUND;
    handle_out = partition_sets().front();
    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::create_part(EntityHandle& set_out)
  {
    // Mark as invalid so we know that it needs to be updated
    globalPartCount = -1;

    // Create set representing part
    ErrorCode rval = mbImpl->create_meshset(MESHSET_SET, set_out);
    if (MB_SUCCESS != rval)
      return rval;

    // Set tag on set
    int val = proc_config().proc_rank();
    rval = mbImpl->tag_set_data(part_tag(), &set_out, 1, &val);

    if (MB_SUCCESS != rval) {
      mbImpl->delete_entities(&set_out, 1);
      return rval;
    }

    if (get_partitioning()) {
      rval = mbImpl->add_entities(get_partitioning(), &set_out, 1);
      if (MB_SUCCESS != rval) {
        mbImpl->delete_entities(&set_out, 1);
        return rval;
      }
    }

    moab::Range& pSets = this->partition_sets();
    if (pSets.index(set_out) < 0) {
      pSets.insert(set_out);
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::destroy_part(EntityHandle part_id)
  {
    // Mark as invalid so we know that it needs to be updated
    globalPartCount = -1;

    ErrorCode rval;
    if (get_partitioning()) {
      rval = mbImpl->remove_entities(get_partitioning(), &part_id, 1);
      if (MB_SUCCESS != rval)
        return rval;
    }

    moab::Range& pSets = this->partition_sets();
    if (pSets.index(part_id) >= 0) {
      pSets.erase(part_id);
    }
    return mbImpl->delete_entities(&part_id, 1);
  }

  ErrorCode ParallelComm::collective_sync_partition()
  {
    int count = partition_sets().size();
    globalPartCount = 0;
    int err = MPI_Allreduce(&count, &globalPartCount, 1, MPI_INT, MPI_SUM,
                            proc_config().proc_comm());
    return err ? MB_FAILURE : MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_part_neighbor_ids(EntityHandle part,
                                                int neighbors_out[MAX_SHARING_PROCS],
                                                int& num_neighbors_out)
  {
    ErrorCode rval;
    Range iface;
    rval = get_interface_sets(part, iface);
    if (MB_SUCCESS != rval)
      return rval;

    num_neighbors_out = 0;
    int n, j = 0;
    int tmp[MAX_SHARING_PROCS] = {0}, curr[MAX_SHARING_PROCS] = {0};
    int *parts[2] = { neighbors_out, tmp };
    for (Range::iterator i = iface.begin(); i != iface.end(); ++i) {
      unsigned char pstat;
      rval = get_sharing_data(*i, curr, NULL, pstat, n);
      if (MB_SUCCESS != rval)
        return rval;
      std::sort(curr, curr + n);
      assert(num_neighbors_out < MAX_SHARING_PROCS);
      int* k = std::set_union(parts[j], parts[j] + num_neighbors_out,
                              curr, curr + n, parts[1 - j]);
      j = 1 - j;
      num_neighbors_out = k - parts[j];
    }
    if (parts[j] != neighbors_out)
      std::copy(parts[j], parts[j] + num_neighbors_out, neighbors_out);

    // Remove input part from list
    int id;
    rval = get_part_id(part, id);
    if (MB_SUCCESS == rval) 
      num_neighbors_out = std::remove(neighbors_out, neighbors_out + num_neighbors_out, id) - neighbors_out;
    return rval;
  }

  ErrorCode ParallelComm::get_interface_sets(EntityHandle,
                                             Range& iface_sets_out,
                                             int* adj_part_id)
  {
    // FIXME : assumes one part per processor.
    // Need to store part iface sets as children to implement
    // this correctly.
    iface_sets_out = interface_sets();

    if (adj_part_id) {
      int part_ids[MAX_SHARING_PROCS], num_parts;
      Range::iterator i = iface_sets_out.begin();
      while (i != iface_sets_out.end()) {
        unsigned char pstat;
        ErrorCode rval = get_sharing_data(*i, part_ids, NULL, pstat, num_parts);
        if (MB_SUCCESS != rval)
          return rval;
      
        if (std::find(part_ids, part_ids + num_parts, *adj_part_id) - part_ids != num_parts)
          ++i;
        else
          i = iface_sets_out.erase(i);
      }
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_owning_part(EntityHandle handle,
                                          int& owning_part_id,
                                          EntityHandle* remote_handle)
  {
    // FIXME : assumes one part per proc, and therefore part_id == rank

    // If entity is not shared, then we're the owner.
    unsigned char pstat;
    ErrorCode result = mbImpl->tag_get_data(pstatus_tag(), &handle, 1,
                                            &pstat);MB_CHK_SET_ERR(result, "Failed to get pstatus tag data");
    if (!(pstat & PSTATUS_NOT_OWNED)) {
      owning_part_id = proc_config().proc_rank();
      if (remote_handle)
        *remote_handle = handle;
      return MB_SUCCESS;
    }

    // If entity is shared with one other proc, then
    // sharedp_tag will contain a positive value.
    result = mbImpl->tag_get_data(sharedp_tag(), &handle, 1,
                                  &owning_part_id);MB_CHK_SET_ERR(result, "Failed to get sharedp tag data");
    if (owning_part_id != -1) {
      // Done?
      if (!remote_handle)
        return MB_SUCCESS;

      // Get handles on remote processors (and this one)
      return mbImpl->tag_get_data(sharedh_tag(), &handle, 1, remote_handle);
    }

    // If here, then the entity is shared with at least two other processors.
    // Get the list from the sharedps_tag
    const void* part_id_list = 0;
    result = mbImpl->tag_get_by_ptr(sharedps_tag(), &handle, 1, &part_id_list);
    if (MB_SUCCESS != result)
      return result;
    owning_part_id = ((const int*)part_id_list)[0];

    // Done?
    if (!remote_handle)
      return MB_SUCCESS;

    // Get remote handles
    const void* handle_list = 0;
    result = mbImpl->tag_get_by_ptr(sharedhs_tag(), &handle, 1, &handle_list);
    if (MB_SUCCESS != result)
      return result;

    *remote_handle = ((const EntityHandle*)handle_list)[0];
    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_sharing_parts(EntityHandle entity,
                                            int part_ids_out[MAX_SHARING_PROCS],
                                            int& num_part_ids_out,
                                            EntityHandle remote_handles[MAX_SHARING_PROCS])
  {
    // FIXME : assumes one part per proc, and therefore part_id == rank

    // If entity is not shared, then we're the owner.
    unsigned char pstat;
    ErrorCode result = mbImpl->tag_get_data(pstatus_tag(), &entity, 1,
                                            &pstat);MB_CHK_SET_ERR(result, "Failed to get pstatus tag data");
    if (!(pstat & PSTATUS_SHARED)) {
      part_ids_out[0] = proc_config().proc_rank();
      if (remote_handles)
        remote_handles[0] = entity;
      num_part_ids_out = 1;
      return MB_SUCCESS;
    }

    // If entity is shared with one other proc, then
    // sharedp_tag will contain a positive value.
    result = mbImpl->tag_get_data(sharedp_tag(), &entity, 1,
                                  part_ids_out);MB_CHK_SET_ERR(result, "Failed to get sharedp tag data");
    if (part_ids_out[0] != -1) {
      num_part_ids_out = 2;
      part_ids_out[1] = proc_config().proc_rank();

      // Done?
      if (!remote_handles)
        return MB_SUCCESS;

      // Get handles on remote processors (and this one)
      remote_handles[1] = entity;
      return mbImpl->tag_get_data(sharedh_tag(), &entity, 1, remote_handles);
    }

    // If here, then the entity is shared with at least two other processors.
    // Get the list from the sharedps_tag
    result = mbImpl->tag_get_data(sharedps_tag(), &entity, 1, part_ids_out);
    if (MB_SUCCESS != result)
      return result;
    // Count number of valid (positive) entries in sharedps_tag
    for (num_part_ids_out = 0; num_part_ids_out < MAX_SHARING_PROCS &&
           part_ids_out[num_part_ids_out] >= 0; num_part_ids_out++);
    //part_ids_out[num_part_ids_out++] = proc_config().proc_rank();
#ifndef NDEBUG
    int my_idx = std::find(part_ids_out, part_ids_out + num_part_ids_out, proc_config().proc_rank()) - part_ids_out;
    assert(my_idx < num_part_ids_out);
#endif

    // Done?
    if (!remote_handles)
      return MB_SUCCESS;

    // Get remote handles
    result = mbImpl->tag_get_data(sharedhs_tag(), &entity, 1, remote_handles);
    //remote_handles[num_part_ids_out - 1] = entity;
    assert(remote_handles[my_idx] == entity);

    return result;
  }

  ErrorCode ParallelComm::pack_shared_handles(std::vector<std::vector<SharedEntityData> > &send_data)
  {
    // Build up send buffers
    ErrorCode rval = MB_SUCCESS;
    int ent_procs[MAX_SHARING_PROCS];
    EntityHandle handles[MAX_SHARING_PROCS];
    int num_sharing, tmp_int;
    SharedEntityData tmp;
    send_data.resize(buffProcs.size());
    for (std::vector<EntityHandle>::iterator i = sharedEnts.begin(); i != sharedEnts.end(); ++i) {
      tmp.remote = *i; // Swap local/remote so they're correct on the remote proc.
      rval = get_owner(*i, tmp_int);
      tmp.owner = tmp_int;
      if (MB_SUCCESS != rval)
        return rval;

      unsigned char pstat;
      rval = get_sharing_data(*i, ent_procs, handles, pstat, num_sharing);
      if (MB_SUCCESS != rval)
        return rval;
      for (int j = 0; j < num_sharing; j++) {
        if (ent_procs[j] == (int)proc_config().proc_rank())
          continue;
        tmp.local = handles[j];
        int ind = get_buffers(ent_procs[j]);
        assert(-1 != ind);
        if ((int)send_data.size() < ind + 1)
          send_data.resize(ind + 1);
        send_data[ind].push_back(tmp);
      }
    }

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::exchange_all_shared_handles(std::vector<std::vector<SharedEntityData> > &send_data,
                                                      std::vector<std::vector<SharedEntityData> > &result)
  {
    int ierr;
    const int tag = 0;
    const MPI_Comm cm = procConfig.proc_comm();
    const int num_proc = buffProcs.size();
    const std::vector<int> procs(buffProcs.begin(), buffProcs.end());
    std::vector<MPI_Request> recv_req(buffProcs.size(), MPI_REQUEST_NULL);
    std::vector<MPI_Request> send_req(buffProcs.size(), MPI_REQUEST_NULL);

    // Set up to receive sizes
    std::vector<int> sizes_send(num_proc), sizes_recv(num_proc);
    for (int i = 0; i < num_proc; i++) {
      ierr = MPI_Irecv(&sizes_recv[i], 1, MPI_INT, procs[i], tag, cm, &recv_req[i]);
      if (ierr)
        return MB_FILE_WRITE_ERROR;
    }

    // Send sizes
    assert(num_proc == (int)send_data.size());

    result.resize(num_proc);
    for (int i = 0; i < num_proc; i++) {
      sizes_send[i] = send_data[i].size();
      ierr = MPI_Isend(&sizes_send[i], 1, MPI_INT, buffProcs[i], tag, cm, &send_req[i]);
      if (ierr) 
        return MB_FILE_WRITE_ERROR;
    }

    // Receive sizes
    std::vector<MPI_Status> stat(num_proc);
    ierr = MPI_Waitall(num_proc, &recv_req[0], &stat[0]);
    if (ierr)
      return MB_FILE_WRITE_ERROR;

    // Wait until all sizes are sent (clean up pending req's)
    ierr = MPI_Waitall(num_proc, &send_req[0], &stat[0]);
    if (ierr)
      return MB_FILE_WRITE_ERROR;

    // Set up to receive data
    for (int i = 0; i < num_proc; i++) {
      result[i].resize(sizes_recv[i]);
      ierr = MPI_Irecv(&result[i][0],
                       sizeof(SharedEntityData)*sizes_recv[i],
                       MPI_UNSIGNED_CHAR,
                       buffProcs[i], tag, cm, &recv_req[i]);
      if (ierr) 
        return MB_FILE_WRITE_ERROR;
    }

    // Send data
    for (int i = 0; i < num_proc; i++) {
      ierr = MPI_Isend(&send_data[i][0],
                       sizeof(SharedEntityData)*sizes_send[i],
                       MPI_UNSIGNED_CHAR,
                       buffProcs[i], tag, cm, &send_req[i]);
      if (ierr) 
        return MB_FILE_WRITE_ERROR;
    }

    // Receive data
    ierr = MPI_Waitall(num_proc, &recv_req[0], &stat[0]);
    if (ierr)
      return MB_FILE_WRITE_ERROR;

    // Wait until everything is sent to release send buffers
    ierr = MPI_Waitall(num_proc, &send_req[0], &stat[0]);
    if (ierr)
      return MB_FILE_WRITE_ERROR;

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::check_all_shared_handles(bool print_em)
  {
    // Get all shared ent data from other procs
    std::vector<std::vector<SharedEntityData> > shents(buffProcs.size()),
      send_data(buffProcs.size());

    ErrorCode result;
    bool done = false;

    while (!done) {
      result = check_local_shared();
      if (MB_SUCCESS != result) {
        done = true;
        continue;
      }

      result = pack_shared_handles(send_data);
      if (MB_SUCCESS != result) {
        done = true;
        continue;
      }

      result = exchange_all_shared_handles(send_data, shents);
      if (MB_SUCCESS != result) {
        done = true;
        continue;
      }

      if (!shents.empty()) 
        result = check_my_shared_handles(shents);
      done = true;
    }

    if (MB_SUCCESS != result && print_em) {
#ifdef MOAB_HAVE_HDF5
      std::ostringstream ent_str;
      ent_str << "mesh." << procConfig.proc_rank() << ".h5m";
      mbImpl->write_mesh(ent_str.str().c_str());
#endif
    }

    return result;
  }

  ErrorCode ParallelComm::check_local_shared() 
  {
    // Do some local checks on shared entities to make sure things look
    // consistent

    // Check that non-vertex shared entities are shared by same procs as all
    // their vertices
    //std::pair<Range::const_iterator,Range::const_iterator> vert_it =
    //    sharedEnts.equal_range(MBVERTEX);
    std::vector<EntityHandle> dum_connect;
    const EntityHandle *connect;
    int num_connect;
    int tmp_procs[MAX_SHARING_PROCS];
    EntityHandle tmp_hs[MAX_SHARING_PROCS];
    std::set<int> tmp_set, vset;
    int num_ps;
    ErrorCode result;
    unsigned char pstat;
    Range bad_ents;
    std::vector<std::string> errors;

    std::vector<EntityHandle>::const_iterator vit;
    for (vit = sharedEnts.begin(); vit != sharedEnts.end(); ++vit) {
      // Get sharing procs for this ent
      result = get_sharing_data(*vit, tmp_procs, tmp_hs, pstat, num_ps);
      if (MB_SUCCESS != result) {
        bad_ents.insert(*vit);
        errors.push_back(std::string("Failure getting sharing data."));
        continue;
      }

      bool bad = false;
      // Entity must be shared
      if (!(pstat & PSTATUS_SHARED))
        errors.push_back(std::string("Entity should be shared but isn't.")), bad = true;

      // If entity is not owned this must not be first proc
      if (pstat & PSTATUS_NOT_OWNED && tmp_procs[0] == (int)procConfig.proc_rank())
        errors.push_back(std::string("Entity not owned but is first proc.")), bad = true;

      // If entity is owned and multishared, this must be first proc
      if (!(pstat & PSTATUS_NOT_OWNED) && pstat & PSTATUS_MULTISHARED && 
          (tmp_procs[0] != (int)procConfig.proc_rank() || tmp_hs[0] != *vit))
        errors.push_back(std::string("Entity owned and multishared but not first proc or not first handle.")), bad = true;

      if (bad) {
        bad_ents.insert(*vit);
        continue;
      }

      EntityType type = mbImpl->type_from_handle(*vit);
      if (type == MBVERTEX || type == MBENTITYSET)
        continue;

      // Copy element's procs to vset and save size
      int orig_ps = num_ps; vset.clear(); 
      std::copy(tmp_procs, tmp_procs + num_ps, std::inserter(vset, vset.begin()));

      // Get vertices for this ent and intersection of sharing procs
      result = mbImpl->get_connectivity(*vit, connect, num_connect, false, &dum_connect);
      if (MB_SUCCESS != result) {
        bad_ents.insert(*vit); 
        errors.push_back(std::string("Failed to get connectivity."));
        continue;
      }

      for (int i = 0; i < num_connect; i++) {
        result = get_sharing_data(connect[i], tmp_procs, NULL, pstat, num_ps);
        if (MB_SUCCESS != result) {
          bad_ents.insert(*vit);
          continue;
        }
        if (!num_ps) {
          vset.clear();
          break;
        }
        std::sort(tmp_procs, tmp_procs + num_ps);
        tmp_set.clear();
        std::set_intersection(tmp_procs, tmp_procs + num_ps,
                              vset.begin(), vset.end(), std::inserter(tmp_set, tmp_set.end()));
        vset.swap(tmp_set);
        if (vset.empty())
          break;
      }

      // Intersect them; should be the same size as orig_ps
      tmp_set.clear();
      std::set_intersection(tmp_procs, tmp_procs + num_ps,
                            vset.begin(), vset.end(), std::inserter(tmp_set, tmp_set.end()));
      if (orig_ps != (int)tmp_set.size()) {
        errors.push_back(std::string("Vertex proc set not same size as entity proc set."));
        bad_ents.insert(*vit);
      }
    }

    if (!bad_ents.empty()) {
      std::cout << "Found bad entities in check_local_shared, proc rank "
                << procConfig.proc_rank() << "," << std::endl;
      std::vector<std::string>::iterator sit;
      Range::iterator rit;
      for (rit = bad_ents.begin(), sit = errors.begin(); rit != bad_ents.end(); ++rit, ++sit) {
        list_entities(&(*rit), 1);
        std::cout << "Reason: " << *sit << std::endl;
      }
      return MB_FAILURE;
    }

    // To do: check interface sets

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::check_all_shared_handles(ParallelComm **pcs,
                                                   int num_pcs)
  {
    std::vector<std::vector<std::vector<SharedEntityData> > > shents, send_data;
    ErrorCode result = MB_SUCCESS, tmp_result;

    // Get all shared ent data from each proc to all other procs
    send_data.resize(num_pcs);
    for (int p = 0; p < num_pcs; p++) {
      tmp_result = pcs[p]->pack_shared_handles(send_data[p]);
      if (MB_SUCCESS != tmp_result) result = tmp_result;
    }
    if (MB_SUCCESS != result) return result;

    // Move the data sorted by sending proc to data sorted by receiving proc
    shents.resize(num_pcs);
    for (int p = 0; p < num_pcs; p++)
      shents[p].resize(pcs[p]->buffProcs.size());

    for (int p = 0; p < num_pcs; p++) {
      for (unsigned int idx_p = 0; idx_p < pcs[p]->buffProcs.size(); idx_p++) {
        // Move send_data[p][to_p] to shents[to_p][idx_p]
        int to_p = pcs[p]->buffProcs[idx_p];
        int top_idx_p = pcs[to_p]->get_buffers(p);
        assert(-1 != top_idx_p);
        shents[to_p][top_idx_p] = send_data[p][idx_p];
      }
    }

    for (int p = 0; p < num_pcs; p++) {
      std::ostringstream ostr;
      ostr << "Processor " << p << " bad entities:";
      tmp_result = pcs[p]->check_my_shared_handles(shents[p], ostr.str().c_str());
      if (MB_SUCCESS != tmp_result) result = tmp_result;
    }

    return result;
  }

  ErrorCode ParallelComm::check_my_shared_handles(std::vector<std::vector<SharedEntityData> > &shents,
                                                  const char *prefix)
  {
    // Now check against what I think data should be
    // Get all shared entities
    ErrorCode result;
    Range all_shared;
    std::copy(sharedEnts.begin(), sharedEnts.end(), range_inserter(all_shared));
    std::vector<EntityHandle> dum_vec;
    all_shared.erase(all_shared.upper_bound(MBPOLYHEDRON), all_shared.end());

    Range bad_ents, local_shared;
    std::vector<SharedEntityData>::iterator vit;
    unsigned char tmp_pstat;
    for (unsigned int i = 0; i < shents.size(); i++) {
      int other_proc = buffProcs[i];
      result = get_shared_entities(other_proc, local_shared);
      if (MB_SUCCESS != result) return result;
      for (vit = shents[i].begin(); vit != shents[i].end(); ++vit) {
        EntityHandle localh = vit->local, remoteh = vit->remote, dumh;
        local_shared.erase(localh);
        result = get_remote_handles(true, &localh, &dumh, 1, other_proc, dum_vec);
        if (MB_SUCCESS != result || dumh != remoteh) 
          bad_ents.insert(localh);
        result = get_pstatus(localh, tmp_pstat);
        if (MB_SUCCESS != result ||
            (!tmp_pstat&PSTATUS_NOT_OWNED && (unsigned)vit->owner != rank()) ||
            (tmp_pstat&PSTATUS_NOT_OWNED && (unsigned)vit->owner == rank()))
          bad_ents.insert(localh);
      }

      if (!local_shared.empty()) 
        bad_ents.merge(local_shared);
    }

    if (!bad_ents.empty()) {
      if (prefix)
        std::cout << prefix << std::endl;
      list_entities(bad_ents);
      return MB_FAILURE;
    }
    else
      return MB_SUCCESS;
  }

  ErrorCode ParallelComm::get_shared_entities(int other_proc,
                                              Range &shared_ents,
                                              int dim,
                                              const bool iface,
                                              const bool owned_filter) 
  {
    shared_ents.clear();
    ErrorCode result = MB_SUCCESS;

    // Dimension
    if (-1 != dim) {
      DimensionPair dp = CN::TypeDimensionMap[dim];
      Range dum_range;
      std::copy(sharedEnts.begin(), sharedEnts.end(), range_inserter(dum_range));
      shared_ents.merge(dum_range.lower_bound(dp.first),
                        dum_range.upper_bound(dp.second));
    }
    else
      std::copy(sharedEnts.begin(), sharedEnts.end(), range_inserter(shared_ents));

    // Filter by iface
    if (iface) {
      result = filter_pstatus(shared_ents, PSTATUS_INTERFACE, PSTATUS_AND);MB_CHK_SET_ERR(result, "Failed to filter by iface");
    }

    // Filter by owned
    if (owned_filter) {
      result = filter_pstatus(shared_ents, PSTATUS_NOT_OWNED, PSTATUS_NOT);MB_CHK_SET_ERR(result, "Failed to filter by owned");
    }

    // Filter by proc
    if (-1 != other_proc) {
      result = filter_pstatus(shared_ents, PSTATUS_SHARED, PSTATUS_AND, other_proc);MB_CHK_SET_ERR(result, "Failed to filter by proc");
    }

    return result;
  }

  ErrorCode ParallelComm::clean_shared_tags(std::vector<Range*>& exchange_ents)
  {
    for (unsigned int i = 0; i < exchange_ents.size(); i++) {
      Range* ents = exchange_ents[i];
      int num_ents = ents->size();
      Range::iterator it = ents->begin();

      for (int n = 0; n < num_ents; n++) {
        int sharing_proc;
        ErrorCode result = mbImpl->tag_get_data(sharedp_tag(), &(*ents->begin()), 1,
                                                &sharing_proc);
        if (result != MB_TAG_NOT_FOUND && sharing_proc == -1) {
          result = mbImpl->tag_delete_data(sharedp_tag(), &(*it), 1);MB_CHK_SET_ERR(result, "Failed to delete sharedp tag data");
          result = mbImpl->tag_delete_data(sharedh_tag(), &(*it), 1);MB_CHK_SET_ERR(result, "Failed to delete sharedh tag data");
          result = mbImpl->tag_delete_data(pstatus_tag(), &(*it), 1);MB_CHK_SET_ERR(result, "Failed to delete pstatus tag data");
        }
        ++it;
      }
    }

    return MB_SUCCESS;
  }

  void ParallelComm::set_debug_verbosity(int verb) 
  {
    myDebug->set_verbosity(verb);
  }

  int ParallelComm::get_debug_verbosity() 
  {
    return myDebug->get_verbosity();
  }

  ErrorCode ParallelComm::get_entityset_procs(EntityHandle set,
                                              std::vector<unsigned>& ranks) const
  {
    return sharedSetData->get_sharing_procs(set, ranks);
  }

  ErrorCode ParallelComm::get_entityset_owner(EntityHandle entity_set,
                                              unsigned& owner_rank,
                                              EntityHandle* remote_handle) const
  {
    if (remote_handle)
      return sharedSetData->get_owner(entity_set, owner_rank, *remote_handle);
    else
      return sharedSetData->get_owner(entity_set, owner_rank);
  }

  ErrorCode ParallelComm::get_entityset_local_handle(unsigned owning_rank,
                                                     EntityHandle remote_handle,
                                                     EntityHandle& local_handle) const
  {
    return sharedSetData->get_local_handle(owning_rank, remote_handle, local_handle);
  }

  ErrorCode ParallelComm::get_shared_sets(Range& result) const
  {
    return sharedSetData->get_shared_sets(result);
  }

  ErrorCode ParallelComm::get_entityset_owners(std::vector<unsigned>& ranks) const
  {
    return sharedSetData->get_owning_procs(ranks);
  }

  ErrorCode ParallelComm::get_owned_sets(unsigned owning_rank, Range& sets_out) const
  {
    return sharedSetData->get_shared_sets(owning_rank, sets_out);
  }

  ErrorCode ParallelComm::gather_data(Range &gather_ents, Tag &tag_handle,
				      Tag id_tag, EntityHandle gather_set, int root_proc_rank)
  {
    int dim = mbImpl->dimension_from_handle(*gather_ents.begin());
    int bytes_per_tag = 0;
    ErrorCode rval = mbImpl->tag_get_bytes(tag_handle, bytes_per_tag);
    if (rval != MB_SUCCESS) return rval;

    int sz_buffer = sizeof(int) + gather_ents.size()*(sizeof(int) + bytes_per_tag);
    void* senddata = malloc(sz_buffer);
    ((int*)senddata)[0] = (int) gather_ents.size();
    int* ptr_int = (int*)senddata + 1;
    rval = mbImpl->tag_get_data(id_tag, gather_ents, (void*)ptr_int);
    if (rval != MB_SUCCESS) return rval;
    ptr_int = (int*)(senddata) + 1 + gather_ents.size();
    rval = mbImpl->tag_get_data(tag_handle, gather_ents, (void*)ptr_int);
    if (rval != MB_SUCCESS) return rval;
    std::vector<int> displs(proc_config().proc_size(), 0);
    MPI_Gather(&sz_buffer, 1, MPI_INT, &displs[0], 1, MPI_INT, root_proc_rank, comm());
    std::vector<int> recvcnts(proc_config().proc_size(), 0);
    std::copy(displs.begin(), displs.end(), recvcnts.begin());
    std::partial_sum(displs.begin(), displs.end(), displs.begin());
    std::vector<int>::iterator lastM1 = displs.end() - 1;
    std::copy_backward(displs.begin(), lastM1, displs.end());
    //std::copy_backward(displs.begin(), --displs.end(), displs.end());
    displs[0] = 0;

    if ((int)rank() != root_proc_rank)
      MPI_Gatherv(senddata, sz_buffer, MPI_BYTE, NULL, NULL, NULL, MPI_BYTE, root_proc_rank, comm());
    else {
      Range gents;
      mbImpl->get_entities_by_dimension(gather_set, dim, gents);
      int recvbuffsz = gents.size() * (bytes_per_tag + sizeof(int)) + proc_config().proc_size() * sizeof(int);
      void* recvbuf = malloc(recvbuffsz);
      MPI_Gatherv(senddata, sz_buffer, MPI_BYTE, recvbuf, &recvcnts[0], &displs[0], MPI_BYTE, root_proc_rank, comm());

      void* gvals = NULL;

      // Test whether gents has multiple sequences
      bool multiple_sequences = false;
      if (gents.psize() > 1)
        multiple_sequences = true;
      else {
        int count;
        rval = mbImpl->tag_iterate(tag_handle, gents.begin(), gents.end(), count, gvals);
        assert(NULL != gvals);
        assert(count > 0);
        if ((size_t)count != gents.size()) {
          multiple_sequences = true;
          gvals = NULL;
        }
      }

      // If gents has multiple sequences, create a temp buffer for gathered values
      if (multiple_sequences) {
        gvals = malloc(gents.size() * bytes_per_tag);
        assert(NULL != gvals);
      }

      for (int i = 0; i != (int)size(); i++) {
        int numents = *(int*)(((char*)recvbuf) + displs[i]);
        int* id_ptr = (int*)(((char*)recvbuf) + displs[i] + sizeof(int));
        char* val_ptr = (char*)(id_ptr + numents);
        for (int j = 0; j != numents; j++) {
          int idx = id_ptr[j];
          memcpy((char*)gvals + (idx - 1)*bytes_per_tag, val_ptr + j*bytes_per_tag, bytes_per_tag);
        }
      }

      // Free the receive buffer
      free(recvbuf);

      // If gents has multiple sequences, copy tag data (stored in the temp buffer) to each sequence separately
      if (multiple_sequences) {
        Range::iterator iter = gents.begin();
        size_t start_idx = 0;
        while (iter != gents.end()) {
          int count;
          void* ptr;
          rval = mbImpl->tag_iterate(tag_handle, iter, gents.end(), count, ptr);
          assert(NULL != ptr);
          assert(count > 0);
          memcpy((char*)ptr, (char*)gvals + start_idx * bytes_per_tag, bytes_per_tag * count);

          iter += count;
          start_idx += count;
        }
        assert(start_idx == gents.size());

        // Free the temp buffer
        free(gvals);
      }
    }

    // Free the send data
    free(senddata);

    return MB_SUCCESS;
  }

  /*
   * This call is collective, so we will use the message ids for tag communications;
   * they are similar, but simpler
   * Pack the number of edges, the remote edge handles, then for each edge, the number
   *    of intersection points, and then 3 doubles for each intersection point
   * On average, there is one intx point per edge, in some cases 2, in some cases 0
   *   so on average, the message size is num_edges * (sizeof(eh) + sizeof(int) + 1*3*sizeof(double))
   *          = num_edges * (8 + 4 + 24)
   */
  ErrorCode ParallelComm::settle_intersection_points(Range & edges, Range & shared_edges_owned,
                                                     std::vector<std::vector<EntityHandle> *> & extraNodesVec,
                                                     double tolerance)
  {
    // The index of an edge in the edges Range will give the index for extraNodesVec
    // the strategy of this follows exchange tags strategy:
    ErrorCode result;
    int success;

    myDebug->tprintf(1, "Entering settle_intersection_points\n");

    // Get all procs interfacing to this proc
    std::set<unsigned int> exch_procs;
    result = get_comm_procs(exch_procs);

    // Post ghost irecv's for all interface procs
    // Index requests the same as buffer/sharing procs indices
    std::vector<MPI_Request>  recv_intx_reqs(3 * buffProcs.size(), MPI_REQUEST_NULL);
    std::vector<unsigned int>::iterator sit;
    int ind;

    reset_all_buffers();
    int incoming = 0;

    for (ind = 0, sit = buffProcs.begin(); sit != buffProcs.end(); ++sit, ind++) {
      incoming++;
      PRINT_DEBUG_IRECV(*sit, procConfig.proc_rank(), remoteOwnedBuffs[ind]->mem_ptr,
          INITIAL_BUFF_SIZE, MB_MESG_TAGS_SIZE, incoming);

      success = MPI_Irecv(remoteOwnedBuffs[ind]->mem_ptr, INITIAL_BUFF_SIZE,
          MPI_UNSIGNED_CHAR, *sit, MB_MESG_TAGS_SIZE, procConfig.proc_comm(),
          &recv_intx_reqs[3 * ind]);
      if (success != MPI_SUCCESS) {
        MB_SET_ERR(MB_FAILURE, "Failed to post irecv in settle intersection point");
      }
    }

    // Pack and send intersection points from this proc to others
    // Make sendReqs vector to simplify initialization
    sendReqs.resize(3 * buffProcs.size(), MPI_REQUEST_NULL);

    // Take all shared entities if incoming list is empty
    Range & entities = shared_edges_owned;

    int dum_ack_buff;

    for (ind = 0, sit = buffProcs.begin(); sit != buffProcs.end(); ++sit, ind++) {
      Range edges_to_send = entities;

      // Get ents shared by proc *sit
      result = filter_pstatus(edges_to_send, PSTATUS_SHARED, PSTATUS_AND, *sit);MB_CHK_SET_ERR(result, "Failed pstatus AND check");

      // Remote nonowned entities; not needed, edges are already owned by this proc

      // Pack the data
      // Reserve space on front for size and for initial buff size
      Buffer* buff = localOwnedBuffs[ind];
      buff->reset_ptr(sizeof(int));

      /*result = pack_intx_points(edges_to_send, edges, extraNodesVec,
          localOwnedBuffs[ind], *sit);*/

      // Count first data, and see if it is enough room?
      // Send the remote handles
      std::vector<EntityHandle> dum_remote_edges(edges_to_send.size());
      /*
       *  get_remote_handles(const bool store_remote_handles,
                                 EntityHandle *from_vec,
                                 EntityHandle *to_vec_tmp,
                                 int num_ents, int to_proc,
                                 const std::vector<EntityHandle> &new_ents);
       */
      // We are sending count, num edges, remote edges handles, and then, for each edge:
      //          -- nb intx points, 3*nbintPointsforEdge "doubles"
      std::vector<EntityHandle> dum_vec;
      result = get_remote_handles(true,
          edges_to_send, &dum_remote_edges[0], *sit,
                                      dum_vec);MB_CHK_SET_ERR(result, "Failed to get remote handles");
      int count = 4; // Size of data
      count += sizeof(int)*(int)edges_to_send.size();
      count += sizeof(EntityHandle)*(int)edges_to_send.size(); // We will send the remote handles
      for (Range::iterator eit = edges_to_send.begin(); eit != edges_to_send.end(); ++eit) {
        EntityHandle edge = *eit;
        unsigned int indx = edges.find(edge) - edges.begin();
        std::vector<EntityHandle> & intx_nodes = *(extraNodesVec[indx]);
        count += (int)intx_nodes.size() * 3 * sizeof(double); // 3 integer for each entity handle
      }
      //
      buff->check_space(count);
      PACK_INT(buff->buff_ptr, edges_to_send.size());
      PACK_EH(buff->buff_ptr, &dum_remote_edges[0], dum_remote_edges.size());
      for (Range::iterator eit = edges_to_send.begin(); eit != edges_to_send.end(); ++eit) {
        EntityHandle edge = *eit;
        // Pack the remote edge
        unsigned int indx = edges.find(edge) - edges.begin();
        std::vector<EntityHandle> & intx_nodes = *(extraNodesVec[indx]);
        PACK_INT(buff->buff_ptr, intx_nodes.size());

        result = mbImpl->get_coords(&intx_nodes[0], intx_nodes.size(),
                                    (double*)buff->buff_ptr);MB_CHK_SET_ERR(result, "Failed to get coords");
        buff->buff_ptr += 3 * sizeof(double) * intx_nodes.size();
      }

      // Done packing the intx points and remote edges
      buff->set_stored_size();

      // Now send it
      result = send_buffer(*sit, localOwnedBuffs[ind], MB_MESG_TAGS_SIZE,
          sendReqs[3 * ind], recv_intx_reqs[3 * ind + 2], &dum_ack_buff, incoming);MB_CHK_SET_ERR(result, "Failed to send buffer");
    }

    // Receive/unpack intx points
    while (incoming) {
      MPI_Status status;
      int index_in_recv_requests;
      PRINT_DEBUG_WAITANY(recv_intx_reqs, MB_MESG_TAGS_SIZE, procConfig.proc_rank());
      success = MPI_Waitany(3 * buffProcs.size(), &recv_intx_reqs[0],
          &index_in_recv_requests, &status);
      if (MPI_SUCCESS != success) {
        MB_SET_ERR(MB_FAILURE, "Failed in waitany in ghost exchange");
      }
      // Processor index in the list is divided by 3
      ind = index_in_recv_requests / 3;

      PRINT_DEBUG_RECD(status);

      // OK, received something; decrement incoming counter
      incoming--;

      bool done = false;
      result = recv_buffer(MB_MESG_TAGS_SIZE, status,
          remoteOwnedBuffs[ind],
          recv_intx_reqs[3*ind + 1], // This is for receiving the second message
          recv_intx_reqs[3*ind + 2], // This would be for ack, but it is not used; consider removing it
          incoming,
          localOwnedBuffs[ind],
          sendReqs[3*ind + 1], // Send request for sending the second message
          sendReqs[3*ind + 2], // This is for sending the ack
          done);MB_CHK_SET_ERR(result, "Failed to resize recv buffer");
      if (done) {
        Buffer * buff = remoteOwnedBuffs[ind];
        buff->reset_ptr(sizeof(int));
        /*result = unpack_tags(remoteOwnedBuffs[ind/2]->buff_ptr, dum_vec, true,
            buffProcs[ind/2]);*/
        // Unpack now the edges and vertex info; compare with the existing vertex positions

        int num_edges;

        UNPACK_INT(buff->buff_ptr, num_edges);
        std::vector<EntityHandle> rec_edges;
        rec_edges.resize(num_edges);
        UNPACK_EH(buff->buff_ptr, &rec_edges[0], num_edges);
        for (int i = 0; i < num_edges; i++) {
          EntityHandle edge=rec_edges[i];
          unsigned int indx = edges.find(edge) - edges.begin();
          std::vector<EntityHandle> & intx_nodes = *(extraNodesVec[indx]);
          // Now get the number of nodes on this (now local) edge
          int nverts;
          UNPACK_INT(buff->buff_ptr, nverts);
          assert(nverts==(int)intx_nodes.size());
          // Get the positions communicated
          std::vector<double> pos_from_owner;
          pos_from_owner.resize(3*nverts);
          UNPACK_DBLS(buff->buff_ptr, &pos_from_owner[0], 3*nverts);
          std::vector<double> current_positions(3*nverts);
          result = mbImpl->get_coords(&intx_nodes[0], nverts, &current_positions[0]);MB_CHK_SET_ERR(result, "Failed to get current positions");
          // Now, look at what we have in current pos, compare to pos from owner, and reset
          for (int k = 0; k < nverts; k++) {
            double * pk = &current_positions[3*k];
            // Take the current pos k, and settle among the ones from owner:
            bool found = false;
            for (int j = 0; j < nverts && !found; j++) {
              double * pj = &pos_from_owner[3*j];
              double dist2 = (pk[0] - pj[0])*(pk[0] - pj[0]) + (pk[1] - pj[1])*(pk[1] - pj[1]) +
                             (pk[2] - pj[2])*(pk[2] - pj[2]);
              if (dist2 < tolerance) {
                pk[0] = pj[0]; pk[1] = pj[1]; pk[2] = pj[2]; // Correct it!
                found = true;
                break;
              }
            }
            if (!found) {
              std::cout << " pk:" << pk[0] << " " << pk[1] << " " << pk[2] << " not found \n";
              result = MB_FAILURE;
            }
          }
          // After we are done resetting, we can set the new positions of nodes:
          result = mbImpl->set_coords(&intx_nodes[0], nverts, &current_positions[0]);MB_CHK_SET_ERR(result, "Failed to set new current positions");
        }
      }
    }

    // OK, now wait
    if (myDebug->get_verbosity() == 5) {
      success = MPI_Barrier(procConfig.proc_comm());
    } else {
      MPI_Status status[3 * MAX_SHARING_PROCS];
      success = MPI_Waitall(3 * buffProcs.size(), &sendReqs[0], status);
    }
    if (MPI_SUCCESS != success) {
      MB_SET_ERR(MB_FAILURE, "Failure in waitall in tag exchange");
    }

    myDebug->tprintf(1, "Exiting settle_intersection_points");

    return MB_SUCCESS;
  }

  ErrorCode ParallelComm::delete_entities(Range & to_delete)
  {
    // Will not look at shared sets yet, but maybe we should
    // First, see if any of the entities to delete is shared; then inform the other processors
    // about their fate (to be deleted), using a crystal router transfer
    ErrorCode rval = MB_SUCCESS;
    unsigned char pstat;
    EntityHandle tmp_handles[MAX_SHARING_PROCS];
    int tmp_procs[MAX_SHARING_PROCS];
    unsigned int num_ps;
    TupleList ents_to_delete;
    ents_to_delete.initialize(1, 0, 1, 0, to_delete.size() * (MAX_SHARING_PROCS + 1)); // A little bit of overkill
    ents_to_delete.enableWriteAccess();
    unsigned int i = 0;
    for (Range::iterator it = to_delete.begin(); it != to_delete.end(); ++it) {
      EntityHandle eh = *it; // Entity to be deleted

      rval = get_sharing_data(eh, tmp_procs, tmp_handles,
                              pstat, num_ps);
      if (rval != MB_SUCCESS || num_ps == 0)
        continue;
      // Add to the tuple list the information to be sent (to the remote procs)
      for (unsigned int p = 0; p < num_ps; p++) {
        ents_to_delete.vi_wr[i] = tmp_procs[p];
        ents_to_delete.vul_wr[i] = (unsigned long)tmp_handles[p];
        i++;
        ents_to_delete.inc_n();
      }
    }

    gs_data::crystal_data *cd = this->procConfig.crystal_router();
    // All communication happens here; no other mpi calls
    // Also, this is a collective call
    rval = cd->gs_transfer(1, ents_to_delete, 0);MB_CHK_SET_ERR(rval, "Error in tuple transfer");

    // Add to the range of ents to delete the new ones that were sent from other procs
    unsigned int received = ents_to_delete.get_n();
    for (i = 0; i < received; i++) {
      //int from = ents_to_delete.vi_rd[i];
      unsigned long valrec = ents_to_delete.vul_rd[i];
      to_delete.insert((EntityHandle)valrec);
    }
    rval = mbImpl->delete_entities(to_delete);MB_CHK_SET_ERR(rval, "Error in deleting actual entities");

    std::vector<EntityHandle> good_ents;
    for (size_t j = 0; j<sharedEnts.size(); j++) {
      int index = to_delete.index(sharedEnts[j]);
      if (-1 == index)
        good_ents.push_back(sharedEnts[j]);
    }
    sharedEnts = good_ents;

    // What about shared sets? Who is updating them?
    return MB_SUCCESS;
  }

  void ParallelComm::print_pstatus(unsigned char pstat, std::string &ostr)
  {
    std::ostringstream str;
    int num = 0;
#define ppstat(a, b) { if (pstat & a) { if (num) str << ", "; str << b; num++; } }

    ppstat(PSTATUS_NOT_OWNED, "NOT_OWNED");
    ppstat(PSTATUS_SHARED, "SHARED");
    ppstat(PSTATUS_MULTISHARED, "MULTISHARED");
    ppstat(PSTATUS_INTERFACE, "INTERFACE");
    ppstat(PSTATUS_GHOST, "GHOST");

    ostr = str.str();
  }

  void ParallelComm::print_pstatus(unsigned char pstat)
  {
    std::string str;
    print_pstatus(pstat, str);
    std::cout << str.c_str() << std::endl;
  }

} // namespace moab
