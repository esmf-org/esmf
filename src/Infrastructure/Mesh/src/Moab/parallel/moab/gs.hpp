#ifndef GS_HPP
#define GS_HPP

#include "moab/MOABConfig.h"
#include "moab/TupleList.hpp"
#include "moab/Types.hpp"

#ifdef MOAB_HAVE_MPI
#include "moab_mpi.h"
#endif

namespace moab {

  class gs_data
  {
  public:
#ifdef MOAB_HAVE_MPI
    class nonlocal_info
    {
    public:
      uint _np;           /* number of processors to communicate with          */
      uint *_target;      /* int target[np]: array of processor ids to comm w/ */
      uint *_nshared;     /* nshared[i] = number of points shared w/ target[i] */
      uint *_sh_ind;      /* list of shared point indices                      */
      slong *_slabels;    /* list of signed long labels (not including gid)    */
      Ulong *_ulabels;    /* list of unsigned long labels                      */
      MPI_Request *_reqs; /* pre-allocated for MPI calls                       */
      realType *_buf;         /* pre-allocated buffer to receive data              */
      uint _maxv;         /* maximum vector size                               */
    
      /**Constructor for nonlocal_info; takes all arguments and initializes 
       * nonlocal_info
       *
       * param np        number of processors to communicate with
       * param count     number of partner processors
       * param nlabels   number of signed long labels (not including gid)
       * param nulabels  number of unsigned long labels
       * param maxv      maximum vector size  
       */
      nonlocal_info (uint tmp_np, uint count, uint nlabels, 
		     uint nulabels, uint tmp_maxv) {
	this->initialize(tmp_np, count, nlabels, nulabels, tmp_maxv);
      }

      /**Initializes nonlocal_info; see constructor for parameter documentation
       */
      void initialize(uint np, uint count, uint nlabels,
		      uint nulabels, uint maxv);

      ~nonlocal_info () { nlinfo_free(); };

      void nonlocal(realType *u, int op, MPI_Comm comm);
      void nonlocal_vec(realType *u, uint n, int op, MPI_Comm comm);
      void nonlocal_many(realType **u, uint n, int op, MPI_Comm comm);
      void nlinfo_free();
    } ;
  public:

    /*---------------------------------------------------------------------------

      Crystal Router

      Accomplishes all-to-all communication in log P msgs per proc
      The routine is low-level; the format of the input/output is an
      array of integers, consisting of a sequence of messages with format:

      target proc
      source proc
      m
      integer
      integer
      ...
      integer  (m integers in total)

      Before moab_crystal_router is called, the source of each message should be
      set to this proc id; upon return from moab_crystal_router, the target of each
      message will be this proc id.

      Usage:

      MPI_Comm comm = ... ;
      moab_crystal_data crystal; 
      //moab_crystal_data crystal(comm);  //or this to initialize on 
      //instantiation

      crystal.initialize(comm);  // initialize the data structure
      // now crystal.id  = this proc
      // and crystal.num = num of procs

      // allocate space for at least MAX ints
      buffer_reserve(&crystal->all->buf, MAX*sizeof(uint));

      // fill up ((uint*)crystal->all->buf.ptr)[0 ... n-1]
      // and set crystal->all->n

      crystal.moab_crystal_router();

      // incoming messages available as
      // ((uint*)crystal->all->buf.ptr)[0 ... crystal->all->n-1]

      crystal.reset(); // release acquired memory

      ---------------------------------------------------------------------------*/

    class crystal_data
    {
    public:
      //moab_crystal_data member variables & data
      typedef struct { uint n; moab::TupleList::buffer buf; } crystal_buf;
      crystal_buf buffers[3];
      //crystal_buf provides buffer space for communications
      crystal_buf *all, *keep, *send;
      MPI_Comm _comm;
      uint _num, _id;

      /**Default constructor (Note:  moab_crystal_data must be initialized
       * before use!)
       */
      crystal_data();

      /**Constructor takes an MPI_Comm and initializes the moab_data_crystal
       */
      crystal_data(MPI_Comm cm) { initialize(cm); };

      ~crystal_data(){ reset(); };

      /**Initializes crystal_data members according to MPI_Comm passed in
       * Note:  moab_crystal_data must be initialized before it can be used
       *
       * param comm  MPI_Comm detailing where to send messages
       */
      void initialize(MPI_Comm comm);

      /**Frees buffers used by moab_crystal_data
       */
      void reset();

      /**Communicates messages with other processors; see class description for
       * more info and usage
       */
      void crystal_router();

      /**Treats one integer (not long) member of the TupleList as a target proc;
       * Sends out tuples accordingly, using the crystal router.
       * Target proc member overwritten with source proc.
       *
       * param dynamic   non-zero if the TupleList should grow to accomodate  
       *                 arrivals
       * param tl        the TupleList
       * param pf        which tuple member specifies target proc
       */
      ErrorCode gs_transfer(int dynamic, moab::TupleList &tl,
			      unsigned pf);

    private:
      //Used by moab_crystal_router:  see .cpp for more details
      void partition(uint cutoff, crystal_buf *lo, crystal_buf *hi);

      void send_(uint target, int recvn);

    };
#else
    //If mpi is not used, moab_crystal_data cannot be used
    class crystal_data{};
#endif

    sint *local_cm; /* local condense map */
#ifdef MOAB_HAVE_MPI
    nonlocal_info *nlinfo;
    MPI_Comm _comm;
#endif

    /**Constructor for moab_gs_data:  takes all arguments and initializes
     * moab_gs_data.  If needs_reset is false after calling constructor, 
     * initialization has failed.
     *
     * param n         number of tuples in tuple list
     * param label     pointer to signed labels
     * param ulabel    pointer to unsigned labels
     * param maxv      max vector size
     * param nlabels   number of signed long labels (not including gid)
     * param nulabels  number of unsigned long labels
     * param crystal   moab_crystal_data contains MPI_Comm and is used for 
     *                 message passing
     */
    gs_data (uint n, const long *label, const Ulong *ulabel,
	     uint maxv, const unsigned int nlabels,
	     const unsigned int nulabels,
	     crystal_data *crystal, ErrorCode &Init_Result) {
      Init_Result = this->initialize (n, label, ulabel, maxv, nlabels,
				      nulabels, crystal);
    };
  
  
    /**Default constructor (Note:  moab_gs_data must be initialized
     * before use!)
     */  
    gs_data () {
    };

    ~gs_data () { reset(); }

    /**Sets up the moab_gs_data; see constructor for parameter documentation
     */
    ErrorCode initialize (uint n, const long *label, const Ulong *ulabel,
			    uint maxv, const unsigned int nlabels,
			    const unsigned int nulabels,
			    crystal_data *crystal);

    void reset();

    void gs_data_op(realType *u, int op);
    void gs_data_op_vec(realType *u, uint n, int op);
    void gs_data_op_many(realType **u, uint n, int op);

#define GS_OP_ADD 1
#define GS_OP_MUL 2
#define GS_OP_MIN 3
#define GS_OP_MAX 4
#define GS_OP_BPR 5
  } ;

}
#endif
