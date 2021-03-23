/* compile-time settings:

 FORTRAN naming convention
 default      cpgs_setup, etc.
 -DUPCASE     CPGS_SETUP, etc.
 -DUNDERSCORE cpgs_setup_, etc.

 -DMPI             parallel version (sequential otherwise)
 -DCRYSTAL_STATIC  avoid some message exchange at the risk of
 crashing b/c of insufficient buffer size

 -DINITIAL_BUFFER_SIZE=expression
 ignored unless CRYSTAL_STATIC is defined.
 arithmetic expression controlling the initial buffer size for the crystal
 router; this needs to be large enough to hold the intermediate messages
 during all stages of the crystal router

 variables that can be used in expression include
 num   - the number of processors
 n     - the length of the global index array

 */

/* default for INITIAL_BUFFER_SIZE */
#ifdef CRYSTAL_STATIC
#  ifndef INITIAL_BUFFER_SIZE
#    define INITIAL_BUFFER_SIZE 2*(3*num+n*9)
#  endif
#endif

/* FORTRAN usage:

 integer hc, np
 call crystal_new(hc,comm,np)  ! get a crystal router handle (see fcrystal.c)

 integer hgs
 integer n, max_vec_dim
 integer*? global_index_array(1:n) ! type corresponding to slong in "types.h"

 call cpgs_setup(hgs,hc,global_index_array,n,max_vec_dim)
 sets hgs to new handle

 !ok to call crystal_done(hc) here, or any later time

 call cpgs_op(hgs, u, op)
 integer handle, op : 1-add, 2-multiply, 3-min, 4-max
 real    u(1:n) - same layout as global_index_array provided to cpgs_setup

 call cpgs_op_vec(hgs, u, d, op)
 integer op : 1-add, 2-multiply, 3-min, 4-max
 integer d    <= max_vec_dim
 real    u(1:d, 1:n) - vector components for each node stored together

 call cpgs_op_many(hgs, u1, u2, u3, u4, u5, u6, d, op)
 integer op : 1-add, 2-multiply, 3-min, 4-max
 integer d : in {1,2,3,4,5,6}, <= max_vec_dim
 real    u1(1:n), u2(1:n), u3(1:n), etc.

 same effect as: call cpgs_op(hgs, u1, op)
 if(d.gt.1) call cpgs_op(hgs, u2, op)
 if(d.gt.2) call cpgs_op(hgs, u3, op)
 etc.
 with possibly some savings as fewer messages are exchanged

 call cpgs_free(hgs)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include "moab/gs.hpp"
#ifdef MOAB_HAVE_MPI
#  include "moab_mpi.h"
#endif

#ifdef MOAB_HAVE_MPI

#ifdef MOAB_HAVE_VALGRIND
#  include <valgrind/memcheck.h>
#elif !defined(VALGRIND_CHECK_MEM_IS_DEFINED)
#  define VALGRIND_CHECK_MEM_IS_DEFINED(a,b) ((void)0)
#endif

#endif

namespace moab {

/*--------------------------------------------------------------------------
 Local Execution Phases
 --------------------------------------------------------------------------*/

#define DO_SET(a,b) b=a
#define DO_ADD(a,b) a+=b
#define DO_MUL(a,b) a*=b
#define DO_MIN(a,b) if(b<a) a=b
#define DO_MAX(a,b) if(b>a) a=b
#define DO_BPR(a,b)							\
  do \
  { uint a_ = a; \
    uint b_ = b; \
    for(;;) { \
       if(a_<b_) \
          b_>>=1; \
       else if(b_<a_) \
          a_>>=1; \
       else break; }	\
    a = a_;								\
  } while(0)

#define LOOP(op) do {				\
    sint i,j;					\
    while((i=*cm++) != -1)			\
      while((j=*cm++) != -1)			\
	      op(u[i],u[j]);				\
  } while(0)

static void local_condense(realType *u, int op, const sint *cm)
{
  switch (op)
  {
  case GS_OP_ADD:
    LOOP(DO_ADD);
    break;
  case GS_OP_MUL:
    LOOP(DO_MUL);
    break;
  case GS_OP_MIN:
    LOOP(DO_MIN);
    break;
  case GS_OP_MAX:
    LOOP(DO_MAX);
    break;
  case GS_OP_BPR:
    LOOP(DO_BPR);
    break;
  }
}

static void local_uncondense(realType *u, const sint *cm)
{
  LOOP(DO_SET);
}

#undef LOOP

#define LOOP(op) do { \
    sint i,j,k; \
    while((i=*cm++) != -1) { \
      realType *pi=u+n*i; \
      while((j=*cm++) != -1) { \
	       realType *pj=u+n*j; \
	       for(k=n;k;--k) { \
	         op(*pi,*pj); \
	         ++pi; \
	         ++pj; \
	       } \
      } \
    } \
  } while(0)

static void local_condense_vec(realType *u, uint n, int op, const sint *cm)
{
  switch (op)
  {
  case GS_OP_ADD:
    LOOP(DO_ADD);
    break;
  case GS_OP_MUL:
    LOOP(DO_MUL);
    break;
  case GS_OP_MIN:
    LOOP(DO_MIN);
    break;
  case GS_OP_MAX:
    LOOP(DO_MAX);
    break;
  case GS_OP_BPR:
    LOOP(DO_BPR);
    break;
  }
}

static void local_uncondense_vec(realType *u, uint n, const sint *cm)
{
  LOOP(DO_SET);
}

#undef LOOP

/*--------------------------------------------------------------------------
 Non-local Execution Phases
 --------------------------------------------------------------------------*/

#ifdef MOAB_HAVE_MPI

void gs_data::nonlocal_info::initialize(uint np, uint count,
    uint nlabels, uint nulabels, uint maxv)
{
  _target = NULL;
  _nshared = NULL;
  _sh_ind = NULL;
  _slabels = NULL;
  _ulabels = NULL;
  _reqs = NULL;
  _buf = NULL;
  _np = np;
  _target = (uint*) malloc((2*np+count)*sizeof(uint));
  _nshared = _target + np;
  _sh_ind = _nshared + np;
  if (1 < nlabels)
  _slabels = (slong*) malloc(((nlabels-1)*count)*sizeof(slong));
  else
  _slabels = NULL;
  _ulabels = (Ulong*) malloc((nulabels*count)*sizeof(Ulong));
  _reqs = (MPI_Request*) malloc(2*np*sizeof(MPI_Request));
  _buf = (realType*) malloc((2*count*maxv)*sizeof(realType));
  _maxv = maxv;
}

void gs_data::nonlocal_info::nlinfo_free()
{
  //Free the ptrs
  free(_buf);
  free(_reqs);
  free(_target);
  free(_slabels);
  free(_ulabels);
  //Set to null
  _ulabels=NULL;
  _buf=NULL;
  _reqs=NULL;
  _target=NULL;
  _slabels=NULL;
  _nshared = NULL;
  _sh_ind = NULL;
}

void gs_data::nonlocal_info::nonlocal(realType *u, int op, MPI_Comm comm)
{
  MPI_Status status;
  uint np = this->_np;
  MPI_Request *reqs = this->_reqs;
  uint *targ = this->_target;
  uint *nshared = this->_nshared;
  uint *sh_ind = this->_sh_ind;
  uint id;
  realType *buf = this->_buf, *start;
  unsigned int i;
  { MPI_Comm_rank(comm,(int *)&i); id=i;}
  for (i=0; i<np; ++i)
  {
    uint c = nshared[i];
    start = buf;
    for (;c;--c)
      *buf++ = u[*sh_ind++];
    MPI_Isend((void*)start,nshared[i]*sizeof(realType),
        MPI_UNSIGNED_CHAR, targ[i],id,comm,reqs++);
  }
  start = buf;
  for(i=0; i<np; ++i)
  {
    MPI_Irecv((void*)start,nshared[i]*sizeof(realType),MPI_UNSIGNED_CHAR,
        targ[i],targ[i],comm,reqs++);
    start+=nshared[i];
  }
  for (reqs=this->_reqs,i=np*2;i;--i)
    MPI_Wait(reqs++,&status);
  sh_ind = this->_sh_ind;
# define LOOP(OP) \
  do { \
    for(i=0;i<np;++i) { \
	    uint c; \
	    for(c=nshared[i];c;--c) \
	    { \
        OP(u[*sh_ind],*buf); \
        ++sh_ind, ++buf; \
      } \
    } \
  } while(0)
  switch(op)
  {
    case GS_OP_ADD: LOOP(DO_ADD); break;
    case GS_OP_MUL: LOOP(DO_MUL); break;
    case GS_OP_MIN: LOOP(DO_MIN); break;
    case GS_OP_MAX: LOOP(DO_MAX); break;
    case GS_OP_BPR: LOOP(DO_BPR); break;
  }
# undef LOOP
}

void gs_data::nonlocal_info::nonlocal_vec(realType *u, uint n,
    int op, MPI_Comm comm)
{
  MPI_Status status;
  uint np = this->_np;
  MPI_Request *reqs = this->_reqs;
  uint *targ = this->_target;
  uint *nshared = this->_nshared;
  uint *sh_ind = this->_sh_ind;
  uint id;
  realType *buf = this->_buf, *start;
  uint size = n*sizeof(realType);
  unsigned int i;
  {
    MPI_Comm_rank(comm,(int *)&i);
    id=i;
  }
  for (i=0; i<np; ++i)
  {
    uint ns=nshared[i], c=ns;
    start = buf;
    for (;c;--c) {
      memcpy(buf,u+n*(*sh_ind++),size);
      buf+=n;
    }
    MPI_Isend((void*)start,ns*size,MPI_UNSIGNED_CHAR,targ[i],id,comm,reqs++);
  }
  start = buf;
  for (i=0; i<np; ++i)
  {
    int nsn=n*nshared[i];
    MPI_Irecv((void*)start,nsn*size,MPI_UNSIGNED_CHAR,targ[i],targ[i],comm,reqs++);
    start+=nsn;
  }
  for (reqs=this->_reqs,i=np*2;i;--i)
    MPI_Wait(reqs++,&status);
  sh_ind = this->_sh_ind;
# define LOOP(OP) \
   do {	\
     for(i=0;i<np;++i) { \
       uint c,j; \
       for(c=nshared[i];c;--c) { \
         realType *uu=u+n*(*sh_ind++); \
	       for(j=n;j;--j) { \
	         OP(*uu,*buf); \
           ++uu; \
           ++buf; \
	       } \
       } \
     } \
  } while(0)
  switch(op)
  {
    case GS_OP_ADD: LOOP(DO_ADD); break;
    case GS_OP_MUL: LOOP(DO_MUL); break;
    case GS_OP_MIN: LOOP(DO_MIN); break;
    case GS_OP_MAX: LOOP(DO_MAX); break;
    case GS_OP_BPR: LOOP(DO_BPR); break;
  }
# undef LOOP
}

void gs_data::nonlocal_info::nonlocal_many(realType **u, uint n, int op,
    MPI_Comm comm)
{
  MPI_Status status;
  uint np = this->_np;
  MPI_Request *reqs = this->_reqs;
  uint *targ = this->_target;
  uint *nshared = this->_nshared;
  uint *sh_ind = this->_sh_ind;
  uint id;
  realType *buf = this->_buf, *start;
  unsigned int i;
  {
    MPI_Comm_rank(comm,(int *)&i);
    id=i;
  }
  for (i=0; i<np; ++i)
  {
    uint c, j, ns = nshared[i];
    start = buf;
    for (j=0; j<n; ++j)
    {
      realType*uu=u[j];
      for(c=0;c<ns;++c)
        *buf++=uu[sh_ind[c]];
    }
    sh_ind+=ns;
    MPI_Isend((void*)start,n*ns*sizeof(realType),MPI_UNSIGNED_CHAR,targ[i],id,comm,reqs++);
  }
  start = buf;
  for (i=0; i<np; ++i)
  {
    int nsn = n*nshared[i];
    MPI_Irecv((void*)start,nsn*sizeof(realType),MPI_UNSIGNED_CHAR,
        targ[i],targ[i],comm,reqs++);
    start+=nsn;
  }
  for (reqs=this->_reqs,i=np*2;i;--i)
    MPI_Wait(reqs++,&status);
  sh_ind = this->_sh_ind;
# define LOOP(OP) \
  do { \
    for(i=0;i<np;++i) { \
	    uint c,j,ns=nshared[i]; \
      for(j=0;j<n;++j) { \
        realType *uu=u[j];					\
        for(c=0;c<ns;++c) { \
          OP(uu[sh_ind[c]],*buf); \
          ++buf; \
        } \
      } \
	    sh_ind+=ns; \
    } \
  } while(0)
  switch(op)
  {
    case GS_OP_ADD: LOOP(DO_ADD); break;
    case GS_OP_MUL: LOOP(DO_MUL); break;
    case GS_OP_MIN: LOOP(DO_MIN); break;
    case GS_OP_MAX: LOOP(DO_MAX); break;
    case GS_OP_BPR: LOOP(DO_BPR); break;
  }
# undef LOOP
}

/*---------------------------------------------------------------------------
 MOAB Crystal Router
 ---------------------------------------------------------------------------*/
gs_data::crystal_data::crystal_data()
{
}

void gs_data::crystal_data::initialize(MPI_Comm comm)
{
  int num,id;
  buffers[0].buf.buffer_init(1024);
  buffers[1].buf.buffer_init(1024);
  buffers[2].buf.buffer_init(1024);
  all=&buffers[0];
  keep=&buffers[1];
  send=&buffers[2];
  memcpy(&(this->_comm),&comm,sizeof(MPI_Comm));
  MPI_Comm_rank(comm,&id );
  this->_id =id;
  MPI_Comm_size(comm,&num);
  this->_num=num;
}

void gs_data::crystal_data::reset()
{
  buffers[0].buf.reset();
  buffers[1].buf.reset();
  buffers[2].buf.reset();
  keep = NULL;
  all = NULL;
  send = NULL;
}

//Partition data before sending
void gs_data::crystal_data::partition(uint cutoff,
    crystal_buf *lo, crystal_buf *hi)
{
  const uint *src = (uint*) all->buf.ptr;
  const uint *end = (uint*) src+all->n;
  uint *target, *lop, *hip;
  lo->n=hi->n=0;
  lo->buf.buffer_reserve(all->n*sizeof(uint));
  hi->buf.buffer_reserve(all->n*sizeof(uint));
  lop = (uint*) lo->buf.ptr;
  hip = (uint*) hi->buf.ptr;
  while(src!=end)
  {
    uint chunk_len = 3 + src[2];
    if(src[0]<cutoff) {
      target=lop;
      lo->n+=chunk_len;
      lop+=chunk_len;
    }
    else {
      target=hip;
      hi->n+=chunk_len;
      hip+=chunk_len;
    }
    memcpy(target,src,chunk_len*sizeof(uint));
    src+=chunk_len;
  }
}

//Send message to target process
void gs_data::crystal_data::send_(uint target, int recvn)
{
  MPI_Request req[3] =
  { MPI_REQUEST_NULL, MPI_REQUEST_NULL, MPI_REQUEST_NULL};
  MPI_Status status[3];
  uint count[2]={ 0,0}, sum, *recv[2];
  crystal_buf *t;
  int i;

  (void)VALGRIND_CHECK_MEM_IS_DEFINED( &send->n, sizeof(uint) );
  MPI_Isend((void*)&send->n,sizeof(uint),MPI_UNSIGNED_CHAR,
      target ,_id ,_comm,&req[ 0]);
  for (i=0; i<recvn; ++i)
    MPI_Irecv((void*)&count[i] ,sizeof(uint),MPI_UNSIGNED_CHAR,
      target+i,target+i,_comm,&req[i+1]);
  MPI_Waitall(recvn+1,req,status);
  sum = keep->n;
  for (i=0; i<recvn; ++i)
    sum+=count[i];
  keep->buf.buffer_reserve(sum*sizeof(uint));
  recv[0]= (uint*) keep->buf.ptr;
  recv[0]+=keep->n;
  recv[1]=recv[0]+count[0];
  keep->n=sum;

  (void)VALGRIND_CHECK_MEM_IS_DEFINED( send->buf.ptr,send->n*sizeof(uint) );
  MPI_Isend((void*)send->buf.ptr,send->n*sizeof(uint),
      MPI_UNSIGNED_CHAR,target,_id,_comm,&req[0]);
  if (recvn)
  {
    MPI_Irecv((void*)recv[0],count[0]*sizeof(uint),MPI_UNSIGNED_CHAR,
        target,target,_comm,&req[1]);
    if (recvn==2)
      MPI_Irecv((void*)recv[1],count[1]*sizeof(uint),MPI_UNSIGNED_CHAR,
        target+1,target+1,_comm,&req[2]);
  }
  MPI_Waitall(recvn+1,req,status);

  t=all;
  all=keep;
  keep=t;
}

void gs_data::crystal_data::crystal_router()
{
  uint bl=0, bh, n=_num, nl, target;
  int recvn;
  crystal_buf *lo, *hi;
  while (n>1)
  {
    nl = n/2, bh = bl+nl;
    if (_id < bh)
    {
      target=_id+nl;
      recvn=(n&1 && _id==bh-1)?2:1;
      lo=keep;
      hi=send;
    }
    else {
      target=_id-nl;
      recvn=(target==bh)?(--target,0):1;
      hi=keep;
      lo=send;
    }
    partition(bh,lo,hi);
    send_(target,recvn);
    if(_id<bh)
      n=nl;
    else {
      n-=nl;
      bl=bh;
    }
  }
}

#define UINT_PER_X(X) ((sizeof(X)+sizeof(uint)-1)/sizeof(uint))
#define UINT_PER_REAL UINT_PER_X(realType)
#define UINT_PER_LONG UINT_PER_X(slong)
#define UINT_PER_ULONG UINT_PER_X(Ulong)

/*-------------------------------------------------------------------------
 Transfer
 -------------------------------------------------------------------------*/
ErrorCode gs_data::crystal_data::gs_transfer(int dynamic,
    moab::TupleList &tl, unsigned pf)
{

  unsigned mi,ml,mul,mr;
  tl.getTupleSize(mi, ml, mul, mr);

  const unsigned tsize = (mi-1) + ml*UINT_PER_LONG + mul*UINT_PER_ULONG +
      mr*UINT_PER_REAL;
  sint p, lp = -1;
  sint *ri;
  slong *rl;
  Ulong *rul;
  realType *rr;
  uint i, j, *buf, *len=0, *buf_end;

  /* sort to group by target proc */
  if (pf >= mi)
    return moab::MB_MEMORY_ALLOCATION_FAILED;
    //fail("pf expected to be in vi array (%d, %d)", pf, mi);
  tl.sort(pf,&all->buf);

  /* pack into buffer for crystal router */
  all->buf.buffer_reserve((tl.get_n()*(3+tsize))*sizeof(uint));
  all->n=0;
  buf = (uint*) all->buf.ptr;

  bool canWrite = tl.get_writeEnabled();
  if(!canWrite) tl.enableWriteAccess();

  ri=tl.vi_wr;
  rl=tl.vl_wr;
  rul=tl.vul_wr;
  rr=tl.vr_wr;

  for (i=tl.get_n();i;--i)
  {
    p = ri[pf];
    if (p!=lp)
    {
      lp = p;
      *buf++ = p; /* target */
      *buf++ = _id; /* source */
      len = buf++;
      *len=0; /* length */
      all->n += 3;
    }
    for (j=0;j<mi;++j,++ri)
      if(j!=pf)
        *buf++ = *ri;
    for (j=ml;j;--j,++rl)
    {
      memcpy(buf,rl,sizeof(slong));
      buf+=UINT_PER_LONG;
    }
    for (j=mul;j;--j,++rul)
    {
      memcpy(buf,rul,sizeof(Ulong));
      buf+=UINT_PER_ULONG;
    }
    for (j=mr;j;--j,++rr)
    {
      memcpy(buf,rr,sizeof(realType ));
      buf+=UINT_PER_REAL;
    }
    *len += tsize, all->n += tsize;
  }

  crystal_router();

  /* unpack */
  buf = (uint*)all->buf.ptr;
  buf_end = buf + all->n;
  tl.set_n(0);
  ri=tl.vi_wr;
  rl=tl.vl_wr;
  rul=tl.vul_wr;
  rr=tl.vr_wr;

  while (buf != buf_end)
  {
    sint llen;
    buf++; /* target ( == this proc ) */
    p = *buf++; /* source */
    llen = *buf++; /* length */
    while (llen>0)
    {
      if (tl.get_n()==tl.get_max())
      {
        if (!dynamic)
        {
          tl.set_n(tl.get_max() + 1);
          if(!canWrite)
            tl.disableWriteAccess();
          return moab::MB_SUCCESS;
        }
        ErrorCode rval = tl.resize(tl.get_max() + (1+tl.get_max())/2 + 1);
        if(rval != moab::MB_SUCCESS)
        {
          if(!canWrite)
            tl.disableWriteAccess();
          return rval;
        }
        ri = tl.vi_wr + mi*tl.get_n();
        rl = tl.vl_wr + ml*tl.get_n();
        rul = tl.vul_wr + mul*tl.get_n();
        rr = tl.vr_wr + mr*tl.get_n();
      }
      tl.inc_n();
      for (j=0;j<mi;++j)
        if(j!=pf)
          *ri++ = *buf++;
      else
        *ri++ = p;
      for (j=ml;j;--j) {
        memcpy(rl++,buf,sizeof(slong));
        buf+=UINT_PER_LONG;
      }
      for (j=mul;j;--j) {
        memcpy(rul++,buf,sizeof(Ulong));
        buf+=UINT_PER_ULONG;
      }
      for (j=mr;j;--j) {
        memcpy(rr++,buf,sizeof(realType ));
        buf+=UINT_PER_REAL;
      }
      llen-=tsize;
    }
  }

  if(!canWrite) tl.disableWriteAccess();
  return moab::MB_SUCCESS;
}
#endif

/*--------------------------------------------------------------------------
 Combined Execution
 --------------------------------------------------------------------------*/

void gs_data::gs_data_op(realType *u, int op)
{
  local_condense(u, op, this->local_cm);
#ifdef MOAB_HAVE_MPI
  this->nlinfo->nonlocal(u,op,_comm);
#endif
  local_uncondense(u, local_cm);
}

void gs_data::gs_data_op_vec(realType *u, uint n, int op)
{
#ifdef MOAB_HAVE_MPI
  if (n>nlinfo->_maxv)
    moab::fail("%s: initialized with max vec size = %d,"
      " but called with vec size = %d\n",__FILE__,nlinfo->_maxv,n);
#endif
  local_condense_vec(u, n, op, local_cm);
#ifdef MOAB_HAVE_MPI
  this->nlinfo->nonlocal_vec(u,n,op,_comm);
#endif
  local_uncondense_vec(u, n, local_cm);
}

void gs_data::gs_data_op_many(realType **u, uint n, int op)
{
  uint i;
#ifdef MOAB_HAVE_MPI
  if (n>nlinfo->_maxv)
    moab::fail("%s: initialized with max vec size = %d,"
      " but called with vec size = %d\n",__FILE__,nlinfo->_maxv,n);
#endif
  for (i = 0; i < n; ++i)
    local_condense(u[i], op, local_cm);

  moab::fail("%s: initialized with max vec size = %d,"
      " but called with vec size = %d\n", __FILE__, 6, n);

#ifdef MOAB_HAVE_MPI
  this->nlinfo->nonlocal_many(u,n,op,_comm);
#endif
  for (i = 0; i < n; ++i)
    local_uncondense(u[i], local_cm);
}

/*--------------------------------------------------------------------------
 Setup
 --------------------------------------------------------------------------*/

ErrorCode gs_data::initialize(uint n, const long *label, const Ulong *ulabel,
    uint maxv, const unsigned int nlabels, const unsigned int nulabels,
    crystal_data *crystal)
{
  nlinfo = NULL;
  unsigned int j;
  TupleList nonzero, primary;
  ErrorCode rval;
#ifdef MOAB_HAVE_MPI
  TupleList shared;
#else
  moab::TupleList::buffer buf;
#endif
  (void)VALGRIND_CHECK_MEM_IS_DEFINED(label, nlabels * sizeof(long));
  (void)VALGRIND_CHECK_MEM_IS_DEFINED(ulabel, nlabels * sizeof(Ulong));
#ifdef MOAB_HAVE_MPI
  MPI_Comm_dup(crystal->_comm,&this->_comm);
#else
  buf.buffer_init(1024);
#endif

  /* construct list of nonzeros: (index ^, label) */
  nonzero.initialize(1, nlabels, nulabels, 0, n);
  nonzero.enableWriteAccess();
  {
    uint i;
    sint *nzi;
    long *nzl;
    Ulong *nzul;
    nzi = nonzero.vi_wr;
    nzl = nonzero.vl_wr;
    nzul = nonzero.vul_wr;
    for (i = 0; i < n; ++i)
      if (label[nlabels * i] != 0)
      {
        nzi[0] = i;
        for (j = 0; j < nlabels; j++)
          nzl[j] = label[nlabels * i + j];
        for (j = 0; j < nulabels; j++)
          nzul[j] = ulabel[nulabels * i + j];
        nzi++;
        nzl += nlabels;
        nzul += nulabels;
        nonzero.inc_n();
      }
  }

  /* sort nonzeros by label: (index ^2, label ^1) */
#ifndef MOAB_HAVE_MPI
  nonzero.sort(1, &buf);
#else
  nonzero.sort(1,&crystal->all->buf);
#endif

  /* build list of unique labels w/ lowest associated index:
   (index in nonzero ^, primary (lowest) index in label, count, label(s),
   ulabel(s)) */
  primary.initialize(3, nlabels, nulabels, 0, nonzero.get_n());
  primary.enableWriteAccess();
  {
    uint i;
    sint *nzi = nonzero.vi_wr, *pi = primary.vi_wr;
    slong *nzl = nonzero.vl_wr, *pl = primary.vl_wr;
    Ulong *nzul = nonzero.vul_wr, *pul = primary.vul_wr;
    slong last = -1;
    for (i = 0; i < nonzero.get_n();
          ++i, nzi += 1, nzl += nlabels, nzul += nulabels)
    {
      if (nzl[0] == last)
      {
        ++pi[-1];
        continue;
      }
      last = nzl[0];
      pi[0] = i;
      pi[1] = nzi[0];
      for (j = 0; j < nlabels; j++)
        pl[j] = nzl[j];
      for (j = 0; j < nulabels; j++)
        pul[j] = nzul[j];
      pi[2] = 1;
      pi += 3, pl += nlabels;
      pul += nulabels;
      primary.inc_n();
    }
  }

  /* calculate size of local condense map */
  {
    uint i, count = 1;
    sint *pi = primary.vi_wr;
    for (i = primary.get_n(); i; --i, pi += 3)
      if (pi[2] > 1)
        count += pi[2] + 1;
    this->local_cm = (sint*) malloc(count * sizeof(sint));
  }

  /* sort unique labels by primary index:
   (nonzero index ^2, primary index ^1, count, label ^2) */
#ifndef MOAB_HAVE_MPI
  primary.sort(0, &buf);
  buf.reset();
  //buffer_free(&buf);
#else
  primary.sort(0,&crystal->all->buf);
#endif

  /* construct local condense map */
  {
    uint i, ln;
    sint *pi = primary.vi_wr;
    sint *cm = this->local_cm;
    for (i = primary.get_n(); i > 0; --i, pi += 3)
      if ((ln = pi[2]) > 1)
      {
        sint *nzi = nonzero.vi_wr + 1 * pi[0];
        for (j = ln; j > 0; --j, nzi += 1)
          *cm++ = nzi[0];
        *cm++ = -1;
      }
    *cm++ = -1;
  }
  nonzero.reset();
#ifndef MOAB_HAVE_MPI
  primary.reset();
#else
  /* assign work proc by label modulo np */
  {
    uint i;
    sint *pi=primary.vi_wr;
    slong *pl=primary.vl_wr;
    for (i=primary.get_n(); i; --i, pi+=3, pl+=nlabels)
      pi[0]=pl[0]%crystal->_num;
  }
  rval = crystal->gs_transfer(1,primary,0); /* transfer to work procs */
  if (rval != MB_SUCCESS)
    return rval;
  /* primary: (source proc, index on src, useless, label) */
  /* sort by label */
  primary.sort(3,&crystal->all->buf);
  /* add sentinel to primary list */
  if (primary.get_n()==primary.get_max())
    primary.resize( (primary.get_max() ? primary.get_max()+(primary.get_max()+1)/2+1 : 2));
  primary.vl_wr[nlabels*primary.get_n()] = -1;
  /* construct shared list: (proc1, proc2, index1, label) */
#ifdef MOAB_HAVE_MPI
  shared.initialize(3,nlabels,nulabels,0,primary.get_n());
  shared.enableWriteAccess();
#endif
  {
    sint *pi1=primary.vi_wr, *si=shared.vi_wr;
    slong lbl, *pl1=primary.vl_wr, *sl=shared.vl_wr;
    Ulong *pul1=primary.vul_wr, *sul=shared.vul_wr;
    for (  ;(lbl=pl1[0])!=-1;  pi1+=3, pl1+=nlabels, pul1+=nulabels)
    {
      sint *pi2=pi1+3;
      slong *pl2=pl1+nlabels;
      Ulong *pul2=pul1+nulabels;
      for (  ;pl2[0]==lbl;  pi2+=3, pl2+=nlabels, pul2+=nulabels)
      {
        if (shared.get_n()+2 > shared.get_max())
          shared.resize((shared.get_max() ? shared.get_max()+(shared.get_max()+1)/2+1 : 2)),
        si=shared.vi_wr+shared.get_n()*3;
        sl=shared.vl_wr+shared.get_n()*nlabels;
        sul=shared.vul_wr+shared.get_n()*nulabels;
        si[0] = pi1[0];
        si[1] = pi2[0];
        si[2] = pi1[1];
        for (j = 0; j < nlabels; j++)
          sl[j] = pl2[j];
        for (j = 0; j < nulabels; j++)
          sul[j] = pul2[j];
        si+=3;
        sl+=nlabels;
        sul+=nulabels;
        shared.inc_n();
        si[0] = pi2[0];
        si[1] = pi1[0];
        si[2] = pi2[1];
        for (j = 0; j < nlabels; j++)
          sl[j] = pl1[j];
        for (j = 0; j < nulabels; j++)
          sul[j] = pul1[j];
        si+=3;
        sl+=nlabels;
        sul+=nulabels;
        shared.inc_n();
      }
    }
  }
  primary.reset();
  rval = crystal->gs_transfer(1,shared,0); /* segfaulting transfer to dest procs */
  if (rval != MB_SUCCESS)
  return rval;
  /* shared list: (useless, proc2, index, label) */
  /* sort by label */
  shared.sort(3,&crystal->all->buf);
  /* sort by partner proc */
  shared.sort(1,&crystal->all->buf);
  /* count partner procs */
  {
    uint i, count=0;
    sint proc=-1,*si=shared.vi_wr;
    for (i=shared.get_n(); i; --i,si+=3)
      if (si[1]!=proc) {
        ++count;
        proc=si[1];
      }
    //this->nlinfo = new nonlocal_info();
    //this->nlinfo->initialize(count,shared.get_n(),
    //                          nlabels, nulabels, maxv);
    this->nlinfo = new nonlocal_info(count,shared.get_n(),
        nlabels, nulabels, maxv);
  }
  /* construct non-local info */
  {
    uint i;
    sint proc=-1,*si=shared.vi_wr;
    slong *sl = shared.vl_wr;
    Ulong *ul = shared.vul_wr;
    uint *target = this->nlinfo->_target;
    uint *nshared = this->nlinfo->_nshared;
    uint *sh_ind = this->nlinfo->_sh_ind;
    slong *slabels = this->nlinfo->_slabels;
    Ulong *ulabels = this->nlinfo->_ulabels;
    for (i=shared.get_n(); i; --i,si+=3)
    {
      if (si[1]!=proc)
      {
        proc=si[1];
        *target++ = proc;
        *nshared++ = 0;
      }
      ++nshared[-1];
      *sh_ind++=si[2];
      // don't store 1st slabel
      sl++;
      for (j = 0; j < nlabels-1; j++)
        slabels[j] = sl[j];
      for (j = 0; j < nulabels; j++)
        ulabels[j] = ul[j];
      slabels+=nlabels-1;
      ulabels+=nulabels;
      sl+=nlabels-1;
      ul+=nulabels;
    }
  }
  shared.reset();
#endif
  return MB_SUCCESS;
}

void gs_data::reset()
{
  free(local_cm);
  local_cm = NULL;
#ifdef MOAB_HAVE_MPI
  if(nlinfo != NULL)
  {
    nlinfo->nlinfo_free();
    delete this->nlinfo;
    MPI_Comm_free(&_comm);
    nlinfo = NULL;
  }
#endif
}

} //namespace

