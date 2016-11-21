#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <iostream>

#include "moab/TupleList.hpp"

namespace moab {

void fail(const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  exit(1);
}

TupleList::buffer::buffer(size_t sz)
{
  ptr = NULL;
  buffSize = 0;
  this->buffer_init_(sz, __FILE__);
}

TupleList::buffer::buffer()
{
  buffSize = 0;
  ptr = NULL;
}

void TupleList::buffer::buffer_init_(size_t sizeIn, const char *file)
{
  this->buffSize = sizeIn;
  void *res = malloc(this->buffSize);
  if (!res && buffSize > 0)
    fail("%s: allocation of %d bytes failed\n", file, (int) buffSize);
  ptr = (char*) res;
}

void TupleList::buffer::buffer_reserve_(size_t min, const char *file)
{
  if (this->buffSize < min)
  {
    size_t newSize = this->buffSize;
    newSize += newSize / 2 + 1;
    if (newSize < min)
      newSize = min;
    void *res = realloc(ptr, newSize);
    if (!res && newSize > 0)
      fail("%s: reallocation of %d bytes failed\n", file, newSize);
    ptr = (char*) res;
    this->buffSize = newSize;
  }
}

void TupleList::buffer::reset()
{
  free(ptr);
  ptr = NULL;
  buffSize = 0;
}

TupleList::TupleList(uint p_mi, uint p_ml, uint p_mul, uint p_mr, uint p_max)
: vi(NULL), vl(NULL), vul(NULL), vr(NULL),
  last_sorted(-1)
{
  initialize(p_mi, p_ml, p_mul, p_mr, p_max);
}

TupleList::TupleList()
: vi_rd(NULL), vl_rd(NULL), vul_rd(NULL), vr_rd(NULL),
  mi(0), ml(0), mul(0), mr(0),
  n(0), max(0),
  vi(NULL), vl(NULL), vul(NULL), vr(NULL),
  last_sorted(-1)
{
  disableWriteAccess();
}

// Allocates space for the tuple list in memory according to parameters
void TupleList::initialize(uint p_mi, uint p_ml, uint p_mul, uint p_mr,
    uint p_max)
{
  this->n = 0;
  this->max = p_max;
  this->mi = p_mi;
  this->ml = p_ml;
  this->mul = p_mul;
  this->mr = p_mr;
  size_t sz;

  if (max * mi > 0)
  {
    sz = max * mi * sizeof(sint);
    void *resi = malloc(sz);
    if (!resi && max * mi > 0)
      fail("%s: allocation of %d bytes failed\n", __FILE__, (int) sz);
    vi = (sint*) resi;
  }
  else
    vi = NULL;
  if (max * ml > 0)
  {
    sz = max * ml * sizeof(slong);
    void *resl = malloc(sz);
    if (!resl && max * ml > 0)
      fail("%s: allocation of %d bytes failed\n", __FILE__, (int) sz);
    vl = (slong*) resl;
  }
  else
    vl = NULL;
  if (max * mul > 0)
  {
    sz = max * mul * sizeof(Ulong);
    void *resu = malloc(sz);
    if (!resu && max * mul > 0)
      fail("%s: allocation of %d bytes failed\n", __FILE__, (int) sz);
    vul = (Ulong*) resu;
  }
  else
    vul = NULL;
  if (max * mr > 0)
  {
    sz = max * mr * sizeof(realType);
    void *resr = malloc(sz);
    if (!resr && max * ml > 0)
      fail("%s: allocation of %d bytes failed\n", __FILE__, (int) sz);
    vr = (realType*) resr;
  }
  else
    vr = NULL;

  //Begin with write access disabled
  this->disableWriteAccess();

  //Set read variables
  vi_rd = vi;
  vl_rd = vl;
  vul_rd = vul;
  vr_rd = vr;
}

// Resizes a tuplelist to the given uint max
ErrorCode TupleList::resize(uint maxIn)
{
  this->max = maxIn;
  size_t sz;

  if (vi || (max * mi > 0))
  {
    sz = max * mi * sizeof(sint);
    void *resi = realloc(vi, sz);
    if (!resi && max * mi > 0)
    {
      fail("%s: allocation of %d bytes failed\n", __FILE__, (int) sz);
      return moab::MB_MEMORY_ALLOCATION_FAILED;
    }
    vi = (sint*) resi;
  }
  if (vl || (max * ml > 0))
  {
    sz = max * ml * sizeof(slong);
    void *resl = realloc(vl, sz);
    if (!resl && max * ml > 0)
    {
      fail("%s: allocation of %d bytes failed\n", __FILE__, (int) sz);
      return moab::MB_MEMORY_ALLOCATION_FAILED;
    }
    vl = (slong*) resl;
  }
  if (vul || (max * mul > 0))
  {
    sz = max * mul * sizeof(Ulong);
    void *resu = realloc(vul, sz);
    if (!resu && max * mul > 0)
    {
      fail("%s: allocation of %d bytes failed\n", __FILE__, (int) sz);
      return moab::MB_MEMORY_ALLOCATION_FAILED;
    }
    vul = (Ulong*) resu;
  }
  if (vr || (max * mr > 0))
  {
    sz = max * mr * sizeof(realType);
    void *resr = realloc(vr, sz);
    if (!resr && max * mr > 0)
    {
      fail("%s: allocation of %d bytes failed\n", __FILE__, (int) sz);
      return moab::MB_MEMORY_ALLOCATION_FAILED;
    }
    vr = (realType*) resr;
  }

  //Set read variables
  vi_rd = vi;
  vl_rd = vl;
  vul_rd = vul;
  vr_rd = vr;

  //Set the write variables if necessary
  if (writeEnabled)
  {
    vi_wr = vi;
    vl_wr = vl;
    vul_wr = vul;
    vr_wr = vr;
  }
  return moab::MB_SUCCESS;
}

// Frees the memory used by the tuplelist
void TupleList::reset()
{
  //free up the pointers
  free(vi);
  free(vl);
  free(vul);
  free(vr);
  //Set them all to null
  vr = NULL;
  vi = NULL;
  vul = NULL;
  vl = NULL;
  //Set the read and write pointers to null
  disableWriteAccess();
  vi_rd = NULL;
  vl_rd = NULL;
  vul_rd = NULL;
  vr_rd = NULL;
}

// Increments n; if n>max, increase the size of the tuplelist
void TupleList::reserve()
{
  n++;
  while (n > max)
    resize((max ? max + max / 2 + 1 : 2));
  last_sorted = -1;
}

// Given the value and the position in the field, finds the index of the tuple
// to which the value belongs
int TupleList::find(unsigned int key_num, sint value)
{
  // we are passing an int, no issue, leave it at long
  long uvalue = (long) value;
  if (!(key_num > mi))
  {
    // Binary search: only if the tuple_list is sorted
    if (last_sorted == (int) key_num)
    {
      int lb = 0, ub = n, index; // lb=lower bound, ub=upper bound, index=mid
      for (; lb <= ub;)
      {
        index = (lb + ub) / 2;
        if (vi[index * mi + key_num] == uvalue)
          return index;
        else if (vi[index * mi + key_num] > uvalue)
          ub = index - 1;
        else if (vi[index * mi + key_num] < uvalue)
          lb = index + 1;
      }
    }
    else
    {
      // Sequential search: if tuple_list is not sorted
      for (uint index = 0; index < n; index++)
      {
        if (vi[index * mi + key_num] == uvalue)
          return index;
      }
    }
  }
  return -1; // If the value wasn't present or an invalid key was given
}

int TupleList::find(unsigned int key_num, slong value)
{
  long uvalue = (long) value;
  if (!(key_num > ml))
  {
    if (last_sorted - mi == key_num)
    {
      int lb = 0, ub = n, index; // lb=lower bound, ub=upper bound, index=mid
      for (; lb <= ub;)
      {
        index = (lb + ub) / 2;
        if (vl[index * ml + key_num] ==  uvalue)
          return index;
        else if (vl[index * ml + key_num] > uvalue)
          ub = index - 1;
        else if (vl[index * ml + key_num] < uvalue)
          lb = index + 1;
      }
    }
    else
    {
      // Sequential search: if tuple_list is not sorted
      for (uint index = 0; index < n; index++)
      {
        if (vl[index * ml + key_num] == uvalue)
          return index;
      }
    }
  }
  return -1; // If the value wasn't present or an invalid key was given
}

int TupleList::find(unsigned int key_num, Ulong value)
{
  if (!(key_num > mul))
  {
    if (last_sorted - mi - ml == key_num)
    {
      int lb = 0, ub = n - 1, index; // lb=lower bound, ub=upper bound, index=mid
      for (; lb <= ub;)
      {
        index = (lb + ub) / 2;
        if (vul[index * mul + key_num] == value)
          return index;
        else if (vul[index * mul + key_num] > value)
          ub = index - 1;
        else if (vul[index * mul + key_num] < value)
          lb = index + 1;
      }
    }
    else
    {
      // Sequential search: if tuple_list is not sorted
      for (uint index = 0; index < n; index++)
      {
        if (vul[index * mul + key_num] == value)
          return index;
      }
    }
  }
  return -1; // If the value wasn't present or an invalid key was given
}

int TupleList::find(unsigned int key_num, realType value)
{
  if (!(key_num > mr))
  {
    // Sequential search: TupleList cannot be sorted by reals
    for (uint index = 0; index < n; index++)
    {
      if (vr[index * mr + key_num] == value)
        return index;
    }
  }
  return -1; // If the value wasn't present or an invalid key was given
}

sint TupleList::get_sint(unsigned int index, unsigned int m)
{
  if (mi > m && n > index)
    return vi[index * mi + m];
  return 0;
}

slong TupleList::get_int(unsigned int index, unsigned int m)
{
  if (ml > m && n > index)
    return vl[index * ml + m];
  return 0;
}

Ulong TupleList::get_ulong(unsigned int index, unsigned int m)
{
  if (mul > m && n > index)
    return vul[index * mul + m];
  return 0;
}

realType TupleList::get_double(unsigned int index, unsigned int m)
{
  if (mr > m && n > index)
    return vr[index * mr + m];
  return 0;
}

ErrorCode TupleList::get(unsigned int index, const sint *&sp, const slong *&ip,
    const Ulong *&lp, const realType *&dp)
{
  if (index <= n)
  {
    if (mi)
      *&sp = &vi[index * mi];
    else
      *&sp = NULL;
    if (ml)
      *&ip = &vl[index * ml];
    else
      *&ip = NULL;
    if (mul)
      *&lp = &vul[index * mul];
    else
      *&lp = NULL;
    if (mr)
      *&dp = &vr[index * mr];
    else
      *&dp = NULL;

    return MB_SUCCESS;
  }
  return MB_FAILURE;
}

unsigned int TupleList::push_back(sint *sp, slong *ip, Ulong *lp, realType *dp)
{
  reserve();
  if (mi)
    memcpy(&vi[mi * (n - 1)], sp, mi * sizeof(sint));
  if (ml)
    memcpy(&vl[ml * (n - 1)], ip, ml * sizeof(long));
  if (mul)
    memcpy(&vul[mul * (n - 1)], lp, mul * sizeof(Ulong));
  if (mr)
    memcpy(&vr[mr * (n - 1)], dp, mr * sizeof(realType));

  last_sorted = -1;
  return n - 1;
}

void TupleList::enableWriteAccess()
{
  writeEnabled = true;
  last_sorted = -1;
  vi_wr = vi;
  vl_wr = vl;
  vul_wr = vul;
  vr_wr = vr;
}

void TupleList::disableWriteAccess()
{
  writeEnabled = false;
  vi_wr = NULL;
  vl_wr = NULL;
  vul_wr = NULL;
  vr_wr = NULL;
}

void TupleList::getTupleSize(uint &mi_out, uint &ml_out, uint &mul_out,
    uint &mr_out) const
{
  mi_out = mi;
  ml_out = ml;
  mul_out = mul;
  mr_out = mr;
}

uint TupleList::inc_n()
{
  //Check for direct write access
  if (!writeEnabled)
  {
    enableWriteAccess();
  }
  n++;
  return n;
}

void TupleList::set_n(uint n_in)
{
  //Check for direct write access;
  if (!writeEnabled)
  {
    enableWriteAccess();
  }
  n = n_in;
}

void TupleList::print(const char *name) const
{
  std::cout << "Printing Tuple " << name << "===================" << std::endl;
  unsigned long i = 0, l = 0, ul = 0, r = 0;
  for (uint k = 0; k < n; k++)
  {
    for (uint j = 0; j < mi; j++)
    {
      std::cout << vi[i++] << " | ";
    }
    for (uint j = 0; j < ml; j++)
    {
      std::cout << vl[l++] << " | ";
    }
    for (uint j = 0; j < mul; j++)
    {
      std::cout << vul[ul++] << " | ";
    }
    for (uint j = 0; j < mr; j++)
    {
      std::cout << vr[r++] << " | ";
    }
    std::cout << std::endl;
  }
  std::cout << "=======================================" << std::endl
      << std::endl;
}

void TupleList::permute(uint *perm, void *work)
{
  const unsigned int_size = mi * sizeof(sint), long_size = ml * sizeof(slong),
      Ulong_size = mul * sizeof(Ulong), real_size = mr * sizeof(realType);
  if (mi)
  {
    uint *p = perm, *pe = p + n;
    char *sorted = (char *) work;
    while (p != pe)
      memcpy((void *) sorted, &vi[mi * (*p++)], int_size), sorted += int_size;
    memcpy(vi, work, int_size * n);
  }
  if (ml)
  {
    uint *p = perm, *pe = p + n;
    char *sorted = (char *) work;
    while (p != pe)
      memcpy((void *) sorted, &vl[ml * (*p++)], long_size), sorted += long_size;
    memcpy(vl, work, long_size * n);
  }
  if (mul)
  {
    uint *p = perm, *pe = p + n;
    char *sorted = (char *) work;
    while (p != pe)
      memcpy((void *) sorted, &vul[mul * (*p++)], Ulong_size), sorted +=
          Ulong_size;
    memcpy(vul, work, Ulong_size * n);
  }
  if (mr)
  {
    uint *p = perm, *pe = p + n;
    char *sorted = (char *) work;
    while (p != pe)
      memcpy((void *) sorted, &vr[mr * (*p++)], real_size), sorted += real_size;
    memcpy(vr, work, real_size * n);
  }
}

# define umax_2(a, b) (((a)>(b)) ? (a):(b))

ErrorCode TupleList::sort(uint key, TupleList::buffer *buf)
{
  const unsigned int_size = mi * sizeof(sint);
  const unsigned long_size = ml * sizeof(slong);
  const unsigned Ulong_size = mul * sizeof(Ulong);
  const unsigned real_size = mr * sizeof(realType);
  const unsigned width = umax_2(umax_2(int_size,long_size),
      umax_2(Ulong_size,real_size));
  unsigned data_size =
      key >= mi ? sizeof(SortData<long> ) : sizeof(SortData<uint> );
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32)
  if (key>=mi+ml)
    data_size = sizeof(SortData<Ulong> );
#endif 

  uint work_min = n * umax_2(2*data_size,sizeof(sint)+width);
  uint *work;
  buf->buffer_reserve(work_min);
  work = (uint *) buf->ptr;
  if (key < mi)
    index_sort((uint *) &vi[key], n, mi, work, (SortData<uint>*) work);
  else if (key < mi + ml)
    index_sort((long*) &vl[key - mi], n, ml, work, (SortData<long>*) work);
  else if (key < mi + ml + mul)
    index_sort((Ulong*) &vul[key - mi - ml], n, mul, work,
        (SortData<Ulong>*) work);
  else
    return MB_NOT_IMPLEMENTED;

  permute(work, work + n);

  if (!writeEnabled)
    last_sorted = key;
  return MB_SUCCESS;
}

#undef umax_2

#define DIGIT_BITS   8
#define DIGIT_VALUES (1<<DIGIT_BITS)
#define DIGIT_MASK   ((Value)(DIGIT_VALUES-1))
#define CEILDIV(a,b) (((a)+(b)-1)/(b))
#define DIGITS       CEILDIV(CHAR_BIT*sizeof(Value),DIGIT_BITS)
#define VALUE_BITS   (DIGIT_BITS*DIGITS)
#define COUNT_SIZE   (DIGITS*DIGIT_VALUES)

/* used to unroll a tiny loop: */
#define COUNT_DIGIT_01(n,i) \
    if(n>i) count[i][val&DIGIT_MASK]++, val>>=DIGIT_BITS
#define COUNT_DIGIT_02(n,i) COUNT_DIGIT_01(n,i); COUNT_DIGIT_01(n,i+ 1)
#define COUNT_DIGIT_04(n,i) COUNT_DIGIT_02(n,i); COUNT_DIGIT_02(n,i+ 2)
#define COUNT_DIGIT_08(n,i) COUNT_DIGIT_04(n,i); COUNT_DIGIT_04(n,i+ 4)
#define COUNT_DIGIT_16(n,i) COUNT_DIGIT_08(n,i); COUNT_DIGIT_08(n,i+ 8)
#define COUNT_DIGIT_32(n,i) COUNT_DIGIT_16(n,i); COUNT_DIGIT_16(n,i+16)
#define COUNT_DIGIT_64(n,i) COUNT_DIGIT_32(n,i); COUNT_DIGIT_32(n,i+32)

template<class Value>
Value TupleList::radix_count(const Value *A, const Value *end, Index stride,
    Index count[DIGITS][DIGIT_VALUES])
{
  Value bitorkey = 0;
  memset(count, 0, COUNT_SIZE * sizeof(Index));
  do
  {
    Value val = *A;
    bitorkey |= val;
    COUNT_DIGIT_64(DIGITS, 0);
    // above macro expands to:
    //if(DIGITS> 0) count[ 0][val&DIGIT_MASK]++, val>>=DIGIT_BITS;
    //if(DIGITS> 1) count[ 1][val&DIGIT_MASK]++, val>>=DIGIT_BITS;
    //  ...
    //if(DIGITS>63) count[63][val&DIGIT_MASK]++, val>>=DIGIT_BITS;

  } while (A += stride, A != end);
  return bitorkey;
}

#undef COUNT_DIGIT_01
#undef COUNT_DIGIT_02
#undef COUNT_DIGIT_04
#undef COUNT_DIGIT_08
#undef COUNT_DIGIT_16
#undef COUNT_DIGIT_32
#undef COUNT_DIGIT_64

void TupleList::radix_offsets(Index *c)
{
  Index sum = 0, t, *ce = c + DIGIT_VALUES;
  do
    t = *c, *c++ = sum, sum += t;
  while (c != ce);
}

template<class Value>
unsigned TupleList::radix_zeros(Value bitorkey,
    Index count[DIGITS][DIGIT_VALUES], unsigned *shift, Index **offsets)
{
  unsigned digits = 0, sh = 0;
  Index *c = &count[0][0];
  do
  {
    if (bitorkey & DIGIT_MASK)
      *shift++ = sh, *offsets++ = c, ++digits, radix_offsets(c);
  } while (bitorkey >>= DIGIT_BITS,sh += DIGIT_BITS,c += DIGIT_VALUES,sh
      != VALUE_BITS);
  return digits;
}

template<class Value>
void TupleList::radix_index_pass_b(const Value *A, Index n, Index stride,
    unsigned sh, Index *off, SortData<Value> *out)
{
  Index i = 0;
  do
  {
    Value v = *A;
    SortData<Value> *d = &out[off[(v >> sh) & DIGIT_MASK]++];
    d->v = v, d->i = i++;
  } while (A += stride, i != n);
}

template<class Value>
void TupleList::radix_index_pass_m(const SortData<Value> *src,
    const SortData<Value> *end, unsigned sh, Index *off, SortData<Value> *out)
{
  do
  {
    SortData<Value> *d = &out[off[(src->v >> sh) & DIGIT_MASK]++];
    d->v = src->v, d->i = src->i;
  } while (++src != end);
}

template<class Value>
void TupleList::radix_index_pass_e(const SortData<Value> *src,
    const SortData<Value> *end, unsigned sh, Index *off, Index *out)
{
  do
    out[off[(src->v >> sh) & DIGIT_MASK]++] = src->i;
  while (++src != end);
}

template<class Value>
void TupleList::radix_index_pass_be(const Value *A, Index n, Index stride,
    unsigned sh, Index *off, Index *out)
{
  Index i = 0;
  do
    out[off[(*A >> sh) & DIGIT_MASK]++] = i++;
  while (A += stride, i != n);
}

template<class Value>
void TupleList::radix_index_sort(const Value *A, Index n, Index stride,
    Index *idx, SortData<Value> *work)
{
  Index count[DIGITS][DIGIT_VALUES];
  Value bitorkey = radix_count(A, A + n * stride, stride, count);
  unsigned shift[DIGITS];
  Index *offsets[DIGITS];
  unsigned digits = radix_zeros(bitorkey, count, shift, offsets);
  if (digits == 0)
  {
    Index i = 0;
    do
      *idx++ = i++;
    while (i != n);
  }
  else if (digits == 1)
  {
    radix_index_pass_be(A, n, stride, shift[0], offsets[0], idx);
  }
  else
  {
    SortData<Value> *src, *dst;
    unsigned d;
    if ((digits & 1) == 0)
      dst = work, src = dst + n;
    else
      src = work, dst = src + n;
    radix_index_pass_b(A, n, stride, shift[0], offsets[0], src);
    for (d = 1; d != digits - 1; ++d)
    {
      SortData<Value> *t;
      radix_index_pass_m(src, src + n, shift[d], offsets[d], dst);
      t = src, src = dst, dst = t;
    }
    radix_index_pass_e(src, src + n, shift[d], offsets[d], idx);
  }
}

template<class Value>
void TupleList::merge_index_sort(const Value *A, const Index An, Index stride,
    Index *idx, SortData<Value> *work)
{
  SortData<Value> * const buf[2] = { work + An, work };
  Index n = An, base = -n, odd = 0, c = 0, b = 1;
  Index i = 0;
  for (;;)
  {
    SortData<Value> *p;
    if ((c & 1) == 0)
    {
      base += n, n += (odd & 1), c |= 1, b ^= 1;
      while (n > 3)
        odd <<= 1, odd |= (n & 1), n >>= 1, c <<= 1, b ^= 1;
    }
    else
      base -= n - (odd & 1), n <<= 1, n -= (odd & 1), odd >>= 1, c >>= 1;
    if (c == 0)
      break;
    p = buf[b] + base;
    if (n == 2)
    {
      Value v[2];
      v[0] = *A, A += stride, v[1] = *A, A += stride;
      if (v[1] < v[0])
        p[0].v = v[1], p[0].i = i + 1, p[1].v = v[0], p[1].i = i;
      else
        p[0].v = v[0], p[0].i = i, p[1].v = v[1], p[1].i = i + 1;
      i += 2;
    }
    else if (n == 3)
    {
      Value v[3];
      v[0] = *A, A += stride, v[1] = *A, A += stride, v[2] = *A, A += stride;
      if (v[1] < v[0])
      {
        if (v[2] < v[1])
          p[0].v = v[2], p[1].v = v[1], p[2].v = v[0], p[0].i = i + 2, p[1].i =
              i + 1, p[2].i = i;
        else
        {
          if (v[2] < v[0])
            p[0].v = v[1], p[1].v = v[2], p[2].v = v[0], p[0].i = i + 1, p[1].i =
                i + 2, p[2].i = i;
          else
            p[0].v = v[1], p[1].v = v[0], p[2].v = v[2], p[0].i = i + 1, p[1].i =
                i, p[2].i = i + 2;
        }
      }
      else
      {
        if (v[2] < v[0])
          p[0].v = v[2], p[1].v = v[0], p[2].v = v[1], p[0].i = i + 2, p[1].i =
              i, p[2].i = i + 1;
        else
        {
          if (v[2] < v[1])
            p[0].v = v[0], p[1].v = v[2], p[2].v = v[1], p[0].i = i, p[1].i = i
                + 2, p[2].i = i + 1;
          else
            p[0].v = v[0], p[1].v = v[1], p[2].v = v[2], p[0].i = i, p[1].i = i
                + 1, p[2].i = i + 2;
        }
      }
      i += 3;
    }
    else
    {
      const Index na = n >> 1, nb = (n + 1) >> 1;
      const SortData<Value> *ap = buf[b ^ 1] + base, *ae = ap + na;
      SortData<Value> *bp = p + na, *be = bp + nb;
      for (;;)
      {
        if (bp->v < ap->v)
        {
          *p++ = *bp++;
          if (bp != be)
            continue;
          do
            *p++ = *ap++;
          while (ap != ae);
          break;
        }
        else
        {
          *p++ = *ap++;
          if (ap == ae)
            break;
        }
      }
    }
  }
  {
    const SortData<Value> *p = buf[0], *pe = p + An;
    do
      *idx++ = (p++)->i;
    while (p != pe);
  }
}

template<class Value>
void TupleList::index_sort(const Value *A, Index n, Index stride, Index *idx,
    SortData<Value> *work)
{
  if (n < DIGIT_VALUES)
  {
    if (n == 0)
      return;
    if (n == 1)
      *idx = 0;
    else
      merge_index_sort(A, n, stride, idx, work);
  }
  else
    radix_index_sort(A, n, stride, idx, work);
}

#undef DIGIT_BITS
#undef DIGIT_VALUES
#undef DIGIT_MASK
#undef CEILDIV
#undef DIGITS
#undef VALUE_BITS
#undef COUNT_SIZE
#undef sort_data_long

} //namespace

