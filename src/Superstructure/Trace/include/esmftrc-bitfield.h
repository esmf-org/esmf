#ifndef _ESMFTRC_BITFIELD_H
#define _ESMFTRC_BITFIELD_H

/*
 * BabelTrace
 *
 * Bitfields read/write functions.
 *
 * Copyright 2010 - Mathieu Desnoyers <mathieu.desnoyers@efficios.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <limits.h>

#ifdef __cplusplus
# define CAST_PTR(_type, _value) \
	static_cast<_type>(static_cast<void *>(_value))
#else
# define CAST_PTR(_type, _value)	((void *) (_value))
#endif

#define ESMFTRC_BYTE_ORDER LITTLE_ENDIAN

/* We can't shift a int from 32 bit, >> 32 and << 32 on int is undefined */
#define _esmftrc_bt_piecewise_rshift(_vtype, _v, _shift) \
do {									\
	unsigned long ___shift = (_shift);				\
	unsigned long sb = (___shift) / (sizeof(_v) * CHAR_BIT - 1);	\
	unsigned long final = (___shift) % (sizeof(_v) * CHAR_BIT - 1); \
									\
	for (; sb; sb--)						\
		_v >>= sizeof(_v) * CHAR_BIT - 1;			\
	_v >>= final;							\
} while (0)

/*
 * esmftrc_bt_bitfield_write - write integer to a bitfield in native endianness
 *
 * Save integer to the bitfield, which starts at the "start" bit, has "len"
 * bits.
 * The inside of a bitfield is from high bits to low bits.
 * Uses native endianness.
 * For unsigned "v", pad MSB with 0 if bitfield is larger than v.
 * For signed "v", sign-extend v if bitfield is larger than v.
 *
 * On little endian, bytes are placed from the less significant to the most
 * significant. Also, consecutive bitfields are placed from lower bits to higher
 * bits.
 *
 * On big endian, bytes are places from most significant to less significant.
 * Also, consecutive bitfields are placed from higher to lower bits.
 */

#define _esmftrc_bt_bitfield_write_le(_ptr, type, _start, _length, _vtype, _v) \
do {									\
	_vtype __v = (_v);					\
	type *__ptr = CAST_PTR(type *, _ptr);				\
	unsigned long __start = (_start), __length = (_length);		\
	type mask, cmask;						\
	unsigned long ts = sizeof(type) * CHAR_BIT; /* type size */	\
	unsigned long start_unit, end_unit, this_unit;			\
	unsigned long end, cshift; /* cshift is "complement shift" */	\
									\
	if (!__length)							\
		break;							\
									\
	end = __start + __length;					\
	start_unit = __start / ts;					\
	end_unit = (end + (ts - 1)) / ts;				\
									\
	/* Trim v high bits */						\
	if (__length < sizeof(__v) * CHAR_BIT)				\
		__v &= ~((~(_vtype) 0) << __length);		\
									\
	/* We can now append v with a simple "or", shift it piece-wise */ \
	this_unit = start_unit;						\
	if (start_unit == end_unit - 1) {				\
		mask = ~((~(type) 0) << (__start % ts));		\
		if (end % ts)						\
			mask |= (~(type) 0) << (end % ts);		\
		cmask = (type) __v << (__start % ts);			\
		cmask &= ~mask;						\
		__ptr[this_unit] &= mask;				\
		__ptr[this_unit] |= cmask;				\
		break;							\
	}								\
	if (__start % ts) {						\
		cshift = __start % ts;					\
		mask = ~((~(type) 0) << cshift);			\
		cmask = (type) __v << cshift;				\
		cmask &= ~mask;						\
		__ptr[this_unit] &= mask;				\
		__ptr[this_unit] |= cmask;				\
		_esmftrc_bt_piecewise_rshift(_vtype, __v, ts - cshift); \
		__start += ts - cshift;					\
		this_unit++;						\
	}								\
	for (; this_unit < end_unit - 1; this_unit++) {			\
		__ptr[this_unit] = (type) __v;				\
		_esmftrc_bt_piecewise_rshift(_vtype, __v, ts); 		\
		__start += ts;						\
	}								\
	if (end % ts) {							\
		mask = (~(type) 0) << (end % ts);			\
		cmask = (type) __v;					\
		cmask &= ~mask;						\
		__ptr[this_unit] &= mask;				\
		__ptr[this_unit] |= cmask;				\
	} else								\
		__ptr[this_unit] = (type) __v;				\
} while (0)

#define _esmftrc_bt_bitfield_write_be(_ptr, type, _start, _length, _vtype, _v) \
do {									\
	_vtype __v = (_v);					\
	type *__ptr = CAST_PTR(type *, _ptr);				\
	unsigned long __start = (_start), __length = (_length);		\
	type mask, cmask;						\
	unsigned long ts = sizeof(type) * CHAR_BIT; /* type size */	\
	unsigned long start_unit, end_unit, this_unit;			\
	unsigned long end, cshift; /* cshift is "complement shift" */	\
									\
	if (!__length)							\
		break;							\
									\
	end = __start + __length;					\
	start_unit = __start / ts;					\
	end_unit = (end + (ts - 1)) / ts;				\
									\
	/* Trim v high bits */						\
	if (__length < sizeof(__v) * CHAR_BIT)				\
		__v &= ~((~(_vtype) 0) << __length);			\
									\
	/* We can now append v with a simple "or", shift it piece-wise */ \
	this_unit = end_unit - 1;					\
	if (start_unit == end_unit - 1) {				\
		mask = ~((~(type) 0) << ((ts - (end % ts)) % ts));	\
		if (__start % ts)					\
			mask |= (~((type) 0)) << (ts - (__start % ts));	\
		cmask = (type) __v << ((ts - (end % ts)) % ts);		\
		cmask &= ~mask;						\
		__ptr[this_unit] &= mask;				\
		__ptr[this_unit] |= cmask;				\
		break;							\
	}								\
	if (end % ts) {							\
		cshift = end % ts;					\
		mask = ~((~(type) 0) << (ts - cshift));			\
		cmask = (type) __v << (ts - cshift);			\
		cmask &= ~mask;						\
		__ptr[this_unit] &= mask;				\
		__ptr[this_unit] |= cmask;				\
		_esmftrc_bt_piecewise_rshift(__v, cshift); \
		end -= cshift;						\
		this_unit--;						\
	}								\
	for (; (long) this_unit >= (long) start_unit + 1; this_unit--) { \
		__ptr[this_unit] = (type) __v;				\
		_esmftrc_bt_piecewise_rshift(__v, ts); \
		end -= ts;						\
	}								\
	if (__start % ts) {						\
		mask = (~(type) 0) << (ts - (__start % ts));		\
		cmask = (type) __v;					\
		cmask &= ~mask;						\
		__ptr[this_unit] &= mask;				\
		__ptr[this_unit] |= cmask;				\
	} else								\
		__ptr[this_unit] = (type) __v;				\
} while (0)

/*
 * esmftrc_bt_bitfield_write_le - write integer to a bitfield in little endian
 * esmftrc_bt_bitfield_write_be - write integer to a bitfield in big endian
 */

#if (ESMFTRC_BYTE_ORDER == LITTLE_ENDIAN)

#define esmftrc_bt_bitfield_write_le(ptr, type, _start, _length, _vtype, _v) \
	_esmftrc_bt_bitfield_write_le(ptr, type, _start, _length, _vtype, _v)

#define esmftrc_bt_bitfield_write_be(ptr, type, _start, _length, _vtype, _v) \
	_esmftrc_bt_bitfield_write_be(ptr, unsigned char, _start, _length, _vtype, _v)

#elif (ESMFTRC_BYTE_ORDER == BIG_ENDIAN)

#define esmftrc_bt_bitfield_write_le(ptr, type, _start, _length, _vtype, _v) \
	_esmftrc_bt_bitfield_write_le(ptr, unsigned char, _start, _length, _vtype, _v)

#define esmftrc_bt_bitfield_write_be(ptr, type, _start, _length, _vtype, _v) \
	_esmftrc_bt_bitfield_write_be(ptr, type, _start, _length, _vtype, _v)

#else /* (ESMFTRC_BYTE_ORDER == PDP_ENDIAN) */

#error "Byte order not supported"

#endif

#endif /* _ESMFTRC_BITFIELD_H */
