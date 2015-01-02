#ifndef EXODUS_ORDER_HPP
#define EXODUS_ORDER_HPP

#include "patran_order.h"

#ifdef __cplusplus
extern "C" {
namespace moab {
#endif

/* Cubit writes ExodusII files with tet mid-face nodes in this order */
static const int exodus_tet8_order[] = { 0, 1, 2, 3, 4, 5, 7, 6 };
static const int exodus_tet9_order[] = { 0, 1, 2, 3, 8, 4, 5, 7, 6 };
static const int exodus_tet14_order[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 12 };
static const int exodus_tet15_order[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 14, 10, 11, 13, 12 };

static const int* exodus_tet_order[] = { 0, 0, 0, 0, 0, 0, 0, 0,
                                  exodus_tet8_order,
                                  exodus_tet9_order,
                                  0, 0, 0, 0,
                                  exodus_tet14_order,
                                  exodus_tet15_order,
                                  0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0 };

static const int* const* exodus_hex_order = patran_hex_order;

static const int *const *const exodus_elem_order_map[] = { 
  patran_null_order, /*  MBVERTEX */
  patran_null_order, /*  MBEDGE */
  patran_null_order, /*  MBTRI */
  patran_null_order, /*  MBQUAD */
  patran_null_order, /*  MBPOLYGON */
  exodus_tet_order,  /*  MBTET */
  patran_null_order, /*  MBPYRAMID */
  patran_pri_order,  /*  MBPRISM */
  patran_null_order, /*  MBKNIFE */
  exodus_hex_order,  /*  MBHEX */
  patran_null_order, /*  MBPOLYHEDRON */
  patran_null_order
};

#ifdef __cplusplus
} /*  extern "C" */
} /* namespace moab */
#endif

#endif
