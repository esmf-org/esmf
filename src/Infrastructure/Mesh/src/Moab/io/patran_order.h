#ifndef PATRAN_ORDER_H
#define PATRAN_ORDER_H

#ifdef __cplusplus
extern "C" {
namespace moab {
#endif

/* Define maps that, when indexed by the PATRAN connectivity index return
   the corresponding index in the MOAB connectivity. */

static const int patran_tet8_order[] = { 0, 1, 2, 3, 7, 5, 6, 4 };
static const int patran_tet9_order[] = { 0, 1, 2, 3, 8, 7, 5, 6, 4 };
static const int patran_tet14_order[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 11, 12, 10 };
static const int patran_tet15_order[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 14, 13, 11, 12, 10 };

static const int patran_hex14_order[] = { 0, 1, 2, 3, 4, 5, 6, 7, 12, 13, 11, 9, 8, 10 };
static const int patran_hex15_order[] = { 0, 1, 2, 3, 4, 5, 6, 7, 14, 12, 13, 11, 9, 8, 10 };
static const int patran_hex26_order[] = { 0, 1, 2, 3, 4, 5, 6, 7,
                                   8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                                   24, 25, 23, 21, 20, 22 };
static const int patran_hex27_order[] = { 0, 1, 2, 3, 4, 5, 6, 7,
                                   8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                                   26, 24, 25, 23, 21, 20, 22 };

static const int patran_pri11_order[] = { 0, 1, 2, 3, 4, 5, 9, 10, 7, 8, 6 };
static const int patran_pri12_order[] = { 0, 1, 2, 3, 4, 5, 11, 9, 10, 7, 8, 6 };
static const int patran_pri20_order[] = { 0, 1, 2, 3, 4, 5,
                                   6, 7, 8, 9, 10, 11, 12, 13, 14,
                                   18, 19, 16, 17, 15 };
static const int patran_pri21_order[] = { 0, 1, 2, 3, 4, 5,
                                   6, 7, 8, 9, 10, 11, 12, 13, 14,
                                   20, 18, 19, 16, 17, 15 };

/* Define list of maps for an element type, indexed by the number
   of nodes in the element. Entries are NULL where MBCN connectivity
   is the same as PATRAN (or the number of nodes is invalid.) */

static const int* const patran_null_order[] = { 0, 0, 0, 0, 0, 0, 0, 0, 
                                         0, 0, 0, 0, 0, 0, 0, 0, 
                                         0, 0, 0, 0, 0, 0, 0, 0, 
                                         0, 0, 0, 0, 0, 0, 0, 0,
                                         0, 0, 0, 0, 0, 0, 0, 0, 
                                         0, 0, 0, 0, 0, 0, 0, 0, 
                                         0, 0, 0, 0, 0, 0, 0, 0, 
                                         0, 0, 0, 0, 0, 0, 0, 0, 
                                         0 };
static const int* const patran_tet_order[] = { 0, 0, 0, 0, 0, 0, 0, 0, 
                                        patran_tet8_order,
                                        patran_tet9_order,
                                        0, 0, 0, 0,  
                                        patran_tet14_order,
                                        patran_tet15_order, 
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0 };
static const int* const patran_hex_order[] = { 0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 0, 0, 0, 
                                        patran_hex14_order,
                                        patran_hex15_order,
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 
                                        patran_hex26_order,
                                        patran_hex27_order,
                                        0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0 };
static const int* const patran_pri_order[] = { 0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 
                                        patran_pri11_order,
                                        patran_pri12_order,
                                        0, 0, 0, 0, 0, 0, 0,
                                        patran_pri20_order,
                                        patran_pri21_order,
                                        0, 0, 
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 0, 0, 0, 0, 0, 
                                        0 };

/* Define matrix of maps indexed by element topology (EntityType) and
   number of nodes. Entries are NULL where MBCN connectivity
   is the same as PATRAN (or the number of nodes is invalid.) */

static const int *const *const patran_elem_order_map[] = { 
  patran_null_order, /*  MBVERTEX */
  patran_null_order, /*  MBEDGE */
  patran_null_order, /*  MBTRI */
  patran_null_order, /*  MBQUAD */
  patran_null_order, /*  MBPOLYGON */
  patran_tet_order,  /*  MBTET */
  patran_null_order, /*  MBPYRAMID */
  patran_pri_order,  /*  MBPRISM */
  patran_null_order, /*  MBKNIFE */
  patran_hex_order,  /*  MBHEX */
  patran_null_order, /*  MBPOLYHEDRON */
  patran_null_order
};

#ifdef __cplusplus
} /*  extern "C" */
} /* namespace moab */
#endif

#endif
