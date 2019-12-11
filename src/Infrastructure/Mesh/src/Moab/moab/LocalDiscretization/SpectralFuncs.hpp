#ifndef SPECTRALFUNCS_HPP
#define SPECTRALFUNCS_HPP

#include "float.h"

//======================================================
// from types.h
//======================================================

/* integer type to use for everything */
#if   defined(USE_LONG)
#  define INTEGER long
#elif defined(USE_LONG_LONG)
#  define INTEGER long long
#elif defined(USE_SHORT)
#  define INTEGER short
#else
#  define INTEGER int
#endif

/* when defined, use the given type for global indices instead of INTEGER */
#if   defined(USE_GLOBAL_LONG_LONG)
#  define GLOBAL_INT long long
#elif defined(USE_GLOBAL_LONG)
#  define GLOBAL_INT long
#else
#  define GLOBAL_INT long
#endif

/* floating point type to use for everything */
#if   defined(USE_FLOAT)
   typedef float real;
#  define floorr floorf
#  define ceilr  ceilf
#  define sqrtr  sqrtf
#  define fabsr  fabsf
#  define cosr   cosf
#  define sinr   sinf
#  define EPS   (128*FLT_EPSILON)
#  define PI 3.1415926535897932384626433832795028841971693993751058209749445923F
#elif defined(USE_LONG_DOUBLE)
   typedef long double real;
#  define floorr floorl
#  define ceilr  ceill
#  define sqrtr  sqrtl
#  define fabsr  fabsl
#  define cosr   cosl
#  define sinr   sinl
#  define EPS   (128*LDBL_EPSILON)
#  define PI 3.1415926535897932384626433832795028841971693993751058209749445923L
#else
   typedef double real;
#  define floorr floor
#  define ceilr  ceil
#  define sqrtr  sqrt
#  define fabsr  fabs
#  define cosr   cos
#  define sinr   sin
#  define EPS   (128*DBL_EPSILON)
#  define PI 3.1415926535897932384626433832795028841971693993751058209749445923
#endif

/* apparently uint and ulong can be defined already in standard headers */
#define uint uint_
#define ulong ulong_
#define sint sint_
#define slong slong_

typedef   signed INTEGER sint;
typedef unsigned INTEGER uint;
#undef INTEGER

#ifdef GLOBAL_INT
  typedef   signed GLOBAL_INT slong;
  typedef unsigned GLOBAL_INT ulong;
#else
  typedef sint slong;
  typedef uint ulong;
#endif

//======================================================
// from poly.h
//======================================================

/* 
  For brevity's sake, some names have been shortened
  Quadrature rules
    Gauss   -> Gauss-Legendre quadrature (open)
    Lobatto -> Gauss-Lobatto-Legendre quadrature (closed at both ends)
  Polynomial bases
    Legendre -> Legendre basis
    Gauss    -> Lagrangian basis using Gauss   quadrature nodes
    Lobatto  -> Lagrangian basis using Lobatto quadrature nodes
*/

/*--------------------------------------------------------------------------
   Legendre Polynomial Matrix Computation
   (compute P_i(x_j) for i = 0, ..., n and a given set of x)
  --------------------------------------------------------------------------*/

/* precondition: n >= 1
   inner index is x index (0 ... m-1);
   outer index is Legendre polynomial number (0 ... n)
 */
void legendre_matrix(const real *x, int m, real *P, int n);

/* precondition: n >= 1
   inner index is Legendre polynomial number (0 ... n)
   outer index is x index (0 ... m-1);
 */
void legendre_matrix_t(const real *x, int m, real *P, int n);

/* precondition: n >= 1
   compute P_i(x) with i = 0 ... n
 */
void legendre_row(real x, real *P, int n);


/*--------------------------------------------------------------------------
   Quadrature Nodes and Weights Calculation
   
   call the _nodes function before calling the _weights function
  --------------------------------------------------------------------------*/

void gauss_nodes(real *z, int n);   /* n nodes (order = 2n-1) */
void lobatto_nodes(real *z, int n); /* n nodes (order = 2n-3) */

void gauss_weights(const real *z, real *w, int n);
void lobatto_weights(const real *z, real *w, int n);

/*--------------------------------------------------------------------------
   Lagrangian to Legendre Change-of-basis Matrix
  --------------------------------------------------------------------------*/

/* precondition: n >= 2
   given the Gauss quadrature rule (z,w,n), compute the square matrix J
   for transforming from the Gauss basis to the Legendre basis:
   
      u_legendre(i) = sum_j J(i,j) u_gauss(j)

   computes J   = .5 (2i+1) w  P (z )
             ij              j  i  j
             
   in column major format (inner index is i, the Legendre index)
 */
void gauss_to_legendre(const real *z, const real *w, int n, real *J);

/* precondition: n >= 2
   same as above, but
   in row major format (inner index is j, the Gauss index)
 */
void gauss_to_legendre_t(const real *z, const real *w, int n, real *J);

/* precondition: n >= 3
   given the Lobatto quadrature rule (z,w,n), compute the square matrix J
   for transforming from the Lobatto basis to the Legendre basis:
   
      u_legendre(i) = sum_j J(i,j) u_lobatto(j)

   in column major format (inner index is i, the Legendre index)
 */
void lobatto_to_legendre(const real *z, const real *w, int n, real *J);

/*--------------------------------------------------------------------------
   Lagrangian basis function evaluation
  --------------------------------------------------------------------------*/

/* given the Lagrangian nodes (z,n) and evaluation points (x,m)
   evaluate all Lagrangian basis functions at all points x
   
   inner index of output J is the basis function index (row-major format)
   provide work array with space for 4*n doubles
 */
void lagrange_weights(const real *z, unsigned n,
                      const real *x, unsigned m,
                      real *J, real *work);

/* given the Lagrangian nodes (z,n) and evaluation points (x,m)
   evaluate all Lagrangian basis functions and their derivatives
   
   inner index of outputs J,D is the basis function index (row-major format)
   provide work array with space for 6*n doubles
 */
void lagrange_weights_deriv(const real *z, unsigned n,
                            const real *x, unsigned m,
                            real *J, real *D, real *work);

/*--------------------------------------------------------------------------
   Speedy Lagrangian Interpolation
   
   Usage:
   
     lagrange_data p;
     lagrange_setup(&p,z,n);    *  setup for nodes z[0 ... n-1] *
     
     the weights
       p->J [0 ... n-1]     interpolation weights
       p->D [0 ... n-1]     1st derivative weights
       p->D2[0 ... n-1]     2nd derivative weights
     are computed for a given x with:
       lagrange_0(p,x);  *  compute p->J *
       lagrange_1(p,x);  *  compute p->J, p->D *
       lagrange_2(p,x);  *  compute p->J, p->D, p->D2 *
       lagrange_2u(p);   *  compute p->D2 after call of lagrange_1(p,x); *
     These functions use the z array supplied to setup
       (that pointer should not be freed between calls)
     Weights for x=z[0] and x=z[n-1] are computed during setup; access as:
       p->J_z0, etc. and p->J_zn, etc.

     lagrange_free(&p);  *  deallocate memory allocated by setup *
  --------------------------------------------------------------------------*/

typedef struct {
  unsigned n;                /* number of Lagrange nodes            */
  const real *z;             /* Lagrange nodes (user-supplied)      */
  real *J, *D, *D2;          /* weights for 0th,1st,2nd derivatives */
  real *J_z0, *D_z0, *D2_z0; /* ditto at z[0]   (computed at setup) */
  real *J_zn, *D_zn, *D2_zn; /* ditto at z[n-1] (computed at setup) */
  real *w, *d, *u0, *v0, *u1, *v1, *u2, *v2; /* work data           */
} lagrange_data;

void lagrange_setup(lagrange_data *p, const real *z, unsigned n);
void lagrange_free(lagrange_data *p);

void lagrange_0(lagrange_data *p, real x) ;
void lagrange_1(lagrange_data *p, real x) ;
void lagrange_2(lagrange_data *p, real x) ;
void lagrange_2u(lagrange_data *p) ;

//======================================================
// from tensor.h
//======================================================

/*--------------------------------------------------------------------------
   1-,2-,3-d Tensor Application
   
   the 3d case:
   tensor_f3(R,mr,nr, S,ms,ns, T,mt,nt, u,v, work1,work2)
     gives v = [ R (x) S (x) T ] u
     where R is mr x nr, S is ms x ns, T is mt x nt,
       each in row- or column-major format according to f := r | c
     u is nr x ns x nt in column-major format (inner index is r)
     v is mr x ms x mt in column-major format (inner index is r)
  --------------------------------------------------------------------------*/

void tensor_c1(const real *R, unsigned mr, unsigned nr, 
               const real *u, real *v);
void tensor_r1(const real *R, unsigned mr, unsigned nr, 
               const real *u, real *v);

/* work holds mr*ns reals */
void tensor_c2(const real *R, unsigned mr, unsigned nr,
               const real *S, unsigned ms, unsigned ns,
               const real *u, real *v, real *work);
void tensor_r2(const real *R, unsigned mr, unsigned nr,
               const real *S, unsigned ms, unsigned ns,
               const real *u, real *v, real *work);

/* work1 holds mr*ns*nt reals,
   work2 holds mr*ms*nt reals */
void tensor_c3(const real *R, unsigned mr, unsigned nr,
               const real *S, unsigned ms, unsigned ns,
               const real *T, unsigned mt, unsigned nt,
               const real *u, real *v, real *work1, real *work2);
void tensor_r3(const real *R, unsigned mr, unsigned nr,
               const real *S, unsigned ms, unsigned ns,
               const real *T, unsigned mt, unsigned nt,
               const real *u, real *v, real *work1, real *work2);

/*--------------------------------------------------------------------------
   1-,2-,3-d Tensor Application of Row Vectors (for Interpolation)
   
   the 3d case:
   v = tensor_i3(Jr,nr, Js,ns, Jt,nt, u, work)
   same effect as tensor_r3(Jr,1,nr, Js,1,ns, Jt,1,nt, u,&v, work1,work2):
     gives v = [ Jr (x) Js (x) Jt ] u
     where Jr, Js, Jt are row vectors (interpolation weights)
     u is nr x ns x nt in column-major format (inner index is r)
     v is a scalar
  --------------------------------------------------------------------------*/

real tensor_i1(const real *Jr, unsigned nr, const real *u);

/* work holds ns reals */
real tensor_i2(const real *Jr, unsigned nr,
               const real *Js, unsigned ns,
               const real *u, real *work);

/* work holds ns*nt + nt reals */
real tensor_i3(const real *Jr, unsigned nr,
               const real *Js, unsigned ns,
               const real *Jt, unsigned nt,
               const real *u, real *work);

/*--------------------------------------------------------------------------
   1-,2-,3-d Tensor Application of Row Vectors
             for simultaneous Interpolation and Gradient computation
   
   the 3d case:
   v = tensor_ig3(Jr,Dr,nr, Js,Ds,ns, Jt,Dt,nt, u,g, work)
     gives v   = [ Jr (x) Js (x) Jt ] u
           g_0 = [ Dr (x) Js (x) Jt ] u
           g_1 = [ Jr (x) Ds (x) Jt ] u
           g_2 = [ Jr (x) Js (x) Dt ] u
     where Jr,Dr,Js,Ds,Jt,Dt are row vectors
       (interpolation & derivative weights)
     u is nr x ns x nt in column-major format (inner index is r)
     v is a scalar, g is an array of 3 reals
  --------------------------------------------------------------------------*/

real tensor_ig1(const real *Jr, const real *Dr, unsigned nr,
                const real *u, real *g);

/* work holds 2*ns reals */
real tensor_ig2(const real *Jr, const real *Dr, unsigned nr,
                const real *Js, const real *Ds, unsigned ns,
                const real *u, real *g, real *work);

/* work holds 2*ns*nt + 3*ns reals */
real tensor_ig3(const real *Jr, const real *Dr, unsigned nr,
                const real *Js, const real *Ds, unsigned ns,
                const real *Jt, const real *Dt, unsigned nt,
                const real *u, real *g, real *work);

//======================================================
// from findpt.h
//======================================================

typedef struct {
  const real *xw[2];   /* geometry data */
  real *z[2];          /* lobatto nodes */
  lagrange_data ld[2]; /* interpolation, derivative weights & data */
  unsigned nptel;      /* nodes per element */
  struct findpt_hash_data_2 *hash;   /* geometric hashing data */
  struct findpt_listel *list, **sorted, **end;
                                        /* pre-allocated list of elements to
                                           check (found by hashing), and
                                           pre-allocated list of pointers into
                                           the first list for sorting */
  struct findpt_opt_data_2 *od; /* data for the optimization algorithm */
  real *od_work;
} findpt_data_2;

typedef struct {
  const real *xw[3];   /* geometry data */
  real *z[3];          /* lobatto nodes */
  lagrange_data ld[3]; /* interpolation, derivative weights & data */
  unsigned nptel;      /* nodes per element */
  struct findpt_hash_data_3 *hash;   /* geometric hashing data */
  struct findpt_listel *list, **sorted, **end;
                                        /* pre-allocated list of elements to
                                           check (found by hashing), and
                                           pre-allocated list of pointers into
                                           the first list for sorting */
  struct findpt_opt_data_3 *od; /* data for the optimization algorithm */
  real *od_work;
} findpt_data_3;

findpt_data_2 *findpt_setup_2(
          const real *const xw[2], const unsigned n[2], uint nel,
          uint max_hash_size, real bbox_tol);
findpt_data_3 *findpt_setup_3(
          const real *const xw[3], const unsigned n[3], uint nel,
          uint max_hash_size, real bbox_tol);

void findpt_free_2(findpt_data_2 *p);
void findpt_free_3(findpt_data_3 *p);

const real *findpt_allbnd_2(const findpt_data_2 *p);
const real *findpt_allbnd_3(const findpt_data_3 *p);

typedef int (*findpt_func)(void *, const real *, int, uint *, real *, real *);
int findpt_2(findpt_data_2 *p, const real x[2], int guess,
             uint *el, real r[2], real *dist);
int findpt_3(findpt_data_3 *p, const real x[3], int guess,
             uint *el, real r[3], real *dist);

inline void findpt_weights_2(findpt_data_2 *p, const real r[2])
{
  lagrange_0(&p->ld[0],r[0]);
  lagrange_0(&p->ld[1],r[1]);
}

inline void findpt_weights_3(findpt_data_3 *p, const real r[3])
{
  lagrange_0(&p->ld[0],r[0]);
  lagrange_0(&p->ld[1],r[1]);
  lagrange_0(&p->ld[2],r[2]);
}

inline double findpt_eval_2(findpt_data_2 *p, const real *u)
{
  return tensor_i2(p->ld[0].J,p->ld[0].n,
                   p->ld[1].J,p->ld[1].n,
                   u, p->od_work);
}

inline double findpt_eval_3(findpt_data_3 *p, const real *u)
{
  return tensor_i3(p->ld[0].J,p->ld[0].n,
                   p->ld[1].J,p->ld[1].n,
                   p->ld[2].J,p->ld[2].n,
                   u, p->od_work);
}

//======================================================
// from extrafindpt.h
//======================================================

typedef struct {
  unsigned constraints;
  unsigned dn, d1, d2;
  real *x[3], *fdn[3];
} opt_face_data_3;

typedef struct {
  unsigned constraints;
  unsigned de, d1, d2;
  real *x[3], *fd1[3], *fd2[3];
} opt_edge_data_3;

typedef struct {
  unsigned constraints;
  real x[3], jac[9];
} opt_point_data_3;

typedef struct {
  lagrange_data *ld;
  unsigned size[4];
  const real *elx[3];
  opt_face_data_3 fd;
  opt_edge_data_3 ed;
  opt_point_data_3 pd;
  real *work;
  real x[3], jac[9];
} opt_data_3;


void opt_alloc_3(opt_data_3 *p, lagrange_data *ld);
void opt_free_3(opt_data_3 *p);
double opt_findpt_3(opt_data_3 *p, const real *const elx[3],
                           const real xstar[3], real r[3], unsigned *constr);
void opt_vol_set_intp_3(opt_data_3 *p, const real r[3]);

const unsigned opt_no_constraints_2 = 3+1;
const unsigned opt_no_constraints_3 = 9+3+1;

/* for 2d spectralQuad */
/*--------------------------------------------------------------------------

   2 - D

  --------------------------------------------------------------------------*/

typedef struct {
  unsigned constraints;
  unsigned de, d1;
  real *x[2], *fd1[2];
} opt_edge_data_2;

typedef struct {
  unsigned constraints;
  real x[2], jac[4];
} opt_point_data_2;

typedef struct {
  lagrange_data *ld;
  unsigned size[3];
  const real *elx[2];
  opt_edge_data_2 ed;
  opt_point_data_2 pd;
  real *work;
  real x[2], jac[4];
} opt_data_2;
void opt_alloc_2(opt_data_2 *p, lagrange_data *ld);
void opt_free_2(opt_data_2 *p);
double opt_findpt_2(opt_data_2 *p, const real *const elx[2],
                           const real xstar[2], real r[2], unsigned *constr);

#endif

