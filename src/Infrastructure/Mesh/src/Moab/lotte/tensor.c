#include "moab/FindPtFuncs.h"

/*--------------------------------------------------------------------------
   Matrix-Matrix Multiplication
   
   mxm_ab (A,na,B,nb,C,nc) :
      gives C = A B where A is na x nb, B is nb x nc, C is na x nc
      a := r | c   to indicate A is in row- or column- major format
      b := r | c   to indicate B is in row- or column- major format
      C is always column-major
  --------------------------------------------------------------------------*/
  
static void mxm_cc(const realType *A, unsigned na,
                   const realType *B, unsigned nb,
                         realType *C, unsigned nc)
{
  unsigned i,j,k;
  realType *Ccol = C;
  const realType *Bcol = B;
  for(j=0;j<nc;++j,Ccol+=na,Bcol+=nb) {
    const realType *Acol = A;
    for(i=0;i<na;++i) Ccol[i]=0;
    for(k=0;k<nb;++k,Acol+=na)
      for(i=0;i<na;++i)
        Ccol[i] += Acol[i] * Bcol[k];
  }
}

static void mxm_rc(const realType *A, unsigned na,
                   const realType *B, unsigned nb,
                         realType *C, unsigned nc)
{
  unsigned i,j,k;
  realType *Ccol = C;
  const realType *Bcol = B;
  for(j=0;j<nc;++j,Ccol+=na,Bcol+=nb) {
    const realType *Arow = A;
    for(i=0;i<na;++i,Arow+=nb) {
      Ccol[i]=0;
      for(k=0;k<nb;++k)
        Ccol[i] += Arow[k] * Bcol[k];
    }
  }
}

static void mxm_cr(const realType *A, unsigned na,
                   const realType *B, unsigned nb,
                         realType *C, unsigned nc)
{
  unsigned i,j,k;
  const realType *Acol = A, *Brow = B;
  for(i=0;i<na*nc;++i) C[i]=0;
  for(k=0;k<nb;++k,Acol+=na,Brow+=nc) {
    realType *Ccol = C;
    for(j=0;j<nc;++j,Ccol+=na)
      for(i=0;i<na;++i)
        Ccol[i] += Acol[i] * Brow[j];
  }
}

/*
static void mxm_rr(const realType *A, unsigned na,
                   const realType *B, unsigned nb,
                         realType *C, unsigned nc)
{
  unsigned i,j,k;
  realType *Ccol = C;
  const realType *Bcol = B;
  for(j=0;j<nc;++j,Ccol+=na,++Bcol) {
    const realType *Arow = A;
    for(i=0;i<na;++i,Arow+=nb) {
      const realType *Bkj = Bcol;
      Ccol[i]=0.0;
      for(k=0;k<nb;++k,Bkj+=nc)
        Ccol[i] += Arow[k] * *Bkj;
    }
  }
}
*/

/*--------------------------------------------------------------------------
   Matrix-Vector Multiplication
   
   mxv_f (y,ny,A,x,nx) :
      gives y = A x where A is ny x nx
      f := r | c   to indicate A is in row- or column- major format
  --------------------------------------------------------------------------*/

static void mxv_c(realType *y, unsigned ny, const realType *A,
                  const realType *x, unsigned nx)
{
  realType *yp=y, *y_end = y+ny;
  const realType *x_end=x+nx;
  realType xk = *x;
  do { *yp++ = *A++ * xk; } while(yp!=y_end);
  for(++x;x!=x_end;++x) {
    xk = *x; yp = y;
    do { *yp++ += *A++ * xk; } while(yp!=y_end);
  }
}

static void mxv_r(realType *y, unsigned ny, const realType *A,
                  const realType *x, unsigned nx)
{
  realType *y_end = y+ny;
  const realType *x_end = x+nx;
  do {
    const realType *xp = x;
    realType sum = *A++ * *xp++;
    while(xp!=x_end) { sum += *A++ * *xp++; }
    *y++ = sum;
  } while(y!=y_end);
}

/*--------------------------------------------------------------------------
   Vector-Vector Multiplication
   
   inner (u,v,n) : inner product
  --------------------------------------------------------------------------*/

/* precondition: n>=1 */
static realType inner(const realType *u, const realType *v, unsigned n)
{
  const realType *u_end = u+n;
  realType sum = *u++ * *v++;
  while(u!=u_end) { sum += *u++ * *v++; }
  return sum;
}

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

void tensor_c1(const realType *R, unsigned mr, unsigned nr, 
               const realType *u, realType *v)
{
  mxv_c(v,mr,R,u,nr);
}

void tensor_r1(const realType *R, unsigned mr, unsigned nr, 
               const realType *u, realType *v)
{
  mxv_r(v,mr,R,u,nr);
}

/* W holds mr*ns reals */
void tensor_c2(const realType *R, unsigned mr, unsigned nr,
               const realType *S, unsigned ms, unsigned ns,
               const realType *u, realType *v, realType *W)
{
  mxm_cc(R,mr,u,nr,W,ns);
  mxm_cr(W,mr,S,ns,v,ms);
}

/* W holds mr*ns reals */
void tensor_r2(const realType *R, unsigned mr, unsigned nr,
               const realType *S, unsigned ms, unsigned ns,
               const realType *u, realType *v, realType *W)
{
  mxm_rc(R,mr,u,nr,W,ns);
  mxm_cc(W,mr,S,ns,v,ms);
}

/* W holds mr*ns*nt reals,
   Z holds mr*ms*nt reals */
void tensor_c3(const realType *R, unsigned mr, unsigned nr,
               const realType *S, unsigned ms, unsigned ns,
               const realType *T, unsigned mt, unsigned nt,
               const realType *u, realType *v, realType *W, realType *Z)
{
  unsigned n,mrns=mr*ns,mrms=mr*ms;
  realType *Zp = Z;
  mxm_cc(R,mr,u,nr,W,ns*nt);
  for(n=0;n<nt;++n,W+=mrns,Zp+=mrms)
    mxm_cr(W,mr,S,ns,Zp,ms);
  mxm_cr(Z,mrms,T,nt,v,mt);
}

/* W holds mr*ns*nt reals,
   Z holds mr*ms*nt reals */
void tensor_r3(const realType *R, unsigned mr, unsigned nr,
               const realType *S, unsigned ms, unsigned ns,
               const realType *T, unsigned mt, unsigned nt,
               const realType *u, realType *v, realType *W, realType *Z)
{
  unsigned n,mrns=mr*ns,mrms=mr*ms;
  realType *Zp = Z;
  mxm_rc(R,mr,u,nr,W,ns*nt);
  for(n=0;n<nt;++n,W+=mrns,Zp+=mrms)
    mxm_cc(W,mr,S,ns,Zp,ms);
  mxm_cc(Z,mrms,T,nt,v,mt);
}

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

realType tensor_i1(const realType *Jr, unsigned nr, const realType *u)
{
  return inner(Jr,u,nr);
}

/* work holds ns reals */
realType tensor_i2(const realType *Jr, unsigned nr,
               const realType *Js, unsigned ns,
               const realType *u, realType *work)
{
  mxv_r(work,ns,u,Jr,nr);
  return inner(Js,work,ns);
}

/* work holds ns*nt + nt reals */
realType tensor_i3(const realType *Jr, unsigned nr,
               const realType *Js, unsigned ns,
               const realType *Jt, unsigned nt,
               const realType *u, realType *work)
{
  realType *work2 = work+nt;
  mxv_r(work2,ns*nt,u,Jr,nr);
  mxv_r(work,nt,work2,Js,ns);
  return inner(Jt,work,nt);
}

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

realType tensor_ig1(const realType *Jr, const realType *Dr, unsigned nr,
                const realType *u, realType *g)
{
  *g = inner(Dr,u,nr);
  return inner(Jr,u,nr);
}

/* work holds 2*ns reals */
realType tensor_ig2(const realType *Jr, const realType *Dr, unsigned nr,
                const realType *Js, const realType *Ds, unsigned ns,
                const realType *u, realType *g, realType *work)
{
  realType *a = work, *ar = a+ns;
  mxv_r(a ,ns,u,Jr,nr);
  mxv_r(ar,ns,u,Dr,nr);
  g[0] = inner(Js,ar,ns);
  g[1] = inner(Ds,a ,ns);
  return inner(Js,a ,ns);
}

/* work holds 2*ns*nt + 3*ns reals */
realType tensor_ig3(const realType *Jr, const realType *Dr, unsigned nr,
                const realType *Js, const realType *Ds, unsigned ns,
                const realType *Jt, const realType *Dt, unsigned nt,
                const realType *u, realType *g, realType *work)
{
  unsigned nsnt = ns*nt;
  realType *a = work, *ar = a+nsnt, *b = ar+nsnt, *br = b+ns, *bs = br+ns;
  mxv_r(a ,nsnt,u ,Jr,nr);
  mxv_r(ar,nsnt,u ,Dr,nr);
  mxv_r(b ,nt  ,a ,Js,ns);
  mxv_r(br,nt  ,ar,Js,ns);
  mxv_r(bs,nt  ,a ,Ds,ns);
  g[0] = inner(Jt,br,nt);
  g[1] = inner(Jt,bs,nt);
  g[2] = inner(Dt,b ,nt);
  return inner(Jt,b ,nt);
}

