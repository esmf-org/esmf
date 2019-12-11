#include "moab/LocalDiscretization/SpectralQuad.hpp"
#include "moab/Forward.hpp"

namespace moab 
{
    
  // filescope for static member data that is cached
int SpectralQuad::_n;
real *SpectralQuad::_z[2];
lagrange_data SpectralQuad::_ld[2];
opt_data_2 SpectralQuad::_data;
real * SpectralQuad::_odwork;
real * SpectralQuad::_glpoints;
bool SpectralQuad::_init = false;

SpectralQuad::SpectralQuad() : Map(0)
{
}
  // the preferred constructor takes pointers to GL blocked positions
SpectralQuad::SpectralQuad(int order, double * x, double *y, double *z) : Map(0)
{
  Init(order);
  _xyz[0]=x; _xyz[1]=y; _xyz[2]=z;
}
SpectralQuad::SpectralQuad(int order) : Map(4)
{
  Init(order);
  _xyz[0]=_xyz[1]=_xyz[2]=NULL;
}
SpectralQuad::~SpectralQuad()
{
  if (_init)
    freedata();
  _init=false;
}
void SpectralQuad::Init(int order)
{
  if (_init && _n==order)
    return;
  if (_init && _n!=order)
  {
      // TODO: free data cached
    freedata();
  }
    // compute stuff that depends only on order
  _init = true;
  _n = order;
    //duplicates! n is the same in all directions !!!
  for(int d=0; d<2; d++){
    _z[d] = tmalloc(double, _n);
    lobatto_nodes(_z[d], _n);
    lagrange_setup(&_ld[d], _z[d], _n);
  }
  opt_alloc_2(&_data, _ld);

  unsigned int nf = _n*_n, ne = _n, nw = 2*_n*_n + 3*_n;
  _odwork = tmalloc(double, 6*nf + 9*ne + nw);
  _glpoints = tmalloc (double, 3*nf);
}

void SpectralQuad::freedata()
{
  for(int d=0; d<2; d++){
    free(_z[d]);
    lagrange_free(&_ld[d]);
  }
  opt_free_2(&_data);
  free(_odwork);
  free(_glpoints);
}

void SpectralQuad::set_gl_points( double * x, double * y, double *z)
{
  _xyz[0] = x;
  _xyz[1] = y;
  _xyz[2] = z;
}
CartVect SpectralQuad::evalFcn(const double *params, const double *field, const int ndim, const int num_tuples, 
                               double *work, double *result) 
{
    //piece that we shouldn't want to cache
  int d=0;
  for(d=0; d<2; d++){
    lagrange_0(&_ld[d], params[d]);
  }
  CartVect result;
  for (d=0; d<3; d++)
  {
    result[d] = tensor_i2(_ld[0].J,_ld[0].n,
                          _ld[1].J,_ld[1].n,
                          _xyz[d],
                          _odwork);
  }
  return result;
}
  // replicate the functionality of hex_findpt
bool SpectralQuad::reverseEvalFcn(const double *posn, const double *verts, const int nverts, const int ndim,
                                  const double iter_tol, const double inside_tol, double *work, 
                                  double *params, int *is_inside)
{
  params = init;

    //find nearest point
  double x_star[3];
  xyz.get(x_star);

  double r[2] = {0, 0 }; // initial guess for parametric coords
  unsigned c = opt_no_constraints_3;
  double dist = opt_findpt_2(&_data, (const double **)_xyz, x_star, r, &c);
    // if it did not converge, get out with throw...
  if (dist > 0.9e+30)
  {
    std::vector<CartVect> dummy;
    throw Map::EvaluationError(CartVect(x_star), dummy);
  }
    //c tells us if we landed inside the element or exactly on a face, edge, or node
    // also, dist shows the distance to the computed point.
    //copy parametric coords back
  params = r;

  return insideFcn(params, 2, inside_tol);
}


Matrix3  SpectralQuad::jacobian(const double *params, const double *verts, const int nverts, const int ndim, 
                                     double *work, double *result)
{
    // not implemented
  Matrix3 J(0.);
  return J;
}


void SpectralQuad::evaluate_vector(const CartVect& params, const double *field, int num_tuples, double *eval) const
{
    //piece that we shouldn't want to cache
  int d;
  for(d=0; d<2; d++){
    lagrange_0(&_ld[d], params[d]);
  }

  *eval = tensor_i2(_ld[0].J,_ld[0].n,
                    _ld[1].J,_ld[1].n,
                    field,
                    _odwork);
}
void SpectralQuad:: integrate_vector(const double *field, const double *verts, const int nverts, const int ndim,
                                      const int num_tuples, double *work, double *result)
{
    // not implemented
}

int SpectralQuad::insideFcn(const double *params, const int ndim, const double tol) 
{
  return EvalSet::inside(params, ndim, tol);
}

  // something we don't do for spectral hex; we do it here because
  //       we do not store the position of gl points in a tag yet
void SpectralQuad::compute_gl_positions()
{
    // will need to use shape functions on a simple linear quad to compute gl points
    // so we know the position of gl points in parametric space, we will just compute those
    // from the 3d vertex position (corner nodes of the quad), using simple mapping
  assert (this->vertex.size()==4);
  static double corner_params[4][2]={ { -1., -1.},
                                      {  1., -1.},
                                      {  1.,  1.},
                                      { -1.,  1.} };
    // we will use the cached lobatto nodes in parametric space _z[d] (the same in both directions)
  int indexGL=0;
  int n2= _n*_n;
  for (int i=0; i<_n; i++)
  {
    double csi=_z[0][i];
    for (int j=0; j<_n; j++)
    {
      double eta = _z[1][j]; // we could really use the same _z[0] array of lobatto nodes
      CartVect pos(0.0);
      for (int k = 0; k < 4; k++) {
        const double N_k = (1 + csi*corner_params[k][0])
            * (1 + eta*corner_params[k][1]);
        pos += N_k * vertex[k];
      }
      pos *= 0.25;// these are x, y, z of gl points; reorder them
      _glpoints[indexGL] = pos[0]; // x
      _glpoints[indexGL+n2] = pos[1]; // y
      _glpoints[indexGL+2*n2] = pos[2]; // z
      indexGL++;
    }
  }
    // now, we can set the _xyz pointers to internal memory allocated to these!
  _xyz[0] =  &(_glpoints[0]);
  _xyz[1] =  &(_glpoints[n2]);
  _xyz[2] =  &(_glpoints[2*n2]);
}
void SpectralQuad::get_gl_points( double *& x, double *& y, double *& z, int & size)
{
  x=  (double *)_xyz[0] ;
  y = (double *)_xyz[1] ;
  z = (double *)_xyz[2] ;
  size = _n*_n;
}
    
} // namespace moab
