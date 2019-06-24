#include "moab/LocalDiscretization/SpectralHex.hpp"
#include "moab/Forward.hpp"

namespace moab 
{
    
  // SpectralHex

  // filescope for static member data that is cached
int SpectralHex::_n;
real *SpectralHex::_z[3];
lagrange_data SpectralHex::_ld[3];
opt_data_3 SpectralHex::_data;
real * SpectralHex::_odwork;

bool SpectralHex::_init = false;

    void SpectralHex::initFcn(const double *verts, const int nverts, double *&work)
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
    //triplicates! n is the same in all directions !!!
  for(int d=0; d<3; d++){
    _z[d] = tmalloc(double, _n);
    lobatto_nodes(_z[d], _n);
    lagrange_setup(&_ld[d], _z[d], _n);
  }
  opt_alloc_3(&_data, _ld);

  unsigned int nf = _n*_n, ne = _n, nw = 2*_n*_n + 3*_n;
  _odwork = tmalloc(double, 6*nf + 9*ne + nw);
}
void SpectralHex::freedata()
{
  for(int d=0; d<3; d++){
    free(_z[d]);
    lagrange_free(&_ld[d]);
  }
  opt_free_3(&_data);
  free(_odwork);
}

void SpectralHex::set_gl_points( double * x, double * y, double *z)
{
  _xyz[0] = x;
  _xyz[1] = y;
  _xyz[2] = z;
}
CartVect SpectralHex::evaluate( const CartVect& params ) const
{
    //piece that we shouldn't want to cache
  int d=0;
  for(d=0; d<3; d++){
    lagrange_0(&_ld[d], params[d]);
  }
  CartVect result;
  for (d=0; d<3; d++)
  {
    result[d] = tensor_i3(_ld[0].J,_ld[0].n,
                          _ld[1].J,_ld[1].n,
                          _ld[2].J,_ld[2].n,
                          _xyz[d],   // this is the "field"
                          _odwork);
  }
  return result;
}
  // replicate the functionality of hex_findpt
    bool SpectralHex::evaluate_reverse(CartVect const & xyz, CartVect &params, double iter_tol, const double inside_tol,
                                       const CartVect &init) const
{
  params = init;
      
    //find nearest point
  double x_star[3];
  xyz.get(x_star);

  double r[3] = {0, 0, 0 }; // initial guess for parametric coords
  unsigned c = opt_no_constraints_3;
  double dist = opt_findpt_3(&_data, (const double **)_xyz, x_star, r, &c);
    // if it did not converge, get out with throw...
  if (dist > 0.9e+30)
    return false;
    //c tells us if we landed inside the element or exactly on a face, edge, or node
    // also, dist shows the distance to the computed point.
    //copy parametric coords back
  params = r;

  return is_inside(params, inside_tol);
}
Matrix3  SpectralHex::jacobian(const CartVect& params) const
{
  real x_i[3];
  params.get(x_i);
    // set the positions of GL nodes, before evaluations
  _data.elx[0]=_xyz[0];
  _data.elx[1]=_xyz[1];
  _data.elx[2]=_xyz[2];
  opt_vol_set_intp_3(&_data,x_i);
  Matrix3 J(0.);
    // it is organized differently
  J(0,0) = _data.jac[0]; // dx/dr
  J(0,1) = _data.jac[1]; // dx/ds
  J(0,2) = _data.jac[2]; // dx/dt
  J(1,0) = _data.jac[3]; // dy/dr
  J(1,1) = _data.jac[4]; // dy/ds
  J(1,2) = _data.jac[5]; // dy/dt
  J(2,0) = _data.jac[6]; // dz/dr
  J(2,1) = _data.jac[7]; // dz/ds
  J(2,2) = _data.jac[8]; // dz/dt
  return J;
}
void SpectralHex::evaluate_vector(const CartVect& params, const double *field, int num_tuples, double *eval) const
{
    //piece that we shouldn't want to cache
  int d;
  for(d=0; d<3; d++){
    lagrange_0(&_ld[d], params[d]);
  }

  *eval = tensor_i3(_ld[0].J,_ld[0].n,
                    _ld[1].J,_ld[1].n,
                    _ld[2].J,_ld[2].n,
                    field,
                    _odwork);
}
void SpectralHex::integrate_vector(const double *field_values, int num_tuples, double *integral) const
{
    // set the position of GL points
    // set the positions of GL nodes, before evaluations
  _data.elx[0]=_xyz[0];
  _data.elx[1]=_xyz[1];
  _data.elx[2]=_xyz[2];
  real params[3];
    //triple loop; the most inner loop is in r direction, then s, then t
  for (int l = 0; l < num_tuples; l++) integral[l] = 0.0;
    //double volume = 0;
  int index=0; // used fr the inner loop
  for (int k=0; k<_n; k++ )
  {
    params[2]=_ld[2].z[k];
      //double wk= _ld[2].w[k];
    for (int j=0; j<_n; j++)
    {
      params[1]=_ld[1].z[j];
        //double wj= _ld[1].w[j];
      for (int i=0; i<_n; i++)
      {
        params[0]=_ld[0].z[i];
          //double wi= _ld[0].w[i];
        opt_vol_set_intp_3(&_data,params);
        double wk= _ld[2].J[k];
        double wj= _ld[1].J[j];
        double wi= _ld[0].J[i];
        Matrix3 J(0.);
          // it is organized differently
        J(0,0) = _data.jac[0]; // dx/dr
        J(0,1) = _data.jac[1]; // dx/ds
        J(0,2) = _data.jac[2]; // dx/dt
        J(1,0) = _data.jac[3]; // dy/dr
        J(1,1) = _data.jac[4]; // dy/ds
        J(1,2) = _data.jac[5]; // dy/dt
        J(2,0) = _data.jac[6]; // dz/dr
        J(2,1) = _data.jac[7]; // dz/ds
        J(2,2) = _data.jac[8]; // dz/dt
        double bm = wk*wj*wi* J.determinant();
        for (int l = 0; l < num_tuples; l++)
          integral[l]+= bm*field_values[num_tuples*index+l];
          //volume +=bm;
      }
    }
  }
    //std::cout << "volume: " << volume << "\n";
}

    int SpectralHex::insideFcn(const double *params, const int ndim, const double tol) 
    {
      return EvalSet::inside(params, ndim, tol);
    }

  // SpectralHex

} // namespace moab
