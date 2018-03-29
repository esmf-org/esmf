#include "moab/LinearTet.hpp"
#include "moab/Forward.hpp"
#include <algorithm>
#include <math.h>
#include <limits>

namespace moab 
{
    
    const double LinearTet::corner[4][3] = { {0,0,0},
                                             {1,0,0},
                                             {0,1,0},
                                             {0,0,1}};

    ErrorCode LinearTet::initFcn(const double *verts, const int /*nverts*/, double *&work) {
        // allocate work array as: 
        // work[0..8] = T
        // work[9..17] = Tinv
        // work[18] = detT
        // work[19] = detTinv
      assert(!work && verts);
      work = new double[20];
      Matrix3 *T = reinterpret_cast<Matrix3*>(work),
          *Tinv = reinterpret_cast<Matrix3*>(work+9);
      double *detT = work+18, *detTinv = work+19;
      
      *T = Matrix3(verts[1*3+0]-verts[0*3+0],verts[2*3+0]-verts[0*3+0],verts[3*3+0]-verts[0*3+0],
                   verts[1*3+1]-verts[0*3+1],verts[2*3+1]-verts[0*3+1],verts[3*3+1]-verts[0*3+1],
                   verts[1*3+2]-verts[0*3+2],verts[2*3+2]-verts[0*3+2],verts[3*3+2]-verts[0*3+2]);
      *Tinv = T->inverse();
      *detT = T->determinant();
      *detTinv = (*detT < 1e-12 ? std::numeric_limits<double>::max() : 1.0 / *detT);

      return MB_SUCCESS;
    }

    ErrorCode LinearTet::evalFcn(const double *params, const double *field, const int /*ndim*/, const int num_tuples, 
                                 double */*work*/, double *result) {
      assert(params && field && num_tuples > 0);
      std::vector<double> f0(num_tuples);
      std::copy(field, field+num_tuples, f0.begin());
      std::copy(field, field+num_tuples, result);

      for (unsigned i = 1; i < 4; ++i) {
        double p = 0.5*(params[i-1] + 1); // transform from -1 <= p <= 1 to 0 <= p <= 1
        for (int j = 0; j < num_tuples; j++)
          result[j] += (field[i*num_tuples+j]-f0[j])*p;
      }

      return MB_SUCCESS;
    }

    ErrorCode LinearTet::integrateFcn(const double *field, const double */*verts*/, const int nverts, const int /*ndim*/, const int num_tuples,
                                      double *work, double *result) 
    {
      assert(field && num_tuples > 0);
      std::fill(result, result+num_tuples, 0.0);
      for(int i = 0; i < nverts; ++i) {
        for (int j = 0; j < num_tuples; j++)
          result[j] += field[i*num_tuples+j];
      }
      double tmp = work[18]/24.0;
      for (int i = 0; i < num_tuples; i++) result[i] *= tmp;

      return MB_SUCCESS;
    }

    ErrorCode LinearTet::jacobianFcn(const double *, const double *, const int, const int , 
                                     double *work, double *result) 
    {
        // jacobian is cached in work array
      assert(work);
      std::copy(work, work+9, result);
      return MB_SUCCESS;
    }
    
    ErrorCode LinearTet::reverseEvalFcn(EvalFcn eval, JacobianFcn jacob, InsideFcn ins, 
                                        const double *posn, const double *verts, const int nverts, const int ndim,
                                        const double iter_tol, const double inside_tol, double *work, 
                                        double *params, int *is_inside) 
    {
      assert(posn && verts);
      return evaluate_reverse(eval, jacob, ins, posn, verts, nverts, ndim, iter_tol, inside_tol, 
                              work, params, is_inside);
    } 

    int LinearTet::insideFcn(const double *params, const int , const double tol) 
    {
      return (params[0] >= -1.0-tol && params[1] >= -1.0-tol && params[2] >= -1.0-tol && 
              params[0] + params[1] + params[2] <= 1.0+tol);
      
    }
    
    ErrorCode LinearTet::evaluate_reverse(EvalFcn eval, JacobianFcn jacob, InsideFcn inside_f,
                                          const double *posn, const double *verts, const int nverts, 
                                          const int ndim, const double iter_tol, const double inside_tol,
                                          double *work, double *params, int *inside) {
        // TODO: should differentiate between epsilons used for
        // Newton Raphson iteration, and epsilons used for curved boundary geometry errors
        // right now, fix the tolerance used for NR
      const double error_tol_sqr = iter_tol*iter_tol;
      CartVect *cvparams = reinterpret_cast<CartVect*>(params);
      const CartVect *cvposn = reinterpret_cast<const CartVect*>(posn);

        // find best initial guess to improve convergence
      CartVect tmp_params[] = {CartVect(-1,-1,-1), CartVect(1,-1,-1), CartVect(-1,1,-1), CartVect(-1,-1,1)};
      //TODO: RLO replaced this code with same line from 5.0.0 release because it seems like a bug, and ESMF will be
      // updating to newer version of MOAB soon anyway
      double resl = std::numeric_limits<double>::max();
      CartVect new_pos, tmp_pos;
      ErrorCode rval;
      for (unsigned int i = 0; i < 4; i++) {
        rval = (*eval)(tmp_params[i].array(), verts, ndim, ndim, work, tmp_pos.array());
        if (MB_SUCCESS != rval) return rval;
        double tmp_resl = (tmp_pos-*cvposn).length_squared();
        if (tmp_resl < resl) {
          *cvparams = tmp_params[i];
          new_pos = tmp_pos;
          resl = tmp_resl;
        }        
      }

        // residual is diff between old and new pos; need to minimize that
      CartVect res = new_pos - *cvposn;
      Matrix3 J;
      rval = (*jacob)(cvparams->array(), verts, nverts, ndim, work, J[0]);
      double det = J.determinant();
      assert(det > std::numeric_limits<double>::epsilon());
      Matrix3 Ji = J.inverse(1.0/det);

      int iters=0;
        // while |res| larger than tol
      int dum, *tmp_inside = (inside ? inside : &dum);
      while (res % res > error_tol_sqr) {
        if(++iters>25) {
            // if we haven't converged but we're outside, that's defined as success
          *tmp_inside = (*inside_f)(params, ndim, inside_tol);
          if (!(*tmp_inside)) return MB_SUCCESS;
          else return MB_INDEX_OUT_OF_RANGE;
        }
        
          // new params tries to eliminate residual
        *cvparams -= Ji * res;

          // get the new forward-evaluated position, and its difference from the target pt
        rval = (*eval)(params, verts, ndim, ndim, work, new_pos.array());
        if (MB_SUCCESS != rval) return rval;
        res = new_pos - *cvposn;
      }

      if (inside)
        *inside = (*inside_f)(params, ndim, inside_tol);

      return MB_SUCCESS;
    }// Map::evaluate_reverse()

    ErrorCode LinearTet::normalFcn(const int ientDim, const int facet, const int nverts, const double *verts,  double normal[3])
    {
      //assert(facet < 4 && ientDim == 2 && nverts == 4);
      if (nverts != 4)
        MB_SET_ERR(MB_FAILURE, "Incorrect vertex count for passed tet :: expected value = 4 ");
      if (ientDim != 2)
        MB_SET_ERR(MB_FAILURE, "Requesting normal for unsupported dimension :: expected value = 2 ");
      if (facet >4 || facet < 0)
        MB_SET_ERR(MB_FAILURE, "Incorrect local face id :: expected value = one of 0-3");

      int id0 = CN::mConnectivityMap[MBTET][ientDim-1].conn[facet][0];
      int id1 = CN::mConnectivityMap[MBTET][ientDim-1].conn[facet][1];
      int id2 = CN::mConnectivityMap[MBTET][ientDim-1].conn[facet][2];

      double x0[3], x1[3];

      for (int i=0; i<3; i++)
        {
          x0[i] = verts[3*id1+i] - verts[3*id0+i];
          x1[i] = verts[3*id2+i] - verts[3*id0+i];
        }

      double a = x0[1]*x1[2] - x1[1]*x0[2];
      double b = x1[0]*x0[2] - x0[0]*x1[2];
      double c = x0[0]*x1[1] - x1[0]*x0[1];
      double nrm = sqrt(a*a+b*b+c*c);

      if (nrm > std::numeric_limits<double>::epsilon()) {
          normal[0] = a/nrm;
          normal[1] = b/nrm;
          normal[2] = c/nrm;
        }
      return MB_SUCCESS;
    }


} // namespace moab
