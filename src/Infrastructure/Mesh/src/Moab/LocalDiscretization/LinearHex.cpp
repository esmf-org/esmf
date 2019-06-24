#include "moab/LocalDiscretization/LinearHex.hpp"
#include "moab/Matrix3.hpp"
#include "moab/Forward.hpp"
#include <math.h>
#include <limits>

namespace moab 
{
    
    const double LinearHex::corner[8][3] = { { -1, -1, -1 },
                                             {  1, -1, -1 },
                                             {  1,  1, -1 },
                                             { -1,  1, -1 },
                                             { -1, -1,  1 },
                                             {  1, -1,  1 },
                                             {  1,  1,  1 },
                                             { -1,  1,  1 } };

      /* For each point, its weight and location are stored as an array.
         Hence, the inner dimension is 2, the outer dimension is gauss_count.
         We use a one-point Gaussian quadrature, since it integrates linear functions exactly.
      */
    const double LinearHex::gauss[1][2] = { {  2.0,           0.0          } };

    ErrorCode LinearHex::jacobianFcn(const double *params, const double *verts, const int /*nverts*/, const int ndim, 
                                     double *, double *result) 
    {
      assert(params && verts);
      Matrix3 *J = reinterpret_cast<Matrix3*>(result);
      *J = Matrix3(0.0);
      for (unsigned i = 0; i < 8; ++i) {
        const double   params_p = 1 + params[0]*corner[i][0];
        const double  eta_p = 1 + params[1]*corner[i][1];
        const double zeta_p = 1 + params[2]*corner[i][2];
        const double dNi_dparams   = corner[i][0] * eta_p * zeta_p;
        const double dNi_deta  = corner[i][1] *  params_p * zeta_p;
        const double dNi_dzeta = corner[i][2] *  params_p *  eta_p;
        (*J)(0,0) += dNi_dparams   * verts[i*ndim+0];
        (*J)(1,0) += dNi_dparams   * verts[i*ndim+1];
        (*J)(2,0) += dNi_dparams   * verts[i*ndim+2];
        (*J)(0,1) += dNi_deta  * verts[i*ndim+0];
        (*J)(1,1) += dNi_deta  * verts[i*ndim+1];
        (*J)(2,1) += dNi_deta  * verts[i*ndim+2];
        (*J)(0,2) += dNi_dzeta * verts[i*ndim+0];
        (*J)(1,2) += dNi_dzeta * verts[i*ndim+1];
        (*J)(2,2) += dNi_dzeta * verts[i*ndim+2];
      }
      (*J) *= 0.125;
      return MB_SUCCESS;
    }// LinearHex::jacobian()

    ErrorCode LinearHex::evalFcn(const double *params, const double *field, const int /*ndim*/, const int num_tuples, 
                                 double *, double *result) 
    {
      assert(params && field && num_tuples != -1);
      for (int i = 0; i < num_tuples; i++) result[i] = 0.0;
      for (unsigned i = 0; i < 8; ++i) {
        const double N_i = (1 + params[0]*corner[i][0])
            * (1 + params[1]*corner[i][1])
            * (1 + params[2]*corner[i][2]);
        for (int j = 0; j < num_tuples; j++) result[j] += N_i * field[i*num_tuples+j];
      }
      for (int i = 0; i < num_tuples; i++) result[i] *= 0.125;

      return MB_SUCCESS;
    }

    ErrorCode LinearHex::integrateFcn(const double *field, const double *verts, const int nverts, const int ndim, 
                                      const int num_tuples, double *work, double *result) 
    {
      assert(field && verts && num_tuples != -1);
      double tmp_result[8];
      ErrorCode rval = MB_SUCCESS;
      for (int i = 0; i < num_tuples; i++) result[i] = 0.0;
      CartVect x;
      Matrix3 J;
      for(unsigned int j1 = 0; j1 < LinearHex::gauss_count; ++j1) {
        x[0] = LinearHex::gauss[j1][1];
        double w1 = LinearHex::gauss[j1][0];
        for(unsigned int j2 = 0; j2 < LinearHex::gauss_count; ++j2) {
          x[1] = LinearHex::gauss[j2][1];
          double w2 = LinearHex::gauss[j2][0];
          for(unsigned int j3 = 0; j3 < LinearHex::gauss_count; ++j3) {
            x[2] = LinearHex::gauss[j3][1];
            double w3 = LinearHex::gauss[j3][0];
            rval = evalFcn(x.array(),field, ndim, num_tuples, NULL, tmp_result);
            if (MB_SUCCESS != rval) return rval;
            rval = jacobianFcn(x.array(), verts, nverts, ndim, work, J[0]);
            if (MB_SUCCESS != rval) return rval;
            double tmp_det =  w1*w2*w3*J.determinant();
            for (int i = 0; i < num_tuples; i++) result[i] += tmp_result[i]*tmp_det;
          }
        }
      }

      return MB_SUCCESS;
    }// LinearHex::integrate_vector()

    ErrorCode LinearHex::reverseEvalFcn(EvalFcn eval, JacobianFcn jacob, InsideFcn ins, 
                                        const double *posn, const double *verts, const int nverts, const int ndim,
                                        const double iter_tol, const double inside_tol, double *work, 
                                        double *params, int *is_inside)
    {
      assert(posn && verts);
      return EvalSet::evaluate_reverse(eval, jacob, ins, posn, verts, nverts, ndim, iter_tol, inside_tol, work, 
                                       params, is_inside);
    }

    int LinearHex::insideFcn(const double *params, const int ndim, const double tol)
    {
      return EvalSet::inside_function(params, ndim, tol);
    }

    ErrorCode LinearHex::normalFcn(const int ientDim, const int facet, const int nverts, const double *verts,  double normal[3])
    {
      //assert(facet < 6 && ientDim == 2 && nverts == 8);
      if (nverts != 8)
        MB_SET_ERR(MB_FAILURE, "Incorrect vertex count for passed hex :: expected value = 8 ");
      if (ientDim != 2)
        MB_SET_ERR(MB_FAILURE, "Requesting normal for unsupported dimension :: expected value = 2 ");
      if (facet >6 || facet < 0)
        MB_SET_ERR(MB_FAILURE, "Incorrect local face id :: expected value = one of 0-5");

      int id0 = CN::mConnectivityMap[MBHEX][ientDim-1].conn[facet][0];
      int id1 = CN::mConnectivityMap[MBHEX][ientDim-1].conn[facet][1];
      int id2 = CN::mConnectivityMap[MBHEX][ientDim-1].conn[facet][3];

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
