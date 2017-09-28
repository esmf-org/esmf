#include "moab/LinearQuad.hpp"
#include "moab/Matrix3.hpp"
#include "moab/Forward.hpp"
#include <math.h>
#include <limits>

namespace moab 
{
    
    const double LinearQuad::corner[4][2] = {{ -1, -1},
                                             {  1, -1},
                                             {  1,  1},
                                             { -1,  1} };

      /* For each point, its weight and location are stored as an array.
         Hence, the inner dimension is 2, the outer dimension is gauss_count.
         We use a one-point Gaussian quadrature, since it integrates linear functions exactly.
      */
    const double LinearQuad::gauss[1][2] = { {  2.0,           0.0          } };

    ErrorCode LinearQuad::jacobianFcn(const double *params, const double *verts, const int /*nverts*/, const int /*ndim*/, 
                                      double *, double *result) 
    {
      Matrix3 *J = reinterpret_cast<Matrix3*>(result);
      *J = Matrix3(0.0);
      for (unsigned i = 0; i < 4; ++i) {
        const double   xi_p = 1 + params[0]*corner[i][0];
        const double  eta_p = 1 + params[1]*corner[i][1];
        const double dNi_dxi   = corner[i][0] * eta_p;
        const double dNi_deta  = corner[i][1] * xi_p;
        (*J)(0,0) += dNi_dxi   * verts[i*3+0];
        (*J)(1,0) += dNi_dxi   * verts[i*3+1];
        (*J)(0,1) += dNi_deta  * verts[i*3+0];
        (*J)(1,1) += dNi_deta  * verts[i*3+1];
      }
      (*J) *= 0.25;
      (*J)(2,2) = 1.0; /* to make sure the Jacobian determinant is non-zero */
      return MB_SUCCESS;
    }// LinearQuad::jacobian()

    ErrorCode LinearQuad::evalFcn(const double *params, const double *field, const int /*ndim*/, const int num_tuples, 
                                  double *, double *result) {
      for (int i = 0; i < num_tuples; i++) result[i] = 0.0;
      for (unsigned i = 0; i < 4; ++i) {
        const double N_i = (1 + params[0]*corner[i][0])
            * (1 + params[1]*corner[i][1]);
        for (int j = 0; j < num_tuples; j++) result[j] += N_i * field[i*num_tuples+j];
      }
      for (int i = 0; i < num_tuples; i++) result[i] *= 0.25;

      return MB_SUCCESS;
    }

    ErrorCode LinearQuad::integrateFcn(const double *field, const double *verts, const int nverts, const int ndim, 
                                       const int num_tuples, double *work, double *result) {
      double tmp_result[4];
      ErrorCode rval = MB_SUCCESS;
      for (int i = 0; i < num_tuples; i++) result[i] = 0.0;
      CartVect x;
      Matrix3 J;
      for(unsigned int j1 = 0; j1 < LinearQuad::gauss_count; ++j1) {
        x[0] = LinearQuad::gauss[j1][1];
        double w1 = LinearQuad::gauss[j1][0];
        for(unsigned int j2 = 0; j2 < LinearQuad::gauss_count; ++j2) {
          x[1] = LinearQuad::gauss[j2][1];
          double w2 = LinearQuad::gauss[j2][0];
          rval = evalFcn(x.array(), field, ndim, num_tuples, NULL, tmp_result);
          if (MB_SUCCESS != rval) return rval;
          rval = jacobianFcn(x.array(), verts, nverts, ndim, work, J[0]);
          if (MB_SUCCESS != rval) return rval;
          double tmp_det =  w1*w2*J.determinant();
          for (int i = 0; i < num_tuples; i++) result[i] += tmp_result[i]*tmp_det;
        }
      }
      return MB_SUCCESS;
    } // LinearHex::integrate_vector()

    ErrorCode LinearQuad::reverseEvalFcn(EvalFcn eval, JacobianFcn jacob, InsideFcn ins, 
                                         const double *posn, const double *verts, const int nverts, const int ndim,
                                         const double iter_tol, const double inside_tol, double *work, 
                                         double *params, int *is_inside) 
    {
      return EvalSet::evaluate_reverse(eval, jacob, ins, posn, verts, nverts, ndim, iter_tol, inside_tol, work, 
                                       params, is_inside);
    } 

    int LinearQuad::insideFcn(const double *params, const int ndim, const double tol) 
    {
      return EvalSet::inside_function(params, ndim, tol);
    }
    
    ErrorCode LinearQuad::normalFcn(const int ientDim, const int facet, const int nverts, const double *verts,  double normal[3])
    {
      //assert(facet <4 && ientDim == 1 && nverts==4);
      if (nverts != 4)
        MB_SET_ERR(MB_FAILURE, "Incorrect vertex count for passed quad :: expected value = 4");
      if (ientDim != 1)
        MB_SET_ERR(MB_FAILURE, "Requesting normal for unsupported dimension :: expected value = 1 ");
      if (facet >4 || facet < 0)
        MB_SET_ERR(MB_FAILURE, "Incorrect local edge id :: expected value = one of 0-3");

      //Get the local vertex ids of  local edge
      int id0 = CN::mConnectivityMap[MBQUAD][ientDim-1].conn[facet][0];
      int id1 = CN::mConnectivityMap[MBQUAD][ientDim-1].conn[facet][1];

      //Find a vector along the edge
      double edge[3];
      for (int i=0; i<3; i++){
          edge[i] = verts[3*id1+i] - verts[3*id0+i];
        }
      //Find the normal of the face
      double x0[3], x1[3], fnrm[3];
      for (int i=0; i<3; i++)
        {
          x0[i] = verts[3*1+i] - verts[3*0+i];
          x1[i] = verts[3*3+i] - verts[3*0+i];
        }
      fnrm[0] = x0[1]*x1[2] - x1[1]*x0[2];
      fnrm[1] = x1[0]*x0[2] - x0[0]*x1[2];
      fnrm[2] = x0[0]*x1[1] - x1[0]*x0[1];

      //Find the normal of the edge as the cross product of edge and face normal

      double a = edge[1]*fnrm[2] - fnrm[1]*edge[2];
      double b = edge[2]*fnrm[0] - fnrm[2]*edge[0];
      double c = edge[0]*fnrm[1] - fnrm[0]*edge[1];
      double nrm = sqrt(a*a+b*b+c*c);

      if (nrm > std::numeric_limits<double>::epsilon()) {
          normal[0] = a/nrm;
          normal[1] = b/nrm;
          normal[2] = c/nrm;
        }
      return MB_SUCCESS;
    }

} // namespace moab
