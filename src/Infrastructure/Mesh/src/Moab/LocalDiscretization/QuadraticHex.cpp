#include "moab/QuadraticHex.hpp"
#include "moab/Forward.hpp"

namespace moab 
{
    
      // those are not just the corners, but for simplicity, keep this name
      //
    const int QuadraticHex::corner[27][3] = {
        { -1, -1, -1 },
        {  1, -1, -1 },
        {  1,  1, -1 },  // corner nodes: 0-7
        { -1,  1, -1 },  // mid-edge nodes: 8-19
        { -1, -1,  1 },  // center-face nodes 20-25  center node  26
        {  1, -1,  1 },  //
        {  1,  1,  1 },
        { -1,  1,  1 }, //                    4   ----- 19   -----  7
        {  0, -1, -1 }, //                .   |                 .   |
        {  1,  0, -1 }, //            16         25         18      |
        {  0,  1, -1 }, //         .          |          .          |
        { -1,  0, -1 }, //      5   ----- 17   -----  6             |
        { -1, -1,  0 }, //      |            12       | 23         15
        {  1, -1,  0 }, //      |                     |             |
        {  1,  1,  0 }, //      |     20      |  26   |     22      |
        { -1,  1,  0 }, //      |                     |             |
        {  0, -1,  1 }, //     13         21  |      14             |
        {  1,  0,  1 }, //      |             0   ----- 11   -----  3
        {  0,  1,  1 }, //      |         .           |         .
        { -1,  0,  1 }, //      |      8         24   |     10
        {  0, -1,  0 }, //      |  .                  |  .
        {  1,  0,  0 }, //      1   -----  9   -----  2
        {  0,  1,  0 }, //
        { -1,  0,  0 },
        {  0,  0, -1 },
        {  0,  0,  1 },
        {  0,  0,  0 }
    };

    double QuadraticHex::SH(const int i, const double params)
    {
      switch (i)
      {
        case -1: return (params*params-params)/2;
        case 0: return 1-params*params;
        case 1: return (params*params+params)/2;
        default: return 0.;
      }
    }
    double QuadraticHex::DSH(const int i, const double params)
    {
      switch (i)
      {
        case -1: return params-0.5;
        case 0: return -2*params;
        case 1: return params+0.5;
        default: return 0.;
      }
    }

    ErrorCode QuadraticHex::evalFcn(const double *params, const double *field, const int /*ndim*/, const int num_tuples, 
                                    double */*work*/, double *result)
    {
      assert(params && field && num_tuples > 0);
      std::fill(result, result+num_tuples, 0.0);
      for (int i=0; i<27; i++)
      {
        const double sh = SH(corner[i][0], params[0]) * SH(corner[i][1], params[1]) * SH(corner[i][2], params[2]);
        for (int j = 0; j < num_tuples; j++) 
          result[j] += sh * field[num_tuples*i+j];
      }

      return MB_SUCCESS;
    }

    ErrorCode QuadraticHex::jacobianFcn(const double *params, const double *verts, const int nverts, const int ndim, 
                                        double */*work*/, double *result)
    {
      assert(27 == nverts && params && verts);
      if (27 != nverts) return MB_FAILURE;
      Matrix3 *J = reinterpret_cast<Matrix3*>(result);
      for (int i=0; i<27; i++)
      {
        const double sh[3]={ SH(corner[i][0], params[0]),
                             SH(corner[i][1], params[1]),
                             SH(corner[i][2], params[2]) };
        const double dsh[3]={ DSH(corner[i][0], params[0]),
                              DSH(corner[i][1], params[1]),
                              DSH(corner[i][2], params[2]) };


        for (int j=0; j<3; j++)
        {
          (*J)(j,0)+=dsh[0]*sh[1]*sh[2]*verts[ndim*i+j]; // dxj/dr first column
          (*J)(j,1)+=sh[0]*dsh[1]*sh[2]*verts[ndim*i+j]; // dxj/ds
          (*J)(j,2)+=sh[0]*sh[1]*dsh[2]*verts[ndim*i+j]; // dxj/dt
        }
      }
      
      return MB_SUCCESS;
    }

    ErrorCode QuadraticHex::integrateFcn(const double */*field*/, const double */*verts*/, const int /*nverts*/, const int /*ndim*/, const int /*num_tuples*/,
                                         double */*work*/, double */*result*/)
    {
      return MB_NOT_IMPLEMENTED;
    }

    ErrorCode QuadraticHex::reverseEvalFcn(EvalFcn eval, JacobianFcn jacob, InsideFcn ins, 
                                           const double *posn, const double *verts, const int nverts, const int ndim,
                                           const double iter_tol, const double inside_tol, double *work, 
                                           double *params, int *is_inside) 
    {
      assert(posn && verts);
      return EvalSet::evaluate_reverse(eval, jacob, ins, posn, verts, nverts, ndim, iter_tol, inside_tol, 
                                       work, params, is_inside);
    } 

    int QuadraticHex::insideFcn(const double *params, const int ndim, const double tol) 
    {
      return EvalSet::inside_function(params, ndim, tol);
    }

    ErrorCode QuadraticHex::normalFcn(const int /*ientDim*/, const int /*facet*/, const int /*nverts*/, const double */*verts*/,  double * /*normal[3]*/)
    {
      return MB_NOT_IMPLEMENTED;
    }
} // namespace moab
