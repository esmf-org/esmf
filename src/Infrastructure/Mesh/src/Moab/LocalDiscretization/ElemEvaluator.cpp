#include <limits>

#include "moab/ElemEvaluator.hpp"
#include "moab/CartVect.hpp"
#include "moab/Matrix3.hpp"

// need to include eval set types here to support get_eval_set; alternative would be to have some
// type of registration, but we'd still need static registration for the built-in types
#include "moab/LocalDiscretization/LinearTri.hpp"
#include "moab/LocalDiscretization/LinearQuad.hpp"
#include "moab/LocalDiscretization/LinearTet.hpp"
#include "moab/LocalDiscretization/LinearHex.hpp"
#include "moab/LocalDiscretization/QuadraticHex.hpp"
//#include "moab/SpectralQuad.hpp"
//#include "moab/SpectralHex.hpp"

namespace moab { 
    ErrorCode EvalSet::evaluate_reverse(EvalFcn eval, JacobianFcn jacob, InsideFcn inside_f,
                                        const double *posn, const double *verts, const int nverts, 
                                        const int ndim, const double iter_tol, const double inside_tol, 
                                        double *work, double *params, int *inside) {
        // TODO: should differentiate between epsilons used for
        // Newton Raphson iteration, and epsilons used for curved boundary geometry errors
        // right now, fix the tolerance used for NR
      const double error_tol_sqr = iter_tol*iter_tol;
      CartVect *cvparams = reinterpret_cast<CartVect*>(params);
      const CartVect *cvposn = reinterpret_cast<const CartVect*>(posn);

        // initialize to center of element
      *cvparams = CartVect(-.4);
  
      CartVect new_pos;
        // evaluate that first guess to get a new position
      ErrorCode rval = (*eval)(cvparams->array(), verts, ndim, 
                               3, // hardwire to num_tuples to 3 since the field is coords
                               work, new_pos.array());
      if (MB_SUCCESS != rval) return rval;
      
        // residual is diff between old and new pos; need to minimize that
      CartVect res = new_pos - *cvposn;
      Matrix3 J;
      int dum, *tmp_inside = (inside ? inside : &dum);

      int iters=0;
        // while |res| larger than tol
      while (res % res > error_tol_sqr) {
        if(++iters>10) {
            // if we haven't converged but we're outside, that's defined as success
          *tmp_inside = (*inside_f)(params, ndim, inside_tol);
          if (!(*tmp_inside)) return MB_SUCCESS;
          else return MB_FAILURE;
        }

          // get jacobian at current params
        rval = (*jacob)(cvparams->array(), verts, nverts, ndim, work, J.array());
        double det = J.determinant();
        if (det < std::numeric_limits<double>::epsilon()) {
          *tmp_inside = (*inside_f)(params, ndim, inside_tol);
          if (!(*tmp_inside)) return MB_SUCCESS;
          else return MB_INDEX_OUT_OF_RANGE;
        }

          // new params tries to eliminate residual
        *cvparams -= J.inverse() * res;

          // get the new forward-evaluated position, and its difference from the target pt
        rval = (*eval)(params, verts, ndim, 
                       3, // hardwire to num_tuples to 3 since the field is coords
                       work, new_pos.array());
        if (MB_SUCCESS != rval) return rval;
        res = new_pos - *cvposn;
      }

      if (inside)
        *inside = (*inside_f)(params, ndim, inside_tol);

      return MB_SUCCESS;
    }// Map::evaluate_reverse()

    int EvalSet::inside_function(const double *params, const int ndims, const double tol) 
    {
      if (params[0] >= -1-tol && params[0] <= 1+tol &&
          (ndims < 2 || (params[1] >= -1-tol && params[1] <= 1+tol)) &&
          (ndims < 3 || (params[2] >= -1-tol && params[2] <= 1+tol))) 
        return true;
      else return false;
    }

        /** \brief Given type & #vertices, get an appropriate eval set */
    ErrorCode EvalSet::get_eval_set(EntityType tp, unsigned int num_vertices, EvalSet &eval_set) 
    {
      switch (tp) {
        case MBEDGE:
            break;
        case MBTRI:
            if (LinearTri::compatible(tp, num_vertices, eval_set)) return MB_SUCCESS;
            break;
        case MBQUAD:
            if (LinearQuad::compatible(tp, num_vertices, eval_set)) return MB_SUCCESS;
//            if (SpectralQuad::compatible(tp, num_vertices, eval_set)) return MB_SUCCESS;
            break;
        case MBTET:
            if (LinearTet::compatible(tp, num_vertices, eval_set)) return MB_SUCCESS;
            break;
        case MBHEX:
            if (LinearHex::compatible(tp, num_vertices, eval_set)) return MB_SUCCESS;
            if (QuadraticHex::compatible(tp, num_vertices, eval_set)) return MB_SUCCESS;
//            if (SpectralHex::compatible(tp, num_vertices, eval_set)) return MB_SUCCESS;
            break;
        default:
            break;
      }

      return MB_NOT_IMPLEMENTED;
    }
      
    ErrorCode ElemEvaluator::find_containing_entity(Range &entities, const double *point, const double iter_tol, 
                                                    const double inside_tol, EntityHandle &containing_ent, 
                                                    double *params, unsigned int *num_evals) 
    {
      int is_inside;
      ErrorCode rval = MB_SUCCESS;
      unsigned int nevals = 0;
      Range::iterator i;
      for(i = entities.begin(); i != entities.end(); ++i) {
        nevals++;
        set_ent_handle(*i);
        rval = reverse_eval(point, iter_tol, inside_tol, params, &is_inside);
        if (MB_SUCCESS != rval) return rval;
        if (is_inside) break;
      }
      containing_ent = (i == entities.end() ? 0 : *i);
      if (num_evals) *num_evals += nevals;
      return MB_SUCCESS;
    }
} // namespace moab
