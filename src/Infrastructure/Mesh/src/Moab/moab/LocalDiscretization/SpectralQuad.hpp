#ifndef SPECTRAL_QUAD_HPP
#define SPECTRAL_QUAD_HPP
  /*\brief Shape function space for spectral quad
   */

#include "moab/ElemEvaluator.hpp"
#include "SpectralFuncs.hpp"

namespace moab 
{
    
class SpectralQuad
{
public:
    /** \brief Forward-evaluation of field at parametric coordinates */
  static ErrorCode evalFcn(const double *params, const double *field, const int ndim, const int num_tuples, 
                           double *work, double *result);
        
    /** \brief Reverse-evaluation of parametric coordinates at physical space position */
  static ErrorCode reverseEvalFcn(EvalFcn eval, JacobianFcn jacob, InsideFcn ins, 
                                  const double *posn, const double *verts, const int nverts, const int ndim,
                                  const double iter_tol, const double inside_tol, double *work, 
                                  double *params, int *is_inside);
        
    /** \brief Evaluate the jacobian at a specified parametric position */
  static ErrorCode jacobianFcn(const double *params, const double *verts, const int nverts, const int ndim, 
                               double *work, double *result);
        
    /** \brief Forward-evaluation of field at parametric coordinates */
  static ErrorCode integrateFcn(const double *field, const double *verts, const int nverts, const int ndim, const int num_tuples, double *work, double *result);

    /** \brief Initialize this EvalSet */
  static ErrorCode initFcn(const double *verts, const int nverts, double *&work);
      
        /** \brief Function that returns whether or not the parameters are inside the natural space of the element */
  static int insideFcn(const double *params, const int ndim, const double tol);
  
  static EvalSet eval_set() 
      {
        return EvalSet(evalFcn, reverseEvalFcn, jacobianFcn, integrateFcn, initFcn);
      }
      
  static bool compatible(EntityType tp, int numv, EvalSet &eset) 
      {
        if (tp != MBQUAD) return false;
        int i;
        for (i = 3; i*i == numv || i*i > numv; i++);
        if (i*i != numv) return false;
        eset = eval_set();
        return true;
      }
  
protected:
  static int _n;
  static double *_z[2];
  static lagrange_data _ld[2];
  static opt_data_2 _data; // we should use only 2nd component
  static double * _odwork;// work area

    // flag for initialization of data
  static bool _init;
  static double * _glpoints; // it is a space we can use to store gl positions for elements
    // on the fly; we do not have a tag yet for them, as in Nek5000 application
    // also, these positions might need to be moved on the sphere, for HOMME grids
    // do we project them or how do we move them on the sphere?
};// class SpectralQuad

} // namespace moab

#endif
