#ifndef DGMSOLVER_HPP
#define DGMSOLVER_HPP
#include <vector>

namespace moab
{

  class DGMSolver
  {
    DGMSolver() {};
    ~DGMSolver() {};
public:
    
    //! \brief compute combinational number, n choose k, maximum output is std::numeric_limits<unsigned int>::max();
    //If overflows, return 0
    static unsigned int nchoosek(unsigned int n, unsigned int k);

    //! \brief compute the number of columns for a multivariate vandermonde matrix, given certen degree
    /** If degree = 0, out put is 1;
        *If kvars = 1, degree = k, output is k+1;
        *If kvars = 2, degree = k, output is (k+2)*(k+1)/2;
    */
    static unsigned int compute_numcols_vander_multivar(unsigned int kvars,unsigned int degree);

    //! \brief compute the monomial basis of mutiple variables, up to input degree, lexicographically ordered
    /** if degree = 0, output basis = {1}
        *If kvars = 1, vars = {u}, degree = k, basis = {1,u,...,u^k}
        *If kvars = 2, vars = {u,v}, degree = k, basis = {1,u,v,u^2,uv,v^2,u^3,u^2*v,uv^2,v^3,...,u^k,u^k-1*v,...,uv^k-1,v^k}
        *If kvars = 3, vars = {u,v,w}, degree = k, basis = {1,u,v,w,u^2,uv,uw,v^2,v*w,w^2,...,u^k,u^k-1v,u^k-1w,...,v^k,v^k-1w,...,vw^k-1,w^k}
        * \param kvars Integer, number of variables
        * \param vars Pointer to array of doubles, size = kvars, variable values
        * \param degree Integer, maximum degree
        * \param basis Reference to vector, user input container to hold output which is appended to existing data; users don't have to preallocate for basis, this function will allocate interally 
    */
    static void gen_multivar_monomial_basis(const int kvars,const double* vars, const int degree, std::vector<double>& basis);

    //! \brief compute multivariate vandermonde matrix, monomial basis ordered in the same way as gen_multivar_monomial_basis
    /** if degree = 0, V = {1,...,1}';
        *If kvars = 1, us = {u1;u2;..,;um}, degree = k, V = {1 u1 u1^2 ... u1^k;1 u2 u2^2 ... u2^k;...;1 um um^2 ... um^k};
        *If kvars = 2, us = {u1 v1;u2 v2;...;um vm}, degree = k, V = {1 u1 v1 u1^2 u1v1 v1^2;...;1 um vm um^2 umvm vm^2};
        * \param mrows Integer, number of points to evaluate Vandermonde matrix
        * \param kvars Integer, number of variables
        * \param us Pointer to array of doubles, size = mrow*kvars, variable values for all points. Stored in row-wise, like {u1 v1 u2 v2 ...}
        * \param degree Integer, maximum degree
        * \param basis Reference to vector, user input container to hold Vandermonde matrix which is appended to existing data; 
                 users don't have to preallocate for basis, this function will allocate interally; 
                 the Vandermonde matrix is stored in an array, columnwise, like {1 ... 1 u1 ...um u1^2 ... um^2 ...}
    */ 
    static void gen_vander_multivar(const int mrows,const int kvars, const double* us, const int degree, std::vector<double>& V);

    static void rescale_matrix(int mrows, int ncols, double *V, double *ts);

    static void compute_qtransposeB(int mrows, int ncols, const double *Q, int bncols, double *bs);

    static void qr_polyfit_safeguarded(const int mrows, const int ncols, double *V, double *D, int *rank);

    static void backsolve(int mrows, int ncols, double *R, int bncols, double *bs, double *ws);

    static void backsolve_polyfit_safeguarded(int dim, int degree, const bool interp, int mrows, int ncols, double *R, int bncols, double *bs, const double *ws, int *degree_out);

    static void vec_dotprod(const int len, const double* a, const double* b, double* c);

    static void vec_scalarprod(const int len, const double* a, const double c, double* b);

    static void vec_crossprod(const double a[3], const double b[3], double (&c)[3]);

    static double vec_innerprod(const int len, const double* a, const double* b);

    static double vec_2norm(const int len, const double* a);

    static double vec_normalize(const int len, const double* a, double* b);

    static double vec_distance(const int len, const double* a, const double*b);

    static void vec_projoff(const int len, const double* a, const double* b, double* c);

    static void vec_linear_operation(const int len, const double mu, const double* a, const double psi, const double* b, double* c);

    static void get_tri_natural_coords(const int dim, const double* cornercoords, const int npts, const double* currcoords, double* naturalcoords);

  };

}
#endif
