/*! \file HiReconstruction.hpp
 * This class implements a high order surface/curve reconstruction method which takes a surface/curve mesh as input 
 * and compute local polynomial fittings (in monomial basis) around user specified vertices. Little noise is allowed and least square 
 * will be used in such case. 
 * This method assumes the underlying geometry of input mesh is smooth.
 * The local fitting results could be used for estimate the exact geometry of the surface. For instance, if mesh 
 *  refinement is perform on the input mesh, then the position of new vertices introduced by refinement could be 
 * estimated by the local fitting, rather than using linear interpolation.
 * Implementations are based on the WALF method in paper:
 * Jiao, Xiangmin, and Duo Wang. "Reconstructing high-order surfaces for meshing." Engineering with Computers 28.4 (2012): 361-373.
 */


#ifndef HI_RECONSTRUCTION_HPP
#define HI_RECONSTRUCTION_HPP

#include "moab/Range.hpp"
#include "moab/HalfFacetRep.hpp"

#ifdef MOAB_HAVE_MPI
#include "moab/ParallelComm.hpp"
#endif

#include <vector>

namespace moab
{
	enum GEOMTYPE{ HISURFACE, HI3DCURVE, HI2DCURVE};

	class Core;
	class HalfFaceRep;
	class ParallelComm;

	class HiReconstruction
	{
		public:

			HiReconstruction(Core *impl, ParallelComm *comm=0, EntityHandle meshIn=0, int minpnts=5, bool recwhole=true);

			~HiReconstruction();

			ErrorCode initialize(bool recwhole);

			//! \brief Reconstruct a high order surface on given surface mesh
			/** Given a mesh, compute vertex based polynomial fittings for all vertices hosted by current processor. 
				* The result will be stored interally for later usage of evalution. The inputs are: a) degree, which
				* is the order of polynomial used for vertex based fitting. b) interp, if it's true, then interpolation
				* will be applied for local fitting, otherwise it's least square fitting. c) safeguard, specifies whether
				* to use safeguarded numeric method. d) reset, if fittings have been computed and stored in current
				* object, then reset=true will recompute the fittings based on user input and replace the existing one.
				* \param degree Integer, order of polynomials used for local fittings.
				* \param interp Boolean, true=Interpolation, false=least square fitting.
				* \param safeguard Boolean, true=using safe guarded method in numerical computing.
				* \param reset Boolean, reset=true will recompute the fittings based on user input and replace the existing one.
			*/
			ErrorCode reconstruct3D_surf_geom(int degree, bool interp, bool safeguard, bool reset=false);

			//! \brief Reconstruct a high order surface on given surface mesh
			/** Given a mesh, compute vertex based polynomial fittings for all vertices hosted by current processor. 
				* User could specify various degrees for different vertices. It assumes that the input degrees for 
				* vertices stored in the same order as that this class stores vertices: 1) reconstruction will be 
				* only performed at vertices hosted by current processor, thus input npts should match the number of 
				* hosted vertices. 2) all hosted vertices will be stored in a MOAB::Range object, degrees for all these 
				* vertices should be stored in degrees as the same order in the MOAB::Range object
				* The result will be stored interally for later usage of evalution.
				* \param npts Integer size of array pointed by degrees, used for check
				* \param degrees Integer arrray, order of polynomials for local fitting at all hosted vertices
				* \param interp Boolean, true=Interpolation, false=least square fitting.
				* \param safeguard Boolean, true=using safe guarded method in numerical computing.
				* \param reset Boolean, reset=true will recompute the fittings based on user input and replace the existing one.
			*/
			ErrorCode reconstruct3D_surf_geom(size_t npts, int* degrees, bool* interps, bool safeguard, bool reset=false);

			//! \brief Reconstruct a high order curve on given curve mesh
			/** Given a curve mesh, compute vertex based polynomail fittings for all vertices hosted by current processor.
				* The vertex based fitting is done by perfoming three one-parameter fittings along each axis, i.e. x,y,z.
				* The result will be stored interally for later usage of evalution.
				* \param degree Integer, order of polynomials used for local fittings.
				* \param interp Boolean, true=Interpolation, false=least square fitting.
				* \param safeguard Boolean, true=using safe guarded method in numerical computing.
				* \param reset Boolean, reset=true will recompute the fittings based on user input and replace the existing one.
			*/
			ErrorCode reconstruct3D_curve_geom(int degree, bool interp, bool safeguard, bool reset=false);

			//! \brief Reconstruct a high order curve on given curve mesh
			/** Given a curve mesh, compute vertex based polynomail fittings for all vertices hosted by current processor.
				* The vertex based fitting is done by perfoming three one-parameter fittings along each axis, i.e. x,y,z.
				* User could specify various degrees for different vertices. It assumes that the input degrees for 
				* vertices stored in the same order as that this class stores vertices: 1) reconstruction will be 
				* only performed at vertices hosted by current processor, thus input npts should match the number of 
				* hosted vertices. 2) all hosted vertices will be stored in a MOAB::Range object, degrees for all these 
				* vertices should be stored in degrees as the same order in the MOAB::Range object
				* The result will be stored interally for later usage of evalution.
				* \param npts Integer size of array pointed by degrees, used for check
				* \param degrees Integer arrray, order of polynomials for local fitting at all hosted vertices.
				* \param interp Boolean, true=Interpolation, false=least square fitting.
				* \param safeguard Boolean, true=using safe guarded method in numerical computing.
				* \param reset Boolean, reset=true will recompute the fittings based on user input and replace the existing one.
			*/
			ErrorCode reconstruct3D_curve_geom(size_t npts, int* degrees, bool* interps, bool safeguard, bool reset=false);

			//! \brief Construct vertex based polynomial fitting on a surface mesh
			/** Given a vertex on a surface mesh, construct a local fitting around this vertex. Stencils around this vertex will 
				* be selected according to input degree and if data is noise. Local uv-plane will be the estimated tangent plane
				* at this vertex. minpnts will be used to specify the minimum number allowed in the local stencil.
				* The result will be returned to user by preallocated memory coords, degree_out, coeffs.
				* \param vid EntityHandle, the fitting will be performed around this vertex for the local height function over the uv-plane.
				* \param interp Boolean, true=Interpolation, false=least square fitting.
				* \param degree Integer, order of polynomials used for local fittings.
				* \param minpnts Integer, the allowed minimum number of vertices in local stencil. If too small, the resulted fitting might be low order accurate. If too large, it may introduce overfitting.
				* \param safeguard Boolean, true=using safe guarded method in numerical computing.
				* \param coords Pointer to double, preallocated memory by user, should have at least 9 doubles; stores the global coordinates of local coordinates system uvw directions.
				* \param degree_out Pointer to integer, used to store the degree of resulted fitting
				* \param coeffs, Pointer to double, preallocated memory for coefficients of local fittings, should have at least (degree+2)(degree+1)/2 doubles.
			*/
			ErrorCode polyfit3d_walf_surf_vertex(const EntityHandle vid, const bool interp, int degree, int minpnts, const bool safeguard, const int ncoords, double* coords, int* degree_out, const int ncoeffs, double* coeffs);

			//! \brief Construct vertex based polynomial fitting on a curve mesh
			/** Given a vertex on a curve mesh, construct three one-parameter local fittings for each coordinates axis around
				* this vertex. Stencils around this vertex will be selected according to input degree and if data is noise. 
				* Local u-line, or the single parameter will be the estimated tangent line at this vertex. On each axis of xyz,
				* a polynomial fitting will be performed according to user input. 
				* minpnts will be used to specify the minimum number allowed in the local stencil.
				* The result will be returned to user by preallocated memory coords, degree_out, coeffs.
				* \param vid EntityHandle, the fittings will be performed around this vertex.
				* \param interp Boolean, true=Interpolation, false=least square fitting.
				* \param degree Integer, order of polynomials used for local fittings.
				* \param minpnts Integer, the allowed minimum number of vertices in local stencil. If too small, the resulted fitting might be low order accurate. If too large, it may introduce overfitting.
				* \param safeguard Boolean, true=using safe guarded method in numerical computing.
				* \param coords Pointer to double, preallocated memory by user, should have at least 3 doubles; stores the global coordinates of local coordinate system u direction.
				* \param degree_out Pointer to integer, used to store the degree of resulted fitting
				* \param coeffs, Pointer to double, preallocated memory for coefficients of local fittings, should have at least 3*(degree+1) doubles.
			*/
			ErrorCode polyfit3d_walf_curve_vertex(const EntityHandle vid, const bool interp, int degree, int minpnts, const bool safeguard, const int ncoords, double* coords, int* degree_out, const int ncoeffs, double* coeffs);

			//! \brief Perform high order projection of points in an element, using estimated geometry by reconstruction class
			/** Given an element on the input mesh, and new points in this element, represented as natural coordinates in element, 
				* estimate their position in surface. This is done by weighted averaging of local fittings: for each vertex of this 
				* elment, a fitting has been computed and the new points could be projected by this fitting. The final result of projection
				* is the weighted average of these projections, weights are chosen as the barycentric coordinates of the point in this element.
				* The result will be returned to the user preallocated memory
				* \param elem EntityHandle, the element on which to perform high order projection.
				* \param nvpe Integer, number of nodes of this element, triangle is 3, quad is four.
				* \param npts2fit Integer, number of points lying in elem to be projected.
				* \param naturalcoords2fit Pointer to array of doubles, size=nvpe*npts2fit, natural coordinates in elem of points to be projected.
				* \param newcoords Pointer to array of doubles, preallocated by user, size=3*npts2fit, estimated positions of input points.
			*/
			ErrorCode hiproj_walf_in_element(EntityHandle elem, const int nvpe, const int npts2fit, const double* naturalcoords2fit, double* newcoords);

			//! \brief Perform high order projection of points around a vertex, using estimated geometry by reconstruction class
			/** Given an vertex on the input mesh, and new points around this vertex, estimate their position in surface. 
				* This is done by first projecting input points onto the local uv-plane around this vertex and use the precomputed local
				* fitting to estimate the ideal position of input points. 
				* The result will be returned to the user preallocated memory
				* \param vid EntityHandle, the vertex around which to perform high order projection.
				* \param npts2fit Integer, number of points lying around vid to be fitted.
				* \param coords2fit Pointer to array of doubles, size=3*npts2fit, current coordinates of points to be projected.
				* \param newcoords Pointer to array of doubles, preallocated by user, size=3*npts2fit, estimated positions of input points.
			*/
			ErrorCode hiproj_walf_around_vertex(EntityHandle vid, const int npts2fit, const double* coords2fit, double* hiproj_new);

			//! \brief Perform high order projection of points around a center vertex, assume geometry is surface
			/** Given a vertex position and the local fitting parameter around this vertex, estimate the ideal position of input position
				* according to the local fitting. This is done by first projecting input points onto the local uv-plane around this vertex 
				* and use the given fitting to estimate the ideal position of input points.
				* The result will be returned to user preallocated memory
				* \param local_origin Pointer to 3 doubles, coordinates of the center vertex
				* \param local_coords Pointer to 9 doubles, global coordinates of directions of local uvw coordinates axis at center vertex
				* \param local_deg Integer, order of local polynomial fitting
				* \param local_coeffs Pointer to array of doubles, size=(local_deg+2)(local_deg+1)/2, coefficients of local polynomial fittings, in monomial basis
				* \param interp Boolean, true=local fitting is interpolation, false=local fitting is least square fitting
				* \param npts2fit Integer, number of points to be estimated, around the center vertices
				* \param coords2fit Pointer to array of doubles, size=3*npts2fit, current coordinates of points to be estimated
				* \param hiproj_new Pointer to array of doubles, size=3*npts2fit, memory preallocated by user to store the fitting/estimated positions of input points.
			*/
			void walf3d_surf_vertex_eval(const double* local_origin, const double* local_coords, const int local_deg, const double* local_coeffs, const bool interp, const int npts2fit, const double* coords2fit, double* hiproj_new);

			//! \brief Perform high order projection of points around a center vertex, assume geometry is curve
			/** Given a vertex position and the local one-parameter fittings parameter around this vertex, estimate the ideal position of input position
				* according to the local fittings. This is done by first projecting input points onto the local u-direction at this vertex 
				* and then use the value u as parameter for the three fittings, one for each coordinates axis of xyz. 
				* The result will be returned to user preallocated memory
				* \param local_origin Pointer to 3 doubles, coordinates of the center vertex
				* \param local_coords Pointer to 3 doubles, global coordinates of direction of local u coordinate axis at center vertex
				* \param local_deg Integer, order of local polynomial fitting
				* \param local_coeffs Pointer to array of doubles, size=3*(local_deg+1), coefficients of three local polynomial fittings, in monomial basis. For each fitting, local_deg+1 parameters.
				* \param interp Boolean, true=local fitting is interpolation, false=local fitting is least square fitting
				* \param npts2fit Integer, number of points to be estimated, around the center vertices
				* \param coords2fit Pointer to array of doubles, size=3*npts2fit, current coordinates of points to be estimated
				* \param hiproj_new Pointer to array of doubles, size=3*npts2fit, memory preallocated by user to store the fitting/estimated positions of input points.
			*/
			void walf3d_curve_vertex_eval(const double* local_origin, const double* local_coords, const int local_deg, const double* local_coeffs, const bool interp, const int npts2fit, const double* coords2fit, double* hiproj_new);


			//! \brief Get interally stored fitting results
			/** Get fittings results of a vertex, stored internally, results will be writtend to user provided memory
				* \param vid EntityHandle, a vertex in _verts2rec
				* \param geomtype GEOMTYPE, one of HISURFACE,HI3DCURVE,HI2DCURVE
				* \param coords vector, global coordinates of local uvw coordinate system axis directions will be appended to the end of coords
				* \param degree_out Reference to Integer, order of polynomial fittings for vid
				* \param coeffs vector, coefficients of local polynomial fittings in monomial basis will be appended to the end of coeffs
				* \param interp Reference to Boolean, true =  interpolation
			*/
			bool get_fittings_data(EntityHandle vid, GEOMTYPE& geomtype, std::vector<double>& coords, int& degree_out, std::vector<double>& coeffs, bool& interp);

			//Helper function: estimate require number of ghost layers in parallel setting
			static int estimate_num_ghost_layers(int degree,bool interp=false){
				return 1+(interp?((degree+1)>>1)+((degree+1)&1):((degree+2)>>1)+((degree+2)&1));
			};

		protected:
			Core *mbImpl;
			ParallelComm *pcomm;
			HalfFacetRep *ahf;
			//prevent copying
			HiReconstruction(const HiReconstruction& source);
			HiReconstruction& operator= (const HiReconstruction& right);

			//mesh on which to perform reconstruction
			EntityHandle _mesh2rec;
			//_verts2rec all locally hosted vertices, in parallel might be different from _invert which is all the vertices in _mesh2rec, including ghost vertices
			Range _verts2rec,_inverts,_inedges,_infaces,_incells;
			size_t _nv2rec;//size of _verts2rec

			int _MAXPNTS,_MINPNTS;
			double _MINEPS;

			//in curve mesh, _hasderiv=true means vertex tangent vectors have been computed over _verts2rec
			//in surface mesh, _hasderiv=true means vertex normals have been computed over _verts2rec
			bool _hasderiv;

			GEOMTYPE _geom;
			int _dim;
			bool _hasfittings;
			bool _initfittings;
			std::vector<double> _local_coords;
			std::vector<double> _local_fit_coeffs;
			std::vector<size_t> _vertID2coeffID;
			std::vector<int> _degrees_out;
			std::vector<bool> _interps;

			//Estimate stencil size
			int estimate_num_rings(int degree,bool interp);

			//! \brief Given a vertex, return the incident elements with dimension elemdim
			/** Wrapper of MOAB Core->get_adjacencies and HalfRep->get_up_adjacencies, depends on if USE_AHF is defined
				* \param vid EntityHandle of vertex
				* \param elemdim Integer, dimension of elements incidented in vid
				* \param adjents vector<EntityHandle>, container which push incident elements in
			*/
			ErrorCode vertex_get_incident_elements(const EntityHandle& vid, const int elemdim, std::vector<EntityHandle>& adjents);

			//! \brief Get n-ring neighbor vertices, assuming curve/surface mesh, not volume mesh
			/** Given a vertex, find its n-ring neighbor vertices including itself in _mesrh2rec. 
				* 1-ring neighbor vertices of a vertex are the vertices connected with this vertex with an edge
				* n-ring vertices are obtained first get the 1-ring vertices and then get the 1-ring of these vertices, and so on
				* \param vid EntityHandle, vertex around which to get n-ring vertices
				* \param ring Integer, number of rings
				* \param minpnts Integer, number of minimum vertices to obtain, if the input ring could not provide enough vertices, i.e. more than minpnts, then expand the number of rings
				* \param ngbvs Range, the n-ring vertices of vid, including vid. If too many points found, i.e. more than _MAXPNTS, then terminate early.
			*/
			ErrorCode obtain_nring_ngbvs(const EntityHandle vid, int ring, const int minpnts, Range& ngbvs);

			/** Initialize the storage for fitting results over _mesh2rec, curve/surface mesh
				* Two options are provided: a) use uniform degree for all vertices b) use customized degrees for different vertices
				* After calling of initializing functions, _initfitting is set to be true, the fitting result could be stored internally
			*/
			void initialize_surf_geom(const int degree);
			void initialize_surf_geom(const size_t npts, const int* degrees);
			void initialize_3Dcurve_geom(const int degree);
			void initialize_3Dcurve_geom(const size_t npts, const int* degrees);

			/** Save fitting results of a vertex into internal storage
				* \param vid EntityHandle, a vertex in _mesh2rec, in _verts2rec
				* \param coords Pointer to double array, global coordinates of local uvw coordinate system axis directions
				* \param degree_out Integer, order of polynomial fittings for vid
				* \param coeffs Pointer to double array, coefficients of local polynomial fittings in monomial basis
				* \param interp Boolean, true =  interpolation
			*/
			//ErrorCode set_geom_data_surf(const EntityHandle vid, const double* coords, const double degree_out, const double* coeffs, bool interp);
			//ErrorCode set_geom_data_3Dcurve(const EntityHandle vid, const double* coords, const double degree_out, const double* coeffs, bool interp);

			/** Compute area weighted average vertex normals for given vertex, assuming surface mesh
				* For arbitrary polygon mesh, use incident two edges of each incident polygon of this vertex to form a triangle, then use 
				* these incident "triangles" to compute area weighted average vertex normals
				* \param vid EntityHandle, vertex in _mesh2rec, might be ghost vertex
				* \param nrm Pointer to 3-doubles array, preallocated by user
			*/
			ErrorCode average_vertex_normal(const EntityHandle vid, double* nrm);
			
			/** Compute weighted average vertex normals for all vertices in _verts2rec, not including ghost vertices, results
				* are stored interally in _local_coords
			*/
			ErrorCode compute_average_vertex_normals_surf();

			/** Return the normals of given vertices in a Range, writing to preallocated memory
				* If normals have been computed and stored, just access them
				* If not, compute on the fly
				* \param vertsh Range, EntityHandles of vertices
				* \param nrms Pointer of array of doubles, size = 3*vertsh.size()
			*/
			ErrorCode get_normals_surf(const Range& vertsh, double* nrms);

			/** Compute area weighted average vertex tangent vector for given vertex, assuming curve mesh
				* Use incident two edges of vertex as estimatation of tangent vectors, weighted by length
				* \param vid EntityHandle, vertex in _mesh2rec, might be ghost vertex
				* \param tang Pointer to 3-doubles array, preallocated by user
			*/
			ErrorCode average_vertex_tangent(const EntityHandle vid, double* tang);

			/** Compute weighted average vertex tangent vectors for all vertices in _verts2rec, not including ghost vertices, results
				* are stored interally in _local_coords
			*/
			ErrorCode compute_average_vertex_tangents_curve();

			/** Return the tangent vectors of given vertices in a Range, writing to preallocated memory
				* If tangent vectors have been computed and stored, just access them
				* If not, compute on the fly
				* \param vertsh Range, EntityHandles of vertices
				* \param tangs Pointer of array of doubles, size = 3*vertsh.size()
			*/
			ErrorCode get_tangents_curve(const Range& vertsh, double* tangs);

			//! \brief Compute local coordinates system of a vertex, and perform vertex based polynomial fittings of local height function
			/** This function take the first vertex of input as center, and build local uv-plane by estimating vertex normals and tangent planes
				* Then other vertices forms vectors starting from center and then are projectd onto this uv-plane to form a local height function. Local fitting of this local height function
				* is performed in WLS sense, according if interpolation required or not.
				* \param nverts Integer, number of vertices of input
				* \param ngbcors Pointer to array of doubles, size = 3*nverts, coordinates of input vertices, first will be center
				* \param ngbnrms Pointer to array of doubles, size = 3*nverts, vertex normals of input vertices
				* \param degree Integer, user specified fitting degree
				* \param interp Boolean, user input, interpolation or not
				* \param safeguard Boolean, true = use safeguarded numerical method in computing
				* \param ncoords Integer, size of *coords, should be 9, used for check
				* \param coords Pointer to array of doubles, preallocated memory for storing the glocal coordinates of local uvw axis directions
				* \param ncoeffs Integer, size of *coeffs, should be (degree+2)(degree+1)/2, used for check
				* \param coeffs Pointer to array of doubles, preallocated memory for storing coefficients of local fittings in monomial basis
				* \param degree_out Pointer to integer, order of resulted polynomial of fitting, could be downgraded due to numerical issues
				* \param degree_pnt Pointer to integer, polynomial fitting order determined by stencil size/number of points
				* \param degree_qr Pointer to integer, polynomial fitting order determined by Vandermonde system condition number
			*/
			void polyfit3d_surf_get_coeff(const int nverts, const double* ngbcors, const double* ngbnrms, int degree, const bool interp, const bool safeguard, const int ncoords, double* coords, const int ncoeffs, double* coeffs, int* degree_out, int* degree_pnt, int* degree_qr);
			//! \brief Form and solve Vandermonde system of bi-variables 
			void eval_vander_bivar_cmf(const int npts2fit, const double* us, const int ndim, double* bs, int degree, const double* ws, const bool interp, const bool safeguard, int* degree_out, int* degree_pnt, int* degree_qr);

			//! \brief Compute local single variable coordinate system of a vertex, and perform vertex based polynomial fittings of three global coordinates axis
			/** This function take the first vertex of input as center, and build local u-line by estimating tangent vector
				* Then other vertices form vectors originating from center and vectors then are projectd onto this u-plane to form three local height functions, 
				* one for each coordinates axis. Local fitting of these local height functions are performed in WLS sense, according if interpolation required or not.
				* \param nverts Integer, number of vertices of input
				* \param ngbcors Pointer to array of doubles, size = 3*nverts, coordinates of input vertices, first will be center
				* \param ngbtangs Pointer to array of doubles, size = 3*nverts, vertex tangent vectors of input vertices
				* \param degree Integer, user specified fitting degree
				* \param interp Boolean, user input, interpolation or not
				* \param safeguard Boolean, true = use safeguarded numerical method in computing
				* \param ncoords Integer, size of *coords, should be 3, used for check
				* \param coords Pointer to array of doubles, preallocated memory for storing the glocal coordinates of local u axis direction
				* \param ncoeffs Integer, size of *coeffs, should be 3*(degree+1), used for check
				* \param coeffs Pointer to array of doubles, preallocated memory for storing coefficients of local fittings in monomial basis
				* \param degree_out Pointer to integer, order of resulted polynomial of fitting, could be downgraded due to numerical issues
			*/
			void polyfit3d_curve_get_coeff(const int nverts, const double* ngbcors, const double* ngbtangs, int degree, const bool interp, const bool safeguard, const int ncoords, double* coords, const int ncoeffs, double* coeffs, int* degree_out);
			//! \brief Form and solve Vandermonde system of single-variables
			void eval_vander_univar_cmf(const int npts2fit, const double* us, const int ndim, double* bs, int degree, const double* ws, const bool interp, const bool safeguard, int* degree_out);
			//! \brief Compute weights for points selected in weighted least square fittigns
			int compute_weights(const int nrows, const int ncols, const double* us, const int nngbs, const double* ngbnrms, const int degree, const double toler, double* ws);
			//! \brief Check the correctness of barycentric coordination, wi>=0 and sum(wi)=1
			bool check_barycentric_coords(const int nws, const double* naturalcoords);
	};//class HiReconstruction
}//namespace moab
#endif
