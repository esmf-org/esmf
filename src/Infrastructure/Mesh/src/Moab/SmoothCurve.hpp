/*
 * SmoothCurve.hpp
 *
 */

#ifndef SMOOTHCURVE_HPP_
#define SMOOTHCURVE_HPP_

#include "moab/Interface.hpp"
#include "moab/Forward.hpp"
#include "moab/CartVect.hpp"

//#include "RefEdge.hpp"
//#include "SmoothFace.hpp"

#include <map>
#include <vector>

namespace moab {
class SmoothFace;
class GeomTopoTool;
//class SmoothVertex;

//class CMLEdgeMesher;
// evaluator for Camal Edge Mesher
// this is really copying what Cubit is doing

class SmoothCurve
{
public:
	//SmoothCurve(RefEdge * edge, SmoothFace * smoothFaceEval, int loopIndex);
	SmoothCurve(Interface * mb, EntityHandle curve,  GeomTopoTool * gTool); // the new constructor, which will use
	// sense entities to establish the control points on feature edges (geo edges, sets of mesh edges)
	virtual ~SmoothCurve();

	virtual double arc_length() ;

	    //! \brief Get the parametric status of the curve.
	    //!
	    //! \return \a true if curve is parametric, \a false otherwise.
	  virtual bool is_parametric() ;

	    //! \brief Get the periodic status of the curve.
	    //!
	    //! \param period The period of the curve if periodic.
	    //!
	    //! \return \a true if curve is periodic, \a false otherwise.
	  virtual bool is_periodic(double& period) ;

	    //! \brief Get the parameter range of the curve.
	    //!
	    //! \param u_start The beginning curve parameter
	    //! \param u_end The ending curve parameter
	    //!
	    //! \note The numerical value of \a u_start may be greater
	    //! than the numerical value of \a u_end.
	  virtual void get_param_range(double& u_start, double& u_end) ;

	    //! Compute the parameter value at a specified distance along the curve.
	    //!
	    //! \param u_root The start parameter from which to compute the distance
	    //! along the curve.
	    //! \param arc_length The distance to move along the curve.
	    //!
	    //! \note For positive values of \a arc_length the distance will be
	    //! computed in the direction of increasing parameter value along the
	    //! curve.  For negative values of \a arc_length the distance will be
	    //! computed in the direction of decreasing parameter value along the
	    //! curve.
	    //!
	    //! \return The parametric coordinate u along the curve
	  virtual double u_from_arc_length(double u_root, double arc_length);


	    //! \brief Evaluate the curve at a specified parameter value.
	    //!
	    //! \param u The parameter at which to evaluate the curve
	    //! \param x The x coordinate of the evaluated point
	    //! \param y The y coordinate of the evaluated point
	    //! \param z The z coordinate of the evaluated point
	   // ! \param tg, if not null, return the tangent too at u
	  virtual bool position_from_u(double u,
	                               double& x, double& y, double& z,
	                               double * tg = NULL) ;

	    //! \brief Move a point near the curve to the closest point on the curve.
	    //!
	    //! \param x The x coordinate of the point
	    //! \param y The y coordinate of the point
	    //! \param z The z coordinate of the point
	  virtual void move_to_curve(double& x, double& y, double& z);

	    //! Get the u parameter value on the curve closest to x,y,z
	    //! and the point on the curve.
	    //!
	    //! \param x The x coordinate of the point
	    //! \param y The y coordinate of the point
	    //! \param z The z coordinate of the point
	    //!
	    //! \return The parametric coordinate u on the curve
	  virtual double u_from_position(double x, double y, double z, EntityHandle & v,
	      int & indexEdge) ;

	    //! \brief Get the starting point of the curve.
	    //!
	    //! \param x The x coordinate of the start point
	    //! \param y The y coordinate of the start point
	    //! \param z The z coordinate of the start point
	  virtual void start_coordinates(double& x, double& y, double& z) ;

	    //! \brief Get the ending point of the curve.
	    //!
	    //! \param x The x coordinate of the start point
	    //! \param y The y coordinate of the start point
	    //! \param z The z coordinate of the start point
	  virtual void end_coordinates(double& x, double& y, double& z) ;

	  // this will recompute the 2 tangents for each edge, considering the geo edge they are into
	  void compute_tangents_for_each_edge();

	  void compute_control_points_on_boundary_edges(double min_dot,
			  std::map<EntityHandle, SmoothFace*> & mapSurfaces,
			  Tag controlPointsTag, Tag markTag);

	  ErrorCode evaluate_smooth_edge(EntityHandle eh, double &tt,
	  		CartVect & outv, CartVect & out_tangent) ;

private:

	  std::vector<EntityHandle> _entities;// the mesh edges are stored here for fast access
	  double _leng;
	  std::vector<double> _fractions;// they are increasing from 0. to 1., do we need these?
	  // this will be decided apriori, and eventually reset for paver
	  // fractions will be from 0.0.. to 1.0, they will be decided upon the length of the geo edge

	  Tag _edgeTag;

	  Interface * _mb;
	  EntityHandle _set;
	  GeomTopoTool * _gtt;



};

} // namespace moab
#endif /* SMOOTHCURVE_HPP_ */
