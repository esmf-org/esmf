#include "moab/Interface.hpp"
#include "moab/BoundBox.hpp"

// this is used for spherical elements, bounding box needs to be updated due to curvature
#include "moab/IntxMesh/IntxUtils.hpp"

namespace moab 
{
    ErrorCode BoundBox::update(Interface &iface, const Range& elems, bool spherical,
        double radius)
    {
      ErrorCode rval;
      bMin = CartVect(HUGE_VAL);
      bMax = CartVect(-HUGE_VAL);
      
      CartVect coords;
      EntityHandle const *conn = NULL, *conn2 = NULL;
      int len = 0, len2 = 0;
      Range::const_iterator i;
      CartVect coordverts[27];// maximum nodes per element supported by MOAB
  
        // vertices
      const Range::const_iterator elem_begin = elems.lower_bound( MBEDGE );
      for (i = elems.begin(); i != elem_begin; ++i) {
        rval = iface.get_coords( &*i, 1, coords.array() );
        if (MB_SUCCESS != rval)
          return rval;
        update_min(coords.array());
        update_max(coords.array());
      }

        // elements with vertex-handle connectivity list
      const Range::const_iterator poly_begin = elems.lower_bound( MBPOLYHEDRON, elem_begin );
      std::vector<EntityHandle> dum_vector;
      for (i = elem_begin; i != poly_begin; ++i) {
        rval = iface.get_connectivity( *i, conn, len, true, &dum_vector);
        if (MB_SUCCESS != rval)
          return rval;

        rval = iface.get_coords( conn, len, &(coordverts[0][0]) );
        if (MB_SUCCESS != rval)
          return rval;
        for (int j = 0; j < len; ++j) {
          update_min(coordverts[j].array());
          update_max(coordverts[j].array());
        }
        // if spherical, use gnomonic projection to improve the computed box
        // it is used now for coupling only
        if (spherical)
          update_box_spherical_elem(coordverts, len, radius);
      }
  
        // polyhedra
      const Range::const_iterator set_begin  = elems.lower_bound( MBENTITYSET, poly_begin );
      for (i = poly_begin; i != set_begin; ++i) {
        rval = iface.get_connectivity( *i, conn, len, true );
        if (MB_SUCCESS != rval)
          return rval;

        for (int j = 0; j < len; ++j) {
          rval = iface.get_connectivity( conn[j], conn2, len2 );
          if (MB_SUCCESS != rval)
            return rval;
          rval = iface.get_coords( conn2, len2, &(coordverts[0][0]) );
          if (MB_SUCCESS != rval)
            return rval;
          for (int k = 0; k < len2; ++k) {
            update_min(coordverts[k].array());
            update_max(coordverts[k].array());
          }
        }
      }
  
        // sets
      BoundBox box;
      for (i = set_begin; i != elems.end(); ++i) {
        Range tmp_elems;
        rval = iface.get_entities_by_handle(*i, tmp_elems);
        if (MB_SUCCESS != rval) return rval;
        rval = box.update(iface, tmp_elems, spherical, radius);
        if (MB_SUCCESS != rval) return rval;

        update(box);
      }
  
      return MB_SUCCESS;
    }
    void BoundBox::update_box_spherical_elem(const CartVect * verts, int len, double R)
    {
      // decide first the gnomonic plane, based on first coordinate
      int plane=-1;
      CartVect pos;
      decide_gnomonic_plane(verts[0], plane);
      double in_plane_positions[20]; // at most polygons with 10 edges; do we need to revise this?
      for (int i=0; i<len && i<10; i++ )
        gnomonic_projection(verts[i], R, plane, in_plane_positions[2*i], in_plane_positions[2*i+1]);
      // look for points on the intersection between edges in gnomonic plane and coordinate axes
      // if yes, reverse projection of intx point, and update box
      double oriented_area2=0;
      // if oriented area is != zero, it means the origin is inside the polygon, so we need to upgrade the
      // box with the origin, so we do not miss anything (the top of the dome)
      for (int i=0; i<len; i++)
      {
        int i1 = (i+1)%len; // next vertex in polygon
        // check intx with x axis
        double ax = in_plane_positions[2*i], ay=in_plane_positions[2*i+1];
        double bx = in_plane_positions[2*i1], by=in_plane_positions[2*i1+1];
        if (ay*by<0)// it intersects with x axis
        {
          double alfa = ay/(ay-by);
          double xintx = ax+alfa*(bx-ax);
          reverse_gnomonic_projection(xintx, 0, R, plane, pos);
          update_min(pos.array());
          update_max(pos.array());
        }
        if (ax*bx<0)// it intersects with y axis
        {
          double alfa = ax/(ax-bx);
          double yintx = ay+alfa*(by-ay);
          reverse_gnomonic_projection(0, yintx, R, plane, pos);
          update_min(pos.array());
          update_max(pos.array());
        }
        oriented_area2 += (ax*by-ay*bx);
      }
      if (fabs(oriented_area2) > R*R * 1.e-6) // origin is in the interior, add the center
      {
        reverse_gnomonic_projection(0, 0, R, plane, pos);
        update_min(pos.array());
        update_max(pos.array());
      }
    }
} // namespace moab
