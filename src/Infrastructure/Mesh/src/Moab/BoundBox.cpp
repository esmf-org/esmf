#include "moab/Interface.hpp"
#include "moab/BoundBox.hpp"

namespace moab 
{
    ErrorCode BoundBox::update(Interface &iface, const Range& elems)
    {
      ErrorCode rval;
      bMin = CartVect(HUGE_VAL);
      bMax = CartVect(-HUGE_VAL);
      
      CartVect coords;
      EntityHandle const *conn = NULL, *conn2 = NULL;
      int len = 0, len2 = 0;
      Range::const_iterator i;
  
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

        for (int j = 0; j < len; ++j) {
          rval = iface.get_coords( conn+j, 1, coords.array() );
          if (MB_SUCCESS != rval)
            return rval;
          update_min(coords.array());
          update_max(coords.array());
        }
      }
  
        // polyhedra
      const Range::const_iterator set_begin  = elems.lower_bound( MBENTITYSET, poly_begin );
      for (i = poly_begin; i != set_begin; ++i) {
        rval = iface.get_connectivity( *i, conn, len, true );
        if (MB_SUCCESS != rval)
          return rval;

        for (int j = 0; j < len; ++j) {
          rval = iface.get_connectivity( conn[j], conn2, len2 );
          for (int k = 0; k < len2; ++k) {
            rval = iface.get_coords( conn2+k, 1, coords.array() );
            if (MB_SUCCESS != rval)
              return rval;
            update_min(coords.array());
            update_max(coords.array());
          }
        }
      }
  
        // sets
      BoundBox box;
      for (i = set_begin; i != elems.end(); ++i) {
        Range tmp_elems;
        rval = iface.get_entities_by_handle(*i, tmp_elems);
        if (MB_SUCCESS != rval) return rval;
        rval = box.update(iface, tmp_elems);
        if (MB_SUCCESS != rval) return rval;

        update(box);
      }
  
      return MB_SUCCESS;
    }

}
