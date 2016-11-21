#include "moab/Tree.hpp"
#include "moab/Range.hpp"
#include "moab/Interface.hpp"

#include <limits>

namespace moab 
{
    ErrorCode Tree::parse_common_options(FileOptions &options) 
    {
      double tmp_dbl;
      int tmp_int;
        // MAX_PER_LEAF: max entities per leaf; default = 6
      ErrorCode rval = options.get_int_option("MAX_PER_LEAF", tmp_int);
      if (MB_SUCCESS == rval) maxPerLeaf = std::max(tmp_int, 1);
      
        // MAX_DEPTH: max depth of the tree; default = 30
      rval = options.get_int_option("MAX_DEPTH", tmp_int);
      if (MB_SUCCESS == rval) maxDepth = tmp_int;
      if (maxDepth < 1) maxDepth = std::numeric_limits<unsigned>::max();

        // MIN_WIDTH: minimum width of box, used like a tolerance; default = 1.0e-10
      rval = options.get_real_option("MIN_WIDTH", tmp_dbl);
      if (MB_SUCCESS == rval) minWidth = tmp_dbl;

        // MESHSET_FLAGS: flags passed into meshset creation for tree nodes; should be a value from
        //          ENTITY_SET_PROPERTY (see Types.hpp); default = MESHSET_SET
      rval = options.get_int_option("MESHSET_FLAGS", tmp_int);
      if (MB_SUCCESS == rval && 0 <= tmp_int) meshsetFlags = (unsigned) tmp_int;
      else if (0 > tmp_int) return MB_FAILURE;

        // CLEAN_UP: if false, do not delete tree sets upon tree class destruction; default = true
      bool tmp_bool;
      rval = options.get_toggle_option("CLEAN_UP", true, tmp_bool);
      if (MB_SUCCESS == rval && !tmp_bool) cleanUp = false;

        // TAG_NAME: tag name to store tree information on tree nodes; default = "AKDTree"
      std::string tmp_str;
      rval = options.get_str_option("TAG_NAME", tmp_str);
      if (MB_SUCCESS == rval) boxTagName = tmp_str;

      return MB_SUCCESS;
    }

    ErrorCode Tree::find_all_trees( Range& results )
    {
      Tag tag = get_box_tag();
      ErrorCode rval = moab()->get_entities_by_type_and_tag( 0, MBENTITYSET, &tag, 0, 1, results );
      if (MB_SUCCESS != rval || results.empty()) return rval;
      std::vector<BoundBox> boxes(results.size());
      rval = moab()->tag_get_data(tag, results, &boxes[0]);
      if (MB_SUCCESS != rval) return rval;
      for (std::vector<BoundBox>::iterator vit = boxes.begin(); vit != boxes.end(); ++vit)
        boundBox.update(*vit);

      if (results.size() == 1) myRoot = *results.begin();
      
      return MB_SUCCESS;
    }

    ErrorCode Tree::create_root( const double box_min[3],
                                 const double box_max[3],
                                 EntityHandle& root_handle )
    {
      ErrorCode rval = mbImpl->create_meshset( meshsetFlags, root_handle );
      if (MB_SUCCESS != rval)
        return rval;

      myRoot = root_handle;
      
      double box_tag[6];
      for (int i = 0; i < 3; i++) {
        box_tag[i] = box_min[i];
        box_tag[3+i] = box_max[i];
      }
      rval = mbImpl->tag_set_data(get_box_tag(), &root_handle, 1, box_tag);
      if (MB_SUCCESS != rval)
        return rval;

      boundBox.bMin = box_min;
      boundBox.bMax = box_max;
      
      return MB_SUCCESS;
    }

    ErrorCode Tree::delete_tree_sets() 
    {
      if (!myRoot) return MB_SUCCESS;
      
      ErrorCode rval;
      std::vector<EntityHandle> children, dead_sets, current_sets;
      current_sets.push_back(myRoot);
      while (!current_sets.empty()) {
        EntityHandle set = current_sets.back();
        current_sets.pop_back();
        dead_sets.push_back( set );
        rval = mbImpl->get_child_meshsets( set, children );
        if (MB_SUCCESS != rval)
          return rval;
        std::copy( children.begin(), children.end(), std::back_inserter(current_sets) );
        children.clear();
      }
  
      rval = mbImpl->tag_delete_data( boxTag, &myRoot, 1 );
      if (MB_SUCCESS != rval)
        return rval;
  
      rval = mbImpl->delete_entities( &dead_sets[0], dead_sets.size() );
      if (MB_SUCCESS != rval) return rval;

      myRoot = 0;

      return MB_SUCCESS;
    }
    
}
