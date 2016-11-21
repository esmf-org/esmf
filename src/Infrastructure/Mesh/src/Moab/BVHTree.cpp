#include "moab/BVHTree.hpp"
#include "moab/Interface.hpp"
#include "moab/ElemEvaluator.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/CpuTimer.hpp"

namespace moab 
{
    const char *BVHTree::treeName = "BVHTree";

    ErrorCode BVHTree::build_tree(const Range& entities,
                                  EntityHandle *tree_root_set,
                                  FileOptions *options) 
    {
      ErrorCode rval;
      CpuTimer cp;

      if (options) {
        rval = parse_options(*options);
        if (MB_SUCCESS != rval) return rval;

        if (!options->all_seen()) return MB_FAILURE;
      }
      
        // calculate bounding box of elements
      BoundBox box;
      rval = box.update(*moab(), entities);
      if (MB_SUCCESS != rval)
        return rval;
  
        // create tree root
      EntityHandle tmp_root;
      if (!tree_root_set) tree_root_set = &tmp_root;
      rval = create_root( box.bMin.array(), box.bMax.array(), *tree_root_set);
      if (MB_SUCCESS != rval)
        return rval;
      rval = mbImpl->add_entities( *tree_root_set, entities );
      if (MB_SUCCESS != rval)
        return rval;
  
        //a fully balanced tree will have 2*_entities.size()
        //which is one doubling away..
      std::vector<Node> tree_nodes;
      tree_nodes.reserve(entities.size()/maxPerLeaf);
      std::vector<HandleData> handle_data_vec;
      rval = construct_element_vec(handle_data_vec, entities, boundBox);
      if (MB_SUCCESS != rval) return rval;

#ifndef NDEBUG
      for(std::vector<HandleData>::const_iterator i = handle_data_vec.begin(); i != handle_data_vec.end(); ++i) {
        if(!boundBox.intersects_box(i->myBox, 0)){
          std::cerr << "BB:" << boundBox << "EB:" << i->myBox << std::endl;
          return MB_FAILURE;
        }
      }
#endif
        //We only build nonempty trees
      if(!handle_data_vec.empty()){ 
          //initially all bits are set
        tree_nodes.push_back(Node());
        const int depth = local_build_tree(tree_nodes, handle_data_vec.begin(), handle_data_vec.end(), 0, boundBox);
#ifndef NDEBUG
        std::set<EntityHandle> entity_handles;
        for(std::vector<Node>::iterator n = tree_nodes.begin(); n != tree_nodes.end(); ++n) {
          for(HandleDataVec::const_iterator j = n->entities.begin(); j != n->entities.end(); ++j) {
            entity_handles.insert(j->myHandle);
          }			
        }
        if(entity_handles.size() != entities.size()){
          std::cout << "Entity Handle Size Mismatch!" 
                    << std::endl;
        }
        for(Range::iterator i = entities.begin(); i != entities.end(); ++i) {
          if (entity_handles.find(*i) == entity_handles.end())
            std::cout << "Tree is missing an entity! " << std::endl;
        } 
#endif
        treeDepth = std::max(depth, treeDepth);
      }

        // convert vector of Node's to entity sets and vector of TreeNode's
      rval = convert_tree(tree_nodes);
      if (MB_SUCCESS != rval) return rval;

      treeStats.reset();
      rval = treeStats.compute_stats(mbImpl, startSetHandle);
      treeStats.initTime = cp.time_elapsed();
      
      return rval;
    }

    ErrorCode BVHTree::convert_tree(std::vector<Node> &tree_nodes) 
    {
        // first construct the proper number of entity sets
      ReadUtilIface *read_util;
      ErrorCode rval = mbImpl->query_interface(read_util);
      if (MB_SUCCESS != rval) return rval;

      {// isolate potentially-large std::vector so it gets deleted earlier
        std::vector<unsigned int> tmp_flags(tree_nodes.size(), meshsetFlags);
        rval = read_util->create_entity_sets(tree_nodes.size(), &tmp_flags[0], 0, startSetHandle);
        if (MB_SUCCESS != rval) return rval;
        rval = mbImpl->release_interface(read_util);
        if (MB_SUCCESS != rval) return rval;
      }
      
        // populate the sets and the TreeNode vector
      EntityHandle set_handle = startSetHandle;
      std::vector<Node>::iterator it;
      myTree.reserve(tree_nodes.size());
      for (it = tree_nodes.begin(); it != tree_nodes.end(); ++it, set_handle++) {
        if (it != tree_nodes.begin() && !it->entities.empty()) {
          Range range;
          for (HandleDataVec::iterator hit = it->entities.begin(); hit != it->entities.end(); ++hit)
            range.insert(hit->myHandle);
          rval = mbImpl->add_entities(set_handle, range);
          if (MB_SUCCESS != rval) return rval;
        }
        myTree.push_back(TreeNode(it->dim, it->child, it->Lmax, it->Rmin, it->box));

        if (it->dim != 3) {
          rval = mbImpl->add_child_meshset(set_handle, startSetHandle+it->child);
          if (MB_SUCCESS != rval) return rval;
          rval = mbImpl->add_child_meshset(set_handle, startSetHandle+it->child+1);
          if (MB_SUCCESS != rval) return rval;
        }
      }

      return MB_SUCCESS;
    }

    ErrorCode BVHTree::parse_options(FileOptions &opts) 
    {
      ErrorCode rval = parse_common_options(opts);
      if (MB_SUCCESS != rval) return rval;
      
        //  SPLITS_PER_DIR: number of candidate splits considered per direction; default = 3
      int tmp_int;
      rval = opts.get_int_option("SPLITS_PER_DIR", tmp_int);
      if (MB_SUCCESS == rval) splitsPerDir = tmp_int;

      return MB_SUCCESS;
    }
    
    void BVHTree::establish_buckets(HandleDataVec::const_iterator begin, 
                                    HandleDataVec::const_iterator end, 
                                    const BoundBox &interval, std::vector<std::vector<Bucket> > &buckets) const 
    {
        //put each element into its bucket
      for(HandleDataVec::const_iterator i = begin; i != end; ++i){
        const BoundBox &box = i->myBox;
        for (unsigned int dim = 0; dim < 3; ++dim){
          const unsigned int index = Bucket::bucket_index(splitsPerDir, box, interval, dim);
          assert(index < buckets[dim].size());
          Bucket &bucket = buckets[dim][index];
          if (bucket.mySize > 0)
            bucket.boundingBox.update(box);
          else
            bucket.boundingBox = box; 
          bucket.mySize++;
        }
      }

#ifndef NDEBUG
      BoundBox elt_union = begin->myBox;
      for(HandleDataVec::const_iterator i = begin; i != end; ++i){
        const BoundBox &box = i->myBox;
        elt_union.update(box);
        for (unsigned int dim = 0; dim < 3; ++dim){
          const unsigned int index = Bucket::bucket_index(splitsPerDir, box, interval, dim);
          Bucket &bucket = buckets[dim][index];
          if(!bucket.boundingBox.intersects_box(box))
            std::cerr << "Buckets not covering elements!" << std::endl;
        }
      }
      if(!elt_union.intersects_box(interval) ){
        std::cout << "element union: " << std::endl << elt_union; 
        std::cout << "intervals: " << std::endl << interval;
        std::cout << "union of elts does not contain original box!" << std::endl;
      }
      if (!interval.intersects_box(elt_union) ){
        std::cout << "original box does not contain union of elts" << std::endl;
        std::cout << interval << std::endl << elt_union << std::endl;
      }
      for(unsigned int d = 0; d < 3; ++d){
        std::vector<unsigned int> nonempty;
        const std::vector<Bucket> &buckets_ = buckets[d];
        unsigned int j = 0;
        for( std::vector<Bucket>::const_iterator i = buckets_.begin(); 
             i != buckets_.end(); ++i, ++j){
          if(i->mySize > 0){ nonempty.push_back(j); }
        }
        BoundBox test_box = buckets_[nonempty.front()].boundingBox;
        for(unsigned int i = 0; i < nonempty.size(); ++i)
          test_box.update(buckets_[nonempty[i]].boundingBox);

        if(!test_box.intersects_box(interval) )
          std::cout << "union of buckets in dimension: " << d << "does not contain original box!" << std::endl;
        if (!interval.intersects_box(test_box) ) {
          std::cout << "original box does " << "not contain union of buckets" 
                    << "in dimension: " << d << std::endl;
          std::cout << interval << std::endl << test_box << std::endl;
        }
      }
#endif
    }

    void BVHTree::initialize_splits(std::vector<std::vector<SplitData> > &splits, 
                                    const std::vector<std::vector<Bucket> > &buckets, 
                                    const SplitData &data) const {
      for(unsigned int d = 0; d < 3; ++d){
        std::vector<SplitData>::iterator splits_begin = splits[d].begin();
        std::vector<SplitData>::iterator splits_end = splits[d].end();
        std::vector<Bucket>::const_iterator left_begin = buckets[d].begin();
        std::vector<Bucket>::const_iterator _end = buckets[d].end();
        std::vector<Bucket>::const_iterator left_end = buckets[d].begin()+1;
        for(std::vector<SplitData>::iterator s = splits_begin; s != splits_end; ++s, ++left_end) {
          s->nl = set_interval(s->leftBox, left_begin, left_end);
          if(s->nl == 0) { 
            s->leftBox = data.boundingBox;
            s->leftBox.bMax[d] = s->leftBox.bMin[d];
          }
          s->nr = set_interval(s->rightBox, left_end,  _end);
          if(s->nr == 0) { 
            s->rightBox = data.boundingBox;
            s->rightBox.bMin[d] = s->rightBox.bMax[d];
          }
          s->Lmax = s->leftBox.bMax[d];
          s->Rmin = s->rightBox.bMin[d];
          s->split = std::distance(splits_begin, s);
          s->dim = d;
        }
#ifndef NDEBUG
        for(std::vector<SplitData>::iterator s = splits_begin; 
            s != splits_end; ++s) {
          BoundBox test_box = s->leftBox;
          test_box.update(s->rightBox);
          if(!data.boundingBox.intersects_box(test_box)) {
            std::cout << "nr: " << s->nr << std::endl;
            std::cout << "Test box: " << std::endl << 
                test_box;
            std::cout << "Left box: " << std::endl << 
                s->leftBox;
            std::cout << "Right box: " << std::endl << 
                s->rightBox;
            std::cout << "Interval: " << std::endl << 
                data.boundingBox;
            std::cout << "Split boxes larger than bb" 
                      << std::endl;
          }
          if(!test_box.intersects_box(data.boundingBox)) {
            std::cout << "bb larger than union of split boxes" << std::endl;
          }         	
        }
#endif 
      }
    }

    void BVHTree::median_order(HandleDataVec::iterator &begin, 
                               HandleDataVec::iterator &end, 
                               SplitData &data) const
    {
      int dim = data.dim;
      for(HandleDataVec::iterator i = begin; i != end; ++i) {
        i->myDim = 
            0.5 * (i->myBox.bMin[dim], i->myBox.bMax[dim]);
      }
      std::sort(begin, end, BVHTree::HandleData_comparator());
      const unsigned int total = std::distance(begin, end);
      HandleDataVec::iterator middle = begin+(total/2);
      double middle_center = middle->myDim;
      middle_center += (++middle)->myDim;
      middle_center *= 0.5;
      data.split = middle_center;
      data.nl = std::distance(begin, middle)+1;
      data.nr = total-data.nl;
      ++middle;
      data.leftBox  = begin->myBox;
      data.rightBox = middle->myBox;
      for(HandleDataVec::iterator i = begin; i != middle; ++i){
        i->myDim = 0;
        data.leftBox.update(i->myBox);
      }
      for(HandleDataVec::iterator i = middle; i != end; ++i){
        i->myDim = 1;
        data.rightBox.update(i->myBox);
      }
      data.Rmin = data.rightBox.bMin[data.dim];
      data.Lmax = data.leftBox.bMax[data.dim];
#ifndef NDEBUG
      BoundBox test_box(data.rightBox);
      if(!data.boundingBox.intersects_box(test_box)) {
        std::cerr << "MEDIAN: BB Does not contain splits" << std::endl;
        std::cerr << "test_box:         " << test_box << std::endl;
        std::cerr << "data.boundingBox: " << data.boundingBox << std::endl;
      }
#endif
    }

    void BVHTree::find_split(HandleDataVec::iterator &begin, 
                             HandleDataVec::iterator &end,
                             SplitData &data) const
    {
      std::vector<std::vector<Bucket> > buckets(3, std::vector<Bucket>(splitsPerDir+1) );
      std::vector<std::vector<SplitData> > splits(3, std::vector<SplitData>(splitsPerDir, data));
	
      const BoundBox interval = data.boundingBox;
      establish_buckets(begin, end, interval, buckets);
      initialize_splits(splits, buckets, data);
      choose_best_split(splits, data);
      const bool use_median = (0 == data.nl) || (data.nr == 0);
      if (!use_median)
        order_elements(begin, end, data);
      else
        median_order(begin, end, data);

#ifndef NDEBUG
      bool seen_one=false,issue=false;
      bool first_left=true,first_right=true;
      unsigned int count_left=0, count_right=0;
      BoundBox left_box, right_box;
      for(HandleDataVec::iterator i = begin; i != end; ++i){
        int order = i->myDim;
        if(order != 0 && order != 1) {
          std::cerr << "Invalid order element !";
          std::cerr << order << std::endl;
          std::exit(-1);
        }
        if(order == 1) {
          seen_one=1;
          count_right++;
          if(first_right) {
            right_box = i->myBox;
            first_right=false;
          }
          else {
            right_box.update(i->myBox);
          }
          if(!right_box.intersects_box(i->myBox)) {
            if(!issue) {
              std::cerr << "Bounding right box issue!" 
                        << std::endl;
            }
            issue=true;
          }
        }
        if(order==0) {
          count_left++;
          if(first_left) {
            left_box = i->myBox;
            first_left=false;
          }
          else {
            left_box.update(i->myBox);
          }
          if(!data.leftBox.intersects_box(i->myBox)) {
            if(!issue) {
              std::cerr << "Bounding left box issue!" 
                        << std::endl;
            }
            issue=true;
          }
          if(seen_one) {
            std::cerr << "Invalid ordering!" << std::endl;
            std::cout << (i-1)->myDim 
                      << order << std::endl;
            exit(-1);
          }
        }
      }
      if(!left_box.intersects_box(data.leftBox)) 
        std::cout << "left elts do not contain left box" << std::endl;
      if(!data.leftBox.intersects_box(left_box)) 
        std::cout << "left box does not contain left elts" << std::endl;
      if(!right_box.intersects_box(data.rightBox))
        std::cout << "right elts do not contain right box" << std::endl;
      if(!data.rightBox.intersects_box(right_box))
        std::cout << "right box do not contain right elts" << std::endl;

      if(count_left != data.nl || count_right != data.nr) {
        std::cerr << "counts are off!" << std::endl;
        std::cerr << "total: " 
                  << std::distance(begin, end) << std::endl;
        std::cerr << "Dim: " << data.dim << std::endl;
        std::cerr << data.Lmax << " , " << data.Rmin << std::endl;
        std::cerr << "Right box: " << std::endl << data.rightBox 
                  << "Left box: " << std::endl << data.leftBox ;
        std::cerr << "supposed to be: " << 
            data.nl << " " << data.nr << std::endl;
        std::cerr << "accountant says: " << 
            count_left << " " << count_right << std::endl;
        std::exit(-1);
      }
#endif
    }

    int BVHTree::local_build_tree(std::vector<Node> &tree_nodes,
                                  HandleDataVec::iterator begin, 
                                  HandleDataVec::iterator end,
                                  const int index, const BoundBox &box, const int depth)
    {
#ifndef NDEBUG
      for(HandleDataVec::const_iterator i = begin; i != end; ++i) {
        if(!box.intersects_box(i->myBox, 0)) {
          std::cerr << "depth: " << depth << std::endl;
          std::cerr << "BB:" << box << "EB:" << i->myBox << std::endl;
          std::exit(-1);
        }
      }
#endif

      const unsigned int total_num_elements = std::distance(begin, end);
      tree_nodes[index].box = box;
      
        //logic for splitting conditions
      if((int)total_num_elements > maxPerLeaf && depth < maxDepth){
        SplitData data;
        data.boundingBox = box;
        find_split(begin, end, data);
          //assign data to node
        tree_nodes[index].Lmax = data.Lmax; tree_nodes[index].Rmin = data.Rmin;
        tree_nodes[index].dim = data.dim; tree_nodes[index].child = tree_nodes.size();
          //insert left, right children;
        tree_nodes.push_back(Node()); tree_nodes.push_back(Node());
        const int left_depth = local_build_tree(tree_nodes, begin, begin+data.nl, tree_nodes[index].child, 
                                                data.leftBox, depth+1);
        const int right_depth = local_build_tree(tree_nodes, begin+data.nl, end, tree_nodes[index].child+1, 
                                                 data.rightBox, depth+1);
        return std::max(left_depth, right_depth);
      }

      tree_nodes[index].dim = 3;
      std::copy(begin, end, std::back_inserter(tree_nodes[index].entities));
      return depth;
    }

    ErrorCode BVHTree::find_point(const std::vector<double> &point, 
                                  const unsigned int &index,
                                  const double iter_tol,
                                  const double inside_tol,
                                  std::pair<EntityHandle, CartVect> &result)
    {
      if (index == 0) treeStats.numTraversals++;
      const TreeNode &node = myTree[index];
      treeStats.nodesVisited++;
      CartVect params;
      int is_inside;
      ErrorCode rval = MB_SUCCESS;
      if(node.dim == 3){
        treeStats.leavesVisited++;
        Range entities;
        rval = mbImpl->get_entities_by_handle(startSetHandle+index, entities);
        if (MB_SUCCESS != rval) return rval;
        
        for(Range::iterator i = entities.begin(); i != entities.end(); ++i) {
          treeStats.traversalLeafObjectTests++;
          myEval->set_ent_handle(*i);
          myEval->reverse_eval(&point[0], iter_tol, inside_tol, params.array(), &is_inside);
          if (is_inside) {
            result.first = *i;
            result.second = params;
            return MB_SUCCESS;
          }
        }
        result.first = 0;
        return MB_SUCCESS;
      }
        //the extra tol here considers the case where
        //0 < Rmin - Lmax < 2tol
      std::vector<EntityHandle> children;
      rval = mbImpl->get_child_meshsets(startSetHandle+index, children);
      if (MB_SUCCESS != rval || children.size() != 2) return rval;
      
      if((node.Lmax+iter_tol) < (node.Rmin-iter_tol)){
        if(point[node.dim] <= (node.Lmax + iter_tol))
          return find_point(point, children[0]-startSetHandle, iter_tol, inside_tol, result);
        else if(point[ node.dim] >= (node.Rmin - iter_tol))
          return find_point(point, children[1]-startSetHandle, iter_tol, inside_tol, result);
        result = std::make_pair(0, CartVect(&point[0])); //point lies in empty space.
        return MB_SUCCESS;
      }

        //Boxes overlap
        //left of Rmin, you must be on the left
        //we can't be sure about the boundaries since the boxes overlap
        //this was a typo in the paper which caused pain.
      if(point[node.dim] < (node.Rmin - iter_tol))
        return find_point(point, children[0]-startSetHandle, iter_tol, inside_tol, result);
        //if you are on the right Lmax, you must be on the right
      else if(point[ node.dim] > (node.Lmax+iter_tol))
        return find_point(point, children[1]-startSetHandle, iter_tol, inside_tol, result);

        /* pg5 of paper
         * However, instead of always traversing either subtree
         * first (e.g. left always before right), we first traverse 
         * the subtree whose bounding plane has the larger distance to the 
         * sought point. This results in less overall traversal, and the correct
         * cell is identified more quickly.
         */
        //So far all testing confirms that this 'heuristic' is
        //significantly slower.
        //I conjecture this is because it gets improperly
        //branch predicted..
        //bool dir = (point[ node.dim] - node.Rmin) <= 
        //				(node.Lmax - point[ node.dim]);
      find_point(point, children[0]-startSetHandle, iter_tol, inside_tol, result);
      if(result.first == 0){ 
        return find_point(point, children[1]-startSetHandle, iter_tol, inside_tol, result);
      }
      return MB_SUCCESS;
    }

    EntityHandle BVHTree::bruteforce_find(const double *point, const double iter_tol, const double inside_tol) {
      treeStats.numTraversals++;
      CartVect params;
      for(unsigned int i = 0; i < myTree.size(); i++) {
        if(myTree[i].dim != 3 || !myTree[i].box.contains_point(point, iter_tol)) continue;
        if (myEval) {
          EntityHandle entity = 0;
          treeStats.leavesVisited++;
          ErrorCode rval = myEval->find_containing_entity(startSetHandle+i, point, iter_tol, inside_tol,
                                                          entity, params.array(), &treeStats.traversalLeafObjectTests);
          if (entity) return entity;
          else if (MB_SUCCESS != rval) return 0;
        }
        else return startSetHandle+i;
      }
      return 0;
    }

    ErrorCode BVHTree::get_bounding_box(BoundBox &box, EntityHandle *tree_node) const 
    {
      if (!tree_node || *tree_node == startSetHandle) {
        box = boundBox;
        return MB_SUCCESS;
      }
      else if ((tree_node && !startSetHandle) ||
               *tree_node < startSetHandle || *tree_node - startSetHandle > myTree.size()) 
        return MB_FAILURE;

      box = myTree[*tree_node - startSetHandle].box;
      return MB_SUCCESS;
    }

    ErrorCode BVHTree::point_search(const double *point,
                                    EntityHandle& leaf_out,
                                    const double iter_tol,
                                    const double inside_tol,
                                    bool *multiple_leaves,
                                    EntityHandle *start_node,
                                    CartVect *params) 
    {
      treeStats.numTraversals++;

      EntityHandle this_set = (start_node ? *start_node : startSetHandle);
        // convoluted check because the root is different from startSetHandle
      if (this_set != myRoot &&
          (this_set < startSetHandle || this_set >= startSetHandle+myTree.size())) 
        return MB_FAILURE;
      else if (this_set == myRoot) this_set = startSetHandle;
      
      std::vector<EntityHandle> candidates, result_list;     // list of subtrees to traverse, and results 
      candidates.push_back(this_set-startSetHandle);
  
      BoundBox box;
      while( !candidates.empty() ) {
        EntityHandle ind = candidates.back();
        treeStats.nodesVisited++;
        if (myTree[ind].dim == 3) treeStats.leavesVisited++;
        this_set = startSetHandle + ind;
        candidates.pop_back();

          // test box of this node
        ErrorCode rval = get_bounding_box(box, &this_set);
        if (MB_SUCCESS != rval) return rval;
        if (!box.contains_point(point, iter_tol)) continue;

          // else if not a leaf, test children & put on list
        else if (myTree[ind].dim != 3) {
          candidates.push_back(myTree[ind].child);
          candidates.push_back(myTree[ind].child+1);
          continue;
        }
        else if (myTree[ind].dim == 3 && myEval && params) {
          rval = myEval->find_containing_entity(startSetHandle+ind, point, iter_tol, inside_tol,
                                                leaf_out, params->array(), &treeStats.traversalLeafObjectTests);
          if (leaf_out || MB_SUCCESS != rval) return rval;
        }
        else {
            // leaf node within distance; return in list
          result_list.push_back(this_set);
        }
      }

      if (!result_list.empty()) leaf_out = result_list[0];
      if (multiple_leaves && result_list.size() > 1) *multiple_leaves = true;
      return MB_SUCCESS;
    }

    ErrorCode BVHTree::distance_search(const double from_point[3],
                                       const double distance,
                                       std::vector<EntityHandle>& result_list,
                                       const double iter_tol,
                                       const double inside_tol,
                                       std::vector<double> *result_dists,
                                       std::vector<CartVect> *result_params,
                                       EntityHandle *tree_root)
    {
        // non-NULL root should be in tree
        // convoluted check because the root is different from startSetHandle
      EntityHandle this_set = (tree_root ? *tree_root : startSetHandle);
      if (this_set != myRoot &&
          (this_set < startSetHandle || this_set >= startSetHandle+myTree.size())) 
        return MB_FAILURE;
      else if (this_set == myRoot) this_set = startSetHandle;

      treeStats.numTraversals++;

      const double dist_sqr = distance * distance;
      const CartVect from(from_point);
      std::vector<EntityHandle> candidates; // list of subtrees to traverse
        // pre-allocate space for default max tree depth
      candidates.reserve(maxDepth );

        // misc temporary values
      ErrorCode rval;
      BoundBox box;
  
      candidates.push_back(this_set-startSetHandle);
  
      while( !candidates.empty() ) {

        EntityHandle ind = candidates.back();
        this_set = startSetHandle + ind;
        candidates.pop_back();
        treeStats.nodesVisited++;
        if (myTree[ind].dim == 3) treeStats.leavesVisited++;

          // test box of this node
        rval = get_bounding_box(box, &this_set);
        if (MB_SUCCESS != rval) return rval;
        double d_sqr = box.distance_squared(from_point);

          // if greater than cutoff, continue
        if (d_sqr > dist_sqr) continue;

          // else if not a leaf, test children & put on list
        else if (myTree[ind].dim != 3) {
          candidates.push_back(myTree[ind].child);
          candidates.push_back(myTree[ind].child+1);
          continue;
        }

        if (myEval && result_params) {
          EntityHandle ent;
          CartVect params;
          rval = myEval->find_containing_entity(startSetHandle+ind, from_point, iter_tol, inside_tol,
                                                ent, params.array(), &treeStats.traversalLeafObjectTests);
          if (MB_SUCCESS != rval) return rval;
          else if (ent) {
            result_list.push_back(ent);
            result_params->push_back(params);
            if (result_dists) result_dists->push_back(0.0);
          }
        }
        else {
            // leaf node within distance; return in list
          result_list.push_back(this_set);
          if (result_dists) result_dists->push_back(sqrt(d_sqr));
        }
      }
      
      return MB_SUCCESS;
    }

    ErrorCode BVHTree::print_nodes(std::vector<Node> &nodes) 
    {
      int i;
      std::vector<Node>::iterator it;
      for (it = nodes.begin(), i = 0; it != nodes.end(); ++it, i++) {
        std::cout << "Node " << i << ": dim = " << it->dim << ", child = " << it->child << ", Lmax/Rmin = "
                  << it->Lmax << "/" << it->Rmin << ", box = " << it->box << std::endl;
      }
      return MB_SUCCESS;
    }
      
    ErrorCode BVHTree::print()
    {
      int i;
      std::vector<TreeNode>::iterator it;
      for (it = myTree.begin(), i = 0; it != myTree.end(); ++it, i++) {
        std::cout << "Node " << i << ": dim = " << it->dim << ", child = " << it->child << ", Lmax/Rmin = "
                  << it->Lmax << "/" << it->Rmin << ", box = " << it->box << std::endl;
      }
      return MB_SUCCESS;
    }
      
      
} // namespace moab

