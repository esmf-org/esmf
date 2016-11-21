#ifndef INIT_TREE_HPP
#define INIT_TREE_HPP
//TODO: refactor out to AdaptiveKDTree
ErrorCode initialize_tree(){
  Range local_ents;
  typename Tree::Settings settings;
  settings.candidatePlaneSet = AdaptiveKDTree::SUBDIVISION;

    //get entities on the local part
  //TODO: fixme
  ErrorCode result = MB_SUCCESS;
  result = pc.get_part_entities( local_ents, 3);
  else local_ents = range;

  if (MB_SUCCESS != result || local_ents.empty()) {
    std::cout << "Problems getting source entities" 
	      << std::endl;
    return result;
  }

    // build the tree for local processor
  for (int i = 0; i < num_iterations; i++) {
    tree = Tree( &impl);
    result = tree.build_tree( local_ents, local_root, &settings);
    if (MB_SUCCESS != result) {
      std::cout << "Problems building tree";
      if (num_iterations != i) {
     	settings.maxEntPerLeaf *= 2;
        std::cout << "; increasing elements/leaf to " 
                  << settings.maxEntPerLeaf << std::endl;;
      }
      else {
        std::cout << "; exiting" << std::endl;
        return result;
      }
    }
    else
      break; // get out of tree building
  }
  
  boxes.resize(6*pc.proc_config().proc_size());

  unsigned int rank = (pc ? pc.proc_config().proc_rank() : 0);
  result = tree.get_tree_box( local_root, 
		  		 &boxes[6*rank], 
				 &boxes[6*rank+3]);
  if (MB_SUCCESS != result) return result;
  
    // now communicate to get all boxes
    // use "in place" option
  if (pc) {
    int mpi_err = MPI_Allgather(MPI_IN_PLACE, 0, MPI_DATATYPE_NULL,
                                &boxes[0], 6, MPI_DOUBLE, 
                                pc.proc_config().proc_comm());
    if (MPI_SUCCESS != mpi_err) return MB_FAILURE;
  }


#ifndef NDEBUG
  double min[3] = {0,0,0}, max[3] = {0,0,0};
  unsigned int dep;
  tree.get_info(local_root, min, max, dep);
  std::cerr << "Proc " << rank << ": box min/max, tree depth = ("
            << min[0] << "," << min[1] << "," << min[2] << "), ("
            << max[0] << "," << max[1] << "," << max[2] << "), "
            << dep << std::endl;
#endif  

  return result;
}
#endif // INIT_TREE_HPP
