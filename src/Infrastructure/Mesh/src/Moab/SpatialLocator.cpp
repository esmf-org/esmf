#include "moab/SpatialLocator.hpp"
#include "moab/Interface.hpp"
#include "moab/ElemEvaluator.hpp"
#include "moab/AdaptiveKDTree.hpp"
#include "moab/BVHTree.hpp"

// include ScdInterface for box partitioning
#include "moab/ScdInterface.hpp"

#ifdef MOAB_HAVE_MPI
#include "moab/ParallelComm.hpp"
#endif

bool debug = false;

namespace moab 
{

    SpatialLocator::SpatialLocator(Interface *impl, Range &elems, Tree *tree, ElemEvaluator *eval) 
            : mbImpl(impl), myElems(elems), myDim(-1), myTree(tree), elemEval(eval), iCreatedTree(false),
              timerInitialized(false)
    {
      create_tree();
      
      if (!elems.empty()) {
        myDim = mbImpl->dimension_from_handle(*elems.rbegin());
        ErrorCode rval = myTree->build_tree(myElems);
        if (MB_SUCCESS != rval) throw rval;

        rval = myTree->get_bounding_box(localBox);
        if (MB_SUCCESS != rval) throw rval;
      }

      regNums[0] = regNums[1] = regNums[2] = 0;
    }

    void SpatialLocator::create_tree() 
    {
      if (myTree) return;
      
      if (myElems.empty() || mbImpl->type_from_handle(*myElems.rbegin()) == MBVERTEX) 
          // create a kdtree if only vertices
        myTree = new AdaptiveKDTree(mbImpl);
      else
          // otherwise a BVHtree, since it performs better for elements
        myTree = new BVHTree(mbImpl);

      iCreatedTree = true;
    }

    ErrorCode SpatialLocator::add_elems(Range &elems) 
    {
      if (elems.empty() ||
          mbImpl->dimension_from_handle(*elems.begin()) != mbImpl->dimension_from_handle(*elems.rbegin()))
        return MB_FAILURE;
  
      myDim = mbImpl->dimension_from_handle(*elems.begin());
      myElems = elems;

      ErrorCode rval = myTree->build_tree(myElems);
      return rval;
    }
    
#ifdef MOAB_HAVE_MPI
    ErrorCode SpatialLocator::initialize_intermediate_partition(ParallelComm *pc) 
    {
      if (!pc) return MB_FAILURE;
      
      BoundBox gbox;
      
        //step 2
        // get the global bounding box
      double sendbuffer[6];
      double rcvbuffer[6];

      localBox.get(sendbuffer); //fill sendbuffer with local box, max values in [0:2] min values in [3:5]
      sendbuffer[0] *= -1;
      sendbuffer[1] *= -1; //negate Xmin,Ymin,Zmin to get their minimum using MPI_MAX
      sendbuffer[2] *= -1; //to avoid calling MPI_Allreduce again with MPI_MIN

      int mpi_err = MPI_Allreduce(sendbuffer, rcvbuffer, 6, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
      if (MPI_SUCCESS != mpi_err)	return MB_FAILURE;

      rcvbuffer[0] *= -1;
      rcvbuffer[1] *= -1;  //negate Xmin,Ymin,Zmin again to get original values
      rcvbuffer[2] *= -1;

      globalBox.update_max(&rcvbuffer[3]); //saving values in globalBox
      globalBox.update_min(&rcvbuffer[0]);

        // compute the alternate decomposition; use ScdInterface::compute_partition_sqijk for this
      ScdParData spd;
      spd.partMethod = ScdParData::SQIJK;
      spd.gPeriodic[0] = spd.gPeriodic[1] = spd.gPeriodic[2] = 0;
      double lg = log10((localBox.bMax - localBox.bMin).length());
      double mfactor = pow(10.0, 6 - lg);
      int ldims[6], lper[3];
      double dgijk[6];
      localBox.get(dgijk);
      for (int i = 0; i < 6; i++) spd.gDims[i] = dgijk[i] * mfactor;
      ErrorCode rval = ScdInterface::compute_partition(pc->size(), pc->rank(), spd,
                                                       ldims, lper, regNums);
      if (MB_SUCCESS != rval) return rval;
        // all we're really interested in is regNums[i], #procs in each direction
      
      for (int i = 0; i < 3; i++)
        regDeltaXYZ[i] = (globalBox.bMax[i] - globalBox.bMin[i])/double(regNums[i]); //size of each region

      return MB_SUCCESS;
    }

//this function sets up the TupleList TLreg_o containing the registration messages
// and sends it
    ErrorCode SpatialLocator::register_src_with_intermediate_procs(ParallelComm *pc, double abs_iter_tol, TupleList &TLreg_o)
    {
      int corner_ijk[6];

        // step 3: compute ijks of local box corners in intermediate partition
        // get corner ijk values for my box
      ErrorCode rval = get_point_ijk(localBox.bMin-CartVect(abs_iter_tol), abs_iter_tol, corner_ijk);
      if (MB_SUCCESS != rval) return rval;
      rval = get_point_ijk(localBox.bMax+CartVect(abs_iter_tol), abs_iter_tol, corner_ijk+3);
      if (MB_SUCCESS != rval) return rval;

        //step 4
        //set up TLreg_o
      TLreg_o.initialize(1,0,0,6,0);
        // TLreg_o (int destProc, real Xmin, Ymin, Zmin, Xmax, Ymax, Zmax)

      int dest;
      double boxtosend[6];

      localBox.get(boxtosend);

        //iterate over all regions overlapping with my bounding box using the computerd corner IDs
      for (int k = corner_ijk[2]; k <= corner_ijk[5]; k++) {
        for (int j = corner_ijk[1]; j <= corner_ijk[4]; j++) {
          for (int i = corner_ijk[0]; i <= corner_ijk[3]; i++) {
            dest = k * regNums[0]*regNums[1] + j * regNums[0] + i;
            TLreg_o.push_back(&dest, NULL, NULL, boxtosend);
          }
        }
      }
	
        //step 5
        //send TLreg_o, receive TLrequests_i
      if (pc) pc->proc_config().crystal_router()->gs_transfer(1, TLreg_o, 0);

        //step 6
        //Read registration requests from TLreg_o and add to list of procs to forward to
        //get number of tuples sent to me

        //read tuples and fill processor list;
      int NN = TLreg_o.get_n();
      for (int i=0; i < NN; i++)
          //TLreg_o is now TLrequests_i
        srcProcBoxes[TLreg_o.vi_rd[i]] = BoundBox(TLreg_o.vr_rd+6*i);

      return MB_SUCCESS;
    }

    ErrorCode SpatialLocator::par_locate_points(ParallelComm */*pc*/,
                                                Range &/*vertices*/,
                                                const double /*rel_iter_tol*/, const double /*abs_iter_tol*/,
                                                const double /*inside_tol*/)
    {
      return MB_UNSUPPORTED_OPERATION;
    }

    bool is_neg(int i) {return (i == -1);}
      
    ErrorCode SpatialLocator::par_locate_points(ParallelComm *pc,
                                                const double *pos, int num_points,
                                                const double rel_iter_tol, const double abs_iter_tol,
                                                const double inside_tol)
    {
      ErrorCode rval;
        //TUpleList used for communication 
      TupleList TLreg_o;   //TLregister_outbound
      TupleList TLquery_o; //TLquery_outbound
      TupleList TLforward_o; //TLforward_outbound
      TupleList TLsearch_results_o; //TLsearch_results_outbound

        // initialize timer 
      myTimer.time_elapsed();
      timerInitialized = true;
      
        // steps 1-2 - initialize the alternative decomposition box from global box
      rval = initialize_intermediate_partition(pc);
      if (rval != MB_SUCCESS) return rval;
      
        //steps 3-6 - set up TLreg_o, gs_transfer, gather registrations
      rval = register_src_with_intermediate_procs(pc, abs_iter_tol, TLreg_o);
      if (rval != MB_SUCCESS) return rval;

      myTimes.slTimes[SpatialLocatorTimes::INTMED_INIT] = myTimer.time_elapsed();

        // actual parallel point location using intermediate partition

        // target_pts: TL(to_proc, tgt_index, x, y, z): tuples sent to source mesh procs representing pts to be located
        // source_pts: TL(from_proc, tgt_index, src_index): results of source mesh proc point location, ready to send
        //             back to tgt procs; src_index of -1 indicates point not located (arguably not useful...)

      unsigned int my_rank = (pc? pc->proc_config().proc_rank() : 0);

        //TLquery_o: Tuples sent to forwarder proc 
        //TL (toProc, OriginalSourceProc, targetIndex, X,Y,Z)

        //TLforw_req_i: Tuples to forward to corresponding procs (forwarding requests)
        //TL (sourceProc, OriginalSourceProc, targetIndex, X,Y,Z)

      TLquery_o.initialize(3,0,0,3,0);

      int iargs[3];

      for (int pnt=0; pnt < 3*num_points; pnt+=3)
      {
        int forw_id = proc_from_point(pos+pnt, abs_iter_tol); //get ID of proc resonsible of the region the proc is in

        iargs[0] = forw_id; 	//toProc
        iargs[1] = my_rank; 	//originalSourceProc
        iargs[2] = pnt/3;    	//targetIndex 	

        TLquery_o.push_back(iargs, NULL, NULL, const_cast<double*>(pos+pnt));
      }

        //send point search queries to forwarders
      if (pc)
        pc->proc_config().crystal_router()->gs_transfer(1, TLquery_o, 0);

      myTimes.slTimes[SpatialLocatorTimes::INTMED_SEND] = myTimer.time_elapsed();

        //now read forwarding requests and forward to corresponding procs
        //TLquery_o is now TLforw_req_i

        //TLforward_o: query messages forwarded to corresponding procs
        //TL (toProc, OriginalSourceProc, targetIndex, X,Y,Z)

      TLforward_o.initialize(3,0,0,3,0);

      int NN = TLquery_o.get_n();

      for (int i=0; i < NN; i++) {
        iargs[1] = TLquery_o.vi_rd[3*i+1];	//get OriginalSourceProc
        iargs[2] = TLquery_o.vi_rd[3*i+2];	//targetIndex
        CartVect tmp_pnt(TLquery_o.vr_rd+3*i);

          //compare coordinates to list of bounding boxes
        for (std::map<int, BoundBox>::iterator mit = srcProcBoxes.begin(); mit != srcProcBoxes.end(); ++mit) {
          if ((*mit).second.contains_point(tmp_pnt.array(), abs_iter_tol)) {
            iargs[0] = (*mit).first;
            TLforward_o.push_back(iargs, NULL, NULL, tmp_pnt.array());
          }
        }

      }

      myTimes.slTimes[SpatialLocatorTimes::INTMED_SEARCH] = myTimer.time_elapsed();

      if (pc)
        pc->proc_config().crystal_router()->gs_transfer(1, TLforward_o, 0);

      myTimes.slTimes[SpatialLocatorTimes::SRC_SEND] = myTimer.time_elapsed();

        // cache time here, because locate_points also calls elapsed functions and we want to account
        // for tuple list initialization here
      double tstart = myTimer.time_since_birth();
      
        //step 12
        //now read Point Search requests
        //TLforward_o is now TLsearch_req_i
        //TLsearch_req_i: (sourceProc, OriginalSourceProc, targetIndex, X,Y,Z)
							  
      NN = TLforward_o.get_n();

        //TLsearch_results_o
        //TL: (OriginalSourceProc, targetIndex, sourceIndex, U,V,W);
      TLsearch_results_o.initialize(3,0,0,0,0);

        //step 13 is done in test_local_box

      std::vector<double> params(3*NN);
      std::vector<int> is_inside(NN, 0);
      std::vector<EntityHandle> ents(NN, 0);
      
      rval = locate_points(TLforward_o.vr_rd, TLforward_o.get_n(), 
                           &ents[0], &params[0], &is_inside[0], 
                           rel_iter_tol, abs_iter_tol, inside_tol);
      if (MB_SUCCESS != rval)
        return rval;
      
      locTable.initialize(1, 0, 1, 3, 0);
      locTable.enableWriteAccess();
      for (int i = 0; i < NN; i++) {
        if (is_inside[i]) {
          iargs[0] = TLforward_o.vi_rd[3*i+1];
          iargs[1] = TLforward_o.vi_rd[3*i+2];
          iargs[2] = locTable.get_n();
          TLsearch_results_o.push_back(iargs, NULL, NULL, NULL);
          Ulong ent_ulong=(Ulong)ents[i];
          sint forward= (sint)TLforward_o.vi_rd[3*i+1];
          locTable.push_back(&forward, NULL, &ent_ulong, &params[3*i]);
        }
      }
      locTable.disableWriteAccess();

      myTimes.slTimes[SpatialLocatorTimes::SRC_SEARCH] =  myTimer.time_since_birth() - tstart;
      myTimer.time_elapsed(); // call this to reset last time called

        //step 14: send TLsearch_results_o and receive TLloc_i
      if (pc)
        pc->proc_config().crystal_router()->gs_transfer(1, TLsearch_results_o, 0);

      myTimes.slTimes[SpatialLocatorTimes::TARG_RETURN] = myTimer.time_elapsed();

        // store proc/index tuples in parLocTable
      parLocTable.initialize(2, 0, 0, 0, num_points);
      parLocTable.enableWriteAccess();
      std::fill(parLocTable.vi_wr, parLocTable.vi_wr + 2*num_points, -1);
      
      for (unsigned int i = 0; i < TLsearch_results_o.get_n(); i++) {
        int idx = TLsearch_results_o.vi_rd[3*i+1];
        parLocTable.vi_wr[2*idx] = TLsearch_results_o.vi_rd[3*i];
        parLocTable.vi_wr[2*idx+1] = TLsearch_results_o.vi_rd[3*i+2];
      }

      if (debug) {
        int num_found = num_points - 0.5 * 
            std::count_if(parLocTable.vi_wr, parLocTable.vi_wr + 2*num_points, is_neg);
        std::cout << "Points found = " << num_found << "/" << num_points 
                  << " (" << 100.0*((double)num_found/num_points) << "%)" << std::endl;
      }
      
      myTimes.slTimes[SpatialLocatorTimes::TARG_STORE] = myTimer.time_elapsed();

      return MB_SUCCESS;
    }

#endif

    ErrorCode SpatialLocator::locate_points(Range &verts,
                                            const double rel_iter_tol, const double abs_iter_tol, 
                                            const double inside_tol) 
    {
      bool i_initialized = false;
      if (!timerInitialized) {
        myTimer.time_elapsed();
        timerInitialized = true;
        i_initialized = true;
      }
      
      assert(!verts.empty() && mbImpl->type_from_handle(*verts.rbegin()) == MBVERTEX);
      std::vector<double> pos(3*verts.size());
      ErrorCode rval = mbImpl->get_coords(verts, &pos[0]);
      if (MB_SUCCESS != rval) return rval;
      rval = locate_points(&pos[0], verts.size(), rel_iter_tol, abs_iter_tol, inside_tol);
      if (MB_SUCCESS != rval) return rval;
      
        // only call this if I'm the top-level function, since it resets the last time called
      if (i_initialized) 
        myTimes.slTimes[SpatialLocatorTimes::SRC_SEARCH] =  myTimer.time_elapsed();

      return MB_SUCCESS;
    }
    
    ErrorCode SpatialLocator::locate_points(const double *pos, int num_points,
                                            const double rel_iter_tol, const double abs_iter_tol, 
                                            const double inside_tol) 
    {
      bool i_initialized = false;
      if (!timerInitialized) {
        myTimer.time_elapsed();
        timerInitialized = true;
        i_initialized = true;
      }
        // initialize to tuple structure (p_ui, hs_ul, r[3]_d) (see header comments for locTable)
      locTable.initialize(1, 0, 1, 3, num_points);
      locTable.enableWriteAccess();

        // pass storage directly into locate_points, since we know those arrays are contiguous
      ErrorCode rval = locate_points(pos, num_points, (EntityHandle*)locTable.vul_wr, locTable.vr_wr, NULL, rel_iter_tol, abs_iter_tol,
                                     inside_tol);
      std::fill(locTable.vi_wr, locTable.vi_wr+num_points, 0);
      locTable.set_n(num_points);
      if (MB_SUCCESS != rval) return rval;

      
        // only call this if I'm the top-level function, since it resets the last time called
      if (i_initialized) 
        myTimes.slTimes[SpatialLocatorTimes::SRC_SEARCH] =  myTimer.time_elapsed();

      return MB_SUCCESS;
    }
      
    ErrorCode SpatialLocator::locate_points(Range &verts,
                                            EntityHandle *ents, double *params, int *is_inside,
                                            const double rel_iter_tol, const double abs_iter_tol, 
                                            const double inside_tol)
    {
      bool i_initialized = false;
      if (!timerInitialized) {
        myTimer.time_elapsed();
        timerInitialized = true;
        i_initialized = true;
      }

      assert(!verts.empty() && mbImpl->type_from_handle(*verts.rbegin()) == MBVERTEX);
      std::vector<double> pos(3*verts.size());
      ErrorCode rval = mbImpl->get_coords(verts, &pos[0]);
      if (MB_SUCCESS != rval) return rval;
      rval = locate_points(&pos[0], verts.size(), ents, params, is_inside, rel_iter_tol, abs_iter_tol, inside_tol);

        // only call this if I'm the top-level function, since it resets the last time called
      if (i_initialized) 
        myTimes.slTimes[SpatialLocatorTimes::SRC_SEARCH] =  myTimer.time_elapsed();

      return rval;
    }

    ErrorCode SpatialLocator::locate_points(const double *pos, int num_points,
                                            EntityHandle *ents, double *params, int *is_inside,
                                            const double /* rel_iter_tol */, const double abs_iter_tol,
                                            const double inside_tol)
    {
      bool i_initialized = false;
      if (!timerInitialized) {
        myTimer.time_elapsed();
        timerInitialized = true;
        i_initialized = true;
      }

      /*
      double tmp_abs_iter_tol = abs_iter_tol;
      if (rel_iter_tol && !tmp_abs_iter_tol) {
          // relative epsilon given, translate to absolute epsilon using box dimensions
        tmp_abs_iter_tol = rel_iter_tol * localBox.diagonal_length();
      }
      */

      if (elemEval && myTree->get_eval() != elemEval)
        myTree->set_eval(elemEval);
      
      ErrorCode rval = MB_SUCCESS;
      for (int i = 0; i < num_points; i++) {
        int i3 = 3*i;
        ErrorCode tmp_rval = myTree->point_search(pos+i3, ents[i], abs_iter_tol, inside_tol, NULL, NULL, 
                                                  (CartVect*)(params+i3));
        if (MB_SUCCESS != tmp_rval) {
          rval = tmp_rval;
          continue;
        }

        if (debug && !ents[i]) {
          std::cout << "Point " << i << " not found; point: (" 
                    << pos[i3] << "," << pos[i3+1] << "," << pos[i3+2] << ")" << std::endl;
        }

        if (is_inside) is_inside[i] = (ents[i] ? true : false);
      }
      
        // only call this if I'm the top-level function, since it resets the last time called
      if (i_initialized) 
        myTimes.slTimes[SpatialLocatorTimes::SRC_SEARCH] =  myTimer.time_elapsed();

      return rval;
    }
    
        /* Count the number of located points in locTable
         * Return the number of entries in locTable that have non-zero entity handles, which
         * represents the number of points in targetEnts that were inside one element in sourceEnts
         *
         */
    int SpatialLocator::local_num_located() 
    {
      int num_located = locTable.get_n() - std::count(locTable.vul_rd, locTable.vul_rd+locTable.get_n(), 0);
      if (num_located != (int)locTable.get_n()) {
        Ulong *nl = std::find(locTable.vul_rd, locTable.vul_rd+locTable.get_n(), 0);
        if (nl) {
          int idx = nl - locTable.vul_rd;
          if (idx) {}
        }
      }
      return num_located;
    }

        /* Count the number of located points in parLocTable
         * Return the number of entries in parLocTable that have a non-negative index in on a remote
         * proc in parLocTable, which gives the number of points located in at least one element in a
         * remote proc's sourceEnts.
         */
    int SpatialLocator::remote_num_located()
    {
      int located = 0;
      for (unsigned int i = 0; i < parLocTable.get_n(); i++)
        if (parLocTable.vi_rd[2*i] != -1) located++;
      return located;
    }
} // namespace moab

