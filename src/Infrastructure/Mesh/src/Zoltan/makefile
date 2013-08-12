# $Id$

ALL: build_here 

# Workaround an MPICH(2) issue
ESMF_CXXCOMPILECPPFLAGS += -DMPICH_IGNORE_CXX_SEEK

SOURCEC	  = all_allo.c assign_param_vals.c bind_param.c box_assign.c \
            build_graph.c build_machine_desc.c ch_dist_graph.c check_param.c \
            ch_init_dist.c ch_input_assign.c ch_input_geom.c ch_input_graph.c ch_input_read.c \
            coloring.c color_test.c comm_create.c comm_destroy.c comm_do.c comm_do_reverse.c \
            comm_exchange_sizes.c comm_info.c comm_invert_map.c comm_invert_plan.c comm_resize.c \
            comm_sort_ints.c costs.c create_proc_list.c DD_Create.c DD_Destroy.c DD_Find.c \
            DD_Hash2.c DD_Print.c DD_Remove.c DD_Set_Hash_Fn.c DD_Set_Neighbor_Hash_Fn1.c \
            DD_Set_Neighbor_Hash_Fn2.c DD_Set_Neighbor_Hash_Fn3.c DD_Stats.c DD_Update.c dfs.c \
            divide_machine.c dr_chaco_io.c dr_dd.c dr_elem.c dr_elem_util.c dr_err.c dr_eval.c \
            dr_exoII_io.c dr_gnuplot.c dr_input.c \
            dr_loadbal.c dr_maps.c dr_migrate.c dr_mmio.c dr_mm_readfile.c dr_output.c dr_par_util.c \
            dr_random_io.c dr_util.c exmesh.c exmisc.c exphg.c free_params.c g2l_hash.c get_processor_name.c \
            hsfc_box_assign.c hsfc.c hsfc_hilbert.c hsfc_point_assign.c inertial1d.c inertial2d.c \
            inertial3d.c key_params.c lb_balance.c lb_box_assign.c lb_copy.c lb_eval.c lb_free.c \
            lb_init.c lb_invert.c lb_migrate.c lb_part2proc.c lb_point_assign.c lb_remap.c \
            lb_set_fn.c lb_set_method.c lb_set_part_sizes.c migoct.c migreg.c migtags.c mpistring.c \
            msg.c octant.c oct_plot.c octree.c octupdate.c oct_util.c order.c order_struct.c \
            output.c par_average.c par_bisect.c par_median.c parmetis_jostle.c par_stats.c \
            par_sync.c par_tflops_special.c perm.c phg_build.c phg_build_calls.c phg.c phg_coarse.c \
            phg_comm.c phg_distrib.c phg_gather.c phg_hypergraph.c phg_match.c phg_order.c \
            phg_parkway.c phg_patoh.c phg_plot.c phg_rdivide.c phg_refinement.c phg_scale.c \
            phg_serialpartition.c phg_util.c phg_Vcycle.c point_assign.c print_params.c queries1.c \
            rcb_box.c rcb.c rcb_util.c reftree_build.c reftree_coarse_path.c reftree_hash.c \
            reftree_part.c rib.c rib_util.c rootlist.c scatter_graph.c set_param.c shared.c \
            timer.c timer_params.c verify_graph.c zoltan_align.c zoltan_id.c zoltan_timer.c \
            zz_coord.c zz_gen_files.c zz_hash.c zz_heap.c zz_init.c zz_obj_list.c zz_rand.c \
            zz_set_fn.c zz_sort.c zz_struct.c zz_util.c mem.c


SOURCEF	  = 
SOURCEH	  = all_allo_const.h BaseLB.h ch_init_dist_const.h ch_input_const.h coloring_const.h \
	    coloring.h comm.h CONV_NS.h costs_const.h costs.h create_proc_list_const.h \
	    DataLocationPort.h DataMigrationPort.h DD.h dfs_const.h dfs.h \
	    DistDataDirectoryPort.h dr_const.h dr_dd.h dr_elem_const.h dr_elem_util_const.h \
	    dr_err_const.h dr_eval_const.h dr_hg_readfile.h dr_input_const.h Driver.h \
	    dr_loadbal_const.h dr_maps_const.h dr_mmio.h dr_output_const.h dr_par_util_const.h \
	    dr_util_const.h EdgeInfoPort.h EntityInfoPort.h EntityList.h EntityListImpl.h \
	    Epetra_ZoltanMpiDistributor.h exzoltan.h g2l_hash.h GeomInfoPort.h ha_const.h \
	    hsfc_const.h hsfc.h hsfc_hilbert_const.h HSFC_upper.h IOPort.h key_params.h \
	    lb_const.h lbi_const.h lb_init_const.h LoadBalancerFactoryPort.h LoadBalancer.h \
	    matrix.h Mesh.h migoct_const.h migreg_const.h migreg.h migtags_const.h msg_const.h \
	    octant_const.h octree_const.h OctTree.h octupdate_const.h octupdate.h \
	    oct_util_const.h order_const.h params_const.h par_average_const.h \
	    par_bisect_const.h par_const.h par_median_const.h ParMetis.h \
	    parmetis_jostle_const.h parmetis_jostle.h par_tflops_special_const.h \
	    PartitionerFactory.h phg_comm.h phg_const.h phg_distrib.h phg.h phg_hypergraph.h \
	    phg_util.h rcb_const.h rcb.h RCB_upper.h reftree_const.h reftree.h rib_const.h \
	    rib.h RIB_upper.h rootlist_const.h SFC.h shared.h timer_const.h timer.h \
	    TreeInfoPort.h zoltan_align.h zoltan_comm_cpp.h zoltan_comm.h zoltan_cpp.h \
	    zoltan_dd_cpp.h zoltan_dd.h zoltan.h zoltan_id.h zoltan_mem.h zoltan_timer_cpp.h \
	    zoltan_timer.h zoltan_types.h zoltan_util.h zz_const.h zz_heap.h zz_id_const.h \
	    zz_rand.h zz_sort.h zz_util_const.h

# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/src/Zoltan

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 

# XLC++ on BGL systems has trouble with some of the Zoltan code in -O mode.
# -> this seems to be a compiler bug and we may be able to take this out once
# IBM has it fixed.
ifeq ($(ESMF_OS),Linux)
ifeq ($(ESMF_COMPILER),xlf)
ESMF_OPTLEVEL = 0
endif
endif
