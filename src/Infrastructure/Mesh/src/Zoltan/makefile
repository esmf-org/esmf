# $Id: makefile,v 1.3 2007/08/20 19:34:51 dneckels Exp $

# leave alone.
ALL: build_here 

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# If you have C files in this directory, list them all on the line below.
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


# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = 
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# List all .h files which should be copied to common include dir
STOREH	  = 

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = mesh
LIBBASE	  = libesmf

# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Mesh/src/Zoltan

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

# Leave the following line blank.
DIRS	  = 



# If you have class files which need to be compiled in a particular
# order, list the dependencies below, with no rules following.
#
# e.g.:
#




