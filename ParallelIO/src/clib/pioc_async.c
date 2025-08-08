/**
 * @file
 * Some initialization and support functions for async operations.
 * @author Jim Edwards
 * @date  2022
 *
 * @see https://github.com/NCAR/ParallelIO
 */
#include <config.h>
#include <pio.h>
#include <pio_internal.h>
#include <parallel_sort.h>

#ifdef NETCDF_INTEGRATION
#include "ncintdispatch.h"
#endif /* NETCDF_INTEGRATION */

#ifdef USE_MPE
/* The event numbers for MPE logging. */
extern int event_num[2][NUM_EVENTS];
#endif /* USE_MPE */

#ifdef NETCDF_INTEGRATION
/* Have we initialized the netcdf integration code? */
extern int ncint_initialized;

/* This is used as the default iosysid for the netcdf integration
 * code. */
extern int diosysid;
#endif /* NETCDF_INTEGRATION */

extern int default_error_handler; /* defined in pioc.c */

/**
 * @defgroup PIO_init_async Initialize an ASYNC IO System
 * Initialize the IOSystem, including specifying number of IO and
 * computation tasks in C.
 *
 */

/**
 * Library initialization used when IO tasks are distinct from compute
 * tasks.
 *
 * This is a collective call.  Input parameters are read on
 * comp_rank=0 values on other tasks are ignored.  This variation of
 * PIO_init sets up a distinct set of tasks to handle IO, these tasks
 * do not return from this call.  Instead they go to an internal loop
 * and wait to receive further instructions from the computational
 * tasks.
 *
 * Sequence of Events to do Asynch I/O
 * -----------------------------------
 *
 * Here is the sequence of events that needs to occur when an IO
 * operation is called from the collection of compute tasks.  I'm
 * going to use pio_put_var because write_darray has some special
 * characteristics that make it a bit more complicated...
 *
 * Compute tasks call pio_put_var with an integer argument
 *
 * The MPI_Send sends a message from comp_rank=0 to io_rank=0 on
 * union_comm (a comm defined as the union of io and compute tasks)
 * msg is an integer which indicates the function being called, in
 * this case the msg is PIO_MSG_PUT_VAR_INT
 *
 * The iotasks now know what additional arguments they should expect
 * to receive from the compute tasks, in this case a file handle, a
 * variable id, the length of the array and the array itself.
 *
 * The iotasks now have the information they need to complete the
 * operation and they call the pio_put_var routine.  (In pio1 this bit
 * of code is in pio_get_put_callbacks.F90.in)
 *
 * After the netcdf operation is completed (in the case of an inq or
 * get operation) the result is communicated back to the compute
 * tasks.
 *
 * @param world the communicator containing all the available tasks.
 *
 * @param num_io_procs the number of processes for the IO component.
 *
 * @param io_proc_list an array of lenth num_io_procs with the
 * processor number for each IO processor. If NULL then the IO
 * processes are assigned starting at processes 0.
 *
 * @param component_count number of computational components
 *
 * @param num_procs_per_comp an array of int, of length
 * component_count, with the number of processors in each computation
 * component.
 *
 * @param proc_list an array of arrays containing the processor
 * numbers for each computation component. If NULL then the
 * computation components are assigned processors sequentially
 * starting with processor num_io_procs.
 *
 * @param user_io_comm pointer to an MPI_Comm. If not NULL, it will
 * get an MPI duplicate of the IO communicator. (It is a full
 * duplicate and later must be freed with MPI_Free() by the caller.)
 *
 * @param user_comp_comm pointer to an array of pointers to MPI_Comm;
 * the array is of length component_count. If not NULL, it will get an
 * MPI duplicate of each computation communicator. (These are full
 * duplicates and each must later be freed with MPI_Free() by the
 * caller.)
 *
 * @param rearranger the default rearranger to use for decompositions
 * in this IO system. Only PIO_REARR_BOX is supported for
 * async. Support for PIO_REARR_SUBSET will be provided in a future
 * version.
 *
 * @param iosysidp pointer to array of length component_count that
 * gets the iosysid for each component.
 *
 * @return PIO_NOERR on success, error code otherwise.
 * @ingroup PIO_init_async
 * @author Ed Hartnett, Jim Edwards
 */
int
PIOc_init_async(MPI_Comm world, int num_io_procs, int *io_proc_list,
                int component_count, int *num_procs_per_comp, int **proc_list,
                MPI_Comm *user_io_comm, MPI_Comm *user_comp_comm, int rearranger,
                int *iosysidp)
{
    int my_rank;          /* Rank of this task. */
    int **my_proc_list;   /* Array of arrays of procs for comp components. */
    int my_io_proc_list[num_io_procs]; /* List of processors in IO component. */
    int mpierr;           /* Return code from MPI functions. */
    int ret;              /* Return code. */
//    int world_size;

    /* Check input parameters. Only allow box rearranger for now. */
    if (num_io_procs < 1 || component_count < 1 || !num_procs_per_comp || !iosysidp ||
        (rearranger != PIO_REARR_BOX && rearranger != PIO_REARR_SUBSET))
        return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__);

    my_proc_list = (int**) malloc(component_count * sizeof(int*));

    /* Turn on the logging system for PIO. */
    if ((ret = pio_init_logging()))
        return pio_err(NULL, NULL, ret, __FILE__, __LINE__);
    PLOG((1, "PIOc_init_async num_io_procs = %d component_count = %d", num_io_procs,
          component_count));

#ifdef USE_MPE
    pio_start_mpe_log(INIT);
#endif /* USE_MPE */

    /* Determine which tasks to use for IO. */
    for (int p = 0; p < num_io_procs; p++)
        my_io_proc_list[p] = io_proc_list ? io_proc_list[p] : p;

    PLOG((1, "PIOc_init_async call determine_procs"));
    /* Determine which tasks to use for each computational component. */
    if ((ret = determine_procs(num_io_procs, component_count, num_procs_per_comp,
                               proc_list, my_proc_list)))
        return pio_err(NULL, NULL, ret, __FILE__, __LINE__);

    PLOG((1, "PIOc_init_async determine_procs done world=%d",world));
    /* Get rank of this task in world. */
    if ((ret = MPI_Comm_rank(world, &my_rank)))
        return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);

    /* Get size of world. */
//    if ((ret = MPI_Comm_size(world, &world_size)))
//        return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);

    PLOG((1, "%d: num_io_procs = %d", my_rank, num_io_procs));

    /* Is this process in the IO component? */
    int pidx;
    for (pidx = 0; pidx < num_io_procs; pidx++)
        if (my_rank == my_io_proc_list[pidx])
            break;
    int in_io = (pidx == num_io_procs) ? 0 : 1;
    PLOG((1, "in_io = %d", in_io));

    /* Allocate struct to hold io system info for each computation component. */
    iosystem_desc_t *iosys[component_count], *my_iosys;
    for (int cmp1 = 0; cmp1 < component_count; cmp1++)
        if (!(iosys[cmp1] = (iosystem_desc_t *)calloc(1, sizeof(iosystem_desc_t))))
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__);

    PLOG((1, "create world group "));
    /* Create group for world. */
    MPI_Group world_group;
    if ((ret = MPI_Comm_group(world, &world_group)))
        return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
    PLOG((1, "world group created"));

    /* We will create a group for the IO component. */
    MPI_Group io_group;

    /* The shared IO communicator. */
    MPI_Comm io_comm;

    /* Rank of current process in IO communicator. */
    int io_rank = -1;

    /* Set to MPI_ROOT on master process, MPI_PROC_NULL on other
     * processes. */
    int iomaster;

    /* Create a group for the IO component. */
    if ((ret = MPI_Group_incl(world_group, num_io_procs, my_io_proc_list, &io_group)))
        return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
    PLOG((1, "created IO group - io_group = %d MPI_GROUP_EMPTY = %d", io_group, MPI_GROUP_EMPTY));

    /* There is one shared IO comm. Create it. */
    if ((ret = MPI_Comm_create(world, io_group, &io_comm)))
        return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
    PLOG((1, "created io comm io_comm = %d", io_comm));

    /* Does the user want a copy of the IO communicator? */
    if (user_io_comm)
    {
        *user_io_comm = MPI_COMM_NULL;
        if (in_io)
            if ((mpierr = MPI_Comm_dup(io_comm, user_io_comm)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }

    /* For processes in the IO component, get their rank within the IO
     * communicator. */
    if (in_io)
    {
        PLOG((3, "about to get io rank"));
        if ((ret = MPI_Comm_rank(io_comm, &io_rank)))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
        iomaster = !io_rank ? MPI_ROOT : MPI_PROC_NULL;
        PLOG((3, "intracomm created for io_comm = %d io_rank = %d IO %s",
              io_comm, io_rank, iomaster == MPI_ROOT ? "MASTER" : "SERVANT"));
    }

    /* We will create a group for each computational component. */
    MPI_Group group[component_count];

    /* We will also create a group for each component and the IO
     * component processes (i.e. a union of computation and IO
     * processes. */
    MPI_Group union_group[component_count];

    /* For each computation component. */
    for (int cmp = 0; cmp < component_count; cmp++)
    {
        PLOG((2, "processing component %d", cmp));

        /* Get pointer to current iosys. */
        my_iosys = iosys[cmp];

        /* The rank of the computation leader in the union comm. */
        my_iosys->comproot = num_io_procs;

        /* Initialize some values. */
        my_iosys->io_comm = MPI_COMM_NULL;
        my_iosys->comp_comm = MPI_COMM_NULL;
        my_iosys->union_comm = MPI_COMM_NULL;
        my_iosys->intercomm = MPI_COMM_NULL;
        my_iosys->my_comm = MPI_COMM_NULL;
        my_iosys->async = 1;
        my_iosys->error_handler = default_error_handler;
        my_iosys->num_comptasks = num_procs_per_comp[cmp];
        my_iosys->num_iotasks = num_io_procs;
        my_iosys->num_uniontasks = my_iosys->num_comptasks + my_iosys->num_iotasks;
        my_iosys->default_rearranger = rearranger;

        /* Initialize the rearranger options. */
        my_iosys->rearr_opts.comm_type = PIO_REARR_COMM_COLL;
        my_iosys->rearr_opts.fcd = PIO_REARR_COMM_FC_2D_DISABLE;

        /* We are not providing an info object. */
        my_iosys->info = MPI_INFO_NULL;

        /* Create a group for this component. */
        if ((ret = MPI_Group_incl(world_group, num_procs_per_comp[cmp], my_proc_list[cmp],
                                  &group[cmp])))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
        PLOG((2, "created component MPI group - group[%d] = %d", cmp, group[cmp]));

        /* For all the computation components create a union group
         * with their processors and the processors of the (shared) IO
         * component. */

        /* How many processors in the union comm? */
        int nprocs_union = num_io_procs + num_procs_per_comp[cmp];

        /* This will hold proc numbers from both computation and IO
         * components. */
        int proc_list_union[nprocs_union];

        /* Add proc numbers from IO. */
        for (int p = 0; p < num_io_procs; p++)
            proc_list_union[p] = my_io_proc_list[p];

        /* Add proc numbers from computation component. */
        for (int p = 0; p < num_procs_per_comp[cmp]; p++)
            proc_list_union[p + num_io_procs] = my_proc_list[cmp][p];

//        qsort(proc_list_union, num_procs_per_comp[cmp] + num_io_procs, sizeof(int), compare_ints);
        for (int p = 0; p < num_procs_per_comp[cmp] + num_io_procs; p++)
            PLOG((3, "p %d num_io_procs %d proc_list_union[p + num_io_procs] %d ",
                  p, num_io_procs, proc_list_union[p]));

        /* The rank of the computation leader in the union comm. First task which is not an io task */
        my_iosys->ioroot = 0;
/*
        my_iosys->comproot = -1;
        my_iosys->ioroot = -1;
        for (int p = 0; p < num_procs_per_comp[cmp] + num_io_procs; p++)
        {
            bool ioproc = false;
            for (int q = 0; q < num_io_procs; q++)
            {
                if (proc_list_union[p] == my_io_proc_list[q])
                {
                    ioproc = true;
                    my_iosys->ioroot = proc_list_union[p];
                    break;
                }
            }
            if ( !ioproc && my_iosys->comproot < 0)
            {
                my_iosys->comproot = proc_list_union[p];
            }
        }
*/

        PLOG((3, "my_iosys->comproot = %d ioroot = %d", my_iosys->comproot, my_iosys->ioroot));



        /* Allocate space for computation task ranks. */
        if (!(my_iosys->compranks = calloc(my_iosys->num_comptasks, sizeof(int))))
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__);

        /* Remember computation task ranks. We need the ranks within
         * the union_comm. */
        for (int p = 0; p < num_procs_per_comp[cmp]; p++)
            my_iosys->compranks[p] = num_io_procs + p;

        /* Remember whether this process is in the IO component. */
        my_iosys->ioproc = in_io;

        /* With async, tasks are either in a computation component or
         * the IO component. */
        my_iosys->compproc = !in_io;

        /* Is this process in this computation component? */
        int in_cmp = 0;
        for (pidx = 0; pidx < num_procs_per_comp[cmp]; pidx++)
            if (my_rank == my_proc_list[cmp][pidx])
                break;
        in_cmp = (pidx == num_procs_per_comp[cmp]) ? 0 : 1;
        PLOG((3, "pidx = %d num_procs_per_comp[%d] = %d in_cmp = %d",
              pidx, cmp, num_procs_per_comp[cmp], in_cmp));

        /* Create the union group. */
        if ((ret = MPI_Group_incl(world_group, nprocs_union, proc_list_union, &union_group[cmp])))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
        PLOG((3, "created union MPI_group - union_group[%d] = %d with %d procs", cmp,
              union_group[cmp], nprocs_union));

        /* Create an intracomm for this component. Only processes in
         * the component need to participate in the intracomm create
         * call. */
        PLOG((3, "creating intracomm cmp = %d from group[%d] = %d", cmp, cmp, group[cmp]));
        if ((ret = MPI_Comm_create(world, group[cmp], &my_iosys->comp_comm)))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);

        if (in_cmp)
        {
            /* Does the user want a copy? */
            if (user_comp_comm)
                if ((mpierr = MPI_Comm_dup(my_iosys->comp_comm, &user_comp_comm[cmp])))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

            /* Get the rank in this comp comm. */
            if ((ret = MPI_Comm_rank(my_iosys->comp_comm, &my_iosys->comp_rank)))
                return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);

            /* Set comp_rank 0 to be the compmaster. It will have a
             * setting of MPI_ROOT, all other tasks will have a
             * setting of MPI_PROC_NULL. */
            my_iosys->compmaster = my_iosys->comp_rank ? MPI_PROC_NULL : MPI_ROOT;

            PLOG((3, "intracomm created for cmp = %d comp_comm = %d comp_rank = %d comp %s",
                  cmp, my_iosys->comp_comm, my_iosys->comp_rank,
                  my_iosys->compmaster == MPI_ROOT ? "MASTER" : "SERVANT"));
        }

        /* If this is the IO component, make a copy of the IO comm for
         * each computational component. */
        if (in_io)
        {
            PLOG((3, "making a dup of io_comm = %d io_rank = %d", io_comm, io_rank));
            if ((ret = MPI_Comm_dup(io_comm, &my_iosys->io_comm)))
                return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
            PLOG((3, "dup of io_comm = %d io_rank = %d", my_iosys->io_comm, io_rank));
            my_iosys->iomaster = iomaster;
            my_iosys->io_rank = io_rank;
            my_iosys->ioroot = 0;
            my_iosys->comp_idx = cmp;
        }

        /* Create an array that holds the ranks of the tasks to be used
         * for IO. */
        if (!(my_iosys->ioranks = calloc(my_iosys->num_iotasks, sizeof(int))))
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__);
        for (int i = 0; i < my_iosys->num_iotasks; i++)
            my_iosys->ioranks[i] = i;

        /* All the processes in this component, and the IO component,
         * are part of the union_comm. */
        PLOG((3, "before creating union_comm my_iosys->io_comm = %d group = %d", my_iosys->io_comm, union_group[cmp]));
        if ((ret = MPI_Comm_create(world, union_group[cmp], &my_iosys->union_comm)))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
        PLOG((3, "created union comm for cmp %d my_iosys->union_comm %d", cmp, my_iosys->union_comm));


        if (in_io || in_cmp)
        {
            if ((ret = MPI_Comm_rank(my_iosys->union_comm, &my_iosys->union_rank)))
                return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
            PLOG((3, "my_iosys->union_rank %d", my_iosys->union_rank));

            /* Set my_comm to union_comm for async. */
            my_iosys->my_comm = my_iosys->union_comm;
            PLOG((3, "intracomm created for union cmp = %d union_rank = %d union_comm = %d",
                  cmp, my_iosys->union_rank, my_iosys->union_comm));

            if (in_io)
            {
                PLOG((3, "my_iosys->io_comm = %d", my_iosys->io_comm));
                /* Create the intercomm from IO to computation component. */
                PLOG((3, "about to create intercomm for IO component to cmp = %d "
                      "my_iosys->io_comm = %d comproot %d", cmp, my_iosys->io_comm, my_iosys->comproot));
                if ((ret = MPI_Intercomm_create(my_iosys->io_comm, 0, my_iosys->union_comm,
                                                my_iosys->comproot, cmp, &my_iosys->intercomm)))
                    return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
            }
            else
            {
                /* Create the intercomm from computation component to IO component. */
                PLOG((3, "about to create intercomm for cmp = %d my_iosys->comp_comm = %d ioroot %d", cmp,
                      my_iosys->comp_comm, my_iosys->ioroot));
                if ((ret = MPI_Intercomm_create(my_iosys->comp_comm, 0, my_iosys->union_comm,
                                                my_iosys->ioroot, cmp, &my_iosys->intercomm)))
                    return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
            }
            PLOG((3, "intercomm created for cmp = %d", cmp));
        }

        /* Add this id to the list of PIO iosystem ids. */
        iosysidp[cmp] = pio_add_to_iosystem_list(my_iosys);
        PLOG((2, "new iosys ID added to iosystem_list iosysidp[%d] = %d", cmp, iosysidp[cmp]));

#ifdef NETCDF_INTEGRATION
        if (in_io || in_cmp)
        {
            /* Remember the io system id. */
            diosysid = iosysidp[cmp];
            PLOG((3, "diosysid = %d", iosysidp[cmp]));
        }
#endif /* NETCDF_INTEGRATION */

    } /* next computational component */

    /* Now call the function from which the IO tasks will not return
     * until the PIO_MSG_EXIT message is sent. This will handle
     * messages from all computation components. */
    if (in_io)
    {
        PLOG((2, "Starting message handler io_rank = %d component_count = %d",
              io_rank, component_count));
#ifdef USE_MPE
        pio_stop_mpe_log(INIT, __func__);
#endif /* USE_MPE */

        /* Start the message handler loop. This will not return until
         * an exit message is sent, or an error occurs. */
        if ((ret = pio_msg_handler2(io_rank, component_count, iosys, io_comm)))
            return pio_err(NULL, NULL, ret, __FILE__, __LINE__);
        PLOG((2, "Returned from pio_msg_handler2() ret = %d", ret));
    }

    /* Free resources if needed. */
    if (in_io)
        if ((mpierr = MPI_Comm_free(&io_comm)))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);

    /* Free the arrays of processor numbers. */
    for (int cmp = 0; cmp < component_count; cmp++)
        free(my_proc_list[cmp]);

    free(my_proc_list);

    /* Free MPI groups. */
    if ((ret = MPI_Group_free(&io_group)))
        return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);

    for (int cmp = 0; cmp < component_count; cmp++)
    {
        if ((ret = MPI_Group_free(&group[cmp])))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
        if ((ret = MPI_Group_free(&union_group[cmp])))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
    }

    if ((ret = MPI_Group_free(&world_group)))
        return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);

#ifdef USE_MPE
    if (!in_io)
        pio_stop_mpe_log(INIT, __func__);
#endif /* USE_MPE */

    PLOG((2, "successfully done with PIOc_init_async"));
    return PIO_NOERR;
}

/**
 * Library initialization used when IO tasks are distinct from compute
 * tasks.
 *
 * This is a collective call.  Input parameters are read on
 * each comp_rank=0 and on io_rank=0, values on other tasks are ignored.
 * This variation of PIO_init uses tasks in io_comm to handle IO,
 * these tasks do not return from this call.  Instead they go to an internal loop
 * and wait to receive further instructions from the computational
 * tasks.
 *
 * Sequence of Events to do Asynch I/O
 * -----------------------------------
 *
 * Here is the sequence of events that needs to occur when an IO
 * operation is called from the collection of compute tasks.  I'm
 * going to use pio_put_var because write_darray has some special
 * characteristics that make it a bit more complicated...
 *
 * Compute tasks call pio_put_var with an integer argument
 *
 * The MPI_Send sends a message from comp_rank=0 to io_rank=0 on
 * union_comm (a comm defined as the union of io and compute tasks)
 * msg is an integer which indicates the function being called, in
 * this case the msg is PIO_MSG_PUT_VAR_INT
 *
 * The iotasks now know what additional arguments they should expect
 * to receive from the compute tasks, in this case a file handle, a
 * variable id, the length of the array and the array itself.
 *
 * The iotasks now have the information they need to complete the
 * operation and they call the pio_put_var routine.  (In pio1 this bit
 * of code is in pio_get_put_callbacks.F90.in)
 *
 * After the netcdf operation is completed (in the case of an inq or
 * get operation) the result is communicated back to the compute
 * tasks.
 *
 * @param world the communicator containing all the available tasks.
 *
 * @param component_count number of computational components
 *
 * @param comp_comm an array of size component_count which are the defined
 * comms of each component - comp_comm should be MPI_COMM_NULL on tasks outside
 * the tasks of each comm these comms may overlap
 *
 * @param io_comm a communicator for the IO group, tasks in this comm do not
 * return from this call.
 *
 * @param rearranger the default rearranger to use for decompositions
 * in this IO system. Only PIO_REARR_BOX is supported for
 * async. Support for PIO_REARR_SUBSET will be provided in a future
 * version.
 *
 * @param iosysidp pointer to array of length component_count that
 * gets the iosysid for each component.
 *
 * @return PIO_NOERR on success, error code otherwise.
 * @ingroup PIO_init_async
 * @author Jim Edwards, Ed Hartnet
 */
int
PIOc_init_async_from_comms(MPI_Comm world, int component_count, MPI_Comm *comp_comm,
                           MPI_Comm io_comm, int rearranger, int *iosysidp)
{
    int my_rank;          /* Rank of this task. */
    int **my_proc_list;   /* Array of arrays of procs for comp components. */
    int *io_proc_list; /* List of processors in IO component. */
    int *num_procs_per_comp; /* List of number of tasks in each component */
    int num_io_procs = 0;
    int ret;              /* Return code. */
#ifdef USE_MPE
    bool in_io = false;
#endif /* USE_MPE */

#ifdef USE_MPE
    pio_start_mpe_log(INIT);
#endif /* USE_MPE */

    /* Check input parameters. Only allow box rearranger for now. */
    if (component_count < 1 || !comp_comm || !iosysidp ||
        (rearranger != PIO_REARR_BOX && rearranger != PIO_REARR_SUBSET))
        return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__);

    /* Turn on the logging system for PIO. */
    if ((ret = pio_init_logging()))
        return pio_err(NULL, NULL, ret, __FILE__, __LINE__);
    PLOG((1, "PIOc_init_async_from_comms component_count = %d", component_count));

    /* Get num_io_procs from io_comm, share with world */
    if (io_comm != MPI_COMM_NULL)
    {
#ifdef USE_MPE
        in_io = true;
#endif /* USE_MPE */
        if ((ret = MPI_Comm_size(io_comm, &num_io_procs)))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
    }
    if ((ret = MPI_Allreduce(MPI_IN_PLACE, &num_io_procs, 1, MPI_INT, MPI_MAX, world)))
        return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);

    /* Get io_proc_list from io_comm, share with world */
    io_proc_list = (int*) calloc(num_io_procs, sizeof(int));
    if (io_comm != MPI_COMM_NULL)
    {
        int my_io_rank;
        if ((ret = MPI_Comm_rank(io_comm, &my_io_rank)))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
        if ((ret = MPI_Comm_rank(world, &my_rank)))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
        io_proc_list[my_io_rank] = my_rank;
        component_count = 0;
    }
    if ((ret = MPI_Allreduce(MPI_IN_PLACE, io_proc_list, num_io_procs, MPI_INT, MPI_MAX, world)))
        return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);

    /* Get num_procs_per_comp for each comp and share with world */
    if ((ret = MPI_Allreduce(MPI_IN_PLACE, &(component_count), 1, MPI_INT, MPI_MAX, world)))
        return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);

    num_procs_per_comp = (int *) malloc(component_count * sizeof(int));

    for(int cmp=0; cmp < component_count; cmp++)
    {
        num_procs_per_comp[cmp] = 0;
        if(comp_comm[cmp] != MPI_COMM_NULL)
            if ((ret = MPI_Comm_size(comp_comm[cmp], &(num_procs_per_comp[cmp]))))
                return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
        if ((ret = MPI_Allreduce(MPI_IN_PLACE, &(num_procs_per_comp[cmp]), 1, MPI_INT, MPI_MAX, world)))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);

    }

    /* Get proc list for each comp and share with world */
    my_proc_list = (int**) malloc(component_count * sizeof(int*));

    for(int cmp=0; cmp < component_count; cmp++)
    {
        if (!(my_proc_list[cmp] = (int *) malloc(num_procs_per_comp[cmp] * sizeof(int))))
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__);
        for(int i = 0; i < num_procs_per_comp[cmp]; i++)
            my_proc_list[cmp][i] = 0;
        if(comp_comm[cmp] != MPI_COMM_NULL){
            int my_comp_rank;
            if ((ret = MPI_Comm_rank(comp_comm[cmp], &my_comp_rank)))
                return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
            if ((ret = MPI_Comm_rank(world, &my_rank)))
                return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
            my_proc_list[cmp][my_comp_rank] = my_rank;
        }
        if ((ret = MPI_Allreduce(MPI_IN_PLACE, my_proc_list[cmp], num_procs_per_comp[cmp],
                                 MPI_INT, MPI_MAX, world)))
            return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
    }

    if((ret = PIOc_init_async(world, num_io_procs, io_proc_list, component_count,
                           num_procs_per_comp, my_proc_list, NULL, NULL, rearranger,
                              iosysidp)))
        return pio_err(NULL, NULL, ret, __FILE__, __LINE__);

    for(int cmp=0; cmp < component_count; cmp++)
        free(my_proc_list[cmp]);
    free(my_proc_list);
    free(io_proc_list);
    free(num_procs_per_comp);

#ifdef USE_MPE
    if (!in_io)
        pio_stop_mpe_log(INIT, __func__);
#endif /* USE_MPE */

    PLOG((2, "successfully done with PIOc_init_async_from_comms"));
    return PIO_NOERR;
}

/**
 * Interface to call from pio_init from fortran.
 *
 * @param f90_world_comm the incoming communicator which includes all tasks
 * @param num_io_procs the number of IO tasks
 * @param io_proc_list the rank of io tasks in f90_world_comm
 * @param component_count the number of computational components
 * used an iosysid will be generated for each
 * @param procs_per_component the number of procs in each computational component
 * @param flat_proc_list a 1D array of size
 * component_count*maxprocs_per_component with rank in f90_world_comm
 * @param f90_io_comm the io_comm handle to be returned to fortran
 * @param f90_comp_comm the comp_comm handle to be returned to fortran
 * @param rearranger currently only PIO_REARRANGE_BOX is supported
 * @param iosysidp pointer to array of length component_count that
 * gets the iosysid for each component.
 * @returns 0 for success, error code otherwise
 * @ingroup PIO_init_async
 * @author Jim Edwards
 */
int
PIOc_init_async_from_F90(int f90_world_comm,
                             int num_io_procs,
                             int *io_proc_list,
                             int component_count,
                             int *procs_per_component,
                             int *flat_proc_list,
                             int *f90_io_comm,
                             int *f90_comp_comm,
                             int rearranger,
                             int *iosysidp)

{
    int ret = PIO_NOERR;
    MPI_Comm io_comm, comp_comm;
    int maxprocs_per_component=0;

   for(int i=0; i< component_count; i++)
        maxprocs_per_component = (procs_per_component[i] > maxprocs_per_component) ? procs_per_component[i] : maxprocs_per_component;

    int **proc_list = (int **) malloc(sizeof(int *) *component_count);

    for(int i=0; i< component_count; i++){
        proc_list[i] = (int *) malloc(sizeof(int) * maxprocs_per_component);
        for(int j=0;j<procs_per_component[i]; j++)
            proc_list[i][j] = flat_proc_list[j+i*maxprocs_per_component];
    }

    ret = PIOc_init_async(MPI_Comm_f2c(f90_world_comm), num_io_procs, io_proc_list,
                          component_count, procs_per_component, proc_list, &io_comm,
                          &comp_comm, rearranger, iosysidp);
    if(comp_comm)
        *f90_comp_comm = MPI_Comm_c2f(comp_comm);
    else
        *f90_comp_comm = 0;
    if(io_comm)
        *f90_io_comm = MPI_Comm_c2f(io_comm);
    else
        *f90_io_comm = 0;

    if (ret != PIO_NOERR)
    {
        PLOG((1, "PIOc_Init_Intercomm failed"));
        return ret;
    }
/*
    if (rearr_opts)
    {
        PLOG((1, "Setting rearranger options, iosys=%d", *iosysidp));
        return PIOc_set_rearr_opts(*iosysidp, rearr_opts->comm_type,
                                   rearr_opts->fcd,
                                   rearr_opts->comp2io.hs,
                                   rearr_opts->comp2io.isend,
                                   rearr_opts->comp2io.max_pend_req,
                                   rearr_opts->io2comp.hs,
                                   rearr_opts->io2comp.isend,
                                   rearr_opts->io2comp.max_pend_req);
    }
*/
    return ret;
}

/**
 * Interface to call from pio_init from fortran.
 *
 * @param f90_world_comm the incoming communicator which includes all tasks
 * @param component_count the number of computational components
 * used an iosysid will be generated for each and a comp_comm is expected
 * for each
 * @param f90_comp_comms the comp_comm handles passed from fortran
 * @param f90_io_comm the io_comm passed from fortran
 * @param rearranger currently only PIO_REARRANGE_BOX is supported
 * @param iosysidp pointer to array of length component_count that
 * gets the iosysid for each component.
 * @returns 0 for success, error code otherwise
 * @ingroup PIO_init_async
 * @author Jim Edwards
 */
int
PIOc_init_async_comms_from_F90(int f90_world_comm,
                               int component_count,
                               int *f90_comp_comms,
                               int f90_io_comm,
                               int rearranger,
                               int *iosysidp)

{
    int ret = PIO_NOERR;
    MPI_Comm comp_comm[component_count];
    MPI_Comm io_comm;

    for(int i=0; i<component_count; i++)
    {
        if(f90_comp_comms[i])
            comp_comm[i] = MPI_Comm_f2c(f90_comp_comms[i]);
        else
            comp_comm[i] = MPI_COMM_NULL;
    }
    if(f90_io_comm)
        io_comm = MPI_Comm_f2c(f90_io_comm);
    else
        io_comm = MPI_COMM_NULL;

    ret = PIOc_init_async_from_comms(MPI_Comm_f2c(f90_world_comm), component_count, comp_comm, io_comm,
                          rearranger, iosysidp);

    if (ret != PIO_NOERR)
    {
        PLOG((1, "PIOc_Init_async_from_comms failed"));
        return ret;
    }
/*
    if (rearr_opts)
    {
        PLOG((1, "Setting rearranger options, iosys=%d", *iosysidp));
        return PIOc_set_rearr_opts(*iosysidp, rearr_opts->comm_type,
                                   rearr_opts->fcd,
                                   rearr_opts->comp2io.hs,
                                   rearr_opts->comp2io.isend,
                                   rearr_opts->comp2io.max_pend_req,
                                   rearr_opts->io2comp.hs,
                                   rearr_opts->io2comp.isend,
                                   rearr_opts->io2comp.max_pend_req);
    }
*/
    return ret;
}
