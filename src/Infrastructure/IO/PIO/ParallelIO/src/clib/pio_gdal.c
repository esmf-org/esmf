#include <config.h>
#include <pio.h>
#include <pio_internal.h>
#include <ogr_api.h>

/**
 * The PIO-C interface for the GDAL function OGR_L_FindFieldIndex()
 *
 * This routine is called collectively by all tasks in the communicator
 * ios.union_comm.
 *
 * @param ncid the ncid of the open file, obtained from
 * GDALc_openfile() or GDALc_createfile().
 * @param name the field name.
 * @param varidp a pointer that will get the variable id
 * @return PIO_NOERR for success, error code otherwise.  <<See GDALc_Set_File_Error_Handling>>
 * @ingroup PIO_inq_var_c
 * @author Michael Long (adapted from Jim Edwards, Ed Hartnett)
 */
int
GDALc_inq_fieldid(int ncid, const char *name, int *varidp)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    /* Get file info based on ncid. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Caller must provide name. */
    if (!name || strlen(name) > NC_MAX_NAME)
        return pio_err(ios, file, PIO_EINVAL, __FILE__, __LINE__);

    PLOG((1, "GDALc_inq_varid ncid = %d name = %s", ncid, name));

    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_INQ_VARID;

            if (ios->compmaster == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmaster, ios->intercomm);
            int namelen;
            namelen = strlen(name);
            if (!mpierr)
                mpierr = MPI_Bcast(&namelen, 1, MPI_INT, ios->compmaster, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast((void *)name, namelen + 1, MPI_CHAR, ios->compmaster, ios->intercomm);
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
#ifdef _PNETCDF
        if (file->iotype == PIO_IOTYPE_PNETCDF)
            ierr = ncmpi_inq_varid(file->fh, name, varidp);
#endif /* _PNETCDF */

        if (file->iotype != PIO_IOTYPE_PNETCDF && file->do_io)
            ierr = nc_inq_varid(file->fh, name, varidp);
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. Ignore NULL parameters. */
    if (varidp)
        if ((mpierr = MPI_Bcast(varidp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
            check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

int
GDALc_openfile(int iosysid, OGRDataSourceH *hDSp, int *iotype, const char *filename, bool mode)
{
    iosystem_desc_t *ios;      /* Pointer to io system information. */
    file_desc_t *file;         /* Pointer to file information. */
    int imode;                 /* Internal mode val for netcdf4 file open. */
    int nvars = 0;
    int *rec_var = NULL;
    int *pio_type = NULL;
    int *pio_type_size = NULL;
    MPI_Datatype *mpi_type = NULL;
    int *mpi_type_size = NULL;
    int *ndims = NULL;
    int mpierr = MPI_SUCCESS, mpierr2;  /** Return code from MPI function codes. */
    int ierr = PIO_NOERR;      /* Return code from function calls. */

#ifdef USE_MPE
    pio_start_mpe_log(OPEN);
#endif /* USE_MPE */

    /* Get the IO system info from the iosysid. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    /* User must provide valid input for these parameters. */
    if (!hDSp || !iotype || !filename)
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__);
    if (*iotype != PIO_IOTYPE_GDAL )
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__);

//    PLOG((2, "PIOc_openfile_retry iosysid = %d iotype = %d filename = %s mode = %d retry = %d",
//          iosysid, *iotype, filename, mode, retry));

    /* Allocate space for the file info. */
    if (!(file = calloc(sizeof(*file), 1)))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);

    /* Fill in some file values. */
    file->fh = -1;
    file->iotype = *iotype;
    file->iosystem = ios;
    file->writable = (mode & PIO_WRITE) ? 1 : 0;

    /* Set to true if this task should participate in IO (only true
     * for one task with netcdf serial files. */
    if (file->iotype == PIO_IOTYPE_GDAL ||
        ios->io_rank == 0)
        file->do_io = 1;

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        int msg = PIO_MSG_OPEN_FILE;
        size_t len = strlen(filename);

        if (!ios->ioproc)
        {
            /* Send the message to the message handler. */
            if (ios->compmaster == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            /* Send the parameters of the function call. */
            if (!mpierr)
                mpierr = MPI_Bcast(&len, 1, MPI_INT, ios->compmaster, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast((void *)filename, len + 1, MPI_CHAR, ios->compmaster, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&file->iotype, 1, MPI_INT, ios->compmaster, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&mode, 1, MPI_INT, ios->compmaster, ios->intercomm);
//>>            if (!mpierr)
//>>                mpierr = MPI_Bcast(&use_ext_ncid, 1, MPI_INT, ios->compmaster, ios->intercomm);
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        switch (file->iotype)
        {
        case PIO_IOTYPE_NETCDF:
            if (ios->io_rank == 0)
            {
//>>                if ((ierr = nc_open(filename, mode, &file->fh)))
                    break;
                ierr = inq_file_metadata(file, file->fh, PIO_IOTYPE_NETCDF,
                                         &nvars, &rec_var, &pio_type,
                                         &pio_type_size, &mpi_type,
                                         &mpi_type_size, &ndims);
                PLOG((2, "PIOc_openfile_retry:nc_open for classic filename = %s mode = %d "
                      "ierr = %d", filename, mode, ierr));
            }
            break;

        default:
            return pio_err(ios, file, PIO_EBADIOTYPE, __FILE__, __LINE__);
        }

    }

    /* Broadcast and check the return code. */
    if (ios->ioroot == ios->union_rank)
        PLOG((2, "Bcasting error code ierr %d ios->ioroot %d ios->my_comm %d",
              ierr, ios->ioroot, ios->my_comm));
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    PLOG((2, "Bcast openfile_retry error code ierr = %d", ierr));

    /* If there was an error, free allocated memory and deal with the error. */
    if (ierr)
    {
        free(file);
        return check_netcdf2(ios, NULL, ierr, __FILE__, __LINE__);
    }

    /* Broadcast writability to all tasks. */
    if ((mpierr = MPI_Bcast(&file->writable, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    /* Broadcast some values to all tasks from io root. */
//>>    if (ios->async)
//>>    {
//>>        PLOG((3, "open bcasting pio_next_ncid %d ios->ioroot %d", pio_next_ncid, ios->ioroot));
//>>        if ((mpierr = MPI_Bcast(&pio_next_ncid, 1, MPI_INT, ios->ioroot, ios->my_comm)))
//>>            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//>>    }

    if ((mpierr = MPI_Bcast(&nvars, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    /* Non io tasks need to allocate to store info about variables. */
    if (nvars && !rec_var)
    {
        if (!(rec_var = malloc(nvars * sizeof(int))))
            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__);
        if (!(pio_type = malloc(nvars * sizeof(int))))
            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__);
        if (!(pio_type_size = malloc(nvars * sizeof(int))))
            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__);
        if (!(mpi_type = malloc(nvars * sizeof(MPI_Datatype))))
            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__);
        if (!(mpi_type_size = malloc(nvars * sizeof(int))))
            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__);
        if (!(ndims = malloc(nvars * sizeof(int))))
            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__);
    }
    if (nvars)
    {
        if ((mpierr = MPI_Bcast(rec_var, nvars, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        if ((mpierr = MPI_Bcast(pio_type, nvars, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        if ((mpierr = MPI_Bcast(pio_type_size, nvars, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        if ((mpierr = MPI_Bcast(mpi_type, nvars*(int)(sizeof(MPI_Datatype)/sizeof(int)), MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        if ((mpierr = MPI_Bcast(mpi_type_size, nvars, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        if ((mpierr = MPI_Bcast(ndims, nvars, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    {
        /* Create the ncid that the user will see. This is necessary
         * because otherwise ncids will be reused if files are opened
         * on multiple iosystems. */
//>>>>>>>>>>        file->pio_ncid = pio_next_ncid++;

        /* Return the PIO ncid to the user. */
//>>>>>>>>>>        *ncidp = file->pio_ncid;
    }

    /* Add this file to the list of currently open files. */
    pio_add_to_file_list(file);

    /* Add info about the variables to the file_desc_t struct. */
    for (int v = 0; v < nvars; v++)
        if ((ierr = add_to_varlist(v, rec_var[v], pio_type[v], pio_type_size[v],
                                   mpi_type[v], mpi_type_size[v], ndims[v],
                                   &file->varlist)))
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__);
    file->nvars = nvars;

    /* Free resources. */
    if (nvars)
    {
        if (rec_var)
            free(rec_var);
        if (pio_type)
            free(pio_type);
        if (pio_type_size)
            free(pio_type_size);
        if (mpi_type)
            free(mpi_type);
        if (mpi_type_size)
            free(mpi_type_size);
        if (ndims)
            free(ndims);
    }

#ifdef USE_MPE
    pio_stop_mpe_log(OPEN, __func__);
#endif /* USE_MPE */
    PLOG((2, "Opened file %s file->pio_ncid = %d file->fh = %d ierr = %d",
          filename, file->pio_ncid, file->fh, ierr));

    return ierr;
}

/**
 * @}
 */
