/** @file
 *
 * Functions to wrap netCDF-4 functions for PIO.
 *
 * @author Ed Hartnett
 */
#include <pio.h>
#include <pio_internal.h>
#include <config.h>

/**
 * Set deflate (zlib) settings for a variable.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable.
 * @param shuffle non-zero to turn on shuffle filter.
 * @param deflate non-zero to turn on zlib compression for this
 * variable.
 * @param deflate_level 1 to 9, with 1 being faster and 9 being more
 * compressed.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_def_var_c
 * @author Ed Hartnett
 */
int
PIOc_def_var_deflate(int ncid, int varid, int shuffle, int deflate,
                     int deflate_level)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    PLOG((1, "PIOc_def_var_deflate ncid = %d varid = %d shuffle = %d deflate = %d deflate_level = %d",
          ncid, varid, shuffle, deflate, deflate_level));

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_DEF_VAR_DEFLATE;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&shuffle, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&deflate, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&deflate_level, 1, MPI_INT, ios->compmain, ios->intercomm);
        }

        /* Handle MPI errors from computation tasks. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    if (ios->ioproc)
    {
#ifdef _NETCDF4
        if (file->do_io)
            ierr = nc_def_var_deflate(file->fh, varid, shuffle, deflate, deflate_level);
#endif
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

/**
 * Set szip settings for a variable.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable.
 * @param option_mask The options mask. Can be PIO_SZIP_EC or PIO_SZIP_NN.
 * @param pixels_per_block Pixels per block. Must be even and not greater than 32, with typical
 * values being 8, 10, 16, or 32. This parameter affects compression
 * ratio; the more pixel values vary, the smaller this number should be
 * to achieve better performance. If pixels_per_block is bigger than the
 * total number of elements in a dataset chunk, NC_EINVAL will be
 * returned.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_def_var_c
 * @author Jim Edwards, Ed Hartnett
 */
int
PIOc_def_var_szip(int ncid, int varid, int options_mask, int pixels_per_block)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    PLOG((1, "PIOc_def_var_szip ncid = %d varid = %d mask = %d ppb = %d",
          ncid, varid, options_mask, pixels_per_block));

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_DEF_VAR_SZIP;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&options_mask, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&pixels_per_block, 1, MPI_INT, ios->compmain, ios->intercomm);
        }

        /* Handle MPI errors from computation tasks. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    if (ios->ioproc)
    {
#ifdef _NETCDF4
        if (file->do_io)
            ierr = nc_def_var_szip(file->fh, varid, options_mask, pixels_per_block);
#endif
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

#ifdef  NC_HAS_BZ2
/**
 * Set bzip2 settings for a variable.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable.
 * @param bzip2_level 1 to 9, with 1 being faster and 9 being more
 * compressed.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_def_var_c
 * @author Jim Edwards, Ed Hartnett
 */
int
PIOc_def_var_bzip2(int ncid, int varid, int level)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    PLOG((1, "PIOc_def_var_bzip2 ncid = %d varid = %d level = %d",
          ncid, varid, level));

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_DEF_VAR_BZIP2;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&level, 1, MPI_INT, ios->compmain, ios->intercomm);
        }

        /* Handle MPI errors from computation tasks. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    if (ios->ioproc)
    {
#ifdef _NETCDF4
        if (file->do_io)
            ierr = nc_def_var_bzip2(file->fh, varid, level);
#endif
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    return PIO_NOERR;
}
#endif

#ifdef NC_HAS_ZSTD
/**
 * Set zstandard settings for a variable.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable.
 * @param zstandard_level 1 to 9, with 1 being faster and 9 being more
 * compressed.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_def_var_c
 * @author Jim Edwards, Ed Hartnett
 */
int
PIOc_def_var_zstandard(int ncid, int varid, int level)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    PLOG((1, "PIOc_def_var_zstandard ncid = %d varid = %d level = %d",
          ncid, varid, level));

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_DEF_VAR_ZSTANDARD;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&level, 1, MPI_INT, ios->compmain, ios->intercomm);
        }

        /* Handle MPI errors from computation tasks. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    if (ios->ioproc)
    {
#ifdef _NETCDF4
        if (file->do_io)
            ierr = nc_def_var_zstandard(file->fh, varid, level);
#endif
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    return PIO_NOERR;
}
#endif

/**
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * Inquire about deflate (zlib compression) settings for a variable.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable to set chunksizes for.
 * @param shufflep pointer to an int that will get the status of the
 * shuffle filter. Ignored if NULL.
 * @param deflatep pointer to an int that will be set to non-zero if
 * deflation is in use for this variable. Ignored if NULL.
 * @param deflate_levelp pointer to an int that will get the deflation
 * level (from 1-9) if deflation is in use for this variable. Ignored
 * if NULL.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_inq_var_c
 * @author Ed Hartnett
 */
int
PIOc_inq_var_deflate(int ncid, int varid, int *shufflep, int *deflatep,
                     int *deflate_levelp)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    PLOG((1, "PIOc_inq_var_deflate ncid = %d varid = %d", ncid, varid));

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_INQ_VAR_DEFLATE;
            char shuffle_present = shufflep ? true : false;
            char deflate_present = deflatep ? true : false;
            char deflate_level_present = deflate_levelp ? true : false;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&shuffle_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (shuffle_present && !mpierr)
                mpierr = MPI_Bcast(shufflep, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&deflate_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (deflate_present && !mpierr)
                mpierr = MPI_Bcast(deflatep, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&deflate_level_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (deflate_level_present && !mpierr)
                mpierr = MPI_Bcast(deflate_levelp, 1, MPI_INT, ios->compmain, ios->intercomm);
            PLOG((2, "PIOc_inq_var_deflate ncid = %d varid = %d shuffle_present = %d deflate_present = %d "
                  "deflate_level_present = %d", ncid, varid, shuffle_present, deflate_present,
                  deflate_level_present));
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    if (ios->ioproc)
    {
#ifdef _NETCDF4
        if (file->do_io)
            ierr = nc_inq_var_deflate(file->fh, varid, shufflep, deflatep, deflate_levelp);
#endif
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. */
    if (shufflep)
        if ((mpierr = MPI_Bcast(shufflep, 1, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (deflatep)
        if ((mpierr = MPI_Bcast(deflatep, 1, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (deflate_levelp)
        if ((mpierr = MPI_Bcast(deflate_levelp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

/**
 * Set chunksizes for a variable.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * Chunksizes have important performance repercussions. NetCDF
 * attempts to choose sensible chunk sizes by default, but for best
 * performance check chunking against access patterns.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable to set chunksizes for.
 * @param storage NC_CONTIGUOUS or NC_CHUNKED.
 * @param chunksizesp an array of chunksizes. Must have a chunksize for
 * every variable dimension.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_def_var_c
 * @author Ed Hartnett
 */
int
PIOc_def_var_chunking(int ncid, int varid, int storage, const PIO_Offset *chunksizesp)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ndims;             /* The number of dimensions for this var. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_def_var_chunking ncid = %d varid = %d storage = %d", ncid,
          varid, storage));

    /* Find the info about this file. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* Run this on all tasks if async is not in use, but only on
     * non-IO tasks if async is in use. Get the number of
     * dimensions. */
    if (!ios->async || !ios->ioproc)
        if ((ierr = PIOc_inq_varndims(ncid, varid, &ndims)))
            return check_netcdf(file, ierr, __FILE__, __LINE__);
    PLOG((2, "PIOc_def_var_chunking first ndims = %d", ndims));

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_DEF_VAR_CHUNKING;
            char chunksizes_present = chunksizesp ? true : false;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&storage, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&ndims, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&chunksizes_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (!mpierr && chunksizes_present)
                mpierr = MPI_Bcast((PIO_Offset *)chunksizesp, ndims, MPI_OFFSET, ios->compmain,
                                   ios->intercomm);
            PLOG((2, "PIOc_def_var_chunking ncid = %d varid = %d storage = %d ndims = %d chunksizes_present = %d",
                  ncid, varid, storage, ndims, chunksizes_present));
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

        /* Broadcast values currently only known on computation tasks to IO tasks. */
        if ((mpierr = MPI_Bcast(&ndims, 1, MPI_INT, ios->comproot, ios->my_comm)))
            check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    PLOG((2, "PIOc_def_var_chunking ndims = %d", ndims));

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
#ifdef _NETCDF4
        if (file->do_io)
        {
            size_t chunksizes_sizet[ndims];
            for (int d = 0; d < ndims; d++)
            {
                if (chunksizesp[d] < 0)
                {
                    ierr = PIO_ERANGE;
                    break;
                }
                chunksizes_sizet[d] = chunksizesp[d];
            }
            if (!ierr)
                ierr = nc_def_var_chunking(file->fh, varid, storage, chunksizes_sizet);
        }
#endif
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

/**
 * Inquire about chunksizes for a variable.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable to set chunksizes for.
 * @param storagep pointer to int which will be set to either
 * NC_CONTIGUOUS or NC_CHUNKED.
 * @param chunksizesp pointer to memory where chunksizes will be
 * set. There are the same number of chunksizes as there are
 * dimensions.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_inq_var_c
 * @author Ed Hartnett
 */
int
PIOc_inq_var_chunking(int ncid, int varid, int *storagep, PIO_Offset *chunksizesp)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */
    int ndims; /* The number of dimensions in the variable. */

    PLOG((1, "PIOc_inq_var_chunking ncid = %d varid = %d"));

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* Run these on all tasks if async is not in use, but only on
     * non-IO tasks if async is in use. */
    if (!ios->async || !ios->ioproc)
    {
        /* Find the number of dimensions of this variable. */
        if ((ierr = PIOc_inq_varndims(ncid, varid, &ndims)))
            return pio_err(ios, file, ierr, __FILE__, __LINE__);
        PLOG((2, "ndims = %d", ndims));
    }

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_INQ_VAR_CHUNKING;
            char storage_present = storagep ? true : false;
            char chunksizes_present = chunksizesp ? true : false;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&storage_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&ndims, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&chunksizes_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            PLOG((2, "PIOc_inq_var_chunking ncid = %d varid = %d storage_present = %d chunksizes_present = %d",
                  ncid, varid, storage_present, chunksizes_present));
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

        /* Broadcast values currently only known on computation tasks to IO tasks. */
        if ((mpierr = MPI_Bcast(&ndims, 1, MPI_INT, ios->comproot, ios->my_comm)))
            check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
#ifdef _NETCDF4
        if (file->do_io)
        {
            size_t chunksizes_sizet[ndims];
            ierr = nc_inq_var_chunking(file->fh, varid, storagep, chunksizes_sizet);
            if (!ierr && chunksizesp)
                for (int d = 0; d < ndims; d++)
                {
                    if (chunksizes_sizet[d] > NC_MAX_INT64)
                    {
                        ierr = PIO_ERANGE;
                        break;
                    }
                    chunksizesp[d] = chunksizes_sizet[d];
                }
        }
#endif
        PLOG((2, "ierr = %d", ierr));
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. */
    if ((mpierr = MPI_Bcast(&ndims, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (storagep)
        if ((mpierr = MPI_Bcast(storagep, 1, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (chunksizesp)
        if ((mpierr = MPI_Bcast(chunksizesp, ndims, MPI_OFFSET, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

/**
 * Set chunksizes for a variable.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * Chunksizes have important performance repercussions. NetCDF
 * attempts to choose sensible chunk sizes by default, but for best
 * performance check chunking against access patterns.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable to set chunksizes for.
 * @param endian NC_ENDIAN_NATIVE, NC_ENDIAN_LITTLE, or NC_ENDIAN_BIG.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_def_var_c
 * @author Ed Hartnett
 */
int
PIOc_def_var_endian(int ncid, int varid, int endian)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_DEF_VAR_ENDIAN;
            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&endian, 1, MPI_INT, ios->compmain, ios->intercomm);
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    if (ios->ioproc)
    {
#ifdef _NETCDF4
        if (file->do_io)
            ierr = nc_def_var_endian(file->fh, varid, endian);
#endif
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

/**
 * Inquire about chunksizes for a variable.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable to set chunksizes for.
 * @param endianp pointer to int which will be set to
 * endianness. Ignored if NULL.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_inq_var_c
 * @author Ed Hartnett
 */
int
PIOc_inq_var_endian(int ncid, int varid, int *endianp)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_inq_var_endian ncid = %d varid = %d"));

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_INQ_VAR_ENDIAN;
            char endian_present = endianp ? true : false;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&endian_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
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
#ifdef _NETCDF4
        if (file->do_io)
            ierr = nc_inq_var_endian(file->fh, varid, endianp);
#endif
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. */
    if (endianp)
        if ((mpierr = MPI_Bcast(endianp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

/**
 * Set chunk cache netCDF files to be opened/created.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * The file chunk cache for HDF5 can be set, and will apply for any
 * files opened or created until the program ends, or the settings are
 * changed again. The cache settings apply only to the open file. They
 * do not persist with the file, and must be set each time the file is
 * opened, before it is opened, if they are to have effect.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param iosysid the IO system ID.
 * @param iotype the iotype of files to be created or opened.
 * @param size size of file cache.
 * @param nelems number of elements in file cache.
 * @param preemption preemption setting for file cache.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_def_var_c
 * @author Ed Hartnett
 */
int
PIOc_set_chunk_cache(int iosysid, int iotype, PIO_Offset size, PIO_Offset nelems,
                     float preemption)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_set_chunk_cache iosysid = %d iotype = %d size = %d nelems = %d preemption = %g",
          iosysid, iotype, size, nelems, preemption));

    /* Get the IO system info. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    /* Only netCDF-4 files can use this feature. */
    if (iotype != PIO_IOTYPE_NETCDF4P && iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, NULL, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_SET_CHUNK_CACHE; /* Message for async notification. */

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&iosysid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&iotype, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&size, 1, MPI_OFFSET, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&nelems, 1, MPI_OFFSET, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&preemption, 1, MPI_FLOAT, ios->compmain, ios->intercomm);
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
#ifdef _NETCDF4
        PLOG((2, "calling nc_chunk_cache"));
        if (iotype == PIO_IOTYPE_NETCDF4P)
            ierr = nc_set_chunk_cache(size, nelems, preemption);
        else
            if (!ios->io_rank)
                ierr = nc_set_chunk_cache(size, nelems, preemption);
#endif
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    if (ierr)
        check_netcdf2(ios, NULL, ierr, __FILE__, __LINE__);

    PLOG((2, "PIOc_set_chunk_cache complete!"));
    return PIO_NOERR;
}

/**
 * Get current file chunk cache settings from HDF5.
 *
 * This function has no effect on netCDF classic files. Calling this
 * function with iotype of PIO_IOTYPE_PNETCDF or PIO_IOTYPE_NETCDF
 * returns an error.
 *
 * The file chunk cache for HDF5 can be set, and will apply for any
 * files opened or created until the program ends, or the settings are
 * changed again. The cache settings apply only to the open file. They
 * do not persist with the file, and must be set each time the file is
 * opened, before it is opened, if they are to have effect.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * Chunksizes have important performance repercussions. NetCDF
 * attempts to choose sensible chunk sizes by default, but for best
 * performance check chunking against access patterns.
 *
 * @param iosysid the IO system ID.
 * @param iotype the iotype of files to be created or opened.
 * @param sizep gets the size of file cache.
 * @param nelemsp gets the number of elements in file cache.
 * @param preemptionp gets the preemption setting for file cache.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_def_var_c
 * @author Ed Hartnett
 */
int
PIOc_get_chunk_cache(int iosysid, int iotype, PIO_Offset *sizep, PIO_Offset *nelemsp,
                     float *preemptionp)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_get_chunk_cache iosysid = %d iotype = %d", iosysid, iotype));

    /* Get the io system info. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    /* Only netCDF-4 files can use this feature. */
    if (iotype != PIO_IOTYPE_NETCDF4P && iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, NULL, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_GET_CHUNK_CACHE; /* Message for async notification. */
            char size_present = sizep ? true : false;
            char nelems_present = nelemsp ? true : false;
            char preemption_present = preemptionp ? true : false;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&iosysid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&iotype, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&size_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&nelems_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&preemption_present, 1, MPI_CHAR, ios->compmain,
                                   ios->intercomm);
            PLOG((2, "PIOc_get_chunk_cache size_present = %d nelems_present = %d "
                  "preemption_present = %d ", size_present, nelems_present, preemption_present));
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
#ifdef _NETCDF4
        if (iotype == PIO_IOTYPE_NETCDF4P)
            ierr = nc_get_chunk_cache((size_t *)sizep, (size_t *)nelemsp, preemptionp);
        else
            if (!ios->io_rank)
                ierr = nc_get_chunk_cache((size_t *)sizep, (size_t *)nelemsp, preemptionp);
#endif
        PLOG((2, "nc_get_chunk_cache called ierr = %d", ierr));
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    PLOG((2, "bcast complete ierr = %d sizep = %d", ierr, sizep));
    if (ierr)
        return check_netcdf(NULL, ierr, __FILE__, __LINE__);

    if (sizep)
    {
        PLOG((2, "bcasting size = %d ios->ioroot = %d", *sizep, ios->ioroot));
        if ((mpierr = MPI_Bcast(sizep, 1, MPI_OFFSET, ios->ioroot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
        PLOG((2, "bcast size = %d", *sizep));
    }
    if (nelemsp)
    {
        if ((mpierr = MPI_Bcast(nelemsp, 1, MPI_OFFSET, ios->ioroot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
        PLOG((2, "bcast complete nelems = %d", *nelemsp));
    }
    if (preemptionp)
    {
        if ((mpierr = MPI_Bcast(preemptionp, 1, MPI_FLOAT, ios->ioroot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
        PLOG((2, "bcast complete preemption = %d", *preemptionp));
    }

    return PIO_NOERR;
}

/**
 * Set chunksizes for a variable.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * Chunksizes have important performance repercussions. NetCDF
 * attempts to choose sensible chunk sizes by default, but for best
 * performance check chunking against access patterns.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable to set chunksizes for.
 * @param size the size in bytes for the cache.
 * @param nelems the number of elements in the cache.
 * @param preemption the cache preemption value.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_def_var_c
 * @author Ed Hartnett
 */
int
PIOc_set_var_chunk_cache(int ncid, int varid, PIO_Offset size, PIO_Offset nelems,
                         float preemption)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_SET_VAR_CHUNK_CACHE;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&size, 1, MPI_OFFSET, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&nelems, 1, MPI_OFFSET, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&preemption, 1, MPI_FLOAT, ios->compmain, ios->intercomm);
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    if (ios->ioproc)
    {
#ifdef _NETCDF4
        if (file->do_io)
            ierr = nc_set_var_chunk_cache(file->fh, varid, size, nelems, preemption);
#endif
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

/**
 * Get the variable chunk cache settings.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * Note that these settings are not part of the data file - they apply
 * only to the open file as long as it is open.
 *
 *  See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable to set chunksizes for.
 * @param sizep will get the size of the cache in bytes. Ignored if NULL.
 * @param nelemsp will get the number of elements in the cache. Ignored if NULL.
 * @param preemptionp will get the cache preemption value. Ignored if NULL.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_inq_var_c
 * @author Ed Hartnett
 */
int
PIOc_get_var_chunk_cache(int ncid, int varid, PIO_Offset *sizep, PIO_Offset *nelemsp,
                         float *preemptionp)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_get_var_chunk_cache ncid = %d varid = %d", ncid, varid));

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_GET_VAR_CHUNK_CACHE; /* Message for async notification. */
            char size_present = sizep ? true : false;
            char nelems_present = nelemsp ? true : false;
            char preemption_present = preemptionp ? true : false;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&size_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&nelems_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&preemption_present, 1, MPI_CHAR, ios->compmain,
                                   ios->intercomm);
            PLOG((2, "PIOc_get_var_chunk_cache size_present = %d nelems_present = %d "
                  "preemption_present = %d ", size_present, nelems_present, preemption_present));
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
#ifdef _NETCDF4
        if (file->do_io)
            ierr = nc_get_var_chunk_cache(file->fh, varid, (size_t *)sizep, (size_t *)nelemsp,
                                          preemptionp);
#endif
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. */
    if (sizep && !ierr)
        if ((mpierr = MPI_Bcast(sizep, 1, MPI_OFFSET, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (nelemsp && !ierr)
        if ((mpierr = MPI_Bcast(nelemsp, 1, MPI_OFFSET, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (preemptionp && !ierr)
        if ((mpierr = MPI_Bcast(preemptionp, 1, MPI_FLOAT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    return PIO_NOERR;
}
/* use this variable in the NETCDF library (introduced in v4.9.0) to determine if the following
   functions are available */
#ifdef NC_HAS_MULTIFILTERS
/**
 * Set the variable filter ids
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 *  See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable.
 * @param id set the filter id.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_filters
 * @author Jim Edwards/Ed Hartnett
 */
int
PIOc_def_var_filter(int ncid, int varid, unsigned int id, size_t nparams, unsigned int* params)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_def_var_filter ncid = %d varid = %d id = %d nparams = %d", ncid, varid, id, nparams));
#ifdef DEBUG
    for(i=0; i<nparams; i++)
        PLOG(1, "  param %d %d\n",i, params[i]);
#endif

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_DEF_VAR_FILTER; /* Message for async notification. */

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&id, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&nparams, 1, PIO_MPI_SIZE_T, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(params, nparams, MPI_UNSIGNED, ios->compmain, ios->intercomm);

        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        if (file->do_io)
            ierr = nc_def_var_filter(file->fh, varid, id, nparams, params);
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    return PIO_NOERR;
}
#ifdef PIO_HAS_PAR_FILTERS
/**
 * Get the variable filter ids if any
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * Note that these settings are not part of the data file - they apply
 * only to the open file as long as it is open.
 *
 *  See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable.
 * @param nfiltersp Pointer to the number of filters; may be 0.
 * @param ids return the filter ids.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_filters
 * @author Jim Edwards/Ed Hartnett
 */
int
PIOc_inq_var_filter_ids(int ncid, int varid, size_t *nfiltersp, unsigned int *ids)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_inq_var_filter_ids ncid = %d varid = %d", ncid, varid));

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_INQ_VAR_FILTER_IDS; /* Message for async notification. */
            char cnt_present = nfiltersp ? true : false;
            char ids_present = ids ? true : false;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&cnt_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&ids_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if(!mpierr && ids_present){
                size_t idcnt;
                idcnt = sizeof(ids);
                mpierr = MPI_Bcast(&idcnt, 1, PIO_MPI_SIZE_T, ios->compmain, ios->intercomm);
            }

            PLOG((2, "PIOc_inq_var_filter_ids cnt_present = %d ids_present = %d",
                  cnt_present, ids_present));
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        if (file->do_io)
          ierr = nc_inq_var_filter_ids(file->fh, varid, nfiltersp, ids);
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. */
    if (nfiltersp && !ierr)
        if ((mpierr = MPI_Bcast(nfiltersp, 1, MPI_OFFSET, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if ((*nfiltersp)> 0 && ids && !ierr)
        if ((mpierr = MPI_Bcast(ids, *nfiltersp, MPI_UNSIGNED, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

/**
 * Get the variable filter info if any
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * Note that these settings are not part of the data file - they apply
 * only to the open file as long as it is open.
 *
 *  See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable.
 * @param id The filter id of interest
 * @param nparamsp (OUT) Storage which will get the number of parameters to the filter
 * @param params   (OUT) Storage which will get the associated parameters.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_filters
 * @author Jim Edwards/Ed Hartnett
 */
int
PIOc_inq_var_filter_info(int ncid, int varid, unsigned int id, size_t *nparamsp, unsigned int *params )
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_inq_var_filter_info ncid = %d varid = %d id=%d", ncid, varid, id));

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_INQ_VAR_FILTER_INFO; /* Message for async notification. */
            char nparamsp_present = nparamsp ? true : false;
            char params_present = params ? true : false;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&id, 1, MPI_UNSIGNED, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&nparamsp_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&params_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if(!mpierr && params_present){
                size_t paramsize;
                paramsize = sizeof(params);
                mpierr = MPI_Bcast(&paramsize, 1, PIO_MPI_SIZE_T, ios->compmain, ios->intercomm);
            }
            PLOG((2, "PIOc_inq_var_filter_info nparamsp_present = %d params_present = %d ",
                  nparamsp_present, params_present));
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        if (file->do_io)
          ierr = nc_inq_var_filter_info(file->fh, varid, id, nparamsp, params);
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. */
    if (nparamsp && !ierr)
        if ((mpierr = MPI_Bcast(nparamsp, 1, MPI_OFFSET, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if ((*nparamsp)> 0 && params && !ierr)
      if ((mpierr = MPI_Bcast(params, *(nparamsp), MPI_UNSIGNED, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

#ifdef NC_HAS_BZ2
/**
 * Get the variable bzip2 filter info if any
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 *
 *  See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable.
 * @param hasfilterp (OUT) Pointer that gets a 0 if bzip2 is not in use for this var and a 1 if it is.  Ignored if NULL
 * @param levelp (OUT) Pointer that gets the level setting (1 - 9) Ignored if NULL
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_filters
 * @author Jim Edwards/Ed Hartnett
 */
int
PIOc_inq_var_bzip2(int ncid, int varid, int* hasfilterp, int *levelp)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_inq_var_bzip2 ncid = %d varid = %d", ncid, varid));

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_INQ_VAR_BZIP2; /* Message for async notification. */
            char hasfilterp_present = hasfilterp ? true : false;
            char levelp_present = levelp ? true : false;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&hasfilterp_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&levelp_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            PLOG((2, "PIOc_inq_var_bzip2 hasfilterp_present = %d levelp_present = %d ",
                  hasfilterp_present, levelp_present));
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        if (file->do_io)
          ierr = nc_inq_var_bzip2(file->fh, varid, hasfilterp, levelp);
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. */
    if (hasfilterp && !ierr)
        if ((mpierr = MPI_Bcast(hasfilterp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    if (levelp && !ierr)
        if ((mpierr = MPI_Bcast(levelp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    return PIO_NOERR;
}
#endif
#ifdef NC_HAS_ZSTD
/**
 * Get the variable zstandard filter info if any
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 *
 *  See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable.
 * @param hasfilterp (OUT) Pointer that gets a 0 if zstandard is not in use for this var and a 1 if it is.  Ignored if NULL
 * @param levelp (OUT) Pointer that gets the level setting (1 - 9) Ignored if NULL
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_filters
 * @author Jim Edwards/Ed Hartnett
 */
int
PIOc_inq_var_zstandard(int ncid, int varid, int* hasfilterp, int *levelp)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_inq_var_zstandard ncid = %d varid = %d", ncid, varid));

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_INQ_VAR_ZSTANDARD; /* Message for async notification. */
            char hasfilterp_present = hasfilterp ? true : false;
            char levelp_present = levelp ? true : false;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&hasfilterp_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&levelp_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            PLOG((2, "PIOc_inq_var_zstandard hasfilterp_present = %d levelp_present = %d ",
                  hasfilterp_present, levelp_present));
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        if (file->do_io)
          ierr = nc_inq_var_zstandard(file->fh, varid, hasfilterp, levelp);
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. */
    if (hasfilterp && !ierr)
        if ((mpierr = MPI_Bcast(hasfilterp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    if (levelp && !ierr)
        if ((mpierr = MPI_Bcast(levelp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    return PIO_NOERR;
}
// NC_HAS_ZSTD
#endif
#endif
#ifdef PIO_HAS_PAR_FILTERS
/**
 *
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * Note that these settings are not part of the data file - they apply
 * only to the open file as long as it is open.
 *
 *  See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param id   the filter of interest
 * @return PIO_NOERR if the filter is available, PIO_ENOFILTER if unavailable
 * @ingroup PIO_filters
 * @author Jim Edwards/Ed Hartnett
 */
int
PIOc_inq_filter_avail(int ncid, unsigned int id )
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_inq_filter_avail ncid = %d id = %d ", ncid, id));

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_INQ_FILTER_AVAIL; /* Message for async notification. */

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&id, 1, MPI_INT, ios->compmain, ios->intercomm);
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        if (file->do_io)
          ierr = nc_inq_filter_avail(file->fh, id);
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr && ierr !=NC_ENOFILTER)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. */

    return ierr;
}
// PIO_HAS_PAR_FILTERS
#endif 
// NC_HAS_MULTIFILTERS
#endif
#ifdef NC_HAS_QUANTIZE
/**
 * Turn on quantization for a variable
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * Note that these settings are not part of the data file - they apply
 * only to the open file as long as it is open.
 *
 *  See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable.
 * @param quantize_mode
 * @param nsd Number of significant digits.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_inq_var_c
 * @author Jim Edwards/Ed Hartnett
 */
int
PIOc_def_var_quantize(int ncid, int varid, int quantize_mode, int nsd )
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_def_var_quantize ncid = %d varid = %d quantize_mode=%d nsd=%d", ncid, varid, quantize_mode, nsd));

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_DEF_VAR_QUANTIZE; /* Message for async notification. */

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&quantize_mode, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&nsd, 1, MPI_INT, ios->compmain, ios->intercomm);
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        if (file->do_io)
          ierr = nc_def_var_quantize(file->fh, varid, quantize_mode, nsd);
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

/**
 * Learn whether quantization is on for a variable, and, if so, the NSD setting.
 *
 * This function only applies to netCDF-4 files. When used with netCDF
 * classic files, the error PIO_ENOTNC4 will be returned.
 *
 * Note that these settings are not part of the data file - they apply
 * only to the open file as long as it is open.
 *
 *  See the <a
 * href="http://www.unidata.ucar.edu/software/netcdf/docs/group__variables.html">netCDF
 * variable documentation</a> for details about the operation of this
 * function.
 *
 * @param ncid the ncid of the open file.
 * @param varid the ID of the variable.
 * @param quantize_modep Pointer that gets a 0 if quantization is not in use for this var, and a 1 if it is. Ignored if NULL.
 * @param nsdp Pointer that gets the NSD setting (from 1 to 15), if quantization is in use. Ignored if NULL.
 * @return PIO_NOERR for success, otherwise an error code.
 * @ingroup PIO_inq_var_c
 * @author Jim Edwards/Ed Hartnett
 */
int
PIOc_inq_var_quantize(int ncid, int varid, int *quantize_mode, int *nsdp )
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    PLOG((1, "PIOc_inq_var_quantize ncid = %d varid = %d ", ncid, varid));

    /* Get the file info. */
    if ((ierr = pio_get_file(ncid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Only netCDF-4 files can use this feature. */
    if (file->iotype != PIO_IOTYPE_NETCDF4P && file->iotype != PIO_IOTYPE_NETCDF4C)
        return pio_err(ios, file, PIO_ENOTNC4, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_INQ_VAR_QUANTIZE; /* Message for async notification. */
            char qmode_present = quantize_mode ? true : false;
            char nsdp_present = nsdp ? true : false;

            if (ios->compmain == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1, MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&qmode_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);
            if (!mpierr)
                mpierr = MPI_Bcast(&nsdp_present, 1, MPI_CHAR, ios->compmain, ios->intercomm);

            PLOG((2, "PIOc_inq_var_quantize qmode_present = %d nsdp_present = %d ",
                  qmode_present, nsdp_present));
        }

        /* Handle MPI errors. */
        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
            return check_mpi(ios, NULL, mpierr2, __FILE__, __LINE__);
        if (mpierr)
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        if (file->do_io)
          ierr = nc_inq_var_quantize(file->fh, varid, quantize_mode, nsdp);
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. */
    if (quantize_mode && !ierr)
        if ((mpierr = MPI_Bcast(quantize_mode, 1, MPI_INT, ios->ioroot, ios->my_comm)))
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (nsdp && !ierr)
      if ((mpierr = MPI_Bcast(nsdp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    return PIO_NOERR;
}
#endif
