#include <config.h>
#include <pio.h>
#include <pio_internal.h>
#include <ogr_api.h>

/**
 * The PIO library maintains its own set of ncids. This is the next
 * ncid number that will be assigned.
 */
extern int pio_next_ncid;

static int
GDALc_inq_file_metadata(file_desc_t *file, GDALDatasetH hDS, int iotype, int *nvars,
                  int **rec_var, int **pio_type, int **pio_type_size,
                  MPI_Datatype **mpi_type, int **mpi_type_size, int **ndims)
{
    int mpierr;
    int ret;

    /* Check inputs. */
    pioassert(rec_var && pio_type && pio_type_size && mpi_type && mpi_type_size,
              "pointers must be provided", __FILE__, __LINE__);

    if (hDS == NULL) {
      return pio_err(file->iosystem, file, -999, __FILE__, __LINE__);
    }
    /* How many vars in the file? */
    if (iotype == PIO_IOTYPE_GDAL)
    {
      OGRLayerH hLayer = OGR_DS_GetLayer( hDS, 0 );
      OGR_L_ResetReading(hLayer);

      OGRFeatureDefnH hFD = OGR_L_GetLayerDefn(hLayer);
      *nvars = OGR_FD_GetFieldCount(hFD);
//      printf(">>>>> NVARS: %d\n",*nvars);
      if (nvars == 0) // empty file
	return pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__);

      /* Allocate storage for info about each var. */
      if (*nvars)
	{
	  if (!(*rec_var = malloc(*nvars * sizeof(int))))
            return PIO_ENOMEM;
	  if (!(*pio_type = malloc(*nvars * sizeof(int))))
            return PIO_ENOMEM;
	  if (!(*pio_type_size = malloc(*nvars * sizeof(int))))
            return PIO_ENOMEM;
	  if (!(*mpi_type = malloc(*nvars * sizeof(MPI_Datatype))))
            return PIO_ENOMEM;
	  if (!(*mpi_type_size = malloc(*nvars * sizeof(int))))
            return PIO_ENOMEM;
	  if (!(*ndims = malloc(*nvars * sizeof(int))))
            return PIO_ENOMEM;
	}

      /* Learn about each variable in the file. */
      for (int v = 0; v < *nvars; v++)
	{
	  int var_ndims;   /* Number of dims for this var. */
	  nc_type my_type;

	  /* Find type of the var and number of dims in this var. Also
	   * learn about type. */
	  size_t type_size;

	  //	    
	  var_ndims = 1; // FIXED FOR NOW. For data-read purposes, it's a 1D stream across the number of
                         // elements.
	  (*ndims)[v] = var_ndims;
	  //>>            if ((ret = nc_inq_var(ncid, v, NULL, &my_type, &var_ndims, NULL, NULL)))
	  //>>                return pio_err(NULL, file, ret, __FILE__, __LINE__);
	  OGRFieldType Fld = OGR_Fld_GetType(OGR_FD_GetFieldDefn(hFD,v));
//	  printf(">>>>> FIELDNAME: %s, TYPE: %d\n",OGR_Fld_GetNameRef(OGR_FD_GetFieldDefn(hFD,v)),Fld);
	  bool typeOK = true; // assume we're good
	  switch (Fld) {
	  case OFTReal:
	    (*pio_type)[v] = (int)PIO_DOUBLE;
	    break;
	  case OFTInteger:
	    (*pio_type)[v] = (int)PIO_INT;
	    break;
	  // This needs to be done. How do we deal with timestamps etc in GDAL vector fields?
	  //>>case OFTDate:
	  //>>  break;
	  //>>	    case OFTTime:
	  //>>	      break;
	  //>>	    case OFTDate:
	  //>>	      break;
	  //>>	    case OFTDateTime:
	  default:
//	    printf(">>>> Fld Type: %d\n",Fld);
	    typeOK = false;
	    break;
//	    return pio_err(file->iosystem, file, PIO_EBADIOTYPE, __FILE__, __LINE__);
	  }
	  //>>            if ((ret = nc_inq_type(ncid, (*pio_type)[v], NULL, &type_size)))
	  //>>                return check_netcdf(file, ret, __FILE__, __LINE__);
	  //>>            (*pio_type_size)[v] = type_size;

//	  printf(">>>>> TEST: hDS %p, hFD %p\n", (void *)hDS, (void *)hFD);
    
	  if (!typeOK) // Not a usable type 
	    continue;

	  /* Get the MPI type corresponding with the PIO type. */
	  if ((ret = find_mpi_type((*pio_type)[v], &(*mpi_type)[v], NULL)))
	    return pio_err(NULL, file, ret, __FILE__, __LINE__);
	  
	  /* Get the size of the MPI type. */
	  if ((*mpi_type)[v] == MPI_DATATYPE_NULL)
	    (*mpi_type_size)[v] = 0;
	  else
	    if ((mpierr = MPI_Type_size((*mpi_type)[v], &(*mpi_type_size)[v])))
	      return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
	  
//>>	  /* What are the dimids associated with this var? */
//>>	  if (var_ndims)
//>>	    {
//>>	      int var_dimids[var_ndims];
//>>	      if (iotype == PIO_IOTYPE_GDAL)
//>>		{
//>>//>>                if ((ret = nc_inq_vardimid(ncid, v, var_dimids)))
//>>//>>                    return pio_err(NULL, file, ret, __FILE__, __LINE__);
//>>		}
//>>
//>>	    }

//	  OGR_FD_Destroy(hFD);
	} /* next var */
    } /* If PIO_TYPE_GDAL */
    return PIO_NOERR;
}

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
GDALc_inq_fieldid(int fileid, const char *name, int *fieldidp)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr;              /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    /* Get file info based on fileid. */
    if ((ierr = pio_get_file(fileid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;

    /* Caller must provide name. */
    if (!name || strlen(name) > NC_MAX_NAME)
        return pio_err(ios, file, PIO_EINVAL, __FILE__, __LINE__);

    PLOG((1, "GDALc_inq_fieldid fileid = %d name = %s", fileid, name));

    if (ios->async)
    {
        if (!ios->ioproc)
        {
            int msg = PIO_MSG_INQ_VARID;

            if (ios->compmaster == MPI_ROOT)
                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);

            if (!mpierr)
                mpierr = MPI_Bcast(&fileid, 1, MPI_INT, ios->compmaster, ios->intercomm);
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

    /* If this is an IO task, then call the GDAL function. */
    if (ios->ioproc)
    {
      GDALDatasetH *hDS = file->hDS;
        if (file->do_io) { // We assume that its a GDAL file
//	  printf("NLayers: %d\n", OGR_DS_GetLayerCount(hDS));
	  OGRLayerH hLayer = OGR_DS_GetLayer( hDS, 0 );
	  OGR_L_ResetReading(hLayer);
	  if (hLayer == NULL) {
	    printf("Layer is NULL");
	    return -1;
	  }
	  *fieldidp = OGR_L_FindFieldIndex(hLayer,name,1);

	  pioassert(*fieldidp > 0, "variable not found", __FILE__, __LINE__);
//	  printf("INQ_FIELDID FIELD %s INDEX: %d\n", name, *fieldidp);

	}
    }

    /* Broadcast and check the return code. */
    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    if (ierr)
        return check_netcdf(file, ierr, __FILE__, __LINE__);

    /* Broadcast results to all tasks. Ignore NULL parameters. */
    if (fieldidp)
        if ((mpierr = MPI_Bcast(fieldidp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
            check_mpi(NULL, file, mpierr, __FILE__, __LINE__);

    return PIO_NOERR;
}

int
GDALc_openfile(int iosysid, int *fileIDp, GDALDatasetH *hDSp,int *iotype, const char *filename, bool mode)
//GDALc_openfile(int iosysid, GDALDatasetH *hDSp, int *iotype, const char *filename, bool mode)
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
    GDALDatasetH hDS;

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
        case PIO_IOTYPE_GDAL:
//            if (ios->io_rank == 0)
            {
	      *hDSp = OGROpen( filename, FALSE, NULL );
	      if( hDSp != NULL )

                ierr = GDALc_inq_file_metadata(file, *hDSp, PIO_IOTYPE_GDAL,
                                         &nvars, &rec_var, &pio_type,
                                         &pio_type_size, &mpi_type,
                                         &mpi_type_size, &ndims);
                PLOG((2, "GDALc_openfile:OGROpen for filename = %s mode = %d "
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
        return PIO_NOERR;// check_netcdf2(ios, NULL, ierr, __FILE__, __LINE__);
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

    file->hDS = *hDSp;
    
    /* Create the ncid/fileid that the user will see. This is necessary
     * because otherwise ids will be reused if files are opened
     * on multiple iosystems. */
    file->pio_ncid = pio_next_ncid++;
    *fileIDp=file->pio_ncid;

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

int
GDALc_sync(int fileid) {
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */
}

int GDALc_inq_timeid(int fileid, int *timeid) { // Is there a field of type OFTDate, OFTTime, or OFTDateTime?
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */

    /* Get file info based on ncid. */
    if ((ierr = pio_get_file(fileid, &file)))
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    ios = file->iosystem;
    if (file->hDS == NULL)
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);

    GDALDatasetH *hDS = file->hDS;
    OGRLayerH hLayer = OGR_DS_GetLayer( hDS, 0 );
    if (hLayer == NULL)
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
    OGR_L_ResetReading(hLayer);

    OGRFeatureDefnH hFD = OGR_L_GetLayerDefn(hLayer);
    if (hFD == NULL)
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
//    printf("XXXXX NFLDS: %d, FID  %d, hDS %p, hFD %p\n", file->nvars, fileid, (void *)file->hDS, (void *)hFD);
    int nFld = OGR_FD_GetFieldCount(hFD);
    OGRFieldDefnH hTMP = OGR_FD_GetFieldDefn(hFD,1);
                  hTMP = OGR_FD_GetFieldDefn(hFD,0);

    for (int i=0;i<(file->nvars)-1;i++) {
      printf("XXXXX i: %d\n", i);
      OGRFieldDefnH hFlD = OGR_FD_GetFieldDefn(hFD,i);
      OGRFieldType Fld = OGR_Fld_GetType(hFlD);
      if (Fld == NULL)
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
//      printf("XXXXX FIELDNAME: %s, TYPE: %d\n",OGR_Fld_GetNameRef(OGR_FD_GetFieldDefn(hFD,i)),Fld);

//      const char* FldTyp = OGR_GetFieldTypeName(Fld);
//      PRINTMSG("Field type: " << FldTyp);
    }

//    OGR_FD_Destroy(hFD);

    return 0;
  }

/**
 * Read an array of data from a file to the (serial) IO library. This
 * function is only used with netCDF classic and netCDF-4 serial
 * iotypes.
 *
 * @param file a pointer to the open file descriptor for the file
 * that will be written to
 * @param iodesc a pointer to the defined iodescriptor for the buffer
 * @param vid the variable id to be read.
 * @param iobuf the buffer to be written from this mpi task. May be
 * null. for example we have 8 ionodes and a distributed array with
 * global size 4, then at least 4 nodes will have a null iobuf. In
 * practice the box rearranger trys to have at least blocksize bytes
 * on each io task and so if the total number of bytes to write is
 * less than blocksize * numiotasks then some iotasks will have a NULL
 * iobuf.
 * @returns 0 for success, error code otherwise.
 * @ingroup PIO_read_darray_c
 * @author Jim Edwards, Ed Hartnett
 */
int
pio_read_darray_shp(file_desc_t *file, io_desc_t *iodesc, int vid,
                          void *iobuf)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    var_desc_t *vdesc;    /* Information about the variable. */
    int ndims;             /* Number of dims in decomposition. */
    int fndims;            /* Number of dims for this var in file. */
    MPI_Status status;
    int mpierr;  /* Return code from MPI functions. */
    int ierr;

    /* Check inputs. */
    pioassert(file && file->iosystem && iodesc && vid >= 0 && vid <= PIO_MAX_VARS,
              "invalid input", __FILE__, __LINE__);

    PLOG((2, "pio_read_darray_shp vid = %d", vid));
    ios = file->iosystem;

#ifdef TIMING
    /* Start timer if desired. */
    if ((ierr = pio_start_timer("PIO:read_darray_shp")))
        return pio_err(ios, NULL, ierr, __FILE__, __LINE__);
#endif /* TIMING */

    /* Get var info for this var. */
    if ((ierr = get_var_desc(vid, &file->varlist, &vdesc)))
        return pio_err(NULL, file, ierr, __FILE__, __LINE__);

    /* Get the number of dims in our decomposition. */
    ndims = iodesc->ndims;

    /* Get number of dims for this var. */
    fndims = vdesc->ndims;

    /* If setframe was not called, use a default value of 0. This is
     * required for backward compatibility. */
    if (fndims == ndims + 1 && vdesc->record < 0)
        vdesc->record = 0;
    PLOG((3, "fndims %d ndims %d vdesc->record %d vdesc->ndims %d", fndims,
          ndims, vdesc->record, vdesc->ndims));

    /* Confirm that we are being called with the correct ndims. */
    pioassert((fndims == ndims && vdesc->record < 0) ||
              (fndims == ndims + 1 && vdesc->record >= 0),
              "unexpected record", __FILE__, __LINE__);

    if (ios->ioproc)
    {
        io_region *region;
        size_t start[fndims];
        size_t count[fndims];
        size_t tmp_start[fndims * iodesc->maxregions];
        size_t tmp_count[fndims * iodesc->maxregions];
        size_t tmp_bufsize;
        void *bufptr;

        /* buffer is incremented by byte and loffset is in terms of
           the iodessc->mpitype so we need to multiply by the size of
           the mpitype. */
        region = iodesc->firstregion;

        /* If setframe was not set before this call, assume a value of
         * 0. This is required for backward compatibility. */
        if (fndims > ndims)
            if (vdesc->record < 0)
                vdesc->record = 0;

        /* Put together start/count arrays for all regions. */
        for (int regioncnt = 0; regioncnt < iodesc->maxregions; regioncnt++)
        {
            if (!region || iodesc->llen == 0)
            {
                /* Nothing to write for this region. */
                for (int i = 0; i < fndims; i++)
                {
                    tmp_start[i + regioncnt * fndims] = 0;
                    tmp_count[i + regioncnt * fndims] = 0;
                }
                bufptr = NULL;
            }
            else
            {
                if (vdesc->record >= 0 && fndims > 1)
                {
                    /* This is a record var. Find start for record dims. */
                    tmp_start[regioncnt * fndims] = vdesc->record;

                    /* Find start/count for all non-record dims. */
                    for (int i = 1; i < fndims; i++)
                    {
                        tmp_start[i + regioncnt * fndims] = region->start[i - 1];
                        tmp_count[i + regioncnt * fndims] = region->count[i - 1];
                    }

                    /* Set count for record dimension. */
                    if (tmp_count[1 + regioncnt * fndims] > 0)
                        tmp_count[regioncnt * fndims] = 1;
                }
                else
                {
                    /* Non-time dependent array */
                    for (int i = 0; i < fndims; i++)
                    {
                        tmp_start[i + regioncnt * fndims] = region->start[i];
                        tmp_count[i + regioncnt * fndims] = region->count[i];
                    }
                }
            }

#if PIO_ENABLE_LOGGING
            /* Log arrays for debug purposes. */
            PLOG((3, "region = %d", region));
            for (int i = 0; i < fndims; i++)
                PLOG((3, "tmp_start[%d] = %d tmp_count[%d] = %d", i + regioncnt * fndims, tmp_start[i + regioncnt * fndims],
                      i + regioncnt * fndims, tmp_count[i + regioncnt * fndims]));
#endif /* PIO_ENABLE_LOGGING */

            /* Move to next region. */
            if (region)
                region = region->next;
        } /* next regioncnt */

        /* IO tasks other than 0 send their starts/counts and data to
         * IO task 0. */
        if (ios->io_rank > 0)
        {
            if ((mpierr = MPI_Send(&iodesc->llen, 1, MPI_OFFSET, 0, ios->io_rank, ios->io_comm)))
                return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
            PLOG((3, "sent iodesc->llen = %d", iodesc->llen));

            if (iodesc->llen > 0)
            {
                if ((mpierr = MPI_Send(&(iodesc->maxregions), 1, MPI_INT, 0,
                                       ios->num_iotasks + ios->io_rank, ios->io_comm)))
                    return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
                if ((mpierr = MPI_Send(tmp_count, iodesc->maxregions * fndims, MPI_OFFSET, 0,
                                       2 * ios->num_iotasks + ios->io_rank, ios->io_comm)))
                    return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
                if ((mpierr = MPI_Send(tmp_start, iodesc->maxregions * fndims, MPI_OFFSET, 0,
                                       3 * ios->num_iotasks + ios->io_rank, ios->io_comm)))
                    return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
                PLOG((3, "sent iodesc->maxregions = %d tmp_count and tmp_start arrays", iodesc->maxregions));

                if ((mpierr = MPI_Recv(iobuf, iodesc->llen, iodesc->mpitype, 0,
                                       4 * ios->num_iotasks + ios->io_rank, ios->io_comm, &status)))
                    return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
                PLOG((3, "received %d elements of data", iodesc->llen));
            }
        }
        else if (ios->io_rank == 0)
        {
            /* This is IO task 0. Get starts/counts and data from
             * other IO tasks. */
            int maxregions = 0;
            size_t loffset, regionsize;
            size_t this_start[fndims * iodesc->maxregions];
            size_t this_count[fndims * iodesc->maxregions];

            for (int rtask = 1; rtask <= ios->num_iotasks; rtask++)
            {
                if (rtask < ios->num_iotasks)
                {
                    if ((mpierr = MPI_Recv(&tmp_bufsize, 1, MPI_OFFSET, rtask, rtask, ios->io_comm, &status)))
                        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
                    PLOG((3, "received tmp_bufsize = %d", tmp_bufsize));

                    if (tmp_bufsize > 0)
                    {
                        if ((mpierr = MPI_Recv(&maxregions, 1, MPI_INT, rtask, ios->num_iotasks + rtask,
                                               ios->io_comm, &status)))
                            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
                        if ((mpierr = MPI_Recv(this_count, maxregions * fndims, MPI_OFFSET, rtask,
                                               2 * ios->num_iotasks + rtask, ios->io_comm, &status)))
                            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
                        if ((mpierr = MPI_Recv(this_start, maxregions * fndims, MPI_OFFSET, rtask,
                                               3 * ios->num_iotasks + rtask, ios->io_comm, &status)))
                            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
                        PLOG((3, "received maxregions = %d this_count, this_start arrays ", maxregions));
                    }
                }
                else
                {
                    maxregions = iodesc->maxregions;
                    tmp_bufsize = iodesc->llen;
                }
                PLOG((3, "maxregions = %d tmp_bufsize = %d", maxregions, tmp_bufsize));

                /* Now get each region of data. */
                loffset = 0;
                for (int regioncnt = 0; regioncnt < maxregions; regioncnt++)
                {
                    /* Get pointer where data should go. */
                    bufptr = (void *)((char *)iobuf + iodesc->mpitype_size * loffset);
                    regionsize = 1;

                    /* ??? */
                    if (rtask < ios->num_iotasks)
                    {
                        for (int m = 0; m < fndims; m++)
                        {
                            start[m] = this_start[m + regioncnt * fndims];
                            count[m] = this_count[m + regioncnt * fndims];
                            regionsize *= count[m];
                        }
                    }
                    else
                    {
                        for (int m = 0; m < fndims; m++)
                        {
                            start[m] = tmp_start[m + regioncnt * fndims];
                            count[m] = tmp_count[m + regioncnt * fndims];
                            regionsize *= count[m];
                        }
                    }
                    loffset += regionsize;

                    /* Read the data. */
                    /* ierr = nc_get_vara(file->fh, vid, start, count, bufptr); */
                    switch (iodesc->piotype)
                    {
                    case PIO_BYTE:
                        return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__);
                    case PIO_CHAR:
                        return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__);
                    case PIO_SHORT:
                        return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__);
                    case PIO_INT:
		        printf("Int 1: Start %d, Count %d, fndims %d\n", start[0], count[0],fndims);
		        printf("Int 2: Start %d, Count %d\n", start[fndims-1], count[fndims-1]);
                        ierr = GDALc_shp_get_int_field(file->pio_ncid);
                        break;
                    case PIO_FLOAT:
                        return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__);
                    case PIO_DOUBLE:
                        ierr = GDALc_shp_get_double_field(file->pio_ncid, vid, start, count, (double *)bufptr);
                        break;
                    default:
                        return pio_err(ios, file, PIO_EBADTYPE, __FILE__, __LINE__);
                    }

                    /* Check error code of netCDF call. */
                    if (ierr)
                        return check_netcdf(file, ierr, __FILE__, __LINE__);
                }

                /* The decomposition may not use all of the active io
                 * tasks. rtask here is the io task rank and
                 * ios->num_iotasks is the number of iotasks actually
                 * used in this decomposition. */
                if (rtask < ios->num_iotasks && tmp_bufsize > 0){
                    if ((mpierr = MPI_Send(iobuf, tmp_bufsize, iodesc->mpitype, rtask,
                                           4 * ios->num_iotasks + rtask, ios->io_comm)))
                        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
		}
            }
        }
    }

#ifdef TIMING
    if ((ierr = pio_stop_timer("PIO:read_darray_nc_serial")))
        return pio_err(ios, NULL, ierr, __FILE__, __LINE__);
#endif /* TIMING */

    PLOG((2, "pio_read_darray_nc_serial complete ierr %d", ierr));
    return PIO_NOERR;
}

int 
GDALc_shp_get_int_field(int fileid)
{
  return PIO_NOERR;
}
int
GDALc_shp_get_double_field(int fileid, int varid, const size_t *startp,
                           const size_t *countp, double *ip)
{
  OGRFeatureH hF;
  file_desc_t *file;         /* Pointer to file information. */
  int ierr;

  /* Get file info based on fileid. */
  if ((ierr = pio_get_file(fileid, &file)))
    return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
  if (file->hDS == NULL)
    return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);

  OGRLayerH hL = OGR_DS_GetLayer( file->hDS, 0 );

  // here, we have to assume start and count are only one dimension, and have
  // only one assigned value.
  for (size_t i = startp[0]; i<countp[0]; i++) {
    
    hF     = OGR_L_GetFeature(hL,i);
    ip[i] = OGR_F_GetFieldAsDouble(hF,varid);
//    printf("Data %d: %0.2f\n",i,ip[i]);
  }

  return PIO_NOERR;
}
/**
 * @}
 */
