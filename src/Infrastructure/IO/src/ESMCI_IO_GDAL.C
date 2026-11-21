// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC IO_NetCDF method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_IO\_GDAL} methods declared
// in the companion file {\tt ESMCI\_IO\_GDAL.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_IO_GDAL.C"

// associated class definition file
#include "ESMCI_IO_GDAL.h"

// higher level, 3rd party or system includes here
#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <string>
#include <sstream>

#include "ESMC_Util.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_ArraySpec.h"
#include "ESMCI_LocalArray.h"
#include "ESMCI_Array.h"

#include "pio.h"

#include "esmf_io_debug.h"

using namespace std;

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

namespace ESMCI
{
//-------------------------------------------------------------------------

  ESMC_TypeKind_Flag  IO_GDAL::gdalToEsmcType(OGRFieldType gdalTypeVal)
  {

#undef  ESMC_METHOD
#define ESMC_METHOD "IO_GDAL::gdalToEsmcType"

    ESMC_TypeKind_Flag  esmcTypeVal = ESMF_NOKIND;

    // Currently only integer and double (real*8) are implemented

    switch (gdalTypeVal)
    {
    case OFTInteger:
      esmcTypeVal = ESMC_TYPEKIND_I4;
      break;
    case OFTIntegerList:
      // TODO?
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTReal:
      esmcTypeVal = ESMC_TYPEKIND_R8;
      break;
    case OFTRealList:
      // TODO?
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTString:
      // TODO?
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTStringList:
      // TODO?
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTBinary:
      // TODO?
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTDate:
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTTime:
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTDateTime:
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTInteger64:
      esmcTypeVal = ESMF_NOKIND;
      break;
    case OFTInteger64List:
      esmcTypeVal = ESMF_NOKIND;
      break;
    default:
      break;
    }

    return esmcTypeVal;
  }

//-------------------------------------------------------------------------

int GDALc_inq_field(int ncid, int varid, char *name, int *xtypep, int *ndimsp,
             int *dimidsp, int *nattsp)
  {
//>>    iosystem_desc_t *ios;
//>>    file_desc_t *file;
//>>    int ndims = 0;    /* The number of dimensions for this variable. */
//>>    int ierr;
//>>    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */
//>>
//>>    PLOG((1, "GDALc_inq_var ncid = %d varid = %d", ncid, varid));
//>>
//>>    /* Get the file info, based on the ncid. */
//>>    if ((ierr = pio_get_file(ncid, &file)))
//>>        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__);
//>>    ios = file->iosystem;
//>>
//>>    /* If async is in use, and this is not an IO task, bcast the parameters. */
//>>    if (ios->async)
//>>    {
//>>        if (!ios->ioproc)
//>>        {
//>>            int msg = PIO_MSG_INQ_VAR;
//>>            char name_present = name ? true : false;
//>>            char xtype_present = xtypep ? true : false;
//>>            char ndims_present = ndimsp ? true : false;
//>>            char dimids_present = dimidsp ? true : false;
//>>            char natts_present = nattsp ? true : false;
//>>
//>>            if (ios->compmaster == MPI_ROOT)
//>>                mpierr = MPI_Send(&msg, 1,MPI_INT, ios->ioroot, 1, ios->union_comm);
//>>
//>>            if (!mpierr)
//>>                mpierr = MPI_Bcast(&ncid, 1, MPI_INT, ios->compmaster, ios->intercomm);
//>>            if (!mpierr)
//>>                mpierr = MPI_Bcast(&varid, 1, MPI_INT, ios->compmaster, ios->intercomm);
//>>            if (!mpierr)
//>>                mpierr = MPI_Bcast(&name_present, 1, MPI_CHAR, ios->compmaster, ios->intercomm);
//>>            if (!mpierr)
//>>                mpierr = MPI_Bcast(&xtype_present, 1, MPI_CHAR, ios->compmaster, ios->intercomm);
//>>            if (!mpierr)
//>>                mpierr = MPI_Bcast(&ndims_present, 1, MPI_CHAR, ios->compmaster, ios->intercomm);
//>>            if (!mpierr)
//>>                mpierr = MPI_Bcast(&dimids_present, 1, MPI_CHAR, ios->compmaster, ios->intercomm);
//>>            if (!mpierr)
//>>                mpierr = MPI_Bcast(&natts_present, 1, MPI_CHAR, ios->compmaster, ios->intercomm);
//>>            PLOG((2, "PIOc_inq_var name_present = %d xtype_present = %d ndims_present = %d "
//>>                  "dimids_present = %d, natts_present = %d nattsp = %d",
//>>                  name_present, xtype_present, ndims_present, dimids_present, natts_present, nattsp));
//>>        }
//>>
//>>        /* Handle MPI errors. */
//>>        if ((mpierr2 = MPI_Bcast(&mpierr, 1, MPI_INT, ios->comproot, ios->my_comm)))
//>>            return check_mpi(NULL, file, mpierr2, __FILE__, __LINE__);
//>>        if (mpierr)
//>>            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//>>    }
//>>
//>>    /* Call the netCDF layer. */
//>>    if (ios->ioproc)
//>>    {
//>>        PLOG((2, "Calling the netCDF layer"));
//>>#ifdef _PNETCDF
//>>        if (file->iotype == PIO_IOTYPE_PNETCDF)
//>>        {
//>>            ierr = ncmpi_inq_varndims(file->fh, varid, &ndims);
//>>            PLOG((2, "from pnetcdf ndims = %d", ndims));
//>>            if (!ierr)
//>>                ierr = ncmpi_inq_var(file->fh, varid, name, xtypep, ndimsp, dimidsp, nattsp);
//>>        }
//>>#endif /* _PNETCDF */
//>>
//>>        if (file->iotype != PIO_IOTYPE_PNETCDF && file->do_io)
//>>        {
//>>            ierr = nc_inq_varndims(file->fh, varid, &ndims);
//>>            PLOG((3, "nc_inq_varndims called ndims = %d", ndims));
//>>            if (!ierr)
//>>            {
//>>                char my_name[NC_MAX_NAME + 1];
//>>                nc_type my_xtype;
//>>                int my_ndims = 0, *my_dimids, my_natts = 0;
//>>                if (ndims > 0)
//>>                  my_dimids = (int *) malloc(ndims * sizeof(int));
//>>                else
//>>                  my_dimids = NULL;
//>>                ierr = nc_inq_var(file->fh, varid, my_name, &my_xtype, &my_ndims, my_dimids,
//>>                                  &my_natts);
//>>                PLOG((3, "my_name = %s my_xtype = %d my_ndims = %d my_natts = %d",  my_name,
//>>                      my_xtype, my_ndims, my_natts));
//>>                if (!ierr)
//>>                {
//>>                    if (name)
//>>                        strcpy(name, my_name);
//>>                    if (xtypep)
//>>                        *xtypep = my_xtype;
//>>                    if (ndimsp)
//>>                        *ndimsp = my_ndims;
//>>                    if (dimidsp)
//>>                    {
//>>                        for (int d = 0; d < ndims; d++)
//>>                            dimidsp[d] = my_dimids[d];
//>>                    }
//>>                    if (my_dimids != NULL)
//>>                      free(my_dimids);
//>>                    if (nattsp)
//>>                        *nattsp = my_natts;
//>>                }
//>>            }
//>>        }
//>>        if (ndimsp)
//>>            PLOG((2, "PIOc_inq_var ndims = %d ierr = %d", *ndimsp, ierr));
//>>    }
//>>
//>>    /* Broadcast and check the return code. */
//>>    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
//>>        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//>>    if (ierr)
//>>        return check_netcdf(file, ierr, __FILE__, __LINE__);
//>>
//>>    /* Broadcast the results for non-null pointers. */
//>>    if (name)
//>>    {
//>>        int slen;
//>>        if (ios->iomaster == MPI_ROOT)
//>>            slen = strlen(name);
//>>        if ((mpierr = MPI_Bcast(&slen, 1, MPI_INT, ios->ioroot, ios->my_comm)))
//>>            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//>>        if ((mpierr = MPI_Bcast((void *)name, slen + 1, MPI_CHAR, ios->ioroot, ios->my_comm)))
//>>            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//>>    }
//>>    if (xtypep)
//>>        if ((mpierr = MPI_Bcast(xtypep, 1, MPI_INT, ios->ioroot, ios->my_comm)))
//>>            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//>>
//>>    if (ndimsp)
//>>    {
//>>        PLOG((2, "PIOc_inq_var about to Bcast ndims = %d ios->ioroot = %d ios->my_comm = %d",
//>>              *ndimsp, ios->ioroot, ios->my_comm));
//>>        if ((mpierr = MPI_Bcast(ndimsp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
//>>            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//>>        PLOG((2, "PIOc_inq_var Bcast ndims = %d", *ndimsp));
//>>    }
//>>    if (dimidsp)
//>>    {
//>>        if ((mpierr = MPI_Bcast(&ndims, 1, MPI_INT, ios->ioroot, ios->my_comm)))
//>>            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//>>        if ((mpierr = MPI_Bcast(dimidsp, ndims, MPI_INT, ios->ioroot, ios->my_comm)))
//>>            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//>>    }
//>>    if (nattsp)
//>>        if ((mpierr = MPI_Bcast(nattsp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
//>>            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//>>
//>>    return PIO_NOERR;
  }

//>>int GDALc_createfile(int iosysid, OGRDataSourceH *ncidp, int *iotype, const char *filename,
//>>                    bool mode)
//>>{
//>>    iosystem_desc_t *ios;  /* Pointer to io system information. */
//>>    int ret;               /* Return code from function calls. */
//>>
//>>//>>    /* Get the IO system info from the id. */
//>>//>>    if (!(ios = pio_get_iosystem_from_id(iosysid)))
//>>//>>        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);
//>>//>>
//>>//>>    PLOG((1, "PIOc_createfile iosysid = %d iotype = %d filename = %s mode = %d",
//>>//>>          iosysid, *iotype, filename, mode));
//>>//>>
//>>//>>    /* Create the file. */
//>>//>>    if ((ret = PIOc_createfile_int(iosysid, ncidp, iotype, filename, mode, 0)))
//>>//>>        return pio_err(ios, NULL, ret, __FILE__, __LINE__);
//>>//>>
//>>//>>    /* Set the fill mode to NOFILL. */
//>>//>>    if ((ret = PIOc_set_fill(*ncidp, NC_NOFILL, NULL)))
//>>//>>        return ret;
//>>
//>>    return ret;
//>>}

//<<>>int GDALc_openfile(int iosysid, OGRDataSourceH *hDSp, int *iotype, const char *filename,
//<<>>                  bool mode)
//<<>>{
//<<>>//>>    PLOG((1, "PIOc_openfile iosysid %d *iotype %d filename %s mode %d", iosysid,
//<<>>//>>          iotype ? *iotype: 0, filename, mode));
//<<>>  OGRRegisterAll();
//<<>>  OGRDataSourceH hDx = OGROpen( filename, FALSE, NULL );
//<<>>  PRINTMSG("NLayers -3: " << OGR_DS_GetLayerCount(hDx) << " file: " << filename );
//<<>>  PRINTMSG("NLayers -2: " << GDALDatasetGetLayerCount(hDx));
//<<>>  OGR_DS_Destroy(hDx);
//<<>>  *hDSp = OGROpen( filename, FALSE, NULL );
//<<>>  PRINTMSG("NLayers -1: " << OGR_DS_GetLayerCount(*hDSp));
//<<>>  if( hDSp == NULL )
//<<>>    {
//<<>>      if (ESMC_LogDefault.MsgFoundError(ESMF_RC_NOT_FOUND, "Open GDAL file failed", ESMC_CONTEXT, 0))
//<<>>      return ESMF_RC_NOT_FOUND;
//<<>>    }
//<<>>return 0;
//<<>>}

//<<>>int GDALc_inq_fieldid(OGRDataSourceH hDS, const char *name, int *varidp)
//<<>>{
//<<>>    iosystem_desc_t *ios;  /* Pointer to io system information. */
//<<>>    file_desc_t *file;     /* Pointer to file information. */
//<<>>    int ierr;              /* Return code from function calls. */
//<<>>    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function codes. */
//<<>>
//<<>>//>>    PLOG((1, "PIOc_inq_varid ncid = %d name = %s", ncid, name));
//<<>>
//<<>>#undef  ESMC_METHOD
//<<>>#define ESMC_METHOD "IO_GDAL::GDALc_inq_fieldid"
//<<>>
//<<>>    if (hDS == NULL) {
//<<>>      PRINTMSG("DataSource is NULL");
//<<>>      return -1;
//<<>>    } else {
//<<>>      PRINTMSG("DataSource is GOOD");
//<<>>      printf("hDS %p\n",(void *)hDS);
//<<>>    }
//<<>>
//<<>>    PRINTPOS;
//<<>>    PRINTMSG("NLayers: " << OGR_DS_GetLayerCount(hDS));
//<<>>    OGRLayerH hLayer = OGR_DS_GetLayer( hDS, 0 );
//<<>>    PRINTPOS;
//<<>>    OGR_L_ResetReading(hLayer);
//<<>>    PRINTPOS;
//<<>>    if (hLayer == NULL) {
//<<>>      PRINTMSG("Layer is NULL");
//<<>>      return -1;
//<<>>    }
//<<>>    *varidp = OGR_L_FindFieldIndex(hLayer,name,1);
//<<>>
//<<>>    PRINTMSG("INQ_FIELDID FIELD " << name << " INDEX: " << *varidp);
//<<>>
//<<>>    return *varidp;
//<<>>
//<<>>//>>    /* Broadcast and check the return code. */
//<<>>//>>    if ((mpierr = MPI_Bcast(&ierr, 1, MPI_INT, ios->ioroot, ios->my_comm)))
//<<>>//>>        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//<<>>//>>    if (ierr)
//<<>>//>>        return check_netcdf(file, ierr, __FILE__, __LINE__);
//<<>>//>>
//<<>>//>>    /* Broadcast results to all tasks. Ignore NULL parameters. */
//<<>>//>>    if (varidp)
//<<>>//>>        if ((mpierr = MPI_Bcast(varidp, 1, MPI_INT, ios->ioroot, ios->my_comm)))
//<<>>//>>            check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
//<<>>
//<<>>    return PIO_NOERR;
//<<>>}

  int GDALc_read_darray(OGRDataSourceH hDS, int fieldid, int ioid, MPI_Offset arraylen, void *array) 
  {
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    io_desc_t *iodesc;     /* Pointer to IO description information. */
    void *iobuf = NULL;    /* holds the data as read on the io node. */
    size_t rlen = 0;       /* the length of data in iobuf. */
    void *tmparray;        /* unsorted copy of array buf if required */
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function calls. */
    int ierr;              /* Return code. */

    PRINTMSG("GDALc_read_darray fieldid "<<fieldid<< " ioid " <<ioid<< " arraylen " << arraylen);

//    /* Get the file info. */
//    if ((ierr = pio_get_file(ncid, &file)))
//        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);
//    ios = file->iosystem;

//    /* Get the iodesc. */
//    if (!(iodesc = pio_get_iodesc_from_id(ioid)))
//        return pio_err(ios, file, PIO_EBADID, __FILE__, __LINE__);
//    pioassert(iodesc->rearranger == PIO_REARR_BOX || iodesc->rearranger == PIO_REARR_SUBSET,
//              "unknown rearranger", __FILE__, __LINE__);

//    /* iomaster needs max of buflen, others need local len */
//    if (ios->iomaster == MPI_ROOT)
//        rlen = iodesc->maxiobuflen;
//    else
//        rlen = iodesc->llen;
//
//    /* Allocate a buffer for one record. */
//    if (ios->ioproc && rlen > 0)
//        if (!(iobuf = malloc(iodesc->mpitype_size * rlen)))
//            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__);
//
//    /* Call the correct darray read function based on iotype. */
//    switch (file->iotype)
//    {
//    case PIO_IOTYPE_NETCDF:
//    case PIO_IOTYPE_NETCDF4C:
//        if ((ierr = pio_read_darray_nc_serial(file, iodesc, varid, iobuf)))
//            return pio_err(ios, file, ierr, __FILE__, __LINE__);
//        break;
//    case PIO_IOTYPE_PNETCDF:
//    case PIO_IOTYPE_NETCDF4P:
//        if ((ierr = pio_read_darray_nc(file, iodesc, varid, iobuf)))
//            return pio_err(ios, file, ierr, __FILE__, __LINE__);
//        break;
//    default:
//        return pio_err(NULL, NULL, PIO_EBADIOTYPE, __FILE__, __LINE__);
//    }
//
//    /* If the map is not monotonically increasing we will need to sort
//     * it. */
//    PLOG((2, "iodesc->needssort %d", iodesc->needssort));
//
//    if (iodesc->needssort)
//    {
//        if (!(tmparray = calloc(iodesc->maplen, iodesc->piotype_size)))
//            return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);
//    }
//    else
//        tmparray = array;
//
//    /* Rearrange the data. */
//    if ((ierr = rearrange_io2comp(ios, iodesc, iobuf, tmparray)))
//        return pio_err(ios, file, ierr, __FILE__, __LINE__);
//
//    /* Free the buffer. */
//    if (ios->ioproc && rlen > 0)
//        free(iobuf);
//
//    /* If we need to sort the map, do it. */
//    if (iodesc->needssort && ios->compproc)
//    {
//        pio_sorted_copy(tmparray, array, iodesc, 1, 1);
//        free(tmparray);
//    }

    PRINTMSG( "done with GDALc_read_darray()");

    return PIO_NOERR;
  }

}  // end namespace ESMCI
