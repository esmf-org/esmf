#define __PIO_FILE__ "pio.cpp"
// ---------------------------------------------------------------------

//  procedures for translating MPI communicators before calling into Fortran
#include <stdlib.h>
#include <mpi.h>
#include "pio.h"
#include "pio_types.h"

extern "C" {


/////////////////////////////////////////
//
//  the real PIO_init function prototypes
//
/////////////////////////////////////////

// subroutine pio_cpp_init_intracom(comp_rank, comp_comm, num_iotasks, 
//                                  num_aggregator, stride, rearr, iosystem,
//                                  base) bind(c)

void pio_cpp_init_intracom_int(int comp_rank,
                               int comp_comm,
                               int num_tasks,
                               int num_aggregator,
                               int stride,
                               int rearr,
                               pio_iosystem_desc_t *iosystem,
                               int base);

// subroutine pio_cpp_init_intercom(component_count, peer_comm, comp_comms,
//                                  io_comm, iosystem) bind(c)

void pio_cpp_init_intercom_int(int component_count,
                               int peer_comm,
                               int *comp_comms,
                               int io_comm,
                               pio_iosystem_desc_t **iosystem);

/////////////////////////////////
//
//  PIOlib Interface Functions
//
/////////////////////////////////

void pio_cpp_init_intracom(int comp_rank,
                           MPI_Comm comp_comm,
                           int num_tasks,
                           int num_aggregator,
                           int stride,
                           int rearr,
                           pio_iosystem_desc_t *iosystem,
                           int base) {
  pio_cpp_init_intracom_int(comp_rank,
                            MPI_Comm_c2f(comp_comm),
                            num_tasks,
                            num_aggregator,
                            stride,
                            rearr,
                            iosystem,
                            base);
}

void pio_cpp_init_intercom(int component_count,
                           MPI_Comm peer_comm,
                           MPI_Comm* comp_comms,
                           MPI_Comm io_comm,
                           pio_iosystem_desc_t **iosystems) {
  int *int_comp_comms;

  int_comp_comms = (int *)malloc(sizeof(int) * component_count);
  if (int_comp_comms != (int *)NULL) {
    for (int i = 0; i < component_count; i++) {
      int_comp_comms[i] = MPI_Comm_c2f(comp_comms[i]);
    }
    pio_cpp_init_intercom_int(component_count,
                              MPI_Comm_c2f(peer_comm),
                              int_comp_comms,
                              MPI_Comm_c2f(io_comm),
                              iosystems);
  }
}

/////////////////////////////////
//
//  NetCDF Interface Functions
//
/////////////////////////////////

// function pio_inquire(File, nDimensions, nVariables, nAttributes,
//                      unlimitedDimID) result(ierr)

// The real pio_cpp_inquire fortran prototype
int pio_cpp_inquire_int(pio_file_desc_t file, int *nDimensions,
                        int *nVariables, int *nAttributes,
                        int *unlimitedDimID);

int pio_cpp_inquire(pio_file_desc_t file, int *nDimensions, int *nVariables,
                    int *nAttributes, int *unlimitedDimID) {
  int iret = PIO_noerr;

  // pio_inquire uses optional arguments (not allowed in C interface).
  // We fake it out by passing in real arguments and cleaning up later.
  int ndim_local;
  int nvar_local;
  int nattr_local;
  int unlimdim_local;

  iret = pio_cpp_inquire_int(file, &ndim_local, &nvar_local,
                             &nattr_local, &unlimdim_local);
  if ((int *)NULL != nDimensions) {
    *nDimensions = ndim_local;
  }
  if ((int *)NULL != nVariables) {
    *nVariables = nvar_local;
  }
  if ((int *)NULL != nAttributes) {
    *nAttributes = nattr_local;
  }
  if ((int *)NULL != unlimitedDimID) {
    *unlimitedDimID = unlimdim_local;
  }

  return iret;
}

// The Fortran version of this function just calls different routines
// depending on what is passed as optional arguments. We do this straignt
// from C to be able to use NULL entries.
int pio_cpp_inquire_variable_vid(pio_file_desc_t ncid, int varid,
                                 char *name, int *xtype, int *ndims,
                                 int *dimids, int *natts) {
  int iret = PIO_noerr;
  int localrc;

  // The Fortran routine ends up returning the error value of the last
  // function called. We do that unless an earlier function had an error
  // in which case we return that.
  if ((char *)NULL != name) {
    localrc = pio_cpp_inq_varname_vid(ncid, varid, name);
    iret = (localrc == PIO_noerr) ? iret : localrc;
  }
  if ((int *)NULL != ndims) {
    localrc = pio_cpp_inq_varndims_vid(ncid, varid, ndims);
    iret = (localrc == PIO_noerr) ? iret : localrc;
    if ((PIO_noerr == localrc) && ((int *)NULL != dimids)) {
      localrc = pio_cpp_inq_vardimid_vid(ncid, varid, dimids, *ndims);
      iret = (localrc == PIO_noerr) ? iret : localrc;
    }
  }
  if ((int *)NULL != natts) {
    localrc = pio_cpp_inq_varnatts_vid(ncid, varid, natts);
    iret = (localrc == PIO_noerr) ? iret : localrc;
  }
  if ((int *)NULL != xtype) {
    localrc = pio_cpp_inq_vartype_vid(ncid, varid, xtype);
    iret = (localrc == PIO_noerr) ? iret : localrc;
  }

  return iret;
}

// The Fortran version of this function just calls different routines
// depending on what is passed as optional arguments. We do this straignt
// from C to be able to use NULL entries.
int pio_cpp_inquire_variable_vdesc(pio_file_desc_t ncid,
                                   pio_file_desc_t vardesc,
                                   char *name, int *xtype, int *ndims,
                                   int *dimids, int *natts) {
  int iret = PIO_noerr;
  int localrc;

  // The Fortran routine ends up returning the error value of the last
  // function called. We do that unless an earlier function had an error
  // in which case we return that.
  if ((char *)NULL != name) {
    localrc = pio_cpp_inq_varname_vdesc(ncid, vardesc, name);
    iret = (localrc == PIO_noerr) ? iret : localrc;
  }
  if ((int *)NULL != ndims) {
    localrc = pio_cpp_inq_varndims_vdesc(ncid, vardesc, ndims);
    iret = (localrc == PIO_noerr) ? iret : localrc;
    if ((PIO_noerr == localrc) && ((int *)NULL != dimids)) {
      localrc = pio_cpp_inq_vardimid_vdesc(ncid, vardesc, dimids, *ndims);
      iret = (localrc == PIO_noerr) ? iret : localrc;
    }
  }
  if ((int *)NULL != natts) {
    localrc = pio_cpp_inq_varnatts_vdesc(ncid, vardesc, natts);
    iret = (localrc == PIO_noerr) ? iret : localrc;
  }
  if ((int *)NULL != xtype) {
    localrc = pio_cpp_inq_vartype_vdesc(ncid, vardesc, xtype);
    iret = (localrc == PIO_noerr) ? iret : localrc;
  }

  return iret;
}

// The Fortran version of this function just calls different routines
// depending on what is passed as optional arguments. We do this straignt
// from C to be able to use NULL entries.
int pio_cpp_inquire_dimension(pio_file_desc_t ncid, int dimid,
                              char *name, int *len) {
  int iret = PIO_noerr;
  int localrc;

  // The Fortran routine ends up returning the error value of the last
  // function called. We do that unless an earlier function had an error
  // in which case we return that.
  if ((int *)NULL != len) {
    localrc = pio_cpp_inq_dimlen(ncid, dimid, len);
    iret = (localrc == PIO_noerr) ? iret : localrc;
  }
  if ((char *)NULL != name) {
    localrc = pio_cpp_inq_dimname(ncid, dimid, name);
    iret = (localrc == PIO_noerr) ? iret : localrc;
  }

  return iret;
}

// Initialize PIO_IOSYSTEM_DESC_NULL
// NB: This should be the only place outside of pio_kinds.h which knows
//     that an iossytem_desc_t is an integer
static int nullint = -1;
const pio_iosystem_desc_t PIO_IOSYSTEM_DESC_NULL = nullint;

} // extern "C"
