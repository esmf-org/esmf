#ifndef __PIO_H_TYPES_INCLUDED_
#define __PIO_H_TYPES_INCLUDED_

// ---------------------------------------------------------------------
// Constants (parameters) defined in pio_types.F90
// ---------------------------------------------------------------------

// Do not use any form of rearrangement
#define PIO_rearr_none ((int)0)
// Use a PIO internal box rearrangement
#define PIO_rearr_box  ((int)1)

// use MPI-IO with data types to read/write C like binary files
#define PIO_iotype_pbinary        1
// use MPI-IO with data types to read/write direct access binary files
#define PIO_iotype_direct_pbinary 2
// serial read/write of binary files using 'base_node'
#define PIO_iotype_binary         4
// parallel read/write of pNetCDF files
#define PIO_iotype_pnetcdf        5
// serial read/write of NetCDF file using 'base_node'
#define PIO_iotype_netcdf         6
// netcdf4 (hdf5 format) file opened for compression (serial write access only)
#define PIO_iotype_netcdf4c       7
// netcdf4 (hdf5 format) file opened in parallel
// (all netcdf4 files for read will be opened this way)
#define PIO_iotype_netcdf4p       8

// Options for PIO_seterrorhandling
#define PIO_INTERNAL_ERROR      -51
#define PIO_BCAST_ERROR         -52
#define PIO_RETURN_ERROR        -53

// Error values may depend on the I/O package in use
#ifdef _PNETCDF
#include <pnetcdf.h>   /* _EXTERNAL */
#define PIO_global        NC_GLOBAL
#define PIO_unlimited     NC_UNLIMITED
#define PIO_double        NC_DOUBLE
#define PIO_real          NC_FLOAT
#define PIO_int           NC_INT
#define PIO_char          NC_CHAR
#define PIO_noerr         NC_NOERR
#define PIO_WRITE         NC_WRITE
#define PIO_NOWRITE       NC_NOWRITE
#define PIO_CLOBBER       NC_CLOBBER	
#define PIO_NOCLOBBER     NC_NOCLOBBER	
#define PIO_NOFILL        NC_NOFILL
#define PIO_MAX_NAME      NC_MAX_NAME
#define PIO_MAX_VAR_DIMS  NC_MAX_VAR_DIMS
#define PIO_64BIT_OFFSET  NC_64BIT_OFFSET
#define PIO_num_OST       16
#else // _PNETCDF
#ifdef _NETCDF
#include <netcdf.h>
#define PIO_global        NC_GLOBAL
#define PIO_unlimited     NC_UNLIMITED
#define PIO_double        NC_DOUBLE
#define PIO_real          NC_FLOAT
#define PIO_int           NC_INT
#define PIO_char          NC_CHAR
#define PIO_noerr         NC_NOERR
#define PIO_WRITE         NC_WRITE
#define PIO_NOWRITE       NC_NOWRITE
#define PIO_CLOBBER       NC_CLOBBER	
#define PIO_NOCLOBBER     NC_NOCLOBBER	
#define PIO_NOFILL        NC_NOFILL
#define PIO_MAX_NAME      NC_MAX_NAME
#define PIO_MAX_VAR_DIMS  NC_MAX_VAR_DIMS
#define PIO_64BIT_OFFSET  NC_64BIT_OFFSET
#define PIO_num_OST       16
#else // _NETCDF
#define PIO_global         0
#define PIO_double         6
#define PIO_real           5
#define PIO_int            4
#define PIO_char           2
#define PIO_noerr          0
#define PIO_MAX_NAME      25
#define PIO_MAX_VAR_DIMS   6
#define PIO_CLOBBER       10
#define PIO_NOCLOBBER     11
#define PIO_WRITE         20
#define PIO_NOWRITE       21
#define PIO_64BIT_OFFSET   0
#define PIO_num_OST       16
#endif // _NETCDF
#endif // _PNETCDF
#endif // __PIO_H_TYPES_INCLUDED_
