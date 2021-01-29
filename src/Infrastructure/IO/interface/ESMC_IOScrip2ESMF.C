// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
// convert a cubed sphere grid file in SCRIP format NetCDF file into a ESMF
// data format in NetCDF and generate a dual mesh NetCDF file using the center
// coordiates.

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <ctype.h>

#include "ESMC_Conf.h"
#include "ESMCI_Util.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_CoordSys.h"
#include "Mesh/include/ESMCI_ClumpPnts.h"

#ifdef ESMF_NETCDF
#include <netcdf.h>
#endif

#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

#define TOL 0.0000000001

#undef ESMC_METHOD
#define ESMC_METHOD "handle_error"
bool handle_error(int status, int lineno) {
#ifdef ESMF_NETCDF
  char errmsg[128];
  int rc;
  if (status != NC_NOERR) {
    sprintf(errmsg, "line %d: NetCDF error: %s", lineno, nc_strerror(status));
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, errmsg, ESMC_CONTEXT,
      &rc);
    return true;
  } else {
    return false;
  }
#else
  return false;
#endif
}

// order the vertices in celltbl (total number=numedges) in counter-clock wise order
// to find the order of the vertices, we use the anchor vertex (lon,lat) (that is supposed to located
// in the center of the polygon).  We sort the angle of the vector from the anchor vertex
// to each corner vertex in ascending order (assuming 0 to 2PI)
void orderit(int index, double lon, double lat, int numedges, double *latlonbuf, int *next) {
  double *angles, temp, clon, clat;
  int i, j, min, temp1;
  angles = (double*)malloc(sizeof(double)*numedges);

  // When the corner vertices are cross the periodic boundary (0 degree longitude), need to
  // convert the longitudes to be consistent with all the corners and the center
  for (i=0; i< numedges; i++) {
    j=*(next+i)-1;
    clon = latlonbuf[j*2];
    clat = latlonbuf[j*2+1];
    if (fabs(clon-lon) > 180) {
      if (lon >= 180) {
        clon = clon+360;
      } else {
        clon = clon-360;
      }
    }
    //if (latlonbuf[j*2] >= 359.99999) latlonbuf[j*2]=latlonbuf[j*2]-360;
    angles[i] = atan2(clat-lat, clon-lon);
    // change angle to 0 to 2PI degree
    if (clat<lat) {
      angles[i] = 2*M_PI+angles[i];
    }
  }
  // sort angles and keep the order of the index
  // do it in stupid way, loop through the list and find
  // the minimal, use insertion sort
  for (i=0; i<numedges-1; i++) {
    min=i;
    for (j=i+1; j<numedges; j++) {
      if (angles[j]<angles[min]) min=j;
    }
    // swap min and i
    if (min != i) {
      temp=angles[i];
      angles[i]=angles[min];
      angles[min]=temp;
      temp1 = *(next+i);
      *(next+i)=*(next+min);
      *(next+min)=temp1;
    }
  }
  free(angles);
}

void convert3D(double lon, double lat, double *x, double *y) {
  double deg2rad = M_PI/180;
  double lonrad, latrad;
  lonrad = lon*deg2rad;
  latrad = (90-lat)*deg2rad;
  *x = cos(lonrad)*sin(latrad);
  *y = sin(lonrad)*sin(latrad);
}

//If the latitude it above (or below) certain treshold (in the polar region), convert the
// coordinates into a equatorial plane and calculate the angles
void orderit2(int index, double lon, double lat, int numedges, double *latlonbuf, int *next) {
  double *angles, temp;
  int i, j, min, temp1;
  double xcenter, ycenter;
  double xcorner, ycorner;

  angles = (double*)malloc(sizeof(double)*numedges);
  convert3D(lon, lat, &xcenter, &ycenter);

  for (i=0; i< numedges; i++) {
    j=*(next+i)-1;
    convert3D(latlonbuf[j*2], latlonbuf[j*2+1], &xcorner, &ycorner);
    angles[i] = atan2(ycorner-ycenter, xcorner-xcenter);
    // change angle to 0 to 2PI degree
    if (ycorner<ycenter) {
      angles[i] = 2*M_PI+angles[i];
    }
  }
  // if it is in the south pole, reverse the angle
  if (lat < 0) {
    for (i=0; i<numedges; i++) {
      angles[i]=-angles[i];
    }
  }
  // sort angles and keep the order of the index
  // do it in stupid way, loop through the list and find
  // the minimal, use insertion sort
  for (i=0; i<numedges-1; i++) {
    min=i;
    for (j=i+1; j<numedges; j++) {
      if (angles[j]<angles[min]) min=j;
    }
    // swap min and i
    if (min != i) {
      temp=angles[i];
      angles[i]=angles[min];
      angles[min]=temp;
      temp1 = *(next+i);
      *(next+i)=*(next+min);
      *(next+min)=temp1;
    }
  }
  free(angles);
}

#undef ESMC_METHOD
#define ESMC_METHOD "c_nc_create"
extern "C" {
  void FTN_X(c_nc_create)(
                          char *infile,
                          int *mode,
                          ESMC_Logical *largefileflag,
                          ESMC_Logical *netcdf4fileflag,
                          int *ncid,
                          int *rc,
                          ESMCI_FortranStrLenArg infileLen)
  {
    int status;
    int id;
    char *c_infile;

#ifdef ESMF_NETCDF

#ifndef NC_64BIT_OFFSET
    if (*largefileflag == ESMF_TRUE) {
        fprintf(stderr, "ERROR: 64 bit file format is not supported in this version of NetCDF library\n");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, "ERROR: 64 bit file format "
          "is not supported in this version of NetCDF library", ESMC_CONTEXT,
          rc);
        return
    }
#define NC_64BIT_OFFSET 0
#endif

#ifndef NC_NETCDF4
    if (*netcdf4fileflag == ESMF_TRUE) {
        fprintf(stderr, "ERROR: NetCDF4 file format is not supported in this version of NetCDF library\n");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, "ERROR: NetCDF4 file format "
          "is not supported in this version of NetCDF library", ESMC_CONTEXT,
          rc);
        return; //bail out
    }
#define NC_NETCDF4 0
#endif

    *rc = 1;

    // ensure C conform string termination
    c_infile=NULL;
    c_infile=ESMC_F90toCstring(infile,infileLen);
    if (c_infile == NULL) {
      ESMC_LogDefault.MsgAllocError("Fail to allocate input NetCDF filename",
        ESMC_CONTEXT, rc);
      return; // bail out
    }

    if (*largefileflag == ESMF_TRUE) {
      status = nc_create(c_infile, *mode | NC_64BIT_OFFSET, &id);
      if (status == NC_ENOTNC) {
        fprintf(stderr, "ERROR: 64 bit file format is not supported in this version of NetCDF library\n");
        fflush(stderr);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, "ERROR: 64 bit file format "
          "is not supported in this version of NetCDF library", ESMC_CONTEXT,
          rc);
        return; //bail out
      }
      if (handle_error(status,__LINE__)) return; //bail out
    } else if (*netcdf4fileflag == ESMF_TRUE) {
      status = nc_create(c_infile, *mode | NC_NETCDF4, &id);
      if (status == NC_ENOTNC) {
        fprintf(stderr, "ERROR: NetCDF4 file format is not supported in this version of NetCDF library\n");
        fflush(stderr);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, "ERROR: NetCDF4 file format "
          "is not supported in this version of NetCDF library", ESMC_CONTEXT,
          rc);
        return; //bail out
      }
      if (handle_error(status,__LINE__)) return; //bail out
    } else {
      status = nc_create(c_infile, *mode, &id);
      if (handle_error(status,__LINE__)) return; //bail out
    }
    *rc = 0;
    *ncid = id;
    delete [] c_infile;
    return;
#else
  ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT, "Have to compile with "
    "ESMF_NETCDF environment variable defined", ESMC_CONTEXT, rc);
  return;
#endif
  }
}

//--------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "c_convertscrip"
extern "C" {
void FTN_X(c_convertscrip)(
  char *infile,
  char *outfile,
  int *dualflag,
  int *rc,
  ESMCI_FortranStrLenArg infileLen,
  ESMCI_FortranStrLenArg outfileLen)
{
  int ncid1, ncid2;
  int gsdimid, gcdimid, grdimid;
  size_t  gsdim, gcdim, grdim;
  int areaid, ctlatid, ctlonid, colatid, colonid, maskid;
  int status;
  int vertexid, cellid, edgeid, ccoordid, caid, cmid;
  int vertdimid, vpcdimid, vdimid, celldimid;
  int dims[2];
  double *cornerlats, *cornerlons, *nodelatlon;
  double *inbuf, *inbuf1;
  int *inbuf2;
  int *dualcells, *dualcellcounts;
  int *cells, temp[16];
  int numedges, *next;
  int *edges, *totalneighbors;
  int i,i1, i2, j, k, totalnodes, count, fillvalue;
  int goodnodes;
  int noarea, nocenter, nomask;
  const char *strbuf;
  char *strbuf2;
  size_t starts[2], counts[2];
  time_t tloc;
  int maxconnection;
  char *c_infile;
  char *c_outfile;
  char units[80];
  int isRadian = 0;
  size_t len;
  double *nodelons,  *nodelats;
  int totalsize;
  int localrc;

  *rc = 1;
#ifdef ESMF_NETCDF
  // ensure C conform string termination
  c_infile=NULL;
  c_infile=ESMC_F90toCstring(infile,infileLen);
  if (c_infile == NULL) {
    ESMC_LogDefault.MsgAllocError("Fail to allocate input NetCDF filename",
      ESMC_CONTEXT, rc);
    return; // bail out
  }

  c_outfile=NULL;
  c_outfile=ESMC_F90toCstring(outfile,outfileLen);
  if (c_outfile == NULL) {
    ESMC_LogDefault.MsgAllocError("Fail to allocate output NetCDF filename",
      ESMC_CONTEXT, rc);
    return; // bail out
  }

  // Open intput SCRIP file
  status = nc_open(c_infile, NC_NOWRITE, &ncid1);
  if (handle_error(status,__LINE__)) return; // bail out;

  // inquire dimension ids
  status = nc_inq_dimid(ncid1, "grid_size", &gsdimid);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_inq_dimid(ncid1, "grid_corners", &gcdimid);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_inq_dimid(ncid1, "grid_rank", &grdimid);
  if (handle_error(status,__LINE__)) return; // bail out;

  // Get dimension values
  status = nc_inq_dimlen(ncid1, gsdimid, &gsdim);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_inq_dimlen(ncid1, gcdimid, &gcdim);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_inq_dimlen(ncid1, grdimid, &grdim);
  if (handle_error(status,__LINE__)) return; // bail out;

#if 0
  if (grdim > 1) {
    fprintf(stderr, "%s: grid_rank is greater than 1.  This program only convert grids with grid_rank=1.\n",c_infile);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG, "The grid_rank is not "
      "equal 1.", ESMC_CONTEXT, rc);
    return;
  }
#endif

  noarea = 0;
  nocenter = 0;
  nomask = 0;
  // inquire variable ids
  status = nc_inq_varid(ncid1, "grid_area", &areaid);
  if (status != NC_NOERR) noarea = 1;
  status = nc_inq_varid(ncid1, "grid_center_lat", &ctlatid);
  if (status != NC_NOERR) nocenter = 1;
  status = nc_inq_varid(ncid1, "grid_center_lon", &ctlonid);
  if ((status != NC_NOERR && nocenter != 1) || (status == NC_NOERR && nocenter == 1)) {
    fprintf(stderr, "%s: Either grid_center_lat or grid_center_lon does not exist.\n",c_infile);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_FOUND, "Either grid_center_lon "
      "or grid_center_lat does not exist.", ESMC_CONTEXT, rc);
    return;
  }
  status = nc_inq_varid(ncid1, "grid_corner_lat", &colatid);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_inq_varid(ncid1, "grid_corner_lon", &colonid);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_inq_varid(ncid1, "grid_imask", &maskid);
  if (status != NC_NOERR) nomask = 1;

  // read in the corner lat/lon and extract unique node (vertex) list
  cornerlats = (double*)malloc(sizeof(double)*gcdim*gsdim);
  cornerlons = (double*)malloc(sizeof(double)*gcdim*gsdim);
  status = nc_get_var_double(ncid1, colatid, cornerlats);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_get_var_double(ncid1, colonid, cornerlons);
  if (handle_error(status,__LINE__)) return; // bail out;

  // get units of grid_cornor_lon
  status = nc_inq_attlen(ncid1, colonid, "units", &len);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_get_att_text(ncid1, colonid, "units", units);
  if (handle_error(status,__LINE__)) return; // bail out;
  units[len] = '\0';

  // convert radian to degree
  for (i=0; i<len; i++) {
    units[i]=tolower(units[i]);
  }
  if (strncmp(units, "degrees", 7) && strncmp(units, "radians", 7)) {
    fprintf(stderr, "%s: The units attribute for grid_corner_lon is not degrees nor radians.\n",c_infile);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG, "The units attribute for "
      "grid_center_lon is not degrees nor radians.", ESMC_CONTEXT, rc);
    return;
  }
  if (!strncmp(units, "radians", 7)) {
    isRadian=1;
    for (i = 0; i < gcdim*gsdim; i++) {
      cornerlats[i] *= ESMC_CoordSys_Rad2Deg;
      cornerlons[i] *= ESMC_CoordSys_Rad2Deg;
    }
  }

  totalsize = gsdim * gcdim;
  cells = (int*)malloc(sizeof(int)*totalsize);
  ESMCI::ClumpPntsLL(totalsize, cornerlons, cornerlats, TOL, cells, &totalnodes,
                     &nodelons, &nodelats, &maxconnection,-91.0, 91.0, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc,
            "Internal error", ESMC_CONTEXT, rc)) return;


  nodelatlon = (double*)malloc(sizeof(double)*totalnodes*2);
  totalneighbors = (int*)calloc(totalnodes, sizeof(int));
  for (i=0; i<totalnodes; i++) {
    nodelatlon[i*2]=nodelons[i];
    nodelatlon[i*2+1]=nodelats[i];
  }

  free(cornerlons);
  free(cornerlats);
  delete[] nodelons;
  delete[] nodelats;

  for (i=0; i<totalsize; i++) {
    if (cells[i] >= totalnodes) printf("out of bound cells: %d %d\n", i, cells[i]);
    totalneighbors[cells[i]] += 1;
  }
  // check for degenerated cells, remove duplicate nodes and fill cell_edges;
  edges = (int*)malloc(gsdim*sizeof(int));
  for (i=0; i<gsdim; i++) {
    i1=i*gcdim;
    temp[0]=cells[i1];
    count = 1;

    for (j=1; j<gcdim; j++) {
      for (k=0; k<j; k++) {
        if (cells[i1+j]==cells[i1+k]) {
          // the two vertices belong to one cell, over-counted
          totalneighbors[cells[i1+j]]--;
          //  printf("duplicate vertex at %d: vertex %d\n", i, cells[i1+j]);
          break;
        }
      }
      if (k==j) {
        temp[count++]=cells[i1+j];
      }
    }
    // copy temp array back to cell, fill with unfilled space with -1
    if (count < 3) {
      //      printf("degenarate cells index %d, edges %d\n", i, count);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "A cell with less than 3 edges were found", ESMC_CONTEXT, rc);
      return;
    }

    // change 0 based index to 1 based for Fortran
    edges[i]=count;
    for (j=0; j<count; j++) {
      cells[i1+j]=temp[j]+1;
    }
    for (j=count; j<gcdim; j++) {
      cells[i1+j]=-1;
    }
  }
  // find the maximal number of neighbors for all the vertices
  maxconnection = 0;
  for (i=0; i< totalnodes; i++) {
    if (totalneighbors[i]>maxconnection)
      maxconnection=totalneighbors[i];
  }
  //printf("Maximal connection per vertex is %d\n", maxconnection);

  if (*dualflag == 0) {
    // create the output netcdf file
#ifdef NC_NETCDF4
    status = nc_create(c_outfile, NC_CLOBBER|NC_NETCDF4, &ncid2);
    if (status == NC_ENOTNC) {
      status = nc_create(c_outfile, NC_CLOBBER, &ncid2);
    }
    if (handle_error(status,__LINE__)) return; // bail out;
#else
    status = nc_create(c_outfile, NC_CLOBBER, &ncid2);
    if (handle_error(status,__LINE__)) return; // bail out;
#endif

    // define the dimensions
    status = nc_def_dim(ncid2, "nodeCount", totalnodes, &vertdimid);
    if (handle_error(status,__LINE__)) return; // bail out;
    status = nc_def_dim(ncid2, "elementCount", gsdim, &celldimid);
    if (handle_error(status,__LINE__)) return; // bail out;
    status = nc_def_dim(ncid2, "maxNodePElement", gcdim, & vpcdimid);
    if (handle_error(status,__LINE__)) return; // bail out;
    status = nc_def_dim(ncid2, "coordDim", 2L, &vdimid);
    if (handle_error(status,__LINE__)) return; // bail out;

    // define the variables
    dims[0]=vertdimid;
    dims[1]=vdimid;
    status = nc_def_var(ncid2,"nodeCoords", NC_DOUBLE, 2, dims, &vertexid);
    if (handle_error(status,__LINE__)) return; // bail out;
    strbuf = "degrees";
    status = nc_put_att_text(ncid2, vertexid, "units", strlen(strbuf)+1, strbuf);
    if (handle_error(status,__LINE__)) return; // bail out;
    dims[0]=celldimid;
    dims[1]=vpcdimid;
    status = nc_def_var(ncid2,"elementConn", NC_INT, 2, dims, &cellid);
    if (handle_error(status,__LINE__)) return; // bail out;
    strbuf = "Node indices that define the element connectivity";
    status = nc_put_att_text(ncid2, cellid, "long_name", strlen(strbuf)+1, strbuf);
    if (handle_error(status,__LINE__)) return; // bail out;
    fillvalue = -1;
    status = nc_put_att_int(ncid2, cellid, "_FillValue", NC_INT, 1, &fillvalue);
    if (handle_error(status,__LINE__)) return; // bail out;
    status = nc_def_var(ncid2,"numElementConn", NC_INT, 1, dims, &edgeid);
    if (handle_error(status,__LINE__)) return; // bail out;
    strbuf = "Number of nodes per element";
    status = nc_put_att_text(ncid2, edgeid, "long_name", strlen(strbuf)+1, strbuf);
    if (handle_error(status,__LINE__)) return; // bail out;
    if (!nocenter) {
      dims[0]=celldimid;
      dims[1]=vdimid;
      status = nc_def_var(ncid2, "centerCoords", NC_DOUBLE, 2, dims, &ccoordid);
      if (handle_error(status,__LINE__)) return; // bail out;
      strbuf = "degrees";
      status = nc_put_att_text(ncid2, ccoordid, "units", strlen(strbuf)+1, strbuf);
      if (handle_error(status,__LINE__)) return; // bail out;
    }
    if (!noarea) {
      status = nc_def_var(ncid2, "elementArea", NC_DOUBLE, 1, dims, &caid);
      if (handle_error(status,__LINE__)) return; // bail out;
      // copy the units and long_name attributes if they exist in the input file
      int attid;
      status = nc_inq_attid(ncid1, areaid, "units", &attid);
      if (status == NC_NOERR) {
        status = nc_copy_att(ncid1, areaid, "units", ncid2, caid);
        if (handle_error(status,__LINE__)) return; // bail out;
      }
      status = nc_inq_attid(ncid1, areaid, "long_name", &attid);
      if (status == NC_NOERR) {
        status = nc_copy_att(ncid1, areaid, "long_name", ncid2, caid);
        if (handle_error(status,__LINE__)) return; // bail out;
      }
    }
    if (!nomask) {
      status = nc_def_var(ncid2, "elementMask", NC_INT, 1, dims, &cmid);
      if (handle_error(status,__LINE__)) return; // bail out;
      // status = nc_copy_att(ncid1, maskid, "_FillValue", ncid2, cmid);
      // if (handle_error(status,__LINE__)) return; // bail out;
    }

    // Global Attribute
    strbuf = "unstructured";
    status = nc_put_att_text(ncid2, NC_GLOBAL, "gridType", strlen(strbuf), strbuf);
    if (handle_error(status,__LINE__)) return; // bail out;
    strbuf = "0.9";
    status = nc_put_att_text(ncid2, NC_GLOBAL, "version", strlen(strbuf), strbuf);
    if (handle_error(status,__LINE__)) return; // bail out;
    status = nc_put_att_text(ncid2, NC_GLOBAL, "inputFile", strlen(c_infile), c_infile);
    if (handle_error(status,__LINE__)) return; // bail out;
    time(&tloc);
    strbuf2 = ctime(&tloc);
    strbuf2[strlen(strbuf2)-1] = '\0';
    status = nc_put_att_text(ncid2, NC_GLOBAL, "timeGenerated", strlen(strbuf2), strbuf2);
    if (handle_error(status,__LINE__)) return; // bail out;

    status=nc_enddef(ncid2);
    if (handle_error(status,__LINE__)) return; // bail out;

    nc_put_var_double(ncid2, vertexid, nodelatlon);
    if (handle_error(status,__LINE__)) return; // bail out;
    nc_put_var_int(ncid2, cellid, cells);
    if (handle_error(status,__LINE__)) return; // bail out;

    nc_put_var_int(ncid2, edgeid, edges);
    if (handle_error(status,__LINE__)) return; // bail out;


    free(edges);
    free(cells);
    free(nodelatlon);

    inbuf = (double*)malloc(sizeof(double)*gsdim);
    if (!nocenter) {
      inbuf1 = (double*)malloc(sizeof(double)*gsdim*2);
      status = nc_get_var_double(ncid1, ctlatid, inbuf);
      if (handle_error(status,__LINE__)) return; // bail out;
      // copy inbuf to inbuf1
      for (i=0; i<gsdim; i++) {
        inbuf1[i*2+1]=inbuf[i];
      }
      status = nc_get_var_double(ncid1, ctlonid, inbuf);
      if (handle_error(status,__LINE__)) return; // bail out;
      // copy inbuf to inbuf1
      for (i=0; i<gsdim; i++) {
        inbuf1[i*2]=inbuf[i];
      }
      // get units of grid_center_lon
      status = nc_inq_attlen(ncid1, ctlonid, "units", &len);
      if (handle_error(status,__LINE__)) return; // bail out;
      status = nc_get_att_text(ncid1, ctlonid, "units", units);
      if (handle_error(status,__LINE__)) return; // bail out;
      units[len] = '\0';
      // convert radian to degree
      for (i=0; i<len; i++) {
        units[i]=tolower(units[i]);
      }
      if (strncmp(units, "degrees", 7) && strncmp(units, "radians", 7)) {
          fprintf(stderr, "%s: The units attribute for grid_center_lon is not degrees nor radians.\n", c_infile);
          ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG, "The units "
            "attribute for grid_center_lon is not degrees nor radians.",
            ESMC_CONTEXT, rc);
          return;
      }
      if (!strncmp(units, "radians", 7)) {
        for (i=0; i<gsdim*2; i++) {
          inbuf1[i] *= ESMC_CoordSys_Rad2Deg;
        }
      }
      starts[0]=0;
      starts[1]=0;
      counts[0]=gsdim;
      counts[1]=2;
      status = nc_put_vara_double(ncid2, ccoordid, starts, counts, inbuf1);
      if (handle_error(status,__LINE__)) return; // bail out;
      free(inbuf1);
    }
    if (!noarea) {
      status = nc_get_var_double(ncid1, areaid, inbuf);
      if (handle_error(status,__LINE__)) return; // bail out;
      status = nc_put_var_double(ncid2, caid, inbuf);
      if (handle_error(status,__LINE__)) return; // bail out;
    }
    free(inbuf);
    if (!nomask) {
      inbuf2=(int*)malloc(sizeof(int)*gsdim);
      status = nc_get_var_int(ncid1, maskid, inbuf2);
      if (handle_error(status,__LINE__)) return; // bail out;
      status = nc_put_var_int(ncid2, cmid, inbuf2);
      if (handle_error(status,__LINE__)) return; // bail out;
      free(inbuf2);
    }
    nc_close(ncid1);
    nc_close(ncid2);
    free(totalneighbors);
    delete [] c_infile;
    delete [] c_outfile;
    *rc = 0;
    return;
  }
  // Now create the dual mesh using the cell coordinates.  The
  // format is the same except that the num_verts = the original num_cells (gsdim),
  // vert_coords will be the original center-coords. num_cells = the original
  // num_verts (totalnodes) mask is not
  // changed, and cell_verts will be generated here
  //
  // for each vert in the original grid, find out which cell uses it, use the
  // the center of the cells to form a new cell
  // The dual mesh should have equal number of cells and vertices
  //
  // celltbl = (int*)malloc(sizeof(int)*maxconnection);
  // the new dual mesh may not have the same topology as the original mesh
  // it depends on how many edges are sharing a specific vertices
  // so, this has to be calculated as well

  // First, read in the center coordinates
  free(edges);
  inbuf = (double*)malloc(sizeof(double)*gsdim);
  inbuf1 = (double*)malloc(sizeof(double)*gsdim*2);
  status = nc_get_var_double(ncid1, ctlatid, inbuf);
  if (handle_error(status,__LINE__)) return; // bail out;
  // copy inbuf to inbuf1
  for (i=0; i<gsdim; i++) {
    inbuf1[i*2+1]=inbuf[i];
  }
  status = nc_get_var_double(ncid1, ctlonid, inbuf);
  if (handle_error(status,__LINE__)) return; // bail out;
  // copy inbuf to inbuf1
  for (i=0; i<gsdim; i++) {
    inbuf1[i*2]=inbuf[i];
  }
  // get units of grid_center_lon
  status = nc_inq_attlen(ncid1, ctlonid, "units", &len);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_get_att_text(ncid1, ctlonid, "units", units);
  if (handle_error(status,__LINE__)) return; // bail out;
  units[len]='\0';
  // convert radian to degree
  for (i=0; i<len; i++) {
    units[i]=tolower(units[i]);
  }
  if (strncmp(units, "degrees", 7) && strncmp(units, "radians", 7)) {
    fprintf(stderr, "%s: The units attribute for grid_center_lon is not degrees nor radians.\n", c_infile);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG,"The units attribute for "
      "grid_center_lon is not degrees nor radians.", ESMC_CONTEXT, rc);
    return;
  }
  if (!strncmp(units, "radians", 7)) {
    for (i=0; i<gsdim*2; i++) {
      inbuf1[i] *= ESMC_CoordSys_Rad2Deg;
    }
  }
  free(inbuf);

  dualcellcounts = (int*)malloc(sizeof(int)*totalnodes);
  dualcells = (int*)malloc(sizeof(int)*maxconnection*totalnodes);
  for (i=0; i<totalnodes; i++)
    dualcellcounts[i]=0;

  // initialize the values to -1
  for (i=0; i<maxconnection*totalnodes; i++)
    dualcells[i]=-1;

  // go through the cells table and count the edges for each dual cell
  for (i=0,k=0; i<gsdim; i++) {
    for (j=0; j<gcdim; j++,k++) {
      if (cells[k] < 0) continue;
      i1 = cells[k]-1;
      dualcells[i1*maxconnection+dualcellcounts[i1]]=i+1;
      dualcellcounts[i1]++;
      if (dualcellcounts[i1] > maxconnection) {
        fprintf(stderr, "Vertex %d exceed maximal connections %d\n", i1, maxconnection);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          "Unrecoveable internal error", ESMC_CONTEXT, rc);
        return; // bail out
      }
    }
  }

  // remove the cells with less than 3 edges in dualcells table
  // also remove them from the node coordinates table and the totalneighbors table
  for (i=0, i1=0; i<totalnodes; i++) {
    if (dualcellcounts[i] >= 3) {
      if (i1 != i) {
        for (k=0; k<maxconnection; k++) {
          dualcells[i1*maxconnection+k]=dualcells[i*maxconnection+k];
	}
	totalneighbors[i1]=totalneighbors[i];
	nodelatlon[i1*2]=nodelatlon[i*2];
	nodelatlon[i1*2+1]=nodelatlon[i*2+1];
      }
      i1++;
    }
  }

  goodnodes = i1;
  //printf("Total nodes: %d, total non-degenerated nodes: %d\n", totalnodes, goodnodes);

  // order the cell center coordinates in counter-clockwise order
  // lonbuf and latbuf contains the center vertex coordinates
  // next points to the cell_vertex location where we will fill
  // in the cell id in counter clockwise order
  for (i = 0; i < goodnodes; i++) {
      next = &dualcells[i*maxconnection];
      numedges = totalneighbors[i];
      if (fabs(nodelatlon[i*2+1]) > 88.0) {
        orderit2(i+1, nodelatlon[i*2], nodelatlon[i*2+1], numedges, inbuf1,next);
      } else {
        orderit(i+1, nodelatlon[i*2], nodelatlon[i*2+1], numedges, inbuf1,next);
      }
  }

  free(dualcellcounts);
  // now write out the dual mesh in a netcdf file
  // create the output netcdf file

#ifdef NC_NETCDF4
  status = nc_create(c_outfile, NC_CLOBBER|NC_NETCDF4, &ncid2);
  if (status == NC_ENOTNC) {
    status = nc_create(c_outfile, NC_CLOBBER, &ncid2);
  }
  if (handle_error(status,__LINE__)) return; // bail out;
#else
  status = nc_create(c_outfile, NC_CLOBBER, &ncid2);
  if (handle_error(status,__LINE__)) return; // bail out;
#endif

  // define the dimensions
  status = nc_def_dim(ncid2, "nodeCount", gsdim, &vertdimid);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_def_dim(ncid2, "elementCount", goodnodes, &celldimid);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_def_dim(ncid2, "maxNodePElement", maxconnection, & vpcdimid);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_def_dim(ncid2, "coordDim", 2L, &vdimid);
  if (handle_error(status,__LINE__)) return; // bail out;

  // define the variables
  dims[0]=vertdimid;
  dims[1]=vdimid;
  status = nc_def_var(ncid2,"nodeCoords", NC_DOUBLE, 2, dims, &vertexid);
  if (handle_error(status,__LINE__)) return; // bail out;
  strbuf = "degrees";
  status = nc_put_att_text(ncid2, vertexid, "units", strlen(strbuf)+1, strbuf);
  if (handle_error(status,__LINE__)) return; // bail out;
  dims[0]=celldimid;
  dims[1]=vpcdimid;
  status = nc_def_var(ncid2,"elementConn", NC_INT, 2, dims, &cellid);
  if (handle_error(status,__LINE__)) return; // bail out;
  strbuf = "Node indices that define the element connectivity";
  status = nc_put_att_text(ncid2, cellid, "long_name", strlen(strbuf), strbuf);
  if (handle_error(status,__LINE__)) return; // bail out;
  fillvalue = -1;
  status = nc_put_att_int(ncid2, cellid, "_FillValue", NC_INT, 1, &fillvalue);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_def_var(ncid2,"numElementConn", NC_INT, 1, dims, &edgeid);
  if (handle_error(status,__LINE__)) return; // bail out;
  strbuf = "Number of nodes per element";
  status = nc_put_att_text(ncid2, edgeid, "long_name", strlen(strbuf), strbuf);
  if (handle_error(status,__LINE__)) return; // bail out;

  // defind node mask
  dims[0]=vertdimid;
  if (!nomask) {
    status = nc_def_var(ncid2, "nodeMask", NC_INT, 1, dims, &cmid);
    if (handle_error(status,__LINE__)) return; // bail out;
    // status = nc_copy_att(ncid1, maskid, "_FillValue", ncid2, cmid);
    // if (handle_error(status,__LINE__)) return; // bail out;
  }

  // Global Attribute
  strbuf = "unstructured";
  status = nc_put_att_text(ncid2, NC_GLOBAL, "gridType", strlen(strbuf), strbuf);
  if (handle_error(status,__LINE__)) return; // bail out;
  strbuf = "0.9";
  status = nc_put_att_text(ncid2, NC_GLOBAL, "version", strlen(strbuf), strbuf);
  if (handle_error(status,__LINE__)) return; // bail out;
  status = nc_put_att_text(ncid2, NC_GLOBAL, "inputFile", strlen(c_infile), c_infile);
  if (handle_error(status,__LINE__)) return; // bail out;
  strbuf="Dual mesh generated using the cell center coordinates";
  status = nc_put_att_text(ncid2, NC_GLOBAL, "description", strlen(strbuf), strbuf);
  if (handle_error(status,__LINE__)) return; // bail out;
  time(&tloc);
  strbuf2 = ctime(&tloc);
  strbuf2[strlen(strbuf2)-1] = '\0';
  status = nc_put_att_text(ncid2, NC_GLOBAL, "timeGenerated", strlen(strbuf2), strbuf2);
  if (handle_error(status,__LINE__)) return; // bail out;

  nc_enddef(ncid2);
  if (!nomask) {
    inbuf2=(int*)malloc(sizeof(int)*gsdim);
    status = nc_get_var_int(ncid1, maskid, inbuf2);
    if (handle_error(status,__LINE__)) return; // bail out;
    status = nc_put_var_int(ncid2, cmid, inbuf2);
    if (handle_error(status,__LINE__)) return; // bail out;
    free(inbuf2);
  }
  nc_put_var_double(ncid2, vertexid, inbuf1);
  if (handle_error(status,__LINE__)) return; // bail out;
  nc_put_var_int(ncid2, cellid, dualcells);
  if (handle_error(status,__LINE__)) return; // bail out;
  nc_put_var_int(ncid2, edgeid, totalneighbors);
  if (handle_error(status,__LINE__)) return; // bail out;

  free(totalneighbors);
  free(cells);
  free(dualcells);
  free(nodelatlon);
  free(inbuf1);
  nc_close(ncid2);
  nc_close(ncid1);
  delete [] c_infile;
  delete [] c_outfile;

  *rc = 0;
  return;

#else
  ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB_NOT_PRESENT, "Have to compile with "
    "ESMF_NETCDF environment variable defined", ESMC_CONTEXT, rc);
  return;
#endif
}
}
