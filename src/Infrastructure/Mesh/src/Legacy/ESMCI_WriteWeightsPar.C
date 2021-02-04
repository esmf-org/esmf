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
#include <Mesh/include/Legacy/ESMCI_WriteWeightsPar.h>

#include <Mesh/include/Regridding/ESMCI_Interp.h>
#include <Mesh/include/Regridding/ESMCI_MeshRegrid.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/Legacy/ESMCI_Migrator.h>
#include <ESMC_Macros.h>

#ifdef ESMF_PNETCDF
#include <pnetcdf.h>
typedef MPI_Offset MPI_OffType;
#else
typedef long long MPI_OffType;
#endif

#include <limits>
#include <time.h>


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

void GatherForWrite(IWeights &w) {
  // Strategy: find min/max row.  Assume indices are somewhat 
  // uniformly distributed.
  
  int nproc = Par::Size(), rank = Par::Rank();
  
  long max_row, min_row, lmin_row, lmax_row;
  
  lmin_row = min_row = std::numeric_limits<long>::max();
  lmax_row = max_row = -std::numeric_limits<long>::max();
  
  IWeights::WeightMap::iterator ri = w.begin_row(), re = w.end_row();
  
  if (ri != re) {
    lmin_row = ri->first.id;
    lmax_row = (--IWeights::WeightMap::iterator(re))->first.id;
  }
  
  MPI_Allreduce(&lmin_row, &min_row, 1, MPI_LONG, MPI_MIN, Par::Comm()); 
  MPI_Allreduce(&lmax_row, &max_row, 1, MPI_LONG, MPI_MAX, Par::Comm());
  
  if (min_row > max_row) return; // must be empty
  
  const int rows_per_proc = (max_row-min_row+1+nproc-1) / nproc;
  
  Par::Out() << "min_row=" << min_row << ", max_row=" << max_row << ", rpp=" << rows_per_proc << std::endl;
  
  // Setup the new distribution indices
  std::vector<UInt> new_dist, iw_dist;
  new_dist.reserve(rows_per_proc);
  for (UInt i = 0; i < rows_per_proc; i++) {
    new_dist.push_back(min_row + rank*rows_per_proc + i);
  }
  /*
  Par::Out() << "New row indices: size=" << new_dist.size() << std::endl;
  std::copy(new_dist.begin(), new_dist.end(), std::ostream_iterator<UInt>(Par::Out(), "\n"));
  Par::Out() << "end" << std::endl;
  */

  w.GetRowGIDS(iw_dist);

  Migrator mig(new_dist.size(), new_dist.size() > 0 ? &new_dist[0] : NULL, 0,
        iw_dist.size(), iw_dist.size() > 0 ? &iw_dist[0] : NULL);
    
  mig.Migrate(static_cast<WMat&>(w));
  
}


// Slurp a netcdf grid definition file into a struct.
struct nc_grid_file1 {
int grid_size;
int grid_corners;
int grid_rank;
MPI_OffType local_grid_start;
MPI_OffType local_grid_size;

std::vector<int> grid_dims;
std::vector<double> grid_center_lat;
bool lat_deg;
std::vector<double> grid_center_lon;
bool lon_deg;
std::vector<int> grid_imask;
std::vector<double> grid_corner_lat;
bool corner_lon_deg;
std::vector<double> grid_corner_lon;
bool corner_lat_deg;
/*
 * Free memory.
 */
void clear() {
  std::vector<double>().swap(grid_center_lat);
  std::vector<double>().swap(grid_center_lon);
  std::vector<double>().swap(grid_corner_lat);
  std::vector<double>().swap(grid_corner_lon);
  std::vector<int>().swap(grid_dims);
  std::vector<int>().swap(grid_imask);
  
}

};

static void nc_grid_file1_2deg(nc_grid_file1 &ncf) {
  
  const double rad2deg = 180.0/M_PI;
  
  for (UInt i = 0; i < ncf.local_grid_size; i++) {
    if (!ncf.lat_deg)
      ncf.grid_center_lat[i] *= rad2deg;
    if (!ncf.lon_deg)
      ncf.grid_center_lon[i] *= rad2deg;
    
    for (UInt k = 0; k < ncf.grid_corners; k++) {
      if (!ncf.corner_lat_deg)
        ncf.grid_corner_lat[i*ncf.grid_corners+k] *= rad2deg;
      if (!ncf.corner_lon_deg)
        ncf.grid_corner_lon[i*ncf.grid_corners+k] *= rad2deg;
    }
  }
  
  ncf.lat_deg = ncf.lon_deg = ncf.corner_lat_deg = ncf.corner_lon_deg = true;
}

static void get_nc_grid_file1(nc_grid_file1 &ncf, const std::string &ncfile, bool header_only) {
#ifdef ESMF_PNETCDF

  int ncid, stat;


  if ((stat = ncmpi_open(Par::Comm(), ncfile.c_str(), NC_NOWRITE, MPI_INFO_NULL, &ncid)) != NC_NOERR) {
    Throw() << "Trouble opening " << ncfile << ", ncerr=" <<
               ncmpi_strerror(stat);
  }

  MPI_OffType n, grid_rank, grid_corners;

  // Get ni, nj; verify nv=4
  int dimid;
  // n
  if ((stat = ncmpi_inq_dimid(ncid, "grid_size", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  ncmpi_inq_dimlen(ncid, dimid, &n);
  
  ncf.grid_size = n;
 
  // ni
  if ((stat = ncmpi_inq_dimid(ncid, "grid_rank", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  ncmpi_inq_dimlen(ncid, dimid, &grid_rank);
  
  ncf.grid_rank = grid_rank;

  ThrowRequire(grid_rank == 2); // for now

  // nv
  if ((stat = ncmpi_inq_dimid(ncid, "grid_corners", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  ncmpi_inq_dimlen(ncid, dimid, &grid_corners);

  ncf.grid_corners = grid_corners;

  if (grid_corners != 4) Throw() << "unexpected nv=" << grid_corners << "; should be 4";

  // Get the grid dims
  int gdimid;
  if ((stat = ncmpi_inq_varid(ncid, "grid_dims", &gdimid)) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }

  
  int grid_dims[2];
  
  ncmpi_get_var_int_all(ncid, gdimid, &grid_dims[0]);
  
  ncf.grid_dims.resize(2);
  std::copy(&grid_dims[0], &grid_dims[2], ncf.grid_dims.begin()); 

  // Note:Read in grid dims as well, above.
  if (header_only) {
    ncmpi_close(ncid);
    ncf.lat_deg = ncf.lon_deg = ncf.corner_lat_deg = ncf.corner_lon_deg = true;
    return;
  }
  
  
  // Figure out which part I will read
  long min_row, max_row, my_start, my_num;
  
  decomp1d(grid_dims[1] - 1, Par::Size(), Par::Rank(), my_num, my_start);
  
  min_row = my_start;
  max_row = min_row + my_num;
  
  MPI_OffType local_grid_size;
  MPI_OffType local_grid_start;
  
  if (my_num == 0) {
    local_grid_size = local_grid_start = 0;
  } else {
    local_grid_size = (my_num+1)*grid_dims[0];
    local_grid_start = my_start*grid_dims[0];
  }
  
  ncf.local_grid_size = local_grid_size;
  ncf.local_grid_start = local_grid_start;
  
  int local_grid_dims[2] = {grid_dims[0], my_num > 0 ? my_num+1 : 0};

  Par::Out() << "min_row=" << min_row << ", max_row=" << max_row << std::endl;
  Par::Out() << "local_grid_start=" << local_grid_start << ", local_grid_size:" << local_grid_size << std::endl;

  ncf.grid_center_lat.resize(ncf.local_grid_size);
  ncf.grid_center_lon.resize(ncf.local_grid_size);

  ncf.grid_corner_lat.resize(ncf.local_grid_size*ncf.grid_corners);
  ncf.grid_corner_lon.resize(ncf.local_grid_size*ncf.grid_corners);
  
  MPI_OffType starts[] = {ncf.local_grid_start, 0};
  MPI_OffType counts[] = {ncf.local_grid_size, 4};
  
  int gclatid, gclonid, gcrnrlatid, gcrnrlonid, imaskid;
  if ((stat = ncmpi_inq_varid(ncid, "grid_center_lon", &gclonid)) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  if ((stat = ncmpi_inq_varid(ncid, "grid_center_lat", &gclatid)) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  
  if ((stat = ncmpi_inq_varid(ncid, "grid_corner_lon", &gcrnrlonid)) != NC_NOERR) {
     Throw() << "NCErr:" << ncmpi_strerror(stat);
   }
   if ((stat = ncmpi_inq_varid(ncid, "grid_corner_lat", &gcrnrlatid)) != NC_NOERR) {
     Throw() << "NCErr:" << ncmpi_strerror(stat);
   }
   
   if ((stat = ncmpi_inq_varid(ncid, "grid_imask", &imaskid)) != NC_NOERR) {
     Throw() << "NCErr:" << ncmpi_strerror(stat);
   }
  
  // Get units
  MPI_OffType gclonattlen, gclatattlen, gcrnrlatattlen, gcrnrlonattlen;
  bool latdeg = false, londeg = false, crnrlatdeg = false, crnrlondeg = false;
  char attbuf[1024];
  
  // Get lens
  if ((stat = ncmpi_inq_attlen(ncid, gclatid, "units", &gclatattlen)) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  if ((stat = ncmpi_inq_attlen(ncid, gclonid, "units", &gclonattlen)) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  if ((stat = ncmpi_inq_attlen(ncid, gcrnrlatid, "units", &gcrnrlatattlen)) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  if ((stat = ncmpi_inq_attlen(ncid, gcrnrlonid, "units", &gcrnrlonattlen)) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  
  
  // Get text
  if ((stat = ncmpi_get_att_text(ncid, gclatid, "units", &attbuf[0])) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  attbuf[gclatattlen] = '\0';
  if (std::string(attbuf) == std::string("degrees")) latdeg = true;
  
  if ((stat = ncmpi_get_att_text(ncid, gclonid, "units", &attbuf[0])) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  attbuf[gclonattlen] = '\0';
  if (std::string(attbuf) == std::string("degrees")) londeg = true;
  
  
  if ((stat = ncmpi_get_att_text(ncid, gcrnrlatid, "units", &attbuf[0])) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  attbuf[gcrnrlatattlen] = '\0';
  if (std::string(attbuf) == std::string("degrees")) crnrlatdeg = true;
  
  if ((stat = ncmpi_get_att_text(ncid, gcrnrlonid, "units", &attbuf[0])) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  attbuf[gcrnrlonattlen] = '\0';
  if (std::string(attbuf) == std::string("degrees")) crnrlondeg = true;
  
  
  ncf.lat_deg = latdeg;
  ncf.lon_deg = londeg;
  ncf.corner_lat_deg = crnrlatdeg;
  ncf.corner_lon_deg = crnrlondeg;

  /*-------------------------------------------------------------------*/
  // Read in the data
  /*-------------------------------------------------------------------*/
  
  // centers
  if ((stat = ncmpi_get_vara_double_all(ncid, gclatid, starts, counts, &ncf.grid_center_lat[0])) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  
  if ((stat = ncmpi_get_vara_double_all(ncid, gclonid, starts, counts, &ncf.grid_center_lon[0])) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  
  // corners
  if ((stat = ncmpi_get_vara_double_all(ncid, gcrnrlatid, starts, counts, &ncf.grid_corner_lat[0])) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  
  if ((stat = ncmpi_get_vara_double_all(ncid, gcrnrlonid, starts, counts, &ncf.grid_corner_lon[0])) != NC_NOERR) {
    Throw() << "NCErr:" << ncmpi_strerror(stat);
  }
  
  ncf.grid_imask.resize(ncf.local_grid_size);
  if ((stat = ncmpi_get_vara_int_all(ncid, imaskid, starts, counts, &ncf.grid_imask[0])) != NC_NOERR) {
     Throw() << "NCErr:" << ncmpi_strerror(stat);
   }
  
  ncmpi_close(ncid);
  
#endif
}

void WriteNCMatFilePar(const std::string &src_ncfile,
                    const std::string &dst_ncfile,
                    const std::string &outfile,
                    const IWeights &w,
                    Mesh &srcmesh,
                    Mesh &dstmesh,
                    Mesh &dstmeshcpy,
                    int *regridConserve,
                    int *regridMethod,
                    int ordering)
{
  Trace __trace("WriteNCMatFilePar(const std::string &src_ncfile, const std::string &dst_ncfile, const std::string &outfile, const IWeights &w, int ordering"); 
#ifdef ESMF_PNETCDF

  std::pair<int,int> pa = w.count_matrix_entries();
  int ln_s = pa.first;
  int lmax_idx = pa.second;
  int local_start_n_s;
  int max_idx, n_s;
  
  Par::Out() << "l_ns=" << ln_s << std::endl;
  MPI_Scan(&ln_s, &local_start_n_s, 1, MPI_INT, MPI_SUM, Par::Comm());
  local_start_n_s -= ln_s; // Scan adds the current proc value
  Par::Out() << "local_start_n_s=" << local_start_n_s << std::endl;
  
  MPI_Allreduce(&ln_s, &n_s, 1, MPI_INT, MPI_SUM, Par::Comm());
  MPI_Allreduce(&lmax_idx, &max_idx, 1, MPI_INT, MPI_MAX, Par::Comm());
  
  // Global reduction.  Each proc needs to know the n_s to attain
  
  // Open the src/out files, header only to begin
  nc_grid_file1 ncsrc, ncdst;
  
  get_nc_grid_file1(ncsrc, src_ncfile, true);
  
  get_nc_grid_file1(ncdst, dst_ncfile, true);
  
  int n_a = ncsrc.grid_size, n_b = ncdst.grid_size;
  
  // Open the netcdf output file

  // open the netcdf file
   int ncid, stat;
   if ((stat = ncmpi_create(Par::Comm(), outfile.c_str(), NC_CLOBBER, MPI_INFO_NULL, &ncid)) != NC_NOERR) {
     Throw() << "Trouble opening " << outfile << ", ncerr=" <<
                ncmpi_strerror(stat);
   }
   int retval;
   
   int n_adimid, n_bdimid, nv_adimid, nv_bdimid, src_grid_rankdimid, dst_grid_rankdimid,
     ni_adimid, nj_adimid, ni_bdimid, nj_bdimid, n_sdimid, num_wgtsdimid, n_kdimid;

   
   // Define dimensions
   if ((retval = ncmpi_def_dim(ncid, "n_a", ncsrc.grid_size, &n_adimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_dim(ncid, "n_b", ncdst.grid_size, &n_bdimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_dim(ncid, "nv_a", ncsrc.grid_corners, &nv_adimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   
   if ((retval = ncmpi_def_dim(ncid, "nv_b", ncdst.grid_corners, &nv_bdimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_dim(ncid, "src_grid_rank", ncsrc.grid_rank, &src_grid_rankdimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_dim(ncid, "dst_grid_rank", ncdst.grid_rank, &dst_grid_rankdimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_dim(ncid, "ni_a", ncsrc.grid_dims[0], &ni_adimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_dim(ncid, "nj_a", ncsrc.grid_dims[1], &nj_adimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_dim(ncid, "ni_b", ncdst.grid_dims[0], &ni_bdimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_dim(ncid, "nj_b", ncdst.grid_dims[1], &nj_bdimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_dim(ncid, "n_s", n_s, &n_sdimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_dim(ncid, "num_wgts", int(1), &num_wgtsdimid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if (max_idx > 0)
     if ((retval = ncmpi_def_dim(ncid, "n_k", (max_idx + 1), &n_kdimid)))
       Throw() << "NC error:" << ncmpi_strerror(retval);


   /*-------------------------------------------------------------------------*/
   // Define the variables
   /*-------------------------------------------------------------------------*/
   
   int src_grid_dimsid, dst_grid_dimsid, yc_aid, yc_bid, xc_aid, xc_bid,
       yv_aid, yv_bid, xv_aid, xv_bid, mask_aid, mask_bid, area_aid, area_bid,
       frac_aid, frac_bid, colid, rowid, Sid;
   
   std::string units;

   int dim_scratch[2];
   
   if ((retval = ncmpi_def_var(ncid, "src_grid_dims", NC_INT, 1, &src_grid_rankdimid, &src_grid_dimsid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_var(ncid, "dst_grid_dims", NC_INT, 1, &dst_grid_rankdimid, &dst_grid_dimsid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_var(ncid, "yc_a", NC_DOUBLE, 1, &n_adimid, &yc_aid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);  
   units = ncsrc.lat_deg ? "degrees" : "radians";
   if ((retval = ncmpi_put_att_text(ncid, yc_aid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_var(ncid, "yc_b", NC_DOUBLE, 1, &n_bdimid, &yc_bid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   units = ncdst.lat_deg ? "degrees" : "radians";
   if ((retval = ncmpi_put_att_text(ncid, yc_bid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_var(ncid, "xc_a", NC_DOUBLE, 1, &n_adimid, &xc_aid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   units = ncsrc.lon_deg ? "degrees" : "radians";
   if ((retval = ncmpi_put_att_text(ncid, xc_aid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_var(ncid, "xc_b", NC_DOUBLE, 1, &n_bdimid, &xc_bid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   units = ncdst.lon_deg ? "degrees" : "radians";
   if ((retval = ncmpi_put_att_text(ncid, xc_bid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << ncmpi_strerror(retval);
   
   dim_scratch[0] = n_adimid; dim_scratch[1] = nv_adimid;
   if ((retval = ncmpi_def_var(ncid, "yv_a", NC_DOUBLE, 2, &dim_scratch[0], &yv_aid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   units = ncsrc.corner_lat_deg ? "degrees" : "radians";
   if ((retval = ncmpi_put_att_text(ncid, yv_aid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_var(ncid, "xv_a", NC_DOUBLE, 2, &dim_scratch[0], &xv_aid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   units = ncsrc.corner_lon_deg ? "degrees" : "radians";
   if ((retval = ncmpi_put_att_text(ncid, xv_aid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << ncmpi_strerror(retval);
   
   dim_scratch[0] = n_bdimid; dim_scratch[1] = nv_bdimid;
   if ((retval = ncmpi_def_var(ncid, "yv_b", NC_DOUBLE, 2, &dim_scratch[0], &yv_bid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   units = ncdst.corner_lat_deg ? "degrees" : "radians";
   if ((retval = ncmpi_put_att_text(ncid, yv_bid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_var(ncid, "xv_b", NC_DOUBLE, 2, &dim_scratch[0], &xv_bid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   units = ncdst.corner_lon_deg ? "degrees" : "radians";
   if ((retval = ncmpi_put_att_text(ncid, xv_bid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_var(ncid, "mask_a", NC_INT, 1, &n_adimid, &mask_aid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   units = "unitless";
   if ((retval = ncmpi_put_att_text(ncid, mask_aid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_var(ncid, "mask_b", NC_INT, 1, &n_bdimid, &mask_bid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   units = "unitless";
    if ((retval = ncmpi_put_att_text(ncid, mask_bid, "units", std::strlen(units.c_str()), units.c_str())))
        Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_def_var(ncid, "area_a", NC_DOUBLE, 1, &n_adimid, &area_aid)))
      Throw() << "NC error:" << ncmpi_strerror(retval);
   units = "unitless";
    if ((retval = ncmpi_put_att_text(ncid, area_aid, "units", std::strlen(units.c_str()), units.c_str())))
        Throw() << "NC error:" << ncmpi_strerror(retval);

    if ((retval = ncmpi_def_var(ncid, "area_b", NC_DOUBLE, 1, &n_bdimid, &area_bid)))
      Throw() << "NC error:" << ncmpi_strerror(retval);
    units = "unitless";
     if ((retval = ncmpi_put_att_text(ncid, area_bid, "units", std::strlen(units.c_str()), units.c_str())))
         Throw() << "NC error:" << ncmpi_strerror(retval);

    if ((retval = ncmpi_def_var(ncid, "frac_a", NC_DOUBLE, 1, &n_adimid, &frac_aid)))
      Throw() << "NC error:" << ncmpi_strerror(retval);
    units = "unitless";
     if ((retval = ncmpi_put_att_text(ncid, frac_aid, "units", std::strlen(units.c_str()), units.c_str())))
         Throw() << "NC error:" << ncmpi_strerror(retval);
      
    if ((retval = ncmpi_def_var(ncid, "frac_b", NC_DOUBLE, 1, &n_bdimid, &frac_bid)))
      Throw() << "NC error:" << ncmpi_strerror(retval);
    units = "unitless";
      if ((retval = ncmpi_put_att_text(ncid, frac_bid, "units", std::strlen(units.c_str()), units.c_str())))
          Throw() << "NC error:" << ncmpi_strerror(retval);
   
    if ((retval = ncmpi_def_var(ncid, "col", NC_INT, 1, &n_sdimid, &colid)))
      Throw() << "NC error:" << ncmpi_strerror(retval);
    
    if ((retval = ncmpi_def_var(ncid, "row", NC_INT, 1, &n_sdimid, &rowid)))
      Throw() << "NC error:" << ncmpi_strerror(retval);
    
    if ((retval = ncmpi_def_var(ncid, "S", NC_DOUBLE, 1, &n_sdimid, &Sid)))
      Throw() << "NC error:" << ncmpi_strerror(retval);
    
    // Add some global attributes to let this work with scrip_test 
    // eventually these should be optional with a flag

    // Title
    // map method 
    char title[128];
    if (*regridConserve == ESMC_REGRID_CONSERVE_OFF &&
        *regridMethod == ESMC_REGRID_METHOD_BILINEAR) {
      sprintf(title, "ESMF Offline Bilinear Remapping");
    } else if (*regridConserve == ESMC_REGRID_CONSERVE_ON &&
        *regridMethod == ESMC_REGRID_METHOD_BILINEAR) {
      sprintf(title, "ESMF Offline Conservative Bilinear Remapping");
    } else if (*regridConserve == ESMC_REGRID_CONSERVE_OFF &&
        *regridMethod == ESMC_REGRID_METHOD_PATCH) {
      sprintf(title, "ESMF Offline Patch Remapping");
    } else if (*regridConserve == ESMC_REGRID_CONSERVE_ON &&
        *regridMethod == ESMC_REGRID_METHOD_PATCH) {
      sprintf(title, "ESMF Offline Conservative Patch Remapping");
    }
    if ((retval = ncmpi_put_att_text(ncid, NC_GLOBAL, "title",std::strlen(title), title)))
      Throw() << "NC error:" << ncmpi_strerror(retval);

    // Normalization type
    static char norm[]="destarea";
    if ((retval = ncmpi_put_att_text(ncid, NC_GLOBAL, "normalization",std::strlen(norm), norm)))
      Throw() << "NC error:" << ncmpi_strerror(retval);

    // map method 
    char map_method[128];
    if (*regridConserve == ESMC_REGRID_CONSERVE_OFF &&
        *regridMethod == ESMC_REGRID_METHOD_BILINEAR) {
      sprintf(map_method, "Bilinear remapping");
    } else if (*regridConserve == ESMC_REGRID_CONSERVE_ON &&
        *regridMethod == ESMC_REGRID_METHOD_BILINEAR) {
      sprintf(map_method, "Conservative remapping");
    } else if (*regridConserve == ESMC_REGRID_CONSERVE_OFF &&
        *regridMethod == ESMC_REGRID_METHOD_PATCH) {
      sprintf(map_method, "Bilinear remapping");
    } else if (*regridConserve == ESMC_REGRID_CONSERVE_ON &&
        *regridMethod == ESMC_REGRID_METHOD_PATCH) {
      sprintf(map_method, "Conservative remapping");
    }
    if ((retval = ncmpi_put_att_text(ncid, NC_GLOBAL, "map_method",std::strlen(map_method), map_method)))
      Throw() << "NC error:" << ncmpi_strerror(retval);

    // conventions
    static char conventions[]="NCAR-CSM";
    if ((retval = ncmpi_put_att_text(ncid, NC_GLOBAL, "conventions",std::strlen(conventions), conventions)))
      Throw() << "NC error:" << ncmpi_strerror(retval);

    // src file
    if ((retval = ncmpi_put_att_text(ncid, NC_GLOBAL, "domain_a",src_ncfile.length(),
                                     src_ncfile.c_str())))
      Throw() << "NC error:" << ncmpi_strerror(retval);

    // dst file
    if ((retval = ncmpi_put_att_text(ncid, NC_GLOBAL, "domain_b",dst_ncfile.length(),
                                     dst_ncfile.c_str())))
      Throw() << "NC error:" << ncmpi_strerror(retval);


    // Some other useful attributes suggested by Brian Kauffman
    // src file
    if ((retval = ncmpi_put_att_text(ncid, NC_GLOBAL, "grid_file_src",src_ncfile.length(),
                                     src_ncfile.c_str())))
      Throw() << "NC error:" << ncmpi_strerror(retval);

    // dst file
    if ((retval = ncmpi_put_att_text(ncid, NC_GLOBAL, "grid_file_dst",dst_ncfile.length(),
                                     dst_ncfile.c_str())))
      Throw() << "NC error:" << ncmpi_strerror(retval);

    if ((retval = ncmpi_put_att_text(ncid, NC_GLOBAL, "CVS_revision",std::strlen(ESMF_VERSION_STRING),
                                     ESMF_VERSION_STRING)))
      Throw() << "NC error:" << ncmpi_strerror(retval);


   // End of definition
   if ((retval = ncmpi_enddef(ncid)))
     Throw() << "NC error:" << ncmpi_strerror(retval);

   // Grid dims are the same on all procs.  Just write on proc 0
   ncmpi_begin_indep_data(ncid);  
   
   
   if (Par::Rank() == 0) {
     MPI_OffType starts[] = {0, 0};
     MPI_OffType counts[] = {2,0};
     if ((retval = ncmpi_put_vara_int(ncid, src_grid_dimsid, starts, counts, &ncsrc.grid_dims[0])))
       Throw() << "NC error:" << ncmpi_strerror(retval);
   
     if ((retval = ncmpi_put_vara_int(ncid, dst_grid_dimsid, starts, counts, &ncdst.grid_dims[0])))
       Throw() << "NC error:" << ncmpi_strerror(retval);
   }
   
   ncmpi_end_indep_data(ncid);
   /*-----------------------------------------------------------------------------*/
   // Read in grids one at a time to conserve good old memory.
   // Source grid first,then dest
   /*-----------------------------------------------------------------------------*/
   
   get_nc_grid_file1(ncsrc, src_ncfile, false);
   
   nc_grid_file1_2deg(ncsrc);
   
   /*-------------------------------------------------------------------------*/
   // Write the src/dest grid variables
   /*-------------------------------------------------------------------------*/
   MPI_OffType startsa[] = {ncsrc.local_grid_start, 0};
   MPI_OffType countsa[] = {ncsrc.local_grid_size, ncsrc.grid_corners};
   
   if ((retval = ncmpi_put_vara_double_all(ncid, yc_aid, startsa, countsa, &ncsrc.grid_center_lat[0])))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_put_vara_double_all(ncid, xc_aid, startsa, countsa, &ncsrc.grid_center_lon[0])))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_put_vara_double_all(ncid, yv_aid, startsa, countsa, &ncsrc.grid_corner_lat[0])))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_put_vara_double_all(ncid, xv_aid, startsa, countsa, &ncsrc.grid_corner_lon[0])))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_put_vara_int_all(ncid, mask_aid, startsa, countsa, &ncsrc.grid_imask[0])))
     Throw() << "NC error:" << ncmpi_strerror(retval);

   // Set Src Area
   {
     std::vector<double> src_area;
     
     src_area.resize(ncsrc.local_grid_size, 0.0);

   if (*regridConserve == ESMC_REGRID_CONSERVE_ON){ 
     MEField<> *src_iwts = srcmesh.GetField("iwts");

    // first sort by get_data_index
    int num_nodes = srcmesh.num_nodes();

    std::vector<MeshObj*> all_nodes;

    all_nodes.resize(num_nodes, static_cast<MeshObj*>(0));

    Mesh::iterator ni = srcmesh.node_begin(), ne = srcmesh.node_end();

    for (; ni != ne; ++ni) {

      int seq = ni->get_data_index();

      ThrowRequire(seq < num_nodes);

      all_nodes[seq] = &*ni;

    }

    // now load data into the vector to write
    for (UInt i=0; i<all_nodes.size(); ++i) {
      MeshObj *node = all_nodes.at(i);
      double *Sdata = src_iwts->data(*node);
      ThrowRequire(i<src_area.size());
      src_area[i] = *Sdata;
    }

   }
     if ((retval = ncmpi_put_vara_double_all(ncid, area_aid, startsa, countsa, &src_area[0])))
       Throw() << "NC error:" << ncmpi_strerror(retval);
     
   }
 
   // Set Src Frac
   {
   std::vector<double> src_frac;
   src_frac.resize(ncsrc.local_grid_size, 0.0);

   // Make frac non-zero for non-zero mask values
   for (int i=0; i<src_frac.size(); i++) {
     if (ncsrc.grid_imask[i] == 1) {
       src_frac[i]=1.0;
     }
   }

   if ((retval = ncmpi_put_vara_double_all(ncid,frac_aid, startsa, countsa, &src_frac[0])))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   } // free frac
 
   ncsrc.clear();
   
   /*---------------------------------------------------------------------------*/
   // And now dest.
   /*---------------------------------------------------------------------------*/
   
   get_nc_grid_file1(ncdst, dst_ncfile, false);
   
   nc_grid_file1_2deg(ncdst);
   
   /*-------------------------------------------------------------------------*/
   // Write the src/dest grid variables
   /*-------------------------------------------------------------------------*/
   MPI_OffType startsb[] = {ncdst.local_grid_start, 0};
   MPI_OffType countsb[] = {ncdst.local_grid_size, ncdst.grid_corners};
   
   if ((retval = ncmpi_put_vara_double_all(ncid, yc_bid, startsb, countsb, &ncdst.grid_center_lat[0])))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_put_vara_double_all(ncid, xc_bid, startsb, countsb, &ncdst.grid_center_lon[0])))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_put_vara_double_all(ncid, yv_bid, startsb, countsb, &ncdst.grid_corner_lat[0])))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_put_vara_double_all(ncid, xv_bid, startsb, countsb, &ncdst.grid_corner_lon[0])))
     Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_put_vara_int_all(ncid, mask_bid, startsb, countsb, &ncdst.grid_imask[0])))
     Throw() << "NC error:" << ncmpi_strerror(retval);

   // Set Dst Area
   {
     std::vector<double> dst_area;
     
     dst_area.resize(ncdst.local_grid_size, 0.0);

// TODO: This routine is writing the weights of a copy mesh
//       when conservation is turned on because of the fact
//       that some of the polar nodes are shipped to a different
//       processor when adding poles in offline regridding.  -RO 5/20/10

   if (*regridConserve == ESMC_REGRID_CONSERVE_ON){ 
     MEField<> *dst_iwts = dstmeshcpy.GetField("iwts");

    // first sort by get_data_index
    int num_nodes = dstmeshcpy.num_nodes();

    std::vector<MeshObj*> all_nodes;

    all_nodes.resize(num_nodes, static_cast<MeshObj*>(0));

    Mesh::iterator ni = dstmeshcpy.node_begin(), ne = dstmeshcpy.node_end();

    for (; ni != ne; ++ni) {

      int seq = ni->get_data_index();

      ThrowRequire(seq < num_nodes);

      all_nodes[seq] = &*ni;

    }

    // now load data into the vector to write
    for (UInt i=0; i<all_nodes.size(); ++i) {
      MeshObj *node = all_nodes.at(i);
      double *Ddata = dst_iwts->data(*node);
      ThrowRequire(i<dst_area.size());
      dst_area[i] = *Ddata;
    }

   }
     if ((retval = ncmpi_put_vara_double_all(ncid, area_bid, startsb, countsb, &dst_area[0])))
        Throw() << "NC error:" << ncmpi_strerror(retval);
   }

   // Set Dst Frac
   {
   std::vector<double> dst_frac;
   
   dst_frac.resize(ncdst.local_grid_size, 0.0);

   // Make frac non-zero for non-zero mask values
   for (int i=0; i<dst_frac.size(); i++) {
     if (ncdst.grid_imask[i] == 1) {
       dst_frac[i]=1.0;
     }
   }

   if ((retval = ncmpi_put_vara_double_all(ncid, frac_bid, startsb, countsb, &dst_frac[0])))
      Throw() << "NC error:" << ncmpi_strerror(retval);

   } // free frac
   
   
   // Free memory used by input grids before building the weight arrays
   ncdst.clear();
   
   std::vector<int> col_data(ln_s,0);
   std::vector<int> row_data(ln_s,0);
   std::vector<double> S_data(ln_s,0.0);
   
   /*
    * Load up the matrix.  Matrix is 1 based.  For the moment only
    * seq ordering is allowed, so all the idx 0 ids are numbered first,
    * followed by the 1 idx, etc...
    */
   
   IWeights::WeightMap::const_iterator wi = w.begin_row(), we = w.end_row();
   
   UInt cnt = 0;
   for (; wi != we; ++wi) {
     
     const IWeights::Entry &_row = wi->first;
     const std::vector<IWeights::Entry> &_col = wi->second;
     
     if (ordering == NCMATPAR_ORDER_SEQ) {
       for (UInt c = 0; c < _col.size(); ++c) {
         row_data[cnt] = _row.id + _row.idx*n_a;
         col_data[cnt] = _col[c].id + _col[c].idx*n_b;
         S_data[cnt] = _col[c].value;
         ++cnt;
       }
     } else if (ordering == NCMATPAR_ORDER_INTERLEAVE) {
       for (UInt c = 0; c < _col.size(); ++c) {
            row_data[cnt] = 2*(_row.id-1) + _row.idx + 1;
            col_data[cnt] = 2*(_col[c].id-1) + _col[c].idx + 1;
            S_data[cnt] = _col[c].value;
            ++cnt;
          }
     } else Throw() << "Unknown ordering:" << ordering;
     
   }
//std::cout<<"  cnt  =  "<<cnt<<std::endl;   
   MPI_OffType starts[] = {local_start_n_s, 0};
   MPI_OffType counts[] = {ln_s, 0};
   
   if ((retval = ncmpi_put_vara_int_all(ncid, colid, starts, counts, &col_data[0])))
      Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_put_vara_int_all(ncid, rowid, starts, counts, &row_data[0])))
      Throw() << "NC error:" << ncmpi_strerror(retval);
   
   if ((retval = ncmpi_put_vara_double_all(ncid, Sid, starts, counts, &S_data[0])))
      Throw() << "NC error:" << ncmpi_strerror(retval);

   ncmpi_close(ncid);
   
#else
   Throw() << "Please recompile with PNETCDF support";
#endif
  
}


} // namespace
