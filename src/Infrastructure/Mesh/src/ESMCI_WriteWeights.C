//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_WriteWeights.h>
#include <Mesh/include/ESMCI_Interp.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_ParEnv.h>

#ifdef ESMC_NETCDF
#include <netcdf.h>
#endif

#include <algorithm>
#include <iterator>
#include <cmath>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

namespace ESMCI {



static void gather_data(const IWeights &w,
                        std::vector<int> &row,
                        std::vector<int> &row_idx,
                        std::vector<int> &col,
                        std::vector<int> &col_idx,
                        std::vector<double> &S)
{
  
  IWeights::WeightMap::const_iterator wi = w.begin_row(), we = w.end_row();
  
  int n_s = row.size();
  
  int n = 0;
  for (; wi != we; ++wi) {
    const IWeights::Entry &_row = wi->first;
    const std::vector<IWeights::Entry> &_col = wi->second;
    
    for (UInt i = 0; i < _col.size(); i++) {
      
      const IWeights::Entry &cent = _col[i];
      
      row[n] = _row.id;
      row_idx[n] = _row.idx;
      col[n] = cent.id;
      col_idx[n] = cent.idx;
      
      S[n] = cent.value;
      
      ++n;
      
      ThrowRequire(n <= n_s);
    }
    
  }
  
}

static std::string get_fname(const std::string &fbase, UInt psize, UInt rank) {

  std::string extension = ".nc";
  
  std::string newname;
  
  // If csize = 1, read fbase.g
  if (psize > 1) {
    std::ostringstream newname_str;
    UInt ndec = numDecimal(psize);
    newname_str << fbase << "." << psize << ".";
    newname_str << std::setw(ndec) << std::setfill('0') << rank;
    newname = newname_str.str() + extension;
  } else newname = fbase + extension;
  
  return newname;
}

void WriteIWeights(const IWeights &w, const std::string &fbase) {
  Trace __trace("WriteIWeights(const IWeights &w, const std::string &fbase)");
#ifdef ESMC_NETCDF

  std::string newname = get_fname(fbase, Par::Size(), Par::Rank());
  
  // open the netcdf file
  int ncid, stat;
  if ((stat = nc_create(newname.c_str(), NC_CLOBBER, &ncid)) != NC_NOERR) {
    Throw() << "Trouble opening " << newname << ", ncerr=" <<
               nc_strerror(stat);
  }

  int retval;
  
  int n_s = w.count_matrix_entries().first;
  int n_sdimid;
  
  // Define dimensions
  if ((retval = nc_def_dim(ncid, "n_s", n_s, &n_sdimid)))
    Throw() << "NC error:" << nc_strerror(retval);
  
  // Define the variables
  int rowid;
  if ((retval = nc_def_var(ncid, "row", NC_INT, 1, 
          &n_sdimid, &rowid)))
    Throw() << "NC error:" << nc_strerror(retval);
  
  int row_idxid;
  if ((retval = nc_def_var(ncid, "row_idx", NC_INT, 1, 
          &n_sdimid, &row_idxid)))
    Throw() << "NC error:" << nc_strerror(retval);
  
  int colid;
  if ((retval = nc_def_var(ncid, "col", NC_INT, 1, 
          &n_sdimid, &colid)))
    Throw() << "NC error:" << nc_strerror(retval);
  
  int col_idxid;
  if ((retval = nc_def_var(ncid, "col_idx", NC_INT, 1, 
          &n_sdimid, &col_idxid)))
    Throw() << "NC error:" << nc_strerror(retval);
  
  int Sid;
  if ((retval = nc_def_var(ncid, "S", NC_DOUBLE, 1, 
          &n_sdimid, &Sid)))
    Throw() << "NC error:" << nc_strerror(retval);
  
  // End of definition
  if ((retval = nc_enddef(ncid)))
    Throw() << "NC error:" << nc_strerror(retval);
  
  
  // Now to write the variables
  std::vector<int> row(n_s,0), col(n_s,0), row_idx(n_s,0), col_idx(n_s,0);
  std::vector<double> S(n_s,0);
  
  gather_data(w, row, row_idx, col, col_idx, S);
  
  if ((retval = nc_put_var_int(ncid, rowid, &row[0])))
    Throw() << "NC error:" << nc_strerror(retval);
  
  if ((retval = nc_put_var_int(ncid, row_idxid, &row_idx[0])))
    Throw() << "NC error:" << nc_strerror(retval);
  
  if ((retval = nc_put_var_int(ncid, colid, &col[0])))
    Throw() << "NC error:" << nc_strerror(retval);
  
  if ((retval = nc_put_var_int(ncid, col_idxid, &col_idx[0])))
    Throw() << "NC error:" << nc_strerror(retval);
  
  if ((retval = nc_put_var_double(ncid, Sid, &S[0])))
    Throw() << "NC error:" << nc_strerror(retval);

  nc_close(ncid);
#else
  Throw() << "Please recompile with netcdf enabled";
#endif
}

void ReadIWeights(IWeights &w, const std::string &fbase, UInt nproc, UInt rank) {
  Trace __trace("ReadIWeights(IWeights &w, const std::string &fbase, UInt nproc, UInt rank)");
#ifdef ESMC_NETCDF
  
  std::string newname = get_fname(fbase, nproc, rank);
  
  int ncid, stat;
  
  if ((stat = nc_open(newname.c_str(), NC_NOWRITE, &ncid)) != NC_NOERR) {
    Throw() << "Trouble opening " << newname << ", ncerr=" <<
               nc_strerror(stat);
  }
  
  int n_sdimid;
  if ((stat = nc_inq_dimid(ncid, "n_s", &n_sdimid)) != NC_NOERR) {
    Throw() << "ncerr=" << nc_strerror(stat);
  }
  
  std::size_t n_s;
  if ((stat = nc_inq_dimlen(ncid, n_sdimid, &n_s)) != NC_NOERR) {
    Throw() << "ncerr=" << nc_strerror(stat);
  }
  
  std::vector<int> row(n_s,0), row_idx(n_s,0), col(n_s,0), col_idx(n_s,0);
  std::vector<double> S(n_s,0);
  
  int rowid, row_idxid, colid, col_idxid, Sid;
  if ((stat = nc_inq_varid(ncid, "row", &rowid)) != NC_NOERR) {
     Throw() << "ncerr=" << nc_strerror(stat);
   }

  if ((stat = nc_inq_varid(ncid, "row_idx", &row_idxid)) != NC_NOERR) {
     Throw() << "ncerr=" << nc_strerror(stat);
   }
  
  if ((stat = nc_inq_varid(ncid, "col", &colid)) != NC_NOERR) {
     Throw() << "ncerr=" << nc_strerror(stat);
   }
  
  if ((stat = nc_inq_varid(ncid, "col_idx", &col_idxid)) != NC_NOERR) {
     Throw() << "ncerr=" << nc_strerror(stat);
   }
  
  if ((stat = nc_inq_varid(ncid, "S", &Sid)) != NC_NOERR) {
     Throw() << "ncerr=" << nc_strerror(stat);
   }
  
  // Read in the values
  if ((stat = nc_get_var_int(ncid, rowid, &row[0])) != NC_NOERR) {
      Throw() << "ncerr=" << nc_strerror(stat);
    }
  
  if ((stat = nc_get_var_int(ncid, row_idxid, &row_idx[0])) != NC_NOERR) {
      Throw() << "ncerr=" << nc_strerror(stat);
    }
  
  if ((stat = nc_get_var_int(ncid, colid, &col[0])) != NC_NOERR) {
      Throw() << "ncerr=" << nc_strerror(stat);
    }
  
  if ((stat = nc_get_var_int(ncid, col_idxid, &col_idx[0])) != NC_NOERR) {
      Throw() << "ncerr=" << nc_strerror(stat);
    }
  
  if ((stat = nc_get_var_double(ncid, Sid, &S[0])) != NC_NOERR) {
      Throw() << "ncerr=" << nc_strerror(stat);
    }
  
  // We can close the netcdf now.
  nc_close(ncid);
  
  // Assimilate the weights.  Go row by row; we are assuming here that the weights
  // are together by row.
  
  UInt i = 0;
  while (i < n_s) {
    
    int irow = row[i];
    int idx = row_idx[i];
    
    IWeights::Entry _row(irow, idx);
    
    // Loop all entries for this row/idx
    std::vector<IWeights::Entry> _col;
    while (i < n_s && row[i] == irow && idx == row_idx[i]) {
      
      _col.push_back(IWeights::Entry(col[i], col_idx[i], S[i]));
      
      ++i;
      
    } // row

    w.InsertRow(_row, _col);
    
  }  // i < n_s

#else
  Throw() << "Please recompile with netcdf enabled";
#endif
  
}

// Slurp a netcdf grid definition file into a struct.
struct nc_grid_file {
int grid_size;
int grid_corners;
int grid_rank;

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

static void nc_grid_file_2deg(nc_grid_file &ncf) {
  
  const double rad2deg = 180.0/M_PI;
  
  for (UInt i = 0; i < ncf.grid_size; i++) {
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

static void get_nc_grid_file(nc_grid_file &ncf, const std::string &ncfile) {
#ifdef ESMC_NETCDF

  int ncid, stat;


  if ((stat = nc_open(ncfile.c_str(), NC_WRITE, &ncid)) != NC_NOERR) {
    Throw() << "Trouble opening " << ncfile << ", ncerr=" <<
               nc_strerror(stat);
  }

  std::size_t n, grid_rank, grid_corners;

  // Get ni, nj; verify nv=4
  int dimid;
  // n
  if ((stat = nc_inq_dimid(ncid, "grid_size", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  nc_inq_dimlen(ncid, dimid, &n);
  
  ncf.grid_size = n;

  // ni
  if ((stat = nc_inq_dimid(ncid, "grid_rank", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  nc_inq_dimlen(ncid, dimid, &grid_rank);
  
  ncf.grid_rank = grid_rank;

  ThrowRequire(grid_rank == 2); // for now

  // nv
  if ((stat = nc_inq_dimid(ncid, "grid_corners", &dimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  nc_inq_dimlen(ncid, dimid, &grid_corners);

  ncf.grid_corners = grid_corners;

  if (grid_corners != 4) Throw() << "unexpected nv=" << grid_corners << "; should be 4";

  // Get the grid dims
  int gdimid;
  if ((stat = nc_inq_varid(ncid, "grid_dims", &gdimid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  
  int grid_dims[2];
  
  nc_get_var_int(ncid, gdimid, &grid_dims[0]);
  
  ncf.grid_dims.resize(2);
  std::copy(&grid_dims[0], &grid_dims[2], ncf.grid_dims.begin()); 

  ncf.grid_center_lat.resize(ncf.grid_size);
  ncf.grid_center_lon.resize(ncf.grid_size);

  ncf.grid_corner_lat.resize(ncf.grid_size*ncf.grid_corners);
  ncf.grid_corner_lon.resize(ncf.grid_size*ncf.grid_corners);
  
  int gclatid, gclonid, gcrnrlatid, gcrnrlonid, imaskid;
  if ((stat = nc_inq_varid(ncid, "grid_center_lon", &gclonid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  if ((stat = nc_inq_varid(ncid, "grid_center_lat", &gclatid)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  
  if ((stat = nc_inq_varid(ncid, "grid_corner_lon", &gcrnrlonid)) != NC_NOERR) {
     Throw() << "NCErr:" << nc_strerror(stat);
   }
   if ((stat = nc_inq_varid(ncid, "grid_corner_lat", &gcrnrlatid)) != NC_NOERR) {
     Throw() << "NCErr:" << nc_strerror(stat);
   }
   
   if ((stat = nc_inq_varid(ncid, "grid_imask", &imaskid)) != NC_NOERR) {
     Throw() << "NCErr:" << nc_strerror(stat);
   }
  
  // Get units
  std::size_t gclonattlen, gclatattlen, gcrnrlatattlen, gcrnrlonattlen;
  bool latdeg = false, londeg = false, crnrlatdeg = false, crnrlondeg = false;
  char attbuf[1024];
  
  // Get lens
  if ((stat = nc_inq_attlen(ncid, gclatid, "units", &gclatattlen)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  if ((stat = nc_inq_attlen(ncid, gclonid, "units", &gclonattlen)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  if ((stat = nc_inq_attlen(ncid, gcrnrlatid, "units", &gcrnrlatattlen)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  if ((stat = nc_inq_attlen(ncid, gcrnrlonid, "units", &gcrnrlonattlen)) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  
  
  // Get text
  if ((stat = nc_get_att_text(ncid, gclatid, "units", &attbuf[0])) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  attbuf[gclatattlen] = '\0';
  if (std::string(attbuf) == std::string("degrees")) latdeg = true;
  
  if ((stat = nc_get_att_text(ncid, gclonid, "units", &attbuf[0])) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  attbuf[gclonattlen] = '\0';
  if (std::string(attbuf) == std::string("degrees")) londeg = true;
  
  
  if ((stat = nc_get_att_text(ncid, gcrnrlatid, "units", &attbuf[0])) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  attbuf[gcrnrlatattlen] = '\0';
  if (std::string(attbuf) == std::string("degrees")) crnrlatdeg = true;
  
  if ((stat = nc_get_att_text(ncid, gcrnrlonid, "units", &attbuf[0])) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
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
  if ((stat = nc_get_var_double(ncid, gclatid, &ncf.grid_center_lat[0])) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  
  if ((stat = nc_get_var_double(ncid, gclonid, &ncf.grid_center_lon[0])) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  
  // corners
  if ((stat = nc_get_var_double(ncid, gcrnrlatid, &ncf.grid_corner_lat[0])) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  
  if ((stat = nc_get_var_double(ncid, gcrnrlonid, &ncf.grid_corner_lon[0])) != NC_NOERR) {
    Throw() << "NCErr:" << nc_strerror(stat);
  }
  
  ncf.grid_imask.resize(ncf.grid_size);
  if ((stat = nc_get_var_int(ncid, imaskid, &ncf.grid_imask[0])) != NC_NOERR) {
     Throw() << "NCErr:" << nc_strerror(stat);
   }
  
  nc_close(ncid);

#else
  Throw() << "Please recompile with netcdf enabled";
#endif
  
}

void WriteNCMatFile(const std::string &src_ncfile,
                    const std::string &dst_ncfile,
                    const std::string &outfile,
                    const IWeights &w,
                    int ordering)
{
  Trace __trace("WriteNCMatFile(const std::string &src_ncfile, const std::string &dst_ncfile, const std::string &outfile, const IWeights &w, int ordering");

#ifdef ESMC_NETCDF
  std::pair<int,int> pa = w.count_matrix_entries();
  int n_s = pa.first;
  int max_idx = pa.second;
  
  // Open the src/out files
  nc_grid_file ncsrc, ncdst;
  
  get_nc_grid_file(ncsrc, src_ncfile);
  
  get_nc_grid_file(ncdst, dst_ncfile);
  
  // Convert to degrees
  nc_grid_file_2deg(ncsrc);
  nc_grid_file_2deg(ncdst);
  
  int n_a = ncsrc.grid_size, n_b = ncdst.grid_size;
  
  // Open the netcdf output file

  // open the netcdf file
   int ncid, stat;
   if ((stat = nc_create(outfile.c_str(), NC_CLOBBER, &ncid)) != NC_NOERR) {
     Throw() << "Trouble opening " << outfile << ", ncerr=" <<
                nc_strerror(stat);
   }

   int retval;
   
   int n_adimid, n_bdimid, nv_adimid, nv_bdimid, src_grid_rankdimid, dst_grid_rankdimid,
       ni_adimid, nj_adimid, ni_bdimid, nj_bdimid, n_sdimid, num_wgtsdimid, n_kdimid;
   
   // Define dimensions
   if ((retval = nc_def_dim(ncid, "n_a", ncsrc.grid_size, &n_adimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_dim(ncid, "n_b", ncdst.grid_size, &n_bdimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_dim(ncid, "nv_a", ncsrc.grid_corners, &nv_adimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   
   if ((retval = nc_def_dim(ncid, "nv_b", ncdst.grid_corners, &nv_bdimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_dim(ncid, "src_grid_rank", ncsrc.grid_rank, &src_grid_rankdimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_dim(ncid, "dst_grid_rank", ncdst.grid_rank, &dst_grid_rankdimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_dim(ncid, "ni_a", ncsrc.grid_dims[0], &ni_adimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_dim(ncid, "nj_a", ncsrc.grid_dims[1], &nj_adimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_dim(ncid, "ni_b", ncdst.grid_dims[0], &ni_bdimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_dim(ncid, "nj_b", ncdst.grid_dims[1], &nj_bdimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_dim(ncid, "n_s", n_s, &n_sdimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_dim(ncid, "num_wgts", int(1), &num_wgtsdimid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if (max_idx > 0)
     if ((retval = nc_def_dim(ncid, "n_k", (max_idx + 1), &n_kdimid)))
       Throw() << "NC error:" << nc_strerror(retval);


   /*-------------------------------------------------------------------------*/
   // Define the variables
   /*-------------------------------------------------------------------------*/
   
   int src_grid_dimsid, dst_grid_dimsid, yc_aid, yc_bid, xc_aid, xc_bid,
       yv_aid, yv_bid, xv_aid, xv_bid, mask_aid, mask_bid, area_aid, area_bid,
       frac_aid, frac_bid, colid, rowid, Sid;
   
   std::string units;

   int dim_scratch[2];
   
   if ((retval = nc_def_var(ncid, "src_grid_dims", NC_INT, 1, &src_grid_rankdimid, &src_grid_dimsid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_var(ncid, "dst_grid_dims", NC_INT, 1, &dst_grid_rankdimid, &dst_grid_dimsid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_var(ncid, "yc_a", NC_DOUBLE, 1, &n_adimid, &yc_aid)))
     Throw() << "NC error:" << nc_strerror(retval);  
   units = ncsrc.lat_deg ? "degrees" : "radians";
   if ((retval = nc_put_att_text(ncid, yc_aid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_var(ncid, "yc_b", NC_DOUBLE, 1, &n_bdimid, &yc_bid)))
     Throw() << "NC error:" << nc_strerror(retval);
   units = ncdst.lat_deg ? "degrees" : "radians";
   if ((retval = nc_put_att_text(ncid, yc_bid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_var(ncid, "xc_a", NC_DOUBLE, 1, &n_adimid, &xc_aid)))
     Throw() << "NC error:" << nc_strerror(retval);
   units = ncsrc.lon_deg ? "degrees" : "radians";
   if ((retval = nc_put_att_text(ncid, xc_aid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_var(ncid, "xc_b", NC_DOUBLE, 1, &n_bdimid, &xc_bid)))
     Throw() << "NC error:" << nc_strerror(retval);
   units = ncdst.lon_deg ? "degrees" : "radians";
   if ((retval = nc_put_att_text(ncid, xc_bid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << nc_strerror(retval);
   
   dim_scratch[0] = n_adimid; dim_scratch[1] = nv_adimid;
   if ((retval = nc_def_var(ncid, "yv_a", NC_DOUBLE, 2, &dim_scratch[0], &yv_aid)))
     Throw() << "NC error:" << nc_strerror(retval);
   units = ncsrc.corner_lat_deg ? "degrees" : "radians";
   if ((retval = nc_put_att_text(ncid, yv_aid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_var(ncid, "xv_a", NC_DOUBLE, 2, &dim_scratch[0], &xv_aid)))
     Throw() << "NC error:" << nc_strerror(retval);
   units = ncsrc.corner_lon_deg ? "degrees" : "radians";
   if ((retval = nc_put_att_text(ncid, xv_aid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << nc_strerror(retval);
   
   dim_scratch[0] = n_bdimid; dim_scratch[1] = nv_bdimid;
   if ((retval = nc_def_var(ncid, "yv_b", NC_DOUBLE, 2, &dim_scratch[0], &yv_bid)))
     Throw() << "NC error:" << nc_strerror(retval);
   units = ncdst.corner_lat_deg ? "degrees" : "radians";
   if ((retval = nc_put_att_text(ncid, yv_bid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_var(ncid, "xv_b", NC_DOUBLE, 2, &dim_scratch[0], &xv_bid)))
     Throw() << "NC error:" << nc_strerror(retval);
   units = ncdst.corner_lon_deg ? "degrees" : "radians";
   if ((retval = nc_put_att_text(ncid, xv_bid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_var(ncid, "mask_a", NC_INT, 1, &n_adimid, &mask_aid)))
     Throw() << "NC error:" << nc_strerror(retval);
   units = "unitless";
   if ((retval = nc_put_att_text(ncid, mask_aid, "units", std::strlen(units.c_str()), units.c_str())))
       Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_var(ncid, "mask_b", NC_INT, 1, &n_bdimid, &mask_bid)))
     Throw() << "NC error:" << nc_strerror(retval);
   units = "unitless";
    if ((retval = nc_put_att_text(ncid, mask_bid, "units", std::strlen(units.c_str()), units.c_str())))
        Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_def_var(ncid, "area_a", NC_DOUBLE, 1, &n_adimid, &area_aid)))
      Throw() << "NC error:" << nc_strerror(retval);
   units = "square radians";
    if ((retval = nc_put_att_text(ncid, area_aid, "units", std::strlen(units.c_str()), units.c_str())))
        Throw() << "NC error:" << nc_strerror(retval);
    
    if ((retval = nc_def_var(ncid, "area_b", NC_DOUBLE, 1, &n_bdimid, &area_bid)))
      Throw() << "NC error:" << nc_strerror(retval);
    units = "square radians";
     if ((retval = nc_put_att_text(ncid, area_bid, "units", std::strlen(units.c_str()), units.c_str())))
         Throw() << "NC error:" << nc_strerror(retval);
    
    if ((retval = nc_def_var(ncid, "frac_a", NC_DOUBLE, 1, &n_adimid, &frac_aid)))
      Throw() << "NC error:" << nc_strerror(retval);
    units = "unitless";
     if ((retval = nc_put_att_text(ncid, frac_aid, "units", std::strlen(units.c_str()), units.c_str())))
         Throw() << "NC error:" << nc_strerror(retval);
      
    if ((retval = nc_def_var(ncid, "frac_b", NC_DOUBLE, 1, &n_bdimid, &frac_bid)))
      Throw() << "NC error:" << nc_strerror(retval);
    units = "unitless";
      if ((retval = nc_put_att_text(ncid, frac_bid, "units", std::strlen(units.c_str()), units.c_str())))
          Throw() << "NC error:" << nc_strerror(retval);
   
    if ((retval = nc_def_var(ncid, "col", NC_INT, 1, &n_sdimid, &colid)))
      Throw() << "NC error:" << nc_strerror(retval);
    
    if ((retval = nc_def_var(ncid, "row", NC_INT, 1, &n_sdimid, &rowid)))
      Throw() << "NC error:" << nc_strerror(retval);
    
    if ((retval = nc_def_var(ncid, "S", NC_DOUBLE, 1, &n_sdimid, &Sid)))
      Throw() << "NC error:" << nc_strerror(retval);
    
   // End of definition
   if ((retval = nc_enddef(ncid)))
     Throw() << "NC error:" << nc_strerror(retval);
   
   /*-------------------------------------------------------------------------*/
   // Write the src/dest grid variables
   /*-------------------------------------------------------------------------*/
   if ((retval = nc_put_var_int(ncid, src_grid_dimsid, &ncsrc.grid_dims[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_int(ncid, dst_grid_dimsid, &ncdst.grid_dims[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_double(ncid, yc_aid, &ncsrc.grid_center_lat[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_double(ncid, yc_bid, &ncdst.grid_center_lat[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_double(ncid, xc_aid, &ncsrc.grid_center_lon[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_double(ncid, xc_bid, &ncdst.grid_center_lon[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_double(ncid, yv_aid, &ncsrc.grid_corner_lat[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_double(ncid, yv_bid, &ncdst.grid_corner_lat[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_double(ncid, xv_aid, &ncsrc.grid_corner_lon[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_double(ncid, xv_bid, &ncdst.grid_corner_lon[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_int(ncid, mask_aid, &ncsrc.grid_imask[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_int(ncid, mask_bid, &ncdst.grid_imask[0])))
     Throw() << "NC error:" << nc_strerror(retval);

   {
     std::vector<double> area;
     
     area.resize(ncsrc.grid_size, 0);
     if ((retval = nc_put_var_double(ncid, area_aid, &area[0])))
       Throw() << "NC error:" << nc_strerror(retval);
     
     area.resize(ncdst.grid_size, 0);
     if ((retval = nc_put_var_double(ncid, area_bid, &area[0])))
        Throw() << "NC error:" << nc_strerror(retval);
   }
   
   {
   std::vector<double> frac;
   
   frac.resize(ncsrc.grid_size, 0);
   if ((retval = nc_put_var_double(ncid, frac_aid, &frac[0])))
     Throw() << "NC error:" << nc_strerror(retval);
   
   frac.resize(ncdst.grid_size, 0);
   if ((retval = nc_put_var_double(ncid, frac_bid, &frac[0])))
      Throw() << "NC error:" << nc_strerror(retval);

   } // free frac
   
   // Free memory used by input grids before building the weight arrays
   ncsrc.clear();
   ncdst.clear();
   
   std::vector<int> col_data(n_s,0);
   std::vector<int> row_data(n_s,0);
   std::vector<double> S_data(n_s,0.0);
   
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
     
     for (UInt c = 0; c < _col.size(); ++c) {
       row_data[cnt] = _row.id + _row.idx*n_a;
       col_data[cnt] = _col[c].id + _col[c].idx*n_b;
       S_data[cnt] = _col[c].value;
       ++cnt;
     }
     
   }
   
   if ((retval = nc_put_var_int(ncid, colid, &col_data[0])))
      Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_int(ncid, rowid, &row_data[0])))
      Throw() << "NC error:" << nc_strerror(retval);
   
   if ((retval = nc_put_var_double(ncid, Sid, &S_data[0])))
      Throw() << "NC error:" << nc_strerror(retval);
   
   nc_close(ncid);

#else
  Throw() << "Please recompile with netcdf enabled";
#endif
  
}

} // namespace
