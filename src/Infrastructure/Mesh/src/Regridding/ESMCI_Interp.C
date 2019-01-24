// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Regridding/ESMCI_Interp.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Regridding/ESMCI_Search.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/Legacy/ESMCI_MEValues.h>
#include <Mesh/include/Regridding/ESMCI_PatchRecovery.h>
#include <Mesh/include/Legacy/ESMCI_MeshField.h>
#include <Mesh/include/Regridding/ESMCI_MeshRegrid.h>
#include <Mesh/include/Legacy/ESMCI_CommRel.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjConn.h>
#include <Mesh/include/Legacy/ESMCI_Migrator.h>
#include <Mesh/include/Legacy/ESMCI_MeshObj.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/Regridding/ESMCI_ConserveInterp.h>
#include <Mesh/include/Regridding/ESMCI_Conserve2ndInterp.h>
#include <Mesh/include/Legacy/ESMCI_Sintdnode.h>
#include <Mesh/include/ESMCI_XGridUtil.h>
#include "PointList/include/ESMCI_PointList.h"

#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>

#include "ESMCI_Macros.h"

 //#define CHECK_SENS

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
  static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


// prototypes of Fortran interfaces used in this source file
extern "C" void FTN_X(f_esmf_getmeshdistgrid)(int*, int*, int*, int*);


namespace ESMCI {


/*-----------------------------------------------------------------*/
// IWeights
/*-----------------------------------------------------------------*/
IWeights::IWeights() :
WMat()
{
}

IWeights::IWeights(const IWeights &rhs) :
WMat(rhs)
{
}

IWeights &IWeights::operator=(const IWeights &rhs)
{

  WMat::operator=(rhs);

  if (this == &rhs) return *this;

  return *this;
}

inline bool IDMatch(const IWeights::Entry &e1, const IWeights::Entry &e2) { return e1.id == e2.id; }

void IWeights::ChangeCoords(const IWeights &src_uv, const IWeights &dst_uv) {
  Trace __trace("IWeights::ChangeCoords(const IWeights &src_uv, const IWeights &dst_uv)");

  struct UV_vect {
     double U[3];
    double V[3];
  };

  WeightMap new_weights; // will create whole new matrix

  // Loop rows in matrix
  WeightMap::iterator wi = weights.begin(), we = weights.end();

  for (; wi != we;) {

    // There must be 3 consecutive entries with this id (index 0,1,2)

    std::vector<const Entry*> rows(3);
    std::vector<std::vector<Entry>*> cols(3);

 //   long id = wi->first.id;
    // Get three rows.  Since weights are redundant, reuse
    {
      rows[0] = &wi->first; cols[0] = &wi->second;
      ThrowRequire(wi->first.idx == 0);
      rows[1] = &wi->first; cols[1] = &wi->second;
      rows[2] = &wi->first; cols[2] = &wi->second;
      ++wi;
    }

    // Lookup UV destination for this row.  These are stored as
    // (U V)^T, i.e.
    // (gid, 0) -> (gid, 0, Ux) (gid, 1, Uy) (gid, 2, Uz)
    // (gid, 1) -> (gid, 0, Vx) (gid, 1, Vy) (gid, 2, Vz)
    WeightMap::const_iterator ruv_i = dst_uv.weights.lower_bound(Entry(rows[0]->id, 0));

    UV_vect ruv;
    for (UInt r = 0; r < 2; r++) {

      ThrowRequire(ruv_i != dst_uv.weights.end());
      ThrowRequire(ruv_i->first.id == rows[0]->id);
      ThrowRequire((UInt) ruv_i->first.idx == r);

      {
         const std::vector <Entry> &ruv_col = ruv_i->second;
        ThrowRequire(ruv_col.size() == 3);
        double *val = r == 0 ? ruv.U : ruv.V;
        val[0] = ruv_col[0].value;
        val[1] = ruv_col[1].value;
        val[2] = ruv_col[2].value;
      }

      ++ruv_i;
    }

    // Loop columns
    UInt ncols = cols[0]->size();
    ThrowRequire(ncols == cols[1]->size() && ncols == cols[2]->size());

    Entry row_u(*rows[0]);
    Entry row_v(*rows[1]); row_v.idx = 1;

    std::vector<std::vector<Entry> > new_cols(2, std::vector<Entry>(2*ncols));

    for (UInt i = 0; i < ncols; i++) {

      // Get the UV for row
      Entry &col_x = cols[0]->operator[](i);
      Entry &col_y = cols[1]->operator[](i);
      Entry &col_z = cols[2]->operator[](i);

      // Entries should reference the same id
      UInt col_id = col_x.id;
      ThrowRequire(col_id == col_y.id && col_id == col_z.id);

      // Get column UV vectors (source)
      // stored as (U V), i.e.
      // (gid, 0) -> (gid, 0, Ux) (gid, 1, Vx)
      // (gid, 1) -> (gid, 0, Uy) (gid, 1, Vy)
      // (gid, 2) -> (gid, 0, Uz) (gid, 1, Vz)
      // Get the column uv vectors
      WeightMap::const_iterator cuv_i = src_uv.weights.lower_bound(Entry(col_id, 0));

      UV_vect cuv;

      for (UInt r = 0; r < 3; r++) {

        ThrowRequire(cuv_i != src_uv.weights.end() && (UInt) cuv_i->first.id == col_id && (UInt) cuv_i->first.idx == r);

        {
          const std::vector <Entry> &cuv_col = cuv_i->second;
          ThrowRequire(cuv_col.size() == 2);

          cuv.U[r] = cuv_col[0].value;
          cuv.V[r] = cuv_col[1].value;
        }

        ++cuv_i;

      }

      // Add the u entries
      {
        Entry &u_col_u = new_cols[0][2*i];
        Entry &u_col_v = new_cols[0][2*i+1];

        u_col_u.id = u_col_v.id = col_x.id;
        u_col_u.idx = 0; u_col_v.idx = 1;

        // After all this data structure ado, the heart of the
        // numerical algorithm:
        u_col_u.value = ruv.U[0]*col_x.value*cuv.U[0] +
                        ruv.U[1]*col_y.value*cuv.U[1] +
                        ruv.U[2]*col_z.value*cuv.U[2];

        u_col_v.value = ruv.U[0]*col_x.value*cuv.V[0] +
                        ruv.U[1]*col_y.value*cuv.V[1] +
                        ruv.U[2]*col_z.value*cuv.V[2];

      }

      // Add the v entries
      {
        Entry &v_col_u = new_cols[1][2*i];
         Entry &v_col_v = new_cols[1][2*i+1];

        v_col_u.id = v_col_v.id = col_x.id;
        v_col_u.idx = 0; v_col_v.idx = 1;

        v_col_u.value = ruv.V[0]*col_x.value*cuv.U[0] +
                        ruv.V[1]*col_y.value*cuv.U[1] +
                        ruv.V[2]*col_z.value*cuv.U[2];

        v_col_v.value = ruv.V[0]*col_x.value*cuv.V[0] +
                        ruv.V[1]*col_y.value*cuv.V[1] +
                        ruv.V[2]*col_z.value*cuv.V[2];

      }

    } // for i

    // Insert new rows
    new_weights[row_u] = new_cols[0];
    new_weights[row_v] = new_cols[1];

  } // for wi

  weights.swap(new_weights);

}

void IWeights::Prune(const Mesh &mesh, const MEField<> *mask) {

  WeightMap::iterator wi = begin_row(), we = end_row(), wn;

  double my_mask = 1.0;

  for (; wi != we;) {

    wn = wi; ++wn;
    UInt gid = wi->first.id;

    Mesh::MeshObjIDMap::const_iterator mi = mesh.map_find(MeshObj::NODE, gid);

     ThrowRequire(mi != mesh.map_end(MeshObj::NODE));

    double *mval = mask ? (double*) mask->data(*mi) : &my_mask;

    if (*mval < 0.5 || !GetMeshObjContext(*mi).is_set(Attr::OWNED_ID))
      weights.erase(wi);

    wi = wn;
  }

}


/*----------------------------------------------------------------*/
// Interp:
// Basic routines to interpolate data.
/*----------------------------------------------------------------*/

/*
 * Patch interpolation, where destination and source fields are nodal.
 */

void patch_serial_transfer(MEField<> &src_coord_field, UInt _nfields, MEField<>* const* _sfields, _field* const *_dfields, SearchResult &sres, Mesh *srcmesh, int imethod) {
   Trace __trace("patch_serial_transfer(MEField<> &src_coord_field, UInt _nfields, MEField<>* const* _sfields, _field* const *_dfields, int *iflag, SearchResult &sres)");


   std::set<int> pdeg_set;
   std::map<int, ElemPatch<>*> patch_map;

  if (_nfields == 0) return;

  // Get mask field pointer
  MEField<> *src_mask_ptr = srcmesh->GetField("mask");


  std::vector<MEField<>* > fields;
  std::vector<_field* > dfields;
  std::vector<int> orders;
   UInt nrhs = 0;

  for (UInt i = 0; i < _nfields; i++) {

    // Only process interp_patch
    if (imethod != Interp::INTERP_PATCH) continue;

    int patch_order=2;
    pdeg_set.insert(patch_order);

    //ThrowRequire(_sfields[i]->is_nodal());

    fields.push_back(_sfields[i]); dfields.push_back(_dfields[i]);

    orders.push_back(patch_order);

    ThrowRequire(_sfields[i]->dim() == _dfields[i]->dim());

    nrhs += _sfields[i]->dim(); // how many rhs for recovery
  }

  // Create the  recovery field
  SearchResult::iterator sb = sres.begin(), se = sres.end();

  for (; sb != se; sb++) {
    Search_result &sres = **sb;
    // Trick:  Gather the data from the source field so we may call interpolate point
    const MeshObj &elem = *(*sb)->elem;
//std::cout << "Transfer: elem:" << elem.get_id() << std::endl;

    {
       std::set<int>::iterator pi = pdeg_set.begin(), pe = pdeg_set.end();

      for (; pi != pe; ++pi) {

        ElemPatch<> *epatch = new ElemPatch<>();

        epatch->CreateElemPatch(*pi, ElemPatch<>::GAUSS_PATCH,
                               elem,
                               src_coord_field,
                                src_mask_ptr,
                               fields.size(),
                               &fields[0],
                               700000
                                );

        patch_map[*pi] = epatch;

      }
    }

    // Gather parametric coords into an array.
    UInt pdim = GetMeshObjTopo(elem)->parametric_dim;
    UInt npts = sres.nodes.size(); // number of points to interpolate
    std::vector<double> pc(pdim*npts);

    for (UInt np = 0; np < npts; np++) {

      for (UInt pd = 0; pd < pdim; pd++) {

         pc[np*pdim+pd] = sres.nodes[np].pcoord[pd];

      }

    }

    std::map<int, std::vector<double> > result;

     {
      std::set<int>::iterator pi = pdeg_set.begin(), pe = pdeg_set.end();

      for (; pi != pe; ++pi) {

        std::pair<std::map<int, std::vector<double> >::iterator, bool> ri
             = result.insert(std::make_pair(*pi, std::vector<double>()));

        ThrowRequire(ri.second == true);

        ri.first->second.resize(nrhs*npts);

         ElemPatch<> &epatch = *patch_map[*pi];

        epatch.Eval(npts, &pc[0], &(ri.first->second[0]));

      }

    }

    std::vector<std::vector<double>* > field_results;

    for (UInt i = 0; i < dfields.size(); i++)
      field_results.push_back(&result[orders[i]]);

    // Now copy data into fields
    for (UInt np = 0; np < npts; np++) {

      const MeshObj &snode = *sres.nodes[np].node;
      UInt cur_field = 0;

      for (UInt i = 0; i < dfields.size(); i++) {

        const _field &dfield = *dfields[i];

        std::vector<double> &results = *field_results[i];

        if (dfield.OnObj(snode)) {
          UInt fdim = dfield.dim();
          double *data = dfield.data(snode);

          for (UInt f = 0; f < fdim; f++) {
            data[f] = results[np*nrhs+(cur_field+f)];
          }

        }

        cur_field += dfield.dim();

      } // for fi

    } // for np

   std::map<int, ElemPatch<>*>::iterator pi = patch_map.begin(), pe = patch_map.end();
   for (; pi != pe; ++pi) delete pi->second;


  } // for searchresult

}


struct dof_add_col {

dof_add_col(std::vector<IWeights::Entry> &_col, UInt _fdim, double *_sens) :
  idx(0),
  col(_col),
  fdim(_fdim),
  sens(_sens)
{}

void operator()(MeshObj *obj, UInt nvalset, UInt n) {

  // Weights are redundant per field dimension entry; just take first
  if (n == 0) {
    col.push_back(IWeights::Entry(obj->get_id(), n, sens[idx*fdim+n]));

/*
    Par::Out() << "Adding id:" << obj->get_id() <<", val:" << sens[idx*fdim+n] << std::endl;
     std::cout << "Adding id:" << obj->get_id() <<", val:" << sens[idx*fdim+n] << std::endl;
*/
    ++idx;
  }


}

UInt idx;
std::vector<IWeights::Entry> &col;
double *sens;
UInt fdim;

 };


/** Matrix patch transfer **/
void mat_patch_serial_transfer(MEField<> &src_coord_field, MEField<> &_sfield, SearchResult &sres,  Mesh *srcmesh, IWeights &iw, PointList *dstpointlist) {
  Trace __trace("mat_patch_serial_transfer(MEField<> &sfield, SearchResult &sres, IWeights &iw, PointList *dstpointlist)");

  const int pdeg = 2; // TODO: deduce this.  For instance, deg source + 1 is reasonable

  // Get mask field pointer
  MEField<> *src_mask_ptr = srcmesh->GetField("mask");

  MEField<>* field = &_sfield;
  MEField<> &sfield = _sfield;
   UInt nrhs = field->dim();


   // Create the sensitivity field
  MEField<SField> sF(sfield);
  MEField<SField> *sFp = &sF;

  // Create the  recovery field
  SearchResult::iterator sb = sres.begin(), se = sres.end();

  int dstpointlist_dim=dstpointlist->get_coord_dim();

  for (; sb != se; sb++) {

    Search_result &sres = **sb;

    // Trick:  Gather the data from the source field so we may call interpolate point
    MeshObj &elem = const_cast<MeshObj&>(*(*sb)->elem);

    // Create a sensitivity field residing on the degrees of
    // freedom used in this interpolation.
    std::set<MeshObj*> elems;

    MeshObjConn::NeighborElements(elem, elems);

     UInt nlocal_dof = sF.AssignElements(elems.begin(), elems.end());

    std::vector<fad_type> fads(nlocal_dof, 0);

     sF.ReInit(&fads[0]);

     // Set up the fad degrees of freedom.
    for (UInt i = 0; i < nlocal_dof; i++) {
      fads[i].diff(i, nlocal_dof);
    }

    ElemPatch<MEField<SField>, fad_type> epatch;

    epatch.CreateElemPatch(pdeg, ElemPatch<>::GAUSS_PATCH,
                           elem,
                           src_coord_field,
                           src_mask_ptr,
                           1,
                           &sFp,
                           700000
                            );

    // Gather parametric coords into an array.
    UInt pdim = GetMeshObjTopo(elem)->parametric_dim;
    UInt npts = sres.nodes.size(); // number of points to interpolate
    std::vector<double> pc(pdim*npts);

    for (UInt np = 0; np < npts; np++) {

      for (UInt pd = 0; pd < pdim; pd++) {

        pc[np*pdim+pd] = sres.nodes[np].pcoord[pd];

       }

    }

    std::vector<fad_type> result(nrhs*npts);
    epatch.Eval(npts, &pc[0], &result[0]);

     // Now copy data into fields and save sensitivies
    for (UInt n = 0; n < npts; n++) {

      // DEBUG printf(">>>> snode=%d# elem=%d \n",snode.get_id(),elem.get_id());

      //     UInt fdim = dfield->dim();

      //for (UInt d = 0; d < fdim; d++) {
      for (UInt d = 0; d < 1; d++) { // weights are redundant per entry

        // DON'T ACTUALLY DO REGRID BECAUSE WE DON'T USE IT
        // data[d] = result[n*nrhs+d].val();

        IWeights::Entry row(sres.nodes[n].dst_gid, d, 0.0, elem.get_id());

        std::vector<IWeights::Entry> col;
        col.reserve(nlocal_dof);

#ifdef CHECK_SENS
        Par::Out() << "sens=" << result[n*nrhs+d] << std::endl;
        double sval = 0;
#endif

        double *sens = &(result[n*nrhs+d].fastAccessDx(0));

        dof_add_col addc(col, dstpointlist_dim, sens);

        sF.dof_iterator(addc);

        iw.InsertRow(row, col);

#ifdef CHECK_SENS
        int dof_div_dim = nlocal_dof/dstpointlist_dim;
        for (UInt s = 0; s < dof_div_dim; s++) {
          sval += fads[s*nrhs+d].val()*sens[s*nrhs+d];
        } // for s

        double diff = sval - result[n*nrhs+d].val();
        Par::Out() << "**diff=" << diff << std::endl;
        if (std::abs(diff) > 1e-4) {

          for (UInt s = 0; s < nlocal_dof/dstpointlist_dim; s++) {
            Par::Out() << fads[s*nrhs+d].val() << " ";
          }
          Par::Out() << std::endl;
        }
#endif

      } // for d

     } // for np

  } // for searchresult
}


void calc_2nd_order_conserve_mat_serial_2D_3D_sph(Mesh &srcmesh, Mesh &dstmesh, Mesh *midmesh, SearchResult &sres,
                                        IWeights &iw, IWeights &src_frac, IWeights &dst_frac,
                                        struct Zoltan_Struct * zz, bool set_dst_status, WMat &dst_status) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");

  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

   // Get src and dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");

  // Get src and dst area field
  MEField<> *dst_area_field = dstmesh.GetField("elem_area");
  MEField<> *src_area_field = srcmesh.GetField("elem_area");


  // src and dst frac2 fields
  MEField<> * src_frac2_field = srcmesh.GetField("elem_frac2");
  MEField<> * dst_frac2_field = dstmesh.GetField("elem_frac2");

  // Declare vectors to hold weight and auxilary information
  std::vector<int> valid;
  std::vector<HC_WGHT> wgts;
  std::vector<double> areas;
  std::vector<double> dst_areas;

  // Declare variable that will hold just unmasked elements

  // Declare some variables that are used inside the weight calc
  // here so that we don't keep reallocating them
  std::vector<SM_CELL> sm_cells;
  std::vector<NBR_ELEM> nbrs;

  // Temporary buffers for concave case,
  // so there isn't lots of reallocation
  std::vector<int> tmp_valid;
  std::vector<double> tmp_areas;
  std::vector<double> tmp_dst_areas;

  // Find maximum number of dst elements in search results
  int max_num_dst_elems=0;
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    // NOTE: sr.elem is a src element and sr.elems is a list of dst elements
    Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.elems.size() > max_num_dst_elems) max_num_dst_elems=sr.elems.size();
  }

  // Allocate space for weight calc output arrays
  valid.resize(max_num_dst_elems,0);
  areas.resize(max_num_dst_elems,0.0);
  dst_areas.resize(max_num_dst_elems,0.0);


  // reserve these variables
  wgts.reserve(max_num_dst_elems);
  sm_cells.reserve(max_num_dst_elems);
  nbrs.reserve(20); // Reserve a reasonable amount of nbrs,
                    // this will grow to fit, so we don't need
                    // to worry too much about the exact size.

  // Loop through search results
  for (sb = sres.begin(); sb != se; sb++) {

    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
    Search_result &sr = **sb;


    // If there are no associated dst elements then skip it
    if (sr.elems.size() == 0) continue;

    //  Only use it if it's locally owned
    //   if (!GetAttr(*(sr.elem)).is_locally_owned()) continue;

     // If this source element is masked then skip it
    bool src_elem_masked=false;
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) {
          src_elem_masked=true;
          if (!set_dst_status) continue; // if this is masked and we aren't
                                         // setting dst status, then go to next search result
                                         // TODO: put code in ESMCI_Search.C, so the masked
                                         // source elements, don't get here
        }
    }



    // THIS HAS SOMETHING TO DO WITH XGrid, turn off for now
    // ASK FEI ABOUT IT.
    // IF YOU DO TURN IT BACK ON, THEN NEED TO ADD frac2 field to ghosting in ESMCI_MeshRegrid.C
    // If this source element is creeped out during merging then skip it
    double src_frac2=1.0;
    if(src_frac2_field){
      const MeshObj &src_elem = *sr.elem;
      src_frac2=*(double *)(src_frac2_field->data(src_elem));
      if (src_frac2 == 0.0) continue;
    }


    // Declare src_elem_area
    double src_elem_area;

    // Clear vectors so that it can be refilled
    wgts.clear();
    sm_cells.clear();
    nbrs.clear();


    // Calculate weights
    calc_2nd_order_weights_2D_3D_sph(sr.elem,src_cfield,src_mask_field,
                                      sr.elems,dst_cfield,dst_mask_field, dst_frac2_field,
                                        &src_elem_area, &valid, &wgts, &areas, &dst_areas,
                                        &tmp_valid, &tmp_areas, &tmp_dst_areas, &sm_cells, &nbrs);


    // Invalidate masked destination elements
    if (dst_mask_field) {
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *msk=dst_mask_field->data(dst_elem);
        if (*msk>0.5) {
          valid[i]=0;
        }
      }
    }

    // Invalidate creeped out dst element
    // THIS HAS SOMETHING TO DO WITH XGrid, turn off for now
    // ASK FEI ABOUT IT
    if(dst_frac2_field){
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *dst_frac2=dst_frac2_field->data(dst_elem);
        if (*dst_frac2 == 0.0){
          valid[i] = 0;
          continue;
        }
      }
    }


     // Set status for src masked cells, and then leave
    if (src_elem_masked) {
        for (int i=0; i<sr.elems.size(); i++) {
          if (valid[i]==1) {

            // Calculate fraction
            double frac=0.0;
            if (dst_areas[i] != 0.0) {
              frac=areas[i]/dst_areas[i];
            }

            // Set col info
            IWeights::Entry col(sr.elem->get_id(), ESMC_REGRID_STATUS_SRC_MASKED,
                                frac, 0);

            // Set row info
            IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

            // Put status entry into matrix
            dst_status.InsertRowMergeSingle(row, col);
          }
        }

      // src is masked, so don't add weights (i.e. continue to next)
      continue;
    }


    // Set status for other cells
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {

        // Calculate fraction
        double frac=0.0;
        if (dst_areas[i] != 0.0) {
          frac=areas[i]/dst_areas[i];
        }

        // Set col info
        IWeights::Entry col(sr.elem->get_id(), ESMC_REGRID_STATUS_MAPPED,
                            frac, 0);

         // Set row info
        IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

        // Put status entry into matrix
        dst_status.InsertRowMergeSingle(row, col);
      }
    }


    // If no weights then go on to next src elem
    if (wgts.empty()) continue;


    // Adjust weights for user area
    if (src_area_field || dst_area_field) {

      // Calculate source user area adjustment
      double src_user_area_adj=1.0;
      if (src_area_field) {
        const MeshObj &src_elem = *sr.elem;
        double *area=src_area_field->data(src_elem);
        if (src_elem_area == 0.0) Throw() << "src_elem_area unexpectedly 0.0";
        src_user_area_adj=*area/src_elem_area;
      }

      // Add weights to weight matrix
      for (int i=0; i<wgts.size(); i++) {

        // Calculate dest user area adjustment
        double dst_user_area_adj=1.0;
        if (dst_area_field) {
          const MeshObj &dst_elem = *(sr.elems[wgts[i].dst_index]);
          double *area=dst_area_field->data(dst_elem);
          if (*area==0.0) Throw() << "0.0 user area in destination grid";
          dst_user_area_adj=dst_areas[wgts[i].dst_index]/(*area);
        }

        // Calculate weight with adjustment
        wgts[i].wgt=wgts[i].wgt*src_user_area_adj*dst_user_area_adj;
      }
    }

    // Add weights to weight matrix
    for (int i=0; i<wgts.size(); i++) {

      // Set col info
      IWeights::Entry col(wgts[i].src_id, 0,
                          src_frac2*wgts[i].wgt, sr.elem->get_id());

      // Set row info
      IWeights::Entry row(wgts[i].dst_id, 0, 0.0, 0);

      // Put weights into weight matrix
      iw.InsertRowSumSingle(row, col);

#if 0
      if (wgts[i].dst_id==162) {
        printf("%d# dst_id=%d src_id=%d s=%d w=%f\n",Par::Rank(),wgts[i].dst_id,wgts[i].src_id,sr.elem->get_id(),wgts[i].wgt);
      }
#endif
    }

    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't need to do the rest
    if (num_valid < 1) continue;

    // Temporary empty col with negatives so unset values
    // can be detected if they sneak through
    IWeights::Entry col_empty(-1, 0, -1.0, 0);

    // Compute src fractions and insert into src_frac
    {
      // Allocate column of empty entries
      std::vector<IWeights::Entry> col;
      col.resize(num_valid,col_empty);

      // Put weights into column
      int j=0;
      for (int i=0; i<sr.elems.size(); i++) {
        if (valid[i]==1) {
          col[j].id=sr.elems[i]->get_id();
          if (src_elem_area == 0.0) Throw() << "src_elem_area unexpectedly 0.0";
          col[j].value=areas[i]/src_elem_area;
          j++;
        }
      }

      // Set row info
      IWeights::Entry row(sr.elem->get_id(), 0, 0.0, 0);

      // Put weights into weight matrix
      src_frac.InsertRowMerge(row, col);
    }

    // Calc and set dst_frac
    // (Can't just sum weights, so need to have a separate dst_frac)
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {

        // Calculate fraction
        double frac=0.0;
        if (dst_areas[i] != 0.0) {
          frac=areas[i]/dst_areas[i];
        }

        // Set col info
        IWeights::Entry col(sr.elem->get_id(), 0,
                            src_frac2*frac, 0);

        // Set row info
        IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

        // Put weights into weight matrix
        dst_frac.InsertRowMergeSingle(row, col);
      }
    }
  } // for searchresult

}


void calc_2nd_order_conserve_mat_serial_2D_2D_cart(Mesh &srcmesh, Mesh &dstmesh, Mesh *midmesh, SearchResult &sres,
                                        IWeights &iw, IWeights &src_frac, IWeights &dst_frac,
                                        struct Zoltan_Struct * zz, bool set_dst_status, WMat &dst_status) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");

  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

   // Get src and dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");

  // Get src and dst area field
  MEField<> *dst_area_field = dstmesh.GetField("elem_area");
  MEField<> *src_area_field = srcmesh.GetField("elem_area");


  // src and dst frac2 fields
  MEField<> * src_frac2_field = srcmesh.GetField("elem_frac2");
  MEField<> * dst_frac2_field = dstmesh.GetField("elem_frac2");

  // Declare vectors to hold weight and auxilary information
  std::vector<int> valid;
  std::vector<HC_WGHT> wgts;
  std::vector<double> areas;
  std::vector<double> dst_areas;

  // Declare variable that will hold just unmasked elements

  // Declare some variables that are used inside the weight calc
  // here so that we don't keep reallocating them
  std::vector<SM_CELL> sm_cells;
  std::vector<NBR_ELEM> nbrs;

  // Temporary buffers for concave case,
  // so there isn't lots of reallocation
  std::vector<int> tmp_valid;
  std::vector<double> tmp_areas;
  std::vector<double> tmp_dst_areas;

  // Find maximum number of dst elements in search results
  int max_num_dst_elems=0;
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    // NOTE: sr.elem is a src element and sr.elems is a list of dst elements
    Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.elems.size() > max_num_dst_elems) max_num_dst_elems=sr.elems.size();
  }

  // Allocate space for weight calc output arrays
  valid.resize(max_num_dst_elems,0);
  areas.resize(max_num_dst_elems,0.0);
  dst_areas.resize(max_num_dst_elems,0.0);


  // reserve these variables
  wgts.reserve(max_num_dst_elems);
  sm_cells.reserve(max_num_dst_elems);
  nbrs.reserve(20); // Reserve a reasonable amount of nbrs,
                    // this will grow to fit, so we don't need
                    // to worry too much about the exact size.

  // Loop through search results
  for (sb = sres.begin(); sb != se; sb++) {

    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
    Search_result &sr = **sb;


    // If there are no associated dst elements then skip it
    if (sr.elems.size() == 0) continue;

    //  Only use it if it's locally owned
    //   if (!GetAttr(*(sr.elem)).is_locally_owned()) continue;

     // If this source element is masked then skip it
    bool src_elem_masked=false;
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) {
          src_elem_masked=true;
          if (!set_dst_status) continue; // if this is masked and we aren't
                                         // setting dst status, then go to next search result
                                         // TODO: put code in ESMCI_Search.C, so the masked
                                         // source elements, don't get here
        }
    }



    // THIS HAS SOMETHING TO DO WITH XGrid, turn off for now
    // ASK FEI ABOUT IT.
    // IF YOU DO TURN IT BACK ON, THEN NEED TO ADD frac2 field to ghosting in ESMCI_MeshRegrid.C
    // If this source element is creeped out during merging then skip it
    double src_frac2=1.0;
    if(src_frac2_field){
      const MeshObj &src_elem = *sr.elem;
      src_frac2=*(double *)(src_frac2_field->data(src_elem));
      if (src_frac2 == 0.0) continue;
    }


    // Declare src_elem_area
    double src_elem_area;

    // Clear vectors so that it can be refilled
    wgts.clear();
    sm_cells.clear();
    nbrs.clear();


    // Calculate weights
    calc_2nd_order_weights_2D_2D_cart(sr.elem,src_cfield,src_mask_field,
                                      sr.elems,dst_cfield,dst_mask_field, dst_frac2_field,
                                        &src_elem_area, &valid, &wgts, &areas, &dst_areas,
                                        &tmp_valid, &tmp_areas, &tmp_dst_areas, &sm_cells, &nbrs);


    // Invalidate masked destination elements
    if (dst_mask_field) {
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *msk=dst_mask_field->data(dst_elem);
        if (*msk>0.5) {
          valid[i]=0;
        }
      }
    }

    // Invalidate creeped out dst element
    // THIS HAS SOMETHING TO DO WITH XGrid, turn off for now
    // ASK FEI ABOUT IT
    if(dst_frac2_field){
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *dst_frac2=dst_frac2_field->data(dst_elem);
        if (*dst_frac2 == 0.0){
          valid[i] = 0;
          continue;
        }
      }
    }


     // Set status for src masked cells, and then leave
    if (src_elem_masked) {
        for (int i=0; i<sr.elems.size(); i++) {
          if (valid[i]==1) {

            // Calculate fraction
            double frac=0.0;
            if (dst_areas[i] != 0.0) {
              frac=areas[i]/dst_areas[i];
            }

            // Set col info
            IWeights::Entry col(sr.elem->get_id(), ESMC_REGRID_STATUS_SRC_MASKED,
                                frac, 0);

            // Set row info
            IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

            // Put status entry into matrix
            dst_status.InsertRowMergeSingle(row, col);
          }
        }

      // src is masked, so don't add weights (i.e. continue to next)
      continue;
    }


    // Set status for other cells
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {

        // Calculate fraction
        double frac=0.0;
        if (dst_areas[i] != 0.0) {
          frac=areas[i]/dst_areas[i];
        }

        // Set col info
        IWeights::Entry col(sr.elem->get_id(), ESMC_REGRID_STATUS_MAPPED,
                            frac, 0);

         // Set row info
        IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

        // Put status entry into matrix
        dst_status.InsertRowMergeSingle(row, col);
      }
    }


    // If no weights then go on to next src elem
    if (wgts.empty()) continue;


    // Adjust weights for user area
    if (src_area_field || dst_area_field) {

      // Calculate source user area adjustment
      double src_user_area_adj=1.0;
      if (src_area_field) {
        const MeshObj &src_elem = *sr.elem;
        double *area=src_area_field->data(src_elem);
        if (src_elem_area == 0.0) Throw() << "src_elem_area unexpectedly 0.0";
        src_user_area_adj=*area/src_elem_area;
      }

      // Add weights to weight matrix
      for (int i=0; i<wgts.size(); i++) {

        // Calculate dest user area adjustment
        double dst_user_area_adj=1.0;
        if (dst_area_field) {
          const MeshObj &dst_elem = *(sr.elems[wgts[i].dst_index]);
          double *area=dst_area_field->data(dst_elem);
          if (*area==0.0) Throw() << "0.0 user area in destination grid";
          dst_user_area_adj=dst_areas[wgts[i].dst_index]/(*area);
        }

        // Calculate weight with adjustment
        wgts[i].wgt=wgts[i].wgt*src_user_area_adj*dst_user_area_adj;
      }
    }

    // Add weights to weight matrix
    for (int i=0; i<wgts.size(); i++) {

      // Set col info
      IWeights::Entry col(wgts[i].src_id, 0,
                          src_frac2*wgts[i].wgt, sr.elem->get_id());

      // Set row info
      IWeights::Entry row(wgts[i].dst_id, 0, 0.0, 0);

      // Put weights into weight matrix
      iw.InsertRowSumSingle(row, col);

#if 0
      if (wgts[i].dst_id==162) {
        printf("%d# dst_id=%d src_id=%d s=%d w=%f\n",Par::Rank(),wgts[i].dst_id,wgts[i].src_id,sr.elem->get_id(),wgts[i].wgt);
      }
#endif
    }

    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't need to do the rest
    if (num_valid < 1) continue;

    // Temporary empty col with negatives so unset values
    // can be detected if they sneak through
    IWeights::Entry col_empty(-1, 0, -1.0, 0);

    // Compute src fractions and insert into src_frac
    {
      // Allocate column of empty entries
      std::vector<IWeights::Entry> col;
      col.resize(num_valid,col_empty);

      // Put weights into column
      int j=0;
      for (int i=0; i<sr.elems.size(); i++) {
        if (valid[i]==1) {
          col[j].id=sr.elems[i]->get_id();
          if (src_elem_area == 0.0) Throw() << "src_elem_area unexpectedly 0.0";
          col[j].value=areas[i]/src_elem_area;
          j++;
        }
      }

      // Set row info
      IWeights::Entry row(sr.elem->get_id(), 0, 0.0, 0);

      // Put weights into weight matrix
      src_frac.InsertRowMerge(row, col);
    }

    // Calc and set dst_frac
    // (Can't just sum weights, so need to have a separate dst_frac)
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {

        // Calculate fraction
        double frac=0.0;
        if (dst_areas[i] != 0.0) {
          frac=areas[i]/dst_areas[i];
        }

        // Set col info
        IWeights::Entry col(sr.elem->get_id(), 0,
                            src_frac2*frac, 0);

        // Set row info
        IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

        // Put weights into weight matrix
        dst_frac.InsertRowMergeSingle(row, col);
      }
    }
  } // for searchresult

}

void calc_2nd_order_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, Mesh *midmesh, SearchResult &sres, IWeights &iw, IWeights &src_frac, IWeights &dst_frac, struct Zoltan_Struct * zz, bool set_dst_status, WMat &dst_status)  {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");

  // both meshes have to have the same dimensions
  if (srcmesh.parametric_dim() != dstmesh.parametric_dim()) {
    Throw() << "src and dst mesh must have the same parametric dimension for conservative regridding";
  }

  if (srcmesh.spatial_dim() != dstmesh.spatial_dim()) {
    Throw() << "src and dst mesh must have the same spatial dimension for conservative regridding";
  }

   // If necessary, set status for masked dst cells
  if (set_dst_status) {
    // Get elem mask pointer
    MEField<> *dmptr = dstmesh.GetField("elem_mask");

    // If mask field exists, then mark masked dst elems
    if (dmptr != NULL) {
      MeshDB::const_iterator ei = dstmesh.elem_begin(), ee = dstmesh.elem_end();
      for (; ei != ee; ++ei) {
        const MeshObj &elem=*ei;

         // Get mask value
        double *m=dmptr->data(*ei);
        
        // If masked, then mark
        if (*m > 0.5) {
          // Set col info
           IWeights::Entry col(0,ESMC_REGRID_STATUS_DST_MASKED,
                              1.0, 0);

          // Set row info
          IWeights::Entry row(elem.get_id(), 0, 0.0, 0);

          // Put status entry into matrix
          dst_status.InsertRowMergeSingle(row, col);
        }
      }
    }

   }

  // Get dimension, because they're the same can just get one
  int sdim=srcmesh.spatial_dim();
  int pdim=srcmesh.parametric_dim();

  // Get weights depending on dimension
  if (pdim==2) {
    if (sdim==2) {
      calc_2nd_order_conserve_mat_serial_2D_2D_cart(srcmesh, dstmesh, midmesh, sres, iw,
                                                src_frac, dst_frac, zz,
                                                set_dst_status, dst_status);
    } else if (sdim==3) {

      calc_2nd_order_conserve_mat_serial_2D_3D_sph(srcmesh, dstmesh, midmesh, sres, iw,
                                                src_frac, dst_frac, zz,
                                                set_dst_status, dst_status);
    }
   } else if (pdim==3) {
    if (sdim==3) {
      Throw() << "Meshes with parametric dim == 3 and spatial dim == 3 not supported for 2nd order conservative regridding";

     } else {
      Throw() << "Meshes with parametric dim == 3, but spatial dim !=3 not supported for conservative regridding";
    }
  } else {
     Throw() << "Meshes with parametric dimension != 2 or 3 not supported for conservative regridding";
  }

}



void calc_conserve_mat_serial_2D_2D_cart(Mesh &srcmesh, Mesh &dstmesh, Mesh *midmesh, SearchResult &sres, IWeights &iw,
                                         IWeights &src_frac, IWeights &dst_frac, struct Zoltan_Struct * zz,
                                         bool set_dst_status, WMat &dst_status) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");




  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

  // Get src and dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");

  // Get src and dst area field
  MEField<> *dst_area_field = dstmesh.GetField("elem_area");
  MEField<> *src_area_field = srcmesh.GetField("elem_area");


  // determine if we should use the dst_frac variable
  bool use_dst_frac=false;
  if (dst_area_field || src_area_field) use_dst_frac=true;

  // src and dst frac2 fields
  MEField<> * src_frac2_field = srcmesh.GetField("elem_frac2");
  MEField<> * dst_frac2_field = dstmesh.GetField("elem_frac2");

  // store all the intersections
  std::vector<sintd_node *> sintd_nodes;
  std::vector<sintd_cell *> sintd_cells;

   // Declare vectors to hold weight and auxilary information
  std::vector<int> valid;
  std::vector<double> wgts;
  std::vector<double> areas;
  std::vector<double> dst_areas;

  // Temporary buffers for concave case,
  // so there isn't lots of reallocation
  std::vector<int> tmp_valid;
  std::vector<double> tmp_areas;
  std::vector<double> tmp_dst_areas;

  // Find maximum number of dst elements in search results
  int max_num_dst_elems=0;
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    // NOTE: sr.elem is a src element and sr.elems is a list of dst elements
    Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.elems.size() > max_num_dst_elems) max_num_dst_elems=sr.elems.size();
  }


  // Allocate space for weight calc output arrays
  valid.resize(max_num_dst_elems,0);
  wgts.resize(max_num_dst_elems,0.0);
  areas.resize(max_num_dst_elems,0.0);
  dst_areas.resize(max_num_dst_elems,0.0);

  // Loop through search results
  for (sb = sres.begin(); sb != se; sb++) {

    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
    Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.elems.size() == 0) continue;

    // If this source element is masked then skip it
    bool src_elem_masked=false;
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) {
          src_elem_masked=true;
          if (!set_dst_status) continue; // if this is masked and we aren't
                                         // setting dst status, then go to next search result
                                         // TODO: put code in ESMCI_Search.C, so the masked
                                         // source elements, don't get here
        }
    }

    // If this source element is creeped out during merging then skip it
    double src_frac2=1.0;
    if(src_frac2_field){
      const MeshObj &src_elem = *sr.elem;
      src_frac2=*(double *)(src_frac2_field->data(src_elem));
      if (src_frac2 == 0.0) continue;
    }

    // Declare src_elem_area
    double src_elem_area;

    // Calculate weights
    std::vector<sintd_node *> tmp_nodes;
    std::vector<sintd_cell *> tmp_cells;
     calc_1st_order_weights_2D_2D_cart(sr.elem,src_cfield,
                                      sr.elems,dst_cfield,dst_mask_field, dst_frac2_field,
                                      &src_elem_area, &valid, &wgts, &areas, &dst_areas,
                                      &tmp_valid, &tmp_areas, &tmp_dst_areas,
                                      midmesh, &tmp_nodes, &tmp_cells, 0, zz);


    // Invalidate masked destination elements
    if (dst_mask_field) {
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *msk=dst_mask_field->data(dst_elem);
        if (*msk>0.5) {
          valid[i]=0;
        }
      }
    }
    // Invalidate creeped out dst element
    if(dst_frac2_field){
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *dst_frac2=dst_frac2_field->data(dst_elem);
        if (*dst_frac2 == 0.0){
          valid[i] = 0;
          continue;
        }
      }
    }


    // Set status for src masked cells, and then leave
    if (src_elem_masked) {
        for (int i=0; i<sr.elems.size(); i++) {
          if (valid[i]==1) {
            // Set col info
            IWeights::Entry col(sr.elem->get_id(), ESMC_REGRID_STATUS_SRC_MASKED,
                                wgts[i], 0);

            // Set row info
            IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

            // Put status entry into matrix
            dst_status.InsertRowMergeSingle(row, col);
          }
        }

      // src is masked, so don't add weights (i.e. continue to next)
      continue;
    }

    // Set status for other cells
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {

        // Set col info
        IWeights::Entry col(sr.elem->get_id(), ESMC_REGRID_STATUS_MAPPED,
                            wgts[i], 0);

        // Set row info
        IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

        // Put status entry into matrix
        dst_status.InsertRowMergeSingle(row, col);
      }
    }

    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't add weights
    if (num_valid < 1) continue;

    // Append only valid nodes/cells
    std::copy(tmp_nodes.begin(), tmp_nodes.end(), std::back_inserter(sintd_nodes));
    std::copy(tmp_cells.begin(), tmp_cells.end(), std::back_inserter(sintd_cells));

    if(! midmesh) {
      // Temporary empty col with negatives so unset values
      // can be detected if they sneak through
      IWeights::Entry col_empty(-1, 0, -1.0, 0);

      // Insert fracs into src_frac
      {
        // Allocate column of empty entries
        std::vector<IWeights::Entry> col;
        col.resize(num_valid,col_empty);

        // Put weights into column
        int j=0;
        for (int i=0; i<sr.elems.size(); i++) {
          if (valid[i]==1) {
            col[j].id=sr.elems[i]->get_id();
            col[j].value=areas[i]/src_elem_area;
            j++;
          }
        }

        // Set row info
        IWeights::Entry row(sr.elem->get_id(), 0, 0.0, 0);

        // Put weights into weight matrix
        src_frac.InsertRowMerge(row, col);
      }

      // Put weights into dst_frac and then add
      // Don't do this if there are no user areas
      if (use_dst_frac) {
        for (int i=0; i<sr.elems.size(); i++) {
          if (valid[i]==1) {
            // Set col info
            IWeights::Entry col(sr.elem->get_id(), 0,
                                src_frac2*wgts[i], 0);

            // Set row info
            IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

            // Put weights into weight matrix
            dst_frac.InsertRowMergeSingle(row, col);
          }
        }
      }


      // Calculate source user area adjustment
      double src_user_area_adj=1.0;
      if (src_area_field) {
          const MeshObj &src_elem = *sr.elem;
          double *area=src_area_field->data(src_elem);
          src_user_area_adj=*area/src_elem_area;
      }


      // Put weights into row column and then add
      for (int i=0; i<sr.elems.size(); i++) {
        if (valid[i]==1) {

          // Calculate dest user area adjustment
          double dst_user_area_adj=1.0;
          if (dst_area_field) {
            const MeshObj &dst_elem = *(sr.elems[i]);
            double *area=dst_area_field->data(dst_elem);
            if (*area==0.0) Throw() << "0.0 user area in destination grid";
            dst_user_area_adj=dst_areas[i]/(*area);
          }

          // Set col info
          IWeights::Entry col(sr.elem->get_id(), 0,
                              src_user_area_adj*dst_user_area_adj*src_frac2*wgts[i], 0);

          // Set row info
          IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

          // Put weights into weight matrix
          iw.InsertRowMergeSingle(row, col);
        }
      }
    }
  } // for searchresult

  if(midmesh != 0)
    compute_midmesh(sintd_nodes, sintd_cells, 2, 2, midmesh);

}



void calc_conserve_mat_serial_2D_3D_sph(Mesh &srcmesh, Mesh &dstmesh, Mesh *midmesh, SearchResult &sres,
                                        IWeights &iw, IWeights &src_frac, IWeights &dst_frac,
                                        struct Zoltan_Struct * zz, bool set_dst_status, WMat &dst_status) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");

  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

   // Get src and dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");

  // Get src and dst area field
  MEField<> *dst_area_field = dstmesh.GetField("elem_area");
  MEField<> *src_area_field = srcmesh.GetField("elem_area");


  // determine if we should use the dst_frac variable
  bool use_dst_frac=false;
  if (dst_area_field || src_area_field) use_dst_frac=true;

  // src and dst frac2 fields
  MEField<> * src_frac2_field = srcmesh.GetField("elem_frac2");
  MEField<> * dst_frac2_field = dstmesh.GetField("elem_frac2");

  // store all the intersections
  std::vector<sintd_node *> sintd_nodes;
  std::vector<sintd_cell *> sintd_cells;

  // Declare vectors to hold weight and auxilary information
  std::vector<int> valid;
  std::vector<double> wgts;
  std::vector<double> areas;
  std::vector<double> dst_areas;

  // Temporary buffers for concave case,
  // so there isn't lots of reallocation
   std::vector<int> tmp_valid;
  std::vector<double> tmp_areas;
  std::vector<double> tmp_dst_areas;

  // Find maximum number of dst elements in search results
  int max_num_dst_elems=0;
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    // NOTE: sr.elem is a src element and sr.elems is a list of dst elements
    Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.elems.size() > max_num_dst_elems) max_num_dst_elems=sr.elems.size();
  }


  // Allocate space for weight calc output arrays
  valid.resize(max_num_dst_elems,0);
  wgts.resize(max_num_dst_elems,0.0);
  areas.resize(max_num_dst_elems,0.0);
  dst_areas.resize(max_num_dst_elems,0.0);

  // Loop through search results
  for (sb = sres.begin(); sb != se; sb++) {

    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
    Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
     if (sr.elems.size() == 0) continue;

    // If this source element is masked then skip it
    bool src_elem_masked=false;
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) {
          src_elem_masked=true;
          if (!set_dst_status) continue; // if this is masked and we aren't
                                         // setting dst status, then go to next search result
                                         // TODO: put code in ESMCI_Search.C, so the masked
                                         // source elements, don't get here
        }
    }

    // If this source element is creeped out during merging then skip it
    double src_frac2=1.0;
    if(src_frac2_field){
      const MeshObj &src_elem = *sr.elem;
      src_frac2=*(double *)(src_frac2_field->data(src_elem));
      if (src_frac2 == 0.0) continue;
    }

    // Declare src_elem_area
    double src_elem_area;

    // Calculate weights
    std::vector<sintd_node *> tmp_nodes;
     std::vector<sintd_cell *> tmp_cells;
    calc_1st_order_weights_2D_3D_sph(sr.elem,src_cfield,
                                     sr.elems,dst_cfield,dst_mask_field, dst_frac2_field,
                                     &src_elem_area, &valid, &wgts, &areas, &dst_areas,
                                     &tmp_valid, &tmp_areas, &tmp_dst_areas,
                                      midmesh, &tmp_nodes, &tmp_cells, 0, zz);

    // Invalidate masked destination elements
    if (dst_mask_field) {
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *msk=dst_mask_field->data(dst_elem);
        if (*msk>0.5) {
          valid[i]=0;
        }
      }
    }
    // Invalidate creeped out dst element
    if(dst_frac2_field){
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *dst_frac2=dst_frac2_field->data(dst_elem);
        if (*dst_frac2 == 0.0){
          valid[i] = 0;
          continue;
        }
      }
    }


    // Set status for src masked cells, and then leave
    if (src_elem_masked) {
        for (int i=0; i<sr.elems.size(); i++) {
          if (valid[i]==1) {
            // Set col info
            IWeights::Entry col(sr.elem->get_id(), ESMC_REGRID_STATUS_SRC_MASKED,
                                wgts[i], 0);

            // Set row info
            IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

            // Put status entry into matrix
            dst_status.InsertRowMergeSingle(row, col);
          }
        }

      // src is masked, so don't add weights (i.e. continue to next)
      continue;
    }


    // Set status for other cells
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {

        // Set col info
        IWeights::Entry col(sr.elem->get_id(), ESMC_REGRID_STATUS_MAPPED,
                            wgts[i], 0);

        // Set row info
        IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

        // Put status entry into matrix
        dst_status.InsertRowMergeSingle(row, col);
      }
    }


    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) num_valid++;
      }

    // If none valid, then don't add weights
     if (num_valid < 1) continue;

    // Append only valid nodes/cells
    std::copy(tmp_nodes.begin(), tmp_nodes.end(), std::back_inserter(sintd_nodes));
    std::copy(tmp_cells.begin(), tmp_cells.end(), std::back_inserter(sintd_cells));

    if(! midmesh) {
      // Temporary empty col with negatives so unset values
      // can be detected if they sneak through
      IWeights::Entry col_empty(-1, 0, -1.0, 0);

      // Insert fracs into src_frac
       {
        // Allocate column of empty entries
        std::vector<IWeights::Entry> col;
        col.resize(num_valid,col_empty);

        // Put weights into column
        int j=0;
        for (int i=0; i<sr.elems.size(); i++) {
          if (valid[i]==1) {
            col[j].id=sr.elems[i]->get_id();
            col[j].value=areas[i]/src_elem_area;
            j++;
          }
        }

        // Set row info
        IWeights::Entry row(sr.elem->get_id(), 0, 0.0, 0);

        // Put weights into weight matrix
        src_frac.InsertRowMerge(row, col);
      }


      // Put weights into dst_frac and then add
      // Don't do this if there are no user areas
      if (use_dst_frac) {
        for (int i=0; i<sr.elems.size(); i++) {
          if (valid[i]==1) {
            // Set col info
             IWeights::Entry col(sr.elem->get_id(), 0,
                                src_frac2*wgts[i], 0);

            // Set row info
            IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

            // Put weights into weight matrix
            dst_frac.InsertRowMergeSingle(row, col);
          }
        }
      }


      // Calculate source user area adjustment
      double src_user_area_adj=1.0;
      if (src_area_field) {
          const MeshObj &src_elem = *sr.elem;
          double *area=src_area_field->data(src_elem);
          src_user_area_adj=*area/src_elem_area;
      }


      // Put weights into row column and then add
       for (int i=0; i<sr.elems.size(); i++) {
        if (valid[i]==1) {

          // Calculate dest user area adjustment
          double dst_user_area_adj=1.0;
          if (dst_area_field) {
             const MeshObj &dst_elem = *(sr.elems[i]);
            double *area=dst_area_field->data(dst_elem);
            if (*area==0.0) Throw() << "0.0 user area in destination grid";
            dst_user_area_adj=dst_areas[i]/(*area);
          }

          // Set col info
          IWeights::Entry col(sr.elem->get_id(), 0,
                              src_user_area_adj*dst_user_area_adj*src_frac2*wgts[i], 0);

          // Set row info
          IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

          // Put weights into weight matrix
          iw.InsertRowMergeSingle(row, col);
        }
      }
    } // not generating mid mesh, need to compute weights
  } // for searchresult

  if(midmesh != 0)
    compute_midmesh(sintd_nodes, sintd_cells, 2, 3, midmesh);

}


void calc_conserve_mat_serial_3D_3D_cart(Mesh &srcmesh, Mesh &dstmesh, Mesh *midmesh, SearchResult &sres,
                                        IWeights &iw, IWeights &src_frac, IWeights &dst_frac,
                                         struct Zoltan_Struct *zz, bool set_dst_status, WMat &dst_status) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");

  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

  // Get src and dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");

  // Get src and dst area field
  MEField<> *dst_area_field = dstmesh.GetField("elem_area");
  MEField<> *src_area_field = srcmesh.GetField("elem_area");


  // determine if we should use the dst_frac variable
  bool use_dst_frac=false;
  if (dst_area_field || src_area_field) use_dst_frac=true;

  // src and dst frac2 fields
  MEField<> * src_frac2_field = srcmesh.GetField("elem_frac2");
  MEField<> * dst_frac2_field = dstmesh.GetField("elem_frac2");

  // store all the intersections
  std::vector<sintd_node *> sintd_nodes;
  std::vector<sintd_cell *> sintd_cells;

  // Loop through search results
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {

    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
    Search_result &sr = **sb;

    // If there are no associated dst elements then skip it
    if (sr.elems.size() == 0) continue;

    // If this source element is masked then skip it
    bool src_elem_masked=false;
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) {
          src_elem_masked=true;
          if (!set_dst_status) continue; // if this is masked and we aren't
                                         // setting dst status, then go to next search result
                                         // TODO: put code in ESMCI_Search.C, so the masked
                                         // source elements, don't get here
        }
    }

    // If this source element is creeped out during merging then skip it
    double src_frac2=1.0;
    if(src_frac2_field){
      const MeshObj &src_elem = *sr.elem;
      src_frac2=*(double *)(src_frac2_field->data(src_elem));
      if (src_frac2 == 0.0) continue;
    }

    // Declare src_elem_area
    double src_elem_area;

    // Declare weight vector
    // TODO: Move these out of the loop, to save the time of allocating them
    std::vector<int> valid;
    std::vector<double> wgts;
    std::vector<double> areas;
    std::vector<double> dst_areas;

    // Allocate space for weight calc output arrays
    valid.resize(sr.elems.size(),0);
    wgts.resize(sr.elems.size(),0.0);
    areas.resize(sr.elems.size(),0.0);
    dst_areas.resize(sr.elems.size(),0.0);

    // Calculate weights
    std::vector<sintd_node *> tmp_nodes;
    std::vector<sintd_cell *> tmp_cells;
    calc_1st_order_weights_3D_3D_cart(sr.elem,src_cfield,
                                     sr.elems,dst_cfield,dst_mask_field, dst_frac2_field,
                                     &src_elem_area, &valid, &wgts, &areas, &dst_areas,
                                     midmesh, &tmp_nodes, &tmp_cells, 0, zz);

    // Invalidate masked destination elements
    if (dst_mask_field) {
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *msk=dst_mask_field->data(dst_elem);
        if (*msk>0.5) {
          valid[i]=0;
        }
      }
    }
    // Invalidate creeped out dst element
    if(dst_frac2_field){
      for (int i=0; i<sr.elems.size(); i++) {
        const MeshObj &dst_elem = *sr.elems[i];
        double *dst_frac2=dst_frac2_field->data(dst_elem);
        if (*dst_frac2 == 0.0){
          valid[i] = 0;
          continue;
        }
      }
    }


    // Set status for src masked cells, and then leave
    if (src_elem_masked) {
        for (int i=0; i<sr.elems.size(); i++) {
          if (valid[i]==1) {
            // Set col info
            IWeights::Entry col(sr.elem->get_id(), ESMC_REGRID_STATUS_SRC_MASKED,
                                wgts[i], 0);

            // Set row info
            IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

            // Put status entry into matrix
            dst_status.InsertRowMergeSingle(row, col);
          }
        }

      // src is masked, so don't add weights (i.e. continue to next)
      continue;
    }


    // Set status for other cells
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {

        // Set col info
        IWeights::Entry col(sr.elem->get_id(), ESMC_REGRID_STATUS_MAPPED,
                            wgts[i], 0);

        // Set row info
        IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

        // Put status entry into matrix
        dst_status.InsertRowMergeSingle(row, col);
      }
    }


    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't add weights
    if (num_valid < 1) continue;

    // Append only valid nodes/cells
    std::copy(tmp_nodes.begin(), tmp_nodes.end(), std::back_inserter(sintd_nodes));
    std::copy(tmp_cells.begin(), tmp_cells.end(), std::back_inserter(sintd_cells));

    // Temporary empty col with negatives so unset values
    // can be detected if they sneak through
    IWeights::Entry col_empty(-1, 0, -1.0, 0);

    // Insert fracs into src_frac
    {
      // Allocate column of empty entries
      std::vector<IWeights::Entry> col;
      col.resize(num_valid,col_empty);

      // Put weights into column
      int j=0;
      for (int i=0; i<sr.elems.size(); i++) {
        if (valid[i]==1) {
          col[j].id=sr.elems[i]->get_id();
          col[j].value=areas[i]/src_elem_area;
          j++;
        }
      }

       // Set row info
      IWeights::Entry row(sr.elem->get_id(), 0, 0.0, 0);

      // Put weights into weight matrix
      src_frac.InsertRowMerge(row, col);
    }


    // Put weights into dst_frac and then add
    // Don't do this if there are no user areas
    if (use_dst_frac) {
      for (int i=0; i<sr.elems.size(); i++) {
        if (valid[i]==1) {
            // Set col info
            IWeights::Entry col(sr.elem->get_id(), 0,
                                src_frac2*wgts[i], 0);

            // Set row info
            IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

            // Put weights into weight matrix
            dst_frac.InsertRowMergeSingle(row, col);
        }
      }
    }


    // Calculate source user area adjustment
    double src_user_area_adj=1.0;
    if (src_area_field) {
        const MeshObj &src_elem = *sr.elem;
        double *area=src_area_field->data(src_elem);
        src_user_area_adj=*area/src_elem_area;
    }


    // Put weights into row column and then add
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {

        // Calculate dest user area adjustment
        double dst_user_area_adj=1.0;
        if (dst_area_field) {
          const MeshObj &dst_elem = *(sr.elems[i]);
          double *area=dst_area_field->data(dst_elem);
          if (*area==0.0) Throw() << "0.0 user area in destination grid";
          dst_user_area_adj=dst_areas[i]/(*area);
        }

          // Set col info
          IWeights::Entry col(sr.elem->get_id(), 0,
                              src_user_area_adj*dst_user_area_adj*src_frac2*wgts[i], 0);

          // Set row info
          IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

          // Put weights into weight matrix
          iw.InsertRowMergeSingle(row, col);
      }
    }

  } // for searchresult

#if 0
  if(midmesh != 0)
    compute_midmesh(sintd_nodes, sintd_cells, 2, 3, midmesh);
#endif

}



void calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, Mesh *midmesh, SearchResult &sres, IWeights &iw, IWeights &src_frac, IWeights &dst_frac,
                              struct Zoltan_Struct * zz, bool set_dst_status, WMat &dst_status) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");


  // both meshes have to have the same dimensions
  if (srcmesh.parametric_dim() != dstmesh.parametric_dim()) {
    Throw() << "src and dst mesh must have the same parametric dimension for conservative regridding";
  }

  if (srcmesh.spatial_dim() != dstmesh.spatial_dim()) {
    Throw() << "src and dst mesh must have the same spatial dimension for conservative regridding";
  }

  // If necessary, set status for masked dst cells
  if (set_dst_status) {
    // Get elem mask pointer
    MEField<> *dmptr = dstmesh.GetField("elem_mask");

    // If mask field exists, then mark masked dst elems
    if (dmptr != NULL) {
      MeshDB::const_iterator ei = dstmesh.elem_begin(), ee = dstmesh.elem_end();
      for (; ei != ee; ++ei) {
        const MeshObj &elem=*ei;

        // Get mask value
        double *m=dmptr->data(*ei);
        
        // If masked, then mark
        if (*m > 0.5) {
          // Set col info
          IWeights::Entry col(0,ESMC_REGRID_STATUS_DST_MASKED,
                              1.0, 0);

          // Set row info
          IWeights::Entry row(elem.get_id(), 0, 0.0, 0);

          // Put status entry into matrix
          dst_status.InsertRowMergeSingle(row, col);
        }
      }
    }

  }

  // Get dimension, because they're the same can just get one
  int sdim=srcmesh.spatial_dim();
  int pdim=srcmesh.parametric_dim();

  // Get weights depending on dimension
  if (pdim==2) {
    if (sdim==2) {
      calc_conserve_mat_serial_2D_2D_cart(srcmesh, dstmesh, midmesh, sres, iw,
                                          src_frac, dst_frac, zz,
                                          set_dst_status, dst_status);
    } else if (sdim==3) {
      calc_conserve_mat_serial_2D_3D_sph(srcmesh, dstmesh, midmesh, sres, iw,
                                         src_frac, dst_frac, zz,
                                         set_dst_status, dst_status);
    }
  } else if (pdim==3) {
    if (sdim==3) {
      calc_conserve_mat_serial_3D_3D_cart(srcmesh, dstmesh, midmesh, sres, iw,
                                          src_frac, dst_frac, zz,
                                          set_dst_status, dst_status);
    } else {
      Throw() << "Meshes with parametric dim == 3, but spatial dim !=3 not supported for conservative regridding";
    }
  } else {
     Throw() << "Meshes with parametric dimension != 2 or 3 not supported for conservative regridding";
  }
}



void calc_nearest_mat_serial(PointList *srcpointlist, PointList *dstpointlist, SearchResult &sres, IWeights &iw) {
  Trace __trace("calc_nearest_mat_serial(PointList *srcpointlist, PointList *dstpointlist, SearchResult &sres, IWeights &iw)");


  if (srcpointlist->get_coord_dim() != dstpointlist->get_coord_dim()) {
    Throw() << "src and dst mesh must have the same spatial dimension for nearest regridding";
  }


  // Temporary empty col with negatives so unset values
  // can be detected if they sneak through
  IWeights::Entry col_empty(-1, 0, -1.0, 0);

  // Loop through search results
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    Search_result &sr = **sb;

    // Set col info
    IWeights::Entry col(sr.src_gid, 0, 1.0, 0);

    // Set row info (i.e. the destination id associated with the above weight)
    IWeights::Entry row(sr.dst_gid, 0, 0.0, 0);


    // Put weights into weight matrix
    // Need merge version in nearest src to dest case where there may be more than 1 src,dst pair with the same dst.
    iw.InsertRowMergeSingle(row, col);

  } // for searchresult
}


void calc_nearest_npnts_mat_serial(PointList *srcpointlist, PointList *dstpointlist, ESMC_R8 dist_exponent, SearchResult &sres, IWeights &iw) {
  Trace __trace("calc_nearest_mat_serial(PointList *srcpointlist, PointList *dstpointlist, SearchResult &sres, IWeights &iw)");

  // Make sure dimensions match
  if (srcpointlist->get_coord_dim() != dstpointlist->get_coord_dim()) {
    Throw() << "src and dst must have the same spatial dimension for nearest regridding";
  }

  // Get dimension
  int sdim=srcpointlist->get_coord_dim();

  // Convert to double
  // (Just in case ESMC_R8 is different)
  double dist_exponent_dbl = (double)dist_exponent;

  // Temporary empty col with negatives so unset values
  // can be detected if they sneak through
  IWeights::Entry col_empty(-1, 0, -1.0, 0);

  // Put this outside loop, so it doesn't keep allocating memory every time
  std::vector<IWeights::Entry> cols;

  // Loop through search results
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    Search_result &sr = **sb;

    // Get dst location in dstpointlist
    int dst_loc=sr.dst_gid;

    // Get destination id
    int dst_id=dstpointlist->get_id(dst_loc);

    // Get dst point coords
    const double *coord=dstpointlist->get_coord_ptr(dst_loc);
    double dst_pnt[3];
    dst_pnt[0] = coord[0];
    dst_pnt[1] = coord[1];
    dst_pnt[2] = (sdim == 3 ? coord[2] : 0.0);

    // Set row info (i.e. the destination id)
    IWeights::Entry row(dst_id, 0, 0.0, 0);

    // Clear and then Reserve to potential size
    cols.clear();
    cols.reserve(sr.nodes.size());

    // See if there are any 0.0 dist
    bool no_zero_dist=true;
    for (int i=0; i<sr.nodes.size(); i++) {

      // Get coordinates of src point
      double coord[3];
      coord[0] = sr.nodes[i].pcoord[0];
      coord[1] = sr.nodes[i].pcoord[1];
      coord[2] = (sdim == 3 ? sr.nodes[i].pcoord[2] : 0.0);

      // Calculate distance
      double dist=sqrt((dst_pnt[0]-coord[0])*(dst_pnt[0]-coord[0])+
                       (dst_pnt[1]-coord[1])*(dst_pnt[1]-coord[1])+
                       (dst_pnt[2]-coord[2])*(dst_pnt[2]-coord[2]));

      // There is a 0.0 dist, so record that fact and leave
      if (dist == 0.0) {
        no_zero_dist=false;
        break;
      }
    }

    // Loop calculating weights
    double tot=0.0;
    if (no_zero_dist) {
      for (int i=0; i<sr.nodes.size(); i++) {

        // Get coordinates of src point
        double coord[3];
        coord[0] = sr.nodes[i].pcoord[0];
        coord[1] = sr.nodes[i].pcoord[1];
        coord[2] = (sdim == 3 ? sr.nodes[i].pcoord[2] : 0.0);

        // Calculate distance
        double dist=sqrt((dst_pnt[0]-coord[0])*(dst_pnt[0]-coord[0])+
                         (dst_pnt[1]-coord[1])*(dst_pnt[1]-coord[1])+
                         (dst_pnt[2]-coord[2])*(dst_pnt[2]-coord[2]));

        // This shouldn't happen, so complain
        if (dist == 0.0) {
          Throw() << " zero distance in part of weight calc that's for nonzero.";
        }

        // 1 over dist raised to a power
        double inv_dist=1.0/pow(dist,dist_exponent_dbl);

        // Sum total weights
        tot += inv_dist;

        // Set col entry info
        // NOTE: dst_gid actually contains src_gid
        IWeights::Entry col_entry(sr.nodes[i].dst_gid, 0, inv_dist, 0);

        // Push into
        cols.push_back(col_entry);
      }
    } else {
      // There are 0.0 dist, so just count those
      for (int i=0; i<sr.nodes.size(); i++) {

        // Get coordinates of src point
        double coord[3];
        coord[0] = sr.nodes[i].pcoord[0];
        coord[1] = sr.nodes[i].pcoord[1];
        coord[2] = (sdim == 3 ? sr.nodes[i].pcoord[2] : 0.0);

        // Calculate distance
        double dist=sqrt((dst_pnt[0]-coord[0])*(dst_pnt[0]-coord[0])+
                         (dst_pnt[1]-coord[1])*(dst_pnt[1]-coord[1])+
                         (dst_pnt[2]-coord[2])*(dst_pnt[2]-coord[2]));

        // This is 0.0, so just add that
        if (dist == 0.0) {

          // Set col entry info using 1.0 as weight
          // NOTE: dst_gid actually contains src_gid
          IWeights::Entry col_entry(sr.nodes[i].dst_gid, 0, 1.0, 0);

          // Sum total weights
          tot += 1.0;

          // Push into
          cols.push_back(col_entry);
        }
      }
    }

    // Loop dividing by tot
    for (int i=0; i<cols.size(); i++) {
      cols[i].value=cols[i].value/tot;
    }

#if 0
    // DEBUG
    //   if ((dst_id==7050) || (dst_id==6878) || (dst_id==6880)) {
    if ((dst_id==3737)) {
      printf("wgt calc: dst_id=%d ::",dst_id);
      for (int i=0; i<cols.size(); i++) {
        printf(" %d %g, \n",cols[i].id,cols[i].value);
      }
      printf("\n");
    }
#endif

    // Put weights into weight matrix
    iw.InsertRow(row, cols);

  } // for searchresult
}


 void mat_point_serial_transfer(MEField<> &sfield, SearchResult &sres, IWeights &iw, PointList *dstpointlist) {
  Trace __trace("mat_point_serial_transfer(UInt num_fields, MEField<> *const *sfields, int *iflag, SearchResult &sres)");

  SearchResult::iterator sb = sres.begin(), se = sres.end();

  UInt nrhs = sfield.dim();

  MEField<SField> sF(sfield);

  int dstpointlist_dim=dstpointlist->get_coord_dim();

  for (; sb != se; sb++) {

    Search_result &sres = **sb;

    // Trick:  Gather the data from the source field so we may call interpolate point
    MeshObj &elem = const_cast<MeshObj&>(*(*sb)->elem);

    const MeshObjTopo *etopo = GetMeshObjTopo(elem);

    UInt pdim = etopo->parametric_dim;

    // Create a sensitivity field residing on the degrees of
    // freedom used in this interpolation.

    UInt nlocal_dof = sF.AssignElement(elem);

    std::vector<fad_type> fads(nlocal_dof);

    sF.ReInit(&fads[0]);

    // Set up the fad degrees of freedom.
    for (UInt i = 0; i < nlocal_dof; i++) {
      fads[i].diff(i, nlocal_dof);
    }

    // Inner loop through fields
    MEValues<METraits<fad_type,double>,MEField<SField> > mev(sfield.GetMEFamily());

    if (dstpointlist_dim != sfield.dim())
      Throw() << "dest and source fields have incompatible dimensions";

    // Load Parametric coords
    UInt npts = sres.nodes.size(); // number of points to interpolate
    std::vector<double> pcoord(pdim*npts);
    for (UInt np = 0; np < npts; np++) {
     for (UInt pd = 0; pd < pdim; pd++)
       pcoord[np*pdim+pd] = sres.nodes[np].pcoord[pd];
    }

    arbq pintg(pdim, npts, &pcoord[0]);
    mev.Setup(elem, MEV::update_sf, &pintg);
    mev.ReInit(elem);

    std::vector<fad_type> ires(npts*dstpointlist_dim);
    mev.GetFunctionValues(sF, &ires[0]);

    // Copy data to nodes
    for (UInt n = 0; n < npts; n++) {

      for (UInt d = 0; d < 1; d++) {

        // DON'T ACTUALLY DO REGRID BECAUSE WE DON'T USE IT
        // ((double*)dfield.data(node))[d] = ires[n*dfield.dim()+d].val();

        IWeights::Entry row(sres.nodes[n].dst_gid, d, 0.0, elem.get_id());

        std::vector<IWeights::Entry> col;
        col.reserve(nlocal_dof);

#ifdef CHECK_SENS
std::cout << "sens=" << ires[n*nrhs+d] << std::endl;
double sval = 0;
#endif

        double *sens = &(ires[n*nrhs+d].fastAccessDx(0));

        dof_add_col addc(col, dstpointlist_dim, sens);

        sF.dof_iterator(addc);

        iw.InsertRow(row, col);

#ifdef CHECK_SENS
        for (UInt s = 0; s < nlocal_dof/dstpointlist_dim; s++) {

  sval += fads[s*nrhs+d].val()*sens[s*nrhs+d];
        } // for s

std::cout << "**diff=" << sval - ires[n*nrhs+d].val() << std::endl;
#endif

      } // for d

    }

  } // for searchresul

}

static GeomRend::DstConfig get_dst_config(int imethod) {

  // Determine the rendezvous destination configuration.  Use Field 0 for the info.
  // All other fields must have compatability with the first.


  // Figure out the type of object to gather values for (i.e. nodes, interp, etc...)
  UInt otype=MeshObj::NODE;

  // Loop fieldpairs.  If any have INTERP_PATCH, collect nieghbors
  bool nbor = false;
  bool cnsrv = false;
  bool all_overlap_dst = false;

  if (imethod == Interp::INTERP_PATCH) nbor = true;
  else if (imethod == Interp::INTERP_CONSERVE) cnsrv = true;
  else if (imethod == Interp::INTERP_CONSERVE_2ND) {
    nbor = true;
    cnsrv = true;
    all_overlap_dst = true;
  }

  Context default_context;
  default_context.flip();

  // TODO: make this more general
  if (cnsrv) {
    return GeomRend::DstConfig(MeshObj::ELEMENT,MeshObj::ELEMENT, default_context, nbor, all_overlap_dst);
  } else {
    return GeomRend::DstConfig(MeshObj::ELEMENT, MeshObj::NODE, default_context, nbor);
  }
}


Interp::Interp(Mesh *src, PointList *srcplist, Mesh *dest, PointList *dstplist, Mesh *midmesh,
               bool freeze_src_, int imethod,
               bool set_dst_status, WMat &dst_status,
               MAP_TYPE mtype, int unmappedaction, int _num_src_pnts, ESMC_R8 _dist_exponent):

sres(),
grend(src, srcplist, dest, dstplist, get_dst_config(imethod), freeze_src_, (mtype==MAP_TYPE_GREAT_CIRCLE)),
is_parallel(Par::Size() > 1),
srcF(),
dstF(),
has_std(false),
has_patch(false),
has_cnsrv(false),
has_nearest_src_to_dst(false),
has_nearest_dst_to_src(false),
has_nearest_idavg(false),
num_src_pnts(_num_src_pnts),
dist_exponent(_dist_exponent),
srcmesh(src),
dstmesh(dest),
dstpointlist(dstplist),
srcpointlist(srcplist),
midmesh(midmesh),
zz(0),
interp_method(imethod)
{

  // Different paths for parallel/serial
  UInt search_obj_type = grend.GetDstObjType();

  if (srcmesh != NULL)
    srcF.push_back(srcmesh->GetCoordField());

  iflag.push_back(interp_method);

  if (interp_method == Interp::INTERP_STD) has_std = true;
  else if (interp_method == Interp::INTERP_PATCH) has_patch = true;
  else if (interp_method == Interp::INTERP_CONSERVE) has_cnsrv = true;
  else if (interp_method == Interp::INTERP_CONSERVE_2ND) has_cnsrv = true;
  else if (interp_method == Interp::INTERP_NEAREST_SRC_TO_DST) has_nearest_src_to_dst = true;
  else if (interp_method == Interp::INTERP_NEAREST_DST_TO_SRC) has_nearest_dst_to_src = true;
  else if (interp_method == Interp::INTERP_NEAREST_IDAVG) has_nearest_idavg = true;

  if (dstmesh != NULL) {
    //must get dst info from mesh
    dstF.push_back(dstmesh->GetCoordField());
  }

  if (is_parallel) {

    // Form the parallel rendezvous meshes/specs
   //  if (Par::Rank() == 0)
       //std::cout << "Building rendezvous..." << std::endl;


    grend.Build(srcF.size(), &srcF[0], dstF.size(), &dstF[0], &zz, midmesh==0? true:false);

    if (has_nearest_dst_to_src) {
      Throw() << "unable to proceed with interpolation method dst_to_src";

    } else if (has_nearest_src_to_dst) {
      ParSearchNearestSrcToDst(grend.GetSrcPlistRend(), grend.GetDstPlistRend(), unmappedaction, sres, set_dst_status, dst_status);

      // Redistribute regrid status
      if (set_dst_status) {
        dst_status.Migrate(*dstplist);
      }
    } else if (has_nearest_idavg) {
      ParSearchNearestSrcToDstNPnts(grend.GetSrcPlistRend(), grend.GetDstPlistRend(), num_src_pnts, unmappedaction, sres, set_dst_status, dst_status);

      // Redistribute regrid status
      if (set_dst_status) {
        dst_status.Migrate(*dstplist);
      }
    } else {
      if (search_obj_type == MeshObj::NODE) {
        OctSearch(grend.GetSrcRend(), grend.GetDstPlistRend(), mtype, search_obj_type,
                  unmappedaction, sres, set_dst_status, dst_status, 1e-8);
        // Redistribute regrid status
        if (set_dst_status) {
          dst_status.Migrate(*dstplist);
        }
      } else if (search_obj_type == MeshObj::ELEMENT) {
        //      OctSearchElems(grend.GetDstRend(), unmappedaction, grend.GetSrcRend(), ESMCI_UNMAPPEDACTION_IGNORE, 1e-8, sres);
        if(freeze_src_) {
          OctSearchElems(*src, ESMCI_UNMAPPEDACTION_IGNORE, grend.GetDstRend(), unmappedaction, 1e-8, sres);
        } else {
          OctSearchElems(grend.GetSrcRend(), ESMCI_UNMAPPEDACTION_IGNORE, grend.GetDstRend(), unmappedaction, 1e-8, sres);
        }
      }
    }


    /*
    Par::Out() << "SrcRend **************" << std::endl;
    //grend.GetSrcRend().Print(Par::Out());
    grend.GetSrcRend().Print(std::cout);
    */

  } else {
    // Serial track.  Meshes already in geometric rendezvous.  (Perhaps get
    // the subset of the mesh for interpolating??)

    if (has_nearest_dst_to_src) {
      Throw() << "unable to proceed with interpolation method dst_to_src";
    } else if (has_nearest_src_to_dst) {
      SearchNearestSrcToDst(*srcpointlist, *dstpointlist, unmappedaction, sres, set_dst_status, dst_status);
    } else if (has_nearest_idavg) {
      SearchNearestSrcToDstNPnts(*srcpointlist, *dstpointlist, num_src_pnts, unmappedaction, sres, set_dst_status, dst_status);
    } else {

      if (search_obj_type == MeshObj::NODE) {
        OctSearch(*src, *dstpointlist, mtype, search_obj_type,
                  unmappedaction, sres, set_dst_status, dst_status, 1e-8);
        //OctSearch(src, dest, mtype, search_obj_type, unmappedaction, sres, 1e-8);
      } else if (search_obj_type == MeshObj::ELEMENT) {
        OctSearchElems(*src, ESMCI_UNMAPPEDACTION_IGNORE, *dest, unmappedaction, 1e-8, sres);
      }
    }

     //PrintSearchResult(sres);
  }

  //dstpointlist->diagprint();

}



Interp::~Interp() {
  DestroySearchResult(sres);
}

/*
 * There is an ASSUMPTION here that the field is nodal, both sides
 */
void Interp::operator()(int fpair_num, IWeights &iw, bool set_dst_status, WMat &dst_status) {
  Trace __trace("Interp::operator()(int fpair_num, IWeights &iw)");

  IWeights src_frac,dst_frac; // Use IW to get out source and dst frac and to migrate it to the correct procs
                              // eventually make a dedicated class for migrating values associated with mesh

  if (is_parallel) mat_transfer_parallel(fpair_num, iw, src_frac, dst_frac, set_dst_status, dst_status);
  else mat_transfer_serial(fpair_num, iw, src_frac, dst_frac, set_dst_status, dst_status);

#if 0
  {
  WMat::WeightMap::iterator wi = iw.begin_row(), we = iw.end_row();
  for (; wi != we; ++wi) {
    const WMat::Entry &w = wi->first;

    std::vector<WMat::Entry> &wcol = wi->second;

    // Construct factor index list
    for (UInt j = 0; j < wcol.size(); ++j) {
      const WMat::Entry &wc = wcol[j];

      printf("#%d in () d_id=%d  s_id=%d w=%f \n",Par::Rank(),w.id,wc.id,wc.value);

    } // for j
  } // for wi
  }
#endif

  // Migrate weights back to row decomposition
  // (use node or elem migration depending on interpolation)
  if (!has_cnsrv) {
    if (is_parallel) {

      if (dstpointlist == NULL )
        iw.Migrate(*dstmesh);
      else
        iw.Migrate(*dstpointlist);

    }
  } else {
    if (is_parallel) {
      iw.MigrateToElem(*dstmesh);
      dst_frac.MigrateToElem(*dstmesh);
      if (set_dst_status) {
        dst_status.MigrateToElem(*dstmesh);
      }
    }
  }

  // Migrate src_frac to source mesh decomp
  if (has_cnsrv) {
    if (is_parallel) {
      src_frac.MigrateToElem(*srcmesh);
    }
  }

  //  printf("%d# M1\n",Par::Rank());

  // Set destination fractions
  if (has_cnsrv) {
    // Dectect if we should use explicit dst_frac or just sum weights
    bool use_dst_frac=false;
    if (interp_method == Interp::INTERP_CONSERVE) {
      MEField<> *dst_area_field = dstmesh->GetField("elem_area");
      MEField<> *src_area_field = srcmesh->GetField("elem_area");

      // if users have set their own areas then need to use dst_frac variable
      if (dst_area_field || src_area_field) use_dst_frac=true;

    } else if (interp_method == Interp::INTERP_CONSERVE_2ND) {
      use_dst_frac=true; // Always use dst_frac for higher order conservative
    }

     // get frac pointer for destination mesh
    MEField<> *elem_frac=dstmesh->GetField("elem_frac");
    if (!elem_frac) Throw() << "Meshes involved in Conservative interp should have frac field";

    // Set everything to 0.0 in case a destination element is masked
    // and doesn't show up in weight matrix
    Mesh::iterator ei=dstmesh->elem_begin(),ee=dstmesh->elem_end();
    for (;ei!=ee; ei++) {
      MeshObj &elem = *ei;
      double *f=elem_frac->data(elem);
      *f=0.0;
    }

    // Go through weights calculating and setting dst frac
    WMat::WeightMap::iterator wi, we;
    if (use_dst_frac) {
      wi = dst_frac.begin_row();
      we = dst_frac.end_row();
    }
    else {
      wi = iw.begin_row();
      we = iw.end_row();
    }

    for (; wi != we; ++wi) {
      const WMat::Entry &w = wi->first;
      std::vector<WMat::Entry> &wcol = wi->second;

      // total weights
      double tot=0.0;
      for (UInt j = 0; j < wcol.size(); ++j) {
        WMat::Entry &wc = wcol[j];
        tot += wc.value;
      } // for j

       // find element corresponding to destination point
      Mesh::MeshObjIDMap::iterator mi =  dstmesh->map_find(MeshObj::ELEMENT, w.id);
      if (mi ==dstmesh->map_end(MeshObj::ELEMENT)) {
        Throw() << "Wmat entry not in dstmesh";
      }

      // Get the element
      const MeshObj &dst_elem = *mi;

      // Get frac data
      double *frac=elem_frac->data(dst_elem);

      // Init in case is not locally owned
      *frac=0.0;

      // Only put it in if it's locally owned
      if (!GetAttr(dst_elem).is_locally_owned()) continue;

      // Since weights with no mask should add up to 1.0
      // fraction is tot
      *frac=tot;
    } // for wi
  }

  // Set source fractions
  if (has_cnsrv) {
    // get frac pointer for destination mesh
   MEField<> *elem_frac=srcmesh->GetField("elem_frac");
   if (!elem_frac) Throw() << "Meshes involved in Conservative interp should have frac field";

   // Set everything to 0.0 in case a destination element is masked
   // and doesn't show up in weight matrix
   Mesh::iterator ei=srcmesh->elem_begin(),ee=srcmesh->elem_end();
   for (;ei!=ee; ei++) {
    MeshObj &elem = *ei;
    double *f=elem_frac->data(elem);
    *f=0.0;
   }

   // Go through weights calculating and setting frac
   WMat::WeightMap::iterator wi = src_frac.begin_row(), we = src_frac.end_row();
   for (; wi != we; ++wi) {
     const WMat::Entry &w = wi->first;
     std::vector<WMat::Entry> &wcol = wi->second;

     // total frac
     double tot=0.0;
     for (UInt j = 0; j < wcol.size(); ++j) {
       WMat::Entry &wc = wcol[j];
       tot += wc.value;
     } // for j

     // find element corresponding to destination point
     Mesh::MeshObjIDMap::iterator mi =  srcmesh->map_find(MeshObj::ELEMENT, w.id);
     if (mi ==srcmesh->map_end(MeshObj::ELEMENT)) {
       Throw() << "Wmat entry not in srcmesh";
     }

     // Get the element
     const MeshObj &src_elem = *mi;

     // Get frac data
     double *frac=elem_frac->data(src_elem);

     // Init in case is not locally owned
     *frac=0.0;

     // Only put it in if it's locally owned
     if (!GetAttr(src_elem).is_locally_owned()) continue;

     // Since weights with no mask should add up to 1.0
     // fraction is tot
     *frac=tot;
   } // for wi

  }

}

void Interp::mat_transfer_serial(int fpair_num, IWeights &iw, IWeights &src_frac, IWeights &dst_frac,
                                 bool set_dst_status, WMat &dst_status) {
  Trace __trace("Interp::mat_transfer_serial(int fpair_num)");


  if (interp_method == INTERP_STD) mat_point_serial_transfer(*srcF[fpair_num], sres, iw, dstpointlist);
  else if (interp_method == INTERP_PATCH) mat_patch_serial_transfer(*srcmesh->GetCoordField(), *srcF[fpair_num], sres, srcmesh, iw, dstpointlist);
  else if (interp_method == INTERP_CONSERVE) calc_conserve_mat_serial(*srcmesh, *dstmesh, midmesh, sres, iw, src_frac, dst_frac, zz, set_dst_status, dst_status);
  else if (interp_method == INTERP_NEAREST_SRC_TO_DST) calc_nearest_mat_serial(srcpointlist, dstpointlist, sres, iw);
  else if (interp_method == INTERP_NEAREST_IDAVG) calc_nearest_npnts_mat_serial(srcpointlist, dstpointlist, dist_exponent,  sres, iw);
  else if (interp_method == INTERP_NEAREST_DST_TO_SRC) calc_nearest_mat_serial(srcpointlist, dstpointlist, sres, iw);
  else if (interp_method == INTERP_CONSERVE_2ND) calc_2nd_order_conserve_mat_serial(*srcmesh, *dstmesh, midmesh, sres, iw, src_frac, dst_frac, zz, set_dst_status, dst_status);

}

void Interp::mat_transfer_parallel(int fpair_num, IWeights &iw, IWeights &src_frac, IWeights &dst_frac,
                                 bool set_dst_status, WMat &dst_status) {

  // By all rights, here we don't HAVE to actually perform the interpolation.
  // However, we actually do it as a cross check.

  if (interp_method == INTERP_CONSERVE) {
    calc_conserve_mat_serial(grend.GetSrcRend(),grend.GetDstRend(),
                             midmesh, sres, iw, src_frac, dst_frac, zz, set_dst_status, dst_status);
  } else if (interp_method == INTERP_CONSERVE_2ND) {
    calc_2nd_order_conserve_mat_serial(grend.GetSrcRend(),grend.GetDstRend(),
                             midmesh, sres, iw, src_frac, dst_frac, zz, set_dst_status, dst_status);
  } else if (interp_method == INTERP_NEAREST_SRC_TO_DST) {
    calc_nearest_mat_serial(&(grend.GetSrcPlistRend()), &(grend.GetDstPlistRend()), sres, iw);
  } else if (interp_method == INTERP_NEAREST_IDAVG) {
    calc_nearest_npnts_mat_serial(&(grend.GetSrcPlistRend()), &(grend.GetDstPlistRend()), dist_exponent, sres, iw);
  } else if (interp_method == INTERP_NEAREST_DST_TO_SRC) {
    calc_nearest_mat_serial(srcpointlist, dstpointlist, sres, iw);
  } else {
    // Send source data to rendezvous decomp
    const std::vector<MEField<> *> &src_rend_Fields = grend.GetSrcRendFields();

    MEField<> *sFR = src_rend_Fields[fpair_num];

    // WE ARE ONLY USING MATRIX HERE, SO DON'T NEED TO COMM. VALUES
    // grend.GetSrcComm().SendFields(1, &sF, &sFR);

    // Get fields for bilinear and patch calc.
    // TODO: think about pulling these out of subroutines below
    const std::vector<_field*> &dst_rend_fields = grend.GetDstRendfields();

    // Calc. Matrix for bilinear and patch

    PointList &plist_rend = grend.GetDstPlistRend();

  if (interp_method == INTERP_STD) {
      mat_point_serial_transfer(*sFR, sres, iw, &plist_rend);
  } else if (interp_method == INTERP_PATCH) {
      mat_patch_serial_transfer(*grend.GetSrcRend().GetCoordField(), *sFR, sres, &grend.GetSrcRend(), iw, dstpointlist);
  }
    // WE ARE ONLY USING MATRIX HERE, SO DON'T NEED TO COMM VALUES
    // Retrieve the interpolated data
    //CommRel &dst_node_rel = grend.GetDstComm().GetCommRel(MeshObj::NODE);
    //
    // Send the data back (comm has been transposed in GeomRend::Build)
    //dst_node_rel.send_fields(1, &dfR, &df);
  }
}

/*
 * There is an ASSUMPTION here that the field is nodal, both sides
 */
void Interp::interpL2csrvM(const IWeights &iw, IWeights *iw2,
                          MEField<> const * const src_iwts,
                          MEField<> const * const dst_iwts) {
  Trace __trace("Interp::interpL2csrvM()");

  if (is_parallel)
    interpL2csrvM_parallel(const_cast<IWeights &> (iw), iw2, src_iwts, dst_iwts);
  else interpL2csrvM_serial(iw, iw2, src_iwts, dst_iwts);

}

void Interp::interpL2csrvM_serial(const IWeights &iw, IWeights *iw2,
                          MEField<> const * const src_iwts,
                          MEField<> const * const dst_iwts) {
  Trace __trace("Interp::interpL2csrvM_serial");

//!!!!!!!!!!!!!!! iw is the backward interpolation matrix

  int idx = 0;

  Mesh &smesh = *dstmesh;
  Mesh &dmesh = *srcmesh;

  // sparse matrix multiply transpose
  IWeights::WeightMap::const_iterator wi = iw.begin_row(), we = iw.end_row();
  for (; wi != we; ++wi) {
    const IWeights::Entry &_row = wi->first;
    const std::vector<IWeights::Entry> &_col = wi->second;
    for (UInt c = 0; c < _col.size(); ++c) {
      // look for destination node id matching _col[c].id
      MeshDB::MeshObjIDMap::iterator ndi =
        dmesh.map_find(MeshObj::NODE, _col[c].id);
      ThrowRequire(ndi != dmesh.map_end(MeshObj::NODE));
      // look for source node id matching _row.id
      MeshDB::MeshObjIDMap::iterator nsi =
        smesh.map_find(MeshObj::NODE, _row.id);
      ThrowRequire(nsi != smesh.map_end(MeshObj::NODE));
      // now we have the index and the data, multiply
      double *Ddata = dst_iwts->data(*ndi);
      double *Sdata = src_iwts->data(*nsi);
      double value = ((1/(*Ddata))*(_col[c].value)*(*Sdata));

      int rowT = _col[c].id;
      int colT = _row.id;
      int idx = _col[c].idx;

      // now we need to set this value in new weightyys matrix
      bool found = false;
      IWeights::WeightMap::iterator wi2 = iw2->begin_row(), we2 = iw2->end_row();
      for (; wi2 != we2; ++wi2) {
        // if the row for this value already exists
        if ((wi2->first).id == rowT) {
          std::vector<IWeights::Entry> &_col2 = wi2->second;
          for (UInt c2 = 0; c2 < _col2.size(); ++c2) {
            // if the column for this value already exits, add it in
            if(_col2[c2].id == colT) {
              _col2[c2].value += value;
              found = true;
              break;
            }
          }
          // if this column doesn't exist, add a new entry to this row
          if (found != true) {
            IWeights::Entry newentry(colT, idx, value, rowT);
            _col2.push_back(newentry);
            // sort the row
            std::sort(_col2.begin(), _col2.end());
            found = true;
          }
        }
      }
      // if the row for this value doesn't exist
      if (!found) {
        // need element number, src_id of the rows is the element id that owns the node
        //ESMCI::MeshObjRelationList::iterator fi = ESMCI::MeshObjConn::
        //  find_relation(*ndi,ESMCI::MeshObj::ELEMENT,0,ESMCI::MeshObj::USED_BY);
        //const MeshObj &elem = *((*fi).obj);
        // make the row entry and column vector and column Entry -> insert row
        //IWeights::Entry row(ndi->get_id(), idx, 0.0, elem.get_id());
        IWeights::Entry row(rowT, idx, 0.0, 0.0);
        std::vector<IWeights::Entry> col;
        IWeights::Entry ent(colT, idx, value, rowT);
        col.push_back(ent);
        iw2->InsertRow(row,col);
        found = true;
      }
      if (found != true) {
        printf("Error, no value! dstid=%d  srcid=%d \n",
                                int(ndi->get_id()), int(nsi->get_id()));
      }
    }
  }
/*
  // print out id's of weight matrix
  IWeights::WeightMap::iterator wit = iw2->begin_row(), wet = iw2->end_row();
  for (; wit != wet; ++wit) {
    const IWeights::Entry &_row = wit->first;
    const std::vector<IWeights::Entry> &_col = wit->second;

    std::cout<<_row.id<<"    ";
    for (UInt c = 0; c < _col.size(); ++c)
      std::cout<<std::setprecision(3)<<_col[c].value<<"  ";
    std::cout<<std::endl;
  }
*/
}

void Interp::interpL2csrvM_parallel(IWeights &iw, IWeights *iw2,
                                    MEField<> const * const src_iwts,
                                    MEField<> const * const dst_iwts) {
  Trace __trace("Interp::interpL2csrvM_parallel");

//!!!!!!!!!!!!!!! iw is the backward interpolation matrix

  int idx = 0;
  int id;
  MPI_Comm_rank(MPI_COMM_WORLD, &id);

  // sparse matrix multiply transpose
  IWeights::WeightMap::const_iterator wi = iw.begin_row(), we = iw.end_row();
  for (; wi != we; ++wi) {
    const IWeights::Entry &_row = wi->first;
    const std::vector<IWeights::Entry> &_col = wi->second;
    // look for source node id matching _row.id
    MeshDB::MeshObjIDMap::iterator nsi =
      dstmesh->map_find(MeshObj::NODE, _row.id);
    ThrowRequire(nsi != dstmesh->map_end(MeshObj::NODE));
    double *Sdata = src_iwts->data(*nsi);
    for (UInt c = 0; c < _col.size(); ++c) {
      // now we have the index and the data, multiply
      double value = (_col[c].value)*(*Sdata);

      int rowT = _col[c].id;
      int colT = _row.id;
      int idx = _col[c].idx;

      // now we need to set this value in new weightyys matrix
      bool found = false;
      IWeights::WeightMap::iterator wi2 = iw2->begin_row(), we2 = iw2->end_row();
      for (; wi2 != we2; ++wi2) {
        // if the row for this value already exists
        if ((wi2->first).id == rowT) {
          std::vector<IWeights::Entry> &_col2 = wi2->second;
          for (UInt c2 = 0; c2 < _col2.size(); ++c2) {
            // if the column for this value already exits, add it in
            if(_col2[c2].id == colT) {
              _col2[c2].value += value;
              found = true;
              break;
            }
          }
          // if this column doesn't exist, add a new entry to this row
          if (found != true) {
            IWeights::Entry newentry(colT, idx, value, rowT);
            _col2.push_back(newentry);
            // sort the row
            std::sort(_col2.begin(), _col2.end());
            found = true;
          }
        }
      }
      // if the row for this value doesn't exist
      if (found != true) {
        // need element number, src_id of the rows is the element id that owns the node
        //ESMCI::MeshObjRelationList::iterator fi = ESMCI::MeshObjConn::
        //  find_relation(*ndi,ESMCI::MeshObj::ELEMENT,0,ESMCI::MeshObj::USED_BY);
        //const MeshObj &elem = *((*fi).obj);
        // make the row entry and column vector and column Entry -> insert row
        //IWeights::Entry row(ndi->get_id(), idx, 0.0, elem.get_id());
        IWeights::Entry row(rowT, idx, 0.0, id);
        std::vector<IWeights::Entry> col;
        IWeights::Entry ent(colT, idx, value, rowT);
        col.push_back(ent);
        iw2->InsertRow(row,col);
        found = true;
      }
      if (found != true) {
        printf("Error, no value!  srcid=%d \n",
                                int(nsi->get_id()));
      }
    }
  }


  // now migrate to the destination mesh decomposition (srcmesh in here)
  iw2->Migrate(*srcmesh);
  iw2->Prune(*srcmesh, 0);


  // OK, now we should have the weights matrix in the destination mesh decomposition
  // combine the Entries of duplicate rows
  IWeights::WeightMap::iterator mwi = iw2->begin_row(), mwe = iw2->end_row();
  for (; mwi != mwe; ++mwi) {
    int lastid = mwi->first.id;

    const IWeights::Entry &_row = mwi->first;
    std::vector<IWeights::Entry> &_col = mwi->second;

    IWeights::WeightMap::iterator ri = mwi;
    ++ri;

    for (; ri != mwe;) {
      int thisid = ri->first.id;
      if (thisid == lastid) {
        // get the last row
        const IWeights::Entry &thisrow = ri->first;
        std::vector<IWeights::Entry> &thiscol = ri->second;

        // move all cols to the last row
        for (UInt c3 = 0; c3 < thiscol.size(); ++c3)
          _col.push_back(thiscol[c3]);

        // erase this row
        iw2->weights.erase(ri++);
      } else ++ri;
    }
  }

  // now we need to go through the rows themselves and look for duplicate Entries
  mwi = iw2->begin_row(), mwe = iw2->end_row();
  for (; mwi != mwe; ++mwi) {
    std::vector<IWeights::Entry> &_col = mwi->second;

    // first traverse the row and compact values - don't erase yet
    std::sort(_col.begin(), _col.end());

/*  // apparently this isn't necessary, but it's staying here for a bit until i'm sure..
    // go through the row
    for (UInt c = 0; c < _col.size(); ++c) {
      int lastid = _col[c].id;
      std::vector<IWeights::Entry>::iterator ci = _col.begin()+c, ce = _col.end();
      ++ci;
      // go through the row from the position of c
      for (; ci != ce; ++ci) {
        int thisid = ci->id;
        // if there is a match, add the value to the original
        if (thisid == lastid)
          _col[c].value += ci->value;
      }
    }
*/
    // now remove the Entries with matching id
    std::vector<IWeights::Entry>::iterator newend =
      std::unique(_col.begin(), _col.end(), IDMatch);

    // now erase all Entries between newend and end
    _col.erase(newend, _col.end());
  }

  // left multiply!
  IWeights::WeightMap::iterator wi2 = iw2->begin_row(), we2 = iw2->end_row();
  for(; wi2 != we2; ++wi2) {
    const IWeights::Entry &_row = wi2->first;
    std::vector<IWeights::Entry> &_col = wi2->second;
    MeshDB::MeshObjIDMap::iterator nsi =
      srcmesh->map_find(MeshObj::NODE, _row.id);
    ThrowRequire(nsi != srcmesh->map_end(MeshObj::NODE));
    double *Ddata = dst_iwts->data(*nsi);
    for (UInt c = 0; c < _col.size(); ++c)
      _col[c].value = (_col[c].value)/(*Ddata);
  }

/*
  // print out id's of weight matrix
  std::cout<<"End  id="<<id<<std::endl;
  IWeights::WeightMap::iterator wit = iw2->begin_row(), wet = iw2->end_row();
  for (; wit != wet; ++wit) {
    const IWeights::Entry &_row = wit->first;
    const std::vector<IWeights::Entry> &_col = wit->second;

    std::cout<<_row.id<<"    ";
    for (UInt c = 0; c < _col.size(); ++c)
      std::cout<<std::setprecision(3)<<_col[c].value<<"  ";
    std::cout<<std::endl;
  }
  std::cout<<std::endl;
*/
}

void DestrySearchResult(SearchResult &sres) {

  for (UInt i = 0; i < sres.size(); i++) {
   delete sres[i];
  }

  SearchResult().swap(sres);

}


} // namespace
