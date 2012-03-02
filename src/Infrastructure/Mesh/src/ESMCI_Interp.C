// $Id: ESMCI_Interp.C,v 1.38 2012/03/02 01:56:48 feiliu Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_Interp.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_Search.h>
#include <Mesh/include/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_MEValues.h>
#include <Mesh/include/ESMCI_PatchRecovery.h>
#include <Mesh/include/ESMCI_MeshField.h>
#include <Mesh/include/ESMCI_CommRel.h>
#include <Mesh/include/ESMCI_MeshOBjConn.h>
#include <Mesh/include/ESMCI_Migrator.h>
#include <Mesh/include/ESMCI_MeshObj.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_ConserveInterp.h>
#include <Mesh/include/ESMCI_Sintdnode.h>
#include <Mesh/include/ESMCI_XGridUtil.h>


#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>

#include "ESMCI_Macros.h"

//#define CHECK_SENS

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_Interp.C,v 1.38 2012/03/02 01:56:48 feiliu Exp $";
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
void patch_serial_transfer(MEField<> &src_coord_field, UInt _nfields, MEField<>* const* _sfields, _field* const *_dfields, const std::vector<Interp::FieldPair> &fpairs, SearchResult &sres, Mesh &srcmesh) {
   Trace __trace("patch_serial_transfer(MEField<> &src_coord_field, UInt _nfields, MEField<>* const* _sfields, _field* const *_dfields, int *iflag, SearchResult &sres)");


   std::set<int> pdeg_set;
   std::map<int, ElemPatch<>*> patch_map;
   
  if (_nfields == 0) return;

  // Get mask field pointer
  MEField<> *src_mask_ptr = srcmesh.GetField("mask");


  std::vector<MEField<>* > fields;
  std::vector<_field* > dfields;
  std::vector<int> orders;
  UInt nrhs = 0;
  
  for (UInt i = 0; i < _nfields; i++) {
  
    // Only process interp_patch  
    if (fpairs[i].idata != Interp::INTERP_PATCH) continue;
   
    pdeg_set.insert(fpairs[i].patch_order);

    //ThrowRequire(_sfields[i]->is_nodal());
  
    fields.push_back(_sfields[i]); dfields.push_back(_dfields[i]);
    orders.push_back(fpairs[i].patch_order);
    
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
  
void point_serial_transfer(UInt num_fields, MEField<> *const *sfields, _field *const *dfields, int *iflag, SearchResult &sres) {
  Trace __trace("point_serial_transfer(UInt num_fields, MEField<> *const *sfields, _field *const *dfields, int *iflag, SearchResult &sres)");
  

  if (num_fields == 0) return;

  SearchResult::iterator sb = sres.begin(), se = sres.end();

  for (; sb != se; sb++) {

    Search_result &sres = **sb;

    // Trick:  Gather the data from the source field so we may call interpolate point
    const MeshObj &elem = *(*sb)->elem;

    UInt pdim = GetMeshObjTopo(elem)->parametric_dim;

    // Inner loop through fields
    for (UInt i = 0; i < num_fields; i++) {

      if (iflag[i] != Interp::INTERP_STD) continue;

      MEField<> &sfield = *sfields[i];
      _field &dfield = *dfields[i];

      MEValues<> mev(sfield.GetMEFamily());

      if (dfield.dim() != sfield.dim())
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

     std::vector<double> ires(npts*dfield.dim());
     mev.GetFunctionValues(sfield, &ires[0]);

     // Copy data to nodes
     for (UInt n = 0; n < npts; n++) {
       const MeshObj &node = *sres.nodes[n].node;
       for (UInt d = 0; d < dfield.dim(); d++)
         ((double*)dfield.data(node))[d] = ires[n*dfield.dim()+d];
     }

    } // for fields

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
void mat_patch_serial_transfer(MEField<> &src_coord_field, MEField<> &_sfield, _field &_dfield, SearchResult &sres,  Mesh &srcmesh, IWeights &iw) {
  Trace __trace("mat_patch_serial_transfer(MEField<> &sfield, _field &dfield, SearchResult &sres, IWeights &iw)");
    
  const int pdeg = 2; // TODO: deduce this.  For instance, deg source + 1 is reasonable

  // Get mask field pointer
  MEField<> *src_mask_ptr = srcmesh.GetField("mask");
    
  MEField<>* field = &_sfield;
  MEField<> &sfield = _sfield;
  _field* dfield = &_dfield;
  UInt nrhs = field->dim();
  
  
  // Create the sensitivity field
  MEField<SField> sF(sfield);
  MEField<SField> *sFp = &sF;
  
  // Create the  recovery field
  SearchResult::iterator sb = sres.begin(), se = sres.end();

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
    
      const MeshObj &snode = *sres.nodes[n].node;
    
      // DEBUG printf(">>>> snode=%d# elem=%d \n",snode.get_id(),elem.get_id());


        if (dfield->OnObj(snode)) {
          
     //     UInt fdim = dfield->dim();
          double *data = dfield->data(snode);
    
          //for (UInt d = 0; d < fdim; d++) {
          for (UInt d = 0; d < 1; d++) { // weights are redundant per entry
            
            data[d] = result[n*nrhs+d].val();
        
            IWeights::Entry row(snode.get_id(), d, 0.0, elem.get_id());
        
            std::vector<IWeights::Entry> col;
            col.reserve(nlocal_dof);
#ifdef CHECK_SENS       
Par::Out() << "sens=" << result[n*nrhs+d] << std::endl;

double sval = 0;
#endif

            double *sens = &(result[n*nrhs+d].fastAccessDx(0));
            
            dof_add_col addc(col, dfield->dim(), sens);
            
            sF.dof_iterator(addc);
            
            iw.InsertRow(row, col);
            
#ifdef CHECK_SENS
            int dof_div_dim = nlocal_dof/dfield->dim();
            for (UInt s = 0; s < dof_div_dim; s++) {
          
  sval += fads[s*nrhs+d].val()*sens[s*nrhs+d];
            } // for s
        
double diff = sval - result[n*nrhs+d].val();
Par::Out() << "**diff=" << diff << std::endl;
if (std::fabs(diff) > 1e-4) {
for (UInt s = 0; s < nlocal_dof/dfield->dim(); s++) {
  Par::Out() << fads[s*nrhs+d].val() << " ";
}
Par::Out() << std::endl;
}
  
#endif

        } // for d

      } // if data on node

    } // for np

  } // for searchresult
}


#if 0
void calc_conserve_mat_serial_2D_2D_cart(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");
    
  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get src coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

  double tot=0.0;


  // Loop through search results
  SearchResult::iterator sb = sres.begin(), se = sres.end();
  for (; sb != se; sb++) {
    
    // NOTE: sr.elem is a dst element and sr.elems is a list of src elements
    Search_result &sr = **sb;

    // Declare weight vector
    std::vector<int> valid;
    std::vector<double> wgts;

    // Allocate space for weight calc output arrays
    valid.resize(sr.elems.size(),0);
    wgts.resize(sr.elems.size(),0.0);

    // Calculate weights
    calc_1st_order_weights_2D_2D_cart(sr.elem,dst_cfield,sr.elems,src_cfield,
			   &valid, &wgts);

    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't add weights
    if (num_valid < 1) continue;

    // Temporary empty col with negatives so unset values
    // can be detected if they sneak through
    IWeights::Entry col_empty(-1, 0, -1.0, 0);

    // Allocate column of empty entries
    std::vector<IWeights::Entry> col;
    col.resize(num_valid,col_empty);

    // Put weights into column
    int j=0;
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {
	col[j].id=sr.elems[i]->get_id();
	col[j].value=wgts[i];
	j++;
      }
    }

    // Set row info
    IWeights::Entry row(sr.elem->get_id(), 0, 0.0, 0);

    // Put weights into weight matrix
    iw.InsertRow(row, col);       

  } // for searchresult


}
#endif

void calc_conserve_mat_serial_2D_2D_cart(Mesh &srcmesh, Mesh &dstmesh, Mesh *midmesh, SearchResult &sres, IWeights &iw, IWeights &src_frac, struct Zoltan_Struct * zz) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");
    
  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

  // Get dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");

  // Get src mask field
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");

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
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) {
          continue; // if this is masked, then go to next search result
          // TODO: put code in ESMCI_Search.C, so the masked source elements, don't get here
        }
    }

    // Declare src_elem_area
    double src_elem_area;

    // Declare weight vector
    // TODO: Move these out of the loop, to save the time of allocating them
    std::vector<int> valid;
    std::vector<double> wgts;
    std::vector<double> areas;

    // Allocate space for weight calc output arrays
    valid.resize(sr.elems.size(),0);
    wgts.resize(sr.elems.size(),0.0);
    areas.resize(sr.elems.size(),0.0);

    // Calculate weights
    calc_1st_order_weights_2D_2D_cart(sr.elem,src_cfield,sr.elems,dst_cfield,
                                     &src_elem_area, &valid, &wgts, &areas, 
                                     midmesh, &sintd_nodes, &sintd_cells, zz);

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

    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }


    // If none valid, then don't add weights
    if (num_valid < 1) continue;

    

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

    
    // Put weights into row column and then add
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {

	// Allocate column of empty entries
	std::vector<IWeights::Entry> col;
	col.resize(1,col_empty);

	col[0].id=sr.elem->get_id();
	col[0].value=wgts[i];

	// Set row info
	IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

	// Put weights into weight matrix
	iw.InsertRowMerge(row, col);       
      }
    }

  } // for searchresult

  if(midmesh != 0)
    compute_midmesh(sintd_nodes, sintd_cells, 2, 2, midmesh);
}

void calc_conserve_mat_serial_2D_3D_sph(Mesh &srcmesh, Mesh &dstmesh, Mesh *midmesh, SearchResult &sres, IWeights &iw, IWeights &src_frac, struct Zoltan_Struct * zz) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");
    
  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

  // Get dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");

  // Get src mask field
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");

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
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) {
          continue; // if this is masked, then go to next search result
          // TODO: put code in ESMCI_Search.C, so the masked source elements, don't get here
        }
    }

    // Declare src_elem_area
    double src_elem_area;

    // Declare weight vector
    // TODO: Move these out of the loop, to save the time of allocating them
    std::vector<int> valid;
    std::vector<double> wgts;
    std::vector<double> areas;

    // Allocate space for weight calc output arrays
    valid.resize(sr.elems.size(),0);
    wgts.resize(sr.elems.size(),0.0);
    areas.resize(sr.elems.size(),0.0);

    // Calculate weights
    calc_1st_order_weights_2D_3D_sph(sr.elem,src_cfield,sr.elems,dst_cfield,
                                     &src_elem_area, &valid, &wgts, &areas,
                                     midmesh, &sintd_nodes, &sintd_cells, zz);

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

    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't add weights
    if (num_valid < 1) continue;

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

    
    // Put weights into row column and then add
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {

	// Allocate column of empty entries
	std::vector<IWeights::Entry> col;
	col.resize(1,col_empty);

	col[0].id=sr.elem->get_id();
	col[0].value=wgts[i];

	// Set row info
	IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

	// Put weights into weight matrix
	iw.InsertRowMerge(row, col);       
      }
    }

  } // for searchresult

  if(midmesh != 0)
    compute_midmesh(sintd_nodes, sintd_cells, 2, 3, midmesh);

}



void calc_conserve_mat_serial_3D_3D_cart(Mesh &srcmesh, Mesh &dstmesh, Mesh *midmesh, SearchResult &sres, IWeights &iw, IWeights &src_frac, struct Zoltan_Struct * zz) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");
    
  // Get src coord field
  MEField<> *src_cfield = srcmesh.GetCoordField();

  // Get dst coord field
  MEField<> *dst_cfield = dstmesh.GetCoordField();

  // Get dst mask field
  MEField<> *dst_mask_field = dstmesh.GetField("elem_mask");

  // Get src mask field
  MEField<> *src_mask_field = srcmesh.GetField("elem_mask");

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
    if (src_mask_field) {
        const MeshObj &src_elem = *sr.elem;
        double *msk=src_mask_field->data(src_elem);
        if (*msk>0.5) {
          continue; // if this is masked, then go to next search result
          // TODO: put code in ESMCI_Search.C, so the masked source elements, don't get here
        }
    }

    // Declare src_elem_area
    double src_elem_area;

    // Declare weight vector
    // TODO: Move these out of the loop, to save the time of allocating them
    std::vector<int> valid;
    std::vector<double> wgts;
    std::vector<double> areas;

    // Allocate space for weight calc output arrays
    valid.resize(sr.elems.size(),0);
    wgts.resize(sr.elems.size(),0.0);
    areas.resize(sr.elems.size(),0.0);

    // Calculate weights
    calc_1st_order_weights_3D_3D_cart(sr.elem,src_cfield,sr.elems,dst_cfield,
                                     &src_elem_area, &valid, &wgts, &areas,
                                     midmesh, &sintd_nodes, &sintd_cells, zz);

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

    // Count number of valid weights
    int num_valid=0;
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) num_valid++;
    }

    // If none valid, then don't add weights
    if (num_valid < 1) continue;

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

    
    // Put weights into row column and then add
    for (int i=0; i<sr.elems.size(); i++) {
      if (valid[i]==1) {

	// Allocate column of empty entries
	std::vector<IWeights::Entry> col;
	col.resize(1,col_empty);

	col[0].id=sr.elem->get_id();
	col[0].value=wgts[i];

	// Set row info
	IWeights::Entry row(sr.elems[i]->get_id(), 0, 0.0, 0);

	// Put weights into weight matrix
	iw.InsertRowMerge(row, col);       
      }
    }

  } // for searchresult

#if 0
  if(midmesh != 0)
    compute_midmesh(sintd_nodes, sintd_cells, 2, 3, midmesh);
#endif

}


void calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, Mesh *midmesh, SearchResult &sres, IWeights &iw, IWeights &src_frac, struct Zoltan_Struct * zz) {
  Trace __trace("calc_conserve_mat_serial(Mesh &srcmesh, Mesh &dstmesh, SearchResult &sres, IWeights &iw)");

  // both meshes have to have the same dimensions
  if (srcmesh.parametric_dim() != dstmesh.parametric_dim()) {
    Throw() << "src and dst mesh must have the same parametric dimension for conservative regridding";
  }

  if (srcmesh.spatial_dim() != dstmesh.spatial_dim()) {
    Throw() << "src and dst mesh must have the same spatial dimension for conservative regridding";
  }

  // Get dimension, because they're the same can just get one
  int sdim=srcmesh.spatial_dim();
  int pdim=srcmesh.parametric_dim();

  // Get weights depending on dimension
  if (pdim==2) {
    if (sdim==2) {
      calc_conserve_mat_serial_2D_2D_cart(srcmesh, dstmesh, midmesh, sres, iw, src_frac, zz);
    } else if (sdim==3) {
      calc_conserve_mat_serial_2D_3D_sph(srcmesh, dstmesh, midmesh, sres, iw, src_frac, zz);
    }
  } else if (pdim==3) {
    if (sdim==3) {
      calc_conserve_mat_serial_3D_3D_cart(srcmesh, dstmesh, midmesh, sres, iw, src_frac, zz);
    } else {
      Throw() << "Meshes with parametric dim == 3, but spatial dim !=3 not supported for conservative regridding";
    }
  } else {
    Throw() << "Meshes with parametric dimension != 2 or 3 not supported for conservative regridding";
  }
}


 
 /* Matrix version of point serial transfer */
 void mat_point_serial_transfer(MEField<> &sfield, _field &dfield, SearchResult &sres, IWeights &iw) {
  Trace __trace("mat_point_serial_transfer(UInt num_fields, MEField<> *const *sfields, _field *const *dfields, int *iflag, SearchResult &sres)");
  

  SearchResult::iterator sb = sres.begin(), se = sres.end();

  UInt nrhs = sfield.dim();
  
  MEField<SField> sF(sfield);

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

    if (dfield.dim() != sfield.dim())
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

    std::vector<fad_type> ires(npts*dfield.dim());
    mev.GetFunctionValues(sF, &ires[0]);
 
    // Copy data to nodes
    for (UInt n = 0; n < npts; n++) {
      
      const MeshObj &node = *sres.nodes[n].node;
      
      
      for (UInt d = 0; d < 1; d++) {
        
        ((double*)dfield.data(node))[d] = ires[n*dfield.dim()+d].val();
        
        IWeights::Entry row(node.get_id(), d, 0.0, elem.get_id());
        
        std::vector<IWeights::Entry> col;
        col.reserve(nlocal_dof);
 
#ifdef CHECK_SENS       
std::cout << "sens=" << ires[n*nrhs+d] << std::endl;
double sval = 0;
#endif

        double *sens = &(ires[n*nrhs+d].fastAccessDx(0));

        dof_add_col addc(col, dfield.dim(), sens);
        
        sF.dof_iterator(addc);
        
        iw.InsertRow(row, col);       
        
#ifdef CHECK_SENS
        for (UInt s = 0; s < nlocal_dof/dfield.dim(); s++) {
          
  sval += fads[s*nrhs+d].val()*sens[s*nrhs+d];
        } // for s
        
std::cout << "**diff=" << sval - ires[n*nrhs+d].val() << std::endl;
#endif

      } // for d
        
    }

  } // for searchresult

}
 
static GeomRend::DstConfig get_dst_config(Mesh &dest, const std::vector<Interp::FieldPair> &fpairs) {
  
  // Determine the rendezvous destination configuration.  Use Field 0 for the info.  
  // All other fields must have compatability with the first.
  
  ThrowRequire(fpairs.size() > 0);
  
  MEField<> &repF = *fpairs[0].second;
  
  // Figure out the type of object to gather values for (i.e. nodes, interp, etc...)
  UInt otype;
  if (repF.is_elemental()) {
    otype = MeshObj::ELEMENT;
  } else if (repF.is_nodal()) {
    otype = MeshObj::NODE;
  } else {
    otype = MeshObj::NODE | MeshObj::INTERP;
  }
  
  // Loop fieldpairs.  If any have INTERP_PATCH, collect nieghbors
  bool nbor = false;
  bool cnsrv = false;
  for (UInt i = 0; i < fpairs.size() && nbor == false; i++) {
    if (fpairs[i].idata == Interp::INTERP_PATCH) nbor = true;
    if (fpairs[i].idata == Interp::INTERP_CONSERVE) cnsrv = true;
  }
  
  // TODO: make this more general
  if (cnsrv) {
    return GeomRend::DstConfig(MeshObj::ELEMENT,MeshObj::ELEMENT, repF.GetContext(), nbor);
  } else {
    return GeomRend::DstConfig(repF.ObjType(), otype, repF.GetContext(), nbor);
  }
}
  
Interp::Interp(Mesh &src, Mesh &dest, Mesh *midmesh, bool freeze_src_, const std::vector<FieldPair> &_fpairs, int unmappedaction) :
sres(),
grend(src, dest, get_dst_config(dest, _fpairs), freeze_src_),
fpairs(_fpairs),
is_parallel(Par::Size() > 1),
srcF(),
dstF(),
has_std(false),
has_patch(false),
has_cnsrv(false),
srcmesh(src),
dstmesh(dest),
midmesh(midmesh),
zz(0)
{


  // Different paths for parallel/serial
  UInt search_obj_type = grend.GetDstObjType();
  
     
  for (UInt i = 0; i < fpairs.size(); i++) {
    srcF.push_back(fpairs[i].first);
    dstF.push_back(fpairs[i].second);
    
    ThrowRequire(fpairs[i].second->is_nodal() || fpairs[i].second->GetInterp());
    
    dstf.push_back(fpairs[i].second->is_nodal() ? fpairs[i].second->GetNodalfield() : fpairs[i].second->GetInterp());
    iflag.push_back(fpairs[i].idata);
  }
  
  if (is_parallel) {
   
    // Form the parallel rendezvous meshes/specs
   //  if (Par::Rank() == 0)
       //std::cout << "Building rendezvous..." << std::endl;
    grend.Build(srcF.size(), &srcF[0], dstF.size(), &dstF[0], &zz, midmesh==0? true:false);
    
    if (search_obj_type == MeshObj::NODE) {
      OctSearch(grend.GetSrcRend(), grend.GetDstRend(), grend.GetDstObjType(), unmappedaction, sres, 1e-8);
    } else if (search_obj_type == MeshObj::ELEMENT) {
      //      OctSearchElems(grend.GetDstRend(), unmappedaction, grend.GetSrcRend(), ESMCI_UNMAPPEDACTION_IGNORE, 1e-8, sres);
      if(freeze_src_)
        OctSearchElems(src, ESMCI_UNMAPPEDACTION_IGNORE, grend.GetDstRend(), unmappedaction, 1e-8, sres);
      else
        OctSearchElems(grend.GetSrcRend(), ESMCI_UNMAPPEDACTION_IGNORE, grend.GetDstRend(), unmappedaction, 1e-8, sres);
    }

    /*
    Par::Out() << "SrcRend **************" << std::endl;
    //grend.GetSrcRend().Print(Par::Out());
    grend.GetSrcRend().Print(std::cout);
    */
    
  } else {
    // Serial track.  Meshes already in geometric rendezvous.  (Perhaps get
    // the subset of the mesh for interpolating??)

    if (search_obj_type == MeshObj::NODE) {
      OctSearch(src, dest, search_obj_type, unmappedaction, sres, 1e-8);
    } else if (search_obj_type == MeshObj::ELEMENT) {
      //      OctSearchElems(dest, unmappedaction, src, ESMCI_UNMAPPEDACTION_IGNORE, 1e-8, sres);
      OctSearchElems(src, ESMCI_UNMAPPEDACTION_IGNORE, dest, unmappedaction, 1e-8, sres);
    }


     //PrintSearchResult(sres);
  }
  
  // Update has_[std/patch] flags
  for (UInt j = 0; j < fpairs.size(); j++) {
    if (fpairs[j].idata == Interp::INTERP_STD) has_std = true;
    if (fpairs[j].idata == Interp::INTERP_PATCH) has_patch = true;
    if (fpairs[j].idata == Interp::INTERP_CONSERVE) has_cnsrv = true;
  }
  
}
  
Interp::~Interp() {
  DestroySearchResult(sres);
}

void Interp::operator()() {
  Trace __trace("Interp::operator()()");  
  
  if (is_parallel) transfer_parallel(); else transfer_serial();
  
}

/*
 * There is an ASSUMPTION here that the field is nodal, both sides
 */
void Interp::operator()(int fpair_num, IWeights &iw) {
  Trace __trace("Interp::operator()(int fpair_num, IWeights &iw)");
  

  ThrowRequire((UInt) fpair_num < fpairs.size());

  IWeights src_frac; // Use IW to get out source frac and to migrate it to the correct procs
                    // eventually make a dedicated class for migrating values associated with mesh

  if (is_parallel) mat_transfer_parallel(fpair_num, iw, src_frac); else mat_transfer_serial(fpair_num, iw, src_frac);

  // Migrate weights back to row decomposition 
  // (use node or elem migration depending on interpolation)
  if (!has_cnsrv) {
    if (is_parallel) {
      iw.Migrate(dstmesh);
    }
  } else {
    if (is_parallel) {
      iw.MigrateToElem(dstmesh);
    }
  }

  // Migrate src_frac to source mesh decomp
  if (has_cnsrv) {
    if (is_parallel) {
      src_frac.MigrateToElem(srcmesh);
    }
  }


  //  printf("%d# M1\n",Par::Rank());

  // Set destination fractions
  if (has_cnsrv) {
    // get frac pointer for destination mesh
   MEField<> *elem_frac=dstmesh.GetField("elem_frac");
   if (!elem_frac) Throw() << "Meshes involved in Conservative interp should have frac field";

   // Set everything to 0.0 in case a destination element is masked
   // and doesn't show up in weight matrix
   Mesh::iterator ei=dstmesh.elem_begin(),ee=dstmesh.elem_end();
   for (;ei!=ee; ei++) {
    MeshObj &elem = *ei;
    double *f=elem_frac->data(elem);
    *f=0.0;
   }

   // Go through weights calculating and setting frac
   WMat::WeightMap::iterator wi = iw.begin_row(), we = iw.end_row();
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
     Mesh::MeshObjIDMap::iterator mi =  dstmesh.map_find(MeshObj::ELEMENT, w.id);
     if (mi ==dstmesh.map_end(MeshObj::ELEMENT)) {
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
   MEField<> *elem_frac=srcmesh.GetField("elem_frac");
   if (!elem_frac) Throw() << "Meshes involved in Conservative interp should have frac field";

   // Set everything to 0.0 in case a destination element is masked
   // and doesn't show up in weight matrix
   Mesh::iterator ei=srcmesh.elem_begin(),ee=srcmesh.elem_end();
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
     Mesh::MeshObjIDMap::iterator mi =  srcmesh.map_find(MeshObj::ELEMENT, w.id);
     if (mi ==srcmesh.map_end(MeshObj::ELEMENT)) {
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


void Interp::transfer_serial() {
  Trace __trace("Interp::transfer_serial()");
  
  // Standard interpolation
  if (fpairs[0].second->is_nodal() && has_std) {

    point_serial_transfer(srcF.size(), &(*srcF.begin()), &(*dstf.begin()), &iflag[0], sres);
  
  } else if (!fpairs[0].second->is_nodal()) {
  
    Throw() << "Non nodal serial not yet implemented";
  
  }
  
  // Patch interpolation
  if (has_patch) {
    
    patch_serial_transfer(*srcmesh.GetCoordField(), srcF.size(), &(*srcF.begin()), &(*dstf.begin()), fpairs, sres,srcmesh);
  
  } else {
  
  
  }
  
}

void Interp::transfer_parallel() {
  Trace __trace("Interp::transfer_parallel()");
  
  // Send source data to rendezvous decomp
  const std::vector<MEField<> *> &src_rend_Fields = grend.GetSrcRendFields();
  
  ThrowRequire(src_rend_Fields.size() == fpairs.size());
  
  grend.GetSrcComm().SendFields(src_rend_Fields.size(), &(*srcF.begin()), &(*src_rend_Fields.begin()));
  
  // Perform the interpolation
  const std::vector<_field*> &dst_rend_fields = grend.GetDstRendfields();
  
  ThrowRequire(dst_rend_fields.size() == src_rend_Fields.size());
  
  if (has_std) point_serial_transfer(dst_rend_fields.size(), &(*src_rend_Fields.begin()), &(*dst_rend_fields.begin()), &iflag[0], sres);
  if (has_patch) patch_serial_transfer(*grend.GetSrcRend().GetCoordField(), src_rend_Fields.size(), &(*src_rend_Fields.begin()), &(*dst_rend_fields.begin()), fpairs, sres,srcmesh);
  
  // Retrieve the interpolated data
  CommRel &dst_node_rel = grend.GetDstComm().GetCommRel(MeshObj::NODE);

  // Send the data back (comm has been transposed in GeomRend::Build)
  ThrowRequire(dstf.size() == dst_rend_fields.size());
  dst_node_rel.send_fields(dstf.size(), &(*dst_rend_fields.begin()), &(*dstf.begin()));
  
  // TODO:Process interpolation (if needed).  This means, for instance, collecting the values 
  // received above and asking the master element to manufacture coefficients.
  
}

void Interp::mat_transfer_serial(int fpair_num, IWeights &iw, IWeights &src_frac) {
  Trace __trace("Interp::mat_transfer_serial(int fpair_num)");

  FieldPair &fpair = fpairs[fpair_num];
  
  // Only implemented for nodal to nodal.  Higher interpolation must
  // use the full interpolation framework.
  ThrowRequire(fpair.first->is_nodal() && fpair.second->is_nodal());
  
  if (fpair.idata == INTERP_STD) mat_point_serial_transfer(*fpair.first, *fpair.second->GetNodalfield(), sres, iw);
  else if (fpair.idata == INTERP_PATCH) mat_patch_serial_transfer(*srcmesh.GetCoordField(), *fpair.first, *fpair.second->GetNodalfield(), sres, srcmesh, iw);
  else if (fpair.idata == INTERP_CONSERVE) calc_conserve_mat_serial(srcmesh, dstmesh, midmesh, sres, iw, src_frac, zz);
    
}

void Interp::mat_transfer_parallel(int fpair_num, IWeights &iw, IWeights &src_frac) {
    
  // By all rights, here we don't HAVE to actually perform the interpolation.
  // However, we actually do it as a cross check.
    
  if (fpairs[fpair_num].idata != INTERP_CONSERVE) {
    // Send source data to rendezvous decomp
    const std::vector<MEField<> *> &src_rend_Fields = grend.GetSrcRendFields();
    
    MEField<> *sFR = src_rend_Fields[fpair_num], *sF = srcF[fpair_num];
    
    grend.GetSrcComm().SendFields(1, &sF, &sFR);
    
    // Perform the interpolation
    const std::vector<_field*> &dst_rend_fields = grend.GetDstRendfields();
    
    _field *dfR = dst_rend_fields[fpair_num], *df = dstf[fpair_num]; 
    
    if (fpairs[fpair_num].idata == INTERP_STD)
      mat_point_serial_transfer(*sFR, *dfR, sres, iw);
    else if (fpairs[fpair_num].idata == INTERP_PATCH)
      mat_patch_serial_transfer(*grend.GetSrcRend().GetCoordField(), *sFR, *dfR, sres, srcmesh, iw);
    
    // Retrieve the interpolated data
    CommRel &dst_node_rel = grend.GetDstComm().GetCommRel(MeshObj::NODE);
    
    // Send the data back (comm has been transposed in GeomRend::Build)
    dst_node_rel.send_fields(1, &dfR, &df);
  } else calc_conserve_mat_serial(grend.GetSrcRend(),grend.GetDstRend(), midmesh, sres, iw, src_frac, zz);

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

  Mesh &smesh = dstmesh;
  Mesh &dmesh = srcmesh;

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
      dstmesh.map_find(MeshObj::NODE, _row.id);
    ThrowRequire(nsi != dstmesh.map_end(MeshObj::NODE));
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
  iw2->Migrate(srcmesh);
  iw2->Prune(srcmesh, 0);

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
      srcmesh.map_find(MeshObj::NODE, _row.id);
    ThrowRequire(nsi != srcmesh.map_end(MeshObj::NODE));
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
