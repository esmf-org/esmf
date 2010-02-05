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

#include <iostream>
#include <fstream>
#include <cmath>

//#define CHECK_SENS



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
  for (UInt i = 0; i < fpairs.size() && nbor == false; i++) {
    if (fpairs[i].idata == Interp::INTERP_PATCH) nbor = true;
  }
  
  return GeomRend::DstConfig(repF.ObjType(), otype, repF.GetContext(), nbor);
}
  
Interp::Interp(Mesh &src, Mesh &dest, const std::vector<FieldPair> &_fpairs, int unmappedaction) :
sres(),
grend(src, dest, get_dst_config(dest, _fpairs)),
fpairs(_fpairs),
is_parallel(Par::Size() > 1),
srcF(),
dstF(),
has_std(false),
has_patch(false),
srcmesh(src),
dstmesh(dest)
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
    grend.Build(srcF.size(), &srcF[0], dstF.size(), &dstF[0]);
    
#ifdef MYSEARCH
//    if (Par::Rank() == 0) std::cout << "Start search" << std::endl;
    Search(grend.GetSrcRend(), grend.GetDstRend(), grend.GetDstObjType(), unmappedaction, sres, 1e-8);
//    if (Par::Rank() == 0) std::cout << "end search" << std::endl;
#else
/*
     if (Par::Rank() == 0)
       std::cout << "Starting search..." << std::endl;
*/
    OctSearch(grend.GetSrcRend(), grend.GetDstRend(), grend.GetDstObjType(), unmappedaction, sres, 1e-8);
  //   if (Par::Rank() == 0)
       //std::cout << "Done with search..." << std::endl;
#endif
    /*
    Par::Out() << "SrcRend **************" << std::endl;
    grend.GetSrcRend().Print(Par::Out());
    */
    
  } else {
    // Serial track.  Meshes already in geometric rendezvous.  (Perhaps get
    // the subset of the mesh for interpolating??)

#ifdef MYSEARCH
//    if (Par::Rank() == 0) std::cout << "Start search" << std::endl;
    Search(src, dest, search_obj_type, unmappedaction, sres, 1e-8);
//    if (Par::Rank() == 0) std::cout << "end search" << std::endl;
#else
/*
     if (Par::Rank() == 0)
       std::cout << "Starting search..." << std::endl;
*/
    OctSearch(src, dest, search_obj_type, unmappedaction, sres, 1e-8);
/*
     if (Par::Rank() == 0)
       std::cout << "Done with search..." << std::endl;
*/
#endif
     

     //PrintSearchResult(sres);
  }
  
  // Update has_[std/patch] flags
  for (UInt j = 0; j < fpairs.size(); j++) {
    if (fpairs[j].idata == Interp::INTERP_STD) has_std = true;
    if (fpairs[j].idata == Interp::INTERP_PATCH) has_patch = true;
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
  
  if (is_parallel) mat_transfer_parallel(fpair_num, iw); else mat_transfer_serial(fpair_num, iw);
  
  // Migrate weights back to row decomposition
  if (is_parallel) {
    iw.Migrate(dstmesh);
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

void Interp::mat_transfer_serial(int fpair_num, IWeights &iw) {
  Trace __trace("Interp::mat_transfer_serial(int fpair_num)");

  FieldPair &fpair = fpairs[fpair_num];
  
  // Only implemented for nodal to nodal.  Higher interpolation must
  // use the full interpolation framework.
  ThrowRequire(fpair.first->is_nodal() && fpair.second->is_nodal());
  
  if (fpair.idata == INTERP_STD) mat_point_serial_transfer(*fpair.first, *fpair.second->GetNodalfield(), sres, iw);
  else if (fpair.idata == INTERP_PATCH) mat_patch_serial_transfer(*srcmesh.GetCoordField(), *fpair.first, *fpair.second->GetNodalfield(), sres, srcmesh, iw);
    
}

void Interp::mat_transfer_parallel(int fpair_num, IWeights &iw) {
    
  // By all rights, here we don't HAVE to actually perform the interpolation.
  // However, we actually do it as a cross check.
    
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
  
}

void DestrySearchResult(SearchResult &sres) {
  
  for (UInt i = 0; i < sres.size(); i++) {
   delete sres[i]; 
  }
  
  SearchResult().swap(sres);
  
}
  
  
} // namespace
