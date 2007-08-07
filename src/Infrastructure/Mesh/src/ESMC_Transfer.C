// $Id: ESMC_Transfer.C,v 1.1 2007/08/07 17:48:02 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_Transfer.h>
#include <ESMC_PatchRecovery.h>
#include <ESMC_MeshUtils.h>
#include <ESMC_MeshObjConn.h>
#include <ESMC_Quadrature.h>
#include <ESMC_MEValues.h>

#include <iostream>
#include <iterator>

namespace ESMCI {
namespace MESH {

template<class fiter>
void Transfer(fiter fields_begin, fiter fields_end, SearchResult &sres, bool low) {

  if (fields_begin == fields_end) return;

  SearchResult::iterator sb = sres.begin(), se = sres.end();

  for (; sb != se; sb++) {
    Search_result &sres = **sb;
    // Trick:  Gather the data from the source field so we may call interpolate point
    const MeshObj &elem = *(*sb)->elem;
//std::cout << "Transfer: elem:" << elem.get_id() << std::endl;
    UInt pdim = GetMeshObjTopo(elem)->parametric_dim;
    // Inner loop through fields
    fiter fi = fields_begin, fe = fields_end;
    for (; fi != fe; ++fi) {
      MEField<> &sfield = *fi->fields.first;
      MEField<> &dfield = *fi->fields.second;
      MEValues<> mev(sfield.GetMEFamily(), sfield);
      if (!dfield.is_nodal())
        throw("wrong field type in transfer");
      if (dfield.dim() != sfield.dim()) 
        throw("dest and source fields have incompatible dimensions");

     // Load Parametric coords
     UInt npts = sres.nodes.size(); // number of points to interpolate
     std::vector<double> pcoord(pdim*npts);
     for (UInt np = 0; np < npts; np++) {
       for (UInt pd = 0; pd < pdim; pd++)
         pcoord[np*pdim+pd] = sres.nodes[np].pcoord[pd];
     }

     arbq pintg(pdim, npts, &pcoord[0]);
     mev.Setup(elem, 0, &pintg);
     mev.ReInit(elem);

     // We can now call interpolate point
     std::string mename = GetMeshObjTopo(elem)->name;
     if (low) mename += "_L";

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

template<class fiter>
void RTransfer(UInt pdeg, fiter fields_begin, fiter fields_end, SearchResult &sres, MEField<> &coord, bool low) {

  if (fields_begin == fields_end) return;

  std::vector<MEField<>* > fields;
  fiter fi = fields_begin;
  UInt nrhs = 0;
  for (; fi != fields_end; ++fi) {
    fields.push_back(fi->fields.first);
    if (fi->fields.first->dim() != fi->fields.second->dim())
      Throw() << "RTansfer, fields dims not equal:"
         << fi->fields.first->name() << ", " <<  fi->fields.second->name();
  
    nrhs += fi->fields.first->dim(); // how many rhs for recovery
  }


  // Create the  recovery field
  SearchResult::iterator sb = sres.begin(), se = sres.end();

  for (; sb != se; sb++) {
    Search_result &sres = **sb;
    // Trick:  Gather the data from the source field so we may call interpolate point
    const MeshObj &elem = *(*sb)->elem;
//std::cout << "Transfer: elem:" << elem.get_id() << std::endl;

    ElemPatch<> epatch;
 
    epatch.CreateElemPatch(pdeg, ElemPatch<>::GAUSS_PATCH,
                           elem,
                           coord,
                           fields.size(),
                           &fields[0],
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

    std::vector<double> result(nrhs*npts);
    epatch.Eval(npts, &pc[0], &result[0]);

    // Now copy data into fields
    for (UInt np = 0; np < npts; np++) {
      const MeshObj &snode = *sres.nodes[np].node;
      UInt cur_field = 0;
      for (fi = fields_begin; fi != fields_end; ++fi) {
        const MEField<> &dfield = *fi->fields.second;
        if (dfield().OnObj(snode)) {
          UInt fdim = dfield.dim();
          double *data = dfield.data(snode);
          for (UInt f = 0; f < fdim; f++) {
            data[f] = result[np*nrhs+(cur_field+f)];
          }
        }
        cur_field++;
      }
    }

  } // for searchresult

}

template<typename fiter>
void RMatrixTransfer(UInt pdeg, fiter fields_begin, fiter fields_end,
         SearchResult &sres, MEField<> &coord, bool low) 
{
  if (fields_begin == fields_end) return;
#ifdef NOT

  std::vector<SField* > fields;
  UInt nrhs = 0;
  {
    fiter fi = fields_begin;
    for (; fi != fields_end; ++fi) {
      fields.push_back(new SField(*fi->fields.first));
      if (fi->fields.first->dim() != fi->fields.second->dim())
        Throw() << "RTansfer, fields dims not equal:"
           << fi->fields.first->name() << ", " <<  fi->fields.second->name();
    
      nrhs += fi->fields.first->dim(); // how many rhs for recovery
    }
  }


  // Create the  recovery field
  SearchResult::iterator sb = sres.begin(), se = sres.end();

  for (; sb != se; sb++) {

    Search_result &sres = **sb;
    // Trick:  Gather the data from the source field so we may call interpolate point
    const MeshObj &elem = *(*sb)->elem;
//std::cout << "Transfer: elem:" << elem.get_id() << std::endl;

    // Go out to all elements around this one and create the nodal field for these
    // guys
    std::set<const MeshObj*> pnodes;
    MeshObjConn::PatchNodes(elem, pnodes);
    // Set up (fake) fields with data.  Also initialize the fad with local dofs.
    UInt num_pnodes = pnodes.size();
    UInt nlocal_dof = nrhs*num_pnodes;
    std::vector<fad_type> fads(nlocal_dof);
    {
      UInt nfields = fields.size();
      UInt cur_loc = 0;
      for (UInt f = 0; f < nfields; f++) {
        SField &sf = *fields[f];
        sf.Reset();
        sf.SetData(pnodes.begin(), pnodes.end(), &fads[cur_loc]);
        // And now set dofs.  We set things up so a given field has its own
        // dof numbering, since regridding many fields, we dont want each sensitive
        // to each other.
        UInt nfsens = num_pnodes*sf.dim();
        for (UInt i = 0; i < nfsens; i++)
           sf.FadBase()[i].diff(i, nfsens);
        cur_loc += nfsens;
      }
    }
    
    ElemPatch<MEField<SField> > epatch;
 
    const MEFamilyLow &meL = MEFamilyLow::instance();
    const MEFamilyStd &meS = MEFamilyStd::instance();
    const MEFamily *mep;
    if (low) mep = &meL; else mep = &meS;
    epatch.CreateElemPatch(pdeg, ElemPatch<>::GAUSS_PATCH,
                           elem, fields.size(),
                           &fields[0],
                           *mep,
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

    // Now copy data into fields
    for (UInt np = 0; np < npts; np++) {
      const MeshObj &snode = *sres.nodes[np].node;
      UInt cur_field = 0;
      for (fiter fi = fields_begin; fi != fields_end; ++fi) {
        const MEField<> &dfield = *fi->fields.second;
        UInt fdim = dfield.dim();
        double *data = dfield.data(snode);
        for (UInt f = 0; f < fdim; f++) {
          data[f] = result[np*nrhs+(cur_field+f)].val();
std::cout << "sens=" << result[np*nrhs+(cur_field+f)] << std::endl;
// check sensitivites
double sval = 0;
double *sens = &(result[np*nrhs+(cur_field+f)].fastAccessDx(0));
for (UInt s = 0; s < fields[cur_field]->NumFad(); s++) {
  sval += fields[cur_field]->FadBase()[s].val()*sens[s];
}
std::cout << "**diff=" << sval - result[np*nrhs+(cur_field+f)].val() << std::endl;
        }
        cur_field++;
      }
    }

  } // for searchresult

  // clean up sensitivity fields.
  std::vector<SField* >::iterator sfi = fields.begin(), sfe = fields.end();
  for (; sfi != sfe; ++sfi) {
    delete *sfi;
  }
#endif
}

template void Transfer(TransferPair*, TransferPair*, SearchResult &, bool);

template void RTransfer(UInt pdeg, TransferPair*, TransferPair*, SearchResult &sres, MEField<> &coord, bool low);

template void RMatrixTransfer(
            UInt pdeg, TransferPair* fields_begin, TransferPair* fields_end,
            SearchResult &sres, MEField<> &coord, bool low);

}// namespace
}// namespace
