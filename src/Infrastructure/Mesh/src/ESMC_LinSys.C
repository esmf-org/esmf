// $Id: ESMC_LinSys.C,v 1.1 2007/11/28 16:28:02 dneckels Exp $
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
#include <mesh/ESMC_LinSys.h>
#include <mesh/ESMC_Mesh.h>
#include <mesh/ESMC_MeshObjConn.h>
#include <mesh/ESMC_MeshUtils.h>
#include <mesh/ESMC_GlobalIds.h>
#include <mesh/ESMC_ParEnv.h>
#include <mesh/ESMC_SparseMsg.h>
#include <mesh/ESMC_Exception.h>

#ifdef ESMC_TRILINOS
#include <Epetra_MpiComm.h>
#include <Epetra_FECrsGraph.h>
#include <AztecOO.h>
//#include <Teuchos_ParameterList.hpp>
#define HAVE_IFPACCK_TEUCHOS
#include <Ifpack.h>
#include <Amesos.h>
#include "ml_include.h"
#include "ml_MultiLevelPreconditioner.h"
#endif

namespace ESMC {

LinSys::LinSys(Mesh &_mesh, UInt nfields, MEField<> **fields) :
 Fields(),
 mesh(_mesh),
 elem_dofs(),
 field_on(),
 nelem_dofs(0)
#ifdef ESMC_TRILINOS
 ,
 map(NULL),
 umap(NULL),
 matrix(NULL),
 x(NULL),
 b(NULL)
#endif
{
  for (UInt i = 0; i < nfields; i++) {

    // For the moment, only allow element fields
    if (fields[i]->ObjType() != MeshObj::ELEMENT) 
      Throw() << "Lin Sys currently only supports element fields";
 
    Fields.push_back(fields[i]);
    
    // Clone the field to store the dof numbering into the linalg vector.
    DFields.push_back(mesh.RegisterField("_dof" + fields[i]->name(),
                          fields[i]->GetMEFamily(),
                          fields[i]->GetType(),
                          fields[i]->GetContext(),
                          fields[i]->dim(),
                          false, // no output 
                          false, // no interp 
                          _fieldType<DField_type>::instance()
        ));
  }

}

void LinSys::delete_epetra_objs() {
#ifdef ESMC_TRILINOS
  if (map) {
    delete map;
    map = NULL;
  }
  if (umap) {
    delete umap;
    umap = NULL;
  }
  if (matrix) {
    delete matrix;
    matrix = NULL;
  }
  if (x) {
    delete x; x = NULL;
  }
  if (b) {
    delete b; b = NULL;
  }
#endif
}

void LinSys::clear() {

  delete_epetra_objs();
  
}

UInt LinSys::Setup(Kernel &ker) {
  
  UInt ndofs = 0;
  
  UInt nfields = Fields.size();

  field_on.resize(nfields);
  
  for (UInt f = 0; f < nfields; f++) {
    if ((ker.type() == Fields[f]->GetType())
          && (ker.GetContext().any(Fields[f]->GetContext()))) {
      ndofs += GetME(*Fields[f], ker).num_functions()*Fields[f]->dim();

      field_on[f] = 1;
    } else field_on[f] = 0;
  }  

  nelem_dofs = ndofs;

  elem_dofs.resize(nelem_dofs);
  elem_dof_signs.resize(nelem_dofs, 1);
  
  return ndofs;
}

void LinSys::ReInit(MeshObj &elem) {

  UInt nfields = Fields.size();
  
  UInt cur_loc = 0;

  for (UInt f = 0; f < nfields; f++) {
    if (field_on[f]) {

      UInt ndofs = GetME(*Fields[f], elem).num_functions()*Fields[f]->dim();

      const MasterElement<> &me = static_cast<const MasterElement<>&>(GetME(*DFields[f], elem));
//Par::Out() << "Gathering" << std::endl;
      GatherElemData<METraits<>,MEField<>,DField_type>(me, static_cast<MEField<>&>(*DFields[f]), elem, &elem_dofs[cur_loc]);

      // Some elements may negate the dofs, so adjust this
      if (me.orientation() == MasterElementBase::ME_SIGN_ORIENTED) {
        for (UInt i = 0; i < ndofs; i++) {
          elem_dof_signs[cur_loc+i] = elem_dofs[cur_loc+i] >= 0 ? 1 : -1;
          elem_dofs[cur_loc+i] = std::abs(elem_dofs[cur_loc+i]);
        }
      }

      cur_loc += ndofs;

    } // field on
  }  

  

Par::Out() << "elem dofs, signs:" << std::endl;
std::copy(elem_dofs.begin(), elem_dofs.end(), std::ostream_iterator<DField_type>(Par::Out(), " "));
Par::Out() << std::endl;
std::copy(elem_dof_signs.begin(), elem_dof_signs.end(), std::ostream_iterator<DField_type>(Par::Out(), " "));
Par::Out() << std::endl;
}

void LinSys::BeginAssembly() {

#ifdef ESMC_TRILINOS
 matrix->PutScalar(0); //matrix->FillComplete();
 b->PutScalar(0);
#endif

}

void LinSys::EndAssembly() {
#ifdef ESMC_TRILINOS

  matrix->GlobalAssemble(*umap, *umap);
  //matrix->FillComplete();

  // Sum rhs together
  b->GlobalAssemble();
#endif

}

void LinSys::SumToGlobal(UInt row, const double mat_row[], double rhs_val) {
#ifdef ESMC_TRILINOS 
  /*
Par::Out() << "add row:" << elem_dofs[row] << std::endl;
for (UInt i = 0; i < nelem_dofs; i++) {
  Par::Out() << "\t(" << elem_dofs[i] << ", " << mat_row[i] << ")" << std::endl; 
}
*/
  if (elem_dof_signs[row] < 0) {

for (UInt i = 0; i < nelem_dofs; i++) {
  const_cast<double&>(mat_row[i])*=-1.0;
}

  }

  int ret = matrix->SumIntoGlobalValues(elem_dofs[row],
            static_cast<int>(nelem_dofs), const_cast<double*>(&mat_row[0]), &elem_dofs[0]);

//Par::Out() << "\tret=" << ret << std::endl;
  ThrowRequire(ret == 0);
  
  b->SumIntoGlobalValues(1, &elem_dofs[row], &rhs_val);
#endif
}

void LinSys::SetFadCoef(UInt offset, std::vector<double> &mcoef, std::vector<fad_type> &fad_coef) {

  for (UInt i = 0; i < nelem_dofs; i++) {
    if (elem_dof_signs[i] < 0) {
      fad_coef[offset + i] = -mcoef[i];
      fad_coef[offset + i].diff(offset+i, nelem_dofs);
      fad_coef[offset + i] *= -1.0;
    } else {
      fad_coef[offset + i] = mcoef[i];
      fad_coef[offset + i].diff(offset+i, nelem_dofs);
    }
  }

}

/**
 * Loop the dof field entries.  call dof_action at every object, with nval and fdim,
 * the object, and pointer:
 * dof_act(obj, nval, fdim, fptr)
 */
template <typename dof_action>
void LinSys::loop_dofs(dof_action &dact) {
  
  UInt nfields = Fields.size();
  
  KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();
    for (; ki != ke; ++ki) {
      Kernel &ker = *ki;

      // only process active
      if (!ker.is_active()) continue;

      // Find out which fields live on kernel
      bool on_kernel = false;
      std::vector<int> field_on(nfields,0);

      for (UInt f = 0; f < nfields; f++) {
        if ((ker.type() == Fields[f]->GetType())
              && (field_on[f] = ker.GetContext().any(Fields[f]->GetContext())))
          on_kernel = true;
      }  
    
      if (!on_kernel) continue;
      
      // At least one of the fields is on the kernel, so request id's for the
      // objects in this kernel
      Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();
        for (; oi !=oe; ++oi) {
          MeshObj &elem = *oi;

          for (UInt f = 0; f < nfields; f++) {
            if (field_on[f]) {
  
              MEField<> &mef = static_cast<MEField<>&>(*Fields[f]);
              MEField<> &dmef = static_cast<MEField<>&>(*DFields[f]);
              
              MasterElementBase &meb = GetME( mef, elem);
              
              // Loop dofs
              for (UInt df = 0; df < meb.num_functions(); df++) {
                const int *dd = meb.GetDofDescription(df);

                // Get the object;
                MeshObj *dofobj = NULL;

                if (dof2mtype(dd[0]) != MeshObj::ELEMENT) {
                  MeshObjRelationList::const_iterator ri = 
                     MeshObjConn::find_relation(elem, dof2mtype(dd[0]), dd[1], MeshObj::USES);

                  ThrowRequire(ri != elem.Relations.end());

                  dofobj = ri->obj;

                } else dofobj = &elem;
                
                UInt nval = meb.GetDofValSet(df);
                
                const _field &llf = *dmef.Getfield(nval);
                
                const _field &lf = *mef.Getfield(nval);

                dact(*dofobj, nval, mef.dim(), (DField_type*) llf.data(*dofobj),
                    (double*) lf.data(*dofobj));
                
              } // num_func
            } // field on
          } // nfields
        } // oi
    } // kernels
}

struct dof_action_set {
dof_action_set(LinSys::DField_type val) {_val = val;}
LinSys::DField_type _val;
void operator()(MeshObj &obj, UInt nval, UInt fdim, LinSys::DField_type *dfptr,double*) {
  
  // First zero:
  for (UInt c = 0; c < nval*fdim; c++) dfptr[c] = _val;
}
};

struct dof_action_count {
dof_action_count() { count = 0;}
UInt count;
void operator()(MeshObj &obj, UInt nval, UInt fdim, LinSys::DField_type *dfptr,double*) {
  
  if (GetMeshObjContext(obj).is_set(Attr::OWNED_ID) && dfptr[0] == 0) {
    
    dfptr[0] = 1;
    
    count += nval*fdim;
    
  }
  
}
};

struct dof_action_assign {
dof_action_assign(const std::vector<long> &needed_ids) : _ids(needed_ids), id(0) {}
const std::vector<long> &_ids;
UInt id;
void operator()(MeshObj &obj, UInt nval, UInt fdim, LinSys::DField_type *dfptr,double*) {
  
  if (GetMeshObjContext(obj).is_set(Attr::OWNED_ID) && dfptr[0] == 0) {
    
    for (UInt c = 0; c < nval*fdim; ++c) {
      dfptr[c] = _ids[id++];
//Par::Out() << "set obj:" << obj.get_id() << " to " << _ids[id-1] << std::endl;
    }
    
  }
  
}
};

struct dof_action_print {
void operator()(MeshObj &obj, UInt nval, UInt fdim, LinSys::DField_type *dfptr,double*) {
  
  for (UInt c = 0; c < nval*fdim; ++c) {
    Par::Out() << "obj:" << obj.get_id() << " is " << dfptr[c] << std::endl;
  }
  
}
};

void LinSys::BuildDofs() {
  Trace __trace("LinSys::BuildDofs()");
  
  UInt nfields = Fields.size();

  std::vector<long> used_ids;
  std::vector<long> needed_ids;

  // The local numbering of dof
  int local_id = 0;

  // First zero the dof field.
  {
    dof_action_set das(0);
    loop_dofs(das);
  }
  
  // Next, we count the unique dofs.  We do this by looping and setting a 1
  // in the first meshobj entry of each locally owned object.  If we haven't
  // visited yet, it is zero, and count every dof on the object.
  UInt num_needed = 0;
  {
    dof_action_count dac;
    loop_dofs(dac);
    num_needed = dac.count;
  }
//Par::Out() << "Num needed=" << num_needed << std::endl;

  used_ids.resize(0);
  needed_ids.resize(num_needed);
  
  // Get the global ids.  These are >= 1
  GlobalIds(used_ids, needed_ids);

  // Zero again, since we will use nonzero to mean already assigned
  {
    dof_action_set das(0);
    loop_dofs(das);
  }
  
  {
    dof_action_assign dass(needed_ids);
    loop_dofs(dass);
    ThrowRequire(dass.id == needed_ids.size());
  }

  // Now share the parallel ids.  Can use swap add, since non-locally owned have
  // zero.
  std::vector<MEField<> *> df;
  for (UInt i = 0; i < DFields.size(); i++) {
    df.push_back(static_cast<MEField<> *>(DFields[i]));
  }
  
  mesh.SwapOp<DField_type>(df.size(), &df[0], CommRel::OP_SUM);

/*
  {
    dof_action_print dac;
    loop_dofs(dac);
  }
*/
  
}

struct dof_get_ids {
dof_get_ids(std::vector<int> &my_global, bool _store_local, bool _store_non_local) :
  _ids(my_global), store_local(_store_local), store_non_local(_store_non_local) {}
std::vector<int> &_ids;
bool store_non_local;
bool store_local;
void operator()(MeshObj &obj, UInt nval, UInt fdim, LinSys::DField_type *dfptr,double*) {
  
  bool owned = GetMeshObjContext(obj).is_set(Attr::OWNED_ID);
  if((owned && store_local) || (!owned && store_non_local)) {
    for (UInt c = 0; c < nval*fdim; ++c) {
      std::vector<int>::iterator lb =
        std::lower_bound(_ids.begin(), _ids.end(), dfptr[c]);

      if (lb == _ids.end() || *lb != dfptr[c]) {
        _ids.insert(lb, dfptr[c]);
      }
      
    }
  }
    
}
};

void LinSys::BuildMatrix() {
  Trace __trace("LinSys::BuildMatrix()");
#ifdef ESMC_TRILINOS

  
  delete_epetra_objs(); // in case they are still around.

  UInt nfields = Fields.size();
  // First the epetra map
  my_global.clear();
  my_owned.clear();  
  {
    std::vector<int> tmp;
    dof_get_ids dgi(my_owned, true, false);
    loop_dofs(dgi);
    
    dof_get_ids dgi1(tmp, false, true);
    loop_dofs(dgi1);

    std::sort(my_owned.begin(), my_owned.end());
    std::sort(tmp.begin(), tmp.end());
    
    std::copy(my_owned.begin(), my_owned.end(), std::back_inserter(my_global));
    std::copy(tmp.begin(), tmp.end(), std::back_inserter(my_global));
    
    // Fit memory to vector.
    std::vector<int>(my_global).swap(my_global);
  }
  // Now sort my_global for lookup, later
//std::copy(my_global.begin(), my_global.end(), std::ostream_iterator<int>(Par::Out(), "\n")); 
  
  Epetra_MpiComm comm(Par::Comm());

  map = new Epetra_Map(-1, my_global.size(), &my_global[0], 0, comm);

  umap = new Epetra_Map(-1, my_owned.size(), &my_owned[0], 0, comm);


  x = new Epetra_FEVector(*map);
  b = new Epetra_FEVector(*umap);

  // Now to the matrix
  // Loop elements.  Build a vector of DofKey for each element
  int max_df = 0;
  // Loop twice.  First time, just find the max_df.  Second time build matrix.
  for (UInt lp = 0; lp < 2; lp++) {

  if (lp == 1) {
    max_df = 300;
    Par::Out() << "max_df=" << max_df << std::endl;
    matrix = new Epetra_FECrsMatrix(Copy, *map, max_df); // 100 = approx entries per row??
  }

  KernelList::iterator ki = mesh.set_begin(), ke = mesh.set_end();
    for (; ki != ke; ++ki) {
      Kernel &ker = *ki;

      // only process active
      if (!ker.is_active()) continue;


      // Find out which fields live on kernel
      bool on_kernel = false;
      std::vector<int> field_on(nfields,0);

      for (UInt f = 0; f < nfields; f++) {
        if ((ker.type() == Fields[f]->GetType())
              && (field_on[f] = ker.GetContext().any(Fields[f]->GetContext())))
          on_kernel = true;
      }
    
      if (!on_kernel) continue; 

      // At least one of the fields is on the kernel, so request id's for the
      // objects in this kernel
      Kernel::obj_iterator oi = ker.obj_begin(), oe = ker.obj_end();

        if (lp == 0) { if(oi != oe) {oe = oi; ++oe;}}
        for (; oi !=oe; ++oi) {
          MeshObj &elem = *oi;

          std::vector<int> dofindices;

          for (UInt f = 0; f < nfields; f++) {
            if (field_on[f]) {
  
              MEFieldBase &mef = *DFields[f];
             
              UInt fdim = mef.dim();
              
              MEField<> &dmef = static_cast<MEField<>&>(*DFields[f]);

              MasterElementBase &meb = GetME( mef, elem);
              
              // Loop dofs
              for (UInt df = 0; df < meb.num_functions(); df++) {
                const int *dd = meb.GetDofDescription(df);

                // Get the object;
                MeshObj *dofobj = NULL;

                if (dof2mtype(dd[0]) != MeshObj::ELEMENT) {
                  MeshObjRelationList::const_iterator ri = 
                     MeshObjConn::find_relation(elem, dof2mtype(dd[0]), dd[1], MeshObj::USES);

                  ThrowRequire(ri != elem.Relations.end());

                  dofobj = ri->obj;

                } else dofobj = &elem;

                UInt nval = meb.GetDofValSet(df);
                
                const _field &llf = *dmef.Getfield(nval);
 
                DField_type *dft = llf.data(*dofobj);
                
                for (UInt d = 0; d < fdim; d++) {

                  // Push back the global id
                  dofindices.push_back(dft[dd[2]*fdim+d]);

                } // for d

              } // dofs

            } // field on obj
          } // for fields
          // Now that we have the dofkeys, add the rows of the matrix; for each
          // owned key, add all others.

          int msize = dofindices.size();
          std::vector<double> vals(msize*msize, 1.1);
          
          /*
Par::Out() << "insert global rows:";
for (UInt i = 0; i < msize; i++) {
  Par::Out() << dofindices[i] << " ";
}
Par::Out() << std::endl;
    */
   
          if (lp == 1) {
            if (matrix->InsertGlobalValues(msize, &dofindices[0], 
              &vals[0]) != 0) {
                 Throw() << "Error inserting matrix values:"
                     << "msize=" << msize;
            }

          }
          else {
            if (msize > max_df) max_df = msize;
          }

        } // for oi on kernel

    
    } // for kernels
    } //lp

//Par::Out() << *matrix << std::endl;
    //matrix->GlobalAssemble();  
    //matrix->FillComplete();

#else
    Throw() << "This function requires trilinos";
#endif
}

void LinSys::PrintMatrix(std::ostream &os) {
#ifdef ESMC_TRILINOS

  os << "*** Matrix *** " << std::endl;
  os << *matrix << std::endl; 

  os << "*** rhs ***" << std::endl;
  os << *b << std::endl;
#endif
}

void LinSys::Solve() {
#ifdef ESMC_TRILINOS
#ifdef NOT
                                 // The Direct option selects the Amesos solver.
  if (solver_params.SOLVER == solver_params_type::DIRECT) {
   
                                 // Setup for solving with
                                 // Amesos.
     Epetra_LinearProblem prob;
     prob.SetOperator(Matrix);
     Amesos_BaseSolver *solver;
     Amesos Factory;

                                 // Other solvers are available
                                 // and may be selected by changing this
                                 // string.
     char *stype = "Amesos_Klu";

     solver = Factory.Create(stype, prob);

     Assert (solver != NULL, ExcInternalError());

                                 // There are two parts to the direct solve.
                                 // As I understand, the symbolic part figures
                                 // out the sparsity patterns, and then the
                                 // numerical part actually performs Gaussian
                                 // elimination or whatever the approach is.
     if (solver_params.OUTPUT == solver_params_type::VERBOSE)
       std::cout << "Starting Symbolic fact\n" << std::flush;

     solver->SymbolicFactorization();

     if (solver_params.OUTPUT == solver_params_type::VERBOSE)
         std::cout << "Starting Numeric fact\n" << std::flush;

     solver->NumericFactorization();

    
                                 // Define the linear problem by setting the
                                 // right hand and left hand sides.
     prob.SetRHS(&b);
     prob.SetLHS(&x);
                                 // And finally solve the problem.
     if (solver_params.OUTPUT == solver_params_type::VERBOSE)
       std::cout << "Starting solve\n" << std::flush;
     solver->Solve();
     niter = 0;
     lin_residual = 0;

                                 // We must free the solver that was created
                                 // for us.
     delete solver;

  } else if (solver_params.SOLVER == solver_params_type::GMRES) {
#endif

  Par::Out() << "*** Beginning linear solve ***" << std::endl;
struct solver_params_type {
 enum {QUIET = 0, VERBOSE=1};

 UInt OUTPUT;
 double ILUT_DROP; 
 double ILUT_FILL;
 double ILUT_ATOL;
 double ILUT_RTOL;
 int MAX_ITERS;
 double RES;
};

solver_params_type solver_params;
solver_params.OUTPUT = solver_params_type::VERBOSE;
solver_params.ILUT_DROP = 1e-12;
solver_params.ILUT_FILL = 3.0;
solver_params.ILUT_ATOL = 1e-12;
solver_params.ILUT_RTOL = 1.00;
solver_params.MAX_ITERS = 1000;
solver_params.RES = 1e-12;

                                 // For the iterative solvers, we use Aztec.
    AztecOO Solver;

                                 // Select the appropriate level of verbosity.
    if (solver_params.OUTPUT == solver_params_type::QUIET)
    Solver.SetAztecOption(AZ_output, AZ_none);

    if (solver_params.OUTPUT == solver_params_type::VERBOSE)
    Solver.SetAztecOption(AZ_output, AZ_all);

                                 // Select gmres.  Other solvers are available.
    Solver.SetAztecOption(AZ_solver, AZ_gmres);
    Solver.SetRHS(b);
    Solver.SetLHS(x);

                                 // Set up the ILUT preconditioner.  I do not know
                       // why, but we must pretend like we are in parallel
                                 // using domain decomposition or the preconditioner
                                 // refuses to activate.
    Solver.SetAztecOption(AZ_precond, AZ_dom_decomp);
    Solver.SetAztecOption(AZ_subdomain_solve, AZ_ilut);
    Solver.SetAztecOption(AZ_overlap, 10);
    Solver.SetAztecOption(AZ_reorder, 1);

                                 // ILUT parameters as described above.
    Solver.SetAztecParam(AZ_drop, solver_params.ILUT_DROP);
    Solver.SetAztecParam(AZ_ilut_fill, solver_params.ILUT_FILL);
    Solver.SetAztecParam(AZ_athresh, solver_params.ILUT_ATOL);
    Solver.SetAztecParam(AZ_rthresh, solver_params.ILUT_RTOL);
    
#ifdef NOT
    Teuchos::ParameterList MList;
    // default values for smoothed aggregation
    ML_Epetra::SetDefaults("SA",MLList);
    MLList.set("max levels",6);
    MLList.set("increasing or decreasing","decreasing");
    MLList.set("aggregation: type", "MIS");
    MLList.set("coarse: type","Amesos_KLU");
    ML_Epetra::MultiLevelPreconditioner * MLPrec =
    new ML_Epetra::MultiLevelPreconditioner(A, MLList, true);
    solver.SetPrecOperator(MLPrec);
#endif
    
    Solver.SetUserMatrix(matrix);

                                 // Run the solver iteration.  Collect the number
                                 // of iterations and the residual.
    Solver.Iterate(solver_params.MAX_ITERS, solver_params.RES);
    UInt niter = Solver.NumIters();
    double lin_residual = Solver.TrueResidual();

  Par::Out() << "*** End linear solve ***, res=" << lin_residual << std::endl;
  
  scatter_sol();
 #endif 
}

void LinSys::Dirichlet(DField_type row_id, double val, bool owned) {
#ifdef ESMC_TRILINOS
  
  owned = true;
  
  std::vector<double> new_row;
  double *old_row;
  int *old_indices;
  int nentries = 0;
  double diag_val;

  int ret = matrix->ExtractGlobalRowView(row_id, nentries, old_row, old_indices);
 
if (nentries == 0) return;
  if (ret == -2)
    Throw() << "Extract view failed.  Perhaps EndAssemble not called before dirichlet??";
  
  //ThrowRequire(ret == 0);
  
  Par::Out() << "Extract ret=" << ret << std::endl;

  if (nentries <= 0)
    Throw() << "nentries==0, for row_id=" << row_id;

  new_row.resize(nentries, 0.0);

  // Find my index
  int k;
  for (k = 0; k < nentries; k++) {
    if (old_indices[k] == row_id) break;
  }
  ThrowRequire(k < nentries);

  new_row[k] = owned ? 1.0 : 0.0;
  
Par::Out() << "Replacing row:" << row_id;
for (UInt i = 0; i < nentries; ++i) {
  Par::Out() << "\tidx:" << old_indices[i] << ", oldval:" << old_row[i] << " -> " << new_row[i] << std::endl;
}
Par::Out() << std::endl;
  
  matrix->ReplaceGlobalValues(row_id, nentries, &new_row[0], old_indices);

  if (!owned) val = 0.0;
  x->ReplaceGlobalValues(1, &row_id, &val);

  double rhs_val = val;
  b->ReplaceGlobalValues(1, &row_id, &rhs_val);

#endif
}

#ifdef ESMC_TRILINOS
struct dof_scatter_sol {
dof_scatter_sol(Epetra_FEVector &_x, std::vector<int> &_my_g) :x(_x), my_g(_my_g) {}

void operator()(MeshObj &obj, UInt nval, UInt fdim, LinSys::DField_type *dfptr,double *fval) {
  
    for (UInt c = 0; c < nval*fdim; ++c) {
      
      int idx = static_cast<int>(dfptr[c]);
      
      std::vector<int>::iterator lb =
        std::lower_bound(my_g.begin(), my_g.end(), idx);
      
      if (lb == my_g.end() || *lb != idx) continue;
      /*
      if (lb == my_g.end() || *lb != idx) {

        Par::Out() << "my_g:";
        std::copy(my_g.begin(), my_g.end(), std::ostream_iterator<int>(Par::Out(), "\n"));
        Par::Out() << "Could not find idx:" << idx << std::endl;
        Throw() << "did not find idx:" << idx;
      }
      */
      
      UInt off = std::distance(my_g.begin(), lb);
      

      
      fval[c] = x[0][off];
      //std::cout << "scatter dof:" << idx << ", off=" << off << " val ="  << fval[c] << std::endl;
      
    }
    
}

Epetra_FEVector &x;
std::vector<int> &my_g;
};
#endif


void LinSys::scatter_sol() {
#ifdef ESMC_TRILINOS
  
  //Par::Out() << "x=" << *x << std::endl;

  dof_scatter_sol dss(*x, my_owned);
  
  loop_dofs(dss);

  //mesh.SwapOp<double>(Fields.size(), &Fields[0], CommRel::OP_SUM);
  mesh.HaloFields(Fields.size(), &Fields[0]);
#endif
}


} // namespace
