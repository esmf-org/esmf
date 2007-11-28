// $Id: ESMC_LinSys.h,v 1.2 2007/11/28 16:43:50 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_LinSys_h
#define ESMC_LinSys_h

#include <ESMC_MeshObj.h>
#include <ESMC_MEField.h>

#ifdef ESMC_TRILINOS
#include <Epetra_Map.h>
#include <Epetra_FECrsMatrix.h>
#include <Epetra_FEVector.h>
#endif

#include <map>

namespace ESMC {

class Mesh;
class Kernel;

// A linear system abstraction class.  Builds a matrix that couples
// the given fields.
class LinSys {
public:

  typedef int DField_type; // type of dof field: use int, since Epetra does.

  LinSys(Mesh &mesh, UInt nfields, MEField<> **fields);

  // Reset the lin sys (same fields, though)
  void clear();

  // Loop the mesh, find dofs, obtain global numbers
  void BuildDofs();

  // Build the sparsity pattern into the global matrix.  If matrix was created
  // previously, it is cleared and then rebuilt.
  void BuildMatrix();

  /**
   * Zero the matrix, create the rhs vector (and zero)
   */
  void BeginAssembly(); 
  void EndAssembly(); 

 /**
  * Solve the linear system.
  */
  void Solve();

  /**
   * Set an entry that constraints the dof to be dirichlet.  Sets I in
   * matrix, and sets val in rhs vector.
   * Should set owned=true on owner, and false if not owned.  Still must call
   * on the non-owner; this proc just zeros everything.
   */
  void Dirichlet(DField_type id, double val, bool owned);
  
  /**
   * Return the number of degrees of freedom on the given kernel.
   * Sets up the map for (field,dim) -> local dof map
   */
  UInt Setup(Kernel &ker);

  /**
   * Call for each element.  Sets up the dofindices mapping.
   */
  void ReInit(MeshObj &elem);

  /**
   * Set up the values of fad coefficients.  Negate if needed.
   */
  void SetFadCoef(UInt offset, std::vector<double> &mcoef, std::vector<fad_type> &fad_coef);

  void PrintMatrix(std::ostream &);

  typedef std::vector<MEField<>*> FieldVec;

  /**
   * Sum a row into the global matrix.  The row should be ordered as
   * field(0,dim=0), field(0,1), ..., field(1,0), field(1,1), etc...
   */
  void SumToGlobal(UInt row, const double mat_row[], double rhs_val);

  // Various iterators
  FieldVec::iterator field_begin() { return Fields.begin(); }
  FieldVec::iterator field_end() { return Fields.end(); }

  FieldVec::const_iterator field_begin() const { return Fields.begin(); }
  FieldVec::const_iterator field_end() const { return Fields.end(); }

private:

  void delete_epetra_objs();
  
  /**
   * Scatter x to the field(s).
   */
  void scatter_sol();

  FieldVec Fields;
  FieldVec DFields;

  std::vector<DField_type> elem_dofs; // Gather expect
  std::vector<int> elem_dof_signs;  // for sign oriented elements
  std::vector<char> field_on;
  UInt nelem_dofs;
  
  template<typename dof_action>
  void loop_dofs(dof_action&);
  
  Mesh &mesh;
#ifdef ESMC_TRILINOS
  Epetra_Map *map, *umap;
  Epetra_FECrsMatrix *matrix;
  Epetra_FEVector *x, *b; 
  std::vector<int> my_global,my_owned;
#endif
};

} // namespace ESMC

#endif

