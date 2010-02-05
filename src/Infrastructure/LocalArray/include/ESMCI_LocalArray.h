// $Id: ESMCI_LocalArray.h,v 1.11.2.1 2010/02/05 19:58:23 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_LocalArray_H
#define ESMCI_LocalArray_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::LocalArray - uniform access to arrays from F90 and C++
//
// !DESCRIPTION:
//
// The code in this file defines the C++ LocalArray class and declares method
// signatures (prototypes).  The companion file {\tt ESMCI\_LocalArray.C}
// contains the full code (bodies) for the {\tt LocalArray} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMC_Base.h"

//-------------------------------------------------------------------------

namespace ESMCI {

  // classes and structs

  class LocalArray;

  // The following two numbers are sufficiently sized to accomodate 
  // the Fortran dope vector on all of the platforms we have tested.
  // The numbers are only used to allocate sufficient memory in the
  // the LocalArray structure, _not_ to do any actual memcpy() operation!
  // The Fortran dope vector is copied via a portable implementation
  // that passes pointers to the dope vectors to the Fortran side 
  // to do the actual copy.
#define ESMF_FPTR_BASE_SIZE         160
#define ESMF_FPTR_PLUS_RANK         64
  
  struct c_F90ptr {
    // Dummy structure which is at least as big as the Fortran pointer 
    // dope vector on any platform.
    unsigned char pad[ESMF_FPTR_BASE_SIZE + ESMF_MAXDIM * ESMF_FPTR_PLUS_RANK];
  };

  // this must stay in sync with the Fortran counter-part
  typedef enum{ 
    FROM_FORTRAN = 1, 
    FROM_CPLUSPLUS 
  }LocalArrayOrigin; 

  // this must stay in sync with the Fortran counter-part
  typedef enum{ 
    DATA_COPY = 1, 
    DATA_REF,
    DATA_DEFER,
    DATA_SPACE,
    DATA_NONE
  }CopyFlag;


  // class definition
  class LocalArray{

   protected:
    ESMC_TypeKind typekind;         // I1, I2, I4, I8, R4, R8
    int rank;                       // 1, 2, ..., ESMF_MAXDIM
    LocalArrayOrigin origin;        // create called from Fortran or C++?
    bool dealloc;                   // responsible for deallocation?
    bool contig;                    // optimization possible if all contiguous
    int offset[ESMF_MAXDIM];        // byte offset from base to 1st element/dim
    int lbound[ESMF_MAXDIM];        // used for fortran indexing
    int ubound[ESMF_MAXDIM];        // used for fortran indexing
    int counts[ESMF_MAXDIM];        // number of elements/dim
    int dimOff[ESMF_MAXDIM];        // Offset size per dim for computing pos.
    int lOff;                       // Offset due to lower bounds for pos. 
    int bytestride[ESMF_MAXDIM];    // byte spacing between elements/dim
    int byte_count;                 // size (in bytes) of data region
    void *base_addr;                // start of data allocation
    struct c_F90ptr f90dopev;       // opaque object which is real f90 ptr
                                    // this is memcpy'd to save and restore 
                                    // contents are not interpreted by esmf
    
  public:    
   private:
    // construct() and destruct()
    int construct(bool aflag, CopyFlag docopy,
      ESMC_TypeKind tk, int irank, LocalArrayOrigin oflag, bool dflag,
      const int *offsets, const int *lbounds, const int *ubounds,
      const int *icounts, void *ibase_addr, struct c_F90ptr *f90ptr,
      const char *name);
    int destruct();

   public:
    // create() and destroy()
    static LocalArray *create(ESMC_TypeKind tk, int rank,
      LocalArrayOrigin oflag, const char *name = NULL, int *rc = NULL);
    static LocalArray *create(ESMC_TypeKind tk, int rank, const int *counts,
      void *base_addr = NULL, const char *name = NULL,
      CopyFlag docopy = DATA_REF, int *rc = NULL);
    static LocalArray *create(ESMC_TypeKind dk, int rank, const int *counts,
      const int *lbounds, const int *ubounds, void *base_addr = NULL, 
      const char *name = NULL, CopyFlag docopy = DATA_REF, int *rc = NULL);
    static LocalArray *create(const LocalArray *larrayIn,
      const int *lbounds = NULL, const int *ubounds = NULL,
      const char *name = NULL, int *rc = NULL);
    static LocalArray *create(const LocalArray *larrayIn, CopyFlag copyflag,
      const int *lbounds, const int *ubounds, int *rc);
    static int destroy(LocalArray *array);

    // standard methods
    int print(const char *options = NULL)const;
    int write(const char *options, const char *filename)const;
    int validate(const char *options)const;

    // simple set/get methods
    void setRank(int rank){ this->rank = rank; }
    int getRank()const{ return rank; }
    void setTypeKind(ESMC_TypeKind typekind){ this->typekind = typekind; }
    ESMC_TypeKind getTypeKind()const{ return typekind; }
    void setBaseAddr(void *base_addr){ this->base_addr = base_addr; }
    void *getBaseAddr()const{ return base_addr; } 
    void setOrigin(LocalArrayOrigin origin){ this->origin = origin; } 
    LocalArrayOrigin getOrigin()const{ return origin; }
    void setByteCount(int byte_count){ this->byte_count = byte_count; }
    int getByteCount()const{ return byte_count; }
    void setDealloc(bool dealloc){ this->dealloc = dealloc; }
    bool getDealloc()const{ return dealloc; }
    const int *getCounts()const{ return counts; }
    const int *getLbounds()const{ return lbound; }
    const int *getUbounds()const{ return ubound; }
    
    // combo set method
    int setInfo(struct c_F90ptr *fptr, void *base, const int *counts, 
      const int *lbounds, const int *ubounds, const int *offsets,
      const bool *cflag, const bool *dflag);

    // set/get the Fortran dope vector
    int setFortranDopev(struct c_F90ptr *p);
    int getFortranDopev(struct c_F90ptr *p)const;
    
    // force the base address in Fortran ptr dope vector *if* mismatch detected
    int forceFortranPtr(void *base){
      void **f90dopev_cast = (void **)(&f90dopev);
      // The rationale for this call is as follows:
      // - some Fortran compilers will make temporary copies of the actual
      //   data array.
      // - such copies can be detected by comparing the address of the first
      //   array element against the address of the first element of the
      //   original data array, stored in the LocalArray object.
      // - if/when a temporary data copy has been detected this routine tries
      //   to force the base address in the stored f90 ptr dope vector to the
      //   correct base address stored in the LocalArray. The assumption is made
      //   that the base address is that of the first element and is stored 
      //   without offset at the start of the dope vector.
      // - since the details about the f90 ptr dope vector are compiler
      //   dependent the above assumption of where to force the base address
      //   (and with what offset in the address) may turn out to be incorrect.
      //   However, this routine is a last resort in those cases where a 
      //   temporary copy has been detected and it tests out fine on XLF and
      //   lahey.
      if (base != base_addr){
        // Only force base_addr into dope vector if the first element according
        // to dope vector is located somewhere else, i.e. a temporary copy.
        *f90dopev_cast = base_addr;
      }
      return ESMF_SUCCESS;
    }

    // Get Data from a position in the LocalArray
    template <class TYPE> int getData(int *index, TYPE *data);

    // Get Data from a position in the LocalArray w/o internal error checking
    template <class TYPE> void getDataInternal(int *index, TYPE *data);
    
    // portably copy Fortran dope vector
    static int tkrPtrCopy(void *dst, void *src, ESMC_TypeKind typekind,
      int rank);
  
  };  // class LocalArray

} // namespace ESMCI

#endif  // ESMCI_LocalArray_H
