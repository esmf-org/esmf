// $Id: ESMC_DELayout.h,v 1.6 2004/04/02 18:36:34 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF DELayout C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_DELayout_H
 #define ESMC_DELayout_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
//#include <ESMC_CommMem.h> 

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_DELayout - Topology of DE's mapped onto a PEList
//
// !DESCRIPTION:
//
// The code in this file defines the C++ DELayout members and declares method 
// signatures (prototypes).  The companion file ESMC\_DELayout.C contains
// the definitions (full code bodies) for the DELayout methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_LocalArray.h>
 #include <ESMC_PE.h>  
 #include <ESMC_PEList.h>  
 #include <ESMC_DE.h>  
 #include <ESMC_Comm.h>  
 #include <mpi.h>

// !PUBLIC TYPES:
// class ESMC_DELayoutConfig;
 class ESMC_DELayout;

// !PRIVATE TYPES:

 // class configuration type
 //class ESMC_DELayoutConfig {
 //  private:
 //   < insert resource items here >
 //};

  class ESMC_DEComm {
    public: 
      MPI_Comm mpicomm;      // MPI communicator for this set of DEs
  };

// hint type about the most performance critical communication direction
enum ESMC_CommHint {ESMC_NOHINT, ESMC_XFAST, ESMC_YFAST, ESMC_ZFAST};

// parameters for asynchronous/sync communication
enum ESMC_Complete {ESMC_TEST_COMPLETE=1, ESMC_WAIT_COMPLETE};

class ESMC_CommHandle {
  int mpi_handle;
  enum ESMC_Complete cflag;
};

typedef int ESMC_CommType;  
#define ESMC_COMMTYPE_MP 0
#define ESMC_COMMTYPE_SHR 2

 // class definitions
 class ESMC_DELayout : public ESMC_Base {    // inherits from ESMC_Base class

  private:
                                 
    ESMC_DE ***layout;        // Topology of DE's -- aligns
                              //   with a Distributed Grid
    int ndim;                 // number of dimensions in DELayout
    int nDEs;                 // total number of DE's in DELayout
    int *length;              // array of length ndim with number of DEs in each dim
    ESMC_CommType *commType;  // array of length ndim with commType for each dim    
    ESMC_CommHint commHint;   // communication hint (obsolete)
    ESMC_PEList *peList;      // PE list on which the DELayout maps 

    ESMC_DE myDE;             // list of my local DEs
    ESMC_PE myPE;             // the PE on which this DELayout resides
    ESMC_Comm comm;           // communication object
    ESMC_DEComm decomm;       // communicator for set of DEs in this layout 
    ESMC_DELayout *parent;    // pointer to my parent DELayout
    ESMC_Machine Mach;        // machine model

// !PUBLIC MEMBER FUNCTIONS:
//
//  pick one or the other of the init/create sections depending on
//  whether this is a deep class (the class/derived type has pointers to
//  other memory which must be allocated/deallocated) or a shallow class
//  (the class/derived type is self-contained) and needs no destroy methods
//  other than deleting the memory for the object/derived type itself.

  public:
    int ESMC_DELayoutConstruct(void);
    int ESMC_DELayoutConstruct(ESMC_DELayout *parent, int *parent_offsets,
			       int *de_indices, int ndim, int *lengths,
                               ESMC_CommType *commtypes); 
    int ESMC_DELayoutConstruct(int *delist, int ndim, int *lengths,
                             ESMC_CommType *commtypes); 
    int ESMC_DELayoutConstruct(ESMC_PEList *pelist, int ndim, int *lengths,
                             ESMC_CommType *commtypes); 
                                             // internal only, deep class
    int ESMC_DELayoutDestruct(void);           // internal only, deep class
    int ESMC_DELayoutInit(void);

 // accessor methods for class members
//    int ESMC_DELayoutGet<Value>(<value type> *value) const;
//    int ESMC_DELayoutSet<Value>(<value type>  value);
    int ESMC_DELayoutSetPEList(void);
    int ESMC_DELayoutGetNumDEs(int *ndes) const;
    int ESMC_DELayoutGetSize(int *nx, int *ny) const;
    int ESMC_DELayoutGetSize(int *nx, int *ny, int *nz) const;
    int ESMC_DELayoutGetDEPosition(ESMC_DE *de, int *x, int *y, int *z) const;
    int ESMC_DELayoutGetDEPosition(int *x, int *y) const;
    int ESMC_DELayoutGetDEID(int *deid) const;
    int ESMC_DELayoutGetDEIDat(int x, int y, int *deid) const;
    int ESMC_DELayoutGetDEIDat(int x, int y, int z, int *deid) const;
    int ESMC_DELayoutGetParentDEID(int childdeid, ESMC_DELayout *parent, 
                                                        int *parentdeid) const;
    int ESMC_DELayoutGetChildDEID(int parentdeid, ESMC_DELayout *child, 
                                                        int *childdeid) const;
    int ESMC_DELayoutGetDEExists(int deid, ESMC_DELayout *other, ESMC_Logical *exists) const;
    int ESMC_DELayoutGetXXX(void) const;
    int ESMC_DELayoutGetDE(int x, int y, int z, ESMC_DE *de) const;
    int ESMC_DELayoutGetDE(int deid, ESMC_DE **de) const;

    int ESMC_DELayoutGetDEExclusive(ESMC_DE *de) const;    
    int ESMC_DELayoutSetAxisIndex(int global_counts[], int size_gcount,
                                int decompids[], int size_decomp,
                                ESMC_AxisIndex *AIPtr);
    int ESMC_DELayoutParse(int axis, int count, int *countsPerDE);
#if 0
    // OLD
    int ESMC_DELayoutGatherArrayI(int *DistArray, int global_dimlengths[],
                                  int decompids[], int size_decomp,
                                  int localDimCounts[], int localMaxDimCount[],
                                  ESMC_AxisIndex *AIPtr, ESMC_AxisIndex *AIPtr2, 
                                  int *GlobalArray);
    int ESMC_DELayoutGatherArrayF(float *DistArray, int global_dimlengths[],
                                  int decompids[], int size_decomp,
                                  int localDimCounts[], int localMaxDimCount[],
                                  ESMC_AxisIndex *AIPtr, ESMC_AxisIndex *AIPtr2, 
                                  float *GlobalArray);
    int ESMC_DELayoutGatherArrayD(double *DistArray, int global_dimlengths[],
                                  int decompids[], int size_decomp,
                                  int localDimCounts[], int localMaxDimCount[],
                                  ESMC_AxisIndex *AIPtr, ESMC_AxisIndex *AIPtr2, 
                                  double *GlobalArray);
#endif
    int ESMC_DELayoutGatherArray(void *DistArray, int global_dimlengths[],
                                  int decompids[], int size_decomp,
                                  int localDimCounts[], int localMaxDimCount[],
                                  ESMC_AxisIndex *AIPtr, ESMC_AxisIndex *AIPtr2, 
                                  ESMC_DataKind datatype, void *GlobalArray);
    
 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_DELayoutValidate(void) const;
    int ESMC_DELayoutPrint(void) const;

 // native C++ constructors/destructors
	ESMC_DELayout(void);
	~ESMC_DELayout(void);
  
 // < declare the rest of the public interface methods here >
  // TODO: make comm public and invoke directly on comm rather than at
  //       DELayout level ? (does not depend on any DELayout knowledge)
  //       Or remove comm from DELayout (standalone) and remove these
  //       entry points from DELayout?

    int ESMC_DELayoutScatter(void *sndArray, void *rcvArray, int len, 
                             ESMC_DataKind type, int srcDEid); 
    int ESMC_DELayoutScatter(ESMC_LocalArray *sndArray,
                             ESMC_LocalArray *rcvArray, int len, int srcDEid); 
    int ESMC_DELayoutAllGatherV(void *sndArray, int sndLen,
                                void *rcvArray, int *rcvLen, int *rcvDispls,
                                ESMC_DataKind kind);
    int ESMC_DELayoutAllGatherV(ESMC_LocalArray *sndArray, int sndLen,
                                ESMC_LocalArray *rcvArray, int *rcvLen,
                                int *rcvDispls);
    int ESMC_DELayoutAllReduce(int *dataArray, int *result, int arrayLen,
			       ESMC_Op op);
    int ESMC_DELayoutSendRecv(void *sbuf, void *rbuf, int snum, int rnum, 
			      int sde_index, int rde_index, ESMC_DataKind type);
    int ESMC_DELayoutBcast(void *buf, int num, int srcde_index, 
			   ESMC_DataKind type);
    int ESMC_DELayoutBarrier(void);
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 
//

    int ESMC_DELayoutGetSameDEID(int srcid, ESMC_DELayout *other, int *otherid) 
                                                                         const;

 // < declare private interface methods here >
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_DELayout

    // create default 1D layout
    ESMC_DELayout *ESMC_DELayoutCreate(int *rc); 

    // create DELayout from PE List
    ESMC_DELayout *ESMC_DELayoutCreate(ESMC_PEList *pelist, int ndim, int *lengths, 
				       ESMC_CommType *commtypes, int *rc);

    // create DELayout from DE List
    ESMC_DELayout *ESMC_DELayoutCreate(int *delist, int ndim, int *lengths, 
				       ESMC_CommType *commtypes, int *rc);

    // create sub-layout from parent
    ESMC_DELayout *ESMC_DELayoutCreate(ESMC_DELayout *parent, int *parent_offsets,
				       int *de_indices, int ndim, int *lengths, 
				       ESMC_CommType *commtypes, int *rc);
 
    int ESMC_DELayoutDestroy(ESMC_DELayout *layout);

 #endif  // ESMC_DELayout_H
