// $Id: ESMC_PEList.C,v 1.7 2003/03/10 05:14:21 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC PEList method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ PEList methods declared
// in the companion file ESMC_PEList.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <iostream.h>
//#include <iostream> // TODO: use when namespaces consistently implemented
//using std::cout;
//using std::cerr;
 #include <stdlib.h>   // qsort
// #include <cstdlib>   // qsort
 #include <new>       // new, bad_alloc
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_PEList.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_PEList.C,v 1.7 2003/03/10 05:14:21 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the PEList routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListCreate - Create a new PEList
//
// !INTERFACE:
      ESMC_PEList *ESMC_PEListCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_PEList
//
// !ARGUMENTS:
      int firstpe,          // in - first PE in list
      int lastpe,           // in - last PE in list
      int *rc) {            // out - return code
//
// !DESCRIPTION:
//      Create a new PEList from ... Allocates memory for a new PEList
//      object and uses the internal routine ESMC\_PEListConstruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC\_PEListInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

  int numpes = lastpe - firstpe + 1;
  ESMC_PEList *pelist = ESMC_PEListCreate(numpes, rc);

  // initialize the CPU ids in the PE list to the given range
  //   (declared as friend function to directly access internal peList)
  if (pelist != 0 && pelist->peList != 0) {
    for(int i=0, peid = firstpe; i<numpes; i++, peid++) {
      pelist->peList[i].ESMC_PESetCpuID(peid);
    }
  }

  return(pelist);

 } // end ESMC_PEListCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListCreate - Create a new PEList
//
// !INTERFACE:
      ESMC_PEList *ESMC_PEListCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_PEList
//
// !ARGUMENTS:
      int numpes,          // in - number of PEs in list
      int *rc) {           // out - return code
//
// !DESCRIPTION:
//      Create a new PEList from ... Allocates memory for a new PEList
//      object and uses the internal routine ESMC\_PEListConstruct to
//      initialize it.  Define for deep classes only, for shallow classes only
//      define and use ESMC\_PEListInit.
//      There can be multiple overloaded methods with the same name, but
//      different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

  ESMC_PEList *pelist;

#if 0
// use this section if exception handling not supported
// TODO:  IBM (blackforest) doesn't support "new (nothrow)"
  if ((pelist = new (nothrow) ESMC_PEList) == 0) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_PEListCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(0);
  }

//cout << "ESMC_PEListCreate() succesful\n";
  *rc = pelist->ESMC_PEListConstruct(numpes);
  return(pelist);
#endif

// TODO: ?? use exception handling when universally supported (pgCC doesn't)
#if 1
  try {
    pelist = new ESMC_PEList;
//cout << "ESMC_PEListCreate() succesful\n";
    *rc = pelist->ESMC_PEListConstruct(numpes);
    return(pelist);
  }
//  catch (bad_alloc) {  // TODO: use when IBM supports it (blackforest doesn't)
  catch (...) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_PEListCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(0);
  }
#endif

 } // end ESMC_PEListCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListDestroy - free a PEList created with Create
//
// !INTERFACE:
      int ESMC_PEListDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_PEList *pelist) {    // PE list to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a PEList object previously allocated
//      via an ESMC\_PEListCreate routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  if (pelist != 0) {
    pelist->ESMC_PEListDestruct();
    delete pelist;
//cout << "ESMC_PEListDestroy() successful\n";
    return(ESMF_SUCCESS);
  } else {
    return(ESMF_FAILURE);
  }

 } // end ESMC_PEListDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListConstruct - fill in an already allocated PEList
//
// !INTERFACE:
      int ESMC_PEList::ESMC_PEListConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int numpes) {          // in - number of PEs in list
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated PEList object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC\_PEListDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC\_PEListCreate, which calls
//      ESMC\_PEListConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

#if 0
// use this section if exception handling not supported
// TODO:  IBM (blackforest) doesn't support "new (nothrow)"
  if (numpes > 0) {
    if ((peList = new (nothrow) ESMC_PE[numpes]) == 0) {
  // TODO:  call ESMF log/err handler
      cerr << "ESMC_PEListConstruct() memory allocation failed\n";
      return(ESMF_FAILURE);
    }

  //cout << "ESMC_PEListConstruct() successful\n";
    numPEs = numpes;
    return(ESMF_SUCCESS);
  }

  return(ESMF_FAILURE);
#endif

// TODO: ?? use exception handling when universally supported (pgCC doesn't)
#if 1
  if (numpes > 0) {
    try {
      peList = new ESMC_PE[numpes];
      numPEs = numpes;
  //cout << "ESMC_PEListConstruct() successful\n";
      return(ESMF_SUCCESS);
    }
//    catch(bad_alloc) { // TODO: use when IBM supports it (blackforest doesn't)
    catch(...) {
  // TODO:  call ESMF log/err handler
      cerr << "ESMC_PEListConstruct() memory allocation failed\n";
      return(ESMF_FAILURE);
    }
  } else return(ESMF_FAILURE);
#endif

 } // end ESMC_PEListConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListDestruct - release resources associated w/a PEList
//
// !INTERFACE:
      int ESMC_PEList::ESMC_PEListDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMC\_PEListConstruct, does any additional cleanup before the
//      original PEList object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC\_PEListDestroy, which calls
//      ESMC\_PEListDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//cout << "ESMC_PEListDestruct() invoked\n";
  delete[] peList;
  peList = 0;
  numPEs = 0;

  return(ESMF_SUCCESS);

 } // end ESMC_PEListDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListInit - initializes a PEList element
//
// !INTERFACE:
      int ESMC_PEList::ESMC_PEListInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int i,                // in - ith element
      int esmfid,           // in - ESMF ID
      int cpuid,            // in - machine cpu ID
      int nodeid) {         // in - machine node ID
//
// !DESCRIPTION:
//      ESMF routine which only initializes PE values; it does not
//      allocate any resources.
//
//EOP
// !REQUIREMENTS:  

  if (peList != 0 && i >= 0 && i < numPEs) {
    peList[i].ESMC_PEInit(esmfid, cpuid, nodeid);
    return(ESMF_SUCCESS);
  } else {
    return(ESMF_FAILURE);
  }

 } // end ESMC_PEListInit

#if 0
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListGetConfig - get configuration info from a PEList
//
// !INTERFACE:
      int ESMC_PEList::ESMC_PEListGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_PEListConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the PEList object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PEListGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListSetConfig - set configuration info for a PEList
//
// !INTERFACE:
      int ESMC_PEList::ESMC_PEListSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_PEListConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the PEList object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PEListSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListGet<Value> - get <Value> for a PEList
//
// !INTERFACE:
      int ESMC_PEList::ESMC_PEListGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of PEList member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PEListGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListSet<Value> - set <Value> for a PEList
//
// !INTERFACE:
      int ESMC_PEList::ESMC_PEListSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the PEList member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_PEListSet<Value>
#endif

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListGetPE - get PE pointer from a PEList
//
// !INTERFACE:
      int ESMC_PEList::ESMC_PEListGetPE(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int i,                    // in  - ith PE
      ESMC_PE **pe) const {     // out - pointer to ith PE
//
// !DESCRIPTION:
//     Returns a pointer to the ith PE in the list.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  if (i<0 || i>=numPEs) {
    *pe = 0;
    return(ESMF_FAILURE);
  } else {
    *pe = &peList[i];
    return (ESMF_SUCCESS);
  }

 } // end ESMC_PEListGetPE


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListValidate - internal consistency check for a PEList
//
// !INTERFACE:
      int ESMC_PEList::ESMC_PEListValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a PEList is internally consistent.
//      Returns error code if problems are found.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//
  return(ESMF_SUCCESS);

 } // end ESMC_PEListValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListPrint - print contents of a PEList
//
// !INTERFACE:
      int ESMC_PEList::ESMC_PEListPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a PEList.  The options control the
//      type of information and level of detail.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  //cout << "number of PEs in list = " << numPEs << "\n";

  // walk list and print each element via PE class Print method
  for(int i=0; i<numPEs; i++) {
    peList[i].ESMC_PEPrint();
  }

  return(ESMF_SUCCESS);

 } // end ESMC_PEListPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEList - native C++ constructor
//
// !INTERFACE:
      ESMC_PEList::ESMC_PEList(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {  // in
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//cout << "ESMC_PEList() invoked\n";

  // initialize to an empty list
  peList = 0;
  numPEs = 0;

 } // end ESMC_PEList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_PEList - native C++ destructor
//
// !INTERFACE:
      ESMC_PEList::~ESMC_PEList(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//cout << "~ESMC_PEList() invoked\n";
  ESMC_PEListDestruct();

 } // end ~ESMC_PEList

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListSort - sort contents of a PEList by node affinity
//
// !INTERFACE:
      int ESMC_PEList::ESMC_PEListSort(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {     //  in - none
//
// !DESCRIPTION:
//      Sorts a PEList by node affinity; all PE's within same node
//      will appear contiguously in the PEList  
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  if (peList != 0 && numPEs > 0) {
    qsort(peList, numPEs, sizeof(ESMC_PE), ESMC_PEListPECompare);
    return(ESMF_SUCCESS);
  }

  return(ESMF_FAILURE);

 } // end ESMC_PEListSort

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_PEListPECompare - compares 2 PE's by node affinity
//
// !INTERFACE:
      int ESMC_PEListPECompare(
//
// !RETURN VALUE:
//    int comparison result as required by qsort()
//
// !ARGUMENTS:
      const void *pe1,       //  in - PE 1
      const void *pe2) {     //  in - PE 2
//
// !DESCRIPTION:
//      Compares 2 PEs by node affinity; used by qsort in ESMC\_PEListSort()
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  int nodeID1, nodeID2;

  // TODO: ?? make this routine friend of ESMC_PE

  ((ESMC_PE*)pe1)->ESMC_PEGetNodeID(&nodeID1);
  ((ESMC_PE*)pe2)->ESMC_PEGetNodeID(&nodeID2);

  if (nodeID1 < nodeID2) return(-1);
  else if (nodeID1 == nodeID2) return(0);
  else return(1);

 } // end ESMC_PEListSort
