// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2014, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC IO_NetCDF method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_IO\_NetCDF} methods declared
// in the companion file {\tt ESMCI\_IO\_NetCDF.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_IO_NetCDF.C"

// associated class definition file
#include "ESMCI_IO_NetCDF.h"

// higher level, 3rd party or system includes here
#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <string>
#include <sstream>

#include "ESMC_Util.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_ArraySpec.h"
#include "ESMCI_LocalArray.h"
#include "ESMCI_Array.h"

using namespace std; 

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

namespace ESMCI
{

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the IO_NetCDF routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_IO_NetCDFCreate - Allocates and Initializes an IO_NetCDF object
//
// !INTERFACE:
      IO_NetCDF *ESMCI_IO_NetCDFCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated IO_NetCDF
//
// !ARGUMENTS:
      const std::string& name,             // in
      ESMC_Base         *base,             // in
      int               *rc) {             // out - return code

// !DESCRIPTION:
//      Allocates and Initializes a {\tt ESMC\_IO\_NetCDF} with given values
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI_IO_NetCDFCreate(new)"

    int returnCode;
    IO_NetCDF *io_netcdf;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // allocate an io object & set defaults via constructor
    try 
    {
      io_netcdf = new IO_NetCDF;
    }
    catch (...) 
    {
      ESMC_LogDefault.AllocError(ESMC_CONTEXT,rc);
      return(ESMC_NULL_POINTER);
    }

    if (!name.empty()) {
      // use given name
      returnCode = io_netcdf->ESMC_BaseSetF90Name(name.c_str(), name.length());
    } else {
      // create default name "IO_NetCDF<ID>"
      returnCode = io_netcdf->ESMC_BaseSetName((const char*) ESMC_NULL_POINTER,
                                               "IO_NetCDF");
    }
    ESMC_LogDefault.MsgFoundError(returnCode, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);

    if (base != ESMC_NULL_POINTER) io_netcdf->base = base;

    // TODO returnCode = io_netcdf->validate();
    returnCode = ESMF_SUCCESS;
    ESMC_LogDefault.MsgFoundError(returnCode, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    return(io_netcdf);

 } // end ESMCI_IO_NetCDFCreate (new)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_IO_NetCDFDestroy - free an IO_NetCDF created with Create
//
// !INTERFACE:
      int ESMCI_IO_NetCDFDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      IO_NetCDF **io_netcdf) {  // in - IO_NetCDF to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys an IO_NetCDF object previously allocated
//      via an {\tt ESMCI\_IO\_NetCDFCreate} routine.  Define for deep classes only.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (io_netcdf == ESMC_NULL_POINTER || *io_netcdf == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to io_netcdf", ESMC_CONTEXT, &rc);
    return rc;
  }

  try{
    // destruct Array object
    (*io_netcdf)->destruct();
    // mark as invalid object
    (*io_netcdf)->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
 } // end ESMCI_IO_NetCDFDestroy

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_NetCDF::read - Performs a read on an IO_NetCDF object
//
// !INTERFACE:
      int IO_NetCDF::read(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:
      const std::string& fileName) {          // in

// !DESCRIPTION:
//      Reads an {\tt ESMC\_IO\_NetCDF} object from file
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::IO_NetCDF::read()"

    int rc = ESMF_SUCCESS;
    ESMCI::VM *globalVM;
    int mypet, numPETs;

    if (this == ESMC_NULL_POINTER) 
    {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // only read on pet 0
    globalVM = ESMCI::VM::getGlobal(&rc);
    if ((globalVM == ESMC_NULL_POINTER) || (rc != ESMF_SUCCESS)) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "FAIL rc=%d, Unable to get GlobalVM\n", rc);
      ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);
      return(ESMF_FAILURE);
    }
    mypet = globalVM->getLocalPet(); 
    numPETs = globalVM->getPetCount();
//printf("mypet = %d, numPETS = %d\n", mypet, numPETs);
//fflush(stdout);
#ifdef ESMF_NETCDF
    if (mypet != 0) return rc;
#else
    if (mypet != 0) return ESMF_RC_LIB_NOT_PRESENT;
#endif

    if (!fileName.empty()) 
    {
      // TODO: only use local of fileName this one time;
      //   don't change set IO_NetCDF member fileName
      this->fileName = fileName;
    } 
    else 
    {
      // TODO use existing IO_NetCDF fileName member
    }

#ifdef ESMF_NETCDF
    NcFile netCdfFile;
    int ncerr;
    ncerr = nc_open (this->fileName.c_str(), NC_NOWRITE, &netCdfFile);
    if (ncerr != NC_NOERR) {
      std::stringstream errstr;
      errstr << ESMC_METHOD << ": Attempting to open existing NcFile: " << this->fileName;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_OPEN,
         errstr.str().c_str(), ESMC_CONTEXT, &rc);
      return ESMF_FAILURE;
    }

    //***
    // Create the State object that we'll be filling with Arrays
    //***
#if 0
    int stateRc = 0;
    theState = State::create(this->fileName.c_str(), &stateRc);
    //printf("*** State RC: %d\n", stateRc);
    if (stateRc != ESMF_SUCCESS)
    {
      return stateRc; 
    }
#endif

    //***
    // Check that the State object that we'll be filling with Arrays is set
    //***
    if (theState == ESMC_NULL_POINTER) 
    {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'theState' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(ESMF_FAILURE);
    }

    /*
    ** Dimensions
    */
    int	numDims;
    if ((ncerr = nc_inq_ndims (netCdfFile, &numDims)) != NC_NOERR) {
      std::cerr << ESMC_METHOD << ": nc_inq_ndims failure" << std::endl;
      return(ESMF_FAILURE);
    }
    //printf("\nNum Dimensions: %d\n", numDims);

    for (int i = 0; i < numDims; ++i)
    {
      char dimname[NC_MAX_NAME+1];
      if ((ncerr = nc_inq_dimname (netCdfFile, i, dimname)) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_inq_dimname failure" << std::endl;
        return(ESMF_FAILURE);
      }

      if (strlen (dimname) > 0)
      {
        //printf("Dimension Name: %s\n", thisDim->name());
        //printf("          Size: %d\n", thisDim->size());
      }
      else
      {
        // TODO:  return ESMF error?
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'thisDim' pointer is not valid.", ESMC_CONTEXT, &rc);
      return(ESMF_FAILURE);
      }
    }

    /*
    ** Variables
    */
    int	numVars;
    if ((ncerr = nc_inq_nvars (netCdfFile, &numVars)) != NC_NOERR) {
      std::cerr << ESMC_METHOD << ": nc_inq_nvars failure" << std::endl;
      return(ESMF_FAILURE);
    }
    //printf("\nNum Variables: %d\n", numVars);

    for (int i = 0; i < numVars; ++i)
    {
      Array*  thisArray = readArray(netCdfFile, i);
      if (thisArray != NULL)
      {
        //thisArray->print();
        //printf("Array Name: %s\n", thisArray->getName());
        theState->addArray(thisArray);
      }
      else
      {
        // TODO:  return ESMF error?
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'thisArray' pointer is not valid.", ESMC_CONTEXT, &rc);
      return(ESMF_FAILURE);
      }
    }

    /*
    ** Attributes
    */
    int	numAtts;
    if ((ncerr = nc_inq_natts (netCdfFile, &numAtts)) != NC_NOERR) {
      std::cerr << ESMC_METHOD << ": nc_inq_natts failure" << std::endl;
      return(ESMF_FAILURE);
    }
    //printf("\nNum Attributes: %d\n", numAtts);

    for (int i = 0; i < numAtts; ++i)
    {
      char attname[NC_MAX_NAME+1];
      nc_type atttype;
      size_t attsize;
      if ((ncerr = nc_inq_att (netCdfFile, i, attname, &atttype, &attsize)) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_inq_att failure" << std::endl;
        return(ESMF_FAILURE);
      }

      if (strlen (attname) > 0)
      {
        //printf("Attribute Name: %s\n", thisAtt->name());
        //printf("          Type: %d\n", thisAtt->type());
        //printf("          Vals: %d\n", thisAtt->num_vals());
      }
      else
      {
        // TODO:  return ESMF error?
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'thisAtt' pointer is not valid.", ESMC_CONTEXT, &rc);
      return(ESMF_FAILURE);
      }
    }
#else
    // netcdf library not present
    rc = ESMF_RC_LIB_NOT_PRESENT;
#endif // ESMF_NETCDF

    return (rc);

}  // end IO_NetCDF::read

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_NetCDF::write - Performs a write on an IO_NetCDF object
//
// !INTERFACE:
      int IO_NetCDF::write(
//
// !RETURN VALUE:
//     int error return code
//
// !ARGUMENTS:
      const std::string& fileName) {          // in

// !DESCRIPTION:
//      Writes an {\tt ESMC\_IO_NetCDF} object to file
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::IO_NetCDF::write()"

    int rc = ESMF_SUCCESS;
    ESMCI::VM *globalVM;
    int mypet, numPETs;

    if (this == ESMC_NULL_POINTER) 
    {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(ESMF_FAILURE);
    }

    // only write on pet 0
    globalVM = ESMCI::VM::getGlobal(&rc);
    if ((globalVM == ESMC_NULL_POINTER) || (rc != ESMF_SUCCESS)) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "FAIL rc=%d, Unable to get GlobalVM\n", rc);
      ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);
      return(ESMF_FAILURE);
    }
    mypet = globalVM->getLocalPet(); 
    numPETs = globalVM->getPetCount();
//printf("mypet = %d, numPETS = %d\n", mypet, numPETs);
//fflush(stdout);
#ifdef ESMF_NETCDF
    if (mypet != 0) return rc;
#else
    if (mypet != 0) return ESMF_RC_LIB_NOT_PRESENT;
#endif

#ifdef ESMF_NETCDF
    // check only when netCDF present
    if (theState == ESMC_NULL_POINTER) 
    {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'theState' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(ESMF_FAILURE);
    }
#endif

    if (!fileName.empty()) 
    {
      // TODO: only use local of fileName this one time;
      //   don't change set IO_NetCDF member fileName
      this->fileName = fileName;
    } 
    else 
    {
      // TODO use existing IO_NetCDF fileName member
    }

#ifdef ESMF_NETCDF
    NcFile netCdfFile;
    int ncerr;
    ncerr = nc_create (this->fileName.c_str(), NC_CLOBBER, &netCdfFile);
    if (ncerr != NC_NOERR) {
      std::stringstream errstr;
      errstr << ESMC_METHOD << ": Attempting to create/overwrite NcFile: " << this->fileName;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_OPEN,
         errstr.str().c_str(), ESMC_CONTEXT, &rc);
      return ESMF_FAILURE;
    }

    int old_fill_mode;
    if ((ncerr = nc_set_fill (netCdfFile, NC_FILL, &old_fill_mode)) != NC_NOERR) {
      std::cerr << ESMC_METHOD << ": nc_set_fill failure: " << nc_strerror(ncerr) << std::endl;
      return(ESMF_FAILURE);
    }

        int  numArrays = 0;
        theState->getNumArrays(&numArrays);

        std::vector<string>  arrayNames = theState->getArrayNames();

        for (int i = 0; i < arrayNames.size(); ++i)
        {
           // std::cout << "Item[" << i << "]: " << arrayNames[i] << endl;
           Array*    thisArray;
           theState->getArray((char*)(arrayNames[i].c_str()), &thisArray);

           LocalArray*  localArray = (thisArray->getLocalarrayList())[0];
           int               numDims = localArray->getRank();
           const int*        dimLengths;
           int*              dimensions = new int[numDims];

           dimLengths = localArray->getCounts();

           ncerr = nc_redef (netCdfFile);  // ensure we are in define mode

           for (int j = 0; j < numDims; ++j)
           {
             std::stringstream dimName;
             dimName << "dim_" << i << "_" << j;

             //printf("Dim[%d] length: %d\n", j, dimLengths[j]);

             size_t dimLengths_sizet = dimLengths[j];
             if ((ncerr = nc_def_dim (netCdfFile, dimName.str().c_str(), dimLengths_sizet, &dimensions[j])) != NC_NOERR) {
               std::cerr << ESMC_METHOD << ": nc_def_dim failure: " << nc_strerror(ncerr) << std::endl;
               delete[] dimensions;
               delete[] dimLengths;
               return(ESMF_FAILURE);
             }
//             dimensions[j] = netCdfFile->add_dim(dimName, dimLengths[j]);
           }

           writeArray(netCdfFile, thisArray, numDims, dimensions);

           delete[] dimensions;
        }
#else
    // netcdf library not present
    rc = ESMF_RC_LIB_NOT_PRESENT;
#endif // ESMF_NETCDF

    return (rc);

}  // end IO_NetCDF::write

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_NetCDF - native C++ constructor
//
// !INTERFACE:
      IO_NetCDF::IO_NetCDF(void)
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes for either C++ or F90, since {\tt ESMC\_IO\_NetCDF} is a deep,
//      dynamically allocated class.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::IO_NetCDF() native constructor"

 : ESMC_Base(-1) {  // invoke ESMC_Base constructor with id=(-1); prevents
                    // Base id increment for non-distributed,
                    // non-reconcilable objects such as IO.
    theState = ESMC_NULL_POINTER; 
    // create default name "IO_NetCDF<ID>"
    ESMC_BaseSetName(ESMC_NULL_POINTER, "IO_NetCDF");
    // copy = false;  // TODO: see notes in constructors and destructor below

 } // end IO_NetCDF

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  IO_NetCDF - destruct()
//
// !INTERFACE:
void IO_NetCDF::destruct(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Destruct an IO_NetCDF object
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 } // end destruct()


//-------------------------------------------------------------------------


#ifdef ESMF_NETCDF
  ESMC_TypeKind_Flag  IO_NetCDF::ncToEsmcType(nc_type ncTypeVal) 
  {

#undef  ESMC_METHOD
#define ESMC_METHOD "IO_NetCDF::ncToEsmcType"

    ESMC_TypeKind_Flag  esmcTypeVal = ESMF_NOKIND;

    switch (ncTypeVal)
    {
    case NC_BYTE:
      esmcTypeVal = ESMC_TYPEKIND_I1;
      break;
    case NC_CHAR:
      esmcTypeVal = ESMC_TYPEKIND_CHARACTER;
      break;
    case NC_SHORT:
      esmcTypeVal = ESMC_TYPEKIND_I2;
      break;
    case NC_INT:
      esmcTypeVal = ESMC_TYPEKIND_I4;
      break;
    //case NC_LONG:  // TODO?: deprecated in netCDF - same as NC_INT
    // esmcTypeVal = ESMC_TYPEKIND_I8;
    //break;
    case NC_FLOAT:
      esmcTypeVal = ESMC_TYPEKIND_R4;
      break;
    case NC_DOUBLE:
      esmcTypeVal = ESMC_TYPEKIND_R8;
      break;
    default:
      break;
    }

    return esmcTypeVal;
  }


//-------------------------------------------------------------------------

  nc_type  IO_NetCDF::esmcToNcType(ESMC_TypeKind_Flag  esmcTypeVal) 
  {

#undef  ESMC_METHOD
#define ESMC_METHOD "IO_NetCDF::esmcToNcType"

    nc_type  ncTypeVal = NC_UNSPECIFIED;

    switch (esmcTypeVal)
    {
    case ESMC_TYPEKIND_I1:
      ncTypeVal = NC_BYTE;
      break;
    case ESMC_TYPEKIND_I2:
      ncTypeVal = NC_SHORT;
      break;
    case ESMC_TYPEKIND_I4:
      ncTypeVal = NC_INT;
      break;
    case ESMC_TYPEKIND_I8:
      ncTypeVal = NC_LONG;  // TODO?: deprecated in netCDF - same ncInt
      break;
    case ESMC_TYPEKIND_R4:
      ncTypeVal = NC_FLOAT;
      break;
    case ESMC_TYPEKIND_R8:
      ncTypeVal = NC_DOUBLE;
      break;
    case ESMF_C8:
      ncTypeVal = NC_UNSPECIFIED;
      // TODO:  ncTypeVal = netCDF 8 byte complex type?
      break;
    case ESMF_C16:
      ncTypeVal = NC_UNSPECIFIED;
      // TODO:  ncTypeVal = netCDF 16 byte complex type?
      break;
    case ESMC_TYPEKIND_LOGICAL:
      ncTypeVal = NC_BYTE;
      break;
    case ESMC_TYPEKIND_CHARACTER:
      ncTypeVal = NC_CHAR;
      break;
    default:
      break;
    }

    return ncTypeVal;
  }



//-------------------------------------------------------------------------

  Array*  IO_NetCDF::readArray(NcFile  netCdfFile,
                               int      varIndex) 
  {

#undef  ESMC_METHOD
#define ESMC_METHOD "IO_NetCDF::readArray"

    int ndims;
    char ncname[NC_MAX_NAME];
    int nctype;
    int dimIds[NC_MAX_VAR_DIMS];
    int ncnatts;
    int ncerr;

    bool trace = false;

    if ((ncerr = nc_inq_var (netCdfFile, varIndex, ncname, &nctype, &ndims, dimIds, &ncnatts)) != NC_NOERR) {
      std::cerr << ESMC_METHOD << ": nc_inq_var failure: " << nc_strerror(ncerr) << std::endl;
      return NULL;
    }
//    NcVar*  thisVar = netCdfFile->get_var(varIndex);
    Array*  thisArray = NULL;

    if (ncerr != NC_NOERR)
    {
       return thisArray;
    }

    if (trace) {
      std::cerr << ESMC_METHOD << ": Variable Name: " << ncname << std::endl;
    //printf("         Type: %d\n", thisVar->type());
      std::cerr << ESMC_METHOD << ":         nDims: " << ndims << std::endl;
      std::cerr << ESMC_METHOD << ":    nAtts: " << ncnatts << std::endl;
    }

    size_t* dimSizes   = new size_t[ndims];
    int*    dimSizes_int = new int[ndims];
    size_t* minIndices = new size_t[ndims]();
    int*    minIndices_int = new int[ndims]();
    size_t* maxIndices = new size_t[ndims];
    int*    maxIndices_int = new int[ndims];
    size_t numValues = 1;
    for (int j=0; j<ndims; j++) {
      if ((ncerr = nc_inq_dimlen (netCdfFile, dimIds[j], &dimSizes[j])) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_inq_ndims failure: " << nc_strerror(ncerr) << std::endl;
        return thisArray;
      }
      if (trace)
        std::cerr << ESMC_METHOD << ": dimSizes[" << j << "] = " << dimSizes[j] << std::endl;
      dimSizes_int[j] = dimSizes[j];  // possible, but unlikely, narrowing here
      maxIndices[j] = dimSizes[j] - 1;
      maxIndices_int[j] = dimSizes_int[j]-1;
      numValues *= dimSizes[j];
    }

    ESMC_TypeKind_Flag	arrayType = ncToEsmcType(nctype);

    if (trace)
      std::cerr << ESMC_METHOD << ": allocating values buffer, numValues = " << numValues << std::endl;
    void * values = new double[numValues];
    if ((ncerr = nc_get_vara (netCdfFile, varIndex, minIndices, dimSizes, values)) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_get_vara failure: " << nc_strerror(ncerr) << std::endl;
        return thisArray;
    }

//    NcValues*	values = thisVar->values();

    int      arrayRc = 0;
    if (trace)
      std::cerr << ESMC_METHOD << ": creating locArray" << std::endl;
    LocalArray*	locArray = LocalArray::create(
                                   arrayType,
                                   ndims,
                                   dimSizes_int,
                                   minIndices_int,
                                   maxIndices_int,
                                   values,
                                   DATA_COPY,
                                   &arrayRc);
    //printf("*** LocalArray RC: %d\n", arrayRc);
    //locArray->print("full");

    InterfaceInt*	minIndex = new InterfaceInt(minIndices_int, ndims);
    InterfaceInt*	maxIndex = new InterfaceInt(maxIndices_int, ndims);

    delete[] (double*) values;
    delete[] maxIndices;
    delete[] minIndices;
    delete[] dimSizes_int;
    delete[] dimSizes;

    if (arrayRc != ESMF_SUCCESS)
    {
      delete[] minIndices_int;
      delete[] maxIndices_int;
      delete minIndex;
      delete maxIndex;
      return thisArray;
    }

    if (trace)
      std::cerr << ESMC_METHOD << ": creating distGrid" << std::endl;
    DistGrid*	distGrid = DistGrid::create(
                               minIndex, maxIndex,
                               (InterfaceInt*)NULL, (Decomp_Flag*)NULL, 0,
                               (InterfaceInt*)NULL, (InterfaceInt*)NULL,
                               (InterfaceInt*)NULL, (ESMC_IndexFlag*)NULL,
                               (InterfaceInt*)NULL, 
                               (DELayout*)NULL, (VM*)NULL, &arrayRc);
    //printf("*** DistGrid RC: %d\n", arrayRc);

    delete[] minIndices_int;
    delete[] maxIndices_int;
    delete minIndex;
    delete maxIndex;

    if (arrayRc != ESMF_SUCCESS)
    {
      std::cerr << ESMC_METHOD << ": distGrid create error. rc = " << arrayRc << std::endl;
      return thisArray;
    }

    if (trace)
      std::cerr << ESMC_METHOD << ": creating thisArray" << std::endl;
    thisArray = Array::create(&locArray, 1,
                              distGrid, DATA_COPY,
                              (InterfaceInt*)NULL, (InterfaceInt*)NULL, 
                              (InterfaceInt*)NULL, (InterfaceInt*)NULL, 
                              (InterfaceInt*)NULL, (InterfaceInt*)NULL,
                              (InterfaceInt*)NULL, (ESMC_IndexFlag*)NULL,
                              (InterfaceInt*)NULL, (InterfaceInt*)NULL,
                              &arrayRc);

    //printf("*** Array RC: %d\n", arrayRc);
    if (arrayRc != ESMF_SUCCESS)
    {
      return NULL;
    }

    thisArray->setName(ncname);

    //thisArray->print();

    for (int j = 0; j < ncnatts; ++j)
    {
      char attname[NC_MAX_NAME];
      if ((ncerr = nc_inq_attname (netCdfFile, varIndex, j, attname)) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_inq_attname failure: " << nc_strerror(ncerr) << std::endl;
        return thisArray;
      }
      if (trace)
        std::cerr << ESMC_METHOD << ": attribute[" << j << "] name: " << attname << std::endl;
      nc_type atttype;
      size_t attlen;
      if ((ncerr = nc_inq_att (netCdfFile, varIndex, attname, &atttype, &attlen)) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_inq_att failure: " << nc_strerror(ncerr) << std::endl;
        return thisArray;
      }
//      NcAtt*	thisAtt = thisVar->get_att(j);
      //printf("      nc att name[%d]: %s\n", j, thisAtt->name());
      //printf("      nc att type[%d]: %d ", j, thisAtt->type());

      void* valueBase;
      string attValString;
      vector<string> attValStringVector;
      attValStringVector.reserve(1);
      int attValInt;
      vector<int> attValIntVector;
      attValIntVector.reserve(1);
      float attValFloat;
      vector<float> attValFloatVector;
      attValFloatVector.reserve(1);
      double attValDouble;
      vector<double> attValDoubleVector;
      attValDoubleVector.reserve(1);

      switch (atttype)
      {
      case NC_CHAR:
         char *att_str;
         att_str = new char[attlen];
         if ((ncerr = nc_get_att_text (netCdfFile, varIndex, attname, att_str)) != NC_NOERR) {
           std::cerr << ESMC_METHOD << ": nc_get_att_text failure: " << nc_strerror(ncerr) << std::endl;
           return thisArray;
         }
         attValString = string(att_str, attlen);
         delete[] att_str;
         attValStringVector.push_back(attValString);
         valueBase = (void*) (&attValStringVector);
         //printf("(ncChar)\n");
         //printf("     nc att value[%d]: %s\n", j, attValString.c_str());
         
        break;

      case NC_INT:
         int *att_int;
         att_int = new int[attlen];
         if ((ncerr = nc_get_att_int (netCdfFile, varIndex, attname, att_int)) != NC_NOERR) {
           std::cerr << ESMC_METHOD << ": nc_get_att_int failure: " << nc_strerror(ncerr) << std::endl;
           return thisArray;
         }
         attValInt = att_int[0];
         delete[] att_int;
         attValIntVector.push_back(attValInt);
         valueBase = (void*) (&attValIntVector);
         //printf("(ncInt)\n");
         //printf("     nc att value[%d]: %d\n", j, attValInt);
        break;

      case NC_FLOAT:
         float *att_float;
         att_float = new float[attlen];
         if ((ncerr = nc_get_att_float (netCdfFile, varIndex, attname, att_float)) != NC_NOERR) {
           std::cerr << ESMC_METHOD << ": nc_get_att_float failure: " << nc_strerror(ncerr) << std::endl;
           return thisArray;
         }
         attValFloat = att_float[0];
         delete[] att_float;
         attValFloatVector.push_back(attValFloat);
         valueBase = (void*) (&attValFloatVector);
         //printf("(ncFloat)\n");
         //printf("     nc att value[%d]: %f\n", j, attValFloat);
        break;

      case NC_DOUBLE:
         double *att_double;
         att_double = new double[attlen];
         if ((ncerr = nc_get_att_double (netCdfFile, varIndex, attname, att_double)) != NC_NOERR) {
           std::cerr << ESMC_METHOD << ": nc_get_att_double failure: " << nc_strerror(ncerr) << std::endl;
           return thisArray;
         }
         attValDouble = att_double[0];
         delete[] att_double;
         attValDoubleVector.push_back(attValDouble);
         valueBase = (void*) (&attValDoubleVector);
         //printf("(ncDouble)\n");
         //printf("     nc att value[%d]: %g\n", j, attValDouble);
        break;

      default:
        break;
      }

      ESMC_TypeKind_Flag	attType = ncToEsmcType(atttype);
      //printf("   ESMC type[%d]: %d (%s)\n", j, attType, 
      //                                      ESMC_TypeKind_FlagString(attType));

      Attribute* esmfAtt = new Attribute(attname,
                                         attType, 1,
                                         valueBase);
      thisArray->root.AttributeSet(esmfAtt);
      //thisArray->root.ESMC_Print();
    }

    return thisArray;
  }


//-------------------------------------------------------------------------

  int  IO_NetCDF::writeArray(NcFile   netCdfFile,
                             Array*   thisArray,
                             int      numDims, 
                             int*  dimensions)
  {

#undef  ESMC_METHOD
#define ESMC_METHOD "IO_NetCDF::writeArray"

    bool trace = false;
    int  rc = ESMF_SUCCESS;

    ESMC_TypeKind_Flag  esmcType = thisArray->getTypekind();
//printf("ESMC Type: %d\n", esmcType);
    nc_type	       ncType = esmcToNcType(esmcType);

    int thisVar;

    if (trace) {
      std::cerr << ESMC_METHOD << ": calling nc_def_var, name = " << thisArray->getName()
          << ", numDims = " << numDims << std::endl;
      for (int i=0; i<numDims; i++)
        std::cerr << "    dimensions[" << i << "] = " << dimensions[i] << std::endl;
    }
    if ((ncerr = nc_def_var (netCdfFile, thisArray->getName(),
                        ncType, numDims, dimensions,
                        &thisVar)) != NC_NOERR) {
      std::cerr << ESMC_METHOD << ": nc_def_var failure: " << nc_strerror(ncerr) << std::endl;
      return ESMF_FAILURE;
    }

//    NcVar*  thisVar = netCdfFile->add_var(thisArray->getName(), 
//                                          ncType, 
//                                          numDims, 
//                                          (const NcDim**)dimensions);

    ncerr = nc_enddef (netCdfFile);  // ensure we are not in Define Mode
    
    LocalArray*  locArray = (thisArray->getLocalarrayList())[0];
    void*             baseAddr;
    baseAddr = locArray->getBaseAddr();

    size_t*		counts = new size_t[numDims];
    for (int i = 0; i < numDims; ++i)
    {
      if ((ncerr = nc_inq_dimlen (netCdfFile, dimensions[i], &counts[i])) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_inq_dimlen failure: " << nc_strerror(ncerr) << std::endl;
        return ESMF_FAILURE;
      }
    }

    switch (ncType)
    {
    case NC_BYTE:
      if ((ncerr = nc_put_var_uchar (netCdfFile, thisVar, (const unsigned char*)baseAddr)) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_put_var_uchar failure: " << nc_strerror(ncerr) << std::endl;
        return ESMF_FAILURE;
      }
//      thisVar->put((const ncbyte*)baseAddr, counts);
      break;

    case NC_CHAR:
      if ((ncerr = nc_put_var_text (netCdfFile, thisVar, (const char*)baseAddr)) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_put_var_text failure: " << nc_strerror(ncerr) << std::endl;
        return ESMF_FAILURE;
      }
//      thisVar->put((const char*)baseAddr, counts);
      break;

    case NC_SHORT:
      if ((ncerr = nc_put_var_short (netCdfFile, thisVar, (const short*)baseAddr)) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_put_var_short failure: " << nc_strerror(ncerr) << std::endl;
        return ESMF_FAILURE;
      }
//      thisVar->put((const short*)baseAddr, counts);
      break;

    case NC_INT:
      if ((ncerr = nc_put_var_int (netCdfFile, thisVar, (const int*)baseAddr)) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_put_var_int failure: " << nc_strerror(ncerr) << std::endl;
        return ESMF_FAILURE;
      }
//      thisVar->put((const int*)baseAddr, counts);
      break;

    case NC_FLOAT:
      if ((ncerr = nc_put_var_float (netCdfFile, thisVar, (const float*)baseAddr)) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_put_var_float failure: " << nc_strerror(ncerr) << std::endl;
        return ESMF_FAILURE;
      }
//      thisVar->put((const float*)baseAddr, counts);
      break;

    case NC_DOUBLE:
      if ((ncerr = nc_put_var_double (netCdfFile, thisVar, (const double*)baseAddr)) != NC_NOERR) {
        std::cerr << ESMC_METHOD << ": nc_put_var_double failure: " << nc_strerror(ncerr) << std::endl;
        return ESMF_FAILURE;
      }
//      thisVar->put((const double*)baseAddr, counts);
      break;

    default:
      break;
    }

    delete[] counts;

    if ((ncerr = nc_sync (netCdfFile)) != NC_NOERR) {
      std::cerr << ESMC_METHOD << ": nc_sync failure: " << nc_strerror(ncerr) << std::endl;
      return ESMF_FAILURE;
    }

    int  numAttributes = thisArray->root.AttributeGetCountTotal();
    if (trace)
      std::cerr << ESMC_METHOD << ": numAttributes = " << numAttributes << std::endl;
    for (int i = 0; i < numAttributes; ++i)
    {
      string         attName;
      ESMC_TypeKind_Flag  attEsmfType;
      int            numAttValues = 0;
      Attribute*     thisAtt = thisArray->root.AttributeGet(i);
      thisArray->root.AttributeGet(i, &attName, &attEsmfType, &numAttValues);

      nc_type  attNcType = esmcToNcType(attEsmfType);

      ncerr = nc_redef (netCdfFile);  // ensure we are in Define Mode

      if (trace)
        std::cerr << ESMC_METHOD << ": attribute name: " << attName.c_str() << std::endl;

      switch (attNcType)
      {
      case NC_CHAR:
        {
          string  attVal;
		  vector<string> attValVector;
          thisArray->root.AttributeGet(attName, &attValVector);
          if (numAttValues == 1) {
            attVal = attValVector.at(0);
            if (trace)
              std::cerr << ESMC_METHOD << ": writing NC_CHAR string attribute: " << attVal.c_str() << ", size = " << attVal.size() << std::endl;
            if ((ncerr = nc_put_att_text (netCdfFile, thisVar, attName.c_str(), attVal.size(), attVal.c_str())) != NC_NOERR) {
              std::cerr << ESMC_METHOD << ": nc_put_att_text failure: " << nc_strerror(ncerr) << std::endl;
              return ESMF_FAILURE;
            }
//            thisVar->add_att(attName.c_str(), attVal.c_str());
          } else {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &rc);
            return ESMF_FAILURE;}
          //printf("      att name[%d]: %s\n", i, attName.c_str());
          //printf("      att val[%d]: %s\n", i, attVal.c_str());
        }
        break;

      case NC_INT:
        {
          if (trace)
            std::cerr << ESMC_METHOD << ": writing NC_INT attribute" << std::endl;
          int  attVal;
          vector<int> attValVector;
          thisArray->root.AttributeGet(attName, &numAttValues, &attValVector);
          if (numAttValues == 1) {
            attVal = attValVector.at(0);
            if ((ncerr = nc_put_att_int (netCdfFile, thisVar, attName.c_str(), NC_INT, 1, &attVal)) != NC_NOERR) {
              std::cerr << ESMC_METHOD << ": nc_put_att_int failure: " << nc_strerror(ncerr) << std::endl;
              return ESMF_FAILURE;
            }

//          thisVar->add_att(attName.c_str(), attVal);
          } else {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &rc);
            return ESMF_FAILURE;}
          //printf("      att name[%d]: %s\n", i, attName.c_str());
          //printf("      att val[%d]: %d\n", i, attVal);
        }
        break;

      case NC_FLOAT:
        {
          if (trace)
            std::cerr << ESMC_METHOD << ": writing NC_FLOAT attribute" << std::endl;
          float  attVal;
          vector<float> attValVector;
          thisArray->root.AttributeGet(attName, &numAttValues, &attValVector);
          if (numAttValues == 1) {
            attVal = attValVector.at(0);
            if ((ncerr = nc_put_att_float (netCdfFile, thisVar, attName.c_str(), NC_FLOAT, 1, &attVal)) != NC_NOERR) {
              std::cerr << ESMC_METHOD << ": nc_put_att_float failure: " << nc_strerror(ncerr) << std::endl;
              return ESMF_FAILURE;
            }
//          thisVar->add_att(attName.c_str(), attVal);
          } else {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &rc);
            return ESMF_FAILURE;}
          //printf("      att name[%d]: %s\n", i, attName.c_str());
          //printf("      att val[%d]: %f\n", i, attVal);
        }
        break;

      case NC_DOUBLE:
        {
          if (trace)
            std::cerr << ESMC_METHOD << ": writing NC_DOUBLE attribute" << std::endl;
          double  attVal;
          vector<double> attValVector;
          thisArray->root.AttributeGet(attName, &numAttValues, &attValVector);
          if (numAttValues == 1) {
            attVal = attValVector.at(0);
            if ((ncerr = nc_put_att_double (netCdfFile, thisVar, attName.c_str(), NC_DOUBLE, 1, &attVal)) != NC_NOERR) {
              std::cerr << ESMC_METHOD << ": nc_put_att_double failure: " << nc_strerror(ncerr) << std::endl;
              return ESMF_FAILURE;
            }
//          thisVar->add_att(attName.c_str(), attVal);
          } else {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, 
              "Write items > 1 - Not yet implemented", ESMC_CONTEXT, &rc);
            return ESMF_FAILURE;}
          //printf("      att name[%d]: %s\n", i, attName.c_str());
          //printf("      att val[%d]: %g\n", i, attVal);
        }
        break;

      default:
        break;
      }
    }

    ncerr = nc_enddef (netCdfFile);

    if ((ncerr = nc_sync (netCdfFile)) != NC_NOERR) {
      std::cerr << ESMC_METHOD << ": nc_sync failure: " << nc_strerror(ncerr) << std::endl;
      return ESMF_FAILURE;
    }

    return rc;
  }
#endif // ESMF_NETCDF

}  // end namespace ESMCI
