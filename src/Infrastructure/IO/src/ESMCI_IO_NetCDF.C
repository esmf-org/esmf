// $Id: ESMCI_IO_NetCDF.C,v 1.8.2.1 2010/02/05 19:58:01 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
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
//
 #define ESMC_FILENAME "ESMCI_IO_NetCDF.C"

 // higher level, 3rd party or system includes here
 #include <stdio.h>
 #include <string.h>
 #include <ctype.h>
 #include <iostream>

 #include <ESMC_Util.h>
 #include <ESMCI_LogErr.h>
 #include <ESMF_LogMacros.inc>
 #include <ESMCI_VM.h>
 #include <ESMCI_ArraySpec.h>
 #include <ESMCI_LocalArray.h>
 #include <ESMCI_Array.h>

 // associated class definition file
 #include <ESMCI_IO_NetCDF.h>

 using namespace std; 

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_IO_NetCDF.C,v 1.8.2.1 2010/02/05 19:58:01 svasquez Exp $";
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
      int                nameLen,          // in
      const char        *name,             // in
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

    if (name != ESMC_NULL_POINTER) {
      // use given name
      returnCode = io_netcdf->ESMC_BaseSetF90Name((char*) name, nameLen);
    } else {
      // create default name "IO_NetCDF<ID>"
      returnCode = io_netcdf->ESMC_BaseSetName((const char*) ESMC_NULL_POINTER,
                                               "IO_NetCDF");
    }
    ESMC_LogDefault.MsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);

    if (base != ESMC_NULL_POINTER) io_netcdf->base = base;

    // TODO returnCode = io_netcdf->validate();
    returnCode = ESMF_SUCCESS;
    ESMC_LogDefault.MsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);
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
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to io_netcdf", &rc);
    return rc;
  }

  try{
    // destruct Array object
    (*io_netcdf)->destruct();
    // mark as invalid object
    (*io_netcdf)->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
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
      int                fileNameLen,         // in
      const char        *fileName) {          // in

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
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // only read on pet 0
    globalVM = ESMCI::VM::getGlobal(&rc);
    if ((globalVM == ESMC_NULL_POINTER) || (rc != ESMF_SUCCESS)) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "FAIL rc=%d, Unable to get GlobalVM\n", rc);
      ESMC_LogDefault.Write(logMsg, ESMC_LOG_WARN, ESMC_CONTEXT);
      return(ESMF_FAILURE);
    }
    mypet = globalVM->getLocalPet(); 
    numPETs = globalVM->getPetCount();
//printf("mypet = %d, numPETS = %d\n", mypet, numPETs);
//fflush(stdout);
    if (mypet != 0) return rc; 

    if (fileName != ESMC_NULL_POINTER) 
    {
      // TODO: only use local of fileName this one time;
      //   don't change set IO_NetCDF member fileName
      if (fileNameLen < ESMF_MAXSTR) 
      {
        strncpy(this->fileName, fileName, fileNameLen);
        this->fileName[fileNameLen] = '\0';  // null terminate
      } 
      else 
      {
        // truncate
        strncpy(this->fileName, fileName, ESMF_MAXSTR-1);
        this->fileName[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg,
                "io_netcdf fileName %s, length >= ESMF_MAXSTR; truncated.",
                fileName);
        ESMC_LogDefault.Write(logMsg, ESMC_LOG_WARN, ESMC_CONTEXT);
        // TODO: return ESMF_WARNING when defined
        // if (rc != ESMC_NULL_POINTER) *rc = ESMF_WARNING;
      }
    } 
    else 
    {
      // TODO use existing IO_NetCDF fileName member
    }

#ifdef ESMF_NETCDF
    NcFile* netCdfFile = new NcFile(this->fileName, NcFile::ReadOnly);

    if (!(netCdfFile->is_valid()))
    {
      // TODO: throw error, or return
      return ESMF_FAILURE;
    }

    //***
    // Create the State object that we'll be filling with Arrays
    //***
#if 0
    int stateRc = 0;
    theState = State::create(this->fileName, &stateRc);
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
         "; 'theState' pointer is NULL.", &rc);
      return(ESMF_FAILURE);
    }

    /*
    ** Dimensions
    */
    int	numDims = netCdfFile->num_dims();
    //printf("\nNum Dimensions: %d\n", numDims);

    for (int i = 0; i < numDims; ++i)
    {
      NcDim*	thisDim = netCdfFile->get_dim(i);

      if (thisDim->is_valid())
      {
        //printf("Dimension Name: %s\n", thisDim->name());
        //printf("          Size: %d\n", thisDim->size());
      }
      else
      {
        // TODO:  return ESMF error?
      }
    }

    /*
    ** Variables
    */
    int	numVars = netCdfFile->num_vars();
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
      }
    }

    /*
    ** Attributes
    */
    int	numAtts = netCdfFile->num_atts();
    //printf("\nNum Attributes: %d\n", numAtts);

    for (int i = 0; i < numAtts; ++i)
    {
      NcAtt*  thisAtt = netCdfFile->get_att(i);

      if (thisAtt->is_valid())
      {
        //printf("Attribute Name: %s\n", thisAtt->name());
        //printf("          Type: %d\n", thisAtt->type());
        //printf("          Vals: %d\n", thisAtt->num_vals());
      }
      else
      {
        // TODO:  return ESMF error?
      }
    }

    delete netCdfFile;
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
      int                fileNameLen,         // in
      const char        *fileName) {          // in

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
         "; 'this' pointer is NULL.", &rc);
      return(ESMF_FAILURE);
    }

    // only write on pet 0
    globalVM = ESMCI::VM::getGlobal(&rc);
    if ((globalVM == ESMC_NULL_POINTER) || (rc != ESMF_SUCCESS)) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "FAIL rc=%d, Unable to get GlobalVM\n", rc);
      ESMC_LogDefault.Write(logMsg, ESMC_LOG_WARN, ESMC_CONTEXT);
      return(ESMF_FAILURE);
    }
    mypet = globalVM->getLocalPet(); 
    numPETs = globalVM->getPetCount();
//printf("mypet = %d, numPETS = %d\n", mypet, numPETs);
//fflush(stdout);
    if (mypet != 0) return rc; 

#ifdef ESMF_NETCDF
    // check only when netCDF present
    if (theState == ESMC_NULL_POINTER) 
    {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'theState' pointer is NULL.", &rc);
      return(ESMF_FAILURE);
    }
#endif

    if (fileName != ESMC_NULL_POINTER) 
    {
      // TODO: only use local of fileName this one time;
      //   don't change set IO_NetCDF member fileName
      if (fileNameLen < ESMF_MAXSTR) 
      {
        strncpy(this->fileName, fileName, fileNameLen);
        this->fileName[fileNameLen] = '\0';  // null terminate
      } 
      else 
      {
        // truncate
        strncpy(this->fileName, fileName, ESMF_MAXSTR-1);
        this->fileName[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg,
                "io_netcdf fileName %s, length >= ESMF_MAXSTR; truncated.",
                fileName);
        ESMC_LogDefault.Write(logMsg, ESMC_LOG_WARN,ESMC_CONTEXT);
        // TODO: return ESMF_WARNING when defined
        // if (rc != ESMC_NULL_POINTER) *rc = ESMF_WARNING;
      }
    } 
    else 
    {
      // TODO use existing IO_NetCDF fileName member
    }

#ifdef ESMF_NETCDF
        NcFile*	netCdfFile = new NcFile(this->fileName, NcFile::Replace);

        if (!(netCdfFile->is_valid()))
        {
          // TODO: throw error, or return
          return ESMF_FAILURE;
        }

        netCdfFile->set_fill(NcFile::Fill);

        int  numArrays = 0;
        theState->getNumArrays(&numArrays);

        vector<string>  arrayNames = theState->getArrayNames();

        for (int i = 0; i < arrayNames.size(); ++i)
        {
           cout << "Item[" << i << "]: " << arrayNames[i] << endl;
           Array*    thisArray;
           theState->getArray((char*)(arrayNames[i].c_str()), &thisArray);

           LocalArray*  localArray = (thisArray->getLocalarrayList())[0];
           int               numDims = localArray->getRank();
           const int*        dimLengths = new int[numDims];
           NcDim**           dimensions = new NcDim*[numDims];

           dimLengths = localArray->getCounts();

           for (int j = 0; j < numDims; ++j)
           {
             char		dimName[256];

             sprintf(dimName, "dim_%d_%d", i, j);
             //printf("Dim[%d] length: %d\n", j, dimLengths[j]);

             dimensions[j] = netCdfFile->add_dim(dimName, dimLengths[j]);
           }

           writeArray(netCdfFile, thisArray, numDims, dimensions);
        }

        delete netCdfFile;
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
  ESMC_TypeKind  IO_NetCDF::ncToEsmcType(NcType  ncTypeVal) 
  {
    ESMC_TypeKind  esmcTypeVal = ESMF_NOKIND;

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

  NcType  IO_NetCDF::esmcToNcType(ESMC_TypeKind  esmcTypeVal) 
  {
    NcType  ncTypeVal = ncNoType;

    switch (esmcTypeVal)
    {
    case ESMC_TYPEKIND_I1:
      ncTypeVal = ncByte;
      break;
    case ESMC_TYPEKIND_I2:
      ncTypeVal = ncShort;
      break;
    case ESMC_TYPEKIND_I4:
      ncTypeVal = ncInt;
      break;
    case ESMC_TYPEKIND_I8:
      ncTypeVal = ncLong;  // TODO?: deprecated in netCDF - same ncInt
      break;
    case ESMC_TYPEKIND_R4:
      ncTypeVal = ncFloat;
      break;
    case ESMC_TYPEKIND_R8:
      ncTypeVal = ncDouble;
      break;
    case ESMF_C8:
      ncTypeVal = ncNoType;
      // TODO:  ncTypeVal = netCDF 8 byte complex type?
      break;
    case ESMF_C16:
      ncTypeVal = ncNoType;
      // TODO:  ncTypeVal = netCDF 16 byte complex type?
      break;
    case ESMC_TYPEKIND_LOGICAL:
      ncTypeVal = ncByte;
      break;
    case ESMC_TYPEKIND_CHARACTER:
      ncTypeVal = ncChar;
      break;
    default:
      break;
    }

    return ncTypeVal;
  }



//-------------------------------------------------------------------------

  Array*  IO_NetCDF::readArray(NcFile*  netCdfFile,
                               int      varIndex) 
  {
    NcVar*  thisVar = netCdfFile->get_var(varIndex);
    Array*  thisArray = NULL;

    if (!(thisVar->is_valid()))
    {
       return thisArray;
    }

    //printf("Variable Name: %s\n", thisVar->name());
    //printf("         Type: %d\n", thisVar->type());
    //printf("         Dims: %d\n", thisVar->num_dims());

    int*   dimSizes = new int[thisVar->num_dims()];
    int*   minIndices = new int[thisVar->num_dims()];
    int*   maxIndices = new int[thisVar->num_dims()];

    for (int j = 0; j < thisVar->num_dims(); ++j)
    {
      NcDim*	thisDim = thisVar->get_dim(j);

      dimSizes[j] = thisDim->size();
      minIndices[j] = 0;
      maxIndices[j] = thisDim->size() - 1;
    }

    ESMC_TypeKind	arrayType = ncToEsmcType(thisVar->type());

    int		numValues = thisVar->num_vals();
    NcValues*	values = thisVar->values();

    int      arrayRc = 0;
    LocalArray*	locArray = LocalArray::create(
                                   arrayType,
                                   thisVar->num_dims(),
                                   dimSizes,
                                   minIndices,
                                   maxIndices,
                                   values->base(),
                                   (char*)thisVar->name(),
                                   DATA_COPY,
                                   &arrayRc);
    //printf("*** LocalArray RC: %d\n", arrayRc);
    //locArray->print("full");

    InterfaceInt*	minIndex = new InterfaceInt(minIndices, 
                                              thisVar->num_dims());
    InterfaceInt*	maxIndex = new InterfaceInt(maxIndices, 
                                              thisVar->num_dims());

    if (arrayRc != ESMF_SUCCESS)
    {
      return thisArray;
    }

    DistGrid*	distGrid = DistGrid::create(
                               minIndex, maxIndex,
                               (InterfaceInt*)NULL, (DecompFlag*)NULL, 0,
                               (InterfaceInt*)NULL, (InterfaceInt*)NULL,
                               (InterfaceInt*)NULL, (ESMC_IndexFlag*)NULL,
                               (InterfaceInt*)NULL, 
                               (DELayout*)NULL, (VM*)NULL, &arrayRc);
    //printf("*** DistGrid RC: %d\n", arrayRc);

    if (arrayRc != ESMF_SUCCESS)
    {
      return thisArray;
    }

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

    thisArray->setName((char*)(thisVar->name()));

    //thisArray->print();
    //printf("    Atts: %d\n", thisVar->num_atts());

    for (int j = 0; j < thisVar->num_atts(); ++j)
    {
      NcAtt*	thisAtt = thisVar->get_att(j);
      //printf("      nc att name[%d]: %s\n", j, thisAtt->name());
      //printf("      nc att type[%d]: %d ", j, thisAtt->type());

      void* valueBase;
      string attValString;
      int attValInt;
      float attValFloat;
      double attValDouble;

      switch (thisAtt->type())
      {
      case NC_CHAR:
         attValString = string(thisAtt->values()->as_string(0));
         valueBase = (void*) (&attValString);
         //printf("(ncChar)\n");
         //printf("     nc att value[%d]: %s\n", j, attValString.c_str());
         
        break;

      case NC_INT:
         attValInt = thisAtt->values()->as_int(0);
         valueBase = (void*) (&attValInt);
         //printf("(ncInt)\n");
         //printf("     nc att value[%d]: %d\n", j, attValInt);
        break;

      case NC_FLOAT:
         attValFloat = thisAtt->values()->as_float(0);
         valueBase = (void*) (&attValFloat);
         //printf("(ncFloat)\n");
         //printf("     nc att value[%d]: %f\n", j, attValFloat);
        break;

      case NC_DOUBLE:
         attValDouble = thisAtt->values()->as_double(0);
         valueBase = (void*) (&attValDouble);
         //printf("(ncDouble)\n");
         //printf("     nc att value[%d]: %g\n", j, attValDouble);
        break;

      default:
        break;
      }

      ESMC_TypeKind	attType = ncToEsmcType(thisAtt->type());
      //printf("   ESMC type[%d]: %d (%s)\n", j, attType, 
      //                                      ESMC_TypeKindString(attType));

      Attribute* esmfAtt = new Attribute(thisAtt->name(),
                                         attType,
                                         1,
                                         valueBase);
      thisArray->root.AttributeSet(esmfAtt);
      //thisArray->root.ESMC_Print();
    }

    return thisArray;
  }


//-------------------------------------------------------------------------

  int  IO_NetCDF::writeArray(NcFile*  netCdfFile,
                             Array*   thisArray,
                             int      numDims, 
                             NcDim**  dimensions)
  {
    int  rc = ESMF_SUCCESS;

    ESMC_TypeKind  esmcType = thisArray->getTypekind();
//printf("ESMC Type: %d\n", esmcType);
    NcType	       ncType = esmcToNcType(esmcType);

    NcVar*  thisVar = netCdfFile->add_var(thisArray->getName(), 
                                          ncType, 
                                          numDims, 
                                          (const NcDim**)dimensions);

    
    LocalArray*  locArray = (thisArray->getLocalarrayList())[0];
    void*             baseAddr;
    baseAddr = locArray->getBaseAddr();

    long*		counts = new long[numDims];
    for (int i = 0; i < numDims; ++i)
    {
      counts[i] = dimensions[i]->size();
    }

    switch (ncType)
    {
    case NC_BYTE:
      thisVar->put((const ncbyte*)baseAddr, counts);
      break;

    case NC_CHAR:
      thisVar->put((const char*)baseAddr, counts);
      break;

    case NC_SHORT:
      thisVar->put((const short*)baseAddr, counts);
      break;

    case NC_INT:
      thisVar->put((const int*)baseAddr, counts);
      break;

    case NC_FLOAT:
      thisVar->put((const float*)baseAddr, counts);
      break;

    case NC_DOUBLE:
      thisVar->put((const double*)baseAddr, counts);
      break;

    default:
      break;
    }

    netCdfFile->sync();

    int  numAttributes = thisArray->root.AttributeGetCountTotal();
    for (int i = 0; i < numAttributes; ++i)
    {
      string         attName;
      ESMC_TypeKind  attEsmfType;
      int            numAttValues = 0;
      Attribute*     thisAtt = thisArray->root.AttributeGet(i);
      thisArray->root.AttributeGet(i, &attName, &attEsmfType, &numAttValues);

      NcType  attNcType = esmcToNcType(attEsmfType);

      switch (attNcType)
      {
      case NC_CHAR:
        {
          string  attVal;
          thisArray->root.AttributeGet(attName, &attVal);
          thisVar->add_att(attName.c_str(), attVal.c_str());
          //printf("      att name[%d]: %s\n", i, attName.c_str());
          //printf("      att val[%d]: %s\n", i, attVal.c_str());
        }
        break;

      case NC_INT:
        {
          int  attVal;
          thisArray->root.AttributeGet(attName, &attVal);
          thisVar->add_att(attName.c_str(), attVal);
          //printf("      att name[%d]: %s\n", i, attName.c_str());
          //printf("      att val[%d]: %d\n", i, attVal);
        }
        break;

      case NC_FLOAT:
        {
          float  attVal;
          thisArray->root.AttributeGet(attName, &attVal);
          thisVar->add_att(attName.c_str(), attVal);
          //printf("      att name[%d]: %s\n", i, attName.c_str());
          //printf("      att val[%d]: %f\n", i, attVal);
        }
        break;

      case NC_DOUBLE:
        {
          double  attVal;
          thisArray->root.AttributeGet(attName, &attVal);
          thisVar->add_att(attName.c_str(), attVal);
          //printf("      att name[%d]: %s\n", i, attName.c_str());
          //printf("      att val[%d]: %g\n", i, attVal);
        }
        break;

      default:
        break;
      }
    }

    return rc;
  }
#endif // ESMF_NETCDF

}  // end namespace ESMCI
