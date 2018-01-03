//$Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
//
// Array of general error message strings, as generic as possible, to allow
// prepending or appending with user strings.

#define ESMC_MAX_ERRORS 1000

static const char *errMsg[] = {

// F90 Error message                                     Return Code (1-500)
// -----------------------------------------------       ----------------------
  "Invalid object ",                                  //    1 = ESMF_RC_OBJ_BAD
  "Object Set or SetDefault method not called ",      //    2 = ESMF_RC_OBJ_INIT
  "Object Create method not called ",                 //    3 = ESMF_RC_OBJ_CREATE
  "Corrupted ESMF object detected ",                  //    4 = ESMF_RC_OBJ_COR
  "Wrong object specified ",                          //    5 = ESMF_RC_OBJ_WRONG

  "Invalid argument ",                                //    6 = ESMF_RC_ARG_BAD
  "Argument rank is not required size ",              //    7 = ESMF_RC_ARG_RANK
  "Argument sizes do not match ",                     //    8 = ESMF_RC_ARG_SIZE
  "Value unrecognized or out of range ",              //    9 = ESMF_RC_ARG_VALUE
  "Two arguments not allowed to be the same ",        //   10 = ESMF_RC_ARG_DUP
  "Two arguments must be same object type ",          //   11 = ESMF_RC_ARG_SAMETYPE
  "Two arguments must have the same communicators ",  //   12 = ESMF_RC_ARG_SAMECOMM
  "Arguments are incompatible ",                      //   13 = ESMF_RC_ARG_INCOMP
  "Argument contains invalid ESMF object ",           //   14 = ESMF_RC_ARG_CORRUPT
  "Wrong argument specified ",                        //   15 = ESMF_RC_ARG_WRONG
  "Input argument out of range ",                     //   16 = ESMF_RC_ARG_OUTOFRANGE
  "Unrecognized option string ",                      //   17 = ESMF_RC_ARG_OPT

  "Operation not yet supported ",                     //   18 = ESMC_RC_NOT_IMPL

  "Unable to open file ",                             //   19 = ESMF_RC_FILE_OPEN
  "Unable to create file ",                           //   20 = ESMF_RC_FILE_CREATE
  "Unable to read from file ",                        //   21 = ESMF_RC_FILE_READ
  "Unable to write to file ",                         //   22 = ESMF_RC_FILE_WRITE
  "Unexpected data in file ",                         //   23 = ESMF_RC_FILE_UNEXPECTED
  "Unable to close file ",                            //   24 = ESMF_RC_FILE_CLOSE
  "Instrumented region is still active ",             //   25 = ESMF_RC_FILE_ACTIVE

  "Value cannot be a NULL pointer ",                  //   26 = ESMF_RC_PTR_NULL
  "Invalid pointer ",                                 //   27 = ESMF_RC_PTR_BAD
  "Pointer must already be allocated ",               //   28 = ESMF_RC_PTR_NOTALLOC
  "Pointer must not already be allocated ",           //   29 = ESMF_RC_PTR_ISALLOC
  "Unspecified memory error ",                        //   30 = ESMF_RC_MEM
  "Unable to allocate memory ",                       //   31 = ESMF_RC_MEM_ALLOCATE
  "Unable to deallocate memory ",                     //   32 = ESMF_RC_MEM_DEALLOCATE
  "Memory corrupted ",                                //   33 = ESMF_RC_MEMC

  "Name already exists ",                             //   34 = ESMF_RC_DUP_NAME
  "Name too long, must be less than ESMF_MAXSTR ",    //   35 = ESMF_RC_LONG_NAME
  "String too long, must be less than ESMF_MAXSTR ",  //   36 = ESMF_RC_LONG_STR
  "Cannot copy non-existent object ",                 //   37 = ESMF_RC_COPY_FAIL
  "Cannot divide by zero ",                           //   38 = ESMF_RC_DIV_ZERO
  "Cannot get value ",                                //   39 = ESMF_RC_CANNOT_GET
  "Cannot set value ",                                //   40 = ESMF_RC_CANNOT_SET
  "Not found ",                                       //   41 = ESMF_RC_NOT_FOUND
  "Not valid " ,                                      //   42 = ESMF_RC_NOT_VALID

  "Internal error: List overflow ",                   //   43 = ESMF_RC_INTNRL_LIST
  "Internal error: Inconsistency ",                   //   44 = ESMF_RC_INTNRL_INCONS
  "Internal error: Bad condition ",                   //   45 = ESMF_RC_INTNRL_BAD

  "System call error ",                               //   46 = ESMF_RC_SYS
  "Resource is busy ",                                //   47 = ESMF_RC_BUSY
  "Error in library called by ESMF ",                 //   48 = ESMF_RC_LIB
  "Library needed by ESMF not present ",              //   49 = ESMF_RC_LIB_NOT_PRESENT

  "Attribute unused ",                                //   50 = ESMF_RC_ATTR_UNUSED
  "Object being used before creation ",               //   51 = ESMF_RC_OBJ_NOT_CREATED
  "Object being used after deletion ",                //   52 = ESMF_RC_OBJ_DELETED
  "Return code not set ",                             //   53 = ESMF_RC_NOT_SET

  "Wrong data value ",                                //   54 = ESMF_RC_VAL_WRONG
  "Value inconsistent with error bound ",             //   55 = ESMF_RC_VAL_ERRBOUND
  "Value out of range ",                              //   56 = ESMF_RC_VAL_OUTOFRANGE

  "Attribute not set ",                               //   57 = ESMF_RC_ATTR_NOTSET
  "Attribute incorrect typekind ",                    //   58 = ESMF_RC_ATTR_WRONGTYPE
  "Attribute wrong number of items ",                 //   59 = ESMF_RC_ATTR_ITEMSOFF
  "Attribute linking error ",                         //   60 = ESMF_RC_ATTR_LINK

  "Input buffer not long enough ",                    //   61 = ESMF_RC_BUFFER_SHORT

  "Timeout reached ",                                 //   62 = ESMF_RC_TIMEOUT

  "File already exists ",                             //   63 = ESMF_RC_FILE_EXISTS
  "File is not a directory ",                         //   64 = ESMF_RC_FILE_NOTDIR
  "The MOAB library returned an error ",              //   65 = ESMF_RC_MOAB_ERROR
  "The NetCDF Library returned an error ",            //   66 = ESMF_RC_NETCDF_ERROR
  "",                                                 //   67 = ESMF_RC_NOOP

// 68-499 reserved for future F90 symmetric return code definitions

                                                     "","","","","","","","",
     "","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",

// non-symmetric error code 500 is empty for now!
  "",

// C++ Error message                                     Return Code (501-1000)
// -----------------------------------------------       -----------------------
  "Invalid object ",                                  //  501 = ESMC_RC_OBJ_BAD
  "Object Set or SetDefault method not called ",      //  502 = ESMC_RC_OBJ_INIT
  "Object Create method not called ",                 //  503 = ESMC_RC_OBJ_CREATE
  "Corrupted ESMF object detected ",                  //  504 = ESMC_RC_OBJ_COR
  "Wrong object specified ",                          //  505 = ESMC_RC_OBJ_WRONG

  "Invalid argument ",                                //  506 = ESMC_RC_ARG_BAD
  "Argument rank is not required size ",              //  507 = ESMC_RC_ARG_RANK
  "Argument sizes do not match ",                     //  508 = ESMC_RC_ARG_SIZE
  "Value unrecognized or out of range ",              //  509 = ESMC_RC_ARG_VALUE
  "Two arguments not allowed to be the same ",        //  510 = ESMC_RC_ARG_DUP
  "Two arguments must be same object type ",          //  511 = ESMC_RC_ARG_SAMETYPE
  "Two arguments must have the same communicators ",  //  512 = ESMC_RC_ARG_SAMECOMM
  "Arguments are incompatible ",                      //  513 = ESMC_RC_ARG_INCOMP
  "Argument contains invalid ESMF object ",           //  514 = ESMC_RC_ARG_CORRUPT
  "Wrong argument specified ",                        //  515 = ESMC_RC_ARG_WRONG
  "Input argument out of range ",                     //  516 = ESMC_RC_ARG_OUTOFRANGE
  "Unrecognized option string ",                      //  517 = ESMC_RC_ARG_OPT

  "Operation not yet supported ",                     //  518 = ESMC_RC_NOT_IMPL

  "Unable to open file ",                             //  519 = ESMC_RC_FILE_OPEN
  "Unable to create file ",                           //  520 = ESMC_RC_FILE_CREATE
  "Unable to read from file ",                        //  521 = ESMC_RC_FILE_READ
  "Unable to write to file ",                         //  522 = ESMC_RC_FILE_WRITE
  "Unexpected data in file ",                         //  523 = ESMC_RC_FILE_UNEXPECTED
  "Unable to close file ",                            //  524 = ESMC_RC_FILE_CLOSE
  "Instrumented region is still active ",             //  525 = ESMC_RC_FILE_ACTIVE

  "Value cannot be a NULL pointer ",                  //  526 = ESMC_RC_PTR_NULL
  "Invalid pointer ",                                 //  527 = ESMC_RC_PTR_BAD
  "Pointer must already be allocated ",               //  528 = ESMC_RC_PTR_NOTALLOC
  "Pointer must not already be allocated ",           //  529 = ESMC_RC_PTR_ISALLOC
  "Unspecified memory error ",                        //  530 = ESMC_RC_MEM
  "Unable to allocate memory ",                       //  531 = ESMF_RC_MEM_ALLOCATE
  "Unable to deallocate memory ",                     //  532 = ESMF_RC_MEM_DEALLOCATE
  "Memory corrupted ",                                //  533 = ESMC_RC_MEMC

  "Name already exists ",                             //  534 = ESMC_RC_DUP_NAME
  "Name too long, must be less than ESMF_MAXSTR ",    //  535 = ESMC_RC_LONG_NAME
  "String too long, must be less than ESMF_MAXSTR ",  //  536 = ESMC_RC_LONG_STR
  "Cannot copy non-existent object ",                 //  537 = ESMC_RC_COPY_FAIL
  "Cannot divide by zero ",                           //  538 = ESMC_RC_DIV_ZERO
  "Cannot get value ",                                //  539 = ESMC_RC_CANNOT_GET
  "Cannot set value ",                                //  540 = ESMC_RC_CANNOT_SET
  "Not found ",                                       //  541 = ESMC_RC_NOT_FOUND
  "Not valid " ,                                      //  542 = ESMC_RC_NOT_VALID

  "Internal error: List overflow ",                   //  543 = ESMC_RC_INTNRL_LIST
  "Internal error: Inconsistency ",                   //  544 = ESMC_RC_INTNRL_INCONS
  "Internal error: Bad condition ",                   //  545 = ESMC_RC_INTNRL_BAD

  "System call error ",                               //  546 = ESMC_RC_SYS
  "Resource is busy ",                                //  547 = ESMC_RC_BUSY
  "Error in library called by ESMF ",                 //  548 = ESMC_RC_LIB
  "Library needed by ESMF not present ",              //  549 = ESMC_RC_LIB_NOT_PRESENT

  "Attribute unused ",                                //  550 = ESMC_RC_ATTR_UNUSED
  "Object being used before creation ",               //  551 = ESMC_RC_OBJ_NOT_CREATED
  "Object being used after deletion ",                //  552 = ESMC_RC_OBJ_DELETED
  "Return code not set ",                             //  553 = ESMC_RC_NOT_SET

  "Wrong data value ",                                //  554 = ESMC_RC_VAL_WRONG
  "Value inconsistent with error bound ",             //  555 = ESMC_RC_VAL_ERRBOUND
  "Value out of range ",                              //  556 = ESMC_RC_VAL_OUTOFRANGE

  "Attribute not set ",                               //  557 = ESMC_RC_ATTR_NOTSET
  "Attribute incorrect typekind ",                    //  558 = ESMC_RC_ATTR_WRONGTYPE
  "Attribute wrong number of items ",                 //  559 = ESMC_RC_ATTR_ITEMSOFF
  "Attribute linking error ",                         //  560 = ESMC_RC_ATTR_LINK

  "Input buffer not long enough ",                    //  561 = ESMC_RC_BUFFER_SHORT

  "Timeout reached ",                                 //  562 = ESMC_RC_TIMEOUT

  "File already exists ",                             //  563 = ESMC_RC_FILE_EXISTS
  "File is not a directory ",                         //  564 = ESMC_RC_FILE_NOTDIR
  "The MOAB library returned an error ",              //  565 = ESMC_RC_MOAB_ERROR
  "The NetCDF library returned an error ",            //  566 = ESMC_RC_NETCDF_ERROR
  "",                                                 //  567 = ESMC_RC_NOOP

// 568-999 reserved for future C++ symmetric return code definitions

                                                        "","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",
  "","","","","","","","","","","","","","","","","","","","","","","","","",

// 1000 used for C++ non-symmetric return code definitions

  "Improperly specified optional argument list ",     // 1000 = ESMC_RC_OPTARG_BAD

};
