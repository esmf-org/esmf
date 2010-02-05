// $Id: ESMCI_IO_NetCDF_F.C,v 1.1.2.1 2010/02/05 19:58:00 svasquez Exp $
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
//
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <ESMCI_F90Interface.h>
#include <ESMCI_IO_NetCDF.h>
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMCI\_IO} class functions.
//  For missing F90 optional arguments, normalize on passing
//  ESMC_NULL_POINTER to C++ regardless of whether the F90 compiler
//  passes ESMC_BAD_POINTER or ESMC_NULL_POINTER.
//
//EOP

namespace ESMCI
{

// the interface subroutine names MUST be in lower case
extern "C" 
{

//--------------------------------------------------------------------
void FTN(c_esmc_io_netcdfcreate)(IO_NetCDF**  ptr,
                                 int*         nameLen,
                                 const char*  name,
                                 ESMC_Base**  base,
                                 int*         status) 
{
   *ptr = ESMCI_IO_NetCDFCreate(*nameLen,
                                ESMC_NOT_PRESENT_FILTER(name),
                                *base,
                                ESMC_NOT_PRESENT_FILTER(status));
}

//--------------------------------------------------------------------
void FTN(c_esmc_io_netcdfdestroy)(IO_NetCDF**  ptr, 
                                  int*         status) 
{
   int	rc = ESMCI_IO_NetCDFDestroy(ptr);

   if (ESMC_PRESENT(status)) 
   {
      *status = rc;
   }
}

//--------------------------------------------------------------------
void FTN(c_esmc_io_netcdfsetstate)(IO_NetCDF**  ptr,
                                   State*       state,
                                   int*         status) 
{
   ESMF_CHECK_POINTER(*ptr, status)

//state->print();

   (*ptr)->IO_NetCDF::setState(state);

   if (ESMC_PRESENT(status)) 
   {
      *status = ESMF_SUCCESS;
   }
}

//--------------------------------------------------------------------
// This function is a bit messed up... I wanted to return a pointer
// to a State pointer, but I'm having problems with that... so, I'm
// copying the arrays over to the passed in state... I may need to add
// a State copy method to the State class.
//--------------------------------------------------------------------
void FTN(c_esmc_io_netcdfgetstate)(IO_NetCDF**  ptr,
                                   State*       state,
                                   int*         status) 
{
   ESMF_CHECK_POINTER(*ptr, status)
   ESMF_CHECK_POINTER(state, status)

#ifdef ESMF_NETCDF
	State*			localState = (*ptr)->IO_NetCDF::getState();
	vector<string>	arrayNames = localState->getArrayNames();

	for (int i = 0; i < arrayNames.size(); ++i)
	{
		Array*	thisArray;
		localState->getArray((char*)(arrayNames[i].c_str()), &thisArray);

		state->addArray(thisArray);
	}
#endif

//printf("\n");
//(state)->print();
//printf("\n");

   if (ESMC_PRESENT(status)) 
   {
      *status = ESMF_SUCCESS;
   }
}

//--------------------------------------------------------------------
void FTN(c_esmc_io_netcdfread)(IO_NetCDF**  ptr,
                               int*         fileNameLen,
                               const char*  fileName,
                               int*         status) 
{
   ESMF_CHECK_POINTER(*ptr, status)

   int rc = (*ptr)->IO_NetCDF::read(*fileNameLen,
                                    ESMC_NOT_PRESENT_FILTER(fileName));

   if (ESMC_PRESENT(status)) 
   {
      *status = rc;
   }
}

//--------------------------------------------------------------------
void FTN(c_esmc_io_netcdfwrite)(IO_NetCDF**  ptr,
                                int*         fileNameLen,
                                const char*  fileName,
                                int*         status) 
{
   ESMF_CHECK_POINTER(*ptr, status)

   int rc = (*ptr)->IO_NetCDF::write(*fileNameLen,
                                     ESMC_NOT_PRESENT_FILTER(fileName));

   if (ESMC_PRESENT(status)) 
   {
      *status = rc;
   }
}

};

} // namespace ESMCI

