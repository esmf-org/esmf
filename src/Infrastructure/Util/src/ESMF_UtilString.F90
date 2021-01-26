! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
#define ESMF_FILENAME "ESMF_UtilString.F90"

!
! ESMF UtilString Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------
! one blank line for protex processing - in case all routines here are
! marked internal (BOPI/EOPI), the output file will still have contents.
!BOP

!EOP

!------------------------------------------------------------------------------
! module definition

      module ESMF_UtilStringMod
 
      ! parameters, types

#include "ESMF.h"

!BOPI
! !MODULE: ESMF_UtilStringMod - Low level Fortran character string utility functions
!
! !DESCRIPTION:
!
!  Low level Fortran character string functions used within ESMF.
!
!EOPI
!------------------------------------------------------------------------------

! !USES:
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
!

!  String functions
      public :: ESMF_UtilArray2String
      public :: ESMF_UtilString2Array
      public :: ESMF_StringConcat

!
!

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id$'
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilArray2String"
!BOPI
!  !IROUTINE:  ESMF_UtilArray2String - convert character array to string
!  
! !INTERFACE: 
    function ESMF_UtilArray2String (charArray) 
!
! !ARGUMENTS:
      character(len=1), intent(in) :: charArray(:)

!
! !RETURN VALUE:
      character(len=size (charArray)) :: ESMF_UtilArray2String

!
! !DESCRIPTION:
!   Converts given an array of characters to a string.
!
!     The arguments are:
!     \begin{description}
!     \item[charArray]
!       An array of characters.
!     \end{description}
!
!
!EOPI

      ESMF_UtilArray2String = transfer (charArray, mold=ESMF_UtilArray2String)

    end function ESMF_UtilArray2String

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilString2Array"
!BOPI
!  !IROUTINE:  ESMF_UtilString2Array - convert character string to array
!  
! !INTERFACE: 
      function ESMF_UtilString2Array (string) 
!
! !ARGUMENTS:
      character(len=*), intent(in) :: string
!
! !RETURN VALUE:
      character(len=1) :: ESMF_UtilString2Array(len (string))

!
! !DESCRIPTION:
!   Converts given a string to an array of characters.
!
!     The arguments are:
!     \begin{description}
!     \item[string]
!       A character string.
!     \end{description}
!
!
!EOPI

      ESMF_UtilString2Array = transfer (string, mold=ESMF_UtilString2Array)

      end function ESMF_UtilString2Array

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StringConcat"
!BOPI
!  !IROUTINE:  ESMF_StringConcat - concatenate two strings
!  
! !INTERFACE: 
    function ESMF_StringConcat (string1, string2) 
!
! !ARGUMENTS:
      character(len=*), intent(in) :: string1
      character(len=*), intent(in) :: string2
!
! !RETURN VALUE:
      character(len=len (string1) + len (string2)) :: ESMF_StringConcat

!
! !DESCRIPTION:
!   Concatenates two strings.  Useful for .cppF90 files where the first pass preprocessing
!   may treat the Fortran // operator to be a C++ inline comment.
!
!     The arguments are:
!     \begin{description}
!     \item[string1]
!       A character string.
!     \item[string2]
!       A character string.
!     \end{description}
!
!
!EOPI

      ESMF_StringConcat(:len (string1))    = string1
      ESMF_StringConcat( len (string1)+1:) = string2

    end function ESMF_StringConcat

!------------------------------------------------------------------------------


      end module ESMF_UtilStringMod
