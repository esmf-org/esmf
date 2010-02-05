! $Id: ESMF_Util.F90,v 1.18.2.1 2010/02/05 20:01:06 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
#define ESMF_FILENAME "ESMF_Util.F90"

!
! ESMF Util Module
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

      module ESMF_UtilMod
 
      ! parameters, types
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod

#include "ESMF.h"

!BOPI
! !MODULE: ESMF_UtilMod - Interface routines to generic utility functions
!
! !DESCRIPTION:
!
!  Interfaces to, in most cases, the C++ implementation of generic utility
!  functions.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
! !DESCRIPTION:
!
! !PUBLIC MEMBER FUNCTIONS:
!

!  Misc methods
      public ESMF_StringLowerCase
      public ESMF_StringUpperCase

!  Misc type-to-string methods
      public ESMF_StatusString
      public ESMF_TypeKindString
      public ESMF_LogicalString

!  Overloaded = operator functions
      public operator(.eq.), operator(.ne.), assignment(=)
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
               '$Id: ESMF_Util.F90,v 1.18.2.1 2010/02/05 20:01:06 svasquez Exp $'
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StringLowerCase"
!BOPI
!  !IROUTINE:  ESMF_StringLowerCase - convert string to lowercase
!  
! !INTERFACE: 
      subroutine ESMF_StringLowerCase(string, rc) 
!
! !ARGUMENTS:
      character(len=*), intent(inout) :: string
      integer, intent(out), optional  :: rc  

!
! !DESCRIPTION:
!   Converts given string to lowercase.
!
!     The arguments are:
!     \begin{description}
!     \item[string]
!       A character string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: shift, i
      character(len=1) :: c

      shift = ichar('a') - ichar('A')
      do i = 1, len(string)
        c = string(i:i)
        if(c .ge. 'A' .and. c .le. 'Z') then
          string(i:i) = char(ichar(c) + shift)
        endif
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StringLowerCase

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StringUpperCase"
!BOPI
!  !IROUTINE:  ESMF_StringUpperCase - convert string to uppercase
!  
! !INTERFACE: 
      subroutine ESMF_StringUpperCase(string, rc) 
!
! !ARGUMENTS:
      character(len=*), intent(inout) :: string
      integer, intent(out), optional  :: rc  

!
! !DESCRIPTION:
!   Converts given string to uppercase.
!
!     The arguments are:
!     \begin{description}
!     \item[string]
!       A character string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: shift, i
      character(len=1) :: c

      shift = ichar('a') - ichar('A')
      do i = 1, len(string)
        c = string(i:i)
        if(c .ge. 'a' .and. c .le. 'z') then
          string(i:i) = char(ichar(c) - shift)
        endif
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StringUpperCase

!------------------------------------------------------------------------- 
!------------------------------------------------------------------------- 
! misc print routines
!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StatusString"
!BOPI 
!  !IROUTINE:  ESMF_StatusString - Return status as a string
!  
! !INTERFACE: 
      subroutine ESMF_StatusString(status, string, rc)
!
! !ARGUMENTS:
      type(ESMF_Status), intent(in) :: status
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return an {\tt ESMF\_Status} as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[status]
!       The {\tt ESMF\_Status}.
!     \item[string]
!       A printable string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      if (status .eq. ESMF_STATUS_UNINIT) string = "Uninitialized"
      if (status .eq. ESMF_STATUS_READY) string = "Ready"
      if (status .eq. ESMF_STATUS_UNALLOCATED) string = "Unallocated"
      if (status .eq. ESMF_STATUS_ALLOCATED) string = "Allocated"
      if (status .eq. ESMF_STATUS_BUSY) string = "Busy"
      if (status .eq. ESMF_STATUS_INVALID) string = "Invalid"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StatusString

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TypeKindString"
!BOPI 
!  !IROUTINE:  ESMF_TypeKindString - Return TypeKind as a string
!  
! !INTERFACE: 
      subroutine ESMF_TypeKindString(datakind, string, rc)
!
! !ARGUMENTS:
      type(ESMF_TypeKind), intent(in) :: datakind
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return an {\tt ESMF\_TypeKind} variable as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[datakind]
!       The {\tt ESMF\_TypeKind}.
!     \item[string]
!       The value as a string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

#ifndef ESMF_NO_INTEGER_1_BYTE 
      if (datakind .eq. ESMF_TYPEKIND_I1)  string = "Integer*1"
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE 
      if (datakind .eq. ESMF_TYPEKIND_I2)  string = "Integer*2"
#endif
      if (datakind .eq. ESMF_TYPEKIND_I4)  string = "Integer*4"
      if (datakind .eq. ESMF_TYPEKIND_I8)  string = "Integer*8"
      if (datakind .eq. ESMF_TYPEKIND_R4)  string = "Real*4"
      if (datakind .eq. ESMF_TYPEKIND_R8)  string = "Real*8"
      if (datakind .eq. ESMF_C8)  string = "Complex*8"
      if (datakind .eq. ESMF_C16) string = "Complex*16"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_TypeKindString

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogicalString"
!BOPI 
!  !IROUTINE:  ESMF_LogicalString - Return Logical as a string
!  
! !INTERFACE: 
      subroutine ESMF_LogicalString(tf, string, rc)
!
! !ARGUMENTS:
      type(ESMF_Logical), intent(in) :: tf
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return an {\tt ESMF\_Logical} as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[tf]
!       An {\tt ESMF\_Logical}.
!     \item[string]
!       The value as a string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      if (tf .eq. ESMF_TRUE)  string = "True"
      if (tf .eq. ESMF_FALSE) string = "False"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LogicalString

!------------------------------------------------------------------------------


      end module ESMF_UtilMod
