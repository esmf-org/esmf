! $Id: ESMF_Util.F90,v 1.20 2010/04/06 00:03:47 w6ws Exp $
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

!  Command line argument methods
      public ESMF_UtilGetArgC
      public ESMF_UtilGetArg

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
               '$Id: ESMF_Util.F90,v 1.20 2010/04/06 00:03:47 w6ws Exp $'
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilGetArgC"
!BOP
! !IROUTINE:  ESMF_UtilGetArgC - Return number of command line arguments
!
! !INTERFACE:
  integer function ESMF_UtilGetArgC ()
!
! !ARGUMENTS:
!  none
!
! !Description:
! This method returns the number of command line arguments specified
! when the process was started.
!
! The number of arguments returned does not include the name of the
! command itself - which is typically returned as argument zero.
!EOP
    integer :: argc

#if !defined (ESMF_NEEDSPXFGETARG) && !defined (ESMF_NEEDSGETARG)
! Fortran 2003 version (default and preferred)

    argc = command_argument_count ()

#elif defined (ESMF_NEEDSPXFGETARG)
! POSIX Fortran bindings (1003.9-1992)

    integer, external :: ipxfargc

    argc = ipxfargc ()

#elif defined (ESMF_NEEDSGETARG)
! Non-Standard, but implemented by many compilers.

    integer, external :: ipxf_argc

    argc = iargc ()

#else
#error need arg count support
#endif

    ESMF_UtilGetArgC = argc

  end function ESMF_UtilGetArgC

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilGetArg"
!BOP
! !IROUTINE:  ESMF_UtilGetArg - Return a command line argument
!
! !INTERFACE:
  subroutine ESMF_UtilGetArg (argindex, value, length, rc)
!
! !ARGUMENTS:
    integer, intent(in) :: argindex
    character(*), intent(out), optional :: value
    integer, intent(out), optional :: length
    integer, intent(out), optional :: rc
!
! !Description:
! This method returns a copy of a command line argument specified
! when the process was started.  This argument is the same as an
! equivalent C++ program would find in the argv array.
!
! The arguments are:
! \begin{description}
! \item [{argindex}]
! A non-negative index into the command line argument {\tt argv} array.
! If argindex is negative or greater than the number of user-specified
! arguments, ESMF\_RC\_ARG\_VALUE is returned in the {\tt rc} argument.
! \item [{[value]}]
! Returns a copy of the desired command line argument.  If the provided
! character string is longer than the command line argument, the string
! will be blank padded.  If the string is too short, truncation will
! occur and ESMF\_RC\_ARG\_SIZE is returned in the {\tt rc} argument.
! \item [{[length]}]
! Returns the length of the desired command line argument in characters.
! The length result does not depend on the length of the {\tt value}
! string.  It may be used to query the length of the argument.
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------- 
#if defined (ESMF_NEEDSPXFGETARG) || defined (ESMF_NEEDSGETARG)
    character(4*ESMF_MAXSTR) :: localvalue
    integer :: locallength
#endif
    integer :: localargc, localrc

#if defined (ESMF_NEEDSPXFGETARG)
    integer, external :: ipxfconst
    integer :: ETRUNC, EINVAL
#endif

    ! assume failure until success
    if (present (rc)) then
      rc = ESMF_RC_NOT_IMPL
    end if

    localargc = ESMF_UtilGetArgc ()
    if (present (value)) value = ""
    if (present (length)) length = 0
    localrc = merge (ESMF_SUCCESS, ESMF_RC_ARG_VALUE,  &
        argindex >= 0 .and. argindex <= localargc)
    if (ESMF_LogMsgFoundError ( localrc,  &
        ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rc))  &
      return

#if !defined (ESMF_NEEDSPXFGETARG) && !defined (ESMF_NEEDSGETARG)
! Fortran 2003 version (default and preferred)

    call get_command_argument (argindex, value, length, status=localrc)

    select case (localrc)
    case (0)
      localrc = ESMF_SUCCESS
    case (-1)
      localrc = ESMF_RC_ARG_SIZE
    case (1:)
      localrc = ESMF_RC_ARG_VALUE
    end select

#elif defined (ESMF_NEEDSPXFGETARG)
! POSIX Fortran bindings (1003.9-1992)
! Has error checking comparable to F2003.  So when F2003 intrinsics
! are not available and PXF is available (e.g., on IRIX), it is
! preferable to calling 'getarg'.

    ETRUNC = ipxfconst ("ETRUNC")
    EINVAL = ipxfconst ("EINVAL")

    if (present (value)) then
      call pxfgetarg (argindex, value, locallength, localrc)
    else
      call pxfgetarg (argindex, localvalue, locallength, localrc)
      if (localrc == ETRUNC) localrc = 0
    end if

    if (localrc == 0) then
      localrc = ESMF_SUCCESS
    else if (localrc == ETRUNC) then
      localrc = ESMF_RC_ARG_SIZE
    else if (localrc == EINVAL) then
      localrc = ESMF_RC_ARG_VALUE
    end if
 
    if (present (length)) then
      length = locallength
    end if
     
#elif defined (ESMF_NEEDSGETARG)
! Non-Standard.  But dates back to the original 7th Edition unix f77
! compiler, so is implemented by many compilers.  No further error
! checking, especially for bad character string lengths, is possible.

    if (present (value)) then
      call getarg (argindex, value)
      locallength = len_trim (value)
    else
      call getarg (argindex, localvalue)
      locallength = len_trim (localvalue)
    end if

    if (present (length)) then
      length = locallength
    end if

    localrc = ESMF_SUCCESS

#else
#error need getarg support
#endif

    if (ESMF_LogMsgFoundError ( localrc,  &
        ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rc))  &
      return

    if (present (rc)) then
      rc = localrc
    end if

  end subroutine ESMF_UtilGetArg

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
