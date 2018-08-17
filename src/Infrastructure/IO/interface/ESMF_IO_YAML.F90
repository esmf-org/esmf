! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_IO_YAML.F90"
!==============================================================================
!
!     ESMF I/O YAML Module
      module ESMF_IO_YAMLMod
!     
!==============================================================================
!     
! This file contains the I/O YAML class definition and all class methods.
!     
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_IO_YAMLMod
!     
! !DESCRIPTION:
! Part of I/O YAML Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class {\tt ESMCI\_IO\_YAML} implementation.
!     
! See {\tt ../include/ESMCI\_IO\_YAML.h} for complete description.  TODO ??
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod        ! ESMF error handling
      use ESMF_UtilTypesMod

      implicit none

      private

      type ESMF_IO_YAML
#ifndef ESMF_NO_SEQUENCE
        sequence
#endif
        private
        type(ESMF_Pointer) :: this
        ESMF_INIT_DECLARE
      end type

      type ESMF_IO_YAMLContent_Flag
#ifndef ESMF_NO_SEQUENCE
        sequence
#endif
        private
        integer :: exp_type
      end type

      type (ESMF_IO_YAMLContent_Flag), parameter :: &
        ESMF_IOYAML_CONTENT_UNSET    = ESMF_IO_YAMLContent_Flag(0), &
        ESMF_IOYAML_CONTENT_NATIVE   = ESMF_IO_YAMLContent_Flag(1), &
        ESMF_IOYAML_CONTENT_FREEFORM = ESMF_IO_YAMLContent_Flag(2)

      type ESMF_IO_YAMLParse_Flag
#ifndef ESMF_NO_SEQUENCE
        sequence
#endif
        private
        integer :: parse_form
      end type

      type (ESMF_IO_YAMLParse_Flag), parameter :: &
        ESMF_IOYAML_PARSE_UNSET    = ESMF_IO_YAMLParse_Flag(0), &
        ESMF_IOYAML_PARSE_NUOPCFD  = ESMF_IO_YAMLParse_Flag(1)

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_IO_YAML
      public ESMF_IO_YAMLContent_Flag
      public ESMF_IO_YAMLParse_Flag
!------------------------------------------------------------------------------
! !PUBLIC PARAMETERS:
      public ESMF_IOYAML_CONTENT_UNSET
      public ESMF_IOYAML_CONTENT_NATIVE
      public ESMF_IOYAML_CONTENT_FREEFORM
      public ESMF_IOYAML_PARSE_UNSET
      public ESMF_IOYAML_PARSE_NUOPCFD
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_IO_YAMLCreate
      public ESMF_IO_YAMLDestroy
      public ESMF_IO_YAMLIngest
      public ESMF_IO_YAMLContentInit
      public ESMF_IO_YAMLContentGet
      public ESMF_IO_YAMLContentWrite
      public ESMF_IO_YAMLParse
      public ESMF_IO_YAMLRead
      public ESMF_IO_YAMLWrite
!EOPI

! !PRIVATE MEMBER FUNCTIONS:

!------------------------------------------------------------------------------
      contains
!------------------------------------------------------------------------------

!=============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IO_YAMLCreate()"
!BOPI
! !IROUTINE: ESMF_IO_YAMLCreate - Create a new ESMF YAML I/O object
!
! !INTERFACE:
  function ESMF_IO_YAMLCreate(keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_IO_YAML) :: ESMF_IO_YAMLCreate

! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional :: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc

! !DESCRIPTION:
!     Creates a new {\tt ESMF\_YAML\_IO} object.    
!
!     The arguments are:
!     \begin{description}
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOPI

    ! local return code
    integer :: localrc

!   ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

!   invoke C to C++ entry point to allocate and initialize new IO_YAML object
    call c_ESMC_IO_YAMLCreate(ESMF_IO_YAMLCreate, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    if (present(rc)) rc = localrc

  end function ESMF_IO_YAMLCreate

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IO_YAMLDestroy()"
!BOPI
! !IROUTINE: ESMF_IO_YAMLDestroy - Destroy a new ESMF YAML I/O object
!
! !INTERFACE:
  subroutine ESMF_IO_YAMLDestroy(yaml, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_IO_YAML)                            :: yaml
type(ESMF_KeywordEnforcer), optional :: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc

! !DESCRIPTION:
!     Destroys a new {\tt ESMF\_YAML\_IO} object.    
!
!     The arguments are:
!     \begin{description}
!     \item[{yaml}]
!          Destroy contents of this {\tt ESMF\_IO_YAML} object.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOPI

    ! local return code
    integer :: localrc
!
!   ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
!
!   invoke C to C++ entry point
    call c_ESMC_IO_YAMLDestroy(yaml, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    if (present(rc)) rc = localrc

  end subroutine ESMF_IO_YAMLDestroy

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IO_YAMLRead()"
!BOPI
! !IROUTINE: ESMF_IO_YAMLRead - Read a new ESMF YAML I/O object
!
! !INTERFACE:
  subroutine ESMF_IO_YAMLRead(yaml, fileName, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_IO_YAML)                             :: yaml
    character(len=*),        intent(in)            :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Load YAML content into {\tt ESMF\_YAML\_IO} object from file.
!
!     The arguments are:
!     \begin{description}
!     \item[{yaml}]
!          Existing {\tt ESMF\_IO_YAML} object.
!     \item[{fileName}]
!          Name of YAML file to read content from.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOPI

    ! local return code
    integer :: localrc
!
!   ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
!
!   invoke C to C++ entry point
    call c_ESMC_IO_YAMLRead(yaml, fileName, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    if (present(rc)) rc = localrc

  end subroutine ESMF_IO_YAMLRead

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IO_YAMLIngest()"
!BOPI
! !IROUTINE: ESMF_IO_YAMLIngest - Ingest a new ESMF YAML I/O object
!
! !INTERFACE:
  subroutine ESMF_IO_YAMLIngest(yaml, content, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_IO_YAML)                      :: yaml
    character(len=*), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Ingests YAML content into an {\tt ESMF\_YAML\_IO} object.    
!
!     The arguments are:
!     \begin{description}
!     \item[{yaml}]
!          Ingest content into this {\tt ESMF\_IO_YAML} object.
!     \item[{content}]
!          string holding YAML content
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOPI

    ! local return code
    integer :: localrc
!
!   ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
!
!   invoke C to C++ entry point
    call c_ESMC_IO_YAMLIngest(yaml, content, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    if (present(rc)) rc = localrc

  end subroutine ESMF_IO_YAMLIngest

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IO_YAMLWrite()"
!BOPI
! !IROUTINE: ESMF_IO_YAMLWrite - Write to file YAML content of ESMF YAML I/O object
!
! !INTERFACE:
  subroutine ESMF_IO_YAMLWrite(yaml, keywordEnforcer, fileName, rc)
!
! !ARGUMENTS:
    type(ESMF_IO_YAML)                             :: yaml
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),        intent(in),  optional :: fileName
    integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Writes the YAML content of a {\tt ESMF\_YAML\_IO} object to file.
!
!     The arguments are:
!     \begin{description}
!     \item[{yaml}]
!          Write YAML content of this {\tt ESMF\_IO_YAML} object.
!     \item[{fileName}]
!          The file name to be written to.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOPI

    ! local return code
    integer :: localrc
!
!   ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    if (present(fileName)) then
!     invoke C to C++ entry point
      call c_ESMC_IO_YAMLWrite(yaml, fileName, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
!     invoke C to C++ entry point
      call c_ESMC_IO_YAMLPrint(yaml, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    end if
!

    ! Return success
    if (present(rc)) rc = localrc

  end subroutine ESMF_IO_YAMLWrite

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IO_YAMLParse()"
!BOPI
! !IROUTINE: ESMF_IO_YAMLParse - Parse a new ESMF YAML I/O object
!
! !INTERFACE:
  subroutine ESMF_IO_YAMLParse(yaml, keywordEnforcer, parseflag, rc)
!
! !ARGUMENTS:
    type(ESMF_IO_YAML)                                 :: yaml
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_IO_YAMLParse_Flag), intent(in), optional :: parseflag
    integer,                     intent(out), optional :: rc

! !DESCRIPTION:
!     Parses the content of a {\tt ESMF\_YAML\_IO} object to file.
!
!     The arguments are:
!     \begin{description}
!     \item[{yaml}]
!          Parse contents of this {\tt ESMF\_IO_YAML} object.
!     \item[{parseflag}]
!          Interpret YAML content according to {\tt parseflag}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOPI

    ! local return code
    integer :: localrc
!
!   ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
!
!   invoke C to C++ entry point
    call c_ESMC_IO_YAMLParse(yaml, parseflag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IO_YAMLParse

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IO_YAMLContentInit()"
!BOPI
! !IROUTINE: ESMF_IO_YAMLContentInit - Create content from ESMF YAML I/O object
!
! !INTERFACE:
  subroutine ESMF_IO_YAMLContentInit(yaml, keywordEnforcer, cflag, &
    rc)
!
! !ARGUMENTS:
    type(ESMF_IO_YAML)                                  :: yaml
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_IO_YAMLContent_Flag), intent(in), optional :: cflag
    integer,                      intent(out), optional :: rc

! !DESCRIPTION:
!     Create content from parsed YAML in a {\tt ESMF\_YAML\_IO} object.
!
!     The arguments are:
!     \begin{description}
!     \item[{yaml}]
!          Output parsed contents of this {\tt ESMF\_IO_YAML} object.
!     \item[{cflag}]
!          Return parsed content in {\tt ESMF_IO_YAMLContent_Flag} format
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    ! local return code
    integer :: localrc
    type(ESMF_IO_YAMLContent_Flag) :: localcflag
!
!   ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    localcflag = ESMF_IOYAML_CONTENT_UNSET
    if (present(cflag)) localcflag = cflag

    call c_ESMC_IO_YAMLCInit(yaml, localcflag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IO_YAMLContentInit

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IO_YAMLContentWrite()"
!BOPI
! !IROUTINE: ESMF_IO_YAMLContentWrite - Write content of ESMF YAML I/O object
!
! !INTERFACE:
  subroutine ESMF_IO_YAMLContentWrite(yaml, keywordEnforcer, &
    fileName, rc)
!
! !ARGUMENTS:
    type(ESMF_IO_YAML)                                  :: yaml
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),              intent(in), optional :: fileName
    integer,                      intent(out), optional :: rc

! !DESCRIPTION:
!     Write cached content of a {\tt ESMF\_YAML\_IO} object to file.
!
!     The arguments are:
!     \begin{description}
!     \item[{yaml}]
!          Write parsed contents of this {\tt ESMF\_IO_YAML} object.
!     \item[{fileName}]
!          Name of file to write content to.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    ! local return code
    integer :: localrc
!
!   ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    if (present(fileName)) then
      call c_ESMC_IO_YAMLCWrite(yaml, fileName, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_IO_YAMLCPrint(yaml, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IO_YAMLContentWrite

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IO_YAMLContentGet()"
!BOPI
! !IROUTINE: ESMF_IO_YAMLContentGet - Get content of an ESMF YAML I/O object
!
! !INTERFACE:
  subroutine ESMF_IO_YAMLContentGet(yaml, keywordEnforcer, &
    content, contentSize, lineCount, rc)
!
! !ARGUMENTS:
    type(ESMF_IO_YAML)                                  :: yaml
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),             intent(out), optional :: content(:)
    integer,                      intent(out), optional :: contentSize
    integer,                      intent(out), optional :: lineCount
    integer,                      intent(out), optional :: rc

! !DESCRIPTION:
!     Retrieve the parsed content of a {\tt ESMF\_YAML\_IO} object.
!
!     The arguments are:
!     \begin{description}
!     \item[{yaml}]
!          Output parsed content of this {\tt ESMF\_IO_YAML} object.
!     \item[{content}]
!          A character(1) array that will hold the content. It must be
!          at least of size {\tt contentSize}.
!     \item[{contentSize}]
!          The number of characters of the available content. It should be
!          retrieved first, to properly allocate the {\tt content} array.
!     \item[{lineCount}]
!          The number of lines of the available content. It should be
!          retrieved first, to properly allocate the {\tt content} array.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOPI

    ! local return codes
    integer :: localrc, stat
    ! local variables
    integer :: i, j, k
    integer :: contentLineCount, contentStringLen
    character(len=1), allocatable :: buffer(:)
!
!   ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    if (present(contentSize)) then
      call c_ESMC_IO_YAMLCSize(yaml, contentSize, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    if (present(lineCount)) then
      call c_ESMC_IO_YAMLCLineC(yaml, lineCount, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    if (present(content)) then

      ! setup receiving buffer for parsed content
      contentStringLen = len(content(1))
      contentLineCount = size(content)
      allocate(buffer(contentStringLen * contentLineCount), &
        stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of string buffer.", &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! initialize buffer
      buffer = ""

      call c_ESMC_IO_YAMLCGet(yaml, buffer, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! convert received content from 1-d to array of strings, delimited by newline characters
      content = ""
      k = 0
      i = 1
      do j = 1, size(buffer)
        if (buffer(j) == achar(0)) exit
        if (buffer(j) == achar(10)) then
            k = 0
            i = i + 1
            if (i > contentLineCount) exit
            cycle
        else
            k = k + 1
            if (k > contentStringLen) cycle
        end if
        content(i)(k:k) = buffer(j)
      end do

      ! release buffer memory
      deallocate(buffer, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Free memory associated with string buffer.", &
        ESMF_CONTEXT, rcToReturn=rc)) return

    end if

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IO_YAMLContentGet

!------------------------------------------------------------------------------

      end module ESMF_IO_YAMLMod
