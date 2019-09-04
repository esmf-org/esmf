! $Id$
!==============================================================================
! Earth System Modeling Framework
!
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
#define ESMF_FILENAME "ESMF_Config.F90"
!
!     ESMF Configuration module
      module ESMF_ConfigMod
!
#include "ESMF.h"
!==============================================================================
!
! This file contains the Config class definition and all Config
! class methods.
!
!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_ConfigMod - Implements ESMF configuration management
!
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_Config} class that implements 
! ESMF configuration management system.
!
! !USES:

      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_UtilMod
      use ESMF_BaseMod
      use ESMF_DELayoutMod
      use ESMF_IOUtilMod
      use ESMF_LogErrMod 
      use ESMF_InitMacrosMod

      implicit none
      private
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!------------------------------------------------------------------------------
       public :: ESMF_ConfigCreate     ! creates configuration
       public :: ESMF_ConfigDestroy    ! destroys configuration
       public :: ESMF_ConfigLoadFile   ! loads resource file into memory
       public :: ESMF_ConfigFindLabel  ! selects a label (key)
       public :: ESMF_ConfigFindNextLabel  ! selects a following label (key)
       public :: ESMF_ConfigNextLine   ! selects next line (for tables)
       public :: ESMF_ConfigGetAttribute ! returns next value
       public :: ESMF_ConfigGetChar    ! returns only a single character
       public :: ESMF_ConfigGetLen ! gets number of words in the line(function)
       public :: ESMF_ConfigGetDim ! gets number of lines in the table
                                   ! and max number of columns by word 
                                   ! counting disregarding type (function)
       public :: ESMF_ConfigIsCreated
       public :: ESMF_ConfigPrint  ! print content of config object
       public :: ESMF_ConfigSetAttribute ! sets value
       public :: ESMF_ConfigValidate   ! validates config object
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!------------------------------------------------------------------------------
       public :: ESMF_Config
       public :: ESMF_ConfigClass, ESMF_ConfigAttrUsed  !For internal lib use only

       public ESMF_ConfigAttrUsedInit       ! For Standardized Initialization
       public ESMF_ConfigAttrUsedValidate   ! For Standardized Initialization
       public ESMF_ConfigAttrUsedGetInit    ! For Standardized Initialization

       public ESMF_ConfigClassInit          ! For Standardized Initialization
       public ESMF_ConfigClassValidate      ! For Standardized Initialization
       public ESMF_ConfigClassGetInit       ! For Standardized Initialization

       public ESMF_ConfigGetInit            ! For Standardized Initialization
!EOPI

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !IROUTINE: ESMF_ConfigCreate - Create a Config object
!
! !INTERFACE:
    interface ESMF_ConfigCreate

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ConfigCreateEmpty
        module procedure ESMF_ConfigCreateFromSection

! !DESCRIPTION:
!     This interface provides methods to create an empty {\tt ESMF\_Config}
!     object or a new {\tt ESMF\_Config} object from a section of an existing
!     one.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------

!BOPI
! !IROUTINE: ESMF_ConfigGetAttribute - Get an attribute from a Config object
!
! !INTERFACE:
    interface ESMF_ConfigGetAttribute
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ConfigGetString
        module procedure ESMF_ConfigGetStrings
        module procedure ESMF_ConfigGetFloatR4
        module procedure ESMF_ConfigGetFloatR8
        module procedure ESMF_ConfigGetFloatsR4
        module procedure ESMF_ConfigGetFloatsR8
        module procedure ESMF_ConfigGetIntI4
        module procedure ESMF_ConfigGetIntI8
        module procedure ESMF_ConfigGetIntsI4
        module procedure ESMF_ConfigGetIntsI8
        module procedure ESMF_ConfigGetLogical
        module procedure ESMF_ConfigGetLogicals

! !DESCRIPTION:
!     This interface provides an entry point for getting
!     items from an {\tt ESMF\_Config} object.
!    
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ConfigSetAttribute - Set an attribute in a Config object
!
! !INTERFACE:
    interface ESMF_ConfigSetAttribute
   
! !PRIVATE MEMBER FUNCTIONS:
!        module procedure ESMF_ConfigSetString
!        module procedure ESMF_ConfigSetFloatR4
!        module procedure ESMF_ConfigSetFloatR8
!        module procedure ESMF_ConfigSetFloatsR4
!        module procedure ESMF_ConfigSetFloatsR8
        module procedure ESMF_ConfigSetIntI4
!        module procedure ESMF_ConfigSetIntI8
!        module procedure ESMF_ConfigSetIntsI4
!        module procedure ESMF_ConfigSetIntsI8
!        module procedure ESMF_ConfigSetLogical
!        module procedure ESMF_ConfigSetLogicals

! !DESCRIPTION:
!     This interface provides an entry point for setting
!     items in an {\tt ESMF\_Config} object.
!    
 
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!! !IROUTINE: ESMF_ConfigEQ - Test Config objects for equivalence
!
! !INTERFACE:
    interface operator(==)
        module procedure ESMF_ConfigEQ
    end interface

    interface operator(/=)
        module procedure ESMF_ConfigNE
    end interface

    public :: operator(==), operator(/=)
!
!------------------------------------------------------------------------------
! PRIVATE PARAMETER  SETTINGS:
!------------------------------------------------------------------------------
! Revised parameter table to fit Fortran 90 standard.

!       integer,   parameter :: LSZ = 256  ! Maximum line size
       integer,   parameter :: LSZ = max (1024,ESMF_MAXPATHLEN)  ! Maximum line size
                                          ! should be at least long enough
                                          ! to read in a file name with full
                                          ! path prepended.
       integer,   parameter :: MSZ = 256  ! Used to size buffer; this is
                                          ! usually *less* than the number
                                          ! of non-blank/comment lines
                                          ! (because most lines are shorter
                                          ! then LSZ)
 
       integer,   parameter :: NBUF_MAX = MSZ*LSZ ! max size of buffer
       integer,   parameter :: NATT_MAX = NBUF_MAX/64 ! max # attributes;  
                                                  ! assumes an average line
                                                  ! size of 16, the code
                                                  ! will do a bound check

       character, parameter :: BLK = achar(32)   ! blank (space)
       character, parameter :: TAB = achar(09)   ! TAB
#ifdef ESMF_HAS_ACHAR_BUG
       character, parameter :: EOL = achar(12)   ! end of line mark (cr)
#else
       character, parameter :: EOL = achar(10)   ! end of line mark (newline)
#endif
       character, parameter :: EOB = achar(00)   ! end of buffer mark (null)
       character, parameter :: NUL = achar(00)   ! what it says
       
!------------------------------------------------------------------------------
       type ESMF_ConfigAttrUsed
#ifndef ESMF_NO_SEQUENCE
          sequence
#endif
          private              
          character, pointer      :: label(:) => null () ! attribute label
          logical                 :: used  = .false.  ! attribute used (retrieved) or not
          ESMF_INIT_DECLARE
       end type ESMF_ConfigAttrUsed

       type ESMF_ConfigClass
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
          sequence
#endif
#endif
          !private              
          character(len=NBUF_MAX),pointer :: buffer => null ()    ! hold the whole file
          character(len=LSZ),     pointer :: this_line => null () ! the current line
          integer :: nbuf                              ! actual size of buffer 
          integer :: next_line                         ! index_ for next line 
                                                       !   on buffer
          integer :: value_begin                       ! index of beginning of
                                                       !   value
          type(ESMF_ConfigAttrUsed), dimension(:), &
                                  pointer :: attr_used => null () ! used attributes table
          integer :: nattr                             ! number of attributes
                                                       !   in the "used" table
          character(len=LSZ)          :: current_attr  ! the current attr label
          ESMF_INIT_DECLARE
       end type ESMF_ConfigClass

!      ! Config wrapper
       type ESMF_Config
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
          sequence
#endif
#endif
          !private       
          type (ESMF_ConfigClass), pointer :: cptr => null ()
          ESMF_INIT_DECLARE
       end type ESMF_Config

     contains


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ConfigAssignment(=) - Config assignment
!
! !INTERFACE:
!   interface assignment(=)
!   config1 = config2
!
! !ARGUMENTS:
!   type(ESMF_Config) :: config1
!   type(ESMF_Config) :: config2
!
!
! !DESCRIPTION:
!   Assign {\tt config1} as an alias to the same {\tt ESMF\_Config} object in memory
!   as {\tt config2}. If {\tt config2} is invalid, then {\tt config1} will be
!   equally invalid after the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[config1]
!     The {\tt ESMF\_Config} object on the left hand side of the assignment.
!   \item[config2]
!     The {\tt ESMF\_Config} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------

! IMPLEMENTATION NOTE:
! Use the default Fortran assignment

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ConfigOperator(==) - Test if Config objects are equivalent
!
! !INTERFACE:
!     interface operator(==)
!     if (config1 == config2) then ... endif
!                  OR
!     result = (config1 == config2)
!
! !RETURN VALUE:
!     configical :: result
!
! !ARGUMENTS:
!     type(ESMF_Config), intent(in) :: config1
!     type(ESMF_Config), intent(in) :: config2
!
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Config} class.
!     Compare two configs for equality; return {\tt .true.} if equal,
!     {\tt .false.} otherwise. Comparison is based on whether the objects
!     are distinct, as with two newly created objects, or are simply aliases
!     to the same object as would be the case when assignment was involved.
!
!     The arguments are:
!     \begin{description}
!     \item[config1]
!          The {\tt ESMF\_Config} object on the left hand side of the equality
!          operation.
!     \item[config2]
!          The {\tt ESMF\_Config} object on the right hand side of the equality
!          operation.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ConfigOperator(/=) - Test if Config objects are not equivalent
!
! !INTERFACE:
!     interface operator(/=)
!     if (config1 /= config2) then ... endif
!                  OR
!     result = (config1 /= config2)
!
! !RETURN VALUE:
!     configical :: result
!
! !ARGUMENTS:
!     type(ESMF_Config), intent(in) :: config1
!     type(ESMF_Config), intent(in) :: config2
!
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Config} class.
!     Compare two configs for equality; return {\tt .true.} if not equivalent,
!     {\tt .false.} otherwise. Comparison is based on whether the Config objects
!     are distinct, as with two newly created objects, or are simply aliases
!     to the same object as would be the case when assignment was involved.
!
!     The arguments are:
!     \begin{description}
!     \item[config1]
!          The {\tt ESMF\_Config} object on the left hand side of the equality
!          operation.
!     \item[config2]
!          The {\tt ESMF\_Config} object on the right hand side of the equality
!          operation.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigAttrUsedGetInit"
!BOPI
! !IROUTINE:  ESMF_ConfigAttrUsedGetInit - Get initialization status of a Config object

! !INTERFACE:
    function ESMF_ConfigAttrUsedGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_ConfigAttrUsed), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_ConfigAttrUsedGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt ConfigAttrUsed}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_ConfigAttrUsed} from which to retrieve status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_ConfigAttrUsedGetInit = ESMF_INIT_GET(s)
       else
         ESMF_ConfigAttrUsedGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_ConfigAttrUsedGetInit


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigAttrUsedInit"
!BOPI
! !IROUTINE:  ESMF_ConfigAttrUsedInit - Initialize ESMF_ConfigAttrUsed

! !INTERFACE:
    subroutine ESMF_ConfigAttrUsedInit(s)
!
! !ARGUMENTS:
       type(ESMF_ConfigAttrUsed) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt configAttrUsed}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_ConfigAttrUsed} of which being initialized.
!     \end{description}
!
!EOPI

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_ConfigAttrUsedInit


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigAttrUsedValidate"
!BOPI
! !IROUTINE:  ESMF_ConfigAttrUsedValidate - Check validity of a ConfigAttrUsed

! !INTERFACE:
    subroutine ESMF_ConfigAttrUsedValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_ConfigAttrUsed), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt ConfigAttrUsed} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_ConfigAttrUsed} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ESMF_INIT_CHECK_SET_SHALLOW(ESMF_ConfigAttrUsedGetInit, ESMF_ConfigAttrUsedInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_ConfigAttrUsedValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigClassGetInit"
!BOPI
! !IROUTINE:  ESMF_ConfigClassGetInit - Get initialization status of a Config object

! !INTERFACE:
    function ESMF_ConfigClassGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_ConfigClass), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_ConfigClassGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt configclass}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_ConfigClass} from which to retrieve status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_ConfigClassGetInit = ESMF_INIT_GET(s)
       else
         ESMF_ConfigClassGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_ConfigClassGetInit


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigClassInit"
!BOPI
! !IROUTINE:  ESMF_ConfigClassInit - Initialize ESMF_ConfigClass

! !INTERFACE:
    subroutine ESMF_ConfigClassInit(s)
!
! !ARGUMENTS:
       type(ESMF_ConfigClass) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt configclass}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_ConfigClass} of which being initialized.
!     \end{description}
!
!EOPI
       nullify(s%buffer)
       nullify(s%this_line)
       nullify(s%attr_used)

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_ConfigClassInit


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigClassValidate"
!BOPI
! !IROUTINE:  ESMF_ConfigClassValidate - Check validity of a ConfigClass

! !INTERFACE:
    subroutine ESMF_ConfigClassValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_ConfigClass), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt Configclass} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_ConfigClass} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ESMF_INIT_CHECK_SET_SHALLOW(ESMF_ConfigClassGetInit, ESMF_ConfigClassInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_ConfigClassValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetInit"
!BOPI
! !IROUTINE:  ESMF_ConfigGetInit - Get initialization status of a Config object

! !INTERFACE:
    function ESMF_ConfigGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_Config), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_ConfigGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Config} from which to retrieve status.
!     \end{description}
!
!EOPI

       if (present(d)) then
         ESMF_ConfigGetInit = ESMF_INIT_GET(d)
       else
         ESMF_ConfigGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_ConfigGetInit



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigCreateEmpty"
!BOP
!
! !IROUTINE: ESMF_ConfigCreate - Instantiate a Config object
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigCreate()
      type(ESMF_Config) function ESMF_ConfigCreateEmpty(keywordEnforcer, rc)

! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
     integer,intent(out), optional              :: rc 
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION: 
!   Instantiates an {\tt ESMF\_Config} object for use in subsequent calls.
!
!   The arguments are:
!   \begin{description}
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      integer :: memstat
      type(ESMF_ConfigClass), pointer :: config_local
      type(ESMF_ConfigAttrUsed), dimension(:), pointer :: attr_used_local

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
! Initialization
      allocate(config_local, stat=memstat)
      ESMF_ConfigCreateEmpty%cptr => config_local
      if (ESMF_LogFoundAllocError(memstat, msg="Allocating config class", &
                                        ESMF_CONTEXT, rcToReturn=rc)) return

      allocate(config_local%buffer, config_local%this_line, stat = memstat)
      if (ESMF_LogFoundAllocError(memstat, msg="Allocating local buffer 1", &
                                        ESMF_CONTEXT, rcToReturn=rc)) return

      ! TODO: Absoft 8 compiler bug necessitates allocating pointer within
      ! derived type via local pointer first.  Absoft 9/Jazz bug necessitates
      ! this must be a separate allocate statement.
      allocate(attr_used_local(NATT_MAX), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat, msg="Allocating local buffer 2", &
                                        ESMF_CONTEXT, rcToReturn=rc)) return

      config_local%nbuf = 0
      config_local%next_line = 0

      config_local%attr_used => attr_used_local

      if (present( rc ))  rc = ESMF_SUCCESS

      ESMF_INIT_SET_CREATED(ESMF_ConfigCreateEmpty)
      return

    end function ESMF_ConfigCreateEmpty

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigCreateFromSection"
!BOP
!
! !IROUTINE: ESMF_ConfigCreate - Instantiate a new Config object from a Config section
!
! !INTERFACE:
    ! Private name; call using ESMF_ConfigCreate()
    type(ESMF_Config) function ESMF_ConfigCreateFromSection(config, &
      openlabel, closelabel, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Config)             :: config
      character(len=*),  intent(in) :: openlabel, closelabel
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,intent(out), optional :: rc
!
!
! !DESCRIPTION:
!   Instantiates an {\tt ESMF\_Config} object from a section of an existing
!   {\tt ESMF\_Config} object delimited by {\tt openlabel} and {\tt closelabel}.
!   An error is returned if neither of the input labels is found in input
!   {\tt config}.
!
!   Note that a section is intended as the content of a given {\tt ESMF\_Config}
!   object delimited by two distinct labels. Such content, as well as each of the
!   surrounding labels, are still within the scope of the parent {\tt ESMF\_Config}
!   object. Therefore, including in a section labels used outside that
!   section should be done carefully to prevent parsing conflicts.
!
!   The arguments are:
!   \begin{description}
!     \item[config]
!       The input {\tt ESMF\_Config} object.
!     \item[openlabel]
!       Label marking the beginning of a section in {\tt config}.
!     \item[closelabel]
!       Label marking the end of a section in {\tt config}.
!     \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if a section is found
!      and a new {\tt ESMF\_Config} object returned.
!   \end{description}
!
!EOP -------------------------------------------------------------------

      integer :: localrc
      integer :: ptr, section_open, section_close
      logical, parameter :: unique = .false.

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Look for opening section label in whole input Config object
      call ESMF_ConfigFindLabel(config, openlabel, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, &
        msg="Opening section label not found",  &
        ESMF_CONTEXT, rcToReturn=rc)) return

      section_open = config % cptr % value_begin

      ! Look closing section label after opening label
      call ESMF_ConfigFindNextLabel(config, closelabel, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, &
        msg="Closing section label not found",  &
        ESMF_CONTEXT, rcToReturn=rc)) return

      section_close = config % cptr % value_begin - len(closelabel) - 1

      if (section_close < section_open) then
        call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
          msg="Closing section label precedes opening section label", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      end if

      ! Create and populate new Config object from parent object's section
      ESMF_ConfigCreateFromSection = ESMF_ConfigCreateEmpty(rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, &
        msg="Instantiating new config object",  &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ptr = section_close - section_open + 1
      if (config % cptr % buffer(section_open:section_open) == EOL) then
        ESMF_ConfigCreateFromSection % cptr % buffer(1:ptr) = &
          config % cptr % buffer(section_open:section_close)
      else
        ptr = ptr + 1
        ESMF_ConfigCreateFromSection % cptr % buffer(1:1) = EOL
        ESMF_ConfigCreateFromSection % cptr % buffer(2:ptr) = &
          config % cptr % buffer(section_open:section_close)
      end if

      ptr = ptr + 1
      ESMF_ConfigCreateFromSection % cptr % nbuf = ptr
      ESMF_ConfigCreateFromSection % cptr % buffer(ptr:ptr) = EOB

      ESMF_ConfigCreateFromSection % cptr % this_line = ' '
      ESMF_ConfigCreateFromSection % cptr % next_line = 1
      ESMF_ConfigCreateFromSection % cptr % value_begin = 1

      call ESMF_ConfigParseAttributes(ESMF_ConfigCreateFromSection, &
                                      unique=unique, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
                             ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_ConfigCreateFromSection

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigDestroy"
!BOP
!
! !IROUTINE: ESMF_ConfigDestroy - Destroy a Config object
!
! !INTERFACE:
    subroutine ESMF_ConfigDestroy(config, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out),  optional :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION: 
!    Destroys the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      integer :: i
      integer :: memstat

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      memstat = 0

      ! TODO: Absoft 9/Jazz bug necessitates this separate deallocate statement
      ! before the other (must be in reverse order of allocation)
      do, i=1, size (config%cptr%attr_used)
        if (associated (config%cptr%attr_used(i)%label)) then
          deallocate(config%cptr%attr_used(i)%label, stat=memstat)
          if (ESMF_LogFoundDeallocError(memstat, msg="Deallocating local buffer 3", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
        end if
      end do
      deallocate(config%cptr%attr_used, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat, msg="Deallocating local buffer 2", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      deallocate(config%cptr%buffer, config%cptr%this_line, stat = memstat)
      if (ESMF_LogFoundDeallocError(memstat, msg="Deallocating local buffer 1", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      deallocate(config%cptr, stat = memstat)
      if (ESMF_LogFoundDeallocError(memstat, msg="Deallocating config type", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      nullify(config%cptr)

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

      ESMF_INIT_SET_DELETED(config)

     end subroutine ESMF_ConfigDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigFindLabel"
!BOP
!
! !IROUTINE: ESMF_ConfigFindLabel - Find a label in a Config object
!
! !INTERFACE:
    subroutine ESMF_ConfigFindLabel(config, label, keywordEnforcer, isPresent, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)           :: config 
      character(len=*),  intent(in)              :: label
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      logical,           intent(out),  optional  :: isPresent
      integer,           intent(out),  optional  :: rc 

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added the {\tt isPresent} argument.  Allows detection of
!  end-of-line condition to be separate from the {\tt rc}.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION: Finds the {\tt label} (key) string in the {\tt config} object
!   starting from the beginning of its content.
!
!   Since the search is done by looking for a string, possibly multi-worded,
!   in the whole {\tt Config} object, it is important to use special 
!   conventions to distinguish {\tt labels} from other words. This is done 
!   in the Resource File by using the NASA/DAO convention to finish
!   line labels with a colon (:) and table labels with a double colon (::).
!
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [label]
!     Identifying label. 
!   \item [{[isPresent]}]
!     Set to {\tt .true.} if the item is found.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     If the label is not found, and the {\tt isPresent} argument is
!     not present, an error is returned.
!   \end{description}
!
!EOP -------------------------------------------------------------------

      integer :: i, j

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if (present (isPresent)) then
        isPresent = .false.
      end if

!     Determine whether label exists
!     ------------------------------    

      i = index_ ( config%cptr%buffer(1:config%cptr%nbuf), EOL//label ) + 1
      if ( i .eq. 1 ) then
         config%cptr%this_line = BLK // EOL
         if (present (isPresent)) then
           if (present (rc)) rc = ESMF_SUCCESS
           return
         end if
         if (ESMF_LogFoundError(ESMF_RC_NOT_FOUND, &
                                msg="label " // trim (label) // " not found", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
      elseif(i.le.0) then
         if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                                msg="invalid operation with index_", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
      end if

      if (present (isPresent)) then
        isPresent = .true.
      end if

!     Save current attribute label without colon,
!       to associate with subsequent GetAttribute() or GetChar()
!     -------------------------------------------
       config%cptr%current_attr = label(1:(index_(label, ":") - 1))


!     Extract the line associated with this label
!     -------------------------------------------
      i = i + len ( label )
      j = i + index_(config%cptr%buffer(i:config%cptr%nbuf),EOL) - 2
      config%cptr%this_line = config%cptr%buffer(i:j) // BLK // EOL
      
      config%cptr%next_line = j + 2
      
      config%cptr%value_begin = i

      if ( present (rc )) rc = ESMF_SUCCESS
      
    end subroutine ESMF_ConfigFindLabel

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigFindNextLabel"
!BOP
!
! !IROUTINE: ESMF_ConfigFindNextLabel - Find a label in Config object starting from current position
!
! !INTERFACE:
    subroutine ESMF_ConfigFindNextLabel(config, label, keywordEnforcer, isPresent, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)           :: config
      character(len=*),  intent(in)              :: label
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      logical,           intent(out),  optional  :: isPresent
      integer,           intent(out),  optional  :: rc

!
! !DESCRIPTION: Finds the {\tt label} (key) string in the {\tt config} object,
!   starting from the current position pointer.
!
!   This method is equivalent to {\tt ESMF\_ConfigFindLabel}, but the search
!   is performed starting from the current position pointer.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [label]
!     Identifying label.
!   \item [{[isPresent]}]
!     Set to {\tt .true.} if the item is found.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     If the label is not found, and the {\tt isPresent} argument is
!     not present, an error is returned.
!   \end{description}
!
!EOP -------------------------------------------------------------------

      integer   :: i, j, ptr

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if (present (isPresent)) then
        isPresent = .false.
      end if

!     Determine whether label exists from current position onward
!     -----------------------------------------------------------
      ptr = max(config%cptr%next_line-1, 1)
      i = index_ ( config%cptr%buffer(ptr:config%cptr%nbuf ), EOL//label) + 1

      if ( i .eq. 1 ) then
         config%cptr%this_line = BLK // EOL
         if (present (isPresent)) then
           if (present (rc)) rc = ESMF_SUCCESS
           return
         end if
         if (ESMF_LogFoundError(ESMF_RC_NOT_FOUND, &
                                msg="label " // trim (label) // " not found", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
      elseif(i.le.0) then
         if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                                msg="invalid operation with index_", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
      end if

      if (present (isPresent)) then
        isPresent = .true.
      end if

!     Save current attribute label without colon,
!       to associate with subsequent GetAttribute() or GetChar()
!     -------------------------------------------
      config%cptr%current_attr = label(1:(index_(label, ":") - 1))

!     Extract the line associated with this label
!     -------------------------------------------
      i = i + len ( label ) + ptr - 1
      j = i + index_ ( config%cptr%buffer(i:config%cptr%nbuf),EOL ) - 2
      config%cptr%this_line = config%cptr%buffer(ptr:j) // BLK // EOL

      config%cptr%next_line = j + 2

      config%cptr%value_begin = i

      if ( present (rc )) rc = ESMF_SUCCESS

    end subroutine ESMF_ConfigFindNextLabel

!BOP
! !IROUTINE: ESMF_ConfigGetAttribute - Get an attribute value from Config object
!
!
! !INTERFACE:
!      subroutine ESMF_ConfigGetAttribute(config, <value>, &
!        keywordEnforcer, label, default, rc)
!
! !ARGUMENTS:
!      type(ESMF_Config), intent(inout)         :: config     
!      <value argument>, see below for supported values
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!      character(len=*),  intent(in),  optional :: label 
!      character(len=*),  intent(in),  optional :: default 
!      integer,           intent(out), optional :: rc     
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION: 
!      Gets a value from the {\tt config} object.  When the
!      value is a sequence of characters
!      it will be terminated by the first white space.
!      
!      Supported values for <value argument> are:
!      \begin{description}
!      \item character(len=*), intent(out)          :: value
!      \item real(ESMF\_KIND\_R4), intent(out)      :: value    
!      \item real(ESMF\_KIND\_R8), intent(out)      :: value
!      \item integer(ESMF\_KIND\_I4), intent(out)   :: value
!      \item integer(ESMF\_KIND\_I8), intent(out)   :: value
!      \item logical, intent(out)                   :: value
!      \end{description}
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [<value argument>]
!     Returned value.
!   \item [{[label]}]
!     Identifing label. 
!   \item [{[default]}]
!     Default value if {\tt label} is not found in {\tt config} object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOP -------------------------------------------------------------------

!BOP -------------------------------------------------------------------
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of attribute values from Config object
!
! !INTERFACE:
!      subroutine ESMF_ConfigGetAttribute(config, <value list argument>, &
!        keywordEnforcer, count, label, default, rc)
!
! !ARGUMENTS:
!      type(ESMF_Config), intent(inout)         :: config     
!      <value list argument>, see below for values      
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!      integer,           intent(in)   optional :: count
!      character(len=*),  intent(in),  optional :: label 
!      character(len=*),  intent(in),  optional :: default 
!      integer,           intent(out), optional :: rc     
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!      Gets a list of values from the {\tt config} object.  
!
!      Supported values for <value list argument> are:
!      \begin{description}
!      \item character(len=*), intent(out)            :: valueList(:)
!      \item real(ESMF\_KIND\_R4), intent(inout)      :: valueList(:)
!      \item real(ESMF\_KIND\_R8), intent(inout)      :: valueList(:)
!      \item integer(ESMF\_KIND\_I4), intent(inout)   :: valueList(:)
!      \item integer(ESMF\_KIND\_I8), intent(inout)   :: valueList(:)
!      \item logical, intent(inout)                   :: valueList(:)
!      \end{description}
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [<value list argument>]
!     Returned value.
!   \item [count]
!     Number of returned values expected.  
!   \item [{[label]}]
!     Identifing label. 
!   \item [{[default]}]
!     Default value if {\tt label} is not found in {\tt config} object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOP -------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetString"
!BOPI
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a character string
!
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetString(config, value, &
        keywordEnforcer, label, default, eolFlag, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config     
      character(len=*), intent(out)          :: value
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*), intent(in), optional :: label 
      character(len=*), intent(in), optional :: default 
      logical, intent(out), optional         :: eolFlag
      integer, intent(out), optional         :: rc     
!
! !DESCRIPTION: Gets a sequence of characters. It will be 
!               terminated by the first white space.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned value. 
!   \item [{[label]}]
!     Identifing label. 
!   \item [{[default]}]
!     Default value if {\tt label} is not found in {\tt config} object. 
!   \item [{[eolFlag]}]
!     Returns {\tt .true.} when end of line is encountered.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}

!EOPI ------------------------------------------------------------------
      character(len=1) :: ch
      integer :: ib, ie, localrc
      logical :: found

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_SUCCESS
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)


! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = BLK
      endif

      if (present (eolFlag)) then
        eolFlag = .false.
      end if

      if (present (default)) then
        if (len (value) < len (default)) then
          if (ESMF_LogFoundError (ESMF_RC_ARG_BAD,  &
            msg='default length too long for value string',  &
            ESMF_CONTEXT, rcToReturn=rc)) return
        end if
      end if

! Processing
      if(present( label )) then
         call ESMF_ConfigFindLabel( config, label=label,  &
             isPresent=found, rc=localrc)
         if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
             ESMF_CONTEXT, rcToReturn=rc)) return

         if (.not. found) then
            if (present(default)) then
               localrc = ESMF_SUCCESS
            else
               localrc = ESMF_RC_NOT_FOUND
            end if
            if ( present (rc )) then
              rc = localrc
            endif
            return
         endif
      endif

      call ESMF_Config_trim ( config%cptr%this_line )
      
      ch = config%cptr%this_line(1:1)
      if ( ch .eq. '"' .or. ch .eq. "'" ) then
         ib = 2
         ie = index_ ( config%cptr%this_line(ib:), ch ) 
      else
         ib = 1
         ie = min(index_(config%cptr%this_line,BLK), &
              index_(config%cptr%this_line,EOL)) - 1
      end if
      
      if ( ie .lt. ib ) then
         value = BLK
         if ( present ( default )) then
           value = default
         endif
         if (present (eolFlag)) then
           eolFlag = .true.
           localrc = ESMF_SUCCESS
         else
           localrc = ESMF_RC_NOT_FOUND
         end if
         if ( present (rc )) then
           rc = localrc
         endif
         return
      else
         ! Get the string, and shift the rest of %this_line to
         ! the left
         value = config%cptr%this_line(ib:ie) 
         config%cptr%this_line = config%cptr%this_line(ie+2:)
         if (len (value) >= ie-ib+1) then
           localrc = ESMF_SUCCESS
         else
           localrc = ESMF_RC_ARG_SIZE
         end if
      end if

      if ( present (rc)) then
        rc = localrc
      endif
      
    end subroutine ESMF_ConfigGetString

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetStrings"
!BOPI
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of strings

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetStrings(config, valueList, &
        keywordEnforcer, count, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config),  intent(inout)         :: config    
      character(len=*),   intent(out)           :: valueList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,            intent(in),  optional :: count 
      character(len=*),   intent(in),  optional :: label 
      character(len=*),   intent(in),  optional :: default
      integer,            intent(out), optional :: rc    
!
! !DESCRIPTION: 
!  Gets a string {\tt valueList} of a given {\tt count} from
!  the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [valueList]
!     Returned string values. 
!   \item [count]
!     Number of returned values expected. 
!   \item [{[label]}]
!     Identifing label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI -------------------------------------------------------------------
!
      integer :: localrc
      integer :: localcount
      integer :: i 
      logical :: found

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_RC_NOT_IMPL
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      localcount = size (valueList)
      if (present (count)) then
        if (count <= 0) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else if (count > size (valueList)) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else
           localcount = count
        end if
      endif

      if (present (default)) then
        if (len (valueList) < len (default)) then
          if (ESMF_LogFoundError (ESMF_RC_ARG_BAD,  &
            msg='default length too long for valueList array',  &
            ESMF_CONTEXT, rcToReturn=rc)) return
        end if
      end if

! Default setting
      if( present( default ) ) then 
         valueList(1:localcount) = default

      else
         valueList(1:localcount) = ""
      endif
! Processing
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label=label,  &
             isPresent=found, rc=localrc)
         if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
             ESMF_CONTEXT, rcToReturn=rc)) return
         if (.not. found)  &
             localrc = ESMF_RC_NOT_FOUND
      end if

      do i = 1, localcount
         
         if(present( default )) then
            call ESMF_ConfigGetString( config, valueList(i), default=default, rc=localrc )
         else
            call ESMF_ConfigGetString( config, valueList(i), rc = localrc)
         endif
      enddo

      if(present( rc )) then
        rc = localrc
      endif

    end subroutine ESMF_ConfigGetStrings

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatR4"
!BOPI
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a 4-byte real number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatR4(config, value, &
        keywordEnforcer, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)      :: config    
      real(ESMF_KIND_R4), intent(out)          :: value    
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*), intent(in), optional   :: label
      real(ESMF_KIND_R4), intent(in), optional :: default 
      integer, intent(out), optional           :: rc     
!
! !DESCRIPTION: 
!   Gets a 4-byte real {\tt value} from the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned value. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if {\tt label} is not found in {\tt config} object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!

!EOPI -------------------------------------------------------------------
!
      integer :: localrc
      integer :: iostat
      character(len=LSZ) :: string
      real(ESMF_KIND_R4) :: x
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_RC_NOT_IMPL
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = 0.0
      endif

! Processing
      if (present (label ) ) then
         call ESMF_ConfigGetString( config, string, label=label, rc=localrc)
      else
         call ESMF_ConfigGetString( config, string, rc = localrc )
      endif

      if ( localrc == ESMF_SUCCESS ) then
           read(string,*,iostat=iostat) x
           if (iostat == 0) then
             call ESMF_ConfigSetCurrentAttrUsed(config, used=.true.)
           else
             ! undo what GetSring() did
             call ESMF_ConfigSetCurrentAttrUsed(config, used=.false.)
             localrc = ESMF_RC_VAL_OUTOFRANGE
           endif
      else
         if( present( default )) then
            x = default
            localrc = ESMF_SUCCESS
         endif
      end if

      if ( localrc == ESMF_SUCCESS ) then
         value = x
      endif

      if( present( rc )) then
        rc = localrc
      endif

    end subroutine ESMF_ConfigGetFloatR4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatR8"
!BOPI
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get an 8-byte real number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatR8(config, value, &
        keywordEnforcer, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)      :: config    
      real(ESMF_KIND_R8), intent(out)          :: value 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*), intent(in), optional   :: label
      real(ESMF_KIND_R8), intent(in), optional :: default 
      integer, intent(out), optional           :: rc     
!
! !DESCRIPTION: 
!   Gets an 8-byte real {\tt value} from the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned real value. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if {\tt label} is not found in {\tt config} object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!

!EOPI -------------------------------------------------------------------
!
      integer :: localrc
      integer :: iostat
      character(len=LSZ) :: string
      real(ESMF_KIND_R8) :: x
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_RC_NOT_IMPL
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = 0.0
      endif

! Processing
      if (present (label ) ) then
         call ESMF_ConfigGetString( config, string, label=label, rc=localrc)
      else
         call ESMF_ConfigGetString( config, string, rc = localrc )
      endif

      if ( localrc == ESMF_SUCCESS ) then
           read(string,*,iostat=iostat) x
           if (iostat == 0) then
             call ESMF_ConfigSetCurrentAttrUsed(config, used=.true.)
           else
             ! undo what GetSring() did
             call ESMF_ConfigSetCurrentAttrUsed(config, used=.false.)
             localrc = ESMF_RC_VAL_OUTOFRANGE
           endif
      else
         if( present( default )) then
            x = default
            localrc = ESMF_SUCCESS
         endif
      end if

      if ( localrc == ESMF_SUCCESS ) then
         value = x
      endif

      if( present( rc )) then
        rc = localrc
      endif

    end subroutine ESMF_ConfigGetFloatR8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatsR4"
!BOPI
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 4-byte real numbers

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatsR4(config, valueList, &
        keywordEnforcer, count, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config),  intent(inout)         :: config    
      real(ESMF_KIND_R4), intent(inout)         :: valueList(:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,            intent(in),  optional :: count 
      character(len=*),   intent(in),  optional :: label 
      real(ESMF_KIND_R4), intent(in),  optional :: default
      integer,            intent(out), optional :: rc    
!
! !DESCRIPTION: 
!  Gets a 4-byte real {\tt valueList} of a given {\tt count} from
!  the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [valueList]
!     Returned real values. 
!   \item [count]
!     Number of returned values expected. 
!   \item [{[label]}]
!     Identifing label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI -------------------------------------------------------------------
!
      integer :: localrc
      integer :: localcount
      integer :: i 
      logical :: found

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_RC_NOT_IMPL
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      localcount = size (valueList)
      if (present (count)) then
        if (count <= 0) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else if (count > size (valueList)) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else
           localcount = count
        end if
      endif

! Default setting
      if( present( default ) ) then 
         valueList(1:localcount) = default

      else
         valueList(1:localcount) = 0.0
      endif
! Processing
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label=label,  &
             isPresent=found, rc=localrc)
         if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
             ESMF_CONTEXT, rcToReturn=rc)) return
         if (.not. found)  &
             localrc = ESMF_RC_NOT_FOUND
      end if

      do i = 1, localcount
         
         if(present( default )) then
            call ESMF_ConfigGetFloatR4( config, valueList(i), default=default, rc=localrc )
         else
            call ESMF_ConfigGetFloatR4( config, valueList(i), rc = localrc)
         endif
      enddo

      if(present( rc )) then
        rc = localrc
      endif

    end subroutine ESMF_ConfigGetFloatsR4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatsR8"
!BOPI 
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 8-byte real numbers

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatsR8(config, valueList, &
        keywordEnforcer, count, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config),  intent(inout)         :: config    
      real(ESMF_KIND_R8), intent(inout)         :: valueList(:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,            intent(in),  optional :: count 
      character(len=*),   intent(in),  optional :: label 
      real(ESMF_KIND_R8), intent(in),  optional :: default
      integer,            intent(out), optional :: rc    
!
! !DESCRIPTION: 
!   Gets an 8-byte real {\tt valueList} of a given {\tt count} from the
!   {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [valueList]
!     Returned values. 
!   \item [count]
!     Number of returned values expected. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI -------------------------------------------------------------------
      integer :: localrc
      integer :: localcount
      integer :: i 
      logical :: found
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_RC_NOT_IMPL
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      localcount = size (valueList)
      if (present (count)) then
        if (count <= 0) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else if (count > size (valueList)) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else
           localcount = count
        end if
      endif
       
! Default setting
      if( present( default ) ) then 
         valueList(1:localcount) = default
      else
         valueList(1:localcount) = 0.0
      endif

! Processing
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label=label,  &
             isPresent=found, rc=localrc)
         if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
             ESMF_CONTEXT, rcToReturn=rc)) return
         if (.not. found)  &
             localrc = ESMF_RC_NOT_FOUND
      end if

      do i = 1, localcount
         
         if(present( default )) then
            call ESMF_ConfigGetFloatR8( config, valueList(i), default=default, rc=localrc )
         else
            call ESMF_ConfigGetFloatR8( config, valueList(i), rc = localrc)
         endif
      enddo

      if(present( rc )) then
        rc = localrc
      endif

    end subroutine ESMF_ConfigGetFloatsR8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntI4"
!BOPI
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a 4-byte integer number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntI4(config, value, &
        keywordEnforcer, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config     
      integer(ESMF_KIND_I4), intent(out)           :: value
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*), intent(in), optional       :: label 
      integer(ESMF_KIND_I4), intent(in), optional  :: default
      integer, intent(out), optional               :: rc   

!
! !DESCRIPTION: 
!  Gets an integer {\tt value} from the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned integer value. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI -------------------------------------------------------------------

      integer :: localrc
      character(len=LSZ) :: string
      real(ESMF_KIND_R8) :: x
      integer(ESMF_KIND_I4) ::  n
      integer :: iostat

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_SUCCESS
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = 0
      endif

! Processing
      if (present (label ) ) then
         call ESMF_ConfigGetString( config, string, label=label, rc=localrc)
      else
         call ESMF_ConfigGetString( config, string, rc = localrc )
      endif

      if ( localrc == ESMF_SUCCESS ) then
           read(string,*,iostat=iostat) x
           if ( iostat == 0 ) then
             call ESMF_ConfigSetCurrentAttrUsed(config, used=.true.)
           else
             ! undo what GetSring() did
             call ESMF_ConfigSetCurrentAttrUsed(config, used=.false.)
             localrc = ESMF_RC_VAL_OUTOFRANGE
           endif
      end if
      if ( localrc == ESMF_SUCCESS ) then
         n = nint(x)
      else
         if( present( default )) then
            n = default
            localrc = ESMF_SUCCESS
         else
            n = 0
         endif
      endif

      if ( localrc == ESMF_SUCCESS ) then
         value = n
      endif

      if( present( rc )) then
        rc = localrc
      endif
      
    end subroutine ESMF_ConfigGetIntI4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntI8"
!BOPI
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get an 8-byte integer number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntI8(config, value, &
        keywordEnforcer, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config     
      integer(ESMF_KIND_I8), intent(out)           :: value
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*), intent(in), optional       :: label 
      integer(ESMF_KIND_I8), intent(in), optional  :: default
      integer, intent(out), optional               :: rc   

!
! !DESCRIPTION: 
!  Gets an 8-byte integer {\tt value} from the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned integer value. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI -------------------------------------------------------------------
!
      integer :: localrc
      integer :: iostat
      character(len=LSZ) :: string
      real(ESMF_KIND_R8) :: x
      integer(ESMF_KIND_I8) :: n

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_SUCCESS
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = 0
      endif

! Processing
      if (present (label ) ) then
         call ESMF_ConfigGetString( config, string, label=label, rc=localrc)
      else
         call ESMF_ConfigGetString( config, string, rc = localrc )
      endif

      if ( localrc == ESMF_SUCCESS ) then
           read(string,*,iostat=iostat) x
           if ( iostat == 0 ) then
             call ESMF_ConfigSetCurrentAttrUsed(config, used=.true.)
           else
             ! undo what GetSring() did
             call ESMF_ConfigSetCurrentAttrUsed(config, used=.false.)
             localrc = ESMF_RC_VAL_OUTOFRANGE
           endif
      end if
      if ( localrc == ESMF_SUCCESS ) then
         n = nint(x)
      else
         if( present( default )) then
            n = default
            localrc = ESMF_SUCCESS
         else
            n = 0
         endif
      endif

      if ( localrc == ESMF_SUCCESS ) then
         value = n
      endif

      if( present( rc )) then
        rc = localrc
      endif
      
    end subroutine ESMF_ConfigGetIntI8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntsI4"
!BOPI
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 4-byte integers
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntsI4(config, valueList, &
        keywordEnforcer, count, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config),     intent(inout)         :: config      
      integer(ESMF_KIND_I4), intent(inout)         :: valueList(:)  
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,               intent(in),  optional :: count  
      character(len=*),      intent(in),  optional :: label 
      integer(ESMF_KIND_I4), intent(in),  optional :: default
      integer,               intent(out), optional :: rc    
!
! !DESCRIPTION: 
!  Gets a 4-byte integer {\tt valueList} of given {\tt count} from the 
!  {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [valueList]
!     Returned values. 
!   \item [count]
!     Number of returned values expected. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI -------------------------------------------------------------------
!
      integer :: localrc
      integer :: localcount
      integer :: i 
      logical :: found
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_SUCCESS
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      localcount = size (valueList)
      if (present (count)) then
        if (count <= 0) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else if (count > size (valueList)) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else
           localcount = count
        end if
      endif
       
 ! Default setting
      if( present( default ) ) then 
         valueList(1:localcount) = default
      else
         valueList(1:localcount) = 0
      endif

! Processing 
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label=label,  &
             isPresent=found, rc=localrc)
         if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
             ESMF_CONTEXT, rcToReturn=rc)) return
         if (.not. found)  &
             localrc = ESMF_RC_NOT_FOUND
      end if

      do i = 1, localcount
         
         if(present( default )) then
            call ESMF_ConfigGetIntI4( config, valueList(i), default = default, rc = localrc)
         else
            call ESMF_ConfigGetIntI4( config, valueList(i), rc = localrc)
         endif
      enddo

      if(present( rc )) then
        rc = localrc
      endif

    end subroutine ESMF_ConfigGetIntsI4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntsI8"
!BOPI
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 8-byte integers
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntsI8(config, valueList, &
        keywordEnforcer, count, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config),     intent(inout)         :: config      
      integer(ESMF_KIND_I8), intent(inout)         :: valueList(:)  
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,               intent(in),  optional :: count  
      character(len=*),      intent(in),  optional :: label 
      integer(ESMF_KIND_I8), intent(in),  optional :: default
      integer,               intent(out), optional :: rc    
!
! !DESCRIPTION: 
!  Gets an 8-byte integer {\tt valueList} of given {\tt count} from
!  the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [valueList]
!     Returned values. 
!   \item [count]
!     Number of returned values expected. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI -------------------------------------------------------------------
!
      integer :: localrc
      integer :: localcount
      integer :: i 
      logical :: found
      
      localrc = ESMF_SUCCESS
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      localcount = size (valueList)
      if (present (count)) then
        if (count <= 0) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else if (count > size (valueList)) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else
           localcount = count
        end if
      endif
       
 ! Default setting
      if( present( default ) ) then 
         valueList(1:localcount) = default
      else
         valueList(1:localcount) = 0
      endif

! Processing 
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label=label,  &
             isPresent=found, rc=localrc)
         if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
             ESMF_CONTEXT, rcToReturn=rc)) return
         if (.not. found)  &
             localrc = ESMF_RC_NOT_FOUND
      end if

      do i = 1, localcount
         
         if(present( default )) then
            call ESMF_ConfigGetIntI8( config, valueList(i), default = default, rc = localrc)
         else
            call ESMF_ConfigGetIntI8( config, valueList(i), rc = localrc)
         endif
      enddo

      if(present( rc )) then
        rc = localrc
      endif

      return
    end subroutine ESMF_ConfigGetIntsI8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetLogical"
!BOPI
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a logical value

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetLogical(config, value, &
        keywordEnforcer, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)         :: config     
      logical,           intent(out)           :: value
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*),  intent(in),  optional :: label 
      logical,           intent(in),  optional :: default
      integer,           intent(out), optional :: rc   

!
! !DESCRIPTION: 
!  Gets a logical {\tt value} from the {\tt config} object.
!
!  Recognizes any upper/lowercase composition of the following keywords as
!  logical true/false values:
!  
!  true  t .true.  .t. yes y on  \\
!  false f .false. .f. no  n off \\
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned logical value. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!     If not specified, the default value is .false.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI -------------------------------------------------------------------
      character(len=LSZ) :: string
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_RC_NOT_IMPL
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      ! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = .false.
      endif

      ! Processing
      if (present (label ) ) then
         call ESMF_ConfigGetString( config, string, label=label, rc=localrc)
      else
         call ESMF_ConfigGetString( config, string, rc = localrc )
      endif

      if ( localrc == ESMF_SUCCESS ) then

        ! Convert string to lower case
         string = ESMF_UtilStringLowerCase(string)

         ! Check if valid true/false keyword
         if (string == 't'      .or. string == 'true' .or. &
             string == '.true.' .or. string == '.t.'  .or. &
             string == 'y'      .or. string == 'yes'  .or. &
             string == 'on') then
           value = .true.
           call ESMF_ConfigSetCurrentAttrUsed(config, used=.true.)
         else
            if (string == 'f'       .or. string == 'false' .or. &
                string == '.false.' .or. string == '.f.'   .or. &
                string == 'n'       .or. string == 'no'    .or. &
                string == 'off') then
              value = .false.
              call ESMF_ConfigSetCurrentAttrUsed(config, used=.true.)
            else
              ! undo what GetSring() did
              call ESMF_ConfigSetCurrentAttrUsed(config, used=.false.)

              if (ESMF_LogFoundError(ESMF_RC_CANNOT_GET, &
                                msg="bad boolean value '" // string // &
                                  "' in configuration file.", &
                                ESMF_CONTEXT, rcToReturn=rc)) return
            endif
         endif
      else
         if( present( default )) then
            localrc = ESMF_SUCCESS
         endif
      end if

      if( present( rc )) then
        rc = localrc
      endif
      
    end subroutine ESMF_ConfigGetLogical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetLogicals"
!BOPI
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of logical values
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetLogicals(config, valueList, &
        keywordEnforcer, count, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)         :: config      
      logical,           intent(inout)         :: valueList(:)  
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(in),  optional :: count  
      character(len=*),  intent(in),  optional :: label 
      logical,           intent(in),  optional :: default
      integer,           intent(out), optional :: rc    
!
! !DESCRIPTION: 
!  Gets a logical {\tt valueList} of given {\tt count} from the 
!  {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [valueList]
!     Returned values. 
!   \item [count]
!     Number of returned values expected. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI -------------------------------------------------------------------
!
      integer :: localrc
      integer :: localcount
      integer :: i 
      logical :: found
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_SUCCESS
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      localcount = size (valueList)
      if (present (count)) then
        if (count <= 0) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else if (count > size (valueList)) then
           if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                  msg="invalid SIZE", &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
        else
           localcount = count
        end if
      endif
       
      ! Default setting
      if( present( default ) ) then 
         valueList(1:localcount) = default
      else
         valueList(1:localcount) = .false.
      endif

      ! Processing 
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label=label,  &
             isPresent=found, rc=localrc)
         if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
             ESMF_CONTEXT, rcToReturn=rc)) return
         if (.not. found)  &
             localrc = ESMF_RC_NOT_FOUND
      end if

      do i = 1, localcount
         
         if(present( default )) then
            call ESMF_ConfigGetLogical( config, valueList(i), &
                                        default = default, rc = localrc)
         else
            call ESMF_ConfigGetLogical( config, valueList(i), rc = localrc)
         endif
      enddo

      if(present( rc )) then
        rc = localrc
      endif

    end subroutine ESMF_ConfigGetLogicals

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetChar"
!BOP
!
! !IROUTINE: ESMF_ConfigGetChar - Get a character attribute value from Config object
!
! !INTERFACE:
      subroutine ESMF_ConfigGetChar(config, value, &
        keywordEnforcer, label, default, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)         :: config 
      character,         intent(out)           :: value
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*),  intent(in),  optional :: label   
      character,         intent(in),  optional :: default
      integer,           intent(out), optional :: rc    
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION: 
!  Gets a character {\tt value} from the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Returned value. 
!   \item [{[label]}]
!     Identifying label. 
!   \item [{[default]}]
!     Default value if label is not found in configuration object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOP -------------------------------------------------------------------
      character(len=LSZ) :: string
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_SUCCESS
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = BLK
      endif

! Processing
      if (present (label ) ) then
         call ESMF_ConfigGetString( config, string, label=label, rc=localrc)
      else
         call ESMF_ConfigGetString( config, string, rc = localrc )
      endif

      if ( localrc == ESMF_SUCCESS ) then
         value = string(1:1)
         call ESMF_ConfigSetCurrentAttrUsed(config, used=.true.)
      else
         if( present( default )) then
            localrc = ESMF_SUCCESS
         endif
      end if

      if (present( rc )) then
        rc = localrc
      endif

    end subroutine ESMF_ConfigGetChar

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetDim"
!BOP
!
! !IROUTINE: ESMF_ConfigGetDim - Get table sizes from Config object
!
! !INTERFACE:
    subroutine ESMF_ConfigGetDim(config, lineCount, columnCount, &
      keywordEnforcer, label, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)         :: config
      integer,           intent(out)           :: lineCount
      integer,           intent(out)           :: columnCount
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*),  intent(in),  optional :: label
      integer,           intent(out), optional :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION: 
!  Returns the number of lines in the table in {\tt lineCount} and 
!  the maximum number of words in a table line in {\tt columnCount}.
!
!  After the call, the line pointer is positioned to the end of the table.
!  To reset it to the beginning of the table, use {\tt ESMF\_ConfigFindLabel}. 
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [lineCount]
!     Returned number of lines in the table. 
!   \item [columnCount]
!     Returned maximum number of words in a table line. 
!   \item [{[label]}]
!     Identifying label (if present), otherwise current line.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
!
      integer :: localrc
      integer :: n
      logical :: found
      logical :: tend

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      lineCount = 0
      columnCount = 0
      
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if ( present(label) ) then
        call ESMF_ConfigFindLabel(config, label=label,  &
            isPresent=found, rc=localrc)
        if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
            ESMF_CONTEXT, rcToReturn=rc)) return
        if (.not. found) then
           localrc = ESMF_RC_NOT_FOUND
           if ( present( rc )) then
             rc = localrc
           endif
           return
        endif
      endif

      do 
         call ESMF_ConfigNextLine(config, tableEnd=tend, rc=localrc)
         if (localrc /= ESMF_SUCCESS ) then
            lineCount = 0
            columnCount = 0
            exit
         endif
         if ( tend ) then
            exit
         else
            lineCount = lineCount + 1
            n = ESMF_ConfigGetLen( config, rc = localrc)
            if ( localrc /= ESMF_SUCCESS ) then
               lineCount = 0
               columnCount = 0
               if ( present( rc )) then
                 rc = localrc
               endif
               return
            else
               columnCount = max(columnCount, n)
            endif
         endif 
      enddo
      if ( present( rc )) then
        rc = localrc
      endif

    end subroutine ESMF_ConfigGetDim
    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetLen"
!BOP
! !IROUTINE: ESMF_ConfigGetLen - Get the length of the line in words from Config object
!
! !INTERFACE:
    integer function ESMF_ConfigGetLen(config, keywordEnforcer, label, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*),  intent(in),   optional :: label
      integer,           intent(out),  optional :: rc         
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION: 
! Gets the length of the line in words by counting words
! disregarding types.  Returns the word count as an integer.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [{[label]}]
!     Identifying label.   If not specified, use the current line.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      character(len=LSZ) :: string
      integer :: localrc
      integer :: count 
      logical :: eol, found

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_SUCCESS
      count = 0
      ESMF_ConfigGetLen = -1    ! assume error
      
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if( present( label )) then
         call ESMF_ConfigFindLabel(config, label=label,  &
             isPresent=found, rc=localrc)
         if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
             ESMF_CONTEXT, rcToReturn=rc)) return
         if (.not. found) then
            localrc = ESMF_RC_NOT_FOUND
            if (present( rc )) then
              rc = localrc
            endif
            return
         endif
      endif

      do
         call ESMF_ConfigGetString( config, string, eolFlag=eol, rc = localrc )
         if (eol) exit            
         if ( localrc == ESMF_SUCCESS ) then
            count = count + 1
         else
            exit
         endif
      enddo
 

      ESMF_ConfigGetLen = count

      if( present ( rc )) then
        rc = localrc
      endif

    end function ESMF_ConfigGetLen


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigIsCreated()"
!BOP
! !IROUTINE: ESMF_ConfigIsCreated - Check whether a Config object has been created

! !INTERFACE:
  function ESMF_ConfigIsCreated(config, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_ConfigIsCreated
!
! !ARGUMENTS:
    type(ESMF_Config), intent(in)            :: config
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt config} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[config]
!     {\tt ESMF\_Config} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_ConfigIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_ConfigGetInit(config)==ESMF_INIT_CREATED) &
      ESMF_ConfigIsCreated = .true.
  end function
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigLoadFile"
!BOP
!
! !IROUTINE: ESMF_ConfigLoadFile - Load resource file into Config object memory
!
! !INTERFACE:
    subroutine ESMF_ConfigLoadFile(config, filename, &
      keywordEnforcer, delayout, unique, rc)

! !ARGUMENTS:
      type(ESMF_Config),   intent(inout)         :: config     
      character(len=*),    intent(in)            :: filename 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_DELayout), intent(in),  optional :: delayout 
      logical,             intent(in),  optional :: unique 
      integer,             intent(out), optional :: rc         
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION: 
!  Resource file with {\tt filename} is loaded into memory.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [filename]
!     Configuration file name.
!   \item [{[delayout]}]
!     {\tt ESMF\_DELayout} associated with this {\tt config} object.
!   \item [{[unique]}]
!     If specified as true, uniqueness of labels are checked and 
!     error code set if duplicates found.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------

      integer :: localrc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      call ESMF_ConfigLoadFile_1proc_( config, filename, localrc )
           if (ESMF_LogFoundError(localrc, &
                                msg="unable to load file: " // trim (filename), &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_ConfigParseAttributes( config, unique, localrc )
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

      if ( present (delayout) ) then
         call ESMF_LogWrite("DELayout not used yet", ESMF_LOGMSG_WARNING, &
                           ESMF_CONTEXT)
      endif

      if (present( rc )) then
        rc = localrc 
      endif

    end subroutine ESMF_ConfigLoadFile

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigLoadFile_1proc_"
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigLoadFile_1proc - Load resource file into Config object memory
!

! !INTERFACE:

    subroutine ESMF_ConfigLoadFile_1proc_( config, filename, rc )

      type(ESMF_Config), intent(inout) :: config     ! ESMF Configuration
      character(len=*),  intent(in)    :: filename   ! file name
      integer,           intent(out), optional :: rc ! Error code
!
! !DESCRIPTION: Resource file filename is loaded into memory
!
!EOPI -------------------------------------------------------------------
      integer :: i, ls, ptr
      integer :: lu, nrecs
      integer :: iostat
      character(len=LSZ) :: line
      integer :: localrc
      character(LSZ), allocatable :: line_buffer(:)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

!     Open file
!     ---------     
      call ESMF_UtilIOUnitGet (lu, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return
      ! A open through an interface to avoid portability problems.
      ! (J.G.)

      call opntext(lu, filename, 'old', rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          msg="error opening text file: " // trim (filename), &
          ESMF_CONTEXT, rcToReturn=rc)) return

!     Count records, then read them into a local buffer
      nrecs = 0
      do
        read (lu, *, iostat=iostat)
        if (iostat /= 0) exit
        nrecs = nrecs + 1
      end do

      rewind (lu)

      allocate (line_buffer(nrecs))
      do, i = 1, nrecs
        read (lu, '(a)') line_buffer(i)
      end do

!     Read to end of file
!     -------------------
      config%cptr%buffer(1:1) = EOL
      ptr = 2                         ! next buffer position
      do, i = 1, nrecs

!        Read next line
!        --------------
         line = line_buffer(i)            ! copy next line
         call ESMF_Config_trim ( line )      ! remove trailing white space
         call ESMF_Config_pad ( line )       ! Pad with # from end of line

!        A non-empty line
!        ----------------
         ls = index_(line,'#' ) - 1    ! line length
         if ( ls .gt. 0 ) then
            if ( (ptr+ls) .gt. NBUF_MAX ) then
               if (ESMF_LogFoundError(ESMF_RC_MEM, msg="exceeded NBUF_MAX size", &
                   ESMF_CONTEXT, rcToReturn=rc)) return
            end if
            config%cptr%buffer(ptr:ptr+ls) = line(1:ls) // EOL
            ptr = ptr + ls + 1
         end if

      end do

!     All done
!     --------
! Close lu
      call clstext(lu, rc=localrc)
      if(localrc /= ESMF_SUCCESS) then
         localrc = ESMF_RC_FILE_CLOSE
         if ( present (rc )) then
           rc = localrc
         endif
         return
      endif
      config%cptr%buffer(ptr:ptr) = EOB
      config%cptr%nbuf = ptr
      config%cptr%this_line = ' '
      config%cptr%next_line = 1
      config%cptr%value_begin = 1

      if ( present (rc )) then
        rc = ESMF_SUCCESS
      endif

    end subroutine ESMF_ConfigLoadFile_1proc_

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigNextLine"
!BOP
!
! !IROUTINE: ESMF_ConfigNextLine - Find next line in a Config object
!
! !INTERFACE:
    subroutine ESMF_ConfigNextLine(config, keywordEnforcer, tableEnd, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      logical,           intent(out),  optional :: tableEnd
      integer,           intent(out),  optional :: rc 
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION: 
!   Selects the next line (for tables).
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [{[tableEnd]}]
!     Returns {\tt .true.} if end of table mark (::) is encountered.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}

!EOP -------------------------------------------------------------------
!
      integer :: localrc
      integer :: i, j
      logical :: local_tend

      ! Initialize return code; assume routine not implemented 
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localrc = ESMF_RC_NOT_IMPL
      local_tend = .false.
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if ( config%cptr%next_line >= config%cptr%nbuf ) then
         localrc = ESMF_RC_MEM
           if ( present (rc )) then
             rc = localrc
           endif
         return
      end if

      i = config%cptr%next_line
      j = i + index_(config%cptr%buffer(i:config%cptr%nbuf),EOL) - 2
      config%cptr%this_line = config%cptr%buffer(i:j) // BLK // EOL
      
      if ( config%cptr%this_line(1:2) .eq. '::' ) then
         localrc = ESMF_SUCCESS      ! end of table. We set rc = ESMF_SUCCESS
         local_tend = .true.         ! and end = .true. Used to be iret = 1  
         config%cptr%next_line = config%cptr%nbuf + 1
         if ( present (tableEnd )) then
           tableEnd = local_tend
         endif
         if ( present (rc )) then
           rc = localrc
         endif
         return
      end if

      config%cptr%next_line = j + 2
      localrc = ESMF_SUCCESS
      if ( present (tableEnd )) then
        tableEnd = local_tend
      endif
      if ( present (rc )) then
        rc = localrc
      endif

    end subroutine ESMF_ConfigNextLine

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigParseAttributes"
!BOPI
!
! !IROUTINE: ESMF_ConfigParseAttributes - Parse all attribute labels in a Config object
!
! !INTERFACE:

    subroutine ESMF_ConfigParseAttributes( config, unique, rc )


      implicit none

      type(ESMF_Config), intent(inout) :: config    ! ESMF Configuration
      logical, intent(in), optional :: unique    ! if unique is present & true, 
                                                 !  uniqueness of labels
                                                 !  is checked and error
                                                 !  code is set
      integer, intent(out), optional :: rc       ! Error return code
!
! !DESCRIPTION: Parse all attribute labels in given config object and place
!               into attributes table to track user retrieval
!
!EOPI -------------------------------------------------------------------
      integer :: i, j, k, a, b, localrc
      character(len=LSZ) :: this_line, label
      character(len=ESMF_MAXSTR) :: logmsg
      logical :: duplicate

      ! Initialize return code; assume routine not implemented
      if ( present (rc) ) then
        rc = ESMF_RC_NOT_IMPL
      endif
      localrc = ESMF_RC_NOT_IMPL

      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      ! initialize this config's attributes table "used" flags to "not used"
      do a = 1, NATT_MAX
        config%cptr%attr_used(a)%used = .false.
      enddo

      i = 1  ! start of buffer
      a = 1  ! first slot in attributes table
      do while ( i .lt. config%cptr%nbuf )

        ! get next line from buffer
        j = i + index_(config%cptr%buffer(i:config%cptr%nbuf), EOL) - 1
        this_line = config%cptr%buffer(i:j)

        ! look for label in this_line; non-blank characters followed by a colon
        if (this_line(1:2) .ne. '::' ) then  ! skip end-of-table mark
          k = index_(this_line, ':') - 1     ! label sans colon
          if (k .ge. 1) then  ! non-blank match
            ! found a label, trim it, 
            label = trim(adjustl(this_line(1:k)))

            ! ... check it for uniqueness if requested,
            duplicate = .false.
            if ( present( unique ) ) then
              if (unique) then
                !  TODO:  pre-sort and use binary search, or use hash function
                do b = 1, a-1
                  if (label == ESMF_UtilArray2String (config%cptr%attr_used(b)%label)) then
                    duplicate = .true.
                    logmsg = "Duplicate label '" // trim(label) // &
                                  "' found in attributes file"
                    call ESMF_LogSetError(rcToCheck=ESMF_RC_DUP_NAME, msg=logmsg, &
                                             ESMF_CONTEXT, rcToReturn=rc)
                    localrc = ESMF_RC_DUP_NAME
                  endif
                enddo
              endif
            endif

            ! ... and place it into attributes table
            if (.not.duplicate) then
               if ( a <= NATT_MAX ) then
                  allocate (config%cptr%attr_used(a)%label(len_trim (label)))
                  config%cptr%attr_used(a)%label = ESMF_UtilString2Array (trim (label))
               else
                  if (ESMF_LogFoundError(ESMF_RC_INTNRL_LIST,    &
                       msg="attribute out-of-range; increase NATT_MAX", &
                       ESMF_CONTEXT, rcToReturn=rc)) return
               endif
               a = a + 1
            endif
          endif
        endif

        ! set index to beginning of next line
        i = j + 1

      enddo

      ! remember number of labels found
      config%cptr%nattr = a-1

      if (present(rc)) then
        if (localrc == ESMF_RC_DUP_NAME) then
          rc = localrc
        else
          rc = ESMF_SUCCESS
        end if
      end if
      return

    end subroutine ESMF_ConfigParseAttributes

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigPrint"
!BOP
!
! !IROUTINE: ESMF_ConfigPrint - Write content of Config object to unit
!
! !INTERFACE:
    subroutine ESMF_ConfigPrint(config, keywordEnforcer, unit, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(in)  :: config
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer, optional, intent(in)  :: unit
      integer, optional, intent(out) :: rc
!
!
! !DESCRIPTION:
!   Write content of input {\tt ESMF\_Config} object to unit {\tt unit}.
!   If {\tt unit} not provided, writes to standard output.
!
!   The arguments are:
!   \begin{description}
!     \item[config]
!       The input {\tt ESMF\_Config} object.
!     \item[{[unit]}]
!       Output unit.
!     \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------

      integer   :: iounit
      integer   :: lbeg, lend

      ! Standard output unit
      integer, parameter :: stdout = 6

      ! Initialize return code; assume routine not implemented
      if ( present (rc) ) then
        rc = ESMF_RC_NOT_IMPL
      endif

      iounit = stdout
      if (present(unit)) iounit = unit

      lbeg = 2
      lend = index_( config % cptr % buffer(lbeg:config % cptr % nbuf), EOL )
      do while (lend >= lbeg .and. lend < config % cptr % nbuf)
        write(iounit, '(a)') trim(config % cptr % buffer(lbeg:lend))
        lbeg = lend + 2
        lend = lend + &
          index_( config % cptr % buffer(lbeg:config % cptr % nbuf), EOL )
      end do

      if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_ConfigPrint

!BOP
!
! !IROUTINE: ESMF_ConfigSetAttribute - Set a value in Config object
!
!
! !INTERFACE:
!     subroutine ESMF_ConfigSetAttribute(config, <value argument>, &
!       keywordEnforcer, label, rc)
!
! !ARGUMENTS:
!     type(ESMF_Config), intent(inout)           :: config     
!     <value argument>, see below for supported values
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!     character(len=*),  intent(in),   optional  :: label 
!     integer,           intent(out),  optional  :: rc   
!
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION: 
!  Sets a value in the {\tt config} object.
!
!      Supported values for <value argument> are:
!      \begin{description}
!      \item integer(ESMF\_KIND\_I4), intent(in)            :: value
!      \end{description}
!
!   The arguments are:
!     \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [<value argument>]
!     Value to set. 
!   \item [{[label]}]
!     Identifying attribute label. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigSetIntI4"
!BOPI
!
! !IROUTINE: ESMF_ConfigSetAttribute - Set a 4-byte integer number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigSetAttribute()
      subroutine ESMF_ConfigSetIntI4(config, value, &
        keywordEnforcer, label, rc)

! !ARGUMENTS:
      type(ESMF_Config),     intent(inout)         :: config     
      integer(ESMF_KIND_I4), intent(in)            :: value
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*),      intent(in),  optional :: label 
      integer,               intent(out), optional :: rc   

!
! !DESCRIPTION: 
!  Sets an integer {\tt value} in the {\tt config} object.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [value]
!     Integer value to set. 
!   \item [{[label]}]
!     Identifying attribute label. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI -------------------------------------------------------------------
!
      integer :: localrc
      character(len=ESMF_MAXSTR) :: logmsg
      character(len=LSZ) :: curVal, newVal
      integer :: i, j, k, m, nchar, ninsert, ndelete, lenThisLine

      ! Initialize return code; assume routine not implemented
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      ! Set config buffer at desired attribute
      if ( present (label) ) then
         call ESMF_ConfigGetString( config, curVal, label=label, rc=localrc)
      else
         call ESMF_ConfigGetString( config, curVal, rc = localrc )
      endif

      if ( localrc /= ESMF_SUCCESS ) then
        if ( localrc == ESMF_RC_NOT_FOUND ) then
          ! set config buffer at end for appending
          i = config%cptr%nbuf
        else
          if ( present( rc ) ) then
            rc = localrc
          endif
          return
        endif
      else ! attribute found
        ! set config buffer for overwriting/inserting
        i = config%cptr%value_begin
        curVal = BLK // trim(curVal) // BLK // EOL ! like config%cptr%this_line
      endif

      ! for appending, create new attribute string with label and value
      if ( i .eq. config%cptr%nbuf .and. present(label) ) then
        write(newVal, *) label, value
        newVal = trim(adjustl(newVal)) // EOL
        j = i + len_trim(newVal)

        ! check to ensure len of newVal doesn't exceed LSZ
        if ( (j-i) .gt. LSZ) then
           write(logmsg, *) ", attribute label, value & EOL are ", j-i, &
               " characters long, only ", LSZ, " characters allowed per line"
           if (ESMF_LogFoundError(ESMC_RC_LONG_STR, msg=logmsg, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! check if enough space left in config buffer
        if (j .ge. NBUF_MAX) then   ! room for EOB if necessary
           write(logmsg, *) ", attribute label & value require ", j-i+1, &
               " characters (including EOL & EOB), only ", NBUF_MAX-i, &
               " characters left in config buffer"
           if (ESMF_LogFoundError(ESMC_RC_LONG_STR, msg=logmsg, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
        endif
      endif

      ! overwrite, with possible insertion or deletion of extra characters
      if (i .eq. config%cptr%value_begin) then
         write(newVal, *) value
         newVal = BLK // trim(adjustl(newVal)) // EOL
         j = i + len_trim(newVal) - 1

         !  check if we need more space to insert new characters;
         !  shift buffer down (linked-list redesign would be better!)
         nchar = j-i+1
         lenThisLine = len_trim(curVal) - 1
         if ( nchar .gt. lenThisLine) then

            ! check to ensure length of extended line doesn't exceed LSZ
            do m = i, 1, -1
              if (config%cptr%buffer(m:m) .eq. EOL) then
                exit
              endif
            enddo
            if (j-m+1 .gt. LSZ) then
               write(logmsg, *) ", attribute label, value & EOL are ", j-m+1, &
                  " characters long, only ", LSZ, " characters allowed per line"
               if (ESMF_LogFoundError(ESMC_RC_LONG_STR, msg=logmsg, &
                                         ESMF_CONTEXT, rcToReturn=rc)) return
            endif

            ! check if enough space left in config buffer to extend line
            if (j+1 .ge. NBUF_MAX) then   ! room for EOB if necessary
               write(logmsg, *) ", attribute label & value require ", j-m+1, &
                   " characters (including EOL & EOB), only ", NBUF_MAX-i, &
                   " characters left in config buffer"
               if (ESMF_LogFoundError(ESMC_RC_LONG_STR, msg=logmsg, &
                                         ESMF_CONTEXT, rcToReturn=rc)) return
            endif

            ninsert = nchar - lenThisLine
            do k = config%cptr%nbuf, j, -1
               config%cptr%buffer(k+ninsert:k+ninsert) = config%cptr%buffer(k:k)
            enddo
            config%cptr%nbuf = config%cptr%nbuf + ninsert

         ! or if we need less space and remove characters;
         ! shift buffer up
         elseif ( nchar .lt. lenThisLine ) then
           ndelete = lenThisLine - nchar
            do k = j+1, config%cptr%nbuf
               config%cptr%buffer(k-ndelete:k-ndelete) = config%cptr%buffer(k:k)
            enddo
            config%cptr%nbuf = config%cptr%nbuf - ndelete
         endif
      endif

      ! write new attribute value into config
      config%cptr%buffer(i:j) = newVal(1:len_trim(newVal))

      ! if appended, reset EOB marker and nbuf
      if (i .eq. config%cptr%nbuf) then
        j = j + 1
        config%cptr%buffer(j:j) = EOB
        config%cptr%nbuf = j
      endif

      if( present( rc )) then
        rc = ESMF_SUCCESS
      endif
      
      return
    end subroutine ESMF_ConfigSetIntI4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigSetCurrentAttrUsed"
!BOPI
!
! !IROUTINE: ESMF_ConfigSetCurrentAttrUsed - Set Current Attribute "Used" flag
!
! !INTERFACE:

    subroutine ESMF_ConfigSetCurrentAttrUsed( config, used, rc )


      implicit none

      type(ESMF_Config), intent(inout)         :: config ! ESMF Configuration
      logical,           intent(in)            :: used     ! used flag
      integer,           intent(out), optional :: rc       ! Error return code
!
! !DESCRIPTION: Set the given config's current attribute's used flag
!
!EOPI -------------------------------------------------------------------
      integer :: i

      ! Initialize return code; assume routine not implemented
      if ( present (rc) ) then
        rc = ESMF_RC_NOT_IMPL
      endif

      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)


      ! find attr label and set its used flag to given value
      !  TODO:  pre-sort and use binary search, or use hash function
      do i = 1, NATT_MAX
        if (associated (config%cptr%attr_used(i)%label)) then
          if (trim(config%cptr%current_attr) == ESMF_UtilArray2String (config%cptr%attr_used(i)%label)) then
            config%cptr%attr_used(i)%used = used
            exit
          end if
        endif
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      return

    end subroutine ESMF_ConfigSetCurrentAttrUsed

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigValidate"
!BOP
!
! !IROUTINE: ESMF_ConfigValidate - Validate a Config object
!
! !INTERFACE:
    subroutine ESMF_ConfigValidate(config, &
      keywordEnforcer, options, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character (len=*), intent(in),   optional :: options
      integer,           intent(out),  optional :: rc 
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION: 
!   Checks whether a {\tt config} object is valid.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     {\tt ESMF\_Config} object to be validated.
!   \item[{[options]}]
!     \begin{sloppypar}
!     If none specified:  simply check that the buffer is not full and the
!       pointers are within range.
!     "unusedAttributes" - Report to the default logfile all attributes not
!       retrieved via a call to {\tt ESMF\_ConfigGetAttribute()} or
!       {\tt ESMF\_ConfigGetChar()}.  The attribute name (label) will be
!       logged via {\tt ESMF\_LogErr} with the WARNING log message type.
!       For an array-valued attribute, retrieving at least one value via
!       {\tt ESMF\_ConfigGetAttribute()} or {\tt ESMF\_ConfigGetChar()}
!       constitutes being "used."
!     \end{sloppypar}
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     Equals {\tt ESMF\_RC\_ATTR\_UNUSED} if any unused attributes are found
!     with option "unusedAttributes" above.
!   \end{description}

!EOP -------------------------------------------------------------------
      character(len=ESMF_MAXSTR) :: logmsg
      integer :: i, localrc

      if (present(rc)) then
        rc = ESMF_RC_NOT_IMPL
      endif
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      ! validate internal buffer indices

      if (config%cptr%nbuf < 0 .or. config%cptr%nbuf > NBUF_MAX) then
        if (ESMF_LogFoundError(ESMF_RC_INTNRL_LIST, &
                                  msg="config%cptr%nbuf out-of-range.", &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      if (config%cptr%next_line < 0 .or. config%cptr%next_line >= config%cptr%nbuf) then
        if (ESMF_LogFoundError(ESMF_RC_INTNRL_LIST, &
                                  msg="config%cptr%next_line out-of-range.", &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      if (config%cptr%nattr < 0 .or. config%cptr%nattr > NATT_MAX) then
        if (ESMF_LogFoundError(ESMF_RC_INTNRL_LIST, &
                                  msg="config%cptr%nattr out-of-range.", &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! optional validations

      if (present(options)) then
        if (options == "unusedAttributes") then
          do i = 1, config%cptr%nattr
            if (.not.(config%cptr%attr_used(i)%used)) then
              logmsg = "Config attribute label '" // &
                  ESMF_UtilArray2String (config%cptr%attr_used(i)%label) // &
                  "' unused (not retrieved via ESMF_ConfigGetAttribute() " // &
                  "or ESMF_ConfigGetChar())."
              call ESMF_LogWrite(logmsg, ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
                localrc = ESMF_RC_ATTR_UNUSED
            endif
          enddo
        endif
      endif

      if (present(rc)) then
        if (localrc == ESMF_RC_ATTR_UNUSED) then
          rc = localrc
        else
          rc = ESMF_SUCCESS
        end if
      end if

      return

    end subroutine ESMF_ConfigValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigEQ()"
!BOPI
! !IROUTINE:  ESMF_ConfigEQ - Compare two Config objects for equality
!
! !INTERFACE:
  function ESMF_ConfigEQ(Config1, Config2)
!
! !RETURN VALUE:
    logical :: ESMF_ConfigEQ

! !ARGUMENTS:
    type(ESMF_Config), intent(in) :: Config1
    type(ESMF_Config), intent(in) :: Config2

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Config}
!     class.  See "interface operator(==)" above for complete description.
!
!EOPI

    ESMF_ConfigEQ = associated (Config1%cptr, Config2%cptr)

  end function ESMF_ConfigEQ

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigNE()"
!BOPI
! !IROUTINE:  ESMF_ConfigEQ - Compare two Config objects for inequality
!
! !INTERFACE:
  function ESMF_ConfigNE(Config1, Config2)
!
! !RETURN VALUE:
    logical :: ESMF_ConfigNE

! !ARGUMENTS:
    type(ESMF_Config), intent(in) :: Config1
    type(ESMF_Config), intent(in) :: Config2

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Config}
!     class.  See "interface operator(==)" above for complete description.
!
!EOPI

    ESMF_ConfigNE = .not. associated (Config1%cptr, Config2%cptr)

  end function ESMF_ConfigNE


!-----------------------------------------------------------------------


    integer function index_ (string,tok)

!-------------------------------------------------------------------------
! !ROUTINE: index_ Extension of the Fortran 77 intrinsic "index" for
!  "string" (input) with length that can exceed 2**15-1 (=MAXLEN).  
!
! !DESCRIPTION: Finds the starting location = "index_", of the first character in "tok" 
!  within "string", where string is of "arbitrary" length.  If tok occurs more than
!  once in "string", then the value of index_ is based on the first occurrence of "tok". 
!
! !CALLING SEQUENCE:
!
!      index_( string,tok )
!
! !INPUT PARAMETERS:
!
      character(len=*), intent(in) :: string, tok
!
!-------------------------------------------------------------------------
      integer :: idx, i, n, nlen, lt, ibot, itop
      integer, parameter :: MAXLEN = 32767   ! max size of signed 2-byte integer

      n = len(string)         ! length of string
      lt = len(tok)           ! length of token tok
      i = 1                   ! initialize loop index
      nlen = MAXLEN-lt        ! index requires len(sting)+len(tok)<=MAXLEN 
      itop = min(nlen,n)      ! top of string to index
      ibot = 1                ! bottom of string
      idx  = index(string(ibot:itop),tok)  ! set for good, if itop=n (<=MAXLEN)
      do while(idx .eq. 0 .and. itop < n)
       i = i+1
       itop = min(i*MAXLEN-lt,n)      ! subtract lt to find tok at bdry
       ibot = max(1,itop+1-nlen)    ! bottom of string to index
       idx  = index(string(ibot:itop),tok)   ! idx>=0, since itop-ibot<=MAXLEN
      end do
      index_ = idx                    ! case where idx = 0, or (i=1 & idx > 0)
      if(idx > 0) index_ = idx - 1 + ibot

    end function index_

    subroutine ESMF_Config_Trim ( string )

!-------------------------------------------------------------------------
!
! !ROUTINE:  ESMF_Config_Trim() - Removes leading white space from strings.
!
! !DESCRIPTION: 
!
!    Removes blanks and TABS from beginning of string. 
!    This is a low level i90 routine.
! 
! !CALLING SEQUENCE: 
!
!     call ESMF_Config_Trim ( string )
!
! !INPUT PARAMETERS: 
!
      character(*), intent(inout) :: string    ! the input string
!
! !OUTPUT PARAMETERS:
!
!     character(*), intent(inout) :: string    ! the modified string
!
!
!-------------------------------------------------------------------------

      integer :: ib, i

!     Find first non-blank/non-tab character
!     --------------------------------------
      ib = 1
      do i = 1, len (string)-1
         if ( string(i:i) .ne. ' ' .and. &
            string(i:i) .ne. TAB ) exit
         ib = ib + 1
      end do

!     String without leading blanks/tabs
!     ----------------------------------
      string = string(ib:)

    end subroutine ESMF_Config_trim


    subroutine ESMF_Config_pad ( string )

!-------------------------------------------------------------------------!
! !ROUTINE:  ESMF_CONFIG_Pad() --- Pad strings.
! 
! !DESCRIPTION: 
!
!     Pads from the right with the comment character (\#). It also
!  replaces TAB's with blanks for convenience. This is a low level
!  i90 routine.
!
! !CALLING SEQUENCE: 
!
!      call ESMF_Config_pad ( string )
!
! !INPUT PARAMETERS: 
!
       character(*), intent(inout) :: string       ! input string

! !OUTPUT PARAMETERS:            ! modified string
!
!      character(*), intent(inout) :: string
!
! !BUGS:  
!
!      It alters TAB's even inside strings.
!
!
! !REVISION HISTORY: 
!
!  19Jun96   da Silva   Original code.
!-------------------------------------------------------------------------

      integer :: i

!     Pad end of string with #
!     ------------------------
      do i = len (string), 1, -1 
         if ( string(i:i) .ne. ' ' .and. &
            string(i:i) .ne. '$' ) exit
         string(i:i) = '#'
      end do

!     Replace TAB's with blanks
!     -------------------------
      do i = 1, len (string)
         if ( string(i:i) .eq. TAB ) string(i:i) = BLK
         if ( string(i:i) .eq. '#' ) exit
      end do

    end subroutine ESMF_Config_pad

!-----------------------------------------------------------------------
! !IROUTINE: opntext - portably open a text file
!
! !DESCRIPTION:
!
!       Open a text (ASCII) file.  Under FORTRAN, it is defined as
!       "formatted" with "sequential" access.
!
! !INTERFACE:

    subroutine opntext(lu, filename, status, rc)

      integer,         intent(in) :: lu     ! logical unit number
      character(len=*),intent(in) :: filename  ! filename to be opened
      character(len=*),intent(in) :: status ! the value for STATUS=<>
      integer,         intent(out):: rc     ! the status

!-----------------------------------------------------------------------
!

                ! local parameter

        character(len=len(status)) :: Ustat
        integer :: iostat


#ifdef _UNICOS
        call asnunit(lu,'-R',iostat)         ! remove any set attributes
        if (iostat /= 0) then
          rc = ESMF_FAILURE
          return  ! let the parent handle it
        end if
#endif

        Ustat = ESMF_UtilStringUpperCase (string=status)
        select case(Ustat)

        case ('APPEND')

          open(                           &
            unit        =lu,              &
            file        =filename,        &
            form        ='formatted',     &
            access      ='sequential',    &
            status      ='unknown',       &
            action      ='readwrite',     &
            position    ='append',        &
            iostat      =iostat            )

        case default

          open(                           &
            unit        =lu,              &
            file        =filename,        &
            form        ='formatted',     &
            access      ='sequential',    &
            status      =status,          &
            action      ='read',          &
            position    ='asis',          &
            iostat      =iostat            )

        end select

        if (iostat == 0) then
          rc = ESMF_SUCCESS
        else
          rc = ESMF_RC_FILE_OPEN
        endif

        end subroutine opntext


!-----------------------------------------------------------------------
!
! !IROUTINE: clstext - close a text file opend with an opntext() call
!
! !DESCRIPTION:

! !INTERFACE:

    subroutine clstext(lu, rc, status)

      integer,                    intent(in)  :: lu     ! a logical unit to close
      integer,                    intent(out) :: rc     ! the status
      Character(len=*), optional, intent(In)  :: status ! keep/delete

!-----------------------------------------------------------------------
          character(len=6) :: status_
          integer :: iostat

          status_ = 'KEEP'
          If (Present(status)) Then
             Select Case (Trim(status))
             Case ('DELETE','delete')
                status_ = 'DELETE'
             Case  ('KEEP','keep')
                status_ = 'KEEP'
             Case Default
                rc = ESMF_RC_FILE_UNEXPECTED
                return
             End Select
          End If

        close(lu,iostat=iostat,status=status_)
#ifdef _UNICOS
        if(iostat == 0) call asnunit(lu,'-R',iostat) ! remove any attributes
#endif

        rc = ESMF_SUCCESS

    end subroutine clstext


!-----------------------------------------------------------------------
    end module ESMF_ConfigMod
