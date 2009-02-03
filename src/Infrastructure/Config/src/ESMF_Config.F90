! $Id: ESMF_Config.F90,v 1.44.2.5 2009/02/03 22:20:35 theurich Exp $
!==============================================================================
! Earth System Modeling Framework
!
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
       public :: ESMF_ConfigNextLine   ! selects next line (for tables)
       public :: ESMF_ConfigGetAttribute ! returns next value
       public :: ESMF_ConfigGetChar    ! returns only a single character
       public :: ESMF_ConfigGetLen ! gets number of words in the line(function)
       public :: ESMF_ConfigGetDim ! gets number of lines in the table
                                   ! and max number of columns by word 
                                   ! counting disregarding type (function)
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
! !IROUTINE: ESMF_ConfigGetAttribute - Get an attribute from a Config
!
! !INTERFACE:
    interface ESMF_ConfigGetAttribute
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ConfigGetString
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
! !IROUTINE: ESMF_ConfigSetAttribute - Set an attribute in a Config
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
! PRIVATE PARAMETER  SETTINGS:
!------------------------------------------------------------------------------
! Revised parameter table to fit Fortran 90 standard.

       integer,   parameter :: LSZ = 256  ! Maximum line size
       integer,   parameter :: MSZ = 512  ! Used to size buffer; this is
                                          ! usually *less* than the number
                                          ! of non-blank/comment lines
                                          ! (because most lines are shorter
                                          ! then LSZ)
 
       integer,   parameter :: NBUF_MAX = MSZ*LSZ ! max size of buffer
       integer,   parameter :: NATT_MAX = NBUF_MAX/16 ! max # attributes;  
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
       

!    Defines standard i/o units.

        integer, parameter :: stdin  = 5
        integer, parameter :: stdout = 6

#ifdef sysHP_UX

        ! Special setting for HP_UX

        integer, parameter :: stderr = 7
#else
        ! Generic setting for UNIX other than HP-UX

        integer, parameter :: stderr = 0
#endif

        integer,parameter :: MX_LU=255
!------------------------------------------------------------------------------
! !OPAQUE TYPES:
!------------------------------------------------------------------------------
       type ESMF_ConfigAttrUsed
          sequence
          private              
          character(len=LSZ)      :: label  ! attribute label
          logical                 :: used   ! attribute used (retrieved) or not
          ESMF_INIT_DECLARE
       end type ESMF_ConfigAttrUsed

       type ESMF_ConfigClass
#ifndef ESMF_SEQUENCE_BUG
          sequence
#endif
          !private              
#ifndef ESMF_NO_INITIALIZERS
          character(len=NBUF_MAX),pointer :: buffer => Null()
                                                       ! hold the whole file
          character(len=LSZ),     pointer :: this_line => Null()
                                                       ! the current line
#else
          character(len=NBUF_MAX),pointer :: buffer    ! hold the whole file
          character(len=LSZ),     pointer :: this_line ! the current line
#endif
          integer :: nbuf                              ! actual size of buffer 
          integer :: next_line                         ! index_ for next line 
                                                       !   on buffer
          integer :: value_begin                       ! index of beginning of
                                                       !   value
#ifndef ESMF_NO_INITIALIZERS
          type(ESMF_ConfigAttrUsed), dimension(:), &
                                  pointer :: attr_used => Null()
                                                       ! used attributes table
#else
          type(ESMF_ConfigAttrUsed), dimension(:), &
                                  pointer :: attr_used ! used attributes table
#endif
          integer :: nattr                             ! number of attributes
                                                       !   in the "used" table
          character(len=LSZ)          :: current_attr  ! the current attr label
          ESMF_INIT_DECLARE
       end type ESMF_ConfigClass

!      ! Config wrapper
       type ESMF_Config
#ifndef ESMF_SEQUENCE_BUG
          sequence
#endif
          !private       
#ifndef ESMF_NO_INITIALIZERS
          type (ESMF_ConfigClass), pointer :: cptr => NULL()
#else
          type (ESMF_ConfigClass), pointer :: cptr
#endif
          ESMF_INIT_DECLARE
       end type ESMF_Config

     contains


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigAttrUsedGetInit"
!BOPI
! !IROUTINE:  ESMF_ConfigAttrUsedGetInit - Get initialization status.

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
!           {\tt ESMF\_ConfigAttrUsed} from which to retreive status.
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

     ESMF_INIT_CHECK_SHALLOW(ESMF_ConfigAttrUsedGetInit, ESMF_ConfigAttrUsedInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_ConfigAttrUsedValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigClassGetInit"
!BOPI
! !IROUTINE:  ESMF_ConfigClassGetInit - Get initialization status.

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
!           {\tt ESMF\_ConfigClass} from which to retreive status.
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

     ESMF_INIT_CHECK_SHALLOW(ESMF_ConfigClassGetInit, ESMF_ConfigClassInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_ConfigClassValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetInit"
!BOPI
! !IROUTINE:  ESMF_ConfigGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_ConfigGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_Config), intent(inout), optional :: d
       ESMF_INIT_TYPE :: ESMF_ConfigGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Config} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(d)) then
         ESMF_ConfigGetInit = ESMF_INIT_GET(d)
       else
         ESMF_ConfigGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_ConfigGetInit



!
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigCreate"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigCreate - Create a Config object
!
! !INTERFACE:
      type(ESMF_Config) function ESMF_ConfigCreate( rc )

! !ARGUMENTS:
      integer,intent(out), optional              :: rc 
!
! !DESCRIPTION: 
!   Creates an {\tt ESMF\_Config} for use in subsequent calls.
!
!   The arguments are:
!   \begin{description}
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      integer :: iret
      type(ESMF_ConfigClass), pointer :: config_local
      type(ESMF_ConfigAttrUsed), dimension(:), pointer :: attr_used_local
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
 
! Initialization
      allocate(config_local, stat=iret)
      if (ESMF_LogMsgFoundAllocError(iret, "Allocating config class", &
                                        ESMF_CONTEXT, rc)) return

      allocate(config_local%buffer, config_local%this_line, stat = iret)
      if (ESMF_LogMsgFoundAllocError(iret, "Allocating local buffer 1", &
                                        ESMF_CONTEXT, rc)) return

      ! TODO: Absoft 8 compiler bug necessitates allocating pointer within
      ! derived type via local pointer first.  Absoft 9/Jazz bug necessitates
      ! this must be a separate allocate statement.
      allocate(attr_used_local(NATT_MAX), stat = iret)
      if (ESMF_LogMsgFoundAllocError(iret, "Allocating local buffer 2", &
                                        ESMF_CONTEXT, rc)) return
      config_local%attr_used => attr_used_local

      ESMF_ConfigCreate%cptr => config_local
      if (present( rc )) then
        rc = iret
      endif

      ESMF_INIT_SET_CREATED(ESMF_ConfigCreate)
      return
    end function ESMF_ConfigCreate


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigDestroy"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigDestroy - Destroy a Config object
!
! !INTERFACE:
    subroutine ESMF_ConfigDestroy( config, rc )

! !ARGUMENTS:
      type(ESMF_Config) :: config
      integer,intent(out), optional    :: rc
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
      integer :: iret

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      iret = 0

      ! TODO: Absoft 9/Jazz bug necessitates this separate deallocate statement
      ! before the other (must be in reverse order of allocation)
      deallocate(config%cptr%attr_used, stat = iret)
      if (ESMF_LogMsgFoundAllocError(iret, "Deallocating local buffer 2", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(config%cptr%buffer, config%cptr%this_line, stat = iret)
      if (ESMF_LogMsgFoundAllocError(iret, "Deallocating local buffer 1", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(config%cptr, stat = iret)
      if (ESMF_LogMsgFoundAllocError(iret, "Deallocating config type", &
                                     ESMF_CONTEXT, rc)) return
      nullify(config%cptr)

      if (present( rc )) then
        rc = iret
      endif

      ESMF_INIT_SET_DELETED(config)
      return

     end subroutine ESMF_ConfigDestroy

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigFindLabel"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigFindLabel - Find a label
!
! !INTERFACE:
    subroutine ESMF_ConfigFindLabel( config, label, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout) :: config 
      character(len=*), intent(in)     :: label
      integer, intent(out), optional   :: rc 

! !DESCRIPTION: Finds the {\tt label} (key) in the {\tt config} file. 
!
!               Since the search is done by looking for a word in the 
!               whole resource file, it is important to use special 
!               conventions to distinguish labels from other words 
!               in the resource files. The DAO convention is to finish 
!               line labels by : and table labels by ::.
!
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [label]
!     Identifying label. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     Equals -1 if buffer could not be loaded, -2 if label not found,
!     and -3 if invalid operation with index.
!   \end{description}
!
!EOP -------------------------------------------------------------------

      integer :: i, j, iret

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0

      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

!     Determine whether label exists
!     ------------------------------    

      i = index_ ( config%cptr%buffer(1:config%cptr%nbuf), EOL//label ) + 1
      if ( i .eq. 1 ) then
         config%cptr%this_line = BLK // EOL
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, &
                                "label not found", &
                                 ESMF_CONTEXT, rc)) return
      elseif(i.le.0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "invalid operation with index", &
                                 ESMF_CONTEXT, rc)) return
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

      iret = ESMF_SUCCESS
      if ( present (rc )) then
        rc = iret
      endif
      
      return
    end subroutine ESMF_ConfigFindLabel


!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a value
!
!
! !INTERFACE:
!      subroutine ESMF_ConfigGetAttribute( config, <value argument>, &
!                                          label, default, rc )
!
! !ARGUMENTS:
!      type(ESMF_Config), intent(inout)       :: config     
!      <value argument>, see below for supported values
!      character(len=*), intent(in), optional :: label 
!      character(len=*), intent(in), optional :: default 
!      integer, intent(out), optional         :: rc     
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

!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of values
!
! !INTERFACE:
!      subroutine ESMF_ConfigGetAttribute( config, <value list argument>, &
!                                          count, label, default, rc )
!
! !ARGUMENTS:
!      type(ESMF_Config), intent(inout)       :: config     
!      <value list argument>, see below for values      
!      integer, intent(in)                    :: count
!      character(len=*), intent(in), optional :: label 
!      character(len=*), intent(in), optional :: default 
!      integer, intent(out), optional         :: rc     
!
! !DESCRIPTION:
!      Gets a list of values from the {\tt config} object.  
!
!      Supported values for <value list argument> are:
!      \begin{description}
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
!   \item [{[label]}]
!     Identifing label. 
!   \item [{[default]}]
!     Default value if {\tt label} is not found in {\tt config} object. 
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetString"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI ------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a character string
!
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetString( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)       :: config     
      character(len=*), intent(out)          :: value
      character(len=*), intent(in), optional :: label 
      character(len=*), intent(in), optional :: default 
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
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}

!EOPI ------------------------------------------------------------------
      character(len=1) :: ch
      integer :: ib, ie, iret
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)


! Default setting
      if( present( default ) ) then 
         value = default
      else
         value = BLK
      endif

! Processing
      if(present( label )) then
         call ESMF_ConfigFindLabel( config, label, iret )
         if ( iret /= 0 ) then
            if (present(default)) then
               iret = ESMF_SUCCESS
            end if
            if ( present (rc )) then
              rc = iret
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
         iret = -1
         if ( present (rc )) then
           rc = iret
         endif
         return
      else
         ! Get the string, and shift the rest of %this_line to
         ! the left
         
         value = config%cptr%this_line(ib:ie) 
         config%cptr%this_line = config%cptr%this_line(ie+2:)
         iret = 0
      end if

      if ( present (rc )) then
        rc = iret
      endif
      return

      
    end subroutine ESMF_ConfigGetString
    
    

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatR4"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a 4-byte real number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatR4( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)      :: config    
      real(ESMF_KIND_R4), intent(out)          :: value    
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
      integer :: iret
      character(len=LSZ) :: string
      real(ESMF_KIND_R4) :: x
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
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
         call ESMF_ConfigGetString( config, string, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, string, rc = iret )
      endif

      if ( iret .eq. 0 ) then
           read(string,*,iostat=iret) x
           if ( iret .ne. 0 ) iret = -2
           if ( iret .eq. 0) then
             call ESMF_ConfigSetCurrentAttrUsed(config, .true.)
           else
             ! undo what GetSring() did
             call ESMF_ConfigSetCurrentAttrUsed(config, .false.)
           endif
      else
         if( present( default )) then
            x = default
            iret = ESMF_SUCCESS
         endif
      end if

      if ( iret .eq. 0 ) then
         value = x
      endif

      if( present( rc )) then
        rc = iret 
      endif
      return

    end subroutine ESMF_ConfigGetFloatR4



#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatR8"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get an 8-byte real number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatR8( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)      :: config    
      real(ESMF_KIND_R8), intent(out)          :: value 
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
      integer :: iret
      character(len=LSZ) :: string
      real(ESMF_KIND_R8) :: x
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
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
         call ESMF_ConfigGetString( config, string, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, string, rc = iret )
      endif

      if ( iret .eq. 0 ) then
           read(string,*,iostat=iret) x
           if ( iret .ne. 0 ) iret = -2
           if ( iret .eq. 0) then
             call ESMF_ConfigSetCurrentAttrUsed(config, .true.)
           else
             ! undo what GetSring() did
             call ESMF_ConfigSetCurrentAttrUsed(config, .false.)
           endif
      else
         if( present( default )) then
            x = default
            iret = ESMF_SUCCESS
         endif
      end if

      if ( iret .eq. 0 ) then
         value = x
      endif

      if( present( rc )) then
        rc = iret 
      endif
      return

    end subroutine ESMF_ConfigGetFloatR8



#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatsR4"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 4-byte real numbers

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatsR4( config, valueList, count, label,  &
                                         default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)      :: config    
      real(ESMF_KIND_R4), intent(inout)        :: valueList(:) 
      integer, intent(in)                      :: count 
      character(len=*), intent(in), optional   :: label 
      real(ESMF_KIND_R4), intent(in), optional :: default
      integer, intent(out), optional           :: rc    
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
      integer :: iret, i 

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if (count.le.0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "invalid SIZE", &
                                 ESMF_CONTEXT, rc)) return
      endif
! Default setting
      if( present( default ) ) then 
         valueList(1:count) = default

      else
         valueList(1:count) = 0.0
      endif
! Processing
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label, rc = iret )
      end if

      do i = 1, count
         
         if(present( default )) then
            call ESMF_ConfigGetFloatR4( config, valueList(i), default=default, rc=iret )
         else
            call ESMF_ConfigGetFloatR4( config, valueList(i), rc = iret)
         endif
      enddo
      if(present( rc )) then
        rc = iret
      endif
      return
    end subroutine ESMF_ConfigGetFloatsR4

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetFloatsR8"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 8-byte real numbers

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetFloatsR8( config, valueList, count, label,  &
                                         default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)      :: config    
      real(ESMF_KIND_R8), intent(inout)        :: valueList(:) 
      integer, intent(in)                      :: count 
      character(len=*), intent(in), optional   :: label 
      real(ESMF_KIND_R8), intent(in), optional :: default
      integer, intent(out), optional           :: rc    
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
      integer :: iret, i 
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if (count.le.0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "invalid SIZE", &
                                 ESMF_CONTEXT, rc)) return
      endif
       
! Default setting
      if( present( default ) ) then 
         valueList(1:count) = default
      else
         valueList(1:count) = 0.0
      endif

! Processing
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label, rc = iret )
      end if

      do i = 1, count
         
         if(present( default )) then
            call ESMF_ConfigGetFloatR8( config, valueList(i), default=default, rc=iret )
         else
            call ESMF_ConfigGetFloatR8( config, valueList(i), rc = iret)
         endif
      enddo

      if(present( rc )) then
        rc = iret
      endif

      return
    end subroutine ESMF_ConfigGetFloatsR8

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntI4"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a 4-byte integer number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntI4( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config     
      integer(ESMF_KIND_I4), intent(out)           :: value
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
      character(len=LSZ) :: string
      real(ESMF_KIND_R8) :: x
      integer(ESMF_KIND_I4) ::  n
      integer :: iret

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
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
         call ESMF_ConfigGetString( config, string, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, string, rc = iret )
      endif

      if ( iret .eq. 0 ) then
           read(string,*,iostat=iret) x
           if ( iret .ne. 0 ) iret = -2
           if ( iret .eq. 0) then
             call ESMF_ConfigSetCurrentAttrUsed(config, .true.)
           else
             ! undo what GetSring() did
             call ESMF_ConfigSetCurrentAttrUsed(config, .false.)
           endif
      end if
      if ( iret .eq. 0 ) then
         n = nint(x)
      else
         if( present( default )) then
            n = default
            iret = ESMF_SUCCESS
         else
            n = 0
         endif
      endif

      if ( iret .eq. 0 ) then
         value = n
      endif

      if( present( rc )) then
        rc = iret
      endif
      
      return
    end subroutine ESMF_ConfigGetIntI4

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntI8"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get an 8-byte integer number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntI8( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config     
      integer(ESMF_KIND_I8), intent(out)           :: value
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
      character(len=LSZ) :: string
      real(ESMF_KIND_R8) :: x
      integer(ESMF_KIND_I8) :: n
      integer :: iret

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
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
         call ESMF_ConfigGetString( config, string, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, string, rc = iret )
      endif

      if ( iret .eq. 0 ) then
           read(string,*,iostat=iret) x
           if ( iret .ne. 0 ) iret = -2
           if ( iret .eq. 0) then
             call ESMF_ConfigSetCurrentAttrUsed(config, .true.)
           else
             ! undo what GetSring() did
             call ESMF_ConfigSetCurrentAttrUsed(config, .false.)
           endif
      end if
      if ( iret .eq. 0 ) then
         n = nint(x)
      else
         if( present( default )) then
            n = default
            iret = ESMF_SUCCESS
         else
            n = 0
         endif
      endif

      if ( iret .eq. 0 ) then
         value = n
      endif

      if( present( rc )) then
        rc = iret
      endif
      
      return
    end subroutine ESMF_ConfigGetIntI8


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntsI4"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 4-byte integers
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntsI4( config, valueList, count, label,  &
                                       default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config      
      integer(ESMF_KIND_I4), intent(inout)         :: valueList(:)  
      integer, intent(in)                          :: count  
      character(len=*), intent(in), optional       :: label 
      integer(ESMF_KIND_I4), intent(in), optional  :: default
      integer, intent(out), optional               :: rc    
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
      integer :: iret, i 
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if (count.le.0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "invalid SIZE", &
                                 ESMF_CONTEXT, rc)) return
      endif
       
 ! Default setting
      if( present( default ) ) then 
         valueList(1:count) = default
      else
         valueList(1:count) = 0
      endif

! Processing 
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label, rc = iret )
      end if

      do i = 1, count
         
         if(present( default )) then
            call ESMF_ConfigGetIntI4( config, valueList(i), default = default, rc = iret)
         else
            call ESMF_ConfigGetIntI4( config, valueList(i), rc = iret)
         endif
      enddo

      if(present( rc )) then
        rc = iret
      endif

      return
    end subroutine ESMF_ConfigGetIntsI4



#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetIntsI8"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of 8-byte integers
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetIntsI8( config, valueList, count, label,  &
                                       default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config      
      integer(ESMF_KIND_I8), intent(inout)         :: valueList(:)  
      integer, intent(in)                          :: count  
      character(len=*), intent(in), optional       :: label 
      integer(ESMF_KIND_I8), intent(in), optional  :: default
      integer, intent(out), optional               :: rc    
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
      integer :: iret, i 
      
      iret = 0
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if (count.le.0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "invalid SIZE", &
                                 ESMF_CONTEXT, rc)) return
      endif
       
 ! Default setting
      if( present( default ) ) then 
         valueList(1:count) = default
      else
         valueList(1:count) = 0
      endif

! Processing 
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label, rc = iret )
      end if

      do i = 1, count
         
         if(present( default )) then
            call ESMF_ConfigGetIntI8( config, valueList(i), default = default, rc = iret)
         else
            call ESMF_ConfigGetIntI8( config, valueList(i), rc = iret)
         endif
      enddo

      if(present( rc )) then
        rc = iret
      endif

      return
    end subroutine ESMF_ConfigGetIntsI8

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetLogical"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a logical value

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetLogical( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config     
      logical, intent(out)                         :: value
      character(len=*), intent(in), optional       :: label 
      logical, intent(in), optional                :: default
      integer, intent(out), optional               :: rc   

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
      integer :: iret

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
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
         call ESMF_ConfigGetString( config, string, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, string, rc = iret )
      endif

      if ( iret .eq. ESMF_SUCCESS ) then

        ! Convert string to lower case
         call ESMF_StringLowerCase(string, iret)

         ! Check if valid true/false keyword
         if (string == 't'      .or. string == 'true' .or. &
             string == '.true.' .or. string == '.t.'  .or. &
             string == 'y'      .or. string == 'yes'  .or. &
             string == 'on') then
           value = .true.
           call ESMF_ConfigSetCurrentAttrUsed(config, .true.)
         else
            if (string == 'f'       .or. string == 'false' .or. &
                string == '.false.' .or. string == '.f.'   .or. &
                string == 'n'       .or. string == 'no'    .or. &
                string == 'off') then
              value = .false.
              call ESMF_ConfigSetCurrentAttrUsed(config, .true.)
            else
              ! undo what GetSring() did
              call ESMF_ConfigSetCurrentAttrUsed(config, .false.)

              if (ESMF_LogMsgFoundError(ESMF_RC_CANNOT_GET, &
                                "bad boolean value '" // string // &
                                "' in configuration file.", &
                                 ESMF_CONTEXT, rc)) return
            endif
         endif
      else
         if( present( default )) then
            iret = ESMF_SUCCESS
         endif
      end if

      if( present( rc )) then
        rc = iret
      endif
      
      return
    end subroutine ESMF_ConfigGetLogical



#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetLogicals"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetAttribute - Get a list of logical values
!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigGetAttribute()
      subroutine ESMF_ConfigGetLogicals( config, valueList, count, label,  &
                                         default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)          :: config      
      logical, intent(inout)                       :: valueList(:)  
      integer, intent(in)                          :: count  
      character(len=*), intent(in), optional       :: label 
      logical, intent(in), optional                :: default
      integer, intent(out), optional               :: rc    
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
      integer :: iret, i 
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if (count.le.0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "invalid SIZE", &
                                 ESMF_CONTEXT, rc)) return
      endif
       
      ! Default setting
      if( present( default ) ) then 
         valueList(1:count) = default
      else
         valueList(1:count) = .false.
      endif

      ! Processing 
      if (present( label )) then
         call ESMF_ConfigFindLabel( config, label, rc = iret )
      end if

      do i = 1, count
         
         if(present( default )) then
            call ESMF_ConfigGetLogical( config, valueList(i), &
                                        default = default, rc = iret)
         else
            call ESMF_ConfigGetLogical( config, valueList(i), rc = iret)
         endif
      enddo

      if(present( rc )) then
        rc = iret
      endif

      return
    end subroutine ESMF_ConfigGetLogicals



#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetChar"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetChar - Get a character
!
! !INTERFACE:
      subroutine ESMF_ConfigGetChar( config, value, label, default, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)    :: config 
      character, intent(out)                 :: value
      character(len=*), intent(in), optional :: label   
      character, intent(in), optional        :: default
      integer, intent(out), optional         :: rc    
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
      integer :: iret

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
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
         call ESMF_ConfigGetString( config, string, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, string, rc = iret )
      endif

      if ( iret .eq. 0 ) then
         value = string(1:1)
         call ESMF_ConfigSetCurrentAttrUsed(config, .true.)
      else
         if( present( default )) then
            iret = ESMF_SUCCESS
         endif
      end if

      if (present( rc )) then
        rc = iret
      endif

      return

    end subroutine ESMF_ConfigGetChar

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetDim"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigGetDim - Get table sizes
!
! !INTERFACE:

    subroutine ESMF_ConfigGetDim( config, lineCount, columnCount, label, rc )

      implicit none

      type(ESMF_Config), intent(inout)       :: config    ! ESMF Configuration
      integer, intent(out)                   :: lineCount
      integer, intent(out)                   :: columnCount

      character(len=*), intent(in), optional :: label ! label (if present)
                                                      ! otherwise, current
                                                      ! line

      integer, intent(out), optional         :: rc     ! Error code
!
! !DESCRIPTION: 
!  Returns the number of lines in the table in {\tt lineCount} and 
!  the maximum number of words in a table line in {\tt columnCount}.
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
!     Identifying label.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP -------------------------------------------------------------------
      integer :: n, iret
      logical :: tend

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      lineCount = 0
      columnCount = 0
      
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if ( present(label) ) then
        call ESMF_ConfigFindLabel(config, label = label, rc = iret )
        if ( iret /= 0 ) then
           if ( present( rc )) then
             rc = iret
           endif
           return
        endif
      endif

      do 
         call ESMF_ConfigNextLine( config, tend, rc = iret)
         if (iret /=0 ) then
            lineCount = 0
            columnCount = 0
            exit
         endif
         if ( tend ) then
            exit
         else
            lineCount = lineCount + 1
            n = ESMF_ConfigGetLen( config, rc = iret)
            if ( iret /= 0 ) then
               lineCount = 0
               columnCount = 0
               if ( present( rc )) then
                 rc = iret
               endif
               return
            else
               columnCount = max(columnCount, n)
            endif
         endif 
      enddo
      if ( present( rc )) then
        rc = iret
      endif
      return

    end subroutine ESMF_ConfigGetDim
    
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigGetLen"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
! !IROUTINE: ESMF_ConfigGetLen - Get the length of the line in words
!
! !INTERFACE:
    integer function ESMF_ConfigGetLen( config, label, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)    :: config 
      character(len=*), intent(in), optional :: label
      integer, intent(out), optional :: rc         
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
      integer :: iret
      integer :: count 

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
      count = 0
      ESMF_ConfigGetLen = -1    ! assume error
      
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if( present( label )) then
         call ESMF_ConfigFindLabel(config, label = label, rc = iret )
         if( iret /= 0) then
            if (present( rc )) then
              rc = iret
            endif
            return
         endif
      endif

      do
         call ESMF_ConfigGetString( config, string, rc = iret )
         if ( iret .eq. 0 ) then
            count = count + 1
         else
            if (iret .eq. -1) iret  = 0  ! end of the line
            exit
         endif
      enddo
 

      ESMF_ConfigGetLen = count

      if( present ( rc )) then
        rc = iret
      endif

      return
    end function ESMF_ConfigGetLen

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigLoadFile"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigLoadFile - Load resource file into memory
!
! !INTERFACE:
    subroutine ESMF_ConfigLoadFile( config, filename, delayout, unique, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)        :: config     
      character(len=*), intent(in)               :: filename 
      type(ESMF_DELayout), intent(in), optional  :: delayout 
      logical, intent(in), optional              :: unique 
      integer, intent(out), optional             :: rc         
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

      integer :: iret

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      call ESMF_ConfigLoadFile_1proc_( config, filename, iret )
      if(iret /= 0) then
           if (ESMF_LogMsgFoundError(ESMF_RC_FILE_OPEN, &
                                "unable to load file", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call ESMF_ConfigParseAttributes( config, unique, iret )
      if(iret /= 0) then
           if (ESMF_LogMsgFoundError(iret, ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

      if ( present (delayout) ) then
         call ESMF_LogWrite("DELayout not used yet", ESMF_LOG_WARNING, &
                           ESMF_CONTEXT)
      endif

      if (present( rc )) then
        rc = iret
      endif
      return

    end subroutine ESMF_ConfigLoadFile


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigLoadFile_1proc_"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigLoadFile_1proc - Load resource file into memory
!

! !INTERFACE:

    subroutine ESMF_ConfigLoadFile_1proc_( config, filename, rc )


      implicit none

      type(ESMF_Config), intent(inout) :: config     ! ESMF Configuration
      character(len=*), intent(in)  :: filename     ! file name
      integer, intent(out), optional :: rc       ! Error code
                                                 !   0 no error
                                                 ! -98 coult not get unit 
                                                 !     number (strange!)
                                                 ! -98 talk to a wizzard
                                                 ! -99 out of memory: increase
                                                 !     NBUF_MAX 
                                                 !     other iostat from open 
                                                 !     statement.
!
! !DESCRIPTION: Resource file filename is loaded into memory
!
!EOPI -------------------------------------------------------------------
      integer :: lu, ios, loop, ls, ptr, iret
      character(len=LSZ) :: line

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

!     Open file
!     ---------     
      lu = luavail() ! a more portable version
      if ( lu .lt. 0 ) then
         iret = -97
         if ( present (rc )) then
           rc = iret
         endif
         return
      end if

      ! A open through an interface to avoid portability problems.
      ! (J.G.)

      call opntext(lu,filename,'old',ios)
      if ( ios .ne. 0 ) then
         if (ESMF_LogMsgFoundError(ESMF_RC_FILE_OPEN, &
                              "opntext() error", &
                               ESMF_CONTEXT, rc)) return
      end if

!     Read to end of file
!     -------------------
      config%cptr%buffer(1:1) = EOL
      ptr = 2                         ! next buffer position
      do loop = 1, NBUF_MAX

!        Read next line
!        --------------
         read(lu,'(a)', end=11) line  ! read next line
         call ESMF_Config_trim ( line )      ! remove trailing blanks
         call ESMF_Config_pad ( line )        ! Pad with # from end of line

!        A non-empty line
!        ----------------
         ls = index_(line,'#' ) - 1    ! line length
         if ( ls .gt. 0 ) then
            if ( (ptr+ls) .gt. NBUF_MAX ) then
               iret = -99
               if ( present (rc )) then
                 rc = iret
               endif
               return
            end if
            config%cptr%buffer(ptr:ptr+ls) = line(1:ls) // EOL
            ptr = ptr + ls + 1
         end if

      end do
      
      iret = -98 ! good chance config%cptr%buffer is not big enough 
      if ( present (rc )) then
        rc = iret
      endif

      return
      
11    continue

!     All done
!     --------
! Close lu
      call clstext(lu,ios)
      if(ios /= 0) then
         iret = -99
         if ( present (rc )) then
           rc = iret
         endif
         return
      endif
      config%cptr%buffer(ptr:ptr) = EOB
      config%cptr%nbuf = ptr
      config%cptr%this_line=' '
      config%cptr%next_line=0
      config%cptr%value_begin=0

      iret = 0
      if ( present (rc )) then
        rc = iret
      endif

      return
    end subroutine ESMF_ConfigLoadFile_1proc_
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigNextLine"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigNextLine - Find next line
!
! !INTERFACE:
    subroutine ESMF_ConfigNextLine( config, tableEnd, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout) :: config 
      logical, intent(out), optional :: tableEnd
      integer, intent(out), optional:: rc 
!
! !DESCRIPTION: 
!   Selects the next line (for tables).
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     Already created {\tt ESMF\_Config} object.
!   \item [{[tableEnd]}]
!     If specifed as {\tt TRUE}, end of table mark (::) is checked.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}

!EOP -------------------------------------------------------------------
      integer :: i, j, iret
      logical :: local_tend

      ! Initialize return code; assume routine not implemented 
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      iret = 0
      local_tend = .false.
      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      if ( config%cptr%next_line .ge. config%cptr%nbuf ) then
         iret = -1
           if ( present (rc )) then
             rc = iret
           endif
         return
      end if

      i = config%cptr%next_line
      j = i + index_(config%cptr%buffer(i:config%cptr%nbuf),EOL) - 2
      config%cptr%this_line = config%cptr%buffer(i:j) // BLK // EOL
      
      if ( config%cptr%this_line(1:2) .eq. '::' ) then
         iret = 0                    ! end of table. We set iret = 0
         local_tend = .true.         ! and end = .true. Used to be iret = 1  
         config%cptr%next_line = config%cptr%nbuf + 1
         if ( present (tableEnd )) then
           tableEnd = local_tend
         endif
         if ( present (rc )) then
           rc = iret
         endif
         return
      end if

      config%cptr%next_line = j + 2
      iret = 0
      if ( present (tableEnd )) then
        tableEnd = local_tend
      endif
      if ( present (rc )) then
        rc = iret
      endif
      return

    end subroutine ESMF_ConfigNextLine
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigParseAttributes"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigParseAttributes - Parse all attribute labels
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
                  if (trim(label) .eq. trim(config%cptr%attr_used(b)%label)) then
                    duplicate = .true.
                    logmsg = "Duplicate label '" // trim(label) // &
                                  "' found in attributes file"
                    call ESMF_LogMsgSetError(ESMF_RC_DUP_NAME, logmsg, &
                                             ESMF_CONTEXT, rc)
                    localrc = ESMF_RC_DUP_NAME
                  endif
                enddo
              endif
            endif

            ! ... and place it into attributes table
            if (.not.duplicate) then
               if ( a <= NATT_MAX ) then
                  config%cptr%attr_used(a)%label = label
               else
                  if (ESMF_LogMsgFoundError(ESMF_RC_INTNRL_LIST,    &
                       "attribute out-of-range; increase NATT_MAX", &
                       ESMF_CONTEXT, rc)) return
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

!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigSetAttribute - Set a value
!
!
! !INTERFACE:
!     subroutine ESMF_ConfigSetAttribute( config, <value argument>, &
!                                         label, rc )
!
! !ARGUMENTS:
!     type(ESMF_Config), intent(inout)             :: config     
!     <value argument>, see below for supported values
!     character(len=*), intent(in), optional       :: label 
!     integer, intent(out), optional               :: rc   
!
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


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigSetIntI4"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigSetAttribute - Set a 4-byte integer number

!
! !INTERFACE:
      ! Private name; call using ESMF_ConfigSetAttribute()
      subroutine ESMF_ConfigSetIntI4( config, value, label, rc )

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)             :: config     
      integer(ESMF_KIND_I4), intent(in)            :: value
      character(len=*), intent(in), optional       :: label 
      integer, intent(out), optional               :: rc   

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
      character(len=ESMF_MAXSTR) :: logmsg
      character(len=LSZ) :: curVal, newVal
      integer :: iret, i, j, k, m, nchar, ninsert, ndelete, lenThisLine

      ! Initialize return code; assume routine not implemented
      iret = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      !check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ConfigGetInit,config,rc)

      ! Set config buffer at desired attribute
      if ( present (label) ) then
         call ESMF_ConfigGetString( config, curVal, label, rc = iret )
      else
         call ESMF_ConfigGetString( config, curVal, rc = iret )
      endif

      if ( iret .ne. ESMF_SUCCESS ) then
        if ( iret .eq. ESMF_RC_NOT_FOUND ) then
          ! set config buffer at end for appending
          i = config%cptr%nbuf
        else
          if ( present( rc ) ) then
            rc = iret
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
           if (ESMF_LogMsgFoundError(ESMC_RC_LONG_STR, logmsg, &
                                     ESMF_CONTEXT, rc)) return
        endif

        ! check if enough space left in config buffer
        if (j .ge. NBUF_MAX) then   ! room for EOB if necessary
           write(logmsg, *) ", attribute label & value require ", j-i+1, &
               " characters (including EOL & EOB), only ", NBUF_MAX-i, &
               " characters left in config buffer"
           if (ESMF_LogMsgFoundError(ESMC_RC_LONG_STR, logmsg, &
                                     ESMF_CONTEXT, rc)) return
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
               if (ESMF_LogMsgFoundError(ESMC_RC_LONG_STR, logmsg, &
                                         ESMF_CONTEXT, rc)) return
            endif

            ! check if enough space left in config buffer to extend line
            if (j+1 .ge. NBUF_MAX) then   ! room for EOB if necessary
               write(logmsg, *) ", attribute label & value require ", j-m+1, &
                   " characters (including EOL & EOB), only ", NBUF_MAX-i, &
                   " characters left in config buffer"
               if (ESMF_LogMsgFoundError(ESMC_RC_LONG_STR, logmsg, &
                                         ESMF_CONTEXT, rc)) return
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
        rc = iret
      endif
      
      return
    end subroutine ESMF_ConfigSetIntI4

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigSetCurrentAttrUsed"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOPI -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigSetCurrentAttrUsed - Set Current Attribute "Used" flag
!
! !INTERFACE:

    subroutine ESMF_ConfigSetCurrentAttrUsed( config, used, rc )


      implicit none

      type(ESMF_Config), intent(inout) :: config ! ESMF Configuration
      logical, intent(in)            :: used     ! used flag
      integer, intent(out), optional :: rc       ! Error return code
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
        if (trim(config%cptr%current_attr) == trim(config%cptr%attr_used(i)%label)) then
          config%cptr%attr_used(i)%used = used
          exit
        endif
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      return

    end subroutine ESMF_ConfigSetCurrentAttrUsed

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConfigValidate"
!-----------------------------------------------------------------------
! Earth System Modeling Framework
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ESMF_ConfigValidate - Validate a Config object
!
! !INTERFACE:
    subroutine ESMF_ConfigValidate(config, options, rc)

! !ARGUMENTS:
      type(ESMF_Config), intent(inout)         :: config 
      character (len=*), intent(in),  optional :: options
      integer, intent(out), optional           :: rc 
!
! !DESCRIPTION: 
!   Checks whether a {\tt config} object is valid.
!
!   The arguments are:
!   \begin{description}
!   \item [config]
!     {\tt ESMF\_Config} object to be validated.
!   \item[{[options]}]
!     If none specified:  simply check that the buffer is not full and the
!       pointers are within range.
!     "unusedAttributes" - Report to the default logfile all attributes not
!       retrieved via a call to {\tt ESMF\_ConfigGetAttribute()} or
!       {\tt ESMF\_ConfigGetChar()}.  The attribute name (label) will be
!       logged via {\tt ESMF\_LogErr} with the WARNING log message type.
!       For an array-valued attribute, retrieving at least one value via
!       {\tt ESMF\_ConfigGetAttribute()} or {\tt ESMF\_ConfigGetChar()}
!       constitutes being "used."
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
        if (ESMF_LogMsgFoundError(ESMF_RC_INTNRL_LIST, &
                                  "config%cptr%nbuf out-of-range.", &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (config%cptr%next_line < 0 .or. config%cptr%next_line >= config%cptr%nbuf) then
        if (ESMF_LogMsgFoundError(ESMF_RC_INTNRL_LIST, &
                                  "config%cptr%next_line out-of-range.", &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (config%cptr%nattr < 0 .or. config%cptr%nattr > NATT_MAX) then
        if (ESMF_LogMsgFoundError(ESMF_RC_INTNRL_LIST, &
                                  "config%cptr%nattr out-of-range.", &
                                  ESMF_CONTEXT, rc)) return
      endif

      ! optional validations

      if (present(options)) then
        if (options == "unusedAttributes") then
          do i = 1, config%cptr%nattr
            if (.not.(config%cptr%attr_used(i)%used)) then
              logmsg = "Config attribute label '" // &
                  trim(config%cptr%attr_used(i)%label) // &
                  "' unused (not retrieved via ESMF_ConfigGetAttribute() " // &
                  "or ESMF_ConfigGetChar())."
              call ESMF_LogWrite(logmsg, ESMF_LOG_WARNING, ESMF_CONTEXT)
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


!-----------------------------------------------------------------------


      integer function index_ (string,tok)

      implicit NONE

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
      return
      end function index_

      subroutine ESMF_Config_Trim ( string )

      implicit NONE


!-------------------------------------------------------------------------
!
! !ROUTINE:  ESMF_Config_Trim() - Removes leading blanks from strings.
!
! !DESCRIPTION: 
!
!    Removes blanks and TABS from begenning of string. 
!    This is a low level i90 routine.
! 
! !CALLING SEQUENCE: 
!
!     call ESMF_Config_Trim ( string )
!
! !INPUT PARAMETERS: 
!
      character*256 string    ! the input string
!
! !OUTPUT PARAMETERS:
!
!     character*256 string    ! the modified string
!
!
!-------------------------------------------------------------------------

      integer :: ib, i

!     Get rid of leading blanks
!     -------------------------
      ib = 1
      do i = 1, 255
         if ( string(i:i) .ne. ' ' .and. &
            string(i:i) .ne. TAB ) go to 21
         ib = ib + 1
      end do
 21   continue

!     String without trailling blanks
!     -------------------------------
      string = string(ib:)

      return
      end subroutine ESMF_Config_trim


      subroutine ESMF_Config_pad ( string )

      implicit NONE


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
       character*256 string       ! input string

! !OUTPUT PARAMETERS:            ! modified string
!
!      character*256 string
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
      do i = 256, 1, -1 
         if ( string(i:i) .ne. ' ' .and. &
            string(i:i) .ne. '$' ) go to 11
         string(i:i) = '#'
      end do
 11   continue

!     Replace TAB's with blanks
!     -------------------------
      do i = 1, 256
         if ( string(i:i) .eq. TAB ) string(i:i) = BLK
         if ( string(i:i) .eq. '#' ) go to 21
      end do
 21   continue

      return
      end subroutine ESMF_Config_pad

    




!-----------------------------------------------------------------------
!
! !IROUTINE: luavail - locate the next available unit
!
! !DESCRIPTION:
!
!    luavail() Look for an available (not opened and not statically
!    assigned to any I/O attributes to) logical unit.
!
! !INTERFACE:

        function luavail()
          !!!use m_stdio_Config
          implicit none
          integer :: luavail ! result

!-----------------------------------------------------------------------

        integer :: lu,ios
        logical :: inuse
#ifdef _UNICOS
        character(len=8) :: attr
#endif

        lu=-1
        ios=0
        inuse=.true.

        do while(ios.eq.0.and.inuse)
          lu=lu+1

                ! Test #1, reserved

          inuse = lu.eq.stdout .or. lu.eq.stdin .or. lu.eq.stderr

#ifdef sysSunOS
                ! Reserved units under SunOS
          inuse = lu.eq.100 .or. lu.eq.101 .or. lu.eq.102
#endif

                ! Test #2, in-use

          if(.not.inuse) inquire(unit=lu,opened=inuse,iostat=ios)

#ifdef _UNICOS
                ! Test #3, if the user has reserved the unit through
                ! UNICOS' assign().

          if(ios.eq.0 .and. .not.inuse) then
            call asnqunit(lu,attr,ios)

                ! see asnqunig(3f):
                !
                ! ios ==  0, has been assigned to some attributes
                !        -1, not been assigned any attributes
                !     >   0, an error condition, but who cares why.

            inuse=ios.ne.-1  ! the unit is in-use
            if(ios .ge. -1) ios=0  ! still a valid test
          endif
#endif

          if(lu .ge. MX_LU) ios=-1
        end do

        if(ios.ne.0) lu=-1
        luavail=lu
end function luavail



!-----------------------------------------------------------------------
! !IROUTINE: opntext - portablly open a text file
!
! !DESCRIPTION:
!
!       Open a text (ASCII) file.  Under FORTRAN, it is defined as
!       "formatted" with "sequential" access.
!
! !INTERFACE:

    subroutine opntext(lu,filename,status,ier)
      implicit none

      integer,         intent(in) :: lu     ! logical unit number
      character(len=*),intent(in) :: filename  ! filename to be opended
      character(len=*),intent(in) :: status ! the value for STATUS=<>
      integer,         intent(out):: ier    ! the status

!-----------------------------------------------------------------------
!

                ! local parameter

        integer,parameter :: iA=ichar('a')
        integer,parameter :: mA=ichar('A')
        integer,parameter :: iZ=ichar('z')

        character(len=len(status)) :: Ustat
        integer :: i,ic

#ifdef _UNICOS
        call asnunit(lu,'-R',ier)        ! remove any set attributes
        if(ier.ne.0) return                ! let the parent handle it
#endif

        do i=1,len(status)
          ic=ichar(status(i:i))
          if(ic .ge. iA .and. ic .le. iZ) ic=ic+(mA-iA)
          Ustat(i:i)=char(ic)
        end do

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
            iostat      =ier                )

        case default

          open(                           &
            unit        =lu,              &
            file        =filename,        &
            form        ='formatted',     &
            access      ='sequential',    &
            status      =status,          &
            action      ='read',          &
            position    ='asis',          &
            iostat      =ier                )

        end select

        end subroutine opntext


!-----------------------------------------------------------------------
!
! !IROUTINE: clstext - close a text file opend with an opntext() call
!
! !DESCRIPTION:

! !INTERFACE:

    subroutine clstext(lu,ier,status)
      implicit none

      integer,                    intent(in)  :: lu     ! a logical unit to close
      integer,                    intent(out) :: ier    ! the status
      Character(len=*), optional, intent(In)  :: status ! keep/delete

!-----------------------------------------------------------------------
          character(len=6) :: status_

          status_ = 'KEEP'
          If (Present(status)) Then
             Select Case (Trim(status))
             Case ('DELETE','delete')
                status_ = 'DELETE'
             Case  ('KEEP','keep')
                status_ = 'KEEP'
             Case Default
                ier = -997
                return
             End Select
          End If

        close(lu,iostat=ier,status=status_)
#ifdef _UNICOS
        if(ier .eq. 0) call asnunit(lu,'-R',ier) ! remove any attributes
#endif

        end subroutine clstext


!-----------------------------------------------------------------------
    end module ESMF_ConfigMod
