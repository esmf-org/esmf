! $Id: ESMF_Regrid.F90,v 1.2 2002/11/03 19:43:00 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF Regrid Module
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! put any constants or macros which apply to the whole component in this 
!  include file.  anything public or esmf-wide should be up higher at
!  the top level include files.

#include <ESMF_Regrid.h>


!------------------------------------------------------------------------------
! module definition

      module ESMF_RegridMod

!BOP
! !MODULE: ESMF_RegridMod - one line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements ...
!
! < insert a paragraph or two explaining what you'll find in this file >
!
!
!
!------------------------------------------------------------------------------
! !USES:
!      use ESMF_Base    ! ESMF base class
!     use ESMF_XXXMod  < if needed >
      implicit none
!
! !PRIVATE TYPES:
      private

!      type ESMF_RegridConfig
!      private
!      sequence
!       < insert resource items here >
!      end type

      type ESMF_Regrid
      private
      sequence
!        type (ESMF_Base) :: base
        integer :: basestate
!       < insert other class members here >
      end type

! !PUBLIC TYPES:
!      public ESMF_RegridConfig
      public ESMF_Regrid


! !PUBLIC MEMBER FUNCTIONS:
!
! pick one or the other of the init/create sections depending on
!  whether this is a deep class (the class/derived type has pointers to
!  other memory which must be allocated/deallocated) or a shallow class
!  (the class/derived type is self-contained) and needs no destroy methods
!  other than deleting the memory for the object/derived type itself.

! the following routines apply to deep classes only
    public ESMF_RegridCreate                 ! (interface only, deep class)
    public ESMF_RegridDestroy                ! (interface only, deep class)
    public ESMF_RegridConstruct              ! (internal only, deep class)
    public ESMF_RegridDestruct               ! (internal only, deep class)

! the following routine applies to a shallow class
    public ESMF_RegridInit                   ! (shallow class)

    public ESMF_RegridGetconfig
    public ESMF_RegridSetconfig
    public ESMF_RegridGet
    public ESMF_RegridSet
 
    public ESMF_RegridValidate
    public ESMF_RegridPrint
 
! < list the rest of the public interfaces here >
!
!
!EOP


!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: &
      version = '$Id: ESMF_Regrid.F90,v 1.2 2002/11/03 19:43:00 cdeluca Exp $'
!------------------------------------------------------------------------------
!
!BOP
! !IROUTINE: ESMF_RegridCreate - Generic interface to create a new Regrid object

! !INTERFACE:
      interface ESMF_RegridCreate 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridCreateNew

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of XXX subprogram. ...
!EOP

      end interface 

! < add other interfaces here>

!------------------------------------------------------------------------------

      contains

!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all the Regrid routines
!
!

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridCreateNew - Create a new Regrid

! !INTERFACE:
      function ESMF_RegridCreateNew(arg1, arg2, arg3, rc)
!
! !RETURN VALUE:
      type(ESMF_Regrid) :: ESMF_RegridCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!   Create a new Regrid from ... Allocates memory for a new Regrid
!   object and uses the internal routine ESMF_RegridContruct to
!   initialize it.  Define for deep classes only, for shallow classes only
!   define and use ESMF_RegridInit
!
!   The arguments are:
!   \begin{description}
!   \item[arg1] The arg is  ...
!   \item[arg2] The arg is  ...
!   \item[arg3] The arg is  ...
!   \item[rc] The optional return code.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

!
!  code goes here
!
      end function ESMF_RegridCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridDestroy - free a Regrid created with Create

! !INTERFACE:
      subroutine ESMF_RegridDestroy(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid   ! regrid to be destroyed
      integer, intent(out), optional :: rc        ! return code
!
! !DESCRIPTION:
!   ESMF routine which destroys a Regrid object previously allocated
!   via an ESMF_RegridCreate routine.  Define for deep classes only.
!
!   The arguments are:
!   \begin{description}
!   \item[regrid] The class to be destroyed.
!   \item[rc] The optional return code.
!   \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_RegridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridConstruct - fill in an already allocated Regrid

! !INTERFACE:
      subroutine ESMF_RegridConstruct(regrid, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid   ! regrid to be initialized
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      ESMF routine which fills in the contents of an already
!      allocated Regrid object.  May need to do additional allocations
!      as needed.  Must call the corresponding ESMF_RegridDestruct
!      routine to free the additional memory.  Intended for internal
!      ESMF use only; end-users use ESMF_RegridCreate, which calls
!      ESMF_RegridConstruct.  Define for deep classes only.
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_RegridConstruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridDestruct - Release resources associated with Regrid

! !INTERFACE:
      subroutine ESMF_RegridDestruct(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid     ! Regrid to be dismantled
      integer, intent(out), optional :: rc          ! return code

!
! !DESCRIPTION:
!      ESMF routine which deallocates any space allocated by
!      ESMF_RegridConstruct, does any additional cleanup before the
!      original Regrid object is freed.  Intended for internal ESMF
!      use only; end-users use ESMF_RegridDestroy, which calls
!      ESMF_RegridDestruct.  Define for deep classes only.
!
!EOP
! !REQUIREMENTS

!
!  code goes here
!
      end subroutine ESMF_RegridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridInit - Initialize a Regrid object

! !INTERFACE:
      subroutine ESMF_RegridInit(regrid, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid   ! Regrid to be initialized
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!      ESMF routine which only initializes Regrid values; it does not
!      allocate any resources.  Define for shallow classes only, 
!      for deep classes define and use routines Create/Destroy and 
!      Construct/Destruct.  Can be overloaded like ESMF_RegridCreate
!      via interface blocks.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_RegridInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridGetConfig - Get configuration information from a Regrid

! !INTERFACE:
      subroutine ESMF_RegridGetConfig(regrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(out) :: config    ! resources
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Returns the set of resources the Regrid object was configured with.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_RegridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridSetConfig - Set configuration information for a Regrid

! !INTERFACE:
      subroutine ESMF_RegridSetConfig(regrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(in) :: config    ! resources
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Configures the Regrid object with set of resources given.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_RegridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridGet - get <Value> for a Regrid

! !INTERFACE:
      subroutine ESMF_RegridGet(regrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(out) :: value
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Returns the value of Regrid member <Value>.
!     Can be multiple routines, one per value
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_RegridGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridSet - set <Value> for a Regrid

! !INTERFACE:
      subroutine ESMF_RegridSet(regrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid
      integer, intent(in) :: value
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Sets the Regrid member <Value> with the given value.
!     Can be multiple routines, one per value
!
!EOP
! !REQUIREMENTS: 
!
!  code goes here
!
      end subroutine ESMF_RegridSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridValidate - internal consistency check for a Regrid

! !INTERFACE:
      subroutine ESMF_RegridValidate(regrid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid        ! regrid to be checked
      character (len=*), intent(in) :: options         ! validate options
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Validates that a Regrid is internally consistent.
!      Returns error code if problems are found.  ESMF_Base class
!      method.
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

!
!  code goes here
!
      end subroutine ESMF_RegridValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridPrint - print contents of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridPrint(regrid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(in) :: regrid        ! regrid to be printed
      character (len=*), intent(in) :: options         ! print options
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Print information about a Regrid.  The options control the
!      type of information and level of detail.  ESMF_Base class
!      method.
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!  code goes here
!
      end subroutine ESMF_RegridPrint

!------------------------------------------------------------------------------

      end module ESMF_RegridMod
