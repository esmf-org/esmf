! $Id: ESMF_PhysGrid.F90,v 1.4 2002/11/03 19:43:00 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF PhysGrid Module
!
! (all lines below between the !BOP and !EOP markers will be included in 
!  the automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! put any constants or macros which apply to the whole component in this 
!  include file.  anything public or esmf-wide should be up higher at
!  the top level include files.

#include <ESMF_PhysGrid.h>


!------------------------------------------------------------------------------
! module definition

      module ESMF_PhysGridMod

!BOP
! !MODULE: ESMF_PhysGridMod - one line general statement about this class
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
!     use ESMF_Base    ! ESMF base class
!     use ESMF_XXXMod  < if needed >
      implicit none
!
! !PRIVATE TYPES:
      private

!      type ESMF_PhysGridConfig
!      private
!      sequence
!       < insert resource items here >
!      end type

      type ESMF_PhysGrid
      private
      sequence
!        type (ESMF_Base) :: base
         integer :: basestate
!       < insert other class members here >
      end type

! !PUBLIC TYPES:
!      public ESMF_PhysGridConfig
      public ESMF_PhysGrid

! !PUBLIC MEMBER FUNCTIONS:
!
! pick one or the other of the init/create sections depending on
!  whether this is a deep class (the class/derived type has pointers to
!  other memory which must be allocated/deallocated) or a shallow class
!  (the class/derived type is self-contained) and needs no destroy methods
!  other than deleting the memory for the object/derived type itself.

! the following routines apply to deep classes only
    public ESMF_PhysGridCreate                 ! (interface only, deep class)
    public ESMF_PhysGridDestroy                ! (interface only, deep class)
    public ESMF_PhysGridConstruct              ! (internal only, deep class)
    public ESMF_PhysGridDestruct               ! (internal only, deep class)

! the following routine applies to a shallow class
    public ESMF_PhysGridInit                   ! (shallow class)

    public ESMF_PhysGridGetConfig
    public ESMF_PhysGridSetConfig
    public ESMF_PhysGridGet
    public ESMF_PhysGridSet
 
    public ESMF_PhysGridValidate
    public ESMF_PhysGridPrint
 
! < list the rest of the public interfaces here >
!
!
!EOP


!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: &
      version = '$Id: ESMF_PhysGrid.F90,v 1.4 2002/11/03 19:43:00 cdeluca Exp $'
!------------------------------------------------------------------------------
!
!BOP
! !IROUTINE: ESMF_PhysGridCreate - Generic interface to create a new PhysGrid object

! !INTERFACE:
      interface ESMF_PhysGridCreate 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_PhysGridCreateNew
!         module procedure ESMF_PhysGridCreateCopy
!         module procedure ESMF_PhysGridCreateRemap

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
! This section includes all the PhysGrid routines
!
!

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridCreateNew - Create a new PhysGrid

! !INTERFACE:
      function ESMF_PhysGridCreateNew(arg1, arg2, arg3, rc)
!
! !RETURN VALUE:
      type(ESMF_PhysGrid) :: ESMF_PhysGridCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!   Create a new PhysGrid from ... Allocates memory for a new PhysGrid
!   object and uses the internal routine ESMF_PhysGridContruct to
!   initialize it.  Define for deep classes only, for shallow classes only
!   define and use ESMF_PhysGridInit
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
      end function ESMF_PhysGridCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridDestroy - free a PhysGrid created with Create

! !INTERFACE:
      subroutine ESMF_PhysGridDestroy(physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid   ! physgrid to be destroyed
      integer, intent(out), optional :: rc        ! return code
!
! !DESCRIPTION:
!   ESMF routine which destroys a PhysGrid object previously allocated
!   via an ESMF_PhysGridCreate routine.  Define for deep classes only.
!
!   The arguments are:
!   \begin{description}
!   \item[physgrid] The class to be destroyed.
!   \item[rc] The optional return code.
!   \end{description}
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_PhysGridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridConstruct - fill in an already allocated PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridConstruct(physgrid, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid   ! physgrid to be initialized
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      ESMF routine which fills in the contents of an already
!      allocated PhysGrid object.  May need to do additional allocations
!      as needed.  Must call the corresponding ESMF_PhysGridDestruct
!      routine to free the additional memory.  Intended for internal
!      ESMF use only; end-users use ESMF_PhysGridCreate, which calls
!      ESMF_PhysGridConstruct.  Define for deep classes only.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_PhysGridConstruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridDestruct - release resources associated w/a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridDestruct(physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid   ! PhysGrid to be dismantled
      integer, intent(out), optional :: rc          ! return code
!
! !DESCRIPTION:
!      ESMF routine which deallocates any space allocated by
!      ESMF_PhysGridConstruct, does any additional cleanup before the
!      original PhysGrid object is freed.  Intended for internal ESMF
!      use only; end-users use ESMF_PhysGridDestroy, which calls
!      ESMF_PhysGridDestruct.  Define for deep classes only.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_PhysGridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridInit - initialize a PhysGrid object

! !INTERFACE:
      subroutine ESMF_PhysGridInit(physgrid, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid   ! PhysGrid to be initialized
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!      ESMF routine which only initializes PhysGrid values; it does not
!      allocate any resources.  Define for shallow classes only, 
!      for deep classes define and use routines Create/Destroy and 
!      Construct/Destruct.  Can be overloaded like ESMF_PhysGridCreate
!      via interface blocks.
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_PhysGridInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetConfig - get configuration information from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetConfig(physgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(out) :: config    ! resources
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Returns the set of resources the PhysGrid object was configured with.
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_PhysGridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetConfig - set configuration information for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridSetConfig(physgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(in) :: config    ! resources
      integer, intent(out), optional :: rc         

!
! !DESCRIPTION:
!     Configures the PhysGrid object with set of resources given.
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_PhysGridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGet - get <Value> for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGet(physgrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(out) :: value          ! change integer to value data type
      integer, intent(out), optional :: rc   ! return code

!
! !DESCRIPTION:
!     Returns the value of PhysGrid member <Value>.
!     Can be multiple routines, one per value
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_PhysGridGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSet - set <Value> for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridSet(physgrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(in) :: value         ! change integer to value data type
      integer, intent(out), optional :: rc ! return code

!
! !DESCRIPTION:
!     Sets the PhysGrid member <Value> with the given value.
!     Can be multiple routines, one per value
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_PhysGridSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridValidate - internal consistency check for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridValidate(physgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid    ! physgrid to be checked
      character (len=*), intent(in) :: opt           ! validate options
      integer, intent(out), optional :: rc           ! return code
!
! !DESCRIPTION:
!      Validates that a PhysGrid is internally consistent.
!      Returns error code if problems are found.  ESMF_Base class
!      method.
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

!
!  code goes here
!
      end subroutine ESMF_PhysGridValidate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridPrint - print contents of a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridPrint(physgrid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid      ! physgrid to be printed
      character (len=*), intent(in) :: options         ! print options
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Print information about a PhysGrid.  The options control the
!      type of information and level of detail.  ESMF_Base class
!      method.
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!  code goes here
!
      end subroutine ESMF_PhysGridPrint

!------------------------------------------------------------------------------

      end module ESMF_PhysGridMod
