! $Id: ESMF_Grid.F90,v 1.3 2002/11/01 17:56:39 jwolfe Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF Grid Module
!
! (all lines below between the !BOP and !EOP markers will be included in 
!  the automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! put any constants or macros which apply to the whole component in this 
!  include file.  anything public or esmf-wide should be up higher at
!  the top level include files.

#include <ESMF_Grid.h>


!------------------------------------------------------------------------------
! module definition

      module ESMF_GridMod

!BOP
! !MODULE: ESMF_GridMod - one line general statement about this class
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
      use ESMF_Base    ! ESMF base class
!     use ESMF_XXXMod  < if needed >
      implicit none
!
! !PRIVATE TYPES:
      private

      type ESMF_GridConfig
      private
      sequence
!       < insert resource items here >
      end type

      type ESMF_Grid
      private
      sequence
        type (ESMF_Base) :: base
        integer :: basestate
!       < insert other class members here >
      end type

! !PUBLIC TYPES:
      public ESMF_GridConfig
      public ESMF_Grid


! !PUBLIC MEMBER FUNCTIONS:
!
! pick one or the other of the init/create sections depending on
!  whether this is a deep class (the class/derived type has pointers to
!  other memory which must be allocated/deallocated) or a shallow class
!  (the class/derived type is self-contained) and needs no destroy methods
!  other than deleting the memory for the object/derived type itself.

! the following routines apply to deep classes only
    public ESMF_GridCreate                 ! (interface only, deep class)
    public ESMF_GridDestroy                ! (interface only, deep class)
    public ESMF_GridConstruct              ! (internal only, deep class)
    public ESMF_GridDestruct               ! (internal only, deep class)

! the following routine applies to a shallow class
    public ESMF_GridInit                   ! (shallow class)

    public ESMF_GridGetconfig
    public ESMF_GridSetconfig
    public ESMF_GridGet<Value>
    public ESMF_GridSet<Value>
 
    public ESMF_GridValidate
    public ESMF_GridPrint
 
! < list the rest of the public interfaces here >
!
!
!EOP


!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = '$Id: ESMF_Grid.F90,v 1.3 2002/11/01 17:56:39 jwolfe Exp $
!------------------------------------------------------------------------------

! interface blocks for functions which are going to have a single
! name for ease-of-use, but internally will be implemented as separate
! subprograms.  the non-optional parts of the argument lists must be 
! distinguishable for this to work.  the following example is appropriate
! for deep classes; shallow objects will only have init routines and
! no creates.
!
!BOP
! !IROUTINE: ESMF_GridCreate - Generic interface to create a new Grid object

! !INTERFACE:
      interface ESMF_GridCreate 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridCreateNew
         module procedure ESMF_GridCreateCopy
         module procedure ESMF_GridCreateRemap

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
! This section includes all the Grid routines
!
!

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateNew - Create a new Grid

! !INTERFACE:
      function ESMF_GridCreateNew(arg1, arg2, arg3, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!   Create a new Grid from ... Allocates memory for a new Grid
!   object and uses the internal routine ESMF_GridContruct to
!   initialize it.  Define for deep classes only, for shallow classes only
!   define and use ESMF_GridInit
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
      end function ESMF_GridCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridDestroy - free a Grid created with Create

! !INTERFACE:
      subroutine ESMF_GridDestroy(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid   ! grid to be destroyed
      integer, intent(out), optional :: rc        ! return code
!
! !DESCRIPTION:
!   ESMF routine which destroys a Grid object previously allocated
!   via an ESMF_GridCreate routine.  Define for deep classes only.
!
!   The arguments are:
!   \begin{description}
!   \item[grid] The class to be destroyed.
!   \item[rc] The optional return code.
!   \end{description}
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_GridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridConstruct - fill in an already allocated Grid

! !INTERFACE:
      subroutine ESMF_GridConstruct(grid, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid   ! grid to be initialized
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      ESMF routine which fills in the contents of an already
!      allocated Grid object.  May need to do additional allocations
!      as needed.  Must call the corresponding ESMF_GridDestruct
!      routine to free the additional memory.  Intended for internal
!      ESMF use only; end-users use ESMF_GridCreate, which calls
!      ESMF_GridConstruct.  Define for deep classes only.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_GridConstruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridDestruct - release resources associated w/a Grid

! !INTERFACE:
      subroutine ESMF_GridDestruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid     ! Grid to be dismantled
      integer, intent(out), optional :: rc          ! return code

!
! !DESCRIPTION:
!      ESMF routine which deallocates any space allocated by
!      ESMF_GridConstruct, does any additional cleanup before the
!      original Grid object is freed.  Intended for internal ESMF
!      use only; end-users use ESMF_GridDestroy, which calls
!      ESMF_GridDestruct.  Define for deep classes only.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_GridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridInit - initialize a Grid object

! !INTERFACE:
      subroutine ESMF_GridInit(grid, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid   ! Grid to be initialized
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!      ESMF routine which only initializes Grid values; it does not
!      allocate any resources.  Define for shallow classes only, 
!      for deep classes define and use routines Create/Destroy and 
!      Construct/Destruct.  Can be overloaded like ESMF_GridCreate
!      via interface blocks.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_GridInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetConfig - get configuration information from a Grid

! !INTERFACE:
      subroutine ESMF_GridGetConfig(grid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_GridConfig), intent(out) :: config    ! resources
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Returns the set of resources the Grid object was configured with.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_GridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetConfig - set configuration information for a Grid

! !INTERFACE:
      subroutine ESMF_GridSetConfig(grid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_GridConfig), intent(in) :: config    ! resources
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Configures the Grid object with set of resources given.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_GridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGet<Value> - get <Value> for a Grid

! !INTERFACE:
      subroutine ESMF_GridGet<Value>(grid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      <value type>, intent(out) :: value
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Returns the value of Grid member <Value>.
!     Can be multiple routines, one per value
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_GridGet<Value>

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSet<Value> - set <Value> for a Grid

! !INTERFACE:
      subroutine ESMF_GridSet<Value>(grid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      <value type>, intent(in) :: value
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Sets the Grid member <Value> with the given value.
!     Can be multiple routines, one per value
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_GridSet<Value>

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridValidate - internal consistency check for a Grid

! !INTERFACE:
      subroutine ESMF_GridValidate(grid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid        ! grid to be checked
      character (len=*), intent(in) :: options         ! validate options
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Validates that a Grid is internally consistent.
!      Returns error code if problems are found.  ESMF_Base class
!      method.
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

!
!  code goes here
!
      end function ESMF_GridValidate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridPrint - print contents of a Grid

! !INTERFACE:
      subroutine ESMF_GridPrint(grid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid        ! grid to be printed
      character (len=*), intent(in) :: options         ! print options
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Print information about a Grid.  The options control the
!      type of information and level of detail.  ESMF_Base class
!      method.
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!  code goes here
!
      end subroutine ESMF_GridPrint

!------------------------------------------------------------------------------

      end module ESMF_GridMod
