! $Id: ESMF_DistGrid.F90,v 1.3 2002/11/01 18:40:49 jwolfe Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF DistGrid Module
!
! (all lines below between the !BOP and !EOP markers will be included in 
!  the automated document processing.)
!------------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the top level DistGrid defined type
!  and functions which operate on DistGrids.  A corresponding
!  C++ interface exists in the companion ../interface directory.
!
!  < more here about what's in a DistGrid >
!
!
!  See the ESMF Grid design document for more details.
!
!------------------------------------------------------------------------------
! put any constants or macros which apply to the whole component in this 
!  include file.  anything public or esmf-wide should be up higher at
!  the top level include files.

#include <ESMF_DistGrid.h>


!------------------------------------------------------------------------------
! module definition

      module ESMF_DistGridMod

!BOP
! !MODULE: ESMF_DistGridMod - one line general statement about this class
!
! !DESCRIPTION:
!
!  This section contains the basic defined type for the decomposition of
!  a single grid into multiple subgrids for parallel processing, and the
!  internal subroutines and functions which operate on them.
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

      type ESMF_DistGridConfig
      private
      sequence
!       < insert resource items here >
      end type

      type ESMF_DistGrid
      private
      sequence
        type (ESMF_Base) :: base
        integer :: basestate
!       < insert other class members here >
      end type

! !PUBLIC TYPES:
      public ESMF_DistGridConfig
      public ESMF_DistGrid


! !PUBLIC MEMBER FUNCTIONS:
!
! pick one or the other of the init/create sections depending on
!  whether this is a deep class (the class/derived type has pointers to
!  other memory which must be allocated/deallocated) or a shallow class
!  (the class/derived type is self-contained) and needs no destroy methods
!  other than deleting the memory for the object/derived type itself.

! the following routines apply to deep classes only
    public ESMF_DistGridCreate                 ! (interface only, deep class)
    public ESMF_DistGridDestroy                ! (interface only, deep class)
    public ESMF_DistGridConstruct              ! (internal only, deep class)
    public ESMF_DistGridDestruct               ! (internal only, deep class)

! the following routine applies to a shallow class
    public ESMF_DistGridInit                   ! (shallow class)

    public ESMF_DistGridGetconfig
    public ESMF_DistGridSetconfig
    public ESMF_DistGridGet<Value>
    public ESMF_DistGridSet<Value>
 
    public ESMF_DistGridValidate
    public ESMF_DistGridPrint
 
! < list the rest of the public interfaces here >
!
!
!EOP


!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = '$Id: ESMF_DistGrid.F90,v 1.3 2002/11/01 18:40:49 jwolfe Exp $
!------------------------------------------------------------------------------

! interface blocks for functions which are going to have a single
! name for ease-of-use, but internally will be implemented as separate
! subprograms.  the non-optional parts of the argument lists must be 
! distinguishable for this to work.  the following example is appropriate
! for deep classes; shallow objects will only have init routines and
! no creates.
!
!BOP
! !IROUTINE: ESMF_DistGridCreate - Generic interface to create a new DistGrid object

! !INTERFACE:
      interface ESMF_DistGridCreate 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridCreateNew
         module procedure ESMF_DistGridCreateCopy
         module procedure ESMF_DistGridCreateRemap

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of XXX subprogram. ...
!      Returns rc=OK if the grid was created without error.
!EOP

      end interface 

! < add other interfaces here>

!------------------------------------------------------------------------------

      contains

!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all the DistGrid routines
!
!

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridCreateNew - Create a new DistGrid

! !INTERFACE:
      function ESMF_DistGridCreateNew(arg1, arg2, arg3, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!   Create a new DistGrid from ... Allocates memory for a new DistGrid
!   object and uses the internal routine ESMF_DistGridContruct to
!   initialize it.  Define for deep classes only, for shallow classes only
!   define and use ESMF_DistGridInit
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
      end function ESMF_DistGridCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridDestroy - free a DistGrid created with Create

! !INTERFACE:
      subroutine ESMF_DistGridDestroy(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid   ! distgrid to be destroyed
      integer, intent(out), optional :: rc        ! return code
!
! !DESCRIPTION:
!   ESMF routine which destroys a DistGrid object previously allocated
!   via an ESMF_DistGridCreate routine.  Define for deep classes only.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid] The class to be destroyed.
!   \item[rc] The optional return code.
!   \end{description}
!
!     Returns rc=OK if the grid was destroyed without error.
!     Releases all resources associated with this grid.
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_DistGridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridConstruct - fill in an already allocated DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridConstruct(distgrid, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid   ! distgrid to be initialized
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      ESMF routine which fills in the contents of an already
!      allocated DistGrid object.  May need to do additional allocations
!      as needed.  Must call the corresponding ESMF_DistGridDestruct
!      routine to free the additional memory.  Intended for internal
!      ESMF use only; end-users use ESMF_DistGridCreate, which calls
!      ESMF_DistGridConstruct.  Define for deep classes only.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_DistGridConstruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridDestruct - release resources associated w/a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridDestruct(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid     ! DistGrid to be dismantled
      integer, intent(out), optional :: rc          ! return code

!
! !DESCRIPTION:
!      ESMF routine which deallocates any space allocated by
!      ESMF_DistGridConstruct, does any additional cleanup before the
!      original DistGrid object is freed.  Intended for internal ESMF
!      use only; end-users use ESMF_DistGridDestroy, which calls
!      ESMF_DistGridDestruct.  Define for deep classes only.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_DistGridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridInit - initialize a DistGrid object

! !INTERFACE:
      subroutine ESMF_DistGridInit(distgrid, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid   ! DistGrid to be initialized
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!      ESMF routine which only initializes DistGrid values; it does not
!      allocate any resources.  Define for shallow classes only, 
!      for deep classes define and use routines Create/Destroy and 
!      Construct/Destruct.  Can be overloaded like ESMF_DistGridCreate
!      via interface blocks.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_DistGridInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGetConfig - get configuration information from a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetConfig(distgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      type(ESMF_DistGridConfig), intent(out) :: config    ! resources
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Returns the set of resources the DistGrid object was configured with.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_DistGridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSetConfig - set configuration information for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetConfig(distgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      type(ESMF_DistGridConfig), intent(in) :: config    ! resources
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Configures the DistGrid object with set of resources given.
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_DistGridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGet<Value> - get <Value> for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGet<Value>(distgrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      <value type>, intent(out) :: value
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Returns the value of DistGrid member <Value>.
!     Can be multiple routines, one per value
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_DistGridGet<Value>

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSet<Value> - set <Value> for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSet<Value>(distgrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      <value type>, intent(in) :: value
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Sets the DistGrid member <Value> with the given value.
!     Can be multiple routines, one per value
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_DistGridSet<Value>

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridValidate - internal consistency check for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridValidate(distgrid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid        ! distgrid to be checked
      character (len=*), intent(in) :: options         ! validate options
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Validates that a DistGrid is internally consistent.
!      Returns error code if problems are found.  ESMF_Base class
!      method.
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

!
!  code goes here
!
      end function ESMF_DistGridValidate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridPrint - print contents of a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridPrint(distgrid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid        ! distgrid to be printed
      character (len=*), intent(in) :: options         ! print options
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Print information about a DistGrid.  The options control the
!      type of information and level of detail.  ESMF_Base class
!      method.
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!  code goes here
!
      end subroutine ESMF_DistGridPrint

!------------------------------------------------------------------------------

      end module ESMF_DistGridMod
