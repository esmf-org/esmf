! $Id: ESMF_class.F90,v 1.1 2002/10/07 16:28:55 eschwab Exp $
!
! ESMF <Class> Module
!
! < Something here from legal about the status of the code, like:
!  This code developed by NASA/NCAR/ESMF whatever, and is covered by
!  the terms of the GNU public license.  See license file for more details. >
!

!-------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements ...
!
! < insert a paragraph or two explaining what you'll find in this file >
!
! (all lines below between the !BOP and !EOP markers will be included in 
!  the automated document processing.)
!
!-------------------------------------------------------------------------
!

! put any constants or macros which apply to the whole component in this file

#include <ESMF_<Comp>.h>


! module definition

      module ESMF_<Class>Mod

! 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
! This section contains ...
!
!

!-------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_<Class>Mod
!
! !USES:
      use ESMF_Base    ! ESMF base class
!     use ESMF_XXXMod  < if needed >
!
! !PUBLIC TYPES:
      implicit none

      type ESMF_<Class>Config
      private
      sequence
!       < insert resource items here >
      end type

      type ESMF_<Class>
      private
      sequence
        type (ESMF_Base) :: base
        integer :: basestate
!       < insert other class members here >
      end type

! !PUBLIC MEMBER FUNCTIONS:
!
!  function   ESMF_<Class>Create()               (interface only, deep class)
!  subroutine ESMF_<Class>Destroy(<class>, rc)   (interface only, deep class)
!  subroutine ESMF_<Class>Construct(<class>, rc) (internal only, deep class)
!  subroutine ESMF_<Class>Destruct(<class>, rc)  (internal only, deep class)
! or
!  subroutine ESMF_<Class>Init(<class>, rc)      (shallow class)
!
!  subroutine ESMF_<Class>Getconfig(<class>, config, rc)
!  subroutine ESMF_<Class>Setconfig(<class>, config, rc)
!  subroutine ESMF_<Class>Get<Value>(<class>, value, rc)
!  subroutine ESMF_<Class>Set<Value>(<class>, value, rc)
!
!  subroutine ESMF_<Class>Validate(<class>, options, rc)
!  subroutine ESMF_<Class>Print(<class>, options, rc)
!
! < list the rest of the public interfaces here >
!
!
!EOP

! !PRIVATE MEMBER FUNCTIONS:
!
!  < list internal func/subrs which won't be visible outside this file >
!

!-------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = '$Id: ESMF_class.F90,v 1.1 2002/10/07 16:28:55 eschwab Exp $'
!-------------------------------------------------------------------------

! interface blocks for functions which are going to have a single
! name for ease-of-use, but internally will be implemented as separate
! subprograms.  the non-optional parts of the argument lists must be 
! distinguishable for this to work.  the following example is appropriate
! for deep classes; shallow objects will only have init routines and
! no creates.
!
!BOP
! !INTERFACE:
      interface ESMF_<Class>Create 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_<Class>CreateNew
         module procedure ESMF_<Class>CreateCopy
         module procedure ESMF_<Class>CreateRemap

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of XXX subprogram. ...
!EOP

      end interface 

! declare actual overloaded routines private
      private ESMF_<Class>CreateNew, ESMF_<Class>CreateCopy, &
              ESMF_<Class>CreateRemap

! < add other interfaces here>

!-------------------------------------------------------------------------

      contains

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
! This section includes all the <Class> routines
!
!

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_<Class>CreateNew(arg1, arg2, arg3, rc)
!
! !RETURN VALUE:
      type(ESMF_<Class>) :: ESMF_<Class>CreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!      Create a new <Class> from ... Allocates memory for a new <Class>
!      object and uses the internal routine ESMF_<Class>Contruct to
!      initialize it.  Define for deep classes only, for shallow classes only
!      define and use ESMF_<Class>Init
!
! !REQUIREMENTS:  AAAn.n.n
!EOP

!
!  code goes here
!
      end function ESMF_<Class>CreateNew


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Destroy(<class>, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>   ! <class> to be destroyed
      integer, intent(out), optional :: rc        ! return code
!
! !DESCRIPTION:
!      ESMF routine which destroys a <Class> object previously allocated
!      via an ESMF_<Class>Create routine.  Define for deep classes only.
!
! !REQUIREMENTS: developer's guide for classes
!EOP

!
!  code goes here
!
      end subroutine ESMF_<Class>Destroy

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Construct(<class>, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>   ! <class> to be initialized
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      ESMF routine which fills in the contents of an already
!      allocated <Class> object.  May need to do additional allocations
!      as needed.  Must call the corresponding ESMF_<Class>Destruct
!      routine to free the additional memory.  Intended for internal
!      ESMF use only; end-users use ESMF_<Class>Create, which calls
!      ESMF_<Class>Construct.  Define for deep classes only.
!
! !REQUIREMENTS: developer's guide for classes
!EOP

!
!  code goes here
!
      end subroutine ESMF_<Class>Construct

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Destruct(<class>, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>     ! <Class> to be dismantled
      integer, intent(out), optional :: rc          ! return code

!
! !DESCRIPTION:
!      ESMF routine which deallocates any space allocated by
!      ESMF_<Class>Construct, does any additional cleanup before the
!      original <Class> object is freed.  Intended for internal ESMF
!      use only; end-users use ESMF_<Class>Destroy, which calls
!      ESMF_<Class>Destruct.  Define for deep classes only.
!
! !REQUIREMENTS: developer's guide for classes
!EOP

!
!  code goes here
!
      end subroutine ESMF_<Class>Destruct

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Init(<class>, arg1, arg2, arg3, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>   ! <Class> to be initialized
      integer, intent(in) :: arg1                        ! arg1
      integer, intent(in) :: arg2                        ! arg2
      character (len = *), intent(in), optional :: arg3  ! arg3
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!      ESMF routine which only initializes <Class> values; it does not
!      allocate any resources.  Define for shallow classes only, 
!      for deep classes define and use routines Create/Destroy and 
!      Construct/Destruct.  Can be overloaded like ESMF_<Class>Create
!      via interface blocks.
!
! !REQUIREMENTS: developer's guide for classes
!EOP

!
!  code goes here
!
      end subroutine ESMF_<Class>Init

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>SetConfig(<class>, config, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>
      type(ESMF_<Class>Config), intent(in) :: config    ! resources
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Configures the <Class> object with set of resources given.
!
! !REQUIREMENTS: developer's guide for classes
!EOP

!
!  code goes here
!
      end subroutine ESMF_<Class>GetConfig

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>GetConfig(<class>, config, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>
      type(ESMF_<Class>Config), intent(out) :: config    ! resources
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Returns the set of resources the <Class> object was configured with.
!
! !REQUIREMENTS: developer's guide for classes
!EOP

!
!  code goes here
!
      end subroutine ESMF_<Class>GetConfig

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Set<Value>(<class>, value, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>
      <value type>, intent(in) :: value
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Sets the <Class> member <Value> with the given value.
!     Can be multiple routines, one per value
!
! !REQUIREMENTS: developer's guide for classes
!EOP

!
!  code goes here
!
      end subroutine ESMF_<Class>Set<Value>

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Get<Value>(<class>, value, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>
      <value type>, intent(out) :: value
      integer, intent(out), optional :: rc              ! return code

!
! !DESCRIPTION:
!     Returns the value of <Class> member <Value>.
!     Can be multiple routines, one per value
!
! !REQUIREMENTS: developer's guide for classes
!EOP

!
!  code goes here
!
      end subroutine ESMF_<Class>Get<Value>

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Validate(<class>, options, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>        ! <class> to be checked
      character (len=*), intent(in) :: options         ! validate options
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Validates that a <Class> is internally consistent.
!      Returns error code if problems are found.  ESMF_Base class
!      method.
!
! !REQUIREMENTS:  XXXn.n, YYYn.n
!EOP

!
!  code goes here
!
      end function ESMF_<Class>Validate


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_<Class>Print(<class>, options, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>        ! <class> to be printed
      character (len=*), intent(in) :: options         ! print options
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Print information about a <Class>.  The options control the
!      type of information and level of detail.  ESMF_Base class
!      method.
!
! !REQUIREMENTS:  SSSn.n, GGGn.n
!EOP

!
!  code goes here
!
      end subroutine ESMF_<Class>Print

!-------------------------------------------------------------------------

      end module ESMF_<Class>Mod

