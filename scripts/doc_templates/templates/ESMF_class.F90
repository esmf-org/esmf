! $Id: ESMF_class.F90,v 1.7 2002/10/11 17:11:45 eschwab Exp $
!
! ESMF <Class> Module
!
! < Something here from legal about the status of the code, like:
!  This code developed by NASA/NCAR/ESMF whatever, and is covered by
!  the terms of the GNU public license.  See license file for more details. >
!
! (all lines below between the !BOP and !EOP markers will be included in 
!  the automated document processing.)
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
! put any constants or macros which apply to the whole component in this 
!  include file.  anything public or esmf-wide should be up higher at
!  the top level include files.

#include <ESMF_<Comp>.h>


!-------------------------------------------------------------------------
! module definition

      module ESMF_<Class>Mod

!BOP
! !MODULE: ESMF_<Class>Mod - one line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements ...
!
! < insert a paragraph or two explaining what you'll find in this file >
!
!
!
!-------------------------------------------------------------------------
! !USES:
      use ESMF_Base    ! ESMF base class
!     use ESMF_XXXMod  < if needed >
      implicit none
!
! !PRIVATE TYPES:
      private

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

! !PUBLIC TYPES:
      public ESMF_<Class>Config
      public ESMF_<Class>


! !PUBLIC MEMBER FUNCTIONS:
!
! pick one or the other of the init/create sections depending on
!  whether this is a deep class (the class/derived type has pointers to
!  other memory which must be allocated/deallocated) or a shallow class
!  (the class/derived type is self-contained) and needs no destroy methods
!  other than deleting the memory for the object/derived type itself.

! the following routines apply to deep classes only
    public ESMF_<Class>Create                 ! (interface only, deep class)
    public ESMF_<Class>Destroy                ! (interface only, deep class)
    public ESMF_<Class>Construct              ! (internal only, deep class)
    public ESMF_<Class>Destruct               ! (internal only, deep class)

! the following routine applies to a shallow class
    public ESMF_<Class>Init                   ! (shallow class)

    public ESMF_<Class>Getconfig
    public ESMF_<Class>Setconfig
    public ESMF_<Class>Get<Value>
    public ESMF_<Class>Set<Value>
 
    public ESMF_<Class>Validate
    public ESMF_<Class>Print
 
! < list the rest of the public interfaces here >
!
!
!EOP


!-------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = '$Id: ESMF_class.F90,v 1.7 2002/10/11 17:11:45 eschwab Exp $
!-------------------------------------------------------------------------

! interface blocks for functions which are going to have a single
! name for ease-of-use, but internally will be implemented as separate
! subprograms.  the non-optional parts of the argument lists must be 
! distinguishable for this to work.  the following example is appropriate
! for deep classes; shallow objects will only have init routines and
! no creates.
!
!BOP
! !IROUTINE: ESMF_<Class>Create - Generic interface to create a new <Class> object

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
! !IROUTINE: ESMF_<Class>CreateNew - Create a new <Class>

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
!   Create a new <Class> from ... Allocates memory for a new <Class>
!   object and uses the internal routine ESMF_<Class>Contruct to
!   initialize it.  Define for deep classes only, for shallow classes only
!   define and use ESMF_<Class>Init
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
      end function ESMF_<Class>CreateNew


!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Destroy - free a <Class> created with Create

! !INTERFACE:
      subroutine ESMF_<Class>Destroy(<class>, rc)
!
! !ARGUMENTS:
      type(ESMF_<Class>), intent(in) :: <class>   ! <class> to be destroyed
      integer, intent(out), optional :: rc        ! return code
!
! !DESCRIPTION:
!   ESMF routine which destroys a <Class> object previously allocated
!   via an ESMF_<Class>Create routine.  Define for deep classes only.
!
!   The arguments are:
!   \begin{description}
!   \item[<class>] The class to be destroyed.
!   \item[rc] The optional return code.
!   \end{description}
!
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_<Class>Destroy

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Construct - fill in an already allocated <Class>

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
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_<Class>Construct

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Destruct - release resources associated w/a <Class>

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
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_<Class>Destruct

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Init - initialize a <Class> object

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
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_<Class>Init

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>GetConfig - get configuration information from a <Class>

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
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_<Class>GetConfig

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>SetConfig - set configuration information for a <Class>

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
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_<Class>SetConfig

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Get<Value> - get <Value> for a <Class>

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
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_<Class>Get<Value>

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Set<Value> - set <Value> for a <Class>

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
!EOP
! !REQUIREMENTS: developer's guide for classes

!
!  code goes here
!
      end subroutine ESMF_<Class>Set<Value>

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Validate - internal consistency check for a <Class>

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
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

!
!  code goes here
!
      end function ESMF_<Class>Validate


!-------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_<Class>Print - print contents of a <Class>

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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!  code goes here
!
      end subroutine ESMF_<Class>Print

!-------------------------------------------------------------------------

      end module ESMF_<Class>Mod
