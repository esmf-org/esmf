! $Id: ESMF_Overloads.F90,v 1.10.2.4 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
!     ESMF Objects Module
      module ESMF_ObjectsMod
!
!==============================================================================
!
! This file contains functions which are logically part of the base class,
!  but because of the rules in compiling fortran, must be defined at the
!  very end, not at the beginning.  These include interfaces like get and
!  set of attributes, read/write, validate, and print.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
! #include "ESMF_Base.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_Base2Mod - Functions overloaded by object type.
!
! !DESCRIPTION:
!
! This file contains functions which are logically part of the {\tt Base} class,
!  but because of the rules in compiling fortran, must be defined at the
!  very end, not at the beginning.  These include interfaces like get and
!  set of attributes, read/write, validate, and print.
!
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_BaseMod    ! ESMF base class
      use ESMF_FieldMod 
      implicit none

!! COMMENTED OUT FOR NOW.
#if 0

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
    !public ESMF_BaseAttSetCharacter
    !public ESMF_BaseAttGetCharacter
    public ESMF_BaseAttSetInteger
    public ESMF_BaseAttGetInteger
    !public ESMF_BaseAttSetReal
    !public ESMF_BaseAttGetReal
    !public ESMF_BaseAttSetLogical
    !public ESMF_BaseAttGetLogical

    !public ESMF_Read
    !public ESMF_Write
    !public ESMF_Print
    !public ESMF_Validate
 
!
!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Overloads.F90,v 1.10.2.4 2009/01/21 21:25:24 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_BaseAttSetCharacter

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_FieldAttrSetChar
      !module procedure ESMF_IGridAttrSetChar
      !module procedure ESMF_ArrayAttrSetChar
      !module procedure ESMF_ClockAttrSetChar
      ! etc

! !DESCRIPTION:
!     This interface provides a single entry point for Attribute Set/Get
!     methods for each object type in the system.
!
!EOPI
      end interface 
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_BaseAttGetCharacter

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_FieldAttrGetChar
      !module procedure ESMF_IGridAttrGetChar
      !module procedure ESMF_ArrayAttrGetChar
      !module procedure ESMF_ClockAttrGetChar
      ! etc

! !DESCRIPTION:
!     This interface provides a single entry point for Attribute Set/Get
!     methods for each object type in the system.
!
!EOPI
      end interface 
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_BaseAttSetInteger

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_FieldAttrSetInt
      !module procedure ESMF_IGridAttrSetInt
      !module procedure ESMF_ArrayAttrSetInt
      !module procedure ESMF_ClockAttrSetInt
      ! etc

! !DESCRIPTION:
!     This interface provides a single entry point for Attribute Set/Get
!     methods for each object type in the system.
!
!EOPI
      end interface 
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_BaseAttGetInteger

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_FieldAttrGetInt
      !module procedure ESMF_IGridAttrGetInt
      !module procedure ESMF_ArrayAttrGetInt
      !module procedure ESMF_ClockAttrGetInt
      ! etc

! !DESCRIPTION:
!     This interface provides a single entry point for Attribute Set/Get
!     methods for each object type in the system.
!
!EOPI
      end interface 
!
!------------------------------------------------------------------------------

!     < add other interfaces here>

!==============================================================================

      contains

!==============================================================================
!
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldAttrSetChar - Set a character attr on a Field object.

! !INTERFACE:
      subroutine ESMF_FieldAttrSetChar(field, name, value, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      character (len = *), intent(in) :: name  
      character (len = *), intent(in) :: value  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Set the character string {\tt value} as a named attribute on 
!     the Field object.
!
!     The arguments are:
!     \begin{description}
!     \item[field] 
!          {\tt ESMF_Field} object to attach attribute onto.
!     \item[name]
!          Attribute name. 
!     \item[value] 
!          Attribute value.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif  

        ! pass thru f90/c++ interface - this requires first entry of object
        ! is a base type.
        call c_ESMC_BaseAttSetChar(field, name, value, status)

        if (rcpresent) rc = status

      end subroutine ESMF_FieldAttrSetChar

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldAttrGetChar - Get a character attr from a Field object.

! !INTERFACE:
      subroutine ESMF_FieldAttrGetChar(field, name, value, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field
      character (len = *), intent(in) :: name  
      character (len = *), intent(out) :: value  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Get the character string {\tt value} which is the named attribute of 
!     the Field object.
!
!     The arguments are:
!     \begin{description}
!     \item[field] 
!          {\tt ESMF_Field} object to get attribute value from.
!     \item[name]
!          Attribute name. 
!     \item[value] 
!          Attribute value.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Get initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif  

        ! pass thru f90/c++ interface - this requires first entry of object
        ! is a base type.
        call c_ESMF_BaseAttGetChar(field, name, value, status)

        if (rcpresent) rc = status

      end subroutine ESMF_FieldAttrGetChar

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldAttrSetInt - Set a integer attr on a Field object.

! !INTERFACE:
      subroutine ESMF_FieldAttrSetInt(field, name, value, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      character (len = *), intent(in) :: name  
      integer, intent(in) :: value  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Set the integer {\tt value} as a named attribute on 
!     the Field object.
!
!     The arguments are:
!     \begin{description}
!     \item[field] 
!          {\tt ESMF_Field} object to attach attribute onto.
!     \item[name]
!          Attribute name. 
!     \item[value] 
!          Attribute value.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Set initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif  

        ! pass thru f90/c++ interface - this requires first entry of object
        ! is a base type.
        call c_ESMF_BaseAttSetInt(field, name, value, status)

        if (rcpresent) rc = status

      end subroutine ESMF_FieldAttrSetInt

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldAttrGetInt - Get a integer attr from a Field object.

! !INTERFACE:
      subroutine ESMF_FieldAttrGetInt(field, name, value, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: field
      character (len = *), intent(in) :: name  
      integer, intent(out) :: value  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Get the integer {\tt value} which is the named attribute of 
!     the Field object.
!
!     The arguments are:
!     \begin{description}
!     \item[field] 
!          {\tt ESMF_Field} object to get attribute value from.
!     \item[name]
!          Attribute name. 
!     \item[value] 
!          Attribute value.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        ! local variables
        integer :: status                                ! local error status
        logical :: rcpresent                             ! did user specify rc?

        ! Get initial values
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif  

        ! pass thru f90/c++ interface - this requires first entry of object
        ! is a base type.
        call c_ESMF_BaseAttGetInt(field, name, value, status)

        if (rcpresent) rc = status

      end subroutine ESMF_FieldAttrGetInt

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_Validate - Check internal consistency of any object.

! !INTERFACE:
      subroutine ESMF_FieldValidate(field, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Base2), intent(in) :: <class>       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a Base2 is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
!          Class to be queried.
!     \item[{[opt]}]
!          Validation options.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  XXXn.n, YYYn.n

      integer :: status                       ! local error status
      logical :: rcpresent                    ! did user specify rc?
      character (len=6) :: defaultopts

      ! Initialize return code; assume failure until success is certain  
      status = ESMF_RC_NOT_IMPL   
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_RC_NOT_IMPL
      endif

      defaultopts = "quick"

      ! Decide at what level of detail to verify.
      if(present(opt)) then
          ! TODO:  decide how much checking to do
      endif

      !
      !  TODO: code goes here
      !

      ! Set return values
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_Base2Validate

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_Base2Print - Print the contents of a Base2

! !INTERFACE:
      subroutine ESMF_Base2Print(<class>, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Base2), intent(in) :: <class>      
      character (len=*), intent(in), optional :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!     Print information about a Base2.  
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
!
!     The arguments are:
!     \begin{description}
!     \item[<class>] 
!          Class to be queried.
!     \item[{[opt]}]
!          Print options that control the type of information and level of 
!          detail.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      integer :: status                       ! local error status
      logical :: rcpresent                    ! did user specify rc?
      character (len=6) :: defaultopts
      character (len=ESMF_MAXSTR) :: name

      ! Initialize return code; assume failure until success is certain  
      status = ESMF_RC_NOT_IMPL   
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_RC_NOT_IMPL
      endif

      defaultopts = "brief"

      ! Decide what to print.
      if(present(opt)) then
          ! TODO:  decide what to print
      endif

      call ESMF_GetName(<class>%<class>type%base, name, status)
      print *, "Base2 print:"
      print *, "  name = ", trim(name)

      ! TODO: add more info here

      ! Set return values
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_Base2Print

!------------------------------------------------------------------------------
#endif

      end module ESMF_ObjectsMod
