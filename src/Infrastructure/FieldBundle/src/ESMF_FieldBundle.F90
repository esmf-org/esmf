! $Id: ESMF_FieldBundle.F90,v 1.100 2011/06/01 19:27:50 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_FieldBundle.F90"
!==============================================================================
!
! ESMF FieldBundle Module
module ESMF_FieldBundleMod
!
!==============================================================================
!
! This file contains the F90 implementation of the fieldbundle class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_FieldBundleMod
!

!   F90 implemenation of fieldbundle
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
  use ESMF_ArrayMod
  use ESMF_ArrayBundleMod
  use ESMF_ContainerMod
  use ESMF_GridMod
  use ESMF_XGridMod
  use ESMF_MeshMod
  use ESMF_LocStreamMod
  use ESMF_GeomBaseMod
  use ESMF_FieldMod
  use ESMF_FieldCreateMod
  use ESMF_FieldGetMod
  use ESMF_FieldPrMod
  use ESMF_FieldSMMMod
  use ESMF_FieldRegridMod
  use ESMF_FieldWrMod
  use ESMF_RHandleMod
  use ESMF_RegridMod
  use ESMF_StaggerLocMod    
  use ESMF_VMMod
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
!     ! ESMF_FieldBundleStatus
!
!------------------------------------------------------------------------------

  type ESMF_FieldBundleStatus
    sequence
    integer :: status
  end type

  type(ESMF_FieldBundleStatus), parameter :: ESMF_FBSTATUS_UNINIT = ESMF_FieldBundleStatus(1), &
                                  ESMF_FBSTATUS_EMPTY = ESMF_FieldBundleStatus(2), &
                                  ESMF_FBSTATUS_GRIDSET = ESMF_FieldBundleStatus(3)
      
!------------------------------------------------------------------------------
!     ! ESMF_FieldBundle
!
!------------------------------------------------------------------------------

  type ESMF_FieldBundleType
  sequence
  ! this data type is not private so the fieldbundlecomm code can
  ! reach directly in and get at the localdata without a loop
  ! of subroutine calls.  but this causes problems with the 'pattern'
  ! declaration below - the fieldbundlecongruentdata derived type is
  ! private and so it wants this to be private as well.
  ! since pattern is not being used yet, comment it out below, but this
  ! needs to be rationalized at some point soon.  perhaps the comm code
  ! will have to go through a subroutine interface.  this is where
  ! fortran needs a 'friend' type of access.
  !private
    type(ESMF_Base)              :: base      ! base class object
    type(ESMF_GeomBase)          :: geombase  ! base class object
    type(ESMF_Container)         :: container ! internal storage implementation
    type(ESMF_FieldBundleStatus) :: status    ! status of this FieldBundle
    logical                      :: is_proxy  ! true if this is a proxy FB
    ESMF_INIT_DECLARE
  end type

  ! F90 class type to hold pointer to FieldBundleType
  type ESMF_FieldBundle
    sequence
    !private
    type(ESMF_FieldBundleType), pointer :: this
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_FieldBundle
  public ESMF_FieldBundleType
  public ESMF_FieldBundleStatus
      
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public operator(==)
  public operator(/=)

  public ESMF_FieldBundleAdd
  public ESMF_FieldBundleAddReplace
  public ESMF_FieldBundleCreate
  public ESMF_FieldBundleDeserialize
  public ESMF_FieldBundleDestroy
  public ESMF_FieldBundleGet
  public ESMF_FieldBundleHalo
  public ESMF_FieldBundleHaloRelease
  public ESMF_FieldBundleHaloStore
  public ESMF_FieldBundlePrint
  public ESMF_FieldBundleRead
  public ESMF_FieldBundleRedist
  public ESMF_FieldBundleRedistRelease
  public ESMF_FieldBundleRedistStore
  public ESMF_FieldBundleRegrid
  public ESMF_FieldBundleRegridRelease
  public ESMF_FieldBundleRegridStore
  public ESMF_FieldBundleRemove
  public ESMF_FieldBundleReplace
  public ESMF_FieldBundleSerialize
  public ESMF_FieldBundleSet
  public ESMF_FieldBundleSMM
  public ESMF_FieldBundleSMMRelease
  public ESMF_FieldBundleSMMStore
  public ESMF_FieldBundleValidate
  public ESMF_FieldBundleWrite


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! - ESMF-internal methods:
  public ESMF_FieldBundleGetInit
  public ESMF_FieldBundleDestruct


!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_FieldBundle.F90,v 1.100 2011/06/01 19:27:50 feiliu Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_FieldBundleAdd -- Generic interface

! !INTERFACE:
  interface ESMF_FieldBundleAdd

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_FieldBundleAddItem
    module procedure ESMF_FieldBundleAddList
!EOPI

  end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_FieldBundleGet -- Generic interface

! !INTERFACE:
  interface ESMF_FieldBundleGet

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_FieldBundleGetItem
    module procedure ESMF_FieldBundleGetList
    module procedure ESMF_FieldBundleGetListAll
    module procedure ESMF_FieldBundleGetIndex
!EOPI

  end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_FieldBundleSet -- Generic interface

! !INTERFACE:
  interface ESMF_FieldBundleSet

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_FieldBundleSetGrid
    module procedure ESMF_FieldBundleSetLS
    module procedure ESMF_FieldBundleSetMesh
    module procedure ESMF_FieldBundleSetXGrid
!EOPI

  end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_FieldBundleRedist -- Generic interface

! !INTERFACE:
  interface ESMF_FieldBundleRedistStore

! !PRIVATE MEMBER FUNCTIONS:
    module procedure ESMF_FieldBundleRedistStoreI4
    module procedure ESMF_FieldBundleRedistStoreI8
    module procedure ESMF_FieldBundleRedistStoreR4
    module procedure ESMF_FieldBundleRedistStoreR8
    module procedure ESMF_FieldBundleRedistStoreNF
!
!EOPI

  end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_FieldBundleSMM -- Generic interface

! !INTERFACE:
  interface ESMF_FieldBundleSMMStore

! !PRIVATE MEMBER FUNCTIONS:
    module procedure ESMF_FieldBundleSMMStoreI4
    module procedure ESMF_FieldBundleSMMStoreI8
    module procedure ESMF_FieldBundleSMMStoreR4
    module procedure ESMF_FieldBundleSMMStoreR8
    module procedure ESMF_FieldBundleSMMStoreNF
!
!EOPI

  end interface


!===============================================================================
! FieldBundleOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_FieldBundleAssignment(=) - fieldbundle assignment
!
! !INTERFACE:
!   interface assignment(=)
!   fieldbundle1 = fieldbundle2
!
! !ARGUMENTS:
!   type(ESMF_FieldBundle) :: fieldbundle1
!   type(ESMF_FieldBundle) :: fieldbundle2
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Assign fieldbundle1 as an alias to the same ESMF fieldbundle object in memory
!   as fieldbundle2. If fieldbundle2 is invalid, then fieldbundle1 will be equally invalid after
!   the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[fieldbundle1]
!     The {\tt ESMF\_FieldBundle} object on the left hand side of the assignment.
!   \item[fieldbundle2]
!     The {\tt ESMF\_FieldBundle} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_FieldBundleOperator(==) - fieldbundle equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (fieldbundle1 == fieldbundle2) then ... endif
!             OR
!   result = (fieldbundle1 == fieldbundle2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_FieldBundle), intent(in) :: fieldbundle1
!   type(ESMF_FieldBundle), intent(in) :: fieldbundle2
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Test whether fieldbundle1 and fieldbundle2 are valid aliases to the same ESMF
!   fieldbundle object in memory. For a more general comparison of two ESMF FieldBundles,
!   going beyond the simple alias test, the ESMF\_FieldBundleMatch() function (not yet
!   implemented) must be used.
!   \end{sloppypar}
!
!   The arguments are:
!   \begin{description}
!   \item[fieldbundle1]
!     The {\tt ESMF\_FieldBundle} object on the left hand side of the equality
!     operation.
!   \item[fieldbundle2]
!     The {\tt ESMF\_FieldBundle} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_FieldBundleEQ
    module procedure ESMF_FieldBundleStatusEQ

  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_FieldBundleOperator(/=) - fieldbundle not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (fieldbundle1 == fieldbundle2) then ... endif
!             OR
!   result = (fieldbundle1 == fieldbundle2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_FieldBundle), intent(in) :: fieldbundle1
!   type(ESMF_FieldBundle), intent(in) :: fieldbundle2
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Test whether fieldbundle1 and fieldbundle2 are {\it not} valid aliases to the
!   same ESMF fieldbundle object in memory. For a more general comparison of two ESMF
!   FieldBundles, going beyond the simple alias test, the ESMF\_FieldBundleMatch() function
!   (not yet implemented) must be used.
!   \end{sloppypar}
!
!   The arguments are:
!   \begin{description}
!   \item[fieldbundle1]
!     The {\tt ESMF\_FieldBundle} object on the left hand side of the non-equality
!     operation.
!   \item[fieldbundle2]
!     The {\tt ESMF\_FieldBundle} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_FieldBundleNE
    module procedure ESMF_FieldBundleStatusNE

  end interface
!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleEQ()"
!BOPI
! !IROUTINE:  ESMF_FieldBundleEQ - Compare two FieldBundles for equality
!
! !INTERFACE:
  function ESMF_FieldBundleEQ(fieldbundle1, fieldbundle2)
! 
! !RETURN VALUE:
    logical :: ESMF_FieldBundleEQ

! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in) :: fieldbundle1
    type(ESMF_FieldBundle), intent(in) :: fieldbundle2

! !DESCRIPTION:
!   Test if both {\tt fieldbundle1} and {\tt fieldbundle2} alias the same ESMF fieldbundle 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE fbinit1, fbinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: use Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    fbinit1 = ESMF_FieldBundleGetInit(fieldbundle1)
    fbinit2 = ESMF_FieldBundleGetInit(fieldbundle2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (fbinit1 .eq. ESMF_INIT_CREATED .and. &
      fbinit2 .eq. ESMF_INIT_CREATED) then
      ESMF_FieldBundleEQ = associated(fieldbundle1%this,fieldbundle2%this)
    else
      ESMF_FieldBundleEQ = ESMF_FALSE
    endif

  end function ESMF_FieldBundleEQ
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleNE()"
!BOPI
! !IROUTINE:  ESMF_FieldBundleNE - Compare two FieldBundles for non-equality
!
! !INTERFACE:
  function ESMF_FieldBundleNE(fieldbundle1, fieldbundle2)
! 
! !RETURN VALUE:
    logical :: ESMF_FieldBundleNE

! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in) :: fieldbundle1
    type(ESMF_FieldBundle), intent(in) :: fieldbundle2

! !DESCRIPTION:
!   Test if both {\tt fieldbundle1} and {\tt fieldbundle2} alias the same ESMF fieldbundle 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE fbinit1, fbinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ESMF_FieldBundleNE = .not.ESMF_FieldBundleEQ(fieldbundle1, fieldbundle2)

  end function ESMF_FieldBundleNE
!-------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAddList()"
!BOP
! !IROUTINE: ESMF_FieldBundleAddList - Add Fields to an fieldbundle
!
! !INTERFACE:
    ! Private name; call using ESMF_FieldBundleAdd()   
    subroutine ESMF_FieldBundleAddList(fieldbundle, fieldList, keywordEnforcer, &
      multiflag, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)         :: fieldbundle
    type(ESMF_Field),       intent(in)            :: fieldList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: multiflag
    logical,                intent(in),  optional :: relaxedflag
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Add field(s) to an fieldbundle. It is an error if {\tt fieldList} contains
!   Fields that match by name Fields already contained in {\tt fieldbundle} when multiflag
!   is set to {\tt .false.} and relaxedflag is set to {\tt .false.} by default.
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} to be added to.
!   \item [fieldList]
!     List of {\tt ESMF\_Field} objects to be added.
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple items with the same name
!     to be added to {\tt fieldbundle}. For {\tt .false.} added items must
!     have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "add"
!     under {\tt multiflag=.false.} mode, where it is {\em not} an error if 
!     {\tt fieldList} contains items with names that are also found in 
!     {\tt fieldbundle}. The {\tt fieldbundle} is left unchanged for these items.
!     For {\tt .false.} this is treated as an error condition. 
!     The default setting is {\tt .false.}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: fieldCount, i, j, addedIndex, garbageSize
    type(ESMF_Logical)            :: linkChange
    type(ESMF_Field), pointer     :: garbageList(:), addedList(:)
    logical                       :: isGarbage

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    ! Determine the number of FieldList elements
    fieldCount = size(fieldList)

    call ESMF_ContainerGarbageOn(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerGarbageClear(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerAdd(fieldbundle%this%container, fieldList, &
      multiflag=multiflag, relaxedflag=relaxedflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    nullify(garbageList)
    call ESMF_ContainerGarbageGet(fieldbundle%this%container, garbageList=garbageList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerGarbageOff(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! There are garbage, some fields are not added. 
    ! Deduce the list of Field actually got added.
    nullify(addedList)
    if(associated(garbageList)) then
      if(size(garbageList) .ge. 1) then
        garbageSize = size(garbageList)

        ! error checking
        if(garbageSize .gt. fieldCount) then
          call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
            msg = " - there are more garbage in garbageList than FieldList", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif

        ! Partial add
        if(garbageSize .lt. fieldCount) then

          allocate(addedList(fieldCount - garbageSize), stat=localrc)
          if(localrc /= 0) then
            call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
              msg = " - cannot allocate addedList", &
              ESMF_CONTEXT, rcToReturn=rc)
            return
          endif

          ! TODO: this is a performance bottlnect
          ! if this causes problem, the container add method should return
          ! a list of field actually are added
          addedIndex = 1
          do i = 1, fieldCount
            isGarbage = .false.
            do j = 1, garbageSize
              if(fieldList(i) == garbageList(j)) isGarbage = .true.
            enddo

            if(.not. isGarbage)  then
              addedList(addedIndex) = fieldList(i)
              addedIndex = addedIndex + 1
            endif
          enddo
        endif  ! partial add
      endif ! there are garbage

      ! Attribute link
      linkChange = ESMF_TRUE
      if(associated(addedList)) then
        do i=1, size(addedList)
          call c_ESMC_AttributeLink(fieldbundle%this%base, addedList(i)%ftypep%base, linkChange, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                        ESMF_CONTEXT, rcToReturn=rc))  return
        enddo
        deallocate(addedList)
      endif

      deallocate(garbageList)
    else
      ! No garbage, all fieldList should be linked
      ! Attribute link
      linkChange = ESMF_TRUE
      do i=1, size(fieldList)
        call c_ESMC_AttributeLink(fieldbundle%this%base, fieldList(i)%ftypep%base, linkChange, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc))  return
      enddo
      
    endif ! associated(garbageList)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleAddList
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAddItem()"
!BOP
! !IROUTINE: ESMF_FieldBundleAddItem - Add Fields to an fieldbundle
!
! !INTERFACE:
    ! Private name; call using ESMF_FieldBundleAdd()   
    subroutine ESMF_FieldBundleAddItem(fieldbundle, field, keywordEnforcer, &
      multiflag, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)         :: fieldbundle
    type(ESMF_Field),       intent(in)            :: field
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: multiflag
    logical,                intent(in),  optional :: relaxedflag
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Add a single field to an fieldbundle. It is an error if {\tt field} 
!   match by name to what is already contained in {\tt fieldbundle} when multiflag
!   is set to {\tt .false.} and relaxedflag is set to {\tt .false.} by default.
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} to be added to.
!   \item [field]
!     {\tt ESMF\_Field} object to be added.
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple items with the same name
!     to be added to {\tt fieldbundle}. For {\tt .false.} added items must
!     have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "add"
!     under {\tt multiflag=.false.} mode, where it is {\em not} an error if 
!     {\tt fieldList} contains items with names that are also found in 
!     {\tt fieldbundle}. The {\tt fieldbundle} is left unchanged for these items.
!     For {\tt .false.} this is treated as an error condition. 
!     The default setting is {\tt .false.}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    ! Call into the list version
    call ESMF_FieldBundleAddList(fieldbundle, (/field/), multiflag=multiflag, &
      relaxedflag=relaxedflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleAddItem
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAddReplace()"
!BOP
! !IROUTINE: ESMF_FieldBundleAddReplace - Conditionally add or replace Fields in an fieldbundle
!
! !INTERFACE:
    subroutine ESMF_FieldBundleAddReplace(fieldbundle, fieldList, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)         :: fieldbundle
    type(ESMF_Field),       intent(in)            :: fieldList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Fields in {\tt fieldList} that do not match any Fields by name in 
!   {\tt fieldbundle} are added to the fieldbundle. Fields in {\tt fieldbundle}
!   that match by name Fields in {\tt fieldList} are replaced by those Fields.
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} to be manipulated.
!   \item [fieldList]
!     List of {\tt ESMF\_Field} objects to be added or used as replacement.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: fieldCount, i
    type(ESMF_Logical)            :: linkChange
    type(ESMF_Field), pointer     :: garbageList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)
    
    ! Determine the number of FieldList elements
    fieldCount = size(fieldList)
    nullify(garbageList)

    call ESMF_ContainerGarbageOn(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerGarbageClear(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into container addreplace
    call ESMF_ContainerAddReplace(fieldbundle%this%container, fieldList, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerGarbageGet(fieldbundle%this%container, garbageList=garbageList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerGarbageOff(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Attribute link
    linkChange = ESMF_TRUE
    ! Add all fields in fieldList
    do i = 1, fieldCount
      call c_ESMC_AttributeLink(fieldbundle%this%base, fieldList(i)%ftypep%base, linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
    enddo
    
    ! Remove those that were replaced
    if(associated(garbageList)) then
      do i=1, size(garbageList)
        call c_ESMC_AttributeLinkRemove(fieldbundle%this%base, garbageList(i)%ftypep%base, linkChange, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc))  return
      enddo
      deallocate(garbageList)
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleAddReplace
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleCreate()"
!BOP
! !IROUTINE: ESMF_FieldBundleCreate - Create an fieldbundle from a list of Fields
!
! !INTERFACE:
  function ESMF_FieldBundleCreate(keywordEnforcer, fieldList, name, rc)
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Field), intent(in),  optional :: fieldList(:)
    character (len=*),intent(in),  optional :: name
    integer,          intent(out), optional :: rc
!         
! !RETURN VALUE:
    type(ESMF_FieldBundle) :: ESMF_FieldBundleCreate
!
! !DESCRIPTION:
!   Create an {\tt ESMF\_FieldBundle} object from a list of existing Fields.
!
!   The creation of an fieldbundle leaves the bundled Fields unchanged, they
!   remain valid individual objects. An fieldbundle is a light weight container
!   of field references. The actual data remains in place, there are no
!   data movements or duplications associated with the creation of an 
!   fieldbundle.
!
!   \begin{description}
!   \item [{[fieldList]}]
!     List of {\tt ESMF\_Field} objects to be bundled.
!   \item [{[name]}]
!     Name of the created {\tt ESMF\_FieldBundle}. A default name is generated
!     if not specified.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc    ! local return code
    type(ESMF_FieldBundleType), pointer  :: this
    integer                 :: fieldCount, i
    type(ESMF_Logical)      :: linkChange
    type(ESMF_GEOMTYPE)     :: geomtype
    type(ESMF_Grid)         :: grid
    type(ESMF_XGrid)        :: xgrid
    type(ESMF_Mesh)         :: mesh
    type(ESMF_LocStream)    :: locstream
    type(ESMF_FieldStatus)  :: fstatus

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
    ! Determine the number of FieldList elements
    if (present(fieldList)) then
      fieldCount = size(fieldList)
    else
      fieldCount = 0
    endif

    ! Check init status of arguments
    do i=1, fieldCount
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, fieldList(i), rc)
    enddo

    ! Initialize
    nullify(this)
    nullify(ESMF_FieldBundleCreate%this)
    
    ! Create the internal objects
    allocate(this, stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
        msg="- Allocating FieldBundle Type", &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_BaseCreate(this%base, "FieldBundle", name, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    this%container = ESMF_ContainerCreate(rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    this%status = ESMF_FBSTATUS_EMPTY

    ! Set up proxy flag
    this%is_proxy = .false.

    ! Set return value
    ESMF_FieldBundleCreate%this => this

    ! Add reference to this object into ESMF garbage collection table
    ! Only call this in those Create() methods that call Construct()
    call c_ESMC_VMAddFObject(ESMF_FieldBundleCreate, ESMF_ID_FIELDBUNDLE%objectID)

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_FieldBundleCreate)

    if(present(fieldList)) then
      call ESMF_FieldBundleAdd(ESMF_FieldBundleCreate, fieldList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      ! link the Attribute hierarchies
      linkChange = ESMF_TRUE;
      do i=1, size(fieldList)
        call c_ESMC_AttributeLink(this%base, fieldList(i)%ftypep%base, linkChange, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc))  return
      enddo
      if(size(fieldList) .ge. 1) then
        call ESMF_FieldGet(fieldList(1), status=fstatus, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc))  return

        if(fstatus == ESMF_FIELDSTATUS_GRIDSET .or. &
           fstatus == ESMF_FIELDSTATUS_COMPLETE) then
          call ESMF_FieldGet(fieldList(1), geomtype=geomtype, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc))  return
          if(geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldGet(fieldList(1), grid=grid, rc=localrc)  
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc))  return
            call ESMF_FieldBundleSet(ESMF_FieldBundleCreate, grid, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc))  return
          else if(geomtype == ESMF_GEOMTYPE_XGRID) then
            call ESMF_FieldGet(fieldList(1), xgrid=xgrid, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc))  return
            call ESMF_FieldBundleSet(ESMF_FieldBundleCreate, xgrid, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc))  return
          else if(geomtype == ESMF_GEOMTYPE_MESH) then
            call ESMF_FieldGet(fieldList(1), mesh=mesh, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc))  return
            call ESMF_FieldBundleSet(ESMF_FieldBundleCreate, mesh, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc))  return
          else if(geomtype == ESMF_GEOMTYPE_LOCSTREAM) then
            call ESMF_FieldGet(fieldList(1), locstream=locstream, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc))  return
            call ESMF_FieldBundleSet(ESMF_FieldBundleCreate, locstream, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc))  return
          endif
          this%status = ESMF_FBSTATUS_GRIDSET
        endif ! field has a geombase internally
      endif ! non-empty fieldlist
    endif ! present

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end function ESMF_FieldBundleCreate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleDestroy()"
!BOP
! !IROUTINE: ESMF_FieldBundleDestroy - Destroy an fieldbundle

! !INTERFACE:
  subroutine ESMF_FieldBundleDestroy(fieldbundle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)           :: fieldbundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),  optional  :: rc  
!         
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
! Destroy an {\tt ESMF\_FieldBundle} object. The member Fields are not
! touched by this operation and remain valid objects that need to be 
! destroyed individually if necessary.
!
! The arguments are:
! \begin{description}
! \item[fieldbundle] 
!      {\tt ESMF\_FieldBundle} object to be destroyed.
! \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    if (.not.associated(fieldbundle%this)) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Uninitialized or already destroyed FieldBundle: this pointer unassociated", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    call ESMF_FieldBundleDestruct(fieldbundle%this, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this fieldbundle as invalid
    if(associated(fieldbundle%this)) nullify(fieldbundle%this)

    ! Set init code
    ESMF_INIT_SET_DELETED(fieldbundle)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_FieldBundleDestroy
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleDestruct()"
!BOPI
! !IROUTINE: ESMF_FieldBundleDestruct - Destruct the FieldBundleType
!
! !INTERFACE:
    ! Private name; call using ESMF_FieldBundleDestruct()   
  subroutine ESMF_FieldBundleDestruct(this, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundleType), pointer       :: this
    integer, intent(out), optional            :: rc
!
!
! !DESCRIPTION:
!   Destruct the FieldBundleType
!
!   \begin{description}
!   \item [this]
!     {\tt ESMF\_FieldBundleType} to be destructed.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    integer                                   :: localrc
    type(ESMF_Status) :: basestatus

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call ESMF_BaseGetStatus(this%base, basestatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (basestatus .eq. ESMF_STATUS_READY) then
      ! Destroy internal container
      call ESMF_ContainerDestroy(this%container, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if(this%status == ESMF_FBSTATUS_GRIDSET) then
        call ESMF_GeomBaseDestroy(this%geombase, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif
    
    ! Mark base object invalid
    call ESMF_BaseSetStatus(this%base, ESMF_STATUS_INVALID, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if(present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleDestruct
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetItem()"
!BOP
! !IROUTINE: ESMF_FieldBundleGet - Query scalar information about a specific fieldName
!
! !INTERFACE:
    ! Private name; call using ESMF_FieldBundleGet()   
    subroutine ESMF_FieldBundleGetItem(fieldbundle, fieldName, &
      keywordEnforcer, field, fieldCount, isPresent, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)            :: fieldbundle
    character(len=*),       intent(in)            :: fieldName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Field),       intent(out), optional :: field
    integer,                intent(out), optional :: fieldCount
    logical,                intent(out), optional :: isPresent
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Get information about items that match {\tt fieldName} in fieldbundle.
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} to be queried.
!   \item [fieldName]
!     Specified name.
!   \item [{[field]}]
!     Upon return holds the requested field item. It is an error if this
!     argument was specified and there is not exactly one field item in 
!     {\tt fieldbundle} that matches {\tt fieldName}.
!   \item [{[fieldCount]}]
!     Number of Fields with {\tt fieldName} in {\tt fieldbundle}.
!   \item [{[isPresent]}]
!     Upon return indicates whether field(s) with {\tt fieldName} exist
!     in {\tt fieldbundle}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)
    
    if (present(fieldCount)) then
      call ESMF_ContainerGet(fieldbundle%this%container, itemName=trim(fieldName), &
        itemCount=fieldCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(isPresent)) then
      call ESMF_ContainerGet(fieldbundle%this%container, itemName=trim(fieldName), &
        isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if(present(fieldCount)) then
      if(fieldCount .gt. 1) then
        if(present(field)) then
          call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
            msg = " - field argument cannot be specified when fieldCount is greater than 1", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
      endif
    endif

    if (present(field)) then
      call ESMF_ContainerGet(fieldbundle%this%container, itemName=trim(fieldName), &
        item=field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleGetItem
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetList()"
!BOP
! !IROUTINE: ESMF_FieldBundleGet - Access a list of Fields matching fieldName
!
! !INTERFACE:
    ! Private name; call using ESMF_FieldBundleGet()   
    subroutine ESMF_FieldBundleGetList(fieldbundle, fieldName, fieldList, &
      keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)            :: fieldbundle
    character(len=*),       intent(in)            :: fieldName
    type(ESMF_Field),       intent(out)           :: fieldList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Get the list of Fields from fieldbundle that match fieldName.
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} to be queried.
!   \item [fieldName]
!     Specified name.
!   \item [{[fieldList]}]
!     List of Fields in {\tt fieldbundle} that match {\tt fieldName}. The
!     argument must be allocated to be at least of size {\tt fieldCount}
!     returned for this {\tt fieldName}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: fieldCount
    type(ESMF_Field), pointer     :: l_fieldList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)
    
    nullify(l_fieldList)
    ! Check size
    call ESMF_ContainerGet(fieldbundle%this%container, trim(fieldName), &
      itemCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if(size(fieldList) .lt. fieldCount) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
      msg=" - Input argument fieldList size is too small", &
      ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    allocate(l_fieldList(fieldCount), stat=localrc)
    if(localrc /= ESMF_SUCCESS) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg = " - cannot allocate l_fieldList internally", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    call ESMF_ContainerGet(fieldbundle%this%container, trim(fieldName), &
      l_fieldList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    fieldList(1:fieldCount) = l_fieldList(1:fieldCount)

    deallocate(l_fieldList)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleGetList
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetListAll()"
!BOP
! !IROUTINE: ESMF_FieldBundleGet - Access a list of all Fields
!
! !INTERFACE:
    ! Private name; call using ESMF_FieldBundleGet()   
    subroutine ESMF_FieldBundleGetListAll(fieldbundle, keywordEnforcer, &
      geomtype, grid, locstream, mesh, xgrid, &
      fieldCount, fieldList, fieldNameList, name, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)            :: fieldbundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_GeomType),    intent(out), optional :: geomType
    type(ESMF_Grid),        intent(out), optional :: grid
    type(ESMF_LocStream),   intent(out), optional :: locstream
    type(ESMF_Mesh),        intent(out), optional :: mesh
    type(ESMF_XGrid),       intent(out), optional :: xgrid
    integer,                intent(out), optional :: fieldCount
    type(ESMF_Field),       intent(out), optional :: fieldList(:)
    character(len=*),       intent(out), optional :: fieldNameList(:)
    character(len=*),       intent(out), optional :: name
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Get the list of all Fields and field names bundled in an fieldbundle.
!
!   \begin{description}
!   \item [fieldbundle]
!         {\tt ESMF\_FieldBundle} to be queried.
!   \item[{[geomType]}]
!      Flag that indicates what type of GeomBase this FieldBundle object holds. 
!      Can be {\tt ESMF\_GEOMTYPE\_GRID}, {\tt ESMF\_GEOMTYPE\_MESH}, {\tt ESMF\_GEOMTYPE\_LOCSTREAM},
!      {\tt ESMF\_GEOMTYPE\_XGRID}
!   \item[{[grid]}]
!      The Grid object that this FieldBundle object holds. 
!   \item[{[locstream]}]
!      The LocStream object that this FieldBundle object holds. 
!   \item[{[mesh]}]
!      The Mesh object that this FieldBundle object holds. 
!   \item[{[xgrid]}]
!      The XGrid object that this FieldBundle object holds. 
!   \item [{[fieldCount]}]
!         Upon return holds the number of Fields bundled in the fieldbundle.
!   \item [{[fieldList]}]
!         Upon return holds a list of Fields bundled in {\tt fieldbundle}. The
!         argument must be allocated to be at least of size {\tt fieldCount}.
!   \item [{[fieldNameList]}]
!         Upon return holds a list of the names of the field bundled in 
!         {\tt fieldbundle}. The argument must be allocated to be at least of
!         size {\tt fieldCount}.
!   \item [{[name]}]
!         Name of the fieldbundle object.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: l_fieldCount, i         ! helper variable
    type(ESMF_Field), pointer     :: l_fieldList(:)
    type(ESMF_FieldBundleType), pointer     :: this
    type(ESMF_GeomType)           :: l_geomtype

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    nullify(this)
    nullify(l_fieldList)

    this => fieldbundle%this

    ! geomBase
    if(present(geomtype)) then
      if(this%status /= ESMF_FBSTATUS_GRIDSET) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg = " - fieldbundle does not have a geombase object stored", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
  
      call ESMF_GeomBaseGet(this%geombase, geomtype=geomtype, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if(present(grid)) then
      if(this%status /= ESMF_FBSTATUS_GRIDSET) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg = " - fieldbundle does not have a geombase object stored", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
  
      call ESMF_GeomBaseGet(this%geombase, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if(present(xgrid)) then
      if(this%status /= ESMF_FBSTATUS_GRIDSET) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg = " - fieldbundle does not have a geombase object stored", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
  
      call ESMF_GeomBaseGet(this%geombase, xgrid=xgrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if(present(mesh)) then
      if(this%status /= ESMF_FBSTATUS_GRIDSET) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg = " - fieldbundle does not have a geombase object stored", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
  
      call ESMF_GeomBaseGet(this%geombase, mesh=mesh, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if(present(locstream)) then
      if(this%status /= ESMF_FBSTATUS_GRIDSET) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg = " - fieldbundle does not have a geombase object stored", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
  
      call ESMF_GeomBaseGet(this%geombase, locstream=locstream, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
        
    
    ! Call into the container
    call ESMF_ContainerGet(fieldbundle%this%container, &
      itemCount=l_fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if(present(fieldCount)) fieldCount = l_fieldCount

    allocate(l_fieldList(l_fieldCount), stat=localrc)
    if(localrc /= ESMF_SUCCESS) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg = " - cannot allocate l_fieldList internally", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    call ESMF_ContainerGet(fieldbundle%this%container, &
      itemList=l_fieldList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if(present(fieldList)) then
      if(size(fieldList) .lt. l_fieldCount) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg = " - Input fieldList size is too small", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      fieldList(1:l_fieldCount) = l_fieldList(1:l_fieldCount)
    endif

    ! Special call to get name out of Base class
    if (present(fieldNameList)) then
      if(size(fieldNameList) .lt. l_fieldCount) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg = " - Input fieldNameList size is too small", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      do i = 1, l_fieldCount
        call ESMF_FieldGet(l_fieldList(i), name=fieldNameList(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    endif
  
    deallocate(l_fieldList)

    if (present(name)) then
        call c_ESMC_GetName(fieldbundle%this%base, name, localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleGetListAll
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetIndex()"
!BOPI
! !IROUTINE: ESMF_FieldBundleGet - Access the fieldIndex-th Field in FieldBundle
!
! !INTERFACE:
    ! Private name; call using ESMF_FieldBundleGet()   
    subroutine ESMF_FieldBundleGetIndex(fieldbundle, fieldIndex, field, keywordEnforcer, &
      rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)            :: fieldbundle
    integer,                intent(in)            :: fieldIndex
    type(ESMF_Field),       intent(inout)         :: field
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Get the fieldIndex-th Field in FieldBundle. The order of the Field in FieldBundle
!   is not guranteed. If this call is used iteratively, then any Add, Replace, Remove
!   call on the FieldBundle will invalidate the order of the Field in the FieldBundle.
!
!   \begin{description}
!   \item [fieldbundle]
!         {\tt ESMF\_FieldBundle} to be queried.
!   \item [fieldIndex]
!         The fieldIndex-th Field to be returned.
!   \item [field]
!         The fieldIndex-th Field to be returned.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: l_fieldCount, i         ! helper variable
    type(ESMF_Field), pointer     :: l_fieldList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    nullify(l_fieldList)
    
    ! Call into the container
    call ESMF_ContainerGet(fieldbundle%this%container, &
      itemCount=l_fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if(fieldIndex .lt. 1 .or. fieldIndex .gt. l_fieldCount) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg = " - fieldIndex must be between 1 and fieldCount in the FieldBundle", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    allocate(l_fieldList(l_fieldCount), stat=localrc)
    if(localrc /= ESMF_SUCCESS) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg = " - cannot allocate l_fieldList internally", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    call ESMF_ContainerGet(fieldbundle%this%container, &
      itemList=l_fieldList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    field = l_fieldList(fieldIndex)
  
    deallocate(l_fieldList)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleGetIndex
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleHalo()"
!BOP
! !IROUTINE: ESMF_FieldBundleHalo - Execute a FieldBundle halo operation
!
! !INTERFACE:
  subroutine ESMF_FieldBundleHalo(fieldbundle, routehandle, keywordEnforcer, &
    checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_FieldBundle), intent(inout)           :: fieldbundle
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
        type(ESMF_KeywordEnforcer),           optional  :: keywordEnforcer 
				! must use keywords below
        logical,                intent(in),   optional  :: checkflag
        integer,                intent(out),  optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed FieldBundle halo operation for the Fields in fieldbundle.
!   See {\tt ESMF\_FieldBundleStore()} on how to compute routehandle.
!   \end{sloppypar}
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} with source data. The data in this 
!       FieldBundle may be destroyed by this call.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input FieldBundle pair will be checked for
!     consistency with the precomputed operation provided by {\tt routehandle}.
!     If set to {\tt .FALSE.} {\em (default)} only a very basic input check
!     will be performed, leaving many inconsistencies undetected. Set
!     {\tt checkflag} to {\tt .FALSE.} to achieve highest performance.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code
        
        ! local variables to buffer optional arguments
        logical                 :: l_checkflag! helper variable
        type(ESMF_Field)        :: l_field ! helper variable
        type(ESMF_Field), allocatable        :: l_fieldList(:) ! helper variable

        ! local internal variables
        integer                 :: fcount, i

        type(ESMF_ArrayBundle)  :: arrayBundle
        type(ESMF_Array), allocatable :: arrays(:)

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments, deal with optional FieldBundle args
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

        ! Set default flags
        l_checkflag = ESMF_FALSE
        if (present(checkflag)) l_checkflag = checkflag

        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

        call ESMF_FieldBundleGet(fieldbundle, fieldCount=fcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(l_fieldList(fcount))

        call ESMF_FieldBundleGet(fieldbundle, fieldList=l_fieldList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! build arrayBundle on-the-fly
        allocate(arrays(fcount))
        do i = 1, fcount
            call ESMF_FieldGet(l_fieldList(i), array=arrays(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        arrayBundle = ESMF_ArrayBundleCreate(arrayList=arrays, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(arrays)

        call ESMF_ArrayBundleHalo(arrayBundle, routehandle, &
            checkflag=l_checkflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
            
        ! garbage collection
        call ESMF_ArrayBundleDestroy(arrayBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(l_fieldList)
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleHalo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleHaloRelease()"
!BOP
! !IROUTINE: ESMF_FieldBundleHaloRelease - Release resources associated with a FieldBundle halo operation
!
! !INTERFACE:
  subroutine ESMF_FieldBundleHaloRelease(routehandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(out),  optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Release resouces associated with a FieldBundle halo operation. After this call
!   {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
            
        ! Call into the RouteHandle code
        call ESMF_RouteHandleRelease(routehandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleHaloRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleHaloStore()"
!BOP
! !IROUTINE: ESMF_FieldBundleHaloStore - Precompute a FieldBundle halo operation
!
! !INTERFACE:
    subroutine ESMF_FieldBundleHaloStore(fieldbundle, routehandle, &
      keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)           :: fieldbundle
    type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),   optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Store a FieldBundle halo operation over the data in {\tt fieldbundle}. By 
!   definition, all elements in the total Field regions that lie
!   outside the exclusive regions will be considered potential destination
!   elements for halo. However, only those elements that have a corresponding
!   halo source element, i.e. an exclusive element on one of the DEs, will be
!   updated under the halo operation. Elements that have no associated source
!   remain unchanged under halo.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldBundleHalo()} on any FieldBundle that is weakly congruent
!   and typekind conform to {\tt fieldbundle}. Congruency for FieldBundles is
!   given by the congruency of its constituents.
!   Congruent Fields possess matching DistGrids, and the shape of the local
!   array tiles matches between the Fields for every DE. For weakly congruent
!   Fields the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Fields that differ in the number of elements in the left most
!   undistributed dimensions.
!  
!   This call is {\em collective} across the current VM.  
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} containing data to be haloed. The data in this 
!       FieldBundle may be destroyed by this call.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        ! internal local variables 
        integer                                       :: localrc, fcount, i 
        type(ESMF_Field)                              :: l_field
        type(ESMF_ArrayBundle)                        :: arrayBundle
        type(ESMF_Array), allocatable                 :: arrays(:)
        type(ESMF_Field), allocatable                 :: l_fieldList(:) ! helper variable

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc) 

        ! build arrayBundle on-the-fly
        call ESMF_FieldBundleGet(fieldbundle, fieldCount=fcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(l_fieldList(fcount))

        call ESMF_FieldBundleGet(fieldbundle, fieldList=l_fieldList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(arrays(fcount))
        do i = 1, fcount
            call ESMF_FieldGet(l_fieldList(i), array=arrays(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        arrayBundle = ESMF_ArrayBundleCreate(arrayList=arrays, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(arrays)

        call ESMF_ArrayBundleHaloStore(arrayBundle, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
            
        ! garbage collection
        call ESMF_ArrayBundleDestroy(arrayBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(l_fieldList)

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleHaloStore
!------------------------------------------------------------------------------ 


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundlePrint()"
!BOP
! !IROUTINE: ESMF_FieldBundlePrint - Print fieldbundle internals

! !INTERFACE:
  subroutine ESMF_FieldBundlePrint(fieldbundle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)              :: fieldbundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),  optional  :: rc  
!         
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Print internal information of the specified {\tt ESMF\_FieldBundle} object. \\
!
!   Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!   On some platforms/compilers there is a potential issue with interleaving
!   Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!   the expected order.  If this occurs, the {\tt ESMF\_IOUnitFlush()} method
!   may be used on unit 6 to get coherent output.  \\
!
!   The arguments are:
!   \begin{description}
!   \item[fieldbundle] 
!     {\tt ESMF\_FieldBundle} object.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: l_fieldCount, i
    type(ESMF_Field), pointer     :: l_fieldList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    nullify(l_fieldList)
    
    ! Get all the fields
    call ESMF_FieldBundleGet(fieldbundle, fieldCount=l_fieldCount,  rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    allocate(l_fieldList(l_fieldCount), stat=localrc)
    if(localrc /= ESMF_SUCCESS) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg = " - cannot allocate l_fieldList internally", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    call ESMF_ContainerGet(fieldbundle%this%container, &
      itemList=l_fieldList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    do i = 1, l_fieldCount
      call ESMF_FieldPrint(l_fieldList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    deallocate(l_fieldList)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_FieldBundlePrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRead()"
!BOP
! !IROUTINE: ESMF_FieldBundleRead - Read Fields to an fieldbundle from file(s)
! \label{api:FieldBundleRead}

! !INTERFACE:
  subroutine ESMF_FieldBundleRead(fieldbundle, file, &
    singleFile, iofmt, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)             :: fieldbundle
    character(*),           intent(in)             :: file
    logical,                intent(in),  optional  :: singleFile
    type(ESMF_IOFmtFlag),   intent(in),  optional  :: iofmt
    integer,                intent(out), optional  :: rc
!         
!
!
! !DESCRIPTION:
!   Read field data to an fieldbundle object from file(s).
!   For this API to be functional, the environment variable {\tt ESMF\_PIO} 
!   should be set to "internal" when the ESMF library is built.
!   Please see the section on Data I/O,~\ref{io:dataio}.
!
!   Limitations:
!   \begin{itemize}
!     \item Only 1 DE per PET supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[fieldbundle] 
!     An {\tt ESMF\_FieldBundle} object.
!   \item[file]
!     The name of the file from which fieldbundle data is read.
!   \item[{[singleFile]}]
!     A logical flag, the default is .true., i.e., all Fields in the bundle 
!     are stored in one single file. If .false., each field is stored 
!     in separate files; these files are numbered with the name based on the
!     argument "file". That is, a set of files are named: [file\_name]001,
!     [file\_name]002, [file\_name]003,...
!   \item[{[iofmt]}]
!     \begin{sloppypar}
!     The IO format. Please see Section~\ref{opt:iofmtflag} for the list
!     of options.  If not present, defaults to {\tt ESMF\_IOFMT\_NETCDF}.
!     \end{sloppypar}
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    character(len=80), allocatable :: Aname(:)
    integer :: fieldCount,i
    type(ESMF_Field), allocatable :: fieldList(:)
    logical                       :: singlef
    character(len=80)             :: filename
    character(len=3)              :: cnum
    type(ESMF_IOFmtFlag)          :: iofmtd

#ifdef ESMF_PIO
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    ! Check options
    singlef = .true.
    if (present(singleFile)) singlef = singleFile
    iofmtd = ESMF_IOFMT_NETCDF   ! default format
    if(present(iofmt)) iofmtd = iofmt

    call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    allocate (Aname(fieldCount))
    allocate (fieldList(fieldCount))
    call ESMF_FieldBundleGet(fieldbundle, fieldList=fieldList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    if (singlef) then
      ! Get and read the fields in the Bundle
      do i=1,fieldCount
       call ESMF_FieldGet(fieldList(i), name=Aname(i), rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
       call ESMF_FieldRead(fieldList(i), file=file, &
          iofmt=iofmtd, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    else
      do i=1,fieldCount
        write(cnum,"(i3.3)") i
        filename = file // cnum
        call ESMF_FieldGet(fieldList(i), name=Aname(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldRead(fieldList(i), file=filename,  &
               iofmt=iofmtd, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

#else
    ! Return indicating PIO not present
    if (present(rc)) rc = ESMF_RC_LIB_NOT_PRESENT
#endif

  end subroutine ESMF_FieldBundleRead
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRedist()"
!BOP
! !IROUTINE: ESMF_FieldBundleRedist - Execute a FieldBundle redistribution
!
! !INTERFACE:
  subroutine ESMF_FieldBundleRedist(srcFieldBundle, dstFieldBundle, &
    routehandle, keywordEnforcer, checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_FieldBundle), intent(in),    optional  :: srcFieldBundle
        type(ESMF_FieldBundle), intent(inout), optional  :: dstFieldBundle
        type(ESMF_RouteHandle), intent(inout)            :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),    optional  :: checkflag
        integer,                intent(out),   optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed FieldBundle redistribution from {\tt srcFieldBundle} to
!   {\tt dstFieldBundle}. Both {\tt srcFieldBundle} and {\tt dstFieldBundle} must be
!   weakly congruent and typekind conform with the respective FieldBundles used during 
!   {\tt ESMF\_FieldBundleRedistStore()}. Congruent FieldBundles possess
!   matching DistGrids and the shape of the local array tiles matches between
!   the FieldBundles for every DE. For weakly congruent Fields the sizes of the 
!   undistributed dimensions, that vary faster with memory than the first distributed 
!   dimension, are permitted to be different. This means that the same {\tt routehandle} 
!   can be applied to a large class of similar Fields that differ in the number of 
!   elements in the left most undistributed dimensions. 
!   \end{sloppypar}
!
!   It is erroneous to specify the identical FieldBundle object for {\tt srcFieldBundle} and
!   {\tt dstFieldBundle} arguments.
!
!   See {\tt ESMF\_FieldBundleRedistStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   For examples and associated documentations using this method see Section  
!   \ref{sec:fieldbundle:usage:redist_1dptr}. 
!
!   \begin{description}
!   \item [{[srcFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with source data.
!   \item [{[dstFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input FieldBundle pair will be checked for
!     consistency with the precomputed operation provided by {\tt routehandle}.
!     If set to {\tt .FALSE.} {\em (default)} only a very basic input check
!     will be performed, leaving many inconsistencies undetected. Set
!     {\tt checkflag} to {\tt .FALSE.} to achieve highest performance.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code
        
        ! local variables to buffer optional arguments
        logical                 :: l_checkflag! helper variable
        type(ESMF_Field)        :: l_srcField ! helper variable
        type(ESMF_Field)        :: l_dstField ! helper variable

        ! local internal variables
        logical                 :: src_bundle
        logical                 :: dst_bundle
        integer                 :: fcount, i
        type(ESMF_ArrayBundle)  :: srcab, dstab
        type(ESMF_Array), allocatable :: srca(:), dsta(:)

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments, deal with optional FieldBundle args
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

        ! Set default flags
        l_checkflag = ESMF_FALSE
        if (present(checkflag)) l_checkflag = checkflag

        src_bundle = .true.
        if (present(srcFieldBundle)) then
            ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc)

            call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=fcount, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            allocate(srca(fcount))
            do i = 1, fcount
                call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
                call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            enddo
            srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(srca)
        else
            src_bundle = .false.
      endif

      dst_bundle = .true.
      if (present(dstFieldBundle)) then
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc)

        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=fcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(dsta(fcount))
        do i = 1, fcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)
    else
        dst_bundle = .false.
    endif
        
    ! perform FieldBundle redistribution
    if(src_bundle .and. dst_bundle) &
        call ESMF_ArrayBundleRedist(srcab, dstab, routehandle, &
            checkflag=l_checkflag, rc=localrc)
    if(src_bundle .and. .not. dst_bundle) &
        call ESMF_ArrayBundleRedist(srcArrayBundle=srcab, routehandle=routehandle, &
            checkflag=l_checkflag, rc=localrc)
    if(.not. src_bundle .and. dst_bundle) &
        call ESMF_ArrayBundleRedist(dstArrayBundle=dstab, routehandle=routehandle, &
            checkflag=l_checkflag, rc=localrc)
    if(.not. src_bundle .and. .not. dst_bundle) &
        call ESMF_ArrayBundleRedist(routehandle=routehandle, &
            checkflag=l_checkflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    if (present(srcFieldBundle)) then
      call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(dstFieldBundle)) then
      call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldBundleRedist

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRedistRelease()"
!BOP
! !IROUTINE: ESMF_FieldBundleRedistRelease - Release resources associated with a FieldBundle redistribution
!
! !INTERFACE:
  subroutine ESMF_FieldBundleRedistRelease(routehandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(out),  optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Release resouces associated with a FieldBundle redistribution. After this call
!   {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
            
        ! Call into the RouteHandle code
        call ESMF_RouteHandleRelease(routehandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleRedistRelease

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute a FieldBundle redistribution with local factor argument 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldBundleRedistStore() 
! subroutine ESMF_FieldBundleRedistStore<type><kind>(srcFieldBundle, &
!   dstFieldBundle, & routehandle, factor, keywordEnforcer, &
!   srcToDstTransposeMap, rc) 
! 
! !ARGUMENTS: 
!   type(ESMF_FieldBundle), intent(in)             :: srcFieldBundle  
!   type(ESMF_FieldBundle), intent(inout)          :: dstFieldBundle  
!   type(ESMF_RouteHandle), intent(inout)          :: routehandle
!   <type>(ESMF_KIND_<kind>), intent(in)           :: factor
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   integer,                intent(in),   optional :: srcToDstTransposeMap(:)
!   integer,                intent(out),  optional :: rc 
! 
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION: 
! 
! Store a FieldBundle redistribution operation from {\tt srcFieldBundle} to {\tt dstFieldBundle}. 
! PETs that
! specify a {\tt factor} argument must use the <type><kind> overloaded interface. Other 
! PETs call into the interface without {\tt factor} argument. If multiple PETs specify 
! the {\tt factor} argument its type and kind as well as its value must match across 
! all PETs. If none of the PETs specifies a {\tt factor} argument the default will be a  
! factor of 1. 
!  
! Both {\tt srcFieldBundle} and {\tt dstFieldBundle} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! tiles within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! Redistribution corresponds to an identity mapping of the source FieldBundle vector to 
! the destination FieldBundle vector. 
!  
! Source and destination FieldBundles may be of different <type><kind>. Further source 
! and destination FieldBundles may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical FieldBundle object for srcFieldBundle 
! and dstFieldBundle arguments. 
!  
! The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
! {\tt ESMF\_FieldBundleRedist()} on any pair of FieldBundles that are congruent and typekind 
! conform with the srcFieldBundle, dstFieldBundle pair. Congruent FieldBundles possess matching 
! DistGrids and the shape of the local array tiles matches between the FieldBundles for 
! every DE. For weakly congruent Fields the sizes of the 
!   undistributed dimensions, that vary faster with memory than the first distributed 
!   dimension, are permitted to be different. This means that the same {\tt routehandle} 
!   can be applied to a large class of similar Fields that differ in the number of 
!   elements in the left most undistributed dimensions. 

!
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!  
! This call is collective across the current VM.  
! 
! For examples and associated documentations using this method see Section  
! \ref{sec:fieldbundle:usage:redist_1dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item [srcFieldBundle]  
!       {\tt ESMF\_FieldBundle} with source data. 
! \item [dstFieldBundle] 
!       {\tt ESMF\_FieldBundle} with destination data. The data in this 
!       FieldBundle may be destroyed by this call. 
! \item [routehandle] 
!       Handle to the precomputed Route. 
! \item [factor]
!       FActor by which to multiply source data. Default is 1.
! \item [{[srcToDstTransposeMap]}] 
!       List with as many entries as there are dimensions in {\tt srcFieldBundle}. Each 
! entry maps the corresponding {\tt srcFieldBundle} dimension 
! against the specified {\tt dstFieldBundle} 
! dimension. Mixing of distributed and undistributed dimensions is supported.  
! \item [{[rc]}]  
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleRedistStoreI4" 
!BOPI
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute a FieldBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleRedistStore()
    subroutine ESMF_FieldBundleRedistStoreI4(srcFieldBundle, &
      dstFieldBundle, routehandle, factor, keywordEnforcer, &
      srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)          :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)       :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)       :: routehandle
        integer(ESMF_KIND_I4),  intent(in)          :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,              intent(in) , optional :: srcToDstTransposeMap(:)
        integer,              intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=sfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=dfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(sfcount /= dfcount) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        ! Retrieve source and destination fields. 
        ! TODO: change loop end if necessary

        allocate(srca(sfcount))
        do i = 1, sfcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        allocate(dsta(dfcount))
        do i = 1, dfcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap=srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRedistStoreI4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleRedistStoreI8" 
!BOPI
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute a FieldBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleRedistStore()
    subroutine ESMF_FieldBundleRedistStoreI8(srcFieldBundle, dstFieldBundle, & 
      routehandle, factor, keywordEnforcer, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)         :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)      :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)      :: routehandle
        integer(ESMF_KIND_I8),  intent(in)         :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,             intent(in) , optional :: srcToDstTransposeMap(:)
        integer,             intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=sfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=dfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(sfcount /= dfcount) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        allocate(srca(sfcount))
        do i = 1, sfcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        allocate(dsta(dfcount))
        do i = 1, dfcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap=srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRedistStoreI8
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleRedistStoreR4"
!BOPI
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute a FieldBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleRedistStore()
    subroutine ESMF_FieldBundleRedistStoreR4(srcFieldBundle, dstFieldBundle, & 
      routehandle, factor, keywordEnforcer, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)          :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)       :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)       :: routehandle
        real(ESMF_KIND_R4),   intent(in)            :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,              intent(in) , optional :: srcToDstTransposeMap(:)
        integer,              intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=sfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=dfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(sfcount /= dfcount) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        allocate(srca(sfcount))
        do i = 1, sfcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        allocate(dsta(dfcount))
        do i = 1, dfcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap=srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRedistStoreR4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleRedistStoreR8"
!BOPI
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute a FieldBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleRedistStore()
    subroutine ESMF_FieldBundleRedistStoreR8(srcFieldBundle, dstFieldBundle, & 
        routehandle, factor, keywordEnforcer, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R8),     intent(in)            :: factor
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(in) , optional :: srcToDstTransposeMap(:)
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=sfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=dfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(sfcount /= dfcount) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        allocate(srca(sfcount))
        do i = 1, sfcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        allocate(dsta(dfcount))
        do i = 1, dfcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap=srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRedistStoreR8

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute a FieldBundle redistribution with local factor argument 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldBundleRedistStore() 
! subroutine ESMF_FieldBundleRedistStoreNF(srcFieldBundle, dstFieldBundle, & 
!        routehandle, factor, keywordEnforcer, srcToDstTransposeMap, rc) 
! 
! !ARGUMENTS: 
!   type(ESMF_FieldBundle), intent(in)             :: srcFieldBundle  
!   type(ESMF_FieldBundle), intent(inout)          :: dstFieldBundle  
!   type(ESMF_RouteHandle), intent(inout)          :: routehandle
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   integer,                intent(in),   optional :: srcToDstTransposeMap(:)
!   integer,                intent(out),  optional :: rc 
! 
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION: 
!
! \begin{sloppypar}
! Store a FieldBundle redistribution operation from {\tt srcFieldBundle}
! to {\tt dstFieldBundle}. PETs that specify non-zero matrix coefficients must use
! the <type><kind> overloaded interface and provide the {\tt factorList} and
! {\tt factorIndexList} arguments. Providing {\tt factorList} and
! {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
! {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
! PET does not provide matrix elements. Alternatively, PETs that do not 
! provide matrix elements may also call into the overloaded interface
! {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
! \end{sloppypar}
! 
! Both {\tt srcFieldBundle} and {\tt dstFieldBundle} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! tiles within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! Redistribution corresponds to an identity mapping of the source FieldBundle vector to 
! the destination FieldBundle vector. 
!  
! Source and destination Fields may be of different <type><kind>. Further source 
! and destination Fields may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical FieldBundle object for srcFieldBundle and dstFieldBundle 
! arguments. 
!  
! The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
! {\tt ESMF\_FieldBundleRedist()} on any pair of Fields that are congruent and typekind 
! conform with the srcFieldBundle, dstFieldBundle pair. Congruent Fields possess matching 
! DistGrids and the shape of the local array tiles matches between the Fields for 
! every DE. For weakly congruent Fields the sizes of the 
!   undistributed dimensions, that vary faster with memory than the first distributed 
!   dimension, are permitted to be different. This means that the same {\tt routehandle} 
!   can be applied to a large class of similar Fields that differ in the number of 
!   elements in the left most undistributed dimensions. 

!  
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!
! This call is collective across the current VM.  
! 
! For examples and associated documentations using this method see Section  
! \ref{sec:fieldbundle:usage:redist_1dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item [srcFieldBundle]  
!       {\tt ESMF\_FieldBundle} with source data. 
! \item [dstFieldBundle] 
!       {\tt ESMF\_FieldBundle} with destination data. The data in this 
!       FieldBundle may be destroyed by this call. 
! \item [routehandle] 
!       Handle to the precomputed Route. 
! \item [{[srcToDstTransposeMap]}] 
!       List with as many entries as there are dimensions in {\tt srcFieldBundle}. Each 
! entry maps the corresponding {\tt srcFieldBundle} dimension 
! against the specified {\tt dstFieldBundle} 
! dimension. Mixing of distributed and undistributed dimensions is supported.  
! \item [{[rc]}]  
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleRedistStoreNF" 
!BOPI
! !IROUTINE: ESMF_FieldBundleRedistStore - Precompute a FieldBundle redistribution
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleRedistStore()
    subroutine ESMF_FieldBundleRedistStoreNF(srcFieldBundle, dstFieldBundle, & 
      routehandle, keywordEnforcer, srcToDstTransposeMap, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(in) , optional :: srcToDstTransposeMap(:)
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=sfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=dfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(sfcount /= dfcount) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        ! TODO:
        ! internal grids match
        !if(ESMF_GridMatch(srcFieldBundle%btypep%grid, dstFieldBundle%btypep%grid) then
        !    call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
        !       "src and dst FieldBundle must have matching grid", &
        !        ESMF_CONTEXT, rcToReturn=rc)
        !    return
        !endif 

        allocate(srca(sfcount))
        do i = 1, sfcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        allocate(dsta(dfcount))
        do i = 1, dfcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, & 
            srcToDstTransposeMap=srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRedistStoreNF
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegrid()"
!BOP
! !IROUTINE: ESMF_FieldBundleRegrid - Execute a FieldBundle regrid operation
!
! !INTERFACE:
  subroutine ESMF_FieldBundleRegrid(srcFieldBundle, dstFieldBundle, &
         routehandle, keywordEnforcer, zeroflag, checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_FieldBundle), intent(in),    optional  :: srcFieldBundle
        type(ESMF_FieldBundle), intent(inout), optional  :: dstFieldBundle
        type(ESMF_RouteHandle), intent(inout)            :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        type(ESMF_RegionFlag),  intent(in),    optional  :: zeroflag
        logical,                intent(in),    optional  :: checkflag
        integer,                intent(out),   optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed FieldBundle regrid from {\tt srcFieldBundle} to
!   {\tt dstFieldBundle}. Both {\tt srcFieldBundle} and {\tt dstFieldBundle} must be
!   congruent and typekind conform with the respective FieldBundles used during 
!   {\tt ESMF\_FieldBundleRegridStore()}. Congruent FieldBundles possess
!   matching DistGrids and the shape of the local array tiles matches between
!   the FieldBundles for every DE. For weakly congruent Fields the sizes of the 
!   undistributed dimensions, that vary faster with memory than the first distributed 
!   dimension, are permitted to be different. This means that the same {\tt routehandle} 
!   can be applied to a large class of similar Fields that differ in the number of 
!   elements in the left most undistributed dimensions. 
!   \end{sloppypar}
!
!   It is erroneous to specify the identical FieldBundle object for {\tt srcFieldBundle} and
!   {\tt dstFieldBundle} arguments.
!
!   See {\tt ESMF\_FieldBundleRegridStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [{[srcFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with source data.
!   \item [{[dstFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[zeroflag]}]
!     \begin{sloppypar}
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of
!     all DEs in {\tt dstFieldBundle} will be initialized to zero before updating the 
!     elements with the results of the sparse matrix multiplication. If set to
!     {\tt ESMF\_REGION\_EMPTY} the elements in {\tt dstFieldBundle} will not be
!     modified prior to the sparse matrix multiplication and results will be
!     added to the incoming element values. Setting {\tt zeroflag} to 
!     {\tt ESMF\_REGION\_SELECT} will only zero out those elements in the 
!     destination FieldBundle that will be updated by the sparse matrix
!     multiplication. See section \ref{opt:regionflag} for a complete list of
!     valid settings.
!     \end{sloppypar}
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input FieldBundle pair will be checked for
!     consistency with the precomputed operation provided by {\tt routehandle}.
!     If set to {\tt .FALSE.} {\em (default)} only a very basic input check
!     will be performed, leaving many inconsistencies undetected. Set
!     {\tt checkflag} to {\tt .FALSE.} to achieve highest performance.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code
        
        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments, deal with optional FieldBundle args
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        call ESMF_FieldBundleSMM(srcFieldBundle=srcFieldBundle, &
          dstFieldBundle=dstFieldBundle, routehandle=routehandle, &
          zeroflag=zeroflag, checkflag=checkflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleRegrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegridRelease()"
!BOP
! !IROUTINE: ESMF_FieldBundleRegridRelease - Release resources associated with a FieldBundle regrid operation
!
! !INTERFACE:
  subroutine ESMF_FieldBundleRegridRelease(routehandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(out),  optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Release resouces associated with a FieldBundle regrid operation. After this call
!   {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

        ! Call into the RouteHandle code
        call ESMF_RouteHandleRelease(routehandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleRegridRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegridStore()"
!BOP
! !IROUTINE: ESMF_FieldBundleRegridStore - Precompute a FieldBundle regrid operation
!
! !INTERFACE:
    subroutine ESMF_FieldBundleRegridStore(srcFieldBundle, dstFieldBundle, &
                                           srcMaskValues, dstMaskValues, &
                                           regridMethod, regridPoleType, &
                                           regridPoleNPnts, regridScheme, &
                                           unmappedDstAction, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle),    intent(in)              :: srcFieldBundle
    type(ESMF_FieldBundle),    intent(inout)           :: dstFieldBundle
    integer(ESMF_KIND_I4),     intent(in),    optional :: srcMaskValues(:)
    integer(ESMF_KIND_I4),     intent(in),    optional :: dstMaskValues(:)
    type(ESMF_RegridMethod),   intent(in),    optional :: regridMethod
    type(ESMF_RegridPole),     intent(in),    optional :: regridPoleType
    integer,                   intent(in),    optional :: regridPoleNPnts
    integer,                   intent(in),    optional :: regridScheme
    type(ESMF_UnmappedAction), intent(in),    optional :: unmappedDstAction
    type(ESMF_RouteHandle),    intent(inout), optional :: routehandle
    integer,                   intent(out),   optional :: rc
!
!
! !DESCRIPTION:
!   Store a FieldBundle regrid operation over the data in {\tt srcFieldBundle} and
!   {\tt dstFieldBundle} pair. 
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldBundleRegrid()} on any FieldBundle pairs that are weakly congruent
!   and typekind conform to the FieldBundle pair used here.
!   Congruency for FieldBundles is
!   given by the congruency of its constituents.
!   Congruent Fields possess matching DistGrids, and the shape of the local
!   array tiles matches between the Fields for every DE. For weakly congruent
!   Fields the sizes of the undistributed dimensions, that vary faster with
!   memory than the first distributed dimension, are permitted to be different.
!   This means that the same {\tt routehandle} can be applied to a large class
!   of similar Fields that differ in the number of elements in the left most
!   undistributed dimensions.
!   Note {\tt ESMF\_FieldBundleRegridStore()} assumes the coordinates used in the Grids 
!   upon which the FieldBundles are built are in degrees.  

!  
!   This call is {\em collective} across the current VM.  
!
!   \begin{description}
!   \item [srcFieldbundle]
!     Source {\tt ESMF\_FieldBundle} containing data to be regridded.
!  \item [{[srcMaskValues]}]
!     List of values that indicate a source point should be masked out. 
!     If not specified, no masking will occur. 
!   \item [dstFieldbundle]
!     Destination {\tt ESMF\_FieldBundle}.
!  \item [{[dstMaskValues]}]
!     List of values that indicate a destination point should be masked out. 
!     If not specified, no masking will occur.
!  \item [{[unmappedDstAction]}]
!    Specifies what should happen if there are destination points that
!    can't be mapped to a source cell. Options are 
!    {\tt ESMF\_UNMAPPEDACTION\_ERROR} or 
!    {\tt ESMF\_UNMAPPEDACTION\_IGNORE}. If not specified, defaults 
!    to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. 
!   \item [{[regridMethod]}]
!     The type of interpolation. Please see Section~\ref{opt:regridmethod} for a list of
!     valid options. If not specified, defaults to {\tt ESMF\_REGRID\_METHOD\_BILINEAR}.
!   \item [{[regridPoleType]}]
!    Which type of artificial pole
!    to construct on the source Grid for regridding. Only valid when {\tt regridScheme} is set to 
!    {\tt ESMF\_REGRID\_SCHEME\_FULL3D}.  Please see Section~\ref{opt:regridpole} for a list of
!    valid options. If not specified, defaults to {\tt ESMF\_REGRIDPOLE\_ALLAVG}. 
!   \item [{[regridPoleNPnts]}]
!    If {\tt regridPoleType} is {\tt ESMF\_REGRIDPOLE\_NPNTAVG}.
!    This parameter indicates how many points should be averaged
!    over. Must be specified if {\tt regridPoleType} is 
!    {\tt ESMF\_REGRIDPOLE\_NPNTAVG}.
!   \item [{[regridScheme]}]
!     Whether to convert to spherical coordinates (ESMF\_REGRID\_SCHEME\_FULL3D), 
!     or to leave in native coordinates (ESMF\_REGRID\_SCHEME\_NATIVE). 
!   \item [{[routehandle]}]
!     Handle to the precomputed Route.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        ! internal local variables 
        integer                                       :: localrc, i, localDeCount, fieldCount
        integer                                       :: rraShift, vectorLengthShift 
        integer                                       :: sfcount, dfcount
        type(ESMF_Field)                              :: srcField, dstField
        type(ESMF_RouteHandle)                        :: rh
        logical         :: havePrev, matchesPrev, isGridPair
        type(ESMF_Grid) :: prevSrcGrid, prevDstGrid
        type(ESMF_StaggerLoc) :: prevSrcStaggerLoc, prevDstStaggerLoc
        type(ESMF_Grid) :: currSrcGrid, currDstGrid
        type(ESMF_StaggerLoc) :: currSrcStaggerLoc, currDstStaggerLoc
        integer(ESMF_KIND_I4), pointer :: tmp_indices(:,:)
        integer(ESMF_KIND_I4), pointer :: prev_indices(:,:)
        real(ESMF_KIND_R8), pointer :: prev_weights(:)
        type(ESMF_GeomType) :: srcGeomtype        
        type(ESMF_GeomType) :: dstGeomtype        
        integer :: j

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=sfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=dfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(sfcount /= dfcount) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of Fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        ! init some variables for optimization
        havePrev=.false.
        matchesPrev=.false.

        fieldCount = sfcount

        if (present(routehandle)) then
          routehandle = ESMF_RouteHandleCreate(rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
          call ESMF_RouteHandlePrepXXE(routehandle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif        

        ! 6) loop over all Fields in FieldBundles, call FieldRegridStore and append rh
        rraShift = 0          ! reset
        vectorLengthShift = 0 ! reset
        do i=1, fieldCount
      
          ! obtain srcField
          call ESMF_FieldBundleGet(srcFieldBundle, fieldIndex=i, field=srcField, &
            rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          
          ! obtain dstField
          call ESMF_FieldBundleGet(dstFieldBundle, fieldIndex=i, field=dstField, &
            rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          
          ! If these are both Grids, then check for optimization
          call ESMF_FieldGet(srcField, geomType=srcGeomType, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
               ESMF_CONTEXT, rcToReturn=rc)) return

          call ESMF_FieldGet(dstField, geomType=dstGeomType, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
               ESMF_CONTEXT, rcToReturn=rc)) return

          isGridPair=.false.
          if ((srcGeomType==ESMF_GEOMTYPE_GRID) .and. (dstGeomType==ESMF_GEOMTYPE_GRID)) then
             ! Mark that this is a pair of grids and therefore optimizable
             isGridPair=.true.

             ! Get Grids and staggerlocs
             call ESMF_FieldGet(srcField, grid=currSrcGrid, staggerloc=currSrcStaggerloc, rc=localrc)
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
               ESMF_CONTEXT, rcToReturn=rc)) return

             call ESMF_FieldGet(dstField, grid=currDstGrid, staggerloc=currDstStaggerloc, rc=localrc)
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
               ESMF_CONTEXT, rcToReturn=rc)) return

             ! see if grids match prev grids
             matchesPrev=.false.
             if (havePrev) then
                if ((currSrcStaggerLoc==prevSrcStaggerLoc) .and.  &
                    (currDstStaggerLoc==prevDstStaggerLoc)) then 

                   ! TODO: This only needs to consider matching the Field staggerlocs in the Grid
                   !       and it only needs to match the coordinates and distribution
                   !       Reimplement as a FieldMatch() with an EXACTMAT output????
                   if ((ESMF_GridMatch(currSrcGrid, prevSrcGrid)==ESMF_GRIDMATCH_EXACT) .and. &
                       (ESMF_GridMatch(currDstGrid, prevDstGrid)==ESMF_GRIDMATCH_EXACT)) then
                      matchesPrev=.true.
                   endif
                endif
             endif
          endif

          ! precompute regrid operation based on grids, etc.
          if (isGridPair) then
             if (matchesPrev) then
                call ESMF_FieldSMMStore(srcField=srcField, dstField=dstField, & 
                        routehandle=rh, &
                        factorList=prev_weights, factorIndexList=prev_indices, &
                        rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                     ESMF_CONTEXT, rcToReturn=rc)) return
             else ! If it doesn't match make a new previous
                ! if we have them, get rid of old matrix
                if (havePrev) then
                   deallocate(prev_indices)
                   deallocate(prev_weights)
                endif

                ! Get routeHandle as well as matrix info
                call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
                     srcMaskValues=srcMaskValues, dstMaskValues=dstMaskValues, &
                     regridMethod=regridMethod, &
                     regridPoleType=regridPoleType, regridPoleNPnts=regridPoleNPnts, &
                     regridScheme=regridScheme, &
                     unmappedDstAction=unmappedDstAction, &
                     routehandle=rh, &
                     indices=tmp_indices, weights=prev_weights, &
                     rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                     ESMF_CONTEXT, rcToReturn=rc)) return

                ! Swap order
                ! TODO: fix order in regrid
                allocate(prev_indices(2,size(tmp_indices,1)))
                do j=1,size(tmp_indices,1)
                   prev_indices(1,j)=tmp_indices(j,1)
                   prev_indices(2,j)=tmp_indices(j,2)
                enddo
                deallocate(tmp_indices)

                ! Mark as prev and record info about prev
                havePrev=.true.
                prevSrcStaggerLoc=currSrcStaggerLoc
                prevDstStaggerLoc=currDstStaggerLoc
                prevSrcGrid=currSrcGrid 
                prevDstGrid=currDstGrid 
             endif
          else ! If not a grid pair no optimization at this point         
             call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
                  srcMaskValues=srcMaskValues, dstMaskValues=dstMaskValues, &
                  regridMethod=regridMethod, &
                  regridPoleType=regridPoleType, regridPoleNPnts=regridPoleNPnts, &
                  regridScheme=regridScheme, &
                  unmappedDstAction=unmappedDstAction, &
                  routehandle=rh,rc=localrc)
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
           endif

          ! append rh to routehandle and clear rh
          if (present(routehandle)) then
            call ESMF_RouteHandleAppendClear(routehandle, appendRoutehandle=rh, &
              rraShift=rraShift, vectorLengthShift=vectorLengthShift, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
           endif
        
          ! adjust rraShift and vectorLengthShift
          call ESMF_FieldGet(srcField, localDeCount=localDeCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          rraShift = rraShift + localDeCount
          call ESMF_FieldGet(dstField, localDeCount=localDeCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          rraShift = rraShift + localDeCount
          vectorLengthShift = vectorLengthShift + 1
        enddo

        ! if we have them, get rid of old matrix
        if (havePrev) then
           deallocate(prev_indices)
           deallocate(prev_weights)
        endif

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleRegridStore
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRemove()"
!BOP
! !IROUTINE: ESMF_FieldBundleRemove - Remove Fields from fieldbundle
!
! !INTERFACE:
    subroutine ESMF_FieldBundleRemove(fieldbundle, fieldNameList, &
      keywordEnforcer, multiflag, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)         :: fieldbundle
    character(len=*),       intent(in)            :: fieldNameList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: multiflag
    logical,                intent(in),  optional :: relaxedflag
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Remove field(s) by name from fieldbundle. In the relaxed setting it is 
!   {\em not} an error if {\tt fieldNameList} contains names that are not 
!   found in {\tt fieldbundle}.
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} from which to remove items.
!   \item [fieldNameList]
!     List of items to remove.
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple Fields with the same name
!     to be removed from {\tt fieldbundle}. For {\tt .false.}, items to be
!     removed must have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "remove"
!     where it is {\em not} an error if {\tt fieldNameList} contains item
!     names that are not found in {\tt fieldbundle}. For {\tt .false.} this is 
!     treated as an error condition. 
!     Further, in {\tt multiflag=.false.} mode, the relaxed definition of
!     "remove" also covers the case where there are multiple items in
!     {\tt fieldbundle} that match a single entry in {\tt fieldNameList}.
!     For {\tt relaxedflag=.false.} this is treated as an error condition.
!     The default setting is {\tt .false.}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: fieldCount, i
    type(ESMF_Logical)            :: linkChange
    type(ESMF_Field), pointer     :: garbageList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    nullify(garbageList)

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)
    
    call ESMF_ContainerGarbageOn(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerGarbageClear(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    ! Call into ContainerRemove
    call ESMF_ContainerRemove(fieldbundle%this%container, fieldNameList, &
      multiflag=multiflag, relaxedflag=relaxedflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerGarbageGet(fieldbundle%this%container, garbageList=garbageList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerGarbageOff(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Remove those that were removed
    if(associated(garbageList)) then
      do i=1, size(garbageList)
        call c_ESMC_AttributeLinkRemove(fieldbundle%this%base, garbageList(i)%ftypep%base, linkChange, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc))  return
      enddo
      deallocate(garbageList)
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleRemove
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleReplace()"
!BOP
! !IROUTINE: ESMF_FieldBundleReplace - Replace Fields in fieldbundle
!
! !INTERFACE:
    subroutine ESMF_FieldBundleReplace(fieldbundle, fieldList, &
      keywordEnforcer, multiflag, relaxedflag, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)         :: fieldbundle
    type(ESMF_Field),       intent(in)            :: fieldList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: multiflag
    logical,                intent(in),  optional :: relaxedflag
    integer,                intent(out), optional :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Replace field(s) by name in fieldbundle. In the relaxed setting it is not
!   an error if {\tt fieldList} contains Fields that do not match by name any
!   item in {\tt fieldbundle}. These Fields are simply ignored in this case.
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} in which to replace items.
!   \item [fieldList]
!     List of items to replace.
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple items with the same name
!     to be replaced in {\tt fieldbundle}. For {\tt .false.}, items to be
!     replaced must have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "replace"
!     where it is {\em not} an error if {\tt fieldList} contains items with
!     names that are not found in {\tt fieldbundle}. These items in 
!     {\tt fieldList} are ignored in the relaxed mode. For {\tt .false.} this
!     is treated as an error condition.
!     Further, in {\tt multiflag=.false.} mode, the relaxed definition of
!     "replace" also covers the case where there are multiple items in
!     {\tt fieldbundle} that match a single entry by name in {\tt fieldList}.
!     For {\tt relaxedflag=.false.} this is treated as an error condition.
!     The default setting is {\tt .false.}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: fieldCount, i
    type(ESMF_Logical)            :: linkChange
    type(ESMF_Field), pointer     :: garbageList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    nullify(garbageList)

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    call ESMF_ContainerGarbageOn(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerGarbageClear(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into ContainerReplace
    call ESMF_ContainerReplace(fieldbundle%this%container, fieldList, &
      multiflag=multiflag, relaxedflag=relaxedflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerGarbageGet(fieldbundle%this%container, garbageList=garbageList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ContainerGarbageOff(fieldbundle%this%container, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Remove those that were replaced
    if(associated(garbageList)) then
      do i=1, size(garbageList)
        call c_ESMC_AttributeLinkRemove(fieldbundle%this%base, garbageList(i)%ftypep%base, linkChange, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc))  return
      enddo
      deallocate(garbageList)
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleReplace
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetGrid"
!BOPI
! !IROUTINE: ESMF_FieldBundleSet - Associate a Grid with an empty FieldBundle
! 
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSet()
      subroutine ESMF_FieldBundleSetGrid(fieldbundle, grid, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout)         :: fieldbundle
      type(ESMF_Grid),        intent(in)            :: grid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Sets the {\tt grid} for a {\tt fieldbundle} that contains no {\tt ESMF\_Field}s. 
!   All {\tt ESMF\_Field}s added to this {\tt fieldbundle} must be
!   associated with the same {\tt ESMF\_Grid}.  Returns an error if 
!   there is already an {\tt ESMF\_GeomBase} object associated with the {\tt fieldbundle}.
!   \end{sloppypar}
!
!   The arguments are:
!   \begin{description}
!   \item [fieldbundle]
!        An {\tt ESMF\_FieldBundle} object.
!   \item [grid]
!        The {\tt ESMF\_Grid} which all {\tt ESMF\_Field}s added to this
!        {\tt ESMF\_FieldBundle} must have.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOPI


      integer :: status                           ! Error status
      type(ESMF_FieldBundleType), pointer :: btype     ! internal data
      type(ESMF_Logical) :: linkChange

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      btype => fieldbundle%this
   
      ! here we will only let someone associate a grid with a fieldbundle
      ! if there is not one already associated with it.  
      if (btype%status == ESMF_FBSTATUS_GRIDSET) then
        if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
          msg="FieldBundle is already associated with a Geombase", &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! Create the geombase around the grid, use the center stagger as a generic stagger here, 
      ! because the stagger won't really matter in this case
      btype%geombase=ESMF_GeomBaseCreate(grid,ESMF_STAGGERLOC_CENTER,rc=status)
      if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc))  return

      ! Set Status to containing a Grid
      btype%status = ESMF_FBSTATUS_GRIDSET

      !  link the Attribute hierarchies
      linkChange = ESMF_TRUE
      call c_ESMC_AttributeLink(btype%base, grid, linkChange, status)
      if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetGrid
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetMesh"
!BOPI
! !IROUTINE: ESMF_FieldBundleSet - Associate a Mesh with an empty FieldBundle
! 
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSet()
      subroutine ESMF_FieldBundleSetMesh(fieldbundle, mesh, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout)         :: fieldbundle
      type(ESMF_Mesh),        intent(in)            :: mesh
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Sets the {\tt mesh} for a {\tt fieldbundle} that contains no {\tt ESMF\_Field}s. 
!   All {\tt ESMF\_Field}s added to this {\tt fieldbundle} must be
!   associated with the same {\tt ESMF\_Mesh}.  Returns an error if 
!   there is already an {\tt ESMF\_Geombase} associated with the {\tt fieldbundle}.
!   \end{sloppypar}
!
!   The arguments are:
!   \begin{description}
!   \item [fieldbundle]
!        An {\tt ESMF\_FieldBundle} object.
!   \item [mesh]
!        The {\tt ESMF\_Mesh} which all {\tt ESMF\_Field}s added to this
!        {\tt ESMF\_FieldBundle} must have.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOPI


      integer :: status                           ! Error status
      type(ESMF_FieldBundleType), pointer :: btype     ! internal data

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)

      btype => fieldbundle%this

      ! here we will only let someone associate a grid with a fieldbundle
      ! if there is not one already associated with it.  
      if (btype%status == ESMF_FBSTATUS_GRIDSET) then
        if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
          msg="FieldBundle is already associated with a Geombase", &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! Create the geombase around the grid, use the center stagger as a generic stagger here, 
      ! because the stagger won't really matter in this case
      btype%geombase=ESMF_GeomBaseCreate(mesh,rc=status)
      if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc))  return

      ! Set Status to containing a Geombase
      btype%status = ESMF_FBSTATUS_GRIDSET

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetMesh
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetLS"
!BOPI
! !IROUTINE: ESMF_FieldBundleSet - Associate a LocStream with an empty FieldBundle
! 
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSet()
      subroutine ESMF_FieldBundleSetLS(fieldbundle, locstream, &
        keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout)         :: fieldbundle
      type(ESMF_LocStream),   intent(in)            :: locstream
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Sets the {\tt locstream} for a {\tt fieldbundle} that contains no {\tt ESMF\_Field}s. 
!   All {\tt ESMF\_Field}s added to this {\tt fieldbundle} must be
!   associated with the same {\tt ESMF\_LocStream}.  Returns an error if 
!   there is already an {\tt ESMF\_Geombase} associated with the {\tt fieldbundle}.
!
!   The arguments are:
!   \begin{description}
!   \item [fieldbundle]
!        An {\tt ESMF\_FieldBundle} object.
!   \item [locstream]
!        The {\tt ESMF\_LocStream} which all {\tt ESMF\_Field}s added to this
!        {\tt ESMF\_FieldBundle} must have.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOPI


      integer :: status                           ! Error status
      type(ESMF_FieldBundleType), pointer :: btype     ! internal data

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

      btype => fieldbundle%this
   
      ! here we will only let someone associate a grid with a fieldbundle
      ! if there is not one already associated with it.  
      if (btype%status == ESMF_FBSTATUS_GRIDSET) then
        if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
          msg="FieldBundle is already associated with a Geombase", &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif

       ! Create the geombase around the locstream
       btype%geombase=ESMF_GeomBaseCreate(locstream,rc=status)
       if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      ! Set Status to containing a Geombase
      btype%status = ESMF_FBSTATUS_GRIDSET

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetLS
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetXGrid"
!BOPI
! !IROUTINE: ESMF_FieldBundleSet - Associate a XGrid with an empty FieldBundle
! 
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSet()
      subroutine ESMF_FieldBundleSetXGrid(fieldbundle, xgrid, &
        keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout)         :: fieldbundle
      type(ESMF_XGrid),   intent(in)                :: xgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Sets the {\tt locstream} for a {\tt fieldbundle} that contains no {\tt ESMF\_Field}s. 
!   All {\tt ESMF\_Field}s added to this {\tt fieldbundle} must be
!   associated with the same {\tt ESMF\_LocStream}.  Returns an error if 
!   there is already an {\tt ESMF\_Geombase} associated with the {\tt fieldbundle}.
!
!   The arguments are:
!   \begin{description}
!   \item [fieldbundle]
!        An {\tt ESMF\_FieldBundle} object.
!   \item [xgrid]
!        The {\tt ESMF\_XGrid} which all {\tt ESMF\_Field}s added to this
!        {\tt ESMF\_FieldBundle} must have.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOPI

      integer :: status                                ! Error status
      type(ESMF_FieldBundleType), pointer :: btype     ! internal data

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

      btype => fieldbundle%this
   
      ! here we will only let someone associate a grid with a fieldbundle
      ! if there is not one already associated with it.  
      if (btype%status == ESMF_FBSTATUS_GRIDSET) then
        if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
          msg="FieldBundle is already associated with a Geombase", &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif

       ! Create the geombase around the locstream
       btype%geombase=ESMF_GeomBaseCreate(xgrid, rc=status)
       if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      ! Set Status to containing a Geombase
      btype%status = ESMF_FBSTATUS_GRIDSET

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetXGrid
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSMM()"
!BOP
! !IROUTINE: ESMF_FieldBundleSMM - Execute a FieldBundle sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_FieldBundleSMM(srcFieldBundle, dstFieldBundle, &
        routehandle, keywordEnforcer, zeroflag, checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_FieldBundle), intent(in),    optional  :: srcFieldBundle
        type(ESMF_FieldBundle), intent(inout), optional  :: dstFieldBundle
        type(ESMF_RouteHandle), intent(inout)            :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        type(ESMF_RegionFlag),  intent(in),    optional  :: zeroflag
        logical,                intent(in),    optional  :: checkflag
        integer,                intent(out),   optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Execute a precomputed FieldBundle sparse matrix multiplication from {\tt srcFieldBundle} to
!   {\tt dstFieldBundle}. Both {\tt srcFieldBundle} and {\tt dstFieldBundle} must be
!   congruent and typekind conform with the respective FieldBundles used during 
!   {\tt ESMF\_FieldBundleSMMStore()}. Congruent FieldBundles possess
!   matching DistGrids and the shape of the local array tiles matches between
!   the FieldBundles for every DE. For weakly congruent Fields the sizes of the 
!   undistributed dimensions, that vary faster with memory than the first distributed 
!   dimension, are permitted to be different. This means that the same {\tt routehandle} 
!   can be applied to a large class of similar Fields that differ in the number of 
!   elements in the left most undistributed dimensions. 
!
!   It is erroneous to specify the identical FieldBundle object for {\tt srcFieldBundle} and
!   {\tt dstFieldBundle} arguments.
!
!   See {\tt ESMF\_FieldBundleSMMStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   For examples and associated documentations using this method see Section  
!   \ref{sec:fieldbundle:usage:smm_1dptr}. 
!
!   \begin{description}
!   \item [{[srcFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with source data.
!   \item [{[dstFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[zeroflag]}]
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of
!     all DEs in {\tt dstFieldBundle} will be initialized to zero before updating the 
!     elements with the results of the sparse matrix multiplication. If set to
!     {\tt ESMF\_REGION\_EMPTY} the elements in {\tt dstFieldBundle} will not be
!     modified prior to the sparse matrix multiplication and results will be
!     added to the incoming element values. Setting {\tt zeroflag} to 
!
!     {\tt ESMF\_REGION\_SELECT} will only zero out those elements in the 
!     destination FieldBundle that will be updated by the sparse matrix
!     multiplication. See section \ref{opt:regionflag} for a complete list of
!     valid settings.
!   \item [{[checkflag]}]
!     If set to {\tt .TRUE.} the input FieldBundle pair will be checked for
!     consistency with the precomputed operation provided by {\tt routehandle}.
!     If set to {\tt .FALSE.} {\em (default)} only a very basic input check
!     will be performed, leaving many inconsistencies undetected. Set
!     {\tt checkflag} to {\tt .FALSE.} to achieve highest performance.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code
        
        ! local variables to buffer optional arguments
        type(ESMF_RegionFlag)   :: l_zeroflag
        logical                 :: l_checkflag! helper variable
        type(ESMF_Field)        :: l_srcField ! helper variable
        type(ESMF_Field)        :: l_dstField ! helper variable

        ! local internal variables
        logical                 :: src_bundle
        logical                 :: dst_bundle
        integer                 :: fcount, i

        type(ESMF_ArrayBundle)  :: srcab, dstab
        type(ESMF_Array), allocatable :: srca(:), dsta(:)

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments, deal with optional FieldBundle args
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

        ! Set default flags
        l_checkflag = ESMF_FALSE
        if (present(checkflag)) l_checkflag = checkflag
        l_zeroflag = ESMF_REGION_TOTAL
        if (present(zeroflag)) l_zeroflag = zeroflag

        src_bundle = .true.
        if (present(srcFieldBundle)) then
            ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc)

            call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=fcount, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            allocate(srca(fcount))
            do i = 1, fcount
                call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
                call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            enddo
            srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(srca)
        else
            src_bundle = .false.
        endif

        dst_bundle = .true.
        if (present(dstFieldBundle)) then
            ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc)

            call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=fcount, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            allocate(dsta(fcount))
            do i = 1, fcount
                call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
                call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
                if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            enddo
            dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(dsta)
        else
            dst_bundle = .false.
        endif

        ! perform FieldBundle SMM
        if(src_bundle .and. dst_bundle) &
            call ESMF_ArrayBundleSMM(srcab, dstab, routehandle, &
                zeroflag=l_zeroflag, checkflag=l_checkflag, rc=localrc)
        if(src_bundle .and. .not. dst_bundle) &
            call ESMF_ArrayBundleSMM(srcArrayBundle=srcab, routehandle=routehandle, &
                zeroflag=l_zeroflag, checkflag=l_checkflag, rc=localrc)
        if(.not. src_bundle .and. dst_bundle) &
            call ESMF_ArrayBundleSMM(dstArrayBundle=dstab, routehandle=routehandle, &
                zeroflag=l_zeroflag, checkflag=l_checkflag, rc=localrc)
        if(.not. src_bundle .and. .not. dst_bundle) &
            call ESMF_ArrayBundleSMM(routehandle=routehandle, &
                zeroflag=l_zeroflag, checkflag=l_checkflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
            
        ! garbage collection
        if (present(srcFieldBundle)) then
          call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if (present(dstFieldBundle)) then
          call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleSMM

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSMMRelease()"
!BOP
! !IROUTINE: ESMF_FieldBundleSMMRelease - Release resources associated with a FieldBundle sparse matrix multiplication
!
! !INTERFACE:
  subroutine ESMF_FieldBundleSMMRelease(routehandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(out),  optional  :: rc
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Release resouces associated with a FieldBundle sparse matrix multiplication. After this call
!   {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
        integer                 :: localrc      ! local return code

        ! initialize return code; assume routine not implemented
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
            
        ! Call into the RouteHandle code
        call ESMF_RouteHandleRelease(routehandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleSMMRelease

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldBundleSMMStore - Precompute a FieldBundle sparse matrix multiplication with local factors
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldBundleSMMStore() 
! subroutine ESMF_FieldBundleSMMStore<type><kind>(srcFieldBundle, &
!   dstFieldBundle,  routehandle, factorList, factorIndexList, &
!   keywordEnforcer, rc) 
! 
! !ARGUMENTS: 
!   type(ESMF_FieldBundle),   intent(in)            :: srcFieldBundle  
!   type(ESMF_FieldBundle),   intent(inout)         :: dstFieldBundle  
!   type(ESMF_RouteHandle),   intent(inout)         :: routehandle
!   <type>(ESMF_KIND_<kind>), intent(in)            :: factorList(:) 
!   integer,                  intent(in),           :: factorIndexList(:,:) 
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   integer,                  intent(out), optional :: rc 
! 
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION: 
! 
! \begin{sloppypar}
! Store a FieldBundle sparse matrix multiplication operation from {\tt srcFieldBundle}
! to {\tt dstFieldBundle}. PETs that specify non-zero matrix coefficients must use
! the <type><kind> overloaded interface and provide the {\tt factorList} and
! {\tt factorIndexList} arguments. Providing {\tt factorList} and
! {\tt factorIndexList} arguments with 
! {\tt size(factorList) = (/0/)} and
! {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
! PET does not provide matrix elements. Alternatively, PETs that do not 
! provide matrix elements may also call into the overloaded interface
! {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
! \end{sloppypar}
!  
! Both {\tt srcFieldBundle} and {\tt dstFieldBundle} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! tiles within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! SMM corresponds to an identity mapping of the source FieldBundle vector to 
! the destination FieldBundle vector. 
!  
! Source and destination Fields may be of different <type><kind>. Further source 
! and destination Fields may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical FieldBundle object for srcFieldBundle 
! and dstFieldBundle arguments. 
!
! The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
! {\tt ESMF\_FieldBundleSMM()} on any pair of FieldBundles that are congruent and typekind 
! conform with the srcFieldBundle, dstFieldBundle pair. Congruent FieldBundles possess matching 
! DistGrids and the shape of the local array tiles matches between the FieldBundles for 
! every DE. For weakly congruent Fields the sizes of the 
!   undistributed dimensions, that vary faster with memory than the first distributed 
!   dimension, are permitted to be different. This means that the same {\tt routehandle} 
!   can be applied to a large class of similar Fields that differ in the number of 
!   elements in the left most undistributed dimensions. 
!  
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!  
! This call is collective across the current VM.  
! 
! For examples and associated documentations using this method see Section  
! \ref{sec:fieldbundle:usage:smm_1dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item [srcFieldBundle]  
!       {\tt ESMF\_FieldBundle} with source data. 
! \item [dstFieldBundle] 
!       {\tt ESMF\_FieldBundle} with destination data. The data in this 
!       FieldBundle may be destroyed by this call.
! \item [routehandle] 
!       Handle to the precomputed Route. 
! \item [factorList]
!       List of non-zero coefficients.
! \item [factorIndexList]
!     Pairs of sequence indices for the factors stored in {\tt factorList}.
!
!     \begin{sloppypar}
!     The second dimension of {\tt factorIndexList} steps through the list of
!     pairs, i.e. {\tt size(factorIndexList,2) == size(factorList)}. The first
!     dimension of {\tt factorIndexList} is either of size 2 or size 4.
!     \end{sloppypar}
!
!     In the {\em size 2 format} {\tt factorIndexList(1,:)} specifies the
!     sequence index of the source element in the {\tt srcFieldBundle} while
!     {\tt factorIndexList(2,:)} specifies the sequence index of the
!     destination element in {\tt dstFieldBundle}. For this format to be a valid
!     option source and destination FieldBundles must have matching number of
!     tensor elements (the product of the sizes of all Field tensor dimensions).
!     Under this condition an identiy matrix can be applied within the space of
!     tensor elements for each sparse matrix factor.
!
!     The {\em size 4 format} is more general and does not require a matching
!     tensor element count. Here the 
!
!     {\tt factorIndexList(1,:)} specifies the
!     sequence index while {\tt factorIndexList(2,:)} specifies the tensor
!     sequence index of the source element in the {\tt srcFieldBundle}. Further
!     {\tt factorIndexList(3,:)} specifies the sequence index and
!     {\tt factorIndexList(4,:)} specifies the tensor sequence index of the 
!     destination element in the {\tt dstFieldBundle}.
!
!     See section \ref{Array:SparseMatMul} for details on the definition of 
!     {\em sequence indices} and {\em tensor sequence indices}.
! \item [{[rc]}]  
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleSMMStoreI4" 
!BOPI
! !IROUTINE: ESMF_FieldBundleSMMStore - Precompute a FieldBundle sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleSMMStore()
    subroutine ESMF_FieldBundleSMMStoreI4(srcFieldBundle, dstFieldBundle, & 
        routehandle, factorList, factorIndexList, keywordEnforcer, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I4),  intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=sfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=dfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(sfcount /= dfcount) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        allocate(srca(sfcount))
        do i = 1, sfcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        allocate(dsta(dfcount))
        do i = 1, dfcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleSMMStore(srcab, dstab, routehandle, factorList, &
            factorIndexList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
            
        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleSMMStoreI4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleSMMStoreI8" 
!BOPI
! !IROUTINE: ESMF_FieldBundleSMMStore - Precompute a FieldBundle sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleSMMStore()
    subroutine ESMF_FieldBundleSMMStoreI8(srcFieldBundle, dstFieldBundle, & 
      routehandle, factorList, factorIndexList, keywordEnforcer, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I8),  intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=sfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=dfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(sfcount /= dfcount) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        allocate(srca(sfcount))
        do i = 1, sfcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        allocate(dsta(dfcount))
        do i = 1, dfcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleSMMStore(srcab, dstab, routehandle, factorList, &
            factorIndexList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
            
        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleSMMStoreI8
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleSMMStoreR4"
!BOPI
! !IROUTINE: ESMF_FieldBundleSMMStore - Precompute a FieldBundle sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleSMMStore()
    subroutine ESMF_FieldBundleSMMStoreR4(srcFieldBundle, dstFieldBundle, & 
      routehandle, factorList, factorIndexList, keywordEnforcer, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R4),     intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=sfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=dfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(sfcount /= dfcount) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        allocate(srca(sfcount))
        do i = 1, sfcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        allocate(dsta(dfcount))
        do i = 1, dfcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleSMMStore(srcab, dstab, routehandle, factorList, &
            factorIndexList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleSMMStoreR4
!------------------------------------------------------------------------------ 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleSMMStoreR8"
!BOPI
! !IROUTINE: ESMF_FieldBundleSMMStore - Precompute a FieldBundle sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleSMMStore()
    subroutine ESMF_FieldBundleSMMStoreR8(srcFieldBundle, dstFieldBundle, & 
      routehandle, factorList, factorIndexList, keywordEnforcer, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R8),     intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=sfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=dfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(sfcount /= dfcount) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        allocate(srca(sfcount))
        do i = 1, sfcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        allocate(dsta(dfcount))
        do i = 1, dfcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleSMMStore(srcab, dstab, routehandle, factorList, &
            factorIndexList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleSMMStoreR8

!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_FieldBundleSMMStore - Precompute a FieldBundle sparse matrix multiplication without local factors
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldBundleSMMStore() 
! subroutine ESMF_FieldBundleSMMStoreNF(srcFieldBundle, dstFieldBundle, & 
!        routehandle, keywordEnforcer, rc) 
! 
! !ARGUMENTS: 
!   type(ESMF_FieldBundle),   intent(in)            :: srcFieldBundle  
!   type(ESMF_FieldBundle),   intent(inout)         :: dstFieldBundle  
!   type(ESMF_RouteHandle),   intent(inout)         :: routehandle
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   integer,                  intent(out), optional :: rc 
! 
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION: 
!
! \begin{sloppypar}
! Store a FieldBundle sparse matrix multiplication operation from {\tt srcFieldBundle}
! to {\tt dstFieldBundle}. PETs that specify non-zero matrix coefficients must use
! the <type><kind> overloaded interface and provide the {\tt factorList} and
! {\tt factorIndexList} arguments. Providing {\tt factorList} and
! {\tt factorIndexList} arguments with {\tt size(factorList) = (/0/)} and
! {\tt size(factorIndexList) = (/2,0/)} or {\tt (/4,0/)} indicates that a 
! PET does not provide matrix elements. Alternatively, PETs that do not 
! provide matrix elements may also call into the overloaded interface
! {\em without} {\tt factorList} and {\tt factorIndexList} arguments.
! \end{sloppypar}
! 
! Both {\tt srcFieldBundle} and {\tt dstFieldBundle} are interpreted as sequentialized 
! vectors. The 
! sequence is defined by the order of DistGrid dimensions and the order of 
! tiles within the DistGrid or by user-supplied arbitrary sequence indices. See 
! section \ref{Array:SparseMatMul} for details on the definition of {\em sequence indices}. 
! SMM corresponds to an identity mapping of the source FieldBundle vector to 
! the destination FieldBundle vector. 
!  
! Source and destination Fields may be of different <type><kind>. Further source 
! and destination Fields may differ in shape, however, the number of elements 
! must match. 
!  
! It is erroneous to specify the identical FieldBundle object for srcFieldBundle and dstFieldBundle 
! arguments. 
!  
! The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
! {\tt ESMF\_FieldBundleSMM()} on any pair of FieldBundles that are congruent and typekind 
! conform with the srcFieldBundle, dstFieldBundle pair. Congruent FieldBundles possess matching 
! DistGrids and the shape of the local array tiles matches between the FieldBundles for 
! every DE. For weakly congruent Fields the sizes of the 
!   undistributed dimensions, that vary faster with memory than the first distributed 
!   dimension, are permitted to be different. This means that the same {\tt routehandle} 
!   can be applied to a large class of similar Fields that differ in the number of 
!   elements in the left most undistributed dimensions. 
!  
! \begin{sloppypar}
! This method is overloaded for
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8}, 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \end{sloppypar}
!
! This call is collective across the current VM.  
! 
! For examples and associated documentations using this method see Section  
! \ref{sec:fieldbundle:usage:smm_1dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item [srcFieldBundle]  
!       {\tt ESMF\_FieldBundle} with source data. 
! \item [dstFieldBundle] 
!       {\tt ESMF\_FieldBundle} with destination data. The data in this 
!       FieldBundle may be destroyed by this call. 
! \item [routehandle] 
!       Handle to the precomputed Route. 
! \item [{[rc]}]  
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldBundleSMMStoreNF" 
!BOPI
! !IROUTINE: ESMF_FieldBundleSMMStore - Precompute a FieldBundle sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleSMMStore()
    subroutine ESMF_FieldBundleSMMStoreNF(srcFieldBundle, dstFieldBundle, & 
        routehandle, keywordEnforcer, rc) 

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,                intent(out), optional :: rc 

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
        type(ESMF_Field)                              :: l_srcField, l_dstField   
        type(ESMF_ArrayBundle)                        :: srcab, dstab
        type(ESMF_Array), allocatable                 :: srca(:), dsta(:)

        ! Initialize return code; assume routine not implemented 
        localrc = ESMF_RC_NOT_IMPL 
        if(present(rc)) rc = ESMF_RC_NOT_IMPL 

        ! check variables
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

        ! loop over source and destination fields. 
        ! verify src and dst FieldBundles can communicate
        ! field_count match
        call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=sfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=dfcount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(sfcount /= dfcount) then
            call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        ! TODO:
        ! internal grids match
        !if(ESMF_GridMatch(srcFieldBundle%btypep%grid, dstFieldBundle%btypep%grid) then
        !    call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
        !       "src and dst FieldBundle must have matching grid", &
        !        ESMF_CONTEXT, rcToReturn=rc)
        !    return
        !endif 

        allocate(srca(sfcount))
        do i = 1, sfcount
            call ESMF_FieldBundleGet(srcFieldBundle, i, l_srcField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_srcField, array=srca(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        srcab = ESMF_ArrayBundleCreate(arrayList=srca, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(srca)

        allocate(dsta(dfcount))
        do i = 1, dfcount
            call ESMF_FieldBundleGet(dstFieldBundle, i, l_dstField, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(l_dstField, array=dsta(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        dstab = ESMF_ArrayBundleCreate(arrayList=dsta, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(dsta)

        call ESMF_ArrayBundleSMMStore(srcab, dstab, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleSMMStoreNF
! ---------------------------------------------------------------------------- 

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleValidate()"
!BOPI
! !IROUTINE: ESMF_FieldBundleValidate - Validate fieldbundle internals

! !INTERFACE:
  subroutine ESMF_FieldBundleValidate(fieldbundle, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)              :: fieldbundle
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt fieldbundle} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[fieldbundle] 
!          Specified {\tt ESMF\_FieldBundle} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Initialize return code
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)
    
    ! Call into the C++ interface layer
    !todo: call c_ESMC_FieldBundleValidate(fieldbundle, localrc)
    localrc = ESMF_SUCCESS  ! remove when todo is done.
    
    ! Use LogErr to handle return code
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_FieldBundleValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleWrite()"
!BOP
! !IROUTINE: ESMF_FieldBundleWrite - Write the Fields into a file
! \label{api:FieldBundleWrite}

! !INTERFACE:
  subroutine ESMF_FieldBundleWrite(fieldbundle, file, &
    singleFile, timeslice, iofmt, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)              :: fieldbundle
    character(*),           intent(in)              :: file
    logical,                intent(in),   optional  :: singleFile
    integer,                intent(in),   optional  :: timeslice
    type(ESMF_IOFmtFlag),   intent(in),   optional  :: iofmt
    integer,                intent(out),  optional  :: rc  
!         
!
!
! !DESCRIPTION:
!   Write the Fields into a file. For this API to be functional,
!   the environment variable {\tt ESMF\_PIO} should be set to "internal"
!   when the ESMF library is built. Please see the section on 
!   Data I/O,~\ref{io:dataio}.
!
!   Limitations:
!   \begin{itemize}
!     \item Only 1 DE per PET supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[fieldbundle] 
!     An {\tt ESMF\_FieldBundle} object.
!   \item[file]
!     The name of the output file to which field bundle data is written.
!   \item[{[singleFile]}]
!     A logical flag, the default is .true., i.e., all fields in the bundle 
!     are written in one single file. If .false., each field will be written
!     in separate files; these files are numbered with the name based on the
!     argument "file". That is, a set of files are named: [file\_name]001,
!     [file\_name]002, [file\_name]003,...
!   \item[{[timeslice]}]
!     Some IO formats (e.g. NetCDF) support the output of data in form of
!     time slices. The {\tt timeslice} argument provides access to this
!     capability. Usage of this feature requires that the first slice is
!     written with a positive {\tt timeslice} value, and that subsequent slices
!     are written with a {\tt timeslice} argument that increments by one each
!     time. By default, i.e. by omitting the {\tt timeslice} argument, no
!     provisions for time slicing are made in the output file.
!   \item[{[iofmt]}]
!     \begin{sloppypar}
!     The IO format. Please see Section~\ref{opt:iofmtflag} for the list
!     of options.  If not present, defaults to {\tt ESMF\_IOFMT\_NETCDF}.
!     \end{sloppypar}
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    character(len=80), allocatable :: Aname(:)
    integer :: fieldCount,i,time
    type(ESMF_Field), allocatable :: fieldList(:)
    logical :: singlef
    character(len=80) :: filename
    character(len=3) :: cnum
    type(ESMF_IOFmtFlag)        :: iofmtd

#ifdef ESMF_PIO
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    ! Check options
    singlef = .true.
    if (present(singleFile)) singlef = singleFile
    iofmtd = ESMF_IOFMT_NETCDF   ! default format
    if(present(iofmt)) iofmtd = iofmt
    time = -1   ! default, no time dimension
    if (present(timeslice)) time = timeslice
    
    call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    allocate (Aname(fieldCount))
    allocate (fieldList(fieldCount))
    call ESMF_FieldBundleGet(fieldbundle, fieldList=fieldList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    if (singlef) then
      ! Get and write the first field in the Bundle
      call ESMF_FieldGet(fieldList(1), name=Aname(1), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldWrite(fieldList(1), file=file, timeslice=time, iofmt=iofmtd, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      ! Get and write the rest of the fields in the Bundle
      do i=2,fieldCount
       call ESMF_FieldGet(fieldList(i), name=Aname(i), rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
       call ESMF_FieldWrite(fieldList(i), file=file, timeslice=time, &
         append=.true., iofmt=iofmtd, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    else
      do i=1,fieldCount
        write(cnum,"(i3.3)") i
        filename = file // cnum
        ! Get and write the first field in the Bundle
        call ESMF_FieldGet(fieldList(i), name=Aname(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldWrite(fieldList(i), file=trim(filename),  &
           timeslice=time, iofmt=iofmtd, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

#else
    ! Return indicating PIO not present
    if (present(rc)) rc = ESMF_RC_LIB_NOT_PRESENT
#endif
 
  end subroutine ESMF_FieldBundleWrite
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSerialize"

!BOPI
! !IROUTINE: ESMF_FieldBundleSerialize - Serialize fieldbundle info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_FieldBundleSerialize(fieldbundle, buffer, length, offset, &
                                          attreconflag, inquireflag, rc) 
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle 
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), intent(in), optional :: attreconflag
      type(ESMF_InquireFlag), intent(in), optional :: inquireflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_FieldBundle} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_FieldBundleWrite()} and {\tt ESMF\_FieldBundleRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           {\tt ESMF\_FieldBundle} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [length]
!           Current length of buffer, in bytes.  If the serialization
!           process needs more space it will allocate it and update
!           this length.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item[{[inquireflag]}]
!           Flag to tell if serialization is to be done (ESMF_NOINQUIRE)
!           or if this is simply a size inquiry (ESMF_INQUIREONLY)
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                     ! Error status
      integer :: i, fieldCount
      type(ESMF_FieldBundleType), pointer :: bp   ! fieldbundle type
      type(ESMF_AttReconcileFlag) :: lattreconflag
      type(ESMF_InquireFlag) :: linquireflag
      type(ESMF_Field), pointer :: l_fieldList(:)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

      nullify(l_fieldList)

      ! deal with optional attreconflag and inquireflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif

      if (present (inquireflag)) then
        linquireflag = inquireflag
      else
        linquireflag = ESMF_NOINQUIRE
      end if

      ! shortcut to internals
      bp => fieldbundle%this

      call ESMF_ContainerGet(bp%container, itemCount=fieldCount, &
        itemList=l_fieldList, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      call c_ESMC_BaseSerialize(bp%base, buffer, length, offset, &
                                 lattreconflag, linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      call c_ESMC_FieldBundleSerialize(bp%status, fieldCount, &
                                 buffer, length, offset, linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if(bp%status == ESMF_FBSTATUS_GRIDSET) then
        call ESMF_GeomBaseSerialize(bp%geombase, buffer, length, offset, &
                                 lattreconflag, linquireflag, localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! TODO: decide if these need to be sent before or after
      do i = 1, fieldCount
          call ESMF_FieldSerialize(l_fieldList(i), buffer, length, offset, &
                                  attreconflag=lattreconflag, &
                                  inquireflag=linquireflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
      enddo

      if(associated(l_fieldList)) deallocate(l_fieldList)

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleDeserialize"

!BOPI
! !IROUTINE: ESMF_FieldBundleDeserialize - Deserialize a byte stream into a FieldBundle
!
! !INTERFACE:
      function ESMF_FieldBundleDeserialize(buffer, offset, &
                                          attreconflag, rc) 
!
! !RETURN VALUE:
      type(ESMF_FieldBundle) :: ESMF_FieldBundleDeserialize   
!
! !ARGUMENTS:
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), optional :: attreconflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a FieldBundle object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_FieldBundleWrite()} and {\tt ESMF\_FieldBundleRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc, status             ! Error status, allocation status
      integer :: i, fieldCount
      type(ESMF_FieldBundleType), pointer :: bp   ! fieldbundle type
      type(ESMF_AttReconcileFlag) :: lattreconflag
      type(ESMF_Grid) :: grid
      type(ESMF_GeomType) :: geomtype
      type(ESMF_Logical) :: linkChange
      type(ESMF_Field), pointer :: flist(:)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      nullify(flist)

      ! deal with optional attreconflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif
      ! linkChange flag true for all but Components
      linkChange = ESMF_TRUE;

      ! in case of error, make sure this is invalid.
      nullify(bp)
      nullify(ESMF_FieldBundleDeserialize%this)

      ! shortcut to internals
      allocate(bp, stat=status)
      if (ESMF_LogFoundAllocError(status, &
        msg="space for new FieldBundle object", &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Deserialize Base
      call c_ESMC_BaseDeserialize(bp%base, buffer(1), offset, lattreconflag, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_BaseSetInitCreated(bp%base, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Deserialize other FieldBundle members
      
      call c_ESMC_FieldBundleDeserialize(bp%status, fieldCount, &
                                 buffer(1), offset, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if(bp%status == ESMF_FBSTATUS_GRIDSET) then
        bp%geombase = ESMF_GeomBaseDeserialize(buffer, offset, &
          attreconflag=attreconflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! TODO: decide if these need to be sent before or after
      allocate(flist(fieldCount), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, &
        msg="Field list", &
        ESMF_CONTEXT, rcToReturn=rc)) return

      do i = 1, fieldCount
          flist(i) = ESMF_FieldDeserialize(buffer, offset, &
                                      attreconflag=lattreconflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) then
            deallocate(flist)
            return
          endif
          !  here we relink the Field Attribute hierarchies to the FieldBundle
          !  Attribute hierarchies, as they were before
          if (lattreconflag%value == ESMF_ATTRECONCILE_ON%value) then
            call c_ESMC_AttributeLink(bp%base, flist(i)%ftypep%base, &
              linkChange, localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) then
              deallocate(flist)
              return
            endif
          endif
      enddo

      bp%container = ESMF_ContainerCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_ContainerAdd(bp%container, flist, multiflag=.true., relaxedflag=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      deallocate(flist)

      bp%is_proxy = .true.

      ESMF_FieldBundleDeserialize%this => bp

      ! Add reference to this object into ESMF garbage collection table
      ! Only call this in those Create() methods that call Construct()
      call c_ESMC_VMAddFObject(ESMF_FieldBundleDeserialize, &
        ESMF_ID_FIELDBUNDLE%objectID)

      ! Set as created
      ESMF_INIT_SET_CREATED(ESMF_FieldBundleDeserialize)

      if  (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_FieldBundleDeserialize

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetInit()"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_FieldBundleGetInit(fieldbundle) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_FieldBundleGetInit   
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(in), optional :: fieldbundle
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           fieldbundle object.
!     \end{description}
!
!EOPI

    if (present(fieldbundle)) then
      ESMF_FieldBundleGetInit = ESMF_INIT_GET(fieldbundle)
    else
      ESMF_FieldBundleGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_FieldBundleGetInit
!------------------------------------------------------------------------------

function ESMF_FieldBundleStatusEQ(sf1, sf2)
 logical ESMF_FieldBundleStatusEQ
 type(ESMF_FieldBundleStatus), intent(in) :: sf1, sf2

 ESMF_FieldBundleStatusEQ = (sf1%status == sf2%status)
end function

function ESMF_FieldBundleStatusNE(sf1, sf2)
 logical ESMF_FieldBundleStatusNE
 type(ESMF_FieldBundleStatus), intent(in) :: sf1, sf2

 ESMF_FieldBundleStatusNE = (sf1%status /= sf2%status)
end function

end module ESMF_FieldBundleMod
