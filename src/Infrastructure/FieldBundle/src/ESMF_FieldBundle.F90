! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
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

!   F90 implementation of fieldbundle
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
  use ESMF_ArraySpecMod
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
  use ESMF_IOMod
  use ESMF_FactorReadMod    ! Read weight factors from netCDF file.
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
!     ! ESMF_FieldBundleStatus
!
!------------------------------------------------------------------------------

  type ESMF_FieldBundleStatus
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
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
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
    type(ESMF_Base)              :: base      ! base class object
    type(ESMF_GeomBase)          :: geombase  ! base class object
    type(ESMF_Container)         :: container ! internal storage implementation
    type(ESMF_FieldBundleStatus) :: status    ! status of this FieldBundle
    logical                      :: is_proxy  ! true if this is a proxy FB
    ESMF_INIT_DECLARE
  end type

  ! F90 class type to hold pointer to FieldBundleType
  type ESMF_FieldBundle
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
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
  public ESMF_FieldBundleIsCreated
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
    '$Id$'

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
    module procedure ESMF_FieldBundleGetListAll
    module procedure ESMF_FieldBundleGetItem
    module procedure ESMF_FieldBundleGetList
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
    module procedure ESMF_FieldBundleSMMStoreFromFile
!
!EOPI

  end interface

!===============================================================================
! FieldBundleOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_FieldBundleAssignment(=) - FieldBundle assignment
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
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
! !IROUTINE: ESMF_FieldBundleOperator(==) - FieldBundle equality operator
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
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
! !IROUTINE: ESMF_FieldBundleOperator(/=) - FieldBundle not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (fieldbundle1 /= fieldbundle2) then ... endif
!             OR
!   result = (fieldbundle1 /= fieldbundle2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_FieldBundle), intent(in) :: fieldbundle1
!   type(ESMF_FieldBundle), intent(in) :: fieldbundle2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
! !IROUTINE: ESMF_FieldBundleAdd - Add Fields to a FieldBundle
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
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Add Field(s) to a FieldBundle. It is an error if {\tt fieldList} contains
!   Fields that match by name Fields already contained in 
!   {\tt fieldbundle} when multiflag
!   is set to {\tt .false.} and relaxedflag is set to {\tt .false.}.
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} to be added to.
!   \item [fieldList]
!     List of {\tt ESMF\_Field} objects to be added.
!   \item [{[multiflag]}]
!     A setting of {\tt .true.} allows multiple items with the same name
!     to be added to {\tt ESMF\_FieldBundle}. For {\tt .false.} added items must
!     have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "add"
!     under {\tt multiflag=.false.} mode, where it is {\em not} an error if 
!     {\tt fieldList} contains items with names that are also found in 
!     {\tt ESMF\_FieldBundle}. The {\tt ESMF\_FieldBundle} is left unchanged for these items.
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
    garbageSize = size(garbageList)
    if(garbageSize /= 0) then

      ! There are garbage after ContainerAdd, figure out which fields are added
      ! error checking
      if(garbageSize .gt. fieldCount) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
          msg = " - there are more garbage in garbageList than fields in FieldList", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif

      ! at least 1 Field was added
      ! No need to handle the case when garbageSize == fieldCount when no Field is added
      if(garbageSize .lt. fieldCount) then

        allocate(addedList(fieldCount - garbageSize), stat=localrc)
        if(localrc /= 0) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
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

        ! Attribute link
        linkChange = ESMF_TRUE

        if(size(addedList) .ge. 1) then
          call ESMF_FieldBundleSetGeom(fieldbundle, addedList(1), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                        ESMF_CONTEXT, rcToReturn=rc))  return
        endif

        do i=1, size(addedList)
          call c_ESMC_AttributeLink(fieldbundle%this%base, addedList(i)%ftypep%base, linkChange, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                        ESMF_CONTEXT, rcToReturn=rc))  return
        enddo
        deallocate(addedList)

      endif  ! partial add

    else
      ! No garbage, all fieldList should be linked
      !
      ! Obviously the attribute linking of the geombase object is a bit tricky,
      ! right now, we don't restrict the geombase to be matched among the fields in the
      ! fieldBundle. But for attribute hierarchy, we actually assume that all fields in
      ! the fieldBundle should be built on the same geombase object.
      !
      ! 3/7/13
      ! New behavior for geombase:
      ! 1. FieldBundleSet() can always change the geombase object in a FieldBundle
      ! 2. Add/AddReplace/Replace can  change the geombase object in a FieldBundle if it's not set
      ! 3. Add/AddReplace/Replace can NOT change  geombase object in a FieldBundle after its set
      !
      ! Attribute link
      if(size(fieldList) .ge. 1 .and. (fieldbundle%this%status /= ESMF_FBSTATUS_GRIDSET) ) then
        ! setgeom links grid geombase automatically
        call ESMF_FieldBundleSetGeom(fieldbundle, fieldList(1), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc))  return
      endif ! non-empty fieldlist

      linkChange = ESMF_TRUE
      do i=1, size(fieldList)
        call c_ESMC_AttributeLink(fieldbundle%this%base, fieldList(i)%ftypep%base, linkChange, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc))  return
      enddo
      
    endif ! garbageSize /= 0

    ! always deallocate garbageList because it's always associated returning from Container
    deallocate(garbageList)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleAddList
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAddItem()"
!BOPI
! !IROUTINE: ESMF_FieldBundleAdd - Add one Field to a FieldBundle
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
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Add a single field to a FieldBundle. It is an error if {\tt field} 
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
!     to be added to {\tt ESMF\_FieldBundle}. For {\tt .false.} added items must
!     have unique names. The default setting is {\tt .false.}.
!   \item [{[relaxedflag]}]
!     A setting of {\tt .true.} indicates a relaxed definition of "add"
!     under {\tt multiflag=.false.} mode, where it is {\em not} an error if 
!     {\tt fieldList} contains items with names that are also found in 
!     {\tt ESMF\_FieldBundle}. The {\tt ESMF\_FieldBundle} is left unchanged for these items.
!     For {\tt .false.} this is treated as an error condition. 
!     The default setting is {\tt .false.}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

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
! !IROUTINE: ESMF_FieldBundleAddReplace - Conditionally add or replace Fields in a FieldBundle
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
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Fields in {\tt fieldList} that do not match any Fields by name in 
!   {\tt fieldbundle} are added to the FieldBundle. Fields in {\tt fieldList}
!   that match any Fields by name in {\tt fieldbundle} replace those Fields.
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
    integer                       :: fieldCount, i, garbageSize
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
    if(size(fieldList) .ge. 1 .and. (fieldbundle%this%status /= ESMF_FBSTATUS_GRIDSET) ) then
      ! setgeom links grid geombase automatically
      call ESMF_FieldBundleSetGeom(fieldbundle, fieldList(1), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc))  return
    endif ! non-empty fieldlist
    linkChange = ESMF_TRUE
    ! Add all fields in fieldList
    do i = 1, fieldCount
      call c_ESMC_AttributeLink(fieldbundle%this%base, fieldList(i)%ftypep%base, linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
    enddo
    
    ! Remove those that were replaced
    garbageSize = size(garbageList)
    do i=1, garbageSize
      call c_ESMC_AttributeLinkRemove(fieldbundle%this%base, garbageList(i)%ftypep%base, linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
    enddo
    deallocate(garbageList)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleAddReplace
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleCreate()"
!BOP
! !IROUTINE: ESMF_FieldBundleCreate - Create a FieldBundle from a list of Fields
!
! !INTERFACE:
  function ESMF_FieldBundleCreate(keywordEnforcer, fieldList, &
      multiflag, relaxedflag, name, rc)
!         
! !RETURN VALUE:
    type(ESMF_FieldBundle) :: ESMF_FieldBundleCreate
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Field), intent(in),  optional :: fieldList(:)
    logical,          intent(in),  optional :: multiflag
    logical,          intent(in),  optional :: relaxedflag
    character (len=*),intent(in),  optional :: name
    integer,          intent(out), optional :: rc
!         
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Create an {\tt ESMF\_FieldBundle} object from a list of existing Fields.
!
!   The creation of a FieldBundle leaves the bundled Fields unchanged, they
!   remain valid individual objects. a FieldBundle is a light weight container
!   of Field references. The actual data remains in place, there are no
!   data movements or duplications associated with the creation of an 
!   FieldBundle.
!
!   \begin{description}
!   \item [{[fieldList]}]
!     List of {\tt ESMF\_Field} objects to be bundled.
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
    type(ESMF_GeomType_Flag)     :: geomtype
    type(ESMF_Grid)         :: grid
    type(ESMF_XGrid)        :: xgrid
    type(ESMF_Mesh)         :: mesh
    type(ESMF_LocStream)    :: locstream
    type(ESMF_FieldStatus_Flag)  :: fstatus

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
      call ESMF_FieldBundleAdd(ESMF_FieldBundleCreate, fieldList, &
        multiflag=multiflag, relaxedflag=relaxedflag, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end function ESMF_FieldBundleCreate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleDestroy()"
!BOP
! !IROUTINE: ESMF_FieldBundleDestroy - Release resources associated with a FieldBundle

! !INTERFACE:
  subroutine ESMF_FieldBundleDestroy(fieldbundle, keywordEnforcer, noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)          :: fieldbundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),   optional :: noGarbage
    integer,                intent(out),  optional :: rc  
!         
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
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
! \item[{[noGarbage]}]
!      If set to {\tt .TRUE.} the object will be fully destroyed and removed
!      from the ESMF garbage collection system. Note however that under this 
!      condition ESMF cannot protect against accessing the destroyed object 
!      through dangling aliases -- a situation which may lead to hard to debug 
!      application crashes.
! 
!      It is generally recommended to leave the {\tt noGarbage} argument
!      set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!      garbage collection system which will prevent problems with dangling
!      aliases or incorrect sequences of destroy calls. However this level of
!      support requires that a small remnant of the object is kept in memory
!      past the destroy call. This can lead to an unexpected increase in memory
!      consumption over the course of execution in applications that use 
!      temporary ESMF objects. For situations where the repeated creation and 
!      destruction of temporary objects leads to memory issues, it is 
!      recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!      removing the entire temporary object from memory.
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

    ! more input checking
    if (.not.associated(fieldbundle%this)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
        msg="Uninitialized or already destroyed FieldBundle: this pointer unassociated", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Destruct all fieldbundle internals and then free fieldbundle memory.
    call ESMF_FieldBundleDestruct(fieldbundle%this, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(noGarbage)) then
      if (noGarbage) then
        ! destroy Base object (which also removes it from garbage collection)
        call ESMF_BaseDestroy(fieldbundle%this%base, noGarbage, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! remove reference to this object from ESMF garbage collection table
        call c_ESMC_VMRmFObject(fieldbundle)
        ! deallocate the actual fieldbundle data structure
        deallocate(fieldbundle%this, stat=localrc)
        if (ESMF_LogFoundDeallocError(localrc, &
          msg="Deallocating FieldBundle information", &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif

    ! Mark this fieldbundle as invalid
    nullify(fieldbundle%this)

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

    integer                                   :: localrc, fcount, i
    type(ESMF_Status) :: basestatus
    type(ESMF_Field), pointer                 :: flist(:)

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call ESMF_BaseGetStatus(this%base, basestatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (basestatus .eq. ESMF_STATUS_READY) then
      
      ! Destroy internal geombase and mark this fieldBundle UNINIT
      if(this%status == ESMF_FBSTATUS_GRIDSET) then
        call ESMF_GeomBaseDestroy(this%geombase, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        this%status = ESMF_FBSTATUS_UNINIT
      endif

      ! Destroy all the internal Fields if this is a proxy fieldBundle
      if(this%is_proxy) then
        nullify(flist)
        call ESMF_ContainerGet(this%container, itemCount=fcount, &
          itemList=flist, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        do i = 1, fcount
          call ESMF_FieldDestroy(flist(i), rc=localrc)
          if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        deallocate(flist)
      endif

      ! Destroy internal container
      call ESMF_ContainerDestroy(this%container, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

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
#define ESMF_METHOD "ESMF_FieldBundleGetListAll()"
!BOP
! !IROUTINE: ESMF_FieldBundleGet - Get object-wide information from a FieldBundle
!
! !INTERFACE:
    ! Private name; call using ESMF_FieldBundleGet()   
    subroutine ESMF_FieldBundleGetListAll(fieldbundle, keywordEnforcer, &
      itemorderflag, geomtype, grid, locstream, mesh, xgrid, &
      fieldCount, fieldList, fieldNameList, name, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle),    intent(in)            :: fieldbundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_ItemOrder_Flag), intent(in),  optional :: itemorderflag
    type(ESMF_GeomType_Flag),  intent(out), optional :: geomtype
    type(ESMF_Grid),           intent(out), optional :: grid
    type(ESMF_LocStream),      intent(out), optional :: locstream
    type(ESMF_Mesh),           intent(out), optional :: mesh
    type(ESMF_XGrid),          intent(out), optional :: xgrid
    integer,                   intent(out), optional :: fieldCount
    type(ESMF_Field),          intent(out), optional :: fieldList(:)
    character(len=*),          intent(out), optional :: fieldNameList(:)
    character(len=*),          intent(out), optional :: name
    integer,                   intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added argument {\tt itemorderflag}.
!              The new argument gives the user control over the order in which
!              the items are returned.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Get the list of all Fields and field names bundled in a FieldBundle.
!
!   \begin{description}
!   \item [fieldbundle]
!         {\tt ESMF\_FieldBundle} to be queried.
!   \item [{[itemorderflag]}]
!         Specifies the order of the returned items in the {\tt fieldList} or the
!         {\tt fieldNameList}.
!         The default is {\tt ESMF\_ITEMORDER\_ABC}.
!         See \ref{const:itemorderflag} for a full list of options.
!   \item[{[geomtype]}]
!      Flag that indicates what type of geometry this FieldBundle object holds. 
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
!         Upon return holds a list of Fields bundled in {\tt ESMF\_FieldBundle}. The
!         argument must be allocated to be at least of size {\tt fieldCount}.
!   \item [{[fieldNameList]}]
!         Upon return holds a list of the names of the fields bundled in 
!         {\tt ESMF\_FieldBundle}. The argument must be allocated to be at least of
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
    type(ESMF_GeomType_Flag)           :: l_geomtype
    type(ESMF_ItemOrder_Flag)          :: l_itemorderflag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    l_itemorderflag = ESMF_ITEMORDER_ABC
    if (present(itemorderflag)) l_itemorderflag = itemorderflag

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    nullify(this)
    nullify(l_fieldList)

    this => fieldbundle%this

    ! geomBase
    if(present(geomtype)) then
      if(this%status /= ESMF_FBSTATUS_GRIDSET) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
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
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
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
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
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
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
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
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
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
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
        msg = " - cannot allocate l_fieldList internally", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    call ESMF_ContainerGet(fieldbundle%this%container, &
      itemList=l_fieldList, itemorderflag=l_itemorderflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if(present(fieldList)) then
      if(size(fieldList) .lt. l_fieldCount) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
          msg = " - Input fieldList size is too small", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      fieldList(1:l_fieldCount) = l_fieldList(1:l_fieldCount)
    endif

    ! Special call to get name out of Base class
    if (present(fieldNameList)) then
      if(size(fieldNameList) .lt. l_fieldCount) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
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
        call ESMF_GetName(fieldbundle%this%base, name, localrc)
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
#define ESMF_METHOD "ESMF_FieldBundleGetItem()"
!BOP
! !IROUTINE: ESMF_FieldBundleGet - Get information about a Field by name and optionally return a Field
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
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Get information about items that match {\tt fieldName} in FieldBundle.
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} to be queried.
!   \item [fieldName]
!     Specified name.
!   \item [{[field]}]
!     Upon return holds the requested field item. It is an error if this
!     argument was specified and there is not exactly one field item in 
!     {\tt ESMF\_FieldBundle} that matches {\tt fieldName}.
!   \item [{[fieldCount]}]
!     Number of Fields with {\tt fieldName} in {\tt ESMF\_FieldBundle}.
!   \item [{[isPresent]}]
!     Upon return indicates whether field(s) with {\tt fieldName} exist
!     in {\tt ESMF\_FieldBundle}.
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
      call ESMF_ContainerGet(fieldbundle%this%container, &
        itemName=trim(fieldName), itemCount=fieldCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(isPresent)) then
      call ESMF_ContainerGet(fieldbundle%this%container, &
        itemName=trim(fieldName), isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if(present(fieldCount)) then
      if(fieldCount .gt. 1) then
        if(present(field)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
            msg = "field argument cannot be specified when fieldCount is "// &
              "greater than 1", ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
      endif
    endif

    if (present(field)) then
      call ESMF_ContainerGet(fieldbundle%this%container, &
        itemName=trim(fieldName), item=field, rc=localrc)
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
! !IROUTINE: ESMF_FieldBundleGet - Get a list of Fields by name
!
! !INTERFACE:
    ! Private name; call using ESMF_FieldBundleGet()   
    subroutine ESMF_FieldBundleGetList(fieldbundle, fieldName, fieldList, &
      keywordEnforcer, itemorderflag, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle),    intent(in)            :: fieldbundle
    character(len=*),          intent(in)            :: fieldName
    type(ESMF_Field),          intent(out)           :: fieldList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_ItemOrder_Flag), intent(in),  optional :: itemorderflag
    integer,                   intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[6.1.0] Added argument {\tt itemorderflag}.
!              The new argument gives the user control over the order in which
!              the items are returned.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Get the list of Fields from fieldbundle that match fieldName.
!
!   \begin{description}
!   \item [fieldbundle]
!     {\tt ESMF\_FieldBundle} to be queried.
!   \item [fieldName]
!     Specified name.
!   \item [fieldList]
!     List of Fields in {\tt ESMF\_FieldBundle} that match {\tt fieldName}. The
!     argument must be allocated to be at least of size {\tt fieldCount}
!     returned for this {\tt fieldName}.
!   \item [{[itemorderflag]}]
!     Specifies the order of the returned items in the {\tt fieldList}.
!     The default is {\tt ESMF\_ITEMORDER\_ABC}.
!     See \ref{const:itemorderflag} for a full list of options.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc      ! local return code
    integer                       :: fieldCount
    type(ESMF_Field), pointer     :: l_fieldList(:)
    type(ESMF_ItemOrder_Flag)     :: l_itemorderflag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    l_itemorderflag = ESMF_ITEMORDER_ABC
    if(present(itemorderflag)) l_itemorderflag = itemorderflag

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)
    
    nullify(l_fieldList)
    ! Check size
    call ESMF_ContainerGet(fieldbundle%this%container, trim(fieldName), &
      itemCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if(size(fieldList) .lt. fieldCount) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
      msg=" - Input argument fieldList size is too small", &
      ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    allocate(l_fieldList(fieldCount), stat=localrc)
    if(localrc /= ESMF_SUCCESS) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
        msg = " - cannot allocate l_fieldList internally", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    call ESMF_ContainerGet(fieldbundle%this%container, trim(fieldName), &
      l_fieldList, itemorderflag=l_itemorderflag, rc=localrc)
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
! TODO: This method is DEPRECATED starting at 5.2.0r (was not part of the 5.2.0r
!       reference manual.
! TODO: We should put a deprecation message into the Log file when this message
!       is being called!
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
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
        msg = "fieldIndex must be between 1 and fieldCount in the FieldBundle",&
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    allocate(l_fieldList(l_fieldCount), stat=localrc)
    if(localrc /= ESMF_SUCCESS) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
        msg = " - cannot allocate l_fieldList internally", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    call ESMF_ContainerGet(fieldbundle%this%container, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, itemList=l_fieldList, rc=localrc)
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
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),   optional  :: checkflag
        integer,                intent(out),  optional  :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed halo operation for the Fields in {\tt fieldbundle}.
!   The FieldBundle must match the respective FieldBundle used during 
!   {\tt ESMF\_FieldBundleHaloStore()} in {\em type}, {\em kind}, and
!   memory layout of the {\em gridded} dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
!   \end{sloppypar}
!
!   See {\tt ESMF\_FieldBundleHaloStore()} on how to precompute 
!   {\tt routehandle}.
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
        call ESMF_ArrayBundleDestroy(arrayBundle, noGarbage=.true., rc=localrc)
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
  subroutine ESMF_FieldBundleHaloRelease(routehandle, keywordEnforcer, &
    noGarbage, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),   optional  :: noGarbage
        integer,                intent(out),  optional  :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Release resources associated with a FieldBundle halo operation. After this call
!   {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
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
        call ESMF_RouteHandleRelease(routehandle, noGarbage=noGarbage, &
          rc=localrc)
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
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Store a FieldBundle halo operation over the data in {\tt fieldbundle}. 
!   By definition, all elements in the total Field regions that lie
!   outside the exclusive regions will be considered potential destination
!   elements for the halo operation. However, only those elements that have a corresponding
!   halo source element, i.e. an exclusive element on one of the DEs, will be
!   updated under the halo operation. Elements that have no associated source
!   remain unchanged under halo.
!
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldBundleHalo()} on any pair of FieldBundles that matches 
!   {\tt srcFieldBundle} and {\tt dstFieldBundle} in {\em type}, {\em kind},
!   and memory layout of the {\em gridded} dimensions. However, the size, 
!   number, and index order of {\em ungridded} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
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
        call ESMF_ArrayBundleDestroy(arrayBundle, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(l_fieldList)

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleHaloStore
!------------------------------------------------------------------------------ 


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleIsCreated()"
!BOP
! !IROUTINE: ESMF_FieldBundleIsCreated - Check whether a FieldBundle object has been created

! !INTERFACE:
  function ESMF_FieldBundleIsCreated(fieldbundle, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_FieldBundleIsCreated
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)            :: fieldbundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt fieldbundle} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[fieldbundle]
!     {\tt ESMF\_FieldBundle} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_FieldBundleIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_FieldBundleGetInit(fieldbundle)==ESMF_INIT_CREATED) &
      ESMF_FieldBundleIsCreated = .true.
  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundlePrint()"
!BOP
! !IROUTINE: ESMF_FieldBundlePrint - Print FieldBundle information

! !INTERFACE:
  subroutine ESMF_FieldBundlePrint(fieldbundle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)              :: fieldbundle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out),  optional  :: rc  
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Print internal information of the specified {\tt fieldbundle} object. \\
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
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
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
! !IROUTINE: ESMF_FieldBundleRead - Read Fields to a FieldBundle from file(s)
! \label{api:FieldBundleRead}

! !INTERFACE:
  subroutine ESMF_FieldBundleRead(fieldbundle, fileName, &
    keywordEnforcer, singleFile, timeslice, iofmt, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)          :: fieldbundle
    character(*),           intent(in)             :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords for the below
    logical,                intent(in),  optional  :: singleFile
    integer,                intent(in),  optional  :: timeslice
    type(ESMF_IOFmt_Flag),  intent(in),  optional  :: iofmt
    integer,                intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Read field data to a FieldBundle object from file(s).
!   For this API to be functional, the environment variable {\tt ESMF\_PIO} 
!   should be set to "internal" when the ESMF library is built.
!   Please see the section on Data I/O,~\ref{io:dataio}.
!
!   Limitations:
!   \begin{itemize}
!     \item Only single tile Arrays within Fields are supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[fieldbundle] 
!     An {\tt ESMF\_FieldBundle} object.
!   \item[fileName]
!     The name of the file from which fieldbundle data is read.
!   \item[{[singleFile]}]
!     A logical flag, the default is .true., i.e., all Fields in the bundle 
!     are stored in one single file. If .false., each field is stored 
!     in separate files; these files are numbered with the name based on the
!     argument "file". That is, a set of files are named: [file\_name]001,
!     [file\_name]002, [file\_name]003,...
!   \item[{[timeslice]}]
!    The time-slice number of the variable read from file.
!   \item[{[iofmt]}]
!     \begin{sloppypar}
!    The I/O format.  Please see Section~\ref{opt:iofmtflag} for the list
!    of options. If not present, file names with a {\tt .bin} extension will
!    use {\tt ESMF\_IOFMT\_BIN}, and file names with a {\tt .nc} extension
!    will use {\tt ESMF\_IOFMT\_NETCDF}.  Other files default to
!    {\tt ESMF\_IOFMT\_NETCDF}.
!     \end{sloppypar}
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                        :: localrc      ! local return code
    character(len=ESMF_MAXSTR)     :: name
    integer                        :: fieldCount
    integer                        :: i
    type(ESMF_Field), allocatable  :: fieldList(:)
    logical                        :: singlef
    character(len=3)               :: cnum
    character(len=len (fileName) + 3) :: filename_num ! len (file)+len (cnum)
    type(ESMF_Array)               :: array 
    type(ESMF_IOFmt_Flag)          :: opt_iofmt
    type(ESMF_IO)                  :: io           ! The I/O object
    logical                        :: errorFound   ! True if error condition
    integer                        :: file_ext_p
    integer                        :: time

#ifdef ESMF_PIO
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    ! Check options
    singlef = .true.
    if (present(singleFile)) singlef = singleFile

    ! Set iofmt based on file name extension (if present)
    if (present (iofmt)) then
      opt_iofmt = iofmt
    else
      if (index (fileName, '.') > 0) then
        file_ext_p = index (fileName, '.', back=.true.)
        select case (fileName(file_ext_p:))
        case ('.nc')
          opt_iofmt = ESMF_IOFMT_NETCDF
        case ('.bin')
          opt_iofmt = ESMF_IOFMT_BIN
        case default
          opt_iofmt = ESMF_IOFMT_NETCDF
        end select
      else
        opt_iofmt = ESMF_IOFMT_NETCDF
      end if
    end if

    time = 0
    if(present(timeslice)) time = timeslice

    call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
         ESMF_CONTEXT, rcToReturn=rc)) return

    allocate (fieldList(fieldCount))
    call ESMF_FieldBundleGet(fieldbundle, fieldList=fieldList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Create an I/O object
    io = ESMF_IOCreate(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! From here on out, we need to clean up so no returning on error
    if (singlef) then
      ! Get and read the fields in the Bundle
      do i=1,fieldCount
        call ESMF_FieldGet(fieldList(i), array=array, name=name, rc=localrc)
        errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,     &
            ESMF_CONTEXT, rcToReturn=rc)
        if (errorFound) exit
        call ESMF_IOAddArray(io, array, variableName=name, rc=localrc)
        errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,     &
            ESMF_CONTEXT, rcToReturn=rc)
        if (errorFound) exit
      enddo
      if (.not. errorFound) then
        call ESMF_IORead(io, trim(fileName), timeslice=time,                &
            iofmt=opt_iofmt, rc=localrc)
        errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,     &
            ESMF_CONTEXT, rcToReturn=rc)
      endif
    else
      do i=1,fieldCount
        ! Clear the IO object (only need to do this for i > 1)
        if (i .gt. 1) call ESMF_IOClear(io)
        write(cnum,"(i3.3)") i
        filename_num = trim (fileName) // cnum
        call ESMF_FieldGet(fieldList(i), array=array, name=name, rc=localrc)
        errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,     &
            ESMF_CONTEXT, rcToReturn=rc)
        if (errorFound) exit
        call ESMF_IOAddArray(io, array, variableName=name, rc=localrc)
        errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,     &
            ESMF_CONTEXT, rcToReturn=rc)
        if (errorFound) exit
        if (.not. errorFound) then
          call ESMF_IORead(io, trim(filename_num), timeslice=time,          &
              iofmt=opt_iofmt, rc=localrc)
          errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,   &
              ESMF_CONTEXT, rcToReturn=rc)
        endif
      enddo
    endif

    ! Set rc here in case we had an error but destroy succeeds
    if (present(rc)) rc = localrc

    call ESMF_IODestroy(io, rc=localrc)
    ! Log error but don't reset rc
    errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,         &
        ESMF_CONTEXT, rcToReturn=localrc)

    ! Last chance to return an error code (IODestroy failed)
    if (present(rc)) then
      if (rc == ESMF_SUCCESS) rc = localrc
    end if

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
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed redistribution from {\tt srcFieldBundle}
!   to {\tt dstFieldBundle}. 
!   Both {\tt srcFieldBundle} and {\tt dstFieldBundle} must match the
!   respective FieldBundles used during {\tt ESMF\_FieldBundleRedistStore()}
!   in {\em type}, {\em kind}, and memory layout of the {\em gridded}
!   dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
!   \end{sloppypar}
!
!   The {\tt srcFieldBundle} and {\tt dstFieldBundle} arguments are optional in support of
!   the situation where {\tt srcFieldBundle} and/or {\tt dstFieldBundle} are not defined on
!   all PETs. The {\tt srcFieldBundle} and {\tt dstFieldBundle} must be specified on those
!   PETs that hold source or destination DEs, respectively, but may be omitted
!   on all other PETs. PETs that hold neither source nor destination DEs may
!   omit both arguments.
!
!   It is erroneous to specify the identical FieldBundle object for {\tt srcFieldBundle} and
!   {\tt dstFieldBundle} arguments.
!
!   See {\tt ESMF\_FieldBundleRedistStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   For examples and associated documentation regarding this method see Section
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
      srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    else
      src_bundle = .false.
    endif

    dst_bundle = .true.
    if (present(dstFieldBundle)) then
      dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    else
      dst_bundle = .false.
    endif
        
    ! perform FieldBundle redistribution
    if(src_bundle .and. dst_bundle) then
        call ESMF_ArrayBundleRedist(srcab, dstab, routehandle, &
            checkflag=l_checkflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else if(src_bundle .and. .not. dst_bundle) then
        call ESMF_ArrayBundleRedist(srcArrayBundle=srcab, routehandle=routehandle, &
            checkflag=l_checkflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else if(.not. src_bundle .and. dst_bundle) then
        call ESMF_ArrayBundleRedist(dstArrayBundle=dstab, routehandle=routehandle, &
            checkflag=l_checkflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    else if(.not. src_bundle .and. .not. dst_bundle) then
        call ESMF_ArrayBundleRedist(routehandle=routehandle, &
            checkflag=l_checkflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! garbage collection
    if (present(srcFieldBundle)) then
      call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(dstFieldBundle)) then
      call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
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
  subroutine ESMF_FieldBundleRedistRelease(routehandle, keywordEnforcer, &
    noGarbage, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),   optional  :: noGarbage
        integer,                intent(out),  optional  :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Release resources associated with a FieldBundle redistribution. After this call
!   {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
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
        call ESMF_RouteHandleRelease(routehandle, noGarbage=noGarbage, &
          rc=localrc)
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
!   dstFieldBundle, routehandle, factor, keywordEnforcer, &
!   srcToDstTransposeMap, rc) 
! 
! !ARGUMENTS: 
!   type(ESMF_FieldBundle),   intent(in)             :: srcFieldBundle  
!   type(ESMF_FieldBundle),   intent(inout)          :: dstFieldBundle  
!   type(ESMF_RouteHandle),   intent(inout)          :: routehandle
!   <type>(ESMF_KIND_<kind>), intent(in)             :: factor
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   integer,                  intent(in),   optional :: srcToDstTransposeMap(:)
!   integer,                  intent(out),  optional :: rc 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldBundleRedist()} on any pair of FieldBundles that matches 
!   {\tt srcFieldBundle} and {\tt dstFieldBundle} in {\em type}, {\em kind},
!   and memory layout of the {\em gridded} dimensions. However, the size, 
!   number, and index order of {\em ungridded} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!  
! This call is collective across the current VM.  
! 
! For examples and associated documentation regarding this method see Section
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
!       Factor by which to multiply source data.
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
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap=srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
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
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap=srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
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
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap=srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
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
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, factor, & 
            srcToDstTransposeMap=srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
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
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldBundleRedist()} on any pair of FieldBundles that matches 
!   {\tt srcFieldBundle} and {\tt dstFieldBundle} in {\em type}, {\em kind},
!   and memory layout of the {\em gridded} dimensions. However, the size, 
!   number, and index order of {\em ungridded} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!  
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!
! This call is collective across the current VM.  
! 
! For examples and associated documentation regarding this method see Section
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
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        ! TODO:
        ! internal grids match
        !if(ESMF_GridMatch(srcFieldBundle%btypep%grid, dstFieldBundle%btypep%grid) then
        !    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
        !       "src and dst FieldBundle must have matching grid", &
        !        ESMF_CONTEXT, rcToReturn=rc)
        !    return
        !endif 

        srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayBundleRedistStore(srcab, dstab, routehandle, & 
            srcToDstTransposeMap=srcToDstTransposeMap, rc=localrc) 
        if (ESMF_LogFoundError(localrc, & 
            ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return 

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
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
         routehandle, keywordEnforcer, zeroregion, termorderflag, checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_FieldBundle), intent(in),    optional  :: srcFieldBundle
        type(ESMF_FieldBundle), intent(inout), optional  :: dstFieldBundle
        type(ESMF_RouteHandle), intent(inout)            :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        type(ESMF_Region_Flag), intent(in),    optional  :: zeroregion
        type(ESMF_TermOrder_Flag), intent(in), optional  :: termorderflag(:)
        logical,                intent(in),    optional  :: checkflag
        integer,                intent(out),   optional  :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.0.0] Added argument {\tt termorderflag}.
!              The new argument gives the user control over the order in which
!              the src terms are summed up.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Execute a precomputed regrid from {\tt srcFieldBundle}
!   to {\tt dstFieldBundle}. 
!   Both {\tt srcFieldBundle} and {\tt dstFieldBundle} must match the
!   respective FieldBundles used during {\tt ESMF\_FieldBundleRedistStore()}
!   in {\em type}, {\em kind}, and memory layout of the {\em gridded}
!   dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
!   \end{sloppypar}
!
!   The {\tt srcFieldBundle} and {\tt dstFieldBundle} arguments are optional in support of
!   the situation where {\tt srcFieldBundle} and/or {\tt dstFieldBundle} are not defined on
!   all PETs. The {\tt srcFieldBundle} and {\tt dstFieldBundle} must be specified on those
!   PETs that hold source or destination DEs, respectively, but may be omitted
!   on all other PETs. PETs that hold neither source nor destination DEs may
!   omit both arguments.
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
!   \item [{[zeroregion]}]
!     \begin{sloppypar}
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of
!     all DEs in {\tt dstFieldBundle} will be initialized to zero before updating the 
!     elements with the results of the sparse matrix multiplication. If set to
!     {\tt ESMF\_REGION\_EMPTY} the elements in {\tt dstFieldBundle} will not be
!     modified prior to the sparse matrix multiplication and results will be
!     added to the incoming element values. Setting {\tt zeroregion} to 
!     {\tt ESMF\_REGION\_SELECT} will only zero out those elements in the 
!     destination FieldBundle that will be updated by the sparse matrix
!     multiplication. See section \ref{const:region} for a complete list of
!     valid settings.
!     \end{sloppypar}
!   \item [{[termorderflag]}]
!     Specifies the order of the source side terms in all of the destination
!     sums. The {\tt termorderflag} only affects the order of terms during 
!     the execution of the RouteHandle. See the \ref{RH:bfb} section for an
!     in-depth discussion of {\em all} bit-for-bit reproducibility
!     aspects related to route-based communication methods.
!     See \ref{const:termorderflag} for a full list of options.
!     The size of this array argument must either be 1 or equal the number of
!     Fields in the {\tt srcFieldBundle} and {\tt dstFieldBundle} arguments. In
!     the latter case, the term order for each Field Regrid operation is
!     indicated separately. If only one term order element is specified, it is
!     used for {\em all} Field pairs.
!     The default is {\tt (/ESMF\_TERMORDER\_FREE/)}, allowing maximum 
!     flexibility in the order of terms for optimum performance.
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
          zeroregion=zeroregion, termorderflag=termorderflag, &
          checkflag=checkflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleRegrid
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegridRelease()"
!BOP
! !IROUTINE: ESMF_FieldBundleRegridRelease - Release resources associated with a FieldBundle regrid operation
!
! !INTERFACE:
  subroutine ESMF_FieldBundleRegridRelease(routehandle, keywordEnforcer, &
    noGarbage, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),   optional  :: noGarbage
        integer,                intent(out),  optional  :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Release resources associated with a FieldBundle regrid operation. After this call
!   {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
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
        call ESMF_RouteHandleRelease(routehandle, noGarbage=noGarbage, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleRegridRelease
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegridStore()"
!BOP
! !IROUTINE: ESMF_FieldBundleRegridStore - Precompute a FieldBundle regrid operation
! \label{api:esmf_fieldbundleregridstore}
! !INTERFACE:
  subroutine ESMF_FieldBundleRegridStore(srcFieldBundle, dstFieldBundle, &
       srcMaskValues, dstMaskValues, regridmethod, polemethod, regridPoleNPnts, &
       lineType, normType, extrapMethod, extrapNumSrcPnts, extrapDistExponent, &
       unmappedaction,  ignoreDegenerate, srcTermProcessing, &
       pipelineDepth, routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle),        intent(in)              :: srcFieldBundle
    type(ESMF_FieldBundle),        intent(inout)           :: dstFieldBundle
    integer(ESMF_KIND_I4),         intent(in),    optional :: srcMaskValues(:)
    integer(ESMF_KIND_I4),         intent(in),    optional :: dstMaskValues(:)
    type(ESMF_RegridMethod_Flag),  intent(in),    optional :: regridmethod
    type(ESMF_PoleMethod_Flag),    intent(in),    optional :: polemethod
    integer,                       intent(in),    optional :: regridPoleNPnts
    type(ESMF_LineType_Flag),      intent(in),    optional :: lineType
    type(ESMF_NormType_Flag),      intent(in),    optional :: normType
    type(ESMF_ExtrapMethod_Flag),   intent(in),   optional :: extrapMethod
    integer,                        intent(in),   optional :: extrapNumSrcPnts
    real,                           intent(in),   optional :: extrapDistExponent
    type(ESMF_UnmappedAction_Flag),intent(in),    optional :: unmappedaction
    logical,                       intent(in),    optional :: ignoreDegenerate 
    integer,                       intent(inout), optional :: srcTermProcessing
    integer,                       intent(inout), optional :: pipelineDepth
    type(ESMF_RouteHandle),        intent(inout), optional :: routehandle
    integer,                       intent(out),   optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.0.0] Added arguments {\tt ignoreDegenerate}, {\tt lineType},
!              and {\tt normType}. The argument {\tt ignoreDegenerate} allows the user to skip degenerate
!              cells in the regridding instead of stopping with an error. 
!              The argument {\tt lineType} allows the user to 
!              control the path of the line between two points on a sphere surface. 
!              This allows the user to use their preferred line path for the calculation
!              of distances and the shape of cells during regrid weight calculation on 
!              a sphere. The argument {\tt normType} allows the user to 
!              control the type of normalization done during conservative weight generation. 
! \item[7.1.0r] Added argument {\tt srcTermProcessing}.
!              Added argument {\tt pipelineDepth}.
!              The new arguments provide access to the tuning parameters
!              affecting the performance and bit-for-bit behavior when applying
!              the regridding weights.
!
!              Added arguments {\tt extrapMethod}, {\tt extrapNumSrcPnts}, and
!              {\tt extrapDistExponent}. These three new extrapolation arguments allow the 
!              user to extrapolate destination points not mapped by the regrid method. 
!              {\tt extrapMethod} allows the user to choose the extrapolation method.
!              {\tt extrapNumSrcPnts} and {\tt extrapDistExponent} are parameters that
!              allow the user to tune the behavior of the {\tt ESMF\_EXTRAPMETHOD\_NEAREST\_IDAVG} 
!              method.
! 
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Store a FieldBundle regrid operation over the data in {\tt srcFieldBundle} and
!   {\tt dstFieldBundle} pair. 
!  
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldBundleRegrid()} on any pair of FieldBundles that matches 
!   {\tt srcFieldBundle} and {\tt dstFieldBundle} in {\em type}, {\em kind},
!   and memory layout of the {\em gridded} dimensions. However, the size, 
!   number, and index order of {\em ungridded} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!  
!   This call is {\em collective} across the current VM.  
!
!   \begin{description}
!   \item [srcFieldbundle]
!     Source {\tt ESMF\_FieldBundle} containing data to be regridded.
!   \item [dstFieldbundle]
!     Destination {\tt ESMF\_FieldBundle}. The data in this FieldBundle may be overwritten by this call. 
!  \item [{[srcMaskValues]}]
!     Mask information can be set in the Grids (see~\ref{sec:usage:items}) or Meshes (see~\ref{sec:mesh:mask}) upon which
!     the Fields in the {\tt srcFieldbundle} are built. 
!     The {\tt srcMaskValues} argument specifies the values in that mask information which indicate a source point should be masked out. 
!     In other words, a location is masked if and only if the value for that location in the mask information matches
!     one of the values listed in {\tt srcMaskValues}.  
!     If {\tt srcMaskValues} is not specified, no masking will occur. 
!  \item [{[dstMaskValues]}]
!     Mask information can be set in the Grids (see~\ref{sec:usage:items}) or Meshes (see~\ref{sec:mesh:mask})
!     upon which the Fields in the {\tt dstFieldbundle} are built. 
!     The {\tt dstMaskValues} argument specifies the values in that mask information which indicate a destination point should be masked out. 
!     In other words, a location is masked if and only if the value for that location in the mask information matches
!     one of the values listed in {\tt dstMaskValues}.  
!     If {\tt dstMaskValues} is not specified, no masking will occur. 
!   \item [{[regridmethod]}]
!     The type of interpolation. Please see Section~\ref{opt:regridmethod} for a list of
!     valid options. If not specified, defaults to {\tt ESMF\_REGRIDMETHOD\_BILINEAR}.
!   \item [{[polemethod]}]
!    Which type of artificial pole
!    to construct on the source Grid for regridding. Please see Section~\ref{const:polemethod} for a list of
!    valid options. If not specified, defaults to {\tt ESMF\_POLEMETHOD\_ALLAVG}. 
!   \item [{[regridPoleNPnts]}]
!    If {\tt polemethod} is {\tt ESMF\_POLEMETHOD\_NPNTAVG}.
!    This parameter indicates how many points should be averaged
!    over. Must be specified if {\tt polemethod} is 
!    {\tt ESMF\_POLEMETHOD\_NPNTAVG}.
!   \item [{[lineType]}]
!           This argument controls the path of the line which connects two points on a sphere surface. This in
!           turn controls the path along which distances are calculated and the shape of the edges that make
!           up a cell. Both of these quantities can influence how interpolation weights are calculated. 
!           As would be expected, this argument is only applicable when {\tt srcField} and {\tt dstField} are
!           built on grids which lie on the surface of a sphere. Section~\ref{opt:lineType} shows a 
!           list of valid options for this argument. If not specified, the default depends on the 
!           regrid method. Section~\ref{opt:lineType} has the defaults by line type. Figure~\ref{line_type_support} shows
!           which line types are supported for each regrid method as well as showing the default line type by regrid method.  
!  \item [{[normType]}] 
!           This argument controls the type of normalization used when generating conservative weights. This option
!           only applies to weights generated with {\tt regridmethod=ESMF\_REGRIDMETHOD\_CONSERVE}. Please see 
!           Section~\ref{opt:normType} for a 
!           list of valid options. If not specified {\tt normType} defaults to {\tt ESMF\_NORMTYPE\_DSTAREA}. 
!  \item [{[extrapMethod]}]
!           The type of extrapolation. Please see Section~\ref{opt:extrapmethod} 
!           for a list of valid options. If not specified, defaults to 
!           {\tt ESMF\_EXTRAPMETHOD\_NONE}.
!  \item [{[extrapNumSrcPnts]}] 
!           The number of source points to use for the extrapolation methods that use more than one source point 
!           (e.g. {\tt ESMF\_EXTRAPMETHOD\_NEAREST\_IDAVG}). If not specified, defaults to 8.
!  \item [{[extrapDistExponent]}] 
!           The exponent to raise the distance to when calculating weights for 
!           the {\tt ESMF\_EXTRAPMETHOD\_NEAREST\_IDAVG} extrapolation method. A higher value reduces the influence 
!           of more distant points. If not specified, defaults to 2.0.
!  \item [{[unmappedaction]}]
!    Specifies what should happen if there are destination points that
!    can't be mapped to a source cell. Please see Section~\ref{const:unmappedaction} for a 
!           list of valid options. If not specified, {\tt unmappedaction} defaults to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. 
!   \item [{[ignoreDegenerate]}]
!           Ignore degenerate cells when checking the input Grids or Meshes for errors. If this is set to true, then the 
!           regridding proceeds, but degenerate cells will be skipped. If set to false, a degenerate cell produces an error. 
!           If not specified, {\tt ignoreDegenerate} defaults to false.
!
!   \item [{[srcTermProcessing]}]
!     The {\tt srcTermProcessing} parameter controls how many source terms,
!     located on the same PET and summing into the same destination element,
!     are summed into partial sums on the source PET before being transferred
!     to the destination PET. A value of 0 indicates that the entire arithmetic
!     is done on the destination PET; source elements are neither multiplied 
!     by their factors nor added into partial sums before being sent off by the
!     source PET. A value of 1 indicates that source elements are multiplied
!     by their factors on the source side before being sent to the destination
!     PET. Larger values of {\tt srcTermProcessing} indicate the maximum number
!     of terms in the partial sums on the source side.
!
!     Note that partial sums may lead to bit-for-bit differences in the results.
!     See section \ref{RH:bfb} for an in-depth discussion of {\em all}
!     bit-for-bit reproducibility aspects related to route-based communication
!     methods.
!
!     \begin{sloppypar}
!     The {\tt ESMF\_FieldRegridStore()} method implements an auto-tuning scheme
!     for the {\tt srcTermProcessing} parameter. The intent on the 
!     {\tt srcTermProcessing} argument is "{\tt inout}" in order to 
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the 
!     {\tt srcTermProcessing} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt srcTermProcessing} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt srcTermProcessing}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt srcTermProcessing} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional 
!     {\tt srcTermProcessing} argument is omitted.
!     \end{sloppypar}
!     
!   \item [{[pipelineDepth]}]
!     The {\tt pipelineDepth} parameter controls how many messages a PET
!     may have outstanding during a sparse matrix exchange. Larger values
!     of {\tt pipelineDepth} typically lead to better performance. However,
!     on some systems too large a value may lead to performance degradation,
!     or runtime errors.
!
!     Note that the pipeline depth has no effect on the bit-for-bit
!     reproducibility of the results. However, it may affect the performance
!     reproducibility of the exchange.
!
!     The {\tt ESMF\_FieldRegridStore()} method implements an auto-tuning scheme
!     for the {\tt pipelineDepth} parameter. The intent on the 
!     {\tt pipelineDepth} argument is "{\tt inout}" in order to 
!     support both overriding and accessing the auto-tuning parameter.
!     If an argument $>= 0$ is specified, it is used for the 
!     {\tt pipelineDepth} parameter, and the auto-tuning phase is skipped.
!     In this case the {\tt pipelineDepth} argument is not modified on
!     return. If the provided argument is $< 0$, the {\tt pipelineDepth}
!     parameter is determined internally using the auto-tuning scheme. In this
!     case the {\tt pipelineDepth} argument is re-set to the internally
!     determined value on return. Auto-tuning is also used if the optional 
!     {\tt pipelineDepth} argument is omitted.
!     
!   \item [{[routehandle]}]
!     Handle to the precomputed Route.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                         :: localrc, stat
    character(len=160)              :: msgString
    integer                         :: i, j
    integer                         :: count, localDeCount
    type(ESMF_Field), allocatable   :: srcFields(:), dstFields(:)
    integer                         :: rraShift, vectorLengthShift
    type(ESMF_RouteHandle)          :: rhh
    integer(ESMF_KIND_I4), pointer  :: factorIndexList(:,:)
    real(ESMF_KIND_R8), pointer     :: factorList(:)
    type(ESMF_Grid)                 :: srcGrid, dstGrid
    type(ESMF_GeomType_Flag)        :: srcGeomtype, dstGeomtype
    type(ESMF_ArraySpec)            :: srcArraySpec, dstArraySpec
    type(ESMF_StaggerLoc)           :: srcStaggerLoc, dstStaggerLoc
    integer, pointer                :: srcGridToFieldMap(:)
    integer, pointer                :: dstGridToFieldMap(:)
    integer, pointer                :: srcUngriddedLBound(:)
    integer, pointer                :: srcUngriddedUBound(:)
    integer, pointer                :: dstUngriddedLBound(:)
    integer, pointer                :: dstUngriddedUBound(:)
    integer                         :: fieldDimCount, gridDimCount
    logical                         :: gridPair

    type RHL
      type(ESMF_Grid)                   :: srcGrid, dstGrid
      ! field specific items, TODO: push into a FieldMatch() method
      type(ESMF_ArraySpec)              :: srcArraySpec, dstArraySpec
      type(ESMF_StaggerLoc)             :: srcStaggerLoc, dstStaggerLoc
      integer, pointer                  :: srcGridToFieldMap(:)
      integer, pointer                  :: dstGridToFieldMap(:)
      integer, pointer                  :: srcUngriddedLBound(:)
      integer, pointer                  :: srcUngriddedUBound(:)
      integer, pointer                  :: dstUngriddedLBound(:)
      integer, pointer                  :: dstUngriddedUBound(:)
      ! remap specific items
      type(ESMF_RouteHandle)            :: rh
      logical                           :: factorAllocFlag
      integer(ESMF_KIND_I4), pointer    :: factorIndexList(:,:)
      real(ESMF_KIND_R8), pointer       :: factorList(:)
      type(RHL), pointer                :: prev
    end type
    
    type(RHL), pointer              :: rhList, rhListE, rhListG
    logical                         :: rhListMatch

    ! Initialize return code; assume routine not implemented 
    localrc = ESMF_RC_NOT_IMPL 
    if(present(rc)) rc = ESMF_RC_NOT_IMPL 

    ! check variables
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, srcFieldBundle, rc) 
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, dstFieldBundle, rc) 

    ! consistency check counts
    call ESMF_FieldBundleGet(srcFieldBundle, fieldCount=count, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldBundleGet(dstFieldBundle, fieldCount=i, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (i /= count) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Number of Fields in srcFieldBundle/dstFieldBundle must match!", &
        ESMF_CONTEXT, rcToReturn=rc)
      return  ! bail out
    endif

    ! access the fields in the add order
    allocate(srcFields(count), dstFields(count), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of srcFields and dstFields.", &
      ESMF_CONTEXT, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_FieldBundleGet(srcFieldBundle, fieldList=srcFields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldBundleGet(dstFieldBundle, fieldList=dstFields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! prepare Routehandle
    if (present(routehandle)) then
      ! create Routehandle
      routehandle = ESMF_RouteHandleCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! set the type for bundle execution
      call ESMF_RouteHandlePrepXXE(routehandle, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif        

    ! prepare auxiliary variables
    rraShift = 0              ! reset
    vectorLengthShift = 0     ! reset
    
    ! prepare rhList linked list
    nullify(rhList)
    
    ! loop over all fields
    do i=1, count
    
      call ESMF_FieldGet(srcFields(i), geomtype=srcGeomtype, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldGet(dstFields(i), geomtype=dstGeomtype, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      gridPair = (srcGeomtype==ESMF_GEOMTYPE_GRID)
      gridPair = gridPair .and. (dstGeomtype==ESMF_GEOMTYPE_GRID)

      rhListMatch = .false.
      rhListG=>NULL()
      
      if (gridPair) then
        ! access the src and dst grid objects
        call ESMF_FieldGet(srcFields(i), arrayspec=srcArraySpec, grid=srcGrid, &
          staggerLoc=srcStaggerLoc, dimCount=fieldDimCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_GridGet(srcGrid, dimCount=gridDimCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(srcGridToFieldMap(gridDimCount))
        allocate(srcUngriddedLBound(fieldDimCount-gridDimCount), &
          srcUngriddedUBound(fieldDimCount-gridDimCount))
        call ESMF_FieldGet(srcFields(i), gridToFieldMap=srcGridToFieldMap, &
          ungriddedLBound=srcUngriddedLBound, &
          ungriddedUBound=srcUngriddedUBound,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldGet(dstFields(i), arrayspec=dstArraySpec, grid=dstGrid, &
          staggerLoc=dstStaggerLoc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_GridGet(dstGrid, dimCount=gridDimCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(dstGridToFieldMap(gridDimCount))
        allocate(dstUngriddedLBound(fieldDimCount-gridDimCount), &
          dstUngriddedUBound(fieldDimCount-gridDimCount))
        call ESMF_FieldGet(dstFields(i), gridToFieldMap=dstGridToFieldMap, &
          ungriddedLBound=dstUngriddedLBound, &
          ungriddedUBound=dstUngriddedUBound,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! search for a match
        rhListE=>rhList
        do while (associated(rhListE))
          ! test src grid match
          rhListMatch = &
            ESMF_GridMatch(rhListE%srcGrid, srcGrid, globalflag=.true.) &
            >= ESMF_GRIDMATCH_EXACT
          if (.not.rhListMatch) goto 123
          ! test dst grid match
          rhListMatch = &
            ESMF_GridMatch(rhListE%dstGrid, dstGrid, globalflag=.true.) &
            >= ESMF_GRIDMATCH_EXACT
          if (.not.rhListMatch) goto 123
          ! keep record of entry for which grids were matching
          rhListG=>rhListE
          ! test src arrayspec match
          rhListMatch = (rhListE%srcArraySpec==srcArraySpec)
          if (.not.rhListMatch) goto 123
          ! test dst arrayspec match
          rhListMatch = (rhListE%dstArraySpec==dstArraySpec)
          if (.not.rhListMatch) goto 123
          ! test src staggerLoc match
          rhListMatch = (rhListE%srcStaggerLoc==srcStaggerLoc)
          if (.not.rhListMatch) goto 123
          ! test dst staggerLoc match
          rhListMatch = (rhListE%dstStaggerLoc==dstStaggerLoc)
          if (.not.rhListMatch) goto 123
          ! test srcGridToFieldMap
          rhListMatch = &
            (size(rhListE%srcGridToFieldMap)==size(srcGridToFieldMap))
          if (.not.rhListMatch) goto 123
          do j=1, size(srcGridToFieldMap)
            rhListMatch = (rhListE%srcGridToFieldMap(j)==srcGridToFieldMap(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test dstGridToFieldMap
          rhListMatch = &
            (size(rhListE%dstGridToFieldMap)==size(dstGridToFieldMap))
          if (.not.rhListMatch) goto 123
          do j=1, size(dstGridToFieldMap)
            rhListMatch = (rhListE%dstGridToFieldMap(j)==dstGridToFieldMap(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test srcUngriddedLBound
          rhListMatch = &
            (size(rhListE%srcUngriddedLBound)==size(srcUngriddedLBound))
          if (.not.rhListMatch) goto 123
          do j=1, size(srcUngriddedLBound)
            rhListMatch = (rhListE%srcUngriddedLBound(j)==srcUngriddedLBound(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test srcUngriddedUBound
          rhListMatch = &
            (size(rhListE%srcUngriddedUBound)==size(srcUngriddedUBound))
          if (.not.rhListMatch) goto 123
          do j=1, size(srcUngriddedUBound)
            rhListMatch = (rhListE%srcUngriddedUBound(j)==srcUngriddedUBound(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test dstUngriddedLBound
          rhListMatch = &
            (size(rhListE%dstUngriddedLBound)==size(dstUngriddedLBound))
          if (.not.rhListMatch) goto 123
          do j=1, size(dstUngriddedLBound)
            rhListMatch = (rhListE%dstUngriddedLBound(j)==dstUngriddedLBound(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! test dstUngriddedUBound
          rhListMatch = &
            (size(rhListE%dstUngriddedUBound)==size(dstUngriddedUBound))
          if (.not.rhListMatch) goto 123
          do j=1, size(dstUngriddedUBound)
            rhListMatch = (rhListE%dstUngriddedUBound(j)==dstUngriddedUBound(j))
            if (.not.rhListMatch) goto 123
          enddo
          ! completed search 
          exit ! break out
          ! continue search with previous element
123       continue
          rhListE=>rhListE%prev   ! previous element
        enddo
        
      endif

      if (.not.rhListMatch) then
        ! No match found: precompute new RH for this field pair
#if 0
call ESMF_LogWrite("no rhListMatch -> pre-compute new remapping!", &
  ESMF_LOGMSG_INFO)
#endif
        if (gridPair) then
          ! add a new rhList element
          allocate(rhListE)
          rhListE%prev=>rhList  ! link new element to previous list head
          rhList=>rhListE       ! list head now pointing to new element
        endif

        if (associated(rhListG)) then
          ! able to reuse already precomputed weight matrix
#if 0
call ESMF_LogWrite("able to reuse already precomputed weight matrix!", &
  ESMF_LOGMSG_INFO)
#endif
          factorList=>rhListG%factorList
          factorIndexList=>rhListG%factorIndexList
          call ESMF_FieldSMMStore(srcField=srcFields(i), &
            dstField=dstFields(i), &
            routehandle=rhh, &
            factorList=factorList, factorIndexList=factorIndexList, &
            srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
            rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          ! must precompute full regridding
#if 0
call ESMF_LogWrite("must precompute full regridding!", &
  ESMF_LOGMSG_INFO)
#endif
          factorList=>NULL()
          factorIndexList=>NULL()
          call ESMF_FieldRegridStore(srcField=srcFields(i), &
            dstField=dstFields(i), &
            srcMaskValues=srcMaskValues, dstMaskValues=dstMaskValues, &
            regridmethod=regridmethod, &
            polemethod=polemethod, regridPoleNPnts=regridPoleNPnts, &
            lineType=lineType, normType=normType, &
            extrapMethod=extrapMethod, &
            extrapNumSrcPnts=extrapNumSrcPnts, &
            extrapDistExponent=extrapDistExponent, &
            unmappedaction=unmappedaction, &
            srcTermProcessing=srcTermProcessing, pipelineDepth=pipelineDepth, &
            routehandle=rhh, &
            factorList=factorList, factorIndexList=factorIndexList, &
            rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if (gridPair) then
          ! store info in the new rhList element
          rhListE%srcGrid             =  srcGrid
          rhListE%dstGrid             =  dstGrid
          rhListE%srcArraySpec        =  srcArraySpec
          rhListE%dstArraySpec        =  dstArraySpec
          rhListE%srcStaggerLoc       =  srcStaggerLoc
          rhListE%dstStaggerLoc       =  dstStaggerLoc
          rhListE%srcGridToFieldMap   => srcGridToFieldMap
          rhListE%dstGridToFieldMap   => dstGridToFieldMap
          rhListE%srcUngriddedLBound  => srcUngriddedLBound
          rhListE%srcUngriddedUBound  => srcUngriddedUBound
          rhListE%dstUngriddedLBound  => dstUngriddedLBound
          rhListE%dstUngriddedUBound  => dstUngriddedUBound
          rhListE%rh                  =  rhh
          rhListE%factorAllocFlag     =  .not.associated(rhListG)
          rhListE%factorIndexList     => factorIndexList
          rhListE%factorList          => factorList
        endif
      else
        ! Match found: reuse previous RH for this field pair
#if 0
call ESMF_LogWrite("found rhListMatch -> reuse routehandle!", &
  ESMF_LOGMSG_INFO)
#endif
        ! pull out the routehandle from the matching rhList element
        rhh = rhListE%rh
        ! deallocate temporary grid/field info
        deallocate(srcGridToFieldMap, dstGridToFieldMap)
        deallocate(srcUngriddedLBound, srcUngriddedUBound)
        deallocate(dstUngriddedLBound, dstUngriddedUBound)
      endif
      
      ! append rhh to rh and clear rhh
      call ESMF_RouteHandleAppend(routehandle, appendRoutehandle=rhh, &
        rraShift=rraShift, vectorLengthShift=vectorLengthShift, &
        transferflag=.not.rhListMatch, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! adjust rraShift and vectorLengthShift
      call ESMF_FieldGet(srcFields(i), localDeCount=localDeCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      rraShift = rraShift + localDeCount
      call ESMF_FieldGet(dstFields(i), localDeCount=localDeCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      rraShift = rraShift + localDeCount
      vectorLengthShift = vectorLengthShift + 1
      
      ! local garbage collection
      if (.not.gridPair) then
        ! grid pairs will have factorIndexList and factorList in rhList struct
        if (associated(factorIndexList)) deallocate(factorIndexList)
        if (associated(factorList)) deallocate(factorList)
      endif

    enddo
    
    ! take down rhList and destroy rh objects
    do while (associated(rhList))
      rhListE=>rhList
      rhList=>rhList%prev
      call ESMF_RouteHandleDestroy(rhListE%rh, noGarbage=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (rhListE%factorAllocFlag) then
        deallocate(rhListE%factorIndexList)
        deallocate(rhListE%factorList)
      endif
      deallocate(rhListE%srcGridToFieldMap, rhListE%dstGridToFieldMap)
      deallocate(rhListE%srcUngriddedLBound, rhListE%srcUngriddedUBound)
      deallocate(rhListE%dstUngriddedLBound, rhListE%dstUngriddedUBound)
      deallocate(rhListE)
    enddo
    
    ! garbage collection
    deallocate(srcFields, dstFields)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleRegridStore
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRemove()"
!BOP
! !IROUTINE: ESMF_FieldBundleRemove - Remove Fields from FieldBundle
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
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Remove field(s) by name from FieldBundle. In the relaxed setting it is 
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
    integer                       :: fieldCount, i, fcount
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

    ! Remove attribute link for those Fields that were removed
    do i=1, size(garbageList)
      call c_ESMC_AttributeLinkRemove(fieldbundle%this%base, garbageList(i)%ftypep%base, linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
    enddo
    deallocate(garbageList)

    ! Check if the fieldbundle is empty
    call ESMF_FieldBundleGet(fieldbundle, fieldCount=fcount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    if(fcount == 0) then
      call ESMF_FieldBundleRemoveGeom(fieldbundle, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleRemove
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleReplace()"
!BOP
! !IROUTINE: ESMF_FieldBundleReplace - Replace Fields in FieldBundle
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
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Replace field(s) by name in FieldBundle. In the relaxed setting it is not
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

    ! Attribute link
    fieldCount = size(fieldList)
    if(fieldCount .ge. 1 .and. (fieldbundle%this%status /= ESMF_FBSTATUS_GRIDSET) ) then
      ! setgeom links grid geombase automatically
      call ESMF_FieldBundleSetGeom(fieldbundle, fieldList(1), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc))  return
    endif ! non-empty fieldlist

    ! Add all the input Fields in fieldList
    linkChange = ESMF_TRUE
    do i = 1, fieldCount
      call c_ESMC_AttributeLink(fieldbundle%this%base, fieldList(i)%ftypep%base, linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
    enddo
    
    ! Remove those that were replaced or not added
    do i = 1, size(garbageList)
      call c_ESMC_AttributeLinkRemove(fieldbundle%this%base, garbageList(i)%ftypep%base, linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
    enddo
    deallocate(garbageList)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  
  end subroutine ESMF_FieldBundleReplace
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetGrid"
!BOP
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
!   Sets the {\tt grid} for a {\tt fieldbundle}.
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
!EOP


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
!BOP
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
!   Sets the {\tt mesh} for a {\tt fieldbundle}.
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
!EOP


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
!BOP
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
!   \begin{sloppypar}
!   Sets the {\tt locstream} for a {\tt fieldbundle}.
!   \end{sloppypar}
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
!EOP


      integer :: status                           ! Error status
      type(ESMF_FieldBundleType), pointer :: btype     ! internal data
      type(ESMF_Logical) :: linkChange

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
       btype%geombase=ESMF_GeomBaseCreate(locstream, rc=status)
       if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      ! Set Status to containing a Geombase
      btype%status = ESMF_FBSTATUS_GRIDSET

      !  link the Attribute hierarchies
      linkChange = ESMF_TRUE
      call c_ESMC_AttributeLink(btype%base, locstream%lstypep%base, linkChange, status)
      if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetLS
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetXGrid"
!BOP
! !IROUTINE: ESMF_FieldBundleSet - Associate a XGrid with an empty FieldBundle
! 
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSet()
      subroutine ESMF_FieldBundleSetXGrid(fieldbundle, xgrid, &
        keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout)         :: fieldbundle
      type(ESMF_XGrid),       intent(in)            :: xgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Sets the {\tt xgrid} for a {\tt fieldbundle}
!   \end{sloppypar}
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
!EOP

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
        routehandle, keywordEnforcer, zeroregion, termorderflag, checkflag, rc)
!
! !ARGUMENTS:
        type(ESMF_FieldBundle), intent(in),    optional  :: srcFieldBundle
        type(ESMF_FieldBundle), intent(inout), optional  :: dstFieldBundle
        type(ESMF_RouteHandle), intent(inout)            :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        type(ESMF_Region_Flag), intent(in),    optional  :: zeroregion
        type(ESMF_TermOrder_Flag), intent(in), optional  :: termorderflag(:)
        logical,                intent(in),    optional  :: checkflag
        integer,                intent(out),   optional  :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.0.0] Added argument {\tt termorderflag}.
!              The new argument gives the user control over the order in which
!              the src terms are summed up.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Execute a precomputed sparse matrix multiplication from {\tt srcFieldBundle}
!   to {\tt dstFieldBundle}.
!   Both {\tt srcFieldBundle} and {\tt dstFieldBundle} must match the
!   respective FieldBundles used during {\tt ESMF\_FieldBundleRedistStore()}
!   in {\em type}, {\em kind}, and memory layout of the {\em gridded}
!   dimensions. However, the size, number, 
!   and index order of {\em ungridded} dimensions may be different. See section
!   \ref{RH:Reusability} for a more detailed discussion of RouteHandle 
!   reusability.
!
!   The {\tt srcFieldBundle} and {\tt dstFieldBundle} arguments are optional in support of
!   the situation where {\tt srcFieldBundle} and/or {\tt dstFieldBundle} are not defined on
!   all PETs. The {\tt srcFieldBundle} and {\tt dstFieldBundle} must be specified on those
!   PETs that hold source or destination DEs, respectively, but may be omitted
!   on all other PETs. PETs that hold neither source nor destination DEs may
!   omit both arguments.
!
!   It is erroneous to specify the identical FieldBundle object for {\tt srcFieldBundle} and
!   {\tt dstFieldBundle} arguments.
!
!   See {\tt ESMF\_FieldBundleSMMStore()} on how to precompute 
!   {\tt routehandle}.
!
!   This call is {\em collective} across the current VM.
!
!   For examples and associated documentation regarding this method see Section
!   \ref{sec:fieldbundle:usage:smm_1dptr}. 
!
!   \begin{description}
!   \item [{[srcFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with source data.
!   \item [{[dstFieldBundle]}]
!     {\tt ESMF\_FieldBundle} with destination data.
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item [{[zeroregion]}]
!     If set to {\tt ESMF\_REGION\_TOTAL} {\em (default)} the total regions of
!     all DEs in {\tt dstFieldBundle} will be initialized to zero before updating the 
!     elements with the results of the sparse matrix multiplication. If set to
!     {\tt ESMF\_REGION\_EMPTY} the elements in {\tt dstFieldBundle} will not be
!     modified prior to the sparse matrix multiplication and results will be
!     added to the incoming element values. Setting {\tt zeroregion} to 
!
!     {\tt ESMF\_REGION\_SELECT} will only zero out those elements in the 
!     destination FieldBundle that will be updated by the sparse matrix
!     multiplication. See section \ref{const:region} for a complete list of
!     valid settings.
!   \item [{[termorderflag]}]
!     Specifies the order of the source side terms in all of the destination
!     sums. The {\tt termorderflag} only affects the order of terms during 
!     the execution of the RouteHandle. See the \ref{RH:bfb} section for an
!     in-depth discussion of {\em all} bit-for-bit reproducibility
!     aspects related to route-based communication methods.
!     See \ref{const:termorderflag} for a full list of options.
!     The size of this array argument must either be 1 or equal the number of
!     Fields in the {\tt srcFieldBundle} and {\tt dstFieldBundle} arguments. In
!     the latter case, the term order for each Field SMM operation is
!     indicated separately. If only one term order element is specified, it is
!     used for {\em all} Field pairs.
!     The default is {\tt (/ESMF\_TERMORDER\_FREE/)}, allowing maximum 
!     flexibility in the order of terms for optimum performance.
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
        type(ESMF_Region_Flag)   :: l_zeroregion
        logical                 :: l_checkflag! helper variable

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
        l_zeroregion = ESMF_REGION_TOTAL
        if (present(zeroregion)) l_zeroregion = zeroregion

        src_bundle = .true.
        if (present(srcFieldBundle)) then
          srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          src_bundle = .false.
        endif

        dst_bundle = .true.
        if (present(dstFieldBundle)) then
          dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          dst_bundle = .false.
        endif

        ! perform FieldBundle SMM
        if(src_bundle .and. dst_bundle) then
          call ESMF_ArrayBundleSMM(srcab, dstab, routehandle, &
            zeroregion=l_zeroregion, termorderflag=termorderflag, &
            checkflag=l_checkflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else if(src_bundle .and. .not. dst_bundle) then
          call ESMF_ArrayBundleSMM(srcArrayBundle=srcab, &
            routehandle=routehandle, &
            zeroregion=l_zeroregion, termorderflag=termorderflag, &
            checkflag=l_checkflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else if(.not. src_bundle .and. dst_bundle) then
          call ESMF_ArrayBundleSMM(dstArrayBundle=dstab, &
            routehandle=routehandle, &
            zeroregion=l_zeroregion, termorderflag=termorderflag, &
            checkflag=l_checkflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else if(.not. src_bundle .and. .not. dst_bundle) then
          call ESMF_ArrayBundleSMM(routehandle=routehandle, &
            zeroregion=l_zeroregion, termorderflag=termorderflag, &
            checkflag=l_checkflag, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
            
        ! garbage collection
        if (present(srcFieldBundle)) then
          call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if (present(dstFieldBundle)) then
          call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
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
  subroutine ESMF_FieldBundleSMMRelease(routehandle, keywordEnforcer, &
    noGarbage, rc)
!
! !ARGUMENTS:
        type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        logical,                intent(in),   optional  :: noGarbage
        integer,                intent(out),  optional  :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.0.0] Added argument {\tt noGarbage}.
!   The argument provides a mechanism to override the default garbage collection
!   mechanism when destroying an ESMF object.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Release resources associated with a FieldBundle sparse matrix multiplication. After this call
!   {\tt routehandle} becomes invalid.
!
!   \begin{description}
!   \item [routehandle]
!     Handle to the precomputed Route.
!   \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
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
        call ESMF_RouteHandleRelease(routehandle, noGarbage=noGarbage, &
          rc=localrc)
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
!   keywordEnforcer, srcTermProcessing, rc)
! 
! !ARGUMENTS: 
!   type(ESMF_FieldBundle),   intent(in)            :: srcFieldBundle  
!   type(ESMF_FieldBundle),   intent(inout)         :: dstFieldBundle  
!   type(ESMF_RouteHandle),   intent(inout)         :: routehandle
!   <type>(ESMF_KIND_<kind>), intent(in)            :: factorList(:) 
!   integer,                  intent(in),           :: factorIndexList(:,:) 
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   integer,                intent(inout), optional :: srcTermProcessing(:)
!   integer,                  intent(out), optional :: rc 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.1.0r] Added argument {\tt srcTermProcessing}.
!              The new argument gives the user access to the tuning parameter
!              affecting the sparse matrix execution and bit-wise
!              reproducibility.
! \end{description}
! \end{itemize}
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
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldBundleSMM()} on any pair of FieldBundles that matches 
!   {\tt srcFieldBundle} and {\tt dstFieldBundle} in {\em type}, {\em kind},
!   and memory layout of the {\em gridded} dimensions. However, the size, 
!   number, and index order of {\em ungridded} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!  
! This method is overloaded for:\newline
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},\newline 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \newline
!  
! This call is collective across the current VM.  
! 
! For examples and associated documentation regarding this method see Section
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
!     Under this condition an identity matrix can be applied within the space of
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
! \item [{[srcTermProcessing]}]
!       Source term summing options for route handle creation. See
!       {\tt ESMF\_FieldRegridStore} documentation for a full parameter description.
!       Two forms may be provided. If a single element list is provided, this
!       integer value is applied across all bundle members. Otherwise, the list must
!       contain as many elements as there are bundle members. For the special case
!       of accessing the auto-tuned parameter (providing a negative integer value),
!       the list length must equal the bundle member count.
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
        routehandle, factorList, factorIndexList, keywordEnforcer, &
        srcTermProcessing, rc)

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I4),  intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,             intent(inout), optional  :: srcTermProcessing(:)
        integer,             intent(out),   optional  :: rc

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
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
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayBundleSMMStore(srcab, dstab, routehandle, factorList, &
            factorIndexList, srcTermProcessing=srcTermProcessing, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
            
        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
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
      routehandle, factorList, factorIndexList, keywordEnforcer, &
      srcTermProcessing, rc)

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        integer(ESMF_KIND_I8),  intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,             intent(inout), optional  :: srcTermProcessing(:)
        integer,             intent(out),   optional  :: rc

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
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
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayBundleSMMStore(srcab, dstab, routehandle, factorList, &
            factorIndexList, srcTermProcessing=srcTermProcessing, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
            
        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
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
      routehandle, factorList, factorIndexList, keywordEnforcer, &
      srcTermProcessing, rc)

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R4),     intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,             intent(inout), optional  :: srcTermProcessing(:)
        integer,             intent(out),   optional  :: rc

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
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
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayBundleSMMStore(srcab, dstab, routehandle, factorList, &
            factorIndexList, srcTermProcessing=srcTermProcessing, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
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
      routehandle, factorList, factorIndexList, keywordEnforcer, &
      srcTermProcessing, rc)

        ! input arguments 
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle  
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle  
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
        real(ESMF_KIND_R8),     intent(in)            :: factorList(:)
        integer,                intent(in)            :: factorIndexList(:,:) 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,             intent(inout), optional  :: srcTermProcessing(:)
        integer,             intent(out),   optional  :: rc

!EOPI
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
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
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayBundleSMMStore(srcab, dstab, routehandle, factorList, &
            factorIndexList, srcTermProcessing=srcTermProcessing, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleSMMStoreR8

!---------------------------------------------------------------------------- 

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSMMStoreNF"
!BOP
! !IROUTINE: ESMF_FieldBundleSMMStore - Precompute a FieldBundle sparse matrix multiplication
!
! !INTERFACE:
  ! Private name; call using ESMF_FieldBundleSMMStore()
    subroutine ESMF_FieldBundleSMMStoreNF(srcFieldBundle, dstFieldBundle, &
        routehandle, keywordEnforcer, srcTermProcessing, rc)

! !ARGUMENTS:
        type(ESMF_FieldBundle), intent(in)            :: srcFieldBundle
        type(ESMF_FieldBundle), intent(inout)         :: dstFieldBundle
        type(ESMF_RouteHandle), intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
        integer,             intent(inout), optional  :: srcTermProcessing(:)
        integer,             intent(out), optional    :: rc

! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.1.0r] Added argument {\tt srcTermProcessing}.
!              The new argument gives the user access to the tuning parameter
!              affecting the sparse matrix execution and bit-wise
!              reproducibility.
! \end{description}
! \end{itemize}
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
!   The routine returns an {\tt ESMF\_RouteHandle} that can be used to call 
!   {\tt ESMF\_FieldBundleSMM()} on any pair of FieldBundles that matches 
!   {\tt srcFieldBundle} and {\tt dstFieldBundle} in {\em type}, {\em kind},
!   and memory layout of the {\em gridded} dimensions. However, the size, 
!   number, and index order of {\em ungridded} dimensions may be different.
!   See section \ref{RH:Reusability} for a more detailed discussion of
!   RouteHandle reusability.
!  
! \begin{sloppypar}
! This method is overloaded for
! {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8}, 
! {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
! \end{sloppypar}
!
! This call is collective across the current VM.  
! 
! For examples and associated documentation regarding this method see Section
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
! \item [{[srcTermProcessing]}]
!       Source term summing options for route handle creation. See
!       {\tt ESMF\_FieldRegridStore} documentation for a full parameter description.
!       Two forms may be provided. If a single element list is provided, this
!       integer value is applied across all bundle members. Otherwise, the list must
!       contain as many elements as there are bundle members. For the special case
!       of accessing the auto-tuned parameter (providing a negative integer value),
!       the list length must equal the bundle member count.
! \item [{[rc]}]  
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP
        ! local variables as temporary input/output arguments 

        ! internal local variables 
        integer                                       :: localrc, sfcount, dfcount, i 
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
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
               msg="src and dst FieldBundle must have same number of fields", &
                ESMF_CONTEXT, rcToReturn=rc)
            return
        endif 

        ! TODO:
        ! internal grids match
        !if(ESMF_GridMatch(srcFieldBundle%btypep%grid, dstFieldBundle%btypep%grid) then
        !    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
        !       "src and dst FieldBundle must have matching grid", &
        !        ESMF_CONTEXT, rcToReturn=rc)
        !    return
        !endif 

        srcab = ESMF_FieldBundleToAB(srcFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstab = ESMF_FieldBundleToAB(dstFieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayBundleSMMStore(srcab, dstab, routehandle, &
            srcTermProcessing=srcTermProcessing, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! garbage collection
        call ESMF_ArrayBundleDestroy(srcab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayBundleDestroy(dstab, noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! return successfully
        if (present(rc)) rc = ESMF_SUCCESS
        
    end subroutine ESMF_FieldBundleSMMStoreNF
! ---------------------------------------------------------------------------- 

! ----------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSMMStoreFromFile"

!BOP
! !IROUTINE: ESMF_FieldBundleSMMStore - Precompute field bundle sparse matrix multiplication using factors read from file
!
! !INTERFACE:
! ! Private name; call using ESMF_FieldBundleSMMStore()
    subroutine ESMF_FieldBundleSMMStoreFromFile(srcFieldBundle, dstFieldBundle, &
      filename, routehandle, keywordEnforcer, srcTermProcessing, rc)

! ! ARGUMENTS:
      type(ESMF_FieldBundle), intent(in)              :: srcFieldBundle
      type(ESMF_FieldBundle), intent(inout)           :: dstFieldBundle
      character(len=*),       intent(in)              :: filename
      type(ESMF_RouteHandle), intent(inout)           :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,             intent(inout), optional    :: srcTermProcessing(:)
      integer,             intent(out),   optional    :: rc

!
! !DESCRIPTION:
!
! Compute an {\tt ESMF\_RouteHandle} using factors read from file.
!
! The arguments are:
!
! \begin{description}
!
! \item [srcFieldBundle]
!       {\tt ESMF\_FieldBundle} with source data.
!
! \item [dstFieldBundle]
!       {\tt ESMF\_FieldBundle} with destination data. The data in this field
!       bundle may be destroyed by this call.
!
! \item [filename]
!       Path to the file containing weights for creating an {\tt ESMF\_RouteHandle}.
!       See ~(\ref{sec:weightfileformat}) for a description of the SCRIP weight
!       file format. Only "row", "col", and "S" variables are required. They
!       must be one-dimensionsal with dimension "n\_s".
!
! \item [routehandle]
!       Handle to the {\tt ESMF\_RouteHandle}.
!
! \item [{[srcTermProcessing]}]
!       Source term summing options for route handle creation. See
!       {\tt ESMF\_FieldRegridStore} documentation for a full parameter description.
!       Two forms may be provided. If a single element list is provided, this
!       integer value is applied across all bundle members. Otherwise, the list must
!       contain as many elements as there are bundle members. For the special case
!       of accessing the auto-tuned parameter (providing a negative integer value),
!       the list length must equal the bundle member count.
!
! \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
! ----------------------------------------------------------------------------------

      ! LOCAL VARIABLES:
      real(ESMF_KIND_R8), dimension(:), allocatable :: factorList
      integer, dimension(:, :), allocatable :: factorIndexList
      integer :: localrc

      ! Initialize return code; assume routine not implemented
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Fill the factorList and factorIndexList.
      call ESMF_FactorRead(filename, &
                           factorList, &
                           factorIndexList, &
                           rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Generate routeHandle from factorList and factorIndexList
      call ESMF_FieldBundleSMMStore(srcFieldBundle, dstFieldBundle, routehandle, &
        factorList, factorIndexList, srcTermProcessing=srcTermProcessing, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      deallocate(factorList)
      deallocate(factorIndexList)

      if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldBundleSMMStoreFromFile

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleValidate()"
!BOP
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
!EOP
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
  subroutine ESMF_FieldBundleWrite(fieldbundle, fileName, keywordEnforcer,  &
      convention, purpose, singleFile, overwrite, status, timeslice, iofmt, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle),     intent(in)             :: fieldbundle
    character(*),               intent(in)             :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords for the below
    character(*),               intent(in),  optional  :: convention
    character(*),               intent(in),  optional  :: purpose
    logical,                    intent(in),  optional  :: singleFile
    logical  ,                  intent(in),  optional  :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional  :: status
    integer,                    intent(in),  optional  :: timeslice
    type(ESMF_IOFmt_Flag),      intent(in),  optional  :: iofmt
    integer,                    intent(out), optional  :: rc  
!
! !DESCRIPTION:
!   Write the Fields into a file. For this API to be functional,
!   the environment variable {\tt ESMF\_PIO} should be set to "internal"
!   when the ESMF library is built. Please see the section on 
!   Data I/O,~\ref{io:dataio}.
!
!   When {\tt convention} and {\tt purpose} arguments are specified, NetCDF dimension
!   labels and variable attributes are written from each Field in the FieldBundle
!   from the corresponding Attribute package. Additionally, Attributes may be
!   set on the FieldBundle level under the same Attribute package.  This allows
!   the specification of global attributes within the file.
!   As with individual Fields, the value associated with each name may be either
!   a scalar character string, or a scalar or array of type integer, real, or
!   double precision.
!
!   Limitations:
!   \begin{itemize}
!     \item Only single tile Fields are supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[fieldbundle] 
!     An {\tt ESMF\_FieldBundle} object.
!   \item[fileName]
!     The name of the output file to which field bundle data is written.
!   \item[{[convention]}]
!     Specifies an Attribute package associated with the FieldBundle, and the
!     contained Fields, used to create NetCDF dimension labels and attributes
!     in the file.  When this argument is present, the {\tt purpose} 
!     argument must also be present.  Use this argument only with a NetCDF
!     I/O format. If binary format is used, ESMF will return an error code.
!   \item[{[purpose]}]
!     Specifies an Attribute package associated with the FieldBundle, and the
!     contained Fields, used to create NetCDF dimension labels and attributes
!     in the file.  When this argument is present, the {\tt convention} 
!     argument must also be present.  Use this argument only with a NetCDF
!     I/O format. If binary format is used, ESMF will return an error code.
!   \item[{[singleFile]}]
!     A logical flag, the default is .true., i.e., all fields in the bundle 
!     are written in one single file. If .false., each field will be written
!     in separate files; these files are numbered with the name based on the
!     argument "file". That is, a set of files are named: [file\_name]001,
!     [file\_name]002, [file\_name]003,...
!   \item[{[overwrite]}]
!    \begin{sloppypar}
!      A logical flag, the default is .false., i.e., existing field data may
!      {\em not} be overwritten.  If .true., the overwrite behavior depends
!      on the value of {\tt iofmt} as shown below:
!    \begin{description}
!    \item[{\tt iofmt} = {\tt ESMF\_IOFMT\_BIN}:]\ All data in the file will
!      be overwritten with each field's data.
!    \item[{\tt iofmt} = {\tt ESMF\_IOFMT\_NETCDF, ESMF\_IOFMT\_NETCDF\_64BIT\_OFFSET}:]\ Only the
!      data corresponding to each field's name will be
!      be overwritten. If the {\tt timeslice} option is given, only data for
!      the given timeslice may be overwritten.
!      Note that it is always an error to attempt to overwrite a NetCDF
!      variable with data which has a different shape.
!    \end{description}
!    \end{sloppypar}
!   \item[{[status]}]
!    \begin{sloppypar}
!    The file status. Please see Section~\ref{const:filestatusflag} for
!    the list of options. If not present, defaults to
!    {\tt ESMF\_FILESTATUS\_UNKNOWN}.
!    \end{sloppypar}
!   \item[{[timeslice]}]
!    \begin{sloppypar}
!    Some I/O formats (e.g. NetCDF) support the output of data in form of
!    time slices. The {\tt timeslice} argument provides access to this
!    capability. {\tt timeslice} must be positive. The behavior of this
!    option may depend on the setting of the {\tt overwrite} flag:
!    \begin{description}
!    \item[{\tt overwrite = .false.}:]\ If the timeslice value is
!    less than the maximum time already in the file, the write will fail.
!    \item[{\tt overwrite = .true.}:]\ Any positive timeslice value is valid.
!    \end{description}
!    By default, i.e. by omitting the {\tt timeslice} argument, no
!    provisions for time slicing are made in the output file,
!    however, if the file already contains a time axis for the variable,
!    a timeslice one greater than the maximum will be written.
!    \end{sloppypar}
!   \item[{[iofmt]}]
!     \begin{sloppypar}
!    The I/O format.  Please see Section~\ref{opt:iofmtflag} for the list
!    of options. If not present, file names with a {\tt .bin} extension will
!    use {\tt ESMF\_IOFMT\_BIN}, and file names with a {\tt .nc} extension
!    will use {\tt ESMF\_IOFMT\_NETCDF}.  Other files default to
!    {\tt ESMF\_IOFMT\_NETCDF}.
!     \end{sloppypar}
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                         :: localrc           ! local return code
    character(len=ESMF_MAXSTR)      :: name
    integer                         :: fieldCount
    integer                         :: i
    type(ESMF_Field), allocatable   :: fieldList(:)
    logical                         :: singlef
    character(len=3)                :: cnum
    character(len=len (fileName) + 3) :: filename_num    ! len (file) + len (cnum)
    type(ESMF_Array)                :: array
    type(ESMF_FieldType), pointer   :: fp 
    type(ESMF_Grid)                 :: grid
    logical                         :: opt_overwriteflag ! helper variable
    type(ESMF_FileStatus_Flag)      :: opt_status        ! helper variable
    type(ESMF_IOFmt_Flag)           :: opt_iofmt
    type(ESMF_IO)                   :: io                ! The I/O object
    logical                         :: errorFound        ! True if err. cond.
    integer                         :: file_ext_p

#ifdef ESMF_PIO
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    ! Check options
    singlef = .true.
    if (present(singleFile)) singlef = singleFile

    opt_overwriteflag = .false.
    if (present(overwrite)) opt_overwriteflag = overwrite

    opt_status = ESMF_FILESTATUS_UNKNOWN
    if (present(status)) opt_status = status

    ! Set iofmt based on file name extension (if present)
    if (present (iofmt)) then
      opt_iofmt = iofmt
    else
      if (index (fileName, '.') > 0) then
        file_ext_p = index (fileName, '.', back=.true.)
        select case (fileName(file_ext_p:))
        case ('.nc')
          opt_iofmt = ESMF_IOFMT_NETCDF
        case ('.bin')
          opt_iofmt = ESMF_IOFMT_BIN
        case default
          opt_iofmt = ESMF_IOFMT_NETCDF
        end select
      else
        opt_iofmt = ESMF_IOFMT_NETCDF
      end if
    end if

    if (present (convention) .neqv. present (purpose)) then
      if (ESMF_LogFoundError (ESMF_RC_ARG_WRONG,  &
          msg='Both convention and purpose must be specified',  &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
         ESMF_CONTEXT, rcToReturn=rc)) return

    allocate (fieldList(fieldCount))
    call ESMF_FieldBundleGet(fieldbundle, fieldList=fieldList, &
!TODO: gjt thinks this should be doine in ADDORDER below. However, currently
!TODO: this causes an ESMF_FieldBundleIOUTest failure. Needs to be fixed.
!      itemorderflag=ESMF_ITEMORDER_ADDORDER, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Create an I/O object
#if 0
call ESMF_LogWrite("Bef IOCreate", ESMF_LOGMSG_INFO, rc=rc)
#endif
    io = ESMF_IOCreate(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
        ESMF_CONTEXT, rcToReturn=rc)) return
#if 0
call ESMF_LogWrite("Aft IOCreate", ESMF_LOGMSG_INFO, rc=rc)
#endif

    ! From here on out, we need to clean up so no returning on error
    if (singlef) then
      ! Get and read the fields in the Bundle
      do i=1,fieldCount
        call ESMF_FieldGet(fieldList(i), array=array, name=name, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
            ESMF_CONTEXT, rcToReturn=rc)) return

        fp => fieldList(i)%ftypep
        if (present (convention)) then
          call ESMF_FieldGet (fieldList(i), grid=grid, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end if

        call c_esmc_fieldioaddarray (io, fp%base, array, grid, name,  &
            fieldbundle%this%base, convention, purpose,  &
            localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
            ESMF_CONTEXT, rcToReturn=rc)) return
      enddo

#if 0
call ESMF_LogWrite("Bef ESMF_IOWrite", ESMF_LOGMSG_INFO, rc=rc)
#endif
      call ESMF_IOWrite(io, trim(fileName), overwrite=opt_overwriteflag,    &
          status=opt_status, timeslice=timeslice, iofmt=opt_iofmt, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
          ESMF_CONTEXT, rcToReturn=rc)) return
#if 0
call ESMF_LogWrite("Aft ESMF_IOWrite", ESMF_LOGMSG_INFO, rc=rc)
#endif

    else
      do i=1,fieldCount
        ! Clear the IO object (only need to do this for i > 1)
        if (i .gt. 1) call ESMF_IOClear(io)
        write(cnum,"(i3.3)") i
        filename_num = trim (fileName) // cnum
        call ESMF_FieldGet(fieldList(i), array=array, name=name, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
            ESMF_CONTEXT, rcToReturn=rc)) return

        fp => fieldList(i)%ftypep
        if (present (convention)) then
          call ESMF_FieldGet (fieldList(i), grid=grid, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end if

        call c_esmc_fieldioaddarray (io, fp%base, array, grid, name,  &
            fieldbundle%this%base, convention, purpose,  &
            localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_IOWrite(io, trim(filename_num),                      &
             overwrite=opt_overwriteflag, status=opt_status,           &
             timeslice=timeslice, iofmt=opt_iofmt, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                  &
            ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    endif

    ! Set rc here in case we had an error but destroy succeeds
    if (present(rc)) rc = localrc

    call ESMF_IODestroy(io, rc=localrc)
    ! Log error but don't reset rc
    errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,         &
        ESMF_CONTEXT, rcToReturn=localrc)

    ! Last chance to return an error code (IODestroy failed)
    if (present(rc)) then
      if (rc == ESMF_SUCCESS) rc = localrc
    end if

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
        itemList=l_fieldList, itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      
      call ESMF_BaseSerialize (bp%base, buffer, offset,  &
          lattreconflag, linquireflag,  &
          rc=localrc)
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

      deallocate(l_fieldList)

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
!           Flag to tell if Attribute deserialization is to be done
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
      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Field), pointer :: flist(:)
      type(ESMF_Logical) :: linkChange

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
      bp%base = ESMF_BaseDeserialize (buffer, offset=offset,  &
          attreconflag=lattreconflag, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_BaseSetInitCreated(bp%base, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Deserialize other FieldBundle members
      
      call c_ESMC_FieldBundleDeserialize(bp%status, fieldCount, &
                                 buffer, offset, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if(bp%status == ESMF_FBSTATUS_GRIDSET) then
        bp%geombase = ESMF_GeomBaseDeserialize(buffer, offset, &
          attreconflag=attreconflag, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        !  link the Attribute hierarchies
        call ESMF_GeomBaseGet(bp%geombase, geomtype=geomtype, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        if((geomtype == ESMF_GEOMTYPE_GRID) .or. &
           (geomtype == ESMF_GEOMTYPE_LOCSTREAM)) then
          call ESMF_GeomBaseGet(bp%geombase, grid=grid, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          linkChange = ESMF_TRUE
          call c_ESMC_AttributeLink(bp%base, grid, linkChange, status)
          if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                        ESMF_CONTEXT, rcToReturn=rc))  return
        endif
      endif

      ! TODO: decide if these need to be sent before or after
      allocate(flist(fieldCount), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, &
        msg = " - Field list", &
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
      enddo

      bp%container = ESMF_ContainerCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      bp%is_proxy = .true.

      ESMF_FieldBundleDeserialize%this => bp

      ! Add reference to this object into ESMF garbage collection table
      ! Only call this in those Create() methods that call Construct()
      call c_ESMC_VMAddFObject(ESMF_FieldBundleDeserialize, &
        ESMF_ID_FIELDBUNDLE%objectID)

      ! Set as created
      ESMF_INIT_SET_CREATED(ESMF_FieldBundleDeserialize)

      call ESMF_FieldBundleAdd(ESMF_FieldBundleDeserialize, flist, multiflag=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      deallocate(flist)

      if (present(rc)) rc = ESMF_SUCCESS

    end function ESMF_FieldBundleDeserialize

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetGeom()"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetGeom - Set a GeomBase in FieldBundle
!
! !INTERFACE:
  subroutine ESMF_FieldBundleSetGeom(fieldbundle, field, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)             :: fieldbundle
    type(ESMF_Field), intent(in)                      :: field
    integer, intent(out), optional                    :: rc 
!
! !DESCRIPTION:
!      Set a geombase in FieldBundle, if the geombase is a Grid, attribute
!     linking is done inside FieldBundleSetGrid.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           fieldbundle object.
!     \item [field]
!           field object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer                                           :: localrc
    type(ESMF_GeomType_Flag)                               :: geomtype
    type(ESMF_Grid)                                   :: grid
    type(ESMF_XGrid)                                  :: xgrid
    type(ESMF_Mesh)                                   :: mesh
    type(ESMF_LocStream)                              :: locstream
    type(ESMF_FieldStatus_Flag)                            :: fstatus

    localrc = ESMF_RC_NOT_IMPL
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check input arguments
    ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field, rc)

    ! Don't do anything if fieldbundle already has a geombase
    ! TODO: we'll check matchness here in the future between field%geombase
    ! and fieldbundle%geombase
    if(fieldbundle%this%status == ESMF_FBSTATUS_GRIDSET) then
      if(present(rc)) rc = ESMF_SUCCESS
      return
    endif

    call ESMF_FieldGet(field, status=fstatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc))  return

    if(fstatus == ESMF_FIELDSTATUS_GRIDSET .or. &
       fstatus == ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc))  return
      if(geomtype == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(field, grid=grid, rc=localrc)  
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc))  return
        ! this call takes care of attribute linking of Grid
        call ESMF_FieldBundleSet(fieldbundle, grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc))  return
      else if(geomtype == ESMF_GEOMTYPE_XGRID) then
        call ESMF_FieldGet(field, xgrid=xgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc))  return
        call ESMF_FieldBundleSet(fieldbundle, xgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc))  return
      else if(geomtype == ESMF_GEOMTYPE_MESH) then
        call ESMF_FieldGet(field, mesh=mesh, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc))  return
        call ESMF_FieldBundleSet(fieldbundle, mesh, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc))  return
      else if(geomtype == ESMF_GEOMTYPE_LOCSTREAM) then
        call ESMF_FieldGet(field, locstream=locstream, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc))  return
        call ESMF_FieldBundleSet(fieldbundle, locstream, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc))  return
      endif
      fieldbundle%this%status = ESMF_FBSTATUS_GRIDSET
    endif

    if(present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldBundleSetGeom

! -----------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRemoveGeom()"
!BOPI
! !IROUTINE: ESMF_FieldBundleRemoveGeom - Remove the GeomBase in FieldBundle
!
! !INTERFACE:
  subroutine ESMF_FieldBundleRemoveGeom(fieldbundle, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(inout)             :: fieldbundle
    integer, intent(out), optional                    :: rc 
!
! !DESCRIPTION:
!      Remove the geombase in FieldBundle, if the geombase is a Grid, attribute
!     linking is removed as well. Called when the fieldbundle is emptied.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           fieldbundle object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer                                           :: localrc
    type(ESMF_GeomType_Flag)                               :: geomtype
    type(ESMF_Grid)                                   :: grid
    type(ESMF_Logical)                                :: linkChange

    localrc = ESMF_RC_NOT_IMPL
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check input arguments
    ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,fieldbundle,rc)

    ! should never call this method when it's not GRIDSET
    if(fieldbundle%this%status /= ESMF_FBSTATUS_GRIDSET) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
        msg = " - cannot remove a gemobase from a fieldbundle that is empty", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    call ESMF_GeomBaseGet(fieldbundle%this%geombase, geomtype=geomtype, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc))  return
    if(geomtype == ESMF_GEOMTYPE_GRID) then
      call ESMF_GeombaseGet(fieldbundle%this%geombase, grid=grid, rc=localrc)  
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc))  return

      linkChange = ESMF_TRUE
      call c_ESMC_AttributeLinkRemove(fieldbundle%this%base, grid, linkChange, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc))  return
    endif
    call ESMF_GeomBaseDestroy(fieldbundle%this%geombase, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc))  return

    fieldbundle%this%status = ESMF_FBSTATUS_EMPTY

    if(present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldBundleRemoveGeom
! -----------------------------------------------------------------------------

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleToAB()"
!BOPI
! !IROUTINE: ESMF_FieldBundleToAB - Create an ArrayBundle from a FieldBundle
!
! !INTERFACE:
  function ESMF_FieldBundleToAB(fieldbundle, rc)

!
! !RETURN VALUE:
    type(ESMF_ArrayBundle)                    :: ESMF_FieldBundleToAB

!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)        :: fieldbundle
    integer, intent(out), optional            :: rc
!
! !DESCRIPTION:
!      Create an ArrayBundle from a FieldBundle.
!
!     The arguments are:
!     \begin{description}
!     \item [fieldbundle]
!           fieldbundle object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer                                   :: fcount, i, localrc
    type(ESMF_Field), pointer                 :: flist(:)
    type(ESMF_Array), pointer                 :: alist(:)
    character(800)                            :: name, msgString

    localrc = ESMF_RC_NOT_IMPL
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_FieldBundleGetInit, fieldbundle, rc)

    call ESMF_FieldBundleGet(fieldbundle, fieldCount=fcount, rc=localrc) 
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc))  return

    allocate(flist(fcount), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg= "allocating flist", &
      ESMF_CONTEXT, rcToReturn=rc)) return ! bail out

    call ESMF_FieldBundleGet(fieldbundle, fieldList=flist, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    allocate(alist(fcount), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg= "allocating alist", &
      ESMF_CONTEXT, rcToReturn=rc)) return ! bail out

    do i = 1, fcount
        call ESMF_FieldGet(flist(i), array=alist(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
#if 0
        call ESMF_ArrayGet(alist(i), name=name, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        write(msgString,*) "alist(",i,") name:", trim(name)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
#endif
    enddo

    ESMF_FieldBundleToAB = ESMF_ArrayBundleCreate(arrayList=alist, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    deallocate(alist, flist)

    if(present(rc)) rc = ESMF_SUCCESS

  end function ESMF_FieldBundleToAB
! -----------------------------------------------------------------------------

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
