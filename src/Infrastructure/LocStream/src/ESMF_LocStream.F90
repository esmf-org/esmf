! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_LocStream.F90"
!==============================================================================
!
!     ESMF LocStream module
module ESMF_LocStreamMod
!
!==============================================================================
!
! This file contains the LocStream class definition and all LocStream
! class method.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_LocStreamMod - Combine physical field metadata, data and grid
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_LocStream} class, which 
! represents a single scalar or vector field.  {\tt ESMF\_LocStream}s associate
! a metadata description expressed as a set of {\tt ESMF\_Attributes} with
! a data {\tt ESMF\_Array} and an {\tt ESMF\_Grid}.
! 
! This type is implemented in Fortran 90.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_UtilMod
  use ESMF_BaseMod
  use ESMF_VMMod
  use ESMF_LogErrMod
  use ESMF_IOUtilMod

  use ESMF_ArraySpecMod
  use ESMF_LocalArrayMod
  use ESMF_DELayoutMod
  use ESMF_DistGridMod
  use ESMF_GridMod
  use ESMF_GridUtilMod
  use ESMF_RHandleMod
  use ESMF_StaggerLocMod
  use ESMF_ArrayMod
  use ESMF_ArrayBundleMod
  use ESMF_ArrayCreateMod
  use ESMF_ArrayGetMod
  use ESMF_InitMacrosMod
  use ESMF_MeshMod
  use ESMF_IOScripMod
  use ESMF_IOUGridMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_LocStreamType
! ! Definition of the LocStream class.


  type ESMF_LocStreamType
#ifndef ESMF_NO_SEQUENCE
     sequence
#endif

    !private
     type (ESMF_Base)                     :: base             ! base class object
     logical                              :: destroyDistgrid 
     type (ESMF_DistGrid)                 :: distgrid         ! description of index space of Arrays
     type(ESMF_Index_Flag)                :: indexflag
     type(ESMF_CoordSys_Flag)             :: coordSys
     integer                              :: keyCount         ! Number of keys
     character(len=ESMF_MAXSTR), pointer  :: keyNames(:)      ! Names
     character(len=ESMF_MAXSTR), pointer  :: keyUnits(:)      ! Units
     character(len=ESMF_MAXSTR), pointer  :: keyLongNames(:)  ! Long names
     logical, pointer                     :: destroyKeys(:)   ! if we're responsible for destroying key array
     type (ESMF_Array), pointer           :: keys(:)          ! Contents

     ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! ! ESMF_LocStream    
! ! The LocStream data structure that is passed between implementation and
! ! calling languages.

  type ESMF_LocStream
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    !private       
    type (ESMF_LocStreamType), pointer :: lstypep
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_LocStream
  public ESMF_LocStreamType ! For internal use only

   public operator(==)
   public operator(/=)

   public ESMF_LocStreamIsCreated
   public ESMF_LocStreamValidate           ! Check internal consistency
   public ESMF_LocStreamCreate
   public ESMF_LocStreamGet
   public ESMF_LocStreamGetBounds
   public ESMF_LocStreamDeserialize
   public ESMF_LocStreamSerialize
   public ESMF_LocStreamDestroy
   public ESMF_LocStreamDestruct           ! for ESMF garbage collection
   public ESMF_LocStreamPrint              ! Print contents of a LocStream
   public ESMF_LocStreamGetKey
   public ESMF_LocStreamAddKey
   public ESMF_LocStreamMatch



! - ESMF-internal methods:
   public ESMF_LocStreamTypeGetInit        ! For Standardized Initialization
   public ESMF_LocStreamTypeInit           ! For Standardized Initialization
   public ESMF_LocStreamTypeValidate
   public ESMF_LocStreamGetInit            ! For Standardized Initialization

!EOPI

! !PRIVATE MEMBER FUNCTIONS:
   
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
! !IROUTINE: ESMF_LocStreamCreate -- Generic interface

! !INTERFACE:
interface ESMF_LocStreamCreate

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_LocStreamCreateFromDG
      module procedure ESMF_LocStreamCreateFromLocal
      module procedure ESMF_LocStreamCreateFromNewDG
      module procedure ESMF_LocStreamCreateReg
      module procedure ESMF_LocStreamCreateIrreg
      module procedure ESMF_LocStreamCreateByBkgMesh
      module procedure ESMF_LocStreamCreateByBkgGrid
      module procedure ESMF_LocStreamCreateFromFile

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_LocStreamCreate} functions.   
!EOPI 
end interface


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_LocStreamGetKey -- Generic interface

! !INTERFACE:
interface ESMF_LocStreamGetKey

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_LocStreamGetKeyI4
      module procedure ESMF_LocStreamGetKeyR4
      module procedure ESMF_LocStreamGetKeyR8
      module procedure ESMF_LocStreamGetKeyArray  
      module procedure ESMF_LocStreamGetKeyInfo
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_LocStreamGetKey} functions.   
!EOPI 
end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_LocStreamAddKey -- Generic interface

! !INTERFACE:
interface ESMF_LocStreamAddKey

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_LocStreamAddKeyAlloc
      module procedure ESMF_LocStreamAddKeyArray
      module procedure ESMF_LocStreamAddKeyI4
      module procedure ESMF_LocStreamAddKeyR4
      module procedure ESMF_LocStreamAddKeyR8

      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_LocStreamAddKey} functions.   
!EOPI 
end interface

!===============================================================================
! LocStreamOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_LocStreamAssignment(=) - LocStream assignment
!
! !INTERFACE:
!   interface assignment(=)
!   locstream1 = locstream2
!
! !ARGUMENTS:
!   type(ESMF_LocStream) :: locstream1
!   type(ESMF_LocStream) :: locstream2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Assign locstream1 as an alias to the same ESMF LocStream object in memory
!   as locstream2. If locstream2 is invalid, then locstream1 will be equally invalid after
!   the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[locstream1]
!     The {\tt ESMF\_LocStream} object on the left hand side of the assignment.
!   \item[locstream2]
!     The {\tt ESMF\_LocStream} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_LocStreamOperator(==) - LocStream equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (locstream1 == locstream2) then ... endif
!             OR
!   result = (locstream1 == locstream2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_LocStream), intent(in) :: locstream1
!   type(ESMF_LocStream), intent(in) :: locstream2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether locstream1 and locstream2 are valid aliases to the same ESMF
!   LocStream object in memory. For a more general comparison of two ESMF LocStreams,
!   going beyond the simple alias test, the ESMF\_LocStreamMatch() function (not yet
!   implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[locstream1]
!     The {\tt ESMF\_LocStream} object on the left hand side of the equality
!     operation.
!   \item[locstream2]
!     The {\tt ESMF\_LocStream} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_LocStreamEQ

  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_LocStreamOperator(/=) - LocStream not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (locstream1 /= locstream2) then ... endif
!             OR
!   result = (locstream1 /= locstream2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_LocStream), intent(in) :: locstream1
!   type(ESMF_LocStream), intent(in) :: locstream2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether locstream1 and locstream2 are {\it not} valid aliases to the
!   same ESMF LocStream object in memory. For a more general comparison of two ESMF
!   LocStreams, going beyond the simple alias test, the ESMF\_LocStreamMatch() function
!   (not yet implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[locstream1]
!     The {\tt ESMF\_LocStream} object on the left hand side of the non-equality
!     operation.
!   \item[locstream2]
!     The {\tt ESMF\_LocStream} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_LocStreamNE

  end interface
!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamEQ()"
!BOPI
! !IROUTINE:  ESMF_LocStreamEQ - Compare two LocStreams for equality
!
! !INTERFACE:
  function ESMF_LocStreamEQ(locstream1, locstream2)
! 
! !RETURN VALUE:
    logical :: ESMF_LocStreamEQ

! !ARGUMENTS:
    type(ESMF_LocStream), intent(in) :: locstream1
    type(ESMF_LocStream), intent(in) :: locstream2

! !DESCRIPTION:
!   Test if both {\tt locstream1} and {\tt locstream2} alias the same ESMF LocStream 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE lsinit1, lsinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: use Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    lsinit1 = ESMF_LocStreamGetInit(locstream1)
    lsinit2 = ESMF_LocStreamGetInit(locstream2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (lsinit1 .eq. ESMF_INIT_CREATED .and. &
      lsinit2 .eq. ESMF_INIT_CREATED) then
      ESMF_LocStreamEQ = associated(locstream1%lstypep,locstream2%lstypep)
    else
      ESMF_LocStreamEQ = ESMF_FALSE
    endif

  end function ESMF_LocStreamEQ
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamNE()"
!BOPI
! !IROUTINE:  ESMF_LocStreamNE - Compare two LocStreams for non-equality
!
! !INTERFACE:
  function ESMF_LocStreamNE(locstream1, locstream2)
! 
! !RETURN VALUE:
    logical :: ESMF_LocStreamNE

! !ARGUMENTS:
    type(ESMF_LocStream), intent(in) :: locstream1
    type(ESMF_LocStream), intent(in) :: locstream2

! !DESCRIPTION:
!   Test if both {\tt locstream1} and {\tt locstream2} alias the same ESMF LocStream 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE lsinit1, lsinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).
    
    ESMF_LocStreamNE = .not.ESMF_LocStreamEQ(locstream1, locstream2)

  end function ESMF_LocStreamNE
!-------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamAddKeyAlloc"
!BOP
! !IROUTINE: ESMF_LocStreamAddKey - Add a key Array and allocate the internal memory

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
  subroutine ESMF_LocStreamAddKeyAlloc(locstream, keyName, keywordEnforcer, &
               keyTypeKind, keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream),     intent(in)            :: locstream
    character (len=*),        intent(in)            :: keyName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_TypeKind_Flag), intent(in),  optional :: keyTypeKind
    character (len=*),        intent(in),  optional :: keyUnits 
    character (len=*),        intent(in),  optional :: keyLongName 
    integer,                  intent(out), optional :: rc
!
! !DESCRIPTION:
! Add a key to a locstream with a required keyName. Once a key has 
! been added, a pointer to its internally allocated memory can be 
! retrieved and used to set key values. 
!
! The arguments are:
! \begin{description}
! \item [locstream]
! The {\tt ESMF\_LocStream} object to add key to.
! \item [keyName]
! The name of the key to add. 
! \item [{[keyTypeKind]}]
! The type/kind of the key data. 
! If not specified then the type/kind will default to 8 byte reals.  
! \item [{[keyUnits]}]
! The units of the key data. 
! If not specified, then the item remains blank.  
! \item [{[keyLongName]}]
! The long name of the key data. 
! If not specified, then the item remains blank.  
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    type(ESMF_LocStreamType), pointer :: lstypep
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: array
    type(ESMF_TypeKind_Flag) :: localKeyTypeKind 
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

    ! set defaults
    if (present(keyTypeKind)) then
        localKeyTypeKind=keyTypeKind     
    else
       localKeyTypeKind=ESMF_TYPEKIND_R8
    endif
    
    ! get the pointer to the locstream
    lstypep => locstream%lstypep

    ! Set ArraySpec
    call ESMF_ArraySpecSet(arrayspec, rank=1, typekind=localKeyTypeKind, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Create Array
    array=ESMF_ArrayCreate(lstypep%distgrid, arrayspec, &
                           indexflag=lstypep%indexflag, name=keyName, &
                           rc=localrc)
   if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

   ! Add key to structure
   call ESMF_LocStreamAddKeyArray(locstream, keyName, keyArray=array, destroyKey=.true., &
               keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamAddKeyAlloc
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamAddKeyArray"
!BOP
! !IROUTINE: ESMF_LocStreamAddKey - Add a key Array 

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
  subroutine ESMF_LocStreamAddKeyArray(locstream, keyName, keyArray, &
               keywordEnforcer, destroyKey, keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)             :: locstream
    character (len=*),    intent(in)             :: keyName
    type(ESMF_Array),     intent(in)             :: keyArray
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,              intent(in),  optional  :: destroyKey
    character (len=*),    intent(in),  optional  :: keyUnits 
    character (len=*),    intent(in),  optional  :: keyLongName 
    integer,              intent(out), optional  :: rc
!
! !DESCRIPTION:
! Add a key to a locstream with a required keyName and a required 
! {\tt ESMF\_Array}.  The user is responsible for the creation of the 
! {\tt ESMF\_Array} that will hold the key values.
!
! The arguments are:
! \begin{description}
! \item [locstream]
! The {\tt ESMF\_LocStream} object to add key to.
! \item [keyName]
! The name of the key to add. 
! \item [keyArray]
! An ESMF Array which contains the key data
! \item [{[destroyKey]}]
! if .true. destroy this key array when the locstream is destroyed.
! Defaults to .false.
! \item [{[keyUnits]}]
! The units of the key data. 
! If not specified, then the item remains blank.  
! \item [{[keyLongName]}]
! The long name of the key data. 
! If not specified, then the item remains blank.  
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    type(ESMF_LocStreamType), pointer :: lstypep
    integer :: i,keyIndex
    integer :: localrc
    logical :: localDestroyKey
    character(len=ESMF_MAXSTR), pointer  :: tmpKeyNames(:) 
    character(len=ESMF_MAXSTR), pointer  :: tmpKeyUnits(:) 
    character(len=ESMF_MAXSTR), pointer  :: tmpKeyLongNames(:) 
    logical, pointer                     :: tmpDestroyKeys(:)
    type (ESMF_Array), pointer           :: tmpKeys(:) 
    integer                              :: keyCount


    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_ArrayGetInit,keyArray,rc)

    ! Set default
    if (present(destroyKey)) then
       localDestroyKey=destroyKey
    else
       localDestroyKey=.false.
    endif
    
    ! get the pointer to the locstream
    lstypep => locstream%lstypep

    ! Get keyCount
    keyCount=lstypep%keyCount

    ! Make sure key name doesn't already exist
    keyIndex=0
    do i=1,keyCount
       if (trim(keyName) .eq. trim(lstypep%keyNames(i))) then
          keyIndex=i
          exit 
       endif
    enddo

   ! If something found return error
   if (keyIndex .ne. 0) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_WRONG, &
            msg=" - keyName already exists in this LocStream", &
            ESMF_CONTEXT, rcToReturn=rc)) return
   endif


   ! Make more space 
   ! (Should we eventually make this a linked list???)   
   !! hold old data
   if (keyCount .gt. 0) then
       tmpKeyNames=>lstypep%keyNames
      tmpKeyUnits =>lstypep%keyUnits 
      tmpKeyLongNames=>lstypep%keyLongNames
      tmpDestroyKeys=>lstypep%destroyKeys
      tmpKeys=>lstypep%keys
   endif

   !! Allocate new space for keys (note +1 to increase space for new key)
   allocate (lstypep%keyNames(keyCount+1), stat=localrc )
   if (ESMF_LogFoundAllocError(localrc, msg=" Allocating KeyNames", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
   allocate (lstypep%keyUnits(keyCount+1), stat=localrc )
   if (ESMF_LogFoundAllocError(localrc, msg=" Allocating units", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
   allocate (lstypep%keyLongNames(keyCount+1), stat=localrc )
   if (ESMF_LogFoundAllocError(localrc, msg=" Allocating longNames", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
   allocate( lstypep%keys(keyCount+1), stat=localrc )  ! Array of keys
   if (ESMF_LogFoundAllocError(localrc, msg=" Allocating keys", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
   allocate( lstypep%destroyKeys(keyCount+1), stat=localrc )  ! Array of keys
   if (ESMF_LogFoundAllocError(localrc, msg=" Allocating keys", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

   !! Copy and deallocate old arrays
   if (keyCount .gt. 0) then
      lstypep%keyNames(1:keyCount)=tmpKeyNames(1:keyCount)
      lstypep%keyUnits(1:keyCount)=tmpKeyUnits(1:keyCount)
      lstypep%keyLongNames(1:keyCount)=tmpKeyLongNames(1:keyCount)
      lstypep%destroyKeys(1:keyCount)=tmpDestroyKeys(1:keyCount)
      lstypep%keys(1:keyCount)=tmpKeys(1:keyCount)
     
      deallocate(tmpKeyNames)
      deallocate(tmpKeyUnits)
      deallocate(tmpKeyLongNames)
      deallocate(tmpDestroyKeys)
      deallocate(tmpKeys)
   endif

   ! Put new key info into locstream structure
   lstypep%keyNames(keyCount+1)=keyName
   if (present(keyUnits)) lstypep%keyUnits(keyCount+1)=keyUnits
   if (present(keyLongName)) lstypep%keyLongNames(keyCount+1)=keyLongName
   lstypep%destroyKeys(keyCount+1)=localDestroyKey 
   lstypep%keys(keyCount+1)=keyArray


   ! Increment keyCount to take into account new key
   lstypep%keyCount=keyCount+1

   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamAddKeyArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LocStreamAddKey - Add a key Array created around user memory 

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
!  subroutine ESMF_LocStreamAddKeyI4(locstream, keyName, farray, &
!               keywordEnforcer, datacopyflag, keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
!    type(ESMF_Locstream), intent(in) :: locstream
!    character (len=*), intent(in) :: keyName
!    <farray>
! type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag
!    character (len=*), intent(in), optional :: keyUnits
!    character (len=*), intent(in), optional :: keyLongName
!    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    Add a key to a locstream with a required keyName and a required 
!    Fortran array.  The user is responsible for the creation of the 
!    Fortran array that will hold the key values, including 
!    the maintenance of any allocated memory.
!
!    Supported values for <farray> are:
!    \begin{description}
!    \item integer(ESMF\_KIND\_I4), intent(in) :: farray(:)
!    \item real(ESMF\_KIND\_R4),    intent(in) :: farray(:)
!    \item real(ESMF\_KIND\_R8),    intent(in) :: farray(:)
!    \end{description}
!
!    The arguments are:
!    \begin{description}
!    \item [locstream]
!    The {\tt ESMF\_LocStream} object to add key to.
!    \item [keyName]
!    The name of the key to add. 
!    \item[farray] 
!    Valid native Fortran array, i.e. memory must be associated with the 
!    actual argument. The type/kind/rank information of {\tt farray} will be 
!    used to set the key Array's properties accordingly. 
!    \item[{[datacopyflag]}] 
!    Specifies whether the Array object will reference the memory allocation 
!    provided by {\tt farray} directly or will copy the data from 
!    {\tt farray} into a new memory allocation. Valid options are 
 !    {\tt ESMF\_DATACOPY\_REFERENCE} (default) or {\tt ESMF\_DATACOPY\_VALUE}. 
!    Depending on the specific situation the {\tt ESMF\_DATACOPY\_REFERENCE} option 
!    may be unsafe when specifying an array slice for {\tt farray}. 
!    \item [{[keyUnits]}]
!    The units of the key data. 
!    If not specified, then the item remains blank.  
!    \item [{[keyLongName]}]
!    The long name of the key data. 
!    If not specified, then the item remains blank.  
!    \item [{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!    \end{description}
!EOP
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamAddKeyI4"
!BOPI
! !IROUTINE: ESMF_LocStreamAddKey - Add a key Array created around user memory 

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
  subroutine ESMF_LocStreamAddKeyI4(locstream, keyName, farray, keywordEnforcer, &
       datacopyflag, keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream),                intent(in)            :: locstream
    character (len=*),                   intent(in)            :: keyName
    integer(ESMF_KIND_I4), dimension(:), intent(in)            :: farray
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_DataCopy_Flag),            intent(in),  optional :: datacopyflag
    character (len=*),                   intent(in),  optional :: keyUnits 
    character (len=*),                   intent(in),  optional :: keyLongName 
    integer,                             intent(out), optional :: rc
!
! !DESCRIPTION:
!  Add a key to a locstream with a required keyName and a required 
!  Fortran array.  The user is responsible for the creation of the 
!  Fortran array that will hold the key values, including 
!  the maintenance of any allocated memory.
!
! The arguments are:
! \begin{description}
! \item [locstream]
! The {\tt ESMF\_LocStream} object to add key to.
! \item [keyName]
! The name of the key to add. 
! \item[farray] 
! Valid native Fortran array, i.e. memory must be associated with the 
! actual argument. The type/kind/rank information of {\tt farray} will be 
! used to set the key Array's properties accordingly. 
! \item[{[datacopyflag]}] 
! Specifies whether the Array object will reference the memory allocation 
! provided by {\tt farray} directly or will copy the data from 
! {\tt farray} into a new memory allocation. Valid options are 
! {\tt ESMF\_DATACOPY\_REFERENCE} (default) or {\tt ESMF\_DATACOPY\_VALUE}. 
! Depending on the specific situation the {\tt ESMF\_DATACOPY\_REFERENCE} option 
! may be unsafe when specifying an array slice for {\tt farray}. 
! \item [{[keyUnits]}]
! The units of the key data. 
! If not specified, then the item remains blank.  
! \item [{[keyLongName]}]
! The long name of the key data. 
! If not specified, then the item remains blank.  
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    type(ESMF_LocStreamType), pointer :: lstypep
    type(ESMF_Array) :: array
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

    ! get the pointer to the locstream
    lstypep => locstream%lstypep

   ! Create Array
   array=ESMF_ArrayCreate(lstypep%distgrid, farray, &
                           datacopyflag=datacopyflag, indexflag=lstypep%indexflag,  &
                           name=keyName, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


   ! Add key to structure
   call ESMF_LocStreamAddKeyArray(locstream, keyName, keyArray=array, destroyKey=.true., &
               keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamAddKeyI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamAddKeyR4"
!BOPI
! !IROUTINE: ESMF_LocStreamAddKey - Add a key Array created around user memory

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
  subroutine ESMF_LocStreamAddKeyR4(locstream, keyName, farray, keywordEnforcer, &
               datacopyflag, keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream),              intent(in)           :: locstream
    character (len=*),                 intent(in)           :: keyName
    real(ESMF_KIND_R4),  dimension(:), intent(in)           :: farray
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_DataCopy_Flag),          intent(in), optional :: datacopyflag
    character (len=*),                 intent(in), optional :: keyUnits 
    character (len=*),                 intent(in), optional :: keyLongName 
    integer,                           intent(out), optional:: rc
!
! !DESCRIPTION:
! Add a key to a locstream with a required keyName and a required 
! Fortran array.  The user is responsible for the creation of the 
! Fortran array that will hold the key values, including 
! the maintenance of any allocated memory.
!
! The arguments are:
! \begin{description}
! \item [locstream]
! The {\tt ESMF\_LocStream} object to add key to.
! \item [keyName]
! The name of the key to add. 
! \item[farray] 
! Valid native Fortran array, i.e. memory must be associated with the 
! actual argument. The type/kind/rank information of {\tt farray} will be 
! used to set the key Array's properties accordingly. 
! \item[{[datacopyflag]}] 
! Specifies whether the Array object will reference the memory allocation 
! provided by {\tt farray} directly or will copy the data from 
! {\tt farray} into a new memory allocation. Valid options are 
! {\tt ESMF\_DATACOPY\_REFERENCE} (default) or {\tt ESMF\_DATACOPY\_VALUE}. 
! Depending on the specific situation the {\tt ESMF\_DATACOPY\_REFERENCE} option 
! may be unsafe when specifying an array slice for {\tt farray}. 
! \item [{[keyUnits]}]
! The units of the key data. 
! If not specified, then the item remains blank.  
! \item [{[keyLongName]}]
! The long name of the key data. 
! If not specified, then the item remains blank.  
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    type(ESMF_LocStreamType), pointer :: lstypep
    type(ESMF_Array) :: array
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

    ! get the pointer to the locstream
     lstypep => locstream%lstypep

   ! Create Array
   array=ESMF_ArrayCreate(lstypep%distgrid, farray, &
                           datacopyflag=datacopyflag, indexflag=lstypep%indexflag,  &
                           name=keyName, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


   ! Add key to structure
   call ESMF_LocStreamAddKeyArray(locstream, keyName, keyArray=array, destroyKey=.true., &
               keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamAddKeyR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamAddKeyR8"
!BOPI
! !IROUTINE: ESMF_LocStreamAddKey - Add a key Array created around user memory

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
  subroutine ESMF_LocStreamAddKeyR8(locstream, keyName, farray, keywordEnforcer, &
       datacopyflag, keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)                   :: locstream
    character (len=*),   intent(in)                    :: keyName
    real(ESMF_KIND_R8),  dimension(:), intent(in)      :: farray
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_DataCopy_Flag), intent(in), optional     :: datacopyflag
    character (len=*),    intent(in), optional         :: keyUnits 
    character (len=*),    intent(in), optional         :: keyLongName 
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Add a key to a locstream with a required keyName and a required 
! Fortran array.  The user is responsible for the creation of the 
! Fortran array that will hold the key values, including 
! the maintenance of any allocated memory.
!
! The arguments are:
! \begin{description}
! \item [locstream]
! The {\tt ESMF\_LocStream} object to add key to.
! \item [keyName]
! The name of the key to add. 
! \item[farray] 
! Valid native Fortran array, i.e. memory must be associated with the 
! actual argument. The type/kind/rank information of {\tt farray} will be 
! used to set the key Array's properties accordingly. 
! \item[{[datacopyflag]}] 
! Specifies whether the Array object will reference the memory allocation 
! provided by {\tt farray} directly or will copy the data from 
! {\tt farray} into a new memory allocation. Valid options are 
! {\tt ESMF\_DATACOPY\_REFERENCE} (default) or {\tt ESMF\_DATACOPY\_VALUE}. 
! Depending on the specific situation the {\tt ESMF\_DATACOPY\_REFERENCE} option 
! may be unsafe when specifying an array slice for {\tt farray}. 
! \item [{[keyUnits]}]
! The units of the key data. 
! If not specified, then the item remains blank.  
! \item [{[keyLongName]}]
! The long name of the key data. 
! If not specified, then the item remains blank.  
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI
!------------------------------------------------------------------------------
    type(ESMF_LocStreamType), pointer :: lstypep
    type(ESMF_Array) :: array
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

    ! get the pointer to the locstream
    lstypep => locstream%lstypep

   ! Create Array
   array=ESMF_ArrayCreate(lstypep%distgrid, farray, &
                           datacopyflag=datacopyflag, indexflag=lstypep%indexflag,  &
                           name=keyName, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

   ! Add key to structure
   call ESMF_LocStreamAddKeyArray(locstream, keyName, keyArray=array, destroyKey=.true., &
               keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamAddKeyR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreateByBkgGrid"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream by projecting onto a Grid

! !INTERFACE:
      ! Private name; call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateByBkgGrid(locstream, &
                 background, keywordEnforcer, maskValues, &
                 unmappedaction, name, rc)

!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateByBkgGrid

!
! !ARGUMENTS:
      type(ESMF_LocStream),           intent(in)            :: locstream
      type(ESMF_Grid),                intent(in)            :: background
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),          intent(in),  optional :: maskValues(:)
      type(ESMF_UnmappedAction_Flag), intent(in),  optional :: unmappedaction
      character (len=*),              intent(in),  optional :: name
      integer,                        intent(out), optional :: rc
!
! !DESCRIPTION:
!
!     Create an location stream from an existing one in accordance with 
!     the distribution of the background Grid.  The entries
!     in the new location stream are redistributed, so that they lie on the same PET
!     as the piece of Grid which contains the coordinates of the entries. The coordinates
!     of the entries are the data in the keys named ESMF:Lon, ESMF:Lat, ESMF:Radius in the 
!     case of a spherical system and ESMF:X, ESMF:Y, ESMF:Z for cartesian. To copy data in
!     Fields or FieldBundles built on {\tt locstream} to the new one simply use {\tt ESMF\_FieldRedist()}
!     or {\tt ESMF\_FieldBundleRedist()}.
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream from which the new location stream is to be created
!      \item[background]
!          Background Grid which determines the distribution of the entries in the new location stream.
!          The background Grid 
!          Note also that this subroutine uses the corner stagger location in the Grid for determining 
!          where a point lies, because this is the stagger location which fully contains the cell. 
!          A Grid must have coordinate data in this stagger location to be used in this subroutine. 
!          For a 2D Grid this stagger location is ESMF\_STAGGERLOC\_CORNER for a 3D Grid this 
!          stagger location is ESMF\_STAGGERLOC\_CORNER\_VFACE. Note that currently the background 
!          Grid also needs to have been created with indexflag=ESMF\_INDEX\_GLOBAL to be usable here. 
!     \item [{[maskValues]}]
!           List of values that indicate a background grid point should be masked out. 
!           If not specified, no masking will occur. 
!     \item [{[unmappedaction]}]
!           Specifies what should happen if there are destination points that
!           can't be mapped to a source cell. Please see Section~\ref{const:unmappedaction} for a 
!           list of valid options. If not specified, {\tt unmappedaction} defaults to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. [NOTE: the {\tt unmappedaction=ESMF\_UNMAPPEDACTION\_IGNORE} option is currently not implemented.]
!      \item[{[name]}]
!          Name of the resulting location stream
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      type(ESMF_LocStreamType), pointer :: oldLStypep, newLStypep
      type(ESMF_UnmappedAction_Flag) :: localunmappedaction
      type(ESMF_Mesh) :: mesh
      type(ESMF_TypeKind_Flag) ::keyTypeKind
      character(len=ESMF_MAXSTR)    :: keytemp, string
      integer :: keyCount,i
      integer :: localrc
      integer :: pntDim, pntCount
      real(ESMF_KIND_R8),  pointer  :: pntList(:)
      integer, pointer :: petList(:), gidList(:)
      type(ESMF_StaggerLoc) :: staggerloc
      integer :: gridDimCount, isSphere


      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Check Variables
      ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,background,rc)


      ! Get Grid dimension
      call ESMF_GridGet(background, dimCount=gridDimCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


     ! Chose staggerloc based on dimension
     if (gridDimCount .eq. 2) then
        staggerLoc=ESMF_STAGGERLOC_CORNER
     else if (gridDimCount .eq. 3) then
        staggerLoc=ESMF_STAGGERLOC_CORNER_VFACE
     else 
        if (ESMF_LogFoundError(ESMF_RC_ARG_WRONG, &
           msg=" - only Grids of dimension 2 or 3 may be used as a background grid ", &
            ESMF_CONTEXT, rcToReturn=rc)) return
     endif

     ! Convert Grid to Mesh
     mesh=ESMF_GridToMesh(background, staggerLoc, 0, maskValues=maskValues, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


     ! Create new locstream from Background Mesh
     ESMF_LocStreamCreateByBkgGrid=ESMF_LocStreamCreate(locstream, &
                 background=mesh, unmappedaction=unmappedaction, &
                 name=name, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
      

      if (present(rc)) rc = ESMF_SUCCESS

       end function ESMF_LocStreamCreateByBkgGrid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreateByBkgMesh"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream by projecting onto a Mesh

! !INTERFACE:
      ! Private name; call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateByBkgMesh(locstream, &
                 background, keywordEnforcer, unmappedaction, name, rc)

!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateByBkgMesh

!
! !ARGUMENTS:
      type(ESMF_LocStream),           intent(in)           :: locstream
      type(ESMF_Mesh),                intent(in)           :: background
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_UnmappedAction_Flag), intent(in), optional :: unmappedaction
      character (len=*),              intent(in), optional :: name
      integer,                        intent(out),optional :: rc
!
! !DESCRIPTION:
!
!     Create an location stream from an existing one in accordance with 
!     the distribution of the background Mesh.  The entries
!     in the new location stream are redistributed, so that they lie on the same PET
!     as the piece of Mesh which contains the coordinates of the entries. The coordinates
!     of the entries are the data in the keys named ESMF:Lon, ESMF:Lat, ESMF:Radius in the 
!     case of a spherical system and ESMF:X, ESMF:Y, ESMF:Z for cartesian. To copy data in
!     Fields or FieldBundles built on {\tt locstream} to the new one simply use {\tt ESMF\_FieldRedist()}
!     or {\tt ESMF\_FieldBundleRedist()}.
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream from which the new location stream is to be created
!      \item[background]
!          Background Mesh which determines the distribution of entries in the new locatiion stream.
!     \item [{[unmappedaction]}]
!           Specifies what should happen if there are destination points that
!           can't be mapped to a source cell. Please see Section~\ref{const:unmappedaction} for a 
!           list of valid options. If not specified, {\tt unmappedaction} defaults to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. [NOTE: the {\tt unmappedaction=ESMF\_UNMAPPEDACTION\_IGNORE} option is currently not implemented.]
!      \item[{[name]}]
!          Name of the resulting location stream
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      type(ESMF_LocStreamType), pointer :: oldLStypep, newLStypep
      type(ESMF_UnmappedAction_Flag) :: localunmappedaction
      type(ESMF_DistGrid) :: newDistGrid
      type(ESMF_TypeKind_Flag) ::keyTypeKind
      character(len=ESMF_MAXSTR)    :: keytemp, string
      integer :: keyCount,i,regrid_dims,idx,idx_cart
      integer :: localrc
      integer :: pntDim, pntCount
      real(ESMF_KIND_R8), pointer  :: pntList(:)
      real(ESMF_KIND_R8), pointer  :: pntList_cart(:)
      integer, pointer :: petList(:)
      character (len=ESMF_MAXSTR)            :: coordKeyNames
      type(ESMF_CoordSys_Flag) :: coordSysLocal, coordSys_ofBkgMesh
      logical :: three_dims

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Check Variables
      ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,background,rc)


      ! Set default vale for unmappedaction
      if (present(unmappedaction)) then
         localunmappedaction=unmappedaction
      else
         localunmappedaction=ESMF_UNMAPPEDACTION_ERROR
      endif

      ! Currently ESMF_UNMAPPEDACTION_IGNORE not implemented here
      if (localunmappedaction .eq. ESMF_UNMAPPEDACTION_IGNORE) then
        if (ESMF_LogFoundError(ESMF_RC_NOT_IMPL, &
           msg=" - ESMF_UNMAPPEDACTION_IGNORE option currently not implemented ", &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif


      ! Get old locstream internal pointer
      oldLStypep=>locstream%lstypep

      call ESMF_MeshGet(background, coordSys=coordSys_ofBkgMesh, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return     

       call ESMF_LocStreamGet(locstream, coordSys=coordSysLocal, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return     

      if ((coordSysLocal .eq. ESMF_COORDSYS_CART .and. &
           coordSys_ofBkgMesh .ne. ESMF_COORDSYS_CART) .or. &
          (coordSysLocal .ne. ESMF_COORDSYS_CART .and. &
           coordSys_ofBkgMesh .eq. ESMF_COORDSYS_CART)) then
        if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
           msg=" - coordinate systems of LocStream and Mesh are not compatible ", &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      if (coordSysLocal .eq. ESMF_COORDSYS_CART) then
        call ESMF_LocStreamGetKey(locstream, keyName="ESMF:Z", &
                                  isPresent=three_dims, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
        if (three_dims) then
          coordKeyNames = "ESMF:X,ESMF:Y,ESMF:Z"
        else
          coordKeyNames = "ESMF:X,ESMF:Y"
        endif

      else
        call ESMF_LocStreamGetKey(locstream, keyName="ESMF:Radius", &
                                  isPresent=three_dims, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
        if (three_dims) then
          coordKeyNames = "ESMF:Lon,ESMF:Lat,ESMF:Radius"
        else
          coordKeyNames = "ESMF:Lon,ESMF:Lat"
        endif

      endif

      ! Calculate pntDim 
       pntDim = 0
       string = trim(coordKeyNames )
       do while ( string /= '' )
          call ESMF_StripKey( string, keytemp )
          pntDim = pntDim + 1
       enddo

      ! Calculate number of local points
      call ESMF_LocStreamGetNumLocal(locstream, localCount=pntCount, &
                                     rc=localrc)      
      if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


      ! Allocate memory for points
      allocate(pntList(pntDim*pntCount), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Allocating pntList", &
        ESMF_CONTEXT, rcToReturn=rc)) return   

      ! Allocate memory for pets
      allocate(petList(pntCount), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Allocating petList", &
        ESMF_CONTEXT, rcToReturn=rc)) return   

      ! Get Points 
      call ESMF_LocStreamGetPntList(locstream, coordKeyNames, pntDim, &
               pntCount, pntList, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


      call c_ESMC_PointListCalcCartDim(coordSysLocal, pntDim, regrid_dims, localrc)
      if (ESMF_LogFoundAllocError(localrc, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      ! Allocate memory for points in cart
      allocate(pntList_cart(regrid_dims*pntCount), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Allocating pntList_cart", &
        ESMF_CONTEXT, rcToReturn=rc)) return   

      do i=1,pntCount
        idx = (i-1) * pntDim + 1
        idx_cart = (i-1) * regrid_dims + 1
        call c_ESMC_PointListSph2CartCoord(coordSysLocal, pntDim, &
                   pntList(idx),pntList_cart(idx_cart),localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      enddo


      ! Find out where points lie on Mesh
      call ESMF_MeshFindPnt(background, localunmappedaction, &
                                regrid_dims, pntCount, pntList_cart, &
                                petList, rc=localrc)

      if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      ! Can now get rid of pntList
      deallocate(pntList)
      deallocate(pntList_cart)

      ! Create a new location stream by shifting the entries between 
      ! the pets based on petList
      ESMF_LocStreamCreateByBkgMesh=ESMF_LocStreamCreatePetList(locstream, name, &
                                  petList, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
     ! Can now get rid of petList
      deallocate(petList)

      ! return success
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateByBkgMesh

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreate"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream from a distgrid

! !INTERFACE:
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateFromDG(distgrid, keywordEnforcer, &
        indexflag, coordSys, name, vm, rc )
!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateFromDG

 !
! !ARGUMENTS:
      type(ESMF_DistGrid),      intent(in)            :: distgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_Index_Flag),    intent(in),  optional :: indexflag    
      type(ESMF_CoordSys_Flag), intent(in),  optional :: coordSys
      character (len=*),        intent(in),  optional :: name
      type(ESMF_VM),            intent(in),  optional :: vm
      integer,                  intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_LocStream} object, constructs its
!     internal derived types. 
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid]
!          Distgrid specifying size and distribution. Only 1D distgrids are allowed.
!     \item[{[indexflag]}]
!          Flag that indicates how the DE-local indices are to be defined.
!          Defaults to {\tt ESMF\_INDEX\_DELOCAL}, which indicates
!          that the index range on each DE starts at 1. See Section~\ref{const:indexflag}
!          for the full range of options. 
!     \item[{[coordSys]}]
!         The coordinate system of the location stream coordinate data.
!         For a full list of options, please see Section~\ref{const:coordsys}.
!         If not specified then defaults to ESMF\_COORDSYS\_SPH\_DEG.
!     \item[{[name]}]
!          Name of the location stream
!     \item[{[vm]}]
!         If present, the LocStream object is created on the specified 
!         {\tt ESMF\_VM} object. The default is to create on the VM of the 
!         current context.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

      integer                             :: localrc  ! Error status
      type (ESMF_LocStreamType), pointer  :: lstypep
      type(ESMF_LocStream)                :: locstream 
      integer                             :: dimCount 
      type(ESMF_Index_Flag)               :: indexflagLocal
      type(ESMF_CoordSys_Flag)            :: coordSysLocal
      type(ESMF_Pointer)                  :: vmThis
      logical                             :: actualFlag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Init check input types
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_DistGridGetInit,distgrid,rc)      

      ! Must make sure the local PET is associated with an actual member
      actualFlag = .true.
      if (present(vm)) then
        call ESMF_VMGetThis(vm, vmThis)
        if (vmThis == ESMF_NULL_POINTER) then
          actualFlag = .false.  ! local PET is not for an actual member of Array
        endif
      endif

      if (actualFlag) then
        ! only actual member PETs actually create a LocStream object
      
        ! Make sure DistGrid is 1D
         call ESMF_DistGridGet(distgrid, dimCount=dimCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return
        if (dimCount .ne. 1) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_RANK, &
            msg=" - DistGrid must be 1D", &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! Set defaults
        if (present(indexflag)) then
          indexflagLocal=indexflag
        else
          indexflagLocal=ESMF_INDEX_DELOCAL
        endif

        if (present(coordSys)) then
          coordSysLocal=coordSys
        else
          coordSysLocal=ESMF_COORDSYS_SPH_DEG
        endif

        ! Initialize pointers
        nullify(lstypep)
        nullify(ESMF_LocStreamCreateFromDG%lstypep)

        ! allocate LocStream type
        allocate(lstypep, stat=localrc)
        if (ESMF_LogFoundAllocError(localrc, msg="Allocating LocStream type object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return


        ! Allocate space for keys
        nullify(lstypep%keyNames)
        nullify(lstypep%keyUnits)
        nullify(lstypep%keyLongNames)
        nullify(lstypep%keys)
        nullify(lstypep%destroyKeys)

        ! Set some remaining info into the struct      
        lstypep%indexflag=indexflagLocal
        lstypep%coordSys=coordSysLocal
        lstypep%destroyDistgrid=.false.
        lstypep%distgrid=distgrid
        lstypep%keyCount=0

        ! set Name
        call ESMF_BaseCreate(lstypep%base,"LocStream",name,0,rc=localrc)       
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

        ! Set pointer to internal locstream type
        locstream%lstypep=>lstypep

        ! Set return value.
        ESMF_LocStreamCreateFromDG=locstream
      
        ! Add reference to this object into ESMF garbage collection table
        ! Only call this in those Create() methods that do not call other LSCreate()
        call c_ESMC_VMAddFObject(locstream, &
          ESMF_ID_LOCSTREAM%objectID)
        
      endif

      ! set init status to created
      ESMF_INIT_SET_CREATED(ESMF_LocStreamCreateFromDG)
 
      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateFromDG
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreate"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream from an irregular distribution

! !INTERFACE:
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateIrreg(minIndex, countsPerDE, &
                  keywordEnforcer, indexflag, coordSys, name, rc)
!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateIrreg

!
! !ARGUMENTS:
      integer, intent(in), optional                   :: minIndex
      integer, intent(in)                             :: countsPerDE(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_Index_Flag), intent(in), optional     :: indexflag
      type(ESMF_CoordSys_Flag), intent(in),  optional :: coordSys
      character (len=*), intent(in), optional         :: name
      integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_LocStream} object, constructs its
!     internal derived types.  The {\tt ESMF\_DistGrid} is set up, indicating
!     how the LocStream is distributed. 
!
!     The arguments are:
!     \begin{description}
!     \item[{[minIndex]}] 
!          If indexflag={\tt ESMF\_INDEX\_DELOCAL}, this setting is used to indicate
!          the number to start the index ranges at. If not present, defaults to 1.
!     \item[{countsPerDE}] 
!          This array has an element for each DE, specifying the number of locations 
!          for that DE.
!     \item[{[indexflag]}]
!          Flag that indicates how the DE-local indices are to be defined.
!          Defaults to {\tt ESMF\_INDEX\_DELOCAL}, which indicates
!          that the index range on each DE starts at 1. See Section~\ref{const:indexflag}
!          for the full range of options. 
!     \item[{[coordSys]}]
!         The coordinate system of the location stream coordinate data.
!         For a full list of options, please see Section~\ref{const:coordsys}.
!         If not specified then defaults to ESMF\_COORDSYS\_SPH\_DEG.
!     \item[{[name]}]
!          Name of the location stream
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

    integer                                               :: localrc  ! Error status
    integer :: i, currMin
    type(ESMF_DistGrid)                 :: distgrid
    integer, pointer :: deBLockList(:,:,:)   
    integer               :: minIndexLocal, maxIndexLocal
    type(ESMF_Index_Flag)  :: indexflagLocal
     type(ESMF_CoordSys_Flag) :: coordSysLocal

    integer :: numDEs

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Set defaults
      if (present(indexflag)) then
         indexflagLocal=indexflag
      else
         indexflagLocal=ESMF_INDEX_DELOCAL
      endif

      if (present(coordSys)) then
         coordSysLocal=coordSys
      else
         coordSysLocal=ESMF_COORDSYS_SPH_DEG
      endif

      if (present(minIndex)) then
         minIndexLocal=minIndex
      else
         minIndexLocal=1 ! default to 1
      endif

      ! get number of DEs
      numDEs=size(countsPerDE)

      ! make they've given us info
      if (numDEs .eq. 0) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
              msg="- countsPerDE is of length 0", & 
              ESMF_CONTEXT, rcToReturn=rc) 
          return
      endif

      ! Calc. maxIndexLocal
      maxIndexLocal=minIndexLocal+sum(countsPerDE(:))-1

     ! Setup DistGrid
     !! setup deBlockList
      allocate(deBlockList(1,2,numDEs), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Allocating deBlockList", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      currMin=minIndexLocal
      do i=1,numDEs
           deBlockList(1,1,i)=currMin    
           deBlockList(1,2,i)=currMin+countsPerDE(i)-1
           currMin=deBlockList(1,2,i)+1
      enddo

      !! Create DistGrid
      distgrid=ESMF_DistGridCreate(minIndex=(/minIndexLocal/), &
                                   maxIndex=(/maxIndexLocal/), &
                                   deBlockList=deBlockList, &
                                   indexflag=indexflagLocal, &
                                   rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      ! cleanup local allocations
      deallocate(deBlockList)

      ! Create LocStream using CreateFromDistGrid version
      ESMF_LocStreamCreateIrreg=ESMF_LocStreamCreateFromDG(name=name, &
                                                               distgrid=distgrid, &
                                                                indexflag=indexflagLocal, &
                                                               coordSys=coordSysLocal, &
                                                               rc=localrc )
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      ! Set distgrid to be destroyed, since ESMF created it
      ESMF_LocStreamCreateIrreg%lstypep%destroyDistgrid=.true.

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateIrreg
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreate"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream from a local count

! !INTERFACE:
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateFromLocal(localCount, keywordEnforcer, &
                  indexflag, coordSys, name, rc)
!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateFromLocal

!
! !ARGUMENTS:
      integer, intent(in)                             :: localCount
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_Index_Flag), intent(in), optional     :: indexflag
      type(ESMF_CoordSys_Flag), intent(in),  optional :: coordSys
      character (len=*), intent(in), optional         :: name
      integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_LocStream} object, constructs its
!     internal derived types.  The {\tt ESMF\_DistGrid} is set up, indicating
!     how the LocStream is distributed. The assumed layout is one DE per PET.
!
!     The arguments are:
!     \begin{description}
!     \item[localCount]
!          Number of grid cells to be distributed to this DE/PET.
!     \item[{[indexflag]}]
!          Flag that indicates how the DE-local indices are to be defined.
!          Defaults to {\tt ESMF\_INDEX\_DELOCAL}, which indicates
!          that the index range on each DE starts at 1. See Section~\ref{const:indexflag}
!          for the full range of options. 
!     \item[{[coordSys]}]
!         The coordinate system of the location stream coordinate data.
!         For a full list of options, please see Section~\ref{const:coordsys}.
!         If not specified then defaults to ESMF\_COORDSYS\_SPH\_DEG.
!     \item[{[name]}]
!          Name of the location stream
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

    integer                                               :: localrc  ! Error status
    type(ESMF_VM)                                   :: vm       ! Virtual machine used
    integer, allocatable  :: countsPerPet(:)
    integer :: localPet, petCount
    integer :: i, currMin
    type(ESMF_DistGrid)                 :: distgrid
    integer, pointer :: deBLockList(:,:,:)   
    integer               :: minIndex(1), maxIndex(1)
    type(ESMF_Index_Flag)  :: indexflagLocal
    type(ESMF_CoordSys_Flag) :: coordSysLocal


      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Set defaults
      if (present(indexflag)) then
         indexflagLocal=indexflag
      else
         indexflagLocal=ESMF_INDEX_DELOCAL
      endif

      if (present(coordSys)) then
         coordSysLocal=coordSys
      else
         coordSysLocal=ESMF_COORDSYS_SPH_DEG
      endif

      ! Get VM for this context
      call ESMF_VMGetCurrent(vm, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      ! Gather localCount for each Pet
      call ESMF_VMGet( vm, localPet = localPet,                        &
                       petCount = petCount, rc=localrc )
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return
      
      allocate(countsPerPet(petCount), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Allocating countsPerPet", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_VMAllGather(vm, sendData=(/localCount/),               &
                            recvData=countsPerPet, count=1, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

     ! Setup DistGrid
     !! define min and maxIndex
     minIndex(1)=1
     maxIndex(1)=sum(countsPerPet(:))

     !! setup deBlockList
      allocate(deBlockList(1,2,petCount), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Allocating deBlockList", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      currMin=1
      do i=1,petCount
           deBlockList(1,1,i)=currMin    
           deBlockList(1,2,i)=currMin+countsPerPet(i)-1
           currMin=deBlockList(1,2,i)+1
      enddo

      !! Create DistGrid
      distgrid=ESMF_DistGridCreate(minIndex=minIndex, &
                                                      maxIndex=maxIndex, &
                                                      deBlockList=deBlockList, &
                                                      indexflag=indexflagLocal, &
                                                      rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      ! cleanup local allocations
      deallocate(countsPerPet)
      deallocate(deBlockList)

      ! Create LocStream using CreateFromDistGrid version
      ESMF_LocStreamCreateFromLocal=ESMF_LocStreamCreateFromDG(name=name, &
                                                               distgrid=distgrid, &
                                                               indexflag=indexflagLocal, &
                                                               coordSys=coordSysLocal, &
                                                               rc=localrc )
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      ! Set distgrid to be destroyed, since ESMF created it
      ESMF_LocStreamCreateFromLocal%lstypep%destroyDistgrid=.true.

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateFromLocal
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD  
#define ESMF_METHOD "ESMF_LocStreamCreateFromNewDG"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream from an old one and a distgrid

! !INTERFACE:  
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateFromNewDG(locstream, distgrid, keywordEnforcer, &
           name, rc)

!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateFromNewDG

!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in)                :: locstream
      type(ESMF_DistGrid),  intent(in)                :: distgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character (len=*),    intent(in), optional      :: name
      integer,              intent(out), optional     :: rc
!
! !DESCRIPTION:
!
!     Create a new location stream that is a copy of an old one, but with a new
!     distribution. The new distribution is given by a distgrid passed into the call.
!     Key and other class information is copied from the old locstream to the new one. 
!     Information contained in Fields build on the location streams can be copied over
!     by using the Field redistribution calls (e.g. {\tt ESMF\_FieldRedistStore()} 
!     and {\tt ESMF\_FieldRedist()}).   
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream from which the new location stream is to be created
!      \item[distgrid]
!          Distgrid for new distgrid
!      \item[{[name]}]
!          Name of the resulting location stream
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      type(ESMF_LocStreamType), pointer :: oldLStypep, newLStypep
      type(ESMF_LocStream):: newLocStream
      type(ESMF_ArrayBundle) :: oldAB, newAB
      type(ESMF_RouteHandle) :: routehandle
      type(ESMF_TypeKind_Flag) ::keyTypeKind
      type(ESMF_CoordSys_Flag) :: coordSysLocal
      character(len=ESMF_MAXSTR)    :: keytemp, string
      integer :: keyCount,i
      integer :: localrc


! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Check Variables
      ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit,distgrid,rc)

      ! Get old locstream internal pointer
      oldLStypep=>locstream%lstypep     

      call ESMF_LocStreamGet(locstream, coordSys=coordSysLocal, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return     

      ! Create new locStream
      newLocStream=ESMF_LocStreamCreateFromDG(name=name, distgrid=distgrid, &
                indexflag=oldLSTypep%indexFlag, &
                coordSys=coordSysLocal,rc=localrc) 
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return     


      ! Add keys to new Locstream
      ! NOTE: We need a subroutine to add a list of keys. This is inefficient because of the allocations 
      !       and searching for already in
      keyCount=oldLStypep%keyCount
      do i=1,keyCount

         ! get key typeKind
         call ESMF_ArrayGet(oldLStypep%keys(i), typekind=keyTypeKind, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return     

         call ESMF_LocStreamAddKey(newLocStream, &
              keyName=oldLstypep%keyNames(i), &
              keyTypekind=keyTypeKind, &
              keyUnits=oldLstypep%keyUnits(i), &
              keyLongName=oldLstypep%keyLongNames(i), &
              rc=localrc)
         if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return     
      enddo


      ! Get new locstream internal pointer
      newLStypep=>newLocStream%lstypep

      ! NOTE THAT THIS ONLY WORKS BECAUSE THE LocStreamAddKey SUBROUTINE
      ! ADDS KEYS AT THE END. IF THIS CHANGES THIS'LL STOP WORKING. 
      ! FOR EFFICENCY REASONS I'LL LEAVE IT FOR NOW. IF IT CHANGES
      ! REWRITE TO NOT DEPEND ON ORDER


      ! Redistribute data from one locstream to another 

      ! Create ArrayBundles for redistribution
      oldAB=ESMF_ArrayBundleCreate(arrayList=oldLStypep%keys, rc=localrc)      
       if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return     

      newAB=ESMF_ArrayBundleCreate(arrayList=newLStypep%keys, rc=localrc)      
       if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return     


      ! Setup for redist
      call ESMF_ArrayBundleRedistStore(srcArrayBundle=oldAB, dstArrayBundle=newAB, &
             routehandle=routeHandle, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return     

      ! Do redist
      call ESMF_ArrayBundleRedist(srcArrayBundle=oldAB, dstArrayBundle=newAB, &
            routehandle=routeHandle, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return     

      ! Get rid of routehandle
      call  ESMF_ArrayBundleRedistRelease(routehandle=routehandle, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return     

      ! Get rid of ArrayBundles
      call ESMF_ArrayBundleDestroy(oldAB, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return     

      call ESMF_ArrayBundleDestroy(newAB, rc=localrc)
       if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return     

     ! Output new locstream
     ESMF_LocStreamCreateFromNewDG=newLocStream

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateFromNewDG

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreate"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream using a regular distribution

! !INTERFACE:
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateReg(regDecomp, decompFlag, &
                    minIndex, maxIndex, keywordEnforcer, &
                    coordSys, indexflag, name, rc)


!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateReg

!
! !ARGUMENTS:
      integer,                  intent(in),  optional  :: regDecomp
      type(ESMF_Decomp_Flag),   intent(in),  optional  :: decompflag
      integer,                  intent(in),  optional  :: minIndex
      integer,                  intent(in)             :: maxIndex
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_CoordSys_Flag), intent(in),  optional  :: coordSys
      type(ESMF_Index_Flag),    intent(in),  optional  :: indexflag
      character (len=*),        intent(in),  optional  :: name
      integer,                  intent(out), optional  :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_LocStream} object, constructs its
!     internal derived types.  The {\tt ESMF\_DistGrid} is set up, indicating
!     how the LocStream is distributed. 
!
!     The arguments are:
!     \begin{description}
!     \item[{[regDecomp]}]
!          Specify into how many chunks to divide the locations. 
!          If not specified, defaults to the number of PETs.
!     \item[{[decompFlag]}]
!          \begin{sloppypar}
!          Specify what to do with leftover locations after division.
!          If not specified, defaults to {\tt ESMF\_DECOMP\_BALANCED}. Please
!          see Section~\ref{const:decompflag} for a full description of the 
!          possible options. 
!          \end{sloppypar}
!     \item[{[minIndex]}] 
!          If indexflag={\tt ESMF\_INDEX\_DELOCAL}, this setting is used to indicate
!          the number to start the index ranges at. If not present, defaults to 1.
!     \item[maxIndex]
!          The maximum index across all PETs.
!     \item[{[coordSys]}]
!         The coordinate system of the location stream coordinate data.
!         For a full list of options, please see Section~\ref{const:coordsys}.
!         If not specified then defaults to ESMF\_COORDSYS\_SPH\_DEG.
!     \item[{[indexflag]}]
!          Flag that indicates how the DE-local indices are to be defined.
!          Defaults to {\tt ESMF\_INDEX\_DELOCAL}, which indicates
!          that the index range on each DE starts at 1. See Section~\ref{const:indexflag}
!          for the full range of options. 
!     \item[{[name]}]
!          Name of the location stream
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

      integer               :: localrc  ! Error status
      type(ESMF_DistGrid)   :: distgrid
      integer               :: minIndexLocal,regDecompLocal
      type(ESMF_Decomp_Flag) :: decompFlagLocal
      type(ESMF_Index_Flag)  :: indexflagLocal
      type(ESMF_CoordSys_Flag) :: coordSysLocal

      type(ESMF_VM)         :: vm 

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Set defaults
      if (present(decompflag)) then
         decompFlagLocal=decompflag
      else
         decompFlagLocal=ESMF_DECOMP_BALANCED
      endif

      if (present(indexflag)) then
         indexflagLocal=indexflag
      else
         indexflagLocal=ESMF_INDEX_DELOCAL
      endif

      if (present(coordSys)) then
         coordSysLocal=coordSys
      else
         coordSysLocal=ESMF_COORDSYS_SPH_DEG
      endif

      if (present(minIndex)) then
         minIndexLocal=minIndex
      else
         minIndexLocal=1
      endif

      if (present(regDecomp)) then
         regDecompLocal=regDecomp
      else
        ! By default set regdecomp to the number of PETs
        !! Get VM for this context
        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

        !! Get petCount from VM
        call ESMF_VMGet(vm, petCount=regDecompLocal, rc=localrc )
        if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! Create DistGrid
      distgrid=ESMF_DistGridCreate(minIndex=(/minIndexLocal/), &
                                   maxIndex=(/maxIndex/), &
                                   regDecomp=(/regDecompLocal/), &
                                   decompFlag=(/decompFlagLocal/), &
                                   indexflag=indexflagLocal, &
                                   rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return


      ! Create LocStream using CreateFromDistGrid version
      ESMF_LocStreamCreateReg=ESMF_LocStreamCreateFromDG(name=name, &
                                                         distgrid=distgrid, &
                                                         indexflag=indexflagLocal,&
                                                         coordSys=coordSysLocal, &
                                                         rc=localrc )
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      ! Set distgrid to be destroyed, since ESMF created it
      ESMF_LocStreamCreateReg%lstypep%destroyDistgrid=.true.

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateReg
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreateFromFile"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream from a grid file
!\label{locstream:createfromfile}
! !INTERFACE:
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateFromFile(filename, keywordEnforcer, &
           fileformat, meshname, varname, indexflag, centerflag, name, rc)
!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateFromFile

!
! !ARGUMENTS:
      character (len=*),          intent(in)           :: filename
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_FileFormat_Flag), intent(in), optional :: fileformat
      character (len=*),          intent(in), optional :: meshname
      character(len=*),           intent(in), optional :: varname
      type(ESMF_Index_Flag),      intent(in), optional :: indexflag
      logical,                    intent(in), optional :: centerflag
      character (len=*),          intent(in), optional :: name
      integer,                    intent(out),optional :: rc

! !DESCRIPTION:
!     Create a new {\tt ESMF\_LocStream} object and add the coordinate keys and mask key
!     to the LocStream using the coordinates defined in a grid file.  Currently, it 
!     supports the SCRIP format, the ESMF unstructured grid format and the UGRID format.
!     For a grid in ESMF or UGRID format, it can construct the LocStream using either 
!     the center coordinates or the corner coordinates.  For a SCRIP format grid file, the
!     LocStream can only be constructed using the center coordinates.  
!
!     The arguments are:
!     \begin{description}
!     \item[filename]
!          Name of grid file to be used to create the location stream.  
!     \item[{[fileformat]}]
!          Flag that indicates the file format of the grid file.  Please see
!          Section~\ref{const:grid:fileformat} and Section~\ref{const:mesh:fileformat} for a 
!          list of valid options (note that the {\tt ESMF\_FILEFORMAT\_GRIDSPEC} format is not
!          supported).  If not specified, the default is {\tt ESMF\_FILEFORMAT\_SCRIP}.
!     \item[{[meshname]}]
!         The dummy variable for the mesh metadata in the UGRID file if the {\tt fileformat}
!         is {\tt ESMF\_FILEFORMAT\_UGRID}.  This argument is optional.
!     \item[{[varname]}]
!         An optional variable name stored in the UGRID file to be used to
!         generate the mask using the missing value of the data value of
!         this variable.  The first two dimensions of the variable has to be the
!         the longitude and the latitude dimension and the mask is derived from the
!         first 2D values of this variable even if this data is 3D, or 4D array. If not 
!         specified, no mask is used for a UGRID file.
!     \item[{[indexflag]}]
!          Flag that indicates how the DE-local indices are to be defined.
!          Defaults to {\tt ESMF\_INDEX\_DELOCAL}, which indicates
!          that the index range on each DE starts at 1. See Section~\ref{const:indexflag}
!          for the full range of options. 
!     \item[{[centerflag]}]
!          Flag that indicates whether to use the center coordinates to construct the location stream.
!          If true, use center coordinates, otherwise, use the corner coordinates.  If not specified,
!          use center coordinates as default.  For SCRIP files, only center coordinate 
!          is supported.
!     \item[{[name]}]
!          Name of the location stream
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

#ifdef ESMF_NETCDF
    integer :: totalpoints,totaldims
    type(ESMF_VM) :: vm
    integer :: numDim, buf(1), msgbuf(3)
    integer :: localrc
    integer :: PetNo, PetCnt
    type(ESMF_Index_Flag) :: indexflagLocal
    real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:), coordZ(:)
    real(ESMF_KIND_R8), pointer :: coord2D(:,:), varbuffer(:)
    integer(ESMF_KIND_I4), pointer :: imask(:)
    integer :: starti, count, localcount, index
    integer :: remain, i
    integer :: meshid
    real(ESMF_KIND_R8) :: missingvalue
    type(ESMF_CoordSys_Flag) :: coordSys
    type(ESMF_LocStream) :: locStream
    type(ESMF_FileFormat_Flag) :: localfileformat
    logical :: localcenterflag, haveface
    character(len=16) :: units, location

    if (present(indexflag)) then
       indexflagLocal=indexflag
    else
       indexflagLocal=ESMF_INDEX_DELOCAL
    endif
    
    if (present(fileformat)) then
       localfileformat = fileformat
    else
       localfileformat = ESMF_FILEFORMAT_SCRIP
    endif

    if (present(centerflag)) then
       localcenterflag = centerflag
    else
       localcenterflag = .TRUE.
    endif

    if (localcenterflag) then
       location = 'face'
    else
       location = 'node'
    endif
    if (localfileformat == ESMF_FILEFORMAT_GRIDSPEC) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
          msg="Create LocStream from a GRIDSPEC file is not supported.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
    endif   

    if ( localfileformat == ESMF_FILEFORMAT_SCRIP .and. &
       .NOT. localcenterflag) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
          msg="Only allow center coordinates if the file is in SCRIP format", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
    endif   

    if (localfileformat /= ESMF_FILEFORMAT_UGRID .and. &
       (present(meshname) .or. present(varname))) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
          msg="Only UGRID file need the optional arguments meshname or varname", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
    endif
   
    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! get global vm information
    !
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    if (localfileformat == ESMF_FILEFORMAT_SCRIP) then 
       ! totaldims represent grid_ranks in SCRIP - 1 for unstructured and
       ! 2 for logically rectangular
       call ESMF_ScripInq(filename, grid_rank=totaldims, &
                          grid_size=totalpoints, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
       call ESMF_ScripInqUnits(filename, units=units, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
       if (units == 'degrees') then
          coordSys = ESMF_COORDSYS_SPH_DEG
       else
          coordSys = ESMF_COORDSYS_SPH_RAD
       endif   
#if 0
    elseif (localfileformat == ESMF_FILEFORMAT_GRIDSPEC) then
       ! totaldims is the dimension of the lat/lon variable: 1 for regular
       ! grid and 2 for curvilinear
       call ESMF_GridspecInq(filename, ndims=totaldims, &
                             grid_dims = grid_dims, &
                             coordids=varids, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
       totalpoints=grid_dims(1)*grid_dims(2)
#endif
    elseif (localfileformat == ESMF_FILEFORMAT_ESMFMESH) then
       ! totaldims is the coordDim, 2 for 2D and 3 for 3D
       if (localcenterflag) then
           call ESMF_EsmfInq(filename, elementCount=totalpoints, & 
                             coordDim=totaldims, rc=localrc)
       else
           call ESMF_EsmfInq(filename, nodeCount=totalpoints, &
                             coordDim=totaldims, rc=localrc)
       endif
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
       call ESMF_EsmfInqUnits(filename, units, rc=localrc)
       if (units == 'degrees') then
          coordSys = ESMF_COORDSYS_SPH_DEG
       elseif (units == 'radians') then
          coordSys = ESMF_COORDSYS_SPH_RAD
       else
          coordSys = ESMF_COORDSYS_CART
       endif   
    elseif (localfileformat == ESMF_FILEFORMAT_UGRID) then
       ! totaldims is the mesh_dimension (2 for 2D and 3 for 3D)
       if (localcenterflag) then
          call ESMF_UGridInq(filename, meshname=meshname, elementCount=totalpoints, &
                             meshid=meshid, nodeCoordDim=totaldims, &
                             faceCoordFlag=haveface, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          if (.not. haveface) then
             call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                    msg="The grid file does not have face coordinates", &
                    ESMF_CONTEXT, rcToReturn=rc)
             return
          endif
       else
          call ESMF_UGridInq(filename, meshname=meshname, nodeCount=totalpoints, &
                             meshid=meshid, nodeCoordDim=totaldims, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
       endif
       coordSys = ESMF_COORDSYS_SPH_DEG
    endif             

    localcount = totalpoints/PetCnt
    remain = totalpoints - (localcount*PetCnt)
    if (PetNo < remain) then
       localcount = localcount+1
       starti = localcount*PetNo+1
    else
       starti = localcount*PetNo+1+remain
    endif

    allocate(coordX(localcount), coordY(localcount),imask(localcount))
    if (localfileformat == ESMF_FILEFORMAT_SCRIP) then 
       call ESMF_ScripGetVar(filename, grid_center_lon=coordX, grid_center_lat=coordY, &
                          grid_imask=imask, start=starti, count=localcount, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
#if 0
    elseif (localfileformat == ESMF_FILEFORMAT_GRIDSPEC) then 
       if (totaldims == 1) then
          call ESMF_GridspecGetVar1D(filename, varids, coordX, coordY, rc=localrc)
          !construct 2D arrays and do the distribution
          xdim=size(coordX)
          ydim=size(coordY)
#endif
    elseif (localfileformat == ESMF_FILEFORMAT_ESMFMESH) then
       allocate(coord2D(totaldims,localcount))
       call ESMF_EsmfGetCoords(filename, coord2D, imask, &
                               starti, localcount, localcenterflag, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
       coordX(:) = coord2D(1,:)
       coordY(:) = coord2D(2,:)
       if (totaldims == 3) then
         allocate(coordZ(localcount))
         coordZ(:) = coord2D(3,:)
       endif
       deallocate(coord2D)
    elseif (localfileformat == ESMF_FILEFORMAT_UGRID) then
       allocate(coord2D(localcount, totaldims))
       call ESMF_UGridGetCoords(filename, meshid, coord2D, &
                                starti, localcount, localcenterflag, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
       coordX(:) = coord2D(:,1)
       coordY(:) = coord2D(:,2)
       if (totaldims == 3) then
          allocate(coordZ(localcount))
          coordZ(:) = coord2D(:,3)
       endif
       deallocate(coord2D)
       ! Get mask from varname
       imask(:)=1
       if (present(varname)) then
          allocate(varbuffer(localcount))
          call ESMF_UGridGetVarByName(filename, varname, varbuffer, &
                                      startind=starti, count=localcount, &
                                      location=location, &
                                      missingvalue=missingvalue, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
          do i=1,localcount
            if (varbuffer(i)==missingvalue) imask(i)=0
          enddo
          deallocate(varbuffer)
       endif
    endif
    ! create Location Stream
    locStream = ESMF_LocStreamCreate(name=name, localcount=localcount, indexflag=indexflagLocal,&
                coordSys = coordSys, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    !print *, PetNo, starti, localcount, coordX(1), coordY(1)
    ! Add coordinate keys
    call ESMF_LocStreamAddKey(locStream, 'ESMF:Lon',coordX, keyUnits=units, &
                              keyLongName='Longitude', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_LocStreamAddKey(locStream, 'ESMF:Lat',coordY, keyUnits=units, &
                              keyLongName='Latitude', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

    !If 3D grid, add the height coordinates
    if (totaldims == 3) then
       call ESMF_LocStreamAddKey(locStream, 'ESMF:Radius',coordZ, keyUnits='radius', &
                                 keyLongName='Height', rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    !Add mask key
    call ESMF_LocStreamAddKey(locStream, 'ESMF:Mask',imask,  &
                              keyLongName='Mask', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
   
    ESMF_LocStreamCreateFromFile = locStream
    if (present(rc)) rc=ESMF_SUCCESS
    return

#else
    if (present(rc)) rc = ESMF_RC_LIB_NOT_PRESENT
#endif

end function ESMF_LocStreamCreateFromFile

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamDestroy"
!BOP
! !IROUTINE: ESMF_LocStreamDestroy - Release resources associated with a LocStream 

! !INTERFACE:
      subroutine ESMF_LocStreamDestroy(locstream, keywordenforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(inout)          :: locstream 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,              intent(out),  optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Deallocate an {\tt ESMF\_LocStream} object and appropriate 
!     internal structures.
!
!     The arguments are:
!     \begin{description}
!     \item[locstream]
!          locstream to destroy
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

      integer                                              :: localrc  ! Error status
      type (ESMF_LocStreamType), pointer :: lstypep
      integer :: i 

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Init check input types
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_LocStreamGetInit,locstream,rc)      

      if (.not.associated(locstream%lstypep)) then 
        call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
          msg="Uninitialized or already destroyed LocStream: lstypep unassociated", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif 

      ! Destruct all field internals and then free field memory.
      call ESMF_LocStreamDestruct(locstream%lstypep, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark object invalid
      call ESMF_BaseSetStatus(locstream%lstypep%base, ESMF_STATUS_INVALID, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return
                                
      ! Set init status to indicate structure has been destroyed
      ESMF_INIT_SET_DELETED(locstream)

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LocStreamDestroy
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamDestruct"
!BOPI
! !IROUTINE: ESMF_LocStreamDestruct - Destruct a LocStream 

! !INTERFACE:
      subroutine ESMF_LocStreamDestruct(lstypep,rc)
!
! !ARGUMENTS:
      type (ESMF_LocStreamType), pointer :: lstypep
      integer, intent(out), optional               :: rc
!
! !DESCRIPTION:
!     Destruct an {\tt ESMF\_LocStream} object and all appropriate 
!     internal structures.
!
!     The arguments are:
!     \begin{description}
!     \item[lstypep]
!          locstream to destruct
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      integer :: localrc  ! Error status
      integer :: i 
      type(ESMF_Status) :: status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      call ESMF_BaseGetStatus(lstypep%base, status, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
      if (status .eq. ESMF_STATUS_READY) then  
        
        ! Destroy  key Arrays
        do i=1,lstypep%keyCount
          if (lstypep%destroyKeys(i)) then
             call ESMF_ArrayDestroy(lstypep%keys(i), rc=localrc)       
             if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return
          endif
        enddo


        ! destroy distgrid
        if (lstypep%destroyDistGrid) then
         !! destroy distgrid
         call ESMF_DistGridDestroy(lstypep%distgrid, rc=localrc)       
         if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! Deallocate space for key data (if its been allocated)
        if (lstypep%keyCount .gt. 0) then
          deallocate (lstypep%keyNames)
          deallocate (lstypep%keyUnits)
          deallocate (lstypep%keyLongNames)
          deallocate( lstypep%keys)
          deallocate( lstypep%destroyKeys)
        endif
        
      endif

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LocStreamDestruct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGet"
!BOP
! !IROUTINE: ESMF_LocStreamGet - Return object-wide information from a LocStream

! !INTERFACE:
  subroutine ESMF_LocStreamGet(locstream, keywordEnforcer, &
       distgrid, keyCount, keyNames, localDECount, indexflag, &
       coordSys, name, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream),         intent(in)            :: locstream
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_DistGrid),          intent(out), optional :: distgrid
    integer,                      intent(out), optional :: keyCount
    character(len=ESMF_MAXSTR),                optional :: keyNames(:) 
    integer,                      intent(out), optional :: localDECount
    type(ESMF_Index_Flag),        intent(out), optional :: indexflag
    type(ESMF_CoordSys_Flag),     intent(out), optional :: coordSys
    character(len=*),             intent(out), optional :: name
    integer,                      intent(out), optional :: rc


!
! !DESCRIPTION:
! Query an {\tt ESMF\_LocStream} for various information. All arguments after
! the {\tt locstream} are optional. 
!
! The arguments are:
! \begin{description}
! \item [locstream]
! The {\tt ESMF\_LocStream} object to query.
! \item [{[distgrid]}]
! The {\tt ESMF\_DistGrid} object that describes 
! \item [{[keyCount]}]
! Number of keys in the {\tt locstream}.
! \item [{[keyNames]}]
! The names of the keys in the {\tt locstream}. Keynames should
! be an array of character strings. The character strings should
! be of length ESMF\_MAXSTR and the array's length should be
! at least keyCount. 
! \item [{[localDECount]}]
! Number of DEs on this PET in the {\tt locstream}.
! \item [{[indexflag]}]
! The indexflag for this indexflag.
! \item [{[coordSys]}]
! The coordinate system for this location stream.
! \item [{[name]}]
! Name of queried item.
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    type(ESMF_LocStreamType), pointer :: lstypep
    integer :: localrc
    type(ESMF_DELayout) :: delayout

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

    ! Get pointer
    lstypep => locstream%lstypep

    ! get distgrid
    if (present(distgrid)) then
        distgrid = lstypep%distgrid
    endif

    ! get keyCount
    if (present(keyCount)) then
        keyCount = lstypep%keyCount
    endif

    ! get keyNames
    if (present(keyNames)) then
       if (size(keyNames) .lt. lstypep%keyCount) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
              msg="- keyNames array too short", & 
              ESMF_CONTEXT, rcToReturn=rc) 
          return 
       endif

       if (lstypep%keyCount .gt. 0) then
          keyNames(1:lstypep%keyCount)=lstypep%keyNames(1:lstypep%keyCount)
       endif
    endif


   ! Get localDECount
   if (present(localDECount)) then
      call ESMF_DistGridGet(lstypep%distgrid, delayout=delayout, rc=localrc) 
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
          ESMF_CONTEXT, rcToReturn=rc)) return
 
      call ESMF_DELayoutGet(delayout, localDeCount=localDECount, rc=localrc) 
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
       ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! get indexflag
    if (present(indexflag)) then
        indexflag = lstypep%indexflag
    endif

    ! get coordSys
    if (present(coordSys)) then
        coordSys = lstypep%coordSys
    endif

    if (present(name)) then
        call ESMF_GetName(lstypep%base, name, localrc)
        if (ESMF_LogFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetBounds"
!BOP
! !IROUTINE: ESMF_LocStreamGetBounds - Get DE-local bounds of a LocStream

! !INTERFACE:
      subroutine ESMF_LocStreamGetBounds(locstream, keywordEnforcer,   &
          localDE, exclusiveLBound, exclusiveUBound, exclusiveCount,   &
          computationalLBound, computationalUBound, computationalCount,&
          rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream),   intent(in) :: locstream
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                intent(in),  optional :: localDE
      integer,                intent(out), optional :: exclusiveLBound
      integer,                intent(out), optional :: exclusiveUBound
      integer,                intent(out), optional :: exclusiveCount
      integer,                intent(out), optional :: computationalLBound
      integer,                intent(out), optional :: computationalUBound
      integer,                intent(out), optional :: computationalCount
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets the bounds of a localDE for a locstream.
!
!     The arguments are:
!     \begin{description}
!     \item[{locstream}]
!          LocStream to get the information from.
!     \item[{localDE}]
!         The local DE for which information is requested. {\tt [0,..,localDECount-1]}.
!         For {\tt localDECount==1} the {\tt localDE} argument may be omitted,
!          in which case it will default to {\tt localDE=0}.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!     \item[{[exclusiveCount]}]
 !          Upon return this holds the number of items in the exclusive region
!     \newline
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount}.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region
!     \newline
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

 integer :: localrc
 integer :: tmpLBnd, tmpUBnd
 type(ESMF_LocStreamType), pointer :: lstypep

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit, locstream, rc) 

 ! Get locstream type object
 lstypep=>locstream%lstypep

 ! Get exclusiveLBound
 if (present(exclusiveLBound)) then
    call c_ESMC_locstreamgetelbnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             exclusiveLBound, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return
 endif 

 ! Get exclusiveUBound
 if (present(exclusiveUBound)) then
    call c_ESMC_locstreamgeteubnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
              exclusiveUBound, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return
 endif 

 ! Get exclusiveCount
 if (present(exclusiveCount)) then
    call c_ESMC_locstreamgetelbnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             tmpLBnd, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_locstreamgeteubnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             tmpUBnd, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

    exclusiveCount=tmpUBnd-tmpLBnd+1
 endif 

 ! For now computational bounds are the same as exclusive bounds

 ! Get computationalLBound
 if (present(computationalLBound)) then
    call c_ESMC_locstreamgetelbnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             computationalLBound, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return
 endif 

 ! Get computationalUBound
 if (present(computationalUBound)) then
    call c_ESMC_locstreamgeteubnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             computationalUBound, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return
 endif 

 ! Get computationalCount
 if (present(computationalCount)) then
    call c_ESMC_locstreamgetelbnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             tmpLBnd, localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_locstreamgeteubnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             tmpUBnd, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

    computationalCount=tmpUBnd-tmpLBnd+1
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetBounds


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyArray"
!BOP
! !IROUTINE: ESMF_LocStreamGetKey - Get an Array associated with a key

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
  subroutine ESMF_LocStreamGetKeyArray(locstream, keyName, keyArray, &
       keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)            :: locstream
    character (len=*),    intent(in)            :: keyName
    type(ESMF_Array),     intent(out)           :: keyArray
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out), optional :: rc
!
! !DESCRIPTION:
! Get ESMF Array associated with key.
!
! The arguments are:
! \begin{description}
! \item [locstream]
! The {\tt ESMF\_LocStream} object to get key from.
! \item [keyName]
! The name of the key to get. 
! \item [keyArray]
! Array associated with key.
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    type(ESMF_LocStreamType), pointer :: lstypep
    integer :: i,keyIndex
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

    ! get the pointer to the locstream
    lstypep => locstream%lstypep

    ! find the index of the key
    keyIndex=0
    do i=1,lstypep%keyCount
       if (trim(keyName) .eq. trim(lstypep%keyNames(i))) then
          keyIndex=i
          exit 
       endif
    enddo

   ! If nothing found return error
   if (keyIndex==0) then
      if (ESMF_LogFoundError(ESMF_RC_ARG_WRONG, &
            msg=" - unable to find the following LocStream key: "//keyName, &
            ESMF_CONTEXT, rcToReturn=rc)) return
   endif

   ! Get Array
   keyArray=lstypep%keys(keyIndex)

   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamGetKeyArray
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyInfo"
!BOP
! !IROUTINE: ESMF_LocStreamGetKey - Get info associated with a key

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
  subroutine ESMF_LocStreamGetKeyInfo(locstream, keyName, keywordEnforcer,&
       keyUnits, keyLongName, typekind, isPresent, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream),     intent(in)            :: locstream
    character (len=*),        intent(in)            :: keyName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character (len=*),        intent(out), optional :: keyUnits 
    character (len=*),        intent(out), optional :: keyLongName 
    type(ESMF_TypeKind_Flag), intent(out), optional :: typekind
    logical,                  intent(out), optional :: isPresent
    integer,                  intent(out), optional :: rc
!
! !DESCRIPTION:
! Get ESMF Array associated with key.
!
! The arguments are:
! \begin{description}
! \item [locstream]
! The {\tt ESMF\_LocStream} object to get key from.
! \item [keyName]
! The name of the key to get. 
! \item [{[keyUnits]}]
! The units of the key data. 
! If not specified, then the item remains blank.  
! \item [{[keyLongName]}]
! The long name of the key data. 
! If not specified, then the item remains blank.  
! \item [{[typekind]}]
! The typekind of the key data
! \item [{[isPresent]}]
! Whether or not the keyname is present 
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------------
    type(ESMF_LocStreamType), pointer :: lstypep
    integer :: i,keyIndex
    integer :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

    ! get the pointer to the locstream
    lstypep => locstream%lstypep

    ! find the index of the key
    keyIndex=0
    do i=1,lstypep%keyCount
       if (trim(keyName) .eq. trim(lstypep%keyNames(i))) then
          keyIndex=i
          exit 
       endif
    enddo


   if (keyIndex==0) then
     if (present(isPresent) .and. &
         .not. present(keyUnits) .and. &
         .not. present(keyLongName) .and. &
         .not. present(typekind)) then
       isPresent = .false.
     else
       if (ESMF_LogFoundError(ESMF_RC_ARG_WRONG, &
                              msg=" - LocStream info not found for this keyName", &
                              ESMF_CONTEXT, rcToReturn=rc)) return
     endif     
   else

     ! Get Info
     if (present(isPresent)) then
       isPresent=.true.
     endif

     if (present(keyUnits)) then
       keyUnits=lstypep%keyUnits(keyIndex)
     endif

     if (present(keyLongName)) then
       keyLongName=lstypep%keyLongNames(keyIndex)
     endif

     if (present(typekind)) then
       call ESMF_ArrayGet(lstypep%keys(keyIndex), typekind=typekind, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
           ESMF_CONTEXT, rcToReturn=rc)) return
     endif

   endif

   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamGetKeyInfo
!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_LocStreamGetKey - Get a DE-local Fortran array pointer to key values

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
!      subroutine ESMF_LocStreamGetKey(locstream, keyName, keywordEnforcer, &
!          localDE, exclusiveLBound, exclusiveUBound, exclusiveCount,       &
!          computationalLBound, computationalUBound, computationalCount,    &
!          totalLBound, totalUBound, totalCount,                            &
!          farray, datacopyflag, rc)
!
! !ARGUMENTS:
!      type(ESMF_LocStream),   intent(in)            :: locstream
!      character (len=*),      intent(in)            :: keyName
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!      integer,                intent(in),  optional :: localDE
!      integer,                intent(out), optional :: exclusiveLBound
!      integer,                intent(out), optional :: exclusiveUBound
!      integer,                intent(out), optional :: exclusiveCount
!      integer,                intent(out), optional :: computationalLBound
!      integer,                intent(out), optional :: computationalUBound
!      integer,                intent(out), optional :: computationalCount
!      integer,                intent(out), optional :: totalLBound
!      integer,                intent(out), optional :: totalUBound
!      integer,                intent(out), optional :: totalCount
!      <farray>
!      type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    key data for a particular key on the given local DE. 
!    This is useful, for example, for setting the key values in a LocStream, or
!    for reading the values. 
!
!    Supported values for <farray> are:
!    \begin{description}
!    \item integer(ESMF\_KIND\_I4), pointer :: farray(:)
!    \item real(ESMF\_KIND\_R4), pointer :: farray(:)
!    \item real(ESMF\_KIND\_R8), pointer :: farray(:)
!    \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[{locstream}]
!          LocStream to get the information from.
!     \item[{keyName}]
!          The key to get the information from.
!     \item[{[localDE]}]
!         The local DE for which information is requested. {\tt [0,..,localDECount-1]}.
!         For {\tt localDECount==1} the {\tt localDE} argument may be omitted,
!          in which case it will default to {\tt localDE=0}.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region \newline
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount}.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region
!          \newline
!          (i.e. {\tt computationalUBound-computationalLBound+1}). 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region
!          (i.e. {\tt totalUBound-totalLBound+1}). 
!     \item[{farray}]
!          The pointer to the coordinate data.
!     \item[{[datacopyflag]}]
!          If not specified, default to {\tt ESMF\_DATACOPY\_REFERENCE}, in this case
!          farray is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{const:datacopyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyI4"
!BOPI
! !IROUTINE: ESMF_LocStreamGetKey - Get pointer to key values

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
      subroutine ESMF_LocStreamGetKeyI4(locstream, keyName, keywordEnforcer, &
          localDE, exclusiveLBound, exclusiveUBound, exclusiveCount,     &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,     &
          farray, datacopyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream),   intent(in)            :: locstream
      character (len=*),      intent(in)            :: keyName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                intent(in),  optional :: localDE
      integer,                intent(out), optional :: exclusiveLBound
      integer,                intent(out), optional :: exclusiveUBound
      integer,                intent(out), optional :: exclusiveCount
      integer,                intent(out), optional :: computationalLBound
      integer,                intent(out), optional :: computationalUBound
      integer,                intent(out), optional :: computationalCount
      integer,                intent(out), optional :: totalLBound
      integer,                intent(out), optional :: totalUBound
      integer,                intent(out), optional :: totalCount
      integer(ESMF_KIND_I4), pointer :: farray(:)
      type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    key data for a particular key on the given local DE. 
!    This is useful, for example, for setting the key values in a LocStream, or
!    for reading the values. 
!
!     The arguments are:
!     \begin{description}
!     \item[{locstream}]
!          LocStream to get the information from.
!     \item[{keyName}]
!          The key to get the information from.
!     \item[{[localDE]}]
!         The local DE for which information is requested. {\tt [0,..,localDECount-1]}.
!         For {\tt localDECount==1} the {\tt localDE} argument may be omitted,
!          in which case it will default to {\tt localDE=0}.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region
 !          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount}.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region
!          (i.e. {\tt computationalUBound-computationalLBound+1}). 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region
!          (i.e. {\tt totalUBound-totalLBound+1}). 
!     \item[{farray}]
!          The pointer to the coordinate data.
!     \item[{[datacopyflag]}]
!          If not specified, default to {\tt ESMF\_DATACOPY\_REFERENCE}, in this case
!          farray is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{const:datacopyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 type(ESMF_LocalArray) :: larray 
 type(ESMF_DataCopy_Flag) :: datacopyflagInt

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit, locstream, rc) 

  
 ! Set Defaults
 if (present(datacopyflag)) then
    datacopyflagInt=datacopyflag
 else
    datacopyflagInt=ESMF_DATACOPY_REFERENCE
 endif

 ! Get Key Array
 call ESMF_LocStreamGetKeyArray(locstream, keyName=keyName, keyArray=array, rc=localrc)  
 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                         ESMF_CONTEXT, rcToReturn=rc)) return

 
 ! Obtain the native array pointer via the LocalArray interface 
 call ESMF_ArrayGet(array, localDE=localDE, localarray=larray, rc=localrc) 
 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGet(larray, farray, datacopyflag=datacopyflag, rc=localrc) 
 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 

  ! Get Bounds via C++
   call c_ESMC_locstreamgetkeybnds(array, localDE, & 
                 exclusiveLBound, exclusiveUBound, exclusiveCount, &
                 computationalLBound, computationalUBound, computationalCount, &
                 totalLBound, totalUBound, totalCount, &
                 localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetKeyI4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyR4"
!BOPI
 ! !IROUTINE: ESMF_LocStreamGetKey - Get pointer to key values

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
  subroutine ESMF_LocStreamGetKeyR4(locstream, keyName, keywordEnforcer, &
          localDE, exclusiveLBound, exclusiveUBound, exclusiveCount,     &
          computationalLBound, computationalUBound, computationalCount,  &
          totalLBound, totalUBound, totalCount,                          &
          farray, datacopyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream),   intent(in)            :: locstream
      character (len=*),      intent(in)            :: keyName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                intent(in),  optional :: localDE
      integer,                intent(out), optional :: exclusiveLBound
      integer,                intent(out), optional :: exclusiveUBound
      integer,                intent(out), optional :: exclusiveCount
      integer,                intent(out), optional :: computationalLBound
      integer,                intent(out), optional :: computationalUBound
      integer,                intent(out), optional :: computationalCount
      integer,                intent(out), optional :: totalLBound
      integer,                intent(out), optional :: totalUBound
      integer,                intent(out), optional :: totalCount 
      real(ESMF_KIND_R4), pointer :: farray(:)
      type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    key data for a particular key on the given local DE. 
!    This is useful, for example, for setting the key values in a LocStream, or
!    for reading the values. 
!
!     The arguments are:
!     \begin{description}
!     \item[{locstream}]
!          LocStream to get the information from.
!     \item[{keyName}]
!          The key to get the information from.
!     \item[{[localDE]}]
!         The local DE for which information is requested. {\tt [0,..,localDECount-1]}.
!         For {\tt localDECount==1} the {\tt localDE} argument may be omitted,
!          in which case it will default to {\tt localDE=0}.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount}.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region
!          (i.e. {\tt computationalUBound-computationalLBound+1}). 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region
!          (i.e. {\tt totalUBound-totalLBound+1}). 
!     \item[{farray}]
!          The pointer to the coordinate data.
!     \item[{[datacopyflag]}]
!          If not specified, default to {\tt ESMF\_DATACOPY\_REFERENCE}, in this case
!          farray is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{const:datacopyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 type(ESMF_LocalArray) :: larray
 type(ESMF_DataCopy_Flag) :: datacopyflagInt

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit, locstream, rc) 

 
 ! Set Defaults
 if (present(datacopyflag)) then
    datacopyflagInt=datacopyflag
 else
    datacopyflagInt=ESMF_DATACOPY_REFERENCE
 endif

 ! Get Key Array
 call ESMF_LocStreamGetKeyArray(locstream, keyName=keyName, keyArray=array, rc=localrc)  
 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                         ESMF_CONTEXT, rcToReturn=rc)) return

 
 ! Obtain the native array pointer via the LocalArray interface 
 call ESMF_ArrayGet(array, localDE=localDE, localarray=larray, rc=localrc) 
 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
  call ESMF_LocalArrayGet(larray, farray, datacopyflag=datacopyflag, rc=localrc) 
 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 

 !TODO: Add computational Bounds Calc
  ! Get Bounds via C++
   call c_ESMC_locstreamgetkeybnds(array, localDE, & 
                 exclusiveLBound, exclusiveUBound, exclusiveCount, &
                 computationalLBound, computationalUBound, computationalCount, &
                 totalLBound, totalUBound, totalCount, &
                 localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetKeyR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyR8"
!BOPI
! !IROUTINE: ESMF_LocStreamGetKey - Get pointer to key values

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
      subroutine ESMF_LocStreamGetKeyR8(locstream, keyName, keywordEnforcer,&
          localDE, exclusiveLBound, exclusiveUBound, exclusiveCount,     &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,     &
          farray, datacopyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream),   intent(in)            :: locstream
      character (len=*),      intent(in)            :: keyName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                intent(in),  optional :: localDE
      integer,                intent(out), optional :: exclusiveLBound
      integer,                intent(out), optional :: exclusiveUBound
      integer,                intent(out), optional :: exclusiveCount
      integer,                intent(out), optional :: computationalLBound
      integer,                intent(out), optional :: computationalUBound
      integer,                intent(out), optional :: computationalCount
      integer,                intent(out), optional :: totalLBound
      integer,                intent(out), optional :: totalUBound
      integer,                intent(out), optional :: totalCount
      real(ESMF_KIND_R8), pointer :: farray(:)
      type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!    This method gets a Fortran pointer to the piece of memory which holds the 
!    key data for a particular key on the given local DE. 
!    This is useful, for example, for setting the key values in a LocStream, or
!    for reading the values. 
!
!     The arguments are:
!     \begin{description}
!     \item[{locstream}]
!          LocStream to get the information from.
!     \item[{keyName}]
!          The key to get the information from.
!     \item[{[localDE]}]
!         The local DE for which information is requested. {\tt [0,..,localDECount-1]}.
!         For {\tt localDECount==1} the {\tt localDE} argument may be omitted,
!          in which case it will default to {\tt localDE=0}.
!     \item[{[exclusiveLBound]}]
!          Upon return this holds the lower bounds of the exclusive region.
!     \item[{[exclusiveUBound]}]
!          Upon return this holds the upper bounds of the exclusive region.
!     \item[{[exclusiveCount]}]
!          Upon return this holds the number of items in the exclusive region
!          (i.e. {\tt exclusiveUBound-exclusiveLBound+1}). {\tt exclusiveCount}.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region.
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the computational region.
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region
!          (i.e. {\tt computationalUBound-computationalLBound+1}). 
!     \item[{[totalLBound]}]
!          Upon return this holds the lower bounds of the total region.
!     \item[{[totalUBound]}]
!          Upon return this holds the upper bounds of the total region.
!     \item[{[totalCount]}]
!          Upon return this holds the number of items in the total region
!          (i.e. {\tt totalUBound-totalLBound+1}). 
 !     \item[{farray}]
!          The pointer to the coordinate data.
!     \item[{[datacopyflag]}]
!          If not specified, default to {\tt ESMF\_DATACOPY\_REFERENCE}, in this case
!          farray is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{const:datacopyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 type(ESMF_LocalArray) :: larray
 type(ESMF_DataCopy_Flag) :: datacopyflagInt

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 



 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit, locstream, rc) 

 
 ! Set Defaults
 if (present(datacopyflag)) then
    datacopyflagInt=datacopyflag
 else
    datacopyflagInt=ESMF_DATACOPY_REFERENCE
 endif

 ! Get Key Array
 call ESMF_LocStreamGetKeyArray(locstream, keyName=keyName, keyArray=array, rc=localrc)  

 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                         ESMF_CONTEXT, rcToReturn=rc)) return
 

 ! Obtain the native array pointer via the LocalArray interface 
 call ESMF_ArrayGet(array, localDE=localDE, localarray=larray, rc=localrc) 
 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGet(larray, farray, datacopyflag=datacopyflag, rc=localrc) 
 if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 

  ! Get Bounds via C++
   call c_ESMC_locstreamgetkeybnds(array, localDE, & 
                 exclusiveLBound, exclusiveUBound, exclusiveCount, &
                 computationalLBound, computationalUBound, computationalCount, &
                 totalLBound, totalUBound, totalCount, &
                 localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return


 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetKeyR8


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamMatch()"
!BOPI
! !IROUTINE: ESMF_LocStreamMatch - Check if two Locstream objects match

! !INTERFACE:
  function ESMF_LocStreamMatch(locstream1, locstream2, keywordEnforcer, rc)
!
! !RETURN VALUE:
    logical :: ESMF_LocStreamMatch
      
! !ARGUMENTS:
    type(ESMF_Locstream),  intent(in)              :: locstream1
    type(ESMF_Locstream),  intent(in)              :: locstream2
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,               intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Check if {\tt locstream1} and {\tt locstream2} match. Returns
!      .true. if Locstream objects match, .false. otherwise. This
!      method current just checks if locstream1 and locstream2 are the
!      same object, future work will do a more complex check.
!
!     The arguments are:
!     \begin{description}
 !     \item[locstream1] 
!          {\tt ESMF\_Locstream} object.
!     \item[locstream2] 
!          {\tt ESMF\_Locstream} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer      :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! init to one setting in case of error
    ESMF_LocStreamMatch = .false.
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocstreamGetInit, locstream1, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_LocstreamGetInit, locstream2, rc)
    
    ! make sure the locstreams point to the same thing
    if (associated(locstream1%lstypep,locstream2%lstypep)) then
       ESMF_LocStreamMatch = .true.
    else
       ESMF_LocStreamMatch = .false.
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end function ESMF_LocStreamMatch
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamIsCreated()"
!BOP
! !IROUTINE: ESMF_LocStreamIsCreated - Check whether a LocStream object has been created
 
! !INTERFACE:
  function ESMF_LocStreamIsCreated(locstream, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_LocStreamIsCreated
!
! !ARGUMENTS:
    type(ESMF_LocStream), intent(in)            :: locstream
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt locstream} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[locstream]
!     {\tt ESMF\_LocStream} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_LocStreamIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_LocStreamGetInit(locstream)==ESMF_INIT_CREATED) &
      ESMF_LocStreamIsCreated = .true.
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamPrint"

!BOP
! !IROUTINE:  ESMF_LocStreamPrint - Print the contents of a LocStream

! !INTERFACE:
      subroutine ESMF_LocStreamPrint(locstream, keywordEnforcer, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in)             :: locstream 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character (len = *),  intent(in),   optional :: options
      integer,              intent(out),  optional :: rc
!
! !DESCRIPTION:
!     Prints information about the {\tt locstream} to {\tt stdout}.
!  This subroutine goes through the internal data members of a locstream
 !  data type and prints information of each data member.
!
!     The arguments are:
!     \begin{description}
!     \item [locstream]
!     \item [{[options]}]
!           Print options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        type(ESMF_LocStreamType),pointer :: lstypep
        character(len=ESMF_MAXSTR)       :: name
        integer                          :: localrc
        character(len=6)                 :: defaultopts
        integer                          :: i
        type(ESMF_TypeKind_Flag)              :: keyKind
        real(ESMF_KIND_R8), pointer :: tmpR8(:)
        real(ESMF_KIND_R4), pointer :: tmpR4(:)
        integer(ESMF_KIND_I4), pointer :: tmpI4(:)
        integer                          :: cl,cu,j

        ! Initialize
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

        !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
        !TODO: Remove the following dummy test when dummy argument actually used
        if (present (options)) continue

        write(ESMF_UtilIOStdout,*) "LocStream Print Starts ====>"

        ! Get internal pointer to locstream
        lstypep => locstream%lstypep

        ! print option is not implemented, but it has to pass to c_ESMC_BasePrint()
        defaultopts = "brief"

        call ESMF_GetName(lstypep%base, name, localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

        write(ESMF_UtilIOStdout,*)  "Name =     '",  trim(name), "'" 
        write(ESMF_UtilIOStdout,*)  "KeyCount =",lstypep%keyCount
        write(ESMF_UtilIOStdout,*) "Keys:"
        do i=1,lstypep%keyCount
           write(ESMF_UtilIOStdout,*) "   ",trim(lstypep%keyNames(i)),     &
                                      " - ",trim(lstypep%keyLongNames(i)), &
                                      "    ",trim(lstypep%keyUnits(i))


          call ESMF_LocStreamGetKey(locstream,lstypep%keyNames(i),typekind=keyKind,rc=localrc)
          if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

          if (keyKind .eq. ESMF_TYPEKIND_I4) then
            call  ESMF_LocStreamGetKey(locstream, keyName=lstypep%keyNames(i), &
              computationalLBound=cl, computationalUBound=cu, farray=tmpI4, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
            do j=cl,cu
              write(ESMF_UtilIOStdout,*) "    arr(",j,")= ",tmpI4(j)
            enddo
          else if (keyKind .eq. ESMF_TYPEKIND_R4) then
            call  ESMF_LocStreamGetKey(locstream, keyName=lstypep%keyNames(i), &
              computationalLBound=cl, computationalUBound=cu, farray=tmpR4, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
            do j=cl,cu
              write(ESMF_UtilIOStdout,*) "    arr(",j,")= ",tmpR4(j)
            enddo
          else if (keyKind .eq. ESMF_TYPEKIND_R8) then
            call  ESMF_LocStreamGetKey(locstream, keyName=lstypep%keyNames(i), &
              computationalLBound=cl, computationalUBound=cu, farray=tmpR8, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rcToReturn=rc)) return
            do j=cl,cu
              write(ESMF_UtilIOStdout,*) "    arr(",j,")= ",tmpR8(j)
            enddo
          else
            if (ESMF_LogFoundError(ESMF_RC_ARG_WRONG, &
              msg=" - unknown typekind for LocStream key", &
              ESMF_CONTEXT, rcToReturn=rc)) return
          endif
        enddo

        write(ESMF_UtilIOStdout,*) "LocStream Print Ends   ====>"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_LocStreamPrint


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamSerialize"

!BOPI
! !IROUTINE: ESMF_LocStreamSerialize - Serialize locstream info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_LocStreamSerialize(locstream, buffer, length, offset, inquireflag, rc) 
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(inout) :: locstream
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      type(ESMF_InquireFlag), intent(in), optional :: inquireflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_Grid} object and adds all the information needed
!      to  recreate the object based on this information.  
!      Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [locstream]
!           {\tt ESMF\_LocStream} object to be serialized.
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
!     \item [inquireflag]
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      type(ESMF_LocStreamType),pointer :: lstypep
      integer :: i,localrc
      type(ESMF_AttReconcileFlag) :: attreconflag
      type(ESMF_InquireFlag) :: linquireflag

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

      if (present (inquireflag)) then
        linquireflag = inquireflag
      else
        linquireflag = ESMF_NOINQUIRE
      end if

      ! Get internal pointer to locstream type
      lstypep => locstream%lstypep

     ! Serialize Base
     attreconflag = ESMF_ATTRECONCILE_OFF
     call c_ESMC_BaseSerialize(lstypep%base, buffer, length, offset, &
      attreconflag, linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

     ! Serialize Distgrid
     call c_ESMC_DistgridSerialize(lstypep%distgrid, buffer, length, offset, &
                                 linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      ! Serialize other locstream items
      call c_ESMC_LocStreamSerialize(lstypep%indexflag, lstypep%keyCount, &
              buffer, length, offset, linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      ! Serialize locstream key info
      do i=1,lstypep%keyCount
         ! Serialize key info
         call c_ESMC_LocStreamKeySerialize(&
                  len_trim(lstypep%keyNames(i)), lstypep%keyNames(i), &
                  len_trim(lstypep%keyUnits(i)), lstypep%keyUnits(i), &
                  len_trim(lstypep%keyLongNames(i)), lstypep%keyLongNames(i), &
                 buffer, length, offset, linquireflag, localrc)
         if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

         ! Serialize key Array
         call c_ESMC_ArraySerialize(lstypep%keys(i), buffer, length, offset, &
          attreconflag, linquireflag, localrc)
         if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      enddo

      ! return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LocStreamSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamDeserialize"

!BOPI
! !IROUTINE: ESMF_LocStreamDeserialize - Deserialize a byte stream into a LocStream
!
! !INTERFACE:
      function ESMF_LocStreamDeserialize(buffer, offset, rc) 
!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamDeserialize   
!
! !ARGUMENTS:
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a LocStream object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc
      type(ESMF_LocStreamType),pointer :: lstypep
      integer :: i
      type(ESMF_AttReconcileFlag) :: attreconflag

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if  (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! allocate LocStream type
      allocate(lstypep, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Allocating LocStream type object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

     ! Deserialize Base
     attreconflag = ESMF_ATTRECONCILE_OFF
     call c_ESMC_BaseDeserialize(lstypep%base, buffer,  offset, &
      attreconflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_BaseSetInitCreated(lstypep%base, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return


     ! Deserialize Distgrid
     call c_ESMC_DistGridDeserialize(lstypep%distgrid, buffer, offset, localrc)
     if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_DistGridSetInitCreated(lstypep%distgrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return


      ! Deserialize other locstream items
      call c_ESMC_LocStreamDeserialize(lstypep%indexflag, lstypep%keyCount, &
              buffer, offset, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return


      ! Allocate arrays for names, etc.
      allocate (lstypep%keyNames(lstypep%keyCount), stat=localrc )
      if (ESMF_LogFoundAllocError(localrc, msg=" Allocating KeyNames", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      allocate (lstypep%keyUnits(lstypep%keyCount), stat=localrc )
      if (ESMF_LogFoundAllocError(localrc, msg=" Allocating units", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      allocate (lstypep%keyLongNames(lstypep%keyCount), stat=localrc )
      if (ESMF_LogFoundAllocError(localrc, msg=" Allocating longNames", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      allocate( lstypep%keys(lstypep%keyCount), stat=localrc )  ! Array of keys
      if (ESMF_LogFoundAllocError(localrc, msg=" Allocating keys", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      allocate( lstypep%destroyKeys(lstypep%keyCount), stat=localrc )  ! Array of keys
      if (ESMF_LogFoundAllocError(localrc, msg=" Allocating keys", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

      ! Serialize locstream key info
      do i=1,lstypep%keyCount
         ! Deserialize key info
         call c_ESMC_LocStreamKeyDeserialize(&
                  lstypep%keyNames(i), &
                  lstypep%keyUnits(i), &
                  lstypep%keyLongNames(i), &
                 buffer, offset, localrc)
         if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

         ! Deserialize key Array
         call c_ESMC_ArrayDeserialize(lstypep%keys(i), buffer, offset, &
          attreconflag, localrc)
         if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

         call ESMF_ArraySetInitCreated(lstypep%keys(i), rc=localrc)
         if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      enddo


     ! Set to destroy proxy objects
     lstypep%destroyDistgrid=.true.
     lstypep%destroyKeys=.true.

     ! Set pointer to locstream
     ESMF_LocStreamDeserialize%lstypep=>lstypep

     ! Add reference to this object into ESMF garbage collection table
     ! Only call this in those Create() methods that do not call other LSCreate()
     call c_ESMC_VMAddFObject(ESMF_LocStreamDeserialize, &
       ESMF_ID_LOCSTREAM%objectID)
        
     ! Set init status
     ESMF_INIT_SET_CREATED(ESMF_LocStreamDeserialize)

     if  (present(rc)) rc = ESMF_SUCCESS

     end function ESMF_LocStreamDeserialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamValidate"

!BOP
! !IROUTINE:  ESMF_LocStreamValidate - Check validity of a LocStream

! !INTERFACE:
      subroutine ESMF_LocStreamValidate(locstream, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in)            :: locstream 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,              intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Validates that the {\tt locstream} is internally consistent.
!      Currently this method determines if the {\tt locstream} is uninitialized 
!      or already destroyed. 
!
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item [locstream]
!           {\tt ESMF\_LocStream} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt locstream} 
!           is valid.
!     \end{description}
!
!EOP

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

      ! return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LocStreamValidate

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all LocStream internal methods.
!
!------------------------------------------------------------------------------

!----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamTypeGetInit"
!BOPI
! !IROUTINE:  ESMF_LocStreamTypeGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_LocStreamTypeGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_LocStreamType), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_LocStreamTypeGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt locstreamtype}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocStreamType} from which to retrieve status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_LocStreamTypeGetInit = ESMF_INIT_GET(s)
       else
         ESMF_LocStreamTypeGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_LocStreamTypeGetInit


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamTypeInit"
!BOPI
! !IROUTINE:  ESMF_LocStreamTypeInit - Initialize LocStreamType

! !INTERFACE:
    subroutine ESMF_LocStreamTypeInit(s)
!
! !ARGUMENTS:
       type(ESMF_LocStreamType) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt locstreamtype}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocStreamType} of which being initialized.
!     \end{description}
!
!EOPI

        ! TODO: more here once the structure is settled

        ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_LocStreamTypeInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamTypeValidate"
!BOPI
! !IROUTINE:  ESMF_LocStreamTypeValidate - Check validity of a LocStreamType

! !INTERFACE:
    subroutine ESMF_LocStreamTypeValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_LocStreamType), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt LocStreamType} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocStreamType} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt s}
!           is valid.
!     \end{description}
!
!EOPI

     ESMF_INIT_CHECK_SET_SHALLOW(ESMF_LocStreamTypeGetInit,ESMF_LocStreamTypeInit,s)

     !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
     !TODO: Remove the following dummy test when dummy argument actually used
     if (s%keycount==s%keycount) continue

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_LocStreamTypeValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetInit"
!BOPI
! !IROUTINE:  ESMF_LocStreamGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_LocStreamGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_LocStream), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_LocStreamGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt locstream}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocStream} from which to retrieve status.
!     \end{description}
!
!EOPI

       if (present(d)) then
         ESMF_LocStreamGetInit = ESMF_INIT_GET(d)
       else
         ESMF_LocStreamGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_LocStreamGetInit





!------------------------------------------------------------------------------
#undef  ESMF_METHOD  
#define ESMF_METHOD "ESMF_LocStreamCreatePetList"
!BOPI
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream by shifting to other pets

! !INTERFACE:
      function ESMF_LocStreamCreatePetList(locstream, name, petList, rc)

!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreatePetList

!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in)                :: locstream
      character (len=*),    intent(in), optional      :: name
      integer,              intent(in)                :: petList(:)
      integer,              intent(out), optional     :: rc
!
! !DESCRIPTION:
!
!     Create an location stream from an existing one by moving entries 
!     between pets according to petList. Currently this is an internal 
!     subroutine not intended for public use. 
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream from which the new location stream is to be created
!      \item[{[name]}]
!          Name of the resulting location stream
!      \item[petList]
!          Local list with the same number of entries as the number of entries on 
!          this Pet. Entries tell which pet to move the entry to an entry <0 says
!          to not put the corresponding locstream entry into the new locstream.
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      type(ESMF_LocStreamType), pointer :: oldLStypep, newLStypep
      type(ESMF_DistGrid) :: newDistGrid
      type(ESMF_LocStream):: newLocStream
      type(ESMF_VM) :: vm
      type(ESMF_ArrayBundle) :: oldAB, newAB
      type(ESMF_RouteHandle) :: routehandle
      type(ESMF_TypeKind_Flag) ::keyTypeKind
      character(len=ESMF_MAXSTR)    :: keytemp, string
      integer :: keyCount,i
      integer :: localrc
      integer :: lDE, localDECount
      integer :: pos
      integer, pointer :: seqInd(:)
      integer :: seqCount, localPet, petCount
      integer, pointer :: sndCounts(:),sndOffsets(:)
      integer, pointer :: rcvCounts(:),rcvOffsets(:)
      integer, pointer :: sndSizes(:)
      integer, pointer :: rcvSizes(:)
      integer, pointer :: sndPos(:)
      integer :: newCount,tmp,petListCount, petInd
      integer, pointer :: sndSeqInd(:), rcvSeqInd(:) 


! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Check Variables
      ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

     ! get size of petList
     petListCount=size(petList)

     ! Get current VM
     call ESMF_VMGetCurrent(vm, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
         ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
         ESMF_CONTEXT, rcToReturn=rc)) return


     ! Allocate stuff for AllToAllV 
     allocate(sndCounts(petCount), stat=localrc)
     if (ESMF_LogFoundAllocError(localrc, msg="Allocating sndCounts", &
         ESMF_CONTEXT, rcToReturn=rc)) return         

     allocate(sndOffsets(petCount), stat=localrc)
     if (ESMF_LogFoundAllocError(localrc, msg="Allocating sndOffsets", &
         ESMF_CONTEXT, rcToReturn=rc)) return         

     allocate(rcvCounts(petCount), stat=localrc)
     if (ESMF_LogFoundAllocError(localrc, msg="Allocating rcvCounts", &
         ESMF_CONTEXT, rcToReturn=rc)) return         

     allocate(rcvOffsets(petCount), stat=localrc)
     if (ESMF_LogFoundAllocError(localrc, msg="Allocating sndCounts", &
         ESMF_CONTEXT, rcToReturn=rc)) return         


     ! Allocate first set of buffers for commmunicating sizes
     allocate(sndSizes(petCount), stat=localrc)
     if (ESMF_LogFoundAllocError(localrc, msg="Allocating sndCounts", &
         ESMF_CONTEXT, rcToReturn=rc)) return         

     allocate(rcvSizes(petCount), stat=localrc)
     if (ESMF_LogFoundAllocError(localrc, msg="Allocating sndCounts", &
         ESMF_CONTEXT, rcToReturn=rc)) return         


     ! Count the number going to each Pet
     sndSizes=0
     do i=1,petListCount
        petInd=petList(i)+1
        if (petInd > 0) then
           sndSizes(petInd)=sndSizes(petInd)+1
        endif
     enddo

     ! Number being sent
     sndCounts=1

     ! Offset being sent
     do i=1,petCount
        sndOffsets(i)=i-1
     enddo

     ! Number being sent
     rcvCounts=1

     ! Offset being sent
     do i=1,petCount
        rcvOffsets(i)=i-1
     enddo
     

     ! Communicate sizes being sent
     call ESMF_VMAllToAllV(vm, sndSizes, sndCounts, sndOffsets, &
            rcvSizes, rcvCounts, rcvOffsets, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
         ESMF_CONTEXT, rcToReturn=rc)) return


     ! Get sizes after communication
     sndCounts=sndSizes
     rcvCounts=rcvSizes


     ! Deallocate first set of buffers for commmunicating sizes
     deallocate(sndSizes)
     deallocate(rcvSizes)

    
     ! Get old locstream internal pointer
     oldLStypep=>locstream%lstypep     

     ! Allocate space for seqInd
     allocate(seqInd(petListCount), stat=localrc)
     if (ESMF_LogFoundAllocError(localrc, msg="Allocating seqInd", &
       ESMF_CONTEXT, rcToReturn=rc)) return         


     ! Get number of localDEs
     call ESMF_LocStreamGet(locstream, localDECount=localDECount, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
         ESMF_CONTEXT, rcToReturn=rc)) return


     ! Loop getting seqIndices
     pos=1
     do lDE=0,localDECount-1

        ! Get number of seqIndices
        call  ESMF_DistGridGet(oldLStypep%distgrid, localDe=lDE, &
                elementCount=seqCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return


        ! Make sure we aren't going to overrun memory
        if ((pos+seqCount-1) >petListCount) then
          if (ESMF_LogFoundError(ESMF_RC_ARG_WRONG, &
                 msg=" - Too many seq indices in locstream disgrid", &
                ESMF_CONTEXT, rcToReturn=rc)) return               
        endif

        ! Get list of seqindices
        call  ESMF_DistGridGet(oldLStypep%distgrid, localDe=lDE, &
                seqIndexList=seqInd(pos:pos+seqCount-1), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return

        ! advance to next set of positions
        pos=pos+seqCount
     enddo


     ! Calculate Snd Offsets
     sndOffsets(1)=0
     do i=2,petCount
        sndOffsets(i)=sndOffsets(i-1)+sndCounts(i-1)
     enddo

     ! Calculate Rcv Offsets
     rcvOffsets(1)=0
     do i=2,petCount
        rcvOffsets(i)=rcvOffsets(i-1)+rcvCounts(i-1)
     enddo


     ! Allocate postions for seqInd data
     allocate(sndPos(petCount), stat=localrc)
     if (ESMF_LogFoundAllocError(localrc, msg="Allocating sndCounts", &
         ESMF_CONTEXT, rcToReturn=rc)) return         

     ! Allocate sndSeqInd
     allocate(sndSeqInd(petListCount), stat=localrc)
     if (ESMF_LogFoundAllocError(localrc, msg="Allocating sndSeqInd", &
       ESMF_CONTEXT, rcToReturn=rc)) return         


     ! Reset positions
     sndPos=sndOffsets
     
     ! Copy seq indices into send buffer in correct order
     do i=1,petListCount
        petInd=petList(i)+1
        if (petInd > 0) then
           sndSeqInd(sndPos(petInd)+1)=seqInd(i)
           sndPos(petInd)=sndPos(petInd)+1
        endif
     enddo


     ! deallocate seqInd
     deallocate(seqInd)

     ! deallocate sndPos
     deallocate(sndPos)


     ! Total size coming in
     newCount=0
     do i=1,petCount
        newCount=newCount+rcvCounts(i)
     enddo

     ! Allocate rcvSeqInd
     allocate(rcvSeqInd(newCount), stat=localrc)
     if (ESMF_LogFoundAllocError(localrc, msg="Allocating rcvSeqInd", &
       ESMF_CONTEXT, rcToReturn=rc)) return         



     ! Communicate sequence indices
     call ESMF_VMAllToAllV(vm, sndSeqInd, sndCounts, sndOffsets, &
            rcvSeqInd, rcvCounts, rcvOffsets, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
              ESMF_CONTEXT, rcToReturn=rc)) return


     ! Deallocate stuff for AllToAllV 
     deallocate(sndCounts)
     deallocate(sndOffsets)
     deallocate(rcvCounts)
     deallocate(rcvOffsets)

     ! Deallocate sndSeqInd
     deallocate(sndSeqInd)


     ! Create the new distgrid
     newDistGrid=ESMF_DistGridCreate(arbSeqIndexList=rcvSeqInd, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return


     ! Deallocate sndSeqInd
     deallocate(rcvSeqInd)


     ! Create new locStream
     ESMF_LocStreamCreatePetList=ESMF_LocStreamCreateFromNewDG(locstream, &
                   distgrid=newDistgrid, name=name, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return     

      ! Set distgrid to be destroyed, since ESMF created it
      ESMF_LocStreamCreatePetList%lstypep%destroyDistgrid=.true.

     ! Return success
     if (present(rc)) rc = ESMF_SUCCESS

     end function ESMF_LocStreamCreatePetList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetNumLocal"
!BOPI
! !IROUTINE: ESMF_LocStreamGetNumLocal - Get number of local entries in locstream

! !INTERFACE:
      subroutine ESMF_LocStreamGetNumLocal(locstream, localCount, rc)

!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in)                :: locstream
      integer,              intent(out)               :: localCount
      integer,              intent(out),  optional    :: rc
!
! !DESCRIPTION:
!    Get number of local entries.
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream from which the new location stream is to be created
!      \item[localCount]
!          Number of local entries.
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      type(ESMF_LocStreamType), pointer :: lstypep
      type(ESMF_DistGrid) :: distgrid
      integer :: localrc
      integer :: localDECount, lDE
      integer :: tmpLBnd, tmpUBnd

! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Check Variables
      ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

      
      ! Get number of localDEs
      call ESMF_LocStreamGet(locstream, localDECount=localDECount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

      ! Get locstream internal pointer
      lstypep=>locstream%lstypep


      ! Loop through DEs and total up number of entries
      localCount=0
      do lDE=0,localDECount-1
         call c_ESMC_locstreamgetelbnd(lstypep%distgrid, lDE, lstypep%indexflag, & 
                 tmpLBnd, localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
               ESMF_CONTEXT, rcToReturn=rc)) return

         call c_ESMC_locstreamgeteubnd(lstypep%distgrid, lDE, lstypep%indexflag, & 
                 tmpUBnd, localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
               ESMF_CONTEXT, rcToReturn=rc)) return

         localCount = localCount + (tmpUBnd-tmpLBnd+1)
      enddo


      ! return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LocStreamGetNumLocal


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetPnts"
!BOPI
! !IROUTINE: ESMF_LocStreamGetPnts - Get list of points from locstream

! !INTERFACE:
      subroutine ESMF_LocStreamGetPntList(locstream, coordKeyNames, pntDim, pntCount, pntList, rc)

!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in)                :: locstream
      character (len=*),    intent(in)                :: coordKeyNames
      integer,              intent(in)               :: pntDim
      integer,              intent(in)               :: pntCount
      real(ESMF_KIND_R8),  dimension(:), intent(out)  :: pntList
      integer,              intent(out),  optional    :: rc
!
! !DESCRIPTION:
!    Get number of local entries.
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream from which the new location stream is to be created
!      \item[coordKeyNames]
!          Names of the keys used to determine the link to background Mesh.
!          The first key in this list matches up with the first coordinate of the 
!          Mesh, the second key in this list matches up with the second coordinate
!          of the Mesh, and so on. The key names should be separated by the : character. 
!      \item[pntList]
!          Local list of points from locstream based on coordKeyNames
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      type(ESMF_LocStreamType), pointer :: lstypep
      type(ESMF_DistGrid) :: distgrid
      integer :: localrc
      integer :: localDECount, lDE
      integer :: tmpLBnd, tmpUBnd
      character(len=ESMF_MAXSTR)    :: string
      character(len=ESMF_MAXSTR)    :: coordKeyList(3)
      type(ESMF_TypeKind_Flag)           :: coordTypeKindList(3)
      real(ESMF_KIND_R8), pointer :: keyDataR8(:)
      real(ESMF_KIND_R4), pointer :: keyDataR4(:)
      integer (ESMF_KIND_I4), pointer :: keyDataI4(:)
      integer :: dim
      integer :: i,j,pos
      type(ESMF_TypeKind_Flag) :: typekind

! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Check Variables
      ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

      ! Get keynames
      ! Calculate pntDim 
       dim = 1
       string = trim(coordKeyNames )
       do while ( string /= '' )
          ! make sure that we aren't overwriting our array
          if (dim >3) then
            if (ESMF_LogFoundError(ESMF_RC_ARG_WRONG, &
                 msg=" - too many coordinate key names", &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif

          ! Pull out coordinate name
          call ESMF_StripKey( string, coordKeyList(dim))

          ! advance to next position
          dim = dim + 1
       enddo

       ! check pntDim
       if (pntDim /= dim-1) then
         if (ESMF_LogFoundError(ESMF_RC_ARG_WRONG, &
           msg=" - number of coordinate key names doesn't match pnt dimension", &
           ESMF_CONTEXT, rcToReturn=rc)) return
       endif

      ! Get number of localDEs
      call ESMF_LocStreamGet(locstream, localDECount=localDECount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return


     ! Get pnt coordinates
     !! TODO: this is doing a couple of internal subroutine searches to match the key with the name
     !!       at some point it might make sense to get rid of these
     do i=1,pntDim

       ! Get typeKind
       call ESMF_LocStreamGetKey(locstream,keyName=trim(coordKeyList(i)), typekind=typekind, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return


        ! Copy data based on typekind
        if (typekind .eq. ESMF_TYPEKIND_R8) then
           pos=i
           do lDE=0,localDECount-1
              ! Get data
              call  ESMF_LocStreamGetKey(locstream, localDE=lDE, keyName=trim(coordKeyList(i)), &
                      exclusiveLBound=tmpLBnd, exclusiveUBound=tmpUBnd, farray=keyDataR8, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                  ESMF_CONTEXT, rcToReturn=rc)) return

              ! put into point list
              do j=tmpLBnd,tmpUBnd
                 pntList(pos)=keyDataR8(j)
                 pos=pos+pntDim
              enddo
           enddo
        else if (typekind .eq. ESMF_TYPEKIND_R4) then
           pos=i
           do lDE=0,localDECount-1
              ! Get data
              call  ESMF_LocStreamGetKey(locstream, localDE=lDE, keyName=trim(coordKeyList(i)), &
                      exclusiveLBound=tmpLBnd, exclusiveUBound=tmpUBnd, farray=keyDataR4, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                  ESMF_CONTEXT, rcToReturn=rc)) return

              ! put into point list
              do j=tmpLBnd,tmpUBnd
                 pntList(pos)=REAL(keyDataR4(j),ESMF_KIND_R8)
                 pos=pos+pntDim
              enddo
           enddo

        else if (typekind .eq. ESMF_TYPEKIND_I4) then
           pos=i
           do lDE=0,localDECount-1
              ! Get data
              call  ESMF_LocStreamGetKey(locstream, localDE=lDE, keyName=trim(coordKeyList(i)), &
                      exclusiveLBound=tmpLBnd, exclusiveUBound=tmpUBnd, farray=keyDataI4, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                  ESMF_CONTEXT, rcToReturn=rc)) return

              ! put into point list
              do j=tmpLBnd,tmpUBnd
                 pntList(pos)=REAL(keyDataI4(j),ESMF_KIND_R8)
                 pos=pos+pntDim
              enddo
           enddo

        else 
          if (ESMF_LogFoundError(ESMF_RC_ARG_WRONG, &
           msg=" - unsupported coordinate data type", &
           ESMF_CONTEXT, rcToReturn=rc)) return
       endif
     enddo
     
      ! return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LocStreamGetPntList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StripKey"
!BOPI
! !IROUTINE: ESMF_StripKey - Attach a real8 key
!
! !INTERFACE:
      subroutine ESMF_StripKey(string, key)
!
! !ARGUMENTS:
      character (len = *), intent(inout)  :: string
      character (len = *), intent(out)    :: key

!
! !DESCRIPTION:
!
!     Pulls out the key which occurring at position
!     occurrence in the colon-separated string, and 
!     return the truncated string.
!
! !REQUIREMENTS:
!EOPI

      integer   :: pos

      pos = index( string, ',')
      if ( pos == 0 ) then
        key = string
        string = ''
      else        
        key = string(1:pos-1)
        string = string( pos+1: )
      endif

      return
      end subroutine ESMF_StripKey
!------------------------------------------------------------------------------



#ifdef FUTURE_LOCSTREAM_WORK
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreateCopy"
!BOPI
! !IROUTINE: ESMF_LocStreamCreate - Create a loc stream from an existing one

! !INTERFACE:
      ! NOT YET INTEGRATED
      ! Private name; call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateCopy(locstream, name, rc)

!
! !RETURN VALUE:
      type (ESMF_LocStream) :: ESMF_LocStreamCreateCopy

!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in)      :: locstream
      character (len=*), intent(in)         :: name
      integer, intent(out), optional        :: rc
!
! !DESCRIPTION:
!
!     Create a new location stream based on the information in an
!     existing one.  The contents of the stream, in particular the
!     keys, and the decomposition are simply adopted.
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream to be copied
!      \item[name]
!          Name of the new location stream
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      type(ESMF_LocStreamClass), pointer      :: igrid       ! Pointer to new grid
      type(ESMF_LocStreamSpecific), pointer   :: igridSpecific
      type(ESMF_LocStream), pointer       :: ls
      type(ESMF_LocStream), pointer       :: lsIn
      type(ESMF_DELayout)                 :: delayout
      type(ESMF_InternDG)                 :: internDG
      type(ESMF_Pointer)                  :: ptr

      integer :: k
      integer :: localrc                                ! Error status
      integer :: rank

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      if (igridIn%ptr%horzLocStreamType /= ESMF_IGRID_TYPE_LOCATIONSTREAM ) then
        if ( ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                   msg="LocStream object not location stream", &
                                   ESMF_CONTEXT, rcToReturn=rc) ) return
        return
      endif

! check grid status
      if (igridIn%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite(msg="trying to copy an uninitialized igrid", &
                           ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Initialize pointers
      nullify(igrid)
      nullify(ESMF_LocStreamCreateLocStreamCopy%ptr)


      allocate(igrid, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, "Allocating LocStream object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call construction method to allocate and initialize igrid internals.
      call ESMF_LocStreamConstructNew(igrid, name, localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_InternDGGetDELayout(igridIn%ptr%internDGs(1)%ptr, delayout, &
                                    localrc)

      !
      ! For the time being, just fill in the LocStreamClass variables
      igrid%igridStructure             = igridIn%ptr%igridStructure
      igrid%horzIgridType              = igridIn%ptr%horzLocStreamType
      igrid%horzStagger                = igridIn%ptr%horzStagger
      igrid%coordOrder                 = igridIn%ptr%coordOrder
      igrid%dimCount                   = 1   ! By definition
      igrid%minGlobalCoordPerDim(1)    = igridIn%ptr%minGlobalCoordPerDim(1)
      igrid%maxGlobalCoordPerDim(1)    = igridIn%ptr%maxGlobalCoordPerDim(1)

!
!     To bind with ESMF:  allocate the specific grid, bind to ls


      lsIn => igridIn%ptr%igridSpecific%locStream
      igrid%igridSpecific%locStream => ESMF_LSConstructCopy(lsIn,localrc)
      if (ESMF_LogFoundAllocError(localrc, "locStream",              &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      ls => igrid%igridSpecific%locStream

!
! Attach keys
!
      do k=1, lsIn%keyCount
        call ESMF_LocalArrayGetThis( lsIn%keys(k), ptr )
        if ( ptr /= ESMF_NULL_POINTER ) then
          ls%keys(k) = ESMF_LocalArrayCreate( lsIn%keys(k), localrc )
          if (ESMF_LogFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rcToReturn=rc)) return
        endif
      enddo

!
! Copy over the key decomposition
!
      allocate( ls%dist(size(lsIn%dist)) )
      ls%dist = lsIn%dist

! Determine the internal DG (later just DistGrid)

      internDG = ESMF_InternDGCreate( 1, (/sum(ls%dist)/), delayout, &
                                      (/1/), ls%dist, rc=localrc )
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      ! now that it's created, add the interndg to the igrid
      call ESMF_LocStreamAddInternDG(igrid, internDG, localrc)

      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      igrid%igridStorage = ESMF_IGRID_STORAGE_ARBITRARY
      igrid%igridStatus  = ESMF_IGRID_STATUS_READY

      ! Set return values.
      ESMF_LocStreamCreateLocStreamCopy%ptr => igrid

      ESMF_INIT_SET_CREATED(ESMF_LocStreamCreateLocStreamCopy)
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateCopy
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreateSubset"
!BOPI
! !IROUTINE: ESMF_LocStreamCreate - Create a subset of an existing LocStream

! !INTERFACE:
      ! NOT YET INTEGRATED
      ! Private name; call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateSubset(locstream, name, keyNames, &
                 ranges, values, complement, nsel, rc)
!
! !RETURN VALUE:
      type (ESMF_LocStream) :: ESMF_LocStreamCreateSubset

!
! !ARGUMENTS:
      type(ESMF_LocStream),  intent(in)            :: locstream
      character (len=*),     intent(in)            :: name
      character (len=*),     intent(in)            :: keyNames
      type(ESMF_LocalArray), intent(in),  optional :: ranges(:)
      type(ESMF_LocalArray), intent(in),  optional :: values(:)
      logical,               intent(in),  optional :: complement
      integer,               intent(out), optional :: nsel
      integer,               intent(out), optioanl :: rc

!
! !DESCRIPTION:
!
!     Create a new location stream based on a subset of an existing one.  
!     The contents of the stream, in particular the keys, and the decomposition
!     are simply adopted.  This operation is communication-free.
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream from which the new location stream is to be created. 
!      \item[name]
!          Name of the new location stream.
!      \item[keyNames]
!          Names of the keys used to determine the subset
!      \item[{[ranges]}]
!          Ranges which fulfill the criteria (lbound,ubound)
!      \item[{[values]}]
!          Values which fulfill the criteria (val(1), val(2), .. val(n))
!      \item[{[complement]}]
!          If present, take the complementary set 
!      \item[{[nsel]}]
!          The number of chosen locations on this DE
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!     This routine selects from the location stream according to a given
!     set of conditions, specified in terms of values and/or enumerated 
!     lists and/or ranges of any of the atomic attributes. For example,
!
!\begin{verbatim}
!     allocate( ranges(2), values(2) )
!     allocate( i4Array(3), r4Array(2) )
!     i4Array=(/1,7,5/); r4Array=(/150.,250./)
!     ranges(1) = ESMF_LocalArrayCreate( i4Array, ESMF_DATACOPY_VALUE )
!     values(2) = ESMF_LocalArrayCreate( r4Array, ESMF_DATACOPY_VALUE )
!     deallocate( i4Array, r4Array )
!     lsnew = ESMF_LocStreamCreateLocStream( ls, "middle atmosphere", &
!                                       key = 'ks:lev', ranges=ranges,  &
!                                       values = values, nsel=nsel, rc=rc )
!     call ESMF_LocalArrayDestroy( ranges(1) )
!     call ESMF_LocalArrayDestroy( values(2) )
!\end{verbatim}
!
!     selects locations that simultaneously satisfy all specified criteria.
!
!     Alternatively,
!\begin{verbatim}
!     lsnew = ESMF_LocStreamCreateLocStream( ls, "middle atmosphere",        &
!                                       key = 'ks:lev', nsel,           &
!                                       ranges=ranges, values = values, &
!                                       complement=complement, rc = rc )
!\end{verbatim}
!     selects observations that do not satisfy any of the specified 
!     conditions.
!   
!     On return, nsel is the number of selected observations.
!
!     Notes:
!\begin{itemize}
!\item The subset is chosen from the {\em local} DE.  There is no 
!      communication.  It is possible that the resulting location
!      stream could be seriously load imbalanced.
!\item This routine must be called with keyword arguments for the
!\item Each range includes the endpoints, BUT
!\item It is up to the user to deal with possible effects of floating
!      point arithmetic on equality tests for real attributes. For
!      example, use
!\begin{verbatim}
!     r4Array=(/500.-epsilon(1.),500.+epsilon(1.)/)
!     ranges(2) = ESMF_LocalArrayCreate( r4Array, ESMF_DATACOPY_VALUE )
!\end{verbatim}
!\item There is no check for inconsistent conditions; the result will
!      be \verb|nsel=0, rc=0|
!\end{itemize}
!
!EOPI

      type(ESMF_LocStreamClass), pointer      :: igrid       ! Pointer to new grid
      type(ESMF_LocStreamSpecific), pointer   :: igridSpecific
      type(ESMF_LocStream), pointer       :: ls         ! Output loc stream
      type(ESMF_LocStream), pointer       :: lsIn       ! Input loc stream
      type(ESMF_LocalArray)               :: lArray     ! for attributes
      type(ESMF_DELayout)                 :: delayout
      type(ESMF_InternDG)                 :: internDG
      type(ESMF_Pointer)                  :: ptr
      type(ESMF_VM)                       :: vm
      type(ESMF_Logical)                  :: otoflag

      character(len=ESMF_MAXSTR)          :: string
      integer(ESMF_KIND_I4), pointer      :: i4ptr(:)
      real(ESMF_KIND_R4), pointer         :: r4ptr(:)
      real(ESMF_KIND_R8), pointer         :: r8ptr(:)

!  Local storage for selection:
!  ---------------------------
      logical, allocatable                :: selected(:)

      integer :: i, k
      integer :: nLocalActive                           ! Number active locs
      integer :: localrc                                ! Error status
      integer :: rank, keyLen
      integer :: counts(1)
      integer :: ns
      integer :: deCount, petCount, localPet
      character(len=ESMF_MAXSTR) :: key
      character(len=ESMF_MAXSTR), pointer :: keyArray(:)  ! order sort keys
      logical :: dummy

      type(ESMF_TypeKind_Flag)              :: keyKind       ! key kind
      integer                          :: localKind

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      if (igridIn%ptr%horzLocStreamType /= ESMF_IGRID_TYPE_LOCATIONSTREAM ) then
        if ( ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                   "LocStream object not location stream", &
                                   ESMF_CONTEXT, rcToReturn=rc) ) return
        return
      endif

! check igrid status
      if (igridIn%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to copy an uninitialized igrid", &
                           ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Initialize pointers
      nullify(igrid)
      nullify(ESMF_LocStreamCreateLocStreamSubset%ptr)

      allocate(igrid, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, "Allocating LocStream object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call construction method to allocate and initialize igrid internals.
      call ESMF_LocStreamConstructNew(igrid, name, localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_LocStreamGetDELayout(igridIn, delayout, rc=localrc)

      !
      ! For the time being, just fill in the LocStreamClass variables
      igrid%igridStructure             = igridIn%ptr%igridStructure
      igrid%horzIgridType              = igridIn%ptr%horzLocStreamType
      igrid%horzStagger                = igridIn%ptr%horzStagger
      igrid%coordOrder                 = igridIn%ptr%coordOrder
      igrid%dimCount                   = 1   ! By definition
      igrid%minGlobalCoordPerDim(1)    = igridIn%ptr%minGlobalCoordPerDim(1)
      igrid%maxGlobalCoordPerDim(1)    = igridIn%ptr%maxGlobalCoordPerDim(1)

      lsIn => igridIn%ptr%igridSpecific%locStream

!
! Create a copy of the location stream (without the contents of the keys)
!
      igrid%igridSpecific%locStream => ESMF_LSConstructCopy(lsIn,localrc)
      if (ESMF_LogFoundAllocError(localrc, "locStream",              &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      ls => igrid%igridSpecific%locStream

      allocate( ls%dist( size(lsIn%dist) ) )

      if ( present( values ) .or. present( ranges ) ) then

!
! First pull out the sorting keys
!
        nLocalActive = lsIn%nLocalActive 
        allocate ( selected(nLocalActive), stat=localrc )
        if (ESMF_LogFoundAllocError(localrc, "locStream",              &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

!  All data are selected to begin with
!  -----------------------------------
        selected = .true.

        i = 0
        string = trim( keyNames )
        do while ( string /= '' )
          i = i + 1

          if ( i > lsIn%keyCount ) then
            dummy = ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                          "TOO MANY KEYS SPECIFIED", &
                                          ESMF_CONTEXT, rcToReturn=rc)
          endif
          call ESMF_StripKey( string, key )

          call ESMF_LSGetLocalArrayKey(lsIn, key, lArray, keyLen, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rcToReturn=rc)) return
          if ( keyLen /= lsIn%nLocalActive ) then
            dummy = ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                          "size of array does not match", &
                                          ESMF_CONTEXT, rcToReturn=rc)
          endif

!
! Select from a list of values (if specified for this key)
!
          if ( present(values) ) then
            call ESMF_MSSelectValues(lArray, values(i), selected)
          endif

!
! Select from the range (if specified for this key)
!
          if ( present(ranges) ) then
            call ESMF_MSSelectRange(lArray, ranges(i), selected)
          endif
        enddo

!
! The array 'selected' now contains the entries to be kept (or excluded)
!
!  Apply complement
!  ----------------
        if ( present(complement) ) then
          if ( complement ) selected = .not. selected
        end if

        ls%nLocalActive = count(selected)
        ns = ls%nLocalActive
        if (present(nsel)) nsel = ns


!
! Attach keys
!
        do i=1, lsIn%keyCount
          call ESMF_LocalArrayGetThis( lsIn%keys(i), ptr )
          if ( ptr /= ESMF_NULL_POINTER ) then
            ls%keys(i) = ESMF_LocalArraySelect( lsIn%keys(i), selected, &
                                                rc=localrc )
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
          endif
        enddo

!
! Communication part of code:  can this be removed to make the
! routine communication-free?
!
        call ESMF_DELayoutGet(delayout, vm=vm, deCount=deCount,         &
                              oneToOneFlag=otoFlag, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,           &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

        if (otoflag .ne. ESMF_TRUE) then
          dummy = ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                        "DELAYOUT not one-to-one", &
                                        ESMF_CONTEXT, rcToReturn=rc)
          return
        endif

        call ESMF_VMGet( vm, localPet = localPet,                       &
                         petCount = petCount, rc=localrc )
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,           &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

        if ( petCount /= deCount ) then
          dummy = ESMF_LogFoundError(ESMF_RC_OBJ_BAD,                &
                                        "PETs to DEs not one-to-one",   &
                                        ESMF_CONTEXT, rcToReturn=rc)
          return
        endif

        ls%dist    = 0

        call ESMF_VMAllGather(vm, sendData=(/ls%nLocalActive/),         &
                              recvData=ls%dist, count=1, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

      
        counts(1) = sum( ls%dist(1:deCount) )
        if ( counts(1) > ls%maxGlobal ) then
          dummy = ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                        "TOO MANY TOTAL LOCATIONS", &
                                        ESMF_CONTEXT, rcToReturn=rc)
          return
        endif

      else
!
! Attach keys
!
        ls%dist = lsIn%dist
        do k=1, lsIn%keyCount
          call ESMF_LocalArrayGetThis( lsIn%keys(k), ptr )
          if ( ptr /= ESMF_NULL_POINTER ) then
            ls%keys(k) = ESMF_LocalArrayCreate( lsIn%keys(k), localrc )
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_LocalArrayGet( lsIn%keys(k), kind = keyKind, rc=localrc )
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return
            localkind = keyKind%dkind
          endif
        enddo

!
! In the trivial case, just sort the locations locally by the key names
!
        call ESMF_LSSortLocal( ls, keyNames, rc=localrc )
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

      endif


!
! Determine the internal DG (later just DistGrid)
!
      internDG = ESMF_InternDGCreate( 1, (/sum(ls%dist)/), delayout, &
                                      (/1/), ls%dist, rc=localrc )
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      ! now that it's created, add the interndg to the igrid
      call ESMF_LocStreamAddInternDG(igrid, internDG, localrc)

      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      igrid%igridStorage = ESMF_IGRID_STORAGE_ARBITRARY
      igrid%igridStatus  = ESMF_IGRID_STATUS_READY

      ! Set return values.
      ESMF_LocStreamCreateLocStreamSubset%ptr => igrid

      ESMF_INIT_SET_CREATED(ESMF_LocStreamCreateLocStreamSubset)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateLocStreamSubset
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreateMerge"
!BOPI
! !IROUTINE: ESMF_LocStreamCreate - Create a new stream by merging other LocStreams

! !INTERFACE:
      ! NOT YET INTEGRATED
      ! Private name; call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateMerge(locstreamList, name, rc )

!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateMerge

!
! !ARGUMENTS:
      type (ESMF_LocStream), dimension(:), intent(in) :: locstreamList
      character (len=*),                  intent(in)  :: name
      integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!     Merges two location streams {\em locally}, which may violate
!     the criteria applied to either of the input location streams.
!
!     The arguments are:
!     \begin{description}
!     \item[locstreamList]
!          List of location streams from which the new location stream is to be created
!     \item[name]
!          Name of the resulting location stream
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      type(ESMF_LocStreamClass), pointer   :: igrid         ! Pointer to new grid
      type(ESMF_LocStream), pointer    :: ls            ! Pointer to new LS
      type(ESMF_LocStream), pointer    :: lsIn          ! Pointer to LS element
      type(ESMF_DELayout)              :: delayout      ! Layout
      type(ESMF_TypeKind_Flag)              :: keyKind       ! key kind
      type(ESMF_InternDG)              :: internDG      ! Internal distgrid
      type(ESMF_Pointer)               :: ptr           ! temporary pointer
      integer :: nDEs, nDEsIn                           ! Number of DEs
      integer :: keyCount, keyCountIn                   ! Number of DEs
      integer :: i, j, k                                ! indices
      integer :: count                                  ! counter
      integer :: localrc                                ! Error status
      integer :: localkind
      integer(ESMF_KIND_I4), dimension(:), pointer :: intptr
      integer(ESMF_KIND_I4), dimension(:), pointer :: intArrayNew
      real(ESMF_KIND_R8), dimension(:), pointer    :: realptr
      real(ESMF_KIND_R8), dimension(:), pointer    :: realArrayNew


      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ! Initialize pointers
      nullify(igrid)
      nullify(ESMF_LocStreamCreateLocStreamMerge%ptr)

      allocate(igrid, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, "Allocating LocStream object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call construction method to allocate and initialize igrid internals.
      call ESMF_LocStreamConstructNew(igrid, name, localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return
!
! Compare key names to see if the location streams are compatible
!

      !
      ! For the time being, just fill in the LocStreamClass variables
      igrid%igridStructure             = ESMF_IGRID_STRUCT_UNSTRUCT
      igrid%horzLocStreamType              = ESMF_IGRID_TYPE_LOCATIONSTREAM
      igrid%horzStagger                = ESMF_IGRID_HORZ_STAGGER_UNKNOWN
      igrid%coordOrder                 = ESMF_COORD_ORDER_XYZ
      igrid%dimCount                   = 1   

!
!     To bind with ESMF:  allocate the specific igrid, bind to ls


      lsIn => igridsIn(1)%ptr%igridSpecific%locStream 
      igrid%igridSpecific%locStream => ESMF_LSConstructCopy( lsIn, localrc )
      if (ESMF_LogFoundAllocError(localrc, "locStream", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_InternDGGetDELayout(igridsIn(1)%ptr%internDGs(1)%ptr, &
                                    delayout, localrc)
      if (ESMF_LogFoundAllocError(localrc, "locStream", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

      ls => igrid%igridSpecific%locStream 

      ls%maxGlobal = 0
      ls%maxLocal  = 0
      ls%nLocalActive = 0

!
! All igrids must have be distributed over the same number of DEs
!
      allocate( ls%dist( size(lsIn%dist) ) )

      ls%dist = 0

      keyCount = igridsIn(1)%ptr%igridSpecific%locStream%keyCount
      do k = 1, size( igridsIn )
        lsIn => igridsIn(k)%ptr%igridSpecific%locStream 
        keyCountIn = lsIn%keyCount

!
! Check for consistency in the number keys
!
        if ( keyCount /= keyCountIn ) then
          if (ESMF_LogFoundError(ESMF_RC_NOT_FOUND, &
                                    "LocStream array inconsistent", &
                                    ESMF_CONTEXT, rcToReturn=rc)) return
        endif

!
! TODO  Check for consistency in the name and type of keys
!
        ls%dist = ls%dist + lsIn%dist

! Determine the maximum number of observations
        ls%maxGlobal = ls%maxGlobal + lsIn%maxGlobal
! Determine the maximum local number of observations
        ls%maxLocal  = ls%maxLocal  + lsIn%maxLocal
! Determine the length of the merged location stream
        ls%nLocalActive = ls%nLocalActive + lsIn%nLocalActive
      enddo


      igrid%minGlobalCoordPerDim(1)    = 0.0
      igrid%maxGlobalCoordPerDim(1)    = REAL(ls%maxGlobal)

!
! Allocate and copy keys
!
      allocate( ls%keys( ls%keyCount ) )  ! Allocate the array of keys

      do k=1, ls%keyCount
        lsIn => igridsIn(k)%ptr%igridSpecific%locStream 
        call ESMF_LocalArrayGetThis( lsIn%keys(k), ptr )
        if ( ptr /= ESMF_NULL_POINTER ) then 

          call ESMF_LocalArrayGet( lsIn%keys(k), kind = keyKind, rc=localrc )
          if (ESMF_LogFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rcToReturn=rc)) return
          localkind = keyKind%dkind

          select case (localkind)
            case (ESMF_TYPEKIND_I4%dkind)
 
              allocate( intArrayNew( ls%nLocalActive ) )
              count = 0
              do j=1, size(igridsIn)
                call ESMF_LocalArrayGetData( lsIn%keys(k), intPtr,   &
                                             ESMF_DATACOPY_REFERENCE, localrc )
                if (ESMF_LogFoundError(localrc, &
                                          ESMF_ERR_PASSTHRU, &
                                          ESMF_CONTEXT, rcToReturn=rc)) return
                do i=1, lsIn%nLocalActive
                  count = count + 1
                  intArrayNew(count) = intPtr(i)     ! Copy key
                enddo
              enddo
              ls%keys(k) = ESMF_LocalArrayCreate( intArrayNew, &
                                                  ESMF_DATACOPY_VALUE, localrc )
              if (ESMF_LogFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc )) return

            case (ESMF_TYPEKIND_R8%dkind)

              allocate( realArrayNew( ls%nLocalActive ) )
              count = 0
              do j=1, size(igridsIn)
                call ESMF_LocalArrayGetData( lsIn%keys(k), realPtr,  &
                                             ESMF_DATACOPY_REFERENCE, localrc )
                if (ESMF_LogFoundError(localrc, &
                                          ESMF_ERR_PASSTHRU, &
                                          ESMF_CONTEXT, rcToReturn=rc)) return
                
                do i=1, lsIn%nLocalActive
                  count = count + 1
                  realArrayNew(count) = realPtr(i)   ! Copy key
                enddo
              enddo
              ls%keys(k) = ESMF_LocalArrayCreate(RealArrayNew, &
                                                 ESMF_DATACOPY_VALUE, localrc)
              if (ESMF_LogFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rcToReturn=rc)) return

            case default
              if (ESMF_LogFoundError(ESMF_RC_NOT_IMPL, &
                                        "Unsupported Key Type", &
                                        ESMF_CONTEXT, rcToReturn=rc) ) return
          end select

        endif
      
      enddo

!
! Determine the new decomposition
!

! Determine the internal DG (later just DistGrid)

      internDG = ESMF_InternDGCreate( 1, (/sum(ls%dist)/), delayout, &
                                      (/1/), ls%dist, rc=localrc )
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      ! now that it's created, add the interndg to the igrid
      call ESMF_LocStreamAddInternDG(igrid, internDG, localrc)

      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      igrid%igridStorage = ESMF_IGRID_STORAGE_ARBITRARY
      igrid%igridStatus  = ESMF_IGRID_STATUS_READY

      ! Set return values.
      ESMF_LocStreamCreateLocStreamMerge%ptr => igrid

      ESMF_INIT_SET_CREATED(ESMF_LocStreamCreateLocStreamMerge)
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateLocStreamMerge
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreateByKeys"
!BOPI
! !IROUTINE: ESMF_LocStreamCreate - Revised LocStream based on sort criteria

! !INTERFACE:
      ! NOT YET INTEGRATED
      ! Private name; call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateByKeys(locstream, name, sortKeys, &
                 balancePrimary, rc)

!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateByKeys

!
! !ARGUMENTS:
      type (ESMF_LocStream), intent(inout)         :: locstream
      character (len=*),     intent(in)            :: name
      character (len=*),     intent(in)            :: sortKeys
      logical,               intent(in)            :: balancePrimary
      integer,               intent(out), optional :: rc

! !DESCRIPTION:
!
! Using an existing location stream, create a new location stream 
! based on a set of sorting keys (primary, secondary, tertiary, 
! ...).  The location stream data are maintained, but they are 
! redistributed over all DEs to reflect the sorting.
!  
! In the simplest case, this routine tries to distribute the
! observations even manner, while respecting the constraint that
! entries with any given key value reside only on one DE.  Since this
! might create load imbalance, a added with the additional parameters
! {\tt sortKeys} and {\tt unsplitKey}.  The former first sorts the
! observations by primary, secondary (and possibly higher level) keys.
! The distribution then ensures that:
!
! \begin{eqnarray}
!  DE_a < DE_b & \Longrightarrow & ( k^{(1)}_a < k^{(1)}_b ) \vee \\
!  &&  (( k^{(1)}_a = k^{(1)}_b ) \wedge ( k^{(2)}_a < k^{(2)}_b )) \vee \\
!  &&  (( k^{(1)}_a = k^{(1)}_b ) \wedge ( k^{(2)}_a = k^{(2)}_b ) 
!                                 \wedge (k^{(3)}_a < k^{(3)}_b) ) \vee \cdots
! \end{eqnarray}
!
! However, while one DE can own multiple primary keys, it is not
! allowed that observations with the same primary key value are
! located on more than one DE:
!     
! \begin{equation}
!   DE_a \ne DE_b \Longrightarrow k^{(1)}_a \ne k^{(1)}_b 
! \end{equation}
!  
! This definition does not allows {\tt balancePrimary} releases this
! constraint.
!
! There may be a need for further constraints at a later time.  Given
! all of these constraints, the routine tries its best to evenly
! distribute the observations.
!
! This routine can be expensive in terms of communication: all the
! keys must be redistributed.  Worse, this routine might be called as
! often as every hour of atmospheric simulation.  Thus it needs to be
! efficient, even for $10^6$ or more locations.
!
! The arguments are:
!
!     \begin{description}
!     \item[locstream]
!          Location stream from which the new location stream is to be created
!     \item[name]
!          Name of the resulting location stream
!     \item[sortKeys]
!          Keys to sort by (primary, secondary, and higher level)
!     \item[{[balancePrimary]}]
!          If is allowed to split locations with the same primary
!          key value over more than one DE for load balance. Default: false.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      type(ESMF_LocStreamClass), pointer   :: igrid         ! Pointer to new grid
      type(ESMF_LocStream), pointer    :: lsIn          ! Pointer to LS
      type(ESMF_LocStream), pointer    :: lsOut         ! Pointer to new LS
      type(ESMF_LocalArray)            :: lArray        ! Temp. Local Array
      type(ESMF_DELayout)              :: delayout      ! Layout
      type(ESMF_InternDG)              :: internDG      ! Internal distgrid
      type(ESMF_VM)                    :: vm            ! Virtual machine
      type(ESMF_Logical)               :: otoFlag       ! One-to-one flag
      type(ESMF_Pointer)               :: ptr           ! pointer

      integer                          :: nLocalActive  ! Number active entries
      integer                          :: deCount       ! Number of DEs
      integer                          :: counts(1)     ! for InternDG
      integer                          :: localrc       ! Error status
      integer                          :: i, k          ! loop index

      character(len=ESMF_MAXSTR)       :: string, primaryKey

      integer, allocatable             :: indx(:)       ! index array for sort
      integer, allocatable             :: blockSizes(:) ! block sizes
      logical                          :: dummy


      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      if ( balancePrimary ) then
        print *, "balancePrimary currently not supported, using unbalanced"
      endif

      if (igridIn%ptr%horzLocStreamType /= ESMF_IGRID_TYPE_LOCATIONSTREAM ) then
        if ( ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                   "LocStream object not location stream", &
                                   ESMF_CONTEXT, rcToReturn=rc) ) return
        return
      endif

! check igrid status
      if (igridIn%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to copy an uninitialized igrid", &
                           ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

     ! Initialize pointers
      nullify(igrid)
      nullify(ESMF_LocStreamCreateLocStreamByKeys%ptr)

      allocate(igrid, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, "Allocating LocStream object", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call construction method to allocate and initialize igrid internals.
      call ESMF_LocStreamConstructNew(igrid, name, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

!
! Compare key names to see if the location streams are compatible
!

      !
      ! For the time being, just fill in the LocStreamClass variables
      igrid%igridStructure             = ESMF_IGRID_STRUCT_UNSTRUCT
      igrid%horzIgridType              = ESMF_IGRID_TYPE_LOCATIONSTREAM
      igrid%horzStagger                = ESMF_IGRID_HORZ_STAGGER_UNKNOWN
      igrid%coordOrder                 = ESMF_COORD_ORDER_XYZ
      igrid%dimCount                   = 1   

      lsIn => igridIn%ptr%igridSpecific%locStream 
      igrid%igridSpecific%locStream => ESMF_LSConstructCopy( lsIn, localrc )
      if (ESMF_LogFoundAllocError(localrc, "locStream", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

!     To bind with ESMF:  allocate the specific igrid, bind to lsOut
      lsOut => igrid%igridSpecific%locStream

!
! Attach keys
!
      do k=1, lsIn%keyCount
        call ESMF_LocalArrayGetThis( lsIn%keys(k), ptr )
        if ( ptr /= ESMF_NULL_POINTER ) then
          lsOut%keys(k) = ESMF_LocalArrayCreate( lsIn%keys(k), localrc )
          if (ESMF_LogFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rcToReturn=rc)) return
        endif
      enddo

!
      call ESMF_LocStreamGetDELayout(igridIn, delayout, rc=localrc)
      if (ESMF_LogFoundAllocError(localrc, "locStream", &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

!
! Distribution part of Create
      call ESMF_DELayoutGet(delayout, vm=vm, deCount=deCount,            &
                            oneToOneFlag=otoFlag, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rcToReturn=rc)) return

      if (otoflag .ne. ESMF_TRUE) then
        dummy = ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                      "DELAYOUT not one-to-one", &
                                      ESMF_CONTEXT, rcToReturn=rc)
        return
      endif

      nLocalActive = lsOut%nLocalActive   ! might be 0 !!

!
! Only redistribute if there is more than one DE, otherwise simply sort
!
      if ( deCount > 1 ) then 

!
! First pull out the primary key
!
        string = trim( sortKeys )
        call ESMF_StripKey( string, primaryKey )

!
! pull out primary key
!
        call ESMF_LSGetLocalArrayKey(lsOut, primaryKey, lArray, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

        allocate( indx(nLocalActive) )
        call ESMF_MSIndexSet( nLocalActive, indx )
        call ESMF_MSIndexSort( indx, lArray, .false., localrc )
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

!
! Find the lowest and highest keys locally
! Find the partitions which would evenly distribute the local data set
!
        allocate( lsOut%dist( deCount ) )
        lsOut%dist = 0
        call collectLAStats(lArray,lsOut%dist)

!
! Redistribute the location stream 
!
        call ESMF_LSRedistribute( lsOut, vm, lsOut%dist, indx, rc=localrc )
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

!
! Now consolidate and gather all the new local sizes
!
        lsOut%dist  = 0
        call ESMF_VMAllGather(vm, sendData=(/lsOut%nLocalActive/), &
                              recvData=lsOut%dist, count=1, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
      
        counts(1) = sum( lsOut%dist(1:deCount) )
        if ( counts(1) > lsOut%maxGlobal ) then
          dummy = ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
                                        "TOO MANY TOTAL LOCATIONS", &
                                        ESMF_CONTEXT, rcToReturn=rc)
          return
        endif

!
! Determine the internal DG (later just DistGrid)
        internDG = ESMF_InternDGCreate( 1, counts, delayout, &
                                        (/1/), lsOut%dist, rc=localrc )
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

      ! now that it's created, add the interndg to the igrid
        call ESMF_LocStreamAddInternDG(igrid, internDG, localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate( indx )

      endif

!
! Finally, sort everything locally with all the sort keys
!
      call ESMF_LSSortLocal( lsOut, sortKeys, localrc )
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

      igrid%igridStorage = ESMF_IGRID_STORAGE_ARBITRARY
      igrid%igridStatus  = ESMF_IGRID_STATUS_READY

      ! Set return values.
      ESMF_LocStreamCreateLocStreamByKeys%ptr => igrid
      ESMF_INIT_SET_CREATED(ESMF_LocStreamCreateLocStreamByKeys)

      if (present(rc)) rc = ESMF_SUCCESS

    contains

      subroutine collectLAStats(lArray,blockSizes)

      type(ESMF_LocalArray), intent(in)    :: lArray
      integer, intent(inout), dimension(:) :: blockSizes

      integer(ESMF_KIND_I4), pointer      :: i4ptr(:)
      integer(ESMF_KIND_I4), allocatable  :: partitionI4(:),resultI4(:)
      real(ESMF_KIND_R4), pointer         :: r4ptr(:)
      real(ESMF_KIND_R4), allocatable     :: partitionR4(:), resultR4(:)
      real(ESMF_KIND_R8), pointer         :: r8ptr(:)
      real(ESMF_KIND_R8), allocatable     :: partitionR8(:), resultR8(:)

      type(ESMF_Pointer)                  :: ptr           ! Temporary pointer
      type(ESMF_TypeKind_Flag)                 :: keyKind       ! key kind
      integer                             :: localkind
      integer                             :: localrc
      integer                             :: iDE, index, partitionSize, part

      partitionSize = (nLocalActive / deCount)


      call ESMF_LocalArrayGetThis( lArray, ptr )
      if ( ptr /= ESMF_NULL_POINTER ) then 
        call ESMF_LocalArrayGet( lArray, kind = keyKind, rc = localrc )
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
        localkind = keyKind%dkind

        select case (localkind)
          case (ESMF_TYPEKIND_I4%dkind)
            call ESMF_LocalArrayGetData( lArray, i4ptr,          &
                                         ESMF_DATACOPY_REFERENCE, localrc )
            if (ESMF_LogFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return

            allocate( resultI4(deCount-1), partitionI4(deCount-1) )
            partitionI4 = 0
            if ( partitionSize > 0 ) then
              index = 0
              do iDE=1, deCount-1
                index = index + partitionSize
                partitionI4(iDE) =  i4ptr( indx( index ) )
              enddo
            endif
            call ESMF_VMAllReduce( vm, partitionI4, resultI4, deCount-1, &
                                   ESMF_REDUCE_MAX)

            part = 0
            iDE  = 1
            do i=1, nLocalActive
              if ( i4ptr( indx( i ) ) .ge. resultI4( iDE )   ) then
                blockSizes(iDE) = part
                iDE = iDE + 1
                part = 1  ! Not zero, because this element has now been read
              else
                part = part + 1
              endif
            enddo
            blockSizes(deCount) = part   !  Last DE gets all the rest
            deallocate( resultI4, partitionI4 )


          case (ESMF_TYPEKIND_R4%dkind)
            call ESMF_LocalArrayGetData( lArray, r4ptr,          &
                                         ESMF_DATACOPY_REFERENCE, localrc )
            if (ESMF_LogFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return


            allocate( resultR4(deCount-1), partitionR4(deCount-1) )
            partitionR4 = 0.0
            if ( partitionSize > 0 ) then
              index = 0
              do iDE=1, deCount-1
                index = index + partitionSize
                partitionR4(iDE) =  r4ptr( indx( index ) )
              enddo
            endif
            call ESMF_VMAllReduce( vm, partitionR4, resultR4, deCount-1, &
                                   ESMF_REDUCE_MAX)

            part = 0
            iDE  = 1
            do i=1, nLocalActive
              if ( r4ptr( indx( i ) ) .ge. resultR4( iDE )   ) then
                blockSizes(iDE) = part
                iDE = iDE + 1
                part = 1   ! Not zero, because this element has now been read
              else
                part = part + 1
              endif
            enddo
            blockSizes(deCount) = part   !  Last DE gets all the rest
            deallocate( resultR4, partitionR4 )

          case (ESMF_TYPEKIND_R8%dkind)
            call ESMF_LocalArrayGetData( lArray, r8ptr,          &
                                     ESMF_DATACOPY_REFERENCE, localrc )
            if (ESMF_LogFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rcToReturn=rc)) return

            allocate( resultR8(deCount-1), partitionR8(deCount-1) )
            partitionR8 = 0.0
            if ( partitionSize > 0 ) then
              index = 0
              do iDE=1, deCount-1
                index = index + partitionSize
                partitionR8(iDE) =  r8ptr( indx( index ) )
              enddo
            endif
            call ESMF_VMAllReduce( vm, partitionR8, resultR8, deCount-1, &
                                   ESMF_REDUCE_MAX)

            part = 0
            iDE  = 1
            do i=1, nLocalActive
              if ( r8ptr( indx( i ) ) .ge. resultR8( iDE ) ) then
                blockSizes(iDE) = part
                iDE = iDE + 1
                part = 1    ! Not zero, because this element has now been read
              else
                part = part + 1
              endif
            enddo
            blockSizes(deCount) = part   !  Last DE gets all the rest
            deallocate( resultR8, partitionR8 )

          case default
           if (ESMF_LogFoundError(ESMF_RC_NOT_IMPL, &
                                     "Unsupported Key Type", &
                                     ESMF_CONTEXT, rcToReturn=rc) ) return
        endselect
      endif
      
      end subroutine collectLAStats

      end function ESMF_LocStreamCreateByKeys
!------------------------------------------------------------------------------

#endif

end module ESMF_LocStreamMod


  subroutine f_esmf_locstreamcollectgarbage(ls, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcollectgarbage()"
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_LocStreamMod

    implicit none

    type(ESMF_LocStream) :: ls
    integer, intent(out) :: rc     
  
    integer :: localrc              
  
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL
  
    !print *, "collecting LocStream garbage"
  
    ! destruct internal data allocations
    call ESMF_LocStreamDestruct(ls%lstypep, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! deallocate actual LocStreamType allocation      
    if (associated(ls%lstypep)) then
      deallocate(ls%lstypep, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Deallocating LocStream", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    nullify(ls%lstypep)

    ! return successfully  
    rc = ESMF_SUCCESS

  end subroutine f_esmf_locstreamcollectgarbage

