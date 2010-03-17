! $Id: ESMF_LocStream.F90,v 1.24.2.4 2010/03/17 20:36:24 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
! a data {\tt ESMF\_Array}, {\tt ESMF\_Grid}, and I/O specification, or
! {\tt ESMF\_IOSpec} (NOT IMPLEMENTED). 
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
  use ESMF_ArraySpecMod
  use ESMF_LocalArrayMod
  use ESMF_DELayoutMod
  use ESMF_DistGridMod
  use ESMF_ArrayMod
  use ESMF_ArrayCreateMod
  use ESMF_ArrayGetMod
  use ESMF_InitMacrosMod
  use ESMF_GridMod
  use ESMF_GridUtilMod
  use ESMF_StaggerLocMod
  use ESMF_ArrayBundleMod
  use ESMF_MeshMod


  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_LocStreamType
! ! Definition of the LocStream class.

  type ESMF_LocStreamType
     sequence

    !private
     type (ESMF_Base)                     :: base             ! base class object
     logical                              :: destroyDistgrid 
     type (ESMF_DistGrid)                 :: distgrid         ! description of index space of Arrays
     type(ESMF_IndexFlag)                 :: indexflag
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
    sequence
    !private       
    type (ESMF_LocStreamType), pointer :: lstypep
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_LocStream
  public ESMF_LocStreamType ! For internal use only
   public ESMF_LocStreamValidate           ! Check internal consistency
   public ESMF_LocStreamCreate
   public ESMF_LocStreamGet
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
    '$Id: ESMF_LocStream.F90,v 1.24.2.4 2010/03/17 20:36:24 oehmke Exp $'

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
      module procedure ESMF_LocStreamCreateReg
      module procedure ESMF_LocStreamCreateIrreg
      module procedure ESMF_LocStreamCreateByBkgMesh
      module procedure ESMF_LocStreamCreateByBkgGrid
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_LocStreamCreate} functions.   
!EOPI 
end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_LocStreamGet -- Generic interface

! !INTERFACE:
interface ESMF_LocStreamGet

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_LocStreamGetDefault
      module procedure ESMF_LocStreamGetBounds
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_LocStreamGet} functions.   
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
      module procedure ESMF_LocStreamGetKeyBounds
      
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



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamAddKeyAlloc"
!BOP
! !IROUTINE: ESMF_LocStreamAddKey - Add a key Array and allocate the internal memory

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
  subroutine ESMF_LocStreamAddKeyAlloc(locstream, keyName, keyTypeKind, &
               keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)            :: locstream
    character (len=*),    intent(in)            :: keyName
    type(ESMF_TypeKind),  intent(in), optional  :: keyTypeKind
    character (len=*),    intent(in), optional  :: keyUnits 
    character (len=*),    intent(in), optional  :: keyLongName 
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Add a key to a locstream. Once a key has been added its internal data
! can be retrieved and used to set key values. 
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
    type(ESMF_TypeKind) :: localKeyTypeKind 
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
    if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return

    ! Create Array
    array=ESMF_ArrayCreate(arrayspec, distgrid=lstypep%distgrid, &
                           indexflag=lstypep%indexflag, name=keyName, &
                           rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return

   ! Add key to structure
   call ESMF_LocStreamAddKeyArray(locstream, keyName, keyArray=array, destroyKey=.true., &
               keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return


   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamAddKeyAlloc
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamAddKeyArray"
!BOP
! !IROUTINE: ESMF_LocStreamAddKey - Add a key ESMF Array 

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
  subroutine ESMF_LocStreamAddKeyArray(locstream, keyName, keyArray, destroyKey, &
               keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)             :: locstream
    character (len=*),    intent(in)             :: keyName
    type(ESMF_Array),     intent(in)             :: keyArray
    logical,              intent(in),  optional  :: destroyKey
    character (len=*),    intent(in),  optional  :: keyUnits 
    character (len=*),    intent(in),  optional  :: keyLongName 
    integer,              intent(out), optional  :: rc
!
! !DESCRIPTION:
! Add a key to a locstream. Once a key has been added its internal data
! can be retrieved and used to set key values. 
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
      if (ESMF_LogMsgFoundError(ESMF_RC_ARG_WRONG, &
            " - keyName already exists in this LocStream", &
            ESMF_CONTEXT, rc)) return
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
   if (ESMF_LogMsgFoundAllocError(localrc, " Allocating KeyNames", &
                                     ESMF_CONTEXT, rc)) return
   allocate (lstypep%keyUnits(keyCount+1), stat=localrc )
   if (ESMF_LogMsgFoundAllocError(localrc, " Allocating units", &
                                     ESMF_CONTEXT, rc)) return
   allocate (lstypep%keyLongNames(keyCount+1), stat=localrc )
   if (ESMF_LogMsgFoundAllocError(localrc, " Allocating longNames", &
                                     ESMF_CONTEXT, rc)) return
   allocate( lstypep%keys(keyCount+1), stat=localrc )  ! Array of keys
   if (ESMF_LogMsgFoundAllocError(localrc, " Allocating keys", &
                                     ESMF_CONTEXT, rc)) return
   allocate( lstypep%destroyKeys(keyCount+1), stat=localrc )  ! Array of keys
   if (ESMF_LogMsgFoundAllocError(localrc, " Allocating keys", &
                                     ESMF_CONTEXT, rc)) return

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
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamAddKeyI4"
!BOP
! !IROUTINE: ESMF_LocStreamAddKey - Add a key Array created around user memory 

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
  subroutine ESMF_LocStreamAddKeyI4(locstream, keyName, farray, copyflag, &
               keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)                   :: locstream
    character (len=*),         intent(in)                    :: keyName
    integer(ESMF_KIND_I4), dimension(:), intent(in)  :: farray
    type(ESMF_CopyFlag), intent(in), optional       :: copyflag
    character (len=*),    intent(in), optional          :: keyUnits 
    character (len=*),    intent(in), optional   :: keyLongName 
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Add a key to a locstream. Once a key has been added its internal data
! can be retrieved and used to set key values. 
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
! \item[{[copyflag]}] 
! Specifies whether the Array object will reference the memory allocation 
! provided by {\tt farray} directly or will copy the data from 
! {\tt farray} into a new memory allocation. Valid options are 
! {\tt ESMF\_DATA\_REF} (default) or {\tt ESMF\_DATA\_COPY}. 
! Depending on the specific situation the {\tt ESMF\_DATA\_REF} option 
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
!EOP
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
   array=ESMF_ArrayCreate(farray, distgrid=lstypep%distgrid, &
                           copyflag=copyflag, indexflag=lstypep%indexflag,  &
                           name=keyName, rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return


   ! Add key to structure
   call ESMF_LocStreamAddKeyArray(locstream, keyName, keyArray=array, destroyKey=.true., &
               keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return


   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamAddKeyI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamAddKeyR4"
!BOP
! !IROUTINE: ESMF_LocStreamAddKey - Add a key Array created around user memory

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
  subroutine ESMF_LocStreamAddKeyR4(locstream, keyName, farray, copyflag, &
               keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)                   :: locstream
    character (len=*),   intent(in)                    :: keyName
    real(ESMF_KIND_R4),  dimension(:), intent(in)      :: farray
    type(ESMF_CopyFlag), intent(in), optional          :: copyflag
    character (len=*),    intent(in), optional         :: keyUnits 
    character (len=*),    intent(in), optional         :: keyLongName 
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Add a key to a locstream. Once a key has been added its internal data
! can be retrieved and used to set key values. 
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
! \item[{[copyflag]}] 
! Specifies whether the Array object will reference the memory allocation 
! provided by {\tt farray} directly or will copy the data from 
! {\tt farray} into a new memory allocation. Valid options are 
! {\tt ESMF\_DATA\_REF} (default) or {\tt ESMF\_DATA\_COPY}. 
! Depending on the specific situation the {\tt ESMF\_DATA\_REF} option 
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
!EOP
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
   array=ESMF_ArrayCreate(farray, distgrid=lstypep%distgrid, &
                           copyflag=copyflag, indexflag=lstypep%indexflag,  &
                           name=keyName, rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return


   ! Add key to structure
   call ESMF_LocStreamAddKeyArray(locstream, keyName, keyArray=array, destroyKey=.true., &
               keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return


   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamAddKeyR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamAddKeyR8"
!BOP
! !IROUTINE: ESMF_LocStreamAddKey - Add a key Array created around user memory

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
  subroutine ESMF_LocStreamAddKeyR8(locstream, keyName, farray, copyflag, &
               keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)                   :: locstream
    character (len=*),   intent(in)                    :: keyName
    real(ESMF_KIND_R8),  dimension(:), intent(in)      :: farray
    type(ESMF_CopyFlag), intent(in), optional          :: copyflag
    character (len=*),    intent(in), optional         :: keyUnits 
    character (len=*),    intent(in), optional         :: keyLongName 
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Add a key to a locstream. Once a key has been added its internal data
! can be retrieved and used to set key values. 
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
! \item[{[copyflag]}] 
! Specifies whether the Array object will reference the memory allocation 
! provided by {\tt farray} directly or will copy the data from 
! {\tt farray} into a new memory allocation. Valid options are 
! {\tt ESMF\_DATA\_REF} (default) or {\tt ESMF\_DATA\_COPY}. 
! Depending on the specific situation the {\tt ESMF\_DATA\_REF} option 
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
!EOP
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
   array=ESMF_ArrayCreate(farray, distgrid=lstypep%distgrid, &
                           copyflag=copyflag, indexflag=lstypep%indexflag,  &
                           name=keyName, rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return


   ! Add key to structure
   call ESMF_LocStreamAddKeyArray(locstream, keyName, keyArray=array, destroyKey=.true., &
               keyUnits=keyUnits, keyLongName=keyLongName, rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return


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
      function ESMF_LocStreamCreateByBkgGrid(locstream, name, coordKeyNames, &
                 background, maskValues, unmappedAction, rc)

!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateByBkgGrid

!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in)                :: locstream
      character (len=*),    intent(in), optional      :: name
      character (len=*),    intent(in)                :: coordKeyNames
      type(ESMF_Grid),      intent(in)                :: background
      integer(ESMF_KIND_I4), intent(in), optional     :: maskValues(:)
      type(ESMF_UnmappedAction), intent(in), optional :: unmappedAction
      integer,              intent(out), optional     :: rc
!
! !DESCRIPTION:
!
!     Create an location stream from an existing one in accordance with 
!     the distribution of the background Grid.  The entries
!     in the new location stream are redistributed, so that they lie on the same PET
!     as the piece of Grid which contains the coordinates of the entries. The coordinates
!     of the entries are the data in the keys named by {\tt coordKeyNames}. To copy data in
!     Fields or FieldBundles built on {\tt locstream} to the new one simply use {\tt ESMF\_FieldRedist()}
!     or {\tt ESMF\_FieldBundleRedist()}.
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream from which the new location stream is to be created
!      \item[{[name]}]
!          Name of the resulting location stream
!      \item[coordKeyNames]
!          Names of the keys used to determine the link to background Grid.
!          The first key in this list matches up with the first coordinate of the 
!          Grid, the second key in this list matches up with the second coordinate
!          of the Grid, and so on. The key names should be separated by the : character. 
!      \item[background]
!          Background Grid which determines the distribution of the entries in the new location stream.
!          The background Grid 
!          needs to have the same number of dimensions as the number of keys in {\tt coordKeyNames}.  
!          Note also that this subroutine uses the corner stagger location in the Grid for determining 
!          where a point lies, because this is the stagger location which fully contains the cell. 
!          A Grid must have coordinate data in this stagger location to be used in this subroutine. 
!          For a 2D Grid this stagger location is ESMF\_STAGGERLOC\_CORNER for a 3D Grid this 
!          stagger location is ESMF\_STAGGERLOC\_CORNER\_VFACE. Note that currently the background 
!          Grid also needs to have been created with indexflag=ESMF\_INDEX\_GLOBAL to be usable here. 
!     \item [{[maskValues]}]
!           List of values that indicate a background grid point should be masked out. 
!           If not specified, no masking will occur. 
!      \item [{[unmappedAction]}]
!           Specifies what should happen if there are destination points that
!           can't be mapped to a source cell. Options are 
!           {\tt ESMF\_UNMAPPEDACTION\_ERROR} or 
!           {\tt ESMF\_UNMAPPEDACTION\_IGNORE} [NOT IMPLEMENTED]. If not specified, defaults 
!           to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. 
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      type(ESMF_LocStreamType), pointer :: oldLStypep, newLStypep
      type(ESMF_UnmappedAction) :: localunmappedAction
      type(ESMF_Mesh) :: mesh
      type(ESMF_TypeKind) ::keyTypeKind
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


      ! Get Grid dimension and index flag
      call ESMF_GridGet(background, dimCount=gridDimCount, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return


     ! Chose staggerloc based on dimension
     if (gridDimCount .eq. 2) then
        staggerLoc=ESMF_STAGGERLOC_CORNER
     else if (gridDimCount .eq. 3) then
        staggerLoc=ESMF_STAGGERLOC_CORNER_VFACE
     else 
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_WRONG, &
           " - only Grids of dimension 2 or 3 may be used as a background grid ", &
            ESMF_CONTEXT, rc)) return
     endif

     ! Convert Grid to Mesh
     mesh=ESMF_GridToMesh(background, staggerLoc, 0, maskValues, rc=localrc)
     if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return

     ! Create new locstream from Background Mesh
     ESMF_LocStreamCreateByBkgGrid=ESMF_LocStreamCreate(locstream, name, coordKeyNames, &
                 mesh, unmappedAction, rc=localrc)
     if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return
      

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateByBkgGrid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreateByBkgMesh"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream by projecting onto a Mesh

! !INTERFACE:
      ! Private name; call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateByBkgMesh(locstream, name, coordKeyNames, &
                 background, unmappedAction, rc)

!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateByBkgMesh

!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in)                :: locstream
      character (len=*),    intent(in), optional      :: name
      character (len=*),    intent(in)                :: coordKeyNames
      type(ESMF_Mesh),      intent(in)                :: background
      type(ESMF_UnmappedAction), intent(in), optional :: unmappedAction
      integer,              intent(out), optional     :: rc
!
! !DESCRIPTION:
!
!     Create an location stream from an existing one in accordance with 
!     the distribution of the background Mesh.  The entries
!     in the new location stream are redistributed, so that they lie on the same PET
!     as the piece of Mesh which contains the coordinates of the entries. The coordinates
!     of the entries are the data in the keys named by {\tt coordKeyNames}. To copy data in
!     Fields or FieldBundles built on {\tt locstream} to the new one simply use {\tt ESMF\_FieldRedist()}
!     or {\tt ESMF\_FieldBundleRedist()}.
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream from which the new location stream is to be created
!      \item[{[name]}]
!          Name of the resulting location stream
!      \item[coordKeyNames]
!          Names of the keys used to determine the link to background Mesh.
!          The first key in this list matches up with the first coordinate of the 
!          Mesh, the second key in this list matches up with the second coordinate
!          of the Mesh, and so on. The key names should be separated by the : character. 
!      \item[background]
!          Background Mesh which determines the distribution of entries in the new locatiion stream.
!          The Mesh must have the same spatial dimension as the number of keys in
!          {\tt coordKeyNames}. 
!      \item [{[unmappedAction]}]
!           Specifies what should happen if there are destination points that
!           can't be mapped to a source cell. Options are 
!           {\tt ESMF\_UNMAPPEDACTION\_ERROR} or 
!           {\tt ESMF\_UNMAPPEDACTION\_IGNORE} [NOT IMPLEMENTED]. If not specified, defaults 
!           to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. 
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      type(ESMF_LocStreamType), pointer :: oldLStypep, newLStypep
      type(ESMF_UnmappedAction) :: localunmappedAction
      type(ESMF_DistGrid) :: newDistGrid
      type(ESMF_TypeKind) ::keyTypeKind
      character(len=ESMF_MAXSTR)    :: keytemp, string
      integer :: keyCount,i
      integer :: localrc
      integer :: pntDim, pntCount
      real(ESMF_KIND_R8),  pointer  :: pntList(:)
      integer, pointer :: petList(:)


      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Check Variables
      ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,background,rc)


      ! Set default vale for unmappedAction
      if (present(unmappedAction)) then
         localunmappedAction=unmappedAction
      else
         localunmappedAction=ESMF_UNMAPPEDACTION_ERROR
      endif

      ! Currently ESMF_UNMAPPEDACTION_IGNORE not implemented here
      if (localunmappedAction .eq. ESMF_UNMAPPEDACTION_IGNORE) then
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
           " - ESMF_UNMAPPEDACTION_IGNORE option currently not implemented ", &
            ESMF_CONTEXT, rc)) return
      endif


      ! Get old locstream internal pointer
      oldLStypep=>locstream%lstypep

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
      if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return


      ! Allocate memory for points
      allocate(pntList(pntDim*pntCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating pntList", &
                                     ESMF_CONTEXT, rc)) return   

      ! Allocate memory for pets
      allocate(petList(pntCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating pntList", &
                                     ESMF_CONTEXT, rc)) return   

      ! Get Points 
      call ESMF_LocStreamGetPntList(locstream, coordKeyNames, pntDim, &
              pntCount, pntList, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return


      ! Find out where points lie on Mesh
      call ESMF_MeshFindPnt(background, localunmappedAction, &
                                pntDim, pntCount, pntList, &
                                petList, rc=localrc)

      if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return

      ! Can now get rid of pntList
      deallocate(pntList)

      ! Create a new location stream by shifting the entries between 
      ! the pets based on petList
      ESMF_LocStreamCreateByBkgMesh=ESMF_LocStreamCreatePetList(locstream, name, &
                                  petList, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return
    
     ! Can now get rid of pntList
      deallocate(petList)

      ! return success
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateByBkgMesh

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreate"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new location stream from a distgrid

! !INTERFACE:
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateFromDG(name, distgrid, &
                 destroyDistgrid, indexflag, rc )
!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateFromDG

!
! !ARGUMENTS:
      character (len=*),        intent(in), optional     :: name
      type(ESMF_DistGrid),   intent(in)                  :: distgrid
      logical,                        intent(in),   optional :: destroyDistgrid
      type(ESMF_IndexFlag), intent(in),   optional :: indexflag    
      integer,                       intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_LocStream} object, constructs its
!     internal derived types. 
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          Name of the location stream
!     \item[distgrid]
!          Distgrid specifying size and distribution. Only 1D distgrids are allowed.
!     \item[{[destroyDistgrid]}]
!          If .true. the locstream is responsible for destroying the distgrid.
!          Defaults to .false. 
!     \item[{[indexflag]}]
!          Flag that indicates how the DE-local indices are to be defined.
!          Defaults to {\tt ESMF\_INDEX\_DELOCAL}, which indicates
!          that the index range on each DE starts at 1. See Section~\ref{opt:indexflag}
!          for the full range of options. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

      integer                            :: localrc  ! Error status
      type (ESMF_LocStreamType), pointer :: lstypep
      type(ESMF_LocStream)               :: locstream 
      integer :: dimCount 
      type(ESMF_IndexFlag)  :: indexflagLocal
      logical :: destroyDistgridLocal

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Init check input types
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_DistGridGetInit,distgrid,rc)      

      ! Make sure DistGrid is 1D
      call ESMF_DistGridGet(distgrid, dimCount=dimCount, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      if (dimCount .ne. 1) then
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                           " - DistGrid must be 1D", &
                                      ESMF_CONTEXT, rc)) return
      endif

      ! Set defaults
      if (present(indexflag)) then
         indexflagLocal=indexflag
      else
         indexflagLocal=ESMF_INDEX_DELOCAL
      endif

      if (present(destroyDistgrid)) then
          destroyDistgridLocal=destroyDistgrid
      else
          destroyDistgridLocal=.false.
      endif


      ! Initialize pointers
      nullify(lstypep)
      nullify(ESMF_LocStreamCreateFromDG%lstypep)

      ! allocate LocStream type
      allocate(lstypep, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating LocStream type object", &
                                     ESMF_CONTEXT, rc)) return


      ! Allocate space for keys
      nullify(lstypep%keyNames)
      nullify(lstypep%keyUnits)
      nullify(lstypep%keyLongNames)
      nullify(lstypep%keys)
      nullify(lstypep%destroyKeys)

      ! Set some remaining info into the struct      
      lstypep%indexflag=indexflagLocal
      lstypep%destroyDistgrid=destroyDistgridLocal
      lstypep%distgrid=distgrid
      lstypep%keyCount=0

      ! set Name
      call ESMF_BaseCreate(lstypep%base,"LocStream",name,0,rc=localrc)       
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! Set pointer to internal locstream type
      locstream%lstypep=>lstypep

      ! Set return value.
      ESMF_LocStreamCreateFromDG=locstream
      
      ! Add reference to this object into ESMF garbage collection table
      ! Only call this in those Create() methods that do not call other LSCreate()
      call c_ESMC_VMAddFObject(locstream, &
        ESMF_ID_LOCSTREAM%objectID)

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
! !IROUTINE: ESMF_LocStreamCreate - Create a new location stream from an irregular dist.

! !INTERFACE:
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateIrreg(name, minIndex, countsPerDE, indexflag, rc)
!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateIrreg

!
! !ARGUMENTS:
      character (len=*), intent(in), optional         :: name
      integer, intent(in), optional                   :: minIndex
      integer, intent(in)                             :: countsPerDE(:)
      type(ESMF_IndexFlag), intent(in), optional      :: indexflag
      integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_LocStream} object, constructs its
!     internal derived types.  The {\tt ESMF\_DistGrid} is set up, indicating
!     how the LocStream is distributed. 
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          Name of the location stream
!     \item[{[minIndex]}] 
!          Number to start the index ranges at. If not present, defaults
!          to 1.
!     \item[{countsPerDE}] 
!          This array specifies the number of locations per DE.
!     \item[{[indexflag]}]
!          Flag that indicates how the DE-local indices are to be defined.
!          Defaults to {\tt ESMF\_INDEX\_DELOCAL}, which indicates
!          that the index range on each DE starts at 1. See Section~\ref{opt:indexflag}
!          for the full range of options. 
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
    type(ESMF_IndexFlag)  :: indexflagLocal
    integer :: numDEs

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Set defaults
      if (present(indexflag)) then
         indexflagLocal=indexflag
      else
         indexflagLocal=ESMF_INDEX_DELOCAL
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
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
              "- countsPerDE is of length 0", & 
              ESMF_CONTEXT, rc) 
          return
      endif

      ! Calc. maxIndexLocal
      maxIndexLocal=minIndexLocal+sum(countsPerDE(:))-1

     ! Setup DistGrid
     !! setup deBlockList
      allocate(deBlockList(1,2,numDEs), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating deBlockList", &
                                     ESMF_CONTEXT, rc)) return
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
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! cleanup local allocations
      deallocate(deBlockList)

      ! Create LocStream using CreateFromDistGrid version
      ESMF_LocStreamCreateIrreg=ESMF_LocStreamCreateFromDG(name=name, &
                                                               distgrid=distgrid, &
                                                               destroyDistgrid=.true., &
                                                               indexflag=indexflagLocal, &
                                                               rc=localrc )
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateIrreg
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreate"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new location stream from a local count

! !INTERFACE:
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateFromLocal(name, localCount, indexflag, rc)
!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateFromLocal

!
! !ARGUMENTS:
      character (len=*), intent(in), optional         :: name
      integer, intent(in)                             :: localCount
      type(ESMF_IndexFlag), intent(in), optional      :: indexflag
      integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_LocStream} object, constructs its
!     internal derived types.  The {\tt ESMF\_DistGrid} is set up, indicating
!     how the LocStream is distributed. 
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          Name of the location stream
!     \item[localCount]
!          Number of grid cells to be distributed to this DE.
!     \item[{[indexflag]}]
!          Flag that indicates how the DE-local indices are to be defined.
!          Defaults to {\tt ESMF\_INDEX\_DELOCAL}, which indicates
!          that the index range on each DE starts at 1. See Section~\ref{opt:indexflag}
!          for the full range of options. 
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
    type(ESMF_IndexFlag)  :: indexflagLocal

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Set defaults
      if (present(indexflag)) then
         indexflagLocal=indexflag
      else
         indexflagLocal=ESMF_INDEX_DELOCAL
      endif

      ! Get VM for this context
      call ESMF_VMGetGlobal( vm, rc=localrc )
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Gather localCount for each Pet
      call ESMF_VMGet( vm, localPet = localPet,                        &
                       petCount = petCount, rc=localrc )
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      allocate(countsPerPet(petCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating countsPerPet", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_VMAllGather(vm, sendData=(/localCount/),               &
                            recvData=countsPerPet, count=1, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

     ! Setup DistGrid
     !! define min and maxIndex
     minIndex(1)=1
     maxIndex(1)=sum(countsPerPet(:))

     !! setup deBlockList
      allocate(deBlockList(1,2,petCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating deBlockList", &
                                     ESMF_CONTEXT, rc)) return
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
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! cleanup local allocations
      deallocate(countsPerPet)
      deallocate(deBlockList)

      ! Create LocStream using CreateFromDistGrid version
      ESMF_LocStreamCreateFromLocal=ESMF_LocStreamCreateFromDG(name=name, &
                                                               distgrid=distgrid, &
                                                               destroyDistgrid=.true., &
                                                               indexflag=indexflagLocal, &
                                                               rc=localrc )
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateFromLocal
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreate"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new location stream using a regular distribution.

! !INTERFACE:
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateReg(name, &
                 regDecomp, decompFlag, minIndex, maxIndex, indexflag, rc )


!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateReg

!
! !ARGUMENTS:
      character (len=*),     intent(in),  optional  :: name
      integer,               intent(in),  optional  :: regDecomp
      type(ESMF_DecompFlag), intent(in),  optional  :: decompflag
      integer,               intent(in),  optional  :: minIndex
      integer,               intent(in)             :: maxIndex
      type(ESMF_IndexFlag),  intent(in),  optional  :: indexflag
      integer,               intent(out), optional  :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_LocStream} object, constructs its
!     internal derived types.  The {\tt ESMF\_DistGrid} is set up, indicating
!     how the LocStream is distributed. 
!     at a later time. 
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          Name of the location stream
!     \item[{[regDecomp]}]
!          Specify into how many chunks to divide the locations. 
!          If not specified, defaults to the number of PETs.
!     \item[{[decompFlag]}]
!          Specify what to do with leftover locations after division.
!          If not specified, defaults to {\tt ESMF\_DECOMP\_HOMOGEN}. Please
!          see Section~\ref{opt:decompflag} for a full description of the 
!          possible options. 
!     \item{[[minIndex]}]
!          The minimum index across all PETs. If not set defaults to 1. 
!     \item[maxIndex]
!          The maximum index across all PETs.
!     \item[{[indexflag]}]
!          Flag that indicates how the DE-local indices are to be defined.
!          Defaults to {\tt ESMF\_INDEX\_DELOCAL}, which indicates
!          that the index range on each DE starts at 1. See Section~\ref{opt:indexflag}
!          for the full range of options. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

      integer               :: localrc  ! Error status
      type(ESMF_DistGrid)   :: distgrid
      integer               :: minIndexLocal,regDecompLocal
      type(ESMF_DecompFlag) :: decompFlagLocal
      type(ESMF_IndexFlag)  :: indexflagLocal
      type(ESMF_VM)         :: vm 

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Set defaults
      if (present(decompflag)) then
         decompFlagLocal=decompflag
      else
         decompFlagLocal=ESMF_DECOMP_HOMOGEN
      endif

      if (present(indexflag)) then
         indexflagLocal=indexflag
      else
         indexflagLocal=ESMF_INDEX_DELOCAL
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
        !! Get VM for this conext
        call ESMF_VMGetGlobal(vm, rc=localrc )
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

        !! Get petCount from VM
        call ESMF_VMGet(vm, petCount=regDecompLocal, rc=localrc )
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      ! Create DistGrid
      distgrid=ESMF_DistGridCreate(minIndex=(/minIndexLocal/), &
                                   maxIndex=(/maxIndex/), &
                                   regDecomp=(/regDecompLocal/), &
                                   decompFlag=(/decompFlagLocal/), &
                                   indexflag=indexflagLocal, &
                                   rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! Create LocStream using CreateFromDistGrid version
      ESMF_LocStreamCreateReg=ESMF_LocStreamCreateFromDG(name=name, &
                                                         distgrid=distgrid, &
                                                         destroyDistgrid=.true., &
                                                         indexflag=indexflagLocal,&
                                                         rc=localrc )
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateReg
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamDestroy"
!BOP
! !IROUTINE: ESMF_LocStreamDestroy - Destroy a LocStream 

! !INTERFACE:
      subroutine ESMF_LocStreamDestroy(locstream,rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(inout)   :: locstream 
      integer, intent(out), optional               :: rc
!
! !DESCRIPTION:
!     Deallocate an {\tt ESMF\_LocStream} object and all appropriate 
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
        call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
          "Uninitialized or already destroyed LocStream: lstypep unassociated", &
          ESMF_CONTEXT, rc)
        return
      endif 

      ! Destruct all field internals and then free field memory.
      call ESMF_LocStreamDestruct(locstream%lstypep, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! mark object invalid
      call ESMF_BaseSetStatus(locstream%lstypep%base, ESMF_STATUS_INVALID, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
                                
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
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
        
      if (status .eq. ESMF_STATUS_READY) then  
        
        ! Destroy  key Arrays
        do i=1,lstypep%keyCount
          if (lstypep%destroyKeys(i)) then
             call ESMF_ArrayDestroy(lstypep%keys(i), rc=localrc)       
             if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
          endif
        enddo


        ! destroy distgrid
        if (lstypep%destroyDistGrid) then
         !! destroy distgrid
         call ESMF_DistGridDestroy(lstypep%distgrid, rc=localrc)       
         if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
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
#define ESMF_METHOD "ESMF_LocStreamGetDefault"
!BOP
! !IROUTINE: ESMF_LocStreamGet - Return info associated with a LocStream

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGet()
  subroutine ESMF_LocStreamGetDefault(locstream, distgrid, keyCount, &
               keyNames, localDECount, indexflag, name, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)             :: locstream
    type(ESMF_DistGrid), intent(out),  optional  :: distgrid
    integer, intent(out),optional                :: keyCount
    character(len=ESMF_MAXSTR),optional          :: keyNames(:) 
    integer, intent(out),optional                :: localDECount
    type(ESMF_IndexFlag), intent(out), optional  :: indexflag
    character(len=*), intent(out),     optional  :: name
    integer, intent(out), optional               :: rc
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
! The {\tt ESMF\_DistGrid} object that descibes 
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
          call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
              "- keyNames array too short", & 
              ESMF_CONTEXT, rc) 
          return 
       endif

       if (lstypep%keyCount .gt. 0) then
          keyNames(1:lstypep%keyCount)=lstypep%keyNames(1:lstypep%keyCount)
       endif
    endif


   ! Get localDECount
   if (present(localDECount)) then
      call ESMF_DistGridGet(lstypep%distgrid, delayout=delayout, rc=localrc) 
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
          ESMF_CONTEXT, rcToReturn=rc)) return
 
      call ESMF_DELayoutGet(delayout, localDeCount=localDECount, rc=localrc) 
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
       ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! get indexflag
    if (present(indexflag)) then
        indexflag = lstypep%indexflag
    endif

    if (present(name)) then
        call c_ESMC_GetName(lstypep%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return
    endif

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamGetDefault
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyArray"
!BOP
! !IROUTINE: ESMF_LocStreamGetKey - Get ESMF Array associated with key

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
  subroutine ESMF_LocStreamGetKeyArray(locstream, keyName, keyArray, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)            :: locstream
    character (len=*),    intent(in)            :: keyName
    type(ESMF_Array),     intent(out)            :: keyArray
    integer, intent(out), optional :: rc
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
      if (ESMF_LogMsgFoundError(ESMF_RC_ARG_WRONG, &
            " - keyName not found in this LocStream", &
            ESMF_CONTEXT, rc)) return
   endif

   ! Get Array
   keyArray=lstypep%keys(keyIndex)

   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamGetKeyArray
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyBounds"
!BOP
! !IROUTINE: ESMF_LocStreamGetKey - Get the bounds of a key Array

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
      subroutine ESMF_LocStreamGetKeyBounds(locstream, localDE, keyName, & 
          exclusiveLBound, exclusiveUBound, exclusiveCount,     &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,     &
          rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream),   intent(in)       :: locstream
      integer,                intent(in)                  :: localDE
      character (len=*),   intent(in)                :: keyName
      integer,                intent(out), optional :: exclusiveLBound
      integer,                intent(out), optional :: exclusiveUBound
      integer,                intent(out), optional :: exclusiveCount
      integer,                intent(out), optional :: computationalLBound
      integer,                intent(out), optional :: computationalUBound
      integer,                intent(out), optional :: computationalCount
      integer,                intent(out), optional :: totalLBound
      integer,                intent(out), optional :: totalUBound
      integer,                intent(out), optional :: totalCount
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
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

 integer :: localrc
 type(ESMF_Array) :: array

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit, locstream, rc) 

 !!!!! REMOVE THESE BECAUSE IT'S DONE IN THE C++ CALLS
 !! Get localDECount
 !call ESMF_LocStreamGetDefault(locstream, localDECount=localDECount, rc=localrc)
 !if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 !    ESMF_CONTEXT, rcToReturn=rc)) return
 !
 !! Check consistency  of localDE
 !if (localDeCount < 0) then 
 !   call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 !          "- Negative number of localDeCount prohibits request", & 
 !          ESMF_CONTEXT, rc) 
 !   return 
 !endif 
 !
 !if (localDE>=localDeCount) then 
 !   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 !          "- localDE too big", & 
 !          ESMF_CONTEXT, rc) 
 !   return 
 !endif 
 !
 !if (localDE<0) then 
 !   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 !          "- localDE can't be less than 0", & 
 !          ESMF_CONTEXT, rc) 
 !   return 
 !endif 

 ! Get Key Array
 call ESMF_LocStreamGetKeyArray(locstream, keyName=keyName, keyArray=array, rc=localrc)  
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                         ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get Bounds via C++
   call c_ESMC_locstreamgetkeybnds(array, localDE, & 
                 exclusiveLBound, exclusiveUBound, exclusiveCount, &
                 computationalLBound, computationalUBound, computationalCount, &
                 totalLBound, totalUBound, totalCount, &
                 localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return


 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetKeyBounds


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyInfo"
!BOP
! !IROUTINE: ESMF_LocStreamGetKey - Get Info associated with key

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
  subroutine ESMF_LocStreamGetKeyInfo(locstream, keyName, keyUnits, keyLongName, typekind, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)              :: locstream
    character (len=*),    intent(in)              :: keyName
    character (len=*),    intent(out), optional   :: keyUnits 
    character (len=*),    intent(out), optional   :: keyLongName 
    type(ESMF_TypeKind), intent(out), optional    :: typekind
    integer, intent(out), optional :: rc
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
      if (ESMF_LogMsgFoundError(ESMF_RC_ARG_WRONG, &
            " - keyName not found in this LocStream", &
            ESMF_CONTEXT, rc)) return
   endif

   ! Get Info
   if (present(keyUnits)) then
      keyUnits=lstypep%keyUnits(keyIndex)
   endif

   if (present(keyLongName)) then
      keyLongName=lstypep%keyLongNames(keyIndex)
   endif

   if (present(typekind)) then
      call ESMF_ArrayGet(lstypep%keys(keyIndex), typekind=typekind, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
          ESMF_CONTEXT, rcToReturn=rc)) return
   endif

   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamGetKeyInfo
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyI4"
!BOP
! !IROUTINE: ESMF_LocStreamGetKey - Get pointer to key values

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
      subroutine ESMF_LocStreamGetKeyI4(locstream, localDE, keyName, & 
          exclusiveLBound, exclusiveUBound, exclusiveCount,     &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,     &
          farray, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in) :: locstream
      integer, intent(in) :: localDE
      character (len=*),    intent(in)              :: keyName
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
      type(ESMF_CopyFlag), intent(in), optional :: docopy
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
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{keyName}]
!          The key to get the information from.
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
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          farray is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 type(ESMF_LocalArray) :: larray 
 type(ESMF_CopyFlag) :: docopyInt
 integer :: localDECount

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit, locstream, rc) 

 
 ! Set Defaults
 if (present(docopy)) then
    docopyInt=docopy
 else
    docopyInt=ESMF_DATA_REF
 endif

 ! Get localDECount
 call ESMF_LocStreamGetDefault(locstream, localDECount=localDECount, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
     ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Check consistency  of localDE
 if (localDeCount < 0) then 
    call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
           "- Negative number of localDeCount prohibits request", & 
           ESMF_CONTEXT, rc) 
    return 
 endif 

 if (localDeCount == 0) then 
    call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
           "- localDeCount == 0 prohibits request", & 
           ESMF_CONTEXT, rc) 
    return 
 endif
 
 if (localDE>=localDeCount) then 
    call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
           "- localDE too big", & 
           ESMF_CONTEXT, rc) 
    return 
 endif 

 if (localDE<0) then 
    call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
           "- localDE can't be less than 0", & 
           ESMF_CONTEXT, rc) 
    return 
 endif 

 ! Get Key Array
 call ESMF_LocStreamGetKeyArray(locstream, keyName=keyName, keyArray=array, rc=localrc)  
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                         ESMF_CONTEXT, rcToReturn=rc)) return

 
 ! Obtain the native array pointer via the LocalArray interface 
 call ESMF_ArrayGet(array, localDE=localDE, larray=larray, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGet(larray, farray, doCopy, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 

  ! Get Bounds via C++
   call c_ESMC_locstreamgetkeybnds(array, localDE, & 
                 exclusiveLBound, exclusiveUBound, exclusiveCount, &
                 computationalLBound, computationalUBound, computationalCount, &
                 totalLBound, totalUBound, totalCount, &
                 localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetKeyI4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyR4"
!BOP
! !IROUTINE: ESMF_LocStreamGetKey - Get pointer to key values

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
  subroutine ESMF_LocStreamGetKeyR4(locstream, localDE, keyName,        & 
          exclusiveLBound, exclusiveUBound, exclusiveCount,     &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,     &
          farray, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in) :: locstream
      integer, intent(in) :: localDE
      character (len=*),    intent(in)              :: keyName
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
      type(ESMF_CopyFlag), intent(in), optional :: docopy
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
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{keyName}]
!          The key to get the information from.
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
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          farray is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 type(ESMF_LocalArray) :: larray
 type(ESMF_CopyFlag) :: docopyInt
 integer :: localDECount

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit, locstream, rc) 

 
 ! Set Defaults
 if (present(docopy)) then
    docopyInt=docopy
 else
    docopyInt=ESMF_DATA_REF
 endif

 ! Get localDECount
 call ESMF_LocStreamGetDefault(locstream, localDECount=localDECount, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
     ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Check consistency  of localDE
 if (localDeCount < 0) then 
    call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
           "- Negative number of localDeCount prohibits request", & 
           ESMF_CONTEXT, rc) 
    return 
 endif 

 if (localDeCount == 0) then 
    call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
           "- localDeCount == 0 prohibits request", & 
           ESMF_CONTEXT, rc) 
    return 
 endif
 
 if (localDE>=localDeCount) then 
    call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
           "- localDE too big", & 
           ESMF_CONTEXT, rc) 
    return 
 endif 

 if (localDE<0) then 
    call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
           "- localDE can't be less than 0", & 
           ESMF_CONTEXT, rc) 
    return 
 endif 

 ! Get Key Array
 call ESMF_LocStreamGetKeyArray(locstream, keyName=keyName, keyArray=array, rc=localrc)  
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                         ESMF_CONTEXT, rcToReturn=rc)) return

 
 ! Obtain the native array pointer via the LocalArray interface 
 call ESMF_ArrayGet(array, localDE=localDE, larray=larray, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGet(larray, farray, doCopy, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 

 !TODO: Add computational Bounds Calc
  ! Get Bounds via C++
   call c_ESMC_locstreamgetkeybnds(array, localDE, & 
                 exclusiveLBound, exclusiveUBound, exclusiveCount, &
                 computationalLBound, computationalUBound, computationalCount, &
                 totalLBound, totalUBound, totalCount, &
                 localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetKeyR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyR8"
!BOP
! !IROUTINE: ESMF_LocStreamGetKey - Get pointer to key values

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
      subroutine ESMF_LocStreamGetKeyR8(locstream, localDE, keyName, & 
          exclusiveLBound, exclusiveUBound, exclusiveCount,     &
          computationalLBound, computationalUBound, computationalCount,     &
          totalLBound, totalUBound, totalCount,     &
          farray, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in) :: locstream
      integer, intent(in) :: localDE
      character (len=*),    intent(in)              :: keyName
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
      type(ESMF_CopyFlag), intent(in), optional :: docopy
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
!     \item[{localDE}]
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
!     \item[{keyName}]
!          The key to get the information from.
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
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          farray is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 type(ESMF_LocalArray) :: larray
 type(ESMF_CopyFlag) :: docopyInt
 integer :: localDECount

 ! Initialize return code 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit, locstream, rc) 

 
 ! Set Defaults
 if (present(docopy)) then
    docopyInt=docopy
 else
    docopyInt=ESMF_DATA_REF
 endif

 ! Get localDECount
 call ESMF_LocStreamGetDefault(locstream, localDECount=localDECount, rc=localrc)
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
     ESMF_CONTEXT, rcToReturn=rc)) return
 
 ! Check consistency  of localDE
 if (localDeCount < 0) then 
    call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
           "- Negative number of localDeCount prohibits request", & 
           ESMF_CONTEXT, rc) 
    return 
 endif 

 if (localDeCount == 0) then 
    call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
           "- localDeCount == 0 prohibits request", & 
           ESMF_CONTEXT, rc) 
    return 
 endif
 
 if (localDE>=localDeCount) then 
    call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
           "- localDE too big", & 
           ESMF_CONTEXT, rc) 
    return 
 endif 

 if (localDE<0) then 
    call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
           "- localDE can't be less than 0", & 
           ESMF_CONTEXT, rc) 
    return 
 endif 

 ! Get Key Array
 call ESMF_LocStreamGetKeyArray(locstream, keyName=keyName, keyArray=array, rc=localrc)  
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                         ESMF_CONTEXT, rcToReturn=rc)) return

 ! Obtain the native array pointer via the LocalArray interface 
 call ESMF_ArrayGet(array, localDE=localDE, larray=larray, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGet(larray, farray, doCopy, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 

  ! Get Bounds via C++
   call c_ESMC_locstreamgetkeybnds(array, localDE, & 
                 exclusiveLBound, exclusiveUBound, exclusiveCount, &
                 computationalLBound, computationalUBound, computationalCount, &
                 totalLBound, totalUBound, totalCount, &
                 localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return


 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetKeyR8


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetBounds"
!BOP
! !IROUTINE: ESMF_LocStreamGet - Get the local bounds of a location stream

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGet()
      subroutine ESMF_LocStreamGetBounds(locstream, localDE, & 
          exclusiveLBound, exclusiveUBound, exclusiveCount,     &
          computationalLBound, computationalUBound, computationalCount,     &
          rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream),   intent(in) :: locstream
      integer,                intent(in) :: localDE
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
!          The local DE to get the information for. {\tt [0,..,localDeCount-1]}
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

 !!!!! REMOVE THESE BECAUSE IT'S DONE IN THE C++ CALLS
 !! Get localDECount
 !call ESMF_LocStreamGetDefault(locstream, localDECount=localDECount, rc=localrc)
 !if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
 !    ESMF_CONTEXT, rcToReturn=rc)) return
 !
 !! Check consistency  of localDE
 !if (localDeCount < 0) then 
 !   call ESMF_LogMsgSetError(ESMF_RC_CANNOT_GET, & 
 !          "- Negative number of localDeCount prohibits request", & 
 !          ESMF_CONTEXT, rc) 
 !   return 
 !endif 
 !
 !if (localDE>=localDeCount) then 
 !   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 !          "- localDE too big", & 
 !          ESMF_CONTEXT, rc) 
 !   return 
 !endif 
 !
 !if (localDE<0) then 
 !   call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
 !          "- localDE can't be less than 0", & 
 !          ESMF_CONTEXT, rc) 
 !   return 
 !endif 

 ! Get locstream type object
 lstypep=>locstream%lstypep

 ! Get exclusiveLBound
 if (present(exclusiveLBound)) then
    call c_ESMC_locstreamgetelbnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             exclusiveLBound, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return
 endif 

 ! Get exclusiveUBound
 if (present(exclusiveUBound)) then
    call c_ESMC_locstreamgeteubnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             exclusiveUBound, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return
 endif 

 ! Get exclusiveCount
 if (present(exclusiveCount)) then
    call c_ESMC_locstreamgetelbnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             tmpLBnd, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_locstreamgeteubnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             tmpUBnd, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

    exclusiveCount=tmpUBnd-tmpLBnd+1
 endif 

 ! For now computational bounds are the same as exclusive bounds

 ! Get computationalLBound
 if (present(computationalLBound)) then
    call c_ESMC_locstreamgetelbnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             computationalLBound, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return
 endif 

 ! Get computationalUBound
 if (present(computationalUBound)) then
    call c_ESMC_locstreamgeteubnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             computationalUBound, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return
 endif 

 ! Get computationalCount
 if (present(computationalCount)) then
    call c_ESMC_locstreamgetelbnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             tmpLBnd, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_locstreamgeteubnd(lstypep%distgrid, localDE, lstypep%indexflag, & 
             tmpUBnd, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

    computationalCount=tmpUBnd-tmpLBnd+1
 endif 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetBounds

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamMatch()"
!BOPI
! !IROUTINE: ESMF_LocStreamMatch - Check if two Locstream objects match

! !INTERFACE:
  function ESMF_LocStreamMatch(locstream1, locstream2, rc)
!
! !RETURN VALUE:
    logical :: ESMF_LocStreamMatch
      
! !ARGUMENTS:
    type(ESMF_Locstream),  intent(in)              :: locstream1
    type(ESMF_Locstream),  intent(in)              :: locstream2
    integer,          intent(out),  optional  :: rc  
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

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamPrint"

!BOP
! !IROUTINE:  ESMF_LocStreamPrint - Print the contents of a LocStream

! !INTERFACE:
      subroutine ESMF_LocStreamPrint(locstream, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(inout) :: locstream 
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc
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

        ! Initialize
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

        !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
        !TODO: Remove the following dummy test when dummy argument actually used
        if (options==options) continue

        write(*,*) "LocStream Print Starts ====>"

        ! Get internal pointer to locstream
        lstypep => locstream%lstypep

        ! print option is not implemented, but it has to pass to c_ESMC_BasePrint()
        defaultopts = "brief"

        call c_ESMC_GetName(lstypep%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        write(*, *)  "Name =     '",  trim(name), "'" 
        write(*, *)  "KeyCount =",lstypep%keyCount
        write(*,*) "Keys:"
        do i=1,lstypep%keyCount
           write(*,*) "   ",trim(lstypep%keyNames(i))," - ",trim(lstypep%keyLongNames(i)),"    ",trim(lstypep%keyUnits(i))
        enddo

        write(*,*) "LocStream Print Ends   ====>"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_LocStreamPrint


#define FINISH_LATER
#ifdef FINISH_LATER
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
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

     ! Serialize Distgrid
     call c_ESMC_DistgridSerialize(lstypep%distgrid, buffer, length, offset, &
                                 linquireflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! Serialize other locstream items
      call c_ESMC_LocStreamSerialize(lstypep%indexflag, lstypep%keyCount, &
              buffer, length, offset, linquireflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! Serialize locstream key info
      do i=1,lstypep%keyCount
         ! Serialize key info
         call c_ESMC_LocStreamKeySerialize(&
                  len_trim(lstypep%keyNames(i)), lstypep%keyNames(i), &
                  len_trim(lstypep%keyUnits(i)), lstypep%keyUnits(i), &
                  len_trim(lstypep%keyLongNames(i)), lstypep%keyLongNames(i), &
                 buffer, length, offset, linquireflag, localrc)
         if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

         ! Serialize key Array
         call c_ESMC_ArraySerialize(lstypep%keys(i), buffer, length, offset, &
          attreconflag, linquireflag, localrc)
         if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

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
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating LocStream type object", &
                                     ESMF_CONTEXT, rc)) return

     ! Deserialize Base
     attreconflag = ESMF_ATTRECONCILE_OFF
     call c_ESMC_BaseDeserialize(lstypep%base, buffer,  offset, &
      attreconflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call ESMF_BaseSetInitCreated(lstypep%base, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return


     ! Deserialize Distgrid
     call c_ESMC_DistGridDeserialize(lstypep%distgrid, buffer, offset, localrc)
     if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call ESMF_DistGridSetInitCreated(lstypep%distgrid, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return


      ! Deserialize other locstream items
      call c_ESMC_LocStreamDeserialize(lstypep%indexflag, lstypep%keyCount, &
              buffer, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return


      ! Allocate arrays for names, etc.
      allocate (lstypep%keyNames(lstypep%keyCount), stat=localrc )
      if (ESMF_LogMsgFoundAllocError(localrc, " Allocating KeyNames", &
                                     ESMF_CONTEXT, rc)) return
      allocate (lstypep%keyUnits(lstypep%keyCount), stat=localrc )
      if (ESMF_LogMsgFoundAllocError(localrc, " Allocating units", &
                                     ESMF_CONTEXT, rc)) return
      allocate (lstypep%keyLongNames(lstypep%keyCount), stat=localrc )
      if (ESMF_LogMsgFoundAllocError(localrc, " Allocating longNames", &
                                     ESMF_CONTEXT, rc)) return
      allocate( lstypep%keys(lstypep%keyCount), stat=localrc )  ! Array of keys
      if (ESMF_LogMsgFoundAllocError(localrc, " Allocating keys", &
                                     ESMF_CONTEXT, rc)) return
      allocate( lstypep%destroyKeys(lstypep%keyCount), stat=localrc )  ! Array of keys
      if (ESMF_LogMsgFoundAllocError(localrc, " Allocating keys", &
                                     ESMF_CONTEXT, rc)) return

      ! Serialize locstream key info
      do i=1,lstypep%keyCount
         ! Deserialize key info
         call c_ESMC_LocStreamKeyDeserialize(&
                  lstypep%keyNames(i), &
                  lstypep%keyUnits(i), &
                  lstypep%keyLongNames(i), &
                 buffer, offset, localrc)
         if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

         ! Deserialize key Array
         call c_ESMC_ArrayDeserialize(lstypep%keys(i), buffer, offset, &
          attreconflag, localrc)
         if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

         call ESMF_ArraySetInitCreated(lstypep%keys(i), rc=localrc)
         if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

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

#endif

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamValidate"

!BOP
! !IROUTINE:  ESMF_LocStreamValidate - Check validity of a LocStream

! !INTERFACE:
      subroutine ESMF_LocStreamValidate(locstream, options, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(inout) :: locstream 
      character (len = *), intent(in), optional :: options 
      integer, intent(out), optional :: rc   
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
!     \item [{[options]}]
!           Validation options are not yet supported.
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

      !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
      !TODO: Remove the following dummy test when dummy argument actually used
      if (options==options) continue

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

     ESMF_INIT_CHECK_SHALLOW(ESMF_LocStreamTypeGetInit,ESMF_LocStreamTypeInit,s)

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
!      \item[{[name]}]
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
        if ( ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                   "LocStream object not location stream", &
                                   ESMF_CONTEXT, rc) ) return
        return
      endif

! check grid status
      if (igridIn%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to copy an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Initialize pointers
      nullify(igrid)
      nullify(ESMF_LocStreamCreateLocStreamCopy%ptr)


      allocate(igrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating LocStream object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize igrid internals.
      call ESMF_LocStreamConstructNew(igrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

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
      if (ESMF_LogMsgFoundAllocError(localrc, "locStream",              &
                                     ESMF_CONTEXT, rc)) return
      ls => igrid%igridSpecific%locStream

!
! Attach keys
!
      do k=1, lsIn%keyCount
        call ESMF_LocalArrayGetThis( lsIn%keys(k), ptr )
        if ( ptr /= ESMF_NULL_POINTER ) then
          ls%keys(k) = ESMF_LocalArrayCreate( lsIn%keys(k), localrc )
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
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
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! now that it's created, add the interndg to the igrid
      call ESMF_LocStreamAddInternDG(igrid, internDG, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

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
!      \item[{[name]}]
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
!     ranges(1) = ESMF_LocalArrayCreate( i4Array, ESMF_DATA_COPY )
!     values(2) = ESMF_LocalArrayCreate( r4Array, ESMF_DATA_COPY )
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
!     ranges(2) = ESMF_LocalArrayCreate( r4Array, ESMF_DATA_COPY )
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

      type(ESMF_TypeKind)              :: keyKind       ! key kind
      integer                          :: localKind

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      if (igridIn%ptr%horzLocStreamType /= ESMF_IGRID_TYPE_LOCATIONSTREAM ) then
        if ( ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                   "LocStream object not location stream", &
                                   ESMF_CONTEXT, rc) ) return
        return
      endif

! check igrid status
      if (igridIn%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to copy an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Initialize pointers
      nullify(igrid)
      nullify(ESMF_LocStreamCreateLocStreamSubset%ptr)

      allocate(igrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating LocStream object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize igrid internals.
      call ESMF_LocStreamConstructNew(igrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

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
      if (ESMF_LogMsgFoundAllocError(localrc, "locStream",              &
                                     ESMF_CONTEXT, rc)) return
      ls => igrid%igridSpecific%locStream

      allocate( ls%dist( size(lsIn%dist) ) )

      if ( present( values ) .or. present( ranges ) ) then

!
! First pull out the sorting keys
!
        nLocalActive = lsIn%nLocalActive 
        allocate ( selected(nLocalActive), stat=localrc )
        if (ESMF_LogMsgFoundAllocError(localrc, "locStream",              &
                                     ESMF_CONTEXT, rc)) return

!  All data are selected to begin with
!  -----------------------------------
        selected = .true.

        i = 0
        string = trim( keyNames )
        do while ( string /= '' )
          i = i + 1

          if ( i > lsIn%keyCount ) then
            dummy = ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                          "TOO MANY KEYS SPECIFIED", &
                                          ESMF_CONTEXT, rc)
          endif
          call ESMF_StripKey( string, key )

          call ESMF_LSGetLocalArrayKey(lsIn, key, lArray, keyLen, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if ( keyLen /= lsIn%nLocalActive ) then
            dummy = ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                          "size of array does not match", &
                                          ESMF_CONTEXT, rc)
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
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
          endif
        enddo

!
! Communication part of code:  can this be removed to make the
! routine communication-free?
!
        call ESMF_DELayoutGet(delayout, vm=vm, deCount=deCount,         &
                              oneToOneFlag=otoFlag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,           &
                                  ESMF_CONTEXT, rc)) return

        if (otoflag .ne. ESMF_TRUE) then
          dummy = ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                        "DELAYOUT not one-to-one", &
                                        ESMF_CONTEXT, rc)
          return
        endif

        call ESMF_VMGet( vm, localPet = localPet,                       &
                         petCount = petCount, rc=localrc )
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,           &
                                  ESMF_CONTEXT, rc)) return

        if ( petCount /= deCount ) then
          dummy = ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD,                &
                                        "PETs to DEs not one-to-one",   &
                                        ESMF_CONTEXT, rc)
          return
        endif

        ls%dist    = 0

        call ESMF_VMAllGather(vm, sendData=(/ls%nLocalActive/),         &
                              recvData=ls%dist, count=1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      
        counts(1) = sum( ls%dist(1:deCount) )
        if ( counts(1) > ls%maxGlobal ) then
          dummy = ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                        "TOO MANY TOTAL LOCATIONS", &
                                        ESMF_CONTEXT, rc)
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
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
            call ESMF_LocalArrayGet( lsIn%keys(k), kind = keyKind, rc=localrc )
            if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
            localkind = keyKind%dkind
          endif
        enddo

!
! In the trivial case, just sort the locations locally by the key names
!
        call ESMF_LSSortLocal( ls, keyNames, rc=localrc )
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif


!
! Determine the internal DG (later just DistGrid)
!
      internDG = ESMF_InternDGCreate( 1, (/sum(ls%dist)/), delayout, &
                                      (/1/), ls%dist, rc=localrc )
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! now that it's created, add the interndg to the igrid
      call ESMF_LocStreamAddInternDG(igrid, internDG, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

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
!     \item[{[name]}]
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
      type(ESMF_TypeKind)              :: keyKind       ! key kind
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
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating LocStream object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize igrid internals.
      call ESMF_LocStreamConstructNew(igrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
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
      if (ESMF_LogMsgFoundAllocError(localrc, "locStream", &
                                     ESMF_CONTEXT, rc)) return
      call ESMF_InternDGGetDELayout(igridsIn(1)%ptr%internDGs(1)%ptr, &
                                    delayout, localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "locStream", &
                                     ESMF_CONTEXT, rc)) return

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
          if (ESMF_LogMsgFoundError(ESMF_RC_NOT_FOUND, &
                                    "LocStream array inconsistent", &
                                    ESMF_CONTEXT, rc)) return
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
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          localkind = keyKind%dkind

          select case (localkind)
            case (ESMF_TYPEKIND_I4%dkind)
 
              allocate( intArrayNew( ls%nLocalActive ) )
              count = 0
              do j=1, size(igridsIn)
                call ESMF_LocalArrayGetData( lsIn%keys(k), intPtr,   &
                                             ESMF_DATA_REF, localrc )
                if (ESMF_LogMsgFoundError(localrc, &
                                          ESMF_ERR_PASSTHRU, &
                                          ESMF_CONTEXT, rc)) return
                do i=1, lsIn%nLocalActive
                  count = count + 1
                  intArrayNew(count) = intPtr(i)     ! Copy key
                enddo
              enddo
              ls%keys(k) = ESMF_LocalArrayCreate( intArrayNew, &
                                                  ESMF_DATA_COPY, localrc )
              if (ESMF_LogMsgFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc )) return

            case (ESMF_TYPEKIND_R8%dkind)

              allocate( realArrayNew( ls%nLocalActive ) )
              count = 0
              do j=1, size(igridsIn)
                call ESMF_LocalArrayGetData( lsIn%keys(k), realPtr,  &
                                             ESMF_DATA_REF, localrc )
                if (ESMF_LogMsgFoundError(localrc, &
                                          ESMF_ERR_PASSTHRU, &
                                          ESMF_CONTEXT, rc)) return
                
                do i=1, lsIn%nLocalActive
                  count = count + 1
                  realArrayNew(count) = realPtr(i)   ! Copy key
                enddo
              enddo
              ls%keys(k) = ESMF_LocalArrayCreate(RealArrayNew, &
                                                 ESMF_DATA_COPY, localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return

            case default
              if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                        "Unsupported Key Type", &
                                        ESMF_CONTEXT, rc) ) return
          end select

        endif
      
      enddo

!
! Determine the new decomposition
!

! Determine the internal DG (later just DistGrid)

      internDG = ESMF_InternDGCreate( 1, (/sum(ls%dist)/), delayout, &
                                      (/1/), ls%dist, rc=localrc )
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! now that it's created, add the interndg to the igrid
      call ESMF_LocStreamAddInternDG(igrid, internDG, localrc)

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

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
!     \item[{[name]}]
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
        if ( ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                   "LocStream object not location stream", &
                                   ESMF_CONTEXT, rc) ) return
        return
      endif

! check igrid status
      if (igridIn%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to copy an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

     ! Initialize pointers
      nullify(igrid)
      nullify(ESMF_LocStreamCreateLocStreamByKeys%ptr)

      allocate(igrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating LocStream object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize igrid internals.
      call ESMF_LocStreamConstructNew(igrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

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
      if (ESMF_LogMsgFoundAllocError(localrc, "locStream", &
                                     ESMF_CONTEXT, rc)) return

!     To bind with ESMF:  allocate the specific igrid, bind to lsOut
      lsOut => igrid%igridSpecific%locStream

!
! Attach keys
!
      do k=1, lsIn%keyCount
        call ESMF_LocalArrayGetThis( lsIn%keys(k), ptr )
        if ( ptr /= ESMF_NULL_POINTER ) then
          lsOut%keys(k) = ESMF_LocalArrayCreate( lsIn%keys(k), localrc )
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
      enddo

!
      call ESMF_LocStreamGetDELayout(igridIn, delayout, rc=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "locStream", &
                                     ESMF_CONTEXT, rc)) return

!
! Distribution part of Create
      call ESMF_DELayoutGet(delayout, vm=vm, deCount=deCount,            &
                            oneToOneFlag=otoFlag, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (otoflag .ne. ESMF_TRUE) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                      "DELAYOUT not one-to-one", &
                                      ESMF_CONTEXT, rc)
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
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        allocate( indx(nLocalActive) )
        call ESMF_MSIndexSet( nLocalActive, indx )
        call ESMF_MSIndexSort( indx, lArray, .false., localrc )
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

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
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

!
! Now consolidate and gather all the new local sizes
!
        lsOut%dist  = 0
        call ESMF_VMAllGather(vm, sendData=(/lsOut%nLocalActive/), &
                              recvData=lsOut%dist, count=1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      
        counts(1) = sum( lsOut%dist(1:deCount) )
        if ( counts(1) > lsOut%maxGlobal ) then
          dummy = ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                        "TOO MANY TOTAL LOCATIONS", &
                                        ESMF_CONTEXT, rc)
          return
        endif

!
! Determine the internal DG (later just DistGrid)
        internDG = ESMF_InternDGCreate( 1, counts, delayout, &
                                        (/1/), lsOut%dist, rc=localrc )
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! now that it's created, add the interndg to the igrid
        call ESMF_LocStreamAddInternDG(igrid, internDG, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        deallocate( indx )

      endif

!
! Finally, sort everything locally with all the sort keys
!
      call ESMF_LSSortLocal( lsOut, sortKeys, localrc )
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

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
      type(ESMF_TypeKind)                 :: keyKind       ! key kind
      integer                             :: localkind
      integer                             :: localrc
      integer                             :: iDE, index, partitionSize, part

      partitionSize = (nLocalActive / deCount)


      call ESMF_LocalArrayGetThis( lArray, ptr )
      if ( ptr /= ESMF_NULL_POINTER ) then 
        call ESMF_LocalArrayGet( lArray, kind = keyKind, rc = localrc )
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        localkind = keyKind%dkind

        select case (localkind)
          case (ESMF_TYPEKIND_I4%dkind)
            call ESMF_LocalArrayGetData( lArray, i4ptr,          &
                                         ESMF_DATA_REF, localrc )
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

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
                                   ESMF_MAX )

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
                                         ESMF_DATA_REF, localrc )
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return


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
                                   ESMF_MAX )

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
                                     ESMF_DATA_REF, localrc )
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return

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
                                   ESMF_MAX )

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
           if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                     "Unsupported Key Type", &
                                     ESMF_CONTEXT, rc) ) return
        endselect
      endif
      
      end subroutine collectLAStats

      end function ESMF_LocStreamCreateByKeys
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreateByBkg"
!BOPI
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream by projecting onto a Grid

! !INTERFACE:
      ! NOT YET INTEGRATED
      ! Private name; call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateByBkg(locstream, name, keyNames, &
                 backgroundGrid, rc)

!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateByBkg

!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(inout)          :: locstream
      character (len=*),    intent(in)             :: name
      character (len=*),    intent(in)             :: keyNames
      type(ESMF_Grid),      intent(inout)          :: backgroundGrid
      integer,              intent(out),  optional :: rc
!
! !DESCRIPTION:
!
!     Create an location stream from an existing one in accordance with 
!     the distribution of the background Grid.  The {\tt keyNames} specify
!     which key(s) will form the overlay to the background Grid.  
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream from which the new location stream is to be created
!      \item[{[name]}]
!          Name of the resulting location stream
!      \item[keyNames]
!          Names of the keys used to determine the link to background Grid.
!          The first key in this list matches up with the first coordinate of the 
!          Grid, the second key in this list matches up with the second coordinate
!          of the Grid, and so on. 
!      \item[backgroundGrid]
!          Background Grid which determines the distribution by "overlaying"
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

     type(ESMF_LocStreamClass), pointer:: igrid         ! Pointer to new igrid
     type(ESMF_LocStream), pointer :: lsIn          ! Pointer to LS
     type(ESMF_LocStream), pointer :: lsOut         ! Pointer to new LS
     type(ESMF_InternDG)           :: internDG      ! Internal distgrid
     type(ESMF_DELayout)           :: delayout      ! Layout
     type(ESMF_VM)                 :: vm            ! Virtual machine
     type(ESMF_Logical)            :: otoFlag       ! One-to-one flag
     type(ESMF_Pointer)            :: ptr

     integer                       :: localrc       ! Error status
     logical                       :: dummy
     integer                       :: i, j, k, off
     integer                       :: numDims
     integer                       :: newCount, oldcount
     integer                       :: iDE, deCount
     integer                       :: counts(1)     ! for InternDG
     integer, allocatable          :: indx(:)
     integer, allocatable          :: blockSizes(:)
     integer, pointer              :: intPtr(:)
     logical, allocatable          :: mask(:)

     character(len=ESMF_MAXSTR)    :: keytemp, string

     character(len=ESMF_MAXSTR), allocatable   :: units(:)
     type(ESMF_LocalArray), allocatable        :: coords(:)
     type(ESMF_LocalArray)                     :: ownerDE

! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      if (igridIn%ptr%horzIgridType /= ESMF_IGRID_TYPE_LOCATIONSTREAM ) then
        if ( ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                   "LocStream object not location stream", &
                                   ESMF_CONTEXT, rc) ) return
        return
      endif

! check igrid status
      if (igridIn%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to copy an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

!
! Currently LATLON and LATLON_UNI are supported,
! but later all logically rectangular grids
!
      if (background%ptr%horzLocStreamType /= ESMF_IGRID_TYPE_LATLON_UNI .and. &
          background%ptr%horzLocStreamType /= ESMF_IGRID_TYPE_LATLON ) then
        if ( ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                   "Background igrid not lat-lon", &
                                   ESMF_CONTEXT, rc) ) return
        return
      endif

! check igrid status
      if (background%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to copy an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

     ! Initialize pointers
      nullify(igrid)
      nullify(ESMF_LocStreamCreateLocStreamByBkg%ptr)

      allocate(igrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating LocStream object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize igrid internals.
      call ESMF_LocStreamConstructNew(igrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

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
      if (ESMF_LogMsgFoundAllocError(localrc, "locStream", &
                                     ESMF_CONTEXT, rc)) return

!     To bind with ESMF:  allocate the specific igrid, bind to lsOut
      lsOut => igrid%igridSpecific%locStream

!
! Attach keys
!
      do k=1, lsIn%keyCount
        call ESMF_LocalArrayGetThis( lsIn%keys(k), ptr )
        if ( ptr /= ESMF_NULL_POINTER ) then
          lsOut%keys(k) = ESMF_LocalArrayCreate( lsIn%keys(k), localrc )
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
      enddo

!
      call ESMF_LocStreamGetDELayout(igridIn, delayout, rc=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "locStream", &
                                     ESMF_CONTEXT, rc)) return

!
! Distribution part of Create
      call ESMF_DELayoutGet(delayout, vm=vm, deCount=deCount,            &
                            oneToOneFlag=otoFlag, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (otoflag .ne. ESMF_TRUE) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                      "DELAYOUT not one-to-one", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      numDims = 0
      string = trim( keyNames )
      do while ( string /= '' )
         call ESMF_StripKey( string, keytemp )
         numDims = numDims + 1
      enddo

      if (numDims > lsIn%keyCount) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                      "Too many keys", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      allocate( units( numDims ) )
      allocate( coords( numDims ) )

      string = trim( keyNames )
      do i = 1, numDims
         call ESMF_StripKey( string, keytemp )
         call ESMF_LSGetLocalArrayKey(lsIn, keytemp, coords(i), &
                                      count=newCount, units=units(i), &
                                      rc=localrc)
         if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rc)) return
         if ( i > 1 .and. newCount /= oldcount ) then
            if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "Unsupported Key Type", &
                                      ESMF_CONTEXT, rc) ) return
         endif
         oldcount = newCount
      enddo

!
! Determine the owners of all of the locations in the stream
! by projecting them on to the background igrid.  Every igrid
! should supply a routine to determine ownership.
! TODO: make a case distinction for various igrids.
!

      call ESMF_LRLocStreamIsOnDE(background, units, coords, newCount, ownerDE, &
                             rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

!
! ownerDE contains the destinations of the individual items
! (== 100000 if ESMF_LRLocStreamIsOnDE could not determine the DE)
! Now sort the local location stream; note that the undefined
! entries will be pushed to the end of the key arrays.
!

      allocate( indx(newCount) )
      call ESMF_LocalArrayGetData(ownerDE, intPtr,         &
                                  docopy=ESMF_DATA_REF, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_MSIndexSet( newCount, indx )
      call ESMF_MSIndexSort( indx, intPtr, descend=.false., stat=localrc )
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
!
! Determine the number of key entries to be sent to each DE
!
      allocate( mask(newCount) )
      allocate( blockSizes(deCount) )
      do iDE=1, deCount
        where ( intPtr == (iDE-1) )
          mask = .true.
        elsewhere
          mask = .false.
        endwhere
        blockSizes( iDE ) = count( mask )
      enddo
      deallocate( mask )

      newCount = sum(blockSizes)   ! Only those which are defined

!
! Redistribute the keys, as dictated by indx and blockSizes
! Note that undefined owners are at the end of the local (permuted)
! arrays and therefore are not communicated
!
      call ESMF_LSRedistribute( lsOut, vm, blockSizes, indx, rc)

!
! Determine the new decomposition
!
      allocate( lsOut%dist(deCount) )

!
! Now consolidate and gather all the new local sizes
!
      lsOut%dist  = 0
      call ESMF_VMAllGather(vm, sendData=(/lsOut%nLocalActive/), &
                            recvData=lsOut%dist, count=1, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      counts(1) = sum( lsOut%dist(1:deCount) )
      if ( counts(1) > lsOut%maxGlobal ) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                      "TOO MANY TOTAL LOCATIONS", &
                                      ESMF_CONTEXT, rc)
        return
      endif

! Determine the internal DG (later just DistGrid)

      internDG = ESMF_InternDGCreate( 1, counts, delayout, &
                                      (/1/), lsOut%dist, rc=localrc )
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! now that it's created, add the interndg to the igrid
      call ESMF_LocStreamAddInternDG(igrid, internDG, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      deallocate( blockSizes )
      deallocate( indx )

      deallocate( coords )
      deallocate( units )

      igrid%igridStorage = ESMF_IGRID_STORAGE_ARBITRARY
      igrid%igridStatus  = ESMF_IGRID_STATUS_READY

      ! Set return values.
      ESMF_LocStreamCreateLocStreamByBkg%ptr => igrid
      ESMF_INIT_SET_CREATED(ESMF_LocStreamCreateLocStreamByBkg)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateByBkg
#endif




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
      type(ESMF_TypeKind) ::keyTypeKind
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
     call ESMF_VMGetGlobal(vm, rc=localrc)
     if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
         ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
     if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
         ESMF_CONTEXT, rcToReturn=rc)) return


     ! Allocate stuff for AllToAllV 
     allocate(sndCounts(petCount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating sndCounts", &
         ESMF_CONTEXT, rc)) return         

     allocate(sndOffsets(petCount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating sndOffsets", &
         ESMF_CONTEXT, rc)) return         

     allocate(rcvCounts(petCount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating rcvCounts", &
         ESMF_CONTEXT, rc)) return         

     allocate(rcvOffsets(petCount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating sndCounts", &
         ESMF_CONTEXT, rc)) return         


     ! Allocate first set of buffers for commmunicating sizes
     allocate(sndSizes(petCount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating sndCounts", &
         ESMF_CONTEXT, rc)) return         

     allocate(rcvSizes(petCount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating sndCounts", &
         ESMF_CONTEXT, rc)) return         


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
     if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
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
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating pntList", &
                                     ESMF_CONTEXT, rc)) return         


     ! Get number of localDEs
     call ESMF_LocStreamGet(locstream, localDECount=localDECount, rc=localrc)
     if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
         ESMF_CONTEXT, rcToReturn=rc)) return


     ! Loop getting seqIndices
     pos=1
     do lDE=0,localDECount-1

        ! Get number of seqIndices
        call  ESMF_DistGridGet(oldLStypep%distgrid, localDe=lDE, &
                elementCount=seqCount, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return


        ! Make sure we aren't going to overrun memory
        if ((pos+seqCount-1) >petListCount) then
	     if (ESMF_LogMsgFoundError(ESMF_RC_ARG_WRONG, &
                 " - Too many seq indices in locstream disgrid", &
                ESMF_CONTEXT, rc)) return               
	endif

        ! Get list of seqindices
        call  ESMF_DistGridGet(oldLStypep%distgrid, localDe=lDE, &
                seqIndexList=seqInd(pos:pos+seqCount-1), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
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
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating sndCounts", &
         ESMF_CONTEXT, rc)) return         

     ! Allocate sndSeqInd
     allocate(sndSeqInd(petListCount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating sndSeqInd", &
                                     ESMF_CONTEXT, rc)) return         


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
     if (ESMF_LogMsgFoundAllocError(localrc, "Allocating rcvSeqInd", &
                                     ESMF_CONTEXT, rc)) return         



     ! Communicate sequence indices
     call ESMF_VMAllToAllV(vm, sndSeqInd, sndCounts, sndOffsets, &
            rcvSeqInd, rcvCounts, rcvOffsets, rc=localrc)
     if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
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
     if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return


     ! Deallocate sndSeqInd
     deallocate(rcvSeqInd)


     ! Create new locStream
     ESMF_LocStreamCreatePetList=ESMF_LocStreamCreateFromNewDG(locstream, name, &
                   distgrid=newDistgrid, destroyDistgrid=.true., rc=localrc)
     if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return     


     ! Return success
     if (present(rc)) rc = ESMF_SUCCESS

     end function ESMF_LocStreamCreatePetList


!------------------------------------------------------------------------------
#undef  ESMF_METHOD  
#define ESMF_METHOD "ESMF_LocStreamCreateFromNewDG"
!BOPI
! !IROUTINE: ESMF_LocStreamCreate - Create a new LocStream from an old one and a distgrid

! !INTERFACE:  
      function ESMF_LocStreamCreateFromNewDG(locstream, name, distgrid, &
                 destroyDistgrid, rc)

!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateFromNewDG

!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in)                :: locstream
      character (len=*),    intent(in), optional      :: name
      type(ESMF_DistGrid),  intent(in)                :: distgrid
      logical,              intent(in)                :: destroyDistgrid
      integer,              intent(out), optional     :: rc
!
! !DESCRIPTION:
!
!     Create a location stream from an existing one and a new distgrid.
!     The data is copied from the old locstream to the new one. 
!     Currently this is an internal subroutine not intended for public use. 
!
!     The arguments are:
!     \begin{description}
!      \item[locstream]
!          Location stream from which the new location stream is to be created
!      \item[{[name]}]
!          Name of the resulting location stream
!      \item[distgrid]
!          Distgrid for new distgrid
!      \item[destroyDistgrid]
!          If true, then destroy the distgrid when the locstream is destroyed
!      \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      type(ESMF_LocStreamType), pointer :: oldLStypep, newLStypep
      type(ESMF_LocStream):: newLocStream
      type(ESMF_ArrayBundle) :: oldAB, newAB
      type(ESMF_RouteHandle) :: routehandle
      type(ESMF_TypeKind) ::keyTypeKind
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


      ! Create new locStream
      newLocStream=ESMF_LocStreamCreateFromDG(name=name, distgrid=distgrid, &
                destroyDistgrid=destroyDistgrid, indexflag=oldLSTypep%indexFlag, rc=localrc) 
      if (ESMF_LogMsgFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return     


      ! Add keys to new Locstream
      ! NOTE: We need a subroutine to add a list of keys. This is inefficient because of the allocations 
      !       and searching for already in
      keyCount=oldLStypep%keyCount
      do i=1,keyCount

         ! get key typeKind
         call ESMF_ArrayGet(oldLStypep%keys(i), typekind=keyTypeKind, rc=localrc)
         if (ESMF_LogMsgFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) return     

         call ESMF_LocStreamAddKey(newLocStream, oldLstypep%keyNames(i), keyTypeKind, &
                       oldLstypep%keyUnits(i), oldLstypep%keyLongNames(i), rc=localrc)
         if (ESMF_LogMsgFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) return     
      enddo


      ! Get new locstream internal pointer
      newLStypep=>newLocStream%lstypep

      ! NOTE THAT THIS ONLY WORKS BECAUSE THE LocStreamAddKey SUBROUTINE
      ! ADDS KEYS AT THE END. IF THIS CHANGES THIS'LL STOP WORKING. 
      ! FOR EFFICENCY REASONS I'LL LEAVE IT FOR NOW. IF IT CHANGES
      ! REWRITE TO NOT DEPEND ON ORDER


      ! Redistribute data from one locstream to another 

      ! Create ArrayBundles for redistribution
      oldAB=ESMF_ArrayBundleCreate(oldLStypep%keys, rc=localrc)      
       if (ESMF_LogMsgFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) return     

      newAB=ESMF_ArrayBundleCreate(newLStypep%keys, rc=localrc)      
       if (ESMF_LogMsgFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) return     


      ! Setup for redist
      call ESMF_ArrayBundleRedistStore(srcArrayBundle=oldAB, dstArrayBundle=newAB, &
             routehandle=routeHandle, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) return     

      ! Do redist
      call ESMF_ArrayBundleRedist(srcArrayBundle=oldAB, dstArrayBundle=newAB, &
            routehandle=routeHandle, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) return     

      ! Get rid of routehandle
      call  ESMF_ArrayBundleRedistRelease(routehandle=routehandle, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) return     

      ! Get rid of ArrayBundles
      call ESMF_ArrayBundleDestroy(oldAB, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) return     

      call ESMF_ArrayBundleDestroy(newAB, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) return     

     ! Output new locstream
     ESMF_LocStreamCreateFromNewDG=newLocStream

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LocStreamCreateFromNewDG



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
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

      ! Get locstream internal pointer
      lstypep=>locstream%lstypep


      ! Loop through DEs and total up number of entries
      localCount=0
      do lDE=0,localDECount-1
         call c_ESMC_locstreamgetelbnd(lstypep%distgrid, lDE, lstypep%indexflag, & 
                 tmpLBnd, localrc)
         if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
               ESMF_CONTEXT, rcToReturn=rc)) return

         call c_ESMC_locstreamgeteubnd(lstypep%distgrid, lDE, lstypep%indexflag, & 
                 tmpUBnd, localrc)
         if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
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
      type(ESMF_TypeKind)           :: coordTypeKindList(3)
      real(ESMF_KIND_R8), pointer :: keyDataR8(:)
      real(ESMF_KIND_R4), pointer :: keyDataR4(:)
      integer (ESMF_KIND_I4), pointer :: keyDataI4(:)
      integer :: dim
      integer :: i,j,pos
      type(ESMF_TypeKind) :: typekind

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
	     if (ESMF_LogMsgFoundError(ESMF_RC_ARG_WRONG, &
                 " - too many coordinate key names", &
                ESMF_CONTEXT, rc)) return
          endif

	  ! Pull out coordinate name
          call ESMF_StripKey( string, coordKeyList(dim))

          ! advance to next position
          dim = dim + 1
       enddo

       ! check pntDim
       if (pntDim /= dim-1) then
	  if (ESMF_LogMsgFoundError(ESMF_RC_ARG_WRONG, &
           " - number of coordinate key names doesn't match pnt dimension", &
           ESMF_CONTEXT, rc)) return
       endif


      ! Get number of localDEs
      call ESMF_LocStreamGet(locstream, localDECount=localDECount, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return



     ! Get pnt coordinates
     !! TODO: this is doing a couple of internal subroutine searches to match the key with the name
     !!       at some point it might make sense to get rid of these
     do i=1,pntDim

       ! Get typeKind
       call ESMF_LocStreamGetKey(locstream,keyName=trim(coordKeyList(i)), typekind=typekind, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
            ESMF_CONTEXT, rcToReturn=rc)) return

	! Copy data based on typekind
	if (typekind .eq. ESMF_TYPEKIND_R8) then
           pos=i
           do lDE=0,localDECount-1
              ! Get data
              call  ESMF_LocStreamGetKey(locstream, localDE=lDE, keyName=trim(coordKeyList(i)), &
                      exclusiveLBound=tmpLBnd, exclusiveUBound=tmpUBnd, farray=keyDataR8, rc=localrc)
              if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
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
              if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
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
              if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                  ESMF_CONTEXT, rcToReturn=rc)) return

              ! put into point list
              do j=tmpLBnd,tmpUBnd
                 pntList(pos)=REAL(keyDataI4(j),ESMF_KIND_R8)
                 pos=pos+pntDim
              enddo
	   enddo

	else 
	  if (ESMF_LogMsgFoundError(ESMF_RC_ARG_WRONG, &
           " - unsupported coordinate data type", &
           ESMF_CONTEXT, rc)) return
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

      pos = index( string, ':')
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


end module ESMF_LocStreamMod


  subroutine f_esmf_locstreamcollectgarbage(ls, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcollectgarbage()"
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_LocStreamMod

    type(ESMF_LocStream) :: ls
    integer, intent(out) :: rc     
  
    integer :: localrc              
  
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL
  
    !print *, "collecting LocStream garbage"
  
    ! destruct internal data allocations
    call ESMF_LocStreamDestruct(ls%lstypep, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! deallocate actual LocStreamType allocation      
    if (associated(ls%lstypep)) then
      deallocate(ls%lstypep, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Deallocating LocStream", &
        ESMF_CONTEXT, rc)) return
    endif
    nullify(ls%lstypep)

    ! return successfully  
    rc = ESMF_SUCCESS

  end subroutine f_esmf_locstreamcollectgarbage

