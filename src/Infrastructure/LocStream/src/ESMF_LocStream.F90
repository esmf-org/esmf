! $Id: ESMF_LocStream.F90,v 1.1 2008/05/06 21:42:14 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research, 
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
     type (ESMF_Base)              :: base             ! base class object

     logical                               :: destroyDistgrid 
     type (ESMF_DistGrid)         :: distgrid       ! description of index space of Arrays
     type(ESMF_IndexFlag)        :: indexflag

      integer :: keyCount                                               ! Number of keys
      character(len=ESMF_MAXSTR), pointer   :: keyNames(:)  ! Names
      character(len=ESMF_MAXSTR), pointer   :: keyUnits(:)     ! Units
      character(len=ESMF_MAXSTR), pointer   :: keyLongNames(:) ! Long names
      logical, pointer :: keyCreated(:)        ! if the key Array has been created
      logical, pointer :: destroyKeys(:)      ! if we're responsible for destroying key array
      type (ESMF_Array), pointer                      :: keys(:)      ! Contents

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
   public ESMF_LocStreamDestroy
   public ESMF_LocStreamPrint              ! Print contents of a LocStream
   public ESMF_LocStreamGetKey
   public ESMF_LocStreamAddKey

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
    '$Id: ESMF_LocStream.F90,v 1.1 2008/05/06 21:42:14 oehmke Exp $'

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
! !IROUTINE: ESMF_LocStreamAddKeyAlloc - Add a key Array and allocate the internal memory

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
    type(ESMF_TypeKind) :: localKeyTypeKind 
    integer :: i,keyIndex
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


   ! Set ArraySpec
   call ESMF_ArraySpecSet(arrayspec, rank=1, typekind=localKeyTypeKind, rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return

   ! Create Array
   lstypep%keys(keyIndex)=ESMF_ArrayCreate(arrayspec, distgrid=lstypep%distgrid, &
                            indexflag=lstypep%indexflag, name=lstypep%keyNames(keyIndex), &
                            rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return

   ! Set key as created and that we should destroy the array
   lstypep%keyCreated(keyIndex)=.true.  
   lstypep%destroyKeys(keyIndex)=.true. 

   ! set other info
   if (present(keyUnits)) then
      lstypep%keyUnits(keyIndex)=keyUnits
   endif

   if (present(keyLongName)) then
      lstypep%keyLongNames(keyIndex)=keyLongName
   endif

   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamAddKeyAlloc
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamAddKeyR8"
!BOP
! !IROUTINE: ESMF_LocStreamAddKeyR8 - Add a key Array created around user money

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamAddKey()
  subroutine ESMF_LocStreamAddKeyR8(locstream, keyName, farray, copyflag, &
               keyUnits, keyLongName, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in)                   :: locstream
    character (len=*),         intent(in)                    :: keyName
    real(ESMF_KIND_R8), dimension(:), intent(in)  :: farray
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
! Valid native Fortran90 array, i.e. memory must be associated with the 
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
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_TypeKind) :: localKeyTypeKind 
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

   ! Make sure the key hasn't already been created
   if (lstypep%keyCreated(keyIndex)) then
      if (ESMF_LogMsgFoundError(ESMF_RC_ARG_WRONG, &
            " - key has already been added to this LocStream", &
            ESMF_CONTEXT, rc)) return
   endif

   ! Create Array
   lstypep%keys(keyIndex)=ESMF_ArrayCreate(farray, distgrid=lstypep%distgrid, &
                            copyflag=copyflag, indexflag=lstypep%indexflag,  &
                            name=lstypep%keyNames(keyIndex), rc=localrc)
   if (ESMF_LogMsgFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rc)) return

   ! Set key as created and that we should destroy the array
   lstypep%keyCreated(keyIndex)=.true.  
   lstypep%destroyKeys(keyIndex)=.true. 

   ! set other info
   if (present(keyUnits)) then
      lstypep%keyUnits(keyIndex)=keyUnits
   endif

   if (present(keyLongName)) then
      lstypep%keyLongNames(keyIndex)=keyLongName
   endif

   ! return success
   if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocStreamAddKeyR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamCreate"
!BOP
! !IROUTINE: ESMF_LocStreamCreate - Create a new location stream from a distgrid

! !INTERFACE:
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateFromDG( name, keyNames, distgrid, &
                 destroyDistgrid, indexflag, rc )
!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateFromDG

!
! !ARGUMENTS:
      character (len=*),        intent(in), optional   :: name
      character (len=*),        intent(in)                  :: keyNames
      type(ESMF_DistGrid),   intent(in)                  :: distgrid
      logical,                        intent(in),   optional :: destroyDistgrid
      type(ESMF_IndexFlag), intent(in),   optional :: indexflag    
      integer,                       intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_LocStream} object, constructs its
!     internal derived types. The key tokens are specified here (and the
!     Arrays are allocated), but the information is attached
!     at a later time. 
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          Name of the location stream
!     \item[keyNames]
!          Names of keys (arbitrary type)
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
      integer :: localPet, petCount
      character(len=ESMF_MAXSTR)       :: keytemp   ! temporary key name
      character(len=ESMF_MAXSTR)       :: string    ! temporary string
      integer :: keyCount,i,dimCount 
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

      ! Count number of keys
      keyCount = 0
      string = trim( keyNames )
      do while ( string /= '' )
          keyCount = keyCount + 1
          call ESMF_StripKey( string, keytemp )
      enddo

      ! Allocate space for keys
       allocate (lstypep%keyNames(keyCount), stat=localrc )
      if (ESMF_LogMsgFoundAllocError(localrc, " Allocating KeyNames", &
                                     ESMF_CONTEXT, rc)) return
      allocate (lstypep%keyUnits(keyCount), stat=localrc )
      if (ESMF_LogMsgFoundAllocError(localrc, " Allocating units", &
                                     ESMF_CONTEXT, rc)) return
      allocate (lstypep%keyLongNames(keyCount), stat=localrc )
      if (ESMF_LogMsgFoundAllocError(localrc, " Allocating longNames", &
                                     ESMF_CONTEXT, rc)) return
      allocate( lstypep%keys(keyCount ), stat=localrc )  ! Array of keys
      if (ESMF_LogMsgFoundAllocError(localrc, " Allocating keys", &
                                     ESMF_CONTEXT, rc)) return
      allocate( lstypep%keyCreated(keyCount ), stat=localrc )  ! Array of keys
      if (ESMF_LogMsgFoundAllocError(localrc, " Allocating keys", &
                                     ESMF_CONTEXT, rc)) return
      allocate( lstypep%destroyKeys(keyCount ), stat=localrc )  ! Array of keys
      if (ESMF_LogMsgFoundAllocError(localrc, " Allocating keys", &
                                     ESMF_CONTEXT, rc)) return

      ! Fill in key info 
      i = 0
      string = trim( keyNames )
      do while ( string /= '' )
        i = i + 1
        call ESMF_StripKey( string, lstypep%keyNames(i) )
        lstypep%keyUnits(i) = ""
        lstypep%keyLongNames(i) = ""
        lstypep%keyCreated(i) = .false.
        lstypep%destroyKeys(i) = .false.
      enddo

      ! Set some remaining info into the struct      
      lstypep%indexflag=indexflagLocal
      lstypep%destroyDistgrid=destroyDistgridLocal
      lstypep%distgrid=distgrid
      lstypep%keyCount=keyCount

      ! set Name
      call ESMF_BaseCreate(lstypep%base,"LocStream",name,0,rc=localrc)       
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! Set pointer to internal locstream type
      locstream%lstypep=>lstypep

      ! Set return value.
      ESMF_LocStreamCreateFromDG=locstream

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
! !IROUTINE: ESMF_LocStreamCreate - Create a new location stream from a local count

! !INTERFACE:
      ! Private name: call using ESMF_LocStreamCreate()
      function ESMF_LocStreamCreateFromLocal(name, keyNames, &
                                                                          localCount, indexflag, rc )
!
! !RETURN VALUE:
      type(ESMF_LocStream) :: ESMF_LocStreamCreateFromLocal

!
! !ARGUMENTS:
      character (len=*), intent(in), optional         :: name
      character (len=*), intent(in)                        :: keyNames
      integer, intent(in)                                       :: localCount
      type(ESMF_IndexFlag), intent(in), optional :: indexflag
      integer, intent(out), optional                     :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_LocStream} object, constructs its
!     internal derived types.  The {\tt ESMF\_DistGrid} is set up, indicating
!     how the LocStream is distributed. The key tokens are specified here (and the
!     Arrays are allocated), but the information is attached
!     at a later time. 
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          Name of the location stream
!     \item[keyNames]
!          Names of keys (arbitrary type)
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
    type (ESMF_LocStreamType), pointer :: lstypep
    type(ESMF_LocStream)                       :: locstream 
    type(ESMF_VM)                                   :: vm       ! Virtual machine used

      integer, allocatable  :: countsPerPet(:)
      logical                             :: dummy
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

      ! Set pointer to internal locstream type
      locstream%lstypep=>lstypep

      ! Create LocStream using CreateFromDistGrid version
      ESMF_LocStreamCreateFromLocal=ESMF_LocStreamCreateFromDG(name=name, &
                                                               keyNames=keynames, &
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

      ! Get internal pointer
      lstypep=>locstream%lstypep

     ! Destroy  key Arrays
     do i=1,lstypep%keyCount
          if (lstypep%keyCreated(i) .and. lstypep%destroyKeys(i)) then
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

      ! destroy base
      call ESMF_BaseDestroy(lstypep%base, rc=localrc)       
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Deallocate space for key data
       deallocate (lstypep%keyNames)
       deallocate (lstypep%keyUnits)
       deallocate (lstypep%keyLongNames)
       deallocate( lstypep%keys)
       deallocate( lstypep%keyCreated)
       deallocate( lstypep%destroyKeys)

      ! Deallocate type memory
      deallocate(lstypep)

      ! Nullify pointer in structure
      nullify(locstream%lstypep)

      ! Set init status to indicate structure has been destroyed
      ESMF_INIT_SET_DELETED(locstream)

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LocStreamDestroy
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetDefault"
!BOP
! !IROUTINE: ESMF_LocStreamGet - Return info associated with a LocStream

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGet()
  subroutine ESMF_LocStreamGetDefault(locstream, distgrid, keyCount, localDECount, name, rc)
!
! !ARGUMENTS:
    type(ESMF_Locstream), intent(in) :: locstream
    type(ESMF_DistGrid), intent(out), optional :: distgrid
    integer, intent(out),optional              :: keyCount
    integer, intent(out),optional              :: localDECount
    character(len=*), intent(out), optional :: name
    integer, intent(out), optional :: rc
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
! \item [{[localDECount]}]
! Number of DEs on this PET in the {\tt locstream}.
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

   ! Get localDECount
   if (present(localDECount)) then
      call ESMF_DistGridGet(lstypep%distgrid, delayout=delayout, rc=localrc) 
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
          ESMF_CONTEXT, rcToReturn=rc)) return
 
      call ESMF_DELayoutGet(delayout, localDeCount=localDECount, rc=localrc) 
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
       ESMF_CONTEXT, rcToReturn=rc)) return
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
! !IROUTINE: ESMF_LocStreamGetKeyArray - Get Array associated with key

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

   ! Make sure the key has been created
   if (.not. lstypep%keyCreated(keyIndex)) then
      if (ESMF_LogMsgFoundError(ESMF_RC_ARG_WRONG, &
            " - key hasn't yet been added in this LocStream", &
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
#define ESMF_METHOD "ESMF_LocStreamGetKeyI4"
!BOPI
! !IROUTINE: ESMF_LocStreamGetKey - Get pointer to I4 key values

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
      subroutine ESMF_LocStreamGetKeyI4(locstream, localDE, keyName, & 
          computationalLBound, computationalUBound, computationalCount,     &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in) :: locstream
      integer, intent(in) :: localDE
      character (len=*),    intent(in)              :: keyName
      integer,                intent(out), optional :: computationalLBound(:)
      integer,                intent(out), optional :: computationalUBound(:)
      integer,                intent(out), optional :: computationalCount(:)
      integer(ESMF_KIND_I4), pointer :: fptr(:)
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
!          The local DE to get the information for  (localDE starts at 0).
!     \item[{keyName}]
!          The key to get the information from.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region.
!          {\tt computationalLBound} must be allocated to be of size 1.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size 1.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size 1.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 type(ESMF_DELayout) :: delayout
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

 
 ! Obtain the native F90 array pointer via the LocalArray interface 
 allocate(larrayList(localDeCount))
 
 call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 

 !TODO: Add computational Bounds Calc


 ! cleanup
 deallocate(larrayList) 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetKeyI4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyR4"
!BOPI
! !IROUTINE: ESMF_LocStreamGetKey - Get pointer to R4 key values

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
      subroutine ESMF_LocStreamGetKeyR4(locstream, localDE, keyName, & 
          computationalLBound, computationalUBound, computationalCount,     &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in) :: locstream
      integer, intent(in) :: localDE
      character (len=*),    intent(in)              :: keyName
      integer,                intent(out), optional :: computationalLBound(:)
      integer,                intent(out), optional :: computationalUBound(:)
      integer,                intent(out), optional :: computationalCount(:)
      real(ESMF_KIND_R4), pointer :: fptr(:)
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
!          The local DE to get the information for  (localDE starts at 0).
!     \item[{keyName}]
!          The key to get the information from.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region.
!          {\tt computationalLBound} must be allocated to be of size 1.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size 1.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size 1.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 type(ESMF_DELayout) :: delayout
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

 
 ! Obtain the native F90 array pointer via the LocalArray interface 
 allocate(larrayList(localDeCount))
 
 call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 

 !TODO: Add computational Bounds Calc


 ! cleanup
 deallocate(larrayList) 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetKeyR4


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocStreamGetKeyR8"
!BOPI
! !IROUTINE: ESMF_LocStreamGetKey - Get pointer to R8 key values

! !INTERFACE:
  ! Private name; call using ESMF_LocStreamGetKey()
      subroutine ESMF_LocStreamGetKeyR8(locstream, localDE, keyName, & 
          computationalLBound, computationalUBound, computationalCount,     &
          fptr, doCopy, rc)
!
! !ARGUMENTS:
      type(ESMF_LocStream), intent(in) :: locstream
      integer, intent(in) :: localDE
      character (len=*),    intent(in)              :: keyName
      integer,                intent(out), optional :: computationalLBound(:)
      integer,                intent(out), optional :: computationalUBound(:)
      integer,                intent(out), optional :: computationalCount(:)
      real(ESMF_KIND_R8), pointer :: fptr(:)
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
!          The local DE to get the information for  (localDE starts at 0).
!     \item[{keyName}]
!          The key to get the information from.
!     \item[{[computationalLBound]}]
!          Upon return this holds the lower bounds of the computational region.
!          {\tt computationalLBound} must be allocated to be of size 1.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalUBound]}]
!          Upon return this holds the upper bounds of the stagger region.
!          {\tt exclusiveUBound} must be allocated to be of size 1.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{[computationalCount]}]
!          Upon return this holds the number of items in the computational region per dimension
!          (i.e. {\tt computationalUBound-computationalLBound+1}). {\tt computationalCount}
!          must be allocated to be of size 1.
!          Please see Section~\ref{sec:grid:usage:bounds} for a description
!          of the regions and their associated bounds and counts. 
!     \item[{fptr}]
!          The pointer to the coordinate data.
!     \item[{[doCopy]}]
!          If not specified, default to {\tt ESMF\_DATA\_REF}, in this case
!          fptr is a reference to the data in the Grid coordinate arrays. 
!          Please see Section~\ref{opt:copyflag} for further description and a
!          list of valid values. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI


 ! Local variables 
 type(ESMF_Array) :: array 
 integer :: localrc ! local error status 
 type(ESMF_TypeKind) :: typekind 
 type(ESMF_LocalArray), allocatable :: larrayList(:) 
 type(ESMF_CopyFlag) :: docopyInt
 type(ESMF_DELayout) :: delayout
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

 
 ! Obtain the native F90 array pointer via the LocalArray interface 
 allocate(larrayList(localDeCount))
 
 call ESMF_ArrayGet(array, larrayList=larrayList, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return
 
 call ESMF_LocalArrayGet(larrayList(localDE+1), fptr, doCopy, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, & 
                              ESMF_CONTEXT, rcToReturn=rc)) return 

 !TODO: Add computational Bounds Calc


 ! cleanup
 deallocate(larrayList) 

 ! Return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 

end subroutine ESMF_LocStreamGetKeyR8


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

        character(len=ESMF_MAXSTR)      :: name, str
        type(ESMF_LocStreamType), pointer   :: lstypep 
        integer                         :: i, localrc
        integer                         :: gridrank, arrayrank
        !character(len=ESMF_MAXSTR) :: msgbuf
        character(len=6)                :: defaultopts


!	Initialize
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

        write(*,*) "LocStream Print Starts ====>"

        ! Get internal pointer to locstream
        lstypep => locstream%lstypep

        ! print option is not implemented, but it has to pass to c_ESMC_BasePrint()
        defaultopts = "brief"

        call c_ESMC_GetName(lstypep%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        write(*, *)  "  Name = '",  trim(name), "'"

        call ESMF_BasePrint(lstypep%base, defaultopts, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! TODO: Fill the rest of this in once the struct has settled a bit

        write(*,*) "LocStream Print Ends   ====>"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_LocStreamPrint

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

      type(ESMF_LocStreamType), pointer :: lstypep

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_LocStreamGetInit,locstream,rc)

      ! TODO: more here once we get everything else settled

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
