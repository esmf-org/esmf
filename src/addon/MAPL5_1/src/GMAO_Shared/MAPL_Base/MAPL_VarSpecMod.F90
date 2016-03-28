!  $Id$

#include "MAPL_ErrLog.h"

!=============================================================================
!BOP

! !MODULE: MAPL_VarSpecMod -- A class for manipulation variable specifications.

! !INTERFACE:

module MAPL_VarSpecMod

! !USES:

use ESMF
use MAPL_BaseMod
use MAPL_IOMod
use MAPL_CommsMod, only: MAPL_AM_I_ROOT

! !PUBLIC MEMBER FUNCTIONS:

implicit none
private

public MAPL_VarSpecCreateInList
public MAPL_VarSpecGetIndex
public MAPL_VarSpecAddRefToList
public MAPL_VarSpecAddToList
public MAPL_VarSpecSet
public MAPL_VarSpecGet
public MAPL_VarSpecPrint
public MAPL_VarSpecPrintCSV
public MAPL_VarSpecDestroy
public MAPL_VarSpecAddChildName
public MAPL_VarSpecReconnect
public MAPL_VarConnCreate
public MAPL_VarConnGet
public MAPL_VarIsConnected
public MAPL_VarIsListed
public MAPL_ConnCheckUnused
public MAPL_ConnCheckReq
public MAPL_VarSpecSamePrec
! !OVERLOADED INTERFACES:

public operator(.eq.)
interface operator (.eq.)
   module procedure MAPL_VarSpecEQ
end interface

interface MAPL_VarSpecAddRefToList
   module procedure MAPL_VarSpecAddRefFromItem
   module procedure MAPL_VarSpecAddRefFromList
end interface

interface MAPL_VarSpecAddToList
   module procedure MAPL_VarSpecAddFromItem
   module procedure MAPL_VarSpecAddFromList
end interface

interface MAPL_VarSpecGetIndex
   module procedure MAPL_VarSpecGetIndexByName
   module procedure MAPL_VarSpecGetIndexOfItem
end interface

interface MAPL_VarSpecGet
   module procedure MAPL_VarSpecGetRegular
   module procedure MAPL_VarSpecGetFieldPtr
   module procedure MAPL_VarSpecGetBundlePtr
   module procedure MAPL_VarSpecGetStatePtr
end interface

interface MAPL_VarSpecSet
   module procedure MAPL_VarSpecSetRegular
   module procedure MAPL_VarSpecSetFieldPtr
   module procedure MAPL_VarSpecSetBundlePtr
   module procedure MAPL_VarSpecSetStatePtr
end interface

interface MAPL_VarSpecDestroy
   module procedure MAPL_VarSpecDestroy0
   module procedure MAPL_VarSpecDestroy1
end interface

interface MAPL_VarIsConnected
   module procedure MAPL_VarIsConnectedEE
   module procedure MAPL_VarIsConnectedIE
end interface

interface MAPL_VarSpecPrint
   module procedure MAPL_VarSpecPrintOne
   module procedure MAPL_VarSpecPrintMany
end interface

! !PUBLIC TYPES:

type, public :: MAPL_VarSpec
  private
  type(MAPL_VarSpecType), pointer :: SpecPtr => null()
end type MAPL_VarSpec

type, public :: MAPL_VarSpecPtr
  type(MAPL_VarSpec), pointer :: Spec(:) => null()
end type MAPL_VarSpecPtr

!EOP

type MAPL_VarSpecType
  private
  character(len=ESMF_MAXSTR)               :: SHORT_NAME
  character(len=ESMF_MAXSTR)               :: LONG_NAME
  character(len=ESMF_MAXSTR)               :: UNITS
  character(len=ESMF_MAXSTR)               :: FRIENDLYTO
  character(len=ESMF_MAXSTR)               :: VECTOR_PAIR
  character(len=ESMF_MAXSTR), pointer      :: ATTR_INAMES(:) => null()
  character(len=ESMF_MAXSTR), pointer      :: ATTR_RNAMES(:) => null()
  integer,                    pointer      :: ATTR_IVALUES(:) => null()
  real,                       pointer      :: ATTR_RVALUES(:) => null()
  integer,                    pointer      :: UNGRIDDED_DIMS(:) => null()
  character(len=ESMF_MAXSTR)               :: UNGRIDDED_UNIT
  character(len=ESMF_MAXSTR)               :: UNGRIDDED_NAME
  real,                       pointer      :: UNGRIDDED_COORDS(:)
  integer                                  :: DIMS
  integer                                  :: LOCATION
  integer                                  :: NUM_SUBTILES
  integer                                  :: STAT
  integer                                  :: ACCMLT_INTERVAL
  integer                                  :: COUPLE_INTERVAL
  integer                                  :: OFFSET
  integer                                  :: LABEL
  integer                                  :: HALOWIDTH
  integer                                  :: PRECISION
  integer                                  :: FIELD_TYPE
  integer                                  :: VECTOR_ORDER
  integer                                  :: STAGGERING
  integer                                  :: ROTATION
  integer                                  :: RESTART
  logical                                  :: defaultProvided
  logical                                  :: doNotAllocate
  logical                                  :: alwaysAllocate ! meant for export specs
  real                                     :: DEFAULT
  type(ESMF_Field), pointer                :: FIELD => null()
  type(ESMF_FieldBundle), pointer          :: BUNDLE => null()
  type(ESMF_State), pointer                :: STATE => null()
  type(ESMF_Grid)                          :: GRID
end type MAPL_VarSpecType

type MAPL_VarConnPoint
  private
  character(len=ESMF_MAXSTR)               :: SHORT_NAME
  integer                                  :: IMPORT
  integer                                  :: EXPORT
end type MAPL_VarConnPoint

type MAPL_VarConnType
  private
  type (MAPL_VarConnPoint)                 :: FROM
  type (MAPL_VarConnPoint)                 :: TO
  logical                                  :: used = .false.
  logical                                  :: notRequired = .false.
end type MAPL_VarConnType

type, public :: MAPL_VarConn
  private
  type(MAPL_VarConnType), pointer :: ConnPtr => null()
end type MAPL_VarConn

contains

  subroutine MAPL_VarSpecCreateInList(SPEC, SHORT_NAME, LONG_NAME,     &
                             UNITS,  Dims, VLocation, FIELD, BUNDLE, STATE, &
                             NUM_SUBTILES, &
                             STAT, ACCMLT_INTERVAL, COUPLE_INTERVAL, OFFSET, &
                             DEFAULT, FRIENDLYTO, &
                             HALOWIDTH, PRECISION, &
                             RESTART, &
                             ATTR_RNAMES, ATTR_INAMES, &
                             ATTR_RVALUES, ATTR_IVALUES, &
                             UNGRIDDED_DIMS, &
                             UNGRIDDED_UNIT, &
                             UNGRIDDED_NAME, &
                             UNGRIDDED_COORDS, &
                             FIELD_TYPE, &
                             STAGGERING, &
                             ROTATION,   & 
                             GRID, &
                                                                   RC  )

    type (MAPL_VarSpec ),             pointer         :: SPEC(:)
    character (len=*)               , intent(IN)      :: SHORT_NAME
    character (len=*)  , optional   , intent(IN)      :: LONG_NAME
    character (len=*)  , optional   , intent(IN)      :: UNITS
    character (len=*)  , optional   , intent(IN)      :: FRIENDLYTO
    integer            , optional   , intent(IN)      :: DIMS
    integer            , optional   , intent(IN)      :: VLOCATION
    integer            , optional   , intent(IN)      :: NUM_SUBTILES
    integer            , optional   , intent(IN)      :: ACCMLT_INTERVAL
    integer            , optional   , intent(IN)      :: COUPLE_INTERVAL
    integer            , optional   , intent(IN)      :: OFFSET
    integer            , optional   , intent(IN)      :: STAT
    real               , optional   , intent(IN)      :: DEFAULT
    type(ESMF_Field)   , optional   , intent(IN), target :: FIELD
    type(ESMF_FieldBundle)  , optional   , intent(IN), target :: BUNDLE
    type(ESMF_State)   , optional   , intent(IN), target :: STATE
    integer            , optional   , intent(IN)      :: HALOWIDTH
    integer            , optional   , intent(IN)      :: PRECISION
    integer            , optional   , intent(IN)      :: RESTART
    character (len=*)  , optional   , intent(IN)      :: ATTR_INAMES(:)
    character (len=*)  , optional   , intent(IN)      :: ATTR_RNAMES(:)
    integer            , optional   , intent(IN)      :: ATTR_IVALUES(:)
    real               , optional   , intent(IN)      :: ATTR_RVALUES(:)
    integer            , optional   , intent(IN)      :: UNGRIDDED_DIMS(:)
    character (len=*)  , optional   , intent(IN)      :: UNGRIDDED_UNIT
    character (len=*)  , optional   , intent(IN)      :: UNGRIDDED_NAME
    real               , optional   , intent(IN)      :: UNGRIDDED_COORDS(:)
    integer            , optional   , intent(IN)      :: FIELD_TYPE
    integer            , optional   , intent(IN)      :: STAGGERING
    integer            , optional   , intent(IN)      :: ROTATION
    type(ESMF_Grid)    , optional   , intent(IN)      :: GRID
    integer            , optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecCreateInList"
    integer                               :: STATUS

    type (MAPL_VarSpec ), pointer         :: TMP(:) => null()

    integer                    :: usableDIMS
    integer                    :: usableVLOC
    integer                    :: usableACCMLT
    integer                    :: usableCOUPLE
    integer                    :: usableOFFSET
    integer                    :: usableSTAT
    integer                    :: usableNUM_SUBTILES
    integer                    :: usableHALOWIDTH
    integer                    :: usablePRECISION
    integer                    :: usableFIELD_TYPE
    integer                    :: usableSTAGGERING
    integer                    :: usableROTATION
    integer                    :: usableRESTART
    character(len=ESMF_MAXSTR) :: usableLONG
    character(len=ESMF_MAXSTR) :: usableUNIT
    character(len=ESMF_MAXSTR) :: usableFRIENDLYTO
    character(len=ESMF_MAXSTR), pointer :: usableATTR_INAMES(:) => NULL()
    character(len=ESMF_MAXSTR), pointer :: usableATTR_RNAMES(:) => NULL()
    integer                   , pointer :: usableATTR_IVALUES(:) => NULL()
    real                      , pointer :: usableATTR_RVALUES(:) => NULL()
    integer                   , pointer :: usableUNGRIDDED_DIMS(:) => null()
    real                       :: usableDEFAULT
    type(ESMF_Grid)            :: usableGRID
    type(ESMF_Field), pointer  :: usableFIELD => null()
    type(ESMF_FieldBundle), pointer :: usableBUNDLE => null()
    type(ESMF_State), pointer  :: usableSTATE => null()
    character(len=ESMF_MAXSTR) :: useableUngrd_Unit
    character(len=ESMF_MAXSTR) :: useableUngrd_Name
    real                      , pointer :: usableUNGRIDDED_COORDS(:) => NULL()

    INTEGER :: I
    integer :: szINAMES, szRNAMES, szIVALUES, szRVALUES
    integer :: szUNGRD
    logical :: defaultProvided

      if(associated(SPEC)) then
       if(MAPL_VarSpecGetIndex(SPEC, SHORT_NAME)/=-1) then
         RETURN_(ESMF_FAILURE)
       endif
      else
       allocate(SPEC(0),stat=STATUS)
       VERIFY_(STATUS)
      endif

      if(present(STAT)) then
       usableSTAT=STAT
      else
       usableSTAT=MAPL_FieldItem !ALT: not sure if needs special attn for bundles
      endif
    
      if(present(ACCMLT_INTERVAL)) then
       usableACCMLT=ACCMLT_INTERVAL
      else
       usableACCMLT=0
      endif
    
      if(present(COUPLE_INTERVAL)) then
       usableCOUPLE=COUPLE_INTERVAL
      else
       usableCOUPLE=0
      endif
    
      if(present(OFFSET)) then
       usableOFFSET=OFFSET
      else
       usableOFFSET=0
      endif
    
      if(present(LONG_NAME)) then
       usableLONG=LONG_NAME
      else
       usableLONG=SHORT_NAME
      endif

      if(present(UNITS)) then
       usableUNIT=UNITS
      else
       usableUNIT=""
      endif

      if(present(FRIENDLYTO)) then
       usableFRIENDLYTO=FRIENDLYTO
       if (LEN(TRIM(FRIENDLYTO)) /= 0) then 
          usableSTAT = ior(usableSTAT,MAPL_FriendlyVariable)
       end if
      else
       usableFRIENDLYTO=""
      endif

      if(present(DIMS)) then
       usableDIMS=DIMS
      else
       usableDIMS=MAPL_DimsUnknown
      endif

      if(present(VLOCATION)) then
       usableVLOC=VLOCATION
      else
       usableVLOC=MAPL_VLocationNone
      endif

      if(present(NUM_SUBTILES)) then
       usableNUM_SUBTILES=NUM_SUBTILES
      else
       usableNUM_SUBTILES=0
      endif

      if(present(DEFAULT)) then
         defaultProvided=.true.
         usableDEFAULT=DEFAULT
      else
         defaultProvided=.false.
         usableDEFAULT=0.0 ! ALT: this could be NaN
!         usableDEFAULT=Z'7F800001' ! DSK: set to NaN, dies in FV Init
!         usableDEFAULT=-999. ! DSK
      endif

      if (present(FIELD_TYPE)) then
         usableFIELD_TYPE=FIELD_TYPE
      else
         usableFIELD_TYPE=MAPL_ScalarField 
      endif

      if (present(STAGGERING)) then
         usableSTAGGERING=STAGGERING
      else
         usableSTAGGERING=MAPL_AGrid
      endif

      if (present(ROTATION)) then
         usableROTATION=ROTATION
      else
         usableROTATION=MAPL_RotateLL
      endif

      if(present(GRID)) then
         usableGRID=GRID
      else
         usableGRID = ESMF_GridEmptyCreate(RC=STATUS)
         VERIFY_(STATUS)
         call ESMF_GridDestroy(usableGRID) !ALT we do not need RC
      endif

      if(present(FIELD)) then
         usableFIELD=>FIELD
      else
         allocate(usableFIELD, STAT=STATUS)
         VERIFY_(STATUS)
         usableFIELD = ESMF_FieldEmptyCreate(NAME=SHORT_NAME,RC=STATUS)
         VERIFY_(STATUS)
         call ESMF_FieldDestroy(usableFIELD) !ALT we do not need RC
      endif

      if(present(BUNDLE)) then
         usableBUNDLE=>BUNDLE
      else
         allocate(usableBUNDLE, STAT=STATUS)
         VERIFY_(STATUS)
         usableBUNDLE = ESMF_FieldBundleCreate(NAME=SHORT_NAME,RC=STATUS)
         VERIFY_(STATUS)
         call ESMF_FieldBundleDestroy(usableBUNDLE) !ALT we do not need RC
      endif

      if(present(STATE)) then
         usableSTATE=>STATE
      else
         allocate(usableSTATE, STAT=STATUS)
         VERIFY_(STATUS)
         usableSTATE = ESMF_StateCreate(NAME=SHORT_NAME,RC=STATUS)
         VERIFY_(STATUS)
         call ESMF_StateDestroy(usableSTATE) !ALT we do not need RC
      endif

      if(present(HALOWIDTH)) then
       usableHALOWIDTH=HALOWIDTH
      else
       usableHALOWIDTH=0
      endif

      if(present(RESTART)) then
       usableRESTART=RESTART
      else
       usableRESTART=MAPL_RestartOptional ! default
      endif

      if(present(PRECISION)) then
       usablePRECISION=PRECISION
      else
       usablePRECISION=kind(0.0) ! default "real" kind
      endif

! Sanity checks
      if (usablePRECISION /= ESMF_KIND_R4 .AND. usablePRECISION /= ESMF_KIND_R8) then
         ! only those 2 values are allowed
         RETURN_(ESMF_FAILURE) 
      end if

      szRNAMES = 0
      if (present(ATTR_RNAMES)) then
         szRNAMES = size(ATTR_RNAMES)
         allocate(usableATTR_RNAMES(szRNAMES), stat=status)
         VERIFY_(STATUS)
         usableATTR_RNAMES = ATTR_RNAMES
      end if

      szINAMES = 0
      if (present(ATTR_INAMES)) then
         szINAMES = size(ATTR_INAMES)
         allocate(usableATTR_INAMES(szINAMES), stat=status)
         VERIFY_(STATUS)
         usableATTR_INAMES = ATTR_INAMES
      end if

      szRVALUES = 0
      if (present(ATTR_RVALUES)) then
         szRVALUES = size(ATTR_RVALUES)
         allocate(usableATTR_RVALUES(szRVALUES), stat=status)
         VERIFY_(STATUS)
         usableATTR_RVALUES = ATTR_RVALUES
      end if

      szIVALUES = 0
      if (present(ATTR_IVALUES)) then
         szIVALUES = size(ATTR_INAMES)
         allocate(usableATTR_IVALUES(szIVALUES), stat=status)
         VERIFY_(STATUS)
         usableATTR_IVALUES = ATTR_IVALUES
      end if
      ASSERT_(szIVALUES == szINAMES)
      ASSERT_(szRVALUES == szRNAMES)

      szUNGRD = 0
      if (present(UNGRIDDED_DIMS)) then
         szUNGRD = size(UNGRIDDED_DIMS)
         allocate(usableUNGRIDDED_DIMS(szUNGRD), stat=status)
         VERIFY_(STATUS)
         usableUNGRIDDED_DIMS = UNGRIDDED_DIMS
      else
         NULLIFY(usableUNGRIDDED_DIMS)
      end if

      if (present(UNGRIDDED_UNIT)) then
         useableUngrd_Unit = UNGRIDDED_UNIT
      else
         useableUngrd_Unit = "N/A"
      end if
      if (present(UNGRIDDED_NAME)) then
         useableUngrd_NAME = UNGRIDDED_NAME
      else
         useableUngrd_NAME = "N/A"
      end if

      szUNGRD = 0
      if (present(UNGRIDDED_COORDS)) then
         szUNGRD = size(UNGRIDDED_COORDS)
         allocate(usableUNGRIDDED_COORDS(szUNGRD), stat=status)
         VERIFY_(STATUS)
         usableUNGRIDDED_COORDS = UNGRIDDED_COORDS
      end if

      I = size(SPEC)

      allocate(TMP(I+1),stat=STATUS)
      VERIFY_(STATUS)
      
      TMP(1:I) = SPEC
      deallocate(SPEC)

      allocate(TMP(I+1)%SPECPtr,stat=STATUS)
      VERIFY_(STATUS)

      TMP(I+1)%SPECPtr%SHORT_NAME =  SHORT_NAME
      TMP(I+1)%SPECPtr%LONG_NAME  =  usableLONG
      TMP(I+1)%SPECPtr%UNITS      =  usableUNIT
      TMP(I+1)%SPECPtr%DIMS       =  usableDIMS
      TMP(I+1)%SPECPtr%LOCATION   =  usableVLOC
      TMP(I+1)%SPECPtr%NUM_SUBTILES = usableNUM_SUBTILES
      TMP(I+1)%SPECPtr%STAT       =  usableSTAT
      TMP(I+1)%SPECPtr%ACCMLT_INTERVAL  =  usableACCMLT
      TMP(I+1)%SPECPtr%COUPLE_INTERVAL  =  usableCOUPLE
      TMP(I+1)%SPECPtr%OFFSET     =  usableOFFSET
      TMP(I+1)%SPECPtr%LABEL      =  0
      TMP(I+1)%SPECPtr%DEFAULT    =  usableDEFAULT
      TMP(I+1)%SPECPtr%defaultProvided = defaultProvided
      TMP(I+1)%SPECPtr%FIELD      => usableFIELD
      TMP(I+1)%SPECPtr%BUNDLE     => usableBUNDLE
      TMP(I+1)%SPECPtr%STATE      => usableSTATE
      TMP(I+1)%SPECPtr%GRID       =  usableGRID
      TMP(I+1)%SPECPtr%FRIENDLYTO =  usableFRIENDLYTO
      TMP(I+1)%SPECPtr%HALOWIDTH  =  usableHALOWIDTH
      TMP(I+1)%SPECPtr%RESTART    =  usableRESTART
      TMP(I+1)%SPECPtr%PRECISION  =  usablePRECISION
      TMP(I+1)%SPECPtr%FIELD_TYPE =  usableFIELD_TYPE
      TMP(I+1)%SPECPtr%UNGRIDDED_UNIT =  useableUngrd_Unit
      TMP(I+1)%SPECPtr%UNGRIDDED_NAME =  useableUngrd_Name
      TMP(I+1)%SPECPtr%STAGGERING =  usableSTAGGERING
      TMP(I+1)%SPECPtr%ROTATION =  usableROTATION
      TMP(I+1)%SPECPtr%doNotAllocate    =  .false.
      TMP(I+1)%SPECPtr%alwaysAllocate   =  .false.
      if(associated(usableATTR_IVALUES)) then
         TMP(I+1)%SPECPtr%ATTR_IVALUES  =>  usableATTR_IVALUES
      else
         NULLIFY(TMP(I+1)%SPECPtr%ATTR_IVALUES)
      endif
      if(associated(usableATTR_RVALUES)) then
         TMP(I+1)%SPECPtr%ATTR_RVALUES  =>  usableATTR_RVALUES
      else
         NULLIFY(TMP(I+1)%SPECPtr%ATTR_RVALUES)
      endif
      if(associated(usableUNGRIDDED_DIMS)) then
         TMP(I+1)%SPECPtr%UNGRIDDED_DIMS  =>  usableUNGRIDDED_DIMS
      else
         NULLIFY(TMP(I+1)%SPECPtr%UNGRIDDED_DIMS)
      endif
      if(associated(usableUNGRIDDED_COORDS)) then
         TMP(I+1)%SPECPtr%UNGRIDDED_COORDS  =>  usableUNGRIDDED_COORDS
      else
         NULLIFY(TMP(I+1)%SPECPtr%UNGRIDDED_COORDS)
      endif
      if(associated(usableATTR_RNAMES)) then
         TMP(I+1)%SPECPtr%ATTR_RNAMES=>  usableATTR_RNAMES
      else
         NULLIFY(TMP(I+1)%SPECPtr%ATTR_RNAMES)
      endif
      if(associated(usableATTR_INAMES)) then
         TMP(I+1)%SPECPtr%ATTR_INAMES=>  usableATTR_INAMES
      else
         NULLIFY(TMP(I+1)%SPECPtr%ATTR_INAMES)
      endif

      SPEC => TMP

      RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_VarSpecCreateInList


  subroutine MAPL_VarSpecAddRefFromItem(SPEC, ITEM, ALLOW_DUPLICATES, RC)

    type (MAPL_VarSpec ), pointer         :: SPEC(:)
    type (MAPL_VarSpec ), intent(IN )     :: ITEM
    logical, optional   , intent(IN)      :: ALLOW_DUPLICATES
    integer, optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecAddRefFromItem"
    integer                               :: STATUS

    type (MAPL_VarSpec ), pointer         :: TMP(:) => null()
    integer                               :: I
    logical                               :: usableALLOW_DUPLICATES
     

    if(present(ALLOW_DUPLICATES)) then
       usableALLOW_DUPLICATES=ALLOW_DUPLICATES
    else
       usableALLOW_DUPLICATES=.FALSE.
    endif


    if(.not.associated(ITEM%SPECPtr)) then
       RETURN_(ESMF_FAILURE)
    endif

    if(associated(SPEC)) then
       if (.not. usableALLOW_DUPLICATES) then
          I = MAPL_VarSpecGetIndex(SPEC, ITEM, RC=STATUS)
          VERIFY_(STATUS)
          if(I /= -1) then
             if (SPEC(I) == ITEM) THEN
                if(present(RC)) then
                   RC=MAPL_DuplicateEntry
                end if
                return
             else
                call write_parallel("ERROR: same SHORT_NAME, ("   // &
                                    trim(ITEM%SPECPtr%SHORT_NAME) // &
                                    "), different attributes")
                call MAPL_VarSpecPrint(ITEM)
                call MAPL_VarSpecPrint(SPEC(I))
                RETURN_(ESMF_FAILURE)
             end if
          endif
       end if
      else
         allocate(SPEC(0),stat=STATUS)
         VERIFY_(STATUS)
      endif

      I = size(SPEC)

      allocate(TMP(I+1),stat=STATUS)
      VERIFY_(STATUS)
      
      TMP(1:I) = SPEC
      deallocate(SPEC)

      TMP(I+1)%SPECPtr => ITEM%SPECPtr
      SPEC => TMP

      RETURN_(ESMF_SUCCESS)


  end subroutine MAPL_VarSpecAddRefFromItem

  subroutine MAPL_VarSpecAddRefFromList(SPEC,ITEM,RC)

    type (MAPL_VarSpec ), pointer         :: SPEC(:)
    type (MAPL_VarSpec ), intent(IN )     :: ITEM(:)
    integer, optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecAddRefFromList"
    integer                               :: STATUS

    integer I

    do I=1,size(ITEM)
     call MAPL_VarSpecAddRefFromItem(SPEC,ITEM(I),RC=STATUS)
     IF (STATUS /= MAPL_DuplicateEntry) then
        VERIFY_(STATUS)
     END IF
    enddo

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_VarSpecAddRefFromList


  function MAPL_VarSpecGetIndexByName(SPEC, NAME, RC) result (INDEX)
    type (MAPL_VarSpec )            , intent(in)   :: SPEC(:)
    character (len=*)               , intent(IN)   :: NAME
    integer, optional               , intent(OUT)  :: RC
    integer                                        :: INDEX

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecGetIndexByName"
    integer :: I


    do I = 1, size(SPEC)
     if(.not.associated(SPEC(I)%SPECPtr)) then
      RETURN_(ESMF_FAILURE)
     endif
     if (trim(SPEC(I)%SPECPtr%SHORT_NAME) == trim(NAME)) then
       INDEX = I
       RETURN_(ESMF_SUCCESS)
     endif
    enddo

    INDEX = -1 ! not found
    RETURN_(ESMF_SUCCESS)

  end function MAPL_VarSpecGetIndexByName



  subroutine MAPL_VarSpecGetDataByName(SPEC, NAME, PTR1, PTR2, PTR3, RC)
    type (MAPL_VarSpec )            , intent(INout):: SPEC(:)
    character (len=*)               , intent(IN)   :: NAME
    real,    optional, pointer                     :: PTR1(:)
    real,    optional, pointer                     :: PTR2(:,:)
    real,    optional, pointer                     :: PTR3(:,:,:)
    integer, optional               , intent(OUT)  :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecGetDataByName"
    integer :: STATUS

    integer :: I

    do I = 1, size(SPEC)
     if(.not.associated(SPEC(I)%SPECPtr)) then
      RETURN_(ESMF_FAILURE)
     endif

     if (trim(SPEC(I)%SPECPtr%SHORT_NAME) == trim(NAME)) then
        call MAPL_VarSpecGetData(SPEC(I),PTR1,PTR2,PTR3,RC=STATUS)
        VERIFY_(STATUS)
        RETURN_(ESMF_SUCCESS)
     endif
    enddo

    RETURN_(ESMF_FAILURE)

  end subroutine MAPL_VarSpecGetDataByName


  subroutine MAPL_VarSpecGetData(SPEC, PTR1, PTR2, PTR3, RC)
    type (MAPL_VarSpec )            , intent(INout):: SPEC
    real,    optional, pointer                     :: PTR1(:)
    real,    optional, pointer                     :: PTR2(:,:)
    real,    optional, pointer                     :: PTR3(:,:,:)
    integer, optional               , intent(OUT)  :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecGetData"
    integer :: STATUS

    type(ESMF_Array) :: ARRAY

     if(.not.associated(SPEC%SPECPtr)) then
      RETURN_(ESMF_FAILURE)
     endif

     call ESMF_FieldGet(SPEC%SPECPtr%FIELD,Array=ARRAY,rc=STATUS)
     VERIFY_(STATUS)

     if (present(PTR1)) then
        call ESMF_ArrayGet(ARRAY, localDE=0, farrayptr=PTR1, RC=STATUS)
        VERIFY_(STATUS)
        ASSERT_(.not.present(PTR2))
        ASSERT_(.not.present(PTR3))
        RETURN_(ESMF_SUCCESS)
     endif

     if (present(PTR2)) then
        call ESMF_ArrayGet(ARRAY, localDE=0, farrayptr=PTR2, RC=STATUS)
        VERIFY_(STATUS)
        ASSERT_(.not.present(PTR3))
        RETURN_(ESMF_SUCCESS)
     endif

     if (present(PTR3)) then
        call ESMF_ArrayGet(ARRAY, localDE=0, farrayptr=PTR3, RC=STATUS)
        VERIFY_(STATUS)
        RETURN_(ESMF_SUCCESS)
     endif

     RETURN_(ESMF_FAILURE)

  end subroutine MAPL_VarSpecGetData

  function MAPL_VarSpecGetIndexOfItem(SPEC, ITEM, RC) result (INDEX)
    type (MAPL_VarSpec )            , intent(in)   :: SPEC(:)
    type (MAPL_VarSpec )            , intent(in)   :: ITEM
    integer, optional               , intent(OUT)  :: RC
    integer                                        :: INDEX

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecGetIndexOfItem"

    integer :: I

    do I = 1, size(SPEC)
     if(.not.associated(SPEC(I)%SPECPtr)) then
      RETURN_(ESMF_FAILURE)
     endif

     if (trim(SPEC(I)%SPECPtr%SHORT_NAME) == trim(ITEM%SPECPtr%SHORT_NAME)) then
        if (SPEC(I) == ITEM) then
           INDEX = I
           RETURN_(ESMF_SUCCESS)
        end if
     endif
    enddo

    INDEX = -1 ! not found
    RETURN_(ESMF_SUCCESS)

  end function MAPL_VarSpecGetIndexOfItem


  subroutine MAPL_VarSpecAddFromItem(SPEC,ITEM,RC)

    type (MAPL_VarSpec ), pointer         :: SPEC(:)
    type (MAPL_VarSpec ), intent(IN )     :: ITEM
    integer, optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecAddFromItem"
    integer                               :: STATUS

     
      if(.not.associated(ITEM%SPECPtr)) then
       RETURN_(ESMF_FAILURE)
      endif

     call MAPL_VarSpecCreateInList(SPEC,                                             &
                                   SHORT_NAME      = ITEM%SPECPTR%SHORT_NAME,        &
                                   LONG_NAME       = ITEM%SPECPTR%LONG_NAME,         &
                                   UNITS           = ITEM%SPECPTR%UNITS,             &
                                   DIMS            = ITEM%SPECPTR%Dims,              &
                                   VLOCATION       = ITEM%SPECPTR%Location,          &
                                   STAT            = ITEM%SPECPTR%STAT,              &
                                   ACCMLT_INTERVAL = ITEM%SPECPTR%ACCMLT_INTERVAL,   &
                                   COUPLE_INTERVAL = ITEM%SPECPTR%COUPLE_INTERVAL,   &
                                   DEFAULT         = ITEM%SPECPTR%DEFAULT,           &
                                   FIELD           = ITEM%SPECPTR%FIELD,             &
                                   BUNDLE          = ITEM%SPECPTR%BUNDLE,            &
                                   STATE           = ITEM%SPECPTR%STATE,            &
                                   HALOWIDTH       = ITEM%SPECPTR%HALOWIDTH,         &
                                   RESTART         = ITEM%SPECPTR%RESTART,           &
                                   PRECISION       = ITEM%SPECPTR%PRECISION,         &
                                   ATTR_INAMES     = ITEM%SPECPTR%ATTR_INAMES,       &
                                   ATTR_RNAMES     = ITEM%SPECPTR%ATTR_RNAMES,       &
                                   ATTR_IVALUES    = ITEM%SPECPTR%ATTR_IVALUES,      &
                                   ATTR_RVALUES    = ITEM%SPECPTR%ATTR_RVALUES,      &
                                   UNGRIDDED_DIMS  = ITEM%SPECPTR%UNGRIDDED_DIMS,    &
                                   FIELD_TYPE      = ITEM%SPECPTR%FIELD_TYPE,        &
                                   STAGGERING      = ITEM%SPECPTR%STAGGERING,        &
                                   ROTATION        = ITEM%SPECPTR%ROTATION,          &
                                   GRID            = ITEM%SPECPTR%GRID,              &
                                                                          RC=STATUS  )     
     VERIFY_(STATUS)

     RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_VarSpecAddFromItem


  subroutine MAPL_VarSpecAddFromList(SPEC,ITEM,RC)

    type (MAPL_VarSpec ), pointer         :: SPEC(:)
    type (MAPL_VarSpec ), intent(IN )     :: ITEM(:)
    integer, optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecAddFromList"
    integer                               :: STATUS

    integer I

    do I=1,size(ITEM)
     call MAPL_VarSpecAddFromItem(SPEC,ITEM(I),RC=STATUS)
     VERIFY_(STATUS)
    enddo

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_VarSpecAddFromList


  subroutine MAPL_VarSpecDestroy0(SPEC, RC )

    type (MAPL_VarSpec ),             intent(INOUT)   :: SPEC
    integer            , optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecDestroy"
    integer                               :: STATUS

      if(associated(SPEC%SPECPtr)) then
       deallocate(SPEC%SPECPtr)
      endif

     RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_VarSpecDestroy0

  subroutine MAPL_VarSpecDestroy1(SPEC, RC )

    type (MAPL_VarSpec ),             pointer      :: SPEC(:)
    integer            , optional   , intent(OUT)  :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecDestroy"
    integer                               :: STATUS
    integer :: i

    if (associated(SPEC)) then
       do I=1,size(SPEC)
          call MAPL_VarSpecDestroy0(spec(i))
       end do
       deallocate(SPEC)
    end if

     RETURN_(ESMF_SUCCESS)

   end subroutine MAPL_VarSpecDestroy1


  subroutine MAPL_VarSpecSetRegular(SPEC, SHORT_NAME, LONG_NAME, UNITS,       &
                             Dims, VLocation, FIELD, BUNDLE, STATE,    &
                             STAT, ACCMLT_INTERVAL, COUPLE_INTERVAL,   &
                             OFFSET, LABEL,                            &
                             FRIENDLYTO,                               &
                             FIELD_TYPE,                               &
                             STAGGERING,                               &
                             ROTATION,                                 &
                             GRID,                                     &
                             doNotAllocate,                            &
                             alwaysAllocate,                            &
                                                                    RC )

    type (MAPL_VarSpec ),             intent(INOUT)   :: SPEC
    character(len=*)   , optional   , intent(IN)      :: SHORT_NAME
    character(len=*)   , optional   , intent(IN)      :: LONG_NAME
    character(len=*)   , optional   , intent(IN)      :: UNITS
    integer            , optional   , intent(IN)      :: DIMS
    integer            , optional   , intent(IN)      :: VLOCATION
    integer            , optional   , intent(IN)      :: ACCMLT_INTERVAL
    integer            , optional   , intent(IN)      :: COUPLE_INTERVAL
    integer            , optional   , intent(IN)      :: OFFSET
    integer            , optional   , intent(IN)      :: STAT
    integer            , optional   , intent(IN)      :: LABEL
    type(ESMF_Field)   , optional   , intent(IN)      :: FIELD
    type(ESMF_FieldBundle)  , optional   , intent(IN) :: BUNDLE
    type(ESMF_State)   , optional   , intent(IN)      :: STATE
    character(len=*)   , optional   , intent(IN)      :: FRIENDLYTO
    integer            , optional   , intent(in)      :: FIELD_TYPE
    integer            , optional   , intent(in)      :: STAGGERING
    integer            , optional   , intent(in)      :: ROTATION
    type(ESMF_Grid)    , optional   , intent(IN)      :: GRID
    logical            , optional   , intent(IN)      :: doNotAllocate
    logical            , optional   , intent(IN)      :: alwaysAllocate
    integer            , optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecSet"
    integer                               :: STATUS

      if(.not.associated(SPEC%SPECPtr)) then
       RETURN_(ESMF_FAILURE)
      endif

      if(present(SHORT_NAME)) then
        SPEC%SPECPtr%SHORT_NAME = SHORT_NAME
      endif

      if(present(LONG_NAME)) then
        SPEC%SPECPtr%LONG_NAME = LONG_NAME
      endif

      if(present(UNITS)) then
        SPEC%SPECPtr%UNITS = UNITS
      endif

      if(present(FRIENDLYTO)) then
        SPEC%SPECPtr%FRIENDLYTO = FRIENDLYTO
      endif

      if(present(STAT)) then
       SPEC%SPECPtr%STAT=STAT
      endif

      if(present(DIMS)) then
       SPEC%SPECPtr%DIMS=DIMS
      endif

      if(present(VLOCATION)) then
       SPEC%SPECPtr%LOCATION=VLOCATION
      endif

      if(present(ACCMLT_INTERVAL)) then
       SPEC%SPECPtr%ACCMLT_INTERVAL=ACCMLT_INTERVAL
      endif

      if(present(COUPLE_INTERVAL)) then
       SPEC%SPECPtr%COUPLE_INTERVAL=COUPLE_INTERVAL
      endif

      if(present(OFFSET)) then
       SPEC%SPECPtr%OFFSET=OFFSET
      endif

      if(present(LABEL)) then
       SPEC%SPECPtr%LABEL=LABEL
      endif

      if(present(FIELD)) then
       SPEC%SPECPtr%FIELD = FIELD
      endif

      if(present(BUNDLE)) then
       SPEC%SPECPtr%BUNDLE = BUNDLE
      endif

      if(present(STATE)) then
       SPEC%SPECPtr%STATE = STATE
      endif

      if(present(GRID)) then
         SPEC%SPECPtr%GRID = GRID
      endif

      if(present(FIELD_TYPE)) then
         SPEC%SPECPtr%FIELD_TYPE = FIELD_TYPE
      endif

      if(present(STAGGERING)) then
         SPEC%SPECPtr%STAGGERING = STAGGERING
      endif

      if(present(ROTATION)) then
         SPEC%SPECPtr%ROTATION = ROTATION
      endif

      if(present(doNotAllocate)) then
         SPEC%SPECPtr%doNotAllocate = doNotAllocate
      endif

      if(present(alwaysAllocate)) then
         SPEC%SPECPtr%alwaysAllocate = alwaysAllocate
      endif

      RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_VarSpecSetRegular


  subroutine MAPL_VarSpecSetFieldPtr(SPEC, FIELDPTR, RC )

    type (MAPL_VarSpec ),             intent(INOUT)   :: SPEC
    type(ESMF_Field)   ,              pointer         :: FIELDPTR
    integer            , optional   , intent(  OUT)   :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecSetFieldPtr"
    integer                               :: STATUS

      if(.not.associated(SPEC%SPECPtr)) then
       RETURN_(ESMF_FAILURE)
      endif

      SPEC%SPECPtr%FIELD => FIELDPTR

      RETURN_(ESMF_SUCCESS)

    end subroutine MAPL_VarSpecSetFieldPtr

    subroutine MAPL_VarSpecSetBundlePtr(SPEC, BUNDLEPTR, RC )

      type (MAPL_VarSpec ),             intent(INOUT)   :: SPEC
      type(ESMF_FieldBundle)  ,         pointer         :: BUNDLEPTR
      integer            , optional   , intent(  OUT)   :: RC


      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecSetBundlePtr"
      integer                               :: STATUS

      if(.not.associated(SPEC%SPECPtr)) then
         RETURN_(ESMF_FAILURE)
      endif

      SPEC%SPECPtr%BUNDLE => BUNDLEPTR

      RETURN_(ESMF_SUCCESS)

    end subroutine MAPL_VarSpecSetBundlePtr

    subroutine MAPL_VarSpecSetStatePtr(SPEC, STATEPTR, RC )

      type (MAPL_VarSpec ),             intent(INOUT)   :: SPEC
      type(ESMF_State)  ,               pointer         :: STATEPTR
      integer            , optional   , intent(  OUT)   :: RC


      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecSetStatePtr"
      integer                               :: STATUS

      if(.not.associated(SPEC%SPECPtr)) then
         RETURN_(ESMF_FAILURE)
      endif

      SPEC%SPECPtr%STATE => STATEPTR

      RETURN_(ESMF_SUCCESS)

    end subroutine MAPL_VarSpecSetStatePtr



  subroutine MAPL_VarSpecGetRegular(SPEC, SHORT_NAME, LONG_NAME, UNITS,       &
                             Dims, VLocation, FIELD, BUNDLE, STATE, &
                             NUM_SUBTILES,      &
                             STAT, ACCMLT_INTERVAL, COUPLE_INTERVAL,   &
                             OFFSET, LABEL, DEFAULT, defaultProvided,  &
                             FRIENDLYTO,                               &
                             RESTART,                                  &
                             HALOWIDTH,                                &
                             PRECISION,                                &
                             ATTR_RNAMES, ATTR_INAMES,                 &
                             ATTR_RVALUES, ATTR_IVALUES,               &
                             UNGRIDDED_DIMS,                           &
                             UNGRIDDED_UNIT,                          &
                             UNGRIDDED_NAME,                          &
                             UNGRIDDED_COORDS,                         &
                             FIELD_TYPE,                               &
                             STAGGERING,                               &
                             ROTATION,                                 &
                             GRID,                                     &
                             doNotAllocate,                            &
                             alwaysAllocate,                           &
                                                                    RC )

    type (MAPL_VarSpec ),             intent(IN )     :: SPEC
    character(len=*)   , optional   , intent(OUT)     :: SHORT_NAME
    character(len=*)   , optional   , intent(OUT)     :: LONG_NAME
    character(len=*)   , optional   , intent(OUT)     :: UNITS
    integer            , optional   , intent(OUT)     :: DIMS
    integer            , optional   , intent(OUT)     :: VLOCATION
    integer            , optional   , intent(OUT)     :: NUM_SUBTILES
    integer            , optional   , intent(OUT)     :: ACCMLT_INTERVAL
    integer            , optional   , intent(OUT)     :: COUPLE_INTERVAL
    integer            , optional   , intent(OUT)     :: OFFSET
    integer            , optional   , intent(OUT)     :: STAT
    integer            , optional   , intent(OUT)     :: LABEL
    real               , optional   , intent(OUT)     :: DEFAULT
    logical            , optional   , intent(OUT)     :: defaultProvided
    type(ESMF_Field)   , optional   , intent(OUT)     :: FIELD
    type(ESMF_FieldBundle)  , optional   , intent(OUT)     :: BUNDLE
    type(ESMF_State)   , optional   , intent(OUT)     :: STATE
    character(len=*)   , optional   , intent(OUT)     :: FRIENDLYTO
    integer            , optional   , intent(OUT)     :: HALOWIDTH
    integer            , optional   , intent(OUT)     :: PRECISION
    integer            , optional   , intent(OUT)     :: RESTART
    character(len=ESMF_MAXSTR), optional, pointer     :: ATTR_INAMES(:)
    character(len=ESMF_MAXSTR), optional, pointer     :: ATTR_RNAMES(:)
    integer,                    optional, pointer     :: ATTR_IVALUES(:)
    real,                       optional, pointer     :: ATTR_RVALUES(:)
    integer,                    optional, pointer     :: UNGRIDDED_DIMS(:)
    character(len=*)   , optional   , intent(OUT)     :: UNGRIDDED_UNIT
    character(len=*)   , optional   , intent(OUT)     :: UNGRIDDED_NAME
    real,                       optional, pointer     :: UNGRIDDED_COORDS(:)
    integer,                    optional              :: FIELD_TYPE
    integer,                    optional              :: STAGGERING
    integer,                    optional              :: ROTATION
    type(ESMF_Grid)    , optional   , intent(OUT)     :: GRID
    logical            , optional   , intent(OUT)     :: doNotAllocate
    logical            , optional   , intent(OUT)     :: alwaysAllocate
    integer            , optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecGet"
    integer                               :: STATUS

      if(.not.associated(SPEC%SPECPtr)) then
       RETURN_(ESMF_FAILURE)
      endif

      if(present(STAT)) then
       STAT = SPEC%SPECPtr%STAT
      endif

      if(present(SHORT_NAME)) then
       SHORT_NAME = SPEC%SPECPtr%SHORT_NAME
      endif

      if(present(LONG_NAME)) then
       LONG_NAME = SPEC%SPECPtr%LONG_NAME
      endif

      if(present(UNITS)) then
       UNITS = SPEC%SPECPtr%UNITS
      endif

      if(present(FRIENDLYTO)) then
       FRIENDLYTO = SPEC%SPECPtr%FRIENDLYTO
      endif

      if(present(DIMS)) then
       DIMS = SPEC%SPECPtr%DIMS
      endif

      if(present(VLOCATION)) then
       VLOCATION = SPEC%SPECPtr%LOCATION
      endif

      if(present(NUM_SUBTILES)) then
       NUM_SUBTILES = SPEC%SPECPtr%NUM_SUBTILES
      endif

      if(present(ACCMLT_INTERVAL)) then
       ACCMLT_INTERVAL = SPEC%SPECPtr%ACCMLT_INTERVAL
      endif

      if(present(COUPLE_INTERVAL)) then
       COUPLE_INTERVAL = SPEC%SPECPtr%COUPLE_INTERVAL
      endif

      if(present(OFFSET)) then
       OFFSET = SPEC%SPECPtr%OFFSET
      endif

      if(present(LABEL)) then
       LABEL = SPEC%SPECPtr%LABEL
      endif

      if(present(DEFAULT)) then
       DEFAULT = SPEC%SPECPtr%DEFAULT
      endif

      if(present(defaultProvided)) then
         defaultProvided= SPEC%SPECPtr%defaultProvided
      endif

      if(present(FIELD)) then
       FIELD = SPEC%SPECPtr%FIELD
      endif

      if(present(BUNDLE)) then
       BUNDLE = SPEC%SPECPtr%BUNDLE
      endif

      if(present(STATE)) then
       STATE = SPEC%SPECPtr%STATE
      endif

      if(present(HALOWIDTH)) then
       HALOWIDTH = SPEC%SPECPtr%HALOWIDTH
      endif

      if(present(PRECISION)) then
       PRECISION = SPEC%SPECPtr%PRECISION
      endif

      if(present(RESTART)) then
       RESTART = SPEC%SPECPtr%RESTART
      endif

      if(present(ATTR_INAMES)) then
       ATTR_INAMES => SPEC%SPECPtr%ATTR_INAMES
      endif

      if(present(ATTR_RNAMES)) then
       ATTR_RNAMES => SPEC%SPECPtr%ATTR_RNAMES
      endif

      if(present(ATTR_IVALUES)) then
       ATTR_IVALUES => SPEC%SPECPtr%ATTR_IVALUES
      endif

      if(present(ATTR_RVALUES)) then
       ATTR_RVALUES => SPEC%SPECPtr%ATTR_RVALUES
      endif

      if(present(UNGRIDDED_DIMS)) then
       UNGRIDDED_DIMS => SPEC%SPECPtr%UNGRIDDED_DIMS
      endif

      if(present(UNGRIDDED_UNIT)) then
       UNGRIDDED_UNIT = SPEC%SPECPtr%UNGRIDDED_UNIT
      endif

      if(present(UNGRIDDED_NAME)) then
       UNGRIDDED_NAME = SPEC%SPECPtr%UNGRIDDED_NAME
      endif

      if(present(UNGRIDDED_COORDS)) then
       UNGRIDDED_COORDS => SPEC%SPECPtr%UNGRIDDED_COORDS
      endif

      if(present(FIELD_TYPE)) then
       FIELD_TYPE = SPEC%SPECPtr%FIELD_TYPE
      endif

      if(present(STAGGERING)) then
       STAGGERING = SPEC%SPECPtr%STAGGERING
      endif

      if(present(ROTATION)) then
       ROTATION = SPEC%SPECPtr%ROTATION
      endif

      if(present(GRID)) then
       GRID = SPEC%SPECPtr%GRID
      endif

      if(present(doNotAllocate)) then
         doNotAllocate = SPEC%SPECPtr%doNotAllocate
      endif

      if(present(alwaysAllocate)) then
         alwaysAllocate = SPEC%SPECPtr%alwaysAllocate
      endif

      RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_VarSpecGetRegular

  subroutine MAPL_VarSpecGetFieldPtr(SPEC, FIELDPTR, RC )

    type (MAPL_VarSpec ),             intent(IN )     :: SPEC
    type(ESMF_Field)   ,              pointer         :: FIELDPTR
    integer            , optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecGetFieldPtr"
    integer                               :: STATUS

      if(.not.associated(SPEC%SPECPtr)) then
       RETURN_(ESMF_FAILURE)
      endif

      FIELDPTR => SPEC%SPECPtr%FIELD

      RETURN_(ESMF_SUCCESS)

    end subroutine MAPL_VarSpecGetFieldPtr

  subroutine MAPL_VarSpecGetBundlePtr(SPEC, BundlePTR, RC )

    type (MAPL_VarSpec ),             intent(IN )     :: SPEC
    type(ESMF_FieldBundle)  ,         pointer         :: BUNDLEPTR
    integer            , optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecGetBundlePtr"
    integer                               :: STATUS

      if(.not.associated(SPEC%SPECPtr)) then
       RETURN_(ESMF_FAILURE)
      endif

      BUNDLEPTR => SPEC%SPECPtr%BUNDLE

      RETURN_(ESMF_SUCCESS)

    end subroutine MAPL_VarSpecGetBundlePtr

  subroutine MAPL_VarSpecGetStatePtr(SPEC, StatePTR, RC )

    type (MAPL_VarSpec ),             intent(IN )     :: SPEC
    type(ESMF_State)  ,               pointer         :: STATEPTR
    integer            , optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecGetStatePtr"
    integer                               :: STATUS

      if(.not.associated(SPEC%SPECPtr)) then
       RETURN_(ESMF_FAILURE)
      endif

      STATEPTR => SPEC%SPECPtr%STATE

      RETURN_(ESMF_SUCCESS)

    end subroutine MAPL_VarSpecGetStatePtr


  subroutine MAPL_VarSpecAddChildName(SPEC,CN,RC)

    type (MAPL_VarSpec ), pointer           :: SPEC(:)
    character(len=ESMF_MAXSTR), intent(IN ) :: CN
    integer, optional   , intent(OUT)       :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecAddChildName"
    integer                               :: STATUS

    integer K

    DO K=1,SIZE(SPEC)
       SPEC(K)%SPECptr%LONG_NAME = trim(SPEC(K)%SPECptr%LONG_NAME) // trim(CN)
    END DO


    RETURN_(ESMF_SUCCESS)
  END subroutine MAPL_VarSpecAddChildName


  subroutine MAPL_VarSpecReconnect(SPEC,ITEM,RC)

    type (MAPL_VarSpec ), pointer         :: SPEC(:)
    type (MAPL_VarSpec ), intent(INOUT)   :: ITEM
    integer, optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecReconnect"
    integer                               :: STATUS

    type(ESMF_Field), pointer             :: FIELD
    type(ESMF_FieldBundle), pointer       :: BUNDLE
    type(ESMF_State), pointer       :: STATE
    integer I
     
      if(.not.associated(ITEM%SPECPtr)) then
       RETURN_(ESMF_FAILURE)
      endif

      if(.not.associated(SPEC)) then
       RETURN_(ESMF_FAILURE)
      endif

      I=MAPL_VarSpecGetIndex(SPEC, ITEM, RC=STATUS)
      VERIFY_(STATUS)

      if (I == -1) then
       RETURN_(ESMF_FAILURE)
      endif

      if (associated(ITEM%SPECptr%FIELD)) then
          deallocate(ITEM%SPECptr%FIELD, STAT=STATUS)
          VERIFY_(STATUS)
      end if
      call MAPL_VarSpecGet(SPEC(I), FIELDPTR=FIELD, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_VarSpecSet(ITEM, FIELDPTR=FIELD, RC=STATUS)
      VERIFY_(STATUS)

      if (associated(ITEM%SPECptr%BUNDLE)) then
          deallocate(ITEM%SPECptr%BUNDLE, STAT=STATUS)
          VERIFY_(STATUS)
      end if
      call MAPL_VarSpecGet(SPEC(I), BUNDLEPTR=BUNDLE, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_VarSpecSet(ITEM, BUNDLEPTR=BUNDLE, RC=STATUS)
      VERIFY_(STATUS)

      if (associated(ITEM%SPECptr%STATE)) then
          deallocate(ITEM%SPECptr%STATE, STAT=STATUS)
          VERIFY_(STATUS)
      end if
      call MAPL_VarSpecGet(SPEC(I), STATEPTR=STATE, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_VarSpecSet(ITEM, STATEPTR=STATE, RC=STATUS)
      VERIFY_(STATUS)

!      deallocate(ITEM%SPECptr, stat=status)
!      VERIFY_(STATUS)

!!      ITEM%SPECptr => SPEC(I)%SPECPtr

      RETURN_(ESMF_SUCCESS)


    end subroutine MAPL_VarSpecReconnect

    function MAPL_VarSpecEQ(s1, s2)
      type (MAPL_VarSpec ), intent(in) :: s1, s2
      logical                          :: MAPL_VarSpecEQ

      MAPL_VarSpecEQ = .FALSE.

      if (S1%SPECPtr%SHORT_NAME      /= S2%SPECPtr%SHORT_NAME      ) RETURN
!ALT: for now we do not compare LONG_NAME nor UNITS
!BMA: we also are not comparing FIELD_TYPE i.e. vector or scalar
      if (S1%SPECPtr%DIMS            /= S2%SPECPtr%DIMS            ) RETURN
      if (S1%SPECPtr%LOCATION        /= S2%SPECPtr%LOCATION        ) RETURN
      if (S1%SPECPtr%HALOWIDTH       /= S2%SPECPtr%HALOWIDTH       ) RETURN
      if (S1%SPECPtr%PRECISION       /= S2%SPECPtr%PRECISION       ) RETURN
#if 0
      if (IOR(S1%SPECPtr%STAT,MAPL_CplSATISFIED) &
           /= IOR(S2%SPECPtr%STAT,MAPL_CplSATISFIED)) then
         RETURN
      end if
#endif
      if (S1%SPECPtr%ACCMLT_INTERVAL /= 0 .and. &
           S2%SPECPtr%ACCMLT_INTERVAL /= 0) then

         if (S1%SPECPtr%ACCMLT_INTERVAL /= S2%SPECPtr%ACCMLT_INTERVAL ) RETURN
         if (S1%SPECPtr%COUPLE_INTERVAL /= S2%SPECPtr%COUPLE_INTERVAL ) RETURN
      end if

      MAPL_VarSpecEQ = .TRUE.
      RETURN
    end function MAPL_VarSpecEQ

    function MAPL_VarSpecSamePrec(s1, s2)
      type (MAPL_VarSpec ), intent(in) :: s1, s2
      logical                          :: MAPL_VarSpecSamePrec

      MAPL_VarSpecSamePrec = .FALSE.

      if (S1%SPECPtr%PRECISION /= S2%SPECPtr%PRECISION       ) RETURN

      MAPL_VarSpecSamePrec = .TRUE.
      RETURN
    end function MAPL_VarSpecSamePrec
 
  subroutine MAPL_VarConnCreate(CONN, SHORT_NAME, TO_NAME, &
       FROM_IMPORT, FROM_EXPORT, TO_IMPORT, TO_EXPORT, RC  )

    type (MAPL_VarConn ),             pointer     :: CONN(:)
    character (len=*)             , intent(IN   ) :: SHORT_NAME
    character (len=*),    optional, intent(IN   ) :: TO_NAME
    integer,              optional, intent(IN   ) :: FROM_IMPORT
    integer,              optional, intent(IN   ) :: FROM_EXPORT
    integer,              optional, intent(IN   ) :: TO_IMPORT
    integer,              optional, intent(IN   ) :: TO_EXPORT
    integer,              optional, intent(  OUT) :: RC     ! Error code:
    


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarConnCreate"
    integer                               :: STATUS

    type (MAPL_VarConn ), pointer         :: TMP(:) => null()

    integer                    :: usableFROM_IMPORT
    integer                    :: usableFROM_EXPORT
    integer                    :: usableTO_IMPORT
    integer                    :: usableTO_EXPORT
    integer                    :: I
    character(len=ESMF_MAXSTR) :: usableTONAME

      if(.not. associated(CONN)) then
       allocate(CONN(0),stat=STATUS)
       VERIFY_(STATUS)
      else
!ALT: check for duplicates ???
      endif
    
      usableFROM_IMPORT=MAPL_ConnUnknown
      usableFROM_EXPORT=MAPL_ConnUnknown
      usableTO_IMPORT=MAPL_ConnUnknown
      usableTO_EXPORT=MAPL_ConnUnknown

      if(present(TO_NAME)) then
         usableTONAME = TO_NAME
      else
         usableTONAME = SHORT_NAME
      endif

      if(present(FROM_IMPORT)) then
         usableFROM_IMPORT=FROM_IMPORT
      endif
      if(present(FROM_EXPORT)) then
         usableFROM_EXPORT=FROM_EXPORT
      end if
      if(present(TO_IMPORT)) then
         usableTO_IMPORT=TO_IMPORT
      end if
      if(present(TO_EXPORT)) then
         usableTO_EXPORT=TO_EXPORT
      endif
        
      I = size(CONN)

      allocate(TMP(I+1),stat=STATUS)
      VERIFY_(STATUS)
      
      TMP(1:I) = CONN
      deallocate(CONN)

      allocate(TMP(I+1)%CONNPtr,stat=STATUS)
      VERIFY_(STATUS)

      TMP(I+1)%CONNPtr%From = MAPL_VarConnPoint(SHORT_NAME, &
           usableFROM_IMPORT,  usableFROM_EXPORT)

      TMP(I+1)%CONNPtr%To   = MAPL_VarConnPoint(usableTONAME, &
           usableTO_IMPORT,  usableTO_EXPORT)

      CONN => TMP

      RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_VarConnCreate

  subroutine MAPL_VarConnGet(CONN, SHORT_NAME, &
       FROM_IMPORT, FROM_EXPORT, TO_IMPORT, TO_EXPORT, RC  )

    type (MAPL_VarConn ),           pointer     :: CONN
    character (len=*),    optional, intent(OUT) :: SHORT_NAME
    integer,              optional, intent(OUT) :: FROM_IMPORT
    integer,              optional, intent(OUT) :: FROM_EXPORT
    integer,              optional, intent(OUT) :: TO_IMPORT
    integer,              optional, intent(OUT) :: TO_EXPORT
    integer,              optional, intent(OUT) :: RC     ! Error code:
    


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarConnGet"
    integer                               :: STATUS


    if(.not.associated(CONN%CONNPtr)) then
       RETURN_(ESMF_FAILURE)
    endif

    if(present(SHORT_NAME)) then
       SHORT_NAME = CONN%CONNPtr%FROM%SHORT_NAME
    endif

    if(present(FROM_IMPORT)) then
       FROM_IMPORT = CONN%CONNPtr%FROM%IMPORT
    endif

    if(present(FROM_EXPORT)) then
       FROM_EXPORT = CONN%CONNPtr%FROM%EXPORT
    endif

    if(present(TO_IMPORT)) then
       TO_IMPORT = CONN%CONNPtr%TO%IMPORT
    endif

    if(present(TO_EXPORT)) then
       TO_EXPORT = CONN%CONNPtr%TO%EXPORT
    endif

    RETURN_(ESMF_SUCCESS)
      
  end subroutine MAPL_VarConnGet

  logical function MAPL_VarIsConnectedEE(CONN, SHORT_NAME, &
                                         FROM_EXPORT, TO_EXPORT, RC)
    type (MAPL_VarConn ),           pointer       :: CONN(:)
    character (len=*),     intent(IN   ) :: SHORT_NAME
    integer,               intent(IN   ) :: FROM_EXPORT
    integer,               intent(IN   ) :: TO_EXPORT
    integer,               intent(  OUT) :: RC     ! Error code:


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarIsConnectedEE"
    integer                               :: STATUS
    integer                               :: I
    integer                               :: FI, TI, FE, TE

    if (.not. associated(CONN)) then
       MAPL_VarIsConnectedEE = .false.
       RETURN_(ESMF_SUCCESS)
    end if


    DO I = 1, size(CONN)
       if (CONN(I)%CONNptr%FROM%SHORT_NAME /= SHORT_NAME) then
          cycle
       end if
       FI = CONN(I)%CONNptr%FROM%IMPORT
       FE = CONN(I)%CONNptr%FROM%EXPORT
       TI = CONN(I)%CONNptr%TO%IMPORT
       TE = CONN(I)%CONNptr%TO%EXPORT

! check for a match
       IF ((FE == FROM_EXPORT) .and. (TE == TO_EXPORT)) then
! check consistency
          if (CONN(I)%CONNptr%TO%SHORT_NAME /= SHORT_NAME) then
             MAPL_VarIsConnectedEE = .false.
             RETURN_(ESMF_FAILURE)
          end if
          MAPL_VarIsConnectedEE = .true.
          CONN(I)%CONNptr%used = .true.
          RETURN_(ESMF_SUCCESS)
       END IF
       
       IF(MIN(FI,TI) /= MAPL_ConnUnknown) then
          MAPL_VarIsConnectedEE = .false.
          RETURN_(ESMF_FAILURE)
       end IF
!       IF(MIN(FE,TE) /= MAPL_ConnUnknown) then
!          MAPL_VarIsConnected = .false.
!          RETURN_(ESMF_FAILURE)
!       end IF

    END DO

    MAPL_VarIsConnectedEE = .false.
    RETURN_(ESMF_SUCCESS)
  end function MAPL_VarIsConnectedEE

  logical function MAPL_VarIsConnectedIE(CONN, IMPORT_NAME, EXPORT_NAME, &
                                         IMPORT, EXPORT, RC)
    type (MAPL_VarConn ),        pointer       :: CONN(:)
    character (len=*),           intent(IN   ) :: IMPORT_NAME
    character (len=*), optional, intent(  OUT) :: EXPORT_NAME
    integer,                     intent(IN   ) :: IMPORT
    integer,                     intent(IN   ) :: EXPORT
    integer,           optional, intent(  OUT) :: RC     ! Error code:


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarIsConnectedIE"
    integer                               :: STATUS
    integer                               :: I
    integer                               :: FI, TI, FE, TE

    MAPL_VarIsConnectedIE = .false.

    if (.not. associated(CONN)) then
       RETURN_(ESMF_SUCCESS)
    end if


! try to find a match with "FROM"
    DO I = 1, size(CONN)
       if (CONN(I)%CONNptr%FROM%SHORT_NAME /= IMPORT_NAME) then
          cycle
       end if
       FI = CONN(I)%CONNptr%FROM%IMPORT
       TE = CONN(I)%CONNptr%TO%EXPORT

       if (FI /= IMPORT) then
          cycle
       end if

       if (TE /= EXPORT) then
          cycle
       end if

       MAPL_VarIsConnectedIE = .true.
       CONN(I)%CONNptr%used = .true.
       if (present(EXPORT_NAME)) then
          EXPORT_NAME = CONN(I)%CONNptr%TO%SHORT_NAME
       end if
       RETURN_(ESMF_SUCCESS)
    END DO

! try to find a match with "TO"
    DO I = 1, size(CONN)
       if (CONN(I)%CONNptr%TO%SHORT_NAME /= IMPORT_NAME) then
          cycle
       end if
       TI = CONN(I)%CONNptr%TO%IMPORT
       FE = CONN(I)%CONNptr%FROM%EXPORT

       if (TI /= IMPORT) then
          cycle
       end if

       if (FE /= EXPORT) then
          cycle
       end if

       MAPL_VarIsConnectedIE = .true.
       CONN(I)%CONNptr%used = .true.
       if (present(EXPORT_NAME)) then
          EXPORT_NAME = CONN(I)%CONNptr%FROM%SHORT_NAME
       end if
       RETURN_(ESMF_SUCCESS)
    END DO


    MAPL_VarIsConnectedIE = .false.
    RETURN_(ESMF_SUCCESS)
  end function MAPL_VarIsConnectedIE

  logical function MAPL_VarIsListed(CONN, SHORT_NAME, IMPORT, RC)
    type (MAPL_VarConn ),             pointer   :: CONN(:)
    character (len=*),               intent(IN) :: SHORT_NAME
    integer,                         intent(IN) :: IMPORT
    integer,              optional, intent(OUT) :: RC     ! Error code:


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarIsListed"
    integer                               :: STATUS
    integer                               :: I
    integer                               :: FI, TI, FE, TE

    if (.not. associated(CONN)) then
       MAPL_VarIsListed = .false.
       RETURN_(ESMF_SUCCESS)
    end if

    DO I = 1, size(CONN)
       if (CONN(I)%CONNptr%FROM%SHORT_NAME /= SHORT_NAME) then
          cycle
       end if
       FI = CONN(I)%CONNptr%FROM%IMPORT
       FE = CONN(I)%CONNptr%FROM%EXPORT
       TI = CONN(I)%CONNptr%TO%IMPORT
       TE = CONN(I)%CONNptr%TO%EXPORT
! first check consistency
       IF(MIN(FI,TI) /= MAPL_ConnUnknown) then
          MAPL_VarIsListed = .false.
          RETURN_(ESMF_FAILURE)
       end IF
       IF(MIN(FE,TE) /= MAPL_ConnUnknown) then
          MAPL_VarIsListed = .false.
          RETURN_(ESMF_FAILURE)
       end IF
! check for a match
       IF(MAX(FI,TI) == IMPORT)  then
          MAPL_VarIsListed = .true.
          CONN(I)%CONNptr%used = .true.
          RETURN_(ESMF_SUCCESS)
       END IF
    END DO

    MAPL_VarIsListed = .false.
    RETURN_(ESMF_SUCCESS)
  end function MAPL_VarIsListed


  subroutine MAPL_VarSpecPrintOne(SPEC, RC )

    type (MAPL_VarSpec ),             intent(IN )     :: SPEC
    integer            , optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecPrint"
    integer                               :: STATUS
    character(len=3) :: tmp
    character(len=ESMF_MAXSTR)            :: string

    if(.not.associated(SPEC%SPECPtr)) then
       RETURN_(ESMF_FAILURE)
    endif

    write(tmp,'(i3)') spec%specptr%label
    CALL WRITE_PARALLEL('NAME = '//trim(SPEC%SPECPtr%SHORT_NAME) // ":" // &
                        trim(SPEC%SPECPtr%LONG_NAME)//":"//tmp)

    WRITE(string,*) 'ACCUMT =',SPEC%SPECPtr%ACCMLT_INTERVAL
    CALL WRITE_PARALLEL(trim(string))
    WRITE(string,*) 'COUPLE =',SPEC%SPECPtr%COUPLE_INTERVAL
    CALL WRITE_PARALLEL(trim(string))
!    WRITE(string,*) 'DIMS =',SPEC%SPECPtr%DIMS
!    CALL WRITE_PARALLEL(trim(string))

!    WRITE(string, *) 'LOCATION =',SPEC%SPECPtr%LOCATION
!    CALL WRITE_PARALLEL(trim(string))

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_VarSpecPrintOne

  subroutine MAPL_VarSpecPrintMany(SPEC, RC )

    type (MAPL_VarSpec ),             intent(IN )     :: SPEC(:)
    integer            , optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecPrintMany"
    integer                               :: STATUS
    integer                               :: I

!    if(.not.associated(SPEC)) then
!       RETURN_(ESMF_FAILURE)
!    endif

    DO I = 1, size(SPEC)
       call MAPL_VarSpecPrint(Spec(I), RC=status)
       VERIFY_(STATUS)
    END DO

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_VarSpecPrintMany


  subroutine MAPL_VarSpecPrint1CSV(SPEC, compName, RC )

    type (MAPL_VarSpec ),             intent(IN )     :: SPEC
    character(len=*),                 intent(IN )     :: compName
    integer            , optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecPrint1CSV"
    integer                               :: STATUS
    character(len=3)                      :: dimensions
    character(len=ESMF_MAXSTR)            :: specInfo

    if(.not.associated(SPEC%SPECPtr)) then
       RETURN_(ESMF_FAILURE)
    endif

    write(dimensions,'(i3)') spec%specptr%dims
    specInfo = '  '//trim(compName)//", " // &
                        trim(SPEC%SPECPtr%SHORT_NAME) // ", " // &
                        trim(SPEC%SPECPtr%LONG_NAME)//", "// &
                        trim(SPEC%SPECPtr%UNITS)//", "// dimensions
    if (MAPL_AM_I_ROOT()) write(6,'(a)') trim(specInfo)
    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_VarSpecPrint1CSV

  subroutine MAPL_VarSpecPrintCSV(SPEC, compName, RC )

    type (MAPL_VarSpec ),             intent(IN )     :: SPEC(:)
    character(len=*),                 intent(IN )     :: compName
    integer            , optional   , intent(OUT)     :: RC


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VarSpecPrintCSV"
    integer                               :: STATUS
    integer                               :: I

    DO I = 1, size(SPEC)
       call MAPL_VarSpecPrint1CSV(Spec(I), compName, RC=status)
       VERIFY_(STATUS)
    END DO

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_VarSpecPrintCSV

  logical function MAPL_ConnCheckUnused(CONN)
    type (MAPL_VarConn ),             pointer   :: CONN(:)


    integer                               :: I

    MAPL_ConnCheckUnused = .true.
    if (.not. associated(CONN)) then
           return
    end if

    DO I = 1, size(CONN)
       if (CONN(I)%CONNptr%notRequired) cycle
       if (.not. CONN(I)%CONNptr%USED) then
          MAPL_ConnCheckUnused = .false.
          call WRITE_PARALLEL('ERROR:' // &
               ' SRC_NAME:' // trim(CONN(I)%CONNptr%FROM%SHORT_NAME) // &
               ' DST_NAME:'//trim(CONN(I)%CONNptr%TO%SHORT_NAME) // &
               ' is not satisfied' )
       end if
    end DO

    return
  end function MAPL_ConnCheckUnused

  subroutine MAPL_ConnCheckReq(CONN, ImSpecPtr, ExSpecPtr, RC)
    type (MAPL_VarConn),        pointer   :: CONN(:)
    type (MAPL_VarSpecPtr),     pointer   :: ImSpecPtr(:)
    type (MAPL_VarSpecPtr),     pointer   :: ExSpecPtr(:)
    integer, optional,        intent(OUT) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ConnCheckReq"
    integer                               :: STATUS
    integer                               :: I, J
    integer                               :: IMP
    integer                               :: FI
    integer                               :: TI
    integer                               :: FE
    integer                               :: TE
    character(len=ESMF_MAXSTR)            :: NAME

    if (.not. associated(CONN)) then
       RETURN_(ESMF_SUCCESS)
    end if

    do I = 1, size(CONN)
       FI = CONN(I)%CONNptr%FROM%IMPORT
       TI = CONN(I)%CONNptr%TO%IMPORT

       IMP = MAPL_ConnUnknown
       if(FI /= MAPL_ConnUnknown) then
          IMP = FI
          NAME = CONN(I)%CONNptr%FROM%SHORT_NAME
       else if (TI /= MAPL_ConnUnknown) then
          IMP = TI
          NAME = CONN(I)%CONNptr%TO%SHORT_NAME
       end if

       if (IMP /= MAPL_ConnUnknown) then
          if(MAPL_VarSpecGetIndex(ImSpecPtr(IMP)%Spec, NAME)==-1) then

             FE = CONN(I)%CONNptr%FROM%EXPORT
             TE = CONN(I)%CONNptr%TO%EXPORT

             J = MAPL_ConnUnknown
             if(FE /= MAPL_ConnUnknown) then
                J = FE
                NAME = CONN(I)%CONNptr%FROM%SHORT_NAME
             else if (TE /= MAPL_ConnUnknown) then
                J = TE
                NAME = CONN(I)%CONNptr%TO%SHORT_NAME
             end if

             if(MAPL_VarSpecGetIndex(ExSpecPtr(J)%Spec, NAME)/=-1) then
!            Export does exist while import does not - we relax the requirement 
                CONN(I)%CONNptr%notRequired = .true.
             end if
          endif
       end if
       
    end do

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ConnCheckReq

end module MAPL_VarSpecMod
