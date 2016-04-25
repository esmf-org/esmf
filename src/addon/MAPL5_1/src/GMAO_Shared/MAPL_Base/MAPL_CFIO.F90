!  $Id$

#include "MAPL_Generic.h"

#define MPI_NULL_TAG 99

#define DEALOC_(A) if(associated(A)) then; A=0; call MAPL_DeAllocNodeArray(A,rc=STATUS); if(STATUS==MAPL_NoShm) deallocate(A, stat=STATUS); VERIFY_(STATUS); NULLIFY(A); endif

#define DEALOC2_(A) if(associated(A)) then; deallocate(A, stat=STATUS); VERIFY_(STATUS); NULLIFY(A); endif

module MAPL_CFIOMod

!BOP

! !MODULE: MAPL_CFIO --- CF Compliant I/O for ESMF

! !DESCRIPTION:  
!
! \input{MAPL_CFIODescr.tex}
!

! !USES:
!
  use ESMF
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_ConstantsMod
  use ESMF_CFIOMod  
  use ESMF_CFIOUtilMod
  use ESMF_CFIOFileMod
  use MAPL_IOMod
  use MAPL_HorzTransformMod
  use ESMFL_Mod
  use MAPL_ShmemMod
  use MAPL_CFIOServerMod

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  ! MAPL-style names
  ! ----------------
  public MAPL_CFIOCreate
  public MAPL_CFIOSet
  public MAPL_CFIOOpenWrite
  public MAPL_CFIOCreateWrite
  public MAPL_CFIOClose
  public MAPL_CFIOWrite
  public MAPL_CFIOWriteBundlePost
  public MAPL_CFIOWriteBundleWait
  public MAPL_CFIOWriteBundleWrite
  public MAPL_CFIORead
  public MAPL_CFIODestroy
  public MAPL_GetCurrentFile
  public MAPL_CFIOIsCreated
  public MAPL_CFIOGetFilename
  public MAPL_CFIOGetTimeString
  public MAPL_CFIOStartAsyncColl
  public MAPL_CFIOBcastIONode

  ! ESMF-style names
  ! ----------------
  public ESMF_ioRead     ! another name for MAPL_CFIORead
  public ESMF_ioCreate   ! another name for MAPL_CFIOCreate
  public ESMF_ioWrite    ! another name for MAPL_CFIOWrite
  public ESMF_ioDestroy  ! another name for MAPL_CFIODestroy 

! !PUBLIC TYPES:
!
  public MAPL_CFIO

!EOP

! !METHOD OVERLOADING:

!                     MAPL Consistent Naming Convention
!                     ---------------------------------

  interface MAPL_CFIOStartAsyncColl
     module procedure MAPL_CFIOStartAsyncColl
  end interface

  interface MAPL_CFIOBcastIONode
     module procedure MAPL_CFIOBcastIONode
  end interface

  interface MAPL_CFIOCreate
     module procedure MAPL_CFIOCreateFromBundle
     module procedure MAPL_CFIOCreateFromState
  end interface

  interface MAPL_CFIOWrite
     module procedure MAPL_CFIOWriteState
     module procedure MAPL_CFIOWriteBundle
  end interface

  interface MAPL_CFIORead
     module procedure MAPL_CFIOReadState
     module procedure MAPL_CFIOReadBundle
     module procedure MAPL_CFIOReadField
     module procedure MAPL_CFIOReadArray3D
     module procedure MAPL_CFIOReadArray2D
  end interface

!                     ESMF Consistent Naming Convention
!                     ---------------------------------

  interface ESMF_ioCreate
     module procedure MAPL_CFIOCreateFromBundle
     module procedure MAPL_CFIOCreateFromState
  end interface

  interface ESMF_ioRead
     module procedure MAPL_CFIOReadState
     module procedure MAPL_CFIOReadBundle
     module procedure MAPL_CFIOReadField
     module procedure MAPL_CFIOReadArray3D
     module procedure MAPL_CFIOReadArray2D
  end interface

  interface ESMF_ioWrite
     module procedure MAPL_CFIOWriteState
     module procedure MAPL_CFIOWriteBundle
  end interface

  interface ESMF_ioDestroy
     module procedure MAPL_CFIODestroy
  end interface

  type Ptr3Arr
     real, pointer              :: Ptr(:,:,:)
  end type Ptr3Arr

  type Ptr2Arr
     real, pointer              :: Ptr(:,:)
  end type Ptr2Arr

  !BOP
  !BOC
  type MAPL_CFIO
     private
     logical                    :: CREATED=.false.
     character(len=ESMF_MAXSTR) :: NAME
     character(len=ESMF_MAXSTR) :: fNAME
     character(len=ESMF_MAXSTR) :: format
     character(len=ESMF_MAXSTR) :: expid
     type(ESMF_CFIO)            :: CFIO
     integer                    :: XYOFFSET
     real                       :: VSCALE
     type(ESMF_TIMEINTERVAL)    :: OFFSET
     type(ESMF_CLOCK)           :: CLOCK
     type(ESMF_FIELDBUNDLE)     :: BUNDLE
     type(ESMF_GridComp)        :: GC
     type(ESMF_Grid)            :: Grid
     integer                    :: Root=1
     integer                    :: PartSize=1
     integer                    :: myPE
     integer                    :: numcores
     integer                    :: comm
     integer                    :: Order=-1
     integer                    :: Nbits=1000
     integer                    :: IM, JM, LM
     integer, pointer           :: SUBSET(:) => null()
     integer, pointer           :: VarDims(:)=>null()
     integer, pointer           :: VarType(:)=>null()
     integer, pointer           :: needVar(:)=>null()
     integer, pointer           :: pairList(:)=>null()
     character(len=ESMF_MAXSTR), &
                        pointer :: vectorList(:,:)=>null()
     logical                    :: Vinterp=.false.
     real                       :: pow=0.0
     character(len=ESMF_MAXSTR) :: Vvar
     character(len=3          ) :: Func
     character(len=ESMF_MAXSTR), &
                        pointer :: VarName(:)=>null()
     integer, pointer           :: Krank(:)=>null()
     real,    pointer           :: levs(:)=>null()
     type(MAPL_CommRequest), &
                        pointer :: reqs(:)=>null()
     type(MAPL_HorzTransform)   :: Trans
     logical                    :: async
     integer                    :: AsyncWorkRank
     integer                    :: globalComm
  end type MAPL_CFIO
  !EOC
  !EOP
  integer, parameter :: trans_tag=9999

  include "mpif.h"

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP
 
! !IROUTINE: MAPL_CFIOCreate --- Creates a MAPL CFIO Object
!
! !IIROUTINE: MAPL_CFIOCreateFromBundle --- Creates MAPL CFIO Object from a Bundle

!
! !INTERFACE:
!
  subroutine MAPL_CFIOCreateFromBundle ( MCFIO, NAME, CLOCK, BUNDLE, OFFSET,      &
                                         RESOLUTION, SUBSET, CHUNKSIZE, FREQUENCY, LEVELS, DESCR,    &
                                         XYOFFSET, VCOORD, VUNIT, VSCALE,         &
                                         SOURCE, INSTITUTION, COMMENT, CONTACT,   &
                                         FORMAT, EXPID, DEFLATE, GC,  ORDER, &
                                         NumCores, nbits, TM, Conservative,  &
                                         Async, VectorList, RC )
! !ARGUMENTS:
!
    type(MAPL_CFIO),             intent(OUT) :: MCFIO
    character(LEN=*),            intent(IN)  :: NAME
    type(ESMF_FIELDBUNDLE),      intent(INout)  :: BUNDLE
    type(ESMF_CLOCK),            intent(INout):: CLOCK
    type(ESMF_TIMEINTERVAL), &
                     optional,   intent(INout):: OFFSET
    integer,         optional,   pointer     :: RESOLUTION(:)
    real,            optional,   pointer     :: SUBSET(:)
    integer,         optional,   pointer     :: CHUNKSIZE(:)
    integer,         optional,   intent(IN)  :: FREQUENCY
    real,            optional,   pointer     :: LEVELS(:)
    character(LEN=*),optional,   intent(IN)  :: DESCR
    integer,         optional,   intent(IN)  :: XYOFFSET
    real,            optional,   intent(IN)  :: VSCALE
    integer,         optional,   intent(IN)  :: DEFLATE
    character(len=*),optional,   intent(IN)  :: VUNIT     
    character(len=*),optional,   intent(IN)  :: VCOORD     
    character(len=*),optional,   intent(IN)  :: source
    character(len=*),optional,   intent(IN)  :: institution     
    character(len=*),optional,   intent(IN)  :: comment
    character(len=*),optional,   intent(IN)  :: contact     
    character(len=*),optional,   intent(IN)  :: format
    character(len=*),optional,   intent(IN)  :: EXPID
    integer,         optional,   intent(IN)  :: Conservative
    type(ESMF_GridComp),optional,intent(IN)  :: GC
    integer,         optional,   intent(IN)  :: Order
    integer,         optional,   intent(IN)  :: Nbits
    integer,         optional,   intent(IN)  :: NumCores
    integer,         optional,   intent(IN)  :: TM
    logical,         optional,   intent(IN)  :: Async
    character(len=*),optional,   pointer     :: vectorList(:,:)
    integer,         optional,   intent(OUT) :: RC

#ifdef ___PROTEX___
!
   !DESCRIPTION:

   Creates a MAPL\_CFIO object from a Bundle. The MAPL\_CFIO objects
   is opaque and its properties can only be set by this method at
   creation. Currently, its properties cannot be queried. The object
   is used only as a handle in write operations. It is not needed for
   reading. 
  
   Its non-optional arguments associate a {\tt NAME}, an ESMF {\tt
   BUNDLE}, and a {\tt CLOCK} with the object. An ESMF TimeInterval
   {\tt OFFSET} is an optional argument that sets an offset between the
   time on the clock when eriting and the time stamp used for the data
   (defaults to no offset).

   The {\tt format} optional argument determines whether the write
   will use the linked self-describing format (SDF) library (HDF or
   netcdf) or write GrADS readable flat files. Currently only the SDF
   library option is supported.

   The remaining (optional) arguments are especialized and used
   primarily to support MAPL\_History, or to provide documentation in
   the form of character strings that will be placed in corresponding
   attributes in the SDF file.

  !REVISION HISTORY:
 
    19Apr2007 Todling  - Added ability to write out ak/bk
                       - Added experiment ID as optional argument

#endif
!
!EOP

! Locals
!-------

    character(len=ESMF_MAXSTR) :: Iam="MAPL_CFIOCreateFromBundle"
    integer                    :: STATUS


    type(ESMF_FIELD)    :: FIELD
    type(ESMF_GRID)     :: ESMFGRID
    type(ESMF_DELayout) :: LAYOUT
    type(ESMF_DistGrid) :: DISTGRID
    type(ESMF_TIME)     :: TIME
    type(ESMF_ALARM)    :: PERPETUAL
    type(ESMF_VM)       :: VM

    type(ESMF_CFIOVarInfo), pointer :: vars(:)
    type(ESMF_CFIOGrid),    pointer :: cfiogrid

    real, pointer  :: lats(:,:)
    real, pointer  :: lons(:,:)
    real, pointer  :: lats1d(:)
    real, pointer  :: lons1d(:)
    real, pointer  :: Local(:,:)
    real, pointer  :: lev (:  )
    real, pointer  :: ak  (:  )
    real, pointer  :: bk  (:  )
    real, pointer  :: ulevels(:  )
    real, pointer  :: Ptr3(:,:,:)
    real           :: RANGE(2)
    real           :: xoff, yoff

    real(KIND=8)           :: dlam, dphi
    real(KIND=8), pointer  :: R8D2(:,:)
    real(KIND=8), allocatable :: cornerX(:)
    real(KIND=8), allocatable :: cornerY(:)
    real(KIND=8), allocatable :: centerX(:)
    real(KIND=8), allocatable :: centerY(:)

    integer        :: L, WriteInterval, counts(5), dims(3)
    integer        :: NumVars
    integer        :: IM,JM,LM
    integer        :: gridRank
    integer        :: fieldRank
    integer        :: Comm, nPEs
    integer        :: hours, mins, secs, timeInc
    integer        :: I, J, II, LL, LT, K, LEN, IMO, JMO, IML, JML
    integer        :: IMSUB, JMSUB, IMBEG, IMEND, JMBEG, JMEND
    integer        :: VLOCATION
    integer        :: Field_Type
    integer        :: GROUP, NEWGROUP
    integer        :: L1, L2, Nlots, Extra, MyNumLevs, MyNumVars
    integer        :: Psize, Df
    integer        :: Num2DVars, Num3dVars
    integer        :: Nnodes, Ncores
    integer        :: YY,MM,DD,H,M,S
    integer        :: noffset

    integer, allocatable :: Location(:)
    integer        :: CNT

    logical        :: HASVLEVS
    logical        :: IS_HDF
    logical        :: Have2D, Have3D, HAVE_center, HAVE_edge, HAVE_ungrd
    logical        :: change_resolution
    logical        :: LPERP

    character(len=ESMF_MAXSTR)  :: VarName, Vunits
    character(len=ESMF_MAXSTR)  :: LongName
    character(len=ESMF_MAXSTR)  :: Units
    character(len=ESMF_MAXSTR)  :: StartTime
    character(len=esmf_maxstr)  :: Usource
    character(len=esmf_maxstr)  :: Uinstitution     
    character(len=esmf_maxstr)  :: Ucomment
    character(len=esmf_maxstr)  :: Ucontact     
    character(len=esmf_maxstr)  :: Utitle
    character(len=esmf_maxstr)  :: GridTypeAttribute

    character(len=ESMF_MAXSTR)  :: ClockName
    character(len=ESMF_MAXSTR)  :: Gridname
    character(len=ESMF_MAXSTR)  :: GridnameIn
    character(len=ESMF_MAXSTR)  :: GridnameOut
    character(len=ESMF_MAXSTR)  :: tileFile
    character(len=2)            :: date
    character(len=2)            :: pole
    integer                     :: nn
    logical                     :: EXACT
    logical                     :: isGridRectalinear
    real, pointer               :: ptr3d(:,:,:)
    integer, allocatable        :: vsize(:)
    logical, allocatable        :: HasUngrid(:)
    character(len=ESMF_MAXSTR), pointer :: ungridded_units(:) => null()
    character(len=ESMF_MAXSTR), pointer :: ungridded_names(:) => null()
    character(len=ESMF_MAXSTR)  :: ungridded_unit, ungridded_name
    integer                     :: ungrdsize
    real, allocatable           :: ungridded_coord(:)
    real, allocatable           :: ungridded_coords(:,:)
    logical                     :: unGrdNameCheck, unGrdUnitCheck, unGrdCoordCheck
    logical                     :: regridConservative
    logical                     :: found
    integer                     :: vectorListSize
! Begin
!------

    MCFIO%NAME   = NAME 
    MCFIO%CLOCK  = CLOCK
    MCFIO%BUNDLE = BUNDLE

! Number of variables in the bundle
!----------------------------------

    call ESMF_FieldBundleGet (BUNDLE, FieldCount=NumVars,    RC=STATUS)
    VERIFY_(STATUS)

    ASSERT_(NumVars>0)

! Process optionals
!------------------

    if(present(NBITS)) then
       MCFIO%Nbits = NBITS
    else
       MCFIO%nBits = 1000
    end if

    if(present(deflate)) then
       df = deflate
    else
       df = 0
    endif

    if(present(Order)) then
       MCFIO%Order = Order
    else
       MCFIO%Order = -1
    endif

    if(present(source)) then
       Usource = source
    else
       Usource = "unknown"
    endif

    if(present(institution)) then
       Uinstitution = institution
    else
       Uinstitution = "unknown"
    endif

    if(present(comment)) then
       Ucomment = comment
    else
       Ucomment = "unknown"
    endif

    if(present(contact)) then
       Ucontact = contact
    else
       Ucontact = "unknown"
    endif

    if(present(format)) then
       MCFIO%format = format
    else
       MCFIO%format = "SDF"
    endif

    if(present(expid)) then
       MCFIO%expid = expid
    else
       MCFIO%expid = "No_ExpId"
    endif

    if(present(descr )) then
       Utitle  = descr 
    else
       Utitle  = "unknown"
    endif

    if(present(LEVELS)) then
       ulevels => LEVELS
    else
       nullify(ulevels)
    endif

    if(present(GC)) then
       MCFIO%GC   = GC
    else
!ALT       MCFIO%GC   = ESMF_GridCompCreate("NULL")
    endif

    if(present(VUNIT)) then
       vunits = trim(vunit)
    else
       vunits = ""
    endif

    if (present(Async)) then
       mcfio%async = async
    else
       mcfio%async = .false.
    end if

    if (present(vectorList)) then
       if (associated(vectorList)) then
          mcfio%vectorList => vectorList
       else
          mcfio%vectorList => null()
       end if
    else
       mcfio%vectorList => null()
    end if

! Get CommBndl, the communicator that is spanned by fields in the bundle
!-----------------------------------------------------------------------

    call ESMF_FieldBundleGet(BUNDLE, 1, FIELD,               RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_FieldGet       (FIELD, grid=ESMFGRID,          RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_GridGet        (ESMFGRID, dimCount=gridRank,   rc=STATUS)
    VERIFY_(STATUS)
    call ESMF_GridGet        (ESMFGRID, NAME=Gridname, rc=status )
    VERIFY_(STATUS)
    call ESMF_GridGet        (ESMFGRID, distgrid=DISTGRID,   RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_DistGridGet    (DISTGRID, delayout=LAYOUT,     RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_DELayoutGet    (LAYOUT,   vm=VM,               RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_VMGet          (VM, mpiCommunicator=Comm,      RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_VMGet          (VM, localpet=mCFIO%MYPE,       RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_VMGet          (VM, petcount=NPES,             RC=STATUS)
    VERIFY_(STATUS)

! Save the ESMFGrid in the object
!--------------------------------

    mCFIO%comm = comm
    mCFIO%Grid = ESMFGRID

! Set the partition size to size of VM. Can be overridden later to call to MAPL_CFIOSet
! -------------------------------------------------------------------------------------

    if(present(NumCores)) then
       mcfio%Numcores = NumCores
    else
       mcfio%Numcores = MAPL_CoresPerNodeGet(comm,rc=status)
       VERIFY_(STATUS)
    end if

    mCFIO%partsize = size(MAPL_NodeRankList)

    regridConservative = .false.
    if (present(Conservative)) then
       if (Conservative /= 0) then
          regridConservative = .true.
       end if
    end if

! Vertical interpolation info
!----------------------------

    if(present(Vcoord)) then
       MCFIO%VVAR = adjustl(vcoord)
       MCFIO%Func = MCFIO%Vvar(1:3)
       if    (MCFIO%Func=='log') then
          MCFIO%Vvar = adjustl(MCFIO%Vvar(index(MCFIO%Vvar,'(')+1:index(MCFIO%Vvar,')')-1))
       elseif(MCFIO%Func=='pow') then
          read( MCFIO%Vvar(index(MCFIO%Vvar,',')+1:index(MCFIO%Vvar,')')-1) , *) mCFIO%pow 
          MCFIO%Vvar = adjustl(MCFIO%Vvar(index(MCFIO%Vvar,'(')+1:index(MCFIO%Vvar,',')-1))
       endif
    else
       MCFIO%VVAR = ""
       MCFIO%Func = ""
    end if

    if(present(vscale)) then
       MCFIO%Vscale = Vscale
    else
       MCFIO%Vscale = 1.0
    endif

! Determine the rank and vertical Location (Mid or Edge) of Fields within Bundle.
!   Note: If User-Defined ULEVELS is not present, ALL levels are written.
!   In this case, vertical Location must be consistent for ALL variables.
! ---------------------------------------------------------------------------------------

    allocate(MCFIO%VarDims(NumVars), stat=STATUS)
    VERIFY_(STATUS)
    allocate(MCFIO%VarName(NumVars), stat=STATUS)
    VERIFY_(STATUS)
    allocate(location     (NumVars), stat=STATUS)
    VERIFY_(STATUS)
    allocate(vsize        (NumVars), stat=STATUS)
    VERIFY_(STATUS)
    allocate(hasUngrid    (NumVars), stat=STATUS)
    VERIFY_(STATUS)
    allocate(MCFIO%VarType(NumVars), stat=STATUS)
    VERIFY_(STATUS)
    allocate(MCFIO%needVar(NumVars), stat=STATUS)
    VERIFY_(STATUS)
    allocate(ungridded_names(NumVars), stat=STATUS)
    VERIFY_(STATUS)
    ungridded_names=""
    allocate(ungridded_units(NumVars), stat=STATUS)
    VERIFY_(STATUS)
    ungridded_units=""

    VARIABLES_1: DO I = 1, NumVars

       call ESMF_FieldBundleGet(BUNDLE, I, FIELD,              RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_FieldGet      (FIELD, NAME= mCFIO%VarName(I), RC=STATUS)
       VERIFY_(STATUS)

       vsize(i) = 1
       hasUngrid(I) = .false.
       if(mCFIO%VarName(I)==MCFIO%Vvar) then
          MCFIO%VarDims(I) = -1
          LOCATION(i)      = MAPL_VLocationNone
       else
          call ESMF_FieldGet(FIELD, dimCount=fieldRank, RC=STATUS)
          VERIFY_(STATUS)
          ASSERT_(fieldRank <= 3)

          MCFIO%VarDims(I) = fieldRank

          call ESMF_AttributeGet(FIELD, NAME="VLOCATION", VALUE=LOCATION(I), RC=STATUS)
          if ( status /= ESMF_SUCCESS ) LOCATION(I) = MAPL_VLocationNone

          if (fieldRank == 3) then
             call ESMF_FieldGet(field, farrayPtr=ptr3d, rc=status)
             VERIFY_(STATUS)
             vsize(i) = size(ptr3d,3)
          end if

          if (fieldRank >= 3 .and. location(I) == MAPL_VLocationNone) then
             hasUngrid(I) = .true.
             call ESMF_AttributeGet(field,NAME="UNGRIDDED_UNIT",value=ungridded_unit,rc=status)
             VERIFY_(STATUS)
             call ESMF_AttributeGet(field,NAME="UNGRIDDED_NAME",value=ungridded_name,rc=status)
             VERIFY_(STATUS)
             ungridded_names(i) = ungridded_name
             ungridded_units(i) = ungridded_unit
             call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",itemcount=ungrdsize,rc=status)
             if (status==ESMF_SUCCESS .and. (ungrdsize/=0) ) then
                ASSERT_(vsize(i)==ungrdsize)
                if (.not.allocated(ungridded_coord)) allocate(ungridded_coord(ungrdsize),stat=status)
                if (.not.allocated(ungridded_coords)) allocate(ungridded_coords(NumVars,ungrdsize),stat=status)
                VERIFY_(STATUS)
                call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",valuelist=ungridded_coord,rc=status)
                VERIFY_(STATUS)
                ungridded_coords(i,:) = ungridded_coord
             end if
          end if

       endif

    end do VARIABLES_1
    ! now put a check in that we aren't trying to do something like have two different
    ! ungridded units if we have any ungridded dimensions, compare ungridded info for
    ! each variable to the last values retrieved, if any differ then obviously user
    ! is trying to put 2 different ungridded variables in a collection that have
    ! different attributes
    if (any(hasUngrid)) then
       do i=1,NumVars
          if (hasUngrid(i)) then
             unGrdUnitCheck = ungridded_units(i) /= ungridded_unit
             unGrdNameCheck = ungridded_names(i) /= ungridded_name
             if ( allocated(ungridded_coords) .and. allocated(ungridded_coords) ) then
                unGrdCoordCheck = any(ungridded_coords(i,:) /= ungridded_coord)
             else
                unGrdCoordCheck = .false.
             end if
             if ( unGrdUnitCheck .or. unGrdNameCheck .or. unGrdCoordCheck) then
                if (mapl_am_i_root()) write(*,*)'Ungridded attributes for variables in collection do not match'
                ASSERT_(.false.) 
             end if    
          end if
       end do
    end if


!ALT: next segment is here only for initial testing
! we need a better logic how to prepare needVar list
! possibilities are: add staggering, vector pair to spec
! special key words in collection, etc.
    mCFIO%needVar = 0
    vectorListSize = 0
    if (associated(mCFIO%vectorList)) then
       vectorListSize = size(mCFIO%vectorList,2)
    end if
    VARLOOP: DO I = 1, NumVars
       DO k = 1, vectorListSize
          if (mCFIO%varName(I) == mCFIO%vectorList(1,k)) then
             ! this is first component of a vector (i.e. U)
             ! find the index of the V component
             found = .false.
             DO J = 1, NumVars
                if (trim(mCFIO%varName(J)) == mCFIO%vectorList(2,k)) then
                   found = .true.
                   exit
                end if
             end DO
             ASSERT_(found)
             mCFIO%needVar(I) = J ! I am first component of the vector
          else if(mCFIO%varName(I) == mCFIO%vectorList(2,k)) then
             ! find the index of the U component
             ! store it as negative number by convension suggested by Max
             found = .false.
             DO J = 1, NumVars
                if (trim(mCFIO%varName(J)) == mCFIO%vectorList(1,k)) then
                   found = .true.
                   exit
                end if
             end DO
             ASSERT_(found)
             mCFIO%needVar(I) = -J ! I am second component of the vector
          end if
       end DO
    end DO VARLOOP

! Sizes of global grid in the bundle. Sizes in the SDF may be different.
!----------------------------------------------------------------------

    call MAPL_GridGet( MCFIO%GRID, globalCellCountPerDim=COUNTS,  &
                       localCellCountPerDim=DIMS,     RC=STATUS)
    VERIFY_(STATUS)

    IML = DIMS(1)
    JML = DIMS(2)

    IM = COUNTS(1)
    JM = COUNTS(2)

    HAVE_center = any(LOCATION==MAPL_VLocationCenter)
    HAVE_edge   = any(LOCATION==MAPL_VLocationEdge  )
    HAVE_ungrd  = any(hasUngrid)

    if     ( associated(ULEVELS)  ) then
       LM = size(ULEVELS)
       HAVE_edge = .false.
       if (HAVE_ungrd) then
          print *, 'ERROR: Specifying LEVELS is not allowed for UNGRIDDED vars'
          ASSERT_(.false.)
       end if
    else 

!      Check on proper levels
!      -----------------------
       if(HAVE_center .and.  HAVE_edge) then
          DO I = 1, NumVars
             IF (LOCATION(I)==MAPL_VLocationEdge) print*, mCFIO%VarName(I)
          ENDDO
          print *, 'ERROR: Mixed Vlocation in CFIO not allowed unless LEVELS is specified'
          ASSERT_(.false.)
       endif

       if( all(MCFIO%VarDims==2)) then
          LM = 1
       else if (HAVE_ungrd) then
          if (HAVE_center .or. HAVE_edge) then
             print *, 'ERROR: Mixed 3d and UNGRIDDED in CFIO not allowed'
             ASSERT_(.false.)
          end if
          if (minval(vsize) /= maxval(vsize)) then
             print *, 'ERROR: Outputting variables with different ungridded sizes in one collection'
             ASSERT_(.false.)
          end if 
          LM = maxval(vsize)
       else
          LM = COUNTS(3)
          if (HAVE_edge) LM = LM+1
       endif
    end if

    mCFIO%LM   = LM

! Allocate request arrays for non-blocking gathers of bundle variables
!  Each 2D variable and each level of each 3D variable needs a request
!---------------------------------------------------------------------

    Num2DVars = count(MCFIO%VarDims==2)
    Num3DVars = count(MCFIO%VarDims==3)

    Have2D    = Num2DVars > 0
    Have3D    = Num3DVars > 0

    ASSERT_(HAVE2D .or. HAVE3D)

    LT        = Num2DVars + Num3DVars*LM

    allocate( MCFIO%reqs (LT),stat=STATUS)
    VERIFY_(STATUS)
    allocate( MCFIO%Krank(LT),stat=STATUS)
    VERIFY_(STATUS)
    allocate(MCFIO%pairList(LT), stat=STATUS)
    VERIFY_(STATUS)

    MCFIO%pairList = 0

! Horizontal dimensions of output fields
!---------------------------------------

    IMO = IM
    JMO = JM

    if (present(RESOLUTION)) then
       if (associated(RESOLUTION)) then
          IMO = resolution(1)
          JMO = resolution(2)
       endif
    endif

    MCFIO%IM = IMO
    MCFIO%JM = JMO

    allocate(MCFIO%SUBSET(4), stat=STATUS)
    VERIFY_(STATUS)
    MCFIO%SUBSET(:) = [-1,-1,-1,-1]
!    if (present(SUBSET)) then 
!       if (associated(SUBSET)) then
!          MCFIO%SUBSET(:) = SUBSET
!       end if
!    end if

!ALT: this is first attempt to guess if the grid is rectalinear
!     i.e. if we need could use 1d LAT/LONs
!     we take clues from the gridname
    Gridname = AdjustL(Gridname)
    nn   = len_trim(Gridname)
    pole = Gridname(1:2)
    date = Gridname(nn-1:nn)

    EXACT = (pole=='PE' .or. pole=='PC' ) .and. &
            (date=='DC' .or. date=='DE' .or. date=='GC' )
    if (pole == 'XY' .or. pole =='xy') EXACT = .true.
    if (date == 'CF' .or. date =='cf') EXACT = .true. !ALT: we are not outputing true coordinates for cubed-sphere

    isGridRectalinear = .true. ! default: 1d LAT/LONs
    if (.not. EXACT) then
       isGridRectalinear = .false. ! 2d LAT/LONs
    endif

!ALT: if change of horizontal resolution is requested, we
!     assume uniform output grid
    if (IM /= IMO .or. JM /= JMO) isGridRectalinear=.true.

    if (isGridRectalinear) then
       allocate(LONS1D(IMO), STAT=status)
       VERIFY_(status)
       allocate(LATS1D(JMO), STAT=status)
       VERIFY_(status)
    else
       allocate(LONS1D(IMO*JMO), LATS1D(IMO*JMO), STAT=status)
       VERIFY_(status)
    endif

! Process horizontal resolution change
!-------------------------------------

    IMBEG = 1
    IMEND = IMO
    JMBEG = 1
    JMEND = JMO
    TRANSFORM: if (IM /= IMO .or. JM /= JMO) then
       if(present(xyoffset)) then
          select case(xyoffset)
          case(0)
             xoff = 0.0
             yoff = 0.0
          case(1)
             xoff = 0.5
             yoff = 0.0
          case(2)
             xoff = 0.0
             yoff = 0.5
          case(3)
             xoff = 0.5
             yoff = 0.5
          case default
             ASSERT_(.false.)
          end select
          mcfio%xyoffset = xyoffset
       else
          xoff = 0.0
          yoff = 0.0
          mcfio%xyoffset = 0
       endif

       dlam = 2.0d0*MAPL_PI_R8/IMO

       if(yoff>0) then
          dphi = MAPL_PI_R8/(JMO  )
       else
          dphi = MAPL_PI_R8/(JMO-1)
       end if

       if(mcfio%xyoffset == 0) then

          allocate(cornerX(IMO+1),cornerY(JMO+1), stat=status)
          VERIFY_(STATUS)
          allocate(centerX(IMO)  ,centerY(JMO)  , stat=status)
          VERIFY_(STATUS)

          cornerX(1) = -MAPL_PI_R8 - dlam/2
          do i = 1,IMO
             cornerX(i+1) = cornerX(i) + dlam
          enddo
       
          cornerY(1) = -MAPL_PI_R8/2 - dphi/2
          do j = 1,JMO
             cornerY(j+1) = cornerY(j) + dphi
          enddo

          do i=1,IMO
             centerX(i) = 0.5d0*(cornerX(I)+cornerX(I+1))
          enddo

          do j=1,JMO
             centerY(j) = 0.5d0*(cornerY(J)+cornerY(J+1))
          enddo

          lons1d = centerX*(180._8/MAPL_PI_R8)
          lats1d = centerY*(180._8/MAPL_PI_R8)

          deallocate(cornerx,cornery)
          deallocate(centerx,centery)
          
       else
          do i=1,IMO
             lons1d(i) = -MAPL_PI_R8     + (i-1+xoff)*dlam
          enddo

          do j=1,JMO
             lats1d(j) = -MAPL_PI_R8/2.0D0 + (j-1+yoff)*dphi
          enddo
          lats1d = lats1d*(180._8/MAPL_PI_R8)
          lons1d = lons1d*(180._8/MAPL_PI_R8)
       endif

       if (present(SUBSET)) then
          if (associated(SUBSET)) then
             do i=1,IMO
                if ( lons1d(i) .ge. real(SUBSET(1))) then
                   IMBEG = i
                   exit
                end if
             enddo
             do i=1,IMO
                if ( lons1d(IMO-i+1) .le. real(SUBSET(2))) then
                   IMEND = IMO - i + 1
                   exit
                end if
             enddo
             do j=1,JMO
                if ( lats1d(j) .ge. real(SUBSET(3))) then
                   JMBEG = j
                   exit
                end if
             enddo
             do j=1,JMO
                if ( lats1d(JMO-j+1) .le. real(SUBSET(4))) then
                   JMEND = JMO - j + 1
                   exit
                end if
             enddo
             IMSUB = IMEND-IMBEG+1
             JMSUB = JMEND-JMBEG+1
             MCFIO%IM = IMSUB
             MCFIO%JM = JMSUB
             MCFIO%SUBSET(1)=IMBEG
             MCFIO%SUBSET(2)=IMEND
             MCFIO%SUBSET(3)=JMBEG
             MCFIO%SUBSET(4)=JMEND
          endif
       endif

       call ESMF_AttributeGet(ESMFGRID, name="GridType", value=GridTypeAttribute, rc=STATUS)
       if (STATUS /= ESMF_SUCCESS) then
          GridTypeAttribute = 'UNKNOWN'
       endif

! If order of transform was not specified, do binning for coarser,
!   and bilinear for finer.
!------------------------------------------------------------------

       if     (mCFIO%order<0) then
          if (IMO <  IM .or. JMO < JM) then
             mCFIO%order=0
          else
             mCFIO%order=1
          end if
       end if

! Create the transform at all pes, whether they need it or not.
!--------------------------------------------------------------

       if (regridConservative) then
          call MAPL_GenGridName(imo, jmo, xyoffset=mcfio%xyoffset, gridname=gridnameOut, geos_style=.false.)
          gridnameIn = gridname
          call MAPL_GeosNameNew(gridnameIn)

          tileFile=trim(adjustl(gridnameOut)) // '_' // &
                   trim(adjustl(gridnameIn))  // '.bin'

          call MAPL_HorzTransformCreate (mCFIO%Trans, tileFile, gridIn=gridname, RootOnly=.false., vm=vm, rc=status)
          VERIFY_(STATUS)

      else
          call MAPL_HorzTransformCreate (mCFIO%Trans, IM, JM, IMO, JMO, &
               GridTypeIn=GridTypeAttribute,                    &
               XYOFFSET=MCFIO%XYOFFSET, order=mCFIO%order, subset=MCFIO%subset, rc=STATUS)
          VERIFY_(STATUS)
       end if

    else

! Arrays of lats and lons from esmfgrid
!--------------------------------------

    if (JMO /= 6*IMO) then

       allocate(LONS (IM ,JM ),STAT=STATUS)
       VERIFY_(STATUS)
       allocate(LATS (IM ,JM ),STAT=STATUS)
       VERIFY_(STATUS)
       allocate(LOCAL(IML,JML),STAT=STATUS)
       VERIFY_(STATUS) 

       call ESMF_GridGetCoord(esmfgrid, localDE=0, coordDim=1, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=R8D2, rc=status)
       VERIFY_(STATUS)

       LOCAL = R8D2*(180._8/MAPL_PI_R8)
       call ArrayGather(LOCAL, LONS, ESMFGRID, RC=STATUS)
       VERIFY_(STATUS) 

       call ESMF_GridGetCoord(esmfgrid, localDE=0, coordDim=2, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=R8D2, rc=status)
       VERIFY_(STATUS) 

       LOCAL = R8D2*(180._8/MAPL_PI_R8)
       call ArrayGather(LOCAL, LATS, ESMFGRID, RC=STATUS)
       VERIFY_(STATUS)

       call MAPL_CommsBcast (layout, lons, size(lons), 0, rc=status)
       VERIFY_(STATUS)
       call MAPL_CommsBcast (layout, lats, size(lats), 0, rc=status)
       VERIFY_(STATUS)

       if (isGridRectalinear) then
          LONS1D = LONS(:,1)
          LATS1D = LATS(1,:)
       else
          k = 0
          do j = 1, jm
             do i = 1, im
                k = k+1
                LONS1D(k) = LONS(i,j)
                LATS1D(k) = LATS(i,j)
             end do
          end do
       endif

       DEALLOCATE(LOCAL)
       DEALLOCATE(LONS)
       DEALLOCATE(LATS)
  
     end if ! Cubed-Sphere ouput

    endif TRANSFORM

    if (JMO == 6*IMO) then
       if (isGridRectalinear) then
          do i = 1, size(lons1d)
             lons1d(i) = i
          end do
          do j = 1, size(lats1d)
             lats1d(j) = j
          end do
       end if
    end if

! Create the CFIO grid and populate it
!-------------------------------------

    allocate(CFIOGRID)

    CFIOGRID = ESMF_CFIOGridCreate(gName=trim(NAME)//"Grid", RC=STATUS)
    VERIFY_(STATUS)

! Horizontal grid info
!---------------------
    if (isGridRectalinear) then
       call ESMF_CFIOGridSet(CFIOGRID, LON=LONS1D(IMBEG:IMEND), LAT=LATS1D(JMBEG:JMEND),  TM=TM,  RC=STATUS)
       VERIFY_(STATUS)
    else
       call ESMF_CFIOGridSet(CFIOGRID, twoDimLat=.true., IM=IM, JM=JM, TM=TM,  &
            LON=LONS1D, LAT=LATS1D,  RC=STATUS)
       VERIFY_(STATUS)
    end if

    deallocate(LONS1D)
    deallocate(LATS1D)

! Vertical grid info
!--------------------

    mCFIO%Vinterp = .false.
    VERTGRID: if(HAVE3D) THEN
       allocate(LEV(LM), stat=status)
       VERIFY_(STATUS)
       
       if (associated(ULEVELS)) then
          LEV = ULEVELS
       else if (HAVE_edge) then
          LEV = (/(L, L=0,LM-1)/)
       else if (HAVE_ungrd) then
          if (allocated(ungridded_coord)) then
             lev = ungridded_coord
          else
             lev = (/(L, L=1,LM)/)
          end if
       else
          LEV = (/(L, L=1,LM)/)
       end if

       mCFIO%Vinterp = MCFIO%Vvar/=""

       allocate(mCFIO%levs(size(lev)), stat=status)
       VERIFY_(STATUS)
       mCFIO%levs = lev
       if (HAVE_ungrd) then
          call ESMF_CFIOGridSet(cfiogrid, levUnit=ungridded_unit, RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_CFIOGridSet(cfiogrid, standardName =ungridded_name, RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_CFIOGridSet(cfiogrid, coordinate ='N/A', RC=STATUS)
          VERIFY_(STATUS)
          mCFIO%levs = -mCFIO%levs
       else if(mCFIO%Vinterp) then
          if    (mCFIO%Func=='log') then
             mCFIO%levs = log(lev* MCFIO%vscale)
          elseif(mCFIO%Func=='pow') then
             mCFIO%levs = (lev* MCFIO%vscale)**mCFIO%pow
          else
             mCFIO%levs = lev* MCFIO%vscale
          end if

          if( trim(vunits).eq."" ) then
             call ESMF_AttributeGet(FIELD, NAME="UNITS", VALUE=units,         RC=STATUS)
             VERIFY_(STATUS)
             call ESMF_CFIOGridSet(cfiogrid, levUnit=trim(units),             RC=STATUS)
             VERIFY_(STATUS)
          else
             call ESMF_CFIOGridSet(cfiogrid, levUnit=trim(vunits),            RC=STATUS)
             VERIFY_(STATUS)
          endif
          call ESMF_CFIOGridSet(cfiogrid, standardName =trim(MCFIO%Vvar)//'_level', RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_CFIOGridSet(cfiogrid, coordinate   =trim(MCFIO%Vvar),           RC=STATUS)
          VERIFY_(STATUS)
       else
          call ESMF_CFIOGridSet(cfiogrid, levUnit      ='layer',              RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_CFIOGridSet(cfiogrid, standardName ='model_layers',       RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_CFIOGridSet(cfiogrid, coordinate   ='eta',                RC=STATUS)
          VERIFY_(STATUS)
       end if

       call ESMF_CFIOGridSet(cfiogrid, lev=abs(lev),  RC=STATUS)
       VERIFY_(STATUS)

       deallocate(LEV)

    else

       call ESMF_CFIOGridSet(cfiogrid, levUnit      ='none',         RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_CFIOGridSet(cfiogrid, standardName ='2d_fields',    RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_CFIOGridSet(cfiogrid, coordinate   ='N/A',          RC=STATUS)
       VERIFY_(STATUS)

    end if VERTGRID

! Create variable objects
!------------------------

    allocate(vars(Num2DVars+Num3dVars), stat=status)
    VERIFY_(STATUS)

! Disable range checking
!-----------------------

    RANGE(2) =  MAPL_UNDEF
    RANGE(1) = -MAPL_UNDEF

    K = 0

    mCFIO%vartype = MAPL_ScalarField
    VARIABLES_2: do L=1,NumVars

       if(mCFIO%VarDims(L)<1) cycle

       K = K + 1

       call ESMF_FieldBundleGet(BUNDLE, L, FIELD,                       RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeGet  (FIELD, NAME="LONG_NAME",VALUE=LongName, RC=STATUS)
       if ( status /= 0 ) LongName = mCFIO%VarName(L)
       call ESMF_AttributeGet  (FIELD, NAME="UNITS"    ,VALUE=Units,    RC=STATUS)
       if ( status /= 0 ) Units = 'unknown'
       call ESMF_AttributeGet  (FIELD, NAME="FIELD_TYPE",VALUE=Field_Type, RC=STATUS)
       if ( status /= 0 ) Field_Type = MAPL_ScalarField
       mCFIO%vartype(L) = Field_Type

       VARS(K) = ESMF_CFIOVarInfoCreate(vName=trim(mCFIO%VarName(L)),   RC=STATUS)
       VERIFY_(STATUS)

       call ESMF_CFIOVarInfoSet(VARS(K),           &
            vName        = mCFIO%VarName(L),       &
            vTitle       = LongName,               &
            grid         = cfioGRID,               &
            amiss        = MAPL_Undef,             &
            scaleFactor  = 1.,                     &
            addOffSet    = 0.,                     &
            standardName = LongName,               &
            twoDimVar    = MCFIO%VarDims(L)==2,    &
            validRange   = RANGE,                  &
            vUnits       = UNITS,                  &
                                         RC=STATUS )
       VERIFY_(STATUS)

       if (present(CHUNKSIZE)) then
          if (associated(CHUNKSIZE)) then
             call ESMF_CFIOVarInfoSet(VARS(K), &
                  ChunkSize = ChunkSize,       &
                         rc = status           )
          end if
       end if

    end do VARIABLES_2

! Get time info from the clock. Note the optional offset
!-------------------------------------------------------

    call ESMF_ClockGet(CLOCK, name=clockname, CurrTime =TIME, RC=STATUS)
    VERIFY_(STATUS)

    if(present(OFFSET)) then

        call ESMF_TimeIntervalGet( OFFSET, S=noffset, rc=status )
        VERIFY_(STATUS)
        if( noffset /= 0 ) then
            LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
        if( LPERP ) then
            call ESMF_ClockGetAlarm ( clock, alarmname='PERPETUAL', alarm=PERPETUAL, rc=status )
            VERIFY_(STATUS)
            if( ESMF_AlarmIsRinging(PERPETUAL) ) then
                call ESMF_TimeGet ( Time, YY = YY, &
                                          MM = MM, &
                                          DD = DD, &
                                          H  = H , &
                                          M  = M , &
                                          S  = S, rc=status )
                                          MM = MM + 1
                call ESMF_TimeSet ( Time, YY = YY, &
                                          MM = MM, &
                                          DD = DD, &
                                          H  = H , &
                                          M  = M , &
                                          S  = S, rc=status )
      if( MAPL_AM_I_ROOT() ) write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside Create: ",YY,MM,DD,H,M,S
            endif
        endif
        endif

       TIME = TIME  - OFFSET
       MCFIO%OFFSET = OFFSET
    else
      call ESMF_TimeIntervalSet( MCFIO%OFFSET, S=0, rc=status )
      VERIFY_(STATUS)
    endif

    call ESMF_TimeGet (TIME,  timeString=StartTime, RC=STATUS)
    VERIFY_(STATUS)

! Create CFIO object
!-------------------

    MCFIO%cfio =  ESMF_CFIOCreate(cfioObjName=trim(Name))

! Set Internal MetaCode Writing interval. Default of 6 hours. If set to 0
!  it is reset to 6 hours.Currently CFIO and GFIO expect timeIncrement to be 
!  in HHMMSS format, this imposes severe limitations to the frequency of the output:
!  no writes should be done less frequently than once every 4 days (99 hours)
! ------------------------------------------------------------------------------

    if (present(FREQUENCY)) then
       !ASSERT_(FREQUENCY <= 4*86400)
       if (frequency == 0 ) then
          writeInterval = 21600
       else
          writeInterval = FREQUENCY
       endif
    else
       writeInterval = 21600
    end if

    hours         = writeInterval/3600
    writeInterval = writeInterval-3600*hours
             mins = writeInterval/60
             secs = writeInterval-60*mins

    timeinc = 10000*hours + 100*mins + secs

! Set global attributes
!----------------------

    call ESMF_CFIOSet(MCFIO%CFIO,                                 &
         varObjs     = VARS,                                      &
         grid        = cfioGRID,                                  &
         format      = MCFIO%Format,                              &
         expid       = MCFIO%ExpId,                               &
         TimeString  = trim(StartTime),                           &
         timeInc     = timeInc,                                   &
         title       = trim(Utitle),                              &
         source      = Usource,                                   &
         history     = 'File written by MAPL_CFIO',               &
         institution = Uinstitution,                              &
         convention  = "COARDS",                                  &
         contact     = Ucontact,                                  &
         references  = "http://gmao.gsfc.nasa.gov",               &
         comment     = Ucomment,                                  & 
         prec        = 0,                                         &
         deflate     = df,                                        &
         RC=STATUS )
    VERIFY_(STATUS)

! Create AK/BKs
! -------------

    if(HAVE3D) then
       call ESMF_AttributeGet(ESMFGRID, NAME='ak', itemcount=CNT, RC=STATUS)
       if (STATUS==ESMF_SUCCESS .and. CNT>0) then
          allocate ( ak(CNT), bk(CNT), stat=status )
          VERIFY_(STATUS)

          call ESMF_AttributeGet(ESMFGRID, name='ak', valueList=ak, rc=STATUS)
          VERIFY_(STATUS)
          call ESMF_CFIOSet(MCFIO%cfio, attRealName='ak', attReal=ak )

          call ESMF_AttributeGet(ESMFGRID, name='bk', valuelist=bk, rc=STATUS)
          VERIFY_(STATUS)
          call ESMF_CFIOSet(MCFIO%cfio, attRealName='bk', attReal=bk )

          deallocate ( ak, bk )
       end if
    endif

! All Done
!---------

    MCFIO%CREATED = .true.

    deallocate(hasUngrid)
    deallocate(vsize)
    deallocate(location)
    deallocate(vars)
    deallocate(cfiogrid)
    deallocate(ungridded_names)
    deallocate(ungridded_units)
    if (allocated(ungridded_coord)) then
       deallocate(ungridded_coord)
    end if
    if (allocated(ungridded_coords)) then
       deallocate(ungridded_coords)
    end if

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_CFIOCreateFromBundle


  subroutine MAPL_CFIOCreatewrite ( MCFIO, nsteps, RC )

    type(MAPL_CFIO),           intent(INOUT) :: MCFIO
    integer,         optional, intent(   IN) :: nsteps
    integer,         optional, intent(  OUT) :: RC

    type(ESMF_Time)                :: CurrTime
    type(ESMF_Alarm)               :: PERPETUAL
    character(len=ESMF_MAXSTR)     :: StartTime
    character(len=ESMF_MAXSTR)     :: ClockName

    character(len=ESMF_MAXSTR)     :: Iam="MAPL_CFIOCreatewrite"
    logical                        :: LPERP
    integer                        :: YY,MM,DD,H,M,S
    integer                        :: noffset
    integer                        :: STATUS


    if(present(nsteps)) then
       call ESMF_CFIOSet(MCFIO%CFIO, nsteps=nsteps, RC=STATUS)
       VERIFY_(STATUS)
    else
       call ESMF_CFIOSet(MCFIO%CFIO, nsteps=1, RC=STATUS)
       VERIFY_(STATUS)
    endif 

! Get time info from the clock. Note the optional offset
!-------------------------------------------------------
    
    call ESMF_ClockGet(mCFIO%CLOCK, name=clockname, CurrTime=CurrTime, RC=STATUS)
    VERIFY_(STATUS)

        call ESMF_TimeIntervalGet( mCFIO%OFFSET, S=noffset, rc=status )
        VERIFY_(STATUS)
        if( noffset /= 0 ) then
            LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
        if( LPERP ) then
            call ESMF_ClockGetAlarm ( mCFIO%CLOCK, alarmName='PERPETUAL', alarm=PERPETUAL, rc=status )
            VERIFY_(STATUS)
            if( ESMF_AlarmIsRinging(PERPETUAL) ) then
                call ESMF_TimeGet ( CurrTime, YY = YY, &
                                              MM = MM, &
                                              DD = DD, &
                                              H  = H , &
                                              M  = M , &
                                              S  = S, rc=status )
                                              MM = MM + 1
                call ESMF_TimeSet ( CurrTime, YY = YY, &
                                              MM = MM, &
                                              DD = DD, &
                                              H  = H , &
                                              M  = M , &
                                              S  = S, rc=status )
            endif
        endif
        endif

    CurrTime = CurrTime - mCFIO%OFFSET

    call ESMF_TimeGet (CurrTime,  timeString=StartTime, RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_CFIOSet(MCFIO%CFIO, TimeString=trim(StartTime), RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_CFIOSet(MCFIO%CFIO, fName=trim(mCFIO%fName),     RC=STATUS)
    VERIFY_(STATUS)

! Create FILE from the root of the partition working on this bundle.
!------------------------------------------------------------------

    AMROOT: if (mCFIO%MYPE==MAPL_NodeRankList(MCFIO%Root)%rank(1)) then

       call ESMF_CFIOFileCreate(MCFIO%CFIO, format=MCFIO%format, &
                                expid=MCFIO%EXPID,      RC=STATUS)
       VERIFY_(STATUS)
!      print *, ' Created CFIO File: ', trim(mCFIO%fName)

    end if AMROOT

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_CFIOCreateWrite



  subroutine MAPL_CFIOOpenWrite ( MCFIO, RC )

    type(MAPL_CFIO),           intent(INOUT) :: MCFIO
    integer,         optional, intent(  OUT) :: RC

    character(len=ESMF_MAXSTR)     :: Iam="MAPL_CFIOOpenWrite"
    integer                        :: STATUS


! Open the file for writing only at the root process
!---------------------------------------------------

    AMROOT: if (mCFIO%MYPE==MAPL_NodeRankList(MCFIO%Root)%rank(1)) then
       call ESMF_CFIOFileOpen(MCFIO%CFIO, fmode=0, RC=STATUS)
       VERIFY_(STATUS)
!      print *, ' Opened CFIO File: ', trim(mCFIO%fName)
    end if AMROOT

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_CFIOOpenWrite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP

! !IIROUTINE: MAPL_CFIOCreateFromState --- Creates MAPL CFIO Object from a State

! !INTERFACE:
!
  subroutine MAPL_CFIOCreateFromState ( MCFIO, NAME, CLOCK, STATE, OFFSET,  &
                                        RESOLUTION, SUBSET, CHUNKSIZE, FREQUENCY, &
                                        LEVELS, DESCR, BUNDLE, &
                                        XYOFFSET, VCOORD, VUNIT, VSCALE,   &
                                        SOURCE, INSTITUTION, COMMENT, CONTACT, &
                                        FORMAT, EXPID, DEFLATE, GC,  ORDER, &
                                        NumCores, nbits, TM, Conservative,  RC )

!
! !ARGUMENTS:
!
    type(MAPL_CFIO),             intent(OUT) :: MCFIO
    character(LEN=*),            intent(IN)  :: NAME
    type(ESMF_State),            intent(INout)  :: STATE
    type(ESMF_Clock),            intent(INOUT)  :: CLOCK
    type(ESMF_FieldBundle), optional,  pointer    :: BUNDLE
    type(ESMF_TimeInterval), &
                     optional,   intent(INOUT):: OFFSET
    integer,         optional,   pointer     :: RESOLUTION(:)
    real,            optional,   pointer     :: SUBSET(:)
    integer,         optional,   pointer     :: CHUNKSIZE(:)
    integer,         optional,   intent(IN)  :: FREQUENCY
    real,            optional,   pointer     :: LEVELS(:)
    character(LEN=*),optional,   intent(IN)  :: DESCR
    real,            optional,   intent(IN)  :: VSCALE
    character(len=*),optional,   intent(IN)  :: VUNIT     
    character(len=*),optional,   intent(IN)  :: VCOORD     
    integer,         optional,   intent(IN)  :: XYOFFSET
    character(len=*),optional,   intent(IN)  :: source
    character(len=*),optional,   intent(IN)  :: institution     
    character(len=*),optional,   intent(IN)  :: comment
    character(len=*),optional,   intent(IN)  :: contact     
    character(len=*),optional,   intent(IN)  :: format
    character(len=*),optional,   intent(IN)  :: EXPID
    integer,         optional,   intent(IN)  :: DEFLATE
    type(ESMF_GridComp),optional,intent(IN)  :: GC
    integer,         optional,   intent(IN)  :: Order
    integer,         optional,   intent(IN)  :: Nbits
    integer,         optional,   intent(IN)  :: NumCores
    integer,         optional,   intent(IN)  :: TM
    integer,         optional,   intent(IN)  :: CONSERVATIVE
    integer, optional,           intent(OUT) :: RC
!
#ifdef ___PROTEX___

    !DESCRIPTION: 

     Creates a MAPL\_CFIO object from a State. States are written by
     ``serializing'' all Fields in them, whether they are directly in
     the State or are contained within a hierarchy of embedded Bundles
     and States, into a single Bundle.

     The Method optionally returns a pointer to the serialized ESMF
     Bundle, but this is not needed for MAPL\_Write
     operations. Otherwise arguments are the same as for
     CreateFromBundle.

   Its non-optional arguments associate a {\tt NAME}, an ESMF {\tt
   BUNDLE}, and a {\tt CLOCK} with the object. An ESMF TimeInterval
   {\tt OFFSET} is an optional argument that sets an offset between the
   time on the clock when eriting and the time stamp used for the data
   (defaults to no offset).

   The {\tt format} optional argument determines whether the write
   will use the linked self-describing format (SDF) library (HDF or
   netcdf) or write GrADS readable flat files. Currently only the SDF
   library option is supported.

   The remaining (optional) arguments are especialized and used
   primarily to support MAPL\_History, or to provide documentation in
   the form of character strings that will be placed in corresponding
   attributes in the SDF file.

  !REVISION HISTORY:
 
   12Jun2007 Todling  Added EXPID as opt argument

#endif

!EOP

    character(len=ESMF_MAXSTR)     :: Iam="MAPL_CFIOCreateFromState"
    integer                        :: STATUS

! Locals

    type(ESMF_FieldBundle), target :: tBUNDLE

!   Create an empty bundle
!   ----------------------

    tBUNDLE = ESMF_FieldBundleCreate ( name=Iam, rc=STATUS )
    VERIFY_(STATUS)
    
!   Serialize the state
!   -------------------

    call ESMFL_BundleAddState ( tBUNDLE, STATE, rc=STATUS, VALIDATE=.true. )
    VERIFY_(STATUS)

!   Create the mapl_CFIO object
!   ----------------------

    call MAPL_CFIOCreateFromBundle ( MCFIO, NAME, CLOCK, tBUNDLE,        &
                                     OFFSET = OFFSET,                    & 
                                     RESOLUTION=RESOLUTION,              &
                                     SUBSET=SUBSET,                      &
                                     CHUNKSIZE=CHUNKSIZE,                &
                                     FREQUENCY=FREQUENCY,                &
                                     LEVELS=LEVELS,                      &
                                     DESCR=DESCR,                        &
                                     XYOFFSET= XYOFFSET,                 &
                                     VCOORD  = VCOORD,                   &
                                     VUNIT   = VUNIT,                    &
                                     VSCALE  = VSCALE,                   &
                                     SOURCE  = SOURCE,                   &
                                     INSTITUTION = INSTITUTION,          &
                                     COMMENT = COMMENT,                  &
                                     CONTACT = CONTACT,                  &
                                     FORMAT  = FORMAT,                   &
                                     EXPID   = EXPID,                    &
                                     DEFLATE = DEFLATE,                  &
                                     GC      = GC,                       &
                                     ORDER   = ORDER,                    &
                                     NumCores= NUMCORES,                 &
                                     nbits   = NBITS,                    &
                                     TM      = TM,                       &
                                     CONSERVATIVE=CONSERVATIVE,          &
                                                               RC=STATUS )
    VERIFY_(STATUS)

    if ( present(BUNDLE) ) then
         BUNDLE => tBUNDLE
    end if

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_CFIOCreateFromState
    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP

! !IROUTINE: MAPL_CFIOWrite --- Writing Methods
! !IIROUTINE: MAPL_CFIOWriteBundle --- Writes an ESMF Bundle 

! !INTERFACE:
!
  subroutine MAPL_CFIOWriteBundlePost( MCFIO, RC )
!
! !ARGUMENTS:
!
    type(MAPL_CFIO  ),               intent(INOUT) :: MCFIO
    integer,               optional, intent(  OUT) :: RC
!
#ifdef ___PROTEX___

 !DESCRIPTION:  

       Writes an ESMF Bundle to a File. Only the MAPL\_CFIO object is
       a required argument as pointers to the actual data to be
       written is recorded in it during creation.

       {\tt CLOCK, BUNDLE} can be used to override the choice
       made at creation, but this is of dubious value, particularly
       for {\tt BUNDLE} since it must be excatly conformant with the
       creation {\tt BUNDLE}. {\tt NBITS} if the number of bits of 
       the mantissa to retain. This is used to write files with degraded
       precision, which can then be compressed with standard utilities.
       The default is no degradation of precision.

       {\bf A note about compression.} NetCDF-4, HDF-4 and HDF-5 all
       support transparent internal GZIP compression of the data being
       written. However, very little is gained by compressing float
       point fields from earth system models. Compression yields can
       be greatly increased by setting to zero bits in the mantissa of float
       numbers. On average 50\% compression can be achieved, while
       preserving a meaningful accuracy in the fields. Unlike
       classical CF compression by means of {\tt scale\_factor} and
       {\tt add\_offset} attributes, internal GZIP compression
       requires no special handling by the users of the data. In fact,
       they do not even need to know that the data is compressed! At this
       point, MAPL\_CFIO does not activate this GZIP compression
       feature in the files being written, but the resulting precision 
       degredaded files can be compressed offline with the HDF-4 
       {\tt hrepack} utility.

#endif

!EOP

    character(len=*), parameter:: Iam="MAPL_CFIOWriteBundlePost"
    integer                    :: status

    type(ESMF_FIELD)           :: FIELD
    integer                    :: L, K, k0, II, LL, LM
    integer                    :: NN, Nnodes
    integer                    :: nv, np
    integer                    :: sgn
    integer,      allocatable  :: varStart(:)
    real,             pointer  :: Ptr2(:,:), Ptr3(:,:,:)
    real, target, allocatable  :: Ple3d(:,:,:)
    real,         allocatable  :: Pl3d(:,:,:)
    real,         allocatable  :: Ptrx(:,:,:)
    real,             pointer  :: layer(:,:)
    

!                              ---

    ASSERT_(MCFIO%CREATED)

!  Set centers and edges of interpolating field
!----------------------------------------------

    if(mCFIO%Vinterp) then
       call ESMF_FieldBundleGet(mCFIO%bundle, fieldName=mCFIO%Vvar, Field=Field,  RC=STATUS)
       VERIFY_(STATUS)

       nullify (ptr3)
       call ESMF_FieldGet(Field, localDE=0, farrayPtr=Ptr3, rc=status)
       VERIFY_(STATUS)

       allocate( LAYER(size(Ptr3,1),size(Ptr3,2) ), stat=status)
       VERIFY_(STATUS)

! The Ptr3 interpolating variable is a zero-based (0-LM) edge variable
!---------------------------------------------------------------------
       if(lbound(PTR3,3)==0) then
          allocate( ple3D(size(Ptr3,1),size(Ptr3,2),size(Ptr3,3)  ), stat=status)
          VERIFY_(STATUS)
          allocate(  pl3D(size(Ptr3,1),size(Ptr3,2),size(Ptr3,3)-1), stat=status)
          VERIFY_(STATUS)

          if    (mCFIO%Func=='log') then
             ple3D = log(Ptr3)
             pl3D  = log( 0.5*(Ptr3(:,:,1:)+Ptr3(:,:,0:ubound(Ptr3,3)-1)) )
          elseif(mCFIO%Func=='pow') then
             ple3D = Ptr3**mCFIO%pow
             pl3D  =    ( 0.5*(Ptr3(:,:,1:)+Ptr3(:,:,0:ubound(Ptr3,3)-1)) )**mCFIO%pow
          else
             ple3D = Ptr3
             pl3D  =    ( 0.5*(Ptr3(:,:,1:)+Ptr3(:,:,0:ubound(Ptr3,3)-1)) )
          end if

       else

! The Ptr3 interpolating variable is a (1-LM) mid-layer variable
!---------------------------------------------------------------
          allocate(  Ptrx(size(Ptr3,1),size(Ptr3,2),0:size(Ptr3,3)  ), stat=status)
          VERIFY_(STATUS)
          allocate( ple3D(size(Ptr3,1),size(Ptr3,2),0:size(Ptr3,3)  ), stat=status)
          VERIFY_(STATUS)
          allocate(  pl3D(size(Ptr3,1),size(Ptr3,2),  size(Ptr3,3)  ), stat=status)
          VERIFY_(STATUS)

          Ptrx(:,:,0               ) = 0.5*( 3* Ptr3(:,:,1)             -Ptr3(:,:,2)                )
          Ptrx(:,:,1:size(Ptr3,3)-1) = 0.5*(    Ptr3(:,:,2:size(Ptr3,3))+Ptr3(:,:,1:size(Ptr3,3)-1) )
          Ptrx(:,:,  size(Ptr3,3)  ) = 0.5*( 3* Ptr3(:,:,  size(Ptr3,3))-Ptr3(:,:,  size(Ptr3,3)-1) )

          if    (mCFIO%Func=='log') then
             ple3D = log(Ptrx)
             pl3D  = log( 0.5*(Ptrx(:,:,1:)+Ptrx(:,:,0:ubound(Ptrx,3)-1)) )
          elseif(mCFIO%Func=='pow') then
             ple3D = Ptrx**mCFIO%pow
             pl3D  =    ( 0.5*(Ptrx(:,:,1:)+Ptrx(:,:,0:ubound(Ptrx,3)-1)) )**mCFIO%pow
          else
             ple3D = Ptrx
             pl3D  =    ( 0.5*(Ptrx(:,:,1:)+Ptrx(:,:,0:ubound(Ptrx,3)-1)) )
          end if

          deallocate(Ptrx)
       end if

    end if

! Counter for slices
!-------------------


! Layers will be collected to different processors in round-robin fashion
!    among the PEs in the partition.
!------------------------------------------------------------------------

    call MAPL_RoundRobinPEList(MCFIO%Krank,MCFIO%PartSize,root=MCFIO%ROOT,rc=status)
    VERIFY_(STATUS)

    allocate(varStart(size(MCFIO%VarDims)), stat=status)
    VERIFY_(status)

    nn = 0

    STARTVAR: do L=1, size(MCFIO%VarDims)
       if(mCFIO%VarDims(L)==2) then ! Rank == 2
          LM = 1
       elseif(MCFIO%VarDims(L)==3) then ! Rank == 3
          LM = MCFIO%LM
       else
          LM = 0
       endif
       if (L /= 0) then
          varStart(L) = nn + 1
          nn = nn + LM
       end if
    end do STARTVAR

    nn = 0

    VECTPAIR: do L=1, size(MCFIO%VarDims)
       if(mCFIO%VarDims(L)==2) then ! Rank == 2
          LM = 1
       elseif(MCFIO%VarDims(L)==3) then ! Rank == 3
          LM = MCFIO%LM
       else
          LM = 0
       endif

       nv = mCFIO%needVar(L)
       if (nv > 0) then
          sgn=1
       else
          sgn = -1
       end if

       do K=1,LM
          nn = nn + 1
          if (nv /= 0) then
             MCFIO%pairList(nn) = sgn*(varStart(abs(nv)) - 1 + k)
          else
             MCFIO%pairList(nn) = 0
          end if
       enddo
    end do VECTPAIR

    deallocate(varStart)


! Modify KRANK to make sure that for any pair,
! all of its components are handled by the SAME processor
    do L=1,size(MCFIO%Krank)
       np = MCFIO%pairList(L)
       if (np > 0) then
          ! I am "U"; overwrite location of "V"
          mCFIO%Krank(abs(np)) = mCFIO%Krank(L)
       end if
    enddo

! Cycle through all variables posting receives.
!----------------------------------------------

    nn = 0

    POSTRECV: do L=1, size(MCFIO%VarDims)
       if    (mCFIO%VarDims(L)==2) then ! Rank == 2
          LM = 1
       elseif(MCFIO%VarDims(L)==3) then ! Rank == 3
          LM = MCFIO%LM
       else
          LM = 0
       endif

       do K=1,LM
          nn    = nn + 1
          call MAPL_CreateRequest(MCFIO%GRID, MCFIO%Krank(nn), MCFIO%reqs(nn), &
                                  tag=nn, RequestType=MAPL_IsGather, RC=STATUS)
          VERIFY_(STATUS)
       enddo
    end do POSTRECV

! Cycle through all variables posting sends.
!-------------------------------------------

    nn = 0

    VARIABLES: do L=1, size(MCFIO%VarDims)

       call ESMF_FieldBundleGet(MCFIO%BUNDLE, L, FIELD,                  RC=STATUS)
       VERIFY_(STATUS)

! We treat only fields with rank 2 (horizontal 2D) and 
!  rank 3 (first 2 dimension are horz, third is vert).
!--------------------------------------------------------

       RANK: if (MCFIO%VarDims(L)==2) then ! Rank == 2

          nn    = nn + 1
          ptr2  =>null()

          call ESMF_FieldGet      (FIELD, localDE=0, farrayPtr=PTR2, RC=STATUS)
          VERIFY_(STATUS)
          call MAPL_ArrayIGather  (Ptr2, MCFIO%reqs(NN), RC=STATUS)
          VERIFY_(STATUS)

       elseif(MCFIO%VarDims(L)==3) then ! Rank == 3

          ptr3 =>null()

          call ESMF_FieldGet      (FIELD, localDE=0, farrayPtr=PTR3, RC=STATUS)
          VERIFY_(STATUS)

          K0  = lbound(PTR3,3) - 1

! For each level, interpolate vertically and post gather
!-------------------------------------------------------

          LAYERS: do K=1,MCFIO%LM
             VINTERP: if(mCFIO%Vinterp) then
                call VertInterp(LAYER, PTR3, MCFIO%LEVS(K), ple3d, pl3d, rc=status)
                VERIFY_(STATUS)
             else if (MCFIO%LEVS(K)<0) then
                LAYER => PTR3(:,:,K+K0)
             else
                LAYER => PTR3(:,:,nint(MCFIO%LEVS(K)))
             end if VINTERP

             nn    = nn + 1

             call MAPL_ArrayIGather(LAYER, MCFIO%reqs(nn), rc=status)
             VERIFY_(STATUS)
          enddo LAYERS

       end if RANK

    end do VARIABLES



!   if(mCFIO%myPE==mCFIO%Root) then
!      print *, ' Posted to File: ', trim(mCFIO%fName)
!   endif


    if(mCFIO%Vinterp) then
       deallocate( ple3D, stat=status)
       VERIFY_(STATUS)
       deallocate( pl3D , stat=status)
       VERIFY_(STATUS)
       deallocate( Layer, stat=status)
       VERIFY_(STATUS)
    end if

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_CFIOWriteBundlePost

  subroutine MAPL_CFIOWriteBundleWait( MCFIO, CLOCK, RC )

    type(MAPL_CFIO  ),                 intent(INOUT) :: MCFIO
    type(ESMF_CLOCK),                  intent(INOUT) :: CLOCK
    integer,                optional,  intent(  OUT) :: RC

! Locals
!-------

    character(len=*), parameter:: Iam="MAPL_CFIOWriteBundleWait"
    integer                    :: status

    integer                    :: L, K, NN
    integer                    :: YY,MM,DD,H,M,S
    integer                    :: noffset
    logical                    :: AmRoot, MyGlobal
    real,          pointer     :: Gptr3Out(:,:,:)
    real,          pointer     :: Gptr2Out(:,:  )
    real,          pointer     :: PtrGlob (:,:  )
    integer                    :: counts(5)
    integer                    :: IM0,JM0,I,IP
    logical                    :: FixPole, SubSet
    integer                    :: levsize
    integer                    :: lm, n, nv
    logical                    :: transAlreadyDone
    type(Ptr2Arr), allocatable :: globPtrArr(:)
    type(Ptr2Arr)              :: PtrTypeIn(2)
    type(Ptr2Arr)              :: PtrTypeOut(2)

! Space for global arrays is allocated everywhere, even if not used.
!------------------------------------------------------------------

    ASSERT_(MCFIO%CREATED)

! Allocate global 2d and 3d arrays at the writing resolution
!  Note that everybody allocated these.
!-----------------------------------------------------------

    call MAPL_GridGet( MCFIO%GRID, globalCellCountPerDim=COUNTS, RC=STATUS)
    VERIFY_(STATUS)

    IM0 = COUNTS(1)
    JM0 = COUNTS(2)

    !if(any(mCFIO%myPE==mCFIO%Krank)) then
       !allocate(Gptr3Out(Mcfio%IM, Mcfio%JM,1), stat=STATUS)
       !VERIFY_(STATUS)

       !Gptr2Out => Gptr3Out(:,:,1)
       !Gptr2Out(:,:) = 0.0
    !end if

    nn   = 0

    AmRoot     = mCFIO%myPE==MAPL_NodeRankList(MCFIO%Root)%rank(1)

    allocate(globPtrArr(size(mCFIO%reqs)), stat=status)
    VERIFY_(STATUS)
    COLCTVWAIT: do nn=1,size(mCFIO%reqs)
       ! Wait on request for slice nn
       !-----------------------------
       call MAPL_CollectiveWait(MCFIO%reqs(nn), DstArray=PtrGlob,  rc=status)
       VERIFY_(STATUS)
       globPtrArr(nn)%ptr => PtrGlob ! this is valid only if myGlobal is .true.
    end do COLCTVWAIT

    nn   = 0
    SubSet = .not. all(MCFIO%subset == -1)
    VARIABLES: do L=1,size(MCFIO%VarDims)
          
       FixPole = (MCFIO%VarType(L) == MAPL_VectorField) .and. &
                 (JM0              == 6*IM0)            .and. &
                 (Mcfio%JM         /= 6*mcfio%IM)       .and. &
                 (SubSet        .eqv. .false.                 ) 
 
       RANK: if (MCFIO%VarDims(L)==2) then
          LM = 1
       else  if (MCFIO%VarDims(L)==3) then
          LM = MCFIO%lm
       else
          LM = 0
       end if RANK

       LEVELS: do k=1,LM
          nn       = nn + 1
          MyGlobal = mCFIO%Krank(nn) == MCFIO%MYPE
          PtrGlob => globPtrArr(nn)%ptr

! Horizontal Interpolation and Shaving on PEs with global data
! ------------------------------------------------------------

          if( MyGlobal ) then
             nv = mCFIO%pairList(nn)
             VECTORTEST: if (nv == 0) then
                ! scalar
                allocate( MCFIO%reqs(nn)%Trans_Array(Mcfio%IM, Mcfio%JM, 1), stat=STATUS )
                VERIFY_(STATUS)
                Gptr2Out => MCFIO%reqs(nn)%Trans_Array(:,:,1)
                PtrTypeIn (1)%ptr => globPtrArr(nn)%ptr
                PtrTypeOut(1)%ptr => Gptr2Out
                call TransShaveAndSend(PtrTypeIn(1:1),PtrTypeOut(1:1),MCFIO%reqs(nn)%s_rqst,doTrans=.true.,IdxOut=1)
             else if (nv > 0) then 
                ! I am U part of vector
                if (associated(MCFIO%reqs(nn)%Trans_Array)) then
                   ASSERT_(associated(MCFIO%reqs(nv)%Trans_Array))
                   TransAlreadyDone = .true.
                else
                   TransAlreadyDone = .false.
                   allocate( MCFIO%reqs(nn)%Trans_Array(Mcfio%IM, Mcfio%JM, 1), stat=STATUS )
                   VERIFY_(STATUS)
                   allocate( MCFIO%reqs(nv)%Trans_Array(Mcfio%IM, Mcfio%JM, 1), stat=STATUS )
                   VERIFY_(STATUS)
                endif
                PtrTypeIn (1)%ptr => globPtrArr(nn)%ptr
                PtrTypeIn (2)%ptr => globPtrArr(nv)%ptr
                PtrTypeOut(1)%ptr => MCFIO%reqs(nn)%Trans_Array(:,:,1)
                PtrTypeOut(2)%ptr => MCFIO%reqs(nv)%Trans_Array(:,:,1)
                call TransShaveAndSend(PtrTypeIn(1:2),PtrTypeOut(1:2),MCFIO%reqs(nn)%s_rqst,doTrans=.not.TransAlreadyDone,IdxOut=1)
             else 
                ! I am V part of vector
                nv = abs(nv)
                if (associated(MCFIO%reqs(nn)%Trans_Array)) then
                   ASSERT_(associated(MCFIO%reqs(nv)%Trans_Array))
                   TransAlreadyDone = .true.
                else
                   TransAlreadyDone = .false.
                   allocate( MCFIO%reqs(nn)%Trans_Array(Mcfio%IM, Mcfio%JM, 1), stat=STATUS )
                   VERIFY_(STATUS)
                   allocate( MCFIO%reqs(nv)%Trans_Array(Mcfio%IM, Mcfio%JM, 1), stat=STATUS )
                   VERIFY_(STATUS)
                endif
                PtrTypeIn (1)%ptr => globPtrArr(nv)%ptr
                PtrTypeIn (2)%ptr => globPtrArr(nn)%ptr
                PtrTypeOut(1)%ptr => MCFIO%reqs(nv)%Trans_Array(:,:,1)
                PtrTypeOut(2)%ptr => MCFIO%reqs(nn)%Trans_Array(:,:,1)
                call TransShaveAndSend(PtrTypeIn(1:2),PtrTypeOut(1:2),MCFIO%reqs(nn)%s_rqst,doTrans=.not.TransAlreadyDone,IdxOut=2)
             end if VECTORTEST
          endif
       end do LEVELS

    end do VARIABLES

!    do nn=1,size(mCFIO%reqs)
!       MyGlobal = MCFIO%Krank(nn) == MCFIO%MYPE
!       if (myGlobal) then
!          deallocate(globPtrArr(nn)%ptr)
!          NULLIFY(globPtrArr(nn)%ptr)
!       end if
!    end do
    deallocate(globPtrArr)

   !if(AmRoot) then
   !   write(6,'(1X,"TransShaveAndSend: ",i6," Slices (",i3," Nodes, ",i2," CoresPerNode) to File:  ",a)') &
   !         size(MCFIO%reqs),mCFIO%partsize/mCFIO%numcores,mCFIO%numcores,trim(mCFIO%fName)
   !endif

    !if (any(mCFIO%myPE==mCFIO%Krank)) then
       !deallocate(Gptr3Out, stat=STATUS)
       !VERIFY_(STATUS)
    !end if

    RETURN_(ESMF_SUCCESS)

  contains
    
    subroutine TransShaveAndSend(PtrIn,PtrOut,request,doTrans,idxOut)
      use, intrinsic :: ISO_C_BINDING
      type(Ptr2Arr) :: PtrIn(:)
      type(Ptr2Arr) :: PtrOut(:)
      integer       :: request
      logical       :: doTrans
      integer       :: idxOut

      real, pointer :: Gin (:,:)
      real, pointer :: Gout(:,:)
      real, dimension(:,:,:), pointer :: uin, uout, vin, vout
      integer :: im, jm

      if (size(PtrIn) == 1) then
         ASSERT_(idxOut ==1)
         Gin => PtrIn(1)%ptr
         Gout => PtrOut(1)%ptr

         if (MAPL_HorzTransformIsCreated(mCFIO%Trans)) then
            call MAPL_HorzTransformRun(mCFIO%Trans, Gin, Gout, MAPL_undef, rc=STATUS)
            VERIFY_(STATUS)

            ! if going from CS to LAT-LON pole winds are wrong, approximate fix below
            if (FixPole) then
               do i=1,mcfio%im
                  ip = i+(mcfio%im/2)
                  if (ip > mcfio%im) ip = ip - mcfio%im
                  if ( (gout(i,mcfio%jm-1) == MAPL_UNDEF) .or. (gout(ip,mcfio%jm-1) == MAPL_UNDEF)) then
                     gout(i,mcfio%jm) = MAPL_UNDEF
                  else
                     gout(i,mcfio%jm)=(gout(i,mcfio%jm-1)-gout(ip,mcfio%jm-1))/2.0
                  end if
                  if ( (gout(i,2) == MAPL_UNDEF) .or. (gout(ip,2) == MAPL_UNDEF)) then
                     gout(i,1) = MAPL_UNDEF
                  else
                     gout(i,1)=(gout(i,2)-gout(ip,2))/2.0
                  endif
               enddo
            endif

         else
            Gout = Gin
         endif
         deallocate(Gin)
         nullify   (Gin)
      else
         ASSERT_(size(PtrIn) == 2) 
         ASSERT_(size(PtrOut) == 2) 
         Gout => PtrOut(idxOut)%ptr
         if (doTrans) then
            if (MAPL_HorzTransformIsCreated(mCFIO%Trans)) then
               im = size(PtrIn(1)%ptr,1)
               jm = size(PtrIn(1)%ptr,2)
               call C_F_POINTER (C_LOC(PtrIn(1)%ptr(1,1)), uin,[im,jm,1])
               call C_F_POINTER (C_LOC(PtrIn(2)%ptr(1,1)), vin,[im,jm,1])
!@#               allocate(uin(im,jm,1), vin(im,jm,1))
!@#               uin(:,:,1) = PtrIn(1)%ptr
!@#               vin(:,:,1) = PtrIn(2)%ptr
               im = size(PtrOut(1)%ptr,1)
               jm = size(PtrOut(1)%ptr,2)
               call C_F_POINTER (C_LOC(PtrOut(1)%ptr(1,1)), uout,[im,jm,1])
               call C_F_POINTER (C_LOC(PtrOut(2)%ptr(1,1)), vout,[im,jm,1])
!@#               allocate(uout(im,jm,1), vout(im,jm,1))
               call MAPL_HorzTransformRun(mCFIO%Trans, &
                    uin, vin, uout, vout, &
                    MAPL_undef, rc=STATUS)
               VERIFY_(STATUS)
!@#               PtrOut(1)%ptr = uout(:,:,1)
!@#               PtrOut(2)%ptr = vout(:,:,1)
!@#               deallocate(uin, vin)
!@#               deallocate(uout, vout)
            else
               do i = 1,size(PtrIn)
                  PtrOut(i)%ptr = PtrIn(i)%ptr
               end do
            end if
            deallocate(PtrIn(1)%ptr)
            nullify(PtrIn(1)%ptr)
            deallocate(PtrIn(2)%ptr)
            nullify(PtrIn(2)%ptr)
         end if
      end if

      if(mCFIO%NBITS < 24) then
         call ESMF_CFIODownBit ( Gout, Gout, mCFIO%NBITS, undef=MAPL_undef, rc=STATUS )
         VERIFY_(STATUS)
      end if

      if (mcfio%async) then
         levsize = mcfio%im*mcfio%jm
         call MPI_ISend(Gout,levsize,MPI_REAL,mcfio%asyncWorkRank, &
              MAPL_TAG_SHIPDATA,mCFIO%globalComm,request,status)
         VERIFY_(STATUS)
      else
         call MPI_ISend(Gout, size(Gout), MPI_REAL, MAPL_NodeRankList(MCFIO%Root)%rank(1), &
                 trans_tag, mCFIO%comm, request,         STATUS)
            VERIFY_(STATUS)

      end if


      return
    end subroutine TransShaveAndSend

  end subroutine MAPL_CFIOWriteBundleWait

  subroutine MAPL_CFIOWriteBundleWrite( MCFIO, CLOCK, RC )

    type(MAPL_CFIO  ),                 intent(INOUT) :: MCFIO
    type(ESMF_CLOCK),                  intent(INOUT) :: CLOCK
    integer,                optional,  intent(  OUT) :: RC

! Locals
!-------

    character(len=*), parameter:: Iam="MAPL_CFIOWriteBundleWrite"
    integer                    :: status

    integer                    :: L, K, NN
    integer                    :: YY,MM,DD,H,M,S
    integer                    :: noffset
    logical                    :: AmRoot, MyGlobal, LPERP
    type(ESMF_TIME )           :: TIME
    type(ESMF_Alarm)           :: PERPETUAL
    character(len=ESMF_MAXSTR) :: DATE
    character(len=ESMF_MAXSTR) :: ClockName
    real,          pointer     :: Gptr3Out(:,:,:)
    real,          pointer     :: Gptr2Out(:,:  )
    real,          pointer     :: PtrGlob (:,:  )
    integer                    :: counts(5)
    integer                    :: IM0,JM0,I,IP
    logical                    :: FixPole, SubSet
    integer                    :: nymd,nhms

! Space for global arrays is allocated everywhere, even if not used.
!------------------------------------------------------------------

    ASSERT_(MCFIO%CREATED)

! Set the time at which we will be writing from the clock
!--------------------------------------------------------

    ASYNCIF: if (.not.mcfio%async) then

       call ESMF_ClockGet       (CLOCK, name=ClockName, CurrTime =TIME, RC=STATUS)
       VERIFY_(STATUS)

           call ESMF_TimeIntervalGet( MCFIO%OFFSET, S=noffset, rc=status )
           VERIFY_(STATUS)
           if( noffset /= 0 ) then
               LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
           if( LPERP ) then
               call ESMF_ClockGetAlarm ( clock, alarmName='PERPETUAL', alarm=PERPETUAL, rc=status )
               VERIFY_(STATUS)
               if( ESMF_AlarmIsRinging(PERPETUAL) ) then
                   call ESMF_TimeGet ( Time, YY = YY, &
                                             MM = MM, &
                                             DD = DD, &
                                             H  = H , &
                                             M  = M , &
                                             S  = S, rc=status )
                                             MM = MM + 1
                   call ESMF_TimeSet ( Time, YY = YY, &
                                             MM = MM, &
                                             DD = DD, &
                                             H  = H , &
                                             M  = M , &
                                             S  = S, rc=status )
               endif
           endif
           endif

       TIME = TIME - MCFIO%OFFSET

       call ESMF_TimeGet        (TIME,     timeString=DATE,     RC=STATUS)
       VERIFY_(STATUS)

   ! Allocate global 2d and 3d arrays at the writing resolution
   !  Note that everybody allocated these.
   !-----------------------------------------------------------

       call MAPL_GridGet( MCFIO%GRID, globalCellCountPerDim=COUNTS, RC=STATUS)
       VERIFY_(STATUS)

       IM0 = COUNTS(1)
       JM0 = COUNTS(2)

       if(any(mCFIO%myPE==mCFIO%Krank)) then
          allocate(Gptr3Out(Mcfio%IM, Mcfio%JM,1), stat=STATUS)
          VERIFY_(STATUS)

          Gptr2Out => Gptr3Out(:,:,1)
          Gptr2Out(:,:) = 0.0
       end if

       AmRoot     = mCFIO%myPE==MAPL_NodeRankList(MCFIO%Root)%rank(1)

       SubSet = .not. all(MCFIO%subset == -1)
   !
   ! Finally Do The Writes
   !______________________

       nn   = 0
       VARIABLESW: do L=1,size(MCFIO%VarDims)

          RANKW: if (MCFIO%VarDims(L)==2) then
             nn       = nn + 1
             MyGlobal = mCFIO%Krank(nn) == MCFIO%MYPE

   ! Horizontal Interpolation and Shaving on PEs with global data
   ! ------------------------------------------------------------

             IAMVARROOT: if(AmRoot) then
                Gptr2Out => Gptr3Out(:,:,1)
                call MPI_Recv(Gptr2Out,size(Gptr2Out),MPI_REAL, mCFIO%Krank(nn), &
                              trans_tag,  mCFIO%comm, MPI_STATUS_IGNORE, STATUS)
                VERIFY_(STATUS)

                call StrToInt(date,nymd,nhms)
                call ESMF_CFIOVarWrite(MCFIO%CFIO, trim(MCFIO%VARNAME(L)), &
                                       Gptr2Out, timeString=DATE,  RC=STATUS)
                VERIFY_(STATUS)

             end if IAMVARROOT

          elseif (MCFIO%VarDims(L)==3) then

   ! Everyone waits, processes their layer, and sends it to root.
   !   Root write it out.
   !-------------------------------------------------------------

             LEVELSW: do k=1,MCFIO%lm
                nn       = nn + 1
                MyGlobal = MCFIO%Krank(nn) == MCFIO%MYPE

                IAMLEVROOT: if(AmRoot) then
                   Gptr2Out => Gptr3Out(:,:,1)
                   call MPI_Recv(Gptr2Out, size(Gptr2Out), MPI_REAL, mCFIO%Krank(nn), &
                                 trans_tag,  mCFIO%comm, MPI_STATUS_IGNORE,   STATUS)
                   VERIFY_(STATUS)

                   call StrToInt(date,nymd,nhms)
                   call ESMF_CFIOVarWrite(MCFIO%CFIO, trim(MCFIO%VARNAME(L)), &
                                          Gptr3Out, kbeg=K, kount=1,          &
                                          timeString=DATE,           RC=STATUS)
                   VERIFY_(STATUS)

                end if IAMLEVROOT
             end do LEVELSW
          endif RANKW

       end do VARIABLESW

    end if ASYNCIF

   !if(AmRoot) then
   !   write(6,'(1X,"Wrote: ",i6," Slices (",i3," Nodes, ",i2," CoresPerNode) to File:  ",a)') &
   !         size(MCFIO%reqs),mCFIO%partsize/mCFIO%numcores,mCFIO%numcores,trim(mCFIO%fName)
   !endif

! Clean-up
!---------
    nn   = 0
    VARIABLESC: do L=1,size(MCFIO%VarDims)

       RANKC: if (MCFIO%VarDims(L)==2) then
          nn       = nn + 1
          MyGlobal = mCFIO%Krank(nn) == MCFIO%MYPE

             if( MyGlobal ) then
                call MPI_Wait(MCFIO%reqs(nn)%s_rqst, MPI_STATUS_IGNORE, STATUS)
                VERIFY_(STATUS)
                deallocate( MCFIO%reqs(nn)%Trans_Array, stat=STATUS)
                VERIFY_(STATUS)
                nullify( MCFIO%reqs(nn)%Trans_Array )
             endif

       elseif (MCFIO%VarDims(L)==3) then

          LEVELSC: do k=1,MCFIO%lm
             nn       = nn + 1
             MyGlobal = MCFIO%Krank(nn) == MCFIO%MYPE

             if( MyGlobal ) then
                call MPI_Wait(MCFIO%reqs(nn)%s_rqst, MPI_STATUS_IGNORE, STATUS)
                VERIFY_(STATUS)
                deallocate( MCFIO%reqs(nn)%Trans_Array, stat=STATUS)
                VERIFY_(STATUS)
                nullify( MCFIO%reqs(nn)%Trans_Array )
             endif

          end do LEVELSC
       endif RANKC

    end do VARIABLESC

  ! if(AmRoot) then
  !    write(6,'(1X,"Cleaned: ",i6," Slices (",i3," Nodes, ",i2," CoresPerNode) to File:  ",a)') &
  !          size(MCFIO%reqs),mCFIO%partsize/mCFIO%numcores,mCFIO%numcores,trim(mCFIO%fName)
  ! endif
    if (.not.mCFIO%async) then

       if (any(mCFIO%myPE==mCFIO%Krank)) then
          deallocate(Gptr3Out, stat=STATUS)
          VERIFY_(STATUS)
       end if

    end if

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_CFIOWriteBundleWrite

!===========================================================================

!BOP

! !IROUTINE: MAPL_CFIOWrite --- Writing Methods
! !IIROUTINE: MAPL_CFIOWriteBundle --- Writes an ESMF Bundle 

! !INTERFACE:
!
  subroutine MAPL_CFIOWriteBundle( MCFIO, CLOCK, Bundle, &
                                   VERBOSE, NBITS, RC    )
!
! !ARGUMENTS:
!
    type(MAPL_CFIO  ),                 intent(INOUT) :: MCFIO
    type(ESMF_CLOCK),       optional,  intent(INOUT) :: CLOCK
    type(ESMF_FIELDBUNDLE), optional,  intent(INout) :: BUNDLE
    logical,                optional,  intent(IN   ) :: VERBOSE
    integer,                optional,  intent(IN   ) :: NBITS
    integer,                optional,  intent(  OUT) :: RC
!
#ifdef ___PROTEX___

 !DESCRIPTION:  

       Writes an ESMF Bundle to a File. Only the MAPL\_CFIO object is
       a required argument as pointers to the actual data to be
       written is recorded in it during creation.

       {\tt CLOCK, BUNDLE} can be used to override the choice
       made at creation, but this is of dubious value, particularly
       for {\tt BUNDLE} since it must be excatly conformant with the
       creation {\tt BUNDLE}. {\tt NBITS} if the number of bits of 
       the mantissa to retain. This is used to write files with degraded
       precision, which can then be compressed with standard utilities.
       The default is no degradation of precision.

       {\bf A note about compression.} NetCDF-4, HDF-4 and HDF-5 all
       support transparent internal GZIP compression of the data being
       written. However, very little is gained by compressing float
       point fields from earth system models. Compression yields can
       be greatly increased by setting to zero bits in the mantissa of float
       numbers. On average 50\% compression can be achieved, while
       preserving a meaningful accuracy in the fields. Unlike
       classical CF compression by means of {\tt scale\_factor} and
       {\tt add\_offset} attributes, internal GZIP compression
       requires no special handling by the users of the data. In fact,
       they do not even need to know that the data is compressed! At this
       point, MAPL\_CFIO does not activate this GZIP compression
       feature in the files being written, but the resulting precision 
       degredaded files can be compressed offline with the HDF-4 
       {\tt hrepack} utility.

#endif

!EOP

    character(len=*), parameter:: Iam="MAPL_CFIOWriteBundle"
    integer                    :: status

    ASSERT_(present(CLOCK))
    ASSERT_(present(BUNDLE))

! for backward compatibility
!---------------------------

    if(present(NBITS)) then
       mCFIO%Nbits = Nbits
    end if

! DSK fName is set in History during runtime, but the G5 tutorial does not, so we must set it here
    if (IACHAR(MCFIO%fName(1:1)) == 0) then
       call MAPL_CFIOSet( MCFIO, fName=MCFIO%Name, RC=status )
       VERIFY_(STATUS)
    endif

    call MAPL_CFIOCreateWrite    ( MCFIO,         RC=status)
    VERIFY_(STATUS)

    call MAPL_CFIOWriteBundlePost( MCFIO,         RC=status)
    VERIFY_(STATUS)

    call MAPL_CFIOWriteBundleWait( MCFIO, CLOCK,  RC=status)
    VERIFY_(STATUS)

    call MAPL_CFIOWriteBundleWrite( MCFIO, CLOCK, RC=status)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_CFIOWriteBundle


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP

! !IIROUTINE: MAPL_CFIOWriteState --- Writes an ESMF State

! !INTERFACE:
!
  subroutine MAPL_CFIOWriteState ( MCFIO, CLOCK, State, &
                                   VERBOSE, NBITS, RC   )
!
! !ARGUMENTS:
!
    type(MAPL_CFIO),             intent(INOUT) :: MCFIO
    type(ESMF_State),            intent(INout) :: STATE
    type(ESMF_CLOCK),            intent(INOUT) :: CLOCK
    integer, optional,           intent(  OUT) :: RC
    logical, optional,           intent(  IN)  :: VERBOSE
    integer, optional,           intent(  IN)  :: NBITS
!
#ifdef ___PROTEX___

    !DESCRIPTION:

     Serializes an ESMF state into a Bundle and writes it to a file.
     Only the MAPL\_CFIO object is a required argument as pointers to
     the actual data to be written is recorded in it during creation.

     {\tt CLOCK, BUNDLE} can be used to override the choice
     made at creation, but this is of dubious value, particularly
     for {\tt BUNDLE} since it must be excatly conformant with the
     creation {\tt BUNDLE}. {\tt NBITS} if the number of bits of 
     the mantissa to retain. This is used to write files with degraded
     precision, which can then be compressed with standard utilities.
     The default is no degradation of precision.

     {\bf A note about compression.} NetCDF-4, HDF-4 and HDF-5 all
     support transparent internal GZIP compression of the data being
     written. However, very little is gained by compressing float
     point fields from earth system models. Compression yields can
     be greatly increased by setting to zero bits in the mantissa of float
     numbers. On average 50\% compression can be achieved, while
     preserving a meaningful accuracy in the fields. Unlike
     classical CF compression by means of {\tt scale\_factor} and
     {\tt add\_offset} attributes, internal GZIP compression
     requires no special handling by the users of the data. In fact,
     they do not even need to know that the data is compressed! At this
     point, MAPL\_CFIO does not activate this GZIP compression
     feature in the files being written, but the resulting precision 
     degredaded files can be compressed offline with the HDF-4 
     {\tt hrepack} utility.

#endif

!EOP

    character(len=*), parameter  :: Iam="MAPL_CFIOWriteState"
    integer                      :: STATUS

! Locals

    type(ESMF_FieldBundle) :: tBUNDLE

! Get the appropriate bundle
!---------------------------

!!ALT    if(present(STATE)) then
       tBUNDLE = ESMF_FieldBundleCreate ( name=Iam, rc=STATUS )
       VERIFY_(STATUS)
       call ESMFL_BundleAddState ( tBUNDLE, STATE, rc=STATUS, VALIDATE=.true. )
       VERIFY_(STATUS)
!!ALT    else
!!ALT       tBUNDLE = MCFIO%BUNDLE
!!ALT    end if

!   Write the Bundle
!   ----------------

    call MAPL_CFIOWriteBundle ( MCFIO, CLOCK=CLOCK, BUNDLE=tBUNDLE, &
                                VERBOSE=VERBOSE, NBITS=NBITS, RC=STATUS   )
    VERIFY_(STATUS)

!!ALT    if(present(STATE)) then
       call ESMF_FieldBundleDestroy ( tBUNDLE, rc=STATUS )
       VERIFY_(STATUS)
!!ALT    endif

!   All done
!   --------

    RETURN_(ESMF_SUCCESS)

 end subroutine MAPL_CFIOWriteState

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IROUTINE: MAPL_CFIORead --- Reading Methods
! !IIROUTINE: MAPL_CFIOReadBundle --- Reads an ESMF Bundle

! !INTERFACE:
!
  subroutine MAPL_CFIOReadBundle ( FILETMPL, TIME, BUNDLE, NOREAD, RC, &
                                   VERBOSE, FORCE_REGRID, ONLY_VARS, ONLY_LEVS, &
                                   TIME_IS_CYCLIC, TIME_INTERP, conservative, &
                                   voting, ignoreCase, doParallel, EXPID )
!
! !ARGUMENTS:
!
    character(len=*),            intent(IN   ) :: FILETMPL
    type(ESMF_TIME),             intent(INout) :: TIME
    type(ESMF_FIELDBUNDLE),           intent(INOUT) :: BUNDLE
    logical, optional,           intent(IN   ) :: NOREAD
    integer, optional,           intent(  OUT) :: RC
    logical, optional,           intent(IN)    :: VERBOSE
    logical, optional,           intent(IN)    :: FORCE_REGRID 
    logical, optional,           intent(IN)    :: TIME_IS_CYCLIC
    logical, optional,           intent(IN)    :: TIME_INTERP
    logical, optional,           intent(IN)    :: conservative
    logical, optional,           intent(IN)    :: voting
    logical, optional,           intent(IN)    :: doParallel
    character(len=*), optional,  intent(IN)    :: ONLY_VARS 
    real,    optional,           intent(IN)    :: ONLY_LEVS(:)
    character(len=*), optional,  intent(IN)    :: EXPID
    logical, optional,           intent(IN)    :: ignoreCase
!
#ifdef ___PROTEX___
    !DESCRIPTION: 

     Reads an ESMF Bundle from a file on a given time. The file is
     open, read from, and closed on exit. The arguments are:
\bd
     \item[FILETMPL] A GrADS-style file name template. In its simplest
     form is the full path name for the file to be read. However, it
     can contain the following tokens which will be expanded from
     the current time in {\em TIME}:
   \bd
        \item[\%y4] 4 digits for year
        \item[\%m2] 2 digits for month, to expand to 01, 02, .., 12
        \item[\%m3] 3 digits for month, to expand to jan, feb, mar, ..., dec
        \item[\%d2] 2 digits for day
        \item[\%h2] 2 digits for hour
        \item[\%n2] 2 digits for minutes
   \ed
    Example: if FILETMPL = ``forecast.\%y4-\%m2-\%d2\_%h2z.nc4'', and the clock
    says it is 18Z on 05 February 2007, the template will expand in the
    following file name: ``forecast.2007-02-05\_18Z.nc4''
%
    \item[TIME] The ESMF time to read from the file
%
    \item[BUNDLE] An ESMF Bundle to read the data in. When the Bundle is empty
    one field is added for each variable present in the input file, and the
    necessary memory allocated according to the ESMF grid present in the Bundle.
%
    \item[{[NOREAD]}] If .TRUE., no data is actually read into the Bundle. This is
    useful to define a Bundle with the same variables as presented in the
    file, which in turn can be used to created a MAPL\_CFIO object for
    writing.
%
    \item[{[RC]}] Error return code; set to ESMF\_SUCCESS if all is well.    
%
    \item[{[VERBOSE]}] If .TRUE., prints progress messages to STDOUT; useful
     for debugging.
%
    \item[{[FORCE\_REGRID]}] Obsolete; kept for backward compatibility but
    has no effect.
%
    \item[{[TIME\_IS\_CYCLIC]}]  If .TRUE. it says that the input file is periodic
    in time. Useful for reading climatological files. For example, if the
    input file has 12 monthly means from January to December of 2001, setting
    this option to .TRUE. allows one to read this data for any other year. See 
    note below regarding issues with reading monthly mean data.
%
    \item[{[TIME\_INTERP]}] If .TRUE., the input file does not have to coincide with the
    actual times on file. In such cases, the data for the bracketing times are
    read and the data is properly interpolated in time. The input time, though,
    need to be within the range of times present on file 
   (unless {\tt TIME\_IS\_CYCLIC} is specified).
%
    \item[{[ONLY\_VARS]}] A list of comma separated vafriables to be read from the
    file. By default, all variables are read from the file. This option allows
    one to read a subset of vafriables. Example: ONLY\_VARS=``u,v,ps''.
%
\ed

    {\bf A note about storing monthly climatological data.} As per the CF
    conventions, month is not a well defined unit of time, as the time
    step is not constant throughout the year. When storing 12 months
    of climatological data one way around it is to use an average
    number of hours: use 732 or 730 hours depending on whether the year
    recorded in the file is a leap-year or not.

     !DESIGN ISSUES:

     The input argument {\tt TIME} should be replaced with {\tt CLOCK}
     for consistency with the rest of the API. One should also provide
     an interface involving the MAPL CFIO object.

#endif

!EOP
!--------------------------------------------------------------------------------
    character(len=*), parameter  :: Iam="MAPL_CFIOReadBundle"
    integer                      :: STATUS

! Locals


    type(ESMF_CFIO)              :: CFIO
    type(ESMF_CFIOGrid), pointer :: CFIOGRID
    type(ESMF_GRID)              :: ESMFGRID
    type(ESMF_FIELD)             :: FIELD
    type(ESMF_ARRAY)             :: ARRAY
    type(ESMF_VM)                :: VM

    type(ESMF_CFIOVarInfo), pointer :: VARS(:)

    type(MAPL_HORZTRANSFORM)          :: Trans
    integer                      :: IM,  JM,  LM, ISTAR, JSTAR
    integer                      :: IM0, JM0
    integer                      :: L1, L, K
    integer                      :: NumVars, nVars
    integer                      :: counts(5)
    integer                      :: dims(3)
    integer                      :: arrayRank

    logical                      :: IamRoot, twoD
    logical                      :: amOnFirstNode

    real, pointer                ::  PTR2      (:,:),  PTR3      (:,:,:)
    real, pointer                :: GPTR2bundle(:,:), GPTR3bundle(:,:,:)
    real, pointer                :: GPTR2file  (:,:), GPTR3file  (:,:,:)

    character(len=ESMF_MAXSTR)   :: NAME
    character(len=ESMF_MAXSTR)   :: DATE
    character(len=ESMF_MAXSTR)   :: BundleVARNAME
    character(len=ESMF_MAXSTR)   :: CFIOVARNAME
    character(len=ESMF_MAXSTR)   :: LONG_NAME
    character(len=ESMF_MAXSTR)   :: UNITS

    real, pointer :: LONSfile(:),   LATSfile(:)
    real, pointer :: LONSbundle(:) => NULL()
    real, pointer :: LATSbundle(:) => NULL()

    !(stassi,14feb2012)--character(len=ESMF_MAXSTR) :: FILENAME
    !character(len=256) :: FILENAME
    character(len=1024) :: FILENAME
    integer :: nymd, nhms
    logical :: timeInterp=.false., VERB = .false., change_resolution, do_xshift, single_point, fcubed
    integer, allocatable    :: gridToFieldMap(:)
    integer                 :: gridRank
    integer                 :: comm
    logical                 :: found
    character(len=ESMF_MAXSTR) :: gridnamef, gridname, tileFile
    logical :: geosGridNames = .false.
    logical :: RegridCnv
    logical :: Voting_, doingMasking
    logical :: runParallel
    integer :: order
    logical :: ignoreCase_
    integer, pointer        :: Krank(:) => null()
    logical                 :: myGlobal
    integer                 :: nn, CoresPerNode, myPet, nPet, numNodes
    logical :: selectedLevels
    real, pointer :: levsfile(:) => null()
    integer :: LM_FILE
    integer :: LL,klev
    integer, allocatable :: LEVIDX(:)
    type(ESMF_CFIOGrid)  :: varsGrid
    real, parameter      :: eps = 1.0e-4 ! tolerance to find "selected" levels
    logical :: kreverse
    integer :: i1w,inw,j1w,jnw
    integer :: xy

!                              ---
    
    if ( present(VERBOSE) )     VERB = VERBOSE
    if ( present(TIME_INTERP) ) timeInterp = TIME_INTERP
    if (present(conservative) ) then
       RegridCnv = conservative
    else
       RegridCnv = .false.
    end if
    if ( present(Voting) ) then
         Voting_ = Voting
    else
         Voting_ = .false.
    endif
    if ( present(ignoreCase) ) then
         ignoreCase_ = ignoreCase
    else
         ignoreCase_ = .false.
    end if

    if ( present(doParallel) ) then
       runParallel = doParallel
    else
       runParallel = .true.
    end if
    if (present(ONLY_LEVS)) then
       selectedLevels = .true.
    else
       selectedLevels = .false.
    end if
    ! by default kreverse is false
    kreverse = .false.

! Create a CFIO object named after the bundle
!--------------------------------------------
    call ESMF_FieldBundleGet(Bundle, name=NAME, RC=STATUS)
    VERIFY_(STATUS)
    cfio =  ESMF_CFIOCreate (cfioObjName=trim(Name), RC=STATUS)
    VERIFY_(STATUS)

! Transform ESMF time to string for use in CFIO
!----------------------------------------------
    call ESMF_TimeGet(TIME, timeString=DATE, RC=STATUS)
    VERIFY_(STATUS)

    call strToInt(DATE, nymd, nhms)
    call ESMF_CFIOstrTemplate ( filename, filetmpl, 'GRADS', &
                                xid=EXPID, nymd=nymd, nhms=nhms, stat=status )
    VERIFY_(STATUS)
    !call WRITE_PARALLEL("CFIO: Reading " // trim(filename))
    if (mapl_am_i_root()) write(*,*)"CFIO: Reading ",trim(filename)," at ",nymd," ",nhms

! Set its filename and open it for reading
!-----------------------------------------
    call ESMF_CFIOSet(CFIO, fName=trim(fileName), RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_CFIOFileOpen  (CFIO, FMODE=1, cyclic=TIME_IS_CYCLIC, RC=STATUS)
    VERIFY_(STATUS)

! Get info from the bundle
!-------------------------
    call ESMF_VMGetCurrent(VM, RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_FieldBundleGet     (Bundle, FieldCount=NUMVARS, RC=STATUS)
    VERIFY_(STATUS)

    IamRoot = MAPL_AM_I_ROOT(VM)
    call ESMF_VMGet(VM, mpiCommunicator=comm, localPet=myPET, PETcount=nPet, rc=status)
    VERIFY_(STATUS)
    amOnFirstNode = MAPL_ShmemAmOnFirstNode(comm=comm, RC=status)
    VERIFY_(STATUS)
    CoresPerNode = MAPL_CoresPerNodeGet(comm,rc=status)
    VERIFY_(STATUS)

! Get info from the CFIO object
!------------------------------
    call ESMF_CFIOGet       (CFIO,     grid=CFIOGRID,                     RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_CFIOGridGet   (CFIOGRID, IM=IM, JM=JM, KM=LM,               RC=STATUS)
    VERIFY_(STATUS)
    if(selectedLevels) then
       LM_FILE = LM
       LM = size(ONLY_LEVS)
       ASSERT_(LM <= LM_FILE)
    end if

    call ESMF_CFIOGridGet    (CFIOGRID, LON=LONSFILE, LAT=LATSFILE, RC=STATUS)
    VERIFY_(STATUS)
    deallocate(CFIOGRID)

    call ESMF_CFIOGet (CFIO,varObjs=VARS, nVars=nVars, RC=STATUS)
    VERIFY_(STATUS)


! If the bundle is empty, read entire varlist from file
!------------------------------------------------------

    if(NUMVARS==0) then

       call ESMF_FieldBundleGet     (Bundle,   Grid=ESMFGRID, RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GridGet(ESMFGRID, globalCellCountPerDim=COUNTS, &
            localCellCountPerDim=DIMS, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_GridGet(ESMFGRID, name=gridname, rc=rc)
       VERIFY_(STATUS)

       ! Assert compatibility of file and bundle
       !----------------------------------------
       ASSERT_( LM==0 .or. counts(3) == 0 .or. LM==counts(3) .or. LM==(counts(3)+1) )

       ! Get lat/lons of input bundle
       ! ----------------------------
       call GridGetLatLons_ ( ESMFGRID, LONSbundle, LATSbundle, rc=status )
       VERIFY_(STATUS)

       NUMVARS = nVars

       L1 = 0
       do L=1,NUMVARS

          call ESMF_CFIOVarInfoGet(VARS(L),vname=CFIOVARNAME, vtitle=LONG_NAME, vunits=UNITS, twoDimVar=twoD, grid=varsGrid, RC=STATUS)   
          VERIFY_(STATUS)

          if ( present(ONLY_VARS) ) then
               if ( index(','//trim(ONLY_VARS)  //',', &
                          ','//trim(CFIOVARNAME)//',') < 1 ) cycle 
          endif

          L1 = L1 + 1

          BundleVarName = CFIOVARNAME
          if(twoD) then
            allocate(PTR2(DIMS(1),DIMS(2)),stat=STATUS)
            VERIFY_(STATUS)
            PTR2  = 0.0

            call ESMF_GridGet(ESMFGRID, dimCount=gridRank, rc=status)
            VERIFY_(STATUS)
            allocate(gridToFieldMap(gridRank), stat=status)
            VERIFY_(STATUS)
            if(gridRank == 2) then
               gridToFieldMap(1) = 1
               gridToFieldMap(2) = 2
            else if (gridRank == 3) then
               gridToFieldMap(1) = 1
               gridToFieldMap(2) = 2
               gridToFieldMap(3) = 0
            else
               RETURN_(ESMF_FAILURE)
            end if

            FIELD = ESMF_FieldCreate(grid=ESMFGRID, &
                            datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
                            farrayPtr=PTR2, gridToFieldMap=gridToFieldMap, &
                            name=BundleVARNAME, RC=STATUS)
            VERIFY_(STATUS)

            deallocate(gridToFieldMap)

!ALT: for now we add only HorzOnly (no tiles)
            call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='UNITS', VALUE=UNITS, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=MAPL_DimsHorzOnly, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='VLOCATION', &
                                        VALUE=MAPL_VLocationNone, RC=STATUS)
            VERIFY_(STATUS) 

          else
            ! 3-d case
             call ESMF_CFIOGridGet (varsGrid, lev=levsfile, rc=status)
             VERIFY_(STATUS) 
             if (levsfile(1) > levsfile(lm)) kreverse = .true.

             if (selectedLevels) then
                if (.not. allocated(levidx)) then
                   allocate(levidx(LM), stat=status)
                   VERIFY_(STATUS) 
                   ! build level index
                   DO K = 1, LM
                      found = .false.
                      DO LL = 1, LM_FILE
                         if (abs(LEVSFILE(LL) - ONLY_LEVS(K)) < eps) then
                            LEVIDX(K) = LL
                            found = .true.
                            exit
                         end if
                      END DO
                      ASSERT_(found)
                   END DO

                end if
             end if
            deallocate(levsfile)

            if (lm == counts(3)) then 
               allocate(PTR3(DIMS(1),DIMS(2),LM),stat=STATUS)
               VERIFY_(STATUS)
            else if (lm == (counts(3)+1)) then
               allocate(PTR3(DIMS(1),DIMS(2),0:LM-1),stat=STATUS)
               VERIFY_(STATUS)
            end if
            PTR3  = 0.0
            FIELD = ESMF_FieldCreate(grid=ESMFGRID, &
                            datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
                            farrayPtr=PTR3, name=BundleVARNAME, RC=STATUS)
            VERIFY_(STATUS)
!ALT: for now we add only HorzVert (no tiles)
            call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='UNITS', VALUE=UNITS, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=MAPL_DimsHorzVert, RC=STATUS)
            VERIFY_(STATUS)
            if (lm == counts(3)) then
               call ESMF_AttributeSet(FIELD, NAME='VLOCATION', &
                                           VALUE=MAPL_VLocationCenter, RC=STATUS)
            else if (lm == (counts(3)+1)) then
               call ESMF_AttributeSet(FIELD, NAME='VLOCATION', &
                                           VALUE=MAPL_VLocationEdge, RC=STATUS)
            end if

            VERIFY_(STATUS)
          end if
          call MAPL_FieldBundleAdd(BUNDLE, FIELD, RC=STATUS)
          VERIFY_(STATUS)

       end do
       NUMVARS = L1  ! could be less than on file if user chooses to

    else
       
       do L=1,NumVars
          call ESMF_FieldBundleGet (BUNDLE, L, FIELD,                     RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_FieldGet(FIELD,NAME=BundleVarName, RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_FieldGet(FIELD,   Grid=ESMFGRID, RC=STATUS)
          VERIFY_(STATUS)
          call MAPL_GridGet(ESMFGRID, globalCellCountPerDim=COUNTS, RC=STATUS)
          VERIFY_(STATUS)
          call ESMF_GridGet(ESMFGRID, name=gridname, rc=rc)
          VERIFY_(STATUS)
          ! Assert compatibility of file and bundle
          !----------------------------------------
          ASSERT_( LM==0 .or. counts(3) == 0 .or. LM==counts(3) .or. lm == (counts(3)+1) )

          ! Get lat/lons of input bundle
          ! ----------------------------
          call GridGetLatLons_ ( ESMFGRID, LONSbundle, LATSbundle, rc=status )
          VERIFY_(STATUS)

          found = .false.
          do K=1,size(VARS)
             call ESMF_CFIOVarInfoGet(VARS(K),vname=CFIOVARNAME,          RC=STATUS)
             VERIFY_(STATUS)
             if (ignoreCase_) then
                BUNDLEVARNAME = ESMF_UtilStringUpperCase(BUNDLEVARNAME,rc=status)
                CFIOVARNAME = ESMF_UtilStringUpperCase(CFIOVARNAME,rc=status)
             end if
             if(trim(BUNDLEVARNAME)==trim(CFIOVARNAME)) then
               found = .true.
               exit
             end if
          end do
          ASSERT_(found)
          call ESMF_CFIOVarInfoGet(VARS(L), twoDimVar=twoD, grid=varsGrid, RC=STATUS)   
          VERIFY_(STATUS)
          if (.not. twoD) then
             call ESMF_CFIOGridGet (varsGrid, lev=levsfile, rc=status)
             VERIFY_(STATUS) 
             if (levsfile(1) > levsfile(lm)) kreverse = .true.
          end if
          if (selectedLevels) then
             if (.not. twoD) then
                ! 3-d case
                if (.not. allocated(levidx)) then
                   allocate(levidx(LM), stat=status)
                   VERIFY_(STATUS) 
                   ! build level index
                   DO K = 1, LM
                      found = .false.
                      DO LL = 1, LM_FILE
                         if (abs(LEVSFILE(LL) - ONLY_LEVS(K)) < eps) then
                            LEVIDX(K) = LL
                            found = .true.
                            exit
                         end if
                      END DO
                      ASSERT_(found)
                   END DO

                end if
             end if
          end if
          if (.not. twoD) deallocate(levsfile)
       end do
    end if

    if(present(NOREAD)) then
       if(NOREAD) goto 10
    end if

!   Do we have to run a transform?
!   ------------------------------
    IM0 = counts(1)
    JM0 = counts(2)

    if (IM /= IM0 .or. JM /= JM0)  then
        change_resolution = .true.
    else                              
        change_resolution = .false.
    end if

! 180 Degree Shifting and Cubed Sphere
! ------------------------------------
!   
!   In the earlier revisions of this subroutine there was an implicit assumption
!   of the input data being on the lat-lon grid. Since there were two
!   possibilities: Longitudinal origin at dateline, or at the Greewitch meridian,
!   the code used to perform Longitudinal shifting, if needed, so that the 
!   output is "properly" oriented at dateline center. 
!
!   Out current strategy is to correct the input (from the file), if needed.
!   We first check if the input is on the Cubed-Sphere grid. 
!   In this case no shifting is done. Otherwise we still assume that the
!   input is on a lat-lon grid and if shifting is needed,
!   it will be done prior to the optional MAPL_HorzTransformRun regridding.


    if ( JM == 6*IM )  then
        fcubed = .true.
    else                              
        fcubed = .false.
    end if

    do_xshift = .FALSE. ! Initialize: do not shift

    if ( IM0==1 .AND. JM0==1 ) then
       single_point = .TRUE.       ! running SCM with single point grid
       change_resolution = .FALSE. ! does not make sense in SCM mode
    else
       single_point = .FALSE. ! Normal case, not SCM
       ! never shift if cubed
       if (.not.fcubed) do_xshift = abs(LONSfile(1)+180._8) .GT. abs(LONSfile(2)-LONSfile(1))
    end if

    if (change_resolution .and. RegridCnv) then

       runParallel = .false. ! override input, conservative regridding now done distributed

       call ESMF_GRID_INTERIOR(ESMFGRID,I1w,INw,J1w,JNw)
       call MAPL_GenGridName(im, jm, LONSfile, LATSfile, gridname=gridnamef, geos_style=geosGridNames)
       if (.not. geosGridNames) call MAPL_GeosNameNew(gridname)

       tileFile=trim(adjustl(gridnamef)) // '_' //trim(adjustl(gridname))  // '.bin'
       call MAPL_HorzTransformCreate (Trans, tileFile, gridnamef, gridname, RootOnly=.false., &
                vm=vm, i1=i1w, in=inw, j1=j1w, jn=jnw, rc=rc)

       if (Voting_) then
          call MAPL_HorzTransformSet(Trans, order=MAPL_HorzTransOrderSample, rc=rc)
       endif

    else if ( change_resolution .and. (.not.RegridCnv)) then
       if (amOnFirstNode .or. runParallel) then
          xy = MAPL_GenXYOffset(lon=LONSfile, lat=LATSfile)
          call MAPL_HorzTransformCreate (Trans, im, jm, im0, jm0, &
               xyoffset=xy, rc=STATUS)
          VERIFY_(STATUS)
       end if

    end if
    call MAPL_SyncSharedMemory(rc=status)
    VERIFY_(STATUS)

! Allocate space for global arrays. If non-conservative perform
! parallel transform distributed across levels
! If conservative do not parallelize over levels this is done
! distributed already so this parallel strategy will not work
!------------------------------------------------------------

    if (RegridCnv .and. change_resolution) then

       call MAPL_AllocNodeArray(Gptr2file,(/im,jm/),rc=STATUS)
       if(STATUS==MAPL_NoShm) allocate(Gptr2file(im,jm),stat=status)
       VERIFY_(STATUS)
       call MAPL_AllocNodeArray(Gptr3file,(/im,jm,1/),rc=STATUS)
       if(STATUS==MAPL_NoShm) allocate(Gptr3file(im,jm,1),stat=status)
       VERIFY_(STATUS)
       Allocate(Gptr2bundle(inw-i1w+1,jnw-j1w+1),stat=STATUS)
       VERIFY_(STATUS)
       allocate(Gptr3bundle(0,0,0), stat=STATUS)
       VERIFY_(STATUS)
       if (LM > 0) then
          allocate(krank(LM),stat=status)
       else
          allocate(krank(1) ,stat=status)
       end if
       krank = 0
   
    else

       IM0 = counts(1)
       JM0 = counts(2)

       CoresPerNode = MAPL_CoresPerNodeGet(comm,rc=status)
       VERIFY_(STATUS)
       if (LM > 0) then
          allocate(krank(LM),stat=status)
       else
          allocate(krank(1) ,stat=status)
       end if

       VERIFY_(STATUS)
       if (runParallel .and. (LM > 0) ) then
          numNodes = size(MAPL_NodeRankList)
          call MAPL_RoundRobinPEList(krank,numNodes,rc=status)
          VERIFY_(STATUS)
       else
          krank = 0
       end if
       nn=count(krank==myPet)

       if (nn > 0) then

          allocate(Gptr2bundle(IM0,JM0   ), stat=STATUS)
          VERIFY_(STATUS)
          allocate(Gptr3bundle(IM0,JM0,nn), stat=STATUS)
          VERIFY_(STATUS)
          allocate(Gptr2file  (IM ,JM    ), stat=STATUS)
          VERIFY_(STATUS)
          allocate(Gptr3file  (IM ,JM , 1), stat=STATUS)
          VERIFY_(STATUS)

       else

          allocate(Gptr2bundle(0,0   ), stat=STATUS)
          VERIFY_(STATUS)
          allocate(Gptr3bundle(0,0,0), stat=STATUS)
          VERIFY_(STATUS)
          allocate(Gptr2file  (0,0    ), stat=STATUS)
          VERIFY_(STATUS)
          allocate(Gptr3file  (0,0,0), stat=STATUS)
          VERIFY_(STATUS)

       end if

    end if

!   Special handling for single column case
!   Pick out index into file grid for lats and lons of scm grid - 
!   Assume that scm grid counts lon from -180 to 180 and lat from -90 to 90
    if(single_point) then
      if(LONSfile(1).lt.0.) then        !  assume lons on file go from -180 to 180
       ISTAR = 1 + (LONSbundle(1)+180.)/( 360./ IM )
      else                              !  lons on file go from 0 to 360
       if(LONSbundle(1).lt.0.) then
       ISTAR = 1 + (LONSbundle(1)+360.)/( 360./IM )
       else
       ISTAR = 1 + LONSbundle(1)/( 360./IM )
       endif
      endif
!  assume lats on file go from -90 to 90
       JSTAR = 1 + (LATSbundle(1)+90.)/( 180. / (JM-1) )
    endif

! Read each variable
!-------------------
    do L=1,NumVars

       call ESMF_FieldBundleGet (BUNDLE, L, FIELD,                       RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_FieldGet       (FIELD, NAME=BundleVarName, array=ARRAY, RC=STATUS)
       VERIFY_(STATUS)
       
       if (ignoreCase_) call getVarNameIgnoreCase(BundleVarName,vars,RC=status)

       call ESMF_FieldGet(FIELD,   Grid=ESMFGRID, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_ArrayGet       (array, rank=arrayRank,                  RC=STATUS)
 
       VERIFY_(STATUS)

       if ( VERB .and. IamRoot ) &
            print *, Iam // ': Reading '//trim(BundleVARNAME)// &
                            ' at ' // trim(date)
       select case (arrayRank)

       case (2)

          call ESMF_ArrayGet(Array, localDE=0, farrayPtr=PTR2, RC=STATUS)
          VERIFY_(STATUS)
          
          ! read the data on root
          if (IamRoot) then
             if ( timeInterp ) then
                call ESMF_CFIOVarReadT(CFIO, trim(BundleVARNAME), GPTR2file, &
                                       timeString=DATE, RC=STATUS)
             else
                call ESMF_CFIOVarRead (CFIO, trim(BundleVARNAME), GPTR2file, &
                                       timeString=DATE, RC=STATUS)
             endif
             VERIFY_(STATUS)
             if ( do_xshift ) then
                 if ( VERB ) print *, Iam // &
                      ': shifting input longitudes by 180 degrees'
                 call shift180Lon2D_ ( Gptr2file, im, jm )
             end if
          end if
 
          ! transform and scatter
          if (change_resolution) then  
              if (RegridCnv) then 
                 call MAPL_SyncSharedMemory(rc=status)
                 VERIFY_(STATUS)
                 call MAPL_BcastShared(VM, Data=Gptr2file, N=im*jm, Root=0, RootOnly=.false., rc=status)
                 VERIFY_(STATUS)
                 call MAPL_SyncSharedMemory(rc=STATUS)
                 VERIFY_(STATUS)
                 call MAPL_HorzTransformRun(Trans, Gptr2file, Gptr2bundle, MAPL_undef, rc=STATUS )
                 VERIFY_(STATUS)
                 call MAPL_SyncSharedMemory(rc=status)
                 VERIFY_(STATUS)
                 ptr2 = Gptr2bundle
                 call MAPL_SyncSharedMemory(rc=STATUS)
                 VERIFY_(STATUS)
              else
                 if (IamRoot) then
                    call MAPL_HorzTransformRun(Trans, Gptr2file, Gptr2bundle, MAPL_undef, rc=STATUS )
                    VERIFY_(STATUS)
                 end if
                 call ArrayScatter(PTR2, GPTR2bundle, ESMFGRID, RC=STATUS)
                 VERIFY_(STATUS)
              end if
          else if ( single_point ) then
             Gptr2bundle(1,1) = Gptr2file(ISTAR,JSTAR)
             ptr2(1,1) = GPTR2bundle(1,1) ! single point SCM case
          else
             if (IamRoot) Gptr2bundle = Gptr2file
             call ArrayScatter(PTR2, GPTR2bundle, ESMFGRID, RC=STATUS)
             VERIFY_(STATUS)
          end if ! change resolution

       case(3)

          nn = 0


          call ESMF_FieldGet(Field, localDE=0, farrayPtr=PTR3, RC=STATUS)
          VERIFY_(STATUS)

          nn=0

          do k = 1, LM

             MyGlobal = Krank(k) == myPet

             call MAPL_SyncSharedMemory(rc=status)
             VERIFY_(STATUS)
             if (MyGlobal) then
                nn=nn+1
                if (selectedLevels) then
                   ASSERT_(allocated(levidx))
                   klev = levidx(k)
                else
                   klev = k
                end if
                if (kreverse) klev = lm - k + 1
                if ( timeInterp ) then
                   call ESMF_CFIOVarReadT(CFIO, trim(BundleVARNAME), GPTR3file, &
                        kbeg=klev, kount=1, timeString=DATE, RC=STATUS)
                else
                   call ESMF_CFIOVarRead (CFIO, trim(BundleVARNAME), GPTR3file, &
                        kbeg=klev, kount=1, timeString=DATE, RC=STATUS)
                end if
                VERIFY_(STATUS)
                GPTR2file = GPTR3file(:,:,1)
                if ( do_xshift ) then
                   call shift180Lon2D_ ( Gptr2file, im, jm )
                end if
             end if

             if (change_resolution) then 
                if (RegridCnv) then
                   call MAPL_SyncSharedMemory(rc=status)
                   VERIFY_(STATUS)
                   call MAPL_BcastShared(VM, Data=Gptr2file, N=im*jm, Root=0, RootOnly=.false., rc=status)
                   VERIFY_(STATUS)
                   call MAPL_SyncSharedMemory(rc=STATUS)
                   VERIFY_(STATUS)
                   call MAPL_HorzTransformRun(Trans, Gptr2file, Gptr2bundle, MAPL_undef, rc=STATUS )
                   VERIFY_(STATUS)
                   call MAPL_SyncSharedMemory(rc=status)
                   VERIFY_(STATUS)
                   L1 = LBOUND(PTR3,3)-1
                   ptr3(:,:,K+L1) = Gptr2bundle
                   call MAPL_SyncSharedMemory(rc=STATUS)
                   VERIFY_(STATUS) 
                 else
                    if (MyGlobal) then
                       call MAPL_HorzTransformRun(Trans, Gptr2file, &
                                                    Gptr2bundle, MAPL_undef, rc=STATUS)
                       VERIFY_(STATUS)
                       Gptr3bundle(:,:,nn)=Gptr2bundle
                    end if
                end if
             else if ( single_point ) then
                  Gptr3bundle(:,:,nn) = Gptr2file(ISTAR,JSTAR)
             else
                if (MyGlobal) Gptr3bundle(:,:,nn)=Gptr2file
             end if

          end do

          if (single_point) then
             ptr3(1,1,:) = Gptr3bundle(1,1,:)
          else
             if ( (.not.RegridCnv) .and. runParallel) then
                call MAPL_CollectiveScatter3D(esmfgrid,Gptr3bundle(:,:,:nn),ptr3,CoresPerNode=CoresPerNode,rc=status)
                VERIFY_(STATUS)
             else if ( (.not.RegridCnv) .and. (.not.RunParallel) ) then
                do K=1,LM
                   L1 = LBOUND(PTR3,3)-1
                   call ArrayScatter(PTR3(:,:,K+L1), Gptr3bundle(:,:,K), ESMFGRID, RC=STATUS)
                   VERIFY_(STATUS)
                end do
             end if
          end if

       end select

    end do
    deallocate(krank)

    deallocate(GPtr2bundle)
    deallocate(GPtr3bundle)

    if (RegridCnv .and. change_resolution) then
       ! make sure everyone is done before potentially releasing shared memory
       call MAPL_SyncSharedMemory(rc=status)
       VERIFY_(STATUS)
       DEALOC_(GPtr2file  )
       DEALOC_(GPtr3file  )
    else
       deallocate(Gptr2file)
       deallocate(Gptr3file)
    end if

    if (amOnFirstNode .or. runParallel .or. RegridCnv) then
       if ( change_resolution ) then
          call MAPL_HorzTransformDestroy(Trans,rc=STATUS)
          VERIFY_(STATUS)
       end if
    end if

10  continue 
! always do this cleanup

    deallocate(LONSfile,LATSfile)
    deallocate(LONSbundle,LATSbundle)
    deallocate(VARS)

    if (selectedLevels) then
       if (allocated(levidx)) then
          deallocate(levidx)
       end if
    end if

    call ESMF_CFIODestroy(CFIO, rc=status)
    VERIFY_(STATUS)
    
    RETURN_(ESMF_SUCCESS)

CONTAINS

    subroutine shift180Lon2D_ ( c, im, jm )
    integer, intent(in) :: im, jm
    real, intent(inout) :: c(im,jm)
    real :: cj(im)
    integer :: m(4), n(4), imh, j
    imh = nint(im/2.)
    m = (/ 1,      imh, 1+imh,    im   /)
    n = (/ 1,   im-imh, 1+im-imh, im   /)
    do j = 1, jm
       cj(n(1):n(2)) = c(m(3):m(4),j)
       cj(n(3):n(4)) = c(m(1):m(2),j)
       c(:,j) = cj
    end do
    return
    end subroutine shift180Lon2D_

    subroutine getVarNameIgnoreCase(vname,vars,rc)
    character(len=*), intent(inout)  :: vname
    type(ESMF_CFIOVarInfo), pointer, intent(in) :: vars(:)
    integer, optional, intent(out)  :: rc

    integer :: status
    character(len=ESMF_MAXSTR) :: Iam
    integer j
    character(len=ESMF_MAXSTR) :: cfiovarname,tname,tcfioname
    Iam = "getVarNameIgnoreCase"

    tname = vname
    tname = ESMF_UtilStringUpperCase(tname)
    do j=1,size(vars)
        call ESMF_CFIOVarInfoGet(vars(j),vname=cfiovarname,RC=STATUS)
        VERIFY_(STATUS)
        tcfioname = cfiovarname
        tcfioname = ESMF_UtilStringUpperCase(tcfioname,rc=status)
        if (trim(tname) == trim(tcfioname)) then
           vname = cfiovarname
           exit
        end if
    enddo
    RETURN_(ESMF_SUCCESS)
    end subroutine getVarNameIgnoreCase

  end subroutine MAPL_CFIOReadBundle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP

! !IIROUTINE: MAPL_CFIOReadState --- Reads an ESMF State

! !INTERFACE:
!
  subroutine MAPL_CFIOReadState ( FILETMPL, TIME, STATE, NOREAD, RC, &
                                  VERBOSE, FORCE_REGRID, ONLY_VARS,  &
                                  TIME_IS_CYCLIC, TIME_INTERP,       &
                                  conservative, voting, ignoreCase, doParallel )
!
! !ARGUMENTS:
!
    character(len=*),            intent(IN   ) :: FILETMPL
    type(ESMF_TIME),             intent(INout) :: TIME
    type(ESMF_STATE),            intent(INOUT) :: STATE
    logical, optional,           intent(IN   ) :: NOREAD
    integer, optional,           intent(  OUT) :: RC
    logical, optional,           intent(  IN)  :: VERBOSE
    logical, optional,           intent(IN)    :: FORCE_REGRID ! obsolete
    logical, optional,           intent(IN)    :: TIME_IS_CYCLIC
    logical, optional,           intent(IN)    :: TIME_INTERP
    logical, optional,           intent(IN)    :: conservative
    logical, optional,           intent(IN)    :: voting
    logical, optional,           intent(IN)    :: ignoreCase
    logical, optional,           intent(IN)    :: doParallel
    character(len=*), optional,  intent(IN   ) :: ONLY_VARS ! comma separated,
                                                            ! no spaces
!
#ifdef ___PROTEX___
!
    !DESCRIPTION: 

     Serializes an ESMF state into a Bundle and reads its content from
     a file. The file is open, read from, and closed on exit. The
     arguments are:
\bd
     \item[FILETMPL] A GrADS-style file name template. In its simplest
     form is the full path name for the file to be read. However, it
     can contain the following tokens which will be expanded from
     the current time in {\em TIME}:
   \bd
        \item[\%y4] 4 digits for year
        \item[\%m2] 2 digits for month, to expand to 01, 02, .., 12
        \item[\%m3] 3 digits for month, to expand to jan, feb, mar, ..., dec
        \item[\%d2] 2 digits for day
        \item[\%h2] 2 digits for hour
        \item[\%n2] 2 digits for minutes
   \ed
    Example: if FILETMPL = ``forecast.\%y4-\%m2-\%d2\_%h2z.nc4'', and the clock
    says it is 18Z on 05 February 2007, the template will expand in the
    following file name: ``forecast.2007-02-05\_18Z.nc4''
%
    \item[TIME] The ESMF time to read from the file
%
    \item[STATE] An ESMF State to read the data in. Usually used in conjubction
    with ONLY\_VARS.
%
    \item[{[NOREAD]}] If .TRUE., no data is actually read into the Bundle. This is
    useful to define a Bundle with the same variables as presented in the
    file, which in turn can be used to created a MAPL\_CFIO object for
    writing.
%
    \item[{[RC]}] Error return code; set to ESMF\_SUCCESS if all is well.    
%
    \item[{[VERBOSE]}] If .TRUE., prints progress messages to STDOUT; useful
     for debugging.
%
    \item[{[FORCE\_REGRID]}] Obsolete; kept for backward compatibility but
    has no effect.
%
    \item[{[TIME\_IS\_CYCLIC]}]  If .TRUE. it says that the input file is periodic
    in time. Useful for reading climatological files. For example, if the
    input file has 12 monthly means from January to December of 2001, setting
    this option to .TRUE. allows one to read this data for any other year. See 
    note below regarding issues with reading monthly mean data.
%
    \item[{[TIME\_INTERP]}] If .TRUE., the input file does not have to coincide with the
    actual times on file. In such cases, the data for the bracketing times are
    read and the data is properly interpolated in time. The input time, though,
    need to be within the range of times present on file 
   (unless {\tt TIME\_IS\_CYCLIC} is specified).
%
    \item[{[ONLY\_VARS]}] A list of comma separated vafriables to be read from the
    file. By default, all variables are read from the file. This option allows
    one to read a subset of vafriables. Example: ONLY\_VARS=``u,v,ps''.
%
\ed

     !DESIGN ISSUES:

     The input argument {\tt TIME} should be replaced with {\tt CLOCK}
     for consistency with the rest of the API. One should also provide
     an interface involving the MAPL CFIO object.

#endif

!EOP

    character(len=*), parameter  :: Iam="MAPL_CFIOReadState"
    integer                      :: STATUS

! Locals

    type(ESMF_FieldBundle) :: tBUNDLE

!                          ----

!   Create an empty bundle
!   ----------------------
    tBUNDLE = ESMF_FieldBundleCreate ( name=Iam, rc=STATUS )
    VERIFY_(STATUS)
    
!   Serialize the state
!   -------------------
    call ESMFL_BundleAddState ( tBUNDLE, STATE, rc=STATUS, VALIDATE=.true. )
    VERIFY_(STATUS)

!   Read the Bundle
!   ---------------
    call MAPL_CFIOReadBundle( FILETMPL, TIME, tBUNDLE,         &
                              NOREAD = NOREAD,                 &
                              VERBOSE = VERBOSE,               &
                              FORCE_REGRID=FORCE_REGRID,       &
                              ONLY_VARS = ONLY_VARS,           &
                              TIME_IS_CYCLIC = TIME_IS_CYCLIC, &
                              TIME_INTERP = TIME_INTERP,       &
                              conservative = conservative,     &
                              voting = voting,                 &
                              ignoreCase = ignoreCase,         &
                              doParallel = doParallel,        &
                              RC = STATUS )

    VERIFY_(STATUS)

!   All done
!   --------
    call ESMF_FieldBundleDestroy ( tBUNDLE, rc=STATUS )
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

 end subroutine MAPL_CFIOReadState

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IIROUTINE: MAPL_CFIOReadField --- Reads an ESMF Field

! !INTERFACE:
!
  subroutine MAPL_CFIOReadField     ( VARN, FILETMPL, TIME,       FIELD, RC, &
                                      VERBOSE, FORCE_REGRID, TIME_IS_CYCLIC, &
                                      TIME_INTERP,                           &
                                      conservative , voting, ignoreCase, doParallel)
!
! !ARGUMENTS:
!
    character(len=*),            intent(IN   ) :: VARN       ! Variable name
    character(len=*),            intent(IN   ) :: FILETMPL   ! File name
    type(ESMF_TIME),             intent(INout) :: TIME
    type(ESMF_FIELD),            intent(INout) :: FIELD
    integer, optional,           intent(  OUT) :: RC
    logical, optional,           intent(  IN)  :: VERBOSE
    logical, optional,           intent(IN)    :: FORCE_REGRID
    logical, optional,           intent(IN)    :: TIME_IS_CYCLIC
    logical, optional,           intent(IN)    :: TIME_INTERP
    logical, optional,           intent(IN)    :: conservative
    logical, optional,           intent(IN)    :: voting
    logical, optional,           intent(IN)    :: ignoreCase
    logical, optional,           intent(IN)    :: doParallel
!
#ifdef ___PROTEX___

    !DESCRIPTION: 

     Reads a variable from a file and stores it on an ESMF Field.
     The file is open, read from, and closed on exit. The
     arguments are:
\bd
     \item[VARN] The variable name.
%
     \item[FILETMPL] A GrADS-style file name template. In its simplest
     form is the full path name for the file to be read. However, it
     can contain the following tokens which will be expanded from
     the current time in {\em TIME}:
   \bd
        \item[\%y4] 4 digits for year
        \item[\%m2] 2 digits for month, to expand to 01, 02, .., 12
        \item[\%m3] 3 digits for month, to expand to jan, feb, mar, ..., dec
        \item[\%d2] 2 digits for day
        \item[\%h2] 2 digits for hour
        \item[\%n2] 2 digits for minutes
   \ed
    Example: if FILETMPL = ``forecast.\%y4-\%m2-\%d2\_%h2z.nc4'', and the clock
    says it is 18Z on 05 February 2007, the template will expand in the
    following file name: ``forecast.2007-02-05\_18Z.nc4''
%
    \item[TIME] The ESMF time to read from the file
%
    \item[{[RC]}] Error return code; set to ESMF\_SUCCESS if all is well.    
%
    \item[{[VERBOSE]}] If .TRUE., prints progress messages to STDOUT; useful
     for debugging.
%
    \item[{[FORCE\_REGRID]}] Obsolete; kept for backward compatibility but
    has no effect.
%
    \item[{[TIME\_IS\_CYCLIC]}]  If .TRUE. it says that the input file is periodic
    in time. Useful for reading climatological files. For example, if the
    input file has 12 monthly means from January to December of 2001, setting
    this option to .TRUE. allows one to read this data for any other year. See 
    note below regarding issues with reading monthly mean data.
%
    \item[{[TIME\_INTERP]}] If .TRUE., the input file does not have to coincide with the
    actual times on file. In such cases, the data for the bracketing times are
    read and the data is properly interpolated in time. The input time, though,
    need to be within the range of times present on file 
   (unless {\tt TIME\_IS\_CYCLIC} is specified).
%
    \item[{[ONLY\_VARS]}] A list of comma separated vafriables to be read from the
    file. By default, all variables are read from the file. This option allows
    one to read a subset of vafriables. Example: ONLY\_VARS=``u,v,ps''.
%
\ed

     !DESIGN ISSUES:

     The input argument {\tt TIME} should be replaced with {\tt CLOCK}
     for consistency with the rest of the API. The input {\tt GRID} is not necessary
     as it can be found inside the field. One should also provide
     an interface involving the MAPL CFIO object.

#endif

!EOP

    character(len=*), parameter  :: Iam="MAPL_CFIOReadField"
    integer                      :: STATUS
    type(ESMF_GRID)              :: GRID

! Locals

    type(ESMF_FIELDBUNDLE)  :: BUNDLE
 
!   Create a temporary empty bundle
!   -------------------------------
    call ESMF_FieldGet(Field, grid=Grid, rc=status)
    VERIFY_(STATUS)
    BUNDLE =  ESMF_FieldBundleCreate ( name=Iam, rc=STATUS )
    VERIFY_(STATUS)
    call ESMF_FieldBundleSet ( bundle, grid=GRID, rc=STATUS )
    VERIFY_(STATUS)

!   Add the input field to the bundle
!   ---------------------------------
    call MAPL_FieldBundleAdd ( BUNDLE, FIELD, rc=STATUS )
    VERIFY_(STATUS)

!   Now, we read the variable into the bundle, which in turn will put
!    the data inside the input array
!   -----------------------------------------------------------------
    call MAPL_CFIOReadBundle( FILETMPL, TIME, BUNDLE,                    &
                              VERBOSE=VERBOSE,                           &
                              FORCE_REGRID=FORCE_REGRID,                 &
                              ONLY_VARS = trim(varn),                    &
                              TIME_IS_CYCLIC=TIME_IS_CYCLIC,             &
                              TIME_INTERP=TIME_INTERP,                   &
                              conservative=conservative,                 &
                              voting = voting, ignoreCase = ignoreCase,  &
                              doParallel = doParallel,                   &
                              RC=STATUS)
    VERIFY_(STATUS)    


!   Destroy temporary bundle; field data will be preserved
!   ------------------------------------------------------
    call ESMF_FieldBundleDestroy ( BUNDLE, rc=STATUS )
    VERIFY_(STATUS)    

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_CFIOReadField

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IIROUTINE: MAPL_CFIOReadArray3D --- Reads a 3D Fortran Array

! !INTERFACE:
!
  subroutine MAPL_CFIOReadArray3D ( VARN, FILETMPL, TIME, GRID, farrayPtr, RC, &
                                    VERBOSE, FORCE_REGRID, TIME_IS_CYCLIC,     &
                                    TIME_INTERP, conservative, voting, ignoreCase, doParallel )
!
! !ARGUMENTS:
!
    character(len=*),            intent(IN   ) :: VARN       ! Variable name
    character(len=*),            intent(IN   ) :: FILETMPL   ! File name
    type(ESMF_TIME),             intent(INout) :: TIME
    type(ESMF_GRID),             intent(IN   ) :: GRID
    real, pointer                              :: farrayPtr(:,:,:)
    integer, optional,           intent(  OUT) :: RC
    logical, optional,           intent(  IN)  :: VERBOSE
    logical, optional,           intent(IN)    :: FORCE_REGRID
    logical, optional,           intent(IN)    :: TIME_IS_CYCLIC
    logical, optional,           intent(IN)    :: TIME_INTERP
    logical, optional,           intent(IN)    :: conservative
    logical, optional,           intent(IN)    :: voting
    logical, optional,           intent(IN)    :: ignoreCase
    logical, optional,           intent(IN)    :: doParallel
!
#ifdef ___PROTEX___

    !DESCRIPTION: 

     Reads a variable from a file and stores it on an 3D Fortrran array.
     The file is open, read from, and closed on exit. The
     arguments are:
\bd
     \item[VARN] The variable name.
%
     \item[FILETMPL] A GrADS-style file name template. In its simplest
     form is the full path name for the file to be read. However, it
     can contain the following tokens which will be expanded from
     the current time in {\em TIME}:
   \bd
        \item[\%y4] 4 digits for year
        \item[\%m2] 2 digits for month, to expand to 01, 02, .., 12
        \item[\%m3] 3 digits for month, to expand to jan, feb, mar, ..., dec
        \item[\%d2] 2 digits for day
        \item[\%h2] 2 digits for hour
        \item[\%n2] 2 digits for minutes
   \ed
    Example: if FILETMPL = ``forecast.\%y4-\%m2-\%d2\_%h2z.nc4'', and the clock
    says it is 18Z on 05 February 2007, the template will expand in the
    following file name: ``forecast.2007-02-05\_18Z.nc4''
%
    \item[TIME] The ESMF time to read from the file
%
    \item[GRID] The ESMF grid associated with the Field. The data will be 
    (horizontally) interpolated to this grid if necessary.
%
    \item[{[RC]}] Error return code; set to ESMF\_SUCCESS if all is well.    
%
    \item[{[VERBOSE]}] If .TRUE., prints progress messages to STDOUT; useful
     for debugging.
%
    \item[{[FORCE\_REGRID]}] Obsolete; kept for backward compatibility but
    has no effect.
%
    \item[{[TIME\_IS\_CYCLIC]}]  If .TRUE. it says that the input file is periodic
    in time. Useful for reading climatological files. For example, if the
    input file has 12 monthly means from January to December of 2001, setting
    this option to .TRUE. allows one to read this data for any other year. See 
    note below regarding issues with reading monthly mean data.
%
    \item[{[TIME\_INTERP]}] If .TRUE., the input file does not have to coincide with the
    actual times on file. In such cases, the data for the bracketing times are
    read and the data is properly interpolated in time. The input time, though,
    need to be within the range of times present on file 
   (unless {\tt TIME\_IS\_CYCLIC} is specified).
%
    \item[{[ONLY\_VARS]}] A list of comma separated vafriables to be read from the
    file. By default, all variables are read from the file. This option allows
    one to read a subset of vafriables. Example: ONLY\_VARS=``u,v,ps''.
%
\ed

     !DESIGN ISSUES:

     The input argument {\tt TIME} should be replaced with {\tt CLOCK}
     for consistency with the rest of the API.  One should also
     provide an interface involving the MAPL CFIO object.

#endif

!EOP

    character(len=*), parameter  :: Iam="MAPL_CFIOReadArray3D"
    integer                      :: STATUS

    type(ESMF_Field)             :: FIELD

    real    :: const = 0.0
    integer :: ios, k

!                            ----

!   Special case: when filename is "/dev/null" it is assumed the user 
!   wants to set the variable to a constant
!   -----------------------------------------------------------------
    if ( FILETMPL(1:9) == '/dev/null' ) then    
         ios = -1
         k = index(FILETMPL,':')
         if ( k > 9 ) read(FILETMPL(k+1:),*,iostat=ios) const
         if ( ios /= 0 ) const = 0.0
         if ( MAPL_am_I_root() ) &
            print *, Iam // ': setting variable ' // trim(varn) // &
                            ' to constant = ', const
         RETURN_(ESMF_SUCCESS)
    end if

!   Create Field with input array
!   -----------------------------
    FIELD = ESMF_FieldCreate(grid=GRID,   &
            datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
            farrayPtr=farrayPtr, name=trim(varn), RC=STATUS)
    VERIFY_(STATUS)

   
!   Read array data from file
!   -------------------------
    call MAPL_CFIOReadField ( VARN, FILETMPL, TIME,       FIELD,          &
                              VERBOSE=VERBOSE, FORCE_REGRID=FORCE_REGRID, &
                              TIME_IS_CYCLIC=TIME_IS_CYCLIC,              &
                              TIME_INTERP=TIME_INTERP,                    &
                              conservative=conservative,                  &
                              voting=voting, ignoreCase = ignoreCase,     &
                              doParallel = doParallel,                    &
                              RC=STATUS)
    VERIFY_(STATUS)

!   Destroy the ESMF array (data will be preserved since we own it)
!   --------------------------------------------------------------
    call ESMF_FieldDestroy ( FIELD, RC=STATUS )
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_CFIOReadArray3D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP

! !IIROUTINE: MAPL_CFIOReadArray2D --- Reads a 2D Fortran Array

! !INTERFACE:
!
  subroutine MAPL_CFIOReadArray2D ( VARN, FILETMPL, TIME, GRID, farrayPtr, RC, &
                                    VERBOSE, FORCE_REGRID, TIME_IS_CYCLIC,     &
                                    TIME_INTERP , conservative, voting, ignoreCase, doParallel)
!
! !ARGUMENTS:
!
    character(len=*),            intent(IN)  :: VARN       ! Variable name
    character(len=*),            intent(IN)  :: FILETMPL   ! File name
    type(ESMF_TIME),             intent(INout)  :: TIME
    type(ESMF_GRID),             intent(IN)  :: GRID
    real, pointer                            :: farrayPtr(:,:)
    integer, optional,           intent(OUT) :: RC
    logical, optional,           intent(IN)  :: VERBOSE
    logical, optional,           intent(IN)  :: FORCE_REGRID
    logical, optional,           intent(IN)  :: TIME_IS_CYCLIC
    logical, optional,           intent(IN)  :: TIME_INTERP
    logical, optional,           intent(IN)    :: conservative
    logical, optional,           intent(IN)    :: voting
    logical, optional,           intent(IN)    :: ignoreCase
    logical, optional,           intent(IN)    :: doParallel
!
#ifdef ___PROTEX___

    !DESCRIPTION: 

     Reads a variable from a file and stores it on an 3D Fortrran array.
     The file is open, read from, and closed on exit. The
     arguments are:
\bd
     \item[VARN] The variable name.
%
     \item[FILETMPL] A GrADS-style file name template. In its simplest
     form is the full path name for the file to be read. However, it
     can contain the following tokens which will be expanded from
     the current time in {\em TIME}:
   \bd
        \item[\%y4] 4 digits for year
        \item[\%m2] 2 digits for month, to expand to 01, 02, .., 12
        \item[\%m3] 3 digits for month, to expand to jan, feb, mar, ..., dec
        \item[\%d2] 2 digits for day
        \item[\%h2] 2 digits for hour
        \item[\%n2] 2 digits for minutes
   \ed
    Example: if FILETMPL = ``forecast.\%y4-\%m2-\%d2\_%h2z.nc4'', and the clock
    says it is 18Z on 05 February 2007, the template will expand in the
    following file name: ``forecast.2007-02-05\_18Z.nc4''
%
    \item[TIME] The ESMF time to read from the file
%
    \item[GRID] The ESMF grid associated with the Field. The data will be 
    (horizontally) interpolated to this grid if necessary.
%
    \item[{[RC]}] Error return code; set to ESMF\_SUCCESS if all is well.    
%
    \item[{[VERBOSE]}] If .TRUE., prints progress messages to STDOUT; useful
     for debugging.
%
    \item[{[FORCE\_REGRID]}] Obsolete; kept for backward compatibility but
    has no effect.
%
    \item[{[TIME\_IS\_CYCLIC]}]  If .TRUE. it says that the input file is periodic
    in time. Useful for reading climatological files. For example, if the
    input file has 12 monthly means from January to December of 2001, setting
    this option to .TRUE. allows one to read this data for any other year. See 
    note below regarding issues with reading monthly mean data.
%
    \item[{[TIME\_INTERP]}] If .TRUE., the input file does not have to coincide with the
    actual times on file. In such cases, the data for the bracketing times are
    read and the data is properly interpolated in time. The input time, though,
    need to be within the range of times present on file 
   (unless {\tt TIME\_IS\_CYCLIC} is specified).
%
    \item[{[ONLY\_VARS]}] A list of comma separated vafriables to be read from the
    file. By default, all variables are read from the file. This option allows
    one to read a subset of vafriables. Example: ONLY\_VARS=``u,v,ps''.
%
\ed

     !DESIGN ISSUES:

     The input argument {\tt TIME} should be replaced with {\tt CLOCK}
     for consistency with the rest of the API.  One should also
     provide an interface involving the MAPL CFIO object.

#endif

!EOP

    character(len=*), parameter  :: Iam="MAPL_CFIOReadArray2D"
    integer                      :: STATUS

    type(ESMF_Field)             :: FIELD

    real    :: const = 0.0
    integer :: ios, k
    integer, allocatable    :: gridToFieldMap(:)
    integer                 :: gridRank

!                            ----


!   Special case: when filename is "/dev/null" it is assumed the user 
!   wants to set the variable to a constant
!   -----------------------------------------------------------------
    if ( FILETMPL(1:9) == '/dev/null' ) then    
         ios = -1
         k = index(FILETMPL,':')
         if ( k > 9 ) read(FILETMPL(k+1:),*,iostat=ios) const
         if ( ios /= 0 ) const = 0.0
         if ( MAPL_am_I_root() ) &
            print *, Iam // ': setting variable ' // trim(varn) // &
                            ' to constant = ', const
         RETURN_(ESMF_SUCCESS)
    end if

!   Create Field with input array
!   -----------------------------

    call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
    VERIFY_(STATUS)
    allocate(gridToFieldMap(gridRank), stat=status)
    VERIFY_(STATUS)
    if(gridRank == 2) then
       gridToFieldMap(1) = 1
       gridToFieldMap(2) = 2
    else if (gridRank == 3) then
       gridToFieldMap(1) = 1
       gridToFieldMap(2) = 2
       gridToFieldMap(3) = 0
    else
       RETURN_(ESMF_FAILURE)
    end if

    FIELD = ESMF_FieldCreate(grid=GRID, &
            datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
            farrayPtr=farrayPtr, name=trim(varn), gridToFieldMap=gridToFieldMap, RC=STATUS)
    VERIFY_(STATUS)
   
    deallocate(gridToFieldMap)

!   Read array data from file
!   -------------------------
    call MAPL_CFIOReadField ( VARN, FILETMPL, TIME,       FIELD,          &
                              VERBOSE=VERBOSE, FORCE_REGRID=FORCE_REGRID, &
                              TIME_INTERP=TIME_INTERP,                    &
                              TIME_IS_CYCLIC=TIME_IS_CYCLIC,              &
                              conservative=conservative,                  &
                              voting = voting, ignoreCase = ignoreCase,   &
                              doParallel = doParallel,                    &
                              RC=STATUS)
    VERIFY_(STATUS)

!   Destroy the ESMF array (data will be preserved since we own it)
!   --------------------------------------------------------------
    call ESMF_FieldDestroy ( FIELD, RC=STATUS )
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

   end subroutine MAPL_CFIOReadArray2D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP
! !IROUTINE: MAPL_CFIODestroy --- Destroys MAPL CFIO Object

! !INTERFACE:
!
  subroutine MAPL_CFIODestroy( MCFIO, RC )
!
! !ARGUMENTS:
!
  type(MAPL_CFIO),             intent(INOUT) :: MCFIO
  integer, optional,           intent(  OUT) :: RC

! !DESCRIPTION: 
!
!    Destroys a MAPL CFIO object. It closes any file associated with
!    it and deallocates memory.

!EOP

  integer :: status
  character(len=*), parameter  :: Iam="MAPL_CFIODestroy"
  integer :: k

  if(associated(MCFIO%Krank     )) deallocate(MCFIO%Krank     )   
  if(associated(MCFIO%reqs      )) deallocate(MCFIO%reqs      )
  if(associated(MCFIO%varname   )) deallocate(MCFIO%varname   )
  if(associated(MCFIO%vardims   )) deallocate(MCFIO%vardims   )
  if(associated(MCFIO%Levs      )) deallocate(MCFIO%Levs      )
  if(associated(MCFIO%vartype   )) deallocate(MCFIO%vartype   )
  if(associated(MCFIO%needvar   )) deallocate(MCFIO%needvar   )
  if(associated(MCFIO%pairList  )) deallocate(MCFIO%pairList  )

  nullify(MCFIO%Krank     )   
  nullify(MCFIO%reqs      )
  nullify(MCFIO%varname   )
  nullify(MCFIO%vardims   )
  nullify(MCFIO%Levs      )

  call MAPL_HorzTransformDestroy(MCFIO%Trans,rc=STATUS)
  VERIFY_(STATUS)

  if (MCFIO%Root > 0) then
     if (MCFIO%myPE == MAPL_NodeRankList(MCFIO%Root)%rank(1)) then
        call ESMF_CFIOFileClose(MCFIO%CFIO,rc=status)
        VERIFY_(STATUS)
     end if
  end if

  call ESMF_CFIODestroy(MCFIO%CFIO,rc=status)
  VERIFY_(STATUS)

  MCFIO%created = .false.

  RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_CFIODestroy


!BOP
! !IROUTINE: MAPL_CFIOClose --- Close file in MAPL CFIO Object

! !INTERFACE:
!
  subroutine MAPL_CFIOClose( MCFIO, filename, RC )
!
! !ARGUMENTS:
!
  type(MAPL_CFIO),             intent(INOUT) :: MCFIO
  character(len=*), optional,  intent(IN   ) :: filename
  integer, optional,           intent(  OUT) :: RC

! !DESCRIPTION: 
!
!    Not a full destroy; only closes the file.

!EOP

  integer :: status
  character(len=*), parameter  :: Iam="MAPL_CFIOClose"

  if (MCFIO%myPE == MAPL_NodeRankList(MCFIO%Root)%rank(1)) then
     call ESMF_CFIOFileClose(MCFIO%CFIO,rc=status)
     VERIFY_(STATUS)
     if (present(filename)) then
        close(99)
        open (99,file=trim(filename)//".done",form='formatted')
        close(99)
     end if
  end if

  RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_CFIOClose



  subroutine MAPL_CFIOSet( MCFIO, Root, Psize, fName, Krank, IOWorker, globalComm, RC )
!
! !ARGUMENTS:
!
  type(MAPL_CFIO),             intent(INOUT) :: MCFIO
  integer, optional,           intent(IN   ) :: Root, Psize
  character*(*), optional,     intent(IN   ) :: fName
  integer, optional,           intent(IN   ) :: Krank(:)
  integer, optional,           intent(IN   ) :: IOWorker
  integer, optional,           intent(IN   ) :: globalComm
  integer, optional,           intent(  OUT) :: RC

! !DESCRIPTION: 
!
!    Not a full destroy; only closes the file.

!EOP

  integer :: status
  character(len=*), parameter  :: Iam="MAPL_CFIOSet"

  if(present(Root)) then
     mCFIO%Root = Root
  endif

  if(present(Psize)) then
     mCFIO%Partsize = Psize
  endif

  if(present(fName)) then
     mCFIO%fName = fName
  endif
 
  if(present(Krank)) then
    mCFIO%Krank = Krank
  endif

  if(present(IOWorker)) then
    mCFIO%AsyncWorkRank = IOWorker
  end if

  if(present(globalComm)) then
    mCFIO%globalComm = globalComm
  end if

  RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_CFIOSet



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! This is a candidate for ESMFL, here for dependency reasons
!  

  subroutine GridGetLatLons_ ( grid, lons, lats, rc )

    implicit NONE
    type(ESMF_Grid) :: grid
    real, pointer   :: lons(:), lats(:)
    integer, optional :: rc

!                     ---

    character(len=*), parameter :: Iam = 'GridGetLatLons'

    real(KIND=8), pointer  :: R8D2(:,:)
    real, pointer          :: lons2d(:,:), lats2d(:,:)
    real, pointer          :: LONSLocal(:,:), LATSlocal(:,:)
    integer                :: IM_WORLD, JM_WORLD, dims(3), STATUS
    type(ESMF_DELayout )   :: layout
    type(ESMF_DistGrid )   :: distgrid

!                          ----

!      Get world dimensions
!      --------------------
       call MAPL_GridGet ( grid, globalCellCountPerDim=DIMS, RC=STATUS)
       VERIFY_(STATUS)

       IM_WORLD = dims(1)
       JM_WORLD = dims(2)

!      Allocate memory for output if necessary
!      ---------------------------------------
       if ( .not. associated(lons) ) then
            allocate(lons(IM_WORLD), stat=STATUS)
       else
            if(size(LONS,1) /= IM_WORLD) STATUS = 1
       end if
       VERIFY_(status)
       if ( .not. associated(lats) ) then
            allocate(lats(JM_WORLD), stat=STATUS)
       else
            if(size(LATS,1) /= JM_WORLD) STATUS = 1
       end if
       VERIFY_(status)

!      Local work space
!      ----------------
       allocate(LONS2d(IM_WORLD,JM_WORLD), LATS2d(IM_WORLD,JM_WORLD), &
                STAT=status)             
       VERIFY_(status)

!      Get the local longitudes and gather them into a global array
!      ------------------------------------------------------------

       call ESMF_GridGetCoord(grid, localDE=0, coordDim=1, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=R8D2, rc=status)
       VERIFY_(STATUS) 
       allocate(LONSLOCAL(size(R8D2,1),size(R8D2,2)),stat=status)
       VERIFY_(STATUS) 
       LONSLOCAL = R8D2*(180._8/MAPL_PI_R8)
       call ArrayGather(LONSLOCAL, LONS2D, GRID, RC=STATUS)
       VERIFY_(STATUS) 
       DEALLOCATE(LONSlocal)

       call ESMF_GridGetCoord(grid, localDE=0, coordDim=2, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=R8D2, rc=status)
       VERIFY_(STATUS) 
       allocate(LATSLOCAL(size(R8D2,1),size(R8D2,2)),stat=status)
       VERIFY_(STATUS) 

       LATSLOCAL = R8D2*(180._8/MAPL_PI_R8)
       call ArrayGather(LATSLOCAL, LATS2D, GRID, RC=STATUS)
       VERIFY_(STATUS)
       DEALLOCATE(LATSlocal)


       call ESMF_GridGet(GRID, DistGrid=distgrid, rc=status)
       VERIFY_(STATUS)
       call ESMF_DistGridGet(distgrid, DELayout=layout, rc=status)
       VERIFY_(STATUS)
       call MAPL_CommsBcast(layout, lons2d, size(lons2d), 0, rc = status)
       VERIFY_(STATUS)
       call MAPL_CommsBcast(layout, lats2d, size(lats2d), 0, rc = status)
       VERIFY_(STATUS)


!      Return 1D arrays
!      ----------------
       LONS = LONS2D(:,1)
       LATS = LATS2D(1,:)

       DEALLOCATE(LONS2d, LATS2d )
       
       RETURN_(ESMF_SUCCESS)
     end subroutine GridGetLatLons_

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  subroutine VertInterp(v2,v3,pp,ple_,pl_,rc)

    real,              intent(OUT) :: v2(:,:)
    real,              intent(IN ) :: v3(:,:,:)
    real,              intent(IN ) :: pp
    real,     target,  intent(IN ) :: ple_(:,:,:)
    real,     target,  intent(IN ) :: pl_(:,:,:)
    integer, optional, intent(OUT) :: rc

    real, dimension(size(v2,1),size(v2,2)) :: al,PT,PB
    integer km, K, msn
    logical flip
    real    ppx
    real, pointer   :: plx(:,:,:),pl(:,:,:),ps(:,:)

    integer        :: status
    character*(10) :: Iam='VertInterp'

    if(size(v3,3)==size(ple_,3)) then
       pl => ple_
       ps => ple_(:,:,ubound(ple_,3))
    else
       pl => pl_
       ps => null()
    endif

    km   = size(pl,3)

    flip = pl(1,1,2) < pl(1,1,1)

    if(flip) then
       allocate(plx(size(pl,1),size(pl,2),size(pl,3)),stat=status)
       VERIFY_(STATUS)
       plx = -pl
       ppx = -pp
       msn = -1
    else
       plx => pl
       ppx = pp
       msn = 1
    end if

    v2   = MAPL_UNDEF

       pb   = plx(:,:,km)
       do k=km-1,1,-1
          pt = plx(:,:,k)
          if(all(pb<ppx)) exit
          where(ppx>pt .and. ppx<=pb)
             al = (pb-ppx)/(pb-pt)
             where (v3(:,:,k)   .eq. MAPL_UNDEF ) v2 = v3(:,:,k+1) 
             where (v3(:,:,k+1) .eq. MAPL_UNDEF ) v2 = v3(:,:,k)
             where (v3(:,:,k)   .ne. MAPL_UNDEF .and.  v3(:,:,k+1) .ne. MAPL_UNDEF  ) 
                    v2 = v3(:,:,k)*al + v3(:,:,k+1)*(1.0-al)
             end where
          end where
          pb = pt
       end do

! Extend Lowest Level Value to the Surface
! ----------------------------------------
    if( associated(ps) ) then
        where( (ppx>plx(:,:,km).and.ppx<=ps*msn) )
                v2 = v3(:,:,km)
        end where
    end if

    if(flip) then
       deallocate(plx,stat=status)
       VERIFY_(STATUS)
    end if

    RETURN_(ESMF_SUCCESS)
  end subroutine VertInterp

  subroutine MAPL_GetCurrentFile(FileTmpl, Time, Filename, RC, EXPID)
    character(len=*),    intent(IN   ) :: filetmpl
    type(ESMF_Time),     intent(INout) :: time
    character(len=*),    intent(  out) :: filename
    integer, optional,   intent(  out) :: rc
    character(len=*), optional, intent(  in) :: EXPID

    integer :: status
    character(len=ESMF_MAXSTR), parameter:: IAm='MAPL_GetCurrentFile'

    character(len=ESMF_MAXSTR)          :: DATE
    integer                             :: nymd
    integer                             :: nhms

    call ESMF_TimeGet(Time, timeString=DATE, RC=STATUS)
    VERIFY_(STATUS)
    
    call strToInt(DATE, nymd, nhms)
    call ESMF_CFIOstrTemplate ( Filename, FileTmpl, 'GRADS', &
                                xid=EXPID, nymd=nymd, nhms=nhms, stat=status )
    VERIFY_(STATUS)
    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_GetCurrentFile

  logical function MAPL_CFIOIsCreated(MCFIO)
    type(MAPL_CFIO),             intent(IN) :: MCFIO
    MAPL_CFIOIsCreated = MCFIO%Created
  end function MAPL_CFIOIsCreated

  character(len=ESMF_MAXSTR) function MAPL_CFIOGetFilename(MCFIO)
    type(MAPL_CFIO),             intent(IN) :: MCFIO
    MAPL_CFIOGetFilename = MCFIO%fNAME
  end function MAPL_CFIOGetFilename

  subroutine MAPL_CFIOGetTimeString(mcfio,Clock,Date,rc)
  
    type(MAPL_CFIO  ),          intent(inout) :: MCFIO
    type(ESMF_Clock),           intent(in   ) :: Clock 
    character(len=ESMF_MAXSTR), intent(inout) :: Date
    integer, optional,          intent(out  ) :: rc

    integer :: status
    character(len=ESMF_MAXSTR) :: Iam

    type(ESMF_Time) :: time
    integer :: YY,MM,DD,H,M,S
    integer :: noffset
    logical :: LPERP
    type(ESMF_Alarm)           :: PERPETUAL
    character(len=ESMF_MAXSTR) :: ClockName

    Iam = "MAPL_CFIOGetTimeString"

    call ESMF_ClockGet       (CLOCK, name=ClockName, CurrTime =TIME, RC=STATUS)
    VERIFY_(STATUS)

     call ESMF_TimeIntervalGet( MCFIO%OFFSET, S=noffset, rc=status )
     VERIFY_(STATUS)
     if( noffset /= 0 ) then
         LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
        if( LPERP ) then
            call ESMF_ClockGetAlarm ( clock, alarmName='PERPETUAL', alarm=PERPETUAL, rc=status )
            VERIFY_(STATUS)
            if( ESMF_AlarmIsRinging(PERPETUAL) ) then
                call ESMF_TimeGet ( Time, YY = YY, &
                                          MM = MM, &
                                          DD = DD, &
                                          H  = H , &
                                          M  = M , &
                                          S  = S, rc=status )
                                          MM = MM + 1
                call ESMF_TimeSet ( Time, YY = YY, &
                                          MM = MM, &
                                          DD = DD, &
                                          H  = H , &
                                          M  = M , &
                                          S  = S, rc=status )
            endif
        endif
     endif

    TIME = TIME - MCFIO%OFFSET

    call ESMF_TimeGet        (TIME,     timeString=DATE,     RC=STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

    end subroutine MAPL_CFIOGetTimeString
    
    subroutine MAPL_CFIOstartAsyncColl(mcfio,clock,mapl_comm,markdone,rc)

    type(MAPL_CFIO  ),          intent(inout) :: MCFIO
    type(ESMF_Clock) ,          intent(inout) :: clock
    type(MAPL_Communicators),   intent(inout) :: mapl_comm
    integer,                    intent(in   ) :: markdone
    integer, optional       ,   intent(out  ) :: rc

    integer :: status
    character(len=ESMF_MAXSTR) :: Iam

    integer :: slices
    character(len=ESMF_MAXSTR) :: TimeString,filename
    integer :: im,jm,ionode,l,nn,k,csize
    integer, allocatable :: levindex(:)
    character(len=esmf_maxstr), allocatable ::  levname(:)

    Iam = "MAPL_CFIOstartAsyncColl"
    im = mcfio%im
    jm = mcfio%jm

    ! count the number of slices
    slices = 0
    do L=1,size(MCFIO%VarDims)
       if (MCFIO%VarDims(L)==2) then
          slices=slices+1
       else if (MCFIO%VarDims(L) ==3) then
          do k=1,MCFIO%lm
             slices=slices+1
          enddo
       endif
    enddo
 
    call MAPL_CFIOServerGetFreeNode(mapl_comm,IOnode,im,jm,slices,rc=status)
    VERIFY_(STATUS)
    call MAPL_CFIOSet(mcfio,IOWorker=IOnode,rc=status)
    VERIFY_(STATUS)
    call MAPL_CFIOGetTimeString(mcfio,clock,timeString,rc=status)
    VERIFY_(STATUS)
    filename = MAPL_CFIOGetFilename(mcfio)
    call MAPL_CFIOAsyncSendCollInfo(filename,IM,JM,timeString,slices &
           ,IOnode,markdone,mapl_comm,rc)
    VERIFY_(STATUS)

    allocate(levindex(slices),stat=status)
    VERIFY_(STATUS)
    allocate(levname(slices),stat=status)
    VERIFY_(STATUS)
    nn = 0

    VARIABLES: do L=1,size(MCFIO%VarDims)

       RANK: if (MCFIO%VarDims(L)==2) then

          nn=nn+1
          levindex(nn)=0
          levname(nn)=mcfio%VarName(L)

       elseif (MCFIO%VarDims(L)==3) then


          LEVELS: do k=1,MCFIO%lm

             nn=nn+1
             levindex(nn)=k
             levname(nn)=mcfio%VarName(L)
          enddo LEVELS
       end if Rank
    enddo VARIABLES

    call MPI_Send(mcfio%krank, slices, MPI_INTEGER, IONODE, MAPL_TAG_SHIPINFO, &
     mapl_comm%maplcomm,status)
    VERIFY_(STATUS)
    call MPI_Send(levindex, slices, MPI_INTEGER , IONODE, MAPL_TAG_SHIPINFO, &
     mapl_comm%maplcomm,status)
    VERIFY_(STATUS)
    csize=slices*ESMF_MAXSTR
    call MPI_Send(levname,csize,MPI_CHARACTER, IONODE, MAPL_TAG_SHIPINFO, &
     mapl_comm%maplcomm,status)
    VERIFY_(STATUS)

    deallocate(levindex)
    deallocate(levname)

    RETURN_(ESMF_SUCCESS)

    end subroutine


    subroutine MAPL_CFIOBcastIONode(mcfio,root,comm,rc)
    type(MAPL_CFIO  ),          intent(inout) :: MCFIO
    integer,                    intent(in   ) :: root
    integer,                    intent(in   ) :: comm
    integer, optional,          intent(out  ) :: rc

    integer :: status
    character(len=ESMF_MAXSTR) :: Iam
    Iam = "MAPL_CFIOBcastIONode"

    call mpi_bcast(mcfio%AsyncWorkRank,1,MPI_INTEGER,root,comm,status)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
    end subroutine

  subroutine MAPL_CFIOAsyncSendCollInfo(filename,IM,JM,timeString,nlevs,workRank,markdone,mapl_comm,rc)
  character(len=ESMF_MAXSTR), intent(in) :: filename
  integer,                    intent(in) :: IM
  integer,                    intent(in) :: JM
  character(len=ESMF_MAXSTR), intent(in) :: timeString
  integer,                    intent(in) :: nlevs
  integer,                    intent(in) :: workRank
  integer,                    intent(in) :: markdone
  type(MAPL_Communicators),   intent(in) :: mapl_comm
  integer, optional,          intent(out) :: rc

  integer :: status
  character(len=ESMF_MAXSTR) :: Iam
  type(MAPL_CFIOServerIOinfo) :: ServerInfo
  integer :: nymd,nhms

  Iam = "MAPL_CFIOAsyncSendCollInfo"

  ServerInfo%filename = filename
  ServerInfo%lons = IM
  ServerInfo%lats = JM
  ServerInfo%nlevs = nlevs
  call StrToInt(timeString,nymd,nhms)
  ServerInfo%date = nymd
  ServerInfo%time = nhms
  ServerInfo%markdone = markdone

  call MPI_Send(ServerInfo, 1, mpi_io_server_info_type, workRank, MAPL_TAG_SHIPINFO, &
     mapl_comm%maplcomm,status)
  VERIFY_(STATUS)

  RETURN_(ESMF_SUCCESS)

  end subroutine

end module MAPL_CFIOMod
