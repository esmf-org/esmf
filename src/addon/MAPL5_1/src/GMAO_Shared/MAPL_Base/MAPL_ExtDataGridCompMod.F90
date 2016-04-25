!#include "MAPL_Exceptions.h"
#include "MAPL_Generic.h"

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------

   MODULE MAPL_ExtDataGridCompMod

!BOP
! !MODULE: MAPL_ExtDataGridCompMod - Implements Interface to External Data
!
! !DESCRIPTION: 
!
!  {\tt MAPL\_ExtDataGridComp} is an ESMF gridded component implementing
!  an interface to boundary conditions and other types of external data
!  files.
!
!  Developed for GEOS-5 release Fortuna 2.0 and later.
!
! !USES:
!
   USE ESMF
   use MAPL_BaseMod
   use MAPL_CommsMod
   use ESMFL_Mod
   use MAPL_GenericMod
   use MAPL_VarSpecMod
   use ESMF_CFIOFileMod
   use ESMF_CFIOMod
   use ESMF_CFIOUtilMod
   use MAPL_CFIOMod
   use MAPL_HorzTransformMod
   use MAPL_NewArthParserMod
   use MAPL_ConstantsMod, only: MAPL_PI
   use MAPL_IOMod, only: MAPL_NCIOParseTimeUnits

   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC SetServices
   public T_EXTDATA_STATE
   public EXTDATA_WRAP
!EOP
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!-------------------------------------------------------------------------

  integer, parameter :: MAPL_ExtDataVectorItem = 32
  logical            :: hasRun

  type Diurnal
     PRIVATE
     integer :: nTimes ! number of times for the month, usually would be 24
     integer :: Year, Day ! year and day on file
     integer, pointer :: times(:) => null()
     type(ESMF_Field), pointer :: field1(:) => null()
     type(ESMF_Time)  :: time1
     type(ESMF_Time)  :: time2
     character(ESMF_MAXPATHLEN) :: currentFile = " "
  end type Diurnal

! Primary Exports
! ---------------
  type PrimaryExport
     PRIVATE
     character(len=ESMF_MAXSTR)   :: name
     character(len=ESMF_MAXSTR)   :: units
     integer                      :: dim
     integer                      :: vloc
     character(len=ESMF_MAXSTR)   :: cyclic
     character(len=ESMF_MAXSTR)   :: refresh_template
     logical                      :: conservative
     real                         :: scale, offset
     logical                      :: do_offset, do_scale
     character(len=ESMF_MAXSTR)   :: var
     character(len=ESMF_MAXPATHLEN)   :: file
     logical                      :: hasFileReffTime
     character(len=ESMF_MAXSTR)   :: FileReffTime

     type(ESMF_Time), pointer     :: refresh_time => null()
     logical                      :: isConst
     real                         :: Const
     integer                      :: vartype ! MAPL_FieldItem or MAPL_BundleItem

!    variables for zero refresh time
     type(ESMF_Field)             :: finterp1, finterp2
     type(ESMF_FieldBundle)       :: binterp1, binterp2
     type(ESMF_Time)              :: time1, time2
     type(ESMF_Time)              :: interp_time1, interp_time2
     integer                      :: climyear
     type(ESMF_TimeInterval)      :: frequency
     type(ESMF_Time)              :: reff_time

     ! if primary export represents a pair of vector fields
     logical                      :: isVector, foundComp1, foundComp2
     ! fields to store endpoints for interpolation of a vector pair
     type(ESMF_Field)             :: v1_finterp1, v1_finterp2
     type(ESMF_Field)             :: v2_finterp1, v2_finterp2

     ! names of the two vector components in the gridded component where import is declared
     character(len=ESMF_MAXSTR)   :: vcomp1, vcomp2
     ! the corresponding names of the two vector components on file
     character(len=ESMF_MAXSTR)   :: fcomp1, fcomp2

     logical                      :: ExtDataAlloc
     ! time shifting during continuous update
     type(ESMF_TimeInterval)      :: tshift
     type(Diurnal)                :: diurnal_data
     type(ESMF_Alarm)             :: update_alarm
     logical                      :: alarmIsEnabled = .false.

  end type PrimaryExport

  type PrimaryExports
     PRIVATE
     integer :: nItems = 0
     type(PrimaryExport), pointer :: item(:) => null() 
  end type PrimaryExports

  type DerivedExport
     PRIVATE
     character(len=ESMF_MAXSTR)   :: name
     character(len=ESMF_MAXSTR)   :: expression
     character(len=ESMF_MAXSTR)   :: refresh_template
     logical                      :: ExtDataAlloc
     logical                      :: masking
     type(ESMF_Time), pointer     :: refresh_time => null()
     ! time shifting during continuous update
     type(ESMF_TimeInterval)      :: tshift
     type(ESMF_Alarm)             :: update_alarm
     logical                      :: alarmIsEnabled = .false.
  end type DerivedExport

  type DerivedExports
     PRIVATE
     integer :: nItems = 0
     type(DerivedExport), pointer :: item(:) => null()
  end type DerivedExports

  type MaskExport
     PRIVATE
     character(len=ESMF_MAXSTR) :: name
     character(len=ESMF_MAXSTR) :: var
     character(len=ESMF_MAXPATHLEN) :: file
     logical                    :: ExtDataAlloc
  end type MaskExport

  type MaskExports
     PRIVATE
     integer :: nItems = 0
     type(MaskExport), pointer :: item(:) => null()
  end type MaskExports

! Legacy state
! ------------
  type MAPL_ExtData_State
     PRIVATE
     type(PrimaryExports) :: Primary
     type(DerivedExports) :: Derived
     type(MaskExports)    :: Mask
     ! will add fields from export state to this state
     ! will also add new fields that could be mask
     ! or primary exports that were not in the export
     ! state recieved by ExtData, i.e. fields that are
     ! needed by a derived field where the primary fields
     ! are not actually required
     type(ESMF_State)     :: ExtDataState
     logical              :: active
     logical              :: ignoreCase
  end type MAPL_ExtData_State

! Hook for the ESMF
! -----------------
  type MAPL_ExtData_Wrap
     type (MAPL_ExtData_State), pointer :: PTR => null()
  end type MAPL_ExtData_WRAP

  type T_EXTDATA_STATE
     type(ESMF_State)    :: expState
     type(ESMF_GridComp) :: gc
  end type T_EXTDATA_STATE

  ! Wrapper for extracting internal state
  ! -------------------------------------
  type EXTDATA_WRAP
     type (T_EXTDATA_STATE), pointer :: PTR
  end type EXTDATA_WRAP

! This EXTERNAL subroutine is in the fv directory
!  and has real*8 interfaces

  interface
     subroutine A2CnoRotate(U, V)
       real, intent(INOUT) :: U(:,:,:)
       real, intent(INOUT) :: V(:,:,:)
     end subroutine A2CnoRotate
  end interface

  interface
     subroutine A2DnoRotate(U, V)
       real, intent(INOUT) :: U(:,:,:)
       real, intent(INOUT) :: V(:,:,:)
     end subroutine A2DnoRotate
  end interface

CONTAINS


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices --- Sets IRF services for the MAPL_ExtData
!
! !INTERFACE:

   SUBROUTINE SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

! !DESCRIPTION: Sets Initialize, Run and Finalize services. 
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

!   Local derived type aliases
!   --------------------------
    type (MAPL_ExtData_State), pointer  :: self   ! internal, that is
    type (MAPL_ExtData_wrap)            :: wrap

    character(len=ESMF_MAXSTR)          :: comp_name
    character(len=ESMF_MAXSTR)          :: Iam
    integer                             :: status

!                              ------------

!   Get my name and set-up traceback handle
!   ---------------------------------------
    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
    Iam = trim(comp_name) // '::' // trim(Iam)

!   Wrap internal state for storing in GC; rename legacyState
!   -------------------------------------
    allocate ( self, stat=STATUS )
    VERIFY_(STATUS)
    wrap%ptr => self
 
!                       ------------------------
!                       ESMF Functional Services
!                       ------------------------

!   Set the Initialize, Run, Finalize entry points
!   ----------------------------------------------
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE,  Initialize_, __RC__ )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,   Run_,        __RC__ )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE, Finalize_,   __RC__ )
        
!   Store internal state in GC
!   --------------------------
    call ESMF_UserCompSetInternalState ( GC, 'MAPL_ExtData_state', wrap, STATUS )
    VERIFY_(STATUS)
  

    call MAPL_TimerAdd(gc,name="Run", rc=status)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(gc,name="-Read", rc=status)
    VERIFY_(STATUS)
!   Generic Set Services
!   --------------------
    call MAPL_GenericSetServices ( GC, __RC__ )

!   All done
!   --------

    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE SetServices


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Initialize_ --- Initialize MAPL_ExtData
!
! !INTERFACE:
!

   SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

   implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout)   :: CLOCK   ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout) :: GC      ! Grid Component
   type(ESMF_State), intent(inout)    :: IMPORT  ! Import State
   type(ESMF_State), intent(inout)    :: EXPORT  ! Export State
   integer, intent(out)               :: rc      ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Grid)                   :: GRID        ! Grid
   type(ESMF_Config)                 :: CF_master          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: Status
   character(len=ESMF_MAXSTR)        :: buffer

   type(PrimaryExports)              :: Primary
   type(PrimaryExport), pointer      :: item
   type(DerivedExports)              :: Derived
   type(DerivedExport), pointer      :: derivedItem 
   type(MaskExports)                 :: Mask
   integer                           :: nLines, nCols
   integer                           :: i, iret
   integer                           :: ItemCount, itemCounter, j
   integer                           :: PrimaryItemCount, DerivedItemCount, MaskItemCount
   integer                           :: dims, vloc, knd, hw
   logical                           :: found

   type(ESMF_Time)                   :: time
   character(len=ESMF_MAXSTR)        :: VarName

   type (ESMF_Field)                 :: field,fieldnew
   integer                           :: fieldRank
   type (ESMF_FieldBundle)           :: bundle
   integer                           :: fieldcount
   type (ESMF_StateItem_Flag), pointer    :: ITEMTYPES(:)
   character(len=ESMF_MAXSTR), allocatable   :: ITEMNAMES(:)

   character(len=ESMF_MAXSTR),allocatable    :: PrimaryVarNames(:)
   character(len=ESMF_MAXSTR),allocatable    :: VarNames(:)
   integer                                   :: NumVarNames

   integer                                   :: nMasks
   character(len=ESMF_MAXSTR),allocatable    :: MaskVarNames(:)
   character(len=ESMF_MAXSTR),allocatable    :: MaskExprs(:)

!  logical to keep track of primary variables needed by derived fields, that ARE NOT in export state
   logical, allocatable              :: PrimaryVarNeeded(:)
   logical, allocatable              :: DerivedVarNeeded(:)
   logical, allocatable              :: MaskVarNeeded(:)
   logical, allocatable              :: LocalVarNeeded(:)

   type(ESMF_CFIO)                   :: cfio
   integer                           :: counter
   real, pointer                     :: ptr2d(:,:) => null()
   real, pointer                     :: ptr3d(:,:,:) => null()
   real                              :: const
   integer                           :: k, ios
   character(len=ESMF_MAXSTR)        :: c_offset, c_scale
   integer                           :: nlists, nlist
   character(len=ESMF_MAXSTR)        :: EXTDATA_CF
   type(ESMF_Config) :: CFtemp
   type(ESMF_Config) :: localCF
   integer           :: totalPrimaryEntries
   integer           :: totalDerivedEntries
   integer           :: totalMaskEntries
   logical           :: caseSensitiveVarNames
   character(len=ESMF_MAXSTR) :: component1,component2,expression
   integer           :: idx,nhms,ihr,imn,isc
   logical           :: isNegative
   integer           :: maskDate, maskTime, iyr, imm, idd, jmask
   integer           :: curTime, curDate, yy, mm, dd, hh, mn, ss, begDate, begTime, incSecs, fHours, tInterval
   integer           :: nymdB, nhmsB
   character(len=ESMF_MAXSTR) :: file_processed,thisLine,vunit
   logical           :: inBlock
   type(ESMF_VM) :: vm

!  Get my name and set-up traceback handle
!  ---------------------------------------
   Iam = 'Initialize_'
   call ESMF_GridCompGet( GC, name=comp_name, config=CF_master, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF_master, __RC__)

! Get information from export state
!----------------------------------
    call ESMF_StateGet(EXPORT, ITEMCOUNT=ItemCount, RC=STATUS)
    VERIFY_(STATUS)

    ! set ExtData on by default, let user turn it off if they want
    call ESMF_ConfigGetAttribute(CF_master,self%active, Label='USE_EXTDATA:',default=.true.,rc=status)

    ! set extdata to ignore case on variable names in files
    call ESMF_ConfigGetAttribute(CF_master,caseSensitiveVarNames, Label='CASE_SENSITIVE_VARIABLE_NAMES:',default=.false.,rc=status)
    self%ignoreCase = .not. caseSensitiveVarNames

    ! no need to run ExtData if there are no imports to fill
    if (ItemCount == 0) then
       self%active = .false.
    end if

    if (.not.self%active) then
       RETURN_(ESMF_SUCCESS)
    end if

!   Greetings
!   ---------
    if (MAPL_am_I_root()) then
         print *, TRIM(Iam)//': ACTIVE'
         print *
    end if

    allocate(ITEMNAMES(ITEMCOUNT), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES(ITEMCOUNT), STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(EXPORT, ITEMNAMELIST=ITEMNAMES, &
                       ITEMTYPELIST=ITEMTYPES, RC=STATUS)
    VERIFY_(STATUS)

!                               --------
!  Initialize MAPL Generic
!  -----------------------
   call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, clock,  __RC__ )


!                         ---------------------------
!                         Parse ExtData Resource File
!                         ---------------------------

   call ESMF_ConfigGetAttribute(CF_Master,value=EXTDATA_CF,Label="CF_EXTDATA:",rc=status)
   VERIFY_(STATUS)

   CFtemp = ESMF_ConfigCreate (rc=STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigLoadFile(CFtemp,EXTDATA_CF,rc=status)
   VERIFY_(STATUS)

   totalPrimaryEntries=0
   totalDerivedEntries=0
   totalMaskEntries=0
   call ESMF_ConfigNextLine(CFtemp,__RC__)
   do while (status == ESMF_SUCCESS) 
      call ESMF_ConfigNextLine(CFtemp,rc=status)
      if (status == ESMF_SUCCESS) then 
         call ESMF_ConfigGetAttribute(CFtemp,thisLine,rc=status)
         VERIFY_(STATUS)
         if (trim(thisLine) == "PrimaryExports%%" .or. trim(thisLine) == "DerivedExports%%" .or. trim(thisLine) == "Masks%%") then
             call advanceAndCount(CFtemp,nLines,rc=status)
             VERIFY_(STATUS)
            select case (trim(thisLine))
               case ("PrimaryExports%%")
                   totalPrimaryEntries = totalPrimaryEntries + nLines
               case ("DerivedExports%%")
                   totalDerivedEntries = totalDerivedEntries + nLines
               case ("Masks%%")
                  totalMaskEntries = totalMaskEntries + nLines
            end select
         end if
      end if
   enddo
   ! destroy the config and reopen since there is no rewind function
   call ESMF_ConfigDestroy(CFtemp,rc=status)
   VERIFY_(STATUS)

   primary%nItems = totalPrimaryEntries
   if (totalPrimaryEntries > 0) then
      allocate (PrimaryVarNames(totalPrimaryEntries), stat=STATUS)
      VERIFY_(STATUS)
      allocate (PrimaryVarNeeded(totalPrimaryEntries), stat=STATUS)
      VERIFY_(STATUS)
      PrimaryVarNeeded = .false.
      allocate(primary%item(totalPrimaryEntries), stat=STATUS)
      VERIFY_(STATUS)
   end if
   
   derived%nItems = totalDerivedEntries
   if (totalDerivedEntries > 0) then 
      Allocate(DerivedVarNeeded(totalDerivedEntries),stat=status)
      VERIFY_(STATUS)
      DerivedVarNeeded = .false.
      allocate(derived%item(totalDerivedEntries),stat=status)
      VERIFY_(STATUS) 
   end if

   mask%nItems = totalMaskEntries
   if (totalMaskEntries > 0) then
      allocate(mask%item(totalMaskEntries),stat=status)
      VERIFY_(STATUS)
      allocate(maskVarNeeded(totalMaskEntries),stat=status)
      VERIFY_(STATUS)
      maskVarNeeded = .false.
      allocate(maskVarNames(totalMaskEntries),stat=status)
      VERIFY_(STATUS)
   end if
!  Primary Exports
!  ---------------

   totalPrimaryEntries = 0
   totalDerivedEntries = 0
   totalMaskEntries = 0
   ! reload file and parse it
   CFtemp = ESMF_ConfigCreate (rc=STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigLoadFile(CFtemp,EXTDATA_CF,rc=status)
   VERIFY_(STATUS)
   call ESMF_ConfigNextLine(CFtemp,__RC__)
   do while(status == ESMF_SUCCESS) 

      call ESMF_ConfigNextLine(CFtemp,rc=status)
      if (status == ESMF_SUCCESS) then
         call ESMF_ConfigGetAttribute(CFtemp,thisLine,rc=status)
         if (trim(thisLine) == "PrimaryExports%%" .or. trim(thisLine) == "DerivedExports%%" .or. trim(thisLine) == "Masks%%") then
            select case (trim(thisLine))
               case ("PrimaryExports%%")
                  inBlock = .true.
                  do while(inBLock)
                     call ESMF_ConfigNextLine(CFtemp, __RC__)
                     call ESMF_ConfigGetAttribute(CFtemp,thisLine,__RC__)
                     if (trim(thisLine) == "%%") then
                        inBlock = .false.
                     else

                        totalPrimaryEntries = totalPrimaryEntries + 1
                        ! name entry
                        primary%item(totalPrimaryEntries)%name = trim(thisLine)
                        !call ESMF_ConfigGetAttribute(CFtemp, primary%item(totalPrimaryEntries)%name,  __RC__)
                        PrimaryVarNames(totalPrimaryEntries) = primary%item(totalPrimaryEntries)%name
                        ! check if this represents a vector by looking for semicolon
                        primary%item(totalPrimaryEntries)%isVector = ( index(primary%item(totalPrimaryEntries)%name,';').ne.0 )
                        primary%item(totalPrimaryEntries)%vartype = MAPL_ExtDataVectorItem
                        primary%item(totalPrimaryEntries)%foundComp2 = .false.
                        primary%item(totalPrimaryEntries)%foundComp1 = .false.

                        ! units entry
                        call ESMF_ConfigGetAttribute(CFtemp, primary%item(totalPrimaryEntries)%units, __RC__)

                        ! horizontal information entry
                        call ESMF_ConfigGetAttribute(CFtemp, buffer, __RC__)
                        call ESMF_StringLowerCase(buffer, iret)
            
                        if (buffer == 'xy') then
                           primary%item(totalPrimaryEntries)%dim = MAPL_DimsHorzOnly
                        else if (buffer == 'xyz') then
                           primary%item(totalPrimaryEntries)%dim = MAPL_DimsHorzVert
                        else
                           __raise__(MAPL_RC_ERROR, "invalid dimension for Primary Export")
                        end if

                        ! vertical information entry
                        call ESMF_ConfigGetAttribute(CFtemp, buffer, __RC__)
                        call ESMF_StringLowerCase(buffer, iret)
                        if (buffer == 'c') then
                           primary%item(totalPrimaryEntries)%vloc = MAPL_VLocationCenter
                        else if (buffer == 'e') then
                           primary%item(totalPrimaryEntries)%vloc = MAPL_VLocationEdge
                        else
                           __raise__(MAPL_RC_ERROR, "invalid vLocation for Primary Export")
                        end if
                  
                        ! climatology entry
                        call ESMF_ConfigGetAttribute(CFtemp, buffer, __RC__)
                        call ESMF_StringLowerCase(buffer, iret)
                        if (trim(buffer) == 'n') then
                           primary%item(totalPrimaryEntries)%cyclic = "n"
                        else if (trim(buffer) == 'y') then
                           primary%item(totalPrimaryEntries)%cyclic = "y"
                        else if (index(buffer,'diurnal') .ne. 0) then
                           primary%item(totalPrimaryEntries)%cyclic = "diurnal"
                        else
                           __raise__(MAPL_RC_ERROR, "the cyclic keyword for extdata primary export must be y, n, or diurnal")
                        end if

                        ! conservative entry
                        call ESMF_ConfigGetAttribute(CFtemp, buffer, __RC__)
                        call ESMF_StringLowerCase(buffer, iret)
                        if (trim(buffer) == 'y') then
                           primary%item(totalPrimaryEntries)%conservative = .true.
                        else if (trim(buffer) == 'n') then
                           primary%item(totalPrimaryEntries)%conservative = .false.
                        else
                           __raise__(MAPL_RC_ERROR, "the conservative keyword for extdata primary export must be y or n")
                        end if

                        ! refresh template entry
                        call ESMF_ConfigGetAttribute(CFtemp, primary%item(totalPrimaryEntries)%refresh_template, __RC__)
                        ! offset entry
                        call ESMF_ConfigGetAttribute(CFtemp, c_offset, __RC__)
                        if (trim(c_offset) == "none") then
                           primary%item(totalPrimaryEntries)%do_offset = .false.
                        else
                           primary%item(totalPrimaryEntries)%do_offset = .true.
                           read(c_offset,*,iostat=ios) primary%item(totalPrimaryEntries)%offset
                        end if
                        ! scaling entry
                        call ESMF_ConfigGetAttribute(CFtemp, c_scale, __RC__)
                        if (trim(c_scale) == "none") then
                           primary%item(totalPrimaryEntries)%do_scale = .false.
                        else
                           primary%item(totalPrimaryEntries)%do_scale = .true.
                           read(c_scale,*,iostat=ios) primary%item(totalPrimaryEntries)%scale
                        end if
                     
                        ! variable name on file entry
                        call ESMF_ConfigGetAttribute(CFtemp, primary%item(totalPrimaryEntries)%var,    __RC__)
                        ! file template entry
                        call ESMF_ConfigGetAttribute(CFtemp, primary%item(totalPrimaryEntries)%file,   __RC__)

                        ! the next  three are optional entries to describe the time information about the file template
                        ! these are what is the first valid time you can apply to the file template to get a file that exists
                        ! then you can specify the frequnecy of the file and the units of the frequency
                        call ESMF_ConfigGetAttribute(CFtemp, primary%item(totalPrimaryEntries)%fileReffTime, rc=status)
                        if (status /= ESMF_SUCCESS) then
                           primary%item(totalPrimaryEntries)%FileReffTime = ""
                           primary%item(totalPrimaryEntries)%hasFileReffTime = .false.
                        else
                           primary%item(totalPrimaryEntries)%hasFileReffTime = .true.
                        end if            
 
              !         assume we will allocate
                        primary%item(totalPrimaryEntries)%ExtDataAlloc = .true.
              !         check if this is going to be a constant
                        primary%item(totalPrimaryEntries)%isConst = .false.
                        if (primary%item(totalPrimaryEntries)%file(1:9) == '/dev/null') then
                           primary%item(totalPrimaryEntries)%isConst = .true.
                           ios = -1
                           k = index(primary%item(totalPrimaryEntries)%file,':')
                           if ( k > 9 ) then
                                read(primary%item(totalPrimaryEntries)%file(k+1:),*,iostat=ios) primary%item(totalPrimaryEntries)%const
                           end if
                           if ( ios /= 0 ) primary%item(totalPrimaryEntries)%const = 0.0
                        end if


                        if ( primary%item(totalPrimaryEntries)%isConst .eqv. .false. )  then
                           call CreateTimeInterval(primary%item(totalPrimaryEntries),clock,__RC__)
                        end if
                     end if
                  enddo
               !  Derived Exports
               !  ---------------
               case ("DerivedExports%%")
                  inBlock = .true.
                  do while(inBlock)
                     call ESMF_ConfigNextLine(CFtemp, __RC__)
                     call ESMF_ConfigGetAttribute(CFtemp,thisLine,__RC__)
                     if (trim(thisLine) == "%%") then
                        inBlock = .false.
                     else
                        totalDerivedEntries = totalDerivedEntries + 1
                        derived%item(totalDerivedEntries)%name = trim(thisLine)
                        !call ESMF_ConfigGetAttribute(CFtemp,derived%item(totalDerivedEntries)%name,__RC__)
                        call ESMF_ConfigGetAttribute(CFtemp,derived%item(totalDerivedEntries)%expression,__RC__)
                        call ESMF_ConfigGetAttribute(CFtemp,derived%item(totalDerivedEntries)%refresh_template, __RC__)
                        derived%item(totalDerivedEntries)%ExtDataAlloc = .true.
                     end if
                  enddo
               !  Masks
               !  ---------------
               case ("Masks%%")
                  inBlock = .true.
                  do while(inBlock)
                     call ESMF_ConfigNextLine(CFtemp, __RC__)
                     call ESMF_ConfigGetAttribute(CFtemp,thisLine,__RC__)
                     if (trim(thisLine) == "%%") then
                        inBlock = .false.
                     else

                        totalMaskEntries = totalMaskEntries + 1
                        Mask%item(totalMaskEntries)%name=trim(thisLine)
                        !call ESMF_ConfigGetAttribute(CFtemp, Mask%item(totalMaskEntries)%name,  __RC__)
                        call ESMF_ConfigGetAttribute(CFtemp, Mask%item(totalMaskEntries)%var,  __RC__)
                        call ESMF_ConfigGetAttribute(CFtemp, Mask%item(totalMaskEntries)%file, __RC__)
                        MaskVarNames(totalMaskEntries) = Mask%item(totalMaskEntries)%name
                     end if
                  end do
            end select
         end if
      end if
   end do
   call ESMF_ConfigDestroy(CFtemp,__RC__)
   !Done parsing resource file    

   PrimaryItemCount = 0
   DerivedItemCount = 0
   MaskItemCount    = 0
   itemCounter = 0

!   find items in primary and derived to fullfill Export state
!   once we find primary or derived put in namespace
    self%ExtDataState = ESMF_StateCreate(Name="ExtDataNameSpace",__RC__)
    do I = 1, ItemCount

       found = .false.
       do J = 1, primary%nItems
          ! special handling if it is a vector
          if (primary%item(J)%isVector) then
             idx = index(primary%item(J)%name,";")
             component1 = primary%item(J)%name(1:idx-1)
             component2 = primary%item(J)%name(idx+1:)
             if ( trim(ItemNames(I)) == trim(component1) ) then
                primary%item(j)%vcomp1 = component1
                idx = index(primary%item(j)%var,";")
                primary%item(j)%fcomp1 = primary%item(j)%var(1:idx-1)
                itemCounter = itemCounter + 1
                found = .true.
                primary%item(j)%foundComp1 = .true.
                PrimaryVarNeeded(j) = .true.
                primary%item(j)%ExtDataAlloc = .false.
                if ( primary%item(j)%foundComp1 .and. primary%item(j)%foundComp2 ) PrimaryItemCount = PrimaryItemCount + 1
                call ESMF_StateGet(Export,component1,field,__RC__)
                call MAPL_StateAdd(self%ExtDataState,field,__RC__)
                ! put protection in, if you are filling vector pair, they must be fields, no bundles
                ASSERT_( ITEMTYPES(I) == ESMF_StateItem_Field )
                exit
             else if ( trim(ItemNames(I)) == trim(component2) ) then
                primary%item(j)%vcomp2 = component2
                idx = index(primary%item(j)%var,";")
                primary%item(j)%fcomp2 = primary%item(j)%var(idx+1:)
                itemCounter = itemCounter + 1
                found = .true.
                primary%item(j)%foundComp2 = .true.
                PrimaryVarNeeded(j) = .true.
                primary%item(j)%ExtDataAlloc = .false.
                if ( primary%item(j)%foundComp1 .and. primary%item(j)%foundComp2 ) PrimaryItemCount = PrimaryItemCount + 1
                call ESMF_StateGet(Export,component2,field,__RC__)
                call MAPL_StateAdd(self%ExtDataState,field,__RC__)
                ! put protection in, if you are filling vector pair, they must be fields, no bundles
                ASSERT_( ITEMTYPES(I) == ESMF_StateItem_Field )
                exit
             end if
          else
             if (ItemNames(I) == primary%item(J)%name) then
                itemCounter = itemCounter + 1
                found = .true.
                if (primary%item(j)%isConst .and. ITEMTYPES(I) == ESMF_StateItem_FieldBundle) then
                   if (mapl_am_I_root()) write(*,*)'Can not have constant bundle in ExtData.rc file'
                   ASSERT_(.false.)
                end if
                PrimaryItemCount = PrimaryItemCount + 1
                PrimaryVarNeeded(j) = .true.
                primary%item(j)%ExtDataAlloc = .false.
                VarName=trim(primary%item(J)%name)
    
                if (ITEMTYPES(I) == ESMF_StateItem_Field) then
                   primary%item(J)%vartype = MAPL_FieldItem
                   call ESMF_StateGet(Export,VarName,field,__RC__)
                   call MAPL_StateAdd(self%ExtDataState,field,__RC__)
                else if (ITEMTYPES(I) == ESMF_StateItem_FieldBundle) then
                   primary%item(J)%vartype = MAPL_BundleItem
                   call ESMF_StateGet(Export,VarName,bundle,__RC__)
                   call MAPL_StateAdd(self%ExtDataState,bundle,__RC__)
                end if
                exit

             end if
          end if
       end do
       if ( (.not.found) .and. (derived%nItems > 0) ) then
          do J = 1, derived%nItems
             if (ItemNames(I) == derived%item(J)%name) then

                if (ITEMTYPES(I) == ESMF_StateItem_FieldBundle) then
                   ASSERT_(.false.)
                end if
                found = .true.
                DerivedVarNeeded(j) = .true.
                itemCounter = itemCounter + 1
                DerivedItemCount = DerivedItemCount + 1
                derived%item(j)%ExtDataAlloc = .false.
                VarName=derived%item(j)%name
                call ESMF_StateGet(Export,VarName,field,__RC__)
                call MAPL_StateAdd(self%ExtDataState,field,__RC__)
                exit

             end if
          end do
       end if
       if ( (.not.found) .and. (mask%nItems > 0) ) then
          do J = 1, mask%nItems
             if (ItemNames(I) == mask%item(J)%name) then
                found = .true.
                MaskVarNeeded(j) = .true.
                itemCounter = itemCounter + 1
                MaskItemCount = MaskItemCount + 1
                mask%item(j)%ExtDataAlloc = .false.
                VarName=mask%item(j)%name
                call ESMF_StateGet(Export,VarName,field,__RC__)
                call MAPL_StateAdd(self%ExtDataState,field,__RC__)
                exit
             end if
          end do
       end if
       if (.not.found) then
          if (mapl_am_I_root()) then
             write(*,*)'ExtData could not satisfy import ',trim(ItemNames(I))
          end if
       end if
    end do

    call ESMF_VMGetCurrent(VM) 
    call ESMF_VMBarrier(VM)
!   we have better found all the items in the export in either a primary or derived item
    if ( itemCounter /= ItemCount ) then
       if (mapl_am_I_root()) then
          write(*,'(A6,I3,A31)')'Found ',ItemCount-itemCounter,' unfullfilled imports in extdata'
       end if
        ASSERT_(.false.)
    end if

    NumVarNames=primary%nItems+mask%nItems
    allocate(VarNames(NumVarNames))
    allocate(LocalVarNeeded(NumVarNames))
    do i=1,primary%nItems
       VarNames(i)=PrimaryVarNames(i)
    end do
    do i=primary%nItems+1,NumVarNames
       VarNames(i)=MaskVarNames(i-primary%nItems)
    end do

    IF (MAPL_am_I_root()) THEN
       PRINT*, '************************************************'
       PRINT*, '** Variables to be provided by the ExtData Component **'
       PRINT*, '************************************************'
       do I = 1, ItemCount
          PRINT*, '---- ', I, ': ', TRIM(ItemNames(I))
       END DO
       PRINT*, '************************************************'
       PRINT*
    END IF

!   search for other primary variables we may need to fill derived types that were not in the export state
!   if we find them allocate them based on grid of variable we are trying to fill
    do i=1, derived%nItems
       if (DerivedVarNeeded(i)) then
          LocalVarNeeded=.false.

          ! first check if it is a non-arithmetic function
          expression = derived%item(i)%expression
          call ESMF_StringLowerCase(expression, iret)
          if ( index(expression,"mask") /=0  ) then
             derived%item(i)%masking = .true.
          else
             derived%item(i)%masking = .false.
          end if
          if (derived%item(i)%masking) then
             call GetMaskName(derived%item(i)%expression,VarNames,LocalVarNeeded,__RC__)
          else
             call CheckSyntax(derived%item(i)%expression,VarNames,LocalVarNeeded,__RC__)
          end if

          do j=1, primary%nItems
             if (LocalVarNeeded(j)) then
                VarName = trim(primary%item(j)%name)
                call ESMF_StateGet(self%ExtDataState,VarName,field,rc=status)
                if (status /= ESMF_SUCCESS) then
                   VarName = trim(derived%item(i)%name)
                   call ESMF_StateGet(self%ExtDataState,VarName,field,__RC__)
                   VarName=trim(primary%item(j)%name)
                   fieldnew = MAPL_FieldCreate(field,varname,doCopy=.true.,__RC__) 
                   call MAPL_StateAdd(self%ExtDataState,fieldnew,__RC__)
                   PrimaryVarNeeded(j) = .true.
                   primary%item(j)%ExtDataAlloc = .true.
                   primary%item(j)%vartype = MAPL_FieldItem
                   PrimaryItemCount = PrimaryItemCount + 1
                end if
             end if
          end do
          do j=primary%nItems+1,NumVarNames
             if (LocalVarNeeded(j)) then
                jmask = j - primary%nItems
                VarName = trim(mask%item(jmask)%name)
                call ESMF_StateGet(self%ExtDataState,VarName,field,rc=status)
                if (status /= ESMF_SUCCESS) then
                   VarName = trim(derived%item(i)%name)
                   call ESMF_StateGet(self%ExtDataState,VarName,field,__RC__)
                   call ESMF_FieldGet(field,grid=grid,__RC__)
                   call ESMF_AttributeGet(FIELD, NAME='PRECISION', VALUE=KND,RC=STATUS)
                   if (status /= ESMF_SUCCESS) knd=kind(0.0)
                   VarName=trim(mask%item(jmask)%name)
                   field = mapl_FieldCreateEmpty(VarName,grid,__RC__)
                   call MAPL_FieldAllocCommit(field,dims=MAPL_DimsHorzOnly,location=MAPL_VlocationNone &
                      ,typekind=knd,hw=0,__RC__)
                   call MAPL_StateAdd(self%ExtDataState,field,__RC__)
                   MaskVarNeeded(jmask) = .true.
                   mask%item(jmask)%ExtDataAlloc = .true.
                   MaskItemCount = MaskItemCount + 1
                end if
             end if
          end do
       end if
    end do

    self%primary%nItems = count(PrimaryVarNeeded)
    if (DerivedItemCount > 0) self%derived%nItems = count(DerivedVarNeeded)
    if (MaskItemCount > 0) self%mask%nItems    = count(MaskVarNeeded)

    allocate(self%primary%item(PrimaryItemCount),__STAT__)
    if (DerivedItemCount > 0) allocate(self%derived%item(DerivedItemCount),__STAT__)
    if (MaskItemCount > 0) allocate(self%mask%item(MaskItemCount),__STAT__)

    counter = 0
    do i=1,primary%nItems
       if (PrimaryVarNeeded(i)) then
          counter = counter + 1
          self%primary%item(counter) = primary%item(i)
          ! put in a special check if it is a vector item
          ! both components must have bubbled up
          if (self%primary%item(counter)%isVector) then
             ASSERT_( self%primary%item(counter)%foundComp1 .and. self%primary%item(counter)%foundComp2 )
          end if
       end if
    end do
    ASSERT_(counter==PrimaryItemCount)

    if (DerivedItemCount > 0) then
       counter = 0
       do i=1,derived%nItems
          if (derivedVarNeeded(i)) then
             counter = counter + 1
             self%derived%item(counter) = derived%item(i)
          end if
       end do
       ASSERT_(counter==DerivedItemCount)
    end if

    if (MaskItemCount > 0) then
       counter = 0
       do i=1,mask%nItems
          if (maskVarNeeded(i)) then
             counter = counter + 1
             self%mask%item(counter) = mask%item(i)
          end if
       end do
       ASSERT_(counter==MaskItemCount)
    end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  Read the masks
   do i=1,self%mask%nItems
      call MAPL_GetPointer(self%ExtDataState, ptr2d, trim(self%mask%item(i)%name),__RC__)
      call ESMF_StateGet(self%ExtDataState, trim(self%mask%item(i)%name), field,__RC__)
      call ESMF_FieldGet(field,grid=grid,__RC__)
      time = MAPL_ExtDataGetFStartTime(trim(self%mask%item(i)%file),__RC__)
      call MAPL_CFIORead(trim(self%mask%item(i)%var), trim(self%mask%item(i)%file), time, grid, ptr2d, &
           time_interp=.false.,conservative=.true.,voting=.true.,__RC__)

   enddo

!  If this is a bundle prepare the bundle using 
!  a cfio no read call
   do i = 1, self%primary%nItems

      item => self%primary%item(i)

      ! parse refresh template to see if we have a time shift during constant updating
      k = index(item%refresh_template,';')
      call ESMF_TimeIntervalSet(item%tshift,__RC__)
      if (k.ne.0) then
         ASSERT_(trim(item%refresh_template(:k-1))=="0")
         if (item%refresh_template(k+1:k+1) == '-' ) then
            isNegative = .true.
            read(item%refresh_template(k+2:),*,iostat=ios)nhms
         else
            isNegative = .false.
            read(item%refresh_template(k+1:),*,iostat=ios)nhms
         end if
         call MAPL_UnpackTime(nhms,ihr,imn,isc)
         if (isNegative) then
            ihr = -ihr
            imn = -imn
            isc = -isc
         end if
         call ESMF_TimeIntervalSet(item%tshift,h=ihr,m=imn,s=isc,__RC__)
         item%refresh_template = "0"
      end if
      call SetRefreshAlarms(clock,primaryItem=item,__RC__)

      if (item%vartype == MAPL_BundleItem) then

         call ESMF_ClockGet(CLOCK, currTIME=time, __RC__)
         call ESMF_StateGet(self%ExtDataState, trim(item%name), bundle,__RC__)
         ! let us check that bundle is empty
         call ESMF_FieldBundleGet(bundle, fieldcount = fieldcount , __RC__)
         ASSERT_(fieldcount == 0)
         call MAPL_CFIORead(item%file,time,bundle,noread=.true.,ignorecase=self%ignorecase, only_vars=item%var,__RC__)

      end if

!  Read the single step files (read interval equal to zero)
!  --------------------------------------------------------

      if (item%isConst) then

         if (item%vartype == MAPL_FieldItem) then
            call ESMF_StateGet(self%ExtDataState,trim(item%name),field,__RC__)
            call ESMF_FieldGet(field,dimCount=fieldRank,__RC__)
            if (fieldRank == 2) then
                  call MAPL_GetPointer(self%ExtDataState, ptr2d, trim(item%name),__RC__)
                  ptr2d = item%const
            else if (fieldRank == 3) then
                  call MAPL_GetPointer(self%ExtDataState, ptr3d, trim(item%name), __RC__)
                  ptr3d = item%const
            endif
         else if (item%vartype == MAPL_BundleItem) then
            ASSERT_(.false.)
         else if (item%vartype == MAPL_ExtDataVectorItem) then
            ! add this capability later
            ASSERT_(.false.)
         end if
         cycle
      end if
 
      ! check if this is a single piece of data if user put - for refresh template
      ! by that it is an untemplated file with one time that could not possibly be time interpolated
      if (PrimaryExportIsConstant_(item)) then
         if (index(item%file,'%') == 0) then
            cfio = ESMF_CFIOCreate(cfioObjname='cfio_obj',__RC__)
            call ESMF_CFIOSet(cfio,fName=trim(item%file),__RC__)
            call ESMF_CFIOFileOpen(cfio,fmode=1,__RC__)
            if (cfio%tsteps == 1) then
               item%cyclic = 'single'
            end if
         end if
      end if

      ! get clim year if this is cyclic
      if (trim(item%cyclic)=='y') then
         call GetClimYear(item,__RC__)
      end if

      IsDiurnal: if ( trim(item%cyclic) == "diurnal" ) then
  
         call ESMF_ClockGet(CLOCK, currTIME=time, __RC__) 
         call ESMF_TimeGet(time, YY=YY, MM=MM, DD=DD,__RC__) 
         call MAPL_PackTime(curDate,yy,mm,dd)
         curTime = 0
         call gx_(file_processed,item%file,nymd=curDate,nhms=curTime,__STAT__)
         cfio = ESMF_CFIOCreate(cfioObjName='cfio_obj',__RC__)
         call ESMF_CFIOSet(cfio,fName=trim(file_processed),__RC__)
         call ESMF_CFIOFileOpen(cfio,fmode=1,__RC__)
         call GetBegDateTime(cfio%fid,begDate,begTime,incSecs,__RC__)
         item%diurnal_data%ntimes = cfio%tsteps
         ! get total length of time on the file, it had better be 24 hours since this is a diurnal cycle
         fHours = (incSecs/60/60)*cfio%tsteps
         ASSERT_(fHours == 24)

         allocate(item%diurnal_data%times(item%diurnal_data%ntimes),__STAT__)
         do j=1,item%diurnal_data%ntimes
            tInterval = incSecs*(j-1)
            call GetDate(begDate,0,tInterval,nymdB,nhmsB,status)
            item%diurnal_data%times(j) = nhmsB
         enddo

         ! get the grid from the variable 
         if (item%vartype == MAPL_FieldItem) then
            call ESMF_StateGet(self%ExtDataState, trim(item%name), field,__RC__)
            call ESMF_FieldGet(field,grid=grid,__RC__)
            dims = item%dim
            vloc = item%vloc
            knd  = kind(0.0)
            allocate(item%diurnal_data%field1(item%diurnal_data%ntimes),__STAT__)
            do j=1,item%diurnal_data%ntimes
               item%diurnal_data%field1(j) = mapl_FieldCreateEmpty(item%var,grid,__RC__)
               call MAPL_FieldAllocCommit(item%diurnal_data%field1(j),dims=dims,location=vloc,typekind=knd,hw=0,__RC__)
            enddo
         else if (item%vartype == MAPL_BundleItem) then
            ASSERT_(.false.)
         end if

      else

         if (item%vartype == MAPL_FieldItem) then

            call ESMF_StateGet(self%ExtDataState, trim(item%name), field,__RC__)
            
            item%finterp1 = MAPL_FieldCreate(field,item%var,doCopy=.true.,__RC__)
            item%finterp2 = MAPL_FieldCreate(field,item%var,doCopy=.true.,__RC__)

         else if (item%vartype == MAPL_BundleItem) then

            call ESMF_StateGet(self%ExtDataState, trim(item%name), bundle,__RC__)
            call ESMF_FieldBundleGet(bundle,grid=grid,__RC__)
            call ESMF_ClockGet(CLOCK, currTIME=time, __RC__)
            item%binterp1 = ESMF_FieldBundleCreate( __RC__)
            call ESMF_FieldBundleSet(item%binterp1, GRID=GRID, __RC__)
            item%binterp2 = ESMF_FieldBundleCreate( __RC__)
            call ESMF_FieldBundleSet(item%binterp2, GRID=GRID, __RC__)
            call MAPL_CFIORead(item%file,time,item%binterp1,noread=.true.,ignorecase=self%ignorecase,only_vars=item%var,__RC__)
            call MAPL_CFIORead(item%file,time,item%binterp2,noread=.true.,ignorecase=self%ignorecase,only_vars=item%var,__RC__)
    
         else if (item%vartype == MAPL_ExtDataVectorItem) then
        
            ! check that we are not asking for conservative regridding
            if (item%conservative) then
               if (mapl_am_i_root()) write(*,*)"No conservative regridding with vectors"
               ASSERT_(.false.)
            end if 

            call ESMF_StateGet(self%ExtDataState, trim(item%vcomp1), field,__RC__)
            item%v1_finterp1 = MAPL_FieldCreate(field, item%vcomp1,doCopy=.true.,__RC__)
            item%v1_finterp2 = MAPL_FieldCreate(field, item%vcomp1,doCopy=.true.,__RC__)
            call ESMF_StateGet(self%ExtDataState, trim(item%vcomp2), field,__RC__)
            item%v2_finterp1 = MAPL_FieldCreate(field, item%vcomp2,doCopy=.true.,__RC__)
            item%v2_finterp2 = MAPL_FieldCreate(field, item%vcomp2,doCopy=.true.,__RC__)

         end if

      end if IsDiurnal

      allocate(item%refresh_time,__STAT__)

      call ESMF_TimeSet(item%refresh_time, yy=0, __RC__)
   end do

   do i =1, self%derived%nItems
      allocate(self%derived%item(i)%refresh_time,__STAT__)

      derivedItem => self%derived%item(i)

      ! parse refresh template to see if we have a time shift during constant updating
      k = index(derivedItem%refresh_template,';')
      call ESMF_TimeIntervalSet(derivedItem%tshift,__RC__)
      if (k.ne.0) then
         ASSERT_(trim(derivedItem%refresh_template(:k-1))=="0")
         if (derivedItem%refresh_template(k+1:k+1) == '-' ) then
            isNegative = .true.
            read(derivedItem%refresh_template(k+2:),*,iostat=ios)nhms
         else
            isNegative = .false.
            read(derivedItem%refresh_template(k+1:),*,iostat=ios)nhms
         end if
         call MAPL_UnpackTime(nhms,ihr,imn,isc)
         if (isNegative) then
            ihr = -ihr
            imn = -imn
            isc = -isc
         end if
         call ESMF_TimeIntervalSet(derivedItem%tshift,h=ihr,m=imn,s=isc,__RC__)
         derivedItem%refresh_template = "0"
      end if

      call SetRefreshAlarms(clock,derivedItem=derivedItem,__RC__)

      call ESMF_TimeSet(self%derived%item(i)%refresh_time, yy=0, __RC__)
   end do

#ifdef DEBUG
   if (MAPL_AM_I_ROOT()) then
      print *, trim(Iam)//': IMPORT   State during Initialize():'
      call ESMF_StatePrint ( IMPORT )
      print *
      print *, trim(Iam)//': EXPORT   State during Initialize():' 
      call ESMF_StatePrint ( EXPORT )
   end if
#endif

! Clean up
! --------
   if (associated(primary%item)) deallocate(primary%item)
   if (associated(derived%item)) deallocate(derived%item)
   deallocate(ItemTypes)
   deallocate(ItemNames)
   if (allocated(PrimaryVarNames)) deallocate(PrimaryVarNames)
   if (allocated(PrimaryVarNeeded)) deallocate(PrimaryVarNeeded)
   if (allocated(VarNames)) deallocate(VarNames)
   if (allocated(DerivedVarNeeded)) deallocate(DerivedVarNeeded)
   if (allocated(LocalVarNeeded)) deallocate(LocalVarNeeded)
   if (associated(mask%item)) deallocate(mask%item)
   if (allocated(MaskVarNames)) Deallocate(MaskVarNames)
   if (allocated(MaskVarNeeded)) Deallocate(MaskVarNeeded)

!  Set has run to false to we know when we first go to run method it is first call
   hasRun = .false.
!  All done
!  --------
   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE Initialize_


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Run_ --- Runs MAPL_ExtData
!
! !INTERFACE:
!

   SUBROUTINE Run_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, intent(out) ::  rc                   ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Field)                  :: field       ! Field
   type(ESMF_FieldBundle)            :: bundle
   type(ESMF_Config)                 :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: status

   type(PrimaryExport), pointer      :: item
   type(DerivedExport), pointer      :: derivedItem
   integer                           :: i, j

   type(ESMF_Time)                   :: time, refresh_time, time0
   logical                           :: time_interp, time_is_cyclic
   type(MAPL_MetaComp), pointer      :: MAPLSTATE

   real, pointer, dimension(:,:)     :: var2d_prev, var2d_next
   real, pointer, dimension(:,:,:)   :: var3d_prev, var3d_next
   logical                           :: doUpdate
   integer                           :: fieldCount, fieldRank
   character(len=ESMF_MAXSTR), ALLOCATABLE  :: NAMES (:)
   type(ESMF_Field)                  :: field1, field2
   character(len=ESMF_MAXSTR)        :: file_processed, file_processed1, file_processed2
   integer                           :: NX, NY
   logical                           :: NotSingle
   logical                           :: updateL, updateR, swap

!  Declare pointers to IMPORT/EXPORT/INTERNAL states 
!  -------------------------------------------------
!  #include "MAPL_ExtData_DeclarePointer___.h"
  
!  Get my name and set-up traceback handle
!  ---------------------------------------
   Iam = 'Run_'
   call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)


!  Call Run for every Child
!  -------------------------
   call MAPL_GenericRun ( GC, IMPORT, EXPORT, CLOCK,  __RC__)


!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF, __RC__ )

   if (.not. self%active) then
      RETURN_(ESMF_SUCCESS)
   end if

   call MAPL_GetObjectFromGC ( gc, MAPLSTATE, RC=STATUS)
   VERIFY_(STATUS) 
   call MAPL_TimerOn(MAPLSTATE,"Run")

   call ESMF_ClockGet(CLOCK, currTIME=time0, __RC__)


!  Fill in the internal state with data from the files 
!  ---------------------------------------------------
 
   do i = 1, self%primary%nItems

      item => self%primary%item(i)

      if (item%isConst) cycle

      NotSingle = .true.
      if (trim(item%cyclic) == 'single') NotSingle = .false.

      call CheckUpdate(doUpdate,time,time0,hasRun,primaryItem=item,__RC__)

      DO_UPDATE: if (doUpdate) then

         HAS_RUN: if ( hasRun .eqv. .false.) then

            if (trim(item%cyclic) == "diurnal") then

               call MAPL_ExtDataUpdateDiurnalBracket(time,item,__RC__)

            else

               if (NotSingle) then
                  ! update left time
                  call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"L",item%cyclic,item%climYear,item%interp_time1, & 
                       item%time1,file_processed1,rc=status)
                  VERIFY_(status)

                  ! update right time
                  call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"R",item%cyclic,item%climYear,item%interp_time2, &
                       item%time2,file_processed2,rc=status)
                  VERIFY_(STATUS)
               else
                  ! just get time on the file
                  item%time1 = MAPL_ExtDataGetFStartTime(trim(item%file),__RC__)
                  item%interp_time1 = item%time1
                  file_processed1 = item%file
               end if

               ! read bracketing data

               if (item%vartype == MAPL_FieldItem) then

                  call MAPL_TimerOn(MAPLSTATE,"-Read")
                  call MAPL_CFIORead(trim(item%var), file_processed1, item%time1, item%finterp1, &
                       time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                       ignoreCase = self%ignoreCase,__RC__)
                  if (NotSingle) then
                     call MAPL_CFIORead(trim(item%var), file_processed2, item%time2, item%finterp2, &
                          time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                          ignoreCase = self%ignoreCase,__RC__)
                  end if
                  call MAPL_TimerOff(MAPLSTATE,"-Read")

               else if (item%vartype == MAPL_BundleItem) then

                  call MAPL_TimerOn(MAPLSTATE,"-Read")
                  call MAPL_CFIORead(file_processed1, item%time1, item%binterp1, &
                       time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                       ignoreCase = self%ignoreCase,__RC__)
                  if (NotSingle) then
                     call MAPL_CFIORead(file_processed2, item%time2, item%binterp2, &
                          time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                          ignoreCase = self%ignoreCase,__RC__)
                  end if
                  call MAPL_TimerOff(MAPLSTATE,"-Read")
             
               else if (item%vartype == MAPL_ExtDataVectorItem) then

                  call ESMF_ConfigGetAttribute(cf, value = NX, Label="NX:", __RC__)
                  call ESMF_ConfigGetAttribute(cf, value = NY, Label="NY:", __RC__)
                  call MAPL_ExtDataReadVector(file_processed1,item%fcomp1,item%fcomp2, &
                                              item%time1,item%v1_finterp1,item%v2_finterp1,NX,NY,self%ignoreCase,__RC__)
                  if (NotSingle) then
                     call MAPL_ExtDataReadVector(file_processed2,item%fcomp1,item%fcomp2, &
                                                 item%time2,item%v1_finterp2,item%v2_finterp2,NX,NY,self%ignoreCase,__RC__)
                  end if

               end if

            end if

         endif HAS_RUN
 
         ! now update bracketing times if neccessary

         NOT_SINGLE: if (NotSingle) then

            IS_DIURNAL: if (trim(item%cyclic) == "diurnal") then

                  call MAPL_ExtDataUpdateDiurnalBracket(time,item,__RC__)
            else

               if (time >= item%interp_time2) then
                  ! normal flow assume clock is moving forward
                  updateR = .true.
                  updateL = .false.
                  swap    = .true.
               else if (time < item%interp_time1) then
                  ! the can only happen if clock was rewound like in replay update both
                  updateR = .true.
                  updateL = .true.
                  swap    = .false.
               else
                  updateR = .false.
                  updateL = .false.
                  swap    = .false.
               end if

               DO_SWAP: if (swap) then

                  item%interp_time1 = item%interp_time2

                  if (item%vartype == MAPL_FieldItem) then

                     if (item%dim == MAPL_DimsHorzOnly) then
                        call ESMF_FieldGet(item%finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
                        call ESMF_FieldGet(item%finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
                        var2d_prev=var2d_next
                     else if (item%dim == MAPL_DimsHorzVert) then
                        call ESMF_FieldGet(item%finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
                        call ESMF_FieldGet(item%finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
                        var3d_prev=var3d_next
                     endif

                  else if (item%vartype == MAPL_BundleItem) then

                     call ESMF_FieldBundleGet(item%binterp2, fieldCount = fieldCount, __RC__)
                     allocate(names(fieldCount),__STAT__)
                     call ESMF_FieldBundleGet(item%binterp2, fieldNameList = Names, __RC__)
                     do j = 1,fieldCount
                        call ESMF_FieldBundleGet(item%binterp1, names(j), field=field1, __RC__)
                        call ESMF_FieldBundleGet(item%binterp2, names(j), field=field2, __RC__)
                        call ESMF_FieldGet(field1, dimCount=fieldRank, __RC__) 
                        if (fieldRank == 2) then
                           call ESMF_FieldGet(field1, localDE=0, farrayPtr=var2d_prev, __RC__)
                           call ESMF_FieldGet(field2, localDE=0, farrayPtr=var2d_next, __RC__)
                           var2d_prev=var2d_next
                        else if (fieldRank == 3) then
                           call ESMF_FieldGet(field1, localDE=0, farrayPtr=var3d_prev, __RC__)
                           call ESMF_FieldGet(field2, localDE=0, farrayPtr=var3d_next, __RC__)
                           var3d_prev=var3d_next
                        endif
                     enddo

                     deallocate(names)

                  else if (item%vartype == MAPL_ExtDataVectorItem) then

                     if (item%dim == MAPL_DimsHorzOnly) then
                        call ESMF_FieldGet(item%v1_finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
                        call ESMF_FieldGet(item%v1_finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
                        var2d_prev=var2d_next
                        call ESMF_FieldGet(item%v2_finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
                        call ESMF_FieldGet(item%v2_finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
                        var2d_prev=var2d_next
                     else if (item%dim == MAPL_DimsHorzVert) then
                        call ESMF_FieldGet(item%v1_finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
                        call ESMF_FieldGet(item%v1_finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
                        var3d_prev=var3d_next
                        call ESMF_FieldGet(item%v2_finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
                        call ESMF_FieldGet(item%v2_finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
                        var3d_prev=var3d_next
                     endif

                  end if

               end if DO_SWAP
 
               if (updateR) then

                  call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"R",item%cyclic,item%climYear,item%interp_time2, &
                       item%time2, file_processed, __RC__)

                  if (item%vartype == MAPL_FieldItem) then

                     call MAPL_TimerOn(MAPLSTATE,"-Read")
                     call MAPL_CFIORead(item%var, file_processed, item%time2, item%finterp2, &
                          time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                          ignoreCase = self%ignoreCase, __RC__)

                  else if (item%vartype == MAPL_BundleItem) then

                     call MAPL_CFIORead(file_processed, item%time2, item%binterp2, &
                        time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                        ignoreCase = self%ignoreCase, __RC__)  
                     call MAPL_TimerOff(MAPLSTATE,"-Read")

                  else if (item%vartype == MAPL_ExtDataVectorItem) then

                     call MAPL_TimerOn(MAPLSTATE,"-Read")
                     call ESMF_ConfigGetAttribute(cf, value = NX, Label="NX:", __RC__)
                     call ESMF_ConfigGetAttribute(cf, value = NY, Label="NY:", __RC__)
                     call MAPL_ExtDataReadVector(file_processed,item%fcomp1,item%fcomp2, & 
                                                 item%time2,item%v1_finterp2,item%v2_finterp2,&
                                                 NX,NY,self%ignoreCase,__RC__)
                     call MAPL_TimerOff(MAPLSTATE,"-Read")

                  end if

               end if
 
               if (updateL) then

                  call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"L",item%cyclic,item%climYear,item%interp_time1, &
                       item%time1, file_processed, __RC__)

                  if (item%vartype == MAPL_FieldItem) then

                     call MAPL_TimerOn(MAPLSTATE,"-Read")
                     call MAPL_CFIORead(item%var, file_processed, item%time1, item%finterp1, &
                          time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                          ignoreCase = self%ignoreCase, __RC__)
                     call MAPL_TimerOff(MAPLSTATE,"-Read")

                  else if (item%vartype == MAPL_BundleItem) then

                     call MAPL_TimerOn(MAPLSTATE,"-Read")
                     call MAPL_CFIORead(file_processed, item%time1, item%binterp1, &
                        time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                        ignoreCase = self%ignoreCase, __RC__)  
                     call MAPL_TimerOff(MAPLSTATE,"-Read")

                  else if (item%vartype == MAPL_ExtDataVectorItem) then

                     call ESMF_ConfigGetAttribute(cf, value = NX, Label="NX:", __RC__)
                     call ESMF_ConfigGetAttribute(cf, value = NY, Label="NY:", __RC__)
                     call MAPL_TimerOn(MAPLSTATE,"-Read")
                     call MAPL_ExtDataReadVector(file_processed,item%fcomp1,item%fcomp1, & 
                                                 item%time1,item%v1_finterp1,item%v2_finterp1,&
                                                 NX,NY,self%ignoreCase,__RC__)
                     call MAPL_TimerOff(MAPLSTATE,"-Read")

                  end if

               end if

            endif IS_DIURNAL

         endif NOT_SINGLE

         ! finally interpolate between bracketing times

         if (trim(item%cyclic) == "diurnal") then

            call MAPL_ExtDataDiurnalInterp(time,item,self%ExtDataState,__RC__)

         else
            if (item%vartype == MAPL_FieldItem) then

                  call ESMF_StateGet(self%ExtDataState, item%name, field, __RC__)
                  call MAPL_ExtDataInterpField(item,time,field,__RC__)

            else if (item%vartype == MAPL_BundleItem) then

                  call ESMF_StateGet(self%ExtDataState, item%name, bundle, __RC__)
                  call ESMF_FieldBundleGet(bundle, fieldCount = fieldCount, __RC__)
                  allocate(names(fieldCount),__STAT__)
                  call ESMF_FieldBundleGet(bundle, fieldNameList = Names, __RC__)
                  do j = 1,fieldCount
                     call ESMF_FieldBundleGet(bundle,names(j), field=field, __RC__)
                     call MAPL_ExtDataInterpField(item,time,field,__RC__)
                  enddo
                  deallocate(names)

            else if (item%vartype == MAPL_ExtDataVectorItem) then

                  call ESMF_StateGet(self%ExtDataState, item%vcomp1, field, __RC__)
                  call MAPL_ExtDataInterpField(item,time,field,vector_comp=1,__RC__)
                  call ESMF_StateGet(self%ExtDataState, item%vcomp2, field, __RC__)
                  call MAPL_ExtDataInterpField(item,time,field,vector_comp=2,__RC__)
    
            end if

         end if

      endif DO_UPDATE

      if (PrimaryExportIsConstant_(item) .and. associated(item%refresh_time)) then
         deallocate(item%refresh_time)
         item%refresh_time => null()
      end if
      nullify(item)

   end do

   ! now take care of derived fields
   do i=1,self%derived%nItems

      derivedItem => self%derived%item(i)

      call CheckUpdate(doUpdate,time,time0,hasRun,derivedItem=deriveditem,__RC__)

      if (doUpdate) then

         call CalcDerivedField(self%ExtDataState,self%primary,derivedItem%name,derivedItem%expression, &
              derivedItem%masking,__RC__)

      end if

      if (DerivedExportIsConstant_(derivedItem) .and. associated(derivedItem%refresh_time)) then
          deallocate(self%derived%item(i)%refresh_time)
          self%derived%item(i)%refresh_time => null()
      end if

   end do

!  All done
!  --------

   if (hasRun .eqv. .false.) hasRun = .true.
   call MAPL_TimerOff(MAPLSTATE,"Run")

   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE Run_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Finalize_ --- Finalize MAPL_ExtData
!
! !INTERFACE:
!

   SUBROUTINE Finalize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK      ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, intent(out) ::  rc                   ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Config)                 :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: status

   integer                           :: i


!  Get my name and set-up traceback handle
!  ---------------------------------------
   Iam = 'Finalize_'
   call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
   Iam = trim(comp_name) // trim(Iam)

!  Finalize MAPL Generic
!  ---------------------
   call MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK,  __RC__ )

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF, __RC__)

  
!  Free the memory used for the bracketing arrays
!  -----------------------------------------------------------
   if (self%active) then
      do i = 1, self%primary%nItems

         if (self%primary%item(i)%isConst) cycle

         if (trim(self%primary%item(i)%refresh_template) == "0") then
            if (self%primary%item(i)%vartype == MAPL_FieldItem) then
               call ESMF_FieldDestroy(self%primary%item(i)%finterp1,__RC__)
               call ESMF_FieldDestroy(self%primary%item(i)%finterp2,__RC__)
            end if
         end if

         if (associated(self%primary%item(i)%refresh_time)) then
            deallocate(self%primary%item(i)%refresh_time)
         end if

      end do

!  Free the memory used to hold the primary export items
!  -----------------------------------------------------
      if (associated(self%primary%item)) then
         deallocate(self%primary%item)
      end if
   end if


!  All done
!  --------
   RETURN_(ESMF_SUCCESS)

 end SUBROUTINE Finalize_

!.......................................................................

 subroutine extract_ ( GC, self, CF, rc)

    type(ESMF_GridComp), intent(INout)  :: GC           ! Grid Comp object

    type(MAPL_ExtData_state), pointer   :: self         ! Legacy state
    type(ESMF_Config),   intent(out)    :: CF           ! Universal Config 

    integer, intent(out), optional      :: rc

!                            ---

    character(len=ESMF_MAXSTR) :: comp_name
    character(len=ESMF_MAXSTR) :: Iam
    integer                    :: status

    type(MAPL_ExtData_Wrap)  :: wrap

!   Get my name and set-up traceback handle
!   ---------------------------------------
    Iam = 'extract_'
    call ESMF_GridCompGet( GC, NAME=comp_name, __RC__ )
    Iam = trim(COMP_NAME) // '::' // trim(Iam)

    rc = 0

!   Get my internal state
!   ---------------------
    call ESMF_UserCompGetInternalState(gc, 'MAPL_ExtData_state', WRAP, STATUS)
    VERIFY_(STATUS)
    self => wrap%ptr

!   Get the configuration
!   ---------------------
    call ESMF_GridCompGet ( GC, config=CF, __RC__ )

    
    RETURN_(ESMF_SUCCESS)

  end subroutine extract_
   
! ............................................................................

   logical function PrimaryExportIsConstant_(item)
   
      type(PrimaryExport), intent(in) :: item

      __Iam__('PrimaryExportIsConstant_')


      if ( trim(item%refresh_template) == '-' .or. &
           trim(item%file) == '/dev/null' ) then
          PrimaryExportIsConstant_ = .true. 
      else
          PrimaryExportIsConstant_ = .false.
      end if

   end function PrimaryExportIsConstant_

! ............................................................................

   logical function DerivedExportIsConstant_(item)
   
      type(DerivedExport), intent(in) :: item

      __Iam__('DerivedExportIsConstant_')


      if ( trim(item%refresh_template) == '-') then
          DerivedExportIsConstant_ = .true. 
      else
          DerivedExportIsConstant_ = .false.
      end if

   end function DerivedExportIsConstant_

! ............................................................................

  subroutine scale_field_(offset, scale_factor, field, rc)
     real, intent(in)                  :: scale_factor
     real, intent(in)                  :: offset
     type (ESMF_Field), intent(inout)  :: field
     integer, optional, intent (inout) :: rc

     __Iam__('scale_field_')

     integer       :: fieldRank
     real, pointer :: xy(:,:)    => null()
     real, pointer :: xyz(:,:,:) => null()


     call ESMF_FieldGet(field, dimCount=fieldRank, __RC__)

     ASSERT_(fieldRank == 2 .or. fieldRank == 3)

     if (fieldRank == 2) then
        call ESMF_FieldGet(field, farrayPtr=xy, __RC__)

        if (associated(xy)) then
           xy = offset + scale_factor*xy
        end if
     else if (fieldRank == 3) then
        call ESMF_FieldGet(field, farrayPtr=xyz, __RC__)

        if (associated(xyz)) then
           xyz = offset + scale_factor*xyz
        end if
     end if

     return 
  end subroutine scale_field_

! ............................................................................

  type (ESMF_Time) function timestamp_(time, template, rc)
     type(ESMF_Time), intent(inout)         :: time
     character(len=ESMF_MAXSTR), intent(in) :: template
     integer, optional, intent(inout)       :: rc 

      __Iam__('timestamp_')

     ! locals
     integer, parameter :: DATETIME_MAXSTR_ = 32
     integer :: yy, mm, dd, hs, ms, ss
     character(len=DATETIME_MAXSTR_) :: buff, buff_date, buff_time
     character(len=DATETIME_MAXSTR_) :: str_yy, str_mm, str_dd
     character(len=DATETIME_MAXSTR_) :: str_hs, str_ms, str_ss

     integer :: i, il, ir
     integer :: stat
    
     ! test the length of the timestamp template
     ASSERT_(len_trim(template) < DATETIME_MAXSTR_)

     buff = trim(template)
     call ESMF_StringLowerCase(buff, stat)
      
     ! test if the template is empty and return the current time as result
     if (buff == '-'  .or. buff == '--'   .or. buff == '---' .or. &
         buff == 'na' .or. buff == 'none' .or. buff == 'n/a') then

        timestamp_ = time
     else   
        ! split the time stamp template into a date and time strings
        i = scan(buff, 't')
        ASSERT_(i > 3)

        buff_date = buff(1:i-1)
        buff_time = buff(i+1:)

        ! parse the date string
        il = scan(buff_date, '-', back=.false.)
        ir = scan(buff_date, '-', back=.true. )
        str_yy = trim(buff_date(1:il-1))
        str_mm = trim(buff_date(il+1:ir-1))
        str_dd = trim(buff_date(ir+1:))

        ! parse the time string
        il = scan(buff_time, ':', back=.false.)
        ir = scan(buff_time, ':', back=.true. )
        str_hs = trim(buff_time(1:il-1))
        str_ms = trim(buff_time(il+1:ir-1))
        str_ss = trim(buff_time(ir+1:))
     
        ! remove the trailing 'Z' from the seconds string
        i = scan(str_ss, 'z')
        if (i > 0) then
           str_ss = trim(str_ss(1:i-1))
        end if

        ! apply the timestamp template
        call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=hs, m=ms, s=ss, __RC__)

        i = scan(str_yy, '%'); if (i == 0) read (str_yy, '(I4)'), yy
        i = scan(str_mm, '%'); if (i == 0) read (str_mm, '(I2)'), mm
        i = scan(str_dd, '%'); if (i == 0) read (str_dd, '(I2)'), dd
        i = scan(str_hs, '%'); if (i == 0) read (str_hs, '(I2)'), hs
        i = scan(str_ms, '%'); if (i == 0) read (str_ms, '(I2)'), ms
        i = scan(str_ss, '%'); if (i == 0) read (str_ss, '(I2)'), ss

        call ESMF_TimeSet(timestamp_, yy=yy, mm=mm, dd=dd, h=hs, m=ms, s=ss, __RC__)
     end if

     RETURN_(ESMF_SUCCESS)

  end function timestamp_
 
  subroutine CreateTimeInterval(item,clock,rc)
     type(PrimaryExport)      , intent(inout) :: item
     type(ESMF_Clock)          , intent(in   ) :: clock
     integer, optional         , intent(out  ) :: rc

     __Iam__('CreateTimeInterval')
     
     integer                    :: iyy,imm,idd,ihh,imn,isc
     integer                    :: lasttoken
     character(len=2)           :: token
     type(ESMF_Time)            :: time
     integer                    :: spos(2),cindex,pindex
     character(len=ESMF_MAXSTR) :: creffTime, ctInt
     
     creffTime = ''
     ctInt     = ''
     call ESMF_ClockGet (CLOCK, currTIME=time, __RC__)
     if (.not.item%hasFileReffTime) then
        ! if int_frequency is less than zero than try to guess it from the file template
        ! if that fails then it must be a single file or a climatology 

        call ESMF_TimeGet(time, yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,__RC__)
        lasttoken = index(item%file,'%',back=.true.)
        if (lasttoken.gt.0) then
           token = item%file(lasttoken+1:lasttoken+2)
           select case(token)
           case("y4") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=1,dd=1,h=0,m=0,s=0,__RC__)
              call ESMF_TimeIntervalSet(item%frequency,yy=1,__RC__)
           case("m2") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=1,h=0,m=0,s=0,__RC__)
              call ESMF_TimeIntervalSet(item%frequency,mm=1,__RC__)
           case("d2") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=idd,h=0,m=0,s=0,__RC__)
              call ESMF_TimeIntervalSet(item%frequency,d=1,__RC__)
           case("h2") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=0,s=0,__RC__)
              call ESMF_TimeIntervalSet(item%frequency,h=1,__RC__)
           case("n2") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=0,__RC__)
              call ESMF_TimeIntervalSet(item%frequency,m=1,__RC__)
           end select
        else
           ! couldn't find any tokens so all the data must be on one file
           call ESMF_TimeIntervalSet(item%frequency,__RC__)
        end if
     else
        ! Get refference time, if not provided use current model date
        pindex=index(item%FileReffTime,'P')
        if (pindex==0) then 
           ASSERT_(.false.)
        end if
        cReffTime = item%FileReffTime(1:pindex-1) 
        if (trim(cReffTime) == '') then
           item%reff_time = Time
        else
           call MAPL_NCIOParseTimeUnits(cReffTime,iyy,imm,idd,ihh,imn,isc,status)
           VERIFY_(STATUS)
           call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=isc,rc=status)
           VERIFY_(STATUS)
        end if
        ! now get time interval. Put 0000-00-00 in front if not there so parsetimeunits doesn't complain
        ctInt = item%FileReffTime(pindex+1:)
        cindex = index(ctInt,'T')
        if (cindex == 0) ctInt = '0000-00-00T'//trim(ctInt)
        call MAPL_NCIOParseTimeUnits(ctInt,iyy,imm,idd,ihh,imn,isc,status)
        VERIFY_(STATUS)
        call ESMF_TimeIntervalSet(item%frequency,yy=iyy,mm=imm,d=idd,h=ihh,m=imn,s=isc,rc=status)
        VERIFY_(STATUS) 
     end if

     RETURN_(ESMF_SUCCESS) 

  end subroutine CreateTimeInterval

  subroutine GetClimYear(item, rc)

     type(PrimaryExport)      , intent(inout) :: item
     integer, optional        , intent(out  ) :: rc

     __Iam__('GetClimYear')

     type(ESMF_CFIO)            :: cfio
     integer(ESMF_KIND_I4)      :: iyr,imm,idd
     integer                    :: begDate
     type(ESMF_TimeInterval)    :: zero
     integer                    :: lasttoken
     character(len=ESMF_MAXPATHLEN) :: file
     character(len=2)           :: token
     integer                    :: nymd, nhms


     call ESMF_TimeIntervalSet(zero,__RC__)
     cfio =  ESMF_CFIOCreate (cfioObjName='cfio_obj',__RC__)


     if (item%frequency == zero) then
        file = item%file 
     else
        lasttoken = index(item%file,'%',back=.true.)
        token = item%file(lasttoken+1:lasttoken+2)
        ASSERT_(token == "m2")
        ! just put a time in so we can evaluate the template to open a file
        nymd = 20000101
        nhms = 0
        call ESMF_CFIOStrTemplate(file,item%file,'GRADS',nymd=nymd,nhms=nhms,__STAT__)
     end if
     call ESMF_CFIOSet(CFIO, fName=trim(file),__RC__)
     call ESMF_CFIOFileOpen  (CFIO, FMODE=1,__RC__)
     begDate = cfio%date
     call MAPL_UnpackTime(begDate,iyr,imm,idd)
     item%climyear = iyr
     call ESMF_CFIODestroy(CFIO,__RC__)

  end subroutine GetClimYear

  subroutine UpdateBracketTime(file_tmpl,cTime,reffTime,frequency,bSide,cyclic,climYear,interpTime,fileTime,file_processed,rc)
     character(len=*          ),          intent(in   ) :: file_tmpl
     type(ESMF_Time),                     intent(inout) :: cTime
     type(ESMF_Time),                     intent(inout) :: reffTime
     type(ESMF_TimeInterval),             intent(inout) :: frequency
     character(len=1),                    intent(in   ) :: bSide
     character(len=*),                    intent(in   ) :: cyclic
     integer,                             intent(in   ) :: climYear
     type(ESMF_TIME),                     intent(inout) :: interpTime
     type(ESMF_TIME),                     intent(inout) :: fileTime
     character(len=*),                    intent(inout) :: file_processed
     integer, optional,                   intent(out  ) :: rc

     __Iam__('UpdateBracketTime')

     type(ESMF_Time)                            :: newTime
     integer                                    :: curDate,curTime,n
     integer(ESMF_KIND_I4)                      :: iyr, imm, idd, ihr, imn, isc, cYear,cMonth
     type(ESMF_TimeInterval)                    :: tint
     type(ESMF_TimeInterval)                    :: zero
     type(ESMF_Time)                            :: fTime
     logical                                    :: UniFileClim
     type(ESMF_Time)                            :: readTime
   
     call ESMF_TimeIntervalSet(zero,__RC__)
     if (frequency == zero) then
        UniFileClim = .false.
        ! if the file is constant, i.e. no tokens in in the template
        ! but it was marked as cyclic we must have a year long climatology 
        ! on one file, set UniFileClim to true
        if (trim(cyclic)=='y') UniFileClim = .true.
        file_processed = file_tmpl
        call GetBracketTimeOnFile(file_tmpl,cTime,bSide,UniFileClim,interpTime,fileTime,rc=status)
        if (status /= ESMF_SUCCESS) then
           RETURN_(ESMF_FAILURE)
        end if
     else 
        UniFileClim = .false.
        if (trim(cyclic)=='y') then
           call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           cYear = iyr
           iyr = climYear
           if (imm == 2 .and. idd==29) idd = 28
           call ESMF_TimeSet(readTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        else 
           tint=cTime-reffTime
           n=floor(tint/frequency)
           ftime = reffTime+(n*frequency)
           ! untemplate file
           call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           readTime = cTime
        end if
        call MAPL_PackTime(curDate,iyr,imm,idd)
        call MAPL_PackTime(curTime,ihr,imn,isc)
        call gx_(file_processed,file_tmpl,nymd=curDate,nhms=curTime,__STAT__)
        ! try to get bracketing time on file using current time
        call GetBracketTimeOnFile(file_processed,readTime,bSide,UniFileClim,interpTime,fileTime,rc=status)
        
        ! if we didn't find the bracketing time look forwards or backwards depending on
        ! whether it is the right or left time   
        if (status /= ESMF_SUCCESS) then

           if (bSide == "R") then

              if (trim(cyclic)=='y') then
                 call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 if (imm == 12) then
                    cYear = iYr + 1
                    ! change year you will read from
                    iyr = climYear-1
                    call ESMF_TimeSet(readTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    iyr = climYear
                    ! change month of file
                    imm = 1
                 else
                    cYear = iYr
                    cMonth = imm
                    iyr = climYear
                    imm = imm + 1
                    if (imm ==2 .and. idd==29) idd = 28
                    call ESMF_TimeSet(readTime,yy=iyr,mm=cMonth,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 end if
              else
                 ! check next time
                 newTime = fTime + frequency
                 ! untemplate file
                 call ESMF_TimeGet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              end if

              call MAPL_PackTime(curDate,iyr,imm,idd)
              call MAPL_PackTime(curTime,ihr,imn,isc)
              call gx_(file_processed,file_tmpl,nymd=curDate,nhms=curTime,__STAT__)
              ! try to get bracketing time on file using new time
              call GetBracketTimeOnFile(file_processed,readTime,bSide,UniFileClim,interpTime,fileTime,rc=status)

              if (status /= ESMF_SUCCESS) then
                 if (mapl_am_I_root()) write(*,*)'ExtData could not find bracketing data from file template ',trim(file_tmpl),' for side ',bSide
                 RETURN_(ESMF_FAILURE)

              end if

           else if (bSide == "L") then

              if (trim(cyclic)=='y') then
                 call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 if (imm == 1) then
                    cYear = iyr - 1
                    ! change year you will read from
                    iyr = climYear+1
                    call ESMF_TimeSet(readTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    iyr = climYear
                    ! change month of file
                    imm = 12
                 else
                    cYear = iyr
                    cMonth = imm
                    iyr = climYear
                    imm = imm - 1
                    if (imm ==2 .and. idd==29) idd = 28
                    call ESMF_TimeSet(readTime,yy=iyr,mm=cMonth,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 end if
              else
                 ! check next time
                 newTime = fTime - frequency
                 ! untemplate file
                 call ESMF_TimeGet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              end if

              call MAPL_PackTime(curDate,iyr,imm,idd)
              call MAPL_PackTime(curTime,ihr,imn,isc)
              call gx_(file_processed,file_tmpl,nymd=curDate,nhms=curTime,__STAT__)
              ! try to get bracketing time on file using new time
              call GetBracketTimeOnFile(file_processed,readTime,bSide,UniFileClim,interpTime,fileTime,rc=status)

              if (status /= ESMF_SUCCESS) then
                 if (mapl_am_I_root()) write(*,*)'ExtData could not find bracketing data from file template ',trim(file_tmpl),' for side ',bSide
                 RETURN_(ESMF_FAILURE)

              end if

           end if

        end if

        if (trim(cyclic)=='y') then
           call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           call ESMF_TimeSet(interpTime,yy=cYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        end if
        RETURN_(ESMF_SUCCESS)

     end if
    
  end subroutine UpdateBracketTime

  subroutine GetBracketTimeOnFile(file,cTime,bSide,UniFileClim,interpTime,fileTime,rc)
     character(len=ESMF_MAXSTR),          intent(in   ) :: file
     type(ESMF_Time),                     intent(inout) :: cTime
     character(len=1),                    intent(in   ) :: bSide
     logical,                             intent(in   ) :: UniFileClim
     type(ESMF_TIME),                     intent(inout) :: interpTime
     type(ESMF_TIME),                     intent(inout) :: fileTime
     integer, optional,                   intent(out  ) :: rc

     __Iam__('GetBracketTimeOnFile')

     type(ESMF_CFIO)                    :: cfio
     integer(ESMF_KIND_I4)              :: iyr,imm,idd,ihr,imn,isc,curYear,climYear
     integer                            :: iCurrInterval,i
     integer                            :: nhmsB, nymdB, incSecs
     integer                            :: begDate, begTime
     type(ESMF_Time), pointer           :: tSeries(:)
     type(ESMF_Time)                    :: climTime
     logical                            :: found
     integer                            :: climSize

     cfio =  ESMF_CFIOCreate (cfioObjName='cfio_obj',__RC__)
     call ESMF_CFIOSet(CFIO, fName=trim(file),__RC__)
     call ESMF_CFIOFileOpen  (CFIO, FMODE=1, __RC__)
     call GetBegDateTime(cfio%fid,begDate,begTime,incSecs,__RC__)
     
     if (UniFileClim) then
        call MAPL_UnpackTime(begDate,iyr,imm,idd)
        climyear = iyr
        call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        iyr = climYear
        if (idd == 29 .and. imm == 2) idd = 28
        call ESMF_TimeSet(climTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        climSize = cfio%tSteps
        ASSERT_(climsize == 12) 
     else
        climTime = cTime
        climSize = 1
     end if   

     allocate(tSeries(cfio%tSteps))
     do i=1,cfio%tSteps
        iCurrInterval = (i-1)*incSecs
        call GetDate ( begDate, begTime, iCurrInterval, nymdB, nhmsB, status )
        call MAPL_UnpackTime(nymdB,iyr,imm,idd)
        call MAPL_UnpackTime(nhmsB,ihr,imn,isc)
        call ESMF_TimeSet(tSeries(i), yy=iyr, mm=imm, dd=idd,  h=ihr,  m=imn, s=isc,__RC__)
     enddo
     found = .false.
     ! we will have to specially handle a climatology in one file
     ! might be better way but can not think of one
     if (bSide == "L") then
        if ( UniFileClim .and. (climTime < tSeries(1)) ) then
           fileTime = tSeries(climsize)
           call ESMF_TimeGet(tSeries(climsize),yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           call ESMF_TimeGet(cTime,yy=curYear,__RC__)
           iyr = curYear - 1 
           call ESMF_TimeSet(interpTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           found = .true.
        else
           do i=cfio%tSteps,1,-1
              if (climTime >= tSeries(i)) then
                 fileTime = tSeries(i)
                 if (UniFileClim) then
                    call ESMF_TimeGet(cTime,yy=curYear,__RC__)
                    call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    call ESMF_TimeSet(interpTime,yy=curYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 else
                    interpTime = tSeries(i)
                 end if
                 found = .true.
                 exit
              end if
           end do
        end if
     else if (bSide == "R") then
        if (UniFileClim .and. (climTime >= tSeries(climsize)) ) then
           fileTime = tSeries(1)
           call ESMF_TimeGet(tSeries(1),yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           call ESMF_TimeGet(cTime,yy=curYear,__RC__)
           iyr = curYear + 1 
           call ESMF_TimeSet(interpTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           found = .true.
        else
           do i=1,cfio%tSteps
              if (climTime < tSeries(i)) then
                 fileTime = tSeries(i)
                 if (UniFileClim) then
                    call ESMF_TimeGet(cTime,yy=curYear,__RC__)
                    call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    call ESMF_TimeSet(interpTime,yy=curYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 else
                    interpTime = tSeries(i)
                 end if
                 found = .true.
                 exit
              end if
           end do
        end if
     end if              
     call ESMF_CFIODestroy(CFIO,__RC__)
     deallocate(tSeries)
     if (found) then
        rc=ESMF_SUCCESS
        return
     else
        rc=ESMF_FAILURE
        return
     endif
     !end if 

  end subroutine GetBracketTimeOnFile

 subroutine CalcDerivedField(state,primaries,exportName,exportExpr,masking,rc)
     type(ESMF_State),        intent(inout) :: state
     type(PrimaryExports),    intent(inout) :: primaries
     character(len=*),        intent(in   ) :: exportName     
     character(len=*),        intent(in   ) :: exportExpr
     logical,                 intent(in   ) :: masking               
     integer, optional,       intent(out  ) :: rc

     __Iam__('CalcDerivedField')

     type(ESMF_Field)                   :: field

     if (masking) then
        call MAPL_ExtDataEvaluateMask(state,exportName,exportExpr,__RC__)
     else
        call ESMF_StateGet(state,exportName,field,__RC__)
        call MAPL_StateEval(state,exportExpr,field,__RC__)
     end if
     RETURN_(ESMF_SUCCESS)
  end subroutine CalcDerivedField

  subroutine CreateBBox(MaskName,MaskExpr,ExtDataState,rc)
     character(len=*),  intent(in   ) :: MaskName
     character(len=*),  intent(in   ) :: MaskExpr
     type(ESMF_State),  intent(inout) :: ExtDataState
     integer, optional, intent(out  ) :: rc

     integer                  :: status
     character(len=ESMF_MAXSTR), parameter :: Iam = "CreateBBox"
     real, pointer            :: lats(:,:),lons(:,:), var2d(:,:)
     integer                  :: counts(ESMF_MAXDIM)
     type(ESMF_Grid)          :: grid
     type(ESMF_Field)         :: field,fieldnew
     real                     :: lat_min,lat_max,lon_min,lon_max,rin,rout
     character(len=ESMF_MAXSTR) :: args(7)
     character(len=ESMF_MAXSTR) :: strtmp,varname
     integer                  :: dims, vloc, knd, hw
     integer :: i1,i2,nargs,i,j,im,jm
     logical :: rnum_error,lat_log,lon_log

     ! parse arguments to bbox
     i1=index(MaskExpr,'(')
     i2=index(MaskExpr,')')
     strtmp = MaskExpr(i1+1:i2-1)
     do nargs=1,7
        i1 = index(strtmp,',')
        if (i1 >0) then
           args(nargs) = strtmp(:i1-1)
        else
           args(nargs) = strtmp
        end if
        strtmp = strtmp(i1+1:)
     end do

     ! get grid of input field and create a horz only field on this grid
     varname = trim(args(1))
     call ESMF_StateGet(ExtDataState,VarName,field,__RC__)

     VarName=trim(MaskName)

     fieldnew = MAPL_FieldCreate(field,VarName,doCopy=.true.,__RC__)
     call MAPL_StateAdd(ExtDataState,fieldnew,__RC__)

     ! get corners of box
     lat_min=RealNum(args(2),error=rnum_error)
     if (rnum_error) then
        RETURN_(ESMF_FAILURE)
     end if
     lat_max=RealNum(args(3),error=rnum_error)
     if (rnum_error) then
        RETURN_(ESMF_FAILURE)
     end if
     lon_min=RealNum(args(4),error=rnum_error)
     if (rnum_error) then
        RETURN_(ESMF_FAILURE)
     end if
     lon_max=RealNum(args(5),error=rnum_error)
     if (rnum_error) then
        RETURN_(ESMF_FAILURE)
     end if

     ! get values for inside and outside
     call LowCase(args(6),strtmp)
     if (trim(strtmp) == "undef") then
        rin = MAPL_UNDEF
     else
        rin=RealNum(args(6),error=rnum_error)
        if (rnum_error) then
           RETURN_(ESMF_FAILURE)
        end if
     end if
     call LowCase(args(7),strtmp)
     if (trim(strtmp) == "undef") then
        rin = MAPL_UNDEF
     else
        rin=RealNum(args(7),error=rnum_error)
        if (rnum_error) then
           RETURN_(ESMF_FAILURE)
        end if
     end if

     ! get lats/lons from grid and construct mask
     call MAPL_GridGet(grid, localCellCountPerDim=counts,__RC__)
     im = counts(1)
     jm = counts(2)
     call ESMFL_GridCoordGet(grid,lats,      & 
          name     = "Latitude",             &
          location = ESMF_STAGGERLOC_CENTER, &
          units    = ESMFL_UnitsRadians,     &
          __RC__)
     lats=lats*180.0/MAPL_PI
     call ESMFL_GridCoordGet(grid,lons,      & 
          name     = "Longitude",            &
          location = ESMF_STAGGERLOC_CENTER, &
          units    = ESMFL_UnitsRadians,     &
          __RC__)
     lons=lons*180.0/MAPL_PI
     VarName = trim(MaskName)
     call MAPL_GetPointer(ExtDataState,var2d,VarName,rc=status)
     VERIFY_(STATUS)    

     ! generate mask based on bounding box
     do i=1,im
        do j=1,jm
           lat_log = (lat_min <= lats(i,j)).and.(lats(i,j)<=lat_max)
           lon_log = (lon_min <= lons(i,j)).and.(lons(i,j)<=lon_max)
           if (lat_log.and.lon_log) then
              var2d(i,j)=rin
           else
              var2d(i,j)=rout
           end if
        end do
     end do

     RETURN_(ESMF_SUCCESS)

  end subroutine CreateBBox

  subroutine MAPL_ExtDataInterpField(item,time,field,vector_comp,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_Time),     intent(in   ) :: time
     type(ESMF_Field),    intent(inout) :: field
     integer, optional,   intent(in   ) :: vector_comp
     integer, optional,   intent(out  ) :: rc

     character(len=ESMF_MAXSTR) :: Iam
     integer                    :: status

     type(ESMF_TimeInterval)    :: tinv1, tinv2
     real                       :: alpha
     real, pointer              :: var2d(:,:)   => null()
     real, pointer              :: var3d(:,:,:) => null()
     real, pointer              :: var2d_prev(:,:)   => null() 
     real, pointer              :: var2d_next(:,:)   => null()
     real, pointer              :: var3d_prev(:,:,:) => null()
     real, pointer              :: var3d_next(:,:,:) => null()
     integer                    :: fieldRank,i,j,k
     character(len=ESMF_MAXSTR) :: name


     Iam = "MAPL_ExtDataInterpField"
     tinv1 = time - item%interp_time1
     tinv2 = item%interp_time2 - item%interp_time1
     alpha = tinv1/tinv2
     call ESMF_FieldGet(FIELD, dimCount=fieldRank,name=name,__RC__)
     if (fieldRank == 2) then
           if (item%vartype == MAPL_FieldItem) then
              call ESMF_FieldGet(item%finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
              call ESMF_FieldGet(item%finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
           else if (item%vartype == MAPL_BundleItem) then
              call ESMFL_BundleGetPointerToData(item%binterp1,name,var2d_prev,__RC__)
              call ESMFL_BundleGetPointerToData(item%binterp2,name,var2d_next,__RC__)
           else if (item%vartype == MAPL_ExtDataVectorItem) then
              ASSERT_(present(vector_comp))
              if (vector_comp == 1) then
                 call ESMF_FieldGet(item%v1_finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
                 call ESMF_FieldGet(item%v1_finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
              else if (vector_comp == 2) then
                 call ESMF_FieldGet(item%v2_finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
                 call ESMF_FieldGet(item%v2_finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
              end if
           end if
           call ESMF_FieldGet(field, localDE=0, farrayPtr=var2d, __RC__)
           ! only interpolate if we have to
           if (time == item%interp_time1) then
              var2d = var2d_prev
           else if (time == item%interp_time2) then
              var2d = var2d_next
           else
              do j=1,size(var2d,2)
                 do i=1,size(var2d,1)
                    if (var2d_next(i,j) /= MAPL_UNDEF .and. var2d_prev(i,j) /= MAPL_UNDEF) then
                        var2d(i,j) = var2d_prev(i,j) + alpha*(var2d_next(i,j)-var2d_prev(i,j))
                    else
                        var2d(i,j) = MAPL_UNDEF
                    end if
                 enddo
              enddo
           end if
           do j=1,size(var2d,2)
              do i=1,size(var2d,1)
                 if (var2d(i,j) /= MAPL_UNDEF) then
                    if (item%do_scale .and. (.not.item%do_offset)) var2d(i,j) = item%scale*var2d(i,j)
                    if ((.not.item%do_scale) .and. item%do_offset) var2d(i,j) = var2d(i,j)+item%offset
                    if (item%do_scale .and. item%do_offset) var2d(i,j) = item%offset + (item%scale * var2d(i,j))
                 else
                     var2d(i,j) = MAPL_UNDEF
                 end if
              enddo
           enddo
      else if (fieldRank == 3) then
           if (item%vartype == MAPL_FieldItem) then
              call ESMF_FieldGet(item%finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
              call ESMF_FieldGet(item%finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
           else if (item%vartype == MAPL_BundleItem) then
              call ESMFL_BundleGetPointerToData(item%binterp1,name,var3d_prev,__RC__)
              call ESMFL_BundleGetPointerToData(item%binterp2,name,var3d_next,__RC__)
           else if (item%vartype == MAPL_ExtDataVectorItem) then
              ASSERT_(present(vector_comp))
              if (vector_comp == 1) then
                 call ESMF_FieldGet(item%v1_finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
                 call ESMF_FieldGet(item%v1_finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
              else if (vector_comp == 2) then
                 call ESMF_FieldGet(item%v2_finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
                 call ESMF_FieldGet(item%v2_finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
              end if
           end if
           call ESMF_FieldGet(field, localDE=0, farrayPtr=var3d, __RC__)
           ! only interpolate if we have to
           if (time == item%interp_time1) then
              var3d = var3d_prev
           else if (time == item%interp_time2) then
              var3d = var3d_next
           else
              do k=lbound(var3d,3),ubound(var3d,3)
                 do j=1,size(var3d,2)
                    do i=1,size(var3d,1)
                       if (var3d_next(i,j,k) /= MAPL_UNDEF .and. var3d_prev(i,j,k) /= MAPL_UNDEF) then
                           var3d(i,j,k) = var3d_prev(i,j,k) + alpha*(var3d_next(i,j,k)-var3d_prev(i,j,k))
                       else
                           var3d(i,j,k) = MAPL_UNDEF
                       end if
                    enddo
                 enddo
               enddo
           end if
           do k=lbound(var3d,3),ubound(var3d,3)
              do j=1,size(var3d,2)
                 do i=1,size(var3d,1)
                    if (var3d(i,j,k) /= MAPL_UNDEF) then
                       if (item%do_scale .and. (.not.item%do_offset)) var3d(i,j,k) = item%scale*var3d(i,j,k)
                       if ((.not.item%do_scale) .and. item%do_offset) var3d(i,j,k) = var3d(i,j,k)+item%offset
                       if (item%do_scale .and. item%do_offset) var3d(i,j,k) = item%offset + (item%scale * var3d(i,j,k))
                    else
                        var3d(i,j,k) = MAPL_UNDEF
                    end if
                 enddo
              enddo
           enddo
           
     endif

     RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_ExtDataInterpField

  ! routine to read two components of a vector on a Lat-Lon A grid
  ! and do proper vector transform to cube-sphere A grid
  ! as we will be making a lat-lon grid to call the HorzTransform
  ! we will need to pass the layout, follows what is done in the
  ! mkiau grid comp
  subroutine  MAPL_ExtDataReadVector(file,fname1,fname2,time,v1out,v2out,NX,NY,ignoreCase,rc)
     character(len=*),  intent(in   ) :: file
     character(len=*),  intent(in   ) :: fname1
     character(len=*),  intent(in   ) :: fname2
     type(ESMF_Time),   intent(inout) :: time
     type(ESMF_Field),  intent(inout) :: v1out
     type(ESMF_Field),  intent(inout) :: v2out
     integer,           intent(in   ) :: NX
     integer,           intent(in   ) :: NY
     logical,           intent(in   ) :: ignoreCase
     integer, optional, intent(out  ) :: rc

     integer                    :: status
     character(len=ESMF_MAXSTR) :: Iam

     integer :: fid
     integer :: im_world, jm_world, lmIn, nt, nvars, natts
     integer :: imIn, jmIn, imOut, jmOut, lmOut
     type(ESMF_Grid) :: gridIn, gridOut
     type(MAPL_HorzTransform) :: trans

     real, pointer :: v1out_3d(:,:,:) => null()
     real, pointer :: v2out_3d(:,:,:) => null()
     real, pointer :: v1out_2d(:,:) => null()
     real, pointer :: v2out_2d(:,:) => null()

     real, pointer :: v1in_3d(:,:,:) => null()
     real, pointer :: v2in_3d(:,:,:) => null()
     real, pointer :: v1in_2d(:,:) => null()
     real, pointer :: v2in_2d(:,:) => null()

     integer           :: gridStagger1,gridStagger2
     integer           :: gridRotation1,gridRotation2
     logical           :: doRotation

     integer           :: fieldRank 
     integer           :: DIMS(ESMF_MAXGRIDDIM)

     Iam = "MAPL_ExtDataReadVector"

     ! query the file to get the size of the lat-lon grid on the file
     call CFIO_Open(file,1,fid,status)
     VERIFY_(STATUS)
     call CFIO_DimInquire(fid,im_world,jm_world,lmIn,nt,nvars,natts,status)
     VERIFY_(STATUS)
     call CFIO_Close(fid,status)
     VERIFY_(STATUS)

     call ESMF_AttributeGet(v1out, NAME='STAGGERING', value=gridStagger1, __RC__)
     call ESMF_AttributeGet(v2out, NAME='STAGGERING', value=gridStagger2, __RC__)
     call ESMF_AttributeGet(v1out, NAME='ROTATION',value=gridRotation1,__RC__)
     call ESMF_AttributeGet(v2out, NAME='ROTATION',value=gridRotation2,__RC__)

     ASSERT_(gridStagger1 == gridStagger2)
     ASSERT_(gridRotation1 == gridRotation2)

     if (gridStagger1 == MAPL_AGrid) then
        if (gridRotation1 == MAPL_RotateLL) then
           doRotation = .false.
        else if (gridRotation1 == MAPL_RotateCube) then
           doRotation = .true.
        end if
     else if (gridStagger1 == MAPL_DGrid) then
        if (gridRotation1 /= MAPL_RotateCube) then
           ASSERT_(.false.)
        else
           doRotation = .true.
        end if 
     else if (gridStagger1 == MAPL_CGrid) then
        if (gridRotation1 /= MAPL_RotateCube) then
           ASSERT_(.false.)
        else
           doRotation = .true.
        end if
     end if
          

     gridIn = MAPL_LatLonGridCreate(name='gridll',       &
                                    NX = NX,             &
                                    NY = NY,             &
                                    IM_World = IM_World, &
                                    JM_World = JM_World, &
                                    LM_World = lmIn ,    &
                                    __RC__)

     call MAPL_GridGet(gridIn, localCellCountPerDim=dims,__RC__)
     imIn = dims(1)
     jmIn = dims(2)

     call ESMF_FieldGet(v1out,grid=gridOut,dimCount=fieldRank,__RC__)
     call MAPL_GridGet(gridOut, localCellCountPerDim=dims,__RC__)
     imOut = dims(1)
     jmOut = dims(2)


     call MAPL_HorzTransformCreate(trans,gridIn,gridOut,__RC__)

     if (fieldRank == 2) then

         call ESMF_FieldGet(v1out,localDe=0,farrayPtr=v1out_2d,__RC__)
         call ESMF_FieldGet(v2out,localDe=0,farrayPtr=v2out_2d,__RC__)
         
         allocate(v1in_3d(imIn,jmIn,1),__STAT__)
         allocate(v2in_3d(imIn,jmIn,1),__STAT__)
         allocate(v1in_2d(imIn,jmIn),__STAT__)
         allocate(v2in_2d(imIn,jmIn),__STAT__)
         allocate(v1out_3d(imOut,jmOut,1),__STAT__)
         allocate(v2out_3d(imOut,jmOut,1),__STAT__)
         

         call MAPL_CFIORead(fname1, file, time, gridIn, v1in_2d,ignoreCase=ignoreCase,__RC__)
         call MAPL_CFIORead(fname2, file, time, gridIn, v2in_2d,ignoreCase=ignoreCase,__RC__)
         
         v1in_3d(:,:,1) = v1in_2d
         v2in_3d(:,:,1) = v2in_2d
         v1out_3d(:,:,1) = v1out_2d
         v2out_3d(:,:,1) = v2out_2d
         
         call MAPL_HorzTransformRun (trans, v1in_3d, v2in_3d, v2out_3d, v2out_3d, rotate=doRotation, __RC__)

         if (gridStagger1 == MAPL_CGrid) call A2CnoRotate(v1out_3d,v2out_3d)

         v1out_2d = v1out_3d(:,:,1)
         v2out_2d = v2out_3d(:,:,1)

         deallocate(v1in_3d,v2in_3d,v1in_2d,v2in_2d,v1out_3d,v2out_3d,__STAT__)

     else if (fieldRank == 3) then
        
         call ESMF_FieldGet(v1out,localDe=0,farrayPtr=v1out_3d,__RC__)
         call ESMF_FieldGet(v2out,localDe=0,farrayPtr=v2out_3d,__RC__)
         lmOut = size(v1out_3d,3)
         ASSERT_(lmOut == lmIn)
         
         allocate(v1in_3d(imIn,jmIn,lmIn),__STAT__)
         allocate(v2in_3d(imIn,jmIn,lmIn),__STAT__)

         call MAPL_CFIORead(fname1, file, time, gridIn, v1in_3d,ignoreCase=ignoreCase,__RC__)
         call MAPL_CFIORead(fname2, file, time, gridIn, v2in_3d,ignoreCase=ignoreCase,__RC__)
         
         call MAPL_HorzTransformRun (trans, v1in_3d, v2in_3d, v1out_3d, v2out_3d, rotate=doRotation, __RC__)

         if (gridStagger1 == MAPL_CGrid) then 
            call A2CnoRotate(v1out_3d,v2out_3d)
         else if (gridStagger1 == MAPL_DGrid) then
            call A2DnoRotate(v1out_3d,v2out_3d)
         end if

         deallocate(v1in_3d,v2in_3d,__STAT__)

     end if

     RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataReadVector

  subroutine GetMaskName(FuncStr,Var,Needed,rc)
     character(len=*),               intent(in)    :: FuncStr
     character(len=*),               intent(in)    :: Var(:)
     logical,                        intent(inout) :: needed(:)
     integer, optional,              intent(out)   :: rc

     character(len=ESMF_MAXSTR)      :: Iam = "GetMaskName"
     integer                         :: status
     integer                         :: i1,i2,i,ivar
     logical                         :: found,twovar
     character(len=ESMF_MAXSTR)      :: tmpstring,tmpstring1,tmpstring2,functionname

     i1 = index(Funcstr,"(")
     ASSERT_(i1 > 0)
     functionname = adjustl(Funcstr(:i1-1))
     call ESMF_StringLowerCase(functionname,status)
     if (trim(functionname) == "regionmask") twovar = .true.
     if (trim(functionname) == "zonemask") twovar = .false.
     tmpstring = adjustl(Funcstr(i1+1:))
     i1 = index(tmpstring,",")
     ASSERT_(i1 > 0)
     i2 = index(tmpstring,";")
     if (twovar) then
        tmpstring1 = adjustl(tmpstring(1:i1-1))
        tmpstring2 = adjustl(tmpstring(i1+1:i2-1))
     else
        tmpstring1 = adjustl(tmpstring(1:i1-1))
     end if

     found = .false.
     do i=1,size(var)
        if ( trim(tmpstring1) == trim(var(i)) ) then
           ivar = i
           found = .true.
           exit
        end if
     end do
     ASSERT_(found)
     needed(ivar) = .true.

     if (twovar) then
        found = .false.
        do i=1,size(var)
           if ( trim(tmpstring2) == trim(var(i)) ) then
              ivar = i
              found = .true.
              exit
           end if
        end do
        ASSERT_(found)
        needed(ivar) = .true.
     end if
     RETURN_(ESMF_SUCCESS)
  end subroutine GetMaskName

  subroutine MAPL_ExtDataEvaluateMask(state,exportName,exportExpr,rc)

    type(ESMF_STATE),           intent(inout) :: state
    character(len=*),           intent(in)    :: exportName
    character(len=*),           intent(in)    :: exportExpr
    integer, optional,          intent(out)   :: rc

    character(len=ESMF_MAXSTR) :: Iam = "EvaluateMask"
    integer :: status

    integer :: k,i
    character(len=ESMF_MAXSTR) :: maskString,maskname,vartomask,functionname,clatS,clatN
    integer, allocatable :: regionNumbers(:), flag(:)
    integer, allocatable :: mask(:,:)
    real, pointer        :: rmask(:,:)    => null()
    real, pointer        :: rvar2d(:,:)   => null()
    real, pointer        :: rvar3d(:,:,:) => null()
    real, pointer        :: var2d(:,:)    => null()
    real, pointer        :: var3d(:,:,:)  => null()
    real, pointer        :: lats(:,:)     => null()
    real                 :: limitS, limitN
    type(ESMF_Field) :: field
    type(ESMF_Grid)  :: grid
    integer :: rank,ib,ie,is,i1

    call ESMF_StateGet(state,exportName,field,__RC__)
    call ESMF_FieldGet(field,rank=rank,grid=grid,__RC__)
    i1 = index(exportExpr,"(")
    ASSERT_(i1 > 0)
    functionname = adjustl(exportExpr(:i1-1))
    call ESMF_StringLowerCase(functionname,status)

    if (trim(functionname) == "regionmask") then

       ! get mask string
       ib = index(exportExpr,";")
       ie = index(exportExpr,")")
       maskString = trim(exportExpr(ib+1:ie-1))
       ! get mask name
       ie = index(exportExpr,";")
       is = index(exportExpr,"(")
       ib = index(exportExpr,",")
       vartomask = trim(exportExpr(is+1:ib-1))
       maskname = trim(exportExpr(ib+1:ie-1))
       call MAPL_GetPointer(state,rmask,maskName,__RC__)
       if (rank == 2) then
          call MAPL_GetPointer(state,rvar2d,vartomask,__RC__)
          call MAPL_GetPointer(state,var2d,exportName,__RC__)
       else if (rank == 3) then
          call MAPL_GetPointer(state,rvar3d,vartomask,__RC__)
          call MAPL_GetPointer(state,var3d,exportName,__RC__)
       else
          ASSERT_(.false.)
       end if

       k=32
       allocate(regionNumbers(k), flag(k), stat=status)
       VERIFY_(STATUS)
       regionNumbers = 0
       call MAPL_ExtDataExtractIntegers(maskString,k,regionNumbers,rc=status)
       VERIFY_(STATUS)
       flag(:) = 1
       WHERE(regionNumbers(:) == 0) flag(:) = 0
       k = SUM(flag)
       deallocate(flag,stat=status)
       VERIFY_(STATUS)

   !   Set local mask to 1 where gridMask matches each integer (within precision!) 
   !   ---------------------------------------------------------------------------
       allocate(mask(size(rmask,1),size(rmask,2)),stat=status)
       VERIFY_(STATUS)
       mask = 0
       DO i=1,k
        WHERE(regionNumbers(i)-0.01 <= rmask .AND. &
              rmask <= regionNumbers(i)+0.01) mask = 1
       END DO

       if (rank == 2) then
          var2d = rvar2d
          where(mask == 0) var2d = 0.0
       else if (rank == 3) then
          var3d = rvar3d
          do i=1,size(var3d,3)
             where(mask == 0) var3d(:,:,i) = 0.0
          enddo
       end if
       deallocate( mask)

    elseif(trim(functionname) == "zonemask") then

       ib = index(exportExpr,"(")
       ie = index(exportExpr,",")
       vartomask = trim(exportExpr(ib+1:ie-1))
       ib = index(exportExpr,",")
       is = index(exportExpr,",",back=.true.)
       ie = index(exportExpr,")")
       clatS = trim(exportExpr(ib+1:is-1))
       clatN = trim(exportExpr(is+1:ie-1))
       READ(clatS,*,IOSTAT=status) limitS
       VERIFY_(status)
       READ(clatN,*,IOSTAT=status) limitN
       VERIFY_(status)

       call ESMFL_GridCoordGet(grid,lats,      &
            name     = "Latitude",             &
            location = ESMF_STAGGERLOC_CENTER, &
            units    = ESMFL_UnitsRadians,     &
            __RC__)
       lats=lats*180.0/MAPL_PI
       if (rank == 2) then
          call MAPL_GetPointer(state,rvar2d,vartomask,__RC__)
          call MAPL_GetPointer(state,var2d,exportName,__RC__)
       else if (rank == 3) then
          call MAPL_GetPointer(state,rvar3d,vartomask,__RC__)
          call MAPL_GetPointer(state,var3d,exportName,__RC__)
       else
          ASSERT_(.false.)
       end if

       if (rank == 2) then
          var2d = 0.0
          where(limitS <= lats .and. lats <=limitN) var2d = rvar2d
       else if (rank == 3) then
          var3d = 0.0
          do i=1,size(var3d,3)
             where(limitS <= lats .and. lats <=limitN) var3d(:,:,i) = rvar3d(:,:,i)
          enddo
       end if

    end if

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataEvaluateMask

  SUBROUTINE MAPL_ExtDataExtractIntegers(string,iSize,iValues,delimiter,verbose,rc)

! !USES:

  IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:

  CHARACTER(LEN=*), INTENT(IN)   :: string     ! Character-delimited string of integers
  INTEGER, INTENT(IN)            :: iSize
  INTEGER, INTENT(INOUT)         :: iValues(iSize)! Space allocated for extracted integers
  CHARACTER(LEN=*), OPTIONAL     :: delimiter     ! 1-character delimiter
  LOGICAL, OPTIONAL, INTENT(IN)  :: verbose    ! Let me know iValues as they are found. 
                                      ! DEBUG directive turns on the message even 
                                      ! if verbose is not present or if 
                                      ! verbose = .FALSE.
  INTEGER, OPTIONAL, INTENT(OUT) :: rc            ! Return code
! !DESCRIPTION: 
!
!  Extract integers from a character-delimited string, for example, "-1,45,256,7,10".  In the context
!  of Chem_Util, this is provided for determining the numerically indexed regions over which an 
!  emission might be applied.
!
!  In multiple passes, the string is parsed for the delimiter, and the characters up to, but not
!  including the delimiter are taken as consecutive digits of an integer.  A negative sign ("-") is
!  allowed.  After the first pass, each integer and its trailing delimiter are lopped of the head of
!  the (local copy of the) string, and the process is started over.
!
!  The default delimiter is a comma (",").
!
!  "Unfilled" iValues are zero.
!  
!  Return codes:
!  1 Zero-length string.
!  2 iSize needs to be increased.
!
!  Assumptions/bugs:
!
!  A non-zero return code does not stop execution.
!  Allowed numerals are: 0,1,2,3,4,5,6,7,8,9.
!  A delimiter must be separated from another delimiter by at least one numeral.
!  The delimiter cannot be a numeral or a negative sign.
!  The character following a negative sign must be an allowed numeral.
!  The first character must be an allowed numeral or a negative sign.
!  The last character must be an allowed numeral.
!  The blank character (" ") cannot serve as a delimiter.
!
!  Examples of strings that will work:
!  "1"
!  "-1"
!  "-1,2004,-3"
!  "1+-2+3"
!  "-1A100A5"
!  Examples of strings that will not work:
!  "1,--2,3"
!  "1,,2,3"
!  "1,A,3"
!  "1,-,2"
!  "1,2,3,4,"
!  "+1"
!  "1 3 6"
!
! !REVISION HISTORY: 
!
!  Taken from chem utilities.
!
!EOP
 CHARACTER(LEN=*), PARAMETER :: Iam = 'Chem_UtilExtractIntegers'

 INTEGER :: base,count,i,iDash,last,lenStr,status
 INTEGER :: multiplier,pos,posDelim,sign
 CHARACTER(LEN=255) :: str
 CHARACTER(LEN=1) :: char,delimChar
 LOGICAL :: Done
 LOGICAL :: tellMe

! Initializations
! ---------------
 rc = 0
 count = 1
 Done = .FALSE.
 iValues(:) = 0
 base = ICHAR("0")
 iDash = ICHAR("-")

! Determine verbosity, letting the DEBUG 
! directive override local specification
! --------------------------------------
  tellMe = .FALSE.
  IF(PRESENT(verbose)) THEN
   IF(verbose) tellMe = .TRUE.
 END IF
#ifdef DEBUG
  tellMe = .TRUE.
#endif
! Check for zero-length string
! ----------------------------
 lenStr = LEN_TRIM(string)
 IF(lenStr == 0) THEN
  rc = 1
  PRINT *,trim(IAm),": ERROR - Found zero-length string."
  RETURN
 END IF

! Default delimiter is a comma
! ----------------------------
 delimChar = ","
 IF(PRESENT(delimiter)) delimChar(1:1) = delimiter(1:1)

! Work on a local copy
! --------------------
 str = TRIM(string)

! One pass for each delimited integer
! -----------------------------------
 Parse: DO

  lenStr = LEN_TRIM(str)

! Parse the string for the delimiter
! ----------------------------------
  posDelim = INDEX(TRIM(str),TRIM(delimChar))
  IF(tellMe) PRINT *,trim(Iam),": Input string is >",TRIM(string),"<"

! If the delimiter does not exist,
! one integer remains to be extracted.
! ------------------------------------
  IF(posDelim == 0) THEN
   Done = .TRUE.
   last = lenStr
  ELSE
   last = posDelim-1
  END IF
  multiplier = 10**last

! Examine the characters of this integer
! --------------------------------------
  Extract: DO pos=1,last

   char = str(pos:pos)
   i = ICHAR(char)

! Account for a leading "-"
! -------------------------
   IF(pos == 1) THEN
    IF(i == iDash) THEN
     sign = -1
    ELSE
     sign = 1
    END IF
   END IF

! "Power" of 10 for this character
! --------------------------------
   multiplier = multiplier/10

   IF(pos == 1 .AND. sign == -1) CYCLE Extract

! Integer comes from remaining characters
! ---------------------------------------
   i = (i-base)*multiplier
   iValues(count) = iValues(count)+i
   IF(pos == last) THEN
    iValues(count) = iValues(count)*sign
    IF(tellMe) PRINT *,trim(Iam),":Integer number ",count," is ",iValues(count)
   END IF

  END DO Extract

  IF(Done) EXIT

! Lop off the leading integer and try again
! -----------------------------------------
  str(1:lenStr-posDelim) = str(posDelim+1:lenStr)
  str(lenStr-posDelim+1:255) = " "
  count = count+1

! Check size
! ----------
  IF(count > iSize) THEN
   rc = 2
   PRINT *,trim(Iam),": ERROR - iValues does not have enough elements."
  END IF

 END DO Parse

 RETURN_(ESMF_SUCCESS)

 END SUBROUTINE MAPL_ExtDataExtractIntegers

  subroutine MAPL_ExtDataUpdateDiurnalBracket(currTime,item,rc)
     type(ESMF_Time),     intent(inout) :: currTime
     type(PrimaryExport), intent(inout) :: item
     integer, optional,   intent(out  ) :: rc

     character(len=ESMF_MAXSTR) :: Iam
     integer                    :: status

     integer                    :: YY, MM, DD, H, M, S
     type(ESMF_Time)            :: newTime
     integer                    :: readMonth, curDate, curTime
     character(len=ESMF_MAXPATHLEN) :: file_processed
     type(ESMF_CFIO)            :: cfio
     integer                    :: i, begDate, begTime, incSecs
     integer                    :: iCurrInterval, nymdB, nhmsB

     Iam = "MAPL_ExtDataUpdateDiurnalBracket"

     call ESMF_TimeGet(currTime,yy=yy,mm=mm,dd=dd,h=h,m=m,s=s,__RC__)

     ! now read the bracketing data
     call MAPL_PackTime(curDate,yy,mm,00)
     curTime = 0
     call gx_(file_processed,item%file,nymd=curDate,nhms=curTime,__STAT__)
     if (trim(file_processed) == trim(item%diurnal_data%currentFile)) then
        RETURN_(ESMF_SUCCESS)
     else
        item%diurnal_data%currentFile = file_processed
     end if

     cfio =  ESMF_CFIOCreate (cfioObjName='cfio_obj',__RC__)
     call ESMF_CFIOSet(CFIO, fName=trim(file_processed),__RC__)
     call ESMF_CFIOFileOpen  (CFIO, FMODE=1, __RC__)        
     call GetBegDateTime(cfio%fid,begDate,begTime,incSecs,__RC__)
     ASSERT_(cfio%tsteps == item%diurnal_data%ntimes)

     do i=1,item%diurnal_data%ntimes

        iCurrInterval = (i-1)*incSecs
        call GetDate ( begDate, begTime, iCurrInterval, nymdB, nhmsB, status )
        call MAPL_UnpackTime(nymdB,YY,MM,DD)
        call MAPL_UnpackTime(nhmsB,H,M,S)
        call ESMF_TimeSet(newTime, yy=YY, mm=MM, dd=DD,  h=H,  m=M, s=S,__RC__)
        if (item%vartype == MAPL_FieldItem) then
              call MAPL_CFIORead(trim(item%var), file_processed, newTime, item%diurnal_data%field1(i), &
                 time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative ,__RC__)
        else if (item%vartype == MAPL_BundleItem) then
           ASSERT_(.false.)
        end if
     enddo

     RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataUpdateDiurnalBracket

  subroutine MAPL_ExtDataDiurnalInterp(currTime,item,state,rc)
     type(ESMF_Time),     intent(inout) :: currTime
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State),    intent(inout) :: state
     integer, optional,   intent(out  ) :: rc

     character(len=ESMF_MAXSTR) :: Iam
     integer                    :: status

     type(ESMF_Field)           :: field
     type(ESMF_FieldBundle)     :: bundle
     integer                    :: i,yr,mm,dd,hr,mn,sc,nhms
     integer                    :: hr1,hr2,mn1,mn2,sc1,sc2
     type(ESMF_Time)            :: time1, time2
     type(ESMF_TimeInterval)    :: tInterval
     integer                    :: idx1,idx2
     logical                    :: onTime

     Iam = "MAPL_ExtDataDiurnalInterp"

     if (item%vartype == MAPL_FieldItem) then
        call ESMF_StateGet(State, item%name, field, __RC__)
     else if (item%vartype == MAPL_BundleItem) then
        ASSERT_(.false.)
     end if
     ! make time interval of one day
     call ESMF_TimeIntervalSet(tInterval,h=24,__RC__)
     call ESMF_TimeGet(currTime,yy=yr,mm=mm,dd=dd,h=hr,m=mn,s=sc,__RC__)
     call MAPL_PackTime(nhms,hr,mn,sc)
     idx1 = -1
     idx2 = -1
     do i=1,item%diurnal_data%nTimes
        if ( (nhms == item%diurnal_data%times(i)) .and. (i < item%diurnal_data%nTimes) ) then
           onTime = .true.
           idx1 = i
           idx2 = i + 1
           exit
        else if ( (nhms == item%diurnal_data%times(i)) .and. (i == item%diurnal_data%nTimes) ) then
           onTime = .true.
           idx1 = i
           idx2 = 1
        else if ( (nhms > item%diurnal_data%times(i)) .and. (i == item%diurnal_data%nTimes) ) then
           onTime = .false. 
           idx1 = i
           idx2 = 1
           exit 
        else if ( (item%diurnal_data%times(i) < nhms) .and. (nhms < item%diurnal_data%times(i+1)) ) then
           onTime = .false. 
           idx1 = i
           idx2 = i + 1
           exit
        end if
     enddo
     call MAPL_UnpackTime(item%diurnal_data%times(idx1),hr1,mn1,sc1)
     call MAPL_UnpackTime(item%diurnal_data%times(idx2),hr2,mn2,sc2)
     call ESMF_TimeSet(time1,yy=yr,mm=mm,dd=dd,h=hr1,m=mn1,s=sc1,__RC__)
     call ESMF_TimeSet(time2,yy=yr,mm=mm,dd=dd,h=hr2,m=mn2,s=sc2,__RC__)
     if (idx2 ==1 ) time2 = time2+tinterval
     item%interp_time1 = time1
     item%interp_time2 = time2
     item%finterp1 = item%diurnal_data%field1(idx1)
     item%finterp2 = item%diurnal_data%field1(idx2)

     if (item%vartype == MAPL_FieldItem) then

        call MAPL_ExtDataInterpField(item,currTime,field,__RC__)          

     else if (item%vartype == MAPL_BundleItem) then
     
        ASSERT_(.false.)

     end if

     RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataDiurnalInterp

  function MAPL_ExtDataGetFStartTime(fname, rc) result(stime)

     character(len=*), intent(in   ) :: fname
     integer, optional, intent(out  ) :: rc

     type(ESMF_Time) :: stime

     character(len=ESMF_MAXSTR), parameter :: IAM="MAPL_ExtDataGetFStartTime"
     integer                               :: status

     integer :: iyr,imm,idd,ihr,imn,isc,begDate,begTime
     type(ESMF_CFIO) :: cfio

     cfio =  ESMF_CFIOCreate (cfioObjName='cfio_obj',__RC__)
     call ESMF_CFIOSet(CFIO, fName=fname,__RC__)
     call ESMF_CFIOFileOpen  (CFIO, FMODE=1, cyclic=.false.,__RC__)
     begDate = cfio%date
     begTime = cfio%begTime
     call MAPL_UnpackTime(begDate,iyr,imm,idd)
     call MAPL_UnpackTime(begTime,ihr,imn,isc)
     call ESMF_TimeSet(sTime, yy=iyr, mm=imm, dd=idd,  h=ihr,  m=imn, s=isc, __RC__)
     call ESMF_CFIODestroy(CFIO,__RC__)

     RETURN_(ESMF_SUCCESS)

  end function MAPL_ExtDataGetFStartTime

  subroutine AdvanceAndCount(CF,nLines,rc)

     type(ESMF_Config), intent(inout) :: cf
     integer, intent(out)             :: nLines
     integer, optional, intent(out)   :: rc

     integer :: iCnt
     logical :: inBlock
     character(len=ESMF_MAXSTR) :: thisLine
     integer :: status
     character(len=ESMF_MAXSTR) :: Iam
     Iam = "AdvanceAndCount"

     inBlock = .true.
     iCnt = 0
     do while(inBlock)
         call ESMF_ConfigNextLine(CF,rc=status)
         VERIFY_(STATUS)
         call ESMF_ConfigGetAttribute(CF,thisLine,rc=status)
         VERIFY_(STATUS)
         if (trim(thisLine) == "%%") then
            inBlock = .false.
         else
            iCnt = iCnt + 1
         end if
     end do
     nLines = iCnt

     RETURN_(ESMF_SUCCESS)

  end subroutine advanceAndCount

  subroutine CheckUpdate(doUpdate,updateTime,currTime,hasRun,primaryItem,derivedItem,rc) 
     logical,                       intent(out  ) :: doUpdate
     type(ESMF_Time),               intent(inout) :: updateTime
     type(ESMF_Time),               intent(inout) :: currTime
     logical        ,               intent(in   ) :: hasRun
     type(PrimaryExport), optional, intent(inout) :: primaryItem
     type(DerivedExport), optional, intent(inout) :: derivedItem
     integer,             optional, intent(out  ) :: rc

     logical                    :: isEnabled
     character(len=ESMF_MAXSTR) :: Iam
     integer                    :: status
     type(ESMF_Time)            :: time,time0,refresh_time
     Iam = "CheckUpdate"

     time0 = currTime
     time  = currTime
     if (present(primaryItem)) then
       
        if (primaryItem%AlarmIsEnabled) then
           doUpdate = ESMF_AlarmIsRinging(primaryItem%update_alarm,__RC__)
           if (hasRun .eqv. .false.) doUpdate = .true.
           updateTime = currTime
        else if (trim(primaryItem%cyclic) == 'single') then
           doUpdate = .true.
        else
           if (primaryItem%refresh_template == "0") then
              doUpdate = .true.
              updateTime = time0 + PrimaryItem%tshift
           else
              updateTime = time0
              if (.not. associated(PrimaryItem%refresh_time)) then
                doUpdate = .false.
              else
                 refresh_time = timestamp_(time, PrimaryItem%refresh_template, __RC__)
                 if (refresh_time /= primaryItem%refresh_time) then
                    doUpdate = .true.
                    primaryItem%refresh_time = refresh_time
                    updateTime = refresh_time
                 else
                    doUpdate = .false.
                 end if
              end if
           end if
        end if
     else if (present(derivedItem)) then
        if (DerivedItem%AlarmIsEnabled) then
           doUpdate = ESMF_AlarmIsRinging(derivedItem%update_alarm,__RC__)
           updateTime = currTime
        else
           if (derivedItem%refresh_template == "0") then
              doUpdate = .true.
              updateTime = time0 + derivedItem%tshift
           else
              updateTime = time0
              if (.not. associated(derivedItem%refresh_time)) then
                doUpdate = .false.
              end if
              refresh_time = timestamp_(time, derivedItem%refresh_template, __RC__)
              if (refresh_time /= derivedItem%refresh_time) then
                 doUpdate = .true.
                 derivedItem%refresh_time = refresh_time
                 time = refresh_time
              else
                 doUpdate = .false.
              end if
           end if
        end if
     end if
     
     RETURN_(ESMF_SUCCESS)
  end subroutine CheckUpdate

  subroutine SetRefreshAlarms(clock,primaryItem,derivedItem,rc) 
     type(ESMF_Clock),              intent(inout) :: Clock
     type(PrimaryExport), optional, intent(inout) :: primaryItem
     type(DerivedExport), optional, intent(inout) :: derivedItem
     integer,             optional, intent(out  ) :: rc

     integer                    :: pindex,cindex,iyy,imm,idd,ihh,imn,isc
     character(len=ESMF_MAXSTR) :: refresh_template,ctInt
     character(len=ESMF_MAXSTR) :: Iam
     type(ESMF_TimeInterval)    :: tInterval
     integer                    :: status
     Iam = "SetRefreshAlarms"

     if (present(primaryItem)) then
        refresh_template = primaryItem%refresh_template
     else if (present(derivedItem)) then
        refresh_template = derivedItem%refresh_template
     end if
     pindex = index(refresh_template,'P')
     if (pindex > 0) then
        ! now get time interval. Put 0000-00-00 in front if not there so parsetimeunits doesn't complain
        ctInt = refresh_template(pindex+1:)
        cindex = index(ctInt,'T')
        if (cindex == 0) ctInt = '0000-00-00T'//trim(ctInt)
        call MAPL_NCIOParseTimeUnits(ctInt,iyy,imm,idd,ihh,imn,isc,status)
        VERIFY_(STATUS)
        call ESMF_TimeIntervalSet(tInterval,yy=iyy,mm=imm,d=idd,h=ihh,m=imn,s=isc,rc=status)
        VERIFY_(STATUS) 
        if (present(primaryItem)) then 
           primaryItem%update_alarm = ESMF_AlarmCreate(clock=clock,ringInterval=tInterval,sticky=.false.,rc=status)
           VERIFY_(STATUS)
           primaryItem%alarmIsEnabled = .true.
        else if (present(derivedItem)) then
           DerivedItem%update_alarm = ESMF_AlarmCreate(clock=clock,ringInterval=tInterval,sticky=.false.,rc=status)
           VERIFY_(STATUS)
           derivedItem%alarmIsEnabled = .true.
        end if
     end if

     RETURN_(ESMF_SUCCESS)
  end subroutine SetRefreshAlarms

 END MODULE MAPL_ExtDataGridCompMod
