! +-======-+ 
!  Copyright (c) 2003-2007 United States Government as represented by 
!  the Admistrator of the National Aeronautics and Space Administration.  
!  All Rights Reserved.
!  
!  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
!  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
!  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
!  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
!  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
!  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
!  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
!  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
!  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
!  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
!  
!  Government Agency: National Aeronautics and Space Administration
!  Government Agency Original Software Designation: GSC-15354-1
!  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
!  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
!  Government Agency Point of Contact for Original Software:  
!  			Dale Hithon, SRA Assistant, (301) 286-2691
!  
! +-======-+ 
#include "MAPL_Exceptions.h"
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
   use MAPL_NewArthParserMod
   use MAPL_ConstantsMod, only: MAPL_PI

   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC SetServices
!EOP
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!-------------------------------------------------------------------------

! Primary Exports
! ---------------
  type PrimaryExport
     PRIVATE
     character(len=ESMF_MAXSTR)   :: name
     character(len=ESMF_MAXSTR)   :: units
     integer                      :: dim
     integer                      :: vloc
     logical                      :: cyclic
     character(len=ESMF_MAXSTR)   :: refresh_template
     logical                      :: conservative
     real                         :: scale, offset
     logical                      :: do_offset, do_scale
     character(len=ESMF_MAXSTR)   :: var
     character(len=ESMF_MAXSTR)   :: file
     character(len=256        )   :: creff_time
     character(len=ESMF_MAXSTR)   :: tunits
     integer                      :: int_frequency

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

     logical                      :: ExtDataAlloc

  end type PrimaryExport

  type PrimaryExports
     PRIVATE
     integer :: nItems
     type(PrimaryExport), pointer :: item(:) => null() 
  end type PrimaryExports

  type DerivedExport
     PRIVATE
     character(len=ESMF_MAXSTR)   :: name
     character(len=ESMF_MAXSTR)   :: expression
     character(len=ESMF_MAXSTR)   :: refresh_template
     logical                      :: ExtDataAlloc
     type(ESMF_Time), pointer     :: refresh_time => null()
     
  end type DerivedExport

  type DerivedExports
     PRIVATE
     integer :: nItems
     type(DerivedExport), pointer :: item(:) => null()
  end type DerivedExports

! Legacy state
! ------------
  type MAPL_ExtData_State
     PRIVATE
     type(PrimaryExports) :: Primary
     type(DerivedExports) :: Derived
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

   real, pointer, dimension(:,:)     :: var2d
   real, pointer, dimension(:,:,:)   :: var3d

   type(PrimaryExports)              :: Primary
   type(PrimaryExport), pointer      :: item
   type(DerivedExports)              :: Derived 
   integer                           :: nLines, nCols
   integer                           :: i, iret
   integer                           :: ItemCount, j
   integer                           :: PrimaryItemCount, DerivedItemCount
   integer                           :: dims, vloc, knd, hw
   logical                           :: found, isPrimaryItem, isDerivedItem
   character(len=ESMF_MAXSTR)        :: CFfilename


   type(ESMF_Time)                   :: time
   character(len=ESMF_MAXSTR)        :: VarName
   integer                           :: VarNum

   integer                           :: yy, mm, dd
   type (ESMF_Field)                 :: field
   integer                           :: fieldRank
   type (ESMF_FieldBundle)           :: bundle
   integer                           :: fieldcount
   type (ESMF_StateItem_Flag), pointer    :: ITEMTYPES(:)
   character(len=ESMF_MAXSTR ), allocatable   :: ITEMNAMES(:)

   character(len=ESMF_MAXSTR),allocatable    :: PrimaryVarNames(:)
   character(len=ESMF_MAXSTR),allocatable    :: VarNames(:)
   integer                                   :: NumVarNames

   integer                                   :: nMasks
   character(len=ESMF_MAXSTR),allocatable    :: MaskNames(:)
   character(len=ESMF_MAXSTR),allocatable    :: MaskExprs(:)

!  logical to keep track of primary variables needed by derived fields, that ARE NOT in export state
   logical, allocatable              :: PrimaryVarNeeded(:)
   logical, allocatable              :: DerivedVarNeeded(:)
   logical, allocatable              :: LocalVarNeeded(:)

   type(ESMF_CFIO)                   :: CFIO
   integer                           :: counter
   real, pointer                     :: ptr2d(:,:) => null()
   real, pointer                     :: ptr3d(:,:,:) => null()
   real                              :: const
   integer                           :: k, ios
   character(len=ESMF_MAXSTR)        :: c_offset, c_scale
   integer                           :: nlists, nlist
   character(len=ESMF_MAXSTR),allocatable    :: ConfigList(:)
   type(ESMF_Config) :: CF, CFtemp
   type(ESMF_Config) :: localCF
   integer           :: totalPrimaryEntries
   integer           :: totalDerivedEntries
   integer           :: totalMaskEntries
   logical           :: caseSensitiveVarNames

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
    call ESMF_ConfigGetAttribute(CF_master,caseSensitiveVarNames, Label='CASE_SENSITIVE_VARIABLE_NAMES:',default=.true.,rc=status)
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

!   Get list of other files to look at for primary exports, if there are none just process master CF file
    nlists = 0
    totalPrimaryEntries = 0
    totalDerivedEntries = 0
    totalMaskEntries = 0
    call ESMF_ConfigGetDim(CF_master, nLines, nCols, LABEL='Include::',rc=status)
    if (status == ESMF_SUCCESS .and. nLines > 0) then
       nlists = nLines
       allocate(ConfigList(nLists),stat=status)
       VERIFY_(STATUS)
       call ESMF_ConfigFindLabel(CF_master, 'Include::',__RC__)
       do i=1, nLines
          call ESMF_ConfigNextLine(CF_master,__RC__)
          call ESMF_ConfigGetAttribute(CF_master,ConfigList(i),__RC__)
       end do 
    end if

    CFtemp = ESMF_ConfigCreate(__RC__)
    localCF = CF_Master
    do nList = 0,nLists
       ! if nlist is greater than 0 then load the other RC files otherwise we just process master one: ExtData.rc
       if (nList > 0) then
          call ESMF_ConfigLoadFile(CFtemp, ConfigList(nList), rc=STATUS )
          VERIFY_(STATUS)
          localCF = CFtemp
       end if
       call ESMF_ConfigGetDim(localCF, nLines, nCols, LABEL='PrimaryExports::', __RC__)
       totalPrimaryEntries = totalPrimaryEntries + nLines
       call ESMF_ConfigGetDim(localCF, nLines, nCols, LABEL='DerivedExports::', __RC__)
       totalDerivedEntries = totalDerivedEntries + nLines
       call ESMF_ConfigGetDim(localCF, nLines, nCols, LABEL='Masks::', __RC__)
       totalMaskEntries = totalMaskEntries + nLines
    enddo
 
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
       allocate(derived%item(totalDerivedEntries),stat=status)
       VERIFY_(STATUS) 
    end if
 
    nMasks = totalMaskEntries
    if (totalMaskEntries > 0) then
       allocate(MaskNames(totalMaskEntries),stat=status)
       VERIFY_(STATUS)
       allocate(MaskExprs(totalMaskEntries),stat=status)
       VERIFY_(STATUS)
    end if
!   Primary Exports
!   ---------------

    totalPrimaryEntries = 0
    totalDerivedEntries = 0
    totalMaskEntries = 0
    localCF = CF_Master
    NLIST_LOOP: do nList=0,nLists

       ! if nlist is greater than 0 then load the other RC files otherwise we just process master one: ExtData.rc
       if (nList > 0) then
          call ESMF_ConfigLoadFile(CFtemp, ConfigList(nList), rc=STATUS )
          VERIFY_(STATUS)
          localCF = CFtemp
       end if

       call ESMF_ConfigGetDim(localCF, nLines, nCols, LABEL='PrimaryExports::', __RC__)

       if ( nLines > 0 ) then

          call ESMF_ConfigFindLabel(localCF, 'PrimaryExports::', __RC__)

          do i = 1, nLines
 
             totalPrimaryEntries = totalPrimaryEntries + 1
            
             call ESMF_ConfigNextLine(localCF, __RC__)

             call ESMF_ConfigGetAttribute(localCF, primary%item(totalPrimaryEntries)%name,  __RC__)
             PrimaryVarNames(totalPrimaryEntries) = primary%item(totalPrimaryEntries)%name
             call ESMF_ConfigGetAttribute(localCF, primary%item(totalPrimaryEntries)%units, __RC__)

             call ESMF_ConfigGetAttribute(localCF, buffer, __RC__)
             call ESMF_StringLowerCase(buffer, iret)
             if (buffer == 'xy') then
                primary%item(totalPrimaryEntries)%dim = MAPL_DimsHorzOnly
             else if (buffer == 'xyz') then
                primary%item(totalPrimaryEntries)%dim = MAPL_DimsHorzVert
             else
                __raise__(MAPL_RC_ERROR, "invalid dimension for Primary Export")
             end if

             call ESMF_ConfigGetAttribute(localCF, buffer, __RC__)
             call ESMF_StringLowerCase(buffer, iret)
             if (buffer == 'c') then
                primary%item(totalPrimaryEntries)%vloc = MAPL_VLocationCenter
             else if (buffer == 'e') then
                primary%item(totalPrimaryEntries)%vloc = MAPL_VLocationEdge
             else
                __raise__(MAPL_RC_ERROR, "invalid vLocation for Primary Export")
             end if
       
             call ESMF_ConfigGetAttribute(localCF, buffer, __RC__)
             call ESMF_StringLowerCase(buffer, iret)
             if (trim(buffer) == 'n') then
                primary%item(totalPrimaryEntries)%cyclic = .false.
             else if (trim(buffer) == 'y') then
                primary%item(totalPrimaryEntries)%cyclic = .true.
             else
                __raise__(MAPL_RC_ERROR, "the cyclic keyword for extdata primary export must be y or n")
             end if

             call ESMF_ConfigGetAttribute(localCF, buffer, __RC__)
             call ESMF_StringLowerCase(buffer, iret)
             if (trim(buffer) == 'y') then
                primary%item(totalPrimaryEntries)%conservative = .true.
             else if (trim(buffer) == 'n') then
                primary%item(totalPrimaryEntries)%conservative = .false.
             else
                __raise__(MAPL_RC_ERROR, "the conservative keyword for extdata primary export must be y or n")
             end if

             call ESMF_ConfigGetAttribute(localCF, primary%item(totalPrimaryEntries)%refresh_template, __RC__)
             call ESMF_ConfigGetAttribute(localCF, c_offset, __RC__)
             if (trim(c_offset) == "none") then
                primary%item(totalPrimaryEntries)%do_offset = .false.
             else
                primary%item(totalPrimaryEntries)%do_offset = .true.
                read(c_offset,*,iostat=ios) primary%item(totalPrimaryEntries)%offset
             end if
             call ESMF_ConfigGetAttribute(localCF, c_scale, __RC__)
             if (trim(c_scale) == "none") then
                primary%item(totalPrimaryEntries)%do_scale = .false.
             else
                primary%item(totalPrimaryEntries)%do_scale = .true.
                read(c_scale,*,iostat=ios) primary%item(totalPrimaryEntries)%scale
             end if
             call ESMF_ConfigGetAttribute(localCF, primary%item(totalPrimaryEntries)%var,    __RC__)
             call ESMF_ConfigGetAttribute(localCF, primary%item(totalPrimaryEntries)%file,   __RC__)

             call ESMF_ConfigGetAttribute(localCF, primary%item(totalPrimaryEntries)%creff_time, rc=status)
             if (status /= ESMF_SUCCESS) then
                primary%item(totalPrimaryEntries)%creff_time = ""
             end if             
             call ESMF_ConfigGetAttribute(localCF, primary%item(totalPrimaryEntries)%tunits, rc=status)
             if (status /= ESMF_SUCCESS) then
                primary%item(totalPrimaryEntries)%tunits = ""
             end if             
             call ESMF_ConfigGetAttribute(localCF, primary%item(totalPrimaryEntries)%int_frequency, rc=status)
             if (status /= ESMF_SUCCESS) then
                primary%item(totalPrimaryEntries)%int_frequency = -1
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

             if ( .not. primary%item(totalPrimaryEntries)%isConst )  then
                call CreateTimeInterval(primary%item(totalPrimaryEntries),clock,__RC__)
                if (primary%item(totalPrimaryEntries)%cyclic) then
                   call GetClimYear(primary%item(totalPrimaryEntries),__RC__)
                end if
             end if

          end do
       end if ! end of primary exports

   !   Derived Exports
   !   ---------------
       call ESMF_ConfigGetDim(localCF, nLines, nCols, Label='DerivedExports::', rc=status)

       if ( nLines > 0 ) then
          totalDerivedEntries = totalDerivedEntries + 1
          call ESMF_ConfigFindLabel(localCF, 'DerivedExports::', __RC__)
          do i=1,nLines
             call ESMF_ConfigNextLine(localCF, __RC__)
             call ESMF_ConfigGetAttribute(localCF,derived%item(totalDerivedEntries)%name,__RC__)
             call ESMF_ConfigGetAttribute(localCF,derived%item(totalDerivedEntries)%expression,__RC__)
             call ESMF_ConfigGetAttribute(localCF,derived%item(totalDerivedEntries)%refresh_template, __RC__)
             derived%item(totalDerivedEntries)%ExtDataAlloc = .true.
          end do
       end if

   !   Masks
   !   ---------------
       call ESMF_ConfigGetDim(localCF, nLines, nCols, Label='Masks::', __RC__)
       if (nLines > 0) then
          call ESMF_ConfigFindLabel(localCF, 'Masks::', __RC__)
          do i = 1, nLines
             totalMaskEntries = totalMaskEntries + 1
             call ESMF_ConfigNextLine(localCF, __RC__)
             call ESMF_ConfigGetAttribute(localCF, MaskNames(totalMaskEntries),  __RC__)
             call ESMF_ConfigGetAttribute(localCF, MaskExprs(totalMaskEntries), __RC__)
          end do
       end if

    end do NLIST_LOOP
   
!   Done parsing resource file    

    PrimaryItemCount = 0
    DerivedItemCount = 0

!   find items in primary and derived to fullfill Export state
!   once we find primary or derived put in namespace
    self%ExtDataState = ESMF_StateCreate(Name="ExtDataNameSpace",__RC__)
    do I = 1, ItemCount

       found = .false.
       do J = 1, primary%nItems
          if (ItemNames(I) == primary%item(J)%name) then
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
       end do
       if ( (.not.found) .and. (derived%nItems > 0) ) then
          do J = 1, derived%nItems
             if (ItemNames(I) == derived%item(J)%name) then

                if (ITEMTYPES(I) == ESMF_StateItem_FieldBundle) then
                   ASSERT_(.false.)
                end if
                found = .true.
                DerivedVarNeeded(j) = .true.
                DerivedItemCount = DerivedItemCount + 1
                derived%item(j)%ExtDataAlloc = .false.
                VarName=derived%item(j)%name
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

!   we have better found all the items in the export in either a primary or derived item
    if ( (PrimaryItemCount+DerivedItemCount) /= ItemCount) then
       if (mapl_am_I_root()) then
          write(*,'(A6,I3,A31)')'Found ',ItemCount-(PrimaryItemCount+DerivedItemCount),' unfullfilled imports in extdata'
       end if
       ASSERT_(.false.)
    end if

    NumVarNames=primary%nItems+nMasks
    allocate(VarNames(NumVarNames))
    allocate(LocalVarNeeded(NumVarNames))
    do i=1,primary%nItems
       VarNames(i)=PrimaryVarNames(i)
    end do
    do i=primary%nItems+1,NumVarNames
       VarNames(i)=MaskNames(i-primary%nItems)
    end do

!   search for other primary variables we may need to fill derived types that were not in the export state
!   if we find them allocate them based on grid of variable we are trying to fill
    do i=1, derived%nItems
       if (DerivedVarNeeded(i)) then
          LocalVarNeeded=.false.
          call CheckSyntax(derived%item(i)%expression,VarNames,LocalVarNeeded,__RC__)
          do j=1, primary%nItems
             if (LocalVarNeeded(j)) then
                VarName = trim(primary%item(j)%name)
                call ESMF_StateGet(self%ExtDataState,VarName,field,rc=status)
                if (status /= ESMF_SUCCESS) then
                   VarName = trim(derived%item(i)%name)
                   call ESMF_StateGet(self%ExtDataState,VarName,field,__RC__)
                   call ESMF_FieldGet(field,grid=grid,__RC__)
                   call ESMF_AttributeGet(FIELD, NAME='DIMS', VALUE=DIMS, RC=STATUS)
                   VERIFY_(STATUS)
                   call ESMF_AttributeGet(FIELD, NAME='VLOCATION', VALUE=VLOC, RC=STATUS)
                   VERIFY_(STATUS)
                   call ESMF_AttributeGet(FIELD, NAME='HALOWIDTH', VALUE=HW, RC=STATUS)
                   VERIFY_(STATUS)
                   call ESMF_AttributeGet(FIELD, NAME='PRECISION', VALUE=KND,RC=STATUS)
                   if (status /= ESMF_SUCCESS) knd=kind(0.0)
                   VarName=trim(primary%item(j)%name)
                   field = mapl_FieldCreateEmpty(VarName,grid,__RC__)
                   call MAPL_FieldAllocCommit(field,dims=dims,location=vloc,typekind=knd,hw=hw,__RC__)
                   call MAPL_StateAdd(self%ExtDataState,field,__RC__)
                   PrimaryVarNeeded(j) = .true.
                   primary%item(j)%ExtDataAlloc = .true.
                   PrimaryItemCount = PrimaryItemCount + 1
                end if
             end if
          end do
          do j=primary%nItems+1,NumVarNames
             if (LocalVarNeeded(j)) then
                call CreateBBox(MaskNames(j-primary%nItems),MaskExprs(j-primary%nItems),self%ExtDataState,rc=status)
                VERIFY_(STATUS)
             end if
          end do
       end if
    end do

    self%primary%nItems = count(PrimaryVarNeeded)
    self%derived%nItems = count(DerivedVarNeeded)

    allocate(self%primary%item(PrimaryItemCount),__STAT__)
    if (DerivedItemCount > 0) allocate(self%derived%item(DerivedItemCount),__STAT__)

    counter = 0
    do i=1,primary%nItems
       if (PrimaryVarNeeded(i)) then
          counter = counter + 1
          self%primary%item(counter) = primary%item(i)
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  If this is a bundle prepare the bundle using 
!  a cfio no read call
   do i = 1, self%primary%nItems

      item => self%primary%item(i)

      if (item%vartype == MAPL_BundleItem) then

         call ESMF_ClockGet(CLOCK, currTIME=time, __RC__)
         call ESMF_StateGet(self%ExtDataState, trim(item%name), bundle,__RC__)
         ! let us check that bundle is empty
         call ESMF_FieldBundleGet(bundle, fieldcount = fieldcount , __RC__)
         ASSERT_(fieldcount == 0)
         call MAPL_CFIORead(item%file,time,bundle,noread=.true.,only_vars=item%var,__RC__)

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
         end if
         cycle
      end if

      ! check if refresh interval is zero, prepare internal state accordingly
      if (.not.PrimaryExportIsConstant_(item)) then
         ! first we need to allocate space for the bracketing fields
         ! create specs for fields from export field, then add to internal state, see readforcing for guide
         ! clear out tinterp_spec before we reuse it

         if (item%vartype == MAPL_FieldItem) then

            call ESMF_StateGet(self%ExtDataState, trim(item%name), field,__RC__)
            call ESMF_FieldGet(field,grid=grid,__RC__)
            dims = item%dim
            vloc = item%vloc
            knd  = kind(0.0)
            item%finterp1 = mapl_FieldCreateEmpty(item%var,grid,__RC__)
            call MAPL_FieldAllocCommit(item%finterp1,dims=dims,location=vloc,typekind=knd,hw=0,__RC__)
            item%finterp2 = mapl_FieldCreateEmpty(item%var,grid,__RC__)
            call MAPL_FieldAllocCommit(item%finterp2,dims=dims,location=vloc,typekind=knd,hw=0,__RC__)
    
         else if (item%vartype == MAPL_BundleItem) then

            call ESMF_StateGet(self%ExtDataState, trim(item%name), bundle,__RC__)
            call ESMF_FieldBundleGet(bundle,grid=grid,__RC__)
            call ESMF_ClockGet(CLOCK, currTIME=time, __RC__)
            item%binterp1 = ESMF_FieldBundleCreate( __RC__)
            call ESMF_FieldBundleSet(item%binterp1, GRID=GRID, __RC__)
            item%binterp2 = ESMF_FieldBundleCreate( __RC__)
            call ESMF_FieldBundleSet(item%binterp2, GRID=GRID, __RC__)
            call MAPL_CFIORead(item%file,time,item%binterp1,noread=.true.,only_vars=item%var,__RC__)
            call MAPL_CFIORead(item%file,time,item%binterp2,noread=.true.,only_vars=item%var,__RC__)

         end if

      else

         if (item%vartype == MAPL_FieldItem) then
           
         call ESMF_ClockGet(CLOCK, currTIME=time, __RC__)
         call ESMF_StateGet(self%ExtDataState, trim(item%name), field,__RC__)
         call ESMF_FieldGet(field,grid=grid,dimCount=fieldRank,__RC__)
            if (fieldRank == 2) then
                  call MAPL_GetPointer(self%ExtDataState, ptr2d, trim(item%name),__RC__)
                  call MAPL_CFIORead(trim(item%var), item%file, time, grid, ptr2d, &
                       time_is_cyclic=.true., time_interp=.true.,conservative=item%conservative, &
                       ignoreCase = self%ignoreCase,__RC__)
            else if (fieldRank == 3) then
                  call MAPL_GetPointer(self%ExtDataState, ptr3d, trim(item%name), __RC__)
                  call MAPL_CFIORead(trim(item%var), item%file, time, grid, ptr3d, &
                       time_is_cyclic=.true., time_interp=.true.,conservative=item%conservative, &
                       ignoreCase = self%ignoreCase,__RC__)
            endif

         else if (item%vartype == MAPL_BundleItem) then

            call ESMF_ClockGet(CLOCK, currTIME=time, __RC__)
            call ESMF_StateGet(self%ExtDataState, trim(item%name), bundle,__RC__)
            call MAPL_CFIORead(item%file, time, bundle, &
                 time_is_cyclic=.true., time_interp=.true., only_vars = item%var, &
                 conservative=item%conservative, ignoreCase = self%ignoreCase,__RC__)
 
        end if
 
      endif

      allocate(item%refresh_time,__STAT__)

      call ESMF_TimeSet(item%refresh_time, yy=0, __RC__)
   end do

   do i =1, self%derived%nItems
      allocate(self%derived%item(i)%refresh_time,__STAT__)

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
   if (allocated(MaskNames)) Deallocate(MaskNames)
   if (allocated(MaskExprs)) Deallocate(MaskExprs)
   if (allocated(ConfigList)) Deallocate(ConfigList)

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
   type(ESMF_Grid)                   :: GRID        ! Grid
   type(ESMF_Field)                  :: field       ! Field
   type(ESMF_FieldBundle)            :: bundle
   type(ESMF_Config)                 :: CF          ! Universal Config 

   integer                           :: im, jm, lm  ! 3D Dimensions
   real(ESMF_KIND_R4), pointer       :: lons(:,:)   ! Longitudes
   real(ESMF_KIND_R4), pointer       :: lats(:,:)   ! Latitudes

   integer                           :: nymd, nhms  ! date, time
   real                              :: cdt         ! time step in secs

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: status

   type(PrimaryExport), pointer      :: item
   type(DerivedExport), pointer      :: derivedItem
   integer                           :: i, j

   type(ESMF_Time)                   :: time, refresh_time
   logical                           :: time_interp, time_is_cyclic
   type(MAPL_MetaComp), pointer      :: MAPLSTATE

   integer                           :: begDate, begTime, curDate,curTime
   integer                           :: incSecs, sec, min, hour, secs
   integer                           :: iyr,imm,idd,ihr,imn,isc
   integer                           :: secs1, secs2, timeIndex1,timeIndex2
   integer                           :: nymd1, nhms1, nymd2, nhms2
   integer                           :: status1, status2
   real                              :: alpha
   real, pointer, dimension(:,:)     :: var2d_prev, var2d_next
   real, pointer, dimension(:,:,:)   :: var3d_prev, var3d_next
   type(ESMF_TimeInterval)           :: tinv1,tinv2
   logical                           :: doUpdate
   integer                           :: fieldCount, fieldRank
   character(len=ESMF_MAXSTR), ALLOCATABLE  :: NAMES (:)
   type(ESMF_Field)                  :: field1, field2
   character(len=ESMF_MAXSTR)        :: file_processed, file_processed1, file_processed2

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

   call ESMF_ClockGet(CLOCK, currTIME=time, __RC__)


!  Fill in the internal state with data from the files 
!  ---------------------------------------------------
 
   do i = 1, self%primary%nItems

      item => self%primary%item(i)

      if (item%isConst .or. PrimaryExportIsConstant_(item) ) cycle

      if (item%refresh_template == "0") then
         doUpdate = .true.
      else
         if (.not. associated(item%refresh_time)) then
           doUpdate = .false.
         end if
         refresh_time = timestamp_(time, item%refresh_template, __RC__)
         if (refresh_time /= item%refresh_time) then
            doUpdate = .true.
            item%refresh_time = refresh_time
         else
            doUpdate = .false.
         end if
      end if

      if (doUpdate) then

         call ESMF_TimeValidate(item%time1,rc=status1)
         call ESMF_TimeValidate(item%time2,rc=status2)

         ! first check if this is first time we are in ExtData run and update bracketing times
         if ((status1 /= ESMF_SUCCESS) .and. (status2 /= ESMF_SUCCESS)) then

            ! update left time
            call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"L",item%cyclic,item%climYear,item%interp_time1, & 
                 item%time1,file_processed1,__RC__)
            ! update right time
            call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"R",item%cyclic,item%climYear,item%interp_time2, &
                 item%time2,file_processed2,__RC__)

            ! read bracketing data

            if (item%vartype == MAPL_FieldItem) then

               call MAPL_CFIORead(trim(item%var), file_processed1, item%time1, item%finterp1, &
                    time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                    ignoreCase = self%ignoreCase,__RC__)
               call MAPL_CFIORead(trim(item%var), file_processed2, item%time2, item%finterp2, &
                    time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                    ignoreCase = self%ignoreCase,__RC__)

            else if (item%vartype == MAPL_BundleItem) then

               call MAPL_CFIORead(file_processed1, item%time1, item%binterp1, &
                    time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                    ignoreCase = self%ignoreCase,__RC__)
               call MAPL_CFIORead(file_processed2, item%time2, item%binterp2, &
                    time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                    ignoreCase = self%ignoreCase,__RC__)

            end if

         endif
 
         ! now update bracketing times if neccessary
         if (time >= item%interp_time2) then

            item%interp_time1 = item%interp_time2
            ! update right time
            call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"R",item%cyclic,item%climYear,item%interp_time2, &
                 item%time2, file_processed, __RC__)

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
               call MAPL_CFIORead(item%var, file_processed, item%time2, item%finterp2, &
                    time_is_cyclic=.false., time_interp=.false.,conservative=item%conservative, &
                    ignoreCase = self%ignoreCase, __RC__)

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
               call MAPL_CFIORead(file_processed, item%time2, item%binterp2, &
                  time_is_cyclic=item%cyclic, time_interp=.false.,conservative=item%conservative, &
                  ignoreCase = self%ignoreCase, __RC__)  

               deallocate(names)

            end if

         endif
         ! finally interpolate between bracketing times
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

         end if

      endif

      nullify(item)

   end do

   ! now take care of derived fields
   do i=1,self%derived%nItems

      derivedItem => self%derived%item(i)

      if (derivedItem%refresh_template == "0") then
         doUpdate = .true.
      else
         if (.not. associated(derivedItem%refresh_time)) then
           doUpdate = .false.
         end if
         refresh_time = timestamp_(time, derivedItem%refresh_template, __RC__)
         if (refresh_time /= derivedItem%refresh_time) then
            doUpdate = .true.
            item%refresh_time = refresh_time
         else
            doUpdate = .false.
         end if
         if (DerivedExportIsConstant_(derivedItem) .and. associated(derivedItem%refresh_time)) then
             deallocate(self%derived%item(i)%refresh_time)
             self%derived%item(i)%refresh_time => null()
         end if
      end if

      if (doUpdate) then

         call CalcDerivedField(self%ExtDataState,self%primary,derivedItem%name,derivedItem%expression,__RC__)

      end if
   end do

!  All done
!  --------

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
   type(ESMF_Grid)                   :: GRID        ! Grid
   type(ESMF_Config)                 :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: status

   integer                           :: i
   type(ESMF_Field)                  :: field


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
               call MAPL_FieldDestroy(self%primary%item(i)%finterp1,__RC__)
               call MAPL_FieldDestroy(self%primary%item(i)%finterp2,__RC__)
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

    type(MAPL_MetaComp), pointer  :: MC
    type(ESMF_Time)      :: TIME
    type(MAPL_ExtData_Wrap)  :: wrap
    integer              :: iyr, imm, idd, ihr, imn, isc
    integer              :: dims(3)

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

  end function timestamp_
 
  subroutine CreateTimeInterval(item,clock,rc)
     type(PrimaryExport)      , intent(inout) :: item
     type(ESMF_Clock)          , intent(in   ) :: clock
     integer, optional         , intent(out  ) :: rc

     __Iam__('CreateTimeInterval')
     
     integer                    :: nymd, nhms, iyy,imm,idd,ihh,imn,isc
     integer                    :: lasttoken
     character(len=2)           :: token
     type(ESMF_Time)            :: time
     integer                    :: spos(2),cindex

     if (item%int_frequency < 0) then
        ! if int_frequency is less than zero than try to guess it from the file template
        ! if that fails then it must be a single file or a climatology 
        call ESMF_ClockGet (CLOCK, currTIME=time, __RC__)

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
        ! user provided data about the file frequency, reference time, and frequency units
        ! so we go with what they provide
        cindex = index(item%creff_time,'-')
        spos(1)=cindex-4
        spos(2)=cindex-1
        read(item%creff_time(spos(1):spos(2)),*)iyy
        cindex = index(item%creff_time,'-')
        spos(1)=cindex+1
        spos(2)=cindex+2
        read(item%creff_time(spos(1):spos(2)),*)imm
        cindex = index(item%creff_time,'-',back=.true.)
        spos(1)=cindex+1
        spos(2)=cindex+2
        read(item%creff_time(spos(1):spos(2)),*)idd
        cindex = index(item%creff_time,':')
        spos(1)=cindex-2
        spos(2)=cindex-1
        read(item%creff_time(spos(1):spos(2)),*)ihh
        cindex = index(item%creff_time,':')
        spos(1)=cindex+1
        spos(2)=cindex+2
        read(item%creff_time(spos(1):spos(2)),*)imn
        cindex = index(item%creff_time,':',back=.true.)
        spos(1)=cindex+1
        spos(2)=cindex+2
        read(item%creff_time(spos(1):spos(2)),*)isc
        call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=isc,rc=status)
        VERIFY_(STATUS)
        if (trim(item%tunits) == "years") then
           call ESMF_TimeIntervalSet(item%frequency,yy=item%int_frequency,rc=status)
           VERIFY_(STATUS)
        else if (trim(item%tunits) == "months") then
           call ESMF_TimeIntervalSet(item%frequency,mm=item%int_frequency,rc=status)
           VERIFY_(STATUS)
        else if (trim(item%tunits) == "days") then
           call ESMF_TimeIntervalSet(item%frequency,d=item%int_frequency,rc=status)
           VERIFY_(STATUS)
        else if (trim(item%tunits) == "hours") then
           call ESMF_TimeIntervalSet(item%frequency,h=item%int_frequency,rc=status)
           VERIFY_(STATUS)
        else if (trim(item%tunits) == "minutes") then
           call ESMF_TimeIntervalSet(item%frequency,m=item%int_frequency,rc=status)
           VERIFY_(STATUS)
        else
           ASSERT_(.false.)
        end if
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
     character(len=ESMF_MAXSTR) :: file
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
     call ESMF_CFIOFileOpen  (CFIO, FMODE=1, cyclic=item%cyclic,__RC__)
     begDate = cfio%date
     call MAPL_UnpackTime(begDate,iyr,imm,idd)
     item%climyear = iyr
     call ESMF_CFIODestroy(CFIO,__RC__)

  end subroutine GetClimYear

  subroutine UpdateBracketTime(file_tmpl,cTime,reffTime,frequency,bSide,cyclic,climYear,interpTime,fileTime,file_processed,rc)
     character(len=ESMF_MAXSTR),          intent(in   ) :: file_tmpl
     type(ESMF_Time),                     intent(inout) :: cTime
     type(ESMF_Time),                     intent(inout) :: reffTime
     type(ESMF_TimeInterval),             intent(inout) :: frequency
     character(len=1),                    intent(in   ) :: bSide
     logical,                             intent(in   ) :: cyclic
     integer,                             intent(in   ) :: climYear
     type(ESMF_TIME),                     intent(inout) :: interpTime
     type(ESMF_TIME),                     intent(inout) :: fileTime
     character(len=ESMF_MAXSTR),          intent(inout) :: file_processed
     integer, optional,                   intent(out  ) :: rc

     __Iam__('UpdateBracketTime')

     integer                                    :: status_freq
     type(ESMF_CFIO)                            :: cfio
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
        ! if the file is constant, i.e. no tokens in in the template
        ! but it was marked as cyclic we must have a year long climatology 
        ! on one file, set UniFileClim to true
        if (cyclic) UniFileClim = .true.
        file_processed = file_tmpl
        call GetBracketTimeOnFile(file_tmpl,cTime,bSide,UniFileClim,interpTime,fileTime,rc=status)
        if (status /= ESMF_SUCCESS) then
           RETURN_(ESMF_FAILURE)
        end if
     else 
        UniFileClim = .false.
        if (cyclic) then
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

              if (cyclic) then
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

              if (cyclic) then
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

        if (cyclic) then
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
     integer                            :: nhms,nymd, nhmsB, nymdB, incSecs
     integer                            :: secs, begDate, begTime, curDate, curTime
     integer                            :: timeIndex1, timeIndex2, timeIndex
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

  subroutine CalcDerivedField(state,primaries,exportName,exportExpr,rc)
     type(ESMF_State),        intent(inout) :: state
     type(PrimaryExports),    intent(inout) :: primaries
     character(len=*),        intent(in   ) :: exportName     
     character(len=*),        intent(in   ) :: exportExpr
     integer, optional,       intent(out  ) :: rc

     __Iam__('CalcDerivedField')

     character(len=ESMF_MAXSTR), pointer    :: field_names(:)
     integer                                :: i,i1
     character(len=ESMF_MAXSTR)             :: strtmp
     type(ESMF_Field)                   :: field

     allocate(field_names(primaries%nItems))
     do i=1,primaries%nItems
        field_names(i) = primaries%item(i)%name
     end do

     ! if neither call evalDerivedExpression
     call ESMF_StateGet(state,exportName,field,__RC__)
     call MAPL_StateEval(state,exportExpr,field,__RC__)
     RETURN_(ESMF_SUCCESS)
     deallocate(field_names)

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
     type(ESMF_Field)         :: field
     type(ESMF_Field)         :: flds(1)
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
     call ESMF_FieldGet(field,grid=grid,__RC__)

     VarName=trim(MaskName)
     DIMS = MAPL_DimsHorzOnly
     KND = kind(0.0)
     HW = 0
     VLOC = MAPL_VLocationNone
     field = mapl_FieldCreateEmpty(VarName,grid,__RC__)
     call MAPL_FieldAllocCommit(field,dims=dims,location=vloc,typekind=knd,hw=hw,__RC__)
     call MAPL_StateAdd(ExtDataState,field,__RC__)

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

  subroutine MAPL_ExtDataInterpField(item,time,field,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_Time),     intent(in   ) :: time
     type(ESMF_Field),    intent(inout) :: field
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
     integer                    :: fieldRank
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
           end if
           call ESMF_FieldGet(field, localDE=0, farrayPtr=var2d, __RC__)
           ! only interpolate if we have to
           if (time == item%interp_time1) then
              var2d = var2d_prev
           else if (time == item%interp_time2) then
              var2d = var2d_next
           else
              var2d = var2d_prev + alpha*(var2d_next-var2d_prev)
           end if
           if (item%do_scale .and. (.not.item%do_offset)) var2d = item%scale*var2d
           if ((.not.item%do_scale) .and. item%do_offset) var2d = var2d+item%offset
           if (item%do_scale .and. item%do_offset) var2d = item%offset + (item%scale * var2d)
      else if (fieldRank == 3) then
           if (item%vartype == MAPL_FieldItem) then
              call ESMF_FieldGet(item%finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
              call ESMF_FieldGet(item%finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
           else if (item%vartype == MAPL_BundleItem) then
              call ESMFL_BundleGetPointerToData(item%binterp1,name,var3d_prev,__RC__)
              call ESMFL_BundleGetPointerToData(item%binterp2,name,var3d_next,__RC__)
           end if
           call ESMF_FieldGet(field, localDE=0, farrayPtr=var3d, __RC__)
           ! only interpolate if we have to
           if (time == item%interp_time1) then
              var3d = var3d_prev
           else if (time == item%interp_time2) then
              var3d = var3d_next
           else
              var3d = var3d_prev + alpha*(var3d_next-var3d_prev)
           end if
           if (item%do_scale .and. (.not.item%do_offset)) var3d = item%scale*var3d
           if ((.not.item%do_scale) .and. item%do_offset) var3d = var3d+item%offset
           if (item%do_scale .and. item%do_offset) var3d = item%offset + (item%scale * var3d)
     endif

     RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_ExtDataInterpField

 END MODULE MAPL_ExtDataGridCompMod
