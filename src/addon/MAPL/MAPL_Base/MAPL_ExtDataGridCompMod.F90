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
!BOP
!
! !MODULE: MAPL_ExtDataGridCompMod - Implements Interface to External Data
!
! !INTERFACE:
!
   MODULE MAPL_ExtDataGridCompMod
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
!
! !DESCRIPTION: 
!
!  {\tt MAPL_ExtDataGridComp} is an ESMF gridded component implementing
!  an interface to boundary conditions and other types of external data
!  files.
!
!  Developed for GEOS-5 release Fortuna 2.0 and later.
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
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
     real                         :: scale, offset
     character(len=ESMF_MAXSTR)   :: var
     character(len=ESMF_MAXSTR)   :: file
     character(len=256        )   :: creff_time
     character(len=ESMF_MAXSTR)   :: tunits
     integer                      :: int_frequency

     type(ESMF_Time), pointer     :: refresh_time => null()
     logical                      :: isConst
     real                         :: Const

!    variables for zero refresh time
     type(ESMF_State)             :: tinterp
     type(ESMF_Time)              :: time1, time2
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
  end type MAPL_ExtData_State

! Hook for the ESMF
! -----------------
  type MAPL_ExtData_Wrap
     type (MAPL_ExtData_State), pointer :: PTR => null()
  end type MAPL_ExtData_WRAP


  real, parameter :: ZERO_TIME_INTERVAL = 1e-3    ! 'zero' time interval in [s]

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
   type(ESMF_Config)                 :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: Status
   character(len=ESMF_MAXSTR)        :: buffer

   real, pointer, dimension(:,:)     :: var2d
   real, pointer, dimension(:,:,:)   :: var3d

   type(PrimaryExports)              :: Primary
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
   type(ESMF_Field)                  :: tinterp_prev
   type(ESMF_Field)                  :: tinterp_next
   integer                           :: counter
   integer                           :: USE_EXTDATA
   real, pointer                     :: ptr2d(:,:) => null()
   real, pointer                     :: ptr3d(:,:,:) => null()
   real                              :: const
   integer                           :: k, ios

!  Get my name and set-up traceback handle
!  ---------------------------------------
   Iam = 'Initialize_'
   call ESMF_GridCompGet( GC, name=comp_name, config=CF, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF, __RC__)

! Get information from export state
!----------------------------------

    call ESMF_StateGet(EXPORT, ITEMCOUNT=ItemCount, RC=STATUS)
    VERIFY_(STATUS)

    ! set ExtData on by default, let user turn it off if they want
    call ESMF_ConfigGetAttribute(CF,USE_EXTDATA, Label='USE_EXTDATA:',default=1,rc=status)
    if (USE_EXTDATA) then
       self%active  = .true.
    else
       self%active = .false.
    end if

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

!   Set the number of primary and derived exports to zero
    primary%nItems = 0
    derived%nItems = 0

!   Primary Exports
!   ---------------
    call ESMF_ConfigGetDim(CF, nLines, nCols, LABEL='PrimaryExports::', __RC__)

    if ( nLines > 0 ) then
       primary%nItems = nLines
       allocate (PrimaryVarNames(nLines), stat=STATUS)
       VERIFY_(STATUS)
       allocate (PrimaryVarNeeded(nLines), stat=STATUS)
       VERIFY_(STATUS)
       PrimaryVarNeeded = .false.

       allocate(primary%item(nLines), stat=STATUS)
       VERIFY_(STATUS)

       call ESMF_ConfigFindLabel(CF, 'PrimaryExports::', __RC__)

       do i = 1, nLines
          call ESMF_ConfigNextLine(CF, __RC__)

          call ESMF_ConfigGetAttribute(CF, primary%item(i)%name,  __RC__)
          PrimaryVarNames(i) = primary%item(i)%name
          call ESMF_ConfigGetAttribute(CF, primary%item(i)%units, __RC__)

          call ESMF_ConfigGetAttribute(CF, buffer, __RC__)
          call ESMF_StringLowerCase(buffer, iret)
          if (buffer == 'xy') then
             primary%item(i)%dim = MAPL_DimsHorzOnly
          else if (buffer == 'xyz') then
             primary%item(i)%dim = MAPL_DimsHorzVert
          else
             __raise__(MAPL_RC_ERROR, "invalid dimension for Primary Export")
          end if

          call ESMF_ConfigGetAttribute(CF, buffer, __RC__)
          call ESMF_StringLowerCase(buffer, iret)
          if (buffer == 'c') then
             primary%item(i)%vloc = MAPL_VLocationCenter
          else if (buffer == 'e') then
             primary%item(i)%vloc = MAPL_VLocationEdge
          else
             __raise__(MAPL_RC_ERROR, "invalid vLocation for Primary Export")
          end if
    
          call ESMF_ConfigGetAttribute(CF, buffer, __RC__)
          call ESMF_ConfigGetAttribute(CF, primary%item(i)%refresh_template, __RC__)
          call ESMF_ConfigGetAttribute(CF, primary%item(i)%offset, __RC__)
          call ESMF_ConfigGetAttribute(CF, primary%item(i)%scale,  __RC__)
          call ESMF_ConfigGetAttribute(CF, primary%item(i)%var,    __RC__)
          call ESMF_ConfigGetAttribute(CF, primary%item(i)%file,   __RC__)

          call ESMF_ConfigGetAttribute(CF, primary%item(i)%creff_time, rc=status)
          if (status /= ESMF_SUCCESS) then
             primary%item(i)%creff_time = ""
          end if             
          call ESMF_ConfigGetAttribute(CF, primary%item(i)%tunits, rc=status)
          if (status /= ESMF_SUCCESS) then
             primary%item(i)%tunits = ""
          end if             
          call ESMF_ConfigGetAttribute(CF, primary%item(i)%int_frequency, rc=status)
          if (status /= ESMF_SUCCESS) then
             primary%item(i)%int_frequency = -1
          end if             
 
          call ESMF_StringLowerCase(buffer, iret)
          if (trim(buffer) == 'n') primary%item(i)%cyclic = .false.
          if (trim(buffer) == 'y') primary%item(i)%cyclic = .true.

!         assume we will allocate
          primary%item(i)%ExtDataAlloc = .true.
!         check if this is going to be a constant
          primary%item(i)%isConst = .false.
          if (primary%item(i)%file(1:9) == '/dev/null') then
             primary%item(i)%isConst = .true.
             ios = -1
             k = index(primary%item(i)%file,':')
             if ( k > 9 ) then
                  read(primary%item(i)%file(k+1:),*,iostat=ios) primary%item(i)%const
             end if
             if ( ios /= 0 ) primary%item(i)%const = 0.0
          end if

          if ((trim(primary%item(i)%refresh_template) == "0") .and. (primary%item(i)%isConst ==.false.) ) then
             call CreateTimeInterval(primary%item(i),clock,__RC__)
          end if

       end do
    else
       primary%nItems = 0
       primary%item => null()
    end if ! end of primary exports

!   Derived Exports
!   ---------------
    call ESMF_ConfigGetDim(CF, nLines, nCols, Label='DerivedExports::', rc=status)

    if ( nLines > 0 ) then
       Allocate(DerivedVarNeeded(nLines))
       DerivedVarNeeded = .false.
       derived%nItems = nLines
       allocate(derived%item(nLines))
       call ESMF_ConfigFindLabel(CF, 'DerivedExports::', __RC__)
       do i=1,nLines
          call ESMF_ConfigNextLine(CF, __RC__)
          call ESMF_ConfigGetAttribute(CF,derived%item(i)%name,__RC__)
          call ESMF_ConfigGetAttribute(CF,derived%item(i)%expression,__RC__)
          call ESMF_ConfigGetAttribute(CF,derived%item(i)%refresh_template, __RC__)
          derived%item(i)%ExtDataAlloc = .true.
       end do
    else
       derived%nItems = 0
       derived%item => null()
    end if

!   Masks
!   ---------------
    call ESMF_ConfigGetDim(CF, nLines, nCols, Label='Masks::', __RC__)
    if (nLines > 0) then
       allocate(MaskNames(nLines),stat=status)
       VERIFY_(STATUS)
       allocate(MaskExprs(nLines),stat=status)
       VERIFY_(STATUS)
       call ESMF_ConfigFindLabel(CF, 'Masks::', __RC__)
       do i = 1, nLines
          call ESMF_ConfigNextLine(CF, __RC__)
          call ESMF_ConfigGetAttribute(CF, MaskNames(i),  __RC__)
          call ESMF_ConfigGetAttribute(CF, MaskExprs(i), __RC__)
       end do
       nMasks=nLines
    else
       nMasks=0
    end if

!   Done parsing resource file    

    PrimaryItemCount = 0
    DerivedItemCount = 0

!   find items in primary and derived to fullfill Export state
!   once we find primary or derived put in namespace
    self%ExtDataState = ESMF_StateCreate(Name="ExtDataNameSpace",__RC__)
    do I = 1, ItemCount
       ASSERT_(ITEMTYPES(I) == ESMF_StateItem_Field)

       found = .false.
       do J = 1, primary%nItems
          if (ItemNames(I) == primary%item(J)%name) then
             found = .true.
             PrimaryItemCount = PrimaryItemCount + 1
             PrimaryVarNeeded(j) = .true.
             primary%item(j)%ExtDataAlloc = .false.
             VarName=trim(primary%item(J)%name)
             call ESMF_StateGet(Export,VarName,field,__RC__)
             call MAPL_StateAdd(self%ExtDataState,field,__RC__)
             exit
          end if
       end do
       if ( (.not.found) .and. (derived%nItems > 0) ) then
          do J = 1, derived%nItems
             if (ItemNames(I) == derived%item(J)%name) then
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
          call CheckSyntax(derived%item(j)%expression,VarNames,LocalVarNeeded,__RC__)
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

!  Read the single step files (read interval equal to zero)
!  --------------------------------------------------------

   do i = 1, self%primary%nItems

      if (self%primary%item(i)%isConst) then
         if (self%primary%item(i)%dim == MAPL_DimsHorzOnly) then
               call MAPL_GetPointer(self%ExtDataState, ptr2d, trim(self%primary%item(i)%name),__RC__)
               ptr2d = self%primary%item(i)%const
         else if (self%primary%item(i)%dim == MAPL_DimsHorzVert) then
               call MAPL_GetPointer(self%ExtDataState, ptr3d, trim(self%primary%item(i)%name), __RC__)
               ptr2d = self%primary%item(i)%const
         endif
         cycle
      end if

      ! check if refresh interval is zero, prepare internal state accordingly
      if (trim(self%primary%item(i)%refresh_template)=="0") then
         ! first we need to allocate space for the bracketing fields
         ! create specs for fields from export field, then add to internal state, see readforcing for guide
         ! clear out tinterp_spec before we reuse it
         self%primary%item(i)%tinterp = ESMF_StateCreate(name=trim(self%primary%item(i)%name)//'_State',__RC__)

         call ESMF_StateGet(self%ExtDataState, trim(self%primary%item(i)%name), field,__RC__)
         call ESMF_FieldGet(field,grid=grid,__RC__)
         dims = self%primary%item(i)%dim
         vloc = self%primary%item(i)%vloc
         knd  = kind(0.0)
         tinterp_prev = mapl_FieldCreateEmpty('tinterp_prev',grid,__RC__)
         call MAPL_FieldAllocCommit(tinterp_prev,dims=dims,location=vloc,typekind=knd,hw=0,__RC__)
         call MAPL_StateAdd(self%primary%item(i)%tinterp,tinterp_prev,__RC__)
         tinterp_next = mapl_FieldCreateEmpty('tinterp_next',grid,__RC__)
         call MAPL_FieldAllocCommit(tinterp_next,dims=dims,location=vloc,typekind=knd,hw=0,__RC__)
         call MAPL_StateAdd(self%primary%item(i)%tinterp,tinterp_next,__RC__)

      endif

      allocate(self%primary%item(i)%refresh_time,__STAT__)

      call ESMF_TimeSet(self%primary%item(i)%refresh_time, yy=0, __RC__)
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
   deallocate(primary%item)
   if (associated(derived%item)) deallocate(derived%item)
   deallocate(ItemTypes)
   deallocate(ItemNames)
   deallocate(PrimaryVarNames)
   deallocate(PrimaryVarNeeded)
   deallocate(VarNames)
   if (allocated(DerivedVarNeeded)) deallocate(DerivedVarNeeded)
   deallocate(LocalVarNeeded)
   if (allocated(MaskNames)) Deallocate(MaskNames)
   if (allocated(MaskExprs)) Deallocate(MaskExprs)

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
   type(ESMF_Config)                 :: CF          ! Universal Config 

   integer                           :: im, jm, lm  ! 3D Dimensions
   real(ESMF_KIND_R4), pointer       :: lons(:,:)   ! Longitudes
   real(ESMF_KIND_R4), pointer       :: lats(:,:)   ! Latitudes

   integer                           :: nymd, nhms  ! date, time
   real                              :: cdt         ! time step in secs

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: status

   real, pointer, dimension(:,:)     :: var2d
   real, pointer, dimension(:,:,:)   :: var3d

   type(PrimaryExport), pointer      :: item
   integer                           :: i

   type(ESMF_Time)                   :: time, refresh_time
   logical                           :: time_interp, time_is_cyclic

   integer                           :: begDate, begTime, curDate,curTime
   integer                           :: incSecs, sec, min, hour, secs
   integer                           :: iyr,imm,idd,ihr,imn,isc
   integer                           :: secs1, secs2, timeIndex1,timeIndex2
   integer                           :: nymd1, nhms1, nymd2, nhms2
   integer                           :: status1, status2
   real                              :: alpha
   real, pointer, dimension(:,:)     :: var2d_prev, var2d_next
   real, pointer, dimension(:,:,:)   :: var3d_prev, var3d_next
   type(ESMF_Field)                  :: tinterp_field
   type(ESMF_TimeInterval)           :: tinv1,tinv2

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

   call ESMF_ClockGet(CLOCK, currTIME=time, __RC__)


!  Fill in the internal state with data from the files 
!  ---------------------------------------------------
 
   do i = 1, self%primary%nItems

      item => self%primary%item(i)

      if (item%isConst) cycle

      if (item%refresh_template == "0") then

         call ESMF_TimeValidate(item%time1,rc=status1)
         call ESMF_TimeValidate(item%time2,rc=status2)

         ! first check if this is first time we are in ExtData run and update bracketing times
         if ((status1 /= ESMF_SUCCESS) .and. (status2 /= ESMF_SUCCESS)) then

            ! update left time
            call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"L",item%cyclic,item%time1,__RC__)
            ! update right time
            call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"R",item%cyclic,item%time2,__RC__)

            ! read bracketing data
            call ESMF_StateGet(self%ExtDataState, trim(item%name), field,__RC__)
            call ESMF_FieldGet(field, grid=grid,__RC__)
            if (item%dim == MAPL_DimsHorzOnly) then
               call MAPL_GetPointer(item%tinterp, var2d_prev, "tinterp_prev",__RC__) 
               call MAPL_GetPointer(item%tinterp, var2d_next, "tinterp_next",__RC__) 
               call MAPL_CFIORead(trim(item%var), item%file, item%time1, grid, var2d_prev, &
                    time_is_cyclic=item%cyclic, time_interp=.false.,__RC__)
               call MAPL_CFIORead(trim(item%var), item%file, item%time2, grid, var2d_next, &
                    time_is_cyclic=item%cyclic, time_interp=.false.,__RC__)
            else if (item%dim == MAPL_DimsHorzVert) then
               call MAPL_GetPointer(item%tinterp, var3d_prev, "tinterp_prev",__RC__) 
               call MAPL_GetPointer(item%tinterp, var3d_next, "tinterp_next",__RC__) 
               call MAPL_CFIORead(trim(item%var), item%file, item%time1, grid, var3d_prev, &
                    time_is_cyclic=item%cyclic, time_interp=.false.,__RC__)
               call MAPL_CFIORead(trim(item%var), item%file, item%time2, grid, var3d_next, &
                    time_is_cyclic=item%cyclic, time_interp=.false.,__RC__)
            endif

         endif
 
         ! now update bracketing times if neccessary
         if (time >= item%time2) then

            item%time1 = item%time2
            ! update right time
            call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"R",item%cyclic,item%time2,__RC__)

            call ESMF_StateGet(self%ExtDataState, trim(item%name), field,__RC__)
            call ESMF_FieldGet(field, grid=grid,__RC__)
            if (item%dim == MAPL_DimsHorzOnly) then
               call MAPL_GetPointer(item%tinterp, var2d_prev, "tinterp_prev",__RC__) 
               call MAPL_GetPointer(item%tinterp, var2d_next, "tinterp_next",__RC__) 
               var2d_prev=var2d_next
               call MAPL_CFIORead(item%var, item%file, item%time2, grid, var2d_next, &
                    time_is_cyclic=item%cyclic, time_interp=.false., __RC__)
            else if (item%dim == MAPL_DimsHorzVert) then
               call MAPL_GetPointer(item%tinterp, var3d_prev, "tinterp_prev",__RC__) 
               call MAPL_GetPointer(item%tinterp, var3d_next, "tinterp_next",__RC__) 
               var3d_prev=var3d_next
               call MAPL_CFIORead(item%var, item%file, item%time2, grid, var3d_next, &
                    time_is_cyclic=item%cyclic, time_interp=.false.,__RC__)
            endif

         endif
         ! finally interpolate between bracketing times
         tinv1 = time - item%time1
         tinv2 = item%time2 - item%time1
         alpha = tinv1/tinv2
         if (item%dim == MAPL_DimsHorzOnly) then
               call MAPL_GetPointer(item%tinterp, var2d_prev, "tinterp_prev",__RC__) 
               call MAPL_GetPointer(item%tinterp, var2d_next, "tinterp_next",__RC__) 
               call MAPL_GetPointer(self%ExtDataState, var2d, trim(item%name),__RC__)
               var2d = var2d_prev + alpha*(var2d_next-var2d_prev)
               var2d = item%offset + (item%scale * var2d)
         else if (item%dim == MAPL_DimsHorzVert) then
               call MAPL_GetPointer(item%tinterp, var3d_prev, "tinterp_prev",__RC__) 
               call MAPL_GetPointer(item%tinterp, var3d_next, "tinterp_next",__RC__) 
               call MAPL_GetPointer(self%ExtDataState, var3d, trim(item%name),__RC__)
               var3d = var3d_prev + alpha*(var3d_next-var3d_prev) 
               var3d = item%offset + (item%scale * var3d)
         endif 

      else

         if (.not. associated(item%refresh_time)) then
           cycle ! skipp reading constant fields once they are read
         end if

         if (PrimaryExportIsConstant_(item)) then
            time_interp    = .true.
            time_is_cyclic = .true.
         else
            time_interp    = .true.
            time_is_cyclic = item%cyclic
         end if
      
         refresh_time = timestamp_(time, item%refresh_template, __RC__)
 
         if (refresh_time /= item%refresh_time) then    ! update 

            call ESMF_StateGet(self%ExtDataState, trim(item%name), field, rc=status)
            VERIFY_(STATUS)
            call ESMF_FieldGet(field, grid=grid, rc=status)
            VERIFY_(STATUS)

            item%refresh_time = refresh_time

            if (item%dim == MAPL_DimsHorzOnly) then
               call MAPL_GetPointer(self%ExtDataState, var2d, trim(item%name), __RC__)

               if (associated(var2d)) then
                  call MAPL_CFIORead(trim(item%var),          &
                                     trim(item%file),         &
                                     item%refresh_time,       &
                                     grid, var2d,             &
                                     time_interp=time_interp, &
                                     time_is_cyclic=time_is_cyclic, __RC__)

                  ! apply the units transformation
                  var2d = item%offset + (item%scale * var2d)
               end if
            else if (item%dim == MAPL_DimsHorzVert) then
               call MAPL_GetPointer(self%ExtDataState, var3d, trim(item%name), __RC__)
 
               if (associated(var3d)) then
                  call MAPL_CFIORead(trim(item%var),          &
                                     trim(item%file),         &
                                     item%refresh_time,       & 
                                     grid, var3d,             &
                                     time_interp=time_interp, &
                                     time_is_cyclic=time_is_cyclic, __RC__)

                  ! apply the units transformation
                  var3d = item%offset + (item%scale * var3d)
               end if
            else
               __raise__(MAPL_RC_ERROR, "Invalid dimensions for Primary Export")
            end if

         end if      

         ! nullify the refresh_time for the constant fields once they are read
         if (PrimaryExportIsConstant_(item) .and. associated(item%refresh_time)) then
             deallocate(self%primary%item(i)%refresh_time)
             self%primary%item(i)%refresh_time => null()
         end if

      endif

      nullify(item)

   end do

   ! now take care of derived fields
   do i=1,self%derived%nItems

      if (self%derived%item(i)%refresh_template == '0') then
         call CalcDerivedField(self%ExtDataState,self%primary,self%derived%item(i)%name,self%derived%item(i)%expression,__RC__)
      else
        if (.not. associated(self%derived%item(i)%refresh_time)) then
           cycle ! skipp reading constant fields once they are read
        end if

        refresh_time = timestamp_(time,self%derived%item(i)%refresh_template, __RC__)
        if (refresh_time /= self%derived%item(i)%refresh_time) then    ! update 

           self%derived%item(i)%refresh_time = refresh_time

           call CalcDerivedField(self%ExtDataState,self%primary,self%derived%item(i)%name,self%derived%item(i)%expression,__RC__)

           if (DerivedExportIsConstant_(self%derived%item(i)) .and. associated(self%derived%item(i)%refresh_time)) then
              deallocate(self%derived%item(i)%refresh_time)
              self%derived%item(i)%refresh_time => null()
          end if

        end if

      end if
   end do

!  All done
!  --------
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
            call ESMF_StateGet(self%primary%item(i)%tinterp,'tinterp_prev',field,__RC__)
            call MAPL_FieldDestroy(field,__RC__)
            call ESMF_StateGet(self%primary%item(i)%tinterp,'tinterp_next',field,__RC__)
            call MAPL_FieldDestroy(field,__RC__)
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
        call ESMF_ClockGet (CLOCK, currTIME=time, __RC__)

        call ESMF_TimeGet(time, yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,__RC__)
        lasttoken = index(item%file,'%',back=.true.)
        if (lasttoken.gt.0) then
           token = item%file(lasttoken+1:lasttoken+2)
           select case(token)
           case("y4") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=0,dd=0,h=0,m=0,s=0,__RC__)
              call ESMF_TimeIntervalSet(item%frequency,yy=1,__RC__)
           case("m2") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=0,h=0,m=0,s=0,__RC__)
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
           ASSERT_(.false.)
        end if
     else
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

  subroutine UpdateBracketTime(file_tmpl,cTime,reffTime,frequency,bSide,cyclic,bracketTime,rc)
     character(len=ESMF_MAXSTR),          intent(in   ) :: file_tmpl
     type(ESMF_Time),                     intent(inout) :: cTime
     type(ESMF_Time),                     intent(inout) :: reffTime
     type(ESMF_TimeInterval),             intent(inout) :: frequency
     character(len=1),                    intent(in   ) :: bSide
     logical,                             intent(in   ) :: cyclic
     type(ESMF_TIME),                     intent(inout) :: bracketTime
     integer, optional,                   intent(out  ) :: rc

     __Iam__('UpdateBracketTime')

     integer                                    :: status_prev,status_next,status_freq
     character(len=ESMF_MAXSTR)                 :: file_processed
     type(ESMF_CFIO)                            :: cfio
     type(ESMF_Time)                            :: newTime,fileTime
     integer                                    :: curDate,curTime,n
     integer(ESMF_KIND_I4)                      :: iyr, imm, idd, ihr, imn, isc
     type(ESMF_TimeInterval)                    :: tint
    
     tint=cTime-reffTime
     n=floor(tint/frequency)
     filetime = reffTime+(n*frequency)
     ! untemplate file
     call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
     call MAPL_PackTime(curDate,iyr,imm,idd)
     call MAPL_PackTime(curTime,ihr,imn,isc)
     call gx_(file_processed,file_tmpl,nymd=curDate,nhms=curTime,__STAT__)
     ! try to get bracketing time on file using current time
     call GetBracketTimeOnFile(file_processed,cTime,bSide,cyclic,bracketTime,rc=status)
     if (status /= ESMF_SUCCESS) then

        call ESMF_TimeIntervalValidate(frequency,rc=status_freq)
        if (status_freq /= ESMF_SUCCESS) then
           if (mapl_am_I_root()) write(*,*)'ExtData could not find bracketing data from file template ',trim(file_tmpl)
           RETURN_(ESMF_FAILURE)
        end if
         

        ! check next time
        newTime = fileTime + frequency
        ! untemplate file
        call ESMF_TimeGet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        call MAPL_PackTime(curDate,iyr,imm,idd)
        call MAPL_PackTime(curTime,ihr,imn,isc)
        call gx_(file_processed,file_tmpl,nymd=curDate,nhms=curTime,__STAT__)
        ! try to get bracketing time on file using new time
        call GetBracketTimeOnFile(file_processed,cTime,bSide,cyclic,bracketTime,rc=status_next)
        if (status_next /= ESMF_SUCCESS) then
           ! check next time
           newTime = fileTime - frequency
           ! untemplate file
           call ESMF_TimeGet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           call MAPL_PackTime(curDate,iyr,imm,idd)
           call MAPL_PackTime(curTime,ihr,imn,isc)
           call gx_(file_processed,file_tmpl,nymd=curDate,nhms=curTime,__STAT__)
           ! try to get bracketing time on file using new time
           call GetBracketTimeOnFile(file_processed,cTime,bSide,cyclic,bracketTime,rc=status_prev)
           if (status_prev /= ESMF_SUCCESS) then
              if (mapl_am_I_root()) write(*,*)'ExtData could not find bracketing data from file template ',trim(file_tmpl)
              RETURN_(ESMF_FAILURE)
           else
              RETURN_(ESMF_SUCCESS)
           end if
        end if
     else
        RETURN_(ESMF_SUCCESS)
     endif

    
  end subroutine UpdateBracketTime

  subroutine GetBracketTimeOnFile(file,cTime,bSide,cyclic,bracketTime,rc)
     character(len=ESMF_MAXSTR),          intent(in   ) :: file
     type(ESMF_Time),                     intent(inout) :: cTime
     character(len=1),                    intent(in   ) :: bSide
     logical,                             intent(in   ) :: cyclic
     type(ESMF_TIME),                     intent(inout) :: bracketTime
     integer, optional,                   intent(out  ) :: rc

     __Iam__('GetBracketTimeOnFile')

     type(ESMF_CFIO)                    :: cfio
     integer(ESMF_KIND_I4)              :: iyr,imm,idd,ihr,imn,isc
     integer                            :: iCurrInterval,i
     integer                            :: nhms,nymd, nhmsB, nymdB, incSecs
     integer                            :: secs, begDate, begTime, curDate, curTime
     integer                            :: timeIndex1, timeIndex2, timeIndex
     type(ESMF_Time), pointer           :: tSeries(:)
     logical                            :: found

     cfio =  ESMF_CFIOCreate (cfioObjName='cfio_obj',__RC__)
     call ESMF_CFIOSet(CFIO, fName=trim(file),__RC__)
     call ESMF_CFIOFileOpen  (CFIO, FMODE=1, cyclic=cyclic,__RC__)
     begDate = cfio%date
     begTime = cfio%begTime
     call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
     call MAPL_PackTime(curDate,iyr,imm,idd)
     call MAPL_PackTime(curTime,ihr,imn,isc)
     call CFIO_parseIntTime(cfio%timeInc,ihr,imn,isc)
     incSecs = isc + 60*(imn +(60*ihr))

     if (cyclic) then
        secs = DiffDate (begDate, begTime, curDate, curTime)
        if ( secs >= 0 ) then
           timeIndex1 = secs/incSecs + 1
        else
           timeIndex1 = secs/incSecs
        end if
        timeIndex2 = timeIndex1 + 1
        if (bSide == "L") timeIndex = timeIndex1
        if (bside == "R") timeIndex = timeIndex2
        secs = (timeIndex-1)*incSecs
        call GetDate ( begDate, begTime, secs, nymdB, nhmsB, status )
        call MAPL_UnpackTime(nymdB,iyr,imm,idd)
        call MAPL_UnpackTime(nhmsB,ihr,imn,isc)
        call ESMF_TimeSet(bracketTime, yy=iyr, mm=imm, dd=idd,  h=ihr,  m=imn, s=isc, __RC__)
        call ESMF_CFIODestroy(CFIO,__RC__)
        rc=ESMF_SUCCESS
        return
     else
        allocate(tSeries(cfio%tSteps))
        do i=1,cfio%tSteps
           iCurrInterval = (i-1)*incSecs
           call GetDate ( begDate, begTime, iCurrInterval, nymdB, nhmsB, status )
           call MAPL_UnpackTime(nymdB,iyr,imm,idd)
           call MAPL_UnpackTime(nhmsB,ihr,imn,isc)
           call ESMF_TimeSet(tSeries(i), yy=iyr, mm=imm, dd=idd,  h=ihr,  m=imn, s=isc,__RC__)
        enddo
        found = .false.
        if (bSide == "L") then
           do i=cfio%tSteps,1,-1
              if (cTime >= tSeries(i)) then
                 bracketTime = tSeries(i)
                 found = .true.
                 exit
              end if
           end do
        else if (bSide == "R") then
           do i=1,cfio%tSteps
              if (cTime < tSeries(i)) then
                 bracketTime = tSeries(i)
                 found = .true.
                 exit
              end if
           end do
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
     end if 

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

 END MODULE MAPL_ExtDataGridCompMod
