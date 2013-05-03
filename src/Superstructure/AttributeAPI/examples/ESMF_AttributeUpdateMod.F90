! $Id$
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------


module ESMF_AttributeUpdateMod

  ! ESMF Framework module
  use ESMF

  implicit none
  
  private
  
  public userm1_setvm, userm1_register
  public userm2_setvm, userm2_register
  public usercpl_setvm, usercpl_register

  contains

!-------------------------------------------------------------------------
!   !  The SetVM Register routines for Gridcomp1
 
  subroutine userm1_setvm(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.
    
    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif
    
  end subroutine userm1_setvm

  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userm1_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userm1_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userm1_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine userm1_register
  
!-------------------------------------------------------------------------
!   !  The SetVM Register routines for Gridcomp2
 
  subroutine userm2_setvm(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif
    
  end subroutine userm2_setvm

  subroutine userm2_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userm2_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userm2_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userm2_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
  end subroutine userm2_register

!-------------------------------------------------------------------------
!   !  The SetVM Register routines for cplcomp

  subroutine usercpl_setvm(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: pthreadsEnabled
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS
    
#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.
    
    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, pthreadsEnabledFlag=pthreadsEnabled, rc=rc)
    if (pthreadsEnabled) then
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine usercpl_setvm

  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS
    
    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, usercpl_init, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_RUN, usercpl_run, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, usercpl_final, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine usercpl_register
  
!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine userm1_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

!BOE
! In the first gridded Component initialize routine we need to create some
! Attribute packages and set all of the Attributes.  These Attributes will
! be attached to realistic Fields, containing a Grid, which are contained in a 
! FieldBundle.  The first thing to do is declare variables and make the Grid.
!EOE

!BOC
    type(ESMF_VM)            :: vm
    integer                  :: petCount, status, myPet
    character(ESMF_MAXSTR)   :: name1,name2,name3,name4,value1,value2, &
                                value3,value4,convESMF,purpGen,convCC
    type(ESMF_ArraySpec)     :: arrayspec
    type(ESMF_Grid)          :: grid
    type(ESMF_Field)         :: DPEDT,DTDT,DUDT,DVDT,PHIS,QTR,CNV,CONVCPT, &
                                CONVKE,CONVPHI
    type(ESMF_FieldBundle)   :: fieldbundle
    character(ESMF_MAXSTR),dimension(2)   :: attrList         
    
    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)

    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, &
           rc=rc)
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/1,petCount/), &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
!EOC

! This first bit is a verification that the {\tt ESMF\_StateReconcile()} call will correctly
! reconcile Attributes and Attribute packages that are attached to the top level 
! State in an Attribute hierarchy.  During the initialize phase of the
! coupler Component, the structure of these Attributes should be reconciled across the
! VM.  The value of the Attributes in this structure are not guaranteed after the 
! completion of {\tt ESMF\_StateReconcile()}, as that is the responsibility of the
! {\tt ESMF\_AttributeUpdate()} call.  There will be more on this subject when we get
! to the coupler Component.
    call ESMF_AttributeSet(exportState, name="TESTESTEST", &
                           value="SUCCESUCCESUCCES", rc=status)
    if (status .ne. ESMF_SUCCESS) return

!BOE
! At this point the Fields will need to have Attribute packages attached to them, and the
! Attributes will be set with appropriate values.
!EOE
  
!BOC
    convCC = 'CustomConvention'
    convESMF = 'ESMF'
    purpGen = 'General'
    name1 = 'ShortName'
    name2 = 'StandardName'
    name3 = 'LongName'
    name4 = 'Units'
 
    value1 = 'DPEDT'
    value2 = 'tendency_of_air_pressure'
    value3 = 'Edge pressure tendency'
    value4 = 'Pa s-1'
      
    DPEDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DPEDT, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(DPEDT, name1, value1, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DPEDT, name2, value2, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DPEDT, name3, value3, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DPEDT, name4, value4, convention=convESMF, &
      purpose=purpGen, rc=status)

!EOC

    value1 = 'DTDT'
    value2 = 'tendency_of_air_temperature'
    value3 = 'Delta-p weighted temperature tendency'
    value4 = 'Pa K s-1'

    DTDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DTDT, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(DTDT, name1, value1, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DTDT, name2, value2, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DTDT, name3, value3, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DTDT, name4, value4, convention=convESMF, &
      purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    value1 = 'DUDT'
    value2 = 'tendency_of_eastward_wind'
    value3 = 'Eastward wind tendency'
    value4 = 'm s-2'
      
    DUDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DUDT, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(DUDT, name1, value1, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DUDT, name2, value2, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DUDT, name3, value3, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DUDT, name4, value4, convention=convESMF, &
      purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    
    value1 = 'DVDT'
    value2 = 'tendency_of_northward_wind'
    value3 = 'Northward wind tendency'
    value4 = 'm s-2'
      
    DVDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DVDT, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(DVDT, name1, value1, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DVDT, name2, value2, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DVDT, name3, value3, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DVDT, name4, value4, convention=convESMF, &
      purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    value1 = 'PHIS'
    value2 = 'surface_geopotential'
    value3 = 'Surface geopotential height'
    value4 = 'm2 s-2'
      
    PHIS = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(PHIS, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(PHIS, name1, value1, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(PHIS, name2, value2, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(PHIS, name3, value3, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(PHIS, name4, value4, convention=convESMF, &
      purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    value1 = 'QTR'
    value2 = ''
    value3 = 'Advected quantities'
    value4 = 'unknown'
      
    QTR = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(QTR, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(QTR, name1, value1, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(QTR, name2, value2, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(QTR, name3, value3, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(QTR, name4, value4, convention=convESMF, &
      purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    value1 = 'CNV'
    value2 = 'atmosphere_kinetic_energy_content'
    value3 = 'Generation of atmosphere kinetic energy content'
    value4 = 'W m-2'
      
    CNV = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CNV, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(CNV, name1, value1, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CNV, name2, value2, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CNV, name3, value3, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CNV, name4, value4, convention=convESMF, &
      purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    value1 = 'CONVCPT'
    value2 = ''
    value3 = 'Vertically integrated enthalpy convergence'
    value4 = 'W m-2'
      
    CONVCPT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVCPT, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(CONVCPT, name1, value1, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVCPT, name2, value2, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVCPT, name3, value3, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVCPT, name4, value4, convention=convESMF, &
      purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    value1 = 'CONVKE'
    value2 = ''
    value3 = 'Vertically integrated kinetic energy convergence'
    value4 = 'W m-2'
      
    CONVKE = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVKE, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(CONVKE, name1, value1, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVKE, name2, value2, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVKE, name3, value3, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVKE, name4, value4, convention=convESMF, &
      purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    value1 = 'CONVPHI'
    value2 = ''
    value3 = 'Vertically integrated geopotential convergence'
    value4 = 'W m-2'
      
    CONVPHI = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVPHI, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(CONVPHI, name1, value1, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVPHI, name2, value2, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVPHI, name3, value3, convention=convESMF, &
      purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVPHI, name4, value4, convention=convESMF, &
      purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Create the Grid Attribute Package
    call ESMF_AttributeAdd(grid,convention=convESMF, purpose=purpGen, &
                        rc=status)
    call ESMF_AttributeSet(grid,'GridType','Cubed sphere', &
                        convention=convESMF, purpose=purpGen, rc=status)    
    call ESMF_AttributeSet(grid,'CongruentTiles',.true., &
                        convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'NumberOfGridTiles','1', &
                        convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'DimensionOrder','YX', &
                        convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'DiscretizationType', &
                        'Logically Rectangular', convention=convESMF, &
                         purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'GeometryType','Sphere', &
                         convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'IsConformal',.false., &
                         convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'IsRegular',.false., &
                         convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'IsUniform',.false.,convention=convESMF, &
			 purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'NorthPoleLocation','long: 0.0 lat: 90.0', &
			 convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'NumberOfCells','53457',convention=convESMF, &
			  purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'NX','96',convention=convESMF, &
                          purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'NY','96',convention=convESMF, &
                          purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'HorizontalResolution','C48', &
                          convention=convESMF, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return

!BOE
!     ... and so on for the other 9 Fields.
!
! Now the Fields will be added to the FieldBundle, at which point the Attribute
! hierarchies of the Fields will also be attached to the Attribute hierarchy of
! the FieldBundle.  After that, the FieldBundle will be attached to the export
! State, again at which time the Attribute hierarchy of the FieldBundle will be
! attached to the Attribute hierarchy of the export State.
!EOE

!BOC
    fieldbundle = ESMF_FieldBundleCreate(name="fieldbundle", rc=status)
    call ESMF_FieldBundleSet(fieldbundle, grid=grid, rc=status)
      
    call ESMF_FieldBundleAdd(fieldbundle, (/DPEDT/), rc=status)
    call ESMF_FieldBundleAdd(fieldbundle, (/DTDT/), rc=status)
    call ESMF_FieldBundleAdd(fieldbundle, (/DUDT/), rc=status)
    call ESMF_FieldBundleAdd(fieldbundle, (/DVDT/), rc=status)
    call ESMF_FieldBundleAdd(fieldbundle, (/PHIS/), rc=status)
    call ESMF_FieldBundleAdd(fieldbundle, (/QTR/), rc=status)
    call ESMF_FieldBundleAdd(fieldbundle, (/CNV/), rc=status)
    call ESMF_FieldBundleAdd(fieldbundle, (/CONVCPT/), rc=status)
    call ESMF_FieldBundleAdd(fieldbundle, (/CONVKE/), rc=status)
    call ESMF_FieldBundleAdd(fieldbundle, (/CONVPHI/), rc=status)

    call ESMF_StateAdd(exportState, fieldbundleList=(/fieldbundle/), rc=status)
!EOC

!BOE
! At this point, the driver of the model run will transfer control to the 
! initialize phase of the second gridded Component.
!EOE

  end subroutine userm1_init

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine userm2_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

!BOE
! In the second gridded Component initialize routine we don't have
! anything to do.  The data that was created in the initialize routine
! of the first gridded Component will be passed to this Component through
! the coupler Component.  The data will not be used in this Component
! until the run phase of the model.  So now the application driver transfers
! control to the initialize phase of the coupler Component.
!EOE

   ! Initialize return code
    rc = ESMF_SUCCESS

  end subroutine userm2_init

!-------------------------------------------------------------------------
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.
    
  subroutine usercpl_init(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

!BOE
! In the coupler Component initialize routine all that is required 
! is to ensure consistent data across the VM.  The data created
! in the first gridded Component on one set of the PETs in the VM is
! intended to be read and manipulated by the second gridded Component
! which runs on an exclusive set of the PETs of the VM for this 
! application.  We need to first make that data consistent across the
! entire VM with the {\tt ESMF\_StateReconcile()} call.
! This State level call handles both the data -- Fields and FieldBundles, 
! and the metadata -- Attribute and Attribute packages.  There is a flag in
! this call to allow the user to specify whether they want 
! the metadata to be reconciled or not.
!EOE

!BOC
    type(ESMF_VM)         :: vm

    rc = ESMF_SUCCESS

    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    call ESMF_StateReconcile(importState, vm=vm, &
               attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    call ESMF_StateReconcile(exportState, vm=vm, &
               attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
!EOC
   
!BOE
! At this point, the driver of the model run will transfer control to the 
! run phase of the first gridded Component.
!EOE

  end subroutine usercpl_init

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine userm1_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

!BOE
! In the run phase of the first gridded Component is typically where the
! data contained in the Fields is manipulated.  For this simple example
! we will do no actual data manipulation because all we are interested in
! at this point is the metadata.  What we will do is add a nested Attribute
! package inside the currently existing Attribute package on each Field.  We
! will also change the value of one of the Attributes in the original Attribute
! package, and remove another of the Attributes from the original Attribute 
! package on each of the Fields.  The first thing is to declare variables and
! get the Component, VM, State, and FieldBundle.
!EOE

!BOC
    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet, k
    character(ESMF_MAXSTR)      :: name2,value2,convESMF,purpGen,purp2,name3
    character(ESMF_MAXSTR),dimension(2) :: attrList
    type(ESMF_Field)            :: field
    type(ESMF_FieldBundle)      :: fieldbundle
    type(ESMF_Grid)             :: grid

    rc = ESMF_SUCCESS

    convESMF = 'ESMF'
    purpGen = 'General'
    name2 = 'StandardName'
    value2 = 'default_standard_name'
    name3 = 'LongName'
    
    purp2 = 'Extended'
    attrList(1) = 'Coordinates'
    attrList(2) = 'Mask'
    
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)

    call ESMF_StateGet(exportState, "fieldbundle", fieldbundle, rc=rc)
    call ESMF_FieldBundleGet(fieldbundle, grid=grid, rc=rc)
!EOC

!BOE
! At this point we will extract each of the Fields in the FieldBundle in turn
! and change the value of one Attribute in the original Attribute package, 
! add a nested Attribute package, and delete one other of the Attributes in the
! original Attribute package.  These three changes represent, respectively, a
! value change and two structural changes to the Attribute hierarchy during
! run time, which must be reconciled across the VM before the second gridded
! Component can be allowed to further manipulate the Attribute hierarchy.
!EOE

!BOC
    do k = 1, 10
        call ESMF_FieldBundleGet(fieldbundle, fieldIndex=k, field=field, rc=rc)
        call ESMF_AttributeSet(field, name2, value2, convention=convESMF, &
          purpose=purpGen, rc=status)
        call ESMF_AttributeAdd(field, convention=convESMF, purpose=purp2, &
          attrList=attrList, nestConvention=convESMF, nestPurpose=purpGen, &
          rc=rc)
        call ESMF_AttributeSet(field, name='Coordinates', value='Latlon', &
          convention=convESMF, purpose=purp2, rc=rc)
        call ESMF_AttributeSet(field, name='Mask', value='Yes', &
          convention=convESMF, purpose=purp2, rc=rc)
        call ESMF_AttributeRemove(field, name=name3, convention=convESMF, &
          purpose=purpGen, rc=status)
    enddo
!EOC

!BOE
! At this point, the driver of the model run will transfer control to the 
! run phase of the coupler Component.
!EOE
                                                             
  end subroutine userm1_run

!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
  subroutine usercpl_run(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

!BOE
! In the run phase of the coupler Component we must now ensure that the
! entire VM again has a consistent view of the Attribute hierarchy.  This 
! is different from the communication done in the initialize phase of the
! model run because the only structural change that has occurred is in the
! Attribute hierarchy.  Therefore an {\tt ESMF\_AttributeUpdate()} call can
! be used at this point to reconcile these changes.  It should be noted that
! the {\tt ESMF\_AttributeUpdate()} call will reconcile value changes to the
! Attribute hierarchy as well as structural changes.
!
! The first thing to do is to retrieve the Component, VM, and States.  Then
! {\tt ESMF\_AttributeUpdate()} will be called on the import State to accomplish
! a VM wide communication.  Afterwards, the Attribute hierarchy can be transfered,
! in a local sense, from the import State to the export State using an
! {\tt ESMF\_AttributeCopy()} call.  
!EOE

!BOC
    type(ESMF_VM)               :: vm
    integer                     :: myPet
    
    integer, dimension(2)       :: rootList

    rc = ESMF_SUCCESS

    call ESMF_CplCompGet(comp, vm=vm, rc=rc)

    call ESMF_VMGet(vm, localPet=myPet, rc=rc)

    call ESMF_StateGet(importState, rc=rc)
    call ESMF_StateGet(exportState, rc=rc)

    rootList = (/0,1/)
    call ESMF_AttributeUpdate(importState, vm, rootList=rootList, rc=rc)
   
    call ESMF_AttributeCopy(importState, exportState, &
      ESMF_COPY_ALIAS, ESMF_ATTTREE_ON, rc=rc)
!EOC


!BOE
! At this point the entire VM has a consistent view of the Attribute hierarchy
! that was recently modified during {\it run time} in the first gridded component
! and the driver of the model run will transfer control to the 
! run phase of the second gridded Component.
!EOE
  
  end subroutine usercpl_run

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine userm2_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

!BOE
! In the run phase of the second gridded Component is normally where
! a user model would again manipulate the data it was given.  In this
! simple example we are only dealing with the metadata, which has already
! been ensured for consistency across the VM, including the exclusive
! piece of which is being used in this Component.  Therefore we are free
! to use the metadata as we wish, considering only that any changes we 
! make to it during run time will have to first be reconciled before other
! parts of the VM can use them.  However, this is not our concern at this
! point because we will now explore the capabilities of {\tt ESMF\_AttributeWrite()}.
!
! First we will get the Component and VM.  Then we will write out the 
! Attribute hierarchy to an .xml file, 
! after which we will write out the Attribute hierarchy to a more reader
! friendly tab-delimited format.  Both of these write calls will output their
! respective data into files in the execution directory, in either a .xml
! or .stdout file.  
!EOE

!BOC
    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet
    character(ESMF_MAXSTR)      :: convESMF,purpGen
    
    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    convESMF = 'ESMF'
    purpGen = 'General'

    if (myPet .eq. 2) then
      call ESMF_AttributeWrite(importState,convESMF,purpGen, &
        attwriteflag=ESMF_ATTWRITE_XML, rc=rc)
      call ESMF_AttributeWrite(importState,convESMF,purpGen,rc=rc)
      if (rc .ne. ESMF_SUCCESS) return
    endif
!EOC

!BOE
! At this point the driver of the model run would normally transfer control
! to the finalize phase of the first gridded Component.  However, there is
! not much of interest as far as metadata is concerned in this portion
! of the model run.  So with that we will conclude this example.
!EOE
  end subroutine userm2_run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine userm1_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_Field)            :: field
    type(ESMF_FieldBundle)      :: fieldbundle
    type(ESMF_Grid)             :: grid
    integer                     :: k

    ! Initialize return code
    rc = ESMF_SUCCESS
    
    call ESMF_StateGet(exportState, "fieldbundle", fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleGet(fieldbundle, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    do k = 1, 10
        call ESMF_FieldBundleGet(fieldbundle, fieldIndex=k, field=field, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
        call ESMF_FieldDestroy(field, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
    enddo
    call ESMF_FieldBundleDestroy(fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridDestroy(grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine userm1_final

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine userm2_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc
    
    ! Initialize return code
    rc = ESMF_SUCCESS

  end subroutine userm2_final

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine usercpl_final(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS
    
  end subroutine usercpl_final
end module

!\end{verbatim}
