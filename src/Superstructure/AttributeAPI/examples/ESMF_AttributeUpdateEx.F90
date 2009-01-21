! $Id: ESMF_AttributeUpdateEx.F90,v 1.2 2009/01/21 21:38:02 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================


!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

module user_model

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userm1_register
  public userm2_register
  public usercpl_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: supportPthreads
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userm1_init, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userm1_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userm1_final, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.
    
    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, supportPthreadsFlag=supportPthreads, rc=rc)
    if (supportPthreads) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif
    
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine userm1_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet
    character(ESMF_MAXSTR)      :: name1,name2,name3,name4,value1,value2,value3,value4,convESG,purpGen
    type(ESMF_ArraySpec)        :: arrayspec
    type(ESMF_Grid)             :: grid
    type(ESMF_Field)            :: DPEDT,DTDT,DUDT,DVDT,PHIS,QTR,CNV,CONVCPT,CONVKE,CONVPHI
    type(ESMF_FieldBundle)      :: fbundle
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Create the destination FieldBundle and add it to the import State
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/1,petCount/), &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), & ! no stagger padding
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! Initialize variables
    convESG = 'ESG'
    purpGen = 'general'
    name1 = 'name'
    name2 = 'standard_name'
    name3 = 'long_name'
    name4 = 'units'
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'DPEDT'
    value2 = 'tendency_of_air_pressure'
    value3 = 'Edge pressure tendency'
    value4 = 'Pa s-1'
      
    DPEDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DPEDT, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DPEDT, name1, value1, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DPEDT, name2, value2, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DPEDT, name3, value3, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DPEDT, name4, value4, convention=convESG, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'DTDT'
    value2 = 'tendency_of_air_temperature'
    value3 = 'Delta-p weighted temperature tendency'
    value4 = 'Pa K s-1'

    DTDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DTDT, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DTDT, name1, value1, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DTDT, name2, value2, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DTDT, name3, value3, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DTDT, name4, value4, convention=convESG, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'DUDT'
    value2 = 'tendency_of_eastward_wind'
    value3 = 'Eastward wind tendency'
    value4 = 'm s-2'
      
    DUDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DUDT, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DUDT, name1, value1, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DUDT, name2, value2, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DUDT, name3, value3, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DUDT, name4, value4, convention=convESG, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'DVDT'
    value2 = 'tendency_of_northward_wind'
    value3 = 'Northward wind tendency'
    value4 = 'm s-2'
      
    DVDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DVDT, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DVDT, name1, value1, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DVDT, name2, value2, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DVDT, name3, value3, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(DVDT, name4, value4, convention=convESG, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'PHIS'
    value2 = 'surface_geopotential'
    value3 = 'Surface geopotential height'
    value4 = 'm2 s-2'
      
    PHIS = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(PHIS, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(PHIS, name1, value1, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(PHIS, name2, value2, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(PHIS, name3, value3, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(PHIS, name4, value4, convention=convESG, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'QTR'
    value2 = ''
    value3 = 'Advected quantities'
    value4 = 'unknown'
      
    QTR = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(QTR, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(QTR, name1, value1, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(QTR, name2, value2, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(QTR, name3, value3, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(QTR, name4, value4, convention=convESG, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CNV'
    value2 = 'atmosphere_kinetic_energy_content'
    value3 = 'Generation of atmosphere kinetic energy content'
    value4 = 'W m-2'
      
    CNV = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CNV, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CNV, name1, value1, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CNV, name2, value2, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CNV, name3, value3, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CNV, name4, value4, convention=convESG, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CONVCPT'
    value2 = ''
    value3 = 'Vertically integrated enthalpy convergence'
    value4 = 'W m-2'
      
    CONVCPT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVCPT, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVCPT, name1, value1, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVCPT, name2, value2, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVCPT, name3, value3, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVCPT, name4, value4, convention=convESG, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CONVKE'
    value2 = ''
    value3 = 'Vertically integrated kinetic energy convergence'
    value4 = 'W m-2'
      
    CONVKE = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVKE, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVKE, name1, value1, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVKE, name2, value2, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVKE, name3, value3, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVKE, name4, value4, convention=convESG, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CONVPHI'
    value2 = ''
    value3 = 'Vertically integrated geopotential convergence'
    value4 = 'W m-2'
      
    CONVPHI = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVPHI, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVPHI, name1, value1, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVPHI, name2, value2, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVPHI, name3, value3, convention=convESG, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(CONVPHI, name4, value4, convention=convESG, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return
      
    ! Create a FieldBundle for Fields
    fbundle = ESMF_FieldBundleCreate(name="fbundle", rc=status)
    call ESMF_FieldBundleSetGrid(fbundle, grid=grid, rc=status)
    if (status .ne. ESMF_SUCCESS) return
      
    ! Add the Fields to the FieldBundle (this will connect the Attribute hierarchies)
    call ESMF_FieldBundleAdd(fbundle, DPEDT, rc=status)
    call ESMF_FieldBundleAdd(fbundle, DTDT, rc=status)
    call ESMF_FieldBundleAdd(fbundle, DUDT, rc=status)
    call ESMF_FieldBundleAdd(fbundle, DVDT, rc=status)
    call ESMF_FieldBundleAdd(fbundle, PHIS, rc=status)
    call ESMF_FieldBundleAdd(fbundle, QTR, rc=status)
    call ESMF_FieldBundleAdd(fbundle, CNV, rc=status)
    call ESMF_FieldBundleAdd(fbundle, CONVCPT, rc=status)
    call ESMF_FieldBundleAdd(fbundle, CONVKE, rc=status)
    call ESMF_FieldBundleAdd(fbundle, CONVPHI, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Connect the Attributes from the FieldBundle to the export State
    call ESMF_StateAdd(exportState, fieldbundle=fbundle, rc=status)
    if (status .ne. ESMF_SUCCESS) return

  end subroutine userm1_init

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine userm1_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet, k
    character(ESMF_MAXSTR)      :: name2,value2,convESG,convCC,purpGen,name3
    character(ESMF_MAXSTR),dimension(2) :: attrList
    type(ESMF_Field)            :: field
    type(ESMF_FieldBundle)      :: fbundle
    type(ESMF_Grid)             :: grid

    ! Initialize return code
    rc = ESMF_SUCCESS

    convESG = 'ESG'
    convCC = 'CustomConvention'
    purpGen = 'general'
    name2 = 'standard_name'
    value2 = 'default_standard_name'
    name3 = 'long_name'
    
    attrList(1) = 'coordinates'
    attrList(2) = 'mask'
    
    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_StateGet(exportState, "fbundle", fbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleGet(fbundle, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    do k = 1, 10
        call ESMF_FieldBundleGet(fbundle, fieldIndex=k, field=field, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
        call ESMF_AttributeSet(field, name2, value2, convention=convESG, purpose=purpGen, rc=status)
        if (rc/=ESMF_SUCCESS) return ! bail out
        call ESMF_AttributeAdd(field, attrList=attrList, convention=convCC, purpose=purpGen, &
          count=2, attpacknestflag=ESMF_ATTPACKNEST_ON, rc=rc)
        call ESMF_AttributeSet(field, name='coordinates', value='latlon', &
          convention=convCC, purpose=purpGen, rc=rc)
        call ESMF_AttributeSet(field, name='mask', value='yes', &
          convention=convCC, purpose=purpGen, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
        call ESMF_AttributeRemove(field, name=name3, convention=convESG, purpose=purpGen, rc=status)
        if (rc/=ESMF_SUCCESS) return ! bail out
    enddo

    !print *, 'myPet = ', myPet

    ! Nothing happens in this run cycle for this simple example
                                                             
  end subroutine userm1_run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine userm1_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_Field)            :: field
    type(ESMF_FieldBundle)      :: fbundle
    type(ESMF_Grid)             :: grid
    integer                     :: k

    ! Initialize return code
    rc = ESMF_SUCCESS
    
    call ESMF_StateGet(exportState, "fbundle", fbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleGet(fbundle, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    do k = 1, 10
        call ESMF_FieldBundleGet(fbundle, fieldIndex=k, field=field, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
        call ESMF_FieldDestroy(field, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
    enddo
    call ESMF_FieldBundleDestroy(fbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_GridDestroy(grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine userm1_final

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm2_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: supportPthreads
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userm2_init, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userm2_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userm2_final, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.

    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, supportPthreadsFlag=supportPthreads, rc=rc)
    if (supportPthreads) then
      call ESMF_GridCompSetVMMinThreads(comp, rc=rc)
    endif
#endif
    
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine userm2_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

   ! Initialize return code
    rc = ESMF_SUCCESS

  end subroutine userm2_init

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine userm2_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc


    ! Local variables
    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet
    character(ESMF_MAXSTR)      :: convESG,purpGen
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Initialize variables
    convESG = 'ESG'
    purpGen = 'general'

    ! Write the Attribute info to esmf/test/testg/<platform>/ESMF_AttributeSTest.stdout
    if (myPet .eq. 0) then
      !call ESMF_AttributeWrite(importState,convESG,purpGen,rc=rc)
      if (rc .ne. ESMF_SUCCESS) return
    endif
                                                             
  end subroutine userm2_run

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
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine usercpl_register(comp, rc)
    type(ESMF_CplComp) :: comp
    integer, intent(out) :: rc

#ifdef ESMF_TESTWITHTHREADS
    type(ESMF_VM) :: vm
    logical :: supportPthreads
#endif

    ! Initialize return code
    rc = ESMF_SUCCESS
    
    ! Register the callback routines.
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETINIT, usercpl_init, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETRUN, usercpl_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_CplCompSetEntryPoint(comp, ESMF_SETFINAL, usercpl_final, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

#ifdef ESMF_TESTWITHTHREADS
    ! The following call will turn on ESMF-threading (single threaded)
    ! for this component. If you are using this file as a template for 
    ! your own code development you probably don't want to include the 
    ! following call unless you are interested in exploring ESMF's 
    ! threading features.
    
    ! First test whether ESMF-threading is supported on this machine
    call ESMF_VMGetGlobal(vm, rc=rc)
    call ESMF_VMGet(vm, supportPthreadsFlag=supportPthreads, rc=rc)
    if (supportPthreads) then
      call ESMF_CplCompSetVMMinThreads(comp, rc=rc)
    endif
#endif

  end subroutine

!-------------------------------------------------------------------------
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.
    
  subroutine usercpl_init(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)         :: vm

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Need to reconcile import and export states
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(importState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateReconcile(exportState, vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
   
  end subroutine usercpl_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !
 
  subroutine usercpl_run(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)               :: vm
    integer                     :: myPet
    
    ! for testing update
    type(ESMF_Array)            :: array
    integer, dimension(2)       :: rootList
    character(ESMF_MAXSTR)      :: convESG,purpGen

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_VMGet(vm, localPet=myPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    ! Get the import and export States
    call ESMF_StateGet(importState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    rootList = (/0,1/)
    call ESMF_AttributeUpdate(importState, vm, rootList=rootList, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    
    ! copy all Attribute information into export State
    call ESMF_AttributeCopy(importState, exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
  
  end subroutine usercpl_run


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


end module user_model




!BOE
! \subsubsection{Example: Advanced Attribute usage: Attributes in a Distributed Environment}
!
! This advanced example illustrates the proper methods of Attribute manipulation
! in a distributed environment to ensure consistency of meta-data across the VM. 
! The first thing we must do is declare variables and initialize the ESMF.
!EOE


!  !PROGRAM: ESMF\_AttributeUpdateEx - Example of Attribute usage in a distributed environment.
!
!  !DESCRIPTION: 
!
! This program shows examples of Attribute usage


program ESMF_AttributeUpdateEx

      ! Use ESMF framework module
      use ESMF_Mod
      
      use user_model, only : userm1_register, &
                             userm2_register, &
                             usercpl_register
      
      implicit none

!BOC
      ! Local variables  
      integer                 :: rc, finalrc, petCount, localPet
      type(ESMF_VM)           :: vm
      type(ESMF_State)        :: c1exp, c2imp
      type(ESMF_GridComp)     :: gridcomp1
      type(ESMF_GridComp)     :: gridcomp2
      type(ESMF_CplComp)      :: cplcomp
      character(ESMF_MAXSTR)  :: name1,name2,name3,name4,name5,name6,name7, &
                                 name8, name9, name10, value1,value2,value3, &
                                 value4,value5,value6,value7,value8,value9, &
                                 value10,convESG,purpGen
!EOC         

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
      if (rc/=ESMF_SUCCESS) goto 10
      
      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "Start of ESMF_AttributeUpdate Example"
        print *, "--------------------------------------- "
      endif

!BOE
! First construct some ESMF objects, such as the gridded Component, States, 
! FieldBundle, and Fields.
!EOE

!BOC
      ! Create section
      if (petCount<4) then
        gridcomp1 = ESMF_GridCompCreate(name="gridcomp1", &
          petList=(/0/), rc=rc)
        gridcomp2 = ESMF_GridCompCreate(name="gridcomp2", &
          petList=(/0/), rc=rc)
        cplcomp = ESMF_CplCompCreate(name="cplcomp", &
          petList=(/0/), rc=rc)
      else
        gridcomp1 = ESMF_GridCompCreate(name="gridcomp1", &
          petList=(/0,1/), rc=rc)
        gridcomp2 = ESMF_GridCompCreate(name="gridcomp2", &
          petList=(/2,3/), rc=rc)
        cplcomp = ESMF_CplCompCreate(name="cplcomp", &
          petList=(/0,1,2,3/), rc=rc)
      endif
!EOC      
      ! Register section
      call ESMF_GridCompSetServices(gridcomp1, userm1_register, rc)
      call ESMF_GridCompSetServices(gridcomp2, userm2_register, rc)
      call ESMF_CplCompSetServices(cplcomp, usercpl_register, rc)
      
      ! Initialize section
      c1exp = ESMF_StateCreate("Comp1 exportState", &
        ESMF_STATE_EXPORT, rc=rc)
      call ESMF_GridCompInitialize(gridcomp1, exportState=c1exp, rc=rc)
      c2imp = ESMF_StateCreate("Comp2 importState", &
        ESMF_STATE_IMPORT, rc=rc)
      call ESMF_GridCompInitialize(gridcomp2, importState=c2imp, rc=rc)
      call ESMF_CplCompInitialize(cplcomp, importState=c1exp, &
        exportState=c2imp, rc=rc)  
      
!BOC
      ! initialize variables
      convESG = 'ESG'
      purpGen = 'general'
      name1 = 'discipline'
      name2 = 'physical_domain'
      name3 = 'agency'
      name4 = 'institution'
      name5 = 'author'
      name6 = 'coding_language'
      name7 = 'model_component_framework'
      name8 = 'name'
      name9 = 'full_name'
      name10 = 'version'
      value1 = 'Atmosphere'
      value2 = 'Earth system'
      value3 = 'NASA'
      value4 = 'Global Modeling and Assimilation Office (GMAO)'
      value5 = 'Max Suarez'
      value6 = 'Fortran 90'
      value7 = 'ESMF (Earth System Modeling Framework)'
      value8 = 'GEOS-5 FV dynamical core'
      value9 = 'Goddard Earth Observing System Version 5 Finite Volume Dynamical Core'
      value10 = 'GEOSagcm-EROS-beta7p12'
  
      ! Add the Attribute package to gridcomp1
      call ESMF_AttributeAdd(gridcomp1, convention=convESG, &
        purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(gridcomp1, name1, value1, &
        convention=convESG, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(gridcomp1, name2, value2, &
        convention=convESG, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(gridcomp1, name3, value3, &
        convention=convESG, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(gridcomp1, name4, value4, &
        convention=convESG, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(gridcomp1, name5, value5, &
        convention=convESG, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(gridcomp1, name6, value6, &
        convention=convESG, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(gridcomp1, name7, value7, &
        convention=convESG, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(gridcomp1, name8, value8, &
        convention=convESG, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(gridcomp1, name9, value9, &
        convention=convESG, purpose=purpGen, rc=rc)
      call ESMF_AttributeSet(gridcomp1, name10, value10, &
        convention=convESG, purpose=purpGen, rc=rc)
      
      ! link the Component Attribute hierarchy to State
      call ESMF_AttributeSet(gridcomp1, c1exp, rc=rc)
!EOC
      
      ! Run section
      call ESMF_GridCompRun(gridcomp1, exportState=c1exp, rc=rc)
      call ESMF_CplCompRun(cplcomp, importState=c1exp, &
        exportState=c2imp, rc=rc)
      call ESMF_GridCompRun(gridcomp2, importState=c2imp, rc=rc)
      
      ! Finalize section
      call ESMF_GridCompDestroy(gridcomp1, rc=rc)
      call ESMF_GridCompDestroy(gridcomp2, rc=rc)
      call ESMF_CplCompDestroy(cplcomp, rc=rc)
      
      call ESMF_StateDestroy(c1exp, rc=rc)
      call ESMF_StateDestroy(c2imp, rc=rc)

  if (localPet==0) then
      print *, "--------------------------------------- "
      print *, "End of ESMF_AttributeUpdate Example"
      print *, "--------------------------------------- "
  endif

10 continue
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_AttributeUpdateEx.F90"
  else
    print *, "FAIL: ESMF_AttributeUpdateEx.F90"
  endif
  
end program

