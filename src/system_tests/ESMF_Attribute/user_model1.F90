! $Id: user_model1.F90,v 1.31 2008/08/14 01:50:20 rokuingh Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User-supplied Component, most recent interface revision.
!
!
!\begin{verbatim}

module user_model1

  ! ESMF Framework module
  use ESMF_Mod

  implicit none
    
  public userm1_register
        
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

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, user_init, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, user_run, &
      ESMF_SINGLEPHASE, rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, user_final, &
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
 
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet
    character(ESMF_MAXSTR)      :: name1,name2,name3,name4,value1,value2,value3,value4,conv,purp
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
    conv = 'CF'
    purp = 'basic'
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
    call ESMF_AttributeAdd(DPEDT, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DPEDT, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DPEDT, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DPEDT, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DPEDT, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'DTDT'
    value2 = 'tendency_of_air_temperature'
    value3 = 'Delta-p weighted temperature tendency'
    value4 = 'Pa K s-1'

    DTDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DTDT, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DTDT, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DTDT, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DTDT, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DTDT, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'DUDT'
    value2 = 'tendency_of_eastward_wind'
    value3 = 'Eastward wind tendency'
    value4 = 'm s-2'
      
    DUDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DUDT, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DUDT, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DUDT, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DUDT, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DUDT, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'DVDT'
    value2 = 'tendency_of_northward_wind'
    value3 = 'Northward wind tendency'
    value4 = 'm s-2'
      
    DVDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DVDT, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DVDT, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DVDT, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DVDT, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DVDT, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'PHIS'
    value2 = 'surface_geopotential'
    value3 = 'Surface geopotential height'
    value4 = 'm2 s-2'
      
    PHIS = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(PHIS, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(PHIS, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(PHIS, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(PHIS, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(PHIS, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'QTR'
    value2 = ''
    value3 = 'Advected quantities'
    value4 = 'unknown'
      
    QTR = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(QTR, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(QTR, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(QTR, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(QTR, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(QTR, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CNV'
    value2 = 'atmosphere_kinetic_energy_content'
    value3 = 'Generation of atmosphere kinetic energy content'
    value4 = 'W m-2'
      
    CNV = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CNV, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CNV, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CNV, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CNV, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CNV, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CONVCPT'
    value2 = ''
    value3 = 'Vertically integrated enthalpy convergence'
    value4 = 'W m-2'
      
    CONVCPT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVCPT, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVCPT, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVCPT, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVCPT, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVCPT, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CONVKE'
    value2 = ''
    value3 = 'Vertically integrated kinetic energy convergence'
    value4 = 'W m-2'
      
    CONVKE = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVKE, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVKE, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVKE, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVKE, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVKE, name4, value4, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CONVPHI'
    value2 = ''
    value3 = 'Vertically integrated geopotential convergence'
    value4 = 'W m-2'
      
    CONVPHI = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVPHI, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVPHI, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVPHI, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVPHI, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVPHI, name4, value4, convention=conv, purpose=purp, rc=status)
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

  end subroutine user_init

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Determine petCount
    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    !print *, 'myPet = ', myPet

    ! Nothing happens in this run cycle for this simple example
                                                             
  end subroutine user_run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp), intent(inout) :: comp
    type(ESMF_State), intent(inout) :: importState, exportState
    type(ESMF_Clock), intent(in) :: clock
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

  end subroutine user_final


end module user_model1
    
!\end{verbatim}
