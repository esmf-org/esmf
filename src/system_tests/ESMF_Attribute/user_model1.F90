! $Id: user_model1.F90,v 1.28 2008/07/28 03:59:23 rokuingh Exp $
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
    character(ESMF_MAXSTR)      :: name1,name2,name3,name4,name5,value1,value2,value3,conv,purp
    logical                     :: value4, value5
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
    name1 = 'shortname'
    name2 = 'longname'
    name3 = 'units'
    name4 = 'import'
    name5 = 'export'
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'DPEDT'
    value2 = 'Edge pressure tendency'
    value3 = 'Pa s-1'
    value4 = .false.
    value5 = .false.
      
    DPEDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DPEDT, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DPEDT, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DPEDT, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DPEDT, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DPEDT, name4, value4, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DPEDT, name5, value5, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'DTDT'
    value2 = 'Delta-p weighted temperature tendency'
    value3 = 'Pa K s-1'
    value4 = .true.
    value5 = .false.

    DTDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DTDT, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DTDT, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DTDT, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DTDT, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DTDT, name4, value4, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DTDT, name5, value5, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'DUDT'
    value2 = 'Eastward wind tendency'
    value3 = 'm s-2'
    value4 = .true.
    value5 = .false.
      
    DUDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DUDT, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DUDT, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DUDT, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DUDT, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DUDT, name4, value4, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DUDT, name5, value5, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'DVDT'
    value2 = 'Northward wind tendency'
    value3 = 'm s-2'
    value4 = .true.
    value5 = .false.
      
    DVDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(DVDT, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DVDT, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DVDT, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DVDT, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DVDT, name4, value4, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(DVDT, name5, value5, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'PHIS'
    value2 = 'Surface geopotential height'
    value3 = 'm+2 sec-2'
    value4 = .true.
    value5 = .false.
      
    PHIS = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(PHIS, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(PHIS, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(PHIS, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(PHIS, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(PHIS, name4, value4, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(PHIS, name5, value5, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'QTR'
    value2 = 'Advected quantities'
    value3 = 'unknown'
    value4 = .true.
    value5 = .false.
      
    QTR = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(QTR, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(QTR, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(QTR, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(QTR, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(QTR, name4, value4, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(QTR, name5, value5, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CNV'
    value2 = 'Generation of atmosphere kinetic energy content'
    value3 = 'W m-2'
    value4 = .false.
    value5 = .true.
      
    CNV = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CNV, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CNV, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CNV, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CNV, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CNV, name4, value4, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CNV, name5, value5, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CONVCPT'
    value2 = 'Vertically integrated enthalpy convergence'
    value3 = 'W m-2'
    value4 = .false.
    value5 = .true.
      
    CONVCPT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVCPT, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVCPT, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVCPT, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVCPT, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVCPT, name4, value4, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVCPT, name5, value5, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CONVKE'
    value2 = 'Vertically integrated kinetic energy convergence'
    value3 = 'W m-2'
    value4 = .false.
    value5 = .true.
      
    CONVKE = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVKE, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVKE, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVKE, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVKE, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVKE, name4, value4, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVKE, name5, value5, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'CONVPHI'
    value2 = 'Vertically integrated geopotential convergence'
    value3 = 'W m-2'
    value4 = .false.
    value5 = .true.
      
    CONVPHI = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
    call ESMF_AttributeAdd(CONVPHI, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVPHI, name1, value1, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVPHI, name2, value2, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVPHI, name3, value3, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVPHI, name4, value4, convention=conv, purpose=purp, rc=status)
    call ESMF_AttributeSet(CONVPHI, name5, value5, convention=conv, purpose=purp, rc=status)
    if (status .ne. ESMF_SUCCESS) return
      
    ! Create a FieldBundle for the first five Fields from above
    fbundle = ESMF_FieldBundleCreate(name="fbundle", rc=status)
    call ESMF_FieldBundleSetGrid(fbundle, grid=grid, rc=status)
    if (status .ne. ESMF_SUCCESS) return
      
    ! Connect the Attributes from the Fields above to the FieldBundle
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
    call ESMF_AttributeSet(fbundle, DPEDT, rc=status)
    call ESMF_AttributeSet(fbundle, DTDT, rc=status)
    call ESMF_AttributeSet(fbundle, DUDT, rc=status)
    call ESMF_AttributeSet(fbundle, DVDT, rc=status)
    call ESMF_AttributeSet(fbundle, PHIS, rc=status)
    call ESMF_AttributeSet(fbundle, QTR, rc=status)
    call ESMF_AttributeSet(fbundle, CNV, rc=status)
    call ESMF_AttributeSet(fbundle, CONVCPT, rc=status)
    call ESMF_AttributeSet(fbundle, CONVKE, rc=status)
    call ESMF_AttributeSet(fbundle, CONVPHI, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Connect the Attributes from the FieldBundle to the export State
    call ESMF_StateAdd(exportState, fieldbundle=fbundle, rc=status)
    call ESMF_AttributeSet(exportState, fbundle, rc=status)
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
