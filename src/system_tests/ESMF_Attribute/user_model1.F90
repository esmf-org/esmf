! $Id: user_model1.F90,v 1.47 2009/07/02 17:18:59 eatfastnoodle Exp $
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
  
  private
    
  public userm1_setvm, userm1_register
        
  contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.
 
  subroutine userm1_setvm(comp, rc)
    type(ESMF_GridComp) :: comp
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

  end subroutine

  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=user_init, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=user_run, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=user_final, &
      rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet
    character(ESMF_MAXSTR)      :: name1,name2,name3,name4,name5, &
                                   name6,name7,name8,name9,name10, &
                                   value1,value2,value3,value4,value5, &
                                   value6,value7,value8,value9,value10, &
                                   conv,purp, convCC, purpGen, waterml, watermlcon
    type(ESMF_ArraySpec)        :: arrayspec
    type(ESMF_Grid)             :: grid
    type(ESMF_Field)            :: DPEDT
    type(ESMF_FieldBundle)      :: fieldbundle
    character(ESMF_MAXSTR),dimension(2)   :: attrList
    character(ESMF_MAXSTR),dimension(8)   :: watermlattrList
         
    
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
    conv = 'ESMF'
    purp = 'General'
    waterml='WaterML'
    watermlcon='General'
    watermlattrList(1)='variableCode'
    watermlattrList(2)='variableName'
	watermlattrList(3)='variableType'
    watermlattrList(4)='GeneralCategory'
    watermlattrList(5)='sampleMedium'
	watermlattrList(6)='units'
	watermlattrList(7)='NoDataValue'
	watermlattrList(8)='timeSupport'
    name1 = 'Name'

 
    ! Create a Field, add an Attribute package, and set Attributes in the package
    value1 = 'variable'
      
    DPEDT = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)

    call ESMF_AttributeAdd(DPEDT, convention=waterml, purpose=watermlcon, attrList=watermlattrList, rc=status)
    call ESMF_AttributeSet(DPEDT, name='variableCode', value='USU39', convention=waterml, purpose=watermlcon, rc=status) 
    call ESMF_AttributeSet(DPEDT, name='variableName', value='Phosphorus, total as P', convention=waterml, purpose=watermlcon, rc=status)
	call ESMF_AttributeSet(DPEDT, name='variableType', value='Sample', convention=waterml, purpose=watermlcon, rc=status) 
	call ESMF_AttributeSet(DPEDT, name='GeneralCategory', value='Water Quality', convention=waterml, purpose=watermlcon, rc=status) 
	call ESMF_AttributeSet(DPEDT, name='sampleMedium', value='Surface Water', convention=waterml, purpose=watermlcon, rc=status) 
    call ESMF_AttributeSet(DPEDT, name='units', value='milligrams per liter', convention=waterml, purpose=watermlcon, rc=status) 
	call ESMF_AttributeSet(DPEDT, name='NoDataValue', value='-9999', convention=waterml, purpose=watermlcon, rc=status) 
	call ESMF_AttributeSet(DPEDT, name='timeSupport', value='false', convention=waterml, purpose=watermlcon, rc=status) 
    if (status .ne. ESMF_SUCCESS) return

       
    ! Create the Grid Attribute Package
    call ESMF_AttributeAdd(grid,convention=conv, purpose=purp, rc=status)

    if (status .ne. ESMF_SUCCESS) return

    ! Create a FieldBundle for Fields
    fieldbundle = ESMF_FieldBundleCreate(name="fieldbundle", rc=status)
    call ESMF_FieldBundleSetGrid(fieldbundle, grid=grid, rc=status)
    if (status .ne. ESMF_SUCCESS) return
      
    ! Add the Fields to the FieldBundle (this will connect the Attribute hierarchies)
    call ESMF_FieldBundleAdd(fieldbundle, DPEDT, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Connect the Attributes from the FieldBundle to the export State
    call ESMF_StateAdd(exportState, fieldbundle=fieldbundle, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    
    ! Add the Attribute package to comp
    call ESMF_AttributeAdd(comp, convention=waterml, purpose=watermlcon, rc=rc)
    if (status .ne. ESMF_SUCCESS) return
    
  end subroutine user_init

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
     type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    character(ESMF_MAXSTR)      :: waterml, watermlcon
    type(ESMF_Field)            :: field


    ! Initialize return code
    rc = ESMF_SUCCESS

    waterml='WaterML'
    watermlcon='General'

        call ESMF_AttributeWrite(field,'WaterML',watermlcon,rc=rc)
        call ESMF_AttributeWrite(field,'WaterML',watermlcon, &
                               attwriteflag=ESMF_ATTWRITE_XML, rc=rc)
        if (rc/=ESMF_SUCCESS) return ! bail out
                                                             
  end subroutine user_run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
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

  end subroutine user_final


end module user_model1
    
!\end{verbatim}
