! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
module AttributeUpdateContainerStressUTestMod

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

    type(ESMF_AttPack)        :: attpack   
    type(ESMF_VM)               :: vm
    integer                     :: petCount, status, myPet
    character(ESMF_MAXSTR)      :: name1,name2,name3,name4,value1,value2, &
                                   value3,value4,convESMF,purpGen,convCC
    type(ESMF_ArraySpec)        :: arrayspec
    type(ESMF_Grid)             :: grid, grid2, grid3
    type(ESMF_Field)            :: field, field2, field3, field4, field5, &
                                   field6, field7, field8, field9, field10
    type(ESMF_FieldBundle)      :: fieldbundle, fieldbundle2
    character(ESMF_MAXSTR),dimension(2)   :: attrList

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(comp, vm=vm, rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/1,petCount/), &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    convCC = 'CustomConvention'
    convESMF = 'ESMF'
    purpGen = 'General'
    name1 = 'ShortName'
    name2 = 'StandardName'
    name3 = 'LongName'
    name4 = 'Units'

    value1 = 'fieldAttribute'
    value2 = 'tendency_of_air_pressure'
    value3 = 'Edge pressure tendency'
    value4 = 'Pa s-1'

    field = ESMF_FieldCreate(grid, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, name="field", rc=status)
    call ESMF_AttributeAdd(field, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(field, name1, value1, &
      convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(field, name2, value2, &
      convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(field, name3, value3, &
      convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(field, name4, value4, &
      convention=convESMF, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    ! Create the Grid Attribute Package
    call ESMF_AttributeAdd(grid,convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'RegDecompX', 96, &
      convention=convESMF, purpose=purpGen, rc=status)
    call ESMF_AttributeSet(grid,'RegDecompY', 84, &
      convention=convESMF, purpose=purpGen, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    fieldbundle = ESMF_FieldBundleCreate(name="fieldbundle", rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_FieldBundleSet(fieldbundle, grid=grid, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_FieldBundleAdd(fieldbundle, (/ field /), rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_StateAdd(exportState, fieldbundleList=(/fieldbundle/), rc=status)
    if (status .ne. ESMF_SUCCESS) return


    ! do a bunch of crazy stuff
    grid2 = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/100,150/), &
      regDecomp=(/1,petCount/), &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    field2 = ESMF_FieldCreate(grid2, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, name="field2", rc=status)
    call ESMF_AttributeAdd(field2, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(field2, name1, value1, &
      convention=convESMF, purpose=purpGen, rc=status)
    field3 = ESMF_FieldCreate(grid2, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, name="field3", rc=status)
    call ESMF_AttributeAdd(field3, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(field3, name1, value1, &
      convention=convESMF, purpose=purpGen, rc=status)
    field4 = ESMF_FieldCreate(grid2, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, name="field4", rc=status)
    call ESMF_AttributeAdd(field4, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(field4, name1, value1, &
      convention=convESMF, purpose=purpGen, rc=status)
    field5 = ESMF_FieldCreate(grid2, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, name="field5", rc=status)
    call ESMF_AttributeAdd(field5, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(field5, name1, value1, &
      convention=convESMF, purpose=purpGen, rc=status)
    field6 = ESMF_FieldCreate(grid2, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, name="field6", rc=status)
    call ESMF_AttributeAdd(field6, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(field6, name1, value1, &
      convention=convESMF, purpose=purpGen, rc=status)
    field7 = ESMF_FieldCreate(grid2, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, name="field7", rc=status)
    call ESMF_AttributeAdd(field7, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(field7, name1, value1, &
      convention=convESMF, purpose=purpGen, rc=status)
    field8 = ESMF_FieldCreate(grid2, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, name="field8", rc=status)
    call ESMF_AttributeAdd(field8, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(field8, name1, value1, &
      convention=convESMF, purpose=purpGen, rc=status)
    field9 = ESMF_FieldCreate(grid2, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, name="field9", rc=status)
    call ESMF_AttributeAdd(field9, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(field9, name1, value1, &
      convention=convESMF, purpose=purpGen, rc=status)
    field10 = ESMF_FieldCreate(grid2, arrayspec=arrayspec, &
              staggerloc=ESMF_STAGGERLOC_CENTER, name="field10", rc=status)
    call ESMF_AttributeAdd(field10, convention=convESMF, purpose=purpGen, &
      rc=status)
    call ESMF_AttributeSet(field10, name1, value1, &
      convention=convESMF, purpose=purpGen, rc=status)
    if (rc/=ESMF_SUCCESS) return

    fieldbundle2 = ESMF_FieldBundleCreate(name="fieldbundle2", rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_FieldBundleSet(fieldbundle2, grid=grid2, rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_FieldBundleAdd(fieldbundle2, (/ field2, field3, field4, field5, &
                                             field6, field7, field8, field9 /), &
                             rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_StateAdd(exportState, fieldList=(/field10/), rc=status)
    if (status .ne. ESMF_SUCCESS) return


    call ESMF_FieldBundleRemove(fieldbundle2, &
                                fieldNameList=(/"field2", "field3"/), rc=status)
    if (status .ne. ESMF_SUCCESS) return


    call ESMF_StateRemove(exportState, itemNameList=(/"fieldbundle"/), rc=status)
    if (status .ne. ESMF_SUCCESS) return
    call ESMF_StateAdd(exportState, fieldbundleList=(/fieldbundle2/), rc=status)
    if (status .ne. ESMF_SUCCESS) return


    call ESMF_FieldBundleAddReplace(fieldbundle2, fieldList=(/field2, field3/), rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_FieldBundleRemove(fieldbundle2, &
                                fieldNameList=(/"field4", "field5"/), rc=status)
    if (status .ne. ESMF_SUCCESS) return

    call ESMF_FieldBundleDestroy(fieldbundle, rc=rc)
    call ESMF_FieldDestroy(field, rc=rc)
    call ESMF_GridDestroy(grid)
    call ESMF_FieldDestroy(field4, rc=rc)
    call ESMF_FieldDestroy(field5, rc=rc)

#if 0

AttributeUpdateContainerStress

The State and FieldBundle containers will be stressed with Add, Replace, and Remove
operations on the Attribute bearing objects before they are Reconciled.  After the
Reconcile call tests are done to verify that the structure of the Container is 
correct.  Then the remaining Attribute bearing objects will be heavily manipulated
with Attribute packages Added and Removed, and the individual Attributes within them
Set and re-Set and Removed multiple times.  AttributeUpdate will be called and then 
another round of tests is done to verify that the Attribute structure and values are
correct.

Pre-Reconcile

create 10 fields and 3 fieldbundles.  each field receives a standard attribute package
with one of the attributes set.  field1 is added to fieldbundle1 which is added to the
state.  fields 2-9 are added to fieldbundle2. field10 is added directly to the state.
field2 and field3 are removed from fieldbundle2, then fieldbundle1 is replaced by 
fieldbundle2.  field2 and field3 are added back to fieldbundle2 with an addreplace call
and then field4 and field5 are removed.

after all of this we have fieldbundle2 and field10 on the state and fieldbundle2
should include field2, 3, 6, 7, 8, 9.

#endif


  end subroutine userm1_init

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
!   !User Comp Component created by higher level calls, here is the
!   ! Initialization routine.

  subroutine usercpl_init(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_VM)         :: vm

    rc = ESMF_SUCCESS

    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_StateReconcile(importState, vm=vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_StateReconcile(exportState, vm=vm, attreconflag=ESMF_ATTRECONCILE_ON, rc=rc)
    if (rc/=ESMF_SUCCESS) return

  end subroutine usercpl_init

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !

  subroutine userm1_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_AttPack)          :: attpack, attpack_parent 
    type(ESMF_VM)               :: vm
    integer                     :: petCount, myPet
    character(ESMF_MAXSTR)      :: name,value,convESMF,purpGen, outVal
    type(ESMF_Field)            :: field2,field3,field6,field7,&
                                   field8,field9,field10
    type(ESMF_FieldBundle)      :: fieldbundle2
    type(ESMF_Grid)             :: grid

    rc = ESMF_SUCCESS

#if 0
here we should have fieldbundle2 and field10 on the state and fieldbundle2
should include field2, 3, 6, 7, 8, 9.

we will pull fieldbundle2 out of the state, and all of the fields out of fieldbundle2.
then for each of the fields we will make different run time modifications:
- field2: remove the attribute package
- field3: add an attribute to the attribute package
- field6: remove an attribute from the package and add a new attribute to the package
- field7: add an attribute package
- field8: add a nested attribute package to the already existing one
- field9: remove the attribute package, add it back, remove all attributes, add one back and then modify the value
#endif

    convESMF = 'ESMF'
    purpGen = 'General'
    name = 'ShortName'
    value = 'fieldAttribute'

    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
    call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    call ESMF_StateGet(exportState, "fieldbundle2", fieldbundle2, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_StateGet(exportState, "field10", field10, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_FieldBundleGet(fieldbundle2, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field2", field=field2, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field3", field=field3, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field6", field=field6, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field7", field=field7, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field8", field=field8, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field9", field=field9, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! field2: add an attribute to the package and then set it
    call ESMF_AttributeGetAttPack(field2, convESMF, purpGen, attpack=attpack, rc=rc)
    call ESMF_AttributeAdd(field2, convESMF, purpGen, attrList=(/"att1"/), &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field2, "att1", "val1", attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! field3: add an attribute to a package and set it, then delete it, readd it and set it to a different value
    call ESMF_AttributeGetAttPack(field3, convESMF, purpGen, attpack=attpack, rc=rc)
    call ESMF_AttributeAdd(field3, convESMF, purpGen, attrList=(/"att1"/), &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field3, "att1", "val1", attpack=attpack, rc=rc)
    call ESMF_AttributeRemove(field3, name="att1", attpack=attpack, rc=rc)
    call ESMF_AttributeAdd(field3, convESMF, purpGen, attrList=(/"att1"/), &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field3, "att1", "val2", attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! field6: add a nested attribute package and set a value
    call ESMF_AttributeAdd(field6, "newConvention", "newPurpose", &
                           attrList=(/"att1","att2"/), &
                           nestConvention=convESMF, nestPurpose=purpGen, &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field6, "att1", "val1", attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! field7: add a nested attribute package (around old package), set a value, 
    ! remove the whole nested package structure, then readd the outside part
    ! of the nested package and set a value
    call ESMF_AttributeAdd(field7, "newConvention", "newPurpose", &
                           attrList=(/"att1","att2"/), &
                           nestConvention=convESMF, nestPurpose=purpGen, &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field7, "att1", "val1", attpack=attpack, rc=rc)
    call ESMF_AttributeRemove(field7, attpack=attpack, rc=rc)
    call ESMF_AttributeAdd(field7, "newConvention", "newPurpose", &
                           attrList=(/"att1","att2"/), &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field7, "att1", "val2", attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! field8: add a nested package, then remove all attributes from parent package and the nested package,
    ! then add it all back, and modify a value
    call ESMF_AttributeGetAttPack(field8, convESMF, purpGen, &
                                  attpack=attpack_parent, rc=rc)
    call ESMF_AttributeAdd(field8, "newConvention", "newPurpose", &
                           attrList=(/"att1","att2"/), &
                           nestConvention=convESMF, nestPurpose=purpGen, &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field8, "att1", "val1", attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field8, "att2", "val2", attpack=attpack, rc=rc)
    call ESMF_AttributeRemove(field8, name="ShortName", &
                              attpack=attpack_parent, rc=rc)
    call ESMF_AttributeRemove(field8, name="StandardName", &
                              attpack=attpack_parent, rc=rc)
    call ESMF_AttributeRemove(field8, name="LongName", &
                              attpack=attpack_parent, rc=rc)
    call ESMF_AttributeRemove(field8, name="Units", &
                              attpack=attpack_parent, rc=rc)
    call ESMF_AttributeRemove(field8, attpack=attpack_parent, rc=rc)
    call ESMF_AttributeRemove(field8, attpack=attpack, rc=rc)

    call ESMF_AttributeAdd(field8, convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeAdd(field8, "newConvention", "newPurpose", &
                           attrList=(/"att1","att2"/), &
                           nestConvention=convESMF, nestPurpose=purpGen, &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(field8, "att1", "val1.2", attpack=attpack, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    ! field9: open for a new test

    ! field10: open for a new test



  end subroutine userm1_run

!-------------------------------------------------------------------------
!   !  The Run routine where data is coupled.
!   !

  subroutine usercpl_run(comp, importState, exportState, clock, rc)
    type(ESMF_CplComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_VM)               :: vm
    integer                     :: myPet

    integer, dimension(2)       :: rootList

    rc = ESMF_SUCCESS

    call ESMF_CplCompGet(comp, vm=vm, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_VMGet(vm, localPet=myPet, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    call ESMF_StateGet(importState, rc=rc)
    if (rc/=ESMF_SUCCESS) return
    call ESMF_StateGet(exportState, rc=rc)
    if (rc/=ESMF_SUCCESS) return

    rootList = (/0,1/)
    call ESMF_AttributeUpdate(importState, vm, rootList=rootList, rc=rc)
    if (rc/=ESMF_SUCCESS) return

  end subroutine usercpl_run

!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !

  subroutine userm2_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

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

#if 0
    if (myPet .eq. 2) then
      call ESMF_AttributeWrite(importState,convESMF,purpGen, &
        attwriteflag=ESMF_ATTWRITE_XML, rc=rc)
      call ESMF_AttributeWrite(importState,convESMF,purpGen,rc=rc)
      if (rc .ne. ESMF_SUCCESS) return
    endif
#endif

  end subroutine userm2_run

!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !

  subroutine userm1_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    type(ESMF_Field)            :: field2, field3, field6, field7, field8, &
                                   field9, field10
    type(ESMF_FieldBundle)      :: fieldbundle
    type(ESMF_Grid)             :: grid

    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_StateGet(exportState, "fieldbundle2", fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_StateGet(exportState, "field10", field10, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_FieldBundleGet(fieldbundle, fieldname="field2", field=field2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleGet(fieldbundle, fieldname="field3", field=field3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleGet(fieldbundle, fieldname="field6", field=field6, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleGet(fieldbundle, fieldname="field7", field=field7, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleGet(fieldbundle, fieldname="field8", field=field8, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleGet(fieldbundle, fieldname="field9", field=field9, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldBundleGet(fieldbundle, grid=grid, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_FieldBundleDestroy(fieldbundle, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out

    call ESMF_FieldDestroy(field2, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldDestroy(field3, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldDestroy(field6, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldDestroy(field7, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldDestroy(field8, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldDestroy(field9, rc=rc)
    if (rc/=ESMF_SUCCESS) return ! bail out
    call ESMF_FieldDestroy(field10, rc=rc)
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

program ESMF_AttributeUpdateContainerStressUTest

#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeUpdateUTest - Attribute Update Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute Update unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF
  use ESMF_TestMod
  use AttributeUpdateContainerStressUTestMod, only : &
    userm1_setvm, userm1_register, &
    userm2_setvm, userm2_register, &
    usercpl_setvm, usercpl_register


  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------


    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = ESMF_SUCCESS

    ! local variables
    integer                 :: petCount, localPet
    type(ESMF_VM)           :: vm
    type(ESMF_State)        :: c1exp, c2imp
    type(ESMF_GridComp)     :: gridcomp1
    type(ESMF_GridComp)     :: gridcomp2
    type(ESMF_CplComp)      :: cplcomp
    character(ESMF_MAXSTR)  :: convESMF,purpGen

  	type(ESMF_AttPack)      :: attpack, attpack_parent
    type(ESMF_Field)        :: field2, field3, field6, field7, field8, field9, field10
    type(ESMF_FieldBundle)  :: fieldbundle2
    type(ESMF_Grid)         :: grid

    character(ESMF_MAXSTR)  :: name1, value1, outVal, attname
    character(ESMF_MAXSTR),dimension(2) :: attrList, valueList
    logical                 :: isPresent1, isPresent2

!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

    !-----------------------------------------------------------------------------
    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !-----------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE
    call ESMF_VMGetCurrent(vm, rc=rc) 
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    gridcomp1 = ESMF_GridCompCreate(name="gridcomp1", &
      petList=(/0,1/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    gridcomp2 = ESMF_GridCompCreate(name="gridcomp2", &
      petList=(/2,3/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    cplcomp = ESMF_CplCompCreate(name="cplcomp", &
      petList=(/0,1,2,3/), rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    c1exp = ESMF_StateCreate(name="Comp1 exportState", &
                             stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    c2imp = ESMF_StateCreate(name="Comp2 importState", &
                             stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetVM(gridcomp1, userm1_setvm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetVM(gridcomp2, userm2_setvm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetVM(cplcomp, usercpl_setvm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetServices(gridcomp1, userRoutine=userm1_register, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompSetServices(gridcomp2, userRoutine=userm2_register, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetServices(cplcomp, userRoutine=usercpl_register, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompInitialize(gridcomp1, exportState=c1exp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(gridcomp2, importState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompInitialize(cplcomp, importState=c1exp, &
      exportState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! now we test to see if the values are what we would expect
    call ESMF_StateGet(c1exp, "fieldbundle2", fieldbundle2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateGet(c1exp, "field10", field10, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleGet(fieldbundle2, grid=grid, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field2", field=field2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field3", field=field3, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field6", field=field6, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field7", field=field7, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field8", field=field8, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleGet(fieldbundle2, fieldname="field9", field=field9, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


    ! Now continue with the run phase, and AttributeUpdate
    call ESMF_GridCompRun(gridcomp1, exportState=c1exp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompRun(cplcomp, importState=c1exp, &
      exportState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompRun(gridcomp2, importState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


    ! at this point we have fieldbundle2 and field10 on the state and fieldbundle2
    ! should include field2, 3, 6, 7, 8, 9.

    ! Now we can start doing some testing
    convESMF = 'ESMF'
    purpGen = 'General'
    name1 = 'ShortName'
    value1 = 'fieldAttribute'

    ! field2: add an attribute to the package and then set it
    !EX_UTest_Multi_Proc_Only
    attname = "att1"
    outVal = ""
    call ESMF_AttributeGetAttPack(field2, convESMF, purpGen, attpack=attpack, &
                                  rc=rc)
    call ESMF_AttributeGet(field2, attname, value=outVal, attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Testing for correct value on field2"
    call ESMF_Test((rc==ESMF_SUCCESS .and. outVal=="val1"), &
                    name, failMsg, result, ESMF_SRCLINE)

    ! field3: add an attribute to a package and set it, then delete it, readd it and set it to a different value
    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGetAttPack(field3, convESMF, purpGen, attpack=attpack, &
                                  rc=rc)
    attname = "att1"
    outVal = ""
    call ESMF_AttributeGet(field3, attname, value=outVal, attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Testing for correct value on field3"
    call ESMF_Test((rc==ESMF_SUCCESS .and. outVal == "val2"), &
                    name, failMsg, result, ESMF_SRCLINE)

    ! field6: add a nested attribute package and set a value
    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGetAttPack(field6, "newConvention", "newPurpose", &
                                  attpack=attpack, rc=rc)
    attname = "att1"
    outVal = ""
    call ESMF_AttributeGet(field6, attname, value=outVal, attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Testing for correct value on field6"
    call ESMF_Test((rc==ESMF_SUCCESS .and. outVal == "val1"), &
                    name, failMsg, result, ESMF_SRCLINE)

    ! field7: add a nested attribute package, set a value, remove the nested package, 
    ! then readd the nested package and reset the value
    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGetAttPack(field7, "newConvention", "newPurpose", &
                                  attpack=attpack_parent, rc=rc)
    call ESMF_AttributeGetAttPack(field7, convESMF, purpGen, &
                                  attpack=attpack, rc=rc)
    attname = "att1"
    outVal = ""
    call ESMF_AttributeGet(field7, attname, value=outVal, attpack=attpack_parent, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Testing for correct value on field7"
    call ESMF_Test((rc==ESMF_SUCCESS .and. outVal == "val2"), &
                    name, failMsg, result, ESMF_SRCLINE)

    ! field8: add a nested package, then remove all attributes from parent package and the nested package,
    ! then add it all back, remove all attributes from nested package, add one back and then modify the value
    !EX_UTest_Multi_Proc_Only
    call ESMF_AttributeGetAttPack(field8, "newConvention", "newPurpose", &
                                  attpack=attpack, rc=rc)
    attname = "att1"
    outVal = ""
    call ESMF_AttributeGet(field8, attname, value=outVal, attpack=attpack, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS or wrong value"
    write(name, *) "Testing for correct value on field8"
    call ESMF_Test((rc==ESMF_SUCCESS .and. outVal == "val1.2"), &
                    name, failMsg, result, ESMF_SRCLINE)

    ! field9: open for a new test

    ! field10: open for a new test

    ! Now back to finalizing the model run
    call ESMF_GridCompFinalize(gridcomp1, exportState=c1exp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompFinalize(gridcomp2, importState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompFinalize(cplcomp, importState=c1exp, &
      exportState=c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompDestroy(gridcomp1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(gridcomp2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_CplCompDestroy(cplcomp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateDestroy(c1exp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateDestroy(c2imp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#endif
    !-----------------------------------------------------------------------------
    call ESMF_TestEnd(ESMF_SRCLINE)
    !-----------------------------------------------------------------------------

end program ESMF_AttributeUpdateContainerStressUTest
