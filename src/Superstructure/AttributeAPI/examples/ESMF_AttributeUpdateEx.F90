! $Id: ESMF_AttributeUpdateEx.F90,v 1.16.2.1 2010/02/05 20:03:28 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================


!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_AttributeUpdateEx

!  !PROGRAM: ESMF\_AttributeUpdateEx - Example of Attribute usage in a distributed environment.
!
!  !DESCRIPTION: 
!
! This program shows examples of Attribute usage

  ! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  use ESMF_AttributeUpdateMod, only : userm1_setvm, userm1_register, &
    userm2_setvm, userm2_register, usercpl_setvm, usercpl_register

implicit none


!BOE
! \subsubsection{Example: Advanced Attribute usage: Attributes in a Distributed Environment}
!
! This advanced example illustrates the proper methods of Attribute manipulation
! in a distributed environment to ensure consistency of metadata across the VM. 
! This example is much more complicated than the previous two because we will
! be following the flow of control of a typical model run with two gridded Components
! and one coupling Component.  We will start out in the application driver, declaring
! Components, States, and the routines used to initialize, run and finalize the user's
! model Components.  Then we will follow the control flow into the actual Component level
! through initialize, run, and finalize examining how Attributes are used to organize the
! metadata.
!
! This example follows a simple user model with two gridded Components and one coupling Component. 
! The initialize routines are used to set up the application data and the run 
! routines are used to manipulate the data.  Accordingly, most of the Attribute manipulation
! will take place in the initialize phase of each of the three Components.  The two gridded
! Components will be running on exclusive pieces of the VM and the coupler Component will 
! encompass the entire VM so that it can handle the Attribute communications.  
!
! The control flow of this
! example will start in the application driver, after which it will complete three cycles
! through the three Components.  The first cycle will be through the initialize routines, 
! from the first gridded Component to the second gridded Component to the coupler Component.  The 
! second cycle will go through the run routines, from the first gridded Component to the 
! coupler Component to the second Gridded component.  The third cycle will be through the
! finalize routines in the same order as the first cycle.
!
! The first thing we must do is declare variables and initialize ESMF in the application driver.
!EOE


!BOC
      integer                 :: rc, finalrc, petCount, localPet
      type(ESMF_VM)           :: vm
      type(ESMF_State)        :: c1exp, c2imp
      type(ESMF_GridComp)     :: gridcomp1
      type(ESMF_GridComp)     :: gridcomp2
      type(ESMF_CplComp)      :: cplcomp
      character(ESMF_MAXSTR)  :: convESMF,purpGen

      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, rc=rc)
      
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
      if (rc/=ESMF_SUCCESS) print *, "ERROR!"
!EOC         
      
      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "Start of ESMF_AttributeUpdate Example"
        print *, "--------------------------------------- "
      endif

!BOE
! Still in the application driver, we must now construct some ESMF objects, 
! such as the gridded Components, the coupler Component, and the States.  This
! is also where it is determined which subsets of the PETs of the VM the
! Components will be using to run their initialize, run, and finalize routines.
!EOE


      if (petCount<4) then
        gridcomp1 = ESMF_GridCompCreate(name="gridcomp1", &
          petList=(/0/), rc=rc)
        gridcomp2 = ESMF_GridCompCreate(name="gridcomp2", &
          petList=(/0/), rc=rc)
        cplcomp = ESMF_CplCompCreate(name="cplcomp", &
          petList=(/0/), rc=rc)
      else
!BOC
        gridcomp1 = ESMF_GridCompCreate(name="gridcomp1", &
          petList=(/0,1/), rc=rc)
        gridcomp2 = ESMF_GridCompCreate(name="gridcomp2", &
          petList=(/2,3/), rc=rc)
        cplcomp = ESMF_CplCompCreate(name="cplcomp", &
          petList=(/0,1,2,3/), rc=rc)

      c1exp = ESMF_StateCreate("Comp1 exportState", &
        ESMF_STATE_EXPORT, rc=rc)
      c2imp = ESMF_StateCreate("Comp2 importState", &
        ESMF_STATE_IMPORT, rc=rc)
!EOC      

      endif
 
! statecreates are in the if temporarily so that I can do there is not
! a random endif in the protex while investigating the uni failures in
! this example, which will not run UNI in the end anyway
                
      call ESMF_GridCompSetVM(gridcomp1, userm1_setvm, rc)
      call ESMF_GridCompSetVM(gridcomp2, userm2_setvm, rc)
      call ESMF_CplCompSetVM(cplcomp, usercpl_setvm, rc)

      call ESMF_GridCompSetServices(gridcomp1, userm1_register, rc)
      call ESMF_GridCompSetServices(gridcomp2, userm2_register, rc)
      call ESMF_CplCompSetServices(cplcomp, usercpl_register, rc)

!BOE
! Before the individual components are initialized, run, and finalized Attributes should be set at the
! Component level.  Here we are going to use the ESG Attribute package on 
! the first gridded Component.  The Attribute package is added, and then
! each of the Attributes is set.  The Attribute hierarchy of the Component
! is then linked to the Attribute hierarchy of the export State in a 
! manual fashion.
!EOE

!BOC
      convESMF = 'ESMF'
      purpGen = 'General'
    call ESMF_AttributeAdd(gridcomp1, convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp1, 'Agency', 'NASA', &
      convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp1, 'Author', 'Max Suarez', &
      convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp1, 'CodingLanguage', &
      'Fortran 90', convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp1, 'Discipline', &
      'Atmosphere', convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp1, 'FullName', &
      'Goddard Earth Observing System Version 5 Finite Volume Dynamical Core', &
        convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp1, 'ModelComponentFramework', &
      'ESMF', &
      convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp1, 'Name', 'GEOS-5 FV dynamical core', &
      convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp1, 'PhysicalDomain', &
      'Earth system', convention=convESMF, purpose=purpGen, rc=rc)
    call ESMF_AttributeSet(gridcomp1, 'Version', &
      'GEOSagcm-EROS-beta7p12', convention=convESMF, purpose=purpGen, rc=rc)
      
      call ESMF_AttributeLink(gridcomp1, c1exp, rc=rc) 
!EOC

!BOE
! Now the individual Components will be run.  First we will initialize the two
! gridded Components, then we will initialize the coupler Component. 
! During each of these Component initialize routines Attribute
! packages will be added, and the Attributes set.  The Attribute
! hierarchies will also be linked.  As the gridded Components will
! be running on exclusive portions of the VM, the Attributes will need to 
! be made available across the VM using an {\tt ESMF\_StateReconcile()}
! call in the coupler Component.  The majority of the work
! with Attributes will take place in this portion of the 
! model run, as metadata rarely needs to be changed during run time.  
!
! What 
! follows are the calls from the driver code that run the initialize, run, and finalize routines
! for each of the Components.  After these calls we will step through the first 
! cycle as explained in the introduction, through the intialize routines of
! gridded Component 1 to gridded Component 2 to the coupler Component.
!EOE

!BOC
      call ESMF_GridCompInitialize(gridcomp1, exportState=c1exp, rc=rc)
      call ESMF_GridCompInitialize(gridcomp2, importState=c2imp, rc=rc)
      call ESMF_CplCompInitialize(cplcomp, importState=c1exp, &
        exportState=c2imp, rc=rc)

      call ESMF_GridCompRun(gridcomp1, exportState=c1exp, rc=rc)
      call ESMF_CplCompRun(cplcomp, importState=c1exp, &
        exportState=c2imp, rc=rc)
      call ESMF_GridCompRun(gridcomp2, importState=c2imp, rc=rc)
      
      call ESMF_GridCompFinalize(gridcomp1, exportState=c1exp, rc=rc)
      call ESMF_GridCompFinalize(gridcomp2, importState=c2imp, rc=rc)
      call ESMF_CplCompFinalize(cplcomp, importState=c1exp, &
        exportState=c2imp, rc=rc)
!EOC
      
      call ESMF_GridCompDestroy(gridcomp1, rc=rc)
      call ESMF_GridCompDestroy(gridcomp2, rc=rc)
      call ESMF_CplCompDestroy(cplcomp, rc=rc)
      
      call ESMF_StateDestroy(c1exp, rc=rc)
      call ESMF_StateDestroy(c2imp, rc=rc)

    call ESMF_Finalize(rc=rc)

10  if (localPet==0) then
      print *, "--------------------------------------- "
      print *, "End of ESMF_AttributeUpdate Example"
      print *, "--------------------------------------- "
  endif

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_AttributeUpdateEx.F90"
  else
    print *, "FAIL: ESMF_AttributeUpdateEx.F90"
  endif
  
end program ESMF_AttributeUpdateEx
