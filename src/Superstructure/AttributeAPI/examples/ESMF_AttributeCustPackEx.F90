! $Id: ESMF_AttributeCustPackEx.F90,v 1.3 2012/01/06 20:18:48 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_AttributeCustPackEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Custom Attribute package}  \label{ex:AttributeCustPackEx}
!
! This example illustrates how to create a user-defined, custom Attribute 
! package.  The package is created on a gridded Component with three custom
! Attributes.
!EOE


!  !PROGRAM: ESMF\_AttributeCustPackEx - Example of custom Attribute Package usage.
!
!  !DESCRIPTION: 
!
! This program shows an example of custom Attribute package usage


      ! Use ESMF framework module
      use ESMF
      implicit none

      ! Local variables  
      integer                 :: rc, finalrc, petCount, localPet
      type(ESMF_VM)           :: vm
      type(ESMF_GridComp)     :: gridcomp
      character(ESMF_MAXSTR)  :: customConv, customPurp
      character(ESMF_MAXSTR),dimension(3)   :: customAttrList         

      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, &
                    defaultlogfilename="AttributeCustPackEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
      
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
      
      if (localPet==0) then
        print *, "-------------------------------------- "
        print *, "Start of ESMF_AttributeCustPack Example"
        print *, "-------------------------------------- "
      endif

!BOE
!    We must construct the ESMF gridded Component object that will be 
!    responsible for the custom Attribute package we will be manipulating.
!EOE
!BOC
      if (petCount<4) then
        gridcomp = ESMF_GridCompCreate(name="gridded_comp_ex3", &
          petList=(/0/), rc=rc)
      else 
        gridcomp = ESMF_GridCompCreate(name="gridded_comp_ex3", &
          petList=(/0,1,2,3/), rc=rc)
      endif
!EOC

!BOE
!    Now we can add a custom Attribute package to the gridded Component object.
!EOE

!BOC 
      customConv = 'CustomConvention'
      customPurp = 'CustomPurpose'

      customAttrList(1) = 'CustomAttrName1'
      customAttrList(2) = 'CustomAttrName2'
      customAttrList(3) = 'CustomAttrName3'

      call ESMF_AttributeAdd(gridcomp, convention=customConv, &
        purpose=customPurp, attrList=customAttrList, rc=rc)

!BOE
!     We must set the Attribute values of our custom Attribute package.
!EOE

!BOC
    call ESMF_AttributeSet(gridcomp, 'CustomAttrName1', 'CustomAttrValue1', &
      convention=customConv, purpose=customPurp, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'CustomAttrName2', 'CustomAttrValue2', &
      convention=customConv, purpose=customPurp, rc=rc)
    call ESMF_AttributeSet(gridcomp, 'CustomAttrName3', 'CustomAttrValue3', &
      convention=customConv, purpose=customPurp, rc=rc)
!EOC

!BOE
!     Write out the contents of our custom Attribute package to an XML file,
!     which is generated with a .xml file extension in the execution directory. 
!EOE


      if (localPet==0) then
!BOC
      call ESMF_AttributeWrite(gridcomp,customConv,customPurp, &
        attwriteflag=ESMF_ATTWRITE_XML,rc=rc)
!EOC
      endif

    ! Destroy
    call ESMF_GridCompDestroy(gridcomp, rc=rc)

  if (localPet==0) then
      print *, "------------------------------------ "
      print *, "End of ESMF_AttributeCustPack Example"
      print *, "------------------------------------ "
  endif

  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_AttributeCustPackEx.F90"
  else
    print *, "FAIL: ESMF_AttributeCustPackEx.F90"
  endif
  
end program
