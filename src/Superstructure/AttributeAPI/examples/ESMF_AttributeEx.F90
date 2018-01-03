! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_AttributeEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
#include "ESMF.h"

!BOE
! \subsubsection{Basic Attribute usage} \label{ex:AttributeEx}
!
! This example illustrates the most basic usage of the Attribute class.  
! This demonstration of Attribute manipulation is limited to the gridded 
! Component, but the same principles apply to the coupler Component, State, 
! Grid, FieldBundle, Field, ArrayBundle and Array.  The
! functionality that is demonstrated includes setting and getting Attributes, 
! working with Attributes with different types and lists, removing Attributes,
! and getting default Attributes.  Various other uses of 
! {\tt ESMF\_AttributeGet()} is covered in detail in the last section.  The
! first thing we must do is declare variables and initialize ESMF.
!EOE


!  !PROGRAM: ESMF\_Attribute - Examples of Attribute usage.
!
!  !DESCRIPTION: 
!
! This program shows examples of Attribute usage


!BOC
      ! Use ESMF framework module
      use ESMF
      use ESMF_TestMod
      implicit none

      ! Local variables  
      integer                 :: rc, finalrc, petCount, localPet, &
                                 itemCount, count, result
      type(ESMF_VM)           :: vm
      type(ESMF_GridComp)     :: gridcomp
      character(ESMF_MAXSTR)  :: name
      type(ESMF_TypeKind_Flag)     :: tk

      integer(ESMF_KIND_I4)                :: inI4
      integer(ESMF_KIND_I4), dimension(3)  :: inI4l
      integer(ESMF_KIND_I8)                :: inI8
      integer(ESMF_KIND_I8), dimension(3)  :: inI8l
      real(ESMF_KIND_R4)                   :: inR4
      real(ESMF_KIND_R4), dimension(3)     :: inR4l
      real(ESMF_KIND_R8)                   :: inR8
      real(ESMF_KIND_R8), dimension(3)     :: inR8l
      character(ESMF_MAXSTR)               :: inChar
      character(ESMF_MAXSTR), dimension(3) :: inCharl, &
                                           defaultCharl, dfltoutCharl
      character(ESMF_MAXSTR), dimension(8) :: outCharl
      logical                              :: inLog
      logical, dimension(3)                :: inLogl, value
      character(ESMF_MAXSTR)               :: testname
      character(ESMF_MAXSTR)               :: failMsg
!EOC

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      write(failMsg, *) "Example failure"
      write(testname, *) "Example ESMF_AttributeEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

!BOC

      
      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, defaultlogfilename="AttributeEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
      
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
!EOC         
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "Start of ESMF_Attribute Example"
        print *, "--------------------------------------- "
      endif

!BOE
!    We will construct the gridded Component which will be responsible for all
!    of the Attributes we will be manipulating.
!EOE
!BOC
      if (petCount<4) then
        gridcomp = ESMF_GridCompCreate(name="gridcomp", &
          petList=(/0/), rc=rc)
      else 
        gridcomp = ESMF_GridCompCreate(name="gridcomp", &
          petList=(/0,1,2,3/), rc=rc)
      endif
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!     We can set Attributes using the {\tt ESMF\_AttributeSet()} command.  
!     Attributes can be any of several different types, all of which are 
!     demonstrated here.
!EOE

!BOC
      inI4 = 4
      inI4l = (/1,2,3/)
      inI8 = 4
      inI8l = (/1,2,3/)
      inR4 = 4
      inR4l = (/1,2,3/)
      inR8 = 4
      inR8l = (/1,2,3/)
      inChar = "Character string 4"
      inCharl = (/ "Character string 1", &
                   "Character string 2", &
                   "Character string 3" /)
      inLog = .true.
      inLogl = (/.true., .false., .true. /)
      
      call ESMF_AttributeSet(gridcomp, name="ESMF_I4name", value=inI4, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, name="ESMF_I4namelist", &
        valueList=inI4l, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, name="ESMF_I8name", value=inI8,  rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, name="ESMF_I8namelist", &
        valueList=inI8l, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, name="ESMF_R4name", value=inR4, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, name="ESMF_R4namelist", &
        valueList=inR4l, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, name="ESMF_R8name", value=inR8, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, name="ESMF_R8namelist", &
        valueList=inR8l, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, name="Character_name", &
        value=inChar, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, name="Character_namelist", &
        valueList=inCharl, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, name="Logical_name", value=inLog, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(gridcomp, name="Logical_namelist", &
        valueList=inLogl, rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!     We can retrieve Attributes by issuing the {\tt ESMF\_AttributeGet()} 
!     command.  This command can also be used with an optional default 
!     value (or value list) so that if the Attribute is not found a value is 
!     returned without an error code.  Removal of Attributes is also 
!     possible, and is demonstrated here as well.  One of the Attributes
!     previously created will be retrieved, then removed, then 
!     retrieved again using a default return value.  In order to use the 
!     default return value capabilites, we must first set up a default parameter.
!EOE

!BOC
      defaultCharl = (/ "Character string 4", &
                        "Character string 5", &
                        "Character string 6" /)
      
      itemCount=3
      call ESMF_AttributeGet(gridcomp, name="Character_namelist", &
        valueList=outCharl(1:5), itemCount=itemCount, rc=rc) 
!EOC  
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

                    
      call ESMF_AttributeRemove(gridcomp, name="Character_namelist", rc=rc)
!EOC  
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      
      call ESMF_AttributeGet(gridcomp, name="Character_namelist", &
        valueList=dfltoutCharl, defaultvalueList=defaultCharl,rc=rc)

!EOC  
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      if (all (dfltoutCharl /= defaultCharl)) then
        print *, "Attribute character list IN did not match OUT"
        print *, "dfltoutCharl(1) = ", dfltoutCharl(1)
        print *, "dfltoutCharl(2) = ", dfltoutCharl(2)
        print *, "dfltoutCharl(3) = ", dfltoutCharl(3)
        print *, "defaultCharl(1) = ", defaultCharl(1)
        print *, "defaultCharl(2) = ", defaultCharl(2)
        print *, "defaultCharl(3) = ", defaultCharl(3)
      endif

!BOE
!    There are more overloaded instances of {\tt ESMF\_AttributeGet()} 
!    which allow the retrieval of Attribute information by name or index 
!    number, or a query for the count of the Attributes on a certain object.  
!    These capabilities are demonstrated here by first retrieving the name of
!    an Attribute using the index number, keep in mind that these index
!    numbers start from 1.  Then the name that is retrieved
!    is used to get other information about the Attribute, such as the
!    typekind, and the number of items in the value of the Attribute.
!    This information is then used to actually retrieve the Attribute value.
!    Then the count of the number of Attributes on the object will be retrieved.
!EOE

!BOC
      call ESMF_AttributeGet(gridcomp, attributeIndex=11 , name=name, rc=rc)
!EOC  
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      
      call ESMF_AttributeGet(gridcomp, name=name, typekind=tk, &
        itemCount=itemCount, rc=rc)
!EOC  
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      
      if (tk==ESMF_TYPEKIND_Logical .AND. itemCount==3) then
        call ESMF_AttributeGet(gridcomp, name=name, valueList=value, rc=rc)
!EOC  
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      endif
      
      call ESMF_AttributeGet(gridcomp, count=count, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


      if (all (value .NEQV. inLogl)) then
        print *, "Attribute logical list IN did not match OUT"
      endif
 
      call ESMF_GridCompDestroy(gridcomp,rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
      ! file that the scripts grep for.
      call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


    call ESMF_Finalize(rc=rc)

  if (localPet==0) then
      print *, "--------------------------------------- "
      print *, "End of ESMF_Attribute Example"
      print *, "--------------------------------------- "
  endif

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_AttributeEx.F90"
  else
    print *, "FAIL: ESMF_AttributeEx.F90"
  endif
  
end program
