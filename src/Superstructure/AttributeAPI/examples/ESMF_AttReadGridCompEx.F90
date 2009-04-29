! $Id: ESMF_AttReadGridCompEx.F90,v 1.1 2009/04/29 05:57:35 eschwab Exp $
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
!
program ESMF_AttReadGridCompEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Example: Reading an XML file-based ESG Attribute Package for a Gridded Component}
!
!EOE

#include "ESMF.h"

!-----------------------------------------------------------------------------
! !PROGRAM: ESMF\_AttReadGridCompEx - Example of reading an XML-file based ESG
!   Attribute Package for a Gridded Component.
!
! !DESCRIPTION:
!
! This program shows an example of reading an XML-based file of an ESG
! Attribute Package for a Gridded Component and placing it on an
! ESMF\_GridComp object.
!
!-----------------------------------------------------------------------------

!BOC
      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! local variables
      type(ESMF_GridComp)    :: gridcomp
      character(ESMF_MAXSTR) :: attrvalue
      type(ESMF_VM)          :: vm
      integer                :: rc, petCount, localPet
!EOC

      ! example program result codes
      logical :: xercesPresent
      integer :: finalrc

      ! assume Xerces XML C++ API library present until proven otherwise
      xercesPresent = .true.
      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF
      call ESMF_Initialize(vm=vm, rc=rc)
!EOC

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
!EOC

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "Start of ESMF_AttReadGridComp Example"
        print *, "--------------------------------------- "
      endif

!BOC
      if (petCount<4) then
        gridcomp = ESMF_GridCompCreate(name="gridcomp", &
          petList=(/0/), rc=rc)
      else
        gridcomp = ESMF_GridCompCreate(name="gridcomp", &
          petList=(/0,1,2,3/), rc=rc)
      endif
!EOC

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
      ! Read an XML file to populate the ESG Attribute package of a GridComp
      call ESMF_AttributeRead(comp=gridcomp, fileName="esmf_gridcomp.xml", rc=rc)
!EOC
      if (rc==ESMF_RC_LIB_NOT_PRESENT) then
        xercesPresent = .false.
      endif

      if (rc .ne. ESMF_SUCCESS .and. xercesPresent) finalrc = ESMF_FAILURE

print *, 'rc = ', rc

!BOC
      ! Get ESG "name" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='name', value=attrValue, &
                             convention='ESG', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='GEOS') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "full_name" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='full_name', value=attrValue, &
                             convention='ESG', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. &
                 attrvalue=='Goddard Earth Observing System Model') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "agency" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='agency', value=attrValue, &
                             convention='ESG', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='NASA') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "institution" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='institution', value=attrValue, &
                             convention='ESG', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. &
                 attrvalue=='Global Modeling and Assimilation Office (GMAO)') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "version" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='version', value=attrValue, &
                             convention='ESG', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='5') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "author" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='author', value=attrValue, &
                             convention='ESG', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Max Suarez') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "discipline" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='discipline', value=attrValue, &
                             convention='ESG', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Atmosphere') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "physical_domain" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='physical_domain', &
                             value=attrValue, &
                             convention='ESG', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Earth System') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "coding_language" Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name='coding_language', &
                             value=attrValue, &
                             convention='ESG', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Fortran 90') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "model_component_framework" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='model_component_framework', &
                             value=attrValue, &
                             convention='ESG', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. &
                 attrvalue=='ESMF (Earth System Modeling Framework)') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get CF "comment" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='comment', value=attrValue, &
                             convention='CF', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. &
                 attrvalue=='ESMF GridComp Attribute IO Test') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get CF "references" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='references', value=attrValue, &
                             convention='CF', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. &
                 attrvalue=='http://gmao.gsfc.nasa.gov/systems/geos5') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      call ESMF_GridCompDestroy(gridcomp, rc=rc)
!EOC

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc=rc)
!EOC

      if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc .eq. ESMF_SUCCESS) then
         print *, "PASS: ESMF_AttReadGridCompEx.F90"
      else
         print *, "FAIL: ESMF_AttReadGridCompEx.F90"
      end if

end program ESMF_AttReadGridCompEx
