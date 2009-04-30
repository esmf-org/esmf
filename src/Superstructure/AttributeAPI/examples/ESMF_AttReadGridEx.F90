! $Id: ESMF_AttReadGridEx.F90,v 1.1 2009/04/30 05:01:01 eschwab Exp $
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
program ESMF_AttReadGridEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Example: Reading an XML file-based GridSpec Attribute Package for a Grid}
!
!EOE

#include "ESMF.h"

!-----------------------------------------------------------------------------
! !PROGRAM: ESMF\_AttReadGridEx - Example of reading an XML-file based GridSpec
!   Attribute Package for a Grid.
!
! !DESCRIPTION:
!
! This program shows an example of reading an XML-based file of an GridSpec
! Attribute Package for a Grid and placing it on an ESMF\_Grid object.
!
!-----------------------------------------------------------------------------

!BOC
      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! local variables
      type(ESMF_Grid)        :: grid
      character(ESMF_MAXSTR) :: attrvalue
      type(ESMF_VM)          :: vm
      integer                :: rc
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
      ! Create a grid
      grid = ESMF_GridCreateEmpty(rc=rc)
!EOC

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
      ! Read an XML file to populate the GridSpec Attribute package of a Grid
      call ESMF_AttributeRead(grid=grid, fileName="esmf_grid.xml", rc=rc)
!EOC
      if (rc==ESMF_RC_LIB_NOT_PRESENT) then
        xercesPresent = .false.
      endif

      if (rc .ne. ESMF_SUCCESS .and. xercesPresent) finalrc = ESMF_FAILURE

print *, 'rc = ', rc

!BOC
      ! Get GridSpec "congruenttiles" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='congruenttiles', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='true') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "gridtype" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='gridtype', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='cubed sphere') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "dimorder" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='dimorder', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='YX') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "discretizationtype" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='discretizationtype', &
                             value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='logically_rectangular') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "geometrytype" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='geometrytype', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='sphere') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "isconformal" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='isconformal', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='false') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "ispolecovered" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='ispolecovered', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='true') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "isregular" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='isregular', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='false') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "isuniform" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='isuniform', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='false') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "northpolelocation" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='northpolelocation', &
                             value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='long: 0.0 lat: 90.0') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "numberofcells" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='numberofcells', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='53457') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "numdims" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='numdims', &
                             value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='2') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "nx" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='nx', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='96') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "ny" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='ny', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='96') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "nz" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='nz', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='15') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "resolution" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='resolution', value=attrValue, &
                             convention='GridSpec', purpose='general', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='C48') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
print *, 'rc = ', rc
print *, 'attrvalue = ', attrvalue

!BOC
      call ESMF_GridDestroy(grid, rc=rc)
!EOC

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc=rc)
!EOC

      if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc .eq. ESMF_SUCCESS) then
         print *, "PASS: ESMF_AttReadGridEx.F90"
      else
         print *, "FAIL: ESMF_AttReadGridEx.F90"
      end if

end program ESMF_AttReadGridEx
