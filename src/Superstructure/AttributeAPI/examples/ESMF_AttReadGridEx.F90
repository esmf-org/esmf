! $Id: ESMF_AttReadGridEx.F90,v 1.6.2.1 2010/02/05 20:03:28 svasquez Exp $
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
!
program ESMF_AttReadGridEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Example: Reading an XML file-based GridSpec Attribute Package for a Grid}
! This example shows how to read a GridSpec Attribute Package from an
! XML file; see
! ESMF\_DIR/src/Infrastructure/Grid/etc/esmf\_grid.xml.
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

!print *, 'rc = ', rc

!BOC
      ! Get GridSpec "CongruentTiles" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='CongruentTiles', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='true') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "GridType" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='GridType', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='cubed sphere') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "DimOrder" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='DimOrder', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='YX') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "DiscretizationType" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='DiscretizationType', &
                             value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='logically_rectangular') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "GeometryType" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='GeometryType', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='sphere') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "IsConformal" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='IsConformal', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='false') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "IsPoleCovered" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='IsPoleCovered', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='true') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "IsRegular" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='IsRegular', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='false') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "IsUniform" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='IsUniform', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='false') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "NorthPoleLocation" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='NorthPoleLocation', &
                             value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='long: 0.0 lat: 90.0') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "NumberOfCells" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='NumberOfCells', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='53457') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "NumDims" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='NumDims', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='2') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "NX" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='NX', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='96') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "NY" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='NY', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='96') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "NZ" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='NZ', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='15') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get GridSpec "Resolution" Attribute from a Grid
      call ESMF_AttributeGet(grid, name='Resolution', value=attrValue, &
                             convention='GridSpec', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='C48') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

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
