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
!
program ESMF_AttReadGridCompEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Read an XML file-based ESG Attribute package for a Gridded Component}
! This example shows how to read an ESG Attribute Package for a Gridded
! Component from an XML file.  The XML file contains Attribute values filled-in
! by the user.  The standard ESG Component Attribute Package is supplied with
! ESMF and is defined in an XSD file, which is used to validate the XML file.
! See
! \begin{description}
! \item ESMF\_DIR/src/Superstructure/Component/etc/esmf\_gridcomp.xml (Attribute Package values) and
! \item ESMF\_DIR/src/Superstructure/Component/etc/esmf\_comp.xsd (Attribute Package definition).
! \end{description}
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
      use ESMF
      use ESMF_TestMod
      implicit none

      ! local variables
      type(ESMF_GridComp)    :: gridcomp
      type(ESMF_AttPack)   :: attpack
      character(ESMF_MAXSTR) :: attrvalue
      type(ESMF_VM)          :: vm
      integer                :: rc, petCount, localPet
!EOC

      ! example program result codes
      logical :: xercesPresent
      integer :: finalrc, result
      character(ESMF_MAXSTR) :: testname
      character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      write(failMsg, *) "Example failure"
      write(testname, *) "Example ESMF_AttReadGridCompEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------



      ! assume Xerces XML C++ API library present until proven otherwise
      xercesPresent = .true.
      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF
      call ESMF_Initialize(vm=vm, defaultlogfilename="AttReadGridCompEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
!EOC

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

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

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! Read an XML file to populate the ESG Attribute package of a GridComp.
      ! The file is validated against an internal, ESMF-supplied XSD file
      ! defining the standard ESG Component Attribute package (see file
      ! pathnames above).
      call ESMF_AttributeRead(comp=gridcomp, fileName="esmf_gridcomp.xml", &
          rc=rc)
!EOC
      if (rc==ESMF_RC_LIB_NOT_PRESENT) then
        xercesPresent = .false.
      endif

      if (rc .ne. ESMF_SUCCESS .and. xercesPresent) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!print *, 'rc = ', rc

!BOC
      ! Get ESG "ComponentShortName" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='ComponentShortName', &
                             value=attrValue, &
                             convention='ESG', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='GEOS') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "ComponentLongName" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='ComponentLongName', &
                             value=attrValue, &
                             convention='ESG', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. &
                 attrvalue=='Goddard Earth Observing System Model') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "Agency" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='Agency', value=attrValue, &
                             convention='ESG', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='NASA') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "Institution" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='Institution', value=attrValue, &
                             convention='ESG', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. &
                 attrvalue=='Global Modeling and Assimilation Office (GMAO)') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "Version" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='Version', value=attrValue, &
                             convention='ESG', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='5') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "Author" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='Author', value=attrValue, &
                             convention='ESG', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Max Suarez') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "Discipline" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='Discipline', value=attrValue, &
                             convention='ESG', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Atmosphere') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "PhysicalDomain" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='PhysicalDomain', &
                             value=attrValue, convention='ESG', &
                             purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Earth System') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "CodingLanguage" Attribute from a GridComp Test
      call ESMF_AttributeGet(gridcomp, name='CodingLanguage', &
                             value=attrValue,  convention='ESG', &
                             purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Fortran 90') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get ESG "ModelComponentFramework" Attribute from a GridComp
      call ESMF_AttributeGet(gridcomp, name='ModelComponentFramework', &
                             value=attrValue, &
                             convention='ESG', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. &
                 attrvalue=='ESMF (Earth System Modeling Framework)') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      call ESMF_GridCompDestroy(gridcomp, rc=rc)
!EOC

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


      ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
      ! file that the scripts grep for.
      call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)



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
