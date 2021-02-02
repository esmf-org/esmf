! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_SCompEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOP
! \subsubsection{Use ESMF\_SciComp and Attach Attributes}
! \label{sec:component:usage:scicomp}
!
!\begin{sloppypar}
! This example illustrates the use of the ESMF\_SciComp to attach Attributes
! within a Component hierarchy.  The hierarchy includes Coupler, Gridded,
! and Science Components and Attributes are attached to the Science Components.
! For demonstrable purposes, we'll add some CIM Component attributes to
! the Gridded Component.
!\end{sloppypar}
!EOP

#include "ESMF.h"

!-----------------------------------------------------------------------------
!
!  !PROGRAM: ESMF\_SCompEx - Example of Science Component usage.
!
!  !DESCRIPTION: 
!
!  This program shows an example of Science Component usage.

      ! Use ESMF framework module
      use ESMF
      use ESMF_TestMod
      implicit none

      ! Local variables  
      integer                 :: rc, finalrc, petCount, localPet, result
      type(ESMF_VM)           :: vm
      type(ESMF_AttPack)      :: attpack

      type(ESMF_CplComp)      :: cplcomp
      type(ESMF_GridComp)     :: atmcomp, ocncomp
      type(ESMF_SciComp)      :: dc_scicomp, adv_scicomp
      type(ESMF_SciComp)      :: rad_scicomp

      character(ESMF_MAXSTR)  :: convCIM, purpComp, purpProp, purpSci
      character(ESMF_MAXSTR)  :: purpField, purpPlatform
      character(ESMF_MAXSTR)  :: convISO, purpRP, purpCitation

      character(ESMF_MAXSTR), dimension(3)  :: dc_sciPropAtt, adv_sciPropAtt
      character(ESMF_MAXSTR), dimension(2)  :: rad_sciPropAtt


      character(ESMF_MAXSTR)  :: testname
      character(ESMF_MAXSTR)  :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      write(failMsg, *) "Example failure"
      write(testname, *) "Example ESMF_SCompEx"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------



      ! initialize ESMF
      finalrc = ESMF_SUCCESS
      call ESMF_Initialize(vm=vm, defaultlogfilename="SCompEx.Log", &
        logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "Start of ESMF_SCompEx Example"
        print *, "--------------------------------------- "
      endif

!BOE
!\begin{sloppypar}
!    Create the top 2 levels of the Component hierarchy.  This example creates
!    a parent Coupler Component and 2 Gridded Components as children.
!\end{sloppypar}
!EOE

!BOC
      ! Create top-level Coupler Component
      cplcomp = ESMF_CplCompCreate(name="coupler_component", rc=rc)

      ! Create Gridded Component for Atmosphere
      atmcomp = ESMF_GridCompCreate(name="Atmosphere", rc=rc)

      ! Create Gridded Component for Ocean
      ocncomp = ESMF_GridCompCreate(name="Ocean", rc=rc)

!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)



!BOE
!\begin{sloppypar}
!    Now add CIM Attribute packages to the Component.  Also, add
!    a CIM Component Properties package, to contain two custom attributes.
!\end{sloppypar}
!EOE

!BOC 
      convCIM = 'CIM 1.5'
      purpComp = 'ModelComp'
      purpProp = 'CompProp'
      purpField = 'Inputs'
      purpPlatform = 'Platform'

      convISO = 'ISO 19115'
      purpRP = 'RespParty'
      purpCitation = 'Citation'

      ! Add CIM Attribute package to the Science Component
      call ESMF_AttributeAdd(atmcomp, convention=convCIM, &
        purpose=purpComp, attpack=attpack, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\begin{sloppypar}
!    The Attribute package can also be retrieved in a multi-Component
!    setting like this:
!\end{sloppypar}
!EOE

!BOC 
      call ESMF_AttributeGetAttPack(atmcomp, convCIM, purpComp, &
                                    attpack=attpack, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      
!BOE
!\begin{sloppypar}
!     Now, add some CIM Component attributes to the Atmosphere Grid Component.
!\end{sloppypar}
!EOE

!BOC
      !
      ! Top-level model component attributes, set on gridded component
      !
      call ESMF_AttributeSet(atmcomp, 'ShortName', 'EarthSys_Atmos', &
        attpack=attpack, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(atmcomp, 'LongName', &
        'Earth System High Resolution Global Atmosphere Model', &
        attpack=attpack, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(atmcomp, 'Description', &
        'EarthSys brings together expertise from the global ' // &
        'community in a concerted effort to develop coupled ' // &
        'climate models with increased horizontal resolutions.  ' // &
        'Increasing the horizontal resolution of coupled climate ' // &
        'models will allow us to capture climate processes and ' // &
        'weather systems in much greater detail.', &
        attpack=attpack, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(atmcomp, 'Version', '2.0', &
        attpack=attpack, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(atmcomp, 'ReleaseDate', '2009-01-01T00:00:00Z', &
        attpack=attpack, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(atmcomp, 'ModelType', 'aerosol', &
        attpack=attpack, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

      call ESMF_AttributeSet(atmcomp, 'URL', &
        'www.earthsys.org', attpack=attpack, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\begin{sloppypar}
!    Now create a set of Science Components as a children of the Atmosphere 
!    Gridded Component. The hierarchy is as follows:
!    \begin{itemize}
!       \item Atmosphere
!       \begin{itemize}
!          \item AtmosDynamicalCore
!          \begin{itemize}
!             \item AtmosAdvection
!          \end{itemize}
!          \item AtmosRadiation
!       \end{itemize}
!    \end{itemize}
!    After each Component is created, we need to link it with its parent
!    Component.  We then add some standard CIM Component properties as well 
!    as Scientific Properties to each of these components.
!\end{sloppypar}
!EOE

!BOC
    !
    ! Atmosphere Dynamical Core Science Component
    !
    dc_scicomp = ESMF_SciCompCreate(name="AtmosDynamicalCore", rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!BOC
    call ESMF_AttributeAdd(dc_scicomp,  &
                           convention=convCIM, purpose=purpComp, &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeSet(dc_scicomp, "ShortName", "AtmosDynamicalCore", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(dc_scicomp, "LongName", &
                           "Atmosphere Dynamical Core", &
                           attpack=attpack, rc=rc)
!EOC

!BOC
    purpSci = 'SciProp'

    dc_sciPropAtt(1) = 'TopBoundaryCondition'
    dc_sciPropAtt(2) = 'HeatTreatmentAtTop'
    dc_sciPropAtt(3) = 'WindTreatmentAtTop'

    call ESMF_AttributeAdd(dc_scicomp,  &
                           convention=convCIM, purpose=purpSci, &
                           attrList=dc_sciPropAtt, &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeSet(dc_scicomp, 'TopBoundaryCondition', &
                           'radiation boundary condition', &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(dc_scicomp, 'HeatTreatmentAtTop', &
                           'some heat treatment', &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(dc_scicomp, 'WindTreatmentAtTop', &
                           'some wind treatment', &
                           attpack=attpack, rc=rc)
!EOC

!BOC
    !
    ! Atmosphere Advection Science Component
    !
    adv_scicomp = ESMF_SciCompCreate(name="AtmosAdvection", rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!BOC
    call ESMF_AttributeAdd(adv_scicomp,  &
                           convention=convCIM, purpose=purpComp, &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeSet(adv_scicomp, "ShortName", "AtmosAdvection", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(adv_scicomp, "LongName", "Atmosphere Advection", &
                           attpack=attpack, rc=rc)
!EOC

!BOC
    adv_sciPropAtt(1) = 'TracersSchemeName'
    adv_sciPropAtt(2) = 'TracersSchemeCharacteristics'
    adv_sciPropAtt(3) = 'MomentumSchemeName'

    call ESMF_AttributeAdd(adv_scicomp,  &
                           convention=convCIM, purpose=purpSci, &
                           attrList=adv_sciPropAtt, &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeSet(adv_scicomp, 'TracersSchemeName', 'Prather', &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(adv_scicomp, 'TracersSchemeCharacteristics', &
                           'modified Euler', &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(adv_scicomp, 'MomentumSchemeName', 'Van Leer', &
                           attpack=attpack, rc=rc)
!EOC

!BOC
    !
    ! Atmosphere Radiation Science Component
    !
    rad_scicomp = ESMF_SciCompCreate(name="AtmosRadiation", rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

!BOC
    call ESMF_AttributeAdd(rad_scicomp,  &
                           convention=convCIM, purpose=purpComp, &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeSet(rad_scicomp, "ShortName", "AtmosRadiation", &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(rad_scicomp, "LongName", &
                           "Atmosphere Radiation", &
                           attpack=attpack, rc=rc)
!EOC

!BOC
    rad_sciPropAtt(1) = 'LongwaveSchemeType'
    rad_sciPropAtt(2) = 'LongwaveSchemeMethod'

    call ESMF_AttributeAdd(rad_scicomp,  &
                           convention=convCIM, purpose=purpSci, &
                           attrList=rad_sciPropAtt, &
                           attpack=attpack, rc=rc)

    call ESMF_AttributeSet(rad_scicomp, &
                           'LongwaveSchemeType', &
                           'wide-band model', &
                           attpack=attpack, rc=rc)
    call ESMF_AttributeSet(rad_scicomp, &
                           'LongwaveSchemeMethod', &
                           'two-stream', &
                           attpack=attpack, rc=rc)
!EOC


!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\begin{sloppypar}
!     Finally, destroy all of the Components.
!\end{sloppypar}
!EOE

!BOC
      call ESMF_SciCompDestroy(rad_scicomp, rc=rc)
      call ESMF_SciCompDestroy(adv_scicomp, rc=rc)
      call ESMF_SciCompDestroy(dc_scicomp, rc=rc)
      call ESMF_GridCompDestroy(atmcomp, rc=rc)
      call ESMF_GridCompDestroy(ocncomp, rc=rc)
      call ESMF_CplCompDestroy(cplcomp, rc=rc)
!EOC
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


      if (localPet==0) then
        print *, "--------------------------------------- "
        print *, "End of ESMF_SCompEx Example"
        print *, "--------------------------------------- "
      endif


      ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors 
      ! in the log file that the scripts grep for.
      call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, &
          ESMF_SRCLINE)


      call ESMF_Finalize(rc=rc)

      if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
      if (finalrc==ESMF_SUCCESS) then
        print *, "PASS: ESMF_SCompEx.F90"
      else
        print *, "FAIL: ESMF_SCompEx.F90"
      endif

  
end program

