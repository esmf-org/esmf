! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_AttReadCustCplCompEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Read and validate an XML file-based set of user-defined Attributes for a Coupler Component}
! This example shows how to read and validate, from an XML and XSD file,
! respectively, a set of user-defined custom Attributes for a Coupler Component.
! See
! \begin{description}
! \item ESMF\_DIR/src/Superstructure/Component/etc/custom\_cplcomp.xml (Attribute values) and 
! \item ESMF\_DIR/src/Superstructure/Component/etc/custom\_cplcomp.xsd (Attribute definitions)
! \end{description}
!EOE

#include "ESMF.h"

!-----------------------------------------------------------------------------
! !PROGRAM: ESMF\_AttReadCustCplCompEx - Example of reading and validating an XML-file based set of Custom Attributes for a Coupler Component.
!
! !DESCRIPTION:
!
! This program shows an example of reading and validating a set of 
! custom Attributes for a Coupler Component and placing them on an
! ESMF\_CplComp object.
!
!-----------------------------------------------------------------------------

!BOC
      ! ESMF Framework module
      use ESMF
      use ESMF_TestMod
      implicit none

      ! local variables
      type(ESMF_CplComp)     :: cplcomp
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
    write(testname, *) "Example ESMF_AttReadCustCplCompEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------



      ! assume Xerces XML C++ API library present until proven otherwise
      xercesPresent = .true.
      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF
      call ESMF_Initialize(vm=vm, &
                    defaultlogfilename="AttReadCustCplCompEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! get the vm
      call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
!EOC

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      if (localPet==0) then
        print *, "----------------------------------------"
        print *, "Start of ESMF_AttReadCustCplComp Example"
        print *, "----------------------------------------"
      endif

!BOC
      if (petCount<4) then
        cplcomp = ESMF_CplCompCreate(name="cplcomp", &
          petList=(/0/), rc=rc)
      else
        cplcomp = ESMF_CplCompCreate(name="cplcomp", &
          petList=(/0,1,2,3/), rc=rc)
      endif
!EOC

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! Read an XML file to decorate a Coupler Component with custom,
      ! user-defined attributes, and validate them against a corresponding
      ! XSD schema file (see file pathnames above).
      call ESMF_AttributeRead(comp=cplcomp, fileName="custom_cplcomp.xml", &
                              schemaFileName="custom_cplcomp.xsd", rc=rc)
!EOC
      if (rc==ESMF_RC_LIB_NOT_PRESENT) then
        xercesPresent = .false.
      endif

      if (rc .ne. ESMF_SUCCESS .and. xercesPresent) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!print *, 'rc = ', rc

!BOC
      ! Get custom "MyAttribute1" from CplComp
      call ESMF_AttributeGet(cplcomp, name='MyAttribute1', value=attrValue, &
           rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='My Coupler') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get custom "MyAttribute2" from CplComp
      call ESMF_AttributeGet(cplcomp, name='MyAttribute2', value=attrValue, &
           rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='My Earth System Model') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get custom "MyAttribute3" from CplComp
      call ESMF_AttributeGet(cplcomp, name='MyAttribute3', value=attrValue, &
           rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Atmosphere') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get custom "MyAttribute4" from CplComp
      call ESMF_AttributeGet(cplcomp, name='MyAttribute4', value=attrValue, &
           rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Land') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get custom "MyAttribute5" from CplComp
      call ESMF_AttributeGet(cplcomp, name='MyAttribute5', value=attrValue, &
           rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Version 1') &
                      .or. .not. xercesPresent)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      call ESMF_CplCompDestroy(cplcomp, rc=rc)
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
         print *, "PASS: ESMF_AttReadCustCplCompEx.F90"
      else
         print *, "FAIL: ESMF_AttReadCustCplCompEx.F90"
      end if

end program ESMF_AttReadCustCplCompEx
