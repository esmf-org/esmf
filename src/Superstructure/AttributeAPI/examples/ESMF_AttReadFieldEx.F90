! $Id: ESMF_AttReadFieldEx.F90,v 1.10 2010/09/13 05:50:47 eschwab Exp $
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
program ESMF_AttReadFieldEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Example: Reading an XML file-based CF Attribute Package for a Field}
! This example shows how to read a CF Attribute Package for a Field from an
! XML file.  The XML file contains Attribute values filled-in by the user.
! The standard CF Attribute Package is supplied with ESMF and is defined in
! an XSD file, which is used to validate the XML file.  See
! \begin{description}
! \item ESMF\_DIR/src/Infrastructure/Field/etc/esmf\_field.xml (Attribute Package values) and
! \item ESMF\_DIR/src/Infrastructure/Field/etc/esmf\_field.xsd (Attribute Package definition).
! \end{description}
!EOE

#include "ESMF.h"

!-----------------------------------------------------------------------------
! !PROGRAM: ESMF\_AttReadFieldEx - Example of reading an XML-file based CF
!   Attribute Package for a Field.
!
! !DESCRIPTION:
!
! This program shows an example of reading an XML-based file of an CF
! Attribute Package for a Field and placing it on an ESMF\_Field object.
!
!-----------------------------------------------------------------------------

!BOC
      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! local variables
      type(ESMF_Field)       :: field
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
      call ESMF_Initialize(vm=vm, defaultlogfilename="AttReadFieldEx.Log", &
                    defaultlogtype=ESMF_LOG_MULTI, rc=rc)
!EOC

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
      ! Create a field
      field = ESMF_FieldCreateEmpty(name="field", rc=rc)
!EOC

      if (rc.ne.ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
      ! Read an XML file to populate the CF Attribute package of a Field.
      ! The file is validated against an internal, ESMF-supplied XSD file
      ! defining the standard CF Attribute package (see file pathnames above).
      call ESMF_AttributeRead(field=field, fileName="esmf_field.xml", rc=rc)
!EOC
      if (rc==ESMF_RC_LIB_NOT_PRESENT) then
        xercesPresent = .false.
      endif

      if (rc .ne. ESMF_SUCCESS .and. xercesPresent) finalrc = ESMF_FAILURE

!print *, 'rc = ', rc

!BOC
      ! Get CF "VariableShortName" Attribute from a Field
      call ESMF_AttributeGet(field, name='VariableShortName', value=attrValue, &
                             convention='CF', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='DPEDT') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get CF "VariableStandardName" Attribute from a Field
      call ESMF_AttributeGet(field, name='VariableStandardName', &
                             value=attrValue, &
                             convention='CF', purpose='Extended', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. &
                 attrvalue=='tendency_of_air_pressure') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get CF "VariableLongName" Attribute from a Field
      call ESMF_AttributeGet(field, name='VariableLongName', value=attrValue, &
                             convention='CF', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Edge pressure tendency') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      ! Get CF "VariableUnits" Attribute from a Field
      call ESMF_AttributeGet(field, name='VariableUnits', value=attrValue, &
                             convention='CF', purpose='General', rc=rc)
!EOC

      if (.not.((rc==ESMF_SUCCESS .and. attrvalue=='Pa s-1') &
                      .or. .not. xercesPresent)) finalrc = ESMF_FAILURE
!print *, 'rc = ', rc
!print *, 'attrvalue = ', attrvalue

!BOC
      call ESMF_FieldDestroy(field, rc=rc)
!EOC

      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc=rc)
!EOC

      if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc .eq. ESMF_SUCCESS) then
         print *, "PASS: ESMF_AttReadFieldEx.F90"
      else
         print *, "FAIL: ESMF_AttReadFieldEx.F90"
      end if

end program ESMF_AttReadFieldEx
