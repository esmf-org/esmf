! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
    program ESMF_StateEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
#include "ESMF.h"

    ! ESMF Framework module
    use ESMF
    use ESMF_TestMod
    implicit none

    ! Local variables
    integer                 :: rc
    type(ESMF_State)        :: state
    type(ESMF_FieldBundle)  :: bundle
    type(ESMF_Field)        :: field, field1, field2
    integer                 :: finalrc
    integer                 :: result = 0     ! all pass
    character(ESMF_MAXSTR)  :: testname
    character(ESMF_MAXSTR)  :: failMsg

    finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_StateEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    call ESMF_Initialize(defaultlogfilename="StateEx.Log", &
      logkindflag=ESMF_LOGKIND_MULTI, rc=rc)

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Create a State and add items}
!
!  Creation of an empty {\tt ESMF\_State}. Then adding an {\tt ESMF\_FieldBundle}
!  to it.  Note that the {\tt ESMF\_FieldBundle} is empty.
!  The {\tt ESMF\_State} only contains a reference to the objects it contains.
!  It does not make a copy; the original objects can be updated and code 
!  accessing them by using the {\tt ESMF\_State} will see the updated version.
!EOE
!BOC
    state = ESMF_StateCreate(name="Ocean", &
      stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    bundle = ESMF_FieldBundleCreate(name="Surface Fields", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    call ESMF_StateAdd(state, [bundle], rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Query a State for items and add more items}
!
! The objects contained in a State can be queried by name.
!EOE
!BOC
    call ESMF_StateGet(state, itemName="Surface Fields", fieldbundle=bundle, &
      rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOE
! More objects can be created and added to the State. Here an empty Field is
! created and added to the State.
!EOE
!BOC
    field = ESMF_FieldEmptyCreate(name="MyField", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    call ESMF_StateAdd(state, [field], rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOE
! Multiple objects of the same type can be added to the State at the same time.
!EOE
!BOC
    field1 = ESMF_FieldEmptyCreate(name="field1", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    field2 = ESMF_FieldEmptyCreate(name="field2", rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    call ESMF_StateAdd(state, [field1, field2], rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Removing items from a State}
!
! Objects contained in a State can be removed using the item name.
!EOE
!BOC
    call ESMF_StateRemove(state, ["field1"], rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOE
! Notice that objects removed from a State are {\em not} destroyed by the
! {\tt ESMF\_StateRemove()} call. They must be destroyed explicitly when no
! longer needed.
!BOC
    call ESMF_FieldDestroy(field1, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Destroy a State}
!
! Once an {\tt ESMF\_State} is not longer needed, it should be destroyed.
!EOE
!BOC
    call ESMF_StateDestroy(state, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOE
! Notice that objects contained in a State are {\em not} destroyed by the
! {\tt ESMF\_StateDestroy()} call. They must be destroyed explicitly when no
! longer needed.
!BOC
    call ESMF_FieldBundleDestroy(bundle, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    call ESMF_FieldDestroy(field, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
    call ESMF_FieldDestroy(field2, rc=rc)
!EOC
    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!===============================================================================
    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

    call ESMF_Finalize(rc=rc)

    if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

    if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_StateEx.F90"
    else
        print *, "FAIL: ESMF_StateEx.F90"
    end if

    end program ESMF_StateEx
