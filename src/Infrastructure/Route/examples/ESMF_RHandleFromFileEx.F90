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

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_RHandleFromFileEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer                 :: rc
  type(ESMF_VM)           :: vm
  integer                 :: petCount
  type(ESMF_Grid)         :: gridA, gridB
  type(ESMF_Field)        :: fieldA, fieldB
  type(ESMF_RouteHandle)  :: rh1, rh2
  
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg
  
  
  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_RHandleFromFileEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(vm=vm, defaultlogfilename="RHandleFromFileEx.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! \subsubsection{Write a RouteHandle to file and creating a RouteHandle from file}
! \label{RH:RHfromFile}
!
! Communication Store() methods, like {\tt ESMF\_FieldRegridStore()}, are used
! to create RouteHandles. These methods can be expensive, both with respect to
! temporary memory requirements as well as the time they require to execute.
! Often the associated cost is acceptable because Store() calls are typically
! used during the initialization phase of the application. The cost of 
! RouteHandle generation is therefore armorized over the entire run phase
! of the application, where the RouteHandle is applied over and over to 
! transfer data according to the same communication pattern.
!
! However, especially for short production runs, an expensive initialization
! time can become problematic. In such cases it is useful to write the
! RouteHandle to file. Subsequent application runs can then re-create the
! RouteHandle during initialization, simply from file at a fraction of the time
! of the original Store() call.
!EOE

  gridA = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/360, 160/), &
    minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
    maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
    staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
    regDecomp=(/petCount,1/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fieldA = ESMF_FieldCreate(gridA, ESMF_TYPEKIND_R8, name="fieldA", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  gridB = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/360, 160/), &
    minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
    maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
    staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
    regDecomp=(/1,petCount/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fieldB = ESMF_FieldCreate(gridB, ESMF_TYPEKIND_R8, name="fieldB", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!BOE
! First a RouteHandle must be created using one of the ESMF Store() methods.
!EOE
!BOC
  call ESMF_FieldRedistStore(srcField=fieldA, dstField=fieldB, &
    routehandle=rh1, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Now the RouteHandle object {\tt rh1} can be written to file using the
! {\em collective} {\tt ESMF\_RouteHandleWrite()} method.
!EOE
!BOC
  call ESMF_RouteHandleWrite(rh1, fileName="testWrite.RH", rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! This creates a single binary file with name {\tt testWrite.RH}. The 
! information from across all PETs that define {\tt rh1} is contained in this
! file.
!
! At this point, the original RouteHandle is no longer needed and can be
! destroyed.
!EOE
!BOC
  call ESMF_RouteHandleDestroy(rh1, noGarbage=.true., rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The RouteHandle just deleted can easily be re-created using the
! {\tt ESMF\_RouteHandleCreate()} method that accepts the file name as an 
! argument. This is a {\em collective} method that must be called on exactly
! the same number of PETs that was used for the original Store() and Write()
! calls that generated the file.
!EOE
!BOC
  rh2 = ESMF_RouteHandleCreate(fileName="testWrite.RH", rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Finally the re-created RouteHandle, {\tt rh2}, can be used to execute the
! communication pattern originally computed in {\tt rh1}.
!EOE
!BOC
  call ESMF_FieldRedist(srcField=fieldA, dstField=fieldB, &
    routehandle=rh2, rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Once done with {\tt rh2}, the RouteHandle can be destroyed as usual.
!EOE
!BOC
  call ESMF_RouteHandleDestroy(rh2, noGarbage=.true., rc=rc)
!EOC
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)


10 continue

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_RHandleFromFileEx.F90"
  else
    print *, "FAIL: ESMF_RHandleFromFileEx.F90"
  endif

end program
