! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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

!------------------------------------------------------------------------------
!BOE
!
! \subsubsection{VM and Components}
!
! The following example shows the role that the VM plays in connection with ESMF 
! Components. A single Component is created in the main program. Through the
! optional {\tt petList} argument the driver code specifies that only resources
! associated with PET 0 are given to the {\tt gcomp} object. 
!
! When the Component code is invoked through the standard ESMF Component methods
! Initialize, Run, or Finalize the Component's VM is automatically entered.
! Inside of the user-written Component code the Component VM can be obtained
! by querying the Component object. The VM object will indicate that only a
! single PET is executing the Component code.
!
!EOE
!------------------------------------------------------------------------------

!BOC
module ESMF_VMComponentEx_gcomp_mod
!EOC

  ! modules
  use ESMF
  
  implicit none
  
  ! module variables
  private
  
  ! module procedures
  
  public mygcomp_register
    
  contains !--------------------------------------------------------------------

  subroutine mygcomp_register(gcomp, rc)
    ! arguments
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    
    ! register INIT method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, mygcomp_init, rc=rc)
    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, mygcomp_run, rc=rc)
    ! register FINAL method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, mygcomp_final, rc=rc)
  end subroutine !--------------------------------------------------------------
  
!BOC
  recursive subroutine mygcomp_init(gcomp, istate, estate, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: istate, estate
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    type(ESMF_VM):: vm
    
    ! get this Component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)
    
    ! the VM object contains information about the execution environment of
    ! the Component

    call ESMF_VMPrint(vm, rc=rc)
    
    rc = 0
  end subroutine !--------------------------------------------------------------

  
  recursive subroutine mygcomp_run(gcomp, istate, estate, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: istate, estate
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    type(ESMF_VM):: vm
    
    ! get this Component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)
    
    ! the VM object contains information about the execution environment of
    ! the Component

    call ESMF_VMPrint(vm, rc=rc)
    
    rc = 0
  end subroutine !--------------------------------------------------------------

  recursive subroutine mygcomp_final(gcomp, istate, estate, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: istate, estate
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables
    type(ESMF_VM):: vm
    
    ! get this Component's vm    
    call ESMF_GridCompGet(gcomp, vm=vm)
    
    ! the VM object contains information about the execution environment of
    ! the Component

    call ESMF_VMPrint(vm, rc=rc)
    
    rc = 0
  end subroutine !--------------------------------------------------------------

end module
!EOC


!BOC
program ESMF_VMComponentEx
#include "ESMF.h"
  use ESMF
  use ESMF_TestMod
  use ESMF_VMComponentEx_gcomp_mod
  implicit none
  
  ! local variables
!EOC  
  integer:: rc
  type(ESMF_GridComp):: gcomp
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_VMComponentEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS

  call ESMF_Initialize(defaultlogfilename="VMComponentEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC  
  gcomp = ESMF_GridCompCreate(petList=(/0/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC  
  call ESMF_GridCompSetServices(gcomp, userRoutine=mygcomp_register, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC  
  call ESMF_GridCompInitialize(gcomp, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC  
  call ESMF_GridCompRun(gcomp, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC  
  call ESMF_GridCompFinalize(gcomp, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC  
  call ESMF_GridCompDestroy(gcomp, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

!BOC  
  call ESMF_Finalize(rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMComponentEx.F90"
  else
    print *, "FAIL: ESMF_VMComponentEx.F90"
  endif
  
!BOC  
end program
!EOC
