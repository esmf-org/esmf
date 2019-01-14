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

!-------------------------------------------------------------------------
!BOP
!
!   When working with ESMF Internal States it is important to consider the
!   applying scoping rules. The user must ensure that the private data block
!   that is being referenced persists for the entire access period. This is
!   not an issue in the previous example, where the private data block was
!   defined on the scope of the main program. However, the Internal State 
!   construct is often useful inside of Component modules to hold Component
!   specific data between calls. One option to ensure persisting private data
!   blocks is to use the Fortran SAVE attribute either on local or module
!   variables. A second option, illustrated in the following example, is to 
!   use Fortran pointers and user controlled memory management via allocate()
!   and deallocate() calls.
!
!   One situation where the Internal State is useful is in the
!   creation of ensembles of the same Component. In this case it can 
!   be tricky to distinguish which data, held in saved module variables, 
!   belongs to which ensemble member - especially if the ensemble members
!   are executing on the same set of PETs. The Internal State solves this
!   problem by providing a handle to instance specific data allocations.
!
!EOP
!-------------------------------------------------------------------------


!BOC

module user_mod

  use ESMF

  implicit none
  
  ! module variables
  private

  ! Internal State Variables
  type testData
  sequence
    integer       :: testValue        ! scalar data
    real          :: testScaling      ! scalar data
    real, pointer :: testArray(:)     ! array data
  end type

  type dataWrapper
  sequence
    type(testData), pointer :: p
  end type

!EOC
  public mygcomp_register

!BOC
  contains !--------------------------------------------------------------
!EOC

  subroutine mygcomp_register(gcomp, rc)
    type(ESMF_GridComp):: gcomp
    integer, intent(out):: rc
    ! register INIT method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, mygcomp_init, rc=rc)
    ! register RUN method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, mygcomp_run, rc=rc)
    ! register FINAL method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, mygcomp_final, rc=rc)
  end subroutine !---------------------------------------------------------
  

!BOC

  subroutine mygcomp_init(gcomp, istate, estate, clock, rc)
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Local variables
    type(dataWrapper) :: wrap
    type(testData), pointer :: data
    integer :: i

    rc = ESMF_SUCCESS
    
    ! Allocate private data block
    allocate(data)

    ! Initialize private data block
    data%testValue = 4567         ! initialize scalar data
    data%testScaling = 0.5        ! initialize scalar data
    allocate(data%testArray(10))  ! allocate array data
    
    do i=1, 10
      data%testArray(i) = real(i) ! initialize array data
    enddo
    
    ! In a real ensemble application the initial data would be set to 
    ! something unique for this ensemble member. This could be 
    ! accomplished for example by reading a member specific config file 
    ! that was specified by the driver code. Alternatively, Attributes, 
    ! set by the driver, could be used to label the Component instances 
    ! as specific ensemble members.
    
    ! Set Internal State
    wrap%p => data
    call ESMF_GridCompSetInternalState(gcomp, wrap, rc)

  end subroutine !-------------------------------------------------------
  
  subroutine mygcomp_run(gcomp, istate, estate, clock, rc)
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Local variables
    type(dataWrapper) :: wrap
    type(testData), pointer :: data
    logical :: match = .true.
    integer :: i
    
    rc = ESMF_SUCCESS

    ! Get Internal State
    call ESMF_GridCompGetInternalState(gcomp, wrap, rc)
    if (rc/=ESMF_SUCCESS) return

    ! Access private data block and verify data
    data => wrap%p 
    if (data%testValue .ne. 4567) match = .false.   ! test scalar data
    if (data%testScaling .ne. 0.5) match = .false.  ! test scalar data
    do i=1, 10
      if (data%testArray(i) .ne. real(i)) match = .false. ! test array data
    enddo
    
    if (match) then
      print *, "got same values back from GetInternalState as original"
    else
      print *, "did not get same values back"
      rc = ESMF_FAILURE
    endif
    
  end subroutine !-------------------------------------------------------

  subroutine mygcomp_final(gcomp, istate, estate, clock, rc)
    type(ESMF_GridComp):: gcomp
    type(ESMF_State):: istate, estate
    type(ESMF_Clock):: clock
    integer, intent(out):: rc

    ! Local variables
    type(dataWrapper) :: wrap
    type(testData), pointer :: data

    rc = ESMF_SUCCESS
    
    ! Get Internal State
    call ESMF_GridCompGetInternalState(gcomp, wrap, rc)
    if (rc/=ESMF_SUCCESS) return
    
    ! Deallocate private data block
    data => wrap%p 
    deallocate(data%testArray)  ! deallocate array data
    deallocate(data)
    
  end subroutine !--------------------------------------------------------------


end module

!EOC

program ESMF_InternalStateModEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  use user_mod
  implicit none
  
  type(ESMF_GridComp) :: comp1
  integer :: rc, finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_InternalStateModEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS
      
  call ESMF_Initialize(defaultlogfilename="InternalStateModEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

  comp1 = ESMF_GridCompCreate(name="test", rc=rc)  
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

  call ESMF_GridCompSetServices(comp1, userRoutine=mygcomp_register, rc=rc)  
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

  call ESMF_GridCompInitialize(comp1, rc=rc)  
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

  call ESMF_GridCompRun(comp1, rc=rc)  
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

  call ESMF_GridCompFinalize(comp1, rc=rc)  
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

  call ESMF_GridCompDestroy(comp1, rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  if (rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE 

  if (finalrc .eq. ESMF_SUCCESS) then
    print *, "PASS: ESMF_InternalStateEx.F90"
  else 
    print *, "FAIL: ESMF_InternalStateEx.F90"
  end if

end program ESMF_InternalStateModEx
    
