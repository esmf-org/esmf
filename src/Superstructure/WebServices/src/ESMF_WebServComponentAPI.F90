! $Id: ESMF_WebServComponentAPI.F90,v 1.1 2010/07/27 05:52:46 theurich Exp $
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  Provides communication between processes to handle socket requests
!
!
!\begin{verbatim}


!-------------------------------------------------------------------------
!   !  The f_esmf_processinit routine ...
!   !   put more info here...

  subroutine f_esmf_processinit(comp, impstate, expstate, clock, phase, rc)
    use ESMF_Mod
    implicit none

    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: impstate
    type(ESMF_State) :: expstate
    type(ESMF_Clock) :: clock
    integer :: phase
    integer, intent(out) :: rc

    character :: proctype
    integer :: localrc

    proctype = 'I'
    call process_request(comp, impstate, expstate, clock, phase, proctype, rc)
    call ESMF_GridCompInitialize(comp, exportState=expState, rc=localrc)

  end subroutine


!-------------------------------------------------------------------------
!   !  The f_esmf_processrun routine ...
!   !   put more info here...

  subroutine f_esmf_processrun(comp, impstate, expstate, clock, phase, rc)
    use ESMF_Mod
    implicit none

    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: impstate
    type(ESMF_State) :: expstate
    type(ESMF_Clock) :: clock
    integer :: phase
    integer, intent(out) :: rc

    character :: proctype
    integer :: localrc

    proctype = 'R'
    call process_request(comp, impstate, expstate, clock, phase, proctype, rc)
    call ESMF_GridCompRun(comp, exportState=expState, rc=localrc)

  end subroutine


!-------------------------------------------------------------------------
!   !  The f_esmf_processfinal routine ...
!   !   put more info here...

  subroutine f_esmf_processfinal(comp, impstate, expstate, clock, phase, rc)
    use ESMF_Mod
    implicit none

    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: impstate
    type(ESMF_State) :: expstate
    type(ESMF_Clock) :: clock
    integer :: phase
    integer, intent(out) :: rc

    character :: proctype
    integer :: localrc

    proctype = 'F'
    call process_request(comp, impstate, expstate, clock, phase, proctype, rc)
    call ESMF_GridCompFinalize(comp, exportState=expState, rc=localrc)

  end subroutine


!-------------------------------------------------------------------------
!   !  The process_request routine ...
!   !   put more info here...
 
  subroutine process_request(comp, importState, exportState, clock, phase, procType, rc)
    use ESMF_Mod
    implicit none

    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState
    type(ESMF_State) :: exportState
    type(ESMF_Clock) :: clock
    integer :: phase
    character :: procType
    integer, intent(out) :: rc

    ! MPI stuff
    integer :: mpierr, rank, size, thread_cntr

    ! Initialize return code
    rc = ESMF_SUCCESS

    print *, "Processing request"

    call MPI_Comm_rank(0, rank, mpierr)

    if (rank == 0) then

       call MPI_Comm_size(0, size, mpierr)
       print *, "MPI Size: ", size

       do thread_cntr = 1, size - 1, 1

          print *, "In do loop: ", thread_cntr
          print *, "Before MPI Send: ", procType
          call MPI_Send(procType, 1, 5, thread_cntr, thread_cntr, 0, mpierr)
          print *, "After MPI Send: ", mpierr

       enddo

    endif

  end subroutine


!-------------------------------------------------------------------------
!   !  The wait_for_request routine ...
!   !   put more info here...

  subroutine wait_for_request(comp, exportState, rc)
    use ESMF_Mod
    implicit none

    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: exportState
    integer, intent(out) :: rc

    integer :: localrc

    ! mpi stuff
    integer :: mpierr, rank, size, count
    integer :: tag, status(5)
    character :: inmsg

    ! Initialize return code
    rc = ESMF_SUCCESS

    call MPI_Comm_rank(0, rank, mpierr)

    do
       print *, "Waiting for request: ", rank
       inmsg = 'A'
       call MPI_Recv(inmsg, 1, 5, 0, -1, 0, status, mpierr)
       call MPI_GET_COUNT(status, 5, count, mpierr)
       !print *, "    Buffer value: ", inmsg, " - ", rank
       print *, "Leaving MPI_Recv: ", rank

       if (inmsg == 'I') then

          print *, "Execute GridCompInitialize: ", rank
          call ESMF_GridCompInitialize(comp, exportState=exportState, rc=localrc)
          print *, "Done Execute GridCompInitialize: ", rank

       else if (inmsg == 'R') then

          print *, "Execute GridCompRun: ", rank
          call ESMF_GridCompRun(comp, exportState=exportState, rc=localrc)
          print *, "Done Execute GridCompRun: ", rank

       else if (inmsg == 'F') then

          print *, "Execute GridCompFinalize: ", rank
          call ESMF_GridCompFinalize(comp, exportState=exportState, rc=localrc)
          print *, "Done Execute GridCompFinalize: ", rank

       endif

    end do

  end subroutine

    
!\end{verbatim}
