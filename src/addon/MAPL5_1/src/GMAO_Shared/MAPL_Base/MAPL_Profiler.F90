
!  $Id$ 

#include "MAPL_ErrLog.h"

!BOP

! !MODULE: MAPL_Profiler -- A Module to instrument codes for profiling


! !INTERFACE:

  module MAPL_ProfMod

! !USES:

  use ESMF
  use MAPL_BaseMod
  use MAPL_IOMod
  use MAPL_CommsMod
#ifdef _CUDA
  use cudafor
#endif

  implicit none
  private

! !PUBLIC TYPES:

  type, public :: MAPL_Prof
    private
    character(len=ESMF_MAXSTR) :: NAME=""
    integer        :: START_TIME
    real  (kind=8) :: CUMM_TIME   
  end type MAPL_Prof

! !PUBLIC MEMBER FUNCTIONS:

  public MAPL_ProfClockOn
  public MAPL_ProfClockOff
  public MAPL_ProfSet
  public MAPL_ProfDisable
  public MAPL_ProfEnable
  public MAPL_ProfWrite
  public MAPL_ProfIsDisabled
  public MAPL_TimerModeSet

!EOP

  integer, public, parameter  :: MAPL_TimerModeOld = 0
  integer, public, parameter  :: MAPL_TimerModeRootOnly = 1
  integer, public, parameter  :: MAPL_TimerModeMax = 2

  type(ESMF_VM), save :: VM
  integer,       save :: COUNT_MAX, COUNT_RATE
  real(kind=8),  save :: CRI
  logical,       save :: FIRSTTIME = .true.
  logical,       save :: DISABLED  = .false.
  integer,       save :: timerMode = MAPL_TimerModeMax

  contains

!********************************************************
    logical function MAPL_ProfIsDisabled()
      MAPL_ProfIsDisabled = DISABLED

    end function MAPL_ProfIsDisabled

!********************************************************

    subroutine MAPL_ProfClockOn(TIMES, NAME, RC)
      type (MAPL_Prof),  intent(INOUT) :: TIMES(:)
      character(len=*),  intent(IN   ) :: NAME      
      integer, optional, intent(OUT  ) :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfClockOn"

      integer :: I, NN
      integer :: status

      if(.not.DISABLED) then

         NN = size(TIMES)

         I=1
         do while (I<=NN)
            if(TIMES(I)%NAME==NAME) then
               exit
            endif
            I=I+1
         enddo

         if(I>NN) then
            print *, 'ERROR: Timer '//trim(NAME)//' needs to be set first'
            RETURN_(ESMF_FAILURE)
         end if
     
#ifdef _CUDA
         status = cudaDeviceSynchronize()
#endif
         if (timerMode == MAPL_TimerModeOld) then
            call ESMF_VMBarrier(VM, rc=status)
         end if
         call SYSTEM_CLOCK(TIMES(I)%START_TIME)  

      end if

      RETURN_(ESMF_SUCCESS)
    
    end subroutine MAPL_ProfClockOn

!********************************************************

    subroutine MAPL_ProfClockOff(TIMES, NAME, RC)
      type (MAPL_Prof),  intent(INOUT) :: TIMES(:)
      character(len=*),  intent(IN   ) :: NAME
      integer, optional, intent(OUT  ) :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfClockOff"

      integer :: COUNTS
      integer :: I, NN
      integer :: status

      if(.not.DISABLED) then

         NN = size(TIMES)

         I=1
         do while (I<=NN)
            if(TIMES(I)%NAME==NAME) then
               exit
            endif
            I=I+1
         enddo

         if(I>NN) then
            print *, 'ERROR: Timer '//trim(NAME)//' needs to be set first'
            RETURN_(ESMF_FAILURE)
         end if

#ifdef _CUDA
         status = cudaDeviceSynchronize()
#endif
         if (timerMode == MAPL_TimerModeOld) then
            call ESMF_VMBarrier(VM, rc=status)
         end if
         call SYSTEM_CLOCK(COUNTS)

         COUNTS = COUNTS-TIMES(I)%START_TIME

         if(COUNTS<0) then
            COUNTS = COUNTS + COUNT_MAX
         endif

         TIMES(I)%CUMM_TIME = TIMES(I)%CUMM_TIME + real(COUNTS,kind=8)*CRI

      end if

      RETURN_(ESMF_SUCCESS)

      
    end subroutine MAPL_ProfClockOff

!********************************************************

    subroutine MAPL_ProfSet(TIMES, NAME, RC)
      type (MAPL_Prof),  pointer       :: TIMES(:)
      character(len=*),  intent(IN   ) :: NAME
      integer, optional, intent(OUT  ) :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfSet"
      type (MAPL_Prof), pointer :: TMP(:)
      integer :: I, STATUS

      if (FIRSTTIME) then
         FIRSTTIME = .false.
         call ESMF_VMGetGlobal(VM, rc=STATUS)
         VERIFY_(STATUS)
         call SYSTEM_CLOCK(COUNT_RATE=COUNT_RATE,COUNT_MAX=COUNT_MAX)
         CRI = 1._8/real(COUNT_RATE,kind=8)
      end if

      if (.not.associated(TIMES)) then
         I = 0
      else
         I = size(TIMES)
      endif

      allocate(TMP(I+1))
      if(associated(TIMES)) then
         TMP(1:I) = TIMES
         deallocate(TIMES)
      endif
      TMP(I+1) = MAPL_Prof(trim(NAME),0,0.D0)
      TIMES => TMP

      RETURN_(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfSet

!********************************************************

    subroutine MAPL_ProfWrite(TIMES, RC)
      type (MAPL_Prof),  pointer       :: TIMES(:)
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfWrite"
      integer :: status

      integer :: I
      integer :: N
      logical :: amIroot
      logical :: writing
      real(kind=8), allocatable :: MAX_CUMM_TIME(:)  

      amIroot = MAPL_AM_I_Root(vm)

!ALT: Currently, only root PE writes the Prof report
!     If we adopt other modes, we need to change next line
      writing = amIroot

      if (associated(TIMES)) then
         if (timerMode == MAPL_TimerModeMax) then
            if(amIroot) then
               N=size(TIMES)
            else
               N=0
            end if
            allocate(MAX_CUMM_TIME(N), stat=status)
            VERIFY_(status)

            call ESMF_VmReduce(vm, sendData=TIMES(:)%CUMM_TIME, &
                 recvData=max_cumm_time, count=size(TIMES), &
                 reduceFlag=ESMF_Reduce_Max, RootPet=MAPL_Root, RC=status)
            VERIFY_(STATUS)
         end if

         if (writing) then
            ! We do the loop twice to make sure TOTAL is reported first
            do I=1,size(TIMES)
               if(trim(TIMES(I)%name)=='TOTAL') then
                  WRITE(*,'("'//TIMES(I)%NAME(1:24)//':",F12.3)') TIMES(I)%CUMM_TIME
                  exit
               end if
            enddo
            do I=1,size(TIMES)
               if(trim(TIMES(I)%name)=='TOTAL') then
                  cycle
               else
                  WRITE(*,'("'//TIMES(I)%NAME(1:24)//':",F12.3)') TIMES(I)%CUMM_TIME
               end if
            enddo
         end if ! writing

         if (timerMode == MAPL_TimerModeMax) then
            ! Restore cummulative time on root
            if (amIroot) then
               do I=1,size(TIMES)
                  TIMES(I)%CUMM_TIME = MAX_CUMM_TIME(I)
               end do
            end if
            deallocate(MAX_CUMM_TIME)
         end if
      end if

      RETURN_(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfWrite

!********************************************************

    subroutine MAPL_ProfDisable(RC)
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfDisable"

      DISABLED = .true.

      RETURN_(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfDisable

!********************************************************

    subroutine MAPL_ProfEnable(RC)
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfEnable"

      DISABLED = .false.

      RETURN_(ESMF_SUCCESS)
      
    end subroutine MAPL_ProfEnable

!********************************************************

    subroutine MAPL_TimerModeSet(MODE, RC)
      integer,           intent(IN )   :: MODE
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ProfModeSet"

      ! Sanity check
      ASSERT_(timerMode >= MAPL_TimerModeOld .and. timerMode <= MAPL_TimerModeMax)
      timerMode = mode

      RETURN_(ESMF_SUCCESS)
      
    end subroutine MAPL_TimerModeSet

!********************************************************


  end module MAPL_ProfMod

