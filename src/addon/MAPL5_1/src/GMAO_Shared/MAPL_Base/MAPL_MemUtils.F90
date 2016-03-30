#include "MAPL_ErrLog.h"

!BOP

! !MODULE: MAPL_MemUtilsMod -- A Module to query/print memory use per processor (Adapted by WMP from FMS memuse utility)


! !INTERFACE:

module MAPL_MemUtilsMod

  use ESMF
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_IOMod

!Author: Balaji (V.Balaji@noaa.gov)
!Various operations for memory management
!these currently include efficient methods for memory-to-memory copy
!including strided data and arbitrary gather-scatter vectors
!also various memory and cache inquiry operators
  implicit none
  private
#ifdef _CRAYT3E
  integer :: pe, shmem_my_pe
#endif

  integer(kind=8) :: l1_cache_line_size, l1_cache_size, l1_associativity
  integer(kind=8) :: l2_cache_line_size, l2_cache_size, l2_associativity

  logical :: memutils_initialized=.FALSE.

  interface memcpy
     module procedure memcpy_r8
     module procedure memcpy_r8_gather
     module procedure memcpy_r8_scatter
     module procedure memcpy_r8_gather_scatter
  end interface

  public MAPL_MemUtilsInit
  public MAPL_MemUtilsDisable
  public MAPL_MemUtilsWrite
  public MAPL_MemUtilsIsDisabled
  public MAPL_MemUtilsFree

#ifdef _CRAY
  public :: hplen
#endif
#ifdef _CRAYT90
  public :: stklen
#endif
  logical,    save :: DISABLED  = .false.

  real, save :: gmax_save

  contains

!********************************************************
    logical function MAPL_MemUtilsIsDisabled()
      MAPL_MemUtilsIsDisabled = DISABLED

    end function MAPL_MemUtilsIsDisabled

!********************************************************
         
    subroutine MAPL_MemUtilsDisable(RC)
      integer, optional, intent(OUT)   :: RC
         
      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_MemUtilsDisable"
               
      DISABLED = .true.
            
      RETURN_(ESMF_SUCCESS)

    end subroutine MAPL_MemUtilsDisable
            
!********************************************************

!********************************************************

    subroutine MAPL_MemUtilsInit(RC)
      integer, optional, intent(OUT)   :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_MemUtilsInit"
!initialize memutils module
!currently sets default cache characteristics
!(will provide overrides later)
!also sets pe to my_pe on t3e
#ifdef _CRAYT3E
!all sizes in bytes
      l1_cache_line_size = 32
      l1_cache_size = 8192
      l1_associativity = 1
      l2_cache_line_size = 64
      l2_cache_size = 98304
      l2_associativity = 3
#else
!defaults
      l1_cache_line_size = 1
      l1_cache_size = 1
      l1_associativity = 1
      l2_cache_line_size = 1
      l2_cache_size = 1
      l2_associativity = 1
#endif
#ifdef _CRAYT3E
      pe = SHMEM_MY_PE()
#endif
      memutils_initialized = .TRUE.
 
      gmax_save = 0.0
     
      RETURN_(ESMF_SUCCESS)
    end subroutine MAPL_MemUtilsInit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                      !
!MEMCPY routines: <nelems> real*8 words are copied from RHS to LHS     !
!  Either side can have constant stride (lhs_stride, rhs_stride)       !
!      or indexed by a gather/scatter array (lhs_indx, rhs_indx)       !
! index arrays are 0-based (i.e C-like not fortran-like: this is       !
! for compatibility with the SHMEM_IXGET/PUT routines)                 !
!                                                                      !
! EXAMPLES:                                                            !
!                                                                      !
!Replace                                                               !
!  a(0:n-1) = b(0:n-1)                                                 !
!with                                                                  !
!  call memcpy(a,b,n)                                                  !
!                                                                      !
!Replace                                                               !
!  a(0:2*n-1:2) = b(0:3*n-1:3)                                         !
!with                                                                  !
!  call memcpy(a,b,dim,n,2,3)    !dim.GE.3*n                           !
!                                                                      !
!Replace                                                               !
!  a(0:n-1) = b(indx(1:n))                                             !
!with                                                                  !
!  call memcpy(a,b,dim,n,1,indx) !dim.GE.max(indx)                     !
!                                                                      !
!Replace                                                               !
!  a(indx(1:n)) = b(0:n-1)                                             !
!with                                                                  !
!  call memcpy(a,b,dim,n,indx,1) !dim.GE.max(indx)                     !
!                                                                      !
!Replace                                                               !
!  a(indxa(1:n)) = b(indxb(1:n))                                       !
!with                                                                  !
!  call memcpy(a,b,dim,n,indx,indxb) !dim.GE.max(indxa,indxb)          !
!                                                                      !
!  There are no error checks!!! (routines are built for speed)         !
!  Specifically there is no bounds-checking: if the stride or          !
!  indexing causes you to exceed <dim> you will have done a            !
!  potentially unsafe memory load                                      !
!                                                                      !
!T3E: we use the shmem routines on-processor to effect the transfer    !
!     via the (faster) E-registers                                     !
!                                                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine memcpy_r8( lhs, rhs, dim, nelems, lhs_stride, rhs_stride )
!base routine: handles constant stride memcpy
!default strides are of course 1
      integer, intent(in) :: dim
      real(kind=8), dimension(0:dim-1), intent(in)  :: rhs
      real(kind=8), dimension(0:dim-1), intent(out) :: lhs
      integer, intent(in), optional :: nelems, lhs_stride, rhs_stride
      integer :: n, rs, ls

!defaults
      n = dim
      ls = 1
      rs = 1
      if( PRESENT(nelems) )then
          n = nelems
!only check for stride if nelems is present
          if( PRESENT(lhs_stride) )ls = lhs_stride
          if( PRESENT(rhs_stride) )rs = rhs_stride
      endif
      if( ls.EQ.1 .AND. rs.EQ.1 )then
#ifdef _CRAYT3E
          call SHMEM_GET( lhs(0), rhs(0), n, pe )
#else
          lhs(0:n-1) = rhs(0:n-1)
#endif
      else
#ifdef _CRAYT3E
          call SHMEM_IGET( lhs(0), rhs(0), ls, rs, n, pe )
#else
          lhs(0:n*ls-1:ls) = rhs(0:n*rs-1:rs)
#endif
      endif
      return
    end subroutine memcpy_r8

    subroutine memcpy_r8_gather( lhs, rhs, dim, nelems, lhs_stride, rhs_indx )
!memcpy routine with gather: copies nelems words from rhs(indx(:)) to lhs(:)
      integer, intent(in) :: dim, nelems, lhs_stride
      real(kind=8), dimension(0:dim-1), intent(in)  :: rhs
      real(kind=8), dimension(0:dim-1), intent(out) :: lhs
      integer, intent(in), dimension(nelems) :: rhs_indx
#ifdef _CRAYT3E
!dir$ CACHE_BYPASS lhs, rhs, rhs_indx
      real(kind=8), dimension(nelems) :: tmp

      if( lhs_stride.EQ.1 )then
          call SHMEM_IXGET( lhs(0), rhs(0), rhs_indx, nelems, pe )
      else
          call SHMEM_IXGET( tmp, rhs(0), rhs_indx, nelems, pe )
          call SHMEM_IGET( lhs(0), tmp, lhs_stride, 1, nelems, pe )
      endif
#else
      lhs(0:nelems*lhs_stride-1:lhs_stride) = rhs(rhs_indx(1:nelems))
#endif
      return
    end subroutine memcpy_r8_gather

    subroutine memcpy_r8_scatter( lhs, rhs, dim, nelems, lhs_indx, rhs_stride )
!memcpy routine with scatter: copies nelems words from rhs(:) to lhs(indx(:))
      integer, intent(in) :: dim, nelems, rhs_stride
      real(kind=8), dimension(0:dim-1), intent(in)  :: rhs
      real(kind=8), dimension(0:dim-1), intent(out) :: lhs
      integer, intent(in), dimension(nelems) :: lhs_indx
#ifdef _CRAYT3E
!dir$ CACHE_BYPASS lhs, rhs, lhs_indx
      real(kind=8), dimension(nelems) :: tmp

      if( rhs_stride.EQ.1 )then
          call SHMEM_IXPUT( lhs(0), rhs(0), lhs_indx, nelems, pe )
      else
          call SHMEM_IGET( tmp, rhs(0), rhs_stride, 1, nelems, pe )
          call SHMEM_IXPUT( lhs(0), tmp, lhs_indx, nelems, pe )
      endif
      call SHMEM_QUIET          !required to ensure completion of put
#else
      lhs(lhs_indx(1:nelems)) = rhs(0:nelems*rhs_stride-1:rhs_stride)
#endif
      return
    end subroutine memcpy_r8_scatter

    subroutine memcpy_r8_gather_scatter( lhs, rhs, dim, nelems, lhs_indx, rhs_indx )
!memcpy routine with gather/scatter: copies nelems words from rhs(indx(:)) to lhs(indx(:))
      integer, intent(in) :: dim, nelems
      real(kind=8), dimension(0:dim-1), intent(in)  :: rhs
      real(kind=8), dimension(0:dim-1), intent(out) :: lhs
      integer, intent(in), dimension(nelems) :: lhs_indx, rhs_indx
#ifdef _CRAYT3E
!dir$ CACHE_BYPASS lhs, rhs, lhs_indx, rhs_indx
      real(kind=8), dimension(nelems) :: tmp

      call SHMEM_IXGET( tmp, rhs(0), rhs_indx, nelems, pe )
      call SHMEM_IXPUT( lhs(0), tmp, lhs_indx, nelems, pe )
      call SHMEM_QUIET          !required to ensure completion of put
#else
      lhs(lhs_indx(1:nelems)) = rhs(rhs_indx(1:nelems))
#endif
      return
    end subroutine memcpy_r8_gather_scatter

#ifdef _CRAY
  integer function hplen(             hpalloc, hplargest, hpshrink, hpgrow, hpfirst, hplast )
!using IHPSTAT calls from SR-2165 v2.0 p535
!with no arguments returns heap length (in words on PVP, bytes on t3e)
    integer, intent(out), optional :: hpalloc, hplargest, hpshrink, hpgrow, hpfirst, hplast
    integer :: IHPSTAT

    hplen = IHPSTAT(1)	                      !Heap length
    if( present(hpalloc  ) )hpalloc   = IHPSTAT( 4) !Blocks allocated
    if( present(hplargest) )hplargest = IHPSTAT(10) !Largest free block size
    if( present(hpshrink ) )hpshrink  = IHPSTAT(11) !Amount heap can shrink
    if( present(hpgrow   ) )hpgrow    = IHPSTAT(12) !Amount heap can grow
    if( present(hpfirst  ) )hpfirst   = IHPSTAT(13) !First word address
    if( present(hplast   ) )hplast    = IHPSTAT(14) !Last word address
    return
  end function hplen
#endif /* _CRAY */

#ifdef _CRAYT90
  integer function stklen(            stkhiwm, stknumber, stktotal, stkmost, stkgrew, stkgtimes )
!using STKSTAT(3C) struct
    integer, optional, intent(out) :: stkhiwm, stknumber, stktotal, stkmost, stkgrew, stkgtimes
    integer :: istat(20)

    call STKSTAT(istat)
    stklen = istat(1)	!Stack length
    if( present(stkhiwm  ) )stkhiwm   = istat(2) !stack hiwatermark
    if( present(stknumber) )stknumber = istat(3) !current #stacks
    if( present(stktotal ) )stktotal  = istat(4) !total #stacks
    if( present(stkmost  ) )stkmost   = istat(5) !most #stacks at one time
    if( present(stkgrew  ) )stkgrew   = istat(6) !#stacks that grew
    if( present(stkgtimes) )stkgtimes = istat(7) !#times stack grew
    return
  end function stklen
#endif /* _CRAYT90 */

!cache utilities: need to write version for other argument types
  function get_l1_cache_line(a)
    integer(kind=8) :: get_l1_cache_line
    real, intent(in) :: a
    integer(kind=8) :: i
    i = LOC(a)
    get_l1_cache_line = mod(i,l1_cache_size/l1_associativity)/l1_cache_line_size
  end function get_l1_cache_line

  function get_l2_cache_line(a)
    integer(kind=8) :: get_l2_cache_line
    real, intent(in) :: a
    integer(kind=8) :: i
    i = LOC(a)
    get_l2_cache_line = mod(i,l2_cache_size/l2_associativity)/l2_cache_line_size
  end function get_l2_cache_line

  subroutine MAPL_MemUtilsWrite( vm, text, always, RC )
    type(ESMF_VM) :: vm
    character(len=*), intent(in) :: text
    logical, intent(in), optional :: always
    integer, optional, intent(OUT  ) :: RC

    real :: mhwm, mrss, memused, swapused
    real :: mmin, mmax, mavg, mstd
    real :: gmin, gmax, gavg, gstd
    real :: lhwm, ghwm
    real :: lmem, gmem, lswap, gswap

!memuse is an external function: works on SGI
!use #ifdef to generate equivalent on other platforms.
    integer :: memuse !default integer OK?

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_MemUtilsWrite"
    integer :: status

    character(len=ESMF_MAXSTR) :: outString

    if( PRESENT(always) )then
        if( .NOT.always ) then
            RETURN_(ESMF_SUCCESS)
        endif
    else
        if( DISABLED ) then
            RETURN_(ESMF_SUCCESS)
        endif
    end if
#if defined(__sgi) || defined(__aix) || defined(__SX)
    m = memuse()*1e-3
#else
    call mem_dump(mhwm, mrss, memused, swapused)
#endif 
    lhwm = mhwm; call MAPL_CommsAllReduceMax(vm, lhwm, ghwm, 1, rc=status)
    VERIFY_(STATUS)
    mmin = mrss; call MAPL_CommsAllReduceMin(vm, mmin, gmin, 1, rc=status)
    VERIFY_(STATUS)
    mmax = mrss; call MAPL_CommsAllReduceMax(vm, mmax, gmax, 1, rc=status)
    VERIFY_(STATUS)
    mavg = mrss; call MAPL_CommsAllReduceSum(vm, mavg, gavg, 1, rc=status)
    VERIFY_(STATUS)
    gavg = gavg/MAPL_NPES(vm)
    mstd = (mrss-gavg)**2; call MAPL_CommsAllReduceSum(vm, mstd, gstd, 1, rc=status)
    gstd = sqrt( gstd/MAPL_NPES(vm) )
 !  write(outString,'(a64,5es11.3)') &
 !       'Memuse(MB) at '//trim(text)//'=', gmin, gmax, gstd, gavg, gmax-gmax_save
    write(outString,'(a64,5es11.3)') &
         'Memuse(MB) at '//trim(text)//'=', ghwm, gmax, gmin, gavg, gmax-gmax_save
    gmax_save = gmax
    call WRITE_PARALLEL(trim(outString),format='(a132)')

    lmem  = memused; call MAPL_CommsAllReduceMax(vm, lmem, gmem, 1, rc=status)
    VERIFY_(STATUS)
    lswap = swapused; call MAPL_CommsAllReduceMax(vm, lswap, gswap, 1, rc=status)
    VERIFY_(STATUS)
    write(outString,'(a64,2es11.3)') &
         'Mem/Swap Used (MB) at '//trim(text)//'=', gmem, gswap
    call WRITE_PARALLEL(trim(outString),format='(a132)')

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_MemUtilsWrite

!#######################################################################

subroutine mem_dump ( memhwm, memrss, memused, swapused, RC )

real, intent(out) :: memhwm, memrss, memused, swapused
integer, optional, intent(OUT  ) :: RC

! This routine returns the memory usage on Linux systems.
! It does this by querying a system file (file_name below).

character(len=32) :: proc_self = '/proc/self/status'
character(len=32) :: meminfo   = '/proc/meminfo'
character(len=32) :: string
integer :: memtot, memfree, swaptot, swapfree
integer :: mem_unit
real    :: multiplier

character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_MemUtils:mem_dump"
integer :: status

  memhwm = 0.0
  memrss = 0.0
  multiplier = 1.0

  call get_unit(mem_unit)
  open(UNIT=mem_unit,FILE=proc_self,FORM='formatted',IOSTAT=STATUS)
  VERIFY_(STATUS)
  do; read (mem_unit,'(a)', end=10) string
    if ( INDEX ( string, 'VmHWM:' ) == 1 ) then  ! High Water Mark
      read (string(7:LEN_TRIM(string)-2),*) memhwm
      if (TRIM(string(LEN_TRIM(string)-1:)) == "kB" ) &
        multiplier = 1.0/1024. ! Convert from kB to MB
      memhwm = memhwm * multiplier
    endif
    if ( INDEX ( string, 'VmRSS:' ) == 1 ) then  ! Resident Memory
      read (string(7:LEN_TRIM(string)-2),*) memrss
      if (TRIM(string(LEN_TRIM(string)-1:)) == "kB" ) &
        multiplier = 1.0/1024. ! Convert from kB to MB
      memrss = memrss * multiplier
    endif
  enddo
10 close(mem_unit)

  call get_unit(mem_unit)
  open(UNIT=mem_unit,FILE=meminfo,FORM='formatted',IOSTAT=STATUS)
  VERIFY_(STATUS)
  do; read (mem_unit,'(a)', end=20) string
    if ( INDEX ( string, 'MemTotal:' ) == 1 ) then  ! High Water Mark
      read (string(10:LEN_TRIM(string)-2),*) memtot
      if (TRIM(string(LEN_TRIM(string)-1:)) == "kB" ) &
        multiplier = 1.0/1024. ! Convert from kB to MB
      memtot = memtot * multiplier
    endif
    if ( INDEX ( string, 'MemFree:' ) == 1 ) then  ! High Water Mark
      read (string(9:LEN_TRIM(string)-2),*) memfree
      if (TRIM(string(LEN_TRIM(string)-1:)) == "kB" ) &
        multiplier = 1.0/1024. ! Convert from kB to MB
      memfree = memfree * multiplier
    endif
    if ( INDEX ( string, 'SwapTotal:' ) == 1 ) then  ! Resident Memory
      read (string(11:LEN_TRIM(string)-2),*) swaptot
      if (TRIM(string(LEN_TRIM(string)-1:)) == "kB" ) &
        multiplier = 1.0/1024. ! Convert from kB to MB
      swaptot = swaptot * multiplier
    endif
    if ( INDEX ( string, 'SwapFree:' ) == 1 ) then  ! Resident Memory
      read (string(10:LEN_TRIM(string)-2),*) swapfree
      if (TRIM(string(LEN_TRIM(string)-1:)) == "kB" ) &
        multiplier = 1.0/1024. ! Convert from kB to MB
      swapfree = swapfree * multiplier
    endif
  enddo
20 close(mem_unit)

   memused = memtot-memfree
   swapused = swaptot-swapfree

   RETURN_(ESMF_SUCCESS)
end subroutine mem_dump

subroutine MAPL_MemUtilsFree ( totmemfree, RC )

real, intent(out) :: totmemfree
integer, optional, intent(OUT  ) :: RC

! This routine returns the amount of free memory on Linux systems.
! It does this by querying a system file (file_name below).

character(len=32) :: meminfo   = '/proc/meminfo'
character(len=32) :: string
real    :: memfree, buffers, cached
integer :: mem_unit
real    :: multiplier

character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_MemUtilsFree"
integer :: status

  totmemfree = 0.0
  memfree = 0.0
  buffers = 0.0
  cached = 0.0
  multiplier = 1.0

  call get_unit(mem_unit)
  open(UNIT=mem_unit,FILE=meminfo,FORM='formatted',IOSTAT=STATUS)
  VERIFY_(STATUS)
  do; read (mem_unit,'(a)', end=20) string
    if ( INDEX ( string, 'MemFree:' ) == 1 ) then  ! Free memory
      read (string(9:LEN_TRIM(string)-2),*) memfree
      if (TRIM(string(LEN_TRIM(string)-1:)) == "kB" ) &
        multiplier = 1.0/1024. ! Convert from kB to MB
      memfree = memfree * multiplier
    endif
    if ( INDEX ( string, 'Buffers:' ) == 1 ) then  ! Buffers
      read (string(9:LEN_TRIM(string)-2),*) buffers
      if (TRIM(string(LEN_TRIM(string)-1:)) == "kB" ) &
        multiplier = 1.0/1024. ! Convert from kB to MB
      buffers = buffers * multiplier
    endif
    if ( INDEX ( string, 'Cached:' ) == 1 ) then  ! Cached
      read (string(8:LEN_TRIM(string)-2),*) cached
      if (TRIM(string(LEN_TRIM(string)-1:)) == "kB" ) &
        multiplier = 1.0/1024. ! Convert from kB to MB
      cached = cached * multiplier
    endif
  enddo
20 close(mem_unit)

  totmemfree = memfree + cached + buffers

   RETURN_(ESMF_SUCCESS)
end subroutine MAPL_MemUtilsFree

subroutine get_unit ( iunit )
  implicit none
!
  integer i
  integer ios
  integer iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end subroutine get_unit

end module MAPL_MemUtilsMod
