! $Id: ESMF_ArrayArbIdxSTest.F90,v 1.6 2007/07/18 18:33:42 svasquez Exp $
!
!-------------------------------------------------------------------------
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test ArrayArbIdx
!     Test the use of 1D arrays with 'user supplied' arbitrary indexing for
!  the array entries.
!
!\begin{verbatim}

program ArrayArbIdx

#include <ESMF_Macros.inc>

! ESMF Framework module
  use ESMF_Mod
  use ESMF_TestMod

  implicit none

  ! Local variables
  type(ESMF_VM)         :: vm
  type(ESMF_DistGrid)   :: srcDistgrid, dstDistgrid
  type(ESMF_Array)      :: srcArray, dstArray
  type(ESMF_ArraySpec)  :: arrayspec
  type(ESMF_RouteHandle):: routehandle
  integer(ESMF_KIND_I4), pointer :: farrayPtr(:)  ! matching F90 array pointer
  integer               :: rc, i, petCount, localPet
  integer, allocatable  :: srcIndices(:)
  integer(ESMF_KIND_I4) :: factorList(3)
  integer               :: factorIndexList(2,3)

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: testresult = 0

  ! individual test name
  character(ESMF_MAXSTR) :: testname

  ! individual test failure message, and final status msg
  character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  print *, "------------------------------------------- "
  print *, "Start of System Test ESMF_ArrayArbIdxSTest: "
  print *, "------------------------------------------- "

  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 6) then
    print *, "This system test needs to run on exactly 6 PETs, petCount = ", &
      petCount
    goto 10
  endif

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  allocate(srcIndices((localPet+1)*2))   ! sizes: 2, 4, 6, 8, 10, 12
  
  do i=1,(localPet+1)*2
    srcIndices(i) = localPet*20 + (21 - i) ! set arbitrary but unique seq. indices
  enddo 

  srcDistgrid = ESMF_DistGridCreate(arbSeqIndexList=srcIndices, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  deallocate(srcIndices)

!  call ESMF_DistGridPrint(srcDistgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! The srcDistgrid has 1 DE per PET, i.e. 6 DEs. Each DE has a different
  ! number of local cells in the DistGrid. The arbitrary sequence indices are
  ! constructed to look like this:
  !
  ! PET   localDE   DE    indices
  ! 0     0         0     20, 19
  ! 1     0         1     40, 39, 38, 37
  ! 2     0         2     60, 59, 58, 57, 56, 55
  ! 3     0         3     80, 79, 78, 77, 76, 75, 74, 73
  ! 4     0         4     100, 99, 98, 97, 96, 95, 94, 93, 92, 91
  ! 5     0         5     120, 119, 118, 117, 116, 115, 114, 113, 112, 111, 110, 109
  
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=1, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_ArrayGet(srcArray, farrayPtr=farrayPtr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
    farrayPtr(i) = localPet * 10 + i    ! initialize
  enddo
  
  ! The lbound(farrayPtr, 1) = 1 because ArrayCreate() by default sets local
  ! bounds starting at 1. Thus the srcArray contents are locally set to:
  !
  ! PET   localDE   DE    srcArray contents
  ! 0     0         0     1, 2
  ! 1     0         1     11, 12, 13, 14
  ! 2     0         2     21, 22, 23, 24, 25, 26
  ! 3     0         3     31, 32, 33, 34, 35, 36, 37, 38
  ! 4     0         4     41, 42, 43, 44, 45, 46, 47, 48, 49, 50
  ! 5     0         5     51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62
  
  dstDistgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/12/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
      
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_ArrayGet(dstArray, farrayPtr=farrayPtr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  farrayPtr = 0     ! initialize
  
  ! The dstDistgrid evenly divides 12 cells across the 6 DEs (becaues default
  ! is 1 DE per PET and there are 6 PETs running this example).
  ! The default sequenceIndex of dstDistGrid is determined by the default rule
  ! of simply enumerating the cells within the tile, starting at 1:
  !
  ! PET   localDE   DE    indices
  ! 0     0         0     1, 2
  ! 1     0         1     3, 4
  ! 2     0         2     5, 6
  ! 3     0         3     7, 8
  ! 4     0         4     9, 10
  ! 5     0         5     11, 12
  !
  ! The dstArray created on the dstDistgrid has the following shape and
  ! initialization:
  !
  ! PET   localDE   DE    srcArray contents
  ! 0     0         0     0, 0
  ! 1     0         1     0, 0
  ! 2     0         2     0, 0
  ! 3     0         3     0, 0
  ! 4     0         4     0, 0
  ! 5     0         5     0, 0


  if (localPet == 0) then
    factorList(1) = 2
    factorIndexList(1,1) = 76   ! src seq index
    factorIndexList(2,1) = 1    ! dst seq index
    factorList(2) = -3
    factorIndexList(1,2) = 40   ! src seq index
    factorIndexList(2,2) = 1    ! dst seq index
    factorList(3) = 1
    factorIndexList(1,3) = 19   ! src seq index
    factorIndexList(2,3) = 4    ! dst seq index
  else if (localPet == 1) then
    factorList(1) = -4
    factorIndexList(1,1) = 112  ! src seq index
    factorIndexList(2,1) = 4    ! dst seq index
    factorList(2) = 2
    factorIndexList(1,2) = 92   ! src seq index
    factorIndexList(2,2) = 4    ! dst seq index
    factorList(3) = 7
    factorIndexList(1,3) = 77   ! src seq index
    factorIndexList(2,3) = 5    ! dst seq index
  else if (localPet == 2) then
    factorList(1) = -2
    factorIndexList(1,1) = 98   ! src seq index
    factorIndexList(2,1) = 6    ! dst seq index
    factorList(2) = 1
    factorIndexList(1,2) = 39   ! src seq index
    factorIndexList(2,2) = 7    ! dst seq index
    factorList(3) = 1
    factorIndexList(1,3) = 58   ! src seq index
    factorIndexList(2,3) = 7    ! dst seq index
  else if (localPet == 3) then
    factorList(1) = 1 
    factorIndexList(1,1) = 95   ! src seq index
    factorIndexList(2,1) = 7    ! dst seq index
    factorList(2) = -4
    factorIndexList(1,2) = 113  ! src seq index
    factorIndexList(2,2) = 7    ! dst seq index
    factorList(3) = -1
    factorIndexList(1,3) = 20   ! src seq index
    factorIndexList(2,3) = 12   ! dst seq index
  else if (localPet == 4) then
    factorList(1) = 100
    factorIndexList(1,1) = 99   ! src seq index
    factorIndexList(2,1) = 9    ! dst seq index
    factorList(2) = -2
    factorIndexList(1,2) = 109  ! src seq index
    factorIndexList(2,2) = 9    ! dst seq index
    factorList(3) = -5
    factorIndexList(1,3) = 80   ! src seq index
    factorIndexList(2,3) = 9    ! dst seq index
  else if (localPet == 5) then
    factorList(1) = 22
    factorIndexList(1,1) = 55   ! src seq index
    factorIndexList(2,1) = 9    ! dst seq index
    factorList(2) = 5
    factorIndexList(1,2) = 120  ! src seq index
    factorIndexList(2,2) = 11   ! dst seq index
    factorList(3) = -11
    factorIndexList(1,3) = 74 ! src seq index
    factorIndexList(2,3) = 10   ! dst seq index
  endif
  
  ! Each of the 6 PETs defines 3 different factors of the sparse matrix
  ! multiplication for a total of 6 x 3 = 18 non-zero entries in the 
  ! 12 x 42 = 504 matrix.
  
  call ESMF_ArraySparseMatMulStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, factorList=factorList, &
    factorIndexList=factorIndexList, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_ArraySparseMatMul(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  ! The expected result of the sparse matrix multiplication in dstArray is:
  !
  !
  ! PET   localDE   DE    srcArray contents
  ! 0     0         0     2x35 - 3x11 = 37, 0
  ! 1     0         1     0, 1x2 - 4x59 + 2x49 = -136
  ! 2     0         2     7x34 = 238, -2x43 = -86
  ! 3     0         3     1x12 + 1x23 + 1x46 - 4x58 = -151, 0
  ! 4     0         4     100x42 - 2x62 - 5x31 + 22x26 = 4493, -11x37 = -407
  ! 5     0         5     5x51 = 255, -1
  
  print *, "localPet: ",localPet," dstArray: ",farrayPtr
  
  if (localPet == 0) then
    if (farrayPtr(1) /= 37)   rc = ESMF_FAILURE
    if (farrayPtr(2) /= 0)    rc = ESMF_FAILURE
  else if (localPet == 1) then
    if (farrayPtr(1) /= 0)    rc = ESMF_FAILURE
    if (farrayPtr(2) /= -136) rc = ESMF_FAILURE
  else if (localPet == 2) then
    if (farrayPtr(1) /= 238)  rc = ESMF_FAILURE
    if (farrayPtr(2) /= -86)  rc = ESMF_FAILURE
  else if (localPet == 3) then
    if (farrayPtr(1) /= -151) rc = ESMF_FAILURE
    if (farrayPtr(2) /= 0)    rc = ESMF_FAILURE
  else if (localPet == 4) then
    if (farrayPtr(1) /= 4493) rc = ESMF_FAILURE
    if (farrayPtr(2) /= -407) rc = ESMF_FAILURE
  else if (localPet == 5) then
    if (farrayPtr(1) /= 255)  rc = ESMF_FAILURE
    if (farrayPtr(2) /= -1)   rc = ESMF_FAILURE
  endif

  call ESMF_ArrayDestroy(srcArray, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(srcDistGrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_ArrayDestroy(dstArray, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(dstDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

10 continue
  print *, "System Test ArrayArbIdx complete."

  ! Normal ESMF Test output
  write(failMsg, *) "System Test failure"
  write(testname, *) "System Test ArrayArbIdx"

  call ESMF_TestGlobal((rc.eq.ESMF_SUCCESS), testname, failMsg, testresult, &
    ESMF_SRCLINE)

  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"
  print *, "Test finished, localPet = ", localPet
  print *, "------------------------------------------------------------"
  print *, "------------------------------------------------------------"

  call ESMF_Finalize()

end program ArrayArbIdx
    
!\end{verbatim}
    
