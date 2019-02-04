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
#include "ESMF_Macros.inc"
#include "ESMF.h"

      module ESMF_VMSubrs
      use ESMF
      use ESMF_TestMod
      
      implicit none

      public

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !Module global variables
      type(ESMF_VM),save :: vm, test_vm, vmAlias
      integer:: localPet, npets, rootPet, time_values(8)
      integer:: test_localPet, test_npets
      integer:: init_sec, end_sec, delay_time
      integer, allocatable:: array1(:), array3(:),array3_soln(:)
      integer, allocatable:: array4(:), array5(:)
      integer, dimension (:, :), allocatable:: array2
      integer::  func_results, myresults
      integer:: nsize, i, j
      integer:: isum, clock_count
      real(ESMF_KIND_R4) :: fsum4
      real(ESMF_KIND_R8) :: fsum
      logical:: vmBool

      real(ESMF_KIND_R8)::  vm_prec
      real(ESMF_KIND_R8), allocatable:: farray1(:), farray4(:), farray5(:)
      real(ESMF_KIND_R8), allocatable:: farray3(:) , farray3_soln(:)
      real(ESMF_KIND_R8), dimension(:,:), allocatable:: farray2
      real(ESMF_KIND_R8):: float_results, my_float_results, delay_interval

      real(ESMF_KIND_R4), allocatable:: f4array1(:), f4array4(:), f4array5(:)
      real(ESMF_KIND_R4), allocatable:: f4array3(:), f4array3_soln(:)
      real(ESMF_KIND_R4), dimension(:,:), allocatable:: f4array2
      real(ESMF_KIND_R4):: float4_results, my_float4_results

!==============================================================================
      contains
!==============================================================================

      subroutine test_vm_current
! This subroutine tests VM get current

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Get Current  Test"
      call ESMF_VMGetCurrent(test_vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Test_VM Get Test"
      call ESMF_VMGet(test_vm, localPet=test_localPet, petCount=test_npets, &
        rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did have correct PET infoS"
      write(name, *) "Verify the VM is correct Test"
      call ESMF_Test(((localPet.eq.test_localPet).and.(npets.eq.test_npets)), &
          name, failMsg, result, ESMF_SRCLINE)

      end subroutine test_vm_current

      subroutine test_vm_operators

      call ESMF_VMGetCurrent(vm, rc=rc)

      !------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "VM equality before assignment Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      vmBool = (vmAlias.eq.vm)
      call ESMF_Test(.not.vmBool, name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_VMAssignment(=)()
      write(name, *) "VM assignment and equality Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      vmAlias = vm
      vmBool = (vmAlias.eq.vm)
      call ESMF_Test(vmBool, name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_VMOperator(==)()
      write(name, *) "VM equality Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      vmBool = (vmAlias==vm)
      call ESMF_Test(vmBool, name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_VMOperator(/=)()
      write(name, *) "VM non-equality after destroy Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      vmBool = (vmAlias/=vm)
      call ESMF_Test(.not.vmBool, name, failMsg, result, ESMF_SRCLINE)
 
      end subroutine test_vm_operators
 
!----------------------------------------------------------------------------

      subroutine test_vm_time
! This subroutine tests VM time methods

      call date_and_time(values=time_values)
      init_sec = time_values(7)
      !=============================
      !EX_UTest
      delay_interval=5
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Time 5 Second Delay Test"
      call ESMF_VMWtimeDelay(delay=delay_interval, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !EX_UTest
      call date_and_time(values=time_values)
      end_sec = time_values(7)
      write(failMsg, *) "Wrong delay time"
      write(name, *) "Verify delay was 5 or 6 Seconds Test"
      delay_time = end_sec - init_sec
      print *, "init_sec =", init_sec
      print *, "end_sec =", end_sec
      print *, "delay_time =", delay_time
      ! if init_sec > end_sec add 60 to the difference.
      if (delay_time.lt.0) delay_time=delay_time + 60
      print *, "delay_time =", delay_time
      call ESMF_Test((delay_time.eq.5.or.delay_time.eq.6), name, failMsg, result, ESMF_SRCLINE)

      
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Time Prec. Test"
      call ESMF_VMWtimePrec(prec=vm_prec, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "vm_prec = ", vm_prec

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Value not between 0 and 1."
      write(name, *) "VM Time Prec. Value Test"
      call ESMF_Test(((vm_prec.gt.0).and.(vm_prec.lt.1)), name, failMsg, result, ESMF_SRCLINE)

      end subroutine test_vm_time

!-----------------------------------------------------------------------------
      subroutine test_Reduce_sum

      ! Tolerance for floating point equality tests
      real(ESMF_KIND_R4), parameter :: fuzz4 = 1.0e-7
      real(ESMF_KIND_R8), parameter :: fuzz8 = 1.0d-15

      logical :: passflg

! This subroutine tests all the overloaded versions of the ESMF global sum.

      ! Test with integer arguments
      !=============================
      !EX_UTest
      array4 = (/50,50/)
      ! Set expected results
      if (npets == 1) then
        array5 = (/1,2/)
      else
        array5 = (/604,608/)
      end if
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Reduce ESMF_REDUCE_SUM Test: integer"
      call ESMF_VMReduce(vm, sendData=array1, recvData=array4, &
        count=nsize, reduceflag=ESMF_REDUCE_SUM, rootPet=rootPet, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      print *, 'Global sum:' 
      print *, localPet,' array4(1): ', array4(1), &
                        ' array4(2):', array4(2) 

      print *, localPet,' array1(1): ', array1(1), &
                        ' array1(2):', array1(2) 
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_REDUCE_SUM Test: integer"
      if (localPet == rootPet) then
        passflg = array4(1)==array5(1) .and. array4(2)==array5(2)
      else
        passflg = .true.  ! Values are undefined after MPI_Reduce
      end if
      call ESMF_Test(passflg, name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !Test with ESMF_KIND_R8 arguments
      !=================================
      !EX_UTest
      farray4 = (/50.0,50.0/)
      ! Set expected results
      if (npets == 1) then
        farray5 = (/1.0,2.0/)
      else
        farray5 = (/604.0,608.0/)
      end if
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Reduce ESMF_REDUCE_SUM Test: ESMF_KIND_R8"
      call ESMF_VMReduce(vm, sendData=farray1, recvData=farray4, &
        count=nsize, reduceflag=ESMF_REDUCE_SUM, rootPet=rootPet, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      print *, 'Global sum:'
      print *, localPet,' farray4(1): ', farray4(1), &
                        ' farray4(2):', farray4(2)

       print *, localPet,' farray1(1): ', farray1(1), &
                        ' farray1(2):', farray1(2)


      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_REDUCE_SUM Results Test: ESMF_KIND_R8"
      if (localPet == rootPet) then
        passflg = abs (farray4(1)-farray5(1)) < fuzz8 .and. abs (farray4(2)-farray5(2)) < fuzz8
      else
        passflg = .true.  ! Values are undefined after MPI_Reduce
      end if
      call ESMF_Test(passflg, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !Test with ESMF_KIND_R4 arguments
      !=================================
      !EX_UTest
      f4array4 = (/50.0,50.0/)
      ! Set expected results
      if (npets == 1) then
        f4array5 = (/1.0,2.0/)
      else
        f4array5 = (/604.0,608.0/)
      end if

      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Reduce ESMF_REDUCE_SUM Test: ESMF_KIND_R4"
      call ESMF_VMReduce(vm, sendData=f4array1, recvData=f4array4, &
        count=nsize, reduceflag=ESMF_REDUCE_SUM, rootPet=rootPet, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      print *, 'Global sum:'
      print *, localPet,' f4array4(1): ', f4array4(1), &
                        ' f4array4(2):', f4array4(2)

       print *, localPet,' f4array1(1): ', f4array1(1), &
                        ' f4array1(2):', f4array1(2)



      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_REDUCE_SUM Results Test: ESMF_KIND_R4"
      if (localPet == rootPet) then
        passflg = abs (f4array4(1)-f4array5(1)) < fuzz4 .and. abs (f4array4(2)-f4array5(2)) < fuzz4
      else
        passflg = .true.  ! Values are undefined after MPI_Reduce
      end if
      call ESMF_Test(passflg, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      end subroutine test_Reduce_sum



!------------------------------------------------------------------------------

      subroutine test_Reduce_min

      ! Tolerance for floating point equality tests
      real(ESMF_KIND_R4), parameter :: fuzz4 = 0.0
      real(ESMF_KIND_R8), parameter :: fuzz8 = 0.0d0

      logical :: passflg

! This subroutine tests all the overloaded versions of the ESMF global sum.

      ! Test with integer arguments
      !=============================
      !EX_UTest
      array4 = (/50,50/)
      ! Set expected results
      array5 = (/1,2/)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Reduce ESMF_REDUCE_MINTest: integer"
      call ESMF_VMReduce(vm, sendData=array1, recvData=array4, &
        count=nsize, reduceflag=ESMF_REDUCE_MIN, rootPet=rootPet, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      print *, 'Global min:' 
      print *, localPet,' array4(1): ', array4(1), &
                        ' array4(2):', array4(2) 

      print *, localPet,' array1(1): ', array1(1), &
                        ' array1(2):', array1(2) 
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_REDUCE_MINResults Test: integer"
      if (localPet == rootPet) then
        passflg = array4(1)==array5(1) .and. array4(2)==array5(2)
      else
        passflg = .true.  ! Values are undefined after MPI_Reduce
      end if
      call ESMF_Test(passflg, name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !Test with ESMF_KIND_R8 arguments
      !=================================
      !EX_UTest
      farray4 = (/50.0,50.0/)
      ! Set expected results
      farray5 = (/1.0,2.0/)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Reduce ESMF_REDUCE_MINTest: ESMF_KIND_R8"
      call ESMF_VMReduce(vm, sendData=farray1, recvData=farray4, &
        count=nsize, reduceflag=ESMF_REDUCE_MIN, rootPet=rootPet, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      print *, 'Global min:'
      print *, localPet,' farray4(1): ', farray4(1), &
                        ' farray4(2):', farray4(2)

       print *, localPet,' farray1(1): ', farray1(1), &
                        ' farray1(2):', farray1(2)


      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_REDUCE_MINResults Test: ESMF_KIND_R8"
      if (localPet == rootPet) then
        passflg = abs (farray4(1)-farray5(1)) <= fuzz8 .and. abs (farray4(2)-farray5(2)) <= fuzz8
      else
        passflg = .true.  ! Values are undefined after MPI_Reduce
      end if
      call ESMF_Test(passflg, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !Test with ESMF_KIND_R4 arguments
      !=================================
      !EX_UTest
      f4array4 = (/50,50/)
      ! Set expected results
      f4array5 = (/1,2/)

      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Reduce ESMF_REDUCE_MINTest: ESMF_KIND_R4"
      call ESMF_VMReduce(vm, sendData=f4array1, recvData=f4array4, &
        count=nsize, reduceflag=ESMF_REDUCE_MIN, rootPet=rootPet, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      print *, 'Global min:'
      print *, localPet,' f4array4(1): ', f4array4(1), &
                        ' f4array4(2):', f4array4(2)

       print *, localPet,' f4array1(1): ', f4array1(1), &
                        ' f4array1(2):', f4array1(2)



      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_REDUCE_MINResults Test: ESMF_KIND_R4"
      if (localPet == rootPet) then
        passflg = abs (f4array4(1)-f4array5(1)) <= fuzz4 .and. abs (f4array4(2)-f4array5(2)) <= fuzz4
      else
        passflg = .true.  ! Values are undefined after MPI_Reduce
      end if
      call ESMF_Test(passflg, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      end subroutine test_Reduce_min


!------------------------------------------------------------------------------

      subroutine test_Reduce_max

      ! Tolerance for floating point equality tests
      real(ESMF_KIND_R4), parameter :: fuzz4 = 0.0
      real(ESMF_KIND_R8), parameter :: fuzz8 = 0.0d0

      logical :: passflg

! This subroutine tests all the overloaded versions of the ESMF global sum.

      ! Test with integer arguments
      !=============================
      !EX_UTest
      array4 = (/50,50/)
      ! Set expected results
      if (npets == 1) then
        array5 = (/1,2/)
      else
        array5 = (/301,302/)
      end if
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Reduce ESMF_REDUCE_MAXTest: integer"
      call ESMF_VMReduce(vm, sendData=array1, recvData=array4, &
        count=nsize, reduceflag=ESMF_REDUCE_MAX, rootPet=rootPet, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      print *, 'Global max:' 
      print *, localPet,' array4(1): ', array4(1), &
                        ' array4(2):', array4(2) 

      print *, localPet,' array1(1): ', array1(1), &
                        ' array1(2):', array1(2) 
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_REDUCE_MAXResults Test: integer"
      if (localPet == rootPet) then
        passflg = array4(1)==array5(1) .and. array4(2)==array5(2)
      else
        passflg = .true.  ! Values are undefined after MPI_Reduce
      end if
      call ESMF_Test(passflg, name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !Test with ESMF_KIND_R8 arguments
      !=================================
      !EX_UTest
      farray4 = (/50.0,50.0/)
      ! Set expected results
      if (npets == 1) then
        farray5 = (/1.0,2.0/)
      else
        farray5 = (/301.0,302.0/)
      end if
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Reduce ESMF_REDUCE_MAXTest: ESMF_KIND_R8"
      call ESMF_VMReduce(vm, sendData=farray1, recvData=farray4, &
        count=nsize, reduceflag=ESMF_REDUCE_MAX, rootPet=rootPet, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      print *, 'Global max:'
      print *, localPet,' farray4(1): ', farray4(1), &
                        ' farray4(2):', farray4(2)

       print *, localPet,' farray1(1): ', farray1(1), &
                        ' farray1(2):', farray1(2)


      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_REDUCE_MAXResults Test: ESMF_KIND_R8"
      if (localPet == rootPet) then
        passflg = abs (farray4(1)-farray5(1)) <= fuzz8 .and. abs (farray4(2)-farray5(2)) <= fuzz8
      else
        passflg = .true.  ! Values on non-root PETs are undefined after MPI_Reduce
      end if
      call ESMF_Test(passflg, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !Test with ESMF_KIND_R4 arguments
      !=================================
      !EX_UTest
      f4array4 = (/50.0,50.0/)
      ! Set expected results
      if (npets == 1) then
        f4array5 = (/1.0,2.0/)
      else
        f4array5 = (/301.0,302.0/)
      end if

      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Reduce ESMF_REDUCE_MAXTest: ESMF_KIND_R4"
      call ESMF_VMReduce(vm, sendData=f4array1, recvData=f4array4, &
        count=nsize, reduceflag=ESMF_REDUCE_MAX, rootPet=rootPet, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      print *, 'Global max:'
      print *, localPet,' f4array4(1): ', f4array4(1), &
                        ' f4array4(2):', f4array4(2)

       print *, localPet,' f4array1(1): ', f4array1(1), &
                        ' f4array1(2):', f4array1(2)



      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_REDUCE_MAXResults Test: ESMF_KIND_R4"
      if (localPet == rootPet) then
        passflg = abs (f4array4(1)-f4array5(1)) <= fuzz4 .and. abs (f4array4(2)-f4array5(2)) <= fuzz4
      else
        passflg = .true.  ! Values are undefined after MPI_Reduce
      end if
      call ESMF_Test(passflg, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      end subroutine test_Reduce_max


!-------------------------------------------------------------------------------
      subroutine test_AllFullReduce_sum
! This subroutine tests all the overloaded versions of the full ESMF global sum.

      ! Test with integer arguments
      !=============================
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_REDUCE_SUM Test"
      call ESMF_VMAllFullReduce(vm, sendData=array1, recvData=func_results, &
                                count=nsize, reduceflag=ESMF_REDUCE_SUM, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      myresults = SUM(array2)
      print *, 'Global sum:'
      print *, localPet,' func_results: ', func_results, &
                        ' myresults:', myresults
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Full Reduce ESMF_REDUCE_SUM Results Test"
      call ESMF_Test((func_results.eq.myresults), name, failMsg, &
                      result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !Test with ESMF_KIND_R8 arguments
      !=================================
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_REDUCE_SUM Test: ESMF_KIND_R8"
      call ESMF_VMAllFullReduce(vm, sendData=farray1, recvData=float_results, &
                                count=nsize, reduceflag=ESMF_REDUCE_SUM, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      my_float_results = SUM(farray2)
      print *, 'Global sum:'
      print *, localPet,' float_results: ', float_results, &
                        ' my_float_results:', my_float_results

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_REDUCE_SUM Results Test: ESMF_KIND_R8"
      call ESMF_Test(.not. abs (float_results - my_float_results) > 0.0, name, failMsg, &
                      result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !Test with ESMF_KIND_R4 arguments
      !=================================
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_REDUCE_SUM Test: ESMF_KIND_R4"
      call ESMF_VMAllFullReduce(vm, sendData=f4array1, recvData=float4_results,&
                                count=nsize, reduceflag=ESMF_REDUCE_SUM, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! print the result
      my_float4_results = SUM(f4array2)
      print *, 'Global sum:'
      print *, localPet,' float4_results: ', float4_results, &
                        ' my_float4_results:', my_float4_results

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_REDUCE_SUM Results Test: ESMF_KIND_R4"
      call ESMF_Test(.not. abs (float4_results - my_float4_results) > 0.0, name, failMsg, &
                      result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      end subroutine test_AllFullReduce_sum

!==============================================================================

      subroutine test_AllFullReduce_min
! This subroutine tests all the overloaded versions of the full ESMF global min.

      !Test with integer arguments
      !===========================
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_REDUCE_MINTest"
      call ESMF_VMAllFullReduce(vm, sendData=array1, recvData=func_results, &
                                count=nsize, reduceflag=ESMF_REDUCE_MIN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      myresults = MINVAL(array2)
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Full Reduce ESMF_REDUCE_MINResults Test"
      call ESMF_Test((func_results.eq.myresults), name, failMsg, &
                      result, ESMF_SRCLINE)
      print *, localPet,' func_results: ', func_results, &
                        ' myresults:', myresults
      !------------------------------------------------------------------------

      !Test with ESMF_KIND_R8 arguments
      !================================
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_REDUCE_MINTest: ESMF_KIND_R8"
      call ESMF_VMAllFullReduce(vm, sendData=farray1, recvData=float_results, &
                                count=nsize, reduceflag=ESMF_REDUCE_MIN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      my_float_results = MINVAL(farray2)
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Full Reduce ESMF_REDUCE_MINResults Test:ESMF_KIND_R8"
      call ESMF_Test((func_results.eq.myresults), name, failMsg, &
                     result, ESMF_SRCLINE)
      print *, localPet,' float_results: ', float_results, &
                        ' my_float_results:', my_float_results
      !------------------------------------------------------------------------

      !Test with ESMF_KIND_R4 arguments
      !================================
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_REDUCE_MINTest: ESMF_KIND_R4"
      call ESMF_VMAllFullReduce(vm, sendData=f4array1, recvData=float4_results,&
                                count=nsize, reduceflag=ESMF_REDUCE_MIN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      my_float4_results = MINVAL(f4array2)
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Full Reduce ESMF_REDUCE_MINResults Test:ESMF_KIND_R4"
      call ESMF_Test((func_results.eq.myresults), name, failMsg, &
                      result, ESMF_SRCLINE)
      print *, localPet,' float4_results: ', float4_results, &
                        ' my_float4_results:', my_float4_results
      !------------------------------------------------------------------------


      end subroutine test_AllFullReduce_min

!==============================================================================

      subroutine test_AllFullReduce_max
! This subroutine tests all the overloaded versions of the full ESMF global max.

      !Test with Integer arguments
      !===========================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_REDUCE_MAXTest"
      call ESMF_VMAllFullReduce(vm, sendData=array1, recvData=func_results, &
                                count=nsize, reduceflag=ESMF_REDUCE_MAX, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      myresults = MAXVAL(array2)
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Full Reduce ESMF_REDUCE_MAXResults Test"
      call ESMF_Test((func_results.eq.myresults), name, failMsg, &
                      result, ESMF_SRCLINE)
      print *, localPet,' func_results: ', func_results, &
                        ' myresults:', myresults


      !Test with ESMF_KIND_R8 arguments
      !=================================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_REDUCE_MAXTest:ESMF_KIND_R8"
      call ESMF_VMAllFullReduce(vm, sendData=farray1, recvData=float_results, &
                                count=nsize, reduceflag=ESMF_REDUCE_MAX, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      my_float_results = MAXVAL(farray2)
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Full Reduce ESMF_REDUCE_MAXResults Test:ESMF_KIND_R8"
      call ESMF_Test(.not. abs (float_results - my_float_results) > 0.0, name, failMsg, &
                      result, ESMF_SRCLINE)
      print *, localPet,' float_results: ', float_results, &
                        ' my_float_results:', my_float_results


      !Test with ESMF_KIND_R4 arguments
      !================================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_REDUCE_MAXTest:ESMF_KIND_R4"
      call ESMF_VMAllFullReduce(vm, sendData=f4array1, recvData=float4_results,&
                                count=nsize, reduceflag=ESMF_REDUCE_MAX, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      my_float4_results = MAXVAL(f4array2)
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Full Reduce ESMF_REDUCE_MAXResults Test:ESMF_KIND_R4"
      call ESMF_Test(.not. abs (float4_results - my_float4_results) > 0.0, name, failMsg, &
                      result, ESMF_SRCLINE)
      print *, localPet,' float4_results: ', float4_results, &
                        ' my_float4_results:', my_float4_results

      end subroutine test_AllFullReduce_max

!=============================================================================
      subroutine test_AllReduce_sum

      !Test with Integer arguments
      !===========================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce ESMF_REDUCE_SUM Test"
      call ESMF_VMAllReduce(vm, sendData=array1, recvData=array3, count=nsize, &
                            reduceflag=ESMF_REDUCE_SUM, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      isum=0
      do j=1,npets
      end do
      do i=1,nsize
        array3_soln(i) = sum( array2(i,:) )
        print *, localPet,'array3(',i,')=',array3(i), &
                          'array3_soln(',i,')=',array3_soln(i)
        isum=isum + abs( array3(i) - array3_soln(i) )
      end do
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce ESMF_REDUCE_SUM Results Test"
      call ESMF_Test((isum.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Scalar version
      ! ==============
      ! EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce scalar ESMF_REDUCE_SUM Test"
      i = npets
      j = -1
      call ESMF_VMAllReduce (vm, sendData=i, recvData=j,  &
                             reduceflag=ESMF_REDUCE_SUM, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce scalar ESMF_REDUCE_SUM Results Test"
      call ESMF_Test(j == npets*npets, name, failMsg, result, ESMF_SRCLINE)


      !Test with ESMF_KIND_R8  arguments
      !=================================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce ESMF_REDUCE_SUM Test"
      call ESMF_VMAllReduce(vm, sendData=farray1, recvData=farray3, &
                            count=nsize, reduceflag=ESMF_REDUCE_SUM, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      fsum=0.0
      do i=1,nsize
        farray3_soln(i) = sum( farray2(i,:) )
        print *, localPet,'farray3(',i,')=',farray3(i), &
                          'farray3_soln(',i,')=',farray3_soln(i)
        fsum=fsum + abs( farray3(i) - farray3_soln(i) )
      end do
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce ESMF_REDUCE_SUM Results Test"
      call ESMF_Test(.not. abs (fsum) > 0.0, name, failMsg, result, ESMF_SRCLINE)

      !Test with ESMF_KIND_R4  arguments
      !=================================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce ESMF_REDUCE_SUM Test"
      call ESMF_VMAllReduce(vm, sendData=f4array1, recvData=f4array3, &
                            count=nsize, reduceflag=ESMF_REDUCE_SUM, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      fsum4=0.0
      do i=1,nsize
        f4array3_soln(i) = sum( f4array2(i,:) )
        print *, localPet,'f4array3(',i,')=',f4array3(i), &
                          'f4array3_soln(',i,')=',f4array3_soln(i)
        fsum4=fsum4 + abs( f4array3(i) - f4array3_soln(i) )
      end do
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce ESMF_REDUCE_SUM Results Test"
      call ESMF_Test(.not. abs (fsum4) > 0.0, name, failMsg, result, ESMF_SRCLINE)

      end subroutine test_AllReduce_sum

!-----------
      subroutine test_AllReduce_min
      !Test with Integer arguments
      !==========================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce ESMF_REDUCE_MINTest"
      call ESMF_VMAllReduce(vm, sendData=array1, recvData=array3, &
                            count=nsize, reduceflag=ESMF_REDUCE_MIN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      isum=0
      do i=1,nsize
        array3_soln(i) = minval( array2(i,:) )
        print *, localPet,'array3(',i,')=',array3(i), &
                          'array3_soln(',i,')=',array3_soln(i)
        isum=isum + abs( array3(i) - array3_soln(i) )
      end do
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce ESMF_REDUCE_MINResults Test"
      call ESMF_Test((isum.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Scalar version
      ! ==============
      ! EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce scalar ESMF_REDUCE_MINTest"
      i = localPet
      j = -1
      call ESMF_VMAllReduce (vm, sendData=i, recvData=j,  &
                             reduceflag=ESMF_REDUCE_MIN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce scalar ESMF_REDUCE_MINResults Test"
      call ESMF_Test(j == 0, name, failMsg, result, ESMF_SRCLINE)


      !Test with ESMF_KIND_R8 arguments
      !================================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce ESMF_REDUCE_MINTest: ESMF_KIND_R8"
      call ESMF_VMAllReduce(vm, sendData=farray1, recvData=farray3, &
                            count=nsize, reduceflag=ESMF_REDUCE_MIN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      fsum=0.0
      do i=1,nsize
        farray3_soln(i) = minval( farray2(i,:) )
        print *, localPet,'farray3(',i,')=',farray3(i), &
                          'farray3_soln(',i,')=',farray3_soln(i)
        fsum=fsum + abs( farray3(i) - farray3_soln(i) )
      end do
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce ESMF_REDUCE_MINResults Test:ESMF_KIND_R8"
      call ESMF_Test(.not. abs (fsum) > 0.0, name, failMsg, result, ESMF_SRCLINE)


      !Test with ESMF_KIND_R4 arguments
      !================================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce ESMF_REDUCE_MINTest: ESMF_KIND_R4"
      call ESMF_VMAllReduce(vm, sendData=f4array1, recvData=f4array3, &
                            count=nsize, reduceflag=ESMF_REDUCE_MIN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      fsum4=0.0
      do i=1,nsize
        f4array3_soln(i) = minval( f4array2(i,:) )
        print *, localPet,'f4array3(',i,')=',f4array3(i), &
                          'f4array3_soln(',i,')=',f4array3_soln(i)
        fsum4=fsum4 + abs( f4array3(i) - f4array3_soln(i) )
      end do
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce ESMF_REDUCE_MINResults Test:ESMF_KIND_R4"
      call ESMF_Test(.not. abs (fsum4) > 0.0, name, failMsg, result, ESMF_SRCLINE)

      end subroutine test_AllReduce_min

!-----------
      subroutine test_AllReduce_max

      !Tests using Integer arguments
      !==================================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce ESMF_REDUCE_MAXTest"
      call ESMF_VMAllReduce(vm, sendData=array1, recvData=array3, &
                            count=nsize, reduceflag=ESMF_REDUCE_MAX, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      isum=0
      do i=1,nsize
        array3_soln(i) = maxval( array2(i,:) )
        print *, localPet,'array3(',i,')=',array3(i), &
                          'array3_soln(',i,')=',array3_soln(i)
        isum=isum + abs( array3(i) - array3_soln(i) )
      end do
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce ESMF_REDUCE_MAXResults Test"
      call ESMF_Test((isum.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Scalar version
      ! ==============
      ! EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce scalar ESMF_REDUCE_MAXTest"
      i = localPet
      j = -1
      call ESMF_VMAllReduce (vm, sendData=i, recvData=j,  &
                             reduceflag=ESMF_REDUCE_MAX, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce scalar ESMF_REDUCE_MAXResults Test"
      call ESMF_Test(j+1 == npets, name, failMsg, result, ESMF_SRCLINE)


      !Tests using ESMF_KIND_R8 arguments
      !==================================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce ESMF_REDUCE_MAXTest: ESMF_KIND_R8"
      call ESMF_VMAllReduce(vm, sendData=farray1, recvData=farray3, &
                            count=nsize, reduceflag=ESMF_REDUCE_MAX, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      fsum=0.0
      do i=1,nsize
        farray3_soln(i) = maxval( farray2(i,:) )
        print *, localPet,'farray3(',i,')=',farray3(i), &
                          'farray3_soln(',i,')=',farray3_soln(i)
        fsum=fsum + abs( farray3(i) - farray3_soln(i) )
      end do
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce ESMF_REDUCE_MAXResults Test: ESMF_KIND_R8"
      call ESMF_Test(.not. abs (fsum) > 0.0, name, failMsg, result, ESMF_SRCLINE)


      !Tests using ESMF_KIND_R4 arguments
      !==================================
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Reduce ESMF_REDUCE_MAXTest: ESMF_KIND_R4"
      call ESMF_VMAllReduce(vm, sendData=f4array1, recvData=f4array3, &
                            count=nsize, reduceflag=ESMF_REDUCE_MAX, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      fsum4=0.0
      do i=1,nsize
        f4array3_soln(i) = maxval( f4array2(i,:) )
        print *, localPet,'f4array3(',i,')=',f4array3(i), &
                          'f4array3_soln(',i,')=',f4array3_soln(i)
        fsum4=fsum4 + abs( f4array3(i) - f4array3_soln(i) )
      end do
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify All Reduce ESMF_REDUCE_MAXResults Test: ESMF_KIND_R4"
      call ESMF_Test(.not. abs (fsum4) > 0.0, name, failMsg, result, ESMF_SRCLINE)

      end subroutine test_AllReduce_max


      end module
!==============================================================================
!==============================================================================


      program ESMF_VMUTest

!------------------------------------------------------------------------------
 

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMTest - This unit test file verifies VM methods.
!
! !DESCRIPTION:
!
! The code in this file drives F90 VM unit tests.
! The companion file ESMF\_VM.F90 contains the definitions for the
! VM methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF

      use ESMF_VMSubrs     ! VM specific subroutines

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      type(ESMF_VMId), allocatable :: vmid1(:), vmid2(:)
      integer   :: id_value
      character :: key_value

      type(ESMF_Grid)     :: grid, grid_temp
      type(ESMF_Pointer)  :: grid_tempp
      type(ESMF_Base)     :: base
      integer             :: id_temp, ssiCount, ssiMinPetCount, ssiMaxPetCount
      type(ESMF_VMId)     :: vmid_temp
      type(ESMF_Logical)  :: object_found
      type(ESMF_Log)      :: log
      character(len=80)   :: msg

      logical :: tf

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "VM Get before initialization Test"
      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "VM validate Test before valid VM"
      call ESMF_VMValidate(vm, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Get Global Test"
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Get Test"
      call ESMF_VMGet(vm, petCount=npets, ssiCount=ssiCount, &
        ssiMinPetCount=ssiMinPetCount, ssiMaxPetCount=ssiMaxPetCount, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      if (npets .ne. 1 .and. npets .ne. 4) then
        print *, 'PET count must be 1 or 4, npets =', npets
        call ESMF_Finalize (endflag=ESMF_END_ABORT)
      end if

      write(msg,*) "petCount=", npets, " ssiCount=", ssiCount
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      write(msg,*) "ssiMinPetCount=", ssiMinPetCount, &
        " ssiMaxPetCount=", ssiMaxPetCount
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM print Test"
      call ESMF_VMPrint(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM validate Test"
      call ESMF_VMValidate(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Get Test"
      call ESMF_VMGet(vm, localPet=localPet, petCount=npets, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      ! allocate data arrays

      nsize = 2
      allocate(array1(nsize))
      allocate(array4(nsize))
      allocate(farray4(nsize))
      allocate(f4array4(nsize))
      allocate(farray1(nsize))
      allocate(f4array1(nsize))
      allocate(array5(nsize))
      allocate(farray5(nsize))
      allocate(f4array5(nsize))

      allocate(array3(nsize))
      allocate(farray3(nsize))
      allocate(f4array3(nsize))

      allocate(array3_soln(nsize))
      allocate(farray3_soln(nsize))
      allocate(f4array3_soln(nsize))

      ! prepare data array1, farray1, f4array1
      do i=1, nsize
        array1(i) = localPet * 100 + i
        farray1(i)= real( array1(i) , ESMF_KIND_R8)
        f4array1(i)=real(farray1(i))
      enddo

      ! Populate array2
      allocate(array2(nsize,npets))
      allocate(farray2(nsize,npets))
      allocate(f4array2(nsize,npets))
      do j=1, npets 
        do i=1, nsize
           array2(i,j) = (j-1) * 100 + i
          farray2(i,j)  = real( array2(i,j) , ESMF_KIND_R8)
          f4array2(i,j) = real(farray2(i,j))
        enddo
      enddo

      !------------------------------------------------------------------------

      rootPet = 0

      call test_vm_current
      call test_vm_operators
      call test_vm_time
      
      call test_Reduce_sum
      call test_AllFullReduce_sum
      call test_AllReduce_sum

      call test_Reduce_min
      call test_AllFullReduce_min
      call test_AllReduce_min

      call test_Reduce_max
      call test_AllFullReduce_max
      call test_AllReduce_max

      !------------------------------------------------------------------------
      ! deallocate data arrays
      
      deallocate(array1)
      deallocate(array4)
      deallocate(farray4)
      deallocate(f4array4)
      deallocate(farray1)
      deallocate(f4array1)
      deallocate(array5)
      deallocate(farray5)
      deallocate(f4array5)

      deallocate(array3)
      deallocate(farray3)
      deallocate(f4array3)

      deallocate(array3_soln)
      deallocate(farray3_soln)
      deallocate(f4array3_soln)

      deallocate(array2)
      deallocate(farray2)
      deallocate(f4array2)

      !------------------------------------------------------------------------
      ! VMId tests
      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VMId Create vmid1 Test"
      allocate (vmid1(1))
      call ESMF_VMIdCreate (vmid1, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VMId Create vmid2 Test"
      allocate (vmid2(1))
      call ESMF_VMIdCreate (vmid2, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Bad comparison result"
      write(name, *) "VMId Compare Test"
      tf = ESMF_VMIdCompare (vmid1(1), vmid2(1), rc=rc)
      call ESMF_Test(tf, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VMId Set test values Test"
      call c_ESMCI_VMIdSet (vmid1(1), 1234, achar (123), rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VMId print test values Test"
      call ESMF_VMIdPrint (vmid1(1), rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Bad comparison result"
      write(name, *) "VMId Compare Test"
      tf = ESMF_VMIdCompare (vmid1(1), vmid2(1))
      call ESMF_Test(.not. tf, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VMId Set test values Test"
      call ESMF_VMIdCopy (dest=vmid2, source=vmid1, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Bad comparison result"
      write(name, *) "VMId Compare Test"
      tf = ESMF_VMIdCompare (vmid1(1), vmid2(1))
      call ESMF_Test(tf, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VMId Get test values Test"
      call c_ESMCI_VMIdGet (vmid2(1), id_value, key_value, rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VMId id_value Test"
      call ESMF_Test(id_value == 1234, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VMId key_value Test"
      call ESMF_Test(key_value == achar (123), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Destroy #1 failed"
      write(name, *) "VMId destroy #1 Test"
      call ESMF_VMIdDestroy (vmid1, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Destroy #2 failed"
      write(name, *) "VMId destroy #2 Test"
      call ESMF_VMIdDestroy (vmid2, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      deallocate (vmid1, vmid2)

      ! Test accessing an object by its id and vmid

      !------------------------------------------------------------------------
      !EX_UTest
      ! Create Grid object.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Create a Grid object"
      write(failMsg, *) 'Did not return ESMF_SUCCESS'
      grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
            regDecomp=(/2,2/), name="Grid", rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_GridPrint (grid, rc=rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Access Grids Base ID.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Access Base of Grid"
      write(failMsg, *) 'Did not return ESMF_SUCCESS'
      call c_esmc_getid (grid, id_temp, rc)
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, 'Grids Base ID =', id_temp

      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a temporary VMId.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Create temporary VMId"
      write(failMsg, *) 'Did not return ESMF_SUCCESS'
      call ESMF_VMIdCreate (vmid_temp, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_VMIdPrint (vmid_temp, rc=rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy a temporary VMId.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Destroy VMId"
      write(failMsg, *) 'Did not return ESMF_SUCCESS'
      call ESMF_VMIdDestroy (vmid_temp, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Access Grids Base VMId.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Access Base ID and VMId"
      write(failMsg, *) 'Did not return ESMF_SUCCESS'
      call c_esmc_getvmid (grid, vmid_temp, rc)
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_VMIdPrint (vmid_temp, rc=rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test obtaining a pointer to an object, given its id and vmid.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Obtain pointer to object via id/vmid lookup"
      write(failMsg, *) 'Can not access object'
      call c_esmc_vmgetobject (grid_temp,  &
          id_temp, vmid_temp, "Grid", ESMF_PROXYNO,  &
          object_found, rc)
      grid_temp%isInit = ESMF_INIT_CREATED
      call ESMF_Test((rc == ESMF_SUCCESS), &
          name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test for object found.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Test for object found"
      write(failMsg, *) 'Did not find object'
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, object_found == ESMF_TRUE)
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Print aliased Grid object.
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Print aliased Grid object"
      write(failMsg, *) 'Could not print object'
      print *, 'calling ESMF_GridPrint(grid_temp):'
      call ESMF_GridPrint (grid_temp, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), &
          name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! ESMF_VMLogMemInfo()
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Write VMLogMemInfo into the default log w/o prefix"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_VMLogMemInfo(rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! ESMF_VMLogMemInfo()
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Write VMLogMemInfo into the default log w/ prefix"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_VMLogMemInfo(prefix="TestPrefix", rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      call ESMF_LogOpen(log, filename="vmLogMemInfo.log", appendflag=.false., &
        rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! ESMF_VMLogMemInfo()
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Write VMLogMemInfo into custom log w/o prefix"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_VMLogMemInfo(log=log, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! ESMF_VMLogMemInfo()
      ! WARNING: This is testing an INTERNAL method.  It is NOT
      ! part of the supported ESMF user API!
      write(name, *) "Write VMLogMemInfo into custom log w/ prefix"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_VMLogMemInfo(prefix="TestPrefix", log=log, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      call ESMF_LogClose(log, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
#endif
      call ESMF_TestEnd(ESMF_SRCLINE)

      end program ESMF_VMUTest
