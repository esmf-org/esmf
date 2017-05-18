! $Id$
!
! Example/test code which shows User Negotiation calls used by main

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  User negotitation modules used by main
!
!
!\begin{verbatim}

  module pet_partitioner

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod
  use user_neg_info

  integer, parameter :: &
    ESMF_ACC_PET_PARTITION_CONTIG = 1, ESMF_ACC_PET_PARTITION_EVEN_STRIDED = 2,&
    ESMF_ACC_PET_PARTITION_LOWEST_RANK = 3

  public ESMF_ACC_PET_PARTITION_CONTIG, ESMF_ACC_PET_PARTITION_EVEN_STRIDED
  public partition_pet_global_list, subtract_pets

  contains

  ! This subroutine partitions the petList [0:petCount) into
  ! an accelerated petlist, aPets, that consists of pets with access to
  ! the accelerator device
  ! and a non accelerated petlist, nonAPets, that consists of pets without
  ! access to the accelerator device
  ! based on the partition strategy, partStrategy
  ! nDevApet returns the number of devices pet APet
  ! ESMF_ACC_PET_PARTITION_CONTIG => accelerated devices are
  !   available from the lowest ranked MPI processes (rank < number of
  !   devices)
  ! ESMF_ACC_PET_PARTITION_EVEN_STRIDED => accelerated devices are
  !   available from the lowest ranked even MPI processes
  ! ESMF_ACC_PET_PARTITION_LOWEST_RANK => accelerated devices are
  !   available from the lowest ranked process (single process)
  !   on each node with access to a device
  subroutine partition_pets(vm, aPets, nonAPets, nDevAPet, partStrategy, rc)
!   ! The ESMF Framework module
    use ESMF
    use mpi
    implicit none
    
    type(ESMF_VM), intent(in) :: vm
    integer, intent(out), allocatable :: aPets(:)
    integer, intent(out), allocatable :: nonAPets(:)
    integer, intent(out) :: nDevAPet
    integer, intent(in) :: partStrategy
    integer, intent(out) :: rc
  
    integer, allocatable :: isAccPetList(:)
    integer :: isAccPet(1)
    integer :: numAPets, numNonAPets

    integer :: petCount, localPet, ssiId, accDeviceCount, ssiIdLocalAPetCount
    integer, allocatable, dimension(:) :: ssiIdLocalCommAccDeviceCounts
    integer :: esmfVMComm, vmComm, ssiIdLocalComm
    integer :: vmCommRank, ssiIdLocalCommSize, ssiIdLocalCommRank
    integer :: i, j, k

    nDevAPet = 0
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=esmfVMComm, rc=rc)
    if(rc .ne. ESMF_SUCCESS) then
      print *, "Getting local pet id  and comm failed"
      return
    endif

    call MPI_Comm_dup(esmfVMComm, vmComm, rc)

    call ESMF_VMGet(vm, localPet, accDeviceCount=accDeviceCount, ssiId=ssiId, rc=rc)
    if(rc .ne. ESMF_SUCCESS) then
      print *, "Quering VM for accelerator device count and ssiId failed"
      return
    endif

    print *, "PET=", localPet, ", Num devices =", accDeviceCount, ", ssid=", ssiId

    call MPI_Comm_split(vmComm, ssiId, 0, ssiIdLocalComm, rc)

    isAccPet(1) = 0
    call MPI_Comm_size(ssiIdLocalComm, ssiIdLocalCommSize, rc)
    call MPI_Comm_rank(ssiIdLocalComm, ssiIdLocalCommRank, rc)
    allocate(ssiIdLocalCommAccDeviceCounts(ssiIdLocalCommSize))
    call MPI_Allgather(accDeviceCount, 1, MPI_INTEGER, ssiIdLocalCommAccDeviceCounts, 1, MPI_INTEGER, ssiIdLocalComm, rc)

    ! FIXME: We don't need ssiIdLocalCommAccDeviceCounts, anymore - delete it
    if(accDeviceCount > 0) then
      if(partStrategy == ESMF_ACC_PET_PARTITION_CONTIG) then
        ! The first (lowest ranked) processes with access to device gets to use it
        if(ssiIdLocalCommRank < accDeviceCount) then
          isAccPet(1) = 1
        end if
      else if(partStrategy == ESMF_ACC_PET_PARTITION_EVEN_STRIDED) then
        ! The first (lowest ranked) even processes with access to device gets to use it
        if((mod(ssiIdLocalCommRank, 2) == 0) .and.&
            (ssiIdLocalCommRank/2 < accDeviceCount)) then
          isAccPet(1) = 1
        end if
      else if(partStrategy == ESMF_ACC_PET_PARTITION_LOWEST_RANK) then
        ! The first (lowest ranked) process with access to device gets to use it
        if(ssiIdLocalCommRank == 0) then
          isAccPet(1) = 1
        end if
      else
        print *, "ERROR: Unrecognized partition strategy"
        return
      end if
    end if

    ! Typical scenarios,
    ! 1) All MPI ranks on a node have access to the same devices OR
    ! 2) One MPI rank on each node has access to the devices
    call MPI_Allreduce(isAccPet, ssiIdLocalAPetCount, 1, MPI_INTEGER, MPI_MAX, ssiIdLocalComm, rc)
    call MPI_Comm_free(ssiIdLocalComm, rc)

    allocate(isAccPetList(petCount))
    call ESMF_VMAllGather(vm, isAccPet, isAccPetList, 1, rc=rc)
    if(rc .ne. ESMF_SUCCESS) then
      deallocate(isAccPetList)
      print *, "Unable to gather list of partitioned pets"
      return
    endif

    numAPets = 0
    do i=1, petCount
      if (isAccPetList(i) == 1) then
        numAPets = numAPets + 1
      endif
    end do
    numNonAPets = petCount - numAPets
    
    if(numAPets /= 0) then
      ! FIXME: We assume that all APets on a node have access to the same
      ! number of devices and that the devices are uniformly distributed
      ! (no heterogeneous nodes)
      nDevAPet = accDeviceCount / ssiIdLocalAPetCount
      if(nDevAPet == 0) then
        print *, "WARNING: Assumption that each apet has atleast 1 dev failed"
        nDevAPet = 1
      end if
      ! Partition petlist [0:petCount) to aPets = accelerated pets
      ! and nonAPets = non-accelerated pets
      allocate(aPets(numAPets))
      allocate(nonAPets(numNonAPets))

      j = 1
      k = 1
      do i=1, petCount
        if ((isAccPetList(i) == 1) .and. (j .le. numAPets)) then
          aPets(j) = i - 1
          j = j + 1
        else
          nonAPets(k) = i - 1
          k = k + 1
        endif
      end do
    else
      nDevAPet = 0
      ! nonAPets = petList [0:petCount)
      ! allocate(aPets(petCount))
      allocate(aPets(0))
      allocate(nonAPets(petCount))
      do i=1, petCount
        !aPets(i) = i-1
        nonAPets(i) = i-1
      end do
    endif
    call MPI_Comm_rank(vmComm, vmCommRank, rc)
    if(vmCommRank == 0) then
      PRINT *, "======================"
      PRINT *, "PARTITION INFO"
      PRINT *, "======================"
      WRITE(*,"(A20)",ADVANCE="NO") "Acc pets are:"
      do i=1, numAPets
        WRITE(*,"(I5,A)",ADVANCE="NO") aPets(i)
      end do
      PRINT *, "."
      PRINT *, "======================"
      WRITE(*,"(A20)", ADVANCE="NO") "Non Acc pets are:"
      do i=1, numNonAPets
        WRITE(*,"(I5,A)", ADVANCE="NO") nonAPets(i)
      end do
      PRINT *, "."
      PRINT *, "======================"
    endif

    deallocate(isAccPetList)
    
  end subroutine partition_pets

   function qsort_partition(array, left, right, pindex)
      integer, pointer :: array(:)
      integer, intent(in)    :: left, right, pindex

      integer :: partition

      integer :: pvalue, tmp, sindex, i

      pvalue = array(pindex)
      tmp = array(right)
      array(right) = pvalue
      array(pindex) = tmp

      sindex = left

      do i = left, right-1
          if(array(i) .le. pvalue) then
              tmp = array(i)
              array(i) = array(sindex)
              array(sindex) = tmp
              sindex = sindex + 1
          endif
      end do

      tmp = array(sindex)
      array(sindex) = array(right)
      array(right) = tmp
      partition = sindex
  end function qsort_partition

  recursive subroutine qsort(array, left, right)
    integer, pointer :: array(:)
    integer, intent(in)    :: left, right

    integer :: pindex, npindex
    if(right .gt. left) then
      pindex = left + (right - left)/2
      npindex = qsort_partition(array, left, right, pindex)
      call qsort(array, left, npindex-1)
      call qsort(array, npindex+1, right)
    endif
  end subroutine qsort

  ! Computes minuend - subtrahend = diff && unique(diff)
  subroutine subtract_pets(minuend, subtrahend, diff, rc)
    use ESMF_UtilSortMod

    integer, intent(in) :: minuend(:)
    integer, intent(in) :: subtrahend(:)
    integer, allocatable, intent(inout) :: diff(:)
    integer, intent(out) :: rc

    integer :: minuend_sz, subtrahend_sz, diff_sz

    integer, allocatable, target :: tmp_minuend(:)
    integer, allocatable, target :: tmp_subtrahend(:)
    integer, dimension(:), pointer :: tmp_ptr

    logical, allocatable :: diff_mask(:)

    integer :: i, j

    minuend_sz = size(minuend)
    subtrahend_sz = size(subtrahend)
    diff_sz = 0

    rc = ESMF_SUCCESS
    allocate(tmp_minuend(minuend_sz))
    tmp_minuend = minuend
    allocate(tmp_subtrahend(subtrahend_sz))
    tmp_subtrahend = subtrahend

    tmp_ptr => tmp_minuend
    call qsort(tmp_ptr, 1, minuend_sz)
    tmp_ptr => tmp_subtrahend
    call qsort(tmp_ptr, 1, subtrahend_sz)

    allocate(diff_mask(minuend_sz))
    diff_mask = .false.
    diff_sz = 0

    i = 1
    j = 1
    do while( (i <= minuend_sz) .and. (j <= subtrahend_sz))
      if(tmp_minuend(i) /= tmp_subtrahend(j)) then
        diff_mask(i) = .true.
        diff_sz = diff_sz + 1
      end if
      i = i + 1
      j = j + 1
      do while((i <= minuend_sz) .and. (tmp_minuend(i) == tmp_minuend(i-1)))
        i = i + 1
      end do
      do while((j <= subtrahend_sz) .and. (tmp_subtrahend(i) == tmp_subtrahend(i-1)))
        j = j + 1
      end do
    end do

    allocate(diff(diff_sz))
    j = 1
    do i=1,minuend_sz
      if(diff_mask(i)) then
        diff(j) = tmp_minuend(i)
        j = j + 1
      end if
    end do

    deallocate(tmp_subtrahend)
    deallocate(tmp_minuend)

  end subroutine subtract_pets

    ! This subroutine partitions the global PET list based on the information
    ! available about the various components
    ! vm : ESMF VM
    ! comp_info(NCOMPS) : Each entry in the array, corresponding to a component,
    !   indicates whether the component can/must/cannot use accelerator devices.
    ! comp_pets_info(NCOMPS) : Each entry in the array, corresponding to a component,
    !   contains PET list related info (PET list, device id list) that can be used 
    !   for the component
    ! partStrategy : Indicates the strategy on how to partition the global PET list
    !   Available partition strategies are:
    !   ESMF_ACC_PET_PARTITION_CONTIG => accelerated devices are
    !     available from the lowest ranked MPI processes (rank < number of
    !     devices)
    !   ESMF_ACC_PET_PARTITION_EVEN_STRIDED => accelerated devices are
    !    available from the lowest ranked even MPI processes
    !   ESMF_ACC_PET_PARTITION_LOWEST_RANK => accelerated devices are
    !     available from the lowest ranked process (single process)
    !     on each node with access to a device
    ! is_concurrent : Set to .true. for concurrent components, .false. otherwise
    ! rc : return code
    subroutine partition_pet_global_list(vm, comp_info, comp_pets_info, part_strategy, is_concurrent, rc)

      type(ESMF_VM), intent(in) :: vm
      integer, intent(in) :: comp_info(:)
      type(ESMF_PetListInfo), allocatable, intent(inout) :: comp_pets_info(:)
      integer, intent(in) :: part_strategy
      logical, intent(in) :: is_concurrent
      integer, intent(out) :: rc

      integer :: total_comps, ncomps_no_acc, ncomps_can_acc, ncomps_must_acc
      integer :: ndev_apet, ndev_idx
      integer :: naccs, nnon_accs, tmp_rem_accs, comp_pet_sz
      integer :: i, j, k, l, pet_count, tmp_pet_sidx, tmp_pet_eidx
      integer, allocatable :: apets(:), nonapets(:)
    

      ! Get Pet count from VM
      call ESMF_VMGet(vm, petCount=pet_count, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Unable to petcount from VM"
        return
      end if

      ! Partition the global pet list into apets : pets with access to accelerators
      ! and nonapets : pets without (may not) access to accelerators
      ! ndev_apet --> number of accelerator devices accessible from a apet
      call partition_pets(vm, apets, nonapets, ndev_apet, part_strategy, rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "ERROR: Could not partition pets to acc and nonacc"
        return
      end if
      print *, "After partition : ", size(apets), " acc pets and ", size(nonapets), " non-acc pets ", ndev_apet, " devices/acc pet"

      ! Find number of components that can/must/cannot use accelerators
      ncomps_can_acc = 0
      ncomps_must_acc = 0
      ncomps_no_acc = 0

      do i=1,size(comp_info)
        if(comp_info(i) == ESMF_COMP_NO_ACC) then
          ncomps_no_acc = ncomps_no_acc + 1
        else if(comp_info(i) == ESMF_COMP_CAN_ACC) then
          ncomps_can_acc = ncomps_can_acc + 1
        else if(comp_info(i) == ESMF_COMP_MUST_ACC) then
          ncomps_must_acc = ncomps_must_acc + 1
        else
          print *, "ERROR: Unrecognized acc info"
        end if
      end do

      print *, "comps(noacc, canacc, mustacc) =", ncomps_no_acc, ncomps_can_acc, ncomps_must_acc

      ! naccs -> total number of components that will be allocated apets
      ! = all components that must use + some/all components that can use
      naccs = 0
      nnon_accs = ncomps_no_acc
      tmp_rem_accs = size(apets)
      if(ncomps_must_acc > 0) then
        if(size(apets) == 0) then
          print *, "ERROR: Atleast one comp MUST ACC but no acc devices available"
          rc = ESMF_FAILURE
          return
        end if
        if(ncomps_must_acc < size(apets)) then
          print *, "ERROR: ", ncomps_must_acc, " comps MUST ACC but no enough PETs available"
          rc = ESMF_FAILURE
          return
        end if
        naccs = ncomps_must_acc
        tmp_rem_accs = tmp_rem_accs - ncomps_must_acc
      end if

      if(ncomps_can_acc > 0) then
        if(size(apets) == 0) then
          print *, "WARNING: Atleast one comp CAN ACC but no acc devices available"
          nnon_accs = nnon_accs + ncomps_can_acc
        else
          if(tmp_rem_accs > 0) then
            if(tmp_rem_accs > ncomps_can_acc) then
                naccs = naccs + ncomps_can_acc
                tmp_rem_accs = tmp_rem_accs - ncomps_can_acc
            else
                naccs = naccs + tmp_rem_accs
                tmp_rem_accs = 0
            end if
          end if
        end if
      end if

      ! Allocate apets/nonapets among the different components
      if(naccs > 0) then
        ! Divide comp pets evenly among apets
        comp_pet_sz = size(apets)/naccs
        print *, "DEBUG: Size of each acc comp petlist = ", comp_pet_sz
        ndev_idx = 0
        tmp_pet_sidx = 1
        tmp_pet_eidx = comp_pet_sz
        do i=1,size(comp_info)
          ! First allocate for all MUST ACC
          if(comp_info(i) == ESMF_COMP_MUST_ACC) then
            allocate(comp_pets_info(i)%pet_list(comp_pet_sz))
            allocate(comp_pets_info(i)%ldevice_list(comp_pet_sz * ndev_apet))
            allocate(comp_pets_info(i)%gdevice_list(comp_pet_sz * ndev_apet))
            k = 1
            do j=tmp_pet_sidx,tmp_pet_eidx
              comp_pets_info(i)%pet_list(k) = apets(j)
              do l=1,ndev_apet
                comp_pets_info(i)%ldevice_list(k+l-1) = l-1
                comp_pets_info(i)%gdevice_list(k+l-1) = ndev_idx
                ndev_idx = ndev_idx + 1
              end do
              k = k + 1
            end do
            if(is_concurrent) then
              tmp_pet_sidx = tmp_pet_sidx + comp_pet_sz
              tmp_pet_eidx = tmp_pet_eidx + comp_pet_sz
            end if
            naccs = naccs - 1
          end if
        end do
        do i=1,size(comp_info)
          ! Now allocate for CAN_ACCs
          if(naccs <= 0) then
            exit
          end if
          if(comp_info(i) == ESMF_COMP_CAN_ACC) then
            allocate(comp_pets_info(i)%pet_list(comp_pet_sz))
            allocate(comp_pets_info(i)%ldevice_list(comp_pet_sz * ndev_apet))
            allocate(comp_pets_info(i)%gdevice_list(comp_pet_sz * ndev_apet))
            k = 1
            do j=tmp_pet_sidx,tmp_pet_eidx
              comp_pets_info(i)%pet_list(k) = apets(j)
              do l=1,ndev_apet
                comp_pets_info(i)%ldevice_list(k+l-1) = l-1
                comp_pets_info(i)%gdevice_list(k+l-1) = ndev_idx
                ndev_idx = ndev_idx + 1
              end do
              k = k + 1
            end do
            if(is_concurrent) then
              tmp_pet_sidx = tmp_pet_sidx + comp_pet_sz
              tmp_pet_eidx = tmp_pet_eidx + comp_pet_sz
            end if
            naccs = naccs - 1
          end if
        end do
      end if
      if(nnon_accs > 0) then
        comp_pet_sz = size(nonapets)/nnon_accs
        print *, "DEBUG: Size of each non-acc comp petlist = ", comp_pet_sz
        tmp_pet_sidx = 1
        tmp_pet_eidx = comp_pet_sz
        do i=1,size(comp_info)
          if(comp_info(i) == ESMF_COMP_NO_ACC) then
            allocate(comp_pets_info(i)%pet_list(comp_pet_sz))
            k = 1
            do j=tmp_pet_sidx,tmp_pet_eidx
              comp_pets_info(i)%pet_list(k) = nonapets(j)
              k = k + 1
            end do
            if(is_concurrent) then
              tmp_pet_sidx = tmp_pet_sidx + comp_pet_sz
              tmp_pet_eidx = tmp_pet_eidx + comp_pet_sz
            end if
            nnon_accs = nnon_accs - 1
          end if
        end do
        if(nnon_accs > 0) then
          print *, "DEBUG: Handling CAN_ACC components (that don't have associated acc devices)"
          do i=1,size(comp_info)
            if(comp_info(i) == ESMF_COMP_CAN_ACC) then
              allocate(comp_pets_info(i)%pet_list(comp_pet_sz))
              k = 1
              do j=tmp_pet_sidx,tmp_pet_eidx
                comp_pets_info(i)%pet_list(k) = nonapets(j)
                k = k + 1
              end do
              if(is_concurrent) then
                tmp_pet_sidx = tmp_pet_sidx + comp_pet_sz
                tmp_pet_eidx = tmp_pet_eidx + comp_pet_sz
              end if
              nnon_accs = nnon_accs - 1
            end if
          end do
        end if
      end if
    end subroutine partition_pet_global_list

  end module pet_partitioner

!\end{verbatim}


