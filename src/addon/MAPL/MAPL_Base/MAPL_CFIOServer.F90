! +-======-+ 
!  Copyright (c) 2003-2007 United States Government as represented by 
!  the Admistrator of the National Aeronautics and Space Administration.  
!  All Rights Reserved.
!  
!  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
!  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
!  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
!  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
!  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
!  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
!  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
!  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
!  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
!  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
!  
!  Government Agency: National Aeronautics and Space Administration
!  Government Agency Original Software Designation: GSC-15354-1
!  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
!  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
!  Government Agency Point of Contact for Original Software:  
!  			Dale Hithon, SRA Assistant, (301) 286-2691
!  
! +-======-+ 
#include "MAPL_Generic.h"

   MODULE MAPL_CFIOServerMod

   use MAPL_BaseMod
   use esmf
   use ESMF_CFIOUtilMod
   use MAPL_MemUtilsMod
   use netcdf

   implicit none
   private

   integer, parameter, public :: MAPL_TAG_GETWORK = 9001! history to master
   integer, parameter, public :: MAPL_TAG_WORKERDONE = 9002 ! worker to master
   integer, parameter, public :: MAPL_TAG_NORMALEXIT = 9003 ! history finalize or abort to master
   integer, parameter, public :: MAPL_TAG_WORKEREXIT = 9004 ! master to worker for ending or aborting
   integer, parameter, public :: MAPL_TAG_CLIENTINFO = 9005 ! master to worker for starting new colleciton
   integer, parameter, public :: MAPL_TAG_WORKERINFO = 9006 ! master to history
   integer, parameter, public :: MAPL_TAG_SHIPINFO = 9007 ! history to worker
   integer, parameter, public :: MAPL_TAG_WORKEREXITED = 9008
   integer, parameter, public :: MAPL_TAG_SHIPLEVEL = 9009 ! history to worker
   integer, parameter, public :: MAPL_TAG_SHIPDATA = 9010 ! history to worker

   integer, public            :: mpi_io_server_info_type

   public MAPL_CFIOServerMaster
   public MAPL_CFIOServerWorker
   public MAPL_CFIOServerInitMpiTypes
   public MAPL_CFIOServerIOinfo
   public MAPL_CFIOServerGetFreeNode
   public MAPL_CFIOServerStart

   interface MAPL_CFIOServerGetFreeNode
      module procedure MAPL_CFIOServerGetFreeNode
   end interface

   interface MAPL_CFIOServerStart
      module procedure MAPL_CFIOServerStart
   end interface

   interface MAPL_CFIOServerInitMpiTypes
      module procedure MAPL_CFIOServerInitMpiTypes
   end interface

   type MAPL_CFIOServerIOinfo
      character(len=ESMF_MAXSTR) :: filename
      integer             :: nlevs
      integer             :: lons
      integer             :: lats
      integer             :: date
      integer             :: time
      integer             :: markdone
   end type MAPL_CFIOServerIOinfo
  
   include "mpif.h"

   contains

   subroutine MAPL_CFIOServerStart(mapl_comm,rc)

   type(MAPL_Communicators), intent(inout) :: mapl_comm
   integer, optional,        intent(out  ) :: rc

   character(len=ESMF_MAXSTR)        :: Iam
   integer                    :: status

   integer :: ioRank
   real    :: lmem, gmem

   Iam = "MAPL_CFIOServerStart"
   call MPI_comm_rank(mapl_comm%iocomm,ioRank,status)
   VERIFY_(STATUS)

   ! check that we have enough memory on each node
   call MAPL_MemUtilsFree(lmem)
   call MPI_Allreduce(lmem, gmem, 1, MPI_REAL, MPI_MAX, mapl_comm%iocomm,status)
   VERIFY_(STATUS)

   ! The next assert is a check that you are not requesting more than the available memory
   ASSERT_(gmem > mapl_comm%maxmem) 

   if (ioRank == 0) then
      call MAPL_CFIOServerMaster(mapl_comm,rc=status)
      VERIFY_(STATUS)
   else
      call MAPL_CFIOServerWorker(mapl_comm,rc=status)
      VERIFY_(STATUS)
   end if

   end subroutine MAPL_CFIOServerStart

   subroutine MAPL_CFIOServerMaster(mapl_comm,rc)

   type(MAPL_Communicators), intent(inout) :: mapl_comm
   integer, optional,        intent(out  ) :: rc

   character(len=ESMF_MAXSTR)        :: Iam
   integer                    :: status

    logical :: done
    integer :: globalComm
    integer :: source, tag
    integer :: stat(MPI_STATUS_SIZE)
    integer :: current_work_load
    integer,allocatable :: globalRank(:)
    integer :: incomingRank
    integer :: rank
    integer :: queueExit(1)
    integer, allocatable :: workQueue(:)
    integer, allocatable :: workQueueSize(:)
    integer :: n_workers, CoresPerNode,num_nodes,MAX_WORK_REQUESTS
    integer :: i,j,k
    integer :: num_q_exit, num_q_workRequests,sender
    logical, allocatable :: freeWorkers(:)
    integer, allocatable :: loadPerNode(:) ! amount of memory being used per node in megabyte
    integer, allocatable :: WorkerNode(:) ! node id of each worker
    integer :: wsize,maxMem,worker,workerGlobalId
    integer :: lastNodeUsed
   
   Iam = "MAPL_CFIOServerMaster"

    n_workers = mapl_comm%iocommSize-1
    allocate(globalRank(0:n_workers),stat=status)
    VERIFY_(STATUS)
    CoresPerNode = mapl_comm%corespernode
    num_nodes = mapl_comm%iocommsize/corespernode
    MAX_WORK_REQUESTS = 50
    maxMem = mapl_comm%maxMem
    lastNodeUsed = 0

    ! translation table between 1-n_workers in global comm
    do i=0,n_workers
       globalRank(i)=mapl_comm%esmfcommsize+i
    enddo

    globalComm = mapl_comm%maplcomm ! we should change it to the global communicator that has been passed

    current_work_load = 0

    num_q_exit = 0
    num_q_workRequests = 0

    allocate(workQueue(MAX_WORK_REQUESTS), stat=status)
    VERIFY_(status)
    allocate(workQueueSize(MAX_WORK_REQUESTS), stat=status)
    VERIFY_(status)
    allocate(loadPerNode(num_nodes),stat=status)
    VERIFY_(status)
    loadPerNode = 0

    allocate(freeWorkers(n_workers), stat=status)
    VERIFY_(status)
    freeWorkers = .true.

    allocate(WorkerNode(n_workers),stat=status)
    VERIFY_(STATUS)
    do i=1,CoresPerNode-1
       WorkerNode(i)=1
    enddo
    k=0
    if (num_nodes > 1) then
       do i=1,num_nodes-1
          do j=1,CoresPerNode
             k=k+1
             WorkerNode(CoresPerNode-1+k)=i+1
          enddo
       enddo
    endif
       
    DONE = .false.

    ! Polling loop (we exit this ONLY at the end)
    do while (.not. DONE)
       call MPI_Recv(wsize, 1, MPI_INTEGER, &
            MPI_ANY_SOURCE, MPI_ANY_TAG, globalComm, &
            stat, status)
       VERIFY_(STATUS)

       ! inspect status object for msg_tag and source
       tag = stat(MPI_TAG)
       source = stat(MPI_SOURCE)

     select case(tag)
        case (MAPL_TAG_GETWORK)
           ! increment current_work_load
           current_work_load = current_work_load + 1
           call get_worker(worker,wsize) 
           if (worker == -1) then 
              call queue_work_request(source,wsize)
           else
              workerGlobalId = globalRank(worker)
              call assign_work(source,workerGlobalId,wsize,worker)
           end if

        case (MAPL_TAG_WORKERDONE)
           incomingRank = source-mapl_comm%esmfCommSize
           ! return resources
           freeWorkers(incomingRank) = .true.
           loadPerNode(workerNode(incomingRank)) = loadPerNode(workerNode(incomingRank)) - wsize 
           ! decrement current_work_load
           current_work_load = current_work_load - 1
           ! check if there are any queued work requests, fullfill as many as possible
           if (num_q_workRequests > 0) then
              call getInfoFromQueue(wsize) 
              call get_worker(worker,wsize)
              if (worker /=  -1) then
                 call getWorkFromQueue(rank,wsize)
                 call assign_work(rank,workerGlobalId,wsize,worker)
              end if
           else
              ! check for queued exitRequests

              if (num_q_exit > 0) then

                 sender = queueExit(1)
                 num_q_exit =  num_q_exit - 1
                 ASSERT_(num_q_exit == 0)
                 call process_normal_exit(sender)
              end if
           end if
        case (MAPL_TAG_NORMALEXIT)
           if (current_work_load==0) then
              call process_normal_exit(source)
           else
              ! queue exit_request
              ASSERT_(num_q_exit == 0)

              num_q_exit = num_q_exit + 1
              queueExit(num_q_exit) = source
           endif
        case default
     end select
  end do

  deallocate(workQueue)
  deallocate(workQueuesize)
  deallocate(freeWorkers)
  deallocate(globalRank)
  deallocate(loadPerNode)

contains
  subroutine assign_work(rank,worker,wsize,localId,rc)
    integer :: rank
    integer :: worker
    integer :: wsize
    integer :: localId
    integer, optional, intent(out  ) :: rc

    integer :: status

        

    ! reply to History (collection server)
    loadPerNode(WorkerNode(localId)) = loadPerNode(WorkerNode(localId)) + wsize
    call MPI_Send(worker, 1, MPI_INTEGER, &
         rank, MAPL_TAG_WORKERINFO, globalComm, status)
    VERIFY_(status)
   
    !send message to worker with collection root ID
    call MPI_Send(rank, 1, MPI_INTEGER, &
         worker, MAPL_TAG_CLIENTINFO, globalComm, status)
    VERIFY_(status)


  end subroutine assign_work

  subroutine getWorkFromQueue(rank,wsize)
    integer :: rank
    integer :: wsize

    integer :: i

    rank = workQueue(1)
    wsize = workQueueSize(1)
    ! adjust the work queue
    do i = 2, num_q_workRequests
       workQueue(i-1) = workQueue(i)
       workQueueSize(i-1) = workQueueSize(i)
    end do
    workQueue(num_q_workRequests) = -1 ! invalid

    num_q_workRequests = num_q_workRequests - 1
  end subroutine getWorkFromQueue

  subroutine GetInfoFromQueue(wsize)
    integer :: wsize
    wsize = workQueueSize(1)
  end subroutine getInfoFromQueue

  subroutine queue_work_request(rank,worksize)
    integer :: rank
    integer :: worksize

    integer :: i

    ASSERT_(num_q_workRequests < MAX_WORK_REQUESTS)

    num_q_workRequests = num_q_workRequests + 1

    workQueue(num_q_workRequests) = rank
    workQueueSize(num_q_workRequests) = worksize

  end subroutine queue_work_request

  subroutine process_normal_exit(rank)
    integer :: rank

    integer :: i

    ! loop over all workers
    do i = 1, n_workers
       call MPI_Send(globalRank(0), 1, MPI_INTEGER, &
            globalRank(i), MAPL_TAG_WORKEREXIT, globalComm, status)
       VERIFY_(status)
    end do
    ! wait for their reply
    do i = 1, n_workers
       call MPI_Recv(rank, 1, MPI_INTEGER, &
            globalRank(i), MAPL_TAG_WORKEREXITED, globalComm, &
            MPI_STATUS_IGNORE, status)
       VERIFY_(status)
    end do

    DONE = .true.
    ! reply to HISTORY, send to root?
    call MPI_Send(rank, 1, MPI_INTEGER, &
         0, MAPL_TAG_WORKEREXIT, globalComm, status)
    VERIFY_(status)

  end subroutine process_normal_exit

  subroutine get_worker(worker,worksize)
   integer :: worker
   integer :: worksize
   integer :: i,cwork,k
 
   worker = -1
   NODELOOP: do j = lastNodeUsed+1,lastNodeUsed+num_nodes
      k=mod(j-1,num_nodes)+1
      cwork = worksize + loadPerNode(k)
      if (cwork < maxMem) then
         WORKERLOOP: do i=1,n_workers
            if (freeWorkers(i) .and. WorkerNode(i)==k ) then
               worker = i
               freeWorkers(i) = .false.
               lastNodeUsed = k
               exit NODELOOP
            end if
         end do WORKERLOOP
      end if
   end do NODELOOP 
  
   end subroutine get_worker

   end subroutine MAPL_CFIOServerMaster

   subroutine MAPL_CFIOServerWorker(mapl_comm,rc)
   
   type(MAPL_Communicators),   intent(inout) :: mapl_comm
   integer, optional,          intent(out  ) :: rc
 
   character(len=ESMF_MAXSTR)    :: Iam
   integer                :: status

   integer                :: rank
   integer                :: mpistatus(MPI_STATUS_SIZE)

   type(MAPL_CFIOServerIOInfo) :: ioinfo
   integer                    :: i, ncid, IM, JM, nymd, nhms, k
   real, allocatable              :: buffer(:,:,:)
   real, allocatable          :: lbuff(:,:)
   character(len=ESMF_MAXSTR), allocatable :: vnames(:)
   integer, allocatable             :: levs(:)
   integer                          :: levsize
   integer                          :: wsize,csize
   real                             :: rwsize
   integer :: globalComm
   integer, allocatable :: krank(:)

   Iam = "MAPL_CFIOServerIOWorker"
 
   globalComm = mapl_comm%maplcomm

   do while(.true.)

      ! receive a message from the master on the io communicator
      ! if it is to tell us to start work then we will look for messages
      ! from the rank we are delivered and do work
      ! if it is telling us to end the rank is irrelevant
      call MPI_RECV(rank,1,MPI_INTEGER,mapl_comm%iocommroot,MPI_ANY_TAG,globalComm,mpistatus,status)
      VERIFY_(STATUS)

      if (mpistatus(MPI_TAG) == MAPL_TAG_CLIENTINFO) then

         ! recieve first batch of information from history to know the file to open 
         ! and how many levels we will be recieving

         call MPI_RECV(ioinfo,1,mpi_io_server_info_type,rank,MAPL_TAG_SHIPINFO, &
            globalcomm,MPI_STATUS_IGNORE,status)
         VERIFY_(STATUS)

         IM = ioinfo%lons
         JM = ioinfo%lats
         levsize = IM*JM
         nymd = ioinfo%date
         nhms = ioinfo%time
         allocate(buffer(im,jm,ioinfo%nlevs),stat=status)
         VERIFY_(STATUS)
         allocate(lbuff(im,jm),stat=status)
         VERIFY_(STATUS)
         allocate(levs(ioinfo%nlevs),stat=status)
         VERIFY_(STATUS)
         allocate(vnames(ioinfo%nlevs),stat=status)
         VERIFY_(STATUS)
         allocate(krank(ioinfo%nlevs),stat=status)
         VERIFY_(STATUS)
         call MPI_RECV(krank,ioinfo%nlevs,MPI_INTEGER,rank,MAPL_TAG_SHIPINFO, &
            globalcomm,MPI_STATUS_IGNORE,status)
         VERIFY_(STATUS) 
         call MPI_RECV(levs,ioinfo%nlevs,MPI_INTEGER,rank,MAPL_TAG_SHIPINFO, &
            globalcomm,MPI_STATUS_IGNORE,status)
         VERIFY_(STATUS)
         csize = ESMF_MAXSTR*ioinfo%nlevs
         call MPI_RECV(vnames,csize,MPI_CHARACTER,rank,MAPL_TAG_SHIPINFO, &
            globalcomm,MPI_STATUS_IGNORE,status)
         VERIFY_(STATUS)

         ! process the levels
         do i=1,ioinfo%nlevs
            call MPI_RECV(lbuff,levsize,MPI_REAL,krank(i),MAPL_TAG_SHIPDATA, &
               globalComm, mpistatus,status)
            VERIFY_(STATUS)
            buffer(:,:,i)=lbuff
         enddo

         ! open the file
         status = nf90_open(trim(ioinfo%filename),NF90_WRITE,ncid)
         VERIFY_(STATUS)

         do i=1,ioinfo%nlevs
            call CFIO_PutVar(ncid,trim(vnames(i)),nymd,nhms, &
                 IM,JM,levs(i),1,buffer(:,:,i),status)
            VERIFY_(STATUS)
         enddo
        
         ! all done close file
         status = NF90_CLOSE(ncid)
         VERIFY_(STATUS)

         deallocate(buffer)
         deallocate(lbuff)
         deallocate(levs)
         deallocate(vnames)
         deallocate(krank)

         ! send message to acknowledge we are done, will send my rank in io communicator
         rwsize = ioinfo%nlevs*levsize*4/1024/1024
         wsize = ceiling(rwsize)
         call MPI_SEND(wsize,1,MPI_INTEGER,mapl_comm%iocommroot,MAPL_TAG_WORKERDONE,globalComm,status)
         VERIFY_(STATUS)

      else if (mpistatus(MPI_TAG) == MAPL_TAG_WORKEREXIT) then

         rank = mapl_comm%myIoRank
         call MPI_Send(rank,1,MPI_INTEGER,mapl_comm%iocommroot,MAPL_TAG_WORKEREXITED,globalComm,status)
         VERIFY_(STATUS)
         exit

      end if 

   end do 

   RETURN_(ESMF_SUCCESS)

   end subroutine MAPL_CFIOServerWorker

   subroutine MAPL_CFIOServerInitMpiTypes(rc)
 
   integer, optional, intent(out) :: rc
   
   character(len=ESMF_MAXSTR) :: Iam

   integer :: icount,csize,status
   integer, allocatable, dimension(:) :: iblock, itype
   integer(KIND=MPI_ADDRESS_KIND), allocatable, dimension(:) :: idisp

   Iam = "MAPL_CFIOServerInitMpiTypes"

   icount =2

   allocate(iblock(icount),stat=status)
   VERIFY_(STATUS)
   allocate(idisp( icount),stat=status)
   VERIFY_(STATUS)
   allocate(itype( icount),stat=status)
   VERIFY_(STATUS)

   itype(1)  = MPI_CHARACTER
   itype(2)  = MPI_INTEGER

   iblock(1) = ESMF_MAXSTR
   iblock(2) = 6

   call MPI_TYPE_SIZE(MPI_CHARACTER,csize,status)
   VERIFY_(STATUS)
   idisp(1)=0
   idisp(2)=idisp(1) + iblock(1)*csize

   call MPI_TYPE_CREATE_STRUCT(icount,iblock,idisp,itype,mpi_io_server_info_type,status)
   VERIFY_(STATUS)
   call MPI_TYPE_COMMIT(mpi_io_server_info_type,status)
   VERIFY_(STATUS)

   deallocate(iblock)
   deallocate(idisp)
   deallocate(itype)

   RETURN_(ESMF_SUCCESS)

   end subroutine MAPL_CFIOServerInitMpiTypes

   ! only collection root should call this
   subroutine MAPL_CFIOServerGetFreeNode(mapl_comm,IOnode,im,jm,nslices,rc)

   type(MAPL_Communicators), intent(in   ) :: mapl_comm
   integer,                  intent(inout) :: IOnode
   integer,                  intent(in   ) :: IM
   integer,                  intent(in   ) :: JM
   integer,                  intent(in   ) :: nslices
   integer, optional,        intent(out  ) :: rc

   integer :: status
   character(len=ESMF_MAXSTR) :: Iam
   integer :: comm, dest,wsize
   real :: rwsize

   Iam = "MAPL_CFIOServerGetFreeNode"

   dest = mapl_comm%ioCommRoot
   comm = mapl_comm%maplcomm
   rwsize = nslices*IM*JM*4/1024/1024
   wsize = ceiling(rwsize)
   call MPI_Send(wsize,1,MPI_INTEGER,dest, &
       MAPL_TAG_GETWORK,comm,status)
   VERIFY_(STATUS)
   call MPI_Recv(IOnode,1,MPI_INTEGER,mapl_comm%iocommroot, &
       MAPL_TAG_WORKERINFO,comm,MPI_STATUS_IGNORE,status)
   VERIFY_(STATUS)
      
   RETURN_(ESMF_SUCCESS)
 
   end subroutine MAPL_CFIOServerGetFreeNode
    

   end module MAPL_CFIOServerMod

   
