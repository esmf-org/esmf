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

#define SHM_SUCCESS  0
#define VERIFY_(A)   if((A)/=SHM_SUCCESS) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define ASSERT_(A)   if(.not.(A)) then; if(present(rc)) rc=1; PRINT *, Iam, __LINE__; return; endif
#define RETURN_(A)   if(present(rc)) rc=A; return

  module MAPL_ShmemMod

    use, intrinsic :: ISO_C_BINDING

    implicit none
    private

    include 'mpif.h'

    public :: MAPL_GetNodeInfo
    public :: MAPL_CoresPerNodeGet
    public :: MAPL_InitializeShmem
    public :: MAPL_FinalizeShmem

    public :: MAPL_AllocNodeArray
    public :: MAPL_DeAllocNodeArray
    public :: MAPL_ShmemAmOnFirstNode

    integer, public, parameter :: MAPL_NoShm=255

    character*30 :: Iam="MAPL_ShmemMod in line "

    integer(c_int), parameter :: IPC_CREAT = 512
    integer(c_int), parameter :: IPC_RMID  = 0

    integer,        parameter :: CHUNK=256

    integer, public, save :: MAPL_NodeComm=-1
    integer, public, save :: MAPL_MyNodeNum=-1
    logical, public, save :: MAPL_AmNodeRoot=.false.
    logical, public, save :: MAPL_ShmInitialized=.false.

    integer,         save :: MAPL_CoresPerNodeUsed=-1
    integer,         save :: MAPL_CoresPerNodeMax=-1

    type Segment_T
       integer (c_int) :: shmid=-1
       type    (c_ptr) :: addr
    end type Segment_T

    type(Segment_T), pointer :: Segs(:) => NULL()
    type(Segment_T), pointer :: SegsNew(:)

    interface
       function shmget(key, size, shmflg) bind(c, name="shmget")
         use, intrinsic :: ISO_C_BINDING
         integer (c_int)              :: shmget
         integer (c_int),       value :: key
         integer (c_long_long), value :: size
         integer (c_int),       value :: shmflg
       end function shmget

       function shmat(shmid, shmaddr, shmflg) bind(c, name="shmat")
         use, intrinsic :: ISO_C_BINDING
         type (c_ptr)           :: shmat
         integer (c_int), value :: shmid
         type (c_ptr),    value :: shmaddr
         integer (c_int), value :: shmflg
       end function shmat

       function shmdt(shmaddr) bind(c, name="shmdt")
         use, intrinsic :: ISO_C_BINDING
         integer (c_int)     :: shmdt
         type (c_ptr), value :: shmaddr
       end function shmdt

       function shmctl(shmid, cmd, buf) bind(c, name="shmctl")
         use, intrinsic :: ISO_C_BINDING
         integer (c_int)        :: shctl
         integer (c_int), value :: shmid
         integer (c_int), value :: cmd
         type (c_ptr),    value :: buf
       end function shmctl

       subroutine perror(s) bind(c,name="perror")
         use, intrinsic :: ISO_C_BINDING
         character(c_char), intent(in) :: s(*)
       end subroutine perror

    end interface

    interface MAPL_AllocNodeArray
       module procedure MAPL_AllocNodeArray_1DI4
       module procedure MAPL_AllocNodeArray_2DI4
       module procedure MAPL_AllocNodeArray_3DI4
       module procedure MAPL_AllocNodeArray_1DR4
       module procedure MAPL_AllocNodeArray_2DR4
       module procedure MAPL_AllocNodeArray_3DR4
       module procedure MAPL_AllocNodeArray_1DR8
       module procedure MAPL_AllocNodeArray_2DR8
       module procedure MAPL_AllocNodeArray_3DR8
    end interface

    interface MAPL_DeAllocNodeArray
       module procedure MAPL_DeAllocNodeArray_1DI4
       module procedure MAPL_DeAllocNodeArray_2DI4
       module procedure MAPL_DeAllocNodeArray_3DI4
       module procedure MAPL_DeAllocNodeArray_1DR4
       module procedure MAPL_DeAllocNodeArray_2DR4
       module procedure MAPL_DeAllocNodeArray_3DR4
       module procedure MAPL_DeAllocNodeArray_1DR8
       module procedure MAPL_DeAllocNodeArray_2DR8
       module procedure MAPL_DeAllocNodeArray_3DR8
    end interface
    
  contains

    subroutine MAPL_GetNodeInfo(comm, rc)
      integer, optional, intent(IN ) :: comm
      integer, optional, intent(OUT) :: rc

      integer :: STATUS, RANK

      if (MAPL_NodeComm == -1) then ! make sure that we do this only once
         MAPL_NodeComm = getNodeComm(comm, rc=STATUS)
         VERIFY_(STATUS)

!        we store the global Max of CoresPerNode (until we implement vector)
         call MPI_AllReduce (MAPL_CoresPerNodeUsed, MAPL_CoresPerNodeMax, &
                             1, MPI_INTEGER, MPI_MAX, comm, status )
         VERIFY_(STATUS)

         call MPI_Comm_rank(MAPL_NodeComm, rank, STATUS)
         ASSERT_(STATUS==MPI_SUCCESS)

         MAPL_AmNodeRoot = rank==0
      end if
      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_GetNodeInfo

    subroutine MAPL_InitializeShmem(comm, rc)
      integer, optional, intent(IN ) :: comm
      integer, optional, intent(OUT) :: rc

      integer :: STATUS

      ASSERT_(MAPL_NodeComm /= -1)

      allocate(Segs(CHUNK),stat=STATUS)
      ASSERT_(STATUS==0)

      MAPL_ShmInitialized=.true.

#ifdef DEBUG
      if(MAPL_AmNodeRoot) &
           print *, "MAPL_Shmem initialized for node ", MAPL_MyNodeNum
#endif

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_InitializeShmem

    subroutine MAPL_FinalizeShmem(rc)
      integer, optional, intent(OUT) :: rc

      integer      :: STATUS, i
      type (c_ptr) :: buf

      do i=1,size(Segs)
         if(Segs(i)%shmid==-1) cycle

!!! Everyone detaches address from shared segment

         STATUS = shmdt(Segs(i)%addr)
         ASSERT_(STATUS /= -1)

!!! Make sure everyone has finished detaching

         call MPI_Barrier(MAPL_NodeComm, STATUS)
         ASSERT_(STATUS==MPI_SUCCESS)

!!! The root processor destroys the segment

         if (MAPL_AmNodeRoot) then
            STATUS = shmctl(Segs(i)%shmid, IPC_RMID, buf)
            ASSERT_(STATUS /= -1)
         end if
      end do

      if (associated(Segs)) then
         deallocate(Segs,stat=STATUS)
         ASSERT_(STATUS==0)
      end if

      MAPL_ShmInitialized=.false.

#ifdef DEBUG
      if(MAPL_AmNodeRoot) &
           print *, "MAPL_Shmem finalized for node ", MAPL_MyNodeNum
#endif

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_FinalizeShmem

    subroutine MAPL_DeAllocNodeArray_1DI4(Ptr,rc)
      integer,           intent(IN ) :: Ptr(:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr)

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_1DI4

    subroutine MAPL_DeAllocNodeArray_2DI4(Ptr,rc)
      integer,           intent(IN ) :: Ptr(:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr)

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_2DI4

    subroutine MAPL_DeAllocNodeArray_3DI4(Ptr,rc)
      integer,           intent(IN ) :: Ptr(:,:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr)

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_3DI4


    subroutine MAPL_DeAllocNodeArray_1DR4(Ptr,rc)
      real*4,            intent(IN ) :: Ptr(:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr)

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_1DR4

    subroutine MAPL_DeAllocNodeArray_2DR4(Ptr,rc)
      real*4,            intent(IN ) :: Ptr(:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr)

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_2DR4

    subroutine MAPL_DeAllocNodeArray_3DR4(Ptr,rc)
      real*4,            intent(IN ) :: Ptr(:,:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr)

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_3DR4


    subroutine MAPL_DeAllocNodeArray_1DR8(Ptr,rc)
      real*8,            intent(IN ) :: Ptr(:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr)

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_1DR8

    subroutine MAPL_DeAllocNodeArray_2DR8(Ptr,rc)
      real*8,            intent(IN ) :: Ptr(:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr)

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_2DR8

    subroutine MAPL_DeAllocNodeArray_3DR8(Ptr,rc)
      real*8,            intent(IN ) :: Ptr(:,:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr)

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_3DR8

    subroutine MAPL_AllocNodeArray_1DI4(Ptr, Shp, lbd, rc)
      integer, pointer,  intent(INOUT) :: Ptr(:)
      integer,           intent(IN   ) :: Shp(1)
      integer, optional, intent(IN   ) :: lbd(1)
      integer, optional, intent(  OUT) :: rc

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      len = shp(1)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      VERIFY_(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      ASSERT_(size(Ptr)==len)

!     if(present(lbd)) Ptr(lbd(1):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_1DI4


    subroutine MAPL_AllocNodeArray_2DI4(Ptr, Shp, lbd, rc)
      integer, pointer,  intent(INOUT) :: Ptr(:,:)
      integer,           intent(IN   ) :: Shp(2)
      integer, optional, intent(IN   ) :: lbd(2)
      integer, optional, intent(  OUT) :: rc

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      len=product(Shp)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      VERIFY_(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      ASSERT_(all(shape(Ptr)==Shp))

!     if(present(lbd)) Ptr(lbd(1):,lbd(2):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_2DI4

    subroutine MAPL_AllocNodeArray_3DI4(Ptr, Shp, lbd, rc)
      integer, pointer,  intent(INOUT) :: Ptr(:,:,:)
      integer,           intent(IN   ) :: Shp(3)
      integer, optional, intent(IN   ) :: lbd(3)
      integer, optional, intent(  OUT) :: rc

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      len=product(Shp)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      VERIFY_(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      ASSERT_(all(shape(Ptr)==Shp))

!     if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_3DI4


    subroutine MAPL_AllocNodeArray_1DR4(Ptr, Shp, lbd, rc)
      real*4, pointer,   intent(INOUT) :: Ptr(:)
      integer,           intent(IN   ) :: Shp(1)
      integer, optional, intent(IN   ) :: lbd(1)
      integer, optional, intent(  OUT) :: rc

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      len = shp(1)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      VERIFY_(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      ASSERT_(size(Ptr)==len)

!     if(present(lbd)) Ptr(lbd(1):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_1DR4


    subroutine MAPL_AllocNodeArray_2DR4(Ptr, Shp, lbd, rc)
      real*4, pointer,   intent(INOUT) :: Ptr(:,:)
      integer,           intent(IN   ) :: Shp(2)
      integer, optional, intent(IN   ) :: lbd(2)
      integer, optional, intent(  OUT) :: rc

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      len=product(Shp)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      VERIFY_(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      ASSERT_(all(shape(Ptr)==Shp))

!     if(present(lbd)) Ptr(lbd(1):,lbd(2):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_2DR4

    subroutine MAPL_AllocNodeArray_3DR4(Ptr, Shp, lbd, rc)
      real*4, pointer,   intent(INOUT) :: Ptr(:,:,:)
      integer,           intent(IN   ) :: Shp(3)
      integer, optional, intent(IN   ) :: lbd(3)
      integer, optional, intent(  OUT) :: rc

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      len=product(Shp)

      call GetSharedMemory(Caddr, len, rc=STATUS)
      VERIFY_(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      ASSERT_(all(shape(Ptr)==Shp))

!     if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_3DR4


    subroutine MAPL_AllocNodeArray_1DR8(Ptr, Shp, lbd, rc)
      real*8, pointer,   intent(INOUT) :: Ptr(:)
      integer,           intent(IN   ) :: Shp(1)
      integer, optional, intent(IN   ) :: lbd(1)
      integer, optional, intent(  OUT) :: rc

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      len = shp(1)*2

      call GetSharedMemory(Caddr, len, rc=STATUS)
      VERIFY_(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      ASSERT_(size(Ptr)==len)

!     if(present(lbd)) Ptr(lbd(1):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_1DR8


    subroutine MAPL_AllocNodeArray_2DR8(Ptr, Shp, lbd, rc)
      real*8, pointer,   intent(INOUT) :: Ptr(:,:)
      integer,           intent(IN   ) :: Shp(2)
      integer, optional, intent(IN   ) :: lbd(2)
      integer, optional, intent(  OUT) :: rc

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      len=product(Shp)*2

      call GetSharedMemory(Caddr, len, rc=STATUS)
      VERIFY_(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      ASSERT_(all(shape(Ptr)==Shp))

!     if(present(lbd)) Ptr(lbd(1):,lbd(2):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_2DR8

    subroutine MAPL_AllocNodeArray_3DR8(Ptr, Shp, lbd, rc)
      real*8, pointer,   intent(INOUT) :: Ptr(:,:,:)
      integer,           intent(IN   ) :: Shp(3)
      integer, optional, intent(IN   ) :: lbd(3)
      integer, optional, intent(  OUT) :: rc

      type(c_ptr) :: Caddr
      integer len, STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      len=product(Shp)*2

      call GetSharedMemory(Caddr, len, rc=STATUS)
      VERIFY_(STATUS)

      call c_f_pointer(Caddr, Ptr, Shp) ! C ptr to Fortran ptr
      ASSERT_(all(shape(Ptr)==Shp))

!     if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_3DR8


    subroutine ReleaseSharedMemory(Caddr,rc)
      type(c_ptr),       intent(  OUT) :: Caddr
      integer, optional, intent(  OUT) :: rc

      integer        :: pos
      type (c_ptr)   :: buf
      integer        :: STATUS

!!! Find the segment in the segment list

      pos=1
      do while(pos<=size(Segs))
         if(c_associated(Segs(pos)%addr,Caddr)) exit
         pos = pos + 1
      end do

!!! Everyone exits if it is not there

      ASSERT_(pos<=size(Segs))

!!! Everyone detaches address from shared segment

      status = shmdt(Caddr)
      ASSERT_(status /= -1)

!!! Make sure everyone has finished detaching

      call MPI_Barrier(MAPL_NodeComm, STATUS)
      ASSERT_(STATUS==MPI_SUCCESS)

!!! The root processor destroys the segment

      if (MAPL_AmNodeRoot) then
         STATUS = shmctl(Segs(pos)%shmid, IPC_RMID, buf)
         ASSERT_(STATUS /= -1)
      end if

!!! Free the position in the segment list

      Segs(pos)%shmid=-1

      RETURN_(SHM_SUCCESS)
    end subroutine ReleaseSharedMemory



    subroutine GetSharedMemory(Caddr,Len,rc)
      type(c_ptr),       intent(  OUT) :: Caddr
      integer,           intent(IN   ) :: Len
      integer, optional, intent(  OUT) :: rc

      integer                   :: status, pos
      integer(c_int)            :: key
      integer(c_long_long)      :: numBytes
      integer, parameter        :: WORD_SIZE = 4
      integer(c_int), parameter :: C_ZERO = 0
      integer(c_int), parameter :: shmflg = ior(IPC_CREAT,o'666')
      integer(c_int), parameter :: keypre = 456000000

!!! Get an empty spot in the list of allocated segments
!!! and use its index as the segment's key

      pos=1
      do while(pos<=size(Segs))
         if(Segs(pos)%shmid==-1) exit ! Found an available spot

         if(pos==size(Segs)) then ! Expand the segment list
            allocate(SegsNew(size(Segs)+CHUNK),stat=STATUS)
            ASSERT_(STATUS==0)

            SegsNew(1:size(Segs)) = Segs

            deallocate(Segs,stat=STATUS)
            ASSERT_(STATUS==0)

            Segs=>SegsNew
            nullify(SegsNew)
         end if

         pos = pos + 1
      end do

      key = keypre + pos

!!!  Create the segment in root and have other processors
!!!  get the segment id using the common key

      numBytes = WORD_SIZE*len

      if (MAPL_AmNodeRoot) then ! root process creates segment
         Segs(pos)%shmid = shmget(key, numBytes, shmflg)
         if (Segs(pos)%shmid < 0) then
            call perror('server shmget():'//C_NULL_CHAR)
            ASSERT_(.false.)
         end if
         call MPI_Barrier(MAPL_NodeComm, STATUS)
         ASSERT_(STATUS==MPI_SUCCESS)
      else                 ! wait for create on root & get segment
         call MPI_Barrier(MAPL_NodeComm, STATUS)
         ASSERT_(STATUS==MPI_SUCCESS)
         Segs(pos)%shmid = shmget(key, numBytes, o'666')
         ASSERT_(Segs(pos)%shmid /= -1)
      end if

!!! Everyone attaches the memory to their own C pointer

      Segs(pos)%addr= shmat(Segs(pos)%shmid, C_NULL_PTR, C_ZERO)

!!! Check that we have valid shared memory

      ASSERT_(c_associated(Segs(pos)%addr))

!!! Return C address. It will be attached to a Fortran pointer
!!!  with rank overloads 

      Caddr = Segs(pos)%addr

      RETURN_(SHM_SUCCESS)
    end subroutine GetSharedMemory

    function getNodeComm(Comm, rc) result(NodeComm)
      integer,           intent( IN) :: Comm
      integer, optional, intent(OUT) :: rc
      integer                        :: NodeComm

      integer, allocatable                  :: colors(:)
      character(len=MPI_MAX_PROCESSOR_NAME) :: name
      character(len=MPI_MAX_PROCESSOR_NAME), allocatable :: names(:)
    
      integer :: len, STATUS, MyColor, NumColors, npes, rank
      integer :: NumCores
    
      NodeComm=MPI_COMM_NULL
      
      call MPI_Get_processor_name(name,len,STATUS)
      ASSERT_(STATUS==MPI_SUCCESS)

      call MPI_COMM_RANK(Comm, rank, STATUS)
      ASSERT_(STATUS==MPI_SUCCESS)
      call MPI_COMM_SIZE(Comm, npes, STATUS)
      ASSERT_(STATUS==MPI_SUCCESS)

      allocate(names(npes),stat=STATUS)
      ASSERT_(STATUS==0)

      call MPI_AllGather(name ,MPI_MAX_PROCESSOR_NAME,MPI_CHARACTER,&
                         names,MPI_MAX_PROCESSOR_NAME,MPI_CHARACTER,Comm,status)
      ASSERT_(STATUS==MPI_SUCCESS)

      myColor = getColor(name, names)

      ! We are ready to split communicators

      call MPI_COMM_SPLIT(Comm, MyColor, rank, NodeComm, STATUS)
      ASSERT_(NodeComm/=MPI_COMM_NULL)

      call MPI_COMM_SIZE(NodeComm, NumCores, STATUS)
      ASSERT_(STATUS==MPI_SUCCESS)

      NumColors = npes/NumCores
      if (NumColors*NumCores /= npes) NumColors=NumColors+1

      MAPL_MyNodeNum = rank/NumCores !ALT: this depends on affinity
                                     !     and breaks if round-robin

      MAPL_CoresPerNodeUsed = NumCores

      if(rank==0) then
         print *
         print *, "In MAPL_InitializeShmem:"
         print *, "    NumCores per Node = ", NumCores
         print *, "    NumNodes in use   = ", NumColors
         print *, "    Total PEs         = ", npes
         print *
      end if

      deallocate(names,stat=STATUS)
      ASSERT_(STATUS==0)
    
      RETURN_(SHM_SUCCESS)
    contains
      function getColor(name, sampleNames) result(color)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: sampleNames(:)
        integer :: color
        
        integer :: i
        
        color = 0 ! unless
        do i = 1, size(sampleNames)
           if (trim(name) == trim(sampleNames(i))) then
              color = i
              exit
           end if
        end do

      end function getColor
    
    end function getNodeComm

    function MAPL_ShmemAmOnFirstNode(comm, rc) result(a)
      integer,           intent(IN   ) :: comm
      integer, optional, intent(  OUT) :: RC
      logical                          :: a

      integer :: status, rank

      if ( MAPL_NodeComm == -1 ) then
           call MAPL_GetNodeInfo(comm, rc=STATUS )
           VERIFY_(STATUS)
      end if

      a = .false.
      if (MAPL_MyNodeNum == 0) then
         if (MAPL_ShmInitialized) then
            a = .true.
         else
            call MPI_Comm_rank(comm, rank, STATUS)
            ASSERT_(STATUS==MPI_SUCCESS)
            a = (rank == 0)
         end if
      end if

      RETURN_(SHM_SUCCESS)
    end function MAPL_ShmemAmOnFirstNode

    integer function MAPL_CoresPerNodeGet(comm, rc)
      integer,           intent(IN   ) :: comm
      integer, optional, intent(  OUT) :: RC
      integer                          :: a

      integer :: status, rank

      if ( MAPL_NodeComm == -1 ) then
           call MAPL_GetNodeInfo(comm, rc=STATUS )
           VERIFY_(STATUS)
      end if

!      MAPL_CoresPerNodeGet = MAPL_CoresPerNodeUsed
      MAPL_CoresPerNodeGet = MAPL_CoresPerNodeMax

      RETURN_(SHM_SUCCESS)
    end function MAPL_CoresPerNodeGet

  end module MAPL_ShmemMod
