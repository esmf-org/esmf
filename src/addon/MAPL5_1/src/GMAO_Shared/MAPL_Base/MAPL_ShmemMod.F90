
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
    public :: MAPL_SyncSharedMemory
    public :: MAPL_BroadcastToNodes

    public :: MAPL_AllocateShared

    integer, public, parameter :: MAPL_NoShm=255

    character*30 :: Iam="MAPL_ShmemMod in line "

    integer(c_int), parameter :: IPC_CREAT = 512
    integer(c_int), parameter :: IPC_RMID  = 0

    integer,        parameter :: CHUNK=256

    integer, public, save :: MAPL_NodeComm=-1
    integer, public, save :: MAPL_NodeRootsComm=-1
    integer, public, save :: MAPL_MyNodeNum=-1
    logical, public, save :: MAPL_AmNodeRoot=.false.
    logical, public, save :: MAPL_ShmInitialized=.false.

    integer,         save :: MAPL_CoresPerNodeUsed=-1
    integer,         save :: MAPL_CoresPerNodeMin=-1
    integer,         save :: MAPL_CoresPerNodeMax=-1
    integer,         save :: MAPL_NumNodes=-1

    type Segment_T
       integer (c_int) :: shmid=-1
       type    (c_ptr) :: addr
    end type Segment_T

    type(Segment_T), pointer :: Segs(:) => NULL()
    type(Segment_T), pointer :: SegsNew(:) => null()

    type NodeRankList_T
       integer, pointer :: rank(:) => NULL()
    end type NodeRankList_T

    type(NodeRankList_T), public, allocatable :: MAPL_NodeRankList(:)

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
         implicit none
         type (c_ptr)           :: shmat
         integer (c_int), value :: shmid
         type (c_ptr),    value :: shmaddr
         integer (c_int), value :: shmflg
       end function shmat

       function shmdt(shmaddr) bind(c, name="shmdt")
         use, intrinsic :: ISO_C_BINDING
         implicit none
         integer (c_int)     :: shmdt
         type (c_ptr), value :: shmaddr
       end function shmdt

       function shmctl(shmid, cmd, buf) bind(c, name="shmctl")
         use, intrinsic :: ISO_C_BINDING
         implicit none
         integer (c_int)        :: shmctl
         integer (c_int), value :: shmid
         integer (c_int), value :: cmd
         type (c_ptr),    value :: buf
       end function shmctl

       subroutine perror(s) bind(c,name="perror")
         use, intrinsic :: ISO_C_BINDING
         implicit none
         character(c_char), intent(in) :: s(*)
       end subroutine perror

    end interface

    interface MAPL_AllocNodeArray
       module procedure MAPL_AllocNodeArray_1DL4
       module procedure MAPL_AllocNodeArray_1DI4
       module procedure MAPL_AllocNodeArray_2DI4
       module procedure MAPL_AllocNodeArray_3DI4
       module procedure MAPL_AllocNodeArray_1DR4
       module procedure MAPL_AllocNodeArray_2DR4
       module procedure MAPL_AllocNodeArray_3DR4
       module procedure MAPL_AllocNodeArray_4DR4
       module procedure MAPL_AllocNodeArray_1DR8
       module procedure MAPL_AllocNodeArray_2DR8
       module procedure MAPL_AllocNodeArray_3DR8
       module procedure MAPL_AllocNodeArray_4DR8
    end interface

    interface MAPL_DeAllocNodeArray
       module procedure MAPL_DeAllocNodeArray_1DL4
       module procedure MAPL_DeAllocNodeArray_1DI4
       module procedure MAPL_DeAllocNodeArray_2DI4
       module procedure MAPL_DeAllocNodeArray_3DI4
       module procedure MAPL_DeAllocNodeArray_1DR4
       module procedure MAPL_DeAllocNodeArray_2DR4
       module procedure MAPL_DeAllocNodeArray_3DR4
       module procedure MAPL_DeAllocNodeArray_4DR4
       module procedure MAPL_DeAllocNodeArray_1DR8
       module procedure MAPL_DeAllocNodeArray_2DR8
       module procedure MAPL_DeAllocNodeArray_3DR8
       module procedure MAPL_DeAllocNodeArray_4DR8
    end interface
   
    interface MAPL_BroadcastToNodes
       module procedure MAPL_BroadcastToNodes_1DI4
       module procedure MAPL_BroadcastToNodes_2DI4
       module procedure MAPL_BroadcastToNodes_3DI4
       module procedure MAPL_BroadcastToNodes_1DR4
       module procedure MAPL_BroadcastToNodes_2DR4
       module procedure MAPL_BroadcastToNodes_3DR4
       module procedure MAPL_BroadcastToNodes_4DR4
       module procedure MAPL_BroadcastToNodes_1DR8
       module procedure MAPL_BroadcastToNodes_2DR8
       module procedure MAPL_BroadcastToNodes_3DR8
       module procedure MAPL_BroadcastToNodes_4DR8
    end interface

    interface MAPL_AllocateShared
       module procedure MAPL_AllocateShared_1DL4
       module procedure MAPL_AllocateShared_1DI4
       module procedure MAPL_AllocateShared_1DR4
       module procedure MAPL_AllocateShared_2DR4
    end interface

  contains

    subroutine MAPL_GetNodeInfo(comm, rc)
      integer,           intent(IN ) :: comm
      integer, optional, intent(OUT) :: rc

      integer :: STATUS

      if (MAPL_NodeComm == -1) then ! make sure that we do this only once
         MAPL_NodeComm = getNodeComm(comm, rc=STATUS)
         VERIFY_(STATUS)
      end if

      if (MAPL_NodeRootsComm == -1) then ! make sure that we do this only once
         MAPL_NodeRootsComm = getNodeRootsComm(comm, rc=STATUS)
         VERIFY_(STATUS)
      end if

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_GetNodeInfo

    subroutine MAPL_InitializeShmem(rc)
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

      if (allocated(MAPL_NodeRankList)) then
         do i=1,size(MAPL_NodeRankList)
            if (associated(MAPL_NodeRankList(i)%rank)) then
               deallocate(MAPL_NodeRankList(i)%rank)
               MAPL_NodeRankList(i)%rank=>NULL()
            end if
         end do
         deallocate(MAPL_NodeRankList)
      end if

      if (associated(Segs)) then
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

         deallocate(Segs,stat=STATUS)
         ASSERT_(STATUS==0)
      end if

      MAPL_ShmInitialized=.false.

#ifdef DEBUG
      if(MAPL_AmNodeRoot) &
           print *, "MAPL_Shmem finalized for node ", MAPL_MyNodeNum
#endif

      call MPI_Comm_free(MAPL_NodeComm, status)
      ASSERT_(STATUS==MPI_SUCCESS)
      MAPL_NodeComm = -1

      if (MAPL_NodeRootsComm /= MPI_COMM_NULL) then
         call MPI_Comm_free(MAPL_NodeRootsComm, status)
         ASSERT_(STATUS==MPI_SUCCESS)
      end if
      MAPL_NodeRootsComm = -1

      MAPL_MyNodeNum=-1
      MAPL_AmNodeRoot=.false.

      MAPL_CoresPerNodeUsed=-1
      MAPL_CoresPerNodeMin=-1
      MAPL_CoresPerNodeMax=-1
      MAPL_NumNodes=-1

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_FinalizeShmem

    subroutine MAPL_DeAllocNodeArray_1DL4(Ptr,rc)
      logical,  pointer              :: Ptr(:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_1DL4

    subroutine MAPL_DeAllocNodeArray_1DI4(Ptr,rc)
      integer,  pointer              :: Ptr(:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_1DI4

    subroutine MAPL_DeAllocNodeArray_2DI4(Ptr,rc)
      integer,  pointer              :: Ptr(:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_2DI4

    subroutine MAPL_DeAllocNodeArray_3DI4(Ptr,rc)
      integer,  pointer              :: Ptr(:,:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_3DI4


    subroutine MAPL_DeAllocNodeArray_1DR4(Ptr,rc)
      real*4,  pointer               :: Ptr(:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_1DR4

    subroutine MAPL_DeAllocNodeArray_2DR4(Ptr,rc)
      real*4,  pointer               :: Ptr(:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_2DR4

    subroutine MAPL_DeAllocNodeArray_3DR4(Ptr,rc)
      real*4,  pointer               :: Ptr(:,:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_3DR4

    subroutine MAPL_DeAllocNodeArray_4DR4(Ptr,rc)
      real,  pointer                 :: Ptr(:,:,:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3),lbound(Ptr,4)))
      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_4DR4


    subroutine MAPL_DeAllocNodeArray_1DR8(Ptr,rc)
      real*8,  pointer               :: Ptr(:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_1DR8

    subroutine MAPL_DeAllocNodeArray_2DR8(Ptr,rc)
      real*8,  pointer               :: Ptr(:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_2DR8

    subroutine MAPL_DeAllocNodeArray_3DR8(Ptr,rc)
      real*8,  pointer               :: Ptr(:,:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3)))

      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_3DR8

    subroutine MAPL_DeAllocNodeArray_4DR8(Ptr,rc)
      real*8,  pointer               :: Ptr(:,:,:,:)
      integer, optional, intent(OUT) :: rc

      type(c_ptr) :: Caddr
      integer     :: STATUS

      if(.not.MAPL_ShmInitialized) then
         RETURN_(MAPL_NoShm)
      endif

      Caddr = C_Loc(Ptr(lbound(Ptr,1),lbound(Ptr,2),lbound(Ptr,3),lbound(Ptr,4)))
      call ReleaseSharedMemory(Caddr,rc=STATUS)
      VERIFY_(STATUS)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_DeAllocNodeArray_4DR8

    subroutine MAPL_AllocNodeArray_1DL4(Ptr, Shp, lbd, rc)
      logical, pointer,  intent(INOUT) :: Ptr(:)
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

      if(present(lbd)) Ptr(lbd(1):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_1DL4

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

      if(present(lbd)) Ptr(lbd(1):) => Ptr

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

      if(present(lbd)) Ptr(lbd(1):,lbd(2):) => Ptr

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

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):) => Ptr

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

      if(present(lbd)) Ptr(lbd(1):) => Ptr

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

      if(present(lbd)) Ptr(lbd(1):,lbd(2):) => Ptr

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

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_3DR4

    subroutine MAPL_AllocNodeArray_4DR4(Ptr, Shp, lbd, rc)
      real, pointer,     intent(INOUT) :: Ptr(:,:,:,:)
      integer,           intent(IN   ) :: Shp(4)
      integer, optional, intent(IN   ) :: lbd(4)
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

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):,lbd(4):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_4DR4


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

      if(present(lbd)) Ptr(lbd(1):) => Ptr

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

      if(present(lbd)) Ptr(lbd(1):,lbd(2):) => Ptr

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

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_3DR8

    subroutine MAPL_AllocNodeArray_4DR8(Ptr, Shp, lbd, rc)
      real*8, pointer,   intent(INOUT) :: Ptr(:,:,:,:)
      integer,           intent(IN   ) :: Shp(4)
      integer, optional, intent(IN   ) :: lbd(4)
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

      if(present(lbd)) Ptr(lbd(1):,lbd(2):,lbd(3):,lbd(4):) => Ptr

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_AllocNodeArray_4DR8


    subroutine MAPL_AllocateShared_1DL4(Ptr, Shp, lbd, TransRoot, rc)
      logical, pointer,  intent(INOUT) :: Ptr(:)
      integer,           intent(IN   ) :: Shp(1)
      integer, optional, intent(IN   ) :: lbd(1)
      logical,           intent(IN   ) :: TransRoot
      integer, optional, intent(  OUT) :: rc


      integer :: status

      call MAPL_AllocNodeArray(Ptr, Shp, lbd, rc=STATUS)
      if(STATUS==MAPL_NoShm) then
         if (TransRoot) then
            allocate(Ptr(Shp(1)),stat=status)
         else
            allocate(Ptr(0),stat=status)
         end if
         VERIFY_(STATUS)
      endif

      RETURN_(STATUS)

    end subroutine MAPL_AllocateShared_1DL4

    subroutine MAPL_AllocateShared_1DI4(Ptr, Shp, lbd, TransRoot, rc)
      integer, pointer,  intent(INOUT) :: Ptr(:)
      integer,           intent(IN   ) :: Shp(1)
      integer, optional, intent(IN   ) :: lbd(1)
      logical,           intent(IN   ) :: TransRoot
      integer, optional, intent(  OUT) :: rc


      integer :: status

      call MAPL_AllocNodeArray(Ptr, Shp, lbd, rc=STATUS)
      if(STATUS==MAPL_NoShm) then 
         if (TransRoot) then
            allocate(Ptr(Shp(1)),stat=status)
         else
            allocate(Ptr(0),stat=status)
         end if
         VERIFY_(STATUS)
      endif

      RETURN_(STATUS)

    end subroutine MAPL_AllocateShared_1DI4

    subroutine MAPL_AllocateShared_1DR4(Ptr, Shp, lbd, TransRoot, rc)
      real, pointer,     intent(INOUT) :: Ptr(:)
      integer,           intent(IN   ) :: Shp(1)
      integer, optional, intent(IN   ) :: lbd(1)
      logical,           intent(IN   ) :: TransRoot
      integer, optional, intent(  OUT) :: rc


      integer :: status

      call MAPL_AllocNodeArray(Ptr, Shp, lbd, rc=STATUS)
      if(STATUS==MAPL_NoShm) then 
         if (TransRoot) then
            allocate(Ptr(Shp(1)),stat=status)
         else
            allocate(Ptr(0),stat=status)
         end if
         VERIFY_(STATUS)
      endif

      RETURN_(STATUS)

    end subroutine MAPL_AllocateShared_1DR4

    subroutine MAPL_AllocateShared_2DR4(Ptr, Shp, lbd, TransRoot, rc)
      real,    pointer,  intent(INOUT) :: Ptr(:,:)
      integer,           intent(IN   ) :: Shp(2)
      integer, optional, intent(IN   ) :: lbd(2)
      logical,           intent(IN   ) :: TransRoot
      integer, optional, intent(  OUT) :: rc


      integer :: status

      call MAPL_AllocNodeArray(Ptr, Shp, lbd, rc=STATUS)
      if(STATUS==MAPL_NoShm) then 
         if (TransRoot) then
            allocate(Ptr(Shp(1),Shp(2)),stat=status)
         else
            allocate(Ptr(0,0),stat=status)
         end if
         VERIFY_(STATUS)
      endif

      RETURN_(STATUS)

    end subroutine MAPL_AllocateShared_2DR4

    subroutine ReleaseSharedMemory(Caddr,rc)
      type(c_ptr),       intent(INOUT) :: Caddr
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

!!! The root processor destroys the segment

      if (MAPL_AmNodeRoot) then
         STATUS = shmctl(Segs(pos)%shmid, IPC_RMID, buf)
         ASSERT_(STATUS /= -1)
      end if

!!! Everyone detaches address from shared segment

      status = shmdt(Caddr)
      ASSERT_(status /= -1)

!!! Make sure everyone has finished detaching

      call MPI_Barrier(MAPL_NodeComm, STATUS)
      ASSERT_(STATUS==MPI_SUCCESS)

!!! The root processor destroys the segment
!
!     if (MAPL_AmNodeRoot) then
!        STATUS = shmctl(Segs(pos)%shmid, IPC_RMID, buf)
!        ASSERT_(STATUS /= -1)
!     end if

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
      integer(c_int), parameter :: myflg = o'666'
      integer(c_int), parameter :: shmflg = ior(IPC_CREAT,myflg)
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
         Segs(pos)%shmid = shmget(key, numBytes, myflg)
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

    subroutine MAPL_BroadcastToNodes_1DR4(DATA,N,ROOT,rc)
      real*4,            intent(INOUT) :: DATA(:)
      integer,           intent(IN   ) :: N
      integer,           intent(IN   ) :: ROOT
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS

      real*4, allocatable :: ldata(:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         RETURN_(SHM_SUCCESS)
      end if

      allocate(ldata(size(data,1)),stat=status)
      VERIFY_(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_REAL, ROOT, MAPL_NodeRootsComm, STATUS)
      VERIFY_(STATUS)
      data = ldata
      deallocate(ldata)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_BroadcastToNodes_1DR4

    subroutine MAPL_BroadcastToNodes_2DR4(DATA,N,ROOT,rc)
      real*4,            intent(INOUT) :: DATA(:,:)
      integer,           intent(IN   ) :: N
      integer,           intent(IN   ) :: ROOT
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS

      real*4, allocatable :: ldata(:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         RETURN_(SHM_SUCCESS)
      end if

      allocate(ldata(size(data,1),size(data,2)),stat=status)
      VERIFY_(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_REAL, ROOT, MAPL_NodeRootsComm, STATUS)
      VERIFY_(STATUS)
      data = ldata
      deallocate(ldata)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_BroadcastToNodes_2DR4

    subroutine MAPL_BroadcastToNodes_3DR4(DATA,N,ROOT,rc)
      real,              intent(INOUT) :: DATA(:,:,:)
      integer,           intent(IN   ) :: N
      integer,           intent(IN   ) :: ROOT
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS

      real, allocatable :: ldata(:,:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         RETURN_(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2),size(data,3)),stat=STATUS)
      VERIFY_(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_REAL, ROOT, MAPL_NodeRootsComm, STATUS)
      VERIFY_(STATUS)
      data = ldata
      deallocate(ldata)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_BroadcastToNodes_3DR4

    subroutine MAPL_BroadcastToNodes_4DR4(DATA,N,ROOT,rc)
      real,              intent(INOUT) :: DATA(:,:,:,:)
      integer,           intent(IN   ) :: N
      integer,           intent(IN   ) :: ROOT
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS

      real, allocatable :: ldata(:,:,:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         RETURN_(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2),size(data,3),size(data,4)),stat=STATUS)
      VERIFY_(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_REAL, ROOT, MAPL_NodeRootsComm, STATUS)
      VERIFY_(STATUS)
      data = ldata
      deallocate(ldata)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_BroadcastToNodes_4DR4

    subroutine MAPL_BroadcastToNodes_1DR8(DATA,N,ROOT,rc)
      real*8,            intent(INOUT) :: DATA(:)
      integer,           intent(IN   ) :: N
      integer,           intent(IN   ) :: ROOT
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS

      real*8, allocatable :: ldata(:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         RETURN_(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1)),stat=STATUS)
      VERIFY_(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_DOUBLE_PRECISION, ROOT, MAPL_NodeRootsComm, STATUS)
      VERIFY_(STATUS)
      data = ldata
      deallocate(ldata)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_BroadcastToNodes_1DR8

    subroutine MAPL_BroadcastToNodes_2DR8(DATA,N,ROOT,rc)
      real*8,            intent(INOUT) :: DATA(:,:)
      integer,           intent(IN   ) :: N
      integer,           intent(IN   ) :: ROOT
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS

      real*8, allocatable :: ldata(:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         RETURN_(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2)),stat=STATUS)
      VERIFY_(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_DOUBLE_PRECISION, ROOT, MAPL_NodeRootsComm, STATUS)
      VERIFY_(STATUS)
      data = ldata
      deallocate(ldata)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_BroadcastToNodes_2DR8

    subroutine MAPL_BroadcastToNodes_3DR8(DATA,N,ROOT,rc)
      real*8,            intent(INOUT) :: DATA(:,:,:)
      integer,           intent(IN   ) :: N
      integer,           intent(IN   ) :: ROOT
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS

      real*8, allocatable :: ldata(:,:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         RETURN_(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2),size(data,3)),stat=STATUS)
      VERIFY_(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_DOUBLE_PRECISION, ROOT, MAPL_NodeRootsComm, STATUS)
      VERIFY_(STATUS)
      data = ldata
      deallocate(ldata)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_BroadcastToNodes_3DR8

    subroutine MAPL_BroadcastToNodes_4DR8(DATA,N,ROOT,rc)
      real*8,            intent(INOUT) :: DATA(:,:,:,:)
      integer,           intent(IN   ) :: N
      integer,           intent(IN   ) :: ROOT
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS

      real*8, allocatable :: ldata(:,:,:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         RETURN_(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2),size(data,3),size(data,4)),stat=STATUS)
      VERIFY_(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_DOUBLE_PRECISION, ROOT, MAPL_NodeRootsComm, STATUS)
      VERIFY_(STATUS)
      data = ldata
      deallocate(ldata)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_BroadcastToNodes_4DR8

    subroutine MAPL_BroadcastToNodes_1DI4(DATA,N,ROOT,rc)
      integer,           intent(INOUT) :: DATA(:)
      integer,           intent(IN   ) :: N
      integer,           intent(IN   ) :: ROOT
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS

      integer, allocatable :: ldata(:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         RETURN_(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1)),stat=STATUS)
      VERIFY_(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_INTEGER, ROOT, MAPL_NodeRootsComm, STATUS)
      VERIFY_(STATUS)
      data = ldata
      deallocate(ldata)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_BroadcastToNodes_1DI4

    subroutine MAPL_BroadcastToNodes_2DI4(DATA,N,ROOT,rc)
      integer,           intent(INOUT) :: DATA(:,:)
      integer,           intent(IN   ) :: N
      integer,           intent(IN   ) :: ROOT
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS

      integer, allocatable :: ldata(:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         RETURN_(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2)),stat=STATUS)
      VERIFY_(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_INTEGER, ROOT, MAPL_NodeRootsComm, STATUS)
      VERIFY_(STATUS)
      data = ldata
      deallocate(ldata)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_BroadcastToNodes_2DI4

    subroutine MAPL_BroadcastToNodes_3DI4(DATA,N,ROOT,rc)
      integer,           intent(INOUT) :: DATA(:,:,:)
      integer,           intent(IN   ) :: N
      integer,           intent(IN   ) :: ROOT
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS

      integer, allocatable :: ldata(:,:,:)

      if(.not.MAPL_ShmInitialized .or. MAPL_NodeRootsComm==MPI_COMM_NULL) THEN
         RETURN_(SHM_SUCCESS)
      endif

      allocate(ldata(size(data,1),size(data,2),size(data,3)),stat=STATUS)
      VERIFY_(STATUS)
      ldata = data
      call MPI_Bcast(LDATA, N, MPI_INTEGER, ROOT, MAPL_NodeRootsComm, STATUS)
      VERIFY_(STATUS)
      data = ldata
      deallocate(ldata)

      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_BroadcastToNodes_3DI4

    subroutine MAPL_SyncSharedMemory(rc)
      integer, optional, intent(  OUT) :: rc
      integer :: STATUS
      if(.not.MAPL_ShmInitialized) then
         RETURN_(SHM_SUCCESS)
      endif
!!! Make sure everyone on a node syncs
      call MPI_Barrier(MAPL_NodeComm, STATUS)
      ASSERT_(STATUS==MPI_SUCCESS)
      RETURN_(SHM_SUCCESS)
    end subroutine MAPL_SyncSharedMemory

    function getNodeComm(Comm, rc) result(NodeComm)
      use MAPL_SortMod
      integer,           intent( IN) :: Comm
      integer, optional, intent(OUT) :: rc
      integer                        :: NodeComm

      integer, allocatable :: colors(:), ranks(:)
      integer :: last
      integer :: i, n

      character(len=MPI_MAX_PROCESSOR_NAME) :: name
      character(len=MPI_MAX_PROCESSOR_NAME), allocatable :: names(:)
    
      integer :: len, STATUS, MyColor, NumColors, npes, rank
      integer :: NumCores
      integer :: nodeRank

      integer :: i1, i2
      integer, allocatable :: newNode(:)

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

      allocate(colors(npes), ranks(npes), stat=STATUS)
      ASSERT_(STATUS==0)
      do i=1,npes
         ranks(i) = i-1
      end do
      
      call MPI_AllGather(myColor, 1, MPI_INTEGER,&
                         colors,  1, MPI_INTEGER,Comm,status)
      ASSERT_(STATUS==MPI_SUCCESS)

      call MAPL_Sort(COLORS,ranks)
      last = 0
      n = 0
      allocate(newNode(npes+1),stat=status)
      VERIFY_(STATUS)
      newNode = 0
      do i=1,npes
         if(last /= colors(i)) then 
            last = colors(i)
            n = n + 1
            newNode(n) = i
         end if
         ! We are done with COLORS, now we reuse the space to store NodeId
         colors(i) = n
      end do
      NumColors = n
      MAPL_NumNodes = NumColors
      MAPL_MyNodeNum = colors(ranks(rank+1)+1)

      newNode(NumColors+1) = npes+1
      allocate(MAPL_NodeRankList(NumColors), stat=status)
      VERIFY_(STATUS) 
      do i=1,NumColors
         i1=newNode(i)
         i2=newNode(i+1)-1
         allocate(MAPL_NodeRankList(i)%rank(i2-i1+1),stat=status)
         VERIFY_(STATUS)
         MAPL_NodeRankList(i)%rank=ranks(i1:i2)
         call MAPL_Sort(MAPL_NodeRankList(i)%rank)
      end do
      deallocate(newNode)

      deallocate(ranks)
      deallocate(colors)

      MAPL_CoresPerNodeUsed = NumCores

      call MPI_Comm_rank(NodeComm, nodeRank, STATUS)
      ASSERT_(STATUS==MPI_SUCCESS)

      MAPL_AmNodeRoot = nodeRank==0

      if (MAPL_AmNodeRoot) then
         ASSERT_(MAPL_NodeRankList(MAPL_MyNodeNum)%rank(1) == rank)
      end if

!     we store the global Min and Max of CoresPerNode
      call MPI_AllReduce (MAPL_CoresPerNodeUsed, MAPL_CoresPerNodeMin, &
                          1, MPI_INTEGER, MPI_MIN, comm, status )
      VERIFY_(STATUS)
      call MPI_AllReduce (MAPL_CoresPerNodeUsed, MAPL_CoresPerNodeMax, &
                          1, MPI_INTEGER, MPI_MAX, comm, status )
      VERIFY_(STATUS)

      if(rank==0) then
         print *
         print *, "In MAPL_Shmem:"
         if (MAPL_CoresPerNodeMin == MAPL_CoresPerNodeMax) then
            print *, "    NumCores per Node = ", NumCores
         else
            print *, "    NumCores per Node varies from ", &
                 MAPL_CoresPerNodeMin, " to ", MAPL_CoresPerNodeMax
         end if
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

    function getNodeRootsComm(Comm, rc) result(NodeRootsComm)
      integer,           intent( IN) :: Comm
      integer, optional, intent(OUT) :: rc
      integer                        :: NodeRootsComm

      integer :: STATUS, MyColor, NumNodes, npes, rank

      NodeRootsComm=MPI_COMM_NULL

      call MPI_COMM_RANK(Comm, rank, STATUS)
      ASSERT_(STATUS==MPI_SUCCESS)
      call MPI_COMM_SIZE(Comm, npes, STATUS)
      ASSERT_(STATUS==MPI_SUCCESS)

      myColor = 0
      if (MAPL_AmNodeRoot) myColor = 1

      ! We are ready to split communicators

      call MPI_COMM_SPLIT(Comm, MyColor, rank, NodeRootsComm, STATUS)
      ASSERT_(NodeRootsComm/=MPI_COMM_NULL)

      if (myColor==0) then
      ! Set nodes outside of this comm back to null
         NodeRootsComm=MPI_COMM_NULL
      else
      ! Confirm we have the proper communicator
         call MPI_COMM_SIZE(NodeRootsComm, NumNodes, STATUS)
         ASSERT_(STATUS==MPI_SUCCESS)
         ! additional sanity checks
         ASSERT_(MAPL_NumNodes == NumNodes)
         call MPI_COMM_RANK(NodeRootsComm, rank, STATUS)
         ASSERT_(STATUS==MPI_SUCCESS)
         ASSERT_(MAPL_MyNodeNum == rank+1)
      endif

      if(rank==0) then
         print *
         print *, "In MAPL_InitializeShmem (NodeRootsComm):"
         print *, "    NumNodes in use   = ", NumNodes
         print *
      end if

      RETURN_(SHM_SUCCESS)

    end function getNodeRootsComm



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
      if (MAPL_MyNodeNum == 1) then
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

      integer :: status

      if ( MAPL_NodeComm == -1 ) then
           call MAPL_GetNodeInfo(comm, rc=STATUS )
           VERIFY_(STATUS)
      end if

      MAPL_CoresPerNodeGet = MAPL_CoresPerNodeUsed

      RETURN_(SHM_SUCCESS)
    end function MAPL_CoresPerNodeGet

  end module MAPL_ShmemMod
