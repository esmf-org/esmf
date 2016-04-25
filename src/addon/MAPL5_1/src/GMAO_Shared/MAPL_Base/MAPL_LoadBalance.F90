

#define LDB_SUCCESS  0
#define VERIFY_(A) if((A)/=LDB_SUCCESS) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define ASSERT_(A) if(.not.(A)) then; if(present(rc)) rc=1; PRINT *, Iam, __LINE__; return; endif
#define RETURN_(A) if(present(rc)) rc=A; return


module MAPL_LoadBalanceMod

  use MAPL_SortMod

  implicit none
  private

  include "mpif.h"

  public MAPL_BalanceWork
  public MAPL_BalanceCreate
  public MAPL_BalanceDestroy
  public MAPL_BalanceGet

  integer, public, parameter :: MAPL_Distribute = 1
  integer, public, parameter :: MAPL_Retrieve   = 2

  type TBalanceStrategy
     integer :: UNBALANCED_LENGTH=-1
     integer :: BALANCED_LENGTH  =-1
     integer :: BUFFER_LENGTH    =-1
     integer :: PASSES           =-1
     integer :: COMM             =-1
     integer, pointer :: NOP(:,:)=>Null()
  end type TBalanceStrategy

  integer,           parameter :: MAX_NUM_STRATEGIES=1000
  type(TBalanceStrategy), save :: THE_STRATEGIES(0:MAX_NUM_STRATEGIES)

  character*30 :: Iam="MAPL_LoadBalanceMod in line "

!!!===============================================================

!  EXAMPLE

!      REAL A(IM,JM,LM), B(IM,JM), C(IM,JM,LM)
!      REAL, allocatable :: AT(:,:), BT(:), CT(:,:)
!      LOGICAL MASK(IM,JM)
!      ...
!      LENGTH = COUNT(MASK)
!      IRUN   = MAPL_BalanceCreate(LENGTH)
!      IDIM   = max(length,irun)

!      allocate(AT(IDIM,LM),BT(IDIM),CT(IDIM,LM)

!      BT(1:LENGTH) = PACK(B,MASK)

!      DO L=1,LM
!       AT(1:LENGTH,L) = PACK(A(:,:,L),MASK)
!      ENDDO

!!! DISTRIBUTE THE INPUTS

!      CALL MAPL_BalanceWork(AT,IDIM,LM,Direction=MAPL_Distribute)
!      CALL MAPL_BalanceWork(BT,IDIM,1 ,Direction=MAPL_Distribute)

!!! PLUG COMPATIBLE ROUTINE AT(IN), BT(INOUT), CT(OUT)

!      CALL WORKSUB(IRUN,AT,BT,CT)

!!! RETRIEVE THE OUTPUTS

!      CALL MAPL_BalanceWork(CT,IDIM,LM,Direction=MAPL_Retrieve)
!      CALL MAPL_BalanceWork(BT,IDIM, 1,Direction=MAPL_Retrieve)

!      B = UNPACK(BT(1:LENGTH),MASK,B)

!      DO L=1,LM
!       C(:,:,L) = UNPACK(CT(1:LENGTH,L),MASK,0)
!      ENDDO
!      ...

!!!===============================================================

contains

  subroutine MAPL_BalanceWork(A, Idim, Direction, Handle, rc)
    real,              intent(INOUT) :: A(:)
    integer,           intent(IN   ) :: Idim, Direction
    integer, optional, intent(IN   ) :: Handle
    integer, optional, intent(  OUT) :: rc

    integer :: PASS, LENGTH, PROCESSOR, CURSOR, ISTRAT
    integer :: COMM, Vtype, VLength, STATUS, K1, K2, K3, Jdim
    logical :: SEND, RECV
    integer, pointer :: NOP(:,:)

! Depending on the argument "Direction", this performs the actual distribution
!  of work or the gathering of results for a given strategy. The strategy has to
!  have been predefined by a call to MAPL_BalanceCreate. A strategy "Handle"
!  obtained from that call can be optionally used to specify the strategy. Otherwise,
!  a default strategy is assumed (see MAPL_BalanceCreate for details).
!  Work (Results) is distributed (retrieved) using the buffer A, which is assumed
!  to consist of Jdim contiguous blocks of size Idim. Of course, Jdim can be 1.
!  The blocksize of A (Idim) must be at least as large as the BufLen associated
!  with the strategy. This size can be obtained by quering the strategy using 
!  its handle or be saving it from the MAPL_BalanceCreate call. Again, see 
!  MAPL_BalanceCreate for details.

    Jdim = size(A)/Idim

    if(present(Handle)) then
       ISTRAT = Handle
    else
       ISTRAT = 0
    endif

    if(THE_STRATEGIES(ISTRAT)%PASSES>0) then ! We have a defined strategy
       ASSERT_(associated(THE_STRATEGIES(ISTRAT)%NOP))

! Initialize CURSOR, which is the location in the first block of A where 
! the next read or write is to occur. K1 and K2 are the limits

       if (Direction==MAPL_Distribute) then
          CURSOR = THE_STRATEGIES(ISTRAT)%UnBALANCED_LENGTH + 1
          k1=1
          k2=THE_STRATEGIES(ISTRAT)%PASSES
          k3=1
       else
          CURSOR = THE_STRATEGIES(ISTRAT)%  BALANCED_LENGTH + 1
          k1=THE_STRATEGIES(ISTRAT)%PASSES
          k2=1
          k3=-1
       end if

! NOP contains the communication pattern for the strategy, i.e,,
!  who passes what to whom within COMM.

       NOP  => THE_STRATEGIES(ISTRAT)%NOP
       COMM =  THE_STRATEGIES(ISTRAT)%COMM

       do PASS=K1,K2,K3
          if(Direction==MAPL_Distribute) then
             SEND   = NOP(1,PASS)>0
             RECV   = NOP(1,PASS)<0
          else
             SEND   = NOP(1,PASS)<0
             RECV   = NOP(1,PASS)>0
          end if

          LENGTH    = abs(NOP(1,PASS))
          PROCESSOR = NOP(2,PASS)

          if(Jdim==1) then
             Vtype   = MPI_REAL
             VLength = LENGTH
          else
             call MPI_Type_VECTOR(Jdim, Length, Idim, MPI_REAL, Vtype, STATUS)
             ASSERT_(STATUS==MPI_SUCCESS)
             call MPI_TYPE_COMMIT(Vtype,STATUS)
             ASSERT_(STATUS==MPI_SUCCESS)
             VLength = 1
          end if

          if(SEND) then ! -- SENDER
             CURSOR = CURSOR - LENGTH
             call MPI_SEND(A(CURSOR), VLength, Vtype, PROCESSOR, PASS, COMM, STATUS)
             ASSERT_(STATUS==MPI_SUCCESS)
          endif


          if(RECV) then ! -- RECEIVER
             call MPI_RECV(A(CURSOR), VLength, Vtype, PROCESSOR, PASS, COMM, &
                                                          MPI_STATUS_IGNORE, STATUS)
             ASSERT_(STATUS==MPI_SUCCESS)
             CURSOR = CURSOR + LENGTH
          endif

          if(Jdim>1) then
             call MPI_TYPE_FREE(Vtype,STATUS)
             ASSERT_(STATUS==MPI_SUCCESS)
          end if
       enddo
    end if

    RETURN_(LDB_SUCCESS)
  end subroutine MAPL_BalanceWork

!!!===============================================================

  subroutine MAPL_BalanceCreate(OrgLen, Comm, MaxPasses, BalCond, &
                                Handle, BalLen, BufLen, rc)

    integer,           intent(IN)  :: OrgLen
    integer,           intent(IN)  :: Comm
    integer, optional, intent(IN)  :: MaxPasses
    real,    optional, intent(IN)  :: BalCond
    integer, optional, intent(OUT) :: Handle, BalLen, BufLen
    integer, optional, intent(OUT) :: rc

    real    :: BalCond_
    integer :: MaxPasses_
    integer :: KPASS, STATUS, Balance, MyNewWork, MyBufSize
    integer :: NPES, MyPE, J, JSPARD, LEN
    real    :: MEAN

    integer, allocatable :: WORK(:), RANK(:), NOP(:,:) 

!!! This routine creates a balancing strategy over an MPI communicator (Comm)
!!!  given the work in the local rank (OrgLen). The startegy can be committed
!!!  and used later through Handle. If a handle is not requested, the latest
!!!  non-committed strategy is kept at Handle=0, which will be the default strategy
!!!  for the other methods. The number of passes may be optionally controlled
!!!  with an upper limit (MaxPasses) or a limiting criterion (BalCond).
!!!  The amount of work resulting for the local rank can be returned (BalLen).
!!!
!!! NOTE: As there may be more than one communicator, Comm is required. This
!!!  will most likely be the communicator from the ESMF VM.

!!! Defaults of optional Inputs
!!!----------------------------

    if(present(BalCond)) then
       BalCond_ = BalCond
    else
       BalCond_ = 0.1
    end if

    if(present(MaxPasses)) then
       MaxPasses_ = MaxPasses
    else
       MaxPasses_ = 100
    end if

!!! Get Communicator parameters
!!!----------------------------

    call MPI_COMM_RANK(Comm, MyPE, STATUS)
    ASSERT_(STATUS==MPI_SUCCESS)
    call MPI_COMM_SIZE(Comm, NPES, STATUS)
    ASSERT_(STATUS==MPI_SUCCESS)

!!! Allocate temporary space
!!!-------------------------

    allocate(NOP(2,MaxPasses_), Work(NPES), Rank(NPES), stat=STATUS)
    VERIFY_(STATUS)

!!! Initialize global lists of work load and corresponding rank
!!!------------------------------------------------------------

    call MPI_AllGather(OrgLen,1,MPI_INTEGER,&
                       Work  ,1,MPI_INTEGER,Comm,status)
    ASSERT_(STATUS==MPI_SUCCESS)

    forall (J=1:NPES) Rank(J) = J-1

    call CreateStrategy(Work, Rank, MyPE, BalCond_, Kpass, MyNewWork, MyBufSize, NOP)

    deallocate(Work, Rank)

!!! Done with balancing strategy. Prepare optional Outputs.
!!!--------------------------------------------------------

    if(present(Handle)) then
       do Balance=1,MAX_NUM_STRATEGIES
          if(.not.associated(THE_STRATEGIES(Balance)%NOP)) exit
       enddo

       ASSERT_(Balance <= MAX_NUM_STRATEGIES)
       Handle  = Balance
    else
       Balance = 0
       if( associated(THE_STRATEGIES(Balance)%NOP) ) &
           deallocate(THE_STRATEGIES(Balance)%NOP)
    end if

    if(present(BalLen)) BalLen =  MyNewWork
    if(present(BufLen)) BufLen =  MyBufSize
 
!!! Save the Strategy
!!!------------------

    allocate(THE_STRATEGIES(Balance)%NOP(2,KPASS))

    THE_STRATEGIES(Balance)%BALANCED_LENGTH   = MyNewWork
    THE_STRATEGIES(Balance)%BUFFER_LENGTH     = MyBufSize
    THE_STRATEGIES(Balance)%UNBALANCED_LENGTH = OrgLen
    THE_STRATEGIES(Balance)%PASSES            = KPASS
    THE_STRATEGIES(Balance)%COMM              = Comm
    THE_STRATEGIES(Balance)%NOP               = NOP(:,:KPASS)

    deallocate(NOP)

    RETURN_(LDB_SUCCESS)

  contains

    subroutine CreateStrategy(Work, Rank, MyPE, BalCond, KPASS, MyNewWork, MyBufSize, NOP)
      integer, intent(INOUT) :: Work(:), Rank(:)
      integer, intent(IN   ) :: MyPE
      real   , intent(IN   ) :: BalCond
      integer, intent(  OUT) :: NOP(:,:), KPASS, MyNewWork, MyBufSize
      
      integer :: NPES, J, JSPARD, LEN, MaxPasses
      real    :: MEAN

      NPES      = size(Work)
      MaxPasses = size(NOP,2)

!!! Loop over passes until either MaxPasses or BalanceCondition is met
!!!-------------------------------------------------------------------

      KPASS     = 0
      MEAN      = sum(Work)/float(NPES)
      MyNewWork = OrgLen
      MyBufSize = OrgLen

      PASSES: do while(KPASS<MaxPasses)

!!! Sort latest work-load and rank lists in ascending order of work
!!!----------------------------------------------------------------

         call MAPL_Sort(Work, Rank)

!!! Check for balance condition on the ratio of max minus min work
!!!  to the ideal average work
!!!---------------------------------------------------------------

         if((Work(NPES)-Work(1))/MEAN < BalCond) exit

!!! Fold the sorted work list and compute the transfers needed
!!!  to balance the "least with the greatest".
!!!-----------------------------------------------------------

         KPASS = KPASS+1

         FOLD: do J=1,NPES/2

            ! Js partner in the fold has >= the work as J
            JSPARD = NPES + 1 - J

            ! Half the difference will be sent to J (can be zero)
            LEN    = (Work(JSPARD)-Work(J))/2

            ! New lengths that obtain after completion of this pass
            Work(J     ) = Work(J     ) + LEN
            Work(JSPARD) = Work(JSPARD) - LEN

            ! A negative length means J receives from partner
            if(Rank(J    ) == MyPE) then
               NOP(1,KPASS) = -LEN
               NOP(2,KPASS) = Rank(JSPARD) ! Partners rank
               MyNewWork    = Work(J)
               MyBufSize    = max(MyBufSize,MyNewWork)
            endif

            ! If I am the partner, I will send to J
            if(Rank(JSPARD) == MyPE) then
               NOP(1,KPASS) = LEN
               NOP(2,KPASS) = Rank(J)  ! Js rank
               MyNewWork    = Work(JSPARD)
               MyBufSize    = max(MyBufSize,MyNewWork)
            endif

         enddo FOLD

      enddo PASSES

    end subroutine CreateStrategy

  end subroutine MAPL_BalanceCreate

!!!===============================================================

  subroutine MAPL_BalanceDestroy(Handle, rc)
    integer, optional, intent(IN ) :: Handle
    integer, optional, intent(OUT) :: rc

    integer :: Handle_

    if (present(Handle)) then 
       ASSERT_(Handle>=0 .and. Handle<=MAX_NUM_STRATEGIES)
       Handle_ = Handle
    else
       ! If we do not pass in a Handle, assume we wish to destroy
       ! the default Strategy which has a Handle of 0
       Handle_ = 0
    end if

    if(associated(THE_STRATEGIES(Handle_)%NOP)) &
         deallocate(THE_STRATEGIES(Handle_)%NOP)

    nullify(THE_STRATEGIES(Handle_)%NOP)

    THE_STRATEGIES(Handle_)%UNBALANCED_LENGTH =-1
    THE_STRATEGIES(Handle_)%BALANCED_LENGTH   =-1
    THE_STRATEGIES(Handle_)%BUFFER_LENGTH     =-1
    THE_STRATEGIES(Handle_)%PASSES            =-1
    THE_STRATEGIES(Handle_)%COMM              =-1

    RETURN_(LDB_SUCCESS)
  end subroutine MAPL_BalanceDestroy

!!!===============================================================

  subroutine MAPL_BalanceGet(Handle, BalLen, BufLen, Passes, Comm, rc)
    integer,           intent(IN ) :: Handle
    integer, optional, intent(OUT) :: BalLen, BufLen, Passes, Comm
    integer, optional, intent(OUT) :: rc

    ASSERT_(Handle>=0 .and. Handle<=MAX_NUM_STRATEGIES)

    ASSERT_(associated(THE_STRATEGIES(Handle)%NOP))

    if(present(BalLen)) &
         BalLen = THE_STRATEGIES(Handle)%BALANCED_LENGTH
    if(present(BufLen)) &
         BufLen = THE_STRATEGIES(Handle)%BUFFER_LENGTH
    if(present(Passes)) &
         Passes = THE_STRATEGIES(Handle)%PASSES
    if(present(Comm  )) &
         Comm   = THE_STRATEGIES(Handle)%COMM

    RETURN_(LDB_SUCCESS)
  end subroutine MAPL_BalanceGet

end module MAPL_LOADBALANCEMOD
