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

#define ASSERT_(A) if(.not.(A))call exit(1)


!  $Id: MAPL_Sort.F90,v 1.7 2012-08-29 17:47:55 msuarez Exp $

!=============================================================================
!BOP

! !MODULE: MAPL_Sort   -- A utility to sort integers

! !INTERFACE:

module MAPL_SortMod

  implicit none
  private

! !PUBLIC ROUTINES:

  public MAPL_Sort

! !DESCRIPTION:
! 
!   {\tt MAPL\_Sort} is a utility to do a quicksort on integers. The general
!   interface is:
!\bv       
!       subroutine MAPL_Sort(A[,B])
!         integer(kind=[4,8]),           intent(INOUT) :: A(:)
!         integer(kind=4),     optional, intent(INOUT) :: B(size(A))
!         real   (kind=[4,8]), optional, intent(INOUT) :: B(size(A))
!
!       subroutine MAPL_Sort(A[,DIM])
!         integer(kind=[4,8]),           intent(INOUT) :: A(:,:)
!         integer(kind=4),     optional, intent(IN   ) :: DIM
!
!       subroutine MAPL_Sort(A,B[,DIM])
!         integer(kind=[4,8]),       intent(INOUT) :: A(:)
!         integer(kind=4),           intent(INOUT) :: B(:,:)
!         integer(kind=4), optional, intent(IN   ) :: DIM
!\ev
!   {\tt MAPL\_Sort} sorts the key (contained in a row or column of A)
!   in ascending order and reorders the data in B or in non-key rows or columns of A
!   in the same order; i.e., it does the same exchanges as were done 
!   to the key in sorting it.  If, for example, on input B(:) contains the ordered integers
!   from 1 to size(A), on output it will contain the positions of the elements of
!   the sorted A in the unsorted A. In the last two signatures, DIM is the dimension
!   of A or B being reordered. In the last signature, for example, DIM=1 corresponds
!   to a B ordered as B(size(A),:), whereas DIM=2 corresponds to B(:,size(A)).
!   The default is DIM=2. The quicksort is coded in C and does not appear here.

!EOP
!=============================================================================

interface MAPL_Sort
   module procedure SORT1SS
   module procedure SORT1SR
   module procedure SORT1SD
   module procedure SORT1LS
   module procedure SORT1LR
   module procedure SORT1LD

   module procedure SORT2LS
   module procedure SORT2LR
   module procedure SORT2LD
   module procedure SORT2SS
   module procedure SORT2SR
   module procedure SORT2SD

   module procedure SORT2AS
   module procedure SORT2AL
end interface

contains

subroutine SORT1SS(A,B)
  integer(kind=4),           intent(INOUT) :: A(:)
  integer(kind=4), optional, intent(INOUT) :: B(:)
  if(present(B)) then
     call QSORTS(A,B,size(A),1)
  else
     call QSORTS(A,A,size(A),0)
  endif
end subroutine SORT1SS

subroutine SORT1SR(A,B)
  integer(kind=4),           intent(INOUT) :: A(:)
  real   (kind=4),           intent(INOUT) :: B(:)
  call QSORTS(A,B,size(A),1)
end subroutine SORT1SR

subroutine SORT1SD(A,B)
  integer(kind=4),           intent(INOUT) :: A(:)
  real   (kind=8),           intent(INOUT) :: B(:)
  call QSORTS(A,B,size(A),2)
end subroutine SORT1SD

subroutine SORT1LS(A,B)
  integer(kind=8), intent(INOUT) :: A(:)
  integer(kind=4), optional, intent(INOUT) :: B(:)
  if(present(B)) then
     call QSORTL(A,B,size(A),1)
  else
     call QSORTL(A,A,size(A),0)
  endif
end subroutine SORT1LS

subroutine SORT1LR(A,B)
  integer(kind=8),           intent(INOUT) :: A(:)
  real   (kind=4),           intent(INOUT) :: B(:)
  call QSORTL(A,B,size(A),1)
end subroutine SORT1LR

subroutine SORT1LD(A,B)
  integer(kind=8),           intent(INOUT) :: A(:)
  real   (kind=8),           intent(INOUT) :: B(:)
  call QSORTL(A,B,size(A),2)
end subroutine SORT1LD


subroutine SORT2SS(A,B,DIM)
  integer(kind=4),   intent(INOUT) :: A(:)
  integer(kind=4),   intent(INOUT) :: B(:,:)
  integer, optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if
  ASSERT_(uDIM>0 .and. uDIM<3)
  ASSERT_(size(A)==size(B,uDIM))
  if(uDIM==1) then
     call QSORTS(A,B,size(A),-size(B,2))
  else
     call QSORTS(A,B,size(A), size(B,1))
  end if

end subroutine SORT2SS

subroutine SORT2SR(A,B,DIM)
  integer(kind=4),   intent(INOUT) :: A(:)
  real   (kind=4),   intent(INOUT) :: B(:,:)
  integer, optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if
  ASSERT_(uDIM>0 .and. uDIM<3)
  ASSERT_(size(A)==size(B,uDIM))
  if(uDIM==1) then
     call QSORTS(A,B,size(A),-size(B,2))
  else
     call QSORTS(A,B,size(A), size(B,1))
  end if

end subroutine SORT2SR

subroutine SORT2SD(A,B,DIM)
  integer(kind=4),   intent(INOUT) :: A(:)
  real   (kind=8),   intent(INOUT) :: B(:,:)
  integer, optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if
  ASSERT_(uDIM>0 .and. uDIM<3)
  ASSERT_(size(A)==size(B,uDIM))
  if(uDIM==1) then
     call QSORTS(A,B,size(A),-size(B,2)*2)
  else
     call QSORTS(A,B,size(A), size(B,1)*2)
  end if

end subroutine SORT2SD

subroutine SORT2LS(A,B,DIM)
  integer(kind=8),   intent(INOUT) :: A(:)
  integer(kind=4),   intent(INOUT) :: B(:,:)
  integer, optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if
  ASSERT_(uDIM>0 .and. uDIM<3)
  ASSERT_(size(A)==size(B,uDIM))
  if(uDIM==1) then
     call QSORTL(A,B,size(A),-size(B,2))
  else
     call QSORTL(A,B,size(A), size(B,1))
  end if
end subroutine SORT2LS

subroutine SORT2LR(A,B,DIM)
  integer(kind=8),   intent(INOUT) :: A(:)
  real   (kind=4),   intent(INOUT) :: B(:,:)
  integer, optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if
  ASSERT_(uDIM>0 .and. uDIM<3)
  ASSERT_(size(A)==size(B,uDIM))
  if(uDIM==1) then
     call QSORTL(A,B,size(A),-size(B,2))
  else
     call QSORTL(A,B,size(A), size(B,1))
  end if
end subroutine SORT2LR

subroutine SORT2LD(A,B,DIM)
  integer(kind=8),   intent(INOUT) :: A(:)
  real   (kind=8),   intent(INOUT) :: B(:,:)
  integer, optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if
  ASSERT_(uDIM>0 .and. uDIM<3)
  ASSERT_(size(A)==size(B,uDIM))
  if(uDIM==1) then
     call QSORTL(A,B,size(A),-size(B,2)*2)
  else
     call QSORTL(A,B,size(A), size(B,1)*2)
  end if
end subroutine SORT2LD

subroutine SORT2AS(B,DIM)
  integer(kind=4),   intent(INOUT) :: B(:,:)
  integer, optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if

  ASSERT_(uDIM>0 .and. uDIM<3)

  if(uDIM==1) then
     call QSORTS(B(:,1),B(:,2:),size(B,1),-(size(B,2)-1))
  else
     call QSORTS(B(1,:),B(2:,:),size(B,2), (size(B,1)-1))
  end if
end subroutine SORT2AS

subroutine SORT2AL(B,DIM)
  integer(kind=8),   intent(INOUT) :: B(:,:)
  integer, optional, intent(IN   ) :: DIM

  integer :: uDIM

  if(present(DIM)) then
     uDIM = DIM
  else
     uDIM = 2
  end if

  ASSERT_(uDIM>0 .and. uDIM<3)

  if(uDIM==1) then
     call QSORTL(B(:,1),B(:,2:),size(B,1),-(size(B,2)-1)*2)
  else
     call QSORTL(B(1,:),B(2:,:),size(B,2), (size(B,1)-1)*2)
  end if
end subroutine SORT2AL

end module MAPL_SortMod
