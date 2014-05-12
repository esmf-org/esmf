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

!  $Id: MAPL_HeapMod.F90,v 1.1.180.1 2012-11-08 21:23:04 atrayano Exp $

#include "MAPL_ErrLog.h"
#define ADDRS_POSITION 1

!BOP

! !MODULE: MAPL_HeapMod -- A Module that implements a private heap

! !INTERFACE:

  module MAPL_HeapMod

! !USES:

  use ESMF
  use MAPL_BaseMod

  implicit none
  private

! !PUBLIC TYPES:


  integer, parameter :: NumSegments=20
  integer, parameter :: DefaultSize=1000000


  type :: RealPtr1
     real, pointer :: a(:)
  end type RealPtr1


  type :: MAPL_Heap
     private
     character(len=ESMF_MAXSTR) :: NAME=""
     integer :: HP_start(NumSegments)   = -1
     integer :: HP_end  (NumSegments)   = -1
     real, pointer  :: BUFFER(:)=>null()
     type(RealPtr1) :: PTRS(NumSegments)
  end type MAPL_Heap

  type(MAPL_Heap), save :: HEAP


! !PUBLIC MEMBER FUNCTIONS:

  public MAPL_Alloc
  public MAPL_DeAlloc
  public MAPL_HeapSet
  public MAPL_HeapGet

!EOP

  interface MAPL_Alloc
    module procedure MAPL_Alloc_R_2D
  end interface

  interface MAPL_DeAlloc
    module procedure MAPL_DeAlloc_R_2D
  end interface

  contains

!********************************************************

    subroutine MAPL_HeapSet(HeapSize, RC)
      integer, optional, intent(IN   ) :: HeapSize      
      integer, optional, intent(OUT  ) :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_Heapset"
      integer :: status

      ASSERT_(.not.associated(HEAP%BUFFER))

      if(present(HeapSize)) then
         ASSERT_(HeapSize > 0)
         allocate(HEAP%BUFFER(1:HeapSize), STAT=STATUS)
         VERIFY_(STATUS)
      end if

      RETURN_(ESMF_SUCCESS)
    
    end subroutine MAPL_HeapSet

!********************************************************

    subroutine MAPL_HeapGet(HeapSize, RC)
      integer, optional, intent(OUT  ) :: HeapSize
      integer, optional, intent(OUT  ) :: RC

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_HeapGet"
      integer :: status

      if(present(HeapSize)) then
         HeapSize = size(heap%buffer)
      end if

      RETURN_(ESMF_SUCCESS)
    
    end subroutine MAPL_HeapGet
    

!********************************************************

    
    subroutine MAPL_Alloc_R_2D(A,IM, JM, RC)

      integer,           intent(IN ) :: IM, JM
      integer, optional, intent(OUT) :: RC
      real,  pointer                 :: A(:,:)

      integer :: gap, len, i, j

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_Alloc_R_2D"
      integer :: status

      interface loadr2d
         function loadr2d(P1,I1,I2) result(P2)
           real          :: P1
           real, pointer :: P2(:,:)
           integer, intent(IN) :: I1, I2
         end function loadr2d
      end interface

      if(.not.associated(heap%buffer)) then
         call MAPL_HeapSet(HeapSize=DefaultSize,RC=status)
         VERIFY_(STATUS)
      end if

      len = im*jm

! Look for space between or after used segments
!----------------------------------------------

      do i=1,NumSegments-1
         if(heap%HP_start(i)<0) exit  ! found the last segment
         if(i==1              ) cycle ! first segment is taken
         ! Beginning at the second segment, check if there is room
         ! between the previous and current segments
         gap = heap%HP_start(i)-heap%HP_end(i-1)-1
         if(gap >= len) exit
      end do

      if(I>=NumSegments) then
         print *, 'MAPL_Alloc: Out of Segments. Need: ', I
         ASSERT_(.false.)
      end if

! If we are filling a gap, move trailing segments down
!-----------------------------------------------------

      if(heap%HP_start(i)>0) then
         j = NumSegments-2
         do while (j >= i)
            heap%HP_start(j+1) = heap%HP_start(j)
            heap%HP_end  (j+1) = heap%HP_end  (j)
            j = j-1
         end do
      end if

! Record the start and end of the segment
!----------------------------------------

      if(i == 1) then
         heap%HP_start(i) = 1
      else
         heap%HP_start(i) = heap%HP_end(i-1) + 1
      end if

      heap%HP_end(i) =  heap%HP_start(i) + (len-1)

! Pass that space back in the pointer
!------------------------------------
 
      if(heap%HP_end(i) > size(heap%buffer)) then
         print *, 'MAPL_Alloc: Out of Space. Need: ', len, &
                  ' Have: ',  size(heap%buffer)-heap%HP_start(i)+1
         do i=1,NumSegments
            print *,  i, heap%HP_start(i),  heap%HP_end(i)
         end do
         ASSERT_(.false.)
      end if
 
      heap%ptrs(i)%a => heap%buffer(heap%HP_start(i):heap%HP_end(i))

      a => loadr2d(heap%buffer(heap%HP_start(i)),im,jm)


      RETURN_(ESMF_SUCCESS)
    end subroutine MAPL_Alloc_R_2D


    
    subroutine MAPL_DeAlloc_R_2D(A, RC)

      real, pointer                  :: A(:,:)
      integer, optional, intent(OUT) :: RC

      interface ival2
         integer*8 function ival2(Ptr)
           real, pointer :: Ptr(:,:)
         end function ival2
      end interface

      interface ival1
         integer*8 function ival1(Ptr)
           real, pointer :: Ptr(:)
         end function ival1
      end interface

      integer :: i

      character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_DeAlloc_R_2D"
      integer :: status

! Look for the pointer in the list of allocated segments
!-------------------------------------------------------

      i = 1
      do while ( ival2(a) /= ival1(heap%ptrs(i)%a) )
         i = i+1
         if(i==NumSegments) then
            print *, 'MAPL_DeAlloc: Bad Pointer'
            ASSERT_(.false.)
         end if
      end do

! I is the segment to be freed. If it is not the last one,
! move up all segments below it.
!---------------------------------------------------------

      do while (heap%HP_start(i+1) /= -1)
         heap%HP_start(i) = heap%HP_start(i+1)
         heap%HP_end  (i) = heap%HP_end  (i+1)
         i = i+1
         if(i==NumSegments-1) then
            print *, 'MAPL_DeAlloc: Something wrong. Missed bottom mark'
            ASSERT_(.false.)
         end if
      end do

! Mark bottom segment as free
!----------------------------

      heap%HP_start(I) = -1
      heap%HP_end  (I) = -1

      RETURN_(ESMF_SUCCESS)
    end subroutine MAPL_DeAlloc_R_2D



  end module MAPL_HeapMod


  integer(kind=8) function ival1(i)
    implicit none
    integer(kind=8), intent(IN) :: I(ADDRS_POSITION)
    ival1 = i(ADDRS_POSITION)
  end function ival1

  integer(kind=8) function ival2(i)
    implicit none
    integer(kind=8), intent(IN) :: I(ADDRS_POSITION)
    ival2 = i(ADDRS_POSITION)
  end function ival2

  integer(kind=8) function ival3(i)
    implicit none
    integer(kind=8), intent(IN) :: I(ADDRS_POSITION)
    ival3 = i(ADDRS_POSITION)
  end function ival3


  function loadr1d(a,i  ) result(ptr2)
    implicit none
    integer, intent(IN) :: i
    real, target, intent(IN) :: a(I)
    real, pointer :: ptr2(:)
    ptr2 => a
  end function loadr1d

  function loadr2d(a,i,j) result(ptr2)
    implicit none
    integer, intent(IN) :: i,j
    real, target, intent(IN) :: a(i,j)
    real, pointer :: ptr2(:,:)
    ptr2 => a
  end function loadr2d

  function loadr3d(a,i,j,k) result(ptr2)
    implicit none
    integer, intent(IN) :: i,j,k
    real, target, intent(IN) :: a(i,j,k)
    real, pointer :: ptr2(:,:,:)
    ptr2 => a
  end function loadr3d
