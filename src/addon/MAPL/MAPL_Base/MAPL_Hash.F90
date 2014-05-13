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
#define INT_MAX 2147483647

!  $Id: MAPL_Hash.F90,v 1.2 2012-06-13 18:19:19 ltakacs Exp $

!=============================================================================
!BOP

! !MODULE: MAPL_Hash   -- A utility to manage hash tables

! !INTERFACE:

module MAPL_HashMod

  implicit none
  private

! !PUBLIC ROUTINES:

  public MAPL_HashCreate
  public MAPL_HashIncrement
  public MAPL_HashDestroy
  public MAPL_HashSize
  public MAPL_HashDump

! !DESCRIPTION:
! 
!   {\tt MAPL\_HashMod} is a FORTRAN binding to a simple C has facility.
!   The API is:
!\bv
!
!   ! Create a hash table with Nbuckets
!
!       integer function MAPL_HashCreate(Nbuckets)
!         integer, intent(IN) :: Nbuckets
! 
!   ! Update table Hash with integer[s] i[,j]
!   ! The return value is the order of occurence of the integer[s].
!   ! If i is new, the return value is the new hash size.
!
!       integer function MAPL_HashIncrement(Hash,i,j)
!         integer,           intent(IN) :: Hash
!         integer,           intent(IN) :: i
!         integer, optional, intent(IN) :: j
!
!   ! Dump the list of integers or integer pairs in the hash.
!   !  The list is in no particular order.
!   ! If the arrays are not long enough, nothing is dumped and -1
!   !  is returned; otherwise it returns the current hash size 
!   !  (the length of the list).
!
!       integer function MAPL_HashDump(Hash,i,j)
!         integer,           intent(IN)  :: Hash
!         integer,           intent(OUT) :: i(:)
!         integer, optional, intent(OUT) :: j(:)
!
!   ! Get the size of a hash
!
!       integer function MAPL_HashSize(Hash)
!         integer, intent(IN) :: Hash
!
!   ! Destroy a hash table
!
!       subroutine MAPL_HashDestroy(Hash)
!         integer, intent(IN) :: Hash
!\ev
!
!  The following is a sample usage that makes a list of
!  unique integers in the large array II. It can similarly
!  be used to find ordered pairs of integers. The asserts
!  are put in to clarify the usage.
!
!\bv       
!       integer :: Hash, k, II(100000), FoundOrder(10000)
!
!       Hash = MAPL_HashCreate(1000)
!
!       latest = 0
!       do i=1,100000
!         k = MAPL_HashIncrement(Hash,ii(i))
!         if(k>latest) then
!           latest   = k
!           isnew    = .true.
!           FoundOrder(k) = ii(i)
!           ASSERT_(k==MAPL_HashSize(Hash))
!         else
!           isnew = .false.
!           ASSERT_(FoundOrder(k)==ii(i))
!         endif
!       enddo
!
!
!\ev

!EOP
!=============================================================================

contains

integer function  MAPL_HashCreate(Nbuckets)
  integer,           intent(IN) :: Nbuckets

  integer CREATEHASH
  MAPL_HashCreate = CREATEHASH(Nbuckets)

end function MAPL_HashCreate

!----------------------------------------------

integer function MAPL_HashIncrement(Hash,i,j,k)
  integer,           intent(IN) :: Hash
  integer,           intent(IN) :: i
  integer, optional, intent(IN) :: j
  integer, optional, intent(IN) :: k

  integer INCREMENTHASH

  if    (present(k)) then
     ASSERT_(present(j))
     MAPL_HashIncrement = INCREMENTHASH(HASH,I,J,K)
  elseif(present(j)) then
     MAPL_HashIncrement = INCREMENTHASH(HASH,I,J,INT_MAX)
  else
     MAPL_HashIncrement = INCREMENTHASH(HASH,I,INT_MAX,INT_MAX)
  endif

end function MAPL_HashIncrement

!----------------------------------------------

subroutine MAPL_HashDestroy(Hash)
  integer, intent(IN) :: Hash

  call DESTROYHASH(HASH)

end subroutine MAPL_HashDestroy

!----------------------------------------------

integer function MAPL_HashDump(Hash,i,j)
  integer, intent(IN ) :: Hash
  integer, intent(OUT) :: i(:)
  integer, optional, intent(OUT) :: j(:)

  integer, allocatable :: jj(:)

  MAPL_HashDump = MAPL_HashSize(HASH)

  if(size(i) < MAPL_HashSize(HASH)) then
     MAPL_HashDump = -1
     return
  end if

  if(present(j)) then
     ASSERT_(size(i) == size(j))
     call DUMPHASH(HASH,I,J)
  else
     allocate(jj(size(i)))
     call DUMPHASH(HASH,I,JJ)
     deallocate(JJ)
  end if

end function MAPL_HashDump

!----------------------------------------------

integer function  MAPL_HashSize(Hash)
  integer, intent(IN) :: Hash

  integer HASHSIZE
  MAPL_HashSize = HASHSIZE(Hash)

end function MAPL_HashSize

!----------------------------------------------

end module MAPL_HashMod
