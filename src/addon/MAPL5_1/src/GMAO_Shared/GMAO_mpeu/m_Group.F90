!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_Group - Group maker
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_Group
      implicit none
      private	! except

      public :: Group_set	! set initial group IDs (no-group)
      public :: Group_index	! incrementally assign group IDs
      public :: Group_count	! count group members

      interface Group_set  ; module procedure set_  ; end interface
      interface Group_index; module procedure index_; end interface
      interface Group_count; module procedure count_; end interface

! !REVISION HISTORY:
! 	21Sep01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

! !USECASE:
!
! 1) Define a table of size N, sorted by keys such as,
!
!	integer,dimension(N) :: kr,kx,ks
!
!   assuming the table is sorted from the highest to the lowest keys in
!   the order of kr-kx-ks.
!
! 2) To assign group IDs to all table entries, first, initialize a
!   no-group ID (0) to all entries
!
!	use m_Group,only : group_set
!	integer,dimension(N) :: gid
!	call group_set(gid)
!
! 3) Start from the least varying (highest) key, incrementally devide
!   the data into smaller groups with identical key values.  Note that
!   connected table entries with the same keys (kr,kx,ks) are considered
!   to be the members of of the same group.  The table index (1 to N) of
!   the first entry in a group is used as the unique group ID, and
!   assigned to all members of the group.
!
!	use m_Group,only : group_index
!	call group_index(kr,gid)
!	call group_index(kx,gid)
!	call group_index(ks,gid)
!
! 4) Locate groups by creating a Navigator if needed.
!
!	use m_Group,only : group_count
!	use m_Navigator,only : Navigator,Navigator_init,define
!	type(Navigator) :: gnav
!	integer :: ngrp
!	integer,dimension(size(gid)) :: counts,displs
!
!	call group_count(gid,lsize=ngrp,counts=counts,displs=displs)
!	call Navigator_init(gnav,ngrp)
!	call define(gnav,counts=counts(1:ngrp))
!
!   Note that all groups have at least one member.
!
! 5) Access the group attributes (keys) if needed.
!
!	use m_Navigator,only : get
!	integer,dimension(M) :: krtabl,kxtabl,kstabl
!	integer :: i,l
!	do i=1,M
!	  call get(gnav),i,lc=l)
!	  krtabl(i)=kr(gid(l))
!	  kxtabl(i)=kx(gid(l))
!	  kstabl(i)=ks(gid(l))
!	end do
!
! 7) Clean up all dynamic objects.

  character(len=*),parameter :: myname='m_Group'

#include "assert.H"
contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: set_ - create initial (no) group IDs
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine set_(gid)
      implicit none
      integer,dimension(:),intent(out) :: gid

! !REVISION HISTORY:
! 	21Sep01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::set_'

  gid(:)=0
end subroutine set_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: index_ - incrementally determine groups
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine index_(key,gid)
      use m_die,only : assert_
      implicit none
      integer,dimension(:),intent(in)    :: key
      integer,dimension(:),intent(inout) :: gid

! !REVISION HISTORY:
! 	21Sep01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::index_'
  integer :: n,i,isave,jsave,ksave

	ASSERT(size(key)==size(gid))

  n=size(key)
  if(n==0) return

  isave=1
  jsave=gid(1)
  ksave=key(1)

  do i=2,n
    gid(i-1)=isave
    if(jsave/=gid(i) .or. ksave/=key(i)) then
      isave=i
      jsave=gid(i)
      ksave=key(i)
    endif
  end do

  gid(n)=isave

end subroutine index_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: count_ - count group members
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine count_(gid,lsize,counts,displs)
      use m_die,only : assert_
      implicit none
      integer,dimension(:),intent(in) :: gid

      integer,intent(out) :: lsize
      integer         ,dimension(:),intent(out) :: counts
      integer,optional,dimension(:),intent(out) :: displs

! !REVISION HISTORY:
! 	21Sep01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::count_'
  integer :: i,k,m,n

  n=size(gid)
  m=size(counts)

#ifndef NDEBUG
  if(present(displs)) then
    ASSERT(size(displs)==size(counts))
  endif
#endif

  k=1
  if(k<=m) counts(k)=1
  do i=2,n
    if(gid(i-1)/=gid(i)) then
      k=k+1
      if(k<=m) counts(k)=0
    endif
    if(k<=m) counts(k)=counts(k)+1
  end do
  lsize=k

  if(present(displs)) then
    if(lsize==0) return
    k=1
    if(k<=m) displs(k)=0
    do k=2,lsize
      if(k<=m) displs(k)=displs(k-1)+counts(k-1)
    end do
  endif

end subroutine count_
end module m_Group
