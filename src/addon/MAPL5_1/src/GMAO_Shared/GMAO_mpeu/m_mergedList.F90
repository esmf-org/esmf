!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_mergedList - Merged lists from multiple PEs
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_mergedList
      implicit none
      private	! except

      public :: mergedList		! The class data structure
      public :: mergedList_init,init	! create mergedList
      public :: clean

      public :: ptr_indx
      public :: ptr_list
      public :: msize
      public :: lsize

    type mergedList
      private
      integer :: len				! list entry size
      integer :: msize				! merged size (%list)
      integer :: lsize				! local size (%indx)
      integer :: root
      character(len=1),pointer,dimension(:,:) :: list
						! a merged list
      integer         ,pointer,dimension(:  ) :: indx
						! indices of local list
						! entries in the merged
						! list
    end type mergedList

    interface mergedList_init ; module procedure merge_; end interface
    interface init ; module procedure merge_; end interface
    interface clean; module procedure clean_; end interface

    interface ptr_indx; module procedure ptr_indx_; end interface
    interface ptr_list; module procedure ptr_list_; end interface

    interface msize; module procedure msize_; end interface
    interface lsize; module procedure lsize_; end interface

! !REVISION HISTORY:
! 	13Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_mergedList'

contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: merge_ - merge distributed list to an indexed common list
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine merge_(names,merged,root,comm)
      use m_die ,only : MP_die,die
      use m_mall,only : mall_ison,mall_mci,mall_mco
      use m_mpif90,only : MP_comm_size
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_MAX
      implicit none

      character(len=*),dimension(:),intent(in) :: names
				! a local list

      type(mergedList),intent(out) :: merged
				! a merged list

      integer,intent(in) :: root
      integer,intent(in) :: comm

! !REVISION HISTORY:
! 	06Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::merge_'
  integer :: nPEs,myID
  integer :: ier
  integer :: iPE,i,k,m
  integer :: iname	! location index of a given name
  integer :: lsize	! the logical local size of names(:)

  integer :: mptype
  integer :: mcount

  integer :: nmax	! the maximum {lsize} over all PEs
  integer :: lenx	! len(names) known by the root PE.
  integer :: msize	! merged list size
  integer         ,allocatable,dimension(:)     :: nname
  character(len=1),allocatable,dimension(:,:)   :: name0
  character(len=1),allocatable,dimension(:,:,:) :: nameA
  integer         ,allocatable,dimension(:,:)	:: indxA
!________________________________________
			! Initialize the object
  merged%msize=0
  merged%lsize=0
  merged%len  =0
  merged%root =-1
  nullify(merged%list)
  nullify(merged%indx)
!________________________________________

  call MP_comm_size(comm,nPEs,ier)
	if(ier/=0) call MP_die(myname_,'MP_comm_size()',ier)

  call MP_comm_rank(comm,myID,ier)
	if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)
!________________________________________

	! Share the len(names) of the root PE, with all PEs.

	lenx=len(names)
	mptype=MP_type(lenx)

  call MPI_bcast(lenx,1,mptype,root,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_bcast(lenx)',ier)

	! Get the max(size(names))

	lsize=size(names)
	mptype=MP_type(lsize)

  call MPI_allreduce(lsize,nmax,1,mptype,MP_MAX,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(nmax)',ier)

!________________________________________

	! Allocate workspace.  Note the dimensions for all allocatable
	! variables are different, because they are used for different
	! purposes when they are on different PEs.

  if(myID==root) then
	allocate( nname(0:nPEs-1),nameA(lenx,nmax,0:nPEs-1),indxA(nmax,0:nPEs-1), stat=ier)
		if(ier/=0) call die(myname_,'allocate(nameA)',ier)
		if(mall_ison()) then
		  call mall_mci(nname,myname)
		  call mall_mci(nameA,myname)
		  call mall_mci(indxA,myname)
		endif

	allocate( name0(lenx,nmax*nPEs),stat=ier)
		if(ier/=0) call die(myname_,'allocate(name0)',ier)
		if(mall_ison()) call mall_mci(name0,myname)
  else
	allocate( nname(0), nameA(lenx,0,0), indxA(0,0), stat=ier)
		if(ier/=0) call die(myname_,'allocate(nameA)',ier)
		if(mall_ison()) then
		  call mall_mci(nname,myname)
		  call mall_mci(nameA,myname)
		  call mall_mci(indxA,myname)
		endif

	allocate( name0(lenx,nmax),stat=ier)
		if(ier/=0) call die(myname_,'allocate(name0)',ier)
		if(mall_ison()) call mall_mci(name0,myname)
  endif
!________________________________________

	! Collect list sizes from all PEs, to root.

	mptype=MP_type(lsize)

  call MPI_gather(lsize,1,mptype,	&
		  nname,1,mptype, root,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_gather(lsize)',ier)

	! Use a subsection of name0(:,:) as a buffer for names(:).

	name0(:,nmax)=""
	do i=1,lsize
	  do k=1,min(lenx,len(names))
	    name0(k,i)=names(i)(k:k)
	  end do
	end do

	! Collect name lists on all PEs to the root PE.

	mcount=nmax*lenx	! a subsection of name0(:,:)
	mptype=MP_type(name0)

  call MPI_gather( name0,mcount,mptype,		&
		   nameA,mcount,mptype, root,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_gather(name0)',ier)

	! Merge the gathered lists on the root PE.

  msize=0	! this is the merged list size on this PE.

  if(myID==root) then

    name0(:,:)=""
    indxA(:,:)=-1	! for all entries not defined on a PE.

	! Merge all entries to a single list, by looking up a name
	! in the merged list.  Note that the current implementation
	! uses a simple minded algorithm, which is not efficient at all.

    do iPE=0,nPEs-1
      do i=1,nname(iPE)
			! lookup or append a name to list name0(:)

	iname=-1

	lookup:	do m=1,msize
	  if(all(nameA(:,i,iPE) == name0(:,m))) then
	    iname=m
	    exit lookup
	  endif
	end do lookup

	if(iname==-1) then
	  msize=msize+1
	  iname=msize
	  name0(:,iname)=nameA(:,i,iPE)
	endif

        indxA(i,iPE)=iname	! Location of an entry in name0(:)

      end do
    end do
  endif

  if(myID==root) then

	allocate(merged%list(lenx,msize),stat=ier)
		if(ier/=0) call die(myname_,'allocate(%list)',ier)

		if(mall_ison()) call mall_mci(merged%list,myname)

    do i=1,msize
      merged%list(:,i)=name0(:,i)
    end do

  else
	nullify(merged%list)
  endif

	! Let the merged size known by all PEs.

	mptype=MP_type(msize)

  call MPI_bcast(msize,1,mptype,root,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_bcast()',ier)

  merged%msize=msize	! merged size of %list(:) on the root PE.
  merged%lsize=lsize	! local logical size of %indx(:)
  merged%len  =lenx	! string length
  merged%root =root	! where the merged list is defined.
!________________________________________

	allocate(merged%indx(nmax),stat=ier)
		if(ier/=0) call die(myname_,'allocate(indx)',ier)

		if(mall_ison()) call mall_mci(merged%indx,myname)

	! Let every processor to know the locations of its own name
	! list entries in the merged name list on the root (name0(:)).

	mptype=MP_type(merged%indx)
	mcount=size(merged%indx,1)

  call MPI_scatter(       indxA,mcount,mptype,	&
		    merged%indx,mcount,mptype,	&
		    root,comm,ier)
		if(ier/=0) call MP_die(myname_,'MPI_scatter()',ier)

		if(mall_ison()) then
		  call mall_mco(nname,myname)
		  call mall_mco(nameA,myname)
		  call mall_mco(indxA,myname)
		endif
	deallocate(nname,nameA,indxA,stat=ier)
		if(ier/=0) call die(myname_,'deallocate(nameA)',ier)

		if(mall_ison()) call mall_mco(name0,myname)
	deallocate(name0,stat=ier)
		if(ier/=0) call die(myname_,'deallocate(name0)',ier)
!________________________________________
end subroutine merge_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: clean_ - clean an object
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine clean_(merged)
      use m_die ,only : die
      use m_mall,only : mall_ison,mall_mco
      implicit none
      type(mergedList),intent(inout) :: merged

! !REVISION HISTORY:
! 	13Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::clean_'
  integer :: ier

  if(associated(merged%list)) then
	if(mall_ison()) call mall_mco(merged%list,myname)
    deallocate(merged%list,stat=ier)
	if(ier/=0) call die(myname_,'deallocate(%list)',ier)
  endif

	if(mall_ison()) call mall_mco(merged%indx,myname)
    deallocate(merged%indx,stat=ier)
	if(ier/=0) call die(myname_,'deallocate(%indx)',ier)

  merged%msize=0
  merged%lsize=0
  merged%len  =0
  merged%root =-1

end subroutine clean_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ptr_indx_ - referencing %indx array of the merged %list.
!
! !DESCRIPTION:
!
! !INTERFACE:

    function ptr_indx_(obj)
      implicit none
      type(mergedList),intent(in) :: obj
      integer,pointer,dimension(:) :: ptr_indx_

! !REVISION HISTORY:
! 	13Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::ptr_indx_'

  ptr_indx_ => obj%indx

end function ptr_indx_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ptr_list_ - referencing the merged %list
!
! !DESCRIPTION:
!
! !INTERFACE:

    function ptr_list_(obj)
      use m_die,only : die
      implicit none
      type(mergedList),intent(in) :: obj
      character(len=1),pointer,dimension(:,:) :: ptr_list_

! !REVISION HISTORY:
! 	13Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::ptr_list_'

	! If this function is invoked not on the root PE, component
	! %list is not associated.

  if(.not.associated(obj%list)) call die(myname_,'null unless on root')
  ptr_list_ => obj%list

end function ptr_list_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: msize_ - merged list size
!
! !DESCRIPTION:
!
! !INTERFACE:

    function msize_(obj)
      implicit none
      type(mergedList),intent(in) :: obj
      integer :: msize_

! !REVISION HISTORY:
! 	15Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::msize_'

  msize_=obj%msize

end function msize_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: lsize_ - local list size
!
! !DESCRIPTION:
!
! !INTERFACE:

    function lsize_(obj)
      implicit none
      type(mergedList),intent(in) :: obj
      integer :: lsize_

! !REVISION HISTORY:
! 	15Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::lsize_'

  lsize_=obj%lsize

end function lsize_
end module m_mergedList

