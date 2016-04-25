!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_showDistrib - show distributions
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_showDistrib
      implicit none
      private	! except

      public :: showDistrib		! The class data structure

      interface showDistrib; module procedure	&
	showalli_,	&
	showallr_,	&
	showalld_
      end interface

! !REVISION HISTORY:
! 	02Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_showDistrib'

contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: showalli_ - show all INTEGER values
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine showalli_(lu,vnam,ival,root,comm,listall)
      use m_mpif90,only : MP_comm_size
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_type
      use m_die   ,only : MP_die,die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      implicit none
      integer,intent(in) :: lu		! output unit
      character(len=*),intent(in) :: vnam	! variable name
      integer,intent(in) :: ival	! to be listed value
      integer,intent(in) :: root	! root PE
      integer,intent(in) :: comm	! communicator
      logical,optional,intent(in) :: listall

! !REVISION HISTORY:
! 	02Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::showalli_'
  integer :: i,j,k,m
  integer :: nPEs,mPEs
  integer :: myID
  integer :: ier
  integer,allocatable,dimension(:) :: ivals
  real :: sum
  integer :: iav,imx,iov,iPE
  integer :: imbx,imbr,imbi

  call MP_comm_size(comm,nPEs,ier)
	if(ier/=0) call MP_die(myname_,'MP_comm_size()',ier)
  call MP_comm_rank(comm,myID,ier)
	if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  mPEs=0
  if(myID==root) mPEs=nPEs

	allocate(ivals(0:mPEs-1),stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)
		if(mall_ison()) call mall_mci(ivals,myname)

  call MPI_gather(ival,1,MP_type(ival),ivals,1,MP_type(ivals),  &
	root,comm,ier)
		if(ier/=0) call MP_die(myname_,'MPI_gather()',ier)

  if(myID==root) then
    iPE=0
    sum=0.
    do i=0,mPEs-1
      sum=sum+max(0,ivals(i))
      if(ivals(iPE)<ivals(i)) iPE=i
    end do
    iav=nint(sum/mPEs)
    imx=ivals(iPE)

    iov=0
    do i=0,mPEs-1
      if(ivals(i) > iav) iov = (ivals(i)-iav) + iov
    end do

    imbx=nint(100.*(imx-iav)/max(sum,1.))
    imbr=nint(100.*(iov    )/max(sum,1.))
    imbi=nint(100.*(imx-iav)/max(real(imx),1.))

    write(lu,'(2a,i4,2a)')	  myname,': ',mPEs,' x ',trim(vnam)
    write(lu,'(2a,2(a,i9),a,i4)') myname,': ',		&
	'avg=',iav ,'  max=',imx ,'  iPE=',iPE
    write(lu,'(2a,3(a,i2.2))')	  myname,': ',		&
	' x%=',imbx,'   r%=',imbr,'   i%=',imbi

    if(present(listall)) then
      if(listall) then
	write(lu,'(3x,8o9.2)') (k,k=0,min(8,mPEs)-1)

	do i=0,mPEs-1,8
	  m=min(i+8,mPEs)-1
	  j=i/8
	  write(lu,'(o3.3,1x,8i9)') j,(ivals(k),k=i,m)
	end do
      endif
    endif

  endif

		if(mall_ison()) call mall_mco(ivals,myname)
	deallocate(ivals,stat=ier)
		if(ier/=0) call die(myname_,'deallocate()',ier)

end subroutine showalli_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: showallr_ - show all INTEGER values
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine showallr_(lu,vnam,rval,root,comm,listall,scalar)
      use m_mpif90,only : MP_comm_size
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_type
      use m_die   ,only : MP_die,die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      use m_realkinds,only : SP
      implicit none
      integer,intent(in) :: lu		! output unit
      character(len=*),intent(in) :: vnam	! variable name
      real(SP),intent(in) :: rval	! to be listed value
      integer,intent(in) :: root	! root PE
      integer,intent(in) :: comm	! communicator
      logical,optional,intent(in) :: listall
      real(SP),optional,intent(in) :: scalar

! !REVISION HISTORY:
! 	02Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::showallr_'
  integer :: i,j,k,m
  integer :: nPEs,mPEs
  integer :: myID
  integer :: ier
  real(SP),allocatable,dimension(:) :: rvals
  real(SP) :: efac
  real(SP) :: sum,avg,rmx,rov
  integer :: imbx,imbr,imbi
  integer :: iPE

  call MP_comm_size(comm,nPEs,ier)
	if(ier/=0) call MP_die(myname_,'MP_comm_size()',ier)
  call MP_comm_rank(comm,myID,ier)
	if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  mPEs=0
  if(myID==root) mPEs=nPEs

	allocate(rvals(0:mPEs-1),stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)
		if(mall_ison()) call mall_mci(rvals,myname)

  call MPI_gather(rval,1,MP_type(rval),rvals,1,MP_type(rvals),  &
	root,comm,ier)
		if(ier/=0) call MP_die(myname_,'MPI_gather()',ier)

  if(myID==root) then
    efac=1.
    if(present(scalar)) efac=abs(scalar)

    iPE=0
    sum=0.
    do i=0,mPEs-1
      sum=sum+max(real(0.,kind=SP),rvals(i))
      if(rvals(iPE)<rvals(i)) iPE=i
    end do
    avg=sum/mPEs
    rmx=rvals(iPE)

    rov=0.
    do i=0,mPEs-1
      if(rvals(i) > avg) rov = (rvals(i)-avg) + rov
    end do

    imbx=nint(100.*(rmx-avg)/max(sum,real(1.,kind=SP)))
    imbr=nint(100.*(rov    )/max(sum,real(1.,kind=SP)))
    imbi=nint(100.*(rmx-avg)/max(rmx,real(1.,kind=SP)))

    write(lu,'(2a,i4,2a)')	  myname,': ',mPEs,' x ',trim(vnam)
    write(lu,'(2a,2(a,es12.4),a,i4)') myname,': ',	&
	'avg=',avg ,'  max=',rmx ,'  iPE=',iPE
    write(lu,'(2a,3(a,i2.2))')	  myname,': ',		&
	' x%=',imbx,'   r%=',imbr,'   i%=',imbi

    if(present(listall)) then
      if(listall) then

	write(lu,'(sp,es9.2,ss,o3.2,7o9.2)') efac,(k,k=0,min(8,mPEs)-1)

	efac=1./efac
	do i=0,mPEs-1,8
	  m=min(i+8,mPEs)-1
	  j=i/8
	  write(lu,'(o3.3,1x,8f9.5)') j,(efac*rvals(k),k=i,m)
	end do
      endif
    endif

  endif

		if(mall_ison()) call mall_mco(rvals,myname)
	deallocate(rvals,stat=ier)
		if(ier/=0) call die(myname_,'deallocate()',ier)

end subroutine showallr_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: showalld_ - show all INTEGER values
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine showalld_(lu,vnam,rval,root,comm,listall,scalar)
      use m_mpif90,only : MP_comm_size
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_type
      use m_die   ,only : MP_die,die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      use m_realkinds,only : DP
      implicit none
      integer,intent(in) :: lu		! output unit
      character(len=*),intent(in) :: vnam	! variable name
      real(DP),intent(in) :: rval	! to be listed value
      integer,intent(in) :: root	! root PE
      integer,intent(in) :: comm	! communicator
      logical,optional,intent(in) :: listall
      real(DP),optional,intent(in) :: scalar

! !REVISION HISTORY:
! 	02Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::showalld_'
  integer :: i,j,k,m
  integer :: nPEs,mPEs
  integer :: myID
  integer :: ier
  real(DP),allocatable,dimension(:) :: rvals
  real(DP) :: efac
  real(DP) :: sum,avg,rmx,rov
  integer :: imbx,imbr,imbi
  integer :: iPE

  call MP_comm_size(comm,nPEs,ier)
	if(ier/=0) call MP_die(myname_,'MP_comm_size()',ier)
  call MP_comm_rank(comm,myID,ier)
	if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  mPEs=0
  if(myID==root) mPEs=nPEs

	allocate(rvals(0:mPEs-1),stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)
		if(mall_ison()) call mall_mci(rvals,myname)

  call MPI_gather(rval,1,MP_type(rval),rvals,1,MP_type(rvals),  &
	root,comm,ier)
		if(ier/=0) call MP_die(myname_,'MPI_gather()',ier)

  if(myID==root) then
    efac=1.
    if(present(scalar)) efac=abs(scalar)

    iPE=0
    sum=0.
    do i=0,mPEs-1
      sum=sum+max(real(0.,kind=DP),rvals(i))
      if(rvals(iPE)<rvals(i)) iPE=i
    end do
    avg=sum/mPEs
    rmx=rvals(iPE)

    rov=0.
    do i=0,mPEs-1
      if(rvals(i) > avg) rov = (rvals(i)-avg) + rov
    end do

    imbx=nint(100.*(rmx-avg)/max(sum,real(1.,kind=DP)))
    imbr=nint(100.*(rov    )/max(sum,real(1.,kind=DP)))
    imbi=nint(100.*(rmx-avg)/max(rmx,real(1.,kind=DP)))

    write(lu,'(2a,i4,2a)')	  myname,': ',mPEs,' x ',trim(vnam)
    write(lu,'(2a,2(a,es12.4),a,i4)') myname,': ',	&
	'avg=',avg ,'  max=',rmx ,'  iPE=',iPE
    write(lu,'(2a,3(a,i2.2))')	  myname,': ',		&
	' x%=',imbx,'   r%=',imbr,'   i%=',imbi

    if(present(listall)) then
      if(listall) then

	write(lu,'(sp,es9.2,ss,o3.2,7o9.2)') efac,(k,k=0,min(8,mPEs)-1)

	efac=1./efac
	do i=0,mPEs-1,8
	  m=min(i+8,mPEs)-1
	  j=i/8
	  write(lu,'(o3.3,1x,8f9.5)') j,(efac*rvals(k),k=i,m)
	end do
      endif
    endif

  endif

		if(mall_ison()) call mall_mco(rvals,myname)
	deallocate(rvals,stat=ier)
		if(ier/=0) call die(myname_,'deallocate()',ier)

end subroutine showalld_

end module m_showDistrib
