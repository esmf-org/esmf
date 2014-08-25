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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_parDOT - parallel dot-product
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_parDOT
      use m_realkinds, only: DP
      use m_realkinds, only: SP
      implicit none
      private	! except

      public :: parDOT		! The class data structure
      public :: parNRM2

      public :: parL1_norm	! l1-norm
      public :: parL2_norm	! l2-norm
      public :: parL3_norm	! l3-norm
      public :: parLi_norm	! l-infinity-norm

      interface parDOT ; module procedure	&
	vdotmDP_,		&
	vdotmSP_,		&
	vdotDP_ ,		&
	vdotSP_ ; end interface
      interface parNRM2; module procedure	&
	vnrm2mDP_,	&
	vnrm2mSP_,	&
	vnrm2DP_,	&
	vnrm2SP_; end interface

      interface parL1_norm; module procedure	&
	vnrm1mDP_,	&
	vnrm1mSP_,	&
	vnrm1DP_,	&
	vnrm1SP_; end interface
      interface parL2_norm; module procedure	&
	vnrm2mDP_,	&
	vnrm2mSP_,	&
	vnrm2DP_,	&
	vnrm2SP_; end interface
      interface parL3_norm; module procedure	&
	vnrm3mDP_,	&
	vnrm3mSP_,	&
	vnrm3DP_,	&
	vnrm3SP_; end interface
      interface parLi_norm; module procedure	&
	vnrmimDP_,	&
	vnrmimSP_,	&
	vnrmiDP_,	&
	vnrmiSP_; end interface

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_parDOT'

contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vdotSP_ - parallel dot-product
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vdotSP_(x,y,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die
      implicit none
      real(SP),dimension(:),intent(in) :: x
      real(SP),dimension(:),intent(in) :: y
      integer,intent(in) :: comm
      real(SP) :: vdotSP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vdotSP_'
  integer :: ier
  real(SP) :: sdot,rdot

  sdot=dot_product(x,y)

  call MPI_allreduce(sdot,rdot,1,MP_TYPE(sdot),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce()',ier)

  vdotSP_=rdot

end function vdotSP_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vdotmSP_ - parallel dot-product for multiple vectors
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vdotmSP_(x,y,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die,die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      implicit none
      real(SP),dimension(:,:),intent(in) :: x	! (n,nvecs)
      real(SP),dimension(:,:),intent(in) :: y	! (n,nvecs)
      integer,intent(in) :: comm
      real(SP),dimension(size(x,2)) :: vdotmSP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vdotmSP_'
  integer :: i,ier
  integer :: nvecs
  real(SP),allocatable,dimension(:) :: sdot,rdot

  nvecs=size(x,2)
  if(nvecs==0) return

	allocate(sdot(nvecs),rdot(nvecs),stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)
		if(mall_ison()) then
		  call mall_mci(sdot,myname)
		  call mall_mci(rdot,myname)
		endif

  do i=1,nvecs
    sdot(i)=dot_product(x(:,i),y(:,i))
  end do

  call MPI_allreduce(sdot,rdot,nvecs,MP_TYPE(sdot),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce()',ier)

  vdotmSP_(:)=rdot(:)

		if(mall_ison()) then
		  call mall_mco(sdot,myname)
		  call mall_mco(rdot,myname)
		endif
	deallocate(sdot,rdot,stat=ier)
		if(ier/=0) call die(myname_,'deallocate()',ier)

end function vdotmSP_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vdotDP_ - parallel dot-product
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vdotDP_(x,y,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die
      implicit none
      real(DP),dimension(:),intent(in) :: x
      real(DP),dimension(:),intent(in) :: y
      integer,intent(in) :: comm
      real(DP) :: vdotDP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vdotDP_'
  integer :: ier
  real(DP) :: sdot,rdot

  sdot=dot_product(x,y)

  call MPI_allreduce(sdot,rdot,1,MP_TYPE(sdot),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce()',ier)

  vdotDP_=rdot

end function vdotDP_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vdotmDP_ - parallel dot-product for multiple vectors
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vdotmDP_(x,y,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die,die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      implicit none
      real(DP),dimension(:,:),intent(in) :: x	! (n,nvecs)
      real(DP),dimension(:,:),intent(in) :: y	! (n,nvecs)
      integer,intent(in) :: comm
      real(DP),dimension(size(x,2)) :: vdotmDP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vdotmDP_'
  integer :: i,ier
  integer :: nvecs
  real(SP),allocatable,dimension(:) :: sdot,rdot

  nvecs=size(x,2)
  if(nvecs==0) return

	allocate(sdot(nvecs),rdot(nvecs),stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)
		if(mall_ison()) then
		  call mall_mci(sdot,myname)
		  call mall_mci(rdot,myname)
		endif

  do i=1,nvecs
    sdot(i)=dot_product(x(:,i),y(:,i))
  end do

  call MPI_allreduce(sdot,rdot,nvecs,MP_TYPE(sdot),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce()',ier)

  vdotmDP_(:)=rdot(:)

		if(mall_ison()) then
		  call mall_mco(sdot,myname)
		  call mall_mco(rdot,myname)
		endif
	deallocate(sdot,rdot,stat=ier)
		if(ier/=0) call die(myname_,'deallocate()',ier)

end function vdotmDP_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm1SP_ - parallel l1-norm with a single vector
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm1SP_(x,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die
      implicit none
      real(SP),dimension(:),intent(in) :: x
      integer,intent(in) :: comm
      real(SP) :: vnrm1SP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm1SP_'
  real(SP) :: sumx
  integer :: ier

  sumx=0.
  if(size(x)>0) sumx=sum(abs(x))

  call MPI_allreduce((sumx),sumx,1,MP_type(sumx),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  vnrm1SP_=sumx
end function vnrm1SP_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm1mSP_ - parallel l1-norm with multiple vectors
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm1mSP_(x,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die,die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      implicit none
      real(SP),dimension(:,:),intent(in) :: x	! (n,nvecs)
      integer,intent(in) :: comm
      real(SP),dimension(size(x,2)) :: vnrm1mSP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm1mSP_'
  integer :: nvecs
  integer :: i,ier
  real(SP),allocatable,dimension(:) :: sumx

  nvecs=size(x,2)
  if(nvecs==0) return

	allocate(sumx(nvecs),stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)
		if(mall_ison()) call mall_mci(sumx,myname)

  sumx(:)=0.
  if(size(x)>0) then
    do i=1,nvecs
      sumx(i)=sum(abs(x(:,i)))
    end do
  endif

  call MPI_allreduce((sumx),sumx,nvecs,MP_type(sumx),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  vnrm1mSP_(:)=sumx(:)

		if(mall_ison()) call mall_mco(sumx,myname)
	deallocate(sumx,stat=ier)
		if(ier/=0) call die(myname_,'deallocate()',ier)

end function vnrm1mSP_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm1DP_ - parallel l1-norm with a single vector
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm1DP_(x,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die
      implicit none
      real(DP),dimension(:),intent(in) :: x
      integer,intent(in) :: comm
      real(DP) :: vnrm1DP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm1DP_'
  real(DP) :: sumx
  integer :: ier

  sumx=0.d0
  if(size(x)>0) sumx=sum(abs(x))

  call MPI_allreduce((sumx),sumx,1,MP_type(sumx),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  vnrm1DP_=sumx
end function vnrm1DP_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm1mDP_ - parallel l1-norm with multiple vectors
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm1mDP_(x,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die,die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      implicit none
      real(DP),dimension(:,:),intent(in) :: x	! (n,nvecs)
      integer,intent(in) :: comm
      real(DP),dimension(size(x,2)) :: vnrm1mDP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm1mDP_'
  integer :: nvecs
  integer :: i,ier
  real(DP),allocatable,dimension(:) :: sumx

  nvecs=size(x,2)
  if(nvecs==0) return

	allocate(sumx(nvecs),stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)
		if(mall_ison()) call mall_mci(sumx,myname)

  sumx(:)=0.d0
  if(size(x)>0) then
    do i=1,nvecs
      sumx(i)=sum(abs(x(:,i)))
    end do
  endif

  call MPI_allreduce((sumx),sumx,nvecs,MP_type(sumx),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  vnrm1mDP_(:)=sumx(:)

		if(mall_ison()) call mall_mco(sumx,myname)
	deallocate(sumx,stat=ier)
		if(ier/=0) call die(myname_,'deallocate()',ier)

end function vnrm1mDP_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm2SP_ - parallel l2-norm with a single vector
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm2SP_(x,comm)
      implicit none
      real(SP),dimension(:),intent(in) :: x
      integer,intent(in) :: comm
      real(SP) :: vnrm2SP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm2SP_'

  vnrm2SP_=sqrt(vdotSP_(x,x,comm))

end function vnrm2SP_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm2mSP_ - parallel l2-norm with multiple vectors
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm2mSP_(x,comm)
      implicit none
      real(SP),dimension(:,:),intent(in) :: x	! (n,nvecs)
      integer,intent(in) :: comm
      real(SP),dimension(size(x,2)) :: vnrm2mSP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm2mSP_'

  vnrm2mSP_(:)=sqrt(vdotmSP_(x,x,comm))

end function vnrm2mSP_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm2DP_ - parallel l2-norm with a single vector
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm2DP_(x,comm)
      implicit none
      real(DP),dimension(:),intent(in) :: x
      integer,intent(in) :: comm
      real(DP) :: vnrm2DP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm2DP_'

  vnrm2DP_=sqrt(vdotDP_(x,x,comm))

end function vnrm2DP_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm2mDP_ - parallel l2-norm with multiple vectors
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm2mDP_(x,comm)
      implicit none
      real(DP),dimension(:,:),intent(in) :: x	! (n,nvecs)
      integer,intent(in) :: comm
      real(DP),dimension(size(x,2)) :: vnrm2mDP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm2mDP_'

  vnrm2mDP_(:)=sqrt(vdotmDP_(x,x,comm))

end function vnrm2mDP_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm3SP_ - parallel l1-norm with a single vector
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm3SP_(x,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die
      implicit none
      real(SP),dimension(:),intent(in) :: x
      integer,intent(in) :: comm
      real(SP) :: vnrm3SP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm3SP_'
  real(SP) :: sumx
  integer :: ier

  sumx=0.
  if(size(x)>0) sumx=sum(abs(x))

  call MPI_allreduce((sumx),sumx,1,MP_type(sumx),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  vnrm3SP_=sumx
end function vnrm3SP_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm3mSP_ - parallel l1-norm with multiple vectors
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm3mSP_(x,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die,die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      implicit none
      real(SP),dimension(:,:),intent(in) :: x	! (n,nvecs)
      integer,intent(in) :: comm
      real(SP),dimension(size(x,2)) :: vnrm3mSP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm3mSP_'
  integer :: nvecs
  integer :: i,ier
  real(SP),allocatable,dimension(:) :: sumx

  nvecs=size(x,2)
  if(nvecs==0) return

	allocate(sumx(nvecs),stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)
		if(mall_ison()) call mall_mci(sumx,myname)

  sumx(:)=0.
  if(size(x)>0) then
    do i=1,nvecs
      sumx(i)=sum(abs( x(:,i)*x(:,i)*x(:,i) ))
    end do
  endif

  call MPI_allreduce((sumx),sumx,nvecs,MP_type(sumx),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  vnrm3mSP_(:)=sumx(:)**(1./3.)

		if(mall_ison()) call mall_mco(sumx,myname)
	deallocate(sumx,stat=ier)
		if(ier/=0) call die(myname_,'deallocate()',ier)

end function vnrm3mSP_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm3DP_ - parallel l1-norm with a single vector
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm3DP_(x,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die
      implicit none
      real(DP),dimension(:),intent(in) :: x
      integer,intent(in) :: comm
      real(DP) :: vnrm3DP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm3DP_'
  real(DP) :: sumx
  integer :: ier

  sumx=0.d0
  if(size(x)>0) sumx=sum(abs(x))

  call MPI_allreduce((sumx),sumx,1,MP_type(sumx),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  vnrm3DP_=sumx
end function vnrm3DP_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrm3mDP_ - parallel l1-norm with multiple vectors
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrm3mDP_(x,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die,die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      implicit none
      real(DP),dimension(:,:),intent(in) :: x	! (n,nvecs)
      integer,intent(in) :: comm
      real(DP),dimension(size(x,2)) :: vnrm3mDP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrm3mDP_'
  integer :: nvecs
  integer :: i,ier
  real(DP),allocatable,dimension(:) :: sumx

  nvecs=size(x,2)
  if(nvecs==0) return

	allocate(sumx(nvecs),stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)
		if(mall_ison()) call mall_mci(sumx,myname)

  sumx(:)=0.d0
  if(size(x)>0) then
    do i=1,nvecs
      sumx(i)=sum(abs( x(:,i)*x(:,i)*x(:,i) ))
    end do
  endif

  call MPI_allreduce((sumx),sumx,nvecs,MP_type(sumx),MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  vnrm3mDP_(:)=sumx(:)**(1./3.)

		if(mall_ison()) call mall_mco(sumx,myname)
	deallocate(sumx,stat=ier)
		if(ier/=0) call die(myname_,'deallocate()',ier)

end function vnrm3mDP_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrmiSP_ - parallel l<inf>-norm with a single vector
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrmiSP_(x,comm,iPE,loc)
      use m_mpif90,only : MP_2type,MP_comm_rank,MP_MAXLOC
      use m_die   ,only : MP_die
      implicit none
      real(SP),dimension(:),intent(in) :: x
      integer,intent(in) :: comm
      integer,optional,intent(out) :: iPE
      integer,optional,intent(out) :: loc
      real(SP) :: vnrmiSP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrmiSP_'
  real(SP),dimension(2) :: sumx
  integer :: ier,myID

	call MP_comm_rank(comm,myID,ier)
		if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  sumx(1)=0.
  if(size(x)>0) sumx(1)=maxval(abs(x))
  sumx(2)=myID

  call MPI_allreduce((sumx),sumx,1,MP_2type(sumx(1)),MP_MAXLOC,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  if(present(loc)) loc=maxloc(abs(x),dim=1)
  if(present(iPE)) iPE=nint(sumx(2))
  vnrmiSP_=sumx(1)
end function vnrmiSP_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrmimSP_ - parallel l<inf>-norm with multiple vectors
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrmimSP_(x,comm,iPE,loc)
      use m_mpif90,only : MP_2type,MP_comm_rank,MP_MAXLOC
      use m_die   ,only : MP_die,die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      implicit none
      real(SP),dimension(:,:),intent(in) :: x	! (n,nvecs)
      integer,intent(in) :: comm
      integer,dimension(size(x,2)),optional,intent(out) :: iPE
      integer,dimension(size(x,2)),optional,intent(out) :: loc
      real(SP),dimension(size(x,2)) :: vnrmimSP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrmimSP_'
  integer :: nvecs
  integer :: i,ier
  real(SP),allocatable,dimension(:,:) :: sumx

  nvecs=size(x,2)
  if(nvecs==0) return

	allocate(sumx(2,nvecs),stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)
		if(mall_ison()) call mall_mci(sumx,myname)

  sumx(1,:)=0.
  if(size(x)>0) then
    do i=1,nvecs
      sumx(1,i)=maxval(abs(x(:,i)))
    end do
  endif

  call MPI_allreduce((sumx),sumx,nvecs,MP_2type(sumx(1,1)),	&
    MP_MAXLOC,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  if(present(loc)) loc(:)=maxloc(abs(x(:,:)),dim=1)
  if(present(iPE)) iPE(:)=nint(sumx(2,:))
  vnrmimSP_(:)=sumx(1,:)

		if(mall_ison()) call mall_mco(sumx,myname)
	deallocate(sumx,stat=ier)
		if(ier/=0) call die(myname_,'deallocate()',ier)

end function vnrmimSP_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrmiDP_ - parallel l<inf>-norm with a single vector
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrmiDP_(x,comm,iPE,loc)
      use m_mpif90,only : MP_2type,MP_comm_rank,MP_MAXLOC
      use m_die   ,only : MP_die
      implicit none
      real(DP),dimension(:),intent(in) :: x
      integer,intent(in) :: comm
      integer,optional,intent(out) :: iPE
      integer,optional,intent(out) :: loc
      real(DP) :: vnrmiDP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrmiDP_'
  real(DP),dimension(2) :: sumx
  integer :: ier,myID

	call MP_comm_rank(comm,myID,ier)
		if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  sumx(1)=0.d0
  if(size(x)>0) sumx(1)=maxval(abs(x))
  sumx(2)=myID

  call MPI_allreduce((sumx),sumx,1,MP_2type(sumx(1)),MP_MAXLOC,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  if(present(loc)) loc=maxloc(abs(x),dim=1)
  if(present(iPE)) iPE=nint(sumx(2))
  vnrmiDP_=sumx(1)
end function vnrmiDP_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: vnrmimDP_ - parallel l<inf>-norm with multiple vectors
!
! !DESCRIPTION:
!
! !INTERFACE:

    function vnrmimDP_(x,comm,iPE,loc)
      use m_mpif90,only : MP_2type,MP_comm_rank,MP_MAXLOC
      use m_die   ,only : MP_die,die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      implicit none
      real(DP),dimension(:,:),intent(in) :: x	! (n,nvecs)
      integer,intent(in) :: comm
      integer,dimension(size(x,2)),optional,intent(out) :: iPE
      integer,dimension(size(x,2)),optional,intent(out) :: loc
      real(DP),dimension(size(x,2)) :: vnrmimDP_

! !REVISION HISTORY:
! 	23May00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::vnrmimDP_'
  integer :: nvecs
  integer :: i,ier
  real(DP),allocatable,dimension(:,:) :: sumx

  nvecs=size(x,2)
  if(nvecs==0) return

	allocate(sumx(2,nvecs),stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)
		if(mall_ison()) call mall_mci(sumx,myname)

  sumx(1,:)=0.d0
  if(size(x)>0) then
    do i=1,nvecs
      sumx(1,i)=maxval(abs(x(:,i)))
    end do
  endif

  call MPI_allreduce((sumx),sumx,nvecs,MP_2type(sumx(1,1)),	&
    MP_MAXLOC,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(sumx)',ier)

  if(present(loc)) loc(:)=maxloc(abs(x(:,:)),dim=1)
  if(present(iPE)) iPE(:)=nint(sumx(2,:))
  vnrmimDP_(:)=sumx(1,:)

		if(mall_ison()) call mall_mco(sumx,myname)
	deallocate(sumx,stat=ier)
		if(ier/=0) call die(myname_,'deallocate()',ier)

end function vnrmimDP_

end module m_parDOT
