!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_wGrADS - write-only GrADS files
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_wGrADS
      use m_String, only : String
      implicit none
      private	! except

      public	:: wGrADS	! The class data structure
      public	:: wGrADS_open	! open a wGrADS instance
      public	:: wGrADS_write	! write a wGrADS 2d/3d field
      public	:: wGrADS_close	! close a wGrADS instance
      public    :: inquire
      public    :: bcast

		! The current version only has limited supports to a
		! GrADS file, with restrictions on the grid, time,
		! and the file format, etc.

	integer,parameter :: LEN_VNAME=12

        type wGrADS
	  private
	  logical :: litend = .false.

	  integer		:: idim		! longitude dimension
	  integer		:: jdim		! latitude dimension
	  integer		:: kdim		! level dimension
	  integer		:: nvar		! number of variables
	  integer		:: ivar		! current variable
	  integer		:: ldim		! time dimension

	  real,dimension(:),pointer :: zdef	! size(zdef)=kdim

	  character(len=LEN_VNAME),dimension(:),pointer :: vname
	  integer,dimension(:),pointer :: lnvar

	  integer		:: nymd		! initial ymd
	  integer		:: nhms		! initial hms
	  integer		:: nh00		! incremental hms

	  integer      :: lu=-1	! logical unit if already opened
	  type(String) :: ctrl	! the filename for "control"
	  type(String) :: dset	! "DSET" filename in the ".ctl"
	  type(String) :: dout	! "DSET" filename for open()

	  integer		:: irec		! current record (level)

	  integer		:: iacc		! access control
	  integer		:: ilen		! record length
	  real			:: udef	! missing value flag

	  real*4,dimension(:,:),pointer :: dbuf
        end type wGrADS

	! Interface definitions

      interface wGrADS_open;  module procedure  &
        open_, &
	opendp_;end interface
      interface wGrADS_close; module procedure close_; end interface
      interface wGrADS_write; module procedure	&
	write3ddp_,	&
	write3d_,	&
	write2ddp_,     &
	write2d_
      end interface
      interface inquire; module procedure inquire_; end interface
      interface   bcast; module procedure   bcast_; end interface

! !REVISION HISTORY:
! 	24Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!	08Sep06 - Todling, changed default extension name for grads files
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_wGrADS'

	! access methods of a Fortran unformatted file
	!---------------------------------------------

  integer, parameter :: iacc_DIRECT = 1
  integer, parameter :: iacc_SEQUENTIAL = 2

  character(len=*),parameter :: DSET_SFX = ".bin"

  character(len=*),parameter :: tTMPL="%h2:%n2Z%d2%Mc%y4"
  character(len=*),parameter :: hTMPL="%h2HR"
  integer,parameter :: tLEN=len(tTMPL)-1-1-1+0+1
  integer,parameter :: hLEN=len(hTMPL)-1

  integer,parameter :: NYMD_=20000101
  integer,parameter :: NHMS_=000000
  integer,parameter :: NH00_=060000
  real   ,parameter :: UDEF_=1.e+15

integer,save :: nsize=0
contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: opendp_ - Open a wGrADS for output
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine opendp_(gs,ctrl,nlon,nlat,nlev,zdef,nvar,	&
	nymd,nhms,incr,nh00,dset,access,udef,stat,unit)

      use m_realkinds,only : DP
      implicit none

      type(wGrADS),     intent(out) :: gs

      character(len=*), intent(in)  :: ctrl	! control-filename

      integer,intent(in) :: nlon
      integer,intent(in) :: nlat
      integer,intent(in) :: nlev
      real(DP),dimension(:),intent(in) :: zdef
      integer,intent(in) :: nvar

      integer,optional,intent(in ) :: nymd	! initial yyyymmdd
      integer,optional,intent(in ) :: nhms	! initial hhmmss
      integer,optional,intent(in ) :: incr	! override nh00
      integer,optional,intent(in ) :: nh00	! old del hhmmss

      character(len=*),optional,intent(in) :: dset	! DSET filename
      character(len=*),optional,intent(in) :: access	! DSET ACCESS
      real(DP),optional,intent(in ) :: udef
      integer,optional,intent(out) :: stat	! status
      integer,optional,intent(in) :: unit	! pick a logical unit

! !REVISION HISTORY:
! 	08Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________
  if(present(udef)) then
    call open_(gs,ctrl,nlon,nlat,nlev,real(zdef),nvar, &
      nymd=nymd,nhms=nhms,incr=incr,nh00=nh00,dset=dset, &
      access=access,udef=real(udef),stat=stat,unit=unit)
  else
    call open_(gs,ctrl,nlon,nlat,nlev,real(zdef),nvar, &
      nymd=nymd,nhms=nhms,incr=incr,nh00=nh00,dset=dset, &
      access=access,stat=stat,unit=unit)
  endif
end subroutine opendp_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: open_ - Open a wGrADS for output
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine open_(gs,ctrl,nlon,nlat,nlev,zdef,nvar,	&
	nymd,nhms,incr,nh00,dset,access,udef,stat,unit)

      use m_chars,   only : uppercase
      use m_stdio,   only : stderr
      use m_die,     only : perr,die
      use m_mall,    only : mall_mci,mall_mco,mall_ison
      use m_ioutil,  only : luavail
      use m_Filename,only : Filename_base,Filename_dir
      use m_String,  only : String,toChar
      use m_String,  only : String_init,String_clean

      implicit none

      type(wGrADS),     intent(out) :: gs

      character(len=*), intent(in)  :: ctrl	! control-filename

      integer,intent(in) :: nlon
      integer,intent(in) :: nlat
      integer,intent(in) :: nlev
      real,dimension(:),intent(in) :: zdef
      integer,intent(in) :: nvar

      integer,optional,intent(in ) :: nymd	! initial yyyymmdd
      integer,optional,intent(in ) :: nhms	! initial hhmmss
      integer,optional,intent(in ) :: incr	! override nh00
      integer,optional,intent(in ) :: nh00	! old del hhmmss

      character(len=*),optional,intent(in) :: dset	! DSET filename
      character(len=*),optional,intent(in) :: access	! DSET ACCESS
      real   ,optional,intent(in ) :: udef
      integer,optional,intent(out) :: stat	! status
      integer,optional,intent(in) :: unit	! pick a logical unit

! !REVISION HISTORY:
! 	08Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::open_'

  logical :: direct
  integer :: i,ier
  character(len=1) :: c
  character(len=len(ctrl)) :: base
  type(String) :: aStr	! This variable has to be used to avoid an
			! error with a SGI compiler.

  if(present(stat)) stat=0

!_______________________________________________________________________
! Initialize primary information of a wGrADS

	!________________________________________
	! GrADS data output file attributes

	! Define GrADS "control" filename

  call String_init(gs%ctrl,trim(ctrl))

	! Define both the "DSET" filename (%dset) as to be shown in the
	! "control" file and the output filename (%dout) as to be used
	! for an open().

  if(present(dset)) then
    c=""
    if(len(dset)>0) c=dset(1:1)

    call String_init(gs%dset,trim(dset))

    select case(c)
    case('^')
      call String_init(gs%dout,				&
	trim(Filename_dir(ctrl))//'/'//dset(2:)		)

    case default
      call String_init(gs%dout,(gs%dset))
    end select

  else
    c='^'
    base=Filename_base(ctrl,'.ctl')
    if(base==ctrl) base=Filename_base(ctrl,'.CTL')
    if(base==ctrl) base=Filename_base(ctrl,'.ctrl')
    if(base==ctrl) base=Filename_base(ctrl,'.CTRL')

    call String_init(gs%dset,'^'//trim(base)//DSET_SFX	)

    call String_init(gs%dout,				&
	trim(Filename_dir(ctrl))//'/'//			&
	trim(base)//DSET_SFX	)
  endif

	! Define the ACCESS type

	direct=.false.	! access
	if(present(access)) then

	  select case(uppercase(access))
	  case('DIRECT')
	    direct=.true.

	  case('SEQUENTIAL')
	    direct=.false.

	  case default
	    if(.not.present(stat)) call die(myname_,	&
		'unknown access("'//trim(access)//'")')
	    stat=1
	    return
	  end select
	endif

  gs%iacc=iacc_SEQUENTIAL
  if(direct) gs%iacc=iacc_DIRECT

	! A reserved (through an open()) logical unit

  gs%lu=luavail()
  if(present(unit)) gs%lu=unit

  gs%irec=0		! The current record counter

	!________________________________________
	! GrADS record information, pre-determined

  gs%idim=nlon
  gs%jdim=nlat
  gs%kdim=nlev
  gs%nvar=nvar
  gs%ivar=0

	! GrADS record information, to be determined

  gs%ldim=0	! time dim. counter

  gs%nymd=NYMD_
  if(present(nymd)) gs%nymd=nymd
  gs%nhms=NHMS_
  if(present(nhms)) gs%nhms=nhms
  gs%nh00=NH00_
  if(present(nh00)) gs%nh00=nh00
  if(present(incr)) gs%nh00=incr	! override [nh00=]

	!________________________________________
	! Additional attributes

  gs%udef=UDEF_
  if(present(udef)) gs%udef=udef

!_______________________________________________________________________
! Define allocatable wGrADS data structure

	!________________________________________
	! Allocate arrays

  allocate( gs%zdef (nlev),	&
	    gs%vname(nvar),	&
	    gs%lnvar(nvar),	&
	    gs%dbuf(nlon,nlat),	stat=ier)

	if(ier/=0) then
	  if(.not.present(stat))	&
		call die(myname_,'allocate()',ier)
	  stat=ier
	  return
	endif

	if(mall_ison()) then
	  call mall_mci(gs%zdef ,myname)
	  call mall_mci(gs%vname,myname)
	  call mall_mci(gs%lnvar,myname)
	  call mall_mci(gs%dbuf ,myname)
	endif

	!________________________________________
	! Open the output file

	call String_init(aStr,gs%dout)
  call opendset_(gs%lu,toChar(aStr),gs%iacc,gs%dbuf,gs%ilen,ier, &
    litend=gs%litend)

	if(ier/=0) then
	  call perr(myname_,	&
		'opendset_("'//toChar(aStr)//'")',ier)

		! Deallocated only if this open() has failed.
	  if(mall_ison()) then
	    call mall_mco(gs%zdef ,myname)
	    call mall_mco(gs%vname,myname)
	    call mall_mco(gs%lnvar,myname)
	    call mall_mco(gs%dbuf ,myname)
	  endif

	  deallocate(	gs%zdef,	&
			gs%vname,	&
			gs%lnvar,	&
			gs%dbuf,	stat=ier)

	  if(ier/=0) call perr(myname_,'deallocate()',ier)

	  if(.not.present(stat)) call die(myname_)
	  stat=ier
	  call String_clean(aStr)
	  return
	endif

	call String_clean(aStr)

  do i=1,nvar
    gs%lnvar(i)=0	! not physical counters
  end do

	!________________________________________
	! Initialize record information

  do i=1,nlev
    gs%zdef(i)=zdef(i)
  end do

	! To be defined when %ldim==1.  Also to be verified against when
	! %ldim>1.

  do i=1,nvar
    gs%vname(i)=""
  end do

end subroutine open_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: close_ - close the file for output and write the "control"
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine close_(gs,stat)
      use m_ioutil,only : clsieee
      use m_die,   only : perr,die
      implicit none
      type(wGrADS),intent(inout) :: gs
      integer,optional,intent(out) :: stat

! !REVISION HISTORY:
! 	08Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::close_'
  integer :: ier

  if(present(stat)) stat=0
  if(gs%lu<0) return

  call clsieee(gs%lu,ier)
	if(ier/=0) then
	  call perr(myname_,'clsieee()',ier)
	  if(.not.present(stat)) call die(myname_)
	  stat=ier
	  return
	endif

  call writeCtrl_(gs%lu,gs,ier)
	if(ier/=0) then
	  call perr(myname_,'writeCtrl_()',ier)
	  if(.not.present(stat)) call die(myname_)
	  stat=ier
	  return
	endif

  call clean_(gs,ier)
	if(ier/=0) then
	  call perr(myname_,'clean_()',ier)
	  if(.not.present(stat)) call die(myname_)
	  stat=ier
	  return
	endif

end subroutine close_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: writeCtrl_ - write a control file for a description
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine writeCtrl_(lu,gs,ier)
      use m_StrTemplate,only : StrTemplate
      use m_String,only : String,toChar
      use m_String,only : String_init,String_clean
      use m_ioutil,only : opntext,clstext
      use m_die,   only : perr
      implicit none
      integer,     intent(in)  :: lu
      type(wGrADS),intent(in)  :: gs
      integer,     intent(out) :: ier

! !REVISION HISTORY:
! 	24Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::writeCtrl_'

  real,parameter :: ELON=-180.,RANGE_LON=360.
  logical,parameter :: PERIODIC=.true.

  real,parameter :: SLAT= -90.,RANGE_LAT=180.

  real    :: dlon,dlat
  integer :: ivar
  character(len=tLEN) :: tdef
  character(len=hLEN) :: hdef
  type(String) :: aStr	! This variable has to be used to avoid an
			! error with a SGI compiler.

!________________________________________
! Write a "control" file for a description of what the data are

	call String_init(aStr,(gs%ctrl))
  call opntext(lu,toChar(aStr),'unknown',ier)
	if(ier/=-0) then
	  call perr(myname_,'opntext("'//toChar(aStr)//'")',ier)
	  call String_clean(aStr)
	  return
	endif
	call String_clean(aStr)

!________
	call String_init(aStr,(gs%dset))
  write(lu,'(a,t9,a)') 'DSET',toChar(aStr)
	call String_clean(aStr)

!________
  write(lu,'(a)') 'TITLE  Analysis Increments'

!________
  write(lu,'(a,t9,e14.6)') 'UNDEF',gs%udef

!________
  select case(gs%iacc)
  case(iacc_SEQUENTIAL)
    if(gs%litend) then
      write(lu,'(a)') 'OPTIONS sequential little_endian'
    else
      write(lu,'(a)') 'OPTIONS sequential big_endian'
    endif
  case(iacc_DIRECT)
    if(gs%litend) then
      write(lu,'(a)') 'OPTIONS direct     little_endian'
    else
      write(lu,'(a)') 'OPTIONS direct     big_endian'
    endif
  end select

!________
  dlon=RANGE_LON/ gs%idim
  if(.not.PERIODIC) dlon=RANGE_LON/(gs%idim-1)
  call rdefLinear_(lu,'XDEF',gs%idim,ELON,dlon)

!________
  dlat=RANGE_LAT/(gs%jdim-1)
  call rdefLinear_(lu,'YDEF',gs%jdim,SLAT,dlat)

!________
  call rdefLevels_(lu,'ZDEF',gs%kdim,gs%zdef   )

!________
  call StrTemplate(tdef,tTMPL,nymd=gs%nymd,nhms=gs%nhms)
  call StrTemplate(hdef,hTMPL,		   nhms=gs%nh00)
  call cdefLinear_(lu,'TDEF',gs%ldim,tdef,hdef)

!________
  write(lu,'(a,t9,i4)') 'VARS',gs%nvar
  do ivar=1,gs%nvar
    write(lu,'(a,2x,2i4,2x,a)') gs%vname(ivar),	&
	gs%lnvar(ivar),0,trim(gs%vname(ivar))
  end do
  write(lu,'(a)') 'ENDVARS'

!________
  call clstext(lu,ier)
	if(ier/=0) then
	  call perr(myname_,'clstext()',ier)
	  return
	endif

end subroutine writeCtrl_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: rdefLinear_ - write out a LINEAR grid definition
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine rdefLinear_(lu,adef,ndim,aorg,adel)
      use m_chars,only : tochars
      implicit none
      integer,intent(in) :: lu
      character(len=*),intent(in) :: adef
      integer,intent(in) :: ndim
      real,   intent(in) :: aorg
      real,   intent(in) :: adel

! !REVISION HISTORY:
! 	24Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::rdefLinear_'
  character(len=32) :: aline
  integer :: l

  call tochars((/aorg,adel/),aline,len=l)
  write(lu,'(a,2x,i4,2(2x,a))') adef,ndim,'LINEAR',aline(1:l)

end subroutine rdefLinear_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: rdefLevels_ - write out a LEVELS grid definition
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine rdefLevels_(lu,adef,ndim,alevs)
      use m_chars,only : tochars
      implicit none
      integer,intent(in) :: lu
      character(len=*),intent(in) :: adef
      integer,intent(in) :: ndim
      real,dimension(:),intent(in) :: alevs

! !REVISION HISTORY:
! 	24Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::rdefLevels_'
  character(len=20) :: aline
  integer :: k,l,n

  write(lu,'(a,t7,i4,2x,a)',advance='no') adef,ndim,'LEVELS'

  n=18
  do k=1,size(alevs)
    call tochars(alevs(k:k),aline,len=l)
    if(n+l>80) then
      write(lu,'(/19x,a)',advance='no') aline(1:l)
      n=18
    else
      write(lu,'(1x,a)',advance='no') aline(1:l)
    endif
    n=n+l
  end do
  write(lu,'(a)',advance='yes')

end subroutine rdefLevels_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: cdefLinear_ - write out a LINEAR grid definition
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine cdefLinear_(lu,adef,ndim,corg,cdel)
      implicit none
      integer,intent(in) :: lu
      character(len=*),intent(in) :: adef
      integer,intent(in) :: ndim
      character(len=*),intent(in) :: corg
      character(len=*),intent(in) :: cdel

! !REVISION HISTORY:
! 	24Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::cdefLinear_'

  write(lu,'(a,2x,i4,2x,a,2(2x,a))') adef,ndim,'LINEAR',corg,cdel

end subroutine cdefLinear_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: opendset_ - open a DSET file for output
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine opendset_(lu,name,iacc,dbuf,ilen,ierr,litend)
      use m_ioutil,only : opnieee
      use m_die,   only : perr
      implicit none

      integer,         intent(in)  :: lu
      character(len=*),intent(in)  :: name
      integer,         intent(in)  :: iacc
      real*4,dimension(:,:),intent(in) :: dbuf

      integer,         intent(out) :: ilen
      integer,         intent(out) :: ierr
      logical,optional,intent(out) :: litend

! !REVISION HISTORY:
! 	21Jan00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::opendset_'
  character(len=16) :: clen
  integer*4,parameter :: i4=10
  integer*2 :: i1,i2

  ilen=0
  inquire(iolength=ilen) dbuf

  select case(iacc)
  case(iacc_SEQUENTIAL)

    ilen=0		! reset %ilen to avoid confusion
    call opnieee(lu,trim(name),'unknown',ierr)
	  if(ierr.ne.0) then
	    call perr(myname_,'opnieee("'//	&
		trim(name)//'")',ierr		)
	    return
	  endif
	if(present(litend)) then
	  write(lu) i4
	  rewind(lu)
	  read(lu) i1,i2
	  rewind(lu)
	  litend = i4==i2*256+i1
	endif

  case(iacc_DIRECT)

    call opnieee(lu,trim(name),'unknown',ierr,recl=ilen)
	  if(ierr.ne.0) then
	    clen='****************'
	    write(clen,'(i16)',iostat=ierr) ilen
	    clen=adjustl(clen)
	    call perr(myname_,'opnieee("'//	&
		trim(name)//'",recl='//		&
		trim(clen)//')',ierr		)
	    return
	  endif
	if(present(litend)) then
	  write(lu,rec=1) i4
	  read(lu,rec=1) i1,i2
	  litend = i4==i2*256+i1
	endif
	
  case default

	  call perr(myname_,'unknown iacc',iacc)
	  return
  end select

	!--------------------------------------------------------
end subroutine opendset_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: clean_ - clean a wGrADS object
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine clean_(gs,ier)
      use m_die, only : perr
      use m_mall,only : mall_mco,mall_ison
      use m_String, only : String_clean
      implicit none

      type(wGrADS),intent(inout) :: gs
      integer     ,intent(out)   :: ier

! !REVISION HISTORY:
! 	24Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::clean_'

  ier=0

  gs%lu=-1

    call String_clean(gs%ctrl)
    call String_clean(gs%dset)
    call String_clean(gs%dout)

    gs%udef=UDEF_
    gs%irec = 0
    gs%iacc = iacc_DIRECT
    gs%ilen = -1

  gs%ldim=-1

    gs%nymd=NYMD_
    gs%nhms=NHMS_
    gs%nh00=NH00_

  gs%nvar=-1
    gs%ivar = 0

  gs%kdim=-1
  gs%jdim=-1
  gs%idim=-1

	if(mall_ison()) then
	  call mall_mco(gs%zdef ,myname)
	  call mall_mco(gs%vname,myname)
	  call mall_mco(gs%lnvar,myname)
	  call mall_mco(gs%dbuf ,myname)
	endif

  deallocate(gs%zdef,gs%vname,gs%lnvar,gs%dbuf,stat=ier)
	if(ier/=0) then
	  call perr(myname_,'deallocate()',ier)
	  return
	endif

end subroutine clean_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: write3ddp_ - output a 3-d field
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine write3ddp_(gs,vnam,vfld,stat)
      use m_realkinds,only : DP
      implicit none
      type(wGrADS),intent(inout) :: gs
      character(len=*),intent(in) :: vnam
      real(DP),        intent(in) :: vfld(:,:,:)
      integer,optional,intent(out):: stat

! !REVISION HISTORY:
! 	08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________
  call write3d_(gs,vnam,real(vfld),stat=stat)
end subroutine write3ddp_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: write3d_ - output a 3-d field
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine write3d_(gs,vnam,vfld,stat)
      use m_stdio, only : stderr
      use m_die,   only : perr,die
      implicit none
      type(wGrADS),intent(inout) :: gs
      character(len=*),intent(in) :: vnam
      real,            intent(in) :: vfld(:,:,:)
      integer,optional,intent(out):: stat

! !REVISION HISTORY:
! 	08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::write3d_'

  integer :: k,ier,nlev

  if(present(stat)) stat=0
  nlev=size(vfld,3)

  call verify_(gs,vnam,min(gs%kdim,nlev),ier)
	if(ier/=0) then
	  call perr(myname_,'verify_()',ier)
	  if(.not.present(stat)) call die(myname_)
	  stat=ier
	  return
	endif

  do k=1,min(gs%kdim,nlev)

    gs%irec=gs%irec+1

    call writelev_( gs%lu,  gs%iacc,gs%irec,		&
		    gs%idim,gs%jdim,vfld(:,:,k),	&
		    gs%dbuf,ier				)

	if(ier/=0) then
	  call perr(myname_,'writelev_()',ier)
	  if(.not.present(stat)) call die(myname_)
	  stat=ier
	  return
	endif
  end do

end subroutine write3d_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: write2ddp_ - output a 2-d field
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine write2ddp_(gs,vnam,vfld,stat)
      use m_realkinds,only : DP
      implicit none
      type(wGrADS),intent(inout) :: gs
      character(len=*),intent(in) :: vnam
      real(DP),        intent(in) :: vfld(:,:)
      integer,optional,intent(out):: stat

! !REVISION HISTORY:
! 	08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________
  call write2d_(gs,vnam,real(vfld),stat=stat)
end subroutine write2ddp_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: write2d_ - output a 2-d field
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine write2d_(gs,vnam,vfld,stat)
      use m_stdio, only : stderr
      use m_die,   only : perr,die
      implicit none
      type(wGrADS),intent(inout) :: gs
      character(len=*),intent(in) :: vnam
      real,            intent(in) :: vfld(:,:)
      integer,optional,intent(out):: stat

! !REVISION HISTORY:
! 	08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::write2d_'

  integer :: ier

  if(present(stat)) stat=0

  call verify_(gs,vnam,0,ier)
	if(ier/=0) then
	  call perr(myname_,'verify_()',ier)
	  if(.not.present(stat)) call die(myname_)
	  stat=ier
	  return
	endif

  gs%irec=gs%irec+1

  call writelev_(gs%lu,gs%iacc,gs%irec,gs%idim,gs%jdim,vfld,gs%dbuf,ier)

	if(ier/=0) then
	  call perr(myname_,'writelev_()',ier)
	  if(.not.present(stat)) call die(myname_)
	  stat=ier
	  return
	endif

end subroutine write2d_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: writelev_ - write a 2-d field
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine writelev_(lu,iacc,irec,nlon,nlat,vfld,dbuf,ier)
      use m_die, only : perr
      implicit none
      integer,intent(in) :: lu
      integer,intent(in) :: iacc
      integer,intent(in) :: irec
      integer,intent(in) :: nlon
      integer,intent(in) :: nlat
      real  ,dimension(:,:),intent(in) :: vfld
      real*4,dimension(:,:)            :: dbuf	! (?out)
      integer,intent(out) :: ier

! !REVISION HISTORY:
! 	24Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::writelev_'

  logical :: no_buffer
  integer :: nx,ny

  no_buffer =	kind(dbuf)	== kind(vfld)	.and.	&
		size(dbuf,1)	== size(vfld,1)	.and.	&
		size(dbuf,2)	== size(vfld,2)

  select case(iacc)
  case(iacc_DIRECT)

    if(no_buffer) then
      write(lu,rec=irec,iostat=ier) vfld
	if(ier/=0) call perr(myname_,'iacc_DIRECT/no_buffer',ier)
	nsize=nsize+size(vfld)

    else
      nx=min(size(vfld,1),size(dbuf,1))
      ny=min(size(vfld,2),size(dbuf,2))
      dbuf(1:nx,1:ny)=vfld(1:nx,1:ny)

      write(lu,rec=irec,iostat=ier) dbuf
	if(ier/=0) call perr(myname_,'iacc_DIRECT/buffer',ier)
	nsize=nsize+size(dbuf)
    endif

  case(iacc_SEQUENTIAL)

    if(no_buffer) then
      write(lu,iostat=ier) vfld
	if(ier/=0) call perr(myname_,'iacc_SEQUENTIAL/no_buffer',ier)
	nsize=nsize+size(vfld)
    else
      nx=min(size(vfld,1),size(dbuf,1))
      ny=min(size(vfld,2),size(dbuf,2))
      dbuf(1:nx,1:ny)=vfld(1:nx,1:ny)

      write(lu,iostat=ier) dbuf
	if(ier/=0) call perr(myname_,'iacc_SEQUENTIAL/buffer',ier)
	nsize=nsize+size(dbuf)
    endif

  case default
    call perr(myname_,'unknown iacc value',iacc)
    ier=-1
    
  end select

end subroutine writelev_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: verify_ - verify the status of adding a field
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine verify_(gs,vnam,nlev,ier)
      use m_chars,only : uppercase
      use m_die,  only : perr
      implicit none
      type(wGrADS)    ,intent(inout) :: gs
      character(len=*),intent(in)    :: vnam
      integer         ,intent(in)    :: nlev
      integer         ,intent(out)   :: ier

! !REVISION HISTORY:
! 	24Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::verify_'
  character(len=LEN_VNAME) :: vtmp
  integer :: ivar

  ier=0

	! Start the t-dim counter

  if(gs%ldim <= 0 ) gs%ldim=1

	! Increase the t-dim counter

  if(gs%ivar >= gs%nvar ) then
    gs%ivar=0
    gs%ldim=gs%ldim+1
  endif

	! Increase the v-dim couter

  gs%ivar=gs%ivar+1
	if(gs%ivar > gs%nvar) then
	  call perr(myname_,'too many %vname',gs%ivar)
	  ier=-1
	  return
	endif

  ivar=gs%ivar

  if(gs%ldim == 1) then
		! If it is the first time, save it.  However, if the
		! name has been given more than once, it will still
		! be accepted.

    gs%vname(ivar)=vnam
    gs%lnvar(ivar)=nlev

  else
		! If it is not the first time, the name and the size
		! must be the same as in the record (i.e. gs).

    vtmp=uppercase(gs%vname(ivar))
    if( vtmp /= uppercase(vnam) ) then
      call perr(myname_,	&
	'"'//trim(gs%vname(ivar))//'" /= "'//trim(vnam)//'"'	)
      ier=1
      return
    endif

    if(gs%lnvar(ivar) /= nlev) then
      call perr(myname_,'"'//trim(vnam)//'": %lnvar',	&
	gs%lnvar(ivar),'nlev',nlev)

      if(nlev==0) then
        call perr(myname_,	&
	  '"'//trim(vnam)//'" is expected to be 3-d',nlev)
	ier=2
      else
	if(gs%lnvar(ivar)==0) then
          call perr(myname_,	&
	    '"'//trim(vnam)//'" is expected to be 2-d',nlev)
	  ier=3
	else
	  ier=4
	endif
      endif
      return
    endif
  endif

end subroutine verify_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: inquire_ - get information
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine inquire_(gs,nlon,nlat,nlev,var,nvar, &
      nymd,nhms,incr,ntim,itim)
      implicit none
      type(wGrADS),intent(in) :: gs
      integer,optional,intent(out) :: nlon
      integer,optional,intent(out) :: nlat
      integer,optional,intent(out) :: nlev
      character(len=*),optional,intent(in) :: var ! nlev for this var
      integer,optional,intent(out) :: nvar
      integer,optional,intent(out) :: nymd
      integer,optional,intent(out) :: nhms
      integer,optional,intent(out) :: incr
      integer,optional,intent(out) :: ntim
      integer,optional,intent(out) :: itim

! !REVISION HISTORY:
! 	31May06	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::inquire_'
  integer :: ivar

! Note: It is probably impossible to support a generic interface for
! multiple KINDs of a real argument, such as udef, at presence of
! other optional arguments, such integers.  Therefore, inquiry for
! multiple kinds of udef must be implemented in a separate procedure.

  if(present(nlon)) nlon=gs%idim
  if(present(nlat)) nlat=gs%jdim
  if(present(nlev)) then
    nlev=gs%kdim
    if(present(var)) then ! return nlev for this variable
      ivar=lindex_(gs,var)
      if(ivar/=0) nlev=gs%lnvar(ivar)
    endif
  endif
  if(present(nvar)) nvar=gs%nvar ! nvar is well defined, but I really
  if(present(ntim)) ntim=gs%ldim ! ntim and itim are always the same
  if(present(itim)) itim=gs%ldim ! for squential output.
  if(present(nymd)) nymd=gs%nymd
  if(present(nhms)) nhms=gs%nhms
  if(present(incr)) incr=gs%nh00
contains
function lindex_(gs,var)
  use m_chars,only : uppercase
  implicit none
  integer :: lindex_
  type(wGrADS),intent(in) :: gs
  character(len=*),intent(in) :: var
!/-
  character(len=len(var)) :: vcap
  integer :: iv
!-/
  lindex_=0
  vcap=uppercase(var)
  do iv=1,gs%nvar
    if(vcap==uppercase(gs%vname(ivar))) then
      lindex_=iv
      exit
    endif
  end do
  end function lindex_
end subroutine inquire_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: bcast_ - broadcast all inquirable information
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine bcast_(gs,root,comm)
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_type
      use m_die,only : MP_die
      implicit none
      type(wGrADS),intent(inout) :: gs
      integer,intent(in) :: root
      integer,intent(in) :: comm

! !REVISION HISTORY:
! 	31May06	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::bcast_'
  integer :: ier,myPE
  integer,allocatable,dimension(:) :: ibufr

  call MP_comm_rank(comm,myPE,ier)
    if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  allocate(ibufr(9))
  if(myPE==root) then
    ibufr(1)=gs%idim
    ibufr(2)=gs%jdim
    ibufr(3)=gs%kdim
    ibufr(4)=gs%nvar
    ibufr(5)=gs%ivar
    ibufr(6)=gs%ldim
    ibufr(7)=gs%nymd
    ibufr(8)=gs%nhms
    ibufr(9)=gs%nh00
  endif

  call MPI_bcast(ibufr,size(ibufr),MP_type(ibufr),root,comm,ier)
    if(ier/=0) call MP_die(myname_,'MPI_bcast(ibufr)',ier)

  if(myPE/=root) then
    gs%idim=ibufr(1)
    gs%jdim=ibufr(2)
    gs%kdim=ibufr(3)
    gs%nvar=ibufr(4)
    gs%ivar=ibufr(5)
    gs%ldim=ibufr(6)
    gs%nymd=ibufr(7)
    gs%nhms=ibufr(8)
    gs%nh00=ibufr(9)
    gs%lu  = -1
  endif
  deallocate(ibufr)

  call MPI_bcast(gs%udef,1,MP_type(gs%udef),root,comm,ier)
    if(ier/=0) call MP_die(myname_,'MPI_bcast(%udef)',ier)

end subroutine bcast_
end module m_wGrADS
!.
