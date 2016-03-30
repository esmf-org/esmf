!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: m_mpout - a multiple but mergable parallel output module
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_mpout
      use m_stdio, only : stdout,LEN_FILENAME
      implicit none
      private	! except

      public :: mpout	! The file handle as a Fortran logical unit

      public :: mpout_open	! open the multiple output streams
      public :: mpout_close	! close the multiple output streams
      public :: mpout_sync	! sync. the multiple output streams
      public :: mpout_flush	! flush the multople output streams
      public :: mpout_ison	! verify if mpout is proper defined
      public :: mpout_log	! write a message to mpout
      public :: mpout_MASK	! mask of all but PE 0
      public :: mpout_setflush  ! modify default flush behavior after
				! writes using mpout_log().

      interface mpout_open;  module procedure open_;  end interface
      interface mpout_close; module procedure close_; end interface
      interface mpout_sync;  module procedure sync_;  end interface
      interface mpout_flush; module procedure flush_; end interface
      interface mpout_ison;  module procedure ison_;  end interface
      interface mpout_log;   module procedure	&
	logi_,	&
	logr_,	&
	logd_,	&
	log_;   end interface
      interface mpout_setflush; module procedure	&
	setflush_; end interface

! !REVISION HISTORY:
!	
!       06Feb01 - Tom Clune
!	        . Added interface to modify default flush
!                 behavior in log routines.
!	12Oct00	- Jing Guo
!		. Revised the module to make its behaviors closer to
!		  what developers might want to see.  The conflict
!		  between the principles for lately added procedures
!		  and for the earlier implementation is merged.
!		  The earlier design expected more disciplined usage,
!		  while the later additions expected applications with
!		  less care.
!		. Added EXAMPLES for the module.
!	28Sep99 - Jing Guo <guo@thunder>
!		- Added additional calls to support the "Violet" system
!		  development.
! 	25Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!
! !SEE ALSO: About../mpout.usc
!	
! !DESIGN ISSUES:
! \begin{itemize}
!
! \item	It might be considered useful to implement this module to be
!	applicable to a given {\sl communicator}.   The argument
!	taken now is to only have one multiple output stream handle
!	per excution.  This is consistent with \verb"stdout" in the
!	traditional sense. (Jing Guo, 25Feb98)
!
! \item \verb"mpout_log()" is implemented in a way producing output
!	only if \verb"mpout_ison()" (being \verb".true.").  The reason
!	of not implementing a default output such as \verb"stdout", is
!	hoping to provent too many unexpected output when the system is
!	switched to a multiple PE system.  The design principle for
!	this module is that \verb"mpout" is basically {\sl not} the same
!	module as \verb"stdout". (Jing Guo, 28Sep99)
!
! \end{itemize}
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname='m_mpout'

  character(len=*),parameter :: def_pfix='mpout'

  integer,save :: isync=-1

  integer,save :: mpout=stdout
		! mpout is the logical unit used on a given PE

  logical,save :: mpout_on=.false.
		! mpout_on indicates if mpout is managed under this
		! module, either by default or by user's requests
		! through mpout_open(..).

  logical,save :: mpout_initialized=.false.
		! mpout_initialized indicates if this module has been
		! initialized, such that that if mpout_open() is not
		! called, this module may still be used to manage the
		! outputs to STDOUT.

  logical,save :: mpout_opened=.false.
		! mpout_opened ensures that open()/close() calls are
		! paired.

  character(len=LEN_FILENAME-4),save :: upfix=def_pfix
  integer,parameter :: mpout_MASK=HUGE(1)	! mask of all but PE 0

  logical, save :: default_flush = .false.

contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: open_ - open a multiple files with the same name prefix
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine open_(mask,pfix,append)
      use m_stdio, only : stderr,stdout
      use m_ioutil, only : luavail,opntext
      use m_dropdead, only : ddie => die
      use m_mpif90, only : MP_comm_WORLD
      use m_mpif90, only : MP_comm_rank
      use m_mpif90, only : MP_perr
      implicit none
      integer,optional,intent(in) :: mask

		! It is a mask of PE ranks where mpout will _not_ be on
		! ("on" == a file is opened).
		!
		!   condition		: status
		!
		! .not.present(mask)	: on PE 0 only
		! mask	== -1		: off
		!     	==  0		: on
		! iand(mask,rank) == 0	: on
		! iand(maxk,rank) /= 0	: off

      character(len=*),optional,intent(in) :: pfix

		! The leading filename component of the mpout file set.
		! If the argument is not present, pfix='mpout' is
		! assumed.  Filenames of the mpout file set opened by
		! this open() are in the form of "pfix.zzz", where
		! "pfix" is the string defined by argument pfix, and
		! "zzz" is the PE rank in hexidecimal.

      logical,optional,intent(in) :: append

		! If the file is to be positioned as 'append'.  The
		! default is not to append.

! !EXAMPLES:
!
!   . mpout_ison() on no PE but PE 0, where mpout is opened to file
!     "mpout.000":
!
!	call mpout_open()
!
!   . mpout_ison() on every 4 PE starting from 0, where mpout is opened
!     to files named, "out.000", "out.004", "out.007", "out.00a", etc.

!	call mpout_open(mask=3,pfix='out')
!
!     Note that 3 = "11"b.  Therefore,
!
!	mask="000011"b
!	PE 0="000000"b	is "clean", mpout_ison()
!	   1="000001"b	is "dirty", .not. on
!	   2="000010"b	is "dirty", .not. on
!	   3="000011"b	is "dirty", .not. on
!	   4="000100"b	is "clean", mpout_ison()
!	   5="000101"b	is "dirty", .not. on
!	   6="000110"b	is "dirty", .not. on
!	   7="000111"b	is "dirty", .not. on
!	   8="001000"b	is "clean", mpout_ison()
!	   9="001001"b	is "dirty", .not. on
!	  10="001010"b	is "dirty", .not. on
!	  11="001011"b	is "dirty", .not. on
!	  12="001100"b	is "clean", mpout_ison()
!	  13="001101"b	is "dirty", .not. on
!	  14="001110"b	is "dirty", .not. on
!	  15="001111"b	is "dirty", .not. on
!
! !REVISION HISTORY:
!	12Oct00	- Jing Guo
!		. Revised argument describtions.
!		. Revised EXAMPLES.
!		. Removed default sync_() action, which should be a
!		  choice by users.
! 	25Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::open_'
  integer :: lu
  character(len=4) :: sfix
  integer :: irank
  integer :: ier
  integer :: umask
  logical :: uappend

	! Set the filename prefix

  upfix=def_pfix
  if(present(pfix)) upfix=pfix

	! Set the mask of the PEs with mpout

  umask=mpout_MASK
  if(present(mask)) umask=mask

  uappend=.false.
  if(present(append)) uappend=append

	! If a check is not in place, sent the outputs to stdout

  mpout=stdout

  call MP_comm_rank(MP_comm_world,irank,ier)
  if(ier /= 0) then
    call MP_perr(myname_,'MP_comm_rank()',ier)
    call ddie(myname_)
  endif

  select case(umask)
  case( 0)	! all on
    mpout_on=.true.
  case(-1)	! all off
    mpout_on=.false.
  case default
    mpout_on=iand(irank,umask)==0
  end select

  if(mpout_on) then

    lu=luavail()
	if(lu<=0) then
	  write(stderr,'(2a,i4)') myname_,	&
		': luavail() error, unit =',lu
	  call ddie(myname_)
	endif
    mpout=lu

    write(sfix,'(a,z3.3)') '.',irank
    if(uappend) then
      call opntext(mpout,trim(upfix)//sfix,'append',ier)
    else
      call opntext(mpout,trim(upfix)//sfix,'unknown',ier)
    endif

	if(ier /= 0) then
	  write(stderr,'(5a,i4)') myname_,	&
		': opntext("',trim(upfix),sfix,'") error, ier =',ier
	  call ddie(myname_)
	endif
    isync=0
  endif

  mpout_initialized=.true.
  mpout_opened=.true.
end subroutine open_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: close_ - close the unit opened by open_
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine close_()
      use m_stdio,  only : stderr
      use m_ioutil, only : clstext, luflush
      use m_dropdead, only : ddie => die
      implicit none

! !REVISION HISTORY:
!	12Oct00	- Jing Guo
!		. Removed default sync_() action, which should be a
!		  choice of users.
! 	25Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::close_'
  integer :: ier

  if(mpout_opened .and. mpout_on) then
    call luflush(mpout)

    endfile(mpout)

    call clstext(mpout,ier)
    if(ier /= 0) then
      write(stderr,'(2a,i3.3,a,i4)') myname_,	&
	': clstext("',mpout,'") error, ier =',ier
      call ddie(myname_)
    endif

    mpout=stdout
    mpout_on=.false.
  endif

  isync=-1

  mpout_initialized=.false.
  mpout_opened=.false.
end subroutine close_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: sync_ - write a mark for posible later file merging
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine sync_(tag)
      implicit none
      character(len=*),intent(in) :: tag

! !REVISION HISTORY:
! 	25Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!
! !DESIGN ISSUES:
! \begin{itemize}
!
! \item	Should the variable \verb"tag" be implemented as an optional
!	argument?  Because the current implementation does not require
!	actual synchronization between all threads of the multiple
!	output streams, forcing the user to supply a unique \verb"tag"
!	would make the final multi-stream merging verifiable.  However,
!	since the \verb"tag"s have not been forced to be unique, the
!	synchronization operations are still symbolic.
!	
! \{itemize}
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::sync_'

  if(.not.mpout_initialized) call initialize_()

  if(mpout_on) then
    isync=isync+1
    write(mpout,'(2a,z8.8,2a)') myname_,' ',isync,' ',trim(tag)
  endif

end subroutine sync_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: flush_ - flush the multiple output streams
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine flush_()
      use m_ioutil, only : luflush
      implicit none

! !REVISION HISTORY:
! 	27Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::flush_'

  if(.not.mpout_initialized) call initialize_()

  if(mpout_on) call luflush(mpout)

end subroutine flush_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ison_ - decide if the current PE has a defined mpout
!
! !DESCRIPTION:
!
!   It needs to be checked to avoid undesired output.
!
! !INTERFACE:

    function ison_()
      implicit none
      logical :: ison_

! !REVISION HISTORY:
! 	14Sep99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::ison_'

  if(.not.mpout_initialized) call initialize_()

  ison_=mpout_on

end function ison_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: log_ - write a message to mpout
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine log_(where,message,showrank,flush)
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_comm_world
      use m_mpif90,only : MP_perr
      use m_ioutil,only : luflush
      use m_dropdead, only : ddie => die
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: message
      logical,optional,intent(in) :: showrank
      logical,optional,intent(in) :: flush

! !REVISION HISTORY:
! 	14Sep99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::log_'
  logical :: show_,flush_
  integer :: myID
  integer :: ier

  show_=.false.
  if(present(showrank)) show_=showrank

  flush_=default_flush
  if(present(flush)) flush_=flush

  myID=0
  if(show_) then
    call MP_comm_rank(MP_comm_world,myID,ier)
    if(ier /= 0) then
      call MP_perr(myname_,'MP_comm_rank()',ier)
      call ddie(myname_)
    endif
  endif 

  if(.not.mpout_initialized) call initialize_()

  if(mpout_on) then
    if(show_) then
      write(mpout,'(z3.3,4a)') myID,'.',where,': ',message
    else
      write(mpout,'(3a)') where,': ',message
    endif
    if(flush_) call luflush(mpout)
  endif

end subroutine log_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: logi_ - write a message to mpout
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine logi_(where,message,num,showrank,flush)
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_comm_world
      use m_mpif90,only : MP_perr
      use m_ioutil,only : luflush
      use m_dropdead, only : ddie => die
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: message
      integer,intent(in) :: num
      logical,optional,intent(in) :: showrank
      logical,optional,intent(in) :: flush

! !REVISION HISTORY:
! 	14Sep99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::logi_'
  logical :: show_,flush_
  integer :: myID
  integer :: ier
  character(len=16) :: cnum

  show_=.false.
  if(present(showrank)) show_=showrank

  flush_=default_flush
  if(present(flush)) flush_=flush

  myID=0
  if(show_) then
    call MP_comm_rank(MP_comm_world,myID,ier)
    if(ier /= 0) then
      call MP_perr(myname_,'MP_comm_rank()',ier)
      call ddie(myname_)
    endif
  endif 

  if(.not.mpout_initialized) call initialize_()

  if(mpout_on) then
    cnum='********'
    write(cnum,'(i16)',iostat=ier) num
    cnum=adjustl(cnum)
    if(show_) then
      write(mpout,'(z3.3,4a,1x,a)') myID,'.',where,': ',	&
	message,trim(cnum)
    else
      write(mpout,'(3a,1x,a)') where,': ',message,trim(cnum)
    endif
    if(flush_) call luflush(mpout)
  endif

end subroutine logi_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: logd_ - write a message to mpout
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine logd_(where,message,val,showrank,flush)
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_comm_world
      use m_mpif90,only : MP_perr
      use m_ioutil,only : luflush
      use m_dropdead, only : ddie => die
      use m_realkinds,only : DP
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: message
      real(DP),intent(in) :: val
      logical,optional,intent(in) :: showrank
      logical,optional,intent(in) :: flush

! !REVISION HISTORY:
! 	14Sep99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::logd_'
  logical :: show_,flush_
  integer :: myID
  integer :: ier

  show_=.false.
  if(present(showrank)) show_=showrank

  flush_=default_flush
  if(present(flush)) flush_=flush

  myID=0
  if(show_) then
    call MP_comm_rank(MP_comm_world,myID,ier)
    if(ier /= 0) then
      call MP_perr(myname_,'MP_comm_rank()',ier)
      call ddie(myname_)
    endif
  endif 

  if(.not.mpout_initialized) call initialize_()

  if(mpout_on) then
    if(show_) then
      write(mpout,'(z3.3,4a,1p,e10.3)') myID,'.',where,': ',message,val
    else
      write(mpout,'(3a,1p,e10.3)') where,': ',message,val
    endif
    if(flush_) call luflush(mpout)
  endif

end subroutine logd_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: logr_ - write a message to mpout
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine logr_(where,message,val,showrank,flush)
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_comm_world
      use m_mpif90,only : MP_perr
      use m_ioutil,only : luflush
      use m_dropdead, only : ddie => die
      use m_realkinds,only : SP
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: message
      real(SP),intent(in) :: val
      logical,optional,intent(in) :: showrank
      logical,optional,intent(in) :: flush

! !REVISION HISTORY:
! 	14Sep99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::logr_'
  logical :: show_,flush_
  integer :: myID
  integer :: ier

  show_=.false.
  if(present(showrank)) show_=showrank

  flush_=default_flush
  if(present(flush)) flush_=flush

  myID=0
  if(show_) then
    call MP_comm_rank(MP_comm_world,myID,ier)
    if(ier /= 0) then
      call MP_perr(myname_,'MP_comm_rank()',ier)
      call ddie(myname_)
    endif
  endif 

  if(.not.mpout_initialized) call initialize_()

  if(mpout_on) then
    if(show_) then
      write(mpout,'(z3.3,4a,1p,e10.3)') myID,'.',where,': ',message,val
    else
      write(mpout,'(3a,1p,e10.3)') where,': ',message,val
    endif
    if(flush_) call luflush(mpout)
  endif

end subroutine logr_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: initialize_ - initialize to a default setting
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine initialize_()
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_comm_world
      use m_mpif90,only : MP_perr
      use m_dropdead,only : ddie => die
      implicit none

! !REVISION HISTORY:
! 	05Oct00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::initialize_'
  integer :: irank
  integer :: ier

  call MP_comm_rank(MP_comm_world,irank,ier)
  if(ier /= 0) then
    call MP_perr(myname_,'MP_comm_rank()',ier)
    call ddie(myname_)
  endif

  mpout_on = iand(irank,mpout_MASK) == 0

  mpout_initialized=.true.
  mpout_opened=.false.
end subroutine initialize_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: setflush_ - initialize to a default setting
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine setflush_(flush)
      implicit none
      logical, intent(in) :: flush

! !REVISION HISTORY:
! 	06Feb01	- Tom Clune <clune@sgi.com>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::setflush_'
  default_flush = flush

end subroutine setflush_

end module m_mpout
!.
