!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!
! !MODULE: m_rGrADS - Read-only files in the GrADS format
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_rGrADS

      implicit none
      private

      public	:: rGrADS	! the class data stucture
      public	:: GrADS_open	! open a rGrADS instance
      public	:: GrADS_read	! read a GrADS 2d/3d field
!      public	:: GrADS_close	! close a rGrADS instance
      public	:: rGrADS_close	! close a rGrADS instance
      public	:: inquire      ! get attributes
      public	:: bcast        ! broadcast meta data

      	! Obsolete interface to be phased out

#ifdef _TORM_
      public	:: GrADS_input	! read a GrADS 2d/3d field
      public	:: GrADS_getdims! get the dimensions of a variable
      public	:: GrADS_zdef	! get the levels of a variable
#endif

      	! Less used interfaces
      public	:: LEN_VARS

      public	:: ptr_zdef	! pointer to the array of levels
      public	:: ptr_vars	! pointer to the list of variables

      public	:: ptr_nrec	! pointer to the record (level) counts
      				! of variables.  Note that value 0 means
				! 1 record of a 2-D variable.  Thus,
      				! there is no blank (0 record) variable
				! in a GrADS data storage.

		! The current version only has limited supports to a
		! GrADS file, with restrictions on the grid, time,
		! and the file format, etc.

	integer,parameter :: LEN_VARS=12
        type rGrADS
	  private

	  integer		:: nvar	! number of variables
	  integer		:: idim		! longitude dimension
	  integer		:: jdim		! latitude dimension
	  integer		:: kdim		! level dimension
	  integer		:: ldim		! time dimension

	  real,dimension(:),pointer :: zdef	! size(zdef)=kdim

	  integer		:: nymd		! initial ymd
	  integer		:: nhms		! initial hms
	  integer		:: nh00		! incremental hms

	  integer		:: irec		! current location
	  integer		:: iacc		! access control
	  integer		:: ilen		! record length
	  real			:: undef	! missing value flag

	  integer		:: nblock
	  character(len=LEN_VARS),dimension(:),pointer :: vname
	  integer,          dimension(:),pointer :: n_rec
	  integer,          dimension(:),pointer :: i_rec

	  integer	     :: lu	! logical unit if already opened
	  character(len=256) :: file	! the filename for input

	  real*4,dimension(:,:),pointer :: dbuf
        end type rGrADS

	! Interface definitions

      interface GrADS_open;  module procedure open_;  end interface
      interface GrADS_read; module procedure	&
	input3ddp_,	&
	input3d_,	&
	input1levdp_,	&
	input1lev_,	&
	input2ddp_,	&
	input2d_
      end interface
      interface GrADS_close; module procedure close_; end interface
      interface rGrADS_close; module procedure close_; end interface
      interface inquire; module procedure inquire_; end interface
      interface   bcast; module procedure   bcast_; end interface

!#ifdef _TORM_
      interface GrADS_input; module procedure	&
	input3d_,	&
	input1lev_,	&
	input2d_
      end interface

      interface GrADS_getdims; module procedure getdims_; end interface
      interface GrADS_zdef;    module procedure zdef_;    end interface
!#endif	! _TORM_

      interface ptr_zdef; module procedure ptr_zdef_; end interface
      interface ptr_vars; module procedure ptr_vars_; end interface
      interface ptr_nrec; module procedure ptr_nrec_; end interface

! !EXAMPLES: (to do)
! !BUGS: (to do)
!
!   Output interfaces should be added soon.  See aio_grads.f for more
!   information.
!
! !SEE ALSO: (to do)
! !SYSTEM ROUTINES: (to do)
!
! !REVISION HISTORY:
! 	16Jul96 - J. Guo	- (to do)
!_______________________________________________________________________

!=======================================================================
!
! !REVISION HISTORY
! 	grads.h - last change: Wed Jul 20 21:00:31 EDT 1994 (ams)
!				- Original source from A. da Silva
!	01Dec94 - Jing G. -	added zdef_gr for small values
!	16Jul96 - Jing G. -	combined to form a module
! file: grads.f - last change: Wed Jul 20 21:00:31 EDT 1994 (ams)
!
!  Routines to read in GrADS like files.
!......................................................................

	! access methods of a Fortran unformatted file
	!---------------------------------------------

  integer, parameter :: stat_DEFINED = 1
  integer, parameter :: stat_UNDEF   = 0
! _TODO_ : read %nymd, %nhms, and %nh00 from TDEF instead of using
! default values.
  integer, parameter :: NYMD_=20000101
  integer, parameter :: NHMS_=000000
  integer, parameter :: NH00_=060000
  real   , parameter :: UDEF_=1.e+15

  character(len=*),parameter :: myname='m_rGrADS'

#include "assert.H"
contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!
! !IROUTINE: open_ - open an input GrADS "control" file for input
!
! !DESCRIPTION: (to do)
! !INTERFACE:

    subroutine open_( gs, ctl_file, stat,unit)

      use m_inpak90, only : i90_loadf
      use m_inpak90, only : i90_release
      use m_inpak90, only : i90_label
      use m_inpak90, only : i90_gtoken
      use m_inpak90, only : i90_gint
      use m_inpak90, only : i90_gfloat
      use m_inpak90, only : i90_gline
      use m_chars,   only : uppercase
      use m_stdio,   only : stderr
      use m_die,     only : perr
      use m_die,     only : die
      use m_mall,    only : mall_mci,mall_ison
      use m_ioutil,  only : luavail

      use m_ior4,only : iacc_DIRECT
      use m_ior4,only : iacc_SEQUENTIAL

      implicit none

      type(rGrADS), intent(out) :: gs
      character(len=*), intent(in)  :: ctl_file	! filename
      integer, optional,intent(out) :: stat	! status
      integer, optional,intent(in) :: unit	! unit

! !EXAMPLES: (to do)
! !BUGS: (to do)
! !SEE ALSO: (to do)
! !SYSTEM ROUTINES: (to do)
!
! !REVISION HISTORY:
!	21Jan00	- Jing Guo
!		. Added "direct" access to open_()
! 	16Jul96 - J. Guo	- modified as a Fortran 90 module.
!	01Dec94 - Jing G.	- added zdef_gr for small values.
!				- Original source from A. da Silva
!_______________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::open_'

! Local variables:

	character(len=64) :: str
	integer  i,k,l,lu

	integer :: idim,jdim,kdim,ldim,nvar
	integer :: ierr
	logical :: formatdefined
	real    :: zori,zinc

	if(present(stat)) stat=0

!----------------------------------------
! Use m_inpak90 read the table file
!----------------------------------------
	call i90_loadf(ctl_file,ierr)
	if(ierr /= 0 ) then
	  write(stderr,'(4a,i3)') myname_,			&
	    ': i90_loadf("',trim(ctl_file),'") error, ierr =', ierr
	  if(.not.present(stat)) call die(myname_)
	  stat=ierr
	  return
	endif

!----------------------------------------
!  Mandatory GrADS settings:
!
!	dset xdef ydef zdef tdef vars
!----------------------------------------
! DSET
	call i90_label('DSET',ierr)
	if(ierr /= 0) call i90_label('dset',ierr)
	if(ierr == 0) call i90_gtoken(gs%file, ierr)
	if(ierr /= 0) then
	  write(stderr,'(4a,i3)') myname_,			&
	    ': DSET error with "',trim(ctl_file),'", ierr = ',ierr
	  if(.not.present(stat)) call die(myname_)
	  stat=ierr
	  return
	endif
	if(gs%file(1:1) == '^') then
	  gs%file=gs%file(2:)
	  i=index(ctl_file,'/',back=.true.)
	  if(i > 0) gs%file=ctl_file(1:i)//gs%file
	endif
!----------------------------------------
! XDEF
	gs%idim = -1
	call i90_label('XDEF',ierr)
	if(ierr /= 0) call i90_label('xdef',ierr)
	if(ierr == 0) gs%idim = i90_gint(ierr)
	if(ierr /= 0) then
	  write(stderr,'(2a,i3)') myname_,': XDEF error, ierr = ',ierr
	  if(.not.present(stat)) call die(myname_)
	  stat=ierr
	  return
	endif
	idim=gs%idim
!----------------------------------------
! YDEF
	gs%jdim = -1
	call i90_label('YDEF',ierr)
	if(ierr /= 0) call i90_label('ydef',ierr)
	if(ierr == 0) gs%jdim = i90_gint(ierr)
	if(ierr /= 0) then
	  write(stderr,'(2a,i3)') myname_,': YDEF error, ierr = ',ierr
	  if(.not.present(stat)) call die(myname_)
	  stat=ierr
	  return
	endif
	jdim=gs%jdim
!----------------------------------------
! ZDEF
	gs%kdim = -1
	call i90_label('ZDEF',ierr)
	if(ierr /= 0) call i90_label('zdef',ierr)
	if(ierr == 0) gs%kdim = i90_gint(ierr)
	if(ierr /= 0) then
	  write(stderr,'(2a,i3)') myname_,': ZDEF error, ierr = ',ierr
	  if(.not.present(stat)) call die(myname_)
	  stat=ierr
	  return
	endif
	kdim=gs%kdim

	if(kdim>0) then
	  call i90_gtoken(str,ierr)
	  if(ierr /=0) then
	    write(stderr,'(2a,i3)') myname_,	&
		': ZDEF type error, ierr = ',ierr
	    if(.not.present(stat)) call die(myname_)
	    stat=ierr
	    return
	  endif

	  str=uppercase(str)
	  select case(str)
	  case ('LEVELS')

	    allocate(gs%zdef(1:kdim),stat=ierr)
	    if(ierr /= 0) then
	      write(stderr,'(2a,i4)') myname_,	&
		': allocate(gs%zdef) error, stat = ',ierr
	      if(.not.present(stat)) call die(myname_)
	      stat=ierr
	      return
	    endif

		if(mall_ison()) call mall_mci(gs%zdef,myname)

	    k=1
	    do while(k<=kdim)
	      gs%zdef(k)=i90_gfloat(ierr)
	      if(ierr/=0) then
		call i90_gline(ierr)
		if(ierr/=0) then
		  call perr(myname_,'i90_gline()',ierr)
		  call perr(myname_,'incomplete ZDEF level error at k',k)
		  if(.not.present(stat)) call die(myname_)
		  stat=ierr
		  return
		endif
	      else
	        k=k+1
	      endif
	    end do

	  case ('LINEAR')
	    allocate(gs%zdef(1:kdim),stat=ierr)
	    if(ierr /= 0) then
	      write(stderr,'(2a,i4)') myname_,	&
		': allocate(gs%zdef) error, stat = ',ierr
	      if(.not.present(stat)) call die(myname_)
	      stat=ierr
	      return
	    endif

		if(mall_ison()) call mall_mci(gs%zdef,myname)

	    zori=i90_gfloat(ierr)
	    if(ierr.eq.0) zinc=i90_gfloat(ierr)

	    if(ierr /= 0) then
	      write(stderr,'(2a,i3)') myname_,	&
		': ZDEF linear error, ierr = ',ierr
	      write(stderr,'(2a,g12.5)') myname_,	&
		': ZDEF linear error, zori = ',zori
	      write(stderr,'(2a,g12.5)') myname_,	&
		': ZDEF linear error, zinc = ',zinc
	      if(.not.present(stat)) call die(myname_)
	      stat=ierr
	      return
	    endif

	    do k=1,kdim
	      gs%zdef(k)=zori+(k-1)*zinc
	    end do

	  case default
	    write(stderr,'(4a)') myname_,	&
		': unknown ZDEF type, "',trim(str),'"'
	    if(.not.present(stat)) call die(myname_)
	    stat=1
	    return
	  end select

	endif
!----------------------------------------
! TDEF
	gs%ldim=-1
	call i90_label('TDEF',ierr)
	if(ierr /= 0) call i90_label('tdef',ierr)
	if(ierr == 0) gs%ldim=i90_gint(ierr)
	if(ierr /= 0) then
	  write(stderr,'(2a,i3)') myname_,': TDEF error, ierr = ',ierr
	  if(.not.present(stat)) call die(myname_)
	  stat=ierr
	  return
	endif
	ldim=gs%ldim
! _TODO_ : read %nymd, %nhms, and %nh00 (incr) from TDEF
	gs%nymd=NYMD_
	gs%nhms=NHMS_
	gs%nh00=NH00_
!----------------------------------------
! VARS -- ENDVARS
	gs%nvar=-1
	call i90_label('VARS',ierr)
	if(ierr /= 0) call i90_label('vars',ierr)
	if(ierr == 0) gs%nvar=i90_gint(ierr)
	if(ierr /= 0) then
	  write(stderr,'(2a,i3)') myname_,': VARS error, ierr = ',ierr
	  if(.not.present(stat)) call die(myname_)
	  stat=ierr
	  return
	endif
	nvar=gs%nvar

	allocate( gs%vname(nvar),gs%n_rec(nvar),	&
		  gs%i_rec(nvar), stat=ierr)

	if(ierr /= 0) then
	  write(stderr,'(2a,i4)') myname_,	&
	    ': allocate(VARS) error, stat = ',ierr
	  if(.not.present(stat)) call die(myname_)
	  stat=ierr
	  return
	endif

		if(mall_ison()) then
		  call mall_mci(gs%vname,myname)
		  call mall_mci(gs%n_rec,myname)
		  call mall_mci(gs%i_rec,myname)
		endif

!     Get variable names and labels
!     -----------------------------
	ierr=0
	do k=1,nvar
	  if(ierr == 0) call i90_gline(ierr)
	  if(ierr == 0) call i90_gtoken(str,ierr)
	  if(ierr == 0) l=i90_gint(ierr)
	  if(ierr == 0) then
	    gs%vname(k)=str
	    gs%n_rec(k)=l
	  endif
	end do
	if(ierr /= 0) then
	  write(stderr,'(2a,i3)') myname_,	&
	    ': VARS entry error, line ',k+1
	  if(.not.present(stat)) call die(myname_)
	  stat=2
	  return
	endif

!	if(ierr == 0 .and. (.upper.trim(str) == 'ENDVARS') exit
!
!	if(k /= nvar) then
!	  write(stdout,'(2a)') myname_,	&
!	    ': mismatched VARS <nvar> and ENDVARS?'
!	  gs%nvar=min(nvar,k)
!	  nvar=gs%nvar
!	  return
!	endif

	gs%i_rec(1)=1
	do k=2,nvar
	  gs%i_rec(k)=gs%i_rec(k-1)+max(gs%n_rec(k-1),1)
	end do
	gs%nblock = gs%i_rec(nvar)+max(gs%n_rec(nvar),1) -1

!----------------------------------------
! Optional settings
!
!	options
!----------------------------------------
! OPTIONS 
	formatdefined=.false.

	gs%iacc = iacc_DIRECT
	call i90_label('OPTIONS',ierr)
	if(ierr /= 0) call i90_label('options',ierr)
	if(ierr == 0) then

	  call i90_gtoken(str,ierr)
	  if(ierr == 0) then

	    str=uppercase(str)
	    select case(str)
	    case('SEQUENTIAL')
	      formatdefined=.true.
	      gs%iacc = iacc_SEQUENTIAL

	    case('DIRECT')
	      formatdefined=.true.
	      gs%iacc = iacc_DIRECT

	    case default
	      write(stderr,'(4a)') myname_,	&
	        ': unsupported option, "',trim(str),'"'
	      if(.not.present(stat)) call die(myname_)
	      stat=3
	      return
	    end select

	  endif
	endif

!----------------------------------------
! Optional settings
!
!	format
!----------------------------------------
! FORMAT
	if(.not. formatdefined) then
	  gs%iacc = iacc_DIRECT
	  call i90_label('FORMAT',ierr)
	  if(ierr /= 0) call i90_label('format',ierr)
	  if(ierr == 0 ) then
	    call i90_gtoken(str,ierr)
	    if(ierr == 0) then

	      str=uppercase(str)
	      if(str /= 'DIRECT' .and. str /= 'SEQUENTIAL') then
	        write(stderr,'(4a)') myname_,	&
	          ': invalid FORMAT option, "',trim(str),'"'
	        if(.not.present(stat)) call die(myname_)
	        stat=3
	        return
	      endif

	      if(str == 'SEQUENTIAL') gs%iacc = iacc_SEQUENTIAL
	    endif
	  endif
	endif

!----------------------------------------
! Optional settings
!----------------------------------------
! UNDEF
	gs%undef=UDEF_
	call i90_label('UNDEF',ierr)
	if(ierr /= 0) call i90_label('undef',ierr)
	if(ierr == 0) then
	  gs%undef=i90_gfloat(ierr)
	  if(ierr /= 0) then
	    write(stderr,'(2a,i3)') myname_,': UNDEF error, ierr =',ierr
	    if(.not.present(stat)) stat=0
	    stat=3
	    return
	  endif
	endif

!----------------------------------------
	call i90_release(ierr)
	  if(ierr /= 0) then
	    call perr(myname_,'i90_release()',ierr)
	    if(.not.present(stat)) call die(myname_)
	    stat=ierr
	    return
	  endif
!----------------------------------------

!	lu=gs%lu
!	if(lu >= 0) close(lu)  ! it does not make sense
	gs%lu=-1

	!--------------------------------------------------------
		! allocate the input buffer

	allocate(gs%dbuf(gs%idim,gs%jdim), stat=ierr)
	if(ierr /= 0) then
	  write(stderr,'(2a,i5)') myname_,	&
	    ': allocate(gs%dbuf) error, stat =',ierr
	  if(.not.present(stat)) call die(myname_)
	  stat=ierr
	  return
	endif

		if(mall_ison()) call mall_mci(gs%dbuf,myname)

	gs%lu = luavail()
	if(present(unit)) gs%lu=unit
	gs%irec=1
	call opendset_(gs%lu,gs%file,gs%iacc,gs%dbuf,gs%ilen,ierr)
		if(ierr/=0) then
		  call perr(myname_,'opendset_()',ierr)
		  if(.not.present(stat)) call die(myname_)
		  stat=ierr
		  return
		endif

end subroutine open_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: opendset_ - open a DSET file
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine opendset_(lu,name,iacc,dbuf,ilen,ierr)
      use m_ioutil,only : opnieee
      use m_die,   only : perr
      use m_ior4,  only : iacc_DIRECT,iacc_SEQUENTIAL
      implicit none

      integer,         intent(in)  :: lu
      character(len=*),intent(in)  :: name
      integer,         intent(in)  :: iacc
      real*4,dimension(:,:),intent(in) :: dbuf

      integer,         intent(out) :: ilen
      integer,         intent(out) :: ierr

! !REVISION HISTORY:
! 	21Jan00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::opendset_'
  character(len=16) :: clen

  ilen=0
  inquire(iolength=ilen) dbuf

  select case(iacc)
  case(iacc_SEQUENTIAL)

    ilen=0		! reset %ilen to avoid confusion
    call opnieee(lu,trim(name),'old',ierr)
	  if(ierr.ne.0) then
	    call perr(myname_,'opnieee("'//	&
		trim(name)//'")',ierr		)
	    return
	  endif

  case(iacc_DIRECT)

    call opnieee(lu,trim(name),'old',ierr,recl=ilen)
	  if(ierr.ne.0) then
	    clen='****************'
	    write(clen,'(i16)',iostat=ierr) ilen
	    clen=adjustl(clen)
	    call perr(myname_,'opnieee("'//	&
		trim(name)//'",recl='//		&
		trim(clen)//')',ierr		)
	    return
	  endif
	
  case default

	  call perr(myname_,'unknown iacc',iacc)
	  return
  end select

	!--------------------------------------------------------
end subroutine opendset_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!
! !IROUTINE: close_ - close a rGrADS variable
!
! !DESCRIPTION: (to do)
!
! !INTERFACE:

    subroutine close_(gs,stat)
      use m_die, only : perr,die
      use m_mall,only : mall_mco,mall_ison
      use m_ioutil,only : clsieee
      use m_ior4,only : iacc_DIRECT
      implicit none

      type(rGrADS), intent(inout) :: gs
      integer,optional, intent(out)   :: stat

! !EXAMPLES: (to do)
! !BUGS: (to do)
! !SEE ALSO: (to do)
! !SYSTEM ROUTINES: (to do)
!
! !REVISION HISTORY:
! 	18Mar97 - Jing Guo <guo@eramus> - initial prototyping and coding
!_______________________________________________________________________
  character(len=*), parameter :: myname_=myname//'::close_'

  integer :: lu
  integer :: ier

  if(present(stat)) stat=0

  lu=gs%lu
  call clsieee(lu,ier)
	if(ier/=0) then
	  call perr(myname_,'deallocate()',ier)
	  if(.not.present(stat)) call die(myname_)
	  stat=ier
	  return
	endif

	if(mall_ison()) then
	  call mall_mco(gs%zdef ,myname)
	  call mall_mco(gs%vname,myname)
	  call mall_mco(gs%n_rec,myname)
	  call mall_mco(gs%i_rec,myname)
	  call mall_mco(gs%dbuf ,myname)
	endif

  deallocate(gs%zdef,gs%vname,gs%n_rec,gs%i_rec,gs%dbuf,stat=ier)
	if(ier/=0) then
	  call perr(myname_,'deallocate()',ier)
	  if(.not.present(stat)) call die(myname_)
	  stat=ier
	  return
	endif

  gs%nvar = -1

  gs%idim = -1
  gs%jdim = -1
  gs%kdim = -1
  gs%ldim = -1

  gs%nblock = -1

  gs%irec = 0
  gs%iacc = iacc_DIRECT
  gs%ilen = -1

  gs%file = ' '
  gs%lu=-1

end subroutine close_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: input1lev_ - input a "level" of a 3-d variable
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine input1lev_(gs,vnam,llev,klev,vfld,stat)

      use m_stdio, only : stderr
      use m_die,   only : die

      implicit none

      type(rGrADS), intent(inout) :: gs	! the input
      character(len=*), intent(in)    :: vnam	! what variable?
      integer,          intent(in)    :: llev	! what time?
      integer,          intent(in)    :: klev	! which level?
      real,dimension(:,:),intent(out) :: vfld	! a 2-d gridded field
      integer, optional,intent(out)   :: stat

! !REVISION HISTORY:
! 	18Mar97 - Jing Guo <guo@eramus> - initial prototyping and coding
! 	08Dec98 - Jing Guo <guo@thunder> - modified from read_ with
!			dbuf for portable input
!EOP ___________________________________________________________________

  character(len=*), parameter :: myname_=myname//'::input1lev_'
  integer :: i,ierr,nrec,nskp,ivar,lu
  logical :: no_buffer

  if(present(stat)) stat=0

	! Sanity checks

		! Check the file status

  if( gs%file == ' ' .or. gs%lu   <  0 .or.	&
      gs%nvar <= 0   .or. gs%idim <= 0 .or.	&
      gs%jdim <= 0   .or. gs%ldim <= 0		) then

    write(stderr,'(2a)') myname_,': uninitialized type(rGrADS)?'
    if(.not.present(stat)) call die(myname_)
    stat=1
    return
  endif

		! Check the buffer dimensions

  if(size(vfld,1) /= gs%idim .or. size(vfld,2) /= gs%jdim) then

    write(stderr,'(2a,$)') myname_,': invalid arguments'
    write(stderr,'(a,2i6,a,$)') ', shape(vfld) = (',shape(vfld),')'
    write(stderr,'(a,2i6,a,$)') ', gs%[ij]dim = (',gs%idim,gs%jdim,')'
    write(stderr,*)
    if(.not.present(stat)) call die(myname_)
    stat=2
    return
  endif

		! Check/index the requested variable

  ivar=lindex_(gs%nvar,gs%vname,vnam)
  if(ivar <= 0) then
    write(stderr,'(4a)') myname_,': unknown variable "',trim(vnam),'"'
    if(.not.present(stat)) call die(myname_)
    stat=3
    return
  endif

		! Check the requested time

  if(llev < 1 .or. llev > gs%ldim) then
    write(stderr,'(2a,$)') myname_,': invalid time request'
    write(stderr,'(2(a,i3))') ', llev =',llev,', gs%ldim =',gs%ldim
    if(.not.present(stat)) call die(myname_)
    stat=4
    return
  endif

		! Check the requested level

  select case(gs%n_rec(ivar))
  case (0)
    if(klev/=0) then
      write(stderr,'(2a)',advance='no') myname_,		&
	': invalid level request for a 2-d field'
      write(stderr,'(a,i3,3a,i3,a,i3)') ', klev =',klev,	&
	', gs%n_rec("',trim(vnam),'") =', gs%n_rec(ivar),	&
	', gs%kdim =',gs%kdim
      if(.not.present(stat)) call die(myname_)
      stat=5
      return
    endif

  case default
    if(klev < 1 .or. klev > gs%n_rec(ivar) .or. klev > gs%kdim) then
      write(stderr,'(2a)',advance='no') myname_,		&
	': invalid level request for a 3-d field'
      write(stderr,'(a,i3,3a,i3,a,i3)') ', klev =',klev,	&
	', gs%n_rec("',trim(vnam),'") =', gs%n_rec(ivar),	&
	', gs%kdim =',gs%kdim
      if(.not.present(stat)) call die(myname_)
      stat=6
      return
    endif
  end select

	!--------------------------------------------------------
	! Compute the record number.  Note if klev==0, this routine
	! can be used to read a 2-d variable with %n_rec(ivar)==0.

  nrec = (llev-1) * gs%nblock + gs%i_rec(ivar)
  if(klev/=0) nrec = nrec + klev-1

	!--------------------------------------------------------
	! Read the nrec-th record.  The current position may be
	! taken into account if the file is sequentially accessed.
	! See read_() for details.

  call read_(gs%lu,gs%iacc,nrec,gs%irec,vfld,gs%dbuf,ierr)

  if(ierr /= 0) then
    write(stderr,'(2a,i5)') myname_,	&
	': read_() error, ierr =',ierr
    if(.not.present(stat)) call die(myname_)
    stat=ierr
    return
  endif

  gs%irec=nrec+1	! current record position

	!--------------------------------------------------------
end subroutine input1lev_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: input1levdp_ - input a "level" of a 3-d variable
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine input1levdp_(gs,vnam,llev,klev,vfld,stat)
      use m_realkinds,only : DP
      use m_die,   only : die,perr

      implicit none

      type(rGrADS), intent(inout) :: gs	! the input
      character(len=*), intent(in)    :: vnam	! what variable?
      integer,          intent(in)    :: llev	! what time?
      integer,          intent(in)    :: klev	! which level?
      real(DP),dimension(:,:),intent(out) :: vfld	! a 2-d gridded field
      integer, optional,intent(out)   :: stat

! !REVISION HISTORY:
! 	18Mar97 - Jing Guo <guo@eramus> - initial prototyping and coding
! 	08Dec98 - Jing Guo <guo@thunder> - modified from read_ with
!			dbuf for portable input
!EOP ___________________________________________________________________

  character(len=*), parameter :: myname_=myname//'::input1levdp_'
  integer :: ierr
  real,dimension(size(vfld,1),size(vfld,2)) :: vinp

  if(present(stat)) stat=0
  call input1lev_(gs,vnam,llev,klev,vinp,stat=ierr)
    if(ierr/=0) then
      call perr(myname_,'inputllev_("'//trim(vnam)//'")',ierr)
      if(.not.present(stat)) call die(myname_)
      stat=ierr
      return
    endif
  vfld=vinp
end subroutine input1levdp_
!=======================================================================

  function lindex_(nlst,lsts,entr)
    use m_chars, only : uppercase
    implicit none
    integer,                      intent(in) :: nlst
    character(len=*),dimension(:),intent(in) :: lsts
    character(len=*),             intent(in) :: entr

    integer :: lindex_	! the result

	!--------------------------------------------------------
    integer :: i

	!--------------------------------------------------------
    lindex_=0
    do i=1,nlst
      if(uppercase(entr) == uppercase(lsts(i))) then
	lindex_=i
	return
      endif
    end do
  end function lindex_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: read_ - read the n-th record
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine read_(lu,iacc,nrec,irec,vfld,dbuf,ierr)
      use m_die,only : perr
      use m_realkinds,only : kind_R4,kind_R8
      use m_ior4,only : iacc_SEQUENTIAL,iacc_DIRECT
      implicit none
      integer,intent(in)  :: lu		! the input unit
      integer,intent(in)  :: iacc	! access
      integer,intent(in)  :: nrec	! which to read
      integer,intent(in)  :: irec	! where it is now
      real,  dimension(:,:),intent(out) :: vfld
      real(kind=kind_R4),dimension(:,:),intent(out) :: dbuf
      integer,intent(out) :: ierr

! !REVISION HISTORY:
! 	22Jan99 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::read_'
  logical :: no_buffer
  integer :: nskp
  integer :: i
  integer :: nx,ny
  real :: xdum

  no_buffer =	kind(dbuf)   == kind(vfld)	.and.	&
		size(dbuf,1) == size(vfld,1)	.and.	&
		size(dbuf,2) == size(vfld,2)

  ierr=-1
  select case(iacc)
  case(iacc_SEQUENTIAL)

			! Sequential skip
    ierr=0
    if(nrec < irec) then
      rewind(lu)		! can we trust backspace()?
      nskp=nrec-1
    else
      nskp=nrec-irec
    endif
    do i=1,nskp
      read(lu,iostat=ierr)
	if(ierr/=0) then
	  call perr(myname_,'read()',i)
	  call perr(myname_,'skip_sequential',ierr)
	  return
	endif
    end do

			! Sequential read()
    if(ierr == 0) then
      if(no_buffer) then
        read(lu,iostat=ierr) vfld
      else
        read(lu,iostat=ierr) dbuf
	 if(ierr == 0) then
	   nx=min(size(vfld,1),size(dbuf,1))
	   ny=min(size(vfld,2),size(dbuf,2))
	   vfld(1:nx,1:ny)=dbuf(1:nx,1:ny)
	 endif
      endif
      	if(ierr/=0) then
	  if(no_buffer) then
	    call perr(myname_,'read_sequential',ierr)
	  else
	    call perr(myname_,'buffered_read_sequential',ierr)
	  endif
	  return
	endif
    endif

  case(iacc_DIRECT)
			! Direct read()
    if(ierr == 0) then
      if(no_buffer) then
	read(lu,rec=nrec,iostat=ierr) vfld
      else
	read(lu,rec=nrec,iostat=ierr) dbuf
	if(ierr == 0) then
	  nx=min(size(vfld,1),size(dbuf,1))
	  ny=min(size(vfld,2),size(dbuf,2))
	  vfld(1:nx,1:ny)=dbuf(1:nx,1:ny)
	endif
      endif
      	if(ierr/=0) then
	  if(no_buffer) then
	    call perr(myname_,'read_direct',ierr)
	  else
	    call perr(myname_,'buffered_direct',ierr)
	  endif
	  return
	endif
    endif
  end select
end subroutine read_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: inquire_ - inquire for object attributes
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine inquire_(ob,nlon,nlat,nlev,var,nvar,ntim,	&
    	nymd,nhms,incr,udef,stat)
      use m_die,only : perr,die
      implicit none
      type(rGrADS),intent(in) :: ob		! file handle
      integer,optional,intent(out) :: nlon	! no. of longitudes
      integer,optional,intent(out) :: nlat	! no. of latitudes
      integer,optional,intent(out) :: nlev	! no. of levels
      character(len=*),optional,intent(in) :: var ! target of nlev
      integer,optional,intent(out) :: nvar	! no. of variables
      integer,optional,intent(out) :: ntim	! no. of time segments

      integer,optional,intent(out) :: nymd	! date in yyyymmdd
      integer,optional,intent(out) :: nhms	! time hhmmss
      integer,optional,intent(out) :: incr	! increment in hhmmss
      real,optional,intent(out) :: udef		! flag of undefined

      integer,optional,intent(out) :: stat	! return status

! !REVISION HISTORY:
! 	20Dec05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::inquire_'
  integer :: ier,ivar

  if(present(stat)) stat=0

  if(present(nlon)) nlon=ob%idim
  if(present(nlat)) nlat=ob%jdim

  if(present(nlev)) then
    if(present(var)) then
      ivar=lindex_(ob%nvar,ob%vname,var)
      if(ivar==0) then
        call perr(myname_,'unknown variable name, "'//trim(var)//'"')
        if(.not.present(stat)) call die(myname_)
	stat=-1
    	return	! Note nlev is undefined
      endif
      nlev=ob%n_rec(ivar)

    else
      nlev=ob%kdim
    endif
  endif

  if(present(nvar)) nvar=ob%nvar
  if(present(ntim)) ntim=ob%ldim

  if(present(nymd)) nymd=ob%nymd
  if(present(nhms)) nhms=ob%nhms
  if(present(incr)) incr=ob%nh00
  if(present(udef)) udef=ob%undef

! _TODO_ : do not use optional argument nymd, nhms, and incr until
! _TODO_ : reading fron TDEF is implemented.
if(present(nymd) .or. present(nhms) .or. present(incr))	&
  call die(myname_,'_TODO_ : optional nymd, nhms, and incr')

end subroutine inquire_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: getdims_ get the dimensions of a given variable
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine getdims_(gs,vnam,nlon,nlat,nlev,udef,stat)
      use m_stdio, only : stderr
      use m_die,   only : die
      implicit none
      character(len=*), intent(in) :: vnam
      type(rGrADS), intent(in) :: gs
      integer,intent(out) :: nlon
      integer,intent(out) :: nlat
      integer,intent(out) :: nlev
      real,   optional,intent(out) :: udef
      integer,optional,intent(out) :: stat

! !REVISION HISTORY:
! 	08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::getdims_'
  integer :: ivar

  if(present(stat)) stat=0

  nlon=gs%idim
  nlat=gs%jdim

  ivar=lindex_(gs%nvar,gs%vname,vnam)
  if(ivar==0) then
    write(stderr,'(4a)') myname_,	&
	': unknown variable name, "',vnam,'"'
    if(.not.present(stat)) call die(myname_)
    stat=-1
    return
  endif

  nlev=gs%n_rec(ivar)
  if(present(udef)) udef=gs%undef

	! A post-condition?

  if(nlev < 0 .or. nlev > gs%kdim) then
    write(stderr,'(4a,i4)') myname_,	&
	': improper number of records, gs%n_rec("',vnam,'") =', nlev
    if(.not.present(stat)) call die(myname_)
    stat=-1
    return
  endif

end subroutine getdims_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: zdef_ - return the leading levels of ZDEF
!
! !DESCRIPTION:
!
! !INTERFACE:

    function zdef_(gs,nlev)
      use m_stdio, only : stderr
      use m_die,   only : die
      implicit none
      type(rGrADS),intent(in) :: gs
      integer,intent(in) :: nlev
      real,dimension(nlev) :: zdef_

! !REVISION HISTORY:
! 	08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::zdef_'

  if(nlev > size(gs%zdef) ) then
    write(stderr,'(2a,i4)') myname_,	&
	': improper number of "ZDEF" levels, nlev =', nlev
    call die(myname_)
  endif

  zdef_(1:nlev)=gs%zdef(1:nlev)

end function zdef_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ptr_zdef_ - pointers of %zdef
!
! !DESCRIPTION:
!
! !INTERFACE:

    function ptr_zdef_(gs)
      implicit none
      real,dimension(:),pointer :: ptr_zdef_
      type(rGrADS),intent(in) :: gs

! !REVISION HISTORY:
! 	27Dec05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::ptr_zdef_'

  ptr_zdef_ => gs%zdef(:)
end function ptr_zdef_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ptr_vars_ - pointers of %vars
!
! !DESCRIPTION:
!
! !INTERFACE:

    function ptr_vars_(gs)
      implicit none
      character(len=LEN_VARS),dimension(:),pointer :: ptr_vars_
      type(rGrADS),intent(in) :: gs

! !REVISION HISTORY:
! 	27Dec05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::ptr_vars_'

  ptr_vars_ => gs%vname(:)
end function ptr_vars_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ptr_nrec_ - pointers of %nrec
!
! !DESCRIPTION:
!
! !INTERFACE:

    function ptr_nrec_(gs)
      implicit none
      integer,dimension(:),pointer :: ptr_nrec_
      type(rGrADS),intent(in) :: gs

! !REVISION HISTORY:
! 	27Dec05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::ptr_nrec_'

  ptr_nrec_ => gs%n_rec(:)
end function ptr_nrec_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: input3d_ - input a 3-d field
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine input3d_(gs,vnam,llev,vfld,stat)
      use m_stdio, only : stderr
      use m_die,   only : die
      implicit none
      type(rGrADS),intent(inout) :: gs
      character(len=*),intent(in) :: vnam
      integer,         intent(in) :: llev
      real,            intent(out):: vfld(:,:,:)
      integer,optional,intent(out):: stat

! !REVISION HISTORY:
! 	08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::input3d_'

  integer :: nlon,nlat,nlev
  integer :: k,ier

  if(present(stat)) stat=0

  call getdims_(gs,vnam,nlon,nlat,nlev)

  do k=1,nlev
    call input1lev_(gs,vnam,llev,k, vfld(:,:,k), stat=ier)

    if(ier /= 0) then
      write(stderr,'(2a,i4)') myname_,	&
	': input1lev_() error, stat =',ier
      if(.not.present(stat)) call die(myname_)
      stat=ier
      return
    endif
  end do

end subroutine input3d_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: input3ddp_ - input a 3-d field
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine input3ddp_(gs,vnam,llev,vfld,stat)
      use m_realkinds,only : DP
      use m_stdio, only : stderr
      use m_die,   only : die
      implicit none
      type(rGrADS),intent(inout) :: gs
      character(len=*),intent(in) :: vnam
      integer,         intent(in) :: llev
      real(DP),        intent(out):: vfld(:,:,:)
      integer,optional,intent(out):: stat

! !REVISION HISTORY:
! 	08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::input3ddp_'

  integer :: nlon,nlat,nlev
  integer :: k,ier

  if(present(stat)) stat=0

  call getdims_(gs,vnam,nlon,nlat,nlev)

  do k=1,nlev
    call input1levdp_(gs,vnam,llev,k, vfld(:,:,k), stat=ier)

    if(ier /= 0) then
      write(stderr,'(2a,i4)') myname_,	&
	': input1levdp_() error, stat =',ier
      if(.not.present(stat)) call die(myname_)
      stat=ier
      return
    endif
  end do

end subroutine input3ddp_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: input2d_ - input a 2-d field
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine input2d_(gs,vnam,llev,vfld,stat)
      use m_stdio, only : stderr
      use m_die,   only : die
      implicit none
      type(rGrADS),intent(inout) :: gs
      character(len=*),intent(in) :: vnam
      integer,         intent(in) :: llev
      real,            intent(out):: vfld(:,:)
      integer,optional,intent(out):: stat

! !REVISION HISTORY:
! 	08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::input2d_'

  integer :: ier

  if(present(stat)) stat=0

  call input1lev_(gs,vnam,llev,0, vfld(:,:), stat=ier)

    if(ier /= 0) then
      write(stderr,'(2a,i4)') myname_,	&
	': input1lev_() error, stat =',ier
      if(.not.present(stat)) call die(myname_)
      stat=ier
      return
    endif

end subroutine input2d_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: input2ddp_ - input a 2-d field
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine input2ddp_(gs,vnam,llev,vfld,stat)
      use m_realkinds,only : DP
      use m_stdio, only : stderr
      use m_die,   only : die
      implicit none
      type(rGrADS),intent(inout) :: gs
      character(len=*),intent(in) :: vnam
      integer,         intent(in) :: llev
      real(DP),        intent(out):: vfld(:,:)
      integer,optional,intent(out):: stat

! !REVISION HISTORY:
! 	08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::input2ddp_'

  integer :: ier

  if(present(stat)) stat=0

  call input1levdp_(gs,vnam,llev,0, vfld(:,:), stat=ier)

    if(ier /= 0) then
      write(stderr,'(2a,i4)') myname_,	&
	': input1levdp_() error, stat =',ier
      if(.not.present(stat)) call die(myname_)
      stat=ier
      return
    endif

end subroutine input2ddp_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: bcast_ - populate meta data to all processors
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine bcast_(gs,root,comm)
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_type
      use m_die   ,only : MP_die
      implicit none
      type(rGrADS),intent(inout) :: gs
      integer,intent(in) :: root
      integer,intent(in) :: comm

! !REVISION HISTORY:
! 	01Jun06	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::bcast_'
  integer :: ier,myPE
  integer,dimension(8) :: ibufr

  call MP_comm_rank(comm,myPE,ier)
    if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  if(myPE==root) then
    ibufr(1)=gs%idim
    ibufr(2)=gs%jdim
    ibufr(3)=gs%kdim
    ibufr(4)=gs%nvar
    ibufr(5)=gs%ldim
    ibufr(6)=gs%nymd
    ibufr(7)=gs%nhms
    ibufr(8)=gs%nh00
  endif
  
  call MPI_bcast(ibufr,size(ibufr),MP_type(ibufr),root,comm,ier)
    if(ier/=0) call MP_die(myname_,'MPI_bcast(ibufr)',ier)

  if(myPE/=root) then
    gs%idim=ibufr(1)
    gs%jdim=ibufr(2)
    gs%kdim=ibufr(3)
    gs%nvar=ibufr(4)
    gs%ldim=ibufr(5)
    gs%nymd=ibufr(6)
    gs%nhms=ibufr(7)
    gs%nh00=ibufr(8)
    gs%lu  = -1
  endif

  call MPI_bcast(gs%undef,1,MP_type(gs%undef),root,comm,ier)
    if(ier/=0) call MP_die(myname_,'MPI_bcast(%undef)',ier)

end subroutine bcast_
end module m_rGrADS
!.
