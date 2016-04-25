!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: m_ioutil - a F90 module for several convenient I/O functions
!
! !DESCRIPTION:
!
!	m\_ioutil is a module containing several portable interfaces for
!	some highly system dependent, but frequently used I/O functions.
!
! !INTERFACE:

	module m_ioutil
	implicit none
	private	! except

	public	:: opntext,clstext ! open/close a text file
	public	:: opnieee,clsieee ! open/close a binary sequential file
	public	:: luavail	   ! return a free logical unit
	public	:: luflush	   ! flush the buffer of a given unit
	public  :: byteswap	   ! swap bytes in an integer array.
	!public	:: MX_LU

	interface byteswap; module procedure	&
	  swapI4_,	&
	  swapI8_; end interface

! !REVISION HISTORY:
!	20Dec2005 - Jing Guo <jguo@gmao.gsfc.nasa.gov>
!		  - merged changes between 1.1.2.6 and 1.1.2.8 to 1.2:
!		    added byteswap() and its interfaces.
! 	16Jul96 - J. Guo	- (to do)
! 	02Apr97 - Jing Guo <guo@eramus> - finished the coding
!	11Feb97 - Jing Guo <guo@thunder> - added luflush()
!EOP
!_______________________________________________________________________

	character(len=*),parameter :: myname="m_ioutil"
	integer,parameter :: MX_LU=255

contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: opnieee - portablly open an IEEE format file
!
! !DESCRIPTION:
!
!	Open a file in `IEEE' format.
!
!	`IEEE' format is refered as a FORTRAN "unformatted" file with
!	"sequantial" access and variable record lengths.  Under common
!	Unix, it is only a file with records packed with a leading 4-
!	byte word and a trailing 4-byte word indicating the size of
!	the record in bytes.  However, under UNICOS, it is also assumed
!	to have numerical data representations represented according to
!	the IEEE standard corresponding KIND conversions.  Under a DEC
!	machine, it means that compilations of the source code should
!	have the "-bigendian" option specified.
!
! !INTERFACE:

    subroutine opnieee(lu,fname,status,ier,recl)
      use m_stdio,only : stderr
      implicit none

      integer,         intent(in) :: lu     ! logical unit number
      character(len=*),intent(in) :: fname  ! filename to be opended
      character(len=*),intent(in) :: status ! the value for STATUS=
      integer,         intent(out):: ier    ! the status
      integer,optional,intent(in) :: recl   ! record length

! !REVISION HISTORY:
!	02Feb95 - Jing G. - First version included in PSAS.  It is not
!		used in the libpsas.a calls, since no binary data input/
!		output is to be handled.
!
! 	09Oct96 - J. Guo  - Check for any previous assign() call under
!		UNICOS.
!EOP
!_______________________________________________________________________

		! local parameter
	character(len=*),parameter :: myname_=myname//'::opnieee'

	integer,parameter :: iA=ichar('a')
	integer,parameter :: mA=ichar('A')
	integer,parameter :: iZ=ichar('z')

	logical :: direct
	character(len=16) :: clen
	character(len=len(status)) :: Ustat
	integer :: i,ic

	direct=.false.
	if(present(recl)) then
	  if(recl<0) then
	    clen='****************'
	    write(clen,'(i16)',iostat=ier) recl
	    write(stderr,'(3a)') myname_,	&
		': invalid recl, ',trim(adjustl(clen))
	    ier=-1
	    return
	  endif
	  direct = recl>0
	endif

#ifdef _UNICOS
	character(len=128) :: attr

	call asnqunit(lu,attr,ier)	! test the unit

	if(ier.eq.-1) then		! the unit is not used
	  if(direct) then
	    call asnunit(lu,'-N ieee -F null',ier)
	  else
	    call asnunit(lu,'-N ieee -F f77',ier)
	  endif
	  ier=0

	elseif(ier.ge.0) then		! the unit is already assigned
	  ier=-1
	endif
	if(ier.ne.0) return
#endif

	do i=1,len(status)
	  ic=ichar(status(i:i))
	  if(ic >= iA .and. ic <= iZ) ic=ic+(mA-iA)
	  Ustat(i:i)=char(ic)
	end do

	select case(Ustat)

	case ('APPEND')

	  if(direct) then
	    write(stderr,'(2a)') myname_,		&
		': invalid arguments, (status=="APPEND",recl>0)'
	    ier=1
	    return
	  endif

	  open(				&
	    unit	=lu,		&
	    file	=fname,		&
	    form	='unformatted',	&
	    access	='sequential',	&
	    status	='unknown',	&
	    position	='append',	&
	    iostat	=ier		)

	case default

	  if(direct) then
	    open(			&
	      unit	=lu,		&
	      file	=fname,		&
	      form	='unformatted',	&
	      access	='direct',	&
	      status	=status,	&
	      recl	=recl,		&
	      iostat	=ier		)

	  else
	    open(			&
	      unit	=lu,		&
	      file	=fname,		&
	      form	='unformatted',	&
	      access	='sequential',	&
	      status	=status,	&
	      position	='asis',	&
	      iostat	=ier		)
	  endif

	end select

	end subroutine opnieee
!-----------------------------------------------------------------------
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: clsieee - Close a logical unit opened by opnieee()
!
! !DESCRIPTION:
!
!	The reason for a paired clsieee() for opnieee() instead of a
!	simple close(), is for the portability reason.  For example,
!	under UNICOS, special system calls may be need to set up the
!	unit right, and the status of the unit should be restored upon
!	close.
!
! !INTERFACE:

	subroutine clsieee(lu,ier,status)
	  implicit none
	  integer,                    intent(in)  :: lu	   ! the unit used by opnieee()
	  integer,                    intent(out) :: ier	   ! the status
          Character(len=*), optional, intent(In)  :: status ! keep/delete

! !REVISION HISTORY:
! 	10Oct96 - J. Guo	- (to do)
!EOP
!_______________________________________________________________________
          character(len=*), parameter :: myname_ = myname//'::clsieee'
          Character(Len=6) :: status_

          status_ = 'KEEP'
          If (Present(status)) Then
             Select Case (Trim(status))
             Case ('DELETE','delete')
                status_ = 'DELETE'
             Case  ('KEEP','keep')
                status_ = 'KEEP'
             Case Default
                ier = -997
                return
             End Select
          End If
                
	  close(lu,iostat=ier,status=status_)
#ifdef _UNICOS
	  if(ier==0) call asnunit(lu,'-R',ier) ! remove attributes
#endif

	end subroutine clsieee

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: opntext - portablly open a text file
!
! !DESCRIPTION:
!
!	Open a text (ASCII) file.  Under FORTRAN, it is defined as
!	"formatted" with "sequential" access.
!
! !INTERFACE:

    subroutine opntext(lu,fname,status,ier)
      implicit none

      integer,         intent(in) :: lu     ! logical unit number
      character(len=*),intent(in) :: fname  ! filename to be opended
      character(len=*),intent(in) :: status ! the value for STATUS=<>
      integer,         intent(out):: ier    ! the status


! !REVISION HISTORY:
!
!	02Feb95 - Jing G. - First version included in PSAS and libpsas.a
! 	09Oct96 - J. Guo  - modified to allow assign() call under UNICOS
!			  = and now, it is a module in Fortran 90.
!EOP
!_______________________________________________________________________

		! local parameter
	character(len=*),parameter :: myname_=myname//'::opntext'

	integer,parameter :: iA=ichar('a')
	integer,parameter :: mA=ichar('A')
	integer,parameter :: iZ=ichar('z')

	character(len=len(status)) :: Ustat
	integer :: i,ic

#ifdef _UNICOS
	call asnunit(lu,'-R',ier)	! remove any set attributes
	if(ier.ne.0) return		! let the parent handle it
#endif

	do i=1,len(status)
	  ic=ichar(status(i:i))
	  if(ic >= iA .and. ic <= iZ) ic=ic+(mA-iA)
	  Ustat(i:i)=char(ic)
	end do

	select case(Ustat)

	case ('APPEND')

	  open(				&
	    unit	=lu,		&
	    file	=fname,		&
	    form	='formatted',	&
	    access	='sequential',	&
	    status	='unknown',	&
	    position	='append',	&
	    iostat	=ier		)

	case default

	  open(				&
	    unit	=lu,		&
	    file	=fname,		&
	    form	='formatted',	&
	    access	='sequential',	&
	    status	=status,	&
	    position	='asis',	&
	    iostat	=ier		)

	end select

	end subroutine opntext

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: clstext - close a text file opend with an opntext() call
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine clstext(lu,ier,status)
      implicit none

      integer,                    intent(in)  :: lu     ! a logical unit to close
      integer,                    intent(out) :: ier    ! the status
      Character(len=*), optional, intent(In)  :: status ! keep/delete

! !REVISION HISTORY:
! 	09Oct96 - J. Guo	- (to do)
!EOP
!_______________________________________________________________________
          character(len=*), parameter :: myname_ = myname//'::clsitext'
          Character(Len=6) :: status_

          status_ = 'KEEP'
          If (Present(status)) Then
             Select Case (Trim(status))
             Case ('DELETE','delete')
                status_ = 'DELETE'
             Case  ('KEEP','keep')
                status_ = 'KEEP'
             Case Default
                ier = -997
                return
             End Select
          End If

	close(lu,iostat=ier,status=status_)
#ifdef _UNICOS
	if(ier == 0) call asnunit(lu,'-R',ier)	! remove any attributes
#endif

	end subroutine clstext

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: luavail - locate the next available unit
!
! !DESCRIPTION:
!
!    luavail() Look for an available (not opened and not statically
!    assigned to any I/O attributes to) logical unit.
!
! !INTERFACE:

	function luavail()
	  use m_stdio
	  implicit none
	  integer :: luavail	! result

! !REVISION HISTORY:
! 	23Apr98 - Jing Guo <guo@thunder> - new prototype/prolog/code
!			- with additional unit constraints for SunOS.
!
! 	: Jing Guo, [09-Oct-96]
! 		+ Checking also Cray assign() attributes, with some
! 		  changes to the code.  See also other routines.
!
! 	: Jing Guo, [01-Apr-94]
! 		+ Initial code.
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::luavail'

	integer lu,ios
	logical inuse
	character*8 attr

	lu=-1
	ios=0
	inuse=.true.

	do while(ios.eq.0.and.inuse)
	  lu=lu+1

		! Test #1, reserved

	  inuse = lu.eq.stdout .or. lu.eq.stdin .or. lu.eq.stderr

#ifdef sysSunOS
		! Reserved units under SunOS
	  inuse = lu.eq.100 .or. lu.eq.101 .or. lu.eq.102
#endif

		! Test #2, in-use

	  if(.not.inuse) inquire(unit=lu,opened=inuse,iostat=ios)

#ifdef _UNICOS
		! Test #3, if the user has reserved the unit through
		! UNICOS' assign().

	  if(ios.eq.0 .and. .not.inuse) then
	    call asnqunit(lu,attr,ios)

		! see asnqunig(3f):
		!
		! ios ==  0, has been assigned to some attributes
		!        -1, not been assigned any attributes
		!     >   0, an error condition, but who cares why.

	    inuse=ios.ne.-1		! the unit is in-use
	    if(ios >= -1) ios=0		! still a valid test
	  endif
#endif

	  if(lu >= MX_LU) ios=-1
	end do

	if(ios.ne.0) lu=-1
	luavail=lu
end function luavail

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: luflush - a uniform interface of system flush()
!
! !DESCRIPTION:
!
!	Flush() calls available on many systems are often implementation
!	dependent.  This subroutine provides a uniform interface.  It
!	also ignores invalid logical unit value.
!
! !INTERFACE:

    subroutine luflush(unit)
      use m_stdio, only : stdout
      implicit none
      integer,optional,intent(in) :: unit

! !REVISION HISTORY:
! 	13Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::luflush'

  integer :: ier
  integer :: lu

	! Which logical unit number?

  lu=stdout
  if(present(unit)) lu=unit
  if(lu < 0) return

	! The following call may be system dependent.

#ifdef sysIRIX64
  call flush(lu,ier)
#else
#if sysAIX 
  call flush_(lu)      ! Function defined in xlf reference document.
#else
  call flush(lu)
#endif
#endif

end subroutine luflush

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: swapI4_ - swap INTEGER*4
!
! !DESCRIPTION:
!
! !INTERFACE:

    function swapI4_(ibuf)
      use m_intkinds,only : I4 => kind_i4
      implicit none
      integer(I4),dimension(:),intent(in) :: ibuf
      integer(I4),dimension(size(ibuf)) :: swapI4_

! !REVISION HISTORY:
! 	15Apr02	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::swapI4_'

  	! TRANSFER() should be used.  The current implementation may be
	! not fully portable.

  call ioutil_byteswap_(size(ibuf),4,ibuf,swapI4_)

end function swapI4_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: swapI8_ - swap INTEGER*8
!
! !DESCRIPTION:
!
! !INTERFACE:

    function swapI8_(ibuf)
      use m_intkinds,only : I8 => kind_i8
      implicit none
      integer(I8),dimension(:),intent(in) :: ibuf
      integer(I8),dimension(size(ibuf)) :: swapI8_

! !REVISION HISTORY:
! 	15Apr02	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::swapI8_'

  	! TRANSFER() should be used.  The current implementation may be
	! not fully portable.

  call ioutil_byteswap_(size(ibuf),8,ibuf,swapI8_)

end function swapI8_
end module m_ioutil

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ioutil_byteswap_ - swap bytes in each word
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine ioutil_byteswap_(nword,nbyte,ibuf,obuf)
      implicit none
      integer,intent(in) :: nword
      integer,intent(in) :: nbyte
      character(len=1),dimension(0:nbyte-1,nword),intent(in ) :: ibuf
      character(len=1),dimension(0:nbyte-1,nword),intent(out) :: obuf

! !REVISION HISTORY:
! 	15Apr02	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_='ioutil_byteswap_'
  integer :: mbyte,ibyte,jbyte

  mbyte=nbyte-1
  do ibyte=0,mbyte
    jbyte=mbyte-ibyte
    obuf(jbyte,:)=ibuf(ibyte,:)
  end do

end subroutine ioutil_byteswap_
!.
