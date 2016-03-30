!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: m_chars - a module for character class object operations
!
! !DESCRIPTION:
!
! !INTERFACE:

	module m_chars
	implicit none
	private

	public	:: operator (.upper.)	! convert a string to uppercase
	public	:: uppercase

	public	:: operator (.lower.)	! convert a string to lowercase
	public	:: lowercase

	public	:: tochars

	interface operator (.upper.)
	  module procedure upper_case
	end interface
	interface uppercase
	  module procedure upper_case
	end interface

	interface operator (.lower.)
	  module procedure lower_case
	end interface
	interface lowercase
	  module procedure lower_case
	end interface

	interface tochars; module procedure &
	  dptochars_, &
	  sptochars_; end interface

! !REVISION HISTORY:
! 	16Jul96 - J. Guo	- (to do)
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname='m_chars'

contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: dptochars_ - make a list of values
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine dptochars_(vals,bufr,len)
      use m_realkinds,only : DP
      implicit none
      real(DP),dimension(:),intent(in) :: vals
      character(len=*),intent(out) :: bufr
      integer,optional,intent(out) :: len

! !REVISION HISTORY:
! 	09May06	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::dptochars_'
  integer :: ln

  ln=listvals(size(vals),real(vals),bufr)
  if(present(len)) len=ln
end subroutine dptochars_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: sptochars_ - make a list of values
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine sptochars_(vals,bufr,len)
      use m_realkinds,only : SP
      implicit none
      real(SP),dimension(:),intent(in) :: vals
      character(len=*),intent(out) :: bufr
      integer,optional,intent(out) :: len

! !REVISION HISTORY:
! 	09May06	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::sptochars_'
  integer :: ln

  ln=listvals(size(vals),vals,bufr)
  if(present(len)) len=ln
end subroutine sptochars_

	integer function listvals(nv,vals,line)
!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!
! !ROUTINE:  listvals
! 
! !DESCRIPTION: list data values in the most compact formats
!
! !CALLING SEQUENCE:
!	integer  listvals
!	external listvals
!	l=listvals(nv,vals,line)
!
! !INPUT PARAMETERS:
!	integer		nv		! number of vals[]
!	real		vals(nv)	! values
!
! !OUTPUT PARAMETERS:
!	character*(*)	line		! output buffer
!
! !BUGS:
!	Assumed upto 5 digits before and 2 digits after the decimal
!	point, which is reasonable for most real*4 values and some
!	physical quantities, such as pressures.
!
! !SEE ALSO:
!
! !SYSTEM ROUTINES:
!
! !FILES USED:
!
! !REVISION HISTORY:
! 	23Aug95 - J. Guo	- added the prolog
!
!-----------------------------------------------------------------------
	implicit none
!   ..Inputs
	integer		nv		! number of vals[]
	real		vals(nv)	! values
!   ..Outputs
	character*(*)	line		! output buffer

!	..Locals
	character*8 bufr
	logical next
	integer i,j,k
	integer ln,ls,l

!   ..Checking buffer size
	ln=len(line)

!   ..For each item in vals(:), upto where the output buffer permits
	ls=0
	k=0
	do while(ls.lt.ln.and.k.lt.nv)
	  k=k+1

!	..Assumed upto [5 digits].[2 digits].

	  write(bufr,'(f8.2)') vals(k)

!	..Checking insignificant leading bytes
	  i=0
	  next=.true.
	  do while(next.and.i.lt.8)
	    i=i+1
	    next=bufr(i:i).eq.' '.or.bufr(i:i).eq.'0'
	  end do

!	..Checking insignificant trailing bytes
	  j=8+1
	  next=.true.
	  do while(next.and.j.gt.i)
	    j=j-1
	    next=bufr(j:j).eq.' ' .or. bufr(j:j).eq.'0'
	  end do

!	..Skipping the dicimal point "."?
	  if(bufr(j:j).eq.'.') then
	    if(j.gt.i) then
	      j=j-1
	    else
	      bufr(j:j)='0'
	    endif
	  endif

	  l=ls+1
	  ls=min(l + j-i+1,ln)
	  line(l:ls)=' '//bufr(i:j)

	end do

!   ..Clean up for the output
	if(ls.lt.ln) line(ls+1:ln)=' '
	listvals=ls
	end function listvals
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: upper_case - convert lowercase letters to uppercase.
!
! !DESCRIPTION:
!
! !INTERFACE:

  function upper_case(str) result(ustr)
    implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: ustr

! !REVISION HISTORY:
! 	13Aug96 - J. Guo	- (to do)
!EOP
!_______________________________________________________________________
    integer i
    integer,parameter :: il2u=ichar('A')-ichar('a')

    ustr=str
    do i=1,len_trim(str)
      if(str(i:i).ge.'a'.and.str(i:i).le.'z')	&
      	ustr(i:i)=char(ichar(str(i:i))+il2u)
    end do
  end function upper_case

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: lower_case - convert uppercase letters to lowercase.
!
! !DESCRIPTION:
!
! !INTERFACE:

  function lower_case(str) result(lstr)
    implicit none
    character(len=*), intent(in) :: str
    character(len=len(str))      :: lstr

! !REVISION HISTORY:
! 	13Aug96 - J. Guo	- (to do)
!EOP
!_______________________________________________________________________
    integer i
    integer,parameter :: iu2l=ichar('a')-ichar('A')

    lstr=str
    do i=1,len_trim(str)
      if(str(i:i).ge.'A'.and.str(i:i).le.'Z')	&
      	lstr(i:i)=char(ichar(str(i:i))+iu2l)
    end do
  end function lower_case

end module m_chars
!.
