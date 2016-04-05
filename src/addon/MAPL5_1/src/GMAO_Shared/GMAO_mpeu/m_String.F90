!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_String - string pointers
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_String
      implicit none
      private	! except

      public :: String		! The class data structure
      public :: toChar		! convert to a CHARACTER(*)
      public :: char		! convert to a CHARACTER(*)

      public :: String_init
      public :: init		! set a CHARACTER(*) type to a String

      public :: String_clean
      public :: clean		! clean a String

      public :: String_len
      public :: len		! length of a String

      public :: String_bcast
      public :: bcast		! Broadcast a String

      public :: String_mci
      public :: String_mco

      public :: ptr_chars

    type String
      character(len=1),dimension(:),pointer :: c
    end type String

#ifdef _TODO_
    public :: equal
	interface equal; module procedure s_equal_s; end interface

    	! equal(sa,sb, adjustl=..,ignorecase=..,len=..)
#endif


! !REVISION HISTORY:
! 	22Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  interface char;  module procedure	&
	str2ch0_,	&
	ch12ch0_
  end interface

  interface toChar;  module procedure	&
	str2ch0_,	&
	ch12ch0_
  end interface

  interface String_init;  module procedure	&
	initc_,		&
	inits_
  end interface

  interface init;  module procedure	&
	initc_,		&
	inits_
  end interface

  interface String_clean; module procedure clean_; end interface
  interface clean; module procedure clean_; end interface
  interface String_len; module procedure len_; end interface
  interface len; module procedure len_; end interface
  interface String_bcast; module procedure bcast_; end interface
  interface bcast; module procedure bcast_; end interface

  interface String_mci; module procedure	&
    mci0_,	&
    mci1_,	&
    mci2_,	&
    mci3_
  end interface

  interface String_mco; module procedure	&
    mco0_,	&
    mco1_,	&
    mco2_,	&
    mco3_
  end interface

  interface ptr_chars; module procedure	&
    ptr_chars_
  end interface

  character(len=*),parameter :: myname='m_String'

contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: str2ch0_ - convert a String to a variable of characters
!
! !DESCRIPTION:
!
! !INTERFACE:

    function str2ch0_(str,upper,lower)
      use m_chars,only : uppercase
      use m_chars,only : lowercase
      implicit none
      type(String),intent(in) :: str
      character(len=size(str%c)) :: str2ch0_
      logical,optional,intent(in) :: upper
      logical,optional,intent(in) :: lower

      		! chs=toChar(str)
		! chs=toChar(str,upper=.true.)
		! chs=toChar(str,lower=.true.)

! !REVISION HISTORY:
! 	23Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::str2ch0_'
  integer :: i
  logical :: lcase_,ucase_

  ucase_=.false.
  if(present(upper)) ucase_=upper
  lcase_=.false.
  if(present(lower)) lcase_=lower

  do i=1,size(str%c)
    str2ch0_(i:i)=str%c(i)
  end do

  if(ucase_.neqv.lcase_) then		! either
    if(ucase_) then			! ucase .and. .not. lcase_,
      str2ch0_ = uppercase(str2ch0_)	! or
    else				! lcase .and. .not. ucase_.
      str2ch0_ = lowercase(str2ch0_)
    endif
  endif
end function str2ch0_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ch12ch0_ - convert a rank-1 char-array to a rank-0 char(*)
!
! !DESCRIPTION:
!
! !INTERFACE:

    function ch12ch0_(ch1,upper,lower)
      use m_chars,only : uppercase
      use m_chars,only : lowercase
      implicit none
      character(len=1),dimension(:),intent(in) :: ch1
      character(len=size(ch1)) :: ch12ch0_
      logical,optional,intent(in) :: upper
      logical,optional,intent(in) :: lower

      		! chs=toChar(str)
		! chs=toChar(str,upper=.true.)
		! chs=toChar(str,lower=.true.)

! !REVISION HISTORY:
! 	22Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::ch12ch0_'
  integer :: i
  logical :: lcase_,ucase_

  ucase_=.false.
  if(present(upper)) ucase_=upper
  lcase_=.false.
  if(present(lower)) lcase_=lower

  do i=1,size(ch1)
    ch12ch0_(i:i)=ch1(i)
  end do

  if(ucase_.neqv.lcase_) then		! either
    if(ucase_) then			! ucase && !lcase_,
      ch12ch0_ = uppercase(ch12ch0_)	! or
    else				! lcase && !ucase_.
      ch12ch0_ = lowercase(ch12ch0_)
    endif
  endif
end function ch12ch0_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: initc_ - convert a character object to a String object
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine initc_(str,chr)
      use m_die, only : die,perr
      use m_mall,only : mall_mci,mall_ison
      implicit none
      type(String),intent(out) :: str
      character(len=*),intent(in) :: chr

! !REVISION HISTORY:
! 	23Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::initc_'
  integer :: ln,ier,i

  ln=len(chr)
  allocate(str%c(ln),stat=ier)
  if(ier /= 0) then
    call perr(myname_,'allocate()',ier)
    call die(myname_)
  endif

	if(mall_ison()) call mall_mci(str%c,myname)

  do i=1,ln
    str%c(i)=chr(i:i)
  end do

end subroutine initc_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: inits_ - convert a String object to a String
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine inits_(oStr,iStr)
      use m_die, only : die
      use m_mall,only : mall_mci,mall_ison
      implicit none
      type(String),intent(out) :: oStr
      type(String),intent(in ) :: iStr

! !REVISION HISTORY:
! 	07Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::inits_'
  integer :: ln,ier,i

  ln=size(iStr%c)

	allocate(oStr%c(ln),stat=ier)
		if(ier /= 0) call die(myname_,'allocate()',ier)

	if(mall_ison()) call mall_mci(oStr%c,myname)

  do i=1,ln
    oStr%c(i)=iStr%c(i)
  end do

end subroutine inits_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: clean_ - clean a defined String object
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine clean_(str)
      use m_die, only : die,perr
      use m_mall,only : mall_mco,mall_ison
      implicit none
      type(String),intent(inout) :: str

! !REVISION HISTORY:
! 	23Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::clean_'
  integer :: ier

	if(mall_ison()) call mall_mco(str%c,myname)

  deallocate(str%c,stat=ier)
  if(ier /= 0) then
    call perr(myname_,'deallocate()',ier)
    call die(myname_)
  endif

end subroutine clean_
#ifdef _TODO_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: s_eq_s_ - := (str==str)
!
! !DESCRIPTION:
!
! !INTERFACE:

    function s_eq_s_(s1,s2,adjustl,ignorecase,len)
      implicit none
      logical :: s_eq_s_
      type(String),intent(in) :: s1,s2
      logical,optional,intent(in) :: adjustl
      logical,optional,intent(in) :: ignorecase
      logical,optional,intent(in) :: len

! !REVISION HISTORY:
! 	27Dec05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::s_eq_s_'
  integer :: flag
  logical :: length_

	! " ab c " == " ab c"	! if no optional argument (right-trim)
	! " ab c " == "ab c"	! adjustl=.true. (left-right-trim)
	! " ab c " == " AB C "	! ignorecase=.true.
	! " ab c " == " ab c "	! len=.true.

  flag=0
  if(present(adjustl)) then
    if(adjustl) flag=ior(flag,1)
  endif
  if(present(ignorecase)) then
    if(ignorecase) flag=ior(flag,2)
  endif

  length_=.false.
  if(present(len)) length_=len

  if(length_) then	! check the length first,
    s_eq_s_ = String_len(s1)==String_len(s2)
    if(.not.s_eq_s_) return
	! otherwise (if String_len() is the same), carry on
  endif

  select case(flag)
  case(0)	! FF
    s_eq_s_ = toChar(s1) == toChar(s2)

  case(1)	! FT
    s_eq_s_ = adjustl(toChar(s1)) == adjustl(toChar(s2))

  case(2)	! TF
    s_eq_s_ = toChar(s1,upper=.true.) == toChar(s2,upper=.true.)

  case(3)	! TT
    s_eq_s_ = adjustl(toChar(s1,upper=.true.)) ==	&
  	      adjustl(toChar(s2,upper=.true.))
  end select

end function s_eq_s_
#endif
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: bcast_ - broadcast a rank-0 String
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine bcast_(Str,root,comm,stat)
      use m_mpif90
      use m_die, only : perr,die
      use m_mall,only : mall_mci,mall_ison
      implicit none
      type(String) :: Str	! (IN) on the root, (OUT) elsewhere
      integer,intent(in) :: root
      integer,intent(in) :: comm
      integer,optional,intent(out) :: stat

! !REVISION HISTORY:
! 	27Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::bcast_'
  integer :: ln,ier,myID

  if(present(stat)) stat=0

  call MP_comm_rank(comm,myID,ier)
  if(ier /= 0) then
    call MP_perr(myname_,'MP_comm_rank()',ier)
    if(.not.present(stat)) call die(myname_)
    stat=ier
    return
  endif

  if(myID==root) ln=size(Str%c)
  call MPI_bcast(ln,1,MP_INTEGER,root,comm,ier)
  if(ier/=0) then
    call MP_perr(myname_,'MPI_bcast(ln)',ier)
    if(.not.present(stat)) call die(myname_)
    stat=ier
    return
  endif

  if(myID /= root) then

    allocate(Str%c(ln),stat=ier)
    if(ier /= 0) then
      call perr(myname_,'allocate()',ier)
      if(.not.present(stat)) call die(myname_)
      stat=ier
      return
    endif

	if(mall_ison()) call mall_mci(Str%c,myname)
  endif

  call MPI_bcast(Str%c,ln,MP_CHARACTER,root,comm,ier)
  if(ier/=0) then
    call MP_perr(myname_,'MPI_bcast(Str%c)',ier)
    if(.not.present(stat)) call die(myname_)
    stat=ier
    return
  endif

end subroutine bcast_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: mci0_ - checking in a String scalar
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mci0_(marg,thread)
      use m_mall, only : mall_ci
      implicit none
      type(String),    intent(in) :: marg
      character(len=*),intent(in) :: thread

! !REVISION HISTORY:
! 	07Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::mci0_'

  call mall_ci(1,thread)

end subroutine mci0_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: mco0_ - checking out a String scalar
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mco0_(marg,thread)
      use m_mall, only : mall_co
      implicit none
      type(String),    intent(in) :: marg
      character(len=*),intent(in) :: thread

! !REVISION HISTORY:
! 	07Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::mco0_'

  call mall_co(1,thread)

end subroutine mco0_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: mci1_ - checking in a String scalar
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mci1_(marg,thread)
      use m_mall, only : mall_ci
      implicit none
      type(String),dimension(:),intent(in) :: marg
      character(len=*)         ,intent(in) :: thread

! !REVISION HISTORY:
! 	07Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::mci1_'

  call mall_ci(size(marg),thread)

end subroutine mci1_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: mco1_ - checking out a String scalar
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mco1_(marg,thread)
      use m_mall, only : mall_co
      implicit none
      type(String),dimension(:),intent(in) :: marg
      character(len=*)         ,intent(in) :: thread

! !REVISION HISTORY:
! 	07Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::mco1_'

  call mall_co(size(marg),thread)

end subroutine mco1_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: mci2_ - checking in a String scalar
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mci2_(marg,thread)
      use m_mall, only : mall_ci
      implicit none
      type(String),dimension(:,:),intent(in) :: marg
      character(len=*)           ,intent(in) :: thread

! !REVISION HISTORY:
! 	07Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::mci2_'

  call mall_ci(size(marg),thread)

end subroutine mci2_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: mco2_ - checking out a String scalar
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mco2_(marg,thread)
      use m_mall, only : mall_co
      implicit none
      type(String),dimension(:,:),intent(in) :: marg
      character(len=*)           ,intent(in) :: thread

! !REVISION HISTORY:
! 	07Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::mco2_'

  call mall_co(size(marg),thread)

end subroutine mco2_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: mci3_ - checking in a String scalar
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mci3_(marg,thread)
      use m_mall, only : mall_ci
      implicit none
      type(String),dimension(:,:,:),intent(in) :: marg
      character(len=*)             ,intent(in) :: thread

! !REVISION HISTORY:
! 	07Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::mci3_'

  call mall_ci(size(marg),thread)

end subroutine mci3_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: mco3_ - checking out a String scalar
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mco3_(marg,thread)
      use m_mall, only : mall_co
      implicit none
      type(String),dimension(:,:,:),intent(in) :: marg
      character(len=*)             ,intent(in) :: thread

! !REVISION HISTORY:
! 	07Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::mco3_'

  call mall_co(size(marg),thread)

end subroutine mco3_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: len_ = len of a String
!
! !DESCRIPTION:
!
! !INTERFACE:

    function len_(str)
      implicit none
      type(String),intent(in) :: str
      integer :: len_

! !REVISION HISTORY:
! 	10Apr00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::len_'

  len_=size(str%c)

end function len_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ptr_chars_ - direct
!
! !DESCRIPTION:
!
! !INTERFACE:

    function ptr_chars_(str)
      implicit none
      type(String),intent(in) :: str
      character(len=1),pointer,dimension(:) :: ptr_chars_

! !REVISION HISTORY:
! 	10Apr00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::ptr_chars_'

  ptr_chars_ => str%c

end function ptr_chars_
end module m_String
