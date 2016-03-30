!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!
! !MODULE: m_date - convert between Julian-days/(y,m,d)/yyyymmdd-date
!
! !DESCRIPTION:
!    Julian days defined in thiss module is the day count since Sep.
!   14, 1752 (the first day of the Gregorian Reformation).
!
! !INTERFACE:

    module m_date
      implicit none
      private	! except

      public :: YZ,MZ,DZ     ! parameters, when jday(yz,mz,dz) == 0
      public :: jday,resjday ! Julian days since Sep. 14, 1752 and back
        interface jDay; module procedure &
	  jday_; end interface    ! =jday(y,m,d)
        interface resjday; module procedure &
	  resjday_; end interface ! call resjday(j, y,m,d)

      public :: dofm         ! days of the month
        interface dofm; module procedure &
	  DofM_; end interface    ! =dofm(y,m)

      public :: valid        ! check validity of a (date) or a (y,m[,d])
        interface valid; module procedure &
          valid_, &               ! =valid(date)
	  validi_; end interface  ! =valid(y,m[,d])

      public :: date,resdate ! (y,m,d) or jd to YMD date and back
      interface date; module procedure &
        date_, &                  ! =date(y,m,d)
	datej_; end interface     ! =date(j)
      interface resdate; module procedure &
        resdate_, &               ! call resdate(date,y,m,d)
	resdatej_; end interface  ! call resdate(date,j)

! !EXAMPLES: (to do)
! !BUGS: (to do)
! !SEE ALSO: (to do)
! !SYSTEM ROUTINES: (to do)
!
! !REVISION HISTORY:
!   18Apr06 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!           - reimplemented with extentions
!   05Sep97 - Jing Guo <guo@gmao> - initial prototyping and coding
!_______________________________________________________________________

  integer,parameter :: Jan=01,Feb=02,Mar=03,Apr=04,May=05,Jun=06, &
		       Jul=07,Aug=08,Sep=09,Oct=10,Nov=11,Dec=12

  integer,parameter :: YZ=1752
  integer,parameter :: MZ=Sep
  integer,parameter :: DZ=14
  integer,save      :: JZ=-1     ! saved jdayi_(YZ,MZ,DZ)
  integer,save      :: J9=-1     ! saved jdayi_(9999,12,31) - JZ
!=======================================================================
contains
!-----------------------------------------------------------------------
function DofM_(y,m) ! -- days of the month with (y,m) validation
  implicit none
  integer, intent(in) :: y	! the year
  integer, intent(in) :: m	! the month in the year
  integer		:: DofM_	! the result

  DofM_=-1
  if(validi_(y,m)) DofM_=dofmi_(y,m)
  end function DofM_
!-----------------------------------------------------------------------
function valid_(date) ! -- validate a YMD.  Y2K is implied
  implicit none
  integer,intent(in) :: date ! in YMD
  logical :: valid_

  integer :: y,m,d ! local variables
  y=date       ! resolve YMD date to (y,m,d)
  d=mod(y,100)
  y=    y/100
  m=mod(y,100)
  y=    y/100
  valid_=validi_(y,m,d)
  end function valid_
!-----------------------------------------------------------------------
function date_(y,m,d) ! -- form a YMD date from (y,m,d)
  implicit none
  integer,intent(in) :: y,m,d
  integer :: date_
  date_=-1
  if(validi_(y,m,d)) date_= d+100*(m+100*y) ! construct date
  end function date_
!-----------------------------------------------------------------------
function datej_(jdays) ! -- form a YMD from a day count since 1752
  implicit none
  integer,intent(in) :: jdays
  integer :: datej_
  integer :: y,m,d
  call resjday_(jdays,y,m,d)  ! resolve jdays to (y,m,d)
  datej_ = date_(y,m,d)       ! construct (y,m,d) to YMD with validation
  end function datej_
!-----------------------------------------------------------------------
subroutine resdate_(date,y,m,d) ! -- resolve a YMD to (y,m,d)
  implicit none
  integer,intent(in) :: date
  integer,intent(out) :: y,m,d

  y=date
  d=mod(y,100)
  y=    y/100
  m=mod(y,100)
  y=    y/100
  if(validi_(y,m,d)) return ! that is it.
  y=-1 ! otherwise, results are flagged as invalid (y,m,d)
  m=0
  d=0
  end subroutine resdate_
!-----------------------------------------------------------------------
subroutine resdatej_(date,jdays) ! -- resolve to a day count since 1752
  implicit none
  integer,intent(in) :: date
  integer,intent(out) :: jdays
  integer :: y,m,d
  call resdate_(date,y,m,d)
  jdays=jday_(y,m,d)
  end subroutine resdatej_
!-----------------------------------------------------------------------
function jday_(y,m,d)
    implicit none
    integer,intent(in) :: y	! the year
    integer,intent(in) :: m	! the month in the year
    integer,intent(in) :: d	! the day in the month
    integer :: jday_		! the result
    integer :: md

    jday_ = -1
    if(.not.validi_(y,m,d)) return

    if(JZ<0) JZ=jdayi_(YZ,MZ,DZ)
    if(J9<0) J9=jdayi_(9999,12,31) - JZ

    jday_ = jdayi_(y,m,d) - JZ
  end function jday_
!-----------------------------------------------------------------------
subroutine resjday_(jd,y,m,d) ! -- jdays to (y,m,d) without validation
    implicit none
    integer, intent(in) :: jd ! Julian days (1752)
    integer, intent(out) :: y,m,d ! output as (year, month, day)

    integer :: dx
    integer :: m5,m2,m1
    integer :: y1,y4,y100,y400

    if(JZ<0) JZ=jdayi_(YZ,MZ,DZ)
    if(J9<0) J9=jdayi_(9999,12,31) -JZ

    call resjdayi_(jd+JZ,y,m,d)
    if(validi_(y,m,d)) return
    y=-1
    m=0
    d=0
  end subroutine resjday_

!=======================================================================
function validi_(y,m,d) ! -- validate (y,m[,d]).  (Y2k)
  implicit none
  integer,intent(in) :: y,m
  integer,optional,intent(in) :: d
  logical :: validi_
  integer :: md
  validi_ = YZ<=y .and. y <=9999 ! check y
  if(.not.validi_) return
  select case(y)                 ! check m of y
  case (YZ)
    validi_ = MZ <=m .and. m<=Dec
  case default
    validi_ = Jan<=m .and. m<= Dec
  end select
  if(.not.validi_ .or. .not.present(d)) return
  md=dofmi_(y,m)                  ! check d of (y,m)
  validi_ = 1<=d .and. d<=md
  end function validi_
!-----------------------------------------------------------------------
function dofmi_(y,m) ! -- days of the month without validating (y,m)
  implicit none
  integer, intent(in) :: y	! the year
  integer, intent(in) :: m	! the month in the year
  integer		:: dofmi_	! the result

	! Local variable
    integer :: md

    dofmi_=-1
    select case(m)
    case(Feb)
      md=28
      if(mod(y,4)   == 0) md=md+1
      if(mod(y,100) == 0) md=md-1
      if(mod(y,400) == 0) md=md+1
    case(Apr,Jun,Sep,Nov)
      md=30
    case(Jan,Mar,May,Jul,Aug,Oct,Dec)
      md=31
    case default
      md=-1
    end select
    dofmi_=md
  end function dofmi_
!-----------------------------------------------------------------------
function jdayi_(y,m,d) ! -- compute jday without validating (y,m,d)
    implicit none
    integer,intent(in) :: y	! the year
    integer,intent(in) :: m	! the month in the year
    integer,intent(in) :: d	! the day in the month
    integer :: jdayi_		! the result

    integer :: yr
    integer :: mr,m5,m2,m1
    integer :: jd
    integer :: md

	! Count the days of the month, starting at 0

    jd=d-1

	! Starting from March 1, there is a two-month period, and
	! there is a five-month period.  61 days for every 2-months,
	! and 153 days for every 5-month period.

    			! [J,F,M,A,M,J,J,A,S,O,N,D]
			! [a,e,a,p,a,u,u,u,e,c,o,e]
			! [n,b,r,r,y,n,l,g,p,t,v,c]

    mr=mod(m+9,12)	! [a,b,0,1,2,3,4,5,6,7,8,9]
    m5=mr/5		! [2,2,0,0,0,0,0,1,1,1,1,1]
    mr=mod(mr,5)	! [0,1,0,1,2,3,4,0,1,2,3,4]
    m2=mr/2		! [0,0,0,0,1,1,2,0,0,1,1,2]
    m1=mod(mr,2)	! [0,1,0,1,0,1,0,0,1,0,1,0]

	! Count the days in the past months, starting from Mar. 1.

    jd=jd+m5*153 + m2*61 + m1*31

	! Shift the beginning of a year to the first day of March and
	! define new indices of "year" and "month".

    yr=y		! Leap year correction only applicable to the
    if(m<3) yr=yr-1	! monthes after the leap actually takes place.

    jdayi_=jd+yr*365+yr/4-yr/100+yr/400

  end function jdayi_
!-----------------------------------------------------------------------
subroutine resjdayi_(jd,y,m,d) ! -- jdays to (y,m,d) without validation
    implicit none
    integer, intent(in) :: jd ! Julian days (1752)
    integer, intent(out) :: y,m,d ! output as (year, month, day)

    integer :: dx
    integer :: m5,m2,m1
    integer :: y1,y4,y100,y400

    dx=jd
    y400=dx/146097	! count years by every 400 years
    dx=dx-y400*146097	! days left after 400 year counts

    y100=dx/36524	! count years by every 100 years
        ! y100 could be [0..4], since 36524*4+1==146097.  Make 4 to 3
    if(y100 == 4) y100=3
    dx=dx-y100*36524	! days left after 100 year counts

    y4=dx/1461		! count years by every 4 years
    dx=dx-y4*1461	! days left after 4 year counts

    y1=dx/365		! count years by every 1 years
        ! y100 could be [0..4], since 365*4+1==1461.  Make 4 to 3
    if(y1 == 4) y1=3
    dx=dx-y1*365	! days left after 1 year counts

    y=y1+y4*4+y100*100+y400*400

    m5=dx/153		! count months since Mar. 1, by every 5 months
    dx=dx-m5*153	! days left after 5-month counts

    m2=dx/61		! count months by every 2 months
    dx=dx-m2*61		! days left after 2-month counts

    m1=dx/31		! count months by every months
    dx=dx-m1*31		! days left after 1-month counts

    m=m1+m2*2+m5*5	! total months since March 1., [10,11,0..9]

    d=dx+1		! adjust the day in the month
    m=mod(m+2,12)+1	! adjust the month in the year
    if(m<3) y=y+1	! adjust the year

  end subroutine resjdayi_
end module m_date
!.
