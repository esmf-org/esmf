!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_datetime - simple algebra of callendar date and clock time
!
! !DESCRIPTION:
!       Perform simple algebra of (date,time), where, "date" is an
!   unsigned integer which in decimal represents a combined information
!   of (year, month, day) in yyyymmdd;  and "time" is an unsigned/signed
!   integer which in decimal represents a combined triple information of
!   (hour, minute, second) in hhhhmmss.
!
! !INTERFACE:

    module m_datetime
      use m_date,only : validdate => valid
      use m_date,only : date,resdate
      implicit none
      private	! except

        ! basic alrithmatic operations
      public :: datetime_norm ! (io:date,io:time)
                              ! -- normalize (date,time)
      public :: datetime_add  ! (io:date,io:time,i:tinc[,i:mult])
                              ! -- (date,time) += mult * tinc
      public :: datetime_diff ! (i:dt1,tm1,dt2,tm2,o:td[,i:di,o:mt])
                              ! -- td [+ mt*di] = (dt2,tm2)-(dt1,tm1)
      public :: datetime_div  ! (i:time,tinc[,o:mult,tmod])
                              ! -- tmod + mult*tinc = time

        ! relational operations
      public :: datetime_eq,eq ! []_same_as
      public :: datetime_ne,ne ! []_diff_from
      public :: datetime_lt,lt ! []_earlier_than
      public :: datetime_le,le ! []_no_later_than
      public :: datetime_gt,gt ! []_later_than, la
      public :: datetime_ge,ge ! []_no_earlier_than

        ! verifications
      public :: valid      ! f.(=i:date,=i:time) -- validate (date,time)
      public :: validdate  ! f.(i:date) -- validate a date
      public :: validtime  ! f.(i:time) -- validate a time

        ! convertions
      public :: date,resdate  ! (y,m,d) to/from date in yyyymmdd
      public :: time,restime  ! (h,m,s[,d]) to/from time in hhhhmmss
      public :: todatetime    ! convert (y,m,d,h,m,s) to (date,time)
                ! call todatetime(yr,mo,dy,hr,mi,sc,date,time)
      public :: resdatetime   ! convert (date,time) to (y,m,d,h,m,s)
                ! call resdatetime(date,time,yr,mo,dy,hr,mi,sc)

      interface datetime_norm  ; module procedure &
        normal_  ; end interface
      interface datetime_add   ; module procedure &
        multadd_ ; end interface
      interface datetime_diff   ; module procedure &
        diff_ ; end interface
      interface datetime_div   ; module procedure &
        div_ ; end interface

      interface datetime_eq; module procedure eq_; end interface
      interface datetime_ne; module procedure ne_; end interface
      interface datetime_lt; module procedure lt_; end interface
      interface datetime_le; module procedure le_; end interface
      interface datetime_gt; module procedure gt_; end interface
      interface datetime_ge; module procedure ge_; end interface
      interface eq; module procedure eq_; end interface
      interface ne; module procedure ne_; end interface
      interface lt; module procedure lt_; end interface
      interface le; module procedure le_; end interface
      interface gt; module procedure gt_; end interface
      interface ge; module procedure ge_; end interface

      interface valid    ; module procedure valid_    ; end interface
      interface validtime; module procedure validtime_; end interface

      interface todatetime; module procedure &
        todatetime_; end interface  ! set date-time from (y,m,d,h,m,s)
      interface resdatetime; module procedure &
        resdatetime_; end interface ! resolve date-time to (y,m,d,h,m,s)
      interface time   ; module procedure &
        time_, &                    ! set HMS time from (h,m,s)
	times_   ; end interface    ! set HMS time from seconds
      interface restime; module procedure &
        restime_, &                 ! resolve HMS time to (h,m,s)
	restimes_; end interface    ! resolve HMS time to seconds

! !REVISION HISTORY:
! 	04Apr06	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_datetime'

! Usecase 1:  add tinc to (date,time)
!     integer :: date=19991231 ! Dec. 31, 1999
!     integer :: time=  060000 ! 06 Z
!     integer :: tinc=  003000 ! 30 min. increment
!     call datetime_add(date,time,tinc) ! result (19991231,063000)
! 
! Usecase 2:  add m*tinc to (date,time)
!     integer :: date=19991231 ! Dec. 31, 1999
!     integer :: time=  060000 ! 06 Z
!     integer :: tinc=  003000 ! 30 min. increment
!     call datetime_add(date,time,tinc,mult=-6) ! -> (19991231,030000)
!
! Usecase 3: verify validity of a given (d,t)
!     integer :: d=19991231, t=060000
!     if(.not.valid(date=d,time=t)) call die(myname)
!
! Usecase 4: normalize a given (d,t)
!     integer :: d=19991231, t=-4560000 ! t is 19 days in hours
!     call datetime_norm(d,t) ! -> (19991212,000000)

contains
!-----------------------------------------------------------------------
subroutine todatetime_(yr,mo,dy,hr,mi,sc,date,time)
  use m_date,only : validdate => valid
  use m_date,only : date_ => date
  implicit none
  integer,intent(in) :: yr,mo,dy,hr,mi,sc
  integer,intent(out) :: date,time
  date=-1;time=0
  if(.not.validdate(yr,mo,dy)) return
  if(.not.validtimei_(hr,mi,sc)) return
  date=date_(yr,mo,dy)
  time=time_(hr,mi,sc)
  call normal_(date,time)
  end subroutine todatetime_
!-----------------------------------------------------------------------
subroutine resdatetime_(date,time,yr,mo,dy,hr,mi,sc)
! -- resolve a given (date,time), "normalize", resolve to (y,m,d,h,m,s)
  use m_date,only : resdate
  implicit none
  integer,intent(in) :: date,time ! YMD date, HMS time
  integer,intent(out) :: yr,mo,dy,hr,mi,sc ! year,mon,day,hour,min,sec
  integer :: d,t
  d=date;t=time      ! make a copy for "normalization"
  call normal_(d,t)  ! carry extra days in time up to date
  yr=-1;mo=0;dy=0; hr= 0;mi=0;sc=0 ! in case the input is not valid
  if(d<0) return ! flag a bad input
  call resdate(d,yr,mo,dy)
  call restime_(t,hr,mi,sc)
  end subroutine resdatetime_
!-----------------------------------------------------------------------
function valid_(date,time) ! -- validate (date,time)
  use m_date,only : validdate => valid
  implicit none
  integer,optional,intent(in) :: date,time ! yyyymmdd,hhhhmmss
  logical :: valid_
  integer :: d,t
  valid_=present(date) .or. present(time)
  if(valid_) then
    if(present(date)) valid_= validdate(date)
    if(present(time)) valid_= valid_ .and. validtime_(time)
  endif
end function valid_
!-----------------------------------------------------------------------
function validtime_(time) ! -- validate time
  implicit none
  integer,intent(in) :: time
  logical :: validtime_
  integer :: h,m,s
  h=time
  s=mod(h,100)
  h=    h/100
  m=mod(h,100)
  validtime_= abs(m) <= 59 .and. abs(s) <= 59 ! any h is fine.
end function validtime_
!-----------------------------------------------------------------------
function validtimei_(hr,mi,sc) ! -- validate time in (h,m,s)
  implicit none
  integer,intent(in) :: hr,mi,sc
  logical :: validtimei_
  validtimei_= abs(mi) <= 59 .and. abs(sc) <= 59 ! any hr is fine.
end function validtimei_

!! Date + date is not a well defined concept.  When adding 1 month
!! to January, does the user intend to add one January month (which is
!! 31 days) or one Febrary month (which is 28 days)?  This ambuguity
!! becomes clear if one tries to add 1-month 3-days to January 31th.
!! The result of adding the 3 days first (ends at March 3rd) is total
!! different from the result of adding the 1 month first (ends at March
!! 6th).  (Leap year February is not considered in the example).

!-----------------------------------------------------------------------
subroutine normal_(date,time)
! -- algebra of (date,time) := (yyyymmdd,hhmmss)
  use m_date, only : resdate,datej => date
  use m_date, only : validdate => valid
  implicit none
  integer,intent(inout) :: date ! in yyyymmdd
  integer,intent(inout) :: time ! in +-hhmmss
  integer :: jd
  integer :: y,m,d,h,s

    ! Nothing can be done if (date,time) is invalid to begin with, such
    ! as, d=19993030 or t=016299.  When the situation occurs, date=-1 is
    ! returned to flag the error condition.

  if(.not.valid_(date,time)) then
    date=-1
    return
  endif

    ! time to (d,h,m,s).  If time is signed, (d,h,m,s) is signed.
  call restime_(time,h,m,s,d=d)
    ! Since time, thus (d,h,m,s), is signed, "unsign" (h,m,s) first.
  if(time<0) then ! (d,h,m,s) may <=0
    if(h<0.or.m<0.or.s<0) then
      d=d-1  ! d is still signed.
      h=h+24 ! unsign h, if (h,m,s) /= 0
      if(m<0.or.s<0) then
        h=h-1
	m=m+60 ! unsign m, if (m,s) /= 0
	if(s<0) then
	  m=m-1
	  s=s+60 ! unsign s, if s /= 0
	endif
      endif
    endif
  endif
  time=s+100*(m+100*h) ! this time is unsigned

    ! do addition in Julian days 
  call resdate(date,jd)
  jd=jd+d
  date=datej(jd)
end subroutine normal_
!-----------------------------------------------------------------------
function time_(h,m,s,d)
  implicit none
  integer,intent(in) :: h,m,s
  integer,optional,intent(in) :: d
  integer :: time_
  integer :: ss,mm,hh
  mm= m + s/60
  ss= mod(s,60)
  hh= h + mm/60
  mm= mod(mm,60)
  if(present(d)) hh=hh+d*24
  time_=ss+100*(mm+100*hh)
  end function time_
!-----------------------------------------------------------------------
function times_(seconds)
  implicit none
  integer,intent(in) :: seconds
  integer :: times_
  integer :: s,m,h
  h=seconds
  s=mod(h,60)
  h=    h/60
  m=mod(h,60)
  h=    h/60
  times_=s+100*(m+100*h)
  end function times_
!-----------------------------------------------------------------------
subroutine restime_(time,h,m,s,d)
  implicit none
  integer,intent(in) :: time      ! time is signed, thus
  integer,intent(out) :: h,m,s    ! (h,m,s) is signed, or
  integer,optional,intent(out) :: d ! (d,h,m,s) is signed

  h=time      ! hhhhmmss
  s=mod(h,100)
  h=    h/100 ! hhhhmm
  m=mod(h,100)
  h=    h/100 ! hhhh

    ! although it is possible that m or s may be output of their
    ! valid range [-59:+59], I don't know what is the best way to
    ! let the user to know.

  if(present(d)) then
    d=h
    h=mod(d,24)
    d=    d/24  ! d=hhhh/24
  endif
  end subroutine restime_
!-----------------------------------------------------------------------
subroutine restimes_(time,seconds)
  implicit none
  integer,intent(in) :: time     ! time in signed HMS
  integer,intent(out) :: seconds ! time in seconds
  integer :: h,m,s
  call restime_(time,h,m,s)
  seconds=s+60*(m+60*h)
  end subroutine restimes_
!-----------------------------------------------------------------------
subroutine multadd_(date,time,tinc,mult)
  implicit none
  integer,intent(inout) :: date ! YMD date
  integer,intent(inout) :: time ! signed HMS time
  integer,intent(in   ) :: tinc ! signed HMS time
  integer,optional,intent(in) :: mult ! multiple of tinc
  integer :: s,x

  call restimes_(time,s)        ! resolve signed HMS time to seconds
  call restimes_(tinc,x)        ! resolve signed HMS interval to seconds
  if(present(mult)) x=mult*x    ! multiply x if mult is given
  s=s+x                         ! compute the new time in seconds
  time=times_(s)                ! construct signed time in HMS
  call normal_(date,time)       ! normalize the new (date,time)
  end subroutine multadd_
!-----------------------------------------------------------------------
subroutine diff_(date1,time1,date2,time2,tdiff, div,mult)
  !! dt = (date2,time2)-(date1,time1)
  !! It is assumed that the expected difference can be expressed in a
  !! 4 byte integer, which is approximately within +-24 years.
  !!   (2^31-1)/100/100/24/366 == 24
  use m_date,only : resdate
  implicit none
  integer,intent(in) :: date2 ! YMD date
  integer,intent(in) :: time2 ! signed HMS time
  integer,intent(in) :: date1 ! YMD date
  integer,intent(in) :: time1 ! signed HMS time
  integer,intent(out) :: tdiff ! dt or mod(dt,div) in signed HMS
  integer,optional,intent(in) :: div ! /=0 is expected
  integer,optional,intent(out) :: mult ! dt == tdiff+mult*div

  integer :: d,s,x

    ! compute j=date2-date1 in days then in seconds
  call resdate(date2,d)
  call resdate(date1,x)
  d=d-x ! date only difference in days

    ! compute (j,t)=time2-time1 in (days, seconds)
  call restimes_(time2,s)
  call restimes_(time1,x)
  s=s-x ! time only difference in seconds
  s=s+d*24*60*60 ! total time difference in seconds

  if(present(div)) then
    call restimes_(div,x) ! interval _div_ in seconds
    if(present(mult)) mult=s/x
    s=mod(s,x) ! s is now mod(dt,div)
  else
    if(present(mult)) mult=0
  endif
    ! compute HMS time difference from seconds
  tdiff=times_(s)
  end subroutine diff_
!-----------------------------------------------------------------------
subroutine div_(time,tinc,mult,tmod)
  !! tmod+mult*tinc := time
! !REVISION HISTORY:
! 	19May06	- Todling - time turned to intent(in)
!EOP ___________________________________________________________________
  implicit none
  integer,intent(in) :: time ! time in signed HMS
  integer,intent(in) :: tinc ! time interval in HMS, /=0.
  integer,optional,intent(out) :: mult ! in 1
  integer,optional,intent(out) :: tmod ! in signed HMS

  integer :: s,x
  if(.not.(present(mult).and.present(tmod))) return
  call restimes_(time,s) ! convert time to seconds
  call restimes_(tinc,x) ! convert tinc to seconds
  if(present(mult)) mult=s/x            ! compute mult
  if(present(tmod)) tmod=times_(mod(s,x)) ! compute tmod from seconds
  end subroutine div_
!-----------------------------------------------------------------------
! Below are relational operations

function eq_(date1,time1,date2,time2)
  implicit none
  integer,intent(in) :: date1,time1,date2,time2
  logical :: eq_
  integer :: d1,t1,d2,t2
  d1=date1;t1=time1
  call normal_(d1,t1)
  d2=date2;t2=time2
  call normal_(d2,t2)
  eq_=(d1==d2).and.(t1==t2)
  end function eq_

function ne_(date1,time1,date2,time2)
  implicit none
  integer,intent(in) :: date1,time1,date2,time2
  logical :: ne_
  integer :: d1,t1,d2,t2
  d1=date1;t1=time1
  call normal_(d1,t1)
  d2=date2;t2=time2
  call normal_(d2,t2)
  ne_=(d1/=d2).or.(t1/=t2)
  end function ne_

function lt_(date1,time1,date2,time2)
  implicit none
  integer,intent(in) :: date1,time1,date2,time2
  logical :: lt_
  integer :: d1,t1,d2,t2
  d1=date1;t1=time1
  call normal_(d1,t1)
  d2=date2;t2=time2
  call normal_(d2,t2)
  lt_=(d1<d2).or.(d1==d2.and.t1<t2)
  end function lt_

function le_(date1,time1,date2,time2)
  implicit none
  integer,intent(in) :: date1,time1,date2,time2
  logical :: le_
  integer :: d1,t1,d2,t2
  d1=date1;t1=time1
  call normal_(d1,t1)
  d2=date2;t2=time2
  call normal_(d2,t2)
  le_=(d1<d2).or.(d1==d2.and.t1<=t2)
  end function le_

function gt_(date1,time1,date2,time2)
  implicit none
  integer,intent(in) :: date1,time1,date2,time2
  logical :: gt_
  integer :: d1,t1,d2,t2
  d1=date1;t1=time1
  call normal_(d1,t1)
  d2=date2;t2=time2
  call normal_(d2,t2)
  gt_=(d1>d2).or.(d1==d2.and.t1>t2)
  end function gt_

function ge_(date1,time1,date2,time2)
  implicit none
  integer,intent(in) :: date1,time1,date2,time2
  logical :: ge_
  integer :: d1,t1,d2,t2
  d1=date1;t1=time1
  call normal_(d1,t1)
  d2=date2;t2=time2
  call normal_(d2,t2)
  ge_=(d1>d2).or.(d1==d2.and.t1>=t2)
  end function ge_

!!! Design concerns:
!!!
!!!   Above ralational operations are implemented in function forms.
!!! Should they also be implemented in operator forms?  Below are three
!!! examples of applications in the two forms.
!!!
!!! 1. if(datetime_ne(19590101,+030000, 19590102,-210000)) exit
!!! 2. if(datetime(19590101,+030000)/=datetime(19590102,-210000)) exit
!!! 3. if((/19590101,+030000/).is.(/19590102,-210000/)) exit
!!!
!!!   Note that although the last form seems to be feasible, one has to
!!! find another name for the operator since it is not possible to
!!! distinguish (/d,t/) == (/d,t/) used as a (/date,time/) from used as
!!! a comparison between two regular arrays.  Since it is not easy to
!!! define sensible and easy-to-use symbols for these relations, form 3
!!! has to be dropped.  OTOH, form 2 seems too long comparing to form 1
!!! for typical usages.  Finally, form 1 is chosen and implemented here.

end module m_datetime
