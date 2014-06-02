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
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: m_zeit - a multi-timer of process times and wall-clock times
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_zeit
      implicit none
      private	! except

      public :: zeit_ci		! push a new name to the timer
      public :: zeit_co		! pop the current name on the timer
      public :: zeit_flush	! print per PE timing
      public :: zeit_allflush	! print all PE timing
      public :: zeit_reset	! reset the timers to its initial state

	! Flags of all printable timers

      public ::  MWTIME	! MPI_Wtime() wall-clock time
      public ::  XWTIME	! times() wall-clock time
      public ::  PUTIME	! times() process user time
      public ::  PSTIME	! times() process system time
      public ::  CUTIME	! times() user time of all child-processes
      public ::  CSTIME	! times() system time of all child-processes
      public :: ALLTIME	! all of above
      public ::  UWRATE ! (putime+cutime)/xwtime

      interface zeit_ci;    module procedure ci_;    end interface
      interface zeit_co;    module procedure co_;    end interface
      interface zeit_flush; module procedure flush_; end interface
      interface zeit_allflush; module procedure allflush_; end interface
      interface zeit_reset; module procedure reset_; end interface

! !REVISION HISTORY:
!
!	20Dec2005 - Jing Guo <jguo@gmao.gsfc.nasa.gov>
!		  - Merged 1.1.2.12-1.1.2.9 to 1.4:
!		  . added optional argument mytime to reset_(), ci_(),
!		    co_(), flush_(), and allflush_(), which is similar
!		    to the scalar user-provided time usrtime, but an
!		    array with all timer values.
!		  . added optional argument subname_to_end to flush_()
!		    and allflush_() to put timer names at the end of the
!		    output table.
!
! 	05Mar98 - Jing Guo <guo@thunder>	-
!		. rewritten for possible MPI applications, with
!		  additional functionalities and new performance
!		  analysis information.
!		. Interface names have been redefined to ensure all
!		  use cases to be verified.
!		. removed the type(pzeit) data structure, therefore,
!		  limited to single _instance_ applications.
!		. added additional data components for more detailed
!		  timing analysis.
!		. used times() for the XPG4 standard conforming
!		  timing functions.
!		. used MPI_Wtime() for the MPI standard conforming
!		  high-resolution timing functions.
!
! 	20Feb97 - Jing Guo <guo@eramus>		-
!		. rewritten in Fortran 90 as the first modular
!		  version, with a type(pzeit) data structure.
!
!	10may96 - Jing G. -	Add _TZEITS macro for the testing code
!	09may96 - Jing G. -	Changed output format also modifed
!				comments
!	11Oct95 - Jing G. -	Removed earlier way of letting clock
!				timing (clkknt and clktot) to be no less
!				then the CPU timing, following a
!				suggestion by James Abeles from Cray.
!				This way, users may use the routings to
!				timing multitasking speedup as well.
!	12May95	- Jing G. -	Merged zeitCRAY.f and zeitIRIS.f.
!	Before	- ?	  -	See zeitCRAY.f and zeitIRIS.f for more
!				information.  Authors of those files are
!				not known to me.
!
! !DESIGN ISSUES:
!
!	05Mar98	- Jing Guo <guo@thunder>	-
!		. Removing the data structure may be consider as a 
!		  limitation to future changes to multiple _instance_
!		  applications.  However, it is unlikely there will be
!		  any neccessary multi-_intance_ application soon, if
!		  ever for this module.
!		. Without an additional layer with the derived
!		  datatype, one may worry less the tricky performance
!		  issues associated with ci_/co_.
!		. Performance issue with the flush_() calls are not
!		  considered.
!
!	20Feb97	- Jing Guo <guo@eramus>		-
!		. Currently a single threaded module.  May be easily
!		  extended to multi-threaded module by adding the name
!		  of an instance of the class to the argument list.  It
!		  requires some but very limited interface extensions.
!		  Right now, the backward compatibility is the main
!		  issue.
!
! 10may96 - Jing Guo <guo@eramus>		-
!
!     + This `zeit' subroutine collection replaces original zeit files
!	used in PSAS on both systems, UNICOS and IRIX, with following
!	changes:
!
!	      +	Removed the some bugs in zeitCRAY.f that ovewrite the
!		first user defined name entry in a special situation
!		(but not being able to correct in zeitCRAY.f).
!
!	      + Unified both zeitCRAY.f and zeitIRIS.f in to one file
!		(this file), that handles system dependency in only
!		one subroutine syszeit_() with a couple of lines of
!		differences.
!
!	      + Added system CPU time counts for system supporting
!		the function.
!
!	      + Added some error checking and reporting functions.
!
!	      + According to zeitCRAY.f, "zeit" is "time" in German.
!		The name is used through the code as another name for
!		"time".
!
!	      + This version does not work for parallelized processes.
!
!     + Elapsed time records since the first call are used.  Although
!	it may loose accuracy when the values of the time records
!	become large, it will keep the total time values conserved.
!
!     +	The accuracy of the elapsed times at a IEEE real*4 accuracy
!	(ffrac = 2^23 ~= 1.19e-7) should be no worse than +- 1 second
!	in 97 days, if only the numerical accuracy is considered.
!
!     +	The precision of "wall clock" time returned by syszeit_() is
!	only required to be reliable upto seconds.
!
!     +	The "wall clock" time for individual `name' (clkknt) is
!	accumulated by adding the differences between two integer
!	values, iclk and iclksv.  Care must be taken to compute the
!	differences of iclk and iclksv first.  That is, doing
!
!		clkknt()=clkknt() + (iclk-iclksv)
!
!	not
!
!		clkknt()=clkknt() + iclk-iclksv
!
!	The latter statement may ignore the difference between the two
!	integer values (iclk and iclksv).
!
! 28may02 - Thomas King <tking@dao.gsfc.nasa.gov>               -
!
!     + To use this module for script-level timing a separate set
!       of programs were written to operate with m_zeit.  These
!       programs write check in/out time stamps to a file.  Another
!       program in this software package then reads this file and sends
!       the in/out times to m_zeit.  Therefore, the call argument lists
!       for zeit_ci, zeit_co, zeit_flush, and zeit_reset in m_zeit.F90
!       were extended to include the use of this externally determined
!       wallclock time.  Accordingly, these same routines were modified
!       slightly to then use only the externally defined time and NOT the
!       current wallclock time that would normally be returned by m_zeit's
!       get_zeits routine.
!     + Additional columns were added to the output format statements in
!       the sp_balances routine so reports could be generated for the
!       timing of long-running programs that may require several days to
!       complete.
!
! 07jun02 - Todling  - merged changes of T King (1_3beta15) w/ rdas_5c
!
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname='m_zeit'

  integer,parameter ::  MWTIME =  1
  integer,parameter ::  XWTIME =  2
  integer,parameter ::  PUTIME =  4
  integer,parameter ::  PSTIME =  8
  integer,parameter ::  CUTIME = 16
  integer,parameter ::  CSTIME = 32
  integer,parameter :: ALLTIME = MWTIME + XWTIME + PUTIME +	&
				 PSTIME + CUTIME + CSTIME
  integer,parameter ::  UWRATE = 64

  integer,parameter :: MASKS(0:5) =	&
	(/ MWTIME,XWTIME,PUTIME,PSTIME,CUTIME,CSTIME /)

  character(len=*),parameter :: ZEIT='.zeit.'
  character(len=8),parameter :: HEADER(0:5) =	&
    (/	'[MWTIME]','[XWTIME]','[PUTIME]',	&
	'[PSTIME]','[CUTIME]','[CSTIME]'	/)
  character(len=8),parameter :: UWRHDR = '[UWRATE]'

  integer,parameter :: MXN= 250	! the size of a name list
! integer,parameter :: NSZ= 32	! the size of a name
! LPC jun/6/2000
  integer,parameter :: NSZ= 36	! the size of a name
  integer,parameter :: MXS= 64	! the depth of the timer stack

  integer,save :: nreset=0
  logical,save :: started=.false.
  logical,save :: balanced=.false.

  character(len=NSZ),	&
	  save :: ciname=' '
  character(len=NSZ),	&
	  save :: coname=' '

  integer,save :: mxdep=0	! the maximum ndep value recorded
  integer,save :: ndep=-1	! depth, number of net ci_()
  integer,save :: lnk_n(0:MXS)	! name index of the depth

  integer,save			:: nname=-1	! number of accounts
  character(len=NSZ),	&
	  save,dimension(0:MXN) :: name_l	! the accounts
  integer,save,dimension(0:MXN)	:: knt_l	! counts of ci_() calls
  integer,save,dimension(0:MXN) :: level_l	! remaining ci_() counts

  real*8,save,dimension(0:5)	   :: zts_sv	! the last timings

  real*8,save,dimension(0:5,0:MXN) ::  zts_l	! credited to a name
  real*8,save,dimension(0:5,0:MXN) :: szts_l	! all under the name
  real*8,save,dimension(0:5,0:MXN) :: szts_sv	! the last ci_ timings

!=======================================================================
contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: ci_ - push an entry into the timer
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine ci_(name,usrtime,mytime)
      use m_stdio ,only : stderr
      use m_die   ,only : die
      use m_mpif90,only : MP_wtime
      implicit none
      character(len=*), intent(in) :: name
      real*8, optional, intent(in) :: usrtime
      real*8, optional,dimension(0:),intent(in) :: mytime

! !REVISION HISTORY:
! 	05Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::ci_'

	! Local variables

  real*8,dimension(0:5) :: zts
  integer :: lname,iname
  integer :: ntm
  integer :: i

	! Encountered a limitation.  Programming is required

  if(ndep >= MXS) then
    write(stderr,'(2a,i4)') myname_,	&
	': stack overflow with "'//trim(name)//'", ndep =',ndep
    call die(myname_)
  endif

	!--------------------------------------------------------
	! Initialize the stack if it is called the first time.

  if(.not.started) call reset_(usrtime=usrtime,mytime=mytime)

	! Get the current _zeits_

  if(present(mytime)) then
    ntm=min(ubound(zts,1),ubound(mytime,1))
    zts(0:ntm)=mytime(0:ntm)
    zts(ntm+1:)=0.
  elseif(present(usrtime))then
    zts(0:1)=usrtime
    zts(2:)=0.
  else
    call get_zeits(zts(1))
    zts(0)=MP_wtime()
  endif

	!--------------------------------------------------------
	! Charge the ticks since the last co_() to the current level

  lname=lnk_n(ndep)

  do i=0,5
    zts_l(i,lname)=zts_l(i,lname) + zts(i)-zts_sv(i)
  end do

  do i=0,5
    zts_sv(i)=zts(i)		! update the record
  end do

	!--------------------------------------------------------
	! Is the name already in the list?  Case sensitive and
	! space maybe sensitive if they are inbeded between non-
	! space characters.
	!
	! If the name is already in the list, the index of the
	! table entry is given.
	!
	! If the name is not in the list, a new entry will be added
	! to the list, if 1) there is room, and 2) 

  iname=lookup_(name)

	!--------------------------------------------------------
	! push up the stack level

  ndep=ndep+1
  if(mxdep <= ndep) mxdep=ndep

  lnk_n(ndep)=iname
  knt_l(iname)=knt_l(iname)+1

	! Recording the check-in time, if there is no remaining 
	! levels for the same name.  This is used to handle 
	! recursive ci_() calls for the same name.

  if(level_l(iname) == 0) then
    do i=0,5
      szts_sv(i,iname)=zts_sv(i)
    end do
  endif

	! open a level

  level_l(iname)=level_l(iname)+1

end subroutine ci_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: co_ - pop the current level
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine co_(name,tms,usrtime,mytime)
      use m_stdio ,only : stderr
      use m_die   ,only : die
      use m_mpif90,only : MP_wtime
      implicit none
      character(len=*), intent(in) :: name	! account name
      real*8,optional,dimension(0:5,0:1),intent(out) :: tms ! timings
      real*8,optional,intent(in)::usrtime
      real*8,optional,dimension(0:),intent(in) :: mytime

!     The returned variable tms(0:5,0:1) contains two sets of timing
!   information.  tms(0:5,0) is the NET timing data charged under the
!   account name only, and tms(0:5,1) is the SCOPE timing data since
!   the last ci() with the same account name and at the out most level.
!
! !REVISION HISTORY:
! 	11Oct99 - J.W. Larson - <jlarson@dao> explicit definition of 
!                 tms as real*8
! 	05Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::co_'

  real*8 :: tms0,tms1
  real*8,dimension(0:5) :: zts
  integer :: lname
  integer :: ntm
  integer :: i

	! Encountered a limitation.  Programming is required

  if(ndep <= 0) then
    write(stderr,'(2a,i4)') myname_,	&
	': stack underflow with "'//trim(name)//'", ndep =',ndep
    call die(myname_)
  endif

	!--------------------------------------------------------
	! Initialize the stack if it is called the first time.

  if(.not.started) call reset_(usrtime=usrtime,mytime=mytime)

	! Get the current _zeits_

  if(present(mytime)) then
    ntm=min(ubound(zts,1),ubound(mytime,1))
    zts(0:ntm)=mytime(0:ntm)
    zts(ntm+1:)=0.
  elseif(present(usrtime))then
    zts(0:1)=usrtime
    zts(2:)=0.
  else
    call get_zeits(zts(1))
    zts(0)=MP_wtime()
  endif

	! need special handling if ndep is too large or too small.

  lname=lnk_n(ndep)
  level_l(lname)=level_l(lname)-1	! close a level

  do i=0,5
      tms0=zts(i)- zts_sv(i)		! NET by the _account_
      tms1=zts(i)-szts_sv(i,lname)	! within its SCOPE

      zts_l(i,lname)= zts_l(i,lname) + tms0

      if(level_l(lname) == 0)		&
        szts_l(i,lname)=szts_l(i,lname) + tms1

      zts_sv(i)=zts(i)

      if(present(tms)) then

	! Return the timings of the current call segment
	!
	!   tms(:,0) is for the NET timing data, that have been charged
	!	to this account.
	!
	!   tms(:,1) is for the SCOPE timing data since the ci() of the
	!	same account name at the out most level.
	!  

        tms(i,0)=tms0
        tms(i,1)=tms1	! only the sub-segments
      endif
  end do

	! Record the unbalanced ci/co.  Name .void. is supplied for
	! backward compartible calls of pzeitend()

  if(name /= '.void.'.and.balanced) then
    balanced = lname == MXN .or. name == name_l(lname)
    if(.not.balanced) then
      ciname=name_l(lname)
      coname=name
    endif
  endif

	! pop (need special handling of ndep too large or too small.

  ndep=ndep-1

end subroutine co_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: reset_ - reset module m_zeit to an initial state
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine reset_(usrtime,mytime)
      use m_mpif90,only : MP_wtime
      implicit none
      real*8, optional,intent(in)::usrtime
      real*8, optional,dimension(0:),intent(in) :: mytime

! !REVISION HISTORY:
! 	04Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::reset_'
  integer :: ntm,i

	! keep tracking the number of reset_() calls

  nreset=nreset+1
  started=.true.
  balanced=.true.

	! Start timing

  if(present(mytime)) then
    ntm=min(ubound(zts_sv,1),ubound(mytime,1))
    zts_sv(0:ntm)=mytime(0:ntm)
    zts_sv(ntm+1:)=0.
  elseif(present(usrtime))then
    zts_sv(0:1)=usrtime
    zts_sv(2:)=0.
  else
    call get_zeits(zts_sv(1))
    zts_sv(0)=MP_wtime()
  endif

	! Sign in the module name for the overheads (.eqv. ci_(ZEIT))

  nname=0
  name_l(nname)=ZEIT
  knt_l(nname)=1

  ndep =0
  lnk_n(ndep)=nname

	! Initialize the timers.

  do i=0,5
     zts_l(i,nname)=0.
    szts_l(i,nname)=0.
    szts_sv(i,nname)=zts_sv(i)
  end do
  level_l(nname)=1

end subroutine reset_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: lookup_ search/insert a name
!
! !DESCRIPTION:
!
! !INTERFACE:

    function lookup_(name)
      implicit none
      character(len=*),intent(in) :: name
      integer :: lookup_

! !REVISION HISTORY:
! 	04Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::lookup_'

  logical :: found
  integer :: ith
  integer :: i

  ith=-1
  found=.false.
  do while(.not.found.and. ith < min(nname,MXN))
    ith=ith+1
    found = name == name_l(ith)
  end do

  if(.not.found) then

    found = nname >= MXN	! Can not handle too many accounts?
    ith=MXN			! Then use the account for ".foo."

    if(.not.found) then		! Otherwise, add a new account.
      nname=nname+1
      ith=nname

      name_l(ith)=name
      if(ith==MXN) name_l(ith)='.foo.'

	! Initialize a new account

      do i=0,5
         zts_l(i,ith)=0.
	szts_l(i,ith)=0.
      end do
      level_l(ith)=0

    endif
  endif

  lookup_=ith

end function lookup_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: flush_ - print the timing data
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine flush_(lu,umask,usrtime,mytime,subname_at_end)
      use m_stdio ,only : stderr
      use m_ioutil,only : luflush
      use m_die   ,only : die
      use m_mpif90,only : MP_wtime
      implicit none
      integer,intent(in) :: lu	! logical unit for the output
      integer,optional,intent(in) :: umask
      real*8, optional,intent(in) :: usrtime
      real*8, optional,dimension(0:),intent(in) :: mytime
      logical,optional,intent(in) :: subname_at_end

! !REVISION HISTORY:
!       14Mar2003 - Lang_ping Chang
!                   Add an optional input logical variable
!                   'subname_at_end'.  When true, the name of
!                   the subroutine under title [WTIME] will
!                   appear at the end in lieu of the beginning
!                   of each line in the timing information table.
! 	05Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::flush_'
  integer :: imask

  real*8,dimension(0:5) :: zts
  integer :: ntm,i,ier

	! specify which timer to print

  imask=MWTIME

        ! if usrtime option is present do not use umask option

  if(.not.(present(usrtime)))then
    if(present(umask)) imask=umask
  endif

	! write a <newline>

  write(lu,*,iostat=ier)
  if(ier /= 0) then
    write(stderr,'(2a,i3)') myname_,': can not write(), unit =',lu
    call die(myname_)
  endif

  if(.not.balanced) write(lu,'(5a)') myname_,	&
	': ci/co unbalanced, ',trim(ciname),'/',trim(coname)

  call luflush(lu)

	! latest times, but not closing on any entry

  if(present(mytime)) then
    ntm=min(ubound(zts,1),ubound(mytime,1))
    zts(0:ntm)=mytime(0:ntm)
    zts(ntm+1:)=0.
  elseif(present(usrtime))then
    zts(0:1)=usrtime
    zts(2:)=0.
  else
    call get_zeits(zts(1))
    zts(0)=MP_wtime()
  endif

	! Print selected tables

  do i=0,5
    if(iand(MASKS(i),imask) /= 0)	&
      call sp_balances_(lu,i,zts(i),subname_at_end=subname_at_end)
  end do
#ifdef TODO
  if(iand(UWRATE,imask) /= 0) call sp_rate_(lu,zts)
#endif

  call luflush(lu)

end subroutine flush_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: sp_balances_ - print a table of a given timer
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine sp_balances_(lu,itm,zti,subname_at_end)
      implicit none
      integer,intent(in) :: lu
      integer,intent(in) :: itm
      real*8,intent(in) :: zti
      logical,optional,intent(in) :: subname_at_end

! !REVISION HISTORY:
!       14Mar2003 - Lang_ping Chang
!                   Add an optional input logical variable
!                   'subname_at_end'.  When true, the name of
!                   the subroutine under title [WTIME] will
!                   appear at the end in lieu of the beginning
!                   of each line in the timing information table.
! 	06Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::sp_balances_'

  real*8,parameter :: res=.001	! (sec)

  integer,parameter :: lnmax=12
  character(len=max(NSZ,lnmax)) :: name

  character(len=1) :: tag
  character(len=4) :: num

  integer :: zt_min,zt_sec
  integer :: sz_min,sz_sec
  integer :: l,i,ln,lt

  real*8 :: sz0
  real*8 :: zt,zt_percent,zt_percall
  real*8 :: sz,sz_percent

  logical :: line_end_subname

  line_end_subname=.false.
  if(present(subname_at_end)) line_end_subname=subname_at_end
 
	! The total time is given in the ZEIT bin

  sz0=szts_l(itm,0)
  if(level_l(0) /= 0) sz0=sz0 + zti - szts_sv(itm,0)
  sz0=max(res,sz0)

 If(.not.line_end_subname)then
  write(lu,'(a,t14,a,t24,a,t34,a,t58,a)')	&
    HEADER(itm), 'counts','period',	&
      'NET     m:s      %',		&
    'SCOPE     m:s      %'

!23.|....1....|....2....|....3....|....4....|....5....|....6....|....7..
![MWTIME]    counts period    NET    m:s      %        SCOPE    m:s      %
!---------------------------------------------------------------------------
!zeit.      (  3s  3d  3)    333.3   33:33   3.3+       333.3   33:33   3.3+
!sub           333   33.3    333.3   33:33   3.3%       333.3   33:33   3.3%
 Else
  write(lu,'(t1,a,t13,a,t22,a,t46,a,a)')        &
    'counts','period',  &
      'NET      m:s     %',             &
    'SCOPE      m:s     %    ',HEADER(itm)

!23.|....1....|....2....|....3....|....4....|....5....|....6....|....7..
!counts      period   NET     m:s      %      SCOPE     m:s      %    [WTIME]
!-----------------------------------------------------------------------------
!zeit.      (  3s  3d  3)   333.3   33:33   3.3+   333.3   33:33   3.3+
!sub           333   33.3   333.3   33:33   3.3%   333.3   33:33   3.3%

 Endif
  write(lu,'(80a)') ('-',i=1,79)
  do l=0,min(MXN,nname)

    zt= zts_l(itm,l)
    sz=szts_l(itm,l)
    tag='%'
    if(level_l(l) /= 0) then
      zt=zt + zti -  zts_sv(itm)
      sz=sz + zti - szts_sv(itm,l)
      tag='+'
    endif

    zt_percall=zt/max(1,knt_l(l))

    zt_percent=100.*zt/sz0
    sz_percent=100.*sz/sz0

    zt_sec=nint(zt)
    zt_min=    zt_sec/60
    zt_sec=mod(zt_sec,60)

    sz_sec=nint(sz)
    sz_min=    sz_sec/60
    sz_sec=mod(sz_sec,60)

    name=name_l(l)
    ln=max(len_trim(name),lnmax)
 If(.not.line_end_subname)then
    write(lu,'(a)',advance='no') name(1:ln)
    if(ln > lnmax) write(lu,'(/,a)',advance='no') repeat(' ',lnmax)
 Endif

    select case(l)
      case(0)
	write(num,'(i4)') mxdep
 If(.not.line_end_subname)then
	write(lu,	&
	  '(1x,2(i3,a),a,t28,2(1x,f8.1,1x,i6.2,a,i2.2,1x,f5.1,a))') &
	  nreset,'s',ndep,'/',num,			&
	  zt,zt_min,':',zt_sec,zt_percent,tag,		&
	  sz,sz_min,':',sz_sec,sz_percent,tag
 Else
        write(lu,       &
          '(1x,2(i3,a),a,t16,2(1x,f8.1,1x,i6.2,a,i2.2,1x,f5.1,a),2x,a)') &
          nreset,'s',ndep,'/',num,                      &
          zt,zt_min,':',zt_sec,zt_percent,tag,          &
          sz,sz_min,':',sz_sec,sz_percent,tag,          &
          name(1:ln)
 Endif

      case default
 If(.not.line_end_subname)then
        write(lu,	&
	  '(1x,i5,1x,f8.1,2(1x,f8.1,1x,i6.2,a,i2.2,1x,f5.1,a))') &
          knt_l(l),zt_percall,			&
	  zt,zt_min,':',zt_sec,zt_percent,tag,	&
	  sz,sz_min,':',sz_sec,sz_percent,tag
 Else
        write(lu,       &
          '(1x,i5,1x,f8.1,2(1x,f8.1,1x,i6.2,a,i2.2,1x,f5.1,a),2x,a)') &
          knt_l(l),zt_percall,                  &
          zt,zt_min,':',zt_sec,zt_percent,tag,  &
          sz,sz_min,':',sz_sec,sz_percent,tag,  &
          name(1:ln)
 Endif

    end select

  end do
  write(lu,'(80a)') ('-',i=1,79)

end subroutine sp_balances_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: allflush_ - print a summary of all PEs.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine allflush_(comm,root,lu,umask,mytime,subname_at_end)
      use m_stdio ,only : stderr
      use m_ioutil,only : luflush
      use m_die   ,only : die,MP_die
      use m_mall  ,only : mall_ison,mall_mci,mall_mco
      use m_mpif90,only : MP_wtime,MP_type
      use m_mpif90,only : MP_comm_size,MP_comm_rank
      use m_mergedList,only : mergedList
      use m_mergedList,only : mergedList_init
      use m_mergedList,only : msize
      use m_mergedList,only : ptr_indx
      use m_mergedList,only : ptr_list
      use m_mergedList,only : clean
      implicit none
      integer,intent(in) :: comm
      integer,intent(in) :: root
      integer,intent(in) :: lu
      integer,optional,intent(in) :: umask
      real*8,optional,dimension(0:),intent(in) :: mytime
      logical,optional,intent(in) :: subname_at_end

! !REVISION HISTORY:
!       14Mar2003 - Lang_ping Chang
!                   Add an optional input logical variable
!                   'subname_at_end'.  When true, the name of
!                   the subroutine under title [WTIME] will
!                   appear at the end in lieu of the beginning
!                   of each line in the timing information table.
! 	09Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::allflush_'
  integer myID,nPEs
  integer :: imask
  real*8,dimension(0:5)	:: zts
  real*8,allocatable,dimension(:,:,:  ) :: ztmp
  real*8,allocatable,dimension(:,:,:  ) :: zsum
  real*8,allocatable,dimension(:,:,:  ) :: zavg
  real*8,allocatable,dimension(:,:,:  ) :: zabv
  real*8,allocatable,dimension(:,:,:,:) :: zmax
  integer,allocatable,dimension(:):: ktmp
  integer,allocatable,dimension(:):: ksum
  integer :: mname
  integer :: nlist
  integer         ,pointer,dimension(:  ) :: pindx
  character(len=1),pointer,dimension(:,:) :: plist
  type(mergedList) :: merged

  integer :: i,k,l
  integer :: ntm
  integer :: ier

  imask=MWTIME
  if(present(umask)) imask=umask

  if(imask==0) return

  if(present(mytime)) then
    ntm=min(ubound(zts,1),ubound(mytime,1))
    zts(0:ntm)=mytime(0:ntm)
    zts(ntm+1:)=zts(ntm)
  else
    call get_zeits(zts(1))
    zts(0)=MP_wtime()
  endif

  mname=min(MXN,nname)
  call mergedList_init(name_l(0:mname),merged,root,comm)

  nlist = msize(merged)-1	! Array indices are from 0: for this 
				! module

	allocate( ztmp(    0:5,0:1,0:nlist),	&
		  zsum(    0:5,0:1,0:nlist),	&
		  zavg(    0:5,0:1,0:nlist),	&
		  zabv(    0:5,0:1,0:nlist),	&
		  zmax(0:1,0:5,0:1,0:nlist),	&
		  ktmp(            0:nlist),	&
		  ksum(            0:nlist),	&
		  stat=ier)
		if(ier/=0) call die(myname_,'allocate()',ier)

		if(mall_ison()) then
		  call mall_mci(ztmp,myname)
		  call mall_mci(zsum,myname)
		  call mall_mci(zavg,myname)
		  call mall_mci(zabv,myname)
		  call mall_mci(zmax,myname)
		  call mall_mci(ktmp,myname)
		  call mall_mci(ksum,myname)
		endif

	! Prepare the information for all accounts as in the merged
	! account list.

	pindx => ptr_indx(merged)	! indices to the merge list

  ztmp(:,:,:)=0.	! zero all accounts in the merge list.
  ktmp(    :)=0

  do l=1,mname		! For all local accounts other than ZEIT,
    k=pindx(l+1)-1	! +lbound(pindx), then -lbound(pindx_values).
    do i=0,5
      ztmp(i,0,k)= zts_l(i,l)
      ztmp(i,1,k)=szts_l(i,l)
    end do

    if(level_l(l) /= 0) then
		! Update all current accounts.
      do i=0,5
	ztmp(i,0,k)=ztmp(i,0,k) + zts(i) - zts_sv(i  )
	ztmp(i,1,k)=ztmp(i,1,k) + zts(i) -szts_sv(i,l)
      end do
    endif

    ktmp(k)=knt_l(l)
  end do

	! Update the account for this timing module.  Note that the
	! concept of "account for this timing module" is irrelvant in
	! if optional dummy argument mytime is present.

  if(present(mytime)) then
    ntm=min(ubound(zts,1),ubound(mytime,1))
    zts(0:ntm)=mytime(0:ntm)
    zts(ntm+1:)=zts(ntm)
  else
    call get_zeits(zts(1))
    zts(0)=MP_wtime()
  endif

			! For the account of ZEIT,
  k=pindx(1)-1		! 0+lbound(pindx), then -lbound(pindx_values).
  do i=0,5
    ztmp(i,0,k)= zts_l(i,0)
    ztmp(i,1,k)=szts_l(i,0)
  end do

  do i=0,5
    ztmp(i,0,k)=ztmp(i,0,k) + zts(i) - zts_sv(i)
    ztmp(i,1,k)=ztmp(i,1,k) + zts(i) -szts_sv(i,0)
  end do
  ktmp(k)=knt_l(0)

	! Compute global statisticis through reducation.

  call MP_comm_rank(comm,myID,ier)
	if(ier /= 0) call MP_die(myname_,'MP_comm_rank()',ier)

  call MP_comm_size(comm,nPEs,ier)
	if(ier /= 0) call MP_die(myname_,'MP_comm_size()',ier)

  call allreduce_(ztmp(    0:5,0:1,0:nlist),	&
		  zsum(    0:5,0:1,0:nlist),	&
		  zavg(    0:5,0:1,0:nlist),	&
		  zabv(    0:5,0:1,0:nlist),	&
		  zmax(0:1,0:5,0:1,0:nlist),	&
		  ktmp(            0:nlist),	&
		  ksum(            0:nlist),	&
		  myID,nPEs,comm)

  if(myID == root) then

	! write a <newline>

    write(lu,*,iostat=ier)
	if(ier/=0) call die(myname_,'write()',ier,'unit',lu)

    call luflush(lu)

	plist => ptr_list(merged)

    do i=0,5
      if(iand(MASKS(i),imask) /= 0)	&
	call mp_balances_(lu,i,nPEs,plist,zsum,zavg,zabv,zmax, &
	     ksum, &
             subname_at_end=subname_at_end)
    end do

    call luflush(lu)
  endif

	if(mall_ison()) then
	  call mall_mco(ztmp,myname)
	  call mall_mco(zsum,myname)
	  call mall_mco(zavg,myname)
	  call mall_mco(zabv,myname)
	  call mall_mco(zmax,myname)
	  call mall_mco(ktmp,myname)
	  call mall_mco(ksum,myname)
	endif
  deallocate(ztmp,zsum,zavg,zabv,zmax,ktmp,ksum,stat=ier)
	if(ier/=0) call die(myname_,'deallocate()',ier)

  call clean(merged)

end subroutine allflush_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: mp_balances_ - summarize the timing data of all PEs
!
! !DESCRIPTION:
!
! \newcommand{\tb}{\overline{t}}
!
!	\verb"mp_balances_"() summarizes the timing data of all PEs
!   with quantified load balancing measures:
!   \begin{eqnarray*}
!	x &=& \frac{\max(t) - \tb}{N\tb}	\times 100\%	\\
!	i &=& \frac{\max(t) - \tb}{\max(t)}	\times 100\%	\\
!	r &=& \frac{1}{N\tb} \sum^{t>\tb}{(t-\tb)}
!		\times 100\%
!   \end{eqnarray*}
!   where
!   \begin{center}
!     \begin{tabular}{rl}
!       $t$: & time by any process element			\\
!     $\tb$: & mean time by all process elements		\\
!	$x$: & the ma{\bf x}imum percentage load deviation	\\
!	$i$: & percentage {\bf i}dle process-time or
!					load {\bf i}mbalance	\\
!	$r$: & percentage {\bf r}elocatable loads		\\
!	$N$: & {\bf n}umber of process elements
!     \end{tabular}
!   \end{center}
!
! !INTERFACE:

    subroutine mp_balances_(lu,item,nPEs,names,zsum,zavg,zabv,zmax,&
               ksum,subname_at_end)
      implicit none
      integer,intent(in) :: lu
      integer,intent(in) :: item
      integer,intent(in) :: nPEs
      character(len=1),dimension(:,0:),intent(in) :: names
      real*8,   dimension(0:,0:,0:   ),intent(in) :: zsum
      real*8,   dimension(0:,0:,0:   ),intent(in) :: zavg
      real*8,   dimension(0:,0:,0:   ),intent(in) :: zabv
      real*8,   dimension(0:,0:,0:,0:),intent(in) :: zmax
      integer,dimension(0:),intent(in) :: ksum
      logical,optional,intent(in) :: subname_at_end

! !REVISION HISTORY:
!       14Mar2003 - Lang_ping Chang
!                   Add an optional input logical variable
!                   'subname_at_end'.  When true, the name of
!                   the subroutine under title [WTIME] will
!                   appear at the end in lieu of the beginning
!                   of each line in the timing information table.
! 	10Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::mp_balances_'

  real*8,parameter :: res=.001	! (sec)

  integer,parameter :: lnmax=12
  character(len=max(size(names,1),lnmax)) :: namei
  character(len=4) :: num

  integer :: i,l,ln,lname

	! NET timings
  integer :: ix_o
  real*8  :: zts_o,zta_o,ztm_o,ztr_o
  integer :: x_o,i_o,r_o,m_o

	! SCOPE timings
  integer :: ix_s
  real*8  :: zts_s,zta_s,ztm_s,ztr_s,tta_s
  integer :: x_s,i_s,r_s,m_s

  logical :: line_end_subname

  line_end_subname=.false.
  if(present(subname_at_end))line_end_subname=subname_at_end

  write(num,'(i4)') nPEs
 If(.not.line_end_subname)then
  write(lu,'(3a,t15,a,t49,a)')	&
    HEADER(item),'x',adjustl(num),	&
    'NET avg %tt     max imx x% r% i%',	&
    'SCP avg %tt     max imx x% r% i%'

!23.|....1....|....2....|....3....|....4....|....5....|....6....|....7..

!MWTIME]x3    NET avg %tt     max imx x% r% i%  SCP avg %tt     max imx x% r% i%
!-------------------------------------------------------------------------------
!zeit.       333333.3 100 33333.3 333 33 33 33 333333.3 100 33333.3 333 33 33 33    999999x
 Else
  write(lu,'(t3,a,t37,a,2x,3a)')        &
    'NET avg %tt     max imx x% r% i%',     &
    'SCP avg %tt     max imx x% r% i%',     &
    HEADER(item),'x',adjustl(num)

!23.|....1....|....2....|....3....|....4....|....5....|....6....|....7..

! NET avg %tt     max imx x% r% i%  SCP avg %tt     max imx x% r% i%    MWTIME]x3
!-------------------------------------------------------------------------------
!333333.3 100 33333.3 333 33 33 33 333333.3 100 33333.3 333 33 33 33    999999x .zeit.
 Endif

lname=size(names,1)
tta_s=max(zavg(item,1,0),res)	! total time from ".zeit."

write(lu,'(80a)') ('-',i=1,80)
do l=0,ubound(zsum,3)
		! Convert the name of the account from an array to a
		! string.
  do i=1,lname
    namei(i:i)=names(i,l)
  end do
  ln=max(len_trim(namei),lnmax)	! this is the actual size

  zts_o=     zsum(  item,0,l)
  zta_o=     zavg(  item,0,l)
  ztr_o=     zabv(  item,0,l)
  ztm_o=     zmax(0,item,0,l)
   ix_o=nint(zmax(1,item,0,l))
    m_o=nint(100.*zta_o/tta_s)

  x_o=nint(100.*(ztm_o-zta_o)/max(zts_o,res))
  r_o=nint(100.* ztr_o       /max(zts_o,res))
  i_o=nint(100.*(ztm_o-zta_o)/max(ztm_o,res))

  zts_s=     zsum(  item,1,l)
  zta_s=     zavg(  item,1,l)
  ztr_s=     zabv(  item,1,l)
  ztm_s=     zmax(0,item,1,l)
   ix_s=nint(zmax(1,item,1,l))
    m_s=nint(100.*zta_s/tta_s)

  x_s=nint(100.*(ztm_s-zta_s)/max(zts_s,res))
  r_s=nint(100.* ztr_s       /max(zts_s,res))
  i_s=nint(100.*(ztm_s-zta_s)/max(ztm_s,res))

		! Write the account name.  Skip to next line of the
		! length of the name is too long.

 If(.not.line_end_subname)then
  write(lu,'(a)',advance='no') namei(1:ln)
  if(ln>lnmax) then
    write(lu,*)
    write(lu,'(a)',advance='no') repeat(' ',lnmax)
  endif

  write(lu,'(2(1x,f8.1,i4,f8.1,1x,z3.3,3i3),i10,a)')		&
	zta_o,m_o,ztm_o,ix_o,x_o,r_o,i_o,			&
	zta_s,m_s,ztm_s,ix_s,x_s,r_s,i_s,ksum(l),'x'
 Else
  write(lu,'(2(1x,f8.1,i4,f8.1,1x,z3.3,3i3),i10,a,1x,a)')	&
        zta_o,m_o,ztm_o,ix_o,x_o,r_o,i_o,			&
        zta_s,m_s,ztm_s,ix_s,x_s,r_s,i_s,ksum(l),'x',namei(1:ln)
 Endif

end do
write(lu,'(80a)') ('-',i=1,80)
end subroutine mp_balances_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: allreduce_ -
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine allreduce_(ztmp,zsum,zavg,zabv,zmax,ktmp,ksum,myID,nPEs,comm)
      use m_mpif90,only : MP_type
      use m_mpif90,only : MP_2type
      use m_mpif90,only : MP_MAXLOC
      use m_mpif90,only : MP_SUM
      use m_die   ,only : MP_die
      implicit none
      real*8,dimension(0:,0:,0:   ),intent(in ) :: ztmp
      real*8,dimension(0:,0:,0:   ),intent(out) :: zsum
      real*8,dimension(0:,0:,0:   ),intent(out) :: zavg
      real*8,dimension(0:,0:,0:   ),intent(out) :: zabv
      real*8,dimension(0:,0:,0:,0:),intent(out) :: zmax
      integer,dimension(0:),intent(in ) :: ktmp
      integer,dimension(0:),intent(out) :: ksum
      integer,intent(in) :: myID
      integer,intent(in) :: nPEs
      integer,intent(in) :: comm

! !REVISION HISTORY:
! 	15Mar01	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::allreduce_'
  integer ::  i, k, l
  integer :: ni,nk,nl
  integer :: mcount,mptype
  integer :: ier

  zmax(0,:,:,:)=ztmp(:,:,:)
  zmax(1,:,:,:)=myID

		! Locate the maximum timing values on all PEs.

	mcount=size(zmax)/2
	mptype=MP_2type(zmax(0,0,0,0))

  call MPI_allreduce((zmax),zmax,mcount,mptype,MP_MAXLOC,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(zmax)',ier)

		! Compute global summations on all PEs.

	mcount=size(ztmp)
	mptype=MP_type(ztmp(0,0,0))

  call MPI_allreduce(ztmp,zsum,mcount,mptype,MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(zsum)',ier)

		! Compute global counts summations on all PEs.

	mcount=size(ktmp)
	mptype=MP_type(ktmp(0))

  call MPI_allreduce(ktmp,ksum,mcount,mptype,MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(ksum)',ier)

		! Compute global averages on all PEs.

  zavg(:,:,:)=zsum(:,:,:)/nPEs

		! Compute local totals of values above global averages.

  ni=ubound(zabv,1)
  nk=ubound(zabv,2)
  nl=ubound(zabv,3)
  do l=0,nl
    do k=0,nk
      do i=0,ni
	zabv(i,k,l)=max(0._8,ztmp(i,k,l)-zavg(i,k,l))
      end do
    end do
  end do

		! Compute global summations of values above global
		! averages.

	mcount=size(zabv)
	mptype=MP_type(zabv(0,0,0))

  call MPI_allreduce((zabv),zabv,mcount,mptype,MP_SUM,comm,ier)
	if(ier/=0) call MP_die(myname_,'MPI_allreduce(zabv)',ier)

end subroutine allreduce_
!=======================================================================
end module m_zeit
!.
