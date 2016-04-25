!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: m_die - die with mpout flushed
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_die
      use m_mpif90, only : MP_perr
      implicit none
      private	! except

      public :: die		! signal an exception
      public :: diex		! a special die() supporting macros
      public :: perr,warn	! message(s) to stderr
      public :: perr_die	! to be phased out
      public :: MP_die		! a special die() for MPI errors
      public :: MP_perr		! perr for MPI errors, from m_mpif90
      public :: MP_perr_die	! a special die() for MPI errors
      public :: assert_		! used by ASSERT() macro of assert.H

      interface die; module procedure	&
	die0_,	  & ! die(where)
	die1_,	  & ! die(where,message)
        die1_r1,  & ! die(where,message) where message is an array of rank 1
	die2_,	  & ! die(where,proc,ier)
	die4_	    ! die(where,mesg1,ival1,mesg2,ival2)
      end interface

      interface diex; module procedure	&
	diex_	   ! diex(where,filename,lineno)
      end interface

      interface perr; module procedure	&
	perr1_,	  & ! perr(where,message)
        perr1_r1, & ! perr(where,message) where message is an array of rank 1
	perr2_,	  & ! perr(where,proc,ier)
	perr4_	   ! perr(where,mesg1,ival1,mesg2,ival2)
      end interface
      interface warn; module procedure	&
	perr1_,	  & ! perr(where,message)
        perr1_r1, & ! perr(where,message) where message is an array of rank 1
	perr2_,	  & ! perr(where,proc,ier)
	perr4_	    ! perr(where,mesg1,ival1,mesg2,ival2)
      end interface

      interface perr_die; module procedure	&
	die2_	  ! perr_die(where,proc,ier)
      end interface

      interface MP_die; module procedure	&
	MPdie2_	  ! MP_die(where,proc,ier)
      end interface
      interface MP_perr_die; module procedure	&
	MPdie2_	  ! MP_die(where,proc,ier)
      end interface

      interface assert_; module procedure	&
	assertmsg_,	&
	assertnomsg_; end interface

! !REVISION HISTORY:
! 	26Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname='m_die'
contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: die0_ - flush(mpout) before die()
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine die0_(where)
      use m_mpout, only : mpout,mpout_flush,mpout_close,mpout_ison
      use m_flow, only : flow_flush
      use m_dropdead, only : ddie => die
      implicit none
      character(len=*),intent(in) :: where

! !REVISION HISTORY:
! 	26Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::die0_'

  call mpout_flush()
  if(mpout_ison()) call flow_flush(mpout)
  call mpout_close()
  call ddie(where)

end subroutine die0_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: die1_ - flush(mpout) before die()
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine die1_(where,message)
      use m_mpout, only : mpout,mpout_flush,mpout_close,mpout_ison
      use m_flow, only : flow_flush
      use m_dropdead, only : ddie => die
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: message

! !REVISION HISTORY:
! 	26Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::die1_'

  call mpout_flush()
  if(mpout_ison()) call flow_flush(mpout)
  call mpout_close()

  call perr1_(where,message)
  call ddie(where)

end subroutine die1_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: die1_r1 - flush(mpout) before die()
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      subroutine die1_r1 ( where, message )
      use m_mpout, only : mpout,mpout_flush,mpout_close,mpout_ison
      use m_flow, only : flow_flush
      use m_dropdead, only : ddie => die
      implicit   none
      character(len=*), intent(in)               :: where
      character(len=*), intent(in), dimension(:) :: message
!
! !REVISION HISTORY:
!     13Oct2000 - C. Redder - initial prototype/prolog/code
!EOP ___________________________________________________________________

      character(len=*),parameter :: myname_=myname//'::die1_r2'

      call mpout_flush()
      if(mpout_ison()) call flow_flush(mpout)
      call mpout_close()

      call perr1_r1(where,message)
      call ddie(where)
      end subroutine die1_r1

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: die2_ - flush(mpout) before die()
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine die2_(where,proc,ier)
      use m_mpout, only : mpout,mpout_flush,mpout_close,mpout_ison
      use m_flow, only : flow_flush
      use m_dropdead, only : ddie => die
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: proc
      integer,intent(in) :: ier

! !REVISION HISTORY:
! 	26Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::die2_'

  call mpout_flush()
  if(mpout_ison()) call flow_flush(mpout)
  call mpout_close()

  call perr2_(where,proc,ier)
  call ddie(where)

end subroutine die2_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: die4_ - flush(mpout) before die()
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine die4_(where,mesg1,ival1,mesg2,ival2)
      use m_mpout, only : mpout,mpout_flush,mpout_close,mpout_ison
      use m_flow, only : flow_flush
      use m_dropdead, only : ddie => die
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: mesg1
      integer,intent(in) :: ival1
      character(len=*),intent(in) :: mesg2
      integer,intent(in) :: ival2

! !REVISION HISTORY:
! 	26Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::die4_'

  call mpout_flush()
  if(mpout_ison()) call flow_flush(mpout)
  call mpout_close()

  call perr4_(where,mesg1,ival1,mesg2,ival2)
  call ddie(where)

end subroutine die4_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: diex_ - flush(mpout) before die()
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine diex_(where,filename,line)
      use m_mpout, only : mpout,mpout_flush,mpout_close,mpout_ison
      use m_flow, only : flow_flush
      use m_dropdead, only : ddie => die
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: filename
      integer,intent(in) :: line

! !REVISION HISTORY:
! 	26Feb98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname_=myname//'::diex_'

  call mpout_flush()
  if(mpout_ison()) call flow_flush(mpout)
  call mpout_close()
  call ddie(where,filename,line)

end subroutine diex_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: perr1_ - send a simple error message to _stderr_
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine perr1_(where,message)
      use m_stdio,only : stderr
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: message

! !REVISION HISTORY:
! 	27Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

      character(len=*),parameter :: myname_=myname//'::perr1_'

      write(stderr,'(3a)') where,': ',message

      end subroutine perr1_


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: perr1_r1 - print a character array of rank 1 to standard error
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      subroutine perr1_r1 ( where, message )
      use m_stdio,only : stderr
      implicit   none
      character(len=*), intent(in)               :: where
      character(len=*), intent(in), dimension(:) :: message
!
! !REVISION HISTORY:
!     13Oct2000 - C. Redder - initial prototype/prolog/code
!EOP ___________________________________________________________________

      integer :: NLines, LLine, iLine

!     For each line ...
!     -----------------
      NLines   = size ( message )
      do iLine = 1, NLines
         LLine = Len_Trim ( message ( iLine ) )

!        ... append to buffer if the line is ...
!        ---------------------------------------
         if ( LLine .eq. 0 ) then
            write ( stderr, '(2a)' ) where, ': '      ! ... blank
                                                      ! ---------
         else
            write ( stderr, '(3a)' ) where, ': ',     &
                                     message ( iLine ) ( : LLine )
                                                      ! ... non-blank
                                                      ! -------------
         end if

      end do
      return
      end subroutine perr1_r1

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: perr2_ - send a simple error message to _stderr_
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine perr2_(where,proc,ier)
      use m_stdio,only : stderr
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: proc
      integer,intent(in) :: ier

! !REVISION HISTORY:
! 	27Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::perr2_'
  character(len=16) :: cer
  integer :: ios

  cer='*******'
  write(cer,'(i16)',iostat=ios) ier
  write(stderr,'(5a)') where,': ',	&
	proc,' error, stat =',trim(adjustl(cer))

end subroutine perr2_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: perr4_ - send a simple error message to _stderr_
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine perr4_(where,mesg1,ival1,mesg2,ival2)
      use m_stdio,only : stderr
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: mesg1
      integer,intent(in) :: ival1
      character(len=*),intent(in) :: mesg2
      integer,intent(in) :: ival2

! !REVISION HISTORY:
! 	27Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::perr4_'
  character(len=16) :: cval1,cval2
  integer :: ios

  cval1='*******'
  cval2='*******'
  write(cval1,'(i16)',iostat=ios) ival1
  write(cval2,'(i16)',iostat=ios) ival2

  write(stderr,'(10a)') where,': error, ',	&
	mesg1,'=',trim(adjustl(cval1)),', ',	&
	mesg2,'=',trim(adjustl(cval2)),'.'

end subroutine perr4_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: MPdie2_ - invoke MP_perr before die_
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine MPdie2_(where,proc,ier)
      use m_mpif90, only : MP_perr
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: proc
      integer,intent(in) :: ier

! !REVISION HISTORY:
! 	27Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::MPdie2_'

  call MP_perr(where,proc,ier)
  call die0_(where)

end subroutine MPdie2_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: assertmsg_ - an utility called by ASSERT() macro only
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine assertmsg_(str, file, line)
      use m_mpout,only : mpout,mpout_flush,mpout_close,mpout_ison
      use m_flow,only : flow_flush
      use m_dropdead,only : ddie => die
      implicit none
      Character(Len=*), Intent(In) :: str	! a message
      Character(Len=*), Intent(In) :: file	! a filename
      Integer, Intent(In) :: line		! a line number

! !REVISION HISTORY:
! 	25Aug00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- modified
!		- included into m_die for easier module management
!	before	- Tom Clune
!		- Created for MPI PSAS implementation as a separate
!		  module
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_='ASSERT_'

  call mpout_flush()
  if(mpout_ison()) call flow_flush(mpout)
  call mpout_close()

  call perr1_(myname_,'failed: "'//str//'"')
  call ddie(myname_,file,line)

End subroutine assertmsg_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: assertnomsg_ - an utility called by ASSERT() macro only
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine assertnomsg_(file, line)
      use m_mpout,only : mpout,mpout_flush,mpout_close,mpout_ison
      use m_flow,only : flow_flush
      use m_dropdead,only : ddie => die
      implicit none
      Character(Len=*), Intent(In) :: file	! a filename
      Integer, Intent(In) :: line		! a line number

! !REVISION HISTORY:
! 	25Aug00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- modified
!		- included into m_die for easier module management
!	before	- Tom Clune
!		- Created for MPI PSAS implementation as a separate
!		  module
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_='ASSERT_'

  call mpout_flush()
  if(mpout_ison()) call flow_flush(mpout)
  call mpout_close()

  call perr1_(myname_,'assertion failed')
  call ddie(myname_,file,line)

End subroutine assertnomsg_
end module m_die
