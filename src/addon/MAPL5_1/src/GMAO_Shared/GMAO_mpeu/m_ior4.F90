!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_ior4 - read/write a real*4 record
!
! !DESCRIPTION:
!
!	This module is expected to be compiled as-is, without modifying
!   the kind of a compiler's default REAL type.  Any selection of the
!   interface is expected to be done by the compiler.
!
! !INTERFACE:
!#include "regime.H"

    module m_ior4
      implicit none
      private	! except

      public :: readr4		! read a REAL*4 record
      public :: writer4		! write a REAL*4 record

      public :: iacc_SEQUENTIAL
      public :: iacc_DIRECT

      interface readr4; module procedure	&
      	readr4_2dr4_,	&	! read to a real*4 buffer
	readr4_2dr8_		! read to a real*8 buffer
      end interface

      interface writer4; module procedure	&
      	writer4_2dr4_,	&	! write to a real*4 buffer
	writer4_2dr8_		! write to a real*8 buffer
      end interface

! !REVISION HISTORY:
! 	29Jul02	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_ior4'

  integer,parameter :: iacc_SEQUENTIAL=1
  integer,parameter :: iacc_DIRECT    =2

#include "assert.H"
contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: readr4_2dr4_ - read the n-th record (nrec) as REAL*4
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine readr4_2dr4_(lu,iacc,nrec,irec,vfld,ierr)
      use m_die,only : perr
      use m_realkinds,only : kind_R4
      implicit none
      integer,intent(in)  :: lu		! the input unit
      integer,intent(in)  :: iacc	! access
      integer,intent(in)  :: nrec	! which to read
      integer,intent(in)  :: irec	! where it is now
      real(kind=kind_R4),dimension(:,:),intent(out) :: vfld
      integer,intent(out) :: ierr

! !REVISION HISTORY:
! 	22Jan99 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::readr4_2dr4_'

!   In this module procedure, the kind of the buffer (real*4) is the
! same as the kind of the record (real*4).  Therefore, there is no need
! for conversion.

  integer :: nskp
  integer :: i

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
	  call perr(myname_,'skip_sequential, irec',irec,'nrec',nrec)
	  return
	endif
    end do

			! Sequential read()

    read(lu,iostat=ierr) vfld

	if(ierr/=0) then
	  call perr(myname_,'read_sequential(nrec)',nrec)
	  return
	endif

  case(iacc_DIRECT)
			! Direct read()

    read(lu,rec=nrec,iostat=ierr) vfld

	if(ierr/=0) then
	  call perr(myname_,'read_direct(nrec)',nrec)
	  return
	endif

  case default
    ierr=-1
    call perr(myname_,'unknown iacc',iacc)
  end select
end subroutine readr4_2dr4_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: readr4_2dr8_ - read the n-th record (nrec) as REAL*8
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine readr4_2dr8_(lu,iacc,nrec,irec,vfld,ierr)
      use m_die,only : perr
      use m_realkinds,only : kind_R4,kind_R8
      implicit none
      integer,intent(in)  :: lu		! the input unit
      integer,intent(in)  :: iacc	! access
      integer,intent(in)  :: nrec	! which to read
      integer,intent(in)  :: irec	! where it is now
      real(kind=kind_R8),dimension(:,:),intent(out) :: vfld
      integer,intent(out) :: ierr

! !REVISION HISTORY:
! 	22Jan99 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::readr4_2dr8_'

!   In this module procedure, the kind of the buffer (real*8) is not the
! same as the kind of the record (real*4).

  real(kind=kind_R4),dimension(size(vfld,1),size(vfld,2)) :: dbuf

  integer :: nskp
  integer :: i

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
	  call perr(myname_,'skip_sequential, irec',irec,'nrec',nrec)
	  return
	endif
    end do

			! Sequential read()

    read(lu,iostat=ierr) dbuf

    	if(ierr/=0) then
	  call perr(myname_,'read_sequential(nrec)',nrec)
	  return
	endif

  case(iacc_DIRECT)
			! Direct read()

    read(lu,rec=nrec,iostat=ierr) vfld

	if(ierr /= 0) then
	  call perr(myname_,'read_direct(nrec)',nrec)
	  return
	endif

  case default
    ierr=-1
    call perr(myname_,'unknown iacc',iacc)
    return
  end select

  vfld(:,:)=dbuf(:,:)

end subroutine readr4_2dr8_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: writer4_2dr4_ - write a 2-d field REAL*4 record
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine writer4_2dr4_(lu,iacc,irec,nlon,nlat,vfld,ier)
      use m_die, only : perr
      use m_realkinds,only : kind_R4
      implicit none
      integer,intent(in) :: lu
      integer,intent(in) :: iacc
      integer,intent(in) :: irec
      integer,intent(in) :: nlon
      integer,intent(in) :: nlat
      real(kind=kind_r4),dimension(:,:),intent(in) :: vfld
      integer,intent(out) :: ier

! !REVISION HISTORY:
! 	24Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::writer4_2dr4_'

  select case(iacc)
  case(iacc_DIRECT)

    write(lu,rec=irec,iostat=ier) vfld
	if(ier/=0) call perr(myname_,'write_DIRECT',irec)

  case(iacc_SEQUENTIAL)

    write(lu,iostat=ier) vfld
	if(ier/=0) call perr(myname_,'write_SEQUENTIAL',irec)

  case default
    call perr(myname_,'unknown iacc value',iacc)
    ier=-1
    
  end select

end subroutine writer4_2dr4_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: writer4_2dr8_ - write a 2-d field REAL*4 record
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine writer4_2dr8_(lu,iacc,irec,nlon,nlat,vfld,ier)
      use m_die, only : perr
      use m_realkinds,only : kind_R4,kind_R8
      implicit none
      integer,intent(in) :: lu
      integer,intent(in) :: iacc
      integer,intent(in) :: irec
      integer,intent(in) :: nlon
      integer,intent(in) :: nlat
      real(kind=kind_R8),dimension(:,:),intent(in) :: vfld
      integer,intent(out) :: ier

! !REVISION HISTORY:
! 	24Feb00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::writer4_2dr8_'

  real(kind=kind_R4),dimension(size(vfld,1),size(vfld,2)) :: dbuf

  dbuf(:,:)=vfld(:,:)

  select case(iacc)
  case(iacc_DIRECT)

    write(lu,rec=irec,iostat=ier) dbuf
	if(ier/=0) call perr(myname_,'write_DIRECT(irec)',irec)

  case(iacc_SEQUENTIAL)

    write(lu,iostat=ier) vfld
	if(ier/=0) call perr(myname_,'write_SEQUENTIAL(irec)',irec)

  case default
    call perr(myname_,'unknown iacc value',iacc)
    ier=-1
    
  end select

end subroutine writer4_2dr8_
end module m_ior4
