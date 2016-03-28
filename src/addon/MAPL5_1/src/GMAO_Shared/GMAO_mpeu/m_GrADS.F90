!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_GrADS - GrADS file reader/writer
!
! !DESCRIPTION:
!
! !INTERFACE:
!#include "regime.H"

    module m_GrADS
      use m_wGrADS,only : GrADS_writer => wGrADS
      use m_wGrADS,only : GrADS_create => wGrADS_open
      use m_wGrADS,only : GrADS_write  => wGrADS_write
      use m_wGrADS,only : GrADS_close  => wGrADS_close
      use m_wGrADS,only : inquire
      use m_wGrADS,only : bcast

      use m_rGrADS,only : GrADS_reader => rGrADS
      use m_rGrADS,only : GrADS_open
      use m_rGrADS,only : GrADS_read
      use m_rGrADS,only : GrADS_close  => rGrADS_close

      use m_rGrADS,only : inquire
      use m_rGrADS,only : bcast
      use m_rGrADS,only : LEN_VARS	! char. len. of ptr_vars

      use m_rGrADS,only : ptr_zdef
      use m_rGrADS,only : ptr_vars
      use m_rGrADS,only : ptr_nrec

      implicit none
      private	! except

      public :: GrADS_writer	! data structure of a GrADS_writer
      public :: GrADS_create	! open a GrADS_writer
      public :: GrADS_write	! write a variable to a _writer

      public :: GrADS_reader	! data structure of a GrADS_reader
      public :: GrADS_open	! open a GrADS_reader
      public :: GrADS_read	! read a variable from a _reader

      public :: GrADS_close	! close a _reader or a _writer

      public :: inquire		! get attributes of a _reader
      public :: bcast		! populate meta data to all PEs
      public :: LEN_VARS	! char. len of ptr_vars(:)
      public :: ptr_zdef	! level references of a _reader
      public :: ptr_vars	! variable name references of a _reader
      public :: ptr_nrec	! no. of levels per variable.  0: 2-d

! !REVISION HISTORY:
! 	20Dec05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_GrADS'

!!.. Create a GrADS file
! use m_GrADS,only : GrADS_writer,GrADS_create,GrADS_write,GrADS_close
! type(GrADS_writer) :: gw
! nlon=..; nlat=..; nlev=..; zdef(1:nlev)=..; nvar=..
! call GrADS_create(gw,ctrl,nlon,nlat,nlev,zdef(1:nlev),nvar	&
!	[,nymd,nhms,incr,dset,access,udef,stat])
! call GrADS_write(gw,'xvar',xvar [,stat])
! call GrADS_write(gw,'yvar',yvar [,stat])
! call GrADS_close(gw [,stat])

!!.. Read a GrADS file
! use m_GrADS,only : GrADS_reader,GrADS_open,GrADS_read,GrADS_close
! type(GrADS_reader) :: gr
! call GrADS_open(gr,ctrl [,stat])
! call GrADS_inquire(gr [,idim=im,jdim=jm,...])
!	allocate(zdef(km),vnames(nvar))
!	allocate(xvar(im,jm,km),yvar(im,jm,km))
! zdef = ptr_zdef(gr)
! call GrADS_read(gr,'xvar',xvar [,stat])
! call GrADS_read(gr,'yvar',yvar [,stat])
! call GrADS_close(gr [,stat])
!	deallocate(xvar,yvar)

end module m_GrADS
