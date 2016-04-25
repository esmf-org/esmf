!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_IndexBin_logical - Template of indexed bin-sorting module
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_IndexBin_logical
      implicit none
      private	! except

      public :: IndexBin
      interface IndexBin; module procedure 	&
	IndexBin0_
      end interface

! !REVISION HISTORY:
! 	19Dec05 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- merged bug fix in indexing (ix=indx(i)) between
!		  1.1.2.2 and 1.1.2.3 to 1.3.
! 	17Feb99 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_IndexBin_logical'

contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: IndexBin0_ - Indexed sorting for a single value
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine IndexBin0_(n,indx,keys,key0,ln0)
      use m_stdio, only : stderr
      use m_die,   only : die
      use m_mall,  only : mall_ison,mall_mci,mall_mco
      implicit none

      integer, intent(in) :: n
      integer, dimension(n), intent(inout) :: indx
      logical, dimension(n), intent(in) :: keys
      logical, intent(in) :: key0 ! The key value to be moved to front
      integer,optional,intent(out) :: ln0

! !REVISION HISTORY:
! 	16Feb99 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!	27Sep99 - Jing Guo <guo@thunder> - Fixed a bug pointed out by
!					   Chris Redder
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::IndexBin0_'
  integer,allocatable,dimension(:) :: inew
  integer :: ni,ix,i,ier
  integer :: ln(0:1),lc(0:1)
!________________________________________

	allocate(inew(n),stat=ier)
		if(ier /= 0) then
		  write(stderr,'(2a,i4)') myname_,	&
			': allocate() error, stat =',ier
		  call die(myname_)
		endif
		if(mall_ison()) call mall_mci(inew,myname)
!________________________________________
		! Count numbers entries for the given key0
	
  lc(0)=1	! the location of values the same as key0
  ln(0)=0
  do i=1,n
    ix=indx(i)
    if(keys(ix) .eqv. key0) ln(0)=ln(0)+1
  end do

  lc(1)=ln(0)+1	! the location of values not the same as key0
!________________________________________
		! Reset the counters
  ln(0:1)=0
  do i=1,n
    ix=indx(i)
    if(keys(ix) .eqv. key0) then
      ni=lc(0)+ln(0)
      ln(0)=ln(0)+1
      
    else
      ni=lc(1)+ln(1)
      ln(1)=ln(1)+1
    endif

    inew(ni)=ix
  end do

!________________________________________
		! Sort out the old pointers according to the new order
  indx(:)=inew(:)
  if(present(ln0)) ln0=ln(0)
!________________________________________

		if(mall_ison()) call mall_mco(inew,myname)
	  deallocate(inew)

end subroutine IndexBin0_
end module m_IndexBin_logical
