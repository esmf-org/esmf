!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: m_intkinds - integer KIND definitions
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_intkinds
      implicit none
      private	! except

      public :: kind_i4		! INTEGER*4
      public :: kind_i8		! INTEGER*8
      public :: kind_i		! default INTEGER

      integer*4,parameter :: I4=1
      integer*8,parameter :: I8=1
      integer,  parameter :: I =1

      integer,parameter :: kind_i4=kind(I4)
      integer,parameter :: kind_i8=kind(I8)
      integer,parameter :: kind_i =kind(I )

! !REVISION HISTORY:
!	15Apr02	- Jing Guo
!		. initial prototype/prolog/code
!EOP
!_______________________________________________________________________
  character(len=*),parameter :: myname='m_intkinds'

end module m_intkinds
