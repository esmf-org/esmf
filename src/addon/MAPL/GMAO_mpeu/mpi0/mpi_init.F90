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
!BOP -------------------------------------------------------------------
!
! !ROUTINE: mpi_init -
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mpi_init(ier)
      use m_mpi0
      implicit none
      integer,intent(out) :: ier

! !REVISION HISTORY:
! 	28Sep99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='mpi_init'

#include "Const_MPItype.H"

  ier=0
  if(mpi0_initialized) ier=-1
  if(ier/=0) return

  mpi0_initialized	=.true.

#ifndef Const_MPItype_
  MPI_INTEGER		=1
  MPI_REAL		=2
  MPI_DOUBLE_PRECISION	=3
  MPI_COMPLEX		=4
  MPI_DOUBLE_COMPLEX	=5
  MPI_LOGICAL		=6
  MPI_CHARACTER		=7
  MPI_BYTE		=-1
  MPI_2INTEGER		=-1
  MPI_2REAL		=-1
  MPI_2DOUBLE_PRECISION	=-1
  ! MPI_2COMPLEX	=-1	! not supported on IRIX64
  ! MPI_2DOUBLE_COMPLEX	=-1	! not supported on IRIX64
  MPI_INTEGER1		=-1
  MPI_INTEGER2		=-1
  MPI_INTEGER4		=8
  ! MPI_REAL2		=-1	! not supported on IRIX64
  MPI_REAL4		=9
  MPI_REAL8		=10
#endif

end subroutine mpi_init
