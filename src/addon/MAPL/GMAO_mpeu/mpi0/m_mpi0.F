C +-======-+ 
C  Copyright (c) 2003-2007 United States Government as represented by 
C  the Admistrator of the National Aeronautics and Space Administration.  
C  All Rights Reserved.
C  
C  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
C  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
C  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
C  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
C  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
C  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
C  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
C  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
C  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
C  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
C  
C  Government Agency: National Aeronautics and Space Administration
C  Government Agency Original Software Designation: GSC-15354-1
C  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
C  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
C  Government Agency Point of Contact for Original Software:  
C  			Dale Hithon, SRA Assistant, (301) 286-2691
C  
C +-======-+ 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_mpi0 - mpi0 flag
!
! !DESCRIPTION:
!
! !INTERFACE:

	module m_mpi0
	implicit none
	private	! except

	public :: mpi0_initialized	! The class data structure
	public :: MPI_INTEGER
	public :: MPI_REAL
	public :: MPI_DOUBLE_PRECISION
	public :: MPI_COMPLEX
	public :: MPI_DOUBLE_COMPLEX
	public :: MPI_LOGICAL
	public :: MPI_CHARACTER
	public :: MPI_BYTE
	public :: MPI_2INTEGER
	public :: MPI_2REAL
	public :: MPI_2DOUBLE_PRECISION
	! public :: MPI_2COMPLEX	! not supported on IRIX64
	! public :: MPI_2DOUBLE_COMPLEX	! not supported on IRIX64
	public :: MPI_INTEGER1
	public :: MPI_INTEGER2
	public :: MPI_INTEGER4
	! public :: MPI_REAL2		! not supported on IRIX64
	public :: MPI_REAL4
	public :: MPI_REAL8
	public :: MPI_STATUS_SIZE
	public :: MPI_PROC_NULL
	public :: MPI_UNDEFINED

! !REVISION HISTORY:
! 	05Jun00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

	character(len=*),parameter :: myname='m_mpi0'
#include "mpif.h"

	logical,save :: mpi0_initialized=.false.

	end module m_mpi0
