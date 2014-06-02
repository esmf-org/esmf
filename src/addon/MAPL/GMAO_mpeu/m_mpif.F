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
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: m_mpif - a portable interface to the MPI "mpif.h" COMMONs.
!
! !DESCRIPTION:
!
!   The purpose of \verb"m_mpif" module is to provide a portable
!   interface of \verb"mpif.h" with different MPI implementation.
!   By combining module \verb"m_mpif" and \verb"m_mpif90", it may be
!   possible to build a Fortran 90 MPI binding module graduately.
!
!   Although it is possible to use \verb'include "mpif.h"' directly
!   in individual modules, it has several problems:
!   \begin{itemize}
!   \item It may conflict with either the source code of a {\sl fixed}
!	format or the code of a {\sl free} format;
!   \item It does not provide the protection and the safety of using
!	these variables as what a \verb"MODULE" would provide.
!   \end{itemize}
!
!   More information may be found in the module \verb"m_mpif90".
!
! !INTERFACE:

	module m_mpif
	  implicit none
	  private	! except

	  public :: MPI_INTEGER2
	  public :: MPI_INTEGER4
	  public :: MPI_INTEGER8

	  public :: MPI_INTEGER
	  public :: MPI_REAL
	  public :: MPI_DOUBLE_PRECISION
	  public :: MPI_LOGICAL
	  public :: MPI_CHARACTER

	  public :: MPI_2INTEGER
	  public :: MPI_2REAL
	  public :: MPI_2DOUBLE_PRECISION

	  public :: MPI_REAL4
 	  public :: MPI_REAL8
	  public :: MPI_REAL16

	  public :: MPI_COMM_WORLD
	  public :: MPI_COMM_NULL
	  public :: MPI_REQUEST_NULL

	  public :: MPI_SUM
	  public :: MPI_PROD
	  public :: MPI_MIN
  	  public :: MPI_MAX
	  public :: MPI_MINLOC
  	  public :: MPI_MAXLOC

	  public :: MPI_MAX_ERROR_STRING
	  public :: MPI_STATUS_SIZE
	  public :: MPI_ERROR

!#if !defined(sysLinux)
	  public :: MPI_OFFSET_KIND
	  public :: MPI_INFO_NULL
	  public :: MPI_MODE_RDONLY
	  public :: MPI_MODE_RDWR
	  public :: MPI_SEEK_SET
!#endif
	  public :: MPI_BYTE

#ifdef MPICH_
	  public :: MPIPRIV	! the common block name
#endif

#include "mpif.h"

! !REVISION HISTORY:
! 	01Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
! 	16Feb05 - Todling - added a number of vars needed by GSI
! 	18Mar05 - Todling - added a few more mpi vars used by GSI
! 	20Mar05 - Todling - see no reason for sysLinux ifdef (commented out)
!EOP
!_______________________________________________________________________
	character(len=*),parameter :: myname='m_mpif'

	end module m_mpif
!.
