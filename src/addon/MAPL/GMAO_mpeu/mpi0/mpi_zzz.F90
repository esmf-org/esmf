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
#if defined (sysOSF1) || defined(sysLinux)
!
!  Added because the compiler requires the presence of these procedures,
!  although they are not actually used, but only declared by the system
!  MPI implementation as EXTERNALs.

	function mpi_null_copy_fn()
	  use m_mpi0,only : MPI_PROC_NULL
	  implicit none
	  integer :: mpi_null_copy_fn
	  mpi_null_copy_fn=MPI_PROC_NULL
	end function mpi_null_copy_fn
	function mpi_null_delete_fn()
	  use m_mpi0,only : MPI_PROC_NULL
	  implicit none
	  integer :: mpi_null_delete_fn
	  mpi_null_delete_fn=MPI_PROC_NULL
	end function mpi_null_delete_fn
	function mpi_dup_fn()
	  use m_mpi0,only : MPI_PROC_NULL
	  implicit none
	  integer :: mpi_dup_fn
	  mpi_dup_fn=MPI_PROC_NULL
	end function mpi_dup_fn
	function pmpi_wtime()
	  use m_mpi0,only : MPI_PROC_NULL
 	  pmpi_wtime=MPI_PROC_NULL
	end function pmpi_wtime
	function pmpi_wtick()
	  use m_mpi0,only : MPI_PROC_NULL
	  pmpi_wtick=MPI_PROC_NULL
	end function pmpi_wtick

#else
	subroutine mpi_fake_zzz()
	end subroutine mpi_fake_zzz
#endif
