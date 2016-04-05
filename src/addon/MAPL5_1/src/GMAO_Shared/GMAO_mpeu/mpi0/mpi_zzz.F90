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
