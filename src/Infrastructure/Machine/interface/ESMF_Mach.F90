! $Id: ESMF_Mach.F90,v 1.7 2004/01/28 21:46:48 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
!     ESMF Machine module
      module ESMF_MachineMod
!
!==============================================================================
!
! This file contains the Machine class definition and all Machine
! class methods.
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_MachineMod - Fortran Interface to C++ ESMC_Machine class
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Machine} class and associated functions and subroutines.  
!
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      implicit none


!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_Machine
!
!     ! Machine data type.  All information is kept on the C++ side inside
!     ! the class structure.   
!     ! TODO: if there is a single, public Machine instance of the machine
!     !  class, then we have no need for a machine pointer here.  This type
!     !  may need to be removed.

      type ESMF_Machine
      sequence
      private
        type(ESMF_Pointer) :: this       ! opaque pointer to the C++ class data
      end type

!------------------------------------------------------------------------------
!     ! Main program source
!     !   ESMF_Initialize is called from what language?
      integer, parameter :: ESMF_MAIN_C=1, ESMF_MAIN_F90=2

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Machine, ESMF_MAIN_C, ESMF_MAIN_F90
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_MachineInitialize
      public ESMF_MachineFinalize

      !public ESMF_MachineInit

      !public ESMF_MachineSetData
      !public ESMF_MachineGetData
      !public ESMF_MachineRestore
      !public ESMF_MachineWrite
      !public ESMF_MachineRead
 
      public ESMF_MachinePrint

!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Mach.F90,v 1.7 2004/01/28 21:46:48 nscollins Exp $'

!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the one-time calls to Initialize and Finalize
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_MachineInitialize - Query the hardware configuration

! !INTERFACE:
      subroutine ESMF_MachineInitialize(language, rc)
!
! !ARGUMENTS:
      integer, intent(in) :: language
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Query the hardware and initialize a global Machine object.
!
!  The arguments are:
!  \begin{description}
! 
!   \item[language]
!    Flag saying what language the main program is.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
      
      ! Local Variables
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

#if ESMF_MPICH
      ! SYSTEM DEPENDENT CODE SECTION
      ! Try a fix to get around problems starting up mpich programs -
      ! if main is in F90 it is hard to get at the arg list, and we
      ! are calling MPI_Init() from C++ in the machine code below.
      ! for now, call MPI_Init() from the F90 side and see if that can
      ! be linked.  the C++ init call is protected - it checks to see if
      ! mpi has been initialized before it calls again, so it should
      ! just return w/o complaint.  in theory.

      if (language .eq. ESMF_MAIN_F90) then
          call MPI_Init(status)
      endif

      ! END SECTION
#endif

      ! Routine which interfaces to the C++ creation routine.
      call c_ESMC_MachineInitialize(language, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "Machine initialization error"
        return
      endif

!     set return values
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_MachineInitialize

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_MachineFinalize - Shutdown any hardware specific resources

! !INTERFACE:
      subroutine ESMF_MachineFinalize(rc)
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Shut down any hardware specific resources.
!
!  The arguments are:
!  \begin{description}
! 
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
      
      ! Local Variables
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

      ! Finalize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      ! Routine which interfaces to the C++ routine.
      call c_ESMC_MachineFinalize(status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "Machine finalization error"
        return
      endif

!     set return values
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_MachineFinalize

!------------------------------------------------------------------------------
!BOP
!
! !INTERFACE:
      subroutine ESMF_MachinePrint(options, rc)
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Print contents of a {\tt Machine}.
!
!EOP

!     Local variables.
      character (len=6) :: defaultopts="brief"

      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     ! Interface to call the C++ print code
      if(present(options)) then
           call c_ESMC_MachinePrint(options, status) 
      else
           call c_ESMC_MachinePrint(defaultopts, status) 
      endif

      if (status .ne. ESMF_SUCCESS) then
         print *, "Machine print error"
         return
      endif

!     set return values
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_MachinePrint


      end module ESMF_MachineMod

