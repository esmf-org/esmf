! $Id: ESMF_Init.F90,v 1.58.2.1 2010/02/05 20:04:40 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Init.F90"
!
!     ESMF Init module
      module ESMF_InitMod
!
!==============================================================================
! A blank line to keep protex happy.
!BOP

!EOP
!
! This file contains the Initialize and Finalize code for the Framework.
!
!------------------------------------------------------------------------------
! INCLUDES

#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_InitMod - Framework Initialize and Finalize
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! Framework-wide init and finalize code.
!
!
! !USES: 
      use ESMF_UtilTypesMod
      use ESMF_BaseMod
      use ESMF_IOUtilMod
      use ESMF_IOSpecMod
      use ESMF_LogErrMod
      use ESMF_ConfigMod
      use ESMF_VMMod
      use ESMF_DELayoutMod
      use ESMF_CalendarMod

      implicit none


!------------------------------------------------------------------------------
!     ! Main program source
!     !   ESMF_Initialize is called from what language?
      integer, parameter :: ESMF_MAIN_C=1, ESMF_MAIN_F90=2

!------------------------------------------------------------------------------
!     ! Private global variables

      ! Has framework init routine been run?
      logical, save :: frameworknotinit = .true.

!------------------------------------------------------------------------------
! !PUBLIC SYMBOLS
      public ESMF_MAIN_C, ESMF_MAIN_F90

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_Initialize, ESMF_Finalize
      
      ! should be private to framework - needed by other modules
      public ESMF_FrameworkInternalInit   

!EOPI

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
! 
! ESMF Framework wide initialization routine. Called exactly once per
!  execution by each participating process.
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_Initialize"
!BOP
! !IROUTINE:  ESMF_Initialize - Initialize ESMF
!
! !INTERFACE:
      subroutine ESMF_Initialize(defaultConfigFileName, defaultCalendar, &
        defaultLogFileName, defaultLogType, mpiCommunicator,  &
        IOUnitLower, IOUnitUpper, vm, rc)
!
! !ARGUMENTS:
      character(len=*),        intent(in),  optional :: defaultConfigFileName
      type(ESMF_CalendarType), intent(in),  optional :: defaultCalendar
      character(len=*),        intent(in),  optional :: defaultLogFileName
      type(ESMF_LogType),      intent(in),  optional :: defaultLogType
      integer,                 intent(in),  optional :: mpiCommunicator
      integer,                 intent(in),  optional :: IOUnitLower
      integer,                 intent(in),  optional :: IOUnitUpper
      type(ESMF_VM),           intent(out), optional :: vm
      integer,                 intent(out), optional :: rc

!
! !DESCRIPTION:
!     This method must be called once on each PET before
!     any other ESMF methods are used.  The method contains a
!     barrier before returning, ensuring that all processes
!     made it successfully through initialization.
!
!     Typically {\tt ESMF\_Initialize()} will call {\tt MPI\_Init()} 
!     internally unless MPI has been initialized by the user code before
!     initializing the framework. If the MPI initialization is left to
!     {\tt ESMF\_Initialize()} it inherits all of the MPI implementation 
!     dependent limitations of what may or may not be done before 
!     {\tt MPI\_Init()}. For instance, it is unsafe for some MPI
!     implementations, such as MPICH, to do IO before the MPI environment
!     is initialized. Please consult the documentation of your MPI
!     implementation for details.
!
!     Note that when using MPICH as the MPI library, ESMF needs to use
!     the application command line arguments for {\tt MPI\_Init()}. However,
!     ESMF acquires these arguments internally and the user does not need
!     to worry about providing them. Also, note that ESMF does not alter
!     the command line arguments, so that if the user obtains them they will
!     be as specified on the command line (including those which MPICH would
!     normally strip out). 
!
!     By default, {\tt ESMF\_Initialize()} will open multiple error log files,
!     one per processor.  This is very useful for debugging purpose.  However,
!     when running the application on a large number of processors, opening a
!     large number of log files and writing log messages from all the processors
!     could become a performance bottleneck.  Therefore, it is recommended
!     to turn the Error Log feature off in these situations by setting
!     {\tt defaultLogType} to ESMF\_LOG\_NONE.
!
!     When integrating ESMF with applications where Fortran unit number conflicts
!     exist, the optional {\tt IOUnitLower} and {\tt IOUnitUpper} arguments may be
!     used to specify an alternate unit number range.  See section \ref{fio:unitnumbers}
!     for more information on how ESMF uses Fortran unit numbers.
!
!     Before exiting the application the user must call {\tt ESMF\_Finalize()}
!     to release resources and clean up ESMF gracefully.
!
!     The arguments are:
!     \begin{description}
!     \item [{[defaultConfigFilename]}]
!           Name of the default configuration file for the entire application.
!     \item [{[defaultCalendar]}]
!           Sets the default calendar to be used by ESMF Time Manager.
!           See section \ref{opt:calendartype} for a list of valid options.
!           If not specified, defaults to {\tt ESMF\_CAL\_NOCALENDAR}.
!     \item [{[defaultLogFileName]}]
!           Name of the default log file for warning and error messages.
!           If not specified, defaults to {\tt ESMF\_ErrorLog}.
!     \item [{[defaultLogType]}]
!           Sets the default Log Type to be used by ESMF Log Manager.
!           See section \ref{opt:logtype} for a list of valid options.
!           If not specified, defaults to {\tt ESMF\_LOG\_MULTI}.
!     \item [{[mpiCommunicator]}]
!           MPI communicator defining the group of processes on which the
!           ESMF application is running.
!           If not specified, defaults to {\tt MPI\_COMM\_WORLD}.
!     \item [{[IOUnitLower]}]
!           Lower bound for Fortran unit numbers used within the ESMF library.
!           Fortran units are primarily used for log files.  Legal unit numbers
!           are positive integers.  A value higher than 10 is recommended
!           in order to avoid the compiler-specific
!           reservations which are typically found on the first few units.
!           If not specified, defaults to {\tt ESMF\_LOG\_FORT\_UNIT\_NUMBER},
!           which is distributed with a value of 50.
!     \item [{[IOUnitUpper]}]
!           Upper bound for Fortran unit numbers used within the ESMF library.
!           Must be set to a value at least 5 units higher than {\tt IOUnitLower}.
!           If not specified, defaults to {\tt ESMF\_LOG\_UPPER}, which is
!           distributed with a value of 99.
!     \item [{[vm]}]
!           Returns the global {\tt ESMF\_VM} that was created 
!           during initialization.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!EOP
      integer       :: localrc                        ! local return code
      type(ESMF_VM) :: localvm

      ! assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      
      ! initialize the framework
      call ESMF_FrameworkInternalInit(lang=ESMF_MAIN_F90, &
        defaultConfigFileName=defaultConfigFileName, &
        defaultCalendar=defaultCalendar, defaultLogFileName=defaultLogFileName,&
        defaultLogType=defaultLogType, mpiCommunicator=mpiCommunicator, &
        IOUnitLower=IOUnitLower, IOUnitUpper=IOUnitUpper,  &
        rc=localrc)
                                      
      ! on failure LogErr is not initialized -> explicit print on error
      if (localrc .ne. ESMF_SUCCESS) then
        print *, "Error initializing framework"
        return 
      endif 
      ! on success LogErr is assumed to be functioning
      
      ! obtain global VM
      call ESMF_VMGetGlobal(localvm, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (present(vm)) vm=localvm

      ! block on all PETs
      call ESMF_VMBarrier(localvm, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_Initialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FrameworkInternalInit"
!BOPI
! !IROUTINE:  ESMF_FrameworkInternalInit - internal routine called by both F90 and C++
!
! !INTERFACE:
      subroutine ESMF_FrameworkInternalInit(lang, defaultConfigFileName, &
        defaultCalendar, defaultLogFileName, defaultLogType, &
        mpiCommunicator, IOUnitLower, IOUnitUpper, rc)
!
! !ARGUMENTS:
      integer,                 intent(in)            :: lang     
      character(len=*),        intent(in),  optional :: defaultConfigFileName
      type(ESMF_CalendarType), intent(in),  optional :: defaultCalendar     
      character(len=*),        intent(in),  optional :: defaultLogFileName
      type(ESMF_LogType),      intent(in),  optional :: defaultLogType  
      integer,                 intent(in),  optional :: mpiCommunicator
      integer,                 intent(in),  optional :: IOUnitLower
      integer,                 intent(in),  optional :: IOUnitUpper
      integer,                 intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Initialize the ESMF framework.
!
!     The arguments are:
!     \begin{description}
!     \item [lang]
!           Flag to say whether main program is F90 or C++.  Affects things
!           related to initialization, such as starting MPI.
!     \item [{[defaultConfigFilename]}]
!           Name of the default config file for the entire application.
!     \item [{[defaultCalendar]}]
!           Sets the default calendar to be used by ESMF Time Manager.
!           If not specified, defaults to {\tt ESMF\_CAL\_NOCALENDAR}.
!     \item [{[defaultLogFileName]}]
!           Name of the default log file for warning and error messages.
!           If not specified, defaults to "ESMF_ErrorLog".
!     \item [{[defaultLogType]}]
!           Sets the default Log Type to be used by ESMF Log Manager.
!           If not specified, defaults to "ESMF\_LOG\_MULTI".
!     \item [{[mpiCommunicator]}]
!           MPI communicator defining the group of processes on which the
!           ESMF application is running.
!           If not sepcified, defaults to {\tt MPI\_COMM\_WORLD}
!     \item [{[IOUnitLower]}]
!           Lower bound for Fortran unit numbers used within the ESMF library.
!           Fortran units are primarily used for log files.
!           If not specified, defaults to {\tt ESMF\_LOG\_FORT\_UNIT\_NUMBER}
!     \item [{[IOUnitUpper]}]
!           Upper bound for Fortran unit numbers used within the ESMF library.
!           If not specified, defaults to {\tt ESMF\_LOG\_UPPER}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      logical :: rcpresent                       ! Return code present   
      integer :: status
      logical, save :: already_init = .false.    ! Static, maintains state.
      type(ESMF_LogType) :: defaultLogTypeUse

      ! Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_RC_NOT_IMPL
      endif

      if (already_init) then
          if (rcpresent) rc = ESMF_SUCCESS
          return
      endif

      ! If non-default Fortran unit numbers are to be used, set them
      ! prior to log files being created.

      if (present (IOUnitLower) .or. present (IOUnitUpper)) then
          call ESMF_IOUnitInit (lower=IOUnitLower, upper=IOUnitUpper, rc=status)
          if (status /= ESMF_SUCCESS) then
              if (rcpresent) rc = status
              print *, "Error setting unit number bounds"
              return
          end if
      end if

      ! Initialize the VM. This creates the GlobalVM.
      ! Note that if VMKernel threading is to be used ESMF_VMInitialize() _must_
      ! be called before any other mechanism calls MPI_Init. This is because 
      ! MPI_Init() on some systems will spawn helper threads which might have 
      ! signal handlers installed incompatible with VMKernel. Calling
      ! ESMF_VMInitialize() with and un-initialized MPI will install correct 
      ! signal handlers _before_ possible helper threads are spawned by 
      ! MPI_Init().
      ! If, however, VMKernel threading is not used it is fine to come in with
      ! a user initialized MPI, and thus we support this mode as well!
      call ESMF_VMInitialize(mpiCommunicator=mpiCommunicator, rc=status)
      ! error handling without LogErr because it's not initialized yet
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error initializing VM"
          return
      endif

      ! check defaultLogType in case it is coming across from the C++ side with
      ! an incorrect value
      if (present(defaultLogType)) then
        if (defaultLogType.eq.ESMF_LOG_SINGLE .OR. &
            defaultLogType.eq.ESMF_LOG_MULTI .OR. &
            defaultLogType.eq.ESMF_LOG_NONE) then
          defaultLogTypeUse = defaultLogType
        else
          defaultLogTypeUse = ESMF_LOG_MULTI
        endif
      else
        defaultLogTypeUse = ESMF_LOG_MULTI
      endif

      if (present(defaultLogFileName)) then
         if (len_trim(defaultLogFileName).ne.0) then
           call ESMF_LogInitialize(defaultLogFileName, logType=defaultLogTypeUse, &
                                  rc=status)
         else
           call ESMF_LogInitialize("ESMF_LogFile", logType=defaultLogTypeUse, &
                                     rc=status)
         endif
      else
         call ESMF_LogInitialize("ESMF_LogFile", logType=defaultLogTypeUse, &
                                   rc=status)
      endif
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error initializing the default log/error manager"
          return
      endif

      ! Write our version number out into the log
      call ESMF_LogWrite(&
        "Running with ESMF Version " // ESMF_VERSION_STRING, &
        ESMF_LOG_INFO, rc=status)
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error writing into the default log"
          return
      endif

      ! Initialize the default time manager calendar
      call ESMF_CalendarInitialize(defaultCalendar, status)
      if (status .ne. ESMF_SUCCESS) then
         print *, "Error initializing the default time manager calendar"
      return
      endif

      ! Open config file if specified
      if (present(defaultConfigFileName)) then
         if (len_trim(defaultConfigFileName).ne.0) then
            ! TODO: write this and remove the fixed status= line
            !call ESMF_ConfigInitialize(defaultConfigFileName, status)
            status = ESMF_SUCCESS
            if (status .ne. ESMF_SUCCESS) then
              print *, "Error opening the default config file"
              return
            endif
         endif
      endif

      ! Initialize the machine model, the comms, etc.  Old code, superceeded
      ! by VM code.
      !call ESMF_MachineInitialize(lang, status)
      !if (status .ne. ESMF_SUCCESS) then
      !    print *, "Error initializing the machine characteristics"
      !    return
      !endif

      ! in case we need to know what the language was for main, we have it.
      ! right now we do not make use of it for anything.
      if (lang .eq. ESMF_MAIN_C) then
          continue
      else if (lang .eq. ESMF_MAIN_F90) then
          continue
      else
          continue
      endif

      already_init = .true.

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FrameworkInternalInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_Finalize"
!BOP
! !IROUTINE:  ESMF_Finalize - Clean up and close ESMF
!
! !INTERFACE:
      subroutine ESMF_Finalize(terminationflag, rc)
!
! !ARGUMENTS:
      type(ESMF_TerminationFlag), intent(in), optional  :: terminationflag
      integer, intent(out), optional                    :: rc

!
! !DESCRIPTION:
!     This must be called once on each PET before the application exits
!     to allow ESMF to flush buffers, close open connections, and 
!     release internal resources cleanly. The optional argument 
!     {\tt terminationflag} may be used to indicate the mode of termination.  
!     Note that this call must be issued only once per PET with 
!     {\tt terminationflag=ESMF\_FINAL}, and that this call may not be followed
!     by {\tt ESMF\_Initialize()}.  This last restriction means that it is not
!     possible to restart ESMF within the same execution.
!
!     The arguments are:
!     \begin{description}
!     \item [{[terminationflag]}]
!           Specify mode of termination. The default is {\tt ESMF\_FINAL}
!           which waits for all PETs of the global VM to reach 
!           {\tt ESMF\_Finalize()} before termination. See section 
!           \ref{app:terminationflag} for a complete list and description of
!           valid flags.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      logical :: rcpresent                        ! Return code present
      logical :: abortFlag
      type(ESMF_Logical) :: keepMpiFlag
      integer :: status
      logical, save :: already_final = .false.    ! Static, maintains state.

      ! Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_RC_NOT_IMPL
      endif

      if (already_final) then
          if (rcpresent) rc = ESMF_SUCCESS
          return
      endif

      ! Close the Config file  
      ! TODO: write this routine and remove the status= line
      ! call ESMF_ConfigFinalize(status)
      status = ESMF_SUCCESS
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error finalizing config file"
          return
      endif

      ! Delete any internal built-in time manager calendars
      call ESMF_CalendarFinalize(status)
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error finalizing the time manager calendars"
          return
      endif

      ! Shut down the log file
      call ESMF_LogFinalize(status)
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error finalizing log file"
          return
      endif

      abortFlag = .false.
      keepMpiFlag = ESMF_FALSE
      if (present(terminationflag)) then
        if (terminationflag==ESMF_ABORT) abortFlag = .true.
        if (terminationflag==ESMF_KEEPMPI) keepMpiFlag = ESMF_TRUE
      endif
      
      if (abortFlag) then
        ! Abort the VM
        call ESMF_VMAbort(rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Error aborting VM"
          return
        endif
      else
        ! Finalize the VM
        call ESMF_VMFinalize(keepMpiFlag=keepMpiFlag, rc=status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Error finalizing VM"
          return
        endif
      endif

      already_final = .true.

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_Finalize


      end module ESMF_InitMod
