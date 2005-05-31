! $Id: ESMF_Init.F90,v 1.31 2005/05/31 17:40:02 nscollins Exp $
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
! !IROUTINE:  ESMF_Initialize - Initialize the ESMF
!
! !INTERFACE:
      subroutine ESMF_Initialize(defaultConfigFileName, defaultCalendar, &
                                 defaultLogFileName, defaultLogType, vm, rc)
!
! !ARGUMENTS:
      character(len=*),        intent(in),  optional :: defaultConfigFileName
      type(ESMF_CalendarType), intent(in),  optional :: defaultCalendar  
      character(len=*),        intent(in),  optional :: defaultLogFileName
      type(ESMF_LogType),      intent(in),  optional :: defaultLogType  
      type(ESMF_VM),           intent(out), optional :: vm   
      integer,                 intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Initialize the ESMF.  This method must be called before
!     any other ESMF methods are used.  The method contains a
!     barrier before returning, ensuring that all processes
!     made it successfully through initialization.
!     Before exiting the application
!     the user must call {\tt ESMF\_Finalize()} to release resources 
!     and clean up the ESMF gracefully.
!
!     The arguments are:
!     \begin{description}
!     \item [{[defaultConfigFilename]}]
!           Name of the default configuration file for the entire application.
!     \item [{[defaultCalendar]}]
!           Sets the default calendar to be used by ESMF Time Manager.
!           If not specified, defaults to {\tt ESMF\_CAL\_NOCALENDAR}.
!     \item [{[defaultLogFileName]}]
!           Name of the default log file for warning and error messages.
!           If not specified, defaults to {\tt ESMF\_ErrorLog}.
!     \item [{[defaultLogType]}]
!           Sets the default Log Type to be used by ESMF Log Manager.
!     \item [{[vm]}]
!           Returns the global {\tt ESMF\_VM} that was created 
!           during initialization.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
      
      type(ESMF_VM):: localvm

      call ESMF_FrameworkInternalInit(ESMF_MAIN_F90, defaultConfigFileName, &
                                      defaultCalendar, defaultLogFileName, &
                                      defaultLogType, rc)
      call ESMF_VMGetGlobal(localvm, rc)
      if (present(vm)) vm = localvm

      call ESMF_VMBarrier(localvm, rc)

      end subroutine ESMF_Initialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FrameworkInternalInit"
!BOPI
! !IROUTINE:  ESMF_FrameworkInternalInit - internal routine called by both F90 and C++
!
! !INTERFACE:
      subroutine ESMF_FrameworkInternalInit(lang, defaultConfigFileName, &
                                       defaultCalendar, defaultLogFileName, &
                                       defaultLogType, rc)
!
! !ARGUMENTS:
      integer,                 intent(in)            :: lang     
      character(len=*),        intent(in),  optional :: defaultConfigFileName
      type(ESMF_CalendarType), intent(in),  optional :: defaultCalendar     
      character(len=*),        intent(in),  optional :: defaultLogFileName
      type(ESMF_LogType),      intent(in),  optional :: defaultLogType  
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
!           If not specified, defaults to "ESMF_LOG_SINGLE".
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
        rc = ESMF_FAILURE
      endif

      if (already_init) then
          if (rcpresent) rc = ESMF_SUCCESS
          return
      endif

      ! Initialize the VM. This creates the GlobalVM.
      ! Note that ESMF_VMInitialize _must_ be called before any other
      ! mechanism calls MPI_Init. This is because MPI_Init on some systems
      ! will spawn helper threads which might have signal handlers installed
      ! incompatible with vmachine. ESMF_VMInitialize must install correct
      ! signal handlers _before_ possible helper threads are spawned by 
      ! MPI_Init.
      call ESMF_VMInitialize(status);
      if (status .ne. ESMF_SUCCESS) then
          print *, "Error initializing VM"
          return
      endif

      ! check defaultLogType in case it is coming across from the C++ side with
      ! an incorrect value
      if (present(defaultLogType)) then
        if (defaultLogType.eq.ESMF_LOG_SINGLE .OR. &
            defaultLogType.eq.ESMF_LOG_MULTI) then
          defaultLogTypeUse = defaultLogType
        else
          defaultLogTypeUse = ESMF_LOG_SINGLE
        endif
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
      call ESMF_LogWrite("Running with ESMF Version " // ESMF_VERSION_STRING, &
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

      already_init = .true.

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FrameworkInternalInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_Finalize"
!BOP
! !IROUTINE:  ESMF_Finalize - Clean up and close the ESMF
!
! !INTERFACE:
      subroutine ESMF_Finalize(terminationflag, rc)
!
! !ARGUMENTS:
      type(ESMF_TerminationFlag), intent(in), optional  :: terminationflag
      integer, intent(out), optional                    :: rc

!
! !DESCRIPTION:
!     Finalize the ESMF.  This must be called before the application exits
!     to allow the ESMF to flush buffers, close open connections, and 
!     release internal resources cleanly. The optional argument 
!     {\tt terminationflag} may be used to indicate the mode of termination.
!
!     The arguments are:
!     \begin{description}
!     \item [{[terminationflag]}]
!           Specify mode of termination. The default is {\tt ESMF\_FINAL}
!           which waits for all PETs of the global VM to reach 
!           {\tt ESMF\_Finalize()} before termination. See section 
!           \ref{app:terminationflag} for a complete list of valid flags.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      logical :: rcpresent                        ! Return code present
      logical :: abortFlag
      integer :: status
      logical, save :: already_final = .false.    ! Static, maintains state.

      ! Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
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
      if (present(terminationflag)) then
        if (terminationflag==ESMF_ABORT) abortFlag = .true.
      endif
      
      if (abortFlag) then
        ! Abort the VM
        call ESMF_VMAbort(status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Error aborting VM"
          return
        endif
      else
        ! Finalize the VM
        call ESMF_VMFinalize(status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Error finalizing VM"
          return
        endif
      endif

      already_final = .true.

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_Finalize


      end module ESMF_InitMod
