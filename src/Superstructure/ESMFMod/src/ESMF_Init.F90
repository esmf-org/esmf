! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
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
      use ESMF_LogErrMod
      use ESMF_ConfigMod
      use ESMF_VMMod
      use ESMF_DELayoutMod
      use ESMF_CalendarMod
      use ESMF_TraceMod

      implicit none
      private

!------------------------------------------------------------------------------
!     ! Main program source
!     !   ESMF_Initialize is called from what language?
      integer, parameter :: ESMF_MAIN_C=1, ESMF_MAIN_F90=2

!------------------------------------------------------------------------------
! !PUBLIC SYMBOLS
      public ESMF_MAIN_C, ESMF_MAIN_F90

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_Initialize, ESMF_Finalize
      public ESMF_IsInitialized, ESMF_IsFinalized
                  
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
      subroutine ESMF_Initialize(keywordEnforcer, defaultConfigFileName, defaultCalKind, &
        defaultLogFileName, logappendflag, logkindflag, mpiCommunicator,  &
        ioUnitLBound, ioUnitUBound, vm, rc)
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*),        intent(in),  optional :: defaultConfigFileName
      type(ESMF_CalKind_Flag), intent(in),  optional :: defaultCalKind
      character(len=*),        intent(in),  optional :: defaultLogFileName
      logical,                 intent(in),  optional :: logappendflag
      type(ESMF_LogKind_Flag), intent(in),  optional :: logkindflag
      integer,                 intent(in),  optional :: mpiCommunicator
      integer,                 intent(in),  optional :: ioUnitLBound
      integer,                 intent(in),  optional :: ioUnitUBound
      type(ESMF_VM),           intent(out), optional :: vm
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.0.0] Added argument {\tt logappendflag} to allow specifying that the existing
!              log files will be overwritten.\newline
! \end{description}
! \end{itemize}
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
!     implementations, such as MPICH, to do I/O before the MPI environment
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
!     {\tt logkindflag} to ESMF\_LOGKIND\_NONE.
!
!     When integrating ESMF with applications where Fortran unit number conflicts
!     exist, the optional {\tt ioUnitLBound} and {\tt ioUnitUBound} arguments may be
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
!     \item [{[defaultCalKind]}]
!           Sets the default calendar to be used by ESMF Time Manager.
!           See section \ref{const:calkindflag} for a list of valid options.
!           If not specified, defaults to {\tt ESMF\_CALKIND\_NOCALENDAR}.
!     \item [{[defaultLogFileName]}]
!           Name of the default log file for warning and error messages.
!           If not specified, defaults to {\tt ESMF\_ErrorLog}.
!     \item [{[logappendflag]}]
!           If the default log file already exists, a value of {\tt .false.}
!           will set the file position to the beginning of the file.  A value
!           of {\tt .true.} sets the position to the end of the file.
!           If not specified, defaults to {\tt .true.}.
!     \item [{[logkindflag]}]
!           Sets the default Log Type to be used by ESMF Log Manager.
!           See section \ref{const:logkindflag} for a list of valid options.
!           If not specified, defaults to {\tt ESMF\_LOGKIND\_MULTI}.
!     \item [{[mpiCommunicator]}]
!           MPI communicator defining the group of processes on which the
!           ESMF application is running.
!           If not specified, defaults to {\tt MPI\_COMM\_WORLD}.
!     \item [{[ioUnitLBound]}]
!           Lower bound for Fortran unit numbers used within the ESMF library.
!           Fortran units are primarily used for log files.  Legal unit numbers
!           are positive integers.  A value higher than 10 is recommended
!           in order to avoid the compiler-specific
!           reservations which are typically found on the first few units.
!           If not specified, defaults to {\tt ESMF\_LOG\_FORT\_UNIT\_NUMBER},
!           which is distributed with a value of 50.
!     \item [{[ioUnitUBound]}]
!           Upper bound for Fortran unit numbers used within the ESMF library.
!           Must be set to a value at least 5 units higher than {\tt ioUnitLBound}.
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
      type(ESMF_Logical) :: logappendflag_local

      ! assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      logappendflag_local = ESMF_TRUE
      if (present (logappendflag)) then
        logappendflag_local = logappendflag
      end if

      ! initialize the framework
      call ESMF_FrameworkInternalInit(lang=ESMF_MAIN_F90, &
        defaultConfigFileName=defaultConfigFileName, defaultCalKind=defaultCalKind,  &
        defaultLogFileName=defaultLogFileName, logappendflag=logappendflag_local,  &
        logkindflag=logkindflag, mpiCommunicator=mpiCommunicator, &
        ioUnitLBound=ioUnitLBound, ioUnitUBound=ioUnitUBound,  &
        rc=localrc)
                                      
      ! on failure LogErr is not initialized -> explicit print on error
      if (localrc .ne. ESMF_SUCCESS) then
        write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error initializing framework"
        return 
      endif 
      ! on success LogErr is assumed to be functioning
      
      ! obtain global VM
      call ESMF_VMGetGlobal(localvm, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (present(vm)) vm=localvm

      ! block on all PETs
      call ESMF_VMBarrier(localvm, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_Initialize
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FrameworkInternalInit"
!BOPI
! !IROUTINE:  ESMF_FrameworkInternalInit - internal routine called by both F90 and C++
!
! !INTERFACE:
      subroutine ESMF_FrameworkInternalInit(lang, defaultConfigFileName, &
        defaultCalKind, defaultLogFileName, logappendflag, logkindflag, &
        mpiCommunicator, ioUnitLBound, ioUnitUBound, rc)
!
! !ARGUMENTS:
      integer,                 intent(in)            :: lang     
      character(len=*),        intent(in),  optional :: defaultConfigFileName
      type(ESMF_CalKind_Flag), intent(in),  optional :: defaultCalKind     
      character(len=*),        intent(in),  optional :: defaultLogFileName
      type(ESMF_Logical),      intent(in),  optional :: logappendflag
      type(ESMF_LogKind_Flag), intent(in),  optional :: logkindflag  
      integer,                 intent(in),  optional :: mpiCommunicator
      integer,                 intent(in),  optional :: ioUnitLBound
      integer,                 intent(in),  optional :: ioUnitUBound
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
!     \item [{[defaultCalKind]}]
!           Sets the default calendar to be used by ESMF Time Manager.
!           If not specified, defaults to {\tt ESMF\_CALKIND\_NOCALENDAR}.
!     \item [{[defaultLogFileName]}]
!           Name of the default log file for warning and error messages.
!           If not specified, defaults to "ESMF_ErrorLog".
!     \item [{[logappendflag]}]
!           If the default log file already exists, a value of {\tt .false.}
!           will set the file position to the beginning of the file.  A value
!           of [\tt .true.} sets the position to the end of the file.
!           If not specified, defaults to {\tt .true.}.
!     \item [{[logkindflag]}]
!           Sets the default Log Type to be used by ESMF Log Manager.
!           If not specified, defaults to "ESMF\_LOGKIND\_MULTI".
!     \item [{[mpiCommunicator]}]
!           MPI communicator defining the group of processes on which the
!           ESMF application is running.
!           If not sepcified, defaults to {\tt MPI\_COMM\_WORLD}
!     \item [{[ioUnitLBound]}]
!           Lower bound for Fortran unit numbers used within the ESMF library.
!           Fortran units are primarily used for log files.
!           If not specified, defaults to {\tt ESMF\_LOG\_FORT\_UNIT\_NUMBER}
!     \item [{[ioUnitUBound]}]
!           Upper bound for Fortran unit numbers used within the ESMF library.
!           If not specified, defaults to {\tt ESMF\_LOG\_UPPER}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      logical :: rcpresent                       ! Return code present   
      integer :: localrc
      logical, save :: already_init = .false.    ! Static, maintains state.
      logical :: logappendflag_local
      type(ESMF_LogKind_Flag) :: logkindflagUse
      logical :: openflag
      integer :: complianceCheckIsOn
      integer :: traceIsOn
      type(ESMF_VM) :: vm
      integer :: localPet

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

      if (present (ioUnitLBound) .or. present (ioUnitUBound)) then
          call ESMF_UtilIOUnitInit (lower=ioUnitLBound, upper=ioUnitUBound, rc=localrc)
          if (localrc /= ESMF_SUCCESS) then
              if (rcpresent) rc = localrc
              write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error setting unit number bounds"
              return
          end if
      end if

      ! Some compiler RTLs have a problem with flushing the unit used by
      ! various ESMF Print routines when nothing has been written on the unit.
      ! Intel 10.1.021 is an example, though the problem is fixed in later
      ! releases.  Doing an inquire up front avoids the problem.

      inquire (ESMF_UtilIOStdin,  opened=openflag)
      inquire (ESMF_UtilIOStdout, opened=openflag)
      inquire (ESMF_UtilIOStderr, opened=openflag)

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
      call ESMF_VMInitialize(mpiCommunicator=mpiCommunicator, rc=localrc)
      ! error handling without LogErr because it's not initialized yet
      if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error initializing VM"
          return
      endif

      ! check logkindflag in case it is coming across from the C++ side with
      ! an incorrect value
      if (present(logkindflag)) then
        if (logkindflag == ESMF_LOGKIND_SINGLE .or. &
            logkindflag == ESMF_LOGKIND_MULTI .or. &
            logkindflag == ESMF_LOGKIND_MULTI_ON_ERROR .or.  &
            logkindflag == ESMF_LOGKIND_NONE) then
          logkindflagUse = logkindflag
        else
          logkindflagUse = ESMF_LOGKIND_MULTI
        endif
      else
        logkindflagUse = ESMF_LOGKIND_MULTI
      endif

      logappendflag_local = .false.
      if (present (logappendflag)) then
        logappendflag_local = logappendflag
      end if

      if (present(defaultLogFileName)) then
         if (len_trim(defaultLogFileName).ne.0) then
           call ESMF_LogInitialize(defaultLogFileName,  &
               logappendflag=logappendflag_local, logkindflag=logkindflagUse, &
               rc=localrc)
         else
           call ESMF_LogInitialize("ESMF_LogFile",  &
               logappendflag=logappendflag_local, logkindflag=logkindflagUse, &
               rc=localrc)
         endif
      else
         call ESMF_LogInitialize("ESMF_LogFile",  &
               logappendflag=logappendflag_local, logkindflag=logkindflagUse, &
               rc=localrc)
      endif
      if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error initializing the default log/error manager"
          return
      endif
      
      ! Write our version number out into the log
      call ESMF_LogWrite(&
           "Running with ESMF Version " // ESMF_VERSION_STRING, &
           ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
         return
      endif

      ! Ensure that at least the version number makes it into the log
      call ESMF_LogFlush(rc=localrc)
      
      ! if compliance checker is on, we want logs to have high prescision timestamps
      call c_esmc_getComplianceCheckJSON(complianceCheckIsOn, localrc)
      if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error checking ESMF_RUNTIME_COMPLIANCECHECK env variable"
          return
      endif
      if (complianceCheckIsOn == 1) then
        call ESMF_LogSet(highResTimestampFlag=.true., rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error setting default log option: highResTimestampFlag"
          return
        endif
      endif

      ! check if tracing is on
      call c_esmc_getComplianceCheckTrace(traceIsOn, localrc)
      if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error checking ESMF_RUNTIME_COMPLIANCECHECK env variable"
          return
      endif
      if (traceIsOn == 1) then
         call ESMF_TraceOpen("./traceout", rc=localrc)
         if (localrc /= ESMF_SUCCESS) then
            write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error initializing trace stream"
            return
         endif
      endif

      ! Initialize the default time manager calendar
      call ESMF_CalendarInitialize(calkindflag=defaultCalKind, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
             ": Error initializing the default time manager calendar"
      return
      endif

      ! Open config file if specified
      if (present(defaultConfigFileName)) then
         if (len_trim(defaultConfigFileName).ne.0) then
            ! TODO: write this and remove the fixed status= line
            !call ESMF_ConfigInitialize(defaultConfigFileName, status)
            localrc = ESMF_SUCCESS
            if (localrc /= ESMF_SUCCESS) then
              write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
                  ": Error opening the default config file"
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

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IsInitialized"
!BOP
! !IROUTINE:  ESMF_IsInitialized - Query Initialized status of ESMF
!
! !INTERFACE:
    function ESMF_IsInitialized(keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_IsInitialized
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                 intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns {\tt .true.} if the framework has been initialized. This means 
!     that {\tt ESMF\_Initialize()} has been called. Otherwise returns
!     {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!     returned, the return value of the function will also be {\tt .false.}.
!
!     The arguments are:
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!EOP
!------------------------------------------------------------------------------
      integer             :: localrc                        ! local return code
      type(ESMF_Logical)  :: flag
      
      ESMF_IsInitialized = .false.   ! default

      call c_ESMC_IsInitialized(flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
      ESMF_IsInitialized = flag
      
      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS
      
    end function ESMF_IsInitialized
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IsFinalized"
!BOP
! !IROUTINE:  ESMF_IsFinalized - Query Finalized status of ESMF
!
! !INTERFACE:
    function ESMF_IsFinalized(keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_IsFinalized
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                 intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns {\tt .true.} if the framework has been finalized. This means 
!     that {\tt ESMF\_Finalize()} has been called. Otherwise returns
!     {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!     returned, the return value of the function will also be {\tt .false.}.
!
!     The arguments are:
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!EOP
!------------------------------------------------------------------------------
      integer             :: localrc                        ! local return code
      type(ESMF_Logical)  :: flag
      
      ESMF_IsFinalized = .false.   ! default

      call c_ESMC_IsFinalized(flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
      ESMF_IsFinalized = flag
      
      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS
      
    end function ESMF_IsFinalized
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_Finalize"
!BOP
! !IROUTINE:  ESMF_Finalize - Clean up and shut down ESMF
!
! !INTERFACE:
      subroutine ESMF_Finalize(keywordEnforcer, endflag, rc)
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_End_Flag), intent(in), optional  :: endflag
      integer,             intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     This must be called once on each PET before the application exits
!     to allow ESMF to flush buffers, close open connections, and 
!     release internal resources cleanly. The optional argument 
!     {\tt endflag} may be used to indicate the mode of termination.  
!     Note that this call must be issued only once per PET with 
!     {\tt endflag=ESMF\_END\_NORMAL}, and that this call may not be followed
!     by {\tt ESMF\_Initialize()}.  This last restriction means that it is not
!     possible to restart ESMF within the same execution.
!
!     The arguments are:
!     \begin{description}
!     \item [{[endflag]}]
!           Specify mode of termination. The default is {\tt ESMF\_END\_NORMAL}
!           which waits for all PETs of the global VM to reach 
!           {\tt ESMF\_Finalize()} before termination. See section 
!           \ref{const:endflag} for a complete list and description of
!           valid flags.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      logical :: rcpresent                        ! Return code present
      logical :: abortFlag
      type(ESMF_Logical) :: keepMpiFlag
      integer :: localrc
      character(ESMF_MAXSTR) :: errmsg
      integer :: errmsg_l
      logical, save :: already_final = .false.    ! Static, maintains state.

      logical, parameter :: trace = .false.
      integer :: traceIsOn

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

      ! Write final message to the log
      call ESMF_LogWrite("Finalizing ESMF", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
      endif

      call c_esmc_getComplianceCheckTrace(traceIsOn, localrc)
      if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error checking ESMF_RUNTIME_COMPLIANCECHECK env variable"
          return
      endif
      if (traceIsOn == 1) then
        call ESMF_TraceClose()
        if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error closing trace stream"
          return
        endif
      endif



      ! Close the Config file  
      ! TODO: write this routine and remove the status= line
      ! call ESMF_ConfigFinalize(localrc)
      localrc = ESMF_SUCCESS
      if (localrc /= ESMF_SUCCESS) then
          call ESMF_LogRc2Msg (localrc, errmsg, errmsg_l)
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": Error finalizing config file: ", errmsg(:errmsg_l)
          return
      endif

      ! Delete any internal built-in time manager calendars
      if (trace) write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
          ': Finalizing Calendar (if needed)'
      call ESMF_CalendarFinalize(rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
          call ESMF_LogRc2Msg (localrc, errmsg, errmsg_l)
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": Error finalizing the time manager calendars"
          return
      endif

      ! Flush log to avoid lost messages
      call ESMF_LogFlush (rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
          call ESMF_LogRc2Msg (localrc, errmsg, errmsg_l)
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": Error flushing log file: ", errmsg(:errmsg_l)
      end if

      abortFlag = .false.
      keepMpiFlag = ESMF_FALSE
      if (present(endflag)) then
        if (endflag==ESMF_END_ABORT) abortFlag = .true.
        if (endflag==ESMF_END_KEEPMPI) keepMpiFlag = ESMF_TRUE
      endif
      
      if (abortFlag) then
        ! Abort the VM
      if (trace) write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
          ': calling ESMF_VMAbort'
        call ESMF_VMAbort(rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          call ESMF_LogRc2Msg (localrc, errmsg, errmsg_l)
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": Error aborting VM: ", errmsg(:errmsg_l)
          return
        endif
      else
        ! Finalize the VM
      if (trace) write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
          ': calling ESMF_VMFinalize'
        call ESMF_VMFinalize(keepMpiFlag=keepMpiFlag, rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          call ESMF_LogRc2Msg (localrc, errmsg, errmsg_l)
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": Error finalizing VM: ", errmsg(:errmsg_l)
          return
        endif
      endif

      ! Shut down the log file
      if (trace) write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
          ': Finalizing Log'
      call ESMF_LogFinalize(localrc)
      if (localrc /= ESMF_SUCCESS) then
          call ESMF_LogRc2Msg (localrc, errmsg, errmsg_l)
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": Error finalizing log file: ", errmsg(:errmsg_l)
          return
      endif

      already_final = .true.

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_Finalize
!------------------------------------------------------------------------------

end module ESMF_InitMod
