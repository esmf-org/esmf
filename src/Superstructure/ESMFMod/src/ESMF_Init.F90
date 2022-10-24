! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research, 
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
      use ESMF_UtilMod

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

      public ESMF_Initialize, ESMF_InitializePreMPI, ESMF_Finalize
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
      subroutine ESMF_Initialize(keywordEnforcer, configFilename, &
        defaultCalKind, defaultDefaultLogFilename, defaultLogFilename, &
        defaultLogAppendFlag, logAppendFlag, defaultLogKindFlag, logKindFlag, &
        mpiCommunicator,  ioUnitLBound, ioUnitUBound, &
        defaultGlobalResourceControl, globalResourceControl, config, vm, rc)
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character(len=*),        intent(in),  optional :: configFilename
      type(ESMF_CalKind_Flag), intent(in),  optional :: defaultCalKind
      character(len=*),        intent(in),  optional :: defaultDefaultLogFilename
      character(len=*),        intent(in),  optional :: defaultLogFilename
      logical,                 intent(in),  optional :: defaultLogAppendFlag
      logical,                 intent(in),  optional :: logAppendFlag
      type(ESMF_LogKind_Flag), intent(in),  optional :: defaultLogKindFlag
      type(ESMF_LogKind_Flag), intent(in),  optional :: logKindFlag
      integer,                 intent(in),  optional :: mpiCommunicator
      integer,                 intent(in),  optional :: ioUnitLBound
      integer,                 intent(in),  optional :: ioUnitUBound
      logical,                 intent(in),  optional :: defaultGlobalResourceControl
      logical,                 intent(in),  optional :: globalResourceControl
      type(ESMF_Config),       intent(out), optional :: config
      type(ESMF_VM),           intent(out), optional :: vm
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.0.0] Added argument {\tt logAppendFlag} to allow specifying that the existing
!              log files will be overwritten.
! \item[8.2.0] Added argument {\tt globalResourceControl} to support ESMF-aware
!              threading and resource control on the global VM level.\newline
!              Added argument {\tt config} to return default handle to the
!              defaultConfig.\newline
!              Renamed argument {\tt defaultConfigFilename} to
!              {\tt configFilename}, in order to clarify that provided settings
!              in the Config file are {\em not} defaults, but final
!              overrides.\newline
!              Introduce {\tt default} prefixed arguments:
!              {\tt defaultDefaultLogFilename},
!              {\tt defaultLogAppendFlag}, {\tt defaultLogKindFlag},
!              {\tt defaultGlobalResourceControl}. These arguments allow
!              specification of defaults for the associated settings. This
!              default can be overridden via the associated argument, without
!              the extra {\tt default} prefix, either specified in the call, or
!              within the specified Config file.
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
!     implementations, such as MPICH1, to do I/O before the MPI environment
!     is initialized. Please consult the documentation of your MPI
!     implementation for details.
!
!     Note that when using MPICH1 as the MPI library, ESMF needs to use
!     the application command line arguments for {\tt MPI\_Init()}. However,
!     ESMF acquires these arguments internally and the user does not need
!     to worry about providing them. Also, note that ESMF does not alter
!     the command line arguments, so that if the user obtains them they will
!     be as specified on the command line (including those which MPICH1 would
!     normally strip out).
!
!     {\tt ESMF\_Initialize()} supports running ESMF inside a user MPI program.
!     Details of this feature are discussed under the VM example 
!     \ref{vm_inside_user_mpi}. It is not necessary that all MPI ranks are
!     handed to ESMF. Section \ref{vm_nesting_esmf} shows how an MPI
!     communicator can be used to execute ESMF on a subset of MPI ranks.
!     {\tt ESMF\_Initialize()} supports running multiple concurrent
!     instances of ESMF under the same user MPI program. This feature is
!     discussed under \ref{vm_multi_instance_esmf}.
!
!     In order to use any of the advanced resource management functions that
!     ESMF provides via the {\tt ESMF\_*CompSetVM*()} methods, the MPI
!     environment must be thread-safe. {\tt ESMF\_Initialize()} handles this
!     automatically if it is in charge of initializing MPI. However, if the
!     user code initializes MPI before calling into {\tt ESMF\_Initialize()},
!     it must do so via {\tt MPI\_Init\_thread()}, specifying
!     {\tt MPI\_THREAD\_SERIALIZED} or above for the required level of thread
!     support.
!
!     In cases where {\tt ESMF\_*CompSetVM*()} methods are used to move
!     processing elements (PEs), i.e. CPU cores, between persistent execution
!     threads (PETs), ESMF uses POSIX signals between PETs. In order to do so
!     safely, the proper signal handlers must be installed {\em before} MPI is
!     initialized. {\tt ESMF\_Initialize()} handles this automatically if it is
!     in charge of initializing MPI. If, however, MPI is explicitly initialized
!     by user code, then to ensure correct signal handling it is necessary to
!     call {\tt ESMF\_InitializePreMPI()} from the user code prior to the MPI
!     initialization.
!
!     By default, {\tt ESMF\_Initialize()} will open multiple error log files,
!     one per processor.  This is very useful for debugging purpose.  However,
!     when running the application on a large number of processors, opening a
!     large number of log files and writing log messages from all the processors
!     could become a performance bottleneck.  Therefore, it is recommended
!     to turn the Error Log feature off in these situations by setting
!     {\tt logKindFlag} to ESMF\_LOGKIND\_NONE.
!
!     When integrating ESMF with applications where Fortran unit number conflicts
!     exist, the optional {\tt ioUnitLBound} and {\tt ioUnitUBound} arguments may be
!     used to specify an alternate unit number range.  See section \ref{fio:unitnumbers}
!     for more information on how ESMF uses Fortran unit numbers.
!
!     Before exiting the application the user must call {\tt ESMF\_Finalize()}
!     to release resources and clean up ESMF gracefully. See the
!     {\tt ESMF\_Finalize()} documentation about details relating to the MPI
!     environment.
!
!     The arguments are:
!     \begin{description}
!     \item [{[configFilename]}]
!           Name of the configuration file for the entire application.
!           If this argument is specified, the configuration file must exist,
!           and its content is read during {\tt ESMF\_Initialize()}.
!           If any of the following labels are found in the specified
!           configuration file, their values are used to set the associated
!           {\tt ESMF\_Initialize()} argument, overriding any defaults.
!           If the same argument is also specified in the
!           {\tt ESMF\_Initialize()} call directly, an error is returned,
!           and ESMF is not initialized.
!           The supported config labels are:
!           \begin{itemize}
!              \item {\tt defaultLogFilename:}
!              \item {\tt logAppendFlag:}
!              \item {\tt logKindFlag:}
!              \item {\tt globalResourceControl:}
!           \end{itemize}
!     \item [{[defaultCalKind]}]
!           Sets the default calendar to be used by ESMF Time Manager.
!           See section \ref{const:calkindflag} for a list of valid options.
!           If not specified, defaults to {\tt ESMF\_CALKIND\_NOCALENDAR}.
!     \item [{[defaultDefaultLogFilename]}]
!           Default value for argument {\tt defaultLogFilename}, the name of
!           the default log file for warning and error messages.
!           If not specified, the default is {\tt ESMF\_LogFile}.
!     \item [{[defaultLogFilename]}]
!           Name of the default log file for warning and error messages.
!           If not specified,
!           defaults according to {\tt defaultDefaultLogFilename}.
!     \item [{[defaultLogAppendFlag]}]
!           Default value for argument {\tt logAppendFlag}, indicating the
!           overwrite behavior in case the default log file already exists.
!           If not specified, the default is {\tt .true.}.
!     \item [{[logAppendFlag]}]
!           If the default log file already exists, a value of {\tt .false.}
!           will set the file position to the beginning of the file.  A value
!           of {\tt .true.} sets the position to the end of the file.
!           If not specified,
!           defaults according to {\tt defaultLogAppendFlag}.
!     \item [{[defaultLogKindFlag]}]
!           Default value for argument {\tt logKindFlag}, setting the LogKind
!           of the default ESMF log.
!           If not specified, the default is {\tt ESMF\_LOGKIND\_MULTI}.
!     \item [{[logKindFlag]}]
!           Sets the LogKind of the default ESMF log. See section
!           \ref{const:logkindflag} for a list of valid options.
!           If not specified,
!           defaults according to {\tt defaultLogKindFlag}.
!     \item [{[mpiCommunicator]}]
!           MPI communicator defining the group of processes on which the
!           ESMF application is running.
!           See section \ref{vm_nesting_esmf} and \ref{vm_multi_instance_esmf}
!           for details.
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
!     \item [{[defaultGlobalResourceControl]}]
!           Default value for argument {\tt globalResourceControl}, indicating
!           whether PETs of the global VM are pinned to PEs and the OpenMP
!           threading level is reset.
!           If not specified, the default is {\tt .false.}.
!     \item [{[globalResourceControl]}]
!           For {\tt .true.}, each global PET is pinned to the corresponding
!           PE (i.e. CPU core) in order. Further, if OpenMP support is enabled
!           for the ESMF installation (during build time), the
!           {\tt OMP\_NUM\_THREADS} is set to {\tt 1} on every PET, regardless
!           of the setting in the launching environment. The {\tt .true.}
!           setting is recommended for applications that utilize the ESMF-aware
!           threading and resource control features.
!           For {\tt .false.}, global PETs are {\em not} pinned by ESMF, and
!           {\tt OMP\_NUM\_THREADS} is {\em not} modified.
!           If not specified,
!           defaults according to {\tt defaultGlobalResourceControl}.
!     \item [{[config]}]
!           Returns the default {\tt ESMF\_Config} if the
!           {\tt configFilename} argument was provided. Otherwise the
!           presence of this argument triggers an error.
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
        configFilename=configFilename, defaultCalKind=defaultCalKind, &
        defaultDefaultLogFilename=defaultDefaultLogFilename, &
        defaultLogFilename=defaultLogFilename, &
        defaultLogAppendFlag=defaultLogAppendFlag, &
        logAppendFlag=logAppendFlag, &
        defaultLogKindFlag=defaultLogKindFlag, logKindFlag=logKindFlag, &
        mpiCommunicator=mpiCommunicator, &
        ioUnitLBound=ioUnitLBound, ioUnitUBound=ioUnitUBound, &
        defaultGlobalResourceControl=defaultGlobalResourceControl, &
        globalResourceControl=globalResourceControl, &
        config=config, rc=localrc)
                                      
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
#define ESMF_METHOD "ESMF_InitializePreMPI"
!BOP
! !IROUTINE:  ESMF_InitializePreMPI - Initialize parts of ESMF that must happen before MPI is initialized
!
! !INTERFACE:
      subroutine ESMF_InitializePreMPI(keywordEnforcer, rc)
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                 intent(out), optional :: rc

!
! !DESCRIPTION:
!     This method is {\em only} needed for cases where MPI is initialized
!     explicitly by user code. In most typical cases {\tt ESMF\_Initialize()}
!     is called before MPI is initialized, and takes care of all the internal
!     initialization, including MPI.
!
!     There are circumstances where it is necessary or convenient to
!     initialize MPI before calling into {\tt ESMF\_Initialize()}. This option
!     is supported by ESMF, and for most cases no special action is required
!     on the user side. However, for cases where {\tt ESMF\_*CompSetVM*()}
!     methods are used to move processing elements (PEs), i.e. CPU cores,
!     between persistent execution threads (PETs), ESMF uses POSIX signals
!     between PETs. In order to do so safely, the proper signal handlers must
!     be installed before MPI is initialized. This is accomplished by calling
!     {\tt ESMF\_InitializePreMPI()} from the user code prior to the MPI
!     initialization.
!
!     Note also that in order to use any of the advanced resource management
!     functions that ESMF provides via the {\tt ESMF\_*CompSetVM*()} methods,
!     the MPI environment must be thread-safe. {\tt ESMF\_Initialize()} handles
!     this automatically if it is in charge of initializing MPI. However, if the
!     user code initializes MPI before calling into {\tt ESMF\_Initialize()},
!     it must do so via {\tt MPI\_Init\_thread()}, specifying
!     {\tt MPI\_THREAD\_SERIALIZED} or above for the required level of thread
!     support.
!
!     The arguments are:
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!EOP
      integer       :: localrc                        ! local return code

      ! assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! initialize pre MPI parts of global VM
      call ESMF_VMInitializePreMPI(rc=localrc)
                                      
      ! on failure LogErr is not initialized -> explicit print on error
      if (localrc .ne. ESMF_SUCCESS) then
        write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error initializing framework"
        return 
      endif 
      ! on success LogErr is assumed to be functioning
      
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_InitializePreMPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FrameworkInternalInit"
!BOPI
! !IROUTINE:  ESMF_FrameworkInternalInit - internal routine called by both F90 and C++
!
! !INTERFACE:
      subroutine ESMF_FrameworkInternalInit(lang, configFilename, &
        defaultCalKind, defaultDefaultLogFilename, defaultLogFilename, &
        defaultLogAppendFlag, logAppendFlag, defaultLogKindFlag, logKindFlag, &
        mpiCommunicator, ioUnitLBound, ioUnitUBound, &
        defaultGlobalResourceControl, globalResourceControl, config, rc)
!
! !ARGUMENTS:
      integer,                 intent(in)            :: lang
      character(len=*),        intent(in),  optional :: configFilename
      type(ESMF_CalKind_Flag), intent(in),  optional :: defaultCalKind
      character(len=*),        intent(in),  optional :: defaultDefaultLogFilename
      character(len=*),        intent(in),  optional :: defaultLogFilename
      logical,                 intent(in),  optional :: defaultLogAppendFlag
      logical,                 intent(in),  optional :: logAppendFlag
      type(ESMF_LogKind_Flag), intent(in),  optional :: defaultLogKindFlag
      type(ESMF_LogKind_Flag), intent(in),  optional :: logKindFlag
      integer,                 intent(in),  optional :: mpiCommunicator
      integer,                 intent(in),  optional :: ioUnitLBound
      integer,                 intent(in),  optional :: ioUnitUBound
      logical,                 intent(in),  optional :: defaultGlobalResourceControl
      logical,                 intent(in),  optional :: globalResourceControl
      type(ESMF_Config),       intent(out), optional :: config
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
!     \item [{[configFilename]}]
!           Name of the config file for the entire application.
!     \item [{[defaultCalKind]}]
!           Sets the default calendar to be used by ESMF Time Manager.
!           If not specified, defaults to {\tt ESMF\_CALKIND\_NOCALENDAR}.
!     \item [{[defaultDefaultLogFilename]}]
!           Default value for argument {\tt defaultLogFilename}.
!     \item [{[defaultLogFilename]}]
!           Name of the default log file for warning and error messages.
!           If not specified, defaults to {\tt ESMF\_LogFile}.
!     \item [{[defaultLogAppendFlag]}]
!           Default value for argument {\tt logAppendFlag}.
!     \item [{[logAppendFlag]}]
!           If the default log file already exists, a value of {\tt .false.}
!           will set the file position to the beginning of the file.  A value
!           of [\tt .true.} sets the position to the end of the file.
!           If not specified, defaults to {\tt .true.}.
!     \item [{[defaultLogKindFlag]}]
!           Default value for argument {\tt logKindFlag}.
!     \item [{[logKindFlag]}]
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
!     \item [{[defaultGlobalResourceControl]}]
!           Default value for argument {\tt globalResourceControl}.
!     \item [{[globalResourceControl]}]
!           Global resource control enabled or disabled. Default {\tt .false.}.
!     \item [{[config]}]
!           Returns the default {\tt ESMF\_Config} if the
!           {\tt configFilename} argument was provided. Otherwise the
!           presence of this argument triggers an error.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      logical :: rcpresent                       ! Return code present   
      integer :: localrc
      logical, save :: already_init = .false.    ! Static, maintains state.
      logical :: openflag, isPresent
      integer :: complianceCheckIsOn
      integer :: traceIsOn, profileIsOn, profileToLog
      type(ESMF_VM) :: vm
      integer :: localPet

      character(ESMF_MAXPATHLEN) :: build_detail
      character(16) :: build_date, build_time
      integer :: detail_loc

      character(ESMF_MAXSTR) :: errmsg
      integer :: errmsg_l
      type(ESMF_Config)   :: configInternal

      logical                 :: globalResourceControlSet, logAppendFlagSet
      character(160)          :: defaultLogFilenameSet, defaultLogFilenameS
      character(80)           :: logKindFlagS, logKindFlagSU
      type(ESMF_LogKind_Flag) :: logKindFlagSet


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

      ! deal with API level defaulting
      !
      globalResourceControlSet = .false.
      if (present(defaultGlobalResourceControl)) &
        globalResourceControlSet = defaultGlobalResourceControl
      if (present(globalResourceControl)) &
        globalResourceControlSet = globalResourceControl
      !
      defaultLogFilenameSet = "ESMF_LogFile"
      if (present(defaultDefaultLogFilename)) &
        defaultLogFilenameSet = trim(defaultDefaultLogFilename)
      if (present(defaultLogFilename)) &
        defaultLogFilenameSet = trim(defaultLogFilename)
      !
      logAppendFlagSet = .true.
      if (present(defaultLogAppendFlag)) &
        logAppendFlagSet = defaultLogAppendFlag
      if (present(logAppendFlag)) &
        logAppendFlagSet = logAppendFlag
      !
      logKindFlagSet = ESMF_LOGKIND_MULTI
      if (present(defaultLogKindFlag)) &
        logKindFlagSet = defaultLogKindFlag
      if (present(logKindFlag)) &
        logKindFlagSet = logKindFlag

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
      ! ESMF_VMInitialize() with an un-initialized MPI will install correct 
      ! signal handlers _before_ possible helper threads are spawned by 
      ! MPI_Init().
      ! If, however, VMKernel threading is not used, or ESMF_InitializePreMPI()
      ! has been called, it is fine to come in with a user initialized MPI,
      ! and thus we support this mode as well!
      call ESMF_VMInitialize(mpiCommunicator=mpiCommunicator, rc=localrc)
      ! error handling without LogErr because it's not initialized yet
      if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error initializing VM"
          return
      endif

      ! deal with Config
      if (present(configFilename)) then
        ! have a default Config

        ! first must initialize a temporary ESMF default log
        call ESMF_LogInitialize("ESMF_LogFile",  &
          logKindFlag=ESMF_LOGKIND_MULTI_ON_ERROR, &
          rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          call ESMF_LogRc2Msg (localrc, errmsg, errmsg_l)
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": Error LogInitialize() log file: ", errmsg(:errmsg_l)
          return
        endif

        ! open the Config
        configInternal = ESMF_ConfigCreate(rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! load config file
        call ESMF_ConfigLoadFile(configInternal, configFilename, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        ! globalResourceControl
        call ESMF_ConfigFindLabel(configInternal, &
          label="globalResourceControl:", isPresent=isPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        if (isPresent) then
          if (present(globalResourceControl)) then
            ! both API and Config want to set -> error
            call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
              msg="Cannot set 'globalResourceControl' from Config and API "//&
              "at the same time.", ESMF_CONTEXT, rcToReturn=rc)
            return  ! bail out
          endif
          call ESMF_ConfigGetAttribute(configInternal, &
            globalResourceControlSet, label="globalResourceControl:", &
            default=.false., rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! logKindFlag
        call ESMF_ConfigFindLabel(configInternal, &
          label="logKindFlag:", isPresent=isPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        if (isPresent) then
          if (present(logKindFlag)) then
            ! both API and Config want to set -> error
            call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
              msg="Cannot set 'logKindFlag' from Config and API "//&
              "at the same time.", ESMF_CONTEXT, rcToReturn=rc)
            return  ! bail out
          endif
          call ESMF_ConfigGetAttribute(configInternal, logKindFlagS, &
            label="logKindFlag:", default="---invalid---", rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          logKindFlagSU = ESMF_UtilStringUpperCase(logKindFlagS, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (trim(logKindFlagSU)=="ESMF_LOGKIND_NONE") then
            logKindFlagSet = ESMF_LOGKIND_NONE
          else if (trim(logKindFlagSU)=="ESMF_LOGKIND_SINGLE") then
            logKindFlagSet = ESMF_LOGKIND_SINGLE
          else if (trim(logKindFlagSU)=="ESMF_LOGKIND_MULTI") then
            logKindFlagSet = ESMF_LOGKIND_MULTI
          else if (trim(logKindFlagSU)=="ESMF_LOGKIND_MULTI_ON_ERROR") then
            logKindFlagSet = ESMF_LOGKIND_MULTI_ON_ERROR
          endif
        endif

        ! defaultLogFilename
        call ESMF_ConfigFindLabel(configInternal, &
          label="defaultLogFilename:", isPresent=isPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        if (isPresent) then
          if (present(defaultLogFilename)) then
            ! both API and Config want to set -> error
            call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
              msg="Cannot set 'defaultLogFilename' from Config and API "//&
              "at the same time.", ESMF_CONTEXT, rcToReturn=rc)
            return  ! bail out
          endif
          call ESMF_ConfigGetAttribute(configInternal, defaultLogFilenameS, &
            label="defaultLogFilename:", default="---invalid---", rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (trim(defaultLogFilenameS)/="---invalid---") then
            defaultLogFilenameSet = trim(defaultLogFilenameS)
          endif
        endif

        ! logAppendFlag
        call ESMF_ConfigFindLabel(configInternal, &
          label="logAppendFlag:", isPresent=isPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        if (isPresent) then
          if (present(logAppendFlag)) then
            ! both API and Config want to set -> error
            call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
              msg="Cannot set 'logAppendFlag' from Config and API "//&
              "at the same time.", ESMF_CONTEXT, rcToReturn=rc)
            return  ! bail out
          endif
          call ESMF_ConfigGetAttribute(configInternal, &
            logAppendFlagSet, label="logAppendFlag:", &
            default=.true., rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! optionally destroy the Config
        if (.not.present(config)) then
          call ESMF_ConfigDestroy(configInternal, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          config = configInternal ! return back to user
        endif

        ! shut down temporary Log
        call ESMF_LogFinalize(rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          call ESMF_LogRc2Msg (localrc, errmsg, errmsg_l)
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": Error finalizing log file: ", errmsg(:errmsg_l)
          return
        endif
      endif ! have a default Config

      ! set global VM resource control
      call ESMF_VMSet(globalResourceControl=globalResourceControlSet, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Initialize ESMF default log
      call ESMF_LogInitialize(trim(defaultLogFilenameSet), &
        logAppendFlag=logAppendFlagSet, logKindFlag=logKindFlagSet, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
        write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error initializing the default log/error manager"
        return
      endif

      ! Write out warning about performance impact of logging
      if ((logKindFlagSet/=ESMF_LOGKIND_NONE) .and. &
        (logKindFlagSet/=ESMF_LOGKIND_MULTI_ON_ERROR)) then
        call ESMF_LogWrite(&
          "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!", &
          ESMF_LOGMSG_INFO, rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, &
            ": Error writing into the default log"
          return
        endif
        call ESMF_LogWrite( &
          "!!! THE ESMF_LOG IS SET TO OUTPUT ALL LOG MESSAGES !!!", &
          ESMF_LOGMSG_INFO, rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, &
            ": Error writing into the default log"
          return
        endif
        call ESMF_LogWrite( &
          "!!!     THIS MAY CAUSE SLOWDOWN IN PERFORMANCE     !!!", &
          ESMF_LOGMSG_INFO, rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, &
            ": Error writing into the default log"
          return
        endif
        call ESMF_LogWrite( &
          "!!! FOR PRODUCTION RUNS, USE:                      !!!", &
          ESMF_LOGMSG_INFO, rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, &
            ": Error writing into the default log"
          return
        endif
        call ESMF_LogWrite( &
          "!!!                   ESMF_LOGKIND_Multi_On_Error  !!!", &
          ESMF_LOGMSG_INFO, rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, &
            ": Error writing into the default log"
          return
        endif
        call ESMF_LogWrite(&
          "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!", &
          ESMF_LOGMSG_INFO, rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, &
            ": Error writing into the default log"
          return
        endif
      endif

      ! Write our version number, build location, and other details to the log
#ifdef ESMF_VERSION_STRING_GIT
      call ESMF_LogWrite(&
           "Running with ESMF Version   : " // ESMF_VERSION_STRING_GIT, &
           ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
         return
      endif
#else
      call ESMF_LogWrite(&
           "Running with ESMF Version   : " // ESMF_VERSION_STRING, &
           ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
         return
      endif
#endif
      call c_esmc_initget_build_datetime (build_date, build_time, localrc)
      call ESMF_LogWrite(&
           "ESMF library build date/time: " // trim (build_date) // ' ' // build_time,  &
           ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
         return
      endif

      call c_esmc_initget_esmf_dir (build_detail, localrc)
      call ESMF_LogWrite(&
           "ESMF library build location : " // build_detail,  &
           ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
         return
      endif

      call c_esmc_initget_esmf_comm (build_detail, localrc)
      call ESMF_LogWrite(&
           "ESMF_COMM                   : " // build_detail,  &
           ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
         return
      endif

#if defined (ESMF_MOAB)
      build_detail = 'enabled'
#else
      build_detail = 'disabled'
#endif
      call ESMF_LogWrite(&
           "ESMF_MOAB                   : " // build_detail,  &
           ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
         return
      endif

#if defined (ESMF_LAPACK)
      build_detail = 'enabled'
#else
      build_detail = 'disabled'
#endif
      call ESMF_LogWrite(&
           "ESMF_LAPACK                 : " // build_detail,  &
           ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
         return
      endif

#if defined (ESMF_NETCDF)
      build_detail = 'enabled'
#else
      build_detail = 'disabled'
#endif
      call ESMF_LogWrite(&
           "ESMF_NETCDF                 : " // build_detail,  &
           ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
         return
      endif

#if defined (ESMF_PNETCDF)
      build_detail = 'enabled'
#else
      build_detail = 'disabled'
#endif
      call ESMF_LogWrite(&
           "ESMF_PNETCDF                : " // build_detail,  &
           ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
         return
      endif

#if defined (ESMF_PIO)
      build_detail = 'enabled'
#else
      build_detail = 'disabled'
#endif
      call ESMF_LogWrite(&
           "ESMF_PIO                    : " // build_detail,  &
           ESMF_LOGMSG_INFO, rc=localrc)
      if (localrc /= ESMF_SUCCESS) then
         write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error writing into the default log"
         return
      endif

#if defined (ESMF_YAMLCPP)
      build_detail = 'enabled'
#else
      build_detail = 'disabled'
#endif
      call ESMF_LogWrite(&
           "ESMF_YAMLCPP                : " // build_detail,  &
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
      call c_esmc_getComplianceCheckTrace(traceIsOn, profileIsOn, localrc)
      if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error checking ESMF_RUNTIME_COMPLIANCECHECK env variable"
          return
      endif
      if (traceIsOn == 1 .or. profileIsOn == 1) then
        profileToLog = 0
        if ((logKindFlagSet/=ESMF_LOGKIND_NONE) .and. &
          (logKindFlagSet/=ESMF_LOGKIND_MULTI_ON_ERROR)) then
          profileToLog = 1
        endif
        call ESMF_TraceOpen("./traceout", profileToLog=profileToLog, rc=localrc)
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

      if (.not.present(configFilename).and.present(config)) then
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Cannot request 'config' without supplying "// &
            "'configFilename'", &
            ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      endif

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

      integer :: traceIsOn, profileIsOn

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

      call c_esmc_getComplianceCheckTrace(traceIsOn, profileIsOn, localrc)
      if (localrc /= ESMF_SUCCESS) then
          write (ESMF_UtilIOStderr,*) ESMF_METHOD, ": Error checking ESMF_RUNTIME_COMPLIANCECHECK env variable"
          return
      endif
      if (traceIsOn == 1 .or. profileIsOn == 1) then
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
        call ESMF_VMAbort(rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          call ESMF_LogRc2Msg (localrc, errmsg, errmsg_l)
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": Error aborting VM: ", errmsg(:errmsg_l)
          return
        endif
      else
        ! Finalize the VM
        call ESMF_VMFinalize(keepMpiFlag=keepMpiFlag, rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          call ESMF_LogRc2Msg (localrc, errmsg, errmsg_l)
          write (ESMF_UtilIOStderr,*) ESMF_METHOD,  &
              ": Error finalizing VM: ", errmsg(:errmsg_l)
          return
        endif
      endif

      ! Shut down the log file
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
