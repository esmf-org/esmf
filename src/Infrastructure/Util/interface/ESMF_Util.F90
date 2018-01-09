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
#define ESMF_FILENAME "ESMF_Util.F90"

!
! ESMF Util Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------
! one blank line for protex processing - in case all routines here are
! marked internal (BOPI/EOPI), the output file will still have contents.
!BOP

!EOP

!------------------------------------------------------------------------------
! module definition

      module ESMF_UtilMod
 
      ! parameters, types
      use ESMF_IOUtilMod
      use ESMF_UtilStringMod
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod

#include "ESMF.h"

!BOPI
! !MODULE: ESMF_UtilMod - Interface routines to generic utility functions
!
! !DESCRIPTION:
!
!  Interfaces to, in most cases, the C++ implementation of generic utility
!  functions.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
! !DESCRIPTION:
!
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
!

!  Overloaded = operator functions
      public :: operator(==)
      public :: operator(/=)
      public :: assignment(=)

!  Command line argument methods
      public :: ESMF_UtilGetArgC
      public :: ESMF_UtilGetArg
      public :: ESMF_UtilGetArgIndex

!  File system directory methods
      public :: ESMF_UtilIOMkDir
      public :: ESMF_UtilIORmDir
      public :: ESMF_UtilIOGetCWD

!  Misc string methods
      public :: ESMF_UtilString2Int
      public :: ESMF_UtilStringLowerCase
      public :: ESMF_UtilStringUpperCase
      public :: ESMF_UtilArray2String
      public :: ESMF_UtilString2Array
      public :: ESMF_StringConcat

!  Misc type-to-string methods
      public :: ESMF_StatusString
      public :: ESMF_TypeKindString
      public :: ESMF_LogicalString

!
!

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id$'
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------- 
! Command line interfaces
!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilGetArg"
!BOP
! !IROUTINE:  ESMF_UtilGetArg - Return a command line argument
!
! !INTERFACE:
  subroutine ESMF_UtilGetArg(argindex, keywordEnforcer, argvalue, arglength, rc)
!
! !ARGUMENTS:
    integer,      intent(in)            :: argindex
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(*), intent(out), optional :: argvalue
    integer,      intent(out), optional :: arglength
    integer,      intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! This method returns a copy of a command line argument specified
! when the process was started.  This argument is the same as an
! equivalent C++ program would find in the argv array.
!
! Some MPI implementations do not consistently provide command line
! arguments on PETs other than PET 0.  It is therefore recommended
! that PET 0 call this method and broadcast the results to the other
! PETs by using the {\tt ESMF\_VMBroadcast()} method.
!
! The arguments are:
!
! \begin{description}
! \item [{argindex}]
! A non-negative index into the command line argument {\tt argv} array.
! If argindex is negative or greater than the number of user-specified
! arguments, {\tt ESMF\_RC\_ARG\_VALUE} is returned in the {\tt rc} argument.
! \item [{[argvalue]}]
! Returns a copy of the desired command line argument.  If the provided
! character string is longer than the command line argument, the string
! will be blank padded.  If the string is too short, truncation will
! occur and {\tt ESMF\_RC\_ARG\_SIZE} is returned in the {\tt rc} argument.
! \item [{[arglength]}]
! Returns the length of the desired command line argument in characters.
! The length result does not depend on the length of the {\tt value}
! string.  It may be used to query the length of the argument.
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------- 
    character(ESMF_MAXPATHLEN) :: localvalue
    integer :: localargc, localrc
    integer :: localstat

#if defined (ESMF_NEEDSPXFGETARG) || defined (ESMF_NEEDSGETARG)
    integer :: locallength
#endif

#if defined (ESMF_NEEDSPXFGETARG)
    integer, external :: ipxfconst
    integer :: ETRUNC, EINVAL
#endif

    ! assume failure until success
    if (present (rc)) then
      rc = ESMF_RC_NOT_IMPL
    end if

    call ESMF_UtilGetArgc (count=localargc)
    if (present (argvalue)) argvalue = ""
    if (present (arglength)) arglength = 0
    localrc = merge (ESMF_SUCCESS, ESMF_RC_ARG_VALUE,  &
        argindex >= 0 .and. argindex <= localargc)
    if (ESMF_LogFoundError ( localrc,  &
        ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc))  &
      return

#if !defined (ESMF_NEEDSPXFGETARG) && !defined (ESMF_NEEDSGETARG)
! Fortran 2003 version (default and preferred)

    ! test on argvalue presense in order to work around a g95
    ! (version (g95 0.93!) Aug 17 2010) optional argument bug.
    if (present (argvalue)) then
      call get_command_argument (number=argindex,  &
                               value=argvalue, length=arglength,  &
                               status=localstat)
    else
      call get_command_argument (number=argindex,  &
                               value=localvalue, length=arglength,  &
                               status=localstat)
    end if

    ! Convert Fortran status to ESMF rc

    select case (localstat)
    case (0)
      localrc = ESMF_SUCCESS
    case (-1)
      localrc = ESMF_RC_ARG_SIZE
    case (1:)
      localrc = ESMF_RC_ARG_VALUE
    case default
      localrc = ESMF_RC_VAL_OUTOFRANGE
    end select

#elif defined (ESMF_NEEDSPXFGETARG)
! POSIX Fortran bindings (1003.9-1992)
! Has error checking comparable to F2003.  So when F2003 intrinsics
! are not available and PXF is available (e.g., on IRIX), it is
! preferable to calling 'getarg'.

    ETRUNC = ipxfconst ("ETRUNC")
    EINVAL = ipxfconst ("EINVAL")

    if (present (argvalue)) then
      call pxfgetarg (argindex, argvalue, locallength, localstat)
    else
      call pxfgetarg (argindex, localvalue, locallength, localstat)
      if (localstat == ETRUNC) localrc = 0
    end if

    ! Convert PXF ierror to ESMF rc

    if (localstat == 0) then
      localrc = ESMF_SUCCESS
    else if (localstat == ETRUNC) then
      localrc = ESMF_RC_ARG_SIZE
    else if (localstat == EINVAL) then
      localrc = ESMF_RC_ARG_VALUE
    else
      localrc = ESMF_RC_VAL_OUTOFRANGE
    end if
 
    if (present (arglength)) then
      arglength = locallength
    end if
     
#else
! Non-Standard.  But dates back to the original 7th Edition unix f77
! compiler, so is implemented by many compilers.  Error checking,
! especially for bad character string lengths, is unreliable.

    call getarg (argindex, localvalue)

! If argvalue is present, use the longer of value and localvalue
! for the buffer.

    if (present (argvalue)) then
      call getarg (argindex, argvalue)
      if (len (argvalue) > len (localvalue)) then
        locallength = len_trim (argvalue)
      else
        locallength = len_trim (localvalue)
      end if
      if (len (argvalue) >= locallength) then
        localrc = ESMF_SUCCESS
      else
        localrc = ESMF_RC_ARG_SIZE
      end if
    else
      locallength = len_trim (localvalue)
      localrc = ESMF_SUCCESS
    end if

    if (present (arglength)) then
      arglength = locallength
    end if

#endif

    if (ESMF_LogFoundError ( localrc, ESMF_ERR_PASSTHRU,  &
        ESMF_CONTEXT, rcToReturn=rc))  &
      return

    if (present (rc)) then
      rc = localrc
    end if

  end subroutine ESMF_UtilGetArg

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilGetArgC"
!BOP
! !IROUTINE:  ESMF_UtilGetArgC - Return number of command line arguments
!
! !INTERFACE:
  subroutine ESMF_UtilGetArgC(count, keywordEnforcer, rc)
!
! !ARGUMENTS:
    integer, intent(out)           :: count
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! This method returns the number of command line arguments specified
! when the process was started.
!
! The number of arguments returned does not include the name of the
! command itself - which is typically returned as argument zero.
!
! Some MPI implementations do not consistently provide command line
! arguments on PETs other than PET 0.  It is therefore recommended
! that PET 0 call this method and broadcast the results to the other
! PETs by using the {\tt ESMF\_VMBroadcast()} method.
!
! The arguments are:
!
! \begin{description}
! \item [count]
! Count of command line arguments.
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
    integer :: argc

#if !defined (ESMF_NEEDSPXFGETARG) && !defined (ESMF_NEEDSGETARG)
! Fortran 2003 version (default and preferred)

    argc = command_argument_count ()

#elif defined (ESMF_NEEDSPXFGETARG)
! POSIX Fortran bindings (1003.9-1992)

    integer, external :: ipxfargc

    argc = ipxfargc ()

#else
! Non-Standard, but implemented by many compilers.

    integer, external :: iargc

    argc = iargc ()

#endif

    count = argc

    if (present (rc)) then
      rc = ESMF_SUCCESS
    end if

  end subroutine ESMF_UtilGetArgC

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilGetArgIndex"
!BOP
! !IROUTINE:  ESMF_UtilGetArgIndex - Return the index of a command line argument
!
! !INTERFACE:
  subroutine ESMF_UtilGetArgIndex(argvalue, keywordEnforcer, argindex, rc)
!
! !ARGUMENTS:
    character(*), intent(in)            :: argvalue
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,      intent(out), optional :: argindex
    integer,      intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! This method searches for, and returns the index of a desired command
! line argument.  An example might be to find a specific keyword
! (e.g., -esmf\_path) so that its associated value argument could be
! obtained by adding 1 to the argindex and calling {\tt ESMF\_UtilGetArg()}.
!
! Some MPI implementations do not consistently provide command line
! arguments on PETs other than PET 0.  It is therefore recommended
! that PET 0 call this method and broadcast the results to the other
! PETs by using the {\tt ESMF\_VMBroadcast()} method.
!
! The arguments are:
!
! \begin{description}
! \item [argvalue]
! A character string which will be searched for in the command line
! argument list.
! \item [{[argindex]}]
! If the {\tt value} string is found, the position will be returned
! as a non-negative integer.  If the string is not found, a negative
! value will be returned.
! \item [{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOP
!------------------------------------------------------------------------- 

    integer :: argindex_local
    integer :: i
    integer :: len_local, len_max
    integer :: localrc
    integer :: nargs

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    call ESMF_UtilGetArgC (count=nargs)

    ! Find the maximum string length of all command line arguments

    len_max = 0
    do, i=0, nargs
      call ESMF_UtilGetArg (i, arglength=len_local, rc=localrc)
      if (ESMF_LogFoundError ( localrc,  ESMF_ERR_PASSTHRU,  &
          ESMF_CONTEXT, rcToReturn=rc))  &
        return
      len_max = max (len_max, len_local)
    end do

    ! Call subroutine so that a proper string length can be used for
    ! comparison.

    call arg_search_worker (len_max, argindex_local, rc1=localrc)
    if (ESMF_LogFoundError ( localrc, ESMF_ERR_PASSTHRU,  &
        ESMF_CONTEXT, rcToReturn=rc))  &
      return
    
    if (present (argindex)) &
      argindex = argindex_local

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
      
  contains

    subroutine arg_search_worker (len_max, argindex1, rc1)
      integer, intent(in)  :: len_max
      integer, intent(out) :: argindex1
      integer, intent(out) :: rc1

      character(len_max) :: string
      integer :: i1
      integer :: localrc1

      do, i1=0, nargs
        call ESMF_UtilGetArg (i1, argvalue=string, rc=localrc1)
        if (ESMF_LogFoundError ( localrc1, ESMF_ERR_PASSTHRU,  &
            ESMF_CONTEXT, rcToReturn=rc1))  &
          return
        if (string == argvalue) exit
      end do

      if (i1 <= nargs) then
        argindex1 = i1
      else
        argindex1 = -1
      end if

    end subroutine arg_search_worker

  end subroutine ESMF_UtilGetArgIndex

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilIOGetCWD"
!BOP
! !IROUTINE: ESMF_UtilIOGetCWD - Get the current directory
!
! !INTERFACE:
  subroutine ESMF_UtilIOGetCWD (pathName, keywordEnforcer, rc)
!
! !PARAMETERS:
    character(*), intent(out)           :: pathName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,      intent(out), optional :: rc
!
! !DESCRIPTION:
!   Call the system-dependent routine to get the current directory from the file
!   system.
!
!     The arguments are:
!     \begin{description}
!     \item[pathName]
!       Name of the current working directory.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP

    integer :: localrc

    if (present(rc)) rc = ESMF_FAILURE

    call c_esmc_getcwd (pathname, localrc)
    if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
        ESMF_CONTEXT, rcToReturn=rc))  &
      return

    if (present (rc)) then
      rc = localrc
    end if

  end subroutine ESMF_UtilIOGetCWD

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilIOMkDir"
!BOP
! !IROUTINE: ESMF_UtilIOMkDir - Create a directory in the file system
!
! !INTERFACE:
!  subroutine ESMF_UtilIOMkDir (pathName, keywordEnforcer,  &
!      mode, relaxedFlag,  &
!      rc)
!
! !PARAMETERS:
!    character(*), intent(in)            :: pathName
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,      intent(in),  optional :: mode
!    logical,      intent(in),  optional :: relaxedFlag
!    integer,      intent(out), optional :: rc
!
! !DESCRIPTION:
!   Call the system-dependent routine to create a directory in the file system.
!
!     The arguments are:
!     \begin{description}
!     \item[pathName]
!       Name of the directory to be created.
!     \item[{[mode]}]
!       File permission mode.  Typically an octal constant is used as a value, for example:
!       {\tt mode=o'755'}.  If not specified on POSIX-compliant systems, the default
!       is {\tt o'755'} - corresponding to owner read/write/execute,
!       group read/execute, and world read/execute.  On native Windows, this argument is
!       ignored and default security settings are used.
!     \item[{[relaxedFlag]}]
!       When set to {\tt .true.}, if the directory already exists, {\tt rc}
!       will be set to {\tt ESMF\_SUCCESS} instead of an error.
!       If not specified, the default is {\tt .false.}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
!TODO: Review and implement parentsFlag for future release.
!BOPI
! !IROUTINE: ESMF_UtilIOMkDir - Create a directory in the file system
!
! !INTERFACE:
  subroutine ESMF_UtilIOMkDir (pathName, keywordEnforcer,  &
      mode, parentsFlag, relaxedFlag,  &
      rc)
!
! !PARAMETERS:
    character(*), intent(in)            :: pathName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,      intent(in),  optional :: mode
    logical,      intent(in),  optional :: parentsFlag
    logical,      intent(in),  optional :: relaxedFlag
    integer,      intent(out), optional :: rc
!
! !DESCRIPTION:
!   Call the system-dependent routine to create a directory in the file system.
!
!     The arguments are:
!     \begin{description}
!     \item[pathName]
!       Name of the directory to be created.
!     \item[{[mode]}]
!       File permission mode.  If not specified on POSIX-compliant systems,
!       the default is {\tt o'755'}.  On native Windows, this argument is
!       ignored and default security settings are used.
!     \item[{[parentsFlag]}]
!       When set to {\tt .true.}, create parent directories as needed.
!       If not specified, the default is {\tt .false.}.
!     \item[{[relaxedFlag]}]
!       When set to {\tt .true.}, if the directory already exists, {\tt rc}
!       will be set to {\tt ESMF\_SUCCESS} instead of an error.
!       If not specified, the default is {\tt .false.}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI

    integer :: mode_local
    type(ESMF_Logical) :: pflag, rflag  
    integer :: localrc

    integer :: mode_default
    data mode_default/o'755'/

    if (present(rc)) rc = ESMF_FAILURE

    mode_local = mode_default
    if (present (mode)) mode_local = mode

    pflag = .false.
    if (present (parentsFlag)) pflag = parentsFlag
    if (pflag == ESMF_TRUE) then
      if (ESMF_LogFoundError (ESMF_RC_NOT_IMPL, ESMF_ERR_PASSTHRU,  &
        ESMF_CONTEXT, rcToReturn=rc))  &
      return
    end if

    rflag = .false.
    if (present (relaxedFlag)) rflag = relaxedFlag

    call c_esmc_makedirectory (pathname, mode_local, rflag, localrc)
    if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
        ESMF_CONTEXT, rcToReturn=rc))  &
      return

    if (present (rc)) then
      rc = localrc
    end if

  end subroutine ESMF_UtilIOMkDir

!-------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilIORmDir"
!BOP
! !IROUTINE: ESMF_UtilIORmDir - Remove a directory from the file system
!
! !INTERFACE:
!  subroutine ESMF_UtilIORmDir (pathName, keywordEnforcer,  &
!      relaxedFlag, rc)
!
! !PARAMETERS:
!    character(*), intent(in)            :: pathName
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    logical,      intent(in),  optional :: relaxedFlag
!    integer,      intent(out), optional :: rc
!
! !DESCRIPTION:
!   Call the system-dependent routine to remove a directory from the file
!   system.  Note that the directory must be empty in order to be successfully
!   removed.
!
!     The arguments are:
!     \begin{description}
!     \item[pathName]
!       Name of the directory to be removed.
!     \item[{[relaxedFlag]}]
!       If set to {\tt .true.}, and if the specified directory does not exist,
!       the error is ignored and {\tt rc} will be set to {\tt ESMF\_SUCCESS}.
!       If not specified, the default is {\tt .false.}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
!TODO: Review and implement filesFlag for future release.
!BOPI
! !IROUTINE: ESMF_UtilIORmDir - Remove a directory from the file system
!
! !INTERFACE:
  subroutine ESMF_UtilIORmDir (pathName, keywordEnforcer,  &
      filesFlag, relaxedFlag, rc)
!
! !PARAMETERS:
    character(*), intent(in)            :: pathName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,      intent(in),  optional :: filesFlag
    logical,      intent(in),  optional :: relaxedFlag
    integer,      intent(out), optional :: rc
!
! !DESCRIPTION:
!   Call the system-dependent routine to remove a directory from the file
!   system.  Note that the directory must be empty in order to be successfully
!   removed.
!
!     The arguments are:
!     \begin{description}
!     \item[pathName]
!       Name of the directory to be removed.
!     \item[{[filesFlag]}]
!       When set to {\tt .true.}, remove the directory, including any contained
!       files.  Contained directories are not removed.  If not specified,
!       the default is {\tt .false.}.
!     \item[{[relaxedFlag]}]
!       If set to {\tt .true.}, and if the specified directory does not exist,
!       the error is ignored and {\tt rc} will be set to {\tt ESMF\_SUCCESS}.
!       If not specified, the default is {\tt .false.}.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI

    integer :: localrc
    type(ESMF_Logical) :: fflag  
    type(ESMF_Logical) :: rflag  

    if (present(rc)) rc = ESMF_FAILURE

    fflag = .false.
    if (present (filesFlag)) fflag = filesFlag
    if (fflag == ESMF_TRUE) then
      if (ESMF_LogFoundError (ESMF_RC_NOT_IMPL, ESMF_ERR_PASSTHRU,  &
        ESMF_CONTEXT, rcToReturn=rc))  &
      return
    end if

    rflag = .false.
    if (present (relaxedFlag)) rflag = relaxedFlag

    call c_esmc_removedirectory (pathname, rflag, localrc)
    if (ESMF_LogFoundError (localrc, ESMF_ERR_PASSTHRU,  &
        ESMF_CONTEXT, rcToReturn=rc))  &
      return

    if (present (rc)) then
      rc = localrc
    end if

  end subroutine ESMF_UtilIORmDir


!------------------------------------------------------------------------- 
!------------------------------------------------------------------------- 
! misc string routines
!------------------------------------------------------------------------- 
!------------------------------------------------------------------------- 
!BOP
! !IROUTINE: ESMF_UtilString2Int - Convert a string to an integer
! !INTERFACE:
  function ESMF_UtilString2Int(string, keywordEnforcer,  &
      specialStringList, specialValueList, rc)
! !RETURN VALUE:
    integer :: ESMF_UtilString2Int
! !ARGUMENTS:
    character(len=*), intent(in)            :: string
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*), intent(in),  optional :: specialStringList(:)
    integer,          intent(in),  optional :: specialValueList(:)
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Return the numerical integer value represented by the {\tt string}.
!   If {\tt string} matches a string in the optional {\tt specialStringList}, the
!   corresponding special value will be returned instead.
!
!   If special strings are to be taken into account, both 
!   {\tt specialStringList} and {\tt specialValueList} arguments must be
!   present and of same size.
!   
!   An error is returned, and return value set to 0, if {\tt string} is not
!   found in {\tt specialStringList}, and does not convert into an integer
!   value.
!
!   Leading and trailing blanks in {\tt string} are ignored when directly
!   converting into integers.
!
!   This procedure may fail when used in an expression in a {\tt write} statement
!   with some older, pre-Fortran 2003, compiler environments that do not support
!   re-entrant I/O calls.
!
!   The arguments are:
!   \begin{description}
!   \item[string]
!     The string to be converted
!   \item[{[specialStringList]}]
!     List of special strings.
!   \item[{[specialValueList]}]
!     List of values associated with special strings.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    logical                 :: ssL, svL
    integer                 :: i
    integer                 :: ioerr
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ESMF_UtilString2Int = 0 ! initialize
    
    ! checking consistency of inputs provided
    ssL = present(specialStringList)
    svL = present(specialValueList)
    
    if (ssL.neqv.svL) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Both specialStringList and specialValueList must either be "// &
        "present or absent.", &
        line=__LINE__, &
        file=ESMF_FILENAME, &
        rcToReturn=rc)
      return ! bail out
    endif
    
    if (ssL) then
      ! special strings and values present
      if (size(specialStringList) /= size(specialValueList)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Both specialStringList and specialValueList must have "// &
          "the same number of elements.", &
          line=__LINE__, &
          file=ESMF_FILENAME, &
          rcToReturn=rc)
        return ! bail out
      endif
      do i=1, size(specialStringList)
        if (trim(string)==trim(specialStringList(i))) then
          ! found a matching special string
          ESMF_UtilString2Int = specialValueList(i)
          return ! successful early return
        endif
      enddo
    endif
    
    if (verify(trim(adjustl(string)),"-+0123456789") == 0) then
      ! should convert to integer just fine
      read (string, "(i12)", iostat=ioerr) ESMF_UtilString2Int
      if (ioerr /= 0) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="The string '"//trim(string)//"' could not be converted to integer.", &
            line=__LINE__, &
            file=ESMF_FILENAME, &
            rcToReturn=rc)
        return ! bail out
      end if
    else
      ! the string contains characters besides numbers
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="The string '"//trim(string)//"' contains characters besides "// &
          "numbers, cannot convert to integer.", &
        line=__LINE__, &
        file=ESMF_FILENAME, &
        rcToReturn=rc)
      return ! bail out
    endif
    
  end function ESMF_UtilString2Int
  !-----------------------------------------------------------------------------


#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilStringLowerCase"
!BOP
!  !IROUTINE:  ESMF_UtilStringLowerCase - convert string to lowercase
!  
! !INTERFACE: 
    function ESMF_UtilStringLowerCase(string, keywordEnforcer, rc) 
!
! !ARGUMENTS:
      character(len=*), intent(in) :: string
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer, intent(out), optional  :: rc  
! !RETURN VALUE:
      character(len (string)) :: ESMF_UtilStringLowerCase

!
! !DESCRIPTION:
!   Converts given string to lowercase.
!
!     The arguments are:
!     \begin{description}
!     \item[string]
!       A character string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: i
      character(len=1) :: c
      integer, parameter :: shift = ichar('a') - ichar('A')

      do i = 1, len(string)
        c = string(i:i)
        if(c >= 'A' .and. c <= 'Z') then
          c = char(ichar(c) + shift)
        endif
        ESMF_UtilStringLowerCase(i:i) = c
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_UtilStringLowerCase

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilStringUpperCase"
!BOP
!  !IROUTINE:  ESMF_UtilStringUpperCase - convert string to uppercase
!  
! !INTERFACE: 
      function ESMF_UtilStringUpperCase(string, keywordEnforcer, rc) 
!
! !ARGUMENTS:
      character(len=*), intent(in) :: string
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer, intent(out), optional  :: rc  
! !RETURN VALUE:
      character(len (string)) :: ESMF_UtilStringUpperCase

!
! !DESCRIPTION:
!   Converts given string to uppercase.
!
!     The arguments are:
!     \begin{description}
!     \item[string]
!       A character string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: i
      character(len=1) :: c
      integer, parameter :: shift = ichar('a') - ichar('A')

      do i = 1, len(string)
        c = string(i:i)
        if(c .ge. 'a' .and. c .le. 'z') then
          c = char(ichar(c) - shift)
        endif
        ESMF_UtilStringUpperCase(i:i) = c
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_UtilStringUpperCase

!------------------------------------------------------------------------- 
!------------------------------------------------------------------------- 
! misc print routines
!------------------------------------------------------------------------- 
!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StatusString"
!BOPI 
!  !IROUTINE:  ESMF_StatusString - Return status as a string
!  
! !INTERFACE: 
      subroutine ESMF_StatusString(status, string, rc)
!
! !ARGUMENTS:
      type(ESMF_Status), intent(in) :: status
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return an {\tt ESMF\_Status} as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[status]
!       The {\tt ESMF\_Status}.
!     \item[string]
!       A printable string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      if (status == ESMF_STATUS_UNINIT) string = "Uninitialized"
      if (status == ESMF_STATUS_READY) string = "Ready"
      if (status == ESMF_STATUS_UNALLOCATED) string = "Unallocated"
      if (status == ESMF_STATUS_ALLOCATED) string = "Allocated"
      if (status == ESMF_STATUS_BUSY) string = "Busy"
      if (status == ESMF_STATUS_INVALID) string = "Invalid"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StatusString

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TypeKindString"
!BOPI 
!  !IROUTINE:  ESMF_TypeKindString - Return TypeKind as a string
!  
! !INTERFACE: 
      subroutine ESMF_TypeKindString(datakind, string, rc)
!
! !ARGUMENTS:
      type(ESMF_TypeKind_Flag), intent(in) :: datakind
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return an {\tt ESMF\_TypeKind\_Flag} variable as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[datakind]
!       The {\tt ESMF\_TypeKind\_Flag}.
!     \item[string]
!       The value as a string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

#ifndef ESMF_NO_INTEGER_1_BYTE 
      if (datakind == ESMF_TYPEKIND_I1)  string = "Integer*1"
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE 
      if (datakind == ESMF_TYPEKIND_I2)  string = "Integer*2"
#endif
      if (datakind == ESMF_TYPEKIND_I4)  string = "Integer*4"
      if (datakind == ESMF_TYPEKIND_I8)  string = "Integer*8"
      if (datakind == ESMF_TYPEKIND_R4)  string = "Real*4"
      if (datakind == ESMF_TYPEKIND_R8)  string = "Real*8"
      if (datakind == ESMF_TYPEKIND_C8)  string = "Complex*8"
      if (datakind == ESMF_TYPEKIND_C16) string = "Complex*16"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_TypeKindString

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogicalString"
!BOPI 
!  !IROUTINE:  ESMF_LogicalString - Return Logical as a string
!  
! !INTERFACE: 
      subroutine ESMF_LogicalString(tf, string, rc)
!
! !ARGUMENTS:
      type(ESMF_Logical), intent(in) :: tf
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return an {\tt ESMF\_Logical} as a string.
!
!     The arguments are:
!     \begin{description}
!     \item[tf]
!       An {\tt ESMF\_Logical}.
!     \item[string]
!       The value as a string.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      if (tf == ESMF_TRUE)  string = "True"
      if (tf == ESMF_FALSE) string = "False"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LogicalString

!------------------------------------------------------------------------------


      end module ESMF_UtilMod
