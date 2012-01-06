! $Id: ESMF_Util.F90,v 1.57.2.2 2012/01/06 21:46:28 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
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
!BOPI
! !IROUTINE: ESMF_UtilNameMapAdd

! !INTERFACE:

    interface ESMF_UtilMapNameAdd
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_UtilMapNameContainerAdd
      module procedure ESMF_UtilMapNameValueAdd
    end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_UtilNameMapLookup

! !INTERFACE:

    interface ESMF_UtilMapNameLookup
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_UtilMapNameContainerLookup
      module procedure ESMF_UtilMapNameValueLookup
    end interface

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
!

!  Overloaded = operator functions
      public :: operator(==)
      public :: operator(/=)
      public :: assignment(=)

!  STL map container interface

      public :: ESMF_UtilMapNameAdd
      public :: ESMF_UtilMapNameCreate
      public :: ESMF_UtilMapNameDestroy
      public :: ESMF_UtilMapNameLookup
      public :: ESMF_UtilMapNamePrint
      public :: ESMF_UtilMapNameRemove
      public :: ESMF_UtilMapNameSize

!  Command line argument methods
      public :: ESMF_UtilGetArgC
      public :: ESMF_UtilGetArg
      public :: ESMF_UtilGetArgIndex

!  Misc methods
      public :: ESMF_Array2String
      public :: ESMF_String2Array
      public :: ESMF_StringLowerCase
      public :: ESMF_StringUpperCase

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
               '$Id: ESMF_Util.F90,v 1.57.2.2 2012/01/06 21:46:28 svasquez Exp $'
!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------- 
! Map routines - Interfaces to C++ STL map containers
!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilMapNameContainerAdd"
!BOPI
! !IROUTINE:  ESMF_UtilMapNameContainerAdd - Add a name/value pair to a map container
!
! !INTERFACE:
  subroutine ESMF_UtilMapNameContainerAdd (this, name, value, rc)
!
! !ARGUMENTS:
    type(ESMF_MapPtr), intent(in) :: this
    character(*),      intent(in) :: name
    type(ESMF_Pointer),intent(in) :: value
    integer,           intent(out), optional :: rc
!
! !Description:
! This method adds a name/value pair to a MapName container.  The
! name argument is used within the container to identify the pair
! for lookups.
!
! The arguments are:
! \begin{description}
! \item [{[this]}]
! A ESMF\_MapName object
! \item [{[name]}]
! A character string which will be the keyword used to identify the
! pair within the container.
! \item [{[value]}]
! A pointer associated with the name.
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI

    integer :: localrc

    localrc = ESMF_FAILURE

    if (this%this%ptr == 0) then
      if (present (rc))  &
        rc = ESMF_RC_PTR_NULL
      return
    end if

    call c_esmc_mapname_add (this, trim (name), value, localrc)

    if (present (rc))  &
      rc = localrc

  end subroutine ESMF_UtilMapNameContainerAdd

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilMapNameValueAdd"
!BOPI
! !IROUTINE:  ESMF_UtilMapNameValueAdd - Add a name/value pair to a map container
!
! !INTERFACE:
  subroutine ESMF_UtilMapNameValueAdd (this, name, value, rc)
!
! !ARGUMENTS:
    type(ESMF_MapPtr), intent(in) :: this
    character(*),      intent(in) :: name
    integer,           intent(in) :: value
    integer,           intent(out), optional :: rc
!
! !Description:
! This method adds a name/value pair to a MapName container.  The
! name argument is used within the container to identify the pair
! for lookups.
!
! The arguments are:
! \begin{description}
! \item [{[this]}]
! A ESMF\_MapName object
! \item [{[name]}]
! A character string which will be the keyword used to identify the
! pair within the container.
! \item [{[value]}]
! An integer value associated with the name.
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI

    integer :: localrc

    localrc = ESMF_FAILURE

    if (this%this%ptr == 0) then
      if (present (rc))  &
        rc = ESMF_RC_PTR_NULL
      return
    end if

    call c_esmc_mapname_add (this, trim (name), value, localrc)

    if (present (rc))  &
      rc = localrc

  end subroutine ESMF_UtilMapNameValueAdd

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilMapNameCreate"
!BOPI
! !IROUTINE:  ESMF_UtilMapNameCreate - Create a map container for name/value pairs
!
! !INTERFACE:
  subroutine ESMF_UtilMapNameCreate (this, rc)
!
! !ARGUMENTS:
    type(ESMF_MapPtr), intent(out) :: this
    integer,           intent(out), optional :: rc
!
! !Description:
! This method creates a MapName container.
!
! The arguments are:
! \begin{description}
! \item [{[this]}]
! A ESMF\_MapName object to be created.
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI

    integer :: localrc

    localrc = ESMF_FAILURE

    call c_esmc_mapname_create (this, localrc)

    if (present (rc))  &
      rc = localrc

  end subroutine ESMF_UtilMapNameCreate

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilMapNameDestroy"
!BOPI
! !IROUTINE:  ESMF_UtilMapNameDestroy - Destroy a map container
!
! !INTERFACE:
  subroutine ESMF_UtilMapNameDestroy (this, rc)
!
! !ARGUMENTS:
    type(ESMF_MapPtr), intent(inout) :: this
    integer,           intent(out), optional :: rc
!
! !Description:
! This method destroys a MapName container.
!
! The arguments are:
! \begin{description}
! \item [{[this]}]
! A ESMF\_MapName object to be destroyed.
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI

    integer :: localrc

    localrc = ESMF_FAILURE

    if (this%this%ptr == 0) then
      if (present (rc))  &
        rc = ESMF_RC_PTR_NULL
      return
    end if

    call c_esmc_mapname_destroy (this, localrc)

    if (present (rc))  &
      rc = localrc

  end subroutine ESMF_UtilMapNameDestroy

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilMapNameContainerLookup"
!BOPI
! !IROUTINE:  ESMF_UtilMapNameContainerLookup - Return the pointer associated with a name
!
! !INTERFACE:
  subroutine ESMF_UtilMapNameContainerLookup (this, name, value, foundFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_MapPtr), intent(in)  :: this
    character(*),      intent(in)  :: name
    type(ESMF_Pointer),intent(out) :: value
    logical,           intent(out) :: foundFlag
    integer,           intent(out), optional :: rc
!
! !Description:
! This method looks up a name/value pair in a MapName container
! and returns the value.
!
! The arguments are:
! \begin{description}
! \item [{[this]}]
! A ESMF\_MapName object
! \item [{[name]}]
! A character string which will be the keyword used to identify the
! pair within the container.
! \item [{[value]}]
! The pointer associated with the name.
! \item [{[foundFlag]}]
! Set to .TRUE. if the name is found, otherwise .FALSE.
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI

    type(ESMF_Logical) :: localfoundflag
    integer :: localrc

    localrc = ESMF_FAILURE

    if (this%this%ptr == 0) then
      if (present (rc))  &
        rc = ESMF_RC_PTR_NULL
      return
    end if

    call c_esmc_mapname_lookup (this, trim (name),  &
                                value, localfoundFlag, localrc)
    foundflag = localfoundflag

    if (present (rc))  &
      rc = localrc

  end subroutine ESMF_UtilMapNameContainerLookup

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilMapNameValueLookup"
!BOPI
! !IROUTINE:  ESMF_UtilMapNameValueLookup - Return the value associated with a name
!
! !INTERFACE:
  subroutine ESMF_UtilMapNameValueLookup (this, name, value, foundFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_MapPtr), intent(in)  :: this
    character(*),      intent(in)  :: name
    integer,           intent(out) :: value
    logical,           intent(out) :: foundFlag
    integer,           intent(out), optional :: rc
!
! !Description:
! This method looks up a name/value pair in a MapName container
! and returns the value.
!
! The arguments are:
! \begin{description}
! \item [{[this]}]
! A ESMF\_MapName object
! \item [{[name]}]
! A character string which will be the keyword used to identify the
! pair within the container.
! \item [{[value]}]
! The integer value associated with the name.
! \item [{[foundFlag]}]
! Set to .TRUE. if the name is found, otherwise .FALSE.
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI

    type(ESMF_Logical) :: localfoundflag
    integer :: localrc

    localrc = ESMF_FAILURE

    if (this%this%ptr == 0) then
      if (present (rc))  &
        rc = ESMF_RC_PTR_NULL
      return
    end if

    call c_esmc_mapname_lookup (this, trim (name),  &
                                value, localfoundFlag, localrc)
    foundflag = localfoundflag

    if (present (rc))  &
      rc = localrc

  end subroutine ESMF_UtilMapNameValueLookup

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilMapNamePrint"
!BOPI
! !IROUTINE:  ESMF_UtilMapNamePrint - Print the name/value pairs 
!
! !INTERFACE:
  subroutine ESMF_UtilMapNamePrint (this, title, rc)
!
! !ARGUMENTS:
    type(ESMF_MapPtr), intent(in) :: this
    character(*),      intent(in),  optional :: title
    integer,           intent(out), optional :: rc
!
! !Description:
! This method prints the name/value pair from a MapName container to
! stdout.
!
! The arguments are:
! \begin{description}
! \item [{[this]}]
! A ESMF\_MapName object
! \item [{[title]}]
! Title of the print out
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI

    integer :: localrc, ignorerc

    localrc = ESMF_FAILURE

    if (present (title)) then
      print *, trim (title)
    else
      print *, "ESMF_UtilMapNamePrint:"
    end if

    if (this%this%ptr == 0) then
      print *, " *** NULL MapName pointer ***"
      if (present (rc))  &
        rc = ESMF_SUCCESS
      return
    end if

    call ESMF_UtilIOUnitFlush (ESMF_UtilIOstderr, rc=ignorerc)
    call ESMF_UtilIOUnitFlush (ESMF_UtilIOstdout, rc=ignorerc)

    if (present (title)) then
      call c_esmc_mapname_print (this, trim (title), localrc)
    else
      call c_esmc_mapname_print (this, "ESMF_UtilMapNamePrint:", localrc)
    end if

    if (present (rc))  &
      rc = localrc

  end subroutine ESMF_UtilMapNamePrint

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilMapNameRemove"
!BOPI
! !IROUTINE:  ESMF_UtilMapNameRemove - Remove a name/value pair
!
! !INTERFACE:
  subroutine ESMF_UtilMapNameRemove (this, name, rc)
!
! !ARGUMENTS:
    type(ESMF_MapPtr), intent(in) :: this
    character(*),      intent(in) :: name
    integer,           intent(out), optional :: rc
!
! !Description:
! This method removes a name/value pair from a MapName container.
!
! The arguments are:
! \begin{description}
! \item [{[this]}]
! A ESMF\_MapName object
! \item [{[name]}]
! A character string which will be the keyword used to identify the
! pair within the container.
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI

    integer :: localrc

    localrc = ESMF_FAILURE

    if (this%this%ptr == 0) then
      if (present (rc))  &
        rc = ESMF_RC_PTR_NULL
      return
    end if

    call c_esmc_mapname_remove (this, trim (name), localrc)

    if (present (rc))  &
      rc = localrc

  end subroutine ESMF_UtilMapNameRemove

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UtilMapNameSize"
!BOPI
! !IROUTINE:  ESMF_UtilMapNameSize - Return the size of the map
!
! !INTERFACE:
  function ESMF_UtilMapNameSize (this, rc)
!
! !RETURN VALUE:
    integer :: ESMF_UtilMapNameSize
!
! !ARGUMENTS:
    type(ESMF_MapPtr), intent(in) :: this
    integer,           intent(out), optional :: rc
!
! !Description:
! This method returns the number of elements contained within
! a MapName container.
!
! The arguments are:
! \begin{description}
! \item [{[this]}]
! A ESMF\_MapName object
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!EOPI

    integer :: localrc

    localrc = ESMF_FAILURE

    if (this%this%ptr == 0) then
      ESMF_UtilMapNameSize = 0
      if (present (rc))  &
        rc = ESMF_SUCCESS
      return
    end if

    call c_esmc_mapname_sizeget (this, ESMF_UtilMapNameSize, localrc)

    if (present (rc))  &
      rc = localrc

  end function ESMF_UtilMapNameSize

!------------------------------------------------------------------------- 

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
#define ESMF_METHOD "ESMF_Array2String"
!BOPI
!  !IROUTINE:  ESMF_Array2String - convert character array to string
!  
! !INTERFACE: 
      function ESMF_Array2String (charArray) 
!
! !ARGUMENTS:
      character(len=1), intent(in) :: charArray(:)

!
! !RETURN VALUE:
      character(len=size (charArray)) :: ESMF_Array2String

!
! !DESCRIPTION:
!   Converts given an array of characters to a string.
!
!     The arguments are:
!     \begin{description}
!     \item[charArray]
!       An array of characters.
!     \end{description}
!
!
!EOPI

      ESMF_Array2String = transfer (charArray, mold=ESMF_Array2String)

      end function ESMF_Array2String

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_String2Array"
!BOPI
!  !IROUTINE:  ESMF_String2Array - convert character string to array
!  
! !INTERFACE: 
      function ESMF_String2Array (string) 
!
! !ARGUMENTS:
      character(len=*), intent(in) :: string
!
! !RETURN VALUE:
      character(len=1) :: ESMF_String2Array(len (string))

!
! !DESCRIPTION:
!   Converts given a string to an array of characters.
!
!     The arguments are:
!     \begin{description}
!     \item[string]
!       A character string.
!     \end{description}
!
!
!EOPI

      ESMF_String2Array = transfer (string, mold=ESMF_String2Array)

      end function ESMF_String2Array

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StringLowerCase"
!BOPI
!  !IROUTINE:  ESMF_StringLowerCase - convert string to lowercase
!  
! !INTERFACE: 
      subroutine ESMF_StringLowerCase(string, rc) 
!
! !ARGUMENTS:
      character(len=*), intent(inout) :: string
      integer, intent(out), optional  :: rc  

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
!EOPI

      integer :: shift, i
      character(len=1) :: c

      shift = ichar('a') - ichar('A')
      do i = 1, len(string)
        c = string(i:i)
        if(c .ge. 'A' .and. c .le. 'Z') then
          string(i:i) = char(ichar(c) + shift)
        endif
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StringLowerCase

!------------------------------------------------------------------------- 
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StringUpperCase"
!BOPI
!  !IROUTINE:  ESMF_StringUpperCase - convert string to uppercase
!  
! !INTERFACE: 
      subroutine ESMF_StringUpperCase(string, rc) 
!
! !ARGUMENTS:
      character(len=*), intent(inout) :: string
      integer, intent(out), optional  :: rc  

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
!EOPI

      integer :: shift, i
      character(len=1) :: c

      shift = ichar('a') - ichar('A')
      do i = 1, len(string)
        c = string(i:i)
        if(c .ge. 'a' .and. c .le. 'z') then
          string(i:i) = char(ichar(c) - shift)
        endif
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StringUpperCase

!------------------------------------------------------------------------- 
!------------------------------------------------------------------------- 
! misc print routines
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
