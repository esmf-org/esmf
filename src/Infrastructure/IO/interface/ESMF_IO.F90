! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_IO.F90"
!==============================================================================
!
!     ESMF I/O Module
module ESMF_IOMod

!     
!==============================================================================
!     
! This file contains the I/O class definition and all I/O class methods.
!     
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_IOMod
!     
! !DESCRIPTION:
! I/O Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class {\tt ESMCI\_IO} implementation.
!     
! See {\tt ../include/ESMCI\_IO.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
  ! inherit from ESMF base class
  use ESMF_UtilTypesMod
  use ESMF_BaseMod
  use ESMF_InitMacrosMod
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_RHandleMod
  use ESMF_F90InterfaceMod  ! ESMF Fortran-C++ interface helper
  ! We need some ESMF types
  use ESMF_ArrayMod
  use ESMF_VMMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
!
!      ! File format
!      type ESMF_IOFileFormat
!      sequence
!      private
!         integer :: iofileformat
!      end type
!
!      ! Predefined file formats
!      type(ESMF_IOFileFormat), parameter :: &
!                       ESMF_IO_FILEFORMAT_UNSPECIFIED = ESMF_IOFileFormat(0), &
!                       ESMF_IO_FILEFORMAT_NETCDF      = ESMF_IOFileFormat(1), &
!                       ESMF_IO_FILEFORMAT_HDF         = ESMF_IOFileFormat(2), &
!                       ESMF_IO_FILEFORMAT_XML         = ESMF_IOFileFormat(3)
!
!      ! What type of I/O - Read only, write only, R/W, append with truncation
!      type ESMF_IORWType
!      sequence
!      private
!         integer :: iorwtype
!      end type
!
!      type(ESMF_IORWType), parameter :: &
!                             ESMF_IO_RWTYPE_UNSPECIFIED = ESMF_IORWType(0), &
!                             ESMF_IO_RWTYPE_READONLY    = ESMF_IORWType(1), &
!                             ESMF_IO_RWTYPE_WRITEONLY   = ESMF_IORWType(2), &
!                             ESMF_IO_RWTYPE_READWRITE   = ESMF_IORWType(3), &
!                             ESMF_IO_RWTYPE_APPEND      = ESMF_IORWType(4), &
!                             ESMF_IO_RWTYPE_TRUNCATE    = ESMF_IORWType(5)
!
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! ! ESMF_IO
!
!------------------------------------------------------------------------------

  ! Fortran class type to hold pointer to C++ object
  type ESMF_IO
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_IO
!      public ESMF_IOFileFormat, ESMF_IORWType
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
  public ESMF_IOAddArray
  !public ESMF_IOClose
  public ESMF_IOClear
  public ESMF_IOCreate
  public ESMF_IODestroy
  !public ESMF_IOFlush 
  !public ESMF_IOGet     
  !public ESMF_IOOpen
  !public ESMF_IOPrint
  public ESMF_IORead
  !public ESMF_IOReadRestart
  !public ESMF_IOSet
  !public ESMF_IOValidate
  public ESMF_IOWrite
  !public ESMF_IOWriteRestart
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
       '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!==============================================================================

contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOCreate()"
!BOPI
! !IROUTINE: ESMF_IOCreate - Create a new ESMF I/O object
!
! !INTERFACE:
  function ESMF_IOCreate(keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_IO) :: ESMF_IOCreate

! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Creates a new {\tt ESMF\_IO} object.    
!
!     The arguments are:
!     \begin{description}
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOPI

    ! local return code
    integer :: localrc
!
!   ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
!
!   invoke C to C++ entry point to allocate and initialize new io
    call c_ESMC_IOCreate(ESMF_IOCreate, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_IOCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IODestroy()"
!BOPI
! !IROUTINE: ESMF_IODestroy - Clean up and delete an ESMF I/O object
!
! !INTERFACE:
  subroutine ESMF_IODestroy(io, rc)
!
! !ARGUMENTS:
    type(ESMF_IO)                  :: io
    integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!     Releases resources associated with this {\tt ESMF\_IO} object.
!     This includes deleting the {\tt ESMF\_IO} object itself.
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!       Destroy contents of this {\tt ESMF\_IO} object.
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

!   invoke C to C++ entry point
    call c_ESMC_IODestroy(io, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) then 
      write(*,*)" c_ESMC_IODestroy failed"
      return
    endif

    ! Return success
    if (present(rc)) rc = localrc
  end subroutine ESMF_IODestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOClear()"
!BOPI
! !IROUTINE: ESMF_IOClear - Clears the I/O queue of an ESMF I/O object
!
! !INTERFACE:
  subroutine ESMF_IOClear(io)
!
! !ARGUMENTS:
    type(ESMF_IO)                  :: io
!     
! !DESCRIPTION:
!     Clears the I/O queue of this {\tt ESMF\_IO} object.
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!       Clear the I/O queue of this {\tt ESMF\_IO} object.
!     \end{description}
!
!EOPI

!   invoke C to C++ entry point
    call c_ESMC_IOClear(io)

  end subroutine ESMF_IOClear

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IOAddArray()"
!BOPI
! !IROUTINE: ESMF_IOAddArray - Add an array to an I/O object's element list

! !INTERFACE:
  subroutine ESMF_IOAddArray(io, array, keywordEnforcer,  &
                             variableName, convention, purpose, rc)

! !ARGUMENTS:
    type(ESMF_IO),           intent(in)            :: io
    type(ESMF_Array),        intent(in)            :: array
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(*),            intent(in),  optional :: variableName
    character(*),            intent(in),  optional :: convention
    character(*),            intent(in),  optional :: purpose
    integer,                 intent(out), optional :: rc
   
! !DESCRIPTION:
!     Gets one or more of the properties of an {\tt ESMF\_IO} object.
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!          The {\tt ESMF\_IO} object instance to modify
!     \item[array]
!          The {\tt ESMF\_Array} object to add to io's element list
!     \item[{[variableName]}]
!          Optional variableName to attach to this array for I/O purposes
!     \item[{[convention]}]
!          Optional convention - AttPack for attribute and dimension names and values
!     \item[{[purpose]}]
!          Optional purpose - AttPack for attribute and dimension names and values
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer                     :: localrc     ! local return code
    integer                     :: len_varName ! name length or 0

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    !   invoke C to C++ entry point
    if (present(variableName)) then
      len_varName = len_trim(variableName)
    else
      len_varName = 0
    endif

    call c_ESMC_IOAddArray(io, array,  &
        variableName, len_varName,     &
        convention, purpose,           &
        localrc)

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IOAddArray

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IORead - Perform a read on an ESMF I/O object
!
! !INTERFACE:
  subroutine ESMF_IORead(io, fileName, keywordEnforcer, timeslice,           &
                         iofmt, schema, rc)
!
! !ARGUMENTS:
    type(ESMF_IO),            intent(in)            :: io
    character (len=*),        intent(in)            :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                  intent(in),  optional :: timeslice
    type(ESMF_IOFmt_Flag),    intent(in),  optional :: iofmt
    character (len=*),        intent(in),  optional :: schema
    integer,                  intent(out), optional :: rc
   
! !DESCRIPTION:
!     Perform a read on an {\tt ESMF\_IO} object.  Any properties specified
!     will override, but not reset, those previously set on the io object.
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!          The object instance to read.
!     \item[{[fileName]}]
!          The file name to be read from.
!     \item[{[timeslice]}]
!      The time-slice number of the variable read from file.
!     \item[{[iofmt]}]
!      \begin{sloppypar}
!      The I/O format.  Please see Section~\ref{opt:iofmtflag} for the list
!      of options. If not present, defaults to {\tt ESMF\_IOFMT\_NETCDF}.
!      \end{sloppypar}
!     \item[{[schema]}]
!          Selects, for reading, the Attribute package of the ESMF
!          objects included in <io> (e.g., with ESMF_IOAddArray)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
! !REQUIREMENTS:

    integer               :: localrc
    integer               :: len_fileName         ! filename length or 0
    type(ESMF_IOFmt_Flag) :: opt_iofmt            ! helper variable
    integer               :: len_schema           ! schema string length or 0

     ! Assume failure until success
     if (present(rc)) rc = ESMF_RC_NOT_IMPL
     localrc = ESMF_RC_NOT_IMPL

    ! Grab the filename length for the C++ level
    len_fileName = len_trim(fileName)

    ! Set default flags
    opt_iofmt = ESMF_IOFMT_NETCDF;
    if ( present(iofmt)) opt_iofmt = iofmt

    ! Grab the schema string length for the C++ level
    if (present(schema)) then
      len_schema = len_trim(schema)
    else
      len_schema = 0
    endif

!     invoke C to C++ entry point  TODO
    call c_ESMC_IORead(io, fileName, len_fileName, opt_iofmt,    &
        timeslice, schema, len_schema, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,           &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IORead
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IOWrite - Perform a write on an ESMF I/O object
!
! !INTERFACE:
  subroutine ESMF_IOWrite(io, fileName, keywordEnforcer,  &
                          overwrite, status,  &
                          timeslice, iofmt, schema, rc)
!
! !ARGUMENTS:
    type(ESMF_IO),              intent(in)            :: io
    character (len=*),          intent(in)            :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                    intent(in),  optional :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional :: status
    integer,                    intent(in),  optional :: timeslice
    type(ESMF_IOFmt_Flag),      intent(in),  optional :: iofmt
    character (len=*),          intent(in),  optional :: schema
    integer,                    intent(out), optional :: rc
   
! !DESCRIPTION:
!   Perform a write on an {\tt ESMF\_IO} object.  Any properties specified
!   will override, but not reset, those previously set on the io object.
!
!   The arguments are:
!   \begin{description}
!   \item[io]
!        The object instance to write.
!   \item[fileName]
!        The file name to be writtten to.
!   \item[{[overwrite]}]
!    \begin{sloppypar}
!      A logical flag, the default is .false., i.e., existing field data may
!      {\em not} be overwritten. If .true., the overwrite behavior depends
!      on the value of {\tt iofmt} as shown below:
!    \begin{description}
!    \item[{\tt iofmt} = {\tt ESMF\_IOFMT\_BIN}:]\ All data in the file will
!      be overwritten with each field's data.
!    \item[{\tt iofmt} = {\tt ESMF\_IOFMT\_NETCDF}:]\ Only the
!      data corresponding to each field's name will be
!      be overwritten. If the {\tt timeslice} option is given, only data for
!      the given timeslice may be overwritten.
!      Note that it is always an error to attempt to overwrite a NetCDF
!      variable with data which has a different shape.
!    \end{description}
!    \end{sloppypar}
!   \item[{[status]}]
!    \begin{sloppypar}
!    The file status. Please see Section~\ref{const:filestatusflag} for
!    the list of options. If not present, defaults to
!    {\tt ESMF\_FILESTATUS\_UNKNOWN}.
!    \end{sloppypar}
!   \item[{[timeslice]}]
!    \begin{sloppypar}
!    Some I/O formats (e.g. NetCDF) support the output of data in form of
!    time slices. The {\tt timeslice} argument provides access to this
!    capability. {\tt timeslice} must be positive. The behavior of this
!    option may depend on the setting of the {\tt overwrite} flag:
!    \begin{description}
!    \item[{\tt overwrite = .false.}:]\ If the timeslice value is
!    less than the maximum time already in the file, the write will fail.
!    \item[{\tt overwrite = .true.}:]\ Any positive timeslice value is valid.
!    \end{description}
!    By default, i.e. by omitting the {\tt timeslice} argument, no
!    provisions for time slicing are made in the output file,
!    however, if the file already contains a time axis for the variable,
!    a timeslice one greater than the maximum will be written.
!    \end{sloppypar}
!   \item[{[iofmt]}]
!        The file format to be used during the read. Default is
!        ESMF_IOFMT_NETCDF
!   \item[{[schema]}]
!        Selects, for reading, the Attribute package of the ESMF
!        objects included in <io> (e.g., with ESMF_IOAddArray)
!   \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI
! !REQUIREMENTS:
!
    integer                    :: localrc
    integer                    :: len_fileName       ! filename length or 0
    type(ESMF_Logical)         :: opt_overwriteflag  ! helper variable
    type(ESMF_FileStatus_Flag) :: opt_status         ! helper variable
    type(ESMF_IOFmt_Flag)      :: opt_iofmt          ! helper variable
    integer                    :: len_schema         ! schema string len or 0

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Set default flags
    opt_overwriteflag = ESMF_FALSE
    if (present(overwrite)) then
      if (overwrite) opt_overwriteflag = ESMF_TRUE
    end if

    opt_status = ESMF_FILESTATUS_UNKNOWN
    if (present(status)) opt_status = status

    opt_iofmt = ESMF_IOFMT_NETCDF;
    if ( present(iofmt)) opt_iofmt = iofmt

    ! Grab the filename length for the C++ level
    len_fileName = len_trim(fileName)

    ! Grab the schema string length for the C++ level
    if (present(schema)) then
      len_schema = len_trim(schema)
    else
      len_schema = 0
    endif

!   invoke C to C++ entry point  TODO
    call c_ESMC_IOWrite(io, fileName, len_fileName, opt_iofmt,   &
         opt_overwriteflag, opt_status, timeslice,               &
         schema, len_schema, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,           &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IOWrite

end module ESMF_IOMod
