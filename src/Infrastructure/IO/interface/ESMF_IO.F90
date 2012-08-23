! $Id: ESMF_IO.F90,v 1.24 2012/08/23 19:29:47 gold2718 Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
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
!     ESMF IO Module
module ESMF_IOMod

!     
!==============================================================================
!     
! This file contains the IO class definition and all IO class methods.
!     
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_IOMod
!     
! !DESCRIPTION:
! IO Fortran API wrapper of C++ implementation.
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
  use ESMF_FieldMod
  use ESMF_FieldGetMod
  
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
  sequence
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
  !public ESMF_IOAddArray
  public ESMF_IOAddField
  !public ESMF_IOClose
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
!  public ESMF_IOWrite
  !public ESMF_IOWriteRestart
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
       '$Id: ESMF_IO.F90,v 1.24 2012/08/23 19:29:47 gold2718 Exp $'

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
! !IROUTINE: ESMF_IOCreate - Create a new ESMF IO object
!
! !INTERFACE:
  function ESMF_IOCreate(keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_IO) :: ESMF_IOCreate

! !ARGUMENTS:
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer !keywords req. below
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
! !IROUTINE: ESMF_IODestroy - Release resources associated with an ESMF IO object
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
!BOPI
! !IROUTINE: ESMF_IOAddField - Add a field to an IO object's element list

! !INTERFACE:
  subroutine ESMF_IOAddField(io, field, keywordEnforcer, variableName, rc)

! !ARGUMENTS:
    type(ESMF_IO),           intent(in)            :: io
    type(ESMF_Field),        intent(in)            :: field
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer !keywords req. below
    character (len=*),        intent(in), optional :: variableName
    integer,                 intent(out), optional :: rc
   
! !DESCRIPTION:
!     Gets one or more of the properties of an {\tt ESMF\_IO} object.
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!          The {\tt ESMF\_IO} object instance to modify
!     \item[field]
!          The {\tt ESMF\_Field} object to add to io's element list
!     \item[{[variableName]}]
!          Optional variableName to attach to this field for I/O purposes
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer                     :: localrc     ! local return code
    integer                     :: len_varName ! name length or 0
    type(ESMF_Array)            :: array       ! Field's array to pass
    type(ESMF_FieldStatus_Flag) :: status      ! Field's status

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Obtain the field's array (if present)
    call ESMF_FieldGet(field, status=status, array=array, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    if (status .ne. ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
           msg="Uninitialized Field: field does not have an array", &
           ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    !   invoke C to C++ entry point
    if (present(variableName)) then
      len_varName = len(trim(variableName))
    else
      len_varName = 0
    endif
    call c_ESMC_IOAddArray(io, array, variableName, len_varName, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IOAddField

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IORead - Perform a read on an ESMF IO object
!
! !INTERFACE:
  subroutine ESMF_IORead(io, fileName, keywordEnforcer, timeslice,           &
                         iofmt, schema, rc)
!
! !ARGUMENTS:
    type(ESMF_IO),           intent(in)            :: io
    character (len=*),       intent(in)            :: fileName
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer !keywords req. below
    integer,                 intent(in),  optional :: timeslice
    type(ESMF_IOFmtFlag),    intent(in),  optional :: iofmt
    character (len=*),       intent(in),  optional :: schema
    integer,                 intent(out), optional :: rc
   
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
!     \item[{[overwrite]}]
!          If .true. allow existing data fields to be overwritten.
!     \item[{[status]}]
!          Determines the action to take with regard to the file's status.
!     \item[{[timeslice]}]
!          Some IO formats (e.g. NetCDF) support the output of data in form of
!          time slices. The {\tt timeslice} argument provides access to this
!          capability. Usage of this feature requires that the first slice is
!          written with a positive {\tt timeslice} value, and that subsequent
!          slices are written with a {\tt timeslice} argument that increments
!          by one each time. By default, i.e. by omitting the {\tt timeslice}
!          argument, no provisions for time slicing are made in the output
!          file.
!     \item[{[iofmt]}]
!          The file format to be used during the read. Default is
!          ESMF_IOFMT_NETCDF
!     \item[{[schema]}]
!          Selects, for reading, the Attribute package of the ESMF
!          objects included in <io> (e.g., with ESMF_IOAddField)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
! !REQUIREMENTS:

     integer :: localrc

     ! Assume failure until success
     if (present(rc)) rc = ESMF_RC_NOT_IMPL
     localrc = ESMF_RC_NOT_IMPL
!
!     ! check variables  TODO
!     ESMF_INIT_CHECK_DEEP(ESMF_IOGetInit,io,rc)
!
!     invoke C to C++ entry point  TODO
!     call c_ESMC_IORead(io, <object>, fileFormat, fileName, &
!                       readWriteType, convention, purpose, localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!       ESMF_CONTEXT, rcToReturn=rc)) return
!
     ! Return success
     if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IORead
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IOWrite - Perform a write on an ESMF IO object
!
! !INTERFACE:
  subroutine ESMF_IOWrite(io, fileName, keywordEnforcer, overwrite, status,  &
                          timeslice, iofmt, schema, rc)
!
! !ARGUMENTS:
    type(ESMF_IO),           intent(in)            :: io
    character (len=*),       intent(in)            :: fileName
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer !keywords req. below
    logical,                 intent(in),  optional :: overwrite
    character (len=*),       intent(in),  optional :: status
    integer,                 intent(in),  optional :: timeslice
    type(ESMF_IOFmtFlag),    intent(in),  optional :: iofmt
    character (len=*),       intent(in),  optional :: schema
    integer,                 intent(out), optional :: rc
   
! !DESCRIPTION:
!     Perform a write on an {\tt ESMF\_IO} object.  Any properties specified
!     will override, but not reset, those previously set on the io object.
!
!     The arguments are:
!     \begin{description}
!     \item[io]
!          The object instance to write.
!     \item[fileName]
!          The file name to be writtten to.
!     \item[{[overwrite]}]
!          If .true. allow existing data fields to be overwritten.
!     \item[{[status]}]
!          Determines the action to take with regard to the file's status.
!     \item[{[timeslice]}]
!          Some IO formats (e.g. NetCDF) support the output of data in form of
!          time slices. The {\tt timeslice} argument provides access to this
!          capability. Usage of this feature requires that the first slice is
!          written with a positive {\tt timeslice} value, and that subsequent
!          slices are written with a {\tt timeslice} argument that increments
!          by one each time. By default, i.e. by omitting the {\tt timeslice}
!          argument, no provisions for time slicing are made in the output
!          file.
!     \item[{[iofmt]}]
!          The file format to be used during the read. Default is
!          ESMF_IOFMT_NETCDF
!     \item[{[schema]}]
!          Selects, for reading, the Attribute package of the ESMF
!          objects included in <io> (e.g., with ESMF_IOAddField)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
! !REQUIREMENTS:
!
    integer   :: localrc
    integer   :: len_fileName ! filename length or 0

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    len_fileName = len(trim(fileName))

!   invoke C to C++ entry point  TODO
    call c_ESMC_IOWrite(io, fileName, len_fileName, iofmt,      &
         overwrite, timeslice, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IOWrite

end module ESMF_IOMod
