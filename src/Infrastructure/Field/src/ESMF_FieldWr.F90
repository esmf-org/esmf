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
#define ESMF_FILENAME "ESMF_FieldWr.F90"
!==============================================================================
!
!     ESMF FieldWr module
module ESMF_FieldWrMod
!
!==============================================================================
!
! This file contains the Field class definition and all Field
! class method.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_UtilMod
  use ESMF_BaseMod
  use ESMF_VMMod
  use ESMF_LogErrMod
  use ESMF_ArraySpecMod
  use ESMF_ArrayMod
  use ESMF_FieldMod
  use ESMF_FieldGetMod
  use ESMF_GridMod
  use ESMF_InitMacrosMod
  use ESMF_IOMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
  public ESMF_FieldWrite              ! Write Field to a file

!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldWrite"

!BOP
! !IROUTINE:  ESMF_FieldWrite - Write Field data into a file
! \label{api:FieldWrite}

! !INTERFACE:
  subroutine ESMF_FieldWrite(field, fileName, keywordEnforcer,   &
      variableName, convention, purpose, overwrite, status, timeslice, iofmt, rc)
!
!
! !ARGUMENTS:
    type(ESMF_Field),           intent(in)             :: field 
    character(*),               intent(in)             :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(*),               intent(in),  optional  :: variableName
    character(*),               intent(in),  optional  :: convention
    character(*),               intent(in),  optional  :: purpose
    logical,                    intent(in),  optional  :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional  :: status
    integer,                    intent(in),  optional  :: timeslice
    type(ESMF_IOFmt_Flag),      intent(in),  optional  :: iofmt
    integer,                    intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Write Field data into a file.  For this API to be functional, the 
!   environment variable {\tt ESMF\_PIO} should be set to "internal" when 
!   the ESMF library is built.  Please see the section on 
!   Data I/O,~\ref{io:dataio}.
!
!   When {\tt convention} and {\tt purpose} arguments are specified,
!   a NetCDF variable can be created with user-specified dimension labels and
!   attributes.  Dimension labels may be defined for both gridded and
!   ungridded dimensions.  Dimension labels for gridded dimensions are specified
!   at the Grid level by attaching an ESMF Attribute package to it.  The Attribute
!   package must contain an attribute named by the pre-defined ESMF parameter
!   {\tt ESMF\_ATT\_GRIDDED\_DIM\_LABELS}.  The corresponding value is an array of
!   character strings specifying the desired names of the dimensions.  Likewise,
!   for ungridded dimensions, an Attribute package is attached at the Field level.
!   The name of the name must be {\tt ESMF\_ATT\_UNGRIDDED\_DIM\_LABELS}.
!
!   NetCDF attributes for the variable can also be specified.  As with dimension labels,
!   an Attribute package is added to the Field with the desired names and values.
!   A value may be either a scalar character string, or a scalar or array of type
!   integer, real, or double precision.  Dimension label attributes can co-exist with
!   variable attributes within a common Attribute package.
!
!   Limitations:
!   \begin{itemize}
!     \item Only single tile Fields are supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item [field]
!     The {\tt ESMF\_Field} object that contains data to be written.
!   \item[fileName]
!     The name of the output file to which Field data is written.
!   \item[{[variableName]}]
!    Variable name in the output file; default is the "name" of field.
!    Use this argument only in the I/O format (such as NetCDF) that
!    supports variable name. If the I/O format does not support this
!    (such as binary format), ESMF will return an error code.
!   \item[{[convention]}]
!     Specifies an Attribute package associated with the Field, used to create NetCDF
!     dimension labels and attributes for the variable in the file.  When this argument is present,
!     the {\tt purpose} argument must also be present.  Use this argument only with a NetCDF
!     I/O format. If binary format is used, ESMF will return an error code.
!   \item[{[purpose]}]
!     Specifies an Attribute package associated with the Field, used to create NetCDF
!     dimension labels and attributes for the variable in the file.  When this argument is present,
!     the {\tt convention} argument must also be present.  Use this argument only with a NetCDF
!     I/O format. If binary format is used, ESMF will return an error code.
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
!    time slices.  An unlimited dimension called {\tt time} is defined in the
!    file variable for this capability.
!    The {\tt timeslice} argument provides access to the {\tt time} dimension,
!    and must have a positive value. The behavior of this
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
!     \begin{sloppypar}
!    The I/O format.  Please see Section~\ref{opt:iofmtflag} for the list
!    of options. If not present, file names with a {\tt .bin} extension will
!    use {\tt ESMF\_IOFMT\_BIN}, and file names with a {\tt .nc} extension
!    will use {\tt ESMF\_IOFMT\_NETCDF}.  Other files default to
!    {\tt ESMF\_IOFMT\_NETCDF}.
!     \end{sloppypar}
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
    integer                         :: localrc
    character(len=ESMF_MAXSTR)      :: name
    type(ESMF_FieldType), pointer   :: fp 
    type(ESMF_Array)                :: array
    type(ESMF_Grid)                 :: grid
    type(ESMF_Base), pointer        :: base_dummy => null ()
    type(ESMF_FieldStatus_Flag)     :: fieldstatus       ! Field's status
    logical                         :: opt_overwriteflag ! helper variable
    type(ESMF_FileStatus_Flag)      :: opt_status        ! helper variable
    type(ESMF_IOFmt_Flag)           :: opt_iofmt
    type(ESMF_IO)                   :: io                ! The I/O object
    integer                         :: file_ext_p
    integer                         :: ndims
    logical                         :: errorFound        ! True if err cond.

#ifdef ESMF_PIO
!   Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    errorFound = .false.

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

    ! Set default flags
    opt_overwriteflag = .false.
    if (present(overwrite)) opt_overwriteflag = overwrite

    opt_status = ESMF_FILESTATUS_UNKNOWN
    if (present(status)) opt_status = status

    ! Set iofmt based on file name extension (if present)
    if (present (iofmt)) then
      opt_iofmt = iofmt
    else
      if (index (fileName, '.') > 0) then
        file_ext_p = index (fileName, '.', back=.true.)
        select case (fileName(file_ext_p:))
        case ('.nc')
          opt_iofmt = ESMF_IOFMT_NETCDF
        case ('.bin')
          opt_iofmt = ESMF_IOFMT_BIN
        case default
          opt_iofmt = ESMF_IOFMT_NETCDF
        end select
      else
        opt_iofmt = ESMF_IOFMT_NETCDF
      end if
    end if

    ! Attributes
    if (present (convention) .neqv. present (purpose)) then
      if (ESMF_LogFoundError (ESMF_RC_ARG_WRONG,  &
          msg='Both convention and purpose must be specified',  &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    fp => field%ftypep
    if (present(variableName)) then
      name = variableName
    else
      call ESMF_GetName(fp%base, name, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                &
           ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    call ESMF_FieldGet(field, status=fieldstatus, array=array, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                &
         ESMF_CONTEXT, rcToReturn=rc)) return
    if (fieldstatus .ne. ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD,                &
           msg="Uninitialized Field: field does not have an array",   &
           ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present (convention)) then
      call ESMF_FieldGet (field, grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    ! Create an I/O object
    io = ESMF_IOCreate(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! From here on out, we need to clean up so no returning on error
    if (localrc .eq. ESMF_SUCCESS) then
      call c_esmc_fieldioaddarray(io, fp%base, array, grid, name,  &
          base_dummy, convention, purpose,  &
          localrc)
      errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,     &
          ESMF_CONTEXT, rcToReturn=rc)
    endif

    if (.not. errorfound) then
      call ESMF_IOWrite(io, trim(fileName),  &
          overwrite=opt_overwriteflag,  &
          status=opt_status, timeslice=timeslice, iofmt=opt_iofmt, rc=localrc)
      errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,     &
          ESMF_CONTEXT, rcToReturn=rc)
    endif

    ! Set rc here in case we had an error but destroy succeeds
    if (present(rc)) rc = localrc

    call ESMF_IODestroy(io, rc=localrc)
    ! Log error but don't reset rc
    errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,       &
        ESMF_CONTEXT, rcToReturn=localrc)

    ! Last chance to return an error code (IODestroy failed)
    if (present(rc)) then
      if (rc == ESMF_SUCCESS) rc = localrc
    end if

#else
    ! Return indicating PIO not present
    if (present(rc)) rc = ESMF_RC_LIB_NOT_PRESENT
#endif

  end subroutine ESMF_FieldWrite

!------------------------------------------------------------------------------

end module ESMF_FieldWrMod
