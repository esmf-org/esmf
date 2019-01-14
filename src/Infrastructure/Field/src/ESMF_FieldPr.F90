! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_FieldPr.F90"
!==============================================================================
!
!     ESMF FieldPr module
module ESMF_FieldPrMod
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
  use ESMF_LocalArrayMod
  use ESMF_DELayoutMod
  use ESMF_StaggerLocMod
  use ESMF_DistGridMod
  use ESMF_GridMod
  use ESMF_GeomBaseMod
  use ESMF_ArrayMod
  use ESMF_ArrayCreateMod
  use ESMF_ArrayGetMod
  use ESMF_TimeMod
  use ESMF_InitMacrosMod
  use ESMF_IOUtilMod
  use ESMF_IOMod

  use ESMF_FieldMod
  use ESMF_FieldGetMod

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
  public ESMF_FieldPrint              ! Print contents of a Field
  public ESMF_FieldRead               ! Read  Field data from a file

!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldPrint"

!BOP
! !IROUTINE:  ESMF_FieldPrint - Print Field information

! !INTERFACE:
  subroutine ESMF_FieldPrint(field, keywordEnforcer, rc)
!
!
! !ARGUMENTS:
    type(ESMF_Field), intent(in)            :: field
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Prints information about the {\tt field} to {\tt stdout}.
!     This subroutine goes through the internal data members of a field
!     data type and prints information of each data member. \\
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           An {\tt ESMF\_Field} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

    character(len=ESMF_MAXSTR)      :: name, str
    type(ESMF_FieldType), pointer   :: fp
    integer                         :: i, localrc
    integer                         :: gridrank, arrayrank
    character(len=6)                :: defaultopts
    type(ESMF_Status)               :: fieldstatus

!       Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

    ! print option is not implemented, but it has to pass to c_ESMC_BasePrint()
    defaultopts = "brief"

    fp => field%ftypep

    call ESMF_BaseGetStatus(fp%base, fieldstatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    !nsc call ESMF_LogWrite("Field Print:", ESMF_LOGMSG_INFO)
    write(ESMF_UtilIOStdout,*) "Field Print Starts ====>"

    call ESMF_StatusString(fieldstatus, str, localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    write(ESMF_UtilIOStdout,*)  "Field base status = ", trim(str)

    if (fieldstatus .ne. ESMF_STATUS_READY) then
      write(ESMF_UtilIOStdout,*) "Empty or Uninitialized Field"
      write(ESMF_UtilIOStdout,*) "Field Print Ends   ====>"
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    if (.not. associated(field%ftypep)) then
      !jw  call ESMF_LogWrite("Empty or Uninitialized Field", ESMF_LOGMSG_INFO)
      write(ESMF_UtilIOStdout,*) "Empty or Uninitialized Field"
      write(ESMF_UtilIOStdout,*) "Field Print Ends   ====>"
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    call ESMF_GetName(fp%base, name, localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !jw  write(msgbuf, *)  "  Name = '",  trim(name), "'"
    !jw  call ESMF_LogWrite(msgbuf, ESMF_LOGMSG_INFO)
    write(ESMF_UtilIOStdout,*)  "  Name = '",  trim(name), "'"

    call ESMF_BasePrint(fp%base, options=defaultopts, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    write(ESMF_UtilIOStdout,*)  "Field status = ", fp%status
    if (fp%status .eq. ESMF_FIELDSTATUS_GRIDSET .or. &
         fp%status .eq. ESMF_FIELDSTATUS_COMPLETE) then
!      call ESMF_GeomBasePrint(fp%geombase, "", localrc)
!      if (ESMF_LogFoundError(localrc, &
!          ESMF_ERR_PASSTHRU, &
!          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_GeomBaseGet(fp%geombase, dimCount=gridrank, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      write(ESMF_UtilIOStdout,*) "gridrank = ", gridrank
    endif

    if (fp%status .eq. ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_ArrayPrint(fp%array, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_ArrayGet(fp%array, rank=arrayrank, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      write(ESMF_UtilIOStdout,*) "arrayrank = ", arrayrank
    endif

    write(ESMF_UtilIOStdout,*) "gridToFieldMap ungriddedLBound ungriddedUBound totalLWidth", &
          " totalUWidth"
    do i = 1, ESMF_MAXDIM
      write(ESMF_UtilIOStdout,*) fp%gridToFieldMap(i), fp%ungriddedLBound(i), fp%ungriddedUBound(i), &
            "    ", fp%totalLWidth(i), fp%totalUWidth(i)
    enddo
    write(ESMF_UtilIOStdout,*) "Field Print Ends   ====>"

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_FieldPrint


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRead"

!BOP
! !IROUTINE:  ESMF_FieldRead - Read Field data from a file
! \label{api:FieldRead}

! !INTERFACE:
  subroutine ESMF_FieldRead(field, fileName, keywordEnforcer,        &
      variableName, timeslice, iofmt, rc)
!
!
! !ARGUMENTS:
    type(ESMF_Field),      intent(inout)          :: field
    character(*),          intent(in)             :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(*),          intent(in),  optional  :: variableName
    integer,               intent(in),  optional  :: timeslice
    type(ESMF_IOFmt_Flag), intent(in),  optional  :: iofmt
    integer,               intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Read Field data from a file and put it into an {ESMF\_Field} object.
!   For this API to be functional, the environment variable {\tt ESMF\_PIO}
!   should be set to "internal" when the ESMF library is built.
!   Please see the section on Data I/O,~\ref{io:dataio}.
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
!     The {\tt ESMF\_Field} object in which the read data is returned.
!   \item[fileName]
!     The name of the file from which Field data is read.
!   \item[{[variableName]}]
!    Variable name in the file; default is the "name" of Field.
!    Use this argument only in the I/O format (such as NetCDF) that
!    supports variable name. If the I/O format does not support this
!    (such as binary format), ESMF will return an error code.
!   \item[timeslice]
!     Number of slices to be read from file, starting from the 1st slice
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
    character(len=ESMF_MAXSTR)      :: name
    type(ESMF_FieldType), pointer   :: fp
    type(ESMF_Array)                :: array
    integer                         :: localrc
    type(ESMF_FieldStatus_Flag)     :: fieldstatus  ! Field's status
    type(ESMF_IOFmt_Flag)           :: opt_iofmt
    type(ESMF_IO)                   :: io           ! The I/O object
    integer                         :: file_ext_p
    logical                         :: errorFound   ! True if error condition
    integer                         :: time

#ifdef ESMF_PIO
!   Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check variables
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

    time = 0
    if(present(timeslice)) time = timeslice

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

    if (present(variableName)) then
      name = variableName
    else
      fp => field%ftypep

      call ESMF_GetName(fp%base, name, localrc)
      if (ESMF_LogFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    call ESMF_FieldGet(field, array=array, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Create an I/O object
    io = ESMF_IOCreate(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,              &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! From here on out, we need to clean up so no returning on error
    if (localrc .eq. ESMF_SUCCESS) then
      call ESMF_IOAddArray(io, array, variableName=name, rc=localrc)
      errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,   &
          ESMF_CONTEXT, rcToReturn=rc)
    endif

    if (.not. errorfound) then
      call ESMF_IORead(io, trim(fileName), timeslice=time,              &
          iofmt=opt_iofmt, rc=localrc)
      errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,   &
          ESMF_CONTEXT, rcToReturn=rc)
    endif

    ! Set rc here in case we had an error but destroy succeeds
    if (present(rc)) rc = localrc

    call ESMF_IODestroy(io, rc=localrc)
    ! Log error but don't reset rc
    errorFound = ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,     &
        ESMF_CONTEXT, rcToReturn=localrc)

    ! Last chance to return an error code (IODestroy failed)
    if (present(rc)) then
       if (rc .eq. ESMF_SUCCESS) rc = localrc
    end if

#else
    ! Return indicating PIO not present
    if (present(rc)) rc = ESMF_RC_LIB_NOT_PRESENT
#endif

  end subroutine ESMF_FieldRead

!------------------------------------------------------------------------------

end module ESMF_FieldPrMod
