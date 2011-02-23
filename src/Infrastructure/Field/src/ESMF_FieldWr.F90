! $Id: ESMF_FieldWr.F90,v 1.15 2011/02/23 20:10:08 w6ws Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
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
  use ESMF_InitMacrosMod

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
      subroutine ESMF_FieldWrite(field, file, keywordEnforcer, &
        append, timeslice, iofmt, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field),     intent(inout)          :: field 
      character(*),         intent(in)             :: file 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      logical,              intent(in),  optional  :: append
      integer,              intent(in),  optional  :: timeslice
      type(ESMF_IOFmtFlag), intent(in),  optional  :: iofmt
      integer,              intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Write Field data into a file.  For this API to be functional, the 
!   environment variable {\tt ESMF\_PIO} should be set to "internal" when 
!   the ESMF library is built.  Please see the section on 
!   Data I/O,~\ref{io:dataio}.
!
!   Limitations:
!   \begin{itemize}
!     \item Only 1 DE per PET supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item [field]
!     The {\tt ESMF\_Field} object that contains data to be written.
!   \item[file]
!     The name of the output file to which Field data is written.
!   \item[{[append]}]
!     Logical: if .true., data (with attributes) is appended to an
!     existing file; default is .false.
!   \item[{[timeslice]}]
!     Some IO formats (e.g. NetCDF) support the output of data in form of
!     time slices. The {\tt timeslice} argument provides access to this
!     capability. Usage of this feature requires that the first slice is
!     written with a positive {\tt timeslice} value, and that subsequent slices
!     are written with a {\tt timeslice} argument that increments by one each
!     time. By default, i.e. by omitting the {\tt timeslice} argument, no
!     provisions for time slicing are made in the output file.
!   \item[{[iofmt]}]
!     \begin{sloppypar}
!     The IO format. Please see Section~\ref{opt:iofmtflag} for the list 
!     of options. If not present, defaults to {\tt ESMF\_IOFMT\_NETCDF}.
!     \end{sloppypar}
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
        character(len=ESMF_MAXSTR)      :: name
        type(ESMF_FieldType), pointer   :: fp 
        type(ESMF_Array)                :: array 
        integer                         :: time, i, localrc
        integer                         :: gridrank, arrayrank
        logical                         :: appd_internal
        type(ESMF_Status)               :: fieldstatus
        type(ESMF_IOFmtFlag)            :: iofmtd

#ifdef ESMF_PIO
!       Initialize
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

        appd_internal = .false.
        if(present(append)) appd_internal = append

        time = -1   ! default, no time dimension
        if (present(timeslice)) time = timeslice

        iofmtd = ESMF_IOFMT_NETCDF   ! default format
        if(present(iofmt)) iofmtd = iofmt

        fp => field%ftypep

        call c_ESMC_GetName(fp%base, name, localrc)
        if (ESMF_LogFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(field, array=array, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayWrite(array, file, variableName=trim(name), &
          append=appd_internal, timeslice=time, iofmt=iofmtd, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rcToReturn=rc)) return

        if (present(rc)) rc = ESMF_SUCCESS

#else
        ! Return indicating PIO not present
        if (present(rc)) rc = ESMF_RC_LIB_NOT_PRESENT
#endif

        end subroutine ESMF_FieldWrite

!------------------------------------------------------------------------------

end module ESMF_FieldWrMod
