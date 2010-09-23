! $Id: ESMF_FieldWr.F90,v 1.1 2010/09/23 22:59:04 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
  use ESMF_IOSpecMod
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
! !IROUTINE:  ESMF_FieldWrite - Write the Field data into a file

! !INTERFACE:
      subroutine ESMF_FieldWrite(field, file, append, iofmt, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field 
      character(*), intent(in) :: file 
      logical, intent(in), optional :: append
      type(ESMF_IOFmtFlag), intent(in), optional :: iofmt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Write the Field data in a {ESMF\_Field} object to a file.
!
!   Limitation:
!     Assume 1 DE per Pet
!     Not support in ESMF\_COMM=mpiuni mode
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!        An {\tt ESMF\_Field} object.
!     \item[file]
!        The name of the output file to which Field data is written to.
!     \item[{[append]}]
!        Logical: if true, data is appended to an existing file,
!        default is false.
!     \item[{[iofmt]}]
!        The IO format. Please see Section~\ref{opt:iofmtflag} for the list 
!        of options. If not present, defaults to ESMF\_IOFMT\_NETCDF.
!     \item [{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
        character(len=ESMF_MAXSTR)      :: name
        type(ESMF_FieldType), pointer   :: fp 
        type(ESMF_Array)                :: array 
        integer                         :: i, localrc
        integer                         :: gridrank, arrayrank
        logical                         :: appended
        type(ESMF_Status)               :: fieldstatus
        type(ESMF_IOFmtFlag)            :: iofmtd

#ifdef ESMF_PIO
!       Initialize
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

        appended = .false.
        if(present(append)) appended = append

        iofmtd = ESMF_IOFMT_NETCDF   ! default format
        if(present(iofmt)) iofmtd = iofmt

        fp => field%ftypep

        call c_ESMC_GetName(fp%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(field, array=array, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_ArrayWrite(array, file, variableName=trim(name), &
          append=appended, iofmt=iofmtd, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (present(rc)) rc = ESMF_SUCCESS

#else
        ! Return indicating PIO not present
        if (present(rc)) rc = ESMF_RC_LIB_NOT_PRESENT
#endif

        end subroutine ESMF_FieldWrite

!------------------------------------------------------------------------------

end module ESMF_FieldWrMod
