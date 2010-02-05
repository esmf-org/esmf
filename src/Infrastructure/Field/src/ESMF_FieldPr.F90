! $Id: ESMF_FieldPr.F90,v 1.11.2.1 2010/02/05 19:56:03 svasquez Exp $
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
  use ESMF_IOSpecMod
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

  use ESMF_FieldMod

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
! !IROUTINE:  ESMF_FieldPrint - Print the contents of a Field

! !INTERFACE:
      subroutine ESMF_FieldPrint(field, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field 
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Prints information about the {\tt field} to {\tt stdout}.
!     This subroutine goes through the internal data members of a field
!     data type and prints information of each data member. \\
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, the {\tt ESMF\_IOUnitFlush()} method
!     may be used on unit 6 to get coherent output.  \\
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

!	Initialize
        localrc = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = ESMF_RC_NOT_IMPL

        ! check variables
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

        ! print option is not implemented, but it has to pass to c_ESMC_BasePrint()
        defaultopts = "brief"

        fp => field%ftypep

        call ESMF_BaseGetStatus(fp%base, fieldstatus, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rc)) return

        !nsc call ESMF_LogWrite("Field Print:", ESMF_LOG_INFO)
        write(*,*) "Field Print Starts ====>"

        call ESMF_StatusString(fieldstatus, str, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        write(*, *)  "Field status = ", trim(str)

        if (fieldstatus .ne. ESMF_STATUS_READY) then
          write(*,*) "Empty or Uninitialized Field"
          write(*,*) "Field Print Ends   ====>"
          if (present(rc)) rc = ESMF_SUCCESS
          return
        endif
        
        if (.not. associated(field%ftypep)) then
        !jw  call ESMF_LogWrite("Empty or Uninitialized Field", ESMF_LOG_INFO)
          write(*,*) "Empty or Uninitialized Field"
          write(*,*) "Field Print Ends   ====>"
          if (present(rc)) rc = ESMF_SUCCESS
          return
        endif

        call c_ESMC_GetName(fp%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      !jw  write(msgbuf, *)  "  Name = '",  trim(name), "'"
      !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
        write(*, *)  "  Name = '",  trim(name), "'"

        call ESMF_BasePrint(fp%base, defaultopts, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


        call ESMF_StatusString(fp%gridstatus, str, localrc)
        write(*, *)  "Grid status = ", trim(str)
        if (fp%gridstatus .eq. ESMF_STATUS_READY) then 
!           call ESMF_GeomBasePrint(fp%geombase, "", localrc)
!          if (ESMF_LogMsgFoundError(localrc, &
!            ESMF_ERR_PASSTHRU, &
!            ESMF_CONTEXT, rc)) return
          call ESMF_GeomBaseGet(fp%geombase, dimCount=gridrank, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
          write(*, *) "gridrank = ", gridrank
        endif

        call ESMF_StatusString(fp%datastatus, str, localrc)
        write(*, *)  "Data status = ", trim(str)
        if (fp%datastatus .eq. ESMF_STATUS_READY) then 
          call ESMF_ArrayPrint(fp%array, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
          call ESMF_ArrayGet(fp%array, rank=arrayrank, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
          write(*, *) "arrayrank = ", arrayrank
        endif

        write(*, *) "gridToFieldMap ungriddedLBound ungriddedUBound maxHaloLWidth", &
            " maxHaloUWidth"
        do i = 1, ESMF_MAXDIM
            write(*, *) fp%gridToFieldMap(i), fp%ungriddedLBound(i), fp%ungriddedUBound(i), &
                "    ", fp%maxHaloLWidth(i), fp%maxHaloUWidth(i)
        enddo
        write(*,*) "Field Print Ends   ====>"

        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_FieldPrint


!------------------------------------------------------------------------------

end module ESMF_FieldPrMod
