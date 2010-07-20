! $Id: ESMF_XGrid.F90,v 1.2 2010/07/20 21:10:20 feiliu Exp $
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
#define ESMF_FILENAME "ESMF_XGrid.F90"
!==============================================================================
!
!     ESMF XGrid module
module ESMF_XGridMod
!
!==============================================================================
!
! This file contains the XGrid class definition and all XGrid
! class method.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_XGridMod - 2D representation of the boundary layer
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_XGrid} class, which 
! represents a 2D boundary layer usually between the
! atmosphere on one side and ocean and land on the other in an Earth
! system model. There are dynamical and thermodynamical processes on
! either side of the boundary layer and on the boundary layer itself.
! The boundary layer exchanges fluxes from either side and adjusts
! boundary condition for model components involved. For climate modeling,
! it's critical that the fluxes transferred by the boundary layer are
! conservative.   
!
! This type is implemented in Fortran 90.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_UtilMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_DistGridMod
  use ESMF_GridMod
  use ESMF_InitMacrosMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_XGridType
! ! Definition of the XGrid class.

  type ESMF_XGridSide
    integer                     :: side
  end type ESMF_XGridSide

  type(ESMF_XGridSide), parameter :: &
    ESMF_XGRID_SIDEA=ESMF_XGridSide(0), &
    ESMF_XGRID_SIDEB=ESMF_XGridSide(1), &
    ESMF_XGRID_BALANCED=ESMF_XGridSide(2)

  type ESMF_XGridSpec
    sequence
    integer, pointer            :: factorIndexList(:,:)       ! factorIndexList
    real(ESMF_KIND_R8), pointer :: factorList(:)   ! factorList
  end type ESMF_XGridSpec

  type ESMF_XGridType
    sequence
    type (ESMF_Base)                       :: base                    ! base class object
    type (ESMF_DistGrid), pointer          :: distgridA(:)            ! A side distgrid
    type (ESMF_DistGrid), pointer          :: distgridB(:)            ! B side distgrid
    type (ESMF_DistGrid)                   :: distgridM               ! load balanced distgrid in the middle
    type (ESMF_Grid), pointer              :: sideA(:), sideB(:)      ! ??? geometric types ???
    real(ESMF_KIND_R8), pointer            :: area(:), centroid(:,:)  ! area and centroids of xgrid
    type(ESMF_XGridSpec), pointer          :: sparseMatA2X(:), sparseMatX2A(:) ! descriptors of mapping sparsemat
    type(ESMF_XGridSpec), pointer          :: sparseMatB2X(:), sparseMatX2B(:)
    logical                                :: is_proxy         ! .true. for a proxy xgrid
    type (ESMF_Status)                     :: status
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! ! ESMF_XGrid    
! ! The XGrid data structure that is passed between implementation and
! ! calling languages.

  type ESMF_XGrid
    sequence
    type (ESMF_XGridType), pointer :: xgtypep
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_XGrid
  public ESMF_XGridType
  public ESMF_XGridSpec
  public ESMF_XGridSide, ESMF_XGRID_SIDEA, ESMF_XGRID_SIDEB, &
    ESMF_XGRID_BALANCED

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
   public ESMF_XGridValidate           ! Check internal consistency

   public assignment(=)

! - ESMF-internal methods:
   public ESMF_XGridGetInit            ! For Standardized Initialization
   public ESMF_XGridSerialize
   public ESMF_XGridDeserialize
   public ESMF_XGridInitialize         ! Default initiailze XGrid member variables

!
!
!EOPI
   
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_XGrid.F90,v 1.2 2010/07/20 21:10:20 feiliu Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: assignment (=) - set one xgrid equal to another
!
! !INTERFACE:
      interface assignment (=)
   
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_XGridAssign
      module procedure ESMF_XGridSpecAssign


! !DESCRIPTION:
!    Set one xgrid equal to another note that since its 
!    a pointer copy the xgrids are actually the same
 
!EOPI
      end interface
!
!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridAssign()"
!BOPI
! !IROUTINE:  ESMF_XGridAssign - set one xgrid struct equal to another

! !INTERFACE:

   subroutine ESMF_XGridAssign(dval, sval)
!
! !ARGUMENTS:
 type(ESMF_XGrid), intent(out) :: dval
 type(ESMF_XGrid), intent(in) :: sval
!
! !DESCRIPTION:
!      Set one xgrid structure equal to another
!
!     The arguments are:
!     \begin{description}
!     \item [dval]
!           destination structure
!     \item [dval]
!           source structure
!     \end{description}
!
!EOPI

 dval%xgtypep => sval%xgtypep

 ESMF_INIT_COPY(dval,sval)

 end subroutine

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridSpecAssign()"
!BOPI
! !IROUTINE:  ESMF_XGridSpecAssign - set one xgrid spec struct equal to another

! !INTERFACE:

   subroutine ESMF_XGridSpecAssign(dval, sval)
!
! !ARGUMENTS:
 type(ESMF_XGridSpec), intent(out) :: dval
 type(ESMF_XGridSpec), intent(in) :: sval
!
! !DESCRIPTION:
!      Set one xgrid spec structure equal to another
!
!     The arguments are:
!     \begin{description}
!     \item [dval]
!           destination structure
!     \item [dval]
!           source structure
!     \end{description}
!
!EOPI

 dval%factorIndexList => sval%factorIndexList
 dval%factorList => sval%factorList

 end subroutine

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridValidate"

!BOP
! !IROUTINE:  ESMF_XGridValidate - Check validity of a XGrid

! !INTERFACE:
      subroutine ESMF_XGridValidate(xgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_XGrid), intent(inout) :: xgrid 
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Validates that the {\tt xgrid} is internally consistent.
!      Currently this method determines if the {\tt xgrid} is uninitialized 
!      or already destroyed. 
!
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!           {\tt ESMF\_XGrid} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt xgrid} 
!           is valid.
!     \end{description}
!
!EOP

      integer :: localrc

      type(ESMF_XGridType), pointer :: xgtypep
      type(ESMF_Status) :: xgridstatus

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

      if (.not.associated(xgrid%xgtypep)) then 
         call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
            "Uninitialized or already destroyed XGrid: xgtypep unassociated", &
             ESMF_CONTEXT, rc)
         return
      endif 

      xgtypep => xgrid%xgtypep

      ! make sure the xgrid is ready before trying to look at contents
      call ESMF_BaseGetStatus(xgtypep%base, xgridstatus, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      if (xgridstatus .ne. ESMF_STATUS_READY) then
         call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
            "Uninitialized or already destroyed XGrid: xgridstatus not ready", &
             ESMF_CONTEXT, rc)
         return
      endif 

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_XGridValidate

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes all XGrid internal methods.
!
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridSerialize"

!BOPI
! !IROUTINE: ESMF_XGridSerialize - Serialize xgrid info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_XGridSerialize(xgrid, buffer, length, offset, &
                                    attreconflag, inquireflag, rc) 
!
! !ARGUMENTS:
      type(ESMF_XGrid), intent(inout) :: xgrid 
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), intent(in), optional :: attreconflag
      type(ESMF_InquireFlag), intent(in), optional :: inquireflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_XGrid} object and adds all the information needed
!      to save the information to a stream or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!           {\tt ESMF\_XGrid} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [length]
!           Current length of buffer, in bytes.  If the serialization
!           process needs more space it will allocate it and update
!           this length.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item[{[inquireflag]}]
!           Flag to tell if serialization is to be done (ESMF_NOINQUIRE)
!           or if this is simply a size inquiry (ESMF_INQUIREONLY)
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc
      type(ESMF_XGridType), pointer :: fp    ! xgrid type
      type(ESMF_AttReconcileFlag) :: lattreconflag
      type(ESMF_InquireFlag) :: linquireflag

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

      ! deal with optional attreconflag and inquireflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif

      if (present (inquireflag)) then
        linquireflag = inquireflag
      else
        linquireflag = ESMF_NOINQUIRE
      end if

      ! shortcut to internals
      fp => xgrid%xgtypep

      call c_ESMC_BaseSerialize(fp%base, buffer(1), length, offset, &
                                 lattreconflag, linquireflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_XGridSerialize(fp%status, &
                                 buffer(1), length, offset, linquireflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return


      !if (fp%gridstatus .eq. ESMF_STATUS_READY) then
      !  call ESMF_GeomBaseSerialize(fp%geombase, buffer, length, offset, &
      !                              lattreconflag, linquireflag, localrc)
      !  if (ESMF_LogMsgFoundError(localrc, &
      !                               ESMF_ERR_PASSTHRU, &
      !                               ESMF_CONTEXT, rc)) return
      !endif

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_XGridSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridDeserialize"

!BOPI
! !IROUTINE: ESMF_XGridDeserialize - Deserialize a byte stream into a XGrid
!
! !INTERFACE:
      function ESMF_XGridDeserialize(buffer, offset, &
                                    attreconflag, rc) 
!
! !RETURN VALUE:
      type(ESMF_XGrid) :: ESMF_XGridDeserialize   
!
! !ARGUMENTS:
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      type(ESMF_AttReconcileFlag), optional :: attreconflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a XGrid object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_XGridWrite()} and {\tt ESMF\_XGridRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item[{[attreconflag]}]
!           Flag to tell if Attribute serialization is to be done
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc
      type(ESMF_XGridType), pointer :: fp    ! xgrid type
      integer staggerloc
      type(ESMF_AttReconcileFlag) :: lattreconflag

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if  (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! deal with optional attreconflag
      if (present(attreconflag)) then
        lattreconflag = attreconflag
      else
        lattreconflag = ESMF_ATTRECONCILE_OFF
      endif

      ! In case of error, make sure this is invalid.
      nullify(ESMF_XGridDeserialize%xgtypep)

      ! Shortcut to internals
      allocate(fp, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, &
                                     "space for new XGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Deserialize Base
      call c_ESMC_BaseDeserialize(fp%base, buffer(1), offset, lattreconflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return
                                 
      call ESMF_BaseSetInitCreated(fp%base, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! Deserialize other XGrid members

      call c_ESMC_XGridDeserialize(fp%status, &
                                   buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      !if (fp%datastatus .eq. ESMF_STATUS_READY) then
      !    call c_ESMC_ArrayDeserialize(fp%array, buffer(1), offset, &
      !                                lattreconflag, localrc)
      !    if (ESMF_LogMsgFoundError(localrc, &
      !                               ESMF_ERR_PASSTHRU, &
      !                               ESMF_CONTEXT, rc)) return

      !    call ESMF_ArraySetInitCreated(fp%array,rc=localrc)
      !    if (ESMF_LogMsgFoundError(localrc, &
      !                               ESMF_ERR_PASSTHRU, &
      !                               ESMF_CONTEXT, rc)) return
      !endif
    
      fp%is_proxy = .true.
      ESMF_XGridDeserialize%xgtypep => fp
      
      ! Add copy of this object into ESMF garbage collection table
      !call c_ESMC_VMAddFObject(ESMF_XGridDeserialize, ESMF_ID_XGRID%objectID)
      
      ESMF_INIT_SET_CREATED(ESMF_XGridDeserialize)

      if  (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_XGridDeserialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetInit"
!BOPI
! !IROUTINE:  ESMF_XGridGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_XGridGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_XGrid), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_XGridGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt xgrid}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_XGrid} from which to retrieve status.
!     \end{description}
!
!EOPI


       if (present(d)) then
         ESMF_XGridGetInit = ESMF_INIT_GET(d)
       else
         ESMF_XGridGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_XGridGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridInitialize"

!BOPI
! !IROUTINE: ESMF_XGridInitialize - Default initialize xgrid member variables
!
! !INTERFACE:
      subroutine ESMF_XGridInitialize(xgtypep, rc) 
!
! !ARGUMENTS:
      type(ESMF_XGridType), pointer :: xgtypep 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_XGrid} object and default initialize its
!      auxiliary data members. Only to be used by other XGrid Create methods.
!
!     The arguments are:
!     \begin{description}
!     \item [xgtypep]
!           {\tt ESMF\_XGridType} object to be default initialized.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
        xgtypep%status  = ESMF_STATUS_UNINIT
        xgtypep%is_proxy       = .false. 

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_XGridInitialize

!------------------------------------------------------------------------------

end module ESMF_XGridMod
