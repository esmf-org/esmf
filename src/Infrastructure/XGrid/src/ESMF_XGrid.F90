! $Id: ESMF_XGrid.F90,v 1.11 2010/09/13 19:29:22 feiliu Exp $
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
! This file contains the XGrid class definition and base XGrid
! class methods.
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
! The XGrid type is implemented in Fortran 90.
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

  ! defines the side relative to an XGrid, SIDEA, SIDEB, or BALANCED
  type ESMF_XGridSide
    sequence
    integer                     :: side
  end type ESMF_XGridSide

  type(ESMF_XGridSide), parameter :: &
    ESMF_XGRID_SIDEA=ESMF_XGridSide(0), &
    ESMF_XGRID_SIDEB=ESMF_XGridSide(1), &
    ESMF_XGRID_BALANCED=ESMF_XGridSide(2)

  ! package the collapsed indicies and weights matrices
  type ESMF_XGridSpec
    sequence
    integer, pointer            :: factorIndexList(:,:) => null()     ! factorIndexList
    real(ESMF_KIND_R8), pointer :: factorList(:) => null()  ! factorList
  end type ESMF_XGridSpec

  ! the XGridType definition
  type ESMF_XGridType
    sequence
    type (ESMF_Base)                       :: base                    ! base class object
    type (ESMF_DistGrid)                   :: distgridM               ! load balanced distgrid in the middle
    type (ESMF_DistGrid), pointer          :: distgridA(:)            ! A side distgrid
    type (ESMF_DistGrid), pointer          :: distgridB(:)            ! B side distgrid
    type (ESMF_Grid), pointer              :: sideA(:), sideB(:)      ! geometric types
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
   public ESMF_XGridMatch              ! Check if two XGrids match

   public assignment(=)
   public operator(.eq.), operator(.ne.) 

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
    '$Id: ESMF_XGrid.F90,v 1.11 2010/09/13 19:29:22 feiliu Exp $'

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
      module procedure ESMF_XGridSideAssign


! !DESCRIPTION:
!    Set one xgrid equal to another note that since its 
!    a pointer copy the xgrids are actually the same
 
!EOPI
      end interface
!
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (.eq.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_XGridSideEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF GridConn.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (.ne.)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_XGridSideNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF GridConn.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface


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

 ! deep copy of the lists
 allocate(dval%factorIndexList(size(sval%factorIndexList,1), &
    size(sval%factorIndexList,2)))
 allocate(dval%factorList(size(sval%factorList,1)))

 dval%factorIndexList = sval%factorIndexList
 dval%factorList = sval%factorList

 end subroutine

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridSideAssign()"
!BOPI
! !IROUTINE:  ESMF_XGridSideAssign - set one xgrid side struct equal to another

! !INTERFACE:

   subroutine ESMF_XGridSideAssign(dval, sval)
!
! !ARGUMENTS:
 type(ESMF_XGridSide), intent(out) :: dval
 type(ESMF_XGridSide), intent(in) :: sval
!
! !DESCRIPTION:
!      Set one xgrid side structure equal to another
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

 ! deep copy of the lists

 dval%side = sval%side

 end subroutine

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridMatch"

!BOPI
! !IROUTINE:  ESMF_XGridValidate - Check if two XGrids match

! !INTERFACE:
      function ESMF_XGridMatch(xgrid1, xgrid2, rc)
!
! !RETURN VALUE:
      logical :: ESMF_XGridMatch
!
! !ARGUMENTS:
      type(ESMF_XGrid), intent(inout) :: xgrid1, xgrid2 
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Compare two {\tt XGrid}s and check if they match each other. The 
!      comparison is incremental. First the internal pointer association
!      is checked to see if they are the same object. A deep check of 
!      individual XGrid members is not implemented yet for performance 
!      consideration.
!
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid1]
!           First {\tt ESMF\_XGrid} to match.
!     \item [xgrid2]
!           Second {\tt ESMF\_XGrid} to match.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt xgrid} 
!           is valid.
!     \end{description}
!
!EOPI

      integer :: localrc

      type(ESMF_XGridType), pointer :: xgtypep
      type(ESMF_Status) :: xgridstatus

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid1,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid2,rc)

      if(associated(xgrid1%xgtypep, xgrid2%xgtypep)) then
        ESMF_XGridMatch = .true.
      else
        ESMF_XGridMatch = .false.
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_XGridMatch

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridValidate"

!BOPI
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
!EOPI

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

      integer :: localrc, ngridA, ngridB, i
      type(ESMF_XGridType), pointer :: fp    ! xgrid type
      type(ESMF_AttReconcileFlag) :: lattreconflag
      type(ESMF_InquireFlag) :: linquireflag
      integer :: s(10)               ! association status variables
      integer :: cellCount, dimCount ! for area and centroid
      integer, dimension(:), allocatable :: eleCountA2X, eleCountX2A, &
                                            eleCountB2X, eleCountX2B

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

      ! serialize the balanced distgrid
      call C_ESMC_DistGridSerialize(fp%distgridM, buffer(1), length, offset, &
                                   linquireflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                               ESMF_ERR_PASSTHRU, &
                               ESMF_CONTEXT, rc)) return

      ! set up the association status array and other meta-data
      s = 0
      cellCount = 0
      dimCount = 0
      ngridA = 0
      ngridB = 0
      if(associated(fp%distgridA)) s(1) = 1
      if(associated(fp%distgridB)) s(2) = 1
      if(associated(fp%sideA)) then
        s(3) = 1
        ngridA = size(fp%sideA,1)
      endif
      if(associated(fp%sideB)) then
        s(4) = 1
        ngridB = size(fp%sideB,1)
      endif
      if(associated(fp%area)) then
        s(5) = 1
        cellCount = size(fp%area, 1)
      endif
      if(associated(fp%centroid)) then
        s(6) = 1
        dimCount = size(fp%centroid, 2) ! manifold dimension count
      endif
      if(associated(fp%sparseMatA2X)) then
        s(7) = 1
        ngridA = size(fp%sparseMatA2X,1)
        allocate(eleCountA2X(ngridA))
        do i = 1, ngridA
            eleCountA2X(i) = size(fp%sparseMatA2X(i)%factorList, 1)
        enddo
      endif
      if(associated(fp%sparseMatX2A)) then
        s(8) = 1
        ngridA = size(fp%sparseMatX2A,1)
        allocate(eleCountX2A(ngridA))
        do i = 1, ngridA
            eleCountX2A(i) = size(fp%sparseMatX2A(i)%factorList, 1)
        enddo
      endif
      if(associated(fp%sparseMatB2X)) then
        s(9) = 1
        ngridB = size(fp%sparseMatB2X,1)
        allocate(eleCountB2X(ngridB))
        do i = 1, ngridB
            eleCountB2X(i) = size(fp%sparseMatB2X(i)%factorList, 1)
        enddo
      endif
      if(associated(fp%sparseMatX2B)) then
        s(10) = 1
        ngridB = size(fp%sparseMatX2B,1)
        allocate(eleCountX2B(ngridB))
        do i = 1, ngridB
            eleCountX2B(i) = size(fp%sparseMatX2B(i)%factorList, 1)
        enddo
      endif


      ! call into the C api first to serialize this status array and other meta-data
      ! this ensures consistency when deserialization
      call c_ESMC_XGridSerialize(s, cellCount, dimCount, ngridA, ngridB, &
                                 eleCountA2X, eleCountX2A, eleCountB2X, eleCountX2B, &
                                 fp%area, fp%centroid, &
                                 buffer(1), length, offset, linquireflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! serialize the rest of the XGrid members
      if(associated(fp%distgridA)) then
          ngridA = size(fp%distgridA,1)
          do i = 1, ngridA
            call C_ESMC_DistGridSerialize(fp%distgridA(i), buffer(1), length, offset, &
                                         linquireflag, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
          enddo
      endif

      if(associated(fp%distgridB)) then
          ngridB = size(fp%distgridB,1)
          do i = 1, ngridB
            call C_ESMC_DistGridSerialize(fp%distgridB(i), buffer(1), length, offset, &
                                         linquireflag, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
          enddo
      endif

      ! serialize the Grids
      if(associated(fp%sideA)) then
          ngridA = size(fp%sideA,1)
          do i = 1, ngridA
            call ESMF_GridSerialize(grid=fp%sideA(i), buffer=buffer, &
                         length=length, offset=offset, &
                         attreconflag=lattreconflag, inquireflag=linquireflag, &
                         rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
          enddo
      endif

      if(associated(fp%sideB)) then
          ngridB = size(fp%sideB,1)
          do i = 1, ngridB
            call ESMF_GridSerialize(grid=fp%sideB(i), buffer=buffer, &
                         length=length, offset=offset, &
                         attreconflag=lattreconflag, inquireflag=linquireflag, &
                         rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
          enddo
      endif

      ! serialize the SparseMatSpec objects
      if(associated(fp%sparseMatA2X)) then
          do i = 1, size(fp%sparseMatA2X, 1)
              call c_ESMC_SMMSpecSerialize(size(fp%sparseMatA2X(i)%factorList, 1), &
                 fp%sparseMatA2X(i)%factorIndexList, &
                 fp%sparseMatA2X(i)%factorList, &
                 buffer(1), length, offset, linquireflag, localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                 ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rc)) return
          enddo
          deallocate(eleCountA2X)
      endif
      if(associated(fp%sparseMatX2A)) then
          do i = 1, size(fp%sparseMatX2A, 1)
              call c_ESMC_SMMSpecSerialize(size(fp%sparseMatX2A(i)%factorList, 1), &
                 fp%sparseMatX2A(i)%factorIndexList, &
                 fp%sparseMatX2A(i)%factorList, &
                 buffer(1), length, offset, linquireflag, localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                 ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rc)) return
          enddo
          deallocate(eleCountX2A)
      endif
      if(associated(fp%sparseMatB2X)) then
          do i = 1, size(fp%sparseMatB2X, 1)
              call c_ESMC_SMMSpecSerialize(size(fp%sparseMatB2X(i)%factorList, 1), &
                 fp%sparseMatB2X(i)%factorIndexList, &
                 fp%sparseMatB2X(i)%factorList, &
                 buffer(1), length, offset, linquireflag, localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                 ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rc)) return
          enddo
          deallocate(eleCountB2X)
      endif
      if(associated(fp%sparseMatX2B)) then
          do i = 1, size(fp%sparseMatX2B, 1)
              call c_ESMC_SMMSpecSerialize(size(fp%sparseMatX2B(i)%factorList, 1), &
                 fp%sparseMatX2B(i)%factorIndexList, &
                 fp%sparseMatX2B(i)%factorList, &
                 buffer(1), length, offset, linquireflag, localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                 ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rc)) return
          enddo
          deallocate(eleCountX2B)
      endif

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
        nullify(xgtypep%sideA)
        nullify(xgtypep%sideB)
        nullify(xgtypep%area)
        nullify(xgtypep%centroid)
        nullify(xgtypep%sparseMatA2X)
        nullify(xgtypep%sparseMatX2A)
        nullify(xgtypep%sparseMatB2X)
        nullify(xgtypep%sparseMatX2B)

        if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_XGridInitialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridSideEqual"
!BOPI
! !IROUTINE: ESMF_XGridSideEqual - Equality of XGridSides
!
! !INTERFACE:
      function ESMF_XGridSideEqual(XGridSide1, XGridSide2)

! !RETURN VALUE:
      logical :: ESMF_XGridSideEqual

! !ARGUMENTS:

      type (ESMF_XGridSide), intent(in) :: &
        XGridSide1,      &! Two xgrid sides to compare for
        XGridSide2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF XGridSide to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[XGridSide1, XGridSide2]
!          Two XGridSide to compare for equality
!     \end{description}
!
!EOPI

      ESMF_XGridSideEqual = (XGridSide1%side == &
                              XGridSide2%side)

      end function ESMF_XGridSideEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridSideNotEqual"
!BOPI
! !IROUTINE: ESMF_XGridSideNotEqual - Non-equality of XGridSides
!
! !INTERFACE:
      function ESMF_XGridSideNotEqual(XGridSide1, XGridSide2)

! !RETURN VALUE:
      logical :: ESMF_XGridSideNotEqual

! !ARGUMENTS:

      type (ESMF_XGridSide), intent(in) :: &
         XGridSide1,      &! Two XGridSide Statuses to compare for
         XGridSide2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF XGridSide to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[XGridSide1, XGridSide2]
!          Two XGridSides to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_XGridSideNotEqual = (XGridSide1%side /= &
                                 XGridSide2%side)

      end function ESMF_XGridSideNotEqual


end module ESMF_XGridMod
