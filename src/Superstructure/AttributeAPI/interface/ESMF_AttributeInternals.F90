! $Id: ESMF_AttributeInternals.F90,v 1.5 2012/08/24 00:32:13 rokuingh Exp $
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
#define ESMF_FILENAME "ESMF_AttributeInternals.F90"
!==============================================================================
!
! ESMF Attribute Internals Module
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

module ESMF_AttributeInternalsMod
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_AttributeInternalsMod - Attribute API helper functions
!
! !DESCRIPTION:
!
! The helper functions for the Attribute API.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_BaseMod
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_GridMod
  use ESMF_StaggerLocMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private


!------------------------------------------------------------------------------
!
! !DESCRIPTION:
!
! !PUBLIC MEMBER FUNCTIONS:
!
      
  public ESMF_AttributeGetInfo
  public AttributeInternalInfo
  public extractInfoInt, extractInfoValueString
    
  interface ESMF_AttributeGetInfo
    module procedure ESMF_GridAttGetInfoI4
    module procedure ESMF_GridAttGetInfoChar
    module procedure ESMF_GridAttGetInfoI4List
    module procedure ESMF_GridAttGetInfoR8List
    module procedure ESMF_GridAttGetInfoLogicalList
  end interface

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!EOPI 
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = &
               '$Id: ESMF_AttributeInternals.F90,v 1.5 2012/08/24 00:32:13 rokuingh Exp $'
!------------------------------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttGetInfo"
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeGetInfo - Get internal Attribute info
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGetInfo()
      subroutine ESMF_GridAttGetInfoI4(grid, name, value, inputList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      character(len=*), intent(in), optional :: inputList(:)
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns internal info from the object.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An ESMF object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The value of the internal info.
!     \item [inputList]
!           A list containing input information needed to retrieve info.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: localDel, i

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localDel = 0

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      ! retrieve input values
      if (present(inputList)) then
        do i=1,size(inputList)
          if (index(inputList(i), "localDe") /= 0) then
            localDel = extractInfoInt(inputList(i))
          endif
        enddo
      endif

      ! retrieve list type info that does not require additional inputs
      select case (name)
        case ("dimCount")
          call ESMF_GridGet(grid, dimCount=value, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case ("tileCount")
          call ESMF_GridGet(grid, tileCount=value, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case ("staggerlocCount")
          call ESMF_GridGet(grid, staggerlocCount=value, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case ("localDECount")
          call ESMF_GridGet(grid, localDECount=value, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case ("arbDim")
          call ESMF_GridGet(grid, arbDim=value, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case ("rank")
          call ESMF_GridGet(grid, rank=value, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case ("arbDimCount")
          call ESMF_GridGet(grid, arbDimCount=value, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case("arbIndexCount") ! requires localDe
          call ESMF_GridGet(grid, localDe=localDel, arbIndexCount=value, &
                            rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case DEFAULT
          call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
            msg="The provided 'name' does not correspond to internal info", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
      end select

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInfoI4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttGetInfo"
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeGetInfo - Get internal Attribute info
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGetInfo()
      subroutine ESMF_GridAttGetInfoChar(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns internal info from the object.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An ESMF object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The value of the internal info.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      type(ESMF_TypeKind_Flag)   :: lcoordTypeKind
      type(ESMF_Index_Flag)      :: lindexflag
      type(ESMF_GridStatus_Flag) :: lstatus

      integer :: localrc

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      select case (name)
        case ("coordTypeKind")
          call ESMF_GridGet(grid, coordTypeKind=lcoordTypeKind, rc=localrc)
print *, "HERE!!!!!!!!!!!"
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          ! assign the string value of this named constant
          value = lcoordTypeKind
print *, "value = ", value
print *, "coordTypeKind = ", lcoordTypeKind
        case ("indexflag")
          call ESMF_GridGet(grid, indexflag=lindexflag, rc=localrc)
          ! assign the string value of this named constant
          value = lindexflag
        case ("status")
          call ESMF_GridGet(grid, status=lstatus, rc=localrc)
          ! assign the string value of this named constant
          value = lstatus
        case ("name")
          call ESMF_GridGet(grid, name=value, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case DEFAULT
          call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
            msg="The provided 'name' does not correspond to internal info", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
      end select

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInfoChar
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttGetInfo"
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeGetInfo - Get internal Attribute info
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGetInfo()
      subroutine ESMF_GridAttGetInfoI4List(grid, name, valueList, &
                                            inputList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid
      character(len=*), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: valueList(:)
      character(len=*), intent(in), optional :: inputList(:)
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns internal info from the object.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An ESMF object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [valueList]
!           The valueList of the internal info.
!     \item [inputList]
!           A list containing input information needed to retrieve info.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc, i
      
      integer :: localDel
      type(ESMF_StaggerLoc) :: staggerlocl
      integer :: tilel
      integer :: coordDiml
      type(ESMF_GridItem_Flag) :: itemflagl

      logical :: getLDe, getStagger, getTile, getCoord, getItem

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! setting default values
      localDel = 0
      staggerlocl = ESMF_STAGGERLOC_CENTER
      tilel = 1
      ! coorddim and itemflag cannot default
      getCoord = .false.
      getItem = .false.

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      ! looking for required input parameters
      if (present(inputList)) then
        do i=1,size(inputList)
          !!!! TODO: this can go away once modName is dynamically sized !!!
          if (len(inputList(i)) > ESMF_MAXSTR) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
              msg="len(inputList(i)) cannot be larger than ESMF_MAXSTR for now", &
              ESMF_CONTEXT, rcToReturn=rc)
            return
          endif
          ! set the parameters based on the inputList
          if (index(inputList(i), "localDe") /= 0) then
            localDel = extractInfoInt(inputList(i))
          elseif (index(inputList(i), "staggerloc") /= 0) then
            staggerlocl = extractInfoValueString(inputList(i))
          elseif (index(inputList(i), "tile") /= 0) then
            tilel = extractInfoInt(inputList(i))
          elseif (index(inputList(i), "coordDim") /= 0) then
            coordDiml = extractInfoInt(inputList(i))
            getCoord = .true.
          elseif (index(inputList(i), "itemflag") /= 0) then
            itemflagl = extractInfoValueString(inputList(i))
            getItem = .true.
          endif
        enddo
      endif

      ! if both getCoord and getItem are both true fault
      

        ! retrieve list style input that requires additional inputs
          !
          ! all of these that requires localDe and staggerloc can default to:
          ! localDe=0
          ! staggerloc=ESMF_STAGGERLOC_CENTER
          !
          !--case("minIndex") ! requires tile and staggerloc
          !--case("maxIndex") ! requires tile and staggerloc

          !--case("exclusiveLBound") ! requires localDe and staggerloc
          !--case("exclusiveUBound") ! requires localDe and staggerloc
          !--case("exclusiveCount") ! requires localDe and staggerloc
          !--case("computationalLBound") ! requires localDe and staggerloc
          !--case("computationalUBound") ! requires localDe and staggerloc
          !--case("computationalCount") ! requires localDe and staggerloc

          !
          !--case("exclusiveLBound") ! requires coordDim, localDe and staggerloc
          !--case("exclusiveUBound") ! requires coordDim, localDe and staggerloc
          !--case("exclusiveCount") ! requires coordDim, localDe and staggerloc
          !--case("computationalLBound") ! requires coordDim, localDe and staggerloc
          !--case("computationalUBound") ! requires coordDim, localDe and staggerloc
          !--case("computationalCount") ! requires coordDim, localDe and staggerloc
          !--case("totalLBound") ! requires coordDim, localDe and staggerloc
          !--case("totalUBound") ! requires coordDim, localDe and staggerloc
          !--case("totalCount") ! requires coordDim, localDe and staggerloc
          !
          !--case("exclusiveLBound") ! requires itemflag, localDe and staggerloc
          !--case("exclusiveUBound") ! requires itemflag, localDe and staggerloc
          !--case("exclusiveCount") ! requires itemflag, localDe and staggerloc
          !--case("computationalLBound") ! requires itemflag, localDe and staggerloc
          !--case("computationalUBound") ! requires itemflag, localDe and staggerloc
          !--case("computationalCount") ! requires itemflag, localDe and staggerloc
          !--case("totalLBound") ! requires itemflag, localDe and staggerloc
          !--case("totalUBound") ! requires itemflag, localDe and staggerloc
          !--case("totalCount") ! requires itemflag, localDe and staggerloc
          !
          ! this one is going to require a different strategy (array input)
          !
          !case("coord") ! requires index(:), localDe and staggerloc

      ! retrieve list type info that DOES NOT require additional inputs
      select case (name)
        case ("distgridToGridMap")
          call ESMF_GridGet(grid, distgridToGridMap=valueList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case ("coordDimCount")
          call ESMF_GridGet(grid, coordDimCount=valueList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case ("gridEdgeLWidth")
          call ESMF_GridGet(grid, gridEdgeLWidth=valueList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case ("gridEdgeUWidth")
          call ESMF_GridGet(grid, gridEdgeUWidth=valueList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case ("gridAlign")
  call ESMF_GridGet(grid, gridAlign=valueList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      ! retrieve list type info that DOES require additional inputs
        case("minIndex")
          call ESMF_GridGet(grid, tile=tilel, staggerloc=staggerlocl, &
                            minIndex=valueList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case("maxIndex")
          call ESMF_GridGet(grid, tile=tilel, staggerloc=staggerlocl, &
                            maxIndex=valueList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case("exclusiveLBound")
          if (getCoord) then
            call ESMF_GridGetCoordBounds(grid, coordDim=coordDiml, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              exclusiveLBound=valueList, rc=localrc)
          elseif (getItem) then
            call ESMF_GridGetItemBounds(grid, itemflag=itemflagl, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              exclusiveLBound=valueList, rc=localrc)
          else
            call ESMF_GridGet(grid, staggerloc=staggerlocl, localDe=localDel, &
                              exclusiveLBound=valueList, rc=localrc)
          endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case("exclusiveUBound")
          if (getCoord) then
            call ESMF_GridGetCoordBounds(grid, coordDim=coordDiml, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              exclusiveUBound=valueList, rc=localrc)
          elseif (getItem) then
            call ESMF_GridGetItemBounds(grid, itemflag=itemflagl, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              exclusiveUBound=valueList, rc=localrc)
          else
            call ESMF_GridGet(grid, staggerloc=staggerlocl, localDe=localDel, &
                              exclusiveUBound=valueList, rc=localrc)
          endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case("exclusiveCount")
          if (getCoord) then
            call ESMF_GridGetCoordBounds(grid, coordDim=coordDiml, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              exclusiveCount=valueList, rc=localrc)
          elseif (getItem) then
            call ESMF_GridGetItemBounds(grid, itemflag=itemflagl, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              exclusiveCount=valueList, rc=localrc)
          else
            call ESMF_GridGet(grid, staggerloc=staggerlocl, localDe=localDel, &
                              exclusiveCount=valueList, rc=localrc)
          endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case("computationalLBound")
          if (getCoord) then
            call ESMF_GridGetCoordBounds(grid, coordDim=coordDiml, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              computationalLBound=valueList, rc=localrc)
          elseif (getItem) then
            call ESMF_GridGetItemBounds(grid, itemflag=itemflagl, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              computationalLBound=valueList, rc=localrc)
          else
            call ESMF_GridGet(grid, staggerloc=staggerlocl, localDe=localDel, &
                              computationalLBound=valueList, rc=localrc)
          endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case("computationalUBound")
          if (getCoord) then
            call ESMF_GridGetCoordBounds(grid, coordDim=coordDiml, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              computationalUBound=valueList, rc=localrc)
          elseif (getItem) then
            call ESMF_GridGetItemBounds(grid, itemflag=itemflagl, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              computationalUBound=valueList, rc=localrc)
          else
            call ESMF_GridGet(grid, staggerloc=staggerlocl, localDe=localDel, &
                              computationalUBound=valueList, rc=localrc)
          endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case("computationalCount")
          if (getCoord) then
            call ESMF_GridGetCoordBounds(grid, coordDim=coordDiml, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              computationalCount=valueList, rc=localrc)
          elseif (getItem) then
            call ESMF_GridGetItemBounds(grid, itemflag=itemflagl, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              computationalCount=valueList, rc=localrc)
          else
            call ESMF_GridGet(grid, staggerloc=staggerlocl, localDe=localDel, &
                              computationalCount=valueList, rc=localrc)
          endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case("totalLBound")
          if (getCoord) then
            call ESMF_GridGetCoordBounds(grid, coordDim=coordDiml, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              totalLBound=valueList, rc=localrc)
          elseif (getItem) then
            call ESMF_GridGetItemBounds(grid, itemflag=itemflagl, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              totalLBound=valueList, rc=localrc)
          endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case("totalUBound")
          if (getCoord) then
            call ESMF_GridGetCoordBounds(grid, coordDim=coordDiml, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              totalUBound=valueList, rc=localrc)
          elseif (getItem) then
            call ESMF_GridGetItemBounds(grid, itemflag=itemflagl, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              totalUBound=valueList, rc=localrc)
          endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case("totalCount")
          if (getCoord) then
            call ESMF_GridGetCoordBounds(grid, coordDim=coordDiml, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              totalCount=valueList, rc=localrc)
          elseif (getItem) then
            call ESMF_GridGetItemBounds(grid, itemflag=itemflagl, &
                              staggerloc=staggerlocl, localDe=localDel, &
                              totalCount=valueList, rc=localrc)
          endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case DEFAULT
          call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
            msg="The provided 'name' does not correspond to internal info", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
      end select

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInfoI4List
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttGetInfo"
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeGetInfo - Get internal Attribute info
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGetInfo()
      subroutine ESMF_GridAttGetInfoR8List(grid, name, valueList, &
                                            inputList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid
      character(len=*), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: valueList(:)
      character(len=*), intent(in), optional :: inputList(:)
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns internal info from the object.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An ESMF object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [valueList]
!           The valueList of the internal info.
!     \item [inputList]
!           A list containing input information needed to retrieve info.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc, i
      
      integer :: localDel
      type(ESMF_StaggerLoc) :: staggerlocl
      integer :: coordDiml
      integer :: min_size, dimCount


      ! have to allocate pointers for all three possiblities of Grid size, R8 will be handled by templates
      real(ESMF_KIND_R8), pointer :: coords1D(:)
      real(ESMF_KIND_R8), pointer :: coords2D(:,:)
      real(ESMF_KIND_R8), pointer :: coords3D(:,:,:)
      integer :: totalCount(3)

      logical :: getLDe, getStagger, getCoord

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! setting default values
      localDel = 0
      staggerlocl = ESMF_STAGGERLOC_CENTER
      ! coorddim cannot default
      getCoord = .false.

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      ! looking for required input parameters
      if (present(inputList)) then
        do i=1,size(inputList)
          !!!! TODO: this can go away once modName is dynamically sized !!!
          if (len(inputList(i)) > ESMF_MAXSTR) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
              msg="len(inputList(i)) cannot be larger than ESMF_MAXSTR for now", &
              ESMF_CONTEXT, rcToReturn=rc)
            return
          endif
          ! set the parameters based on the inputList
          if (index(inputList(i), "localDe") /= 0) then
            localDel = extractInfoInt(inputList(i))
          elseif (index(inputList(i), "staggerloc") /= 0) then
            staggerlocl = extractInfoValueString(inputList(i))
          elseif (index(inputList(i), "coordDim") /= 0) then
            coordDiml = extractInfoInt(inputList(i))
            getCoord = .true.
          endif
        enddo
      endif

      ! retrieve list type info that DOES NOT require additional inputs
      select case (name)
         case("farrayPtr")
          if (getCoord) then
            ! get the dimcount
            call ESMF_GridGet(grid, dimCount=dimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
            if (dimCount == 1) then
              ! get the coordinates
              call ESMF_GridGetCoord(grid, coordDiml, farrayPtr=coords1D, &
                                     totalCount=totalCount, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
                ! check that valueList is large enough
                min_size = totalCount(1)
                if (size(valueList) < min_size) then 
                  call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                    msg="The valueList size is not large enough!", &
                    ESMF_CONTEXT, rcToReturn=rc)
                endif
                ! fill the valueList with coordinates
                valueList(1:totalCount(1)) = coords1D
            else if (dimCount == 2) then
              ! get the coordinates
              call ESMF_GridGetCoord(grid, coordDiml, farrayPtr=coords2D, &
                                     totalCount=totalCount, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
                ! check that valueList is large enough
                min_size = totalCount(1)*totalCount(2)
                if (size(valueList) < min_size) then 
                  call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                    msg="The valueList size is not large enough!", &
                    ESMF_CONTEXT, rcToReturn=rc)
                endif
                ! fill the valueList with coordinates
                valueList(1:totalCount(1)) = coords2D(1,:)
                valueList(totalCount(1)+1:totalCount(2)) = coords2D(2,:)
            else if (dimCount == 3) then
              ! get the coordinates
              call ESMF_GridGetCoord(grid, coordDiml, farrayPtr=coords3D, &
                                     totalCount=totalCount, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
                ! check that valueList is large enough
                min_size = totalCount(1)*totalCount(2)*totalCount(3)
                if (size(valueList) < min_size) then 
                  call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
                    msg="The valueList size is not large enough!", &
                    ESMF_CONTEXT, rcToReturn=rc)
                endif
                ! fill the valueList with coordinates
                !valueList(1:totalCount(1)) = coords3D(1,:)
                !valueList(totalCount(1)+1:totalCount(2)) = coords3D(2,:)
                !valueList(totalCount(2)+1:totalCount(3)) = coords3D(3,:)
            endif
          else
            call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
              msg="The required argument 'coordDim' was not specified!", &
              ESMF_CONTEXT, rcToReturn=rc)
          endif
        case DEFAULT
          call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
            msg="The provided 'name' does not correspond to internal info", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
      end select

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInfoR8List
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AttGetInfo"
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AttributeGetInfo - Get internal Attribute info
!
! !INTERFACE:
      ! Private name; call using ESMF_AttributeGetInfo()
      subroutine ESMF_GridAttGetInfoLogicalList(grid, name, valueList, &
                                                inputList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid
      character (len = *), intent(in) :: name
      logical, intent(out) :: valueList(:)
      character(len=*), intent(in), optional :: inputList(:)
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns internal info from the object.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An ESMF object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [valueList]
!           The value list of the internal info.
!     \item [inputList]
!           A list containing input information needed to retrieve info.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: localrc
      integer :: localDel
      integer :: i

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      localDel = 0

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      if (present(inputList)) then
        do i=1,size(inputList)
          if (index(inputList(i), "localDe") /= 0) then
            localDel = extractInfoInt(inputList(i))
          endif
        enddo
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
          msg="No input information was given!", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!! info with inputs !!!!!!!!!!!!!!!!!!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !case("isLBound") ! LOGICAL list, requires localDe
      !case("isUBound") ! LOGICAL list, requires localDe

      select case (name)
        case ("isLBound")
          call ESMF_GridGet(grid, localDe=localDel, isLBound=valueList, &
                            rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case ("isUBound")
          call ESMF_GridGet(grid, localDe=localDel, isUBound=valueList, &
                            rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        case DEFAULT
          call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_VALID, &
            msg="The provided 'name' does not correspond to internal info", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
      end select

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAttGetInfoLogicalList
!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Helper routines
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "AttributeInternalInfo"
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: AttributeInternalInfo - Query for internal Attribute info
!
! !INTERFACE:
      function AttributeInternalInfo(name, modName)

! !RETURN VALUE:
      logical :: AttributeInternalInfo

! !ARGUMENTS:
      character (len = *), intent(in) :: name
      character (len = *), intent(out)   :: modName

!
! !DESCRIPTION:
!     Returns .true. if this name indicates Attribute internal info.
!
!     The arguments are:
!     \begin{description}
!     \item [name]
!           The name of the Attribute.
!     \item [modName]
!           The name of the Attribute without the tag (ESMF:).
!     \end{description}
!
!
!EOPI

      ! Initialize to false
      AttributeInternalInfo = .false.

      ! get the length of name
      if ((len(name) <= 5)) then
        AttributeInternalInfo = .false.
      elseif (name(1:5) == "ESMF:") then
        modName = name(6:len(name))
        AttributeInternalInfo = .true.
      endif

      end function AttributeInternalInfo
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "extractInfo"
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: extractInfo - Get information out of string for AttributeGetInternal
!
! !INTERFACE:
      function extractInfoInt(string)

! !RETURN VALUE:
      integer :: extractInfoInt

! !ARGUMENTS:
      character (len = *), intent(in) :: string

!
! !DESCRIPTION:
!     Returns relevant information in proper format from the input string.
!
!     The arguments are:
!     \begin{description}
!     \item [string]
!           The string containing the information to be extracted.
!     \end{description}
!
!
!EOPI

      integer :: ind
      ! TODO remove ESMF_MAXSTR
      character(len=ESMF_MAXSTR) :: temp

      ! take everything after the colon, minus whitespace
      ind = index(string, ":")
      temp = trim(adjustl(string((ind+1):len(string))))
      ! convert 'temp' (string) to integer
      read (temp, '(i1)') extractInfoInt

      end function extractInfoInt
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "extractInfo"
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: extractInfo - Get information out of string for AttributeGetInternal
!
! !INTERFACE:
      function extractInfoValueString(string)

! !RETURN VALUE:
      ! TODO remove ESMF_MAXSTR
      character (len=ESMF_MAXSTR) :: extractInfoValueString

! !ARGUMENTS:
      character (len=*), intent(in) :: string

!
! !DESCRIPTION:
!     Returns relevant information in proper format from the input string.
!
!     The arguments are:
!     \begin{description}
!     \item [string]
!           The string containing the information to be extracted.
!     \end{description}
!
!
!EOPI

      integer :: ind

      ! take everything after the colon, minus whitespace
      ind = index(string, ":")
      extractInfoValueString = trim(adjustl(string((ind+1):len(string))))
      
      end function extractInfoValueString
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
end module ESMF_AttributeInternalsMod
!------------------------------------------------------------------------------
