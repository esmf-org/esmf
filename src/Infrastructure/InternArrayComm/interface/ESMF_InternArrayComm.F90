! $Id: ESMF_InternArrayComm.F90,v 1.25.2.3 2009/01/21 21:25:22 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_InternArrayComm.F90"
!==============================================================================
!
! ESMF Array Comm module
module ESMF_InternArrayCommMod
!
!==============================================================================
!
! This file contains the Array methods which do communication, and so
! must be compiled after the comm routines.  These are logically part 
! of the Array class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_IArrayCommMod - Data communication routines at the Array level
!
! !DESCRIPTION:
!
! The code in this file implements the distributed {\tt ESMF\_Array} class 
! communication routines.  The {\tt ESMF\_Array} class definitions and basic
! methods are in the Array module.  These routines are broken out into a
! separate module so that the Route and IGrid methods can be compiled 
! after the basic Array definitions, and before the ArrayComm routines.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_IOSpecMod
  use ESMF_LocalArrayMod
  use ESMF_InternArrayDataMapMod
  use ESMF_VMMod
  use ESMF_DELayoutMod  
  use ESMF_InternArrayMod
  use ESMF_InternArrayGetMod
  use ESMF_IGridTypesMod
  use ESMF_IGridMod
  use ESMF_RHandleMod
  use ESMF_RouteMod
  use ESMF_FieldDataMapMod
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

  public ESMF_IArrayGetAllAxisIndices

  public ESMF_IArrayHaloStore, ESMF_IArrayHalo, ESMF_IArrayHaloRelease
  public ESMF_IArrayRedistStore, ESMF_IArrayRedist, ESMF_IArrayRedistRelease
  public ESMF_IArrayHaloValidate, ESMF_IArrayRedistValidate
  ! Regrid methods are in ESMF_Regrid.F90

  public ESMF_IArrayGather, ESMF_IArrayScatter

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_InternArrayComm.F90,v 1.25.2.3 2009/01/21 21:25:22 cdeluca Exp $'
!
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !IROUTINE: ESMF_IArrayHaloStore - Compute info to Halo a distributed array
!
! !INTERFACE:
  interface ESMF_IArrayHaloStore

! !PRIVATE MEMBER FUNCTIONS:
    module procedure ESMF_IArrayHaloStoreOne
    module procedure ESMF_IArrayHaloStoreIndex

! !DESCRIPTION:
!     This interface provides for calling halo on 
!     an {\tt ESMF\_Array} object, or a list of compatible array objects.

!EOPI
  end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IArrayHalo - Halo a distributed array
!
! !INTERFACE:
  interface ESMF_IArrayHalo

! !PRIVATE MEMBER FUNCTIONS:
    module procedure ESMF_IArrayHaloOne
    module procedure ESMF_IArrayHaloList

! !DESCRIPTION:
!     This interface provides for calling Halo on a 
!     single {\tt ESMF\_Array} or a list of compatible array objects.

!EOPI
  end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IArrayHaloValidate - Validate a Halo
!
! !INTERFACE:
  interface ESMF_IArrayHaloValidate

! !PRIVATE MEMBER FUNCTIONS:
    module procedure ESMF_IArrayHaloValidateOne
    module procedure ESMF_IArrayHaloValidateList

! !DESCRIPTION:
!     This interface provides for calling halo validate on a 
!     single {\tt ESMF\_Array} or a list of compatible array objects.

!EOPI
  end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IArrayRedistStore - Compute info to Redistribute an Array
!
! !INTERFACE:
  interface ESMF_IArrayRedistStore

! !PRIVATE MEMBER FUNCTIONS:
    module procedure ESMF_IArrayRedistStoreOne
    module procedure ESMF_IArrayRedistStoreIndex

! !DESCRIPTION:
!     This interface provides for
!      calling redistribute store on an {\tt ESMF\_Array} object, or a
!      list of compatible array objects.

!EOPI
  end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IArrayRedist - Redistribute an Array
!
! !INTERFACE:
  interface ESMF_IArrayRedist

! !PRIVATE MEMBER FUNCTIONS:
    module procedure ESMF_IArrayRedistOne
    module procedure ESMF_IArrayRedistList

! !DESCRIPTION:
!     This interface provides for
!      calling redistribute on an {\tt ESMF\_Array} object, or a
!      list of compatible array objects.

!EOPI
  end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IArrayRedistValidate - Validate a Redistribution
!
! !INTERFACE:
  interface ESMF_IArrayRedistValidate

! !PRIVATE MEMBER FUNCTIONS:
    module procedure ESMF_IArrayRedistValidateOne
    module procedure ESMF_IArrayRedistValidateList

! !DESCRIPTION:
!     This interface provides for
!      calling redistribute validate on an {\tt ESMF\_Array} object, or a
!      list of compatible array objects.

!EOPI
  end interface


!==============================================================================

  contains

!==============================================================================


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayGather"
!BOPI
! !IROUTINE: ESMF_IArrayGather - Gather an Array onto one DE
!
! !INTERFACE:
  subroutine ESMF_IArrayGather(array, igrid, datamap, rootDE, gatheredArray, rc)
!
! !ARGUMENTS:
    type(ESMF_InternArray), intent(in) :: array
    type(ESMF_IGrid), intent(inout) :: igrid
    type(ESMF_FieldDataMap), intent(inout) :: datamap
    integer, intent(in) :: rootDE
    type(ESMF_InternArray), intent(out) :: gatheredArray
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Gather a distributed {\tt ESMF\_Array} over multiple DEs into 
!  a single {\tt ESMF\_Array} on one DE.
!
!  The arguments are:
!     \begin{description}
!     \item [array]
!           {\tt ESMF\_Array} containing distributed data to be gathered.
!     \item [igrid]
!           {\tt ESMF\_IGrid} which corresponds to the distributed data.
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap} which describes the mapping of the
!           data onto the cells in the {\tt ESMF\_IGrid}.
!     \item [rootDE]
!           The DE number on which the resulting gathered {\tt ESMF\_Array}
!           will be created.  
!     \item [gatheredArray]
!           On the {\tt rootDE}, the resulting gathered {\tt ESMF\_Array}.
!           On all other DEs, an invalid {\tt ESMF\_Array}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

    integer :: status         ! local error status
    integer :: igridrank, datarank
    integer :: i, j, nDEs
    type(ESMF_DELayout) :: delayout
    type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc
    integer, dimension(ESMF_MAXDIM) :: decompids
    integer, dimension(:,:), pointer :: localAxisLengths, tempCCPDEPD
    integer, dimension(ESMF_MAXDIM) :: dimOrder, dimlengths
    integer, dimension(ESMF_MAXIGRIDDIM) :: decomps
    integer:: size_decomp
    integer, dimension(ESMF_MAXDIM) :: localMaxDimCount, globalCellDim
    integer, dimension(:), allocatable :: tempMLCCPD, tempGCCPD

    ! initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, igrid, rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, datamap)

    ! extract necessary information from the igrid
    call ESMF_IGridGet(igrid, dimCount=igridrank, delayout=delayout, rc=status)
    
    call ESMF_DELayoutGetDeprecated(delayout, deCount=nDEs, &
      dimCount=size_decomp, rc=status)
    
    ! gjt size_decomp can be hardcoded to 2 here because that's what it had to be
      
    allocate(localAxisLengths(nDEs,ESMF_MAXDIM), stat=status)
    allocate( tempMLCCPD(     igridrank), stat=status)
    allocate(  tempGCCPD(     igridrank), stat=status)
    allocate(tempCCPDEPD(nDEs,igridrank), stat=status)

    ! Query the datamap and set info for igrid so it knows how to match up the
    ! array indices and the igrid indices.
    call ESMF_FieldDataMapGet(datamap, dataIndexList=dimOrder, &
                              horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                              rc=status)
    if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    call ESMF_IGridGet(igrid, &
                        horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                        globalCellCountPerDim=tempGCCPD, &
                        cellCountPerDEPerDim=tempCCPDEPD, &
                        maxLocalCellCountPerDim=tempMLCCPD, rc=rc)
    ! call ESMF_IGridGet(igrid, decomps, rc=status)   !TODO: add decomps
    if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
    ! size_decomp = size(decompids) ! already have this value
    decomps(1) = 1    ! TODO: remove this once the igrid call is created
    decomps(2) = 2

    ! get the Array sizes
    call ESMF_InternArrayGet(array, rank=datarank, counts=dimlengths, rc=status)
    if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    ! calculate decompids and dimlengths, modified if necessary by dimorders
    do i=1, datarank
      decompids(i) = dimorder(i)
      globalCellDim(i) = dimlengths(i)
      localMaxDimCount(i) = dimlengths(i)
      do j=1, nDEs
        localAxisLengths(j,i) = dimlengths(i)
      enddo
      if(dimorder(i).ne.0) then
        decompids(i) = decomps(dimorder(i))
        globalCellDim(i) = tempGCCPD(dimorder(i))
        localMaxDimCount(i) = tempMLCCPD(dimorder(i))
        do j=1, nDEs
          localAxisLengths(j,i) = tempCCPDEPD(j,dimorder(i))
        enddo
      endif
    enddo


    ! call c routine to gather
    call c_ESMC_IArrayGather(array, delayout, decompids, size_decomp, &
                            localAxisLengths, globalCellDim, localMaxDimCount, &
                            rootDE, gatheredArray, status)
    ! Init gathered Array as created
    call ESMF_InternArraySetInitCreated(gatheredArray)

#if 0
        call c_ESMC_IArrayAllGather(array, delayout, decompids, datarank, &
                                   localAxisLengths, globalCellDim, &
                                   localMaxDimCount, gatheredArray, status)
#endif

    if (ESMF_LogMsgFoundError(status, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return

    ! Clean up
    deallocate(localAxisLengths, stat=status)
    deallocate(      tempMLCCPD, stat=status)
    deallocate(       tempGCCPD, stat=status)
    deallocate(     tempCCPDEPD, stat=status)

    ! set return code if user specified it
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IArrayGather

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayGetAIsAllDEs"
!BOPI
! !IROUTINE: ESMF_IArrayGetAIsAllDEs - Get AxisIndex list for all DEs in a distributed Array
!
! !INTERFACE:
  subroutine ESMF_IArrayGetAIsAllDEs(array, igrid, datamap, &
                                      localGlobalFlag, domainTypeFlag, &
                                      AIListPerDEPerRank, rc)
!
! !ARGUMENTS:
    type(ESMF_InternArray), intent(in) :: array
    type(ESMF_IGrid), intent(inout) :: igrid
    type(ESMF_FieldDataMap), intent(inout) :: datamap
    type(ESMF_LocalGlobalFlag), intent(in) :: localGlobalFlag
    type(ESMF_DomainTypeFlag), intent(in) :: domainTypeFlag
    type(ESMF_AxisIndex), dimension(:,:), pointer :: AIListPerDEPerRank
    integer, intent(out), optional :: rc
! 
! !DESCRIPTION:
!   
!   Used to retrieve the AxisIndex lists from all {\tt ESMF\_Array}s         
!   associated with a {\tt ESMF\_IGrid}.  
!   The axes associated with the IGrid need halo widths added to the numbers
!   coming back from the IGridGet routines; the axes which are non-igrid
!   associated need to get the sizes from the array itself, and must be
!   the same across all DEs.  The DataMap is needed to query the mapping
!   between the axes order in the array and the axes order in the IGrid.
!
!   If the AIListPerDEPerRank is unallocated, this routine will query
!   for the right sizes, allocate the space, and fill it in.  In that case
!   the calling code *must* deallocate the space when done.  
!
!   If the AIListPerDEPerRank is already allocated, it must be (nDES, rank)
!   where rank is the *datarank* from the array and not the igrid rank.
!
!  The arguments are:
!     \begin{description}
!     \item [array]
!           {\tt ESMF\_Array} containing data.  Needed here not for the data
!           contents but to read the sizes of the non-igrid indices and to
!           look up the halo widths for the igrid-based indices.
!     \item [igrid]
!           {\tt ESMF\_IGrid} which corresponds to the array data.
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap} which describes the mapping of the
!           data onto the cells in the {\tt ESMF\_IGrid}.  Needed to match up
!           the array axes and the igrid axes.
!     \item [localGlobalFlag]
!           Return sizes and offsets either relative to this chunk (local)
!           or relative to the entire undecomposed igrid (global).
!           Valid values are {\tt ESMF\_LOCAL} or {\tt ESMF\_GLOBAL}.
!     \item [domainTypeFlag]
!           TypeKind of counts you want back.  Valid values are:
!           {\tt ESMF\_DOMAIN\_EXCLUSIVE}, 
!           {\tt ESMF\_DOMAIN\_COMPUTATIONAL},
!           {\tt ESMF\_DOMAIN\_TOTAL} or {\tt ESMF\_DOMAIN\_ALLOCATED}.
!     \item [AIListPerDEPerRank]
!           This is why you called this routine.  (Ok, there's more.)
!           This must either be a 2d unallocated array or pointer of 
!           {\tt ESMF\_AxisIndex} derived types, or an array of them which
!           is *exactly* (nDEs, datarank) long.  This routine fills in these
!           values.  Presto.  
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

    integer :: localrc, nDEs, i
    integer :: igridrank, datarank
    integer, dimension(:), allocatable :: dimOrder
    type(ESMF_AxisIndex), dimension(:), pointer :: myArrayAIsPerRank
    type(ESMF_AxisIndex), dimension(:,:), pointer :: igridAIsPerDEPerRank
    type(ESMF_DELayout) :: delayout
    type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc

     ! initialize return code; assume routine not implemented
     if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
!    ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, igrid, rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, datamap)

    ! make sure the compilers can tell these are unassociated.
    nullify(myArrayAIsPerRank)
    nullify(igridAIsPerDEPerRank)

    ! get layout from the igrid in order to get the number of DEs
    call ESMF_InternArrayGet(array, rank=datarank, rc=localrc)
    call ESMF_IGridGet(igrid, dimCount=igridrank, delayout=delayout, &
                      rc=localrc)
    call ESMF_DELayoutGet(delayout, deCount=nDEs, rc=localrc)

    ! check if the AI array pointer is associated
    !  -  If it is, check that it is right size to hold the requested data.
    !  -  If it is not, allocate it here
    if (associated(AIListPerDEPerRank)) then
      if (size(AIListPerDEPerRank,1).ne.nDEs .OR. &
          size(AIListPerDEPerRank,2).ne.datarank) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                           "AIList array not correct size for requested data", &
                           ESMF_CONTEXT, rc)
        return
      endif
    else
      allocate(AIListPerDEPerRank(nDEs,datarank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "allocating AIList array", &
                                     ESMF_CONTEXT, rc)) return
    endif

    ! get information from the datamap
    allocate(dimOrder(datarank), stat=localrc)
    call ESMF_FieldDataMapGet(datamap, dataIndexList=dimOrder, &
                              horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                              rc=localrc)

    ! call igrid to access AIs for all DEs
    call ESMF_IGridGetAIsAllDEs(igrid, localGlobalFlag, &
                               igridAIsPerDEPerRank, &
                               horzRelLoc, vertRelLoc, rc=localrc)

    ! allocate arrayindex array; get all types (excl, comp, total?) from the array
    allocate(myArrayAIsPerRank(datarank), stat=localrc)
    call ESMF_IArrayGetAxisIndex(array, ESMF_DOMAIN_COMPUTATIONAL, &
                                myArrayAIsPerRank, rc=localrc)

    ! load AIListPerDEPerRank with array AIs and igrid AIs
    do i = 1,datarank
      if (dimOrder(i).eq.0) then
        AIListPerDEPerRank(:,i) = myArrayAIsPerRank(i)
      else
        AIListPerDEPerRank(:,i) = igridAIsPerDEPerRank(:,dimOrder(i))
      endif
    enddo

    ! if the request was for computational domain, we're done now
    ! TODO: case statement on domainTypeFlag -- nothing to do for comp

    ! TODO: modify if not computational
    ! if the request was for anything else, we have to query for the halo
    ! widths.  they should come back (datarank, 2). 
    ! then we make a new AI modify routine which takes this halo array,
    ! plus a domain type, and either adds or subtracts to get the right
    ! values.  (you cannot ask for allocate here because you can't.)  (see
    ! note 1.)   the input here will be (nDEs, datarank) long, and this
    ! new routine just computes over all of them.

    ! note 1:
    ! there is no way to know the allocate size for all nDEs in a
    ! distributed array without a broadcast.

    ! Clean up
    deallocate(dimOrder,            stat=localrc)
    deallocate(myArrayAIsPerRank,   stat=localrc)
    deallocate(igridAIsPerDEPerRank, stat=localrc)

    if (present(rc)) rc = localrc

  end subroutine ESMF_IArrayGetAIsAllDEs

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayGetGlobalAIs"
!BOPI
! !IROUTINE: ESMF_IArrayGetGlobalAIs - Get AIs for local chunk in global index space
!
! !INTERFACE:
  subroutine ESMF_IArrayGetGlobalAIs(array, igrid, datamap, &
                                      domainTypeFlag, globalAIPerRank, rc)
!
! !ARGUMENTS:
    type(ESMF_InternArray), intent(in) :: array
    type(ESMF_IGrid), intent(inout) :: igrid
    type(ESMF_FieldDataMap), intent(inout) :: datamap
    type(ESMF_DomainTypeFlag), intent(in) :: domainTypeFlag
    type(ESMF_AxisIndex), dimension(:), pointer :: globalAIPerRank
    integer, intent(out), optional :: rc
! 
! !DESCRIPTION:
!   
!   This routine returns AI info in global index space.
!   If you want local AI info, you can ask the array directly.  If you want
!   AIs in global index space, you need to query the igrid for the global 
!   offsets for each DE, then pick out which one you are and add that.
!   This is the same regardless of which kind of AI you are asking to return.
!   This routine queries the array for the local AIs of the requested type 
!   and adds the global offset to move the index space from local to global.
!
!  The arguments are:
!     \begin{description}
!     \item [array]
!           {\tt ESMF\_Array} containing data.  Needed here not for the data
!           contents but to read the sizes of the non-igrid indices and to
!           look up the halo widths for the igrid-based indices.
!     \item [igrid]
!           {\tt ESMF\_IGrid} which corresponds to the array data.
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap} which describes the mapping of the
!           data onto the cells in the {\tt ESMF\_IGrid}.  Needed to match up
!           the array axes and the igrid axes.
!     \item [domainTypeFlag]
!           TypeKind of counts you want back.  Valid values are:
!           {\tt ESMF\_DOMAIN\_EXCLUSIVE}, 
!           {\tt ESMF\_DOMAIN\_COMPUTATIONAL},
!           {\tt ESMF\_DOMAIN\_TOTAL} or {\tt ESMF\_DOMAIN\_ALLOCATED}.
!     \item [globalAIPerRank]
!           AI list per rank of the kind of AI you requested with the domain
!           flag.  This must come in already allocated.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

    integer :: localrc, i
    integer :: datarank
    integer :: dimOrder(ESMF_MAXDIM)
    integer :: igridOffsets(ESMF_MAXIGRIDDIM)
    type(ESMF_AxisIndex),pointer :: localAIsPerRank(:)
    type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc

     ! initialize return code; assume routine not implemented
     if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
!    ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, igrid, rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, datamap)

    ! get layout from the igrid in order to get the number of DEs
    call ESMF_InternArrayGet(array, rank=datarank, rc=localrc)

    ! allocate array for localAIs
    allocate(localAIsPerRank(datarank), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "allocating localAIsPerRank", &
                                     ESMF_CONTEXT, rc)) return

    ! get information from the datamap
    call ESMF_FieldDataMapGet(datamap, dataIndexList=dimOrder, &
                              horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                              rc=localrc)

    ! call igrid to access AIs for all DEs
    call ESMF_IGridGetDELocalInfo(igrid, horzRelLoc, vertRelLoc, &
                                 globalStartPerDim=igridOffsets, rc=localrc)

    ! allocate arrayindex array; get requested type from array
    call ESMF_IArrayGetAxisIndex(array, domainTypeFlag, &
                                localAIsPerRank, rc=localrc)

    ! load AIListPerDEPerRank with array AIs and igrid AIs
    ! adding igrid offsets for igrid-aligned axes
    do i = 1,datarank
      if (dimOrder(i).eq.0) then
        globalAIPerRank(i) = localAIsPerRank(i)
      else
        globalAIPerRank(i) = localAIsPerRank(i)
        globalAIPerRank(i)%min = globalAIPerRank(i)%min + &
                                                    igridOffsets(dimOrder(i))
        globalAIPerRank(i)%max = globalAIPerRank(i)%max + &
                                                    igridOffsets(dimOrder(i))
      endif
    enddo

    ! deallocate localAIsPerRank
    deallocate(localAIsPerRank)
  
    if (present(rc)) rc = localrc

  end subroutine ESMF_IArrayGetGlobalAIs

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayGetAllAxisIndices"
!BOPI
! !IROUTINE: ESMF_IArrayGetAllAxisIndices - Get all AIs associated with a IGrid
!
! !INTERFACE:
  subroutine ESMF_IArrayGetAllAxisIndices(array, igrid, datamap, &
                                             totalindex, compindex, exclindex, &
                                             AICountPerDE, rc)
!
! !ARGUMENTS:
    type(ESMF_InternArray), intent(in) :: array
    type(ESMF_IGrid), intent(inout) :: igrid
    type(ESMF_FieldDataMap), intent(inout) :: datamap
    type(ESMF_AxisIndex), dimension(:,:), pointer, optional :: totalindex
    type(ESMF_AxisIndex), dimension(:,:), pointer, optional :: compindex
    type(ESMF_AxisIndex), dimension(:,:), pointer, optional :: exclindex
    integer, dimension(:), pointer, optional :: AIcountPerDE
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Used to retrieve the index annotations from all {\tt ESMF\_Array}s
!    associated with a {\tt ESMF\_IGrid}.  This computes the values
!    instead of broadcasting them.
!EOPI

    integer :: status, nDEs, nAIs, i, j
    integer :: igridrank, datarank, maxrank
    integer, dimension(:), allocatable :: dimOrder, countPerDim
    type(ESMF_AxisIndex), dimension(:), pointer :: arrayindex
    type(ESMF_AxisIndex), dimension(:,:), pointer :: igridindex, globalindex
    type(ESMF_DELayout) :: delayout
    type(ESMF_IGridStorage) :: igridStorage
    type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc

     ! initialize return code; assume routine not implemented
     if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, igrid, rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, datamap)

      ! get layout from the igrid in order to get the number of DEs
      call ESMF_InternArrayGet(array, rank=datarank, rc=status)
      call ESMF_IGridGet(igrid, dimCount=igridrank, delayout=delayout, &
                        igridStorage=igridStorage, rc=status)
      call ESMF_DELayoutGet(delayout, deCount=nDEs, rc=status)

      ! allocate dimOrder array and get from datamap
      maxrank = max(datarank, igridrank)
      if (igridStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) &
        maxrank = max(datarank+1, igridrank)     ! possible for arbitrary storage
      allocate(dimOrder(maxrank), stat=status)
      call ESMF_FieldDataMapGet(datamap, dataIndexList=dimOrder, &
                           horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                           rc=status)
    
      ! for arbitrary storage, modify the dimOrder array
      if (igridStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) then
        do i = maxrank,igridrank,-1
          dimOrder(i) = dimOrder(i-1)
        enddo
        dimOrder(igridrank) = 2
      endif

      ! set the number of AIs based on the igrid storage
      nAIs = nDEs
      if (igridStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) then
        allocate(countPerDim(igridrank))
        call ESMF_IGridGet(igrid, horzrelloc=horzRelLoc, vertrelloc=vertRelLoc, &
                          globalCellCountPerDim=countPerDim, rc=status)
        nAIs = 1
        do i = 1,igridrank
          nAIs = nAIs * countPerDim(i)
        enddo
        deallocate(countPerDim, stat=status)
      endif

      ! allocate arrayindex array and get all of them from the array
      allocate(arrayindex(datarank), stat=status)
      call ESMF_IArrayGetAxisIndex(array, arrayindex, rc=status)

      ! allocate igridindex array and get all of them from the igrid
      allocate(igridindex(nAIs,igridrank), stat=status)
      call ESMF_IGridGetAllAxisIndex(igrid, igridindex, &
                                    horzRelLoc=horzRelLoc, &
                                    vertRelLoc=vertRelLoc, &
                                    AICountPerDE=AICountPerDE, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

      ! load globalindex with arrayindex and igridindex
      allocate(globalindex(nAIs, maxrank), stat=status)
      do i = 1,maxrank
        if (dimOrder(i).eq.0) then
          globalindex(:,i) = arrayindex(i)
        else
          globalindex(:,i) = igridindex(:,dimOrder(i))
        endif
      enddo

      if (igridStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) then
        if (present(compindex)) then
          do j=1,size(compindex, 2)
            do i=1, nAIs
              compindex(i,j)%min    = globalindex(i,j)%min
              compindex(i,j)%max    = globalindex(i,j)%max
              compindex(i,j)%stride = globalindex(i,j)%stride
            enddo
          enddo
       endif

      else
        if (present(totalindex)) then
          call c_ESMC_IArrayGetAllAxisIndex(array, ESMF_DOMAIN_TOTAL, &
                                           globalindex, nAIs, datarank, &
                                           totalindex, status)
          if (status .ne. ESMF_SUCCESS) goto 10
          ! translate from C++ to F90
          do j=1,size(totalindex, 2)
            do i=1, nDEs
              totalindex(i,j)%min = totalindex(i,j)%min + 1
              totalindex(i,j)%max = totalindex(i,j)%max + 1
            enddo
          enddo
        endif

        if (present(compindex)) then
          call c_ESMC_IArrayGetAllAxisIndex(array, ESMF_DOMAIN_COMPUTATIONAL, &
                                           globalindex, nAIs, datarank, &
                                           compindex, status)
          if (status .ne. ESMF_SUCCESS) goto 10
          ! translate from C++ to F90
          do j=1,size(compindex, 2)
            do i=1, nDEs
              compindex(i,j)%min = compindex(i,j)%min + 1
              compindex(i,j)%max = compindex(i,j)%max + 1
            enddo
          enddo
        endif

        if (present(exclindex)) then
          call c_ESMC_IArrayGetAllAxisIndex(array, ESMF_DOMAIN_EXCLUSIVE, &
                                           globalindex, nAIs, datarank, &
                                           exclindex, status)
          if (status .ne. ESMF_SUCCESS) goto 10
          ! translate from C++ to F90
          do j=1,size(exclindex, 2)
            do i=1, nDEs
              exclindex(i,j)%min = exclindex(i,j)%min + 1
              exclindex(i,j)%max = exclindex(i,j)%max + 1
            enddo
          enddo
        endif
      endif

 10   continue

      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

      ! Clean up
      deallocate(dimOrder,    stat=status)
      deallocate(arrayindex,  stat=status)
      deallocate(igridindex,   stat=status)
      deallocate(globalindex, stat=status)

      if (present(rc)) rc = ESMF_SUCCESS 

  end subroutine ESMF_IArrayGetAllAxisIndices

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayHaloList"
!BOPI
! !IROUTINE: ESMF_IArrayHalo - Halo a list of Arrays
!
! !INTERFACE:
    ! Private name; call using ESMF_IArrayHalo()
  subroutine ESMF_IArrayHaloList(arrayList, routehandle, routeIndex, &
                                  blocking, routeOptions, rc)
!
! !ARGUMENTS:
    type(ESMF_InternArray), intent(inout) :: arrayList(:)
    type(ESMF_RouteHandle), intent(in) :: routehandle
    integer, intent(in), optional :: routeIndex
    type(ESMF_BlockingFlag), intent(in), optional :: blocking
    type(ESMF_RouteOptions), intent(in), optional :: routeOptions
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Perform a halo operation over the data in a list of {\tt ESMF\_Array}s.
!   This routine updates the data inside the {\tt ESMF\_Array}s in place.
!   It uses a precomputed {\tt ESMF\_Route} for the communications pattern.
!   (See {\tt ESMF\_ArrayHaloPrecompute()} for how to precompute and 
!   associate an {\tt ESMF\_Route} with an {\tt ESMF\_RouteHandle}).
!
!   \begin{description}
!   \item [arrayList]
!         List of {\tt ESMF\_Array}s containing data to be haloed.
!   \item [routehandle]
!         {\tt ESMF\_RouteHandle} which was returned from an
!         {\tt ESMF\_ArrayHaloPrecompute()} call.
!   \item [{[routeIndex]}]
!         If specified, select which of possibly multiple routes to execute
!         from this route handle.  Default value is 1.
!   \item [{[blocking]}]
!         Optional argument which specifies whether the operation should
!         wait until complete before returning or return as soon
!         as the communication between DEs has been scheduled.
!         If not present, default is to do synchronous communications.
!         Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!         {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!    \item [{[routeOptions]}]
!          Not normally specified.  Specify which internal strategy to select
!          when executing the communication needed to execute the halo.
!          See Section~\ref{opt:routeopt} for possible values.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

    integer :: status         ! local error status
    integer :: i, nitems
    type(ESMF_LocalArray), allocatable :: local_arrayList(:)
    type(ESMF_Route) :: route

    ! initialize return code; assume failure until success is certain
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
    ! Check init status of arguments
    do i=1, size(arrayList)
      ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, arrayList(i), rc)
    enddo
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

    if (present(routeIndex)) then
        call ESMF_RouteHandleGet(routehandle, which_route=routeIndex, &
                                      route=route, rc=status)
    else
        call ESMF_RouteHandleGet(routehandle, which_route=1, &
                                      route=route, rc=status)
    endif
    if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    nitems = size(arrayList)
    allocate(local_arrayList(nitems), stat=status)
    if (ESMF_LogMsgFoundAllocError(status, &
                                   "Allocating localarraylist information", &
                                    ESMF_CONTEXT, rc)) return

    ! fortran equivalent of a cast - routerun wants a local array 
    do i=1, nitems
      local_arrayList(i)%this%ptr = arrayList(i)%this%ptr
      ESMF_INIT_COPY(local_arrayList(i),arrayList(i))
    enddo

    ! Set the route options if given, then execute.
    if (present(routeOptions)) then
        call c_ESMC_RouteSet(route, routeOptions, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
    endif


    call ESMF_RouteRunList(route, local_arrayList, rc=status)
    if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    deallocate(local_arrayList, stat=status)
    ! do not error check this; preserve rc from routerun

    ! last call to routerun set rc
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IArrayHaloList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayHaloOne"
!BOPI
! !IROUTINE: ESMF_IArrayHalo - Halo an Array
!
! !INTERFACE:
    ! Private name; call using ESMF_IArrayHalo()
  subroutine ESMF_IArrayHaloOne(array, routehandle, routeIndex, &
                                 blocking, routeOptions, rc)
!
! !ARGUMENTS:
    type(ESMF_InternArray), intent(inout) :: array
    type(ESMF_RouteHandle), intent(in) :: routehandle
    integer, intent(in), optional :: routeIndex
    type(ESMF_BlockingFlag), intent(in), optional :: blocking
    type(ESMF_RouteOptions), intent(in), optional :: routeOptions
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Perform a halo operation over the data in an {\tt ESMF\_Array}.
!   This routine updates the data inside the {\tt ESMF\_Array} in place.
!   It uses a precomputed {\tt ESMF\_Route} for the communications pattern.
!   (See {\tt ESMF\_ArrayHaloPrecompute()} for how to precompute and 
!   associate an {\tt ESMF\_Route} with an {\tt ESMF\_RouteHandle}).
!
!   \begin{description}
!   \item [array]
!         {\tt ESMF\_Array} containing data to be haloed.
!   \item [routehandle]
!         {\tt ESMF\_RouteHandle} which was returned from an
!         {\tt ESMF\_ArrayHaloPrecompute()} call.
!   \item [{[routeIndex]}]
!         If specified, select which of possibly multiple routes to execute
!         from this route handle.  Default value is 1.
!   \item [{[blocking]}]
!         Optional argument which specifies whether the operation should
!         wait until complete before returning or return as soon
!         as the communication between DEs has been scheduled.
!         If not present, default is to do synchronous communications.
!         Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!         {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the halo.
!           See Section~\ref{opt:routeopt} for possible values.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

    integer :: status         ! local error status
    type(ESMF_LocalArray) :: local_array
    type(ESMF_Route) :: route

    ! initialize return code; assume failure until success is certain
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
    ! Check init status of arguments
!    ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)

    if (present(routeIndex)) then
        call ESMF_RouteHandleGet(routehandle, which_route=routeIndex, &
                                 route=route, rc=status)
    else
        call ESMF_RouteHandleGet(routehandle, which_route=1, &
                                 route=route, rc=status)
    endif
    if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    ! fortran equivalent of a cast - routerun wants a local array 
    local_array%this%ptr = array%this%ptr
    ESMF_INIT_COPY(local_array,array)

    ! Set the route options if given, then execute the route.
    if (present(routeOptions)) then
        call c_ESMC_RouteSet(route, routeOptions, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
    endif


    call ESMF_RouteRun(route, local_array, rc=status)
    if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IArrayHaloOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayHaloRelease"
!BOPI
! !IROUTINE: ESMF_IArrayHaloRelease - Release resources stored for halo operation
!
! !INTERFACE:
  subroutine ESMF_IArrayHaloRelease(routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout) :: routehandle
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     When the precomputed information about a halo operation is no longer
!     needed, this routine releases the associated resources.
!
!     \begin{description}
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} associated with halo operation which
!           should be released.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
    integer :: localrc                        ! local return code

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)

    call ESMF_RouteHandleDestroy(routehandle, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IArrayHaloRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayHaloStoreOne"
!BOPI
! !IROUTINE: ESMF_IArrayHaloStore - Store resources for a halo operation
!
! !INTERFACE:
      ! Private interface; call using ESMF_IArrayHaloStore()
  subroutine ESMF_IArrayHaloStoreOne(array, localFlag, igrid, datamap, &
    routehandle, halodirection, routeOptions, rc)
!
! !ARGUMENTS:
    type(ESMF_InternArray), intent(inout) :: array
    logical, intent(in) :: localFlag
    type(ESMF_IGrid), intent(inout) :: igrid
    type(ESMF_FieldDataMap), intent(inout) :: datamap
    type(ESMF_RouteHandle), intent(out) :: routehandle
    type(ESMF_HaloDirection), intent(in), optional :: halodirection
    type(ESMF_RouteOptions), intent(in), optional :: routeOptions
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Precompute the data movements needed to 
!     perform a halo operation over the data in an {\tt ESMF\_Array}.  
!     It associates this information with the {\tt routehandle}, which 
!     should then provided to {\tt ESMF\_ArrayHalo()} at execution time.
!     The {\tt ESMF\_IGrid} and {\tt ESMF\_FieldDataMap} are used as
!     templates to understand how this {\tt ESMF\_Array} relates 
!     to {\tt ESMF\_Array}s on other DEs.
!
!     \begin{description}
!     \item [array]
!           {\tt ESMF\_Array} containing data to be haloed.
!     \item [igrid]
!           {\tt ESMF\_IGrid} which matches how this data was decomposed.
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap} which matches how the data in the
!           {\tt ESMF\_Array} relates to the given {\tt ESMF\_IGrid}.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} is returned to be used during the
!           execution of the halo operation.
!     \item [{[halodirection]}]
!           {\tt ESMF\_HaloDirection} to indicate which of the boundaries
!           should be updated.  If not specified, all boundaries are updated.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the halo.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc                        ! local return code
      ! note that the routehandle coming in here is intent(out) because
      ! it is going to be created from scratch in the subsequent call.
      ! however, the StoreIndex() call has to have the routehandle intent
      ! as (inout) because setting any index > 1 will add a route to
      ! an existing routehandle and *not* call the create code.
  
      ! TODO: i have a feeling that some compilers will not like this
      ! arrangement.  solutions might be to either make the intent at
      ! this level (inout) as well, even though that is confusing to users,
      ! or make a temporary here and do an assignment before returning.

      ! initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Check init status of arguments
      if (localFlag) then
        ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc)
      endif
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, igrid, rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, datamap)

      ! passthru call, setting index to 1 and type 1-to-1
      call ESMF_IArrayHaloStoreIndex(array, localFlag, 1, ESMF_1TO1HANDLEMAP, &
        1, igrid, datamap, routehandle, halodirection, routeOptions, rc=localrc)

      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IArrayHaloStoreOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayHaloStoreIndex"
!BOPI
! !IROUTINE: ESMF_IArrayHaloStoreIndex - Store resources for a halo operation
!
! !INTERFACE:
      ! Internal routine, intended to be called directly by FieldBundle code only
  subroutine ESMF_IArrayHaloStoreIndex(array, localFlag, index, rmaptype, &
    maxindex, igrid, datamap, routehandle, halodirection, routeOptions, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(inout) :: array
      logical, intent(in) :: localFlag
      integer, intent(in) :: index
      integer, intent(in) :: rmaptype
      integer, intent(in) :: maxindex
      type(ESMF_IGrid), intent(inout) :: igrid
      type(ESMF_FieldDataMap), intent(inout) :: datamap
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_HaloDirection), intent(in), optional :: halodirection
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Precompute the data movements needed to 
!     perform a halo operation over the data in an {\tt ESMF\_Array}.  
!     It associates this information with the {\tt routehandle}, which 
!     should then provided to {\tt ESMF\_ArrayHalo()} at execution time.
!     The {\tt ESMF\_IGrid} and {\tt ESMF\_FieldDataMap} are used as
!     templates to understand how this {\tt ESMF\_Array} relates 
!     to {\tt ESMF\_Array}s on other DEs.
!
!     \begin{description}
!     \item [array]
!           {\tt ESMF\_Array} containing data to be haloed.
!     \item [index]
!           Integer specifying which route number this is to be inside
!           the {\tt routehandle}.  Route handles for bundles can contain
!           either a single route which applies to all fields in the bundle,
!           or multiple routes which correspond to the differences in
!           data motion for different array sizes, ranks, relative locations,
!           data types, etc.  Since this is Fortran, index 1 is the first
!           route number.
!     \item [rmaptype]
!           Integer parameter value used if the index number is 1.
!           Controls whether the route handle will contain a separate route
!           for each index, or if all indices map to the same route.
!     \item [maxindex]
!           If {\tt index} is 1, set the maximum number of routes which
!           will eventually be set.  If not known, use 1.  The list of
!           routes can be reallocated and expanded later.
!     \item [igrid]
!           {\tt ESMF\_IGrid} which matches how this data was decomposed.
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap} which matches how the data in the
!           {\tt ESMF\_Array} relates to the given {\tt ESMF\_IGrid}.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} is returned to be used during the
!           execution of the halo operation.
!     \item [{[halodirection]}]
!           {\tt ESMF\_HaloDirection} to indicate which of the boundaries
!           should be updated.  If not specified, all boundaries are updated.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the halo.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: status         ! local error status
      type(ESMF_DELayout) :: delayout
      type(ESMF_VM) :: vm
      type(ESMF_Logical), dimension(:), allocatable :: periodic
      type(ESMF_AxisIndex), dimension(:,:), pointer :: src_AI, dst_AI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: gl_src_AI, gl_dst_AI
      type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc
      type(ESMF_Route) :: route
      integer, dimension(:), allocatable :: globalCellCountPerDim, decompids
      integer, dimension(ESMF_MAXDIM) :: dimorder, dimlengths
      integer, dimension(:,:), allocatable :: globalStartPerDEPerDim
      integer :: nDEs, my_DE
      integer :: igridrank, datarank, cellCount

      ! initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
    ! Check init status of arguments
    if (localFlag) then
      ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc)
    endif
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, igrid, rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, datamap)

      ! TODO: all this code could be moved to the C++ side once IGrid has
      !       an interface

      ! create the routehandle if this is the first route.  otherwise
      ! we are adding a route to an existing handle and we do not want
      ! to overwrite it.
      if (index .eq. 1) then
          routehandle = ESMF_RouteHandleCreate(status)
          ! set the type and mapping
          call ESMF_RouteHandleSet(routehandle, htype=ESMF_HALOHANDLE, &
                                   route_count=maxindex, rmaptype=rmaptype, &
                                   tv_count=0, tvmaptype=ESMF_NOHANDLEMAP, &
                                   rc=status)
      else
          ! if adding to an existing handle make sure it exists
          ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
      endif
    
      ! Extract layout information from the IGrid
      call ESMF_IGridGet(igrid, delayout=delayout, rc=status)
      call ESMF_IGridGet(igrid, dimCount=igridrank, rc=status)
      
      ! Get the current VM context
      call ESMF_VMGetCurrent(vm, rc)

      ! Our DE number in the layout and the total number of DEs
      call ESMF_DELayoutGetDeprecated(delayout, deCount=nDEs, localDE=my_DE, rc=status)

      ! Allocate temporary arrays
      allocate(periodic(igridrank), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating local information", &
                                       ESMF_CONTEXT, rc)) return
      allocate(decompids(igridrank), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating local information", &
                                       ESMF_CONTEXT, rc)) return
      allocate(globalCellCountPerDim(igridrank), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating local information", &
                                       ESMF_CONTEXT, rc)) return
      allocate(globalStartPerDEPerDim(nDEs, igridrank), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating local information", &
                                       ESMF_CONTEXT, rc)) return
      allocate(src_AI(nDEs, igridrank), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating local information", &
                                       ESMF_CONTEXT, rc)) return
      allocate(dst_AI(nDEs, igridrank), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating local information", &
                                       ESMF_CONTEXT, rc)) return
      allocate(gl_src_AI(nDEs, igridrank), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating local information", &
                                       ESMF_CONTEXT, rc)) return
      allocate(gl_dst_AI(nDEs, igridrank), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating local information", &
                                       ESMF_CONTEXT, rc)) return


      ! Query the datamap and set info for igrid so it knows how to
      ! match up the array indicies and the igrid indicies.
      call ESMF_FieldDataMapGet(datamap, horzRelLoc=horzRelLoc, &
                           vertRelLoc=vertRelLoc, &
                           dataIndexList=dimorder, rc=status)
       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
     
      ! Extract more information from the IGrid
      call ESMF_IGridGet(igrid, &
                        horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                        globalCellCountPerDim=globalCellCountPerDim, &
                        globalStartPerDEPerDim=globalStartPerDEPerDim, &
                        periodic=periodic, rc=status)
      ! TODO: get decompids, get igrid rank here?
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! see if you have local data
      call ESMF_IGridGetDELocalInfo(igrid, &
                        horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                        localCellCount=cellCount, &
                        rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! And get the Array sizes
      if (localFlag) then
        call ESMF_InternArrayGet(array, rank=datarank, counts=dimlengths, rc=status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      ! TODO: apply dimorder and decompids to get mapping of array to data

      ! set up things we need to find a cached route or precompute one
      if (localFlag) then
        call ESMF_IArrayGetAllAxisIndices(array, igrid, datamap, totalindex=dst_AI, &
                                       compindex=src_AI, rc=status)
      endif
      
      ! translate AI's into global numbering
      call ESMF_IGridDELocalToGlobalAI(igrid, horzRelLoc=horzRelLoc, &
                                      vertRelLoc=vertRelloc, &
                                      localAI2D=dst_AI, &
                                      globalAI2D=gl_dst_AI, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      call ESMF_IGridDELocalToGlobalAI(igrid, horzRelLoc=horzRelLoc, &
                                      vertRelLoc=vertRelloc, &
                                      localAI2D=src_AI, &
                                      globalAI2D=gl_src_AI, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Create the route object.
      route = ESMF_RouteCreate(vm, rc)

      if (cellCount .gt. 0)  then
          call ESMF_RoutePrecomputeHalo(route, datarank, my_DE, gl_src_AI, &
                                        gl_dst_AI, nDEs, &
                                        globalStartPerDEPerDim, &
                                        globalCellCountPerDim, delayout, &
                                        periodic, status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) goto 10
      endif

      ! Set the route options if given.
      if (present(routeOptions)) then
          call c_ESMC_RouteSet(route, routeOptions, status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      endif

      ! and set route into routehandle object
      call ESMF_RouteHandleSet(routehandle, which_route=index, route=route, &
                               rc=status)

      ! clean up
10  continue
      ! get rid of temporary arrays
      status = 0
      if (allocated(periodic))    deallocate(periodic, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Deallocating local information", &
                                       ESMF_CONTEXT, rc)) return
      if (allocated(decompids))   deallocate(decompids, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Deallocating local information", &
                                       ESMF_CONTEXT, rc)) return
      if (allocated(globalCellCountPerDim)) &
                                  deallocate(globalCellCountPerDim, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Deallocating local information", &
                                       ESMF_CONTEXT, rc)) return
      if (allocated(globalStartPerDEPerDim)) &
                                 deallocate(globalStartPerDEPerDim, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Deallocating local information", &
                                       ESMF_CONTEXT, rc)) return
      if (associated(    src_AI)) deallocate(src_AI, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Deallocating local information", &
                                       ESMF_CONTEXT, rc)) return
      if (associated(    dst_AI)) deallocate(dst_AI, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Deallocating local information", &
                                       ESMF_CONTEXT, rc)) return
      if (associated( gl_src_AI)) deallocate(gl_src_AI, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Deallocating local information", &
                                       ESMF_CONTEXT, rc)) return
      if (associated( gl_dst_AI)) deallocate(gl_dst_AI, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Deallocating local information", &
                                       ESMF_CONTEXT, rc)) return

      ! set return code if user specified it
      if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IArrayHaloStoreIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayHaloValidateList"
!BOPI
! !IROUTINE: ESMF_IArrayHaloValidate - Validate a list of Arrays
!
! !INTERFACE:
    ! Private name; call using ESMF_IArrayHaloValidate()
  subroutine ESMF_IArrayHaloValidateList(arrayList, routehandle, routeIndex, &
                                          rc)
!
! !ARGUMENTS:
    type(ESMF_InternArray), intent(inout) :: arrayList(:)
    type(ESMF_RouteHandle), intent(in) :: routehandle
    integer, intent(in), optional :: routeIndex
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Do extensive error checking on the incoming 
!     {\tt ESMF\_Array} and the precomputed {\tt ESMF\_RouteHandle}
!     which was constructed to perform the communication necessary
!     to execute the halo operation.   If the inputs are not compatible
!     with each other, for example if the handle was precomputed based
!     on a different size {\tt ESMF\_Array}, an error message will be
!     logged and an error returned from this routine.
!
!   \begin{description}
!   \item [arrayList]
!         List of {\tt ESMF\_Array}s containing data to be haloed.
!   \item [routehandle]
!         {\tt ESMF\_RouteHandle} which was returned from an
!         {\tt ESMF\_ArrayHaloValidatePrecompute()} call.
!   \item [{[routeIndex]}]
!         If specified, select which of possibly multiple routes to execute
!         from this route handle.  Default value is 1.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      integer :: status         ! local error status
      integer :: i, j, nitems
      type(ESMF_Route) :: route
      integer :: datarank
      integer :: counts(ESMF_MAXDIM)
      integer, allocatable :: srctotal(:), dsttotal(:)

      ! initialize return code; assume failure until success is certain
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
    ! Check init status of arguments
    do i=1, size(arrayList)
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_InternArrayGetInit, arrayList(i), rc)
    enddo
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

      if (present(routeIndex)) then
          call ESMF_RouteHandleGet(routehandle, which_route=routeIndex, &
                                        route=route, rc=status)
      else
          call ESMF_RouteHandleGet(routehandle, which_route=1, &
                                        route=route, rc=status)
      endif
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      nitems = size(arrayList)
      allocate(srctotal(nitems), dsttotal(nitems), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, &
                                    "Allocating count information", &
                                     ESMF_CONTEXT, rc)) return


      do j=1, nitems
        call ESMF_InternArrayGet(arrayList(j), counts=counts, &
                          rank=datarank, rc=status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        srctotal(j) = 1
        do i=1, datarank
           srctotal(j) = srctotal(j) * counts(i)
        enddo

      enddo

      dsttotal(:) = srctotal(:)
         
      call ESMF_RouteValidate(route, nitems, srctotal, &
                                     nitems, dsttotal, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      deallocate(srctotal, dsttotal, stat=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IArrayHaloValidateList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayHaloValidateOne"
!BOPI
! !IROUTINE: ESMF_IArrayHaloValidate - Validate an Array
!
! !INTERFACE:
    ! Private name; call using ESMF_IArrayHaloValidate()
  subroutine ESMF_IArrayHaloValidateOne(array, routehandle, routeIndex, rc)
!
! !ARGUMENTS:
    type(ESMF_InternArray), intent(inout) :: array
    type(ESMF_RouteHandle), intent(in) :: routehandle
    integer, intent(in), optional :: routeIndex
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Do extensive error checking on the incoming 
!     {\tt ESMF\_Array} and the precomputed {\tt ESMF\_RouteHandle}
!     which was constructed to perform the communication necessary
!     to execute the halo operation.   If the inputs are not compatible
!     with each other, for example if the handle was precomputed based
!     on a different size {\tt ESMF\_Field}, an error message will be
!     logged and an error returned from this routine.
!
!   \begin{description}
!   \item [array]
!         {\tt ESMF\_Array} containing data to be haloed.
!   \item [routehandle]
!         {\tt ESMF\_RouteHandle} which was returned from an
!         {\tt ESMF\_ArrayHaloValidatePrecompute()} call.
!   \item [{[routeIndex]}]
!         If specified, select which of possibly multiple routes to execute
!         from this route handle.  Default value is 1.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      integer :: status         ! local error status
      type(ESMF_Route) :: route
      integer :: i, dummy, dummy2, datarank
      integer :: counts(ESMF_MAXDIM), totalcount(1), totalcount2(1)

      ! initialize return code; assume failure until success is certain
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

      if (present(routeIndex)) then
          call ESMF_RouteHandleGet(routehandle, which_route=routeIndex, &
                                   route=route, rc=status)
      else
          call ESMF_RouteHandleGet(routehandle, which_route=1, &
                                   route=route, rc=status)
      endif
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      
      call ESMF_InternArrayGet(array, counts=counts, rank=datarank, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      dummy = 1
      totalcount = 1
      do i=1, datarank
         totalcount = totalcount * counts(i)
      enddo

      dummy2 = 1
      totalcount2 = totalcount
       
      call ESMF_RouteValidate(route, dummy, totalcount, &
                                     dummy2, totalcount2, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
     

      if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IArrayHaloValidateOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRedistList"
!BOPI
! !IROUTINE: ESMF_IArrayRedist - Redistribute a list of Arrays
!
! !INTERFACE:
      ! Private name; call using ESMF_IArrayRedist()
  subroutine ESMF_IArrayRedistList(srcArrayList, srcLocalFlag, dstArrayList, &
    dstLocalFlag, routehandle, routeIndex, blocking, routeOptions, rc) 
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(inout) :: srcArrayList(:)
      logical, intent(in) :: srcLocalFlag
      type(ESMF_InternArray), intent(inout) :: dstArrayList(:)
      logical, intent(in) :: dstLocalFlag
      type(ESMF_RouteHandle), intent(in) :: routehandle
      integer, intent(in), optional :: routeIndex
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Redistribute the data in one set of {\tt ESMF\_Array}s to another
!  set of {\tt ESMF\_Array}s.  Data redistribution does no interpolation,
!  so during the {\tt ESMF\_ArrayRedistPrecompute()} call the 
!  {\tt ESMF\_IGrid}s must have identical coordinates.  
!  The distribution of the {\tt ESMF\_IGrid} can be over different
!  {\tt ESMF\_DELayout}s, or the {\tt ESMF\_FieldDataMaps} can differ.
!  The {\tt routehandle} argument must be the one which was associated
!  with the precomputed data movements during the precompute operation, and
!  if the data movement is identical for different collections of
!  {\tt ESMF\_Array}s, the same {\tt routehandle} can be supplied during
!  multiple calls to this execution routine, specifying a different set of
!  source and destination {\tt ESMF\_Array}s each time.
!
!     \begin{description}
!     \item [srcArrayList]
!           List of {\tt ESMF\_Array}s containing source data.
!     \item [dstArrayList]
!           List of {\tt ESMF\_Array}s containing results.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} precomputed by 
!           {\tt ESMF\_ArrayRedistPrecompute()}.
!   \item [{[routeIndex]}]
!         If specified, select which of possibly multiple routes to execute
!         from this route handle.  Default value is 1.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status         ! local error status
      integer :: i, nitemsSrc, nitemsDst
      type(ESMF_LocalArray), allocatable :: srcLocalArrayList(:)
      type(ESMF_LocalArray), allocatable :: dstLocalArrayList(:)
      type(ESMF_Route) :: route

      ! initialize return code; assume failure until success certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    if (srcLocalFlag) then
      do i=1, size(srcArrayList)
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_InternArrayGetInit, srcArrayList(i), rc)
      enddo
    endif
    if (dstLocalFlag) then
      do i=1, size(dstArrayList)
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_InternArrayGetInit, dstArrayList(i), rc)
      enddo
    endif
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)

      if (present(routeIndex)) then
          call ESMF_RouteHandleGet(routehandle, which_route=routeIndex, &
                                        route=route, rc=status)
      else
          call ESMF_RouteHandleGet(routehandle, which_route=1, &
                                        route=route, rc=status)
      endif
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! figure out the length of the input args and allocate enough space
      ! for localarray copies; if fortran could do better inheritance this
      ! would not be necessary.  (routerun wants a localarray object;
      ! we can pass in a pointer to an array because routerun will only
      ! access the localarray part of the array object.)
      nitemsSrc = size(srcArrayList)
      allocate(srcLocalArrayList(nitemsSrc), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating local information", &
                                       ESMF_CONTEXT, rc)) return

      nitemsDst = size(dstArrayList)
      allocate(dstLocalArrayList(nitemsDst), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating local information", &
                                       ESMF_CONTEXT, rc)) return

      if (srcLocalFlag) then
        do i=1, nitemsSrc
          srcLocalArrayList(i)%this%ptr = srcArrayList(i)%this%ptr
          ESMF_INIT_COPY(srcLocalArrayList(i),srcArrayList(i))
        enddo
      endif

      if (dstLocalFlag) then
        do i=1, nitemsDst
          dstLocalArrayList(i)%this%ptr = dstArrayList(i)%this%ptr
          ESMF_INIT_COPY(dstLocalArrayList(i),dstArrayList(i))
        enddo
      endif

      ! Set the route options if given.
      if (present(routeOptions)) then
          call c_ESMC_RouteSet(route, routeOptions, status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      endif


      ! Execute the communications call.
      call ESMF_RouteRunList(route, srcLocalArrayList, dstLocalArrayList, status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) goto 10

      ! come here on error to free the temp buffers
 10   continue

      deallocate(srcLocalArrayList, stat=status)
      deallocate(dstLocalArrayList, stat=status)

      if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IArrayRedistList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRedistOne"
!BOPI
! !IROUTINE: ESMF_IArrayRedist - Redistribute an Array
!
! !INTERFACE:
      ! Private name; call using ESMF_IArrayRedist()
  subroutine ESMF_IArrayRedistOne(srcArray, srcLocalFlag, dstArray, &
    dstLocalFlag, routehandle, routeIndex, blocking, routeOptions, rc) 
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(in) :: srcArray
      logical, intent(in) :: srcLocalFlag
      type(ESMF_InternArray), intent(in) :: dstArray
      logical, intent(in) :: dstLocalFlag
      type(ESMF_RouteHandle), intent(in) :: routehandle
      integer, intent(in), optional :: routeIndex
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Redistribute the data in one set of {\tt ESMF\_Array}s to another
!  set of {\tt ESMF\_Array}s.  Data redistribution does no interpolation,
!  so during the {\tt ESMF\_ArrayRedistPrecompute()} call the 
!  {\tt ESMF\_IGrid}s must have identical coordinates.  
!  The distribution of the {\tt ESMF\_IGrid} can be over different
!  {\tt ESMF\_DELayout}s, or the {\tt ESMF\_FieldDataMaps} can differ.
!  The {\tt routehandle} argument must be the one which was associated
!  with the precomputed data movements during the precompute operation, and
!  if the data movement is identical for different collections of
!  {\tt ESMF\_Array}s, the same {\tt routehandle} can be supplied during
!  multiple calls to this execution routine, specifying a different set of
!  source and destination {\tt ESMF\_Array}s each time.
!
!     \begin{description}
!     \item [srcArray]
!           {\tt ESMF\_Array} containing source data.
!     \item [dstArray]
!           {\tt ESMF\_Array} containing results.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} precomputed by 
!           {\tt ESMF\_ArrayRedistPrecompute()}.
!   \item [{[routeIndex]}]
!         If specified, select which of possibly multiple routes to execute
!         from this route handle.  Default value is 1.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status         ! local error status
      type(ESMF_LocalArray) :: dstLocalArray, srcLocalArray
      type(ESMF_Route) :: route

      ! initialize return code; assume failure until success certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      
    ! Before going further down into this code, make sure
    ! that this DE has at least src or dst data.   If neither, return now.
    if ((.not.srcLocalFlag) .and. (.not.dstLocalFlag)) then
        if (present(rc)) rc = ESMF_SUCCESS
        return
    endif

    ! Check init status of arguments
    if (srcLocalFlag) then
      ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, srcArray, rc)
    endif
    if (dstLocalFlag) then
      ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, dstArray, rc)
    endif
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit, routehandle, rc)

      if (present(routeIndex)) then
          call ESMF_RouteHandleGet(routehandle, which_route=routeIndex, &
                                        route=route, rc=status)
      else
          call ESMF_RouteHandleGet(routehandle, which_route=1, &
                                        route=route, rc=status)
      endif
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set the route options if given.
      if (present(routeOptions)) then
          call c_ESMC_RouteSet(route, routeOptions, status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      endif



      ! Convert from Intern to Local Arrays
!      if (srcLocalFlag) then
        srcLocalArray%this%ptr = srcArray%this%ptr
        ESMF_INIT_COPY(srcLocalArray,srcArray)
!      endif
!      if (dstLocalFlag) then
        dstLocalArray%this%ptr = dstArray%this%ptr
        ESMF_INIT_COPY(dstLocalArray,dstArray)
!      endif

      ! Execute the communications call
      call ESMF_RouteRun(route, srcLocalArray, dstLocalArray, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IArrayRedistOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRedistRelease"
!BOPI
! !IROUTINE: ESMF_IArrayRedistRelease - Release resources stored for redist operation
!
! !INTERFACE:
  subroutine ESMF_IArrayRedistRelease(routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     When the precomputed information about a 
!     redistribution operation is no longer
!     needed, this routine releases the associated resources.
!
!     \begin{description}
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} associated with redist operation which
!           should be released.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
     integer :: localrc                        ! local return code
     ! initialize return code; assume routine not implemented
     if (present(rc)) rc = ESMF_RC_NOT_IMPL
     localrc = ESMF_RC_NOT_IMPL

     ! Check init status of arguments
     ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

     call ESMF_RouteHandleDestroy(routehandle, rc=localrc)
     if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
     ESMF_CONTEXT, rcToReturn=rc)) return

     if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IArrayRedistRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRedistStoreOne"
!BOPI
! !IROUTINE: ESMF_IArrayRedistStore - Store resources for a redist operation
!
! !INTERFACE:
      ! Private name; call using ESMF_IArrayRedistStore()
  subroutine ESMF_IArrayRedistStoreOne(srcArray, srcLocalFlag, srcIGrid, &
    srcDataMap, dstArray, dstLocalFlag, dstIGrid, dstDataMap, parentVM, &
    routeOptions, routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(in) :: srcArray
      logical, intent(in) :: srcLocalFlag
      type(ESMF_IGrid), intent(inout) :: srcIGrid
      type(ESMF_FieldDataMap), intent(inout) :: srcDataMap
      type(ESMF_InternArray), intent(in) :: dstArray
      logical, intent(in) :: dstLocalFlag
      type(ESMF_IGrid), intent(inout) :: dstIGrid
      type(ESMF_FieldDataMap), intent(inout) :: dstDataMap
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      type(ESMF_RouteHandle), intent(out) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Precompute and associate the required data movements to redistribute
!  data over one set of {\tt ESMF\_Array}s to another
!  set of {\tt ESMF\_Array}s.  Data redistribution does no interpolation,
!  so both {\tt ESMF\_IGrid}s must have identical coordinates.
!  The distribution of the {\tt ESMF\_IGrid}s can be over different
!  {\tt ESMF\_DELayout}s, or the {\tt ESMF\_FieldDataMap}s can differ.
!  The {\tt routehandle} argument is associated with the stored information
!  and must be supplied to {\tt ESMF\_ArrayRedist()} to execute the
!  operation.  Call {\tt ESMF\_ArrayRedistRelease()} when this information
!  is no longer required.
!
!  The arguments are:
!   \begin{description}
!   \item[srcArray]
!    {\tt ESMF\_Array} containing the data source.
!   \item[srcIGrid]
!    {\tt ESMF\_IGrid} describing the igrid on which the source data is arranged.
!   \item[srcDataMap]
!    {\tt ESMF\_FieldDataMap} describing how the source data maps onto the igrid.
!   \item[dstArray]
!    {\tt ESMF\_Array} where the destination data will be put.
!   \item[dstIGrid]
!    {\tt ESMF\_IGrid} describing the igrid on which the destination data is 
!    arranged.
!   \item[dstDataMap]
!    {\tt ESMF\_FieldDataMap} describing how the destination data maps 
!    onto the igrid.
!   \item[parentVM]
!    {\tt ESMF\_VM} object which includes all PETs in both the
!    source and destination igrids.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the
!           See Section~\ref{opt:routeopt} for possible values.
!   \item [routehandle]
!    Returned {\tt ESMF\_RouteHandle} which identifies this 
!    communication pattern.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
    integer :: localrc                        ! local return code

    ! if problems compiling, see the comment in HaloStore() for
    ! suggestions regarding intent(out) vs intent(inout).

     ! initialize return code; assume routine not implemented
     if (present(rc)) rc = ESMF_RC_NOT_IMPL
     localrc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    if (srcLocalFlag) then
      ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, srcArray, rc)
    endif
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, srcIGrid, rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, srcDatamap)
    if (dstLocalFlag) then
      ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, dstArray, rc)
    endif
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, dstIGrid, rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, dstDatamap)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, parentVM, rc)

    call ESMF_IArrayRedistStoreIndex(srcArray, srcLocalFlag, srcIGrid, &
      srcDataMap, dstArray, dstLocalFlag, dstIGrid, dstDataMap, &
      1, ESMF_1TO1HANDLEMAP, 1, parentVM, routehandle, routeOptions, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IArrayRedistStoreOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRedistStoreIndex"
!BOPI
! !IROUTINE: ESMF_IArrayRedistStoreIndex - Store resources for a redist operation
!
! !INTERFACE:
      ! internal use only; called by FieldBundle code for multi-fields
  subroutine ESMF_IArrayRedistStoreIndex(srcArray, srcLocalFlag, srcIGrid, &
    srcDataMap, dstArray, dstLocalFlag, dstIGrid, dstDataMap, index, &
    rmaptype, maxindex, parentVM, routehandle, routeOptions, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(in) :: srcArray
      logical, intent(in) :: srcLocalFlag
      type(ESMF_IGrid), intent(inout) :: srcIGrid
      type(ESMF_FieldDataMap), intent(inout) :: srcDataMap
      type(ESMF_InternArray), intent(in) :: dstArray
      logical, intent(in) :: dstLocalFlag
      type(ESMF_IGrid), intent(inout) :: dstIGrid
      type(ESMF_FieldDataMap), intent(inout) :: dstDataMap
      integer, intent(in) :: index
      integer, intent(in) :: rmaptype
      integer, intent(in) :: maxindex
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_RouteHandle), intent(inout) :: routehandle 
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Precompute and associate the required data movements to redistribute
!  data over one set of {\tt ESMF\_Array}s to another
!  set of {\tt ESMF\_Array}s.  Data redistribution does no interpolation,
!  so both {\tt ESMF\_IGrid}s must have identical coordinates.
!  The distribution of the {\tt ESMF\_IGrid}s can be over different
!  {\tt ESMF\_DELayout}s, or the {\tt ESMF\_FieldDataMap}s can differ.
!  The {\tt routehandle} argument is associated with the stored information
!  and must be supplied to {\tt ESMF\_ArrayRedist()} to execute the
!  operation.  Call {\tt ESMF\_ArrayRedistRelease()} when this information
!  is no longer required.
!
!  The arguments are:
!   \begin{description}
!   \item[srcArray]
!    {\tt ESMF\_Array} containing the data source.
!   \item[srcIGrid]
!    {\tt ESMF\_IGrid} describing the igrid on which the source data is arranged.
!   \item[srcDataMap]
!    {\tt ESMF\_FieldDataMap} describing how the source data maps onto the igrid.
!   \item[dstArray]
!    {\tt ESMF\_Array} where the destination data will be put.
!   \item[dstIGrid]
!    {\tt ESMF\_IGrid} describing the igrid on which the destination data is 
!    arranged.
!   \item[dstDataMap]
!    {\tt ESMF\_FieldDataMap} describing how the destination data maps 
!    onto the igrid.
!     \item [index]
!           Integer specifying which route number this is to be inside
!           the {\tt routehandle}.  Route handles for bundles can contain
!           either a single route which applies to all fields in the bundle,
!           or multiple routes which correspond to the differences in
!           data motion for different array sizes, ranks, relative locations,
!           data types, etc.  Since this is Fortran, index 1 is the first
!           route number.
!     \item [rmaptype]
!           Integer parameter value used if the index number is 1.
!           Controls whether the route handle will contain a separate route
!           for each index, or if all indices map to the same route.
!     \item [maxindex]
!           If {\tt index} is 1, set the maximum number of routes which
!           will eventually be set.  If not known, use 1.  The list of
!           routes can be reallocated and expanded later.
!   \item[parentVM]
!    {\tt ESMF\_VM} object which includes all PETs in both the
!    source and destination igrids.
!   \item [routehandle]
!    Returned {\tt ESMF\_RouteHandle} which identifies this 
!    communication pattern.
!   \item [{[routeOptions]}]
!    Not normally specified.  Specify which internal strategy to select
!    when executing the communication needed to execute the
!    See Section~\ref{opt:routeopt} for possible values.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      integer :: status         ! local error status
      integer :: dstDEs, srcDEs, myDstDE, MySrcDE
      integer :: srcCount, dstCount
      integer :: igridrank, datarank
      integer, dimension(ESMF_MAXDIM) :: dstDimOrder, srcDimOrder, dimlengths
      integer, dimension(:), pointer :: dstAICountPerDE, srcAICountPerDE
      type(ESMF_AxisIndex), dimension(:), pointer :: &
		mySrcGlobalTotalAIperRank, myDstGlobalTotalAIperRank
      type(ESMF_AxisIndex), dimension(:,:), pointer :: &
		srcGlobalCompAIperDEperRank, dstGlobalCompAIperDEperRank
      type(ESMF_DELayout) :: dstDElayout, srcDElayout
      type(ESMF_IGridStorage) :: dstStorage, srcStorage
      type(ESMF_Logical) :: hasSrcData, hasDstData
      type(ESMF_RelLoc) :: dstHorzRelLoc, srcHorzRelLoc, &
                           dstVertRelLoc, srcVertRelLoc
      type(ESMF_Route) :: route

      ! initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    if (srcLocalFlag) then
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_InternArrayGetInit, srcArray, rc)
    endif
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, srcIGrid, rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, srcDatamap)
    if (dstLocalFlag) then
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_InternArrayGetInit, dstArray, rc)
    endif
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, dstIGrid, rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, dstDatamap)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, parentVM, rc)

      ! start with a very clean slate...
      nullify(dstAICountPerDE)
      nullify(srcAICountPerDE)
      nullify(mySrcGlobalTotalAIperRank)
      nullify(myDstGlobalTotalAIperRank)
      nullify(srcGlobalCompAIperDEperRank)
      nullify(dstGlobalCompAIperDEperRank)

      ! create the routehandle if this is the first route.  otherwise
      ! we are adding a route to an existing handle and we do not want
      ! to overwrite it.
      if (index .eq. 1) then
          routehandle = ESMF_RouteHandleCreate(status)
          ! set the type and mapping
          call ESMF_RouteHandleSet(routehandle, htype=ESMF_REDISTHANDLE, &
                                   route_count=maxindex, rmaptype=rmaptype, &
                                   tv_count=0, tvmaptype=ESMF_NOHANDLEMAP, &
                                   rc=status)
      else
          ! if adding to an existing handle make sure it exists
          ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
      endif
    

      ! Query the datamap and set info for igrid so it knows how to
      ! match up the array indices and the igrid indices.
      call ESMF_FieldDataMapGet(dstDataMap, horzRelLoc=dstHorzRelLoc, &
                                vertRelLoc=dstVertRelLoc, &
                                dataIndexList=dstDimOrder, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_FieldDataMapGet(srcDataMap, horzRelLoc=srcHorzRelLoc, &
                                vertRelLoc=srcVertRelLoc, &
                                dataIndexList=srcDimOrder, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! TODO: move storage to the IGridGet that does not require RelLocs

      ! Extract information from the IGrids
      call ESMF_IGridGet(dstIGrid, delayout=dstDElayout, &
                        igridStorage=dstStorage, &
                        horzRelLoc=dstHorzRelLoc, vertRelLoc=dstVertRelLoc, &
                        rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_IGridGetDELocalInfo(dstIGrid, &
                        horzRelLoc=dstHorzRelLoc, vertRelLoc=dstVertRelLoc, &
                        localCellCount=dstCount, &
                        rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_IGridGet(srcIGrid, delayout=srcDElayout, &
                        igridStorage=srcStorage, dimCount=igridrank, &
                        horzRelLoc=srcHorzRelLoc, vertRelLoc=srcVertRelLoc, &
                        rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_IGridGetDELocalInfo(srcIGrid, &
                        horzRelLoc=srcHorzRelLoc, vertRelLoc=srcVertRelLoc, &
                        localCellCount=srcCount, &
                        rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (dstCount .gt. 0) then
          hasDstData = ESMF_TRUE
      else
          hasDstData = ESMF_FALSE
      endif
      if (srcCount .gt. 0) then
          hasSrcData = ESMF_TRUE
      else
          hasSrcData = ESMF_FALSE
      endif

      ! TODO: if I don't have any data then return here 

      ! for now, branch out if arbitrary storage
      if (dstStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY .OR. &
          srcStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) then
        call ESMF_IArrayRedistStoreIndexArb(srcArray, srcIGrid, srcDataMap, &
                                          dstArray, dstIGrid, dstDataMap, &
                                          index, rmaptype, maxindex, &
                                          parentVM, routehandle, &
                                          routeOptions, rc)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        return
      endif

      ! Our DE number in the layout and the total number of DEs
      call ESMF_DELayoutGetDeprecated(dstDElayout, deCount=dstDEs,  &
                            localDE=myDstDE, rc=status)
      call ESMF_DELayoutGetDeprecated(srcDElayout, deCount=srcDEs, &
                            localDE=mySrcDE, rc=status)

      ! And get the Array sizes
      call ESMF_InternArrayGet(srcArray, rank=datarank, counts=dimlengths, &
        rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! TODO: error check/validation code needs to be added here.
      ! these things must be true (or equal):
      !  igrid ranks equal
      !  data ranks equal
      !  computational global igrid sizes equal
      !  (halo sizes should *not* need to match anymore)
      !  non-igrid axes lengths must match
      !  (should we call validate on the objects?)
      !

      ! Allocate temporary arrays
      allocate(srcGlobalCompAIperDEperRank(srcDEs, datarank), &
               dstGlobalCompAIperDEperRank(dstDEs, datarank), stat=status)
      allocate(mySrcGlobalTotalAIperRank(datarank), &
               myDstGlobalTotalAIperRank(datarank), stat=status)


      ! get more igrid information
      !call ESMF_IGridGet(dstIGrid, &
      !                  horzRelLoc=dstHorzRelLoc, vertRelLoc=dstVertRelLoc, &
      !                  globalCellCountPerDim=dstCellCountPerDim, &
      !                  globalStartPerDEPerDim=dstStartPerDEPerDim, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      !call ESMF_IGridGet(srcIGrid, &
      !                  horzRelLoc=srcHorzRelLoc, vertRelLoc=srcVertRelLoc, &
      !                  globalCellCountPerDim=srcCellCountPerDim, &
      !                  globalStartPerDEPerDim=srcStartPerDEPerDim, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! get all of the AIs -- that mean for all of the DEs
      call ESMF_IArrayGetAIsAllDEs(dstArray, dstIGrid, dstDataMap, &
                                  ESMF_GLOBAL, ESMF_DOMAIN_COMPUTATIONAL, &
                                  dstGlobalCompAIperDEperRank, &
                                  rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_IArrayGetAIsAllDEs(srcArray, srcIGrid, srcDataMap, &
                                  ESMF_GLOBAL, ESMF_DOMAIN_COMPUTATIONAL, &
                                  srcGlobalCompAIperDEperRank, &
                                  rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! TODO: make GlobalTotalAI arrays
      call ESMF_IArrayGetGlobalAIs(dstArray, dstIGrid, dstDataMap, &
                                  ESMF_DOMAIN_TOTAL, &
                                  myDstGlobalTotalAIPerRank, rc=status)
      call ESMF_IArrayGetGlobalAIs(srcArray, srcIGrid, srcDataMap, &
                                  ESMF_DOMAIN_TOTAL, &
                                  mySrcGlobalTotalAIPerRank, rc=status)

      ! Create the route object.
      route = ESMF_RouteCreate(parentVM, rc)

      call ESMF_RoutePrecomputeRedist(route, datarank, &
                                      hasSrcData, srcDELayout, mySrcDE, &
                                      srcGlobalCompAIperDEperRank, &
                                      mySrcGlobalTotalAIperRank, &
                                      hasDstData, dstDELayout, myDstDE, &
                                      dstGlobalCompAIperDEperRank, &
                                      myDstGlobalTotalAIperRank, status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set the route options if given.
      if (present(routeOptions)) then
          call c_ESMC_RouteSet(route, routeOptions, status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      endif

      ! and set route into routehandle object
      call ESMF_RouteHandleSet(routehandle,  which_route=index, route=route, &
                               rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! get rid of temporary arrays
      if (associated(mySrcGlobalTotalAIperRank)) &
                          deallocate(mySrcGlobalTotalAIperRank, stat=status)
      if (associated(myDstGlobalTotalAIperRank)) &
                          deallocate(myDstGlobalTotalAIperRank, stat=status)
      if (associated(srcGlobalCompAIperDEperRank)) &
                          deallocate(srcGlobalCompAIperDEperRank, stat=status)
      if (associated(dstGlobalCompAIperDEperRank)) &
                          deallocate(dstGlobalCompAIperDEperRank, stat=status)

      if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_IArrayRedistStoreIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRedistStoreIndexArb"
!BOPI
! !IROUTINE: ESMF_IArrayRedistStoreIndexArb - Store resources for a redist operation
!
! !INTERFACE:
      ! internal use only; called by FieldBundle code for multi-fields
  subroutine ESMF_IArrayRedistStoreIndexArb(srcArray, srcIGrid, srcDataMap, &
                                       dstArray, dstIGrid, dstDataMap, &
                                       index, rmaptype, maxindex, &
                                       parentVM, routehandle, routeOptions, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(in) :: srcArray
      type(ESMF_IGrid), intent(inout) :: srcIGrid
      type(ESMF_FieldDataMap), intent(inout) :: srcDataMap
      type(ESMF_InternArray), intent(in) :: dstArray
      type(ESMF_IGrid), intent(inout) :: dstIGrid
      type(ESMF_FieldDataMap), intent(inout) :: dstDataMap
      integer, intent(in) :: index
      integer, intent(in) :: rmaptype
      integer, intent(in) :: maxindex
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Precompute and associate the required data movements to redistribute
!  data over one set of {\tt ESMF\_Array}s to another
!  set of {\tt ESMF\_Array}s.  Data redistribution does no interpolation,
!  so both {\tt ESMF\_IGrid}s must have identical coordinates.
!  The distribution of the {\tt ESMF\_IGrid}s can be over different
!  {\tt ESMF\_DELayout}s, or the {\tt ESMF\_FieldDataMap}s can differ.
!  The {\tt routehandle} argument is associated with the stored information
!  and must be supplied to {\tt ESMF\_ArrayRedist()} to execute the
!  operation.  Call {\tt ESMF\_ArrayRedistRelease()} when this information
!  is no longer required.
!
!  The arguments are:
!   \begin{description}
!   \item[srcArray]
!    {\tt ESMF\_Array} containing the data source.
!   \item[srcIGrid]
!    {\tt ESMF\_IGrid} describing the igrid on which the source data is arranged.
!   \item[srcDataMap]
!    {\tt ESMF\_FieldDataMap} describing how the source data maps onto the igrid.
!   \item[dstArray]
!    {\tt ESMF\_Array} where the destination data will be put.
!   \item[dstIGrid]
!    {\tt ESMF\_IGrid} describing the igrid on which the destination data is 
!    arranged.
!   \item[dstDataMap]
!    {\tt ESMF\_FieldDataMap} describing how the destination data maps 
!    onto the igrid.
!     \item [index]
!           Integer specifying which route number this is to be inside
!           the {\tt routehandle}.  Route handles for bundles can contain
!           either a single route which applies to all fields in the bundle,
!           or multiple routes which correspond to the differences in
!           data motion for different array sizes, ranks, relative locations,
!           data types, etc.  Since this is Fortran, index 1 is the first
!           route number.
!     \item [rmaptype]
!           Integer parameter value used if the index number is 1.
!           Controls whether the route handle will contain a separate route
!           for each index, or if all indices map to the same route.
!     \item [maxindex]
!           If {\tt index} is 1, set the maximum number of routes which
!           will eventually be set.  If not known, use 1.  The list of
!           routes can be reallocated and expanded later.
!   \item[parentVM]
!    {\tt ESMF\_VM} object which includes all PETs in both the
!    source and destination igrids.
!   \item [routehandle]
!    Returned {\tt ESMF\_RouteHandle} which identifies this 
!    communication pattern.
!   \item [{[routeOptions]}]
!    Not normally specified.  Specify which internal strategy to select
!    when executing the communication needed to execute the
!    See Section~\ref{opt:routeopt} for possible values.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      integer :: status         ! local error status
      integer :: dstAICount, srcAICount
      integer :: dstDEs, srcDEs, dstMyDE, srcMyDE
      integer :: srcCount, dstCount
      integer :: igridrank, datarank
      integer, dimension(ESMF_MAXDIM) :: dstDimOrder, srcDimOrder, dimlengths
      integer, dimension(:), allocatable :: dstCellCountPerDim, decompids, &
                                            srcCellCountPerDim
      integer, dimension(:), pointer :: dstAICountPerDE, srcAICountPerDE
      integer, dimension(:,:), allocatable :: dstStartPerDEPerDim, &
                                              srcStartPerDEPerDim
      logical :: dstVector, srcVector
      type(ESMF_AxisIndex), dimension(:,:), pointer :: dstCompAI, srcCompAI, &
                                                     dstTotalAI, srcTotalAI, &
                                                   dstCLocalAI, srcCLocalAI, &
                                                   dstTLocalAI, srcTLocalAI
      type(ESMF_DELayout) :: dstDElayout, srcDElayout
      type(ESMF_IGridStorage) :: dstStorage, srcStorage
      type(ESMF_Logical), dimension(:), allocatable :: periodic
      type(ESMF_Logical) :: hasSrcData, hasDstData
      type(ESMF_RelLoc) :: dstHorzRelLoc, srcHorzRelLoc, &
                           dstVertRelLoc, srcVertRelLoc
      type(ESMF_Route) :: route

      ! initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_InternArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, srcIGrid, rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, srcDatamap)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_InternArrayGetInit, dstArray, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit, dstIGrid, rc)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit, ESMF_FieldDataMapInit, dstDatamap)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, parentVM, rc)

      ! TODO: query to get storage type and if vector, branch out.

      ! start with a very clean slate...
      nullify(dstAICountPerDE)
      nullify(srcAICountPerDE)
      nullify(dstCompAI)
      nullify(srcCompAI)
      nullify(dstTotalAI)
      nullify(srcTotalAI)
      nullify(dstCLocalAI)
      nullify(srcCLocalAI)
      nullify(dstTLocalAI)
      nullify(srcTLocalAI)

      ! create the routehandle if this is the first route.  otherwise
      ! we are adding a route to an existing handle and we do not want
      ! to overwrite it.
      if (index .eq. 1) then
          routehandle = ESMF_RouteHandleCreate(status)
          ! set the type and mapping
          call ESMF_RouteHandleSet(routehandle, htype=ESMF_REDISTHANDLE, &
                                   route_count=maxindex, rmaptype=rmaptype, &
                                   tv_count=0, tvmaptype=ESMF_NOHANDLEMAP, &
                                   rc=status)
      else
        ! if adding to an existing handle make sure it exists
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)
      endif
    

      ! Query the datamap and set info for igrid so it knows how to
      ! match up the array indices and the igrid indices.
      call ESMF_FieldDataMapGet(dstDataMap, horzRelLoc=dstHorzRelLoc, &
                                vertRelLoc=dstVertRelLoc, &
                                dataIndexList=dstDimOrder, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_FieldDataMapGet(srcDataMap, horzRelLoc=srcHorzRelLoc, &
                                vertRelLoc=srcVertRelLoc, &
                                dataIndexList=srcDimOrder, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Extract information from the IGrids
      call ESMF_IGridGet(dstIGrid, delayout=dstDElayout, &
                        igridStorage=dstStorage, &
                        horzRelLoc=dstHorzRelLoc, vertRelLoc=dstVertRelLoc, &
                        rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_IGridGetDELocalInfo(dstIGrid, &
                        horzRelLoc=dstHorzRelLoc, vertRelLoc=dstVertRelLoc, &
                        localCellCount=dstCount, &
                        rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_IGridGet(srcIGrid, delayout=srcDElayout, &
                        igridStorage=srcStorage, dimCount=igridrank, &
                        horzRelLoc=srcHorzRelLoc, vertRelLoc=srcVertRelLoc, &
                        rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_IGridGetDELocalInfo(srcIGrid, &
                        horzRelLoc=srcHorzRelLoc, vertRelLoc=srcVertRelLoc, &
                        localCellCount=srcCount, &
                        rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Our DE number in the layout and the total number of DEs
      call ESMF_DELayoutGetDeprecated(dstDElayout, deCount=dstDEs,  &
                            localDE=dstMyDE, rc=status)
      call ESMF_DELayoutGetDeprecated(srcDElayout, deCount=srcDEs, &
                            localDE=srcMyDE, rc=status)

      if (dstCount .gt. 0) then
          hasDstData = ESMF_TRUE
      else
          hasDstData = ESMF_FALSE
      endif
      if (srcCount .gt. 0) then
          hasSrcData = ESMF_TRUE
      else
          hasSrcData = ESMF_FALSE
      endif
 
      ! TODO: error check/validation code needs to be added here.
      ! these things must be true (or equal):
      !  igrid ranks equal
      !  data ranks equal
      !  computational global igrid sizes equal
      !  (halo sizes should *not* need to match anymore)
      !  non-igrid axes lengths must match
      !  (should we call validate on the objects?)
      !

      ! Allocate temporary arrays
      dstVector = .false.
      srcVector = .false.
      allocate(          periodic(igridrank), &
                        decompids(igridrank), &
               dstCellCountPerDim(igridrank), &
               srcCellCountPerDim(igridrank), stat=status)
      allocate(dstStartPerDEPerDim(dstDEs, igridrank), &
               srcStartPerDEPerDim(srcDEs, igridrank), stat=status)

      if (dstStorage.eq.ESMF_IGRID_STORAGE_LOGRECT) then
        allocate(  dstCompAI(dstDEs, igridrank), &
                  dstTotalAI(dstDEs, igridrank), &
                 dstCLocalAI(dstDEs, igridrank), &
                 dstTLocalAI(dstDEs, igridrank), stat=status)
      endif
      if (srcStorage.eq.ESMF_IGRID_STORAGE_LOGRECT) then
        allocate(  srcCompAI(srcDEs, igridrank), &
                  srcTotalAI(srcDEs, igridrank), &
                 srcCLocalAI(srcDEs, igridrank), &
                 srcTLocalAI(srcDEs, igridrank), stat=status)
      endif

      ! get more igrid information
      call ESMF_IGridGet(dstIGrid, &
                        horzRelLoc=dstHorzRelLoc, vertRelLoc=dstVertRelLoc, &
                        globalCellCountPerDim=dstCellCountPerDim, &
                        globalStartPerDEPerDim=dstStartPerDEPerDim, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_IGridGet(srcIGrid, &
                        horzRelLoc=srcHorzRelLoc, vertRelLoc=srcVertRelLoc, &
                        globalCellCountPerDim=srcCellCountPerDim, &
                        globalStartPerDEPerDim=srcStartPerDEPerDim, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! allocate more local arrays for arbitrary storage
      if (dstStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) then
        dstVector  = .true.
        dstAICount = dstCellCountPerDim(1)*dstCellCountPerDim(2)
        allocate(  dstCompAI(dstAICount, igridrank), &
                  dstTotalAI(dstAICount, igridrank), stat=status)
      endif
      if (srcStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) then
        srcVector  = .true.
        srcAICount = srcCellCountPerDim(1)*srcCellCountPerDim(2)
        allocate(  srcCompAI(srcAICount, igridrank), &
                  srcTotalAI(srcAICount, igridrank), stat=status)
      endif
      if (dstVector .OR. srcVector) then
        allocate(dstAICountPerDE(dstDEs), &
                 srcAICountPerDE(srcDEs), stat=status)
        dstAICountPerDE = 1
        srcAICountPerDE = 1
      endif

      ! And get the Array sizes
      call ESMF_InternArrayGet(srcArray, rank=datarank, counts=dimlengths, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      if (srcStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) datarank = datarank + 1

      ! TODO: apply dimorder and decompids to get mapping of array to data

      ! check if arbitrary-to-arbitrary optimization is to be used
      if (dstStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY .and. &
	  srcStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) then
        call ESMF_IArrayGetAllAxisIndices(dstArray, dstIGrid, dstDataMap, &
                                         compindex=dstCompAI, &
                                         AICountPerDE=dstAICountPerDE, rc=status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        call ESMF_IArrayGetAllAxisIndices(srcArray, srcIGrid, srcDataMap, &
                                         compindex=srcCompAI, &
                                         AICountPerDE=srcAICountPerDE, rc=status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
                                  
      else ! at least one side is block distributed

        ! set up things we need to precompute a route
        if (dstStorage.eq.ESMF_IGRID_STORAGE_LOGRECT) then
          call ESMF_IArrayGetAllAxisIndices(dstArray, dstIGrid, dstDataMap, &
                                         compindex =dstCLocalAI, &
                                         totalindex=dstTLocalAI, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

          ! translate AI's into global numbering
          call ESMF_IGridDELocalToGlobalAI(dstIGrid, &
                                        horzRelLoc=dstHorzRelLoc, &
                                        vertRelLoc=dstVertRelLoc, &
                                        localAI2D=dstCLocalAI, &
                                        globalAI2D=dstCompAI, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          call ESMF_IGridDELocalToGlobalAI(dstIGrid, &
                                        horzRelLoc=dstHorzRelLoc, &
                                        vertRelLoc=dstVertRelLoc, &
                                        localAI2D=dstTLocalAI, &
                                        globalAI2D=dstTotalAI, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        elseif (dstStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) then
          call ESMF_IArrayGetAllAxisIndices(dstArray, dstIGrid, dstDataMap, &
                                         compindex=dstCompAI, &
                                         AICountPerDE=dstAICountPerDE, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          call ESMF_IArrayGetAllAxisIndices(dstArray, dstIGrid, dstDataMap, &
                                         compindex=dstTotalAI, &
                                         AICountPerDE=dstAICountPerDE, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif

        ! now the source igrid
        if (srcStorage.eq.ESMF_IGRID_STORAGE_LOGRECT) then
          call ESMF_IArrayGetAllAxisIndices(srcArray, srcIGrid, srcDataMap, &
                                         compindex =srcCLocalAI, &
                                         totalindex=srcTLocalAI, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          call ESMF_IGridDELocalToGlobalAI(srcIGrid, &
                                        horzRelLoc=srcHorzRelLoc, &
                                        vertRelLoc=srcVertRelLoc, &
                                        localAI2D=srcCLocalAI, &
                                        globalAI2D=srcCompAI, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          call ESMF_IGridDELocalToGlobalAI(srcIGrid, &
                                        horzRelLoc=srcHorzRelLoc, &
                                        vertRelLoc=srcVertRelLoc, &
                                        localAI2D=srcTLocalAI, &
                                        globalAI2D=srcTotalAI, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        elseif (srcStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) then
          call ESMF_IArrayGetAllAxisIndices(srcArray, srcIGrid, srcDataMap, &
                                         compindex=srcCompAI, &
                                         AICountPerDE=srcAICountPerDE, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          call ESMF_IArrayGetAllAxisIndices(srcArray, srcIGrid, srcDataMap, &
                                         compindex=srcTotalAI, &
                                         AICountPerDE=srcAICountPerDE, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        endif
      endif

      ! Create the route object.
      route = ESMF_RouteCreate(parentVM, rc)

      if (dstStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY .and. &
	  srcStorage.eq.ESMF_IGRID_STORAGE_ARBITRARY) then
	  call ESMF_RoutePrecomputeRedistA2A(route, datarank, &
				hasDstData, dstMyDE, dstCompAI, &
				dstAICountPerDE, dstStartPerDEPerDim, &
				dstCellCountPerDim, dstDElayout, &
				hasSrcData, srcMyDE, srcCompAI, &
				srcAICountPerDE, srcStartPerDEPerDim, &
				srcCellCountPerDim, srcDElayout, status)
          if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      else				
          call ESMF_RoutePrecomputeRedistV(route, datarank, hasDstData, &
                                       dstMyDE, dstVector, &
                                       dstCompAI, dstTotalAI, &
                                       dstAICountPerDE, dstStartPerDEPerDim, &
                                       dstCellCountPerDim, dstDElayout, &
                                       hasSrcData, srcMyDE, srcVector, &
                                       srcCompAI, srcTotalAI, &
                                       srcAICountPerDE, srcStartPerDEPerDim, &
                                       srcCellCountPerDim, srcDElayout, status)
          if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      ! Set the route options if given.
      if (present(routeOptions)) then
          call c_ESMC_RouteSet(route, routeOptions, status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      endif

      ! and set route into routehandle object
      call ESMF_RouteHandleSet(routehandle,  which_route=index, route=route, &
                               rc=status)

      ! get rid of temporary arrays
      if (allocated(           periodic)) deallocate(periodic, stat=status)
      if (allocated(          decompids)) deallocate(decompids, stat=status)
      if (allocated( dstCellCountPerDim)) deallocate(dstCellCountPerDim, stat=status)
      if (allocated( srcCellCountPerDim)) deallocate(srcCellCountPerDim, stat=status)
      if (allocated(dstStartPerDEPerDim)) deallocate(dstStartPerDEPerDim, stat=status)
      if (allocated(srcStartPerDEPerDim)) deallocate(srcStartPerDEPerDim, stat=status)
      if (associated(   dstAICountPerDE)) deallocate(dstAICountPerDE, stat=status)
      if (associated(   srcAICountPerDE)) deallocate(srcAICountPerDE, stat=status)
      if (associated(         dstCompAI)) deallocate(dstCompAI, stat=status)
      if (associated(         srcCompAI)) deallocate(srcCompAI, stat=status)
      if (associated(        dstTotalAI)) deallocate(dstTotalAI, stat=status)
      if (associated(        srcTotalAI)) deallocate(srcTotalAI, stat=status)
      if (associated(       dstCLocalAI)) deallocate(dstCLocalAI, stat=status)
      if (associated(       srcCLocalAI)) deallocate(srcCLocalAI, stat=status)
      if (associated(       dstTLocalAI)) deallocate(dstTLocalAI, stat=status)
      if (associated(       srcTLocalAI)) deallocate(srcTLocalAI, stat=status)

      ! set return code if user specified it
      if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IArrayRedistStoreIndexArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRedistValidateList"
!BOPI
! !IROUTINE: ESMF_IArrayRedistValidate - Validate Redist for a list of Arrays
!
! !INTERFACE:
      ! Private name; call using ESMF_IArrayRedistValidate()
  subroutine ESMF_IArrayRedistValidateList(srcArrayList, dstArrayList, &
                                              routehandle, routeIndex, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(inout) :: srcArrayList(:)
      type(ESMF_InternArray), intent(inout) :: dstArrayList(:)
      type(ESMF_RouteHandle), intent(in) :: routehandle
      integer, intent(in), optional :: routeIndex
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Do extensive error checking on the incoming 
!     {\tt ESMF\_Array} list and the precomputed {\tt ESMF\_RouteHandle}
!     which was constructed to perform the communication necessary
!     to execute the redist operation.   If the inputs are not compatible
!     with each other, for example if the handle was precomputed based
!     on a different size {\tt ESMF\_Array}, an error message will be
!     logged and an error returned from this routine.
!
!     \begin{description}
!     \item [srcArrayList]
!           List of {\tt ESMF\_Array}s containing source data.
!     \item [dstArrayList]
!           List of {\tt ESMF\_Array}s containing results.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} precomputed by 
!           {\tt ESMF\_ArrayRedistValidatePrecompute()}.
!     \item [{[routeIndex]}]
!           If specified, select which of possibly multiple routes to execute
!           from this route handle.  Default value is 1.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status         ! local error status
      integer :: i, j, nitemsSrc, nitemsDst
      type(ESMF_Route) :: route
      ! debug only
      integer :: datarank
      integer :: counts(ESMF_MAXDIM)
      integer, allocatable :: srctotal(:), dsttotal(:)

      ! initialize return code; assume failure until success certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    do i=1, size(srcArrayList)
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_InternArrayGetInit, srcArrayList(i), rc)
    enddo
    do i=1, size(dstArrayList)
      ESMF_INIT_CHECK_DEEP_SHORT(ESMF_InternArrayGetInit, dstArrayList(i), rc)
    enddo
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

      if (present(routeIndex)) then
          call ESMF_RouteHandleGet(routehandle, which_route=routeIndex, &
                                        route=route, rc=status)
      else
          call ESMF_RouteHandleGet(routehandle, which_route=1, &
                                        route=route, rc=status)
      endif
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      nitemsSrc = size(srcArrayList)
      nitemsDst = size(dstArrayList)
      allocate(srctotal(nitemsSrc), dsttotal(nitemsDst), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Allocating count information", &
                                       ESMF_CONTEXT, rc)) return

      do j=1, nitemsSrc

        call ESMF_InternArrayGet(srcArrayList(j), counts=counts, &
                           rank=datarank, rc=status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        srctotal(j) = 1
        do i=1, datarank
           srctotal(j) = srctotal(j) * counts(i)
        enddo

      enddo
        
      do j=1, nitemsDst
        call ESMF_InternArrayGet(dstArrayList(j), counts=counts, &
                           rank=datarank, rc=status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        dsttotal(j) = 1
        do i=1, datarank
           dsttotal(j) = dsttotal(j) * counts(i)
        enddo
       
      enddo

      
      call ESMF_RouteValidate(route, nitemsSrc, srctotal, &
                                     nitemsDst, dsttotal, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      

      ! come here on error to free the temp buffers
 10   continue

      deallocate(srctotal, dsttotal, stat=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IArrayRedistValidateList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRedistValidateOne"
!BOPI
! !IROUTINE: ESMF_IArrayRedistValidate - Validate an Array Redist
!
! !INTERFACE:
      ! Private name; call using ESMF_IArrayRedistValidate()
  subroutine ESMF_IArrayRedistValidateOne(srcArray, dstArray, routehandle, &
                                             routeIndex, rc)
!
! !ARGUMENTS:
    type(ESMF_InternArray), intent(in) :: srcArray
    type(ESMF_InternArray), intent(in) :: dstArray
    type(ESMF_RouteHandle), intent(in) :: routehandle
    integer, intent(in), optional :: routeIndex
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Do extensive error checking on the incoming 
!     {\tt ESMF\_Array} and the precomputed {\tt ESMF\_RouteHandle}
!     which was constructed to perform the communication necessary
!     to execute the redist operation.   If the inputs are not compatible
!     with each other, for example if the handle was precomputed based
!     on a different size {\tt ESMF\_Array}, an error message will be
!     logged and an error returned from this routine.
!
!     \begin{description}
!     \item [srcArray]
!           {\tt ESMF\_Array} containing source data.
!     \item [dstArray]
!           {\tt ESMF\_Array} containing results.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} precomputed by 
!           {\tt ESMF\_ArrayRedistValidatePrecompute()}.
!     \item [{[routeIndex]}]
!           If specified, select which of possibly multiple routes to execute
!           from this route handle.  Default value is 1.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status         ! local error status
      type(ESMF_Route) :: route
      integer :: i, nitems, nitems2, datarank
      integer :: counts(ESMF_MAXDIM), srctotal(1), dsttotal(1)

      ! initialize return code; assume failure until success certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_InternArrayGetInit, srcArray, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_InternArrayGetInit, dstArray, rc)
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_RouteHandleGetInit, routehandle, rc)

      if (present(routeIndex)) then
          call ESMF_RouteHandleGet(routehandle, which_route=routeIndex, &
                                        route=route, rc=status)
      else
          call ESMF_RouteHandleGet(routehandle, which_route=1, &
                                        route=route, rc=status)
      endif
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


      call ESMF_InternArrayGet(srcArray, counts=counts, rank=datarank, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      nitems = 1
      srctotal(1) = 1
      do i=1, datarank
         srctotal(1) = srctotal(1) * counts(i)
      enddo
      
      call ESMF_InternArrayGet(dstArray, counts=counts, rank=datarank, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      nitems2 = 1
      dsttotal(1) = 1
      do i=1, datarank
         dsttotal(1) = dsttotal(1) * counts(i)
      enddo
       
      call ESMF_RouteValidate(route, nitems, srctotal, &
                                     nitems2, dsttotal, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IArrayRedistValidateOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayScatter"
!BOPI
! !IROUTINE: ESMF_IArrayScatter - Scatter a single Array across multiple DEs
!
! !INTERFACE:
  subroutine ESMF_IArrayScatter(array, delayout, decompids, rootDE, &
                                   scatteredArray, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(in) :: array
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(:), intent(in) :: decompids
      integer, intent(in) :: rootDE
      type(ESMF_InternArray), intent(out) :: scatteredArray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Scatter a single {\tt ESMF\_Array} on one DE into
!  a distributed {\tt ESMF\_Array} over multiple DEs.
!
!  The arguments are:
!     \begin{description}
!     \item [array]
!           {\tt ESMF\_Array} containing undistributed data to be scattered.
!     \item [igrid]
!           {\tt ESMF\_IGrid} which will correspond to the distributed data.
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap} which describes the mapping of the
!           data onto the cells in the {\tt ESMF\_IGrid}.
!     \item [rootDE]
!           The DE number on which the source {\tt ESMF\_Array} is located.
!     \item [scatteredArray]
!           The resulting distributed {\tt ESMF\_Array} after scattering.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        integer :: localrc         ! local error status
        integer :: size_decomp

        ! initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)

        ! call c routine to allgather
        size_decomp = size(decompids)
        call c_ESMC_IArrayScatter(array, delayout, decompids, size_decomp, &
                                 rootDE, scatteredArray, localrc)
        ! Init gathered Array as created
        call ESMF_InternArraySetInitCreated(scatteredArray)

        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_IArrayScatter

!------------------------------------------------------------------------------
end module ESMF_InternArrayCommMod









