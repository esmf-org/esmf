! $Id: ESMF_FieldBundleComm.F90,v 1.1.2.4 2009/01/21 21:25:21 cdeluca Exp $
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
!
#define ESMF_FILENAME "ESMF_FieldBundleComm.F90"
!
!     ESMF FieldBundle Communications module
      module ESMF_FieldBundleCommMod
!
!==============================================================================
!
! This file contains the FieldBundle communication methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldBundleCommMod - Communication routines for FieldBundle objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_FieldBundle} class
! communication routines, including Regridding, Redistribution, Halo, Gather,
! Scatter, and others.
!
! This type is implemented in Fortran 90 and a corresponding
! C++ interface is provided for access.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_VMMod
      use ESMF_DELayoutMod
      use ESMF_LocalArrayMod
      use ESMF_InternArrayMod
      use ESMF_RHandleMod
      use ESMF_RouteMod
      use ESMF_InternArrayCommMod
      use ESMF_InternArrayDataMapMod
      use ESMF_IGridTypesMod
      use ESMF_IGridMod
      use ESMF_FieldDataMapMod
      use ESMF_FieldMod
      use ESMF_FieldCommMod
      use ESMF_FieldBundleMod
      use ESMF_RegridMod
      use ESMF_RegridTypesMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
       private

       ! private flag - validate the inputs, and compare 2 bundles to be
       ! sure they have the same structure so we can call the comm routines.
       integer, parameter :: ESMF_BUNDLECOMM_NOMATCH = -1
       integer, parameter :: ESMF_BUNDLECOMM_CONGRUENT = 1
       integer, parameter :: ESMF_BUNDLECOMM_NONCONGRUENT = 2

! 
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!  <none>

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

   ! These are the recommended entry points; the code itself is in Array:
                             ! Synchronize boundary data between decompositions
   public ESMF_FieldBundleHaloStore, ESMF_FieldBundleHalo, ESMF_FieldBundleHaloRelease 
                             ! Redistribute existing arrays, matching igrids
   public ESMF_FieldBundleRedistStore, ESMF_FieldBundleRedist, ESMF_FieldBundleRedistRelease 
                             ! Regridding and interpolation, different igrids
   public ESMF_FieldBundleRegridStore, ESMF_FieldBundleRegrid, ESMF_FieldBundleRegridRelease 

   public ESMF_FieldBundleGather   ! Combine 1 decomposed bundle into 1 on 1 DE
   !public ESMF_FieldBundleAllGather! Combine 1 decomposed bundle into N copies on N DEs

   public ESMF_FieldBundleScatter  ! Split 1 bundle into a decomposed one over N DEs
   !public ESMF_FieldBundleBroadcast! Send 1 bundle to all DEs, none decomposed
   !public ESMF_FieldBundleAlltoAll ! might make sense with bundles; each DE could
                              ! call with a different non-decomposed bundle
                              ! and the result would be a packed bundle of
                              ! data with decomposed bundle on each DE.

   public ESMF_FieldBundleReduce     ! Global reduction operation, return on 1 DE
   !public ESMF_FieldBundleAllReduce  ! Global reduction operation, return on each DE

!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_FieldBundleComm.F90,v 1.1.2.4 2009/01/21 21:25:21 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldBundleRedist - Redist data, either with a routehandle or not
!
! !INTERFACE:
      interface ESMF_FieldBundleRedist

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleRedistAllinOne
        module procedure ESMF_FieldBundleRedistRun

! !DESCRIPTION:
!     Allow a single call to redist which precomputes, runs and releases
!     the route table; also support the more commonly expected use of
!     executing a route handle multiple times, where the user explicitly
!     calls store and release on the routehandle.
!
!EOPI
      end interface
!
!
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldBundleRegrid - Regrid data, either with a routehandle or not
!
! !INTERFACE:
      interface ESMF_FieldBundleRegrid

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleRegridAllinOne
        module procedure ESMF_FieldBundleRegridRun

! !DESCRIPTION:
!     Allow a single call to regrid which precomputes, runs and releases
!     the route table; also support the more commonly expected use of
!     executing a route handle multiple times, where the user explicitly
!     calls store and release on the routehandle.
!
!EOPI
      end interface
!
!
!
!==============================================================================
!
      contains
!
!==============================================================================
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Entry points for fuctionality which will happen mostly below at the IGrid
!   level, but needs a Data Pointer as well as igrid info to operate.
!   These include Reduction operations, Halo, and Transpose.
!
!------------------------------------------------------------------------------
#if 0
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAllGather"
!BOPI
! !IROUTINE: ESMF_FieldBundleAllGather - Data AllGather operation on a FieldBundle

! !INTERFACE:
      subroutine ESMF_FieldBundleAllGather(bundle, array, blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle                 
      type(ESMF_InternArray), intent(out) :: array
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform an allgather operation
!     over the data in an {\tt ESMF\_FieldBundle}.  If the {\tt ESMF\_FieldBundle} is
!     decomposed over N DEs, this routine returns a copy of the
!     entire collected data {\tt ESMF\_Array} on each of the N DEs.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_FieldBundle} containing data to be gathered.
!     \item [array] 
!           Newly created array containing the collected data.
!           It is the size of the entire undecomposed igrid.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      type(ESMF_FieldBundleType), pointer :: btypep    ! bundle type info
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      btypep => bundle%btypep

      ! Call Array method to perform actual work
      call ESMF_IArrayAllGather(btypep%flist(1)%ftypep%localfield%localdata, btypep%igrid, &
                               btypep%flist(1)%ftypep%mapping, array, status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleAllGather
#endif


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGather"
!BOPI
! !IROUTINE: ESMF_FieldBundleGather - Data gather operation on a FieldBundle

! !INTERFACE:
      subroutine ESMF_FieldBundleGather(bundle, destinationDE, arrayList, blocking, &
                                    commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle                 
      integer, intent(in) :: destinationDE
      type(ESMF_InternArray), intent(out) :: arrayList(:)
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a gather operation
!     over the data in an {\tt ESMF\_FieldBundle}.  If the {\tt ESMF\_FieldBundle} is
!     decomposed over N DEs, this routine returns a copy of the
!     entire collected data as an {\tt ESMF\_Array} 
!     on the specified destination
!     DE number.  On all other DEs there is no return {\tt array} value.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_FieldBundle} containing data to be gathered.
!     \item [destinationDE] 
!           Destination DE number where the gathered data is to be returned.
!     \item [arrayList] 
!           Newly created list of {\tt ESMF\_Array}s, one per {\tt ESMF\_Field}
!           in the {\tt ESMF\_FieldBundle}, containing the collected data on the
!           specified DE.  It is the size of the entire undecomposed igrid.
!           On all other DEs this argument returns an invalid object.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, the default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      integer :: i                                ! loop counter
      type(ESMF_FieldBundleType), pointer :: btypep    ! bundle type info
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      btypep => bundle%btypep

      do i=1, btypep%field_count
          ! Call Array method to perform actual work
          call ESMF_IArrayGather(btypep%flist(i)%ftypep%localfield%localdata, &
                                btypep%igrid, btypep%flist(i)%ftypep%mapping, &
                                destinationDE, arrayList(i), status)
          if (ESMF_LogMsgFoundError(status, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
      enddo

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGather


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleHalo"
!BOPI
! !IROUTINE: ESMF_FieldBundleHalo - Execute a halo operation on each Field in a FieldBundle

! !INTERFACE:
      subroutine ESMF_FieldBundleHalo(bundle, routehandle, blocking, &
                                 commhandle, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_BlockingFlag), intent(in) , optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a halo operation over each {\tt ESMF\_Field}
!     in an {\tt ESMF\_FieldBundle}.  This routine updates the data 
!     inside the {\tt ESMF\_FieldBundle} in place.  The current version
!     of the code does not support {\tt ESMF\_FieldBundle}s with packed data.
!     It simply operates on a {\tt ESMF\_Field} by {\tt ESMF\_Field}
!     basis, updating each one at a time.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_FieldBundle} containing data to be haloed.
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} which was returned by the corresponding
!           {\tt ESMF\_FieldBundleHaloStore()} call. It is associated with 
!           the precomputed data movement and communication needed to 
!           perform the halo operation.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is what was specified at Store time.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to 
!           select when executing the communication needed to update the halo.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      integer :: i                                ! loop counter
      integer :: nitems
      type(ESMF_FieldBundleType), pointer :: btypep    ! bundle type info
      type(ESMF_InternArray), allocatable :: arrayList(:)
      integer :: maptype
      logical :: bundlepack
      integer :: bopt, bflag, rcount

   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if(present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btypep => bundle%btypep

      ! before looking a bundle, see what kind of mapping is in this handle.
      ! and if the route options request no packing by bundle.
      call ESMF_RouteHandleGet(routehandle, rmaptype=maptype, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      bundlepack = .true.
      if (present(routeOptions)) then
          bopt = routeOptions    ! turn into ints
          bflag = ESMF_ROUTE_OPTION_PACK_BUFFER
          if (iand(bopt, bflag) .eq. 0) bundlepack = .false.  ! bitwise and

          ! TODO: sort out options at the rhandle vs route level.
          ! call ESMF_RouteSet(route, routeOptions, rc=status)
      endif
   
      ! if the map was computed one per input, then call it here.
      if (maptype .eq. ESMF_1TO1HANDLEMAP) then
          ! make sure things match
          call ESMF_RouteHandleGet(routehandle, route_count=rcount, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (rcount .ne. btypep%field_count) then
              call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                  "FieldBundles do not match FieldBundles used in FieldBundleHaloStore", &
                   ESMF_CONTEXT, rc)
              return
          endif

          ! one route per source field; loop here.
          do i=1, btypep%field_count

            call ESMF_IArrayHalo(btypep%flist(i)%ftypep%localfield%localdata, &
                                routehandle, i, blocking, rc=status)
            if (ESMF_LogMsgFoundError(status, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
          enddo

      else
        ! a single route applies to all fields.  requires a congruent bundle.
        if (.not. ESMF_FieldBundleIsCongruent(bundle, rc=status)) then
          ! problem - the map was computed with a congruent bundle
          ! and this one is not.  error out and return. 
          call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                  "FieldBundleHaloStore() was called with an incompatible FieldBundle", &
                   ESMF_CONTEXT, rc)
          return
        else
          ! the default is to loop inside the route and pack fields, but
          ! the options can prevent that from happening.
          if (bundlepack) then

            ! bundle fields are all the same, and routehandle has only
            ! one shared route in it.
            nitems = btypep%field_count
            allocate(arrayList(nitems), stat=status)
            if (ESMF_LogMsgFoundAllocError(status, & 
                                          "Allocating arraylist information", &
                                           ESMF_CONTEXT, rc)) return
            ! make a list of arrays
            do i=1, nitems
              arrayList(i) = btypep%flist(i)%ftypep%localfield%localdata
            enddo
    
    
            call ESMF_IArrayHalo(arrayList, routehandle, 1,  &
                                  blocking, rc=status)
    
          else
            ! multiple congruent fields but single route.   
            ! loop here.
            do i=1, btypep%field_count
  
              call ESMF_IArrayHalo(btypep%flist(i)%ftypep%localfield%localdata, &
                                routehandle, 1, blocking, rc=status)
              if (ESMF_LogMsgFoundError(status, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return
            enddo
          endif
        endif
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleHalo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleHaloRelease"
!BOPI
! !IROUTINE: ESMF_FieldBundleHaloRelease - Release resources associated w/ handle

! !INTERFACE:
      subroutine ESMF_FieldBundleHaloRelease(routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Release all stored information about the halo operation associated
!     with this {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this halo operation.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc                        ! local return code

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_RouteHandleDestroy(routehandle, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_FieldBundleHaloRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleHaloStore"
!BOPI
! !IROUTINE: ESMF_FieldBundleHaloStore - Precompute a data halo operation on a FieldBundle

! !INTERFACE:
      subroutine ESMF_FieldBundleHaloStore(bundle, routehandle, halodirection, &
                                      routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_HaloDirection), intent(in), optional :: halodirection
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Precompute the data movement or communication operations needed 
!     to perform a halo operation over the data in an {\tt ESMF\_FieldBundle}. 
!     The list of operations will be associated internally to the
!     framework with the 
!     {\tt ESMF\_RouteHandle} object.  
!     To perform the actual halo operation
!     the {\tt ESMF\_FieldBundleHalo()} routine must be called with the
!     {\tt ESMF\_FieldBundle} containing the data to be updated and the 
!     {\tt ESMF\_RouteHandle} computed during this store call.
!     Although probably less common with bundles than with fields,
!     if more than one {\tt ESMF\_FieldBundle} has an identical 
!     {\tt ESMF\_IGrid} and contains identical {\tt ESMF\_Field}s, then
!     the same {\tt ESMF\_RouteHandle} can be computed once and used
!     in multiple executions of the halo operation.
!     In the current version of the code {\tt ESMF\_FieldBundle}s with
!     packed data are not supported.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_FieldBundle} containing data to be haloed.
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} which will be returned after being
!           associated with the precomputed 
!           information for a halo operation on this {\tt ESMF\_FieldBundle}.
!           This handle must be supplied at run time to execute the halo.
!     \item [{[halodirection]}]
!           Optional argument to restrict halo direction to a subset of the
!           possible halo directions.  If not specified, the halo is executed
!           along all boundaries. (This feature is not yet supported.)
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to 
!           select when executing the communication needed to update the halo.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      integer :: i
      type(ESMF_FieldBundleType), pointer :: btypep     ! bundle type info
      logical :: bundlepack
      integer :: bopt, bflag
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btypep => bundle%btypep

      ! if specified, make sure routeoptions do not disable the
      ! field-level packing option.
      bundlepack = .true.
      if (present(routeOptions)) then
          bopt = routeOptions    ! turn into ints
          bflag = ESMF_ROUTE_OPTION_PACK_BUFFER
          if (iand(bopt, bflag) .eq. 0) bundlepack = .false.  ! bitwise and
      endif

      ! if all fields are identical, they can share a route
      if (ESMF_FieldBundleIsCongruent(bundle, rc=status) .and. bundlepack) then
        call ESMF_IArrayHaloStore( &
                                  btypep%flist(1)%ftypep%localfield%localdata, &
                                  btypep%flist(1)%ftypep%localfield%localFlag, &
                                  1, ESMF_ALLTO1HANDLEMAP, 1, &
                                  btypep%igrid, &
                                  btypep%flist(1)%ftypep%mapping, routehandle, &
                                  halodirection, routeOptions, rc=status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      else
        ! exercise the new code in routehandle - it can now contain multiple
        ! routes inside a single handle.  set the map to be 1 to 1.
        do i=1, btypep%field_count
 
          call ESMF_IArrayHaloStore( &
                                  btypep%flist(i)%ftypep%localfield%localdata, &
                                  btypep%flist(i)%ftypep%localfield%localFlag, &
                                  i, ESMF_1TO1HANDLEMAP, btypep%field_count, &
                                  btypep%igrid, &
                                  btypep%flist(i)%ftypep%mapping, &
                                  routehandle, &
                                  halodirection, routeOptions, rc=status)
  
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        enddo

        ! TODO:  when we want to support the minimal set of routes for
        ! all fields in the bundle (e.g. all 2d, integer fields share a route),
        ! put the logic here and set the handle type to INDIRECTHANDLEMAP.

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleHaloStore

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRedist"
!BOP
! !IROUTINE: ESMF_FieldBundleRedist - Data redistribution operation on a FieldBundle

! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleRedist()
      subroutine ESMF_FieldBundleRedistAllinOne(srcFieldBundle, dstFieldBundle, parentVM, &
                                   blocking, commhandle, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: srcFieldBundle
      type(ESMF_FieldBundle), intent(inout) :: dstFieldBundle
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_BlockingFlag), intent(in) , optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a redistribution operation over the data
!     in an {\tt ESMF\_FieldBundle}.  This version does not take a {\tt routehandle}
!     and computes, runs, and releases the communication information in a
!     single subroutine.  It should be used when a redist operation will be
!     done only a single time; otherwise computing and reusing a communication
!     pattern will be more efficient. 
!     This routine reads the source bundle and leaves 
!     the data untouched.  It reads the {\tt ESMF\_IGrid} and 
!     {\tt ESMF\_FieldDataMap}
!     from the destination bundle and updates the array data in the destination.
!     The {\tt ESMF\_IGrid}s may have different decompositions (different
!     {\tt ESMF\_DELayout}s) or different data maps, but the source and
!     destination igrids must describe the same set of coordinates.
!     Unlike {\tt ESMF\_FieldBundleRegrid} this routine does not do interpolation,
!     only data movement.
!
!     The arguments are:
!     \begin{description}
!     \item [srcFieldBundle] 
!           {\tt ESMF\_FieldBundle} containing source data.
!     \item [dstFieldBundle] 
!           {\tt ESMF\_FieldBundle} containing destination igrid.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_FieldBundle}s,
!           most commonly the VM of the Coupler if the redistribution is
!           inter-component, but could also be the individual VM for a
!           component if the redistribution is intra-component.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is to do synchronous communication.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to redistribute data.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                            ! Error status
      type(ESMF_FieldBundleType) :: stypep, dtypep      ! bundle type info
      type(ESMF_RouteHandle) :: routehandle
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,srcFieldBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,dstFieldBundle,rc)

      stypep = srcFieldBundle%btypep
      dtypep = dstFieldBundle%btypep
      routehandle = ESMF_RouteHandleCreate(status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call FieldBundleRedistStore
      call ESMF_FieldBundleRedistStore(srcFieldBundle, dstFieldBundle, parentVM, &
                                  routehandle, routeOptions, status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call FieldBundleRedistRun
      call ESMF_FieldBundleRedistRun(srcFieldBundle, dstFieldBundle, routehandle, &
                                blocking, commhandle, routeOptions, status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call FieldBundleRedistRelease
      call ESMF_FieldBundleRedistRelease(routehandle, status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleRedistAllinOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRedist"
!BOP
! !IROUTINE: ESMF_FieldBundleRedist - Data redistribution operation on a FieldBundle

! !INTERFACE:
      ! Private name; call using ESMF_FieldRedist()
      subroutine ESMF_FieldBundleRedistRun(srcFieldBundle, dstFieldBundle, routehandle, &
                                   blocking, commhandle, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: srcFieldBundle
      type(ESMF_FieldBundle), intent(inout) :: dstFieldBundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_BlockingFlag), intent(in) , optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a redistribution operation over the data
!     in an {\tt ESMF\_FieldBundle}.  
!     This routine reads the source bundle and leaves 
!     the data untouched.  It reads the {\tt ESMF\_IGrid} and 
!     {\tt ESMF\_FieldDataMap}
!     from the destination bundle and updates the array data in the destination.
!     The {\tt ESMF\_IGrid}s may have different decompositions (different
!     {\tt ESMF\_DELayout}s) or different data maps, but the source and
!     destination igrids must describe the same set of coordinates.
!     Unlike {\tt ESMF\_FieldBundleRegrid} this routine does not do interpolation,
!     only data movement.
!
!     The arguments are:
!     \begin{description}
!     \item [srcbundle] 
!           {\tt ESMF\_FieldBundle} containing source data.
!     \item [dstbundle] 
!           {\tt ESMF\_FieldBundle} containing destination igrid.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} which was returned by the corresponding
!           {\tt ESMF\_FieldBundleRedistStore()} call. It is associated with
!           the precomputed data movement and communication needed to
!           perform the redistribution operation.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is to do synchronous communication.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to redistribute data.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                            ! Error status
      integer :: i                                 ! loop counter
      type(ESMF_FieldBundleType) :: stypep, dtypep      ! bundle type info
      type(ESMF_InternArray), allocatable :: srcArrayList(:), dstArrayList(:)
      integer :: condition
      integer :: maptype
      logical :: bundlepack
      integer :: bopt, bflag, rcount
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,srcFieldBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,dstFieldBundle,rc)

      stypep = srcFieldBundle%btypep
      dtypep = dstFieldBundle%btypep

      ! Does validate of both bundles and checks for consistent types.
      condition = ESMF_FieldBundleCommPrepCheck(srcFieldBundle, dstFieldBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
     
      if (condition .eq. ESMF_BUNDLECOMM_NOMATCH) return


      ! If the bundle consists of identical fields - in every way: data type,
      ! relloc, index order, the works -- then pass in a list of all arrays
      ! and RouteRun will have the option of looping over the arrays and 
      ! packing at a higher level.   If the fields differ, then call routerun
      ! once per separate field, essentially doing a loop inside the framework
      ! instead of having to be done by the user outside; but there is no
      ! additional optimization possible other than reusing the same route
      ! table each time.
      
      ! if specified, make sure routeoptions do not disable the
      ! field-level packing option.
      bundlepack = .true.
      if (present(routeOptions)) then
          bopt = routeOptions    ! turn into int
          bflag = ESMF_ROUTE_OPTION_PACK_BUFFER
          if (iand(bopt, bflag) .eq. 0) bundlepack = .false.
      endif

      ! before looking a bundle, see what kind of mapping is in this handle.
      ! and if the route options request no packing by bundle.
      call ESMF_RouteHandleGet(routehandle, rmaptype=maptype, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! based on the handle type, see if we are sharing a route table (and
      ! if that's permitted) or if we are looping internally.

      ! if the map was computed one route per field, loop here.
      if (maptype .eq. ESMF_1TO1HANDLEMAP) then

        ! some simple error checks here
        call ESMF_RouteHandleGet(routehandle, route_count=rcount, rc=status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
   
        ! we have already verified that source count == dest count, so either
        ! can be compared against the number of routes in this handle.
        if (rcount .ne. stypep%field_count) then
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                "RouteHandle and FieldBundles do not have matching Field counts", &
                                     ESMF_CONTEXT, rc)
            return
        endif

        do i = 1, stypep%field_count

         ! routehandle now internally stores multiple routes
         call ESMF_IArrayRedist(stypep%flist(i)%ftypep%localfield%localdata, &
                                stypep%flist(i)%ftypep%localfield%localFlag, &
                                dtypep%flist(i)%ftypep%localfield%localdata, &
                                dtypep%flist(i)%ftypep%localfield%localFlag, &
                                routehandle, i, blocking, &
                                routeOptions, status)
         if (ESMF_LogMsgFoundError(status, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rc)) return
        enddo

      else
        ! a single route was computed for all fields.  requires congruent
        ! bundles as inputs.
        if (condition .eq. ESMF_BUNDLECOMM_NONCONGRUENT) then
          ! problem.  routehandle thinks they are.
          call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                  "FieldBundles do not match FieldBundles used in FieldBundleRedistStore()", &
                   ESMF_CONTEXT, rc)
          return
        else
          ! this case can loop inside routerun but the options may prevent
          ! that from happining.
          if (bundlepack) then

            allocate(srcArrayList(stypep%field_count), stat=status)
            allocate(dstArrayList(dtypep%field_count), stat=status)
     
            do i=1, stypep%field_count
                srcArrayList(i) = stypep%flist(i)%ftypep%localfield%localdata
            enddo
      
            do i=1, dtypep%field_count
                dstArrayList(i) = dtypep%flist(i)%ftypep%localfield%localdata
            enddo
      
            call ESMF_IArrayRedist(srcArrayList, &
              dtypep%flist(1)%ftypep%localfield%localFlag, &
              dstArrayList, stypep%flist(1)%ftypep%localfield%localFlag, &
              routehandle, 1, blocking, routeOptions, status)
            if (ESMF_LogMsgFoundError(status, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) then
                deallocate(srcArrayList, dstArrayList, stat=status) 
                return
            endif
  
            deallocate(srcArrayList, dstArrayList, stat=status) 
          else
            ! multiple congruent fields in the bundles but loop over the
            ! single route.
            do i=1, stypep%field_count
  
              call ESMF_IArrayRedist( &
                                 stypep%flist(i)%ftypep%localfield%localdata, &
                                 stypep%flist(i)%ftypep%localfield%localFlag, &
                                 dtypep%flist(i)%ftypep%localfield%localdata, &
                                 dtypep%flist(i)%ftypep%localfield%localFlag, &
                                 routehandle, 1, blocking, &
                                 routeOptions, status)
              if (ESMF_LogMsgFoundError(status, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return
             enddo
  
          endif
        endif
      endif

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleRedistRun

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRedistRelease"
!BOP
! !IROUTINE: ESMF_FieldBundleRedistRelease - Release resources associated with handle

! !INTERFACE:
      subroutine ESMF_FieldBundleRedistRelease(routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!  Release all stored information about the redistribution operation associated
!  with this {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this redistribution.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
      integer :: localrc                        ! local return code

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_RouteHandleDestroy(routehandle, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_FieldBundleRedistRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRedistStore"
!BOP
! !IROUTINE: ESMF_FieldBundleRedistStore - Data redistribution operation on a FieldBundle

! !INTERFACE:
      subroutine ESMF_FieldBundleRedistStore(srcFieldBundle, dstFieldBundle, parentVM, &
                                        routehandle, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: srcFieldBundle
      type(ESMF_FieldBundle), intent(inout) :: dstFieldBundle
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_RouteHandle), intent(out) :: routehandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Precompute the data movement or communications operations needed to
!     accomplish a data redistribution operation over the data
!     in an {\tt ESMF\_FieldBundle}.  Data redistribution differs from regridding
!     in that redistribution does no interpolation, only a 1-for-1 movement
!     of data from one location to another.
!     Therefore, while 
!     the {\tt ESMF\_IGrid}s for the source and destination may have
!     different decompositions (different {\tt ESMF\_DELayout}s) 
!     or different data maps, the source and destination igrids 
!     must describe the same set of coordinates.
!
!     The arguments are:
!     \begin{description}
!     \item [srcFieldBundle]
!           {\tt ESMF\_FieldBundle} containing source data.
!     \item [dstFieldBundle]
!           {\tt ESMF\_FieldBundle} containing destination igrid.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_FieldBundle}s, 
!           most commonly the VM
!           of the Coupler if the redistribution is inter-component, 
!           but could also be the individual VM for a component if the 
!           redistribution is intra-component.  
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} which will be used to execute the
!           redistribution when {\tt ESMF\_FieldBundleRedist} is called.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to 
!           select when executing the communication needed to update the halo.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      integer :: i
      integer :: condition
      type(ESMF_FieldBundleType), pointer :: stypep, dtypep
      logical :: bundlepack
      integer :: bopt, bflag
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,srcFieldBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,dstFieldBundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(srcFieldBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_FieldBundleValidate(dstFieldBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

 
      ! validate both bundles, make sure they have the same number
      ! of fields, make sure the data types are consistent, etc.

      condition = ESMF_FieldBundleCommPrepCheck(srcFieldBundle, dstFieldBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
     
      if (condition .eq. ESMF_BUNDLECOMM_NOMATCH) return

      ! if specified, make sure routeoptions do not disable the
      ! field-level packing option.
      bundlepack = .true.
      if (present(routeOptions)) then
          bopt = routeOptions    ! turn into int
          bflag = ESMF_ROUTE_OPTION_PACK_BUFFER
          if (iand(bopt, bflag) .eq. 0) bundlepack = .false.
      endif

      dtypep => dstFieldBundle%btypep
      stypep => srcFieldBundle%btypep

      ! if all fields are identical, they can share a route
      if ((condition .eq. ESMF_BUNDLECOMM_CONGRUENT) .and. bundlepack) then 
         call ESMF_IArrayRedistStore( &
                                 stypep%flist(1)%ftypep%localfield%localdata, &
                                 stypep%flist(1)%ftypep%localfield%localFlag, &
                                 stypep%igrid, &
                                 stypep%flist(1)%ftypep%mapping, &
                                 dtypep%flist(1)%ftypep%localfield%localdata, &
                                 dtypep%flist(1)%ftypep%localfield%localFlag, &
                                 dtypep%igrid, &
                                 dtypep%flist(1)%ftypep%mapping, &
                                 1, ESMF_ALLTO1HANDLEMAP, 1, &
                                 parentVM, &
                                 routehandle, routeOptions, status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      else
        ! the prepcheck call above has verified that the number of fields
        ! match, so we can use either field count safely here.
        do i=1, stypep%field_count
 
           call ESMF_IArrayRedistStore( &
                               stypep%flist(i)%ftypep%localfield%localdata, &
                               stypep%flist(i)%ftypep%localfield%localFlag, &
                               stypep%igrid, &
                               stypep%flist(i)%ftypep%mapping, &
                               dtypep%flist(i)%ftypep%localfield%localdata, &
                               dtypep%flist(i)%ftypep%localfield%localFlag, &
                               dtypep%igrid, &
                               dtypep%flist(i)%ftypep%mapping, &
                               i, ESMF_1TO1HANDLEMAP, stypep%field_count, &
                               parentVM, &
                               routehandle, routeOptions, status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        enddo
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleRedistStore


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleReduce"
!BOPI
! !IROUTINE: ESMF_FieldBundleReduce - Reduction operation on a FieldBundle

! !INTERFACE:
      subroutine ESMF_FieldBundleReduce(bundle, rtype, result, blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldBundle) :: bundle                 
      integer :: rtype
      integer :: result
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a reduction operation over the data in an {\tt ESMF\_FieldBundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_FieldBundle} containing data to be reduced.
!     \item [rtype]
!           Type of reduction operation to perform.  Options include: ...
!           (Not yet implemented).
!     \item [result] 
!           Numeric result (may be single number, may be array)
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Call IGrid method to perform actual work??  TODO: is this right?
      !call ESMF_IGridReduce(field%btypep%igrid, &
      !                     field%btypep%flist(1)%ftypep%localfield%localdata, &
      !                     rtype, result, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleReduce

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegrid"
!BOP
! !IROUTINE: ESMF_FieldBundleRegrid - Execute a regrid operation on a FieldBundle

! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleRegrid()
      subroutine ESMF_FieldBundleRegridAllinOne(srcFieldBundle, dstFieldBundle, parentVM, &
                                   regridMethod, regridNorm, &
                                   srcMask, dstMask, blocking, commhandle, &
                                   routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: srcFieldBundle
      type(ESMF_FieldBundle), intent(inout) :: dstFieldBundle
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_RegridMethod), intent(in) :: regridMethod
      type(ESMF_RegridNormOpt), intent(in), optional :: regridNorm
      type(ESMF_Mask), intent(in), optional :: srcMask
      type(ESMF_Mask), intent(in), optional :: dstMask
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a regrid operation over the data
!     in an {\tt ESMF\_FieldBundle}.  This routine reads the source bundle and 
!     leaves the data untouched.  It uses the {\tt ESMF\_IGrid} and
!     {\tt ESMF\_FieldDataMap} information in the destination bundle to
!     control the transformation of data.  The array data in the 
!     destination bundle is overwritten by this call.
!     This version does not take a {\tt routehandle}
!     but computes, runs, and releases the communication information in a
!     single subroutine.  It should be used when a redist operation will be
!     done only a single time; otherwise computing and reusing a communication
!     pattern will be more efficient. 
!
!     The arguments are:
!     \begin{description}
!     \item [srcFieldBundle] 
!           {\tt ESMF\_FieldBundle} containing source data.
!     \item [dstFieldBundle] 
!           {\tt ESMF\_FieldBundle} containing destination igrid.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_FieldBundle}s, 
!           most commonly the VM of the Coupler if the regridding is
!           inter-component, but could also be the individual VM for a component
!           if the regridding is intra-component.  
!     \item [regridMethod]
!           Type of regridding to do.  A set of predefined methods are
!           supplied.
!     \item [{[regridNorm]}]
!           Normalization option, only for specific regrid types.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is to do synchronous communication.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to redistribute data.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: localrc                           ! Error status
      type(ESMF_FieldBundleType) :: stypep, dtypep      ! bundle type info
      type(ESMF_RouteHandle) :: routehandle
   
      ! Initialize return code   
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,srcFieldBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,dstFieldBundle,rc)

      stypep = srcFieldBundle%btypep
      dtypep = dstFieldBundle%btypep
      routehandle = ESMF_RouteHandleCreate(localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call FieldBundleRegridStore
      call ESMF_FieldBundleRegridStore(srcFieldBundle, dstFieldBundle, parentVM, &
                                  routehandle, regridMethod, regridNorm, &
                                  srcMask, dstMask, routeOptions, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call FieldBundleRegridRun
      call ESMF_FieldBundleRegridRun(srcFieldBundle, dstFieldBundle, routehandle, &
                                srcMask, dstMask, blocking, commhandle, &
                                routeOptions, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call FieldBundleRegridRelease
      call ESMF_FieldBundleRegridRelease(routehandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleRegridAllinOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegrid"
!BOPI
! !IROUTINE: ESMF_FieldBundleRegrid - Execute a regrid operation on a FieldBundle

! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleRegrid()
      subroutine ESMF_FieldBundleRegridRun(srcFieldBundle, dstFieldBundle, routehandle, &
                                   srcMask, dstMask, blocking, commhandle, &
                                   routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: srcFieldBundle
      type(ESMF_FieldBundle), intent(inout) :: dstFieldBundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_Mask), intent(in), optional :: srcMask
      type(ESMF_Mask), intent(in), optional :: dstMask
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a regrid operation over the data
!     in an {\tt ESMF\_FieldBundle}.  This routine reads the source bundle and 
!     leaves the data untouched.  It uses the {\tt ESMF\_IGrid} and
!     {\tt ESMF\_FieldDataMap} information in the destination bundle to
!     control the transformation of data.  The array data in the 
!     destination bundle is overwritten by this call.
!
!     The arguments are:
!     \begin{description}
!     \item [srcbundle] 
!           {\tt ESMF\_FieldBundle} containing source data.
!     \item [dstbundle] 
!           {\tt ESMF\_FieldBundle} containing destination igrid and data map.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} which will be returned after being
!           associated with the precomputed
!           information for a regrid operation on this {\tt ESMF\_Field}.
!           This handle must be supplied at run time to execute the regrid.
!     \item [{[srcMask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!           (Not yet implemented.)
!     \item [{[dstMask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!           (Not yet implemented.)
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between DEs has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the regrid.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                            ! Error status
      integer :: i                                 ! loop counter
      type(ESMF_FieldBundleType) :: stypep, dtypep      ! bundle type info
      type(ESMF_InternArray), allocatable :: srcArrayList(:), dstArrayList(:)
      integer :: condition
      logical :: hasSrcData, hasDstData
      integer :: maptype
      logical :: bundlepack
      integer :: bopt, bflag, rcount
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,srcFieldBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,dstFieldBundle,rc)

      ! Does validate of both bundles and checks for consistent types.
      condition = ESMF_FieldBundleCommPrepCheck(srcFieldBundle, dstFieldBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
     
      if (condition .eq. ESMF_BUNDLECOMM_NOMATCH) return

      ! before looking a bundle, see what kind of mapping is in this handle.
      ! and if the route options request no packing by bundle.
      call ESMF_RouteHandleGet(routehandle, rmaptype=maptype, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      bundlepack = .true.
      if (present(routeOptions)) then
          bopt = routeOptions    ! turn into ints
          bflag = ESMF_ROUTE_OPTION_PACK_BUFFER
          if (iand(bopt, bflag) .eq. 0) bundlepack = .false.  ! bitwise and

          ! TODO: sort out options at the rhandle vs route level.
          ! call ESMF_RouteSet(route, routeOptions, rc=status)
      endif
   
      stypep = srcFieldBundle%btypep
      dtypep = dstFieldBundle%btypep

      
      ! If the bundle consists of identical fields - in every way: data type,
      ! relloc, index order, the works -- then pass in a list of all arrays
      ! and RouteRun will have the option of looping over the arrays and 
      ! packing at a higher level.   If the fields differ, then call routerun
      ! once per separate field, essentially doing a loop inside the framework
      ! instead of having to be done by the user outside; but there is no
      ! additional optimization possible other than reusing the same route
      ! table each time.
     
      ! if the map was computed one per input, then call it here.
      if (maptype .eq. ESMF_1TO1HANDLEMAP) then
          ! make sure things match
          call ESMF_RouteHandleGet(routehandle, route_count=rcount, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (rcount .ne. stypep%field_count) then
              call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                  "FieldBundles do not match FieldBundles used in FieldBundleRegridStore", &
                   ESMF_CONTEXT, rc)
              return
          endif

          do i = 1, stypep%field_count
            hasSrcData = ESMF_RegridHasData(stypep%igrid, &
                                          stypep%flist(i)%ftypep%mapping)
            hasDstData = ESMF_RegridHasData(dtypep%igrid, &
                                          dtypep%flist(i)%ftypep%mapping)

            call ESMF_IArrayRegrid(stypep%flist(i)%ftypep%localfield%localdata, &
                                  stypep%flist(i)%ftypep%mapping, hasSrcData, &
                                  dtypep%flist(i)%ftypep%localfield%localdata, &
                                  dtypep%flist(i)%ftypep%mapping, hasDstData, &
                                  routehandle, i, &
                                  srcMask, dstMask, &
                                  blocking, commhandle, routeOptions, status)
            if (ESMF_LogMsgFoundError(status, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
          enddo

      else
          ! a single route applies to all fields.  requires a pair of 
          ! congruent bundles.
          if (condition .eq. ESMF_BUNDLECOMM_NONCONGRUENT) then
            ! problem.  routehandle thinks they are.
            call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                  "FieldBundles do not match FieldBundles used in FieldBundleRegridStore()", &
                   ESMF_CONTEXT, rc)
            return
          endif

          if (bundlepack) then
              ! bundle fields all the same, and routehandle ahs only 
              ! one shared route in it.
              allocate(srcArrayList(stypep%field_count), stat=status)
              allocate(dstArrayList(dtypep%field_count), stat=status)
       
              do i=1, stypep%field_count
                  srcArrayList(i) = stypep%flist(i)%ftypep%localfield%localdata
              enddo
        
              do i=1, dtypep%field_count
                  dstArrayList(i) = dtypep%flist(i)%ftypep%localfield%localdata
              enddo
        
              hasSrcData = ESMF_RegridHasData(stypep%igrid, &
                                              stypep%flist(1)%ftypep%mapping)
              hasDstData = ESMF_RegridHasData(dtypep%igrid, &
                                              dtypep%flist(1)%ftypep%mapping)
    
              ! TODO: do the datamaps have to go in as lists as well?
              call ESMF_IArrayRegrid(srcArrayList, & 
                                 stypep%flist(1)%ftypep%mapping, hasSrcData, &
                                 dstArrayList, &
                                 dtypep%flist(1)%ftypep%mapping, hasDstData, &
                                 routehandle, 1, srcMask, dstMask, &
                                 blocking, commhandle, routeOptions, status)
              if (ESMF_LogMsgFoundError(status, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) then
                  deallocate(srcArrayList, dstArrayList, stat=status) 
                  return
              endif
    
              deallocate(srcArrayList, dstArrayList, stat=status) 
          else
              ! multiple congruent fields, but a single route
              do i = 1, stypep%field_count
                hasSrcData = ESMF_RegridHasData(stypep%igrid, &
                                              stypep%flist(i)%ftypep%mapping)
                hasDstData = ESMF_RegridHasData(dtypep%igrid, &
                                              dtypep%flist(i)%ftypep%mapping)

                call ESMF_IArrayRegrid( &
                                  stypep%flist(i)%ftypep%localfield%localdata, &
                                  stypep%flist(i)%ftypep%mapping, hasSrcData, &
                                  dtypep%flist(i)%ftypep%localfield%localdata, &
                                  dtypep%flist(i)%ftypep%mapping, hasDstData, &
                                  routehandle, 1, &
                                  srcMask, dstMask, &
                                  blocking, commhandle, routeOptions, status)
                if (ESMF_LogMsgFoundError(status, &
                                          ESMF_ERR_PASSTHRU, &
                                          ESMF_CONTEXT, rc)) return
              enddo
        
          endif
      endif

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleRegridRun


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegridRelease"
!BOPI
! !IROUTINE: ESMF_FieldBundleRegridRelease - Release information for this handle

! !INTERFACE:
      subroutine ESMF_FieldBundleRegridRelease(routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Release all stored information about the regridding operation
!     associated with this {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this regrid operation.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 
      integer :: localrc                        ! local return code

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      call ESMF_RouteHandleDestroy(routehandle, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_FieldBundleRegridRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRegridStore"
!BOPI
! !IROUTINE: ESMF_FieldBundleRegridStore - Precompute regrid operation on a FieldBundle

! !INTERFACE:
      subroutine ESMF_FieldBundleRegridStore(srcFieldBundle, dstFieldBundle, parentVM, &
                                        routehandle, regridMethod, regridNorm, &
                                        srcMask, dstMask, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: srcFieldBundle
      type(ESMF_FieldBundle), intent(inout) :: dstFieldBundle
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_RouteHandle), intent(out) :: routehandle
      type(ESMF_RegridMethod), intent(in) :: regridMethod
      type(ESMF_RegridNormOpt), intent(in), optional :: regridNorm
      type(ESMF_Mask), intent(in), optional :: srcMask
      type(ESMF_Mask), intent(in), optional :: dstMask
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Precompute the data movement or communications operations plus the
!     interpolation information needed to execute
!     a regrid operation which will move and transform data 
!     from the source
!     bundle to the destination bundle. 
!     This information is associated with the {\tt ESMF\_RouteHandle}
!     which must then be supplied during the actual execution of the
!     regrid operation.
!
!     The arguments are:
!     \begin{description}
!     \item [srcFieldBundle] 
!           {\tt ESMF\_FieldBundle} containing source data.
!     \item [dstFieldBundle] 
!           {\tt ESMF\_FieldBundle} containing destination igrid and data map.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_FieldBundle}s, 
!           most commonly the VM
!           of the Coupler if the regridding is inter-component, but could 
!           also be the individual VM for a component if the 
!           regridding is intra-component.  
!     \item [routehandle]
!           Output from this call, identifies the precomputed work which
!           will be executed when {\tt ESMF\_FieldBundleRegrid} is called.
!     \item [regridMethod]
!           Type of regridding to do.  A set of predefined methods are
!           supplied.
!     \item [{[regridNorm]}]
!           Normalization option, only for specific regrid types.
!     \item [{[srcMask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstMask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the regrid.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      integer :: i
      type(ESMF_FieldBundleType), pointer :: stypep, dtypep
      integer :: condition
      logical :: bundlepack
      integer :: bopt, bflag
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,srcFieldBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,dstFieldBundle,rc)

      ! Does validate of both bundles and checks for consistent types.
      condition = ESMF_FieldBundleCommPrepCheck(srcFieldBundle, dstFieldBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
     
      if (condition .eq. ESMF_BUNDLECOMM_NOMATCH) return

      ! if specified, make sure routeoptions do not disable the
      ! field-level packing option.
      bundlepack = .true.
      if (present(routeOptions)) then
          bopt = routeOptions    ! turn into ints
          bflag = ESMF_ROUTE_OPTION_PACK_BUFFER
          if (iand(bopt, bflag) .eq. 0) bundlepack = .false.  ! bitwise and
      endif

      dtypep => dstFieldBundle%btypep
      stypep => srcFieldBundle%btypep

      ! TODO: this only works for all fields in the bundle being identical,
      ! including relloc.  if that is not true, we have to compute different
      ! weights for each field. 

      if ((condition .eq. ESMF_BUNDLECOMM_CONGRUENT) .and. bundlepack) then
          call ESMF_IArrayRegridStore( &
                                 stypep%flist(1)%ftypep%localfield%localdata, &
                                 stypep%igrid, &
                                 stypep%flist(1)%ftypep%mapping, &
                                 dtypep%flist(1)%ftypep%localfield%localdata, &
                                 dtypep%igrid, &
                                 dtypep%flist(1)%ftypep%mapping, &
                                 parentVM, routehandle, &
                                 1, ESMF_ALLTO1HANDLEMAP, 1, &
                                 regridMethod, regridNorm, srcMask, dstMask, &
                                 routeOptions, status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      else
        ! each field must be treated separately.
        do i=1, stypep%field_count
            call ESMF_IArrayRegridStore( &
                                 stypep%flist(i)%ftypep%localfield%localdata, &
                                 stypep%igrid, &
                                 stypep%flist(i)%ftypep%mapping, &
                                 dtypep%flist(i)%ftypep%localfield%localdata, &
                                 dtypep%igrid, &
                                 dtypep%flist(i)%ftypep%mapping, &
                                 parentVM, routehandle, &
                                 i, ESMF_1TO1HANDLEMAP, stypep%field_count, &
                                 regridMethod, regridNorm, srcMask, dstMask, &
                                 routeOptions, status)
            if (ESMF_LogMsgFoundError(status, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
        enddo
      endif

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleRegridStore


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleScatter"
!BOPI
! !IROUTINE: ESMF_FieldBundleScatter - Data scatter operation on a FieldBundle

! !INTERFACE:
      subroutine ESMF_FieldBundleScatter(array, sourceDE, bundle, &
                                    blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(inout) :: array
      integer, intent(in) :: sourceDE
      type(ESMF_FieldBundle), intent(inout) :: bundle                 
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!   Perform a scatter operation over the data
!   in an {\tt ESMF\_Array}, returning it as the data array 
!   in an {\tt ESMF\_FieldBundle}.  
!   If the FieldBundle is decomposed over N DEs, this routine
!   takes a single array on the specified DE and 
!   returns a decomposed copy
!   on each of the N DEs, as the {\tt ESMF\_Array} 
!   associated with the given empty {\tt ESMF\_FieldBundle}.
!
!   The arguments are:
!   \begin{description}
!   \item [array] 
!         Input {\tt ESMF\_Array} containing the collected data.
!         It must be the size of the entire undecomposed igrid.
!   \item [sourceDE]
!         Integer DE number where the data to be scattered 
!         is located.  The
!         {\tt ESMF\_Array} input is ignored on all other DEs.
!   \item [bundle] 
!         Empty FieldBundle containing {\tt ESMF\_IGrid} which will correspond to 
!         the data 
!         in the array which will be scattered.  When this routine returns
!         each {\tt ESMF\_FieldBundle} will contain a valid data array containing 
!         the subset of the decomposed data.
!   \item [{[blocking]}]
!         Optional argument which specifies whether the operation should
!         wait until complete before returning or return as soon
!         as the communication between DEs has been scheduled.
!         If not present, default is to do synchronous communications.
!         Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!         {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!   \item [{[commhandle]}]
!         If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!         argument is required.  Information about the pending operation
!         will be stored in the {\tt ESMF\_CommHandle} and can be queried
!         or waited for later.
!   \item [{[rc]}] 
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      type(ESMF_FieldBundleType) :: btypep             ! bundle type info
      !type(ESMF_AxisIndex) :: axis(ESMF_MAXDIM)   ! Size info for IGrid
      type(ESMF_DELayout) :: delayout          ! layout
      type(ESMF_InternArray) :: dstarray                ! Destination array
      integer :: i, datarank
      !integer :: thisdim, thislength, numDims
      integer :: dimorder(ESMF_MAXDIM)   
      integer :: dimlengths(ESMF_MAXDIM)   
      integer :: decomps(ESMF_MAXIGRIDDIM), decompids(ESMF_MAXDIM)
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      btypep = bundle%btypep

      ! Query the datamap and set info for igrid so it knows how to
      !  match up the array indices and the igrid indices.
      call ESMF_FieldDataMapGet(btypep%flist(1)%ftypep%mapping, &
                           dataIndexList=dimorder, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
!     call ESMF_IGridGet(btypep%igrid, decomps, rc=status)   !TODO
      !if (ESMF_LogMsgFoundError(status, &
      !                            ESMF_ERR_PASSTHRU, &
      !                            ESMF_CONTEXT, rc)) return
      decomps(1) = 1    ! TODO: remove this once the igrid call is created
      decomps(2) = 2

      ! And get the Array sizes
      call ESMF_InternArrayGet(btypep%flist(1)%ftypep%localfield%localdata, rank=datarank, &
                         counts=dimlengths, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      do i=1, datarank
        decompids(i) = dimorder(i)
        if(dimorder(i).ne.0) decompids(i) = decomps(dimorder(i))
      enddo

      ! Call Array method to perform actual work
      call ESMF_IGridGet(btypep%igrid, delayout=delayout, rc=status)
      call ESMF_IArrayScatter(array, delayout, decompids, sourceDE, dstarray, &
                             status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! TODO: do we need to set dimorder here?  should datamap be an input
      !  to this routine, or specified at create time?   or should this be
      !  a bundle create method?
      btypep%flist(1)%ftypep%localfield%localdata = dstarray

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleScatter


!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleCommPrepCheck"
!BOPI
! !IROUTINE: ESMF_FieldBundleCommPrepCheck - Validate 2 FieldBundles before Store call

! !INTERFACE:
      function ESMF_FieldBundleCommPrepCheck(srcFieldBundle, dstFieldBundle, rc)
!
! !RETURN VALUE:
      integer :: ESMF_FieldBundleCommPrepCheck
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: srcFieldBundle                 
      type(ESMF_FieldBundle), intent(inout) :: dstFieldBundle                 
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!   Internal function, called by RedistStore() and RegridStore() before
!   proceeding.  Validate both source and destination bundles, then
!   compare them to be sure they have the same number of fields, that the
!   fields line up (same data types, etc).  Return error condition if the
!   store operation cannot proceed;  otherwise return a code to indicate
!   either that the bundles are congruent (a single route table can apply
!   to all fields in the bundles) or that there are different types of
!   fields and each needs a separate route table.
!
!  Note: the arguments are intent(inout) only because the call to test
!   whether a bundle is congruent can actually set the internal flag as
!   a side-effect if the state isn't already known.
!
!   The arguments are:
!   \begin{description}
!   \item [srcFieldBundle] 
!         {\tt ESMF\_FieldBundle} to be validated and compared to destination.
!   \item [dstFieldBundle] 
!         {\tt ESMF\_FieldBundle} to be validated and compared to source.
!   \item [{[rc]}] 
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      integer :: i
      type(ESMF_FieldBundleType), pointer :: stypep, dtypep
      type(ESMF_IGridStorage) :: sigridStorage, digridStorage
      integer :: srank, drank
      type(ESMF_TypeKind) :: skind, dkind
   
      ! Initialize return code   
      ESMF_FieldBundleCommPrepCheck = ESMF_BUNDLECOMM_NOMATCH
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,srcFieldBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,dstFieldBundle,rc)

      ! Validate bundles before going further
      call ESMF_FieldBundleValidate(srcFieldBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_FieldBundleValidate(dstFieldBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      stypep => srcFieldBundle%btypep
      dtypep => dstFieldBundle%btypep

      ! Make sure both bundles have the same number of fields.
      if (stypep%field_count .ne. dtypep%field_count) then
        call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                 "FieldBundles must contain same number of Fields", &
                                 ESMF_CONTEXT, rc)
        return

      endif

      ! For now, make sure that none of the fields are empty.
      if ((stypep%field_count .le. 0) .or. (dtypep%field_count .le. 0)) then
        call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                 "FieldBundles must not be empty", &
                                  ESMF_CONTEXT, rc)
        return

      endif

      ! (TODO: we could put in some more code to handle empty fields but it
      ! probably would force us into the non-packing loop for sanity's sake).

      ! Make sure the data types are consistent. 
      do i=1, stypep%field_count
          call ESMF_InternArrayGet(stypep%flist(1)%ftypep%localfield%localdata, &
                             rank=srank, typekind=skind, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          call ESMF_InternArrayGet(dtypep%flist(1)%ftypep%localfield%localdata, &
                             rank=drank, typekind=dkind, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          
	  
        ! check if the rank of the source array and the destination array are 
        ! the same if the igrid distribution are both arbitrary or both block.
        ! Skip the checking if one is arbitrary and the other is block
	! ** P Li ** 10/17/2006
          call ESMF_IGridGet(stypep%igrid, igridStorage=sigridStorage, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          call ESMF_IGridGet(dtypep%igrid, igridStorage=digridStorage, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (sigridStorage.eq.digridStorage) then				    
            if (srank .ne. drank) then
              call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                  "Corresponding Fields in FieldBundles must have same data rank", &
                                       ESMF_CONTEXT, rc)
              return
            endif
	  endif

          if (skind .ne. dkind) then
              call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                  "Corresponding Fields in FieldBundles must have same typekind", &
                                       ESMF_CONTEXT, rc)
              return
          endif

      enddo

      ! Set return value based on whether both bundles are congruent or not.
      if (ESMF_FieldBundleIsCongruent(srcFieldBundle, rc=status) .and. &
          ESMF_FieldBundleIsCongruent(dstFieldBundle, rc=status)) then
              ESMF_FieldBundleCommPrepCheck = ESMF_BUNDLECOMM_CONGRUENT
      else
              ESMF_FieldBundleCommPrepCheck = ESMF_BUNDLECOMM_NONCONGRUENT
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_FieldBundleCommPrepCheck
!------------------------------------------------------------------------------

      end module ESMF_FieldBundleCommMod
