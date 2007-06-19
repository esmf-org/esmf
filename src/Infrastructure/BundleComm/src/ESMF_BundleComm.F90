! $Id: ESMF_BundleComm.F90,v 1.70 2007/06/19 21:23:39 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_BundleComm.F90"
!
!     ESMF Bundle Communications module
      module ESMF_BundleCommMod
!
!==============================================================================
!
! This file contains the Bundle communication methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_BundleCommMod - Communication routines for Bundle objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Bundle} class
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
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_FieldDataMapMod
      use ESMF_FieldMod
      use ESMF_FieldCommMod
      use ESMF_BundleMod
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
   public ESMF_BundleHaloStore, ESMF_BundleHalo, ESMF_BundleHaloRelease 
                             ! Redistribute existing arrays, matching grids
   public ESMF_BundleRedistStore, ESMF_BundleRedist, ESMF_BundleRedistRelease 
                             ! Regridding and interpolation, different grids
   public ESMF_BundleRegridStore, ESMF_BundleRegrid, ESMF_BundleRegridRelease 

   public ESMF_BundleGather   ! Combine 1 decomposed bundle into 1 on 1 DE
   !public ESMF_BundleAllGather! Combine 1 decomposed bundle into N copies on N DEs

   public ESMF_BundleScatter  ! Split 1 bundle into a decomposed one over N DEs
   !public ESMF_BundleBroadcast! Send 1 bundle to all DEs, none decomposed
   !public ESMF_BundleAlltoAll ! might make sense with bundles; each DE could
                              ! call with a different non-decomposed bundle
                              ! and the result would be a packed bundle of
                              ! data with decomposed bundle on each DE.

   public ESMF_BundleReduce     ! Global reduction operation, return on 1 DE
   !public ESMF_BundleAllReduce  ! Global reduction operation, return on each DE

!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_BundleComm.F90,v 1.70 2007/06/19 21:23:39 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleRedist - Redist data, either with a routehandle or not
!
! !INTERFACE:
      interface ESMF_BundleRedist

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleRedistAllinOne
        module procedure ESMF_BundleRedistRun

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
! !IROUTINE: ESMF_BundleRegrid - Regrid data, either with a routehandle or not
!
! !INTERFACE:
      interface ESMF_BundleRegrid

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleRegridAllinOne
        module procedure ESMF_BundleRegridRun

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
! Entry points for fuctionality which will happen mostly below at the Grid
!   level, but needs a Data Pointer as well as grid info to operate.
!   These include Reduction operations, Halo, and Transpose.
!
!------------------------------------------------------------------------------
#if 0
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleAllGather"
!BOPI
! !IROUTINE: ESMF_BundleAllGather - Data AllGather operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleAllGather(bundle, array, blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle                 
      type(ESMF_InternArray), intent(out) :: array
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform an allgather operation
!     over the data in an {\tt ESMF\_Bundle}.  If the {\tt ESMF\_Bundle} is
!     decomposed over N DEs, this routine returns a copy of the
!     entire collected data {\tt ESMF\_Array} on each of the N DEs.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_Bundle} containing data to be gathered.
!     \item [array] 
!           Newly created array containing the collected data.
!           It is the size of the entire undecomposed grid.
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
      type(ESMF_BundleType), pointer :: btypep    ! bundle type info
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      btypep => bundle%btypep

      ! Call Array method to perform actual work
      call ESMF_IArrayAllGather(btypep%flist(1)%ftypep%localfield%localdata, btypep%grid, &
                               btypep%flist(1)%ftypep%mapping, array, status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleAllGather
#endif


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGather"
!BOPI
! !IROUTINE: ESMF_BundleGather - Data gather operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleGather(bundle, destinationDE, arrayList, blocking, &
                                    commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle                 
      integer, intent(in) :: destinationDE
      type(ESMF_InternArray), intent(out) :: arrayList(:)
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a gather operation
!     over the data in an {\tt ESMF\_Bundle}.  If the {\tt ESMF\_Bundle} is
!     decomposed over N DEs, this routine returns a copy of the
!     entire collected data as an {\tt ESMF\_Array} 
!     on the specified destination
!     DE number.  On all other DEs there is no return {\tt array} value.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_Bundle} containing data to be gathered.
!     \item [destinationDE] 
!           Destination DE number where the gathered data is to be returned.
!     \item [arrayList] 
!           Newly created list of {\tt ESMF\_Array}s, one per {\tt ESMF\_Field}
!           in the {\tt ESMF\_Bundle}, containing the collected data on the
!           specified DE.  It is the size of the entire undecomposed grid.
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
      type(ESMF_BundleType), pointer :: btypep    ! bundle type info
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      btypep => bundle%btypep

      do i=1, btypep%field_count
          ! Call Array method to perform actual work
          call ESMF_IArrayGather(btypep%flist(i)%ftypep%localfield%localdata, &
                                btypep%grid, btypep%flist(i)%ftypep%mapping, &
                                destinationDE, arrayList(i), status)
          if (ESMF_LogMsgFoundError(status, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
      enddo

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGather


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleHalo"
!BOPI
! !IROUTINE: ESMF_BundleHalo - Execute a halo operation on each Field in a Bundle

! !INTERFACE:
      subroutine ESMF_BundleHalo(bundle, routehandle, blocking, &
                                 commhandle, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_BlockingFlag), intent(in) , optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a halo operation over each {\tt ESMF\_Field}
!     in an {\tt ESMF\_Bundle}.  This routine updates the data 
!     inside the {\tt ESMF\_Bundle} in place.  The current version
!     of the code does not support {\tt ESMF\_Bundle}s with packed data.
!     It simply operates on a {\tt ESMF\_Field} by {\tt ESMF\_Field}
!     basis, updating each one at a time.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_Bundle} containing data to be haloed.
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} which was returned by the corresponding
!           {\tt ESMF\_BundleHaloStore()} call. It is associated with 
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
      type(ESMF_BundleType), pointer :: btypep    ! bundle type info
      type(ESMF_InternArray), allocatable :: arrayList(:)
      integer :: maptype
      logical :: bundlepack
      integer :: bopt, bflag, rcount

   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if(present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
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
                  "Bundles do not match Bundles used in BundleHaloStore", &
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
        if (.not. ESMF_BundleIsCongruent(bundle, rc=status)) then
          ! problem - the map was computed with a congruent bundle
          ! and this one is not.  error out and return. 
          call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                  "BundleHaloStore() was called with an incompatible Bundle", &
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

      end subroutine ESMF_BundleHalo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleHaloRelease"
!BOPI
! !IROUTINE: ESMF_BundleHaloRelease - Release resources associated w/ handle

! !INTERFACE:
      subroutine ESMF_BundleHaloRelease(routehandle, rc)
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

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleHaloRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleHaloStore"
!BOPI
! !IROUTINE: ESMF_BundleHaloStore - Precompute a data halo operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleHaloStore(bundle, routehandle, halodirection, &
                                      routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_HaloDirection), intent(in), optional :: halodirection
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Precompute the data movement or communication operations needed 
!     to perform a halo operation over the data in an {\tt ESMF\_Bundle}. 
!     The list of operations will be associated internally to the
!     framework with the 
!     {\tt ESMF\_RouteHandle} object.  
!     To perform the actual halo operation
!     the {\tt ESMF\_BundleHalo()} routine must be called with the
!     {\tt ESMF\_Bundle} containing the data to be updated and the 
!     {\tt ESMF\_RouteHandle} computed during this store call.
!     Although probably less common with bundles than with fields,
!     if more than one {\tt ESMF\_Bundle} has an identical 
!     {\tt ESMF\_Grid} and contains identical {\tt ESMF\_Field}s, then
!     the same {\tt ESMF\_RouteHandle} can be computed once and used
!     in multiple executions of the halo operation.
!     In the current version of the code {\tt ESMF\_Bundle}s with
!     packed data are not supported.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_Bundle} containing data to be haloed.
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} which will be returned after being
!           associated with the precomputed 
!           information for a halo operation on this {\tt ESMF\_Bundle}.
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
      type(ESMF_BundleType), pointer :: btypep     ! bundle type info
      logical :: bundlepack
      integer :: bopt, bflag
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
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
      if (ESMF_BundleIsCongruent(bundle, rc=status) .and. bundlepack) then
        call ESMF_IArrayHaloStore( &
                                  btypep%flist(1)%ftypep%localfield%localdata, &
                                  btypep%flist(1)%ftypep%localfield%localFlag, &
                                  1, ESMF_ALLTO1HANDLEMAP, 1, &
                                  btypep%grid, &
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
                                  btypep%grid, &
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

      end subroutine ESMF_BundleHaloStore

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRedist"
!BOP
! !IROUTINE: ESMF_BundleRedist - Data redistribution operation on a Bundle

! !INTERFACE:
      ! Private name; call using ESMF_BundleRedist()
      subroutine ESMF_BundleRedistAllinOne(srcBundle, dstBundle, parentVM, &
                                   blocking, commhandle, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: srcBundle
      type(ESMF_Bundle), intent(inout) :: dstBundle
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_BlockingFlag), intent(in) , optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a redistribution operation over the data
!     in an {\tt ESMF\_Bundle}.  This version does not take a {\tt routehandle}
!     and computes, runs, and releases the communication information in a
!     single subroutine.  It should be used when a redist operation will be
!     done only a single time; otherwise computing and reusing a communication
!     pattern will be more efficient. 
!     This routine reads the source bundle and leaves 
!     the data untouched.  It reads the {\tt ESMF\_Grid} and 
!     {\tt ESMF\_FieldDataMap}
!     from the destination bundle and updates the array data in the destination.
!     The {\tt ESMF\_Grid}s may have different decompositions (different
!     {\tt ESMF\_DELayout}s) or different data maps, but the source and
!     destination grids must describe the same set of coordinates.
!     Unlike {\tt ESMF\_BundleRegrid} this routine does not do interpolation,
!     only data movement.
!
!     The arguments are:
!     \begin{description}
!     \item [srcBundle] 
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstBundle] 
!           {\tt ESMF\_Bundle} containing destination grid.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Bundle}s,
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
      type(ESMF_BundleType) :: stypep, dtypep      ! bundle type info
      type(ESMF_RouteHandle) :: routehandle
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,srcBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,dstBundle,rc)

      stypep = srcBundle%btypep
      dtypep = dstBundle%btypep
      routehandle = ESMF_RouteHandleCreate(status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call BundleRedistStore
      call ESMF_BundleRedistStore(srcBundle, dstBundle, parentVM, &
                                  routehandle, routeOptions, status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call BundleRedistRun
      call ESMF_BundleRedistRun(srcBundle, dstBundle, routehandle, &
                                blocking, commhandle, routeOptions, status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call BundleRedistRelease
      call ESMF_BundleRedistRelease(routehandle, status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleRedistAllinOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRedist"
!BOP
! !IROUTINE: ESMF_BundleRedist - Data redistribution operation on a Bundle

! !INTERFACE:
      ! Private name; call using ESMF_FieldRedist()
      subroutine ESMF_BundleRedistRun(srcBundle, dstBundle, routehandle, &
                                   blocking, commhandle, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: srcBundle
      type(ESMF_Bundle), intent(inout) :: dstBundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_BlockingFlag), intent(in) , optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a redistribution operation over the data
!     in an {\tt ESMF\_Bundle}.  
!     This routine reads the source bundle and leaves 
!     the data untouched.  It reads the {\tt ESMF\_Grid} and 
!     {\tt ESMF\_FieldDataMap}
!     from the destination bundle and updates the array data in the destination.
!     The {\tt ESMF\_Grid}s may have different decompositions (different
!     {\tt ESMF\_DELayout}s) or different data maps, but the source and
!     destination grids must describe the same set of coordinates.
!     Unlike {\tt ESMF\_BundleRegrid} this routine does not do interpolation,
!     only data movement.
!
!     The arguments are:
!     \begin{description}
!     \item [srcbundle] 
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstbundle] 
!           {\tt ESMF\_Bundle} containing destination grid.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} which was returned by the corresponding
!           {\tt ESMF\_BundleRedistStore()} call. It is associated with
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
      type(ESMF_BundleType) :: stypep, dtypep      ! bundle type info
      type(ESMF_InternArray), allocatable :: srcArrayList(:), dstArrayList(:)
      integer :: condition
      integer :: maptype
      logical :: bundlepack
      integer :: bopt, bflag, rcount
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,srcBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,dstBundle,rc)

      stypep = srcBundle%btypep
      dtypep = dstBundle%btypep

      ! Does validate of both bundles and checks for consistent types.
      condition = ESMF_BundleCommPrepCheck(srcBundle, dstBundle, rc=status)
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
                "RouteHandle and Bundles do not have matching Field counts", &
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
                  "Bundles do not match Bundles used in BundleRedistStore()", &
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

      end subroutine ESMF_BundleRedistRun

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRedistRelease"
!BOP
! !IROUTINE: ESMF_BundleRedistRelease - Release resources associated with handle

! !INTERFACE:
      subroutine ESMF_BundleRedistRelease(routehandle, rc)
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


      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleRedistRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRedistStore"
!BOP
! !IROUTINE: ESMF_BundleRedistStore - Data redistribution operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRedistStore(srcBundle, dstBundle, parentVM, &
                                        routehandle, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: srcBundle
      type(ESMF_Bundle), intent(inout) :: dstBundle
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_RouteHandle), intent(out) :: routehandle
      type(ESMF_RouteOptions), intent(in), optional :: routeOptions
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Precompute the data movement or communications operations needed to
!     accomplish a data redistribution operation over the data
!     in an {\tt ESMF\_Bundle}.  Data redistribution differs from regridding
!     in that redistribution does no interpolation, only a 1-for-1 movement
!     of data from one location to another.
!     Therefore, while 
!     the {\tt ESMF\_Grid}s for the source and destination may have
!     different decompositions (different {\tt ESMF\_DELayout}s) 
!     or different data maps, the source and destination grids 
!     must describe the same set of coordinates.
!
!     The arguments are:
!     \begin{description}
!     \item [srcBundle]
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstBundle]
!           {\tt ESMF\_Bundle} containing destination grid.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Bundle}s, 
!           most commonly the VM
!           of the Coupler if the redistribution is inter-component, 
!           but could also be the individual VM for a component if the 
!           redistribution is intra-component.  
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} which will be used to execute the
!           redistribution when {\tt ESMF\_BundleRedist} is called.
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
      type(ESMF_BundleType), pointer :: stypep, dtypep
      logical :: bundlepack
      integer :: bopt, bflag
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,srcBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,dstBundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(srcBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_BundleValidate(dstBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

 
      ! validate both bundles, make sure they have the same number
      ! of fields, make sure the data types are consistent, etc.

      condition = ESMF_BundleCommPrepCheck(srcBundle, dstBundle, rc=status)
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

      dtypep => dstBundle%btypep
      stypep => srcBundle%btypep

      ! if all fields are identical, they can share a route
      if ((condition .eq. ESMF_BUNDLECOMM_CONGRUENT) .and. bundlepack) then 
         call ESMF_IArrayRedistStore( &
                                 stypep%flist(1)%ftypep%localfield%localdata, &
                                 stypep%flist(1)%ftypep%localfield%localFlag, &
                                 stypep%grid, &
                                 stypep%flist(1)%ftypep%mapping, &
                                 dtypep%flist(1)%ftypep%localfield%localdata, &
                                 dtypep%flist(1)%ftypep%localfield%localFlag, &
                                 dtypep%grid, &
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
                               stypep%grid, &
                               stypep%flist(i)%ftypep%mapping, &
                               dtypep%flist(i)%ftypep%localfield%localdata, &
                               dtypep%flist(i)%ftypep%localfield%localFlag, &
                               dtypep%grid, &
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

      end subroutine ESMF_BundleRedistStore


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleReduce"
!BOPI
! !IROUTINE: ESMF_BundleReduce - Reduction operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleReduce(bundle, rtype, result, blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle) :: bundle                 
      integer :: rtype
      integer :: result
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a reduction operation over the data in an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_Bundle} containing data to be reduced.
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

      ! Call Grid method to perform actual work??  TODO: is this right?
      !call ESMF_GridReduce(field%btypep%grid, &
      !                     field%btypep%flist(1)%ftypep%localfield%localdata, &
      !                     rtype, result, status)
      !if (ESMF_LogMsgFoundError(status, &
      !                            ESMF_ERR_PASSTHRU, &
      !                            ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleReduce

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRegrid"
!BOP
! !IROUTINE: ESMF_BundleRegrid - Execute a regrid operation on a Bundle

! !INTERFACE:
      ! Private name; call using ESMF_BundleRegrid()
      subroutine ESMF_BundleRegridAllinOne(srcBundle, dstBundle, parentVM, &
                                   regridMethod, regridNorm, &
                                   srcMask, dstMask, blocking, commhandle, &
                                   routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: srcBundle
      type(ESMF_Bundle), intent(inout) :: dstBundle
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
!     in an {\tt ESMF\_Bundle}.  This routine reads the source bundle and 
!     leaves the data untouched.  It uses the {\tt ESMF\_Grid} and
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
!     \item [srcBundle] 
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstBundle] 
!           {\tt ESMF\_Bundle} containing destination grid.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Bundle}s, 
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
      type(ESMF_BundleType) :: stypep, dtypep      ! bundle type info
      type(ESMF_RouteHandle) :: routehandle
   
      ! Initialize return code   
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,srcBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,dstBundle,rc)

      stypep = srcBundle%btypep
      dtypep = dstBundle%btypep
      routehandle = ESMF_RouteHandleCreate(localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call BundleRegridStore
      call ESMF_BundleRegridStore(srcBundle, dstBundle, parentVM, &
                                  routehandle, regridMethod, regridNorm, &
                                  srcMask, dstMask, routeOptions, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call BundleRegridRun
      call ESMF_BundleRegridRun(srcBundle, dstBundle, routehandle, &
                                srcMask, dstMask, blocking, commhandle, &
                                routeOptions, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call BundleRegridRelease
      call ESMF_BundleRegridRelease(routehandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleRegridAllinOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRegrid"
!BOPI
! !IROUTINE: ESMF_BundleRegrid - Execute a regrid operation on a Bundle

! !INTERFACE:
      ! Private name; call using ESMF_BundleRegrid()
      subroutine ESMF_BundleRegridRun(srcBundle, dstBundle, routehandle, &
                                   srcMask, dstMask, blocking, commhandle, &
                                   routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: srcBundle
      type(ESMF_Bundle), intent(inout) :: dstBundle
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
!     in an {\tt ESMF\_Bundle}.  This routine reads the source bundle and 
!     leaves the data untouched.  It uses the {\tt ESMF\_Grid} and
!     {\tt ESMF\_FieldDataMap} information in the destination bundle to
!     control the transformation of data.  The array data in the 
!     destination bundle is overwritten by this call.
!
!     The arguments are:
!     \begin{description}
!     \item [srcbundle] 
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstbundle] 
!           {\tt ESMF\_Bundle} containing destination grid and data map.
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
      type(ESMF_BundleType) :: stypep, dtypep      ! bundle type info
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
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,srcBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,dstBundle,rc)

      ! Does validate of both bundles and checks for consistent types.
      condition = ESMF_BundleCommPrepCheck(srcBundle, dstBundle, rc=status)
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
   
      stypep = srcBundle%btypep
      dtypep = dstBundle%btypep

      
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
                  "Bundles do not match Bundles used in BundleRegridStore", &
                   ESMF_CONTEXT, rc)
              return
          endif

          do i = 1, stypep%field_count
            hasSrcData = ESMF_RegridHasData(stypep%grid, &
                                          stypep%flist(i)%ftypep%mapping)
            hasDstData = ESMF_RegridHasData(dtypep%grid, &
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
                  "Bundles do not match Bundles used in BundleRegridStore()", &
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
        
              hasSrcData = ESMF_RegridHasData(stypep%grid, &
                                              stypep%flist(1)%ftypep%mapping)
              hasDstData = ESMF_RegridHasData(dtypep%grid, &
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
                hasSrcData = ESMF_RegridHasData(stypep%grid, &
                                              stypep%flist(i)%ftypep%mapping)
                hasDstData = ESMF_RegridHasData(dtypep%grid, &
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

      end subroutine ESMF_BundleRegridRun


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRegridRelease"
!BOPI
! !IROUTINE: ESMF_BundleRegridRelease - Release information for this handle

! !INTERFACE:
      subroutine ESMF_BundleRegridRelease(routehandle, rc)
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

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleRegridRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRegridStore"
!BOPI
! !IROUTINE: ESMF_BundleRegridStore - Precompute regrid operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRegridStore(srcBundle, dstBundle, parentVM, &
                                        routehandle, regridMethod, regridNorm, &
                                        srcMask, dstMask, routeOptions, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: srcBundle
      type(ESMF_Bundle), intent(inout) :: dstBundle
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
!     \item [srcBundle] 
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstBundle] 
!           {\tt ESMF\_Bundle} containing destination grid and data map.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Bundle}s, 
!           most commonly the VM
!           of the Coupler if the regridding is inter-component, but could 
!           also be the individual VM for a component if the 
!           regridding is intra-component.  
!     \item [routehandle]
!           Output from this call, identifies the precomputed work which
!           will be executed when {\tt ESMF\_BundleRegrid} is called.
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
      type(ESMF_BundleType), pointer :: stypep, dtypep
      integer :: condition
      logical :: bundlepack
      integer :: bopt, bflag
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,srcBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,dstBundle,rc)

      ! Does validate of both bundles and checks for consistent types.
      condition = ESMF_BundleCommPrepCheck(srcBundle, dstBundle, rc=status)
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

      dtypep => dstBundle%btypep
      stypep => srcBundle%btypep

      ! TODO: this only works for all fields in the bundle being identical,
      ! including relloc.  if that is not true, we have to compute different
      ! weights for each field. 

      if ((condition .eq. ESMF_BUNDLECOMM_CONGRUENT) .and. bundlepack) then
          call ESMF_IArrayRegridStore( &
                                 stypep%flist(1)%ftypep%localfield%localdata, &
                                 stypep%grid, &
                                 stypep%flist(1)%ftypep%mapping, &
                                 dtypep%flist(1)%ftypep%localfield%localdata, &
                                 dtypep%grid, &
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
                                 stypep%grid, &
                                 stypep%flist(i)%ftypep%mapping, &
                                 dtypep%flist(i)%ftypep%localfield%localdata, &
                                 dtypep%grid, &
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

      end subroutine ESMF_BundleRegridStore


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleScatter"
!BOPI
! !IROUTINE: ESMF_BundleScatter - Data scatter operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleScatter(array, sourceDE, bundle, &
                                    blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(inout) :: array
      integer, intent(in) :: sourceDE
      type(ESMF_Bundle), intent(inout) :: bundle                 
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!   Perform a scatter operation over the data
!   in an {\tt ESMF\_Array}, returning it as the data array 
!   in an {\tt ESMF\_Bundle}.  
!   If the Bundle is decomposed over N DEs, this routine
!   takes a single array on the specified DE and 
!   returns a decomposed copy
!   on each of the N DEs, as the {\tt ESMF\_Array} 
!   associated with the given empty {\tt ESMF\_Bundle}.
!
!   The arguments are:
!   \begin{description}
!   \item [array] 
!         Input {\tt ESMF\_Array} containing the collected data.
!         It must be the size of the entire undecomposed grid.
!   \item [sourceDE]
!         Integer DE number where the data to be scattered 
!         is located.  The
!         {\tt ESMF\_Array} input is ignored on all other DEs.
!   \item [bundle] 
!         Empty Bundle containing {\tt ESMF\_Grid} which will correspond to 
!         the data 
!         in the array which will be scattered.  When this routine returns
!         each {\tt ESMF\_Bundle} will contain a valid data array containing 
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
      type(ESMF_BundleType) :: btypep             ! bundle type info
      !type(ESMF_AxisIndex) :: axis(ESMF_MAXDIM)   ! Size info for Grid
      type(ESMF_DELayout) :: delayout          ! layout
      type(ESMF_InternArray) :: dstarray                ! Destination array
      integer :: i, datarank
      !integer :: thisdim, thislength, numDims
      integer :: dimorder(ESMF_MAXDIM)   
      integer :: dimlengths(ESMF_MAXDIM)   
      integer :: decomps(ESMF_MAXGRIDDIM), decompids(ESMF_MAXDIM)
   
      ! Initialize return code   
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      btypep = bundle%btypep

      ! Query the datamap and set info for grid so it knows how to
      !  match up the array indices and the grid indices.
      call ESMF_FieldDataMapGet(btypep%flist(1)%ftypep%mapping, &
                           dataIndexList=dimorder, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
!     call ESMF_GridGet(btypep%grid, decomps, rc=status)   !TODO
      !if (ESMF_LogMsgFoundError(status, &
      !                            ESMF_ERR_PASSTHRU, &
      !                            ESMF_CONTEXT, rc)) return
      decomps(1) = 1    ! TODO: remove this once the grid call is created
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
      call ESMF_GridGet(btypep%grid, delayout=delayout, rc=status)
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

      end subroutine ESMF_BundleScatter


!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleCommPrepCheck"
!BOPI
! !IROUTINE: ESMF_BundleCommPrepCheck - Validate 2 Bundles before Store call

! !INTERFACE:
      function ESMF_BundleCommPrepCheck(srcBundle, dstBundle, rc)
!
! !RETURN VALUE:
      integer :: ESMF_BundleCommPrepCheck
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: srcBundle                 
      type(ESMF_Bundle), intent(inout) :: dstBundle                 
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
!   \item [srcBundle] 
!         {\tt ESMF\_Bundle} to be validated and compared to destination.
!   \item [dstBundle] 
!         {\tt ESMF\_Bundle} to be validated and compared to source.
!   \item [{[rc]}] 
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      integer :: i
      type(ESMF_BundleType), pointer :: stypep, dtypep
      type(ESMF_GridStorage) :: sgridStorage, dgridStorage
      integer :: srank, drank
      type(ESMF_TypeKind) :: skind, dkind
   
      ! Initialize return code   
      ESMF_BundleCommPrepCheck = ESMF_BUNDLECOMM_NOMATCH
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variable
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,srcBundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,dstBundle,rc)

      ! Validate bundles before going further
      call ESMF_BundleValidate(srcBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_BundleValidate(dstBundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      stypep => srcBundle%btypep
      dtypep => dstBundle%btypep

      ! Make sure both bundles have the same number of fields.
      if (stypep%field_count .ne. dtypep%field_count) then
        call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                 "Bundles must contain same number of Fields", &
                                 ESMF_CONTEXT, rc)
        return

      endif

      ! For now, make sure that none of the fields are empty.
      if ((stypep%field_count .le. 0) .or. (dtypep%field_count .le. 0)) then
        call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                 "Bundles must not be empty", &
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
        ! the same if the grid distribution are both arbitrary or both block.
        ! Skip the checking if one is arbitrary and the other is block
	! ** P Li ** 10/17/2006
          call ESMF_GridGet(stypep%grid, gridStorage=sgridStorage, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          call ESMF_GridGet(dtypep%grid, gridStorage=dgridStorage, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (sgridStorage.eq.dgridStorage) then				    
            if (srank .ne. drank) then
              call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                  "Corresponding Fields in Bundles must have same data rank", &
                                       ESMF_CONTEXT, rc)
              return
            endif
	  endif

          if (skind .ne. dkind) then
              call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                  "Corresponding Fields in Bundles must have same typekind", &
                                       ESMF_CONTEXT, rc)
              return
          endif

      enddo

      ! Set return value based on whether both bundles are congruent or not.
      if (ESMF_BundleIsCongruent(srcBundle, rc=status) .and. &
          ESMF_BundleIsCongruent(dstBundle, rc=status)) then
              ESMF_BundleCommPrepCheck = ESMF_BUNDLECOMM_CONGRUENT
      else
              ESMF_BundleCommPrepCheck = ESMF_BUNDLECOMM_NONCONGRUENT
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_BundleCommPrepCheck
!------------------------------------------------------------------------------

      end module ESMF_BundleCommMod
