! $Id: ESMF_BundleComm.F90,v 1.33 2004/06/23 13:40:09 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
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
      use ESMF_BaseTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_VMMod
      use ESMF_DELayoutMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayMod
      use ESMF_RHandleMod
      use ESMF_RouteMod
      use ESMF_ArrayCommMod
      use ESMF_ArrayDataMapMod
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_FieldDataMapMod
      use ESMF_FieldMod
      use ESMF_BundleMod
      use ESMF_RegridMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
       private

!  <none>
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
      '$Id: ESMF_BundleComm.F90,v 1.33 2004/06/23 13:40:09 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
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
      type(ESMF_Array), intent(out) :: array
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
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: btypep    ! bundle type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      btypep => bundle%btypep

      ! Call Array method to perform actual work
      call ESMF_ArrayAllGather(btypep%flist(1)%ftypep%localfield%localdata, btypep%grid, &
                               btypep%flist(1)%ftypep%mapping, array, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleAllGather
#endif


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGather"
!BOPI
! !IROUTINE: ESMF_BundleGather - Data gather operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleGather(bundle, destinationDE, array, blocking, &
                                    commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle                 
      integer, intent(in) :: destinationDE
      type(ESMF_Array), intent(out) :: array
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
!     \item [array] 
!           Newly created {\tt ESMF\_Array} containing the collected data on the
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
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: btypep    ! bundle type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      btypep => bundle%btypep

      ! Call Array method to perform actual work
      call ESMF_ArrayGather(btypep%flist(1)%ftypep%localfield%localdata, &
                            btypep%grid, btypep%flist(1)%ftypep%mapping, &
                            destinationDE, array, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGather


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleHalo"
!BOPI
! !IROUTINE: ESMF_BundleHalo - Execute a halo operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleHalo(bundle, routehandle, blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_BlockingFlag), intent(in) , optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a halo operation over the data
!     in an {\tt ESMF\_Bundle}.  This routine updates the data 
!     inside the {\tt ESMF\_Bundle} in place.
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType) :: btypep              ! bundle type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      btypep = bundle%btypep

      call ESMF_ArrayHalo(btypep%flist(1)%ftypep%localfield%localdata, routehandle, &
                             blocking, commhandle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if(rcpresent) rc = ESMF_SUCCESS

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

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleHaloRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleHaloStore"
!BOPI
! !IROUTINE: ESMF_BundleHaloStore - Precompute a data halo operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleHaloStore(bundle, routehandle, halodirection, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_HaloDirection), intent(in), optional :: halodirection
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: btypep     ! bundle type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! Sanity checks for good bundle, and that it has an associated grid
      ! and data before going down to the next level.
      if (.not.associated(bundle%btypep)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

      btypep => bundle%btypep

      if (btypep%bundlestatus .ne. ESMF_STATUS_READY) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif


      call ESMF_ArrayHaloStore(btypep%flist(1)%ftypep%localfield%localdata, btypep%grid, &
                               btypep%flist(1)%ftypep%mapping, routehandle, &
                               halodirection, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleHaloStore

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRedist"
!BOPI
! !IROUTINE: ESMF_BundleRedist - Data redistribution operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRedist(srcBundle, dstBundle, &
                                   routehandle, blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcBundle
      type(ESMF_Bundle), intent(inout) :: dstBundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_BlockingFlag), intent(in) , optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType) :: stypep, dtypep      ! bundle type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      stypep = srcBundle%btypep
      dtypep = dstBundle%btypep

      call ESMF_ArrayRedist(stypep%flist(1)%ftypep%localfield%localdata, &
                            dtypep%flist(1)%ftypep%localfield%localdata, &
                            routehandle, blocking, commhandle, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleRedist


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRedistRelease"
!BOPI
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
!EOPI

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleRedistRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRedistStore"
!BOPI
! !IROUTINE: ESMF_BundleRedistStore - Data redistribution operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRedistStore(srcBundle, dstBundle, parentDElayout, &
                                        routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcBundle
      type(ESMF_Bundle), intent(inout) :: dstBundle
      type(ESMF_DELayout), intent(in) :: parentDElayout
      type(ESMF_RouteHandle), intent(out) :: routehandle
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
!     \item [parentDElayout]
!           {\tt ESMF\_DELayout} which encompasses both {\tt ESMF\_Bundle}s, 
!           most commonly the layout
!           of the Coupler if the redistribution is inter-component, 
!           but could also be the individual layout for a component if the 
!           redistribution is intra-component.  
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} which will be used to execute the
!           redistribution when {\tt ESMF\_BundleRedist} is called.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: stypep, dtypep
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! Sanity checks for good bundle, and that it has an associated grid
      ! and data before going down to the next level.
      if (.not.associated(dstBundle%btypep)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif
      if (.not.associated(srcBundle%btypep)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

      dtypep => dstBundle%btypep
      stypep => srcBundle%btypep

      if (dtypep%bundlestatus.ne.ESMF_STATUS_READY .or. &
          stypep%bundlestatus.ne.ESMF_STATUS_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call ESMF_ArrayRedistStore(stypep%flist(1)%ftypep%localfield%localdata, &
                                 stypep%grid, &
                                 stypep%flist(1)%ftypep%mapping, &
                                 dtypep%flist(1)%ftypep%localfield%localdata, &
                                 dtypep%grid, &
                                 dtypep%flist(1)%ftypep%mapping, &
                                 parentDElayout, &
                                 routehandle, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if(rcpresent) rc = ESMF_SUCCESS

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
      logical :: rcpresent                        ! Return code present
   
!     Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

!     Call Grid method to perform actual work
      !call ESMF_GridReduce(field%btypep%grid, &
      !                     field%btypep%flist(1)%ftypep%localfield%localdata, &
      !                     rtype, result, status)
      !if (ESMF_LogMsgFoundError(status, &
      !                            ESMF_ERR_PASSTHRU, &
      !                            ESMF_CONTEXT, rc)) return

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleReduce


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRegrid"
!BOPI
! !IROUTINE: ESMF_BundleRegrid - Execute a regrid operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRegrid(srcbundle, dstbundle, routehandle, &
                                   srcmask, dstmask, blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcbundle
      type(ESMF_Bundle), intent(inout) :: dstbundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_Mask), intent(in), optional :: srcmask
      type(ESMF_Mask), intent(in), optional :: dstmask
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
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
!     \item [{[srcmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!           (Not yet implemented.)
!     \item [{[dstmask]}]
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     


      !TODO: add code  here


      ! Set return values.
      !if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleRegrid


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

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleRegridRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRegridStore"
!BOPI
! !IROUTINE: ESMF_BundleRegridStore - Precompute regrid operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRegridStore(srcbundle, dstbundle, parentDElayout, &
                                        routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcbundle
      type(ESMF_Bundle), intent(inout) :: dstbundle
      type(ESMF_DELayout), intent(in) :: parentDElayout
      type(ESMF_RouteHandle), intent(out) :: routehandle
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
!     \item [srcbundle] 
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstbundle] 
!           {\tt ESMF\_Bundle} containing destination grid and data map.
!     \item [parentDElayout]
!           {\tt ESMF\_DELayout} which encompasses both {\tt ESMF\_Bundle}s, 
!           most commonly the layout
!           of the Coupler if the regridding is inter-component, but could 
!           also be the individual layout for a component if the 
!           regridding is intra-component.  
!     \item [routehandle]
!           Output from this call, identifies the precomputed work which
!           will be executed when {\tt ESMF\_FieldRegrid} is called.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     


      !TODO: add code  here


      ! Set return values.
      !if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleRegridStore


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleScatter"
!BOPI
! !IROUTINE: ESMF_BundleScatter - Data scatter operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleScatter(array, sourceDE, bundle, blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
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
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType) :: btypep             ! bundle type info
      !type(ESMF_AxisIndex) :: axis(ESMF_MAXDIM)   ! Size info for Grid
      type(ESMF_DELayout) :: delayout          ! layout
      type(ESMF_Array) :: dstarray                ! Destination array
      integer :: i, datarank
      !integer :: thisdim, thislength, numDims
      integer :: dimorder(ESMF_MAXDIM)   
      integer :: dimlengths(ESMF_MAXDIM)   
      integer :: decomps(ESMF_MAXGRIDDIM), decompids(ESMF_MAXDIM)
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

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
      call ESMF_ArrayGet(btypep%flist(1)%ftypep%localfield%localdata, rank=datarank, &
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
      call ESMF_ArrayScatter(array, delayout, decompids, sourceDE, dstarray, &
                             status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! TODO: do we need to set dimorder here?  should datamap be an input
      !  to this routine, or specified at create time?   or should this be
      !  a bundle create method?
      btypep%flist(1)%ftypep%localfield%localdata = dstarray

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleScatter


!------------------------------------------------------------------------------

      end module ESMF_BundleCommMod
