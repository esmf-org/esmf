! $Id: ESMF_BundleComm.F90,v 1.19 2004/04/19 21:55:32 nscollins Exp $
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
      use ESMF_BaseMod
      use ESMF_IOSpecMod
      use ESMF_newDELayoutMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayMod
      use ESMF_RHandleMod
      use ESMF_RouteMod
      use ESMF_ArrayCommMod
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_DataMapMod
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
      '$Id: ESMF_BundleComm.F90,v 1.19 2004/04/19 21:55:32 nscollins Exp $'

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
!BOP
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
!     Perform a {\tt ESMF\_AllGather} operation
!     over the data in a {\tt ESMF\_Bundle}.  If the {\tt ESMF\_Bundle} is
!     decomposed over N {\tt ESMF\_DE}s, this routine returns a copy of the
!     entire collected data {\tt ESMF\_Array} on each of the N {\tt ESMF\_DE}s.
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
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
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
!EOP
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
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleGather: Array Gather returned failure"
        return
      endif 

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleAllGather
#endif


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGather - Data Gather operation on a Bundle

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
!     Call {\tt ESMF\_Grid} routines to perform a {\tt ESMF\_Gather} operation
!     over the data in a {\tt ESMF\_Bundle}.  If the {\tt ESMF\_Bundle} is
!     decomposed over N {\tt ESMF\_DE}s, this routine returns a copy of the
!     entire collected data {\tt ESMF\_Array} on the specified destination
!     {\tt ESMF\_DE} number.  On all other {\tt ESMF\_DE}s, there is no return
!     {\tt ESMF\_Array}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_Bundle} containing data to be gathered.
!     \item [destinationDE] 
!           Destination {\tt ESMF\_DE} number where the Gathered Array is to be returned.
!     \item [array] 
!           Newly created array containing the collected data on the
!           specified {\tt ESMF\_DE}.  It is the size of the entire undecomposed grid.
!           On all other {\tt ESMF\_DE}s this return is an invalid object.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
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
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleGather: Array Gather returned failure"
        return
      endif 

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGather


!------------------------------------------------------------------------------
!BOP
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
!     Perform a Reduction operation over the data in a {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           Bundle containing data to be reduced.
!     \item [rtype]
!           Type of reduction operation to perform.  Options include: ...
!     \item [result] 
!           Numeric result (may be single number, may be array)
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
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
      !if(status .NE. ESMF_SUCCESS) then 
      !  print *, "ERROR in BundleReduce: Grid reduce"
      !  return
      !endif 

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleReduce


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleScatter - Data Scatter operation on a Bundle

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
!     Perform a Scatter operation over the data
!     in an {\tt ESMF\_Array}, returning it as the data array in a {\tt ESMF\_Bundle}.  
!     If the Bundle is decomposed over N {\tt ESMF\_DE}s, this routine
!     takes a single array on the specified {\tt ESMF\_DE} and returns a decomposed copy
!     on each of the N {\tt ESMF\_DE}s, as the {\tt ESMF\_Array} associated with the given empty {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [array] 
!           Input {\tt ESMF\_Array} containing the collected data.
!           It must be the size of the entire undecomposed grid.
!     \item [sourceDE]
!           Integer {\tt ESMF\_DE} number where the data to be Scattered is located.  The
!           {\tt ESMF\_Array} input is ignored on all other {\tt ESMF\_DE}s.
!     \item [bundle] 
!           Empty Bundle containing {\tt ESMF\_Grid} which will correspond to the data 
!           in the array which will be scattered.  When this routine returns
!           each {\tt ESMF\_Bundle} will contain a valid data array containing the 
!           subset of the decomposed data.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType) :: btypep             ! bundle type info
      type(ESMF_AxisIndex) :: axis(ESMF_MAXDIM)   ! Size info for Grid
      type(ESMF_newDELayout) :: delayout          ! layout
      type(ESMF_Array) :: dstarray                ! Destination array
      integer :: i, datarank, thisdim, thislength, numDims
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
      call ESMF_DataMapGet(btypep%flist(1)%ftypep%mapping, &
                           dataIorder=dimorder, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleScatter: DataMapGet returned failure"
        return
      endif 
!     call ESMF_GridGet(btypep%grid, decomps, rc=status)   !TODO
!     if(status .NE. ESMF_SUCCESS) then 
!       print *, "ERROR in BundleScatter: GridGet returned failure"
!       return
!     endif 
      decomps(1) = 1    ! TODO: remove this once the grid call is created
      decomps(2) = 2

      ! And get the Array sizes
      call ESMF_ArrayGet(btypep%flist(1)%ftypep%localfield%localdata, rank=datarank, &
                         counts=dimlengths, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleGather: ArrayGet returned failure"
        return
      endif 

      do i=1, datarank
        decompids(i) = dimorder(i)
        if(dimorder(i).ne.0) decompids(i) = decomps(dimorder(i))
      enddo

      ! Call Array method to perform actual work
      call ESMF_GridGet(btypep%grid, delayout=delayout, rc=status)
      call ESMF_ArrayScatter(array, delayout, decompids, sourceDE, dstarray, &
                             status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleScatter: Array Scatter returned failure"
        return
      endif 

      ! TODO: do we need to set dimorder here?  should datamap be an input
      !  to this routine, or specified at create time?   or should this be
      !  a bundle create method?
      btypep%flist(1)%ftypep%localfield%localdata = dstarray

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleScatter


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleHalo - Execute a Data Halo operation on a Bundle

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
!     Perform a {\tt Halo} operation over the data
!     in an {\tt ESMF\_Bundle}.  This routine updates the data 
!     inside the {\tt ESMF\_Bundle} in place.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_Bundle} containing data to be halo'd.
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} containing index of precomputed information
!           about this Halo.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is what was specified at Store time.
!           If {\tt both} was specified at Store time, this defaults to  
!           blocking.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
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
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleHalo: ArrayHalo returned failure"
        return
      endif 

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleHalo

!------------------------------------------------------------------------------
!BOP
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
!     Release all stored information about the Halo associated
!     with this {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this Bundle Halo.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleHaloRelease

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleHaloStore - Precompute a Data Halo operation on a Bundle

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
!     Perform a {\tt Halo} operation over the data
!     in an {\tt ESMF\_Bundle}.  This routine updates the data 
!     inside the {\tt ESMF\_Bundle} in place.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_Bundle} containing data to be halo'd.
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} containing index to precomputed 
!           information for the Halo operation on this {\tt ESMF\_Bundle}.
!           This handle must be supplied at run time to execute the Halo.
!     \item [{halodirection]}]
!           Optional argument to restrict halo direction to a subset of the
!           possible halo directions.  If not specified, the halo is executed
!           along all boundaries.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
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
        print *, "Invalid or Destroyed Bundle"
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif

      btypep => bundle%btypep

      if (btypep%bundlestatus .ne. ESMF_STATE_READY) then
        print *, "Bundle not ready"
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif


      call ESMF_ArrayHaloStore(btypep%flist(1)%ftypep%localfield%localdata, btypep%grid, &
                               btypep%flist(1)%ftypep%mapping, routehandle, &
                               halodirection, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleHaloStore: ArrayHaloStore returned failure"
        return
      endif 

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleHaloStore

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleRedist - Data Redistribution operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRedist(srcBundle, dstBundle, parentDElayout, &
                                   routehandle, blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcBundle
      type(ESMF_Bundle), intent(inout) :: dstBundle
      type(ESMF_newDELayout), intent(in) :: parentDElayout
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_BlockingFlag), intent(in) , optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a {\tt Redistribution} operation over the data
!     in a {\tt ESMF\_Bundle}.  This routine reads the source bundle and leaves 
!     the data untouched.  It reads the {\t ESMF\_Grid} and {\tt ESMF\_DataMap}
!     from the destination bundle and updates the array data in the destination.
!     The {\tt ESMF\_Grid}s may have different decompositions (different
!     {\tt ESMF\_DELayout}s) or different data maps, but the source and
!     destination grids must describe the same set of coordinates.
!     Unlike {\tt ESMF\_Regrid} this routine does not do interpolation,
!     only data movement.
!
!     The arguments are:
!     \begin{description}
!     \item [srcbundle] 
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstbundle] 
!           {\tt ESMF\_Bundle} containing destination grid.
!     \item [parentDElayout]
!           {\tt ESMF\_DELayout} which encompasses both {\tt ESMF\_Bundle}s, 
!           most commonly the layout
!           of the Coupler if the redistribution is inter-component, 
!           but could also be the individual layout for a component if the 
!           redistribution is intra-component.  
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communication.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
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
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in BundleRedist: ArrayRedist returned failure"
        return
      endif

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleRedist


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleRedistRelease - Release resources associated w/ handle

! !INTERFACE:
      subroutine ESMF_BundleRedistRelease(routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Release all stored information about the Redist associated
!     with this {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this Bundle Redist.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleRedistRelease

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleRedistStore - Data Redistribution operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRedistStore(srcBundle, dstBundle, parentDElayout, &
                                        routehandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcBundle
      type(ESMF_Bundle), intent(inout) :: dstBundle
      type(ESMF_newDELayout), intent(in) :: parentDElayout
      type(ESMF_RouteHandle), intent(out) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Precompute a {\tt Redistribution} operation over the data
!     in a {\tt ESMF\_Bundle}.  This routine reads the source bundle and leaves 
!     the data untouched.  It reads the {\t ESMF\_Grid} and {\tt ESMF\_DataMap}
!     from the destination bundle and updates the array data in the destination.
!     The {\tt ESMF\_Grid}s may have different decompositions (different
!     {\tt ESMF\_DELayout}s) or different data maps, but the source and
!     destination grids must describe the same set of coordinates.
!     Unlike {\tt ESMF\_Regrid} this routine does not do interpolation,
!     only data movement.
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
!EOP
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
        print *, "Invalid or Destroyed Bundle"
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif
      if (.not.associated(srcBundle%btypep)) then
        print *, "Invalid or Destroyed Bundle"
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif

      dtypep => dstBundle%btypep
      stypep => srcBundle%btypep

      if (dtypep%bundlestatus.ne.ESMF_STATE_READY .or. &
          stypep%bundlestatus.ne.ESMF_STATE_READY) then
        print *, "Bundle not ready"
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif

      call ESMF_ArrayRedistStore(stypep%flist(1)%ftypep%localfield%localdata, &
                                 stypep%grid, &
                                 stypep%flist(1)%ftypep%mapping, &
                                 dtypep%flist(1)%ftypep%localfield%localdata, &
                                 dtypep%grid, &
                                 dtypep%flist(1)%ftypep%mapping, &
                                 parentDElayout, &
                                 routehandle, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleRedistStore: ArrayRedistStore returned failure"
        return
      endif 

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleRedistStore


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleRegrid - Execute a Regrid operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRegrid(srcbundle, dstbundle, parentDElayout, &
                                   blocking, commhandle, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcbundle
      type(ESMF_Bundle), intent(inout) :: dstbundle
      type(ESMF_newDELayout), intent(in) :: parentDElayout
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a {\tt ESMF\_Regrid} operation over the data
!     in a {\tt ESMF\_Bundle}.  This routine reads the source bundle and 
!     leaves the data untouched.  It uses the {\tt ESMF\_Grid} and
!     {\tt ESMF\_DataMap} information in the destination bundle to
!     control the transformation of data.  The array data in the 
!     destination bundle is overwritten by this call.
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
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
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
!BOP
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
!     Release all stored information about the Regridding associated
!     with this {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this Bundle Regridding.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleRegridRelease

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleRegridStore - Precompute Regrid operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRegridStore(srcbundle, dstbundle, parentDElayout, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcbundle
      type(ESMF_Bundle), intent(inout) :: dstbundle
      type(ESMF_newDELayout), intent(in) :: parentDElayout
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a {\tt ESMF\_Regrid} operation over the data
!     in a {\tt ESMF\_Bundle}.  This routine reads the source bundle and 
!     leaves the data untouched.  It uses the {\tt ESMF\_Grid} and
!     {\tt ESMF\_DataMap} information in the destination bundle to
!     control the transformation of data.  The array data in the 
!     destination bundle is overwritten by this call.
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
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
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

      end module ESMF_BundleCommMod
