! $Id: ESMF_BundleComm.F90,v 1.2 2004/02/19 21:23:34 jwolfe Exp $
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
!BOP
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
      use ESMF_IOMod
      use ESMF_DELayoutMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayBaseMod
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
   public ESMF_BundleAllGather! Combine 1 decomposed bundle into N copies on N DEs

   public ESMF_BundleScatter  ! Split 1 bundle into a decomposed one over N DEs
   !public ESMF_BundleBroadcast! Send 1 bundle to all DEs, none decomposed
   !public ESMF_BundleAlltoAll ! might make sense with bundles; each DE could
                              ! call with a different non-decomposed bundle
                              ! and the result would be a packed bundle of
                              ! data with decomposed bundle on each DE.

   public ESMF_BundleReduce     ! Global reduction operation, return on 1 DE
   !public ESMF_BundleAllReduce  ! Global reduction operation, return on each DE

!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_BundleComm.F90,v 1.2 2004/02/19 21:23:34 jwolfe Exp $'

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
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleReduce - Reduction operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleReduce(bundle, rtype, result, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle) :: bundle                 
      integer :: rtype
      integer :: result
      type(ESMF_Async), intent(inout), optional :: async
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a Reduction operation over the data in a {\tt ESMF\_Bundle}.
!
!     \begin{description}
!     \item [bundle] 
!           Bundle containing data to be reduced.
!     \item [rtype]
!           Type of reduction operation to perform.  Options include: ...
!     \item [result] 
!           Numeric result (may be single number, may be array)
!     \item [{[async]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
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
! !IROUTINE: ESMF_BundleAllGather - Data AllGather operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleAllGather(bundle, array, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle                 
      type(ESMF_Array), intent(out) :: array
      type(ESMF_Async), intent(inout), optional :: async
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a {\tt ESMF\_AllGather} operation
!     over the data in a {\tt ESMF\_Bundle}.  If the {\tt ESMF\_Bundle} is
!     decomposed over N {\tt ESMF\_DE}s, this routine returns a copy of the
!     entire collected data {\tt ESMF\_Array} on each of the N {\tt ESMF\_DE}s.
!
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_Bundle} containing data to be gathered.
!     \item [array] 
!           Newly created array containing the collected data.
!           It is the size of the entire undecomposed grid.
!     \item [{[async]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
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
        print *, "ERROR in BundleAllGather: Array AllGather returned failure"
        return
      endif 

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleAllGather


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGather - Data Gather operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleGather(bundle, destination_de, array, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle                 
      integer, intent(in) :: destination_de
      type(ESMF_Array), intent(out) :: array
      type(ESMF_Async), intent(inout), optional :: async
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
!     \begin{description}
!     \item [bundle] 
!           {\tt ESMF\_Bundle} containing data to be gathered.
!     \item [destination\_de] 
!           Destination {\tt ESMF\_DE} number where the Gathered Array is to be returned.
!     \item [array] 
!           Newly created array containing the collected data on the
!           specified {\tt ESMF\_DE}.  It is the size of the entire undecomposed grid.
!           On all other {\tt ESMF\_DE}s this return is an invalid object.
!     \item [{[async]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: btypep     ! bundle type info
      type(ESMF_AxisIndex) :: axis(ESMF_MAXDIM)   ! Size info for Grid
      type(ESMF_DELayout) :: layout               ! layout
      integer :: i, gridrank, datarank, thisdim, thislength
      integer, dimension(ESMF_MAXDIM) :: dimorder, dimlengths, &
                                         global_dimlengths
      integer, dimension(ESMF_MAXGRIDDIM) :: decomps, globalCellCountPerDim
      integer, dimension(ESMF_MAXGRIDDIM) :: maxLocalCellCountPerDim, local_maxlengths
      integer, dimension(:), pointer :: decompids
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      btypep => bundle%btypep

      ! Query the datamap and set info for grid so it knows how to
      !  match up the array indices and the grid indices.
      call ESMF_DataMapGet(btypep%flist(1)%ftypep%mapping, gridrank=gridrank, dimlist=dimorder, &
                           rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleGather: DataMapGet returned failure"
        return
      endif 
      call ESMF_GridGet(btypep%grid, &
                        globalCellCountPerDim=globalCellCountPerDim, &
                        maxLocalCellCountPerDim=maxLocalCellCountPerDim, &
                        rc=status)
!     call ESMF_GridGet(btypep%grid, decomps, rc=status)   !TODO: add decomps
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleGather: GridGet returned failure"
        return
      endif 
      decomps(1) = 1    ! TODO: remove this once the grid call is created
      decomps(2) = 2

      ! And get the Array sizes
      call ESMF_ArrayGet(btypep%flist(1)%ftypep%localfield%localdata, rank=datarank, &
                         counts=dimlengths, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleGather: ArrayGet returned failure"
        return
      endif 

      allocate(decompids(datarank), stat=status)
      do i=1, datarank
        decompids(i) = dimorder(i)
        global_dimlengths(i) = dimlengths(i)
        if(dimorder(i).ne.0) then
          decompids(i) = decomps(dimorder(i))
          global_dimlengths(i) = globalCellCountPerDim(dimorder(i))
          local_maxlengths(i) = maxLocalCellCountPerDim(dimorder(i))
        endif
      enddo

      ! Set the axis info on the array to pass thru to DistGrid
      do i=1, gridrank
          thisdim = dimorder(i)
          if (thisdim .eq. 0) cycle

          thislength = dimlengths(thisdim)
     
          call ESMF_AxisIndexSet(axis(i), 1, thislength, thislength, rc=status)
     
      enddo

      ! Attach this info to the array
      call ESMF_ArraySetAxisIndex(btypep%flist(1)%ftypep%localfield%localdata, &
                                       compindex=axis, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleGather: ArraySetAxisIndex returned failure"
        return
      endif 

      ! Call Array method to perform actual work
      call ESMF_GridGetDELayout(btypep%grid, layout, status)
      call ESMF_ArrayGather(btypep%flist(1)%ftypep%localfield%localdata, layout, decompids, &
                            global_dimlengths, local_maxlengths, destination_de, &
                            array, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleGather: Array Gather returned failure"
        return
      endif 

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGather


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleScatter - Data Scatter operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleScatter(array, source_de, bundle, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      integer, intent(in) :: source_de
      type(ESMF_Bundle), intent(inout) :: bundle                 
      type(ESMF_Async), intent(inout), optional :: async
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a Scatter operation over the data
!     in an {\tt ESMF\_Array}, returning it as the data array in a {\tt ESMF\_Bundle}.  
!     If the Bundle is decomposed over N {\tt ESMF\_DE}s, this routine
!     takes a single array on the specified {\tt ESMF\_DE} and returns a decomposed copy
!     on each of the N {\tt ESMF\_DE}s, as the {\tt ESMF\_Array} associated with the given empty {\tt ESMF\_Bundle}.
!
!     \begin{description}
!     \item [array] 
!           Input {\tt ESMF\_Array} containing the collected data.
!           It must be the size of the entire undecomposed grid.
!     \item [source\_de]
!           Integer {\tt ESMF\_DE} number where the data to be Scattered is located.  The
!           {\tt ESMF\_Array} input is ignored on all other {\tt ESMF\_DE}s.
!     \item [bundle] 
!           Empty Bundle containing {\tt ESMF\_Grid} which will correspond to the data 
!           in the array which will be scattered.  When this routine returns
!           each {\tt ESMF\_Bundle} will contain a valid data array containing the 
!           subset of the decomposed data.
!     \item [{[async]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType) :: btypep              ! bundle type info
      type(ESMF_AxisIndex) :: axis(ESMF_MAXDIM)   ! Size info for Grid
      type(ESMF_DELayout) :: layout               ! layout
      type(ESMF_Array) :: dstarray                ! Destination array
      integer :: i, gridrank, datarank, thisdim, thislength
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
      call ESMF_DataMapGet(btypep%flist(1)%ftypep%mapping, gridrank=gridrank, &
                           dimlist=dimorder, rc=status)
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
      call ESMF_GridGetDELayout(btypep%grid, layout, status)
      call ESMF_ArrayScatter(array, layout, decompids, source_de, dstarray, &
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
! !IROUTINE: ESMF_BundleHaloStore - Precompute a Data Halo operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleHaloStore(bundle, routehandle, halodirection, & 
                                     blocking, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_HaloDirection), intent(in), optional :: halodirection
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a {\tt Halo} operation over the data
!     in an {\tt ESMF\_Bundle}.  This routine updates the data 
!     inside the {\tt ESMF\_Bundle} in place.
!
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
!     \item [{blocking]}]
!           Specify that the communications will be blocking, nonblocking,
!           or that the option will be specified at run time.  If not 
!           specified, the default is blocking.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
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
                               halodirection, blocking, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleHaloStore: ArrayHaloStore returned failure"
        return
      endif 

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleHaloStore

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleHalo - Execute a Data Halo operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleHalo(bundle, routehandle, blocking, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a {\tt Halo} operation over the data
!     in an {\tt ESMF\_Bundle}.  This routine updates the data 
!     inside the {\tt ESMF\_Bundle} in place.
!
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
!           
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
                             blocking, rc=status)
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
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this Bundle Halo.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleHaloRelease

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleRedistStore - Data Redistribution operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRedistStore(srcbundle, dstbundle, parentlayout, &
                                       routehandle, blocking, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcbundle                 
      type(ESMF_Bundle), intent(inout) :: dstbundle                 
      type(ESMF_DELayout), intent(in) :: parentlayout
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_Async), intent(inout), optional :: blocking
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
!     \begin{description}
!     \item [srcbundle] 
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstbundle] 
!           {\tt ESMF\_Bundle} containing destination grid.
!     \item [parentlayout]
!           {\tt ESMF\_Layout} which encompasses both {\tt ESMF\_Bundle}s, 
!           most commonly the layout
!           of the Coupler if the redistribution is inter-component, 
!           but could also be the individual layout for a component if the 
!           redistribution is intra-component.  
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} which will be used to execute the
!           redistribution when {\tt ESMF\_BundleRedist} is called.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communication.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType) :: stypep, dtypep      ! bundle type info
      type(ESMF_Route) :: route
      type(ESMF_DELayout) :: srclayout, dstlayout
      type(ESMF_Logical) :: hasdata        ! does this DE contain localdata?
      logical :: hassrcdata        ! does this DE contain localdata from src?
      logical :: hasdstdata        ! does this DE contain localdata from dst?
      logical :: hascachedroute    ! can we reuse an existing route?
      integer :: i, gridrank, datarank, thisdim
      integer :: nx, ny
      integer, dimension(ESMF_MAXDIM) :: dimorder, dimlengths, &
                                         global_dimlengths
      integer, dimension(ESMF_MAXGRIDDIM) :: decomps, global_cell_dim
      integer :: my_src_DE, my_dst_DE, my_DE
      type(ESMF_AxisIndex), dimension(:,:), pointer :: src_AI_exc, dst_AI_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: src_AI_tot, dst_AI_tot
      type(ESMF_AxisIndex), dimension(:,:), pointer :: gl_src_AI_exc, gl_dst_AI_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: gl_src_AI_tot, gl_dst_AI_tot
      type(ESMF_LocalArray) :: src_local_array, dst_local_array
      integer, dimension(ESMF_MAXGRIDDIM) :: src_global_count
      integer, dimension(:,:), allocatable :: src_global_start
      integer, dimension(ESMF_MAXGRIDDIM) :: dst_global_count
      integer, dimension(:,:), allocatable :: dst_global_start
      type(ESMF_Logical), dimension(ESMF_MAXGRIDDIM) :: periodic
      integer :: AI_snd_count, AI_rcv_count

   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      stypep = srcbundle%btypep
      dtypep = dstbundle%btypep

      ! Our DE number in the parent layout
      call ESMF_DELayoutGetDEid(parentlayout, my_DE, status)

      ! TODO: we need not only to know if this DE has data in the bundle,
      !   but also the de id for both src & dest bundles

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination bundles do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      ! if srclayout ^ parentlayout == NULL, nothing to send from this DE id.
      call ESMF_GridGetDELayout(stypep%grid, srclayout, status)
      call ESMF_DELayoutGetDEExists(parentlayout, my_DE, srclayout, hasdata)
      hassrcdata = (hasdata .eq. ESMF_TRUE) 
      hassrcdata = .true.   ! temp for now
      if (hassrcdata) then
          ! don't ask for our de number if this de isn't part of the layout
          call ESMF_DELayoutGetDEid(srclayout, my_src_DE, status)
      endif

      ! if dstlayout ^ parentlayout == NULL, nothing to recv on this DE id.
      call ESMF_GridGetDELayout(dtypep%grid, dstlayout, status)
      call ESMF_DELayoutGetDEExists(parentlayout, my_DE, dstlayout, hasdata)
      hasdstdata = (hasdata .eq. ESMF_TRUE) 
      hasdstdata = .true.   ! temp for now
      if (hasdstdata) then
          ! don't ask for our de number if this de isn't part of the layout
          call ESMF_DELayoutGetDEid(dstlayout, my_dst_DE, status)
      endif

      ! if neither are true this DE cannot be involved in the communication
      !  and it can just return now.
      if ((.not. hassrcdata) .and. (.not. hasdstdata)) then
          if (rcpresent) rc = ESMF_SUCCESS
          return
      endif

      ! if src bundle exists on this DE, query it for information
      if (hassrcdata) then
          ! Query the datamap and set info for grid so it knows how to
          !  match up the array indicies and the grid indicies.
          call ESMF_DataMapGet(stypep%flist(1)%ftypep%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
            print *, "ERROR in BundleRedist: DataMapGet returned failure"
            return
          endif 

          ! And get the Array sizes
          call ESMF_ArrayGet(stypep%flist(1)%ftypep%localfield%localdata, rank=datarank, &
                                               counts=dimlengths, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
             print *, "ERROR in BundleRedist: ArrayGet returned failure"
             return
          endif 
      endif 

      ! if dst bundle exists on this DE, query it for information
      if (hasdstdata) then
          ! Query the datamap and set info for grid so it knows how to
          !  match up the array indicies and the grid indicies.
          call ESMF_DataMapGet(dtypep%flist(1)%ftypep%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
            print *, "ERROR in BundleRedist: DataMapGet returned failure"
            return
          endif 

          ! And get the Array sizes
          call ESMF_ArrayGet(dtypep%flist(1)%ftypep%localfield%localdata, rank=datarank, &
                                               counts=dimlengths, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
             print *, "ERROR in BundleRedist: ArrayGet returned failure"
             return
          endif 
      endif

      ! set up things we need to find a cached route or precompute one
      if (hassrcdata) then
          call ESMF_DELayoutGetSize(srclayout, nx, ny);
          AI_snd_count = nx * ny

          allocate(src_global_start(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          call ESMF_GridGet(stypep%grid, &
                            globalCellCountPerDim=src_global_count, &
                            globalStartPerDEPerDim=src_global_start, rc=status)

          allocate(src_AI_tot(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(src_AI_exc(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(gl_src_AI_tot(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(gl_src_AI_exc(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          call ESMF_ArrayGetAllAxisIndices(stypep%flist(1)%ftypep%localfield%localdata, &
                                           stypep%grid, stypep%flist(1)%ftypep%mapping, &
                                           src_AI_tot, src_AI_exc, rc=rc)
          ! translate the AI's to global index
          call ESMF_GridLocalToGlobalIndex(stypep%grid, localAI2D=src_AI_tot, &
                                           globalAI2D=gl_src_AI_tot, rc=rc)
          call ESMF_GridLocalToGlobalIndex(stypep%grid, localAI2D=src_AI_exc, &
                                           globalAI2D=gl_src_AI_exc, rc=rc)
      else
          AI_snd_count = 0
      endif
      if (hasdstdata) then
          call ESMF_DELayoutGetSize(dstlayout, nx, ny);
          AI_rcv_count = nx * ny

          allocate(dst_global_start(AI_rcv_count, ESMF_MAXGRIDDIM), stat=status)
          call ESMF_GridGet(dtypep%grid, &
                            globalCellCountPerDim=dst_global_count, &
                            globalStartPerDEPerDim=dst_global_start, rc=status)

          allocate(dst_AI_tot(AI_rcv_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(dst_AI_exc(AI_rcv_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(gl_dst_AI_tot(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(gl_dst_AI_exc(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          call ESMF_ArrayGetAllAxisIndices(dtypep%flist(1)%ftypep%localfield%localdata, &
                                           dtypep%grid, dtypep%flist(1)%ftypep%mapping, &
                                           dst_AI_tot, dst_AI_exc, rc=rc)
          ! translate the AI's to global index
          call ESMF_GridLocalToGlobalIndex(dtypep%grid, localAI2D=dst_AI_tot, &
                                           globalAI2D=gl_dst_AI_tot, rc=rc)
          call ESMF_GridLocalToGlobalIndex(dtypep%grid, localAI2D=dst_AI_exc, &
                                           globalAI2D=gl_dst_AI_exc, rc=rc)
      else
          AI_rcv_count = 0
      endif
          
      ! periodic only matters for halo operations
      do i=1, ESMF_MAXGRIDDIM
        periodic(i) = ESMF_FALSE
      enddo

      ! Does this same route already exist?  If so, then we can drop
      ! down immediately to RouteRun.
      call ESMF_RouteGetCached(datarank, &
                               my_dst_DE, gl_dst_AI_exc, gl_dst_AI_tot, &
                               AI_rcv_count, dstlayout, &
                               my_src_DE, gl_src_AI_exc, gl_src_AI_tot, &
                               AI_snd_count, srclayout, periodic, &
                               hascachedroute, route, rc=status)

      if (.not. hascachedroute) then
          ! Create the route object.  This needs to be the parent layout which
          ! includes the DEs from both bundles.
          route = ESMF_RouteCreate(parentlayout, rc) 

          call ESMF_RoutePrecomputeRedist(route, datarank, &
                                    my_dst_DE, gl_dst_AI_exc, gl_dst_AI_tot, &
                                    AI_rcv_count, dst_global_start, &
                                    dst_global_count, dstlayout,  &
                                    my_src_DE, gl_src_AI_exc, gl_src_AI_tot, &
                                    AI_snd_count, src_global_start, &
                                    src_global_count, srclayout, &
                                    rc=status)

      endif

      ! Once table is full, execute the communications it represents.

      ! There are 3 possible cases - src+dst, src only, dst only
      !  (if both are false then we've already returned.)
      if ((hassrcdata) .and. (.not. hasdstdata)) then
          src_local_array=stypep%flist(1)%ftypep%localfield%localdata
          call ESMF_RouteRun(route, srcarray=src_local_array, rc=status) 

      else if ((.not. hassrcdata) .and. (hasdstdata)) then
          dst_local_array=dtypep%flist(1)%ftypep%localfield%localdata
          call ESMF_RouteRun(route, dstarray=dst_local_array, rc=status)

      else
          src_local_array=stypep%flist(1)%ftypep%localfield%localdata
          dst_local_array=dtypep%flist(1)%ftypep%localfield%localdata
          call ESMF_RouteRun(route, src_local_array, dst_local_array, status)
      endif
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleRedist: RouteRun returned failure"
        return
      endif 

      ! TODO: do not delete the route because we are caching it.
      !call ESMF_RouteDestroy(route, rc)

      ! get rid of temporary arrays
      if (associated(src_AI_tot)) deallocate(src_AI_tot, stat=status)
      if (associated(src_AI_exc)) deallocate(src_AI_exc, stat=status)
      if (associated(dst_AI_tot)) deallocate(dst_AI_tot, stat=status)
      if (associated(dst_AI_exc)) deallocate(dst_AI_exc, stat=status)
      if (associated(gl_src_AI_tot)) deallocate(gl_src_AI_tot, stat=status)
      if (associated(gl_src_AI_exc)) deallocate(gl_src_AI_exc, stat=status)
      if (associated(gl_dst_AI_tot)) deallocate(gl_dst_AI_tot, stat=status)
      if (associated(gl_dst_AI_exc)) deallocate(gl_dst_AI_exc, stat=status)
      if (allocated(src_global_start)) deallocate(src_global_start, stat=status)
      if (allocated(dst_global_start)) deallocate(dst_global_start, stat=status)

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleRedistStore


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleRedist - Data Redistribution operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRedist(srcbundle, dstbundle, parentlayout, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcbundle                 
      type(ESMF_Bundle), intent(inout) :: dstbundle                 
      type(ESMF_DELayout), intent(in) :: parentlayout
      type(ESMF_Async), intent(inout), optional :: async
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
!     \begin{description}
!     \item [srcbundle] 
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstbundle] 
!           {\tt ESMF\_Bundle} containing destination grid.
!     \item [parentlayout]
!           {\tt ESMF\_Layout} which encompasses both {\tt ESMF\_Bundle}s, 
!           most commonly the layout
!           of the Coupler if the redistribution is inter-component, 
!           but could also be the individual layout for a component if the 
!           redistribution is intra-component.  
!     \item [{[async]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communication.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType) :: stypep, dtypep      ! bundle type info
      type(ESMF_Route) :: route
      type(ESMF_DELayout) :: srclayout, dstlayout
      type(ESMF_Logical) :: hasdata        ! does this DE contain localdata?
      logical :: hassrcdata        ! does this DE contain localdata from src?
      logical :: hasdstdata        ! does this DE contain localdata from dst?
      logical :: hascachedroute    ! can we reuse an existing route?
      integer :: i, gridrank, datarank, thisdim
      integer :: nx, ny
      integer, dimension(ESMF_MAXDIM) :: dimorder, dimlengths, &
                                         global_dimlengths
      integer, dimension(ESMF_MAXGRIDDIM) :: decomps, global_cell_dim
      integer :: my_src_DE, my_dst_DE, my_DE
      type(ESMF_AxisIndex), dimension(:,:), pointer :: src_AI_exc, dst_AI_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: src_AI_tot, dst_AI_tot
      type(ESMF_AxisIndex), dimension(:,:), pointer :: gl_src_AI_exc, gl_dst_AI_exc
      type(ESMF_AxisIndex), dimension(:,:), pointer :: gl_src_AI_tot, gl_dst_AI_tot
      type(ESMF_LocalArray) :: src_local_array, dst_local_array
      integer, dimension(ESMF_MAXGRIDDIM) :: src_global_count
      integer, dimension(:,:), allocatable :: src_global_start
      integer, dimension(ESMF_MAXGRIDDIM) :: dst_global_count
      integer, dimension(:,:), allocatable :: dst_global_start
      type(ESMF_Logical), dimension(ESMF_MAXGRIDDIM) :: periodic
      integer :: AI_snd_count, AI_rcv_count

   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      stypep = srcbundle%btypep
      dtypep = dstbundle%btypep

      ! Our DE number in the parent layout
      call ESMF_DELayoutGetDEid(parentlayout, my_DE, status)

      ! TODO: we need not only to know if this DE has data in the bundle,
      !   but also the de id for both src & dest bundles

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination bundles do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      ! if srclayout ^ parentlayout == NULL, nothing to send from this DE id.
      call ESMF_GridGetDELayout(stypep%grid, srclayout, status)
      call ESMF_DELayoutGetDEExists(parentlayout, my_DE, srclayout, hasdata)
      hassrcdata = (hasdata .eq. ESMF_TRUE) 
      hassrcdata = .true.   ! temp for now
      if (hassrcdata) then
          ! don't ask for our de number if this de isn't part of the layout
          call ESMF_DELayoutGetDEid(srclayout, my_src_DE, status)
      endif

      ! if dstlayout ^ parentlayout == NULL, nothing to recv on this DE id.
      call ESMF_GridGetDELayout(dtypep%grid, dstlayout, status)
      call ESMF_DELayoutGetDEExists(parentlayout, my_DE, dstlayout, hasdata)
      hasdstdata = (hasdata .eq. ESMF_TRUE) 
      hasdstdata = .true.   ! temp for now
      if (hasdstdata) then
          ! don't ask for our de number if this de isn't part of the layout
          call ESMF_DELayoutGetDEid(dstlayout, my_dst_DE, status)
      endif

      ! if neither are true this DE cannot be involved in the communication
      !  and it can just return now.
      if ((.not. hassrcdata) .and. (.not. hasdstdata)) then
          if (rcpresent) rc = ESMF_SUCCESS
          return
      endif

      ! if src bundle exists on this DE, query it for information
      if (hassrcdata) then
          ! Query the datamap and set info for grid so it knows how to
          !  match up the array indices and the grid indices.
          call ESMF_DataMapGet(stypep%flist(1)%ftypep%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
            print *, "ERROR in BundleRedist: DataMapGet returned failure"
            return
          endif 

          ! And get the Array sizes
          call ESMF_ArrayGet(stypep%flist(1)%ftypep%localfield%localdata, rank=datarank, &
                                               counts=dimlengths, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
             print *, "ERROR in BundleRedist: ArrayGet returned failure"
             return
          endif 
      endif 

      ! if dst bundle exists on this DE, query it for information
      if (hasdstdata) then
          ! Query the datamap and set info for grid so it knows how to
          !  match up the array indices and the grid indices.
          call ESMF_DataMapGet(dtypep%flist(1)%ftypep%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
            print *, "ERROR in BundleRedist: DataMapGet returned failure"
            return
          endif 

          ! And get the Array sizes
          call ESMF_ArrayGet(dtypep%flist(1)%ftypep%localfield%localdata, rank=datarank, &
                                               counts=dimlengths, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
             print *, "ERROR in BundleRedist: ArrayGet returned failure"
             return
          endif 
      endif

      ! set up things we need to find a cached route or precompute one
      if (hassrcdata) then
          call ESMF_DELayoutGetSize(srclayout, nx, ny);
          AI_snd_count = nx * ny

          allocate(src_global_start(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          call ESMF_GridGet(stypep%grid, &
                            globalCellCountPerDim=src_global_count, &
                            globalStartPerDEPerDim=src_global_start, rc=status)

          allocate(src_AI_tot(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(src_AI_exc(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(gl_src_AI_tot(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(gl_src_AI_exc(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          call ESMF_ArrayGetAllAxisIndices(stypep%flist(1)%ftypep%localfield%localdata, &
                                           stypep%grid, stypep%flist(1)%ftypep%mapping, &
                                           src_AI_tot, src_AI_exc, rc=rc)
          ! translate the AI's to global index
          call ESMF_GridLocalToGlobalIndex(stypep%grid, localAI2D=src_AI_tot, &
                                           globalAI2D=gl_src_AI_tot, rc=rc)
          call ESMF_GridLocalToGlobalIndex(stypep%grid, localAI2D=src_AI_exc, &
                                           globalAI2D=gl_src_AI_exc, rc=rc)
      else
          AI_snd_count = 0
      endif
      if (hasdstdata) then
          call ESMF_DELayoutGetSize(dstlayout, nx, ny);
          AI_rcv_count = nx * ny

          allocate(dst_global_start(AI_rcv_count, ESMF_MAXGRIDDIM), stat=status)
          call ESMF_GridGet(dtypep%grid, &
                            globalCellCountPerDim=dst_global_count, &
                            globalStartPerDEPerDim=dst_global_start, rc=status)

          allocate(dst_AI_tot(AI_rcv_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(dst_AI_exc(AI_rcv_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(gl_dst_AI_tot(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          allocate(gl_dst_AI_exc(AI_snd_count, ESMF_MAXGRIDDIM), stat=status)
          call ESMF_ArrayGetAllAxisIndices(dtypep%flist(1)%ftypep%localfield%localdata, &
                                           dtypep%grid, dtypep%flist(1)%ftypep%mapping, &
                                           dst_AI_tot, dst_AI_exc, rc=rc)
          ! translate the AI's to global index
          call ESMF_GridLocalToGlobalIndex(dtypep%grid, localAI2D=dst_AI_tot, &
                                           globalAI2D=gl_dst_AI_tot, rc=rc)
          call ESMF_GridLocalToGlobalIndex(dtypep%grid, localAI2D=dst_AI_exc, &
                                           globalAI2D=gl_dst_AI_exc, rc=rc)
      else
          AI_rcv_count = 0
      endif
          
      ! periodic only matters for halo operations
      do i=1, ESMF_MAXGRIDDIM
        periodic(i) = ESMF_FALSE
      enddo

      ! Does this same route already exist?  If so, then we can drop
      ! down immediately to RouteRun.
      call ESMF_RouteGetCached(datarank, &
                               my_dst_DE, gl_dst_AI_exc, gl_dst_AI_tot, &
                               AI_rcv_count, dstlayout, &
                               my_src_DE, gl_src_AI_exc, gl_src_AI_tot, &
                               AI_snd_count, srclayout, periodic, &
                               hascachedroute, route, rc=status)

      if (.not. hascachedroute) then
          ! Create the route object.  This needs to be the parent layout which
          ! includes the DEs from both bundles.
          route = ESMF_RouteCreate(parentlayout, rc) 

          call ESMF_RoutePrecomputeRedist(route, datarank, &
                                    my_dst_DE, gl_dst_AI_exc, gl_dst_AI_tot, &
                                    AI_rcv_count, dst_global_start, &
                                    dst_global_count, dstlayout,  &
                                    my_src_DE, gl_src_AI_exc, gl_src_AI_tot, &
                                    AI_snd_count, src_global_start, &
                                    src_global_count, srclayout, &
                                    rc=status)

      endif

      ! Once table is full, execute the communications it represents.
      ! TODO: fix code here

      ! There are 3 possible cases - src+dst, src only, dst only
      !  (if both are false then we've already returned.)
      if ((hassrcdata) .and. (.not. hasdstdata)) then
          src_local_array=stypep%flist(1)%ftypep%localfield%localdata
          call ESMF_RouteRun(route, srcarray=src_local_array, rc=status) 

      else if ((.not. hassrcdata) .and. (hasdstdata)) then
          dst_local_array=dtypep%flist(1)%ftypep%localfield%localdata
          call ESMF_RouteRun(route, dstarray=dst_local_array, rc=status)

      else
          src_local_array=stypep%flist(1)%ftypep%localfield%localdata
          dst_local_array=dtypep%flist(1)%ftypep%localfield%localdata
          call ESMF_RouteRun(route, src_local_array, dst_local_array, status)
      endif
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in BundleRedist: RouteRun returned failure"
        return
      endif 

      ! TODO: do not delete the route because we are caching it.
      !call ESMF_RouteDestroy(route, rc)

      ! get rid of temporary arrays
      if (associated(src_AI_tot)) deallocate(src_AI_tot, stat=status)
      if (associated(src_AI_exc)) deallocate(src_AI_exc, stat=status)
      if (associated(dst_AI_tot)) deallocate(dst_AI_tot, stat=status)
      if (associated(dst_AI_exc)) deallocate(dst_AI_exc, stat=status)
      if (associated(gl_src_AI_tot)) deallocate(gl_src_AI_tot, stat=status)
      if (associated(gl_src_AI_exc)) deallocate(gl_src_AI_exc, stat=status)
      if (associated(gl_dst_AI_tot)) deallocate(gl_dst_AI_tot, stat=status)
      if (associated(gl_dst_AI_exc)) deallocate(gl_dst_AI_exc, stat=status)
      if (allocated(src_global_start)) deallocate(src_global_start, stat=status)
      if (allocated(dst_global_start)) deallocate(dst_global_start, stat=status)

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
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this Bundle Redist.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleRedistRelease

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleRegridStore - Precompute Regrid operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRegridStore(srcbundle, dstbundle, parentlayout, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcbundle                 
      type(ESMF_Bundle), intent(inout) :: dstbundle                 
      type(ESMF_DELayout), intent(in) :: parentlayout
      type(ESMF_Async), intent(inout), optional :: async
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
!     \begin{description}
!     \item [srcbundle] 
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstbundle] 
!           {\tt ESMF\_Bundle} containing destination grid and data map.
!     \item [parentlayout]
!           {\tt ESMF\_Layout} which encompasses both {\tt ESMF\_Bundle}s, 
!           most commonly the layout
!           of the Coupler if the regridding is inter-component, but could 
!           also be the individual layout for a component if the 
!           regridding is intra-component.  
!     \item [{[async]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
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
!BOP
! !IROUTINE: ESMF_BundleRegrid - Execute a Regrid operation on a Bundle

! !INTERFACE:
      subroutine ESMF_BundleRegrid(srcbundle, dstbundle, parentlayout, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: srcbundle                 
      type(ESMF_Bundle), intent(inout) :: dstbundle                 
      type(ESMF_DELayout), intent(in) :: parentlayout
      type(ESMF_Async), intent(inout), optional :: async
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
!     \begin{description}
!     \item [srcbundle] 
!           {\tt ESMF\_Bundle} containing source data.
!     \item [dstbundle] 
!           {\tt ESMF\_Bundle} containing destination grid and data map.
!     \item [parentlayout]
!           {\tt ESMF\_Layout} which encompasses both {\tt ESMF\_Bundle}s, 
!           most commonly the layout
!           of the Coupler if the regridding is inter-component, but could 
!           also be the individual layout for a component if the 
!           regridding is intra-component.  
!     \item [{[async]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
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
!     \begin{description}
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} associated with this Bundle Regridding.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_BundleRegridRelease

!------------------------------------------------------------------------------

      end module ESMF_BundleCommMod
