! $Id: ESMF_FieldComm.F90,v 1.1 2004/01/26 17:44:00 nscollins Exp $
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
!     ESMF Field Communications module
      module ESMF_FieldCommMod
!
!==============================================================================
!
! This file contains the Field communication methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOP
! !MODULE: ESMF_FieldCommMod - Communication routines for Field objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Field} class
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
   public ESMF_FieldHaloStore, ESMF_FieldHalo, ESMF_FieldHaloRelease 
                             ! Redistribute existing arrays, matching grids
   public ESMF_FieldRedistStore, ESMF_FieldRedist, ESMF_FieldRedistRelease 
                             ! Regridding and interpolation, different grids
   public ESMF_FieldRegridStore, ESMF_FieldRegrid, ESMF_FieldRegridRelease 

   public ESMF_FieldGather   ! Combine 1 decomposed field into 1 on 1 DE
   public ESMF_FieldAllGather! Combine 1 decomposed field into N copies on N DEs

   public ESMF_FieldScatter  ! Split 1 field into a decomposed one over N DEs
   !public ESMF_FieldBroadcast! Send 1 field to all DEs, none decomposed
   !public ESMF_FieldAlltoAll ! might make sense with bundles; each DE could
                              ! call with a different non-decomposed field
                              ! and the result would be a packed bundle of
                              ! data with decomposed fields on each DE.

   public ESMF_FieldReduce     ! Global reduction operation, return on 1 DE
   !public ESMF_FieldAllReduce  ! Global reduction operation, return on each DE

!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_FieldComm.F90,v 1.1 2004/01/26 17:44:00 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldHalo - Temporary interface to ease transition
!
! !INTERFACE:
      interface ESMF_FieldHalo
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldHaloRun
        module procedure ESMF_FieldHaloDeprecated

! !DESCRIPTION:
!     Temporary interface to east transition from old syntax to new.
!     All new code should be using the {\tt ESMF\_FieldHaloRun} syntax.
!    
!EOP
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
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldReduce - Reduction operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldReduce(field, rtype, result, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field) :: field                 
      integer :: rtype
      integer :: result
      type(ESMF_Async), intent(inout), optional :: async
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a Reduction operation over the data in a {\tt ESMF\_Field}.
!
!     \begin{description}
!     \item [field] 
!           Field containing data to be reduced.
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
      !call ESMF_GridReduce(field%ftypep%grid, &
      !                     field%ftypep%localfield%localdata, &
      !                     rtype, result, status)
      !if(status .NE. ESMF_SUCCESS) then 
      !  print *, "ERROR in FieldReduce: Grid reduce"
      !  return
      !endif 

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldReduce


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldAllGather - Data AllGather operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldAllGather(field, array, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field                 
      type(ESMF_Array), intent(out) :: array
      type(ESMF_Async), intent(inout), optional :: async
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a {\tt ESMF\_AllGather} operation
!     over the data in a {\tt ESMF\_Field}.  If the {\tt ESMF\_Field} is
!     decomposed over N {\tt ESMF\_DE}s, this routine returns a copy of the
!     entire collected data {\tt ESMF\_Array} on each of the N {\tt ESMF\_DE}s.
!
!     \begin{description}
!     \item [field] 
!           {\tt ESMF\_Field} containing data to be gathered.
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
      type(ESMF_FieldType), pointer :: ftypep     ! field type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ftypep => field%ftypep

      ! Call Array method to perform actual work
      call ESMF_ArrayAllGather(ftypep%localfield%localdata, ftypep%grid, &
                               ftypep%mapping, array, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldAllGather: Array AllGather returned failure"
        return
      endif 

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldAllGather


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldGather - Data Gather operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldGather(field, destination_de, array, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field                 
      integer, intent(in) :: destination_de
      type(ESMF_Array), intent(out) :: array
      type(ESMF_Async), intent(inout), optional :: async
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Call {\tt ESMF\_Grid} routines to perform a {\tt ESMF\_Gather} operation
!     over the data in a {\tt ESMF\_Field}.  If the {\tt ESMF\_Field} is
!     decomposed over N {\tt ESMF\_DE}s, this routine returns a copy of the
!     entire collected data {\tt ESMF\_Array} on the specified destination
!     {\tt ESMF\_DE} number.  On all other {\tt ESMF\_DE}s, there is no return
!     {\tt ESMF\_Array}.
!
!     \begin{description}
!     \item [field] 
!           {\tt ESMF\_Field} containing data to be gathered.
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
      type(ESMF_FieldType), pointer :: ftypep     ! field type info
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

      ftypep => field%ftypep

      ! Query the datamap and set info for grid so it knows how to
      !  match up the array indices and the grid indices.
      call ESMF_DataMapGet(ftypep%mapping, gridrank=gridrank, dimlist=dimorder, &
                           rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldGather: DataMapGet returned failure"
        return
      endif 
      call ESMF_GridGet(ftypep%grid, &
                        globalCellCountPerDim=globalCellCountPerDim, &
                        maxLocalCellCountPerDim=maxLocalCellCountPerDim, &
                        rc=status)
!     call ESMF_GridGet(ftypep%grid, decomps, rc=status)   !TODO: add decomps
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldGather: GridGet returned failure"
        return
      endif 
      decomps(1) = 1    ! TODO: remove this once the grid call is created
      decomps(2) = 2

      ! And get the Array sizes
      call ESMF_ArrayGet(ftypep%localfield%localdata, rank=datarank, &
                         counts=dimlengths, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldGather: ArrayGet returned failure"
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
      call ESMF_ArraySetAxisIndex(ftypep%localfield%localdata, &
                                       compindex=axis, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldGather: ArraySetAxisIndex returned failure"
        return
      endif 

      ! Call Array method to perform actual work
      call ESMF_GridGetDELayout(ftypep%grid, layout, status)
      call ESMF_ArrayGather(ftypep%localfield%localdata, layout, decompids, &
                            global_dimlengths, local_maxlengths, destination_de, &
                            array, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldGather: Array Gather returned failure"
        return
      endif 

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldGather


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldScatter - Data Scatter operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldScatter(array, source_de, field, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      integer, intent(in) :: source_de
      type(ESMF_Field), intent(inout) :: field                 
      type(ESMF_Async), intent(inout), optional :: async
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a Scatter operation over the data
!     in an {\tt ESMF\_Array}, returning it as the data array in a {\tt ESMF\_Field}.  
!     If the Field is decomposed over N {\tt ESMF\_DE}s, this routine
!     takes a single array on the specified {\tt ESMF\_DE} and returns a decomposed copy
!     on each of the N {\tt ESMF\_DE}s, as the {\tt ESMF\_Array} associated with the given empty {\tt ESMF\_Field}.
!
!     \begin{description}
!     \item [array] 
!           Input {\tt ESMF\_Array} containing the collected data.
!           It must be the size of the entire undecomposed grid.
!     \item [source\_de]
!           Integer {\tt ESMF\_DE} number where the data to be Scattered is located.  The
!           {\tt ESMF\_Array} input is ignored on all other {\tt ESMF\_DE}s.
!     \item [field] 
!           Empty Field containing {\tt ESMF\_Grid} which will correspond to the data 
!           in the array which will be scattered.  When this routine returns
!           each {\tt ESMF\_Field} will contain a valid data array containing the 
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
      type(ESMF_FieldType) :: ftypep              ! field type info
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

      ftypep = field%ftypep

      ! Query the datamap and set info for grid so it knows how to
      !  match up the array indices and the grid indices.
      call ESMF_DataMapGet(ftypep%mapping, gridrank=gridrank, &
                           dimlist=dimorder, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldScatter: DataMapGet returned failure"
        return
      endif 
!     call ESMF_GridGet(ftypep%grid, decomps, rc=status)   !TODO
!     if(status .NE. ESMF_SUCCESS) then 
!       print *, "ERROR in FieldScatter: GridGet returned failure"
!       return
!     endif 
      decomps(1) = 1    ! TODO: remove this once the grid call is created
      decomps(2) = 2

      ! And get the Array sizes
      call ESMF_ArrayGet(ftypep%localfield%localdata, rank=datarank, &
                         counts=dimlengths, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldGather: ArrayGet returned failure"
        return
      endif 

      do i=1, datarank
        decompids(i) = dimorder(i)
        if(dimorder(i).ne.0) decompids(i) = decomps(dimorder(i))
      enddo

      ! Call Array method to perform actual work
      call ESMF_GridGetDELayout(ftypep%grid, layout, status)
      call ESMF_ArrayScatter(array, layout, decompids, source_de, dstarray, &
                             status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldScatter: Array Scatter returned failure"
        return
      endif 

      ! TODO: do we need to set dimorder here?  should datamap be an input
      !  to this routine, or specified at create time?   or should this be
      !  a field create method?
      ftypep%localfield%localdata = dstarray

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldScatter


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldHaloDeprecated - Data Halo operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldHaloDeprecated(field, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field                 
      type(ESMF_Async), intent(inout), optional :: async
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!  {\tt DEPRECATED!} - these args are for the previous version of Halo
!   which did internal caching.  The next version of the software will
!    have a Precompute/Store call and then FieldHalo will take a 
!    handle and do the execution of a precomputed route.  This routine
!    remains only until the new interfaces are working.
!
!     Perform a {\tt Halo} operation over the data
!     in an {\tt ESMF\_Field}.  This routine updates the data 
!     inside the {\tt ESMF\_Field} in place.
!
!     \begin{description}
!     \item [field] 
!           {\tt ESMF\_Field} containing data to be halo'd.
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
      type(ESMF_FieldType) :: ftypep              ! field type info
      type(ESMF_AxisIndex) :: axis(ESMF_MAXDIM)   ! Size info for Grid
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: grid
      integer :: i, j, gridrank, datarank, thisdim
      integer :: dimorder(ESMF_MAXDIM)   
      integer :: dimlengths(ESMF_MAXDIM)   
      type(ESMF_Route) :: route
      type(ESMF_LocalArray) :: local_array
      logical :: hascachedroute    ! can we reuse an existing route?
      integer :: nDEs
      integer :: my_DE
      integer, dimension(ESMF_MAXGRIDDIM) :: global_count
      integer, dimension(:,:), allocatable :: globalStartPerDEPerDim
      type(ESMF_AxisIndex), dimension(:,:), pointer :: src_AI, dst_AI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: gl_src_AI, gl_dst_AI
      type(ESMF_Logical), dimension(ESMF_MAXGRIDDIM) :: periodic
      integer :: AI_count

   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! Get the Layout from the Field's Grid
      ftypep = field%ftypep
      call ESMF_GridGetDELayout(ftypep%grid, layout, status)

      ! Our DE number in the layout
      call ESMF_DELayoutGetDEid(layout, my_DE, status)

      ! Query the datamap and set info for grid so it knows how to
      !  match up the array indices and the grid indices.
      call ESMF_DataMapGet(ftypep%mapping, gridrank=gridrank, &
                           dimlist=dimorder, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldHalo: DataMapGet returned failure"
        return
      endif 

      ! And get the Array sizes
      call ESMF_ArrayGet(ftypep%localfield%localdata, rank=datarank, &
                         counts=dimlengths, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
         print *, "ERROR in FieldHalo: ArrayGet returned failure"
         return
      endif 

      ! Get global starting counts and global counts
      call ESMF_DElayoutGetNumDEs(layout, nDEs, rc=status)
      AI_count = nDEs
      allocate(globalStartPerDEPerDim(nDEs, ESMF_MAXGRIDDIM), stat=status)
      allocate(src_AI(nDEs, ESMF_MAXGRIDDIM), stat=status)
      allocate(dst_AI(nDEs, ESMF_MAXGRIDDIM), stat=status)
      allocate(gl_src_AI(nDEs, ESMF_MAXGRIDDIM), stat=status)
      allocate(gl_dst_AI(nDEs, ESMF_MAXGRIDDIM), stat=status)
      call ESMF_GridGet(ftypep%grid, globalCellCountPerDim=global_count, &
                        globalStartPerDEPerDim=globalStartPerDEPerDim, &
                        periodic=periodic, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
         print *, "ERROR in FieldHalo: GridGet returned failure"
         return
      endif

      ! set up things we need to find a cached route or precompute one
      call ESMF_ArrayGetAllAxisIndices(ftypep%localfield%localdata, ftypep%grid, &
                                       totalindex=dst_AI, compindex=src_AI, &
                                       rc=status)       

      ! translate AI's into global numbering
      call ESMF_GridLocalToGlobalIndex(ftypep%grid, localAI2D=dst_AI, &
                                       globalAI2D=gl_dst_AI, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
         print *, "ERROR in FieldHalo: GridLocalToGlobalIndex returned failure"
         return
      endif
      call ESMF_GridLocalToGlobalIndex(ftypep%grid, localAI2D=src_AI, &
                                       globalAI2D=gl_src_AI, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
         print *, "ERROR in FieldHalo: GridLocalToGlobalIndex returned failure"
         return
      endif
          
      ! Does this same route already exist?  If so, then we can drop
      ! down immediately to RouteRun.  Note the confusing ordering of args;
      ! in this case, the receiving exclusive is the same as the source,
      ! and the receiving total is the same as dst.  ditto for the sending
      ! side.  these names should be changed to make this clearer.  TODO!
      call ESMF_RouteGetCached(datarank, my_DE, gl_src_AI, gl_dst_AI, &
                               AI_count, layout, my_DE, gl_src_AI, gl_dst_AI, &
                               AI_count, layout, periodic, &
                               hascachedroute, route, status)

      if (.not. hascachedroute) then
          ! Create the route object.
          route = ESMF_RouteCreate(layout, rc) 

          call ESMF_RoutePrecomputeHalo(route, datarank, my_DE, gl_src_AI, &
                                        gl_dst_AI, AI_count, &
                                        globalStartPerDEPerDim, &
                                        global_count, layout, periodic, status)

      endif

      ! Once table is full, execute the communications it represents.

      local_array = ftypep%localfield%localdata
      call ESMF_RouteRun(route, local_array, local_array, status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldHalo: RouteRun returned failure"
        return
      endif 

      ! TODO: we are caching the route so don't delete it.
      !call ESMF_RouteDestroy(route, rc)

      ! get rid of temporary arrays
      if (allocated(globalStartPerDEPerDim)) &
         deallocate(globalStartPerDEPerDim, stat=status)
      if (associated(src_AI)) deallocate(src_AI, stat=status)
      if (associated(dst_AI)) deallocate(dst_AI, stat=status)
      if (associated(gl_src_AI)) deallocate(gl_src_AI, stat=status)
      if (associated(gl_dst_AI)) deallocate(gl_dst_AI, stat=status)

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldHaloDeprecated


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldHaloStore - Precompute a Data Halo operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldHaloStore(field, routehandle, halodirection, & 
                                     blocking, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_HaloDirection), intent(in), optional :: halodirection
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a {\tt Halo} operation over the data
!     in an {\tt ESMF\_Field}.  This routine updates the data 
!     inside the {\tt ESMF\_Field} in place.
!
!     \begin{description}
!     \item [field] 
!           {\tt ESMF\_Field} containing data to be halo'd.
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} containing index to precomputed 
!           information for the Halo operation on this {\tt ESMF\_Field}.
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
      type(ESMF_FieldType), pointer :: ftypep     ! field type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! Sanity checks for good field, and that it has an associated grid
      ! and data before going down to the next level.
      if (.not.associated(field%ftypep)) then
        print *, "Invalid or Destroyed Field"
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif

      ftypep => field%ftypep

      if (ftypep%fieldstatus .ne. ESMF_STATE_READY) then
        print *, "Field not ready"
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif


      call ESMF_ArrayHaloStore(ftypep%localfield%localdata, ftypep%grid, &
                               ftypep%mapping, routehandle, &
                               halodirection, blocking, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldHaloStore: ArrayHaloStore returned failure"
        return
      endif 

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldHaloStore

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldHaloRun - Execute a Data Halo operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldHaloRun(field, routehandle, blocking, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a {\tt Halo} operation over the data
!     in an {\tt ESMF\_Field}.  This routine updates the data 
!     inside the {\tt ESMF\_Field} in place.
!
!     \begin{description}
!     \item [field] 
!           {\tt ESMF\_Field} containing data to be halo'd.
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
      type(ESMF_FieldType) :: ftypep              ! field type info
   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ftypep = field%ftypep

      call ESMF_ArrayHalo(ftypep%localfield%localdata, routehandle, &
                             blocking, rc=status)
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldHalo: ArrayHalo returned failure"
        return
      endif 

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldHaloRun

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldHaloRelease - Release resources associated w/ handle

! !INTERFACE:
      subroutine ESMF_FieldHaloRelease(routehandle, rc)
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
!           {\tt ESMF\_RouteHandle} associated with this Field Halo.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_FieldHaloRelease

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldRedistStore - Data Redistribution operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldRedistStore(srcfield, dstfield, parentlayout, &
                                       routehandle, blocking, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcfield                 
      type(ESMF_Field), intent(inout) :: dstfield                 
      type(ESMF_DELayout), intent(in) :: parentlayout
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Precompute a {\tt Redistribution} operation over the data
!     in a {\tt ESMF\_Field}.  This routine reads the source field and leaves 
!     the data untouched.  It reads the {\t ESMF\_Grid} and {\tt ESMF\_DataMap}
!     from the destination field and updates the array data in the destination.
!     The {\tt ESMF\_Grid}s may have different decompositions (different
!     {\tt ESMF\_DELayout}s) or different data maps, but the source and
!     destination grids must describe the same set of coordinates.
!     Unlike {\tt ESMF\_Regrid} this routine does not do interpolation,
!     only data movement.
!
!     \begin{description}
!     \item [srcfield] 
!           {\tt ESMF\_Field} containing source data.
!     \item [dstfield] 
!           {\tt ESMF\_Field} containing destination grid.
!     \item [parentlayout]
!           {\tt ESMF\_Layout} which encompasses both {\tt ESMF\_Field}s, 
!           most commonly the layout
!           of the Coupler if the redistribution is inter-component, 
!           but could also be the individual layout for a component if the 
!           redistribution is intra-component.  
!     \item [routehandle] 
!           {\tt ESMF\_RouteHandle} which will be used to execute the
!           redistribution when {\tt ESMF\_FieldRedist} is called.
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
      type(ESMF_FieldType) :: stypep, dtypep      ! field type info
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

      stypep = srcfield%ftypep
      dtypep = dstfield%ftypep

      ! Our DE number in the parent layout
      call ESMF_DELayoutGetDEid(parentlayout, my_DE, status)

      ! TODO: we need not only to know if this DE has data in the field,
      !   but also the de id for both src & dest fields

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
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

      ! if src field exists on this DE, query it for information
      if (hassrcdata) then
          ! Query the datamap and set info for grid so it knows how to
          !  match up the array indicies and the grid indicies.
          call ESMF_DataMapGet(stypep%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
            print *, "ERROR in FieldRedist: DataMapGet returned failure"
            return
          endif 

          ! And get the Array sizes
          call ESMF_ArrayGet(stypep%localfield%localdata, rank=datarank, &
                                               counts=dimlengths, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
             print *, "ERROR in FieldRedist: ArrayGet returned failure"
             return
          endif 
      endif 

      ! if dst field exists on this DE, query it for information
      if (hasdstdata) then
          ! Query the datamap and set info for grid so it knows how to
          !  match up the array indicies and the grid indicies.
          call ESMF_DataMapGet(dtypep%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
            print *, "ERROR in FieldRedist: DataMapGet returned failure"
            return
          endif 

          ! And get the Array sizes
          call ESMF_ArrayGet(dtypep%localfield%localdata, rank=datarank, &
                                               counts=dimlengths, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
             print *, "ERROR in FieldRedist: ArrayGet returned failure"
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
          call ESMF_ArrayGetAllAxisIndices(stypep%localfield%localdata, &
                                           stypep%grid, src_AI_tot, &
                                           src_AI_exc, rc=rc)
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
          call ESMF_ArrayGetAllAxisIndices(dtypep%localfield%localdata, &
                                           dtypep%grid, dst_AI_tot, &
                                           dst_AI_exc, rc=rc)
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
          ! includes the DEs from both fields.
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
          src_local_array=stypep%localfield%localdata
          call ESMF_RouteRun(route, srcarray=src_local_array, rc=status) 

      else if ((.not. hassrcdata) .and. (hasdstdata)) then
          dst_local_array=dtypep%localfield%localdata
          call ESMF_RouteRun(route, dstarray=dst_local_array, rc=status)

      else
          src_local_array=stypep%localfield%localdata
          dst_local_array=dtypep%localfield%localdata
          call ESMF_RouteRun(route, src_local_array, dst_local_array, status)
      endif
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldRedist: RouteRun returned failure"
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

      end subroutine ESMF_FieldRedistStore


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldRedist - Data Redistribution operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldRedist(srcfield, dstfield, parentlayout, async, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcfield                 
      type(ESMF_Field), intent(inout) :: dstfield                 
      type(ESMF_DELayout), intent(in) :: parentlayout
      type(ESMF_Async), intent(inout), optional :: async
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a {\tt Redistribution} operation over the data
!     in a {\tt ESMF\_Field}.  This routine reads the source field and leaves 
!     the data untouched.  It reads the {\t ESMF\_Grid} and {\tt ESMF\_DataMap}
!     from the destination field and updates the array data in the destination.
!     The {\tt ESMF\_Grid}s may have different decompositions (different
!     {\tt ESMF\_DELayout}s) or different data maps, but the source and
!     destination grids must describe the same set of coordinates.
!     Unlike {\tt ESMF\_Regrid} this routine does not do interpolation,
!     only data movement.
!
!     \begin{description}
!     \item [srcfield] 
!           {\tt ESMF\_Field} containing source data.
!     \item [dstfield] 
!           {\tt ESMF\_Field} containing destination grid.
!     \item [parentlayout]
!           {\tt ESMF\_Layout} which encompasses both {\tt ESMF\_Field}s, 
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
      type(ESMF_FieldType) :: stypep, dtypep      ! field type info
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

      stypep = srcfield%ftypep
      dtypep = dstfield%ftypep

      ! Our DE number in the parent layout
      call ESMF_DELayoutGetDEid(parentlayout, my_DE, status)

      ! TODO: we need not only to know if this DE has data in the field,
      !   but also the de id for both src & dest fields

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
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

      ! if src field exists on this DE, query it for information
      if (hassrcdata) then
          ! Query the datamap and set info for grid so it knows how to
          !  match up the array indices and the grid indices.
          call ESMF_DataMapGet(stypep%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
            print *, "ERROR in FieldRedist: DataMapGet returned failure"
            return
          endif 

          ! And get the Array sizes
          call ESMF_ArrayGet(stypep%localfield%localdata, rank=datarank, &
                                               counts=dimlengths, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
             print *, "ERROR in FieldRedist: ArrayGet returned failure"
             return
          endif 
      endif 

      ! if dst field exists on this DE, query it for information
      if (hasdstdata) then
          ! Query the datamap and set info for grid so it knows how to
          !  match up the array indices and the grid indices.
          call ESMF_DataMapGet(dtypep%mapping, gridrank=gridrank, &
                                               dimlist=dimorder, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
            print *, "ERROR in FieldRedist: DataMapGet returned failure"
            return
          endif 

          ! And get the Array sizes
          call ESMF_ArrayGet(dtypep%localfield%localdata, rank=datarank, &
                                               counts=dimlengths, rc=status)
          if(status .NE. ESMF_SUCCESS) then 
             print *, "ERROR in FieldRedist: ArrayGet returned failure"
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
          call ESMF_ArrayGetAllAxisIndices(stypep%localfield%localdata, &
                                           stypep%grid, src_AI_tot, &
                                           src_AI_exc, rc=rc)
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
          call ESMF_ArrayGetAllAxisIndices(dtypep%localfield%localdata, &
                                           dtypep%grid, dst_AI_tot, &
                                           dst_AI_exc, rc=rc)
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
          ! includes the DEs from both fields.
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
          src_local_array=stypep%localfield%localdata
          call ESMF_RouteRun(route, srcarray=src_local_array, rc=status) 

      else if ((.not. hassrcdata) .and. (hasdstdata)) then
          dst_local_array=dtypep%localfield%localdata
          call ESMF_RouteRun(route, dstarray=dst_local_array, rc=status)

      else
          src_local_array=stypep%localfield%localdata
          dst_local_array=dtypep%localfield%localdata
          call ESMF_RouteRun(route, src_local_array, dst_local_array, status)
      endif
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldRedist: RouteRun returned failure"
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

      end subroutine ESMF_FieldRedist


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldRedistRelease - Release resources associated w/ handle

! !INTERFACE:
      subroutine ESMF_FieldRedistRelease(routehandle, rc)
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
!           {\tt ESMF\_RouteHandle} associated with this Field Redist.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_FieldRedistRelease

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldRegridStore - Data Regrid operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldRegridStore(srcfield, dstfield, parentlayout, &
                                       routehandle, regridtype, &
                                       srcmask, dstmask, blocking, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcfield                 
      type(ESMF_Field), intent(inout) :: dstfield                 
      type(ESMF_DELayout), intent(in) :: parentlayout
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(in), optional :: regridtype 
      type(ESMF_Mask), intent(in), optional :: srcmask                 
      type(ESMF_Mask), intent(in), optional :: dstmask                 
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Precompute a {\tt ESMF\_Regrid} operation over the data
!     in a {\tt ESMF\_Field}.  This routine reads the source field and 
!     leaves the data untouched.  It uses the {\tt ESMF\_Grid} and
!     {\tt ESMF\_DataMap} information in the destination field to
!     control the transformation of data.  The {\tt routehandle} is
!     returned to identify the stored information, and must be supplied
!     to the execution call to actually move the data.
!
!     \begin{description}
!     \item [srcfield] 
!           {\tt ESMF\_Field} containing source data.
!     \item [dstfield] 
!           {\tt ESMF\_Field} containing destination grid and data map.
!     \item [parentlayout]
!           {\tt ESMF\_Layout} which encompasses both {\tt ESMF\_Field}s, 
!           most commonly the layout
!           of the Coupler if the regridding is inter-component, but could 
!           also be the individual layout for a component if the 
!           regridding is intra-component.  
!     \item [routehandle]
!           Output from this call, identifies the precomputed work which
!           will be executed when {\tt ESMF\_FieldRegrid} is called.
!     \item [{[regridtype]}]
!           Type of regridding to do.  A set of predefined types are
!           supplied.
!     \item [{[srcmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!     \item [{[blocking]}]
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
      type(ESMF_Route) :: route
      type(ESMF_DELayout) :: srclayout, dstlayout
      type(ESMF_Logical) :: hasdata        ! does this DE contain localdata?
      logical :: hassrcdata        ! does this DE contain localdata from src?
      logical :: hasdstdata        ! does this DE contain localdata from dst?
      integer :: i
      integer, dimension(ESMF_MAXDIM) :: dimorder, dimlengths, &
                                         global_dimlengths
      integer, dimension(ESMF_MAXGRIDDIM) :: decomps, global_cell_dim
      integer :: my_src_DE, my_dst_DE, my_DE
      type(ESMF_Array) :: src_array, dst_array
      type(ESMF_Grid) :: src_grid, dst_grid
      type(ESMF_DataMap) :: src_datamap, dst_datamap

   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     

      ! Our DE number in the parent layout
      call ESMF_DELayoutGetDEid(parentlayout, my_DE, status)

      ! TODO: we need not only to know if this DE has data in the field,
      !   but also the de id for both src & dest fields

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      ! if srclayout ^ parentlayout == NULL, nothing to send from this DE id.
      call ESMF_FieldGetGrid(srcfield, src_grid, rc=status)
      call ESMF_GridGetDELayout(src_grid, srclayout, status)
 !     call ESMF_DELayoutGetDEExists(parentlayout, my_DE, srclayout, hasdata)
      hassrcdata = (hasdata .eq. ESMF_TRUE) 
      hassrcdata = .true.   ! temp for now
      if (hassrcdata) then
          ! don't ask for our de number if this de isn't part of the layout
          call ESMF_DELayoutGetDEid(srclayout, my_src_DE, status)
          call ESMF_FieldGetData(srcfield, src_array, rc=status)
          call ESMF_FieldGetDataMap(srcfield, src_datamap, rc=status)
      endif

      ! if dstlayout ^ parentlayout == NULL, nothing to recv on this DE id.
      call ESMF_FieldGetGrid(dstfield, dst_grid, rc=status)
      call ESMF_GridGetDELayout(dst_grid, dstlayout, status)
 !     call ESMF_DELayoutGetDEExists(parentlayout, my_DE, dstlayout, hasdata)
      hasdstdata = (hasdata .eq. ESMF_TRUE) 
      hasdstdata = .true.   ! temp for now
      if (hasdstdata) then
          ! don't ask for our de number if this de isn't part of the layout
          call ESMF_DELayoutGetDEid(dstlayout, my_dst_DE, status)
          call ESMF_FieldGetData(dstfield, dst_array, rc=status)
          call ESMF_FieldGetDataMap(dstfield, dst_datamap, rc=status)
      endif

      ! if neither are true this DE cannot be involved in the communication
      !  and it can just return now.
      if ((.not. hassrcdata) .and. (.not. hasdstdata)) then
          if (rcpresent) rc = ESMF_SUCCESS
          return
      endif


 !  TODO: should be parent layout, but for now src=dst=parent
 !     call ESMF_ArrayRegridStore(src_array, src_grid, src_datamap, &      
 !                                dst_grid, dst_datamap, parentlayout, &
 !                                routehandle, regridtype, &    
 !                                srcmask, dstmask, blocking, status)
      call ESMF_ArrayRegridStore(src_array, src_grid, src_datamap, &      
                                 dst_grid, dst_datamap, srclayout, &
                                 routehandle, regridtype, &    
                                 srcmask, dstmask, blocking, status)


      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldRegridStore

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldRegrid - Data Regrid operation on a Field

! !INTERFACE:
      subroutine ESMF_FieldRegrid(srcfield, dstfield, routehandle, &
                                  srcmask, dstmask, blocking, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Field), intent(in) :: srcfield                 
      type(ESMF_Field), intent(inout) :: dstfield                 
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      type(ESMF_Mask), intent(in), optional :: srcmask                 
      type(ESMF_Mask), intent(in), optional :: dstmask                 
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Perform a {\tt ESMF\_Regrid} operation over the data
!     in a {\tt ESMF\_Field}.  This routine reads the source field and 
!     leaves the data untouched.  It uses the {\tt ESMF\_Grid} and
!     {\tt ESMF\_DataMap} information in the destination field to
!     control the transformation of data.  The array data in the 
!     destination field is overwritten by this call.
!
!     \begin{description}
!     \item [srcfield] 
!           {\tt ESMF\_Field} containing source data.
!     \item [dstfield] 
!           {\tt ESMF\_Field} containing destination grid and data map.
!     \item [routehandle]
!           Created by a call to {\tt ESMF\_FieldRegridStore}.
!           Identifies the precomputed work which
!           will be executed when {\tt ESMF\_FieldRegrid} is called.
!     \item [{[srcmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!     \item [{[blocking]}]
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
      type(ESMF_Route) :: route
      type(ESMF_DELayout) :: srclayout, dstlayout, parentlayout
      type(ESMF_Logical) :: hasdata        ! does this DE contain localdata?
      logical :: hassrcdata        ! does this DE contain localdata from src?
      logical :: hasdstdata        ! does this DE contain localdata from dst?
      integer :: my_src_DE, my_dst_DE, my_DE
      type(ESMF_Array) :: src_array, dst_array
      type(ESMF_Grid) :: src_grid, dst_grid
      type(ESMF_DataMap) :: src_datamap, dst_datamap

   
      ! Initialize return code   
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE. 
        rc = ESMF_FAILURE
      endif     


      ! Our DE number in the parent layout
      ! call ESMF_DELayoutGetDEid(parentlayout, my_DE, status)

      ! TODO: we need not only to know if this DE has data in the field,
      !   but also the de id for both src & dest fields

      ! This routine is called on every processor in the parent layout.
      !  It is quite possible that the source and destination fields do
      !  not completely cover every processor on that layout.  Make sure
      !  we do not go lower than this on the processors which are uninvolved
      !  in this communication.

      ! if srclayout ^ parentlayout == NULL, nothing to send from this DE id.
      call ESMF_FieldGetGrid(srcfield, src_grid, rc=status)
      call ESMF_GridGetDELayout(src_grid, srclayout, status)
 !     call ESMF_DELayoutGetDEExists(parentlayout, my_DE, srclayout, hasdata)
      hassrcdata = (hasdata .eq. ESMF_TRUE) 
      hassrcdata = .true.   ! temp for now
      if (hassrcdata) then
          ! don't ask for our de number if this de isn't part of the layout
          call ESMF_DELayoutGetDEid(srclayout, my_src_DE, status)
          call ESMF_FieldGetData(srcfield, src_array, rc=status)
          call ESMF_FieldGetDataMap(srcfield, src_datamap, rc=status)
      endif

      ! if dstlayout ^ parentlayout == NULL, nothing to recv on this DE id.
      call ESMF_FieldGetGrid(dstfield, dst_grid, rc=status)
      call ESMF_GridGetDELayout(dst_grid, dstlayout, status)
 !     call ESMF_DELayoutGetDEExists(parentlayout, my_DE, dstlayout, hasdata)
      hasdstdata = (hasdata .eq. ESMF_TRUE) 
      hasdstdata = .true.   ! temp for now
      if (hasdstdata) then
          ! don't ask for our de number if this de isn't part of the layout
          call ESMF_DELayoutGetDEid(dstlayout, my_dst_DE, status)
          call ESMF_FieldGetData(dstfield, dst_array, rc=status)
          call ESMF_FieldGetDataMap(dstfield, dst_datamap, rc=status)
      endif

      ! if neither are true this DE cannot be involved in the communication
      !  and it can just return now.
      if ((.not. hassrcdata) .and. (.not. hasdstdata)) then
          if (rcpresent) rc = ESMF_SUCCESS
          return
      endif


      ! There are 3 possible cases - src+dst, src only, dst only
      !  (if both are false then we've already returned.)
      if ((hassrcdata) .and. (.not. hasdstdata)) then
          !call ESMF_ArrayRegrid(src_array, dst_array, routehandle, &       
          !                        srcmask, dstmask, blocking, rc)
      else if ((.not. hassrcdata) .and. (hasdstdata)) then
          !call ESMF_ArrayRegrid(src_array, dst_array, routehandle, &       
          !                        srcmask, dstmask, blocking, rc)
      else
          call ESMF_ArrayRegrid(src_array, dst_array, routehandle, &       
                                srcmask, dstmask, blocking, rc)
      endif
      if(status .NE. ESMF_SUCCESS) then 
        print *, "ERROR in FieldRegrid: RouteRun returned failure"
        return
      endif 


      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldRegrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_FieldRegridRelease - Release information for this handle

! !INTERFACE:
      subroutine ESMF_FieldRegridRelease(routehandle, rc)
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
!           {\tt ESMF\_RouteHandle} associated with this Field Regridding.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!           
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      call ESMF_RouteHandleDestroy(routehandle, rc)

      end subroutine ESMF_FieldRegridRelease

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      end module ESMF_FieldCommMod
