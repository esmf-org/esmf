! $Id: ESMF_Regrid.F90,v 1.70 2004/04/27 23:07:47 jwolfe Exp $
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
!     ESMF Regrid Module
      module ESMF_RegridMod
!
!==============================================================================
!
! This file contains most of the Regrid class methods.  The Regrid data type
! and some utility functions are contained in RegridTypesMod.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_RegridMod - Regridding and interpolation
!
! !DESCRIPTION:
!
! The code in this file implements most of the Regrid class methods.  Regrid 
! is responsible for any regridding and interpolation required for ESMF 
! applications.
! Regridding includes any process that transforms a field from one ESMF
! grid to another, including:
! \begin{itemize}
! \item bilinear or bicubic interpolation
! \item conservative remapping
! \item spectral or other functional transforms
! \item sub-sampling, super-sampling or shifting.
! \end{itemize}
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod         ! ESMF base   class
      use ESMF_DataMapMod      ! ESMF datamap class
      use ESMF_newDELayoutMod  ! ESMF DE layout class
      use ESMF_ArrayMod        ! ESMF array  class
      use ESMF_ArrayGetMod     ! ESMF array  class
      use ESMF_DistGridMod     ! ESMF distributed grid class
      use ESMF_PhysGridMod     ! ESMF physical grid class
      use ESMF_GridMod         ! ESMF grid   class
      use ESMF_RHandleMod      ! ESMF route handle class
      use ESMF_RouteMod        ! ESMF route  class
      use ESMF_ArrayCommMod    ! ESMF array comm class
      use ESMF_FieldMod        ! ESMF field  class
      use ESMF_BundleMod       ! ESMF bundle class
      use ESMF_RegridTypesMod  ! ESMF regrid data types and utilities
      use ESMF_RegridBilinearMod ! ESMF rg methods related to bilinear regrid
      use ESMF_RegridNearNbrMod  ! ESMF rg methods related to nearest-nbr regrid
      use ESMF_RegridConservMod  ! ESMF rg methods related to conservative regrid
      use ESMF_RegridLinearMod   ! ESMF rg methods related to linear regrid

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     !  ESMF_Regrid
!
!     !  The Regrid data structure that is passed between languages.

      type ESMF_Regrid
      sequence
      private
        type (ESMF_RegridType), pointer :: ptr     ! pointer to a regrid type
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!
    ! TODO: This should not be a public object.  It's here right now
    !  to get the code to compile, but it should be removed asap.
    public ESMF_Regrid

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    ! These are wrapper routines which call RegridCreate to do the
    !  actual work.  Since all our routines are data centric methods
    !  and we are not exposing an externally visible "regrid" object, 
    !  these routines must exist to be consistent with the other interfaces.  
    ! 
    public ESMF_ArrayRegridStore, ESMF_ArrayRegrid, ESMF_ArrayRegridRelease
    public ESMF_RegridGet        ! returns value of a regrid attribute
    public ESMF_RegridDestroy    ! deallocate memory associated with a regrid
    public ESMF_RegridValidate   ! Error checking and validation
    public ESMF_RegridPrint      ! Prints various regrid info

!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
         '$Id: ESMF_Regrid.F90,v 1.70 2004/04/27 23:07:47 jwolfe Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes some of the Regrid Create methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridCreate - Precomputes Regrid data

! !INTERFACE:
      subroutine ESMF_RegridCreate(srcarray, srcgrid, srcdatamap, &
                                   dstarray, dstgrid, dstdatamap, &
                                   parentDELayout, routehandle, regridmethod, &
                                   srcmask, dstmask, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: srcarray
      type(ESMF_Grid), intent(inout) :: srcgrid
      type(ESMF_DataMap), intent(in) :: srcdatamap
      type(ESMF_Array), intent(inout) :: dstarray
      type(ESMF_Grid), intent(inout) :: dstgrid
      type(ESMF_DataMap), intent(in) :: dstdatamap
      type(ESMF_newDELayout), intent(in) :: parentDELayout
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(in), optional :: regridmethod
      type(ESMF_Mask), intent(in), optional :: srcmask
      type(ESMF_Mask), intent(in), optional :: dstmask
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source array, grid, and datamap, this routine precomputes
!     both the communication pattern needed to move data to the proper
!     processors, and the sparse matrix of weights needed to compute the data
!     interpolation for moving data from one grid to another.
!     This routine returns a handle to the precomputed information in the
!     {\tt routehandle} argument.  This same value should be supplied
!     at run time, along with the actual data pointers.  The same precomputed
!     handle can be used on any data which matches the data arrays, grid,
!     and datamaps supplied here, so one does not have to generate multiple
!     routehandles for similar data values.
!
!     TODO: Do we need the parent layout here at this level?  I believe not,
!     because for exclusive processor sets the higher level code should
!     have created proxy grid objects which then are both on the same layout.
!
!     The arguments are:
!     \begin{description}
!     \item [srcarray]
!           {\tt ESMF\_Array} containing source data.
!     \item [srcgrid]
!           {\tt ESMF\_Grid} which corresponds to how the data in the
!           source array has been decomposed.
!     \item [srcdatamap]
!           {\tt ESMF\_DataMap} which describes how the array maps to
!           the specified source grid.
!     \item [dstgrid]
!           {\tt ESMF\_Grid} which corresponds to how the data in the
!           destination array should be decomposed.
!     \item [dstdatamap]
!           {\tt ESMF\_DataMap} which describes how the array should map to
!           the specified destination grid.
!     \item [routehandle]
!           Returned value which identifies the precomputed Route and other
!           necessary information.
!     \item [{[regridtype]}]
!           Type of regridding to do.  A set of predefined types are
!           supplied.
!     \item [{[srcmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!   The supported regridding methods for this create function are currently:
!   \begin{description}
!   \item[ESMF\_RegridMethod\_FieldCopy] same Grid, just copy the field
!   \item[ESMF\_RegridMethod\_Redist  ] same PhysGrid just redistribute field
!   \item[ESMF\_RegridMethod\_Bilinear] bilinear (logically-rectangular grids)
!   \item[ESMF\_RegridMethod\_Bicubic ] bicubic  (logically-rectangular grids)
!   \item[ESMF\_RegridMethod\_Conserv1] first-order conservative
!   \item[ESMF\_RegridMethod\_Conserv2] second-order conservative
!   \item[ESMF\_RegridMethod\_Raster  ] regrid by rasterizing domain
!   \item[ESMF\_RegridMethod\_NearNbr ] nearest-neighbor distance-weighted average
!   \item[ESMF\_RegridMethod\_Fourier ] Fourier transform
!   \item[ESMF\_RegridMethod\_Legendre] Legendre transform
!   \item[ESMF\_RegridMethod\_Index   ] index-space regridding (shift, stencil)
!   \item[ESMF\_RegridMethod\_Linear  ] linear for 1-d regridding
!   \item[ESMF\_RegridMethod\_Spline  ] cubic spline for 1-d regridding
!   \item[ESMF\_RegridMethod\_User    ] user-supplied method
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      ! TODO: the interfaces have changed - this will no longer be called
      !  with fields, but with the grid, datamap, and array which are either
      !  contained in a field or are specified by the user separately in an
      !  array method call.  i believe the code at this level needs to 
      !  take an already declared routehandle and fill it in with the
      !  route and weights that it computes.  we have no "Regrid" object
      !  at this point - it is a subtype of a routehandle object.  so this
      !  could either compute the route and weights and return them, or
      !  it seems maybe better to fill them in here in case we end up with
      !  more than just a single route and weight array.
      ! TODO: so this code needs to be overhauled...   

      integer :: status             ! Error status
      logical :: rcpresent          ! Return code present
      character (len=ESMF_MAXSTR) :: regrid_name

      ! Initialize return code
      rcpresent = .FALSE.
      status = ESMF_FAILURE
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      !! TODO:  TEMPORARY CODE TO BYPASS REAL REGRID CODE
      !!  remove these next 2 lines to finish debugging regrid code.
  !    routehandle = ESMF_RouteHandleCreate(rc)
  !    if(present(rc)) rc = ESMF_SUCCESS
  !    return
      !! END BYPASS
      
      ! Call the appropriate create routine based on method choice

      select case(regridmethod)

      !-------------
      case(ESMF_RegridMethod_FieldCopy) ! copy field
         !*** no regrid type required
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Field copy not yet supported"
         status = ESMF_FAILURE

      !-------------
      case(ESMF_RegridMethod_Redist)   ! redistribution of field
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Redistribution not yet supported"
         status = ESMF_FAILURE

      !-------------
      case(ESMF_RegridMethod_Bilinear) ! bilinear
          routehandle = ESMF_RegridConstructBilinear( &
                                              srcarray, srcgrid, srcdatamap, &
                                              dstarray, dstgrid, dstdatamap, &
                                              parentDELayout, srcmask, dstmask, &
                                              rc=status)

      !-------------
      case(ESMF_RegridMethod_Bicubic)  ! bicubic
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Bicubic not yet supported"
         status = ESMF_FAILURE

      !-------------
      case(ESMF_RegridMethod_Conserv1)
          routehandle = ESMF_RegridConstructConserv( &
                                              srcarray, srcgrid, srcdatamap, &
                                              dstarray, dstgrid, dstdatamap, &
                                              parentDELayout, srcmask, dstmask, &
                                              order=1, rc=status)
      !-------------
      case(ESMF_RegridMethod_Conserv2) ! 2nd-order conservative
      !   routehandle = ESMF_RegridConstructConserv(srcarray, dstarray, &
      !                                        regrid_name, order=2, rc=status)
      !-------------
      case(ESMF_RegridMethod_Raster) ! regrid by rasterizing domain
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Raster method not yet supported"
         status = ESMF_FAILURE
      !-------------
      case(ESMF_RegridMethod_NearNbr) ! nearest-neighbor dist-weighted avg
      !   routehandle = ESMF_RegridConstructNearNbr(srcarray, dstarray, &
      !                                        regrid_name, rc=status)
      !-------------
      case(ESMF_RegridMethod_Fourier) ! Fourier transform
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Fourier transforms not yet supported"
         status = ESMF_FAILURE
      !-------------
      case(ESMF_RegridMethod_Legendre) ! Legendre transform
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Legendre transforms not yet supported"
         status = ESMF_FAILURE
      !-------------
      case(ESMF_RegridMethod_Index) ! index-space regridding (shift, stencil)
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "Index-space methods not yet supported"
         status = ESMF_FAILURE
      !-------------
      case(ESMF_RegridMethod_Linear) ! linear for 1-d regridding
          routehandle = ESMF_RegridConstructLinear( &
                                              srcarray, srcgrid, srcdatamap, &
                                              dstarray, dstgrid, dstdatamap, &
                                              parentDELayout, srcmask, dstmask, &
                                              rc=status)
      !-------------
      case(ESMF_RegridMethod_Spline) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "1-d cubic splines not yet supported"
         status = ESMF_FAILURE
      !-------------
      case(ESMF_RegridMethod_User) ! cubic spline for 1-d regridding
         print *, "ERROR in ESMF_RegridCreate: ", &
                  "User-defined regridding not yet supported"
         status = ESMF_FAILURE
      !-------------
      case default
         print *, "ERROR in ESMF_RegridCreate: Invalid method"
         status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
         ! Use error function eventually...
         print *, 'ERROR in ESMF_RegridCreate: error in creation'
      endif

      if (rcpresent) rc = status

      end subroutine ESMF_RegridCreate

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RegridRun - Performs a regridding between two arrays

! !INTERFACE:
      subroutine ESMF_RegridRun(srcarray, dstarray, srcDataMap, dstDataMap, &
                                routehandle, rc)
!
! !ARGUMENTS:

      type(ESMF_Array), intent(in) :: srcarray    ! array to be regridded
      type(ESMF_Array), intent(inout) :: dstarray   ! resulting regridded array
      type (ESMF_DataMap), intent(in) :: srcDatamap
      type (ESMF_DataMap), intent(in) :: dstDatamap
      type(ESMF_RouteHandle), intent(in) :: routehandle 
                                                  ! precomputed regrid structure
                                                  ! with regridding info
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source array and precomputed regridding information, this 
!     routine regrids the source array to a new array on the destination
!     grid.  
!
!EOPI
      integer :: status
      logical :: rcpresent
      integer :: i, i2, n, d1, d2, s1, s2, asize, rank, counts(ESMF_MAXDIM)
      integer :: dstUndecomp, srcUndecomp, srcUndecompSize, srcStride
      integer :: di1, di2, dj1, dj2, dk1, dk2
      integer :: si1, si2, sj1, sj2, sk1, sk2
      integer, dimension(:), allocatable :: dstDimOrder, srcDimOrder
      integer, dimension(:,:), allocatable :: dindex, sindex
      integer(ESMF_KIND_I4), dimension(:), pointer :: dstIndex, srcIndex
      real(ESMF_KIND_R8) :: zero
      real(ESMF_KIND_R8), dimension(:), pointer :: weights
      real(ESMF_KIND_R8), dimension(:), pointer :: gatheredData
      real(ESMF_KIND_R8), dimension(:,:), pointer :: dstData2D
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: dstData3D
      real(ESMF_KIND_R8), dimension(:,:,:,:), pointer :: dstData4D
      type(ESMF_DataType) :: type
      type(ESMF_DataKind) :: kind
      type(ESMF_Route) :: rh
      type(ESMF_LocalArray) :: srcindexarr, dstindexarr, weightsarr
      integer :: numlinks
      type(ESMF_LocalArray) :: gatheredArray, srcLocalArray
      type(ESMF_TransformValues) :: tv

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

     zero = 0.0
     ! get the first route from the table and run it to gather the
     ! data values which overlap this bounding box.
 
     call ESMF_RouteHandleGet(routehandle, route1=rh, rc=status)

     ! get the indirect indices and weights from the routehandle
     call ESMF_RouteHandleGet(routehandle, tdata=tv, rc=status)

     ! get a real f90 pointer from all the arrays
     ! srcIndex, dstIndex and weights TKR can be fixed, but unfortunately the
     ! gatheredData and dstData can be whatever the user wants - so this code
     ! might need to move into another file and be macroized heavily for TKR.
     call ESMF_TransformValuesGet(tv, numlist=numlinks, srcindex=srcindexarr, &
                                  dstindex=dstindexarr, weights=weightsarr, rc=rc)
     call ESMF_LocalArrayGetData(srcindexarr, srcIndex, ESMF_DATA_REF, rc)
     call ESMF_LocalArrayGetData(dstindexarr, dstIndex, ESMF_DATA_REF, rc)
     call ESMF_LocalArrayGetData(weightsarr, weights, ESMF_DATA_REF, rc)

     ! from the domain or from someplace, get the counts of how many data points
     ! we are expecting from other DEs.  we might also need to know what
     ! data type is coming since this is user data and not coordinates at 
     ! execution time.  or does the incoming data have to match the type
     ! of the outgoing array?  so we can get the data type and shape from
     ! the dstarray argument to this function.  and what about halo widths?

     call ESMF_RouteGetRecvItems(rh, asize, status)

     call ESMF_ArrayGet(srcarray, rank=rank, type=type, kind=kind, &
                        counts=counts, rc=status)
     gatheredArray = ESMF_LocalArrayCreate(1, type, kind, asize, rc=status)
     srcLocalArray = srcarray
     call ESMF_RouteRun(rh, srcLocalArray, gatheredArray, status)

     allocate(dstDimOrder(rank))
     allocate(srcDimOrder(rank))
     call ESMF_DataMapGet(dstDataMap, dataIorder=dstDimOrder, rc=status)
     call ESMF_DataMapGet(srcDataMap, dataIorder=srcDimOrder, rc=status)

     ! break out here by rank   TODO: compare datarank to regridrank (or
     !                                compare datamaps)
     select case(rank)

     ! TODO: apply the weights from src to destination
     !  does this need a nested loop and an array of ESMF_Arrays, one
     !  for each DE which sends data?  i think the answer is not for now
     !  because all data has been pushed into a single array.  but eventually
     !  if we want to start supporting vectors or other complicated data 
     !  shapes, we may have to start preserving the array and datamaps
     !  from the original locations.

     !-------------
     case(2) ! 2D data for regrid

       call ESMF_LocalArrayGetData(gatheredArray, gatheredData, &
                                   ESMF_DATA_REF, rc)
       call ESMF_ArrayGetData(dstarray, dstData2D, ESMF_DATA_REF, rc)

       !*** initialize dest field to zero
   
       dstData2D = zero

       !*** do the regrid

       do n = 1,numlinks
         d1 = dstIndex((n-1)*2 + 1)
         d2 = dstIndex((n-1)*2 + 2)
         s1 = srcIndex(n)
         dstData2D(d1,d2) = dstData2D(d1,d2) &
                          + (gatheredData(s1) * weights(n))
       enddo

     !-------------
     case(3) ! 3D data for regrid

       call ESMF_LocalArrayGetData(gatheredArray, gatheredData, &
                                   ESMF_DATA_REF, rc)
       call ESMF_ArrayGetData(dstarray, dstData3D, ESMF_DATA_REF, rc)

       !*** initialize dest field to zero
   
       dstData3D = zero

       !*** for cases where datarank>gridrank, figure out the undecomposed
       dstUndecomp     = 0   !TODO: should be arrays to be general
       srcUndecomp     = 0
       srcUndecompSize = 0
       srcStride       = 1
       do i = 1,rank
         if (dstDimOrder(i).eq.0) then
           dstUndecomp = i
         endif
         if (srcDimOrder(i).eq.0) then
           srcUndecomp = i
           srcUndecompSize = counts(i)
         else
           srcStride = srcStride * counts(i)
         endif
       enddo

       !*** special code if Undecomp = 1 or rank
       if (srcUndecomp.eq.1 .and. dstUndecomp.eq.1) then

         !*** do the regrid
         i2 = size(dstData3D,1)
         do n = 1,numlinks
           d1 = dstIndex((n-1)*2 + 1)
           d2 = dstIndex((n-1)*2 + 2)
           s1 = (srcIndex(n)-1)*i2        ! assumes i2 = srcUndecompSize
           do i = 1,i2
             dstData3D(i,d1,d2) = dstData3D(i,d1,d2) &
                                + (gatheredData(s1+i) * weights(n))
           enddo
         enddo

       elseif (srcUndecomp.eq.rank .and. dstUndecomp.eq.rank) then

         !*** do the regrid
         i2 = size(dstData3D,rank)
         do i = 1,i2
           do n = 1,numlinks
             d1 = dstIndex((n-1)*2 + 1)
             d2 = dstIndex((n-1)*2 + 2)
             s1 = (i-1)*srcStride + srcIndex(n)
             dstData3D(d1,d2,i) = dstData3D(d1,d2,i) &
                                + (gatheredData(s1) * weights(n))
           enddo
         enddo

       elseif (srcUndecomp.eq.1 .and. dstUndecomp.eq.rank) then

         !*** do the regrid
         i2 = size(dstData3D,rank)
         do i = 1,i2
           do n = 1,numlinks
             d1 = dstIndex((n-1)*2 + 1)
             d2 = dstIndex((n-1)*2 + 2)
             s1 = (srcIndex(n)-1)*i2 + i    ! assumes i2 = srcUndecompSize
             dstData3D(d1,d2,i) = dstData3D(d1,d2,i) &
                                + (gatheredData(s1) * weights(n))
           enddo
         enddo

       elseif (srcUndecomp.eq.rank .and. dstUndecomp.eq.1) then

         !*** do the regrid
         i2 = size(dstData3D,1)
         do n = 1,numlinks
           d1 = dstIndex((n-1)*2 + 1)
           d2 = dstIndex((n-1)*2 + 2)
           do i = 1,i2
             s1 = (i-1)*srcStride + srcIndex(n)
             dstData3D(i,d1,d2) = dstData3D(i,d1,d2) &
                                + (gatheredData(s1) * weights(n))
           enddo
         enddo

       else
         allocate(dindex(rank,2))
         allocate(sindex(rank,2))
         do n = 1,numlinks
           dindex(1,1) = 1
           dindex(1,2) = size(dstData3D,dstUndecomp)
           dindex(2,1) = dstIndex((n-1)*2 + 1)
           dindex(2,2) = dstIndex((n-1)*2 + 1)
           dindex(3,1) = dstIndex((n-1)*2 + 2)
           dindex(3,2) = dstIndex((n-1)*2 + 2)
           di1 = dindex(dstDimOrder(1)+1,1)
           di2 = dindex(dstDimOrder(1)+1,2)
           dj1 = dindex(dstDimOrder(2)+1,1)
           dj2 = dindex(dstDimOrder(2)+1,2)
           dk1 = dindex(dstDimOrder(3)+1,1)
           dk2 = dindex(dstDimOrder(3)+1,2)
           sindex(1,1) = 1
           sindex(1,2) = size(gatheredData,srcUndecomp)
           sindex(2,1) = srcIndex((n-1)*2 + 1)
           sindex(2,2) = srcIndex((n-1)*2 + 1)
           sindex(3,1) = srcIndex((n-1)*2 + 2)
           sindex(3,2) = srcIndex((n-1)*2 + 2)
           si1 = sindex(srcDimOrder(1)+1,1)
           si2 = sindex(srcDimOrder(1)+1,2)
           sj1 = sindex(srcDimOrder(2)+1,1)
           sj2 = sindex(srcDimOrder(2)+1,2)
           sk1 = sindex(srcDimOrder(3)+1,1)
           sk2 = sindex(srcDimOrder(3)+1,2)
   !        dstData3D(di1:di2,dj1:dj2,dk1:dk2) = &
   !                 dstData3D(di1:di2,dj1:dj2,dk1:dk2) &
   !              + (gatheredData(si1:si2,sj1:sj2) * weights(n))

         enddo
       endif

     case default
       print *, "ERROR in ESMF_RegridRun: Invalid rank"
       status = ESMF_FAILURE
     end select

     ! set return codes
     ! nuke temp array

     end subroutine ESMF_RegridRun

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridGet - Get an attribute of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridGet(regrid, name,            &
                                srcArray, dstArray,  &
                                srcGrid,  dstGrid,   &
                                srcDatamap,  dstDatamap,   &
                                method, numLinks , gather, rc)
!
! !ARGUMENTS:

      type(ESMF_Regrid),    intent(inout) :: regrid
      character (*),        intent(out), optional :: name
      type (ESMF_Array),    intent(out), optional :: srcArray, dstArray
      type (ESMF_Grid),     intent(out), optional :: srcGrid,  dstGrid
      type (ESMF_DataMap),  intent(out), optional :: srcDatamap,  dstDatamap
      integer,              intent(out), optional :: method
      integer,              intent(out), optional :: numLinks
      type (ESMF_Route),    intent(out), optional :: gather
      integer,              intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns the value of a Regrid attribute.  The
!     attribute is specified through optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Regrid to be queried.
!     \item[TODO:] fix this doc - make it match arg list and fix double
!          brackets to be bracket, curley brace, bracket.
!     \item[{[name]}]
!          Name for this regrid.
!     \item[{[srcArray]}]
!          
!     \item[{[dstArray]}]
!          
!     \item[{[srcGrid]}]
!          
!     \item[{[dstGrid]}]
!          
!     \item[{[srcDatamap]}]
!          
!     \item[{[dstDatamap]}]
!          
!     \item[{[method]}]
!          Integer enum of method used in this regrid.
!     \item[{[numLinks]}]
!          Number of unique links between grids for this regrid.
!     \item[{[gather]}]
!          Route used to gather non-local information to perform regrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!  TODO

      logical :: rcpresent
      integer :: status

      ! Assume failure until sure of success
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then 
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif
      
      ! Get name if requested
      if (present(name)) then
         call ESMF_RegridTypeGet(regrid%ptr, name=name, rc=status)
         if (status .ne. ESMF_SUCCESS) then
             print *, "ESMF_RegridGet: failed getting regrid name"
             return
         endif
      endif

! TODO: make these arrays, grid, datamaps.   Are we really going to 
!  store the arrays & grids this was based on?  why?   nsc.
!      ! Get bundles if requested
!      if (present(srcbundle)) then
!         call ESMF_RegridTypeGet(regrid%ptr, srcbundle=srcbundle, rc=status)
!        if (status .ne. ESMF_SUCCESS) then
!            print *, "ESMF_RegridGet: failed getting regrid bundle"
!            return
!        endif
!      endif
!      if (present(dstbundle)) then
!         call ESMF_RegridTypeGet(regrid%ptr, dstbundle=dstbundle, rc=status)
!        if (status .ne. ESMF_SUCCESS) then
!            print *, "ESMF_RegridGet: failed getting regrid bundle"
!            return
!        endif
!      endif
!
!      ! Get fields if requested
!      if (present(srcfield)) then
!         call ESMF_RegridTypeGet(regrid%ptr, srcfield=srcfield, rc=status)
!        if (status .ne. ESMF_SUCCESS) then
!            print *, "ESMF_RegridGet: failed getting regrid ..."
!            return
!        endif
!      endif
!      if (present(dstfield)) then
!         call ESMF_RegridTypeGet(regrid%ptr, dstfield=dstfield, rc=stat)
!        if (status .ne. ESMF_SUCCESS) then
!            print *, "ESMF_RegridGet: failed getting regrid ..."
!            return
!        endif
!         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
!      endif
!
      ! get method or number of links or gather route
      if (present(method)) then
         call ESMF_RegridTypeGet(regrid%ptr, method=method, rc=status)
         if (status .ne. ESMF_SUCCESS) then
             print *, "ESMF_RegridGet: failed getting regrid ..."
             return
         endif
      endif
      if (present(numLinks)) then
         call ESMF_RegridTypeGet(regrid%ptr, numLinks=numLinks, rc=status)
         if (status .ne. ESMF_SUCCESS) then
             print *, "ESMF_RegridGet: failed getting regrid ..."
             return
         endif
      endif
      if (present(gather)) then
         call ESMF_RegridTypeGet(regrid%ptr, gather=gather, rc=status)
         if (status .ne. ESMF_SUCCESS) then
             print *, "ESMF_RegridGet: failed getting regrid ..."
             return
         endif
      endif

      if (rcpresent) rc = status

      end subroutine ESMF_RegridGet

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RegridDestroy - Free all resources associated with a Regrid

! !INTERFACE:
      subroutine ESMF_RegridDestroy(routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Destroys all a {\tt Regrid} object previously allocated
!     via an {\tt ESMF\_RegridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          The regrid object to be destroyed.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: status             ! Error status
      logical :: rcpresent          ! Return code present

      ! Initialize return code
      rcpresent = .FALSE.
      status = ESMF_FAILURE
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Does this destroy each of the routes?  it certainly needs to
      !  destroy the arrays in the TransformValues object.  (i think the
      !  answer is yes - this code should call route delete here.)

      !call ESMF_RouteHandleGet(routehandle, rhandle1=rh, transformvalues=tv, &
      !                          rc=status)
      !call ESMF_RouteDestroy(rh, status)
      !call ESMF_TransformValuesDestroy(tv, status)
      if (status /= ESMF_SUCCESS) then
        ! Use error function eventually...
        print *, "ERROR in ESMF_RegridDestroy: Regrid destruct"
        return
      endif

      ! Set return values.
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegridValidate - Check internal consistency of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridValidate(routehandle, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: routehandle
      character (len=*), intent(in), optional :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Validates that a Regrid is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[routehandle]
!          Class to be queried.
!     \item[[opt]]
!          Validation options.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

      ! 
      ! code goes here
      ! 

      end subroutine ESMF_RegridValidate

!------------------------------------------------------------------------------
! TODO: this routine should be BOP once it is filled in
!BOPI
! !IROUTINE: ESMF_RegridPrint - Print the contents of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridPrint(routehandle, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: routehandle
      character (len=*), intent(in) :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Print information about a Regrid.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Class to be queried.
!     \item[[opt]]
!          Print ptions that control the type of information and level of
!          detail.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ! TODO: does this even need to be here?  it seems the print and
      ! validate routines should be in the routehandle code only.

      ! 
      ! code goes here
      ! 

      end subroutine ESMF_RegridPrint

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Non-Regrid methods which must be here to be able to call RegridCreate,
! RegridRun, etc.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayRegridStore(srcarray, srcgrid, srcdatamap, &
                                       dstgrid, dstdatamap, parentDElayout, &
                                       routehandle, regridtype, &
                                       srcmask, dstmask, rc) 
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: srcarray
      type(ESMF_Grid), intent(inout) :: srcgrid
      type(ESMF_DataMap), intent(in) :: srcdatamap
      type(ESMF_Grid), intent(inout) :: dstgrid
      type(ESMF_DataMap), intent(in) :: dstdatamap
      type(ESMF_newDELayout) :: parentDElayout
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(in), optional :: regridtype
      type(ESMF_Mask), intent(in), optional :: srcmask
      type(ESMF_Mask), intent(in), optional :: dstmask
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to Regrid data in an Array.
!
!     \begin{description}
!     \item [srcarray]
!           {\tt ESMF\_Array} containing source data.
!     \item [srcgrid]
!           {\tt ESMF\_Grid} which corresponds to how the data in the
!           source array has been decomposed.  
!     \item [srcdatamap]
!           {\tt ESMF\_DataMap} which describes how the array maps to
!           the specified source grid.
!     \item [dstgrid]
!           {\tt ESMF\_Grid} which corresponds to how the data in the
!           destination array should be decomposed.  
!     \item [dstdatamap]
!           {\tt ESMF\_DataMap} which describes how the array should map to
!           the specified destination grid.
!     \item [parentDElayout]
!           {\tt ESMF\_DELayout} which encompasses both {\tt ESMF\_Field}s,
!           most commonly the layout
!           of the Coupler if the regridding is inter-component, but could
!           also be the individual layout for a component if the
!           regridding is intra-component.
!     \item [routehandle]
!           Returned value which identifies the precomputed Route and other
!           necessary information.
!     \item [{[regridtype]}]
!           Type of regridding to do.  A set of predefined types are
!           supplied.
!     \item [{[srcmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!

!
!
!EOP
! !REQUIREMENTS:
        integer :: status         ! local error status
        logical :: rcpresent      ! did user specify rc?
        integer :: size_rank_trans
        integer :: size_decomp
        type(ESMF_Array) :: dstarray     ! is this really needed?

        ! assume failure until success certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! TODO: add code here
        !  The form of this code depends on how we organize the interfaces
        !  between the Regrid code and this code.  This is the lowest level
        !  public interface to the Regrid code, so anything we do below
        !  here will be internal interfaces and not public.  The interfaces
        !  need to be as useful to the regrid code as possible.

        call ESMF_RegridCreate(srcarray, srcgrid, srcdatamap, &   
                               dstarray, dstgrid, dstdatamap, &
                               parentDELayout, routehandle, regridtype, &
                               srcmask, dstmask, status)

        ! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayRegridStore

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayRegrid(srcarray, dstarray, srcDataMap, dstDataMap, &
                                  routehandle, srcmask, dstmask, blocking, commhandle, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: srcarray
      type(ESMF_Array), intent(inout) :: dstarray
      type(ESMF_DataMap), intent(inout) :: srcDataMap
      type(ESMF_DataMap), intent(inout) :: dstDataMap
      type(ESMF_RouteHandle), intent(in) :: routehandle
      type(ESMF_Mask), intent(in), optional :: srcmask
      type(ESMF_Mask), intent(in), optional :: dstmask
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a {\tt Regrid} operation over the data in an {\tt ESMF\_Array}.
!     This routine updates the data inside the {\tt ESMF\_Array} in place.
!     It uses a precomputed {\tt ESMF\_Route} for the communications
!     pattern.
!
!     \begin{description}
!     \item [srcarray]
!           {\tt ESMF\_Array} containing source data to be regridded.
!     \item [dstarray]
!           {\tt ESMF\_Array} containing locations to put regridded data.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} has been precomputed.
!     \item [{[srcmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
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
!
!EOP
! !REQUIREMENTS:
      integer :: status         ! local error status
      logical :: rcpresent      ! did user specify rc?

      ! initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
 
      ! Execute the communications call.
      call ESMF_RegridRun(srcarray, dstarray, srcDataMap, dstDataMap, &
                          routehandle, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ArrayRegrid: RegridRun returned failure"
        return
      endif

        ! Set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayRegrid

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayRegridRelease(routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Release the information stored about this Regrid operation.
!
!     \begin{description}
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} associated with Regrid that is no longer
!           needed.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc=rc)

      end subroutine ESMF_ArrayRegridRelease


   end module ESMF_RegridMod
