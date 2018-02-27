!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!!-------------------------------------------------------------------------------------

!==============================================================================
#define ESMF_FILENAME "ESMF_RegridWeightGenCheck.F90"
!==============================================================================
!
module ESMF_RegridWeightGenCheckMod
!
!==============================================================================
!
! This file contains the API wrapper for the ESMF_RegridWeightGenCheck application
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
! !USES:
#ifdef ESMF_NETCDF
  use netcdf
#endif
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod
  use ESMF_VMMod
  use ESMF_ArraySpecMod
  use ESMF_ArrayMod
  use ESMF_DistGridMod
  use ESMF_GridMod
  use ESMF_FieldMod
  use ESMF_FieldCreateMod
  use ESMF_FieldSMMMod
  use ESMF_FieldRegridMod
  use ESMF_RHandleMod
  use ESMF_FactorReadMod

  ! unused imports
  !use ESMF_LogPublicMod
  !use ESMF_IOScripMod
  !use ESMF_IOGridspecMod
  !use ESMF_IOUGridMod
  !use ESMF_GridUtilMod
  !use ESMF_StaggerLocMod
  !use ESMF_MeshMod
  !use ESMF_FieldGetMod
  !use ESMF_FieldGatherMod


  implicit none

!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:

  public ESMF_RegridWeightGenCheck

!------------------------------------------------------------------------------

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridWeightGenCheck"

!BOPI
! !IROUTINE: ESMF_RegridWeightGenCheck - Check regridding weights
! !INTERFACE:
  subroutine ESMF_RegridWeightGenCheck(weightFile, keywordEnforcer, &
                                       checkMethod, rc)

! !ARGUMENTS:

  character(len=*),             intent(in)                           :: weightFile
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  type(ESMF_RWGCheckMethod_Flag), intent(in),  optional  :: checkMethod
  integer,                                    intent(out), optional  :: rc

! !DESCRIPTION:
!
! The arguments are:
!   \begin{description}
!   \item [weightFile]
!     The interpolation weight file name.
!   \item [{[checkMethod]}]
!     Method to use when checking sparse matrix multiplication. There are two
!     options. {\tt ESMF\_CHECKMETHOD\_FIELD} (the default) uses field sparse
!     matrix multiplication. {\tt ESMF\_CHECKMETHOD\_ARRAY} uses array sparse
!     matrix multiplication. The underlying sparse matrix multiplication code
!     does not change between field and array. The difference is in how the
!     higher level objects call into the sparse matrix multiplication.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI
    !--------------------------------------------------------------------------
    ! DECLARATIONS
    !--------------------------------------------------------------------------
    integer :: localPet, nPet
    integer :: status

    type(ESMF_VM) :: vm

    character(ESMF_MAXPATHLEN) :: title

    real(ESMF_KIND_R8), allocatable :: factorList(:)
    integer, allocatable            :: factorIndexList(:,:)

    real(ESMF_KIND_R8), pointer :: src_lat(:), src_lon(:), &
                                   dst_lat(:), dst_lon(:), &
                                   src_area(:), dst_area(:), &
                                   src_mask(:), dst_mask(:), &
                                   src_frac(:), dst_frac(:)

    integer :: src_dim, dst_dim
    integer :: i, src, dst

    real(ESMF_KIND_R8), parameter :: two = 2.0
    real(ESMF_KIND_R8), parameter :: d2r = 3.141592653589793238/180
    real(ESMF_KIND_R8), parameter :: UNINITVAL = 422397696

    real(ESMF_KIND_R8), allocatable :: FsrcArray(:)
    real(ESMF_KIND_R8), allocatable :: FdstArray(:), FdstArrayX(:)
    real(ESMF_KIND_R8), pointer :: FdstArrayPtr(:), farrayPtr(:)

    type(ESMF_DistGrid) :: src_distgrid, dst_distgrid
    type(ESMF_ArraySpec):: src_arrayspec, dst_arrayspec
    type(ESMF_Array) :: srcArray, dstArray
    type(ESMF_RouteHandle) :: routehandle
    type(ESMF_Grid) :: srcGrid, dstGrid
    type(ESMF_Field) :: srcField, dstField

    real(ESMF_KIND_R8) :: maxRelError, meanRelError, lsRelError
    real(ESMF_KIND_R8) :: totErrDif, twoErrDif, twoErrX
    real(ESMF_KIND_R8) :: err, relError, maxneg, maxpos
    integer:: numRelError
    real(ESMF_KIND_R8) :: grid1min, grid1max, grid2min, grid2max
    real(ESMF_KIND_R8), pointer :: grid1area(:), grid2area(:)
    real(ESMF_KIND_R8), pointer :: grid1areaXX(:), grid2areaXX(:)
    type(ESMF_NormType_Flag) :: normType
    type(ESMF_RegridMethod_Flag) :: regridmethod
    type(ESMF_RWGCheckMethod_Flag) :: checkMethodLocal
    !--------------------------------------------------------------------------
    ! EXECUTION
    !--------------------------------------------------------------------------

    ! Set the default for the SMM check method.
    if (.not. present(checkMethod)) then
      checkMethodLocal = ESMF_RWGCHECKMETHOD_FIELD
    else
      checkMethodLocal = ESMF_RWGCHECKMETHOD_ARRAY
    endif

#ifdef ESMF_NETCDF
    ! set log to flush after every message
    call ESMF_LogSet(flush=.true., rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! get all vm information
    call ESMF_VMGetGlobal(vm, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! set up local pet info
    call ESMF_VMGet(vm, localPet=localPet, petCount=nPet, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    !Set finalrc to success
    rc = ESMF_SUCCESS

    ! read in the grid dimensions
    call NCFileInquire(weightFile, title, normType, src_dim, dst_dim, &
         regridmethod, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! only read the data on PET 0 until we get ArrayRead going...
    if (localPet == 0) then

      allocate(src_lat(src_dim))
      allocate(src_lon(src_dim))
      allocate(src_area(src_dim))
      allocate(src_mask(src_dim))
      allocate(src_frac(src_dim))
      allocate(dst_lat(dst_dim))
      allocate(dst_lon(dst_dim))
      allocate(dst_area(dst_dim))
      allocate(dst_mask(dst_dim))
      allocate(dst_frac(dst_dim))

      call GridReadCoords(weightFile, src_lat, src_lon, src_area, src_mask, src_frac, &
        dst_lat, dst_lon, dst_area, dst_mask, dst_frac, rc=status)
      if (ESMF_LogFoundError(status, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, &
        rcToReturn=rc)) return

      ! create Fortran arrays
      allocate(FsrcArray(src_dim))
      allocate(FdstArray(dst_dim))
      allocate(FdstArrayX(dst_dim))

      ! Initialize data
      FsrcArray = two + cos(src_lat)**2*cos(two*src_lon)
      FdstArrayX = two + cos(dst_lat)**2*cos(two*dst_lon)
      FdstArray = UNINITVAL

      ! deallocate arrays
      deallocate(src_lat)
      deallocate(src_lon)
      deallocate(dst_lat)
      deallocate(dst_lon)
    endif

    ! create DistGrids for the ESMF Arrays
    src_distgrid = ESMF_DistGridCreate(minIndex=(/1/), &
      maxIndex=(/src_dim/), rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return
    dst_distgrid = ESMF_DistGridCreate(minIndex=(/1/), &
      maxIndex=(/dst_dim/), rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! create dummy grids for fields
    srcGrid = ESMF_GridCreate(src_distgrid, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return
    dstGrid = ESMF_GridCreate(dst_distgrid, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! create ArraySpecs for the ESMF Arrays
    call ESMF_ArraySpecSet(src_arrayspec, typekind=ESMF_TYPEKIND_R8, rank=1, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return
    call ESMF_ArraySpecSet(dst_arrayspec, typekind=ESMF_TYPEKIND_R8, rank=1, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! create the ESMF Arrays
    srcArray = ESMF_ArrayCreate(arrayspec=src_arrayspec, distgrid=src_distgrid, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return
    dstArray = ESMF_ArrayCreate(arrayspec=dst_arrayspec, distgrid=dst_distgrid, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return
    ! initialize the array..
    call ESMF_ArrayGet(dstArray, farrayPtr=farrayPtr, rc=rc)
    farrayPtr = UNINITVAL

    ! Scatter the ESMF Arrays
    call ESMF_ArrayScatter(srcArray, farray=FsrcArray, rootPet=0, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! create fields on the empty grid and arrays
    srcField = ESMF_FieldCreate(srcGrid, srcArray, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! create fields on the empty grid and arrays
    dstField = ESMF_FieldCreate(dstGrid, dstArray, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

#if 0
    !-------------------- diagnostics -----------------------------------------
    print *, "size of factorList = (", size(factorList,1),")"
    print *, "size of factorIndexList = (", size(factorIndexList,1), ",", size(   factorIndexList,2),")"

    do i=1,size(factorList)
    src = factorIndexList(1,i)
    dst = factorIndexList(2,i)
    FdstArray(dst) = FdstArray(dst) + factorList(i)*FsrcArray(src)
    !print *, FdstArray(dst), "  ", FsrcArray(factorIndexList(1,i)), "  ",    factorList(i)
    !print *, factorList(i), FsrcArray(factorIndexList(1,i)), "  ", FdstArrayX(   factorIndexList(2,i))
    enddo
#endif

    if (checkMethodLocal .eq. ESMF_RWGCHECKMETHOD_FIELD) then
      ! Field and Grid way of doing things
      call ESMF_FieldSMMStore(srcField=srcField, dstField=dstField, &
        filename=weightFile, routehandle=routehandle, rc=status)
      if (ESMF_LogFoundError(status, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, &
        rcToReturn=rc)) return

      ! compute a Regrid from srcField to dstField
      call ESMF_FieldRegrid(srcField, dstField, routehandle, &
        zeroregion=ESMF_REGION_SELECT, rc=status)
      if (ESMF_LogFoundError(status, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, &
        rcToReturn=rc)) return
      call ESMF_FieldRegridRelease(routehandle, rc=status)
      if (ESMF_LogFoundError(status, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, &
        rcToReturn=rc)) return
    else if (checkMethodLocal .eq. ESMF_RWGCHECKMETHOD_ARRAY) then
      ! Array way of doing things
      call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
        filename=weightFile, routehandle=routehandle, rc=status)
      if (ESMF_LogFoundError(status, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, &
        rcToReturn=rc)) return

      ! *************************************************************
      ! compute a SMM from srcArray to dstArray
      call ESMF_ArraySMM(srcArray, dstArray, routehandle, &
        zeroregion=ESMF_REGION_SELECT, rc=status)
      if (ESMF_LogFoundError(status, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, &
        rcToReturn=rc)) return
      call ESMF_ArraySMMRelease(routehandle, rc=status)
      if (ESMF_LogFoundError(status, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, &
        rcToReturn=rc)) return
    else
      if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
          rcToReturn=rc)) return
    endif

    ! ArrayGather the dst array
    call ESMF_ArrayGather(dstArray, farray=FdstArray, rootPet=0, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    ! -----------------------------------------------------------------------
    ! ERROR ANALYSIS - serial
    ! -----------------------------------------------------------------------
    if (localPet == 0) then
      totErrDif = 0
      twoErrDif = 0
      twoErrX = 0
      maxRelError=0.0
      meanRelError=0.0
      lsRelError=0.0
      numRelError=0

      ! source error
      grid1min = UNINITVAL
      grid1max = 0
      do i=1,src_dim
        if (src_mask(i) /=0) then
          if(FsrcArray(i) < grid1min) grid1min = FsrcArray(i)
          if(FsrcArray(i) > grid1max) grid1max = FsrcArray(i)
        endif
      enddo

      ! destination error
      grid2min = UNINITVAL
      grid2max = 0
      do i=1,dst_dim
        ! don't look in masked cells
        ! if frac is below .999, then a significant portion of this cell is
        ! missing from the weight calculation and error is misleading here
        ! also don't look in unitialized cells, for the regional to global cases
        if (dst_mask(i) /= 0 .and. dst_frac(i) > .999 &
            .and. FdstArray(i) /= UNINITVAL) then

          ! compute the raw pointwise error
          err = abs(FdstArray(i) - FdstArrayX(i))

          ! Compute relative error
          if (FdstArrayX(i) .ne. 0.0) then
             relError=err/FdstArrayX(i)
          else
             relError=err
          endif

          ! Compute the max relative error
          if (relError > maxRelError) then
            maxRelError = relError
          endif

          ! relative pointwise error summation
          totErrDif = totErrDif + relError
          twoErrDif = twoErrDif + relError**2
          twoErrX = twoErrX + FdstArrayX(i)**2
          ! raw pointwise error summation
          !totErrDif = totErrDif + err
          !twoErrDif = twoErrDif + err**2
          !twoErrX = twoErrX + FdstArrayX(i)**2

          ! Number of points with frac > .999
          numRelError=numRelError+1

          ! masking will screw this one up
          if (FdstArray(i) < grid2min) grid2min = FdstArray(i)
          if (FdstArray(i) > grid2max) grid2max = FdstArray(i)
        endif
      enddo

      ! Read factors from weights file. Factors are allocated in the
      ! subroutine. Only the factorList is needed for error assessment.
      call ESMF_FactorRead(weightFile, factorList, factorIndexList, rc=status)
      if (ESMF_LogFoundError(status, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, &
        rcToReturn=rc)) return

      ! maximum negative weight
      maxneg = 0
      maxneg = minval(factorList)
      if (maxneg > 0) maxneg = 0

      ! maximum positive weight
      maxpos = 0
      maxpos = maxval(factorList)

      deallocate(factorList, factorIndexList)
      ! error measures
      meanRelError = totErrDif/REAL(numRelError,ESMF_KIND_R8)
      lsRelError = sqrt(twoErrDif)/sqrt(twoErrX)

     ! area calculations for conservative regridding only
     ! Calculate dst area depending on norm option
      if (regridmethod == ESMF_REGRIDMETHOD_CONSERVE) then
         ! use one of src_ or dst_frac, but NOT both!
         allocate(grid1area(src_dim))
         allocate(grid2area(dst_dim))
         allocate(grid1areaXX(src_dim))
         allocate(grid2areaXX(dst_dim))

         grid1area = FsrcArray*src_area*src_frac
         grid2area=0.0
         if (normType == ESMF_NORMTYPE_DSTAREA) then
            do i=1,dst_dim
              ! Only calculate dst area over region that is unmasked and initialized
              if ((dst_mask(i) /= 0) .and. (FdstArray(i) /=UNINITVAL)) then
                 grid2area(i) = FdstArray(i)*dst_area(i)
              endif
            enddo
         else if (normType == ESMF_NORMTYPE_FRACAREA) then
            do i=1,dst_dim
              ! Only calculate dst area over region that is unmasked and initialized
              if ((dst_mask(i) /= 0) .and. (FdstArray(i) /=UNINITVAL)) then
                 grid2area(i) = FdstArray(i)*dst_area(i)*dst_frac(i)
              endif
            enddo
         endif

         grid1areaXX = FsrcArray*src_area
         grid2areaXX = FdstArray*dst_area*dst_frac
      endif
      print *, trim(weightFile), " - ", trim(title)
      print *, " "
      print *, "Grid 1 min: ", grid1min, "    Grid 1 max: ", grid1max
      print *, "Grid 2 min: ", grid2min, "    Grid 2 max: ", grid2max
      print *, " "
      print *, "Maximum negative weight = ", maxneg
      print *, "Maximum positive weight = ", maxpos
      print *, " "
      print *, "Mean relative error     = ", meanRelError
      print *, "Maximum relative error  = ", maxRelError
      print *, "Least squares error     = ", lsRelError
      print *, " "
      if (regridmethod == ESMF_REGRIDMETHOD_CONSERVE) then
         print *, "Grid 1 area = ", sum(grid1area)
         print *, "Grid 2 area = ", sum(grid2area)
         print *, "Conservation error = ", abs(sum(grid2area)-sum(grid1area))
         deallocate(grid1area)
         deallocate(grid2area)
         deallocate(grid1areaXX)
         deallocate(grid2areaXX)
      else
         print *, "Grid 1 area = 0.000000000000000"
         print *, "Grid 2 area = 0.000000000000000"
         print *, "Conservation error = 0.000000000000000"
      endif
!      print *, " "
!      print *, "reverse fracs  - Grid 1 area = ", sum(grid1areaXX)
!      print *, "reverse fracs  - Grid 2 area = ", sum(grid2areaXX)
!      print *, "reverse - Conservation error = ", abs(sum(grid2areaXX)-sum(grid1areaXX))

      deallocate(src_area)
      deallocate(src_frac)
      deallocate(dst_area)
      deallocate(dst_frac)
      deallocate(src_mask)
      deallocate(dst_mask)
      deallocate(FsrcArray)
      deallocate(FdstArray)
      deallocate(FdstArrayX)
    endif

    ! destroy and deallocate
    call ESMF_ArrayDestroy(srcArray, rc=status)
    call ESMF_ArrayDestroy(dstArray, rc=status)
    call ESMF_GridDestroy(srcGrid, rc=status)
    call ESMF_GridDestroy(dstGrid, rc=status)
    call ESMF_FieldDestroy(srcField, rc=status)
    call ESMF_FieldDestroy(dstField, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

  end subroutine ESMF_RegridWeightGenCheck

  !****************************************************************************
  ! Read in the grid dimensions info from the weights file.
  ! The weights file should have the source and destination grid information
  ! provided.
  !****************************************************************************
  subroutine NCFileInquire (weightFile, title, normType, src_dim, dst_dim, regridmethod, rc)

    character(len=*), intent(in)   :: weightFile
    character(len=*), intent(out)  :: title
    integer, intent(out)           :: src_dim
    integer, intent(out)           :: dst_dim
    type(ESMF_NormType_Flag), intent(out) :: normType
    type(ESMF_RegridMethod_Flag), intent(out) :: regridmethod
    integer, intent(out), optional :: rc

    integer :: ncstat,  nc_file_id,  nc_srcdim_id, nc_dstdim_id
    integer :: titleLen, normLen, methodLen

    character(ESMF_MAXPATHLEN) :: msg, normStr, methodStr

#ifdef ESMF_NETCDF
    !-----------------------------------------------------------------
    ! open netcdf file
    !-----------------------------------------------------------------

    ncstat = nf90_open(weightFile, NF90_NOWRITE, nc_file_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_open error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    !-----------------------------------------------------------------
    ! Title
    !-----------------------------------------------------------------
    ncstat = nf90_inquire_attribute(nc_file_id, nf90_global, 'title', len=titleLen)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inquire_attribute error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    if(len(title) < titleLen) then
      print *, "Not enough space to put title."
      return
    end if
    ncstat = nf90_get_att(nc_file_id, nf90_global, "title", title)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_att error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    !-----------------------------------------------------------------
    ! Regridmethod
    !-----------------------------------------------------------------
    ncstat = nf90_inquire_attribute(nc_file_id, nf90_global, 'map_method', &
             len=methodLen)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inquire_attribute error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    if(len(methodStr) < methodLen) then
      print *, "Not enough space to put regridmethod string."
      return
    end if
    ncstat = nf90_get_att(nc_file_id, nf90_global, "map_method", methodStr)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_att error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    regridmethod = ESMF_REGRIDMETHOD_BILINEAR
    if (trim(methodStr) .eq. "Conservative remapping")  then
       regridmethod = ESMF_REGRIDMETHOD_CONSERVE
    endif

    !-----------------------------------------------------------------
    ! Normalization
    !-----------------------------------------------------------------
    ncstat = nf90_inquire_attribute(nc_file_id, nf90_global, 'normalization', &
             len=normLen)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inquire_attribute error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    if(len(normStr) < normLen) then
      print *, "Not enough space to put normalization string."
      return
    end if
    ncstat = nf90_get_att(nc_file_id, nf90_global, "normalization", normStr)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_att error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    ! translate normalization into normtype
    normType=ESMF_NORMTYPE_DSTAREA
    if (trim(normStr) .eq. "fracarea") then
       normType=ESMF_NORMTYPE_FRACAREA
    endif

    !-----------------------------------------------------------------
    ! source grid dimensions
    !-----------------------------------------------------------------

    ncstat = nf90_inq_dimid(nc_file_id, 'n_a', nc_srcdim_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_dimid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_inquire_dimension(nc_file_id, nc_srcdim_id, len=src_dim)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inquire_variable error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    !-----------------------------------------------------------------
    ! destination grid dimensions
    !-----------------------------------------------------------------

    ncstat = nf90_inq_dimid(nc_file_id, 'n_b', nc_dstdim_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_dimid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_inquire_dimension(nc_file_id, nc_dstdim_id, len=dst_dim)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inquire_variable error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif




    !------------------------------------------------------------------------
    !     close input file
    !------------------------------------------------------------------------

    ncstat = nf90_close(nc_file_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_close error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    if(present(rc)) rc = ESMF_SUCCESS

#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

  end subroutine NCFileInquire

  !****************************************************************************
  ! Read in the grid info from the weights file.
  ! The weights file should have the source and destination grid information
  ! provided.
  !****************************************************************************
  subroutine GridReadCoords (weightFile, src_lat, src_lon, src_area, &
                             src_mask, src_frac, &
                             dst_lat, dst_lon, dst_area, dst_mask, &
                             dst_frac, rc)

    character(len=*), intent(in)   :: weightFile
    real(ESMF_KIND_R8), pointer    :: src_lat(:)
    real(ESMF_KIND_R8), pointer    :: src_lon(:)
    real(ESMF_KIND_R8), pointer    :: src_area(:)
    real(ESMF_KIND_R8), pointer    :: src_mask(:)
    real(ESMF_KIND_R8), pointer    :: src_frac(:)
    real(ESMF_KIND_R8), pointer    :: dst_lat(:)
    real(ESMF_KIND_R8), pointer    :: dst_lon(:)
    real(ESMF_KIND_R8), pointer    :: dst_area(:)
    real(ESMF_KIND_R8), pointer    :: dst_mask(:)
    real(ESMF_KIND_R8), pointer    :: dst_frac(:)
    integer, intent(out), optional :: rc

    integer :: ncstat,  nc_file_id
    integer :: nc_srcgridlat_id, nc_srcgridlon_id, &
               nc_dstgridlat_id, nc_dstgridlon_id, &
               nc_srcarea_id, nc_dstarea_id, &
               nc_srcmask_id, nc_dstmask_id, &
               nc_srcfrac_id, nc_dstfrac_id
    integer :: unitsLen

    character(ESMF_MAXPATHLEN) :: units, buffer
    character(ESMF_MAXPATHLEN) :: msg

    real(ESMF_KIND_R8), parameter :: d2r = 3.141592653589793238/180

#ifdef ESMF_NETCDF

    !-----------------------------------------------------------------
    ! open netcdf file
    !-----------------------------------------------------------------

    ncstat = nf90_open(weightFile, NF90_NOWRITE, nc_file_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_open error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    !-----------------------------------------------------------------
    ! get the grid coordinates
    !-----------------------------------------------------------------

    ncstat = nf90_inq_varid(nc_file_id, 'yc_a', nc_srcgridlat_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_varid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_get_var(ncid=nc_file_id, varid=nc_srcgridlat_id, &
      values=src_lat)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_var error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    ! get the units of the grid coordinates
    ncstat = nf90_inquire_attribute(nc_file_id, nc_srcgridlat_id, 'units', &
      len=unitsLen)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inquire_attribute error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    if(len(units) < unitsLen) then
      print *, "Not enough space to get units."
      return
    endif
    ncstat = nf90_get_att(nc_file_id, nc_srcgridlat_id, "units", buffer)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_att error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    units = buffer(1:unitsLen)
    ! convert to radians if coordinates are in degrees
    if (trim(units)==trim("degrees")) then
      src_lat = src_lat*d2r
    else if (trim(units)/=trim("radians")) then
      write (msg, '(a,i4)') "- units are not 'degrees' or 'radians'"
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    ncstat = nf90_inq_varid(nc_file_id, 'xc_a', nc_srcgridlon_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_varid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_get_var(ncid=nc_file_id, varid=nc_srcgridlon_id, &
      values=src_lon)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_var error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    ! get the units of the grid coordinates
    ncstat = nf90_inquire_attribute(nc_file_id, nc_srcgridlon_id, 'units', &
      len=unitsLen)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inquire_attribute error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    if(len(units) < unitsLen) then
      print *, "Not enough space to get units."
      return
    endif
    ncstat = nf90_get_att(nc_file_id, nc_srcgridlon_id, "units", buffer)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_att error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    units = buffer(1:unitsLen)
    ! convert to radians if coordinates are in degrees
    if (trim(units)==trim("degrees")) then
      src_lon = src_lon*d2r
    else if (trim(units)/=trim("radians")) then
      write (msg, '(a,i4)') "- units are not 'degrees' or 'radians'"
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    !-----------------------------------------------------------------
    ! get the grid coordinates
    !-----------------------------------------------------------------
    ncstat = nf90_inq_varid(nc_file_id, 'yc_b', nc_dstgridlat_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_varid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_get_var(ncid=nc_file_id, varid=nc_dstgridlat_id, &
      values=dst_lat)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_var error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    ! get the units of the grid coordinates
    ncstat = nf90_inquire_attribute(nc_file_id, nc_dstgridlat_id, 'units', &
      len=unitsLen)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inquire_attribute error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    if(len(units) < unitsLen) then
      print *, "Not enough space to get units."
      return
    endif
    ncstat = nf90_get_att(nc_file_id, nc_dstgridlat_id, "units", buffer)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_att error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    units = buffer(1:unitsLen)
    ! convert to radians if coordinates are in degrees
    if (trim(units)==trim("degrees")) then
      dst_lat = dst_lat*d2r
    else if (trim(units)/=trim("radians")) then
      write (msg, '(a,i4)') "- units are not 'degrees' or 'radians'"
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    ncstat = nf90_inq_varid(nc_file_id, 'xc_b', nc_dstgridlon_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_varid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_get_var(ncid=nc_file_id, varid=nc_dstgridlon_id, &
      values=dst_lon)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_var error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    ! get the units of the grid coordinates
    ncstat = nf90_inquire_attribute(nc_file_id, nc_dstgridlon_id, 'units', &
      len=unitsLen)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inquire_attribute error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    if(len(units) < unitsLen) then
      print *, "Not enough space to get units."
      return
    endif
    ncstat = nf90_get_att(nc_file_id, nc_dstgridlon_id, "units", buffer)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_att error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    units = buffer(1:unitsLen)
    ! convert to radians if coordinates are in degrees
    if (trim(units)==trim("degrees")) then
      dst_lon = dst_lon*d2r
    else if (trim(units)/=trim("radians")) then
      write (msg, '(a,i4)') "- units are not 'degrees' or 'radians'"
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    !-----------------------------------------------------------------
    ! get the grid areas
    !-----------------------------------------------------------------
    ncstat = nf90_inq_varid(nc_file_id, 'area_a', nc_srcarea_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_varid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_get_var(ncid=nc_file_id, varid=nc_srcarea_id, &
      values=src_area)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_var error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    ncstat = nf90_inq_varid(nc_file_id, 'area_b', nc_dstarea_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_varid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_get_var(ncid=nc_file_id, varid=nc_dstarea_id, &
      values=dst_area)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_var error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    !-----------------------------------------------------------------
    ! get the grid masks
    !-----------------------------------------------------------------
    ncstat = nf90_inq_varid(nc_file_id, 'mask_a', nc_srcmask_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_varid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_get_var(ncid=nc_file_id, varid=nc_srcmask_id, &
      values=src_mask)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_var error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    ncstat = nf90_inq_varid(nc_file_id, 'mask_b', nc_dstmask_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_varid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_get_var(ncid=nc_file_id, varid=nc_dstmask_id, &
      values=dst_mask)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_var error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    !-----------------------------------------------------------------
    ! get the grid fracs
    !-----------------------------------------------------------------
    ncstat = nf90_inq_varid(nc_file_id, 'frac_a', nc_srcfrac_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_varid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_get_var(ncid=nc_file_id, varid=nc_srcfrac_id, &
      values=src_frac)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_var error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    ncstat = nf90_inq_varid(nc_file_id, 'frac_b', nc_dstfrac_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_inq_varid error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif
    ncstat = nf90_get_var(ncid=nc_file_id, varid=nc_dstfrac_id, &
      values=dst_frac)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_get_var error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    !------------------------------------------------------------------------
    !     close input file
    !------------------------------------------------------------------------

    ncstat = nf90_close(nc_file_id)
    if(ncstat /= 0) then
      write (msg, '(a,i4)') "- nf90_close error:", ncstat
      call ESMF_LogSetError(ESMF_RC_SYS, msg=msg, &
        line=__LINE__, file=__FILE__ , rcToReturn=rc)
      return
    endif

    if(present(rc)) rc = ESMF_SUCCESS

#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

  end subroutine GridReadCoords

end module ESMF_RegridWeightGenCheckMod
