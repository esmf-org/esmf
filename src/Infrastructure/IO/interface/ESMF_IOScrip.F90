! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_IOScrip.F90"
!
!     ESMF IOScrip Module
      module ESMF_IOScripMod
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_IOScripMod - Grid I/O utility class
!
! !DESCRIPTION:
!
! The code in this file reads the SCRIP Grid files and write out Scrip Weight
! files
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_UtilMod
      use ESMF_InitMacrosMod    ! ESMF initializer macros
      use ESMF_LogErrMod        ! ESMF error handling
      use ESMF_VMMod
      use ESMF_IOUGridMod
      use ESMF_IOGridspecMod
      use ESMF_DistGridMod
      use ESMF_ArrayMod
      use ESMF_IOGridmosaicMod
      use ESMF_AttPackTypeMod
#ifdef ESMF_NETCDF
      use netcdf
#endif

!     NEED TO ADD MORE HERE
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE:
      private
!------------------------------------------------------------------------------

!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
  public ESMF_ScripInq
  public ESMF_ScripInqUnits
  public ESMF_ScripGetVar
  public ESMF_OutputWeightFile
  public ESMF_OutputScripWeightFile
  public ESMF_OutputSimpleWeightFile
  public ESMF_EsmfGetNode
  public ESMF_EsmfGetElement
  public ESMF_OutputScripVarFile
  public ESMF_EsmfInq
  public ESMF_EsmfInqUnits
  public ESMF_EsmfGetCoords

  public ESMF_SparseMatrixWrite     !TODO: move this into SparseMatrix class
                                    !TODO: once implemented

!==============================================================================

      contains

!==============================================================================

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SparseMatrixWrite"
!BOP
! !IROUTINE: ESMF_SparseMatrixWrite - Write a sparse matrix to file
! \label{api:SparseMatrixWrite}
!
! !INTERFACE:
  subroutine ESMF_SparseMatrixWrite(factorList, factorIndexList, fileName, &
    keywordEnforcer, rc)
!
! !ARGUMENTS:
    real(ESMF_KIND_R8),    intent(in)            :: factorList(:)
    integer(ESMF_KIND_I4), intent(in)            :: factorIndexList(:,:)
    character(*),          intent(in)            :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,               intent(out), optional :: rc
!
! !DESCRIPTION:
!   Write the {\tt factorList} and {\tt factorIndexList} into a NetCDF file.
!   The data is stored in SCRIP format documented under section 
!   ~(\ref{sec:weightfileformat}).
!
!   Limitations:
!   \begin{itemize}
!     \item Only {\tt real(ESMF\_KIND\_R8) factorList} and 
!           {\tt integer(ESMF\_KIND\_I4) factorIndexList} supported.
!     \item Not supported in {\tt ESMF\_COMM=mpiuni} mode.
!   \end{itemize}
!
!  The arguments are:
!  \begin{description}
!   \item[factorList]
!    The sparse matrix factors to be written.
!   \item[factorIndexList]
!    The sparse matrix sequence indices to be written.
!   \item[fileName]
!    The name of the output file to be written.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Local vars
    integer                    :: localrc           ! local return code

    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into lower level implementation
    call ESMF_OutputWeightFile(weightFile=fileName, factorList=factorList, &
      factorIndexList=factorIndexList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,  &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_SparseMatrixWrite
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_OutputWeightFile"
!BOPI
! !ROUTINE: ESMF_OutputWeightFile
! output the weight and indices tables only
!

! !INTERFACE:
subroutine ESMF_OutputWeightFile (weightFile, factorList, factorIndexList, rc)
!
! !ARGUMENTS:
    character(len=*), intent(in) :: weightFile
    real(ESMF_KIND_R8), intent(in) :: factorList(:)
    integer(ESMF_KIND_I4), intent(in) :: factorIndexList(:,:)
    integer, intent(inout), optional :: rc

    type(ESMF_DistGrid) :: distgridFL
    type(ESMF_Array) :: arrayFL, arrayFIL1, arrayFIL2

    type(ESMF_AttPack) :: attpack
    integer :: lens(3), lens2(1), nfactors, ii, localPet, petCount, startIndex, &
               stopIndex, localrc, memstat, hasFactors, nLivePETs(1), offset
    character(len=22), parameter :: specString = "distgridnetcdfmetadata"
    character(len=23), parameter :: name = "ESMF:gridded_dim_labels"
    character(len=3), parameter :: value = "n_s"
    character(len=70), parameter :: noFactorsMsg = '"factorList" has size 0 and PET count is 1. There is nothing to write.'
    integer(ESMF_KIND_I4), allocatable, dimension(:) :: col, row
    type(ESMF_VM) :: vm
    integer(ESMF_KIND_I4), dimension(1) :: sendData, recvData
    integer(ESMF_KIND_I4), dimension(2) :: bcstData
    integer(ESMF_KIND_I4), allocatable, dimension(:,:,:) :: deBlockList
    
    ! ==============================================================================

    if (present(rc)) then
      localrc = rc
    else
      localrc = ESMF_RC_NOT_IMPL
    endif
    
#if (!defined ESMF_PIO || (!defined ESMF_NETCDF && !defined ESMF_PNETCDF))
    ! Writing weights requires netCDF and the subroutine should not continue if
    ! the netCDF library is not available.
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
#endif
    
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! ==============================================================================
    ! Create the DistGrid. The factors may be ragged (factor count differs between
    ! PETs). Synchronize min and max indices across PETs.
    
    ! Number of local factors.
    nfactors = size(factorList, 1)
    
    ! Bail out if there are no factors and this is a single process.
    if ((nfactors .eq. 0) .and. (petCount .eq. 1)) then
      if (ESMF_LogFoundError(ESMF_RC_NOT_IMPL, msg=noFactorsMsg, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! Determine if we need a redistribution. A redistribution is needed if one of
    ! the PETs does not have any factors.
    if (nfactors .eq. 0) then
      hasFactors = 0
    else
      hasFactors = 1
    endif
    call ESMF_VMAllReduce(vm, (/hasFactors/), nLivePETs, 1, ESMF_REDUCE_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
        rcToReturn=rc)) return
    
    ! Chain start and stop index calculation.
    if (localPet .ne. 0) then
      call ESMF_VMRecv(vm, recvData, 1, localPet-1, rc=localrc)
      startIndex = recvData(1)
    else
      startIndex = 1
    endif
    if (nfactors .eq. 0) then
      stopIndex = startIndex
    else
      stopIndex = startIndex + nfactors - 1
    endif
    if ((localPet .ne. petCount-1) .and. (petCount > 1)) then
      if (nfactors == 0) then
        offset = 0
      else
        offset = 1
      endif
      call ESMF_VMSend(vm, (/stopIndex+offset/), 1, localPet+1, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    ! Some PETs do not have data. We need to gather and scatter to ensure the
    ! asynchronous write has data for each proc - or - we can use the simple weight
    ! file write implementation that can handle zero-length factor lists.
    
    ! TODO (bekozi): Array should be able to handle empty data and the write should
    !  correspondingly work.

    if (nLivePETs(1) .ne. petCount) then
      ! This streams everything to a single PET for writing avoiding the need for an
      ! asynchronous write.
      call ESMF_OutputSimpleWeightFile(weightFile, factorList, factorIndexList, &
                                       rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, &
        rcToReturn=rc)) return ! bail on error

      ! return successfully
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    
    ! Ragged factor counts may require a non-regular decomposition. This requires a 
    ! custom block definition per DE.
    allocate(deBlockList(1, 2, petCount), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat,  &
        ESMF_CONTEXT, rcToReturn=rc)) return

    do ii=1,petCount
      if (localPet .eq. ii-1) then
        bcstData = (/startIndex, stopIndex/)
      else
        bcstData = (/0, 0/)
      endif
      call ESMF_VMBroadcast(vm, bcstData, 2, ii-1, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      deBlockList(1, :, ii) = bcstData
    enddo
    
    distgridFL = ESMF_DistGridCreate(minIndex=(/1/), &
                                     maxIndex=(/deBlockList(1, 2, petCount)/), &
                                     deBlockList=deBlockList, &
                                     rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    ! ==============================================================================
    ! Set up attributes to allow variables to share a common dimension name in the
    ! output file.

    !NOTE: removed distgridcreate from factorIndexList so that all
    !      Arrays could share the same DistGrid (i.e. dimensions)

    ! distgrid metadata
    lens(1) = 8
    lens(2) = 6
    lens(3) = 8
    
    lens2(1) = 3
    
    ! set up the metadata on distgrid
    call c_ESMC_AttPackCreateCustom(distgridFL, size(lens), specString, &
                                    lens, attpack, localrc)
    !call ESMF_AttributeAdd(grid, convention="netcdf", purpose="metadata",  &
    !  attrList=(/ ESMF_ATT_GRIDDED_DIM_LABELS /), rc=rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !call c_ESMC_AttPackAddAtt(grid, name, size(lens), specString, &
    !                          lens, rc)
    call c_ESMC_AttPackAddAtt(name, attpack, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
    call c_ESMC_AttPackSetCharList(distgridFL, name, ESMF_TYPEKIND_CHARACTER, &
                                  1, value, lens2, attpack, 0, localrc)
    !call ESMF_AttributeSet(grid, name=ESMF_ATT_GRIDDED_DIM_LABELS, &
    !                       convention="netcdf", purpose="metadata",  &
    !                       valueList=(/ "n_s"/), rc=rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! ==============================================================================
    ! Create arrays.

    arrayFL = ESMF_ArrayCreate(farray=factorList, distgrid=distgridFL, &
                               indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Copy factor indexing before passing to array create. Passing an array section 
    ! here causes undefined behavior in the array buffer access. "datacopyflag" does 
    ! not work with this interface??
    allocate(col(nfactors), row(nfactors), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat,  &
        ESMF_CONTEXT, rcToReturn=rc)) return

    do ii=1,nfactors
      col(ii) = factorIndexList(1, ii)
      row(ii) = factorIndexList(2, ii)
    enddo
    
    arrayFIL1 = ESMF_ArrayCreate(farray=col, distgrid=distgridFL, &
                                 indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    arrayFIL2 = ESMF_ArrayCreate(farray=row, distgrid=distgridFL, &
                                 indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! ==============================================================================
    ! Write arrays to file.

    ! Do not overwrite the output file by default.
    call ESMF_ArrayWrite(arrayFL, weightFile, variableName="S", &
                         convention="netcdf", purpose="metadata", &
                         overwrite=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set overwrite to true for consecutive writes. The file is created on the first
    ! write.
    call ESMF_ArrayWrite(arrayFIL1, weightFile, variableName="col", &
                         convention="netcdf", purpose="metadata", &
                         overwrite=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ArrayWrite(arrayFIL2, weightFile, variableName="row", &
                         convention="netcdf", purpose="metadata", &
                         overwrite=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! ==============================================================================

    deallocate(col, row, deBlockList, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat,  &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ArrayDestroy(arrayFL, rc=localrc)
    call ESMF_ArrayDestroy(arrayFIL1, rc=localrc)
    call ESMF_ArrayDestroy(arrayFIL2, rc=localrc)
    call ESMF_DistGridDestroy(distgridFL, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    rc = localrc

end subroutine ESMF_OutputWeightFile

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ScripInq"
 !BOPI
! !ROUTINE: ESMF_ScripInq: Return the dimension, grid_rank, and other related
!  information from a SCRIP file
!
! !INTERFACE:
  subroutine ESMF_ScripInq(filename, grid_size, grid_corners, &
                          grid_dims, grid_rank, has_area, rc)

! !ARGUMENTS:

    character(len=*), intent(in)  :: filename
    integer, intent(out), optional :: grid_size
    integer, intent(out), optional :: grid_corners
    integer, intent(out), optional :: grid_dims(:)
    integer, intent(out), optional :: grid_rank
    logical, intent(out), optional :: has_area
    integer, intent(out), optional :: rc

    integer:: localrc, ncStatus
    integer :: DimId, VarId
    integer :: ncid, local_rank
    character(len=256) :: errmsg
    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, &
      trim(filename), &
      rc)) return

    ! get number of vertices
    errmsg = "dimension grid_size in "//trim(filename)
    if (present(grid_size)) then
      ncStatus = nf90_inq_dimid (ncid, "grid_size", DimId)
      if (CDFCheckError (ncStatus, &
         ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=grid_size)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
    end if

    ! Get vertex dimension
    if (present(grid_corners)) then
      ncStatus = nf90_inq_dimid (ncid, "grid_corners", DimId)
      errmsg = "dimension grid_corners in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=grid_corners)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg, &
        rc)) return
    end if

    ! Get vertex dimension
    if (present(grid_rank)) then
      ncStatus = nf90_inq_dimid (ncid, "grid_rank", DimId)
      errmsg = "dimension grid_rank in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
         ESMF_SRCLINE,&
        errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=local_rank)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      grid_rank = local_rank
    end if

    ! get grid dimension
    ! If we didn't get the rank earlier, get it now
    if (present(grid_dims)) then
      if (.not. present(grid_rank)) then
        ncStatus = nf90_inq_dimid (ncid, "grid_rank", DimId)
        errmsg = "dimension grid_rank in "//trim(filename)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg,&
          rc)) return

        ncStatus = nf90_inquire_dimension (ncid, DimId, len=local_rank)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg,&
        rc)) return
      end if
      ncStatus = nf90_inq_varid (ncid, "grid_dims", VarId)
      errmsg = "dimension grid_dims in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_dims(1:local_rank))
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
    end if
    if (present(has_area)) then
      ncStatus = nf90_inq_varid (ncid, "grid_area", VarId)
      if (ncStatus /= nf90_noerror) then
        has_area = .false.
      else
        has_area = .true.
      endif
    endif
    ncStatus = nf90_close(ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, &
      trim(filename), &
      rc)) return
    if (present(rc)) rc=ESMF_SUCCESS
    return
#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end subroutine ESMF_ScripInq

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ScripInqUnits"
!BOPI
! !ROUTINE: ESMF_ScripInqUnits: Return the units attribute for
! coordinate variables, assuming all the coordinate variables
! have the same units
!
! !INTERFACE:
subroutine ESMF_ScripInqUnits(filename, units, rc)

! !ARGUMENTS:

    character(len=*), intent(in)   :: filename
    character(len=*), intent(out)   :: units
    integer, intent(out), optional :: rc

    integer:: localrc, ncStatus
    integer :: ncid, VarId, len
    character(len=80) :: buffer, buffer1
    character(len=256) :: errmsg

#ifdef ESMF_NETCDF
    if (present(rc)) rc=ESMF_SUCCESS
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE, &
      trim(filename), &
      rc)) return

    ncStatus = nf90_inq_varid (ncid, "grid_center_lat", VarId)
    errmsg = "variable grid_center_lat in "//trim(filename)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return

    ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
    errmsg = "attribute units for grid_center_lat in "//trim(filename)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
    ncStatus = nf90_get_att(ncid, VarId, "units", buffer)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return

    if (buffer(len:len) .eq. achar(0)) len = len-1
    units = ESMF_UtilStringLowerCase(buffer(1:len), rc=rc)
    if (present(rc)) rc=ESMF_SUCCESS
    return
#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

    return
end subroutine ESMF_ScripInqUnits

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ScripGetVar"
!BOPI
! !ROUTINE: ESMF_ScripGetVar
!

! !INTERFACE:
  subroutine ESMF_ScripGetVar(filename, grid_center_lon, grid_center_lat, &
                        grid_imask, grid_corner_lon, grid_corner_lat, &
                        grid_area, convertToDeg, start, count, rc)
!
! !ARGUMENTS:
    character(len=*), intent(in)  :: filename
    real(ESMF_KIND_R8), intent(inout),optional:: grid_center_lon(:)
    real(ESMF_KIND_R8), intent(inout),optional:: grid_center_lat(:)
    integer, intent(inout), optional:: grid_imask(:)
    real(ESMF_KIND_R8), intent(inout),optional:: grid_corner_lon(:,:)
    real(ESMF_KIND_R8), intent(inout),optional:: grid_corner_lat(:,:)
    real(ESMF_KIND_R8), intent(inout),optional:: grid_area(:)
    logical, intent(in), optional:: convertToDeg
    integer, intent(in), optional:: start, count
    integer, intent(out), optional:: rc

    integer:: ncStatus
    integer:: ncid, VarId
    integer:: len
    character(len=128) :: units
    character(len=256) :: errmsg
    integer:: start1(1), count1(1), start2(2), count2(2)
    integer:: totalcells, grid_corners
    logical :: convertToDegLocal
    integer:: localrc
    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    convertToDegLocal = .false.
    if (present(convertToDeg)) convertToDegLocal = convertToDeg

    call ESMF_ScripInq(trim(filename), grid_size=totalcells, &
           grid_corners=grid_corners, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      trim(filename), &
      rc)) return

    if (present(start)) then
        start1(1)=start
        start2(2)=start
        start2(1)=1
    else
        start1(1)=1
        start2(:)=1
    endif
    if (present(count)) then
        count1(1)=count
        count2(2)=count
        count2(1)=grid_corners
    else
        count1(1)=totalcells
        count2(2)=totalcells
        count2(1)=grid_corners
    endif

    ! Read in grid_center_lon and grid_center_lat
    if (present(grid_center_lon)) then
      ncStatus = nf90_inq_varid (ncid, "grid_center_lon", VarId)
      errmsg = "variable grid_center_lon in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_center_lon, start1, count1)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return

      ! get the attribute 'units'
      ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
      errmsg = "Attribute units for grid_center_lon in "//trim(filename)
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg,&
          rc)) return
      ncStatus = nf90_get_att(ncid, VarId, "units", units)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ! if units is not "degrees" or "radians" return errors
      if (units(len:len) .eq. achar(0)) len = len-1
      units = ESMF_UtilStringLowerCase(units(1:len))
      if (units(1:len) .ne. 'degrees' .and. units(1:len) .ne. 'radians') then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- units attribute is not degrees or radians", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
      endif
      ! if units is "radians", convert it to degrees
      if (convertToDegLocal) then
         if (units(1:len) .eq. "radians") then
            grid_center_lon(:) = &
                   grid_center_lon(:)*ESMF_COORDSYS_RAD2DEG
         endif
      endif     
    endif
    if (present(grid_center_lat)) then
      ncStatus = nf90_inq_varid (ncid, "grid_center_lat", VarId)
      errmsg = "variable grid_center_lat in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_center_lat, start1, count1)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ! get the attribute 'units'
      ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
      errmsg = "Attribute units for grid_center_lat in "//trim(filename)
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg,&
          rc)) return
      ncStatus = nf90_get_att(ncid, VarId, "units", units)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ! if units is not "degrees" or "radians" return errors
     if (units(len:len) .eq. achar(0)) len = len-1
      units = ESMF_UtilStringLowerCase(units(1:len))
      if (units(1:len) .ne. 'degrees' .and. units(1:len) .ne. 'radians') then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- units attribute is not degrees or radians", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
      endif
      ! if units is "radians", convert it to degree
      if (convertToDegLocal) then
         if (units(1:len) .eq. "radians") then
            grid_center_lat(:) = &
                 grid_center_lat(:)*ESMF_COORDSYS_RAD2DEG
         endif
      endif
    endif

    if (present(grid_imask)) then
      ncStatus = nf90_inq_varid (ncid, "grid_imask", VarId)
      errmsg = "variable grid_imask is not defined in "//trim(filename)
      if (ncStatus /= nf90_noerror) then
         print *, 'Warning:', errmsg
         grid_imask = 1
      else
         ncStatus = nf90_get_var (ncid, VarId, grid_imask, start1, count1)
         if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return
      endif
    endif

    ! Read in grid_corner_lon and grid_corner_lat
    if (present(grid_corner_lon)) then
      ncStatus = nf90_inq_varid (ncid, "grid_corner_lon", VarId)
      errmsg = "variable grid_corner_lon in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_corner_lon, start2, count2)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ! get the attribute 'units'
      ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
      errmsg = "attribute units for grid_center_lon in "//trim(filename)
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg,&
          rc)) return
      ncStatus = nf90_get_att(ncid, VarId, "units", units)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ! if units is not "degrees" or "radians" return errors
     if (units(len:len) .eq. achar(0)) len = len-1
      units = ESMF_UtilStringLowerCase(units(1:len))
      if (units(1:len) .ne. 'degrees' .and. units(1:len) .ne. 'radians') then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- units attribute is not degrees or radians", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
      endif
      ! if units is "radians", convert it to degree
      if (convertToDegLocal) then
         if (units(1:len) .eq. "radians") then
            grid_corner_lon(:,:) = &
               grid_corner_lon(:,:)*ESMF_COORDSYS_RAD2DEG
         endif
      endif     
    endif
    if (present(grid_corner_lat)) then
      ncStatus = nf90_inq_varid (ncid, "grid_corner_lat", VarId)
      errmsg = "variable grid_corner_lat in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_corner_lat, start2, count2)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ! get the attribute 'units'
      ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
      errmsg = "Attribute units for grid_center_lon in "//trim(filename)
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg,&
          rc)) return
      ncStatus = nf90_get_att(ncid, VarId, "units", units)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ! if units is not "degrees" or "radians" return errors
     if (units(len:len) .eq. achar(0)) len = len-1
      units = ESMF_UtilStringLowerCase(units(1:len))
      if (units(1:len) .ne. 'degrees' .and. units(1:len) .ne. 'radians') then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- units attribute is not degrees or radians", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
      endif
      ! if units is "radians", convert it to degree
      if (convertToDegLocal) then
         if (units(1:len) .eq. "radians") then
            grid_corner_lat(:,:) = &
                grid_corner_lat(:,:)*ESMF_COORDSYS_RAD2DEG
         endif
      endif
    endif

    ! Read in grid_area and grid_corner_lat
    if (present(grid_area)) then
      ncStatus = nf90_inq_varid (ncid, "grid_area", VarId)
      errmsg = "variable grid_area in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, grid_area, start1, count1)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,&
        errmsg,&
        rc)) return
    endif

    ncStatus = nf90_close(ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE,&
      trim(filename),&
      rc)) return

    if(present(rc)) rc = ESMF_SUCCESS
    return
#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end subroutine ESMF_ScripGetVar

#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_OutputScripWeightFile"
!BOPI
! !ROUTINE: ESMF_OUtputScripWeightFile
! output the weight and indices tables together with the source/dest vertex
! coordinates/masks to the SCRIP format NetCDF file
!

! !INTERFACE:
subroutine ESMF_OutputScripWeightFile (wgtFile, factorList, factorIndexList, &
                                      srcFile, dstFile, srcFileType, dstFileType,&
                                      title, method, normType, srcArea, dstArea, &
                                      srcFrac, dstFrac, largeFileFlag, netcdf4FileFlag, &
                                      srcmeshname, dstmeshname, &
                                      srcMissingValue, dstMissingValue, &
                                      srcvarname, dstvarname, &
                                      useSrcCorner, useDstCorner, &
                                      srccoordnames, dstcoordnames, tileFilePath, rc)
!
! !ARGUMENTS:
      character(len=*), intent(in) :: wgtFile
      real(ESMF_KIND_R8) , intent(in) :: factorList(:)
      integer(ESMF_KIND_I4) , intent(in), target :: factorIndexList(:,:)
      character(len=*), optional, intent(in) :: srcFile
      character(len=*), optional, intent(in) :: dstFile
      type(ESMF_FileFormat_Flag), optional, intent(in) :: srcFileType
      type(ESMF_FileFormat_Flag), optional, intent(in) :: dstFileType
      character(len=*), optional, intent(in) :: title
      type(ESMF_RegridMethod_Flag), optional, intent(in) :: method
      type(ESMF_NormType_Flag),    intent(in),   optional :: normType
      real(ESMF_KIND_R8),optional, intent(in) :: srcArea(:),dstArea(:)
      real(ESMF_KIND_R8),optional, intent(in) :: srcFrac(:), dstFrac(:)
      logical, optional, intent(in) :: largeFileFlag, netcdf4FileFlag
      character(len=*), optional, intent(in) :: srcmeshname, dstmeshname
      logical, optional, intent(in) :: srcMissingValue, dstMissingValue
      character(len=*), optional, intent(in) :: srcvarname, dstvarname
      character(len=*), optional, intent(in) :: srccoordnames(:), dstcoordnames(:)
      logical, optional, intent(in) :: useSrcCorner, useDstCorner
      character(len=*), optional, intent(in) :: tileFilePath
      integer, optional :: rc

      type(ESMF_VM):: vm
      integer :: PetNo, PetCnt

      integer :: total, localCount(1)
      integer :: ncid, ncid1
      integer :: ncStatus
      integer :: status
      integer :: i,j, k, start
      integer :: srcDim, dstDim
      integer:: naDimId, nbDimId, nsDimId, srankDimId, drankDimId, varId, varId1, varId2
      integer :: nvaDimId, nvbDimId
      integer :: src_grid_rank, dst_grid_rank, src_grid_corner, dst_grid_corner
      integer, pointer :: src_grid_dims(:), dst_grid_dims(:)
      integer :: src_ndims, dst_ndims
      integer :: src_coordids(2), dst_coordids(2)
      integer :: src_dimids(2), dst_dimids(2)
      real(ESMF_KIND_R8), pointer   :: coords(:),area(:),frac(:)
      real(ESMF_KIND_R8), pointer   :: latBuffer(:),lonBuffer(:)
      real(ESMF_KIND_R8), pointer   :: latBuffer1D(:),lonBuffer1D(:)
      real(ESMF_KIND_R8), pointer   :: latBuffer2(:,:),lonBuffer2(:,:)
      real(ESMF_KIND_R8), pointer   :: cornerlon2D(:,:), cornerlat2D(:,:)
      real(ESMF_KIND_R8), pointer   :: cornerlon3D(:,:,:), cornerlat3D(:,:,:)
      real(ESMF_KIND_R8), pointer   :: weightbuf(:), varBuffer(:,:)
      real(ESMF_KIND_R8), pointer   :: varBuffer1D(:)
      real(ESMF_KIND_R8) :: missing_value
      integer(ESMF_KIND_I4), pointer:: indexbuf(:), next(:)
      integer(ESMF_KIND_I4), pointer:: mask(:)
      integer(ESMF_KIND_I4), pointer:: allCounts(:)
      character(len=256) :: titlelocal, norm, map_method
      character(len=256) :: esmf_regrid_method,conventions
      type(ESMF_RegridMethod_Flag) :: methodlocal
      character(len=80) :: srcunits, dstunits
      integer :: maxcount
      type(ESMF_FileFormat_Flag) :: srcFileTypeLocal, dstFileTypeLocal
      integer :: srcNodeDim, dstNodeDim, srcCoordDim, dstCoordDim
      integer, parameter :: nf90_noerror = 0
      character(len=256) :: errmsg
      character(len=20) :: varStr
      type(ESMF_Logical) :: largeFileFlaglocal
      type(ESMF_Logical) :: netcdf4FileFlaglocal
      logical            :: faceCoordFlag
      logical            :: srchasbound, dsthasbound
      logical            :: useSrcCornerlocal, useDstCornerlocal
      type(ESMF_NormType_Flag):: localNormType
      logical            :: src_has_area, dst_has_area
      type(ESMF_Mosaic)  :: srcmosaic, dstmosaic
      character(len=ESMF_MAXPATHLEN) :: tempname
      integer            :: totalsize, totallen
      integer            :: meshId
      integer            :: memstat
      integer            :: dim
      type(ESMF_CoordSys_Flag):: coordsys

#ifdef ESMF_NETCDF
      ! write out the indices and weights table sequentially to the output file
      ! first find out the starting index of my portion of table
      ! Global reduce

      src_has_area = .false.
      dst_has_area = .false.
      coordsys = ESMF_COORDSYS_SPH_DEG

      if (present(srcFileType)) then
          srcFileTypeLocal = srcFileType
      else
        srcFileTypeLocal = ESMF_FILEFORMAT_SCRIP
      endif     

      if (present(dstFileType)) then
          dstFileTypeLocal = dstFileType
      else
        dstFileTypeLocal = ESMF_FILEFORMAT_SCRIP
      endif     

      if (present(largeFileFlag)) then
        largeFileFlaglocal = largeFileFlag
      else
        largeFileFlaglocal = .false.
      endif

      if (present(netcdf4FileFlag)) then
        netcdf4FileFlaglocal = netcdf4FileFlag
      else
        netcdf4FileFlaglocal = .false.
      endif

      ! Handle optional normType argument
      if (present(normType)) then
         localNormType=normType
      else
         localNormType=ESMF_NORMTYPE_DSTAREA
      endif

      if (present(useSrcCorner)) then
         useSrcCornerlocal=useSrcCorner
      else
         useSrcCornerlocal=.false.
      endif

      if (present(useDstCorner)) then
         useDstCornerlocal=useDstCorner
      else
         useDstCornerlocal=.false.
      endif

      call ESMF_VMGetCurrent(vm, rc=rc)
      if (rc /= ESMF_SUCCESS) return

      ! set up local pet info
      call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
      if (rc /= ESMF_SUCCESS) return

      localCount(1)=size(factorList,1)
      allocate(allCounts(PetCnt), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat,  &
          ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_VMAllGather(vm,localCount,allCounts,1,rc=status)
      if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

      ! calculate the size of the global weight table
      total = 0
      do i=1,PetCnt
         total=allCounts(i)+total
      end do
      !print *, PetNo, 'local count ', localCount(1), AllCounts(PetNo+1), total

     !Read the variables from the input grid files at PET0
      if (PetNo == 0) then
         ! Check if srcFile and dstFile exists
         if (.not. present(srcFile)) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                  msg="- The srcFile argument does not exist on PET0 ", &
                  ESMF_CONTEXT, rcToReturn=rc)
             return
         endif
         if (.not. present(dstFile)) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                  msg="- The dstFile argument does not exist on PET0 ",  &
                  ESMF_CONTEXT, rcToReturn=rc)
             return
         endif
         ! Create output file and create dimensions and variables
         call c_nc_create(wgtFile, NF90_CLOBBER, &
                largeFileFlaglocal, netcdf4FileFlaglocal, ncid, status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

         ! global variables
          if (present(title)) then
             titlelocal = trim(title)
          else
             titlelocal = "ESMF Offline Regridding Weight Generator"
          endif

          ! Norm type
          if (localNormType .eq. ESMF_NORMTYPE_DSTAREA) then
             norm = "destarea"
          elseif (localNormType .eq. ESMF_NORMTYPE_FRACAREA) then
             norm = "fracarea"
          else
             norm = "unknown"
          endif

         ! Regrid method
         if (present(method)) then
           methodlocal = method
           if (methodlocal%regridmethod == ESMF_REGRIDMETHOD_BILINEAR%regridmethod) then
              map_method = "Bilinear remapping"
              esmf_regrid_method = "Bilinear"
           elseif (methodlocal%regridmethod == ESMF_REGRIDMETHOD_PATCH%regridmethod) then
              !scrip_test does not recognize patch remapping
              map_method = "Bilinear remapping"
              esmf_regrid_method = "Higher-order Patch"
           elseif (methodlocal%regridmethod == ESMF_REGRIDMETHOD_CONSERVE%regridmethod) then
              map_method = "Conservative remapping"
              esmf_regrid_method = "First-order Conservative"
           elseif (methodlocal%regridmethod == ESMF_REGRIDMETHOD_CONSERVE_2ND%regridmethod) then
              map_method = "Conservative remapping"
              esmf_regrid_method = "Second-order Conservative"
           elseif (methodlocal%regridmethod == ESMF_REGRIDMETHOD_NEAREST_STOD%regridmethod) then
              map_method = "Bilinear remapping"
              esmf_regrid_method = "Nearest source to destination"
           elseif (methodlocal%regridmethod == ESMF_REGRIDMETHOD_NEAREST_DTOS%regridmethod) then
              map_method = "Bilinear remapping"
              esmf_regrid_method = "Nearest destination to source"
           else
              !report error
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                  msg="- regridmethod not recongized", &
                  ESMF_CONTEXT, rcToReturn=rc)
              return
           endif
         else
           methodlocal = ESMF_REGRIDMETHOD_BILINEAR
           map_method = "Bilinear remapping"
           esmf_regrid_method = "Bilinear"
         endif
         conventions = "NCAR-CSM"

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "title", trim(titlelocal))
         errmsg = "Attribute title in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "normalization", trim(norm))
         errmsg = "Attribute normalization in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "map_method", trim(map_method))
         errmsg = "Attribute map_method in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "ESMF_regrid_method", &
                        trim(esmf_regrid_method))
         errmsg = "Attribute esmf_regrid_method in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "conventions", trim(conventions))
         errmsg = "Attribute conventions in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "domain_a", trim(srcFile))
         errmsg = "Attribute domain_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "domain_b", trim(dstFile))
         errmsg = "Attribute domain_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "grid_file_src", trim(srcFile))
         errmsg = "Attribute grid_file_src in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "grid_file_dst", trim(dstFile))
         errmsg = "Attribute grid_file_dst in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "CVS_revision", ESMF_VERSION_STRING)
         errmsg = "Attribute CVS_revision in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! Get Source Grid dimension and variables
        if (srcFileTypeLocal == ESMF_FILEFORMAT_SCRIP) then
          allocate(src_grid_dims(2), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return

          call ESMF_ScripInq(srcFile, grid_rank=src_grid_rank, grid_size=srcDim, &
              grid_dims=src_grid_dims, grid_corners=src_grid_corner, &
              has_area=src_has_area, rc=status)
          ! The grid_dims for an unstructured grie (grid_rank = 1) is not used
          ! by SCRIP, thus some of the SCRIP files did not set this value correctly.
          ! This causes the scrip_test generating wrong output data, i.e.
          ! the grid coordinates will not be written in the weight file
          if (src_grid_rank == 1) src_grid_dims(1) = srcDim
          call ESMF_ScripInqUnits(srcFile,units = srcunits, rc=status)
        else if (srcFileTypeLocal == ESMF_FILEFORMAT_GRIDSPEC) then
          allocate(src_grid_dims(2), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return
          call ESMF_GridspecInq(srcFile, src_ndims, src_grid_dims, &
                dimids = src_dimids, coordids = src_coordids, &
                coord_names = srccoordnames, hasbound=srchasbound, &
                units=srcunits,rc=status)
          if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          src_grid_rank = 2
          srcDim = src_grid_dims(1)*src_grid_dims(2)
          if (src_ndims == 1) then
                src_grid_corner = 2
          else
                src_grid_corner = 4
          endif
        else if (srcFileTypeLocal == ESMF_FILEFORMAT_TILE) then
          allocate(src_grid_dims(2), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return
          ! this returns the size of the center stagger, not the supergrid
          call ESMF_GridSpecQueryTileSize(srcfile, src_grid_dims(1), src_grid_dims(2), &
                  units=srcunits, rc=status)
          if (ESMF_LogFoundError(status, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          src_grid_rank = 2
          srcDim = src_grid_dims(1)*src_grid_dims(2)
          src_grid_corner = 4
        else if (srcFileTypeLocal == ESMF_FILEFORMAT_ESMFMESH) then
          ! If bilinear, we have to switch node and elment, so the nodeCount became srcDim and
          ! elementCount becomes srcNodeDim. Hard code src_grid_corner to 3.  The xv_a and xv_b
          ! will be empty
          if (useSrcCornerlocal .and. &
              (methodlocal%regridmethod ==ESMF_REGRIDMETHOD_BILINEAR%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_PATCH%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_STOD%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_DTOS%regridmethod)) then
            call ESMF_EsmfInq(srcFile, nodeCount=srcDim,  &
                coordDim = srcCoordDim, elementCount=srcNodeDim, rc=status)
                src_grid_corner =3
          else
            call ESMF_EsmfInq(srcFile, elementCount=srcDim, maxNodePElement=src_grid_corner, &
                coordDim = srcCoordDim, nodeCount=srcNodeDim, haveArea=src_has_area, rc=status)
          endif
          call ESMF_EsmfInqUnits(srcFile,units = srcunits, rc=status)
          allocate(src_grid_dims(1), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return

          src_grid_dims(1)=srcDim
          src_grid_rank = 1
        else if (srcFileTypeLocal == ESMF_FILEFORMAT_UGRID) then
          if (useSrcCornerlocal .and. &
              (methodlocal%regridmethod ==ESMF_REGRIDMETHOD_BILINEAR%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_PATCH%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_STOD%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_DTOS%regridmethod)) then
            call ESMF_UGridInq(srcFile, srcmeshname, nodeCount=srcDim,  &
                nodeCoordDim=dim, units=srcunits, rc=status)
            ! If it is 1D network topology, there is no corner coordinates
            if (dim==1) then 
              src_grid_corner = 0
            else
              src_grid_corner =3
            endif
          else
            call ESMF_UGridInq(srcFile, srcmeshname, elementCount=srcDim, &
                maxNodePElement=src_grid_corner, units=srcunits, &
                nodeCount = srcNodeDim, rc=status)
          endif
          if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          srcCoordDim = 2
          allocate(src_grid_dims(1), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return

          src_grid_dims(1)=srcDim
          src_grid_rank = 1
        else if (srcFileTypeLocal == ESMF_FILEFORMAT_MOSAIC) then
          call ESMF_GridSpecReadMosaic(srcFile, srcmosaic, tileFilePath=tileFilePath, rc=status)
          if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
          srcCoordDim = 2
          src_grid_rank = 1
          src_grid_corner = 0
          allocate(src_grid_dims(1))
          srcDim = srcmosaic%nx * srcmosaic%ny * srcmosaic%ntiles
          src_grid_dims(1)=srcDim
          srcunits = 'degrees'
        endif

        if (dstFileTypelocal == ESMF_FILEFORMAT_SCRIP) then
          allocate(dst_grid_dims(2), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return

          call ESMF_ScripInq(dstFile, grid_rank=dst_grid_rank, grid_size=dstDim, &
             grid_dims=dst_grid_dims, grid_corners=dst_grid_corner, &
             has_area=dst_has_area, rc=status)
          ! The grid_dims for an unstructured grie (grid_rank = 1) is not used
          ! by SCRIP, thus some of the SCRIP files did not set this value correctly.
          ! This causes the scrip_test generating wrong output data, i.e.
          ! the grid coordinates will not be written in the weight file
          if (dst_grid_rank == 1) dst_grid_dims(1) = dstDim
          call ESMF_ScripInqUnits(dstFile,units = dstunits, rc=status)
        else if (dstFileTypeLocal == ESMF_FILEFORMAT_GRIDSPEC) then
          allocate(dst_grid_dims(2), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return

          call ESMF_GridspecInq(dstFile, dst_ndims, dst_grid_dims, &
                dimids = dst_dimids, coordids = dst_coordids, &
                coord_names = dstcoordnames, hasbound=dsthasbound, &
                units=dstunits, rc=status)
          if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          dst_grid_rank = 2
          dstDim = dst_grid_dims(1)*dst_grid_dims(2)
          if (dst_ndims == 1) then
                dst_grid_corner = 2
          else
                dst_grid_corner = 4
          endif
        else if (dstFileTypeLocal == ESMF_FILEFORMAT_TILE) then
          allocate(dst_grid_dims(2), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return
          ! this returns the size of the center stagger, not the supergrid
          call ESMF_GridSpecQueryTileSize(dstfile, dst_grid_dims(1),dst_grid_dims(2), &
               units=dstunits, rc=status)
          if (ESMF_LogFoundError(status, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rcToReturn=rc)) return
          dst_grid_rank = 2
          dstDim = dst_grid_dims(1)*dst_grid_dims(2)
          dst_grid_corner = 4
        else if (dstFileTypeLocal == ESMF_FILEFORMAT_ESMFMESH) then
          ! If bilinear, we have to switch node and elment, so the nodeCount became dstDim and
          ! elementCount becomes dstNodeDim. Hard code dst_grid_corner to 3.  The xv_a and xv_b
          ! will be empty
          if (useDstCornerlocal .and. &
              (methodlocal%regridmethod ==ESMF_REGRIDMETHOD_BILINEAR%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_PATCH%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_STOD%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_DTOS%regridmethod)) then
            call ESMF_EsmfInq(dstFile, nodeCount=dstDim,  &
                coordDim = dstCoordDim, elementCount=dstNodeDim, rc=status)
                dst_grid_corner =3
          else
            call ESMF_EsmfInq(dstFile, elementCount=dstDim, maxNodePElement=dst_grid_corner, &
              coordDim = dstCoordDim, nodeCount=dstNodeDim, haveArea=dst_has_area, rc=status)
          endif
          call ESMF_EsmfInqUnits(dstFile,units = dstunits, rc=status)
          allocate(dst_grid_dims(1), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return

          dst_grid_dims(1)=dstDim
          dst_grid_rank = 1
        else if (dstFileTypeLocal == ESMF_FILEFORMAT_UGRID) then
          if (useDstCornerlocal .and. &
              (methodlocal%regridmethod ==ESMF_REGRIDMETHOD_BILINEAR%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_PATCH%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_STOD%regridmethod .or. &
              methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_DTOS%regridmethod)) then
            call ESMF_UGridInq(dstFile, dstmeshname, nodeCount=dstDim,  &
                nodeCoordDim=dim, units=dstunits, rc=status)
            if (dim==1) then 
              dst_grid_corner = 0
            else
              dst_grid_corner = 3
            endif
          else
            call ESMF_UGridInq(dstFile, dstmeshname, elementCount=dstDim, &
                maxNodePElement=dst_grid_corner, units=dstunits, &
                nodeCount = dstNodeDim, rc=status)
          endif
          if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                        ESMF_CONTEXT, rcToReturn=rc)) return
          dstCoordDim = 2
          allocate(dst_grid_dims(1), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return

          dst_grid_dims(1)=dstDim
          dst_grid_rank = 1
        else if (dstFileTypeLocal == ESMF_FILEFORMAT_MOSAIC) then
          call ESMF_GridSpecReadMosaic(dstFile, dstmosaic, tileFilePath=tileFilePath, rc=status)
          if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
          dstCoordDim = 2
          dst_grid_rank = 1
          dst_grid_corner = 0
          allocate(dst_grid_dims(1), stat=memstat)
          if (ESMF_LogFoundAllocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return

          dstDim = dstmosaic%nx * dstmosaic%ny * dstmosaic%ntiles
          dst_grid_dims(1)=dstDim
          dstunits = 'degrees'

        endif
        ! define dimensions
         ncStatus = nf90_def_dim(ncid,"n_a",srcDim, naDimId)
         errmsg = "Dimension n_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_def_dim(ncid,"n_b",dstDim, nbDimId)
         errmsg = "Dimension n_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_def_dim(ncid,"n_s",total, nsDimId)
         errmsg = "Dimension n_s in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! if src_grid_corner < 0 -- the input file is a ESMF Mesh file with ragged array
        ! do not define nv_a, xv_a and yv_a
        if (src_grid_corner > 0) then
        ! define max number of vertices
         ncStatus = nf90_def_dim(ncid,"nv_a",src_grid_corner, nvaDimId)
         errmsg = "Dimension nv_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         endif

         if (dst_grid_corner > 0) then
         ncStatus = nf90_def_dim(ncid,"nv_b",dst_grid_corner, nvbDimId)
         errmsg = "Dimension nv_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         endif

        ! define max number of vertices
         ncStatus = nf90_def_dim(ncid,"num_wgts",1, VarId)
         errmsg = "Dimension num_wgts in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ! define grid ranks
         ncStatus = nf90_def_dim(ncid,"src_grid_rank",src_grid_rank, srankDimId)
         errmsg = "Dimension src_grid_rank in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_def_dim(ncid,"dst_grid_rank",dst_grid_rank, drankDimId)
         errmsg = "Dimension dst_grid_rank in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! define variables
         ! Grid Dims
         ncStatus = nf90_def_var(ncid,"src_grid_dims",NF90_INT, (/srankDimId/),  VarId)
         errmsg = "Variable src_grid_dims in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_def_var(ncid,"dst_grid_dims",NF90_INT, (/drankDimId/),  VarId)
         errmsg = "Variable dst_grid_dims in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! yc_a: source vertex coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yc_a",NF90_DOUBLE, (/naDimId/),  VarId)
         errmsg = "Variable yc_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(srcunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! yc_b: destination vertex coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yc_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         errmsg = "Variable yc_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(dstunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! xc_a: source vertex coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xc_a",NF90_DOUBLE, (/naDimId/),  VarId)
         errmsg = "Variable xc_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(srcunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! xc_b: dest. vertex coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xc_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         errmsg = "Variable xc_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(dstunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        if (src_grid_corner > 0) then
        ! yv_a: source corner coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yv_a",NF90_DOUBLE, (/nvaDimId,naDimId/),  VarId)
         errmsg = "Variable yv_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(srcunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! xv_a: source corner coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xv_a",NF90_DOUBLE, (/nvaDimId,naDimId/),  VarId)
         errmsg = "Variable xv_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(srcunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         endif

         if (dst_grid_corner > 0) then
        ! yv_b: source corner coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yv_b",NF90_DOUBLE, (/nvbDimId,nbDimId/),  VarId)
         errmsg = "Variable yv_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(dstunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! xv_b: source corner coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xv_b",NF90_DOUBLE, (/nvbDimId,nbDimId/),  VarId)
         errmsg = "Variable xv_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", trim(dstunits))
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         endif

        ! mask_a
         ncStatus = nf90_def_var(ncid,"mask_a",NF90_INT, (/naDimId/),  VarId)
         errmsg = "Variable mask_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "unitless")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! mask_b
         ncStatus = nf90_def_var(ncid,"mask_b",NF90_INT, (/nbDimId/),  VarId)
         errmsg = "Variable mask_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "unitless")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! area_a
         ncStatus = nf90_def_var(ncid,"area_a",NF90_DOUBLE, (/naDimId/),  VarId)
         errmsg = "Variable area_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "square radians")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! area_b
         ncStatus = nf90_def_var(ncid,"area_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         errmsg = "Variable area_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "square radians")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! frac_a
         ncStatus = nf90_def_var(ncid,"frac_a",NF90_DOUBLE, (/naDimId/),  VarId)
         errmsg = "Variable frac_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "unitless")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! frac_b
         ncStatus = nf90_def_var(ncid,"frac_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         errmsg = "Variable frac_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
         ncStatus = nf90_put_att(ncid, VarId, "units", "unitless")
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return


        ! col: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"col",NF90_INT, (/nsDimId/),  VarId)
         errmsg = "Variable col in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! row: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"row",NF90_INT, (/nsDimId/),  VarId)
         errmsg = "Variable row in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! S: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"S",NF90_DOUBLE, (/nsDimId/),  VarId)
         errmsg = "Variable S in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus=nf90_enddef(ncid)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           trim(wgtFile),&
           rc)) return

        ! write src_grid_dims and dst_grid_dims
        ncStatus=nf90_inq_varid(ncid,"src_grid_dims",VarId)
        ncStatus=nf90_put_var(ncid,VarId, src_grid_dims(1:src_grid_rank))
        errmsg = "Variable src_grid_dims in "//trim(wgtfile)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg,&
        rc)) return

        ncStatus=nf90_inq_varid(ncid,"dst_grid_dims",VarId)
        ncStatus=nf90_put_var(ncid,VarId, dst_grid_dims(1:dst_grid_rank))
        errmsg = "Variable dst_grid_dims in "//trim(wgtfile)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg,&
        rc)) return

        if (srcFileTypeLocal == ESMF_FILEFORMAT_SCRIP) then
           ! Read the srcGrid variables and write them out
           allocate(latBuffer(srcDim), lonBuffer(srcDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           call ESMF_ScripGetVar(srcFile, grid_center_lon=lonBuffer, &
                grid_center_lat=latBuffer, rc=status)
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

           ncStatus=nf90_inq_varid(ncid,"xc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
           errmsg = "Variable xc_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer)
           errmsg = "Variable yc_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(latBuffer, lonBuffer, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return


          ! Write xv_a, yv_a    
           allocate(latBuffer2(src_grid_corner,srcDim),lonBuffer2(src_grid_corner,srcDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           call ESMF_ScripGetVar(srcFile, grid_corner_lon=lonBuffer2, &
                grid_corner_lat=latBuffer2, rc=status)
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

           ncStatus=nf90_inq_varid(ncid,"xv_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)
           errmsg = "Variable xv_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yv_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer2)
           errmsg = "Variable yv_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(latBuffer2, lonBuffer2, stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return


           allocate(mask(srcDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           call ESMF_ScripGetVar(srcFile, grid_imask=mask, rc=status)
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           ncStatus=nf90_inq_varid(ncid,"mask_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, mask)
           errmsg = "Variable mask_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(mask, stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return


           if (src_has_area .and. .not. present(srcArea)) then
              allocate(area(srcDim), stat=memstat)
              if (ESMF_LogFoundAllocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

              call ESMF_ScripGetVar(srcFile, grid_area=area, rc=status)
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              ncStatus=nf90_inq_varid(ncid,"area_a",VarId)
              ncStatus=nf90_put_var(ncid,VarId, area)
              errmsg = "Variable area_a in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,&
                errmsg,&
                rc)) return
              deallocate(area, stat=memstat)
              if (ESMF_LogFoundAllocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

           endif
        else if (srcFileTypeLocal == ESMF_FILEFORMAT_GRIDSPEC) then
           allocate(lonBuffer(srcDim), latBuffer(srcDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           if (src_ndims == 1) then
             allocate(lonBuffer1D(src_grid_dims(1)), latBuffer1D(src_grid_dims(2)), stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

             if (srchasbound) then
                allocate(cornerlon2D(src_grid_corner,src_grid_dims(1)), &
                      cornerlat2D(src_grid_corner,src_grid_dims(2)), stat=memstat)
                if (ESMF_LogFoundAllocError(memstat,  &
                    ESMF_CONTEXT, rcToReturn=rc)) return

                call ESMF_GridspecGetVar1D(srcFile, src_coordids, lonBuffer1D, latBuffer1D, &
                      cornerlon = cornerlon2D, cornerlat=cornerlat2D, rc=status)
                if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc)) return
             else
                call ESMF_GridspecGetVar1D(srcFile, src_coordids, lonBuffer1D, latBuffer1D, &
                      rc=status)
                if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc)) return
             endif
             k=1
             do j=1,src_grid_dims(2)
              do i=1,src_grid_dims(1)
                lonBuffer(k)=lonBuffer1D(i)
                latBuffer(k)=latBuffer1D(j)
                k=k+1
              enddo
            enddo
            deallocate(lonBuffer1D, latBuffer1D, stat=memstat)
            if (ESMF_LogFoundDeallocError(memstat,  &
                ESMF_CONTEXT, rcToReturn=rc)) return

            if (srchasbound) then
               allocate(lonBuffer2(src_grid_corner, srcDim),latBuffer2(src_grid_corner,srcDim), stat=memstat)
               if (ESMF_LogFoundAllocError(memstat,  &
                   ESMF_CONTEXT, rcToReturn=rc)) return

               k=1
               do j=1,src_grid_dims(2)
                 do i=1,src_grid_dims(1)
                   lonBuffer2(:,k)=cornerlon2D(:,i)
                   latBuffer2(:,k)=cornerlat2D(:,j)
                   k=k+1
                 enddo
               enddo
            endif
          else
             allocate(lonBuffer2(src_grid_dims(1),src_grid_dims(2)), &
                      latBuffer2(src_grid_dims(1),src_grid_dims(2)), stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

             if (srchasbound) then              
                allocate(cornerlon3D(src_grid_corner,src_grid_dims(1), src_grid_dims(2)),&
                    cornerlat3D(src_grid_corner,src_grid_dims(1), src_grid_dims(2)), stat=memstat)
                if (ESMF_LogFoundAllocError(memstat,  &
                    ESMF_CONTEXT, rcToReturn=rc)) return

                call ESMF_GridspecGetVar2D(srcFile, src_coordids, lonBuffer2, latBuffer2, &
                    cornerlon=cornerlon3D, cornerlat=cornerlat3D, rc=status)
                if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
             else
                call ESMF_GridspecGetVar2D(srcFile, src_coordids, lonBuffer2, latBuffer2, &
                    rc=status)
                if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
             endif
             lonBuffer = reshape(lonBuffer2, (/srcDim/))
             latBuffer = reshape(latBuffer2, (/srcDim/))
             deallocate(lonBuffer2, latBuffer2, stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

             if (srchasbound) then
                allocate(lonBuffer2(src_grid_corner, srcDim),latBuffer2(src_grid_corner,srcDim), stat=memstat)
                if (ESMF_LogFoundAllocError(memstat,  &
                    ESMF_CONTEXT, rcToReturn=rc)) return

                lonBuffer2=reshape(cornerlon3D, (/src_grid_corner, srcDim/))
                latBuffer2=reshape(cornerlat3D, (/src_grid_corner, srcDim/))
                deallocate(cornerlon3D, cornerlat3D, stat=memstat)
                if (ESMF_LogFoundAllocError(memstat,  &
                    ESMF_CONTEXT, rcToReturn=rc)) return

             endif
           endif

           ncStatus=nf90_inq_varid(ncid,"xc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
           errmsg = "Variable xc_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer)
           errmsg = "Variable yc_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(latBuffer, lonBuffer, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

        
          ! Write xv_a, yv_a    
           if (srchasbound) then
           ncStatus=nf90_inq_varid(ncid,"xv_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)
           errmsg = "Variable xv_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yv_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer2)
           errmsg = "Variable yv_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(lonBuffer2, latBuffer2, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           endif

           ! Mask
           allocate(mask(srcDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           mask(:)=1
           if (present(srcMissingValue)) then
             if (srcMissingValue) then
              allocate(varBuffer(src_grid_dims(1),src_grid_dims(2)), stat=memstat)
              if (ESMF_LogFoundAllocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

              call ESMF_GridspecGetVarByName(srcFile, srcvarname, src_dimids, &
                                varBuffer, missing_value = missing_value, &
                                rc=status)
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              k=1
              do i=1,size(varBuffer,2)
                do j=1,size(varBuffer,1)
                   if (varBuffer(j,i) == missing_value) mask(k)=0
                   k=k+1
                enddo   
              enddo
              deallocate(varBuffer, stat=memstat)
              if (ESMF_LogFoundDeallocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

            endif
           endif

           ncStatus=nf90_inq_varid(ncid,"mask_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, mask)
           errmsg = "Variable mask_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(mask, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

        else if (srcFileTypeLocal == ESMF_FILEFORMAT_TILE) then
           allocate(lonBuffer2(src_grid_dims(1),src_grid_dims(2)), &
                    latBuffer2(src_grid_dims(1), src_grid_dims(2)), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return
           allocate(cornerlon2D(src_grid_dims(1)+1,src_grid_dims(2)+1), &
                    cornerlat2D(src_grid_dims(1)+1, src_grid_dims(2)+1), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

           call ESMF_GridspecReadTile(srcFile, src_grid_dims(1), src_grid_dims(2), &
                lonBuffer2, latBuffer2, cornerLon=cornerlon2D, cornerLat=cornerlat2D, &
                rc=status)
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           allocate(lonBuffer(srcDim), latBuffer(srcDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           lonBuffer = reshape(lonBuffer2, (/srcDim/))
           latBuffer = reshape(latBuffer2, (/srcDim/))
           deallocate(lonBuffer2, latBuffer2, stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           allocate(lonBuffer2(src_grid_corner, srcDim),latBuffer2(src_grid_corner,srcDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           k=1
           do j=1,src_grid_dims(2)
              do i=1,src_grid_dims(1)
                lonBuffer2(1,k)=cornerlon2D(i,j) 
                lonBuffer2(2,k)=cornerlon2D(i,j+1) 
                lonBuffer2(3,k)=cornerlon2D(i+1,j+1) 
                lonBuffer2(4,k)=cornerlon2D(i+1,j) 
                latBuffer2(1,k)=cornerlat2D(i,j) 
                latBuffer2(2,k)=cornerlat2D(i,j+1) 
                latBuffer2(3,k)=cornerlat2D(i+1,j+1) 
                latBuffer2(4,k)=cornerlat2D(i+1,j) 
                k=k+1
              enddo
           enddo

           deallocate(cornerlon2D, cornerlat2D, stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           ncStatus=nf90_inq_varid(ncid,"xc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
           errmsg = "Variable xc_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer)
           errmsg = "Variable yc_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(latBuffer, lonBuffer, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

        
          ! Write xv_a, yv_a    
           ncStatus=nf90_inq_varid(ncid,"xv_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)
           errmsg = "Variable xv_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yv_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer2)
           errmsg = "Variable yv_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(lonBuffer2, latBuffer2, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

        else if (srcFileTypeLocal == ESMF_FILEFORMAT_ESMFMESH) then
           ! ESMF unstructured grid
           ncStatus=nf90_open(srcFile,NF90_NOWRITE,ncid1)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             trim(srcFile),&
             rc)) return
           if (useSrcCornerlocal .and. (methodlocal%regridmethod ==ESMF_REGRIDMETHOD_BILINEAR%regridmethod &
                .or. methodlocal%regridmethod ==ESMF_REGRIDMETHOD_PATCH%regridmethod &
                .or. methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_STOD%regridmethod &
                .or. methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_DTOS%regridmethod)) then
             ! check if centerCoords exit
             ncStatus=nf90_inq_varid(ncid1,"nodeCoords",VarId)
             varStr = "nodeCoords"
             src_has_area = .false.
           else
             ncStatus=nf90_inq_varid(ncid1,"centerCoords",VarId)
             varStr = "centerCoords"
           endif
           if (ncStatus /= nf90_noerror) then
             write(*,*) "Warning: "//trim(varStr)// &
                " not present in src grid file, so not outputting xc_a and yc_a to weight file."
             write(*,*)
           else
             allocate(latBuffer2(srcCoordDim, srcDim), stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

             allocate(latBuffer(srcDim), lonBuffer(srcDim), stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

             ncStatus=nf90_get_var(ncid1,VarId, latBuffer2)
             errmsg = "Variable "//trim(varStr)//" in "//trim(srcFile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg,&
               rc)) return
             do i=1,srcDim
                lonBuffer(i)=latBuffer2(1,i)
                latBuffer(i)=latBuffer2(2,i)
             enddo
             ncStatus=nf90_inq_varid(ncid,"xc_a",VarId)
             ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
             errmsg = "Variable xc_a in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg,&
               rc)) return

             ncStatus=nf90_inq_varid(ncid,"yc_a",VarId)
             ncStatus=nf90_put_var(ncid,VarId, latBuffer)
             errmsg = "Variable yc_a in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg,&
               rc)) return
             deallocate(latBuffer, lonBuffer, latBuffer2, stat=memstat)
             if (ESMF_LogFoundDeallocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

           endif

           ! only write out xv_a and yv_a when the regrid method is conserve
           if (.not. useSrcCornerlocal .or. &
                methodlocal%regridmethod == ESMF_REGRIDMETHOD_CONSERVE%regridmethod .or. &
                methodlocal%regridmethod ==ESMF_REGRIDMETHOD_CONSERVE_2ND%regridmethod) then
           ! output xv_a and yv_a is harder, we have to read in the nodeCoords and
           ! elementConn and construct the the latitudes and longitudes for
           ! all the corner vertices
             if (src_grid_corner > 0) then
             allocate(latBuffer2(src_grid_corner,srcDim),lonBuffer2(src_grid_corner,srcDim), stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

             call ESMF_EsmfGetVerts(ncid1, srcFile, srcDim, src_grid_corner, srcNodeDim, &
                  latBuffer2, lonBuffer2,status)
             if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return

             ncStatus=nf90_inq_varid(ncid,"xv_a",VarId)
             ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)
             errmsg = "Variable xv_a in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE, errmsg, &
               rc)) return

             ncStatus=nf90_inq_varid(ncid,"yv_a",VarId)
             ncStatus=nf90_put_var(ncid,VarId, latBuffer2)
             errmsg = "Variable yv_a in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
             deallocate(latBuffer2, lonBuffer2, stat=memstat)
             if (ESMF_LogFoundDeallocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

             endif
           endif
           allocate(mask(srcDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           if (.not. useSrcCornerlocal .and. &
               (methodlocal%regridmethod ==ESMF_REGRIDMETHOD_CONSERVE%regridmethod .or. &
                methodlocal%regridmethod ==ESMF_REGRIDMETHOD_CONSERVE_2ND%regridmethod)) then
             ncStatus=nf90_inq_varid(ncid1,"elementMask",VarId)
             if (ncStatus /= nf90_noerror) then
               write(*,*) "Warning: elementMask"// &
                " not present in src grid file, so setting mask_a=1 in weight file."
               write(*,*)
               mask = 1
             else
               ncStatus=nf90_get_var(ncid1,VarId, mask)
               errmsg = "Variable elementMask in "//trim(srcFile)
               if (CDFCheckError (ncStatus, &
                 ESMF_METHOD, &
                 ESMF_SRCLINE,errmsg,&
                 rc)) return
              end if
           else
              mask = 1
           endif
           ncStatus=nf90_inq_varid(ncid,"mask_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, mask)
           errmsg = "Variable mask_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,errmsg,&
             rc)) return
           deallocate(mask, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           if (src_has_area .and. .not. present(srcArea)) then
             allocate(area(srcDim), stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

             ncStatus=nf90_inq_varid(ncid1,"elementArea",VarId)
             errmsg = "Variable elementArea in "//trim(srcFile)
             if (CDFCheckError (ncStatus, &
                 ESMF_METHOD, &
                 ESMF_SRCLINE,errmsg,&
                 rc)) return
             ncStatus=nf90_get_var(ncid1,VarId, area)
             errmsg = "Variable elementArea in "//trim(srcFile)
             if (CDFCheckError (ncStatus, &
                 ESMF_METHOD, &
                 ESMF_SRCLINE,errmsg,&
                 rc)) return
             ncStatus=nf90_inq_varid(ncid,"area_a",VarId)
             ncStatus=nf90_put_var(ncid,VarId, area)
             errmsg = "Variable area_a in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
             deallocate(area, stat=memstat)
             if (ESMF_LogFoundDeallocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

           endif
           ncStatus=nf90_close(ncid1)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,trim(srcFile),&
             rc)) return
        else if (srcFileTypeLocal == ESMF_FILEFORMAT_UGRID) then
           call ESMF_UGridInq(srcfile, srcmeshname, meshId=meshId, faceCoordFlag=faceCoordFlag)
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
           if (.not. useSrcCornerlocal .or. &
                methodlocal%regridmethod ==ESMF_REGRIDMETHOD_CONSERVE%regridmethod .or. &
                methodlocal%regridmethod ==ESMF_REGRIDMETHOD_CONSERVE_2ND%regridmethod) then
             ! check if faceCoords exit
              allocate(latBuffer2(src_grid_corner,srcDim),&
                       lonBuffer2(src_grid_corner,srcDim), stat=memstat)
              if (ESMF_LogFoundAllocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

              if (faceCoordFlag) then
                allocate(latBuffer(srcDim), lonBuffer(srcDim), stat=memstat)
                if (ESMF_LogFoundAllocError(memstat,  &
                    ESMF_CONTEXT, rcToReturn=rc)) return

                call ESMF_UGridGetVar(srcfile, meshId, &
                   faceXcoords=lonBuffer, faceYcoords=latBuffer, &
                   faceNodeConnX=lonBuffer2, faceNodeConnY=latBuffer2, rc=status)
              else
                write(*,*) "Warning: face coordinates not present in src grid file,"// &
                  " so not outputting xc_a and yc_a to weight file."
                write(*,*)
                call ESMF_UGridGetVar(srcfile, meshId, &
                   faceNodeConnX=lonBuffer2, faceNodeConnY=latBuffer2, rc=status)
              endif
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              if (faceCoordFlag) then
                ncStatus=nf90_inq_varid(ncid,"xc_a",VarId)
                ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
                errmsg = "Variable xc_a in "//trim(wgtfile)
                if (CDFCheckError (ncStatus, &
                  ESMF_METHOD, &
                  ESMF_SRCLINE,errmsg,&
                  rc)) return
                ncStatus=nf90_inq_varid(ncid,"yc_a",VarId)
                ncStatus=nf90_put_var(ncid,VarId, latBuffer)
                errmsg = "Variable yc_a in "//trim(wgtfile)
                if (CDFCheckError (ncStatus, &
                  ESMF_METHOD, &
                  ESMF_SRCLINE,errmsg,&
                  rc)) return
                deallocate(latBuffer, lonBuffer, stat=memstat)
                if (ESMF_LogFoundDeallocError(memstat,  &
                    ESMF_CONTEXT, rcToReturn=rc)) return

              endif
              ncStatus=nf90_inq_varid(ncid,"xv_a",VarId)
              ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)
              errmsg = "Variable xv_b in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                  ESMF_METHOD, &
                  ESMF_SRCLINE,errmsg,&
                  rc)) return
              ncStatus=nf90_inq_varid(ncid,"yv_a",VarId)
              ncStatus=nf90_put_var(ncid,VarId, latBuffer2)
              errmsg = "Variable yv_b in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                  ESMF_METHOD, &
                  ESMF_SRCLINE,errmsg,&
                  rc)) return
              deallocate(latBuffer2, lonBuffer2, stat=memstat)
              if (ESMF_LogFoundDeallocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

           else
              allocate(latBuffer(srcDim), lonBuffer(srcDim), stat=memstat)
              if (ESMF_LogFoundAllocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

              call ESMF_UGridGetVar(srcfile, meshId, &
                   nodeXcoords=lonBuffer, nodeYcoords=latBuffer, rc=status)
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              ncStatus=nf90_inq_varid(ncid,"xc_a",VarId)
              ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
              errmsg = "Variable xc_a in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return
              ncStatus=nf90_inq_varid(ncid,"yc_a",VarId)
              ncStatus=nf90_put_var(ncid,VarId, latBuffer)
              errmsg = "Variable yc_a in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
              rc)) return
              deallocate(latBuffer, lonBuffer, stat=memstat)
              if (ESMF_LogFoundAllocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

           endif

           ! Write out mask
           allocate(mask(srcDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           mask(:)=1
           if (present(srcMissingValue)) then
             if (srcMissingValue) then
              allocate(varBuffer1D(srcDim), stat=memstat)
              if (ESMF_LogFoundAllocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

              call ESMF_UgridGetVarByName(srcFile, srcvarname, &
                                varBuffer1D, missingvalue = missing_value, &
                                rc=status)
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              do j=1,size(varBuffer1D)
                 if (varBuffer1D(j) == missing_value) mask(j)=0
              enddo
              deallocate(varBuffer1D, stat=memstat)
              if (ESMF_LogFoundDeallocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

             endif
            endif

           ncStatus=nf90_inq_varid(ncid,"mask_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, mask)
           errmsg = "Variable mask_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(mask, stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

        else if (srcFileTypeLocal == ESMF_FILEFORMAT_MOSAIC) then
           !read coordinates from the tile files
           allocate(latBuffer2(srcmosaic%nx, srcmosaic%ny), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           allocate(lonBuffer2(srcmosaic%nx, srcmosaic%ny), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           totalsize = srcmosaic%nx*srcmosaic%ny
           allocate(varBuffer1D(totalsize), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           ncStatus=nf90_inq_varid(ncid,"xc_a",VarId1)
           ncStatus=nf90_inq_varid(ncid,"yc_a",VarId2)
           errmsg = "Variable xc_a or yc_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,&
              errmsg,&
           rc)) return
           do k=1,srcmosaic%ntiles
             totallen = len_trim(srcmosaic%filenames(k))+len_trim(srcmosaic%tileDirectory)
             tempname = trim(srcmosaic%tileDirectory)//trim(srcmosaic%filenames(k))
             call ESMF_GridspecReadTile(trim(tempname),srcmosaic%nx, srcmosaic%ny, &
                  centerlon=lonBuffer2, centerlat=latBuffer2, rc=status)
             if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
             varBuffer1D=reshape(lonBuffer2,(/totalsize/))
             start=(k-1)*totalsize+1
             ncStatus = nf90_put_var(ncid, VarId1,varBuffer1D, (/start/), (/totalsize/))
             if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,&
                "writing xc_a in weight file", &
             rc)) return
             varBuffer1D=reshape(latBuffer2,(/totalsize/))
             ncStatus = nf90_put_var(ncid, VarId2, varBuffer1D, (/start/), (/totalsize/))
             if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,&
                "writing xc_a in weight file", &
             rc)) return
          enddo
          deallocate(lonBuffer2, latBuffer2, varBuffer1D, stat=memstat)
          if (ESMF_LogFoundDeallocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return

        endif

        ! Read the dstGrid variables and write them out
        if (dstFileTypeLocal == ESMF_FILEFORMAT_SCRIP) then
         allocate(latBuffer(dstDim), lonBuffer(dstDim), stat=memstat)
         if (ESMF_LogFoundAllocError(memstat,  &
             ESMF_CONTEXT, rcToReturn=rc)) return

         call ESMF_ScripGetVar(dstFile, grid_center_lon=lonBuffer, &
                grid_center_lat=latBuffer, rc=status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
         ncStatus=nf90_inq_varid(ncid,"xc_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
         errmsg = "Variable xc_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return

         ncStatus=nf90_inq_varid(ncid,"yc_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, latBuffer)
         errmsg = "Variable yc_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return
         deallocate(latBuffer, lonBuffer, stat=memstat)
         if (ESMF_LogFoundDeallocError(memstat,  &
             ESMF_CONTEXT, rcToReturn=rc)) return


         allocate(latBuffer2(dst_grid_corner,dstDim),lonBuffer2(dst_grid_corner,dstDim), stat=memstat)
         if (ESMF_LogFoundAllocError(memstat,  &
             ESMF_CONTEXT, rcToReturn=rc)) return

         call ESMF_ScripGetVar(dstFile, grid_corner_lon=lonBuffer2, &
                grid_corner_lat=latBuffer2, rc=status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

         ncStatus=nf90_inq_varid(ncid,"xv_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)
         errmsg = "Variable xv_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return

         ncStatus=nf90_inq_varid(ncid,"yv_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, latBuffer2)
         errmsg = "Variable yv_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return
         deallocate(latBuffer2, lonBuffer2, stat=memstat)
         if (ESMF_LogFoundDeallocError(memstat,  &
             ESMF_CONTEXT, rcToReturn=rc)) return

         allocate(mask(dstDim), stat=memstat)
         if (ESMF_LogFoundAllocError(memstat,  &
             ESMF_CONTEXT, rcToReturn=rc)) return

         call ESMF_ScripGetVar(dstFile, grid_imask=mask, rc=status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
         ncStatus=nf90_inq_varid(ncid,"mask_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, mask)
         errmsg = "Variable mask_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return
         deallocate(mask, stat=memstat)
         if (ESMF_LogFoundDeallocError(memstat,  &
             ESMF_CONTEXT, rcToReturn=rc)) return

         if (dst_has_area .and. .not. present(dstArea)) then
              allocate(area(dstDim), stat=memstat)
              if (ESMF_LogFoundAllocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

              call ESMF_ScripGetVar(dstFile, grid_area=area, rc=status)
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              ncStatus=nf90_inq_varid(ncid,"area_b",VarId)
              ncStatus=nf90_put_var(ncid,VarId, area)
              errmsg = "Variable area_b in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,&
                errmsg,&
                rc)) return
              deallocate(area, stat=memstat)
              if (ESMF_LogFoundDeallocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

         endif
        else if (dstFileTypeLocal == ESMF_FILEFORMAT_GRIDSPEC) then
           allocate(lonBuffer(dstDim), latBuffer(dstDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return


           ! check if bound variables exist or not
           if (dst_ndims == 1) then
             allocate(lonBuffer1D(dst_grid_dims(1)), latBuffer1D(dst_grid_dims(2)), stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return


             if (dsthasbound) then
                allocate(cornerlon2D(dst_grid_corner,dst_grid_dims(1)), &
                      cornerlat2D(dst_grid_corner,dst_grid_dims(2)), stat=memstat)
                if (ESMF_LogFoundAllocError(memstat,  &
                    ESMF_CONTEXT, rcToReturn=rc)) return

                call ESMF_GridspecGetVar1D(dstFile, dst_coordids, lonBuffer1D, latBuffer1D,&
                        cornerlon=cornerlon2D, cornerlat=cornerlat2D, rc=status)
             else
                call ESMF_GridspecGetVar1D(dstFile, dst_coordids, lonBuffer1D, latBuffer1D,&
                        rc=status)
             endif              
             if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
             k=1
             do j=1,dst_grid_dims(2)
              do i=1,dst_grid_dims(1)
                lonBuffer(k)=lonBuffer1D(i)
                latBuffer(k)=latBuffer1D(j)
                k=k+1
              enddo
             enddo
             deallocate(lonBuffer1D, latBuffer1D, stat=memstat)
             if (ESMF_LogFoundDeallocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return


             if (dsthasbound) then
               allocate(lonBuffer2(dst_grid_corner, dstDim),latBuffer2(dst_grid_corner,dstDim), stat=memstat)
               if (ESMF_LogFoundAllocError(memstat,  &
                   ESMF_CONTEXT, rcToReturn=rc)) return


               k=1
               do j=1,dst_grid_dims(2)
                 do i=1,dst_grid_dims(1)
                   lonBuffer2(:,k)=cornerlon2D(:,i)
                   latBuffer2(:,k)=cornerlat2D(:,j)
                   k=k+1
                 enddo
               enddo
               deallocate(cornerlon2D, cornerlat2D, stat=memstat)
               if (ESMF_LogFoundDeallocError(memstat,  &
                   ESMF_CONTEXT, rcToReturn=rc)) return


             endif
           else
             allocate(lonBuffer2(dst_grid_dims(1),dst_grid_dims(2)), &
                      latBuffer2(dst_grid_dims(1),dst_grid_dims(2)), stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return


             if (dsthasbound) then
               allocate(cornerlon3D(dst_grid_corner,dst_grid_dims(1), dst_grid_dims(2)),&
                    cornerlat3D(dst_grid_corner,dst_grid_dims(1), dst_grid_dims(2)), stat=memstat)
               if (ESMF_LogFoundAllocError(memstat,  &
                   ESMF_CONTEXT, rcToReturn=rc)) return


               call ESMF_GridspecGetVar2D(dstFile, dst_coordids, lonBuffer2, latBuffer2, &
                   cornerlon=cornerlon3D, cornerlat=cornerlat3D, rc=status)
             else
               call ESMF_GridspecGetVar2D(dstFile, dst_coordids, lonBuffer2, latBuffer2, &
                   rc=status)
             endif
             if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
             lonBuffer = reshape(lonBuffer2, (/dstDim/))
             latBuffer = reshape(latBuffer2, (/dstDim/))
             deallocate(lonBuffer2, latBuffer2, stat=memstat)
             if (ESMF_LogFoundDeallocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return


             if (dsthasbound) then
               allocate(lonBuffer2(dst_grid_corner, dstDim),latBuffer2(dst_grid_corner,dstDim),  &
                   stat=memstat)
               if (ESMF_LogFoundAllocError(memstat,  &
                   ESMF_CONTEXT, rcToReturn=rc)) return


               lonBuffer2=reshape(cornerlon3D, (/dst_grid_corner, dstDim/))
               latBuffer2=reshape(cornerlat3D, (/dst_grid_corner, dstDim/))
               deallocate(cornerlon3D, cornerlat3D, stat=memstat)
               if (ESMF_LogFoundDeallocError(memstat,  &
                   ESMF_CONTEXT, rcToReturn=rc)) return
             endif
           endif

           ncStatus=nf90_inq_varid(ncid,"xc_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
           errmsg = "Variable xc_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yc_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer)
           errmsg = "Variable yc_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(latBuffer, lonBuffer, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! Write xv_b, yv_b    
           if (dsthasbound) then
           ncStatus=nf90_inq_varid(ncid,"xv_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)
           errmsg = "Variable xv_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yv_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer2)
           errmsg = "Variable yv_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(lonBuffer2, latBuffer2, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           endif

           ! Mask
           allocate(mask(dstDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           mask(:)=1
           if (present(dstMissingValue)) then
             if (dstMissingValue) then
              allocate(varBuffer(dst_grid_dims(1),dst_grid_dims(2)), stat=memstat)
              if (ESMF_LogFoundAllocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

              call ESMF_GridspecGetVarByName(dstFile, dstvarname, dst_dimids, &
                                varBuffer, missing_value = missing_value, &
                                rc=status)
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              k=1
              do i=1,size(varBuffer,2)
                do j=1,size(varBuffer,1)
                   if (varBuffer(j,i) == missing_value) mask(k)=0
                   k=k+1
                enddo   
              enddo
              deallocate(varBuffer, stat=memstat)
              if (ESMF_LogFoundDeallocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

            endif
           endif

           ncStatus=nf90_inq_varid(ncid,"mask_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, mask)
           errmsg = "Variable mask_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(mask, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

        else if (dstFileTypeLocal == ESMF_FILEFORMAT_TILE) then
           allocate(lonBuffer2(dst_grid_dims(1),dst_grid_dims(2)), &
                    latBuffer2(dst_grid_dims(1), dst_grid_dims(2)), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return
           allocate(cornerlon2D(dst_grid_dims(1)+1,dst_grid_dims(2)+1), &
                    cornerlat2D(dst_grid_dims(1)+1, dst_grid_dims(2)+1), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

           call ESMF_GridspecReadTile(dstFile, dst_grid_dims(1), dst_grid_dims(2), &
                lonBuffer2, latBuffer2, cornerLon=cornerlon2D, cornerLat=cornerlat2D, &
                rc=status)
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           allocate(lonBuffer(dstDim), latBuffer(dstDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           lonBuffer = reshape(lonBuffer2, (/dstDim/))
           latBuffer = reshape(latBuffer2, (/dstDim/))
           deallocate(lonBuffer2, latBuffer2, stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           allocate(lonBuffer2(dst_grid_corner, dstDim),latBuffer2(dst_grid_corner,dstDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           k=1
           do j=1,dst_grid_dims(2)
              do i=1,dst_grid_dims(1)
                lonBuffer2(1,k)=cornerlon2D(i,j) 
                lonBuffer2(2,k)=cornerlon2D(i,j+1) 
                lonBuffer2(3,k)=cornerlon2D(i+1,j+1) 
                lonBuffer2(4,k)=cornerlon2D(i+1,j) 
                latBuffer2(1,k)=cornerlat2D(i,j) 
                latBuffer2(2,k)=cornerlat2D(i,j+1) 
                latBuffer2(3,k)=cornerlat2D(i+1,j+1) 
                latBuffer2(4,k)=cornerlat2D(i+1,j) 
                k=k+1
              enddo
           enddo

           deallocate(cornerlon2D, cornerlat2D, stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           ncStatus=nf90_inq_varid(ncid,"xc_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
           errmsg = "Variable xc_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yc_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer)
           errmsg = "Variable yc_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(latBuffer, lonBuffer, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

        
          ! Write xv_b, yv_b    
           ncStatus=nf90_inq_varid(ncid,"xv_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)
           errmsg = "Variable xv_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yv_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer2)
           errmsg = "Variable yv_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(lonBuffer2, latBuffer2, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

        else if (dstFileTypeLocal == ESMF_FILEFORMAT_ESMFMESH) then
           ncStatus=nf90_open(dstFile,NF90_NOWRITE,ncid1)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,errmsg,&
         rc)) return
           ! only write out xv_a and yv_a when the regrid method is conserve
           if (useDstCornerlocal .and. (methodlocal%regridmethod ==ESMF_REGRIDMETHOD_BILINEAR%regridmethod &
                .or. methodlocal%regridmethod ==ESMF_REGRIDMETHOD_PATCH%regridmethod &
                .or. methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_STOD%regridmethod &
                .or. methodlocal%regridmethod ==ESMF_REGRIDMETHOD_NEAREST_DTOS%regridmethod)) then
             ! check if centerCoords exit
             ncStatus=nf90_inq_varid(ncid1,"nodeCoords",VarId)
             varStr = "nodeCoords"
             dst_has_area = .false.
           else
             ncStatus=nf90_inq_varid(ncid1,"centerCoords",VarId)
             varStr = "centerCoords"
           endif
           if (ncStatus /= nf90_noerror) then
             write(*,*) "Warning: "//trim(varStr)// &
                " not present in dst grid file, so not outputting xc_b and yc_b to weight file."
             write(*,*)
           else
             allocate(latBuffer2(dstCoordDim, dstDim), stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

             allocate(latBuffer(dstDim), lonBuffer(dstDim), stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

             ncStatus=nf90_get_var(ncid1,VarId, latBuffer2)
             errmsg = "Variable "//varStr//" in "//trim(dstFile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
             do i=1,dstDim
                lonBuffer(i)=latBuffer2(1,i)
                latBuffer(i)=latBuffer2(2,i)
             enddo
             ncStatus=nf90_inq_varid(ncid,"xc_b",VarId)
             ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
             errmsg = "Variable xc_b in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return

             ncStatus=nf90_inq_varid(ncid,"yc_b",VarId)
             ncStatus=nf90_put_var(ncid,VarId, latBuffer)
             errmsg = "Variable yc_b in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
             deallocate(latBuffer, lonBuffer, latBuffer2, stat=memstat)
             if (ESMF_LogFoundDeallocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

           endif

           ! output xv_b and yv_b is harder, we have to read in the nodeCoords and
           ! elementConn and construct the the latitudes and longitudes for
           ! all the corner vertices
           if (.not. useDstCornerlocal .or. &
                methodlocal%regridmethod ==ESMF_REGRIDMETHOD_CONSERVE%regridmethod .or. &
                methodlocal%regridmethod ==ESMF_REGRIDMETHOD_CONSERVE_2ND%regridmethod) then
           if (dst_grid_corner > 0) then
           allocate(latBuffer2(dst_grid_corner,dstDim),lonBuffer2(dst_grid_corner,dstDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           call ESMF_EsmfGetVerts(ncid1, dstFile, dstDim, dst_grid_corner, dstNodeDim, &
                latBuffer2, lonBuffer2, status)
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

           ncStatus=nf90_inq_varid(ncid,"xv_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)
           errmsg = "Variable xv_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,errmsg,&
             rc)) return

           ncStatus=nf90_inq_varid(ncid,"yv_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, latBuffer2)
           errmsg = "Variable yv_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,errmsg,&
             rc)) return
           deallocate(latBuffer2, lonBuffer2, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           endif
           endif
           ! Write mask_b
           allocate(mask(dstDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           if (.not. useDstCornerlocal .or. &
                methodlocal%regridmethod ==ESMF_REGRIDMETHOD_CONSERVE%regridmethod .or. &
                methodlocal%regridmethod ==ESMF_REGRIDMETHOD_CONSERVE_2ND%regridmethod) then
             ncStatus=nf90_inq_varid(ncid1,"elementMask",VarId)
             if (ncStatus /= nf90_noerror) then
               write(*,*) "Warning: elementMask"// &
                " not present in dst grid file, so setting mask_b=1 in weight file."
               write(*,*)
               mask = 1
             else
               ncStatus=nf90_get_var(ncid1,VarId, mask)
               errmsg = "Variable elementMask in "//trim(dstFile)
               if (CDFCheckError (ncStatus, &
                 ESMF_METHOD, &
                 ESMF_SRCLINE,errmsg,&
                 rc)) return
             end if
           else
             ! make mask all 1
             mask=1
           endif
           ncStatus=nf90_inq_varid(ncid,"mask_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, mask)
           errmsg = "Variable mask_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,errmsg,&
             rc)) return
           deallocate(mask, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           if (dst_has_area .and. .not. present(dstArea)) then
             allocate(area(dstDim), stat=memstat)
             if (ESMF_LogFoundAllocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

             ncStatus=nf90_inq_varid(ncid1,"elementArea",VarId)
             errmsg = "Variable elementArea in "//trim(dstFile)
             if (CDFCheckError (ncStatus, &
                 ESMF_METHOD, &
                 ESMF_SRCLINE,errmsg,&
                 rc)) return
             ncStatus=nf90_get_var(ncid1,VarId, area)
             errmsg = "Variable elementArea in "//trim(dstFile)
             if (CDFCheckError (ncStatus, &
                 ESMF_METHOD, &
                 ESMF_SRCLINE,errmsg,&
                 rc)) return
             ncStatus=nf90_inq_varid(ncid,"area_b",VarId)
             ncStatus=nf90_put_var(ncid,VarId, area)
             errmsg = "Variable mask_a in "//trim(wgtfile)
             if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
             deallocate(area, stat=memstat)
             if (ESMF_LogFoundDeallocError(memstat,  &
                 ESMF_CONTEXT, rcToReturn=rc)) return

           endif
           ncStatus=nf90_close(ncid1)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,trim(dstFile),&
             rc)) return
        else if (dstFileTypeLocal == ESMF_FILEFORMAT_UGRID) then
           call ESMF_UGridInq(dstfile, dstmeshname, meshId=meshId, faceCoordFlag=faceCoordFlag)
           if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
           if (.not. useDstCornerlocal .or. &
                methodlocal%regridmethod ==ESMF_REGRIDMETHOD_CONSERVE%regridmethod .or. &
                methodlocal%regridmethod ==ESMF_REGRIDMETHOD_CONSERVE_2ND%regridmethod) then
             ! check if faceCoords exit
                allocate(latBuffer2(dst_grid_corner,dstDim),&
                        lonBuffer2(dst_grid_corner,dstDim), stat=memstat)
                if (ESMF_LogFoundAllocError(memstat,  &
                    ESMF_CONTEXT, rcToReturn=rc)) return

              if (faceCoordFlag) then
                allocate(latBuffer(dstDim), lonBuffer(dstDim), stat=memstat)
                if (ESMF_LogFoundAllocError(memstat,  &
                    ESMF_CONTEXT, rcToReturn=rc)) return

                call ESMF_UGridGetVar(dstfile, meshId, &
                   faceXcoords=lonBuffer, faceYcoords=latBuffer, &
                   faceNodeConnX=lonBuffer2, faceNodeConnY=latBuffer2, rc=status)
              else
                write(*,*) "Warning: face coordinates not present in dst grid file,"// &
                  " so not outputting xc_a and yc_a to weight file."
                write(*,*)
                call ESMF_UGridGetVar(dstfile, meshId, &
                   faceNodeConnX=lonBuffer2, faceNodeConnY=latBuffer2, rc=status)
              endif
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              if (faceCoordFlag) then
                ncStatus=nf90_inq_varid(ncid,"xc_b",VarId)
                ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
                errmsg = "Variable xc_a in "//trim(wgtfile)
                if (CDFCheckError (ncStatus, &
                  ESMF_METHOD, &
                  ESMF_SRCLINE,errmsg,&
                  rc)) return
                ncStatus=nf90_inq_varid(ncid,"yc_b",VarId)
                ncStatus=nf90_put_var(ncid,VarId, latBuffer)
                errmsg = "Variable yc_a in "//trim(wgtfile)
                if (CDFCheckError (ncStatus, &
                  ESMF_METHOD, &
                  ESMF_SRCLINE,errmsg,&
                rc)) return
                deallocate(latBuffer, lonBuffer, stat=memstat)
                if (ESMF_LogFoundDeallocError(memstat,  &
                    ESMF_CONTEXT, rcToReturn=rc)) return

              endif
              ncStatus=nf90_inq_varid(ncid,"xv_b",VarId)
              ncStatus=nf90_put_var(ncid,VarId, lonBuffer2)
              errmsg = "Variable xv_b in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return

              ncStatus=nf90_inq_varid(ncid,"yv_b",VarId)
              ncStatus=nf90_put_var(ncid,VarId, latBuffer2)
              errmsg = "Variable yv_b in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return
              deallocate(latBuffer2, lonBuffer2, stat=memstat)
              if (ESMF_LogFoundDeallocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

           else
              allocate(latBuffer(dstDim), lonBuffer(dstDim), stat=memstat)
              if (ESMF_LogFoundAllocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

              call ESMF_UGridGetVar(dstfile, meshId, &
                   nodeXcoords=lonBuffer, nodeYcoords=latBuffer, rc=rc)
              ncStatus=nf90_inq_varid(ncid,"xc_b",VarId)
              ncStatus=nf90_put_var(ncid,VarId, lonBuffer)
              errmsg = "Variable xc_a in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return
              ncStatus=nf90_inq_varid(ncid,"yc_b",VarId)
              ncStatus=nf90_put_var(ncid,VarId, latBuffer)
              errmsg = "Variable yc_a in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
              rc)) return
              deallocate(latBuffer, lonBuffer, stat=memstat)
              if (ESMF_LogFoundDeallocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

          endif

           ! Write out mask
           allocate(mask(dstDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           mask(:)=1
           if (present(dstMissingValue)) then
             if (dstMissingValue) then
              allocate(varBuffer1D(dstDim), stat=memstat)
              if (ESMF_LogFoundAllocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

              call ESMF_UgridGetVarByName(dstFile, dstvarname, &
                                varBuffer1D, missingvalue = missing_value, &
                                rc=status)
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              do j=1,size(varBuffer1D)
                 if (varBuffer1D(j) == missing_value) mask(j)=0
              enddo
              deallocate(varBuffer1D, stat=memstat)
              if (ESMF_LogFoundDeallocError(memstat,  &
                  ESMF_CONTEXT, rcToReturn=rc)) return

             endif
            endif

           ncStatus=nf90_inq_varid(ncid,"mask_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, mask)
           errmsg = "Variable mask_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
             ESMF_METHOD, &
             ESMF_SRCLINE,&
             errmsg,&
             rc)) return
           deallocate(mask, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

        else if (dstFileTypeLocal == ESMF_FILEFORMAT_MOSAIC) then
           !read coordinates from the tile files
           allocate(latBuffer2(dstmosaic%nx, dstmosaic%ny), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           allocate(lonBuffer2(dstmosaic%nx, dstmosaic%ny), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           totalsize = dstmosaic%nx*dstmosaic%ny
           allocate(varBuffer1D(totalsize), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           ncStatus=nf90_inq_varid(ncid,"xc_b",VarId1)
           ncStatus=nf90_inq_varid(ncid,"yc_b",VarId2)
           errmsg = "Variable xc_b or yc_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,&
              errmsg,&
           rc)) return
           do k=1,dstmosaic%ntiles
             totallen = len_trim(dstmosaic%filenames(k))+len_trim(dstmosaic%tileDirectory)
             tempname = trim(dstmosaic%tileDirectory)//trim(dstmosaic%filenames(k))
             call ESMF_GridspecReadTile(trim(tempname),dstmosaic%nx, dstmosaic%ny, &
                  centerlon=lonBuffer2, centerlat=latBuffer2, rc=status)
             if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
             varBuffer1D=reshape(lonBuffer2,(/totalsize/))
             start=(k-1)*totalsize+1
             ncStatus = nf90_put_var(ncid, VarId1, varBuffer1D, (/start/), (/totalsize/))
             if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,&
                "writing xc_b in weight file", &
             rc)) return
             varBuffer1D=reshape(latBuffer2,(/totalsize/))
             ncStatus = nf90_put_var(ncid, VarId2,varBuffer1D, (/start/), (/totalsize/))
             if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,&
                "writing yc_b in weight file", &
             rc)) return
          enddo
          deallocate(lonBuffer2, latBuffer2, varBuffer1D, stat=memstat)
          if (ESMF_LogFoundDeallocError(memstat,  &
              ESMF_CONTEXT, rcToReturn=rc)) return

        endif

         ! Write area_a
         ncStatus=nf90_inq_varid(ncid,"area_a",VarId)
         if (present(srcArea)) then
           ncStatus=nf90_put_var(ncid,VarId, srcArea)
           errmsg = "Variable area_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
         else if (.not. src_has_area) then
          ! Just set these to 0.0, because not provided
           allocate(area(srcDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           area=0.0
           ncStatus=nf90_put_var(ncid,VarId, area)
           errmsg = "Variable area_a in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
           deallocate(area, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return


         endif

         ! Write area_b
         ncStatus=nf90_inq_varid(ncid,"area_b",VarId)
         if (present(dstArea)) then
           ncStatus=nf90_put_var(ncid,VarId, dstArea)
           errmsg = "Variable area_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
         else if (.not. dst_has_area) then
          ! Just set these to 0.0, because not provided
           allocate(area(dstDim), stat=memstat)
           if (ESMF_LogFoundAllocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

           area=0.0
           ncStatus=nf90_put_var(ncid,VarId, area)
           errmsg = "Variable area_b in "//trim(wgtfile)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
           deallocate(area, stat=memstat)
           if (ESMF_LogFoundDeallocError(memstat,  &
               ESMF_CONTEXT, rcToReturn=rc)) return

         endif

         ! Write frac_a
         ncStatus=nf90_inq_varid(ncid,"frac_a",VarId)
         if (present(srcFrac)) then
            ncStatus=nf90_put_var(ncid,VarId, srcFrac)
         else
            allocate(frac(srcDim), stat=memstat)
            if (ESMF_LogFoundAllocError(memstat,  &
                ESMF_CONTEXT, rcToReturn=rc)) return

            frac=0.0
            ncStatus=nf90_put_var(ncid,VarId, frac)
            deallocate(frac, stat=memstat)
            if (ESMF_LogFoundDeallocError(memstat,  &
                ESMF_CONTEXT, rcToReturn=rc)) return

         endif
         errmsg = "Variable frac_a in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return

         ! Write frac_b
         ncStatus=nf90_inq_varid(ncid,"frac_b",VarId)
         if (present(dstFrac)) then
            ncStatus=nf90_put_var(ncid,VarId, dstFrac)
         else
            allocate(frac(dstDim), stat=memstat)
            if (ESMF_LogFoundAllocError(memstat,  &
                ESMF_CONTEXT, rcToReturn=rc)) return

            frac=1.0
            ncStatus=nf90_put_var(ncid,VarId, frac)
            deallocate(frac, stat=memstat)
            if (ESMF_LogFoundDeallocError(memstat,  &
                ESMF_CONTEXT, rcToReturn=rc)) return

         endif
         errmsg = "Variable frac_b in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return
         deallocate(src_grid_dims, dst_grid_dims, stat=memstat)
         if (ESMF_LogFoundAllocError(memstat,  &
             ESMF_CONTEXT, rcToReturn=rc)) return

    endif  ! PetNo==0
    ! Block all other PETs until the NetCDF file has been created
    call ESMF_VMBarrier(vm)

    ! find the max of allCounts(i) and allocate colrow
    maxcount=0
    do i=1,PetCnt
        if (allCounts(i) > maxcount) maxcount = allCounts(i)
    enddo

    if (PetNo == 0) then
        ! First write out its own weight and indices, then receive the data from
        ! other PETs and write them out
        start = 1
        ! allocate indexbuf and weightbuf to receive data from other PETs
        allocate(indexbuf(maxcount*2), weightbuf(maxcount), stat=memstat)
        if (ESMF_LogFoundAllocError(memstat,  &
            ESMF_CONTEXT, rcToReturn=rc)) return

        do i=1, PetCnt
          ! write the local factorList and factorIndexList first
          localCount(1)=allCounts(i)
          if (i==1) then
            !do j=1,localCount(1)
            !    indexbuf(j) = factorIndexList(1,j)
            !enddo
            next => factorIndexList(1,:)
            ncStatus=nf90_inq_varid(ncid,"col",VarId)
            ncStatus=nf90_put_var(ncid,VarId, next,(/start/),localCount)
            errmsg = "Variable col in "//trim(wgtfile)
            if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,errmsg,&
              rc)) return
            !do j=1,localCount(1)
            !  indexbuf(j) = factorIndexList(2,j)
            !enddo
            next => factorIndexList(2,:)
            ncStatus=nf90_inq_varid(ncid,"row",VarId)
            ncStatus=nf90_put_var(ncid,VarId, next,(/start/),localCount)
            errmsg = "Variable row in "//trim(wgtfile)
            if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,errmsg,&
              rc)) return

            ncStatus=nf90_inq_varid(ncid,"S",VarId)
            ncStatus=nf90_put_var(ncid,VarId, factorList, (/start/),localCount)
            errmsg = "Variable S in "//trim(wgtfile)
            if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,errmsg,&
              rc)) return
          else
            if (localCount(1) > 0) then
              ! receive the factorList and factorIndexList
              call ESMF_VMRecv(vm, indexbuf, localCount(1)*2, i-1, rc=status)
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              call ESMF_VMRecv(vm, weightbuf, localCount(1), i-1, rc=status)
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return

              ncStatus=nf90_inq_varid(ncid,"col",VarId)
                      ncStatus=nf90_put_var(ncid,VarId, indexbuf,(/start/),localCount)
              errmsg = "Variable col in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return
              next => indexbuf(localCount(1)+1:localCount(1)*2)
              ncStatus=nf90_inq_varid(ncid,"row",VarId)
              ncStatus=nf90_put_var(ncid,VarId, next ,(/start/),localCount)
              errmsg = "Variable row in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return

              ncStatus=nf90_inq_varid(ncid,"S",VarId)
              ncStatus=nf90_put_var(ncid,VarId, weightbuf, (/start/),localCount)
              errmsg = "Variable S in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return
            end if
          end if
          start = start + localCount(1)
       end do
    else
       allocate(indexbuf(localcount(1)*2), stat=memstat)
       if (ESMF_LogFoundAllocError(memstat,  &
           ESMF_CONTEXT, rcToReturn=rc)) return

       if (localcount(1) > 0) then
         do j=1,localCount(1)
             indexbuf(j) = factorIndexList(1,j)
             indexbuf(j+localCount(1)) = factorIndexList(2,j)
         enddo
         ! a non-root PET, send the results to PET 0
         call ESMF_VMSend(vm, indexbuf, localCount(1)*2, 0, rc=status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
         call ESMF_VMSend(vm, factorList, localCount(1), 0, rc=status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
       end if
    end if

    deallocate(allCounts, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat,  &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if (PetNo == 0) then
       ncStatus = nf90_close(ncid)
       if (CDFCheckError (ncStatus, &
         ESMF_METHOD, &
         ESMF_SRCLINE, trim(wgtfile),&
         rc)) return
       deallocate(weightbuf, stat=memstat)
       if (ESMF_LogFoundDeallocError(memstat,  &
           ESMF_CONTEXT, rcToReturn=rc)) return
    end if
    call ESMF_VMBarrier(vm)

    deallocate(indexbuf, stat=memstat)
    if (ESMF_LogFoundDeallocError(memstat,  &
         ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
    return
#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end subroutine ESMF_OutputScripWeightFile

#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_OutputSimpleWeightFile"
!BOPI
! !ROUTINE: ESMF_OUtputSimpleWeightFile
! output the weight and indices tables into a NetCDF format file
!

! !INTERFACE:
subroutine ESMF_OutputSimpleWeightFile (wgtFile, factorList, factorIndexList, &
                                      title, method, &
                                      largeFileFlag, netcdf4FileFlag, &
                                      rc)
!
! !ARGUMENTS:
      character(len=*), intent(in) :: wgtFile
      real(ESMF_KIND_R8) , intent(in) :: factorList(:)
      integer(ESMF_KIND_I4) , intent(in), target :: factorIndexList(:,:)
      character(len=*), optional, intent(in) :: title
      type(ESMF_RegridMethod_Flag), optional, intent(in) :: method
      logical, optional, intent(in) :: largeFileFlag, netcdf4FileFlag
      integer, optional :: rc

      type(ESMF_VM):: vm
      integer :: PetNo, PetCnt
      type(ESMF_Logical) :: largeFileFlaglocal
      type(ESMF_Logical) :: netcdf4FileFlaglocal
      type(ESMF_RegridMethod_Flag) :: methodlocal
      character(len=256) :: titlelocal, map_method
      character(len=256) :: esmf_regrid_method,conventions
      integer :: total, localCount(1)
      integer(ESMF_KIND_I4), pointer:: allCounts(:)
      integer(ESMF_KIND_I4), pointer:: indexbuf(:), next(:)
      integer :: ncid, ncid1
      integer :: ncStatus
      integer :: status
      integer :: maxcount
      integer :: i,j, k, start
      integer :: nsDimId, VarId
      real(ESMF_KIND_R8), pointer   :: weightbuf(:)
      character(len=256) :: errmsg
      integer :: memstat

#ifdef ESMF_NETCDF

      call ESMF_VMGetCurrent(vm, rc=rc)
      if (rc /= ESMF_SUCCESS) return

      ! set up local pet info
      call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
      if (rc /= ESMF_SUCCESS) return

      if (present(largeFileFlag)) then
        largeFileFlaglocal = largeFileFlag
      else
        largeFileFlaglocal = .false.
      endif

      if (present(netcdf4FileFlag)) then
        netcdf4FileFlaglocal = netcdf4FileFlag
      else
        netcdf4FileFlaglocal = .false.
      endif

      localCount(1)=size(factorList,1)
      allocate(allCounts(PetCnt), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat,  &
          ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_VMAllGather(vm,localCount,allCounts,1,rc=status)
      if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

      ! calculate the size of the global weight table
      total = 0
      do i=1,PetCnt
         total=allCounts(i)+total
      end do
      !print *, PetNo, 'local count ', localCount(1), AllCounts(PetNo+1), total

      if (PetNo == 0) then
         ! Create output file and create dimensions and variables
         call c_nc_create(wgtFile, NF90_CLOBBER, &
                largeFileFlaglocal, netcdf4FileFlaglocal, ncid, status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

         ! global variables
          if (present(title)) then
             titlelocal = trim(title)
          else
             titlelocal = "Undefined"
          endif

         ! Regrid method
         if (present(method)) then
           methodlocal = method
           if (methodlocal%regridmethod == ESMF_REGRIDMETHOD_BILINEAR%regridmethod) then
              map_method = "Bilinear remapping"
              esmf_regrid_method = "Bilinear"
           elseif (methodlocal%regridmethod == ESMF_REGRIDMETHOD_PATCH%regridmethod) then
              !scrip_test does not recognize patch remapping
              map_method = "Bilinear remapping"
              esmf_regrid_method = "Higher-order Patch"
           elseif (methodlocal%regridmethod == ESMF_REGRIDMETHOD_CONSERVE%regridmethod) then
              map_method = "Conservative remapping"
              esmf_regrid_method = "First-order Conservative"
           elseif (methodlocal%regridmethod == ESMF_REGRIDMETHOD_CONSERVE_2ND%regridmethod) then
              map_method = "Conservative remapping"
              esmf_regrid_method = "Second-order Conservative"
           elseif (methodlocal%regridmethod == ESMF_REGRIDMETHOD_NEAREST_STOD%regridmethod) then
              map_method = "Bilinear remapping"
              esmf_regrid_method = "Nearest source to destination"
           elseif (methodlocal%regridmethod == ESMF_REGRIDMETHOD_NEAREST_DTOS%regridmethod) then
              map_method = "Bilinear remapping"
              esmf_regrid_method = "Nearest destination to source"
           else
              !report error
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                  msg="- regridmethod not recongized", &
                  ESMF_CONTEXT, rcToReturn=rc)
              return
           endif
         else
           map_method = "Undefined"
           esmf_regrid_method = "Undefined"
         endif

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "title", trim(titlelocal))
         errmsg = "Attribute title in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "map_method", trim(map_method))
         errmsg = "Attribute map_method in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "ESMF_regrid_method", &
                        trim(esmf_regrid_method))
         errmsg = "Attribute esmf_regrid_method in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus = nf90_def_dim(ncid,"n_s",total, nsDimId)
         errmsg = "Dimension n_s in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! col: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"col",NF90_INT, (/nsDimId/),  VarId)
         errmsg = "Variable col in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! row: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"row",NF90_INT, (/nsDimId/),  VarId)
         errmsg = "Variable row in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

        ! S: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"S",NF90_DOUBLE, (/nsDimId/),  VarId)
         errmsg = "Variable S in "//trim(wgtfile)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

         ncStatus=nf90_enddef(ncid)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           trim(wgtFile),&
           rc)) return
      endif

      ! find the max of allCounts(i) and allocate colrow
      maxcount=0
      do i=1,PetCnt
        if (allCounts(i) > maxcount) maxcount = allCounts(i)
      enddo

      call ESMF_VMBarrier(vm)

     if (PetNo == 0) then
        ! First write out its own weight and indices, then receive the data from
        ! other PETs and write them out
        start = 1
        ! allocate indexbuf and weightbuf to receive data from other PETs
        allocate(indexbuf(maxcount*2), weightbuf(maxcount), stat=memstat)
        if (ESMF_LogFoundAllocError(memstat,  &
            ESMF_CONTEXT, rcToReturn=rc)) return

        do i=1, PetCnt
          ! write the local factorList and factorIndexList first
          localCount(1)=allCounts(i)
          if (i==1) then
            !do j=1,localCount(1)
            !    indexbuf(j) = factorIndexList(1,j)
            !enddo
            next => factorIndexList(1,:)
            ncStatus=nf90_inq_varid(ncid,"col",VarId)
            ncStatus=nf90_put_var(ncid,VarId, next,(/start/),localCount)
            errmsg = "Variable col in "//trim(wgtfile)
            if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,errmsg,&
              rc)) return
            !do j=1,localCount(1)
            !  indexbuf(j) = factorIndexList(2,j)
            !enddo
            next => factorIndexList(2,:)
            ncStatus=nf90_inq_varid(ncid,"row",VarId)
            ncStatus=nf90_put_var(ncid,VarId, next,(/start/),localCount)
            errmsg = "Variable row in "//trim(wgtfile)
            if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,errmsg,&
              rc)) return

            ncStatus=nf90_inq_varid(ncid,"S",VarId)
            ncStatus=nf90_put_var(ncid,VarId, factorList, (/start/),localCount)
            errmsg = "Variable S in "//trim(wgtfile)
            if (CDFCheckError (ncStatus, &
              ESMF_METHOD, &
              ESMF_SRCLINE,errmsg,&
              rc)) return
          else
            if (localCount(1) > 0) then
              ! receive the factorList and factorIndexList
              call ESMF_VMRecv(vm, indexbuf, localCount(1)*2, i-1, rc=status)
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
              call ESMF_VMRecv(vm, weightbuf, localCount(1), i-1, rc=status)
              if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return

              ncStatus=nf90_inq_varid(ncid,"col",VarId)
                      ncStatus=nf90_put_var(ncid,VarId, indexbuf,(/start/),localCount)
              errmsg = "Variable col in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return
              next => indexbuf(localCount(1)+1:localCount(1)*2)
              ncStatus=nf90_inq_varid(ncid,"row",VarId)
              ncStatus=nf90_put_var(ncid,VarId, next ,(/start/),localCount)
              errmsg = "Variable row in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return

              ncStatus=nf90_inq_varid(ncid,"S",VarId)
              ncStatus=nf90_put_var(ncid,VarId, weightbuf, (/start/),localCount)
              errmsg = "Variable S in "//trim(wgtfile)
              if (CDFCheckError (ncStatus, &
                ESMF_METHOD, &
                ESMF_SRCLINE,errmsg,&
                rc)) return
            end if
          end if
          start = start + localCount(1)
       end do
     else
       allocate(indexbuf(localcount(1)*2), stat=memstat)
       if (ESMF_LogFoundAllocError(memstat,  &
           ESMF_CONTEXT, rcToReturn=rc)) return

       if (localcount(1) > 0) then
         do j=1,localCount(1)
             indexbuf(j) = factorIndexList(1,j)
             indexbuf(j+localCount(1)) = factorIndexList(2,j)
         enddo
         ! a non-root PET, send the results to PET 0
         call ESMF_VMSend(vm, indexbuf, localCount(1)*2, 0, rc=status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
         call ESMF_VMSend(vm, factorList, localCount(1), 0, rc=status)
         if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
       end if
     end if

     deallocate(allCounts, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat,  &
          ESMF_CONTEXT, rcToReturn=rc)) return

     if (PetNo == 0) then
       ncStatus = nf90_close(ncid)
       if (CDFCheckError (ncStatus, &
         ESMF_METHOD, &
         ESMF_SRCLINE, trim(wgtfile),&
         rc)) return
       deallocate(weightbuf, stat=memstat)
       if (ESMF_LogFoundDeallocError(memstat,  &
           ESMF_CONTEXT, rcToReturn=rc)) return
     end if
     call ESMF_VMBarrier(vm)

     deallocate(indexbuf, stat=memstat)
     if (ESMF_LogFoundAllocError(memstat,  &
         ESMF_CONTEXT, rcToReturn=rc)) return

     if (present(rc)) rc = ESMF_SUCCESS
     return
#else
     if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end subroutine ESMF_OutputSimpleWeightFile


#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_OutputScripVarFile"
!
subroutine ESMF_OutputScripVarFile(filename, varname, varbuffer, rc)

    character(len=*), intent(in)   :: filename
    character(len=*), intent(in)   :: varname
    real(ESMF_KIND_R8), pointer    :: varbuffer(:)
    integer                       :: rc

    integer :: ncid, dimid, varid
    integer :: localrc, ncStatus
    integer :: varsize
    character(len=256) :: errmsg

#ifdef ESMF_NETCDF

    ncStatus = nf90_open (path=trim(filename), mode=nf90_write, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    ! define a new varilable with dimension 'n_b'
    ncStatus = nf90_inq_dimid(ncid, 'n_b', dimid)
    errmsg = "Dimension n_b in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

   ncStatus = nf90_inquire_dimension (ncid, dimid, len=varsize)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

   if (size(varbuffer,1) /= varsize) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
           msg="- the variable size is inconsistent with the dest grid dimension", &
           ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
                
    ncStatus = nf90_redef(ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    ncStatus = nf90_def_var(ncid, varname, NF90_DOUBLE, (/dimid/), varid)
    errmsg = "Variable src_grid_dims in "//trim(filename)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return

    ncStatus = nf90_enddef(ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    ncStatus = nf90_put_var(ncid, varid, varbuffer)
    errmsg = "Variable "//trim(varname)//" in "//trim(filename)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,&
           errmsg,&
           rc)) return
    ncStatus = nf90_close(ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, &
      trim(filename), &
      rc)) return
    rc=ESMF_SUCCESS
    return
#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end subroutine ESMF_OutputScripVarFile

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_EsmfInq"
!BOPI
! !ROUTINE: ESMF_EsmfInq: Return the dimension information
!  information from a ESMF Unstructured grid file
!
! !INTERFACE:
subroutine ESMF_EsmfInq(filename, nodeCount, elementCount, &
                        maxNodePElement, coordDim,  &
                        haveNodeMask, haveElmtMask, haveArea, &
                        haveOrigGridDims, origGridDims, rc)

! !ARGUMENTS:

    character(len=*), intent(in)   :: filename
    integer, intent(out), optional :: nodeCount
    integer, intent(out), optional :: elementCount
    integer, intent(out), optional :: maxNodePElement
    integer, intent(out), optional :: coordDim
    logical, intent(out), optional :: haveNodeMask
    logical, intent(out), optional :: haveElmtMask
    logical, intent(out), optional :: haveArea
    logical, intent(out), optional :: haveOrigGridDims
    integer, intent(out), optional :: origGridDims(2)
    integer, intent(out), optional :: rc

    integer:: localrc, ncStatus
    integer :: DimId, VarId
    integer :: ncid, local_rank
    character(len=256):: errmsg
    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    if (present(rc)) rc=ESMF_SUCCESS
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE, trim(filename), rc)) return

    ! get number of elements
    if (present(nodeCount)) then
      ncStatus = nf90_inq_dimid (ncid, "nodeCount", DimId)
      errmsg = "Dimension nodeCount in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=nodeCount)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return
    end if

    ! get number of elements
    if (present(elementCount)) then
      ncStatus = nf90_inq_dimid (ncid, "elementCount", DimId)
      errmsg = "Dimension elementCount in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=elementCount)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return
    end if

    ! get number of elements
    if (present(maxNodePElement)) then
      ncStatus = nf90_inq_dimid (ncid, "maxNodePElement", DimId)
      errmsg = "Dimension maxNodePElement in "//trim(filename)
      if (ncStatus /= nf90_noerror) then
         maxNodePElement = -1
      else
         ncStatus = nf90_inquire_dimension (ncid, DimId, len=maxNodePElement)
         if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return
      end if
    end if

    ! get number of elements
    if (present(coordDim)) then
      ncStatus = nf90_inq_dimid (ncid, "coordDim", DimId)
      errmsg = "Dimension coordDim in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

      ncStatus = nf90_inquire_dimension (ncid, DimId, len=coordDim)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return
    end if

    ! check if elementMask exit
    if (present(haveElmtMask)) then
      ncStatus = nf90_inq_varid (ncid, "elementMask", VarId)
      if (ncStatus == nf90_noerror) then
           haveElmtMask = .true.
      else
           haveElmtMask = .false.
      end if
    end if

    ! check if nodeMask exit
    if (present(haveNodeMask)) then
      ncStatus = nf90_inq_varid (ncid, "nodeMask", VarId)
      if (ncStatus == nf90_noerror) then
           haveNodeMask = .true.
      else
           haveNodeMask = .false.
      end if
    end if

    ! check if elementArea exit
    if (present(haveArea)) then
      ncStatus = nf90_inq_varid (ncid, "elementArea", VarId)
      if (ncStatus == nf90_noerror) then
           haveArea = .true.
      else
           haveArea = .false.
      end if
    end if

    ! check if haveOrigGridDims
    if (present(haveOrigGridDims)) then
      ncStatus = nf90_inq_varid (ncid, "origGridDims", VarId)
      if (ncStatus == nf90_noerror) then
           haveOrigGridDims = .true.
      else
           haveOrigGridDims = .false.
      end if
    end if

! XMRKX !
    ! Get origGridDims
    if (present(origGridDims)) then
      ncStatus = nf90_inq_varid (ncid, "origGridDims", VarId)
      errmsg = "Variable origGridDims in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

      ncStatus=nf90_get_var(ncid, VarId, origGridDims)
      if (CDFCheckError (ncStatus, &
           ESMF_METHOD, &
           ESMF_SRCLINE,errmsg,&
           rc)) return
    end if

    if (present(rc)) rc=ESMF_SUCCESS
    return
#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end subroutine ESMF_EsmfInq

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_EsmfInqUnits"
!BOPI
! !ROUTINE: ESMF_EsmfInqUnits: Return the units attribute for
! coordinate variables, assuming all the coordinate variables
! have the same units
!
! !INTERFACE:
subroutine ESMF_EsmfInqUnits(filename, units, rc)

! !ARGUMENTS:

    character(len=*), intent(in)   :: filename
    character(len=*), intent(out)   :: units
    integer, intent(out), optional :: rc

    integer:: localrc, ncStatus
    integer :: ncid, VarId, len
    character(len=80) :: buffer
    character(len=256) :: errmsg
    integer, parameter :: nf90_noerror = 0
    integer :: coordDim,DimID

#ifdef ESMF_NETCDF
    if (present(rc)) rc=ESMF_SUCCESS
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE, trim(filename),rc)) return

    ! Get vertex dimension
    ncStatus = nf90_inq_dimid (ncid, "coordDim", DimId)
    errmsg = "Dimension coordDim in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=coordDim)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get units
    ncStatus = nf90_inq_varid (ncid, "nodeCoords", VarId)
    errmsg = "Variable nodeCoords in "//trim(filename)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

    ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
    ! only require units for 2D
    if (coordDim==2) then
       if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,errmsg,&
            rc)) return
    else
       ! if no units are present set to default and leave
       if (ncStatus /= nf90_noerror) then
          units="UNKNOWN"
          if (present(rc)) rc=ESMF_SUCCESS
          return
       endif
    endif

    ncStatus = nf90_get_att(ncid, VarId, "units", buffer)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD, &
        ESMF_SRCLINE,errmsg,&
        rc)) return

    if (buffer(len:len) .eq. achar(0)) len = len-1
    units = ESMF_UtilStringLowerCase(buffer(1:len))
    if (present(rc)) rc=ESMF_SUCCESS
    return
#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end subroutine ESMF_EsmfInqUnits

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_EsmfGetNode"
subroutine ESMF_EsmfGetNode (filename, nodeCoords, nodeMask, &
                            convertToDeg, coordSys, rc)

    character(len=*), intent(in)   :: filename
    real(ESMF_KIND_R8), pointer    :: nodeCoords (:,:)
    integer(ESMF_KIND_I4), pointer, optional :: nodeMask (:)
    logical, intent(in), optional  :: convertToDeg
    type(ESMF_CoordSys_Flag), optional :: coordSys
    integer, intent(out), optional :: rc

    type(ESMF_VM) :: vm
    integer :: PetNo, PetCnt

    integer :: ncid
    integer :: ncStatus
    integer :: RecCnt (2)

    integer :: DimId
    integer :: nodeCnt, ElmtCount, MaxNodePerElmt, NodeDim
    integer :: localCount, remain

    integer :: VarNo
    character(len=256)::errmsg
    character(len=80) :: units
    integer :: len
    logical :: convertToDegLocal
    type(ESMF_CoordSys_Flag) :: coordSysLocal
    integer :: memstat

#ifdef ESMF_NETCDF
    coordSysLocal = ESMF_COORDSYS_SPH_DEG
    convertToDegLocal = .false.
    if (present(convertToDeg)) convertToDegLocal = convertToDeg

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    ! get number of vertices
    ncStatus = nf90_inq_dimid (ncid, "nodeCount", DimId)
    errmsg = "Dimension nodeCount in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=nodeCnt)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get vertex dimension
    ncStatus = nf90_inq_dimid (ncid, "coordDim", DimId)
    errmsg = "Dimension coordDim in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=NodeDim)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! allocate memory for verticies
    allocate (nodeCoords (NodeDim, nodeCnt), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat,  &
        ESMF_CONTEXT, rcToReturn=rc)) return


    RecCnt(:) = ubound(nodeCoords)
    !print *, "nodeCoords:",nodeCnt, NodeDim

    ! read vertex data
    ncStatus = nf90_inq_varid (ncid, "nodeCoords", VarNo)
    errmsg = "Variable nodeCoords in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_get_var (ncid, VarNo, nodeCoords, start=(/1,1/), count=(/NodeDim, nodeCnt/))
    errmsg = "Variable nodeCoords in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Check units, and set the coordSys output, if nodeDim==3, normalize the height
    ! field
    !if (nodeDim==2) then
       ncStatus = nf90_inquire_attribute(ncid, VarNo, "units", len=len)
       if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,errmsg,&
            rc)) return

       ncStatus = nf90_get_att(ncid, VarNo, "units", units)
       if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,errmsg,&
            rc)) return
       ! if len != 7, something is wrong, check the value.  If it starts
       ! with Degres/degrees/Radians/radians, ignore the garbage after the
       ! word.  Otherwise, return the whole thing
       if (units(len:len) .eq. achar(0)) len = len-1
       units = ESMF_UtilStringLowerCase(units(1:len))
       ! if the units is meters, kilometers, or km, make it Cartisian 2D
       if (units(1:len) .eq. "meters" .or. units(1:len) .eq. "m" .or. &
           units(1:len) .eq. "km" .or. units(1:len) .eq. "kilometers") then     
           coordSysLocal = ESMF_COORDSYS_CART
       elseif (units(1:len) .eq. 'radians' .and. .not. convertToDegLocal) then
            coordSysLocal = ESMF_COORDSYS_SPH_RAD
       elseif (units(1:len) .ne. 'degrees' .and. &
            units(1:len) .ne. 'radians') then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- units attribute is not degrees or radians", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
       endif

       ! if units is "radians", convert it to degree
       ! if nodeDim=3, only convert the first two dimensions, leave the 3rd dim unchanged
       if (convertToDegLocal) then
          if (units(1:len) .eq. "radians") then
             nodeCoords(1:2,:) = &
                 nodeCoords(1:2,:)*ESMF_COORDSYS_RAD2DEG
          endif
       endif

    !endif

    ! get nodeMask
    if (present(nodeMask)) then
       allocate(nodeMask(nodeCnt), stat=memstat)
       if (ESMF_LogFoundAllocError(memstat,  &
           ESMF_CONTEXT, rcToReturn=rc)) return

       ncStatus = nf90_inq_varid (ncid, "nodeMask", VarNo)
       errmsg = "Variable nodeMask in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return

       ncStatus = nf90_get_var (ncid, VarNo, nodeMask, start=(/1/), count=(/nodeCnt/))
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
    endif

    ncStatus = nf90_close (ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    if (present(coordSys)) then
       coordSys = coordSysLocal
    endif
#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end subroutine ESMF_EsmfGetNode

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_EsmfGetElement"
subroutine ESMF_EsmfGetElement (filename, elementConn, &
                                 elmtNums, startElmt, elementMask, &
                                 elementArea, centerCoords, convertToDeg, rc)

    character(len=*), intent(in)   :: filename
    integer(ESMF_KIND_I4), pointer :: elementConn (:)
    integer(ESMF_KIND_I4), pointer :: elmtNums (:)
#if defined (ESMF_NO_INTEGER_1_BYTE)
    ! TODO: Eventually use F2008 kind 'int8'.
    integer(selected_int_kind(1)), allocatable :: elmtNums_i1(:)
#else
    integer(ESMF_KIND_I1), allocatable :: elmtNums_i1(:)
#endif
    integer,           intent(out) :: startElmt
    integer(ESMF_KIND_I4), pointer, optional :: elementMask (:)
    real(ESMF_KIND_R8), pointer, optional :: elementArea (:)
    real(ESMF_KIND_R8), pointer, optional :: centerCoords (:,:)
    logical, intent(in), optional  :: convertToDeg
    integer, intent(out), optional :: rc

    type(ESMF_VM) :: vm
    integer :: PetNo, PetCnt

    integer :: ncid
    integer :: ncStatus, status
    integer :: RecCnt (2)

    integer :: DimId
    integer :: nodeCnt, ElmtCount, MaxNodePerElmt, coordDim
    integer :: localCount, remain

    integer :: VarNo, VarType
    character(len=256)::errmsg
    character(len=80) :: units
    integer :: len
    logical :: convertToDegLocal
    integer, parameter :: nf90_noerror = 0
    integer :: localPolyBreakValue, startIndex, i, j, k
    logical :: PolyBreakFound
    logical :: isRaggedArray
    integer, allocatable :: elementConnLocal(:,:)
    integer :: totalConn, startConn
    integer :: senddata(1)
    integer, allocatable :: recvdata(:)
    integer :: memstat

#ifdef ESMF_NETCDF
     convertToDegLocal = .false.
     isRaggedArray = .false.
    if (present(convertToDeg)) convertToDegLocal = convertToDeg

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ncStatus = nf90_open (path=trim(filename), mode=NF90_SHARE, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    ! get number of elmts
    ncStatus = nf90_inq_dimid (ncid, "elementCount", DimId)
    errmsg = "Dimension elementCount in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=ElmtCount)
     if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get max_verts_per_elmt
    ncStatus = nf90_inq_dimid (ncid, "maxNodePElement", DimId)
    errmsg = "Dimension maxNodePElement in "//trim(filename)
    ! if not exist, check if connectionCount dimension exists, if so it is ragged array
    if (ncStatus /= nf90_noerror) then
        ncStatus = nf90_inq_dimid (ncid, "connectionCount", DimId)
        errmsg = "Either maxNodePElement or connectionCount does not exist"
        if (CDFCheckError (ncStatus, &
           ESMF_METHOD,  &
           ESMF_SRCLINE, errmsg, &
           rc)) return
        isRaggedArray = .true.
    else
      ncStatus = nf90_inquire_dimension (ncid, DimId, len=MaxNodePerElmt)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
    endif

    ! Decompose the elmt array evenly on all the PEs
    localcount = elmtCount/PetCnt
    remain = mod(elmtCount,PetCnt)
    startElmt = localcount * PetNo+1
    if (PetNo == (PetCnt-1)) localcount = localcount+remain

    ! allocate memory for elmts
    allocate (elmtNums (localcount), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat,  &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! read num_elmt_verts
    ncStatus = nf90_inq_varid (ncid, "numElementConn", VarNo)
    errmsg = "Variable numElementConn in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_inquire_variable (ncid, VarNo, xtype=VarType)
    errmsg = "Variable numElementConn type inquiry in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Even though NetCDF would automatically do data conversion, special casing NF90_BYTE
    ! can mitigate memory issues when reading very large arrays.  In particular, the
    ! Intel compiler can place large temporaries on the stack, rather than heap, causing
    ! problems.  (See ticket 3614272.)
    select case (VarType)
    case (NF90_INT)
      ncStatus = nf90_get_var (ncid, VarNo, elmtNums, start=(/startElmt/), count=(/localcount/))
      errmsg = "Reading numElementConn from int variable in " // trim (filename)
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return

    case (NF90_BYTE)
      allocate (elmtNums_i1(localcount), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat,  &
          ESMF_CONTEXT, rcToReturn=rc)) return

      ncStatus = nf90_get_var (ncid, VarNo, elmtNums_i1, start=(/startElmt/), count=(/localcount/))
      errmsg = "Reading numElementConn from byte variable in " // trim (filename)
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return

      elmtNums = elmtNums_i1
      deallocate (elmtNums_i1, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat,  &
          ESMF_CONTEXT, rcToReturn=rc)) return

    case default
      if (ESMF_LogFoundError(ESMF_RC_FILE_UNEXPECTED,  &
          msg='unsupport numElementConn variable type', &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end select

    ! calculate the RaggedArray size
    totalConn = 0
    do i=1,localcount
      totalConn=totalConn+elmtNums(i)
    enddo
    allocate(elementConn(totalConn), stat=memstat)
    if (ESMF_LogFoundAllocError(memstat,  &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! read elmt_verts data
    ncStatus = nf90_inq_varid (ncid, "elementConn", VarNo)
    errmsg = "Variable elementConn in "//trim(filename)
    if (CDFCheckError (ncStatus, &
       ESMF_METHOD,  &
       ESMF_SRCLINE, errmsg, &
       rc)) return

    if (isRaggedArray) then
      ! need to find out the start index, broadcast the totalConn
      senddata(1)=totalConn
      allocate(recvdata(PetCnt), stat=memstat)
      if (ESMF_LogFoundAllocError(memstat,  &
          ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_VMAllGather(vm, senddata, recvdata, 1, rc=status)
      if (ESMF_LogFoundError(status, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
      startConn=1
      do i=1,PetNo
        startConn=startConn+recvdata(i)
      enddo
      deallocate(recvdata, stat=memstat)
      if (ESMF_LogFoundDeallocError(memstat,  &
          ESMF_CONTEXT, rcToReturn=rc)) return

      ncStatus = nf90_get_var(ncid, VarNo, elementConn, start=(/startConn/), &
                              count=(/totalConn/))
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
    else
       ! allocate local 2D array to read the data
       allocate (elementConnLocal (MaxNodePerElmt, localcount), stat=memstat)
       if (ESMF_LogFoundAllocError(memstat,  &
           ESMF_CONTEXT, rcToReturn=rc)) return

       ncStatus = nf90_get_var(ncid, VarNo, elementConnLocal, start=(/1,startElmt/), &
                              count=(/MaxNodePerElmt, localcount/))
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
       ! copy it to elementConn
       j=1
       do i=1,localcount
          elementConn(j:j+elmtNums(i)-1)=elementConnLocal(1:elmtNums(i),i)
          j = j + elmtNums(i)
       end do
       deallocate(elementConnLocal, stat=memstat)
       if (ESMF_LogFoundAllocError(memstat,  &
           ESMF_CONTEXT, rcToReturn=rc)) return

    endif

    ! Get polybreak if it exists
    PolyBreakFound=.false.
     ncStatus = nf90_get_att (ncid, VarNo, "polygon_break_value", &
         values=localPolyBreakValue)
    if (ncStatus == nf90_noerror) then
       PolyBreakFound=.true.
    endif

    ! If a Polygon break value was found, then change those to internal polygon break
    if (PolyBreakFound) then
       do i=1,totalConn
          if (elementConn(i)==localPolyBreakValue) then
             elementConn(i)=ESMF_MESH_POLYBREAK
          endif
       enddo
    endif

    ! Get start_index attribute if it exists
    ! change it to 1-based if it is zero-based
    ncStatus = nf90_get_att (ncid, VarNo, "start_index", &
         values=startIndex)
    if (ncStatus == nf90_noerror) then
      if (startIndex == 0) then
         do i=1,totalConn
           if (elementConn(i) >= 0) then
              elementConn(i)=elementConn(i)+1
           endif
         enddo
      endif
    endif

    ! Check for negative index values that is not defiend as ESMF_MESH_POLYBREAK
    do i=1,totalConn
       if (elementConn(i) < 0 .and. elementConn(i) /= ESMF_MESH_POLYBREAK) then
           call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                msg="- negative index found in elementConn table", &
                ESMF_CONTEXT, rcToReturn=rc)
           return
       endif
    enddo          
    if (present(elementMask)) then
       allocate(elementMask(localcount), stat=memstat)
       if (ESMF_LogFoundAllocError(memstat,  &
           ESMF_CONTEXT, rcToReturn=rc)) return

       ncStatus = nf90_inq_varid (ncid, "elementMask", VarNo)
       errmsg = "Variable elementMask in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return

       ncStatus = nf90_get_var (ncid, VarNo, elementMask, start=(/startElmt/), count=(/localcount/))
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
           ESMF_SRCLINE, errmsg, &
          rc)) return

    end if

    if (present(elementArea)) then
       allocate(elementArea(localcount), stat=memstat)
       if (ESMF_LogFoundAllocError(memstat,  &
           ESMF_CONTEXT, rcToReturn=rc)) return

       ncStatus = nf90_inq_varid (ncid, "elementArea", VarNo)
       errmsg = "Variable elementArea in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return

       ncStatus = nf90_get_var (ncid, VarNo, elementArea, start=(/startElmt/), &
                                count=(/localcount/))
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
    end if

    ! check if centerCoords exist, if not, warning and return null pointer, do not return
    ! error message
    if (present(centerCoords)) then
       ncStatus = nf90_inq_varid (ncid, "centerCoords", VarNo)
       if (ncStatus /= nf90_noerror) then
         errmsg = "Variable centerCoords in "//trim(filename)// " does not exist"
         call ESMF_LogWrite (msg=errmsg, logmsgFlag=ESMF_LOGMSG_WARNING, &
               ESMF_CONTEXT)
       else
       ! Get vertex dimension
       ncStatus = nf90_inq_dimid (ncid, "coordDim", DimId)
       errmsg = "Dimension coordDim in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
           rc)) return

       ncStatus = nf90_inquire_dimension (ncid, DimId, len=coordDim)
       if (CDFCheckError (ncStatus, &
           ESMF_METHOD,  &
           ESMF_SRCLINE, errmsg, &
           rc)) return

       allocate(centerCoords(coordDim, localcount), stat=memstat)
       if (ESMF_LogFoundAllocError(memstat,  &
           ESMF_CONTEXT, rcToReturn=rc)) return

       ncStatus = nf90_inq_varid (ncid, "centerCoords", VarNo)
       errmsg = "Variable centerCoords in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return

       ncStatus = nf90_get_var (ncid, VarNo, centerCoords, start=(/1,startElmt/), &
                                count=(/coordDim, localcount/))
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return

       ! Check units, but only if 2D
       if (coordDim==2) then
           ncStatus = nf90_inquire_attribute(ncid, VarNo, "units", len=len)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return

           ncStatus = nf90_get_att(ncid, VarNo, "units", units)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,errmsg,&
               rc)) return
           ! if len != 7, something is wrong, check the value.  If it starts
            ! with Degres/degrees/Radians/radians, ignore the garbage after the
           ! word.  Otherwise, return the whole thing
           if (units(len:len) .eq. achar(0)) len = len-1
           units = ESMF_UtilStringLowerCase(units(1:len))
           if (units(1:len) .ne. 'degrees' .and. &
                 units(1:len) .ne. 'radians') then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                    msg="- units attribute is not degrees or radians", &
                    ESMF_CONTEXT, rcToReturn=rc)
              return
           endif

           ! if units is "radians", convert it to degree
           if (convertToDegLocal) then
              if (units(1:len) .eq. "radians") then
                  centerCoords(:,:) = &
                      centerCoords(:,:)*ESMF_COORDSYS_RAD2DEG
              endif
           endif
           endif
       endif
    end if

    ncStatus = nf90_close (ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return
#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end subroutine ESMF_EsmfGetElement

!
!  Get the NodeCoords and ElementConn from the ESMF unstructured file and construct
!   corner vertices table similar to what stored in the SCRIP file
!
#undef  ESMF_METHOD
#define ESMF_METHOD "EsmfGetVerts"
subroutine ESMF_EsmfGetVerts(ncid, filename, numElements, numNodePElement, numNodes, &
                latBuffer, lonBuffer, rc)

    integer, intent(in)  :: ncid
    character(len=*), intent(in) :: filename
    integer, intent(in)  :: numElements
    integer, intent(in)  :: numNodePElement
    integer, intent(in)  :: numNodes
    real(ESMF_KIND_R8), intent(inout)   :: latBuffer(:,:)
    real(ESMF_KIND_R8), intent(inout)   :: lonBuffer(:,:)
    integer, intent(out), optional      :: rc

    integer :: ncStatus
    real(ESMF_KIND_R8), allocatable :: nodeCoords(:,:)
    integer(ESMF_KIND_I4), allocatable :: elementConn(:,:)
    integer :: varId, fillValue
    integer, parameter :: fillValue1 = -9999
    integer :: i,j, index
    character(len=256)::errmsg
    integer :: localPolyBreakValue
    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    allocate(nodeCoords(2, numNodes))
    allocate(elementConn(numNodePElement, numElements))

    ! Get NodeCoords
    ncStatus = nf90_inq_varid(ncid, "nodeCoords", varId)
    errmsg = "Variable nodeCoords in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ncStatus = nf90_get_var(ncid, varId, nodeCoords)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get elementConn table
    ncStatus = nf90_inq_varid(ncid, "elementConn", varId)
    errmsg = "Variable elementConn in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ncStatus = nf90_get_var(ncid, varId, elementConn)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Get fill value
    ncStatus = nf90_get_att(ncid, varId, "_FillValue", fillValue)
    errmsg = "Attribute _FillValue for elementConn in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get polybreak if it exists, if it doesn't just use set to fillValue
     ncStatus = nf90_get_att (ncid, varId, "polygon_break_value", &
         values=localPolyBreakValue)
    if (ncStatus /= nf90_noerror) then
       localPolyBreakValue=fillValue
    endif


    ! Fill latBuffer and lonBuffer
    do i=1, numElements
      do j=1, numNodePElement
         if ((elementConn(j,i) /= fillValue) .and. &
             (elementConn(j,i) /= localPolyBreakValue)) then
            index = elementConn(j,i)
            latBuffer(j,i) = nodeCoords(2,index)
            lonBuffer(j,i) = nodeCoords(1,index)
         else
            latBuffer(j,i) = fillValue1
            lonBuffer(j,i) = fillValue1
         endif
      enddo
    enddo
    if (present(rc)) rc=ESMF_SUCCESS
    return
#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end subroutine ESMF_EsmfGetVerts


!
!  Get either the NodeCoords or centerCoords from the ESMF unstructured file on all the PETs.
!  It is used in ESMF_LocStreamCreate().  If faceflag is .TRUE., get centerCoords.
!
#undef  ESMF_METHOD
#define ESMF_METHOD "EsmfGetCoords"
subroutine ESMF_EsmfGetCoords(filename, coordBuffer, maskBuffer, start, count, &
                faceflag, rc)
     character(len=*), intent(in) :: filename
     real(ESMF_KIND_R8), intent(out) :: coordBuffer(:,:)
     integer(ESMF_KIND_I4), intent(out) :: maskBuffer(:)
     integer,          intent(in) :: start
     integer,          intent(in) :: count
     logical, intent(in), optional :: faceflag
     integer, intent(out)          :: rc

    integer, parameter :: nf90_noerror = 0
    logical            :: localfaceflag
    integer            :: ncStatus
    integer            :: ncid, varid
    integer            :: coorddim
    character(len=256) :: coordVar, maskVar, errmsg

    if (present(faceflag)) then
        localfaceflag = faceflag
    else
        localfaceflag = .FALSE.
    endif
#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    if (localfaceflag) then
       coordVar = "centerCoords"
       maskVar = "elementMask"
    else
      coordVar = "nodeCoords"
       maskVar = "nodeMask"
    endif
    coorddim = size(coordBuffer,1)
    ncStatus = nf90_inq_varid(ncid, coordVar, varid)
    if (ncStatus /= nf90_noerror) then
       errmsg = "- error reading "// trim(coordVar)//" from "//trim(filename)
       if (CDFCheckError (ncStatus, &
            ESMF_METHOD,  &
            ESMF_SRCLINE, errmsg, &
            rc)) return
    else
       ncStatus = nf90_get_var(ncid, varid, coordBuffer, start=(/1,start/), &
             count=(/coorddim,count/))
       errmsg = "- error reading "// trim(coordVar)//" from "//trim(filename)
       if (CDFCheckError (ncStatus, &
            ESMF_METHOD,  &
            ESMF_SRCLINE, errmsg, &
            rc)) return
    endif

    ncStatus = nf90_inq_varid(ncid, maskVar, varid)
    if (ncStatus /= nf90_noerror) then
         ! no mask defined, set it to 1
       maskBuffer(:) = 1
    else
       ncStatus = nf90_get_var(ncid, varid, maskBuffer, start=(/start/), &
             count=(/count/))
       errmsg = "- error reading "//trim(maskVar)//" from "//trim(filename)
       if (CDFCheckError (ncStatus, &
            ESMF_METHOD,  &
            ESMF_SRCLINE, errmsg, &
            rc)) return
    endif

    ncStatus = nf90_close (ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end subroutine ESMF_EsmfGetCoords
!-----------------------------------------------------------------------

!
!  check CDF file error code
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CDFCheckError"
function CDFCheckError (ncStatus, module, fileName, lineNo, errmsg, rc)

    logical                       :: CDFCheckError

    integer,          intent(in)  :: ncStatus
    character(len=*), intent(in)  :: module
    character(len=*), intent(in)  :: fileName
    integer,          intent(in)  :: lineNo
    character(len=*), intent(in)  :: errmsg
    integer, intent(out),optional :: rc

    integer, parameter :: nf90_noerror = 0

    CDFCheckError = .FALSE.

#ifdef ESMF_NETCDF
    if ( ncStatus .ne. nf90_noerror) then
        call ESMF_LogWrite (  &
            msg="netCDF Error: " // trim (errmsg) // ": " // trim (nf90_strerror(ncStatus)),  &
            logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=lineNo, file=fileName, method=module)
        print '("NetCDF Error: ", A, " : ", A)', &
            trim(errmsg),trim(nf90_strerror(ncStatus))
        call ESMF_LogFlush()
        if (present(rc)) rc = ESMF_FAILURE
        CDFCheckError = .TRUE.
    else
       if (present(rc)) rc = ESMF_SUCCESS
       return
    end if
#else
    if (ESMF_LogFoundError(ESMF_RC_LIB_NOT_PRESENT, &
                msg="- ESMF_NETCDF not defined when lib was compiled", &
                ESMF_CONTEXT, rcToReturn=rc)) return
#endif

end function CDFCheckError


end module ESMF_IOScripMod


!!!!!! f_esmf_ interfaces to be callable from ESMCI !!!!!!!!!!!!!!!!!!!!!!!

subroutine f_esmf_outputsimpleweightfile(fileName, count, factorList, &
  factorIndexList, rc)
  use ESMF_UtilTypesMod
  use ESMF_IOScripMod
  use ESMF_LogErrMod
  
  implicit none
  
  character(len=*)          :: fileName
  integer                   :: count
  real(ESMF_KIND_R8)        :: factorList(count)
  integer(ESMF_KIND_I4)     :: factorIndexList(2,count)
  integer                   :: rc
  
  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  ! call into the ESMF_ routine
  call ESMF_OutputSimpleWeightFile(fileName, factorList, &
    factorIndexList, rc=rc)
  if (ESMF_LogFoundError(rc, &
    ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT)) return
  
  ! return successfully
  rc = ESMF_SUCCESS
  
end subroutine f_esmf_outputsimpleweightfile
