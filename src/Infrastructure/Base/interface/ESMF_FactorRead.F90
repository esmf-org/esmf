! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_FactorRead.F90"
!==============================================================================
!
!     ESMF Factor Read Module
      module ESMF_FactorReadMod
!
!==============================================================================
!
! This file contains a subroutine for reading weights from a SCRIP/ESMF format
! weights file. It also contains an inquire function to get the size of a
! netCDF dimension and a subroutine to check netCDF library return codes.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!===============================================================================
!BOPI
!
! !MODULE: ESMF_FactorReadMod
!
! !DESCRIPTION:
! Reads weights from supported netCDF weight file specifications.
!
!------------------------------------------------------------------------------
! !USES:
#ifdef ESMF_NETCDF
      use netcdf
#endif
      use ESMF_UtilTypesMod
      use ESMF_LogErrMod
      use ESMF_VMMod
      implicit none

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_FactorRead

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_NetCDFCheckError
      private ESMF_NetCDFInquireDimension

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FactorRead"

!BOPI
! !IROUTINE: ESMF_FactorRead - Read factors from an ESMF-formatted weights file.
subroutine ESMF_FactorRead(filename, factorList, factorIndexList, rc)

! ! ARGUMENTS:
    character(len=*), intent(in) :: filename
    real(ESMF_KIND_R8), dimension(:), allocatable, intent(out) :: factorList
    integer, dimension(:, :), allocatable, intent(out) :: factorIndexList
    integer, intent(out), optional :: rc

!-------------------------------------------------------------------------------
! !DESCRIPTION:
!
! The arguments are:
!
! \begin{description}
!
! \item [filename]
!       Path to the file containing weights for creating an {\tt ESMF\_RouteHandle}.
!       See ~(\ref{sec:weightfileformat}) for a description of the SCRIP weight
!       file format. Only "row", "col", and "S" variables are required. They
!       must be one-dimensional with dimension "n\_s".
!
! \item [factorList]
!       The weight factors / interpolation weights to be read from file.
!
! \item [factorIndexList]
!       The indices into the source and destination arrays to be read from file. The
!       first dimension are the source indices. The second dimension are the
!       destination indices.
!
! \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOPI
!-------------------------------------------------------------------------------

    ! LOCAL VARIABLES ----------------------------------------------------------

    ! Variable name for index into the destination array.
    character(len=3), parameter :: vfactorindexlist_dst = "row"
    ! Variable name for index into the source array.
    character(len=3), parameter :: vfactorindexlist_src = "col"
    ! Variable name for the factor value.
    character(len=1), parameter :: vfactorlist = "S"
    ! Dimensions for the factor variables.
    character(len=3), parameter :: dfactor = "n_s"

    integer :: ncid, varid, dimid, localPet, petCount, nElements, esplit, lb, ub, remainder
    integer, dimension(1) :: nElementsArray, startArray
    integer :: ncStatus, theSize
    type(ESMF_VM) :: vm

    ! --------------------------------------------------------------------------

#ifdef ESMF_NETCDF
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Get all VM information
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set up local pet info
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! --------------------------------------------------------------------------

    ! Open the netCDF file.
    ncStatus = nf90_open(filename, NF90_NOWRITE, ncid)
    if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, filename, __LINE__, rc)) return

    ! Get the number of factors from the target netCDF file.
    nElements = ESMF_NetCDFInquireDimension(dfactor, ncid=ncid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Compute the lower and upper bounds for the current PET -------------------

    esplit = floor(real(nElements) / real(petCount))
    remainder = nElements - esplit*petCount
    if (localPet<remainder) then
      lb = localPet * (esplit+1) + 1
      theSize = esplit+1
    else
      lb = localPet * esplit + remainder + 1
      theSize = esplit
    endif
    ub = lb + theSize - 1

    ! --------------------------------------------------------------------------

    ! Convert to arrays as expected by the netCDF Fortran interface. Set the size
    ! of the factor arrays to zero if the localPet will not read any values.
    startArray = (/lb/)
    nElementsArray = (/theSize/)

    ! Allocate our factor arrays now that we know the size of the dimension.
    allocate(factorList(theSize))
    allocate(factorIndexList(2, theSize))

    !--- Fill variables from the netCDF file -----------------------------------

    ! Only read from file if the lower bound is reasonable - falls inside the size
    ! of the target variable.
    if (lb <= nElements) then
      ncStatus = nf90_inq_varid(ncid, vfactorindexlist_dst, varid)
      if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, filename, __LINE__, rc)) &
        return
      ncStatus = nf90_get_var(ncid, varid, factorIndexList(2, :), &
        start=startArray, count=nElementsArray)
      if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, filename, __LINE__, rc)) &
        return

      ncStatus = nf90_inq_varid(ncid, vfactorindexlist_src, varid)
      if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, filename, __LINE__, rc)) &
        return
      ncStatus = nf90_get_var(ncid, varid, factorIndexList(1, :), &
        start=startArray, count=nElementsArray)
      if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, filename, __LINE__, rc)) &
        return

      ncStatus = nf90_inq_varid(ncid, vfactorlist, varid)
      if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, filename, __LINE__, rc)) &
        return
      ncStatus = nf90_get_var(ncid, varid, factorList, start=startArray, &
        count=nElementsArray)
      if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, filename, __LINE__, rc)) &
        return
    endif

    !---------------------------------------------------------------------------

    ! Close the file.
    ncStatus = nf90_close(ncid)
    if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, filename, __LINE__, rc)) return

    if (present(rc)) rc = ESMF_SUCCESS
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", ESMF_CONTEXT, &
      rcToReturn=rc)
    return

#endif

  end subroutine ESMF_FactorRead

!-------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NetCDFInquireDimension"
  integer function ESMF_NetCDFInquireDimension(dimensionName, path, ncid, rc) result(n)

! ! ARGUMENTS:
    character(len=*), intent(in) :: dimensionName
    character(len=*), intent(in), optional :: path
    integer, intent(in), optional :: ncid
    integer, intent(out), optional :: rc

!-------------------------------------------------------------------------------
! !DESCRIPTION:
!
! Return the integer length of a dimension in a netCDF file using a file path or file
! identifier.
!
! The arguments are:
!
! \begin{description}
!
! \item [dimensionName]
!       The name of the target dimension.
!
! \item [{[path]}]
!       Path to the target file. Required if {\tt ncid} is not provided.
!
! \item [{[ncid]}]
!       NetCDF file identifier of the target file. Required if {\tt path} is not provided.
!
! \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOPI
!-------------------------------------------------------------------------------

    ! LOCAL VARIABLES:
    integer :: dimid, ncidV, ncStatus

    ! --------------------------------------------------------------------------

#ifdef ESMF_NETCDF
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    if (present(ncid)) ncidV = ncid

    ! Open the file and update the file identifier variable.
    if (.not. present(ncid)) then
      ncStatus = nf90_open(path, NF90_NOWRITE, ncidV)
      if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, path, __LINE__, rc)) return
    endif

    ! Get the dimension identifier.
    ncStatus = nf90_inq_dimid(ncidV, dimensionName, dimid)
    if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, path, __LINE__, rc)) return
    ! Get the length/size of the dimension.
    ncStatus = nf90_inquire_dimension(ncidV, dimid, len=n)
    if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, path, __LINE__, rc)) return

    ! Close the file.
    if (.not. present(ncid)) then
      ncStatus = nf90_close(ncidV)
      if (ESMF_NetCDFCheckError(ncStatus, ESMF_METHOD, path, __LINE__, rc)) return
    endif

    if (present(rc)) rc = ESMF_SUCCESS
#else
    n=-1  ! indicate invalid
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

  end function ESMF_NetCDFInquireDimension

!-------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NetCDFCheckError"
function ESMF_NetCDFCheckError(ncStatus, module, fileName, lineNo, rc)

    logical                        :: ESMF_NetCDFCheckError

    integer, intent(in)            :: ncStatus
    character(len=*), intent(in)   :: module
    character(len=*), intent(in)   :: fileName
    integer, intent(in)  :: lineNo
    integer, intent(out), optional :: rc

    integer, parameter :: nf90_noerror = 0

    ESMF_NetCDFCheckError = .FALSE.

#ifdef ESMF_NETCDF
    if (ncStatus .ne. nf90_noerror) then
      call ESMF_LogWrite (msg="netCDF Status Return Error", &
          logmsgFlag=ESMF_LOGMSG_ERROR, line=lineNo, file=fileName, method=module)
      call ESMF_LogFlush()
      if (present(rc)) rc = ESMF_FAILURE
      ESMF_NetCDFCheckError = .TRUE.
    else
      if (present(rc)) rc = ESMF_SUCCESS
      return
    end if
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", ESMF_CONTEXT, &
      rcToReturn=rc)
    return
#endif

end function ESMF_NetCDFCheckError

!-------------------------------------------------------------------------------

      end module ESMF_FactorReadMod
