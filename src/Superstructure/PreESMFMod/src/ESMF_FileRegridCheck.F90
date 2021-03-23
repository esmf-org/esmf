!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!!-------------------------------------------------------------------------------------

!==============================================================================
#define ESMF_FILENAME "ESMF_FileRegridCheck.F90"
!==============================================================================
!
module ESMF_FileRegridCheckMod
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
  use ESMF_IOUGridMod
  use ESMF_IOGridmosaicMod
  use ESMF_IOFileTypeCheckMod
  use ESMF_FileRegridMod

  implicit none

!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:

  public ESMF_FileRegridCheck

!------------------------------------------------------------------------------

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FileRegridCheck"

!BOPI
! !IROUTINE: ESMF_FileRegridCheck - Check regridding weights
! !INTERFACE:
  subroutine ESMF_FileRegridCheck(dstFile, dstVarName, &
             keywordEnforcer,dstDataFile, tileFilePath, &
             regridmethod, rc)

! !ARGUMENTS:

  character(len=*),             intent(in)            :: dstFile
  character(len=*),             intent(in)            :: dstVarName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  character(len=*),             intent(in),  optional :: dstDataFile     
  character(len=*),             intent(in),  optional :: tileFilePath
  type(ESMF_RegridMethod_Flag), intent(in),  optional :: regridmethod
  integer,                      intent(out), optional :: rc


! !DESCRIPTION:
!
! The arguments are:
!   \begin{description}
!   \item [srcFile]
!     The source grid file name.
!   \item [dstFile]
!     The destination grid file name.
!   \item [dstVarName]
!     The destination variable names to be regridded to. If more than one,
!     separate them by comma.
!   \item [{[dstDataFile]}]
!     The output data file prefix if the dstFile is in GRIDSPEC MOSAIC
!     fileformat.  The tilename and the file extension (.nc) will be added to
!     the prefix.  The tilename is defined in the MOSAIC file using variable "gridtiles".
!   \item [{[tileFilePath]}]
!     The alternative file path for the tile files and mosaic data files when either srcFile or
!     dstFile is a GRIDSPEC MOSAIC grid.  The path can be either relative or absolute.  If it is
!     relative, it is relative to the working directory.  When specified, the gridlocation 
!     variable defined in the Mosaic file will be ignored.
!   \item [{[regridmethod]}]
!     The type of interpolation. Please see Section~\ref{opt:regridmethod}
!     for a list of valid options. If not specified, defaults to
!     {\tt ESMF\_REGRIDMETHOD\_BILINEAR}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!EOPI
    !--------------------------------------------------------------------------
    ! DECLARATIONS
    !--------------------------------------------------------------------------
    integer :: PetNo, PetCnt
    integer :: status

    type(ESMF_VM) :: vm

    type(ESMF_FileFormat_Flag)   :: localdstFileType
    character(ESMF_MAXPATHLEN)   :: localOutputfile
    logical                      :: isConserve
    type(ESMF_Mosaic)            :: dstMosaic
    integer                      :: dstRank
    logical                      :: dstVarExist, useDstMask
    integer, pointer             :: dstVarDims(:,:)    
    integer, pointer             :: dstVarRank(:)
    character(len=MAXNAMELEN)    :: dstLocStr
    character(len=MAXNAMELEN)    :: dstLocStrSave
    character(len=MAXNAMELEN)    :: dstMeshVar
    character(len=MAXNAMELEN), pointer :: dstVarNames(:)
    integer                      :: dstVarType
    character(len=MAXNAMELEN*2)  :: dstVarStr
    integer                      :: dstDimids(MAX_VARDIMS)
    integer                      :: dstVarCount
    type(ESMF_MeshLoc)           :: dstmeshloc
    real(ESMF_KIND_R8)           :: dstMissingVal
    logical                      :: useDstCorner
    integer                      :: start1, count1, pos1
    integer                      :: i, j, k, l, m
    real(ESMF_KIND_R8), allocatable  :: lonarray1D(:), latarray1D(:)
    real(ESMF_KIND_R8), allocatable  :: lonarray2D(:,:), latarray2D(:,:)
    real(ESMF_KIND_R8), allocatable  :: fptr1d(:), fptr2d(:,:)
    real(ESMF_KIND_R8), allocatable  :: fptr3d(:,:,:), fptr4d(:,:,:,:)
    real(ESMF_KIND_R8), allocatable  :: synfptr1d(:), synfptr2d(:,:)
    real(ESMF_KIND_R8), allocatable  :: synfptr3d(:,:,:), synfptr4d(:,:,:,:)
    real(ESMF_KIND_R8)           :: base, mindata, maxdata, maxerr, meanerr
    real(ESMF_KIND_R8)           :: totalerr, relerror
    integer                      :: totalcnt
    integer                      :: localrc
    integer                      :: ntiles, nsize, tile
    character(ESMF_MAXPATHLEN)   :: dsttempname

    real(ESMF_KIND_R8), parameter :: two = 2.0

    !--------------------------------------------------------------------------
    ! EXECUTION
    !--------------------------------------------------------------------------

    localOutputfile=' '
    isConserve = .FALSE.
    if (present(dstDataFile)) localOutputfile = dstDataFile
    if (present(regridmethod)) then
      if (regridmethod == ESMF_REGRIDMETHOD_CONSERVE .or. &
          regridmethod == ESMF_REGRIDMETHOD_CONSERVE_2ND) then
          isConserve = .TRUE.
      endif
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
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=status)
    if (ESMF_LogFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, &
      rcToReturn=rc)) return

    !Set finalrc to success
    rc = ESMF_SUCCESS

    if (PetNo == 0) then
      ! Do this in all the PETs so that no need to communicate the info to other nodess
      call ESMF_FileTypeCheck(dstFile, localdstFileType, varname=dstMeshVar, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
      if (localdstFileType /= ESMF_FILEFORMAT_UGRID .and. localdstFileType /= ESMF_FILEFORMAT_GRIDSPEC & 
          .and. localdstFileType /= ESMF_FILEFORMAT_MOSAIC) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg = " Destination FileType has to be one of UGRID, CFTILE or GRIDSPEC MOSAIC", &
               ESMF_CONTEXT, rcToReturn=rc)
          return
      endif
      if (localdstFileType == ESMF_FILEFORMAT_UGRID) then
          dstRank = 1
      else
          dstRank = 2
      endif

      if (localdstFileType == ESMF_FILEFORMAT_MOSAIC .and. localOutputFile .eq. ' ') then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg = " dstDataFile argument not present when the dstFile is a GRIDSPEC MOSAIC grid", &
               ESMF_CONTEXT, rcToReturn=rc)
          return
      endif
   
      if (localdstFileType == ESMF_FILEFORMAT_MOSAIC) then
         call ESMF_GridSpecReadMosaic(dstFile, dstMosaic, tileFilePath = tileFilePath, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! Parse dstVarName, store the variable names in array dstVarNames(:)
      
      ! Two phase, first find out how many variables, secondly, store the variable
      ! names in an array

      pos1 = index(dstVarName(1:),",")
      start1 = 1
      count1=1
      do while (pos1 > 0)
        start1 = start1+pos1
        count1 = count1+1
        pos1 = index(dstVarName(start1:),",")
      end do
      dstVarCount = count1

      allocate(dstVarRank(dstVarCount))
      allocate(dstVarNames(dstVarCount))
      allocate(dstVarDims(MAX_VARDIMS, dstVarCount))
      
      pos1 = index(dstVarName(1:),",")
      start1 = 1
      count1=1
      do while (pos1 > 0)
        dstVarNames(count1) = dstVarName(start1:start1+pos1-2)
        start1 = start1+pos1
        pos1 = index(dstVarName(start1:),",")
        count1 = count1+1
      end do
      dstVarNames(count1) = trim(dstVarName(start1:))

      do i=1,dstVarCount
        if (localdstFileType == ESMF_FILEFORMAT_MOSAIC) then
          dsttempname = trim(dstMosaic%tileDirectory)//trim(localOutputFile)//"."//trim(dstMosaic%tilenames(1))//".nc"
          call checkVarInfo(trim(dsttempname), trim(dstVarNames(i)), dstVarExist, &
                   localdstFileType, dstMeshVar, dstVarStr, &
                   dstVarRank(i), dstVarDims(:,i), dstDimids, &
                   useDstMask, dstMissingVal, &
                   vartype=dstVarType,  rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          dstLocStr = 'face'
        else
          call checkVarInfo(trim(dstFile), trim(dstVarNames(i)), dstVarExist, &
                   localdstFileType, dstMeshVar, dstVarStr, &
                   dstVarRank(i), dstVarDims(:,i), dstDimids, &
                   useDstMask, dstMissingVal, &
                   vartype=dstVarType, locStr=dstLocStr, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if (i==1) then
          if (localdstFileType == ESMF_FILEFORMAT_UGRID) then
             if (dstLocStr .eq. 'node') then     
                useDstCorner = .TRUE.
                dstmeshloc=ESMF_MESHLOC_NODE
             else
                dstmeshloc=ESMF_MESHLOC_ELEMENT
             endif 
          endif
          dstLocStrSave = dstLocStr
        else
          if (trim(dstLocStr) .ne. trim(dstLocStrSave)) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
                 msg = " All the destination variables have to be on the same stagger location", &
                 ESMF_CONTEXT, rcToReturn=rc)
             return
          endif
        endif
    
        ! For each variable, read in the coordinates and the variable value to
        ! check the error.
        if (i==1) then
          ntiles = 1
          nsize = dstVarDims(1,1)
          if (localDstFileType == ESMF_FILEFORMAT_GRIDSPEC) then
            allocate(lonarray2D(dstVarDims(1,1), dstVarDims(2,1)), &
                   latarray2D(dstVarDims(1,1), dstVarDims(2,1)))
            call GridSpecReadCoords(dstFile, dstVarStr, lonarray2D, latarray2D, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
          else if (localDstFileType == ESMF_FILEFORMAT_UGRID) then
            allocate(lonarray1D(dstVarDims(1,1)),latarray1D(dstVarDims(1,1)))
            call UGridReadCoords(dstFile, dstMeshVar, dstmeshloc, lonarray1D, &
              latarray1D, rc=localrc) 
            if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
          else !Cubed Sphere Mosaic
            ! append the coordinates along the latitude dimension (2nd)
            ntiles = dstMosaic%ntiles;
            nsize = dstMosaic%nx;
            allocate(lonarray2D(nsize,nsize*ntiles), latarray2D(nsize,nsize*ntiles))
            call MosaicReadCoords(dstMosaic, lonarray2D, latarray2D, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          endif
        endif

        if (localdstFiletype /= ESMF_FILEFORMAT_MOSAIC) then
          if (dstVarRank(i) == 1) then
            allocate(fptr1d(dstVarDims(1,i)))
            ! UGRID only, read the variable
            call ReadVar1D(dstfile, dstVarNames(i), fptr1d, localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          else if (dstVarRank(i) == 2) then
            allocate(fptr2d(dstVarDims(1,i),dstVarDims(2,i)))
            call readVar2D(dstfile, dstVarNames(i), fptr2d, localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          else if (dstVarRank(i) == 3) then
            allocate(fptr3d(dstVarDims(1,i),dstVarDims(2,i), dstVarDims(3,i)))
            call readVar3D(dstfile, dstVarNames(i), fptr3d, localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          else if (dstVarRank(i) == 4) then
            allocate(fptr4d(dstVarDims(1,i), dstVarDims(2,i), dstVarDims(3,i), dstVarDims(4,i)))
            call readVar4D(dstfile, dstVarNames(i), fptr4d, localrc)
            if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          endif
        else
          ! Cubed-Sphere multi-tile, append the tiles along its last dimension
          dstVarDims(1,i)=nsize
          dstVarDims(2,i)=nsize
          do j=1,ntiles
            dsttempname = trim(dstMosaic%tileDirectory)//trim(localOutputFile)//"."//trim(dstMosaic%tilenames(j))//".nc"
            if (dstVarRank(i) == 2) then
              if (j==1) allocate(fptr2d(nsize,nsize*ntiles))
              call readVar2D(dsttempname, dstVarNames(i), fptr2d(:,(j-1)*nsize+1:j*nsize), localrc)
              if (ESMF_LogFoundError(localrc, &
                 ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rcToReturn=rc)) return 
           else if (dstVarRank(i) == 3) then
              if (j==1) allocate(fptr3d(nsize, nsize, dstVarDims(3,i)*ntiles))
              call readVar3D(dsttempname, dstVarNames(i), &
                 fptr3d(:,:,(j-1)*dstVarDims(3,i)+1:j*dstVarDims(3,i)), localrc)
              if (ESMF_LogFoundError(localrc, &
                 ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rcToReturn=rc)) return 
            else if (dstVarRank(i) == 4) then
              if (j==1) allocate(fptr4d(ntiles*nsize, nsize, dstVarDims(3,i), dstVarDims(4,i)*ntiles))
              call readVar4D(dsttempname, dstVarNames(i), &
                 fptr4d(:,:,:,(j-1)*dstVarDims(4,i)+1:j*dstVarDims(4,i)), localrc)
              if (ESMF_LogFoundError(localrc, &
                 ESMF_ERR_PASSTHRU, &
                 ESMF_CONTEXT, rcToReturn=rc)) return 
            endif
          enddo
        endif

        ! Create synthetic fields
        ! The formular is 
        ! data(i,j,k,l)=2.0+cos(lat(i,j))**2*cos(2*lon(i,j))+(k-1)+2*(l-1)
        ! If it is a cubed-sphere grid, the data from the multiple tiles are appended along the l dimension
        if (dstVarRank(i) == 1) then       
          allocate(synfptr1d(dstVarDims(1,i)))
          !UGRID only
          do j=1, dstVarDims(1,i)
            synfptr1d(j)=two+dcos(latarray1D(j))**2*dcos(two*lonarray1D(j))
          enddo
        else if (dstVarRank(i) == 2) then  
          allocate(synfptr2d(size(fptr2d,1),size(fptr2d,2)))
          if (localdstfiletype == ESMF_FILEFORMAT_UGRID) then
            do j=1,size(fptr2d,1)
              do k=1,size(fptr2d,2)
                 synfptr2d(j,k)=two+(k-1)+dcos(latarray1D(j))**2*dcos(two*lonarray1D(j))
              enddo
            enddo
          else
            do j=1,size(fptr2d,1)
              do k=1,size(fptr2d,2)
                 synfptr2d(j,k)=two+dcos(latarray2D(j,k))**2*dcos(two*lonarray2D(j,k))
              enddo
            enddo
          endif
        else if (dstVarRank(i) == 3) then
          allocate(synfptr3d(size(fptr3d,1),size(fptr3d,2),size(fptr3d,3)))
          if (localdstfiletype == ESMF_FILEFORMAT_UGRID) then
            do j=1,size(fptr3d,1)
              do k=1,size(fptr3d,2)
                do l=1,size(fptr3d,3)
                  synfptr3d(j,k,l)= two+(k-1)+2*(l-1)+dcos(latarray1D(j))**2*dcos(two*lonarray1D(j))
                enddo
              enddo
            enddo
          else
            do j=1,size(fptr3d,1)
              do k=1,size(fptr3d,2)
                do tile = 1, ntiles
                  base = two+dcos(latarray2D(j,k+nsize*(tile-1)))**2*dcos(two*lonarray2D(j,k+nsize*(tile-1)))
                  do l=dstVarDims(3,i)*(tile-1)+1, dstVarDims(3,i)*tile
                    synfptr3d(j,k,l)= base+(l-1)-dstVarDims(3,i)*(tile-1)
                  enddo
                enddo
              enddo
            enddo
          endif
        else if (dstVarRank(i) == 4) then
          allocate(synfptr4d(size(fptr4d,1),size(fptr4d,2),size(fptr4d,3), size(fptr4d,4)))
          do j=1,size(fptr4d,1)
            do k=1,size(fptr4d,2)
              do l=1,size(fptr4d,3)
                do tile = 1, ntiles
                  base = two+dcos(latarray2D(j,k+nsize*(tile-1)))**2*dcos(two*lonarray2D(j,k+nsize*(tile-1)))+l-1
                  do m=dstVarDims(4,i)*(tile-1)+1, dstVarDims(4,i)*tile
                    synfptr4d(j,k,l,m)= base+two*(m-dstVarDims(4,i)*(tile-1)-1)
                  enddo
                enddo
              enddo
            enddo
          enddo
        endif

        !Calculate maximal and mean relative errors and the min/max value of the destination field
        totalerr = 0
        maxerr = 0
        totalcnt = 0
        mindata = 999.0
        maxdata = 0
        if (dstVarRank(i) == 1) then       
          do j=1,size(fptr1d,1)
            if (fptr1d(j) /= dstMissingVal) then
              if (synfptr1d(j) /= 0) then
                 relerror = abs(fptr1d(j)-synfptr1d(j))/synfptr1d(j)
              else
                 relerror = abs(fptr1d(j)-synfptr1d(j))
              endif
              totalerr = totalerr+relerror
              if (relerror > maxerr) maxerr = relerror
              if (fptr1d(j) > maxdata) maxdata = fptr1d(j)
              if (fptr1d(j) < mindata) mindata = fptr1d(j)
              totalcnt = totalcnt+1    
            endif
          enddo
        elseif (dstVarRank(i) == 2) then   
          do j=1,size(fptr2d,1)
            do k=1,size(fptr2d,2)
              if (fptr2d(j,k) /= dstMissingVal) then
                if (synfptr2d(j,k) /= 0) then
                   relerror = abs(fptr2d(j,k)-synfptr2d(j,k))/synfptr2d(j,k)
                else
                   relerror = abs(fptr2d(j,k)-synfptr2d(j,k))
                endif
                totalerr = totalerr+relerror
                if (relerror > maxerr) maxerr = relerror
                if (fptr2d(j,k) > maxdata) maxdata = fptr2d(j,k)
                if (fptr2d(j,k) < mindata) mindata = fptr2d(j,k)
                totalcnt = totalcnt+1    
              endif
            enddo   
          enddo
        elseif (dstVarRank(i) == 3) then   
          do j=1,size(fptr3d,1)
            do k=1,size(fptr3d,2)
              do l=1,size(fptr3d,3)
                if (fptr3d(j,k,l) /= dstMissingVal) then
                  if (synfptr3d(j,k,l) /= 0) then
                     relerror = abs(fptr3d(j,k,l)-synfptr3d(j,k,l))/synfptr3d(j,k,l)
                  else
                     relerror = abs(fptr3d(j,k,l)-synfptr3d(j,k,l))
                  endif
                  totalerr = totalerr+relerror
                  if (relerror > maxerr) maxerr = relerror
                  totalcnt = totalcnt+1    
                  if (fptr3d(j,k,l) > maxdata) maxdata = fptr3d(j,k,l)
                  if (fptr3d(j,k,l) < mindata) mindata = fptr3d(j,k,l)
                endif
              enddo
            enddo   
          enddo
        elseif (dstVarRank(i) == 4) then   
          do j=1,size(fptr4d,1)
            do k=1,size(fptr4d,2)
              do l=1,size(fptr4d,3)
                do m=1,size(fptr4d,4)
                  if (fptr4d(j,k,l,m) /= dstMissingVal) then
                    if (synfptr4d(j,k,l,m) /= 0) then
                       relerror = abs(fptr4d(j,k,l,m)-synfptr4d(j,k,l,m))/synfptr4d(j,k,l,m)
                    else
                       relerror = abs(fptr4d(j,k,l,m)-synfptr4d(j,k,l,m))
                    endif
                    totalerr = totalerr+relerror
                    if (relerror > maxerr) maxerr = relerror
                    totalcnt = totalcnt+1    
                    if (fptr4d(j,k,l,m) > maxdata) maxdata = fptr4d(j,k,l,m)
                    if (fptr4d(j,k,l,m) < mindata) mindata = fptr4d(j,k,l,m)
                  endif
                enddo
              enddo
            enddo   
          enddo
        endif

        meanerr = totalerr/totalcnt
             
        print *, " "
        print *, "Variable Name           = ", trim(dstVarNames(i))
        print *, " "
        print *, "Value min: ", mindata, "    Value max: ", maxdata
        print *, "Mean relative error     = ", meanerr
        print *, "Maximum relative error  = ", maxerr
        print *, " "

        if (dstVarRank(i) == 1) deallocate(fptr1d, synfptr1d)
        if (dstVarRank(i) == 2) deallocate(fptr2d, synfptr2d)
        if (dstVarRank(i) == 3) deallocate(fptr3d, synfptr3d)
        if (dstVarRank(i) == 4) deallocate(fptr4d, synfptr4d)
      enddo
      deallocate(dstVarRank, dstVarNames, dstVarDims)
    endif
    rc = ESMF_SUCCESS
    return        
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

  end subroutine ESMF_FileRegridCheck

#undef  ESMF_METHOD
#define ESMF_METHOD "GridSpecReadCoords"
  subroutine GridSpecReadCoords(filename, coordsname, lonarray, latarray, rc)

  character(len=*), intent(in) :: filename
  character(len=*), intent(in) :: coordsname
  real(ESMF_KIND_R8), TARGET  :: lonarray(:,:)
  real(ESMF_KIND_R8), TARGET  :: latarray(:,:)
  integer                      :: rc

    character(len=128) :: errmsg
    integer, parameter :: nf90_noerror = 0
    integer :: ncStatus
    integer ::  gridid, varid, tempids(1), varids(2), meshid, len
    character(len=128) :: attvalue, locallocstr, varnames(2)
    integer :: ndims, dimids(2), dims(2)
    integer :: i, j, nvars, pos
    real(ESMF_KIND_R8), allocatable :: buffer(:)
    real(ESMF_KIND_R8), parameter :: d2r = 3.141592653589793238/180

#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=filename, mode=nf90_nowrite, ncid=gridid)
    errmsg = 'Fail to open '//trim(filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
    pos = INDEX(coordsname, ' ')
    varnames(1)=coordsname(1:pos-1)
    varnames(2)=coordsname(pos+1:)
    do i=1,2
       ncStatus = nf90_inq_varid(gridid, varnames(i), varid)
       errmsg = 'Coordinate variable '//trim(varnames(i))//' does not exist'
       if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
       ncStatus = nf90_inquire_variable(gridid, varid, ndims=ndims, dimids=dimids)
       errmsg = 'nf90_inquire_variable failed '//trim(varnames(i))
       if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
       do j=1, ndims
           ncStatus = nf90_inquire_dimension(gridid, dimids(j), len=dims(j))
           errmsg = 'nf90_inquire_dimension failed '//trim(filename)
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
       enddo
       if (ndims == 1) then 
           allocate(buffer(dims(1)))
           ncStatus = nf90_get_var(gridid, varid, buffer)
           errmsg = 'Read variable failed: '//trim(varnames(i))
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
           if (i==1) then !longitude
             do j=1,size(lonarray, 2)
               lonarray(:,j)=buffer
             enddo
           else
             do j=1,size(latarray, 1)
               latarray(j,:)=buffer
             enddo
           endif
           deallocate(buffer)
       else
           if (i==1) then
               ncStatus = nf90_get_var(gridid, varid, lonarray)
           else
               ncStatus = nf90_get_var(gridid, varid, latarray)
           endif
           errmsg = 'Read variable failed: '//trim(varnames(i))
           if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
       endif
    enddo  
    latarray = latarray*d2r
    lonarray = lonarray*d2r
    rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif
  end subroutine GridSpecReadCoords

#undef  ESMF_METHOD
#define ESMF_METHOD "UGridReadCoords"
  subroutine UGridReadCoords(filename, meshvar, meshloc, lonarray, latarray, rc)

  character(len=*), intent(in) :: filename
  character(len=*), intent(in) :: meshvar
  type(ESMF_MeshLoc)           :: meshloc
  real(ESMF_KIND_R8), TARGET  :: lonarray(:)
  real(ESMF_KIND_R8), TARGET  :: latarray(:)
  integer                      :: rc

    character(len=128) :: errmsg
    integer, parameter :: nf90_noerror = 0
    integer :: ncStatus
    integer ::  gridid, varid, tempids(1), varids(2), meshid, len
    character(len=128) :: attvalue, coordsname, varnames(2)
    integer :: ndims, dimids(2)
    integer :: i, nvars, pos
    real(ESMF_KIND_R8), parameter :: d2r = 3.141592653589793238/180

#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=filename, mode=nf90_nowrite, ncid=gridid)
    errmsg = 'Fail to open '//trim(filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return

    ncStatus = nf90_inq_varid(gridid, meshvar, meshid)
    errmsg = 'Dummy variable '//trim(meshvar)//' does not exist'
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return

    if (meshloc .eq. ESMF_MESHLOC_ELEMENT) then
        ncStatus=nf90_get_att(gridid, meshid, 'face_coordinates', coordsname) 
        errmsg = 'face_coordinates attribute does not exist '//trim(filename)
        if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
    else ! node, default for non-conservative
        ncStatus=nf90_get_att(gridid, meshid, 'node_coordinates', coordsname)
        errmsg = 'node_coordinates attribute does not exist '//trim(filename)
        if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
    endif
    pos = INDEX(coordsname, ' ')
    varnames(1)=coordsname(1:pos-1)
    varnames(2)=coordsname(pos+1:)
      
    do i=1,2
       ncStatus = nf90_inq_varid(gridid, varnames(i), varid)
       errmsg = 'Coordinate variable '//trim(varnames(i))//' does not exist'
       if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
       ! Check it is a longitude or a latitude
       ncStatus = nf90_inquire_attribute(gridid, varid, 'units', len=len)
       errmsg = 'Attribute units of '//trim(varnames(i))//' does not exist'
       if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
       ncStatus=nf90_get_att(gridid, varid, 'units', attvalue)
       errmsg = 'Attribute units of '//trim(varnames(i))//' does not exist'
       if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
        if (attvalue(len:len) .eq. achar(0)) len = len-1
        if (attvalue(1:len) .eq. "degrees_east" .or. &
              attvalue(1:len) .eq. "degree_east" .or. &
              attvalue(1:len) .eq. "degree_E" .or. &
              attvalue(1:len) .eq. "degrees_E" .or. &
              attvalue(1:len) .eq. "degreeE" .or. &
              attvalue(1:len) .eq. "degreesE")  then
             ncStatus = nf90_get_var(gridid, varid,lonarray)
             errmsg = 'Read variable failed: '//trim(varnames(i))
             if (CDFCheckError (ncStatus, &
                 ESMF_METHOD, &
                 ESMF_SRCLINE,&
                 errmsg, &
                 rc)) return
        else
            ncStatus = nf90_get_var(gridid, varid, latarray)
            errmsg = 'Read variable failed: '//trim(varnames(i))
            if (CDFCheckError (ncStatus, &
               ESMF_METHOD, &
               ESMF_SRCLINE,&
               errmsg, &
               rc)) return
        endif
    enddo  
    latarray = latarray*d2r
    lonarray = lonarray*d2r
    rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif
  end subroutine UGridReadCoords

#undef  ESMF_METHOD
#define ESMF_METHOD "MosaicReadCoords"
  subroutine MosaicReadCoords(mosaic, lonarray, latarray, rc)

  type(ESMF_Mosaic)            :: mosaic
  real(ESMF_KIND_R8), TARGET  :: lonarray(:,:)
  real(ESMF_KIND_R8), TARGET  :: latarray(:,:)
  integer                      :: rc

    character(len=128) :: errmsg
    integer, parameter :: nf90_noerror = 0
    integer :: ncStatus
    integer ::  gridid, varid, tempids(1), varids(2), meshid, len
    character(len=128) :: attvalue, locallocstr, varnames(2)
    integer :: ndims, dimids(2)
    integer :: i, nvars, pos
    integer :: localrc
    integer :: ntiles, nsize
    real(ESMF_KIND_R8), pointer  :: lontemp(:,:), lattemp(:,:)
    character(ESMF_MAXPATHLEN)   :: filename
    real(ESMF_KIND_R8), parameter :: d2r = 3.141592653589793238/180

#ifdef ESMF_NETCDF
    ntiles = mosaic%ntiles
    nsize = mosaic%ny
    do i=1,ntiles
      filename = trim(mosaic%tileDirectory)//trim(mosaic%filenames(i))
      call ESMF_GridspecReadStagger(filename, mosaic%nx, mosaic%ny, &
           lonarray(:,nsize*(i-1)+1:nsize*i), latarray(:,nsize*(i-1)+1:nsize*i), &
           ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
           ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
    enddo  
    latarray = latarray*d2r
    lonarray = lonarray*d2r
    rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif
  end subroutine MosaicReadCoords

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadVar1D"
  subroutine readVar1D(filename, varname, farray, rc)

  character(len=*), intent(in) :: filename
  character(len=*), intent(in) :: varname
  real(ESMF_KIND_R8), TARGET  :: farray(:)
  integer                      :: rc

    character(len=128) :: errmsg
    integer, parameter :: nf90_noerror = 0
    integer :: ncStatus
    integer ::  gridid, varid

    rc = ESMF_FAILURE

#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=filename, mode=nf90_nowrite, ncid=gridid)
    errmsg = 'Fail to open '//trim(filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return

    ncStatus = nf90_inq_varid(gridid, varname, varid)
    errmsg = 'Data variable '//trim(varname)//' does not exist'
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return

    ncStatus = nf90_get_var(gridid, varid, farray)
    errmsg = 'Fail to read variable '//trim(varname)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
    rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif
  end subroutine readVar1D

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadVar2D"
  subroutine readVar2D(filename, varname, farray, rc)

  character(len=*), intent(in) :: filename
  character(len=*), intent(in) :: varname
  real(ESMF_KIND_R8), TARGET  :: farray(:,:)
  integer                      :: rc

    character(len=128) :: errmsg
    integer, parameter :: nf90_noerror = 0
    integer :: ncStatus
    integer ::  gridid, varid

    rc = ESMF_FAILURE

#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=filename, mode=nf90_nowrite, ncid=gridid)
    errmsg = 'Fail to open '//trim(filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return

    ncStatus = nf90_inq_varid(gridid, varname, varid)
    errmsg = 'Data variable '//trim(varname)//' does not exist'
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return

    ncStatus = nf90_get_var(gridid, varid, farray)
    errmsg = 'Fail to read variable '//trim(varname)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
    rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif
  end subroutine readVar2D

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadVar3D"
  subroutine readVar3D(filename, varname, farray, rc)

  character(len=*), intent(in) :: filename
  character(len=*), intent(in) :: varname
  real(ESMF_KIND_R8), TARGET  :: farray(:,:,:)
  integer                      :: rc

    character(len=128) :: errmsg
    integer, parameter :: nf90_noerror = 0
    integer :: ncStatus
    integer ::  gridid, varid

    rc = ESMF_FAILURE

#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=filename, mode=nf90_nowrite, ncid=gridid)
    errmsg = 'Fail to open '//trim(filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return

    ncStatus = nf90_inq_varid(gridid, varname, varid)
    errmsg = 'Data variable '//trim(varname)//' does not exist'
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return

    ncStatus = nf90_get_var(gridid, varid, farray)
    errmsg = 'Fail to read variable '//trim(varname)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
    rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif
  end subroutine readVar3D

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadVar4D"
  subroutine readVar4D(filename, varname, farray, rc)

  character(len=*), intent(in) :: filename
  character(len=*), intent(in) :: varname
  real(ESMF_KIND_R8), TARGET  :: farray(:,:,:,:)
  integer                      :: rc

    character(len=128) :: errmsg
    integer, parameter :: nf90_noerror = 0
    integer :: ncStatus
    integer ::  gridid, varid

    rc = ESMF_FAILURE

#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=filename, mode=nf90_nowrite, ncid=gridid)
    errmsg = 'Fail to open '//trim(filename)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return

    ncStatus = nf90_inq_varid(gridid, varname, varid)
    errmsg = 'Data variable '//trim(varname)//' does not exist'
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return

    ncStatus = nf90_get_var(gridid, varid, farray)
    errmsg = 'Fail to read variable '//trim(varname)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
    rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
      msg="- ESMF_NETCDF not defined when lib was compiled", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
#endif
  end subroutine readVar4D
  
end module ESMF_FileRegridCheckMod
