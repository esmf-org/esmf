!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id: ESMF_CubedSphereRegridEx.F90,v 1.5 2010/09/29 04:31:48 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
! Offline Regrid Program for Cubed Sphere grid stored in a NetCDF file with the following 
! structure
!dimensions:
!	num_verts = 48600 ;
!	num_cells = 48602 ;
!	max_verts_per_cell = 4 ;
!	vert_dim = 2 ;
!variables:
!	double vert_coords(num_verts, vert_dim) ;
!		vert_coords:units = "degrees" ;
!	int cell_verts(num_cells, max_verts_per_cell) ;
!		cell_verts:long_name = "Vertex Indices of the cell" ;
!		cell_verts:_FillValue = -1 ;
!	byte num_cell_verts(num_cells) ;
!		num_cell_verts:long_name = "Number of vertices per cell" ;
!	double center_coords(num_cells, vert_dim) ;
!		center_coords:units = "degrees" ;
!	double cell_area(num_cells) ;
!		cell_area:units = "radians^2" ;
!		cell_area:long_name = "area weights" ;
!	double cell_mask(num_cells) ;
!		cell_mask:_FillValue = -9999. ;
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CubedSphereRegrid"

program ESMF_CubedSphereRegridEx

!==============================================================================
!ESMF_disable_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

#include <ESMF.h>

! !USES:
!      use ESMF_TestMod     ! test methods
      use ESMF_Mod
      use ESMF_LogErrMod
#ifdef ESMF_NETCDF
      use netcdf
#endif


      implicit none

      integer :: PetNo, PetCnt

      integer :: rc, status

      type(ESMF_VM) :: vm

      ! mesh topology & coordinates
      real(ESMF_KIND_R8),    pointer :: VertexCoords (:,:)
      integer(ESMF_KIND_I4), pointer :: CellConnect  (:,:)
      integer(ESMF_KIND_I4), pointer :: CellNums     (:)

      ! ESMF mesh
      type(ESMF_Mesh) :: srcMesh
      type(ESMF_RouteHandle) :: rh, rh1
      ! test result status variables
      integer :: nlen, nsize
      integer, allocatable:: array1(:), array2(:)
      integer :: FailCnt
      integer :: StartCell
      logical, parameter :: checkpoint = .false.
      integer, parameter :: nf_noerror = 0
      integer totalNodes, totalCells, count
      integer :: i, j, k
      integer :: xdim, ydim
      integer, allocatable:: nodeIds(:)
      integer :: lbnd1(2), lbnd2(2), ubnd1(2), ubnd2(2)
      integer :: dims(2)
      real(ESMF_KIND_R8), pointer :: fptr(:), fptr1(:,:), fptr2(:,:)
      real(ESMF_KIND_R8), pointer :: array3(:,:)
      real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)
      real(ESMF_KIND_R8) :: val, error, deg2rad
      type(ESMF_Grid) :: dstgrid1
      type(ESMF_DistGrid):: nodeDG, elemDG
      type(ESMF_Array) :: darray
      type(ESMF_ArraySpec) :: arraySpec
      type(ESMF_Field) :: field1, field2
      type(ESMF_FieldBundle) :: bundle
      integer(ESMF_KIND_I4), pointer:: indicies(:,:), seqIndex(:)
      real(ESMF_KIND_R8), pointer :: weights(:)
      character(len=2) :: petstring     
      character(len=256) :: srcfile, dstfile, wgtfile
      character(len=256) :: input_scrip_file, esmf_mesh_file, dual_mesh_file
      integer :: input_scrip_file_len, esmf_mesh_file_len, dual_mesh_file_len
      character(len=40) :: regrid_type, revflag
      integer :: numarg
      logical :: convert3D

      !------------------------------------------------------------------------
      ! Initialize ESMF
      !
      call ESMF_Initialize (defaultCalendar=ESMF_CAL_GREGORIAN, &
			defaultlogfilename="CubedSphereRegridEx.Log", &
                    	defaultlogtype=ESMF_LOG_MULTI, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
	  ESMF_CONTEXT, rcToReturn=rc)) goto 90

      !------------------------------------------------------------------------
      ! get global vm information
      !
      call ESMF_VMGetGlobal(vm, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 99

      ! set up local pet info
      call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 99

      !------------------------------------------------------------------------
      ! Usage:  ESMF_CubedSphereRegrid input_grid output_grid weight_file regrid_method
      !
      numarg = ESMF_UtilGetArgC()
      if (numarg < 4) then
	if (PetNo == 0) then
          print *, 'ERROR: insufficient arguments'
	  print *, 'USAGE: ESMF_CubedSphereRegridEx first_grid second_grid weight_file regrid_method [rev]'
          print *, 'the first_grid is a unstructured grid with the format defined in the ESMF'
          print *, 'unstructured grid NetCDF format and the second_grid is a rectangular 2D grid'
          print *, 'in SCRIP format.  The weight_file is the output weight file also in SCRIP'
          print *, 'format. regrid_method specifies the interpolation method, which can be'
          print *, 'either "bilinear" or "patch". "rev" is an optional argument. Without'
          print *, 'it, we regrid the first_grid into the second_grid.  With it set to "rev",'
          print *, 'we regrid the second_grid into the first_grid.'
	endif
        call ESMF_Finalize(terminationflag=ESMF_ABORT)
      endif
      call ESMF_UtilGetArg(1,srcfile)
      call ESMF_UtilGetArg(2,dstfile)
      call ESMF_UtilGetArg(3,wgtfile)
      call ESMF_UtilGetArg(4,regrid_type)
      if (numarg == 5) then
	call ESMF_UtilGetArg(5, revflag)
        if (trim(revflag) .ne. 'rev') then
	     print *, 'The fifth argument is not "rev".'
             call ESMF_Finalize(terminationflag=ESMF_ABORT)
        endif
      endif
      !Set finalrc to success
      rc = ESMF_SUCCESS
      failCnt = 0
      
#define SRC_IS_SCRIP
#ifdef SRC_IS_SCRIP
      ! *************************************************************
      ! convert a cubed sphere grid file from SCRIP NetCDF format
      ! into ESMF NetCDF data format
      ! *************************************************************
      ! prepare file name strings
      input_scrip_file = srcfile
      esmf_mesh_file = ".esmf.nc"
      dual_mesh_file = ".dual.nc"
      srcfile = dual_mesh_file
      if (PetNo == 0) then
        ! this is a serial call into C code for now
        input_scrip_file_len = len_trim(input_scrip_file)
        esmf_mesh_file_len = len_trim(esmf_mesh_file)
        dual_mesh_file_len = len_trim(dual_mesh_file)
        call c_ConvertSCRIP(input_scrip_file, input_scrip_file_len, &
          esmf_mesh_file, esmf_mesh_file_len, &
          dual_mesh_file, dual_mesh_file_len, status)
        if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
      endif
      call ESMF_VMBarrier(vm, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
#endif

      ! *************************************************************
      ! read the mesh from the NetCDF file
      call ReadCSMesh(srcfile, VertexCoords, CellConnect, CellNums, StartCell, status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

      ! *************************************************************      
      ! create ESMF mesh
      convert3D = .true.
      call CreateCSMesh(srcMesh, VertexCoords, CellConnect, CellNums, StartCell,convert3D, status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

      ! check the MeshGet
      call ESMF_MeshGet(srcMesh, nodalDistgrid=nodeDG, elementDistgrid=elemDG, &
            numOwnedNodes=totalNodes, numOwnedElements=totalCells, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

      ! print *, "PE nodeDG elemDG",PetNo, totalNodes, totalCells
      
      ! get node information from nodeDG
      allocate(nodeIds(totalNodes))
      call ESMF_DistGridGet(nodeDG, 0, seqIndexList=nodeIds, elementCount=count, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

      ! *************************************************************      
      ! get vertex data
      ! Create a field using the Mesh and an ESMF Array 
      call ESMF_ArraySpecSet(arraySpec, rank=1, typekind=ESMF_TYPEKIND_R8, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

      darray = ESMF_ArrayCreate(arraySpec, nodeDG, computationalEdgeLWidth=(/0/), &
	computationalEdgeUWidth=(/0/), rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

      call ESMF_ArrayGet(darray, farrayPtr=fptr, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

      ! fake the data array to use a linear function of its coordinates
      !!!!!!!
      deg2rad = 3.141592653589793238/180;
      do i=1,count
!         The below can give misleading bigger errors if dest points are closer to pole than src points 
!         Do constant for basic sanity check, until develop something more complex
!	fptr(i) = COS(deg2rad*VertexCoords(2,nodeIds(i)))
	fptr(i) = 1.0
      enddo
     !!!!!!!

      ! Create a field using the array and mesh
      field1 = ESMF_FieldCreate(srcMesh, darray, name="MeshFieldo", rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

      call ESMF_FieldValidate(field1, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

      ! deallocate (VertexCoords)
      deallocate (CellConnect)

      ! release data
      deallocate (NodeIds)

      !! Read in the destination grid fie and create a ESMF_Grid out of it
      if (PetNo == 0) then 
          call ReadRegGrid(dstfile, dims, coordX, coordY, status)
          if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
	  !! broadcast the grid dimension to all the PETs
       endif   
       call ESMF_VMBroadcast(vm, dims, 2, 0, rc=status)
       xdim = dims(1)
       ydim = dims(2)

      call CreateRegGrid(xdim, ydim, coordX, coordY, dstgrid1, field2, status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

      call ESMF_GridGetCoord(dstgrid1, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
	     computationalLBound=lbnd1, computationalUBound=ubnd1, &
	     fptr = fptr1, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
      call ESMF_GridGetCoord(dstgrid1, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
	     computationalLBound=lbnd2, computationalUBound=ubnd2, &
	     fptr = fptr2, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
      ! print *, lbnd1(1), lbnd1(2), fptr1(lbnd1(1),lbnd1(2)), fptr2(lbnd2(1),lbnd2(2))

      if (trim(regrid_type) .eq. 'bilinear') then
        if (trim(revflag) .ne. 'rev') then
        call ESMF_FieldRegridStore(srcField=field1, dstField=field2, & 
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_BILINEAR, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=status)
            if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
	else 
          call ESMF_FieldRegridStore(srcField=field2, dstField=field1, & 
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_BILINEAR, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=status)
            if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
        endif
      else if (trim(regrid_type) .eq. 'patch') then
        if (trim(revflag) .ne. 'rev') then
        call ESMF_FieldRegridStore(srcField=field1, dstField=field2, & 
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_PATCH, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=status)
            if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
	else 
          call ESMF_FieldRegridStore(srcField=field2, dstField=field1, & 
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_PATCH, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=status)
            if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
        endif
      else if (trim(regrid_type) .eq. 'conservative') then
        if (trim(revflag) .ne. 'rev') then
          call ESMF_FieldRegridStore(srcField=field1, dstField=field2, & 
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_BILINEAR, &
!	    regridConserve = ESMF_REGRID_CONSERVE_ON, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=status)
            if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
        else
          call ESMF_FieldRegridStore(srcField=field2, dstField=field1, & 
	    unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, routehandle = rh1, &
	    indicies=indicies, weights=weights, &
            regridMethod = ESMF_REGRID_METHOD_BILINEAR, &
!	    regridConserve = ESMF_REGRID_CONSERVE_ON, &
	    regridScheme = ESMF_REGRID_SCHEME_FULL3D, rc=status)
            if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
	endif
 
      else ! nothing recognizable so report error
	     print *, 'The fourth argument is not a recognized interpolation method.'
             call ESMF_Finalize(terminationflag=ESMF_ABORT)
      endif

      !! Write the weight table into a SCRIP format NetCDF file
      if (PetNo == 0) then
         call OutputWeightFile(wgtfile, srcfile, dstfile, indicies, weights, revflag, VertexCoords, &
	      coordX, coordY, xdim, ydim, rc=status)
         if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
	 deallocate(coordX, coordY)
      else 
	 call OutputWeightFile(wgtfile, srcfile, dstfile, indicies, weights, rc=status)
         if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
      endif
      !!deallocate(VertexCoords)

#ifdef INTERNAL_INTERP_CHECK
      if (trim(revflag) .ne. 'rev') then
         call ESMF_FieldRegrid(field1, field2, rh1, rc=status)
      else
         call ESMF_FieldRegrid(field2, field1, rh1, rc=status)
      endif
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

      !! Check the field results to verify if the regrid is correct
      !! first get the coordinates from the grid
      call ESMF_GridGetCoord(dstgrid1, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
	     computationalLBound=lbnd1, computationalUBound=ubnd1, &
	     fptr = fptr1, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
      call ESMF_GridGetCoord(dstgrid1, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
	     computationalLBound=lbnd2, computationalUBound=ubnd2, &
	     fptr = fptr2, rc=status)
      if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
      !   print *, lbnd1(1), lbnd1(2), fptr1(lbnd1(1),lbnd1(2)), fptr2(lbnd2(1),lbnd2(2))
#if 0
      !! write weight table into an ascii file, one file per PET
      if (PetNo < 10) then
         write(petstring,'(I1)') PetNo
      else
         write(petstring,'(I2)') PetNo
      endif
      open(10,FILE=trim(wgtfile)//'.'//trim(petstring))
      if (trim(revFlag) .eq. 'rev') then 
        do i=1,size(weights)
	  j=indicies(i,1)/xdim+1
          k = indicies(i,1)-((j-1)*xdim)
          write(10,'(2I7, 3F9.4)') indicies(i,1), indicies(i,2), &
	        VertexCoords(1,indicies(i,2)), &
		VertexCoords(2,indicies(i,2)), weights(i)
        enddo
      else
        do i=1,size(weights)
	  j=indicies(i,2)/xdim+1
          k = indicies(i,2)-((j-1)*xdim)
          write(10,'(2I7, 5F9.4)') indicies(i,1), indicies(i,2),  VertexCoords(1,indicies(i,1)), &
		VertexCoords(2,indicies(i,1)), fptr1(k,j), fptr2(k,j), weights(i)
        enddo
      endif
      close(10)
#endif

      if (trim(revflag) .eq. 'rev') then     
        ! regrid from dstgrid to srcgrid
        call ESMF_FieldGet(field1, localDe=0, farrayPtr=fptr, rc=status)
        ! Get Mesh's node distgrid and its seqIndex 
        call ESMF_MeshGet(srcMesh, nodalDistgrid=NodeDG, numOwnedNodes=totalNodes, rc=status)
        if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
        allocate(seqIndex(totalNodes))
        call ESMF_DistGridGet(NodeDG, localDe=0, seqIndexList=seqIndex, rc=status)
	if (status .ne. ESMF_SUCCESS)  call ESMF_Finalize(terminationflag=ESMF_ABORT)
        count = 0
        do i=1,totalNodes
!         The below can give bigger errors if dest points are closer to pole than src points 
!         Do constant for basic sanity check for now
!	  val = COS(deg2rad*VertexCoords(2,seqIndex(i)))
	  val = 1.0
	  if (val .ne. 0) then
             error = abs((fptr(i)-val)/val)
          else
	     error = abs(fptr(i)-val)
          endif
          if (error > 0.01) then
   	     write(*,'(I2,2I6,4F9.4)') PetNo, i,seqIndex(i), VertexCoords(1,seqIndex(i)), &
		VertexCoords(2,seqIndex(i)),fptr(i),val
		 count = count+1
          endif
        enddo
        deallocate(seqIndex)
      else
        call ESMF_FieldGet(field2, localDe=0, farrayPtr=array3, rc=status)
        if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
        count = 0
        do i=lbnd1(1),ubnd1(1)
          do j=lbnd1(2),ubnd1(2)
!         The below can give bigger errors if dest points are closer to pole than src points 
!         Do constant for basic sanity check, until develop something more complex
!	      val = COS(deg2rad*fptr2(i,j))
              val=1.0
	  if (val .ne. 0) then
             error = abs((array3(i,j)-val)/val)
          else
	     error = abs(array3(i,j)-val)
          endif
              if (error > 0.01) then
  		 write(*,'(3I4,5F9.3)') PetNo, i,j,fptr1(i,j),fptr2(i,j),array3(i,j), val
		 count = count+1
              endif
          enddo
        enddo
      endif
      print*, 'PE', PetNo, ' Total bad items', count, ' out of', totalNodes
      if (count > 0) status = ESMF_FAILURE

      ! *************************************************************
90    continue
      if (status .ne. ESMF_SUCCESS) then
          failCnt = 1
      end if

#if 1
      ! Gather test results from all processors
      ! allocate test status memory (field for gathering test results via mpi gather)
      nsize = 1
      nlen = nsize * PetCnt
      allocate(array1(nlen))
      allocate(array2(nsize))

      ! gather
      array2(1) = failCnt
      call ESMF_VMGather(vm, sendData=array2, recvData=array1, count=nsize, &
        root=0, rc=status)

      ! The gather pet checks the results and prints out PASS/FAIL message.
      if (PetNo .eq. 0) then
         status = 0
         do i=1, nlen
            status = status + array1(i)
         enddo

         if (status .eq. 0) then
            print *, "PASS: ESMF_CubedSphereRegridEx.F90"
         else
            print *, "FAIL: ESMF_CubedSphereRegridEx.F90"
         end if
      end if

      ! halt the computer, display 0 on operator console lamps
      call ESMF_Finalize()
      STOP
#endif

#else

90    continue
      if (status .eq. ESMF_SUCCESS) then
         print *, "ESMF_CubedSphereRegridEx executed successfully."
      else
         print *, "ERROR IN ESMF_CubedSphereRegridEx"
      end if

      ! halt the computer, display 0 on operator console lamps
      call ESMF_Finalize()
      STOP
#endif

! error exit point
99    continue
         print *, "ERROR IN ESMF_CubedSphereRegridEx."


      call ESMF_Finalize()
      ! halt the computer, display 777 on operator console lamps
      STOP 777
	
      ! -------- end of test code ------------------------
contains

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Create ESMF mesh
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateCSMesh"
!***********************************************************************************
! Create a triangle and quad mesh using a global node coordinate table and a distributed
! element connection array
!  arguments:  VertexCoords(3, NodeCnt), where NodeCnt is the total node count for the
!  global mesh, the first dimension stores the x,y,z coordinates of the node
!              CellConnect(4, ElemCnt), where ElemCnt is the total number of elements
!                The first dimension contains the global node IDs at the four corner of the element 
!              StartCell, the first Element ID in this PET
!  in this local PET.  Note the CellConnect is local and VertexCoords is global.
!  In this routine, we have to figure out which nodes are used by the local Elements 
!  and who are the owners of the local nodes, then add the local nodes and elements into
!  the mesh
!***********************************************************************************
subroutine CreateCSMesh (Mesh, VertexCoords, CellConnect, CellNums, StartCell, convert3D,status)

    type(ESMF_Mesh),       intent (out) :: Mesh
    real(ESMF_KIND_R8),    intent (in)  :: VertexCoords(:,:)
    integer(ESMF_KIND_I4), intent (in)  :: CellConnect(:,:)
    integer(ESMF_KIND_I4), intent (in)  :: CellNums(:)
    integer,                intent (in) ::  StartCell
    logical,      intent (in), optional :: convert3D
    integer,      intent (out),optional :: status

    integer                             :: NodeNo
    integer                             :: NodeCnt, NodeDim, total
    integer, allocatable                :: NodeId(:)
    integer, allocatable                :: NodeUsed(:)
    real(ESMF_KIND_R8), allocatable     :: NodeCoords(:)
    real(ESMF_KIND_R8)                  :: coorX, coorY, deg2rad
    integer, allocatable                :: NodeOwners(:)
    integer, allocatable                :: NodeOwners1(:)

    integer                             :: ElemNo, TotalElements
    integer                             :: ElemCnt,i,j,k,dim
    integer				:: localNodes, MyStartCell
    integer                             :: ConnNo, TotalConnects
    integer, allocatable                :: ElemId(:)
    integer, allocatable                :: ElemType(:)
    integer, allocatable                :: ElemConn(:)
    integer, allocatable                :: LocalElmTable(:)
    integer                             :: sndBuf(1)
    logical                             :: convert3Dlocal

    ! Total number of nodes for the global mesh
    NodeCnt = ubound (VertexCoords, 2)
    NodeDim  = ubound (VertexCoords, 1)
    deg2rad = 3.141592653589793238/180;

    ! create the mesh
    ! The spatialDim=3, we need to convert the 2D coordinate into 3D Cartisian in order
    ! to handle the periodic longitude correctly
    if (present(convert3D)) then
	convert3Dlocal = convert3D
    else
	convert3Dlocal = .FALSE.
    end if 
    if (convert3Dlocal) then
        Mesh = ESMF_MeshCreate (2, 3, status)
    else
        Mesh = ESMF_MeshCreate (2, NodeDim, status)
    end if
    if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ! These two arrays are temp arrays
    ! NodeUsed() used for multiple purposes, first, find the owners of the node
    ! later, used to store the local Node ID to be used in the ElmtConn table
    ! NodeOwners1() is the receiving array for ESMF_VMAllReduce(), it will store
    ! the lowest PET number that stores the node.  That PET will become the owner
    ! of the node.
    allocate (NodeUsed(NodeCnt))
    allocate (NodeOwners1(NodeCnt))

    ! Set to a number > PetCnt because it will store the PetNo if this node is used by
    ! the local elements and we will do a global reduce to find the minimal values
    NodeUsed(:)=PetCnt+100

    ! Total number of local elements
    ElemCnt = ubound (CellConnect, 2)

    ! Set the coorsponding NodeUsed(:) value to my PetNo if it is used by the local element 
    ! Also calculate the total number of mesh elements based on CellNums
    ! if CellNums == 3 or 4, no change, if CellNums > 4, break it into CellNums-2 triangles
    totalElements = ElemCnt
    totalConnects = 0
    do ElemNo =1, ElemCnt
        do i=1,CellNums(ElemNo)	
            NodeUsed(CellConnect(i,ElemNo))=PetNo
        enddo
	if (CellNums(ElemNo) > 4) TotalElements = TotalElements + (CellNums(ElemNo)-3)
	if (CellNums(ElemNo) <= 4) then
           TotalConnects = TotalConnects+CellNums(ElemNo)
        else
	   TotalConnects = TotalConnects+3*(CellNums(ElemNo)-2)
        end if
    end do

    ! Do a global reduce to find out the lowest PET No that owns each node, the result is in
    ! NodeOwners1(:) 
    call ESMF_VMAllReduce(vm, NodeUsed, NodeOwners1, NodeCnt, ESMF_MIN, rc=status)
    if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
    

    ! count number of nodes used and convert NodeUsed values into local index
    localNodes = 0
    do NodeNo = 1, NodeCnt
       if (NodeUsed(NodeNo) == PetNo) then
         localNodes = localNodes+1
	 NodeUsed(NodeNo) = localNodes
       else
	 NodeUsed(NodeNo) = 0
       endif
    enddo

    ! allocate nodes arrays for ESMF_MeshAddNodes()
    allocate (NodeId(localNodes))
    if (convert3Dlocal) then
       allocate(NodeCoords(localNodes*3))
    else
       allocate (NodeCoords(localNodes*NodeDim))
    endif
    allocate (NodeOwners(localNodes))

    ! copy vertex information into nodes, NodeUsed(:) now contains either 0 (not for me) or
    ! the local node index.  The owner of the node is stored in NodeOwners1(:)
    ! Also calculate how many nodes are "owned" by me -- total
    i = 1
    total = 0
    do NodeNo = 1, NodeCnt
        if (NodeUsed(NodeNo) > 0) then
	   NodeId     (i) = NodeNo     
	   if (convert3Dlocal) then
              coorX = VertexCoords(1,NodeNo)*deg2rad
              coorY = (90.0-VertexCoords(2,NodeNo))*deg2rad
              NodeCoords((i-1)*3+1) = COS(coorX)*SIN(coorY)             
              NodeCoords((i-1)*3+2) = SIN(coorX)*SIN(coorY)             
              NodeCoords((i-1)*3+3) = COS(coorY)
           !   write (*,'(6F8.4)')VertexCoords(:,NodeNo), COS(coorX),SIN(coorX),COS(coorY),SIN(coorY)
           else 
             do dim = 1, NodeDim
               NodeCoords ((i-1)*NodeDim+dim) = VertexCoords (dim, NodeNo)
	     end do
           endif
           NodeOwners (i) = NodeOwners1(NodeNo)
	   if (NodeOwners1(NodeNo) == PetNo) total = total+1
           i = i+1
        endif
    end do

    ! Add nodes
    call ESMF_MeshAddNodes (Mesh, NodeId, NodeCoords, NodeOwners, status)
    if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ! Need to calculate the total number of ESMF_MESH objects and the start element ID
    ! Do a global gather to get all the local TotalElements

    allocate(localElmTable(PetCnt))
    sndBuf(1)=TotalElements
    call ESMF_VMAllGather(vm, sndBuf, localElmTable, 1, rc=status)
    if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
    
    ! Find out the start element ID
    MyStartCell=0
    do i=1,PetNo
      MyStartCell = MyStartCell+localElmTable(i)
    end do
    deallocate(localElmTable)

    ! print *, PetNo, ' My start cell and total local ', MyStartCell, TotalElements, TotalConnects

    ! allocate element arrays for the local elements
    allocate (ElemId(TotalElements))
    allocate (ElemType(TotalElements))
    allocate (ElemConn(TotalConnects))

    ! the node number is 0 based, need to change it to 1 based
    ! The ElemId is the global ID.  The MyStartCell is the starting Element ID(-1), and the
    ! element IDs will be from StartCell to StartCell+ElemCnt-1
    ! The ElemConn() contains the four corner node IDs for each element and it is organized
    ! as a 1D array.  The node IDs are "local", which are stored in NodeUsed(:)
    ElemNo = 1
    ConnNo = 0
    do j = 1, ElemCnt
	if (CellNums(j)==3) then        
           ElemId(ElemNo) = MyStartCell+ElemNo
           ElemType (ElemNo) = ESMF_MESHELEMTYPE_TRI
           do i=1,3
              ElemConn (ConnNo+i) = NodeUsed(CellConnect(i,j))
	   end do
           ElemNo=ElemNo+1
           ConnNo=ConnNo+3
	elseif (CellNums(j)==4) then
           ElemId(ElemNo) = MyStartCell+ElemNo
           ElemType (ElemNo) = ESMF_MESHELEMTYPE_QUAD
           do i=1,4
              ElemConn (ConnNo+i) = NodeUsed(CellConnect(i,j))
	   end do
           ElemNo=ElemNo+1
	   ConnNo=ConnNo+4
	else
	! CellNums(j) > 4, break into CellNums(j)-2 triangles
	   do k=0,CellNums(j)-3
             ElemId(ElemNo)=MyStartCell+ElemNo
             ElemType (ElemNo) = ESMF_MESHELEMTYPE_TRI
             ElemConn (ConnNo+1) = NodeUsed(CellConnect(1,j))
             ElemConn (ConnNo+2) = NodeUsed(CellConnect(2+k,j))
             ElemConn (ConnNo+3) = NodeUsed(CellConnect(3+k,j))
             ElemNo=ElemNo+1
	     ConnNo=ConnNo+3
	   end do
         end if
    end do
    
    if (ElemNo /= TotalElements+1) then
	print *, PetNo, ' TotalElements does not match ',ElemNo-1, TotalElements
    end if

    ! Add elements
    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, status)
    if (CheckError (status, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    deallocate(NodeUsed, NodeId, NodeCoords, NodeOwners, NodeOwners1)
    deallocate(ElemId, ElemType, ElemConn)
90  continue
    if (status .ne. ESMF_SUCCESS) then
        print *, "Error detected in ", ESMF_METHOD
    end if

    return
end subroutine CreateCSMesh

!------------------------------------------------------------------------------
! ReadCSMesh: get vert_coords, cell_verts, and num_cell_verts arrays from the NetCDF file
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadCSMesh"
subroutine ReadCSMesh (filename, VertexCoords, CellConnect, CellNums, StartCell, status)
    implicit none
    character(len=256), intent(in):: filename
    real(ESMF_KIND_R8),    pointer :: VertexCoords (:,:)
    integer(ESMF_KIND_I4), pointer :: CellConnect (:,:)
    integer(ESMF_KIND_I4), pointer :: CellNums (:)
    integer,                        intent(out) :: StartCell
    integer,                        intent(out) :: status

    integer :: ncid
    integer :: ncStatus
    integer :: RecCnt (2)

    integer :: DimId
    integer :: VertexCnt, CellCount, MaxVertPerCell, VertDim
    integer :: localCount, remain

    integer :: VarNo

#ifdef ESMF_NETCDF
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ! get number of vertices
    ncStatus = nf90_inq_dimid (ncid, "num_verts", DimId)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=VertexCnt)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ! Get vertex dimension
    ncStatus = nf90_inq_dimid (ncid, "vert_dim", DimId)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=VertDim)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ! allocate memory for verticies
    allocate (VertexCoords (VertDim, VertexCnt))

    RecCnt(:) = ubound(VertexCoords)
   ! print *, lbound(VertexCoords), ubound(VertexCoords)
   ! print *, VertexCnt

    ! read vertex data
    ncStatus = nf90_inq_varid (ncid, "vert_coords", VarNo)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_get_var (ncid, VarNo, VertexCoords, start=(/1,1/), count=(/VertDim, VertexCnt/))
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ! get number of cells
    ncStatus = nf90_inq_dimid (ncid, "num_cells", DimId)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=CellCount)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ! Get max_verts_per_cell
    ncStatus = nf90_inq_dimid (ncid, "max_verts_per_cell", DimId)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=MaxVertPerCell)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ! Decompose the cell array evenly on all the PEs
    localcount = CellCount/PetCnt;
    remain = mod(CellCount,PetCnt);
    StartCell = localcount * PetNo+1
    if (PetNo == (PetCnt-1)) localcount = localcount+remain

    ! allocate memory for cells
    allocate (CellConnect (MaxVertPerCell, localcount))
    allocate (CellNums (localcount))

    ! print *, PetNo, StartCell, localcount
    
    ! read cell_verts data
    ncStatus = nf90_inq_varid (ncid, "cell_verts", VarNo)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_get_var (ncid, VarNo, CellConnect, start=(/1,StartCell/), count=(/MaxVertPerCell, localcount/))
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
    
    ! read num_cell_verts
    ncStatus = nf90_inq_varid (ncid, "num_cell_verts", VarNo)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_get_var (ncid, VarNo, CellNums, start=(/StartCell/), count=(/localcount/))
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_close (ncid=ncid)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

90  continue

    ! return ESMF compatible status
    if (ncStatus .eq. nf_noerror) then
        status = ESMF_SUCCESS
    else
        status = ESMF_FAILURE
    end if


    !status = (ncStatus .ne. nf_noerror)
    
#else
    status = ESMF_RC_LIB_NOT_PRESENT
#endif

    return
end subroutine ReadCSMesh

!------------------------------------------------------------------------------
! ReadRegGrid: Read in the center coordinates of a 2D Regular grid from a SCRIP file
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRegGrid"
subroutine ReadRegGrid(filename, dims, coordX, coordY, status)

    character(len=256), intent(in) :: filename
    integer, intent(out):: dims(2)	
    real(ESMF_KIND_R8),    pointer :: coordX(:), coordY(:)
    integer, intent(out):: status

#ifdef ESMF_NETCDF
    integer :: ncid
    integer :: ncStatus
    integer :: totalpoints,totaldims

    integer :: DimId, VarId

    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ! get number of vertices
    ncStatus = nf90_inq_dimid (ncid, "grid_size", DimId)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=totalpoints)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ! Get vertex dimension
    ncStatus = nf90_inq_dimid (ncid, "grid_rank", DimId)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_inquire_dimension (ncid, DimId, len=totaldims)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    if (totaldims /= 2) then
	call ESMF_LogMsgSetError(ESMF_RC_ARG_RANK,"- The grip has to be 2D", &
	ESMF_CONTEXT, status)
	return
    endif

    ncStatus = nf90_inq_varid (ncid, "grid_dims", VarId)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_get_var (ncid, VarId, dims)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    if (dims(1)*dims(2) /= totalpoints) then
	call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE,"- The grid_dims does not match with the grid_size", &
	ESMF_CONTEXT, status)
	return
    endif

    ! print *,trim(filename), dims, totalpoints

    ! Read in grid_center_lon and grid_center_lat
    allocate(coordX(totalpoints), coordY(totalpoints))
    ncStatus = nf90_inq_varid (ncid, "grid_center_lon", VarId)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
    ncStatus = nf90_get_var (ncid, VarId, coordX)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_inq_varid (ncid, "grid_center_lat", VarId)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
    ncStatus = nf90_get_var (ncid, VarId, coordY)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

    ncStatus = nf90_close(ncid)
    if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

90  continue

    ! return ESMF compatible status
    if (ncStatus .eq. nf_noerror) then
        status = ESMF_SUCCESS
    else
        status = ESMF_FAILURE
    end if

#else
    status = ESMF_RC_LIB_NOT_PRESENT
#endif

    return
end subroutine ReadRegGrid

!------------------------------------------------------------------------------
!
! check ESMF return code
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CheckError"
function CheckError (status, module, fileName, lineNo, checkpoint)
    implicit none
    integer,          intent(in)  :: status
    character(len=*), intent(in)  :: module
    character(len=*), intent(in)  :: fileName
    integer,          intent(in)  :: lineNo
    logical,          intent(in)  :: checkpoint
    logical                       :: CheckError

    !integer :: rc

    CheckError = status .ne. ESMF_SUCCESS

    if (checkpoint) then
        call ESMF_LogWrite ("Checkpoint", ESMF_LOG_INFO, lineNo, fileName, module)

        print '("Checkpoint detected in ", A, " near line ", I5, ", pet ", I5, ", (", A, ")")', &
          module, lineNo, petNo, fileName
    endif


    if (CheckError) then
        call ESMF_LogWrite ("Status Return Error", ESMF_LOG_ERROR, lineNo, fileName, module)
        print '("Error in ", A, " near line ", I5, ", pet ", I5, ", (", A, "), status code = ", I5)', &
          module, lineNo, petNo, fileName, status
    end if
    return
end function CheckError

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
!  check CDF file error code
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CDFCheckError"
function CDFCheckError (ncStatus, module, fileName, lineNo, checkpoint)
    implicit none
    integer,          intent(in)  :: ncStatus
    character(len=*), intent(in)  :: module
    character(len=*), intent(in)  :: fileName
    integer,          intent(in)  :: lineNo
    logical,          intent(in)  :: checkpoint
    logical                       :: CDFCheckError

#ifdef ESMF_NETCDF
    CDFCheckError = ncStatus .ne. nf_noerror

    if (checkpoint) then
        call ESMF_LogWrite ("netCDF Checkpoint", ESMF_LOG_INFO, lineNo, fileName, module)

        print '("CDF Checkpoint detected in ", A, " near line ", I5, ", pet ", I5, ", (", A, ")")', &
          module, lineNo, petNo, fileName
    endif


    if (CDFCheckError) then
        call ESMF_LogWrite ("netCDF Status Return Error", ESMF_LOG_ERROR, lineNo, fileName, module)
        print '("NetCDF Error in ", A, " near line ", I5, ", pet ", I5, ", (", A, "), error msg: ", A)', &
          module, lineNo, petNo, fileName, trim(nf90_strerror(ncStatus))
    end if
    return
#else
    CDFCheckError = .false.
#endif

end function CDFCheckError

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Create a regular grid of fv1.9x2.5 and a field on the grid
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CreateRegGrid"
!***********************************************************************************
!
!***********************************************************************************
subroutine CreateRegGrid (xdim, ydim, coordX, coordY, grid, field, rc)

    integer, intent(in) :: xdim, ydim
    real(ESMF_KIND_R8), intent(in) :: coordX(:)
    real(ESMF_KIND_R8), intent(in) :: coordY(:)
    type(ESMF_Grid),       intent (out) :: grid
    type(ESMF_Field),       intent (out) :: field
    integer,               intent (out) :: rc

    integer :: bigFac, nPetsX, nPetsY
    integer :: i
    real(ESMF_KIND_R8), pointer :: coord2D(:,:)
    real(ESMF_KIND_R8) :: deg2rad
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: array
    integer :: localrc
    integer :: ubnd(2,1), lbnd(2,1),lbnd1(2),ubnd1(2)
    real(ESMF_KIND_R8), pointer :: fptr(:,:), fptr1(:,:)
    rc = ESMF_SUCCESS

    bigFac = 1
    DO I=2,INT(sqrt(FLOAT(PetCnt)))
      IF ( (PetCnt/I)*I .EQ. PetCnt ) THEN
        bigFac = I
      ENDIF
    ENDDO
    nPetsX = bigFac
    nPetsY = PetCnt/bigFac
    if (PetNo .eq. 0) then 
       print *, 'source grid decomp: (',nPetsX, nPetsY, ')'
    endif

    ! Create Grid based on the decomposition
    grid = ESMF_GridCreateShapeTile(maxIndex=(/xdim, ydim/), &
		regDecomp=(/nPetsX, nPetsY/), &
		gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
		indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)

    ! Set coordinate tables 
    ! Longitude
    call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)
    call ESMF_GridGetCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
	array = array, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)

    allocate(coord2D(xdim,ydim))
    if (PetNo == 0) then
       coord2D = RESHAPE(coordX,(/xdim, ydim/))
       call ESMF_ArrayGet(array,minIndexPDimPPatch=lbnd,maxIndexPDimPPatch=ubnd,rc=localrc)
    endif
    call ESMF_ArrayScatter(array, coord2D, rootPet=0, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)

    
    ! Latitude
    call ESMF_GridGetCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
	array = array, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)
    if (PetNo == 0) then
       coord2D = RESHAPE(coordY,(/xdim, ydim/))
    endif
    call ESMF_ArrayScatter(array, coord2D, rootPet=0, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)
    if (PetNo == 0)  deallocate(coord2D)
    ! create field
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
    field = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)

    ! Assign the field values using its latitude values, get the latitude 
    call ESMF_GridGetCoord(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
	     computationalLBound=lbnd1, computationalUBound=ubnd1, &
	     fptr = fptr1, rc=status)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)
    call ESMF_FieldGet(field, localDe=0, farrayPtr=fptr, rc=status)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)

    ! fake the data array to use a linear function of its coordinates
    !!!!!!!
    deg2rad = 3.141592653589793238/180;
    do i=lbnd1(1),ubnd1(1)
       do j=lbnd1(2),ubnd1(2)
!          fptr(i,j) = COS(deg2rad*fptr1(i,j))
	   fptr(i,j) = 1.0
       enddo
    enddo
      !!!!!!!
    return
end subroutine CreateRegGrid

!--------------------------------------------------------------------
! output the weight and indices tables together with the source/dest vertex coordinates/masks to the
! SCRIP format NetCDF file
!--------------------------------------------------------------------
subroutine OutputWeightFile(filename, file1, file2, indices, weights, revflag, &
                            SrcVertexCoords, coordX, coordY, xdim,  ydim, rc)

      character(len=256) :: filename
      character(len=256) :: file1, file2
      real(ESMF_KIND_R8) , pointer :: weights(:)   
      integer(ESMF_KIND_I4) , pointer :: indices(:,:) 
      character(len=40), optional :: revflag
      real(ESMF_KIND_R8) , pointer, optional:: SrcVertexCoords(:,:)    
      real(ESMF_KIND_R8) , pointer, optional:: coordX(:), coordY(:)
      integer, optional :: xdim, ydim
      integer, optional :: rc

      integer :: total, localCount(1)
      integer :: ncid
      integer :: ncStatus
      integer :: status
      integer :: i,j, start
      integer :: srcDim, dstDim
      integer:: naDimId, nbDimId, nsDimId, srankDimId, drankDimId, varId
      integer :: nvaDimId, nvbDimId
      real(ESMF_KIND_R8), pointer   :: coords(:),area(:),frac(:)
      real(ESMF_KIND_R8), pointer   :: cnr_coords(:,:), weightbuf(:)
      integer(ESMF_KIND_I4), pointer:: indexbuf(:), next(:)
      integer(ESMF_KIND_I4), pointer:: mask(:) 
      integer(ESMF_KIND_I4), pointer:: allCounts(:) 
      integer :: maxcount
      character(len=128) :: srcfile, dstfile
      character(len=128) :: title, norm, map_method, conventions

#ifdef ESMF_NETCDF
      ! write out the indices and weights table sequentially to the output file
      ! first find out the starting index of my portion of table
      ! Global reduce
      localCount(1)=size(weights,1)
      allocate(allCounts(PetCnt))
      call ESMF_VMAllGather(vm,localCount,allCounts,1,rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) goto 90

      ! calculate the size of the global weight table
      total = 0
      do i=1,PetCnt
	 total=allCounts(i)+total
      end do

     !The file is created at PET 0 and only PET0 needs to provide the Src/Dst vertex coords and/or masks.
      if (PetNo == 0) then
         if (.not. (present(SrcVertexCoords))) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
            "- SrcVertexCoords has to be present in PET 0", ESMF_CONTEXT, status)
            return
         endif
         if ((.not. present(coordX)) .or. (.not. present(coordY))) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
            "- DstVertexCoords has to be present in PET 0", ESMF_CONTEXT, status)
            return
         endif
        
	if (trim(revflag) .ne. 'rev') then  
           srcDim = size(SrcVertexCoords,2)
           dstDim = size(coordX,1)
	   srcfile = file1
           dstfile = file2
        else
           dstDim = size(SrcVertexCoords,2)
           srcDim = size(coordX,1)
	   dstfile = file1
           srcfile = file2
        end if
        ! Create output file and create dimensions and variables
         ncStatus = nf90_create(trim(filename), NF90_CLOBBER, ncid)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         
         ! global variables
         title = "ESMF Offline Bilinear Remapping"
         norm = "destarea"
         map_method = "Bilinear remapping"
         conventions = "NCAR-CSM"

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "title", trim(title))
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "normalization", trim(norm))
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "map_method", trim(map_method))
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "conventions", trim(conventions))
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

	 ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "domain_a", trim(srcfile))
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "domain_b", trim(dstfile))
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "grid_file_src", trim(srcfile))
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "grid_file_dst", trim(dstfile))
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus = nf90_put_att(ncid, NF90_GLOBAL, "CVS_revision", ESMF_VERSION_STRING)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! define dimensions
         ncStatus = nf90_def_dim(ncid,"n_a",srcDim, naDimId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus = nf90_def_dim(ncid,"n_b",dstDim, nbDimId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90


         ncStatus = nf90_def_dim(ncid,"n_s",total, nsDimId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! define max number of vertices
         ncStatus = nf90_def_dim(ncid,"nv_a",4, nvaDimId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus = nf90_def_dim(ncid,"nv_b",4, nvbDimId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! define max number of vertices
         ncStatus = nf90_def_dim(ncid,"num_wgts",1, VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ! define grid ranks
         if (trim(revflag) .ne. 'rev') then
            ncStatus = nf90_def_dim(ncid,"src_grid_rank",1, srankDimId)
            if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
            ncStatus = nf90_def_dim(ncid,"dst_grid_rank",2, drankDimId)
            if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         else
            ncStatus = nf90_def_dim(ncid,"src_grid_rank",2, srankDimId)
            if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
            ncStatus = nf90_def_dim(ncid,"dst_grid_rank",1, drankDimId)
            if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         end if
        ! define variables

         ! Grid Dims
         ncStatus = nf90_def_var(ncid,"src_grid_dims",NF90_INT, (/srankDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "degrees")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus = nf90_def_var(ncid,"dst_grid_dims",NF90_INT, (/drankDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "degrees")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! yc_a: source vertex coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yc_a",NF90_DOUBLE, (/naDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "degrees")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! yc_b: destination vertex coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yc_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "degrees")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! xc_a: source vertex coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xc_a",NF90_DOUBLE, (/naDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "degrees")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! xc_b: dest. vertex coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xc_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "degrees")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! yv_a: source corner coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yv_a",NF90_DOUBLE, (/nvaDimId,naDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "degrees")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! xv_a: source corner coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xv_a",NF90_DOUBLE, (/nvaDimId,naDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "degrees")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! yv_b: source corner coordinate (latitude)
         ncStatus = nf90_def_var(ncid,"yv_b",NF90_DOUBLE, (/nvbDimId,nbDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "degrees")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! xv_b: source corner coordinate (longitude)
         ncStatus = nf90_def_var(ncid,"xv_b",NF90_DOUBLE, (/nvbDimId,nbDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "degrees")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! mask_a
         ncStatus = nf90_def_var(ncid,"mask_a",NF90_INT, (/naDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "unitless")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! mask_b
         ncStatus = nf90_def_var(ncid,"mask_b",NF90_INT, (/nbDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "degrees")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! area_a
         ncStatus = nf90_def_var(ncid,"area_a",NF90_DOUBLE, (/naDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "square radians")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! area_b
         ncStatus = nf90_def_var(ncid,"area_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "square radians")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! frac_a
         ncStatus = nf90_def_var(ncid,"frac_a",NF90_DOUBLE, (/naDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "unitless")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! frac_b
         ncStatus = nf90_def_var(ncid,"frac_b",NF90_DOUBLE, (/nbDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         ncStatus = nf90_put_att(ncid, VarId, "units", "unitless")
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90


        ! col: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"col",NF90_INT, (/nsDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! row: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"row",NF90_INT, (/nsDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

        ! S: sparse matrix weight table
         ncStatus = nf90_def_var(ncid,"S",NF90_DOUBLE, (/nsDimId/),  VarId)
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus=nf90_enddef(ncid) 
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         if (trim(revflag) .ne. 'rev') then
           ! Write the vertex information and rank information
           ncStatus=nf90_inq_varid(ncid,"src_grid_dims",VarId)
           ncStatus=nf90_put_var(ncid,VarId,1)          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

           ncStatus=nf90_inq_varid(ncid,"dst_grid_dims",VarId)
           ncStatus=nf90_put_var(ncid,VarId, (/xdim, ydim/))          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
	
           ! Write xc_a, xc_b, yc_a, yc_b
           ! get the longitude from SrcVertexCoords
           allocate(coords(srcDim))         
	   do i=1,srcDim
	      coords(i)=SrcVertexCoords(1,i)
           end do
           ncStatus=nf90_inq_varid(ncid,"xc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, coords)          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

           ! get the latitude from SrcVertexCoords
	   do i=1,srcDim
	      coords(i)=SrcVertexCoords(2,i)
           end do
           ncStatus=nf90_inq_varid(ncid,"yc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, coords)          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
           deallocate(coords)
         
	   ! get the longitude from DstVertexCoords
           ncStatus=nf90_inq_varid(ncid,"xc_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, coordX)          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

           ! get the latitude from DstVertexCoords
           ncStatus=nf90_inq_varid(ncid,"yc_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, coordY)          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         else
          ! Write the vertex information and rank information
           ncStatus=nf90_inq_varid(ncid,"dst_grid_dims",VarId)
           ncStatus=nf90_put_var(ncid,VarId,1)          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

           ncStatus=nf90_inq_varid(ncid,"src_grid_dims",VarId)
           ncStatus=nf90_put_var(ncid,VarId, (/xdim, ydim/))          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
	
           ! Write xc_a, xc_b, yc_a, yc_b
           ! get the longitude from SrcVertexCoords
           allocate(coords(dstDim))         
	   do i=1,dstDim
	      coords(i)=SrcVertexCoords(1,i)
           end do
           ncStatus=nf90_inq_varid(ncid,"xc_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, coords)          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

           ! get the latitude from SrcVertexCoords
	   do i=1,dstDim
	      coords(i)=SrcVertexCoords(2,i)
           end do
           ncStatus=nf90_inq_varid(ncid,"yc_b",VarId)
           ncStatus=nf90_put_var(ncid,VarId, coords)          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
           deallocate(coords)
         
	   ! get the longitude from DstVertexCoords
           ncStatus=nf90_inq_varid(ncid,"xc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, coordX)          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

           ! get the latitude from DstVertexCoords
           ncStatus=nf90_inq_varid(ncid,"yc_a",VarId)
           ncStatus=nf90_put_var(ncid,VarId, coordY)          
           if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         end if
 
         ! Write xv_a, yv_a
         ! Just set these to 0.0, because they don't seem to be used
         allocate(cnr_coords(4,srcDim))         
         cnr_coords=0.0
         ncStatus=nf90_inq_varid(ncid,"yv_a",VarId)
         ncStatus=nf90_put_var(ncid,VarId, cnr_coords)          
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus=nf90_inq_varid(ncid,"xv_a",VarId)
         ncStatus=nf90_put_var(ncid,VarId, cnr_coords)          
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         deallocate(cnr_coords)


         ! Write xv_b, yv_b
         ! Just set these to 0.0, because they don't seem to be used
         allocate(cnr_coords(4,dstDim))         
         cnr_coords=0.0
         ncStatus=nf90_inq_varid(ncid,"yv_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, cnr_coords)          
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

         ncStatus=nf90_inq_varid(ncid,"xv_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, cnr_coords)          
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         deallocate(cnr_coords)

         ! Write mask_a
         ! Just set these to 0.0, because we don't support masks here yet
         allocate(mask(srcDim))         
         mask=1
         ncStatus=nf90_inq_varid(ncid,"mask_a",VarId)
         ncStatus=nf90_put_var(ncid,VarId, mask)          
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         deallocate(mask)

         ! Write mask_b
         ! Just set these to 0.0, because we don't support masks here yet
         allocate(mask(dstDim))         
         mask=1
         ncStatus=nf90_inq_varid(ncid,"mask_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, mask)          
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         deallocate(mask)

         ! Write area_a
         ! Just set these to 1.0, because not testing conservative yet
         allocate(area(srcDim))         
         area=0.0
         ncStatus=nf90_inq_varid(ncid,"area_a",VarId)
         ncStatus=nf90_put_var(ncid,VarId, area)          
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         deallocate(area)

         ! Write area_b
         ! Just set these to 1.0, because not testing conservative yet
         allocate(area(dstDim))         
         area=0.0
         ncStatus=nf90_inq_varid(ncid,"area_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, area)          
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         deallocate(area)


         ! Write frac_a
         allocate(frac(srcDim))         
         frac=1.0
         ncStatus=nf90_inq_varid(ncid,"frac_a",VarId)
         ncStatus=nf90_put_var(ncid,VarId, frac)          
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         deallocate(frac)

         ! Write frac_b
         allocate(frac(dstDim))         
         frac=1.0
         ncStatus=nf90_inq_varid(ncid,"frac_b",VarId)
         ncStatus=nf90_put_var(ncid,VarId, frac)          
         if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
         deallocate(frac)

    end if

    ! Block all other PETs until the NetCDF file has been created
    call ESMF_VMBarrier(vm)

    ! find the max of allCounts(i) and allocate colrow
    maxcount=0
    do i=1,PetCnt
	if (allCounts(i) > maxcount) maxcount = allCounts(i)
    enddo

    if (PetNo == 0) then 
	! First write out its own weight and indices, then receive the data from other PETs and write them out
	start = 1
        ! allocate indexbuf and weightbuf to receive data from other PETs
        allocate(indexbuf(maxcount*2), weightbuf(maxcount))
        do i=1, PetCnt
          ! write the local weights and indices first
          localCount(1)=allCounts(i)
 	  if (i==1) then 
  	    !do j=1,localCount(1)
            !    indexbuf(j) = indicies(j,1)
            !enddo
            next => indicies(:,1)
            ncStatus=nf90_inq_varid(ncid,"col",VarId)
   	    ncStatus=nf90_put_var(ncid,VarId, next,(/start/),localCount)          
            if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
            !do j=1,localCount(1)
            !  indexbuf(j) = indicies(j,2)
            !enddo
            next => indicies(:,2)
            ncStatus=nf90_inq_varid(ncid,"row",VarId)
   	    ncStatus=nf90_put_var(ncid,VarId, next,(/start/),localCount)          
            if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

            ncStatus=nf90_inq_varid(ncid,"S",VarId)
            ncStatus=nf90_put_var(ncid,VarId, weights, (/start/),localCount)
            if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
	  else 
            ! receive the weight and indices
            call ESMF_VMRecv(vm, indexbuf, localCount(1)*2, i-1, rc=status)
 	    if (status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
            call ESMF_VMRecv(vm, weightbuf, localCount(1), i-1, rc=status)
 	    if (status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

            ncStatus=nf90_inq_varid(ncid,"col",VarId)
     	    ncStatus=nf90_put_var(ncid,VarId, indexbuf,(/start/),localCount)          
            if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
            next => indexbuf(localCount(1)+1:localCount(1)*2)
            ncStatus=nf90_inq_varid(ncid,"row",VarId)
     	    ncStatus=nf90_put_var(ncid,VarId, next ,(/start/),localCount)          
            if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90

            ncStatus=nf90_inq_varid(ncid,"S",VarId)
            ncStatus=nf90_put_var(ncid,VarId, weightbuf, (/start/),localCount)
            if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
          end if
          start = start + localCount(1)
       end do
    else
       allocate(indexbuf(localcount(1)*2))
       do j=1,localCount(1)
           indexbuf(j) = indicies(j,1)
           indexbuf(j+localCount(1)) = indicies(j,2)
       enddo
       ! a non-root PET, send the results to PET 0
        call ESMF_VMSend(vm, indexbuf, localCount(1)*2, 0, rc=status)
	if (status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
        call ESMF_VMSend(vm, weights, localCount(1), 0, rc=status)
	if (status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
        ncStatus = nf_Noerror
    end if
       
    call ESMF_VMBarrier(vm)
    
    if (PetNo == 0) then
       ncStatus = nf90_close(ncid)                        
       if (CDFCheckError (ncStatus, ESMF_METHOD, ESMF_SRCLINE, checkpoint)) goto 90
       deallocate(weightbuf)
    end if
    deallocate(indexbuf)

90  continue

    ! return ESMF compatible status
    if (ncStatus .eq. nf_Noerror) then
        rc = ESMF_SUCCESS
    else
        rc = ESMF_FAILURE
    end if

#else
    rc = ESMF_RC_LIB_NOT_PRESENT
#endif

    return
end subroutine OutputWeightFile

end program ESMF_CubedSphereRegridEx
