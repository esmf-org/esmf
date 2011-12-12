! $Id: ESMF_IOUGrid.F90,v 1.3 2011/12/12 18:49:54 peggyli Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_IOUGrid.F90"
!
!     ESMF IOUGrid Module
      module ESMF_IOUGridMod
!
!==============================================================================
!
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_IOUGridMod - Grid IO utility class
!
! !DESCRIPTION:
!
! The code in this file reads the UGrid files based on the proposed CF unstructured grid standard
! at http://public.deltares.nl/display/NETCDF/Deltares+CF+proposal+for+Unstructured+Grid+data+model
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod   
      use ESMF_UtilMod
      use ESMF_InitMacrosMod    ! ESMF initializer macros
      use ESMF_LogErrMod        ! ESMF error handling
      use ESMF_VMMod
#ifdef ESMF_NETCDF
      use netcdf
#endif

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE:
      private
      integer, SAVE :: PetNo, PetCnt
      type(ESMF_VM), SAVE:: vm
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
  public ESMF_UGridInq
  public ESMF_UGridGetVar
  public ESMF_GetMeshFromUGridFile
 
!==============================================================================

     contains
!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UGridInq"
!BOPI
! !ROUTINE: ESMF_UGridInq: Return the dimension information
!  information from a ESMF Unstructured grid file
!
! !INTERFACE:
subroutine ESMF_UGridInq(filename, meshname, nodeCount, elementCount, &
	      		maxNodePElement, units, fillvalue, rc)    

! !ARGUMENTS:

    character(len=*), intent(in)   :: filename
    character(len=*), intent(in)   :: meshname
    integer, intent(out), optional :: nodeCount
    integer, intent(out), optional :: elementCount
    integer, intent(out), optional :: maxNodePElement
    character(len=*), intent(out), optional :: units
    integer, intent(out), optional :: fillvalue
    integer, intent(out), optional :: rc

    integer:: localrc, ncStatus
    integer :: DimIds(2), VarId
    integer :: ncid, local_rank, len
    character(len=256):: errmsg, nodeCoordString, nodeCoordNames(2), elmtConnName
    integer :: meshId, pos
     

#ifdef ESMF_NETCDF
    if (present(rc)) rc=ESMF_SUCCESS
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE, trim(filename), rc)) return

    ! get the dummy variable meshname which contains the topology data as attributes
    ncStatus = nf90_inq_varid (ncid, trim(meshname), meshId)
    errmsg = "Dummy Variable "//trim(meshname)//" does not exist in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(meshname), &
      rc)) return

    ! get number of nodes
    if (present(nodeCount) .or. present(units)) then
      ncStatus = nf90_get_att (ncid, meshId, "node_coordinates", nodeCoordString)
      errmsg = "Attribute node_coordinates in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      pos = index(nodeCoordString(1:)," ")
      nodeCoordNames(1) = nodeCoordString(1:pos-1)
      nodeCoordNames(2)=nodeCoordString(pos+1:)

      ! Get dimension (# nodes) used to define the node coordinates
      errmsg = "Variable "//trim(nodeCoordNames(1))//" in "//trim(filename)
      ncStatus = nf90_inq_varid (ncid, nodeCoordNames(1), VarId)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      if (present(nodeCount)) then
        ncStatus = nf90_inquire_variable (ncid, VarId, dimids=DimIds)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
        ncStatus = nf90_inquire_dimension (ncid, DimIds(1), len=nodeCount)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
      endif
      if (present(units)) then
        ! get the attribute 'units'
        ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
        errmsg = "Attribute units for "//nodeCoordNames(1)//" in "//trim(filename)
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
        call ESMF_StringLowerCase(units(1:len))
	units = units(1:7)
      endif
    end if

    if (present(elementCount) .or. present(maxNodePElement) .or. present(fillvalue)) then
      errmsg = "Attribute face_node_connectivity in "//trim(filename)
      ncStatus = nf90_get_att (ncid, meshId, "face_node_connectivity", values=elmtConnName)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ! Get element connectivity 
      errmsg = "Variable "//trim(elmtConnName)//" in "//trim(filename)
      ncStatus = nf90_inq_varid (ncid, trim(elmtConnName), VarId)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ! Get dimensions of element connectivity 
      errmsg = "Dimensions of "//trim(elmtConnName)//" in "//trim(filename)
      ncStatus = nf90_inquire_variable (ncid, VarId, dimids=DimIds)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      if (present(elementCount)) then	
        ncStatus = nf90_inquire_dimension (ncid, DimIds(2), len=elementCount)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
      endif
      if (present(maxNodePElement)) then
        ncStatus = nf90_inquire_dimension (ncid, DimIds(1), len=maxNodePElement)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
      endif
      if (present(fillvalue)) then
        ! Get elmt conn fill value (if it is mixed topology with different number of node
        ! per element.)
        errmsg = "Attribute "//trim(elmtConnName)//"_FillValue in "//trim(filename)
        ncStatus = nf90_get_att (ncid, VarId, "_FillValue", values=fillvalue)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
      endif
   endif

   ncStatus = nf90_close(ncid)
   if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
#endif

    return
end subroutine ESMF_UGridInq

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UGridGetVar"
!BOPI
! !ROUTINE: ESMF_UGridGetVar
!
subroutine ESMF_UGridGetVar (filename, meshname, &
		nodeXcoords, nodeYcoords, &
		faceXcoords, faceYcoords, &
		faceNodeConnX, faceNodeConnY, rc) 
! !INTERFACE:
    character(len=*), intent(in)   :: filename
    character(len=*), intent(in)   :: meshname
    real(ESMF_KIND_R8), optional, pointer :: nodeXcoords(:), nodeYcoords(:)
    real(ESMF_KIND_R8), optional, pointer :: faceXcoords(:), faceYcoords(:)
    real(ESMF_KIND_R8), optional, pointer :: faceNodeConnX(:,:),faceNodeConnY(:,:)
    integer, intent(out), optional :: rc
    
    integer :: i,j,dim1,dim2
    integer, allocatable :: elemConn(:,:)
    integer:: localrc, ncStatus
    integer :: VarId, meshId, pos
    integer :: ncid, local_rank
    integer :: localFillValue, indexBase, offset
    real(ESMF_KIND_R8), pointer :: nodeXcoordsLocal(:), nodeYcoordsLocal(:)
    integer :: dimIds(1), nodeDim
    character(len=256):: errmsg, nodeCoordString, nodeCoordNames(2), elmtConnName
    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    if (present(rc)) rc=ESMF_SUCCESS
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE, trim(filename), rc)) return

    ! get the dummy variable meshname which contains the topology data as attributes
    ncStatus = nf90_inq_varid (ncid, trim(meshname), meshId)
    errmsg = "Dummy Variable "//trim(meshname)//" does not exist in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(meshname), &
      rc)) return

    ! get number of nodes
    if (present(nodeXcoords) .or. present(nodeYcoords) .or. present(faceNodeConnX)) then
      ncStatus = nf90_get_att (ncid, meshId, "node_coordinates", nodeCoordString)
      errmsg = "Attribute node_coordinates in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      pos = index(nodeCoordString(1:)," ")
      nodeCoordNames(1) = nodeCoordString(1:pos-1)
      nodeCoordNames(2)=nodeCoordString(pos+1:)

      errmsg = "Variable "//trim(nodeCoordNames(1))//" in "//trim(filename)
      ncStatus = nf90_inq_varid (ncid, nodeCoordNames(1), VarId)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return

      if (.not. present(nodeXcoords) .and. .not. present(nodeYcoords)) then
        ! find out the node dimension and allocate local nodeCoord arrays
        ncStatus = nf90_inquire_variable(ncid, VarId, dimids=dimIds)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
        ncStatus = nf90_inquire_dimension (ncid, DimIds(1), len=nodeDim)
        errmsg = "Dimension in "//trim(filename)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
	allocate(nodeXcoordsLocal(nodeDim), nodeYcoordsLocal(nodeDim))
      else if (present(nodeXcoords)) then
	nodeDim = size(nodeXcoords,1)
	allocate(nodeYcoordsLocal(nodeDim))
	nodeXcoordsLocal = nodeXcoords
      else if (present(nodeYcoords)) then
	nodeDim = size(nodeYcoords,1)
	allocate(nodeXcoordsLocal(nodeDim))
	nodeYcoordsLocal = nodeYcoords
      else
	nodeXcoordsLocal = nodeXcoords
	nodeYcoordsLocal = nodeYcoords
      endif
      ncStatus = nf90_get_var (ncid, VarId, nodeXcoordsLocal)  
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      errmsg = "Variable "//trim(nodeCoordNames(2))//" in "//trim(filename)
      ncStatus = nf90_inq_varid (ncid, nodeCoordNames(2), VarId)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, nodeYcoordsLocal)  
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
    endif

    ! get number of face
    if (present(faceXcoords) .or. present(faceYcoords)) then
      ncStatus = nf90_get_att (ncid, meshId, "face_coordinates", nodeCoordString)
      errmsg = "Attribute face_coordinates in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      pos = index(nodeCoordString(1:)," ")
      nodeCoordNames(1) = nodeCoordString(1:pos-1)
      nodeCoordNames(2)=nodeCoordString(pos+1:)

      errmsg = "Variable "//trim(nodeCoordNames(1))//" in "//trim(filename)
      ncStatus = nf90_inq_varid (ncid, nodeCoordNames(1), VarId)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      if (present(faceXcoords)) then
        ncStatus = nf90_get_var (ncid, VarId, faceXcoords)  
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
      endif
      if (present(faceYcoords)) then
        errmsg = "Variable "//trim(nodeCoordNames(2))//" in "//trim(filename)
        ncStatus = nf90_inq_varid (ncid, nodeCoordNames(2), VarId)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
        ncStatus = nf90_get_var (ncid, VarId, faceYcoords)  
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
      endif
    endif
    
    ! faceNodeConnX and faceNodeConnY are the node coordinates for a given cell (element).
    ! In the UGRID standard, face_node_connectivity variable contains the indices
    ! to the nodes, rather than the actually coodinates.  This is used to write the weight
    ! file in the SCRIP format.
    if (present(faceNodeConnX)) then 
        errmsg = "Attribute face_node_connectivity in "//trim(filename)
        ncStatus = nf90_get_att (ncid, meshId, "face_node_connectivity", values=elmtConnName)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
        ! Get element connectivity
        errmsg = "Variable "//trim(elmtConnName)//" in "//trim(filename)
        ncStatus = nf90_inq_varid (ncid, trim(elmtConnName), VarId)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
        ! Get elmt conn fill value
        errmsg = "Attribute "//trim(elmtConnName)//"_FillValue in "//trim(filename)
        ncStatus = nf90_get_att (ncid, VarId, "_FillValue", values=localFillValue)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
        ! Get start_index attribute to find out the index base (0 or 1)
        ncStatus = nf90_get_att (ncid, VarId, "start_index", values=indexBase)
        ! if not defined, default to 0-based
        if (ncStatus /= nf90_noerror) indexBase = 0

        dim1 = size(faceNodeConnX,1)
        dim2 = size(faceNodeConnX,2)
        allocate(elemConn(dim1,dim2) )
        ncStatus = nf90_get_var (ncid, VarId, elemConn)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
	faceNodeConnX(:,:)=localFillValue
	faceNodeConnY(:,:)=localFillValue
        !adjust index to 1-based of the start_index is 0
        if (indexBase == 1) then
	   offset = 0
        else
           offset = 1
        endif
        do i=1,dim1
	  do j=1,dim2
		if (elemConn(i,j) /= localFillValue) then
		   faceNodeConnX(i,j) = nodeXcoordsLocal(elemConn(i,j)+offset)
		   faceNodeConnY(i,j) = nodeYcoordsLocal(elemConn(i,j)+offset)
	        endif
          enddo
        enddo
        deallocate(elemConn)
	if (.not. present(nodeXcoords)) then
	  deallocate(nodeXcoordsLocal, nodeYcoordsLocal)
	endif
    endif
    ncStatus = nf90_close(ncid)
    
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
#endif

    return
    end subroutine ESMF_UGridGetVar
!---------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GetMeshFromUGridFile"
subroutine ESMF_GetMeshFromUGridFile (filename, meshname, nodeCoords, elmtConn, &
                                elmtNums, startElmt, convertToDeg, rc)

    character(len=*), intent(in)   :: filename
    character(len=*), intent(in)   :: meshname
    real(ESMF_KIND_R8), pointer    :: nodeCoords (:,:)
    integer(ESMF_KIND_I4), pointer :: elmtConn (:,:)
    integer(ESMF_KIND_I4), pointer :: elmtNums (:)
    integer,           intent(out) :: startElmt
    logical, intent(in), optional  :: convertToDeg
    integer, intent(out), optional :: rc

    integer(ESMF_KIND_I4), allocatable :: elmtConnT(:,:)
    integer :: ncid, meshId, DimIds(2), VarId
    integer :: ncStatus

    integer :: coordinateDims(3), coordDim, meshDim
    integer :: i, j, count, nodeCount, MaxNodePerElmt, localFillValue
    integer :: localCount, remain, elmtCount

    character(len=256) :: errmsg, locations, locNames(3), elmtConnName
    character(len=256) :: nodeCoordString
    character(len=80), allocatable :: nodeCoordNames(:)
    integer :: pos0, pos, pos1, pos2, n, yesNode
    integer :: len, indexBase
    character(len=24) :: units
    real(ESMF_KIND_R8) :: rad2deg
    logical :: convertToDegLocal
    integer, parameter :: nf90_noerror = 0
    real(ESMF_KIND_R8), allocatable:: nodeCoord1D(:)
    
#ifdef ESMF_NETCDF
    convertToDegLocal = .false.
    if (present(convertToDeg)) convertToDegLocal = convertToDeg

    ! Get VM information
    call ESMF_VMGetGlobal(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    ! get the dummy variable meshname which contains the topology data as attributes
    ncStatus = nf90_inq_varid (ncid, trim(meshname), meshId)
    errmsg = "Dummy Variable "//trim(meshname)//" does not exist in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(meshname), &
      rc)) return

    ! Get mesh dimension
    ncStatus = nf90_get_att (ncid, meshId, "dimension", values=meshDim)
    errmsg = "Attribute dimension in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Currently, only support 2D mesh
    if ( meshDim/=2) then
        call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- Only 2D mesh is supported currently", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif

    ! Not sure if I need this attribute if "node_coordinates" is a required attribute
    ! Get location names
    ncStatus = nf90_inquire_attribute(ncid, meshId, "locations", len=len)
    errmsg = "Attribute locations in "//trim(filename)
    if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
	  errmsg,&
          rc)) return
    ncStatus = nf90_get_att (ncid, meshId, "locations", values=locations)
    errmsg = "Attribute locations in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Break up space-separated string "locations" into individual locNames
    pos1 = 1
    yesNode = 0
    n = 1
    do 
	pos2 = index(locations(pos1:len), " ")
	if (pos2 == 0) then
	   locNames(n) = locations(pos1:len)
  	   if (locNames(n) .eq. "node") yesNode = 1
	   exit
        endif
        locNames(n) = locations(pos1:pos1+pos2-2)
	if (locNames(n) .eq. "node") yesNode = 1
        ! Check supported location names convention? (node/edge/face)
        ! (Later, can general to some support of other forms?)
        if ((locNames(n) .ne. "node") .and. (locNames(n) .ne. "face") .and. &
            (locNames(n) .ne. "edge")) then
            print *, "Error - Must use UGRID location names convention (node/edge/face) ", locNames(n)
	    call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- location attribute is wrong", & 
               	 ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
	pos1 = pos2+pos1
	n = n + 1
    enddo
    ! Check if node exists in the location attribute
    if (yesNode == 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- node coordinates not defined", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif
    ! Get node coordinates
    allocate( nodeCoordNames(2) )
    ncStatus = nf90_get_att (ncid, meshId, "node_coordinates", nodeCoordString)
    errmsg = "Attribute node_coordinates in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Parse the attribute to find the varible names for node_coordinates
    pos = index(nodeCoordString(1:)," ")
    nodeCoordNames(1) = nodeCoordString(1:pos-1)
    nodeCoordNames(2)=nodeCoordString(pos+1:)

    ! Get dimension (# nodes) used to define the node coordinates
    errmsg = "Variable "//trim(nodeCoordNames(1))//" in "//trim(filename)
    ncStatus = nf90_inq_varid (ncid, nodeCoordNames(1), VarId)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ncStatus = nf90_inquire_variable (ncid, VarId, dimids=DimIds)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ncStatus = nf90_inquire_dimension (ncid, DimIds(1), len=nodeCount)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    allocate( nodeCoords(2,nodeCount), nodeCoord1D(nodeCount) )
    do i=1,2
      errmsg = "Variable "//nodeCoordNames(i)//" in "//trim(filename)
      ncStatus = nf90_inq_varid (ncid, nodeCoordNames(i), VarId)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, nodeCoord1D)  
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return

      do j=1,nodeCount
	nodeCoords(i,j)=nodeCoord1d(j)
      enddo

      ! if units is radians_east or radians_north, convert to degrees
      ! get the attribute 'units'
      ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
      errmsg = "Attribute units for "//nodeCoordNames(i)//" in "//trim(filename)
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
      call ESMF_StringLowerCase(units(1:len))
      if (units(1:7) .ne. 'degrees' .and. units(1:7) .ne. 'radians') then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- units attribute is not degrees or radians", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return
      endif
      ! if units is "radians", convert it to degrees
      if (convertToDegLocal) then
         if (units(1:7) .eq. "radians") then
            rad2deg = 180.0/3.141592653589793238
            !print *, 'Convert radians to degree ', rad2deg
            nodeCoords(i,:) = nodeCoords(i,:)*rad2deg
         endif
      endif	   
    
    enddo

    ! Get element connectivity, if it does not exist, bail out
    errmsg = "Attribute face_node_connectivity in "//trim(filename)
    ncStatus = nf90_get_att (ncid, meshId, "face_node_connectivity", values=elmtConnName)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Get element connectivity (UGRID convention is transposed compared to others)
    errmsg = "Variable "//trim(elmtConnName)//" in "//trim(filename)
    ncStatus = nf90_inq_varid (ncid, trim(elmtConnName), VarId)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Get elmt conn fill value (if there are triangles mixed with squares, eg)
    errmsg = "Attribute "//trim(elmtConnName)//" _FillValue in "//trim(filename)
    ncStatus = nf90_get_att (ncid, VarId, "_FillValue", values=localFillValue)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Get start_index attribute to find out the index base (0 or 1)
    ncStatus = nf90_get_att (ncid, VarId, "start_index", values=indexBase)
    ! if not defined, default to 0-based
    if (ncStatus /= nf90_noerror) indexBase = 0
    ! Get dimensions of element connectivity (transposed) (for allocation)
    errmsg = "Dimensions of "//trim(elmtConnName)//" in "//trim(filename)
    ncStatus = nf90_inquire_variable (ncid, VarId, dimids=DimIds)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ncStatus = nf90_inquire_dimension (ncid, DimIds(2), len=elmtCount)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ncStatus = nf90_inquire_dimension (ncid, DimIds(1), len=MaxNodePerElmt)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
 
    ! Decompose the element array evenly across all PETs
    localCount = elmtCount/PetCnt
    remain = mod (elmtCount,PetCnt) 
    startElmt = localCount*PetNo +1
    if (PetNo==PetCnt-1) localCount=localCount+remain
    allocate(elmtConn(MaxNodePerElmt,localCount) )
    allocate( elmtNums(localCount) )
    ! Get element connectivity... transposed
    ncStatus = nf90_get_var (ncid, VarId, elmtConn, start=(/1,startElmt/), &
                            count=(/MaxNodePerElmt,localCount/))
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get the number of nodes for each element
    ! if indexBase is 0, change it to 1 based index
    elmtNums(:)=MaxNodePerElmt
    if (indexBase == 0) then
      do i=1,localcount
        do j=1,MaxNodePerElmt
	  ! change 0-base to 1-base
          if (elmtConn(j,i) /= localFillValue) then 
	     elmtConn(j,i)=elmtConn(j,i)+1
	  else
	     ! find the first FillValue
	     elmtNums(i) = j-1
	     exit
	  endif
         enddo	
      enddo
    else
      do i=1,localcount
        j = MaxNodePerElmt
        do while (elmtConn(j,i) == localFillValue)
	  j = j - 1
        enddo
        elmtNums(i) = j
      enddo
    endif

    ! Deallocations
    deallocate( nodeCoordNames, nodeCoord1D )

    ncStatus = nf90_close (ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine ESMF_GetMeshFromUGridFile

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
        call ESMF_LogWrite (msg="netCDF Status Return Error", logmsgList=ESMF_LOGMSG_ERROR, &
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
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end function CDFCheckError


end module ESMF_IOUGridMod
