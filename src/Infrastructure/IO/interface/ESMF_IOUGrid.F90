! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2013, University Corporation for Atmospheric Research,
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
  public ESMF_UGridGetVarByName
 
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
    integer :: meshId, pos, meshDim
    character(len=80):: varname

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

    ! get dimension
    ncStatus = nf90_get_att (ncid, meshId, "dimension", values=meshDim)
    errmsg = "Attribute dimension in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! get number of nodes
    if (present(nodeCount) .or. present(units)) then
      ncStatus = nf90_inquire_attribute(ncid, meshId, "node_coordinates", len=len)
      errmsg = "Attribute node_coordinates in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ncStatus = nf90_get_att (ncid, meshId, "node_coordinates", nodeCoordString)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      pos = index(nodeCoordString(1:)," ")
      nodeCoordNames(1) = nodeCoordString(1:pos-1)

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
      if (meshDim == 2) then
        varname = "face_node_connectivity"
      else
        varname = "volume_node_connectivity"
      endif    
      ncStatus = nf90_inquire_attribute(ncid, meshId, varname, len=len)
      errmsg = "Attribute face_node_connectivity in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ncStatus = nf90_get_att (ncid, meshId, varname, values=elmtConnName)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ! Get element connectivity 
      errmsg = "Variable "//elmtConnName(1:len)//" in "//trim(filename)
      ncStatus = nf90_inq_varid (ncid, elmtConnName(1:len), VarId)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ! Get dimensions of element connectivity 
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
    integer :: VarId, meshId, meshDim, pos1, pos2, len
    integer :: ncid, local_rank
    integer :: localFillValue, indexBase, offset
    real(ESMF_KIND_R8), pointer :: nodeXcoordsLocal(:), nodeYcoordsLocal(:)
    integer :: dimIds(1), nodeDim
    character(len=256):: errmsg, nodeCoordString, nodeCoordNames(2), elmtConnName
    integer, parameter :: nf90_noerror = 0
    character(len=80) :: varname

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

    ! get dimension
    ncStatus = nf90_get_att (ncid, meshId, "dimension", values=meshDim)
    errmsg = "Attribute dimension in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! get number of nodes
    if (present(nodeXcoords) .or. present(nodeYcoords) .or. present(faceNodeConnX)) then
      ncStatus = nf90_inquire_attribute(ncid, meshId, "node_coordinates", len=len)
      errmsg = "Attribute node_coordinates in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ncStatus = nf90_get_att (ncid, meshId, "node_coordinates", nodeCoordString)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      pos1 = index(nodeCoordString(1:)," ")
      nodeCoordNames(1) = nodeCoordString(1:pos1-1)
      pos2 = index(nodeCoordString(pos1+1:)," ")
      nodeCoordNames(2)=nodeCoordString(pos1+1:pos1+pos2-1)

      errmsg = "Variable "//trim(nodeCoordNames(1))//" in "//trim(filename)
      ncStatus = nf90_inq_varid (ncid, nodeCoordNames(1), VarId)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return

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
      if (present(nodeXcoords)) nodeXcoords = nodeXcoordsLocal
      if (present(nodeYcoords)) nodeYcoords = nodeYcoordsLocal

      ! faceNodeConnX and faceNodeConnY are the node coordinates for a given cell (element).
      ! In the UGRID standard, face_node_connectivity variable contains the indices
      ! to the nodes, rather than the actually coodinates.  This is used to write the weight
      ! file in the SCRIP format.
      if (meshDim == 2) then 
        varname = "face_node_connectivity"
      else
        varname = "volume_node_connectivity"
      endif    
      if (present(faceNodeConnX)) then 
        errmsg = "Attribute "//trim(varname)//" in "//trim(filename)
        ncStatus = nf90_inquire_attribute(ncid, meshId, trim(varname), len=len)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
        ncStatus = nf90_get_att (ncid, meshId, trim(varname), values=elmtConnName)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
        ! Get element connectivity
        errmsg = "Variable "//elmtConnName(1:len)//" in "//trim(filename)
        ncStatus = nf90_inq_varid (ncid, elmtConnName(1:len), VarId)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
        ! Get elmt conn fill value
        errmsg = "Attribute "//elmtConnName(1:len)//"_FillValue in "//trim(filename)
        ncStatus = nf90_get_att (ncid, VarId, "_FillValue", values=localFillValue)
	if (ncStatus /= nf90_noerror) localFillValue = -1
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
      endif
      deallocate(nodeXcoordsLocal, nodeYcoordsLocal)
    endif

    ! get number of face
    if (present(faceXcoords) .or. present(faceYcoords)) then
      ncStatus = nf90_inquire_attribute(ncid, meshId, "face_coordinates", len=len)
      errmsg = "Attribute face_coordinates in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ncStatus = nf90_get_att (ncid, meshId, "face_coordinates", nodeCoordString)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      pos1 = index(nodeCoordString(1:)," ")
      nodeCoordNames(1) = nodeCoordString(1:pos1-1)
      pos2 = index(nodeCoordString(pos1+1:)," ")
      nodeCoordNames(2)=nodeCoordString(pos1+1:pos1+pos2-1)

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
#define ESMF_METHOD "ESMF_UGridGetVarByName"
subroutine ESMF_UGridGetVarByName (filename, varname, varbuffer, &
	                           startind, count, location, missingvalue, rc)

    character(len=*), intent(in)   :: filename
    character(len=*), intent(in)   :: varname
    real(ESMF_KIND_R8), pointer    :: varbuffer (:)
    integer, intent(in), optional  :: startind
    integer, intent(in), optional  :: count
    character(len=*), intent(in), optional:: location
    real(ESMF_KIND_R8), intent(out), optional:: missingvalue
    integer                       :: rc

    integer   :: ncid, VarId
    integer   :: ncStatus
    character(len=256) :: errmsg
    character(len=80) :: attstr
    integer   :: ndims, dimids(3), dimsize, length
    integer, pointer   :: starts(:), counts(:)
    integer, parameter :: nf90_noerror=0

#ifdef ESMF_NETCDF

    if (present(startind)) then
	if (.not. present(count)) then
           call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                msg="- optional argument count is missing", &
		ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
    endif
           
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    errmsg = "Variable "//varname//" in "//trim(filename)
    ncStatus = nf90_inq_varid (ncid, varname, VarId)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! if location is given, check if the location attribute of the variable match with the
    ! input value
    if (present(location)) then
	ncStatus = nf90_get_att(ncid, VarId, "location", attstr)
        if (CDFCheckError (ncStatus, &
           ESMF_METHOD,  &
           ESMF_SRCLINE, errmsg, &
           rc)) return
	length =len(location)
        if (attstr(1:length) .ne. location) then
	  errmsg = "- location attribute does not match with input location "//location
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg=errmsg, ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
    endif

    ! check if the variable dimension matches with the allocated array
    errmsg = "Variable "//varname//" in "//trim(filename)
    ncStatus = nf90_inquire_variable(ncid, VarId, ndims=ndims, dimids=dimids)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    
    ! get the first dimension length of the variable
    ncStatus = nf90_inquire_dimension (ncid, dimids(1), len=dimsize)
    if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg,&
            rc)) return

    if (present(startind)) then
      if (size(varbuffer) < count) then
	call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
	       msg="- the varbuffer is too small", &
	       ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    else if (dimsize /= size(varbuffer)) then
	call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
	       msg="- the variable array dimension does not match with dimension length", &
	       ESMF_CONTEXT, rcToReturn=rc)
        return
    endif

    allocate(starts(ndims), counts(ndims))
    starts(:)=1
    counts(:)=1
    counts(1)=dimsize
    if (present(startind)) then
        starts(1)=startind
        counts(1)=count
    endif
    ncStatus = nf90_get_var (ncid, VarId, varbuffer, start=starts, count=counts)  
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    deallocate(starts, counts)

    ! get missisng value 
    if (present(missingvalue)) then
      ncStatus = nf90_get_att(ncid, VarId, "_FillValue", missingvalue)
      if (ncStatus /= nf90_noerror) then
	  ncStatus = nf90_get_att(ncid, varid, "missing_value", missingvalue)
          errmsg = "missing value attribute does not exist for "//trim(varname)
          if (CDFCheckError (ncStatus, &
            ESMF_METHOD, &
            ESMF_SRCLINE,&
            errmsg, &
            rc)) return
      end if
    end if  

    ncStatus = nf90_close (ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return
    rc = ESMF_SUCCESS
    return
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif

end subroutine ESMF_UGridGetVarByName

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

    integer :: ncid, meshId
    integer :: ncStatus
    integer :: meshDim
    character(len=256) :: errmsg
    character(len=24) :: attbuf
    integer :: len
    logical :: convertToDegLocal
    
#ifdef ESMF_NETCDF
    convertToDegLocal = .false.
    if (present(convertToDeg)) convertToDegLocal = convertToDeg

    ! Get VM information
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

    ! get the dummy variable meshname which contains the topology data as attributes
    ncStatus = nf90_inq_varid (ncid, trim(meshname), meshId)
    errmsg = "Dummy Variable "//trim(meshname)//" does not exist in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(meshname), &
      rc)) return

    ! Check if cf_role attribute is set
    ncStatus = nf90_get_att (ncid, meshId, "cf_role", values=attbuf)
    errmsg = "Attribute cf_role in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    if (attbuf(1:13) .ne. 'mesh_topology') then
      call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- cf_role attribute is not mesh_topology", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    ! Get mesh dimension
    ncStatus = nf90_get_att (ncid, meshId, "dimension", values=meshDim)
    errmsg = "Attribute dimension in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Currently, only support 2D mesh
    if (meshDim == 2) then
       call ESMF_GetMesh2DFromUGrid (filename, ncid, meshId, nodeCoords, elmtConn, &
                                elmtNums, startElmt, convertToDegLocal, rc)
    elseif (meshDim == 3) then
       call ESMF_GetMesh3DFromUGrid (filename, ncid, meshId, nodeCoords, elmtConn, &
                                elmtNums, startElmt, rc)
    else
        call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- Only 2D or 3D mesh is supported currently", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif
    return

#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
    return
#endif
end subroutine ESMF_GetMeshFromUGridFile

!---------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GetMesh2DFromUGrid"
subroutine ESMF_GetMesh2DFromUGrid (filename, ncid, meshid, nodeCoords, elmtConn, &
                                elmtNums, startElmt, convertToDeg, rc)

    character(len=*), intent(in)   :: filename
    integer,           intent(in)  :: ncid, meshid				
    real(ESMF_KIND_R8), pointer    :: nodeCoords (:,:)
    integer(ESMF_KIND_I4), pointer :: elmtConn (:,:)
    integer(ESMF_KIND_I4), pointer :: elmtNums (:)
    integer,           intent(out) :: startElmt
    logical, intent(in), optional  :: convertToDeg
    integer, intent(out), optional :: rc

    integer(ESMF_KIND_I4), allocatable :: elmtConnT(:,:)
    integer :: DimIds(2), VarId
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

    ! Get node coordinates
    ncStatus = nf90_inquire_attribute(ncid, meshId, "node_coordinates", len=len)
    errmsg = "Attribute node_coordinates in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
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
    nodeCoordNames(2)=nodeCoordString(pos+1:len)

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
    ncStatus = nf90_inquire_attribute(ncid, meshId, "face_node_connectivity", len=len)
    ncStatus = nf90_get_att (ncid, meshId, "face_node_connectivity", values=elmtConnName)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Get element connectivity (UGRID convention is transposed compared to others)
    errmsg = "Variable "//elmtConnName(1:len)//" in "//trim(filename)
    ncStatus = nf90_inq_varid (ncid, elmtConnName(1:len), VarId)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Get elmt conn fill value (if there are triangles mixed with squares, eg)
    ! _FillValue is optional.  It is not needed if all the elements have the same
    ! number of corner nodes
    errmsg = "Attribute "//elmtConnName(1:len)//" _FillValue in "//trim(filename)
    ncStatus = nf90_get_att (ncid, VarId, "_FillValue", values=localFillValue)
    if (ncStatus /= nf90_noerror) localFillValue = 0
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

end subroutine ESMF_GetMesh2DFromUGrid

!---------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GetMesh3DFromUGrid"
subroutine ESMF_GetMesh3DFromUGrid (filename, ncid, meshid, nodeCoords, elmtConn, &
                                elmtNums, startElmt, rc)

    character(len=*), intent(in)   :: filename
    integer,           intent(in)  :: ncid, meshid				
    real(ESMF_KIND_R8), pointer    :: nodeCoords (:,:)
    integer(ESMF_KIND_I4), pointer :: elmtConn (:,:)
    integer(ESMF_KIND_I4), pointer :: elmtNums (:)
    integer,           intent(out) :: startElmt
    integer, intent(out), optional :: rc

    integer(ESMF_KIND_I4), allocatable :: elmtConnT(:,:)
    integer :: DimIds(2), VarId
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
    real(ESMF_KIND_R8) :: deg2rad, earthradius
    real(ESMF_KIND_R8) :: coord(3)
    integer, parameter :: nf90_noerror = 0
    real(ESMF_KIND_R8), allocatable:: nodeCoord1D(:)

#ifdef ESMF_NETCDF

    ! Get node coordinates
    ncStatus = nf90_inquire_attribute(ncid, meshId, "node_coordinates", len=len)
    errmsg = "Attribute node_coordinates in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    allocate( nodeCoordNames(3))
    ncStatus = nf90_get_att (ncid, meshId, "node_coordinates", nodeCoordString)
    errmsg = "Attribute node_coordinates in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Parse the attribute to find the varible names for node_coordinates
    pos1 = index(nodeCoordString(1:)," ")
    nodeCoordNames(1) = nodeCoordString(1:pos1-1)
    pos2 = index(nodeCoordString(pos1+1:)," ")
    nodeCoordNames(2) = nodeCoordString(pos1+1:pos1+pos2-1)
    nodeCoordNames(3)=nodeCoordString(pos1+pos2+1:len)

    ! print *, pos1, pos2, trim(nodeCoordNames(1)), ' ', trim(nodeCoordNames(2)), ' ', trim(nodeCoordNames(3))

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

    allocate( nodeCoords(3,nodeCount), nodeCoord1D(nodeCount) )
    do i=1,3
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

      ! Convert to Cartisian 3D coordinates
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
      if (i==1 .or. i==2) then
        ! if units is not "degrees" or "radians" return errors
        call ESMF_StringLowerCase(units(1:len))
        if (units(1:7) .ne. 'degrees' .and. units(1:7) .ne. 'radians') then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- units attribute is not degrees or radians", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
        ! if units is "degrees", convert it to radians
        if (units(1:7) .eq. "degrees") then
            deg2rad = 3.141592653589793238/180.0
            nodeCoords(i,:) = nodeCoords(i,:)*deg2rad
        endif
      else	   
        ! normalize the height using the earth radius
        if (units(1:len) .eq. "meters") then
	  earthradius = 6371000.0
        else if (units(1:len) .eq. "km" .or. units(1:len) .eq. "kilometers") then
	  earthradius = 6371.0
        else    
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
                 msg="- units attribute for height is not meters, km, or kilometers", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
        nodeCoords(i,:)=1+nodeCoords(i,:)/earthradius
      endif
    enddo

    ! Convert the coordinates into Cartesian 3D
    do i=1,nodeCount
      coord(1)=nodeCoords(3,i)*cos(nodeCoords(1,i))*cos(nodeCoords(2,i))
      coord(2)=nodeCoords(3,i)*sin(nodeCoords(1,i))*cos(nodeCoords(2,i))
      coord(3)=nodeCoords(3,i)*sin(nodeCoords(2,i))
      nodeCoords(:,i)=coord(:)
    enddo

    ! Get element connectivity, if it does not exist, bail out
    errmsg = "Attribute volume_node_connectivity in "//trim(filename)
    ncStatus = nf90_inquire_attribute(ncid, meshId, "volume_node_connectivity", len=len)
    ncStatus = nf90_get_att (ncid, meshId, "volume_node_connectivity", values=elmtConnName)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Get element connectivity (UGRID convention is transposed compared to others)
    errmsg = "Variable "//elmtConnName(1:len)//" in "//trim(filename)
    ncStatus = nf90_inq_varid (ncid, elmtConnName(1:len), VarId)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return
    ! Currently, only hexahedron and tetrahedrons are supported, but
    ! eventually, we want to support prisms (or wedge as used in UGRID)
    errmsg = "Attribute "//elmtConnName(1:len)//" _FillValue in "//trim(filename)
    ncStatus = nf90_get_att (ncid, VarId, "_FillValue", values=localFillValue)
    if (ncStatus /= nf90_noerror) localFillValue = -1
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

end subroutine ESMF_GetMesh3DFromUGrid

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
        call ESMF_LogWrite (msg="netCDF Status Return Error", logmsgFlag=ESMF_LOGMSG_ERROR, &
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
