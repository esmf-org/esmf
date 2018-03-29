! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
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
! !MODULE: ESMF_IOUGridMod - Grid I/O utility class
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
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
  public ESMF_UGridInq
  public ESMF_UGridGetVar
  public ESMF_GetMeshFromUGridFile
  public ESMF_UGridGetVarByName
  public ESMF_UGridGetCoords
#if 1
  public ESMF_GetElemFromUGridFile
  public ESMF_GetNodeFromUGridFile
#endif

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
                        maxNodePElement, units, fillvalue, nodeCoordDim, &
                        faceCoordFlag, meshid, rc)

! !ARGUMENTS:

    character(len=*), intent(in)   :: filename
    character(len=*), intent(in), optional :: meshname
    integer, intent(out), optional :: nodeCount
    integer, intent(out), optional :: elementCount
    integer, intent(out), optional :: maxNodePElement
    character(len=*), intent(out), optional :: units
    integer, intent(out), optional :: fillvalue
    integer, intent(out), optional :: nodeCoordDim
    logical, intent(out), optional :: faceCoordFlag
    integer, intent(out), optional :: meshid
    integer, intent(out), optional :: rc

    integer:: localrc, ncStatus
    integer :: DimIds(2), VarId, dummyId
    integer :: ncid, local_rank, len
    character(len=256):: errmsg, nodeCoordString, nodeCoordNames(2), elmtConnName
    integer :: pos, meshDim
    character(len=80):: varname, attvalue
    integer :: i, nvars

    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    if (present(rc)) rc=ESMF_SUCCESS
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD, &
      ESMF_SRCLINE, trim(filename), rc)) return

    ! get the dummy variable meshname which contains the topology data as attributes
    ! If the meshname is not given, find it in the file using its attribute cf_role or
    ! standard_name
    dummyId = 0
    if (present(meshname)) then
      ncStatus = nf90_inq_varid (ncid, trim(meshname), dummyId)
      errmsg = "Dummy Variable "//trim(meshname)//" does not exist in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
    else
       ! find the mesh name using attribute "cf_role"
       ncStatus = nf90_inquire(ncid, nVariables=nvars)
       errmsg = "inquiry error with "//trim(filename)
       if (CDFCheckError (ncStatus, &
           ESMF_METHOD,  &
           ESMF_SRCLINE, errmsg, &
           rc)) return
       do i=1,nvars
          ncStatus=nf90_get_att(ncid, i, 'cf_role', attvalue)
          if (ncStatus == nf90_noerror) then
               ncStatus = nf90_inquire_attribute(ncid, i, 'cf_role', len=len)
          else
               ncStatus = nf90_get_att (ncid, i, "standard_name", attvalue)
               if (ncStatus == nf90_noerror) then
                  ncStatus = nf90_inquire_attribute(ncid, i, 'standard_name', len=len)
               endif
          endif
          if (ncStatus == nf90_noerror) then
               if (attvalue(len:len) .eq. achar(0)) len = len-1
               if (attvalue(1:len) .eq. 'mesh_topology') then
                 dummyId=i
               endif
          endif
       enddo
    endif
    if (dummyId == 0) then
       ! Mesh variable not found, return error
       errmsg = "- dummy mesh variable not found in "//trim(filename)
       call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
             msg=errmsg, ESMF_CONTEXT, rcToReturn=rc)
       return
    endif

    if (present(meshId)) meshId=dummyId
    ! get dimension
    ! Change to topology_dimension based on the update on 2/28/2013 at
    ! http://publicwiki.deltares.nl/display/NETCDF/Deltares+CF+proposal+for+Unstructured+Grid+data+model
    ! for backward compatibility, use dimension if topology_dimension does not exist

    ncStatus = nf90_get_att (ncid, dummyId, "topology_dimension", values=meshDim)
    if (ncStatus /= nf90_noerror) then
       ncStatus = nf90_get_att (ncid, dummyId, "dimension", values=meshDim)
       errmsg = "Attribute topology_dimension or dimension in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
    endif
    if (present(NodeCoordDim)) NodeCoordDim=meshDim
    ! get number of nodes
    if (present(nodeCount) .or. present(units)) then
      ncStatus = nf90_inquire_attribute(ncid, dummyId, "node_coordinates", len=len)
      errmsg = "Attribute node_coordinates in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ncStatus = nf90_get_att (ncid, dummyId, "node_coordinates", nodeCoordString)
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
        units = ESMF_UtilStringLowerCase(units(1:len))
        if (units(len:len) .eq. achar(0)) len = len-1
        units = units(1:7)
      endif
    end if

    if (present(elementCount) .or. present(maxNodePElement) .or. present(fillvalue)) then
      if (meshDim == 2) then
        varname = "face_node_connectivity"
      else
        varname = "volume_node_connectivity"
      endif
      ncStatus = nf90_inquire_attribute(ncid, dummyId, varname, len=len)
      errmsg = "Attribute face_node_connectivity in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ncStatus = nf90_get_att (ncid, dummyId, varname, values=elmtConnName)
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

   if (present(faceCoordFlag)) then
      ncStatus = nf90_inquire_attribute(ncid, dummyId, "face_coordinates", len=len)
      if (ncStatus /= nf90_noerror) then
          faceCoordFlag = .FALSE.
      else
          faceCoordFlag = .TRUE.
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
subroutine ESMF_UGridGetVar (filename, meshId, &
                nodeXcoords, nodeYcoords, &
                faceXcoords, faceYcoords, &
                faceNodeConnX, faceNodeConnY, rc)
! !INTERFACE:
    character(len=*), intent(in)   :: filename
    integer                        :: meshId
    real(ESMF_KIND_R8), optional, pointer :: nodeXcoords(:), nodeYcoords(:)
    real(ESMF_KIND_R8), optional, pointer :: faceXcoords(:), faceYcoords(:)
    real(ESMF_KIND_R8), optional, pointer :: faceNodeConnX(:,:),faceNodeConnY(:,:)
    integer, intent(out), optional :: rc

    integer :: i,j,dim1,dim2
    integer, allocatable :: elemConn(:,:)
    integer:: localrc, ncStatus
    integer :: VarId, meshDim, pos1, pos2, len
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

    ! get dimension
    ncStatus = nf90_get_att (ncid, meshId, "topology_dimension", values=meshDim)
    if (ncStatus /= nf90_noerror) then
       ncStatus = nf90_get_att (ncid, meshId, "dimension", values=meshDim)
       errmsg = "Attribute topology_dimension or dimension in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
    endif

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
#define ESMF_METHOD "ESMF_UGridInqVarLoc"
subroutine ESMF_UGridInqVarLoc (ncid, VarId, varname,location, rc)

    integer, intent(in) :: ncid
    integer, intent(in) :: VarId
    character(len=*), intent(in) :: varname
    integer, intent(out) :: location ! 1 for node, 2 for face
    integer                        :: rc

    integer   :: meshId
    integer   :: ncStatus
    character(len=256) :: errmsg
    character(len=80) :: attstr, attstr1, locationStr
    integer   :: len,len1
    integer, parameter :: nf90_noerror=0

#ifdef ESMF_NETCDF
    ncStatus = nf90_get_att(ncid, VarId, "location", locationStr)
    if (ncStatus /= nf90_noerror) then
      ! location attribute does not exist, check coordinates attribute
      ! check if the coordinate attribute exist or not
      ncStatus = nf90_inquire_attribute(ncid, VarId, "coordinates", len=len)
      errmsg ="No location or coordinates attributes defined for "//varname
      if (CDFCheckError (ncStatus, &
                         ESMF_METHOD,  &
                         ESMF_SRCLINE, errmsg, &
                         rc)) return
      ncStatus = nf90_get_att(ncid, VarId, "coordinates", attstr)
      if (CDFCheckError (ncStatus, &
                                 ESMF_METHOD,  &
                         ESMF_SRCLINE, errmsg, &
                         rc)) return
      ! check if it matches with the node_coordinates or face_coordinates defined
      ! on the topology variable
      ncStatus = nf90_inquire_attribute(ncid, VarId, "mesh", len=len1)
      errmsg ="No mesh attribute defined for "//varname
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
      ncStatus = nf90_get_att(ncid, VarId, "mesh", attstr1)
      if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
      ncStatus = nf90_inq_varid (ncid, attstr1(1:len1), meshId)
      errmsg = "Dummy Variable "//attstr1(1:len1)//" does not exist"
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      errmsg = "Attribute node_coordinates in variable "//attstr1(1:len1)
      ncStatus = nf90_inquire_attribute(ncid, meshId, "node_coordinates", len=len1)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ncStatus = nf90_get_att (ncid, meshId, "node_coordinates", attstr1)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      if (attstr1(1:len1) .eq. attstr(1:len)) then
        location = 1
      else
        ! check if the coordinates match with face_coordinates
        ncStatus = nf90_inquire_attribute(ncid, meshId, "face_coordinates", len=len1)
        errmsg = "Attribute face_coordinates"
        if (CDFCheckError (ncStatus, &
           ESMF_METHOD,  &
           ESMF_SRCLINE, errmsg, &
           rc)) return
        ncStatus = nf90_get_att (ncid, meshId, "face_coordinates", attstr1)
        if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
        if (attstr1(1:len1) .eq. attstr(1:len)) then
          location = 2
        else
          errmsg = "- coordinates attribute does not match with face or node coordinates"
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg=errmsg, ESMF_CONTEXT, rcToReturn=rc)
          return
                endif
      endif             
     else
       ncStatus = nf90_inquire_attribute(ncid, VarId, "location", len=len)
        if (locationStr(1:len) .eq. 'node') then
          location = 1
       elseif (locationStr(1:len) .eq. 'face') then
          location = 2
       else
          errmsg = "- location attribute is not recognizable: "//locationStr(1:len)
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg=errmsg, ESMF_CONTEXT, rcToReturn=rc)
          return
       endif
     endif
     rc = ESMF_SUCCESS
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
#endif
     return
end subroutine ESMF_UGridInqVarLoc

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
    integer   :: ncStatus, localrc
    character(len=256) :: errmsg
    character(len=80) :: attstr
    integer   :: ndims, dimids(3), dimsize, length
    integer, pointer   :: starts(:), counts(:)
    integer :: locflag
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
        call ESMF_UGridInqVarLoc(ncid, VarId, varname, locflag, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                        ESMF_CONTEXT, rcToReturn=rc)) return
        if (((location .eq. 'node') .and. (locflag /= 1)) .or. &
           ((location .eq. 'face') .and. (locflag /= 2))) then
          errmsg = "- variable "//varname//" is not defined on location "//location
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
subroutine ESMF_GetMeshFromUGridFile (filename, nodeCoords, elmtConn, &
                                elmtNums, startElmt,  &
                                faceCoords, convertToDeg, rc)

    character(len=*), intent(in)   :: filename
    real(ESMF_KIND_R8), pointer    :: nodeCoords (:,:)
    integer(ESMF_KIND_I4), pointer :: elmtConn (:)
    integer(ESMF_KIND_I4), pointer :: elmtNums (:)
    integer,           intent(out) :: startElmt
    real(ESMF_KIND_R8), pointer, optional    :: faceCoords (:,:)
    logical, intent(in), optional  :: convertToDeg
    integer, intent(out), optional :: rc


    type(ESMF_VM) :: vm
    integer PetNo, PetCnt

    integer :: ncid, meshId
    integer :: ncStatus
    integer :: meshDim
    character(len=256) :: errmsg
    character(len=24) :: attbuf
    integer :: len
    logical :: convertToDegLocal
    logical :: faceCoordFlag
    integer :: localrc
    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    convertToDegLocal = .false.
    if (present(convertToDeg)) convertToDegLocal = convertToDeg

    ! Get VM information
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    call ESMF_UGridInq(filename, meshid=meshid, nodeCoordDim=meshDim, &
         faceCoordFlag=faceCoordFlag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return

    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    if (meshDim == 2) then
       if (faceCoordFlag) then
          call ESMF_GetMesh2DFromUGrid (filename, ncid, meshId, nodeCoords, elmtConn, &
                                elmtNums, startElmt, faceCoords=faceCoords, &
                                convertToDeg=convertToDegLocal, rc=rc)
       else
          call ESMF_GetMesh2DFromUGrid (filename, ncid, meshId, nodeCoords, elmtConn, &
                                elmtNums, startElmt, convertToDeg=convertToDegLocal, rc=rc)
       endif
    elseif (meshDim == 3) then
       if (faceCoordFlag) then
          call ESMF_GetMesh3DFromUGrid (filename, ncid, meshId, nodeCoords, elmtConn, &
                                elmtNums, startElmt, faceCoords=faceCoords, &
                                rc=rc)
       else
          call ESMF_GetMesh3DFromUGrid (filename, ncid, meshId, nodeCoords, elmtConn, &
                                elmtNums, startElmt, rc=rc)
       endif                    
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
                                elmtNums, startElmt, faceCoords, convertToDeg, rc)

    character(len=*), intent(in)   :: filename
    integer,           intent(in)  :: ncid, meshid                              
    real(ESMF_KIND_R8), pointer    :: nodeCoords (:,:)
    integer(ESMF_KIND_I4), pointer :: elmtConn (:)
    integer(ESMF_KIND_I4), pointer :: elmtNums (:)
    integer,           intent(out) :: startElmt
    real(ESMF_KIND_R8), pointer, optional   :: faceCoords(:,:)
     logical, intent(in), optional  :: convertToDeg
    integer, intent(out), optional :: rc

    type(ESMF_VM) :: vm
    integer PetNo, PetCnt

    integer(ESMF_KIND_I4), allocatable :: elmtConnT(:,:)
    integer :: DimIds(2), VarId
    integer :: ncStatus

    integer :: coordinateDims(3), coordDim, meshDim
    integer :: i, j, count, nodeCount, MaxNodePerElmt, localFillValue
     integer :: localCount, remain, elmtCount, localPolyBreakValue

    character(len=256) :: errmsg, locations, locNames(3), elmtConnName
    character(len=256) :: nodeCoordString, faceCoordString
    character(len=80), allocatable :: nodeCoordNames(:), faceCoordNames(:)
    integer :: pos0, pos, pos1, pos2, n, yesNode
    integer :: len, indexBase
    character(len=24) :: units
    logical :: convertToDegLocal
    integer, parameter :: nf90_noerror = 0
    real(ESMF_KIND_R8), allocatable:: nodeCoord1D(:), faceCoord1D(:)
    integer :: totalConnections

#ifdef ESMF_NETCDF

    ! Get VM information
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    if (present(convertToDeg)) then
       convertToDegLocal = convertToDeg
    else
       convertToDegLocal = .false.
    endif
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

    ! Get face coordinates if faceCoords argument is given
    if (present(faceCoords)) then
       ncStatus = nf90_inquire_attribute(ncid, meshId, "face_coordinates", len=len)
       errmsg = "Attribute face_coordinates in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
       allocate( faceCoordNames(2) )
       ncStatus = nf90_get_att (ncid, meshId, "face_coordinates", faceCoordString)
       errmsg = "Attribute face_coordinates in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
       ! Parse the attribute to find the varible names for face_coordinates
       pos = index(faceCoordString(1:)," ")
       faceCoordNames(1) = faceCoordString(1:pos-1)
       faceCoordNames(2)=faceCoordString(pos+1:len)
    endif
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
     if (units(len:len) .eq. achar(0)) len = len-1
      units = ESMF_UtilStringLowerCase(units(1:len))
      if (units(1:7) .ne. 'degrees' .and. units(1:7) .ne. 'radians') then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- units attribute is not degrees or radians", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
      endif
      ! if units is "radians", convert it to degrees
       if (convertToDegLocal) then
         if (units(1:7) .eq. "radians") then
            nodeCoords(i,:) = &
                 nodeCoords(i,:)*ESMF_COORDSYS_RAD2DEG
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
    if (ncStatus /= nf90_noerror) localFillValue = -1

    ! Get PolyBreak Value if it's not present, then set it to localFillValue, so
    ! it's ignored
    ncStatus = nf90_get_att (ncid, VarId, "polygon_break_value", &
         values=localPolyBreakValue)
    if (ncStatus /= nf90_noerror) then
       localPolyBreakValue = localFillValue
    else
      ! If it's been set, then make sure that this value isn't the same as _FillValue
       if (localPolyBreakValue == localFillValue) then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
            msg="- polygon_break_value can't be the same as _localFillValue", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
       endif
    endif

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
    allocate(elmtConnT(MaxNodePerElmt,localCount) )
    allocate( elmtNums(localCount) )
    ! Get element connectivity... transposed
    ncStatus = nf90_get_var (ncid, VarId, elmtConnT, start=(/1,startElmt/), &
                            count=(/MaxNodePerElmt,localCount/))
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get faceCoordinates if faceCoords is present
    if (present(faceCoords)) then
       allocate(faceCoords(2,localCount), faceCoord1D(localCount))
       do i=1,2
           errmsg = "Variable "//faceCoordNames(i)//" in "//trim(filename)
          ncStatus = nf90_inq_varid (ncid, faceCoordNames(i), VarId)
          if (CDFCheckError (ncStatus, &
              ESMF_METHOD,  &
              ESMF_SRCLINE, errmsg, &
              rc)) return
          ncStatus = nf90_get_var (ncid, VarId, faceCoord1D, start=(/startElmt/), &
                                  count=(/localCount/))
          if (CDFCheckError (ncStatus, &
              ESMF_METHOD,  &
              ESMF_SRCLINE, errmsg, &
              rc)) return

          do j=1,localCount
             faceCoords(i,j)=faceCoord1d(j)
          enddo

          ! if units is radians_east or radians_north, convert to degrees
          ! get the attribute 'units'
          ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
          errmsg = "Attribute units for "//faceCoordNames(i)//" in "//trim(filename)
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
          if (units(1:7) .ne. 'degrees' .and. units(1:7) .ne. 'radians') then
              call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                     msg="- units attribute is not degrees or radians", &
                    ESMF_CONTEXT, rcToReturn=rc)
              return
          endif
          ! if units is "radians", convert it to degrees
          if (convertToDegLocal) then
             if (units(1:7) .eq. "radians") then
                 faceCoords(i,:) = &
                     faceCoords(i,:)*ESMF_COORDSYS_RAD2DEG
             endif
          endif         
       enddo
       deallocate(faceCoordNames, faceCoord1d)
    endif

    ! Get the number of nodes for each element
    ! if indexBase is 0, change it to 1 based index
    totalConnections = 0
    elmtNums(:)=MaxNodePerElmt
    if (indexBase == 0) then
      do i=1,localcount
        do j=1,MaxNodePerElmt
          ! change 0-base to 1-base
          if (elmtConnT(j,i) /= localFillValue) then
             if (elmtConnT(j,i) /= localPolyBreakValue) then
                elmtConnT(j,i)=elmtConnT(j,i)+1
             endif
          else
             ! find the first FillValue
             elmtNums(i) = j-1
             totalConnections = totalConnections+elmtNums(i)
             exit
          endif
         enddo  
         !! Change j to i in elmtNums(i)
         if (elmtNums(i)==MaxNodePerElmt) then
            totalConnections = totalconnections+elmtNums(i)
         endif
      enddo
    else
      do i=1,localcount
        j = MaxNodePerElmt
        do while (elmtConnT(j,i) == localFillValue)
          j = j - 1
        enddo
        elmtNums(i) = j
        totalConnections = totalConnections+elmtNums(i)
      enddo
    endif

    ! Change File PolyBreak to MeshPolyBreak
    ! (if localPolyBreakValue was set then it'll be different than localFillValue)
    if (localPolyBreakValue /= localFillValue) then
       do i=1,localcount
          do j=1,elmtNums(i)
             if (elmtConnT(j,i)==localPolyBreakValue) then
                elmtConnT(j,i)=ESMF_MESH_POLYBREAK
             endif
          enddo
       enddo
    endif

    allocate(elmtConn(totalConnections))
    j=1
    do i=1,localcount
       elmtConn(j:j+elmtNums(i)-1)=elmtConnT(1:elmtNums(i),i)
       j = j + elmtNums(i)
    enddo
    deallocate(elmtConnT)

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
                                elmtNums, startElmt, faceCoords, rc)

    character(len=*), intent(in)   :: filename
    integer,           intent(in)  :: ncid, meshid                              
    real(ESMF_KIND_R8), pointer    :: nodeCoords (:,:)
    integer(ESMF_KIND_I4), pointer :: elmtConn (:)
    integer(ESMF_KIND_I4), pointer :: elmtNums (:)
    integer,           intent(out) :: startElmt
    real(ESMF_KIND_R8), pointer, optional   :: faceCoords(:,:)
    integer, intent(out), optional :: rc

    type(ESMF_VM) :: vm
    integer :: PetNo, PetCnt

    integer(ESMF_KIND_I4), allocatable :: elmtConnT(:,:)
    integer :: DimIds(2), VarId
    integer :: ncStatus

    integer :: coordinateDims(3), coordDim, meshDim
    integer :: i, j, count, nodeCount, MaxNodePerElmt, localFillValue
    integer :: localCount, remain, elmtCount, localPolyBreakValue

    character(len=256) :: errmsg, locations, locNames(3), elmtConnName
    character(len=256) :: nodeCoordString, faceCoordString
    character(len=80), allocatable :: nodeCoordNames(:), faceCoordNames(:)
    integer :: pos0, pos, pos1, pos2, n, yesNode
    integer :: len, indexBase
    character(len=24) :: units
    real(ESMF_KIND_R8) :: deg2rad, earthradius
    real(ESMF_KIND_R8) :: coord(3)
    integer, parameter :: nf90_noerror = 0
    real(ESMF_KIND_R8), allocatable:: nodeCoord1D(:), faceCoord1D(:)
    integer            :: localrc
    integer            :: totalConnections

#ifdef ESMF_NETCDF

    ! Get VM information
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
    if (rc /= ESMF_SUCCESS) return

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

    ! Get face coordinates if faceCoords argument is given
    if (present(faceCoords)) then
       ncStatus = nf90_inquire_attribute(ncid, meshId, "face_coordinates", len=len)
       errmsg = "Attribute face_coordinates in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
       allocate( faceCoordNames(3) )
       ncStatus = nf90_get_att (ncid, meshId, "face_coordinates", faceCoordString)
       errmsg = "Attribute face_coordinates in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
       ! Parse the attribute to find the varible names for face_coordinates
       pos1 = index(faceCoordString(1:)," ")
       faceCoordNames(1) = faceCoordString(1:pos1-1)
       pos2 = index(faceCoordString(pos1+1:)," ")
       faceCoordNames(2)=faceCoordString(pos1+1:pos1+pos2-1)
       faceCoordNames(3)=faceCoordString(pos1+pos2+1:len)
    endif
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
        units = ESMF_UtilStringLowerCase(units(1:len))
        if (units(len:len) .eq. achar(0)) len = len-1
        if (units(1:7) .ne. 'degrees' .and. units(1:7) .ne. 'radians') then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- units attribute is not degrees or radians", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
        ! if units is "radians", convert it to degrees
        if (units(1:7) .eq. "radians") then
            nodeCoords(i,:) = nodeCoords(i,:)*ESMF_COORDSYS_RAD2DEG
        endif
      else      
        ! normalize the height using the earth radius
        if (units(len:len) .eq. achar(0)) len = len-1
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

! save coordinates as ESMF_COORDSYS_SPH_DEG, no need to convert to CART
#if 0
    ! Convert the coordinates into Cartesian 3D
    do i=1,nodeCount
      call c_esmc_sphdeg_to_cart(nodeCoords(1,i), nodeCoords(2,i), &
                  coord(1), coord(2), coord(3), &
                  localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return
      nodeCoords(1,i)=nodeCoords(3,i)*coord(1)
      nodeCoords(2,i)=nodeCoords(3,i)*coord(2)
      nodeCoords(3,i)=nodeCoords(3,i)*coord(3)
    enddo
#endif

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
    if (ncStatus .ne. nf90_noerror) localFillValue = -1

    ! Get PolyBreak Value if it's not present, then set it to localFillValue, so
    ! it's ignored
    ncStatus = nf90_get_att (ncid, VarId, "polygon_break_value", &
         values=localPolyBreakValue)
    if (ncStatus /= nf90_noerror) then
       localPolyBreakValue = localFillValue
    else
       ! If it's been set, then make sure that this value isn't the same as _FillValue
       if (localPolyBreakValue == localFillValue) then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
            msg="- polygon_break_value can't be the same as _localFillValue", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
       endif
    endif

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
    allocate(elmtConnT(MaxNodePerElmt,localCount) )
    allocate( elmtNums(localCount) )
    ! Get element connectivity... transposed
    ncStatus = nf90_get_var (ncid, VarId, elmtConnT, start=(/1,startElmt/), &
                            count=(/MaxNodePerElmt,localCount/))
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get faceCoordinates if faceCoords is present
    if (present(faceCoords)) then
       allocate(faceCoords(3,localCount), faceCoord1D(localCount))
       do i=1,3
          errmsg = "Variable "//faceCoordNames(i)//" in "//trim(filename)
          ncStatus = nf90_inq_varid (ncid, faceCoordNames(i), VarId)
          if (CDFCheckError (ncStatus, &
              ESMF_METHOD,  &
              ESMF_SRCLINE, errmsg, &
              rc)) return
          ncStatus = nf90_get_var (ncid, VarId, faceCoord1D, start=(/startElmt/), &
                                  count=(/localCount/))
          if (CDFCheckError (ncStatus, &
              ESMF_METHOD,  &
              ESMF_SRCLINE, errmsg, &
              rc)) return

          do j=1,localCount
             faceCoords(i,j)=faceCoord1d(j)
          enddo

       enddo
       deallocate(faceCoordNames, faceCoord1d)
    endif

    ! Get the number of nodes for each element
    ! if indexBase is 0, change it to 1 based index
    totalConnections = 0
    elmtNums(:)=MaxNodePerElmt
    if (indexBase == 0) then
      do i=1,localcount
        do j=1,MaxNodePerElmt
          ! change 0-base to 1-base
          if (elmtConnT(j,i) /= localFillValue) then
             if (elmtConnT(j,i) /= localPolyBreakValue) then
                elmtConnT(j,i)=elmtConnT(j,i)+1
             endif
          else
             ! find the first FillValue
             elmtNums(i) = j-1  
             totalConnections = totalConnections+elmtNums(i)
             exit
          endif
         enddo  
      enddo
    else
      do i=1,localcount
        j = MaxNodePerElmt
        do while (elmtConnT(j,i) == localFillValue)
          j = j - 1
        enddo
        elmtNums(i) = j
        totalConnections = totalConnections+elmtNums(i)
      enddo
    endif

    ! Change File PolyBreak to MeshPolyBreak
    ! (if localPolyBreakValue was set then it'll be different than localFillValue)
    if (localPolyBreakValue /= localFillValue) then
       do i=1,localcount
          do j=1,elmtNums(i)
             if (elmtConnT(j,i)==localPolyBreakValue) then
                elmtConnT(j,i)=ESMF_MESH_POLYBREAK
             endif
          enddo
       enddo
    endif

    allocate(elmtConn(totalConnections))
    j=1
    do i=1,localcount
       elmtConn(j:j+elmtNums(i)-1)=elmtConnT(1:elmtNums(i),i)
       j = j + elmtNums(i)
    enddo

    if (totalConnections /= j-1) then
      print *, PetNo, 'totalconnection does match ', totalConnections, j-1
    endif
    deallocate(elmtConnT)

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

#if 1
!---------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GetElemFromUGridFile"
subroutine ESMF_GetElemFromUGridFile (filename, meshname, elmtConn, &
                                elmtNums, startElmt, rc)

    character(len=*), intent(in)   :: filename
    character(len=*), intent(in)   :: meshname
    integer(ESMF_KIND_I4), pointer :: elmtConn (:)
    integer(ESMF_KIND_I4), pointer :: elmtNums (:)
    integer,           intent(out) :: startElmt
    integer, intent(out), optional :: rc

    type(ESMF_VM) :: vm
    integer PetNo, PetCnt

    integer :: ncid, meshid                             
    integer(ESMF_KIND_I4), allocatable :: elmtConnT(:,:)
    integer :: DimIds(2), VarId
    integer :: ncStatus
    integer :: coordDim, meshDim
    integer :: i, j, count, nodeCount, MaxNodePerElmt, localFillValue
    integer :: localCount, remain, elmtCount

    character(len=256) :: locations, locNames(3), elmtConnName
    integer :: pos0, pos, pos1, pos2, n, yesNode
    integer :: len, indexBase
    character(len=24) :: units
    character(len=256) :: errmsg
    character(len=24) :: attbuf
    integer :: totalConnections
    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF

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
    if (ncStatus /= nf90_noerror) then
      ncStatus = nf90_get_att (ncid, meshId, "standard_name", values=attbuf)
      errmsg = "Attribute cf_role in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
    endif
    if (attbuf(1:13) .ne. "mesh_topology") then
      call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- cf_role attribute is not mesh_topology", &
                 ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Get mesh dimension
    ncStatus = nf90_get_att (ncid, meshId, "topology_dimension", values=meshDim)
    if (ncStatus /= nf90_noerror) then
       ncStatus = nf90_get_att (ncid, meshId, "dimension", values=meshDim)
       errmsg = "Attribute topology_dimension or dimension in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
    endif

    if (meshDim == 2) then
       ! Get element connectivity, if it does not exist, bail out
       errmsg = "Attribute face_node_connectivity in "//trim(filename)
       ncStatus = nf90_inquire_attribute(ncid, meshId, "face_node_connectivity", len=len)
       ncStatus = nf90_get_att (ncid, meshId, "face_node_connectivity", values=elmtConnName)
    else if (meshDim == 3) then
       ! Get element connectivity, if it does not exist, bail out
       errmsg = "Attribute volume_node_connectivity in "//trim(filename)
       ncStatus = nf90_inquire_attribute(ncid, meshId, "volume_node_connectivity", len=len)
       ncStatus = nf90_get_att (ncid, meshId, "volume_node_connectivity", values=elmtConnName)
    else
       ! error message -- wrong dimension
    endif
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
    ! print *, PetNo, 'Before allocating elmtConn', localCount
    allocate(elmtConnT(MaxNodePerElmt,localCount) )
    allocate( elmtNums(localCount) )
    ! Get element connectivity... transposed
    ! print *, PetNo, 'Before nf90_get_var()', startElmt,localCount
    ncStatus = nf90_get_var (ncid, VarId, elmtConnT, start=(/1,startElmt/), &
                            count=(/MaxNodePerElmt,localCount/))
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Get the number of nodes for each element
    ! if indexBase is 0, change it to 1 based index
    totalConnections = 0
    elmtNums(:)=MaxNodePerElmt
    if (indexBase == 0) then
      do i=1,localcount
        do j=1,MaxNodePerElmt
          ! change 0-base to 1-base
          if (elmtConnT(j,i) /= localFillValue) then
             elmtConnT(j,i)=elmtConnT(j,i)+1
          else
             ! find the first FillValue
             elmtNums(i) = j-1
             totalConnections = totalConnections+elmtNums(i)
             exit
          endif
         enddo  
         if (elmtNums(i)==MaxNodePerElmt) then
             totalConnections = totalConnections+elmtNums(i)
         endif
      enddo
    else
      do i=1,localcount
        j = MaxNodePerElmt
        do while (elmtConnT(j,i) == localFillValue)
          j = j - 1
        enddo
        elmtNums(i) = j
        totalConnections = totalConnections+elmtNums(i)
      enddo
    endif

    allocate(elmtConn(totalConnections))
    j=1
    do i=1,localcount
       elmtConn(j:j+elmtNums(i)-1)=elmtConnT(1:elmtNums(i),i)
       j = j + elmtNums(i)
    enddo
    if (totalConnections /= j-1) then
      print *, PetNo, 'totalconnection does match ', totalConnections, j-1
    endif
    deallocate(elmtConnT)
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

end subroutine ESMF_GetElemFromUGridFile

!---------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GetNodeFromUGridFile"
subroutine ESMF_GetNodeFromUGridFile (filename, meshname, nodeCoords,  &
                                nodeCount, startNode, convertToDeg, rc)

    character(len=*), intent(in)       :: filename
    character(len=*), intent(in)       :: meshname
    real(ESMF_KIND_R8), pointer     :: nodeCoords (:,:)
    integer,  intent(in), optional      :: nodeCount
    integer,  intent(in), optional      :: startNode
    logical, intent(in), optional        :: convertToDeg
    integer, intent(out), optional     :: rc

    type(ESMF_VM) :: vm
    integer :: PetNo, PetCnt

    integer :: ncid, meshid                             
    integer :: DimIds(2), VarId
    integer :: ncStatus

    integer :: coordinateDims(3), coordDim, meshDim
    integer :: i, j, count, totalCounts, MaxNodePerElmt, localFillValue
    integer :: localCount, localStart

    character(len=256) :: errmsg, locations, locNames(3), elmtConnName
    character(len=256) :: nodeCoordString
    character(len=80), allocatable :: nodeCoordNames(:)
    integer :: pos0, pos, pos1, pos2, n, yesNode
    integer :: len, indexBase
    integer :: totalnodes
    character(len=24) :: units, attbuf
    real(ESMF_KIND_R8) :: deg2rad, earthradius
    real(ESMF_KIND_R8) :: rad2deg
    real(ESMF_KIND_R8) :: coord(3)
    logical  :: convertToDegLocal
    integer, parameter :: nf90_noerror = 0
    real(ESMF_KIND_R8), allocatable:: nodeCoord1D(:)

#ifdef ESMF_NETCDF

    ! Get VM information
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    convertToDegLocal = .false.
    if (present(convertToDeg)) convertToDegLocal = convertToDeg

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
    if (ncStatus /= nf90_noerror) then
      ncStatus = nf90_get_att (ncid, meshId, "standard_name", values=attbuf)
      errmsg = "Attribute cf_role in "//trim(filename)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
    endif
    if (attbuf(1:13) .ne. "mesh_topology") then
      call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- cf_role attribute is not mesh_topology", &
                 ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! Get mesh dimension
    ncStatus = nf90_get_att (ncid, meshId, "topology_dimension", values=meshDim)
    if (ncStatus /= nf90_noerror) then
       ncStatus = nf90_get_att (ncid, meshId, "dimension", values=meshDim)
       errmsg = "Attribute topology_dimension or dimension in "//trim(filename)
       if (CDFCheckError (ncStatus, &
          ESMF_METHOD,  &
          ESMF_SRCLINE, errmsg, &
          rc)) return
    endif

    ! Get node coordinates
    ncStatus = nf90_inquire_attribute(ncid, meshId, "node_coordinates", len=len)
    errmsg = "Attribute node_coordinates in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ncStatus = nf90_get_att (ncid, meshId, "node_coordinates", nodeCoordString)
    errmsg = "Attribute node_coordinates in "//trim(filename)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Parse the attribute to find the varible names for node_coordinates
    if (meshDim == 2) then
       allocate( nodeCoordNames(2))
       pos1 = index(nodeCoordString(1:)," ")
       nodeCoordNames(1) = nodeCoordString(1:pos1-1)
       nodeCoordNames(2)=nodeCoordString(pos1+1:len)
    else
       allocate( nodeCoordNames(3))
       pos1 = index(nodeCoordString(1:)," ")
       nodeCoordNames(1) = nodeCoordString(1:pos1-1)
       pos2 = index(nodeCoordString(pos1+1:)," ")
       nodeCoordNames(2) = nodeCoordString(pos1+1:pos1+pos2-1)
       nodeCoordNames(3)=nodeCoordString(pos1+pos2+1:len)
    endif
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
    ncStatus = nf90_inquire_dimension (ncid, DimIds(1), len=totalNodes)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    if (present(startNode)) then
      localStart = startNode
    else
      localStart = 1
    endif
    if (present(nodeCount)) then
      if (localStart+nodeCount-1 > totalNodes) then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- startNode+nodeCount > node dimension", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
      endif
      localCount=nodeCount
    else
      localCount=totalNodes
    endif
    allocate( nodeCoords(meshDim,localCount), nodeCoord1D(localCount) )
    do i=1,meshDim
      errmsg = "Variable "//trim(nodeCoordNames(i))//" in "//trim(filename)
      ncStatus = nf90_inq_varid (ncid, trim(nodeCoordNames(i)), VarId)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, nodeCoord1D, start=(/localStart/), &
                         count=(/localCount/))
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return

      do j=1,localCount
        nodeCoords(i,j)=nodeCoord1D(j)
      enddo

      ! Convert to Cartisian 3D coordinates
      ! get the attribute 'units'
      ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
      errmsg = "Attribute units for "//trim(nodeCoordNames(i))//" in "//trim(filename)
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
      if (units(len:len) .eq. achar(0)) len = len-1
      if (i==1 .or. i==2) then
        ! if units is not "degrees" or "radians" return errors
        units = ESMF_UtilStringLowerCase(units(1:len))
        if (units(1:7) .ne. 'degrees' .and. units(1:7) .ne. 'radians') then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- units attribute is not degrees or radians", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
        ! if units is "radians", convert it to degrees
        if (meshDim == 2 .and. convertToDegLocal) then
          if (units(1:7) .eq. "radians") then
            rad2deg = 180.0/3.141592653589793238
            !print *, 'Convert radians to degree ', rad2deg
            nodeCoords(i,:) = nodeCoords(i,:)*rad2deg
          endif
        elseif (meshDim == 3) then      
          ! if units is "degrees", convert it to radians
          if (units(1:7) .eq. "degrees") then
             deg2rad = 3.141592653589793238/180.0
             nodeCoords(i,:) = nodeCoords(i,:)*deg2rad
          endif
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

    ! keep the coordinates as Spherical Degree
#if 0
    if (meshDim == 3) then
    ! Convert the coordinates into Cartesian 3D
      do i=1,localCount
        coord(1)=nodeCoords(3,i)*cos(nodeCoords(1,i))*cos(nodeCoords(2,i))
        coord(2)=nodeCoords(3,i)*sin(nodeCoords(1,i))*cos(nodeCoords(2,i))
        coord(3)=nodeCoords(3,i)*sin(nodeCoords(2,i))
        nodeCoords(:,i)=coord(:)
      enddo
    endif
#endif
    ! Deallocations
    deallocate( nodeCoordNames, nodeCoord1D)

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

end subroutine ESMF_GetNodeFromUGridFile

#endif

!---------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_UGridGetCoords"
subroutine ESMF_UGridGetCoords (filename, meshid, coords,  &
                                start, count, centerflag, rc)

    character(len=*), intent(in)       :: filename
    integer, intent(in)               :: meshid
    real(ESMF_KIND_R8), pointer        :: coords (:,:)
    integer,  intent(in)               :: count
    integer,  intent(in)               :: start
    logical, intent(in)                :: centerflag
    integer, intent(out)               :: rc

    type(ESMF_VM) :: vm
    integer :: PetNo, PetCnt

    integer :: ncid, VarId
    integer :: ncStatus

    integer :: i, coorddims,pos1,pos2,len
    character(len=256) :: units, errmsg, attname, coordString
    character(len=256), pointer :: coordNames(:)
    real(ESMF_KIND_R8) :: earthradius

#ifdef ESMF_NETCDF

    ! Get VM information
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! if count==0, return with success
    if (count==0) then
       rc=ESMF_SUCCESS
       return
    endif
    ncStatus = nf90_open (path=trim(filename), mode=nf90_nowrite, ncid=ncid)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, trim(filename), &
      rc)) return

    coorddims = size(coords, 2)
    if (centerflag) then
       attname="face_coordinates"
    else
       attname="node_coordinates"
    endif
    ncStatus = nf90_inquire_attribute(ncid, meshId, attname, len=len)
    errmsg = "Attribute "//trim(attname)//" in "//trim(filename)
    if (CDFCheckError (ncStatus, &
          ESMF_METHOD, &
          ESMF_SRCLINE,&
          errmsg,&
          rc)) return
    ncStatus = nf90_get_att (ncid, meshId, attname, coordString)
    if (CDFCheckError (ncStatus, &
      ESMF_METHOD,  &
      ESMF_SRCLINE, errmsg, &
      rc)) return

    ! Parse the attribute to find the varible names for node_coordinates
    if (coorddims == 2) then
       allocate( coordNames(2))
       pos1 = index(coordString(1:)," ")
       coordNames(1) = trim(coordString(1:pos1-1))
       coordNames(2)=trim(coordString(pos1+1:len))
    else
       allocate( coordNames(3))
       pos1 = index(coordString(1:)," ")
       coordNames(1) = trim(coordString(1:pos1-1))
       pos2 = index(coordString(pos1+1:)," ")
       coordNames(2) = trim(coordString(pos1+1:pos1+pos2-1))
       coordNames(3)=trim(coordString(pos1+pos2+1:len))
    endif

    do i=1,coorddims
      errmsg = "Variable "//trim(coordNames(i))//" in "//trim(filename)
      ncStatus = nf90_inq_varid (ncid, trim(coordNames(i)), VarId)
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return
      ncStatus = nf90_get_var (ncid, VarId, coords(:,i) , start=(/start/), &
                         count=(/count/))
      if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, errmsg, &
        rc)) return

      ! Convert to Cartisian 3D coordinates
      ! get the attribute 'units'
      ncStatus = nf90_inquire_attribute(ncid, VarId, "units", len=len)
      errmsg = "Attribute units for "//coordNames(i)//" in "//trim(filename)
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
        units = ESMF_UtilStringLowerCase(units(1:len))
        if (units(len:len) .eq. achar(0)) len = len-1
        if (units(1:7) .ne. 'degrees' .and. units(1:7) .ne. 'radians') then
          call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, &
                 msg="- units attribute is not degrees or radians", &
                 ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
        ! if units is "radians", convert it to degrees
        if (units(1:7) .eq. "radians") then
            coords(:,i) = coords(:,i)*ESMF_COORDSYS_RAD2DEG
        endif
      else      
        ! normalize the height using the earth radius
        if (units(len:len) .eq. achar(0)) len = len-1
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
        coords(:,i)=1+coords(:,i)/earthradius
      endif
    enddo
    ncStatus = nf90_close(ncid)
    if (CDFCheckError (ncStatus, &
        ESMF_METHOD,  &
        ESMF_SRCLINE, trim(filename), &
        rc)) return
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

end subroutine ESMF_UGridGetCoords
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
        call ESMF_LogWrite (msg=trim(errmsg)//':'//trim(nf90_strerror(ncStatus)), &
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
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif

end function CDFCheckError


end module ESMF_IOUGridMod
