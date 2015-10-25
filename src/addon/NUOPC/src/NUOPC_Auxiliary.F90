! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2015, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Auxiliary.F90"
!==============================================================================

module NUOPC_Auxiliary

  use ESMF

  implicit none
  
  private
  
  public NUOPC_ConvertStringToInt         ! method
  public NUOPC_CreateSimpleSphGrid        ! method
  public NUOPC_CreateSimpleXYGrid         ! method
  public NUOPC_FillField                  ! method

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ConvertStringToInt - Convert a string to an integer
! !INTERFACE:
  function NUOPC_ConvertStringToInt(string, specialStringList, specialValueList, rc)
! !RETURN VALUE:
    integer :: NUOPC_ConvertStringToInt
! !ARGUMENTS:
    character(len=*), intent(in)            :: string
    character(len=*), intent(in),  optional :: specialStringList(:)
    integer,          intent(in),  optional :: specialValueList(:)
    integer,          intent(out), optional :: rc
! !DESCRIPTION:
!   Return the numerical integer value represented by the {\tt string}. 
!   If special strings are to be takein into account, both 
!   {\tt specialStringList} and {\tt specialValueList} arguments must be
!   present and of same size.
!   
!   An error is returned, and return value set to 0, if {\tt string} is not
!   found in {\tt specialStringList}, and does not convert into an integer
!   value.
!
!   Leading and trailing blanks in {\tt string} are ignored when directly
!   converting into integers. 
!
!   The arguments are:
!   \begin{description}
!   \item[string]
!     The string to be converted
!   \item[{[specialStringList]}]
!     List of special strings.
!   \item[{[specialValueList]}]
!     List of values associated with special strings.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    logical                 :: ssL, svL
    integer                 :: i
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    NUOPC_ConvertStringToInt = 0 ! initialize
    
    ! checking consistency of inputs provided
    ssL = present(specialStringList)
    svL = present(specialValueList)
    
    if (ssL.neqv.svL) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Both specialStringList and specialValueList must either be "// &
        "present or absent.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    endif
    
    if (ssL) then
      ! special strings and values present
      if (size(specialStringList) /= size(specialValueList)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Both specialStringList and specialValueList must have "// &
          "the same number of elements.", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      endif
      do i=1, size(specialStringList)
        if (trim(string)==trim(specialStringList(i))) then
          ! found a matching special string
          NUOPC_ConvertStringToInt = specialValueList(i)
          return ! successful early return
        endif
      enddo
    endif
    
    if (verify(trim(adjustl(string)),"0123456789") == 0) then
      ! should convert to integer just fine
      read (string, "(i10)") NUOPC_ConvertStringToInt
    else
      ! the string contains characters besides numbers
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="The string '"//trim(string)//"' contains characters besides "// &
          "numbers, cannot convert to integer.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    endif
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CreateSimpleSphGrid - Create a simple spherical grid
! !INTERFACE:
  function NUOPC_CreateSimpleSphGrid(x_min, y_min, x_max, y_max, &
    i_count, j_count, half_polar_cell, area_adj, tag, scheme, rc)
! !RETURN VALUE:
    type(ESMF_Grid):: NUOPC_CreateSimpleSphGrid
! !ARGUMENTS:
    real(ESMF_KIND_R8), intent(in)            :: x_min, x_max, y_min, y_max
    integer,            intent(in)            :: i_count, j_count
    logical,            intent(in),  optional :: half_polar_cell
    real(ESMF_KIND_R4), intent(in),  optional :: area_adj
    character(len=*),   intent(in),  optional :: tag
    integer,            intent(in) , optional :: scheme
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Return a simple spherical Grid.
!
!   The arguments are:
!   \begin{description}
!   \item[x\_min]
!     Lower bound x coordinate.
!   \item[x\_max]
!     Upper bound x coordinate.
!   \item[y\_min]
!     Lower bound y coordinate.
!   \item[y\_max]
!     Upper bound y coordinate.
!   \item[i\_count]
!     Number of elements along x.
!   \item[j\_count]
!     Number of elements along y.
!   \item[{[half\_polar\_cell]}]
!     {\em Need documentation.}
!   \item[{[area\_adj]}]
!     {\em Need documentation.}
!   \item[{[tag]}]
!     {\em Need documentation.}
!   \item[{[scheme]}]
!     {\em Need documentation.}
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                                   :: nx, ny
    real(ESMF_KIND_R8)                        :: dx, dy, sx, sy, halfdy
    integer                                   :: i, j
    real(ESMF_KIND_R8), pointer               :: coordX(:,:), coordY(:,:)
    real(ESMF_KIND_R8), pointer               :: f_area(:,:), f_area_m(:)
    real(ESMF_KIND_R8), pointer               :: o_area(:,:)
    real(ESMF_KIND_R8)                        :: startx, starty
    integer                                   :: l_scheme
    type(ESMF_Mesh)                           :: mesh
    type(ESMF_Field)                          :: field
    logical                                   :: l_half_polar_cell
    
    if (present(rc)) rc = ESMF_SUCCESS
    l_half_polar_cell = .false.
    if(present(half_polar_cell)) l_half_polar_cell = half_polar_cell

    ! convert to input variables to the internal variables
    sx = x_min
    sy = y_min
    nx = i_count
    ny = j_count
    dx = (x_max - x_min) / nx
    if(l_half_polar_cell) then
      dy = (y_max - y_min) / (ny - 1)
      halfdy = dy/2.
    else
      dy = (y_max - y_min) / ny
    endif
    
    ! scheme
    l_scheme = ESMF_REGRID_SCHEME_REGION3D
    if(present(scheme)) l_scheme = scheme

    if(l_scheme == ESMF_REGRID_SCHEME_FULL3D) then
      NUOPC_CreateSimpleSphGrid = ESMF_GridCreate1PeriDim(maxIndex=(/nx, ny/), &
        indexflag=ESMF_INDEX_GLOBAL, &
        gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
        !regDecomp=(/npet, 1/), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    else
      NUOPC_CreateSimpleSphGrid = ESMF_GridCreateNoPeriDim(maxIndex=(/nx, ny/),&
        indexflag=ESMF_INDEX_GLOBAL, &
        gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/1,1/), &
        !regDecomp=(/npet, 1/), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    endif 

    call ESMF_GridAddCoord(NUOPC_CreateSimpleSphGrid, &
      staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_GridAddCoord(NUOPC_CreateSimpleSphGrid, &
      staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! global indexing
    ! atm grid is not decomposed in the y direction
    !startx = lpet*nx/npet*dx
    startx = sx
    starty = sy
    ! compute coord
    ! X center
    call ESMF_GridGetCoord(NUOPC_CreateSimpleSphGrid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, farrayPtr=coordX, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! Y center
    call ESMF_GridGetCoord(NUOPC_CreateSimpleSphGrid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, farrayPtr=coordY, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if(l_half_polar_cell) then
      do i = lbound(coordX,1), ubound(coordX,1)
        do j = lbound(coordX, 2), ubound(coordX, 2)
          coordX(i,j) = startx + dx/2. + (i-1)*dx
          coordY(i,j) = starty + halfdy/2. + (j-1)*dy
        enddo
      enddo
    else
      do i = lbound(coordX,1), ubound(coordX,1)
        do j = lbound(coordX, 2), ubound(coordX, 2)
          coordX(i,j) = startx + dx/2. + (i-1)*dx
          coordY(i,j) = starty + dy/2. + (j-1)*dy
        enddo
      enddo
    endif
    !print *, 'startx: ', startx, lbound(coordX, 1), ubound(coordX, 1), 'coordX: ', coordX(:,1)
    ! X corner
    call ESMF_GridGetCoord(NUOPC_CreateSimpleSphGrid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, farrayPtr=coordX, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    ! Y corner
    call ESMF_GridGetCoord(NUOPC_CreateSimpleSphGrid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, farrayPtr=coordY, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if(l_half_polar_cell) then
      do i = lbound(coordX,1), ubound(coordX,1)
        do j = lbound(coordX, 2), ubound(coordX, 2)
          coordX(i,j) = startx + (i-1)*dx
          if(j == 1) then
            coordY(i,j) = starty
          else 
            coordY(i,j) = starty + halfdy + (j-2)*dy
          endif
        enddo
      enddo
    else
      do i = lbound(coordX,1), ubound(coordX,1)
        do j = lbound(coordX, 2), ubound(coordX, 2)
          coordX(i,j) = startx + (i-1)*dx
          coordY(i,j) = starty + (j-1)*dy
        enddo
      enddo
    endif

    if(present(area_adj)) then
      ! retrieve area

      !mesh = ESMF_GridToMesh(NUOPC_CreateSimpleSphGrid, &
      !  ESMF_STAGGERLOC_CORNER, 0, &
      !  regridConserve=ESMF_REGRID_CONSERVE_ON, rc=rc)
      !if (ESMF_LogFoundError(rc, &
      !    ESMF_ERR_PASSTHRU, &
      !    ESMF_CONTEXT, rcToReturn=rc)) return

      !allocate(f_area_m(mesh%NumOwnedElements))
      !call ESMF_MeshGetElemArea(mesh,  arealist=f_area_m, rc=rc)
      !if (ESMF_LogFoundError(rc, &
      !    ESMF_ERR_PASSTHRU, &
      !    ESMF_CONTEXT, rcToReturn=rc)) return
      !deallocate(f_area_m)

      ! find out original Grid cell area
      field = ESMF_FieldCreate(NUOPC_CreateSimpleSphGrid, typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call ESMF_FieldRegridGetArea(field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      call ESMF_FieldGet(field, farrayPtr=o_area, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      ! add area to Grid
      call ESMF_GridAddItem(NUOPC_CreateSimpleSphGrid, ESMF_GRIDITEM_AREA, &
        staggerloc=ESMF_STAGGERLOC_CENTER,  rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      call ESMF_GridGetItem(NUOPC_CreateSimpleSphGrid, ESMF_GRIDITEM_AREA, &
        staggerloc=ESMF_STAGGERLOC_CENTER, farrayptr=f_area, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      ! adjust Grid area
      f_area = area_adj*o_area

    endif

    if(present(rc)) rc = ESMF_SUCCESS

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_CreateSimpleXYGrid - Create a simple XY cartesian grid
! !INTERFACE:
  function NUOPC_CreateSimpleXYGrid(x_min, y_min, x_max, y_max, &
    i_count, j_count, rc)
! !RETURN VALUE:
    type(ESMF_Grid):: NUOPC_CreateSimpleXYGrid
! !ARGUMENTS:
    real(ESMF_KIND_R8), intent(in)            :: x_min, x_max, y_min, y_max
    integer,            intent(in)            :: i_count, j_count
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Create a very simple XY cartesian Grid.
!
!   The arguments are:
!   \begin{description}
!   \item[x\_min]
!     Lower bound x coordinate.
!   \item[x\_max]
!     Upper bound x coordinate.
!   \item[y\_min]
!     Lower bound y coordinate.
!   \item[y\_max]
!     Upper bound y coordinate.
!   \item[i\_count]
!     Number of elements along x.
!   \item[j\_count]
!     Number of elements along y.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer :: i, j, imin_t, imax_t, jmin_t, jmax_t
    real(ESMF_KIND_R8), pointer :: CoordX(:), CoordY(:)
    real(ESMF_KIND_R8):: dx, dy
    type(ESMF_Grid):: grid
    
    if (present(rc)) rc = ESMF_SUCCESS

    dx = (x_max-x_min)/i_count
    dy = (y_max-y_min)/j_count

    grid = ESMF_GridCreateNoPeriDim(maxIndex=(/i_count,j_count/), &
      coordDep1=(/1/), coordDep2=(/2/), &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      indexflag=ESMF_INDEX_GLOBAL, name="SimpleXY", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! add center stagger
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=1, farrayPtr=coordX, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=2, farrayPtr=coordY, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! compute center stagger coordinate values
    imin_t = lbound(CoordX,1)
    imax_t = ubound(CoordX,1)
    jmin_t = lbound(CoordY,1)
    jmax_t = ubound(CoordY,1)
      
    coordX(imin_t) = x_min + (imin_t-1)*dx + 0.5*dx
    do i = imin_t+1, imax_t
      coordX(i) = coordX(i-1) + dx
    enddo
    coordY(jmin_t) = y_min + (jmin_t-1)*dy + 0.5*dy
    do j = jmin_t+1, jmax_t
      coordY(j) = coordY(j-1) + dy
    enddo
    
    NUOPC_CreateSimpleXYGrid = grid
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_FillField - Fill data into a Field
! !INTERFACE:
  subroutine NUOPC_FillField(field, dataFillScheme, member, step, rc)
! !ARGUMENTS:
    type(ESMF_Field), intent(inout) :: field
    character(len=*), intent(in)    :: dataFillScheme
    integer, intent(in)             :: member
    integer, intent(in)             :: step
    integer, intent(out), optional  :: rc
! !DESCRIPTION:
!   \label{NUOPC_FillField}
!   Fill {\tt field} with data according to {\tt dataFillScheme}. Depending
!   on the chosen fill scheme, the {\tt member} and {\tt step} arguments are
!   used to provide differing fill data patterns.
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object to fill with data.
!   \item[dataFillScheme]
!     The fill scheme. The available options are "sincos", and "one".
!   \item[member]
!     Member incrementor.
!   \item[step]
!     Step incrementor.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Grid)                 :: grid
    type(ESMF_TypeKind_Flag)        :: typekind
    integer                         :: rank
    real(ESMF_KIND_R8), pointer     :: dataPtrR8D1(:)
    real(ESMF_KIND_R8), pointer     :: dataPtrR8D2(:,:)
    real(ESMF_KIND_R8), pointer     :: dataPtrR8D3(:,:,:)
    real(ESMF_KIND_R4), pointer     :: dataPtrR4D1(:)
    real(ESMF_KIND_R4), pointer     :: dataPtrR4D2(:,:)
    real(ESMF_KIND_R4), pointer     :: dataPtrR4D3(:,:,:)
    real(ESMF_KIND_R8), pointer     :: coord1PtrR8D1(:)
    real(ESMF_KIND_R8), pointer     :: coord1PtrR8D2(:,:)
    real(ESMF_KIND_R8), pointer     :: coord2PtrR8D2(:,:)
    real(ESMF_KIND_R8), pointer     :: coord1PtrR8D3(:,:,:)
    real(ESMF_KIND_R8), pointer     :: coord2PtrR8D3(:,:,:)
    real(ESMF_KIND_R8), pointer     :: coord3PtrR8D3(:,:,:)
    integer                         :: i, j, k
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_FieldGet(field, typekind=typekind, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (trim(dataFillScheme)=="sincos") then
      if (typekind==ESMF_TYPEKIND_R8 .and. rank==1) then
        ! 1D sin pattern
        ! TODO: support Meshes
        call ESMF_FieldGet(field, grid=grid, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=coord1PtrR8D1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_FieldGet(field, farrayPtr=dataPtrR8D1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        do i=lbound(dataPtrR8D2,1),ubound(dataPtrR8D2,1)
          dataPtrR8D1(i) = &
            sin(real(member)*3.1416*(coord1PtrR8D1(i)+real(step))/180.)
        enddo
      elseif (typekind==ESMF_TYPEKIND_R8 .and. rank==2) then
        ! 2D sin*cos pattern
        ! TODO: support Meshes
        call ESMF_FieldGet(field, grid=grid, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=coord1PtrR8D2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=coord2PtrR8D2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_FieldGet(field, farrayPtr=dataPtrR8D2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        do j=lbound(dataPtrR8D2,2),ubound(dataPtrR8D2,2)
        do i=lbound(dataPtrR8D2,1),ubound(dataPtrR8D2,1)
          dataPtrR8D2(i,j) = &
            sin(real(member)*3.1416*(coord1PtrR8D2(i,j)+real(step))/180.) * &
            cos(real(member)*3.1416*(coord2PtrR8D2(i,j)+real(step))/180.)
        enddo
        enddo
      elseif (typekind==ESMF_TYPEKIND_R8 .and. rank==3) then
        ! 3D sin*cos*sin pattern
        ! TODO: support Meshes
        call ESMF_FieldGet(field, grid=grid, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=coord1PtrR8D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=coord2PtrR8D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_GridGetCoord(grid, coordDim=3, farrayPtr=coord3PtrR8D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_FieldGet(field, farrayPtr=dataPtrR8D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        do k=lbound(dataPtrR8D3,3),ubound(dataPtrR8D3,3)
        do j=lbound(dataPtrR8D3,2),ubound(dataPtrR8D3,2)
        do i=lbound(dataPtrR8D3,1),ubound(dataPtrR8D3,1)
          dataPtrR8D3(i,j,k) = &
            sin(real(member)*3.1416*(coord1PtrR8D3(i,j,k)+real(step))/180.) * &
            cos(real(member)*3.1416*(coord2PtrR8D3(i,j,k)+real(step))/180.) * &
            sin(real(member)*3.1416*(coord3PtrR8D3(i,j,k)+real(step))/180.)
        enddo
        enddo
        enddo
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Unsupported typekind-rank and scheme combination requested.", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      endif
    else if (trim(dataFillScheme)=="one") then
      if (typekind==ESMF_TYPEKIND_R8 .and. rank==1) then
        ! 1D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR8D1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! initialize the entire array
        dataPtrR8D1 = 1._ESMF_KIND_R8
      elseif (typekind==ESMF_TYPEKIND_R4 .and. rank==1) then
        ! 1D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR4D1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! initialize the entire array
        dataPtrR4D1 = 1._ESMF_KIND_R4
      elseif (typekind==ESMF_TYPEKIND_R8 .and. rank==2) then
        ! 2D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR8D2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! initialize the entire array
        dataPtrR8D2 = 1._ESMF_KIND_R8
      elseif (typekind==ESMF_TYPEKIND_R4 .and. rank==2) then
        ! 2D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR4D2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! initialize the entire array
        dataPtrR4D2 = 1._ESMF_KIND_R4
      elseif (typekind==ESMF_TYPEKIND_R8 .and. rank==3) then
        ! 3D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR8D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! initialize the entire array
        dataPtrR8D3 = 1._ESMF_KIND_R8
      elseif (typekind==ESMF_TYPEKIND_R4 .and. rank==3) then
        ! 3D all 1.
        call ESMF_FieldGet(field, farrayPtr=dataPtrR4D3, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! initialize the entire array
        dataPtrR4D3 = 1._ESMF_KIND_R4
      endif
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Unknown dataFillScheme requested.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    endif
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
