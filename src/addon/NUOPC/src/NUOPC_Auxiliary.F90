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
  
  public NUOPC_ConvertStringToInt         ! method TODO: to cover by ESMF
  public NUOPC_CreateSimpleSphGrid        ! method TODO: to cover by ESMF
  public NUOPC_CreateSimpleXYGrid         ! method TODO: to cover by ESMF
  public NUOPC_FillField                  ! method TODO: to cover by ESMF
  public NUOPC_Write                      ! method
  
!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

  interface NUOPC_Write
    module procedure NUOPC_WriteWeights
    module procedure NUOPC_FieldWrite
    module procedure NUOPC_StateWrite
  end interface
  
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
    i_count, j_count, half_polar_cell, area_adj, regional, rc)
! !RETURN VALUE:
    type(ESMF_Grid):: NUOPC_CreateSimpleSphGrid
! !ARGUMENTS:
    real(ESMF_KIND_R8), intent(in)            :: x_min, x_max, y_min, y_max
    integer,            intent(in)            :: i_count, j_count
    logical,            intent(in),  optional :: half_polar_cell
    real(ESMF_KIND_R4), intent(in),  optional :: area_adj
    logical,            intent(in) , optional :: regional
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
!   \item[{[regional]}]
!     When set to .true., consider this a regional grid. Otherwise apply 
!     periodic boundary conditions (default).
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
    logical                                   :: l_regional
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
    l_regional = .false.
    if(present(regional)) l_regional = regional

    if(.not.l_regional) then
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

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Write - Write Field data to file
! !INTERFACE:
  ! call using generic interface: NUOPC_Write
  subroutine NUOPC_FieldWrite(field, file, overwrite, status, timeslice, &
    iofmt, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_Field),           intent(in)            :: field 
    character(*),               intent(in)            :: file 
    logical,                    intent(in),  optional :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional :: status
    integer,                    intent(in),  optional :: timeslice
    type(ESMF_IOFmt_Flag),      intent(in),  optional :: iofmt
    logical,                    intent(in),  optional :: relaxedflag
    integer,                    intent(out), optional :: rc
! !DESCRIPTION:
!   Write the data in {\tt field} to {\tt file} under the field's "StandardName" 
!   attribute if supported by the {\tt iofmt}.
!
!   The arguments are:
!   \begin{description}
!   \item[field]
!     The {\tt ESMF\_Field} object whose data is to be written.
!   \item[file]
!     The name of the file to write to.
!   \item[{[overwrite]}]
!      A logical flag, the default is .false., i.e., existing Field data may
!      {\em not} be overwritten. If .true., the
!      data corresponding to each field's name will be
!      be overwritten. If the {\tt timeslice} option is given, only data for
!      the given timeslice may be overwritten.
!      Note that it is always an error to attempt to overwrite a NetCDF
!      variable with data which has a different shape.
!   \item[{[status]}]
!      The file status. Valid options are {\tt ESMF\_FILESTATUS\_NEW}, 
!      {\tt ESMF\_FILESTATUS\_OLD}, {\tt ESMF\_FILESTATUS\_REPLACE}, and
!      {\tt ESMF\_FILESTATUS\_UNKNOWN} (default).
!   \item[{[timeslice]}]
!     Time slice counter. Must be positive. The behavior of this
!     option may depend on the setting of the {\tt overwrite} flag:
!     \begin{description}
!     \item[{\tt overwrite = .false.}:]\ If the timeslice value is
!     less than the maximum time already in the file, the write will fail.
!     \item[{\tt overwrite = .true.}:]\ Any positive timeslice value is valid.
!     \end{description}
!     By default, i.e. by omitting the {\tt timeslice} argument, no
!     provisions for time slicing are made in the output file,
!     however, if the file already contains a time axis for the variable,
!     a timeslice one greater than the maximum will be written.
!   \item[{[iofmt]}]
!    The IO format.  Valid options are  {\tt ESMF\_IOFMT\_BIN} and 
!    {\tt ESMF\_IOFMT\_NETCDF}. If not present, file names with a {\tt .bin} 
!    extension will use {\tt ESMF\_IOFMT\_BIN}, and file names with a {\tt .nc}
!    extension will use {\tt ESMF\_IOFMT\_NETCDF}.  Other files default to
!    {\tt ESMF\_IOFMT\_NETCDF}.
!   \item[{[relaxedflag]}]
!     If {\tt .true.}, then no error is returned even if the call cannot write
!     the file due to library limitations. Default is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)  :: standardName
    logical                 :: ioCapable
    logical                 :: doItFlag

    if (present(rc)) rc = ESMF_SUCCESS
    
    ioCapable = (ESMF_IO_PIO_PRESENT .and. &
      (ESMF_IO_NETCDF_PRESENT .or. ESMF_IO_PNETCDF_PRESENT))
    
    doItFlag = .true. ! default
    if (present(relaxedFlag)) then
      doItFlag = .not.relaxedflag .or. (relaxedflag.and.ioCapable)
    endif
    
    if (doItFlag) then
      
      call ESMF_AttributeGet(field, name="StandardName", &
        value=standardName, convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
    
      call ESMF_FieldWrite(field, file=file, variableName=standardName, &
        overwrite=overwrite, status=status, timeslice=timeslice, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Write - Write the Fields within a State to NetCDF files
! !INTERFACE:
  ! call using generic interface: NUOPC_Write
  subroutine NUOPC_StateWrite(state, fieldNameList, filePrefix, overwrite, &
    status, timeslice, relaxedflag, rc)
! !ARGUMENTS:
    type(ESMF_State),           intent(in)            :: state
    character(len=*),           intent(in),  optional :: fieldNameList(:)
    character(len=*),           intent(in),  optional :: filePrefix
    logical,                    intent(in),  optional :: overwrite
    type(ESMF_FileStatus_Flag), intent(in),  optional :: status
    integer,                    intent(in),  optional :: timeslice
    logical,                    intent(in),  optional :: relaxedflag
    integer,                    intent(out), optional :: rc
! !DESCRIPTION:
!   Write the data of the fields within a {\tt state} to NetCDF files. Each 
!   field is written to an individual file using the "StandardName" attribute
!   as NetCDF attribute.
!
!   The arguments are:
!   \begin{description}
!   \item[state]
!     The {\tt ESMF\_State} object containing the fields.
!   \item[{[fieldNameList]}]
!     List of names of the fields to be written. By default write all the fields
!     in {\tt state}.
!   \item[{[filePrefix]}]
!     File name prefix, common to all the files written.
!   \item[{[overwrite]}]
!      A logical flag, the default is .false., i.e., existing Field data may
!      {\em not} be overwritten. If .true., the
!      data corresponding to each field's name will be
!      be overwritten. If the {\tt timeslice} option is given, only data for
!      the given timeslice may be overwritten.
!      Note that it is always an error to attempt to overwrite a NetCDF
!      variable with data which has a different shape.
!   \item[{[status]}]
!      The file status. Valid options are {\tt ESMF\_FILESTATUS\_NEW}, 
!      {\tt ESMF\_FILESTATUS\_OLD}, {\tt ESMF\_FILESTATUS\_REPLACE}, and
!      {\tt ESMF\_FILESTATUS\_UNKNOWN} (default).
!   \item[{[timeslice]}]
!     Time slice counter. Must be positive. The behavior of this
!     option may depend on the setting of the {\tt overwrite} flag:
!     \begin{description}
!     \item[{\tt overwrite = .false.}:]\ If the timeslice value is
!     less than the maximum time already in the file, the write will fail.
!     \item[{\tt overwrite = .true.}:]\ Any positive timeslice value is valid.
!     \end{description}
!     By default, i.e. by omitting the {\tt timeslice} argument, no
!     provisions for time slicing are made in the output file,
!     however, if the file already contains a time axis for the variable,
!     a timeslice one greater than the maximum will be written.
!   \item[{[relaxedflag]}]
!     If {\tt .true.}, then no error is returned even if the call cannot write
!     the file due to library limitations. Default is {\tt .false.}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: i, itemCount
    type(ESMF_Field)                :: field
    type(ESMF_StateItem_Flag)       :: itemType
    character(len=80)               :: fileName
    character(len=80), allocatable  :: fieldNameList_loc(:)

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(fieldNameList)) then
      allocate(fieldNameList_loc(size(fieldNameList)))
      do i=1, size(fieldNameList)
        fieldNameList_loc(i) = trim(fieldNameList(i))
      enddo
    else
      call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate(fieldNameList_loc(itemCount))
      call ESMF_StateGet(state, itemNameList=fieldNameList_loc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    do i=1, size(fieldNameList_loc)
      call ESMF_StateGet(state, itemName=fieldNameList_loc(i), &
        itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      if (itemType == ESMF_STATEITEM_FIELD) then
        ! field is available in the state
        call ESMF_StateGet(state, itemName=fieldNameList_loc(i), field=field, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
        ! -> output to file
        if (present(filePrefix)) then
          write (fileName,"(A)") filePrefix//trim(fieldNameList_loc(i))//".nc"
        else
          write (fileName,"(A)") trim(fieldNameList_loc(i))//".nc"
        endif
        call NUOPC_FieldWrite(field, file=trim(fileName), overwrite=overwrite, &
          status=status, timeslice=timeslice, relaxedflag=relaxedflag, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed writing file: "// &
          trim(fileName), &
          line=__LINE__, &
          file=FILENAME)) &
          return  ! bail out
      endif
    enddo
    
    deallocate(fieldNameList_loc)

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_Write - Write a distributed factorList to file
! !INTERFACE:
  ! call using generic interface: NUOPC_Write
  subroutine NUOPC_WriteWeights(factorList, fileName, rc)
! !ARGUMENTS:
    real(ESMF_KIND_R8), pointer               :: factorList(:)
    character(*),       intent(in)            :: fileName
    integer,            intent(out), optional :: rc
! !DESCRIPTION:
!   Write the destributed {\tt factorList} to file. Each PET calls with its 
!   local list of factors. The call then writes the distributed factors into
!   a single file. The order of the factors in the file is first by PET, and 
!   within each PET the PET-local order is preserved. Changing the number of 
!   PETs for the same regrid operation will likely change the order of factors
!   across PETs, and therefore files written will differ.
!
!   The arguments are:
!   \begin{description}
!   \item[factorList]
!     The distributed factor list.
!   \item[fileName]
!     The name of the file to be written to.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer, allocatable            :: deBlockList(:,:,:), weightsPerPet(:)
    type(ESMF_VM)                   :: vm
    type(ESMF_DistGrid)             :: dg
    type(ESMF_Array)                :: array
    integer                         :: localPet, petCount
    integer                         :: j
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    allocate(weightsPerPet(petCount))
    call ESMF_VMAllGather(vm, (/size(factorList)/), weightsPerPet, &
      count=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    allocate(deBlockList(1,2,petCount))
    do j=1, petCount
      if (j==1) then
        deBlockList(1,1,j) = 1
        deBlockList(1,2,j) = weightsPerPet(1)
      else
        deBlockList(1,1,j) = deBlockList(1,2,j-1) + 1
        deBlockList(1,2,j) = deBlockList(1,1,j) + weightsPerPet(j) - 1
      endif
    enddo
    dg = ESMF_DistGridCreate(minIndex=(/1/), &
      maxIndex=(/deBlockList(1,2,petCount)/), &
      deBlockList=deBlockList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    array = ESMF_ArrayCreate(dg, factorList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_ArrayWrite(array, fileName, variableName="weights", &
      status=ESMF_FILESTATUS_REPLACE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_ArrayDestroy(array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_DistGridDestroy(dg, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    deallocate(weightsPerPet, deBlockList)
    
  end subroutine
  !-----------------------------------------------------------------------------

end module
