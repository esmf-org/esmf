module dataProcess

  !-----------------------------------------------------------------------------
  ! Data Processing
  !-----------------------------------------------------------------------------

  use ESMF

  implicit none

  private

  public process

  !-----------------------------------------------------------------------------

  contains

  !-----------------------------------------------------------------------------

  subroutine process(importState, expression, exportField, step, rc)
    ! Process according to the expression infix string and store in exportField

    type(ESMF_State), intent(in)    :: importState
    character(len=*), intent(in)    :: expression
    type(ESMF_Field), intent(inout) :: exportField
    integer,          intent(in)    :: step
    integer,          intent(out)   :: rc

    type(ESMF_Field)                :: importField
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtrImportI4(:)
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtrExportI4(:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtrImportI8(:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtrExportI8(:)
    real(ESMF_KIND_R4), pointer, contiguous    :: fPtrImportR4(:)
    real(ESMF_KIND_R4), pointer, contiguous    :: fPtrExportR4(:)
    real(ESMF_KIND_R8), pointer, contiguous    :: fPtrImportR8(:)
    real(ESMF_KIND_R8), pointer, contiguous    :: fPtrExportR8(:)
    real(ESMF_KIND_R8)            :: value
    character(len=:), allocatable :: infix_expression, rpn_expression
    character(len=:), allocatable :: token, tempString
    integer                       :: count, cur, top, depth
    type(ESMF_TYPEKIND_Flag)      :: tkImport, tkExport
    real(ESMF_KIND_R8), allocatable :: stack(:,:)

    rc = ESMF_SUCCESS

    ! Normalize the incoming infix string with single white space deliminators
    call normalize_infix(expression, infix_expression)

    ! Convert standard infix notation to reverse polish notation
    call infix_to_rpn(infix_expression, rpn_expression)

    ! Determine the required stack depth for RPN processing
    depth = compute_rpn_depth(rpn_expression)

    ! Setup export pointer
    call ESMF_FieldGet(exportField, typekind=tkExport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (tkExport == ESMF_TYPEKIND_I4) then
      call access_data_i4(exportField, fPtrExportI4, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      count = size(fPtrExportI4)
    else if (tkExport == ESMF_TYPEKIND_I8) then
      call access_data_i8(exportField, fPtrExportI8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      count = size(fPtrExportI8)
    else if (tkExport == ESMF_TYPEKIND_R4) then
      call access_data_r4(exportField, fPtrExportR4, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      count = size(fPtrExportR4)
    else if (tkExport == ESMF_TYPEKIND_R8) then
      call access_data_r8(exportField, fPtrExportR8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      count = size(fPtrExportR8)
    else
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
        msg="process() only supports I4, I8, R4, and R8.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

    ! Setup workspace stack
    allocate(stack(count, depth))

    ! Evaluate RPN
    top = 0
    cur = 1
    do while (cur <= len_trim(rpn_expression))
      call get_next_token(rpn_expression, cur, token)
      if (token == "") exit

      select case (token)
      case ("+")
        stack(:,top-1) = stack(:,top-1) + stack(:,top)
        top = top - 1
      case ("-")
        stack(:,top-1) = stack(:,top-1) - stack(:,top)
        top = top - 1
      case ("*")
        stack(:,top-1) = stack(:,top-1) * stack(:,top)
        top = top - 1
      case ("/")
        stack(:,top-1) = stack(:,top-1) / stack(:,top)
        top = top - 1
      case default
        top = top + 1
        if (try_parse(token, value)) then
          ! Numerical value
          stack(:,top) = value
        else if (token(1:1) == "_") then
          ! Special variable
          tempString = ESMF_UtilStringUpperCase(token, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (tempString == "_STEP") then
            ! Step
            stack(:,top) = real(step, ESMF_KIND_R8)
          else if (tempString(1:6) == "_COORD") then
            ! Coordinate
            call push_coord(exportField, token, stack(:,top), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return
          else
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="Unknown special variable: "//token, &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
          end if
        else
          ! Field in importState
          call ESMF_StateGet(importState, itemName=token, field=importField, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldGet(importField, typekind=tkImport, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (tkImport == ESMF_TYPEKIND_I4) then
            call access_data_i4(importField, fPtrImportI4, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            stack(:,top) = fPtrImportI4
          else if (tkImport == ESMF_TYPEKIND_I8) then
            call access_data_i8(importField, fPtrImportI8, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            stack(:,top) = fPtrImportI8
          else if (tkImport == ESMF_TYPEKIND_R4) then
            call access_data_r4(importField, fPtrImportR4, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            stack(:,top) = fPtrImportR4
          else if (tkImport == ESMF_TYPEKIND_R8) then
            call access_data_r8(importField, fPtrImportR8, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            stack(:,top) = fPtrImportR8
          else
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
              msg="process() only supports I4, I8, R4, and R8.", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return  ! bail out
          end if
        end if
      end select
    end do

    ! Stored final result in the export field
    if (tkExport == ESMF_TYPEKIND_I4) then
      fPtrExportI4 = stack(:,1)
    else if (tkExport == ESMF_TYPEKIND_I8) then
      fPtrExportI8 = stack(:,1)
    else if (tkExport == ESMF_TYPEKIND_R4) then
      fPtrExportR4 = stack(:,1)
    else if (tkExport == ESMF_TYPEKIND_R8) then
      fPtrExportR8 = stack(:,1)
    end if

    ! clean-up workspace stack
    deallocate(stack)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine push_coord(field, token, stackColumn, rc)
    type(ESMF_Field),   intent(in)  :: field
    character(len=*),   intent(in)  :: token
    real(ESMF_KIND_R8), intent(out) :: stackColumn(:)
    integer,            intent(out) :: rc

    integer                         :: coordDim
    type(ESMF_Grid)                 :: grid
    type(ESMF_Mesh)                 :: mesh
    type(ESMF_GeomType_Flag)        :: geomtype
    type(ESMF_StaggerLoc)           :: staggerloc
    type(ESMF_MeshLoc)              :: meshloc
    integer                         :: dimCount, m, i, j, k, idx
    integer                         :: inner_repeat, outer_replicate
    integer, allocatable            :: coordDimCount(:), exclusiveCount(:)
    integer                         :: numOwnedPoints
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr(:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr1D(:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr2D(:,:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr3D(:,:,:)

    rc = ESMF_SUCCESS

    ! Extract digit from "_coordX"
    read(token(7:), *, iostat=rc) coordDim

    call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (geomtype==ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, grid=grid, staggerloc=staggerloc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_GridGet(grid, dimCount=dimCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(coordDimCount(dimCount))
      call ESMF_GridGet(grid, coordDimCount=coordDimCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (coordDimCount(coordDim)==1) then
        allocate(exclusiveCount(dimCount))
        call ESMF_GridGet(grid, staggerloc=staggerloc, localDE=0, &
          exclusiveCount=exclusiveCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_GridGetCoord(grid, coordDim=coordDim, staggerloc=staggerloc, &
          farrayPtr=fPtr1D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        m = size(fPtr1D)
        inner_repeat = product(exclusiveCount(1:coordDim-1))
        outer_replicate = product(exclusiveCount(coordDim+1:dimCount))
        ! Populate stackColumn with replicated fPtr1D data
        idx = 1
        do k = 1, outer_replicate
          do j = 1, m
            do i = 1, inner_repeat
              stackColumn(idx) = fPtr1D(lbound(fPtr1D,1)-1+j)
              idx = idx + 1
            end do
          end do
        end do
      else if (coordDimCount(coordDim)==2) then
        call ESMF_GridGetCoord(grid, coordDim=coordDim, staggerloc=staggerloc, &
          farrayPtr=fPtr2D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        ! Reinterpret
        fPtr(1:size(fPtr2D)) => fPtr2D(:, :)
        ! Copy into stackColumn
        stackColumn(:) = fPtr
      else if (coordDimCount(coordDim)==3) then
        call ESMF_GridGetCoord(grid, coordDim=coordDim, staggerloc=staggerloc, &
          farrayPtr=fPtr3D, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        ! Reinterpret
        fPtr(1:size(fPtr3D)) => fPtr3D(:, :, :)
        ! Copy into stackColumn
        stackColumn(:) = fPtr
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Unsupported coordDimCount detected.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return ! bail out
      endif
    elseif (geomtype==ESMF_GEOMTYPE_MESH) then
      call ESMF_FieldGet(field, mesh=mesh, meshloc=meshloc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (meshloc==ESMF_MESHLOC_ELEMENT) then
        call ESMF_MeshGet(mesh, spatialDim=dimCount, &
          numOwnedElements=numOwnedPoints, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      elseif (meshloc==ESMF_MESHLOC_NODE) then
        call ESMF_MeshGet(mesh, spatialDim=dimCount, &
          numOwnedNodes=numOwnedPoints, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Unsupported MESHLOC detected.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return ! bail out
      endif
      if (dimCount==1) then
        ! Directly fill stackColumn
        if (meshloc==ESMF_MESHLOC_ELEMENT) then
          call ESMF_MeshGet(mesh, ownedElemCoords=stackColumn, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          call ESMF_MeshGet(mesh, ownedNodeCoords=stackColumn, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
      else
        ! Require temporary fPtr1D
        allocate(fPtr1D(dimCount*numOwnedPoints))
        if (meshloc==ESMF_MESHLOC_ELEMENT) then
          call ESMF_MeshGet(mesh, ownedElemCoords=fPtr1D, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          call ESMF_MeshGet(mesh, ownedNodeCoords=fPtr1D, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        stackColumn = fPtr1D(coordDim::dimCount)  ! copy the coorDim entries
        deallocate(fPtr1D)
      endif
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Unsupported geomtype detected.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine normalize_infix(input, output)
    ! Normalize the incoming infix string with single white space deliminators
    character(len=*),               intent(in)  :: input
    character(len=:), allocatable,  intent(out) :: output
    character                       :: c
    integer                         :: i
    logical                         :: needs_leading_zero, in_operand

    output = ""
    needs_leading_zero = .true.
    in_operand = .false.

    do i = 1, len_trim(input)
      c = input(i:i)  ! current character

      if (c == " ") then
        if (in_operand) then
          ! finish operand by adding trailing space
          output = output // " "
          in_operand = .false.
        end if
        cycle
      end if

      if (is_boundary(c)) then
        if (in_operand) then
          ! finish operand by adding trailing space
          output = output // " "
          in_operand = .false.
        end if
        ! Handle Unary Plus and Minus
        if ((c == "-" .or. c == "+") .and. needs_leading_zero) then
          ! insert the leading zero with space
          output = output // "0 "
        end if
        ! Add operator and trailing space
        output = output // c // " "
        ! Update the flag for next iteration according to operator
        needs_leading_zero = (c /= ")")
      else
        ! Inside operand
        in_operand = .true.
        output = output // c
        needs_leading_zero = .false.
      end if
    end do

    ! Remove trailing space
    output = trim(output)

  end subroutine

  !-----------------------------------------------------------------------------

  logical function is_boundary(ch)
    ! Look for boundary character
    character, intent(in) :: ch
    select case (ch)
      case ("+", "-", "*", "/", "(", ")")
        is_boundary = .true.
      case default
        is_boundary = .false.
    end select
  end function

  !-----------------------------------------------------------------------------

  subroutine infix_to_rpn(infix, rpn)
    ! Convert standard infix notation to reverse polish notation
    character(len=*), intent(in)               :: infix
    character(len=:), allocatable, intent(out) :: rpn
    character(len=128):: op_stack(20) ! Stack for operators
    integer           :: stack_ptr
    character(len=:), allocatable :: token
    integer           :: cur, i

    rpn = ""
    stack_ptr = 0
    cur = 1

    do while (cur <= len_trim(infix))
      ! Extract next space-separated token
      call get_next_token(infix, cur, token)
      if (len(token) == 0) exit

      if (is_operator(token)) then
        ! Handle Operators
        do while (stack_ptr > 0)
          if (op_stack(stack_ptr) /= "(" .and. &
              precedence(op_stack(stack_ptr)) >= precedence(token)) then
            rpn = rpn // trim(op_stack(stack_ptr)) // " "
            stack_ptr = stack_ptr - 1
          else
            exit
          end if
        end do
        stack_ptr = stack_ptr + 1
        op_stack(stack_ptr) = token

      else if (token == "(") then
        ! Handle Left Parenthesis
        stack_ptr = stack_ptr + 1
        op_stack(stack_ptr) = "("

      else if (token == ")") then
        ! Handle Right Parenthesis
        do while (stack_ptr > 0 .and. op_stack(stack_ptr) /= "(")
          rpn = rpn // trim(op_stack(stack_ptr)) // " "
          stack_ptr = stack_ptr - 1
        end do
        if (stack_ptr > 0) stack_ptr = stack_ptr - 1 ! Pop the "("

      else
        ! Number or field name - send straight to output
        rpn = rpn // token // " "
      end if
    end do

    ! Pop remaining operators from stack
    do i = stack_ptr, 1, -1
      rpn = rpn // trim(op_stack(i)) // " "
    end do

    ! Remove trailing space
    rpn = trim(rpn)

  end subroutine

  !-----------------------------------------------------------------------------

  integer function compute_rpn_depth(rpn)
    ! Find high-water mark for a dryrun RPN execution
    character(len=*), intent(in)  :: rpn
    character(len=:), allocatable :: token
    integer                       :: cur, current_depth, max_depth

    max_depth = 0
    current_depth = 0
    cur = 1

    do while (cur <= len_trim(rpn))
      ! Extract next token from the RPN string
      call get_next_token(rpn, cur, token)
      if (token == "") exit

      if (is_operator(token)) then
        ! Binary operators (+, -, *, /) pop two operands and push one result.
        ! This results in a net change of -1 to the stack height.
        current_depth = current_depth - 1
      else
        ! Field names or numeric constants are pushed onto the stack.
        ! This results in a net change of +1.
        current_depth = current_depth + 1
      end if

      ! Update the "high-water mark"
      if (current_depth > max_depth) max_depth = current_depth
    end do

    compute_rpn_depth = max_depth

  end function

  !-----------------------------------------------------------------------------

  subroutine get_next_token(str, cur, token)
    ! Look for the next token in a white space separated string
    character(len=*), intent(in)               :: str
    integer, intent(inout)                     :: cur
    character(len=:), allocatable, intent(out) :: token

    integer :: next_s

    if (cur > len(str)) then
      token = ""; return
    end if

    next_s = index(str(cur:), " ")

    if (next_s == 0) then
      token = str(cur:)
      cur = len(str) + 1
    else
      token = str(cur : cur + next_s - 2)
      cur = cur + next_s
    end if

  end subroutine

  !-----------------------------------------------------------------------------

  logical function try_parse(token, value)
    ! Try to parse the token as a numerical constant
    character(len=*), intent(in)    :: token
    real(ESMF_KIND_R8), intent(out) :: value
    character(len=128)              :: buffer
    integer                         :: ios

    buffer = adjustl(token) ! use fixed size buffer for read
    read(buffer, *, iostat=ios) value
    try_parse = (ios == 0)
  end function try_parse

  !-----------------------------------------------------------------------------

  integer function precedence(op)
    ! Operator precendece
    character(len=*), intent(in) :: op
    select case (trim(op))
      case ("+", "-") ; precedence = 2
      case ("*", "/") ; precedence = 3
      case default    ; precedence = 0
    end select
  end function

  !-----------------------------------------------------------------------------

  logical function is_operator(token)
    ! Identify token as operator
    character(len=*), intent(in) :: token
    select case (trim(token))
      case ("+", "-", "*", "/") ; is_operator = .true.
      case default              ; is_operator = .false.
    end select
  end function

  !-----------------------------------------------------------------------------

  subroutine access_data_i4(field, fPtr, rc)
    ! Access field data as 1D contigous data array
    type(ESMF_Field),                           intent(in)   :: field
    integer(ESMF_KIND_I4), pointer, contiguous, intent(out)  :: fPtr(:)
    integer,                                    intent(out)  :: rc

    integer                                    :: rank
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr2D(:,:)
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr3D(:,:,:)
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr4D(:,:,:,:)
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr5D(:,:,:,:,:)
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr6D(:,:,:,:,:,:)
    integer(ESMF_KIND_I4), pointer, contiguous :: fPtr7D(:,:,:,:,:,:,:)

    call ESMF_FieldGet(field, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (rank == 1) then
      call ESMF_FieldGet(field, farrayPtr=fPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else if (rank == 2) then
      call ESMF_FieldGet(field, farrayPtr=fPtr2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr2D)) => fPtr2D(:, :)
    else if (rank == 3) then
      call ESMF_FieldGet(field, farrayPtr=fPtr3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr3D)) => fPtr3D(:, :,:)
    else if (rank == 4) then
      call ESMF_FieldGet(field, farrayPtr=fPtr4D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr4D)) => fPtr4D(:, :,:,:)
    else if (rank == 5) then
      call ESMF_FieldGet(field, farrayPtr=fPtr5D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr5D)) => fPtr5D(:, :,:,:,:)
    else if (rank == 6) then
      call ESMF_FieldGet(field, farrayPtr=fPtr6D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr6D)) => fPtr6D(:, :,:,:,:,:)
    else if (rank == 7) then
      call ESMF_FieldGet(field, farrayPtr=fPtr7D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr7D)) => fPtr7D(:, :,:,:,:,:,:)
    end if
  end subroutine

  subroutine access_data_i8(field, fPtr, rc)
    ! Access field data as 1D contigous data array
    type(ESMF_Field),                           intent(in)   :: field
    integer(ESMF_KIND_I8), pointer, contiguous, intent(out)  :: fPtr(:)
    integer,                                    intent(out)  :: rc

    integer                                    :: rank
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr2D(:,:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr3D(:,:,:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr4D(:,:,:,:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr5D(:,:,:,:,:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr6D(:,:,:,:,:,:)
    integer(ESMF_KIND_I8), pointer, contiguous :: fPtr7D(:,:,:,:,:,:,:)

    call ESMF_FieldGet(field, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (rank == 1) then
      call ESMF_FieldGet(field, farrayPtr=fPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else if (rank == 2) then
      call ESMF_FieldGet(field, farrayPtr=fPtr2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr2D)) => fPtr2D(:, :)
    else if (rank == 3) then
      call ESMF_FieldGet(field, farrayPtr=fPtr3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr3D)) => fPtr3D(:, :,:)
    else if (rank == 4) then
      call ESMF_FieldGet(field, farrayPtr=fPtr4D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr4D)) => fPtr4D(:, :,:,:)
    else if (rank == 5) then
      call ESMF_FieldGet(field, farrayPtr=fPtr5D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr5D)) => fPtr5D(:, :,:,:,:)
    else if (rank == 6) then
      call ESMF_FieldGet(field, farrayPtr=fPtr6D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr6D)) => fPtr6D(:, :,:,:,:,:)
    else if (rank == 7) then
      call ESMF_FieldGet(field, farrayPtr=fPtr7D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr7D)) => fPtr7D(:, :,:,:,:,:,:)
    end if
  end subroutine

  subroutine access_data_r4(field, fPtr, rc)
    ! Access field data as 1D contigous data array
    type(ESMF_Field),                        intent(in)   :: field
    real(ESMF_KIND_R4), pointer, contiguous, intent(out)  :: fPtr(:)
    integer,                                 intent(out)  :: rc

    integer                                 :: rank
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr2D(:,:)
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr3D(:,:,:)
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr4D(:,:,:,:)
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr5D(:,:,:,:,:)
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr6D(:,:,:,:,:,:)
    real(ESMF_KIND_R4), pointer, contiguous :: fPtr7D(:,:,:,:,:,:,:)

    call ESMF_FieldGet(field, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (rank == 1) then
      call ESMF_FieldGet(field, farrayPtr=fPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else if (rank == 2) then
      call ESMF_FieldGet(field, farrayPtr=fPtr2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr2D)) => fPtr2D(:, :)
    else if (rank == 3) then
      call ESMF_FieldGet(field, farrayPtr=fPtr3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr3D)) => fPtr3D(:, :,:)
    else if (rank == 4) then
      call ESMF_FieldGet(field, farrayPtr=fPtr4D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr4D)) => fPtr4D(:, :,:,:)
    else if (rank == 5) then
      call ESMF_FieldGet(field, farrayPtr=fPtr5D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr5D)) => fPtr5D(:, :,:,:,:)
    else if (rank == 6) then
      call ESMF_FieldGet(field, farrayPtr=fPtr6D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr6D)) => fPtr6D(:, :,:,:,:,:)
    else if (rank == 7) then
      call ESMF_FieldGet(field, farrayPtr=fPtr7D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr7D)) => fPtr7D(:, :,:,:,:,:,:)
    end if
  end subroutine

  subroutine access_data_r8(field, fPtr, rc)
    ! Access field data as 1D contigous data array
    type(ESMF_Field),                        intent(in)   :: field
    real(ESMF_KIND_R8), pointer, contiguous, intent(out)  :: fPtr(:)
    integer,                                 intent(out)  :: rc

    integer                                 :: rank
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr2D(:,:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr3D(:,:,:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr4D(:,:,:,:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr5D(:,:,:,:,:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr6D(:,:,:,:,:,:)
    real(ESMF_KIND_R8), pointer, contiguous :: fPtr7D(:,:,:,:,:,:,:)

    call ESMF_FieldGet(field, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (rank == 1) then
      call ESMF_FieldGet(field, farrayPtr=fPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else if (rank == 2) then
      call ESMF_FieldGet(field, farrayPtr=fPtr2D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr2D)) => fPtr2D(:, :)
    else if (rank == 3) then
      call ESMF_FieldGet(field, farrayPtr=fPtr3D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr3D)) => fPtr3D(:, :,:)
    else if (rank == 4) then
      call ESMF_FieldGet(field, farrayPtr=fPtr4D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr4D)) => fPtr4D(:, :,:,:)
    else if (rank == 5) then
      call ESMF_FieldGet(field, farrayPtr=fPtr5D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr5D)) => fPtr5D(:, :,:,:,:)
    else if (rank == 6) then
      call ESMF_FieldGet(field, farrayPtr=fPtr6D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr6D)) => fPtr6D(:, :,:,:,:,:)
    else if (rank == 7) then
      call ESMF_FieldGet(field, farrayPtr=fPtr7D, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      fPtr(1:size(fPtr7D)) => fPtr7D(:, :,:,:,:,:,:)
    end if
  end subroutine

  !-----------------------------------------------------------------------------

end module dataProcess
