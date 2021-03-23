interface

  function c_info_base_get(base_address) bind(C, name="ESMC_BaseGetInfo")
    use iso_c_binding
    implicit none
    integer(C_LONG), intent(in) :: base_address
    type(C_PTR) :: c_info_base_get
  end function c_info_base_get

  !=============================================================================

  function c_info_copy(info, rc) bind(C, name="ESMC_InfoCopy")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    integer(C_INT), intent(out) :: rc
    type(C_PTR) :: c_info_copy
  end function c_info_copy

  !=============================================================================

  function c_info_create(rc) bind(C, name="ESMC_InfoCreate")
    use iso_c_binding
    implicit none
    integer(C_INT), intent(out) :: rc
    type(C_PTR) :: c_info_create
  end function c_info_create

  function c_info_create_by_key(srcInfo, key, rc) bind(C, name="ESMC_InfoCreateByKey")
    use iso_c_binding
    implicit none
    type(C_PTR), value, intent(in) :: srcInfo
    character(C_CHAR), intent(in) :: key(*)
    integer(C_INT), intent(out) :: rc
    type(C_PTR) :: c_info_create_by_key
  end function c_info_create_by_key

  function c_info_create_by_parse(payload, rc) bind(C, name="ESMC_InfoCreateByParse")
    use iso_c_binding
    implicit none
    character(C_CHAR), intent(in) :: payload(*)
    integer(C_INT), intent(out) :: rc
    type(C_PTR) :: c_info_create_by_parse
  end function c_info_create_by_parse

  !=============================================================================

  subroutine c_info_broadcast(info, rootPet, rc) bind(C, name="ESMC_InfoBroadcast")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    integer(C_INT), intent(in) :: rootPet
    integer(C_INT), intent(out) :: rc
  end subroutine

  !=============================================================================
  
  subroutine c_info_copyforattribute(isrc, idst, rc) bind(C, name="ESMC_InfoCopyForAttribute")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: isrc
    type(C_PTR), value :: idst
    integer(C_INT), intent(out) :: rc
  end subroutine

  subroutine c_info_copyforattribute_reference(src_base_address, dst_base_address, rc) bind(C, name="ESMC_InfoCopyForAttributeReference")
    use iso_c_binding
    implicit none
    integer(C_LONG) :: src_base_address
    integer(C_LONG) :: dst_base_address
    integer(C_INT), intent(out) :: rc
  end subroutine
  
  !=============================================================================

  subroutine c_info_destroy(info, rc) bind(C, name="ESMC_InfoDestroy")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    integer(C_INT), intent(out) :: rc
  end subroutine c_info_destroy

  !=============================================================================

  subroutine c_info_dump(info, output, rc, indent) bind(C, name="ESMC_InfoDump")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(C_CHAR), intent(out) :: output(*)
    integer(C_INT), intent(out) :: rc
    integer(C_INT), intent(in) :: indent
  end subroutine c_info_dump

  subroutine c_info_dump_len(info, dump_length, rc, indent) bind(C, name="ESMC_InfoDumpLength")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    integer(C_INT), intent(out) :: dump_length
    integer(C_INT), intent(out) :: rc
    integer(C_INT), intent(in) :: indent
  end subroutine c_info_dump_len

  !=============================================================================

  subroutine c_info_erase(info, keyParent, keyChild, recursive, rc) bind(C, name="ESMC_InfoErase")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(C_CHAR), intent(in) :: keyParent(*)
    character(C_CHAR), intent(in) :: keyChild(*)
    logical(C_BOOL), intent(in) :: recursive
    integer(C_INT), intent(out) :: rc
  end subroutine c_info_erase

  !=============================================================================

  subroutine c_info_inquire(info, inq, key, recursive, idx, attr_compliance, rc) bind(C, name="ESMC_InfoInquire")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    type(C_PTR), value :: inq
    character(C_CHAR), intent(in) :: key(*)
    integer(C_INT), intent(in) :: recursive
    type(C_PTR), value :: idx
    integer(C_INT), intent(in) :: attr_compliance
    integer(C_INT), intent(out) :: rc
  end subroutine c_info_inquire

  !=============================================================================

  subroutine c_info_is_equal(lhs, rhs, is_equal, localrc) bind (C, name="ESMC_InfoIsEqual")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: lhs
    type(C_PTR), value :: rhs
    integer(C_INT), intent(inout) :: is_equal
    integer(C_INT), intent(inout) :: localrc
  end subroutine c_info_is_equal

  !=============================================================================

  subroutine c_info_is_present(info, key, res, rc, recursive, isptr) bind(C, name="ESMC_InfoIsPresent")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(C_CHAR), intent(in) :: key(*)
    integer(C_INT), intent(inout) :: res
    integer(C_INT), intent(out) :: rc
    integer(C_INT), intent(in) :: recursive
    integer(C_INT), intent(in) :: isptr
  end subroutine c_info_is_present

  !=============================================================================

  subroutine c_info_get_tk(info, key, typekind, rc, recursive) bind(C, name="ESMC_InfoGetTK")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(C_CHAR), intent(in) :: key(*)
    integer(C_INT), intent(out) :: typekind
    integer(C_INT), intent(out) :: rc
    integer(C_INT), intent(in) :: recursive
  end subroutine c_info_get_tk

  !=============================================================================

  subroutine c_info_get_array_meta(info, key, is_array, size, recursive, rc) bind(C, name="ESMC_InfoGetArrayMeta")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(C_CHAR), intent(in) :: key(*)
    integer(C_INT), intent(out) :: is_array
    integer(C_INT), intent(out) :: size
    integer(C_INT), intent(in) :: recursive
    integer(C_INT), intent(out) :: rc
  end subroutine c_info_get_array_meta

  !=============================================================================

  subroutine c_info_is_set(info, key, is_set_c, rc) bind(C, name="ESMC_InfoIsSet")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(C_CHAR), intent(in) :: key(*)
    integer(C_INT), intent(inout) :: is_set_c
    integer(C_INT), intent(out) :: rc
  end subroutine c_info_is_set

  !=============================================================================

  subroutine c_info_update(lhs, rhs, recursive_int, overwrite_int, rc) bind(C, name="ESMC_InfoUpdate")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: lhs
    type(C_PTR), value :: rhs
    integer(C_INT), intent(in) :: recursive_int
    integer(C_INT), intent(in) :: overwrite_int
    integer(C_INT), intent(out) :: rc
  end subroutine c_info_update

  !=============================================================================

  subroutine c_info_base_sync(inqstate, rootPet, vmAddress, markClean, rc) bind(C, name="ESMC_InfoBaseSync")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: inqstate
    integer(C_INT), intent(in) :: rootPet
    integer(C_LONG), intent(in) :: vmAddress
    integer(C_INT), intent(in) :: markClean
    integer(C_INT), intent(out) :: rc
  end subroutine

  !=============================================================================

  subroutine c_info_read_json(info, filename, rc) bind(C, name="ESMC_InfoReadJSON")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(C_CHAR), intent(in) :: filename(*)
    integer(C_INT), intent(out) :: rc
  end subroutine c_info_read_json

  subroutine c_info_write_json(info, filename, rc) bind(C, name="ESMC_InfoWriteJSON")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(C_CHAR), intent(in) :: filename(*)
    integer(C_INT), intent(out) :: rc
  end subroutine c_info_write_json

  !=============================================================================

  subroutine c_info_get_CH(info, key, value, vlen, rc, default, idx, recursive, strlen_only) bind(C, name="ESMC_InfoGetCH")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(C_CHAR), intent(in) :: key(*)
    character(C_CHAR), intent(inout) :: value(*)
    integer(C_INT), intent(in) :: vlen
    integer(C_INT), intent(out) :: rc
    type(C_PTR), value :: default, idx
    integer(C_INT), intent(in) :: recursive
    integer(C_INT), intent(in) :: strlen_only
  end subroutine c_info_get_CH

  !=============================================================================

  subroutine c_info_set_INFO(info, key, value, force, rc) bind(C, name="ESMC_InfoSetINFO")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(kind=C_CHAR), intent(in) :: key(*)
    type(C_PTR), value :: value
    logical(C_BOOL), intent(in) :: force
    integer(C_INT), intent(out) :: rc
  end subroutine c_info_set_INFO

  subroutine c_info_set_NULL(info, key, force, rc) bind(C, name="ESMC_InfoSetNULL")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(kind=C_CHAR), intent(in) :: key(*)
    logical(C_BOOL), intent(in) :: force
    integer(C_INT), intent(out) :: rc
  end subroutine c_info_set_NULL

  subroutine c_info_set_array_CH(info, key, itemcount, force, rc, pkey) bind(C, name="ESMC_InfoSetArrayCH")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    character(kind=C_CHAR), intent(in) :: key(*)
    integer(C_INT), intent(in) :: itemcount
    logical(C_BOOL), intent(in) :: force
    integer(C_INT), intent(out) :: rc
    character(kind=C_CHAR), intent(in) :: pkey(*)
  end subroutine c_info_set_array_CH

  !=============================================================================

  subroutine c_info_set_dirty(info, flag, rc) bind(C, name="ESMC_InfoSetDirty")
    use iso_c_binding
    implicit none
    type(C_PTR), value :: info
    integer(C_INT), intent(in) :: flag
    integer(C_INT), intent(out) :: rc
  end subroutine c_info_set_dirty

end interface
