#include "ESMF.h"

  module comp_utils
  
  use ESMF
  use ESMF_TestMod

  implicit none
  public user_comp_recreate
  public printCompInfo

  contains

  subroutine printCompInfo(mapper, compInfo, rc)
    type(ESMF_Mapper), intent(in)    :: mapper
    type(ESMF_MapperCompInfo), intent(in)     :: compInfo
    integer,             intent(out)          :: rc

    integer, dimension(:), allocatable      :: petList
    integer :: npets

    call ESMF_MapperGet(mapper, compInfo, npets=npets, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Getting comp info from mapper failed"
      return
    end if

    allocate(petlist(npets))

    call ESMF_MapperGet(mapper, compInfo, petList=petList, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Getting comp petlist from mapper failed"
      return
    end if

    print *, "Optimized pet list : ", petList

    rc = ESMF_SUCCESS
  end subroutine

  subroutine user_comp_recreate(gComp, gCompInfo, mapper, rc)
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)        :: gComp
    type(ESMF_MapperCompInfo), intent(in)     :: gCompInfo
    type(ESMF_Mapper), intent(in)    :: mapper
    integer,             intent(out)          :: rc

    integer, dimension(:), allocatable      :: petlist
    integer :: npets
    character(len=ESMF_MAXSTR) :: cname
    type(ESMF_VM) :: vm

    print *, "Getting info about the gComp : name"
    call ESMF_GridCompGet(gComp, name=cname, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Getting info about gComp failed"
      return
    end if

    print *, "Destroying old gComp"
    call ESMF_GridCompDestroy(gComp, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Destroying gComp failed"
      return
    end if

    call ESMF_MapperGet(mapper, gCompInfo, npets=npets, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Getting comp info from mapper failed"
      return
    end if

    allocate(petlist(npets))

    call ESMF_MapperGet(mapper, gCompInfo, petList=petlist, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Getting comp petlist from mapper failed"
      return
    end if

    print *, "Recreating gComp : petlist = ", petList
    gComp = ESMF_GridCompCreate(name=cname, petList=petList, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Recreating gComp failed"
      return
    end if

    print *, "Successfully recreated gComp :", trim(cname)

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine user_comp_recreate


  end module comp_utils
