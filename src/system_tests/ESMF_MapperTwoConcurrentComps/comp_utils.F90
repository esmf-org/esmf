#include "ESMF.h"

  module comp_utils
  
  use ESMF
  use ESMF_TestMod

  implicit none
  public user_comp_recreate

  contains

  subroutine user_comp_recreate(gridcomp, mapper, rc)
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)        :: gridcomp
    type(ESMF_Mapper), intent(in)    :: mapper
    integer,             intent(out)          :: rc

    integer, dimension(:), allocatable      :: petlist
    integer :: npets
    character(len=ESMF_MAXSTR) :: cname
    type(ESMF_VM) :: vm

    print *, "Getting info about the gridcomp : name"
    call ESMF_GridCompGet(gridcomp, name=cname, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Getting info about gridcomp failed"
      return
    end if

    print *, "Destroying old gridcomp"
    call ESMF_GridCompDestroy(gridcomp, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Destroying gridcomp failed"
      return
    end if

    call ESMF_MapperGet(mapper, gridcomp, npets=npets, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Getting comp info from mapper failed"
      return
    end if

    allocate(petlist(npets))

    call ESMF_MapperGet(mapper, gridcomp, petList=petlist, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Getting comp petlist from mapper failed"
      return
    end if

    print *, "Recreating gridcomp : petlist = ", petList
    gridcomp = ESMF_GridCompCreate(name=cname, petList=petList, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Recreating gridcomp failed"
      return
    end if

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine user_comp_recreate


  end module comp_utils
