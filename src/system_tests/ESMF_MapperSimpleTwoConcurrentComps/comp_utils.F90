#include "ESMF.h"

  module comp_utils
  
  use ESMF
  use ESMF_TestMod

  implicit none
  public user_comp_recreate

  abstract interface
    subroutine compSetVMInterface(gridcomp, rc)
      use ESMF_CompMod
      implicit none
      type(ESMF_GridComp)  :: gridcomp
      integer, intent(out)  :: rc
    end subroutine compSetVMInterface
  end interface

  abstract interface
    subroutine compRegInterface(gridcomp, rc)
      use ESMF_CompMod
      implicit none
      type(ESMF_GridComp)  :: gridcomp
      integer, intent(out)  :: rc
    end subroutine compRegInterface
  end interface

  contains

  subroutine user_comp_recreate(gComp, compSetVM, compReg, startPet, endPet, mapper, rc)
! !ARGUMENTS:
    type(ESMF_GridComp), intent(inout)        :: gComp
    procedure(compSetVMInterface) :: compSetVM
    procedure(compRegInterface) :: compReg
    integer, intent(in) :: startPet
    integer, intent(in) :: endPet
    type(ESMF_Mapper), intent(in)    :: mapper
    integer,             intent(out)          :: rc

    integer :: localrc, userrc, i
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

    !call ESMF_MapperGet(mapper, gCompInfo, npets=npets, rc=rc)
    !if(rc  /= ESMF_SUCCESS) then
    !  print *, "Getting comp info from mapper failed"
    !  return
    !end if
    npets = endPet - startPet + 1;
    allocate(petlist(npets))
    do i=1,npets
      petlist(i) = startPet + i - 1
    end do

    !call ESMF_MapperGet(mapper, gCompInfo, petList=petlist, rc=rc)
    !if(rc  /= ESMF_SUCCESS) then
    !  print *, "Getting comp petlist from mapper failed"
    !  return
    !end if

    print *, "Recreating gComp : petlist = ", petList
    gComp = ESMF_GridCompCreate(name=cname, petList=petList, rc=rc)
    if(rc  /= ESMF_SUCCESS) then
      print *, "Recreating gComp failed"
      return
    end if

    print *, "Successfully recreated gComp :", trim(cname)

    call ESMF_GridCompSetVM(gComp, userRoutine=compSetVM, &
      userRc=userrc, rc=localrc)
    print *, "Comp SetVM finished, rc= ", localrc, userrc
    if((localrc  /= ESMF_SUCCESS) .or. (userrc /= ESMF_SUCCESS)) then
      print *, "Setting setvm routine failed"
      rc = localrc
      return
    end if

    call ESMF_GridCompSetServices(gComp, userRoutine=compReg, &
      userRc=userrc, rc=localrc)
    print *, "Comp SetServices finished, rc= ", localrc, userrc
    if((localrc  /= ESMF_SUCCESS) .or. (userrc /= ESMF_SUCCESS)) then
      print *, "Setting reg routine failed"
      rc = localrc
      return
    end if

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine user_comp_recreate


  end module comp_utils
