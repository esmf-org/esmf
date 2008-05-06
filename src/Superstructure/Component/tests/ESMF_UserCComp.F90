! $Id: ESMF_UserCComp.F90,v 1.5.10.1 2008/05/06 04:31:39 cdeluca Exp $
!
! Test code which supplies a user-written coupler component.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  A skeletal user-written component for testing framework.
!
!
!\begin{verbatim}

    module UserCplCompMod
    
!   ! ESMF Framework module
    use ESMF_Mod
    
    implicit none
    private
    
    public User_SetServices

    type mydata
        integer :: per_instance_data
    end type

    type datawrapper
        type(mydata), pointer :: wrap
    end type

    contains

    subroutine User_SetServices(ccomp, rc)
       type(ESMF_CplComp) :: ccomp
       integer :: rc
       type(mydata), pointer :: privatedata
       type(datawrapper) :: wrapper

       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETINIT, my_init, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETRUN, my_run, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_CplCompSetEntryPoint(ccomp, ESMF_SETFINAL, my_final, &
                                                     ESMF_SINGLEPHASE, rc)

       allocate(privatedata)
       wrapper%wrap => privatedata

       call ESMF_CplCompSetInternalState(ccomp, wrapper, rc)

    end subroutine User_SetServices


    subroutine my_init(ccomp, import, export, externalclock, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: import, export
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      type(ESMF_Field) :: field1, field2

      call ESMF_StateGet(import, "fieldname1", field1, rc=rc)
      call ESMF_StateGet(export, "fieldname2", field2, rc=rc)

    end subroutine my_init


    subroutine my_run(ccomp, import, export, externalclock, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: import, export
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      type(ESMF_Field) :: field1, field2

      call ESMF_StateGet(import, "fieldname1", field1, rc=rc)
      call ESMF_StateGet(export, "fieldname2", field2, rc=rc)


    end subroutine my_run


    subroutine my_final(ccomp, import, export, externalclock, rc)
      type(ESMF_CplComp) :: ccomp
      type(ESMF_State) :: import, export
      type(ESMF_Clock) :: externalclock
      integer :: rc
     
      type(ESMF_Field) :: field1, field2

      call ESMF_StateGet(import, "fieldname1", field1, rc=rc)
      call ESMF_StateGet(export, "fieldname2", field2, rc=rc)


    end subroutine my_final


    end module UserCplCompMod
    
!\end{verbatim}
    
