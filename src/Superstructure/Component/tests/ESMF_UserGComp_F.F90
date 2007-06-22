      subroutine ExternalUser_SetServices(gcomp, rc)
        use ESMF_CompMod
        use UserInternGridCompMod

        type(ESMF_InternGridComp) :: gcomp
        integer :: rc

        call User_SetServices(gcomp, rc)

      end subroutine

