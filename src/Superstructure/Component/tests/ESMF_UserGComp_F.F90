      subroutine ExternalUser_SetServices(gcomp, rc)
        use ESMF_CompMod
        use UserGridCompMod

        type(ESMF_GridComp) :: gcomp
        integer :: rc

        call User_SetServices(gcomp, rc)

      end subroutine

