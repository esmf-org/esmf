      subroutine ExternalUser_SetServices(gcomp, rc)
        use ESMF_GridCompMod
        use UserGridCompMod

        type(ESMF_GridComp) :: gcomp
        integer :: rc

        call User_SetServices(gcomp, rc)

      end subroutine

