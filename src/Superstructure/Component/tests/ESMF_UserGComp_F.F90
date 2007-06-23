      subroutine ExternalUser_SetServices(gcomp, rc)
        use ESMF_CompMod
        use UserIGridCompMod

        type(ESMF_IGridComp) :: gcomp
        integer :: rc

        call User_SetServices(gcomp, rc)

      end subroutine

