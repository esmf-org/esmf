program test_log_2
!include "ESMF_LogErr.inc"
    use ESMF_Mod
    use ESMF_LogErrMod
    implicit none

    type(ESMF_Log) :: aLog1
    real :: real_num
    integer :: rc,rc2
    logical :: ret

    call ESMF_Initialize(rc=rc2)
    call ESMF_LogInitialize("aLog1.txt",rc)
    call ESMF_LogWrite("Log Write 2",ESMF_LOG_INFO,rc=rc)
    ret = ESMF_LogFoundError(ESMF_FAILURE,"Log Found Error 2",ESMF_LOG_INFO)
    call ESMF_Finalize(rc2)
end program
