program test_log_2
#include "ESMF_LogMacros.inc"
#define ESMF_METHOD "test_log_2"
    use ESMF_Mod
    use ESMF_LogErrMod
    implicit none

    type(ESMF_Log) :: aLog1
    real :: real_num
    integer :: rc
    logical :: ret

    call ESMF_Initialize(rc=rc)
    call ESMF_LogInitialize("aLog1.txt",rc=rc)
    call ESMF_LogWrite("Log Write 2",ESMF_LOG_INFO)
    ret = ESMF_LogFoundError(ESMF_FAILURE,"Log Found Error 2",ESMF_LOG_INFO)
end program
