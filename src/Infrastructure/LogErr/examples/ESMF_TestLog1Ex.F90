program test_log_2
#include "ESMF_LogErr.inc"
    use ESMF_Mod
    use ESMF_LogErrMod
    implicit none

    type(ESMF_Log) :: aLog1
    real :: real_num
    integer :: rc
    logical :: ret

    call ESMF_LogOpen(aLog1,"testlog.txt",rc)
    call ESMF_LogWrite(aLog1,"Log Write 2",ESMF_LOG_INFO,rc=rc)
    ret = ESMF_LogFoundError(aLog1,ESMF_FAILURE,"Log Found Error 2",ESMF_LOG_INFO)

end program
