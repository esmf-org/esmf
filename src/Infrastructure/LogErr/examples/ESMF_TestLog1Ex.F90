program test_log_2
#include "ESMF_LogErr.inc"
    use ESMF_Mod
    use ESMF_LogErrMod
    implicit none

    type(ESMF_Log) :: aLog1
    real :: real_num
    integer :: rc
    logical :: ret

    call ESMF_LogInit(aLog1)
    print *, "ESMF_LogOpen"
    call ESMF_LogOpen(aLog1,"testlog.txt",rc)
    print *, "ESMF_LogWrite"
    call ESMF_LogWrite(aLog1,"Log Write 1",ESMF_LOG_INFO,rc=rc)
    call ESMF_LogWrite(aLog1,"Log Write 2",ESMF_LOG_INFO,rc=rc)
    print *, "ESMF_LogFoundError"
    ret = ESMF_LogFoundError(aLog1,ESMF_SUCCESS,"Log Found Error 1",ESMF_LOG_INFO)
    ret = ESMF_LogFoundError(aLog1,ESMF_FAILURE,"Log Found Error 2",ESMF_LOG_ERR)
    print *, "ESMF_LogClose"
    call ESMF_LogClose(aLog1,rc)

end program
