program test_log_2
#include "ESMF_LogErr.inc"
    use ESMF_Mod
    use ESMF_LogErrMod
    implicit none

    type(ESMF_Log) :: aLog1
    real :: real_num
    integer :: rc

    call ESMF_LogInit(aLog1)
    print *, "ESMF_LogOpen"
    call ESMF_LogOpen(aLog1,"testlog.txt",rc)
    print *, "ESMF_LogWrite"
    call ESMF_LogWrite(aLog1,ESMF_LOG_INFO,"Log Write 1",rc)
    call ESMF_LogWrite(aLog1,ESMF_LOG_INFO,"Log Write 2",rc)
    print *, "ESMF_LogClose"
    call ESMF_LogClose(aLog1,rc)

end program
