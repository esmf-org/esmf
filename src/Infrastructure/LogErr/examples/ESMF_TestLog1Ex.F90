program test_log_2
#include "ESMF_LogErr.inc"
    use ESMF_Mod
    use ESMF_LogErrMod
    implicit none

    type(ESMF_Log) :: aLog1
    real :: real_num
    integer :: rc

   
    call ESMF_LogOpen(aLog1, ESMF_SINGLE_LOG_FILE, "testlog.txt",rc)
    call ESMF_LogWrite(aLog1,ESMF_SINGLE_LOG_FILE,"Log Write",rc)
    call ESMF_LogClose(aLog1,rc)

end program
