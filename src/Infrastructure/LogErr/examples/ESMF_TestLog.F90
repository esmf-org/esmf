
#include "ESMF_LogErr.inc"

program test_log_2
    use ESMF_Mod
    
    character(len=4) shep
    character :: ch='s'
    type(ESMF_Log) :: aLog1,aLog2

    shep="xxxx"
    real_num=3.5
    int_num=8

    call ESMF_LogInit(aLog1)
    call ESMF_LogInit(aLog2,flushflag=ESMF_LOG_FALSE, verbose=ESMF_LOG_TRUE)
    call ESMF_LogOpenFile(aLog1,ESMF_SINGLE_LOG_FILE,"aLog.txt")
    write(LogWrite(aLog1),*)"This is a test."
    call ESMF_LogInfo(aLog2,"Hi there %c %d %f ",ch,int_num,real_num)
    call ESMF_LogInfo(aLog1,"Hi again %s",shep)
    call ESMF_LogCloseFile(aLog1)
end program
