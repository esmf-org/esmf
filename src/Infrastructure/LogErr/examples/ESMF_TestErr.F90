
#include "ESMC.h"
#include "ESMF_LogErr.inc"

program test_log
    use ESMF_LogErr
    
    integer rc,int_num,nnn
    type(ESMF_Log) :: anErr,anErr2
    real*8 real_num
    character(len=10) shep
    character :: ch='s'
    shep="hi"
    int_num=8
    real_num=3.5

    nnn=ESMF_SINGLE_LOG_FILE
    call ESMF_LogInit(anErr)
    call ESMF_LogInit(anErr2)
    call ESMF_LogOpenFile(anErr,ESMF_SINGLE_LOG_FILE,"anErr.txt")
    call ESMF_LogNotHaltOnErr(anErr);
    call ESMF_LogErrMsg(anErr,ESMF_FATAL,"oy")
    call ESMF_LogInfo(anErr2,"Hi there %c",ch)
    call ESMF_LogWarn(anErr2,ESMF_FATAL)
    call ESMF_LogInfo(anErr,"Hi there %c",ch)
    call ESMF_LogCloseFile(anErr)
    call ESMF_LogErrMsg(anErr,ESMF_FATAL,"oy")
    call ESMF_LogCloseFile(anErr)
    call ESMF_LogHaltOnWarn(anErr2)
    call ESMF_LogWarnMsg(anErr2,ESMF_FATAL,"Hi there")
    write(6,*)'did I get here?'

end program
