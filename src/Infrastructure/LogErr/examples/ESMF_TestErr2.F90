
#include "ESMF.h"
#include "ESMF_LogErr.inc"

program test_log
    use ESMF
    
    integer rc,int_num,nnn
    type(ESMF_Log) :: anErr
    real real_num
    character(len=4) shep
    character :: ch='s'
    shep="xxxx"
    
    int_num=8
    real_num=3.5
    nnn=ESMF_SINGLE_LOG_FILE
    call ESMF_LogInit(anErr)
    call ESMF_LogOpenFile(anErr,ESMF_SINGLE_LOG_FILE,"anErr.txt")
    write(6,*)'real ',real_num
    write(6,*)'shep ', shep
    call ESMF_LogInfo(anErr,"Hi there %c %d %f ",ch,int_num,real_num)
    call ESMF_LogInfo(anErr,"Hi again %s",shep)
    call ESMF_LogCloseFile(anErr)
end program
