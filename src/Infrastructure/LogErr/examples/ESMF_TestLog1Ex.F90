program test_log_2
#include "ESMF_LogErr.inc"
    use ESMF_Mod
    use ESMF_LogErrMod
    implicit none

    
    character(len=4) shep
    character :: ch
    type(ESMF_Log) :: aLog1
    real :: real_num
    integer :: int_num

    shep="xxxx"
    real_num=3.5
    int_num=8
    ch='s'

    call ESMF_LogSet(aLog1,flush=ESMF_FALSE, verbose=ESMF_TRUE)
    call ESMF_LogOpenFile(ESMF_Log_World,ESMF_SINGLE_LOG_FILE,"aLog.txt")
!jw no such    write(ESMF_LogGetUnit(ESMF_Log_World),*)"This is a test."
    call ESMF_LogInfo(aLog1,"Hi there %c %d %f ",ch,int_num,real_num)
    call ESMF_LogCloseFile(ESMF_Log_World)
end program
