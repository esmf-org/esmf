
#include "ESMF_LogErr.inc"

program test_log_options
    use ESMF_Mod
    
    integer rc
    type(ESMF_Log) :: anErr1,anErr2

    call ESMF_LogInit(anErr1,haltOnErr=ESMF_LOG_FALSE,flushFlag=ESMF_LOG_TRUE)
    call ESMF_LogInit(anErr2,haltOnErr=ESMF_LOG_FALSE,haltOnWarn=ESMF_LOG_FALSE, &
                      verbose=ESMF_LOG_FALSE)
    call ESMF_LogOpenFile(anErr1,ESMF_SINGLE_LOG_FILE,"anErr.txt")
    call foo(rc)
    if (rc /= ESMF_SUCCESS) then
       call ESMF_LogErr(anErr1,rc)
       call ESMF_LogErrMsg(anErr1,rc,"This is not silly.")
    end if

    call foo2(rc)
    if (rc /= ESMF_SUCCESS)  then
       call ESMF_LogWarnMsg(anErr2,rc,"This is silly")
       call ESMF_LogWarn(anErr2,rc)
    end if

    call ESMF_LogVerbose(anErr2)
    call foo2(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_LogWarnMsg(anErr2,rc,"This is not silly")

    call ESMF_LogNotVerbose(anErr1)
    call foo(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_LogErr(anErr1,rc)

    call ESMF_HaltOnErr(anErr1)
    call ESMF_NotHaltOnWarn(anErr1)
    call foo2(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_LogWarnMsg(anErr2,rc,"This is silly")

    call ESMF_HaltOnWarn(anErr2)
    call ESMF_NotHaltOnErr(anErr2)
    call foo(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_LogErr(anErr2,rc)

    call ESMF_LogNotFlush(anErr1)
    call foo2(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_LogWarn(anErr1,rc)

    call ESMF_LogFlush(anErr2)
    call foo2(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_LogWarn(anErr1,rc)


    call ESMF_LogCloseFile(anErr1)

end program
