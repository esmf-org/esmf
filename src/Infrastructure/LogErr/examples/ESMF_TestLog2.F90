
#include "ESMF_LogErr.inc"

program test_log_options
    use ESMF_Mod
    
    integer rc
    type(ESMF_Log) :: anErr1,anErr2
    type(ESMF_Logical) :: myLocalFlush,myLocalVerbose
    character(len=32) :: verySilly, notSilly

    notSilly='This is not silly'
    verySilly='This is very silly'
    call ESMF_LogSet(anErr1,haltOnErr=ESMF_TF_FALSE,flush=ESMF_TF_TRUE)
    call ESMF_LogSet(anErr2,haltOnErr=ESMF_TF_FALSE,haltOnWarn=ESMF_TF_FALSE, &
                      verbose=ESMF_TF_FALSE)
    call ESMF_LogOpenFile(anErr1,ESMF_SINGLE_LOG_FILE,"anErr.txt")
    call foo(rc)
    if (rc /= ESMF_SUCCESS) then
       call ESMF_LogErr(anErr1,rc)
       call ESMF_LogErrMsg(anErr1,rc,notSilly)
    end if

    call foo2(rc)
    if (rc /= ESMF_SUCCESS)  then
       call ESMF_LogWarnMsg(anErr2,rc,verySilly)
       call ESMF_LogWarn(anErr2,rc)
    end if

    call ESMF_LogSet(anErr2,verbose=ESMF_TF_TRUE)
    call ESMF_LogGet(anErr2,verbose=myLocalVerbose)
    call foo2(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_LogWarnMsg(anErr2,rc,notSilly)

    call ESMF_LogSet(anErr1,verbose=ESMF_TF_FALSE)
    call foo(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_LogErr(anErr1,rc)

    call ESMF_LogSet(anErr1,haltOnErr=ESMF_TF_TRUE)
    call ESMF_LogSet(anErr1,haltOnWarn=ESMF_TF_FALSE)
    call foo2(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_LogWarnMsg(anErr2,rc,verySilly)

    call ESMF_LogSet(anErr2,haltOnWarn=ESMF_TF_TRUE)
    call ESMF_LogSet(anErr2,haltOnErr=ESMF_TF_TRUE)
    call foo(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_LogErr(anErr2,rc)

    call ESMF_LogSet(anErr1,flush=ESMF_TF_FALSE)
    call ESMF_LogGet(anErr1,flush=myLocalFlush)
!jw ?    write(*,*) "My local value of flush is", myLocalFlush
    call foo2(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_LogWarn(anErr1,rc)

    call ESMF_LogSet(anErr2,flush=ESMF_TF_TRUE)
    call foo2(rc)

    if (rc /= ESMF_SUCCESS) call ESMF_LogWarn(anErr1,rc)


    call ESMF_LogCloseFile(anErr1)

end program
