
#include "ESMF_LogErr.inc"

program test_err
    
    use ESMF_Mod

    type(ESMF_Log) :: anErr

    call ESMF_LogInit(anErr,verbose=ESMF_LOG_FALSE,haltOnErr=ESMF_LOG_FALSE, &
    		  haltOnWarn=ESMF_LOG_TRUE)
    call ESMF_LogErrMsg(anErr,ESMF_FATAL,"This is a test.")
    call ESMF_LogVerbose(anErr)
    call ESMF_LogErrMsg(anErr,ESMF_FATAL,"This is a test.")
    call ESMF_LogWarn(anErr,ESMF_FATAL)
    
end program
