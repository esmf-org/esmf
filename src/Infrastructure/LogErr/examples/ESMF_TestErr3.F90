#include "../include/ESMF_LogMacros.inc"
#include "../include/ESMF_LogConstants.inc"
#include "../include/ESMF_ErrConstants.inc"
program test_err
use ESMF_LogErr
type(ESMF_Log) :: anErr
call ESMF_LogInit(anErr,verbose=ESMF_LOG_FALSE,haltOnErr=ESMF_LOG_FALSE,   &
		  haltOnWarn=ESMF_LOG_TRUE)
call ESMF_LogErrMsg(anErr,ESMF_FATAL,"This is a test.")
call ESMF_LogNotVerbose(anErr)
call ESMF_LogErrMsg(anErr,ESMF_FATAL,"This is a test.")
call ESMF_LogWarn(anErr,ESMF_FATAL)
end program
