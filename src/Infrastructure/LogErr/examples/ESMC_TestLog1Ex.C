
#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"
#define ESMC_METHOD "TestLog1Ex"

int main() {
int *rc;
    
    ESMC_LogSetFilename("aLog1.txt");
    ESMC_LogDefault.ESMC_LogWrite("LogWrite",ESMC_LOG_WARN);
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_TRUE,"Log Found Error",rc);
    ESMC_LogDefault.ESMC_LogAllocError();
}

