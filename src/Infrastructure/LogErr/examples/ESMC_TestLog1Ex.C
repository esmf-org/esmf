
#include "ESMC.h"
#include "ESMF_LogMacros.inc"
#define ESMC_METHOD "TestLog1Ex"

int main() {
    
    ESMC_LogSetFilename("aLog1.txt");
    ESMC_LogDefault.ESMC_LogWrite("LogWrite",ESMC_LOG_WARN);
    ESMC_LogDefault.ESMC_LogFoundError(ESMF_TRUE,"Log Found Error",ESMC_LOG_WARN);
}

