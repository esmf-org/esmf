
#include "ESMC.h"
#include "ESMF_LogMacros.inc"

int main() {
    
    ESMC_LogSetFilename("aLog1.txt");
    ESMF_Log.ESMC_LogWrite("LogWrite",ESMC_LOG_WARN);
    ESMF_Log.ESMC_LogWrite("LogWrite",ESMC_LOG_WARN,"mod1");
    ESMF_Log.ESMC_LogWrite("LogWrite",ESMC_LOG_WARN),"mod1","meth1";
    ESMF_Log.ESMC_LogFoundError(ESMF_TRUE,"Log Found Error",ESMC_LOG_WARN);
    ESMF_Log.ESMC_LogFoundError(ESMF_TRUE,"Log Found Error",ESMC_LOG_WARN,"mod1");
    ESMF_Log.ESMC_LogFoundError(ESMF_TRUE,"Log Found Error",ESMC_LOG_WARN,"mod1","meth1");
}

