
#include "ESMC.h"
#include "ESMC_LogErr.inc"

int main() {
    
    ESMC_LogSetFilename("aLog1.txt");
    ESMC_LogDefault.ESMC_LogWrite("LogWrite",ESMC_LOG_WARN);
    ESMC_LogDefault.ESMC_LogWrite("LogWrite",ESMC_LOG_WARN,"mod1");
    ESMC_LogDefault.ESMC_LogWrite("LogWrite",ESMC_LOG_WARN),"mod1","meth1";
    ESMC_LogDefault.ESMC_LogFoundError(ESMF_TRUE,"Log Found Error",ESMC_LOG_WARN);
    ESMC_LogDefault.ESMC_LogFoundError(ESMF_TRUE,"Log Found Error",ESMC_LOG_WARN,"mod1");
    ESMC_LogDefault.ESMC_LogFoundError(ESMF_TRUE,"Log Found Error",ESMC_LOG_WARN,"mod1","meth1");
}

