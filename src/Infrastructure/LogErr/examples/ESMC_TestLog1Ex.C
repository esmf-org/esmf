
#include "ESMC.h"

int main() {
    ESMC_Log aLog;
    
    ESMC_LogInitialize("aLog1.txt");
    aLog.ESMC_LogWrite("LogWrite",ESMC_LOG_WARN,"mod1","meth1");
    aLog.ESMC_LogFoundError(ESMF_TRUE,"Log Found Error",ESMC_LOG_WARN,"mod1","meth1");
}

