
#include "ESMC.h"

int main() {
    ESMC_Log aLog;
    
    aLog.ESMC_LogOpen("aLog1.txt");
    aLog.ESMC_LogWrite("LogWrite",ESMF_LOG_WARN,"mod1","meth1");
    aLog.ESMC_LogFoundError(ESMF_TRUE,"Log Found Error",ESMF_LOG_WARN,"mod1","meth1");
}

