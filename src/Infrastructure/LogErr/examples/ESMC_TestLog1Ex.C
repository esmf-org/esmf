
#include "ESMC.h"
#include "ESMC_LogErr.h"

int main() {
    ESMC_Log aLog,aLog2;
    
    aLog.ESMC_LogOpen("aLog1.txt");
    aLog.ESMC_LogWrite("Test message 2",ESMF_LOG_WARN,"mod1","meth1");
    aLog.ESMC_LogFoundError(ESMF_TRUE,"Test message 2",ESMF_LOG_WARN,"mod1","meth1");
}

