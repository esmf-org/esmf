
#include "ESMC.h"
#include "ESMF_LogMacros.inc"

int main() {
    
    ESMC_LogInitialize("aLog1.txt");
    Glog.ESMC_LogWrite("LogWrite",ESMC_LOG_WARN,"mod1","meth1");
    Glog.ESMC_LogFoundError(ESMF_TRUE,"Log Found Error",ESMC_LOG_WARN,"mod1","meth1");
}

