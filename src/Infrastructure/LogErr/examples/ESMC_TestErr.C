#include "ESMC.h"
#include "ESMC_LogErr.h"

int main() {

    ESMC_Log aLog;
    int int_num=88;

    aLog.ESMC_LogInit();
    aLog.ESMC_LogInfo("Hi there agent %d",int_num);
}

