#include "../include/ESMF_LogMacros.inc"
#include "../include/ESMC_LogErr.h"
#include "../include/ESMF_LogConstants.inc"
#include "../include/ESMF_ErrConstants.inc"

int main() {
ESMC_Log aLog,aLog2;
int int_num=88;
float real_num=3.5;
char shep[10]="xxxx";
char ch ='s';
aLog.ESMC_LogInit();
aLog2.ESMC_LogInit();
aLog2.ESMC_LogOpenFile(ESMF_SINGLE_LOG_FILE,"aLog2.txt");
aLog2.ESMC_LogInfo("Hi there %d %c %f %s",int_num,ch,real_num,shep);
aLog.ESMC_LogInfo("Hi there %d %c %f %s",int_num,ch,real_num,shep);
aLog2.ESMC_LogCloseFile();
}

