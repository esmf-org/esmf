#include "ESMC.h"

#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"
#include <iostream.h>
#define ESMC_METHOD "ESMC_LogErrEx"

//!EXAMPLE String used by test script to count examples.
int main() {
    int rc;
    int rc1;

    //Set finalrc to success
    int finalrc=ESMF_SUCCESS;

    ESMC_Initialize();
    
    ESMC_LogSetFilename("log1.txt");
    ESMC_LogDefault.ESMC_LogWrite("LogWrite",ESMC_LOG_WARN);
    rc = ESMF_RC_OBJ_BAD;
    rc1 = ESMC_LogDefault.ESMC_LogFoundError(ESMF_TRUE,&rc);

   if (!rc1) {
       finalrc = ESMF_FAILURE;
   }

    rc1 = ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_TRUE,"Log Msg Found Error",&rc);
   if (!rc1) {
       finalrc = ESMF_FAILURE;
   }
    
    rc1 = ESMC_LogDefault.ESMC_LogAllocError(&rc);
   if (!rc1) {
       finalrc = ESMF_FAILURE;
   }
    rc1 = ESMC_LogDefault.ESMC_LogMsgAllocError("Log Msg Alloc Error",&rc);

   if (!rc1) {
       finalrc = ESMF_FAILURE;
   }

    ESMC_Finalize();
                                           
   if (finalrc == ESMF_SUCCESS) {
        cout << "PASS: ESMC_LogErrEx.C" << endl;
        return(ESMF_SUCCESS);
   }
   else {
        cout << "FAIL: ESMC_LogErrEx.C" << endl;
        return(ESMF_FAILURE);
   }

}

