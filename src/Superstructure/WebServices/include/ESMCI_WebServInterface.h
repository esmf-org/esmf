#include "ESMCI_Comp.h"
//#include "ESMC_Comp.h"
//#include "ESMC_GridComp.h"

extern "C"
{
	void componentsvcloop_(ESMCI::GridComp*  comp,
                          ESMCI::State*     importState,
                          ESMCI::State*     exportState,
                          ESMCI::Clock*     clock,
                          int*        		  portNum);

	void gridserviceloop_(ESMCI::GridComp*  comp,
                         ESMCI::State*     importState,
                         ESMCI::State*     exportState,
                         ESMCI::Clock*     clock,
                         int*        		 portNum);

	void couplerserviceloop_(ESMCI::CplComp*  comp,
                            ESMCI::State*    importState,
                            ESMCI::State*    exportState,
                            ESMCI::Clock*    clock,
                            int*        		portNum);

	void  registercomponent_(char*  params, unsigned int param_len);
	void  unregistercomponent_(char*  params, unsigned int param_len);

/*
	void remoteinit_(ESMC_GridComp*     comp,
                    ESMC_State*        importState,
                    ESMC_State*        exportState,
                    ESMC_Clock*        clock,
                    int*               portNum,
                    char*              hostName,
                    int                hostNameLen);

	void remoterun_(ESMC_GridComp*     comp,
                   ESMC_State*        importState,
                   ESMC_State*        exportState,
                   ESMC_Clock*        clock,
                   int*               portNum,
                   char*              hostName,
                   int                hostNameLen);

	void remotefinal_(ESMC_GridComp*     comp,
                     ESMC_State*        importState,
                     ESMC_State*        exportState,
                     ESMC_Clock*        clock,
                     int*               portNum,
                     char*              hostName,
                     int                hostNameLen);
*/
};
