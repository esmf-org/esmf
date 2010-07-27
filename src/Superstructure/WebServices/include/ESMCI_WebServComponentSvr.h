/*
 *
 */

#ifndef _ComponentSvr_h_
#define _ComponentSvr_h_

#include "ESMCI_WebServServerSocket.h"
#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_Comp.h"
#include "ESMCI_Util.h"
#include "ESMCI_WebServNetEsmf.h"
#include "ESMCI_WebServClientInfo.h"
#include <map>

using namespace std;


class ComponentSvr 
{
public:

	ComponentSvr(int  port);
	~ComponentSvr();

	int  getPort()		{ return thePort; }
	void setPort(int  port);

	void requestLoop(ESMCI::GridComp*	comp,
                    ESMCI::State*      importState,
                    ESMCI::State*      exportState,
                    ESMCI::Clock*      clock,
                    int                phase,
                    ESMC_BlockingFlag  blockingFlag);

	void*  runInit(void);
	void*  runRun(void);
	void*  runFinal(void);


private:

	int  getNextRequest();
	int  serviceRequest(int  request);

	int   getRequestId(const char  request[]);
	char* getRequestFromId(int  id);

	void  setStatus(int  status);

	void  processInit();
	void  processRun();
	void  processFinal();
	void  processState();
	void  processFiles();
	void  processEnd();

	int				thePort;
	char				theMsg[8192];
	ServerSocket	theSocket;

	char*				theStatus;
	ClientSocket	theSyncSocket;

	ESMCI::GridComp*		theGridComp;
   ESMCI::State*			theImportState;
   ESMCI::State*			theExportState;
   ESMCI::Clock*			theClock;
	int						thePhase;
	ESMC_BlockingFlag		theBlockingFlag;

   int		theCurrentClientId;
	int		theCurrentStatus;

	pthread_mutex_t	theStatusMutex;
};


#endif
