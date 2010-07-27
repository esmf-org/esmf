/*
 *
 */

#ifndef _NetEsmfServer_h_
#define _NetEsmfServer_h_

#include "ESMCI_WebServServerSocket.h"
#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_Comp.h"
#include "ESMCI_Util.h"
#include "ESMCI_WebServNetEsmf.h"
#include "ESMCI_WebServClientInfo.h"
#include <map>

using namespace std;


class NetEsmfServer 
{
public:

   typedef enum ESMC_CompType
	{
		ESMC_COMPTYPE_GRID = 0,
		ESMC_COMPTYPE_COUPLER = 1
	} ESMC_CompType;

	NetEsmfServer(int  port);
	~NetEsmfServer();

	int  getPort()		{ return thePort; }
	void setPort(int  port);

	void requestLoop(ESMCI::GridComp*	comp,
                    ESMCI::State*      importState,
                    ESMCI::State*      exportState,
                    ESMCI::Clock*      clock,
                    int                phase,
                    ESMC_BlockingFlag  blockingFlag);

	void requestLoop(ESMCI::CplComp*		comp,
                    ESMCI::State*      importState,
                    ESMCI::State*      exportState,
                    ESMCI::Clock*      clock,
                    int                phase,
                    ESMC_BlockingFlag  blockingFlag);

private:

	int  getNextRequest();
	int  serviceRequest(int  request);

	int   getRequestId(const char  request[]);
	char* getRequestFromId(int  id);

	void  processNew();
	void  processInit();
	void  processRun();
	void  processFinal();
	void  processState();
	void  processFiles();
	void  processEnd();
	void  processPing();

	int  getNextClientId();
	void  copyFile(const char*  srcFilename, 
                  const char*  destFilename);

	int				thePort;
	char				theMsg[8192];
	ServerSocket	theSocket;

	char*				theStatus;
	ClientSocket	theSyncSocket;

	ESMC_CompType			theCompType;

	ESMCI::GridComp*		theGridComp;
	ESMCI::CplComp*		theCouplerComp;
   ESMCI::State*			theImportState;
   ESMCI::State*			theExportState;
   ESMCI::Clock*			theClock;
	int						thePhase;
	ESMC_BlockingFlag		theBlockingFlag;

	map<int, ClientInfo*>	theClients;
	int							theNextClientId;
};


#endif
