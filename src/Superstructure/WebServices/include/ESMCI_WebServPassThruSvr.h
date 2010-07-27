/*
 *
 */

#ifndef _PassThruSvr_h_
#define _PassThruSvr_h_

#include "ESMCI_WebServServerSocket.h"
#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_WebServNetEsmf.h"
#include "ESMCI_WebServClientInfo.h"
#include "ESMCI_WebServCAMOutputFile.h"
#include <map>

using namespace std;


class PassThruSvr 
{
public:

	PassThruSvr(int     port, 
               string  camDir);
	~PassThruSvr();

	int  getPort()		{ return thePort; }
	void setPort(int  port);

	void requestLoop();


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
	void  processGetData();
	void  processEnd();
	void  processPing();

	int  getNextClientId();

	int				thePort;
	char				theMsg[8192];
	ServerSocket	theSocket;

	char*				theStatus;
	ClientSocket	theSyncSocket;

	map<int, ClientInfo*>	theClients;
	int							theNextClientId;

	string			theCAMDir;
	CAMOutputFile*	theOutputFile;
};


#endif
