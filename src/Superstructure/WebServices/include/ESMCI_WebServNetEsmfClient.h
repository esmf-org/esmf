/*
 *
 */

#ifndef _NetEsmfClient_h_
#define _NetEsmfClient_h_

#include <stdlib.h>
#include <string>
#include <vector>

#include "ESMCI_WebServServerSocket.h"
#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_WebServNetEsmf.h"

using namespace std;


class NetEsmfClient 
{
public:

	NetEsmfClient(const char*  host,
                 int          port);
	~NetEsmfClient();

	void setHost(const char*  host);
	void setPort(int  port);

   int  newClient(const char*  clientName);
   int  init(int  clientId);
   int  init(int  clientId, const char*  filename);
   int  run(int  clientId);
   int  final(int  clientId);
   int  state(int  clientId);
   vector<string>  files(int  clientId);
	string  getData(int  clientId, string  varName, string  time, string  lat, string  lon, string&  dataValue);
   int  end(int  clientId);
   

	int  sendRequest(int    request,
                    int    length = 0,
                    void*  data = NULL);
	int  getResponse(int    request,
                    int&   length,
                    void*  data);

	int  connect();
	void disconnect();

	//void requestLoop();

private:

	int   getRequestId(const char  request[]);
	char* getRequestFromId(int  id);

	char*				theHost;
	int				thePort;
	char				theMsg[8192];
	ClientSocket	theSocket;
};


#endif
