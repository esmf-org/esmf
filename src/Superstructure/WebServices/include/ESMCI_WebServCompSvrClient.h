/*
 *
 */

#ifndef _CompSvrClient_h_
#define _CompSvrClient_h_

#include <stdlib.h>
#include <string>
#include <vector>

#include "ESMCI_WebServServerSocket.h"
#include "ESMCI_WebServClientSocket.h"
#include "ESMCI_WebServNetEsmf.h"

using namespace std;


class CompSvrClient 
{
public:

	CompSvrClient(const char*  host,
                 int          port,
                 int          clientId);
	~CompSvrClient();

	void setHost(const char*  host);
	void setPort(int  port);
	void setClientId(int  clientId);

   int  init();
   int  init(const char*  filename);
   int  run();
   int  final();
   int  state();
   vector<string>  files();
   int  end();
   

	int  sendRequest(int    request,
                    int    length = 0,
                    void*  data = NULL);
	int  getResponse(int    request,
                    int&   length,
                    void*  data);

	int  connect();
	void disconnect();


private:

	int   getRequestId(const char  request[]);
	char* getRequestFromId(int  id);

	char*				theHost;
	int				thePort;
	char				theMsg[8192];
	int				theClientId;
	ClientSocket	theSocket;
};


#endif
