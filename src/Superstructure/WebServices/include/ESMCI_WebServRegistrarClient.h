/*
 *
 */

#ifndef _RegistrarClient_h_
#define _RegistrarClient_h_

#include <stdlib.h>

#include "ESMCI_WebServClientSocket.h"


#define REGISTRAR_PORT	45002


class RegistrarClient 
{
public:

	RegistrarClient(const char*  host,
                   int          port);
	~RegistrarClient();

	void setHost(const char*  host);
	void setPort(int  port);

	int  sendRequest(int    request,
                    int    length = 0,
                    void*  data = NULL);
	int  getResponse(int    request,
                    int&   length,
                    void*  data);

	int  registerComp(char*  name,
                     char*  desc,
                     char*  hostName,
                     char*  portNum,
                     void*  retValue);
	int  unregisterComp(char*  name,
                       char*  hostName,
                       char*  portNum,
                       void*  retValue);

	int  connect();
	void disconnect();

private:

	char*				theHost;
	int				thePort;
	char				theMsg[8192];
	ClientSocket	theSocket;
};


#endif
