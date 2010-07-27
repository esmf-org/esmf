/*
 *
 */

#ifndef _ServerSocket_h_
#define _ServerSocket_h_

#include "ESMCI_WebServLowLevelSocket.h"

class ServerSocket : public LowLevelSocket
{
public:

	ServerSocket();
	~ServerSocket();

	int  connect(int  port);
};


#endif
