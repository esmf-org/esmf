/*
 *
 */

#ifndef _ClientSocket_h_
#define _ClientSocket_h_

#include "ESMCI_WebServLowLevelSocket.h"

class ClientSocket : public LowLevelSocket
{
public:

	ClientSocket();
	~ClientSocket();

	int  connect(const char*  host,
                int          port);
};


#endif
