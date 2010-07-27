/*
 *
 */

#ifndef _LowLevelSocket_h_
#define _LowLevelSocket_h_

/*
#include <netinet/in.h>
*/

#define MAGIC  (0x8765)

/*
 * packet header
 */
struct pHeader
{
	int		magic;
	int		size;
};

class LowLevelSocket
{
public:

	LowLevelSocket();
	~LowLevelSocket();

	int  serverConnect(int  port);
	int  accept();

	int  clientConnect(const char*  host,
                      int          port);

	void  close();
	void  disconnect();

	int  read(int&   size,
             void*  data);
	int  write(int    size,
              void*  data);

	int  send(const char  msg[]);

	int  nonblock();

private:

	int  send(int    size,
             void*  data);
	int  recv(int    size,
             void*  data);


	int		theTSock;
	int		theSock;
	int		theCloseSock;
	bool		theNonBlock;
	int		thePhSize;
	pHeader	thePHead;
};


#endif
