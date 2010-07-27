/*
 *
 */

#ifndef _SocketUtils_h_
#define _SocketUtils_h_

#define TWAIT	(10)

enum Severity
{
	PRINT,
	WARN,
	ERROR,
	FATAL
};

void  notify(const char  msg[],
             Severity    severity,
             const char  proc[]);

int  send(int    fd,
          int    size,
          void*  data);

int  recv(int    fd,
          int    size,
          void*  data);

int  recv(int          fd,
          const char*  s);

#endif
