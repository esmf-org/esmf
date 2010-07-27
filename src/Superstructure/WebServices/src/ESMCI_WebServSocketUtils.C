/*
 *
 */

#include "ESMCI_WebServSocketUtils.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


/*
******************************************************************************
**
******************************************************************************
*/
void  notify(const char  msg[],
             Severity    severity = PRINT,
             const char  proc[] = NULL)
{
	switch (severity)
	{
	case PRINT:
		fprintf(stderr, "MSG");
		break;

	case WARN:
		fprintf(stderr, "WARNING");
		break;

	case ERROR:
		fprintf(stderr, "ERROR");
		break;

	case FATAL:
		fprintf(stderr, "FATAL");
		break;

	default:
		break;
	}

	if (proc != NULL)
	{
		fprintf(stderr, "[%s]", proc);
	}

	fprintf(stderr, ": %s\n", msg);

	if (severity == FATAL)
	{
		exit(0);
	}
}


/*
******************************************************************************
**
******************************************************************************
*/
int  send(int    fd,
          int    size,
          void*  data)
{
	if (size < 0)
	{
		return 0;
	}

	unsigned char*		ptr = (unsigned char*)data;
//printf("Sending: %s\n", ptr);

	int	totalBytesWritten = 0;
	int	t = 0;

	while ((totalBytesWritten < size)  &&  (t < TWAIT))
	{
		int	bytesWritten = write(fd, 
                                 ptr + totalBytesWritten, 
                                 size - totalBytesWritten);
//printf("::send - Bytes Written: %d\n", bytesWritten);

		if (bytesWritten > 0)
		{
			totalBytesWritten += bytesWritten;
		}
	}

	return totalBytesWritten;
}


/*
******************************************************************************
**
******************************************************************************
*/
int  recv(int    fd,
          int    size,
          void*  data)
{
	if (size < 0)
	{
		return 0;
	}

//printf("recv Size: %d\n", size);
	unsigned char*	ptr = (unsigned char*)data;

	int	totalBytesRead = 0;
	int	t = 0;

	while ((totalBytesRead < size)  &&  (t < TWAIT))
	{
		int	bytesRead = read(fd, ptr + totalBytesRead, size - totalBytesRead);

		if (bytesRead > 0)
		{
			totalBytesRead += bytesRead;
		}
	}

	return totalBytesRead;
}


/*
******************************************************************************
**
******************************************************************************
*/
int  recv(int          fd,
          const char*  s)
{
	char*		cp = const_cast<char*>(s);
	int		totalBytesRead = 0;
	int		bytesRead = 0;

	do
	{
		bytesRead = read(fd, cp, 1);
//printf("Bytes Read: %d\n", bytesRead);

		if (bytesRead > 0)
		{
			totalBytesRead += bytesRead;
			if (*cp == 0)
			{
				break;
			}

			cp += bytesRead;
		}
	} while (totalBytesRead > 0);

	return totalBytesRead;
}
