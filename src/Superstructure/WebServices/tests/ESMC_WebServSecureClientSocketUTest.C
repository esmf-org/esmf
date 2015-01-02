/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "ESMCI_WebServSecureClientSocket.h"

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"


static char*   monthStr[] =
{
   (char*)"January",
   (char*)"Febuary",
   (char*)"March",
   (char*)"April",
   (char*)"May",
   (char*)"June",
   (char*)"July",
   (char*)"August",
   (char*)"September",
   (char*)"October",
   (char*)"November",
   (char*)"December"
};


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "getDateAndTime"
//BOPI
// !ROUTINE:  getDateAndTime()
//
// !INTERFACE:
const char*  getDateAndTime(
//
// !RETURN VALUE:
//   const char*  string containing the current date and time
//
// !ARGUMENTS:
//
  )
//
// !DESCRIPTION:
//    Returns a string with the formatted, current date and time
//
//EOPI
//-----------------------------------------------------------------------------
{
   static char 	datestr[1024];
   time_t         ttime;
   struct tm*     tm;

   ttime = time(NULL);
   tm = localtime(&ttime);

   char* a = (char*)"am";
   if (tm->tm_hour >= 12)
   {
      a = (char*)"pm";
      if (tm->tm_hour > 12)
      {
         tm->tm_hour -= 12;
      }
   }

   sprintf(datestr, "%s %d, %d @ %d:%02d:%02d %s",
           monthStr[tm->tm_mon], tm->tm_mday, 1900+tm->tm_year,
           tm->tm_hour, tm->tm_min, tm->tm_sec, a);

   return datestr;
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
int main(int    argc, 
         char*  argv[])
{
  	printf("hello from ESMCI_WebServSecureClientSocketUTest\n");

   int	rc;
	int	result = 0;
   char  name[80];
   char  failMsg[80];
	int	portNum = 27060;
	char	host[512] = { "" };



  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

	gethostname(host, sizeof(host) - 1);

	ESMCI::ESMCI_WebServSecureClientSocket*	
		client = new ESMCI::ESMCI_WebServSecureClientSocket();

   printf("\n");
   printf("ESMF_WebServSecureClientSocketUTest\n");
   printf("-----------------------------------------------------\n");
   printf("  date:  %s\n", getDateAndTime());
   printf("  host:  %s\n", host);
   printf("  port:  %d\n", portNum);
   printf("-----------------------------------------------------\n");
   printf("\n");

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call connect");
   strcpy(failMsg, "Returned invalid socket file descriptor");

   int   socketFd = client->connect(host, portNum);
   printf("Socket FD: %d\n", socketFd);

   ESMC_Test((socketFd >= 0), name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call send string");
   strcpy(failMsg, "Returned invalid number of bytes sent");

   int   bytesSent = client->send("This is a sample string.");
   printf("Bytes Sent: %d\n", bytesSent);

   ESMC_Test((bytesSent > 0), name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------

   int   numBytes = 0;
   char  retValue[257];

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call read return string");
   strcpy(failMsg, "Returned invalid data size");

   int   bytesRead = client->read(numBytes, retValue);
   printf(" Num Bytes: %d\n", numBytes);
   printf("Bytes Read: %d\n", bytesRead);
   printf("Input: %s\n", retValue);

   ESMC_Test((bytesRead > 0), name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

   printf("\n-----------------------------------------------------\n");
   fflush(stdout);

  	return 0;
}
