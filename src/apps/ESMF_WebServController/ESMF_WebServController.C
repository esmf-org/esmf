/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "ESMCI_WebServPassThruSvr.h"


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
  	printf("hello from ESMF_WebServController\n");

	if (argc < 4)
	{
		printf("Usage: ESMF_WebServController <portNum> <runDir> <svrPort>\n");
		return 1;
	}

	int	portNum = atoi(argv[1]);
	char	runDir[512];
	int	svrPort = atoi(argv[3]);
	char	host[512] = { "" };

	strcpy(runDir, argv[2]);
	gethostname(host, sizeof(host) - 1);

	ESMCI::ESMCI_WebServPassThruSvr		server(portNum, runDir, svrPort);

   printf("\n");
   printf("ESMF_WebServController\n");
   printf("-----------------------------------------------------\n");
   printf(" date:  %s\n", getDateAndTime());
   printf(" host:  %s\n", host);
   printf(" port:  %d\n", server.getPort());
   printf("-----------------------------------------------------\n");
   printf("\n");

   printf("Listening for requests...\n");
   fflush(stdout);

   server.requestLoop();

   printf("\n-----------------------------------------------------\n");
   fflush(stdout);

  	return 0;
}
