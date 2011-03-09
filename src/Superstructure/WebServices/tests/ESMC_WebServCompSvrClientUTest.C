/* $Id: ESMC_WebServCompSvrClientUTest.C,v 1.1 2011/03/09 14:16:39 ksaint Exp $ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "ESMCI_WebServCompSvrClient.h"


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
  	printf("hello from ESMCI_WebServCompSvrClientUTest\n");

/*
	if (argc < 3)
	{
		printf("Usage: ESMF_WebServCompSvrClientUTest <portNum> <runDir>");
		return 1;
	}

	int	portNum = atoi(argv[1]);
	char	runDir[512];
	char	host[512] = { "" };
*/
	int	portNum = 27060;
	int	clientId = 1001;
	char	host[512] = { "" };

//	strcpy(runDir, argv[2]);
	gethostname(host, sizeof(host) - 1);

	ESMCI::ESMCI_WebServCompSvrClient		client(host, portNum, clientId);

   printf("\n");
   printf("ESMF_WebServCompSvrClientUTest\n");
   printf("-----------------------------------------------------\n");
   printf("  date:  %s\n", getDateAndTime());
   printf("  host:  %s\n", host);
   printf("  port:  %d\n", portNum);
   printf(" client: %d\n", clientId);
   printf("-----------------------------------------------------\n");
   printf("\n");

   printf("Sending request...\n");
   fflush(stdout);

   client.init();
   int   currentState = client.state();
   int   waitCount = 0;

printf("Waiting for initialize...\n");
   while (currentState == NET_ESMF_STAT_INITIALIZING)
   {
      sleep(1);
      ++waitCount;
      currentState = client.state();
   }
printf("Done Waiting... waited for %d seconds\n", waitCount);

   client.run();

printf("Waiting for run...\n");
   currentState = client.state();
   waitCount = 0;

   while (currentState == NET_ESMF_STAT_RUNNING)
   {
      sleep(1);
      ++waitCount;
      currentState = client.state();
   }
printf("Done Waiting... waited for %d seconds\n", waitCount);

   client.final();

printf("Waiting for finalize...\n");
   currentState = client.state();
   waitCount = 0;

   while (currentState == NET_ESMF_STAT_FINALIZING)
   {
      sleep(1);
      ++waitCount;
      currentState = client.state();
   }
printf("Done Waiting... waited for %d seconds\n", waitCount);

   client.files();
   client.end();

   client.killServer();

   printf("\n-----------------------------------------------------\n");
   fflush(stdout);

  	return 0;
}
