/* $Id$ */
!-------------------------------------------------------------------------

!ESMF_disable_SYSTEM_TEST      String used by test script to count system tests.
!=========================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "ESMCI_WebServCompSvrClient.h"

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"
#include "ESMCI_Test.h"


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

   int	rc;
	int	result = 0;
   char  name[80];
   char  failMsg[80];
	int	portNum = 27060;
	int	clientId = 1001;
	char	host[512] = { "" };



  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

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

	//***
	// Calling Initialize
	//***
	printf("Waiting for initialize...\n");
   rc = client.init();

   int   currentState = client.state();
   int   waitCount = 0;

   while (currentState == NET_ESMF_STAT_INITIALIZING)
   {
      sleep(1);
      ++waitCount;
      currentState = client.state();
   }
	printf("Done Waiting... waited for %d seconds\n", waitCount);

	//***
	// Calling Run
	//***
	printf("Waiting for run...\n");
   rc = client.run();

   currentState = client.state();
   waitCount = 0;

   while (currentState == NET_ESMF_STAT_RUNNING)
   {
      sleep(1);
      ++waitCount;
      currentState = client.state();
   }
	printf("Done Waiting... waited for %d seconds\n", waitCount);

	//***
	// Calling Finalize
	//***
	printf("Waiting for finalize...\n");
   rc = client.final();

   currentState = client.state();
   waitCount = 0;

   while (currentState == NET_ESMF_STAT_FINALIZING)
   {
      sleep(1);
      ++waitCount;
      currentState = client.state();
   }
	printf("Done Waiting... waited for %d seconds\n", waitCount);

	//***
	// Calling Get Files
	//***
	vector<string>	retFiles = client.files();

	//***
	// Calling End
	//***
   rc = client.end();

	//***
	// Kill the Server
	//***
   client.killServer();

	strcpy(name, "WebServicesClient");
	strcpy(failMsg, "Error communicating with Component Service.");
	ESMCI::Test(rc == 9, name, failMsg, &result, __FILE__, __LINE__, 0);

   printf("\n-----------------------------------------------------\n");
   fflush(stdout);

  	return 0;
}
