/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if !defined (ESMF_OS_MinGW)
#include <unistd.h>
#else
#include <Windows.h>
#define sleep(secs) Sleep(secs*1000)
#include <Winsock.h>
#endif

#include "ESMCI_WebServProcCtrlClient.h"

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
   static char          datestr[1024];
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

   int  rc;
        int     result = 0;
   char  name[80];
   char  failMsg[80];
        int     portNum = 27070;
        int     clientId = 1001;
        char    host[512] = { "" };


  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

//      strcpy(runDir, argv[2]);
        gethostname(host, sizeof(host) - 1);

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Instantiate Process Control Client");
   strcpy(failMsg, "Instantiation Failed");

        ESMCI::ESMCI_WebServProcCtrlClient              client(host,
                                                 portNum,
                                                 "ksaint",
                                                 "mickey");

   ESMC_Test((client.getClientId() > 0),
             name, failMsg, &result, __FILE__, __LINE__, 0);
   //---------------------------------------------------------------------------

   printf("\n");
   printf("ESMF_WebServProcCtrlClientUTest\n");
   printf("-----------------------------------------------------\n");
   printf("  date:  %s\n", getDateAndTime());
   printf("  host:  %s\n", host);
   printf("  port:  %d\n", portNum);
   printf(" client: %d\n", clientId);
   printf("-----------------------------------------------------\n");
   printf("\n");

        //***
        // Sleep for a few just to give the server a chance to startup
        //***
        printf("\n... Don't panic!  We're sleeping for 5 seconds...\n");
        sleep(5);

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Component Service State");
   strcpy(failMsg, "Returned ESMF_FAILURE");

   int   currentState = client.state();
printf("Current State: %d\n", currentState);
printf("Current State: %s\n", client.getStateStr(currentState));

   ESMC_Test((currentState != ESMF_FAILURE),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   //---------------------------------------------------------------------------


   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Component Initialize");
   strcpy(failMsg, "Returned ESMF_FAILURE");

        printf("Executing initialize...\n");
   rc = client.init();
printf("Current State: %d\n", rc);
printf("Current State: %s\n", client.getStateStr(rc));

   ESMC_Test((rc!=ESMF_FAILURE), name, failMsg, &result, __FILE__, __LINE__, 0);
   //---------------------------------------------------------------------------

   currentState = client.state();
   int   waitCount = 0;

        printf("Waiting for initialize...\n");
   while (currentState == NET_ESMF_STAT_INITIALIZING)
   {
      sleep(1);
      ++waitCount;
      currentState = client.state();
   }
        printf("Done Waiting... waited for %d seconds\n", waitCount);

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Component Run");
   strcpy(failMsg, "Returned ESMF_FAILURE");

        printf("Executing run...\n");
   rc = client.run();
printf("Current State: %d\n", rc);
printf("Current State: %s\n", client.getStateStr(rc));

   ESMC_Test((rc!=ESMF_FAILURE), name, failMsg, &result, __FILE__, __LINE__, 0);
   //---------------------------------------------------------------------------

   currentState = client.state();
   waitCount = 0;

        printf("Waiting for run...\n");
   while (currentState == NET_ESMF_STAT_RUNNING)
   {
      sleep(1);
      ++waitCount;
      currentState = client.state();
   }
        printf("Done Waiting... waited for %d seconds\n", waitCount);

/*
   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Component Finalize");
   strcpy(failMsg, "Returned ESMF_FAILURE");
   rc = client.final();
   ESMC_Test((rc!=ESMF_FAILURE), name, failMsg, &result, __FILE__, __LINE__, 0);
   //---------------------------------------------------------------------------

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
*/

/*
   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Component Files");
   strcpy(failMsg, "No files returned");
        vector<string>  retFiles = client.files();
   ESMC_Test((retFiles.size()==0),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   //---------------------------------------------------------------------------
*/

/*
   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Component End");
   strcpy(failMsg, "Returned ESMF_FAILURE");
   rc = client.end();
   ESMC_Test((rc!=ESMF_FAILURE), name, failMsg, &result, __FILE__, __LINE__, 0);
   //---------------------------------------------------------------------------
*/

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

   client.killServer();


   printf("\n-----------------------------------------------------\n");
   fflush(stdout);

        return 0;
}
