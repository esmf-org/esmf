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

#include "ESMCI_WebServForkClient.h"

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

        int     portNum = 27060;
        int     clientId = 1001;
        char    host[512] = { "" };


  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

//      strcpy(runDir, argv[2]);
        gethostname(host, sizeof(host) - 1);

        ESMCI::ESMCI_WebServForkClient*         client =
                new ESMCI::ESMCI_WebServForkClient("localhost",
                                         "/home/ksaint",
                                         "testshell.sh");

   printf("\n");
   printf("ESMF_WebServForkClientUTest\n");
   printf("-----------------------------------------------------\n");
   printf("  date:  %s\n", getDateAndTime());
   printf("  host:  %s\n", host);
   printf("  port:  %d\n", portNum);
   printf(" client: %d\n", clientId);
   printf("-----------------------------------------------------\n");
   printf("\n");

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Submit Job");
   strcpy(failMsg, "Returned ESMF_FAILURE");

   string       jobId = client->submitJob(clientId, "localhost", portNum);
   printf(" Job ID: %s\n", jobId.c_str());

   ESMC_Test((!jobId.empty()), name, failMsg, &result, __FILE__, __LINE__, 0);
        printf("\n");
   //---------------------------------------------------------------------------

        printf("Sleeping for 10 seconds...\n");
        sleep(10);

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Cancel Job");
   strcpy(failMsg, "Returned -1");

   rc = client->cancelJob(jobId);

   ESMC_Test((rc == 0), name, failMsg, &result, __FILE__, __LINE__, 0);
        printf("\n");
   //---------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

   printf("\n-----------------------------------------------------\n");
   fflush(stdout);

        return 0;
}
