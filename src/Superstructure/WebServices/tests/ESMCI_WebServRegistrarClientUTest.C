/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if !defined (ESMF_OS_MinGW)
#include <unistd.h>
#endif

#include "ESMCI_WebServRegistrarClient.h"
#include "ESMCI_WebServCompSvrInfo.h"

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
        int     portNum = 45002;
        int     clientId = 1001;
        char    host[512] = { "localhost" };


  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Instantiate Registrar Client");
   strcpy(failMsg, "Instantiation Failed");

        ESMCI::ESMCI_WebServRegistrarClient             client(host, portNum);

   ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
   //---------------------------------------------------------------------------

   printf("\n");
   printf("ESMF_WebServRegistrarClientUTest\n");
   printf("-----------------------------------------------------\n");
   printf("  date:  %s\n", getDateAndTime());
   printf("  host:  %s\n", host);
   printf("  port:  %d\n", portNum);
   printf(" client: %d\n", clientId);
   printf("-----------------------------------------------------\n");
   printf("\n");

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Register");
   strcpy(failMsg, "Returned ESMF_FAILURE");

   rc = client.registerComp("110", "bluefire", "27001");

   ESMC_Test((rc == NET_ESMF_STAT_IDLE),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Component Submitted");
   strcpy(failMsg, "Returned ESMF_FAILURE");

   rc = client.compSubmitted("110", "job_bluefire_1001");

   ESMC_Test((rc == NET_ESMF_STAT_SUBMITTED),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------


   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Component Started");
   strcpy(failMsg, "Returned ESMF_FAILURE");

   rc = client.compStarted("110",
                           "CAM",
                           "Community Atmospheric Model",
                           "ben0001");

   ESMC_Test((rc == NET_ESMF_STAT_READY),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------

   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Component Submitted Again");
   strcpy(failMsg, "Returned ESMF_FAILURE");

   rc = client.compSubmitted("110", "job_bluefire_1001");

   ESMC_Test((rc == NET_ESMF_STAT_READY),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------


   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Get Component");
   strcpy(failMsg, "Returned ESMF_FAILURE");
        ESMCI::ESMCI_WebServCompSvrInfo         goodClient;

   rc = client.getComponent("110", &goodClient);
        goodClient.print();

   ESMC_Test((rc != ESMF_FAILURE),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------


   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Get Component with unknown client ID");
   strcpy(failMsg, "Did not return ESMF_FAILURE");
        ESMCI::ESMCI_WebServCompSvrInfo         dummyClient;

   rc = client.getComponent("666", &dummyClient);

   ESMC_Test((rc == ESMF_FAILURE),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------


   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Get Status");
   strcpy(failMsg, "Returned ESMF_FAILURE");

   rc = client.getStatus("110");

   ESMC_Test((rc != ESMF_FAILURE),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------


   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Set Status");
   strcpy(failMsg, "Returned ESMF_FAILURE");

   rc = client.setStatus("110", "DONE");

   ESMC_Test((rc == NET_ESMF_STAT_DONE),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------


   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Set Status with invalid state");
   strcpy(failMsg, "Did not return ESMF_FAILURE");

   rc = client.setStatus("110", "EXITED");

   ESMC_Test((rc == ESMF_FAILURE),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------


   //---------------------------------------------------------------------------
   //NEX_disable_UTest
   strcpy(name, "Call Unregister");
   strcpy(failMsg, "Returned ESMF_FAILURE");

   rc = client.unregisterComp("110");

   ESMC_Test((rc != ESMF_FAILURE),
                name, failMsg, &result, __FILE__, __LINE__, 0);
   printf("\n");
   //---------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

   printf("\n-----------------------------------------------------\n");
   fflush(stdout);

        return 0;
}
