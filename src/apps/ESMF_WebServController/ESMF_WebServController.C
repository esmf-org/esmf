/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
//#include "ESMCI_WebServPassThruSvr.h"
#include "ESMCI_WebServProcCtrl.h"


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
      printf("Usage: ESMF_WebServController <procCtrlPort> <registrarHost> <registrarPort>\n");
		return 1;
	}

   int      procCtrlPort = atoi(argv[1]);
   char     registrarHost[256];
   strcpy(registrarHost, argv[2]);
   int      registrarPort = atoi(argv[3]);

   string   compSvrHost = "localhost";
   int      compSvrStartPort = 27060;
   int      portPoolSize = 5;
   string   compSvrScriptDir = "/nics/c/home/ksaint/Scripts";
   string   compSvrScriptName = "runjob.sh";


   ESMCI::ESMCI_WebServProcCtrl
      server(procCtrlPort,
             registrarHost, registrarPort,
             compSvrHost,
             compSvrStartPort, portPoolSize,
             compSvrScriptDir, compSvrScriptName,
             ESMCI::ESMCI_WebServProcCtrl::ESMC_JOBMGRTYPE_FORK);

   printf("\n");
   printf("ESMF_WebServController\n");
   printf("-----------------------------------------------------\n");
   printf(" date:  %s\n", getDateAndTime());
   printf(" Registrar:\n");
   printf("    host:  %s\n", registrarHost);
   printf("    port:  %d\n", registrarPort);
   printf("-----------------------------------------------------\n");
   printf("\n");

   printf("Listening for requests...\n");
   fflush(stdout);

   server.requestLoop();

   printf("\n-----------------------------------------------------\n");
   fflush(stdout);

  	return 0;
}
