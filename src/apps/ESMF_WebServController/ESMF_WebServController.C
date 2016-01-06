/* $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
//#include "ESMCI_WebServPassThruSvr.h"
#include "ESMC.h"
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
int print_usage() 
{
   printf("ESMF_WebServController: Run a Process Controller that provides access to an ESMF Web Service enabled Component.\n");
   printf("Usage: ESMF_WebServController [--help] [--version] [-V] procCtrlPort registrarHost registrarPort\n");
   printf("    [--help]        Display this information and exit.\n");
   printf("    [--version]     Display ESMF version and license information "
        "and exit.\n");
   printf("    [-V]            Display ESMF version string and exit.\n");
   printf("    procCtrlPort    Port num for Process Controller listener.\n");
   printf("    registrarHost   Host name on which Registrar is running.\n");
   printf("    registrarPort   Port num on which Registrar is listening.\n");
   printf("    runScriptDir    Directory containing run script.\n");
   printf("    runScriptFile   File name of run script.\n");
   printf("\n");
   return 0;
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
int main(int    argc, 
         char*  argv[])
{
   string   compSvrHost = "localhost";
   int      compSvrStartPort = 27060;
   int      portPoolSize = 5;
   //string   compSvrScriptDir = "./";
   //string   compSvrScriptName = "runjob.sh";


   int	argIndex = 0;
   int	argFlag = 0;
   int	vFlag = 0;
   int	versionFlag = 0;
   int   rc = 0;

   /* check for standard command line arguments */
   /* if any of these flags are set, print out the info and exit */
   argIndex = ESMC_UtilGetArgIndex(argc, argv, "--help", &rc);
   if (argIndex >= 0)
   {
      argFlag = 1;
      print_usage();
   }

   argIndex = ESMC_UtilGetArgIndex(argc, argv, "--version", &rc);
   if (argIndex >= 0)
   {
      argFlag = 1;
      versionFlag = 1;
   }

   argIndex = ESMC_UtilGetArgIndex(argc, argv, "-V", &rc);
   if (argIndex >= 0)
   {
      argFlag = 1;
      vFlag = 1;
   }

   if (argFlag)
   {
      ESMC_UtilVersionPrint(vFlag, versionFlag, &rc);
		return 0;
   }

	if (argc < 6)
	{
      print_usage();
		return 1;
	}

   int      procCtrlPort = atoi(argv[1]);
   char     registrarHost[256];
   strcpy(registrarHost, argv[2]);
   int      registrarPort = atoi(argv[3]);

   //string   compSvrScriptDir = "./";
   //string   compSvrScriptName = "runjob.sh";
   char compSvrScriptDir[512];
   strcpy(compSvrScriptDir, argv[4]);
   char compSvrScriptName[512];
   strcpy(compSvrScriptName, argv[5]);

   ESMC_Initialize(NULL, ESMC_InitArgLogKindFlag(ESMC_LOGKIND_SINGLE), ESMC_ArgLast);

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
   printf("    script dir: %s\n", compSvrScriptDir);
   printf("    script name: %s\n", compSvrScriptName);
   printf("-----------------------------------------------------\n");
   printf("\n");

   printf("Listening for requests...\n");
   fflush(stdout);

   server.requestLoop();

   printf("\n-----------------------------------------------------\n");
   fflush(stdout);

   ESMC_Finalize();

  	return 0;
}
