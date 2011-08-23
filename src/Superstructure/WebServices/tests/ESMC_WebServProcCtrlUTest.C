#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if !defined (ESMF_OS_MinGW)
#include <unistd.h>
#else
#include <Winsock.h>
#endif

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

#include "ESMCI_WebServProcCtrl.h"

using namespace ESMCI;

static char*	monthStr[] = 
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



/*
****************************************************************************
**
****************************************************************************
*/
void usage()
{
	fprintf(stderr, "\n"
           "USAGE: oirnode [options]\n"
           "WHERE: options is any combination of the following:\n"
           "       [-p port]  Port to listen for ESMF requests on.\n"
           "       [-h]       Print this usage screen.\n"
           "\n");

	exit(0);
}


/*
****************************************************************************
**
****************************************************************************
*/
void  parseArgs(int             argc, 
                char*           argv[], 
                ESMCI_WebServProcCtrl&  server)
{
	for (int i = 1; i < argc; ++i)
	{
		if ((strcmp(argv[i], "-p") == 0)  &&  (i < (argc - 1)))
		{
			//server.setPort(atoi(argv[++i]));
		}
		else if (strcmp(argv[i], "-h") == 0)
		{
			usage();
		}
		else
		{
			fprintf(stderr, "\nERROR: Command line error `%s`\n", argv[i]);
			usage();
		}
	}
}


/*
****************************************************************************
**
****************************************************************************
*/
const char*  getDateAndTime()
{
	static char	datestr[1024];
	time_t			ttime;
	struct tm*		tm;

	ttime = time(NULL);
	tm = localtime(&ttime);

	char*	a = (char*)"am";
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


/*
****************************************************************************
**
****************************************************************************
*/
int  main(int    argc, 
          char*  argv[])
{
  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

	int	result = 0;

	ESMCI_WebServProcCtrl	server(27070, 
                                  "localhost", 
                                  45002, 
                                  "localhost", 
                                  27060, 
                                  5, 
                                  "/home/ksaint/UCAR/ESMF/Baseline/esmf_teragrid/esmf/test/testO/Linux.gfortran.64.openmpi.default", 
                                  "startcompsvr.sh", 
                                  ESMCI_WebServProcCtrl::ESMC_JOBMGRTYPE_FORK);

	// parse command line args...
	parseArgs(argc, argv, server);

	// print a notification banner
	char host[512] = {""};
	gethostname(host, sizeof(host)-1);

	printf("\n");
	printf("ESMF Server\n");
	printf("-----------------------------------------------------\n");
	printf(" date:  %s\n", getDateAndTime());
	printf(" host:  %s\n", host);
	printf(" port:  %d\n", server.getProcCtrlPort());
	printf("-----------------------------------------------------\n");
	printf("\n");

	printf("Listening for requests...\n");
	fflush(stdout);

	server.requestLoop();

	printf("\n-----------------------------------------------------\n");
	fflush(stdout);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


	return 0;
}

