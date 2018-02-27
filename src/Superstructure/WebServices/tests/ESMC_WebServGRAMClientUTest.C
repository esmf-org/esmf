#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include "ESMCI_WebServGRAMClient.h"

using namespace ESMCI;

static char*    monthStr[] =
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
void  parseArgs(int                       argc,
                char*                     argv[],
                ESMCI_WebServGRAMClient*  myclient)
{
        for (int i = 1; i < argc; ++i)
        {
                if ((strcmp(argv[i], "-p") == 0)  &&  (i < (argc - 1)))
                {
                        //server.setPort(atoi(argv[++i]));
                }
                else
                {
                        fprintf(stderr, "\nERROR: Command line error `%s`\n", argv[i]);
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
        static char     datestr[1024];
        time_t                  ttime;
        struct tm*              tm;

        ttime = time(NULL);
        tm = localtime(&ttime);

        char*   a = (char*)"am";
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
        ESMCI_WebServGRAMClient*        myclient =
                                                                                        new ESMCI_WebServGRAMClient("localhost");

        // parse command line args...
        parseArgs(argc, argv, myclient);

        // print a notification banner
        char host[512] = {""};
        gethostname(host, sizeof(host)-1);

        printf("\n");
        printf("ESMF Server\n");
        printf("-----------------------------------------------------\n");
        printf(" date:  %s\n", getDateAndTime());
        printf(" host:  %s\n", host);
        printf("-----------------------------------------------------\n");
        printf("\n");
        fflush(stdout);

//      server.requestLoop();

        printf("\n-----------------------------------------------------\n");
        fflush(stdout);

        return 0;
}

