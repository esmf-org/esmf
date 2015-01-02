/* pprunlaunch: parse the procfile and start up executables in RMS environment*/

/*
  2003 Gerhard Theurich (gjt) NASA/NCCS and SGI Professional Services
  <theurich@zot.gsfc.nasa.gov> or <gtheurich@sgi.com>
*/

#include <stdlib.h>
#include <stdio.h>

int main(int argc, char *argv[]){
  int myrank = atoi(getenv("RMS_RANK"));
  FILE *fp;
  int callflag=0;
  int nproc, proccount=0;
  char procname[80];
  
  /* parse the procfile given in argv[1] */
  fp = fopen(argv[1], "r");
  while(fscanf(fp, "%d %[^\n]", &nproc, procname)!=EOF){
    if (proccount <= myrank && myrank < proccount+nproc){
      callflag=1;
      system(procname);
    }
    proccount += nproc;
  }
  fclose(fp);
  
  if (!callflag)
    fprintf(stderr, "pprunlaunch: no executable found for process: %d\n",
      myrank);

  return 0;
}
