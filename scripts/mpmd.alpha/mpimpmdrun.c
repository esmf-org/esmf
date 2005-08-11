/* mpimpmdrun: utilize pprun facility to start MPMD MPI application */

/*
  2005 gjt for ESMF <gtheurich@sgi.com>
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define NP_ARG "-np"
#define PROCF_ARG "-procf"
#define PROCFILE ".mpimpmdrun.procfile"

int main(int argc, char *argv[]){
  int i;
  int helpflag=0;
  char installdir[256];
  char pprun_arg[1024];
  char pprun_call[2048];
  
  pprun_arg[0] = pprun_call[0] = '\0';

  FILE *fp = fopen(PROCFILE, "w");
      
  /* extract the -np arguments from list of arguments if they exist */
  for (i=1; i<argc; i++){
    if (strstr(argv[i], "-h")==argv[i])
      helpflag=1;
    if (strstr(argv[i], "--help")==argv[i])
      helpflag=1;
    if (strstr(argv[i], NP_ARG)==argv[i]){
      if(strlen(argv[i])==strlen(NP_ARG)){
        i+=2;
        if (i<argc)
          fprintf(fp, "%s\t%s\n", argv[i-1], argv[i]);
        else{
          fprintf(stderr, "mpimpmdrun: incomplete -np argument detected...\n");
          exit(0);
        }
        ++i; // take care of the colon
      }
    }else{
      strcat(pprun_arg, argv[i]);
      strcat(pprun_arg, " ");
    }
  }
  fclose(fp);
  
  if (helpflag || argc==1){
    printf("Usage: mpimpmdrun [prun-options] -np X ./executable:\n");
    exit(0);
  }
  
  sprintf(pprun_call, "%s/./pprun %s %s %s", INSTALLDIR, pprun_arg, PROCF_ARG, PROCFILE);
    
  system(pprun_call);
  
  return 0;
}
