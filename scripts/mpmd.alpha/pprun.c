/* pprun: provide -procf option to prun */

/*
  2003 Gerhard Theurich (gjt) NASA/NCCS and SGI Professional Services
  <theurich@zot.gsfc.nasa.gov> or <gtheurich@sgi.com>
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define PROCF_ARG "-procf"

int main(int argc, char *argv[]){
  int i;
  int helpflag=0;
  char installdir[256];
  char procfile[256];
  char prun_arg[1024];
  char prun_call[2048];
  
  procfile[0] = prun_arg[0] = prun_call[0] = '\0';
    
  /* extract the procfile argument from list of arguments if it exists */
  for (i=1; i<argc; i++){
    if (strstr(argv[i], "-h")==argv[i])
      helpflag=1;
    if (strstr(argv[i], "--help")==argv[i])
      helpflag=1;
    if (strstr(argv[i], PROCF_ARG)==argv[i]){
      if(strlen(argv[i])==strlen(PROCF_ARG)){
        if (++i<argc)
          strcpy(procfile, argv[i]);
        else{
          fprintf(stderr, "pprun: incomplete -procf argument detected...\n");
          exit(0);
        }
      }else
        strcpy(procfile, argv[i]+strlen(PROCF_ARG));
    }else{
      strcat(prun_arg, argv[i]);
      strcat(prun_arg, " ");
    }
  }
  
  if (helpflag || argc==1){
    printf("Usage: pprun [prun-options] -procf file | executable:\n");
    printf("  the procfile contains lines of the format \"nproc procname"
           " [options]\"\n");
    printf("  if instead an executable is give pprun becomes prun\n");
    exit(0);
  }
  
  sprintf(prun_call, "prun %s ", prun_arg);
  if (strlen(procfile)){
#ifdef INSTALLDIR
    strcat(prun_call, INSTALLDIR"/pprunlaunch ");
#else
    strcat(prun_call, "./pprunlaunch ");
#endif
    strcat(prun_call, procfile);
  }
    
  system(prun_call);
  
  return 0;
}
