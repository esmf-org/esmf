// $Id: ESMC_LogErr.C,v 1.21 2004/03/19 07:16:52 cpboulder Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Log method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The LogErr class (defined in ESMC\_Log.C and declared in
// the companion file ESMC\_LogErr.h) provides the user a way to write {\tt ESMC\_Log} data.
//
// insert any higher level, 3rd party or system includes here

#include <stdio.h>        
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <time.h>

// associated class definition file
#include <ESMC.h>
#include <ESMC_Base.h>
#include "ESMC_LogErr.h"

//Global Variables
FILE* logErrCFilePtr[10];
int numCFiles=0;
int logErrFortFile[10];
int numFortFiles=0;
char listOfCFileNames[20][32];
char listOfFortFileNames[20][32];

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_LogErr.C,v 1.21 2004/03/19 07:16:52 cpboulder Exp $";
//----------------------------------------------------------------------------
//
// This section includes all the Log routines
//
//----------------------------------------------------------------------------
//
//
//BOP
// !IROUTINE:  ESMC_LogOpenFile -  opens a Log object
//
// !INTERFACE:

void ESMC_Log::ESMC_LogOpenFile(
//
// !RETURN VALUE:
//   none
//
// !ARGUMENTS:

     int numLogFile, //number of log files written (input). Set
                     // to either ESMF\_SINGLE\_LOG\_FILE or
                     // ESMF\_MULT\_LOG\_FILE .

     char name[]     //string to form name of log file (input)

   )
//
// !DESCRIPTION:
// {\tt ESMC\_LogErrOpenFile} takes two
// arguments.  The first should be set to ESMF\_SINGLE\_LOG\_FILE or
// ESMF\_MULT\_LOG\_FILE. These are symbolic constants, defined in
// ESMF\_LogConstants.h, set whether one file should be written for all 
// processes (ESMF\_SINGLE\_LOG\_FILE), or whether one file per process should
// be written (ESMF\_MULT\_LOG\_FILE).
//
// The second argument is a string and is used to form the name of the
// logfile.
//
// This routine is called from native C or C++ code. C I/O libraries are used.
//
//EOP
// 
//

{
   printf("%s",__DATE__," Open");
   if (!ESMC_LogNameValid(name,ESMF_FALSE) ) {
      printf("File name is already being used.\n");
      ESMC_LogExit();
   } 
   switch(numLogFile) {
    
   case ESMF_SINGLE_LOG_FILE:
       oneLogErrFile=ESMF_TRUE;
       strcpy(nameLogErrFile,name);
       break;
      
   case ESMF_MULT_LOG_FILE:
       oneLogErrFile=ESMF_FALSE;
       strcpy(nameLogErrFile,name);
       break;

   default:
     ESMC_LogExit();
  }
  if (oneLogErrFile == ESMF_FALSE) ESMC_LogFormName();
  logErrCFilePtr[numCFiles]=fopen(nameLogErrFile,"a+");
  numFilePtr=numCFiles;
  numCFiles++;
  if (logErrCFilePtr[numCFiles] == NULL) {
     printf("Could not open file.");
     ESMC_LogExit();
  }
     
}   //end ESMC_LogErrOpenFile

//----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogOpenFortFile -  opens a Log object
//
// !INTERFACE:

void ESMC_Log::ESMC_LogOpenFortFile(
//
// !RETURN VALUE:
//   none
//
// !ARGUMENTS:

     int numLogFile, //number of log files written (input). Set
                     // to either ESMF\_SINGLE\_LOG\_FILE or
                     // ESMF_MULT_LOG_FILE .

     char name[]     //string to form name of log file (input)

   )
//
// !DESCRIPTION:
// {\tt ESMC\_LogErrOpenFile} takes two
// arguments.  The first should be set to ESMF\_SINGLE\_LOG\_FILE or
// ESMF\_MULT\_LOG\_FILE. These are symbolic constants, defined in
// ESMF\_LogConstants.h, set whether one file should be written for all 
// processes (ESMF\_SINGLE\_LOG\_FILE), or whether one file per process should
// be written (ESMF\_MULT\_LOG\_FILE).
//
// The second argument is a string and is used to form the name of the
// logfile.
//
// This routine is called from native Fortran code. Fortran I/O libraries are
// used.
//
//EOP
// 
//
{
    
//   ESMC_Logical  fortIsOpen;
   printf("%s",__TIME__," OpenFort");
   if (!ESMC_LogNameValid(name,ESMF_TRUE) ) {
      printf("File name is already being used.\n");
      ESMC_LogExit();
   } 

   switch(numLogFile) {
    
   case ESMF_SINGLE_LOG_FILE:
       oneLogErrFile=ESMF_TRUE;
       strcpy(nameLogErrFile,name);
       break;
      
   case ESMF_MULT_LOG_FILE :
       oneLogErrFile=ESMF_FALSE;
       strcpy(nameLogErrFile,name);
       break;

   default:
     ESMC_LogExit();
  }
  if (oneLogErrFile == ESMF_FALSE) ESMC_LogFormName();
  unitNumber=ESMF_LOG_FORT_UNIT_NUMBER;
  FTN(f_esmf_logopenfortran)(&fortIsOpen,&unitNumber,nameLogErrFile, 
                                                       strlen(nameLogErrFile));
  if (fortIsOpen == ESMF_FALSE) {
     printf("Could not open file.");
     ESMC_LogExit();
  }
  logErrFortFile[numFortFiles]=unitNumber;
  numFortFiles++;
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogNameValid - checks to see if a name has been used before
//
// !INTERFACE:
bool ESMC_Log::ESMC_LogNameValid(
// !RETURN VALUE:
//    none
// !ARGUMENTS:
      char name[],          // name of file
      int FortIO            //are we doing FortIO?
      )
// !DESCRIPTION:
//    Checks to see if a file of the name name has been opened by {\tt ESMC\_Log}.
//    If it has the function returns a false value.  Note: this function
//    use a global array that all {\tt ESMC\_Log} objects have access to.
//
//EOP
{
  int i;
  if (FortIO == ESMF_FALSE) {
    for(i=0; i< numCFiles; i++)
     if (strcmp(name,listOfCFileNames[i])  == 0) 
	return false;
    strcpy(listOfCFileNames[i-1],name);
    return true;
  } else {
    for(i=0; i< numFortFiles;i++)
      if (strcmp(name,listOfFortFileNames[i])  == 0)
          return false;
    strcpy(listOfFortFileNames[i-1],name);
    return true;
  }
}
//----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogInfo - print contents of a Log
//
// !INTERFACE:

void ESMC_Log::ESMC_LogInfo(

//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:

     char* fmt, // c-style character format; subsequent arguments
     ...
    )
//
// !DESCRIPTION:
// {\tt ESMC\_Log\_Info} works similar to C's printf statement.
// The first argument is a character string that can include text to be
// written as well as a description of the number and kind of characters
// to be printed out.
// The format of this string follows the C format description convention,
// though not every feature is supported.  The current conversion specifiers
// are supported: d (signed decimal integer), f (double values),
// and s (string).
//
// Any number of data items may be passed to {\tt ESMC\_Log\_Print}.
// 
// The items are printed on a single line.  Widths, precision, and
// escape sequences are not supported.  If you specify these, the code
// ignores them.
//
//EOP
//


{

 if (verbose == ESMF_FALSE) return;
 int fortIO=ESMF_FALSE;
 ESMC_LogPrintHeader(fortIO);
 char* chPtr;
 va_list argp;

 va_start(argp,fmt);
 for (chPtr=fmt; *chPtr; chPtr++) {
   if ( *chPtr != '%') {
    fprintf(logErrCFilePtr[numFilePtr],"%c",*chPtr);
    if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
    continue;
   }
   chPtr++;
   while (*chPtr == '-' || isdigit(*chPtr) || *chPtr == '.' ||
                 *chPtr == 'h' || *chPtr=='l') chPtr++;
   switch (*chPtr) {
     case 'c':
      fprintf(logErrCFilePtr[numFilePtr],"%c ",(char)(va_arg(argp, int)));
      if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
      break;
     case 's':
      fprintf(logErrCFilePtr[numFilePtr],"%s ",va_arg(argp, char*));
      if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
      break;
     case 'd':
      fprintf(logErrCFilePtr[numFilePtr],"%d ",va_arg(argp, int));
      if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
      break;
     case 'f':
      fprintf(logErrCFilePtr[numFilePtr],"%f ",va_arg(argp, double));
      if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
      break;
     default:
      printf("Error in character descriptor.");
      if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
      ESMC_LogExit();
    }
  }
  fprintf(logErrCFilePtr[numFilePtr],"\n");
  fprintf(logErrCFilePtr[numFilePtr],"\n");
  if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
  va_end(argp);
} // end ESMC_LogErrInfo

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogInfoFortran - print contents of a Log from Fortran
//
// !INTERFACE:

void ESMC_Log::ESMC_LogInfoFortran(

//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
     
     char fmt[],           // array for c-style character format (input)

     char charData[],      // holds character data
     char strData[][32],   // two dimensional array for string data;
                           // strDat[i][j] is the jth
			   // character of string i. (input)

     int intData[],        // array storing integer data to be printed (input)

     double floatData[]    // array storing double data to be printed (input) 

    )
//
// !DESCRIPTION:
// {\tt ESMC\_LogInfoFortran} is the version of {\tt ESMC\_LogInfo} used for the Fortran
// interface.  Instead of printing the data from the stack as {\tt ESMC\_LogInfo} 
// does, {\tt ESMC\_LogInfoFortran} prints the data stored in the "Data" arrays. 
// The routine is called from {\tt ESMF\_LogPrint} which is  defined in
// ESMF\_LogInterface.C.  {\tt ESMF\_LogPrint} is callable from the user's
// from Fortran code.  It is this routine that takes the data
// from the stack and stores it in the "Data" arrays.
//
//
//EOP
//

{


 if (verbose == ESMF_FALSE) return;
 int fortIO=ESMF_TRUE;
 ESMC_LogPrintHeader(fortIO);
 int numStr=0;
 char* chPtr;
 char msg[72]; 
 char stringTemp[72];
 char stringToPrint[72];
 int len;

 stringToPrint[0]='\0';
 for (chPtr=fmt; *chPtr; chPtr++)
  {
    if (*chPtr != '%') {
      sprintf(stringTemp,"%c",*chPtr);
      strcat(stringToPrint,stringTemp);
   } else {
   chPtr++;
   while (*chPtr == '-' || isdigit(*chPtr) || *chPtr == '.' ||
                 *chPtr == 'h' || *chPtr=='l') chPtr++;
   switch (*chPtr) {
     case 'c':
      sprintf(stringTemp,"%c",*charData);
      strcat(stringToPrint,stringTemp);
      charData++;
      break;
     case 's':
      strcpy(msg,strData[numStr]);
      strcat(stringToPrint,msg);
      numStr++;
      break;
     case 'd':
      sprintf(stringTemp,"%d",*intData);
      strcat(stringToPrint,stringTemp);
      intData++;
      break;
     case 'f':
      sprintf(stringTemp,"%f",*floatData);
      strcat(stringToPrint,stringTemp);
      floatData++;
      break;
     default:
      ESMC_LogExit();
    }
   }
  }
 len=strlen(stringToPrint);
 FTN(f_esmf_logprintstring)(&unitNumber,stringToPrint,&flush,len);
 FTN(f_esmf_logprintnewline)(&unitNumber,&flush);
}     
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogPrintHeader - prints header data
//
// !INTERFACE:
   void ESMC_Log::ESMC_LogPrintHeader(
//
// !RETURN VALUE:
//    none 
//
// !ARGUMENTS:
     int fortIO       //if set to ESMF_TRUE, use fortran io libraries

   )
//
// !Description:
// This is a private method, used by LogErr's various
// print methods to print header
// data to the log file.  This data consists of a time stamp, an ascii string 
// holding the day, month, time, and year. If the code is running in parallel
// with MPI, the header also consists of the PE number.
//
//EOP
{
#ifdef HAS_MPI
  int rank;
#endif
  struct tm *timePtr;  
  time_t timeVal;   
  char* timeAsc;    
  char underline[]="-------------------------";
  time(&timeVal);
  timePtr=localtime(&timeVal);
  timeAsc=asctime(timePtr);
  int len1=strlen(timeAsc);
  timeAsc[len1-1]='\0';
  if (fortIO == ESMF_TRUE) { 
   FTN(f_esmf_logprintstring)(&unitNumber,timeAsc,&flush,len1);
   len1=strlen(underline);
   FTN(f_esmf_logprintstring)(&unitNumber,underline,&flush,len1);

  } else {
    fprintf(logErrCFilePtr[numFilePtr],"%s",timeAsc);
    fprintf(logErrCFilePtr[numFilePtr],"%s",underline);
    if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
  } 
#ifdef HAS_MPI
  rank=MPI::COMM_WORLD.Get_rank();
  if (fortIO == ESMF_TRUE) { 
   sprintf(stringToPrint,"PE: %d",rank);
   FTN(f_esmf_logprintstring)(&unitNumber,stringToPrint,&flush,72);
  } else {
    fprintf(logErrCFilePtr[numFilePtr],"PE: %d",rank);
    if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
  } 
#endif
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogGet - get value of verbose, flush, haltOnErr, and/or haltOnWarn
//
// !INTERFACE:
   void ESMC_Log::ESMC_LogGet(
//
// !RETURN VALUE:
//  none
// !ARGUMENTS:
//  An arbitrary number of "option, value" pairs

   char* option, ESMC_Logical & value,...)
// 
// !DESCRIPTION:
//  This method returns the value of the argument option in the variable value.
//  Option may be set tot he string verbose, flush,haltOnErr or haltOnWarn
//EOP
{
  char flushOption[]="flush";
  char verboseOption[]="verbose";
  char haltOnErrOption[] = "haltOnErr";
  char haltOnWarnOption[] = "haltOnWarn";
  va_list argp;
  va_start(argp,value);
  for (;;)
  {
    if (strcmp(option,flushOption) == 0) {
//      value=flush;
       value=ESMF_TRUE;
    } else if (strcmp(option,verboseOption) == 0) {
      value=verbose;
    } else if (strcmp(option,haltOnErrOption) == 0) {
      value=haltOnErr;
    } else if (strcmp(option,haltOnWarnOption) == 0) {
      value=haltOnWarn;
    } else {
      printf("Error using ESMC_Set");
      ESMC_LogExit();
    }
    option= va_arg(argp, char*);
    if (!(*option)) break;
    //value = va_arg(argp, ESMC_Logical);
    value = (ESMC_Logical)va_arg(argp, int);
  }
} 

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogSet - sets the value of verbose, flush, haltOnWarn and/or haltOnErr
//
// !INTERFACE:
   void ESMC_Log::ESMC_LogSet(

// !RETURN VALUE 
//   none
//
// !ARGUMENTS
//  An arbitrary number of "option, value" pairs
//
    char* option,ESMC_Logical value, ...) 
// !DESCRIPTION:
// This method returns the value of the argument option in the variable value.
// Option may be set tot he string verbose, flush,haltOnErr or haltOnWarn
//EOP
// 
{ 
  char flushOption[]="flush";
  char verboseOption[]="verbose";
  char haltOnErrOption[] = "haltOnErr";
  char haltOnWarnOption[] = "haltOnWarn";
  va_list argp;
  va_start(argp,value);
  for (;;) 
  {
    if (strcmp(option,flushOption) == 0) {
      flush=value;
    } else if (strcmp(option,verboseOption) == 0) { 
      verbose=value;
    } else if (strcmp(option,haltOnErrOption) == 0) { 
      haltOnErr=value;
    } else if (strcmp(option,haltOnWarnOption) == 0) { 
      haltOnWarn=value;
    } else {
      printf("Error using ESMC_Set");
      ESMC_LogExit();
    }
    option= va_arg(argp, char*);
    if (!(*option)) break;
    //value = va_arg(argp, ESMC_Logical);
    value = (ESMC_Logical)va_arg(argp, int);
  }
}    
   

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_GetFileHandle - used in conjunction with fprintf to write
// data to the log file.
//
// !INTERFACE:
   FILE* ESMC_Log::ESMC_GetFileHandle(
//
// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
//  none
   )
//
// !DESCRIPTION:
// This method is used in conjunction with fprintf 
// to write data to the log file, eg. fprintf(aLog.ESMC\_Getit(),"Hi")
// The routine writes header data, and
// then returns a point to a file structure to fprintf
//
//EOP
{
  int fortIO=ESMF_FALSE;
  ESMC_LogPrintHeader(fortIO);
  return logErrCFilePtr[numFilePtr];
}
//
//----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogGetUnit - used in conjunction with standard Fortran write
// to write data to log file. 
//
// !INTERFACE:
int ESMC_Log::ESMC_LogGetUnit(
//
// !RETURN VALUE:
//    unit number 
//
// !ARGUMENTS:
//     none.

   )
//
// !DESCRIPTION:
// This is method is
// used with the standard Fortran write function, eg.
// write(ESMC\_LogGetUnitNumber(aLog),*) 'Hi '. 
// The routine writes header data, and
// then returns a unit number to the Fortran write function.
//
//EOP

{
#ifdef HAS_MPI
  char name[32];
  char rankName[3];
  int rank;
#endif

  int fortIO=ESMF_TRUE;
  ESMC_LogPrintHeader(fortIO);
  return unitNumber;
}

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogFormName - private method that forms
//  the name of the LogErr file to be written when multiple LogErr
//  files are to be written.
//
// !INTERFACE:

void ESMC_Log::ESMC_LogFormName(

//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//     none.

   )
//
// !DESCRIPTION:
// This routine forms the names the LogErr files by appending the
// PE number to base name specified in the Open method.  This method
// is used only when running in parallel on multiple PEs.
//
//EOP

{

#ifdef HAS_MPI
  char rankName[3];
  int rank;
#endif

#ifdef HAS_MPI
     rank=MPI::COMM_WORLD.Get_rank();
     sprintf(rankName,"%d",rank);
     strcat(nameLogErrFile,rankName);
#endif
}

//-----------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogCloseFortFile - closes log file. 
//
// !INTERFACE:
void ESMC_Log::ESMC_LogCloseFortFile(
//
// ! RETURN VALUE:
//    none
//
// !ARGUMENTS:
//   none

   )
//
// ! DESCRIPTION:
// This routine simply closes the log file(s).  It also removes
// file from the global file array. The routine is called from native
// Fortran code (File is closed with Fortran I/O libraries.)
//EOP
{
  int i,j;
  if (standardOut == ESMF_FALSE) {
    if (fortIsOpen==ESMF_TRUE) {
      FTN(f_esmf_logclosefortran)(&unitNumber);
      for( i=0; i<numFortFiles; i++)
        if (strcmp(nameLogErrFile,listOfFortFileNames[i])  == 0) {
         for(j=i+1;j+numFortFiles; j++)
          strcpy(listOfFortFileNames[j-1],listOfFortFileNames[j]);
         for(j=i+1;j+numFortFiles; j++)
	  logErrFortFile[j-1]=logErrFortFile[j]; 
         numFortFiles--;
         break;
        }
    }
  }
}
//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogCloseFile - closes log file. 
//
// !INTERFACE:
void ESMC_Log::ESMC_LogCloseFile(
//
// ! RETURN VALUE:
//    none
//
// !ARGUMENTS:
//   none

   )
//
// ! DESCRIPTION:
// This routine simply closes the log file(s).  It also removes
// file from the global file array. The routine is called from native
// C/C++ code (File is closed with C I/O libraries.)
//
//EOP

{
  int i,j;

   if (standardOut == ESMF_FALSE) {
     if (logErrCFilePtr[numFilePtr] != NULL) {
       fclose(logErrCFilePtr[numFilePtr]);
     }
    for( i=0; i< numCFiles; i++)
      if (strcmp(nameLogErrFile,listOfCFileNames[i])  == 0) {
       for(j=i+1;j<numCFiles; j++)
          strcpy(listOfCFileNames[j-1],listOfCFileNames[j]);
       for(j=i+1;j<numCFiles; j++)
	  logErrCFilePtr[j-1]=logErrCFilePtr[j];
       numCFiles--;
       break;
      } 
   }
}


 

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogErr - write error message to log file
//
// !INTERFACE:

void ESMC_Log::ESMC_LogErr_(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:

   int errCode,     // Error code
   int line,        // Line number
   char file[],     // Filename
   char dir[]      // Directory
  )
// !DESCRIPTION:
// Prints error code, corresponding message, line number, file, directory
// that error occurred at. 
//EOP

{

   if (verbose == ESMF_FALSE) return;
   int fortIO=ESMF_FALSE;
   char msg[1];
   msg[0]='\0';
   ESMC_LogPrintHeader(fortIO);
   ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);

   if (haltOnErr==ESMF_TRUE) {
    ESMC_LogCloseFile();
    ESMC_LogExit();
   }
 }


//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogErrMsg - write error message to log file
//
// !INTERFACE:

void ESMC_Log::ESMC_LogErrMsg_(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:

   int errCode,     // Error code
   int line,        // Line number
   char file[],     // Filename
   char dir[],     // Directory
   char msg[]
  )
// !DESCRIPTION:
// Prints error code, corresponding message, line number, file, directory
// that error occurred at.  Can also print a message.
//EOP

{

   if (verbose == ESMF_FALSE) return;
   int fortIO=ESMF_FALSE;
   ESMC_LogPrintHeader(fortIO);
   ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);

   if (haltOnErr==ESMF_TRUE) {
    ESMC_LogCloseFile();
    ESMC_LogExit();
   }
}

//----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_logWrite - write to log file
//
// !INTERFACE:

void ESMC_Log::ESMC_LogWrite(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
      int logtype,  // Log Type   
      char msg[]	// Log Entry
    )
// !DESCRIPTION:
// Prints log messsge, line number, file, directory
//EOP
{ 
 	int fortIO=ESMF_FALSE;
#ifdef HAS_MPI
 	int rank;
#endif
	flush=ESMF_TRUE;
  	struct tm *timePtr;  
  	time_t timeVal;   
  	char* timeAsc;    

	
  	time(&timeVal);
  	timePtr=localtime(&timeVal);
  	timeAsc=asctime(timePtr);
  	int len1=strlen(timeAsc);
  	timeAsc[len1-1]='\0';
    fprintf(logErrCFilePtr[numFilePtr],"%s",timeAsc," :",msg);
    //fprintf(logErrCFilePtr[numFilePtr],"%s",underline);
    if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
/*  
//#ifdef HAS_MPI
  rank=MPI::COMM_WORLD.Get_rank();
  if (fortIO == ESMF_TRUE) { 
   sprintf(stringToPrint,"PE: %d",rank);
   FTN(f_esmf_logprintstring)(&unitNumber,stringToPrint,&flush,72);
  } else {
    fprintf(logErrCFilePtr[numFilePtr],"PE: %d",rank);
    if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
  } 
#endif
*/
}


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogWarn -- Print warning message
//
// !INTERFACE:
      void ESMC_Log::ESMC_LogWarn_(
//
//
// !ARGUMENTS:
      int errCode,     // Error code
      int line,        // Line number
      char file[],     // Filename
      char dir[]      // Directory
    )
//
// !DESCRIPTION:
//  Same as {\tt ESMC\_LogErr}, except execution is not stopped after
//  printing message, except when haltOnWarn set to true
//
//EOP
{ 

 if (verbose == ESMF_FALSE) return;
 int fortIO=ESMF_FALSE;
 char msg[1];
 msg[0]='\0';
 ESMC_LogPrintHeader(fortIO);
 ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);

 if (haltOnWarn == ESMF_TRUE) {
    ESMC_LogCloseFile();
    ESMC_LogExit();
 }

}

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogWarnMsg -- Print warning message
//
// !INTERFACE:
      void ESMC_Log::ESMC_LogWarnMsg_(
//
//
// !ARGUMENTS:
      int errCode,     // Error code
      int line,        // Line number
      char file[],     // Filename
      char dir[],      // Directory
      char msg[]
    )
//
// !DESCRIPTION:
//  Same as {\tt ESMC\_LogErr}, except execution is not stopped after
//  printing message, except when haltOnWarn set to true
//
//EOP
{ 

 if (verbose==ESMF_FALSE) return;
 int fortIO=ESMF_FALSE;
 ESMC_LogPrintHeader(fortIO);
 ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);

 if (haltOnWarn == ESMF_TRUE) {
    ESMC_LogCloseFile();
    ESMC_LogExit();
 }

}

//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogExit() - private routine uses by Log to stop program
//
// !INTERFACE:
//
void ESMC_Log::ESMC_LogExit(
// !RETURN VALUE:
//  none
// !ARGUMENTS:
//  none
  )
// !DESCRIPTION:
// Used by {\tt ESMC\_Log} to exit program.
//
//EOP

{
  #ifdef HAS_MPI
    if ( MPI::Is_Initialized() ) {
      MPI::COMM_WORLD.Abort(-1);
     } else {
      exit(EXIT_FAILURE);
     }
  #else
      exit(EXIT_FAILURE);
  #endif

}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogErrFortran - called by fortran wrapper to write error msg
//
// !INTERFACE:

void ESMC_Log::ESMC_LogErrFortran(

// !ARGUMENTS:

    int errCode,         //error code
    int line,            //line number
    char file[],         //file error occurred in
    char dir[],          //directory error occurred in
    char msg[]           //msg
  )
// !DESCRIPTION:
//Similar to {\tt ESMC\_LogErr}, except called by the fortran wrapper
//esmf\_logerr which is defined in
//ESMC\_Interface.C.  The major difference between this routine
//and {\tt ESMC\_LogErr} is that this
//routine prints the printf style data from the Data arrays not the stack.
//EOP

{

  if (verbose == ESMF_FALSE) return;
  int fortIO=ESMF_TRUE;
  ESMC_LogPrintHeader(fortIO);
  ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);


  if (haltOnErr == ESMF_TRUE) {
    ESMC_LogCloseFortFile();
    ESMC_LogExit();
  }
    

}



//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogWarnFortran - called by fortran wrapper to write warnings
//
// !INTERFACE:

void ESMC_Log::ESMC_LogWarnFortran(
// !RETURN VALUE:
//  none
// !ARGUMENTS:

     int errCode,
     int line,
     char file[],
     char dir[],
     char msg[]

)
// !DESCRIPTION:
// Similar to {\tt ESMC\_LogWarn}, except called by the fortran
// wrapper esmf\_logerr which is
// defined in ESMC\_Interface.C
//EOP

{
 if (verbose == ESMF_FALSE) return;
 int fortIO=ESMF_TRUE;
 ESMC_LogPrintHeader(fortIO);
 ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);

 if (haltOnWarn == ESMF_TRUE) {
    ESMC_LogCloseFortFile();
    ESMC_LogExit();
 }

}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_Log() - constructor
// !INTERFACE:

  ESMC_Log::ESMC_Log(
//
// !RETURN VALUE:
//  none
// !ARGUMENTS:
//  none

    )
//
// !DESCRIPTION:
// This is the constructor.  Sets verbose, flush, haltOnErr and haltOnWarn
// and other variables to their default values.
//
//EOP
  {
    verbose=ESMF_TRUE;
    flush=ESMF_FALSE;
    haltOnErr=ESMF_TRUE;
    haltOnWarn=ESMF_FALSE;
    nameLogErrFile[0]='\0';
    oneLogErrFile=ESMF_TRUE;
    standardOut=ESMF_TRUE;
    fortIsOpen=ESMF_FALSE;
    numFilePtr=numCFiles;
    unitNumber=50;
    flush=ESMF_FALSE;
    logErrCFilePtr[numFilePtr]=stdout;
}


//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogPrint - prints to the log file
// 
// !INTERFACE:

void ESMC_Log:: ESMC_LogPrint(

// !ARGUMENTS:

   int fortIO,          //if set to ESMF_TRUE use Fortran IO libraries
   int errCode,
   int line,            // see LogErr for a definition
   char file[],         // of these variables
   char dir[],           
   char msg[]            // optional msg

  )

// !DESCRIPTION: This is a private routine, used by many methods of
// {\tt ESMC\_Log} to print data to the log file. If used from Fortran, then
// Fortran I/O libraries are used.  Otherwise C I/O libraries are used.
//EOP
{
 char errMsg[32];
 char stringToPrint[72];
 int len;
 ESMC_LogGetErrMsg(errCode,errMsg);
 if (fortIO == ESMF_TRUE) {

  sprintf(stringToPrint,"Error Code: %d",errCode);
  len=strlen(stringToPrint);
  FTN(f_esmf_logprintstring)(&unitNumber,stringToPrint,&flush,len);
  
  len=strlen(errMsg);
  FTN(f_esmf_logprintstring)(&unitNumber,errMsg,&flush,len);

  sprintf(stringToPrint,"Directory: %s",dir);
  len=strlen(stringToPrint);
  FTN(f_esmf_logprintstring)(&unitNumber,stringToPrint,&flush,len);

  sprintf(stringToPrint,"File: %s",file);
  len=strlen(stringToPrint);
  FTN(f_esmf_logprintstring)(&unitNumber,file,&flush,len);

  sprintf(stringToPrint,"Line: %d",line);
  len=strlen(stringToPrint);
  FTN(f_esmf_logprintstring)(&unitNumber,stringToPrint,&flush,len);

  if (msg != '\0') {
     sprintf(stringToPrint,"Comments: %s",msg);
     len=strlen(stringToPrint);
     FTN(f_esmf_logprintstring)(&unitNumber,stringToPrint,&flush,len);
  }
  FTN(f_esmf_logprintnewline)(&unitNumber,&flush);
 } else {
  fprintf(logErrCFilePtr[numFilePtr],"Error Code: %d\n",errCode);
  if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
  fprintf(logErrCFilePtr[numFilePtr],"%s\n",errMsg);
  if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
  fprintf(logErrCFilePtr[numFilePtr],"Directory: %s \n",dir);
  if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
  fprintf(logErrCFilePtr[numFilePtr],"File: %s\n",file);
  if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
  fprintf(logErrCFilePtr[numFilePtr],"Line: %d\n",line);
  if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
  if ((msg != NULL) && (msg[0] != '\0')) {
    fprintf(logErrCFilePtr[numFilePtr],"Comments: %s\n",msg);
    if (flush==ESMF_TRUE) fflush(logErrCFilePtr[numFilePtr]);
  }
  fprintf(logErrCFilePtr[numFilePtr],"\n");
  fprintf(logErrCFilePtr[numFilePtr],"\n");
 }
}
//----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogGetErrMsg -- Return error message for given error code.
//
// !INTERFACE:
      void ESMC_Log::ESMC_LogGetErrMsg(
// !RETURN VALUE:
//  none
// !ARGUMENTS:
//
     int errCode,
     char msg[]
     ) const
//
// !DESCRIPTION:
// {\tt ESMC\_GetErrMsg} returns a string corresponding to the error code
//
//EOP
{
 switch (errCode)
 {
  case ESMF_ERR_FILE_OPEN:
           strcpy(msg,"Error opening file");
           break;
  case ESMF_ERR_FILE_READ:
           strcpy(msg,"Unable to read from file");
           break;
  case ESMF_ERR_FILE_WRITE:
           strcpy(msg,"Unable to write to file.");
           break;
  case ESMF_ERR_FILE_CLOSE:
           strcpy(msg,"Unable to close file.");
           break;
  case ESMF_ERR_INIT:
           strcpy(msg,"Init method not called.");
           break;
  case ESMF_ERR_FILE_ACTIVE:
           strcpy(msg,"Instrumented region still active.");
           break;
  case ESMF_FATAL:
           strcpy(msg,"Fatal Error");
           break;
  case ESMF_WARNING:
           strcpy(msg,"Warning Detected.");
           break;
  default:
           printf("Unknown error code.\n");
           exit(EXIT_FAILURE);
 }     
} // end ESMC_GetErrMsg

 
//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetFlush() - set the flushSet variable.
// !INTERFACE:

/*inline*/ void ESMC_Log::ESMC_LogSetFlush(
//
// !RETURN VALUE:
//  none
// !ARGUMENTS:
//   none

   ) 

// !DESCRIPTION: 
// Causes output to be flushed.
// 
//EOP
{
      flush=ESMF_TRUE;
}


//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogGetFlush() - returns the flush variable 
// !INTERFACE:

ESMC_Logical ESMC_Log::ESMC_LogGetFlush (
//
// !RETURN VALUE
//  Value of flush
// !ARGUMENTS:
//   none

   ) const 

// !DESCRIPTION: 
// Returns the flush variable 
// 
//EOP
{
      return flush;
}




//----------------------------------------------------------------------------
//BOP                
//                   
// !IROUTINE: ESMC_LogSetNotFlush() - output not flushed
// !INTERFACE:       
	     
/*inline*/ void ESMC_Log::ESMC_LogSetNotFlush(
//
// !RETUN VALUE:
//  none
// !ARGUMENTS        
//   none   

 )    
	     
// !DESCRIPTION:
// Causes output not to be flushed.
//EOP
{                    
   flush=ESMF_FALSE;
}               
									


//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetVerbose - make output verbose 
//
// !INTERFACE:

/*inline*/ void ESMC_Log::ESMC_LogSetVerbose(
//
// !RETURN VALUE:
//  none
// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If theVerbosity is set to ESMF\_TF\_TRUE, messages are printed out. 
// 
//EOP
{
     verbose=ESMF_TRUE;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogGetVerbose - return verbose 
//
// !INTERFACE:

ESMC_Logical ESMC_Log::ESMC_LogGetVerbose(
//
// !RETURN VALUE:
//  value of verbose
// !ARGUMENTS
//   none
  ) const

// !DESCRIPTION:
// Returns  verbose value 
// 
//EOP
{
     return verbose;
}


//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetNotVerbose - output not verbose 
//
// !INTERFACE:

/*inline*/ void ESMC_Log::ESMC_LogSetNotVerbose(
// RETURN VALUE:
//  none
// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If theVerbosity is set to ESMC\_TF\_FALSE, no messages are printed out. 
// 
//EOP
{
     verbose=ESMF_FALSE;
}



//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetHaltOnErr - code will stop on encountering an error  
//
// !INTERFACE:

/*inline*/ void ESMC_Log::ESMC_LogSetHaltOnErr(
// RETURN VALUE:
//  none
// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnErr is set to ESMC\_TF\_TRUE, code will stop executing when
// encountering an error.
// 
//EOP
{
     haltOnErr=ESMF_TRUE;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogGetHaltOnErr - returns haltOnErr
//
// !INTERFACE:

ESMC_Logical ESMC_Log::ESMC_LogGetHaltOnErr(
//
// !RETURN VALUE
//  haltOnErr
// !ARGUMENTS
//   none
  ) const

// !DESCRIPTION:
// Returns haltOnErr
// 
//EOP
{
     return haltOnErr;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetNotHaltOnErr - code will not stop on encountering
// an error  
//
// !INTERFACE:

/*inline*/ void ESMC_Log::ESMC_LogSetNotHaltOnErr(
//
// !RETURN VALUE:
//  none
// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnErr is set to ESMC\_TF\_FALSE, code will not stop executing when
// encountering an error.
// 
//EOP
{
     haltOnErr=ESMF_FALSE;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetHaltOnWarn - code will stop on encountering
// a warning
//
// !INTERFACE:

/*inline*/ void ESMC_Log::ESMC_LogSetHaltOnWarn(
//
// !RETURN VALUE:
// none
// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnWarn is set to ESMC\_TF\_TRUE, code will stop executing when
// encountering an error.
// 
//EOP
{
     haltOnWarn=ESMF_TRUE;
}

//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogGetHaltOnWarn - returns haltOnwarn value
// a warning
//
// !INTERFACE:

ESMC_Logical ESMC_Log::ESMC_LogGetHaltOnWarn(
//
// !RETURN VALUE
//  Value of HaltOnWarn
// !ARGUMENTS
//   none
  ) const

// !DESCRIPTION:
// Returns haltOnWarn
// 
//EOP
{
     return haltOnWarn;
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogSetNotHaltOnWarn - code will not stop on encountering
// a warning
//
// !INTERFACE:

/*inline*/ void ESMC_Log::ESMC_LogSetNotHaltOnWarn(
//
// !RETURN VALUE:
//  none
// !ARGUMENTS
//   none
  )

// !DESCRIPTION:
// If haltOnWarn is set to ESMC\_Tf\_FALSE, code will not stop executing when
// encountering an error.
// 
//EOP
{
     haltOnWarn=ESMF_FALSE;
}


