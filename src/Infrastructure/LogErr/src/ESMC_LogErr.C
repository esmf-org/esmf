// $Id: ESMC_LogErr.C,v 1.6 2003/04/03 20:05:15 nscollins Exp $
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
// The LogErr class (defined in ESMC_Log.C and declared in
// the companion file ESMC_LogErr.h) provides the user a way to write Log data.
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
#include "ESMC_LogErr.h"

//Global Variables
FILE* logErrFilePtr[10];
int numFileGlobal=0;
int logErrFileFortran[10];
int numFileFortGlobal=1;
char listOfFileNames[20][32];
char listOfFortFileNames[20][32];

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_LogErr.C,v 1.6 2003/04/03 20:05:15 nscollins Exp $";
//----------------------------------------------------------------------------/
//
// This section includes all the Log routines
//
//---------------------------------------------------------------------------------
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
		     // to either ESMF_SINGLE_LOG_FILE or
		     // ESMF_MULT_LOG_FILE .

     char name[]     //string to form name of log file (input)

   )
//
// !DESCRIPTION
// ESMC\_LogErrOpenFile takes two
// arguments.  The first should be set to ESMF\_SINGLE\_LOG\_FILE or
// ESMF\_MULT\_LOG\_FILE. These are symbolic constants, defined in
// ESMF\_LogConstants.h, set whether one file should be written for all 
// processes (ESMF_SINGLE\_LOG\_FILE), or whether one file per process should
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
   
   if (!ESMC_LogNameValid(name) ) {
      printf("File name is already being used.\n");
      ESMC_LogExit();
   } 
   switch(numLogFile) {
    
   case ESMF_SINGLE_LOG_FILE:
       oneLogErrFile=ESMF_LOG_TRUE;
       strcpy(nameLogErrFile,name);
       break;
      
   case ESMF_MULT_LOG_FILE:
       oneLogErrFile=ESMF_LOG_FALSE;
       strcpy(nameLogErrFile,name);
       break;

   default:
     ESMC_LogExit();
  }
  if (oneLogErrFile == ESMF_LOG_FALSE) ESMC_LogFormName();
  logErrFilePtr[numFilePtr]=fopen(nameLogErrFile,"a+");
  if (logErrFilePtr[numFilePtr] == NULL) {
     printf("Could not open file.");
     ESMC_LogExit();
  }
     
 }   //end ESMC_LogErrOpenFile

//-------------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogOpenFile -  opens a Log object
//
// !INTERFACE:

void ESMC_Log::ESMC_LogOpenFileForWrite(
//
// !RETURN VALUE:
//   none
//
// !ARGUMENTS:

     int numLogFile, //number of log files written (input). Set
		     // to either ESMF_SINGLE_LOG_FILE or
		     // ESMF_MULT_LOG_FILE .

     char name[]     //string to form name of log file (input)

   )
//
// !DESCRIPTION
// ESMC\_LogErrOpenFile takes two
// arguments.  The first should be set to ESMF_SINGLE_LOG_FILE or
// ESMF\_MULT\_LOG\_FILE. These are symbolic constants, defined in
// ESMF\_LogConstants.h, set whether one file should be written for all 
// processes (ESMF_SINGLE\_LOG\_FILE), or whether one file per process should
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
    
   if (!ESMC_LogNameValid(name) ) {
      printf("File name is already being used.\n");
      ESMC_LogExit();
   } 

   switch(numLogFile) {
    
   case ESMF_SINGLE_LOG_FILE:
       oneLogErrFile=ESMF_LOG_TRUE;
       strcpy(nameLogErrFile,name);
       break;
      
   case ESMF_MULT_LOG_FILE :
       oneLogErrFile=ESMF_LOG_FALSE;
       strcpy(nameLogErrFile,name);
       break;

   default:
     ESMC_LogExit();
  }
  if (oneLogErrFile == ESMF_LOG_FALSE) ESMC_LogFormName();
  unitNumber=ESMF_LOG_FORT_UNIT_NUMBER;
  FTN(esmf_logopenfortran)(&fortIsOpen,&unitNumber,nameLogErrFile);
  if (fortIsOpen == ESMF_LOG_FALSE) {
     printf("Could not open file.");
     ESMC_LogExit();
  }
  logErrFileFortran[numFileFort]=unitNumber;
}

//----------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogNameValid - checks to see if a name has been used before
//
// !INTERFACE:
bool ESMC_Log::ESMC_LogNameValid(
// !RETURN VALUE
//    none
// !ARGUMENTS
      char name[]           // name of file
      )
// !DESCRIPTION
//    Checks to see if a file of the name name has been opened by Log.
//    If it has the function returns a false value.  Note: this function
//    use a global array that all ESMC\_Log objects have access to.
{
  int i;
  for(i=0; i< numFileGlobal+numFileFortGlobal; i++)
    if (strcmp(name,listOfFileNames[i])  == 0) 
	return false;
  strcpy(listOfFileNames[i-1],name);
  return true;
}
//----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogInit - initializes a Log object
//
// !INTERFACE:
void ESMC_Log::ESMC_LogInit(
//
// !RETURN VALUE:
//    None
// !ARGUMENTS:
//     
    int verbosity, //set verbosity level
    int flush,      //sets flush level
    int haltOnError,
    int haltOnWarning
    )

// !DESCRIPTION
// ESMC_LogInit initializes the log object.  The following items are set:
//   verbosity (whether or not something is written out, flush (is output 
//   flushed?), whether or not the code halts on errors and warnings.  By
//   default, all output goes to standard out.  This can be changed by calling
//   the open file methods.
//EOP
// !REQUIREMENTS:  developer's guide for classes
//
//
{
   if (verbosity == ESMF_LOG_TRUE)
      verbose=ESMF_LOG_TRUE;
   else if (verbosity == ESMF_LOG_FALSE)
      verbose=ESMF_LOG_FALSE;
   else {
      printf("Verbosity must be set to ESMF_LOG_FALSE or ESMF_LOG_TRUE.\n");
      ESMC_LogExit();
   }
   if (flush == ESMF_LOG_TRUE)
      flush=ESMF_LOG_TRUE;
   else if (flush == ESMF_LOG_FALSE)
      flush=ESMF_LOG_FALSE;
   else {
      printf("Flush must be set to ESMF_LOG_FALSE or ESMF_LOG_TRUE.\n");
      ESMC_LogExit();
   }
   if (haltOnError == ESMF_LOG_TRUE)
      haltOnErr=ESMF_LOG_TRUE;
   else if (haltOnError == ESMF_LOG_FALSE)
      haltOnErr=ESMF_LOG_FALSE;
   else {
      printf("haltOnError must be set to ESMF_LOG_FALSE or ESMF_LOG_TRUE.\n");
      ESMC_LogExit();
   }
   if (haltOnWarning == ESMF_LOG_TRUE)
      haltOnWarn=ESMF_LOG_TRUE;
   else if (haltOnWarning == ESMF_LOG_FALSE)
      haltOnWarn=ESMF_LOG_FALSE;
   else {
      printf("haltOnWarning must be set to ESMF_LOG_FALSE or ESMF_LOG_TRUE.\n");
      ESMC_LogExit();
   }
   numFilePtr=numFileGlobal;
   numFileGlobal++;
   numFileFort=numFileFortGlobal;
   numFileFortGlobal++;
   logErrFilePtr[numFilePtr]=stdout;
   logErrFileFortran[numFileFort]=ESMF_LOG_FORT_STDOUT;
   standardOut=ESMF_LOG_TRUE;
   fortIsOpen=ESMF_LOG_FALSE;
   oneLogErrFile=ESMF_LOG_TRUE; 
   unitNumber=ESMF_LOG_FORT_STDOUT;
   nameLogErrFile[0]=NULL;

 } // end ESMC_LogErrInit
//-----------------------------------------------------------------------------
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
// ESMC\_Log\_Info works similar to C's printf statement.
// The first argument is a character string that can include text to be
// written as well as a description of the number and kind of characters
// to be printed out.
// The format of this string follows the C format description convention,
// though not every feature is supported.  The current conversion specifiers
// are supported: d (signed decimal integer), f (double values),
// and s (string).
//
// Any number of data items may be passed to ESMC\_Log\_Print.
// 
// The items are printed on a single line.  Widths, precision, and
// escape sequences are not supported.  If you specify these, the code
// ignores them.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n
//


{

 if (verbose == ESMF_LOG_FALSE) return;
 int fortIO=ESMF_LOG_FALSE;
 ESMC_LogPrintHeader(fortIO);
 char* chPtr;
 va_list argp;

 va_start(argp,fmt);
 for (chPtr=fmt; *chPtr; chPtr++) {
   if ( *chPtr != '%') {
    fprintf(logErrFilePtr[numFilePtr],"%c",*chPtr);
    if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
    continue;
   }
   chPtr++;
   while (*chPtr == '-' || isdigit(*chPtr) || *chPtr == '.' ||
                 *chPtr == 'h' || *chPtr=='l') chPtr++;
   switch (*chPtr) {
     case 'c':
      fprintf(logErrFilePtr[numFilePtr],"%c ",va_arg(argp, char));
      if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
      break;
     case 's':
      fprintf(logErrFilePtr[numFilePtr],"%s ",va_arg(argp, char*));
      if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
      break;
     case 'd':
      fprintf(logErrFilePtr[numFilePtr],"%d ",va_arg(argp, int));
      if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
      break;
     case 'f':
      fprintf(logErrFilePtr[numFilePtr],"%f ",va_arg(argp, double));
      if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
      break;
     default:
      printf("Error in character descriptor.");
      if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
      ESMC_LogExit();
    }
  }
  fprintf(logErrFilePtr[numFilePtr],"\n");
  fprintf(logErrFilePtr[numFilePtr],"\n");
  if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
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
// ESMC\_LogInfoFortran is the version of LogInfo used for the Fortran
// interface.  Instead of printing the data from the stack as ESMC\_LogInfo 
// does, ESMC\_LogInfoFortran prints the data stored in the "Data" arrays. 
// The routine is called from esmf\_logprint which is  defined in
// ESMF\_LogInterface.C.  esmf\_logprint is callable from the user's
// from Fortran code.  It is this routine that takes the data
// from the stack and stores it in the "Data" arrays.
//
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n
//

{


 if (verbose == ESMF_LOG_FALSE) return;
 int fortIO=ESMF_LOG_TRUE;
 ESMC_LogPrintHeader(fortIO);
 int numStr=0;
 char* chPtr;
 int* intPtr;
 char msg[72]; 
 int len=0;
 int len2;

 for (chPtr=fmt; *chPtr; chPtr++)
  {
    if (*chPtr != '%') {
      FTN(esmf_logprintchar)(&unitNumber,chPtr,&flush,"",&len);
   } else {
   chPtr++;
   while (*chPtr == '-' || isdigit(*chPtr) || *chPtr == '.' ||
                 *chPtr == 'h' || *chPtr=='l') chPtr++;
   switch (*chPtr) {
     case 'c':
      FTN(esmf_logprintchar)(&unitNumber,charData,&flush,"",&len);
      charData++;
      break;
     case 's':
      strcpy(msg,strData[numStr]);
      len2=strlen(msg);
      FTN(esmf_logprintstring)(&unitNumber,msg,&len2,&flush,"",&len);
      numStr++;
      break;
     case 'd':
      intPtr=intData;
      FTN(esmf_logprintint)(&unitNumber,intPtr,&flush,"",&len);
      intData++;
      break;
     case 'f':
      FTN(esmf_logprintreal)(&unitNumber,floatData,&flush,"",&len);
      floatData++;
      break;
     default:
      ESMC_LogExit();
    }
   }
  }
 FTN(esmf_logprintnewline)(&unitNumber,&flush);
 FTN(esmf_logprintnewline)(&unitNumber,&flush);
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
     int fortIO       //if set to ESMF_LOG_TRUE, use fortran io libraries

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
  time(&timeVal);
  timePtr=localtime(&timeVal);
  timeAsc=asctime(timePtr);
  int len1=strlen(timeAsc);
  int len2=0;
  if (fortIO == ESMF_LOG_TRUE) { 
   FTN(esmf_logprintstring)(&unitNumber,timeAsc,&len1,&flush,"",&len2);
  } else {
    fprintf(logErrFilePtr[numFilePtr],"%s",timeAsc);
    if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
  } 
#ifdef HAS_MPI
  rank=MPI::COMM_WORLD.Get_rank();
  len2=4;
  if (fortIO == ESMF_LOG_TRUE) { 
   FTN(esmf_logprintint)(&unitNumber,&rank,&flush,"PE: ",&len2);
  } else {
    fprintf(logErrFilePtr[numFilePtr],"PE: %d",rank);
    if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
  } 
#endif
}
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogWrite - used in conjunction with standard Fortran write
// to write data to log file. 
//
// !INTERFACE:
int ESMC_Log::ESMC_LogWrite(
//
// !RETURN VALUE:
//    unit number 
//
// !ARGUMENTS:
//     none.

   )
//
// !Description:
// This is method is used in conjunction with esmf\_logwrite (defined in
// ESMC\_LogInterface.C) to write data to the log file.  This routine
// is used with the standard Fortran write function, eg.
// write(logwrite(aLog),*) 'Hi '. 
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

  int fortIO=ESMF_LOG_TRUE;
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
// !Description:
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
// !IROUTINE: ESMC_LogCloseFileForWrite - closes log file. 
//
// !INTERFACE:
void ESMC_Log::ESMC_LogCloseFileForWrite(
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
  if (standardOut == ESMF_LOG_FALSE) {
    if (fortIsOpen==ESMF_LOG_TRUE) {
      FTN(esmf_logclosefortran)(&unitNumber);
      for( i=0; i<numFileFortGlobal; i++)
        if (strcmp(nameLogErrFile,listOfFortFileNames[i])  == 0) {
         for(j=i+1;j+numFileFortGlobal; j++)
          strcpy(listOfFortFileNames[j-1],listOfFortFileNames[j]);
         for(j=i+1;j+numFileFortGlobal; j++)
	  logErrFileFortran[j-1]=logErrFileFortran[j]; 
         numFileFortGlobal--;
         break;
        }
    }
  }
}
//-----------------------------------------------------------------------
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

   if (standardOut == ESMF_LOG_FALSE) {
     if (logErrFilePtr[numFilePtr] != NULL) {
       fclose(logErrFilePtr[numFilePtr]);
     }
    for( i=0; i< numFileGlobal; i++)
      if (strcmp(nameLogErrFile,listOfFileNames[i])  == 0) {
       for(j=i+1;j<numFileGlobal; j++)
          strcpy(listOfFileNames[j-1],listOfFileNames[j]);
       for(j=i+1;j<numFileGlobal; j++)
	  logErrFilePtr[j-1]=logErrFilePtr[j];
       numFileGlobal--;
       break;
      } 
   }
}


 
//----------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_Log - native C++ constructor for log class
//
// !INTERFACE:

ESMC_Log::ESMC_Log(

// !RETURN VALUE:
//  none
//
// !ARGUMENTS:
//  none

  )

// !DESCRIPTION:
//   Native C++ constructor
//EOP


{
 nameLogErrFile[0]=NULL;
 oneLogErrFile=0;
 standardOut=ESMF_LOG_TRUE;
 verbose=ESMF_LOG_TRUE; 
 fortIsOpen=ESMF_LOG_FALSE;
 numFilePtr=numFileGlobal;
 unitNumber=50;
 flush=ESMF_LOG_FALSE;
}

//----------------------------------------------------------------------
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

   if (verbose == ESMF_LOG_FALSE) return;
   int fortIO=ESMF_LOG_FALSE;
   char msg[1];
   msg[0]=NULL;
   ESMC_LogPrintHeader(fortIO);
   ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);

   if (haltOnErr==ESMF_LOG_TRUE) {
    ESMC_LogCloseFile();
    ESMC_LogExit();
   }
 }


//----------------------------------------------------------------------
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

   if (verbose == ESMF_LOG_FALSE) return;
   int fortIO=ESMF_LOG_FALSE;
   ESMC_LogPrintHeader(fortIO);
   ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);

   if (haltOnErr==ESMF_LOG_TRUE) {
    ESMC_LogCloseFile();
    ESMC_LogExit();
   }
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
//  Same as ESMC\_LogErr, except execution is not stopped after
//  printing message, except when haltOnWarn set to true
//
//EOP
{ 

 if (verbose == ESMF_LOG_FALSE) return;
 int fortIO=ESMF_LOG_FALSE;
 char msg[1];
 msg[0]=NULL;
 ESMC_LogPrintHeader(fortIO);
 ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);

 if (haltOnWarn == ESMF_LOG_TRUE) {
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
//  Same as ESMC\_LogErr, except execution is not stopped after
//  printing message, except when haltOnWarn set to true
//
//EOP
{ 

 if (verbose==ESMF_LOG_FALSE) return;
 int fortIO=ESMF_LOG_FALSE;
 ESMC_LogPrintHeader(fortIO);
 ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);

 if (haltOnWarn == ESMF_LOG_TRUE) {
    ESMC_LogCloseFile();
    ESMC_LogExit();
 }

}

//---------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogExit() - private routine uses by Log to stop program
//                             execution
// !INTERFACE:

void ESMC_Log::ESMC_LogExit(

// !ARGUMENTS:
//  none

  )

// !DESCRIPTION:
// Used by Log to exit program.
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

//------------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogErrFortran - called by fortran wrapper to write error
//                                 message
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
//Similar to LogErr, except called by the fortran wrapper
//esmf\_logerr which is defined in
//ESMC\_Interface.C.  The major difference between this routine
//and LogErr is that this
//routine prints the printf style data from the Data arrays not the stack.
//EOP

{

  if (verbose == ESMF_LOG_FALSE) return;
  int fortIO=ESMF_LOG_TRUE;
  ESMC_LogPrintHeader(fortIO);
  ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);


  if (haltOnErr == ESMF_LOG_TRUE) {
    ESMC_LogCloseFileForWrite();
    ESMC_LogExit();
  }
    

}



//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_LogWarnFortran - called by fortran wrapper
// to write warnings
//
// !INTERFACE:

void ESMC_Log::ESMC_LogWarnFortran(

// !ARGUMENTS:

     int errCode,
     int line,
     char file[],
     char dir[],
     char msg[]

)
// !DESCRIPTION:
// Similar to ESMC\_LogWarn, except called by the fortran
// wrapper esmf\_logerr which is
// defined in ESMC\_Interface.C
//EOP

{
 if (verbose == ESMF_LOG_FALSE) return;
 int fortIO=ESMF_LOG_TRUE;
 ESMC_LogPrintHeader(fortIO);
 ESMC_LogPrint(fortIO,errCode,line,file,dir,msg);

 if (haltOnWarn == ESMF_LOG_TRUE) {
    ESMC_LogCloseFileForWrite();
    ESMC_LogExit();
 }

}

//------------------------------------------------------------------------------
//BOP
//
// !IROUTINE: ESMC_LogPrint - prints to the log file
// 
// !INTERFACE:

void ESMC_Log:: ESMC_LogPrint(

// !ARGUMENTS:

   int fortIO,          //if set to ESMF_LOG_TRUE use Fortran IO libraries
   int errCode,
   int line,            // see LogErr for a definition
   char file[],         // of these variables
   char dir[],           
   char msg[]            // optional msg

  )

// !DESCRIPTION: This is a private routine, used by many methods of
// ESMC\_Log to print data to the log file. If used from Fortran, then
// Fortran I/O libraries are used.  Otherwise C I/O libraries are used.
//EOP
{
 char errMsg[32];
 int len1,len2;
 ESMC_LogGetErrMsg(errCode,errMsg);
 if (fortIO == ESMF_LOG_TRUE) {
  len1=12;
  FTN(esmf_logprintint)(&unitNumber,&errCode,&flush,"Error Code: ",&len1);
  FTN(esmf_logprintnewline)(&unitNumber,&flush);
  len1=0;
  len2=strlen(errMsg);
  FTN(esmf_logprintstring)(&unitNumber,errMsg,&len2,&flush,"",&len1);
  FTN(esmf_logprintnewline)(&unitNumber,&flush);
  len1=11;
  len2=strlen(dir);
  FTN(esmf_logprintstring)(&unitNumber,dir,&len2,&flush,"Directory: ",&len1);
  FTN(esmf_logprintnewline)(&unitNumber,&flush);
  len1=6;
  len2=strlen(file);
  FTN(esmf_logprintstring)(&unitNumber,file,&len2,&flush,"File: ",&len1);
  FTN(esmf_logprintnewline)(&unitNumber,&flush);
  len1=7;
  FTN(esmf_logprintint)(&unitNumber,&line,&flush,"Line: ",&len1);
  FTN(esmf_logprintnewline)(&unitNumber,&flush);
  if (msg[0] != NULL) {
     len1=10;
     len2=strlen(msg);
     FTN(esmf_logprintstring)(&unitNumber,msg, &len2,&flush,"Comments: ",&len1);
     FTN(esmf_logprintnewline)(&unitNumber,&flush);
  }
  FTN(esmf_logprintnewline)(&unitNumber,&flush);
  FTN(esmf_logprintnewline)(&unitNumber,&flush);
 } else {
  fprintf(logErrFilePtr[numFilePtr],"Error Code: %d\n",errCode);
  if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
  fprintf(logErrFilePtr[numFilePtr],"%s\n",errMsg);
  if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
  fprintf(logErrFilePtr[numFilePtr],"Directory: %s \n",dir);
  if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
  fprintf(logErrFilePtr[numFilePtr],"File: %s\n",file);
  if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
  fprintf(logErrFilePtr[numFilePtr],"Line: %d\n",line);
  if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
  if (msg[0] != NULL) {
    fprintf(logErrFilePtr[numFilePtr],"Comments: %s\n",msg);
    if (flush==ESMF_LOG_TRUE) fflush(logErrFilePtr[numFilePtr]);
  }
  fprintf(logErrFilePtr[numFilePtr],"\n");
  fprintf(logErrFilePtr[numFilePtr],"\n");
 }
}
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LogGetErrMsg -- Return error message for given error code.
//
// !INTERFACE:
      void ESMC_Log::ESMC_LogGetErrMsg(

//
//
// !ARGUMENTS:
//
     int errCode,
     char msg[]
     ) const
//
// !DESCRIPTION:
// GetErrMsg returns a string corresponding to the error code
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
           strcpy(msg,"Unknown error code.");
           printf("Unknown error code.\n");
           exit(EXIT_FAILURE);
 }     
} // end ESMC_GetErrMsg
