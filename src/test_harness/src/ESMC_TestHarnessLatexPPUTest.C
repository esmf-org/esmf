// LatexPostProcess.cpp : Defines the entry point for the console application.
//

//#include "stdafx.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ESMC.h"

// size of maximum input line
#define MAX_LINE 1024

//static const char InFname  [] = "Dummy.tex";
//static const char OutFname [] = "XDummy.tex";

void PostProcessFile (FILE * InFile, FILE * OutFile);
bool ReadLine (FILE * InFile, int InLineCnt, char * InLine);
void PostProcessLine (FILE * OutFile, char * InLine);
void MakeSubs (FILE * OutFile, char * InLine);

int main(int argc, char * argv[])
{
	FILE * InFile;
	FILE * OutFile;

	if (argc < 3)
	{
	  printf ("\nmissing filenames on command line");
	  exit (-1);
	}

        // Dummy call to ESMF lib to make linking in some environments
        // (e.g., NAG v5.3) work in the ESMF build environment.  (In the
        // case of NAG v5.3, it avoids unsatisfied libm calls from the
        // NAG libf53.so.)
        if (argc < 0) {
          int localrc;
          ESMC_Initialize (&localrc);
          ESMC_Finalize ();
        }

	// open output files
	InFile = fopen (argv[1], "rb");
	if (InFile == NULL)
	{
		printf ("\nUnable to open input file - Filename = %s", argv[1]);
		exit (-1);
	}

	OutFile = fopen (argv[2], "wb");
	if (OutFile == NULL)
	{
		printf ("\nUnable to open output file - Filename = %s", argv[2]);
		exit (-1);
	}

	// post process file
	PostProcessFile (InFile, OutFile);

	// close output files
	fclose (InFile);
	fclose (OutFile);

	return 0;
}

void PostProcessFile (FILE * InFile, FILE * OutFile)
{
	char InLine  [MAX_LINE];

	while (ReadLine (InFile, MAX_LINE, InLine))
	{
		PostProcessLine (OutFile, InLine);
	}
}

// state machine to skip html declaration at front of file
static const int PPLState_SkipFront1 = 0;
static const int PPLState_SkipFront2 = 1;
static const int PPLState_MakeSubs   = 2;
static int PPLState = PPLState_SkipFront1;

void PostProcessLine (FILE * OutFile, char * InLine)
{
	switch (PPLState)
	{
	case PPLState_SkipFront1:
		PPLState = PPLState_SkipFront2;
		break;

	case PPLState_SkipFront2:
		PPLState = PPLState_MakeSubs;
		break;

	case PPLState_MakeSubs:
		PPLState = PPLState_MakeSubs;
		MakeSubs (OutFile, InLine);
		break;
	}
}

//static 	char OutLine [MAX_LINE];

// copy inline to outline making substitutions, write out finished line
void MakeSubs (FILE * OutFile, char * InLine)
{
	char OutLine [MAX_LINE];
	int    OutPos = 0;
	char   OutChar;
	int    InPos = 0;
	bool   more = true;

	while (more)
	{
		if (OutPos >= MAX_LINE)
		{
			more = false;
		} else {
			OutChar = InLine[InPos];
			InPos ++;

			switch (OutChar)
			{
				case '_':
					// append backslash character
					OutLine[OutPos] = '\\';
					OutPos ++;

					OutLine[OutPos] = '_';
					OutPos ++;
					break;

				case '&':   // escape sequence
					if (strncmp (&InLine[InPos], "amp;", 4) == 0)
					{
						OutLine[OutPos] = '&';
						OutPos ++;

						InPos += 4;  // skip escape chars
					} else if (strncmp (&InLine[InPos], "gt;", 3) == 0)
					{
						// insert gt sign
						OutLine[OutPos] = '>';
						OutPos ++;

						// skip escape sequence
						InPos += 3;
					} else { // unknown escape sequence
						// insert ?
						OutLine[OutPos] = '?';
						OutPos ++;
					}
					break;

				case '/':   // long path names
					OutLine[OutPos] = '/';
					OutPos ++;
										
					// allow hypen after slash
					OutLine[OutPos] = '\\';
					OutPos ++;

					OutLine[OutPos] = '-';
					OutPos ++;
					break;

				default:
					// append character
					OutLine[OutPos] = OutChar;
					OutPos ++;
					break;
			}
		}
	}

	// insert terminator at end of line
	if (OutPos < MAX_LINE)
	{
		OutLine[OutPos] = 0;
	} else {
		OutLine[MAX_LINE-1] = 0;
	}


	// copy until & or _
	fprintf (OutFile, "%s\n", OutLine);
}


// read line, truncate with null, don't insert line terminator, skip any 0x0d characters
// 
bool ReadLine (FILE * InFile, int InLineMax, char * InLine)
{
	int     InChar;
	int     InLineCnt = 0;
	bool    More = true;
	bool    EofFlag;

	EofFlag = (feof (InFile) != 0);

	if (!EofFlag)
	{
		while (More)
		{
			InChar = fgetc (InFile);

			switch (InChar)
			{
			case -1:    // end line
				More = false;
				break;

			case 0x0a:  // end line
				More = false;
				break;

			case 0x0d:  // skip char
				break;

			default:
				// insert char into buffer
				if (InLineCnt < InLineMax)
				{
					InLine[InLineCnt] = (char)InChar;
					InLineCnt ++;
				}

				break;
			}
		}
	}

	if (InLineCnt < InLineMax)
	{
		InLine[InLineCnt] = 0;
	} else {
		InLine[InLineMax-1] = 0;
	}

	return !EofFlag;
}

void PostProcessLine ()
{

}

