/* $Id: tolower.c,v 1.1 2002/10/07 16:36:44 eschwab Exp $ */

#include <stdio.h>
#include <ctype.h>
#include <string.h>

/* convert argv[1] to all lower case; output to stdout */
int main(int argc, char **argv)
{
	char buf[128];
	int i;

	for(i=0; i<strlen(argv[1]); i++)
	{
		buf[i] = tolower(argv[1][i]);
	}
	buf[i] = '\0';
	printf("%s", buf);
}
