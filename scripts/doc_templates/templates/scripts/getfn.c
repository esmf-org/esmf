/* $Id: getfn.c,v 1.1 2002/10/07 16:37:13 eschwab Exp $ */

#include <stdio.h>
#include <string.h>

/* print out filename given full pathname */
int main(int argc, char **argv)
{
	printf("%s", strrchr(argv[1], '/') + 1);
}
