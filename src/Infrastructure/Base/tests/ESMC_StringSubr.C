
#include <stdio.h>
#include <string.h>

typedef void (*FUNC)(int *, int *, int *, int *);
typedef void (*FUNC2)(int *, int *, char *, int *, int *, int);
extern FUNC f90string;
extern FUNC2 f90string2;

extern "C" {

      void c_strings(FUNC f90cb, FUNC2 f90cb2, int *i1, int *i2, 
                                    char *fstr, int *i3, int *i4, int slen) {

      int ni1, ni2, ni3, ni4;
      int clen;
      static char *newstr = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      char tbuf[8192];


      printf("\n\n-- entering c_strings\n");

      strncpy(tbuf, fstr, slen);
      tbuf[slen] = '\0';
      printf(" %d %d %d %d %d\n", *i1, *i2, *i3, *i4, slen);
      printf(" strlen = %d, str='%s'\n", slen, tbuf);

      ni1 = 199;
      ni2 = 299;
      ni3 = 399;
      ni4 = 499;

  
      printf("\n\n-- ready to call f90string by callback\n");
      (*f90cb)(&ni1, &ni2, &ni3, &ni4);
      printf("\n\n-- returned from call of f90string by calback\n");

      clen = 15;

      printf("\n\n-- ready to call f90string2 by callback, passing in 15\n");
      (*f90cb2)(&ni1, &ni2, newstr, &ni3, &ni4, clen);
      printf("\n\n-- returned from call f90string2 by callback\n");

      clen = 25;

      printf("\n\n-- ready to call f90string2 by callback, passing in 25\n");
      (*f90cb2)(&ni1, &ni2, newstr, &ni3, &ni4, clen);
      printf("\n\n-- returned from call f90string2 by callback\n");

      printf("\n\n-- leaving c_strings\n");
   }

}




