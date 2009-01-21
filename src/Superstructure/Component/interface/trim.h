// $Id: trim.h,v 1.3.2.2 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//
// this is in a separate file so it can be both static and included
// in the two files which need it.
//
//==============================================================================
//
// this trim routine does several things:
//
// most importantly, it null terminates a counted-char string passed in
// from fortran.  it's not guarenteed you can write into the N+1th
// character location (if the string is full length in fortran, for example)
// so we're forced to make a copy and copy into it.  this routine allocates,
// so the char string created here MUST be deleted by the caller when finished.
//
// secondly, the phase number (init 1, init 2, etc) is passed in as an int.
// if > 0 this routine turns it into a 2-char string filled with
// leading 0s and tacks it onto the end of the name to make it unique.
//
// and finally, component routines can be called with either a single
// state or a pair of states. we can require that the user specify the
// interface at registration time, or we can decide at run time which
// form was called and dispatch the corresponding entry point.
// for now i'm going to fill both types of component entry points for each
// registration.  i can always remove that code if we force the user
// to specify at registration time what format the states are expected in.
// so if nstate > 0, it gets the same treatment as phase: it's turned into
// a single char string and tacked on the end.
//
// (we'd also at some point like to be able to pass back into fortran
// a really type(State) F90 array - but the bytes on the stack are 
// compiler dependent - we'd have to create the array in fortran and
// save a copy of it to be safe.  that code is *NOT* implemented at this
// point, but i know it would sure seem natural from the user's viewpoint.)
// 
//

// this is max of 2 char phase + 'P' + 1 char nstate + 'S' + trailing NULL
#define MAXPAD 8

static void newtrim(char *c, int clen, int *phase, int *nstate, char **newc) {
     char *cp, *ctmp;
     int hasphase = 0;
     int hasstate = 0;
     char tspace[MAXPAD];
     int pad=2;         // if neither phase nor nstate, still need term NULL

     //printf("in newtrim, c = '%s', clen = %d\n", c, clen);

     // warning - on the intel compiler, optional args come in
     // as -1, not 0.  check for both before dereferencing.
     if ((phase != NULL) && (phase != (int *)-1) && (*phase > 0))  {
         pad = MAXPAD;
         hasphase++;
     }

     // warning - on the intel compiler, optional args come in
     // as -1, not 0.  check for both before dereferencing.
     // if state > 0, use it to alter the EP name.
     if ((nstate != NULL) && (nstate != (int *)-1) && (*nstate > 0))  {
         pad = MAXPAD;
         hasstate++;
     }

     // make new space and leave room for at least a null terminator, more
     // if it has either phase or num states or both.
     ctmp = new char[clen+pad];
     strncpy(ctmp, c, clen);
     ctmp[clen] = '\0';
     for (cp = &ctmp[clen-1]; *cp == ' '; cp--)   // trim() trailing blanks
         *cp = '\0';
  
     // tack on trailing numbers if phase or nstate
     if (hasphase && hasstate) {
         sprintf(tspace, "%02dP%1dS", *phase, *nstate);
         strcat(ctmp, tspace);
     } else if (hasphase) {
         sprintf(tspace, "%02dP", *phase);
         strcat(ctmp, tspace);
     } else if (hasstate) {
         sprintf(tspace, "%1dS", *nstate);
         strcat(ctmp, tspace);
     }

     // set return pointer.  caller MUST free this when finished with it.
     *newc = ctmp;
     //printf("out newtrim, newc = '%s'\n", *newc);
     return;

}

