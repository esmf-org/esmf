// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Calendar_H
#define ESMC_Calendar_H

//-----------------------------------------------------------------------------
// ESMC_Calendar - Public C interface to the ESMF Calendar class
//
// The code in this file defines the public C Calendar interfaces and declares
// method signatures (prototypes).  The companion file {\tt ESMC\_Calendar.C}
// contains the definitions (full code bodies) for the Calendar methods.
//-----------------------------------------------------------------------------

// TODO: these definitions need different home (shared with ESMCI_Calendar)
#define CALENDAR_KIND_COUNT 8
enum ESMC_CalKind_Flag {ESMC_CALKIND_GREGORIAN=1,
                        ESMC_CALKIND_JULIAN,
                        ESMC_CALKIND_JULIANDAY,   
                        ESMC_CALKIND_MODJULIANDAY,   
                        ESMC_CALKIND_NOLEAP,      // like Gregorian, except
                                                  //   Feb always has 28 days
                        ESMC_CALKIND_360DAY,      // 12 months, 30 days each
                        ESMC_CALKIND_CUSTOM,      // user defined
                        ESMC_CALKIND_NOCALENDAR}; // track base time seconds
                                                  //   only
                        // Note: add new calendars between 
                        // ESMC_CALKIND_GREGORIAN and ESMC_CALKIND_NOCALENDAR so
                        // ESMCI::Calendar::validate() does not need
                        // to change.  Also add to static intializers at top
                        // of ESMCI_Calendar.C

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
//-----------------------------------------------------------------------------
typedef struct {
  // private:  // members opaque on C side, philosophically.
    void *ptr;
    // TODO:  implement isInit initialization like in F90 API?
} ESMC_Calendar;
//-----------------------------------------------------------------------------

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CalendarCreate - Create a Calendar
//
// !INTERFACE:
ESMC_Calendar ESMC_CalendarCreate(
  const char *name,                      // in
  enum ESMC_CalKind_Flag calkindflag,    // in
  int *rc                                // out
);

// !RETURN VALUE:
//  Newly created ESMC_Calendar object.
//
// !DESCRIPTION:
//
//  Creates and sets a {\tt ESMC\_Calendar} object to the given built-in
//  {\tt ESMC\_CalKind\_Flag}. 
//
//  The arguments are:
//  \begin{description}
//  \item[{[name]}]
//    The name for the newly created Calendar.  If not specified, i.e. NULL,
//    a default unique name will be generated: "CalendarNNN" where NNN
//    is a unique sequence number from 001 to 999.
//  \item[calkindflag]
//    The built-in {\tt ESMC\_CalKind\_Flag}.  Valid values are:
//    \newline
//    {\tt ESMC\_CALKIND\_360DAY}, 
//    \newline
//    {\tt ESMC\_CALKIND\_GREGORIAN},
//    \newline
//    {\tt ESMC\_CALKIND\_JULIAN}, 
//    \newline
//    {\tt ESMC\_CALKIND\_JULIANDAY},
//    \newline
//    {\tt ESMC\_CALKIND\_MODJULIANDAY}, 
//    \newline
//    {\tt ESMC\_CALKIND\_NOCALENDAR},
//    \newline
//    and {\tt ESMC\_CALKIND\_NOLEAP}.
//    \newline
//    See Section ~\ref{subsec:Calendar_options} for a description of each
//    calendar kind.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CalendarDestroy - Destroy a Calendar
//
// !INTERFACE:
int ESMC_CalendarDestroy(
  ESMC_Calendar *calendar   // inout
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Releases all resources associated with this {\tt ESMC\_Calendar}.
//
//  The arguments are:
//  \begin{description}
//  \item[calendar]
//    Destroy contents of this {\tt ESMC\_Calendar}.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_CalendarPrint - Print a Calendar
//
// !INTERFACE:
int ESMC_CalendarPrint(
  ESMC_Calendar calendar   // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//  Prints out an {\tt ESMC\_Calendar}'s properties to {\tt stdio}, 
//  in support of testing and debugging.
//
//  The arguments are:
//  \begin{description}
//  \item[calendar]
//    {\tt ESMC\_Calendar} object to be printed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ESMC_Calendar_H
