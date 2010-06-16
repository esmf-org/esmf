// $Id: ESMC_Calendar.h,v 1.70 2010/06/16 17:05:09 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
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

// TODO: these definitions need different home (shared with ESMCI_Calendar)
#define CALENDAR_TYPE_COUNT 8
enum ESMC_CalendarType {ESMC_CAL_GREGORIAN=1,
                        ESMC_CAL_JULIAN,
                        ESMC_CAL_JULIANDAY,   
                        ESMC_CAL_MODJULIANDAY,   
                        ESMC_CAL_NOLEAP,      // like Gregorian, except
                                              //   Feb always has 28 days
                        ESMC_CAL_360DAY,      // 12 months, 30 days each
                        ESMC_CAL_CUSTOM,      // user defined
                        ESMC_CAL_NOCALENDAR}; // track base time seconds
                                              //   only
                        // Note: add new calendars between ESMC_CAL_GREGORIAN
                        // and ESMC_CAL_NOCALENDAR so
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
  const char *name, 
  enum ESMC_CalendarType calendartype,
  int *rc);  

// !DESCRIPTION:
//
//  Creates and sets a {\tt calendar} to the given built-in
//  {\tt ESMC\_CalendarType}. 
//
//  The arguments are:
//  \begin{description}
//  \item[{[name]}]
//    The name for the newly created calendar.  If not specified, i.e. NULL,
//    a default unique name will be generated: "CalendarNNN" where NNN
//    is a unique sequence number from 001 to 999.
//  \item[calendartype]
//    The built-in {\tt ESMC\_CalendarType}.  Valid values are:
//    {\tt ESMC\_CAL\_360DAY}, {\tt ESMC\_CAL\_GREGORIAN},
//    {\tt ESMC\_CAL\_JULIAN}, {\tt ESMC\_CAL\_JULIANDAY},
//    {\tt ESMC\_CAL\_MODJULIANDAY}, {\tt ESMC\_CAL\_NOCALENDAR},
//    and {\tt ESMC\_CAL\_NOLEAP}.
//    See Section ~\ref{subsec:Calendar_options} for a description of each
//    calendar type.
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
  ESMC_Calendar *calendar);

// !DESCRIPTION:
//
//  Releases all resources associated with this {\tt ESMC\_Calendar}.
//
//  The arguments are:
//  \begin{description}
//  \item[calendar]
//    Destroy contents of this {\tt ESMC\_Calendar}.
//  \item[[rc]]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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
  ESMC_Calendar calendar);

// !DESCRIPTION:
//  Prints out an {\tt ESMC\_Calendar}'s properties to {\tt stdio}, 
//  in support of testing and debugging.
//
//  The arguments are:
//  \begin{description}
//  \item[calendar]
//    {\tt ESMC\_Calendar} to be printed out.
//  \item[[rc]]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------


#ifdef __cplusplus
} // extern "C"
#endif

#endif // ESMC_Calendar_H
