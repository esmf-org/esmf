// $Id: ESMCI_Util_F.C,v 1.7.4.1 2012/01/06 21:46:28 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMF_FILENAME "ESMCI_Util_F.C"
//==============================================================================

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the Fortran callable 
// interfaces to the C++ Util methods.
//
//-----------------------------------------------------------------------------
//

//insert any higher level, 3rd party or system includes here
#if defined (MAPDEBUG)
#include <iostream>
#endif
#include <map>
#include <string>
#include <cstring>
#include <cstdio>
using namespace std;

// associated class definition file and others
#include "ESMCI_Macros.h"
#include "ESMCI_Util.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_Util_F.C,v 1.7.4.1 2012/01/06 21:46:28 svasquez Exp $";
//-----------------------------------------------------------------------------


// MapName container type for name/int pairs
typedef struct {
  std::map<std::string, int> table;
} MapName;


extern "C" {

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Map container routines
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// MapName routines allow Fortran callable management of STL map containers
// containing string/int pairs.
 
void FTN(c_esmc_mapname_add) (MapName **ptr,
                            char *name, // in - name to be entered
                            int *value, // in - associated value
                            int *rc,    // out - return code
                            ESMCI_FortranStrLenArg name_len) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_mapname_add"

        ESMF_CHECK_POINTER(ptr, rc)

        string cname (name, name_len);
#if defined (MAPDEBUG)
        cout << ESMC_METHOD << ": cname = " << cname;
        cout << ", value = " << *value << endl;
//        cout << "    nameTable map address = " << hex << *ptr << endl;
#endif
        (*ptr) -> table[cname] = *value;
#if defined (MAPDEBUG)
        cout << "    value lookup returned: " << (*ptr) -> table[cname] << endl;
#endif
        *rc = ESMF_SUCCESS;

}

void FTN(c_esmc_mapname_create) (MapName **ptr,
                            int *rc     // out - return code
                            ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_mapname_create"

        ESMF_CHECK_POINTER(ptr, rc)

#if defined (MAPDEBUG)
        cout << ESMC_METHOD << ": entered" << endl;
#endif
        *ptr = new MapName;
        *rc = *ptr ? ESMF_SUCCESS : ESMF_FAILURE;
#if defined (MAPDEBUG)
        cout << "    nameTable map address = " << hex << *ptr << endl;
#endif

}

void FTN(c_esmc_mapname_destroy) (MapName **ptr,
                            int *rc     // out - return code
                            ) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_mapname_destroy"

        ESMF_CHECK_POINTER(ptr, rc)

#if defined (MAPDEBUG)
        cout << ESMC_METHOD << ": entered" << endl;
#endif
        delete *ptr;
        *rc = ESMF_SUCCESS;

}

void FTN(c_esmc_mapname_lookup) (MapName **ptr,
                            char *name, // in - name to be looked up
                            int *value, // out - associated value
                            ESMC_Logical *foundflag, // out - true if name was found 
                            int *rc,    // out - return code
                            ESMCI_FortranStrLenArg name_len) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_mapname_lookup"

        ESMF_CHECK_POINTER(ptr, rc)

#if defined (MAPDEBUG)
        cout << ESMC_METHOD << ": entered" << endl;
#endif
        string cname (name, name_len);
        std::map<std::string, int>::iterator pos;
        pos = (*ptr)->table.find (cname);

        bool found = pos != (*ptr)->table.end ();
        if (found)
          *value = pos->second;
        else
          *value = -1;
        *foundflag = found ? ESMF_TRUE : ESMF_FALSE;
#if defined (MAPDEBUG)
        cout << "    name: " << cname << ", value returned = " << *value;
        cout << ", found = " << found << endl;
#endif
        *rc = ESMF_SUCCESS;
}

void FTN(c_esmc_mapname_print) (MapName **ptr,
                            char *title, // in - title for printout
                            int *rc,     // out - return code
                            ESMCI_FortranStrLenArg title_len) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_mapname_print"

        ESMF_CHECK_POINTER(ptr, rc)

	fprintf (stdout, "%s\n", string(title, title_len).c_str ());
	fprintf (stdout, "%s\n", string(title_len, '-').c_str ());
#if defined (MAPDEBUG)
        cout << " map size = " << (*ptr)->table.size () << endl;
#endif

        std::map<std::string, int>::iterator pos;
        int i=1;
	for (pos=(*ptr)->table.begin (); pos != (*ptr)->table.end (); ++pos)
	  fprintf (stdout, " %i: %s, %i\n",
             i++, pos->first.c_str (), pos->second);
        *rc = ESMF_SUCCESS;
}

void FTN(c_esmc_mapname_remove) (MapName **ptr,
                            char *name, // in - name to be entered
                            int *rc,    // out - return code
                            ESMCI_FortranStrLenArg name_len) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_mapname_remove"

        ESMF_CHECK_POINTER(ptr, rc)

#if defined (MAPDEBUG)
        cout << ESMC_METHOD << ": entered" << endl;
#endif
        string cname (name, name_len);
        (*ptr)->table.erase (cname);
        *rc = ESMF_SUCCESS;
}

void FTN(c_esmc_mapname_sizeget) (MapName **ptr,
                            int *size,  // out - # of items in the map
                            int *rc) {  // out - return code
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_mapname_sizeget"

        ESMF_CHECK_POINTER(ptr, rc)

#if defined (MAPDEBUG)
        cout << ESMC_METHOD << ": entered" << endl;
#endif
        *size = (*ptr)->table.size ();
        *rc = ESMF_SUCCESS;
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// String routines
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_StringSerialize - Serialize String object 
//
// !INTERFACE:
      void FTN(c_esmc_stringserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      char *string,             // in/out - string object
      char *buf,                // in/out - really a byte stream
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      ESMC_InquireFlag *inquireflag, // in - inquire flag
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg clen) { // in, hidden - string length
// 
// !DESCRIPTION:
//     Serialize the contents of a string object.
//
//EOPI

  char *cp;

  if (!string) {
    //printf("uninitialized String object\n");
    ESMC_LogDefault.Write("String object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }

  int fixedpart = clen + 1;
  if (*inquireflag != ESMF_INQUIREONLY) {
    if ((*length - *offset) < fixedpart) {
         
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                             "Buffer too short to add a String object", rc);
       return;
 
      //buffer = (char *)realloc((void *)buffer,
      //                         *length + 2*fixedpart + byte_count);
      //*length += 2 * fixedpart;
    }
  }

  cp = buf + *offset;
  if (*inquireflag != ESMF_INQUIREONLY)
    memcpy(cp, string, clen);
  cp += clen;
  
  *offset = cp - buf;

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_StringSerialize


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_StringDeserialize - Deserialize String object 
//
// !INTERFACE:
      void FTN(c_esmc_stringdeserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      char *string,             // in/out - string object
      char *buf,                // in/out - really a byte stream
      int *offset,              // in/out - current offset in the stream
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg clen) { // in, hidden - string length
// 
// !DESCRIPTION:
//     Deserialize the contents of a base object.
//
//EOPI

  char *cp;

  cp = buf + *offset;
  memcpy(string, cp, clen);
  cp += clen;
  
  *offset = cp - buf;

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_StringDeserialize




//-----------------------------------------------------------------------------

void FTN(c_pointerprint)(void **ptr){
  printf("ESMF_PointerPrint: %p\n", *ptr);
}

} // extern "C"
