#include <string.h>
#include <stdlib.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Trace.h"
#include "esmftrc_filesys.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

extern "C" {

  void FTN_X(c_esmftrc_filesys_init)
     (				   
      int *buf_size,  
      const char *trace_dir,           
      int *localPet,
      int *rc,                        
      ESMCI_FortranStrLenArg nlen) {   //strlen for trace_dir
    
    int localrc = 0;
    localrc = esmftrc_filesys_init(*buf_size, trace_dir, *localPet);
    if (localrc != 0) {
      *rc = ESMF_SUCCESS;
    }
    else {
      *rc = ESMF_FAILURE;
    }
    
  } 

  void FTN_X(c_esmftrc_filesys_fini)() 
  {
    esmftrc_filesys_fini();
  }
  

  
} // extern "C"
