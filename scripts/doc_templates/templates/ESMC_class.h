// $Id: ESMC_class.h,v 1.1 2002/10/07 16:28:55 eschwab Exp $
//
// ESMF <Class> definition include file
//
// < Something here from legal about the status of the code, like:
//  This code developed by NASA/NCAR/ESMF whatever, and is covered by
//  the terms of the GNU public license.  See license file for more details. >
//
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_<Class>_h
#define ESMC_<Class>_h

//-------------------------------------------------------------------------
//
// !PURPOSE:
//
// The code in this file defines the C++ <Class> members and method 
// signatures (prototypes).  The companion file ESMC_<Class>.C contains
// the full code (bodies) for the <Class> methods.
//
// < insert a paragraph or two explaining what you'll find in this file >
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//
//-------------------------------------------------------------------------
//

// put any constants or macros which apply to the whole component in this file
#include <ESMC_<Comp>.h> 

#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.

//-------------------------------------------------------------------------
// class configuration type
struct ESMC_<Class>Config
{
   private:
//   < insert resource items here >
};

//-------------------------------------------------------------------------
// class definition

class ESMC_<Class> : public ESMC_Base    // inherits from ESMC_Base class
{

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMC_<Class>
//

// !PUBLIC MEMBER FUNCTIONS:
//
  public:
    ESMC_<Class> *ESMC_<Class>Create(); // interface only, deep class
    int ESMC_<Class>Destroy(void);      // interface only, deep class
    int ESMC_<Class>Construct(void);    // internal only, deep class
    int ESMC_<Class>Destruct(void);     // internal only, deep class
// or
    int ESMC_<Class>Init();         // shallow class only

    int ESMC_<Class>GetConfig(ESMC_<Class>Config *config);
    int ESMC_<Class>SetConfig(ESMC_<Class>Config *config);

    int ESMC_<Class>Get<Value>(<value type> *value);
    int ESMC_<Class>Set<Value>(<value type> *value);
    
//  int ESMC_<Class>Validate(char *options);	// from ESMC_Base
//  int ESMC_<Class>Print(char *options);	    // from ESMC_Base
  
// < list the rest of the public interfaces here >
  
  
//EOP

  private: 
//
// < list the rest of the private/internal vars and interfaces here >
//

};   // end of class definition

#endif  // ESMC_<Class>_h
