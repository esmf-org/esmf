!
! Simple exception handling macros for MAPL.
!

#ifndef MAPL_Exceptions_DONE
#define MAPL_Exceptions_DONE

#ifdef __Iam__
#undef __Iam__
#endif

#ifdef __RC__
#undef __RC__
#endif

#ifdef __STAT__
#undef __STAT__
#endif

#ifdef __rc__
#undefine __rc__
#endif

#ifdef __try__
#undefine __try__
#endif

#ifdef __catch__
#undefine __catch__
#endif

#ifdef __endcatch__
#undefine __endcatch__
#endif

#ifdef __except__
#undefine __except__
#endif

#ifdef __endtry__
#undefine __endtry__
#endif

#ifdef __raise__
#undefine __raise__
#endif

#ifdef  VERIFY_
#undef VERIFY_
#endif

#ifdef  IGNORE_
#undef IGNORE_
#endif

#ifdef __PROTEX__

!BOP

! !INCLUDE: MAPL_Exceptions - Macros for Exception Handling


 !DEFINED PARAMETERS:

      -----------------|---------------------------------------------
           Macro       |                Description
      -----------------|---------------------------------------------
                       |
        __Iam(name)__  |  Declares variables STATUS and Iam, using
                       |   string *name* to initialize Iam.
                       |
         __RC__        |  automatic exception handler for rc=STATUS
        __STAT__       |  automatic exception handler for stat=STATUS
                       |
         __try__       |  start of exception handler block
         __endtry__    |  end   of exception handler block
         __rc__        |  exception handler inside __try__ block
         __except__    |  catches all exceptions inside a __try__ block
                       |
     __Try__(label)    |  start of named exception handler block
     __endTry__(label) |  end   of named exception handler block
     __Rc__(label)     |  exception handler inside __Try__ block
                       |
        __catch__      |  start of exception catching block
        __endcatch__   |  end   of exception catching block
                       |
      __raise(e,d)__   |  raise exception *e* usinf the string *d*
                       |   as a verbose description of the exception.
      -----------------|---------------------------------------------
 
    IMPORTANT: No do-loops allowed inside __try__/__endtry__ blocks.
               Use the named __Try__/__endTry__ construct in such
               cases.

  !DESCRIPTION:

   \input{TeX/MAPL_ExceptionsDescr.tex}

  !BUGS:

    In the curent implementation, the scope of __try_/__endtry__ blocks 
    cannot contain Fortran do-loops. Use the alternative "named" 
    __Try__/__endTry__ construct in such cases.
  
 !REVISION HISTORY:

  05Nov2008  da Silva  Design and initial implementation


!EOP

#else

!-------------------------------------------------------------------

!
! Short hand for declaring key variables
!
#define __Iam__(name) integer :: STATUS; character(len=255) :: Iam=name

!
! Automatic exception hanler
!

#define __RC__         RC=STATUS); VERIFY_(STATUS
#define __STAT__       STAT=STATUS); VERIFY_(STATUS

!
! Try & catch exception functionality; the __rc__ macro is similar 
! to the __RC__ macro above but it does not invole the VERIFY_(STATUS)
! macro. Instead, it jumps out of the TRY block.
!

#define __try__        do 
#define __endtry__     exit; end do
#define __except__     exit; end do; do; if(STATUS==ESMF_SUCCESS) exit
#define __rc__         RC=STATUS); if(STATUS/=0) exit; IGNORE_(STATUS

#define __catch__      select case(STATUS)
#define __endcatch__   end select

#define __Try__(label)     label: do 
#define __endTry__(label)  exit label; end do
#define __Rc__(label)      RC=STATUS); if(STATUS/=ESMF_SUCCESS)exit label; IGNORE_(STATUS

!
! Raising exceptions
!

#define __raise__(exception,description) print '(a,'': '',a)', "exception", description; RETURN_(exception)  

!
! ESMF Error codes are defined here
!                     

#include "ESMF_ErrReturnCodes.inc"

!
! Pre-defined MAPL error codes
!

#define MAPL_RC_ERROR                     2000
#define MAPL_RC_RESOURCE_NOT_FOUND        2001
#define MAPL_RC_FIELD_NOT_FOUND           2002
#define MAPL_RC_BUNDLE_NOT_FOUND          2003
#define MAPL_RC_FORTRAN_ARRAY_NOT_FOUND   2004

!
! The MAPL_ErrLog must be defined after MAPL_Exceptions
!

#include "MAPL_ErrLog.h"

#endif

#endif



