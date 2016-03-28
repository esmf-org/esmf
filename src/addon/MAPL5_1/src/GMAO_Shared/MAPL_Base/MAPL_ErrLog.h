
!  $Id$ 

! The error logging may eventually evolve into a module based
! on the ESMF logger.  For now these macros provide simple
! traceback capability. 

#ifndef MAPL_ErrLog_DONE


#define MAPL_ErrLog_DONE

#ifdef RETURN_
#undef RETURN_
#endif
#ifdef VERIFY_
#undef VERIFY_
#endif
#ifdef ASSERT_
#undef ASSERT_
#endif

#ifdef IGNORE_
#undef IGNORE_
#endif
 
#define IGNORE_(a) continue

#ifdef I_AM_MAIN

#define VERIFY_(A) if(MAPL_VRFY(A,Iam,__LINE__))call MAPL_Abort
#define ASSERT_(A) if(MAPL_ASRT(A,Iam,__LINE__))call MAPL_Abort

#else

#ifdef ANSI_CPP

#define RETURN_(...)   if(MAPL_RTRN(__VA_ARGS__,Iam,__LINE__,RC))return
#define VERIFY_(...)   if(MAPL_VRFY(__VA_ARGS__,Iam,__LINE__,RC))return
#define ASSERT_(...)   if(MAPL_ASRT(__VA_ARGS__,Iam,__LINE__,RC))return

#else

#define RETURN_(A)     if(MAPL_RTRN(A,Iam,__LINE__,RC))return
#define VERIFY_(A)     if(MAPL_VRFY(A,Iam,__LINE__,RC))return
#define ASSERT_(A)     if(MAPL_ASRT(A,Iam,__LINE__,RC))return

#endif
#endif


#endif
