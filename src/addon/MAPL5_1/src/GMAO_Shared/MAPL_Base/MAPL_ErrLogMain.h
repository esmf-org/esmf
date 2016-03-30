
!  $Id$ 

! The error logging may eventually evolve into a module based
! on the ESMF logger.  For now these macros provide simple
! traceback capability. 

#ifndef MAPL_ErrLogMain_DONE
#define MAPL_ErrLogMain_DONE

#ifdef VERIFY_
#undef VERIFY_
#endif
#ifdef ASSERT_
#undef ASSERT_
#endif

#define VERIFY_(A) if(MAPL_VRFY(A,Iam,__LINE__,RC))call MAPL_Abort

#define ASSERT_(A) if(MAPL_ASRT(A,Iam,__LINE__,RC))call MAPL_Abort

#endif
