#if defined(sysIRIX64) || defined(sysOSF1) || defined(sysLinux) || defined(sysAIX) || defined(sysDarwin)
#define Const_MPItype_
#else
#ifdef Const_MPItype_
#undef Const_MPItype_
#endif
#endif
