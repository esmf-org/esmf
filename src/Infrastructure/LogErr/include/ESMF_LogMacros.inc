#define ESMF_LogErrMsg(anErr,err,msg) \
ESMF_LogErrMsg_(anErr,err,__LINE__,__FILE__,__DIR__,msg)

#define ESMC_LogErrMsg(err,msg) \
ESMC_LogErrMsg_(err,__LINE__,__FILE__,__DIR__,msg)

#define ESMF_LogErr(anErr,err) \
ESMF_LogErr_(anErr,err,__LINE__,__FILE__,__DIR__)

#define ESMC_LogErr(err) \
ESMC_LogErr_(err,__LINE__,__FILE__,__DIR__)

#define ESMF_LogWarnMsg(anErr,err,msg) \
ESMF_LogWarnMsg_(anErr,err,__LINE__,__FILE__,__DIR__,msg)

#define ESMC_LogWarnMsg(err,msg) \
ESMC_LogWarnMsg_(err,__LINE__,__FILE__,__DIR__,msg)

#define ESMF_LogWarn(anErr,err) \
ESMF_LogWarn_(anErr,err,__LINE__,__FILE__,__DIR__)

#define ESMC_LogWarn(err) \
ESMC_LogWarn_(err,__LINE__,__FILE__,__DIR__)
