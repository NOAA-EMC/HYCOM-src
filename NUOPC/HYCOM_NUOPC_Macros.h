!-------------------------------------------------------------------------------
! HYCOM NUOPC CPP Macros
!-------------------------------------------------------------------------------
#ifndef FILENAME
#define FILENAME __FILE__
#endif
#ifndef CONTEXT
#define CONTEXT  line=__LINE__,file=__FILE__
#endif
#define PASSTHRU msg=ESMF_LOGERR_PASSTHRU,CONTEXT
#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#define ESMF_MSGERRORCHECK(rc,txt) ESMF_LogFoundError(rcToCheck=rc,msg=txt,line=__LINE__,file=__FILE__)
