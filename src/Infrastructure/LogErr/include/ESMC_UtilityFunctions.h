extern "C" {

int FTN(esmf_logopenfortran)(int* fortisOpen,int* num,
		   char nameLogFile[]);

int FTN(esmf_logclosefortran)(int* unitNumber);

void FTN(esmf_logprintchar)(int* unitNumber,char* data,
		    int* flush, char msg[],int* len1);

void FTN(esmf_logprintint)(int* unitNumber, int* data,
                    int* flush, char msg[],int* len1);

void FTN(esmf_logprintreal)(int* unitNumber,
                    double* data,int* flush, char msg[],int* len1);

void FTN(esmf_logprintstring)(int* unitNumber, 
                    char data[],int* len1,int* flush,
		    char msg[],int* len2);

void FTN(esmf_logprintnewline)(int* unitNumber,int* flush);

}
