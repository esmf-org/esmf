// $Id: ESMC_FieldDataMap.h,v 1.5 2007/06/23 04:00:27 cdeluca Exp $
//
//-------------------------------------------------------------------------
//BOP
//
// !CLASS: ESMC_FieldDataMap

// TODO: make the Field DataMap class based on the Array DataMap,
// and just add the additional information needed for the field level.

// !PUBLIC MEMBER FUNCTIONS:

// !INTERFACE:
//  void ESMC_FieldDataMapValidate(ESMC_FieldDataMap *datamap, int *rc);
//   // Routine to validate the internal state of a Field datamap object.
//   // !REQUIREMENTS:  FLD4.1
//   //
//   //
// !INTERFACE:
//  void ESMC_FieldDataMapPrint(ESMC_FieldDataMap *datamap, char *options, int *rc);
//   // Routine to print information about a Field datamap object.
//   // !REQUIREMENTS:
//   //
//   //
//EOP
//

extern "C" {

// non-method functions
void FTN(c_esmc_arraydatamapserialize)(int *status, int *dataRank,
                                  int *dataDimOrder,      /* ESMF_MAXDIM ints */
                                  int *dataNonIGridCounts, /* ESMF_MAXDIM ints */
                         void *buffer, int *length, int *offset, int *localrc);

void FTN(c_esmc_arraydatamapdeserialize)(int *status, int *dataRank,
                                  int *dataDimOrder,      /* ESMF_MAXDIM ints */
                                  int *dataNonIGridCounts, /* ESMF_MAXDIM ints */
                                  void *buffer, int *offset, int *localrc); 

void FTN(c_esmc_fielddatamapserialize)(int *status, int *isScalar,
                                  int *rankLength,  /* ESMF_MAXDIM ints */
                                  int *interleave,  /* 4 ints */
                                  int *horzRelloc, int *vertRelloc,
                         void *buffer, int *length, int *offset, int *localrc); 

void FTN(c_esmc_fielddatamapdeserialize)(int *status, int *isScalar,
                                    int *rankLength,  /* ESMF_MAXDIM ints */
                                    int *interleave,  /* 4 ints */
                                    int *horzRelloc, int *vertRelloc,
                                    void *buffer, int *offset, int *localrc); 

} // extern "C"
