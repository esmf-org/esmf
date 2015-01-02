#ifndef DAMSEL_UTIL_HPP
#define DAMSEL_UTIL_HPP

#include "moab/Forward.hpp"
#include "DebugOutput.hpp"

#include "damsel.h"
#include "damsel-internal.h"

  // Some macros to handle error checking (cribbed from WriteHDF5).  The
  // CHK_MB_ERR_* check the value of an ErrorCode.
  // The *_0 macros accept no other arguments. The *_1
  // macros accept a single damsel handle to close on error.
  // All macros contain a "return" statement.  These macros are coded with a do if while
  // to allow statements calling them to be terminated with a ;
#define CHK_MB_ERR( A, B )                                    \
do if (MB_SUCCESS != (A)) { \
mError->set_last_error(B);\
return error(A);} while(false)

#define CHK_MB_ERR_NR( A, B )                                    \
do if (MB_SUCCESS != (A)) { \
mError->set_last_error(B);\
std::cerr << B << std::endl;} while(false)

#define CHK_MB_ERR_2( A, B, C )                   \
do if (MB_SUCCESS != (A   )) { \
mError->set_last_error(B, C);                 \
return error(A);} while(false)

#define CHK_MB_ERR_FINALIZE( A, B )       \
do if (MB_SUCCESS != (A)) {             \
  DMSLlib_finalize(dmslLib); \
  dmslLib = 0;          \
  mError->set_last_error(B);\
  return error(A);     \
} while(false)

#define CHK_DMSL_ERR( A, B )                 \
do if (DMSL_OK.id != A.id) {             \
mError->set_last_error(B);\
return error(MB_FAILURE);             \
} while(false)

#define CHK_DMSL_ERR_2( A, B, C )            \
do if (DMSL_OK.id != A.id) {             \
mError->set_last_error(B, C);            \
return error(MB_FAILURE);             \
} while(false)

#define CHK_DMSL_ERR_NM( A )                 \
if (DMSL_OK.id != A.id) return error(MB_FAILURE);

#define CHK_DMSL_ERR_FINALIZE( A, B )        \
do if (DMSL_OK.id != A.id) {             \
  DMSLlib_finalize(dmslLib); \
  dmslLib = 0;          \
  mError->set_last_error(B);\
  return error(MB_FAILURE);                    \
} while(false)

namespace moab {

class DamselUtil 
{
public:
  friend class WriteDamsel;
  friend class ReadDamsel;
  
  //! needs to be a constructor to initialize dtom_data_type
  DamselUtil();

  static damsel_data_type mtod_data_type[MB_MAX_DATA_TYPE+1];

  static enum DataType dtom_data_type[DAMSEL_DATA_TYPE_PREDEFINED_WATERMARK+1];

  static enum damsel_entity_type mtod_entity_type[MBMAXTYPE+1];

  static enum EntityType dtom_entity_type[DAMSEL_ENTITY_TYPE_ALL_TYPES+1];

    //! convert handles in a container to a range; assumes EntityHandle and Damsel
    //! entity handles are the same size
  static ErrorCode container_to_range(damsel_model m, damsel_container &cont, Range &range);

    //! struct to hold information on damsel/moab tags
  class tinfo
  {
    public:
    tinfo(Tag mt, damsel_handle dt, TagType tt) : mTagh(mt), dTagh(dt), tagType(tt) {};
    tinfo()  : mTagh(0), dTagh(0), tagType(MB_TAG_ANY) {};
          
    Tag mTagh;
    damsel_handle dTagh;
    TagType tagType;
  };
  template<class T> struct MtagP : std::unary_function<T, bool> {
    public:
    MtagP(const Tag &th) {tH = th;};
    bool operator() (const T &tclass) {return tclass.mTagh == tH;};
    Tag tH;
  };
  
  template<class T> struct DtagP : std::unary_function<T, bool> {
    public:
    DtagP(const damsel_handle &th) {tH = th;};
    bool operator() (const T &tclass) {return tclass.dTagh == tH;};
    damsel_handle tH;
  };

private:
    //! damsel library id
  damsel_library dmslLib;
  
    //! damsel model id
  damsel_model dmslModel;

    //! other conventional tags
  tinfo xcoordsTag, ycoordsTag, zcoordsTag, 
      collFlagsTag, parentsTag, childrenTag;

    //! MOAB/damsel handles for dense [0], sparse [1], and conventional [2] tags
  std::vector<tinfo> tagMap;
  
    //! Damsel handle type used in (this build of) MOAB
  damsel_handle_type moabHandleType;

};

// This function doesn't do anything useful.  It's just a nice
// place to set a break point to determine why the reader fails.
    static inline ErrorCode error( ErrorCode rval )
  { return rval; }

}

#endif
