#include "moab/Types.hpp"

const char* const moab::ErrorCodeStr[] = { 
  "MB_SUCCESS",
  "MB_INDEX_OUT_OF_RANGE",
  "MB_TYPE_OUT_OF_RANGE",
  "MB_MEMORY_ALLOCATION_FAILED",
  "MB_ENTITY_NOT_FOUND",
  "MB_MULTIPLE_ENTITIES_FOUND",
  "MB_TAG_NOT_FOUND",
  "MB_FILE_DOES_NOT_EXIST",
  "MB_FILE_WRITE_ERROR",
  "MB_NOT_IMPLEMENTED",
  "MB_ALREADY_ALLOCATED",
  "MB_VARIABLE_DATA_LENGTH",
  "MB_INVALID_SIZE",
  "MB_UNSUPPORTED_OPERATION",
  "MB_UNHANDLED_OPTION",
  "MB_STRUCTURED_MESH",
  "MB_FAILURE"
};

const char* const moab::DataTypeStr[] = { 
  "MB_TYPE_OPAQUE",
  "MB_TYPE_INTEGER",
  "MB_TYPE_DOUBLE",
  "MB_TYPE_BIT",
  "MB_TYPE_HANDLE"
};

const char* const SenseTypeStr_private[] = { 
  "SENSE_INVALID",
  "SENSE_REVERSE",
  "SENSE_BOTH",
  "SENSE_FORWARD"
};

/* Make this point into SenseTypeStr_private such that 
   it can be indexed with negative enumeration values.
   NOTE: If SENSE_INVALID is negative, then subtracting
         it means that we are adding abs(SENSE_INVALID). */
const char* const* const moab::SenseTypeStr = SenseTypeStr_private - moab::SENSE_INVALID;
