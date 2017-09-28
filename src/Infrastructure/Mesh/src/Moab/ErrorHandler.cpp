#include "moab/ErrorHandler.hpp"
#include "ErrorOutput.hpp"
#ifdef MOAB_HAVE_MPI
#include "moab_mpi.h"
#endif

#include <stdlib.h>
#include <assert.h>

#ifdef _WIN32
#include <io.h>
#include <windows.h>
namespace
{
  void sleep(int sec)
  {
	  Sleep(sec*1000);
  }
}
#else
#include <unistd.h>
#endif

namespace moab {

static ErrorOutput* errorOutput = NULL;
static std::string lastError = "No error";

void MBErrorHandler_Init()
{
  if (NULL == errorOutput) {
    errorOutput = new (std::nothrow) ErrorOutput(stderr);
    assert(NULL != errorOutput);
    errorOutput->use_world_rank();
  }
}

void MBErrorHandler_Finalize()
{
  if (NULL != errorOutput) {
    delete errorOutput;
    errorOutput = NULL;
  }
}

bool MBErrorHandler_Initialized()
{
  return (NULL != errorOutput);
}

void MBErrorHandler_GetLastError(std::string& error)
{
  error = lastError;
}

void MBTraceBackErrorHandler(int line, const char* func, const char* file, const char* dir, const char* err_msg, ErrorType err_type)
{
  if (NULL == errorOutput)
    return;

  // For a globally fatal error, get world rank of current processor, so that it is only printed from processor 0
  // For a per-processor relevant error, set rank of current processor to 0, so that it is always printed
  int rank = 0;
  if (MB_ERROR_TYPE_NEW_GLOBAL == err_type && errorOutput->have_rank())
    rank = errorOutput->get_rank();

  if (0 == rank) {
    // Print the error message for a new error
    if (MB_ERROR_TYPE_EXISTING != err_type && NULL != err_msg) {
      errorOutput->print("--------------------- Error Message ------------------------------------\n");
      errorOutput->printf("%s!\n", err_msg);
      lastError = err_msg;
    }

    // Print a line of stack trace for a new error, or an existing one
    errorOutput->printf("%s() line %d in %s%s\n", func, line, dir, file);
  }
  else {
    // Do not print the error message or stack trace, since processor 0 will print them
    // Sleep 10 seconds before aborting so it will not accidently kill process 0
    sleep(10);
    abort();
  }
}

ErrorCode MBError(int line, const char* func, const char* file, const char* dir, ErrorCode err_code, const char* err_msg, ErrorType err_type)
{
  // When this routine is called to handle an existing error (instead of creating a new one),
  // we need to check if the returned non-success result from a function might be a non-error
  // condition. If no last error message was ever set, just return the given error code.
  if (MB_ERROR_TYPE_EXISTING == err_type && "No error" == lastError)
    return err_code;

  MBTraceBackErrorHandler(line, func, file, dir, err_msg, err_type);

#ifdef MOAB_HAVE_MPI
  // If this is called from the main() routine we call MPI_Abort() to allow
  // the parallel program to be properly shutdown
  if (strncmp(func, "main", 4) == 0)
    MPI_Abort(MPI_COMM_WORLD, err_code);
#endif

  return err_code;
}

} // namespace moab
