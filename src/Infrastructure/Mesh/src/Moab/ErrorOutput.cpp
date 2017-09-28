#include "ErrorOutput.hpp"
#include "moab/MOABConfig.h"

#include <iostream>
#include <string.h>
#include <algorithm>
#include <assert.h>

#ifdef MOAB_HAVE_MPI
#include "moab_mpi.h"
#endif

namespace moab {

class FILEErrorStream : public ErrorOutputStream
{
private:
  FILE* filePtr;

public:
  FILEErrorStream(FILE* filep) : filePtr(filep) {}
  void println(int rank, const char* str);
  void println(const char* str);
};

void FILEErrorStream::println(int rank, const char* str)
{
  fprintf(filePtr, "[%d]MOAB ERROR: %s\n", rank, str);
  fflush(filePtr);
}

void FILEErrorStream::println(const char* str)
{
  fprintf(filePtr, "MOAB ERROR: %s\n", str);
  fflush(filePtr);
}

class CxxErrorStream : public ErrorOutputStream
{
private:
  std::ostream& outStr;

public:
  CxxErrorStream(std::ostream& str) : outStr(str) {}
  void println(int rank, const char* str);
  void println(const char* str);
};

void CxxErrorStream::println(int rank, const char* str)
{
  outStr << "[" << rank << "]MOAB ERROR: "  << str << std::endl;
  outStr.flush();
}

void CxxErrorStream::println(const char* str)
{
  outStr << "MOAB ERROR: " << str << std::endl;
  outStr.flush();
}

ErrorOutput::ErrorOutput(FILE* impl)
  : outputImpl(new FILEErrorStream(impl)),
    mpiRank(-1)
{
  lineBuffer.reserve(1024);
}

ErrorOutput::ErrorOutput(std::ostream& str)
  : outputImpl(new CxxErrorStream(str)),
    mpiRank(-1)
{
  lineBuffer.reserve(1024);
}

ErrorOutput::~ErrorOutput()
{ 
  if (!lineBuffer.empty()) {
    lineBuffer.push_back('\n');
    process_line_buffer();
  }

  if (NULL != outputImpl) {
    delete outputImpl;
    outputImpl = NULL;
  }
}

void ErrorOutput::use_world_rank()
{
#ifdef MOAB_HAVE_MPI
    int flag1;
    MPI_Initialized(&flag1);
    int flag2;
    MPI_Finalized(&flag2);
    if (flag1 && !flag2)
      MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);
#endif
}

void ErrorOutput::print_real(const char* buffer)
{
  lineBuffer.insert(lineBuffer.end(), buffer, buffer + strlen(buffer));
  process_line_buffer();
}

void ErrorOutput::print_real(const std::string& str)
{
  lineBuffer.insert(lineBuffer.end(), str.begin(), str.end());
  process_line_buffer();
}

void ErrorOutput::print_real(const char* fmt, va_list args1, va_list args2)
{
  size_t idx = lineBuffer.size();
#ifdef MOAB_HAVE_VSNPRINTF
  // try once with remaining space in buffer
  lineBuffer.resize(lineBuffer.capacity());
  unsigned size = vsnprintf(&lineBuffer[idx], lineBuffer.size() - idx, fmt, args1);
  ++size; // trailing null
  // if necessary, increase buffer size and retry
  if (size > (lineBuffer.size() - idx)) {
    lineBuffer.resize(idx + size);
    size = vsnprintf(&lineBuffer[idx], lineBuffer.size() - idx, fmt, args2);
    ++size; // trailing null
  }
#else
  // Guess how much space might be required.
  // If every character is a format code then there are len/3 format codes.
  // Guess a random large value of num_chars characters per formatted argument.
  const unsigned num_chars = 180;
  unsigned exp_size = (num_chars / 3) * strlen(fmt);
  lineBuffer.resize(idx + exp_size);
  unsigned size = vsprintf(&lineBuffer[idx], fmt, args1);
  ++size; // trailing null
  // check if we overflowed the buffer
  if (size > exp_size) {
    // crap!
    fprintf(stderr, "ERROR: Buffer overflow at %s:%d\n", __FILE__, __LINE__);
    lineBuffer.resize(idx + exp_size);
    size = vsprintf(&lineBuffer[idx], fmt, args2);
    ++size; // trailing null
  }
#endif

  // less one because we don't want the trailing '\0'
  lineBuffer.resize(idx + size - 1);
  process_line_buffer();
}

void ErrorOutput::process_line_buffer()
{
  size_t last_idx = 0;
  std::vector<char>::iterator i;
  for (i = std::find(lineBuffer.begin(), lineBuffer.end(), '\n');
       i != lineBuffer.end(); i = std::find(i, lineBuffer.end(), '\n')) {
    *i = '\0';
    if (have_rank())
      outputImpl->println(get_rank(), &lineBuffer[last_idx]);
    else
      outputImpl->println(&lineBuffer[last_idx]);
    ++i;
    last_idx = i - lineBuffer.begin();
  }

  if (last_idx) {
    i = std::copy(lineBuffer.begin() + last_idx, lineBuffer.end(), lineBuffer.begin());
    lineBuffer.erase(i, lineBuffer.end());
  }
}

} // namespace moab
