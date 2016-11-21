#ifndef moab_ERROR_OUTPUT_HPP
#define moab_ERROR_OUTPUT_HPP

#include <stdarg.h>
#include <stdio.h>
#include <vector>
#include <iosfwd>
#include <string>

#include "moab/Compiler.hpp"

namespace moab {

class ErrorOutputStream;

/**\brief Utility class for printing error output
 *
 * This class implements line-oriented output. That is, it buffers
 * output data until a newline is encountered, at which point it
 * sends the output to the output stream followed by an explicit
 * flush, and optionally prefixed with the MPI rank.
 *
 * \Note Any output not terminated with an newline character or
 *       followed by later output containing a newline character
 *       will not be flushed until the destructor is invoked.
 */
class ErrorOutput {
public:
  /**
   *\param str       Output stream to which to flush output
   */
  ErrorOutput(FILE* str);

  /**
   *\param str       Output stream to which to flush output
   */
  ErrorOutput(std::ostream& str);

  /**
   * Destructor flushes any remaining output that wasn't followed
   * by a newline character.
   */
  ~ErrorOutput();

  //!\brief Check if MPI rank has been set.
  bool have_rank() const { return mpiRank >= 0; }
  //!\brief Get MPI rank.
  int get_rank() const { return mpiRank; }
  //!\brief Set MPI rank.
  void set_rank(int rank) { mpiRank = rank; }
  //!\brief Set MPI rank to the rank of this process in MPI_COMM_WORLD,
  //!       if MOAB is built with MPI and MPI_Init has been called
  void use_world_rank();

  //!\brief Output the specified string
  void print(const char* str) { print_real(str); }

  //!\brief Output the specified string
  void print(const std::string& str) { print_real(str); }

  //!\brief Output the specified printf-formatted output
  void printf(const char* fmt, ...) MB_PRINTF(1);

private:
  ErrorOutputStream* outputImpl;
  int mpiRank;

  void print_real(const char* buffer);
  void print_real(const std::string& str);

  // Function must be passed to copies of the same va_list because
  // a) it might have to call vs(n)printf twice, b) vs(n)printf modifies
  // the va_list such that it cannot be reused, and c) va_copy is not
  // (yet) portable (c99, no c++ standard).
  void print_real(const char* buffer, va_list args1, va_list args2);
  void process_line_buffer();

  std::vector<char> lineBuffer;
};

inline void ErrorOutput::printf(const char* fmt, ...)
{
  va_list args1, args2;
  va_start(args1, fmt);
  va_start(args2, fmt);
  print_real(fmt, args1, args2);
  va_end(args2);
  va_end(args1);
}

class ErrorOutputStream {
public:
  ErrorOutputStream() {}
  virtual ~ErrorOutputStream() {}
  virtual void println(const char* str) = 0;
  virtual void println(int rank, const char* str) = 0;
};

} // namespace moab

#endif
