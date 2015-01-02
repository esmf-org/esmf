#ifndef moab_DEBUG_OUTPUT_HPP
#define moab_DEBUG_OUTPUT_HPP

#include <stdarg.h>
#include <stdio.h>
#include <vector>
#include <iosfwd>
#include <string>

#include "moab/Compiler.hpp"

namespace moab {

class Range;
class DebugOutputStream;

/**\brief Utility class for printing debug output
 *
 * This class implements line-oriented output.  That is, it buffers
 * output data until a newline is encountered, at which point it
 * sends the output to the output stream followed by an explicit
 * flush, and optionally prefixed with the MPI rank.
 *
 * This class also implements a verbosity filter for all output.
 * The class instance has a verbosity limit.  Each request
 * for output has an associated verbosity level.  If the verbosity
 * level for the output is is less greater than the limit then
 * the output is discarded.  By convetion a verbosity limit
 * of zero should indicate no output. Therefore all requests
 * for output should have an associated verbosity level greater
 * than or equal to one.
 *
 * \Note Any output not terminated with an newline character or
 *       followed by later output containing a newline character
 *       will not be flushed until the destructor is invoked.
 * \Note C++-style IO (i.e. std::ostream) is not supported because
 *       it is necessarily inefficient for debug-type output.  All
 *       formatting (e.g. converting arguments to strings, etc.) must
 *       be done even when output is disabled.
 */
class DebugOutput {
  
public:

  /**
   *\param str       Output stream to which to flush output
   *\param verbosity Verbosity limit.
   */
  DebugOutput( DebugOutputStream* str, unsigned verbosity = 0 );
  /**
   *\param str       Output stream to which to flush output
   *\param rank      MPI rank with which to prefix output.
   *\param verbosity Verbosity limit.
  */
  DebugOutput( DebugOutputStream* str, int rank, unsigned verbosity = 0 );
  /**
   *\param str     Output stream to which to flush output
   *\param enabled Enable output: if not true, all output operations to nothing.
   */
  DebugOutput( FILE* str, unsigned verbosity = 0 );
  /**
   *\param str       Output stream to which to flush output
   *\param rank      MPI rank with which to prefix output.
   *\param verbosity Verbosity limit.
   */
  DebugOutput( FILE* str, int rank, unsigned verbosity = 0 );
  /**
   *\param str       Output stream to which to flush output
   *\param verbosity Verbosity limit.
   */
  DebugOutput( std::ostream& str, unsigned verbosity = 0 );
  /**
   *\param str       Output stream to which to flush output
   *\param rank      MPI rank with which to prefix output.
   *\param verbosity Verbosity limit.
   */
  DebugOutput( std::ostream& str, int rank, unsigned verbosity = 0 );

  /**
   *\param pfx       Prefix for output
   *\param str       Output stream to which to flush output
   *\param verbosity Verbosity limit.
   */
  DebugOutput( const char* pfx, DebugOutputStream* str, unsigned verbosity = 0 );
  /**
   *\param pfx       Prefix for output
   *\param str       Output stream to which to flush output
   *\param rank      MPI rank with which to prefix output.
   *\param verbosity Verbosity limit.
  */
  DebugOutput( const char* pfx, DebugOutputStream* str, int rank, unsigned verbosity = 0 );
  /**
   *\param pfx       Prefix for output
   *\param str     Output stream to which to flush output
   *\param enabled Enable output: if not true, all output operations to nothing.
   */
  DebugOutput( const char* pfx, FILE* str, unsigned verbosity = 0 );
  /**
   *\param pfx       Prefix for output
   *\param str       Output stream to which to flush output
   *\param rank      MPI rank with which to prefix output.
   *\param verbosity Verbosity limit.
   */
  DebugOutput( const char* pfx, FILE* str, int rank, unsigned verbosity = 0 );
  /**
   *\param pfx       Prefix for output
   *\param str       Output stream to which to flush output
   *\param verbosity Verbosity limit.
   */
  DebugOutput( const char* pfx, std::ostream& str, unsigned verbosity = 0 );
  /**
   *\param pfx       Prefix for output
   *\param str       Output stream to which to flush output
   *\param rank      MPI rank with which to prefix output.
   *\param verbosity Verbosity limit.
   */
  DebugOutput( const char* pfx, std::ostream& str, int rank, unsigned verbosity = 0 );

  DebugOutput( const DebugOutput& copy );
  DebugOutput& operator=( const DebugOutput& copy );

  /**
   * Destructor flushes any remaining output that wasn't followed
   * by a newline character.
   */
  ~DebugOutput();
  
  //!\brief Check if MPI rank has been set.
  bool have_rank() const    { return mpiRank >= 0; }
  //!\brief Get MPI rank.
  int get_rank() const      { return mpiRank; }
  //!\brief Set MPI rank.
  void set_rank( int rank ) { mpiRank  = rank; }
  //!\brief Set MPI rank to the rank of this proccess in MPI_COMM_WORLD,
  //!       or zero if MOAB is build w/out MPI.
  void use_world_rank();
  
  //!\brief Only print debug output from N processes
  void limit_output_to_first_N_procs( int N )
    { if (mpiRank >= N) verbosityLimit = 0; }
  
  //!\brief Get verbosity limit
  unsigned get_verbosity() const      { return verbosityLimit; }
  //!\brief Set verbosity limit
  void set_verbosity( unsigned val )   { verbosityLimit = val; }

  //!\brief Get line prefix
  const std::string& get_prefix() const  { return linePfx; }
  //!\brief Set line prefix
  void set_prefix(const std::string& str) { linePfx = str; }

  //!\brief Output the specified string iff output is enabled.
  void print( int verbosity, const char* str ) 
    { if (check(verbosity)) print_real(str); }
  
  //!\brief Output the specified string iff output is enabled.
  void print( int verbosity, const std::string& str )
    { if (check(verbosity)) print_real(str); }
    
  //!\brief Output the specified printf-formatted output iff output is enabled
  inline void printf( int verbosity, const char* fmt, ... ) MB_PRINTF(2);

  //!\brief Output the specified string iff output is enabled.
  //! 
  //! Include current CPU time (as returned by clock()) in output.
  void tprint( int verbosity, const char* str ) 
    { if (check(verbosity)) tprint_real(str); }
  
  //!\brief Output the specified string iff output is enabled.
  //! 
  //! Include current CPU time (as returned by clock()) in output.
  void tprint( int verbosity, const std::string& str )
    { if (check(verbosity)) tprint_real(str); }
    
  //!\brief Output the specified printf-formatted output iff output is enabled
  //! 
  //! Include current CPU time (as returned by clock()) in output.
  inline void tprintf( int verbosity, const char* fmt, ... ) MB_PRINTF(2);

   
  //!\brief Print the contents of a moab::Range
  //!\param pfx String to print after default class prefix and before range contents
  void print( int verbosity, const char* pfx, const Range& range )
    { if (check(verbosity)) list_range_real( pfx, range ); }
  //!\brief Print the contents of a moab::Range
  void print( int verbosity, const Range& range )
    { if (check(verbosity)) list_range_real( 0, range ); }
   
  //!\brief Print the contents of a moab::Range as numerical values only
  //!\param pfx String to print after default class prefix and before range contents
  void print_ints( int verbosity, const char* pfx, const Range& range )
    { if (check(verbosity)) list_ints_real( pfx, range ); }
  //!\brief Print the contents of a moab::Range as numerical values only
  void print_ints( int verbosity, const Range& range )
    { if (check(verbosity)) list_ints_real( 0, range ); }

private:
  
  std::string linePfx;
  DebugOutputStream *outputImpl;
  int mpiRank;
  unsigned verbosityLimit;
  double initTime;

  void tprint();

  void list_range_real( const char* pfx, const Range& range );
  void list_ints_real( const char* pfx, const Range& range );
  void print_real( const char* buffer );
  void print_real( const std::string& str );
  void tprint_real( const char* buffer );
  void tprint_real( const std::string& str );
  
  // Function must be passed to copies of the same va_list because
  // a) it might have to call vs(n)printf twice, b) vs(n)printf modifies
  // the va_list such that it cannot be reused, and c) va_copy is not
  // (yet) portable (c99, no c++ standard).
  void print_real( const char* buffer, va_list args1, va_list args2 );
  void tprint_real( const char* buffer, va_list args1, va_list args2 );
  void process_line_buffer();
  
  std::vector<char> lineBuffer;
  
  inline bool check(unsigned verbosity) 
    { return verbosity <= verbosityLimit; }
};


class DebugOutputStream {
protected:
  friend class DebugOutput;
  int referenceCount;
public:
  DebugOutputStream() : referenceCount(1) {}
  virtual ~DebugOutputStream();
  virtual void println( const char* pfx, const char* str ) = 0;
  virtual void println( int rank, const char* pfx, const char* str ) = 0;
};
  
void DebugOutput::printf( int verbosity, const char* fmt, ... )
{
  if (check(verbosity)) {
    va_list args1, args2;
    va_start(args1, fmt);
    va_start(args2, fmt);
    print_real(fmt, args1, args2);
    va_end(args2);
    va_end(args1);
  }
}

void DebugOutput::tprintf( int verbosity, const char* fmt, ... )
{
  if (check(verbosity)) {
    va_list args1, args2;
    va_start(args1, fmt);
    va_start(args2, fmt);
    tprint_real(fmt, args1, args2);
    va_end(args2);
    va_end(args1);
  }
}

} // namespace moab

#endif
