#ifndef ESMC_Exception_h
#define ESMC_Exception_h

#include <string>
#include <sstream>
#include <ostream>

#include <exception>


namespace ESMC {

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMC::Ex
//  
// !DESCRIPTION:
//     A wrapper for std::exception.  This class enables throwing an
//   exception with a formatted string on a single line, i.e.
//    throw Ex() << "Bad error, val=" << val << ", but should be "
//               << std::setw(5) << goodval;
///EOP
//-------------------------------------------------------------------------
class Ex : public std::exception {
public:
Ex() : description(),
       whatString()
{}
Ex(const std::string &err) : description(err),
       whatString(err)
{}

// Copy constructor.  Used at each stage of << ... << 
Ex(const Ex &rhs) : description(),
                    whatString()
{ description << rhs.description.str();
  whatString = description.str();
}

~Ex() throw() {}

// Allow any type to be <<'ed into the objects (growing) string buffer.
template<class T>
Ex &operator<<(const T &t) {
  description << t;
  return *this;
}

// Allow such operators as std::setw(), std::setfill, etc...
Ex &operator<<(std::ostream& (f)(std::ostream&)) {
  f(description);
  return *this;
}

const char *what() const throw() {
  return whatString.c_str();
}

private:
std::ostringstream description; // Accumlate 
std::string whatString;
};

} // namespace

#endif
