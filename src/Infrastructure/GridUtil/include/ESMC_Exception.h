#ifndef ESMC_Exception_h
#define ESMC_Exception_h

#include <string>
#include <sstream>

#include <exception>


namespace ESMC {

class Ex : public std::exception {
public:
Ex() : description(),
       whatString()
{}
Ex(const std::string &err) : description(err),
       whatString(err)
{}

Ex(const Ex &rhs) : description(),
                    whatString()
{ description << rhs.description.str();
  whatString = description.str();
}

~Ex() throw() {}

template<class T>
Ex &operator<<(const T &t) {
  description << t;
  return *this;
}

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
