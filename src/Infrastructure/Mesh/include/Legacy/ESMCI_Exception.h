// $Id$
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_Exception_h
#define ESMCI_Exception_h

#include <string>
#include <sstream>
#include <vector>

#include <exception>
#include <iostream>


namespace ESMCI {

/**
 * An exception helper class.  Allows one to throw formatted
 * messages such as Ex() << "This int:" << bad_int << " is not correct";
*/
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




/**
 * Full traceback function.  Saves the stack trace when an 
 * exception is unwinding.
*/
class TraceBack {
public:
TraceBack(const char *_func) : funcName(_func)
{}
~TraceBack();
static std::string StackTrace();
private:
const char *funcName;
};

/**
 * Replaces the stacktrace object with a faster
 * version that does nothing for very fast, optimized code.
*/
class EmptyTrace {
public:
EmptyTrace(const char *) {}
static std::string StackTrace();
};

#ifdef NOTRACE
typedef EmptyTrace Trace;
#else
typedef TraceBack Trace;
#endif

// Condition vanishes under optimized build
#ifdef NDEBUG
#define ThrowAssert(cond) ((void) 0)
#else
#define ThrowAssert(cond) (cond ? (void) 0 : throw Ex() << "Condition {" << #cond << "} failed at " << __FILE__ << ", line:" << __LINE__)
#endif

// Condition is tested in optimized and debug
#define ThrowRequire(cond) (cond ? (void) 0 : throw Ex() << "Condition {" << #cond << "} failed at " << __FILE__ << ", line:" << __LINE__)


#if 1
#define Throw() throw Ex() << __FILE__ << ", line:" << __LINE__ << ":" 
#else
// Use this one if exceptions aren't working and you want to see where one is being thrown
#define Throw() std::cerr << __FILE__ << ", line:" << __LINE__ << ":" 
#endif


} // namespace

#endif
