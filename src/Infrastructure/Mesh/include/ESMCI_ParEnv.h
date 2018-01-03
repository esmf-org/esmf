// $Id$
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_ParEnv_h
#define ESMCI_ParEnv_h

#include <Mesh/include/ESMCI_MeshTypes.h>

#include <iostream>
#include <fstream>
#include <streambuf>
#include <map>

#include <mpi.h>

namespace ESMCI {

#ifdef ESMF_PARLOG
// Implement a null ostream for ESMF users when we don't want mesh
// writing to its own logfile.
template <class cT, class traits = std::char_traits<cT> >
class basic_nullbuf: public std::basic_streambuf<cT, traits> {
typename traits::int_type overflow(typename traits::int_type c)
{
return traits::not_eof(c); // indicate success
}
};

template <class cT, class traits = std::char_traits<cT> >
class basic_onullstream: public std::basic_ostream<cT, traits> {
public:
basic_onullstream():
std::basic_ios<cT, traits>(&m_sbuf),
std::basic_ostream<cT, traits>(&m_sbuf)
{
init(&m_sbuf);
}

private:
basic_nullbuf<cT, traits> m_sbuf;
};

typedef basic_onullstream<char> onullstream;
typedef basic_onullstream<wchar_t> wonullstream;

#else

/// PGI DOESN'T SEEM TO LIKE TEMPLATE OVERLOADING
/// STREAMS ABOVE. THE MESH LOGGING ISN'T USED IN ESMF
/// SO TAKE IT OUT BY DEFAULT, USER CAN PUT BACK IN
/// BY DEFINING ESMF_PARLOG ABOVE. 

class MeshNullStream {

 public:

 template <typename T>
  MeshNullStream& operator<<(const T &t) {return *this; }

  MeshNullStream& operator<<(std::ostream &(*f)(std::ostream &t)) { return *this; }



 private:

  int t;
};




#endif


class ParLog {
public:
// If called (first time) with a fstem, it opens a file
// fstem.rank, otherwise this arg is ignored.
static ParLog *instance(const std::string &fstem="PARALOG", UInt rank = 0, bool use_log = false);

#ifdef ESMF_PARLOG
static std::ostream &stream() { return ParLog::instance()->use_log ? static_cast<std::ostream&>(ParLog::instance()->of) :
                                                           static_cast<std::ostream&>(ParLog::instance()->nl); }
static void flush() { ParLog::instance()->of.flush(); }

#else

static MeshNullStream &stream() { return ParLog::instance()->nl; }
static void flush() { }

#endif



private:

#ifdef ESMF_PARLOG
std::ofstream of;
onullstream nl;
#else

MeshNullStream nl;

#endif



static ParLog *classInstance;
ParLog(const std::string &fname, bool _use_log);
ParLog(const ParLog&);
ParLog &operator=(const ParLog&);
bool use_log;
};




/// *********** Wrap MPI junk *************
class Par {
public:
static void Init(const std::string &logfile= "PARLOG", bool use_log=false, MPI_Comm _comm = MPI_COMM_WORLD);
static void Abort();
static void End();
static MPI_Comm Comm() { return comm;}
void SetComm(MPI_Comm _comm);

static UInt Rank() { if (psize == 0) Init(); return rank; }
static UInt Size() { if (psize == 0) Init(); return psize; }
static bool Serial() { if (psize == 0) Init(); return serial; }

#if ESMF_PARLOG
static std::ostream &Out() { if (psize == 0) Init(); return log->stream(); }
#else
static MeshNullStream &Out() { if (psize == 0) Init(); return log->stream(); }
#endif



//static ParLog *Log() { if (psize == 0) Init(); return log;}

private:
static bool serial;
static int rank;
static int psize;
static ParLog *log;
static MPI_Comm comm;
};

} // namespace

#endif
