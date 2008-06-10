//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_ParEnv_h
#define ESMC_ParEnv_h

#include <Mesh/include/ESMC_MeshTypes.h>

#include <iostream>
#include <fstream>
#include <streambuf>
#include <map>

#include <mpi.h>

namespace ESMC {


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

class ParLog {
public:
// If called (first time) with a fstem, it opens a file
// fstem.rank, otherwise this arg is ignored.
static ParLog *instance(const std::string &fstem="PARALOG", UInt rank = 0, bool use_log = false);
static std::ostream &stream() { return ParLog::instance()->use_log ? static_cast<std::ostream&>(ParLog::instance()->of) :
                                                           static_cast<std::ostream&>(ParLog::instance()->nl); }
static void flush() { ParLog::instance()->of.flush(); }
private:
static ParLog *classInstance;
ParLog(const std::string &fname, bool _use_log);
std::ofstream of;
onullstream nl;
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

static UInt Rank() { return rank; }
static UInt Size() { return psize; }
static bool Serial() { return serial; }

static std::ostream &Out() { return log->stream(); }
static ParLog *Log() { return log;}

private:
static bool serial;
static int rank;
static int psize;
static ParLog *log;
static MPI_Comm comm;
};

} // namespace

#endif
