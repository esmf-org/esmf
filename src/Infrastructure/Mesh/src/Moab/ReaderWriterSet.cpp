/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 *
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 */

#include "moab/Core.hpp"

#include "moab/ReaderWriterSet.hpp"
#include "moab/ReaderIface.hpp"
#include "moab/WriterIface.hpp"

#include "ReadVtk.hpp"
#include "ReadSTL.hpp"
#include "ReadGmsh.hpp"
#include "ReadIDEAS.hpp"
#include "ReadMCNP5.hpp"
#include "ReadOBJ.hpp"
#include "ReadNASTRAN.hpp"
#include "ReadRTT.hpp"
#include "ReadABAQUS.hpp"
#include "ReadSms.hpp"
#include "Tqdcfr.hpp"
#include "ReadTetGen.hpp"
#include "ReadSmf.hpp"
#include "ReadTemplate.hpp"
#ifdef MOAB_HAVE_CGM
#  include "ReadCGM.hpp"
#endif

#include "WriteAns.hpp"
#include "WriteVtk.hpp"
#include "WriteGMV.hpp"
#include "WriteSTL.hpp"
#include "WriteGmsh.hpp"
#include "WriteSmf.hpp"
#include "WriteTemplate.hpp"

#ifdef MOAB_HAVE_NETCDF
#  include "ReadNCDF.hpp"
#  include "WriteNCDF.hpp"
#  include "WriteNC.hpp"
#  include "WriteSLAC.hpp"
#  include "ReadNC.hpp"
#endif

// 2nd include of ReadNC in case we have pnetcdf and not netcdf
#if defined(MOAB_HAVE_PNETCDF) && !defined(MOAB_HAVE_NETCDF)
#  include "ReadNC.hpp"
#endif

#ifdef MOAB_HAVE_CGNS
#  include "ReadCGNS.hpp"
#  include "WriteCGNS.hpp"
#endif

#ifdef MOAB_HAVE_CCMIO
#  include "ReadCCMIO.hpp"
#  include "WriteCCMIO.hpp"
#endif

#ifdef MOAB_HAVE_DAMSEL
#  include "WriteDamsel.hpp"
#  include "ReadDamsel.hpp"
#endif

#ifdef MOAB_HAVE_HDF5
#  include "ReadHDF5.hpp"
#  ifdef MOAB_HAVE_HDF5_PARALLEL
#    include "WriteHDF5Parallel.hpp"
#  else
#    include "WriteHDF5.hpp"
#  endif
#endif

#include <algorithm>

namespace moab {

ReaderWriterSet::ReaderWriterSet(Core* mdb)
  : mbCore( mdb )
{
#ifdef MOAB_HAVE_HDF5
  const char* hdf5_sufxs[] = { "h5m", "mhdf", NULL };
#ifdef MOAB_HAVE_HDF5_PARALLEL
  register_factory(  ReadHDF5::factory, WriteHDF5Parallel::factory,
                     "MOAB native (HDF5)", hdf5_sufxs, "MOAB" );
#else
  register_factory(  ReadHDF5::factory, WriteHDF5::factory,
                     "MOAB native (HDF5)", hdf5_sufxs, "MOAB" );
#endif
#endif

#ifdef MOAB_HAVE_NETCDF
  const char* exo_sufxs[] = { "exo", "exoII", "exo2", "g", "gen", NULL };
  register_factory( ReadNCDF::factory, WriteNCDF::factory, "Exodus II", exo_sufxs, "EXODUS" );
  register_factory( ReadNC::factory, WriteNC::factory, "Climate NC", "nc", "NC" );
#endif

#ifdef MOAB_HAVE_CGNS
  const char* cgns_sufxs[] = { "cgns", NULL };
  register_factory( ReadCGNS::factory, WriteCGNS::factory, "CGNS", cgns_sufxs, "CGNS" );
#endif

  register_factory( ReadIDEAS::factory, NULL, "IDEAS format", "unv", "UNV" );

  register_factory( ReadMCNP5::factory, NULL, "MCNP5 format", "meshtal", "MESHTAL" );

  const char* nastran_sufxs[] = { "nas", "bdf", NULL };
  register_factory( ReadNASTRAN::factory, NULL, "NASTRAN format", nastran_sufxs, "NAS" );

  register_factory( ReadABAQUS::factory, NULL, "ABAQUS INP mesh format", "abq", "Abaqus mesh" );

  register_factory( ReadRTT::factory, NULL, "RTT Mesh Format", "rtt", "Atilla RTT Mesh" );

  register_factory( ReadVtk::factory, WriteVtk::factory, "Kitware VTK", "vtk", "VTK" );

  register_factory( ReadOBJ::factory, NULL, "OBJ mesh format", "obj", "OBJ mesh" );

  register_factory( ReadSms::factory, NULL, "RPI SMS", "sms", "SMS" );

  register_factory( Tqdcfr::factory, NULL, "Cubit", "cub", "CUBIT" );

  register_factory( ReadSmf::factory, WriteSmf::factory , "QSlim format", "smf", "SMF");
#ifdef MOAB_HAVE_CGM_FACET
  const char* facet_sufxs[] = { "facet", NULL };
  register_factory( ReadCGM::factory, NULL, "Facet Engine Solid Model", facet_sufxs, "facet");
#endif
#ifdef MOAB_HAVE_CGM_OCC
  const char* occ_sufxs[] = { "brep", "occ", NULL };
  const char* step_sufxs[] = { "step", "stp", NULL };
  const char* iges_sufxs[] = { "iges", "igs", NULL };
  register_factory( ReadCGM::factory, NULL, "OpenCascade solid model", occ_sufxs, "OCC");
  register_factory( ReadCGM::factory, NULL, "STEP B-Rep exchange", step_sufxs, "STEP");
  register_factory( ReadCGM::factory, NULL, "IGES B-Rep exchange", iges_sufxs, "IGES");
#endif

#ifdef MOAB_HAVE_NETCDF
  register_factory( NULL, WriteSLAC::factory, "SLAC", "slac", "SLAC" );
#endif

#ifdef MOAB_HAVE_CCMIO
  const char* ccmio_sufxs[] = { "ccm", "ccmg", NULL };
  register_factory( ReadCCMIO::factory, WriteCCMIO::factory, "CCMIO files", ccmio_sufxs, "CCMIO");
#endif

#ifdef MOAB_HAVE_DAMSEL
  const char* damsel_sufxs[] = { "h5", NULL };
  register_factory( ReadDamsel::factory, WriteDamsel::factory, "Damsel files", damsel_sufxs, "DAMSEL");
#endif

  register_factory( NULL, WriteGMV::factory, "GMV", "gmv", "GMV" );

  register_factory( NULL, WriteAns::factory, "Ansys", "ans", "ANSYS" );

  const char* gmsh_sufxs[] = { "msh", "gmsh", NULL };
  register_factory( ReadGmsh::factory, WriteGmsh::factory, "Gmsh mesh file", gmsh_sufxs, "GMSH" );

  register_factory( ReadSTL::factory, WriteSTL::factory, "Stereo Lithography File (STL)", "stl", "STL" );

  const char* tetgen_sufxs[] = { "node", "ele", "face", "edge", NULL };
  register_factory( ReadTetGen::factory, 0, "TetGen output files", tetgen_sufxs, "TETGEN" );

  const char* template_sufxs[] = { NULL };
  register_factory( ReadTemplate::factory, WriteTemplate::factory, "Template input files", template_sufxs, "TEMPLATE" );

}


ReaderWriterSet::~ReaderWriterSet()
{
}

ErrorCode ReaderWriterSet::register_factory( reader_factory_t reader,
                                                 writer_factory_t writer,
                                                 const char* description,
                                                 const char* const* extensions,
                                                 const char* name )
{
  if (!reader && !writer)
    return MB_FAILURE;

    // check for duplicate names
  iterator h = handler_by_name( name );
  if (h != end()) {
    MB_SET_ERR(MB_FAILURE, "Conflicting string name for file formats: \"" << name << "\"");
  }

    // count extensions and check for duplicates
  const char* const* iter;
  for (iter = extensions; *iter; ++iter)
  {
    h = handler_from_extension( *iter );
    if (h != end())
    {
      if (NULL != reader && h->have_reader())
        MB_SET_ERR(MB_FAILURE, "Conflicting readers for file extension \"" << *iter << "\": \"" << h->description() << "\" and \"" << description << "\".");
      else if (NULL != writer && h->have_writer())
        MB_SET_ERR(MB_FAILURE, "Conflicting writers for file extension \"" << *iter << "\": \"" << h->description() << "\" and \"" << description << "\".");
    }
  }
  handlerList.push_back( Handler(reader, writer, name, description, extensions, iter - extensions) );
  return MB_SUCCESS;
}

ErrorCode ReaderWriterSet::register_factory( reader_factory_t reader,
                                                 writer_factory_t writer,
                                                 const char* description,
                                                 const char* extension,
                                                 const char* name )
{
  const char* extensions[2] = {extension, NULL};
  return register_factory( reader, writer, description, extensions, name );
}


ReaderIface* ReaderWriterSet::get_file_extension_reader(
                                  const std::string& filename ) const
{
  std::string ext = extension_from_filename( filename );
  iterator handler = handler_from_extension( ext, true, false );
  return handler == end() ? NULL : handler->make_reader(mbCore);
}

WriterIface* ReaderWriterSet::get_file_extension_writer(
                                  const std::string& filename ) const
{
  std::string ext = extension_from_filename( filename );
  iterator handler = handler_from_extension( ext, false, true );
  return handler == end() ? NULL : handler->make_writer(mbCore);
}

std::string ReaderWriterSet::extension_from_filename(
                                 const std::string& filename )
{
  std::string::size_type idx = filename.find_last_of( "." );
  std::string::size_type idirx = filename.find_last_of( "\\/" );

  if (idx == std::string::npos)
    return std::string("");
  if ((idirx != std::string::npos) && (idirx > idx))
    return std::string("");
  return filename.substr( idx + 1 );
}

ReaderWriterSet::Handler::Handler( reader_factory_t read_f,
                                     writer_factory_t write_f,
                                     const char* nm,
                                     const char* desc,
                                     const char* const* ext,
                                     int num_ext )
 : mReader(read_f), mWriter(write_f), mName(nm), mDescription(desc), mExtensions(num_ext)
{
  for (int i = 0; i < num_ext; ++i)
    mExtensions[i] = ext[i];
}

#ifdef WIN32
#define strcasecmp(A,B) _stricmp( A, B )
#endif

ReaderWriterSet::iterator
ReaderWriterSet::handler_from_extension( const std::string& ext,
                                           bool with_reader,
                                           bool with_writer ) const
{
  iterator iter;
  std::vector<std::string>::const_iterator siter;

    // try case-sensitive compare
  for (iter = begin(); iter != end(); ++iter)
  {
    if ((with_reader && !iter->have_reader()) ||
        (with_writer && !iter->have_writer()))
      continue;

    for (siter = iter->mExtensions.begin(); siter != iter->mExtensions.end(); ++siter)
      if (*siter == ext)
        return iter;
  }

    // try case-insensitive compare
  for (iter = begin(); iter != end(); ++iter)
  {
    if ((with_reader && !iter->have_reader()) ||
        (with_writer && !iter->have_writer()))
      continue;

    for (siter = iter->mExtensions.begin(); siter != iter->mExtensions.end(); ++siter)
      if (0 == strcasecmp( siter->c_str(), ext.c_str() ))
        return iter;
  }

  return end();
}

bool ReaderWriterSet::Handler::reads_extension(const char *ext) const
{
  if (!have_reader()) return false;

  std::vector<std::string>::const_iterator siter;
  for (siter = mExtensions.begin(); siter != mExtensions.end(); ++siter)
    if (!(*siter).compare(ext)) return true;
    else if (0 == strcasecmp( siter->c_str(), ext)) return true;

  return false;
}

bool ReaderWriterSet::Handler::writes_extension(const char *ext) const
{
  if (!have_writer()) return false;

  std::vector<std::string>::const_iterator siter;
  for (siter = mExtensions.begin(); siter != mExtensions.end(); ++siter)
    if (!(*siter).compare(ext)) return true;
    else if (0 == strcasecmp( siter->c_str(), ext)) return true;

  return false;
}

ReaderWriterSet::iterator
ReaderWriterSet::handler_by_name( const char* nm ) const
{
  return std::find( begin(), end(), nm );
}

bool ReaderWriterSet::Handler::operator==( const char* nm ) const
{
    // do case-insensitive comparison
  std::string::const_iterator siter = mName.begin();
  for (; *nm; ++nm, ++siter)
    if (siter == mName.end() || tolower(*nm) != tolower(*siter))
      return false;
  return *nm == '\0';
}

} // namespace moab



