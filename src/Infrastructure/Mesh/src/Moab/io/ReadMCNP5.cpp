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

#include "ReadMCNP5.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "Internals.hpp" // For MB_START_ID
#include "moab/Range.hpp"
#include "moab/FileOptions.hpp"
#include "moab/Util.hpp"

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <cmath>
#include <cassert>

namespace moab {

// Parameters
const double ReadMCNP5::PI   = 3.141592653589793;
const double ReadMCNP5::C2PI = 0.1591549430918954;
const double ReadMCNP5::CPI  = 0.3183098861837907;

ReaderIface* ReadMCNP5::factory(Interface* iface)
{
  return new ReadMCNP5(iface);
}

// Constructor
ReadMCNP5::ReadMCNP5(Interface* impl)
  : MBI(impl), fileIDTag(NULL), nodeId(0), elemId(0)
{
  assert(NULL != impl);
  MBI->query_interface(readMeshIface);
  assert(NULL != readMeshIface);
}

// Destructor
ReadMCNP5::~ReadMCNP5() {
  if (readMeshIface) {
    MBI->release_interface(readMeshIface);
    readMeshIface = 0;
  }
}

ErrorCode ReadMCNP5::read_tag_values(const char* /* file_name */,
                                     const char* /* tag_name */,
                                     const FileOptions& /* opts */,
                                     std::vector<int>& /* tag_values_out */,
                                     const SubsetList* /* subset_list */)
{
  return MB_NOT_IMPLEMENTED;
}

// Load the file as called by the Interface function
ErrorCode ReadMCNP5::load_file(const char* filename,
                               const EntityHandle* input_meshset,
                               const FileOptions& options,
                               const ReaderIface::SubsetList* subset_list,
                               const Tag* file_id_tag)
{
  // At this time there is no support for reading a subset of the file
  if (subset_list) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Reading subset of files not supported for meshtal");
  }

  nodeId = elemId = 0;
  fileIDTag = file_id_tag;

  // Average several meshtal files if the AVERAGE_TALLY option is given.
  // In this case, the integer value is the number of files to average.
  // If averaging, the filename passed to load_file is assumed to be the
  // root filename. The files are indexed as "root_filename""index".meshtal.
  // Indices start with 1.
  int n_files;
  bool average = false;
  ErrorCode result;
  if (MB_SUCCESS == options.get_int_option("AVERAGE_TALLY", n_files)) {
    // Read the first file (but do not average -> can't average a single file)
    result = load_one_file(filename,
                           input_meshset,
                           options,
                           average);
    if (MB_SUCCESS != result)
      return result;

    // Get the root filename
    std::string root_filename(filename);
    int length = root_filename.length();
    root_filename.erase(length - sizeof(".meshtal"));

    // Average the first file with the rest of the files
    average = true;
    for (int i = 2; i <= n_files; i++) {
      std::stringstream index;
      index << i;
      std::string subsequent_filename = root_filename + index.str() + ".meshtal";
      result = load_one_file(subsequent_filename.c_str(),
                             input_meshset,
                             options,
                             average);
      if (MB_SUCCESS != result)
        return result;
    }

  // If not averaging, read a single file
  }
  else {
    result = load_one_file(filename,
                           input_meshset,
                           options,
                           average);
    if (MB_SUCCESS != result)
      return result;
  }

  return MB_SUCCESS;
}

// This actually reads the file. It creates the mesh elements unless
// the file is being averaged with a pre-existing mesh.
ErrorCode ReadMCNP5::load_one_file(const char *fname,
                                   const EntityHandle *input_meshset,
                                   const FileOptions &options,
                                   const bool average)
{
  bool debug = false;
  if (debug)
    std::cout << "begin ReadMCNP5::load_one_file" << std::endl;

  ErrorCode result;
  std::fstream file;
  file.open(fname, std::fstream::in);
  char line[10000];

  // Create tags
  Tag date_and_time_tag,
        title_tag,
        nps_tag,
        tally_number_tag,
        tally_comment_tag,
        tally_particle_tag,
        tally_coord_sys_tag,
        tally_tag,
        error_tag;

  result = create_tags(date_and_time_tag,
                       title_tag,
                       nps_tag,
                       tally_number_tag,
                       tally_comment_tag,
                       tally_particle_tag,
                       tally_coord_sys_tag,
                       tally_tag,
                       error_tag);
  if (MB_SUCCESS != result)
    return result;

  // ******************************************************************
  // This info exists only at the top of each meshtal file
  // ******************************************************************

  // Define characteristics of the entire file
  char date_and_time[100] = "";
  char title[100] = "";
  // This file's number of particles
  unsigned long int nps;
  // Sum of this file's and existing file's nps for averaging
  unsigned long int new_nps;

  // Read the file header
  result = read_file_header(file,
                            debug,
                            date_and_time,
                            title,
                            nps);
  if (MB_SUCCESS != result)
    return result;

  // Blank line
  file.getline(line, 10000);

  // Everything stored in the file being read will be in the input_meshset.
  // If this is being saved in MOAB, set header tags
  if (!average && 0 != input_meshset) {
    result = set_header_tags(*input_meshset,
                             date_and_time,
                             title,
                             nps,
                             date_and_time_tag,
                             title_tag,
                             nps_tag);
    if (MB_SUCCESS != result)
      return result;
  }

  // ******************************************************************
  // This info is repeated for each tally in the meshtal file.
  // ******************************************************************

  // If averaging, nps will hold the sum of particles simulated in both tallies.
  while (!file.eof()) {
    // Define characteristics of this tally
    unsigned int        tally_number;
    char                tally_comment[100] = "";
    particle            tally_particle;
    coordinate_system   tally_coord_sys;
    std::vector<double> planes[3];
    unsigned int        n_chopped_x0_planes; 
    unsigned int        n_chopped_x2_planes;

    // Read tally header
    result = read_tally_header(file,
                               debug,
                               tally_number,
                               tally_comment,
                               tally_particle);
    if (MB_SUCCESS != result)
      return result;

    // Blank line
    file.getline(line, 10000);
    std::string l = line;
    // if this string is present then skip the following blank line
    if(std::string::npos != l.find("This mesh tally is modified by a dose response function.")) {
      file.getline(line, 10000);
    }
    
    // Read mesh planes
    result = read_mesh_planes(file,
                              debug,
                              planes,
                              tally_coord_sys);
    if (MB_SUCCESS != result)
      return result;

    // Get energy boundaries
    file.getline(line, 10000);
    std::string a = line;
    if (debug)
      std::cout << "Energy bin boundaries:=| " << a << std::endl;

    // Blank
    file.getline(line, 10000);

    // Column headers
    file.getline(line, 10000);

    // If using cylindrical mesh, it may be necessary to chop off the last theta element.
    // We chop off the last theta plane because the elements will be wrong and skew up
    // the tree building code. This is
    // because the hex elements are a linear approximation to the cylindrical elements.
    // Chopping off the last plane is problem-dependent, and due to MCNP5's mandate 
    // that the cylindrical mesh must span 360 degrees.
    if (CYLINDRICAL == tally_coord_sys &&
        MB_SUCCESS == options.get_null_option("REMOVE_LAST_AZIMUTHAL_PLANE")) {
      planes[2].pop_back();
      n_chopped_x2_planes = 1;
      if (debug)
        std::cout << "remove last cylindrical plane option found" << std::endl;
    }
    else {
      n_chopped_x2_planes = 0;
    }

    // If using cylindrical mesh, it may be necessary to chop off the first radial element.
    // These elements extend from the axis and often do not cover areas of interest. For
    // example, if the mesh was meant to cover r=390-400, the first layer will go from
    // 0-390 and serve as incorrect source elements for interpolation.
    if (CYLINDRICAL == tally_coord_sys &&
        MB_SUCCESS == options.get_null_option("REMOVE_FIRST_RADIAL_PLANE")) {
      std::vector<double>::iterator front=planes[0].begin();
      planes[0].erase(front);
      n_chopped_x0_planes = 1;
      if (debug)
        std::cout << "remove first radial plane option found" << std::endl;
    }
    else {
      n_chopped_x0_planes = 0;
    }

    // Read the values and errors of each element from the file.
    // Do not read values that are chopped off.
    unsigned int n_elements = (planes[0].size() - 1) * (planes[1].size() - 1) * (planes[2].size() - 1);
    double *values = new double [n_elements];
    double *errors = new double [n_elements];
    result = read_element_values_and_errors(file,
                                            debug,
                                            planes,
                                            n_chopped_x0_planes,
                                            n_chopped_x2_planes,
                                            tally_particle,
                                            values,
                                            errors);
    if (MB_SUCCESS != result)
      return result;

    // Blank line
    file.getline(line, 10000);

    // ****************************************************************
    // This tally has been read. If it is not being averaged, build tags,
    // vertices and elements. If it is being averaged, average the data
    // with a tally already existing in the MOAB instance.
    // ****************************************************************
    if (!average) {
      EntityHandle tally_meshset;
      result = MBI->create_meshset(MESHSET_SET, tally_meshset);
      if (MB_SUCCESS != result)
        return result;

      // Set tags on the tally
      result = set_tally_tags(tally_meshset,
                              tally_number,
                              tally_comment,
                              tally_particle,
                              tally_coord_sys,
                              tally_number_tag,
                              tally_comment_tag,
                              tally_particle_tag,
                              tally_coord_sys_tag);
      if (MB_SUCCESS != result)
        return result;

      // The only info needed to build elements is the mesh plane boundaries.
      // Build vertices...
      EntityHandle start_vert = 0;
      result = create_vertices(planes,
                               debug,
                               start_vert,
                               tally_coord_sys,
                               tally_meshset);
      if (MB_SUCCESS != result)
        return result;

      // Build elements and tag them with tally values and errors, then add
      // them to the tally_meshset.
      result = create_elements(debug,
                               planes,
                               n_chopped_x0_planes,
                               n_chopped_x2_planes,
                               start_vert,
                               values,
                               errors,
                               tally_tag,
                               error_tag,
                               tally_meshset,
                               tally_coord_sys);
      if (MB_SUCCESS != result)
        return result;

      // Add this tally's meshset to the output meshset
      if (debug)
        std::cout << "not averaging tally" << std::endl;

    // Average the tally values, then delete stuff that was created
    }
    else {
      if (debug)
        std::cout << "averaging tally" << std::endl;
      result = average_with_existing_tally(debug,
                                           new_nps,
                                           nps,
                                           tally_number,
                                           tally_number_tag,
                                           nps_tag,
                                           tally_tag,
                                           error_tag,
                                           values,
                                           errors,
                                           n_elements);
      if (MB_SUCCESS != result)
        return result;
    }

    // Clean up
    delete[] values;
    delete[] errors;
  }

  // If we are averaging, delete the remainder of this file's information.
  // Add the new nps to the existing file's nps if we are averaging.
  // This is calculated during every tally averaging but only used after the last one.
  if (average) {
    Range matching_nps_sets;
    result = MBI->get_entities_by_type_and_tag(0, MBENTITYSET, &nps_tag,
                                               0, 1, matching_nps_sets);
    if (MB_SUCCESS != result)
      return result;
    if (debug)
      std::cout << "number of matching nps meshsets="
                         << matching_nps_sets.size() << std::endl;
    assert(1 == matching_nps_sets.size());
    result = MBI->tag_set_data(nps_tag, matching_nps_sets, &new_nps);
    if (MB_SUCCESS != result)
      return result;

  // If this file is not being averaged, return the output meshset.
  }

  file.close();
  return MB_SUCCESS;
}

// create tags needed for this reader
ErrorCode ReadMCNP5::create_tags(Tag &date_and_time_tag,
                                 Tag &title_tag,
                                 Tag &nps_tag,
                                 Tag &tally_number_tag,
                                 Tag &tally_comment_tag,
                                 Tag &tally_particle_tag,
                                 Tag &tally_coord_sys_tag,
                                 Tag &tally_tag,
                                 Tag &error_tag)
{
  ErrorCode result;
  result = MBI->tag_get_handle("DATE_AND_TIME_TAG", 100, MB_TYPE_OPAQUE, 
                               date_and_time_tag, MB_TAG_SPARSE | MB_TAG_CREAT);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_get_handle("TITLE_TAG", 100, MB_TYPE_OPAQUE, title_tag,
                               MB_TAG_SPARSE | MB_TAG_CREAT);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_get_handle("NPS_TAG", sizeof(unsigned long int), MB_TYPE_OPAQUE,
                               nps_tag, MB_TAG_SPARSE | MB_TAG_CREAT);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_get_handle("TALLY_NUMBER_TAG", 1, MB_TYPE_INTEGER, 
                               tally_number_tag, MB_TAG_SPARSE | MB_TAG_CREAT);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_get_handle("TALLY_COMMENT_TAG", 100, MB_TYPE_OPAQUE, 
                               tally_comment_tag, MB_TAG_SPARSE | MB_TAG_CREAT);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_get_handle("TALLY_PARTICLE_TAG", sizeof(particle), MB_TYPE_OPAQUE,
                               tally_particle_tag, MB_TAG_SPARSE | MB_TAG_CREAT);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_get_handle("TALLY_COORD_SYS_TAG", sizeof(coordinate_system), MB_TYPE_OPAQUE,
                               tally_coord_sys_tag, MB_TAG_SPARSE | MB_TAG_CREAT);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_get_handle("TALLY_TAG", 1, MB_TYPE_DOUBLE, tally_tag,
                               MB_TAG_DENSE | MB_TAG_CREAT);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_get_handle("ERROR_TAG", 1, MB_TYPE_DOUBLE, error_tag,
                               MB_TAG_DENSE | MB_TAG_CREAT);
  if (MB_SUCCESS != result)
    return result;

  return MB_SUCCESS;
}

ErrorCode ReadMCNP5::read_file_header(std::fstream &file,
                                      bool debug,
                                      char date_and_time[100],
                                      char title[100],
                                      unsigned long int &nps)
{
  // Get simulation date and time
  // mcnp   version 5     ld=11242008  probid =  03/23/09 13:38:56
  char line[100];
  file.getline(line, 100);
  date_and_time = line;
  if (debug)
    std::cout << "date_and_time=| " << date_and_time << std::endl;

  // Get simulation title
  // iter Module 4                                                                   
  file.getline(line, 100);
  title = line;
  if (debug)
    std::cout << "title=| " << title  << std::endl;

  // Get number of histories
  // Number of histories used for normalizing tallies =      50000000.00
  file.getline(line, 100);
  std::string a = line;
  std::string::size_type b = a.find("Number of histories used for normalizing tallies =");
  if (std::string::npos != b) {
    std::istringstream nps_ss(
      a.substr(b + sizeof("Number of histories used for normalizing tallies ="), 100));
    nps_ss >> nps;
    if (debug)
      std::cout << "nps=| " << nps << std::endl;
  }
  else
    return MB_FAILURE;

  return MB_SUCCESS;
}

ErrorCode ReadMCNP5::set_header_tags(EntityHandle output_meshset,
                                     char date_and_time[100],
                                     char title[100],
                                     unsigned long int nps,
                                     Tag data_and_time_tag,
                                     Tag title_tag,
                                     Tag nps_tag)
{
  ErrorCode result;
  result = MBI->tag_set_data(data_and_time_tag, &output_meshset, 1, &date_and_time);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_set_data(title_tag, &output_meshset, 1, &title);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_set_data(nps_tag, &output_meshset, 1, &nps);
  if (MB_SUCCESS != result)
    return result;

  return MB_SUCCESS;
}

ErrorCode ReadMCNP5::read_tally_header(std::fstream &file,
                                       bool debug,
                                       unsigned int &tally_number,
                                       char tally_comment[100],
                                       particle &tally_particle)
{
  // Get tally number
  // Mesh Tally Number 104
  ErrorCode result;
  char line[100];
  file.getline(line, 100);
  std::string a = line;
  std::string::size_type b = a.find("Mesh Tally Number");
  if (std::string::npos != b) {
    std::istringstream tally_number_ss(a.substr(b + sizeof("Mesh Tally Number"), 100));
    tally_number_ss >> tally_number;
    if (debug)
      std::cout << "tally_number=| " << tally_number << std::endl;
  }
  else {
    std::cout << "tally number not found" << std::endl;
    return MB_FAILURE;
  }

  // Next get the tally comment (optional) and particle type
  // 3mm neutron heating in Be (W/cc)
  // This is a neutron mesh tally.
  // std::string tally_comment;
  
  // Get tally particle
  file.getline(line, 100);
  a = line;
  result = get_tally_particle(a, debug, tally_particle);
  if (MB_FAILURE == result) {
    // If this line does not specify the particle type, then it is a tally comment.
    // Get the comment, then get the particle type from the next line.
    tally_comment = line;
    file.getline(line, 100);
    a = line;
    result = get_tally_particle(a, debug, tally_particle);
    if (MB_SUCCESS != result)
      return result;
  }
  if (debug)
    std::cout << "tally_comment=| " << tally_comment << std::endl;

  return MB_SUCCESS;
}

ErrorCode ReadMCNP5::get_tally_particle(std::string a,
                                        bool debug,
                                        particle &tally_particle)
{
  if (std::string::npos != a.find("This is a neutron mesh tally.")) {
    tally_particle = NEUTRON;
  }
  else if (std::string::npos != a.find("This is a photon mesh tally.")) {
    tally_particle = PHOTON;
  }
  else if (std::string::npos != a.find("This is an electron mesh tally.")) {
    tally_particle = ELECTRON;
  }
  else return MB_FAILURE;

  if (debug)
    std::cout << "tally_particle=| " << tally_particle << std::endl;

  return MB_SUCCESS;
}

ErrorCode ReadMCNP5::read_mesh_planes(std::fstream &file,
                                      bool debug,
                                      std::vector<double> planes[3],
                                      coordinate_system &coord_sys)
{
  // Tally bin boundaries:
  ErrorCode result;
  char line[10000];
  file.getline(line, 10000);
  std::string a = line;
  if (std::string::npos == a.find("Tally bin boundaries:"))
    return MB_FAILURE;

  // Decide what coordinate system the tally is using
  // first check for Cylindrical coordinates:
  file.getline(line, 10000);
  a = line;
  std::string::size_type b = a.find("Cylinder origin at");
  if (std::string::npos != b) {
    coord_sys = CYLINDRICAL;
    if (debug)
      std::cout << "origin, axis, direction=| " << a << std::endl;
    std::istringstream ss(a.substr(b + sizeof("Cylinder origin at"), 10000));
    // The meshtal file does not contain sufficient information to transform
    // the particle. Although origin, axs, and vec is needed only origin and
    // axs appear in the meshtal file. Why was vec omitted?.
    // get origin (not used)
    // Cylinder origin at   0.00E+00  0.00E+00  0.00E+00, 
    // axis in  0.000E+00 0.000E+00 1.000E+00 direction
    double origin[3];
    if (debug)
      std::cout << "origin=| ";
    for (int i = 0; i < 3; i++) {
      ss >> origin[i];
      if (debug)
        std::cout << origin[i] << " ";
    }
    if (debug)
      std::cout << std::endl;
    int length_of_string = 10;
    ss.ignore(length_of_string, ' ');
    ss.ignore(length_of_string, ' ');
    ss.ignore(length_of_string, ' ');
    // Get axis (not used)
    double axis[3];
    if (debug)
      std::cout << "axis=| ";
    for (int i = 0; i < 3; i++) {
      ss >> axis[i];
      if (debug)
        std::cout << axis[i] << " ";
    }
    if (debug)
      std::cout << std::endl;
    file.getline(line, 10000);
    a = line;

    // Get r planes
    if (debug)
      std::cout << "R direction:=| ";
    b = a.find("R direction:");
    if (std::string::npos != b) {
      std::istringstream ss2(a.substr(b + sizeof("R direction"), 10000));
      result = get_mesh_plane(ss2, debug, planes[0]);
      if (MB_SUCCESS != result)
        return result;
    }
    else
      return MB_FAILURE;

    // Get z planes
    file.getline(line, 10000);
    a = line;
    if (debug)
      std::cout << "Z direction:=| ";
    b = a.find("Z direction:");
    if (std::string::npos != b) {
      std::istringstream ss2(a.substr(b + sizeof("Z direction"), 10000));
      result = get_mesh_plane(ss2, debug, planes[1]);
      if (MB_SUCCESS != result)
        return result;
    }
    else
      return MB_FAILURE;

    // Get theta planes
    file.getline(line, 10000);
    a = line;
    if (debug)
      std::cout << "Theta direction:=| ";
    b = a.find("Theta direction (revolutions):");
    if (std::string::npos != b) {
      std::istringstream ss2(a.substr(b + sizeof("Theta direction (revolutions):"), 10000));
      result = get_mesh_plane(ss2, debug, planes[2]);
      if (MB_SUCCESS != result)
        return result;
    }
    else
      return MB_FAILURE;

  // Cartesian coordinate system:
  }
  else if (std::string::npos != a.find("X direction:")) {
    coord_sys = CARTESIAN;
    // Get x planes
    if (debug)
      std::cout << "X direction:=| ";
    b = a.find("X direction:");
    if (std::string::npos != b) {
      std::istringstream ss2(a.substr(b + sizeof("X direction"), 10000));
      result = get_mesh_plane(ss2, debug, planes[0]);
      if (MB_SUCCESS != result)
        return result;
    }
    else
      return MB_FAILURE;

    // Get y planes
    file.getline(line, 10000);
    a = line;
    if (debug)
      std::cout << "Y direction:=| ";
    b = a.find("Y direction:");
    if (std::string::npos != b) {
      std::istringstream ss2(a.substr(b + sizeof("Y direction"), 10000));
      result = get_mesh_plane(ss2, debug, planes[1]);
      if (MB_SUCCESS != result)
        return result;
    }
    else
      return MB_FAILURE;

    // Get z planes
    file.getline(line, 10000);
    a = line;
    if (debug)
      std::cout << "Z direction:=| ";
    b = a.find("Z direction:");
    if (std::string::npos != b) {
      std::istringstream ss2(a.substr(b + sizeof("Z direction"), 10000));
      result = get_mesh_plane(ss2, debug, planes[2]);
      if (MB_SUCCESS != result)
        return result;
    }
    else
      return MB_FAILURE;

  // Spherical coordinate system not yet implemented:
  }
  else
    return MB_FAILURE;

  return MB_SUCCESS;
}

// Given a stringstream, return a vector of values in the string.
ErrorCode ReadMCNP5::get_mesh_plane(std::istringstream &ss,
                                    bool debug,
                                    std::vector<double> &plane)
{
  double value;
  plane.clear();
  while (!ss.eof()) {
    ss >> value;
    plane.push_back(value);
    if (debug)
      std::cout << value << " ";
  }
  if (debug)
    std::cout << std::endl;

  return MB_SUCCESS;
}

ErrorCode ReadMCNP5::read_element_values_and_errors(std::fstream &file,
                                                    bool /* debug */,
                                                    std::vector<double> planes[3],
                                                    unsigned int n_chopped_x0_planes,
                                                    unsigned int n_chopped_x2_planes,
                                                    particle tally_particle,
                                                    double values[],
                                                    double errors[])
{
  unsigned int index = 0;
  // Need to read every line in the file, even if we chop off some elements
  for (unsigned int i = 0; i < planes[0].size() - 1 + n_chopped_x0_planes; i++) {
    for (unsigned int j = 0; j < planes[1].size() - 1; j++) {
      for (unsigned int k = 0; k < planes[2].size() - 1 + n_chopped_x2_planes; k++) {
        char line[100];
        file.getline(line, 100);
        // If this element has been chopped off, skip it
        if (i < n_chopped_x0_planes) continue;
        if (k >= planes[2].size() - 1 && k < planes[2].size() - 1 + n_chopped_x2_planes) continue;
        std::string a=line;
        std::stringstream ss(a);
        double centroid[3];
        double energy;
        // For some reason, photon tallies print the energy in the first column
        if (PHOTON == tally_particle) ss >> energy;
        // The centroid is not used in this reader
        ss >> centroid[0];
        ss >> centroid[1];
        ss >> centroid[2];
        // Only the tally values and errors are used
        ss >> values[index];
        ss >> errors[index];

        // Make sure that input data is good
        if (!Util::is_finite(errors[index])) {
          std::cerr << "found nan error while reading file" << std::endl;
          errors[index] = 1.0;
        }
        if (!Util::is_finite(values[index])) {
          std::cerr << "found nan value while reading file" << std::endl;
          values[index] = 0.0;
        }

        index++;
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadMCNP5::set_tally_tags(EntityHandle tally_meshset,
                                    unsigned int tally_number,
                                    char tally_comment[100],
                                    particle tally_particle,
                                    coordinate_system tally_coord_sys,
                                    Tag tally_number_tag,
                                    Tag tally_comment_tag,
                                    Tag tally_particle_tag,
                                    Tag tally_coord_sys_tag)
{
  ErrorCode result;
  result = MBI->tag_set_data(tally_number_tag, &tally_meshset, 1, &tally_number);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_set_data(tally_comment_tag, &tally_meshset, 1, &tally_comment);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_set_data(tally_particle_tag, &tally_meshset, 1, &tally_particle);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_set_data(tally_coord_sys_tag, &tally_meshset, 1, &tally_coord_sys);
  if (MB_SUCCESS != result)
    return result;

  return MB_SUCCESS;
}

ErrorCode ReadMCNP5::create_vertices(std::vector<double> planes[3],
                                     bool debug,
                                     EntityHandle &start_vert,
                                     coordinate_system coord_sys,
                                     EntityHandle tally_meshset)
{
  // The only info needed to build elements is the mesh plane boundaries.
  ErrorCode result;
  int n_verts = planes[0].size() * planes[1].size() * planes[2].size();
  if (debug)
    std::cout << "n_verts=" << n_verts << std::endl;
  std::vector<double*> coord_arrays(3);
  result = readMeshIface->get_node_coords(3, n_verts, MB_START_ID,
                                          start_vert, coord_arrays);
  if (MB_SUCCESS != result)
    return result;
  assert(0 != start_vert); // Check for NULL

  for (unsigned int k = 0; k < planes[2].size(); k++) {
    for (unsigned int j = 0; j < planes[1].size(); j++) {
      for (unsigned int i = 0; i < planes[0].size(); i++) {
        unsigned int idx = (k*planes[0].size()*planes[1].size() + j*planes[0].size() + i);
        double in[3], out[3];

        in[0] = planes[0][i];
        in[1] = planes[1][j];
        in[2] = planes[2][k];
        result = transform_point_to_cartesian(in, out, coord_sys);
        if (MB_SUCCESS != result)
          return result;

        // Cppcheck warning (false positive): variable coord_arrays is assigned a value that is never used
        coord_arrays[0][idx] = out[0];
        coord_arrays[1][idx] = out[1];
        coord_arrays[2][idx] = out[2];
      }
    }
  }
  Range vert_range(start_vert, start_vert + n_verts - 1);
  result = MBI->add_entities(tally_meshset, vert_range);
  if (MB_SUCCESS != result)
    return result;
  
  if (fileIDTag) {
    result = readMeshIface->assign_ids(*fileIDTag, vert_range, nodeId);
    if (MB_SUCCESS != result)
      return result;
    nodeId += vert_range.size();
  }

  return MB_SUCCESS;
}

ErrorCode ReadMCNP5::create_elements(bool debug,
                                     std::vector<double> planes[3],
                                     unsigned int /*n_chopped_x0_planes*/,
                                     unsigned int /*n_chopped_x2_planes*/,
                                     EntityHandle start_vert,
                                     double *values,
                                     double *errors,
                                     Tag tally_tag,
                                     Tag error_tag,
                                     EntityHandle tally_meshset,
                                     coordinate_system tally_coord_sys)
{
  ErrorCode result;
  unsigned int index;
  EntityHandle start_element = 0;
  unsigned int n_elements = (planes[0].size() - 1) * (planes[1].size() - 1)
                                                   * (planes[2].size() - 1);
  EntityHandle *connect;
  result = readMeshIface->get_element_connect(n_elements, 8, MBHEX, MB_START_ID,
                                              start_element, connect);
  if (MB_SUCCESS != result)
    return result;
  assert(0 != start_element); // Check for NULL

  unsigned int counter = 0;
  for (unsigned int i = 0; i < planes[0].size() - 1; i++) {
    for (unsigned int j = 0; j < planes[1].size() - 1; j++) {
      for (unsigned int k = 0; k < planes[2].size() - 1; k++) {
        index = start_vert + i + j*planes[0].size() + k*planes[0].size()*planes[1].size();
        // For rectangular mesh, the file prints: x y z
        // z changes the fastest and x changes the slowest.
        // This means that the connectivity ordering is not consistent between
        // rectangular and cylindrical mesh.
        if (CARTESIAN == tally_coord_sys) {
          connect[0] = index;
          connect[1] = index + 1;
          connect[2] = index + 1 + planes[0].size();
          connect[3] = index +     planes[0].size();
          connect[4] = index +                        planes[0].size()*planes[1].size();
          connect[5] = index + 1 +                    planes[0].size()*planes[1].size();
          connect[6] = index + 1 + planes[0].size() + planes[0].size()*planes[1].size();
          connect[7] = index +     planes[0].size() + planes[0].size()*planes[1].size();
        // For cylindrical mesh, the file prints: r z theta
        // Theta changes the fastest and r changes the slowest.
        }
        else if (CYLINDRICAL == tally_coord_sys) {
          connect[0] = index;
          connect[1] = index + 1;
          connect[2] = index + 1 +                    planes[0].size()*planes[1].size();
          connect[3] = index +                        planes[0].size()*planes[1].size();
          connect[4] = index +     planes[0].size();
          connect[5] = index + 1 + planes[0].size();
          connect[6] = index + 1 + planes[0].size() + planes[0].size()*planes[1].size();
          connect[7] = index +     planes[0].size() + planes[0].size()*planes[1].size();
        }
        else
          return MB_NOT_IMPLEMENTED;

        connect += 8;
        counter++;
      }
    }
  }
  if (counter != n_elements)
    std::cout << "counter=" << counter << " n_elements="
              << n_elements << std::endl;

  Range element_range(start_element, start_element+n_elements-1);
  result = MBI->tag_set_data(tally_tag, element_range, values);
  if (MB_SUCCESS != result)
    return result;
  result = MBI->tag_set_data(error_tag, element_range, errors);
  if (MB_SUCCESS != result)
    return result;

  // Add the elements to the tally set
  result = MBI->add_entities(tally_meshset, element_range);
  if (MB_SUCCESS != result)
    return result;
  if (debug)
    std::cout << "Read " << n_elements << " elements from tally." << std::endl;

  if (fileIDTag) {
    result = readMeshIface->assign_ids(*fileIDTag, element_range, elemId);
    if (MB_SUCCESS != result)
      return result;
    elemId += element_range.size();
  }

  return MB_SUCCESS;
}

// Average a tally that was recently read in with one that already exists in
// the interface. Only the existing values will be updated.
ErrorCode ReadMCNP5::average_with_existing_tally(bool debug,
                                                 unsigned long int &new_nps,
                                                 unsigned long int nps1,
                                                 unsigned int tally_number,
                                                 Tag tally_number_tag,
                                                 Tag nps_tag,
                                                 Tag tally_tag,
                                                 Tag error_tag,
                                                 double *values1,
                                                 double *errors1,
                                                 unsigned int n_elements)
{
  // Get the tally number
  ErrorCode result;

  // Match the tally number with one from the existing meshtal file
  Range matching_tally_number_sets;
  const void* const tally_number_val[] = {&tally_number};
  result = MBI->get_entities_by_type_and_tag(0, MBENTITYSET, &tally_number_tag,
                                              tally_number_val, 1, 
                                              matching_tally_number_sets);
  if (MB_SUCCESS != result)
    return result;
  if (debug)
    std::cout << "number of matching meshsets="
              << matching_tally_number_sets.size() << std::endl;
  assert(1 == matching_tally_number_sets.size());

  // Identify which of the meshsets is existing
  EntityHandle existing_meshset;
  existing_meshset = matching_tally_number_sets.front();

  // Get the existing elements from the set
  Range existing_elements;
  result = MBI->get_entities_by_type(existing_meshset, MBHEX, existing_elements);
  if (MB_SUCCESS != result)
    return result;
  assert(existing_elements.size() == n_elements);

  // Get the nps of the existing and new tally
  unsigned long int nps0;
  Range sets_with_this_tag;
  result = MBI->get_entities_by_type_and_tag(0, MBENTITYSET, &nps_tag, 0, 1,
                                             sets_with_this_tag);
  if (MB_SUCCESS != result)
    return result;
  if (debug)
    std::cout << "number of nps sets=" << sets_with_this_tag.size() << std::endl;
  assert(1 == sets_with_this_tag.size());
  result = MBI->tag_get_data(nps_tag, &sets_with_this_tag.front(), 1, &nps0);
  if (MB_SUCCESS != result)
    return result;
  if (debug)
    std::cout << "nps0=" << nps0 << " nps1=" << nps1 << std::endl;
  new_nps = nps0 + nps1;

  // Get tally values from the existing elements
  double *values0 = new double [existing_elements.size()];
  double *errors0 = new double [existing_elements.size()];
  result = MBI->tag_get_data(tally_tag, existing_elements, values0);
  if (MB_SUCCESS != result) {
    delete[] values0;
    delete[] errors0;
    return result;
  }
  result = MBI->tag_get_data(error_tag, existing_elements, errors0);
  if (MB_SUCCESS != result) {
    delete[] values0;
    delete[] errors0;
    return result;
  }

  // Average the values and errors
  result = average_tally_values(nps0, nps1, values0, values1,
                                 errors0, errors1, n_elements);
  if (MB_SUCCESS != result) {
    delete[] values0;
    delete[] errors0;
    return result;
  }

  // Set the averaged information back onto the existing elements
  result = MBI->tag_set_data(tally_tag, existing_elements, values0);
  if (MB_SUCCESS != result) {
    delete[] values0;
    delete[] errors0;
    return result;
  }
  result = MBI->tag_set_data(error_tag, existing_elements, errors0);
  if (MB_SUCCESS != result) {
    delete[] values0;
    delete[] errors0;
    return result;
  }

  // Cleanup
  delete[] values0;
  delete[] errors0;

  return MB_SUCCESS;
}

ErrorCode ReadMCNP5::transform_point_to_cartesian(double *in, double *out,
                                                  coordinate_system coord_sys)
{
  // Transform coordinate system
  switch (coord_sys) {
    case CARTESIAN:
      out[0] = in[0]; // x
      out[1] = in[1]; // y
      out[2] = in[2]; // z
      break;
    // theta is in rotations
    case CYLINDRICAL:
      out[0] = in[0] * cos(2 * PI * in[2]); // x
      out[1] = in[0] * sin(2 * PI * in[2]); // y
      out[2] = in[1]; // z
      break;
    case SPHERICAL:
      return MB_NOT_IMPLEMENTED;
    default :
      return MB_NOT_IMPLEMENTED;
  }

  return MB_SUCCESS;
}

// Average two tally values and their error. Return average values in the
// place of first tally values.
ErrorCode ReadMCNP5::average_tally_values(const unsigned long int nps0,
                                          const unsigned long int nps1,
                                          double *values0,
                                          const double *values1,
                                          double *errors0,
                                          const double *errors1,
                                          const unsigned long int n_values)
{
  for (unsigned long int i = 0; i < n_values; i++) {
    //std::cout << " values0=" << values0[i] << " values1=" << values1[i]
    //          << " errors0=" << errors0[i] << " errors1=" << errors1[i]
    //          << " nps0=" << nps0 << " nps1=" << nps1 << std::endl;
    errors0[i] = sqrt(pow(values0[i]*errors0[i]*nps0, 2) +
                      pow(values1[i]*errors1[i]*nps1, 2)) /
                 (values0[i]*nps0 + values1[i]*nps1);

    // It is possible to introduce nans if the values are zero.
    if (!Util::is_finite(errors0[i]))
      errors0[i] = 1.0;

    values0[i] = (values0[i]*nps0 + values1[i]*nps1) / (nps0 + nps1);

    //std::cout << " values0=" << values0[i] << " errors0=" << errors0[i] << std::endl;
  }
  // REMEMBER TO UPDATE NPS0 = NPS0 + NPS1 after this

  return MB_SUCCESS;
}

} // namespace moab
