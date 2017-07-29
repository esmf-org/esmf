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

/**
 * \class ReadSmf
 * \brief SMF reader from QSLIM
 * \author Michael Garland 
 */

#ifdef _MSC_VER  /* windows */
#  define _USE_MATH_DEFINES //For M_PI
#endif

#include <assert.h>
#include <stdlib.h>
#include <iostream>

#include "ReadSmf.hpp"
#include "moab/Range.hpp"
#include "Internals.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/FileOptions.hpp"
#include "AffineXform.hpp"

static inline int streq(const char *a, const char *b) { return strcmp(a, b) == 0; }

namespace moab {

ReadSmf::cmd_entry ReadSmf::read_cmds[] = {
  {"v", &ReadSmf::vertex},
  {":vn", &ReadSmf::v_normal},
  {":vc", &ReadSmf::v_color},
  {":fc", &ReadSmf::f_color},
  {"t", &ReadSmf::face},
  {"f", &ReadSmf::face},

  {"begin", &ReadSmf::begin},
  {"end", &ReadSmf::end},
  {"set", &ReadSmf::set},
  {"inc", &ReadSmf::inc},
  {"dec", &ReadSmf::dec},

  {"mmult", &ReadSmf::mload},
  {"mload", &ReadSmf::mmult},
  {"trans", &ReadSmf::trans},
  {"scale", &ReadSmf::scale},
  {"rot", &ReadSmf::rot},

  { NULL, NULL }
};

ErrorCode ReadSmf::parse_mat(const std::vector<std::string> & argv, AffineXform& mat)
{
  double values[12];
  ErrorCode err = parse_doubles(12, argv, values);
  if (MB_SUCCESS != err)
    return err;

  mat = AffineXform(values, values+9);
  return MB_SUCCESS;
}

void ReadSmf::bad_annotation(const char *cmd)
{
  std::cerr << "SMF: Malformed annotation ["<< cmd << "]" << std::endl;
}

ReaderIface* ReadSmf::factory(Interface* iface)
{
  return new ReadSmf(iface);
}

ReadSmf::ReadSmf(Interface* impl)
  : mdbImpl(impl), mCurrentMeshHandle(0), lineNo(0), commandNo(0), versionMajor(0), versionMinor(0)
{
  mdbImpl->query_interface(readMeshIface);
  ivar.next_vertex = 0;
  ivar.next_face = 0;
  _numNodes = _numFaces = 0;
  _numNodesInFile = _numElementsInFile = 0;
}

ReadSmf::~ReadSmf()
{
  if (readMeshIface) {
    mdbImpl->release_interface(readMeshIface);
    readMeshIface = 0;
  }
}

ErrorCode ReadSmf::read_tag_values(const char* /* file_name */,
                                   const char* /* tag_name */,
                                   const FileOptions& /* opts */,
                                   std::vector<int>& /* tag_values_out */,
                                   const SubsetList* /* subset_list */)
{
  return MB_NOT_IMPLEMENTED;
}

ErrorCode ReadSmf::load_file(const char *filename,
                             const EntityHandle* /* file_set */,
                             const FileOptions& opts,
                             const ReaderIface::SubsetList* subset_list,
                             const Tag* file_id_tag)
{
  ErrorCode result;
  lineNo = 0;
  commandNo = 0;
  versionMajor = 0;
  versionMinor = 0;

  if (subset_list) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Reading subset of files not supported for VTK");
  }

  // Does the caller want a field to be used for partitioning the entities?
  // If not, we'll assume any scalar integer field named MATERIAL_SET specifies partitions.
  std::string partition_tag_name;
  result = opts.get_option("PARTITION", partition_tag_name);
  if (result == MB_SUCCESS)
    mPartitionTagName = partition_tag_name;

  std::ifstream smfFile(filename);
  if (!smfFile)
    return MB_FILE_DOES_NOT_EXIST;

  ivar.next_face = 1;
  ivar.next_vertex = 1;
  state.push_back(SMF_State(ivar));

  while (smfFile.getline(line, SMF_MAXLINE, '\n').good()) {
    ++lineNo;
    result = parse_line(line);
    if (MB_SUCCESS != result)
      return result;
  }

  if (!smfFile.eof()){
    // Parsing terminated for a reason other than EOF: signal failure.
    return MB_FILE_WRITE_ERROR;
  }

  // At this point we have _numNodesInFile vertices and _numElementsInFile triangles
  // the coordinates are in _coords, and connectivities in _connec
  // std::vector<double> _coords; // 3*numNodes; we might not know the number of nodes
  // std::vector<int> _connec; // 3*num of elements; we might not know them;

  // Create vertices
  std::vector<double*> arrays;
  EntityHandle start_handle_out;
  start_handle_out = 0;
  result = readMeshIface->get_node_coords(3, _numNodesInFile, MB_START_ID,
                                          start_handle_out, arrays);

  if (MB_SUCCESS != result)
    return result;

  // Fill the arrays with data from _coords
  // Cppcheck warning (false positive): variable arrays is assigned a value that is never used
  for (int i = 0; i < _numNodesInFile; i++) {
    int i3 = 3*i;
    arrays[0][i] = _coords[i3];
    arrays[1][i] = _coords[i3 + 1];
    arrays[2][i] = _coords[i3 + 2];
  }
  // Elements

  EntityHandle start_handle_elem_out;
  start_handle_elem_out = 0;
  EntityHandle* conn_array_out;
  result = readMeshIface->get_element_connect(_numElementsInFile,
                                              3,
                                              MBTRI, // EntityType
                                              MB_START_ID,
                                              start_handle_elem_out,
                                              conn_array_out);
  if (MB_SUCCESS != result)
    return result;
  for (int j = 0; j < _numElementsInFile * 3; j++)
    conn_array_out[j] = _connec[j];

  // Notify MOAB of the new elements
  result = readMeshIface->update_adjacencies(start_handle_elem_out, _numElementsInFile,
                                               3, conn_array_out);

  if (MB_SUCCESS != result)
    return result;

  if (file_id_tag) {
    Range nodes(start_handle_out, start_handle_out + _numNodesInFile - 1);
    Range elems(start_handle_elem_out, start_handle_elem_out + _numElementsInFile - 1);
    readMeshIface->assign_ids(*file_id_tag, nodes);
    readMeshIface->assign_ids(*file_id_tag, elems);
  }

  return MB_SUCCESS;
}

ErrorCode ReadSmf::annotation(char *cmd, std::vector<std::string> & argv)
{
  // Skip over the '#$' prefix
  cmd += 2;

  if (streq(cmd, "SMF")) {
    // If SMF version is specified, it must be the first
    // thing specified in the file.
    if (commandNo > 1) {
      MB_SET_ERR(MB_FILE_WRITE_ERROR, "SMF file version specified at line " << lineNo);
    }

    if (2 == sscanf(argv[0].c_str(), "%d.%d", &versionMajor, &versionMinor)) {
      if (versionMajor != 1 || versionMinor != 0) {
        MB_SET_ERR(MB_FILE_WRITE_ERROR, "Unsupported SMF file version: " << versionMajor << "." << versionMinor);
      }
    }
    else {
      MB_SET_ERR(MB_FILE_WRITE_ERROR, "Invalid SMF version annotation");
    }
  }
  else if (streq(cmd, "vertices")) {
    if (argv.size() == 1)
      _numNodes = atoi(argv[0].c_str());
    else
      bad_annotation(cmd);
  }
  else if (streq(cmd, "faces")) {
    if (argv.size() == 1)
      _numFaces = atoi(argv[0].c_str());
    else
      bad_annotation(cmd);
  }
  else if (streq(cmd, "BBox")) {
  }
  else if (streq(cmd, "BSphere")) {
  }
  else if (streq(cmd, "PXform")) {
    if (argv.size() == 16) {
      //parse_mat(argv);
    }
    else
      bad_annotation(cmd);
  }
  else if (streq(cmd, "MXform")) {
    if (argv.size() == 16) {
      //parse_mat(argv);
    }
    else
      bad_annotation(cmd);
  }

  return MB_SUCCESS;
}

ErrorCode ReadSmf::parse_line(char *ln)
{
  char *cmd, *s;
  std::vector<std::string> argv;
  ErrorCode err;

  while (*ln == ' ' || *ln == '\t')
    ln++; // Skip initial white space

  // Ignore empty lines
  if (ln[0] == '\n' || ln[0] == '\0')
    return MB_SUCCESS;

  // Ignore comments
  if (ln[0] == '#' && ln[1] != '$')
    return MB_SUCCESS;

  // First, split the line into tokens
  cmd = strtok(ln, " \t\n");

  while ((s = strtok(NULL, " \t\n"))) {
    std::string stg(s);
    argv.push_back(stg);
  }

  // Figure out what command it is and execute it
  if (cmd[0] == '#' && cmd[1] == '$') {
    err = annotation(cmd, argv);
    if (MB_SUCCESS != err)
      return err;
  }
  else {
    cmd_entry *entry = &read_cmds[0];
    bool handled = 0;

    while (entry->name && !handled) {
      if (streq(entry->name, cmd)) {
        err = (this->*(entry->cmd))(argv);
        if (MB_SUCCESS != err)
          return err;
        handled = 1;
        ++commandNo;
      }
      else
        entry++;
    }

    if (!handled) {
      // If the first command was invalid, this probably
      // wasn't an Smf file. Fail silently in this case.
      // If versionMajor is set, then we saw an initial #$SMF,
      // in which case it must be a SMF file.
      if (!versionMajor && !commandNo)
        return MB_FILE_WRITE_ERROR;

      // Invalid command:
      MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Illegal SMF command at line " << lineNo << ": \"" << cmd << "\"");
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadSmf::check_length(int count,
                                const std::vector<std::string>& argv)
{
  if ((argv.size() < (unsigned)count) ||
      (argv.size() > (unsigned)count && argv[count][0] != '#')) {
    MB_SET_ERR(MB_FILE_WRITE_ERROR, "Expect " << count << " arguments at line " << lineNo);
  }

  return MB_SUCCESS;
}

ErrorCode ReadSmf::parse_doubles(int count,
                                 const std::vector<std::string> & argv,
                                 double results[])
{
  ErrorCode rval = check_length(count, argv);
  if (MB_SUCCESS != rval)
    return rval;

  char* endptr;
  for (int i = 0; i < count; i++) {
    results[i] = strtod(argv[i].c_str(), &endptr);
    if (*endptr) {
      MB_SET_ERR(MB_FILE_WRITE_ERROR, "Invalid vertex coordinates at line " << lineNo);
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadSmf::vertex(std::vector<std::string> & argv)
{
  double v[3];
  ErrorCode err = parse_doubles(3, argv, v);
  if (MB_SUCCESS != err)
    return err;

  state.back().vertex(v);
  ivar.next_vertex++;
  _numNodesInFile++;
  for (int j = 0; j < 3; j++)
    _coords.push_back(v[j]);
  //model->in_Vertex(v);
  return MB_SUCCESS;
}

ErrorCode ReadSmf::v_normal(std::vector<std::string> & /*argv*/)
{
  return MB_SUCCESS;
}

ErrorCode ReadSmf::v_color(std::vector<std::string> & /*argv*/)
{
  return MB_SUCCESS;
}

ErrorCode ReadSmf::f_color(std::vector<std::string> & /*argv*/)
{
  return MB_SUCCESS;
}

ErrorCode ReadSmf::face(std::vector<std::string> & argv)
{
  ErrorCode err = check_length(3, argv);
  if (MB_SUCCESS != err)
    return err;

  int vert[3] = {};
  char* endptr;
  for (unsigned int i = 0; i < argv.size(); i++) {
    vert[i] = strtol(argv[i].c_str(), &endptr, 0);
    if (*endptr) {
      MB_SET_ERR(MB_FILE_WRITE_ERROR, "Invalid face spec at line " << lineNo);
    }
  }

  state.back().face(vert, ivar);
  ivar.next_face++;
  for (int j = 0; j < 3; j++)
    _connec.push_back(vert[j]);
  _numElementsInFile++;

  return MB_SUCCESS;
}

ErrorCode ReadSmf::begin(std::vector<std::string> & /*argv*/)
{
  state.push_back(SMF_State(ivar, &state.back()));

  return MB_SUCCESS;
}

ErrorCode ReadSmf::end(std::vector<std::string> & /*argv*/)
{
  // There must always be at least one state on the stack.
  // Don't let mismatched begin/end statements cause us
  // to read from an empty vector.
  if (state.size() == 1) {
    MB_SET_ERR(MB_FILE_WRITE_ERROR, "End w/out Begin at line " << lineNo);
  }

  state.pop_back();

  return MB_SUCCESS;
}

ErrorCode ReadSmf::set(std::vector<std::string> & argv)
{
  if (argv.size() < 2 || argv[0] != "vertex_coorection")
    return MB_SUCCESS;

  char* endptr;
  int val = strtol(argv[1].c_str(), &endptr, 0);
  if (*endptr) {
    MB_SET_ERR(MB_FILE_WRITE_ERROR, "Invalid value at line " << lineNo);
  }

  state.back().set_vertex_correction(val);

  return MB_SUCCESS;
}

ErrorCode ReadSmf::inc(std::vector<std::string> & /*argv*/)
{
  //std::cerr << "SMF: INC not yet implemented." << std::endl;
  return MB_SUCCESS;
}

ErrorCode ReadSmf::dec(std::vector<std::string> &)
{
  //std::cerr << "SMF: DEC not yet implemented." << std::endl;
  return MB_SUCCESS;
}

ErrorCode ReadSmf::trans(std::vector<std::string> & argv)
{
  double v3[3];
  ErrorCode err = parse_doubles(3, argv, v3);
  if (MB_SUCCESS != err)
    return err;

  AffineXform M = AffineXform::translation(v3);
  //Mat4 M = Mat4::trans(atof(argv(0)), atof(argv(1)), atof(argv(2)));
  state.back().mmult(M);

  return MB_SUCCESS;
}

ErrorCode ReadSmf::scale(std::vector<std::string> & argv)
{
  double v3[3];
  ErrorCode err = parse_doubles(3, argv, v3);
  if (MB_SUCCESS != err)
    return err;

  AffineXform M = AffineXform::scale(v3);
  //Mat4 M = Mat4::scale(atof(argv(0)), atof(argv(1)), atof(argv(2)));
  state.back().mmult(M);

  return MB_SUCCESS;
}

ErrorCode ReadSmf::rot(std::vector<std::string> & argv)
{
  ErrorCode err = check_length(2, argv);
  if (MB_SUCCESS != err)
    return err;

  double axis[3] = {0., 0., 0.};
  std::string axisname = argv.front();
  argv.erase(argv.begin());
  if (axisname.size() != 1) {
    MB_SET_ERR(MB_FILE_WRITE_ERROR, "Malformed rotation command at line " << lineNo);
  }
  switch (axisname[0]) {
    case 'x':
      axis[0] = 1.;
      break;
    case 'y':
      axis[1] = 1.;
      break;
    case 'z':
      axis[2] = 1.;
      break;
    default:
      MB_SET_ERR(MB_FILE_WRITE_ERROR, "Malformed rotation command at line " << lineNo);
  }

  double angle;
  err = parse_doubles(1, argv, &angle);
  if (MB_SUCCESS != err)
    return err;
  angle *= M_PI / 180.0;

  AffineXform M = AffineXform::rotation(angle, axis);
  state.back().mmult(M);

  return MB_SUCCESS;
}

ErrorCode ReadSmf::mmult(std::vector<std::string> & argv)
{
  AffineXform mat;
  ErrorCode rval = parse_mat(argv, mat);
  if (MB_SUCCESS != rval)
    return rval;

  state.back().mmult(mat);

  return MB_SUCCESS;
}

ErrorCode ReadSmf::mload(std::vector<std::string> & argv)
{
  AffineXform mat;
  ErrorCode rval = parse_mat(argv, mat);
  if (MB_SUCCESS != rval)
    return rval;

  state.back().mload(mat);

  return MB_SUCCESS;
}

} // namespace moab
