#include "ReadTetGen.hpp"
#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/FileOptions.hpp"
#include "MBTagConventions.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <ctype.h>
#include <map>

namespace moab {

ReaderIface* ReadTetGen::factory(Interface* moab)
{
  return new ReadTetGen(moab);
}

ReadTetGen::ReadTetGen(Interface* moab)
  : mbIface(moab), readTool(0)
{
  moab->query_interface(readTool);
}

ReadTetGen::~ReadTetGen()
{
  if (mbIface && readTool)
    mbIface->release_interface(readTool);
}

ErrorCode ReadTetGen::open_file(const std::string& filename,
                                const std::string& basename,
                                const std::string& suffix,
                                const char* exp_suffix,
                                const char* opt_name,
                                const FileOptions& opts,
                                std::ifstream& file_stream,
                                bool file_required)
{
  std::string real_file_name;
  ErrorCode rval = opts.get_option(opt_name, real_file_name);
  if (MB_ENTITY_NOT_FOUND == rval || real_file_name.empty()) {
    if (MB_SUCCESS == rval)
      file_required = true;
    if (suffix == exp_suffix) {
      real_file_name = filename;
    }
    else {
      real_file_name = basename;
      real_file_name += ".";
      real_file_name += exp_suffix;
    }
  }

  if (!real_file_name.empty())
    file_stream.open(real_file_name.c_str(), std::ios::in);
  if (file_required && !file_stream.is_open()) {
    MB_SET_ERR(MB_FILE_DOES_NOT_EXIST, real_file_name << ": cannot read file");
  }

  return MB_SUCCESS;
}

ErrorCode ReadTetGen::read_tag_values(const char* /* file_name */,
                                      const char* /* tag_name */,
                                      const FileOptions& /* opts */,
                                      std::vector<int>& /* tag_values_out */,
                                      const SubsetList* /* subset_list */)
{
  return MB_NOT_IMPLEMENTED;
}

ErrorCode ReadTetGen::load_file(const char* file_name_c,
                                const EntityHandle* /* file_set */,
                                const FileOptions& opts,
                                const ReaderIface::SubsetList* subset_list,
                                const Tag* file_id_tag)
{
  std::ifstream node_file, ele_file, face_file, edge_file;
  ErrorCode rval;

  if (subset_list) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Reading subset of files not supported for TetGen");
  }

  std::string suffix, base, filename(file_name_c);
  size_t dot_idx = filename.find_last_of('.');
  if (dot_idx == std::string::npos) {
    base = filename;
  }
  else {
    suffix = filename.substr(dot_idx + 1);
    for (size_t i = 0; i < suffix.length(); ++i)
      suffix[i] = (char)tolower(suffix[i]);
    if (suffix == "node" || suffix == "ele" || suffix == "face" || suffix == "edge") {
      base = filename.substr(0, dot_idx);
    }
    else {
      base = filename;
      suffix.clear();
    }
  }

  rval = open_file(filename, base, suffix, "node", "NODE_FILE", opts, node_file, true);
  if (MB_SUCCESS != rval)
    return rval;
  rval = open_file(filename, base, suffix, "ele", "ELE_FILE", opts, ele_file);
  if (MB_SUCCESS != rval)
    return rval;
  rval = open_file(filename, base, suffix, "face", "FACE_FILE", opts, face_file);
  if (MB_SUCCESS != rval)
    return rval;
  rval = open_file(filename, base, suffix, "edge", "EDGE_FILE", opts, edge_file);
  if (MB_SUCCESS != rval)
    return rval;

  std::vector<Tag> attr_tags[4];
  std::vector<int> attr_idx[4];
  const char* option_names[4] = {"NODE_ATTR_LIST", "EDGE_ATTR_LIST", "TRI_ATTR_LIST", "TET_ATTR_LIST"};
  const char* group_names[4] = {0, "CURVE_ID", "SURFACE_ID", "VOLUME_ID"};
  for (int i = 0; i < 4; ++i) {
    std::string opt_str;
    rval = opts.get_str_option(option_names[i], opt_str);
    if (MB_SUCCESS != rval)
      continue;
    rval = parse_attr_list(opt_str, attr_tags[i], attr_idx[i], group_names[i]);
    if (MB_SUCCESS != rval) {
      MB_SET_ERR(MB_TYPE_OUT_OF_RANGE, option_names[i] << ": invalid option value");
    }
  }

  Range tets, tris, edges;
  std::vector<EntityHandle> nodes;
  rval = read_node_file(node_file, &attr_tags[0][0], &attr_idx[0][0], attr_tags[0].size(), nodes);
  if (MB_SUCCESS == rval && ele_file.is_open())
    rval = read_elem_file(MBTET, ele_file, nodes, tets);
  if (MB_SUCCESS == rval && face_file.is_open())
    rval = read_elem_file(MBTRI, face_file, nodes, tris);
  if (MB_SUCCESS == rval && edge_file.is_open())
    rval = read_elem_file(MBEDGE, edge_file, nodes, edges);

  if (file_id_tag && MB_SUCCESS == rval)
    rval = readTool->assign_ids(*file_id_tag, &nodes[0], nodes.size());
  if (file_id_tag && MB_SUCCESS == rval)
    rval = readTool->assign_ids(*file_id_tag, edges);
  if (file_id_tag && MB_SUCCESS == rval)
    rval = readTool->assign_ids(*file_id_tag, tris);
  if (file_id_tag && MB_SUCCESS == rval)
    rval = readTool->assign_ids(*file_id_tag, tets);

  return rval;
}

ErrorCode ReadTetGen::parse_attr_list(const std::string& option_str,
                                      std::vector<Tag>& tag_list,
                                      std::vector<int>& index_list,
                                      const char* group_designator)
{
  std::vector<std::string> name_list;
  size_t prev_pos = 0;
  while (prev_pos != std::string::npos) {
    size_t pos = option_str.find_first_of(',', prev_pos);
    name_list.push_back(option_str.substr(prev_pos, pos));
    prev_pos = pos + 1;
  }

  index_list.resize(name_list.size());
  std::map<std::string, int> name_count;
  for (size_t i = 0; i < name_list.size(); ++i)
    index_list[i] = name_count[name_list[i]]++;

  for (size_t i = 0; i < name_list.size(); ++i) {
    if (group_designator && name_list[i] == group_designator) {
      tag_list[i] = 0;
      index_list[i] = -1;
    }
    else if (name_list.empty()) {
      tag_list[i] = 0;
      index_list[i] = 0;
    }
    else {
      ErrorCode rval = mbIface->tag_get_handle(name_list[i].c_str(),
                                               name_count[name_list[i]],
                                               MB_TYPE_DOUBLE,
                                               tag_list[i],
                                               MB_TAG_DENSE | MB_TAG_CREAT);
      if (MB_SUCCESS != rval)
        return rval;
    }
  }

  return MB_SUCCESS;
}

ErrorCode ReadTetGen::read_line(std::istream& file,
                                std::string& line,
                                int& lineno)
{
  // Loop until we find a non-empty line
  do {
    // Read a line
    line.clear();
    if (!getline(file, line))
      return MB_FILE_WRITE_ERROR;
    ++lineno;
    // Strip comments from line
    size_t pos = line.find_first_of('#');
    if (pos != std::string::npos)
      line = line.substr(0, pos);
    // Strip leading whitespace from line
    for (pos = 0; pos < line.length() && isspace(line[pos]); ++pos);
    if (pos == line.length())
      line.clear();
    else if (pos != 0)
      line = line.substr(pos);
  }
  while (line.empty());

  return MB_SUCCESS;
}

ErrorCode ReadTetGen::read_line(std::istream& file,
                                double* values_out,
                                int num_values,
                                int& lineno)
{
  // Get a line of text
  std::string line;
  ErrorCode rval = read_line(file, line, lineno);
  if (MB_SUCCESS != rval)
    return rval;

  // Tokenize line as doubles
  std::stringstream str(line);
  for (int i = 0; i < num_values; ++i) {
    double v;
    if (!(str >> v)) {
      MB_SET_ERR(MB_FAILURE, "Error reading node data at line " << lineno);
    }
    values_out[i] = v;
  }

  // Check that we're at the end of the line
  int junk;
  if ((str >> junk) || !str.eof()) {
    MB_SET_ERR(MB_FAILURE, "Unexpected trailing data for line " << lineno << " of node data");
  }

  return MB_SUCCESS;
}

ErrorCode ReadTetGen::read_node_file(std::istream& file,
                                     const Tag* attr_tag_list,
                                     const int* attr_tag_index,
                                     int attr_tag_list_len,
                                     std::vector<EntityHandle>& nodes)
{
  int lineno = 0;
  ErrorCode rval;

  double header_vals[4];
  rval = read_line(file, header_vals, 4, lineno);
  if (MB_SUCCESS != rval)
    return rval;

  const int num_vtx = (int)header_vals[0];
  const int dim = (int)header_vals[1];
  const int num_attr = (int)header_vals[2];
  const int bdry_flag = (int)header_vals[3];
  if (num_vtx < 1 || dim < 2 || dim > 3 || num_attr < 0 || bdry_flag < 0 || bdry_flag > 1) {
    MB_SET_ERR(MB_FAILURE, "Invalid header line for node data");
  }
  if (attr_tag_list_len > num_attr)
    attr_tag_list_len = num_attr;

  // Allocate space for tag data
  std::map<Tag, int> tag_size;
  std::map<Tag, std::vector<double> > tag_data;
  for (int i = 0; i < attr_tag_list_len; ++i) {
    if (!attr_tag_list[i] || attr_tag_index[i] < 0)
      continue;
    std::vector<double>& data = tag_data[attr_tag_list[i]];
    // Increase tag size by one value per vertex for each time
    // we encounter it in the list.
    data.resize(data.size() + num_vtx);
    ++tag_size[attr_tag_list[i]];
  }
  std::vector<double*> attr_data(attr_tag_list_len);
  std::vector<int> attr_size(attr_tag_list_len);
  for (int i = 0; i < attr_tag_list_len; ++i) {
    if (!attr_tag_list[i] || attr_tag_index[i] < 0) {
      attr_data[i] = 0;
      attr_size[i] = 0;
    }
    else {
      attr_data[i] = &(tag_data[attr_tag_list[i]])[0];
      attr_size[i] = tag_size[attr_tag_list[i]];
    }
  }

  // Allocate vertices
  std::vector<double*> coords;
  EntityHandle start_handle;
  rval = readTool->get_node_coords(dim, num_vtx, 1, start_handle, coords);
  if (MB_SUCCESS != rval)
    return rval;

  // Read data line for each node
  nodes.reserve(num_vtx);
  std::vector<double> data(1 + dim + num_attr + bdry_flag);
  std::vector<int> ids(num_vtx);
  for (int i = 0; i < num_vtx; ++i) {
    rval = read_line(file, &data[0], data.size(), lineno);
    if (MB_SUCCESS != rval)
      return rval;

    // Get ID
    ids[i] = (int)data[0];
    if (ids[i] >= (int)nodes.size())
      nodes.resize(ids[i] + 1);
    nodes[ids[i]] = start_handle + i;

    // Get coordinates
    // Cppcheck warning (false positive): variable coords is assigned a value that is never used
    for (int j = 0; j < dim; ++j)
      coords[j][i] = data[j + 1];

    // Get attribute data
    for (int j = 0; j < attr_tag_list_len; ++j)
      if (attr_data[j])
        attr_data[j][i*attr_size[j] + attr_tag_index[j]] = data[j + 1 + dim];

    // Discard boundary bit
  }

  // Store tag data
  Range node_range;
  node_range.insert(start_handle, start_handle + num_vtx - 1);
  for (std::map<Tag, std::vector<double> >::iterator i = tag_data.begin();
       i != tag_data.end(); ++i) {
    rval = mbIface->tag_set_data(i->first, node_range, &i->second[0]);
    if (MB_SUCCESS != rval)
      return rval;
  }
  Tag idtag;
  rval = mbIface->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, idtag);
  if (MB_SUCCESS == rval) {
    rval = mbIface->tag_set_data(idtag, node_range, &ids[0]);
    if (MB_SUCCESS != rval)
      return rval;
  }

  return MB_SUCCESS;
}

ErrorCode ReadTetGen::read_elem_file(EntityType type,
                                     std::istream& file,
                                     const std::vector<EntityHandle>& nodes,
                                     Range& elems)
{
  int lineno = 0;
  ErrorCode rval;

  int node_per_elem, have_group_id, dim;
  double header_vals[3];
  switch (type) {
    case MBTET:
      rval = read_line(file, header_vals, 3, lineno);
      node_per_elem = (int)header_vals[1];
      have_group_id = (int)header_vals[2];
      dim = 3;
      break;
    case MBTRI:
      rval = read_line(file, header_vals, 2, lineno);
      node_per_elem = 3;
      have_group_id = (int)header_vals[1];
      dim = 2;
      break;
    case MBEDGE:
      rval = read_line(file, header_vals, 1, lineno);
      node_per_elem = 2;
      have_group_id = 0;
      dim = 1;
      break;
    default:
      rval = MB_FAILURE;
      break;
  }
  if (MB_SUCCESS != rval)
    return rval;
  const int num_elem = (int)header_vals[0];
  if (num_elem < 1 || node_per_elem < 2 || have_group_id < 0 || have_group_id > 1) {
    MB_SET_ERR(MB_FAILURE, "Invalid header line for element data");
  }

  // Create group map
  std::map<double, EntityHandle> groups;
  Tag dim_tag, id_tag;
  rval = mbIface->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, id_tag);
  if (MB_SUCCESS != rval)
    return rval;
  const int negone = -1;
  rval = mbIface->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER,
                                 dim_tag, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);
  if (MB_SUCCESS != rval)
    return rval;

  // Allocate elements
  EntityHandle start_handle, *conn_array;
  rval = readTool->get_element_connect(num_elem, node_per_elem, type, 1, start_handle, conn_array);
  if (MB_SUCCESS != rval)
    return rval;
  elems.insert(start_handle, start_handle + num_elem - 1);

  // Read data line for each node
  std::vector<double> data(1 + node_per_elem + have_group_id);
  std::vector<int> ids(num_elem);
  for (int i = 0; i < num_elem; ++i) {
    rval = read_line(file, &data[0], data.size(), lineno);
    if (MB_SUCCESS != rval)
      return rval;

    // Get ID
    ids[i] = (int)data[0];

    // Get connectivity
    for (int j = 0; j < node_per_elem; ++j)
      conn_array[node_per_elem*i + j] = nodes[(int)data[j + 1]];

    // Grouping
    if (have_group_id && 0.0 != data[node_per_elem + 1]) {
      double id = data[node_per_elem + 1];
      EntityHandle grp = groups[id];
      if (0 == grp) {
        rval = mbIface->create_meshset(MESHSET_SET, grp);
        if (MB_SUCCESS != rval)
          return rval;
        elems.insert(grp);
        rval = mbIface->tag_set_data(dim_tag, &grp, 1, &dim);
        if (MB_SUCCESS != rval)
          return rval;
        int iid = (int)id;
        rval = mbIface->tag_set_data(id_tag, &grp, 1, &iid);
        if (MB_SUCCESS != rval)
          return rval;
        groups[id] = grp;
      }
      EntityHandle handle = start_handle + i;
      rval = mbIface->add_entities(grp, &handle, 1);
      if (MB_SUCCESS != rval)
        return rval;
    }
  }

  // Store id data
  Range elems2;
  elems2.insert(start_handle, start_handle + num_elem - 1);
  rval = mbIface->tag_set_data(id_tag, elems2, &ids[0]);
  if (MB_SUCCESS != rval)
    return rval;

  return MB_SUCCESS;
}

} // namespace moab
