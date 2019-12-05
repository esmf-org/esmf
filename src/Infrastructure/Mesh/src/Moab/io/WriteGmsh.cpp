#include "WriteGmsh.hpp"
#include "moab/CN.hpp"
#include "MBTagConventions.hpp"
#include "MBParallelConventions.h"
#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/WriteUtilIface.hpp"
#include "moab/FileOptions.hpp"
#include "GmshUtil.hpp"

#include <fstream>
#include <map>
#include <set>

namespace moab {

const int DEFAULT_PRECISION = 10;

WriterIface *WriteGmsh::factory(Interface* iface)
{
  return new WriteGmsh(iface);
}

WriteGmsh::WriteGmsh(Interface* impl)
  : mbImpl(impl)
{
  impl->query_interface(mWriteIface);
}

WriteGmsh::~WriteGmsh()
{
  mbImpl->release_interface(mWriteIface);
}

// A structure to store per-element information.
struct ElemInfo {
  void set(int st, int idt) {
    while (count < st)
      sets[count++] = 0;
    sets[count++] = idt;
  }
  int count;   // Number of valid entries in sets[]
  int sets[3]; // IDs of owning block, geom, and partition; respectively
  int id;      // Global ID of element
  int type;    // Gmsh element type
};

//! Writes out a file
ErrorCode WriteGmsh::write_file(const char *file_name,
                                const bool overwrite,
                                const FileOptions& options,
                                const EntityHandle *output_list,
                                const int num_sets,
                                const std::vector<std::string>& /* qa_list */,
                                const Tag* /* tag_list */,
                                int /* num_tags */,
                                int /* export_dimension */)
{
  ErrorCode rval;
  Tag global_id = 0, block_tag = 0, geom_tag = 0, prtn_tag = 0;

  if (!overwrite) {
    rval = mWriteIface->check_doesnt_exist(file_name);
    if (MB_SUCCESS != rval)
      return rval;
  }

  // Get tags
  mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, global_id);
  mbImpl->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER, block_tag);
  if (global_id)
    mbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER, geom_tag);
  mbImpl->tag_get_handle(PARALLEL_PARTITION_TAG_NAME, 1, MB_TYPE_INTEGER, prtn_tag);

  // Define arrays to hold entity sets of interest
  Range sets[3];
  Tag set_tags[] = {block_tag, geom_tag, prtn_tag};
  Tag set_ids[] = {block_tag, 0 /*global_id*/, prtn_tag};

  // Get entities to write
  Range elements, nodes;
  if (!output_list) {
    rval = mbImpl->get_entities_by_dimension(0, 0, nodes, false);
    if (MB_SUCCESS != rval)
      return rval;
    for (int d = 1; d <= 3; ++d) {
      Range tmp_range;
      rval = mbImpl->get_entities_by_dimension(0, d, tmp_range, false);
      if (MB_SUCCESS != rval)
        return rval;
      elements.merge(tmp_range);
    }

    for (int s = 0; s < 3; ++s) {
      if (set_tags[s]) {
        rval = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, set_tags + s, 0, 1, sets[s]);
        if (MB_SUCCESS != rval)
          return rval;
      }
    }
  }
  else {
    for (int i = 0; i < num_sets; ++i) {
      EntityHandle set = output_list[i];
      for (int d = 1; d < 3; ++d) {
        Range tmp_range, tmp_nodes;
        rval = mbImpl->get_entities_by_dimension(set, d, tmp_range, true);
        if (rval != MB_SUCCESS)
          return rval;
        elements.merge(tmp_range);
        rval = mbImpl->get_adjacencies(tmp_range, set, false, tmp_nodes);
        if (rval != MB_SUCCESS)
          return rval;
        nodes.merge(tmp_nodes);
      }

      for (int s = 0; s < 3; ++s) {
        if (set_tags[s]) {
          Range tmp_range;
          rval = mbImpl->get_entities_by_type_and_tag(set, MBENTITYSET, set_tags + s, 0, 1, tmp_range);
          if (MB_SUCCESS != rval)
            return rval;
          sets[s].merge(tmp_range);
          int junk;
          rval = mbImpl->tag_get_data(set_tags[s], &set, 1, &junk);
          if (MB_SUCCESS == rval)
            sets[s].insert(set);
        }
      }
    }
  }

  if (elements.empty()) {
    MB_SET_ERR(MB_ENTITY_NOT_FOUND, "Nothing to write");
  }

  // Get global IDs for all elements.
  // First try to get from tag.  If tag is not defined or not set
  // for all elements, use handle value instead.
  std::vector<int> global_id_array(elements.size());
  std::vector<int>::iterator id_iter;
  if (!global_id || MB_SUCCESS !=
      mbImpl->tag_get_data(global_id, elements, &global_id_array[0])) {
    id_iter = global_id_array.begin();
    for (Range::iterator i = elements.begin(); i != elements.end(); ++i, ++id_iter)
      *id_iter = mbImpl->id_from_handle(*i);
  }

  // Figure out the maximum ID value so we know where to start allocating
  // new IDs when we encounter ID conflicts.
  int max_id = 0;
  for (id_iter = global_id_array.begin(); id_iter != global_id_array.end(); ++id_iter)
    if (*id_iter > max_id)
      max_id = *id_iter;

  // Initialize ElemInfo struct for each element
  std::map<EntityHandle, ElemInfo> elem_sets; // Per-element info
  std::set<int> elem_global_ids; // Temporary for finding duplicate IDs
  id_iter = global_id_array.begin();
  // Iterate backwards to give highest-dimension entities first dibs for
  // a conflicting ID.
  for (Range::reverse_iterator i = elements.rbegin(); i != elements.rend(); ++i) {
    int id = *id_iter;
    ++id_iter;
    if (!elem_global_ids.insert(id).second)
      id = ++max_id;

    ElemInfo& ei = elem_sets[*i];
    ei.count = 0;
    ei.id = id;

    EntityType type = mbImpl->type_from_handle(*i);
    int num_vtx;
    const EntityHandle* conn;
    rval = mbImpl->get_connectivity(*i, conn, num_vtx);
    if (MB_SUCCESS != rval)
      return rval;

    ei.type = GmshUtil::get_gmsh_type(type, num_vtx);
    if (ei.type < 0) {
      MB_SET_ERR(MB_FILE_WRITE_ERROR, "Gmem file format does not support element of type " << CN::EntityTypeName(type) << " with " << num_vtx << " vertices");
    }
  }
  // Don't need these any more, free memory.
  elem_global_ids.clear();
  global_id_array.clear();

  // For each material set, geometry set, or partition; store
  // the ID of the set on each element.
  for (int s = 0; s < 3; ++s) {
    if (!set_tags[s])
      continue;

    for (Range::iterator i = sets[s].begin(); i != sets[s].end(); ++i) {
      int id;
      if (set_ids[s]) {
        rval = mbImpl->tag_get_data(set_ids[s], &*i, 1, &id);
        if (MB_SUCCESS != rval)
          return rval;
      }
      else
        id = mbImpl->id_from_handle(*i);

      Range elems;
      rval = mbImpl->get_entities_by_handle(*i, elems);
      if (MB_SUCCESS != rval)
        return rval;

      elems = intersect(elems, elements);
      for (Range::iterator j = elems.begin(); j != elems.end(); ++j)
        elem_sets[*j].set(s, id);
    }
  }

  // Create file
  std::ofstream out(file_name);
  if (!out)
    return MB_FILE_DOES_NOT_EXIST;

  // Write header
  out << "$MeshFormat" << std::endl;
  out << "2.0 0 " << sizeof(double) << std::endl;
  out << "$EndMeshFormat" << std::endl;

  // Set precision for node coordinates
  int precision;
  if (MB_SUCCESS != options.get_int_option("PRECISION", precision))
    precision = DEFAULT_PRECISION;
  const int old_precision = out.precision();
  out.precision(precision);

  // Write nodes
  out << "$Nodes" << std::endl;
  out << nodes.size() << std::endl;
  std::vector<double> coords(3*nodes.size());
  rval = mbImpl->get_coords(nodes, &coords[0]);
  if (MB_SUCCESS != rval)
    return rval;
  std::vector<double>::iterator c = coords.begin();
  for (Range::iterator i = nodes.begin(); i != nodes.end(); ++i) {
    out << mbImpl->id_from_handle(*i);
    out << " " << *c; ++c;
    out << " " << *c; ++c;
    out << " " << *c; ++c;
    out << std::endl;
  }
  out << "$EndNodes" << std::endl;
  coords.clear();

  // Restore stream state
  out.precision(old_precision);

  // Write elements
  out << "$Elements" << std::endl;
  out << elem_sets.size() << std::endl;
  for (std::map<EntityHandle, ElemInfo>::iterator i = elem_sets.begin();
       i != elem_sets.end(); ++i) {
    int num_vtx;
    const EntityHandle* conn;
    rval = mbImpl->get_connectivity(i->first, conn, num_vtx);
    if (MB_SUCCESS != rval)
      return rval;
    out << i->second.id << ' ' << i->second.type << ' ' << i->second.count;
    for (int j = 0; j < i->second.count; ++j)
      out << ' ' << i->second.sets[j];

    const int* order = GmshUtil::gmshElemTypes[i->second.type].node_order;

    // Need to re-order vertices
    if (order) {
      for (int j = 0; j < num_vtx; ++j)
        out << ' ' << mbImpl->id_from_handle(conn[order[j]]);
    }
    else {
      for (int j = 0; j < num_vtx; ++j)
        out << ' ' << mbImpl->id_from_handle(conn[j]);
    }
    out << std::endl;
  }
  out << "$EndElements" << std::endl;

  // Done
  return MB_SUCCESS;
}

} // namespace moab
