#include "moab/MergeMesh.hpp"

#include "moab/Skinner.hpp"
#include "moab/AdaptiveKDTree.hpp"
#include "moab/Range.hpp"
#include "moab/CartVect.hpp"

#include "Internals.hpp"
#include <vector>
#include <algorithm>
#include <string>
#include <vector>
#include <cassert>
#include <iostream>
#include <iomanip>

#include <stdlib.h>

namespace moab {

MergeMesh::MergeMesh(Interface *impl, bool printErrorIn) :
    mbImpl(impl), mbMergeTag(0), mergeTol(0.001), mergeTolSq(0.000001), printError(printErrorIn)
{
}

MergeMesh::~MergeMesh()
{
  if (mbMergeTag)
    mbImpl->tag_delete(mbMergeTag);
  mbMergeTag=NULL;
}


ErrorCode MergeMesh::merge_entities(EntityHandle *elems,
    int elems_size, const double merge_tol, const int do_merge,
    const int update_sets, Tag merge_tag, bool do_higher_dim)
{
  mergeTol = merge_tol;
  mergeTolSq = merge_tol * merge_tol;
  Range tmp_elems;
  tmp_elems.insert(elems, elems + elems_size);
  ErrorCode result = merge_entities(tmp_elems, merge_tol, do_merge,
      update_sets, (Tag) merge_tag, do_higher_dim);

  return result;
}

/*  This function appears to be not necessary after MOAB conversion

 void MergeMesh::perform_merge(iBase_TagHandle merge_tag)
 {
 // put into a range
 ErrorCode result = perform_merge((Tag) merge_tag);
 if (result != MB_SUCCESS)
 throw MKException(iBase_FAILURE, "");
 }*/

ErrorCode MergeMesh::merge_entities(Range &elems,
    const double merge_tol, const int do_merge, const int, Tag merge_tag,
    bool merge_higher_dim)
{
  //If merge_higher_dim is true, do_merge must also be true
  if (merge_higher_dim && !do_merge)
  {
    return MB_FAILURE;
  }

  mergeTol = merge_tol;
  mergeTolSq = merge_tol * merge_tol;

  // get the skin of the entities
  Skinner skinner(mbImpl);
  Range skin_range;
  ErrorCode result = skinner.find_skin(0, elems, 0, skin_range, false,
      false);
  if (MB_SUCCESS != result)
    return result;

  // create a tag to mark merged-to entity; reuse tree_root
  EntityHandle tree_root = 0;
  if (0 == merge_tag)
  {
    result = mbImpl->tag_get_handle("__merge_tag", 1, MB_TYPE_HANDLE,
        mbMergeTag, MB_TAG_DENSE | MB_TAG_EXCL, &tree_root);
    if (MB_SUCCESS != result)
      return result;
  }
  else
    mbMergeTag = merge_tag;

  // build a kd tree with the vertices
  AdaptiveKDTree kd(mbImpl);
  result = kd.build_tree(skin_range, &tree_root);
  if (MB_SUCCESS != result)
    return result;

  // find matching vertices, mark them
  result = find_merged_to(tree_root, kd, mbMergeTag);
  if (MB_SUCCESS != result)
    return result;

  // merge them if requested
  if (do_merge)
  {
    result = perform_merge(mbMergeTag);
    if (MB_SUCCESS != result)
      return result;
  }

  if (merge_higher_dim && deadEnts.size() != 0)
  {
    result = merge_higher_dimensions(elems);
    if (MB_SUCCESS != result)
      return result;
  }

  return MB_SUCCESS;
}

ErrorCode MergeMesh::merge_all(EntityHandle meshset, const double merge_tol)
{
  ErrorCode rval;
  if (0 == mbMergeTag)
  {
    EntityHandle def_val = 0;
    rval = mbImpl->tag_get_handle("__merge_tag", 1, MB_TYPE_HANDLE,
        mbMergeTag, MB_TAG_DENSE | MB_TAG_EXCL, &def_val);MB_CHK_ERR(rval);
  }
  // get all entities;
  // get all vertices connected
  // build a kdtree
  // find merged to
  mergeTol = merge_tol;
  mergeTolSq = merge_tol * merge_tol;

  // get all vertices
  Range entities;
  rval = mbImpl->get_entities_by_handle(meshset, entities, /*recursive*/ true);MB_CHK_ERR(rval);
  Range sets= entities.subset_by_type(MBENTITYSET);
  entities=subtract(entities, sets);
  Range verts;
  rval = mbImpl->get_connectivity(entities, verts); MB_CHK_ERR(rval);

  // build a kd tree with the vertices
  AdaptiveKDTree kd(mbImpl);
  EntityHandle tree_root;
  rval = kd.build_tree(verts, &tree_root);MB_CHK_ERR(rval);

  // find matching vertices, mark them
  rval = find_merged_to(tree_root, kd, mbMergeTag); MB_CHK_ERR(rval);

  rval = perform_merge(mbMergeTag); MB_CHK_ERR(rval);

  if ( deadEnts.size() != 0)
  {
    rval = merge_higher_dimensions(entities);MB_CHK_ERR(rval);
  }
  return MB_SUCCESS;
}
ErrorCode MergeMesh::perform_merge(Tag merge_tag)
{
  // we start with an empty range of vertices that are "merged to"
  // they are used (eventually) for higher dim entities
  mergedToVertices.clear();
  ErrorCode result;
  if (deadEnts.size() == 0)
  {
    if (printError)
      std::cout
          << "\nWarning: Geometries don't have a common face; Nothing to merge"
          << std::endl;
    return MB_SUCCESS; //nothing to merge carry on with the program
  }
  if (mbImpl->type_from_handle(*deadEnts.rbegin()) != MBVERTEX)
    return MB_FAILURE;
  std::vector<EntityHandle> merge_tag_val(deadEnts.size());
  result = mbImpl->tag_get_data(merge_tag, deadEnts, &merge_tag_val[0]);
  if (MB_SUCCESS != result)
    return result;

  Range::iterator rit;
  unsigned int i;
  for (rit = deadEnts.begin(), i = 0; rit != deadEnts.end(); ++rit, i++)
  {
    assert(merge_tag_val[i]);
    if (MBVERTEX==TYPE_FROM_HANDLE(merge_tag_val[i]) )
      mergedToVertices.insert(merge_tag_val[i]);
    result = mbImpl->merge_entities(merge_tag_val[i], *rit, false, false);
    if (MB_SUCCESS != result)
    {
      return result;
    }
  }
  result = mbImpl->delete_entities(deadEnts);
  return result;
}
// merge vertices according to an input tag
// merge them if the tags are equal
struct handle_id
{
  EntityHandle eh;
  int val;
};

// handle structure comparison function for qsort
// if the id is the same , compare the handle.
int compare_handle_id(const void * a, const void * b) {

  handle_id * ia = (handle_id*) a;
  handle_id * ib = (handle_id*) b;
  if(ia->val == ib->val) {
    return (ia->eh<ib->eh)? -1 : 1;
  } else {
    return (ia->val - ib->val);
  }
}

ErrorCode MergeMesh::merge_using_integer_tag(Range & verts, Tag user_tag, Tag merge_tag)
{
  ErrorCode rval;
  DataType tag_type;
  rval = mbImpl->tag_get_data_type(user_tag, tag_type);
  if (rval!=MB_SUCCESS || tag_type!=MB_TYPE_INTEGER)
    return MB_FAILURE;

  std::vector<int> vals(verts.size());
  rval = mbImpl->tag_get_data(user_tag, verts, &vals[0]);
  if (rval!=MB_SUCCESS)
    return rval;

  if (0 == merge_tag)
  {
    EntityHandle def_val = 0;
    rval = mbImpl->tag_get_handle("__merge_tag", 1, MB_TYPE_HANDLE, mbMergeTag,
        MB_TAG_DENSE | MB_TAG_EXCL, &def_val);
    if (MB_SUCCESS != rval)
      return rval;
  }
  else
    mbMergeTag = merge_tag;

  std::vector<handle_id>  handles(verts.size());
  int i=0;
  for (Range::iterator vit = verts.begin(); vit!= verts.end(); ++vit)
  {
    handles[i].eh=*vit;
    handles[i].val = vals[i];
    i++;
  }
  //std::sort(handles.begin(), handles.end(), compare_handle_id);
  qsort(&handles[0], handles.size(), sizeof(handle_id), compare_handle_id);
  i=0;
  while (i<(int)verts.size()-1)
  {
    handle_id  first = handles[i];
    int j=i+1;
    while (handles[j].val == first.val && j<(int)verts.size())
    {
      rval= mbImpl->tag_set_data(mbMergeTag, &(handles[j].eh), 1, &(first.eh));
      if (rval!=MB_SUCCESS)
        return rval;
      deadEnts.insert(handles[j].eh);
      j++;
    }
    i=j;
  }

  rval = perform_merge(mbMergeTag);

  return rval;
}
ErrorCode MergeMesh::find_merged_to(EntityHandle &tree_root,
    AdaptiveKDTree &tree, Tag merge_tag)
{
  AdaptiveKDTreeIter iter;

  // evaluate vertices in this leaf
  Range leaf_range, leaf_range2;
  std::vector<EntityHandle> sorted_leaves;
  std::vector<double> coords;
  std::vector<EntityHandle> merge_tag_val, leaves_out;

  ErrorCode result = tree.get_tree_iterator(tree_root, iter);
  if (MB_SUCCESS != result)
    return result;
  while (result == MB_SUCCESS)
  {
    sorted_leaves.push_back(iter.handle());
    result = iter.step();
  }
  if (result != MB_ENTITY_NOT_FOUND)
    return result;
  std::sort(sorted_leaves.begin(), sorted_leaves.end());

  std::vector<EntityHandle>::iterator it;
  for (it = sorted_leaves.begin(); it != sorted_leaves.end(); ++it)
  {

    leaf_range.clear();
    result = mbImpl->get_entities_by_handle(*it, leaf_range);
    if (MB_SUCCESS != result)
      return result;
    coords.resize(3 * leaf_range.size());
    merge_tag_val.resize(leaf_range.size());
    result = mbImpl->get_coords(leaf_range, &coords[0]);
    if (MB_SUCCESS != result)
      return result;
    result = mbImpl->tag_get_data(merge_tag, leaf_range, &merge_tag_val[0]);
    if (MB_SUCCESS != result)
      return result;
    Range::iterator rit;
    unsigned int i;
    bool inleaf_merged, outleaf_merged = false;
    unsigned int lr_size = leaf_range.size();

    for (i = 0, rit = leaf_range.begin(); i != lr_size; ++rit, i++)
    {
      if (0 != merge_tag_val[i])
        continue;
      CartVect from(&coords[3 * i]);
      inleaf_merged = false;

      // check close-by leaves too
      leaves_out.clear();
      result = tree.distance_search(from.array(), mergeTol, leaves_out,
          mergeTol, 1.0e-6, NULL, NULL, &tree_root);
      leaf_range2.clear();
      for (std::vector<EntityHandle>::iterator vit = leaves_out.begin();
          vit != leaves_out.end(); ++vit)
      {
        if (*vit > *it)
        { // if we haven't visited this leaf yet in the outer loop
          result = mbImpl->get_entities_by_handle(*vit, leaf_range2,
              Interface::UNION);
          if (MB_SUCCESS != result)
            return result;
        }
      }
      if (!leaf_range2.empty())
      {
        coords.resize(3 * (lr_size + leaf_range2.size()));
        merge_tag_val.resize(lr_size + leaf_range2.size());
        result = mbImpl->get_coords(leaf_range2, &coords[3 * lr_size]);
        if (MB_SUCCESS != result)
          return result;
        result = mbImpl->tag_get_data(merge_tag, leaf_range2,
            &merge_tag_val[lr_size]);
        if (MB_SUCCESS != result)
          return result;
        outleaf_merged = false;
      }

      // check other verts in this leaf
      for (unsigned int j = i + 1; j < merge_tag_val.size(); j++)
      {
        EntityHandle to_ent =
            j >= lr_size ? leaf_range2[j - lr_size] : leaf_range[j];

        if (*rit == to_ent)
          continue;

        if ((from - CartVect(&coords[3 * j])).length_squared()
            < mergeTolSq)
        {
          merge_tag_val[j] = *rit;
          if (j < lr_size)
          {
            inleaf_merged = true;
          }
          else
          {
            outleaf_merged = true;
          }
          deadEnts.insert(to_ent);
        }

      }
      if (outleaf_merged)
      {
        result = mbImpl->tag_set_data(merge_tag, leaf_range2,
            &merge_tag_val[leaf_range.size()]);
        if (MB_SUCCESS != result)
          return result;
        outleaf_merged = false;
      }
      if (inleaf_merged)
      {
        result = mbImpl->tag_set_data(merge_tag, leaf_range, &merge_tag_val[0]);
        if (MB_SUCCESS != result)
          return result;
      }

    }
  }
  return MB_SUCCESS;
}

//Determine which higher dimensional entities should be merged
ErrorCode MergeMesh::merge_higher_dimensions(Range &elems)
{
  // apply a different strategy
  // look at the vertices that were merged to, earlier, and find all entities adjacent to them
  // elems (input) are used just for initial connectivity
  ErrorCode result;
  Range verts;
  result = mbImpl->get_connectivity(elems, verts);
  if (MB_SUCCESS!=result)
    return result;

  // all higher dim entities that will be merged will be connected to the vertices that were
  // merged earlier; we will look at these vertices only
  Range vertsOfInterest=intersect(this->mergedToVertices, verts);
  //Go through each dimension
  Range possibleEntsToMerge, conn, matches, moreDeadEnts;

  for (int dim = 1; dim < 3; dim++)
  {
    moreDeadEnts.clear();
    possibleEntsToMerge.clear();
    result = mbImpl->get_adjacencies(vertsOfInterest,
                                             dim, false, possibleEntsToMerge,
                                             Interface::UNION);
    if (MB_SUCCESS!=result)
      return result;
    //Go through each possible entity and see if it shares vertices with another entity of same dimension
    for (Range::iterator pit = possibleEntsToMerge.begin();
        pit != possibleEntsToMerge.end(); ++pit)
    {
      EntityHandle eh=*pit;//possible entity to be matched
      conn.clear();
      //Get the vertices connected to it in a range

      result = mbImpl->get_connectivity(&eh, 1, conn);
      if (MB_SUCCESS!=result)
        return result;
      matches.clear();
      // now retrieve all entities connected to all conn vertices
      result = mbImpl->get_adjacencies(conn, dim, false, matches,
                                                   Interface::INTERSECT);
      if (MB_SUCCESS!=result)
        return result;
      if (matches.size() > 1)
      {
        for (Range::iterator matchIt = matches.begin();
            matchIt != matches.end(); ++matchIt)
        {
          EntityHandle to_remove=*matchIt;
          if (to_remove != eh)
          {
            moreDeadEnts.insert(to_remove);
            result = mbImpl->merge_entities(eh, to_remove, false, false);
            if (result != MB_SUCCESS)
              return result;
            possibleEntsToMerge.erase(to_remove);
          }
        }
      }

    }
    //Delete the entities of dimension dim
    result = mbImpl->delete_entities(moreDeadEnts);
    if (result != MB_SUCCESS)
      return result;
  }
  return MB_SUCCESS;
#if 0
  Range skinEnts, adj, matches, moreDeadEnts;
  ErrorCode result;
  Skinner skinner(mbImpl);
  //Go through each dimension
  for (int dim = 1; dim < 3; dim++)
  {
    skinEnts.clear();
    moreDeadEnts.clear();
    result = skinner.find_skin(0, elems, dim, skinEnts, false, false);
    //Go through each skin entity and see if it shares adjacancies with another entity
    for (Range::iterator skinIt = skinEnts.begin();
        skinIt != skinEnts.end(); ++skinIt)
    {
      adj.clear();
      //Get the adjacencies 1 dimension lower
      result = mbImpl->get_adjacencies(&(*skinIt), 1, dim - 1, false, adj);
      if (result != MB_SUCCESS)
        return result;
      //See what other entities share these adjacencies
      matches.clear();
      result = mbImpl->get_adjacencies(adj, dim, false, matches,
          Interface::INTERSECT);
      if (result != MB_SUCCESS)
        return result;
      //If there is more than one entity, then we have some to merge and erase
      if (matches.size() > 1)
      {
        for (Range::iterator matchIt = matches.begin();
            matchIt != matches.end(); ++matchIt)
        {
          if (*matchIt != *skinIt)
          {
            moreDeadEnts.insert(*matchIt);
            result = mbImpl->merge_entities(*skinIt, *matchIt, false, false);
            if (result != MB_SUCCESS)
              return result;
            skinEnts.erase(*matchIt);
          }
        }
      }
    }
    //Delete the entities
    result = mbImpl->delete_entities(moreDeadEnts);
    if (result != MB_SUCCESS)
      return result;
  }
  return MB_SUCCESS;
#endif
}

} //End namespace moab
