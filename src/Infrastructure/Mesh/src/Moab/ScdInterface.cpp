#include "moab/ScdInterface.hpp"
#include "moab/Core.hpp"
#include "SequenceManager.hpp"
#include "EntitySequence.hpp"
#include "StructuredElementSeq.hpp"
#include "VertexSequence.hpp"
#include "ScdVertexData.hpp"
#include "MBTagConventions.hpp"
#ifdef MOAB_HAVE_MPI
#  include "moab/ParallelComm.hpp"
#  include "moab/TupleList.hpp"
#  include "moab/gs.hpp"
#endif
#include "assert.h"
#include <iostream>
#include <functional>

#define ERRORR(rval, str) {if (MB_SUCCESS != rval)          \
      {std::cerr << str; return rval; }}

        
namespace moab 
{

const char *ScdParData::PartitionMethodNames[] = {"alljorkori", "alljkbal", "sqij", "sqjk", "sqijk", "trivial","rcbzoltan", "nopart"};

ScdInterface::ScdInterface(Interface *imp, bool boxes) 
        : mbImpl(imp), 
          searchedBoxes(false),
          boxPeriodicTag(0),
          boxDimsTag(0),
          globalBoxDimsTag(0),
          partMethodTag(0),
          boxSetTag(0)
{
  if (boxes) find_boxes(scdBoxes);
}

  // Destructor
ScdInterface::~ScdInterface() 
{
  std::vector<ScdBox*> tmp_boxes;
  tmp_boxes.swap(scdBoxes);

  for (std::vector<ScdBox*>::iterator rit = tmp_boxes.begin(); rit != tmp_boxes.end(); ++rit)
    delete *rit;

  if (box_set_tag(false)) 
    mbImpl->tag_delete(box_set_tag());

}

Interface *ScdInterface::impl() const
{
  return mbImpl;
}

ErrorCode ScdInterface::find_boxes(std::vector<ScdBox*> &scd_boxes) 
{
  Range tmp_boxes;
  ErrorCode rval = find_boxes(tmp_boxes);
  if (MB_SUCCESS != rval) return rval;

  for (Range::iterator rit = tmp_boxes.begin(); rit != tmp_boxes.end(); ++rit) {
    ScdBox *tmp_box = get_scd_box(*rit);
    if (tmp_box) scd_boxes.push_back(tmp_box);
    else rval = MB_FAILURE;
  }

  return rval;
}

ErrorCode ScdInterface::find_boxes(Range &scd_boxes) 
{
  ErrorCode rval = MB_SUCCESS;
  box_dims_tag();
  Range boxes;
  if (!searchedBoxes) {
    rval = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &boxDimsTag, NULL, 1, 
                                                boxes, Interface::UNION);
    searchedBoxes = true;
    if (!boxes.empty()) {
      scdBoxes.resize(boxes.size());
      rval = mbImpl->tag_get_data(boxSetTag, boxes, &scdBoxes[0]);
      ScdBox *dum = NULL;
      std::remove_if(scdBoxes.begin(), scdBoxes.end(), std::bind2nd(std::equal_to<ScdBox*>(), dum) ) ;
    }
  }

  for (std::vector<ScdBox*>::iterator vit = scdBoxes.begin(); vit != scdBoxes.end(); ++vit)
    scd_boxes.insert((*vit)->box_set());

  return rval;
}

ScdBox *ScdInterface::get_scd_box(EntityHandle eh) 
{
  ScdBox *scd_box = NULL;
  if (!box_set_tag(false)) return scd_box;

  mbImpl->tag_get_data(box_set_tag(), &eh, 1, &scd_box);
  return scd_box;
}

ErrorCode ScdInterface::construct_box(HomCoord low, HomCoord high, const double * const coords, unsigned int num_coords,
                                      ScdBox *& new_box, int * const lperiodic, ScdParData *par_data,
                                      bool assign_gids, int tag_shared_ents)
{
    // create a rectangular structured mesh block
  ErrorCode rval;

  int tmp_lper[3] = {0, 0, 0};
  if (lperiodic) std::copy(lperiodic, lperiodic+3, tmp_lper);
  
#ifndef MOAB_HAVE_MPI
  if (-1 != tag_shared_ents) ERRORR(MB_FAILURE, "Parallel capability requested but MOAB not compiled parallel.");
  if (-1 == tag_shared_ents && !assign_gids) assign_gids = true; // need to assign gids in order to tag shared verts
#else
  if (par_data && low == high && ScdParData::NOPART != par_data->partMethod) {
      // requesting creation of parallel mesh, so need to compute partition
    if (!par_data->pComm) {
        // this is a really boneheaded way to have to create a PC
      par_data->pComm = ParallelComm::get_pcomm(mbImpl, 0);
      if (NULL == par_data->pComm) par_data->pComm = new ParallelComm(mbImpl, MPI_COMM_WORLD);
    }
    int ldims[6];
    rval = compute_partition(par_data->pComm->size(), par_data->pComm->rank(), *par_data, 
                             ldims, tmp_lper, par_data->pDims);
    ERRORR(rval, "Error returned from compute_partition.");
    low.set(ldims[0],ldims[1],ldims[2]);
    high.set(ldims[3],ldims[4],ldims[5]);
    if (par_data->pComm->get_debug_verbosity() > 0) {
      std::cout << "Proc " << par_data->pComm->rank() << ": " << *par_data;
      std::cout << "Proc " << par_data->pComm->rank() << " local dims: " 
                << low << "-" << high << std::endl;
    }
  }
#endif
  
  HomCoord tmp_size = high - low + HomCoord(1, 1, 1, 0);
  if ((tmp_size[1] && num_coords && (int)num_coords < tmp_size[0]) ||
      (tmp_size[2] && num_coords && (int)num_coords < tmp_size[0]*tmp_size[1]))
    return MB_FAILURE;

  rval = create_scd_sequence(low, high, MBVERTEX, 0, new_box);
  ERRORR(rval, "Trouble creating scd vertex sequence.");

    // set the vertex coordinates
  double *xc, *yc, *zc;
  rval = new_box->get_coordinate_arrays(xc, yc, zc);
  ERRORR(rval, "Couldn't get vertex coordinate arrays.");

  if (coords && num_coords) {
    unsigned int i = 0;
    for (int kl = low[2]; kl <= high[2]; kl++) {
      for (int jl = low[1]; jl <= high[1]; jl++) {
        for (int il = low[0]; il <= high[0]; il++) {
          xc[i] = coords[3*i];
          if (new_box->box_size()[1])
            yc[i] = coords[3*i+1];
          if (new_box->box_size()[2])
            zc[i] = coords[3*i+2];
          i++;
        }
      }
    }
  }
  else {
    unsigned int i = 0;
    for (int kl = low[2]; kl <= high[2]; kl++) {
      for (int jl = low[1]; jl <= high[1]; jl++) {
        for (int il = low[0]; il <= high[0]; il++) {
          xc[i] = (double) il;
          if (new_box->box_size()[1])
            yc[i] = (double) jl;
          else yc[i] = 0.0;
          if (new_box->box_size()[2])
            zc[i] = (double) kl;
          else zc[i] = 0.0;
          i++;
        }
      }
    }
  }

    // create element sequence
  Core *mbcore = dynamic_cast<Core*>(mbImpl);
  SequenceManager *seq_mgr = mbcore->sequence_manager();

  EntitySequence *tmp_seq;
  EntityHandle start_ent;

    // construct the sequence
  EntityType this_tp = MBHEX;
  if (1 >= tmp_size[2]) this_tp = MBQUAD;
  if (1 >= tmp_size[2] && 1 >= tmp_size[1]) this_tp = MBEDGE;
  rval = seq_mgr->create_scd_sequence(low, high, this_tp, 0, start_ent, tmp_seq, 
                                      tmp_lper);
  ERRORR(rval, "Trouble creating scd element sequence.");

  new_box->elem_seq(tmp_seq);
  new_box->start_element(start_ent);

    // add vertex seq to element seq, forward orientation, unity transform
  rval = new_box->add_vbox(new_box,
                             // p1: imin,jmin
                           low, low, 
                             // p2: imax,jmin
                           low + HomCoord(1, 0, 0),
                           low + HomCoord(1, 0, 0),
                             // p3: imin,jmax
                           low + HomCoord(0, 1, 0),
                           low + HomCoord(0, 1, 0));
  ERRORR(rval, "Error constructing structured element sequence.");

    // add the new hexes to the scd box set; vertices were added in call to create_scd_sequence
  Range tmp_range(new_box->start_element(), new_box->start_element() + new_box->num_elements() - 1);
  rval = mbImpl->add_entities(new_box->box_set(), tmp_range);
  ERRORR(rval, "Couldn't add new hexes to box set.");

  if (par_data) new_box->par_data(*par_data);
  

  if (assign_gids) {
    rval = assign_global_ids(new_box);
    ERRORR(rval, "Trouble assigning global ids");
  }

#ifdef MOAB_HAVE_MPI
  if (par_data && -1 != tag_shared_ents) {
    rval = tag_shared_vertices(par_data->pComm, new_box);
  }
#endif
  
  return MB_SUCCESS;
}

ErrorCode ScdInterface::assign_global_ids(ScdBox *box)
{
  // Get a ptr to global id memory
  void* data;
  int count = 0;
  Tag gid_tag;
  ErrorCode rval = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, gid_tag, 
                                          MB_TAG_CREAT & MB_TAG_DENSE, &count);
  ERRORR(rval, "Trouble getting global id tag handle.");
  Range tmp_range(box->start_vertex(), box->start_vertex() + box->num_vertices());
  rval = mbImpl->tag_iterate(gid_tag, tmp_range.begin(), tmp_range.end(), count, data);
  ERRORR(rval, "Failed to get tag iterator.");
  assert(count == box->num_vertices());
  int* gid_data = (int*) data;
  int di = box->par_data().gDims[3] - box->par_data().gDims[0] + 1;
  int dj = box->par_data().gDims[4] - box->par_data().gDims[1] + 1;

  for (int kl = box->box_dims()[2]; kl <= box->box_dims()[5]; kl++) {
    for (int jl = box->box_dims()[1]; jl <= box->box_dims()[4]; jl++) {
      for (int il = box->box_dims()[0]; il <= box->box_dims()[3]; il++) {
        int itmp = (!box->locally_periodic()[0] && box->par_data().gPeriodic[0] && il == box->par_data().gDims[3] ? 
                    box->par_data().gDims[0] : il);
        *gid_data = (-1 != kl ? kl * di * dj : 0) + jl * di + itmp + 1;
        gid_data++;
      }
    }
  }

  return MB_SUCCESS;
}

ErrorCode ScdInterface::create_scd_sequence(const HomCoord &low, const HomCoord &high, EntityType tp,
                                            int starting_id, ScdBox *&new_box,
                                            int *is_periodic)
{
  HomCoord tmp_size = high - low + HomCoord(1, 1, 1, 0);
  if ((tp == MBHEX && 1 >= tmp_size[2])  ||
      (tp == MBQUAD && 1 >= tmp_size[1]) || 
      (tp == MBEDGE && 1 >= tmp_size[0]))
    return MB_TYPE_OUT_OF_RANGE;

  Core *mbcore = dynamic_cast<Core*>(mbImpl);
  assert(mbcore != NULL);
  SequenceManager *seq_mgr = mbcore->sequence_manager();

  EntitySequence *tmp_seq;
  EntityHandle start_ent, scd_set;

    // construct the sequence
  ErrorCode rval = seq_mgr->create_scd_sequence(low, high, tp, starting_id, start_ent, tmp_seq,
                                                is_periodic);
  if (MB_SUCCESS != rval) return rval;

    // create the set for this rectangle
  rval = create_box_set(low, high, scd_set);
  if (MB_SUCCESS != rval) return rval;

    // make the ScdBox
  new_box = new ScdBox(this, scd_set, tmp_seq);
  if (!new_box) return MB_FAILURE;

    // set the start vertex/element
  Range new_range;
  if (MBVERTEX == tp) {
    new_range.insert(start_ent, start_ent+new_box->num_vertices()-1);
  }
  else {
    new_range.insert(start_ent, start_ent+new_box->num_elements()-1);
  }

    // put the entities in the box set
  rval = mbImpl->add_entities(scd_set, new_range);
  if (MB_SUCCESS != rval) return rval;

    // tag the set with the box
  rval = mbImpl->tag_set_data(box_set_tag(), &scd_set, 1, &new_box);
  if (MB_SUCCESS != rval) return rval;

  return MB_SUCCESS;
}

ErrorCode ScdInterface::create_box_set(const HomCoord &low, const HomCoord &high,
                                       EntityHandle &scd_set, int *is_periodic) 
{
    // create the set and put the entities in it
  ErrorCode rval = mbImpl->create_meshset(MESHSET_SET, scd_set);
  if (MB_SUCCESS != rval) return rval;

    // tag the set with parameter extents
  int boxdims[6];
  for (int i = 0; i < 3; i++) boxdims[i] = low[i];
  for (int i = 0; i < 3; i++) boxdims[3+i] = high[i];
  rval = mbImpl->tag_set_data(box_dims_tag(), &scd_set, 1, boxdims);
  if (MB_SUCCESS != rval) return rval;

  if (is_periodic) {
    rval = mbImpl->tag_set_data(box_periodic_tag(), &scd_set, 1, is_periodic);
    if (MB_SUCCESS != rval) return rval;
  }

  return rval;
}

Tag ScdInterface::box_periodic_tag(bool create_if_missing) 
{
  // Reset boxPeriodicTag in case it has been deleted (e.g. by Core::clean_up_failed_read)
  if (boxPeriodicTag) {
    std::string tag_name;
    if (MB_TAG_NOT_FOUND == mbImpl->tag_get_name(boxPeriodicTag, tag_name))
      boxPeriodicTag = NULL;
  }

  if (boxPeriodicTag || !create_if_missing) return boxPeriodicTag;

  ErrorCode rval = mbImpl->tag_get_handle("BOX_PERIODIC", 3, MB_TYPE_INTEGER, 
                                          boxPeriodicTag, MB_TAG_SPARSE|MB_TAG_CREAT);
  if (MB_SUCCESS != rval) return 0;
  return boxPeriodicTag;
}

Tag ScdInterface::box_dims_tag(bool create_if_missing) 
{
  // Reset boxDimsTag in case it has been deleted (e.g. by clean_up_failed_read)
  if (boxDimsTag) {
    std::string tag_name;
    if (MB_TAG_NOT_FOUND == mbImpl->tag_get_name(boxDimsTag, tag_name))
      boxDimsTag = NULL;
  }

  if (boxDimsTag || !create_if_missing) return boxDimsTag;

  ErrorCode rval = mbImpl->tag_get_handle("BOX_DIMS", 6, MB_TYPE_INTEGER, 
                                          boxDimsTag, MB_TAG_SPARSE|MB_TAG_CREAT);
  if (MB_SUCCESS != rval) return 0;
  return boxDimsTag;
}

Tag ScdInterface::global_box_dims_tag(bool create_if_missing) 
{
  // Reset globalBoxDimsTag in case it has been deleted (e.g. by Core::clean_up_failed_read)
  if (globalBoxDimsTag) {
    std::string tag_name;
    if (MB_TAG_NOT_FOUND == mbImpl->tag_get_name(globalBoxDimsTag, tag_name))
      globalBoxDimsTag = NULL;
  }

  if (globalBoxDimsTag || !create_if_missing) return globalBoxDimsTag;

  ErrorCode rval = mbImpl->tag_get_handle("GLOBAL_BOX_DIMS", 6, MB_TYPE_INTEGER, 
                                          globalBoxDimsTag, MB_TAG_SPARSE|MB_TAG_CREAT);
  if (MB_SUCCESS != rval) return 0;
  return globalBoxDimsTag;
}

Tag ScdInterface::part_method_tag(bool create_if_missing) 
{
  // Reset partMethodTag in case it has been deleted (e.g. by Core::clean_up_failed_read)
  if (partMethodTag) {
    std::string tag_name;
    if (MB_TAG_NOT_FOUND == mbImpl->tag_get_name(partMethodTag, tag_name))
      partMethodTag = NULL;
  }

  if (partMethodTag || !create_if_missing) return partMethodTag;

  ErrorCode rval = mbImpl->tag_get_handle("PARTITION_METHOD", 1, MB_TYPE_INTEGER, 
                                          partMethodTag, MB_TAG_SPARSE|MB_TAG_CREAT);
  if (MB_SUCCESS != rval) return 0;
  return partMethodTag;
}

Tag ScdInterface::box_set_tag(bool create_if_missing) 
{
  // Reset boxSetTag in case it has been deleted (e.g. by Core::clean_up_failed_read)
  if (boxSetTag) {
    std::string tag_name;
    if (MB_TAG_NOT_FOUND == mbImpl->tag_get_name(boxSetTag, tag_name))
      boxSetTag = NULL;
  }

  if (boxSetTag || !create_if_missing) return boxSetTag;

  ErrorCode rval = mbImpl->tag_get_handle("__BOX_SET", sizeof(ScdBox*), MB_TYPE_OPAQUE,
                                          boxSetTag, MB_TAG_SPARSE|MB_TAG_CREAT);
  if (MB_SUCCESS != rval) return 0;
  return boxSetTag;
}

  //! Remove the box from the list on ScdInterface
ErrorCode ScdInterface::remove_box(ScdBox *box) 
{
  std::vector<ScdBox*>::iterator vit = std::find(scdBoxes.begin(), scdBoxes.end(), box);
  if (vit != scdBoxes.end()) {
    scdBoxes.erase(vit);
    return MB_SUCCESS;
  }
  else return MB_FAILURE;
}

  //! Add the box to the list on ScdInterface
ErrorCode ScdInterface::add_box(ScdBox *box) 
{
  scdBoxes.push_back(box);
  return MB_SUCCESS;
}

ErrorCode ScdInterface::get_boxes(std::vector<ScdBox*> &boxes) 
{
  std::copy(scdBoxes.begin(), scdBoxes.end(), std::back_inserter(boxes));
  return MB_SUCCESS;
}

ScdBox::ScdBox(ScdInterface *impl, EntityHandle bset,
               EntitySequence *seq1, EntitySequence *seq2) 
        : scImpl(impl), boxSet(bset), vertDat(NULL), elemSeq(NULL), startVertex(0), startElem(0)
{
  for (int i = 0; i < 6; i++) boxDims[i] = 0;
  for (int i = 0; i < 3; i++) locallyPeriodic[i] = false;
  VertexSequence *vseq = dynamic_cast<VertexSequence *>(seq1);
  if (vseq) vertDat = dynamic_cast<ScdVertexData*>(vseq->data());
  if (vertDat) {
      // retrieve the parametric space
    for (int i = 0; i < 3; i++) {
      boxDims[i] = vertDat->min_params()[i];
      boxDims[3+i] = vertDat->max_params()[i];
    }
    startVertex = vertDat->start_handle();
  }
  else if (impl->boxDimsTag) {
      // look for parametric space info on set
    ErrorCode rval = impl->mbImpl->tag_get_data(impl->boxDimsTag, &bset, 1, boxDims);
    if (MB_SUCCESS == rval) {
      Range verts;
      impl->mbImpl->get_entities_by_dimension(bset, 0, verts);
      if (!verts.empty()) startVertex = *verts.begin();
    }
  }

  elemSeq = dynamic_cast<StructuredElementSeq *>(seq2);
  if (!elemSeq)
    elemSeq = dynamic_cast<StructuredElementSeq *>(seq1);

  if (elemSeq) {
    if (vertDat) {
        // check the parametric space to make sure it's consistent
      assert(elemSeq->sdata()->min_params() == HomCoord(boxDims, 3) && 
             (elemSeq->sdata()->max_params() + HomCoord(1, 1, 1)) == HomCoord(boxDims, 3));

    } 
    else {
        // get the parametric space from the element sequence
      for (int i = 0; i < 3; i++) {
        boxDims[i] = elemSeq->sdata()->min_params()[i];
        boxDims[3+i] = elemSeq->sdata()->max_params()[i];
      }
    }

    startElem = elemSeq->start_handle();
  }
  else {
    Range elems;
    impl->mbImpl->get_entities_by_dimension(bset, (boxDims[2] == boxDims[5] ? (boxDims[1] == boxDims[4] ? 1 : 2) : 3), elems);
    if (!elems.empty()) startElem = *elems.begin();
      // call the following w/o looking at return value, since it doesn't really need to be there
    if (impl->boxPeriodicTag) 
      impl->mbImpl->tag_get_data(impl->boxPeriodicTag, &bset, 1, locallyPeriodic);
  }

  assert(vertDat || elemSeq || 
         boxDims[0] != boxDims[3]|| boxDims[1] != boxDims[4]|| boxDims[2] != boxDims[5]);

  boxSize = HomCoord(boxDims+3, 3) - HomCoord(boxDims, 3) + HomCoord(1, 1, 1);
  boxSizeIJ = (boxSize[1] ? boxSize[1] : 1) * boxSize[0];
  boxSizeIM1 = boxSize[0]-(locallyPeriodic[0] ? 0 : 1);
  boxSizeIJM1 = (boxSize[1] ? (boxSize[1]-(locallyPeriodic[1] ? 0 : 1)) : 1) * boxSizeIM1;

  scImpl->add_box(this);
}

ScdBox::~ScdBox() 
{
  // Reset the tag on the set
  if (boxSet) {
    // It is possible that the box set entity has been deleted (e.g. by Core::clean_up_failed_read)
    Core* mbcore = dynamic_cast<Core*>(scImpl->mbImpl);
    assert(mbcore != NULL);
    if (mbcore->is_valid(boxSet)) {
      ScdBox* tmp_ptr = NULL;
      scImpl->mbImpl->tag_set_data(scImpl->box_set_tag(), &boxSet, 1, &tmp_ptr);
    }
    else
      boxSet = 0;
  }

  scImpl->remove_box(this);
}

EntityHandle ScdBox::get_vertex_from_seq(int i, int j, int k) const
{
  assert(elemSeq);
  return elemSeq->get_vertex(i, j, k);
}

int ScdBox::box_dimension() const
{
  return (startElem ? scImpl->mbImpl->dimension_from_handle(startElem) : -1);
}

ErrorCode ScdBox::add_vbox(ScdBox *vbox,
                           HomCoord from1, HomCoord to1, 
                           HomCoord from2, HomCoord to2,
                           HomCoord from3, HomCoord to3,
                           bool bb_input,
                           const HomCoord &bb_min,
                           const HomCoord &bb_max)
{
  if (!vbox->vertDat) return MB_FAILURE;
  ScdVertexData *dum_data = dynamic_cast<ScdVertexData*>(vbox->vertDat);
  ErrorCode rval = elemSeq->sdata()->add_vsequence(dum_data, from1, to1, from2, to2, from3, to3,
                                                   bb_input, bb_min, bb_max);
  return rval;
}

bool ScdBox::boundary_complete() const
{
  return elemSeq->boundary_complete();
}

ErrorCode ScdBox::get_coordinate_arrays(double *&xc, double *&yc, double *&zc) 
{
  if (!vertDat) return MB_FAILURE;

  xc = reinterpret_cast<double*>(vertDat->get_sequence_data(0));
  yc = reinterpret_cast<double*>(vertDat->get_sequence_data(1));
  zc = reinterpret_cast<double*>(vertDat->get_sequence_data(2));
  return MB_SUCCESS;
}

ErrorCode ScdBox::get_coordinate_arrays(const double *&xc, const double *&yc, const double *&zc) const
{
  if (!vertDat) return MB_FAILURE;
  xc = reinterpret_cast<const double*>(vertDat->get_sequence_data(0));
  yc = reinterpret_cast<const double*>(vertDat->get_sequence_data(1));
  zc = reinterpret_cast<const double*>(vertDat->get_sequence_data(2));
  return MB_SUCCESS;
}

ErrorCode ScdBox::vert_dat(ScdVertexData *vert_dt)
{
  vertDat = vert_dt;
  return MB_SUCCESS;
}

ErrorCode ScdBox::elem_seq(EntitySequence *elem_sq)
{
  elemSeq = dynamic_cast<StructuredElementSeq*>(elem_sq);
  if (elemSeq) elemSeq->is_periodic(locallyPeriodic);

  if (locallyPeriodic[0])
    boxSizeIM1 = boxSize[0]-(locallyPeriodic[0] ? 0 : 1);
  if (locallyPeriodic[0] || locallyPeriodic[1])
    boxSizeIJM1 = (boxSize[1] ? (boxSize[1]-(locallyPeriodic[1] ? 0 : 1)) : 1) * boxSizeIM1;

  return (elemSeq ? MB_SUCCESS : MB_FAILURE);
}  

ErrorCode ScdBox::get_params(EntityHandle ent, HomCoord &ijkd) const 
{
    // check first whether this is an intermediate entity, so we know what to do
  int dimension = box_dimension();
  int this_dim = scImpl->impl()->dimension_from_handle(ent);

  if ((0 == this_dim && !vertDat) ||
      (this_dim && this_dim == dimension)) {
    assert(elemSeq);
    return elemSeq->get_params(ent, ijkd[0], ijkd[1], ijkd[2]);
  }

  else if (!this_dim && vertDat)
    return vertDat->get_params(ent, ijkd[0], ijkd[1], ijkd[2]);

  else return MB_NOT_IMPLEMENTED;
}

  //! Get the entity of specified dimension adjacent to parametric element
  /**
   * \param dim Dimension of adjacent entity being requested
   * \param i Parametric coordinates of cell being evaluated
   * \param j Parametric coordinates of cell being evaluated
   * \param k Parametric coordinates of cell being evaluated
   * \param dir Direction (0, 1, or 2), for getting adjacent edges (2d, 3d) or faces (3d) 
   * \param ent EntityHandle of adjacent entity
   * \param create_if_missing If true, creates the entity if it doesn't already exist
   */
ErrorCode ScdBox::get_adj_edge_or_face(int dim, int i, int j, int k, int dir, EntityHandle &ent,
                                       bool create_if_missing) const 
{
    // describe connectivity of sub-element in static array
    // subconnect[dim-1][dir][numv][ijk] where dimensions are:
    // [dim-1]: dim=1 or 2, so this is 0 or 1
    // [dir]: one of 0..2, for ijk directions in a hex
    // [numv]: number of vertices describing sub entity = 2*dim <= 4
    // [ijk]: 3 values for i, j, k
  int subconnect[2][3][4][3] = {
      {{{0, 0, 0}, {1, 0, 0}, {-1, -1, -1}, {-1, -1, -1}}, // i edge
       {{0, 0, 0}, {0, 1, 0}, {-1, -1, -1}, {-1, -1, -1}}, // j edge
       {{0, 0, 0}, {0, 0, 1}, {-1, -1, -1}, {-1, -1, -1}}}, // k edge

      {{{0, 0, 0}, {0, 1, 0}, {0, 1, 1}, {0, 0, 1}}, // i face
       {{0, 0, 0}, {1, 0, 0}, {1, 0, 1}, {0, 0, 1}}, // j face
       {{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}}}}; // k face

    // check proper input dimensions and lower bound
  if (dim < 1 || dim > 2 || i < boxDims[0] || j < boxDims[1] || k < boxDims[2]) 
    return MB_FAILURE;

    // now check upper bound; parameters must be <= upper corner, since edges/faces
    // follow element parameterization, not vertex parameterization
  else if ((boxDims[3] != boxDims[0] && i > (locallyPeriodic[0] ? boxDims[3]+1 : boxDims[3])) ||
           (boxDims[4] != boxDims[1] && j > (locallyPeriodic[1] ? boxDims[4]+1 : boxDims[4])) ||
           (boxDims[5] != boxDims[2] && k > boxDims[5])) return MB_FAILURE;

        // get the vertices making up this entity
  EntityHandle verts[4];
  for (int ind = 0; ind < 2*dim; ind++) {
    int i1=i+subconnect[dim-1][dir][ind][0];
    int j1=j+subconnect[dim-1][dir][ind][1];
    // if periodic in i and i1 is boxDims[3]+1, wrap around
    if (locallyPeriodic[0] && i1==boxDims[3]+1) i1=boxDims[0];
    // if periodic in j and j1 is boxDims[4]+1, wrap around
    if (locallyPeriodic[1] && j1==boxDims[4]+1) j1=boxDims[1];
    verts[ind] = get_vertex(i1,
                            j1,
                            k+subconnect[dim-1][dir][ind][2]);
    if (!verts[ind]) return MB_FAILURE;
  }
  
  Range ents;
  ErrorCode rval = scImpl->impl()->get_adjacencies(verts, 2*dim, dim, false, ents);
  if (MB_SUCCESS != rval) return rval;

  if (ents.size() > 1) return MB_FAILURE;
  
  else if (ents.size() == 1) {
    ent = *ents.begin();
  }
  else if (create_if_missing)
    rval = scImpl->impl()->create_element((1 == dim ? MBEDGE : MBQUAD), verts, 2*dim, ent);
    
  return rval;
}
    
#ifndef MOAB_HAVE_MPI
ErrorCode ScdInterface::tag_shared_vertices(ParallelComm *, ScdBox *) 
{
  return MB_FAILURE;
#else
ErrorCode ScdInterface::tag_shared_vertices(ParallelComm *pcomm, ScdBox *box) 
{
  EntityHandle seth = box->box_set();

    // check the # ents in the box against the num in the set, to make sure it's only 1 box;
    // reuse tmp_range
  Range tmp_range;
  ErrorCode rval = mbImpl->get_entities_by_dimension(seth, box->box_dimension(), tmp_range);
  if (MB_SUCCESS != rval) return rval;
  if (box->num_elements() != (int)tmp_range.size()) return MB_FAILURE;
    
  const int *gdims = box->par_data().gDims;
  if ((gdims[0] == gdims[3] && gdims[1] == gdims[4] && gdims[2] == gdims[5]) ||
      -1 == box->par_data().partMethod) return MB_FAILURE;

    // ok, we have a partitioned box; get the vertices shared with other processors
  std::vector<int> procs, offsets, shared_indices;
  rval = get_shared_vertices(pcomm, box, procs, offsets, shared_indices);
  if (MB_SUCCESS != rval) return rval;

    // post receives for start handles once we know how many to look for
  std::vector<MPI_Request> recv_reqs(procs.size(), MPI_REQUEST_NULL), 
      send_reqs(procs.size(), MPI_REQUEST_NULL);
  std::vector<EntityHandle> rhandles(4*procs.size()), shandles(4);
  for (unsigned int i = 0; i < procs.size(); i++) {
    int success = MPI_Irecv(&rhandles[4*i], 4*sizeof(EntityHandle),
                            MPI_UNSIGNED_CHAR, procs[i],
                            1, pcomm->proc_config().proc_comm(), 
                            &recv_reqs[i]);
    if (success != MPI_SUCCESS) return MB_FAILURE;
  }

    // send our own start handles
  shandles[0] = box->start_vertex();
  shandles[1] = 0;
  if (box->box_dimension() == 1) {
    shandles[1] = box->start_element();
    shandles[2] = 0;
    shandles[3] = 0;
  } else if (box->box_dimension() == 2) {
    shandles[2] = box->start_element();
    shandles[3] = 0;
  }
  else {
    shandles[2] = 0;
    shandles[3] = box->start_element();
  }
  for (unsigned int i = 0; i < procs.size(); i++) {
    int success = MPI_Isend(&shandles[0], 4*sizeof(EntityHandle), MPI_UNSIGNED_CHAR, procs[i], 
                            1, pcomm->proc_config().proc_comm(), &send_reqs[i]);
    if (success != MPI_SUCCESS) return MB_FAILURE;
  }
  
    // receive start handles and save info to a tuple list
  int incoming = procs.size();
  int p, j, k;
  MPI_Status status;
  TupleList shared_data;
  shared_data.initialize(1, 0, 2, 0, 
                         shared_indices.size()/2);
  shared_data.enableWriteAccess();

  j = 0; k = 0;
  while (incoming) {
    int success = MPI_Waitany(procs.size(), &recv_reqs[0], &p, &status);
    if (MPI_SUCCESS != success) return MB_FAILURE;
    unsigned int num_indices = (offsets[p+1]-offsets[p])/2;
    int *lh = &shared_indices[offsets[p]], *rh = lh + num_indices;
    for (unsigned int i = 0; i < num_indices; i++) {
      shared_data.vi_wr[j++] = procs[p];
      shared_data.vul_wr[k++] = shandles[0] + lh[i];
      shared_data.vul_wr[k++] = rhandles[4*p] + rh[i];
      shared_data.inc_n();
    }
    incoming--;
  }

  // still need to wait for the send requests
  std::vector<MPI_Status> mult_status(procs.size());
  int success = MPI_Waitall(procs.size(), &send_reqs[0], &mult_status[0]);
  if (MPI_SUCCESS != success) {
    MB_SET_ERR(MB_FAILURE, "Failed in waitall in ScdInterface::tag_shared_vertices");
  }
    // sort by local handle
  TupleList::buffer sort_buffer;
  sort_buffer.buffer_init(shared_indices.size()/2);
  shared_data.sort(1, &sort_buffer);
  sort_buffer.reset();
  
    // process into sharing data
  std::map<std::vector<int>, std::vector<EntityHandle> > proc_nvecs;
  Range dum;
  rval = pcomm->tag_shared_verts(shared_data, proc_nvecs, dum, 0);
  if (MB_SUCCESS != rval) return rval;
  
    // create interface sets
  rval = pcomm->create_interface_sets(proc_nvecs);
  if (MB_SUCCESS != rval) return rval;

    // add the box to the PComm's partitionSets
  pcomm->partition_sets().insert(box->box_set());

    // make sure buffers are allocated for communicating procs
  for (std::vector<int>::iterator pit = procs.begin(); pit != procs.end(); ++pit)
    pcomm->get_buffers(*pit);

  if (pcomm->get_debug_verbosity() > 1) pcomm->list_entities(NULL, 1);

#ifndef NDEBUG
  rval = pcomm->check_all_shared_handles();
  if (MB_SUCCESS != rval) return rval;
#endif
  
  return MB_SUCCESS;
  
#endif
}

ErrorCode ScdInterface::get_neighbor_alljkbal(int np, int pfrom,
                                              const int * const gdims, const int * const gperiodic, const int * const dijk, 
                                              int &pto, int *rdims, int *facedims, int *across_bdy)
{
  if (dijk[0] != 0) {
    pto = -1;
    return MB_SUCCESS;
  }
  
  pto = -1;
  across_bdy[0] = across_bdy[1] = across_bdy[2] = 0;
  
  int ldims[6], pijk[3], lperiodic[3];
  ErrorCode rval = compute_partition_alljkbal(np, pfrom, gdims, gperiodic, 
                                              ldims, lperiodic, pijk);
  if (MB_SUCCESS != rval) return rval;
  assert(pijk[1] * pijk[2] == np);
  pto = -1;
  bool bot_j = pfrom < pijk[2],
      top_j = pfrom > np - pijk[2];
  if ((1 == pijk[2] && dijk[2]) ||  // 1d in j means no neighbors with dk != 0
      (!(pfrom%pijk[2]) && -1 == dijk[2]) || // at -k bdy
      (pfrom%pijk[2] == pijk[2]-1 && 1 == dijk[2]) || // at +k bdy
      (pfrom < pijk[2] && -1 == dijk[1] && !gperiodic[1]) ||  // down and not periodic
      (pfrom >= np-pijk[2] && 1 == dijk[1] && !gperiodic[1]))  // up and not periodic
    return MB_SUCCESS;
    
  pto = pfrom;
  std::copy(ldims, ldims+6, rdims);
  std::copy(ldims, ldims+6, facedims);
  
  if (0 != dijk[1]) {
    pto = (pto + dijk[1]*pijk[2] + np) % np;
    assert (pto >= 0 && pto < np);
    int dj = (gdims[4] - gdims[1]) / pijk[1], extra = (gdims[4] - gdims[1]) % pijk[1];
    if (-1 == dijk[1]) {
      facedims[4] = facedims[1];
      if (bot_j) {
          // going across periodic lower bdy in j
        rdims[4] = gdims[4];
        across_bdy[1] = -1;
      }
      else {
        rdims[4] = ldims[1];
      }
      rdims[1] = rdims[4] - dj;
      if (pto < extra) rdims[1]--;
    }
    else {
      if (pfrom > np-pijk[2]) facedims[4] = gdims[1];
      facedims[1] = facedims[4];
      if (top_j) {
          // going across periodic upper bdy in j
        rdims[1] = gdims[1];
        across_bdy[1] = 1;
      }
      else {
        rdims[1] = ldims[4];
      }
      rdims[4] = rdims[1] + dj;
      if (pto < extra) rdims[4]++;
    }
  }
  if (0 != dijk[2]) {
    pto = (pto + dijk[2]) % np;
    assert (pto >= 0 && pto < np);
    facedims[2] = facedims[5] = (-1 == dijk[2] ? facedims[2] : facedims[5]);
    int dk = (gdims[5] - gdims[2]) / pijk[2];
    if (-1 == dijk[2]) {
      facedims[5] = facedims[2];
      rdims[5] = ldims[2];
      rdims[2] = rdims[5] - dk; // never any kextra for alljkbal
    }
    else {
      facedims[2] = facedims[5];
      rdims[2] = ldims[5];
      rdims[5] = rdims[2] + dk; // never any kextra for alljkbal
    }
  }

  assert(-1 == pto || (rdims[0] >= gdims[0] && rdims[3] <= gdims[3]));
  assert(-1 == pto || (rdims[1] >= gdims[1] && (rdims[4] <= gdims[4] || (across_bdy[1] && bot_j))));
  assert(-1 == pto || (rdims[2] >= gdims[2] && rdims[5] <= gdims[5]));
  assert(-1 == pto || ((facedims[0] >= rdims[0] || (gperiodic[0] && rdims[3] == gdims[3]+1 && facedims[0] == gdims[0]))));
  assert(-1 == pto || (facedims[3] <= rdims[3]));
  assert(-1 == pto || ((facedims[1] >= rdims[1]  || (gperiodic[1] && rdims[4] == gdims[4]+1 && facedims[1] == gdims[1]))));
  assert(-1 == pto || (facedims[4] <= rdims[4]));
  assert(-1 == pto || (facedims[2] >= rdims[2]));
  assert(-1 == pto || (facedims[5] <= rdims[5]));
  assert(-1 == pto || (facedims[0] >= ldims[0]));
  assert(-1 == pto || (facedims[3] <= ldims[3]));
  assert(-1 == pto || (facedims[1] >= ldims[1]));
  assert(-1 == pto || (facedims[4] <= ldims[4]));
  assert(-1 == pto || (facedims[2] >= ldims[2]));
  assert(-1 == pto || (facedims[5] <= ldims[5]));
  
  return MB_SUCCESS;
}

ErrorCode ScdInterface::get_neighbor_sqij(int np, int pfrom,
                                          const int * const gdims, const int * const gperiodic, const int * const dijk, 
                                          int &pto, int *rdims, int *facedims, int *across_bdy)
{
  if (dijk[2] != 0) {
      // for sqij, there is no k neighbor, ever
    pto = -1;
    return MB_SUCCESS;
  }
  
  pto = -1;
  across_bdy[0] = across_bdy[1] = across_bdy[2] = 0;
  int lperiodic[3], pijk[3], ldims[6];
  ErrorCode rval = compute_partition_sqij(np, pfrom, gdims, gperiodic, ldims, lperiodic, pijk);
  if (MB_SUCCESS != rval) return rval;
  assert(pijk[0] * pijk[1] == np);
  pto = -1;
  bool top_i = 0, top_j = 0, bot_i = 0, bot_j = 0;
  int ni = pfrom%pijk[0], nj = pfrom/pijk[0]; // row / column number of me
  if (ni == pijk[0]-1) top_i = 1;
  if (nj == pijk[1]-1) top_j = 1;
  if (!ni) bot_i = 1;
  if (!nj) bot_j = 1;
  if ((!gperiodic[0] && bot_i && -1 == dijk[0]) ||  // left and not periodic
      (!gperiodic[0] && top_i && 1 == dijk[0]) ||  // right and not periodic
      (!gperiodic[1] && bot_j && -1 == dijk[1]) || // bottom and not periodic
      (!gperiodic[1] && top_j && 1 == dijk[1]))  // top and not periodic
    return MB_SUCCESS;
  
  std::copy(ldims, ldims+6, facedims);
  std::copy(ldims, ldims+6, rdims);
  pto = pfrom;
  int j = gdims[4] - gdims[1], dj = j / pijk[1], jextra = (gdims[4] - gdims[1]) % dj,
      i = gdims[3] - gdims[0], di = i / pijk[0], iextra = (gdims[3] - gdims[0]) % di;
  
  if (0 != dijk[0]) {
    pto = (ni + dijk[0] + pijk[0]) % pijk[0]; // get pto's ni value
    pto = nj*pijk[0] + pto;  // then convert to pto
    assert (pto >= 0 && pto < np);
    if (-1 == dijk[0]) {
      facedims[3] = facedims[0];
      if (bot_i) {
          // going across lower periodic bdy in i
        across_bdy[0] = -1;
        rdims[3] = gdims[3]+1; // +1 because ldims[3] on remote proc is gdims[3]+1
        rdims[0] = rdims[3] - di - 1; // -1 to account for rdims[3] being one larger
      }
      else {
        rdims[3] = ldims[0];
        rdims[0] = rdims[3] - di;
      }
      
      if (pto%pijk[0] < iextra) rdims[0]--;
    }
    else {
      if (top_i) {
          // going across lower periodic bdy in i
        facedims[3] = gdims[0];
        across_bdy[0] = 1;
      }
      facedims[0] = facedims[3];
      rdims[0] = (top_i ? gdims[0] : ldims[3]);
      rdims[3] = rdims[0] + di;
      if (pto%pijk[0] < iextra) rdims[3]++;
      if (gperiodic[0] && ni == pijk[0]-2) rdims[3]++; // remote proc is top_i and periodic
    }
  }
  if (0 != dijk[1]) {
    pto = (pto + dijk[1]*pijk[0] + np) % np;
    assert (pto >= 0 && pto < np);
    if (-1 == dijk[1]) {
      facedims[4] = facedims[1];
      if (bot_j) {
          // going across lower periodic bdy in j
        rdims[4] = gdims[4]+1; // +1 because ldims[4] on remote proc is gdims[4]+1
        rdims[1] = rdims[4] - dj - 1; // -1 to account for gdims[4] being one larger
        across_bdy[1] = -1;
      }
      else {
        rdims[4] = ldims[1];
        rdims[1] = rdims[4] - dj;
      }
      if (pto/pijk[0] < jextra) rdims[1]--;
    }
    else {
      if (top_j) {
          // going across lower periodic bdy in j
        facedims[4] = gdims[1];
        rdims[1] = gdims[1];
        across_bdy[1] = 1;
      }
      else {
        rdims[1] = ldims[4];
      }
      facedims[1] = facedims[4];
      rdims[4] = rdims[1] + dj;
      if (nj+1 < jextra) rdims[4]++;
      if (gperiodic[1] && nj == pijk[1]-2) rdims[4]++; // remote proc is top_j and periodic
    }
  }

    // rdims within gdims
  assert (-1 == pto || (rdims[0] >= gdims[0] && (rdims[3] <= gdims[3] + (gperiodic[0] && pto%pijk[0] == pijk[0]-1 ? 1 : 0))));
  assert (-1 == pto || (rdims[1] >= gdims[1] && (rdims[4] <= gdims[4] + (gperiodic[1] && pto/pijk[0] == pijk[1]-1 ? 1 : 0))));
  assert (-1 == pto || (rdims[2] >= gdims[2] && rdims[5] <= gdims[5]));
    // facedims within rdims
  assert (-1 == pto || ((facedims[0] >= rdims[0] || (gperiodic[0] && pto%pijk[0] == pijk[0]-1 && facedims[0] == gdims[0]))));
  assert (-1 == pto || (facedims[3] <= rdims[3]));
  assert (-1 == pto || ((facedims[1] >= rdims[1]  || (gperiodic[1] && pto/pijk[0] == pijk[1]-1 && facedims[1] == gdims[1]))));
  assert (-1 == pto || (facedims[4] <= rdims[4]));
  assert (-1 == pto || (facedims[2] >= rdims[2] && facedims[5] <= rdims[5]));
    // facedims within ldims
  assert (-1 == pto || ((facedims[0] >= ldims[0] || (top_i && facedims[0] == gdims[0]))));
  assert (-1 == pto || (facedims[3] <= ldims[3]));
  assert (-1 == pto || ((facedims[1] >= ldims[1] || (gperiodic[1] && top_j && facedims[1] == gdims[1]))));
  assert (-1 == pto || (facedims[4] <= ldims[4]));
  assert (-1 == pto || (facedims[2] >= ldims[2] && facedims[5] <= ldims[5]));

  return MB_SUCCESS;
}

ErrorCode ScdInterface::get_neighbor_sqjk(int np, int pfrom,
                                          const int * const gdims, const int * const gperiodic, const int * const dijk, 
                                          int &pto, int *rdims, int *facedims, int *across_bdy)
{
  if (dijk[0] != 0) {
    pto = -1;
    return MB_SUCCESS;
  }
  
  pto = -1;
  across_bdy[0] = across_bdy[1] = across_bdy[2] = 0;
  int pijk[3], lperiodic[3], ldims[6];
  ErrorCode rval = compute_partition_sqjk(np, pfrom, gdims, gperiodic, ldims, lperiodic, pijk);
  if (MB_SUCCESS != rval) return rval;
  assert(pijk[1] * pijk[2] == np);
  pto = -1;
  bool top_j = 0, top_k = 0, bot_j = 0, bot_k = 0;
  int nj = pfrom%pijk[1], nk = pfrom/pijk[1];
  if (nj == pijk[1]-1) top_j = 1;
  if (nk == pijk[2]-1) top_k = 1;
  if (!nj) bot_j = 1;
  if (!nk) bot_k = 1;
  if ((!gperiodic[1] && bot_j && -1 == dijk[1]) ||  // down and not periodic
      (!gperiodic[1] && top_j && 1 == dijk[1]) ||  // up and not periodic
      (bot_k && -1 == dijk[2]) || // k- bdy 
      (top_k && 1 == dijk[2])) // k+ bdy
    return MB_SUCCESS;
    
  std::copy(ldims, ldims+6, facedims);
  std::copy(ldims, ldims+6, rdims);
  pto = pfrom;
  int dj = (gdims[4] - gdims[1]) / pijk[1], jextra = (gdims[4] - gdims[1]) % dj,
      dk = (gdims[5] == gdims[2] ? 0 : (gdims[5] - gdims[2]) / pijk[2]), kextra = (gdims[5] - gdims[2]) - dk*pijk[2];
  assert((dj*pijk[1] + jextra == (gdims[4]-gdims[1])) && (dk*pijk[2] + kextra == (gdims[5]-gdims[2])));
  if (0 != dijk[1]) {
    pto = (nj + dijk[1] + pijk[1]) % pijk[1]; // get pto's ni value
    pto = nk*pijk[1] + pto;  // then convert to pto
    assert (pto >= 0 && pto < np);
    if (-1 == dijk[1]) {
      facedims[4] = facedims[1];
      if (bot_j) {
          // going across lower periodic bdy in j
        rdims[4] = gdims[4]+1; // +1 because ldims[4] on remote proc is gdims[4]+1
        across_bdy[1] = -1;
      }
      else {
        rdims[4] = ldims[1];
      }
      rdims[1] = rdims[4] - dj;
      if (nj < jextra) rdims[1]--;
    }
    else {
      if (top_j) {
          // going across upper periodic bdy in j
        rdims[1] = gdims[1];
        facedims[4] = gdims[1];
        across_bdy[1] = 1;
      }
      else {
        rdims[1] = ldims[4];
      }
      facedims[1] = facedims[4];
      rdims[4] = rdims[1] + dj;
      if (nj < jextra) rdims[4]++;
      if (gperiodic[1] && nj == dijk[1]-2) rdims[4]++; // +1 because next proc is on periodic bdy
    }
  }
  if (0 != dijk[2]) {
    pto = (pto + dijk[2]*pijk[1] + np) % np;
    assert (pto >= 0 && pto < np);
    if (-1 == dijk[2]) {
      facedims[5] = facedims[2];
      rdims[5] = ldims[2];
      rdims[2] -= dk;
      if (pto/pijk[1] < kextra) rdims[2]--;
    }
    else {
      facedims[2] = facedims[5];
      rdims[2] = ldims[5];
      rdims[5] += dk;
      if (pto/pijk[1] < kextra) rdims[5]++;
    }
  }

  assert(-1 == pto || (rdims[0] >= gdims[0] && rdims[3] <= gdims[3]));
  assert(-1 == pto || (rdims[1] >= gdims[1] && (rdims[4] <= gdims[4] || (across_bdy[1] && bot_j))));
  assert(-1 == pto || (rdims[2] >= gdims[2] && rdims[5] <= gdims[5]));
  assert(-1 == pto || (facedims[0] >= rdims[0] && facedims[3] <= rdims[3]));
  assert(-1 == pto || ((facedims[1] >= rdims[1]  || (gperiodic[1] && rdims[4] == gdims[4] && facedims[1] == gdims[1]))));
  assert(-1 == pto || (facedims[4] <= rdims[4]));
  assert(-1 == pto || (facedims[2] >= rdims[2] && facedims[5] <= rdims[5]));
  assert(-1 == pto || (facedims[0] >= ldims[0] && facedims[3] <= ldims[3]));
  assert(-1 == pto || (facedims[1] >= ldims[1] && facedims[4] <= ldims[4]));
  assert(-1 == pto || (facedims[2] >= ldims[2] && facedims[5] <= ldims[5]));

  return MB_SUCCESS;
}

ErrorCode ScdInterface::get_neighbor_sqijk(int np, int pfrom,
                                           const int * const gdims, const int * const gperiodic, const int * const dijk, 
                                           int &pto, int *rdims, int *facedims, int *across_bdy)
{
  if (gperiodic[0] || gperiodic[1] || gperiodic[2]) return MB_FAILURE;
  
  pto = -1;
  across_bdy[0] = across_bdy[1] = across_bdy[2] = 0;
  int pijk[3], lperiodic[3], ldims[6];
  ErrorCode rval = compute_partition_sqijk(np, pfrom, gdims, gperiodic, ldims, lperiodic, pijk);
  if (MB_SUCCESS != rval) return rval;
  assert(pijk[0] * pijk[1] * pijk[2] == np);
  pto = -1;
  bool top[3] = {false, false, false}, bot[3] = {false, false, false};
    // nijk: rank in i/j/k direction
  int nijk[3] = {pfrom%pijk[0], (pfrom%(pijk[0]*pijk[1]))/pijk[0], pfrom/(pijk[0]*pijk[1])};
  
  for (int i = 0; i < 3; i++) {
    if (nijk[i] == pijk[i]-1) top[i] = true;
    if (!nijk[i]) bot[i] = true;
    if ((!gperiodic[i] && bot[i] && -1 == dijk[i]) || // downward && not periodic
        (!gperiodic[i] && top[i] && 1 == dijk[i])) // upward && not periodic
      return MB_SUCCESS;
  }

  std::copy(ldims, ldims+6, facedims);
  std::copy(ldims, ldims+6, rdims);
  pto = pfrom;
  int delijk[3], extra[3];
    // nijk_to: rank of pto in i/j/k direction
  int nijk_to[3];
  for (int i = 0; i < 3; i++) {
    delijk[i] = (gdims[i+3] == gdims[i] ? 0 : (gdims[i+3] - gdims[i])/pijk[i]);
    extra[i] = (gdims[i+3]-gdims[i]) % delijk[i];
    nijk_to[i] = (nijk[i]+dijk[i]+pijk[i]) % pijk[i];
  }
  pto = nijk_to[2]*pijk[0]*pijk[1] + nijk_to[1]*pijk[0] + nijk_to[0];
  assert (pto >= 0 && pto < np);
  for (int i = 0; i < 3; i++) {
    if (0 != dijk[i]) {
      if (-1 == dijk[i]) {
        facedims[i+3] = facedims[i];
        if (bot[i]) {
            // going across lower periodic bdy in i
          rdims[i+3] = gdims[i+3]+1; // +1 because ldims[4] on remote proc is gdims[4]+1
          across_bdy[i] = -1;
        }
        else {
          rdims[i+3] = ldims[i];
        }
        rdims[i] = rdims[i+3] - delijk[i];
        if (nijk[i] < extra[i]) rdims[i]--;
      }
      else {
        if (top[i]) {
          // going across upper periodic bdy in i
          rdims[i] = gdims[i];
          facedims[i+3] = gdims[i];
          across_bdy[i] = 1;
        }
        else {
          rdims[i] = ldims[i+3];
        }
        facedims[i] = facedims[i+3];
        rdims[i+3] = rdims[i] + delijk[i];
        if (nijk[i] < extra[i]) rdims[i+3]++;
        if (gperiodic[i] && nijk[i] == dijk[i]-2) rdims[i+3]++; // +1 because next proc is on periodic bdy
      }
    }
  }

  assert(-1 != pto);
#ifndef NDEBUG
  for (int i = 0; i < 3; i++) {
    assert((rdims[i] >= gdims[i] && (rdims[i+3] <= gdims[i+3] || (across_bdy[i] && bot[i]))));
    assert(((facedims[i] >= rdims[i]  || (gperiodic[i] && rdims[i+3] == gdims[i+3] && facedims[i] == gdims[i]))));
    assert((facedims[i] >= ldims[i] && facedims[i+3] <= ldims[i+3]));
  }
#endif  

  return MB_SUCCESS;
}

ErrorCode ScdInterface::get_neighbor_alljorkori(int np, int pfrom,
                                                const int * const gdims, const int * const gperiodic, const int * const dijk, 
                                                int &pto, int *rdims, int *facedims, int *across_bdy)
{
  ErrorCode rval = MB_SUCCESS;
  pto = -1;
  if (np == 1) return MB_SUCCESS;
  
  int pijk[3], lperiodic[3], ldims[6];
  rval = compute_partition_alljorkori(np, pfrom, gdims, gperiodic, ldims, lperiodic, pijk);
  if (MB_SUCCESS != rval) return rval;

  int ind = -1;
  across_bdy[0] = across_bdy[1] = across_bdy[2] = 0;

  for (int i = 0; i < 3; i++) {
    if (pijk[i] > 1) {
      ind = i;
      break;
    }
  }
  
  assert(-1 < ind);
  
  if (!dijk[ind]) 
      // no neighbor, pto is already -1, return
    return MB_SUCCESS;

  bool is_periodic = ((gperiodic[0] && ind == 0) || (gperiodic[1] && ind == 1));
  if (dijk[(ind+1)%3] || dijk[(ind+2)%3] || // stepping in either other two directions
      (!is_periodic && ldims[ind] == gdims[ind] && dijk[ind] == -1) || // lower side and going lower
      (!is_periodic && ldims[3+ind] >= gdims[3+ind] && dijk[ind] == 1)) // not >= because ldims is only > gdims when periodic; 
                                                                        // higher side and going higher
    return MB_SUCCESS;

  std::copy(ldims, ldims+6, facedims);
  std::copy(ldims, ldims+6, rdims);
  
  int dind = (gdims[ind+3] - gdims[ind])/np;
  int extra = (gdims[ind+3] - gdims[ind])%np;
  if (-1 == dijk[ind] && pfrom) {
      // actual left neighbor
    pto = pfrom-1; // no need for %np, because pfrom > 0
    facedims[ind+3] = facedims[ind];
    rdims[ind+3] = ldims[ind];
    rdims[ind] = rdims[ind+3] - dind - (pto < extra ? 1 : 0);
  }
  else if (1 == dijk[ind] && pfrom < np-1) {
      // actual right neighbor
    pto = pfrom+1;
    facedims[ind] = facedims[ind+3];
    rdims[ind] = ldims[ind+3];
    rdims[ind+3] = rdims[ind] + dind + (pto < extra ? 1 : 0);
    if (is_periodic && pfrom == np-2) rdims[ind+3]++; // neighbor is on periodic bdy
  }
  else if (-1 == dijk[ind] && !pfrom && gperiodic[ind]) {
      // downward across periodic bdy
    pto = np - 1;
    facedims[ind+3] = facedims[ind] = gdims[ind]; // by convention, facedims is within gdims, so lower value
    rdims[ind+3] = gdims[ind+3] + 1; // by convention, local dims one greater than gdims to indicate global lower value
    rdims[ind] = rdims[ind+3] - dind - 1;
    across_bdy[ind] = -1;
  }
  else if (1 == dijk[ind] && pfrom == np-1 && is_periodic) {
      // right across periodic bdy
    pto = 0;
    facedims[ind+3] = facedims[ind] = gdims[ind]; // by convention, facedims is within gdims, so lowest value
    rdims[ind] = gdims[ind];
    rdims[ind+3] = rdims[ind] + dind + (pto < extra ? 1 : 0);
    across_bdy[ind] = 1;
  }

  assert(-1 == pto || (rdims[0] >= gdims[0] && (rdims[3] <= gdims[3] || (across_bdy[0] && !pfrom))));
  assert(-1 == pto || (rdims[1] >= gdims[1] && (rdims[4] <= gdims[4] || (across_bdy[1] && !pfrom))));
  assert(-1 == pto || (rdims[2] >= gdims[2] && rdims[5] <= gdims[5]));
  assert(-1 == pto || (facedims[0] >= rdims[0] && facedims[3] <= rdims[3]));
  assert(-1 == pto || (facedims[1] >= rdims[1] && facedims[4] <= rdims[4]));
  assert(-1 == pto || (facedims[2] >= rdims[2] && facedims[5] <= rdims[5]));
  assert(-1 == pto || (facedims[0] >= ldims[0] && facedims[3] <= ldims[3]));
  assert(-1 == pto || (facedims[1] >= ldims[1] && facedims[4] <= ldims[4]));
  assert(-1 == pto || (facedims[2] >= ldims[2] && facedims[5] <= ldims[5]));

  return rval;
}
  
  //! get shared vertices for alljorkori partition scheme
#ifndef MOAB_HAVE_MPI
ErrorCode ScdInterface::get_shared_vertices(ParallelComm *, ScdBox *, 
                                            std::vector<int> &,
                                            std::vector<int> &, std::vector<int> &)  
{
  return MB_FAILURE;
#else
ErrorCode ScdInterface::get_shared_vertices(ParallelComm *pcomm, ScdBox *box, 
                                            std::vector<int> &procs,
                                            std::vector<int> &offsets, std::vector<int> &shared_indices) 
{
    // get index of partitioned dimension
  const int *ldims = box->box_dims();
  ErrorCode rval;
  int ijkrem[6], ijkface[6], across_bdy[3];

  for (int k = -1; k <= 1; k ++) {
    for (int j = -1; j <= 1; j ++) {
      for (int i = -1; i <= 1; i ++) {
        if (!i && !j && !k) continue;
        int pto;
        int dijk[] = {i, j, k};
        rval = get_neighbor(pcomm->proc_config().proc_size(), pcomm->proc_config().proc_rank(), 
                            box->par_data(), dijk,
                            pto, ijkrem, ijkface, across_bdy);
        if (MB_SUCCESS != rval) return rval;
        if (-1 != pto) {
          if (procs.empty() || pto != *procs.rbegin()) {
            procs.push_back(pto);
            offsets.push_back(shared_indices.size());
          }
          rval = get_indices(ldims, ijkrem, across_bdy, ijkface, shared_indices);
          if (MB_SUCCESS != rval) return rval;

            // check indices against known #verts on local and remote 
            // begin of this block is shared_indices[*offsets.rbegin()], end is shared_indices.end(), halfway
            // is (shared_indices.size()-*offsets.rbegin())/2
#ifndef NDEBUG
          int start_idx = *offsets.rbegin(), end_idx = shared_indices.size(), mid_idx = (start_idx+end_idx)/2;
          
          int num_local_verts = (ldims[3]-ldims[0]+1)*(ldims[4]-ldims[1]+1)*
              (-1 == ldims[2] && -1 == ldims[5] ? 1 : (ldims[5]-ldims[2]+1)),
              num_remote_verts = (ijkrem[3]-ijkrem[0]+1)*(ijkrem[4]-ijkrem[1]+1)*
              (-1 == ijkrem[2] && -1 == ijkrem[5] ? 1 : (ijkrem[5]-ijkrem[2]+1));
          
          assert(*std::min_element(&shared_indices[start_idx], &shared_indices[mid_idx]) >= 0 &&
                 *std::max_element(&shared_indices[start_idx], &shared_indices[mid_idx]) < num_local_verts &&
                 *std::min_element(&shared_indices[mid_idx], &shared_indices[end_idx]) >= 0 &&
                 *std::max_element(&shared_indices[mid_idx], &shared_indices[end_idx]) < num_remote_verts);
#endif          
        }
      }
    }
  }

  offsets.push_back(shared_indices.size());

  return MB_SUCCESS;
#endif
}

} // namespace moab
