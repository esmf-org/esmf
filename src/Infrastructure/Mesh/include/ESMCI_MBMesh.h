// $Id$
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_MBMesh_h
#define ESMCI_MBMesh_h


#if defined ESMF_MOAB
#include "moab/Core.hpp"
// #include "MBTagConventions.hpp"
using namespace moab;
#endif

#include "ESMCI_Base.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "Mesh/include/Legacy/ESMCI_Exception.h"

#include <map>
#include <vector>

namespace ESMCI {

// Specified in the input connection list information for an element. 
// When specified, indicates that the element should be divided at that point into 
// another polygon
#define MBMESH_POLYBREAK_IND -7

// use when no pointer *rc is expected to return
#define ESMC_CHECK_MOAB_THROW(merr) \
  if (merr != MB_SUCCESS) {\
    ESMCI::esmc_error local_macro_error("", ESMC_RC_MOAB_ERROR, moab::ErrorCodeStr[merr]); \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, local_macro_error.what(), ESMC_CONTEXT, nullptr); \
      throw(local_macro_error);}

// use when no pointer *rc is expected to return
#define ESMC_CHECK_THROW(localrc) \
    if (localrc != ESMF_SUCCESS) {\
      std::string rcmsg; \
      rcmsg = ESMC_LogGetErrMsg(localrc); \
      ESMCI::esmc_error local_macro_error(rcmsg, localrc, ""); \
      ESMC_LogDefault.MsgFoundError(localrc, local_macro_error.what(), ESMC_CONTEXT, nullptr); \
      throw(local_macro_error);}

// use when pointer *rc is expected as a return value
#define ESMC_CHECK_PASSTHRU_THROW(localrc) \
  if (localrc != ESMF_SUCCESS) {\
    ESMCI::esmc_error local_macro_error("", localrc, ""); \
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
      throw(local_macro_error);}

// use when pointer *rc is NOT returned
#define CATCH_MBMESH_RETHROW \
  catch(int localrc){ \
    if (localrc != ESMF_SUCCESS) {\
      ESMCI::esmc_error local_macro_error("", localrc, ""); \
      ESMC_LogDefault.MsgFoundError(localrc, local_macro_error.what(), ESMC_CONTEXT, nullptr); \
      throw(local_macro_error);} \
  } catch(std::string errstr){ \
    ESMCI::esmc_error local_macro_error("ESMC_RC_MOAB_ERROR", ESMC_RC_MOAB_ERROR, errstr); \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, errstr, ESMC_CONTEXT, nullptr); \
    throw(local_macro_error); \
  } catch (std::exception &exc) {\
    if (exc.what()) { \
      ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, exc.what(), ESMC_CONTEXT, nullptr); \
    } else { \
      ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, "Unknown exception", ESMC_CONTEXT, nullptr); \
    } \
    ESMCI::esmc_error local_macro_error("ESMC_RC_MOAB_ERROR", ESMC_RC_MOAB_ERROR, exc.what()); \
    throw(local_macro_error); \
  } catch(...) {\
    std::string msg;\
    msg = "Unknown exception";\
    ESMCI::esmc_error local_macro_error("ESMC_RC_MOAB_ERROR", ESMC_RC_MOAB_ERROR, msg); \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, msg, ESMC_CONTEXT, nullptr); \
    throw(local_macro_error); }

// use when pointer *rc is returned
#define CATCH_MBMESH_RETURN(rc) \
  catch(int localrc){ \
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
    return; \
  } catch(std::string errstr){ \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, errstr, ESMC_CONTEXT, rc); \
    return; \
  } catch (ESMCI::esmc_error &exc) { \
    ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
    return; \
  } catch (std::exception &exc) { \
    if (exc.what()) { \
      ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, exc.what(), ESMC_CONTEXT, rc); \
    } else { \
      ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, "Unknown exception", ESMC_CONTEXT, rc); \
    } \
    return; \
  } catch(...) { \
    std::string msg; \
    msg = "Unknown exception"; \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL, msg, ESMC_CONTEXT, rc); \
    return; }


  class MBMesh {
#if defined ESMF_MOAB

  private:
    int _num_node;
    int _num_elem;
    int _num_elem_conn;
    int _num_orig_node;
    int _num_orig_elem;
    int _num_orig_elem_conn;
    int _num_owned_node;
    int _num_owned_elem;
    int _num_owned_elem_conn;
    bool has_ghost;

  public:
    int pdim; 
    int sdim; // Spatial dim of coordinates (after conversion), maybe change to cart_sdim? 
    int orig_sdim;  // Original spatial dim before converting
    ESMC_CoordSys_Flag coordsys;

    // RLO: why isn't this private? (and most of the other members as well)
    Interface *mesh; // Moab mesh  MAYBE I SHOULD NAME ThIS SOMETHING ELSE????

    // Guard variables to make sure things have been finalized
    bool nodes_finalized;

    // Guard variables to make sure things have been finalized
    bool elems_finalized;
    

    // Tags
    Tag gid_tag;
    Tag orig_pos_tag;
    Tag owner_tag;
    enum {ORIG_POS_MIN_OUTPUT=0, ORIG_POS_AFTERCREATE=-1, ORIG_POS_SPLITELEM=-2}; 

    bool has_node_orig_coords;
    Tag node_orig_coords_tag;

    bool has_node_mask;
    Tag node_mask_tag;
    Tag node_mask_val_tag;

    bool has_elem_coords;
    Tag  elem_coords_tag;

    bool has_elem_orig_coords;
    Tag  elem_orig_coords_tag;

    bool has_elem_mask;
    Tag elem_mask_tag;
    Tag elem_mask_val_tag;

    bool has_elem_area;
    Tag  elem_area_tag;

    bool has_elem_frac; // TODO: Get rid of this if it's always there???
                        //       or make it so that elem fracs can be added later.
    Tag  elem_frac_tag;

    // Split stuff
    // TODO: rethink this to make it more efficient
    bool is_split;
    int max_non_split_id;
    std::map<int,int> split_to_orig_id;
    std::map<int,double> split_id_to_frac;

    // RLO: this requires C++11
    static constexpr auto ms = 4;
    const std::pair<int, int> MOAB2ESMFElemType[ms] = {{2, 3}, {3, 4}, {5, 10}, {9, 12}};
    const std::pair<int, int> ESMF2MOABElemType[ms] = {{3, 2}, {4, 3}, {10, 5}, {12, 9}};

    int M2EType(int key, int range = ms);
    int M2ETypeT(int value, int range = ms);
    int E2MType(int key, int range = ms);
    int E2MTypeT(int value, int range = ms);
    
    // ESMF Mesh element types defined in Mesh/include/ESMCI_Mesh.h
    //
    // ESMC_MESHELEMTYPE_TRI  3
    // ESMC_MESHELEMTYPE_QUAD 4
    // ESMC_MESHELEMTYPE_TETRA 10
    // ESMC_MESHELEMTYPE_HEX  12
    //
    // enum MOAB_ENTITY_TYPE_NAME {
    //   MBVERTEX = 0,
    //   MBEDGE,    1
    //   MBTRI,     2
    //   MBQUAD,    3
    //   MBPOLYGON, 4
    //   MBTET,     5
    //   MBPYRAMID, 6
    //   MBPRISM,   7
    //   MBKNIFE,   8
    //   MBHEX,     9
    //   MBPOLYHEDRON, 10
    //   MBENTITYSET,  11
    //   MBMAXTYPE     12
    // };
    
    void CreateGhost();

    // Mesh from inputs
    MBMesh(int _pdim, int _orig_sdim, ESMC_CoordSys_Flag _coordSys);

    // Create empty mesh
    // EVENTUALLY MAKE THIS PRIVATE TO ENCOURAGE THE USE OF THE CONSTRUCTOR
    // THAT SETS EVEYTHING UP CORRECTLY
    MBMesh();

    // Add one node
    EntityHandle add_node(double *orig_coords, int gid, int orig_pos, int owner);

    // Add a set of nodes
    // Returns the range of added nodes
    void add_nodes(int num_nodes,       // Number of nodes
                   double *orig_coords, // For each node it's orig_coords
                   int *gids,           // For each node it's gid
                   int *orig_pos,       // For each node it's orig_pos, if NULL just order
                   int *owners,         // For each node it's owner
                   Range &added_nodes);


    // Turn on node masking for this mesh
    void setup_node_mask();

    // Set node mask value
    void set_node_mask_val(EntityHandle eh, int mask_val);

    // Set node mask value on a Range of nodes
    void set_node_mask_val(Range nodes, int *mask_vals);

    // Get node mask value for one entity
    int get_node_mask_val(EntityHandle node);

    // Set node mask
    void set_node_mask(EntityHandle eh, int mask_val);

    // Set node mask
    void set_node_mask(Range nodes, int *masks);

    // Set node mask
    void set_node_mask(std::vector<EntityHandle> const &nodes, int *masks);

    // Get node mask for one entity
    int get_node_mask(EntityHandle node);


    // Set node coords
    void set_node_coords(EntityHandle eh, double *orig_coords);

    // Get original node coords
    void get_node_orig_coords(EntityHandle node, double *coords);

    // Get internal Cartesian node coords
    void get_node_cart_coords(EntityHandle node, double *coords);

    // Add one elem
    EntityHandle add_elem(EntityType elem_type, int num_nodes, EntityHandle *nodes, 
                          int gid, int orig_pos, int owner);

    // Add a set of elems
    // Returns the range of added elems
    void add_elems(int num_elems,  // The number of elems to add
                   EntityType elem_type, int nodes_per_elem, // The type and number of nodes in each elem
                   EntityHandle *nodes, // List of nodes that make up each elem (of size num_elems*nodes_per_elem)
                   int *gid,      // global ids for each elem (of size num_elems)
                   int *orig_pos,  // original position for each elem (of size num_elems)
                   int *owner,     // owner for each elem (of size num_elems)
                   Range &added_elems);


    // Change owner
    void set_owner(EntityHandle eh, int owner);

    // Get owner
    int get_owner(EntityHandle eh);

    // Get original position
    int get_orig_pos(EntityHandle eh);

    // Get gid
    int get_gid(EntityHandle eh);


    // TODO:
    //      + We should eventually have a get set of each type of field (mask, etc.) for individual entityhandles, as well as an array of entity handles. 
    //        For the individual gets, I'm not sure what to do about coordinates. It's useful to have the gids, etc come out of the return value of the method, so I hate
    //        to force all of the gets to return through the arg list. However, I'm not sure if there's a good way to do that for the coords. May have to just have the couple of get coord
    //        interfaces be different. 
    //      + Make versions of get_all_elems() and get_all_nodes() to just return local things? 


    // There are 3 main classes of Entities
    // - all entities which are returned via a range
    // - orig entities which we store as a vector sorted by the original position. Note that there may be entities which aren't original (e.g. ghost entities), so
    //   orig_entities may be smaller than all entities. 
    // - local entities which you need to go through one of the above lists and figure out which has owener==localPet. However, I think
    //   that I might add a vector of these soon. 

    // Basic accessors for number of nodes, elements, and connectivity
    int num_node(){
      if (!nodes_finalized) {Throw() << "Nodes not finalized, so num_node not set.";}
      return _num_node;
    };

    int num_elem(){
      if (!elems_finalized) {Throw() << "Elements not finalized, so num_elem not set.";}
      return _num_elem;
    };

    int num_elem_conn(){
      if (!elems_finalized) {Throw() << "Elements not finalized, so num_elem_conn not set.";}
      return _num_elem_conn;
    };

    int num_owned_node(){
      if (!nodes_finalized) {Throw() << "Nodes not finalized, so num_owned_node not set.";}
      return _num_owned_node;
    };

    int num_owned_elem(){
      if (!elems_finalized) {Throw() << "Elements not finalized, so num_owned_elem not set.";}
      return _num_owned_elem;
    };

    int num_owned_elem_conn(){
      if (!elems_finalized) {Throw() << "Elements not finalized, so num_owned_elem_conn not set.";}
      return _num_owned_elem_conn;
    };

    int num_orig_node() {
      if (!nodes_finalized) {Throw() << "Nodes not finalized, so num_orig_node not set.";}
      return _num_orig_node;
    }

    int num_orig_elem() {
      if (!elems_finalized) {Throw() << "Elems not finalized, so num_orig_elem not set.";}
      return _num_orig_elem;
    }

    int num_orig_elem_conn() {
      if (!elems_finalized) {Throw() << "Elems not finalized, so num_orig_elem_conn not set.";}
      return _num_orig_elem_conn;
    }

    void get_all_nodes(std::vector<EntityHandle> &all_nodes);
    void get_all_elems(std::vector<EntityHandle> &all_elems);

    void get_owned_nodes(std::vector<EntityHandle> &owned_nodes);
    void get_owned_elems(std::vector<EntityHandle> &owned_elems);

    // Return a a vector of EntityHandles for the original entities used for creation
    // on this processor sorted in the order they were used for creation
    void get_sorted_orig_nodes(std::vector<EntityHandle> &owned_nodes);
    void get_sorted_orig_elems(std::vector<EntityHandle> &owned_elems);

    // used in finalize
    int get_num_elem_conn(std::vector<EntityHandle> elems);
    

    // Get a Range of all nodes and elements on this processor
    void get_all_nodes(Range &all_nodes);
    void get_all_elems(Range &all_elems);

    // Range based accessors for required element tags
    void get_elem_connectivity(int *elem_conn);
    void get_elem_ids(int *elem_ids);
    void get_elem_types(int *elem_types);
    // Range-based accessors for optional element tags
    void get_elem_areas(double *elem_area);
    void get_elem_coords(double *elem_coords);
    void get_elem_mask(int *elem_mask);
    void get_elem_centroids(double *elem_centroid);

    // Range based accessors for required node tags
    void get_node_coords(double *node_coords);
    void get_node_ids(int *node_ids);
    void get_node_owners(int *node_owners);
    // Range based accessors for optional node tags
    void get_node_mask(int *node_mask);


    //// Vector based accesors for object (either nodes or elems) info ////

    // Get global ids for a vector of entities (e.g. for the orig_nodes)
    void get_gid(std::vector<EntityHandle> const &objs, int *gids);

    // Get owners for a vector of entities (e.g. for the orig_nodes)
    void get_owners(std::vector<EntityHandle> const &objs, int *owners);

    // Get orig_pos for a vector of entities (e.g. for the orig_nodes)
    void get_orig_pos(std::vector<EntityHandle> const &objs, int *orig_pos);


    //// Vector based accesors for node info ////

    // Get node coords for a vector of entities (e.g. for the orig_nodes)
    void get_node_orig_coords(std::vector<EntityHandle> const &nodes, double *orig_coords);

    // Set node mask values for a vector of entities (e.g. for the orig_nodes)
    void set_node_mask_val(std::vector<EntityHandle> const &nodes, int *mask_vals);

    // Get node mask values for a vector of entities (e.g. for the orig_nodes)
    void get_node_mask_val(std::vector<EntityHandle> const &nodes, int *mask_vals);


    //// Vector based accesors for elem info ////

    // Set elem mask for a vector of entities (e.g. using orig_elems)
    void set_elem_mask(std::vector<EntityHandle> const &elems, int *masks);

    // Set elem mask values for a vector of entities (e.g. using orig_elems)
    void set_elem_mask_val(std::vector<EntityHandle> const &elems, int *mask_val);

    // Get elem mask values for a vector of entities (e.g. using orig_elems)
    void get_elem_mask_val(std::vector<EntityHandle> const &elems, int *mask_val);

    // Set elem area values for a vector of entities (e.g. using orig_elems)
    void set_elem_area(std::vector<EntityHandle> const &elems, double *areas);

    // Get elem area values for a vector of entities (e.g. using orig_elems)
    void get_elem_area(std::vector<EntityHandle> const &elems, double *areas);

    // Get elem frac values for a vector of entities (e.g. using orig_elems)
    void get_elem_frac(bool merge_split, std::vector<EntityHandle> const &elems, double *fracs);

    // Get elem coords for a vector of entities (e.g. using orig_elems)
    void get_elem_orig_coords(std::vector<EntityHandle> const &elems, double *orig_coords);

    void get_elem_connectivity(std::vector<EntityHandle> const &elems, int *elem_conn);
    void get_elem_types(std::vector<EntityHandle> const &elems, int *elem_conn);




    // Accessors for values from a single EntityHandle (avoid if possible, slow)
    int get_elem_mask_val(EntityHandle eh);
    double get_elem_area(EntityHandle eh);
    double get_elem_frac(EntityHandle eh);

    // Turn on elem masking
    void setup_elem_mask();

    // Get elem mask (not mask val) 
    int get_elem_mask(EntityHandle eh);

    // Set elem mask (not mask val) 
    void set_elem_mask(EntityHandle eh, int mask);
    void set_elem_mask(Range elems, int *masks);

    // Set an element mask value 
    void set_elem_mask_val(EntityHandle eh, int mask_val);
    void set_elem_mask_val(Range elems, int *mask_vals);

    // Setup elem areas
    void setup_elem_area();

    // Set an elem area value
    void set_elem_area(EntityHandle eh, double area);

    // Set a range of element area values 
    void set_elem_area(Range elems, double *area);

    // Setup elem coords
    void setup_elem_coords();

    // Set coords in an elem
    void set_elem_coords(EntityHandle eh, double *orig_coords);

    void set_elem_coords(Range elems, double *orig_coords);

    // Get Cartesian coords from elem
    void get_elem_cart_coords(EntityHandle elem, double *coords);

    // Get orig coords from elem
    void get_elem_orig_coords(EntityHandle elem, double *orig_coords);

    // Get the corner nodes of an element
    void get_elem_corner_nodes(EntityHandle elem, int &num_corner_nodes, const EntityHandle *&corner_nodes);


    // Do halo communication on all node tags
    void halo_comm_nodes_all_tags(bool do_internal_coords=false);

    // Do halo communication on all elem tags
    void halo_comm_elems_all_tags();


    // Setup MBMesh to operate in parallel by resolving shared ents, etc. 
    void setup_parallel();

    // If the mesh has changed, update parallel (NOT TESTED!)
    void update_parallel();

    // Output mesh nodes for debugging
    void debug_output_nodes();

    // Output mesh elems for debugging
    void debug_output_elems();

    // change proc numbers to those of new VM
    void map_proc_numbers(int num_procs, int *proc_map);

    // Call after all nodes have been added
    void finalize_nodes();

    // Call after all elems have been added
    void finalize_elems();


// DEPRECATED 
// TODO: Get rid of verts array
// Call after all nodes have been added to setup verts array
    void setup_verts_array();

#endif

    // Get rid of mesh
    ~MBMesh();

  };

} // namespace 

#endif
