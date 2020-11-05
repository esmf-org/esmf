// $Id$
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research, 
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
using namespace moab;
#endif

#include "ESMCI_Base.h"
#include "ESMCI_CoordSys.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

#include <map>

namespace ESMCI {

#define MBMESH_CHECK_ERR(merr, localrc) {\
  if (merr != MB_SUCCESS) \
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, \
      moab::ErrorCodeStr[merr], ESMC_CONTEXT, &localrc)) throw localrc; }

#define ESMC_CHECK_MOAB_RC(actual_rc) {\
  if (actual_rc != MB_SUCCESS) {\
    ESMCI::esmc_error local_macro_error("", actual_rc, ""); \
    if (ESMC_LogDefault.MsgFoundError(actual_rc, local_macro_error.what(), ESMC_CONTEXT, nullptr)) \
      throw(local_macro_error);}}

// should go in base, but it's here for now
// for use when pointer *rc is expected as a return value
#define ESMC_CHECK_ERRPASSTHRU(localrc) {\
  if (localrc != ESMF_SUCCESS) {\
    ESMCI::esmc_error local_macro_error("", localrc, ""); \
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) \
      throw(local_macro_error);}}

// for routines with direct access to MOAB, when pointer *rc is NOT returned
#define ESMC_CATCH_MOAB \
  catch(int localrc){ \
    if (localrc != ESMF_SUCCESS) {\
      ESMCI::esmc_error local_macro_error("", localrc, ""); \
      if (ESMC_LogDefault.MsgFoundError(localrc, local_macro_error.what(), ESMC_CONTEXT, nullptr)) \
        throw(local_macro_error);} \
  } catch(std::string errstr){ \
    ESMCI::esmc_error local_macro_error("ESMC_RC_MOAB_ERROR", ESMC_RC_MOAB_ERROR, errstr); \
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, errstr, ESMC_CONTEXT, nullptr)) \
      throw(local_macro_error); \
  } catch (std::exception &exc) {\
    if (ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, exc.what(), ESMC_CONTEXT, nullptr)) \
      throw(exc); \
  } catch(...) {\
    std::string msg;\
    msg = "Unhandled throw";\
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, msg, ESMC_CONTEXT, nullptr);}

// for routines with direct access to MOAB, when pointer *rc is returned
#define ESMC_CATCH_MOAB_RC \
  catch(int localrc){ \
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
  } catch(std::string errstr){ \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, errstr, ESMC_CONTEXT, rc); \
  } catch (ESMCI::esmc_error &exc) {\
    ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
  } catch (std::exception &exc) {\
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, exc.what(), ESMC_CONTEXT, rc); \
  } catch(...) {\
    std::string msg;\
    msg = "Unhandled throw";\
    ESMC_LogDefault.MsgFoundError(ESMC_RC_MOAB_ERROR, msg, ESMC_CONTEXT, rc);}

// for MBMesh routines, when pointer *rc is returned
#define ESMC_CATCH_MBMESH \
  catch (ESMCI::esmc_error &exc) {\
    ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc); \
  } catch(...) {\
    std::string msg;\
    msg = "Unhandled throw";\
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL, msg, ESMC_CONTEXT, rc);}

// may need one more for MBMesh routines which do not return rc pointer (constructor)


  class MBMesh {
#if defined ESMF_MOAB

  public:
    int pdim; 
    int sdim; // Spatial dim of coordinates (after conversion), maybe change to cart_sdim? 
    int orig_sdim;  // Original spatial dim before converting
    ESMC_CoordSys_Flag coordsys;

    // RLO: why isn't this private? (and most of the other members as well)
    Interface *mesh; // Moab mesh  MAYBE I SHOULD NAME ThIS SOMETHING ELSE????

    int num_verts; // number of verts this processor

    // eventualy get rid of this
    EntityHandle *verts; // Temporary storage for element create

    int num_elems; // number of elems on this processor

    // Tags
    Tag gid_tag;
    Tag orig_pos_tag;
    Tag owner_tag;

    bool has_node_orig_coords;
    Tag node_orig_coords_tag;

    bool has_node_mask;
    Tag node_mask_tag;
    Tag node_mask_val_tag;

    bool has_elem_coords;
    Tag  elem_coords_tag;

    bool has_elem_orig_coords;
    Tag  elem_orig_coords_tag;

    bool has_elem_frac; // TODO: Get rid of this
    Tag  elem_frac_tag;

    bool has_elem_mask;
    Tag elem_mask_tag;
    Tag elem_mask_val_tag;

    bool has_elem_area;
    Tag  elem_area_tag;

    // Split stuff
    bool is_split;
    int max_non_split_id;
    std::map<int,int> split_to_orig_id;
    std::map<int,double> split_id_to_frac;

    void CreateGhost();

    // Mesh from inputs
    MBMesh(int _pdim, int _orig_sdim, ESMC_CoordSys_Flag _coordSys);

    // Add one node
    EntityHandle add_node(double *orig_coords, int gid, int orig_pos, int owner);

    // Add one elem
    EntityHandle add_elem(EntityType elem_type, int num_nodes, EntityHandle *nodes, 
                          int gid, int orig_pos, int owner);



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


    // BOB: Should these come out via return??
    // RLO: that would be nice, but we can't without a Range return from MOAB..
    //      an option would be to allocated space for a Range, deleted on destruct

    // Get a Range of all nodes and elements on this processor
    void get_all_nodes(Range &all_nodes);
    void get_all_elems(Range &all_elems);

    // Basic accessors for number of nodes, elements, and connectivity
    int num_elem();
    int num_node();
    int num_elem_conn();

    // Range based accessors for required element tags
    void get_elem_connectivity(int *elem_conn);
    void get_elem_ids(int *elem_ids);
    void get_elem_types(int *elem_types);
    // Range-based accessors for optional element tags
    void get_elem_areas(double *elem_area);
    void get_elem_coords(double *elem_coords);
    void get_elem_mask(int *elem_mask);

    // Range based accessors for required node tags
    void get_node_coords(double *node_coords);
    void get_node_ids(int *node_ids);
    void get_node_owners(int *node_owners);
    // Range based accessors for optional node tags
    void get_node_mask(int *node_mask);


    // Accessors for values from a single EntityHandle (avoid if possible, slow)
    int get_elem_mask_val(EntityHandle eh);
    double get_elem_area(EntityHandle eh);


    // Turn on node masking for this mesh
    void setup_node_mask();

    // Set node mask value
    void set_node_mask_val(EntityHandle eh, int mask_val);


    // Set node coords
    void set_node_coords(EntityHandle eh, double *orig_coords);

    // Turn on elem masking
    void setup_elem_mask();

    // Set an element mask value 
    void set_elem_mask_val(EntityHandle eh, int mask_val);

    // Setup elem areas
    void setup_elem_area();

    // Set an elem area value
    void set_elem_area(EntityHandle eh, double area);


    // Setup elem coords
    void setup_elem_coords();

    // Set coords in an elem
    void set_elem_coords(EntityHandle eh, double *orig_coords);

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


// DEPRECATED 
// TODO: Get rid of verts array
// Call after all nodes have been added to setup verts array
    void setup_verts_array();

#endif

    // Create empty mesh
    MBMesh();

    // Get rid of mesh
    ~MBMesh();

  };

} // namespace 

#endif
