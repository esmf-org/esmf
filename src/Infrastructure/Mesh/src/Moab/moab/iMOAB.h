#ifndef IMOAB_H
#define IMOAB_H
/** \file iMOAB.h
  iMOAB: a language-agnostic, lightweight interface to MOAB

  Supports usage from C/C++, Fortran (77/90/2003), Python
 
  \remark 1) All data in the interface are exposed via POD-types.
  \remark 2) Pass everything by reference, so we do not have to use %VAL()
  \remark 3) Arrays are allocated by the client code. No concerns about 
     de-allocation of the data will be taken up by the interface.
  \remark 4) Always pass the pointer to the start of array along with the
     total allocated size for the array.
  \remark 5) Return the filled array requested by client along with 
     optionally the actual length of the array that was filled.
     (for typical cases, should be the allocated length)
*/

/*

1) Fortran MPI_Comm won't work. Take an integer argument and use MPI_F2C calls to get the C-Comm object
2) ReadHeaderInfo - Does it need the pid ? 
3) Reuse the comm object form the registration for both load and write ops. Do not take comm objects again to avoid confusion.
4) Decipher the global comm object and the subset partioning for each application based on the comm object
5) GetMeshInfo - return separately the owned and ghosted vertices/elements -- not together in visible_** but rather owned_** and ghosted_**. Make these arrays of size 2.
6) Should we sort the vertices after we do ghosting ? So that the locally owned is first and then the ghosted is appended.
7) RCM only for hte owned part of the mesh -- do not screw with the ghosted layers
8) GetBlockID - identical to GetVertexID -- return the global numberign for block
9) GetVertexID -- remember that the order of vertices returned have an implicit numbering embedded in it. DO NOT CHANGE THIS ORDERING...
10) GetBlockInfo takes global Block ID; Remove blockname unless there is a separate use case for it..
11) GetElementConnectivity - clarify whether we return global or local vertex numbering. Preferably local numbering else lot of deciphering for global.
*/

#include "moab/MOABConfig.h"

#define iMOAB_AppID    int*
#define iMOAB_String   char*
#define iMOAB_GlobalID int
#define iMOAB_LocalID  int
#define ErrCode        int

#include "imoab_protos.h"
/*
 * tag types can be: dense/sparse, int/ double/entityhandle , they could be on both elements and vertices
 */
enum MOAB_TAG_TYPE { DENSE_INTEGER = 0,
                     DENSE_DOUBLE ,
                     DENSE_ENTITYHANDLE,
                     SPARSE_INTEGER ,
                     SPARSE_DOUBLE ,
                     SPARSE_ENTITYHANDLE
};

#ifdef __cplusplus
extern "C" {
#endif

/** 
  \brief Initialize the iMOAB interface implementation.

   Will create the MOAB instance, if not created already (reference counted).

  <B>Operations:</B> Collective

  \param[in] argc (int)           Number of command line arguments 
  \param[in] argv (iMOAB_String*) Command line arguments
*/
ErrCode iMOAB_Initialize( int argc, iMOAB_String* argv );

/** 
  \brief Initialize the iMOAB interface implementation from Fortran driver.

   It will create the MOAB instance, if not created already (reference counted).

  <B>Operations:</B> Collective
*/
ErrCode iMOAB_InitializeFortran( );

/**
  \brief Finalize the iMOAB interface implementation.

   It will delete the internally reference counted MOAB instance, if the reference count reaches 0.

  <B>Operations:</B> Collective
*/
ErrCode iMOAB_Finalize();

/**
  \brief Register application - Create a unique application ID and bootstrap interfaces for further queries.
  
  \note
  Internally, a mesh set will be associated with the application ID and all subsequent queries on the MOAB
  instance will be directed to this mesh/file set.

  <B>Operations:</B> Collective

  \param[in]  app_name (iMOAB_String) Application name (PROTEUS, NEK5000, etc)
  \param[in]  comm (MPI_Comm*)        MPI communicator to be used for all mesh-related queries originating from this application
  \param[out] pid (iMOAB_AppID)       The unique pointer to the application ID
*/
ErrCode iMOAB_RegisterApplication( const iMOAB_String app_name,
#ifdef MOAB_HAVE_MPI
    MPI_Comm* comm,
#endif
    iMOAB_AppID pid );

/**
   \brief Register a Fortran-based application - Create a unique application ID and bootstrap interfaces for further queries.
  
  \note
  Internally, the comm object, usually a 32 bit integer in Fortran, will be converted using MPI_Comm_f2c and stored as MPI_Comm. A mesh set will be associated with the
  application ID and all subsequent queries on the MOAB instance will be directed to this mesh/file set.

  <B>Operations:</B> Collective

  \param[in]  app_name (iMOAB_String) Application name (PROTEUS, NEK5000, etc)
  \param[in]  comm (int*)             Fortran MPI communicator to be used for all mesh-related queries originating from this application
  \param[out] pid (iMOAB_AppID)       The unique pointer to the application ID
  \param[in]  app_name_length (int)   Length of application name string.
*/
ErrCode iMOAB_RegisterFortranApplication( const iMOAB_String app_name,
#ifdef MOAB_HAVE_MPI
    int* comm,
#endif
    iMOAB_AppID pid, int app_name_length );

/**
  \brief De-Register application: delete mesh (set) associated with the application ID.

  The associated communicator will be released, and all associated mesh entities and sets will be deleted from the
  mesh data structure. Associated tag storage data will be freed too.

  <B>Operations:</B> Collective

  \param[in] pid (iMOAB_AppID) The unique pointer to the application ID
*/
ErrCode iMOAB_DeregisterApplication( iMOAB_AppID pid );

/**
    \brief Get global information from the file.

    It should be called on master task only, and information obtained could be broadcasted by the user. It is a fast
    lookup in the header of the file.

  <B>Operations:</B> Not collective

  \param[in]  filename (iMOAB_String)    The MOAB mesh file (H5M) to probe for header information
  \param[out] num_global_vertices (int*) The total number of vertices in the mesh file
  \param[out] num_global_elements (int*) The total number of elements (of highest dimension only) 
  \param[out] num_dimension (int*)       The highest dimension of elements in the mesh (Edge=1, Tri/Quad=2, Tet/Hex/Prism/Pyramid=3)
  \param[out] num_parts (int*)           The total number of partitions available in the mesh file, typically partitioned with mbpart during pre-processing
  \param[in]  filename_length (int)      Length of the file name string
*/
ErrCode iMOAB_ReadHeaderInfo ( const iMOAB_String filename, int* num_global_vertices, int* num_global_elements, int* num_dimension, int* num_parts, int filename_length );

/**
   \brief Load a MOAB mesh file in parallel and exchange ghost layers as requested.

   All communication is MPI-based, and read options include parallel loading information, resolving
   shared entities. Local MOAB instance is populated with mesh cells and vertices in the corresponding
   local partitions.

  \note
  This will also exchange ghost cells and vertices, as requested. The default bridge dimension is 0 (vertices),
  and all additional lower dimensional sub-entities are exchanged (mesh edges and faces). The tags in the file are not
  exchanged by default. Default tag information for GLOBAL_ID, MATERIAL_SET, NEUMANN_SET and DIRICHLET_SET is
  exchanged. Global ID tag is exchanged for all cells and vertices. Material sets, Neumann sets and Dirichlet sets
  are all augmented with the ghost entities.

  <B>Operations:</B> Collective

  \param[in] pid (iMOAB_AppID)            The unique pointer to the application ID
  \param[in] filename (iMOAB_String)      The MOAB mesh file (H5M) to load onto the internal application mesh set
  \param[in] read_options (iMOAB_String)  Additional options for reading the MOAB mesh file in parallel 
  \param[in] num_ghost_layers (int*)      The total number of ghost layers to exchange during mesh loading
  \param[in] filename_length (int)        Length of the filename string
  \param[in] read_options_length (int)    Length of the read options string  
*/
ErrCode iMOAB_LoadMesh( iMOAB_AppID pid, const iMOAB_String filename, const iMOAB_String read_options, int * num_ghost_layers, int filename_length, int read_options_length );

/**
  \brief Write a MOAB mesh along with the solution tags to a file.

  \note
  The interface will write one single file (H5M) and for serial files (VTK/Exodus), it will write one file per task.
  Write options include parallel write options, if needed. Only the mesh set and solution data associated to the application will be
  written to the file.

  <B>Operations:</B> Collective for parallel write, non collective for serial write.

  \param[in] pid (iMOAB_AppID)            The unique pointer to the application ID
  \param[in] filename (iMOAB_String)      The MOAB mesh file (H5M) to write all the entities contained in the internal application mesh set
  \param[in] write_options (iMOAB_String) Additional options for writing the MOAB mesh in parallel
  \param[in] filename_length (int)       Length of the filename string
  \param[in] write_options_length (int)  Length of the write options string
*/
ErrCode iMOAB_WriteMesh( iMOAB_AppID pid, iMOAB_String filename, iMOAB_String write_options, int filename_length, int write_options_length );

/**
   \brief Obtain local mesh information based on the loaded file.

   Number of visible vertices and cells include ghost entities. All arrays returned have size 3.
   Local entities are first, then ghost entities are next. Shared vertices can be owned in MOAB sense by
   different tasks. Ghost vertices and cells are always owned by other tasks.

  <B>Operations:</B> Not Collective

  \param[in]  pid (iMOAB_AppID)            The unique pointer to the application ID
  \param[out] num_visible_vertices (int*)  The number of vertices in the current partition/process arranged as: owned/shared only, ghosted, total_visible (array allocated by client, <TT>size := 3</TT>)
  \param[out] num_visible_elements (int*)  The number of elements in current partition/process arranged as: owned only, ghosted/shared, total_visible (array allocated by client, <TT>size := 3</TT>)
  \param[out] num_visible_blocks (int*)    The number of material sets in local mesh in current partition/process arranged as: owned only, ghosted/shared, total_visible (array allocated by client, <TT>size := 3</TT>)
  \param[out] num_visible_surfaceBC (int*) The number of mesh surfaces that have a NEUMANN_SET BC defined in local mesh in current partition/process arranged as: owned only, ghosted/shared, total_visible (array allocated by client, <TT>size := 3</TT>)
  \param[out] num_visible_vertexBC (int*)  The number of vertices that have a DIRICHLET_SET BC defined in local mesh in current partition/process arranged as: owned only, ghosted/shared, total_visible (array allocated by client, <TT>size := 3</TT>)
*/
ErrCode iMOAB_GetMeshInfo( iMOAB_AppID pid, int* num_visible_vertices, int* num_visible_elements, int *num_visible_blocks, int* num_visible_surfaceBC, int* num_visible_vertexBC );

/**
   \brief Get the global vertex IDs for all locally visible (owned and shared/ghosted) vertices.

   The array should be allocated by the client, sized with the total number of visible vertices from
   iMOAB_GetMeshInfo method.

  <B>Operations:</B> Not collective

  \param[in]  pid (iMOAB_AppID)                   The unique pointer to the application ID
  \param[in]  vertices_length (int*)              The allocated size of array (typical <TT>size := num_visible_vertices</TT>)
  \param[out] global_vertex_ID (iMOAB_GlobalID*)  The global IDs for all locally visible vertices (array allocated by client)
*/
ErrCode iMOAB_GetVertexID( iMOAB_AppID pid, int * vertices_length, iMOAB_GlobalID* global_vertex_ID );

/**
   \brief Get vertex ownership information.

   For each vertex based on the local ID, return the process that owns the vertex (local, shared or ghost)

  \note
  Shared vertices could be owned by different tasks. Local and shared vertices are first, ghost vertices are next
  in the array. Ghost vertices are always owned by a different process ID. Array allocated by the client with
  total size of visible vertices.

  <B>Operations:</B> Not Collective

  \param[in]  pid (iMOAB_AppID)             The unique pointer to the application ID
  \param[in]  vertices_length (int*)         The allocated size of array (typically <TT>size := num_visible_vertices</TT>)
  \param[out] visible_global_rank_ID (int*) The processor rank owning each of the local vertices 
*/
ErrCode iMOAB_GetVertexOwnership( iMOAB_AppID pid, int * vertices_length, int* visible_global_rank_ID );

/**
  \brief Get vertex coordinates for all local (owned and ghosted) vertices.

  \note coordinates are returned in an array allocated by client, interleaved. (do need an option for blocked coordinates ?)
  size of the array is dimension times number of visible vertices. The local ordering is implicit, owned/shared vertices are first, then
  ghosts.

  <B>Operations:</B> Not Collective

  \param[in]  pid (iMOAB_AppID)     The unique pointer to the application ID
  \param[in]  coords_length (int*)   The size of the allocated coordinate array (array allocated by client, <TT>size := 3*num_visible_vertices</TT>)
  \param[out] coordinates (double*) The pointer to client allocated memory that will be filled with interleaved coordinates

*/
ErrCode iMOAB_GetVisibleVerticesCoordinates( iMOAB_AppID pid, int * coords_length, double* coordinates );

/**
  \brief Get the global block IDs for all locally visible (owned and shared/ghosted) blocks.

  Block IDs are corresponding to MATERIAL_SET tags for material sets. Usually the block ID is exported from Cubit
  as a unique integer value. First blocks are local, and next blocks are fully ghosted. First blocks have at least
  one owned cell/element, ghost blocks have only ghost cells. Internally, a block corresponds to a mesh set with a MATERIAL_SET tag
  value equal to the block ID.

  <B>Operations:</B> Not Collective

  \param[in]  pid (iMOAB_AppID)                  The unique pointer to the application ID
  \param[in]  block_length (int*)                The allocated size of array (typical <TT>size := num_visible_blocks</TT>)
  \param[out] global_block_IDs (iMOAB_GlobalID*) The global IDs for all locally visible blocks (array allocated by client)
*/
ErrCode iMOAB_GetBlockID( iMOAB_AppID pid, int * block_length, iMOAB_GlobalID* global_block_IDs);

/**
  \brief Get the global block information and number of visible elements of belonging to a block (MATERIAL SET).

  A block has to be homogeneous, it can contain elements of a single type

  <B>Operations:</B> Not Collective

  \param[in]  pid (iMOAB_AppID)                 The unique pointer to the application ID
  \param[in]  global_block_ID (iMOAB_GlobalID)  The global block ID of the set to be queried
  \param[out] vertices_per_element (int*)       The number of vertices per element
  \param[out] num_elements_in_block (int*)      The number of elements in block
*/
ErrCode iMOAB_GetBlockInfo(iMOAB_AppID pid, iMOAB_GlobalID * global_block_ID, int* vertices_per_element, int* num_elements_in_block);

/**
  \brief Get the visible elements information.

  Return for all visible elements the global IDs, ranks they belong to, block ids they belong to.
  \note : should we return also the index for each block?

  <B>Operations:</B> Not Collective

  \param[in]  pid (iMOAB_AppID)                     The unique pointer to the application ID
  \param[in]  num_visible_elements (int*)           The number of visible elements (returned by GetMeshInfo)
  \param[out] element_global_IDs (iMOAB_GlobalID*)  element global ids
  \param[out] ranks (int*)                          The owning ranks of elements
  \param[out] block_IDs (iMOAB_GlobalID*)           The block ids the elements belong to
*/
ErrCode iMOAB_GetVisibleElementsInfo(iMOAB_AppID pid, int* num_visible_elements,
    iMOAB_GlobalID * element_global_IDs, int * ranks, iMOAB_GlobalID * block_IDs);


/** 
  \brief Get the connectivities for elements within a certain block.

  \note input is the block ID. Should we change to visible block local index?

  <B>Operations:</B> Not Collective

  \param[in]  pid (iMOAB_AppID)                 The unique pointer to the application ID
  \param[in]  global_block_ID (iMOAB_GlobalID*) The global block ID of the set being queried
  \param[in]  connectivity_length (int*)        The allocated size of array (typical <TT>size := vertices_per_element*num_elements_in_block</TT>)
  \param[out] element_connectivity (int*)       The connectivity array to store element ordering in MOAB canonical numbering scheme (array allocated by client);
    array contains vertex indices in the local numbering order for vertices
    elements are in the same order as provided by GetElementOwnership and GetElementID
*/
ErrCode iMOAB_GetBlockElementConnectivities(iMOAB_AppID pid, iMOAB_GlobalID * global_block_ID, int * connectivity_length, int* element_connectivity);


/**
  \brief Get the connectivity for one element only.

  \note it was added for convenience, it should not be needed.

  <B>Operations:</B> Not Collective
  \param[in]  pid (iMOAB_AppID)                 The unique pointer to the application ID.
  \param[in]  elem_index (iMOAB_LocalID *)      Local element index.
  \param[in,out] connectivity_length (int *)     On input, maximum length of connectivity. On output, actual length.
  \param[out] element_connectivity (int*)       The connectivity array to store connectivity in MOAB canonical numbering scheme.
   Array contains vertex indices in the local numbering order for vertices

*/
ErrCode iMOAB_GetElementConnectivity(iMOAB_AppID pid, iMOAB_LocalID * elem_index, int * connectivity_length, int * element_connectivity);


/**
  \brief Get the element ownership within a certain block i.e., processor ID of the element owner.

  \note : Should we use local block index for input, instead of the global block ID ?

  <B>Operations:</B> Not Collective

  \param[in]  pid (iMOAB_AppID)                 The unique pointer to the application ID
  \param[in]  global_block_ID (iMOAB_GlobalID)  The global block ID of the set being queried
  \param[in]  num_elements_in_block (int*)      The allocated size of ownership array, same as <TT>num_elements_in_block</TT> returned from GetBlockInfo()
  \param[out] element_ownership (int*)          The ownership array to store processor ID for all elements (array allocated by client) 
*/
ErrCode iMOAB_GetElementOwnership(iMOAB_AppID pid, iMOAB_GlobalID * global_block_ID, int * num_elements_in_block, int* element_ownership);

/**
  \brief Get the global IDs for all locally visible elements belonging to a particular block.

  The method will return also the local index of each element, in the local range that contains all visible elements

  <B>Operations:</B> Not Collective

  \param[in]  pid (iMOAB_AppID)                   The unique pointer to the application ID
  \param[in]  global_block_ID (iMOAB_GlobalID*)   The global block ID of the set being queried
  \param[in]  num_elements_in_block (int*)        The allocated size of global element ID array, same as <TT>num_elements_in_block</TT> returned from GetBlockInfo()
  \param[out] global_element_ID (iMOAB_GlobalID*) The global IDs for all locally visible elements (array allocated by client)
  \param[out] local_element_ID (iMOAB_LocalID*)   (<I><TT>Optional</TT></I>) The local IDs for all locally visible elements (index in the range of all primary elements in the rank)
*/
ErrCode iMOAB_GetElementID(iMOAB_AppID pid, iMOAB_GlobalID * global_block_ID, int * num_elements_in_block, iMOAB_GlobalID* global_element_ID, iMOAB_LocalID* local_element_ID);

/**
  \brief Get the surface boundary condition information.

  <B>Operations:</B> Not Collective

  \param[in]  pid (iMOAB_AppID)                   The unique pointer to the application ID
  \param[in]  surface_BC_length (int*)             The allocated size of surface boundary condition array, same as <TT>num_visible_surfaceBC</TT> returned by GetMeshInfo()
  \param[out] local_element_ID (iMOAB_LocalID*)   The local element IDs that contains the side with the surface BC
  \param[out] reference_surface_ID (int*)         The surface number with the BC in the canonical reference element (e.g., 1 to 6 for HEX, 1-4 for TET)
  \param[out] boundary_condition_value (int*)     The boundary condition type as obtained from the mesh description (value of the NeumannSet defined on the element)
*/
ErrCode iMOAB_GetPointerToSurfaceBC(iMOAB_AppID pid, int * surface_BC_length, iMOAB_LocalID* local_element_ID, int* reference_surface_ID, int* boundary_condition_value);

/**
  \brief Get the vertex boundary condition information

  <B>Operations:</B> Not Collective

  \param[in]  pid (iMOAB_AppID)                   The unique pointer to the application ID
  \param[in]  vertex_BC_length (int)              The allocated size of vertex boundary condition array, same as <TT>num_visible_vertexBC</TT> returned by GetMeshInfo()
  \param[out] local_vertex_ID (iMOAB_LocalID*)    The local vertex ID that has Dirichlet BC defined
  \param[out] boundary_condition_value (int*)     The boundary condition type as obtained from the mesh description (value of the Dirichlet_Set tag defined on the vertex)
*/
ErrCode iMOAB_GetPointerToVertexBC(iMOAB_AppID pid, int * vertex_BC_length, iMOAB_LocalID* local_vertex_ID, int* boundary_condition_value);

/**
  \brief Define a MOAB Tag corresponding to the application depending on requested types.

  \note
  In MOAB, for most solution vectors, we only need to create a "Dense", "Double" Tag. A sparse tag can
  be created too. If the tag is already existing in the file, it will not be created. If it is a new tag,
  memory will be allocated when setting the values. Default values are 0 for for integer tags, 0.0 for double tags,
  0 for entity handle tags.

  <B>Operations:</B> Collective

   \param[in] pid (iMOAB_AppID)               The unique pointer to the application ID
   \param[in] tag_storage_name (iMOAB_String) The tag name to store/retrieve the data in MOAB
   \param[in] tag_type (int*)                 The type of MOAB tag (Dense/Sparse, Double/Int/EntityHandle), enum MOAB_TAG_TYPE
   \param[in] components_per_entity (int*)    The total size of vector dimension per entity for the tag (e.g., number of doubles per entity)
   \param[out] tag_index (int*)               The tag index which can be used as identifier in synchronize methods
   \param[in] tag_storage_name_length (int)   The length of the tag_storage_name string
*/
ErrCode iMOAB_DefineTagStorage(iMOAB_AppID pid, const iMOAB_String tag_storage_name, int* tag_type, int* components_per_entity, int * tag_index, int tag_storage_name_length);

/**
    \brief Store the specified values in a MOAB integer Tag.

    Values are set on vertices or elements, depending on entity_type

   \note we could change the api to accept as input the tag index, as returned by iMOAB_DefineTagStorage.

   <B>Operations:</B> Collective

   \param[in]  pid (iMOAB_AppID)                       The unique pointer to the application ID
   \param[in]  tag_storage_name (iMOAB_String)         The tag name to store/retreive the data in MOAB
   \param[in]  num_tag_storage_length (int*)           The size of tag storage data (e.g., num_visible_vertices*components_per_entity or num_visible_elements*components_per_entity)
   \param[in]  entity_type (int*)                      type 0 for vertices, 1 for primary elements
   \param[out] tag_storage_data (int*)                 The array data of type <I>int</I> to replace the internal tag memory; The data is assumed to be contiguous over the local set of visible entities (either vertices or elements)
   \param[in]  tag_storage_name_length (iMOAB_String)  The length of the tag_storage_name string
*/
ErrCode iMOAB_SetIntTagStorage(iMOAB_AppID pid, const iMOAB_String tag_storage_name, int * num_tag_storage_length, int * entity_type, int* tag_storage_data, int tag_storage_name_length);

/**
   \brief Get the specified values in a MOAB integer Tag.

   <B>Operations:</B> Collective

   \param[in]  pid (iMOAB_AppID)                       The unique pointer to the application ID
   \param[in]  tag_storage_name (iMOAB_String)         The tag name to store/retreive the data in MOAB
   \param[in]  num_tag_storage_length (int*)            The size of tag storage data (e.g., num_visible_vertices*components_per_entity or num_visible_elements*components_per_entity)
   \param[in]  entity_type (int*)                      type 0 for vertices, 1 for primary elements
   \param[out] tag_storage_data (int*)                 The array data of type <I>int</I> to be copied from the internal tag memory; The data is assumed to be contiguous over the local set of visible entities (either vertices or elements)
   \param[in]  tag_storage_name_length (iMOAB_String)  The length of the tag_storage_name string
*/
ErrCode iMOAB_GetIntTagStorage(iMOAB_AppID pid, const iMOAB_String tag_storage_name, int *num_tag_storage_length, int * entity_type, int* tag_storage_data, int tag_storage_name_length);

/**
   \brief Store the specified values in a MOAB double Tag.

   <B>Operations:</B> Collective

   \param[in]  pid (iMOAB_AppID)                       The unique pointer to the application ID
   \param[in]  tag_storage_name (iMOAB_String)         The tag name to store/retreive the data in MOAB
   \param[in]  num_tag_storage_length (int*)            The size of tag storage data (e.g., num_visible_vertices*components_per_entity or num_visible_elements*components_per_entity)
   \param[in]  entity_type (int*)                      type 0 for vertices, 1 for primary elements
   \param[out] tag_storage_data (double*)              The array data of type <I>double</I> to replace the internal tag memory; The data is assumed to be contiguous over the local set of visible entities (either vertices or elements)
   \param[in]  tag_storage_name_length (iMOAB_String)  The length of the tag_storage_name string
*/
ErrCode iMOAB_SetDoubleTagStorage(iMOAB_AppID pid, const iMOAB_String tag_storage_name, int * num_tag_storage_length, int * entity_type, double* tag_storage_data, int tag_storage_name_length);

/**
   \brief Retrieve the specified values in a MOAB double Tag.

   <B>Operations:</B> Collective

   \param[in]  pid (iMOAB_AppID)                The unique pointer to the application ID
   \param[in]  tag_storage_name (iMOAB_String)  The tag name to store/retreive the data in MOAB
   \param[in]  num_tag_storage_length (int)     The size of tag storage data (e.g., num_visible_vertices*components_per_entity or num_visible_elements*components_per_entity)
   \param[in]  entity_type (int*)                      type 0 for vertices, 1 for primary elements
   \param[out] tag_storage_data (double*)       The array data of type <I>double</I> to be copied from the internal tag memory; The data is assumed to be contiguous over the local set of visible entities (either vertices or elements)
   \param[in]  tag_storage_name_length (int)    The length of the tag_storage_name string
*/
ErrCode iMOAB_GetDoubleTagStorage(iMOAB_AppID pid, const iMOAB_String tag_storage_name, int * num_tag_storage_length, int * entity_type, double* tag_storage_data, int tag_storage_name_length);

/**
   \fn ErrCode iMOAB_SynchronizeTags(iMOAB_AppID pid,  int * num_tags, int * tag_indices, int * ent_type )
   \brief Exchange tag values for given tags

   <B>Operations:</B> Collective

   \param[in]  pid (iMOAB_AppID)                The unique pointer to the application ID
   \param[in]  num_tags (int*)                  Number of tags to exchange
   \param[in]  tag_indices (int*)               Array with tag indices of interest (size  = *num_tags)
   \param[in]  ent_type (int*)                  type of entity for tag exchange
  */
ErrCode iMOAB_SynchronizeTags(iMOAB_AppID pid, int * num_tag, int * tag_indices, int * ent_type);

/**
   \brief retrieve the adjacencies for the element entities.

   <B>Operations:</B> Not Collective

   \param[in]  pid (iMOAB_AppID)                      The unique pointer to the application ID
   \param[in]  local_index (iMOAB_LocalID*)           The local element ID for which adjacency information is needed
   \param[out] num_adjacent_elements (int*)           The total number of adjacent elements
   \param[out] adjacent_element_IDs (iMOAB_LocalID*)  The local element IDs of all adjacent elements to the current one (typically, num_total_sides for internal elements or num_total_sides-num_sides_on_boundary for boundary elements)
*/
ErrCode iMOAB_GetNeighborElements(iMOAB_AppID pid, iMOAB_LocalID * local_index, int* num_adjacent_elements, iMOAB_LocalID* adjacent_element_IDs);

/**
   \brief Get the adjacencies for the vertex entities. (not implemented yet)

   <B>Operations:</B> Not Collective

   \param[in]  pid (iMOAB_AppID)                      The unique pointer to the application ID
   \param[in]  local_vertex_ID (iMOAB_LocalID*)       The local vertex ID for which adjacency information is needed
   \param[out] num_adjacent_vertices (int*)           The total number of adjacent vertices
   \param[out] adjacent_vertex_IDs (iMOAB_LocalID*)   The local element IDs of all adjacent vertices to the current one (typically, num_total_sides for internal elements or num_total_sides-num_sides_on_boundary for boundary elements)
*/
ErrCode iMOAB_GetNeighborVertices(iMOAB_AppID pid, iMOAB_LocalID* local_vertex_ID, int* num_adjacent_vertices, iMOAB_LocalID* adjacent_vertex_IDs);

#ifdef __cplusplus
}
#endif

#endif
