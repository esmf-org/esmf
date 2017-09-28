#ifndef MHDF_PUBLIC_H
#define MHDF_PUBLIC_H

#include <H5Tpublic.h>

#ifdef __cplusplus
  extern "C" {
#endif


#define MHDF_MESSAGE_BUFFER_LEN 160

/** \brief Struct used to return error status. */
typedef struct struct_mhdf_Status { char message[MHDF_MESSAGE_BUFFER_LEN]; } mhdf_Status;
/* another name/alias/typedef for mhdf_Status*/
typedef mhdf_Status MHDF_Status;

/** \brief Return 1 if passed status object indicates an error.  Zero otherwise. */
int
mhdf_isError( mhdf_Status const* );

/** \brief Get the error message given a status object.  */
const char*
mhdf_message( mhdf_Status const* );

/*@}*/

/**
 *\defgroup mhdf_type Common element type names.
 */
/*@{*/

/** \brief Name to use for edge element */
#define mhdf_EDGE_TYPE_NAME        "Edge"
/** \brief Name to use for triangle element */
#define mhdf_TRI_TYPE_NAME         "Tri"
/** \brief Name to use for quadrilateral element */
#define mhdf_QUAD_TYPE_NAME        "Quad"
/** \brief Name to use for general polygon element */
#define mhdf_POLYGON_TYPE_NAME     "Polygon"
/** \brief Name to use for tetrahedral element */
#define mhdf_TET_TYPE_NAME         "Tet"
/** \brief Name to use for quad-based pyramid element */
#define mhdf_PYRAMID_TYPE_NAME     "Pyramid"
/** \brief Name to use for triangular prism element */
#define mhdf_PRISM_TYPE_NAME       "Prism"
/** \brief Name to use for knife element */
#define mdhf_KNIFE_TYPE_NAME       "Knife"
/** \brief Name to use for quad-sided hexahedral element */
#define mdhf_HEX_TYPE_NAME         "Hex"
/** \brief Name to use for general polyhedron specified as a arbitrary-length list of faces */
#define mhdf_POLYHEDRON_TYPE_NAME  "Polyhedron"
/** \brief Name to use for hexagonal-based pyramid */
#define mhdf_SEPTAHEDRON_TYPE_NAME "Septahedron"

/*@}*/

/** \brief Enum for tag data type class
 *
 * Enumerates known types for tag data
 */
typedef enum mhdf_TagDataType {
  mhdf_OPAQUE = 0, /**< Opaque/unknown type */
  mhdf_INTEGER,    /**< Integer type */
  mhdf_FLOAT,      /**< Floating point value */
  mhdf_BITFIELD,   /**< Bit field */
  mhdf_BOOLEAN,    /**< Boolean values stored as one byte each */
  mhdf_ENTITY_ID   /**< Global ID referencing another entity in file */
} MHDF_TagDataType;

/**\brief Type used when creating index tables
 *
 * The data type used by mhdf_create* functions that create tables
 * if indices (e.g. mhdf_createSetMeta, mhdf_createVarLenTag, etc.).
 */
typedef long mhdf_index_t;


/*@}*/

/**
 *\defgroup mhdf_file File operations
 */
/*@{*/

/** \brief Opaque handle to an open file */
typedef void* mhdf_FileHandle;
/* another name/alias/typedef for mhdf_FileHandle*/
typedef mhdf_FileHandle MHDF_FileHandle;


/* Top level file operations */

/** \brief Create a new file.
 *
 * Create a new HDF mesh file.  This handle must be closed with
 * <code>mhdf_closeFile</code> to avoid resource loss.
 *
 * \param filename   The path and name of the file to create
 * \param overwrite  If zero, will fail if the specified file
 *                   already exists.  If non-zero, will overwrite
 *                   an existing file.
 * \param elem_type_list The list of element types that will be stored
 *                   in the file.  If the element name exits as a pre-
 *                   defined constant (\ref mhdf_type), that constant
 *                   should be used.  If a constant does not exist for
 *                   the type, a similar naming pattern should be used
 *                   (accepted name for type, first character uppercase,
 *                   subsequent characters lowercase.)  The element type
 *                   index passed to \ref mhdf_addElement is then an
 *                   index into this list.  The array may contain
 *                   null entries to allow the caller some control over
 *                   the assigned indices without creating dummy types
 *                   which may confuse readers.
 * \param elem_type_list_len The length of <code>elem_type_list</code>.
 * \param id_type    Type to use when creating datasets containing file IDs
 * \param status     Passed back status of API call.
 * \return An opaque handle to the file.
 */
mhdf_FileHandle
mhdf_createFile( const char* filename,
                 int overwrite,
                 const char** elem_type_list,
                 size_t elem_type_list_len,
                 hid_t id_type,
                 mhdf_Status* status );

/** \brief Open an existing file.
 *
 * Open an existing HDF mesh file.  This handle must be closed with
 * <code>mhdf_closeFile</code> to avoid resource loss.
 *
 * \param filename   The path and name of the file to open
 * \param writable  If non-zero, open read-write.  Otherwise read-only.
 * \param status     Passed back status of API call.
 * \param max_id     Used to pass back the maximum global ID used in the
 *                   file.  Provided as an indication to the caller of the
 *                   size of the mesh.  This parameter is optional.  NULL
 *                   may be passed.
 * \param id_type    Type to use when creating datasets containing file IDs
 * \return An opaque handle to the file.
 */
mhdf_FileHandle
mhdf_openFile( const char* filename,
               int writable,
               unsigned long* max_id,
               hid_t id_type,
               mhdf_Status* status );

/** \brief Open an existing file with options.
 *
 * Open an existing HDF mesh file.  This handle must be closed with
 * <code>mhdf_closeFile</code> to avoid resource loss.  This function
 * allows the calling application to specify the HDF5 access property
 * list that is passed to the HDF5 H5Fopen API.  If this is passed as
 * H5P_DEFAULT, the behavior is the same as \ref mhdf_openFile .
 * This argument is typically used to specify a parallel context for
 * for writing the file in parallel.
 *
 * \param filename   The path and name of the file to open
 * \param writable  If non-zero, open read-write.  Otherwise read-only.
 * \param status     Passed back status of API call.
 * \param max_id     Used to pass back the maximum global ID used in the
 *                   file.  Provided as an indication to the caller of the
 *                   size of the mesh.  This parameter is optional.  NULL
 *                   may be passed.
 * \param options    The HDF5 access property list to use when opening
 *                   the file.  See the HDF5 documentation for H5Fopen.
 * \param id_type    Type to use when creating datasets containing file IDs
 * \return An opaque handle to the file.
 */
mhdf_FileHandle
mhdf_openFileWithOpt( const char* filename,
                      int writable,
                      unsigned long* max_id,
                      hid_t id_type,
                      hid_t options,
                      mhdf_Status* status );

/**\brief Get number of open HDF5 objects from file */
int
mhdf_countOpenHandles( mhdf_FileHandle h );

/** Data common to sets, nodes, and each element type */
typedef struct mhdf_EntDesc {
  long start_id;           /**< First file ID for table of data */
  long count;              /**< Number of entities in table */
  int vals_per_ent;        /**< Connectivity length for elems, dimension for verts, unused for sets, -1 for variable length poly* data */
  int* dense_tag_indices;  /**< Indices into mhdf_FileDesc::tags for each tag for which dense data is present for these entities */
  int num_dense_tags;      /**< Length of dense_tag_indices */
} MHDF_EntDesc;
/** Struct describing a tag */
typedef struct mhdf_TagDesc {
  const char* name;           /**< Tag name */
  enum mhdf_TagDataType type; /**< Data type */
  int size;                   /**< Tag size (num of data type) */
  int bytes;                  /**< Tag size (number of bytes) */
  int storage;                /**< MOAB tag type (dense or sparse) */
  int have_sparse;            /**< Have sparse id/data pairs in file */
  void* default_value;        /**< Default value, NULL if none. */
  int default_value_size;
  void* global_value;         /**< Global value, NULL if none. */
  int global_value_size;
  int* dense_elem_indices;    /**< Array of indices indicating element types for which dense
                                   data is stored.  -2 for sets, -1 for nodes. */
  int num_dense_indices;
} MHDF_TagDesc;
typedef struct mhdf_ElemDesc {
  const char* handle;       /**< String table identifier */
  const char* type;         /**< String type designator */
  int have_adj;             /**< File contains adjacency data for this element group */
  struct mhdf_EntDesc desc;
} MHDF_ElemDesc;
typedef struct mhdf_FileDesc {
  struct mhdf_EntDesc nodes;
  struct mhdf_EntDesc sets;
  int have_set_contents;
  int have_set_children;
  int have_set_parents;
  struct mhdf_ElemDesc* elems; /**< Array of element table descriptions */
  int num_elem_desc;
  struct mhdf_TagDesc* tags;   /**< Array of tag descriptions */
  int num_tag_desc;
  int * numEntSets ; /* used to be [4] */
  /*int num_parts; will look specifically for number of sets with PARALLEL_PARTITION tags*/
  /* int num_mats; will look specifically for number of sets with MATERIAL_SET tags*/
  /*int num_neumann; will look specifically for number of sets with NEUMANN_SET tags*/
  /*int num_diri; will look specifically for number of sets with DIRICHLET_SET tags*/
  int **defTagsEntSets; /* we may need to add geometry_dimension tags */
  int **defTagsVals ;
  size_t total_size;           /**< Size of memory block containing all struct data */
  unsigned char* offset;       /**< Unused, may be used by application */
} MHDF_FileDesc;

/** \brief Get summary of data tables contained within file.
 *
 * Returned struct, including all pointed-to data, is allocated in a
 * single contiguous block of memory with a size equal to 'total_size'.
 * Caller is responsible for freeing the returned struct FileDesc pointer
 * (and *only* that pointer, not pointers nexted within the struct!).
 * Caller may copy (e.g. MPI_BCast) entire struct as one contiguous block,
 * assuming all nested pointers in the copy are updated to the correct
 * relative offset from the beginning of the struct.
 */
MHDF_FileDesc *
mhdf_getFileSummary( mhdf_FileHandle file_handle,
                     hid_t file_id_type,
                     mhdf_Status* status, int extraSetInfo);

/**\brief Fix nested pointers for copied/moved FileDesc struct
 *
 * This is a utility method to facility copying/moving/communicating
 * struct FileDesc instances.  The structure and all data it references
 * are allocated in a single contiguous block of memory of size
 * FileDesc::total_size.  As such, the struct can be copied with a single
 * memcpy, packed into a single network packet, communicated with a single
 * MPI call, etc.  However, the pointers contained within the struct will
 * not be valid in the copied instance (they will still point into the
 * original instance.)  Given a pointer to the copied struct and the address
 * of the original struct, this function will updated all contained pointers.
 */
void
mhdf_fixFileDesc( struct mhdf_FileDesc* copy_ptr, const struct mhdf_FileDesc* orig_addr );

/** \brief Close the file
 * \param handle     The file to close.
 * \param status     Passed back status of API call.
 */
void
mhdf_closeFile( mhdf_FileHandle handle,
                mhdf_Status* status );

/**\brief Check for open handles in file
 **/

#ifdef __cplusplus
} /* extern "C" */
#endif


#endif /* MHDF_PUBLIC_H*/
