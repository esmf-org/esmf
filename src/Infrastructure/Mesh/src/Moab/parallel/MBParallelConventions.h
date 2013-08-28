#ifndef MB_PARALLEL_CONVENTIONS_H
#define MB_PARALLEL_CONVENTIONS_H

/** Tag conventions for naming parallel things.  Note this header
 * file belongs in the main MOAB directory because even serial
 * applications (e.g. partitioners) may write tags for use in
 * parallel applications.
 */

/** \brief Global identifier for interface mesh
 *
 * An integer identifier common to the corresponding mesh entity
 * instances on each processor for a mesh entity on the interface.
 */
#define PARALLEL_GID_TAG_NAME "GLOBAL_ID"

/** \brief Tag on a meshset representing a parallel partition.
 *
 * When the mesh is partitioned for use in a parallel environment,
 * the each CPUs partiiton of the mesh is stored in a meshset with
 * this tag.  The value of the tag is an integer "part identifier".
 */
#define PARALLEL_PARTITION_TAG_NAME "PARALLEL_PARTITION"
#define PARALLEL_PART_TAG_NAME PARALLEL_PARTITION_TAG_NAME

/** \brief Tag that groups the set of parts/partitions that are
 *         a covering of the mesh.
 *
 * This tag labels an entity set for which the child sets are part(ition)s
 * that together are a single partitioning of the mesh.  I.e. There should
 * be no mesh entity that is contained in more than one child part(ition)
 * set, and typically every mesh entity of the dimenion used to partition
 * the mesh is contained in exactly one of the child sets.
 *
 * The data for this tag is a single integer value.  The value of
 * the tag is undefined.
 */
#define PARALLEL_PARITIONING_TAG_NAME "PARALLEL_MESH_PARITIONING"

/** \brief Tag storing which other processor a given entity is shared with
 *
 * This single-valued tag implies an entity is shared with one other proc
 */
#define PARALLEL_SHARED_PROC_TAG_NAME "__PARALLEL_SHARED_PROC"
 
/** \brief Tag storing which other processorS a given entity is shared with
 *
 * This multiple-valued tag implies an entity is shared with multiple
 * other processors.  Length of tag is application-dependent, and depends on
 * what the maximum number of processors is which share an entity
 */
#define PARALLEL_SHARED_PROCS_TAG_NAME "__PARALLEL_SHARED_PROCS"
 
/** \brief Tag storing the handle of a shared entity on the other proc
 *
 * This single-valued tag implies an entity is shared with one other proc
 */
#define PARALLEL_SHARED_HANDLE_TAG_NAME "__PARALLEL_SHARED_HANDLE"
 
/** \brief Tag storing handles of a shared entity on other processors
 *
 * This multiple-valued tag implies an entity is shared with multiple
 * other processors.  Length of tag is application-dependent, and depends on
 * what the maximum number of processors is which share an entity
 */
#define PARALLEL_SHARED_HANDLES_TAG_NAME "__PARALLEL_SHARED_HANDLES"
 
/** \brief Tag storing parallel status (as bits in this tag)
 *
 * This tag stores various aspects of parallel status in bits; see also 
 * #define's following, to be used in bit mask operations.  If an entity is
 * not shared with any other processors, the pstatus is 0, otherwise it's > 0
 *
 * bit 0: !owned (0=owned, 1=not owned)
 * bit 1: shared (0=not shared, 1=shared)
 * bit 2: multishared (shared by > 2 procs; 0=not shared, 1=shared)
 * bit 3: interface (0=not interface, 1=interface)
 * bit 4: ghost (0=not ghost, 1=ghost)
 */
#define PARALLEL_STATUS_TAG_NAME "__PARALLEL_STATUS"

#define PSTATUS_NOT_OWNED 0x1
#define PSTATUS_SHARED 0x2
#define PSTATUS_MULTISHARED 0x4
#define PSTATUS_INTERFACE 0x8
// note, these numbers are in hex, so 0x10 is the 4th bit, or 2^4.
#define PSTATUS_GHOST 0x10

#define PSTATUS_AND 0x1
#define PSTATUS_OR 0x2
#define PSTATUS_NOT 0x3
#endif
