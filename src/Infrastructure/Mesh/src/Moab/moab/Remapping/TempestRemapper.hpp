/*
 * =====================================================================================
 *
 *       Filename:  TempestRemapper.hpp
 *
 *    Description:  Interface to the TempestRemap library to enable intersection and
 *                  high-order conservative remapping of climate solution from
 *                  arbitrary resolution of source and target grids on the sphere.
 *
 *         Author:  Vijay S. Mahadevan (vijaysm), mahadevan@anl.gov
 *
 * =====================================================================================
 */

#ifndef MB_TEMPESTREMAPPER_HPP
#define MB_TEMPESTREMAPPER_HPP

#include "moab/Remapping/Remapper.hpp"
#include "moab/IntxMesh/Intx2MeshOnSphere.hpp"
#include "moab/IntxMesh/IntxUtils.hpp"

// Tempest includes
#ifdef MOAB_HAVE_TEMPESTREMAP
#include "netcdfcpp.h"
#include "TempestRemapAPI.h"
#else
#error "This tool depends on TempestRemap library. Reconfigure using --with-tempestremap"
#endif

namespace moab
{

// Forward declare our friend, the mapper
class TempestOnlineMap;

class TempestRemapper : public Remapper
{
  public:
#ifdef MOAB_HAVE_MPI
    TempestRemapper( moab::Interface* mbInt, moab::ParallelComm* pcomm = NULL )
        : Remapper( mbInt, pcomm ),
#else
    TempestRemapper( moab::Interface* mbInt )
        : Remapper( mbInt ),
#endif
          meshValidate( false ), constructEdgeMap( false ), m_source_type( DEFAULT ), m_target_type( DEFAULT )
    {
    }

    virtual ~TempestRemapper();

    // Mesh type with a correspondence to Tempest/Climate formats
    enum TempestMeshType
    {
        DEFAULT        = -1,
        CS             = 0,
        RLL            = 1,
        ICO            = 2,
        ICOD           = 3,
        OVERLAP_FILES  = 4,
        OVERLAP_MEMORY = 5,
        OVERLAP_MOAB   = 6
    };

    friend class TempestOnlineMap;

  public:
    /// <summary>
    ///     Initialize the TempestRemapper object internal datastructures including the mesh sets
    ///     and TempestRemap mesh references.
    /// </summary>
    virtual ErrorCode initialize( bool initialize_fsets = true );

    /// <summary>
    ///     Deallocate and clear any memory initialized in the TempestRemapper object
    /// </summary>
    virtual ErrorCode clear();

    /// <summary>
    ///     Generate a mesh in memory of given type (CS/RLL/ICO/MPAS(structured)) and store it
    ///     under the context specified by the user.
    /// </summary>
    moab::ErrorCode GenerateMesh( Remapper::IntersectionContext ctx, TempestMeshType type );

    /// <summary>
    ///     Load a mesh from disk of given type and store it under the context specified by the
    ///     user.
    /// </summary>
    moab::ErrorCode LoadMesh( Remapper::IntersectionContext ctx, std::string inputFilename, TempestMeshType type );

    /// <summary>
    ///     Construct a source covering mesh such that it completely encompasses the target grid in
    ///     parallel. This operation is critical to ensure that the parallel advancing-front
    ///     intersection algorithm can the intersection mesh only locally without any process
    ///     communication.
    /// </summary>
    moab::ErrorCode ConstructCoveringSet( double tolerance = 1e-8, double radius_src = 1.0, double radius_tgt = 1.0,
                                          double boxeps = 0.1, bool regional_mesh = false );

    /// <summary>
    ///     Compute the intersection mesh between the source and target grids that have been
    ///     instantiated in the Remapper. This function invokes the parallel advancing-front
    ///     intersection algorithm internally for spherical meshes and can handle arbitrary
    ///     unstructured grids (CS, RLL, ICO, MPAS) with and without holes.
    /// </summary>
    moab::ErrorCode ComputeOverlapMesh( bool kdtree_search = true, bool use_tempest = false );

    /* Converters between MOAB and Tempest representations */

    /// <summary>
    ///     Convert the TempestRemap mesh object to a corresponding MOAB mesh representation
    ///     according to the intersection context.
    /// </summary>
    moab::ErrorCode ConvertTempestMesh( Remapper::IntersectionContext ctx );

    /// <summary>
    ///     Convert the MOAB mesh representation to a corresponding TempestRemap mesh object
    ///     according to the intersection context.
    /// </summary>
    moab::ErrorCode ConvertMeshToTempest( Remapper::IntersectionContext ctx );

    /// <summary>
    ///     Get the TempestRemap mesh object according to the intersection context.
    /// </summary>
    Mesh* GetMesh( Remapper::IntersectionContext ctx );

    /// <summary>
    ///     Set the TempestRemap mesh object according to the intersection context.
    /// </summary>
    void SetMesh( Remapper::IntersectionContext ctx, Mesh* mesh, bool overwrite = true );

    void SetMeshSet( Remapper::IntersectionContext ctx /* Remapper::CoveringMesh*/, moab::EntityHandle mset,
                     moab::Range& entities );
    /// <summary>
    ///     Get the covering mesh (TempestRemap) object.
    /// </summary>
    Mesh* GetCoveringMesh();

    /// <summary>
    ///     Get the MOAB mesh set corresponding to the intersection context.
    /// </summary>
    moab::EntityHandle& GetMeshSet( Remapper::IntersectionContext ctx );

    /// <summary>
    ///     Const overload. Get the MOAB mesh set corresponding to the intersection context.
    /// </summary>
    moab::EntityHandle GetMeshSet( Remapper::IntersectionContext ctx ) const;

    /// <summary>
    ///     Get the mesh element entities corresponding to the intersection context.
    /// </summary>
    moab::Range& GetMeshEntities( Remapper::IntersectionContext ctx );

    /// <summary>
    ///     Const overload. Get the mesh element entities corresponding to the intersection context.
    /// </summary>
    const moab::Range& GetMeshEntities( Remapper::IntersectionContext ctx ) const;

    /// <summary>
    ///     Get the mesh vertices corresponding to the intersection context. Useful for point-cloud
    ///     meshes
    /// </summary>
    moab::Range& GetMeshVertices( Remapper::IntersectionContext ctx );

    /// <summary>
    ///     Const overload. Get the mesh vertices corresponding to the intersection context. Useful
    ///     for point-cloud meshes.
    /// </summary>
    const moab::Range& GetMeshVertices( Remapper::IntersectionContext ctx ) const;

    /// <summary>
    ///     Get access to the underlying source covering set if available. Else return the source
    ///     set.
    /// </summary>
    moab::EntityHandle& GetCoveringSet();

    /// <summary>
    ///     Set the mesh type corresponding to the intersection context
    /// </summary>
    void SetMeshType( Remapper::IntersectionContext ctx, TempestMeshType type,
                      const std::vector< int >* metadata = nullptr );

    /// <summary>
    ///     Get the mesh type corresponding to the intersection context
    /// </summary>
    TempestMeshType GetMeshType( Remapper::IntersectionContext ctx ) const;

    /// <summary>
    ///     Get the global ID corresponding to the local entity ID according to the context (source,
    ///     target, intersection)
    /// </summary>
    int GetGlobalID( Remapper::IntersectionContext ctx, int localID );

    /// <summary>
    ///     Get the local ID corresponding to the global entity ID according to the context (source,
    ///     target, intersection)
    /// </summary>
    int GetLocalID( Remapper::IntersectionContext ctx, int globalID );

    /// <summary>
    ///     Gather the overlap mesh and asssociated source/target data and write it out to disk
    ///     using the TempestRemap output interface. This information can then be used with the
    ///     "GenerateOfflineMap" tool in TempestRemap as needed.
    /// </summary>
    moab::ErrorCode WriteTempestIntersectionMesh( std::string strOutputFileName, const bool fAllParallel,
                                                  const bool fInputConcave, const bool fOutputConcave );

    /// <summary>
    ///     Generate the necessary metadata and specifically the GLL node numbering for DoFs for a
    ///     CS mesh. This negates the need for running external code like HOMME to output the
    ///     numbering needed for computing maps. The functionality is used through the `mbconvert`
    ///     tool to compute processor-invariant Global DoF IDs at GLL nodes.
    /// </summary>
    moab::ErrorCode GenerateCSMeshMetadata( const int ntot_elements, moab::Range& entities,
                                            moab::Range* secondary_entities, const std::string dofTagName, int nP );

    /// <summary>
    ///     Generate the necessary metadata for DoF node numbering in a given mesh.
    ///     Currently, only the functionality to generate numbering on CS grids is supported.
    /// </summary>
    moab::ErrorCode GenerateMeshMetadata( Mesh& mesh, const int ntot_elements, moab::Range& entities,
                                          moab::Range* secondary_entities, const std::string dofTagName, int nP );

    /// <summary>
    ///     Compute the local and global IDs for elements in source/target/coverage meshes.
    /// </summary>
    moab::ErrorCode ComputeGlobalLocalMaps();

    ///	<summary>
    ///		Get all the ghosted overlap entities that were accumulated to enable conservation in
    /// parallel
    ///	</summary>
    moab::ErrorCode GetOverlapAugmentedEntities( moab::Range& sharedGhostEntities );

#ifndef MOAB_HAVE_MPI
    /// <summary>
    ///    Internal method to assign vertex and element global IDs if one does not exist already
    /// </summary>
    moab::ErrorCode assign_vertex_element_IDs( Tag idtag, EntityHandle this_set, const int dimension = 2,
                                               const int start_id = 1 );
#endif

  public:               // public members
    bool meshValidate;  // Validate the mesh after loading from file

    bool constructEdgeMap;  //  Construct the edge map within the TempestRemap datastructures

    static const bool verbose = true;

  private:
    moab::ErrorCode convert_overlap_mesh_sorted_by_source();

    // private methods
    moab::ErrorCode load_tempest_mesh_private( std::string inputFilename, Mesh** tempest_mesh );

    moab::ErrorCode convert_mesh_to_tempest_private( Mesh* mesh, moab::EntityHandle meshset, moab::Range& entities,
                                                     moab::Range* pverts );

    moab::ErrorCode convert_tempest_mesh_private( TempestMeshType type, Mesh* mesh, moab::EntityHandle& meshset,
                                                  moab::Range& entities, moab::Range* vertices );

    moab::ErrorCode augment_overlap_set();

    /* Source meshset, mesh and entity references */
    Mesh* m_source;
    TempestMeshType m_source_type;
    moab::Range m_source_entities;
    moab::Range m_source_vertices;
    moab::EntityHandle m_source_set;
    int max_source_edges;
    bool point_cloud_source;
    std::vector< int > m_source_metadata;

    /* Target meshset, mesh and entity references */
    Mesh* m_target;
    TempestMeshType m_target_type;
    moab::Range m_target_entities;
    moab::Range m_target_vertices;
    moab::EntityHandle m_target_set;
    int max_target_edges;
    bool point_cloud_target;
    std::vector< int > m_target_metadata;

    /* Overlap meshset, mesh and entity references */
    Mesh* m_overlap;
    TempestMeshType m_overlap_type;
    moab::Range m_overlap_entities;
    moab::EntityHandle m_overlap_set;
    std::vector< std::pair< int, int > > m_sorted_overlap_order;

    /* Intersection context on a sphere */
    moab::Intx2MeshOnSphere* mbintx;

    /* Parallel - migrated mesh that is in the local view */
    Mesh* m_covering_source;
    moab::EntityHandle m_covering_source_set;
    moab::Range m_covering_source_entities;
    moab::Range m_covering_source_vertices;

    /* local to glboal and global to local ID maps */
    std::map< int, int > gid_to_lid_src, gid_to_lid_covsrc, gid_to_lid_tgt;
    std::map< int, int > lid_to_gid_src, lid_to_gid_covsrc, lid_to_gid_tgt;

    IntxAreaUtils::AreaMethod m_area_method;

    bool rrmgrids;
    bool is_parallel, is_root;
    int rank, size;
};

// Inline functions
inline Mesh* TempestRemapper::GetMesh( Remapper::IntersectionContext ctx )
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            return m_source;
        case Remapper::TargetMesh:
            return m_target;
        case Remapper::OverlapMesh:
            return m_overlap;
        case Remapper::CoveringMesh:
            return m_covering_source;
        case Remapper::DEFAULT:
        default:
            return NULL;
    }
}

inline void TempestRemapper::SetMesh( Remapper::IntersectionContext ctx, Mesh* mesh, bool overwrite )
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            if( !overwrite && m_source ) return;
            if( overwrite && m_source ) delete m_source;
            m_source = mesh;
            break;
        case Remapper::TargetMesh:
            if( !overwrite && m_target ) return;
            if( overwrite && m_target ) delete m_target;
            m_target = mesh;
            break;
        case Remapper::OverlapMesh:
            if( !overwrite && m_overlap ) return;
            if( overwrite && m_overlap ) delete m_overlap;
            m_overlap = mesh;
            break;
        case Remapper::CoveringMesh:
            if( !overwrite && m_covering_source ) return;
            if( overwrite && m_covering_source ) delete m_covering_source;
            m_covering_source = mesh;
            break;
        case Remapper::DEFAULT:
        default:
            break;
    }
}

inline moab::EntityHandle& TempestRemapper::GetMeshSet( Remapper::IntersectionContext ctx )
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            return m_source_set;
        case Remapper::TargetMesh:
            return m_target_set;
        case Remapper::OverlapMesh:
            return m_overlap_set;
        case Remapper::CoveringMesh:
            return m_covering_source_set;
        case Remapper::DEFAULT:
        default:
            MB_SET_ERR_RET_VAL( "Invalid context passed to GetMeshSet", m_overlap_set );
    }
}

inline moab::EntityHandle TempestRemapper::GetMeshSet( Remapper::IntersectionContext ctx ) const
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            return m_source_set;
        case Remapper::TargetMesh:
            return m_target_set;
        case Remapper::OverlapMesh:
            return m_overlap_set;
        case Remapper::CoveringMesh:
            return m_covering_source_set;
        case Remapper::DEFAULT:
        default:
            MB_SET_ERR_RET_VAL( "Invalid context passed to GetMeshSet", m_overlap_set );
    }
}

inline moab::Range& TempestRemapper::GetMeshEntities( Remapper::IntersectionContext ctx )
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            return m_source_entities;
        case Remapper::TargetMesh:
            return m_target_entities;
        case Remapper::OverlapMesh:
            return m_overlap_entities;
        case Remapper::CoveringMesh:
            return m_covering_source_entities;
        case Remapper::DEFAULT:
        default:
            MB_SET_ERR_RET_VAL( "Invalid context passed to GetMeshSet", m_overlap_entities );
    }
}

inline const moab::Range& TempestRemapper::GetMeshEntities( Remapper::IntersectionContext ctx ) const
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            return m_source_entities;
        case Remapper::TargetMesh:
            return m_target_entities;
        case Remapper::OverlapMesh:
            return m_overlap_entities;
        case Remapper::CoveringMesh:
            return m_covering_source_entities;
        case Remapper::DEFAULT:
        default:
            MB_SET_ERR_RET_VAL( "Invalid context passed to GetMeshSet", m_overlap_entities );
    }
}

inline moab::Range& TempestRemapper::GetMeshVertices( Remapper::IntersectionContext ctx )
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            return m_source_vertices;
        case Remapper::TargetMesh:
            return m_target_vertices;
        case Remapper::CoveringMesh:
            return m_covering_source_vertices;
        case Remapper::DEFAULT:
        default:
            MB_SET_ERR_RET_VAL( "Invalid context passed to GetMeshSet", m_source_vertices );
    }
}

inline const moab::Range& TempestRemapper::GetMeshVertices( Remapper::IntersectionContext ctx ) const
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            return m_source_vertices;
        case Remapper::TargetMesh:
            return m_target_vertices;
        case Remapper::CoveringMesh:
            return m_covering_source_vertices;
        case Remapper::DEFAULT:
        default:
            MB_SET_ERR_RET_VAL( "Invalid context passed to GetMeshSet", m_source_vertices );
    }
}

inline void TempestRemapper::SetMeshType( Remapper::IntersectionContext ctx, TempestRemapper::TempestMeshType type,
                                          const std::vector< int >* metadata )
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            m_source_type = type;
            if( metadata )
            {
                m_source_metadata.resize( metadata->size() );
                std::copy( metadata->begin(), metadata->end(), m_source_metadata.begin() );
            }
            break;
        case Remapper::TargetMesh:
            m_target_type = type;
            if( metadata )
            {
                m_target_metadata.resize( metadata->size() );
                std::copy( metadata->begin(), metadata->end(), m_target_metadata.begin() );
            }
            break;
        case Remapper::OverlapMesh:
            m_overlap_type = type;
            break;
        case Remapper::DEFAULT:
        default:
            break;
    }
}

inline TempestRemapper::TempestMeshType TempestRemapper::GetMeshType( Remapper::IntersectionContext ctx ) const
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            return m_source_type;
        case Remapper::TargetMesh:
            return m_target_type;
        case Remapper::OverlapMesh:
            return m_overlap_type;
        case Remapper::DEFAULT:
        default:
            return TempestRemapper::DEFAULT;
    }
}

inline Mesh* TempestRemapper::GetCoveringMesh()
{
    return m_covering_source;
}

inline moab::EntityHandle& TempestRemapper::GetCoveringSet()
{
    return m_covering_source_set;
}

inline int TempestRemapper::GetGlobalID( Remapper::IntersectionContext ctx, int localID )
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            return lid_to_gid_src[localID];
        case Remapper::TargetMesh:
            return lid_to_gid_tgt[localID];
        case Remapper::CoveringMesh:
            return lid_to_gid_covsrc[localID];
        case Remapper::OverlapMesh:
        case Remapper::DEFAULT:
        default:
            return -1;
    }
}

inline int TempestRemapper::GetLocalID( Remapper::IntersectionContext ctx, int globalID )
{
    switch( ctx )
    {
        case Remapper::SourceMesh:
            return gid_to_lid_src[globalID];
        case Remapper::TargetMesh:
            return gid_to_lid_tgt[globalID];
        case Remapper::CoveringMesh:
            return gid_to_lid_covsrc[globalID];
        case Remapper::DEFAULT:
        case Remapper::OverlapMesh:
        default:
            return -1;
    }
}

}  // namespace moab

#endif  // MB_TEMPESTREMAPPER_HPP
