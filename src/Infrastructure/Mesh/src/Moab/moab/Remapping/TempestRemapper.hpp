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
class TempestOfflineMap;

class TempestRemapper : public Remapper
{
public:
#ifdef MOAB_HAVE_MPI
    TempestRemapper(moab::Interface* mbInt, moab::ParallelComm* pcomm = NULL) :
        Remapper(mbInt, pcomm),
#else
    TempestRemapper(moab::Interface* mbInt) :
        Remapper(mbInt),
#endif
        meshValidate(false), constructEdgeMap(false), m_source_type(DEFAULT), m_target_type(DEFAULT)
    {
    }

    virtual ~TempestRemapper();

    virtual ErrorCode initialize(bool initialize_fsets=true);

    // Mesh type with a correspondence to Tempest/Climate formats
    enum TempestMeshType { 
        DEFAULT = -1,
        CS = 0,
        RLL = 1,
        ICO = 2,
        ICOD = 3,
        OVERLAP_FILES = 4,
        OVERLAP_MEMORY = 5,
        OVERLAP_MOAB = 6
    };

    friend class TempestOfflineMap;

    moab::ErrorCode GenerateMesh(Remapper::IntersectionContext ctx, TempestMeshType type);

    moab::ErrorCode LoadMesh(Remapper::IntersectionContext ctx, std::string inputFilename, TempestMeshType type);

    moab::ErrorCode ComputeOverlapMesh(double tolerance=1e-8, double radius_src=1.0, double radius_tgt=1.0, double boxeps=0.1, bool use_tempest=false);

    // Converters between MOAB and Tempest representations
    moab::ErrorCode ConvertTempestMesh(Remapper::IntersectionContext ctx);

    moab::ErrorCode ConvertMeshToTempest(Remapper::IntersectionContext ctx);

    Mesh* GetMesh(Remapper::IntersectionContext ctx);

    void SetMesh(Remapper::IntersectionContext ctx, Mesh* mesh, bool overwrite=true);

    Mesh* GetCoveringMesh();

    moab::EntityHandle& GetMeshSet(Remapper::IntersectionContext ctx);

    moab::EntityHandle GetMeshSet(Remapper::IntersectionContext ctx) const;

    moab::Range& GetMeshEntities(Remapper::IntersectionContext ctx);

    const moab::Range& GetMeshEntities(Remapper::IntersectionContext ctx) const;

    moab::EntityHandle& GetCoveringSet();

    void SetMeshType(Remapper::IntersectionContext ctx, TempestMeshType type);

    TempestMeshType GetMeshType(Remapper::IntersectionContext ctx) const;

    int GetGlobalID(Remapper::IntersectionContext ctx, int localID);

    int GetLocalID(Remapper::IntersectionContext ctx, int globalID);

    // public members
    bool meshValidate;  // Validate the mesh after loading from file

    bool constructEdgeMap;  //  Construct the edge map within the TempestRemap datastructures

    static const bool verbose = true;

private:

    moab::ErrorCode AssociateSrcTargetInOverlap();

    moab::ErrorCode ConvertMOABMesh_WithSortedEntitiesBySource();

    // private methods
    moab::ErrorCode LoadTempestMesh_Private(std::string inputFilename, Mesh** tempest_mesh);

    moab::ErrorCode ConvertMOABMeshToTempest_Private(Mesh* mesh, moab::EntityHandle meshset, moab::Range& entities);

    moab::ErrorCode ConvertTempestMeshToMOAB_Private(TempestMeshType type, Mesh* mesh, moab::EntityHandle& meshset);

    moab::ErrorCode augment_overlap_set();

    // Source, Target amd Overlap meshes
    Mesh* m_source;
    TempestMeshType m_source_type;
    moab::Range m_source_entities;
    moab::EntityHandle m_source_set;

    Mesh* m_target;
    TempestMeshType m_target_type;
    moab::Range m_target_entities;
    moab::EntityHandle m_target_set;

    // Overlap meshes
    Mesh* m_overlap;
    TempestMeshType m_overlap_type;
    moab::Range m_overlap_entities;
    moab::EntityHandle m_overlap_set;
    std::vector<std::pair<int,int> > m_sorted_overlap_order;
    // Mesh* m_sorted_overlap;

    // Parallel - migrated mesh that is in the local view
    Mesh* m_covering_source;
    moab::EntityHandle m_covering_source_set;
    moab::Range m_covering_source_entities;

    std::map<int,int> gid_to_lid_src, gid_to_lid_covsrc, gid_to_lid_tgt;
    std::map<int,int> lid_to_gid_src, lid_to_gid_covsrc, lid_to_gid_tgt;

    bool is_parallel, is_root;
    int rank, size;
};

// Inline functions
inline
Mesh* TempestRemapper::GetMesh(Remapper::IntersectionContext ctx)
{
    switch(ctx)
    {
        case Remapper::SourceMesh:
            return m_source;
        case Remapper::TargetMesh:
            return m_target;
        case Remapper::IntersectedMesh:
            return m_overlap;
        case Remapper::CoveringMesh:
            return m_covering_source;
        case Remapper::DEFAULT:
        default:
            return NULL;
    }
}

inline
void TempestRemapper::SetMesh(Remapper::IntersectionContext ctx, Mesh* mesh, bool overwrite)
{
    switch(ctx)
    {
        case Remapper::SourceMesh:
            if (!overwrite && m_source) return;
            if (overwrite && m_source) delete m_source;
            m_source = mesh;
            break;
        case Remapper::TargetMesh:
            if (!overwrite && m_target) return;
            if (overwrite && m_target) delete m_target;
            m_target = mesh;
            break;
        case Remapper::IntersectedMesh:
            if (!overwrite && m_overlap) return;
            if (overwrite && m_overlap) delete m_overlap;
            m_overlap = mesh;
            break;
        case Remapper::CoveringMesh:
            if (!overwrite && m_covering_source) return;
            if (overwrite && m_covering_source) delete m_covering_source;
            m_covering_source = mesh;
            break;
        case Remapper::DEFAULT:
        default:
            break;
    }
}

inline
moab::EntityHandle& TempestRemapper::GetMeshSet(Remapper::IntersectionContext ctx)
{
    switch(ctx)
    {
        case Remapper::SourceMesh:
            return m_source_set;
        case Remapper::TargetMesh:
            return m_target_set;
        case Remapper::IntersectedMesh:
            return m_overlap_set;
        case Remapper::CoveringMesh:
            return m_covering_source_set;
        case Remapper::DEFAULT:
        default:
            MB_SET_ERR_RET_VAL("Invalid context passed to GetMeshSet", m_overlap_set);
    }
}

inline
moab::EntityHandle TempestRemapper::GetMeshSet(Remapper::IntersectionContext ctx) const
{
    switch(ctx)
    {
        case Remapper::SourceMesh:
            return m_source_set;
        case Remapper::TargetMesh:
            return m_target_set;
        case Remapper::IntersectedMesh:
            return m_overlap_set;
        case Remapper::CoveringMesh:
            return m_covering_source_set;
        case Remapper::DEFAULT:
        default:
            MB_SET_ERR_RET_VAL("Invalid context passed to GetMeshSet", m_overlap_set);
    }
}


inline
moab::Range& TempestRemapper::GetMeshEntities(Remapper::IntersectionContext ctx)
{
    switch(ctx)
    {
        case Remapper::SourceMesh:
            return m_source_entities;
        case Remapper::TargetMesh:
            return m_target_entities;
        case Remapper::IntersectedMesh:
            return m_overlap_entities;
        case Remapper::CoveringMesh:
            return m_covering_source_entities;
        case Remapper::DEFAULT:
        default:
            MB_SET_ERR_RET_VAL("Invalid context passed to GetMeshSet", m_overlap_entities);
    }
}

inline
const moab::Range& TempestRemapper::GetMeshEntities(Remapper::IntersectionContext ctx) const
{
    switch(ctx)
    {
        case Remapper::SourceMesh:
            return m_source_entities;
        case Remapper::TargetMesh:
            return m_target_entities;
        case Remapper::IntersectedMesh:
            return m_overlap_entities;
        case Remapper::CoveringMesh:
            return m_covering_source_entities;
        case Remapper::DEFAULT:
        default:
            MB_SET_ERR_RET_VAL("Invalid context passed to GetMeshSet", m_overlap_entities);
    }
}

inline
void TempestRemapper::SetMeshType(Remapper::IntersectionContext ctx, TempestRemapper::TempestMeshType type)
{
    switch(ctx)
    {
        case Remapper::SourceMesh:
            m_source_type = type;
            break;
        case Remapper::TargetMesh:
            m_target_type = type;
            break;
        case Remapper::IntersectedMesh:
            m_overlap_type = type;
            break;
        case Remapper::DEFAULT:
        default:
            break;
    }
}

inline
TempestRemapper::TempestMeshType TempestRemapper::GetMeshType(Remapper::IntersectionContext ctx) const
{
    switch(ctx)
    {
        case Remapper::SourceMesh:
            return m_source_type;
        case Remapper::TargetMesh:
            return m_target_type;
        case Remapper::IntersectedMesh:
            return m_overlap_type;
        case Remapper::DEFAULT:
        default:
            return TempestRemapper::DEFAULT;
    }
}

inline
Mesh* TempestRemapper::GetCoveringMesh() {
    return m_covering_source;
}

inline
moab::EntityHandle& TempestRemapper::GetCoveringSet() {
    return m_covering_source_set;
}

inline
int TempestRemapper::GetGlobalID(Remapper::IntersectionContext ctx, int localID)
{
    switch(ctx)
    {
        case Remapper::SourceMesh:
            return lid_to_gid_src[localID];
        case Remapper::TargetMesh:
            return lid_to_gid_tgt[localID];
        case Remapper::CoveringMesh:
            return lid_to_gid_covsrc[localID];
        case Remapper::IntersectedMesh:
        case Remapper::DEFAULT:
        default:
            return -1;
    }
}

inline
int TempestRemapper::GetLocalID(Remapper::IntersectionContext ctx, int globalID)
{
    switch(ctx)
    {
        case Remapper::SourceMesh:
            return gid_to_lid_src[globalID];
        case Remapper::TargetMesh:
            return gid_to_lid_tgt[globalID];
        case Remapper::CoveringMesh:
            return gid_to_lid_covsrc[globalID];
        case Remapper::DEFAULT:
        case Remapper::IntersectedMesh:
        default:
            return -1;
    }
}


}

#endif // MB_TEMPESTREMAPPER_HPP
