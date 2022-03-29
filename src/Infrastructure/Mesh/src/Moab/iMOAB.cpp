/** \file iMOAB.cpp
 */

#include "moab/MOABConfig.h"
#include "moab/Core.hpp"

#ifdef MOAB_HAVE_MPI
#include "moab_mpi.h"
#include "moab/ParallelComm.hpp"
#include "moab/ParCommGraph.hpp"
#include "moab/ParallelMergeMesh.hpp"
#include "moab/IntxMesh/IntxUtils.hpp"
#endif
#include "DebugOutput.hpp"
#include "moab/iMOAB.h"

/* this is needed because of direct access to hdf5/mhdf */
#ifdef MOAB_HAVE_HDF5
#include "mhdf.h"
#include <H5Tpublic.h>
#endif

#include "moab/CartVect.hpp"
#include "MBTagConventions.hpp"
#include "moab/MeshTopoUtil.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/MergeMesh.hpp"

#ifdef MOAB_HAVE_TEMPESTREMAP
#include "STLStringHelper.h"
#include "moab/IntxMesh/IntxUtils.hpp"

#include "moab/Remapping/TempestRemapper.hpp"
#include "moab/Remapping/TempestOnlineMap.hpp"
#endif

// C++ includes
#include <cassert>
#include <sstream>
#include <iostream>

using namespace moab;

// #define VERBOSE

// global variables ; should they be organized in a structure, for easier references?
// or how do we keep them global?

#ifdef __cplusplus
extern "C" {
#endif

#define CHKERRVAL( ierr )                            \
    {                                                \
        if( moab::MB_SUCCESS != ( ierr ) ) return 1; \
    }
#define CHKIERRVAL( ierr )            \
    {                                 \
        if( 0 != ( ierr ) ) return 1; \
    }

#ifdef MOAB_HAVE_TEMPESTREMAP
struct TempestMapAppData
{
    moab::TempestRemapper* remapper;
    std::map< std::string, moab::TempestOnlineMap* > weightMaps;
    iMOAB_AppID pid_src;
    iMOAB_AppID pid_dest;
};
#endif

struct appData
{
    EntityHandle file_set;
    int global_id;  // external component id, unique for application
    std::string name;
    Range all_verts;
    Range local_verts;  // it could include shared, but not owned at the interface
    // these vertices would be all_verts if no ghosting was required
    Range ghost_vertices;  // locally ghosted from other processors
    Range primary_elems;
    Range owned_elems;
    Range ghost_elems;
    int dimension;             // 2 or 3, dimension of primary elements (redundant?)
    long num_global_elements;  // reunion of all elements in primary_elements; either from hdf5
                               // reading or from reduce
    long num_global_vertices;  // reunion of all nodes, after sharing is resolved; it could be
                               // determined from hdf5 reading
    Range mat_sets;
    std::map< int, int > matIndex;  // map from global block id to index in mat_sets
    Range neu_sets;
    Range diri_sets;
    std::map< std::string, Tag > tagMap;
    std::vector< Tag > tagList;
    bool point_cloud;

#ifdef MOAB_HAVE_MPI
    // constructor for this ParCommGraph takes the joint comm and the MPI groups for each
    // application
    std::map< int, ParCommGraph* > pgraph;  // map from context () to the parcommgraph*
#endif

#ifdef MOAB_HAVE_TEMPESTREMAP
    TempestMapAppData tempestData;
#endif
};

struct GlobalContext
{
    // are there reasons to have multiple moab inits? Is ref count needed?
    Interface* MBI;
    // we should also have the default tags stored, initialized
    Tag material_tag, neumann_tag, dirichlet_tag,
        globalID_tag;  // material, neumann, dirichlet,  globalID
    int refCountMB;
    int iArgc;
    iMOAB_String* iArgv;
    int unused_pid;

    std::map< std::string, int > appIdMap;  // from app string (uppercase) to app id
    std::map< int, int > appIdCompMap;      // from component id to app id

#ifdef MOAB_HAVE_MPI
    std::vector< ParallelComm* > pcomms;  // created in order of applications, one moab::ParallelComm for each
#endif

    std::vector< appData > appDatas;  // the same order as pcomms
    int globalrank, worldprocs;
    bool MPI_initialized;

    GlobalContext()
    {
        MBI        = 0;
        refCountMB = 0;
        unused_pid = 0;
    }
};

static struct GlobalContext context;

ErrCode iMOAB_Initialize( int argc, iMOAB_String* argv )
{
    context.iArgc = argc;
    context.iArgv = argv;  // shallow copy

    if( 0 == context.refCountMB )
    {
        context.MBI = new( std::nothrow ) moab::Core;
        // retrieve the default tags
        const char* const shared_set_tag_names[] = { MATERIAL_SET_TAG_NAME, NEUMANN_SET_TAG_NAME,
                                                     DIRICHLET_SET_TAG_NAME, GLOBAL_ID_TAG_NAME };
        // blocks, visible surfaceBC(neumann), vertexBC (Dirichlet), global id, parallel partition
        Tag gtags[4];

        for( int i = 0; i < 4; i++ )
        {

            ErrorCode rval =
                context.MBI->tag_get_handle( shared_set_tag_names[i], 1, MB_TYPE_INTEGER, gtags[i], MB_TAG_ANY );CHKERRVAL( rval );
        }

        context.material_tag  = gtags[0];
        context.neumann_tag   = gtags[1];
        context.dirichlet_tag = gtags[2];
        context.globalID_tag  = gtags[3];
    }

    context.MPI_initialized = false;
#ifdef MOAB_HAVE_MPI
    int flagInit;
    MPI_Initialized( &flagInit );

    if( flagInit && !context.MPI_initialized )
    {
        MPI_Comm_size( MPI_COMM_WORLD, &context.worldprocs );
        MPI_Comm_rank( MPI_COMM_WORLD, &context.globalrank );
        context.MPI_initialized = true;
    }
#endif

    context.refCountMB++;
    return 0;
}

ErrCode iMOAB_InitializeFortran()
{
    return iMOAB_Initialize( 0, 0 );
}

ErrCode iMOAB_Finalize()
{
    context.refCountMB--;

    if( 0 == context.refCountMB )
    {
        delete context.MBI;
    }

    return MB_SUCCESS;
}

ErrCode iMOAB_RegisterApplication( const iMOAB_String app_name,
#ifdef MOAB_HAVE_MPI
                                   MPI_Comm* comm,
#endif
                                   int* compid, iMOAB_AppID pid )
{
    // will create a parallel comm for this application too, so there will be a
    // mapping from *pid to file set and to parallel comm instances
    std::string name( app_name );

    if( context.appIdMap.find( name ) != context.appIdMap.end() )
    {
        std::cout << " application " << name << " already registered \n";
        return 1;
    }

    *pid                   = context.unused_pid++;
    context.appIdMap[name] = *pid;
    int rankHere           = 0;
#ifdef MOAB_HAVE_MPI
    MPI_Comm_rank( *comm, &rankHere );
#endif
    if( !rankHere ) std::cout << " application " << name << " with ID = " << *pid << " is registered now \n";
    if( *compid <= 0 )
    {
        std::cout << " convention for external application is to have its id positive \n";
        return 1;
    }

    if( context.appIdCompMap.find( *compid ) != context.appIdCompMap.end() )
    {
        std::cout << " external application with comp id " << *compid << " is already registered\n";
        return 1;
    }

    context.appIdCompMap[*compid] = *pid;

    // now create ParallelComm and a file set for this application
#ifdef MOAB_HAVE_MPI
    if( comm )
    {
        ParallelComm* pco = new ParallelComm( context.MBI, *comm );

#ifndef NDEBUG
        int index = pco->get_id();  // it could be useful to get app id from pcomm instance ...
        assert( index == *pid );
        // here, we assert the the pid is the same as the id of the ParallelComm instance
        // useful for writing in parallel
#endif
        context.pcomms.push_back( pco );
    }
    else
    {
        context.pcomms.push_back( 0 );
    }
#endif

    // create now the file set that will be used for loading the model in
    EntityHandle file_set;
    ErrorCode rval = context.MBI->create_meshset( MESHSET_SET, file_set );CHKERRVAL( rval );

    appData app_data;
    app_data.file_set  = file_set;
    app_data.global_id = *compid;  // will be used mostly for par comm graph
    app_data.name      = name;     // save the name of application

#ifdef MOAB_HAVE_TEMPESTREMAP
    app_data.tempestData.remapper = NULL;  // Only allocate as needed
#endif

    app_data.point_cloud = false;

    context.appDatas.push_back(
        app_data );  // it will correspond to app_FileSets[*pid] will be the file set of interest
    return 0;
}

ErrCode iMOAB_RegisterApplicationFortran( const iMOAB_String app_name,
#ifdef MOAB_HAVE_MPI
                                          int* comm,
#endif
                                          int* compid, iMOAB_AppID pid, int app_name_length )
{
    std::string name( app_name );

    if( (int)strlen( app_name ) > app_name_length )
    {
        std::cout << " length of string issue \n";
        return 1;
    }

#ifdef MOAB_HAVE_MPI
    MPI_Comm ccomm;
    if( comm )
    {
        // convert from Fortran communicator to a C communicator
        // see transfer of handles
        // http://www.mpi-forum.org/docs/mpi-2.2/mpi22-report/node361.htm
        ccomm = MPI_Comm_f2c( (MPI_Fint)*comm );
    }
#endif

    // now call C style registration function:
    return iMOAB_RegisterApplication( app_name,
#ifdef MOAB_HAVE_MPI
                                      &ccomm,
#endif
                                      compid, pid );
}

ErrCode iMOAB_DeregisterApplication( iMOAB_AppID pid )
{
    // the file set , parallel comm are all in vectors indexed by *pid
    // assume we did not delete anything yet
    // *pid will not be reused if we register another application
    appData& data = context.appDatas[*pid];
    int rankHere  = 0;
#ifdef MOAB_HAVE_MPI
    ParallelComm* pco = context.pcomms[*pid];
    rankHere          = pco->rank();
#endif
    if( !rankHere )
        std::cout << " application with ID: " << *pid << " global id: " << data.global_id << " name: " << data.name
                  << " is de-registered now \n";

    EntityHandle fileSet = data.file_set;
    // get all entities part of the file set
    Range fileents;
    ErrorCode rval = context.MBI->get_entities_by_handle( fileSet, fileents, /*recursive */ true );CHKERRVAL( rval );

    fileents.insert( fileSet );

    rval = context.MBI->get_entities_by_type( fileSet, MBENTITYSET, fileents );CHKERRVAL( rval );  // append all mesh sets

#ifdef MOAB_HAVE_TEMPESTREMAP
    if( data.tempestData.remapper ) delete data.tempestData.remapper;
    if( data.tempestData.weightMaps.size() ) data.tempestData.weightMaps.clear();
#endif

#ifdef MOAB_HAVE_MPI

    // we could get the pco also with
    // ParallelComm * pcomm = ParallelComm::get_pcomm(context.MBI, *pid);

    std::map< int, ParCommGraph* >& pargs = data.pgraph;

    // free the parallel comm graphs associated with this app
    for( std::map< int, ParCommGraph* >::iterator mt = pargs.begin(); mt != pargs.end(); mt++ )
    {
        ParCommGraph* pgr = mt->second;
        delete pgr;
        pgr = NULL;
    }
    if( pco ) delete pco;
#endif

    // delete first all except vertices
    Range vertices = fileents.subset_by_type( MBVERTEX );
    Range noverts  = subtract( fileents, vertices );

    rval = context.MBI->delete_entities( noverts );CHKERRVAL( rval );
    // now retrieve connected elements that still exist (maybe in other sets, pids?)
    Range adj_ents_left;
    rval = context.MBI->get_adjacencies( vertices, 1, false, adj_ents_left, Interface::UNION );CHKERRVAL( rval );
    rval = context.MBI->get_adjacencies( vertices, 2, false, adj_ents_left, Interface::UNION );CHKERRVAL( rval );
    rval = context.MBI->get_adjacencies( vertices, 3, false, adj_ents_left, Interface::UNION );CHKERRVAL( rval );

    if( !adj_ents_left.empty() )
    {
        Range conn_verts;
        rval = context.MBI->get_connectivity( adj_ents_left, conn_verts );CHKERRVAL( rval );
        vertices = subtract( vertices, conn_verts );
    }

    rval = context.MBI->delete_entities( vertices );CHKERRVAL( rval );

    std::map< std::string, int >::iterator mit;

    for( mit = context.appIdMap.begin(); mit != context.appIdMap.end(); mit++ )
    {
        int pidx = mit->second;

        if( *pid == pidx )
        {
            break;
        }
    }

    context.appIdMap.erase( mit );
    std::map< int, int >::iterator mit1;

    for( mit1 = context.appIdCompMap.begin(); mit1 != context.appIdCompMap.end(); mit1++ )
    {
        int pidx = mit1->second;

        if( *pid == pidx )
        {
            break;
        }
    }

    context.appIdCompMap.erase( mit1 );

    context.unused_pid--;  // we have to go backwards always TODO
    context.appDatas.pop_back();
#ifdef MOAB_HAVE_MPI
    context.pcomms.pop_back();
#endif
    return 0;
}

// Utility function
static void split_tag_names( std::string input_names, std::string& separator,
                             std::vector< std::string >& list_tag_names )
{
    size_t pos = 0;
    std::string token;
    while( ( pos = input_names.find( separator ) ) != std::string::npos )
    {
        token = input_names.substr( 0, pos );
        list_tag_names.push_back( token );
        // std::cout << token << std::endl;
        input_names.erase( 0, pos + separator.length() );
    }
    if( !input_names.empty() )
    {
        // if leftover something, or if not ended with delimiter
        list_tag_names.push_back( input_names );
    }
    return;
}

ErrCode iMOAB_ReadHeaderInfo( const iMOAB_String filename, int* num_global_vertices, int* num_global_elements,
                              int* num_dimension, int* num_parts, int filename_length )
{
    assert( filename_length != 0 );
#ifdef MOAB_HAVE_HDF5
    std::string filen( filename );

    if( filename_length < (int)strlen( filename ) )
    {
        filen = filen.substr( 0, filename_length );
    }

    *num_global_vertices = 0;
    int edges            = 0;
    int faces            = 0;
    int regions          = 0;
    *num_global_elements = 0;
    *num_dimension       = 0;
    *num_parts           = 0;

    mhdf_FileHandle file;
    mhdf_Status status;
    unsigned long max_id;
    struct mhdf_FileDesc* data;

    file = mhdf_openFile( filen.c_str(), 0, &max_id, -1, &status );

    if( mhdf_isError( &status ) )
    {
        fprintf( stderr, "%s: %s\n", filename, mhdf_message( &status ) );
        return 1;
    }

    data = mhdf_getFileSummary( file, H5T_NATIVE_ULONG, &status,
                                1 );  // will use extra set info; will get parallel partition tag info too!

    if( mhdf_isError( &status ) )
    {
        fprintf( stderr, "%s: %s\n", filename, mhdf_message( &status ) );
        return 1;
    }

    *num_dimension       = data->nodes.vals_per_ent;
    *num_global_vertices = (int)data->nodes.count;

    for( int i = 0; i < data->num_elem_desc; i++ )
    {
        struct mhdf_ElemDesc* el_desc = &( data->elems[i] );
        struct mhdf_EntDesc* ent_d    = &( el_desc->desc );

        if( 0 == strcmp( el_desc->type, mhdf_EDGE_TYPE_NAME ) )
        {
            edges += ent_d->count;
        }

        if( 0 == strcmp( el_desc->type, mhdf_TRI_TYPE_NAME ) )
        {
            faces += ent_d->count;
        }

        if( 0 == strcmp( el_desc->type, mhdf_QUAD_TYPE_NAME ) )
        {
            faces += ent_d->count;
        }

        if( 0 == strcmp( el_desc->type, mhdf_POLYGON_TYPE_NAME ) )
        {
            faces += ent_d->count;
        }

        if( 0 == strcmp( el_desc->type, mhdf_TET_TYPE_NAME ) )
        {
            regions += ent_d->count;
        }

        if( 0 == strcmp( el_desc->type, mhdf_PYRAMID_TYPE_NAME ) )
        {
            regions += ent_d->count;
        }

        if( 0 == strcmp( el_desc->type, mhdf_PRISM_TYPE_NAME ) )
        {
            regions += ent_d->count;
        }

        if( 0 == strcmp( el_desc->type, mdhf_KNIFE_TYPE_NAME ) )
        {
            regions += ent_d->count;
        }

        if( 0 == strcmp( el_desc->type, mdhf_HEX_TYPE_NAME ) )
        {
            regions += ent_d->count;
        }

        if( 0 == strcmp( el_desc->type, mhdf_POLYHEDRON_TYPE_NAME ) )
        {
            regions += ent_d->count;
        }

        if( 0 == strcmp( el_desc->type, mhdf_SEPTAHEDRON_TYPE_NAME ) )
        {
            regions += ent_d->count;
        }
    }

    *num_parts = data->numEntSets[0];

    // is this required?
    if( edges > 0 )
    {
        *num_dimension       = 1;  // I don't think it will ever return 1
        *num_global_elements = edges;
    }

    if( faces > 0 )
    {
        *num_dimension       = 2;
        *num_global_elements = faces;
    }

    if( regions > 0 )
    {
        *num_dimension       = 3;
        *num_global_elements = regions;
    }

    mhdf_closeFile( file, &status );

    free( data );

#else
    std::cout << filename
              << ": Please reconfigure with HDF5. Cannot retrieve header information for file "
                 "formats other than a h5m file.\n";
    *num_global_vertices = *num_global_elements = *num_dimension = *num_parts = 0;
#endif

    return 0;
}

ErrCode iMOAB_LoadMesh( iMOAB_AppID pid, const iMOAB_String filename, const iMOAB_String read_options,
                        int* num_ghost_layers, int filename_length, int read_options_length )
{
    assert( filename_length != 0 );

    if( (int)strlen( filename ) > filename_length )
    {
        std::cout << " filename length issue\n";
        return 1;
    }

    if( (int)strlen( read_options ) > read_options_length )
    {
        std::cout << " read options length issue\n";
        return 1;
    }

    // make sure we use the file set and pcomm associated with the *pid
    std::ostringstream newopts;
    newopts << read_options;

#ifdef MOAB_HAVE_MPI

    if( context.MPI_initialized )
    {
        if( context.worldprocs > 1 )
        {
            std::string opts( read_options );
            std::string pcid( "PARALLEL_COMM=" );
            std::size_t found = opts.find( pcid );

            if( found != std::string::npos )
            {
                std::cerr << " cannot specify PARALLEL_COMM option, it is implicit \n";
                return 1;
            }

            // in serial, apply PARALLEL_COMM option only for h5m files; it does not work for .g
            // files (used in test_remapping)
            std::string filen( filename );
            std::string::size_type idx = filen.rfind( '.' );

            if( idx != std::string::npos )
            {
                std::string extension = filen.substr( idx + 1 );
                if( extension == std::string( "h5m" ) ) newopts << ";;PARALLEL_COMM=" << *pid;
            }

            if( *num_ghost_layers >= 1 )
            {
                // if we want ghosts, we will want additional entities, the last .1
                // because the addl ents can be edges, faces that are part of the neumann sets
                std::string pcid2( "PARALLEL_GHOSTS=" );
                std::size_t found2 = opts.find( pcid2 );

                if( found2 != std::string::npos )
                {
                    std::cout << " PARALLEL_GHOSTS option is already specified, ignore passed "
                                 "number of layers \n";
                }
                else
                {
                    // dimension of primary entities is 3 here, but it could be 2 for climate
                    // meshes; we would need to pass PARALLEL_GHOSTS explicitly for 2d meshes, for
                    // example:  ";PARALLEL_GHOSTS=2.0.1"
                    newopts << ";PARALLEL_GHOSTS=3.0." << *num_ghost_layers << ".3";
                }
            }
        }
    }
#else
    assert( num_ghost_layers == nullptr );
#endif

    // Now let us actually load the MOAB file with the appropriate read options
    ErrorCode rval = context.MBI->load_file( filename, &context.appDatas[*pid].file_set, newopts.str().c_str() );CHKERRVAL( rval );

#ifdef VERBOSE
    // some debugging stuff
    std::ostringstream outfile;
#ifdef MOAB_HAVE_MPI
    int rank   = context.pcomms[*pid]->rank();
    int nprocs = context.pcomms[*pid]->size();
    outfile << "TaskMesh_n" << nprocs << "." << rank << ".h5m";
#else
    outfile << "TaskMesh_n1.0.h5m";
#endif
    // the mesh contains ghosts too, but they are not part of mat/neumann set
    // write in serial the file, to see what tags are missing
    rval = context.MBI->write_file( outfile.str().c_str() );CHKERRVAL( rval );  // everything on current task, written in serial
#endif

    // Update mesh information
    return iMOAB_UpdateMeshInfo( pid );
}

ErrCode iMOAB_WriteMesh( iMOAB_AppID pid, iMOAB_String filename, iMOAB_String write_options, int filename_length,
                         int write_options_length )
{
    // maybe do some processing of strings and lengths
    if( (int)strlen( filename ) > filename_length )
    {
        std::cout << " file name length issue\n";
        return 1;
    }

    if( (int)strlen( write_options ) > write_options_length )
    {
        std::cout << " write options issue\n";
        return 1;
    }

    std::ostringstream newopts;
#ifdef MOAB_HAVE_MPI
    std::string write_opts( write_options );
    std::string pcid( "PARALLEL_COMM=" );
    std::size_t found = write_opts.find( pcid );

    if( found != std::string::npos )
    {
        std::cerr << " cannot specify PARALLEL_COMM option, it is implicit \n";
        return 1;
    }

    // if write in parallel, add pc option, to be sure about which ParallelComm instance is used
    std::string pw( "PARALLEL=WRITE_PART" );
    found = write_opts.find( pw );

    if( found != std::string::npos )
    {
        newopts << "PARALLEL_COMM=" << *pid << ";";
    }

#endif

    newopts << write_options;

    // Now let us actually write the file to disk with appropriate options
    ErrorCode rval = context.MBI->write_file( filename, 0, newopts.str().c_str(), &context.appDatas[*pid].file_set, 1 );CHKERRVAL( rval );

    return 0;
}

ErrCode iMOAB_WriteLocalMesh( iMOAB_AppID pid, iMOAB_String prefix, int prefix_length )
{
    std::ostringstream file_name;
    int rank = 0, size = 1;
#ifdef MOAB_HAVE_MPI
    rank = context.pcomms[*pid]->rank();
    size = context.pcomms[*pid]->size();
#endif
    file_name << prefix << "_" << size << "_" << rank << ".h5m";
    // Now let us actually write the file to disk with appropriate options
    ErrorCode rval = context.MBI->write_file( file_name.str().c_str(), 0, 0, &context.appDatas[*pid].file_set, 1 );CHKERRVAL( rval );

    return 0;
}

ErrCode iMOAB_UpdateMeshInfo( iMOAB_AppID pid )
{
    // this will include ghost elements info
    //
    appData& data        = context.appDatas[*pid];
    EntityHandle fileSet = data.file_set;
    // first clear all data ranges; this can be called after ghosting
    data.all_verts.clear();
    data.primary_elems.clear();
    data.local_verts.clear();
    data.ghost_vertices.clear();
    data.owned_elems.clear();
    data.ghost_elems.clear();
    data.mat_sets.clear();
    data.neu_sets.clear();
    data.diri_sets.clear();

    // Let us get all the vertex entities
    ErrorCode rval = context.MBI->get_entities_by_type( fileSet, MBVERTEX, data.all_verts, true );CHKERRVAL( rval );  // recursive

    // Let us check first entities of dimension = 3
    data.dimension = 3;
    rval           = context.MBI->get_entities_by_dimension( fileSet, data.dimension, data.primary_elems, true );CHKERRVAL( rval );  // recursive

    if( data.primary_elems.empty() )
    {
        // Now 3-D elements. Let us check entities of dimension = 2
        data.dimension = 2;
        rval           = context.MBI->get_entities_by_dimension( fileSet, data.dimension, data.primary_elems, true );CHKERRVAL( rval );  // recursive

        if( data.primary_elems.empty() )
        {
            // Now 3-D/2-D elements. Let us check entities of dimension = 1
            data.dimension = 1;
            rval = context.MBI->get_entities_by_dimension( fileSet, data.dimension, data.primary_elems, true );CHKERRVAL( rval );  // recursive

            if( data.primary_elems.empty() )
            {
                // no elements of dimension 1 or 2 or 3; it could happen for point clouds
                data.dimension = 0;
            }
        }
    }

    // check if the current mesh is just a point cloud
    data.point_cloud = ( ( data.primary_elems.size() == 0 && data.all_verts.size() > 0 ) || data.dimension == 0 );

#ifdef MOAB_HAVE_MPI

    if( context.MPI_initialized )
    {
        ParallelComm* pco = context.pcomms[*pid];

        // filter ghost vertices, from local
        rval = pco->filter_pstatus( data.all_verts, PSTATUS_GHOST, PSTATUS_NOT, -1, &data.local_verts );CHKERRVAL( rval );

        // Store handles for all ghosted entities
        data.ghost_vertices = subtract( data.all_verts, data.local_verts );

        // filter ghost elements, from local
        rval = pco->filter_pstatus( data.primary_elems, PSTATUS_GHOST, PSTATUS_NOT, -1, &data.owned_elems );CHKERRVAL( rval );

        data.ghost_elems = subtract( data.primary_elems, data.owned_elems );
    }
    else
    {
        data.local_verts = data.all_verts;
        data.owned_elems = data.primary_elems;
    }

#else

    data.local_verts = data.all_verts;
    data.owned_elems = data.primary_elems;

#endif

    // Get the references for some standard internal tags such as material blocks, BCs, etc
    rval = context.MBI->get_entities_by_type_and_tag( fileSet, MBENTITYSET, &( context.material_tag ), 0, 1,
                                                      data.mat_sets, Interface::UNION );CHKERRVAL( rval );

    rval = context.MBI->get_entities_by_type_and_tag( fileSet, MBENTITYSET, &( context.neumann_tag ), 0, 1,
                                                      data.neu_sets, Interface::UNION );CHKERRVAL( rval );

    rval = context.MBI->get_entities_by_type_and_tag( fileSet, MBENTITYSET, &( context.dirichlet_tag ), 0, 1,
                                                      data.diri_sets, Interface::UNION );CHKERRVAL( rval );

    return 0;
}

ErrCode iMOAB_GetMeshInfo( iMOAB_AppID pid, int* num_visible_vertices, int* num_visible_elements,
                           int* num_visible_blocks, int* num_visible_surfaceBC, int* num_visible_vertexBC )
{
    ErrorCode rval;
    appData& data        = context.appDatas[*pid];
    EntityHandle fileSet = data.file_set;

    // this will include ghost elements
    // first clear all data ranges; this can be called after ghosting
    if( num_visible_elements )
    {
        num_visible_elements[2] = (int)data.primary_elems.size();
        // separate ghost and local/owned primary elements
        num_visible_elements[0] = (int)data.owned_elems.size();
        num_visible_elements[1] = (int)data.ghost_elems.size();
    }
    if( num_visible_vertices )
    {
        num_visible_vertices[2] = (int)data.all_verts.size();
        num_visible_vertices[1] = (int)data.ghost_vertices.size();
        num_visible_vertices[0] =
            num_visible_vertices[2] - num_visible_vertices[1];  // local are those that are not ghosts
    }

    if( num_visible_blocks )
    {
        rval = context.MBI->get_entities_by_type_and_tag( fileSet, MBENTITYSET, &( context.material_tag ), 0, 1,
                                                          data.mat_sets, Interface::UNION );CHKERRVAL( rval );

        num_visible_blocks[2] = data.mat_sets.size();
        num_visible_blocks[0] = num_visible_blocks[2];
        num_visible_blocks[1] = 0;
    }

    if( num_visible_surfaceBC )
    {
        rval = context.MBI->get_entities_by_type_and_tag( fileSet, MBENTITYSET, &( context.neumann_tag ), 0, 1,
                                                          data.neu_sets, Interface::UNION );CHKERRVAL( rval );

        num_visible_surfaceBC[2] = 0;
        // count how many faces are in each neu set, and how many regions are
        // adjacent to them;
        int numNeuSets = (int)data.neu_sets.size();

        for( int i = 0; i < numNeuSets; i++ )
        {
            Range subents;
            EntityHandle nset = data.neu_sets[i];
            rval              = context.MBI->get_entities_by_dimension( nset, data.dimension - 1, subents );CHKERRVAL( rval );

            for( Range::iterator it = subents.begin(); it != subents.end(); ++it )
            {
                EntityHandle subent = *it;
                Range adjPrimaryEnts;
                rval = context.MBI->get_adjacencies( &subent, 1, data.dimension, false, adjPrimaryEnts );CHKERRVAL( rval );

                num_visible_surfaceBC[2] += (int)adjPrimaryEnts.size();
            }
        }

        num_visible_surfaceBC[0] = num_visible_surfaceBC[2];
        num_visible_surfaceBC[1] = 0;
    }

    if( num_visible_vertexBC )
    {
        rval = context.MBI->get_entities_by_type_and_tag( fileSet, MBENTITYSET, &( context.dirichlet_tag ), 0, 1,
                                                          data.diri_sets, Interface::UNION );CHKERRVAL( rval );

        num_visible_vertexBC[2] = 0;
        int numDiriSets         = (int)data.diri_sets.size();

        for( int i = 0; i < numDiriSets; i++ )
        {
            Range verts;
            EntityHandle diset = data.diri_sets[i];
            rval               = context.MBI->get_entities_by_dimension( diset, 0, verts );CHKERRVAL( rval );

            num_visible_vertexBC[2] += (int)verts.size();
        }

        num_visible_vertexBC[0] = num_visible_vertexBC[2];
        num_visible_vertexBC[1] = 0;
    }

    return 0;
}

ErrCode iMOAB_GetVertexID( iMOAB_AppID pid, int* vertices_length, iMOAB_GlobalID* global_vertex_ID )
{
    Range& verts = context.appDatas[*pid].all_verts;

    if( (int)verts.size() != *vertices_length )
    {
        return 1;
    }  // problem with array length

    // global id tag is context.globalID_tag
    ErrorCode rval = context.MBI->tag_get_data( context.globalID_tag, verts, global_vertex_ID );CHKERRVAL( rval );

    return 0;
}

ErrCode iMOAB_GetVertexOwnership( iMOAB_AppID pid, int* vertices_length, int* visible_global_rank_ID )
{
    Range& verts = context.appDatas[*pid].all_verts;
    int i        = 0;
#ifdef MOAB_HAVE_MPI
    ParallelComm* pco = context.pcomms[*pid];

    for( Range::iterator vit = verts.begin(); vit != verts.end(); vit++, i++ )
    {
        ErrorCode rval = pco->get_owner( *vit, visible_global_rank_ID[i] );CHKERRVAL( rval );
    }

    if( i != *vertices_length )
    {
        return 1;
    }  // warning array allocation problem

#else

    /* everything owned by proc 0 */
    if( (int)verts.size() != *vertices_length )
    {
        return 1;
    }  // warning array allocation problem

    for( Range::iterator vit = verts.begin(); vit != verts.end(); vit++, i++ )
    {
        visible_global_rank_ID[i] = 0;
    }  // all vertices are owned by processor 0, as this is serial run

#endif
    return 0;
}

ErrCode iMOAB_GetVisibleVerticesCoordinates( iMOAB_AppID pid, int* coords_length, double* coordinates )
{
    Range& verts = context.appDatas[*pid].all_verts;

    // interleaved coordinates, so that means deep copy anyway
    if( *coords_length != 3 * (int)verts.size() )
    {
        return 1;
    }

    ErrorCode rval = context.MBI->get_coords( verts, coordinates );CHKERRVAL( rval );

    return 0;
}

ErrCode iMOAB_GetBlockID( iMOAB_AppID pid, int* block_length, iMOAB_GlobalID* global_block_IDs )
{
    // local id blocks? they are counted from 0 to number of visible blocks ...
    // will actually return material set tag value for global
    Range& matSets = context.appDatas[*pid].mat_sets;

    if( *block_length != (int)matSets.size() )
    {
        return 1;
    }

    // return material set tag gtags[0 is material set tag
    ErrorCode rval = context.MBI->tag_get_data( context.material_tag, matSets, global_block_IDs );CHKERRVAL( rval );

    // populate map with index
    std::map< int, int >& matIdx = context.appDatas[*pid].matIndex;
    for( unsigned i = 0; i < matSets.size(); i++ )
    {
        matIdx[global_block_IDs[i]] = i;
    }

    return 0;
}

ErrCode iMOAB_GetBlockInfo( iMOAB_AppID pid, iMOAB_GlobalID* global_block_ID, int* vertices_per_element,
                            int* num_elements_in_block )
{
    std::map< int, int >& matMap      = context.appDatas[*pid].matIndex;
    std::map< int, int >::iterator it = matMap.find( *global_block_ID );

    if( it == matMap.end() )
    {
        return 1;
    }  // error in finding block with id

    int blockIndex          = matMap[*global_block_ID];
    EntityHandle matMeshSet = context.appDatas[*pid].mat_sets[blockIndex];
    Range blo_elems;
    ErrorCode rval = context.MBI->get_entities_by_handle( matMeshSet, blo_elems );

    if( MB_SUCCESS != rval || blo_elems.empty() )
    {
        return 1;
    }

    EntityType type = context.MBI->type_from_handle( blo_elems[0] );

    if( !blo_elems.all_of_type( type ) )
    {
        return 1;
    }  // not all of same  type

    const EntityHandle* conn = NULL;
    int num_verts            = 0;
    rval                     = context.MBI->get_connectivity( blo_elems[0], conn, num_verts );CHKERRVAL( rval );

    *vertices_per_element  = num_verts;
    *num_elements_in_block = (int)blo_elems.size();

    return 0;
}

ErrCode iMOAB_GetVisibleElementsInfo( iMOAB_AppID pid, int* num_visible_elements, iMOAB_GlobalID* element_global_IDs,
                                      int* ranks, iMOAB_GlobalID* block_IDs )
{
    appData& data = context.appDatas[*pid];
#ifdef MOAB_HAVE_MPI
    ParallelComm* pco = context.pcomms[*pid];
#endif

    ErrorCode rval = context.MBI->tag_get_data( context.globalID_tag, data.primary_elems, element_global_IDs );CHKERRVAL( rval );

    int i = 0;

    for( Range::iterator eit = data.primary_elems.begin(); eit != data.primary_elems.end(); ++eit, ++i )
    {
#ifdef MOAB_HAVE_MPI
        rval = pco->get_owner( *eit, ranks[i] );CHKERRVAL( rval );

#else
        /* everything owned by task 0 */
        ranks[i]             = 0;
#endif
    }

    for( Range::iterator mit = data.mat_sets.begin(); mit != data.mat_sets.end(); ++mit )
    {
        EntityHandle matMeshSet = *mit;
        Range elems;
        rval = context.MBI->get_entities_by_handle( matMeshSet, elems );CHKERRVAL( rval );

        int valMatTag;
        rval = context.MBI->tag_get_data( context.material_tag, &matMeshSet, 1, &valMatTag );CHKERRVAL( rval );

        for( Range::iterator eit = elems.begin(); eit != elems.end(); ++eit )
        {
            EntityHandle eh = *eit;
            int index       = data.primary_elems.index( eh );

            if( -1 == index )
            {
                return 1;
            }

            if( -1 >= *num_visible_elements )
            {
                return 1;
            }

            block_IDs[index] = valMatTag;
        }
    }

    return 0;
}

ErrCode iMOAB_GetBlockElementConnectivities( iMOAB_AppID pid, iMOAB_GlobalID* global_block_ID, int* connectivity_length,
                                             int* element_connectivity )
{
    appData& data                     = context.appDatas[*pid];
    std::map< int, int >& matMap      = data.matIndex;
    std::map< int, int >::iterator it = matMap.find( *global_block_ID );

    if( it == matMap.end() )
    {
        return 1;
    }  // error in finding block with id

    int blockIndex          = matMap[*global_block_ID];
    EntityHandle matMeshSet = data.mat_sets[blockIndex];
    std::vector< EntityHandle > elems;

    ErrorCode rval = context.MBI->get_entities_by_handle( matMeshSet, elems );CHKERRVAL( rval );

    if( elems.empty() )
    {
        return 1;
    }

    std::vector< EntityHandle > vconnect;
    rval = context.MBI->get_connectivity( &elems[0], elems.size(), vconnect );CHKERRVAL( rval );

    if( *connectivity_length != (int)vconnect.size() )
    {
        return 1;
    }  // mismatched sizes

    for( int i = 0; i < *connectivity_length; i++ )
    {
        int inx = data.all_verts.index( vconnect[i] );

        if( -1 == inx )
        {
            return 1;
        }  // error, vertex not in local range

        element_connectivity[i] = inx;
    }

    return 0;
}

ErrCode iMOAB_GetElementConnectivity( iMOAB_AppID pid, iMOAB_LocalID* elem_index, int* connectivity_length,
                                      int* element_connectivity )
{
    appData& data = context.appDatas[*pid];
    assert( ( *elem_index >= 0 ) && ( *elem_index < (int)data.primary_elems.size() ) );

    int num_nodes;
    const EntityHandle* conn;

    EntityHandle eh = data.primary_elems[*elem_index];

    ErrorCode rval = context.MBI->get_connectivity( eh, conn, num_nodes );CHKERRVAL( rval );

    if( *connectivity_length < num_nodes )
    {
        return 1;
    }  // wrong number of vertices

    for( int i = 0; i < num_nodes; i++ )
    {
        int index = data.all_verts.index( conn[i] );

        if( -1 == index )
        {
            return 1;
        }

        element_connectivity[i] = index;
    }

    *connectivity_length = num_nodes;

    return 0;
}

ErrCode iMOAB_GetElementOwnership( iMOAB_AppID pid, iMOAB_GlobalID* global_block_ID, int* num_elements_in_block,
                                   int* element_ownership )
{
    std::map< int, int >& matMap = context.appDatas[*pid].matIndex;

    std::map< int, int >::iterator it = matMap.find( *global_block_ID );

    if( it == matMap.end() )
    {
        return 1;
    }  // error in finding block with id

    int blockIndex          = matMap[*global_block_ID];
    EntityHandle matMeshSet = context.appDatas[*pid].mat_sets[blockIndex];
    Range elems;

    ErrorCode rval = context.MBI->get_entities_by_handle( matMeshSet, elems );CHKERRVAL( rval );

    if( elems.empty() )
    {
        return 1;
    }

    if( *num_elements_in_block != (int)elems.size() )
    {
        return 1;
    }  // bad memory allocation

    int i = 0;
#ifdef MOAB_HAVE_MPI
    ParallelComm* pco = context.pcomms[*pid];
#endif

    for( Range::iterator vit = elems.begin(); vit != elems.end(); vit++, i++ )
    {
#ifdef MOAB_HAVE_MPI
        rval = pco->get_owner( *vit, element_ownership[i] );CHKERRVAL( rval );
#else
        element_ownership[i] = 0; /* owned by 0 */
#endif
    }

    return 0;
}

ErrCode iMOAB_GetElementID( iMOAB_AppID pid, iMOAB_GlobalID* global_block_ID, int* num_elements_in_block,
                            iMOAB_GlobalID* global_element_ID, iMOAB_LocalID* local_element_ID )
{
    appData& data                = context.appDatas[*pid];
    std::map< int, int >& matMap = data.matIndex;

    std::map< int, int >::iterator it = matMap.find( *global_block_ID );

    if( it == matMap.end() )
    {
        return 1;
    }  // error in finding block with id

    int blockIndex          = matMap[*global_block_ID];
    EntityHandle matMeshSet = data.mat_sets[blockIndex];
    Range elems;
    ErrorCode rval = context.MBI->get_entities_by_handle( matMeshSet, elems );CHKERRVAL( rval );

    if( elems.empty() )
    {
        return 1;
    }

    if( *num_elements_in_block != (int)elems.size() )
    {
        return 1;
    }  // bad memory allocation

    rval = context.MBI->tag_get_data( context.globalID_tag, elems, global_element_ID );CHKERRVAL( rval );

    // check that elems are among primary_elems in data
    for( int i = 0; i < *num_elements_in_block; i++ )
    {
        local_element_ID[i] = data.primary_elems.index( elems[i] );

        if( -1 == local_element_ID[i] )
        {
            return 1;
        }  // error, not in local primary elements
    }

    return 0;
}

ErrCode iMOAB_GetPointerToSurfaceBC( iMOAB_AppID pid, int* surface_BC_length, iMOAB_LocalID* local_element_ID,
                                     int* reference_surface_ID, int* boundary_condition_value )
{
    // we have to fill bc data for neumann sets;/
    ErrorCode rval;

    // it was counted above, in GetMeshInfo
    appData& data  = context.appDatas[*pid];
    int numNeuSets = (int)data.neu_sets.size();

    int index = 0;  // index [0, surface_BC_length) for the arrays returned

    for( int i = 0; i < numNeuSets; i++ )
    {
        Range subents;
        EntityHandle nset = data.neu_sets[i];
        rval              = context.MBI->get_entities_by_dimension( nset, data.dimension - 1, subents );CHKERRVAL( rval );

        int neuVal;
        rval = context.MBI->tag_get_data( context.neumann_tag, &nset, 1, &neuVal );CHKERRVAL( rval );

        for( Range::iterator it = subents.begin(); it != subents.end(); ++it )
        {
            EntityHandle subent = *it;
            Range adjPrimaryEnts;
            rval = context.MBI->get_adjacencies( &subent, 1, data.dimension, false, adjPrimaryEnts );CHKERRVAL( rval );

            // get global id of the primary ents, and side number of the quad/subentity
            // this is moab ordering
            for( Range::iterator pit = adjPrimaryEnts.begin(); pit != adjPrimaryEnts.end(); pit++ )
            {
                EntityHandle primaryEnt = *pit;
                // get global id
                /*int globalID;
                rval = context.MBI->tag_get_data(gtags[3], &primaryEnt, 1, &globalID);
                if (MB_SUCCESS!=rval)
                  return 1;
                global_element_ID[index] = globalID;*/

                // get local element id
                local_element_ID[index] = data.primary_elems.index( primaryEnt );

                if( -1 == local_element_ID[index] )
                {
                    return 1;
                }  // did not find the element locally

                int side_number, sense, offset;
                rval = context.MBI->side_number( primaryEnt, subent, side_number, sense, offset );CHKERRVAL( rval );

                reference_surface_ID[index]     = side_number + 1;  // moab is from 0 to 5, it needs 1 to 6
                boundary_condition_value[index] = neuVal;
                index++;
            }
        }
    }

    if( index != *surface_BC_length )
    {
        return 1;
    }  // error in array allocations

    return 0;
}

ErrCode iMOAB_GetPointerToVertexBC( iMOAB_AppID pid, int* vertex_BC_length, iMOAB_LocalID* local_vertex_ID,
                                    int* boundary_condition_value )
{
    ErrorCode rval;

    // it was counted above, in GetMeshInfo
    appData& data   = context.appDatas[*pid];
    int numDiriSets = (int)data.diri_sets.size();
    int index       = 0;  // index [0, *vertex_BC_length) for the arrays returned

    for( int i = 0; i < numDiriSets; i++ )
    {
        Range verts;
        EntityHandle diset = data.diri_sets[i];
        rval               = context.MBI->get_entities_by_dimension( diset, 0, verts );CHKERRVAL( rval );

        int diriVal;
        rval = context.MBI->tag_get_data( context.dirichlet_tag, &diset, 1, &diriVal );CHKERRVAL( rval );

        for( Range::iterator vit = verts.begin(); vit != verts.end(); ++vit )
        {
            EntityHandle vt = *vit;
            /*int vgid;
            rval = context.MBI->tag_get_data(gtags[3], &vt, 1, &vgid);
            if (MB_SUCCESS!=rval)
              return 1;
            global_vertext_ID[index] = vgid;*/
            local_vertex_ID[index] = data.all_verts.index( vt );

            if( -1 == local_vertex_ID[index] )
            {
                return 1;
            }  // vertex was not found

            boundary_condition_value[index] = diriVal;
            index++;
        }
    }

    if( *vertex_BC_length != index )
    {
        return 1;
    }  // array allocation issue

    return 0;
}

ErrCode iMOAB_DefineTagStorage( iMOAB_AppID pid, const iMOAB_String tag_storage_name, int* tag_type,
                                int* components_per_entity, int* tag_index, int tag_storage_name_length )
{
    // see if the tag is already existing, and if yes, check the type, length
    if( *tag_type < 0 || *tag_type > 5 )
    {
        return 1;
    }  // we have 6 types of tags supported so far

    DataType tagDataType;
    TagType tagType;
    void* defaultVal        = NULL;
    int* defInt             = new int[*components_per_entity];
    double* defDouble       = new double[*components_per_entity];
    EntityHandle* defHandle = new EntityHandle[*components_per_entity];

    for( int i = 0; i < *components_per_entity; i++ )
    {
        defInt[i]    = 0;
        defDouble[i] = -1e+10;
        defHandle[i] = (EntityHandle)0;
    }

    switch( *tag_type )
    {
        case 0:
            tagDataType = MB_TYPE_INTEGER;
            tagType     = MB_TAG_DENSE;
            defaultVal  = defInt;
            break;

        case 1:
            tagDataType = MB_TYPE_DOUBLE;
            tagType     = MB_TAG_DENSE;
            defaultVal  = defDouble;
            break;

        case 2:
            tagDataType = MB_TYPE_HANDLE;
            tagType     = MB_TAG_DENSE;
            defaultVal  = defHandle;
            break;

        case 3:
            tagDataType = MB_TYPE_INTEGER;
            tagType     = MB_TAG_SPARSE;
            defaultVal  = defInt;
            break;

        case 4:
            tagDataType = MB_TYPE_DOUBLE;
            tagType     = MB_TAG_SPARSE;
            defaultVal  = defDouble;
            break;

        case 5:
            tagDataType = MB_TYPE_HANDLE;
            tagType     = MB_TAG_SPARSE;
            defaultVal  = defHandle;
            break;

        default: {
            delete[] defInt;
            delete[] defDouble;
            delete[] defHandle;
            return 1;
        }  // error
    }

    std::string tag_name( tag_storage_name );

    if( tag_storage_name_length < (int)strlen( tag_storage_name ) )
    {
        tag_name = tag_name.substr( 0, tag_storage_name_length );
    }

    Tag tagHandle;
    ErrorCode rval = context.MBI->tag_get_handle( tag_name.c_str(), *components_per_entity, tagDataType, tagHandle,
                                                  tagType, defaultVal );

    if( MB_TAG_NOT_FOUND == rval )
    {
        rval = context.MBI->tag_get_handle( tag_name.c_str(), *components_per_entity, tagDataType, tagHandle,
                                            tagType | MB_TAG_CREAT, defaultVal );
    }

    // we don't need default values anymore, avoid leaks
    delete[] defInt;
    delete[] defDouble;
    delete[] defHandle;

    appData& data = context.appDatas[*pid];

    if( MB_ALREADY_ALLOCATED == rval )
    {
        std::map< std::string, Tag >& mTags        = data.tagMap;
        std::map< std::string, Tag >::iterator mit = mTags.find( tag_name );

        if( mit == mTags.end() )
        {
            // add it to the map
            mTags[tag_name] = tagHandle;
            // push it to the list of tags, too
            *tag_index = (int)data.tagList.size();
            data.tagList.push_back( tagHandle );
        }

        return 0;  // OK, we found it, and we have it stored in the map tag
    }
    else if( MB_SUCCESS == rval )
    {
        data.tagMap[tag_name] = tagHandle;
        *tag_index            = (int)data.tagList.size();
        data.tagList.push_back( tagHandle );
        return 0;
    }
    else
        return 1;  // some error, maybe the tag was not created
}

ErrCode iMOAB_SetIntTagStorage( iMOAB_AppID pid, const iMOAB_String tag_storage_name, int* num_tag_storage_length,
                                int* ent_type, int* tag_storage_data, int tag_storage_name_length )
{
    std::string tag_name( tag_storage_name );

    if( tag_storage_name_length < (int)strlen( tag_storage_name ) )
    {
        tag_name = tag_name.substr( 0, tag_storage_name_length );
    }

    appData& data = context.appDatas[*pid];

    if( data.tagMap.find( tag_name ) == data.tagMap.end() )
    {
        return 1;
    }  // tag not defined

    Tag tag = data.tagMap[tag_name];

    int tagLength  = 0;
    ErrorCode rval = context.MBI->tag_get_length( tag, tagLength );CHKERRVAL( rval );

    DataType dtype;
    rval = context.MBI->tag_get_data_type( tag, dtype );

    if( MB_SUCCESS != rval || dtype != MB_TYPE_INTEGER )
    {
        return 1;
    }

    // set it on a subset of entities, based on type and length
    Range* ents_to_set;

    if( *ent_type == 0 )  // vertices
    {
        ents_to_set = &data.all_verts;
    }
    else  // if (*ent_type == 1) // *ent_type can be 0 (vertices) or 1 (elements)
    {
        ents_to_set = &data.primary_elems;
    }

    int nents_to_be_set = *num_tag_storage_length / tagLength;

    if( nents_to_be_set > (int)ents_to_set->size() || nents_to_be_set < 1 )
    {
        return 1;
    }  // to many entities to be set or too few

    // restrict the range; everything is contiguous; or not?

    // Range contig_range ( * ( ents_to_set->begin() ), * ( ents_to_set->begin() + nents_to_be_set -
    // 1 ) );
    rval = context.MBI->tag_set_data( tag, *ents_to_set, tag_storage_data );CHKERRVAL( rval );

    return 0;  // no error
}

ErrCode iMOAB_GetIntTagStorage( iMOAB_AppID pid, const iMOAB_String tag_storage_name, int* num_tag_storage_length,
                                int* ent_type, int* tag_storage_data, int tag_storage_name_length )
{
    ErrorCode rval;
    std::string tag_name( tag_storage_name );

    if( tag_storage_name_length < (int)tag_name.length() )
    {
        tag_name = tag_name.substr( 0, tag_storage_name_length );
    }

    appData& data = context.appDatas[*pid];

    if( data.tagMap.find( tag_name ) == data.tagMap.end() )
    {
        return 1;
    }  // tag not defined

    Tag tag = data.tagMap[tag_name];

    int tagLength = 0;
    rval          = context.MBI->tag_get_length( tag, tagLength );CHKERRVAL( rval );

    DataType dtype;
    rval = context.MBI->tag_get_data_type( tag, dtype );CHKERRVAL( rval );

    if( dtype != MB_TYPE_INTEGER )
    {
        return 1;
    }

    // set it on a subset of entities, based on type and length
    Range* ents_to_get;

    if( *ent_type == 0 )  // vertices
    {
        ents_to_get = &data.all_verts;
    }
    else  // if (*ent_type == 1)
    {
        ents_to_get = &data.primary_elems;
    }

    int nents_to_get = *num_tag_storage_length / tagLength;

    if( nents_to_get > (int)ents_to_get->size() || nents_to_get < 1 )
    {
        return 1;
    }  // to many entities to get, or too little

    // restrict the range; everything is contiguous; or not?
    // Range contig_range ( * ( ents_to_get->begin() ), * ( ents_to_get->begin() + nents_to_get - 1
    // ) );

    rval = context.MBI->tag_get_data( tag, *ents_to_get, tag_storage_data );CHKERRVAL( rval );

    return 0;  // no error
}

ErrCode iMOAB_SetDoubleTagStorage( iMOAB_AppID pid, const iMOAB_String tag_storage_name, int* num_tag_storage_length,
                                   int* ent_type, double* tag_storage_data, int tag_storage_name_length )
{
    ErrorCode rval;
    // exactly the same code as for int tag :) maybe should check the type of tag too
    std::string tag_name( tag_storage_name );

    if( tag_storage_name_length < (int)tag_name.length() )
    {
        tag_name = tag_name.substr( 0, tag_storage_name_length );
    }

    appData& data = context.appDatas[*pid];

    if( data.tagMap.find( tag_name ) == data.tagMap.end() )
    {
        return 1;
    }  // tag not defined

    Tag tag = data.tagMap[tag_name];

    int tagLength = 0;
    rval          = context.MBI->tag_get_length( tag, tagLength );CHKERRVAL( rval );

    DataType dtype;
    rval = context.MBI->tag_get_data_type( tag, dtype );

    if( MB_SUCCESS != rval || dtype != MB_TYPE_DOUBLE )
    {
        return 1;
    }

    // set it on a subset of entities, based on type and length
    Range* ents_to_set = NULL;

    if( *ent_type == 0 )  // vertices
    {
        ents_to_set = &data.all_verts;
    }
    else if( *ent_type == 1 )
    {
        ents_to_set = &data.primary_elems;
    }

    int nents_to_be_set = *num_tag_storage_length / tagLength;

    if( nents_to_be_set > (int)ents_to_set->size() || nents_to_be_set < 1 )
    {
        return 1;
    }  // to many entities to be set

    // restrict the range; everything is contiguous; or not?
    // Range contig_range ( * ( ents_to_set->begin() ), * ( ents_to_set->begin() + nents_to_be_set -
    // 1 ) );

    rval = context.MBI->tag_set_data( tag, *ents_to_set, tag_storage_data );CHKERRVAL( rval );

    return 0;  // no error
}

ErrCode iMOAB_GetDoubleTagStorage( iMOAB_AppID pid, const iMOAB_String tag_storage_name, int* num_tag_storage_length,
                                   int* ent_type, double* tag_storage_data, int tag_storage_name_length )
{
    ErrorCode rval;
    // exactly the same code, except tag type check
    std::string tag_name( tag_storage_name );

    if( tag_storage_name_length < (int)tag_name.length() )
    {
        tag_name = tag_name.substr( 0, tag_storage_name_length );
    }

    appData& data = context.appDatas[*pid];

    if( data.tagMap.find( tag_name ) == data.tagMap.end() )
    {
        return 1;
    }  // tag not defined

    Tag tag = data.tagMap[tag_name];

    int tagLength = 0;
    rval          = context.MBI->tag_get_length( tag, tagLength );CHKERRVAL( rval );

    DataType dtype;
    rval = context.MBI->tag_get_data_type( tag, dtype );CHKERRVAL( rval );

    if( dtype != MB_TYPE_DOUBLE )
    {
        return 1;
    }

    // set it on a subset of entities, based on type and length
    Range* ents_to_get = NULL;

    if( *ent_type == 0 )  // vertices
    {
        ents_to_get = &data.all_verts;
    }
    else if( *ent_type == 1 )
    {
        ents_to_get = &data.primary_elems;
    }

    int nents_to_get = *num_tag_storage_length / tagLength;

    if( nents_to_get > (int)ents_to_get->size() || nents_to_get < 1 )
    {
        return 1;
    }  // to many entities to get

    // restrict the range; everything is contiguous; or not?

    // Range contig_range ( * ( ents_to_get->begin() ), * ( ents_to_get->begin() + nents_to_get - 1
    // ) );
    rval = context.MBI->tag_get_data( tag, *ents_to_get, tag_storage_data );CHKERRVAL( rval );

    return 0;  // no error
}

ErrCode iMOAB_SynchronizeTags( iMOAB_AppID pid, int* num_tag, int* tag_indices, int* ent_type )
{
#ifdef MOAB_HAVE_MPI
    appData& data = context.appDatas[*pid];
    Range ent_exchange;
    std::vector< Tag > tags;

    for( int i = 0; i < *num_tag; i++ )
    {
        if( tag_indices[i] < 0 || tag_indices[i] >= (int)data.tagList.size() )
        {
            return 1;
        }  // error in tag index

        tags.push_back( data.tagList[tag_indices[i]] );
    }

    if( *ent_type == 0 )
    {
        ent_exchange = data.all_verts;
    }
    else if( *ent_type == 1 )
    {
        ent_exchange = data.primary_elems;
    }
    else
    {
        return 1;
    }  // unexpected type

    ParallelComm* pco = context.pcomms[*pid];

    ErrorCode rval = pco->exchange_tags( tags, tags, ent_exchange );CHKERRVAL( rval );

#else
    /* do nothing if serial */
    // just silence the warning
    // do not call sync tags in serial!
    int k = *pid + *num_tag + *tag_indices + *ent_type;
    k++;
#endif

    return 0;
}

ErrCode iMOAB_ReduceTagsMax( iMOAB_AppID pid, int* tag_index, int* ent_type )
{
#ifdef MOAB_HAVE_MPI
    appData& data = context.appDatas[*pid];
    Range ent_exchange;

    if( *tag_index < 0 || *tag_index >= (int)data.tagList.size() )
    {
        return 1;
    }  // error in tag index

    Tag tagh = data.tagList[*tag_index];

    if( *ent_type == 0 )
    {
        ent_exchange = data.all_verts;
    }
    else if( *ent_type == 1 )
    {
        ent_exchange = data.primary_elems;
    }
    else
    {
        return 1;
    }  // unexpected type

    ParallelComm* pco = context.pcomms[*pid];
    // we could do different MPI_Op; do not bother now, we will call from fortran
    ErrorCode rval = pco->reduce_tags( tagh, MPI_MAX, ent_exchange );CHKERRVAL( rval );

#else
    /* do nothing if serial */
    // just silence the warning
    // do not call sync tags in serial!
    int k = *pid + *tag_index + *ent_type;
    k++;  // just do junk, to avoid complaints
#endif
    return 0;
}

ErrCode iMOAB_GetNeighborElements( iMOAB_AppID pid, iMOAB_LocalID* local_index, int* num_adjacent_elements,
                                   iMOAB_LocalID* adjacent_element_IDs )
{
    ErrorCode rval;

    // one neighbor for each subentity of dimension-1
    MeshTopoUtil mtu( context.MBI );
    appData& data   = context.appDatas[*pid];
    EntityHandle eh = data.primary_elems[*local_index];
    Range adjs;
    rval = mtu.get_bridge_adjacencies( eh, data.dimension - 1, data.dimension, adjs );CHKERRVAL( rval );

    if( *num_adjacent_elements < (int)adjs.size() )
    {
        return 1;
    }  // not dimensioned correctly

    *num_adjacent_elements = (int)adjs.size();

    for( int i = 0; i < *num_adjacent_elements; i++ )
    {
        adjacent_element_IDs[i] = data.primary_elems.index( adjs[i] );
    }

    return 0;
}

#if 0

ErrCode iMOAB_GetNeighborVertices ( iMOAB_AppID pid, iMOAB_LocalID* local_vertex_ID, int* num_adjacent_vertices, iMOAB_LocalID* adjacent_vertex_IDs )
{
    return 0;
}

#endif

ErrCode iMOAB_CreateVertices( iMOAB_AppID pid, int* coords_len, int* dim, double* coordinates )
{
    ErrorCode rval;
    appData& data = context.appDatas[*pid];

    if( !data.local_verts.empty() )  // we should have no vertices in the app
    {
        return 1;
    }

    int nverts = *coords_len / *dim;

    rval = context.MBI->create_vertices( coordinates, nverts, data.local_verts );CHKERRVAL( rval );

    rval = context.MBI->add_entities( data.file_set, data.local_verts );CHKERRVAL( rval );

    // also add the vertices to the all_verts range
    data.all_verts.merge( data.local_verts );
    return 0;
}

ErrCode iMOAB_CreateElements( iMOAB_AppID pid, int* num_elem, int* type, int* num_nodes_per_element, int* connectivity,
                              int* block_ID )
{
    // Create elements
    appData& data = context.appDatas[*pid];

    ReadUtilIface* read_iface;
    ErrorCode rval = context.MBI->query_interface( read_iface );CHKERRVAL( rval );

    EntityType mbtype = ( EntityType )( *type );
    EntityHandle actual_start_handle;
    EntityHandle* array = NULL;
    rval = read_iface->get_element_connect( *num_elem, *num_nodes_per_element, mbtype, 1, actual_start_handle, array );CHKERRVAL( rval );

    // fill up with actual connectivity from input; assume the vertices are in order, and start
    // vertex is the first in the current data vertex range
    EntityHandle firstVertex = data.local_verts[0];

    for( int j = 0; j < *num_elem * ( *num_nodes_per_element ); j++ )
    {
        array[j] = connectivity[j] + firstVertex - 1;
    }  // assumes connectivity uses 1 based array (from fortran, mostly)

    Range new_elems( actual_start_handle, actual_start_handle + *num_elem - 1 );

    rval = context.MBI->add_entities( data.file_set, new_elems );CHKERRVAL( rval );

    data.primary_elems.merge( new_elems );

    // add to adjacency
    rval = read_iface->update_adjacencies( actual_start_handle, *num_elem, *num_nodes_per_element, array );CHKERRVAL( rval );
    // organize all new elements in block, with the given block ID; if the block set is not
    // existing, create  a new mesh set;
    Range sets;
    int set_no            = *block_ID;
    const void* setno_ptr = &set_no;
    rval = context.MBI->get_entities_by_type_and_tag( data.file_set, MBENTITYSET, &context.material_tag, &setno_ptr, 1,
                                                      sets );
    EntityHandle block_set;

    if( MB_FAILURE == rval || sets.empty() )
    {
        // create a new set, with this block ID
        rval = context.MBI->create_meshset( MESHSET_SET, block_set );CHKERRVAL( rval );

        rval = context.MBI->tag_set_data( context.material_tag, &block_set, 1, &set_no );CHKERRVAL( rval );

        // add the material set to file set
        rval = context.MBI->add_entities( data.file_set, &block_set, 1 );CHKERRVAL( rval );
    }
    else
    {
        block_set = sets[0];
    }  // first set is the one we want

    /// add the new ents to the clock set
    rval = context.MBI->add_entities( block_set, new_elems );CHKERRVAL( rval );

    return 0;
}

ErrCode iMOAB_SetGlobalInfo( iMOAB_AppID pid, int* num_global_verts, int* num_global_elems )
{
    appData& data            = context.appDatas[*pid];
    data.num_global_vertices = *num_global_verts;
    data.num_global_elements = *num_global_elems;
    return 0;
}

ErrCode iMOAB_GetGlobalInfo( iMOAB_AppID pid, int* num_global_verts, int* num_global_elems )
{
    appData& data = context.appDatas[*pid];
    if( NULL != num_global_verts )
    {
        *num_global_verts = data.num_global_vertices;
    }
    if( NULL != num_global_elems )
    {
        *num_global_elems = data.num_global_elements;
    }

    return 0;
}

#ifdef MOAB_HAVE_MPI

// this makes sense only for parallel runs
ErrCode iMOAB_ResolveSharedEntities( iMOAB_AppID pid, int* num_verts, int* marker )
{
    appData& data     = context.appDatas[*pid];
    ParallelComm* pco = context.pcomms[*pid];
    EntityHandle cset = data.file_set;
    int dum_id        = 0;
    ErrorCode rval;
    if( data.primary_elems.empty() )
    {
        // skip actual resolve, assume vertices are distributed already ,
        // no need to share them
    }
    else
    {
        // create an integer tag for resolving ; maybe it can be a long tag in the future
        // (more than 2 B vertices;)

        Tag stag;
        rval = context.MBI->tag_get_handle( "__sharedmarker", 1, MB_TYPE_INTEGER, stag, MB_TAG_CREAT | MB_TAG_DENSE,
                                            &dum_id );CHKERRVAL( rval );

        if( *num_verts > (int)data.local_verts.size() )
        {
            return 1;
        }  // we are not setting the size

        rval = context.MBI->tag_set_data( stag, data.local_verts, (void*)marker );CHKERRVAL( rval );  // assumes integer tag

        rval = pco->resolve_shared_ents( cset, -1, -1, &stag );CHKERRVAL( rval );

        rval = context.MBI->tag_delete( stag );CHKERRVAL( rval );
    }
    // provide partition tag equal to rank
    Tag part_tag;
    dum_id = -1;
    rval   = context.MBI->tag_get_handle( "PARALLEL_PARTITION", 1, MB_TYPE_INTEGER, part_tag,
                                        MB_TAG_CREAT | MB_TAG_SPARSE, &dum_id );

    if( part_tag == NULL || ( ( rval != MB_SUCCESS ) && ( rval != MB_ALREADY_ALLOCATED ) ) )
    {
        std::cout << " can't get par part tag.\n";
        return 1;
    }

    int rank = pco->rank();
    rval     = context.MBI->tag_set_data( part_tag, &cset, 1, &rank );CHKERRVAL( rval );

    return 0;
}

// this assumes that this was not called before
ErrCode iMOAB_DetermineGhostEntities( iMOAB_AppID pid, int* ghost_dim, int* num_ghost_layers, int* bridge_dim )
{
    ErrorCode rval;

    // verify we have valid ghost layers input specified. If invalid, exit quick.
    if( *num_ghost_layers <= 0 )
    {
        return 0;
    }  // nothing to do

    appData& data     = context.appDatas[*pid];
    ParallelComm* pco = context.pcomms[*pid];

    int addl_ents = 0;  // maybe we should be passing this too; most of the time we do not need additional ents
    // collective call
    rval =
        pco->exchange_ghost_cells( *ghost_dim, *bridge_dim, *num_ghost_layers, addl_ents, true, true, &data.file_set );CHKERRVAL( rval );

    // now re-establish all mesh info; will reconstruct mesh info, based solely on what is in the
    // file set
    int rc = iMOAB_UpdateMeshInfo( pid );
    return rc;
}

ErrCode iMOAB_SendMesh( iMOAB_AppID pid, MPI_Comm* global, MPI_Group* receivingGroup, int* rcompid, int* method )
{
    int ierr       = 0;
    ErrorCode rval = MB_SUCCESS;
    // appData & data = context.appDatas[*pid];
    ParallelComm* pco = context.pcomms[*pid];

    MPI_Comm sender = pco->comm();  // the sender comm is obtained from parallel comm in moab;
    // no need to pass it along
    // first see what are the processors in each group; get the sender group too, from the sender
    // communicator
    MPI_Group senderGroup;
    ierr = MPI_Comm_group( sender, &senderGroup );
    if( ierr != 0 ) return 1;

    // instantiate the par comm graph
    // ParCommGraph::ParCommGraph(MPI_Comm joincomm, MPI_Group group1, MPI_Group group2, int coid1,
    // int coid2)
    ParCommGraph* cgraph =
        new ParCommGraph( *global, senderGroup, *receivingGroup, context.appDatas[*pid].global_id, *rcompid );
    // we should search if we have another pcomm with the same comp ids in the list already
    // sort of check existing comm graphs in the map context.appDatas[*pid].pgraph
    context.appDatas[*pid].pgraph[*rcompid] = cgraph;

    int sender_rank = -1;
    MPI_Comm_rank( sender, &sender_rank );

    // decide how to distribute elements to each processor
    // now, get the entities on local processor, and pack them into a buffer for various processors
    // we will do trivial partition: first get the total number of elements from "sender"
    std::vector< int > number_elems_per_part;
    // how to distribute local elements to receiving tasks?
    // trivial partition: compute first the total number of elements need to be sent
    Range owned = context.appDatas[*pid].owned_elems;
    if( owned.size() == 0 )
    {
        // must be vertices that we want to send then
        owned = context.appDatas[*pid].local_verts;
        // we should have some vertices here
    }

    if( *method == 0 )  // trivial partitioning, old method
    {
        int local_owned_elem = (int)owned.size();
        int size             = pco->size();
        int rank             = pco->rank();
        number_elems_per_part.resize( size );  //
        number_elems_per_part[rank] = local_owned_elem;
#if( MPI_VERSION >= 2 )
        // Use "in place" option
        ierr = MPI_Allgather( MPI_IN_PLACE, 0, MPI_DATATYPE_NULL, &number_elems_per_part[0], 1, MPI_INT, sender );
#else
        {
            std::vector< int > all_tmp( size );
            ierr = MPI_Allgather( &number_elems_per_part[rank], 1, MPI_INT, &all_tmp[0], 1, MPI_INT, sender );
            number_elems_per_part = all_tmp;
        }
#endif

        if( ierr != 0 )
        {
            return 1;
        }

        // every sender computes the trivial partition, it is cheap, and we need to send it anyway
        // to each sender
        rval = cgraph->compute_trivial_partition( number_elems_per_part );CHKERRVAL( rval );

        rval = cgraph->send_graph( *global );CHKERRVAL( rval );
    }
    else  // *method != 0, so it is either graph or geometric, parallel
    {
        // owned are the primary elements on this app
        rval = cgraph->compute_partition( pco, owned, *method );CHKERRVAL( rval );

        // basically, send the graph to the receiver side, with unblocking send
        rval = cgraph->send_graph_partition( pco, *global );CHKERRVAL( rval );
    }
    // pco is needed to pack, not for communication
    rval = cgraph->send_mesh_parts( *global, pco, owned );CHKERRVAL( rval );

    // mark for deletion
    MPI_Group_free( &senderGroup );
    return 0;
}

// fortran wrapper
ErrCode iMOAB_SendMeshFortran( iMOAB_AppID pid, int* join, int* receivingGroup, int* rcompid,
        int* method )
{
    MPI_Comm jcomm = MPI_Comm_f2c( (MPI_Fint) *join );
    MPI_Group recvGroup = MPI_Group_f2c( (MPI_Fint) *receivingGroup );
    return iMOAB_SendMesh(pid, &jcomm, &recvGroup, rcompid, method );
}

ErrCode iMOAB_ReceiveMesh( iMOAB_AppID pid, MPI_Comm* global, MPI_Group* sendingGroup, int* scompid )
{
    appData& data          = context.appDatas[*pid];
    ParallelComm* pco      = context.pcomms[*pid];
    MPI_Comm receive       = pco->comm();
    EntityHandle local_set = data.file_set;
    ErrorCode rval;

    // first see what are the processors in each group; get the sender group too, from the sender
    // communicator
    MPI_Group receiverGroup;
    int ierr = MPI_Comm_group( receive, &receiverGroup );
    CHKIERRVAL( ierr );

    // instantiate the par comm graph
    ParCommGraph* cgraph =
        new ParCommGraph( *global, *sendingGroup, receiverGroup, *scompid, context.appDatas[*pid].global_id );
    // TODO we should search if we have another pcomm with the same comp ids in the list already
    // sort of check existing comm graphs in the map context.appDatas[*pid].pgraph
    context.appDatas[*pid].pgraph[*scompid] = cgraph;

    int receiver_rank = -1;
    MPI_Comm_rank( receive, &receiver_rank );

    // first, receive from sender_rank 0, the communication graph (matrix), so each receiver
    // knows what data to expect
    std::vector< int > pack_array;
    rval = cgraph->receive_comm_graph( *global, pco, pack_array );CHKERRVAL( rval );

    // senders across for the current receiver
    int current_receiver = cgraph->receiver( receiver_rank );

    std::vector< int > senders_local;
    size_t n = 0;

    while( n < pack_array.size() )
    {
        if( current_receiver == pack_array[n] )
        {
            for( int j = 0; j < pack_array[n + 1]; j++ )
            {
                senders_local.push_back( pack_array[n + 2 + j] );
            }

            break;
        }

        n = n + 2 + pack_array[n + 1];
    }

#ifdef VERBOSE
    std::cout << " receiver " << current_receiver << " at rank " << receiver_rank << " will receive from "
              << senders_local.size() << " tasks: ";

    for( int k = 0; k < (int)senders_local.size(); k++ )
    {
        std::cout << " " << senders_local[k];
    }

    std::cout << "\n";
#endif

    if( senders_local.empty() )
    {
        std::cout << " we do not have any senders for receiver rank " << receiver_rank << "\n";
    }
    rval = cgraph->receive_mesh( *global, pco, local_set, senders_local );CHKERRVAL( rval );

    // after we are done, we could merge vertices that come from different senders, but
    // have the same global id
    Tag idtag;
    rval = context.MBI->tag_get_handle( "GLOBAL_ID", idtag );CHKERRVAL( rval );

    //   data.point_cloud = false;
    Range local_ents;
    rval = context.MBI->get_entities_by_handle( local_set, local_ents );CHKERRVAL( rval );

    // do not do merge if point cloud
    if( !local_ents.all_of_type( MBVERTEX ) )
    {
        if( (int)senders_local.size() >= 2 )  // need to remove duplicate vertices
        // that might come from different senders
        {

            Range local_verts = local_ents.subset_by_type( MBVERTEX );
            Range local_elems = subtract( local_ents, local_verts );

            // remove from local set the vertices
            rval = context.MBI->remove_entities( local_set, local_verts );CHKERRVAL( rval );

#ifdef VERBOSE
            std::cout << "current_receiver " << current_receiver << " local verts: " << local_verts.size() << "\n";
#endif
            MergeMesh mm( context.MBI );

            rval = mm.merge_using_integer_tag( local_verts, idtag );CHKERRVAL( rval );

            Range new_verts;  // local elems are local entities without vertices
            rval = context.MBI->get_connectivity( local_elems, new_verts );CHKERRVAL( rval );

#ifdef VERBOSE
            std::cout << "after merging: new verts: " << new_verts.size() << "\n";
#endif
            rval = context.MBI->add_entities( local_set, new_verts );CHKERRVAL( rval );
        }
    }
    else
        data.point_cloud = true;

    if( !data.point_cloud )
    {
        // still need to resolve shared entities (in this case, vertices )
        rval = pco->resolve_shared_ents( local_set, -1, -1, &idtag );CHKERRVAL( rval );
    }
    else
    {
        // if partition tag exists, set it to current rank; just to make it visible in VisIt
        Tag densePartTag;
        rval = context.MBI->tag_get_handle( "partition", densePartTag );
        if( NULL != densePartTag && MB_SUCCESS == rval )
        {
            Range local_verts;
            rval = context.MBI->get_entities_by_dimension( local_set, 0, local_verts );CHKERRVAL( rval );
            std::vector< int > vals;
            int rank = pco->rank();
            vals.resize( local_verts.size(), rank );
            rval = context.MBI->tag_set_data( densePartTag, local_verts, &vals[0] );CHKERRVAL( rval );
        }
    }
    // set the parallel partition tag
    Tag part_tag;
    int dum_id = -1;
    rval       = context.MBI->tag_get_handle( "PARALLEL_PARTITION", 1, MB_TYPE_INTEGER, part_tag,
                                        MB_TAG_CREAT | MB_TAG_SPARSE, &dum_id );

    if( part_tag == NULL || ( ( rval != MB_SUCCESS ) && ( rval != MB_ALREADY_ALLOCATED ) ) )
    {
        std::cout << " can't get par part tag.\n";
        return 1;
    }

    int rank = pco->rank();
    rval     = context.MBI->tag_set_data( part_tag, &local_set, 1, &rank );CHKERRVAL( rval );

    // populate the mesh with current data info
    ierr = iMOAB_UpdateMeshInfo( pid );
    CHKIERRVAL( ierr );

    // mark for deletion
    MPI_Group_free( &receiverGroup );

    return 0;
}

ErrCode iMOAB_ReceiveMeshFortran( iMOAB_AppID pid, int* join, int* sendingGroup, int* scompid )
{
    MPI_Comm jcomm = MPI_Comm_f2c( (MPI_Fint) *join );
    MPI_Group sendGroup = MPI_Group_f2c( (MPI_Fint) *sendingGroup );
    return iMOAB_ReceiveMesh( pid, &jcomm, &sendGroup, scompid );
}

ErrCode iMOAB_SendElementTag( iMOAB_AppID pid, const iMOAB_String tag_storage_name, MPI_Comm* join, int* context_id,
                              int tag_storage_name_length )
{

    appData& data                               = context.appDatas[*pid];
    std::map< int, ParCommGraph* >::iterator mt = data.pgraph.find( *context_id );
    if( mt == data.pgraph.end() )
    {
        return 1;
    }
    ParCommGraph* cgraph = mt->second;
    ParallelComm* pco    = context.pcomms[*pid];
    Range owned          = data.owned_elems;
    ErrorCode rval;
    EntityHandle cover_set;

    if( data.point_cloud )
    {
        owned = data.local_verts;
    }
    // another possibility is for par comm graph to be computed from iMOAB_ComputeCommGraph, for
    // after atm ocn intx, from phys (case from imoab_phatm_ocn_coupler.cpp) get then the cover set
    // from ints remapper
#ifdef MOAB_HAVE_TEMPESTREMAP
    if( data.tempestData.remapper != NULL )  // this is the case this is part of intx;;
    {
        cover_set = data.tempestData.remapper->GetMeshSet( Remapper::CoveringMesh );
        rval      = context.MBI->get_entities_by_dimension( cover_set, 2, owned );CHKERRVAL( rval );
        // should still have only quads ?
    }
#else
    // now, in case we are sending from intx between ocn and atm, we involve coverage set
    // how do I know if this receiver already participated in an intersection driven by coupler?
    // also, what if this was the "source" mesh in intx?
    // in that case, the elements might have been instantiated in the coverage set locally, the
    // "owned" range can be different the elements are now in tempestRemap coverage_set
    cover_set = cgraph->get_cover_set();  // this will be non null only for intx app ?

    if( 0 != cover_set )
    {
        rval = context.MBI->get_entities_by_dimension( cover_set, 2, owned );CHKERRVAL( rval );
    }
#endif

    std::string tag_name( tag_storage_name );

    if( tag_storage_name_length < (int)strlen( tag_storage_name ) )
    {
        tag_name = tag_name.substr( 0, tag_storage_name_length );
    }
    // basically, we assume everything is defined already on the tag,
    //   and we can get the tags just by its name
    // we assume that there are separators ";" between the tag names
    std::vector< std::string > tagNames;
    std::vector< Tag > tagHandles;
    std::string separator( ";" );
    split_tag_names( tag_name, separator, tagNames );
    for( size_t i = 0; i < tagNames.size(); i++ )
    {
        Tag tagHandle;
        rval = context.MBI->tag_get_handle( tagNames[i].c_str(), tagHandle );
        if( MB_SUCCESS != rval || NULL == tagHandle )
        {
            return 1;
        }
        tagHandles.push_back( tagHandle );
    }

    // pco is needed to pack, and for moab instance, not for communication!
    // still use nonblocking communication, over the joint comm
    rval = cgraph->send_tag_values( *join, pco, owned, tagHandles );CHKERRVAL( rval );
    // now, send to each corr_tasks[i] tag data for corr_sizes[i] primary entities

    return 0;
}

// fortran wrapper
ErrCode iMOAB_SendElementTagFortran( iMOAB_AppID pid, const iMOAB_String tag_storage_name, int* join, int* context_id,
                              int tag_storage_name_length )
{
    MPI_Comm jcomm = MPI_Comm_f2c( (MPI_Fint) *join );
    return iMOAB_SendElementTag(pid, tag_storage_name, &jcomm, context_id, tag_storage_name_length);
}

ErrCode iMOAB_ReceiveElementTag( iMOAB_AppID pid, const iMOAB_String tag_storage_name, MPI_Comm* join, int* context_id,
                                 int tag_storage_name_length )
{
    appData& data = context.appDatas[*pid];

    std::map< int, ParCommGraph* >::iterator mt = data.pgraph.find( *context_id );
    if( mt == data.pgraph.end() )
    {
        return 1;
    }
    ParCommGraph* cgraph = mt->second;

    ParallelComm* pco = context.pcomms[*pid];
    Range owned       = data.owned_elems;

    // how do I know if this receiver already participated in an intersection driven by coupler?
    // also, what if this was the "source" mesh in intx?
    // in that case, the elements might have been instantiated in the coverage set locally, the
    // "owned" range can be different the elements are now in tempestRemap coverage_set
    EntityHandle cover_set = cgraph->get_cover_set();
    ErrorCode rval;
    if( 0 != cover_set )
    {
        rval = context.MBI->get_entities_by_dimension( cover_set, 2, owned );CHKERRVAL( rval );
    }
    if( data.point_cloud )
    {
        owned = data.local_verts;
    }
    // another possibility is for par comm graph to be computed from iMOAB_ComputeCommGraph, for
    // after atm ocn intx, from phys (case from imoab_phatm_ocn_coupler.cpp) get then the cover set
    // from ints remapper
#ifdef MOAB_HAVE_TEMPESTREMAP
    if( data.tempestData.remapper != NULL )  // this is the case this is part of intx;;
    {
        cover_set = data.tempestData.remapper->GetMeshSet( Remapper::CoveringMesh );
        rval      = context.MBI->get_entities_by_dimension( cover_set, 2, owned );CHKERRVAL( rval );
        // should still have only quads ?
    }
#endif
    /*
     * data_intx.remapper exists though only on the intersection application
     *  how do we get from here ( we know the pid that receives, and the commgraph used by migrate
     * mesh )
     */

    std::string tag_name( tag_storage_name );

    if( tag_storage_name_length < (int)strlen( tag_storage_name ) )
    {
        tag_name = tag_name.substr( 0, tag_storage_name_length );
    }

    // we assume that there are separators ";" between the tag names
    std::vector< std::string > tagNames;
    std::vector< Tag > tagHandles;
    std::string separator( ";" );
    split_tag_names( tag_name, separator, tagNames );
    for( size_t i = 0; i < tagNames.size(); i++ )
    {
        Tag tagHandle;
        rval = context.MBI->tag_get_handle( tagNames[i].c_str(), tagHandle );
        if( MB_SUCCESS != rval || NULL == tagHandle )
        {
            return 1;
        }
        tagHandles.push_back( tagHandle );
    }

#ifdef VERBOSE
    std::cout << pco->rank() << ". Looking to receive data for tags: " << tag_name
              << " and file set = " << ( data.file_set ) << "\n";
#endif
    // pco is needed to pack, and for moab instance, not for communication!
    // still use nonblocking communication
    rval = cgraph->receive_tag_values( *join, pco, owned, tagHandles );CHKERRVAL( rval );

#ifdef VERBOSE
    std::cout << pco->rank() << ". Looking to receive data for tags: " << tag_name << "\n";
#endif

    return 0;
}
ErrCode iMOAB_ReceiveElementTagFortran( iMOAB_AppID pid, const iMOAB_String tag_storage_name, int* join,
        int* context_id,  int tag_storage_name_length )
{
    MPI_Comm jcomm = MPI_Comm_f2c( (MPI_Fint) *join );
    return iMOAB_ReceiveElementTag( pid, tag_storage_name, &jcomm,
          context_id, tag_storage_name_length );
}
ErrCode iMOAB_FreeSenderBuffers( iMOAB_AppID pid, int* context_id )
{
    // need first to find the pgraph that holds the information we need
    // this will be called on sender side only
    appData& data                               = context.appDatas[*pid];
    std::map< int, ParCommGraph* >::iterator mt = data.pgraph.find( *context_id );
    if( mt == data.pgraph.end() ) return 1;  // error

    mt->second->release_send_buffers();
    return 0;
}

/**
\brief compute a comm graph between 2 moab apps, based on ID matching
<B>Operations:</B> Collective
*/
//#define VERBOSE
ErrCode iMOAB_ComputeCommGraph( iMOAB_AppID pid1, iMOAB_AppID pid2, MPI_Comm* join, MPI_Group* group1,
                                MPI_Group* group2, int* type1, int* type2, int* comp1, int* comp2 )
{
    ErrorCode rval = MB_SUCCESS;
    int localRank = 0, numProcs = 1;
    MPI_Comm_rank( *join, &localRank );
    MPI_Comm_size( *join, &numProcs );
    // instantiate the par comm graph
    // ParCommGraph::ParCommGraph(MPI_Comm joincomm, MPI_Group group1, MPI_Group group2, int coid1,
    // int coid2)
    ParCommGraph* cgraph = NULL;
    if( *pid1 >= 0 ) cgraph = new ParCommGraph( *join, *group1, *group2, *comp1, *comp2 );
    ParCommGraph* cgraph_rev = NULL;
    if( *pid2 >= 0 ) cgraph_rev = new ParCommGraph( *join, *group2, *group1, *comp2, *comp1 );
    // we should search if we have another pcomm with the same comp ids in the list already
    // sort of check existing comm graphs in the map context.appDatas[*pid].pgraph
    if( *pid1 >= 0 ) context.appDatas[*pid1].pgraph[*comp2] = cgraph;      // the context will be the other comp
    if( *pid2 >= 0 ) context.appDatas[*pid2].pgraph[*comp1] = cgraph_rev;  // from 2 to 1
    // each model has a list of global ids that will need to be sent by gs to rendezvous the other
    // model on the joint comm
    TupleList TLcomp1;
    TLcomp1.initialize( 2, 0, 0, 0, 0 );  // to proc, marker
    TupleList TLcomp2;
    TLcomp2.initialize( 2, 0, 0, 0, 0 );  // to proc, marker
    // will push_back a new tuple, if needed

    TLcomp1.enableWriteAccess();

    // tags of interest are either GLOBAL_DOFS or GLOBAL_ID
    Tag gdsTag;
    // find the values on first cell
    int lenTagType1 = 1;
    if( 1 == *type1 || 1 == *type2 )
    {
        rval = context.MBI->tag_get_handle( "GLOBAL_DOFS", gdsTag );CHKERRVAL( rval );
        rval = context.MBI->tag_get_length( gdsTag, lenTagType1 );CHKERRVAL( rval );  // usually it is 16
    }
    Tag tagType2 = context.MBI->globalId_tag();

    std::vector< int > valuesComp1;
    // populate first tuple
    if( *pid1 >= 0 )
    {
        appData& data1     = context.appDatas[*pid1];
        EntityHandle fset1 = data1.file_set;
        // in case of tempest remap, get the coverage set
#ifdef MOAB_HAVE_TEMPESTREMAP
        if( data1.tempestData.remapper != NULL )  // this is the case this is part of intx;;
        {
            fset1 = data1.tempestData.remapper->GetMeshSet( Remapper::CoveringMesh );
            // should still have only quads ?
        }
#endif
        Range ents_of_interest;
        if( *type1 == 1 )
        {
            assert( gdsTag );
            rval = context.MBI->get_entities_by_type( fset1, MBQUAD, ents_of_interest );CHKERRVAL( rval );
            valuesComp1.resize( ents_of_interest.size() * lenTagType1 );
            rval = context.MBI->tag_get_data( gdsTag, ents_of_interest, &valuesComp1[0] );CHKERRVAL( rval );
        }
        else if( *type1 == 2 )
        {
            rval = context.MBI->get_entities_by_type( fset1, MBVERTEX, ents_of_interest );CHKERRVAL( rval );
            valuesComp1.resize( ents_of_interest.size() );
            rval = context.MBI->tag_get_data( tagType2, ents_of_interest, &valuesComp1[0] );CHKERRVAL( rval );  // just global ids
        }
        else if( *type1 == 3 )  // for FV meshes, just get the global id of cell
        {
            rval = context.MBI->get_entities_by_dimension( fset1, 2, ents_of_interest );CHKERRVAL( rval );
            valuesComp1.resize( ents_of_interest.size() );
            rval = context.MBI->tag_get_data( tagType2, ents_of_interest, &valuesComp1[0] );CHKERRVAL( rval );  // just global ids
        }
        else
        {
            CHKERRVAL( MB_FAILURE );  // we know only type 1 or 2 or 3
        }
        // now fill the tuple list with info and markers
        // because we will send only the ids, order and compress the list
        std::set< int > uniq( valuesComp1.begin(), valuesComp1.end() );
        TLcomp1.resize( uniq.size() );
        for( std::set< int >::iterator sit = uniq.begin(); sit != uniq.end(); sit++ )
        {
            // to proc, marker, element local index, index in el
            int marker               = *sit;
            int to_proc              = marker % numProcs;
            int n                    = TLcomp1.get_n();
            TLcomp1.vi_wr[2 * n]     = to_proc;  // send to processor
            TLcomp1.vi_wr[2 * n + 1] = marker;
            TLcomp1.inc_n();
        }
    }

    ProcConfig pc( *join );  // proc config does the crystal router
    pc.crystal_router()->gs_transfer( 1, TLcomp1,
                                      0 );  // communication towards joint tasks, with markers
    // sort by value (key 1)
#ifdef VERBOSE
    std::stringstream ff1;
    ff1 << "TLcomp1_" << localRank << ".txt";
    TLcomp1.print_to_file( ff1.str().c_str() );  // it will append!
#endif
    moab::TupleList::buffer sort_buffer;
    sort_buffer.buffer_init( TLcomp1.get_n() );
    TLcomp1.sort( 1, &sort_buffer );
    sort_buffer.reset();
#ifdef VERBOSE
    // after sorting
    TLcomp1.print_to_file( ff1.str().c_str() );  // it will append!
#endif
    // do the same, for the other component, number2, with type2
    // start copy
    TLcomp2.enableWriteAccess();
    // populate second tuple
    std::vector< int > valuesComp2;
    if( *pid2 >= 0 )
    {
        appData& data2     = context.appDatas[*pid2];
        EntityHandle fset2 = data2.file_set;
        // in case of tempest remap, get the coverage set
#ifdef MOAB_HAVE_TEMPESTREMAP
        if( data2.tempestData.remapper != NULL )  // this is the case this is part of intx;;
        {
            fset2 = data2.tempestData.remapper->GetMeshSet( Remapper::CoveringMesh );
            // should still have only quads ?
        }
#endif

        Range ents_of_interest;
        if( *type2 == 1 )
        {
            assert( gdsTag );
            rval = context.MBI->get_entities_by_type( fset2, MBQUAD, ents_of_interest );CHKERRVAL( rval );
            valuesComp2.resize( ents_of_interest.size() * lenTagType1 );
            rval = context.MBI->tag_get_data( gdsTag, ents_of_interest, &valuesComp2[0] );CHKERRVAL( rval );
        }
        else if( *type2 == 2 )
        {
            rval = context.MBI->get_entities_by_type( fset2, MBVERTEX, ents_of_interest );CHKERRVAL( rval );
            valuesComp2.resize( ents_of_interest.size() );  // stride is 1 here
            rval = context.MBI->tag_get_data( tagType2, ents_of_interest, &valuesComp2[0] );CHKERRVAL( rval );  // just global ids
        }
        else if( *type2 == 3 )
        {
            rval = context.MBI->get_entities_by_dimension( fset2, 2, ents_of_interest );CHKERRVAL( rval );
            valuesComp2.resize( ents_of_interest.size() );  // stride is 1 here
            rval = context.MBI->tag_get_data( tagType2, ents_of_interest, &valuesComp2[0] );CHKERRVAL( rval );  // just global ids
        }
        else
        {
            CHKERRVAL( MB_FAILURE );  // we know only type 1 or 2
        }
        // now fill the tuple list with info and markers
        std::set< int > uniq( valuesComp2.begin(), valuesComp2.end() );
        TLcomp2.resize( uniq.size() );
        for( std::set< int >::iterator sit = uniq.begin(); sit != uniq.end(); sit++ )
        {
            // to proc, marker, element local index, index in el
            int marker               = *sit;
            int to_proc              = marker % numProcs;
            int n                    = TLcomp2.get_n();
            TLcomp2.vi_wr[2 * n]     = to_proc;  // send to processor
            TLcomp2.vi_wr[2 * n + 1] = marker;
            TLcomp2.inc_n();
        }
    }
    pc.crystal_router()->gs_transfer( 1, TLcomp2,
                                      0 );  // communication towards joint tasks, with markers
    // sort by value (key 1)
#ifdef VERBOSE
    std::stringstream ff2;
    ff2 << "TLcomp2_" << localRank << ".txt";
    TLcomp2.print_to_file( ff2.str().c_str() );
#endif
    sort_buffer.buffer_reserve( TLcomp2.get_n() );
    TLcomp2.sort( 1, &sort_buffer );
    sort_buffer.reset();
    // end copy
#ifdef VERBOSE
    TLcomp2.print_to_file( ff2.str().c_str() );
#endif
    // need to send back the info, from the rendezvous point, for each of the values
    /* so go over each value, on local process in joint communicator

    now have to send back the info needed for communication;
     loop in in sync over both TLComp1 and TLComp2, in local process;
      So, build new tuple lists, to send synchronous communication
      populate them at the same time, based on marker, that is indexed
    */

    TupleList TLBackToComp1;
    TLBackToComp1.initialize( 3, 0, 0, 0, 0 );  // to proc, marker, from proc on comp2,
    TLBackToComp1.enableWriteAccess();

    TupleList TLBackToComp2;
    TLBackToComp2.initialize( 3, 0, 0, 0, 0 );  // to proc, marker,  from proc,
    TLBackToComp2.enableWriteAccess();

    int n1 = TLcomp1.get_n();
    int n2 = TLcomp2.get_n();

    int indexInTLComp1 = 0;
    int indexInTLComp2 = 0;  // advance both, according to the marker
    if( n1 > 0 && n2 > 0 )
    {

        while( indexInTLComp1 < n1 && indexInTLComp2 < n2 )  // if any is over, we are done
        {
            int currentValue1 = TLcomp1.vi_rd[2 * indexInTLComp1 + 1];
            int currentValue2 = TLcomp2.vi_rd[2 * indexInTLComp2 + 1];
            if( currentValue1 < currentValue2 )
            {
                // we have a big problem; basically, we are saying that
                // dof currentValue is on one model and not on the other
                // std::cout << " currentValue1:" << currentValue1 << " missing in comp2" << "\n";
                indexInTLComp1++;
                continue;
            }
            if( currentValue1 > currentValue2 )
            {
                // std::cout << " currentValue2:" << currentValue2 << " missing in comp1" << "\n";
                indexInTLComp2++;
                continue;
            }
            int size1 = 1;
            int size2 = 1;
            while( indexInTLComp1 + size1 < n1 && currentValue1 == TLcomp1.vi_rd[2 * ( indexInTLComp1 + size1 ) + 1] )
                size1++;
            while( indexInTLComp2 + size2 < n2 && currentValue2 == TLcomp2.vi_rd[2 * ( indexInTLComp2 + size2 ) + 1] )
                size2++;
            // must be found in both lists, find the start and end indices
            for( int i1 = 0; i1 < size1; i1++ )
            {
                for( int i2 = 0; i2 < size2; i2++ )
                {
                    // send the info back to components
                    int n = TLBackToComp1.get_n();
                    TLBackToComp1.reserve();
                    TLBackToComp1.vi_wr[3 * n] =
                        TLcomp1.vi_rd[2 * ( indexInTLComp1 + i1 )];  // send back to the proc marker
                                                                     // came from, info from comp2
                    TLBackToComp1.vi_wr[3 * n + 1] = currentValue1;  // initial value (resend?)
                    TLBackToComp1.vi_wr[3 * n + 2] = TLcomp2.vi_rd[2 * ( indexInTLComp2 + i2 )];  // from proc on comp2
                    n                              = TLBackToComp2.get_n();
                    TLBackToComp2.reserve();
                    TLBackToComp2.vi_wr[3 * n] =
                        TLcomp2.vi_rd[2 * ( indexInTLComp2 + i2 )];  // send back info to original
                    TLBackToComp2.vi_wr[3 * n + 1] = currentValue1;  // initial value (resend?)
                    TLBackToComp2.vi_wr[3 * n + 2] = TLcomp1.vi_rd[2 * ( indexInTLComp1 + i1 )];  // from proc on comp1
                    // what if there are repeated markers in TLcomp2? increase just index2
                }
            }
            indexInTLComp1 += size1;
            indexInTLComp2 += size2;
        }
    }
    pc.crystal_router()->gs_transfer( 1, TLBackToComp1, 0 );  // communication towards original tasks, with info about
    pc.crystal_router()->gs_transfer( 1, TLBackToComp2, 0 );

    if( *pid1 >= 0 )
    {
        // we are on original comp 1 tasks
        // before ordering
        // now for each value in TLBackToComp1.vi_rd[3*i+1], on current proc, we know the
        // processors it communicates with
#ifdef VERBOSE
        std::stringstream f1;
        f1 << "TLBack1_" << localRank << ".txt";
        TLBackToComp1.print_to_file( f1.str().c_str() );
#endif
        sort_buffer.buffer_reserve( TLBackToComp1.get_n() );
        TLBackToComp1.sort( 1, &sort_buffer );
        sort_buffer.reset();
#ifdef VERBOSE
        TLBackToComp1.print_to_file( f1.str().c_str() );
#endif
        // so we are now on pid1, we know now each marker were it has to go
        // add a new method to ParCommGraph, to set up the involved_IDs_map
        cgraph->settle_comm_by_ids( *comp1, TLBackToComp1, valuesComp1 );
    }
    if( *pid2 >= 0 )
    {
        // we are on original comp 1 tasks
        // before ordering
        // now for each value in TLBackToComp1.vi_rd[3*i+1], on current proc, we know the
        // processors it communicates with
#ifdef VERBOSE
        std::stringstream f2;
        f2 << "TLBack2_" << localRank << ".txt";
        TLBackToComp2.print_to_file( f2.str().c_str() );
#endif
        sort_buffer.buffer_reserve( TLBackToComp2.get_n() );
        TLBackToComp2.sort( 2, &sort_buffer );
        sort_buffer.reset();
#ifdef VERBOSE
        TLBackToComp2.print_to_file( f2.str().c_str() );
#endif
        cgraph_rev->settle_comm_by_ids( *comp2, TLBackToComp2, valuesComp2 );
        //
    }
    return 0;
}

ErrCode iMOAB_ComputeCommGraphFortran( iMOAB_AppID pid1, iMOAB_AppID pid2, int* join, int* group1,
                                int* group2, int* type1, int* type2, int* comp1, int* comp2 )
{
    MPI_Comm jcomm = MPI_Comm_f2c( (MPI_Fint) *join );
    MPI_Group gr1 = MPI_Group_f2c( (MPI_Fint) *group1 );
    MPI_Group gr2 = MPI_Group_f2c( (MPI_Fint) *group2 );
    return iMOAB_ComputeCommGraph( pid1, pid2, &jcomm, &gr1,
            &gr2, type1, type2, comp1, comp2 );
}

//#undef VERBOSE

ErrCode iMOAB_MergeVertices( iMOAB_AppID pid )
{
    appData& data     = context.appDatas[*pid];
    ParallelComm* pco = context.pcomms[*pid];
    // collapse vertices and transform cells into triangles/quads /polys
    // tags we care about: area, frac, global id
    std::vector< Tag > tagsList;
    Tag tag;
    ErrorCode rval = context.MBI->tag_get_handle( "GLOBAL_ID", tag );
    if( !tag || rval != MB_SUCCESS ) return 1;  // fatal error, abort
    tagsList.push_back( tag );
    rval = context.MBI->tag_get_handle( "area", tag );
    if( tag && rval == MB_SUCCESS ) tagsList.push_back( tag );
    rval = context.MBI->tag_get_handle( "frac", tag );
    if( tag && rval == MB_SUCCESS ) tagsList.push_back( tag );
    double tol = 1.0e-9;
    rval       = IntxUtils::remove_duplicate_vertices( context.MBI, data.file_set, tol, tagsList );CHKERRVAL( rval );

    // clean material sets of cells that were deleted
    rval = context.MBI->get_entities_by_type_and_tag( data.file_set, MBENTITYSET, &( context.material_tag ), 0, 1,
                                                      data.mat_sets, Interface::UNION );CHKERRVAL( rval );

    if( !data.mat_sets.empty() )
    {
        EntityHandle matSet = data.mat_sets[0];
        Range elems;
        rval = context.MBI->get_entities_by_dimension( matSet, 2, elems );CHKERRVAL( rval );
        rval = context.MBI->remove_entities( matSet, elems );CHKERRVAL( rval );
        // put back only cells from data.file_set
        elems.clear();
        rval = context.MBI->get_entities_by_dimension( data.file_set, 2, elems );CHKERRVAL( rval );
        rval = context.MBI->add_entities( matSet, elems );CHKERRVAL( rval );
    }
    int ierr = iMOAB_UpdateMeshInfo( pid );
    if( ierr > 0 ) return ierr;
    ParallelMergeMesh pmm( pco, tol );
    rval = pmm.merge( data.file_set,
                      /* do not do local merge*/ false,
                      /*  2d cells*/ 2 );CHKERRVAL( rval );

    // assign global ids only for vertices, cells have them fine
    rval = pco->assign_global_ids( data.file_set, /*dim*/ 0 );CHKERRVAL( rval );

    // set the partition tag on the file set
    Tag part_tag;
    int dum_id = -1;
    rval       = context.MBI->tag_get_handle( "PARALLEL_PARTITION", 1, MB_TYPE_INTEGER, part_tag,
                                        MB_TAG_CREAT | MB_TAG_SPARSE, &dum_id );

    if( part_tag == NULL || ( ( rval != MB_SUCCESS ) && ( rval != MB_ALREADY_ALLOCATED ) ) )
    {
        std::cout << " can't get par part tag.\n";
        return 1;
    }

    int rank = pco->rank();
    rval     = context.MBI->tag_set_data( part_tag, &data.file_set, 1, &rank );CHKERRVAL( rval );

    return 0;
}

#ifdef MOAB_HAVE_TEMPESTREMAP

// this call must be collective on the joint communicator
//  intersection tasks on coupler will need to send to the components tasks the list of
// id elements that are relevant: they intersected some of the target elements (which are not needed
// here)
//  in the intersection
ErrCode iMOAB_CoverageGraph( MPI_Comm* join, iMOAB_AppID pid_src, iMOAB_AppID pid_migr, iMOAB_AppID pid_intx,
                             int* src_id, int* migr_id, int* context_id )
{
    // first, based on the scompid and migrcomp, find the parCommGraph corresponding to this
    // exchange
    ErrorCode rval;
    std::vector< int > srcSenders;
    std::vector< int > receivers;
    ParCommGraph* sendGraph = NULL;
    int ierr;
    int default_context_id = -1;

    // First, find the original graph between PE sets
    // And based on this one, we will build the newly modified one for coverage
    if( *pid_src >= 0 )
    {
        default_context_id = *migr_id;  // the other one
        assert( context.appDatas[*pid_src].global_id == *src_id );
        sendGraph = context.appDatas[*pid_src].pgraph[default_context_id];  // maybe check if it does not exist

        // report the sender and receiver tasks in the joint comm
        srcSenders = sendGraph->senders();
        receivers  = sendGraph->receivers();
#ifdef VERBOSE
        std::cout << "senders: " << srcSenders.size() << " first sender: " << srcSenders[0] << std::endl;
#endif
    }

    ParCommGraph* recvGraph = NULL;  // will be non null on receiver tasks (intx tasks)
    if( *pid_migr >= 0 )
    {
        // find the original one
        default_context_id = *src_id;
        assert( context.appDatas[*pid_migr].global_id == *migr_id );
        recvGraph = context.appDatas[*pid_migr].pgraph[default_context_id];
        // report the sender and receiver tasks in the joint comm, from migrated mesh pt of view
        srcSenders = recvGraph->senders();
        receivers  = recvGraph->receivers();
#ifdef VERBOSE
        std::cout << "receivers: " << receivers.size() << " first receiver: " << receivers[0] << std::endl;
#endif
    }

    // loop over pid_intx elements, to see what original processors in joint comm have sent the
    // coverage mesh;
    // If we are on intx tasks, send coverage info towards original component tasks,
    // about needed cells
    TupleList TLcovIDs;
    TLcovIDs.initialize( 2, 0, 0, 0, 0 );  // to proc, GLOBAL ID; estimate about 100 IDs to be sent
    // will push_back a new tuple, if needed
    TLcovIDs.enableWriteAccess();
    // the crystal router will send ID cell to the original source, on the component task
    // if we are on intx tasks, loop over all intx elements and

    int currentRankInJointComm = -1;
    ierr                       = MPI_Comm_rank( *join, &currentRankInJointComm );
    CHKIERRVAL( ierr );

    // if currentRankInJointComm is in receivers list, it means that we are on intx tasks too, we
    // need to send information towards component tasks
    if( find( receivers.begin(), receivers.end(), currentRankInJointComm ) !=
        receivers.end() )  // we are on receivers tasks, we can request intx info
    {
        // find the pcomm for the intx pid
        if( *pid_intx >= (int)context.appDatas.size() ) return 1;

        appData& dataIntx = context.appDatas[*pid_intx];
        Tag parentTag, orgSendProcTag;

        rval = context.MBI->tag_get_handle( "SourceParent", parentTag );CHKERRVAL( rval );          // global id of the blue, source element
        if( !parentTag ) return 1;  // fatal error, abort

        rval = context.MBI->tag_get_handle( "orig_sending_processor", orgSendProcTag );CHKERRVAL( rval );
        if( !orgSendProcTag ) return 1;  // fatal error, abort

        // find the file set, red parents for intx cells, and put them in tuples
        EntityHandle intxSet = dataIntx.file_set;
        Range cells;
        // get all entities from the set, and look at their RedParent
        rval = context.MBI->get_entities_by_dimension( intxSet, 2, cells );CHKERRVAL( rval );

        std::map< int, std::set< int > > idsFromProcs;  // send that info back to enhance parCommGraph cache
        for( Range::iterator it = cells.begin(); it != cells.end(); it++ )
        {
            EntityHandle intx_cell = *it;
            int gidCell, origProc;  // look at receivers

            rval = context.MBI->tag_get_data( parentTag, &intx_cell, 1, &gidCell );CHKERRVAL( rval );
            rval = context.MBI->tag_get_data( orgSendProcTag, &intx_cell, 1, &origProc );CHKERRVAL( rval );
            // we have augmented the overlap set with ghost cells ; in that case, the
            // orig_sending_processor is not set so it will be -1;
            if( origProc < 0 ) continue;

            std::set< int >& setInts = idsFromProcs[origProc];
            setInts.insert( gidCell );
        }

        // if we have no intx cells, it means we are on point clouds; quick fix just use all cells
        // from coverage set
        if( cells.empty() )
        {
            // get coverage set
            assert( *pid_intx >= 0 );
            appData& dataIntx      = context.appDatas[*pid_intx];
            EntityHandle cover_set = dataIntx.tempestData.remapper->GetMeshSet( Remapper::CoveringMesh );

            // get all cells from coverage set
            Tag gidTag;
            rval = context.MBI->tag_get_handle( "GLOBAL_ID", gidTag );CHKERRVAL( rval );
            rval = context.MBI->get_entities_by_dimension( cover_set, 2, cells );CHKERRVAL( rval );
            // look at their orig_sending_processor
            for( Range::iterator it = cells.begin(); it != cells.end(); it++ )
            {
                EntityHandle covCell = *it;
                int gidCell, origProc;  // look at o

                rval = context.MBI->tag_get_data( gidTag, &covCell, 1, &gidCell );CHKERRVAL( rval );
                rval = context.MBI->tag_get_data( orgSendProcTag, &covCell, 1, &origProc );CHKERRVAL( rval );
                // we have augmented the overlap set with ghost cells ; in that case, the
                // orig_sending_processor is not set so it will be -1;
                if( origProc < 0 )  // it cannot < 0, I think
                    continue;
                std::set< int >& setInts = idsFromProcs[origProc];
                setInts.insert( gidCell );
            }
        }

#ifdef VERBOSE
        std::ofstream dbfile;
        std::stringstream outf;
        outf << "idsFromProc_0" << currentRankInJointComm << ".txt";
        dbfile.open( outf.str().c_str() );
        dbfile << "Writing this to a file.\n";

        dbfile << " map size:" << idsFromProcs.size()
               << std::endl;  // on the receiver side, these show how much data to receive
        // from the sender (how many ids, and how much tag data later; we need to size up the
        // receiver buffer) arrange in tuples , use map iterators to send the ids
        for( std::map< int, std::set< int > >::iterator mt = idsFromProcs.begin(); mt != idsFromProcs.end(); mt++ )
        {
            std::set< int >& setIds = mt->second;
            dbfile << "from id: " << mt->first << " receive " << setIds.size() << " cells \n";
            int counter = 0;
            for( std::set< int >::iterator st = setIds.begin(); st != setIds.end(); st++ )
            {
                int valueID = *st;
                dbfile << " " << valueID;
                counter++;
                if( counter % 10 == 0 ) dbfile << "\n";
            }
            dbfile << "\n";
        }
        dbfile.close();
#endif
        if( NULL != recvGraph )
        {
            ParCommGraph* recvGraph1 = new ParCommGraph( *recvGraph );  // just copy
            recvGraph1->set_context_id( *context_id );
            recvGraph1->SetReceivingAfterCoverage( idsFromProcs );
            // this par comm graph will need to use the coverage set
            // so we are for sure on intx pes (the receiver is the coupler mesh)
            assert( *pid_intx >= 0 );
            appData& dataIntx      = context.appDatas[*pid_intx];
            EntityHandle cover_set = dataIntx.tempestData.remapper->GetMeshSet( Remapper::CoveringMesh );
            recvGraph1->set_cover_set( cover_set );
            context.appDatas[*pid_migr].pgraph[*context_id] = recvGraph1;
        }
        for( std::map< int, std::set< int > >::iterator mit = idsFromProcs.begin(); mit != idsFromProcs.end(); mit++ )
        {
            int procToSendTo       = mit->first;
            std::set< int >& idSet = mit->second;
            for( std::set< int >::iterator sit = idSet.begin(); sit != idSet.end(); sit++ )
            {
                int n = TLcovIDs.get_n();
                TLcovIDs.reserve();
                TLcovIDs.vi_wr[2 * n]     = procToSendTo;  // send to processor
                TLcovIDs.vi_wr[2 * n + 1] = *sit;          // global id needs index in the local_verts range
            }
        }
    }

    ProcConfig pc( *join );  // proc config does the crystal router
    pc.crystal_router()->gs_transfer( 1, TLcovIDs,
                                      0 );  // communication towards component tasks, with what ids are needed
    // for each task from receiver

    // a test to know if we are on the sender tasks (original component, in this case, atmosphere)
    if( NULL != sendGraph )
    {
        // collect TLcovIDs tuple, will set in a local map/set, the ids that are sent to each
        // receiver task
        ParCommGraph* sendGraph1 = new ParCommGraph( *sendGraph );  // just copy
        sendGraph1->set_context_id( *context_id );
        context.appDatas[*pid_src].pgraph[*context_id] = sendGraph1;
        rval                                           = sendGraph1->settle_send_graph( TLcovIDs );CHKERRVAL( rval );
    }
    return 0;  // success
}

ErrCode iMOAB_CoverageGraphFortran( int* join, iMOAB_AppID pid_src, iMOAB_AppID pid_migr,
        iMOAB_AppID pid_intx, int* src_id, int* migr_id, int* context_id )
{
    MPI_Comm jcomm = MPI_Comm_f2c( (MPI_Fint) *join );
    return iMOAB_CoverageGraph(&jcomm,  pid_src,  pid_migr,  pid_intx, src_id, migr_id, context_id );
}


ErrCode iMOAB_DumpCommGraph( iMOAB_AppID pid, int* context_id, int* is_sender, const iMOAB_String prefix,
                             int length_prefix )
{
    ParCommGraph* cgraph = context.appDatas[*pid].pgraph[*context_id];
    std::string prefix_str( prefix );
    if( length_prefix < (int)strlen( prefix ) )
    {
        prefix_str = prefix_str.substr( 0, length_prefix );
    }
    if( NULL != cgraph )
        cgraph->dump_comm_information( prefix_str, *is_sender );
    else
    {
        std::cout << " cannot find ParCommGraph on app with pid " << *pid << " name: " << context.appDatas[*pid].name
                  << " context: " << *context_id << "\n";
    }
    return 0;
}

#endif  // #ifdef MOAB_HAVE_TEMPESTREMAP

#endif  // #ifdef MOAB_HAVE_MPI

#ifdef MOAB_HAVE_TEMPESTREMAP

#ifdef MOAB_HAVE_NETCDF

ErrCode iMOAB_LoadMappingWeightsFromFile(
    iMOAB_AppID pid_intersection, const iMOAB_String solution_weights_identifier, /* "scalar", "flux", "custom" */
    const iMOAB_String remap_weights_filename, int solution_weights_identifier_length,
    int remap_weights_filename_length )
{
    assert( remap_weights_filename_length > 0 && solution_weights_identifier_length > 0 );
    // assert( row_major_ownership != NULL );

    ErrorCode rval;
    bool row_based_partition = true;

    // Get the source and target data and pcomm objects
    appData& data_intx       = context.appDatas[*pid_intersection];
    TempestMapAppData& tdata = data_intx.tempestData;

    // Get the handle to the remapper object
    if( tdata.remapper == NULL )
    {
        // Now allocate and initialize the remapper object
#ifdef MOAB_HAVE_MPI
        ParallelComm* pco = context.pcomms[*pid_intersection];
        tdata.remapper    = new moab::TempestRemapper( context.MBI, pco );
#else
        tdata.remapper = new moab::TempestRemapper( context.MBI );
#endif
        tdata.remapper->meshValidate     = true;
        tdata.remapper->constructEdgeMap = true;

        // Do not create new filesets; Use the sets from our respective applications
        tdata.remapper->initialize( false );
        // tdata.remapper->GetMeshSet( moab::Remapper::SourceMesh )  = data_src.file_set;
        // tdata.remapper->GetMeshSet( moab::Remapper::TargetMesh )  = data_tgt.file_set;
        tdata.remapper->GetMeshSet( moab::Remapper::OverlapMesh ) = data_intx.file_set;
    }

    // Setup loading of weights onto TempestOnlineMap
    // Set the context for the remapping weights computation
    tdata.weightMaps[std::string( solution_weights_identifier )] = new moab::TempestOnlineMap( tdata.remapper );

    // Now allocate and initialize the remapper object
    moab::TempestOnlineMap* weightMap = tdata.weightMaps[std::string( solution_weights_identifier )];
    assert( weightMap != NULL );

    std::vector< int > tmp_owned_ids;  // this will do a trivial row distribution
    rval = weightMap->ReadParallelMap( remap_weights_filename, tmp_owned_ids, row_based_partition );CHKERRVAL( rval );

    return 0;
}

#ifdef MOAB_HAVE_MPI
ErrCode iMOAB_MigrateMapMeshFortran( iMOAB_AppID pid1, iMOAB_AppID pid2, iMOAB_AppID pid3,
        int* join, int* group1, int* group2, int* type, int* comp1, int* comp2, int* direction )
{
    MPI_Comm jcomm = MPI_Comm_f2c( (MPI_Fint) *join );
    MPI_Group gr1 = MPI_Group_f2c( (MPI_Fint) *group1 );
    MPI_Group gr2 = MPI_Group_f2c( (MPI_Fint) *group2 );
    return iMOAB_MigrateMapMesh(pid1, pid2, pid3, &jcomm, &gr1, &gr2, type, comp1, comp2, direction); 
}
ErrCode iMOAB_MigrateMapMesh( iMOAB_AppID pid1, iMOAB_AppID pid2, iMOAB_AppID pid3, MPI_Comm* join, MPI_Group* group1,
                              MPI_Group* group2, int* type, int* comp1, int* comp2, int* direction )
{
    ErrorCode rval = MB_SUCCESS;
    int localRank = 0, numProcs = 1;
    MPI_Comm_rank( *join, &localRank );
    MPI_Comm_size( *join, &numProcs );
    // instantiate the par comm graph
    // ParCommGraph::ParCommGraph(MPI_Comm joincomm, MPI_Group group1, MPI_Group group2, int coid1,
    // int coid2)
    // here we need to look at direction
    // this direction is good for atm source -> ocn target coupler example

    ParCommGraph* cgraph = NULL;
    if( *pid1 >= 0 ) cgraph = new ParCommGraph( *join, *group1, *group2, *comp1, *comp2 );
    ParCommGraph* cgraph_rev = NULL;
    if( *pid3 >= 0 ) cgraph_rev = new ParCommGraph( *join, *group2, *group1, *comp2, *comp1 );
    // we should search if we have another pcomm with the same comp ids in the list already
    // sort of check existing comm graphs in the map context.appDatas[*pid].pgraph
    if( *pid1 >= 0 ) context.appDatas[*pid1].pgraph[*comp2] = cgraph;      // the context will be the other comp
    if( *pid3 >= 0 ) context.appDatas[*pid3].pgraph[*comp1] = cgraph_rev;  // from 2 to 1
    // each model has a list of global ids that will need to be sent by gs to rendezvous the other
    // model on the joint comm
    TupleList TLcomp1;
    TLcomp1.initialize( 2, 0, 0, 0, 0 );  // to proc, marker
    TupleList TLcomp2;
    TLcomp2.initialize( 2, 0, 0, 0, 0 );  // to proc, marker
    // will push_back a new tuple, if needed

    TLcomp1.enableWriteAccess();

    // tags of interest are either GLOBAL_DOFS or GLOBAL_ID
    Tag gdsTag;

    // find the values on first cell
    int lenTagType1 = 1;
    if( *type == 1 )
    {
        rval = context.MBI->tag_get_handle( "GLOBAL_DOFS", gdsTag );CHKERRVAL( rval );
        rval = context.MBI->tag_get_length( gdsTag, lenTagType1 );CHKERRVAL( rval );  // usually it is 16
    }
    Tag tagType2 = context.MBI->globalId_tag();
    ;

    std::vector< int > valuesComp1;

    // populate first tuple
    Range ents_of_interest;  // will be filled with entities on pid1, that need to be distributed,
                             // rearranged in split_ranges map
    if( *pid1 >= 0 )
    {
        appData& data1     = context.appDatas[*pid1];
        EntityHandle fset1 = data1.file_set;

        if( *type == 1 )
        {
            assert( gdsTag );
            rval = context.MBI->get_entities_by_type( fset1, MBQUAD, ents_of_interest );CHKERRVAL( rval );
            valuesComp1.resize( ents_of_interest.size() * lenTagType1 );
            rval = context.MBI->tag_get_data( gdsTag, ents_of_interest, &valuesComp1[0] );CHKERRVAL( rval );
        }
        else if( *type == 2 )
        {
            rval = context.MBI->get_entities_by_type( fset1, MBVERTEX, ents_of_interest );CHKERRVAL( rval );
            valuesComp1.resize( ents_of_interest.size() );
            rval = context.MBI->tag_get_data( tagType2, ents_of_interest, &valuesComp1[0] );CHKERRVAL( rval );  // just global ids
        }
        else if( *type == 3 )  // for FV meshes, just get the global id of cell
        {
            rval = context.MBI->get_entities_by_dimension( fset1, 2, ents_of_interest );CHKERRVAL( rval );
            valuesComp1.resize( ents_of_interest.size() );
            rval = context.MBI->tag_get_data( tagType2, ents_of_interest, &valuesComp1[0] );CHKERRVAL( rval );  // just global ids
        }
        else
        {
            CHKERRVAL( MB_FAILURE );  // we know only type 1 or 2 or 3
        }
        // now fill the tuple list with info and markers
        // because we will send only the ids, order and compress the list
        std::set< int > uniq( valuesComp1.begin(), valuesComp1.end() );
        TLcomp1.resize( uniq.size() );
        for( std::set< int >::iterator sit = uniq.begin(); sit != uniq.end(); sit++ )
        {
            // to proc, marker, element local index, index in el
            int marker               = *sit;
            int to_proc              = marker % numProcs;
            int n                    = TLcomp1.get_n();
            TLcomp1.vi_wr[2 * n]     = to_proc;  // send to processor
            TLcomp1.vi_wr[2 * n + 1] = marker;
            TLcomp1.inc_n();
        }
    }

    ProcConfig pc( *join );  // proc config does the crystal router
    pc.crystal_router()->gs_transfer( 1, TLcomp1,
                                      0 );  // communication towards joint tasks, with markers
    // sort by value (key 1)
#ifdef VERBOSE
    std::stringstream ff1;
    ff1 << "TLcomp1_" << localRank << ".txt";
    TLcomp1.print_to_file( ff1.str().c_str() );  // it will append!
#endif
    moab::TupleList::buffer sort_buffer;
    sort_buffer.buffer_init( TLcomp1.get_n() );
    TLcomp1.sort( 1, &sort_buffer );
    sort_buffer.reset();
#ifdef VERBOSE
    // after sorting
    TLcomp1.print_to_file( ff1.str().c_str() );  // it will append!
#endif
    // do the same, for the other component, number2, with type2
    // start copy
    TLcomp2.enableWriteAccess();

    moab::TempestOnlineMap* weightMap = NULL;  // declare it outside, but it will make sense only for *pid >= 0
    // we know that :) (or *pid2 >= 0, it means we are on the coupler PEs, read map exists, and coupler procs exist)
    // populate second tuple with ids  from read map: we need row_gdofmap and col_gdofmap
    std::vector< int > valuesComp2;
    if( *pid2 >= 0 )  // we are now on coupler, map side
    {
        appData& data2           = context.appDatas[*pid2];
        TempestMapAppData& tdata = data2.tempestData;
        // should be only one map, read from file
        assert( tdata.weightMaps.size() == 1 );
        // maybe we need to check it is the map we expect
        weightMap = tdata.weightMaps.begin()->second;
        // std::vector<int> ids_of_interest;
        // do a deep copy of the ids of interest: row ids or col ids, target or source direction
        if( *direction == 1 )
        {
            // we are interested in col ids, source
            // new method from moab::TempestOnlineMap
            rval = weightMap->fill_col_ids( valuesComp2 );CHKERRVAL( rval );
        }
        else if( *direction == 2 )
        {
            // we are interested in row ids, for target ids
            rval = weightMap->fill_row_ids( valuesComp2 );CHKERRVAL( rval );
        }
        //

        // now fill the tuple list with info and markers
        std::set< int > uniq( valuesComp2.begin(), valuesComp2.end() );
        TLcomp2.resize( uniq.size() );
        for( std::set< int >::iterator sit = uniq.begin(); sit != uniq.end(); sit++ )
        {
            // to proc, marker, element local index, index in el
            int marker               = *sit;
            int to_proc              = marker % numProcs;
            int n                    = TLcomp2.get_n();
            TLcomp2.vi_wr[2 * n]     = to_proc;  // send to processor
            TLcomp2.vi_wr[2 * n + 1] = marker;
            TLcomp2.inc_n();
        }
    }
    pc.crystal_router()->gs_transfer( 1, TLcomp2,
                                      0 );  // communication towards joint tasks, with markers
    // sort by value (key 1)
    // in the rendez-vous approach, markers meet at meet point (rendez-vous) processor marker % nprocs
#ifdef VERBOSE
    std::stringstream ff2;
    ff2 << "TLcomp2_" << localRank << ".txt";
    TLcomp2.print_to_file( ff2.str().c_str() );
#endif
    sort_buffer.buffer_reserve( TLcomp2.get_n() );
    TLcomp2.sort( 1, &sort_buffer );
    sort_buffer.reset();
    // end copy
#ifdef VERBOSE
    TLcomp2.print_to_file( ff2.str().c_str() );
#endif
    // need to send back the info, from the rendezvous point, for each of the values
    /* so go over each value, on local process in joint communicator,

    now have to send back the info needed for communication;
     loop in in sync over both TLComp1 and TLComp2, in local process;
      So, build new tuple lists, to send synchronous communication
      populate them at the same time, based on marker, that is indexed
    */

    TupleList TLBackToComp1;
    TLBackToComp1.initialize( 3, 0, 0, 0, 0 );  // to proc, marker, from proc on comp2,
    TLBackToComp1.enableWriteAccess();

    TupleList TLBackToComp2;
    TLBackToComp2.initialize( 3, 0, 0, 0, 0 );  // to proc, marker,  from proc,
    TLBackToComp2.enableWriteAccess();

    int n1 = TLcomp1.get_n();
    int n2 = TLcomp2.get_n();

    int indexInTLComp1 = 0;
    int indexInTLComp2 = 0;  // advance both, according to the marker
    if( n1 > 0 && n2 > 0 )
    {

        while( indexInTLComp1 < n1 && indexInTLComp2 < n2 )  // if any is over, we are done
        {
            int currentValue1 = TLcomp1.vi_rd[2 * indexInTLComp1 + 1];
            int currentValue2 = TLcomp2.vi_rd[2 * indexInTLComp2 + 1];
            if( currentValue1 < currentValue2 )
            {
                // we have a big problem; basically, we are saying that
                // dof currentValue is on one model and not on the other
                // std::cout << " currentValue1:" << currentValue1 << " missing in comp2" << "\n";
                indexInTLComp1++;
                continue;
            }
            if( currentValue1 > currentValue2 )
            {
                // std::cout << " currentValue2:" << currentValue2 << " missing in comp1" << "\n";
                indexInTLComp2++;
                continue;
            }
            int size1 = 1;
            int size2 = 1;
            while( indexInTLComp1 + size1 < n1 && currentValue1 == TLcomp1.vi_rd[2 * ( indexInTLComp1 + size1 ) + 1] )
                size1++;
            while( indexInTLComp2 + size2 < n2 && currentValue2 == TLcomp2.vi_rd[2 * ( indexInTLComp2 + size2 ) + 1] )
                size2++;
            // must be found in both lists, find the start and end indices
            for( int i1 = 0; i1 < size1; i1++ )
            {
                for( int i2 = 0; i2 < size2; i2++ )
                {
                    // send the info back to components
                    int n = TLBackToComp1.get_n();
                    TLBackToComp1.reserve();
                    TLBackToComp1.vi_wr[3 * n] =
                        TLcomp1.vi_rd[2 * ( indexInTLComp1 + i1 )];  // send back to the proc marker
                                                                     // came from, info from comp2
                    TLBackToComp1.vi_wr[3 * n + 1] = currentValue1;  // initial value (resend?)
                    TLBackToComp1.vi_wr[3 * n + 2] = TLcomp2.vi_rd[2 * ( indexInTLComp2 + i2 )];  // from proc on comp2
                    n                              = TLBackToComp2.get_n();
                    TLBackToComp2.reserve();
                    TLBackToComp2.vi_wr[3 * n] =
                        TLcomp2.vi_rd[2 * ( indexInTLComp2 + i2 )];  // send back info to original
                    TLBackToComp2.vi_wr[3 * n + 1] = currentValue1;  // initial value (resend?)
                    TLBackToComp2.vi_wr[3 * n + 2] = TLcomp1.vi_rd[2 * ( indexInTLComp1 + i1 )];  // from proc on comp1
                    // what if there are repeated markers in TLcomp2? increase just index2
                }
            }
            indexInTLComp1 += size1;
            indexInTLComp2 += size2;
        }
    }
    pc.crystal_router()->gs_transfer( 1, TLBackToComp1, 0 );  // communication towards original tasks, with info about
    pc.crystal_router()->gs_transfer( 1, TLBackToComp2, 0 );

    TupleList TLv;  // vertices
    TupleList TLc;  // cells if needed (not type 2)

    if( *pid1 >= 0 )
    {
        // we are on original comp 1 tasks
        // before ordering
        // now for each value in TLBackToComp1.vi_rd[3*i+1], on current proc, we know the
        // processors it communicates with
#ifdef VERBOSE
        std::stringstream f1;
        f1 << "TLBack1_" << localRank << ".txt";
        TLBackToComp1.print_to_file( f1.str().c_str() );
#endif
        // so we are now on pid1, we know now each marker were it has to go
        // add a new method to ParCommGraph, to set up the split_ranges and involved_IDs_map
        rval = cgraph->set_split_ranges( *comp1, TLBackToComp1, valuesComp1, lenTagType1, ents_of_interest, *type );CHKERRVAL( rval );
        // we can just send vertices and elements, with crystal routers;
        // on the receiving end, make sure they are not duplicated, by looking at the global id
        // if *type is 1, also send global_dofs tag in the element tuple
        rval = cgraph->form_tuples_to_migrate_mesh( context.MBI, TLv, TLc, *type, lenTagType1 );CHKERRVAL( rval );
    }
    pc.crystal_router()->gs_transfer( 1, TLv, 0 );  // communication towards coupler tasks, with mesh vertices
    if( *type != 2 ) pc.crystal_router()->gs_transfer( 1, TLc, 0 );  // those are cells

    if( *pid3 >= 0 )  // will receive the mesh, on coupler pes!
    {
        appData& data3     = context.appDatas[*pid3];
        EntityHandle fset3 = data3.file_set;
        Range primary_ents3;                 // vertices for type 2, cells of dim 2 for type 1 or 3
        std::vector< int > values_entities;  // will be the size of primary_ents3 * lenTagType1
        rval = cgraph_rev->form_mesh_from_tuples( context.MBI, TLv, TLc, *type, lenTagType1, fset3, primary_ents3,
                                                  values_entities );CHKERRVAL( rval );
        iMOAB_UpdateMeshInfo( pid3 );
        int ndofPerEl = 1;
        if( 1 == *type ) ndofPerEl = (int)( sqrt( lenTagType1 ) );
        // because we are on the coupler, we know that the read map pid2 exists
        assert( *pid2 >= 0 );
        appData& dataIntx        = context.appDatas[*pid2];
        TempestMapAppData& tdata = dataIntx.tempestData;

        // if we are on source coverage, direction 1, we can set covering mesh, covering cells
        if( 1 == *direction )
        {
            tdata.pid_src = pid3;
            tdata.remapper->SetMeshSet( Remapper::CoveringMesh, fset3, primary_ents3 );
            weightMap->SetSourceNDofsPerElement( ndofPerEl );
            weightMap->set_col_dc_dofs( values_entities );  // will set col_dtoc_dofmap
        }
        // if we are on target, we can set the target cells
        else
        {
            tdata.pid_dest = pid3;
            tdata.remapper->SetMeshSet( Remapper::TargetMesh, fset3, primary_ents3 );
            weightMap->SetDestinationNDofsPerElement( ndofPerEl );
            weightMap->set_row_dc_dofs( values_entities );  // will set row_dtoc_dofmap
        }
    }

    return 0;
}
#endif // #ifdef MOAB_HAVE_MPI
ErrCode iMOAB_WriteMappingWeightsToFile(
    iMOAB_AppID pid_intersection, const iMOAB_String solution_weights_identifier, /* "scalar", "flux", "custom" */
    const iMOAB_String remap_weights_filename, int solution_weights_identifier_length,
    int remap_weights_filename_length )
{
    assert( remap_weights_filename_length > 0 && solution_weights_identifier_length > 0 );

    ErrorCode rval;

    // Get the source and target data and pcomm objects
    appData& data_intx       = context.appDatas[*pid_intersection];
    TempestMapAppData& tdata = data_intx.tempestData;

    // Get the handle to the remapper object
    assert( tdata.remapper != NULL );

    // Now get online weights object and ensure it is valid
    moab::TempestOnlineMap* weightMap = tdata.weightMaps[std::string( solution_weights_identifier )];
    assert( weightMap != NULL );

    std::string filename = std::string( remap_weights_filename );

    // Write the map file to disk in parallel using either HDF5 or SCRIP interface
    rval = weightMap->WriteParallelMap( filename );CHKERRVAL( rval );

    return 0;
}

#endif  // #ifdef MOAB_HAVE_NETCDF

#define USE_API
static ErrCode ComputeSphereRadius( iMOAB_AppID pid, double* radius )
{
    ErrorCode rval;
    moab::CartVect pos;

    Range& verts                   = context.appDatas[*pid].all_verts;
    moab::EntityHandle firstVertex = ( verts[0] );

    // coordinate data
    rval = context.MBI->get_coords( &( firstVertex ), 1, (double*)&( pos[0] ) );CHKERRVAL( rval );

    // compute the distance from origin
    // TODO: we could do this in a loop to verify if the pid represents a spherical mesh
    *radius = pos.length();
    return 0;
}

ErrCode iMOAB_ComputeMeshIntersectionOnSphere( iMOAB_AppID pid_src, iMOAB_AppID pid_tgt, iMOAB_AppID pid_intx )
{
    ErrorCode rval;
    ErrCode ierr;
    bool validate = true;

    double radius_source = 1.0;
    double radius_target = 1.0;
    const double epsrel  = 1e-15;
    const double boxeps  = 1.e-6;

    // Get the source and target data and pcomm objects
    appData& data_src  = context.appDatas[*pid_src];
    appData& data_tgt  = context.appDatas[*pid_tgt];
    appData& data_intx = context.appDatas[*pid_intx];
#ifdef MOAB_HAVE_MPI
    ParallelComm* pco_intx = context.pcomms[*pid_intx];
#endif

    // Mesh intersection has already been computed; Return early.
    TempestMapAppData& tdata = data_intx.tempestData;
    if( tdata.remapper != NULL ) return 0;

    bool is_parallel = false, is_root = true;
    int rank = 0;
#ifdef MOAB_HAVE_MPI
    if( pco_intx )
    {
        rank        = pco_intx->rank();
        is_parallel = ( pco_intx->size() > 1 );
        is_root     = ( rank == 0 );
        rval        = pco_intx->check_all_shared_handles();CHKERRVAL( rval );
    }
#endif

    moab::DebugOutput outputFormatter( std::cout, rank, 0 );
    outputFormatter.set_prefix( "[iMOAB_ComputeMeshIntersectionOnSphere]: " );

    ierr = iMOAB_UpdateMeshInfo( pid_src );
    CHKIERRVAL( ierr );
    ierr = iMOAB_UpdateMeshInfo( pid_tgt );
    CHKIERRVAL( ierr );

    // Rescale the radius of both to compute the intersection
    ComputeSphereRadius( pid_src, &radius_source );
    ComputeSphereRadius( pid_tgt, &radius_target );
#ifdef VERBOSE
    if( is_root )
        outputFormatter.printf( 0, "Radius of spheres: source = %12.14f, and target = %12.14f\n", radius_source,
                                radius_target );
#endif

    /* Let make sure that the radius match for source and target meshes. If not, rescale now and
     * unscale later. */
    bool radii_scaled  = false;
    bool defaultradius = 1.0;
    if( fabs( radius_source - radius_target ) > 1e-10 )
    { /* the radii are different */
        radii_scaled = true;
        rval         = IntxUtils::ScaleToRadius( context.MBI, data_src.file_set, defaultradius );CHKERRVAL( rval );
        rval = IntxUtils::ScaleToRadius( context.MBI, data_tgt.file_set, defaultradius );CHKERRVAL( rval );
    }

    // Default area_method = lHuiller; Options: Girard, GaussQuadrature (if TR is available)
#ifdef MOAB_HAVE_TEMPESTREMAP
    IntxAreaUtils areaAdaptor( IntxAreaUtils::GaussQuadrature );
#else
    IntxAreaUtils areaAdaptor( IntxAreaUtils::lHuiller );
#endif

    // print verbosely about the problem setting
    bool use_kdtree_search = false;
    double srctgt_areas[2], srctgt_areas_glb[2];
    {
        moab::Range rintxverts, rintxelems;
        rval = context.MBI->get_entities_by_dimension( data_src.file_set, 0, rintxverts );CHKERRVAL( rval );
        rval = context.MBI->get_entities_by_dimension( data_src.file_set, data_src.dimension, rintxelems );CHKERRVAL( rval );
        rval = IntxUtils::fix_degenerate_quads( context.MBI, data_src.file_set );CHKERRVAL( rval );
        rval = areaAdaptor.positive_orientation( context.MBI, data_src.file_set, defaultradius /*radius_source*/ );CHKERRVAL( rval );
        srctgt_areas[0] = areaAdaptor.area_on_sphere( context.MBI, data_src.file_set, defaultradius /*radius_source*/ );
#ifdef VERBOSE
        if( is_root )
            outputFormatter.printf( 0, "The red set contains %d vertices and %d elements \n", rintxverts.size(),
                                    rintxelems.size() );
#endif

        moab::Range bintxverts, bintxelems;
        rval = context.MBI->get_entities_by_dimension( data_tgt.file_set, 0, bintxverts );CHKERRVAL( rval );
        rval = context.MBI->get_entities_by_dimension( data_tgt.file_set, data_tgt.dimension, bintxelems );CHKERRVAL( rval );
        rval = IntxUtils::fix_degenerate_quads( context.MBI, data_tgt.file_set );CHKERRVAL( rval );
        rval = areaAdaptor.positive_orientation( context.MBI, data_tgt.file_set, defaultradius /*radius_target*/ );CHKERRVAL( rval );
        srctgt_areas[1] = areaAdaptor.area_on_sphere( context.MBI, data_tgt.file_set, defaultradius /*radius_target*/ );
#ifdef VERBOSE
        if( is_root )
            outputFormatter.printf( 0, "The blue set contains %d vertices and %d elements \n", bintxverts.size(),
                                    bintxelems.size() );
#endif
#ifdef MOAB_HAVE_MPI
        MPI_Allreduce( &srctgt_areas[0], &srctgt_areas_glb[0], 2, MPI_DOUBLE, MPI_SUM, pco_intx->comm() );
#else
        srctgt_areas_glb[0] = srctgt_areas[0];
        srctgt_areas_glb[1] = srctgt_areas[1];
#endif
        use_kdtree_search = ( srctgt_areas_glb[0] < srctgt_areas_glb[1] );
    }

    data_intx.dimension = data_tgt.dimension;
    // set the context for the source and destination applications
    tdata.pid_src  = pid_src;
    tdata.pid_dest = pid_tgt;

    // Now allocate and initialize the remapper object
#ifdef MOAB_HAVE_MPI
    tdata.remapper = new moab::TempestRemapper( context.MBI, pco_intx );
#else
    tdata.remapper = new moab::TempestRemapper( context.MBI );
#endif
    tdata.remapper->meshValidate     = true;
    tdata.remapper->constructEdgeMap = true;

    // Do not create new filesets; Use the sets from our respective applications
    tdata.remapper->initialize( false );
    tdata.remapper->GetMeshSet( moab::Remapper::SourceMesh )  = data_src.file_set;
    tdata.remapper->GetMeshSet( moab::Remapper::TargetMesh )  = data_tgt.file_set;
    tdata.remapper->GetMeshSet( moab::Remapper::OverlapMesh ) = data_intx.file_set;

    rval = tdata.remapper->ConvertMeshToTempest( moab::Remapper::SourceMesh );CHKERRVAL( rval );
    rval = tdata.remapper->ConvertMeshToTempest( moab::Remapper::TargetMesh );CHKERRVAL( rval );

    // First, compute the covering source set.
    rval = tdata.remapper->ConstructCoveringSet( epsrel, 1.0, 1.0, boxeps, false );CHKERRVAL( rval );

    // Next, compute intersections with MOAB.
    rval = tdata.remapper->ComputeOverlapMesh( use_kdtree_search, false );CHKERRVAL( rval );

    // Mapping computation done
    if( validate )
    {
        double local_area,
            global_areas[3];  // Array for Initial area, and through Method 1 and Method 2
        local_area = areaAdaptor.area_on_sphere( context.MBI, data_intx.file_set, radius_source );

        global_areas[0] = srctgt_areas_glb[0];
        global_areas[1] = srctgt_areas_glb[1];

#ifdef MOAB_HAVE_MPI
        if( is_parallel )
        {
            MPI_Reduce( &local_area, &global_areas[2], 1, MPI_DOUBLE, MPI_SUM, 0, pco_intx->comm() );
        }
        else
        {
            global_areas[2] = local_area;
        }
#else
        global_areas[2] = local_area;
#endif
        if( is_root )
        {
            outputFormatter.printf( 0,
                                    "initial area: source mesh = %12.14f, target mesh = %12.14f, "
                                    "overlap mesh = %12.14f\n",
                                    global_areas[0], global_areas[1], global_areas[2] );
            outputFormatter.printf( 0, " relative error w.r.t source = %12.14e, and target = %12.14e\n",
                                    fabs( global_areas[0] - global_areas[2] ) / global_areas[0],
                                    fabs( global_areas[1] - global_areas[2] ) / global_areas[1] );
        }
    }

    return 0;
}

ErrCode iMOAB_ComputePointDoFIntersection( iMOAB_AppID pid_src, iMOAB_AppID pid_tgt, iMOAB_AppID pid_intx )
{
    ErrorCode rval;
    ErrCode ierr;

    double radius_source = 1.0;
    double radius_target = 1.0;
    const double epsrel  = 1e-15;
    const double boxeps  = 1.e-8;

    // Get the source and target data and pcomm objects
    appData& data_src  = context.appDatas[*pid_src];
    appData& data_tgt  = context.appDatas[*pid_tgt];
    appData& data_intx = context.appDatas[*pid_intx];
#ifdef MOAB_HAVE_MPI
    ParallelComm* pco_intx = context.pcomms[*pid_intx];
#endif

    // Mesh intersection has already been computed; Return early.
    TempestMapAppData& tdata = data_intx.tempestData;
    if( tdata.remapper != NULL ) return 0;

#ifdef MOAB_HAVE_MPI
    if( pco_intx )
    {
        rval = pco_intx->check_all_shared_handles();CHKERRVAL( rval );
    }
#endif

    ierr = iMOAB_UpdateMeshInfo( pid_src );
    CHKIERRVAL( ierr );
    ierr = iMOAB_UpdateMeshInfo( pid_tgt );
    CHKIERRVAL( ierr );

    // Rescale the radius of both to compute the intersection
    ComputeSphereRadius( pid_src, &radius_source );
    ComputeSphereRadius( pid_tgt, &radius_target );

    IntxAreaUtils areaAdaptor;
    // print verbosely about the problem setting
    {
        moab::Range rintxverts, rintxelems;
        rval = context.MBI->get_entities_by_dimension( data_src.file_set, 0, rintxverts );CHKERRVAL( rval );
        rval = context.MBI->get_entities_by_dimension( data_src.file_set, 2, rintxelems );CHKERRVAL( rval );
        rval = IntxUtils::fix_degenerate_quads( context.MBI, data_src.file_set );CHKERRVAL( rval );
        rval = areaAdaptor.positive_orientation( context.MBI, data_src.file_set, radius_source );CHKERRVAL( rval );
#ifdef VERBOSE
        std::cout << "The red set contains " << rintxverts.size() << " vertices and " << rintxelems.size()
                  << " elements \n";
#endif

        moab::Range bintxverts, bintxelems;
        rval = context.MBI->get_entities_by_dimension( data_tgt.file_set, 0, bintxverts );CHKERRVAL( rval );
        rval = context.MBI->get_entities_by_dimension( data_tgt.file_set, 2, bintxelems );CHKERRVAL( rval );
        rval = IntxUtils::fix_degenerate_quads( context.MBI, data_tgt.file_set );CHKERRVAL( rval );
        rval = areaAdaptor.positive_orientation( context.MBI, data_tgt.file_set, radius_target );CHKERRVAL( rval );
#ifdef VERBOSE
        std::cout << "The blue set contains " << bintxverts.size() << " vertices and " << bintxelems.size()
                  << " elements \n";
#endif
    }

    data_intx.dimension = data_tgt.dimension;
    // set the context for the source and destination applications
    // set the context for the source and destination applications
    tdata.pid_src         = pid_src;
    tdata.pid_dest        = pid_tgt;
    data_intx.point_cloud = ( data_src.point_cloud || data_tgt.point_cloud );
    assert( data_intx.point_cloud == true );

    // Now allocate and initialize the remapper object
#ifdef MOAB_HAVE_MPI
    tdata.remapper = new moab::TempestRemapper( context.MBI, pco_intx );
#else
    tdata.remapper = new moab::TempestRemapper( context.MBI );
#endif
    tdata.remapper->meshValidate     = true;
    tdata.remapper->constructEdgeMap = true;

    // Do not create new filesets; Use the sets from our respective applications
    tdata.remapper->initialize( false );
    tdata.remapper->GetMeshSet( moab::Remapper::SourceMesh )  = data_src.file_set;
    tdata.remapper->GetMeshSet( moab::Remapper::TargetMesh )  = data_tgt.file_set;
    tdata.remapper->GetMeshSet( moab::Remapper::OverlapMesh ) = data_intx.file_set;

    /* Let make sure that the radius match for source and target meshes. If not, rescale now and
     * unscale later. */
    bool radii_scaled = false;
    if( fabs( radius_source - radius_target ) > 1e-10 )
    { /* the radii are different */
        radii_scaled = true;
        rval         = IntxUtils::ScaleToRadius( context.MBI, data_src.file_set, 1.0 );CHKERRVAL( rval );
        rval = IntxUtils::ScaleToRadius( context.MBI, data_tgt.file_set, 1.0 );CHKERRVAL( rval );
    }

    rval = tdata.remapper->ConvertMeshToTempest( moab::Remapper::SourceMesh );CHKERRVAL( rval );
    rval = tdata.remapper->ConvertMeshToTempest( moab::Remapper::TargetMesh );CHKERRVAL( rval );

    // First, compute the covering source set.
    rval = tdata.remapper->ConstructCoveringSet( epsrel, 1.0, 1.0, boxeps, false );CHKERRVAL( rval );

#ifdef MOAB_HAVE_MPI
    /* VSM: This context should be set on the data_src but it would overwrite the source
       covering set context in case it is coupled to another APP as well.
       This needs a redesign. */
    // data_intx.covering_set = tdata.remapper->GetCoveringSet();
    // data_src.covering_set = tdata.remapper->GetCoveringSet();
#endif

    // Now let us re-convert the MOAB mesh back to Tempest representation
    rval = tdata.remapper->ComputeGlobalLocalMaps();MB_CHK_ERR( rval );

    return 0;
}

ErrCode iMOAB_ComputeScalarProjectionWeights(
    iMOAB_AppID pid_intx, const iMOAB_String solution_weights_identifier, /* "scalar", "flux", "custom" */
    const iMOAB_String disc_method_source, int* disc_order_source, const iMOAB_String disc_method_target,
    int* disc_order_target, int* fNoBubble, int* fMonotoneTypeID, int* fVolumetric, int* fNoConservation,
    int* fValidate, const iMOAB_String source_solution_tag_dof_name, const iMOAB_String target_solution_tag_dof_name,
    int solution_weights_identifier_length, int disc_method_source_length, int disc_method_target_length,
    int source_solution_tag_dof_name_length, int target_solution_tag_dof_name_length )
{
    moab::ErrorCode rval;

    assert( disc_order_source && disc_order_target && *disc_order_source > 0 && *disc_order_target > 0 );
    assert( disc_method_source_length > 0 && disc_method_target_length > 0 );
    assert( solution_weights_identifier_length > 0 && source_solution_tag_dof_name_length > 0 &&
            target_solution_tag_dof_name_length > 0 );

    // Get the source and target data and pcomm objects
    appData& data_intx       = context.appDatas[*pid_intx];
    TempestMapAppData& tdata = data_intx.tempestData;

    // Setup computation of weights
    // Set the context for the remapping weights computation
    tdata.weightMaps[std::string( solution_weights_identifier )] = new moab::TempestOnlineMap( tdata.remapper );

    // Call to generate the remap weights with the tempest meshes
    moab::TempestOnlineMap* weightMap = tdata.weightMaps[std::string( solution_weights_identifier )];
    assert( weightMap != NULL );

    // Now let us compute the local-global mapping and store it in the context
    // We need this mapping when computing matvec products and to do reductions in parallel
    // Additionally, the call below will also compute weights with TempestRemap
    rval = weightMap->GenerateRemappingWeights(
        std::string( disc_method_source ),                   // std::string strInputType
        std::string( disc_method_target ),                   // std::string strOutputType
        ( *disc_order_source ), ( *disc_order_target ),      // const int nPin, const int nPout
        ( fNoBubble ? *fNoBubble : false ),                  // bool fNoBubble=false
        ( fMonotoneTypeID ? *fMonotoneTypeID : 0 ),          // int fMonotoneTypeID=0
        ( fVolumetric ? *fVolumetric > 0 : false ),          // bool fVolumetric=false
        ( fNoConservation ? *fNoConservation > 0 : false ),  // bool fNoConservation=false
        ( fValidate ? *fValidate : false ),                  // bool fNoCheck=false
        source_solution_tag_dof_name,                        // const std::string srcDofTagName = "GLOBAL_ID"
        target_solution_tag_dof_name,                        // const std::string tgtDofTagName = "GLOBAL_ID"
        false,                                               // bool fInputConcave = false
        false                                                // bool fOutputConcave = false
    );CHKERRVAL( rval );

    return 0;
}

ErrCode iMOAB_ApplyScalarProjectionWeights(
    iMOAB_AppID pid_intersection, const iMOAB_String solution_weights_identifier, /* "scalar", "flux", "custom" */
    const iMOAB_String source_solution_tag_name, const iMOAB_String target_solution_tag_name,
    int solution_weights_identifier_length, int source_solution_tag_name_length, int target_solution_tag_name_length )
{
    moab::ErrorCode rval;

    assert( solution_weights_identifier_length > 0 && source_solution_tag_name_length > 0 &&
            target_solution_tag_name_length > 0 );

    // Get the source and target data and pcomm objects
    appData& data_intx       = context.appDatas[*pid_intersection];
    TempestMapAppData& tdata = data_intx.tempestData;

    // Now allocate and initialize the remapper object
    moab::TempestRemapper* remapper   = tdata.remapper;
    moab::TempestOnlineMap* weightMap = tdata.weightMaps[std::string( solution_weights_identifier )];

    // we assume that there are separators ";" between the tag names
    std::vector< std::string > srcNames;
    std::vector< std::string > tgtNames;
    std::vector< Tag > srcTagHandles;
    std::vector< Tag > tgtTagHandles;
    std::string separator( ";" );
    std::string src_name( source_solution_tag_name );
    std::string tgt_name( target_solution_tag_name );
    split_tag_names( src_name, separator, srcNames );
    split_tag_names( tgt_name, separator, tgtNames );
    if( srcNames.size() != tgtNames.size() )
    {
        std::cout << " error in parsing source and target tag names. \n";
        return 1;
    }

    for( size_t i = 0; i < srcNames.size(); i++ )
    {
        Tag tagHandle;
        rval = context.MBI->tag_get_handle( srcNames[i].c_str(), tagHandle );
        if( MB_SUCCESS != rval || NULL == tagHandle )
        {
            return 1;
        }
        srcTagHandles.push_back( tagHandle );
        rval = context.MBI->tag_get_handle( tgtNames[i].c_str(), tagHandle );
        if( MB_SUCCESS != rval || NULL == tagHandle )
        {
            return 1;
        }
        tgtTagHandles.push_back( tagHandle );
    }

    std::vector< double > solSTagVals;
    std::vector< double > solTTagVals;

    moab::Range sents, tents;
    if( data_intx.point_cloud )
    {
        appData& data_src = context.appDatas[*tdata.pid_src];
        appData& data_tgt = context.appDatas[*tdata.pid_dest];
        if( data_src.point_cloud )
        {
            moab::Range& covSrcEnts = remapper->GetMeshVertices( moab::Remapper::CoveringMesh );
            solSTagVals.resize( covSrcEnts.size(), -1.0 );
            sents = covSrcEnts;
        }
        else
        {
            moab::Range& covSrcEnts = remapper->GetMeshEntities( moab::Remapper::CoveringMesh );
            solSTagVals.resize( covSrcEnts.size() * weightMap->GetSourceNDofsPerElement() *
                                    weightMap->GetSourceNDofsPerElement(),
                                -1.0 );
            sents = covSrcEnts;
        }
        if( data_tgt.point_cloud )
        {
            moab::Range& tgtEnts = remapper->GetMeshVertices( moab::Remapper::TargetMesh );
            solTTagVals.resize( tgtEnts.size(), -1.0 );
            tents = tgtEnts;
        }
        else
        {
            moab::Range& tgtEnts = remapper->GetMeshEntities( moab::Remapper::TargetMesh );
            solTTagVals.resize( tgtEnts.size() * weightMap->GetDestinationNDofsPerElement() *
                                    weightMap->GetDestinationNDofsPerElement(),
                                -1.0 );
            tents = tgtEnts;
        }
    }
    else
    {
        moab::Range& covSrcEnts = remapper->GetMeshEntities( moab::Remapper::CoveringMesh );
        moab::Range& tgtEnts    = remapper->GetMeshEntities( moab::Remapper::TargetMesh );
        solSTagVals.resize(
            covSrcEnts.size() * weightMap->GetSourceNDofsPerElement() * weightMap->GetSourceNDofsPerElement(), -1.0 );
        solTTagVals.resize( tgtEnts.size() * weightMap->GetDestinationNDofsPerElement() *
                                weightMap->GetDestinationNDofsPerElement(),
                            -1.0 );

        sents = covSrcEnts;
        tents = tgtEnts;
    }

    for( size_t i = 0; i < srcTagHandles.size(); i++ )
    {
        // The tag data is np*np*n_el_src
        Tag ssolnTag = srcTagHandles[i];
        Tag tsolnTag = tgtTagHandles[i];
        rval         = context.MBI->tag_get_data( ssolnTag, sents, &solSTagVals[0] );CHKERRVAL( rval );

        // Compute the application of weights on the suorce solution data and store it in the
        // destination solution vector data Optionally, can also perform the transpose application
        // of the weight matrix. Set the 3rd argument to true if this is needed
        rval = weightMap->ApplyWeights( solSTagVals, solTTagVals, false );CHKERRVAL( rval );

        // The tag data is np*np*n_el_dest
        rval = context.MBI->tag_set_data( tsolnTag, tents, &solTTagVals[0] );CHKERRVAL( rval );
    }

// #define VERBOSE
#ifdef VERBOSE
    ParallelComm* pco_intx = context.pcomms[*pid_intersection];

    int ivar = 0;
    {
        Tag ssolnTag = srcTagHandles[ivar];
        std::stringstream sstr;
        sstr << "covsrcTagData_" << *pid_intersection << "_" << ivar << "_" << pco_intx->rank() << ".txt";
        std::ofstream output_file( sstr.str().c_str() );
        for( unsigned i = 0; i < sents.size(); ++i )
        {
            EntityHandle elem = sents[i];
            std::vector< double > locsolSTagVals( 16 );
            rval = context.MBI->tag_get_data( ssolnTag, &elem, 1, &locsolSTagVals[0] );CHKERRVAL( rval );
            output_file << "\n" << remapper->GetGlobalID( Remapper::CoveringMesh, i ) << "-- \n\t";
            for( unsigned j = 0; j < 16; ++j )
                output_file << locsolSTagVals[j] << " ";
        }
        output_file.flush();  // required here
        output_file.close();
    }
    {
        std::stringstream sstr;
        sstr << "outputSrcDest_" << *pid_intersection << "_" << ivar << "_" << pco_intx->rank() << ".h5m";
        EntityHandle sets[2] = { context.appDatas[*tdata.pid_src].file_set,
                                 context.appDatas[*tdata.pid_dest].file_set };
        rval                 = context.MBI->write_file( sstr.str().c_str(), NULL, "", sets, 2 );MB_CHK_ERR( rval );
    }
    {
        std::stringstream sstr;
        sstr << "outputCovSrcDest_" << *pid_intersection << "_" << ivar << "_" << pco_intx->rank() << ".h5m";
        // EntityHandle sets[2] = {data_intx.file_set, data_intx.covering_set};
        EntityHandle covering_set = remapper->GetCoveringSet();
        EntityHandle sets[2]      = { covering_set, context.appDatas[*tdata.pid_dest].file_set };
        rval                      = context.MBI->write_file( sstr.str().c_str(), NULL, "", sets, 2 );MB_CHK_ERR( rval );
    }
    {
        std::stringstream sstr;
        sstr << "covMesh_" << *pid_intersection << "_" << pco_intx->rank() << ".vtk";
        // EntityHandle sets[2] = {data_intx.file_set, data_intx.covering_set};
        EntityHandle covering_set = remapper->GetCoveringSet();
        rval                      = context.MBI->write_file( sstr.str().c_str(), NULL, "", &covering_set, 1 );MB_CHK_ERR( rval );
    }
    {
        std::stringstream sstr;
        sstr << "tgtMesh_" << *pid_intersection << "_" << pco_intx->rank() << ".vtk";
        // EntityHandle sets[2] = {data_intx.file_set, data_intx.covering_set};
        EntityHandle target_set = remapper->GetMeshSet( Remapper::TargetMesh );
        rval                    = context.MBI->write_file( sstr.str().c_str(), NULL, "", &target_set, 1 );MB_CHK_ERR( rval );
    }
    {
        std::stringstream sstr;
        sstr << "colvector_" << *pid_intersection << "_" << ivar << "_" << pco_intx->rank() << ".txt";
        std::ofstream output_file( sstr.str().c_str() );
        for( unsigned i = 0; i < solSTagVals.size(); ++i )
            output_file << i << " " << weightMap->col_dofmap[i] << " " << weightMap->col_gdofmap[i] << " "
                        << solSTagVals[i] << "\n";
        output_file.flush();  // required here
        output_file.close();
    }
#endif
    // #undef VERBOSE

    return 0;
}

#endif  // MOAB_HAVE_TEMPESTREMAP

#ifdef __cplusplus
}
#endif
