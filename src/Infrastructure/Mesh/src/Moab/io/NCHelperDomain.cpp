#include "NCHelperDomain.hpp"
#include "moab/FileOptions.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/IntxMesh/IntxUtils.hpp"
#ifdef MOAB_HAVE_MPI
#include "moab/ParallelMergeMesh.hpp"
#endif
#include <cmath>
#include <sstream>

namespace moab
{

bool NCHelperDomain::can_read_file( ReadNC* readNC, int fileId )
{
    std::vector< std::string >& dimNames = readNC->dimNames;

    // If dimension names "n" AND "ni" AND "nj" AND "nv" exist then it should be the Domain grid
    if( ( std::find( dimNames.begin(), dimNames.end(), std::string( "n" ) ) != dimNames.end() ) &&
        ( std::find( dimNames.begin(), dimNames.end(), std::string( "ni" ) ) != dimNames.end() ) &&
        ( std::find( dimNames.begin(), dimNames.end(), std::string( "nj" ) ) != dimNames.end() ) &&
        ( std::find( dimNames.begin(), dimNames.end(), std::string( "nv" ) ) != dimNames.end() ) )
    {
        // Make sure it is CAM grid
        std::map< std::string, ReadNC::AttData >::iterator attIt = readNC->globalAtts.find( "source" );
        if( attIt == readNC->globalAtts.end() ) return false;
        unsigned int sz = attIt->second.attLen;
        std::string att_data;
        att_data.resize( sz + 1 );
        att_data[sz] = '\000';
        int success =
            NCFUNC( get_att_text )( fileId, attIt->second.attVarId, attIt->second.attName.c_str(), &att_data[0] );
        if( success ) return false;
        /*if (att_data.find("CAM") == std::string::npos)
          return false;*/

        return true;
    }

    return false;
}

ErrorCode NCHelperDomain::init_mesh_vals()
{
    Interface*& mbImpl                                = _readNC->mbImpl;
    std::vector< std::string >& dimNames              = _readNC->dimNames;
    std::vector< int >& dimLens                       = _readNC->dimLens;
    std::map< std::string, ReadNC::VarData >& varInfo = _readNC->varInfo;
    DebugOutput& dbgOut                               = _readNC->dbgOut;
    bool& isParallel                                  = _readNC->isParallel;
    int& partMethod                                   = _readNC->partMethod;
    ScdParData& parData                               = _readNC->parData;

    ErrorCode rval;

    // Look for names of i/j dimensions
    // First i
    std::vector< std::string >::iterator vit;
    unsigned int idx;
    if( ( vit = std::find( dimNames.begin(), dimNames.end(), "ni" ) ) != dimNames.end() )
        idx = vit - dimNames.begin();
    else
    {
        MB_SET_ERR( MB_FAILURE, "Couldn't find 'ni' variable" );
    }
    iDim     = idx;
    gDims[0] = 0;
    gDims[3] = dimLens[idx];

    // Then j
    if( ( vit = std::find( dimNames.begin(), dimNames.end(), "nj" ) ) != dimNames.end() )
        idx = vit - dimNames.begin();
    else
    {
        MB_SET_ERR( MB_FAILURE, "Couldn't find 'nj' variable" );
    }
    jDim     = idx;
    gDims[1] = 0;
    gDims[4] = dimLens[idx];  // Add 2 for the pole points ? not needed

    // do not use gcdims ? or use only gcdims?

    // Try a truly 2D mesh
    gDims[2] = -1;
    gDims[5] = -1;

    // Get number of vertices per cell
    if( ( vit = std::find( dimNames.begin(), dimNames.end(), "nv" ) ) != dimNames.end() )
        idx = vit - dimNames.begin();
    else
    {
        MB_SET_ERR( MB_FAILURE, "Couldn't find 'nv' dimension" );
    }
    nvDim = idx;
    nv    = dimLens[idx];

    // Parse options to get subset
    int rank = 0, procs = 1;
#ifdef MOAB_HAVE_MPI
    if( isParallel )
    {
        ParallelComm*& myPcomm = _readNC->myPcomm;
        rank                   = myPcomm->proc_config().proc_rank();
        procs                  = myPcomm->proc_config().proc_size();
    }
#endif
    if( procs > 1 )
    {
        for( int i = 0; i < 6; i++ )
            parData.gDims[i] = gDims[i];
        parData.partMethod = partMethod;
        int pdims[3];

        locallyPeriodic[0] = locallyPeriodic[1] = locallyPeriodic[2] = 0;
        rval = ScdInterface::compute_partition( procs, rank, parData, lDims, locallyPeriodic, pdims );MB_CHK_ERR( rval );
        for( int i = 0; i < 3; i++ )
            parData.pDims[i] = pdims[i];

        dbgOut.tprintf( 1, "Partition: %dx%d (out of %dx%d)\n", lDims[3] - lDims[0], lDims[4] - lDims[1],
                        gDims[3] - gDims[0], gDims[4] - gDims[1] );
        if( 0 == rank )
            dbgOut.tprintf( 1, "Contiguous chunks of size %d bytes.\n",
                            8 * ( lDims[3] - lDims[0] ) * ( lDims[4] - lDims[1] ) );
    }
    else
    {
        for( int i = 0; i < 6; i++ )
            lDims[i] = gDims[i];
        locallyPeriodic[0] = globallyPeriodic[0];
    }

    // Now get actual coordinate values for vertices and cell centers
    lCDims[0] = lDims[0];

    lCDims[3] = lDims[3];

    // For FV models, will always be non-periodic in j
    lCDims[1] = lDims[1];
    lCDims[4] = lDims[4];

#if 0
  // Resize vectors to store values later
  if (-1 != lDims[0])
    ilVals.resize(lDims[3] - lDims[0] + 1);
  if (-1 != lCDims[0])
    ilCVals.resize(lCDims[3] - lCDims[0] + 1);
  if (-1 != lDims[1])
    jlVals.resize(lDims[4] - lDims[1] + 1);
  if (-1 != lCDims[1])
    jlCVals.resize(lCDims[4] - lCDims[1] + 1);
  if (nTimeSteps > 0)
    tVals.resize(nTimeSteps);
#endif

    dbgOut.tprintf( 1, "I=%d-%d, J=%d-%d\n", lDims[0], lDims[3], lDims[1], lDims[4] );
    dbgOut.tprintf( 1, "%d elements, %d vertices\n", ( lDims[3] - lDims[0] ) * ( lDims[4] - lDims[1] ),
                    ( lDims[3] - lDims[0] ) * ( lDims[4] - lDims[1] ) * nv );

    // For each variable, determine the entity location type and number of levels
    std::map< std::string, ReadNC::VarData >::iterator mit;
    for( mit = varInfo.begin(); mit != varInfo.end(); ++mit )
    {
        ReadNC::VarData& vd = ( *mit ).second;

        // Default entLoc is ENTLOCSET
        if( std::find( vd.varDims.begin(), vd.varDims.end(), tDim ) != vd.varDims.end() )
        {
            if( ( std::find( vd.varDims.begin(), vd.varDims.end(), iCDim ) != vd.varDims.end() ) &&
                ( std::find( vd.varDims.begin(), vd.varDims.end(), jCDim ) != vd.varDims.end() ) )
                vd.entLoc = ReadNC::ENTLOCFACE;
            else if( ( std::find( vd.varDims.begin(), vd.varDims.end(), jDim ) != vd.varDims.end() ) &&
                     ( std::find( vd.varDims.begin(), vd.varDims.end(), iCDim ) != vd.varDims.end() ) )
                vd.entLoc = ReadNC::ENTLOCNSEDGE;
            else if( ( std::find( vd.varDims.begin(), vd.varDims.end(), jCDim ) != vd.varDims.end() ) &&
                     ( std::find( vd.varDims.begin(), vd.varDims.end(), iDim ) != vd.varDims.end() ) )
                vd.entLoc = ReadNC::ENTLOCEWEDGE;
        }

        // Default numLev is 0
        if( std::find( vd.varDims.begin(), vd.varDims.end(), levDim ) != vd.varDims.end() ) vd.numLev = nLevels;
    }

    std::vector< std::string > ijdimNames( 2 );
    ijdimNames[0] = "__ni";
    ijdimNames[1] = "__nj";
    std::string tag_name;
    Tag tagh;

    // __<dim_name>_LOC_MINMAX (for slon, slat, lon and lat)
    for( unsigned int i = 0; i != ijdimNames.size(); i++ )
    {
        std::vector< int > val( 2, 0 );
        if( ijdimNames[i] == "__ni" )
        {
            val[0] = lDims[0];
            val[1] = lDims[3];
        }
        else if( ijdimNames[i] == "__nj" )
        {
            val[0] = lDims[1];
            val[1] = lDims[4];
        }

        std::stringstream ss_tag_name;
        ss_tag_name << ijdimNames[i] << "_LOC_MINMAX";
        tag_name = ss_tag_name.str();
        rval     = mbImpl->tag_get_handle( tag_name.c_str(), 2, MB_TYPE_INTEGER, tagh, MB_TAG_SPARSE | MB_TAG_CREAT );MB_CHK_SET_ERR( rval, "Trouble creating conventional tag " << tag_name );
        rval = mbImpl->tag_set_data( tagh, &_fileSet, 1, &val[0] );MB_CHK_SET_ERR( rval, "Trouble setting data to conventional tag " << tag_name );
        if( MB_SUCCESS == rval ) dbgOut.tprintf( 2, "Conventional tag %s is created.\n", tag_name.c_str() );
    }

    // __<dim_name>_LOC_VALS (for slon, slat, lon and lat)
    // Assume all have the same data type as lon (expected type is float or double)
    switch( varInfo["xc"].varDataType )
    {
        case NC_FLOAT:
        case NC_DOUBLE:
            break;
        default:
            MB_SET_ERR( MB_FAILURE, "Unexpected variable data type for 'lon'" );
    }

    // do not need conventional tags
    Tag convTagsCreated = 0;
    int def_val         = 0;
    rval                = mbImpl->tag_get_handle( "__CONV_TAGS_CREATED", 1, MB_TYPE_INTEGER, convTagsCreated,
                                   MB_TAG_SPARSE | MB_TAG_CREAT, &def_val );MB_CHK_SET_ERR( rval, "Trouble getting _CONV_TAGS_CREATED tag" );
    int create_conv_tags_flag = 1;
    rval                      = mbImpl->tag_set_data( convTagsCreated, &_fileSet, 1, &create_conv_tags_flag );MB_CHK_SET_ERR( rval, "Trouble setting _CONV_TAGS_CREATED tag" );

    return MB_SUCCESS;
}

ErrorCode NCHelperDomain::create_mesh( Range& faces )
{
    Interface*& mbImpl = _readNC->mbImpl;
    // std::string& fileName = _readNC->fileName;
    Tag& mGlobalIdTag = _readNC->mGlobalIdTag;
    // const Tag*& mpFileIdTag = _readNC->mpFileIdTag;
    DebugOutput& dbgOut = _readNC->dbgOut;
    /*int& gatherSetRank = _readNC->gatherSetRank;
    int& trivialPartitionShift = _readNC->trivialPartitionShift;*/
    /*

      int rank = 0;
      int procs = 1;
    #ifdef MOAB_HAVE_MPI
      bool& isParallel = _readNC->isParallel;
      if (isParallel) {
        ParallelComm*& myPcomm = _readNC->myPcomm;
        rank = myPcomm->proc_config().proc_rank();
        procs = myPcomm->proc_config().proc_size();
      }
    #endif
    */

    ErrorCode rval;
    int success = 0;

    /*
      bool create_gathers = false;
      if (rank == gatherSetRank)
        create_gathers = true;

      // Shift rank to obtain a rotated trivial partition
      int shifted_rank = rank;
      if (procs >= 2 && trivialPartitionShift > 0)
        shifted_rank = (rank + trivialPartitionShift) % procs;*/

    // how many will have mask 0 or 1
    // how many will have a fraction  ? we will not instantiate all elements; only those with mask 1
    // ? also, not all vertices, only those that belong to mask 1 elements ? we will not care about
    // duplicate vertices; maybe another time ? we will start reading masks, vertices
    int local_elems = ( lDims[4] - lDims[1] ) * ( lDims[3] - lDims[0] );
    dbgOut.tprintf( 1, "local cells: %d \n", local_elems );

    // count how many will be with mask 1 here
    // basically, read the mask variable on the local elements;
    std::string maskstr( "mask" );
    ReadNC::VarData& vmask = _readNC->varInfo[maskstr];

    // mask is (nj, ni)
    vmask.readStarts.push_back( lDims[1] );
    vmask.readStarts.push_back( lDims[0] );
    vmask.readCounts.push_back( lDims[4] - lDims[1] );
    vmask.readCounts.push_back( lDims[3] - lDims[0] );
    std::vector< int > mask( local_elems );
    success = NCFUNCAG( _vara_int )( _fileId, vmask.varId, &vmask.readStarts[0], &vmask.readCounts[0], &mask[0] );
    if( success ) MB_SET_ERR( MB_FAILURE, "Failed to read int data for mask variable " );

    int nb_with_mask1 = 0;
    for( int i = 0; i < local_elems; i++ )
        if( 1 == mask[i] ) nb_with_mask1++;

    std::vector< NCDF_SIZE > startsv( 3 );
    startsv[0] = vmask.readStarts[0];
    startsv[1] = vmask.readStarts[1];
    startsv[2] = 0;
    std::vector< NCDF_SIZE > countsv( 3 );
    countsv[0] = vmask.readCounts[0];
    countsv[1] = vmask.readCounts[1];
    countsv[2] = nv;  // number of vertices per element

    // read xv and yv coords for vertices, and create elements;
    std::string xvstr( "xv" );
    ReadNC::VarData& var_xv = _readNC->varInfo[xvstr];
    std::vector< double > xv( local_elems * nv );
    success = NCFUNCAG( _vara_double )( _fileId, var_xv.varId, &startsv[0], &countsv[0], &xv[0] );
    if( success ) MB_SET_ERR( MB_FAILURE, "Failed to read double data for xv variable " );

    std::string yvstr( "yv" );
    ReadNC::VarData& var_yv = _readNC->varInfo[yvstr];
    std::vector< double > yv( local_elems * nv );
    success = NCFUNCAG( _vara_double )( _fileId, var_yv.varId, &startsv[0], &countsv[0], &yv[0] );
    if( success ) MB_SET_ERR( MB_FAILURE, "Failed to read double data for yv variable " );

    // read other variables, like xc, yc, frac, area
    std::string xcstr( "xc" );
    ReadNC::VarData& var_xc = _readNC->varInfo[xcstr];
    std::vector< double > xc( local_elems );
    success = NCFUNCAG( _vara_double )( _fileId, var_xc.varId, &vmask.readStarts[0], &vmask.readCounts[0], &xc[0] );
    if( success ) MB_SET_ERR( MB_FAILURE, "Failed to read double data for xc variable " );

    std::string ycstr( "yc" );
    ReadNC::VarData& var_yc = _readNC->varInfo[ycstr];
    std::vector< double > yc( local_elems );
    success = NCFUNCAG( _vara_double )( _fileId, var_yc.varId, &vmask.readStarts[0], &vmask.readCounts[0], &yc[0] );
    if( success ) MB_SET_ERR( MB_FAILURE, "Failed to read double data for yc variable " );

    std::string fracstr( "frac" );
    ReadNC::VarData& var_frac = _readNC->varInfo[fracstr];
    std::vector< double > frac( local_elems );
    success = NCFUNCAG( _vara_double )( _fileId, var_frac.varId, &vmask.readStarts[0], &vmask.readCounts[0], &frac[0] );
    if( success ) MB_SET_ERR( MB_FAILURE, "Failed to read double data for frac variable " );
    std::string areastr( "area" );
    ReadNC::VarData& var_area = _readNC->varInfo[areastr];
    std::vector< double > area( local_elems );
    success = NCFUNCAG( _vara_double )( _fileId, var_area.varId, &vmask.readStarts[0], &vmask.readCounts[0], &area[0] );
    if( success ) MB_SET_ERR( MB_FAILURE, "Failed to read double data for area variable " );
    // create tags for them
    Tag areaTag, fracTag, xcTag, ycTag;
    rval = mbImpl->tag_get_handle( "area", 1, MB_TYPE_DOUBLE, areaTag, MB_TAG_DENSE | MB_TAG_CREAT );MB_CHK_SET_ERR( rval, "Trouble creating area tag" );
    rval = mbImpl->tag_get_handle( "frac", 1, MB_TYPE_DOUBLE, fracTag, MB_TAG_DENSE | MB_TAG_CREAT );MB_CHK_SET_ERR( rval, "Trouble creating frac tag" );
    rval = mbImpl->tag_get_handle( "xc", 1, MB_TYPE_DOUBLE, xcTag, MB_TAG_DENSE | MB_TAG_CREAT );MB_CHK_SET_ERR( rval, "Trouble creating xc tag" );
    rval = mbImpl->tag_get_handle( "yc", 1, MB_TYPE_DOUBLE, ycTag, MB_TAG_DENSE | MB_TAG_CREAT );MB_CHK_SET_ERR( rval, "Trouble creating yc tag" );

    //
    EntityHandle* conn_arr;
    EntityHandle start_vertex;
    Range tmp_range;

    // set connectivity into that space

    EntityHandle start_cell;
    EntityType mdb_type = MBVERTEX;
    if( nv == 3 )
        mdb_type = MBTRI;
    else if( nv == 4 )
        mdb_type = MBQUAD;
    else if( nv > 4 )  // (nv > 4)
        mdb_type = MBPOLYGON;
    // for nv = 1 , type is vertex

    if( nv > 1 )
    {
        rval = _readNC->readMeshIface->get_element_connect( nb_with_mask1, nv, mdb_type, 0, start_cell, conn_arr );MB_CHK_SET_ERR( rval, "Failed to create local cells" );

        tmp_range.insert( start_cell, start_cell + nb_with_mask1 - 1 );
        // create also nv*nb_with_mask1 vertices, and compute their coordinates
    }

    // Create vertices
    int nLocalVertices = nb_with_mask1 * nv;
    std::vector< double* > arrays;
    rval = _readNC->readMeshIface->get_node_coords( 3, nLocalVertices, 0, start_vertex, arrays );MB_CHK_SET_ERR( rval, "Failed to create local vertices" );

    // Set vertex coordinates
    // will read all xv, yv, but use only those with correct mask on
    Range::iterator rit;
    double* xptr       = arrays[0];
    double* yptr       = arrays[1];
    double* zptr       = arrays[2];
    int index          = 0;  // consider the mask for advancing in moab arrays;
    int elem_index     = 0;  // total index in netcdf arrays
    const double pideg = acos( -1.0 ) / 180.0;
    double radius      = 1;

    // int nj = gDims[4]-gDims[1]; // is it about 1 in irregular cases
    int j              = lDims[1];
    int i              = lDims[0];  // if elem_index is getting to next row, increase j
    int local_row_size = lDims[3] - lDims[0];
    for( ; elem_index < local_elems; elem_index++ )
    {
        if( 0 == mask[elem_index] ) continue;  // nothing to do, do not advance elem_index in actual moab arrays
        // set area and fraction on those elements too
        for( int k = 0; k < nv; k++ )
        {
            EntityHandle vertex = start_vertex + nv * index + k;

            int index_v_arr = nv * elem_index + k;
            double x, y;
            if( nv > 1 )
            {
                conn_arr[nv * index + k] = vertex;
                x                        = xv[index_v_arr];
                y                        = yv[index_v_arr];
                double cosphi            = cos( pideg * y );
                double zmult             = sin( pideg * y );
                double xmult             = cosphi * cos( x * pideg );
                double ymult             = cosphi * sin( x * pideg );
                xptr[nv * index + k]     = radius * xmult;
                yptr[nv * index + k]     = radius * ymult;
                zptr[nv * index + k]     = radius * zmult;
            }
            else  // nv ==1 , tempest remap case, only xc make sense
            {
                x                    = xc[elem_index];
                y                    = yc[elem_index];
                xptr[nv * index + k] = x;
                yptr[nv * index + k] = y;
                zptr[nv * index + k] = 0;
            }
        }
        EntityHandle cell = start_vertex + index;
        if( nv > 1 ) cell = start_cell + index;
        // set other tags, like xc, yc, frac, area
        rval = mbImpl->tag_set_data( xcTag, &cell, 1, &xc[elem_index] );MB_CHK_SET_ERR( rval, "Failed to set xc tag" );
        rval = mbImpl->tag_set_data( ycTag, &cell, 1, &yc[elem_index] );MB_CHK_SET_ERR( rval, "Failed to set yc tag" );
        rval = mbImpl->tag_set_data( areaTag, &cell, 1, &area[elem_index] );MB_CHK_SET_ERR( rval, "Failed to set area tag" );
        rval = mbImpl->tag_set_data( fracTag, &cell, 1, &frac[elem_index] );MB_CHK_SET_ERR( rval, "Failed to set frac tag" );

        // set the global id too:
        int globalId = j * local_row_size + i + 1;
        i++;
        if( ( i - lDims[0] ) % local_row_size == 0 )
        {
            j++;
            i = lDims[0];  // start over next row
        }
        rval = mbImpl->tag_set_data( mGlobalIdTag, &cell, 1, &globalId );MB_CHK_SET_ERR( rval, "Failed to set global id tag" );
        index++;
    }

    rval = mbImpl->add_entities( _fileSet, tmp_range );MB_CHK_SET_ERR( rval, "Failed to add new cells to current file set" );

    // modify local file set, to merge coincident vertices, and to correct repeated vertices in elements
    std::vector< Tag > tagList;
    tagList.push_back( mGlobalIdTag );
    tagList.push_back( xcTag );
    tagList.push_back( ycTag );
    tagList.push_back( areaTag );
    tagList.push_back( fracTag );
    double tol = 1.e-9;
    rval       = IntxUtils::remove_duplicate_vertices( mbImpl, _fileSet, tol, tagList );MB_CHK_SET_ERR( rval, "Failed to remove duplicate vertices" );

    rval = mbImpl->get_entities_by_dimension( _fileSet, 2, faces );MB_CHK_ERR( rval );
    Range all_verts;
    rval = mbImpl->get_connectivity( faces, all_verts );MB_CHK_ERR( rval );
    rval = mbImpl->add_entities( _fileSet, all_verts );MB_CHK_ERR( rval );
#ifdef MOAB_HAVE_MPI
    ParallelComm*& myPcomm = _readNC->myPcomm;
    if( myPcomm )
    {
        ParallelMergeMesh pmm( myPcomm, tol );
        rval = pmm.merge( _fileSet,
                          /* do not do local merge*/ false,
                          /*  2d cells*/ 2 );MB_CHK_SET_ERR( rval, "Failed to merge vertices in parallel" );

        // assign global ids only for vertices, cells have them fine
        rval = myPcomm->assign_global_ids( _fileSet, /*dim*/ 0 );MB_CHK_ERR( rval );
    }
#endif

    return MB_SUCCESS;
}
}  // namespace moab
