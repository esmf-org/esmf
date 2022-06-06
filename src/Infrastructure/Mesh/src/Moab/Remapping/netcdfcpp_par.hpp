//-------------------------------------------------------------------------
// Filename      : netcdfcpp_par.hpp
//
// Purpose       : Parallel C++ Climate NC file read/write
//
// Creator       : Vijay Mahadevan
//-------------------------------------------------------------------------

#if defined( MOAB_HAVE_NETCDFPAR ) && defined( MOAB_HAVE_TEMPESTREMAP )
#include "netcdf_par.h"
#include "netcdfcpp.h"

class ParNcFile : public NcFile
{
  public:
    ParNcFile( MPI_Comm comm, MPI_Info comm_info, const char* path, FileMode fmode = ReadOnly,
               FileFormat fformat = Classic )
        : NcFile(), m_comm( comm )
    {
        NcError err( NcError::silent_nonfatal );  // constructor must not fail

        int mode      = NC_NOWRITE;
        the_fill_mode = Fill;
        int status;

        // If the user wants a 64-bit offset format, set that flag.
        if( fformat == Offset64Bits ) mode |= NC_64BIT_OFFSET;
#ifndef NETCDF3_ONLY
        else if( fformat == Netcdf4 )
            mode |= NC_NETCDF4;
        else if( fformat == Netcdf4Classic )
            mode |= NC_NETCDF4 | NC_CLASSIC_MODEL;
#endif
        mode |= NC_MPIIO;

        switch( fmode )
        {
            case Write:
                mode |= NC_WRITE;
            /*FALLTHRU*/
            case ReadOnly:
                // use netcdf-3 interface to permit specifying tuning parameter
                status = NcError::set_err( nc_open_par( path, mode, comm, comm_info, &the_id ) );
                if( status != NC_NOERR )
                {
                    NcError::set_err( status );
                    the_id = -1;
                }
                in_define_mode = 0;
                break;
            case New:
                mode |= NC_NOCLOBBER;
            /*FALLTHRU*/
            case Replace:
                // use netcdf-3 interface to permit specifying tuning parameters
                status = NcError::set_err( nc_create_par( path, mode, comm, comm_info, &the_id ) );
                if( status != NC_NOERR )
                {
                    NcError::set_err( status );
                    the_id = -1;
                }
                in_define_mode = 1;
                break;
            default:
                the_id         = ncBad;
                in_define_mode = 0;
                break;
        }
        if( is_valid() )
        {
            dimensions = new NcDim*[NC_MAX_DIMS];
            variables  = new NcVar*[NC_MAX_VARS];
            int i;
            for( i = 0; i < num_dims(); i++ )
                dimensions[i] = new NcDim( this, i );
            for( i = 0; i < num_vars(); i++ )
                variables[i] = new NcVar( this, i );
            globalv = new NcVar( this, ncGlobal );
        }
        else
        {
            dimensions = 0;
            variables  = 0;
            globalv    = 0;
        }
    }

    virtual ~ParNcFile( void ){};

    NcBool enable_var_par_access( NcVar* var, bool is_independent = true )  // synchronize to disk
    {
        int status;
        status = NcError::set_err(
            nc_var_par_access( the_id, var->id(), ( is_independent ? NC_INDEPENDENT : NC_COLLECTIVE ) ) );
        if( status != NC_NOERR )
        {
            NcError::set_err( status );
            return 0;
        }
        return 1;
    }

  protected:
    MPI_Comm m_comm;
    static const int ncGlobal = NC_GLOBAL;  // psuedo-variable for global attributes
    static const int ncBad    = -1;         // failure return for netCDF C interface
};

#endif  // #if defined(MOAB_HAVE_NETCDFPAR) && defined(MOAB_HAVE_TEMPESTREMAP)
