#include <config.h>
#include <argp.h>
#include <mpi.h>
#include <pio.h>
#include <pio_internal.h>

const char *argp_program_version = "pioperformance 0.1";
const char *argp_program_bug_address = "<https://github.com/NCAR/ParallelIO>";

static char doc[] = 
    "a test of pio for performance and correctness of a given decomposition";

static struct argp_option options[] = {
    {"wdecomp", 'w', "FILE", 0, "Decomposition file for write"},
    {"rdecomp", 'r', "FILE", 0, "Decomposition file for read (same as write if not provided)"},
    {"variable", 'v', "NAME", 0, "Variable name to write and/or read"},
    { 0 }
};

struct arguments 
{
    char *args[2];
    char *wdecomp_file;
    char *rdecomp_file;
    char *varname;
};

static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
    struct arguments *arguments = state->input;

    switch (key)
    {
    case 'w':
        arguments->wdecomp_file = arg;
        break;
    case 'r':
        arguments->rdecomp_file = arg;
        break;
    case 'v':
        arguments->varname = arg;
        break;
    case ARGP_KEY_ARG:
        if (state->arg_num >= 2)
            argp_usage(state);
        arguments->args[state->arg_num] = arg;
        break;
    default:
        return ARGP_ERR_UNKNOWN;
    }
    return 0;
}

/* Our argp parser. */
static struct argp argp = { options, parse_opt, 0, doc };

error_t mpi_argp_parse(const int rank,
                       const struct argp *argp,
                       int argc,
                       char **argv,
                       unsigned flags,
                       int *arg_index,
                       void *input);

static int debug = 0;

double *dvarw, *dvarr;
float *fvarw, *fvarr;
int *ivarw, *ivarr;
char *cvarw, *cvarr;

int test_write_darray(int iosys, const char decomp_file[], int rank, const char myvarname[])
{
    int ierr;
    int comm_size;
    int ncid;
    int iotype = PIO_IOTYPE_PNETCDF;
    int ndims;
    int *global_dimlen;
    int num_tasks;
    int *maplen;
    int maxmaplen;
    int *full_map;
    int *dimid;
    int varid;
    int globalsize;
    int ioid;
    char dimname[PIO_MAX_NAME];
    char varname[PIO_MAX_NAME];

    ierr = pioc_read_nc_decomp_int(iosys, decomp_file, &ndims, &global_dimlen, &num_tasks, 
                                   &maplen, &maxmaplen, &full_map, NULL, NULL, NULL, NULL, NULL);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);

    ierr = MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    /* TODO: allow comm_size to be >= num_tasks */
    if(comm_size != num_tasks)
    {
        if(rank == 0)
        {
            printf("Not enough MPI tasks for decomp, expected task count %d\n",num_tasks);
            ierr = MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }
       
    ierr = PIOc_createfile(iosys, &ncid, &iotype, "testfile.nc", PIO_CLOBBER);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);
    
    dimid = calloc(ndims,sizeof(int));
    for(int i=0; i<ndims; i++)
    {
        sprintf(dimname,"dim%4.4d",i);
        ierr = PIOc_def_dim(ncid, dimname, (PIO_Offset) global_dimlen[i], &(dimid[i]));
        if(ierr || debug) printf("%d %d\n",__LINE__,ierr);
    }
    /* TODO: support multiple variables and types*/
    if(myvarname != NULL)
        sprintf(varname,"%s",myvarname);        
    else
        sprintf(varname,"var%4.4d",0);

    ierr = PIOc_def_var(ncid, varname, PIO_DOUBLE, ndims, dimid, &varid);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);

    free(dimid);
    ierr = PIOc_enddef(ncid);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);

    PIO_Offset *dofmap;

    if (!(dofmap = malloc(sizeof(PIO_Offset) * maplen[rank])))
        return PIO_ENOMEM;

    /* Copy array into PIO_Offset array. */
    dvarw = malloc(sizeof(double)*maplen[rank]);
    for (int e = 0; e < maplen[rank]; e++)
    {
        dofmap[e] = full_map[rank * maxmaplen + e]+1;
        dvarw[e] = dofmap[e];
    }
    /* allocated in pioc_read_nc_decomp_int */
    free(full_map);
    ierr = PIOc_InitDecomp(iosys, PIO_DOUBLE, ndims, global_dimlen, maplen[rank],
                           dofmap, &ioid, NULL, NULL, NULL);

    free(global_dimlen);
    double dsum=0;
    for(int i=0; i < maplen[rank]; i++)
        dsum += dvarw[i];
    if(dsum != rank)
        printf("%d: dvarwsum = %d\n",rank, dsum);

    ierr = PIOc_write_darray(ncid, varid, ioid, maplen[rank], dvarw, NULL);
    free(maplen);
    ierr = PIOc_closefile(ncid);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);

    return ierr;
}


int test_read_darray(int iosys,const char decomp_file[], int rank, const char myvarname[])
{
    int ierr;
    int comm_size;
    int ncid;
    int iotype = PIO_IOTYPE_PNETCDF;
    int ndims;
    int *global_dimlen;
    int num_tasks;
    int *maplen;
    int maxmaplen;
    int *full_map;
    int *dimid;
    int varid;
    int globalsize;
    int ioid;
    int pio_type;
    char dimname[PIO_MAX_NAME];
    char varname[PIO_MAX_NAME];

    ierr = pioc_read_nc_decomp_int(iosys, decomp_file, &ndims, &global_dimlen, &num_tasks, 
                                   &maplen, &maxmaplen, &full_map, NULL, NULL, NULL, NULL, NULL);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);

    ierr = MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    /* TODO: allow comm_size to be >= num_tasks */
    if(comm_size != num_tasks)
    {
        if(rank == 0)
        {
            printf("Not enough MPI tasks for decomp, expected task count %d\n",num_tasks);
            ierr = MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }
       
    ierr = PIOc_openfile(iosys, &ncid, &iotype, "testfile.nc", PIO_NOWRITE);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);
    
    /* TODO: support multiple variables and types*/
    if(myvarname != NULL)
        sprintf(varname,"%s",myvarname);        
    else
        sprintf(varname,"var%4.4d",0);
    ierr = PIOc_inq_varid(ncid, varname, &varid);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);

    ierr = PIOc_inq_varndims(ncid, varid, &ndims);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);

    ierr = PIOc_inq_vartype(ncid, varid, &pio_type);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);

    dimid = calloc(ndims,sizeof(int));
    ierr = PIOc_inq_vardimid(ncid, varid, dimid);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);

    for(int i=0; i<ndims; i++)
    {
        PIO_Offset gdimlen;
        ierr = PIOc_inq_dimlen(ncid, dimid[i], &gdimlen);
        
        pioassert(gdimlen == global_dimlen[i], "testfile.nc does not match decomposition file",__FILE__,__LINE__);
    }
    free(dimid);
    PIO_Offset *dofmap;

    if (!(dofmap = malloc(sizeof(PIO_Offset) * maplen[rank])))
        return PIO_ENOMEM;

    for (int e = 0; e < maplen[rank]; e++)
    {
        dofmap[e] = full_map[rank * maxmaplen + e] + 1;
    }
    free(full_map);
//    PIOc_set_log_level(3);
    ierr = PIOc_InitDecomp(iosys, pio_type, ndims, global_dimlen, maplen[rank],
                           dofmap, &ioid, NULL, NULL, NULL);
    free(dofmap);
    free(global_dimlen);
    switch(pio_type)
    {
    case PIO_DOUBLE:
        dvarr = malloc(sizeof(double)*maplen[rank]);
        ierr = PIOc_read_darray(ncid, varid, ioid, maplen[rank], dvarr);
        double dsum=0;
        for(int i=0; i < maplen[rank]; i++)
            dsum += dvarr[i];
        if(dsum != rank)
            printf("%d: dsum = %d\n",rank, dsum);
        break;
    case PIO_INT:
        ivarr = malloc(sizeof(int)*maplen[rank]);
        ierr = PIOc_read_darray(ncid, varid, ioid, maplen[rank], ivarr);
        int isum=0;
        for(int i=0; i < maplen[rank]; i++)
            isum += ivarr[i];
        printf("%d: isum = %d\n",rank, isum);
        break;
    case PIO_FLOAT:
        fvarr = malloc(sizeof(float)*maplen[rank]);
        ierr = PIOc_read_darray(ncid, varid, ioid, maplen[rank], fvarr);
        float fsum=0;
        for(int i=0; i < maplen[rank]; i++)
            fsum += fvarr[i];
        printf("%d: fsum = %f\n",rank, fsum);
        break;
    case PIO_BYTE:
        cvarr = malloc(sizeof(char)*maplen[rank]);
        ierr = PIOc_read_darray(ncid, varid, ioid, maplen[rank], cvarr);
        int csum=0;
        for(int i=0; i < maplen[rank]; i++)
            csum += (int) cvarr[i];
        printf("%d: csum = %d\n",rank, csum);
        break;
    }

    ierr = PIOc_closefile(ncid);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);


    free(maplen);
    return ierr;

}



int main(int argc, char *argv[])
{
    struct arguments arguments;
    int ierr;
    int rank;
    int comm_size;
    int iosys;
    int iotasks;
    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);

    arguments.wdecomp_file = NULL;
    arguments.rdecomp_file = NULL;
    arguments.varname = NULL;
    mpi_argp_parse(rank, &argp, argc, argv, 0, 0, &arguments);

    if(! arguments.rdecomp_file)
        arguments.rdecomp_file = arguments.wdecomp_file;

    iotasks = comm_size/36;

    ierr = PIOc_Init_Intracomm(MPI_COMM_WORLD, iotasks, 36, 0, PIO_REARR_SUBSET, &iosys);
    if(ierr || debug) printf("%d %d\n",__LINE__,ierr);

    if(arguments.wdecomp_file)
    {
        ierr = test_write_darray(iosys, arguments.wdecomp_file, rank, arguments.varname);
        if(ierr || debug) printf("%d %d\n",__LINE__,ierr);
    }
    if(arguments.rdecomp_file)
    {
        ierr = test_read_darray(iosys, arguments.rdecomp_file, rank, arguments.varname);
        if(ierr || debug) printf("%d %d\n",__LINE__,ierr);
    }
    MPI_Finalize();

}

