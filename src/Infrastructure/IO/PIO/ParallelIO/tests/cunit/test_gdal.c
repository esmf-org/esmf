/*
 * Tests for PIO distributed arrays.
 *
 * @author Ed Hartnett
 * @date 2/16/17
 */
#include <config.h>
#include <pio.h>
#include <pio_internal.h>
#include <pio_tests.h>

/* The number of tasks this test should run on. */
#define TARGET_NTASKS 4

/* The minimum number of tasks this test should run on. */
#define MIN_NTASKS 4

/* The name of this test. */
#define TEST_NAME "test_gdal"

/* Number of processors that will do IO. */
#define NUM_IO_PROCS 1

/* Number of computational components to create. */
#define COMPONENT_COUNT 1

/* The number of dimensions in the example data. In this test, we
 * are using three-dimensional data. */
#define NDIM 1

/* But sometimes we need arrays of the non-record dimensions. */
#define NDIM2 2

/* The length of our sample data along each dimension. */
#define X_DIM_LEN 4
#define Y_DIM_LEN 4

/* The number of timesteps of data to write. */
#define NUM_TIMESTEPS 2

/* The names of variables in the netCDF output files. */
#define VAR_NAME "Billy-Bob"
#define VAR_NAME2 "Sally-Sue"

/* Test cases relating to PIOc_write_darray_multi(). */
#define NUM_TEST_CASES_WRT_MULTI 3

/* Test with and without specifying a fill value to
 * PIOc_write_darray(). */
#define NUM_TEST_CASES_FILLVALUE 2

/* The dimension names. */
//char dim_name[NDIM][PIO_MAX_NAME + 1] = {"timestep", "x", "y"};

/* Length of the dimensions in the sample data. */
//int dim_len[NDIM] = {NC_UNLIMITED, X_DIM_LEN, Y_DIM_LEN};

/* Create a 1D decomposition.
 *
 * @param ntasks the number of available tasks
 * @param my_rank rank of this task.
 * @param iosysid the IO system ID.
 * @param dim_len an array of length 3 with the dimension sizes.
 * @param ioid a pointer that gets the ID of this decomposition.
 * @param pio_type the type that will be used for basetype.
 * @returns 0 for success, error code otherwise.
 **/
int create_decomposition_1d(int ntasks, int my_rank, int iosysid, int *ioid, int pio_type)
{
    PIO_Offset elements_per_pe;     /* Array elements per processing unit. */
    int dim_len_1d[NDIM] = {X_DIM_LEN};
    int ret;

    /* How many data elements per task? In this example we will end up
     * with 2. */
    elements_per_pe = X_DIM_LEN / ntasks;

    PIO_Offset compdof[elements_per_pe];

    /* Don't forget to add 1! */
    compdof[0] = my_rank + 1;

    /* This means fill value will be used here. */
    compdof[1] = 0;

    /* Create the PIO decomposition for this test. */
    if ((ret = PIOc_InitDecomp(iosysid, pio_type, NDIM, dim_len_1d, elements_per_pe,
                               compdof, ioid, NULL, NULL, NULL)))
        ERR(ret);

    return 0;
}

/**
 * Test the darray functionality. Create a netCDF file with 3
 * dimensions and 1 PIO_INT variable, and use darray to write some
 * data.
 *
 * @param iosysid the IO system ID.
 * @param ioid the ID of the decomposition.
 * @param num_flavors the number of IOTYPES available in this build.
 * @param flavor array of available iotypes.
 * @param my_rank rank of this task.
 * @param pio_type the type of the data.
 * @returns 0 for success, error code otherwise.
 */
int test_gdal(int iosysid, int ioid, int num_flavors, int *flavor, int my_rank,
                int pio_type)
{
    char filename[PIO_MAX_NAME + 1]; /* Name for the output files. */
    int dimids[NDIM];      /* The dimension IDs. */
    int ncid;      /* The ncid of the netCDF file. */
    int ncid2;     /* The ncid of the re-opened netCDF file. */
    int varid;     /* The ID of the netCDF varable. */
    int varid2;     /* The ID of a netCDF varable of different type. */
    int wrong_varid = TEST_VAL_42;  /* A wrong ID. */
    int ret;       /* Return code. */
    MPI_Datatype mpi_type;
    int type_size; /* size of a variable of type pio_type */
    int other_type; /* another variable of the same size but different type */
    PIO_Offset arraylen = 4;
    void *fillvalue, *ofillvalue;
    void *test_data;
    void *test_data_in;
    int fillvalue_int = NC_FILL_INT;
    int test_data_int[arraylen];
    int test_data_int_in[arraylen];
    float fillvalue_float = NC_FILL_FLOAT;
    float test_data_float[arraylen];
    float test_data_float_in[arraylen];
    double fillvalue_double = NC_FILL_DOUBLE;
    double test_data_double[arraylen];
    double test_data_double_in[arraylen];
    int iotype = PIO_IOTYPE_GDAL;

    GDALDatasetH hDSp;

    /* Initialize some data. */
    for (int f = 0; f < arraylen; f++)
    {
        test_data_int[f] = my_rank * 10 + f;
        test_data_float[f] = my_rank * 10 + f + 0.5;
        test_data_double[f] = my_rank * 100000 + f + 0.5;
    }

    /* Use PIO to create the example file in each of the four
     * available ways. */
    for (int fmt = 0; fmt < num_flavors; fmt++)
    {

        /* Add a couple of extra tests for the
         * PIOc_write_darray_multi() function. */
        for (int test_multi = 0; test_multi < NUM_TEST_CASES_WRT_MULTI; test_multi++)
        {
	  sprintf(filename, "data/cb_2018_us_region_20m.shp");

	  test_data_in = test_data_float_in;
	  /* Open the file. */
	  if ((ret = GDALc_openfile(iosysid, &ncid2, &hDSp, &iotype, filename, PIO_NOWRITE)))
	    ERR(ret);

	  if ((ret = GDALc_inq_fieldid(ncid2, "GEOID", &varid)))
	    ERR(ret);

	  /* Read the data. */
	  if ((ret = PIOc_read_darray(ncid2, varid, ioid, arraylen, test_data_in)))
	    ERR(ret);
	  
	  /* Check the results. */
// TEMPORARILY DISABLED UNTIL A SEGFAULT IS DIAGNOSED
//	  for (int f = 0; f < arraylen; f++)
//	    {
//	      switch (pio_type)
//		{
//		case PIO_INT:
//		  if (test_data_int_in[f] != test_data_int[f])
//		    return ERR_WRONG;
//		  break;
//		case PIO_FLOAT:
//		  if (test_data_float_in[f] != test_data_float[f])
//		    return ERR_WRONG;
//		  break;
//		case PIO_DOUBLE:
//		  if (test_data_double_in[f] != test_data_double[f])
//		    return ERR_WRONG;
//		  break;
//		default:
//		  ERR(ERR_WRONG);
//		}
//	    }

	  /* Close the netCDF file. */
	  if ((ret = PIOc_closefile(ncid2)))
	    ERR(ret);
        } /* next test multi */
    } /* next iotype */

    return PIO_NOERR;
}

/**
 * Run all the tests.
 *
 * @param iosysid the IO system ID.
 * @param num_flavors number of available iotypes in the build.
 * @param flavor pointer to array of the available iotypes.
 * @param my_rank rank of this task.
 * @param test_comm the communicator the test is running on.
 * @returns 0 for success, error code otherwise.
 */
int test_all_gdal(int iosysid, int num_flavors, int *flavor, int my_rank,
                    MPI_Comm test_comm)
{
#define NUM_TYPES_TO_TEST 1
    int ioid;
    char filename[PIO_MAX_NAME + 1];
    int pio_type[NUM_TYPES_TO_TEST] = {PIO_DOUBLE};
    int dim_len_1d[NDIM] = {X_DIM_LEN};//, Y_DIM_LEN};
    int ret; /* Return code. */

    for (int t = 0; t < NUM_TYPES_TO_TEST; t++)
    {
//        /* This will be our file name for writing out decompositions. */
//        sprintf(filename, "%s_decomp_rank_%d_flavor_%d_type_%d.nc", TEST_NAME, my_rank,
//                *flavor, pio_type[t]);

        /* Decompose the data over the tasks. */
        if ((ret = create_decomposition_1d(TARGET_NTASKS, my_rank, iosysid,
                                           &ioid, pio_type[t])))
	  return ret;

	printf("my_rank %d iosysid %d ioid %d ret %d\n",my_rank, iosysid, ioid, ret);

        /* Run a simple darray test. */
        if ((ret = test_gdal(iosysid, ioid, num_flavors, flavor, my_rank, pio_type[t])))
	  return ret;

        /* Free the PIO decomposition. */
        if ((ret = PIOc_freedecomp(iosysid, ioid)))
	  ERR(ret);
    }

    return PIO_NOERR;
}

/* Run tests for darray functions. */
int main(int argc, char **argv)
{
#define NUM_REARRANGERS_TO_TEST 2
    int rearranger[NUM_REARRANGERS_TO_TEST] = {PIO_REARR_BOX, PIO_REARR_SUBSET};
    int my_rank;
    int ntasks;
    int num_flavors; /* Number of PIO netCDF flavors in this build. */
    int flavor[NUM_FLAVORS]; /* iotypes for the supported netCDF IO flavors. */
    MPI_Comm test_comm; /* A communicator for this test. */
    int ret;         /* Return code. */

    OGRRegisterAll();

    /* Initialize test. */
    if ((ret = pio_test_init2(argc, argv, &my_rank, &ntasks, MIN_NTASKS,
                              MIN_NTASKS, -1, &test_comm)))
        ERR(ERR_INIT);

    PIOc_set_log_level(4);

    if ((ret = PIOc_set_iosystem_error_handling(PIO_DEFAULT, PIO_RETURN_ERROR, NULL)))
        return ret;

    /* Only do something on max_ntasks tasks. */
    if (my_rank < TARGET_NTASKS)
    {
        int iosysid;  /* The ID for the parallel I/O system. */
        int ioproc_stride = 1;    /* Stride in the mpi rank between io tasks. */
        int ioproc_start = 0;     /* Zero based rank of first processor to be used for I/O. */
        int ret;      /* Return code. */

        /* Figure out iotypes. */
        if ((ret = get_iotypes(&num_flavors, flavor)))
            ERR(ret);

        for (int r = 0; r < NUM_REARRANGERS_TO_TEST; r++)
        {
            /* Initialize the PIO IO system. This specifies how
             * many and which processors are involved in I/O. */
            if ((ret = PIOc_Init_Intracomm(test_comm, TARGET_NTASKS, ioproc_stride,
                                           ioproc_start, rearranger[r], &iosysid)))
                return ret;

            /* Run tests. */
            if ((ret = test_all_gdal(iosysid, num_flavors, flavor, my_rank, test_comm)))
                return ret;

            /* Finalize PIO system. */
            if ((ret = PIOc_free_iosystem(iosysid)))
                return ret;
        } /* next rearranger */
    } /* endif my_rank < TARGET_NTASKS */

    /* Finalize the MPI library. */
    if ((ret = pio_test_finalize(&test_comm)))
        return ret;
    /* if ((ret = pio_test_finalize2(&test_comm, TEST_NAME))) */
    /*     return ret; */

    printf("%d %s SUCCESS!!\n", my_rank, TEST_NAME);
    return 0;
}
