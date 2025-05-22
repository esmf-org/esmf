#include <pio.h>

#if NETCDF_VERSION_LE(4,9,1)
// only netcdf4 formats supported
#define NUM_MODES 4
#elif defined(HAVE_NETCDF_PAR)
#define NUM_MODES 10
#else
// need to fix this: netcdf4 not available with netcdf serial bld
#define NUM_MODES  6
#endif


#if NUM_MODES==4
            int cmode[NUM_MODES] = {NC_PIO|NC_NETCDF4,
                                    NC_PIO|NC_NETCDF4|NC_MPIIO,
                                    NC_PIO|NC_NETCDF4|NC_CLASSIC_MODEL,
                                    NC_PIO|NC_NETCDF4|NC_MPIIO|NC_CLASSIC_MODEL};
            char mode_name[NUM_MODES][NC_MAX_NAME] = {"netcdf4 serial          ",
                                                      "netcdf4 parallel        ",
                                                      "netcdf4 classic serial  ",
                                                      "netcdf4 classic parallel"};
            int expected_format[NUM_MODES] = {NC_PIO|NC_FORMAT_NETCDF4,
                                              NC_PIO|NC_FORMAT_NETCDF4,
                                              NC_PIO|NC_FORMAT_NETCDF4_CLASSIC,
                                              NC_PIO|NC_FORMAT_NETCDF4_CLASSIC};
#endif
#if NUM_MODES==6
int cmode[NUM_MODES] = {NC_PIO,
                        NC_PIO|NC_64BIT_OFFSET,
                        NC_PIO|NC_64BIT_DATA,
                        NC_PIO|NC_PNETCDF,
                        NC_PIO|NC_PNETCDF|NC_64BIT_OFFSET,
                        NC_PIO|NC_PNETCDF|NC_64BIT_DATA};


            char mode_name[NUM_MODES][NC_MAX_NAME] = {"classic serial          ",
                                                      "64bit offset serial     ",
                                                      "64bit data serial       ",
                                                      "classic pnetcdf         ",
                                                      "64bit offset pnetcdf    ",
                                                      "64bit data pnetcdf      "};


            int expected_format[NUM_MODES] = {NC_PIO|NC_FORMAT_CLASSIC,
                                              NC_PIO|NC_FORMAT_64BIT_OFFSET,
                                              NC_PIO|NC_FORMAT_64BIT_DATA,
                                              NC_PIO|NC_FORMAT_CLASSIC,
                                              NC_PIO|NC_FORMAT_64BIT_OFFSET,
                                              NC_PIO|NC_FORMAT_64BIT_DATA};
#endif
#if NUM_MODES==10
int cmode[NUM_MODES] = {NC_PIO,
                        NC_PIO|NC_64BIT_OFFSET,
                        NC_PIO|NC_64BIT_DATA,
                        NC_PIO|NC_PNETCDF,
                        NC_PIO|NC_PNETCDF|NC_64BIT_OFFSET,
                        NC_PIO|NC_PNETCDF|NC_64BIT_DATA,
                        NC_PIO|NC_NETCDF4,
                        NC_PIO|NC_NETCDF4|NC_CLASSIC_MODEL,
			NC_PIO|NC_NETCDF4|NC_MPIIO,
			NC_PIO|NC_NETCDF4|NC_MPIIO|NC_CLASSIC_MODEL};

            char mode_name[NUM_MODES][NC_MAX_NAME] = {"classic serial          ",
                                                      "64bit offset serial     ",
                                                      "64bit data serial       ",
                                                      "classic pnetcdf         ",
                                                      "64bit offset pnetcdf    ",
                                                      "64bit data pnetcdf      ",
                                                      "netcdf4 serial          ",
                                                      "netcdf4 classic serial  ",
                                                      "netcdf4 parallel        ",
                                                      "netcdf4 classic parallel"};


            int expected_format[NUM_MODES] = {NC_PIO|NC_FORMAT_CLASSIC,
                                              NC_PIO|NC_FORMAT_64BIT_OFFSET,
                                              NC_PIO|NC_FORMAT_64BIT_DATA,
                                              NC_PIO|NC_FORMAT_CLASSIC,
                                              NC_PIO|NC_FORMAT_64BIT_OFFSET,
                                              NC_PIO|NC_FORMAT_64BIT_DATA,
                                              NC_PIO|NC_FORMAT_NETCDF4,
                                              NC_PIO|NC_FORMAT_NETCDF4_CLASSIC,
                                              NC_PIO|NC_FORMAT_NETCDF4,
                                              NC_PIO|NC_FORMAT_NETCDF4_CLASSIC};
#endif
