#==============================================================================
#  DEFINE THE TARGETS AND TESTS
#==============================================================================

add_executable (pioperf EXCLUDE_FROM_ALL
  pioperformance.F90)
target_link_libraries (pioperf piof)
add_dependencies (tests pioperf)

# Compiler-specific compile options
if ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  target_compile_options (piof
    PRIVATE -ffree-line-length-none)
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
  set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mismatch_all " )
  #    target_compile_options (piof
  #        PRIVATE -mismatch_all)
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Cray")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ef")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -debug minimal")
endif()
if (CMAKE_BUILD_TYPE STREQUAL "DEBUG")
     set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g")
endif()

if (PIO_HDF5_LOGGING)
  target_compile_definitions (pioperf
    PUBLIC LOGGING)
endif ()
#if (DEFINED ENV{NCAR_SPACK_HASH_LINARO_FORGE})
#     set (CMAKE_EXE_LINKER_FLAGS "-L. -lmap-sampler-pmpi -lmap-sampler -Wl,-rpath /glade/work/jedwards/sandboxes/ParallelIO/bld/tests/performance")
#endif()
