include (LibMPI)

include_directories("${CMAKE_SOURCE_DIR}/tests/cperf")
include_directories("${CMAKE_SOURCE_DIR}/src/clib")
include_directories("${CMAKE_BINARY_DIR}")

# Compiler-specific compiler options
if ("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "PGI")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -c99")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -c99")
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "Intel")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")
endif()
#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O0")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0")

#==============================================================================
#  PREPARE FOR TESTING
#==============================================================================

# Don't run these tests if we are using MPI SERIAL.
if (NOT PIO_USE_MPISERIAL)
  add_executable (piodecomptest EXCLUDE_FROM_ALL piodecomptest.c mpi_argp.c)
  add_dependencies (tests piodecomptest)
  target_link_libraries (piodecomptest pioc)
endif()

# Test Timeout in seconds.
if (PIO_VALGRIND_CHECK)
  set (DEFAULT_TEST_TIMEOUT 800)
else ()
  set (DEFAULT_TEST_TIMEOUT 600)
endif ()

MESSAGE("CMAKE_EXE_LINKER_FLAGS ${CMAKE_EXE_LINKER_FLAGS}")
