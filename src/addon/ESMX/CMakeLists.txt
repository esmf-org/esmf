cmake_minimum_required(VERSION 3.22)

# Where to look for the local Find<Package>.cmake files
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/Driver/cmake")

# Find ESMF
find_package(ESMF 8.6.0 MODULE REQUIRED)

# Set compilers consistent with ESMF
set(CMAKE_Fortran_COMPILER        "${ESMF_F90COMPILER}")
set(CMAKE_CXX_COMPILER            "${ESMF_CXXCOMPILER}")
set(CMAKE_C_COMPILER              "${ESMF_CCOMPILER}")

# Project
project(ESMX
        VERSION 1.0.0
        LANGUAGES Fortran CXX C
        )

# Add ESMX driver
add_subdirectory(Driver)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -fbacktrace -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fbacktrace -O0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -fast")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2 -fast")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0")
else()
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} Fortran compiler will be used with default options")
endif()

# define ESMX_EXE_NAME
if(NOT DEFINED ESMX_EXE_NAME)
  set(ESMX_EXE_NAME "esmx_app")
endif()

# Create ESMX executable
add_executable(${ESMX_EXE_NAME} ESMX_App.F90)
target_include_directories(${ESMX_EXE_NAME} PUBLIC ${PROJECT_BINARY_DIR})
target_link_libraries(${ESMX_EXE_NAME} PUBLIC esmx_driver)
set_target_properties(${ESMX_EXE_NAME} PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)

# Install executable
install(
  TARGETS ${ESMX_EXE_NAME}
  EXPORT esmx
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)
