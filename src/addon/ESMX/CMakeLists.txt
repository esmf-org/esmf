cmake_minimum_required(VERSION 3.22)

# Where to look for the local Find<Package>.cmake files
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

# Find ESMF
find_package(ESMF 8.9.0 MODULE REQUIRED)

if(NOT DEFINED CMAKE_Fortran_COMPILER)
  set(CMAKE_Fortran_COMPILER        "${ESMF_F90COMPILER}")
endif()
if(NOT DEFINED CMAKE_CXX_COMPILER)
  set(CMAKE_CXX_COMPILER            "${ESMF_CXXCOMPILER}")
endif()
if(NOT DEFINED CMAKE_C_COMPILER)
  set(CMAKE_C_COMPILER              "${ESMF_CCOMPILER}")
endif()

# Project
project(ESMX
        VERSION 8.9.0
        LANGUAGES Fortran CXX C
        )

# Define standard installation directories
include(GNUInstallDirs)

# Add ESMX driver
add_subdirectory(Driver)

# define ESMX_EXE_NAME
if(NOT DEFINED ESMX_EXE_NAME)
  set(ESMX_EXE_NAME "esmx_app")
endif()

# Create ESMX executable
add_executable(${ESMX_EXE_NAME} ESMX_App.F90)
target_include_directories(${ESMX_EXE_NAME} PUBLIC ${PROJECT_BINARY_DIR})
target_link_libraries(${ESMX_EXE_NAME} PUBLIC esmx_driver)
set_target_properties(${ESMX_EXE_NAME} PROPERTIES
  INSTALL_RPATH     "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR};${CMAKE_INSTALL_PREFIX}/lib"
  LINKER_LANGUAGE   Fortran
)

# Install executable
install(TARGETS ${ESMX_EXE_NAME})
