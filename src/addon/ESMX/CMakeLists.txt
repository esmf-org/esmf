cmake_minimum_required(VERSION 3.22)

# Project
project(ESMX
        VERSION 9.0.0
        LANGUAGES Fortran CXX C
        )

# Find ESMF
find_package(ESMF 9.0.0 REQUIRED)

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
