cmake_minimum_required(VERSION 3.22)

# Project
project(ESMX
        VERSION 9.0.0
        LANGUAGES Fortran CXX C
        )

if(NOT TARGET ESMF AND NOT ESMF_FOUND)
  # Find ESMF
  find_package(ESMF 9.0.0 REQUIRED)
endif()

# Define standard installation directories
include(GNUInstallDirs)

# Add ESMX driver
add_subdirectory(Driver)

# Define ESMX_EXE_NAME defaults if not supplied by parent scope/CLI
if(NOT DEFINED ESMX_EXE_NAME)
  set(ESMX_EXE_NAME "esmx_app" CACHE STRING "Name of the ESMX executable")
endif()

# Create ESMX executable
add_executable(${ESMX_EXE_NAME} ESMX_App.F90)
target_include_directories(${ESMX_EXE_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(${ESMX_EXE_NAME} PRIVATE esmx_driver)
set_target_properties(${ESMX_EXE_NAME} PROPERTIES
  INSTALL_RPATH     "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR};${CMAKE_INSTALL_PREFIX}/lib"
  LINKER_LANGUAGE   Fortran
)

# Install executable
install(TARGETS ${ESMX_EXE_NAME} DESTINATION ${CMAKE_INSTALL_BINDIR})
