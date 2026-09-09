cmake_minimum_required(VERSION 3.22)

project(ESMX_Data
        VERSION 9.0.0
        LANGUAGES Fortran)

find_package(ESMF 9.0.0 REQUIRED)

include(GNUInstallDirs)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  string(APPEND CMAKE_Fortran_FLAGS " -ffree-line-length-none")
endif()

add_library(ESMX_Data ESMX_Data.F90 ESMX_DataProcess.F90)
target_include_directories(ESMX_Data
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
set_target_properties(ESMX_Data
  PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR}
)
target_link_libraries(ESMX_Data ESMF)
