# Where to look for Find<Package>.cmake files
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

# Find ESMF
find_package(ESMF 8.4.0 MODULE REQUIRED)

# Set compilers per ESMFMKFILE
find_program(CMAKE_CXX_COMPILER ${ESMF_CXXCOMPILER})
find_program(CMAKE_Fortran_COMPILER ${ESMF_F90COMPILER})
set(CMAKE_CXX_COMPILER ${ESMF_CXXCOMPILER} PARENT_SCOPE)
set(CMAKE_Fortran_COMPILER ${ESMF_F90COMPILER} PARENT_SCOPE)

# Specify driver dependency
add_library(esmx_driver ESMX_Driver.F90)
target_include_directories(esmx_driver PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(esmx_driver PUBLIC ESMF)

# Generate comp*.* files from esmxBuild.yaml
find_package(PythonInterp 3.5 REQUIRED)
execute_process(COMMAND ${PYTHON_EXECUTABLE}
  ${CMAKE_CURRENT_LIST_DIR}/esmxGen.py --ifile ${ESMX_BUILD_FILE} --odir ${CMAKE_CURRENT_BINARY_DIR}
  RESULT_VARIABLE ret)
if(NOT ret EQUAL "0")
  message(FATAL_ERROR "esmxGen.py failed processing esmxBuild.yaml")
endif()

# Include compList.txt which in turn includes component libraries
include(${CMAKE_CURRENT_BINARY_DIR}/compList.txt)
include(ExternalProject)

foreach(CMP IN ITEMS ${COMPS})
  # get component options
  set(CMP_BUILD_TYPE         "${${CMP}-BUILD_TYPE}")
  set(CMP_SOURCE_DIR         "${${CMP}-SOURCE_DIR}")
  set(CMP_CMAKE_CONFIG       "${${CMP}-CMAKE_CONFIG}")
  set(CMP_INSTALL_PREFIX     "${${CMP}-INSTALL_PREFIX}")
  set(CMP_INSTALL_LIBDIR     "${${CMP}-INSTALL_LIBDIR}")
  set(CMP_INSTALL_CONFDIR    "${${CMP}-INSTALL_CONFDIR}")
  set(CMP_INSTALL_INCLUDEDIR "${${CMP}-INSTALL_INCLUDEDIR}")
  set(CMP_FORT_MODULE        "${${CMP}-FORT_MODULE}")
  set(CMP_LIBRARY            "${${CMP}-LIBRARY}")
  set(CMP_BUILD_ARGS         "${${CMP}-BUILD_ARGS}")
  string(TOLOWER "${CMP_BUILD_TYPE}" CMP_BUILD_TYPE)
  if(${CMP_BUILD_TYPE} STREQUAL "external_project")
    message(STATUS "Building ExternalProject ... ${CMP}")
    find_file(FND_CMAKE_LISTS CMakeLists.txt
      PATHS ${CMP_SOURCE_DIR}
      NO_CACHE NO_DEFAULT_PATH
    )
    if(NOT FND_CMAKE_LISTS)
      message(FATAL_ERROR "Could not find CMakeLists.txt"
        " in ${CMP_SOURCE_DIR}."
        " Check source_dir in build configuration."
      )
    endif()
    unset(FND_CMAKE_LISTS)
    ExternalProject_Add(${CMP}
      PREFIX ${CMAKE_CURRENT_BINARY_DIR}/${CMP}
      SOURCE_DIR ${CMP_SOURCE_DIR}
      INSTALL_DIR ${CMP_INSTALL_PREFIX}
      CMAKE_ARGS
        -DCMAKE_INSTALL_PREFIX=${CMP_INSTALL_PREFIX}
        ${CMP_BUILD_ARGS}
      BUILD_ALWAYS YES
    )
    add_dependencies(esmx_driver ${CMP})
    target_include_directories(esmx_driver PUBLIC
      ${CMAKE_CURRENT_BINARY_DIR}/${CMP}/src/${CMP}-build/${CMP_INSTALL_INCLUDEDIR})
    target_link_libraries(esmx_driver PUBLIC
      ${CMAKE_CURRENT_BINARY_DIR}/${CMP}/src/${CMP}-build/lib${CMP_LIBRARY}.a)
  elseif(${CMP_BUILD_TYPE} STREQUAL "subdirectory")
    message(STATUS "Adding Subdirectory ... ${CMP}")
    find_file(FND_CMAKE_LISTS CMakeLists.txt
      PATHS ${CMP_SOURCE_DIR}
      NO_CACHE NO_DEFAULT_PATH
    )
    if(NOT FND_CMAKE_LISTS)
      message(FATAL_ERROR "Could not find CMakeLists.txt"
        " in ${CMP_SOURCE_DIR}."
        " Check source_dir in build configuration."
      )
    endif()
    unset(FND_CMAKE_LISTS)
    add_subdirectory(${CMP_SOURCE_DIR} ${CMP})
    add_dependencies(esmx_driver ${CMP_LIBRARY})
    target_include_directories(esmx_driver PUBLIC
      ${CMAKE_CURRENT_BINARY_DIR}/${CMP}/${CMP_INSTALL_INCLUDEDIR})
    target_link_libraries(esmx_driver PUBLIC ${CMP_LIBRARY})
  elseif(${CMP_BUILD_TYPE} STREQUAL "preinstalled")
    message(STATUS "Linking Preinstalled ... ${CMP}")
    find_file(FND_CMAKE_CONFIG NAMES ${CMP_CMAKE_CONFIG}
      HINTS ${CMP_INSTALL_PREFIX}
      PATH_SUFFIXES ${CMP_INSTALL_CONFDIR} "." "cmake"
      NO_CACHE NO_DEFAULT_PATH
    )
    if(NOT FND_CMAKE_CONFIG)
      message(FATAL_ERROR "Could not find ${CMP_CMAKE_CONFIG}"
        " in ${CMP_INSTALL_PREFIX}."
        " Check installation or set install_prefix in build configuration."
      )
    endif()
    include(${FND_CMAKE_CONFIG})
    unset(FND_CMAKE_CONFIG)
    find_path(FND_FORT_MODULE NAMES ${CMP_FORT_MODULE}
      HINTS ${CMP_INSTALL_PREFIX}
      PATH_SUFFIXES ${CMP_INSTALL_INCLUDEDIR} "." "include" "mod"
      NO_CACHE NO_DEFAULT_PATH
    )
    if(NOT FND_FORT_MODULE)
      message(FATAL_ERROR "Could not find ${CMP_FORT_MODULE}"
        " in ${CMP_INSTALL_PREFIX}."
        " Check installation or set install_prefix in build configuration"
      )
    endif()
    target_include_directories(esmx_driver PUBLIC ${FND_FORT_MODULE})
    unset(FND_FORT_MODULE)
    find_library(FND_LIBRARY NAMES ${CMP_LIBRARY}
      HINTS ${CMP_INSTALL_PREFIX}
      PATH_SUFFIXES ${CMP_INSTALL_LIBDIR} "." "lib" "lib64"
      NO_CACHE NO_DEFAULT_PATH
    )
    if(NOT FND_LIBRARY)
      message(FATAL_ERROR "Could not find lib${CMP_LIBRARY}.a"
        " in ${CMP_INSTALL_PREFIX}."
        " Check installation or set install_prefix in build configuration"
      )
    endif()
    target_link_libraries(esmx_driver PUBLIC ${FND_LIBRARY})
    unset(FND_LIBRARY)
  else()
    message(FATAL_ERROR "Invalid BUILD_TYPE: ${CMP_BUILD_TYPE} for ${CMP}")
  endif()
  unset(CMP_BUILD_TYPE)
  unset(CMP_SOURCE_DIR)
  unset(CMP_CMAKE_CONFIG)
  unset(CMP_INSTALL_PREFIX)
  unset(CMP_INSTALL_LIBDIR)
  unset(CMP_INSTALL_CONFDIR)
  unset(CMP_INSTALL_INCLUDEDIR)
  unset(CMP_FORT_MODULE)
  unset(CMP_LIBRARY)
  unset(CMP_BUILD_ARGS)
endforeach()
