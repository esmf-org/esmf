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
include(FetchContent)

# Set -Dvar=value and execute add_subdirectory
function(esmx_add_subdirectory CMP_SOURCE_DIR CMP CMP_BUILD_ARGS)
  foreach(ARG IN ITEMS ${CMP_BUILD_ARGS})
    string(REGEX REPLACE "^-D" "" ARG ${ARG})
    string(FIND ${ARG} "=" EQPOS)
    string(SUBSTRING ${ARG} 0 ${EQPOS}-1 LARG)
    math(EXPR EQPOS "${EQPOS}+1")
    string(SUBSTRING ${ARG} ${EQPOS} -1 RARG)
    set(${LARG} ${RARG} CACHE STRING "${CMP} setting for ${LARG}" FORCE)
  endforeach()
  add_subdirectory(${CMP_SOURCE_DIR} ${CMP})
endfunction()

# Add components using build type
foreach(CMP IN ITEMS ${COMPS})
  # get component options
  set(CMP_BUILD_TYPE     "${${CMP}-BUILD_TYPE}")
  string(TOLOWER         "${CMP_BUILD_TYPE}" CMP_BUILD_TYPE)
  set(CMP_SOURCE_DIR     "${${CMP}-SOURCE_DIR}")
  set(CMP_GIT_REPOSITORY "${${CMP}-GIT_REPOSITORY}")
  set(CMP_GIT_TAG        "${${CMP}-GIT_TAG}")
  set(CMP_CMAKE_CONFIG   "${${CMP}-CMAKE_CONFIG}")
  set(CMP_INSTALL_PREFIX "${${CMP}-INSTALL_PREFIX}")
  set(CMP_LIBRARY_DIR    "${${CMP}-LIBRARY_DIR}")
  set(CMP_CONFIG_DIR     "${${CMP}-CONFIG_DIR}")
  set(CMP_INCLUDE_DIR    "${${CMP}-INCLUDE_DIR}")
  set(CMP_LIBRARIES      "${${CMP}-LIBRARIES}")
  string(REPLACE "," ";" "${${CMP}-LIBRARIES}" CMP_LIBRARIES)
  set(CMP_BUILD_ARGS     "${${CMP}-BUILD_ARGS}")
  if(NOT CMP_GIT_REPOSITORY STREQUAL "")
    message(STATUS "Fetching content ... ${CMP}")
    if(EXISTS ${CMP_SOURCE_DIR})
      message(FATAL_ERROR "Source directory already exists for ${CMP}")
    endif()
    FetchContent_Declare(${CMP}
      GIT_REPOSITORY ${CMP_GIT_REPOSITORY}
      GIT_TAG ${CMP_GIT_TAG}
      SOURCE_DIR ${CMP_SOURCE_DIR}
    )
    FetchContent_Populate(${CMP})
  endif()
  if(${CMP_BUILD_TYPE} STREQUAL "externalproject")
    message(STATUS "Adding ExternalProject ... ${CMP}")
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
      ${CMP_INSTALL_PREFIX}/${CMP_INCLUDE_DIR})
    foreach (CMP_LIBRARY IN ITEMS ${CMP_LIBRARIES})
      string(CONCAT CMP_LIBRARY "${CMAKE_STATIC_LIBRARY_PREFIX}"
                                "${CMP_LIBRARY}"
                                "${CMAKE_STATIC_LIBRARY_SUFFIX}")
      target_link_libraries(esmx_driver PUBLIC
        ${CMP_INSTALL_PREFIX}/${CMP_LIBRARY_DIR}/${CMP_LIBRARY})
    endforeach()
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
    esmx_add_subdirectory(${CMP_SOURCE_DIR} ${CMP} "${CMP_BUILD_ARGS}")
    target_include_directories(esmx_driver PUBLIC
      ${CMAKE_BINARY_DIR}/${CMP_INCLUDE_DIR})
    foreach (CMP_LIBRARY IN ITEMS ${CMP_LIBRARIES})
      add_dependencies(esmx_driver ${CMP_LIBRARY})
      target_link_libraries(esmx_driver PUBLIC ${CMP_LIBRARY})
    endforeach()
  elseif(${CMP_BUILD_TYPE} STREQUAL "preinstalled")
    message(STATUS "Adding Preinstalled ... ${CMP}")
    find_file(FND_CMAKE_CONFIG NAMES ${CMP_CMAKE_CONFIG}
      HINTS ${CMP_INSTALL_PREFIX}
      PATH_SUFFIXES ${CMP_CONFIG_DIR} "." "cmake" "config"
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
    foreach (CMP_LIBRARY IN ITEMS ${CMP_LIBRARIES})
      target_link_libraries(esmx_driver PUBLIC ${CMP_LIBRARY})
    endforeach()
  else()
    message(FATAL_ERROR "Invalid BUILD_TYPE: ${CMP_BUILD_TYPE} for ${CMP}")
  endif()
  unset(CMP_BUILD_TYPE)
  unset(CMP_SOURCE_DIR)
  unset(CMP_CMAKE_CONFIG)
  unset(CMP_INSTALL_PREFIX)
  unset(CMP_LIBRARY_DIR)
  unset(CMP_CONFIG_DIR)
  unset(CMP_INCLUDE_DIR)
  unset(CMP_LIBRARIES)
  unset(CMP_BUILD_ARGS)
endforeach()

