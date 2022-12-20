# Where to look for Find<Package>.cmake files
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

# Find ESMF
find_package(ESMF 8.4.0 MODULE REQUIRED)

# Set compilers per ESMFMKFILE
set(CMAKE_CXX_COMPILER ${ESMF_CXXCOMPILER})
set(CMAKE_Fortran_COMPILER ${ESMF_F90COMPILER})
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
  set(CMP_BUILD_TYPE   "${${CMP}-BUILD_TYPE}")
  set(CMP_CMAKE_CONFIG "${${CMP}-CMAKE_CONFIG}")
  set(CMP_FORT_MODULE  "${${CMP}-FORT_MODULE}")
  set(CMP_LIBRARY      "${${CMP}-LIBRARY}")
  set(CMP_BUILD_SRC    "${${CMP}-BUILD_SRC}")
  set(CMP_BUILD_ARGS   "${${CMP}-BUILD_ARGS}")
  set(CMP_INSTALL_DIR  "${${CMP}-INSTALL_DIR}")
  set(CMP_EXPORT_DIR   "${${CMP}-EXPORT_DIR}")
  set(CMP_INCLUDE_DIR  "${${CMP}-INCLUDE_DIR}")
  set(CMP_LIBRARY_DIR  "${${CMP}-LIBRARY_DIR}")
  string(TOLOWER "${CMP_BUILD_TYPE}" CMP_BUILD_TYPE)
  if(${CMP_BUILD_TYPE} STREQUAL "external_project")
    message(STATUS "Building ExternalProject ... ${CMP}")
    find_file(FND_CMAKE_LISTS CMakeLists.txt
      PATHS ${CMP_BUILD_SRC}
      NO_DEFAULT_PATH
    )
    if(NOT FND_CMAKE_LISTS)
      message(FATAL_ERROR "Could not find CMakeLists.txt"
        " in ${CMP_BUILD_SRC}."
        " Check build_src in build configuration."
      )
    endif()
    ExternalProject_Add(${CMP}
      PREFIX ${CMAKE_CURRENT_BINARY_DIR}/${CMP}
      SOURCE_DIR ${CMP_BUILD_SRC}
      INSTALL_DIR ${CMP_INSTALL_DIR}
      CMAKE_ARGS
        -DCMAKE_INSTALL_PREFIX=${CMP_INSTALL_DIR}
        ${CMP_BUILD_ARGS}
      BUILD_ALWAYS YES
    )
    add_dependencies(esmx_driver ${CMP})
    target_include_directories(esmx_driver PUBLIC
      ${CMAKE_CURRENT_BINARY_DIR}/${CMP}/src/${CMP}-build/${CMP_INCLUDE_DIR})
    target_link_libraries(esmx_driver PUBLIC
      ${CMAKE_CURRENT_BINARY_DIR}/${CMP}/src/${CMP}-build/lib${CMP_LIBRARY}.a)
  elseif(${CMP_BUILD_TYPE} STREQUAL "subdirectory")
    message(STATUS "Adding Subdirectory ... ${CMP}")
    find_file(FND_CMAKE_LISTS CMakeLists.txt
      PATHS ${CMP_BUILD_SRC}
      NO_DEFAULT_PATH
    )
    if(NOT FND_CMAKE_LISTS)
      message(FATAL_ERROR "Could not find CMakeLists.txt"
        " in ${CMP_BUILD_SRC}."
        " Check build_src in build configuration."
      )
    endif()
    add_subdirectory(${CMP_BUILD_SRC} ${CMP})
    add_dependencies(esmx_driver ${CMP_LIBRARY})
    target_include_directories(esmx_driver PUBLIC
      ${CMAKE_CURRENT_BINARY_DIR}/${CMP}/${CMP_INCLUDE_DIR})
    target_link_libraries(esmx_driver PUBLIC ${CMP_LIBRARY})
  elseif(${CMP_BUILD_TYPE} STREQUAL "preinstalled")
    message(STATUS "Linking Preinstalled ... ${CMP}")
    find_file(FND_CMAKE_CONFIG ${CMP_CMAKE_CONFIG}
      PATHS ${CMP_INSTALL_DIR}
      PATH_SUFFIXES "${CMP_EXPORT_DIR}" "." "cmake"
      NO_DEFAULT_PATH
    )
    if(NOT FND_CMAKE_CONFIG)
      message(FATAL_ERROR "Could not find ${CMP_CMAKE_CONFIG}"
        " in ${CMP_INSTALL_DIR}/${CMP_EXPORT_DIR}."
        " Check installation or export_dir in build configuration."
      )
    endif()
    include(${FND_CMAKE_CONFIG})
    find_path(FND_FORT_MODULE ${CMP_FORT_MODULE}
      PATHS ${CMP_INSTALL_DIR}
      PATH_SUFFIXES "${CMP_INCLUDE_DIR}" "." "include" "mod"
      NO_DEFAULT_PATH
    )
    if(NOT FND_FORT_MODULE)
      message(FATAL_ERROR "Could not find ${CMP_FORT_MODULE}"
        " in ${CMP_INSTALL_DIR}/${CMP_INCLUDE_DIR}."
        " Check installation or include_dir in build configuration"
      )
    endif()
    target_include_directories(esmx_driver PUBLIC ${FND_FORT_MODULE})
    find_library(FND_LIBRARY ${CMP_LIBRARY}
      PATHS ${CMP_INSTALL_DIR}
      PATH_SUFFIXES "${CMP_LIBRARY_DIR}" "." "lib" "lib64"
      NO_DEFAULT_PATH
    )
    if(NOT FND_LIBRARY)
      message(FATAL_ERROR "Could not find lib${CMP_LIBRARY}.a"
        " in ${CMP_INSTALL_DIR}/${CMP_LIBRARY_DIR}."
        " Check installation or library_dir in build configuration"
      )
    endif()
    target_link_libraries(esmx_driver PUBLIC ${FND_LIBRARY})
  else()
    message(FATAL_ERROR "Invalid BUILD_TYPE: ${CMP_BUILD_TYPE} for ${CMP}")
  endif()
endforeach()
