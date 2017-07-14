include_directories( ${HDF5_INCLUDE_DIR} )

set(
  mhdf_LIB_SRCS
  include/mhdf.h
  src/adjacency.c
  src/connectivity.c
  src/file-desc.c
  src/file-handle.h  src/file-handle.c
  src/file.c
  src/names-and-paths.h
  src/nodes.c
  src/sets.c
  src/status.h src/status.c
  src/tags.c
  src/util.h src/util.c
  )

add_library( mhdf OBJECT ${mhdf_LIB_SRCS} )

add_executable( h5minfo include/mhdf.h example/info.c $<TARGET_OBJECTS:mhdf> )
target_link_libraries( h5minfo ${HDF5_LIBRARIES} )
install(
  TARGETS h5minfo
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  )

add_executable( h5mvalidate example/validate.c $<TARGET_OBJECTS:mhdf> )
install(
  TARGETS h5mvalidate
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  )
target_link_libraries( h5mvalidate ${HDF5_LIBRARIES} )
