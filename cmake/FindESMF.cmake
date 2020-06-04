# - Try to find ESMF
#
# Requires setting ESMFMKFILE to the filepath of esmf.mk. If this is NOT set,
# then ESMF_FOUND will always be FALSE. If ESMFMKFILE exists, then ESMF_FOUND=TRUE
# and all ESMF makefile variables will be set in the global scope. Optionally,
# set ESMF_MKGLOBALS to a string list to filter makefile variables. For example,
# to globally scope only ESMF_LIBSDIR and ESMF_APPSDIR variables, use this CMake
# command in CMakeLists.txt:
#
#   set(ESMF_MKGLOBALS "LIBSDIR" "APPSDIR")


# Add the ESMFMKFILE path to the cache if defined as system env variable
if (DEFINED ENV{ESMFMKFILE} AND NOT DEFINED ESMFMKFILE)
    set(ESMFMKFILE $ENV{ESMFMKFILE} CACHE FILEPATH "Path to ESMF mk file")
endif ()

# Found the mk file and ESMF exists on the system
if (EXISTS ${ESMFMKFILE})
    set(ESMF_FOUND TRUE CACHE BOOL "ESMF mk file found" FORCE)
    # Did not find the ESMF mk file
else()
    set(ESMF_FOUND FALSE CACHE BOOL "ESMF mk file NOT found" FORCE)
    # Best to warn users that without the mk file there is no way to find ESMF
    if (NOT DEFINED ESMFMKFILE)
        message(WARNING "ESMFMKFILE not defined. This is the path to esmf.mk file. Without this filepath, ESMF_FOUND will always be FALSE.")
    endif ()
endif()

# Only parse the mk file if it is found
if (ESMF_FOUND)
    # Read the mk file
    file(STRINGS "${ESMFMKFILE}" esmfmkfile_contents)
    # Parse each line in the mk file
    foreach(str ${esmfmkfile_contents})
        # Only consider uncommented lines
        string(REGEX MATCH "^[^#]" def ${str})
        # Line is not commented
        if (def)
            # Extract the variable name
            string(REGEX MATCH "^[^=]+" esmf_varname ${str})
            # Extract the variable's value
            string(REGEX MATCH "=.+$" esmf_vardef ${str})
            # Only for variables with a defined value
            if (esmf_vardef)
                # Get rid of the assignment string
                string(SUBSTRING ${esmf_vardef} 1 -1 esmf_vardef)
                # Remove whitespace
                string(STRIP ${esmf_vardef} esmf_vardef)
                # A string or single-valued list
                if(NOT DEFINED ESMF_MKGLOBALS)
                    # Set in global scope
                    set(${esmf_varname} ${esmf_vardef})
                    # Don't display by default in GUI
                    mark_as_advanced(esmf_varname)
                else() # Need to filter global promotion
                    foreach(m ${ESMF_MKGLOBALS})
                        string(FIND ${esmf_varname} ${m} match)
                        # Found the string
                        if(NOT ${match} EQUAL -1)
                            # Promote to global scope
                            set(${esmf_varname} ${esmf_vardef})
                            # Don't display by default in the GUI
                            mark_as_advanced (esmf_varname)
                            # No need to search for the current string filter
                            break()
                        endif()
                    endforeach()
                endif()
            endif()
        endif()
    endforeach()
endif()

# Creates a CMake target so that users can use it natively
add_library(ESMF UNKNOWN IMPORTED)

# Find the ESMF library
find_library(ESMFLIB NAMES esmf PATHS ${ESMF_LIBSDIR})

# Prepare the compile options list
string(REPLACE " " ";" COMPILEOPTS "${ESMF_F90COMPILEOPTS} ${ESMF_F90COMPILEPATHS} ${ESMF_F90COMPILEFREECPP} ${ESMF_F90COMPILECPPFLAGS}")

# Set appropriate COMPILE and LINK options
set_target_properties(ESMF PROPERTIES
       IMPORTED_LOCATION ${ESMFLIB}
       INTERFACE_COMPILE_OPTIONS "${COMPILEOPTS}"
       INTERFACE_LINK_LIBRARIES "${ESMF_F90LINKOPTS} ${ESMF_F90LINKPATHS} ${ESMF_F90LINKRPATHS} ${ESMF_F90ESMFLINKLIBS}")

