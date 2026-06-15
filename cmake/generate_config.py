#!/usr/bin/env python3
"""
generate_config.py
==================
Dynamically generate a standard CMake package configuration file (ESMFConfig.cmake)
from ESMF makefile fragment (esmf.mk).

It handles extracting include paths, linking flags, and conditionally
configuring downstream MPI dependencies based on the selected ESMF_COMM.
"""

import os
import sys
import argparse
import re
import platform

def parse_arguments():
    parser = argparse.ArgumentParser(description="Generate ESMFConfig.cmake and ESMFConfigVersion.cmake from an explicit esmf.mk.")
    parser.add_argument("--esmfmkfile", required=True, help="The esmf.mk file with full path.")
    parser.add_argument("--template", required=True, help="The ESMFConfig.cmake.in template with full path.")
    parser.add_argument("--outputdir", required=True, help="Destination directory for the generated CMake config files.")
    return parser.parse_args()

def main():
    args = parse_arguments()

    if not os.path.exists(args.esmfmkfile):
        print(f"Error: Specified esmf.mk file does not exist at: {args.esmfmkfile}", file=sys.stderr)
        sys.exit(1)

    # Read the esmf.mk file content
    esmf_vars = {}
    with open(args.esmfmkfile, "r") as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                key, val = line.split("=", 1)
                esmf_vars[key.strip()] = val.strip()

    # Extract values
    version = esmf_vars.get("ESMF_VERSION_STRING", "0.0.0").strip("'\"")
    git_str = esmf_vars.get("ESMF_VERSION_STRING_GIT", "unknown").strip("'\"")
    apps_dir = esmf_vars.get("ESMF_APPSDIR", "unknown").strip("'\"")
    libs_dir = esmf_vars.get("ESMF_LIBSDIR", "unknown").strip("'\"")
    esmx_dir = esmf_vars.get("ESMF_ESMXDIR", "unknown").strip("'\"")

    # Mirror legacy FindESMF.cmake tracking for downstream compatibility variables
    is_beta = "TRUE" if esmf_vars.get("ESMF_VERSION_BETASNAPSHOT", "").strip("'\"") == "T" else "FALSE"

    # Include compilation paths parsing
    esmf_inc = [x[2:] for x in esmf_vars.get("ESMF_F90COMPILEPATHS", "").split() if x.startswith("-I")]
    esmc_inc = [x[2:] for x in esmf_vars.get("ESMF_CCOMPILEPATHS", "").split() if x.startswith("-I")]

    # Determine if this build is MPI-dependent or an mpiuni stub layout
    cpp_flags = esmf_vars.get("ESMF_F90COMPILECPPFLAGS", "")
    if "-DESMF_COMM=mpiuni" in cpp_flags:
        is_mpi_build = "FALSE"
    else:
        is_mpi_build = "TRUE"

    # Consolidated linking strings
    esmf_libs = " ".join([esmf_vars.get(k, "") for k in ["ESMF_F90LINKRPATHS", "ESMF_F90ESMFLINKRPATHS", "ESMF_F90ESMFLINKPATHS", "ESMF_F90LINKPATHS", "ESMF_F90LINKLIBS", "ESMF_F90LINKOPTS"]]).strip()
    esmc_libs = " ".join([esmf_vars.get(k, "") for k in ["ESMF_CLINKRPATHS", "ESMF_CESMFLINKRPATHS", "ESMF_CESMFLINKPATHS", "ESMF_CLINKPATHS", "ESMF_CLINKLIBS", "ESMF_CLINKOPTS"]]).strip()

    # Prioritize shared library (.dylib or .so), with static archive (.a) fallback
    libs_dir = esmf_vars.get("ESMF_LIBSDIR", os.path.dirname(args.esmfmkfile))
    if platform.system() == "Darwin":
        lib_loc = os.path.join(libs_dir, "libesmf.dylib")
    else:
        lib_loc = os.path.join(libs_dir, "libesmf.so")
    if not os.path.exists(lib_loc):
        lib_loc = os.path.join(libs_dir, "libesmf.a")

    if not os.path.exists(args.template):
        print(f"Error: Template file not found at {args.template}", file=sys.stderr)
        sys.exit(1)
    with open(args.template, "r") as f:
        content = f.read()

    replacements = {
        "@PACKAGE_INIT@": "",
        "@ESMF_VERSION@": version,
        "@ESMF_VERSION_STRING_GIT@": git_str,
        "@ESMF_BETA_RELEASE@": is_beta,
        "@ESMF_HAS_MPI@": is_mpi_build,
        "@ESMF_APPSDIR@": apps_dir,
        "@ESMF_LIBSDIR@": libs_dir,
        "@ESMF_ESMXDIR@": esmx_dir,
        "@ESMF_INCLUDE_DIRECTORIES@": ";".join(esmf_inc),
        "@ESMC_INCLUDE_DIRECTORIES@": ";".join(esmc_inc),
        "@ESMF_INTERFACE_LINK_LIBRARIES@": esmf_libs,
        "@ESMC_INTERFACE_LINK_LIBRARIES@": esmc_libs,
        "@ESMF_LIBRARY_LOCATION@": lib_loc
    }
    for token, value in replacements.items():
        content = content.replace(token, value)

    # Outputdir
    out_dir = os.path.abspath(args.outputdir)
    os.makedirs(out_dir, exist_ok=True)

    # Write the main config file
    config_output = os.path.join(out_dir, "ESMFConfig.cmake")
    with open(config_output, "w") as f:
        f.write(content)
    print(f"Successfully generated config: {config_output} (MPI: {is_mpi_build}, Imported Target: {os.path.basename(lib_loc)})")

    # --------------------------------------------------------------------------
    # Generate the companion ESMFConfigVersion.cmake file inline
    # --------------------------------------------------------------------------
    version_output = os.path.join(out_dir, "ESMFConfigVersion.cmake")

    # Extract clean major.minor.patch version string (e.g., "9.0.0") from the full beta snapshot string
    numeric_match = re.search(r"([0-9]+\.[0-9]+\.[0-9]+)", version)
    numeric_version = numeric_match.group(1) if numeric_match else "9.0.0"

    version_file_content = f"""# Generated automatically by generate_config.py - Do not edit
set(PACKAGE_VERSION "{version}")

# Clean numeric representation for CMake version comparison logic
set(NUMERIC_PACKAGE_VERSION "{numeric_version}")

if(PACKAGE_FIND_VERSION)
    # Extract numeric tokens from requested package constraint
    string(REGEX MATCH "^([0-9]+\\.[0-9]+\\.[0-9]+)" NUMERIC_REQUEST "${{PACKAGE_FIND_VERSION}}")
    if(NOT NUMERIC_REQUEST)
        set(NUMERIC_REQUEST "${{PACKAGE_FIND_VERSION}}")
    endif()

    if(NUMERIC_REQUEST VERSION_LESS_EQUAL NUMERIC_PACKAGE_VERSION)
        set(PACKAGE_VERSION_COMPATIBLE TRUE)
        if(NUMERIC_REQUEST VERSION_EQUAL NUMERIC_PACKAGE_VERSION)
            set(PACKAGE_VERSION_EXACT TRUE)
        endif()
    else()
        set(PACKAGE_VERSION_COMPATIBLE FALSE)
    endif()
endif()
"""

    with open(version_output, "w") as f:
        f.write(version_file_content)
    print(f"Successfully generated version handler: {version_output}")

if __name__ == "__main__":
    main()
