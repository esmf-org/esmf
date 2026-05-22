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

def parse_arguments():
    parser = argparse.ArgumentParser(description="Generate ESMFConfig.cmake from an explicit esmf.mk.")
    parser.add_argument("--esmfmkfile", required=True, help="Exact absolute path to the esmf.mk file.")
    parser.add_argument("--template", required=True, help="Path to ESMFConfig.cmake.in input template.")
    parser.add_argument("--output", required=True, help="Full destination path for ESMFConfig.cmake.")
    return parser.parse_args()

def main():
    args = parse_arguments()

    if not os.path.exists(args.esmfmkfile):
        print(f"Error: Specified esmf.mk file does not exist at: {args.esmfmkfile}", file=sys.stderr)
        sys.exit(1)

    esmf_vars = {}
    with open(args.esmfmkfile, "r") as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                key, val = line.split("=", 1)
                esmf_vars[key.strip()] = val.strip()

    # Extract values
    version = esmf_vars.get("ESMF_VERSION_STRING", "0.0.0").strip("'\"")
    esmf_inc = [x.replace("-I", "") for x in esmf_vars.get("ESMF_F90COMPILEPATHS", "").split()]
    esmc_inc = [x.replace("-I", "") for x in esmf_vars.get("ESMF_CCOMPILEPATHS", "").split()]

    # Determine if this build is MPI-dependent or an mpiuni stub layout
    cpp_flags = esmf_vars.get("ESMF_F90COMPILECPPFLAGS", "")
    if "-DESMF_COMM=mpiuni" in cpp_flags:
        is_mpi_build = "FALSE"
    else:
        is_mpi_build = "TRUE"

    # Consolidated linking strings
    esmf_libs = " ".join([esmf_vars.get(k, "") for k in ["ESMF_F90LINKRPATHS", "ESMF_F90ESMFLINKRPATHS", "ESMF_F90ESMFLINKPATHS", "ESMF_F90LINKPATHS", "ESMF_F90LINKLIBS", "ESMF_F90LINKOPTS"]]).strip()
    esmc_libs = " ".join([esmf_vars.get(k, "") for k in ["ESMF_CLINKRPATHS", "ESMF_CESMFLINKRPATHS", "ESMF_CESMFLINKPATHS", "ESMF_CLINKPATHS", "ESMF_CLINKLIBS", "ESMF_CLINKOPTS"]]).strip()

    libs_dir = esmf_vars.get("ESMF_LIBSDIR", os.path.dirname(args.esmfmkfile))
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
        "@ESMF_HAS_MPI@": is_mpi_build,  # <-- Injects TRUE or FALSE dynamically
        "@ESMF_INCLUDE_DIRECTORIES@": ";".join(esmf_inc),
        "@ESMC_INCLUDE_DIRECTORIES@": ";".join(esmc_inc),
        "@ESMF_INTERFACE_LINK_LIBRARIES@": esmf_libs,
        "@ESMC_INTERFACE_LINK_LIBRARIES@": esmc_libs,
        "@ESMF_LIBRARY_LOCATION@": lib_loc
    }
    for token, value in replacements.items():
        content = content.replace(token, value)

    os.makedirs(os.path.dirname(os.path.abspath(args.output)), exist_ok=True)
    with open(args.output, "w") as f:
        f.write(content)

    print(f"Successfully generated: {args.output} (MPI Dependencies: {is_mpi_build})")

if __name__ == "__main__":
    main()

