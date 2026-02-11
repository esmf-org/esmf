#!/usr/bin/env python3
import os
import re
import argparse
from datetime import datetime

# Regex to match copyright years (e.g., 2002-2025, 2025, 2002-2026)
FORMAT_REGEX = re.compile(r"Copyright \(c\) (\d{4})-(\d{4})(?:,)? University Corporation for Atmospheric Research")
COPYRIGHT_REGEX = re.compile(r"(?i)copyright")
DATE_REGEX = re.compile(r"\s*(?:\(c\)\s*|=\s*(?:u\')?)?(\d{4})(?:-(\d{4}))?")
NLDATE_REGEX = re.compile(r"^\s*(?:\!|\%|\/\/|\*)?\s*(?:\(c\)\s*|=\s*(?:u\')?|u\')?(\d{4})(?:-(\d{4}))?")

# List of root directories to skip
SKIP_ROOTDIRS = ['.git', 'obj', 'mod', 'lib']

# List of file extensions to skip
SKIP_EXTENSIONS = ['.vsd', '.eps']

# List of directories to skip
SKIP_DIRS = [
    os.path.normpath('src/Infrastructure/Mesh/src/Zoltan'),
    os.path.normpath('src/Infrastructure/Mesh/src/Lapack'),
    os.path.normpath('src/Infrastructure/Mesh/src/Moab'),
    os.path.normpath('src/Infrastructure/Base/include/nlohmann'),
    os.path.normpath('src/prologue/yaml-cpp'),
    os.path.normpath('src/Infrastructure/IO/PIO'),
    os.path.normpath('src/Infrastructure/Mesh/include/sacado'),
    os.path.normpath('src/addon/esmpy/src/esmpy/util/enum'),
]

# List of specific files to skip
SKIP_FILES = [
    os.path.normpath('scripts/maintain_copyrights.py'),
    os.path.normpath('src/doc/ESMF_quickstart.tex'),
    os.path.normpath('src/Infrastructure/Trace/include/ESMCI_HashNode.h'),
    os.path.normpath('src/Infrastructure/Trace/include/ESMCI_HashMap.h'),
    os.path.normpath('src/Infrastructure/Trace/include/ESMCI_KeyHash.h'),
    os.path.normpath('src/Infrastructure/Trace/include/esmftrc.h'),
    os.path.normpath('src/Infrastructure/Trace/include/metadata'),
    os.path.normpath('src/Infrastructure/Trace/include/esmftrc-bitfield.h'),
    os.path.normpath('src/Infrastructure/Trace/src/esmftrc.c'),
    os.path.normpath('src/Infrastructure/Util/include/dmp_diff.hpp'),
]

# List of copyright line matches to skip
SKIP_MATCHES = [
  "HashMap Development Team",
  "Philippe Proulx",
  "Mathieu Desnoyers",
  "The diff-match-patch Authors",
  "Victor Grishchenko",
]

def print_exclusions():
    """
    Print the lists of excluded file extensions, directories, files, and line
    matches.
    """
    print("### Exclusions")
    print("\nExclude file extensions:")
    for d in SKIP_EXTENSIONS:
        print("- " + d)
    print("\nExclude directories:")
    for d in SKIP_DIRS:
        print("- " + d)
    print("\nExclude files:")
    for f in SKIP_FILES:
        print("- " + f)
    print("\nExclude lines matching:")
    for m in SKIP_MATCHES:
        print("- " + m)
    print("")

def find_outdated_copyrights(search_year, loutdated=False):
    """
    Find all copyright years in the codebase that are older than the given
    search year.
    """
    outdated = []
    for root, dirs, files in os.walk('.'):
        # Skip root directories
        if any(skip in root for skip in SKIP_ROOTDIRS):
            continue
        # Skip specified directories
        rel_root = os.path.normpath(os.path.relpath(root, '.'))
        if any(rel_root.startswith(skip_dir) for skip_dir in SKIP_DIRS):
            continue
        for fname in files:
            # Skip files with extensions in SKIP_EXTENSIONS
            if any(fname.lower().endswith(ext) for ext in SKIP_EXTENSIONS):
                continue
            path = os.path.join(root, fname)
            # Skip specific files
            rel_path = os.path.normpath(os.path.relpath(path, '.'))
            if rel_path in SKIP_FILES:
                continue
            try:
                with open(path, 'r', encoding='utf-8') as f:
                    lines = f.readlines()
                for i in range(len(lines)):
                    line = lines[i]
                    nline = lines[i+1] if i < len(lines)-1 else ""
                    cmatch = COPYRIGHT_REGEX.search(line)
                    if cmatch:
                        # Skip lines matching SKIP_MATCHES
                        if any(skip_str in line for skip_str in SKIP_MATCHES):
                            continue
                        dmatch = DATE_REGEX.search(line, pos=cmatch.end())
                        if dmatch:
                            fwarn = not(FORMAT_REGEX.search(line))
                            syear = int(dmatch.group(1))
                            eyear = int(dmatch.group(2)) if dmatch.group(2) else syear
                            pos = dmatch.end()-3
                            if eyear != search_year:
                                outdated.append((path, i+1, pos, eyear, bool(fwarn)))
                        else:
                            nmatch = NLDATE_REGEX.search(nline)
                            if nmatch:
                                fwarn = not(FORMAT_REGEX.search(nline))
                                syear = int(nmatch.group(1))
                                eyear = int(nmatch.group(2)) if nmatch.group(2) else syear
                                pos = nmatch.end()-3
                                if eyear != search_year:
                                    outdated.append((path, i+2, pos, eyear, bool(fwarn)))
            except UnicodeDecodeError:
                # Skip files that cannot be decoded as UTF-8
                continue
    if loutdated:
        print_outdated_copyrights(outdated)
    return outdated

def print_outdated_copyrights(outdated_list):
    """
    Print each outdated copyright year in the outdated list as a markdown table.
    """
    if outdated_list:
        print("### Outdated Copyright Years")
        print("\n| file:line:position | year | warning |")
        print("|--------------------|------|---------|")
        for file, line, pos, year, fwarn in outdated_list:
            if fwarn:
                print(f"| {file}:{line}:{pos} | {year} | ! |")
            else:
                print(f"| {file}:{line}:{pos} | {year} |   |")
        print("")

def format_warnings(outdated_list):
    """
    Print warnings for copyright years that may not be in the correct format.
    """
    fwarnings = []
    if outdated_list:
        for file, line, pos, year, fwarn in outdated_list:
            if fwarn:
                fwarnings.append((file, line, pos, year))
    if fwarnings:
        print("### Format Warnings")
        print("\nCheck Copyright Format: "
              " \n Copyright (c) YYYY-YYYY, University Corporation for"
              " Atmospheric Research")
        print("\n| file:line:position | year | warning |")
        print("|--------------------|------|---------|")
        for file, line, pos, year in fwarnings:
            print(f"|{file}:{line}:{pos} | {year} | ! |")
        print("")
    return fwarnings

def update_copyrights(update_list, new_year, add=False, lupdated=False):
    """
    For each copyright year in update list, replace the old
    copyright year with the new copyright year.
    """
    updated = []
    if update_list:
        for file, line, pos, year, fwarn in update_list:
            with open(file, 'r', newline="") as f:
                lines = f.readlines()
            oldline = lines[line-1]
            newline = oldline[:pos-1] + str(new_year) + oldline[pos-1+len(str(year)):]
            lines[line-1] = newline
            with open(file, 'w') as f:
                f.writelines(lines)
            updated.append((file, line, pos, year, new_year, fwarn))
            if add:
                os.system(f"git add {file}")
    if lupdated:
        print_updated_copyrights(updated)
    return updated

def print_updated_copyrights(updated_list):
    """
    Print each updated copyright year in the updated list as a markdown table.
    """
    if updated_list:
        print("### Updated Copyright Years")
        print("\n| file:line:position | old year | new year | warning |")
        print("|--------------------|----------|----------|---------|")
        for file, line, pos, oyear, nyear, ewarn in updated_list:
            if ewarn:
                print(f"{file}:{line}:{pos} | {oyear} | {nyear} | ! |")
            else:
                print(f"{file}:{line}:{pos} | {oyear} | {nyear} |   |")
        print("")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="check and update copyright years.")
    parser.add_argument('-y', '--year', type=int, default=datetime.now().year, help='year to check copyright against (default: current year)')
    parser.add_argument('-l', '--list', action='store_true', default=False, help='list outdated copyright years')
    parser.add_argument('-w', '--warnings', action='store_true', default=False, help='list warnings for updating copyright years')
    parser.add_argument('-u', '--update', action='store_true', default=False, help='update outdated copyright years')
    parser.add_argument('-a', '--add', action='store_true', default=False, help='run git add on updated files')
    parser.add_argument('-x', '--exclusions', action='store_true', default=False, help='list excluded extensions, directories, files, and patterns')

    args = parser.parse_args()

    if args.exclusions:
        print_exclusions()
    outdated = find_outdated_copyrights(args.year,
        (args.list and not args.update))
    if args.update:
        updated = update_copyrights(outdated, args.year, args.add, args.list)
    else:
        updated = []
    if args.warnings:
        fwarnings = format_warnings(outdated)
    else:
        fwarnings = []

    print(f"Total outdated copyright years: {len(outdated)}")
    if args.update:
        print(f"Total updated copyright years: {len(updated)}")
    if args.warnings:
        print(f"Total warnings: {len(fwarnings)}")
