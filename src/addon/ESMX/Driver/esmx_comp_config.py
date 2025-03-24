import os
import sys
import argparse
from esmx_tools import *

def create_compList(cmpCfg: ESMXCmpCfg, odir):
    # open file
    with open(os.path.join(odir, 'compList.txt'), 'w') as f:
        # loop through components and create use statements
        f.write('set(COMPS {})\n\n'.format(' '.join(cmpCfg.list())))
        for cmp in cmpCfg.list():
            f.write('# - auto-generated section for component: {}\n'.format(cmp))
            cfg = cmpCfg.get_config(cmp)
            options = [ESMXOpt('build_type', 'none', str),
                ESMXOpt('source_dir', cmp, dir),
                ESMXOpt('cmake_config', str(cmp)+'.cmake', str),
                ESMXOpt('install_prefix', '', dir),
                ESMXOpt('config_dir', 'cmake', str),
                ESMXOpt('library_dir', 'lib', str),
                ESMXOpt('include_dir', 'include', str),
                ESMXOpt('fort_module', (str(cmp)+'.mod').lower(), str),
                ESMXOpt('libraries', str(cmp), str),
                ESMXOpt('build_script', '', str),
                ESMXOpt('build_args', '', str),
                ESMXOpt('link_libraries', '', str),
                ESMXOpt('link_paths', '', dir),
                ESMXOpt('link_into_app', 'True', str),
                ESMXOpt('git_repository', '', str),
                ESMXOpt('git_tag', '', str),
                ESMXOpt('git_dir', '', dir),
                ESMXOpt('test_dir', '', dir),
                ESMXOpt('test_exe', '', str),
                ESMXOpt('test_tasks', '', str)]
            for opt in options:
                val = cfg.get(opt.option, opt.default)
                if (val):
                    val = str(val)
                    val = val.replace(" ", ";")
                    val = val.replace(",", ";")
                    if (opt.ctype == dir):
                        dirs = list(val.split(";"))
                        for i in range(len(dirs)):
                            dirs[i] = dirs[i].strip()
                            if not dirs[i].startswith('$'):
                                dirs[i] = os.path.abspath(dirs[i])
                        val = ';'.join(dirs)
                f.write('set({}-{} {})\n'.format(cmp, opt.upper(), val))
                if opt.option == "link_into_app":
                    if not val:
                        cmpCfg.remove_ci(cmp)

def create_compUse(comps: ESMXCmpCfg, odir):
    # open file
    with open(os.path.join(odir, 'compUse.inc'), 'w') as f:
        # loop through components and create use statements
        for comp in comps.list():
            cfg = comps.get_config(comp)
            fort_module = cfg.get('fort_module', (comp+'.mod').lower())
            f.write('use {}, only: {}SS => SetServices, {}SV => SetVM\n'.format(Path(fort_module).stem, comp, comp))

def create_compDef(comps: ESMXCmpCfg, odir):
    # open file
    with open(os.path.join(odir, 'compDef.inc'), 'w') as f:
        # loop through components and create use statements
        i = 1
        for comp in comps.list():
            f.write('CompDef({})%ssPtr => {}SS\n'.format(i, comp))
            f.write('CompDef({})%svPtr => {}SV\n'.format(i, comp))
            f.write('CompDef({})%name = "{}"\n'.format(i, comp))
            i = i+1

def create_compCnt(comps: ESMXCmpCfg, odir):
    # open file
    with open(os.path.join(odir, 'compCnt.inc'), 'w') as f:
        f.write('integer, parameter :: componentDefCount = {}\n'.format(len(comps)))

def main(argv):

    # default value
    odir = '.'
    disable_comps = []

    # read input arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('--ifile' , help='Input driver yaml file', required=True)
    parser.add_argument('--odir'  , help='Output directory for generated code')
    parser.add_argument('--disable_comps' , help='List of components to disable')
    args = parser.parse_args()

    if args.ifile:
        ifile = args.ifile
    if args.odir:
        odir = args.odir
    if args.disable_comps:
        disable_comps = list(args.disable_comps.lower().split(","))

    # read driver configuration yaml file
    comps = ESMXCmpCfg(ifile)

    # add default components
    if 'ESMX_Data' not in comps:
        comps.update({'ESMX_Data':{}})
        comps.get('ESMX_Data').update(
            {'build_type':'cmake'})
        comps.get('ESMX_Data').update(
            {'source_dir':'${ESMF_ESMXDIR}/Comps/ESMX_Data'})
        comps.get('ESMX_Data').update(
            {'test_dir':'${ESMF_ESMXDIR}/Tests/esmx-data'})

    # remove disabled components
    for dis in disable_comps:
        comps.remove_ci(dis)

    # create compList.txt for CMake, and remove unlinked components from list
    create_compList(comps, odir)

    # create compUse.inc
    create_compUse(comps, odir)

    # create compCnt.inc
    create_compCnt(comps, odir)

    # create compDef.inc
    create_compDef(comps, odir)

if __name__ == "__main__":
    main(sys.argv[1:])
