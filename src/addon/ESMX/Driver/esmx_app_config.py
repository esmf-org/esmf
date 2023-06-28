import os
import sys
import argparse
from esmx_tools import *

def create_appConf(appCfg: ESMXAppCfg, odir):
    options = [ESMXOpt('disable_comps', None, str),
               ESMXOpt('link_module_paths', None, dir),
               ESMXOpt('link_paths', None, dir),
               ESMXOpt('link_libraries', None, str),
               ESMXOpt('link_packages', None, str),
               ESMXOpt('build_args', None, str),
               ESMXOpt('build_jobs', None, str),
               ESMXOpt('build_verbose', None, str),
               ESMXOpt('cmake_build_args', None, str),
               ESMXOpt('cmake_build_jobs', None, str),
               ESMXOpt('cmake_build_verbose', None, str),
               ESMXOpt('make_build_args', None, str),
               ESMXOpt('make_build_jobs', None, str),
               ESMXOpt('script_build_args', None, str),
               ESMXOpt('test', None, str),
               ESMXOpt('test_exe', None, str),
               ESMXOpt('test_dir', None, dir),
               ESMXOpt('test_tasks', None, str)]
    # open file
    with open(os.path.join(odir, 'appConf.txt'), 'w') as f:
        for opt in options:
            val = appCfg.get(opt.option, opt.default)
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
                f.write('set(ESMX_{} {})\n'.format(opt.upper(), val))

def main(argv):
    # default value
    odir = '.'

    # read input arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('--ifile' , help='Input driver yaml file', required=True)
    parser.add_argument('--odir'  , help='Output directory for generated code')

    args = parser.parse_args()

    if args.ifile:
        ifile = args.ifile
    if args.odir:
        odir = args.odir

    # read app configuration yaml file
    appCfg = ESMXAppCfg(ifile)

    # create appConf.txt for CMake
    create_appConf(appCfg, odir)

if __name__ == "__main__":
    main(sys.argv[1:])
