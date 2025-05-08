import os
import sys
import argparse
from esmx_tools import *

def create_testList(tstCfg: ESMXTstCfg, odir):
    # open file
    with open(os.path.join(odir, 'testList.txt'), 'w') as f:
        # loop through components and create use statements
        f.write('set(TESTS {})\n\n'.format(' '.join(tstCfg.list())))
        for tst in tstCfg.list():
            f.write('# - auto-generated section for test: {}\n'.format(tst))
            cfg = tstCfg.get_config(tst)
            options = [ESMXOpt('dir', None, dir),
                       ESMXOpt('exe', None, str),
                       ESMXOpt('tasks', None, str)]
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
                    f.write('set({}-{} {})\n'.format(tst, opt.upper(), val))

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

    # read driver configuration yaml file
    tests = ESMXTstCfg(ifile)

    # create testList.txt for CMake
    create_testList(tests, odir)

if __name__ == "__main__":
    main(sys.argv[1:])
