try:
    from yaml import CDumper as Dumper
    from yaml import CLoader as Loader
except ImportError:
    from yaml import Loader, Dumper

from pathlib import Path
import os
import sys
import yaml
import argparse
import collections

def read_drv_yaml_file(file_path):
    # open yaml file and read it
    if not os.path.exists(file_path):
        sys.exit('File not found: {}'.format(file_path))
    with open(file_path) as _file:
        data = yaml.load(_file, Loader=Loader)
        return dict({k.replace("-", "_"): v for k, v in data.items()})

def create_compList(_dict, odir):
    # open file
    with open(os.path.join(odir, 'compList.txt'), 'w') as f:
        # loop through components and create use statements
        od = collections.OrderedDict(_dict['components'].items())
        comp_str = [comp for comp in od.keys()]
        f.write('set(COMPS {})\n\n'.format(' '.join(comp_str)))
        for k1, v1 in od.items():
          if type(v1) is dict:
            cmake_config = v1.get('cmake_config', None)
            cmake_target = v1.get('cmake_target', None)
            f.write('# - auto-generated section for component: {}\n'.format(k1))
            if (cmake_config): # include cmake_config for component
              configFile = os.path.join(os.getcwd(), v1['cmake_config'])
              f.write('include({})\n\n'.format(configFile))
            if (cmake_target): # link library for component
              f.write('target_link_libraries(esmx_driver PUBLIC {})\n'.format(cmake_target))
            else:
              sys.exit('Build configuration is missing cmake_target for: {}'.format(k1))
          else:
            sys.exit('Build configuration is missing arguments for: {}'.format(k1))

def create_compUse(_dict, odir):
    # open file
    with open(os.path.join(odir, 'compUse.inc'), 'w') as f:
        # loop through components and create use statements
        od = collections.OrderedDict(_dict['components'].items())
        for k1, v1 in od.items():
          if type(v1) is dict:
            cmake_config = v1.get('cmake_config', None)
            fort_module = v1.get('fort_module', None)
            if (fort_module): # if fort_module field present, use it to identify fortran module
              f.write('use {}, only: {}SS => SetServices, {}SV => SetVM\n'.format(fort_module, k1, k1))
            elif (cmake_config): # otherwise use step of the cmake_config name
              f.write('use {}, only: {}SS => SetServices, {}SV => SetVM\n'.format(Path(cmake_config).stem, k1, k1))
            else:
              sys.exit('Build configuration is missing fort_module for: {}'.format(k1))
          else:
            sys.exit('Build configuration is missing arguments for: {}'.format(k1))

def create_compDef(_dict, odir):
    # open file
    with open(os.path.join(odir, 'compDef.inc'), 'w') as f:
        # loop through components and create use statements
        i = 1
        od = collections.OrderedDict(_dict['components'].items())
        for k1, v1 in od.items():
          f.write('CompDef({})%ssPtr => {}SS\n'.format(i, k1))
          f.write('CompDef({})%svPtr => {}SV\n'.format(i, k1))
          f.write('CompDef({})%name = "{}"\n'.format(i, k1))
          i = i+1

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
    dict_drv = read_drv_yaml_file(ifile)

    # create compList.txt for CMake
    create_compList(dict_drv, odir)

    # create compUse.inc
    create_compUse(dict_drv, odir)

    # create compDef.inc
    create_compDef(dict_drv, odir)

if __name__== "__main__":
	main(sys.argv[1:])
