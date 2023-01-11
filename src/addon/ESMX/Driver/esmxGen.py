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
          if type(v1) is not dict:
            v1 = {'build_type': 'preinstalled'}
          f.write('# - auto-generated section for component: {}\n'.format(k1))
          build_type = v1.get('build_type', 'preinstalled')
          source_dir = v1.get('source_dir', k1)
          source_dir = os.path.abspath(source_dir)
          cmake_config = v1.get('cmake_config', k1+'.cmake')
          if (cmake_config == os.path.basename(cmake_config)):
            install_prefix = v1.get('install_prefix', 'install')
          else:
            install_prefix = v1.get('install_prefix', os.path.dirname(cmake_config))
          cmake_config = os.path.basename(cmake_config)
          install_prefix = os.path.abspath(install_prefix)
          install_confdir = v1.get('install_confdir', 'cmake')
          if (os.path.isabs(install_confdir)):
            install_confdir = os.path.basename(install_confdir)
          install_libdir = v1.get('install_libdir', 'lib')
          if (os.path.isabs(install_libdir)):
            install_libdir = os.path.basename(install_libdir)
          install_includedir = v1.get('install_includedir', '')
          if (os.path.isabs(install_includedir)):
            install_includedir = os.path.basename(install_includedir)
          fort_module = v1.get('fort_module', (k1+'.mod').lower())
          libraries = v1.get('libraries', k1)
          build_args = v1.get('build_args', None)
          f.write('set({}-BUILD_TYPE   {})\n'.format(k1, build_type))
          f.write('set({}-SOURCE_DIR   {})\n'.format(k1, source_dir))
          f.write('set({}-CMAKE_CONFIG {})\n'.format(k1, cmake_config))
          f.write('set({}-INSTALL_PREFIX {})\n'.format(k1, install_prefix))
          f.write('set({}-INSTALL_LIBDIR     {})\n'.format(k1, install_libdir))
          f.write('set({}-INSTALL_CONFDIR    {})\n'.format(k1, install_confdir))
          f.write('set({}-INSTALL_INCLUDEDIR {})\n'.format(k1, install_includedir))
          f.write('set({}-FORT_MODULE  {})\n'.format(k1, fort_module))
          f.write('set({}-LIBRARIES    {})\n'.format(k1, libraries))
          if (build_args):
            f.write('set({}-BUILD_ARGS   {})\n'.format(k1, build_args))

def create_compUse(_dict, odir):
    # open file
    with open(os.path.join(odir, 'compUse.inc'), 'w') as f:
        # loop through components and create use statements
        od = collections.OrderedDict(_dict['components'].items())
        for k1, v1 in od.items():
          if type(v1) is not dict:
            v1 = {'build_type': 'preinstalled'}
          fort_module = v1.get('fort_module', (k1+'.mod').lower())
          f.write('use {}, only: {}SS => SetServices, {}SV => SetVM\n'.format(Path(fort_module).stem, k1, k1))

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
