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
          config_dir = v1.get('config_dir', 'cmake')
          if (os.path.isabs(config_dir)):
            config_dir = os.path.basename(config_dir)
          library_dir = v1.get('library_dir', 'lib')
          if (os.path.isabs(library_dir)):
            library_dir = os.path.basename(library_dir)
          include_dir = v1.get('include_dir', 'include')
          if (os.path.isabs(include_dir)):
            include_dir = os.path.basename(include_dir)
          libraries = v1.get('libraries', k1)
          build_args = v1.get('build_args', None)
          git_repository = v1.get('git_repository', None)
          git_tag = v1.get('git_tag', None)
          f.write('set({}-BUILD_TYPE     {})\n'.format(k1, build_type))
          f.write('set({}-SOURCE_DIR     {})\n'.format(k1, source_dir))
          f.write('set({}-CMAKE_CONFIG   {})\n'.format(k1, cmake_config))
          f.write('set({}-INSTALL_PREFIX {})\n'.format(k1, install_prefix))
          f.write('set({}-LIBRARY_DIR    {})\n'.format(k1, library_dir))
          f.write('set({}-CONFIG_DIR     {})\n'.format(k1, config_dir))
          f.write('set({}-INCLUDE_DIR    {})\n'.format(k1, include_dir))
          f.write('set({}-LIBRARIES      {})\n'.format(k1, libraries))
          if (build_args):
            f.write('set({}-BUILD_ARGS         {})\n'.format(k1, build_args))
          if (git_repository):
            f.write('set({}-GIT_REPOSITORY     {})\n'.format(k1, git_repository))
          if (git_tag):
            f.write('set({}-GIT_TAG            {})\n'.format(k1, git_tag))

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
