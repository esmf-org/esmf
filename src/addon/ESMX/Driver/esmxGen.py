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
      data = yaml.safe_load(_file)
      if data is not None:
        _dict = dict({k.replace("-", "_"): v for k, v in data.items()})
      else:
        _dict = {}
      if _dict.get("components") is None:
        _dict['components'] = {}
      if _dict.get('tests') is None:
        _dict['tests'] = {}
      return _dict

def create_compList(_dict, odir):
    # open file
    with open(os.path.join(odir, 'compList.txt'), 'w') as f:
      if _dict['components'] is not None:
        # loop through components and create use statements
        od = collections.OrderedDict(_dict['components'].items())
        comp_str = [comp for comp in od.keys()]
        f.write('set(COMPS {})\n\n'.format(' '.join(comp_str)))
        for k1, v1 in od.items():
          if type(v1) is not dict:
            v1 = {'build_type': 'auto'}
          f.write('# - auto-generated section for component: {}\n'.format(k1))
          # determine component settings
          build_type = v1.get('build_type', 'auto')
          source_dir = v1.get('source_dir', k1)
          if source_dir:
            source_dir = source_dir.strip()
            if not source_dir.startswith('$'):
              source_dir = os.path.abspath(source_dir)
          cmake_config = v1.get('cmake_config', k1+'.cmake')
          install_prefix = v1.get('install_prefix', '')
          if install_prefix:
            install_prefix = install_prefix.strip()
            if not install_prefix.startswith('$'):
              install_prefix = os.path.abspath(install_prefix)
          config_dir = v1.get('config_dir', 'cmake')
          library_dir = v1.get('library_dir', 'lib')
          include_dir = v1.get('include_dir', 'include')
          fort_module = v1.get('fort_module', (k1+'.mod').lower())
          libraries = v1.get('libraries', k1)
          build_script = v1.get('build_script', 'build.sh')
          build_args = v1.get('build_args', '')
          link_libraries = v1.get('link_libraries', '')
          link_paths = v1.get('link_paths', '')
          git_repository = v1.get('git_repository', '')
          git_tag = v1.get('git_tag', '')
          git_dir = v1.get('git_dir', '')
          if git_dir:
            git_dir = git_dir.strip()
            if not git_dir.startswith('$'):
              git_dir = os.path.abspath(git_dir)
          test_dir = v1.get('test_dir', '')
          if test_dir:
            test_dir = test_dir.strip()
            if not test_dir.startswith('$'):
              test_dir = os.path.abspath(test_dir)
          # write component settings to file
          if (build_type):
            f.write('set({}-BUILD_TYPE     {})\n'.format(k1, build_type))
          if (source_dir):
            f.write('set({}-SOURCE_DIR     {})\n'.format(k1, source_dir))
          if (cmake_config):
            f.write('set({}-CMAKE_CONFIG   {})\n'.format(k1, cmake_config))
          if (install_prefix):
            f.write('set({}-INSTALL_PREFIX {})\n'.format(k1, install_prefix))
          if (library_dir):
            f.write('set({}-LIBRARY_DIR    {})\n'.format(k1, library_dir))
          if (config_dir):
            f.write('set({}-CONFIG_DIR     {})\n'.format(k1, config_dir))
          if (include_dir):
            f.write('set({}-INCLUDE_DIR    {})\n'.format(k1, include_dir))
          if (fort_module):
            f.write('set({}-FORT_MODULE    {})\n'.format(k1, fort_module))
          if (libraries):
            f.write('set({}-LIBRARIES      {})\n'.format(k1, libraries))
          if (build_script):
            f.write('set({}-BUILD_SCRIPT   {})\n'.format(k1, build_script))
          if (build_args):
            f.write('set({}-BUILD_ARGS     {})\n'.format(k1, build_args))
          if (git_repository):
            f.write('set({}-GIT_REPOSITORY {})\n'.format(k1, git_repository))
          if (git_tag):
            f.write('set({}-GIT_TAG        {})\n'.format(k1, git_tag))
          if (git_dir):
            f.write('set({}-GIT_DIR        {})\n'.format(k1, git_dir))
          if (link_libraries):
            f.write('set({}-LINK_LIBRARIES {})\n'.format(k1, link_libraries))
          if (link_paths):
            f.write('set({}-LINK_PATHS     {})\n'.format(k1, link_paths))
          if (test_dir):
            f.write('set({}-TEST_DIR       {})\n'.format(k1, test_dir))

def create_testList(_dict, odir):
    # open file
    with open(os.path.join(odir, 'testList.txt'), 'w') as f:
      if _dict['tests'] is not None:
        # loop through tests and create use statements
        od = collections.OrderedDict(_dict['tests'].items())
        test_str = [test for test in od.keys()]
        f.write('set(TESTS {})\n\n'.format(' '.join(test_str)))
        for k1, v1 in od.items():
          f.write('# - auto-generated section for test: {}\n'.format(k1))
          # determine test settings
          test_dir = v1.get('dir', '')
          if test_dir:
            test_dir = test_dir.strip()
            if not test_dir.startswith('$'):
              test_dir = os.path.abspath(test_dir)
          # write test settings to file
          if (test_dir):
            f.write('set({}-TEST_DIR       {})\n'.format(k1, test_dir))

def create_compUse(_dict, odir):
    # open file
    with open(os.path.join(odir, 'compUse.inc'), 'w') as f:
      if _dict['components'] is not None:
        # loop through components and create use statements
        od = collections.OrderedDict(_dict['components'].items())
        for k1, v1 in od.items():
          if type(v1) is not dict:
            v1 = {'build_type': 'auto'}
          fort_module = v1.get('fort_module', (k1+'.mod').lower())
          f.write('use {}, only: {}SS => SetServices, {}SV => SetVM\n'.format(Path(fort_module).stem, k1, k1))

def create_compDef(_dict, odir):
    # open file
    with open(os.path.join(odir, 'compDef.inc'), 'w') as f:
      if _dict['components'] is not None:
        # loop through components and create use statements
        i = 1
        od = collections.OrderedDict(_dict['components'].items())
        for k1, v1 in od.items():
          f.write('CompDef({})%ssPtr => {}SS\n'.format(i, k1))
          f.write('CompDef({})%svPtr => {}SV\n'.format(i, k1))
          f.write('CompDef({})%name = "{}"\n'.format(i, k1))
          i = i+1

def create_compCnt(_dict, odir):
    # open file
    with open(os.path.join(odir, 'compCnt.inc'), 'w') as f:
      if _dict['components'] is not None:
        f.write('integer, parameter :: componentDefCount = {}\n'.format(len(_dict['components'])))

def main(argv):

    # default value
    odir = '.'
    disable_esmx_comps = []
    esmx_comps = []

    # read input arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('--ifile' , help='Input driver yaml file', required=True)
    parser.add_argument('--odir'  , help='Output directory for generated code')
    parser.add_argument('--disable_esmx_comps' , help='List of ESMX components to disable')
    args = parser.parse_args()

    if args.ifile:
        ifile = args.ifile
    if args.odir:
        odir = args.odir
    if args.disable_esmx_comps:
        disable_esmx_comps = list(args.disable_esmx_comps.lower().split(","))

    # read driver configuration yaml file
    dict_drv = read_drv_yaml_file(ifile)

    # add default components
    if ('ESMX_Data' not in dict_drv['components'] and
        'esmx_data' not in disable_esmx_comps):
      dict_drv['components'].update({'ESMX_Data':{}})
      dict_drv['components'].get('ESMX_Data').update(
        {'build_type':'cmake'})
      dict_drv['components'].get('ESMX_Data').update(
        {'source_dir':'${ESMF_ESMXDIR}/Comps/ESMX_Data'})
      dict_drv['components'].get('ESMX_Data').update(
        {'test_dir':'${ESMF_ESMXDIR}/Tests/esmx-data'})

    # create compList.txt for CMake
    create_compList(dict_drv, odir)

    # create testList.txt for CMake
    create_testList(dict_drv, odir)

    # create compUse.inc
    create_compUse(dict_drv, odir)

    # create compCnt.inc
    create_compCnt(dict_drv, odir)

    # create compDef.inc
    create_compDef(dict_drv, odir)

if __name__== "__main__":
	main(sys.argv[1:])
