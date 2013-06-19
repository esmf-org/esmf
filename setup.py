# $Id$

import sys
import os

# distutils
from distutils.core import setup, Command
from distutils.util import get_platform

class BuildCommand(Command):
    description = "build: will build the ESMF package"
    user_options = [('ESMFMKFILE=', 'e',
                      "Location of esmf.mk for the ESMF installation")]
    def initialize_options(self):
        self.cwd = None
        self.ESMFMKFILE = None
        SITEDIR = os.system('python -m site --user-site')
        self.build_base = 'build'
        self.build_lib=None
        self.plat_name=None
    def finalize_options(self):
        self.cwd = os.getcwd()
        if self.ESMFMKFILE is None:
          self.ESMFMKFILE = os.getenv('ESMFMKFILE')
        if self.build_lib is None:
          self.build_lib = os.path.join(self.build_base, 'lib')
        if self.plat_name is None:
          self.plat_name = get_platform()

    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in package root: %s' % self.cwd

        # create the esmfmkfile.py file holding the value of ESMFMKFILE
        if self.ESMFMKFILE is not None:
            f = open('src/ESMF/interface/esmfmkfile.py', 'w')
            f.write('ESMFMKFILE = "%s"' % self.ESMFMKFILE)
            f.close()

        # load ESMF
        sys.path.append('src/ESMF/api')
        sys.path.append('src/ESMF/interface')

        import loadESMF

class CleanCommand(Command):
    description = "clean: will remove all libraries, log and output files"
    user_options = []
    def initialize_options(self):
        self.cwd = None
    def finalize_options(self):
        self.cwd = os.getcwd()
    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in package root: %s' % self.cwd
        os.system('find . -name "*.pyc" -exec rm -f {} \;')
        os.system('find . -name "*ESMF_LogFile*" -exec rm -f {} \;')
        os.system('find . -name "*.log" -exec rm -f {} \;')
        os.system('find . -name "*.vtk" -exec rm -f {} \;')
        os.system('rm src/ESMF/interface/esmfmkfile.py')
        os.system('rm -rf build')
        os.system('rm -rf dist')
        os.system('rm -rf src/ESMF/test/regrid_test/regrid_from_file_test/data')

class DustCommand(Command):
    description = "dust: will remove log and output files"
    user_options = []
    def initialize_options(self):
        self.cwd = None
    def finalize_options(self):
        self.cwd = os.getcwd()
    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in package root: %s' % self.cwd
        os.system('find . -name "*ESMF_LogFile*" -exec rm -f {} \;')
        os.system('find . -name "*.log" -exec rm -f {} \;')
        os.system('find . -name "*.vtk" -exec rm -f {} \;')

class TestCommand(Command):
    description = "test"
    user_options = []
    def initialize_options(self):
        self.cwd = None
    def finalize_options(self):
        self.cwd = os.getcwd()
    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in package root: %s' % self.cwd
        os.system('python src/ESMF/test/run_unit_test.py')

class TestRegridCommand(Command):
    description = "test"
    user_options = []
    def initialize_options(self):
        self.cwd = None
    def finalize_options(self):
        self.cwd = os.getcwd()
    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in package root: %s' % self.cwd
        os.system('python src/ESMF/test/regrid_test/run_regrid.py')

class TestRegridFromFileCommand(Command):
    description = "test"
    user_options = []
    def initialize_options(self):
        self.cwd = None
    def finalize_options(self):
        self.cwd = os.getcwd()
    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in package root: %s' % self.cwd
        os.system('python src/ESMF/test/regrid_test/regrid_from_file_test/run_regrid_from_file.py')

class TestRegridFromFileDryrunCommand(Command):
    description = "test"
    user_options = []
    def initialize_options(self):
        self.cwd = None
    def finalize_options(self):
        self.cwd = os.getcwd()
    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in package root: %s' % self.cwd
        os.system('python src/ESMF/test/regrid_test/regrid_from_file_test/run_regrid_from_file.py --dryrun')

class TestAllCommand(Command):
    description = "test all"
    user_options = []
    def initialize_options(self):
        self.cwd = None
    def finalize_options(self):
        self.cwd = os.getcwd()
    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in package root: %s' % self.cwd
        os.system('python src/ESMF/test/run_unit_test.py')
        os.system('python src/ESMF/test/regrid_test/run_regrid.py')
        os.system('python src/ESMF/test/regrid_test/regrid_from_file_test/run_regrid_from_file.py')

## get package structure
def _get_dot_(path,root='src'):
    ret = []
    path_parse = path
    while True:
        path_parse,tail = os.path.split(path_parse)
        if tail == root:
            break
        else:
            ret.append(tail)
    ret.reverse()
    return('.'.join(ret))
package_dir = {'':'src'}
src_path = os.path.join(package_dir.keys()[0],package_dir.values()[0],'ESMF')
packages = []
for dirpath,dirnames,filenames in os.walk(src_path):
    if '__init__.py' in filenames:
        package = _get_dot_(dirpath)
        packages.append(package)

# TODO: remove duplicated metadata: here and src/ESMF/__init__.py
setup(\
      name="ESMPy",
      version="620_01b",
      description="ESMF Python interface",
      author="University Corporation for Atmospheric Research, \
              Massachusetts Institute of Technology, \
              Geophysical Fluid Dynamics Laboratory, \
              University of Michigan, \
              National Centers for Environmental Prediction, \
              Los Alamos National Laboratory, \
              Argonne National Laboratory, \
              NASA Goddard Space Flight Center",
      license = "University of Illinois-NCSA",
      author_email="esmf_support@list.woc.noaa.gov",
      url="http://earthsystemcog.org/projects/esmpy/",
      packages=packages,
      package_dir = {'':'src'},
      cmdclass={'build': BuildCommand,
                'clean': CleanCommand,
                'dust': DustCommand,
                'test': TestCommand,
                'test_regrid': TestRegridCommand,
                'test_regrid_from_file': TestRegridFromFileCommand,
                #'test_regrid_from_file_dryrun': TestRegridFromFileDryrunCommand,
                'test_all': TestAllCommand}
     )