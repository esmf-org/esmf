# $Id$

import os
import sys
from distutils.core import setup, Command
from distutils.util import get_platform
import subprocess


def download_test_data(func):
    def wrapper(self):
        update_system_path()
        from ESMF.util.cache_data import cache_data_files
        cache_data_files()
        return func(self)
    return wrapper


def update_system_path():
    if 'src' not in sys.path:
        sys.path.insert(0, 'src')


class AbstractESMFCommand(Command):
    user_options = []

    def initialize_options(self):
        self.cwd = None

    def finalize_options(self):
        self.cwd = os.getcwd()

    def _validate_(self):
        if os.getcwd() != self.cwd:
            raise RuntimeError('Must be in package root: %s' % self.cwd)


class AbstractESMFNoseCommand(AbstractESMFCommand):
    _nose_attrs = None
    _nose_base_attrs = ['!slow']
    _nose_parallel = False
    _nose_flags = None
    _default_target = os.path.join('src', 'ESMF')
    _needs_data = False

    def run(self):
        self._validate_()
        cmd = self.nosetests_command()
        subprocess.check_call(cmd)

    @classmethod
    def nosetests_command(cls):
        ret = ['nosetests', '-vs']
        if cls._nose_attrs is not None:
            nose_attrs = cls._nose_base_attrs + cls._nose_attrs
            nose_attrs = ','.join(nose_attrs)
            cmd_nose_attrs = ['-a', nose_attrs]
            ret = ret + cmd_nose_attrs

        if cls._nose_parallel:
            # Needed for ESMF contants import
            sys.path.append('src')

            from ESMF.api import constants
            mpirun_prefix = [constants._ESMF_MPIRUN, '-n', str(constants._ESMF_MPIRUN_NP)]
            ret = mpirun_prefix + ret

        if cls._nose_flags is not None:
            ret.append(cls._nose_flags)
        ret.append(cls._default_target)
        return ret


class BuildCommand(AbstractESMFCommand):
    description = "build: will build the ESMF package"
    user_options = [('ESMFMKFILE=', 'e',
                     "Location of esmf.mk for the ESMF installation")]

    def initialize_options(self):
        self.cwd = None
        self.ESMFMKFILE = None
        SITEDIR = os.system('%s -m site --user-site' % sys.executable)
        self.build_base = 'build'
        self.build_lib = None
        self.plat_name = None

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

        # Create "esmfmkfile.py" file holding the path to the ESMF "esmf.mk" file
        if self.ESMFMKFILE is not None:
            f = open(os.path.join('src', 'ESMF', 'interface', 'esmfmkfile.py'), 'w')
            f.write('ESMFMKFILE = "%s"' % self.ESMFMKFILE)
            f.close()

        # Attempt to load ESMF.
        update_system_path()
        import ESMF.interface.loadESMF


class CleanCommand(AbstractESMFCommand):
    description = "clean: will remove all libraries, log and output files"

    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in package root: %s' % self.cwd
        os.system('find . -name "*.pyc" -exec rm -f {} \;')
        os.system('find . -name "*ESMF_LogFile*" -exec rm -f {} \;')
        os.system('find . -name "*.log" -exec rm -f {} \;')
        os.system('find . -name "*.vtk" -exec rm -f {} \;')
        os.system('rm src/ESMF/interface/esmfmkfile.py')
        os.system('rm -rf build')
        os.system('rm -rf dist')
        os.system('rm -rf src/ESMF/test/regrid_from_file/data')
        os.system('rm -rf examples/data')


class DustCommand(AbstractESMFCommand):
    description = "dust: will remove log and output files"

    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in package root: %s' % self.cwd
        os.system('find . -name "*ESMF_LogFile*" -exec rm -f {} \;')
        os.system('find . -name "*.log" -exec rm -f {} \;')
        os.system('find . -name "*.vtk" -exec rm -f {} \;')
        os.system('find . -name "*.pyc" -exec rm -f {} \;')


class TestCommand(AbstractESMFNoseCommand):
    description = "run serial tests"
    _nose_attrs = ['!parallel']


class TestParallelCommand(AbstractESMFNoseCommand):
    description = "run parallel tests"
    _nose_attrs = ['!serial']
    _nose_parallel = True


class TestRegridCommand(AbstractESMFNoseCommand):
    description = "run test_regrid.py"
    _default_target = os.path.join('src', 'ESMF', 'test', 'test_api', 'test_regrid.py')


class TestRegridParallelCommand(TestRegridCommand):
    description = "test regrid parallel"
    _nose_attrs = ['!serial']
    _nose_parallel = True

class TestExamplesCommand(AbstractESMFNoseCommand):
    description = "run examples in serial"
    _nose_attrs = ['!parallel']
    _default_target = os.path.join('examples', 'exampletest.py')

    @download_test_data
    def run(self):
        update_system_path()
        AbstractESMFNoseCommand.run(self)

class TestExamplesParallelCommand(TestExamplesCommand):
    description = "run examples in parallel"
    _nose_attrs = ['!serial']
    _nose_parallel = True

class TestExamplesDryrunCommand(TestExamplesCommand):
    description = "collect example tests only and download data"
    _nose_flags = '--collect-only'


class TestRegridFromFileCommand(AbstractESMFCommand):
    description = "test regrid from file"
    _filename = 'run_regrid_from_file.py'
    _flags = None

    def run(self):
        original_pp = os.environ.get('PYTHONPATH', '')
        path = os.path.join(os.getcwd(), 'src')
        os.environ['PYTHONPATH'] = '{0}:{1}'.format(path, original_pp)
        self._validate_()
        target = os.path.join('src', 'ESMF', 'test', 'regrid_from_file', self._filename)
        cmd = [sys.executable, target]
        if self._flags is not None:
            cmd.append(self._flags)
        subprocess.check_call(cmd)


class TestRegridFromFileDryrunCommand(TestRegridFromFileCommand):
    description = "test regrid from file dryrun"
    _filename = 'run_regrid_from_file_dryrun.py'


class TestRegridFromFileParallelCommand(TestRegridFromFileCommand):
    description = "test regrid from file parallel"
    _filename = 'run_regrid_from_file.py'
    _flags = '--parallel'

    def run(self):
        TestRegridFromFileCommand.run(self)

class TestAllCommand(AbstractESMFCommand):
    description = "run serial, parallel, and example tests"

    @download_test_data
    def run(self):
        self._validate_()
        to_run = [TestCommand, TestParallelCommand, TestExamplesCommand, TestExamplesParallelCommand]
        for t in to_run:
            cmd = t.nosetests_command()
            subprocess.check_call(cmd)

# Get package structure
def _get_dot_(path, root='src'):
    ret = []
    path_parse = path
    while True:
        path_parse, tail = os.path.split(path_parse)
        if tail == root:
            break
        else:
            ret.append(tail)
    ret.reverse()
    return '.'.join(ret)


src_path = os.path.join('src', 'ESMF')
packages = []
for dirpath, dirnames, filenames in os.walk(src_path):
    if '__init__.py' in filenames:
        package = _get_dot_(dirpath)
        packages.append(package)

# TODO: build doc command
# TODO: remove duplicated metadata: here and src/ESMF/__init__.py
setup(name="ESMPy",
      version="8.0.0 beta",
      description="ESMF Python interface",
      author="University Corporation for Atmospheric Research, \
              Massachusetts Institute of Technology, \
              Geophysical Fluid Dynamics Laboratory, \
              University of Michigan, \
              National Centers for Environmental Prediction, \
              Los Alamos National Laboratory, \
              Argonne National Laboratory, \
              NASA Goddard Space Flight Center",
      license="University of Illinois-NCSA",
      author_email="esmf_support@list.woc.noaa.gov",
      url="http://earthsystemcog.org/projects/esmpy/",
      packages=packages,
      package_dir={'': 'src'},
      cmdclass={'build': BuildCommand,
                'clean': CleanCommand,
                'dust': DustCommand,
                'test': TestCommand,
                'test_all': TestAllCommand,
                'test_parallel': TestParallelCommand,
                'test_examples': TestExamplesCommand,
                'test_examples_dryrun': TestExamplesDryrunCommand,
                'test_examples_parallel': TestExamplesParallelCommand,
                'test_regrid': TestRegridCommand,
                'test_regrid': TestRegridCommand,
                'test_regrid_from_file': TestRegridFromFileCommand,
                'test_regrid_from_file_dryrun': TestRegridFromFileDryrunCommand,
                'test_regrid_parallel': TestRegridParallelCommand,
                'test_regrid_from_file_parallel': TestRegridFromFileParallelCommand})
