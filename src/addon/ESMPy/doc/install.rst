============
Installation
============

------------
Requirements
------------

The following packages are *required* to work with ESMPy:

* `ESMF installation <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_usrdoc/node9.html>`_
* `python <http://python.org/>`_
* `numpy <http://www.numpy.org/>`_

The following packages are *optional*:

* `mpi4py <http://mpi4py.scipy.org/>`_- python bindings to MPI, needed to run the parallel regridding tests
* ESMF installation with NetCDF - required to create grids and meshes from file
    - NetCDF must be built as a shared library for ESMPy installation to succeed
* `nose <https://nose.readthedocs.org/en/latest/>`_ - for nose testing

----------------
Getting the code
----------------

The `ESMF User's Guide <http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_usrdoc/>`_
contains information on building and installing ESMF.
The `ESMF Reference Manual <http://www.earthsystemmodeling.org/esmf_releases/last/ESMF_refdoc/>`_
contains information on the architecture of ESMF, example code, and details of the API (Application Programming
Interface).

Instructions on how to download the ESMPy code can be found at the `ESMPy Download page
<http://www.earthsystemcog.org/projects/esmpy/releases>`_.

----------------
Installing ESMPy
----------------

Installation of ESMPy requires a pointer to a file named esmf.mk inside of an
ESMF installation.  This file resides in a directory which looks like:

<ESMF_INSTALL_DIR>/lib/lib<g<or>O>/<platform>/esmf.mk

If the ESMFMKFILE flag is set when building ESMPy then it will not need to be
referenced again.  If not, an environment variable of the same name must be set
with the path to the esmf.mk file EVERY time that a new shell is initiated.

The ESMPy build can be installed in a custom location using the
--prefix, --home, or --install-base flags to the install command.  If this
is done, then this location needs to be added to the PYTHONPATH environment
variable in the user's shell EVERY time that a new shell is initiated.  If a
customized install location is not specified, ESMPy will be installed in the
standard Python package installation directory on that particular machine.

Note: The ESMPy build does not have to be installed to be used.  The
PYTHONPATH environment variable can simply be pointed to the directory
containing the ESMF module (esmf/src/addon/ESMPy/src from a default git clone)
after the build command.

As usual, any command followed by --help should print out some information
on what options are available.

An installation of ESMPy in the default location for Python packages can be done
with the following command issued from the top level ESMPy directory:

- default Python package installation:

    python setup.py build --ESMFMKFILE=<DIR_TO_esmf.mk> install

- custom install location:

    python setup.py build --ESMFMKFILE=<DIR_TO_esmf.mk>

    python setup.py install --prefix=<custom_install_location>

    setenv PYTHONPATH <custom_install_location>/lib/\*/site_packages

Please contact esmf_support@list.woc.noaa.gov with any questions or problems.


---------------
Importing ESMPy
---------------

To use ESMPy in an external program, import it with:

    import ESMF

----------
Validation
----------

The ESMPy testing is done with the nose package, both in serial and
parallel.  The nose commands are wrapped in the following ESMPy targets:

    python setup.py test

    python setup.py test_examples

    python setup.py test_regrid_from_file

    python setup.py test_parallel

    python setup.py test_examples_parallel

    python setup.py test_regrid_from_file_parallel

NOTE: The regrid_from_file tests can take up a lot of memory and bandwidth.
The "test_regrid_from_file_dryrun" command will simply download the test
files without actually running them (allowing the stress on the machine to
be applied to bandwidth first, and then memory).

Alternatively, individual tests can be run with nose using the following format:

    nosetests <file>:<test>

e.g.

    nosetests src/ESMF/test/test_api/test_regrid.py:TestRegrid.test_field_regrid

-----------
Limitations
-----------

ESMPy doesn't include many aspects of ESMF, including components, field bundles,
time management, etc.  The limitations listed here are relative
to ESMF offline and integrated regridding capabilities.

- There is no FieldBundle class, only single Fields.
- There is no support for multi-tile Grids, nor Grids defined across multiple
  files.
- ESMPy cannot use an ESMF installation that is built with external LAPACK
  support.
- Conservative regridding with a source Mesh created from file is not supported,
  because the Mesh cannot retrieve coordinates from the elements.
- Meshes can only be created in-memory (not from-file) with a Cartesian
  coordinate system.
- To avoid memory leaks, each ESMPy class instance should be manually released
  using the destroy() method.

Testing related:

- Nightly regression testing is limited to a small subset of the ESMF test platforms,
  including Darwin, Linux and Cray running gfortran with openMPI.


