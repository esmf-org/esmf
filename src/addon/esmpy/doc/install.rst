============
Installation
============

------------
Requirements
------------

The following packages are *required* to work with ESMPy:

* `ESMF installation <http://earthsystemmodeling.org/docs/release/latest/ESMF_usrdoc/>`_
* `python <http://python.org/>`_, minimum version 3.8
* `numpy <http://www.numpy.org/>`_, minimum version 1.19

The following packages are *optional*:

* ESMF installation with NetCDF - required to create :class:`Grids <esmpy.api.grid.Grid>`, :class:`Meshes <esmpy.api.mesh.Mesh>` and :class:`Fields <esmpy.api.field.Field>` from file, and to write regridding weights to file
    - NetCDF must be built as a shared library for ESMPy installation to succeed
* ESMF installation with PIO (the Parallel IO library) - required to create :class:`Meshes <esmpy.api.mesh.Mesh>` and :class:`Fields <esmpy.api.field.Field>` from file, and to write regridding weights to file
* `mpi4py <https://mpi4py.readthedocs.io/en/stable/>`_- python bindings to MPI, needed to run some of the parallel regridding examples
* `pytest <https://docs.pytest.org/en/7.1.x/>`_ - for testing
* `scipy <https://scipy.org/>`_ - for testing (for reading NetCDF files in Python tests)

----------------
Getting the code
----------------

The ESMPy source code can be downloaded from the 
`ESMF git repository <https://github.com/esmf-org/esmf>`_.

The `ESMF User's Guide <http://earthsystemmodeling.org/docs/release/latest/ESMF_usrdoc/>`_
contains information on building and installing esmpy.

The `ESMF Reference Manual <http://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/>`_
contains information on the architecture of ESMF, example code, and details of the API (Application Programming
Interface).

~~~~~~~~~~~~~~~~~
Anaconda Packages
~~~~~~~~~~~~~~~~~

ESMPy conda packages are available through the conda-forge channel:

.. code::

    conda create -n esmpy -c conda-forge esmpy

Specific versions of the conda package can be installed like this:

.. code::

    conda create -n esmpy -c conda-forge esmpy=8.1.0

Development versions can be found in the ``esmpy_dev`` channel:

.. code::

    conda create -n esmpy -c conda-forge -c esmpy_dev esmpy

----------------------------
Installing ESMPy from Source
----------------------------

When installing from source, ESMPy uses `pip <https://pypi.org/project/pip//>`_ 
to build and install the package. An installation of ESMPy in the default location for Python packages can be done
with the following command issued from the top level ESMPy directory (``src/addon/esmpy``):

.. code::

    python3 -m pip install .

Please contact esmf_support@ucar.edu with any questions.

.. Note::

   With some python versions, there is a problem with the automatic installation of the required package ``setuptools-git-versioning``. If the output from the ``pip install`` command ends with ``Successfully installed esmpy-0.0.0``, this indicates that the ``setuptools-git-versioning`` package was not invoked and the build thinks you are using ESMF version 0.0.0. This will cause problems when you try to use ESMPy. To solve this problem, simply rerun the above installation command (``python3 -m pip install .``). You should then see a reasonable version number at the end, such as, ``Successfully installed esmpy-8.4.0``.

---------------
Importing ESMPy
---------------

ESMPy relies on a build of the underlying ESMF library. When using a conda package, this
is made available automatically. However, when installing ESMPy from source, the
corresponding ESMF library must also be built, per the `ESMF documentation
<http://earthsystemmodeling.org/docs/release/latest/ESMF_usrdoc/>`_.

Before importing ESMPy, the environment variable ``ESMFMKFILE`` must be set, pointing to
the location of the ``esmf.mk`` file from the ESMF installation. (If this environment
variable is not found, the package will try to guess a few common locations, but we
recommend correctly setting the variable nonetheless.) The location of this file is:

.. code:: shell

    <ESMF_INSTALL_DIR>/lib/lib<g<or>O>/<platform>/esmf.mk

For example, with the bash shell on a Mac, where an optimized version of ESMF has been
built with the gfortran+clang compilers and the OpenMPI MPI library, ``ESMFMKFILE`` can be
set with the following (replacing the ``<ESMF_INSTALL_DIR>`` placeholder with the
appropriate path):

.. code:: bash

   export ESMFMKFILE=<ESMF_INSTALL_DIR>/lib/libO/Darwin.gfortranclang.64.openmpi.default/esmf.mk

After ``ESMFMKFILE`` has been set appropriately, to use ESMPy in an external program,
import it with:

.. code:: python

    import esmpy

.. Note::

   The Python module name for ESMPy was changed in v8.4.0 from "ESMF" to "esmpy". If you are using a version older than v8.4.0, the import command is ``import ESMF``. See the `ESMF Release Notes <http://earthsystemmodeling.org/static/releases.html>`_ for more details and links to previous versions of the ESMPy documentation.

----------
Validation
----------

The ESMPy testing is done with the pytest package, both in serial and
parallel. Basic unit tests can be run with the following command, from
the top level ESMPy directory (``src/addon/esmpy``):

.. code::

    python3 -m pytest
    
There are a few other pytest targets available for a wider range of testing if 
greater test coverage is desired:

.. code::

    make test_unit

    make test_examples

    make test_unit_parallel

    make test_examples_parallel
    
    make test_regrid_from_file

.. Note:: 

    The ``regrid_from_file`` and ``example`` tests can take up a lot of memory 
    and bandwidth. The ``download_regrid_from_file`` and ``download_examples`` 
    commands will simply download the test files without actually running them 
    (allowing the stress on the machine to be applied to bandwidth first, and 
    then memory).

.. Note::

   By default, test data will be downloaded to a ``data`` subdirectory of the ESMPy installation directory. This location can be changed by setting one of the following environment variables:

   - If ``ESMPY_DATA_DIR`` is set, this should point to a directory that has already been populated with the necessary data; the pre-existing data will be read from this directory and no automatic downloads will be attempted.

   - Otherwise, if ``ESMPY_DATA_NEW_DIR`` is set, data will be downloaded to the path set by this variable instead of using the ``data`` subdirectory of the ESMPy installation directory.

-----------
Limitations
-----------

ESMPy doesn't include many aspects of ESMF, including components, field bundles,
time management, etc.  The limitations listed here are relative
to ESMF offline and integrated regridding capabilities.

- ESMPy cannot use an ESMF installation that is built with external LAPACK
  support.
- Coordinates cannot be retrieved from the elements of a 
  :class:`~esmpy.api.mesh.Mesh`. This can affect the ability to set 
  :class:`~esmpy.api.field.Field` values on a source :class:`~esmpy.api.mesh.Mesh`
  created from file when using conservative regridding.
- Multi-tile :class:`~esmpy.api.grid.Grid` support is limited to cubed-sphere 
  grids. A :class:`~esmpy.api.field.Field` created from a cubed-sphere
  :class:`~esmpy.api.grid.Grid` cannot be written to file in parallel.
- There is no ``FieldBundle`` class, only single :class:`Fields <esmpy.api.field.Field>`.

Testing related:

- Nightly regression testing is limited to a small subset of the ESMF test platforms,
  including Darwin and Linux with the gfortran and intel compilers.
