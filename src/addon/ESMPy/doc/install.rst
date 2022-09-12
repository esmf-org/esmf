============
Installation
============

------------
Requirements
------------

The following packages are *required* to work with ESMPy:

* `ESMF installation <http://earthsystemmodeling.org/docs/release/latest/ESMF_usrdoc/>`_
* `python <http://python.org/>`_, minimum version 3.7
* `numpy <http://www.numpy.org/>`_

The following packages are *optional*:

* ESMF installation with NetCDF - required to create :class:`Grids <esmpy.api.grid.Grid>` and :class:`Meshes <esmpy.api.mesh.Mesh>` from file
    - NetCDF must be built as a shared library for ESMPy installation to succeed
* `mpi4py <http://mpi4py.scipy.org/>`_- python bindings to MPI, needed to run some of the parallel regridding examples
* `pytest <https://docs.pytest.org/en/7.1.x/>`_ - for testing

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
to build and install the package. This requires setting an environment variable 
pointing to a file named esmf.mk that is generated during an ESMF installation.  
The path of this file is:

.. code::

    <ESMF_INSTALL_DIR>/lib/lib<g<or>O>/<platform>/esmf.mk

If ``ESMFMKFILE`` is set when building ESMPy then it will not need to be
referenced again. 

An installation of ESMPy in the default location for Python packages can be done
with the following command issued from the top level ESMPy directory:

.. code::

    python3 -m pip install .

Please contact esmf_support@ucar.edu with any questions.

---------------
Importing ESMPy
---------------

To use ESMPy in an external program, import it with:

.. code::

    import esmpy

----------
Validation
----------

The ESMPy testing is done with the pytest package, both in serial and
parallel. Basic unit tests can be run with the following command:

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
  grids created on 6 processors. A cubed-sphere grid can be created on any
  number of processors, but only when it is created on 6 processors will the
  coordinates be retrievable for the entire object. A 
  :class:`~esmpy.api.field.Field` created from a cubed-sphere 
  :class:`~esmpy.api.grid.Grid` cannot be written to file in parallel.
- There is no ``FieldBundle`` class, only single :class:`Fields <esmpy.api.field.Field>`.

Testing related:

- Nightly regression testing is limited to a small subset of the ESMF test platforms,
  including Darwin and Linux running gfortran with openMPI.
