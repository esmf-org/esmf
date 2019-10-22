============
Installation
============

------------
Requirements
------------

The following packages are *required* to work with ESMPy:

* `ESMF installation <http://www.earthsystemmodeling.org/esmf_releases/non_public/ESMF_8_0_0/ESMF_usrdoc/node9.html>`_
* `python <http://python.org/>`_
* `numpy <http://www.numpy.org/>`_

The following packages are *optional*:

* ESMF installation with NetCDF - required to create :class:`Grids <ESMF.api.grid.Grid>` and :class:`Meshes <ESMF.api.mesh.Mesh>` from file
    - NetCDF must be built as a shared library for ESMPy installation to succeed
* `mpi4py <http://mpi4py.scipy.org/>`_- python bindings to MPI, needed to run some of the parallel regridding examples
* `nose <https://nose.readthedocs.org/en/latest/>`_ - for nose testing

----------------
Getting the code
----------------

The `ESMF User's Guide <http://www.earthsystemmodeling.org/esmf_releases/non_public/ESMF_8_0_0/ESMF_usrdoc/>`_
contains information on building and installing ESMF.
The `ESMF Reference Manual <http://www.earthsystemmodeling.org/esmf_releases/non_public/ESMF_8_0_0/ESMF_refdoc/>`_
contains information on the architecture of ESMF, example code, and details of the API (Application Programming
Interface).

Instructions on how to download the ESMPy code can be found at the `ESMPy Download page
<http://www.earthsystemcog.org/projects/esmpy/releases>`_.

~~~~~~~~~~~~~~~~~
Anaconda Packages
~~~~~~~~~~~~~~~~~

ESMPy conda packages are available through the NESII channel:

.. code::

    conda install -n esmpy -c nesii -c conda-forge esmpy

Specific versions of the conda package can be installed like this:

.. code::

    conda create -n esmpy -c nesii -c conda-forge esmpy=7.1.0r

----------------------------
Installing ESMPy from Source
----------------------------

When installing from source, ESMPy requires a pointer to a file named esmf.mk 
that is generated during an ESMF installation.  The path of this file is:

.. code::

    <ESMF_INSTALL_DIR>/lib/lib<g<or>O>/<platform>/esmf.mk

If the ``ESMFMKFILE`` flag is set when building ESMPy then it will not need to be
referenced again.  If not, an environment variable of the same name must be set
with the path to the esmf.mk file every time a new shell is initiated.

ESMPy can be installed in a custom location using the
``--prefix``, ``--home``, or ``--install-base`` flags to the install command.  If this
is done, then this location needs to be added to the ``PYTHONPATH`` environment
variable every time a new shell is initiated.  If a
custom install location is not specified, ESMPy will be installed in the
standard Python package installation directory on that particular machine.

An installation of ESMPy in the default location for Python packages can be done
with the following command issued from the top level ESMPy directory:

.. code::

    python setup.py build --ESMFMKFILE=<DIR_TO_esmf.mk>/esmf.mk install

- custom install location:

.. code::

    python setup.py build --ESMFMKFILE=<DIR_TO_esmf.mk>/esmf.mk

    python setup.py install --prefix=<custom_install_location>

    setenv PYTHONPATH <custom_install_location>/lib/\*/site_packages

Please contact esmf_support@ucar.edu with any questions.

---------------
Importing ESMPy
---------------

To use ESMPy in an external program, import it with:

.. code::

    import ESMF

----------
Validation
----------

The ESMPy testing is done with the nose package, both in serial and
parallel.  The nose commands are wrapped in the following ESMPy targets:

.. code::

    python setup.py test

    python setup.py test_examples

    python setup.py test_regrid_from_file

    python setup.py test_parallel

    python setup.py test_examples_parallel

    python setup.py test_regrid_from_file_parallel

.. Note:: 

    The ``regrid_from_file`` tests can take up a lot of memory and bandwidth.
    The ``test_regrid_from_file_dryrun`` command will simply download the test
    files without actually running them (allowing the stress on the machine to
    be applied to bandwidth first, and then memory).

Alternatively, individual tests can be run with nose using the following format:

.. code::

    nosetests <file>:<test>

e.g.

.. code::

    nosetests src/ESMF/test/test_api/test_regrid.py:TestRegrid.test_field_regrid

-----------
Limitations
-----------

ESMPy doesn't include many aspects of ESMF, including components, field bundles,
time management, etc.  The limitations listed here are relative
to ESMF offline and integrated regridding capabilities.

- ESMPy cannot use an ESMF installation that is built with external LAPACK
  support.
- Coordinates cannot be retrieved from the elements of a 
  :class:`~ESMF.api.mesh.Mesh`. This can affect the ability to set 
  :class:`~ESMF.api.field.Field` values on a source :class:`~ESMF.api.mesh.Mesh`
  created from file when using conservative regridding.
- Multi-tile :class:`~ESMF.api.grid.Grid` support is limited to cubed-sphere 
  grids created on 6 processors. A cubed-sphere grid can be created on any
  number of processors, but only when it is created on 6 processors will the
  coordinates be retrievable for the entire object. A 
  :class:`~ESMF.api.field.Field` created from a cubed-sphere 
  :class:`~ESMF.api.grid.Grid` cannot be written to file in parallel.
- There is no ``FieldBundle`` class, only single :class:`Fields <ESMF.api.field.Field>`.

Testing related:

- Nightly regression testing is limited to a small subset of the ESMF test platforms,
  including Darwin and Linux running gfortran with openMPI.


