=========
Tutorials
=========

The first few tutorials are stand-alone scripts that can be run from any Python
interpreter.

-----------
Hello World
-----------

    .. literalinclude:: ../examples/hello_world.py

-------------------------
Create and Read from File
-------------------------

    .. literalinclude:: ../examples/create_read_from_file.py


----------------------------
Regridding from Grid to Mesh
----------------------------

    .. literalinclude:: ../examples/grid_mesh_regrid.py

---------------------------------
Regridding from Grid to LocStream
---------------------------------

    .. literalinclude:: ../examples/grid_locstream_regrid.py

---------------------------------
Regridding from Mesh to LocStream
---------------------------------

    .. literalinclude:: ../examples/mesh_locstream_regrid.py

---------------------------------
Regridding from LocStream to Grid
---------------------------------

    .. literalinclude:: ../examples/locstream_grid_regrid.py

---------------------------
Regridding Helper Functions
---------------------------

The following code snippets demonstrate how to build all of the pieces
necessary to regrid data between Fields built on Grids, Meshes and LocStreams.

~~~~~~~~~~~~~~~~
LocStream Create
~~~~~~~~~~~~~~~~

.. literalinclude:: ../src/ESMF/test/test_api/locstream_utilities.py
        :pyobject: create_locstream_spherical_16

~~~~~~~~~~~~~~~~~~~~~~~~~
LocStream Create Parallel
~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../src/ESMF/test/test_api/locstream_utilities.py
        :pyobject: create_locstream_spherical_16_parallel

~~~~~~~~~~~~~~~~
Create a 2D Grid
~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/test/test_api/grid_utilities.py
        :pyobject: grid_create

~~~~~~~~~~~~~~~~
Create a 3D Grid
~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/test/test_api/grid_utilities.py
        :pyobject: grid_create_3d

~~~~~~~~~~~~~~~~~~~~~~
Create a periodic Grid
~~~~~~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/test/test_api/grid_utilities.py
        :pyobject: grid_create_periodic

~~~~~~~~~~~~~~~~~~~~~~~
Create a 5 element Mesh
~~~~~~~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/test/test_api/mesh_utilities.py
        :pyobject: mesh_create_5

~~~~~~~~~~~~~~
Create a Field
~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/test/test_api/test_field.py
        :pyobject: TestField.create_field


~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Initialize an analytic Field
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/test/test_api/grid_utilities.py
        :pyobject: initialize_field_grid_periodic


~~~~~~~~~~~~~~~~~~~~
Run ESMPy regridding
~~~~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/test/test_api/test_regrid.py
        :pyobject: TestRegrid.run_regridding


~~~~~~~~~~~~~~~~~~
Compute Field mass
~~~~~~~~~~~~~~~~~~

  .. literalinclude:: ../src/ESMF/test/test_api/grid_utilities.py
    :pyobject: compute_mass_grid


