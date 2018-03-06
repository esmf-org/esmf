=========
Tutorials
=========

The first few tutorials are stand-alone scripts that can be run from any Python
interpreter.

-----------
Hello World
-----------

    .. literalinclude:: ../examples/hello_world.py

---------------------------
Regridding Helper Functions
---------------------------

The following code snippets demonstrate how to build all of the pieces
necessary to regrid data between :class:`Fields <ESMF.api.field.Field>` built on 
:class:`Grids <ESMF.api.grid.Grid>`, :class:`Meshes <ESMF.api.mesh.Mesh>` 
and :class:`LocStreams <ESMF.api.locstream.LocStream>`.

~~~~~~~~~~~~~~~~
LocStream Create
~~~~~~~~~~~~~~~~

.. literalinclude:: ../src/ESMF/util/locstream_utilities.py
        :pyobject: create_locstream_spherical_16

~~~~~~~~~~~~~~~~~~~~~~~~~
LocStream Create Parallel
~~~~~~~~~~~~~~~~~~~~~~~~~

.. literalinclude:: ../src/ESMF/util/locstream_utilities.py
        :pyobject: create_locstream_spherical_16_parallel

~~~~~~~~~~~~~~~~
Create a 2D Grid
~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/util/grid_utilities.py
        :pyobject: grid_create_from_coordinates

~~~~~~~~~~~~~~~~
Create a 3D Grid
~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/util/grid_utilities.py
        :pyobject: grid_create_from_coordinates_3d

~~~~~~~~~~~~~~~~~~~~~~
Create a Periodic Grid
~~~~~~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/util/grid_utilities.py
        :pyobject: grid_create_from_coordinates_periodic

~~~~~~~~~~~~~~~~~~~~~~~
Create a 5 Element Mesh
~~~~~~~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/util/mesh_utilities.py
        :pyobject: mesh_create_5

~~~~~~~~~~~~~~
Create a Field
~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/test/test_api/test_field.py
        :pyobject: TestField.create_field


~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Initialize an Analytic Field
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/util/grid_utilities.py
        :pyobject: initialize_field_grid_periodic


~~~~~~~~~~~~~~~~~~~~
Run ESMPy Regridding
~~~~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../src/ESMF/test/test_api/test_regrid.py
        :pyobject: TestRegrid.run_regridding


~~~~~~~~~~~~~~~~~~
Compute Field Mass
~~~~~~~~~~~~~~~~~~

  .. literalinclude:: ../src/ESMF/util/grid_utilities.py
    :pyobject: compute_mass_grid

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

-------------------------------------
Parallel Regridding Using MPI.Spawn()
-------------------------------------

    .. literalinclude:: ../examples/mpi_spawn_regrid.py

