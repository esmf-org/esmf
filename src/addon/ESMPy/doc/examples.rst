=========
Tutorials
=========

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


----------
Regridding
----------

The following stand alone scripts demonstrate how to use regridding between
:class:`Fields <ESMF.api.field.Field>` built on 
:class:`Grids <ESMF.api.grid.Grid>`, :class:`Meshes <ESMF.api.mesh.Mesh>` 
and :class:`LocStreams <ESMF.api.locstream.LocStream>`. These scripts
can be run in serial or parallel with no modification.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Grid, Mesh and Field Created from File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../examples/regrid_from_file.py

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Read and Write a Weight File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../examples/read_write_weight_file.py

~~~~~~~~~~~~~~~~~
Grid to LocStream
~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../examples/grid_locstream_regrid.py

~~~~~~~~~~~~~~~~~
Mesh to LocStream
~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../examples/mesh_locstream_regrid.py

~~~~~~~~~~~~~~~~~
LocStream to Grid
~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../examples/locstream_grid_regrid.py

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Using MPI.Spawn() from a Serial Python Driver
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    .. literalinclude:: ../examples/mpi_spawn_regrid.py

