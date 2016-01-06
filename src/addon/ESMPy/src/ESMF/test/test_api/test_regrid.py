"""
regrid unit test file
"""

from ESMF import *
from ESMF.test.base import TestBase, attr
from ESMF.test.test_api.mesh_utilities import *
from ESMF.test.test_api.grid_utilities import *

class TestRegrid(TestBase):

    Manager(debug=True)

    # this is for the documentation, do not modify
    def run_regridding(srcfield, dstfield, srcfracfield, dstfracfield):
        '''
        PRECONDITIONS: Two Fields have been created and a regridding
                       operation is desired from 'srcfield' to 'dstfield'.
                       The 'srcfracfield' and 'dstfractfield' are Fields
                       created to hold the fractions of the source and
                       destination fields which contribute to conservative
                       regridding.\n
        POSTCONDITIONS: A regridding operation has set the data on
                        'dstfield', 'srcfracfield', and 'dstfracfield'.\n
        RETURN VALUES: \n Field :: dstfield \n
                          Field :: srcfracfield \n
                          Field :: dstfracfield \n
        '''
        # call the regridding functions
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    regrid_method=ESMF.RegridMethod.CONSERVE,
                                    unmapped_action=ESMF.UnmappedAction.ERROR,
                                    src_frac_field=srcfracfield,
                                    dst_frac_field=dstfracfield)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        return dstfield, srcfracfield, dstfracfield

    def test_field_regrid(self):
        # create grids
        max_index = np.array([20, 20])
        srcgrid = Grid(max_index, coord_sys=CoordSys.CART)
        max_index = np.array([25, 25])
        dstgrid = Grid(max_index, coord_sys=CoordSys.CART)

        # Add coordinates
        srcgrid.add_coords()
        dstgrid.add_coords()

        [x, y] = [0, 1]
        gridXCorner = srcgrid.get_coords(x)
        gridYCorner = srcgrid.get_coords(y)

        for i in xrange(gridXCorner.shape[x]):
            gridXCorner[i, :] = float(i) / 6.

        for j in xrange(gridYCorner.shape[y]):
            gridYCorner[:, j] = float(j) / 6.

        gridXCorner = dstgrid.get_coords(x)
        gridYCorner = dstgrid.get_coords(y)

        for i in xrange(gridXCorner.shape[x]):
            gridXCorner[i, :] = float(i) / 4.

        for j in xrange(gridYCorner.shape[y]):
            gridYCorner[:, j] = float(j) / 4.

        # create a Field on the Grid
        srcfield = Field(srcgrid)
        srcfield.data[:, :] = 10.
        dstfield = Field(srcgrid)
        dstfield.data[:, :] = 10.

        # regridding
        rh = Regrid(srcfield, dstfield, regrid_method=RegridMethod.BILINEAR,
                    line_type=LineType.CART)
        dstfield = rh(srcfield, dstfield)

    def test_field_regrid_gridmesh(self):
        # create mesh
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, _ = \
                mesh_create_50()
        dstfield = Field(mesh, meshloc=MeshLoc.ELEMENT)

        # create grid
        max_index = np.array([16, 16])
        grid = Grid(max_index, coord_sys=CoordSys.CART)

        # Add coordinates
        grid.add_coords(staggerloc=[StaggerLoc.CENTER, StaggerLoc.CORNER])

        [x, y] = [0, 1]
        gridXCorner = grid.get_coords(x, staggerloc=StaggerLoc.CORNER)
        gridYCorner = grid.get_coords(y, staggerloc=StaggerLoc.CORNER)

        for i in xrange(gridXCorner.shape[x]):
            gridXCorner[i, :] = float(i)

        for j in xrange(gridYCorner.shape[y]):
            gridYCorner[:, j] = float(j)

        # create a Field on the Grid
        srcfield = Field(grid)

        srcfield.data[:, :] = 10.

        # regridding
        rh = Regrid(srcfield, dstfield, regrid_method=RegridMethod.CONSERVE)
        dstfield = rh(srcfield, dstfield)

    def test_field_regrid_zeroregion(self):
        # create mesh
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, _ = \
                mesh_create_50()

        # create a field on the mesh
        srcfield = Field(mesh, meshloc=MeshLoc.ELEMENT)

        # initialize the source field
        for i in range(srcfield.data.shape[0]):
            srcfield.data[i] = 20.0

        # create grid
        grid = grid_create([0, 4], [0, 4], 8, 8, corners=True, domask=True)

        [x, y] = [0, 1]

        # create a Field on the Grid
        dstfield = Field(grid)

        # initialize the destination field according to the mask
        dstfield.data[:, :] = -100

        # regridding
        rh = Regrid(srcfield, dstfield, regrid_method=RegridMethod.CONSERVE,
                    dst_mask_values=np.array([0]))
        dstfield = rh(srcfield, dstfield, zero_region=Region.SELECT)

        # validate that the masked values were not zeroed out
        for i in range(dstfield.data.shape[x]):
            for j in range(dstfield.data.shape[y]):
                if dstfield.grid.mask[StaggerLoc.CENTER][i, j] == 0:
                    assert(dstfield[i, j] == 0)

    def test_field_regrid_area(self):
        # create mesh
        parallel = False
        if pet_count() > 1:
            if pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        mesh = None
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, _ = \
                mesh_create_50()

        # create grid
        grid = grid_create([0, 4], [0, 4], 8, 8, corners=True, doarea=True)

        [x, y] = [0, 1]

        # create area field
        dstarea = Field(mesh, name='DESTINATION AREAS!',
                           meshloc=MeshLoc.ELEMENT)
        dstarea.get_area()

        # create a Field on the Grid
        srcarea = Field(grid, name="SOURCE AREAS!")
        srcarea.get_area()

        for i in range(srcarea.data.shape[x]):
            for j in range(srcarea.data.shape[y]):
                if (srcarea.data[i, j] != 5):
                    print "Cell area is {0}, but expected 5".format(srcarea[i, j])

        # subtract two because the last two cells of mesh are triangles with half area
        for i in range(dstarea.data.shape[0]):
            if (dstarea.data[i] != 0.25):
                assert (dstarea.data[i] == 0.125)

    def test_field_regrid_periodic(self):
        parallel = False
        if ESMF.pet_count() > 1:
            if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        # create a grid
        srcgrid = grid_create_periodic(60, 30, corners=True, domask=True)
        dstgrid = grid_create_periodic(55, 28, corners=True)

        # create the Fields
        srcfield = ESMF.Field(srcgrid, name='srcfield')
        dstfield = ESMF.Field(dstgrid, name='dstfield')
        exactfield = ESMF.Field(dstgrid, name='exactfield')

        # create the fraction fields
        srcfracfield = ESMF.Field(srcgrid, name='srcfracfield')
        dstfracfield = ESMF.Field(dstgrid, name='dstfracfield')

        # initialize the Fields to an analytic function
        srcfield = initialize_field_grid_periodic(srcfield)
        exact_field = initialize_field_grid_periodic(exactfield)

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    src_mask_values=np.array([0]),
                                    regrid_method=ESMF.RegridMethod.CONSERVE,
                                    unmapped_action=ESMF.UnmappedAction.ERROR,
                                    src_frac_field=srcfracfield,
                                    dst_frac_field=dstfracfield)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compute the mass
        srcmass = compute_mass_grid(srcfield,
                                    dofrac=True, fracfield=srcfracfield)
        dstmass = compute_mass_grid(dstfield)

        # compare results and output PASS or FAIL
        meanrel, csrvrel = compare_fields_grid(dstfield, exact_field, 10E-2, 10e-15, parallel=parallel,
                            dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)

        self.assertAlmostEqual(meanrel, 0.0016447124122954575)
        self.assertAlmostEqual(csrvrel, 0.0)

    def test_grid_grid_regrid_csrv_mask_3D(self):
        parallel = False
        if ESMF.pet_count() > 1:
            if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        # create a grid
        srcgrid = grid_create_3d([0, 21], [0, 21], [0, 21], 21, 21, 21, corners=True)
        dstgrid = grid_create_3d([0.5, 19.5], [0.5, 19.5], [0.5, 19.5], 19, 19, 19, corners=True)

        # create Field objects on the Meshes
        srcfield = ESMF.Field(srcgrid, name='srcfield')
        srcfracfield = ESMF.Field(srcgrid, name='srcfracfield')
        dstfield = ESMF.Field(dstgrid, name='dstfield')
        dstfracfield = ESMF.Field(dstgrid, name='dstfracfield')
        exactfield = ESMF.Field(dstgrid, name='exactfield')

        # initialize the Fields to an analytic function
        srcfield = initialize_field_grid_3d(srcfield)
        exactfield = initialize_field_grid_3d(exactfield)

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    regrid_method=ESMF.RegridMethod.CONSERVE,
                                    unmapped_action=ESMF.UnmappedAction.ERROR,
                                    src_frac_field=srcfracfield,
                                    dst_frac_field=dstfracfield)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compute the mass
        srcmass = compute_mass_grid(srcfield,
                                    dofrac=True, fracfield=srcfracfield)
        dstmass = compute_mass_grid(dstfield)

        # compare results and output PASS or FAIL
        meanrel, csrvrel = compare_fields_grid(dstfield, exactfield, 10E-03, 10E-16, parallel=parallel,
                            dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)

        self.assertAlmostEqual(meanrel, 0.0021560174316746865)
        self.assertAlmostEqual(csrvrel, 0.0)

    def test_grid_grid_regrid_csrv_mask(self):
        parallel = False
        if ESMF.pet_count() > 1:
            if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        # create two unique Grid objects
        srcgrid = grid_create([0, 21], [0, 21], 21, 21, corners=True, domask=True)
        dstgrid = grid_create([0.5, 19.5], [0.5, 19.5], 19, 19, corners=True)

        # create Field objects on the Meshes
        srcfield = ESMF.Field(srcgrid, name='srcfield')
        srcfracfield = ESMF.Field(srcgrid, name='srcfracfield')
        dstfield = ESMF.Field(dstgrid, name='dstfield')
        dstfracfield = ESMF.Field(dstgrid, name='dstfracfield')
        exactfield = ESMF.Field(dstgrid, name='exactfield')

        # initialize the Fields to an analytic function
        srcfield = initialize_field_grid(srcfield)
        dstfield2 = initialize_field_grid(exactfield)

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    src_mask_values=np.array([0]),
                                    regrid_method=ESMF.RegridMethod.CONSERVE,
                                    unmapped_action=ESMF.UnmappedAction.ERROR,
                                    src_frac_field=srcfracfield,
                                    dst_frac_field=dstfracfield)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compute the mass
        srcmass = compute_mass_grid(srcfield,
                                    dofrac=True, fracfield=srcfracfield)
        dstmass = compute_mass_grid(dstfield)

        # compare results and output PASS or FAIL
        meanrel, csrvrel = compare_fields_grid(dstfield, dstfield2, 10E-3, 10E-16, parallel=parallel,
                            dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)

        self.assertAlmostEqual(meanrel, 0.0024803189848013785)
        self.assertAlmostEqual(csrvrel, 0.0)

    def test_grid_grid_regrid_srcmask_types(self):
        # NOTE: this tests an old issue where the items of a grid were not properly set when
        # the grid coord_typekind differed from the field typekind.
        parallel = False
        if ESMF.pet_count() > 1:
            if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        # create two unique Grid objects
        srcgrid = grid_create([0, 21], [0, 21], 21, 21, corners=True, domask=True,
                              ctk=ESMF.TypeKind.R4)
        dstgrid = grid_create([0.5, 19.5], [0.5, 19.5], 19, 19, corners=True)

        # create Field objects on the Meshes
        srcfield = ESMF.Field(srcgrid, name='srcfield')
        srcfracfield = ESMF.Field(srcgrid, name='srcfracfield')
        dstfield = ESMF.Field(dstgrid, name='dstfield')
        dstfracfield = ESMF.Field(dstgrid, name='dstfracfield')
        exactfield = ESMF.Field(dstgrid, name='exactfield')

        # initialize the Fields to an analytic function
        srcfield = initialize_field_grid(srcfield)
        dstfield2 = initialize_field_grid(exactfield)

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    src_mask_values=[0],
                                    regrid_method=ESMF.RegridMethod.CONSERVE,
                                    unmapped_action=ESMF.UnmappedAction.ERROR,
                                    src_frac_field=srcfracfield,
                                    dst_frac_field=dstfracfield)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compute the mass
        srcmass = compute_mass_grid(srcfield,
                                    dofrac=True, fracfield=srcfracfield)
        dstmass = compute_mass_grid(dstfield)

        # compare results and output PASS or FAIL
        meanrel, csrvrel = compare_fields_grid(dstfield, dstfield2, 10E-3, 10E-16, parallel=parallel,
                            dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)

        self.assertAlmostEqual(meanrel, 0.0024803189848013785)
        self.assertAlmostEqual(csrvrel, 0.0)

    def test_grid_mesh_regrid_csrv_mask(self):
        parallel = False
        if ESMF.pet_count() > 1:
            if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        # create a Mesh
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask, elemArea = \
                mesh_create_50_parallel(domask=True, doarea=True)
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, elemMask, elemArea = \
                mesh_create_50(domask=True, doarea=True)

        # create a grid
        grid = grid_create([0, 4], [0, 4], 8, 8, corners=True, doarea=True)

        # create Field objects on the Meshes
        srcfield = ESMF.Field(mesh, name='srcfield', meshloc=ESMF.MeshLoc.ELEMENT)
        srcfracfield = ESMF.Field(mesh, name='srcfracfield', meshloc=ESMF.MeshLoc.ELEMENT)

        # make gridded fields
        exactfield = ESMF.Field(grid, name='exactfield')
        dstfield = ESMF.Field(grid, name='dstfield')
        dstfracfield = ESMF.Field(grid, name='dstfracfield')

        # initialize the Fields to an analytic function
        srcfield = initialize_field_mesh(srcfield, nodeCoord, nodeOwner, elemType, elemConn,
                                         domask=True, elemMask=elemMask)
        exactfield = initialize_field_grid(exactfield)

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    src_mask_values=np.array([0]),
                                    regrid_method=ESMF.RegridMethod.CONSERVE,
                                    unmapped_action=ESMF.UnmappedAction.ERROR,
                                    src_frac_field=srcfracfield,
                                    dst_frac_field=dstfracfield)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compute the mass
        srcmass = compute_mass_mesh(srcfield, dofrac=True,
                                    fracfield=srcfracfield)
        dstmass = compute_mass_grid(dstfield)

        # compare results and output PASS or FAIL
        meanrel, csrvrel = compare_fields_grid(dstfield, exactfield, 80E-1, 10E-15, parallel=parallel,
                            dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)

        self.assertAlmostEqual(meanrel, 0.038806630051265847)
        self.assertAlmostEqual(csrvrel, 0.0)

    def test_grid_mesh_regrid_csrv(self):
        parallel = False
        if ESMF.pet_count() > 1:
            if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        # create a Mesh
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, _ = \
                mesh_create_50()

        # create a grid
        grid = grid_create([0, 4], [0, 4], 8, 8, corners=True)

        # create Fields
        srcfield = ESMF.Field(mesh, name='srcfield', meshloc=ESMF.MeshLoc.ELEMENT)
        srcfracfield = ESMF.Field(mesh, name='srcfracfield', meshloc=ESMF.MeshLoc.ELEMENT)
        dstfield = ESMF.Field(grid, name='dstfield')
        dstfracfield = ESMF.Field(grid, name='dstfracfield')
        exactfield = ESMF.Field(grid, name='exactfield')

        # initialize the Fields to an analytic function
        srcfield = initialize_field_mesh(srcfield, nodeCoord, nodeOwner, elemType, elemConn)
        exactfield = initialize_field_grid(exactfield)

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    regrid_method=ESMF.RegridMethod.CONSERVE,
                                    norm_type=ESMF.NormType.FRACAREA,
                                    unmapped_action=ESMF.UnmappedAction.ERROR,
                                    src_frac_field=srcfracfield,
                                    dst_frac_field=dstfracfield)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compute the mass
        srcmass = compute_mass_mesh(srcfield,
                                    dofrac=True, fracfield=srcfracfield)
        dstmass = compute_mass_grid(dstfield,
                                    dofrac=True, fracfield=dstfracfield)

        # compare results and output PASS or FAIL
        meanrel, csrvrel = compare_fields_grid(dstfield, exactfield, 50E-1, 10E-16, parallel=parallel,
                            dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)

        self.assertAlmostEqual(meanrel, 0.037733241800767432)
        self.assertAlmostEqual(csrvrel, 0.0)

    def test_grid_mesh_regrid_mask(self):
        parallel = False
        if ESMF.pet_count() > 1:
            if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        # create a grid
        grid = grid_create([0, 4], [0, 4], 8, 8, corners=True, domask=True)

        # create a Mesh
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, _ = \
                mesh_create_50()

        # create Field objects
        srcfield = ESMF.Field(mesh, name='srcfield')
        dstfield = ESMF.Field(grid, name='dstfield')
        exactfield = ESMF.Field(grid, name='exactfield')

        # initialize the Fields to an analytic function
        srcfield = initialize_field_mesh(srcfield, nodeCoord, nodeOwner, elemType, elemConn)
        exactfield = initialize_field_grid(exactfield, domask=True)

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    dst_mask_values=np.array([0]),
                                    regrid_method=ESMF.RegridMethod.BILINEAR,
                                    unmapped_action=ESMF.UnmappedAction.IGNORE)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compare results and output PASS or FAIL
        meanrel, csrvrel = compare_fields_grid(dstfield, exactfield, 20E-1, 10E-16, parallel=parallel,
                            regrid_method=ESMF.RegridMethod.BILINEAR)

        self.assertAlmostEqual(meanrel, 0.0)
        self.assertAlmostEqual(csrvrel, 0.0)

    def test_grid_mesh_regrid(self):
        parallel = False
        if ESMF.pet_count() > 1:
            if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        # create a grid
        grid = grid_create([0, 4], [0, 4], 8, 8, corners=True)

        # create a Mesh
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn, _ = \
                mesh_create_50()

        # create Field objects
        srcfield = ESMF.Field(mesh, name='srcfield')
        dstfield = ESMF.Field(grid, name='dstfield')
        exactfield = ESMF.Field(grid, name='exactfield')

        # initialize the Fields to an analytic function
        srcfield = initialize_field_mesh(srcfield, nodeCoord, nodeOwner, elemType, elemConn)
        exactfield = initialize_field_grid(exactfield)

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    regrid_method=ESMF.RegridMethod.BILINEAR,
                                    unmapped_action=ESMF.UnmappedAction.ERROR)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compare results and output PASS or FAIL
        meanrel, csrvrel = compare_fields_grid(dstfield, exactfield, 40E-2, 10E-16, parallel=parallel,
                            regrid_method=ESMF.RegridMethod.BILINEAR)

        self.assertAlmostEqual(meanrel, 0.0)
        self.assertAlmostEqual(csrvrel, 0.0)

    def test_mesh_mesh_regrid(self):
        parallel = False
        if ESMF.pet_count() > 1:
            if ESMF.pet_count() > 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        # create two unique Mesh objects
        if parallel:
            srcmesh, nodeCoordSrc, nodeOwnerSrc, elemTypeSrc, elemConnSrc = \
                mesh_create_50_parallel()
            dstmesh, nodeCoordDst, nodeOwnerDst, elemTypeDst, elemConnDst = \
                mesh_create_10_parallel()
        else:
            srcmesh, nodeCoordSrc, nodeOwnerSrc, elemTypeSrc, elemConnSrc, _ = \
                mesh_create_50()
            dstmesh, nodeCoordDst, nodeOwnerDst, elemTypeDst, elemConnDst, _ = \
                mesh_create_10()

        # create ESMP_Field objects on the Meshes
        srcfield = ESMF.Field(srcmesh, name='srcfield', meshloc=ESMF.MeshLoc.ELEMENT)
        srcfracfield = ESMF.Field(srcmesh, name='srcfracfield', meshloc=ESMF.MeshLoc.ELEMENT)
        dstfield = ESMF.Field(dstmesh, name='dstfield', meshloc=ESMF.MeshLoc.ELEMENT)
        dstfracfield = ESMF.Field(dstmesh, name='dstfracfield', meshloc=ESMF.MeshLoc.ELEMENT)
        exactfield = ESMF.Field(dstmesh, name='exactfield', meshloc=ESMF.MeshLoc.ELEMENT)

        # initialize the Fields to an analytic function
        srcfield = initialize_field_mesh(srcfield, nodeCoordSrc, nodeOwnerSrc,
                                         elemTypeSrc, elemConnSrc)
        exactfield = initialize_field_mesh(exactfield, nodeCoordDst, nodeOwnerDst,
                                           elemTypeDst, elemConnDst)

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    regrid_method=ESMF.RegridMethod.CONSERVE,
                                    norm_type=ESMF.NormType.FRACAREA,
                                    unmapped_action=ESMF.UnmappedAction.ERROR,
                                    ignore_degenerate=True,
                                    src_frac_field=srcfracfield,
                                    dst_frac_field=dstfracfield)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compute the mass
        srcmass = compute_mass_mesh(srcfield,
                                    dofrac=True, fracfield=srcfracfield)
        dstmass = compute_mass_mesh(dstfield,
                                    dofrac=True, fracfield=dstfracfield)

        # compare results and output PASS or FAIL
        meanrel, csrvrel = compare_fields_mesh(dstfield, exactfield, 20E-2, 10E-16, parallel=parallel,
                            dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)

        self.assertAlmostEqual(meanrel, 0.037109375)
        self.assertAlmostEqual(csrvrel, 0.0)

    def est_grid_mesh_pentatri_regrid_csrv(self):
        parallel = False
        if ESMF.pet_count() > 1:
            if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        # create a Mesh
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons()

        # create a grid
        grid = grid_create([0, 4], [0, 4], 8, 8, corners=True, doarea=True)

        # create Field objects on the Meshes
        srcfield = ESMF.Field(mesh, name='srcfield', meshloc=ESMF.MeshLoc.ELEMENT)
        srcfracfield = ESMF.Field(mesh, name='srcfracfield', meshloc=ESMF.MeshLoc.ELEMENT)

        # make gridded fields
        exactfield = ESMF.Field(grid, name='exactfield')
        dstfield = ESMF.Field(grid, name='dstfield')
        dstfracfield = ESMF.Field(grid, name='dstfracfield')

        # initialize the Fields to an analytic function
        # srcfield = initialize_field_mesh(srcfield, nodeCoord, nodeOwner, elemType, elemConn)
        # exactfield = initialize_field_grid(exactfield)
        # TODO: cannot make analytic fields on ngons yet
        srcfield.data[...] = 25.
        exactfield.data[...] = 25.

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    regrid_method=ESMF.RegridMethod.CONSERVE,
                                    unmapped_action=ESMF.UnmappedAction.ERROR,
                                    src_frac_field=srcfracfield,
                                    dst_frac_field=dstfracfield)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compute the mass
        srcmass = compute_mass_mesh(srcfield, dofrac=True,
                                    fracfield=srcfracfield)
        dstmass = compute_mass_grid(dstfield)

        # compare results and output PASS or FAIL
        meanrel, csrvrel = compare_fields_grid(dstfield, exactfield, 80E-1, 10E-15, parallel=parallel,
                                               dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)

        assert (meanrel < 10E-2)
        assert (csrvrel < 10E-14)

    @attr('serial')
    def est_grid_mesh_pentatri_regrid_csrv_simple(self):
        if ESMF.pet_count() > 1:
            raise NameError('This test can only be run in serial!')

        # create a Mesh
        mesh, nodeCoord, nodeOwner, elemType, elemConn = \
            mesh_create_4_ngons()

        # create a grid
        grid = grid_create([0, 4], [0, 4], 8, 8, corners=True, doarea=True)

        # create Field objects on the Meshes
        srcfield = ESMF.Field(mesh, name='srcfield', meshloc=ESMF.MeshLoc.ELEMENT)
        srcfracfield = ESMF.Field(mesh, name='srcfracfield', meshloc=ESMF.MeshLoc.ELEMENT)

        # make gridded fields
        exactfield = ESMF.Field(grid, name='exactfield')
        dstfield = ESMF.Field(grid, name='dstfield')
        dstfracfield = ESMF.Field(grid, name='dstfracfield')

        # initialize the Fields to an analytic function
        # srcfield = initialize_field_mesh(srcfield, nodeCoord, nodeOwner, elemType, elemConn)
        # exactfield = initialize_field_grid(exactfield)
        # TODO: cannot make analytic fields on ngons yet
        srcfield.data[...] = 25.
        exactfield.data[...] = 25.

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    regrid_method=ESMF.RegridMethod.CONSERVE,
                                    unmapped_action=ESMF.UnmappedAction.ERROR,
                                    src_frac_field=srcfracfield,
                                    dst_frac_field=dstfracfield)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compute the mass
        srcmass = compute_mass_mesh(srcfield, dofrac=True,
                                    fracfield=srcfracfield)
        dstmass = compute_mass_grid(dstfield)

        # compare results and output PASS or FAIL
        meanrel, csrvrel = compare_fields_grid(dstfield, exactfield, 80E-1, 10E-15,
                                               dstfracfield=dstfracfield, mass1=srcmass, mass2=dstmass)

        assert (meanrel < 10E-2)
        assert (csrvrel < 10E-14)

    def test_grid_mesh_pentatri_regrid_bilinear(self):
        parallel = False
        if ESMF.pet_count() > 1:
            if ESMF.pet_count() != 4:
                raise NameError('MPI rank must be 4 in parallel mode!')
            parallel = True

        # create a Mesh
        if parallel:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons_parallel()
        else:
            mesh, nodeCoord, nodeOwner, elemType, elemConn = \
                mesh_create_50_ngons()

        # create a grid
        grid = grid_create([0, 4], [0, 4], 8, 8, corners=True, doarea=True)

        # create Field objects on the Meshes
        srcfield = ESMF.Field(mesh, name='srcfield', meshloc=ESMF.MeshLoc.NODE)

        # make gridded fields
        exactfield = ESMF.Field(grid, name='exactfield')
        dstfield = ESMF.Field(grid, name='dstfield')

        # initialize the Fields to an analytic function
        # srcfield = initialize_field_mesh(srcfield, nodeCoord, nodeOwner, elemType, elemConn)
        # exactfield = initialize_field_grid(exactfield)
        # TODO: cannot make analytic fields on ngons yet
        srcfield.data[...] = 25.
        exactfield.data[...] = 25.

        # run the ESMF regridding
        regridSrc2Dst = ESMF.Regrid(srcfield, dstfield,
                                    regrid_method=ESMF.RegridMethod.BILINEAR,
                                    unmapped_action=ESMF.UnmappedAction.ERROR)
        dstfield = regridSrc2Dst(srcfield, dstfield)

        # compare results and output PASS or FAIL
        meanrel, _ = compare_fields_grid(dstfield, exactfield, 80E-1, 10E-16, parallel=parallel)

        self.assertAlmostEqual(meanrel, 0)
