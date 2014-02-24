import sys, os
import ESMF
from ESMF.test.regrid_test.regrid_from_file_test.run_regrid_from_file_dryrun import cache_data_file

def mesh_create(filename, filetype, meshname):
    mesh = ESMF.Mesh(filename=filename,
                     filetype=filetype,
                     meshname=meshname)

    return mesh

def main():
    # Test setup
    meshes = [
             # cubed sphere
             "ne30np4-t2.nc", 
             # Regional 205x275
             "wr50a_090614.nc",
             # Regional RACM ar9v4 grid - 1560x1080 with masking
             "ar9v4_100920.nc",
             # ESMF format unstructured file with clockwise cells removed
             "mpas_uniform_10242_dual_counterclockwise.nc",
             # unstructured grid in UGRID format
             "FVCOM_grid2d_20120314.nc",
             # Regional regular grid in SCRIP format
             "scrip_regional_1140x690.nc"]

    filetype = [
                ESMF.FileFormat.SCRIP,
                ESMF.FileFormat.SCRIP,
                ESMF.FileFormat.SCRIP,
                ESMF.FileFormat.ESMFMESH,
                ESMF.FileFormat.UGRID,
                ESMF.FileFormat.SCRIP]

    meshname = [
                meshes[0]+'-mesh',
                meshes[1]+'-mesh',
                meshes[2]+'-mesh',
                meshes[3]+'-mesh',
                'fvcom_mesh',
                meshes[5]+'-mesh']

    mesh = 4
    prefix = 'data/'
    filename = prefix+meshes[mesh]
    if ESMF.local_pet() == 0:
        if not os.path.exists(prefix):
            os.mkdir(prefix)
        cache_data_file(filename)

    # Start up ESMF.
    esmp = ESMF.Manager(logkind=ESMF.LogKind.SINGLE, debug=True)
    pet_count = ESMF.pet_count()

    # create Mesh
    mesh = mesh_create(filename, filetype[mesh], meshname[mesh])

    # create a field on the elements of the mesh
    name = filename+'-field'
    field = ESMF.Field(mesh, name, meshloc=ESMF.MeshLoc.ELEMENT)

    # write the mesh to vtk formatted file
    #mesh._write(filename.rsplit('.',1)[0])

    # print the field
    #print field

if __name__ == '__main__':
    sys.exit(main())
