CoordSys
========

This flag indicates the coordinate system of a Grid. This value is 
useful both to indicate to other users the type of the coordinates, 
but also to control how the coordinates are interpreted in 
regridding methods (e.g. Regrid()).

Values are:

    INVALID = -2
    UNINIT = -1
    CART = 0
        Cartesian coordinate system. In this system, the Cartesian 
        coordinates are mapped to the Grid coordinate dimensions in the 
        following order: x,y,z. (e.g. using coord_dim=1 in 
        Grid.get_coords() references the y dimension)
    SPH_DEG = 1
        Spherical coordinates in degrees. In this system, the spherical
        coordinates are mapped to the Grid coordinate dimensions in the 
        following order: longitude, latitude, radius. (E.g. using 
        coord_dim=1 in Grid.get_coords() references the latitude dimension) 
        Note, however, that Regrid() currently just supports longitude and 
        latitude (i.e. with this system, only Grids of dimension 2 are 
        supported in the regridding).
    SPH_RAD = 2
        Spherical coordinates in radians. In this system, the spherical
        coordinates are mapped to the Grid coordinate dimensions in the 
        following order: longitude, latitude, radius. (E.g. using 
        coord_dim=1 in Grid.get_coords() references the latitude dimension) 
        Note, however, that Regrid() currently just supports longitude and 
        latitude (i.e. with this system, only Grids of dimension 2 are 
        supported in the regridding).
