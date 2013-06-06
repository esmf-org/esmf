StaggerLoc
==========

In the Grid class, data can be located at different positions in a 
Grid cell.  When setting or retrieving coordinate data the stagger 
location is specified to tell the Grid method from where in the 
cell to get the data.

Values are:

    2D STAGGER LOCATIONS:

        INVALID = -2
        UNINIT = -1
        CENTER = 0
            Cell centered stagger location
        EDGE1 = 1
            First dimension (latitude) edge stagger location
        EDGE2 = 2
            Second dimension (longitude) edge stagger location
        CORNER = 3
            Cell corner stagger location

    3D STAGGER LOCATIONS:

        CENTER_VCENTER = 0
            Vertically centered stagger location
        EDGE1_VCENTER = 1
            Vertically centered first dimension (latitude) edge stagger location
        EDGE2_VCENTER = 2
            Vertically centered second dimension (longitude) edge stagger location
        CORNER_VCENTER = 3
            Vertically centered corner stagger location
        CENTER_VFACE = 4
            Centered stagger location of the top and bottom cell faces
        EDGE1_VFACE = 5
            First dimension (latitude) edge stagger location of the top and bottom cell faces
        EDGE2_VFACE = 6
            Second dimension (longitude) edge stagger location of the top and bottom cell faces
        CORNER_VFACE = 7
            Corner stagger location of the top and bottom cell faces
