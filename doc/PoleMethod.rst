PoleMethod
=======

Indicates which type of artificial pole to construct on the source 
Grid for regridding.

Values are:

        NONE = 0
            No pole. Destination points which lie above the top or
            below the bottom row of the source Grid won't be mapped.
        ALLAVG = 1
            Construct an artificial pole placed in the center of the
            top (or bottom) row of nodes, but projected onto the
            sphere formed by the rest of the grid. The value at this
            pole is the average of all the source values surrounding
            the pole.
        NPNTAVG = 2
            Construct an artificial pole placed in the center of the
            top (or bottom) row of nodes, but projected onto the
            sphere formed by the rest of the grid. The value at this
            pole is the average of the N source nodes next to the pole
            and surrounding the destination point (i.e. the value may
            differ for each destination point). Here N is set by using
            the regridPoleNPnts parameter and ranges from 1 to
            the number of nodes around the pole. This option is useful
            for interpolating values which may be zeroed out by
            averaging around the entire pole (e.g. vector components).
        TEETH = 3
            No new pole point is constructed, instead the holes at the
            poles are filled by constructing triangles across the top
            and bottom row of the source Grid. This can be useful
            because no averaging occurs, however, because the top and
            bottom of the sphere are now flat, for a big enough
            mismatch between the size of the destination and source
            pole holes, some destination points may still not be able
            to be mapped to the source Grid.
