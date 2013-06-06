MeshElemType
============

An Mesh can be constructed from a combination of different elements.
The type of elements that can be used in a Mesh depends on the 
parametric dimension of the Mesh, which is set during Mesh 
creation.  The following are the valid Mesh element types for each 
valid Mesh parametric dimension (2D or 3D).

Values are:

    TRI = 5
        2D triangular elements with 3 sides
    QUAD = 9
        2D quadrilateral elements with 4 sides
    TETRA = 10
        3D tetrahedral elements with 4 faces
    HEX = 12
        3D hexahedral elements with 6 faces
