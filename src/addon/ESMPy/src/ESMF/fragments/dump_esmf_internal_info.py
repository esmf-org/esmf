# This file contains old routines to dump internal ESMF info from ESMPy Grid and Field objects
# This code is likely obsolete, but was useful in the original development process ..
#    so it is being kept for a rainy day situation

def dump_gridinfo(self, stagger):

    [x,y,z] = [0,1,2]

    print "bounds - low, high"
    print self.lower_bounds[stagger], \
                self.upper_bounds[stagger]
    print "shape - [x, y, z] or [lat, lon]"
    print self.coords[stagger][0].shape

    if self.rank == 2:
        return [self.coords[stagger][x], self.coords[stagger][y]]
    elif self.rank == 3:
        return [self.coords[stagger][x], self.coords[stagger][y], self.coords[stagger][z]]

def dump_gridinfo_lower(self, stagger):
    [x, y, z] = [0, 1, 2]

    print "bounds - low, high"
    print self.lower_bounds[stagger], \
                self.upper_bounds[stagger]
    print "shape - [x, y, z] or [lat, lon]"
    print self.coords[stagger][0].shape

    # retrieve buffers to esmf coordinate memory
    gridptrX = self.get_grid_coords_from_esmc(x, stagger)
    gridptrY = self.get_grid_coords_from_esmc(y, stagger)

    #print esmf_coords
    if self.rank == 2:
        return [gridptrX, gridptrY]
    elif self.rank == 3:
        gridptrZ = self.get_grid_coords_from_esmc(z, stagger)
        return [gridptrX, gridptrY, gridptrZ]

def dump_gridinfo_ctypes(self, stagger, dim=2):

    # find the size of the local coordinates at this stagger location
    from operator import mul
    size = reduce(mul,self.size[stagger])
    [x, y, z] = [0, 1, 2]

    # get the pointer to the underlying ESMF data array for coordinates

    xptr = ESMP_GridGetCoordPtr(self, x, staggerloc=stagger)

    import ctypes;
    buffer_from_memory = ctypes.pythonapi.PyBuffer_FromMemory
    buffer_from_memory.restype = ctypes.py_object
    xbuffer = buffer_from_memory(xptr, int(8*size))
    '''
    xbuffer = np.core.multiarray.int_asbuffer(
        ct.addressof(xptr.contents),
        np.dtype(constants._ESMF2PythonType[self.type]).itemsize*size)
    '''
    xcoords = np.frombuffer(xbuffer, np.float64)
    #xcoordarray = numpy.ndarray((nrows, ncols), dtype=numpy.float32, order='F',
    #                 buffer=buf)

    yptr = ESMP_GridGetCoordPtr(self, y, staggerloc=stagger)
    ybuffer = np.core.multiarray.int_asbuffer(
        ct.addressof(yptr.contents),
        np.dtype(constants._ESMF2PythonType[self.type]).itemsize*size)
    ycoords = np.frombuffer(ybuffer, constants._ESMF2PythonType[self.type])

    print "DIAGNOSTICS:"
    print "self.type = ", self.type
    print "constants._ESMF2PythonType", constants._ESMF2PythonType[self.type]
    print "constants._ESMF2PythonType.itemsize", constants._ESMF2PythonType[self.type].itemsize


    # find the size of the local coordinates at this stagger location
    from operator import mul
    size = reduce(mul,self.size[stagger])

    # these appear to both return bounds information only
    lb, ub = ESMP_GridGetCoordBounds(self, staggerloc=stagger)

    print "Bounds:"
    print "  ESMPy.Grid:    ", size
    print "  ESMPy.ctypes1: ", lb, ub


    print "Coordinates:"
    I = ub[x]-lb[x]
    J = ub[y]-lb[y]
    if dim == 3:
        K = ub[z]-lb[z]

    coordcount = 0
    if dim == 2:
        for i in range(I):
            for j in range(J):
                ind = i*J + j

                if  (1e-10 > xcoords[ind] and xcoords[ind] != 0) or \
                    xcoords[ind] > 360 or \
                    -90 > ycoords[ind] or \
                    (ycoords[ind] > -1e-10 and 1e-10 > ycoords[ind] and ycoords[ind] != 0) or \
                    ycoords[ind] > 90:

                    print "[", i, ", ", j, "] = [", xcoords[ind], ", ", ycoords[ind], "]"

                coordcount += 1
    elif dim == 3:
        zptr = ESMP_GridGetCoordPtr(self, z, staggerloc=stagger)
        zbuffer = np.core.multiarray.int_asbuffer(
            ct.addressof(zptr.contents),
            np.dtype(constants._ESMF2PythonType[self.type]).itemsize*size)
        zcoords = np.frombuffer(zbuffer, constants._ESMF2PythonType[self.type])

        for i in xrange(I):
            for j in xrange(J):
                for k in xrange(K):
                    ind = i*I + j*J + k
                    if  (1e-10 > xcoords[ind] and xcoords[ind] != 0) or \
                        xcoords[ind] > 360 or \
                        -90 > ycoords[ind] or \
                        (ycoords[ind] > -1e-10 and 1e-10 > ycoords[ind] and ycoords[ind] != 0) or \
                        ycoords[ind] > 90:

                        print "[", i, ", ", j, "] = [", xcoords[ind], ", ", ycoords[ind], "]"
                    coordcount += 1

    print "Coordcount = ", coordcount
    '''
    # create a numpy array to point to the ESMF allocation
    # reshape the numpy array of coordinates using Fortran ordering in Grid
    gridCoordP = np.reshape(gridCoordP, self.size[stagger], order='F')
    '''

def _dump_field_coords_(self):
    from operator import mul

    # retrieve buffers to esmf coordinate memory
    field_data = ESMP_FieldGetPtr(self.struct)

    # find the reduced size of the coordinate arrays
    size = reduce(mul,self.grid.size[self.staggerloc])

    # loop through and alias esmf data to numpy arrays
    buffer = np.core.multiarray.int_asbuffer(
        ct.addressof(field_data.contents),
        np.dtype(constants._ESMF2PythonType[self.type]).itemsize*size)
    esmf_coords = np.frombuffer(buffer, constants._ESMF2PythonType[self.type])

    print esmf_coords
