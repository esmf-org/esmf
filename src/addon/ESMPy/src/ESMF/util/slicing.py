"""
helpers
"""

import numpy as np
import ESMF

#### HELPERS #########################################################

def get_formatted_slice(slc, n_dims):
    def _format_(slc):
        if isinstance(slc, int):
            ret = slice(slc, slc + 1)
        elif isinstance(slc, slice):
            ret = slc
        elif isinstance(slc, np.ndarray):
            ret = slc
        else:
            if len(slc) == 1:
                ret = slice(slc[0])
            elif len(slc) > 1:
                ret = np.array(slc)
            else:
                raise (NotImplementedError(slc, n_dims))
        return ret

    if isinstance(slc, slice) and slc == slice(None):
        if n_dims == 1:
            ret = slc
        else:
            ret = [slice(None)] * n_dims
    elif n_dims == 1:
        ret = _format_(slc)
    elif n_dims > 1:
        try:
            assert (len(slc) == n_dims)
        except (TypeError, AssertionError):
            raise IndexError("Only {0}-d slicing allowed.".format(n_dims))
        ret = tuple(map(_format_, slc))
    else:
        raise (NotImplementedError((slc, n_dims)))

    return ret

def get_none_or_slice(target, slc):
    if target is None:
        ret = None
    else:
        ret = target[slc]
    return ret

def get_none_or_ssslice(target, slc, stagger, rank):
    """
    Get none or stagger specific slice
    :param target: grid coordinates array
    :param slc: the slice to modify and take out of target
    :param stagger: slice that needs to be modifies according to the stagger location
    :param rank: rank of the grid
    :return:
    """

    if target is None:
        ret = None
    else:
        slc2 = None
        if rank == 2:
            assert(len(slc) == 2)
            if stagger == ESMF.StaggerLoc.CENTER:
                slc2 = slc
            elif stagger == ESMF.StaggerLoc.EDGE1:
                #slc[0] + 1
                slc2 = (slice(slc[0].start, slc[0].stop + 1, slc[0].step), slc[1])
            elif stagger == ESMF.StaggerLoc.EDGE2:
                #slc[1] + 1
                slc2 = (slc[0], slice(slc[1].start, slc[1].stop + 1, slc[1].step))
            elif stagger == ESMF.StaggerLoc.CORNER:
                #slc[0] + 1
                #slc[1] + 1
                slc2 = ([slice(slc[i].start, slc[i].stop + 1, slc[i].step) for i in range(len(slc))])
            else:
                raise ValueError("Stagger location is invalid")

        elif rank == 3:
            assert (len(slc) is 3)
            if stagger == ESMF.StaggerLoc.CENTER_VCENTER:
                slc2 = slc
            elif stagger == ESMF.StaggerLoc.EDGE1_VCENTER:
                #slc[0] + 1
                slc2 = (slice(slc[0].start, slc[0].stop + 1, slc[0].step), slc[1], slc[2])
            elif stagger == ESMF.StaggerLoc.EDGE2_VCENTER:
                #slc[1] + 1
                slc2 = (slc[0], slice(slc[1].start, slc[1].stop + 1, slc[1].step), slc[2])
            elif stagger == ESMF.StaggerLoc.CORNER_VCENTER:
                #slc[0] + 1
                #slc[1] + 1
                slc2 = (slice(slc[0].start, slc[0].stop + 1, slc[0].step), slice(slc[1].start, slc[1].stop + 1, slc[1].step), slc[2])
            elif stagger == ESMF.StaggerLoc.CENTER_VFACE:
                #slc[2] + 1
                slc2 = (slc[0], slc[1], slice(slc[2].start, slc[2].stop + 1, slc[2].step))
            elif stagger == ESMF.StaggerLoc.EDGE1_VFACE:
                #slc[0] + 1
                #slc[2] + 1
                slc2 = (slice(slc[0].start, slc[0].stop + 1, slc[0].step), slc[1], slice(slc[2].start, slc[2].stop + 1, slc[2].step))
            elif stagger == ESMF.StaggerLoc.EDGE2_VFACE:
                #slc[1] + 1
                #slc[2] + 1
                slc2 = (slc[0], slice(slc[1].start, slc[1].stop + 1, slc[1].step), slice(slc[2].start, slc[2].stop + 1, slc[2].step))
            elif stagger == ESMF.StaggerLoc.CORNER_VFACE:
                #slc[0] + 1
                #slc[1] + 1
                #slc[2] + 1
                slc2 = ([slice(slc[i].start, slc[i].stop + 1, slc[i].step) for i in range(len(slc))])
            else:
                raise ValueError("Stagger location is invalid")
        else:
            raise ValueError("Grid cannot have less than 2 or more than 3 dimensions")

        ret = target[slc2]

    return ret

def get_none_or_bound(target, ind):
    """
    :param grid coord variable:
    :param stagger:
    :return:
    """
    if target[ind] is None:
        ret = None
    else:
        ret = np.array(target[ind].shape, dtype=np.int32)
    return ret

def get_none_or_bound_list(target, ind):
    """
    :param grid coord variable:
    :param stagger:
    :return:
    """
    if target[ind] is None:
        ret = None
    else:
        temp = target[ind].shape
        assert (len(temp) == 1)
        ret = int(temp[0])
    return ret
