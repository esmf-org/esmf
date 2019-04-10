from ESMF import Manager
from ESMF.api.constants import Reduce
import numpy as np


def reduce_val(inval, op=Reduce.SUM):
    mg = Manager()
    send_buf = np.array([inval], dtype=np.float64)
    recv_buf = np.array([0], dtype=np.float64)
    mg._reduce_(send_buf, recv_buf, 1, reduceflag=op)
    outval = recv_buf[0]
    return outval


def broadcast_val(inval):
    mg = Manager()
    bcst_buf = np.array([inval], dtype=np.float64)
    mg._broadcast_(bcst_buf, 1)
    outval = bcst_buf[0]
    return outval
