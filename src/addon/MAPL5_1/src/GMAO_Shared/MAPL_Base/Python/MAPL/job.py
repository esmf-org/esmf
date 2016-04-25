"""
This package implements the functionality of a single Job. Methods
specifics of an applcation are defined as "abstract" --- that is, to be
defined by each specific Application.

Design remarks:

1. A Job should not have any knowledge of the specific Operating System
   (OS) and Queueing System (QS). If this knowledge becomes essential
   it should be abstracted out and implemented in the Experiment class.

"""

import Abstract
from exp import Exp

class Job(Exp):
    
    def __init__(self,ConfigFile):
        """Initialize a Job."""

#       Initialize Experiment specific stuff in base class
#       --------------------------------------------------
        Exp.__init__(self,ConfigFile) 

#       Job specific parameters (will raise exception if not present)
#       -------------------------------------------------------------
        self.nSegs = self.cf.nSegs
        self.recyclables = self.cf.recyclables # File list
        self.JobDelTime = self.cf.DelTime

#       Bring over resource files
#       -------------------------
        self.getResources()

#       Bring over recyclables to runing ExpExecDir
#       ---------------------------------- --------
        self.getRecyclables()

    def __call__(self):
        """
        Carries out a single Job by running several segments of the
        Application.
        """

#       Per-job Application setup
#       -------------------------
        self.signin()

#       Run application for each segment
#       --------------------------------
        for n in range(self.nSegs):
            self.execute()

#       Per-job Application clean-up
#       ----------------------------
        self.signout()

    def __del__(self):

#       Save recyclables to ExpHomeDir for next Job
#       -------------------------------------------
        self.putRecyclables()

#       Finalize experiment specific stuff in base class;
#       this will resubmit the job if necessary
#       ------------------------------------------------
        Experiment.__del__(self)

#                         -----------------
#                         Recycling Methods
#                         -----------------

    def getResources(self):
        raise NotImplementedError, "Not implemented yet"

    def getRecyclables(self):
        raise NotImplementedError, "Not implemented yet"

    def putRecyclables(self):
        raise NotImplementedError, "Not implemented yet"


#                         ----------------
#                         Abstract Methods
#                         ----------------

    signin  = Abstract.Method('signin')
    execute = Abstract.Method('execute')
    signout = Abstract.Method('signout')


