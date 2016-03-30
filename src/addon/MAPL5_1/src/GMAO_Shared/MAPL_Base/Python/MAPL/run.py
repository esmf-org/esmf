"""
This package implements the running of a segment: it runs a MAPL
application for a prescribed period of time (or the end of the
experiment, whichever is sooner.)

"""

from job import Job

class Run(Job):

    def __init__(self,ConfigFile,Children=[]):

#       Initialize Job specific stuff in base class
#       -------------------------------------------
        Job.__init__(self,ConfigFile) 

        self.Children = Children

#                         -------------------
#                         Per-segment Methods
#                         -------------------

    def execute(self):
        """Executes the Application for one segment."""
        self.initialize()
        self.run()
        self.finalize()

    def initialize(self):
        self._initialize()
        for child in self.Children:
            child.initialize()
        self.initialize_()

    def run(self):
        self._run()
        for child in self.Children:
            child.run()
        self.run_()

    def finalize(self):
        self._finalize()
        for child in self.Children:
            child.finalize()
        self.finalize_()


#                         -----------------
#                          Per-job Methods
#                         -----------------

    def signin(self):
        self._signin()
        for child in self.Children:
            child.signin()
        self.signin_()

    def signout(self):
        self._signout()
        for child in self.Children:
            child.signout()
        self.signout_()


#                        ---------------------
#                        No-op Default Methods
#                        ---------------------

#   No-op pre-child methods
#   -----------------------
    def _initialize(self): pass
    def _run(self):        pass
    def _finalize(self):   pass
    def _signin(self):     pass
    def _signout(self):    pass

#   No-op post-child methods
#   ------------------------
    def initialize_(self): pass
    def run_(self):        pass
    def finalize_(self):   pass
    def signin_(self):     pass
    def signout_(self):    pass
