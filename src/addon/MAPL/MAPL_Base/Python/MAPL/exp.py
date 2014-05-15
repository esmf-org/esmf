# +-======-+ 
#  Copyright (c) 2003-2007 United States Government as represented by 
#  the Admistrator of the National Aeronautics and Space Administration.  
#  All Rights Reserved.
#  
#  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
#  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
#  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
#  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
#  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
#  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
#  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
#  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
#  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
#  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
#  
#  Government Agency: National Aeronautics and Space Administration
#  Government Agency Original Software Designation: GSC-15354-1
#  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
#  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
#  Government Agency Point of Contact for Original Software:  
#  			Dale Hithon, SRA Assistant, (301) 286-2691
#  
# +-======-+ 
"""
This package implements the Experiment class.
"""

import os

class Exp(object):

    def __init__(self,ConfigFiles=None):

        if ConfigFiles is None:
            here = _whereami()
            Configfiles = [ here + 'Experiment.rc',
                            here + 'Grids.rc',
                            here + 'Chem_Registry.rc'
                          ]

        self.cf = Config(ConfigFiles);

        self.EsmaDir = self.cf.EsmaDir # Location of system binaries

        self.SysID = self.cf.SysID     # e.g., "GEOSagcm"
        self.ExpID = self.cf.ExpID     # e.g., "a0202"
        self.ExpDescr = self.cf.ExpDescr

        self.ExpHomeDir = self.cf.ExpHomeDir 
        self.ExpExecDir = self.cf.ExpExecDir 
        self.ExpHoldDir = self.cf.ExpHoldDir 
        self.ExpArchDir = self.cf.ExpArchDir 

        self.ExpBegTime = cf.ExpBegTime 
        self.ExpEndTime = cf.ExpEndTime 

    def __del__(self):
        self.submit()  # resubmit itsef

    def submit(self):
        raise NotImplementedError, "Not implemented yet"
        

#                   --------------
#                   Static Methods
#                   --------------

def setup(inConfigFiles=None):
    """
    In the very beginning, setup the environment for
    running the experiment. It interacts with the user
    to setup all the necessary experiment directories
    and resource files.
    """

#   Default (input) Config files
#   ----------------------------
    if inConfigFiles is None:
        etc = _whereami() + '../etc'
        inConfigfiles = [ here + 'Experiment.irc',
                          here + 'Grids.irc',
                          here + 'Chem_Registry.irc',
                          here + 'History.irc'
                        ]

#   Derive Config file names by replacing ".irc" extensions with ".rc"
#   ------------------------------------------------------------------
    cmd = '$ESMADIR/bin/red_ma.pl'
    ConfigFiles = []
    for irc in inConfigFiles:
        cmd = cmd + ' ' + irc
        ConfigFiles.append(irc.replace('.irc','.rc'))

#   Get user input by lauching Red MAPL GUI
#   ---------------------------------------
    tmpdir = "/tmp/red_mapl.%s-%d"%(os.getenv('USER'),os.getpid())
    os.mkdir(tmpdir)
    os.chdir(tmpdir)
    if os.system(cmd):
       raise IOerror, "red_ma.pl did not complete successfully"

#   Resources as specified by user
#   ------------------------------
    cf = Config(ConfigFiles)
        
#   Setup directory tree
#   --------------------
    for dir in  cf.regex('Dir$').values():
        os.mkdir(dir)

#   Populate Resources
#   ------------------
    cf.save(cf('ExpRsrcDir')+'/Master.rc')
    os.system('/bin/cp -pr $ESMADIR/$ARCH/etc/*.rc ' + 
              cf('ExpRsrcDir') )

#   All done
#   --------
    os.system('/bin/rm -rf ' + tmpdir)

def tearDown(self):
    """
    Once an experiment is completed, run this for all
    necessary cleanup.
    """
    pass # can't think of anything useful yet

#.........................................................................................

if __name__ == "__main__":

    e = Experiment()
    e.submitt()

    
