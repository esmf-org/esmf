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
This package contains foundation classes for assembling MAPL-based
systems using Python as the scripting language. At this stage of
development system are composed of MAPL-based Applications in the form
of stand alone executables (e.g., GEOSgcm.x). The following packages
define the basic functionality:

exp
  This package defines the base class *Exp* (as in *experiment*) which
  controls the execution of a long *experiment*. Each experiment is
  carried out by means of several *jobs* which are submitted through a
  queueing system such as PBS.

job 
  This package defines the base class *Job* which inherits from
  *Exp*. A *job* carries out a portion of the *experiment*,
  itself consisting of several *run* segments.

run
  This package defines the base class *Run* which inherits from
  *Job* extending it with methods for running a single *segment* of a
  job.  A *run* segment consists of running the stand-alone Fortran
  executable for a fixed period of time.

config
  This package defines the class Config providing functionality for
  basic resource file management loosely based in the ESMF_Config
  class.

Typically, an application such as GEOSgcm would inherit from
*Run* and implement specific methods for dealing with its own
resource files, boundary contitions, restarts and pre- and
post-processing.

Here is an illustration of an experiment consistng of 3 jobs, each
with 2 run segments.

   |----------------------- Experiment ------------------------|
   |------ Job 1 ------|------ Job 2 ------|------ Job 3 ------|
   |- Run 1 -|- Run 2 -|- Run 3 -|- Run 4 -|- Run 5 -|- Run 6 -|
 
If each run segment is 2 weeks long, each job performs a 4 week
integration, and the the whole experiment is about 3 month long.

"""

__version__ = "0.1.2"

from exp      import *
from job      import *
from run      import *
from config   import *
from history  import *
from Date     import *
from filelock import *

