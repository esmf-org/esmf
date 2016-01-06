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
A simpe implementation of a ESMF_Config-like class in Python.
Tables are not supported yet.

"""

__version__ = "0.1.0"

import string
import re
import os
import sys
from types import *
from datetime import datetime
    
class Config(object):

    def __init__(self,RcFiles,delim=':'):

        if type(RcFiles) is StringType:
            Files = ( RcFiles, ) # in case a single file is given
        else:
            Files = RcFiles     # more often, a List/Tuple of RC files

        self.Rc = {}
        self.delim = delim
        for rcfile in Files:
            self.Lines = open(rcfile).readlines()
            for line in self.Lines:
                line = line.rstrip()
                name, value, comment = _parseLine(line,self.delim)
                if name:
                    self.Rc[name] = {  'value': value, 
                                     'comment': comment, 
                                        'flag': 0}
                    
    def __call__(self,name,value=None):
        """Either get or set a resource depending on whether *value* is given"""
        if value == None:
            if self.Rc.__contains__(name):
                return self.Rc[name]['value']
        else:
            if self.Rc.__contains__(name):
                self.Rc[name]['value'] = value 
                self.Rc[name]['flag'] = 1
                return self.Rc[name]['value']
        return None
        
    get = __call__

    def set(self,name,value):
        return self.__call__(name,value)

    def save(self,rcfile=None):
        """Save to resource file."""
        if rcfile is None:
            f = sys.stdout
        else:
            f = open(rcfile,'w') 
        for line in self.Lines:
            line = line.rstrip()
            name, value, comment = _parseLine(line,self.delim)
            if name:
                if self.Rc[name]['flag']:
                    if comment:
                        comment = '#' + comment
                    else:
                        comment = ''
                    value = self.Rc[name]['value']
                    print >>f, name + self.delim+' ' + str(value) + comment # this line has been edited
                else:
                    print >>f, line
            else:
                print >>f, line
        f.close()
       
    def upd(self,dict):
        pass
         
    def interp(self,str,outFile=None,**kws):
        """
        Use the resource values for $-substitution (a.k.a.
        interpolation.)  When *outFile* is specified *str* is assumed
        to be the name of the input template file. Otherwise,
        *str* is a simple string template to be interpolated.
        """
        if outFile is None:
            return self.interpStr(str,**kws)
        else:
            self.interpFile(str,outFile,**kws)

    def interpFile(self,template,outFile,**kws):
        """
        Use the resource values for $-substitution in the
        input template file *Template*
        """
        Tmpl = open(template).readlines()
        Text = []
        for tmpl in Tmpl:
            Text.append(self.interpStr(tmpl,**kws))
        open(outFile,"w").writelines(Text)
                    
    def interpStr(self,template,strict=False):
        """
        Replace occurences of resource variables in the
        input string *StrTemplate*. For example, if
             StrTemplate = "This is $thing"
        $thing will be replaced with the value of the
        resource *thing*. When *strict* is True, an
        exeption will be raised if any $-token is left
        unresolved.
        """
        dict = {}
        for name in self.Rc:
            dict[name] = self.Rc[name]['value']
        if strict:
            return string.Template(template).substitute(dict)
        else:
            return string.Template(template).safe_substitute(dict)

    def regex(self,pattern,ignoreCase=True):
        """
        Return a dictionary with those resources matching the
        regular expression *pattern*. For example.
                cf.regex('RESTART_FILE').values()
        return a list of all restart files.
        """
        if ignoreCase is True:
            p = re.compile(pattern,re.IGNORECASE)
        else:
            p = re.compile(pattern)
        dict = {}
        for name in self.Rc:
            if p.search(name) is not None:
                dict[name] = self.Rc[name]['value']
        return dict

    def setenv(self,Only=None):
        """
        Use resources to set environment variables. Option,
        once can provide a list of strings (*Only*) with those
        resources to be turned into environment variables.
        """ 
        for name in self.Rc:
            if Only is None:
                os.environ[name] = self.Rc[name]['value']
            elif name in Only:
                os.environ[name] = self.Rc[name]['value']
 
    def keys(self):
        """ 
        Return list of resource names.
        """
        return self.Rc.keys()

    def values(self):
        """ 
        Return list of resource names.
        """
        vals = []
        for name in self.Rc:
            vals.append(self.Rc[name]['value'])
        return vals

    def strTemplate(self,name,expid=None,nymd=None,nhms=None,
                    yy=None,mm=None,dd=None,h=None,m=None,s=None,dtime=None):
        """
        Expand GrADS style templates in resource *name*. See
        static method strTemplate() for additional information
        on the date/time input parameters.
        """
        return strTemplate(self(name),expid,nymd,nhms,yy,mm,dd,h,m,s,dtime)

#                       Static Methods
#                       --------------

def _parseLine(line,delim):
    name, value, comment = (None,None,None)
    if line:
        all = line.split('#',1)
        rc = all[0]
        if len(all)>1:
            comment = all[1]
        if rc:
            rcs = rc.split(delim,1) # resource name and value
            if len(rcs) > 1:
                name = rcs[0].strip()
                value = rcs[1].strip()
    return name, value, comment

def strTemplate(templ,expid=None,nymd=None,nhms=None,
                    yy=None,mm=None,dd=None,h=None,m=None,s=None,
                    dtime=None):
    """
    Expands GrADS template in string *templ*. On input,

       expid ---  experiment id, expands %s
       yy    ---  year, expands %y4 and %y2
       mm    ---  month, expands %m2 or %m3
       dd    ---  day, expands %d2
       h     ---  hour, expands %h2
       m     ---  minute, expands %n2
       s     ---  minute, expands %S2 (notice capital "S")

       nymd  ---  same as yy*10000 + mm*100 + dd
       nhms  ---  same as h *10000 + h*100  + s

       dtime ---  python datetime

    Unlike GrADS, notice that seconds are expanded using the %S2 token. 
    Input date/time can be either strings or integers.

    Examples:

    >>> templ = "%s.aer_f.eta.%m3%y2.%y4%m2%d2_%h2:%n2:%S2z.nc"
    >>> print strTemplate(templ,expid="e0054A",yy=2008,mm=6,dd=30,h=1,m=30,s=47)
    e0054A.aer_f.eta.jun08.20080630_01:30:47z.nc
    >>> print strTemplate(templ,expid="e0054A",nymd=20080630,nhms=13000)
    e0054A.aer_f.eta.jun08.20080630_01:30:00z.nc

    """

    MMM = ( 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 
            'jul', 'aug', 'sep', 'oct', 'nov', 'dec' ) 
    
    str_ = templ[:]

    if dtime is not None:
        yy = dtime.year
        mm = dtime.month
        dd = dtime.day
        h  = dtime.hour
        m  = dtime.minute
        s  = dtime.second

    if nymd is not None:
        nymd = int(nymd)
        yy = nymd/10000
        mm = (nymd - yy*10000)/100
        dd = nymd - (10000*yy + 100*mm )

    if nhms is not None:
        nhms = int(nhms)
        h = nhms/10000
        m = (nhms - h * 10000)/100
        s = nhms - (10000*h + 100*m)

    if expid is not None: 
        str_ = str_.replace('%s',expid)
    if yy is not None: 
        y2 = yy%100
        str_ = str_.replace('%y4',str(yy))
        str_ = str_.replace('%y2',"%02d"%y2)
    if mm is not None: 
        mm = int(mm)
        mmm = MMM[mm-1]
        str_ = str_.replace('%m2',"%02d"%mm)
        str_ = str_.replace('%m3',mmm)
    if dd is not None: 
        str_ = str_.replace('%d2',"%02d"%int(dd))
    if h  is not None: 
        str_ = str_.replace('%h2',"%02d"%int(h))
    if m  is not None: 
        str_ = str_.replace('%n2',"%02d"%int(m))
    if s  is not None: 
        str_ = str_.replace('%S2',"%02d"%int(s))

    return str_

#................................................................

#                           Testing
#                           -------

def _ut_strTemplate():


    
    templ = "%s.aer_f.eta.%m3%y2.%y4%m2%d2_%h2:%n2:%S2z.nc"

    expid = "e0054A"
    yy = "2008"
    mm = "10"
    dd = "30"
    
    h = "1"
    m = "30"
    s = "47"

    dtime = datetime(2008,10,30,1,30,47)

    nymd = int(yy) * 10000 + int(mm)*100 + int(dd)
    nhms = int(h) * 10000 + int(m) * 100 + int(s)

    print "Template: "+templ
    print strTemplate(templ)
    print strTemplate(templ,expid=expid)
    print strTemplate(templ,expid=expid,yy=2008)
    print strTemplate(templ,expid=expid,yy=2008,mm=mm)
    print strTemplate(templ,expid=expid,yy=2008,mm=mm,dd=dd)
    print strTemplate(templ,expid=expid,yy=2008,mm=mm,dd=dd,h=h)
    print strTemplate(templ,expid=expid,yy=2008,mm=mm,dd=dd,h=h,m=m,s=s)
    print strTemplate(templ,expid=expid,nymd=nymd)
    print strTemplate(templ,expid=expid,nymd=nymd,nhms=nhms)
    print strTemplate(templ,expid=expid,dtime=dtime)

if __name__ == "__main__":
    _ut_strTemplate()

