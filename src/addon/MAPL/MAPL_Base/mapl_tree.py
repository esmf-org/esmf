#!/usr/bin/env python
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

import os, sys, glob
import itertools, argparse


class MAPL_Tree():
    """
    #-----------------------------------------------------------------------
    # defines a dict gc_dir with key: comp_name, val: corresponding_dir
    # the method getDirFromComp returns takes a component name and returns
    # the corresponding dir name
    #-----------------------------------------------------------------------    
    """

    def __init__(self, output_format='ascii', output_color=False, add_link=False):
        """
        #-----------------------------------------------------------------------    
        # defines the following:
        #
        # attributes:
        #   name
        #   dict gc_dir
        #   out_typ
        # 
        # for output type mindmap, prints <map version="0.9.0">
        #-----------------------------------------------------------------------    
        """

        # who am i
        # --------
        self.name = 'mapl_tree'
        
        # output format
        # -------------
        self.out_format = output_format

        # output color
        # color surface and children green
        # --------------------------------
        self.out_color = output_color

        # add external links
        # ------------------
        self.add_link = add_link

        # how much space
        # --------------
        self.space = '    '

        # header for mindmap output
        # -------------------------
        if self.out_format=='mindmap':
            print '<map version="0.9.0">'

        # the dictionary
        # --------------
        self.gc_dir = dict()
        self.gc_dir['gcs'] = 'GEOSgcs_GridComp'
        self.gc_dir['gcm'] = 'GEOSgcm_GridComp'
        self.gc_dir['ana'] = None
        self.gc_dir['dataatm'] = None
        self.gc_dir['agcm'] = 'GEOSagcm_GridComp'
        self.gc_dir['aiau'] = 'GEOSmkiau_GridComp'
        self.gc_dir['adfi'] = None
        self.gc_dir['ogcm'] = 'GEOSogcm_GridComp'
        self.gc_dir['scmdynamics'] = None
        self.gc_dir['superdynamics'] = 'GEOSsuperdyn_GridComp'
        self.gc_dir['physics'] = 'GEOSphysics_GridComp'
        self.gc_dir['dyn'] = 'FVdycoreCubed_GridComp'
        self.gc_dir['gwd'] = 'GEOSgwd_GridComp'
        self.gc_dir['moist'] = 'GEOSmoist_GridComp'
        self.gc_dir['surface'] = 'GEOSsurface_GridComp'
        self.gc_dir['turbulence'] = 'GEOSturbulence_GridComp'
        self.gc_dir['chemistry'] = 'GEOSchem_GridComp'
        self.gc_dir['radiation'] = 'GEOSradiation_GridComp'
        self.gc_dir['saltwater'] = 'GEOSsaltwater_GridComp'
        self.gc_dir['lake'] = 'GEOSlake_GridComp'
        self.gc_dir['landice'] = 'GEOSlandice_GridComp'
        self.gc_dir['land'] = 'GEOSland_GridComp'
        self.gc_dir['vegdyn'] = 'GEOSvegdyn_GridComp' 
        self.gc_dir['catch'] = 'GEOScatch_GridComp'
        self.gc_dir['chemenv'] = None
        self.gc_dir['pchem'] = 'GEOSpchem_GridComp'
        self.gc_dir['gocart'] = 'GOCART_GridComp'
        self.gc_dir['gaas'] = 'GAAS_GridComp'
        self.gc_dir['h2o'] = None
        self.gc_dir['stratchem'] = 'StratChem_GridComp'
        self.gc_dir['gmichem'] = 'GMIchem_GridComp'
        self.gc_dir['carma'] = 'CARMAchem_GridComp'
        self.gc_dir['geoschem'] = None
        self.gc_dir['matrix'] = None
        self.gc_dir['mam'] = 'MAMchem_GridComp'
        self.gc_dir['solar'] = 'GEOSsolar_GridComp'
        self.gc_dir['irrad'] = 'GEOSirrad_GridComp'
        self.gc_dir['satsim'] = 'GEOSsatsim_GridComp'
        self.gc_dir['obio'] = 'GEOSoceanbiogeochem_GridComp'
        self.gc_dir['orad'] = 'GEOSorad_GridComp'
        self.gc_dir['seaice'] = 'GEOSseaice_GridComp'
        self.gc_dir['ocean'] = 'GEOSocean_GridComp'



    def __del__(self):
        """
        #-----------------------------------------------------------------------    
        # for output format mindmap, prints </map>
        #-----------------------------------------------------------------------    
        """

        if self.out_format=='mindmap':
            print '</map>'



    def getDirFromComp(self, comp):
        """
        #-----------------------------------------------------------------------
        # returns the dir name corresponding to the gridcomp
        # returns None if gridcomp name is not in the list
        #-----------------------------------------------------------------------
        """

        comp = comp.lower()
        if comp in self.gc_dir.keys():
            return self.gc_dir[comp]
        else:
            return None



    def get_link(self, compname):
        """
        #-----------------------------------------------------------------------
        # returns an (external) link corresponding to the component
        # if no link exists, returns None
        #-----------------------------------------------------------------------
        """
        
        if compname.lower() == 'agcm':
            return 'http://geos5.org/wiki/index.php?title=AGCM_Structure'
        elif compname.lower() == 'ogcm':
            return 'http://geos5.org/wiki/index.php?title=OGCM_Structure'
        elif compname.lower() == 'aana':
            return 'http://geos5.org/wiki/index.php?title=Atmospheric_Analysis'
        else:
            return None



    def get_color(self, compname):
        """
        #-----------------------------------------------------------------------
        # returns color of the component. may be None
        #-----------------------------------------------------------------------
        """

        if compname.lower() in ['surface', 'lake', 'landice', 
                                'land', 'saltwater', 'vegdyn', 'catch']:
            return '#008000'
        else:
            return None



    def write_comp(self, compname, level):
        """
        #-----------------------------------------------------------------------
        # write gridcomp name to stdout in the format specified by self.out_typ
        #-----------------------------------------------------------------------
        """

        if self.add_link: MYLINK = self.get_link(compname)
        else: MYLINK = None
        if self.out_color: MYCOLOR = self.get_color(compname)
        else: MYCOLOR = None

        if self.out_format=='mindmap':
            txt2prnt = level*self.space + '<node TEXT="' + compname + '"'
            if MYLINK:
                txt2prnt += ' LINK="' + MYLINK + '"'
            if MYCOLOR:
                txt2prnt += ' COLOR="' + MYCOLOR + '"'
            print txt2prnt +'>'
            
        elif self.out_format=='ascii':
            print level*('|'+self.space) + compname
        else:
            raise Exception('output format [%s] not recognized' % self.out_format)
            


    def write_end(self, level):
        """
        #-----------------------------------------------------------------------
        # for mindmap (xml) output, we need a </node>
        #-----------------------------------------------------------------------
        """

        if self.out_format=='mindmap':
            print level*self.space + '</node>'



    def traverse_chname(self, parentdir, parentcomp, level=0):
        """
        #-----------------------------------------------------------------------
        # recursive function that traverses the directory struture and prints
        # out either an ascii table or xml type tree structure of GEOS-5
        #
        # inputs:
        #    parentdir:  root GEOS-5 directory
        #    parentcomp: name of the root component
        #    level:      counter for recursion (default 0)
        #
        # output:
        #    tree structure of GEOS-5 component names (children)
        #-----------------------------------------------------------------------
        """

        parentdir = parentdir.rstrip(os.sep)

        # list of GridComp src files
        # --------------------------
        filelist = os.listdir(parentdir)
        src_files = glob.glob(parentdir+os.sep+'*GridComp.F90')


        # get children comp names from src files
        # ch_comps is a list of lists, and needs to be flattened
        # ------------------------------------------------------
        ch_comps = list()
        for file in src_files:
            ch_comps.append(self.parse_fort_src(file))
        ch_comps = list(itertools.chain(*ch_comps))


        # write comp name to stdout
        # -------------------------
        self.write_comp(parentcomp, level)


        # for each child comp, if a corresponding
        # directory exists, recurse. else print child name
        # ------------------------------------------------
        for child_comp in ch_comps:
            child_dir = self.getDirFromComp(child_comp)
            if child_dir:
                CH_DIR = parentdir + os.sep + child_dir
                self.traverse_chname(CH_DIR, child_comp, level+1)
            else:
                self.write_comp(child_comp, level+1)
                self.write_end(level+1)


        # write end to stdout (xml)
        # -------------------------
        self.write_end(level)



    def parse_fort_src(self, fortsrc):
        """
        #-----------------------------------------------------------------------
        # process gridcomp src file and return a list of children components
        #
        # inputs:
        #    fortsrc: fortran src file
        #
        # output:
        #    list of children components
        #-----------------------------------------------------------------------
        """

        children = list()
        fin = open(fortsrc, 'r')
        for line in fin:
            if 'mapl_addchild' in line.lower():
                child = line.split(',')[1].split('=')[1].replace("'","").replace('"','').lower()
                children.append(child)
        fin.close()
        return children


    def traverse_dirname(self, parentdir, level=0):
        """
        #-----------------------------------------------------------------------
        # recursive function that traverses the directory struture and prints
        # out either an ascii table or xml type tree structure of GEOS-5 by
        # processing the dir names
        #
        # inputs:
        #    parentdir:  root GEOS-5 directory
        #    level:      counter for recursion (default 0)
        #
        # output:
        #    tree structure of GEOS-5 component names (directory names)
        #-----------------------------------------------------------------------
        """

        parentdir = parentdir.rstrip(os.sep)

        # write comp name to stdout
        # -------------------------
        NAM2WRT = parentdir.split('/')[-1].split('_')[0].replace('GEOS','').replace('GMAO','')
        self.write_comp(NAM2WRT, level)

        # get subdirectories of parentdir, consider 
        # only those that end with _GridComp or ModPlug
        # ---------------------------------------------
        dirlist = os.listdir(parentdir)
        chdirs = list()
        for subdir in dirlist:
            if os.path.isdir(parentdir + os.sep + subdir):
                if ('_GridComp' in subdir) or ('PlugMod' in subdir):
                    chdirs.append(parentdir + os.sep + subdir)

        # if chdirs is non-empty, recurse, else do nothing
        # ------------------------------------------------
        if chdirs:
            for chdir in chdirs:
                self.traverse_dirname(chdir, level+1)

        # write end to stdout (xml)
        # -------------------------
        self.write_end(level)


# end class MAPL_Tree



def main():
    """
    # --------------------------------------------------------------------------
    # main function
    # --------------------------------------------------------------------------
    """

    comm_opts = parse_args()
    ROOTDIR   = comm_opts['rootdir']
    ROOTCOMP  = comm_opts['rootcomp']
    OUT_TYPE  = comm_opts['outtype']
    OUT_FORM  = comm_opts['outform']
    OUT_COLOR = comm_opts['color']
    ADD_LINK  = comm_opts['link']

    MT = MAPL_Tree(OUT_FORM, OUT_COLOR, ADD_LINK)
    if OUT_TYPE=='chname': MT.traverse_chname(ROOTDIR, ROOTCOMP)
    elif OUT_TYPE=='dirname': MT.traverse_dirname(ROOTDIR)
    else: raise Exception('output type [%s] not recognized' % OUT_TYPE)



def parse_args():
    """
    # --------------------------------------------------------------------------
    # parse command line arguments and return a dict of options
    # --------------------------------------------------------------------------
    """

    p = argparse.ArgumentParser(epilog= "Program to create a hierarchical "
                                "tree of GridComps. This is done either by reading "
                                "the _GridComp directory names or parsing the GridComp "
                                "source code for children names. Output is either an "
                                "ascii table or an xml type file readable by Freemind.")

    # top level directory, output type
    # --------------------------------
    p.add_argument('--rootdir', help='top level GridComp directory', required=True)
    p.add_argument('--rootcomp', help='top level GridComp name', required=True)
    p.add_argument('--outtype', help='output type (dirname/chname)', required=True)
    p.add_argument('--outform', help='output format (ascii/mindmap)', required=True)
    p.add_argument('--color', help='color nodes (edit MAPL_Tree::get_color)', action='store_true')
    p.add_argument('--link', help='add external link to nodes (edit MAPL_Tree::get_link)', action='store_true')

    args = vars(p.parse_args()) # vars converts to dict

    # checks on input values
    # ----------------------
    if '_GridComp' not in args['rootdir']:
        raise Exception('rootdir is not a GridComp')
    if not os.path.isdir(args['rootdir']):
        raise Exception('rootdir [%s] does not exist' % args['rootdir'])
    if args['outtype'] not in ['dirname', 'chname']:
        raise Exception('output type is one of dirname/chname, not [%s]' % args['outtype'])
    if args['outform'] not in ['ascii', 'mindmap']:
        raise Exception('output format is one of ascii/mindmap, not [%s]' % args['outform'])

    return args



if __name__=="__main__":
    main()
    
