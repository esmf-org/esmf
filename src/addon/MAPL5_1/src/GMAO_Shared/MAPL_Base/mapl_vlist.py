#!/usr/bin/env python

import os, sys, argparse



# -----------------------------------------------------------------------------
# main program: get a list of fortran src files and *_Registr.rc files, parse
# them for IM/EX/IN states and write out the three lists
#
# Starting point is Arlindo da Silva's mapl_vlist.pl.
# -----------------------------------------------------------------------------
def main():

    # parse command line args
    # -----------------------
    comm_args = parse_args()
    ROOT_DIR = comm_args['root']
    FORMAT = comm_args['format']
    SORTBY = comm_args['sort']
    ONLYME = comm_args['self']


    # get lists of fort and rc files
    # ------------------------------
    fort_files, rc_files = find_files(ROOT_DIR, ONLYME)


    # lists to store variables
    # ------------------------
    im_list = list()  # import   states
    ex_list = list()  # export   states
    in_list = list()  # internal states


    # process fortran files
    # ---------------------
    for file in fort_files:
        prc_fort(file, im_list, ex_list, in_list)


    # process rc files
    # ----------------
    for file in rc_files:
        prc_rc(file, im_list, ex_list, in_list)
    

    # sort lists
    # ----------
    sort_list(im_list, SORTBY)
    sort_list(ex_list, SORTBY)
    sort_list(in_list, SORTBY)
    
    # print list of states in specified format
    # ----------------------------------------
    write_states(im_list, 'im', FORMAT)
    write_states(ex_list, 'ex', FORMAT)
    write_states(in_list, 'in', FORMAT)



# -----------------------------------------------------------------------------
# process *_Registry.rc files for IM/EX/IN states
# -----------------------------------------------------------------------------
def prc_rc(src, im_list, ex_list, in_list):
    assert(src.split('_')[-1].strip()=='Registry.rc')
    
    # flags for IM/EX/IN states
    # -------------------------
    imFlag  = 0
    exFlag  = 0
    inFlag  = 0
    inBlock = 0

    # read src file and process
    # -------------------------
    fin = open(src)
    for line in fin:
        line = line.strip()

        # skip comment/blank lines
        # ------------------------
        if not line or line[0] in ['!', '#']:
            continue

        tokens = line.split()
        KEYWORD = tokens[0].lower()

        # look for gridcomp name
        # ----------------------
        if KEYWORD == 'comp_name:':
            gc = prc_gc(tokens[1])
            continue
    
        # start of Import/Export/Internal block
        # -------------------------------------
        try:
            gc
        except NameError:
            gc = 'not found'
        lgc = gc.lower()
        if KEYWORD in ['<importspec',         '<exportspec',         '<internalspec',
                       '<%s::importspec'%lgc, '<%s::exportspec'%lgc, '<%s::internalspec'%lgc]:
            if   KEYWORD in ['<importspec',   '<%s::importspec'%lgc]:   imFlag = 1
            elif KEYWORD in ['<exportspec',   '<%s::exportspec'%lgc]:   exFlag = 1
            elif KEYWORD in ['<internalspec', '<%s::internalspec'%lgc]: inFlag = 1
            inBlock = imFlag + exFlag + inFlag
            continue

        # continue unless inside a spec call
        # ----------------------------------
        if not inBlock: continue

        # end of block - reset flags
        # --------------------------
        if KEYWORD in ['</importspec>',        '</exportspec>',        '</internalspec>',
                       '</%s::importspec'%lgc, '</%s::exportspec'%lgc, '</%s::internalspec'%lgc]:
            imFlag  = 0; exFlag = 0; inFlag  = 0; inBlock = 0
            
        # inside an import/export/internal block - save variable info
        # -----------------------------------------------------------
        lineSplt = line.split('|')
        if len(lineSplt) > 1:
            short = prc_short(lineSplt[0])
            units = prc_units(lineSplt[1])
            dims = prc_dims(lineSplt[2])
            vloc = lineSplt[3]
            long = prc_long(lineSplt[-1])

            if imFlag: im_list.append([short, gc, units, dims, long, vloc])
            if exFlag: ex_list.append([short, gc, units, dims, long, vloc])
            if inFlag: in_list.append([short, gc, units, dims, long, vloc])


# -----------------------------------------------------------------------------
# process fortran source files for 'add spec' calls
# -----------------------------------------------------------------------------
def prc_fort(src, im_list, ex_list, in_list):

    # flags for IM/EX/IN states
    # -------------------------
    imFlag  = 0
    exFlag  = 0
    inFlag  = 0
    inBlock = 0

    # read src file and process
    # -------------------------
    fin = open(src)

    for line in fin:
        line = line.strip()

        # skip comment/blank lines
        # ------------------------
        if not line or line[0] in ['!', '#']:
            continue

        tokens = line.split()
        KEYWORD = tokens[0].lower()

        # look for gridcomp name
        # ----------------------
        if KEYWORD == 'module' and tokens[1].lower() != 'procedure':
            gc = prc_gc(tokens[1])
            continue

        # start of Import/Export/Internal block
        # -------------------------------------
        if KEYWORD == 'call':
            method = tokens[1].lower().split('(')[0]
            if method in ['mapl_addimportspec', 'geos_stateaddimportspec']: 
                imFlag = 1
            elif method in ['mapl_addexportspec', 'geos_stateaddexportspec']:
                exFlag = 1
            elif method in ['mapl_addinternalspec', 'geos_stateaddinternalspec']:
                inFlag = 1
            inBlock = imFlag + exFlag + inFlag

            # for that rare case when all of spec call is in one line
            # e.g. call MAPL_AddExportSpec(GC, SHORT_NAME='LST', CHILD_ID=CATCH(1), RC=STATUS)
            if line[-1] == ')':
                imFlag  = 0; exFlag = 0; inFlag  = 0; inBlock = 0
            continue

        # continue unless inside a spec call
        # ----------------------------------
        if not inBlock: continue

        # end of block - save variable info, reset flags
        # ----------------------------------------------
        lword = line[-1]
        if lword == ')':
            try: units 
            except NameError: units = ''
            try: dims
            except NameError: dims = ''
            try: long
            except NameError: long = ''
            try: vloc
            except NameError: vloc = ''

            if imFlag: im_list.append([short, gc, units, dims, long, vloc])
            if exFlag: ex_list.append([short, gc, units, dims, long, vloc])
            if inFlag: in_list.append([short, gc, units, dims, long, vloc])

            imFlag  = 0; exFlag = 0; inFlag  = 0; inBlock = 0
            
        # inside a spec call block
        # ------------------------
        lineSplt = line.split('=')
        if len(lineSplt) > 1:
            key = trim(lineSplt[0]).lower()
            val = trim(lineSplt[1])
            if key == 'short_name': short = prc_short(val)
            if key == 'long_name': long = prc_long(val)
            if key == 'units': units = prc_units(val)
            if key == 'dims': dims = prc_dims(val)
            if key == 'vlocation': vloc = prc_vloc(val)



# -----------------------------------------------------------------------------
# sort list as specified
# -----------------------------------------------------------------------------
def sort_list(List, sortBy):
    numVars = len(List)
    
    if sortBy=='short': 
        List.sort(key=lambda x: x[0])
    elif sortBy=='comp': 
        List.sort(key=lambda x: x[1])
    elif sortBy=='both':
        List.sort(key=lambda x: x[0])
        List.sort(key=lambda x: x[1])
    elif sortBy=='none':
        pass
    else: raise Exception('sorting option [%s] not recognized' % sortBy)
    


# -----------------------------------------------------------------------------
# print list of states (IM/EX/IN) in specified format
# -----------------------------------------------------------------------------
def write_states(List, stateName, format):
    if len(List)>0:
        if   format=='ascii': __write_ascii__(List, stateName)
        elif format=='wiki': __write_wiki__(List, stateName)
        elif format=='latex': __write_latex__(List, stateName)
        else: raise Exception('output format [%s] not recognized' % format)


# -----------------------------------------------------------------------------
# print list of states (IM/EX/IN) in mediawiki format
# -----------------------------------------------------------------------------
def __write_wiki__(List, state):
    print '\n'
    print "__FORCETOC__"

    if   state=='ex': print '===Export States (%s) ===\n' % len(List)
    elif state=='im': print '===Import States (%s) ===\n' % len(List)
    elif state=='in': print '===Internal States (%s) ===\n' % len(List)
    else: raise Exception('state [%s] not recognized' % state)

    print '{| class="wikitable"'
    print '|+ List of GEOS-5 [%s] State variables' % state.upper()
    # print '! width="100px" | Name'
    # print '! width="100px" | Component'
    # print '! width="100px" | Units'
    # print '! width="50px"  | Dim'
    # print '! width="200px" | Long Name'
    print '! Name !! Component !! Units !! Dim !! Long name'

    for i in xrange(len(List)):
        short = List[i][0].strip()
        gc = List[i][1].strip()
        units = List[i][2].strip()
        dims = List[i][3].strip()
        long = List[i][4].strip()
        
        print '|-'
        print '| %s || %s || %s || %s || %s' % (short, gc, units, dims, long)

    print '|}'



# -----------------------------------------------------------------------------
# print ascii list of states (IM/EX/IN)
# -----------------------------------------------------------------------------
def __write_ascii__(List, state):
    print "\n\n"

    if   state=='ex': print 'Export States (%s):\n' % len(List)
    elif state=='im': print 'Import States (%s):\n' % len(List)
    elif state=='in': print 'Internal States (%s):\n' % len(List)
    else: raise Exception('state [%s] not recognized' % state)
    
    print(" ----------------|------------|------------|-------|-" 
          "---------------------------------------------------")
    print("   Name          | Component  |    Units   | Dim   | Long Name")
    print(" ----------------|------------|------------|-------|-"
          "---------------------------------------------------")  

    for i in xrange(len(List)):
        short = List[i][0]
        gc = List[i][1];
        units = List[i][2]
        dims = List[i][3]
        long = List[i][4]
        if short.strip().__len__() > 15:
            short = short + '...\n ' + '.'*15
        if units.strip().__len__() > 10:
            long += ' [' + units + ']'
            units = '--->'
        print ' %-15s | %-10s | %-10s | %-5s | %s' % (short, gc, units, dims, long)

    print(" ----------------|------------|------------|-------|-" 
          "---------------------------------------------------")



# -----------------------------------------------------------------------------
# print list of states (IM/EX/IN) as a latex longtable
# -----------------------------------------------------------------------------
def __write_latex__(List, state):
    
    if   state=='ex': print 'Export states (%s):\\\\' % len(List)
    elif state=='im': print 'Import States (%s):\\\\' % len(List)
    elif state=='in': print 'Internal States (%s):\\\\' % len(List)
    else: raise Exception('state [%s] not recognized' % state)

    print '\\begin{longtable}{p{1in} | p{0.8in} p{0.5in} p{3.1in}}'
    print '\\hline'
    print '{\\sf Short Name} & {\\sf Units} & {\\sf Dims} & {\\sf Long name}\\\\'
    #print 'Short Name & Units & Dims & Long name\\\\'
    print '\\hline'
    print '\\endhead'

    for i in xrange(len(List)):
        short = List[i][0].strip().replace('_','\\_')
        gc = List[i][1].strip().replace('_','\\_')
        units = List[i][2].strip().replace('_','\\_')
        dims = List[i][3].strip().replace('_','\\_')
        long = List[i][4].strip().lower().replace('_','\\_')
        vloc = List[i][5].strip().replace('_','\\_')
        print ' \\texttt{%s} & %s & %s & %s\\\\' % (short, units, dims, long)

    print '\\hline'
    print '\\end{longtable}'



# -----------------------------------------------------------------------------
# trim string
# -----------------------------------------------------------------------------
def trim(str):
    str = str.replace("'","").replace(',','').replace(':','')
    return str.replace('/','').replace('"','').strip()



# -----------------------------------------------------------------------------
# process gridcomp names
# -----------------------------------------------------------------------------
def prc_gc(str):
    str = trim(str)
    str = str.replace('GridComp','').replace('_Mod','').replace('Mod', '')
    str = str.replace('MAPL_','').replace('GEOS_','')
    return str



# -----------------------------------------------------------------------------
# process short names
# -----------------------------------------------------------------------------
def prc_short(str):
    str = trim(str).split()[0]
    if 'TRIM(' in str.upper(): return 'UNKNOWN'
    else: return str



# -----------------------------------------------------------------------------
# process long names
# -----------------------------------------------------------------------------
def prc_long(str):
    str = trim(str).split('&')[0].strip().replace('_',' ')
    if 'TRIM(' in str.upper(): return 'UNKNOWN'
    else: return str



# -----------------------------------------------------------------------------
# process dimensions
# -----------------------------------------------------------------------------
def prc_dims(str):
    str = str.split('&')[0].lower().strip().replace('mapl_','')
    if   str == 'dimshorzvert': return 'xyz'
    elif str == 'dimshorzonly': return 'xy'
    elif str == 'dimsvertonly': return '  z'
    elif str == 'dimstileonly': return 'tile'
    elif str == 'dimstiletile': return 'tile2'
    elif str == 'xyz'or str == 'xy' or str == 'z' or str == 'tile': return str
    else: return 'unk'



# -----------------------------------------------------------------------------
# process units
# -----------------------------------------------------------------------------
def prc_units(str):
    str = trim(str.strip().split('&')[0].strip())
    if 'TRIM' in str.upper(): return 'UNKNOWN'
    else: return str



# -----------------------------------------------------------------------------
# process vlocation
# -----------------------------------------------------------------------------
def prc_vloc(str):
    str = trim(str.strip().split('&')[0].strip().split()[0].strip())
    return str.replace('MAPL_VLocation','')



# -----------------------------------------------------------------------------
# find fortran source files and *_Registry.rc files under rootdir
# -----------------------------------------------------------------------------
def find_files(rootdir, ONLYME):

    fort_files = list()
    rc_files = list()

    if ONLYME:
        files = os.listdir(rootdir)
        for file in files:
            if '.f' in file.lower() or '.f90' in file.lower() or '.p90' in file.lower():
                fort_files.append(rootdir+os.sep+file)
            if '_Registry.rc' in file:
                rc_files.append(rootdir+os.sep+file)
    else:
        for path, subdirs, files in os.walk(rootdir):
            for file in files:
                if os.path.splitext(file)[1].lower() in ['.f', '.f90', '.p90']:
                    fort_files.append(os.path.join(path,file))
                if '_Registry.rc' in file:
                    rc_files.append(os.path.join(path,file))
    
    return [fort_files, rc_files]



# -----------------------------------------------------------------------------
# parse command line arguments and return a dict
# -----------------------------------------------------------------------------
def parse_args():
    
    DESCRIPTION = """
DESCRIPTION

     This utility finds Fortran files containing 'add spec' calls and
     *_Registry.rc files listing the state variables and creates three
     lists (one each for Import, Export and Internal states) either as
     ascii text or in the MediaWiki format. One can sort the lists by
     short name (short) or component name (comp) or both.

     The GEOS-5 coding style is implicitly assumed. Here is an example
     of a typical 'add spec' code fragment in Fortran source file:

     call MAPL_AddImportSpec ( GC,                               &
          LONG_NAME  = 'mass_fraction_of_cloud_ice_in_air',      &
          UNITS      = '1',                                      &
          SHORT_NAME = 'QI',                                     &
          DIMS       = MAPL_DimsHorzVert,                        &
          VLOCATION  = MAPL_VLocationCenter,                     &
          AVERAGING_INTERVAL = ACCUMINT,                         &
          REFRESH_INTERVAL   = MY_STEP,                          &
          RC=STATUS  )
     
     Notice that arguments are entered one per continuation line. 
     The module name is derived from the Fortran "Module" keyword in
     the file, trimming out any trailing "_GridComp*" and leading
     "GEOS_". For example, the statement

           Module GEOS_SolarGridCompMod

     would be interpreted as the having "Solar" as the component name.
     Notice this is related but not exactly the Component name entering
     a MAPL_History resource file. 

EXAMPLE

     % ./mapl_vlist.py --root /path/to/GEOSgcm_GridComp --sort comp --format
"""

    p = argparse.ArgumentParser(description=DESCRIPTION,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('--root',   help='[RQD] top level GridComp directory', required=True)
    p.add_argument('--sort',   help='[RQD] argument to sort by (short/comp/both/none)', required=True)
    p.add_argument('--format', help='[RQD] output format (ascii/wiki/latex)', required=True)
    p.add_argument('--self',   help='[OPT] only the specified dir (default: sub dirs as well)', action='store_true')

    args = vars(p.parse_args()) # vars converts to dict

    # some checks
    # -----------
    if args['sort'] not in ['short', 'comp', 'both', 'none']:
        sys.exit('error: sort is one of short/comp/both/none')
    
    if args['format'] not in ['ascii', 'wiki', 'latex']:
        sys.exit('error: format is one of ascii/wiki/latex')

    if not os.path.isdir(args['root']):
        sys.exit('error: root dir [%s] not found' % args['root'])


    return args



# -----------------------------------------------------------------------------
# main program
# -----------------------------------------------------------------------------
if __name__=="__main__":
    main()
