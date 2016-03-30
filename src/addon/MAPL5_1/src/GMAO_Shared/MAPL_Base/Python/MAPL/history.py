"""
A special class for handling history resources.
"""

from config import *

class History(Config):

    def collections(self):
        """
        Returns a list of active collections.
        """
        p = re.compile('^[ ]*::')
        on = False
        for line in self.Lines:
            tok = line.lstrip()
            if tok[0:11] == 'COLLECTIONS':
                first = self.get('COLLECTIONS')
                if first != '':
                    colls = [ first.replace("'",""), ]
                else:
                    colls = []
                on = True
            elif on is True:
                if tok[0:2] == '::':
                    break
                elif tok[0:1] != '#':
                    coll = tok.split()[0]
                    colls.append(coll.replace("'",""))
        return colls

    def arc(self,outFile):
        """
        Create a PESTO resource file (.arc) based on the
        *.temkplate resources.
        """
        dict = self.regex('template$')
        Tmpl = [str.replace("'","").replace(",","") for str in dict.values()]
        Coll = [ str.split('.')[0].replace(",","")  for str in dict.keys()  ]
        Arch = [str.replace("'","").replace(",","") \
                for str in self.regex('archive$').values()]

        if len(Tmpl) != len(Arch):
            raise IOError,\
            "There are %d template resources but only %d archive resources."\
            %(len(Tmpl),len(Arch))

        header = '# PESTO resource for History Collections ' + \
                 '(automatically generated - do not edit)'
        Text = [header,]        
        c = 0
        for tmpl in Tmpl:
            coll = Coll[c]
            path = Arch[c].replace('%c',coll)
            line = '$PESTOROOT%s/' + path + '/%s.' + coll + \
                   '.' + tmpl + '\n'
            Text.append(line)
            c = c + 1
        open(outFile,"w").writelines(Text)
