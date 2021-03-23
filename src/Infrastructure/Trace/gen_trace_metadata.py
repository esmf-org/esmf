#!/usr/bin/env/python
#
# Jinja2 is required:
# http://jinja.pocoo.org/docs/2.10/intro/#installation
#
# See README in this directory.
#

from jinja2 import Environment

template_esmci_metadata_c = """

// $Id$
/*
 * Standard trace metadata used by all ESMF traces.
 *
 * Earth System Modeling Framework
 * Copyright 2002-2021, University Corporation for Atmospheric Research, 
 * Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
 * Laboratory, University of Michigan, National Centers for Environmental 
 * Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
 * NASA Goddard Space Flight Center.
 * Licensed under the University of Illinois-NCSA License.
 */

#include <string.h>
#include "ESMCI_Trace.h"

namespace ESMCI {

  std::string TraceGetMetadataString() {
    
    std::string metadata_string;
    metadata_string = ""
    {% for ln in lines %}"{{ln}}\\n"
    {% endfor %};

    return metadata_string;
  }
}
"""


#extern "C" {
#  {% for f in cfunc_list %}
#  {{f.ret}} __wrap_{{f.name}}({{f.params}});
#  {% endfor %}

def gen():

    lines = [line.rstrip('\n').replace('"', '\\"') for line in open('include/metadata', 'r')]
    
    template = Environment().from_string(template_esmci_metadata_c)
    text = template.render(lines=lines)

    f = open('src/ESMCI_TraceMetadata.C', 'w+')
    f.write(text)
    f.close()
    print 'Generated src/ESMCI_TraceMetadata.C'
        
if __name__ == '__main__':
    gen()
    
