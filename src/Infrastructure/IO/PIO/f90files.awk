#
# File f90files.awk -- Modify public PIO public symbols
# $\Id$
#
# Take an .F90 file and modifies public symbols (e.g., symbols found in use
#  statements) to begin with esmfpio. This allows the re-defined PIO public
#  symbols in libesmf.a to coexist with an independent libpio.a.
#

BEGIN {
  IGNORECASE  =  0
  FOUNDMODULE =  0
  INPUBLIC    =  0
  PREPROCPOS  = -1
  split(PUBSYMS, pubsyms, ":")
  for ( sym in pubsyms ) {
    # We need to match a Fortran symbol but are only comparing lowercase
    matchsym[sym] = "([^a-z_])" tolower(pubsyms[sym]) "([^a-z0-9_]|$)"
  }
  # We prepend ESMF<sym> if <sym> begins with pio, otherwise ESMFPIO_
  for ( sym in pubsyms ) {
    if (tolower(substr(pubsyms[sym], 1, 3)) ~ /pio/) {
      if (substr(pubsyms[sym], 1, 1) == "P") {
        matchsub[sym] = "ESMF" pubsyms[sym]
      } else {
        matchsub[sym] = "esmf" pubsyms[sym]
      }
    } else {
      if (substr(pubsyms[sym], 1, 1) ~ /[A-Z]/) {
        matchsub[sym] = "ESMFPIO_" pubsyms[sym]
      } else {
        matchsub[sym] = "esmfpio_" pubsyms[sym]
      }
    }
  }
}


#
# awk reads each line of the input file and converts each public symbol to
#     lowercase
# 
# 

# Skip any preprocessor line (i.e., just print)
($1 ~ /^\#/) {
  print
  next
}

# Skip any comment line (i.e., just print)
($1 ~ /^!/) {
  print
  next
}

# The general case
{
  symfound = 0
  # Look for any public symbol on the line
  for ( sym in pubsyms ) {
    do {
      pos = match(tolower($0), matchsym[sym], matcharr)
      if (pos > 0) {
        symfound = 1
        # The first part of the sub includes the char before the matched sym.
        zstart = substr($0, 1, pos)
        # The last part of the sub starts just after the matched sym
        zend = substr($0, (pos + length(pubsyms[sym]) + 1))
        $0 = zstart matchsub[sym] zend
      } else {
        symfound = 0
      }
    }
    while (symfound)
  }
  print
}
