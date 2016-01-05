#
# File cppsymbols.awk -- Create a header file for PIO public C++ interfaces
# $\Id$
#
# Take a C++ header file and generate a list of CPP #define statements to allow
# hiding of PIO public interfaces with the ESMFPIO_ prefix.
#
# Example:
# #define pio_cpp_initdecomp_dof ESMFPIO_pio_cpp_initdecomp_dof
#

BEGIN { IGNORECASE = 0
        PRLINE     = "#define %s esmc%s\n"
        CPPFUNCID  = /[A-Za-z][A-Za-z0-9_]*\(?/
        CPPID      = /[*]?[A-Za-z][A-Za-z0-9_]*,?/
        INFUNCARGS = 0
        # We ignore the first #ifndef in a C include file
        PREPROCPOS = -2
}


#
# awk reads each line of the input file until if finds a variable or
# function declaration.
# 

INFUNCARGS {
  # We assume there are no comments inside of function declarations
  for (i = 1; i <= NF; i++) {
    # Look for ); or ) ;
    if ( $i ~ /\);$/ ) {
      INFUNCARGS = 0
      break;
    } else if (( $i ~ /\)$/ ) && ( i < NF ) && ( $(i + 1) ~ /^;/ )) {
      INFUNCARGS = 0
      break;
    }
  }
  # We assume that there is no new function declaration on this line
  next
}

# We need to copy #ifdef/#endif statements to make sure that some definitions
# only happen at the correct times
# To avoid file clutter, we keep track of the current state and just wrap
# each definition with the current state
/^#[ \t]*if/ {
  PREPROCPOS = PREPROCPOS + 1
  ifdef[PREPROCPOS] = $0
  next
}

/^#[ \t]*endif/ {
  PREPROCPOS = PREPROCPOS - 1
  next
}

# NB: The C++ interface functions are either void or return an int
/^[ \t]*void[ \t]+/ || /^[ \t]*int[ \t]+/ {

  if (! INFUNCARGS && ($2 ~ /[A-Za-z][A-Za-z0-9_]*\(?/)) {
    INFUNCARGS = 1
    # Truncate the string at the open paren (if present)
    pos = match($2, /\(/)
    if (pos > 0) {
      fname = substr($2, 1, (pos - 1))
    } else {
      fname = $2
    }
    # Finally, print the definition in context
    for (i = 0; i <= PREPROCPOS; i++) {
      print ifdef[i]
    }
    printf PRLINE, fname, fname
    for (i = 0; i <= PREPROCPOS; i++) {
      print "#endif"
    }
  }
  # See if this is a one line function declaration
  for (i = 3; i <= NF; i++) {
    # Look for ); or ) ;
    if ( $i ~ /\);$/ ) {
      INFUNCARGS = 0
      break;
    } else if (( $i ~ /\)$/ ) && ( i < NF ) && ( $(i + 1) ~ /^;/ )) {
      INFUNCARGS = 0
      break;
    }
  }
}
