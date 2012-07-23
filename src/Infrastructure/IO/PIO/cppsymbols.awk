#
# File cppsymbols.awk
#
# Take a C++ header file and generate a list of CPP #define statements to allow
# hiding of PIO public interfaces with the ESMFPIO_ prefix.
#
# Example:
# #define pio_cpp_initdecomp_dof ESMFPIO_pio_cpp_initdecomp_dof
#

BEGIN { IGNORECASE = 0
        PRLINE     = "#define %s ESMCPIO_%s\n"
        CPPFUNCID  = /[A-Za-z][A-Za-z0-9_]*\(?/
        CPPID      = /[*]?[A-Za-z][A-Za-z0-9_]*,?/
        INFUNCARGS = 0
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

# NB: The C++ interface functions are either void or return an int
/^[ \t]*void[ \t]+/ || /^[ \t]*int[ \t]+/ {

  if (! INFUNCARGS && ($2 ~ /[A-Za-z][A-Za-z0-9_]*\(?/)) {
    INFUNCARGS = 1
    sub(/\([A-Za-z][A-Za-z0-9_]*$/,"",$2)
    printf PRLINE, $2, $2
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

# /^[ \t]*int[ \t]+/ {

#   if ($2 ~ /[A-Za-z][A-Za-z0-9_]*\(?/) {
#     sub(/\([A-Za-z][A-Za-z0-9_]*$/,"",$2)
#     printf PRLINE, $2, $2
#   }
# }
