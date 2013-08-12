#
# File f90symbols.awk -- Gather a list of PIO public symbols
# $\Id$
#
# Take an .F90 file and generate a list of PIO public symbols (e.g., module
# names and non-module subroutines).
#
# Example:
# #define piolib_mod ESMFPIO_piolib_mod
#

BEGIN { IGNORECASE  = 1
        FIRSTSYMBOL = 1
}


#
# awk reads each line of the input file until it finds 
# a "module" or "public" statement
# 


/^[ \t]*module[ \t]+/ {

  # We only want the first module line to make things easier
  # Assume the second field is the F90 module name

  printf "%s", $2
  # We are done with this file
#  next
  exit
}

# If we get here (and find any matches), we are not in a module file
# Look for subroutine statements
/^[ \t]*subroutine[ \t]/ {
  sub(/[(].*$/,"",$2)
  if (FIRSTSYMBOL) {
    printf "%s", $2
    FIRSTSYMBOL = 0
  } else {
    printf ":%s", $2
  }
}
