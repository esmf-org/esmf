#
# File f90symbols.awk
#
# Take an .F90 file and generate a list of CPP #define statements to allow
# hiding of PIO public interfaces with the ESMFPIO_ prefix.
#
# Example:
# #define piolib_mod ESMFPIO_piolib_mod
#

BEGIN { IGNORECASE  = 1
        PRLINE      = "#define %s ESMFPIO_%s\n"
        FOUNDMODULE = 0
        INPUBLIC    = 0
}


#
# awk reads each line of the input file until it finds 
# a "module" or "public" statement
# 


/^[ \t]*module[ \t]+/ {

  # We only want the first module line to make things easier
  if ( FOUNDMODULE ) next

  # Assume the second field is the F90 module name

#  sub(/,$/,"",$2)
  printf PRLINE, $2, $2
  FOUNDMODULE=1
}


# This will match public lines

/^[ \t]*public[ \t]/ {

  INPUBLIC=1
}

INPUBLIC {
  INPUBLIC=0
  for (i = 1; i <= NF; i++) {
    if ( $i ~ /^!/ ) break
    if ( $i ~ /[A-Za-z][A-Za-z0-9_]*,?/ ) {
      if ( $i !~ /public/ ) {
        sub(/,$/,"",$i)
        printf PRLINE, $i, $i
      }
    }
    if ( $i ~ /&/ ) {
      INPUBLIC=1
    }
  }
}
