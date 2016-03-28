#!/bin/sh

:
usage() {

cat<<EOF
NAME
     esma_timer - prints timing info for profiling the build
          
SYNOPSIS

     esma_timer.sh name beg
     esma_timer.sh name end
     esma_timer.sh name run cmd
          
DESCRIPTION

     Uses the date command to print out bracketing seconds and nanoseconds 
     for timing execution of commands; see examples below. This output is
     intended to be post-processed for creating the necessary reports.

EXAMPLES

     1) User executes the command

        % esma_timer.sh sleep beg
        % sleep 3
        % esma_timer.sh sleep end

     which resulys in

        TIME sleep beg 1167770701 336430000
        TIME sleep end 1167770704 341670000
    
     2) This is equivalent to

        % esma_timer.sh sleep run sleep 3

BUGS
    The actual running of date adds overhead to the timings, but
    this is small. 
    
SEE ALSO
    esma_tgraph.pl - displays timing results in (ASCII) graphical form.

EOF
exit 0 # so it does not stop compilation in case of misuse
}

#.................................................................

if [ $# -lt 2 ]; then
  usage
fi

# Parse command line args
# -----------------------
  name=$1
  shift
  what=$1
  shift
  if [ $# -lt 1 ]; then
    cmd='echo Nothing to run'
  else
    cmd=$*
  fi

  if [ "$what" != "end" ];  then
        date "+TIME $name beg %s %N"
  fi

  if [ "$what" = "run" ];  then
     $cmd
  fi

  if [ "$what" != "beg" ];  then
        date "+TIME $name end %s %N"
  fi

  exit 0

usage:

