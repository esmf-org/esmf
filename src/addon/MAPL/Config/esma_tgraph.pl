#!/usr/bin/perl
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
#
# Parses output of esma_timer.sh and prints simple graph.
# 
#.......................................................................

use Getopt::Std;         # command line options

getopts('bdefghin:qs:t');

if ( $#ARGV < 0 || $opt_h ) {
   usage()
}

my ($rows,$cols) = split(/ /,`/bin/stty size`);

$pat = '\+';

$tmax = 0;
$tmin = 9999999999;

while ( <> ) {

   @tokens = split;

   if ( /^[ ]*TIME/ ) {
      $name = $tokens[1];
      if ( $opt_i ) {
         if ( $name =~ m/_install/ ) {
            next;
         }
      }

      if ( ! $opt_f ) {
         $name =~ s/_install//;
         $name =~ s/_GridComp/_/;
         $name =~ s/GridComp/_/;
      }
      # print "Name: <$name>, what = <$tokens[2]>\n";
      $isdir = ( $name =~ /\+/ );
      if ( $isdir ) {
      }
      if ( $opt_d ) { $ok = $isdir; }
      else          { $ok = 1 - $isdir; } 
      if ( $ok ) {
         if ( "$tokens[2]" eq "beg" ) {
            if ( $tokens[3] < $tmin ) { $tmin = $tokens[3]; } 
            $beg{$name} = $tokens[3];
         }
         if ( "$tokens[2]" eq "end" ) {
            if ( $tokens[3] > $tmax ) { $tmax = $tokens[3]; } 
            $end{$name} = $tokens[3];
         }
      }
   }

}

$scale = 1 unless ( $scale = $opt_s );
foreach $name ( keys %beg ) { 
   $delta{$name} = $end{$name} - $beg{$name}; 
   $Del{$name} = int(0.5+($end{$name} - $beg{$name})/$scale); 
   $Beg{$name} = int(0.5+($beg{$name} - $tmin)/$scale);
   $End{$name} = int(0.5+($end{$name} - $tmin)/$scale);
}

if ( $opt_b ) {
   @Names =  sort bybeg keys %beg;
} elsif ( $opt_e ) {
   print "by del\n";
   @Names =  sort byend keys %beg;
} elsif ( $opt_t ) {
   @Names =  sort bydel keys %beg;
} else {
   @Names =  sort byend keys %beg;
}

if ( $opt_f ) {
   $name_len = (reverse sort { $a <=> $b } map { length($_) } @Names)[0];
   if ( $name_len > 50 ) { $name_len = 50 };
} else {
   $name_len = 20;
}

$prolog = 10 + $name_len;
$max_bar = $cols - $prolog;

$numnames = 0;
$maxnames = -1 unless ( $maxnames = $opt_n );

foreach $name ( @Names ) {

   last if ( $opt_n && $numnames >= $maxnames );

   $del = $delta{$name};
   $off = $beg{$name} - $tmin;
   if ( $opt_s ) {
      $del = $del / $opt_s;
      $off = $off / $opt_s;
   }
   $pad="";
   if ( ! $opt_g) {
      for $i ( 1..$off ) { $pad = $pad." "; }
   }
   if ( $del > $max_bar && $opt_g ) {
      for $i ( 1..$max_bar ) { $pad = $pad.'#';}
   } else {
      for $i ( 1..$del ) { $pad = $pad.'*'; }
   }
   if ( $del < 1 )   { $pad = $pad.'o';}
   if ( $opt_q ) { $pad = "";}
   printf("%5d | %${name_len}s |%s\n",$delta{$name}, substr($name,0,$name_len), $pad) if ( $del>0 );
   $numnames += 1;
}

printf(">>> Elapsed time: %d minutes\n",($tmax-$tmin+0.0)/60.); 

#.......................................................................

sub bybeg {
   $Beg{$a} <=> $Beg{$b}
      or
   $delta{$b} <=> $delta{$a}
      or
   $end{$b} <=> $end{$a}
      or
   $a cmp $b;
}

sub byend {
   $End{$b} <=> $End{$a}
      or
   $delta{$b} <=> $delta{$a}
      or
   $end{$b} <=> $end{$a}
      or
   $beg{$a} <=> $beg{$b}
      or
   $a cmp $b;
}

sub bydel {
   $Del{$b} <=> $Del{$a}
      or
   $delta{$b} <=> $delta{$a}
      or
   $Beg{$a} <=> $Beg{$b}
      or
   $a cmp $b;
}

#.......................................................................

sub usage {

   print<<'EOF';

NAME
     esma_tgraph - Parses output of esma_timer.sh and prints simple graph

SYNOPSIS

     esma_tgraph.pl [-b] [-d] [-f] [-g] [-h] [-i] [-n NUM] [-q] [-s dt] [-t] makelog_filename

DESCRIPTION

     Parses output of esma_timer.sh and prints simple graph, optionally
     sorting the names by beginning or ending time.

OPTIONS

     -b     sort by beginning time (default sorts by ending time)
     -d     prints times for directories, not individual files
     -f     prints the full name of the file (up to 50 characters)
     -g     prints info as bar graph
     -h     prints this usage information
     -i     ignore install timers
     -n NUM print NUM entries (useful with -t)
     -q     suppress the graph
     -s dt  time unit for display; default is dt=1 or 1 sec
     -t     sort by execution time

EXAMPLE

     An example way of running it is:
     
        esma_tgraph.pl -tgfi -n 20 make.log

     This will show the top twenty files that take the most time to compile.

SEE ALSO

     esma_timer.sh   the basic timing script

EOF

   exit(1)

}
