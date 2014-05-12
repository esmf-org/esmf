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
# Resource file editor.
#
# REVISION HISTORY:
# 03Mar2005  da Silva  First crack.
# 11Mar2005
# 16MAR2005 Added File Browse feature using getOpenFile widget. DirTree
#	    Changed Fonts and sizes, Main title bar displays output
#	    Main window big title displays resource name, [Save] dded
# 18MAR2005 J. Yoon Added &CheckBox widget. widget line in RCfile 
#           different from other lines and parsed differently. 
# 21Mar2005 J. Yoon added &CheckBox with simpler processing of checkbox 
#           line without having to edit the parsing subroutine. 
#           Also added FILE save as, reload, exit, EDIT vi emacs xemacs nedit
# 25Mar2005 J. Yoon ability to edit more than one file at a time
# 30Mar2005 J. Yoon fixed window resize bug, flipped CHECKBOX to left side.
#           minor change to frame design layout, leftside menu clickable
#           right side widget and file/button, widget automatically updates
#           added HELP About RED, RC file, Revision history           
#           embedded NASA meatball inside of script using MIME Base64
# 04Apr2005 J. Yoon [Next] button appears if there are more than one files
#           [Finish] button for the last file  [Cancel] will exit
#           leftside menu has labels clickable rather than buttons
# 05Apr2005 J. Yoon on the leftt, use resource name rather than file name
#           left some space between the meatball and the recource list; 
#           flush left the resource list.
# 06Apr2005 J. Yoon fixed Help:/About Chem_Registry.RC coredumps 
#           fixed Help/Revision history: displays garbage
#           fixed Check box. If one unchecks all the boxes 
#           and click on [back] then it retains previous info.
# 08Apr2005 J. Yoon Fixed Geometry and window-resize related issues.
# 20Apr2005 J. Yoon Improved Geometry windowsize as function of font, 
#           Fixed checkbox bug, Added Instructions in About menu, 
#           Automatic Reload after EDIT, Fixed Directory Browse Bug
# 25Apr2005 J. Yoon run-external-program option added, outputfile at top,
#            updated layout. 
# 29Aug2009 da Silva added &Eval, &Assert, fixed window size calculation,
#           text UI and no UI.
#........................................................................

use Env;                 # Environment variables
use Getopt::Std;         # command line options
#use Tcl::Tk qw(:perlTk);
use Tk;                  # Tk GUI
use Tk::DirTree;         # Tk Directory
use FindBin;             # so we can find where this script resides
use File::Path;          # for mkpath()
use File::Copy "cp";     # for cp()
use File::Basename;      # for basename(), dirname()
use MIME::Base64 ();	 # for MIME base 64 logo

# Defaults and such
# -----------------
  Init();

# No user interaction...
# ----------------------
  if ($opt_d) {

    for ( my $f=0; $f<=$#RCfilename; $f++ ) { No_UI($RCfilename[$f]); }

  }        

# Text UI ...
# -----------
  elsif ($opt_t) { 

    for ( my $f=0; $f<=$#RCfilename; $f++ ) { Text_UI($RCfilename[$f]); }

#   ... or TK UI
#   ------------
   } else { 

    Tk_UI(); 

 }  
  
 if ($opt_x) { 
    foreach my $a (keys %outfn) { 
    	system("$opt_x $a");
    }    
 } 

# All done
# --------
  exit(0);

#.........................................................................

sub Init {

    chomp($ARCH = `uname -s`);
    chomp($MACH = `uname -m`);
    chomp($SITE = `uname -n`);

$, = ' ';		# set output field separator
$\ = " ";		# set output record separator

redmapl_mime();	# nasa logo in MIME base64 format

$ROOTDIR  = "";

%RCname  = ();		# Resource File name hash
%RCtitle = ();		# Resource title hash
%Line    = ();		# Actual RC lines hash

$elementfontsize  = 12;
$elementfont      = "Arial $elementfontsize bold";

#$rightwidget_width = 1020;
$textentrywidth   = 18;
$textqcolor 	  = "#FEFEFE";
$textentrycolor   = "#EFEFDF";
$textitemcolor 	  = "#DFCFCF";
$textframecolor   = "#FFFEF0";

$titlecolor  	  = "#DCF1dF";
$radioqcolor 	  = "#FEFEFE";

#$radioframecolor = "#FFEEEE";
$radioframecolor  = "#FFFEF0";
$radioitemcolor   = "#DFCFCF";
$radiobuttoncolor = "#EFEFDF";

$checkframecolor  = "#FFFEF0";
$checkbuttoncolor = "#EFEFDF";
$checkbackground  = "#FEFEFE";

$filebrowsecolor  = "#FEFEFE";
$filebrowseframe  = "#FFEEEE";
$filelabelcolor   = "#FEFEFE";
$fileframecolor	  = "#FFFEF0";
$fileentrycolor   = "#EFEFDF";

$dirbrowsecolor   = "#FEFEFE";
$dirbrowseframe   = "#FFEEEE";
$dirlabelcolor    = "#FEFEFE";
$dirframecolor	  = "#FFFEF0";
$direntrycolor    = "#EFEFDF";

$buttoncolor 	  = "#EFE4CF";
$buttonframecolor = "#EEEEF1";

$mainframecolor   = "#E1DAE0";

# Command line options
# --------------------
  getopts('Dgdevhto:x:');

  usage() if ( $opt_h || $#ARGV < 0 );
  $debug = $opt_D;
  $outfn = "-" unless ( $outfn = $opt_o ); 

  my $a = 0; $b = 0; 
  my ($newfile, $ofile);
  while ($a <= $#ARGV ) { 
     if ($ARGV[$a] =~ /\.irc$/i ) { 
        if ($ARGV[$a] =~ /\.rc$/) { 
		$ofile = $ARGV[$a];
		$newfile = $ofile . "~";
		system("cp -f $ofile $newfile");
	} 
	$RCfilename[$b] = $ARGV[$a];
        Parse_RCfile($RCfilename[$b]);
       	$b++;
     } 
     $a++;       

  } 

  print "";

}

#.........................................................................

sub Parse_RCfile { 
    my ($file) = @_;

    $i = -1;  # line index
    $j = -1;  # help index
    $k = -1;  # history index
    $l = -1;  # GUI index (widgets+questions)
    $m = -1;  # GUI index (actual questions)
    
# Main loop over file
# -------------------
open (FILE, $file);
LINE:
while (<FILE>) {

    chop;     # strip record separator

    $i++;
    $Line[$i]{$file} = $_;    # store all lines for later output

    @Fld  = split(' ', $_, 9999);

    if ( $Fld[0] eq '#BOP' ) {
         $doing_help = 0;         
         $doing_hist = 0;         
         $doing_prol = 1;         
         next LINE;
     }

    if ( $Fld[1] eq '!RESOURCE:' ) {
         $RCname{$file} = $Fld[2];
         shift @Fld;
         shift @Fld;
         shift @Fld;
         shift @Fld;
         @RCtitle = @Fld;
	 $RCtitle{$file} = "@RCtitle";
         next LINE;
    }

    if ( $Fld[1] eq '!HELP:' ) {
         $doing_help = 1;         
         next LINE;
     }

     if ( $Fld[1] eq '!REVISION' ) {
         $doing_help = 0;         
         $doing_hist = 1;         
         next LINE;
     }

     if ( $Fld[0] eq '#EOP' ) {
         $doing_help = 0;         
         $doing_hist = 0;         
         $doing_prol = 0;         
         next LINE;
     }

    if ( $doing_help ) {
        $j++;
	$Help[$j]{$file} = $Line[$i]{$file};
	$Help[$j]{$file} =~ s/^\#/ /;
    }

    if ( $doing_hist ) {
        $k++;
	$Hist[$k]{$file} = $Line[$i]{$file};
	$Hist[$k]{$file} =~ s/^\#/ /;
    }

    next LINE if ( $doing_prol );

    ( $tmp, $cmd ) = split('#', $_,   2);  # rcv = resource value
    ( $rcn, $rcv ) = split(':', $tmp, 2);  # rcn = resource name
    $rcn =~ s/[ ]*\?[ ]*/\?/;
    $cmd =~ s/^\#[ ]*//;                   # cmd = GUI directive

#   Continue if a UI command is found
#   ---------------------------------
###    next LINE unless ( $rcv =~ /[ ]*\?/ || $cmd =~ /[ ]*&/ );

#   Is this a question of a GUI label?
#   ---------------------------------
    if ( $rcv =~ /[ ]*\?/ || $cmd =~ /[ ]*&/ ) {

         $rcn = $rcn . ":" if $rcv;       # put ":" back in label

#        keep track of number of widgets
#        -------------------------------
         $m++ unless ( $cmd =~ /^[ ]*&Eval/ || $cmd =~ /^[ ]*&Assert/ ); 

#        Parse widget and args
#        ---------------------
         $rcv =~ s/^[ ]*//;        # strip out leading blancks"
         $rcv =~ s/^\?//;          # strip out "?"
         $rcv =~ s/[ ]*$//;        # strip out trailing blanks

	 if ( $cmd =~ /[ ]*&/ ) {
	   ( $rcw, $rca ) = split(' ', $cmd, 2);  
	 } 
	 else {
	   $cmd =~ s/[ ]*//;
	   $rcw = "&Str";   # default widget name (deprecated)
	   $rca = $cmd; 
	 } 

#        Record user interface request
#        -----------------------------
	 $l++;
	 $UI[$l]{'index'}{$file}  = $i;    # Line in the original file
	 $UI[$l]{'name'}{$file}   = $rcn;  # resource name
	 $UI[$l]{'value'}{$file}  = $rcv;  # actually, the default value
	 $UI[$l]{'widget'}{$file} = $rcw;
	 $UI[$l]{'args'}{$file}   = $rca;
    } 

  }  # end LINE

#   Convenience counters
#   --------------------
    $nError{$file} = 0;
    $nLine{$file} = $i+1;  # number of lines on file
    $nHelp{$file} = $j+1;  # number of help lines
    $nHist{$file} = $k+1;  # number of revision history lines
    if ( $m < 0 ) {
         $nUI{$file} = 0;  # no real questions, ignore GUI labels
    } else {
         $nUI{$file}   = $m+2;  # number of user interface requests
    }
    if ( $l < 0 ) {
         $nWI{$file} = 0;  # no widgets
    } else {
         $nWI{$file}   = $l+1;  # number of widgets
    }

    if ( $debug ) {
	print "\n" ;
	print " NAME: ", $RCname{$file}, "\n";
        print "TITLE: ", $RCtitle{$file},"\n";
        for ( $i=0; $i < $nHelp{$file}; $i++ ) { print " HELP: ",$Help[$i]{$file},"\n"; }
        for ( $i=0; $i < $nHist{$file}; $i++ ) { print " HIST: ",$Hist[$i]{$file},"\n"; }
        for ( $i=0; $i < $nWI{$file};   $i++ ) { 
             print "\n   LABEL: |$UI[$i]{'name'}{$file}| ($UI[$i]{'index'}{$file})\n";
             print "  VALUE: |$UI[$i]{'value'}{$file}|\n";
             print " WIDGET: |$UI[$i]{'widget'}{$file}|\n";
             print "   ARGS: |$UI[$i]{'args'}{$file}|\n"; 
	   }
    } # if debug
close(FILE);

# Write RC -> rc extension
  my ($fname) = $file; 
  $fname =~ s/\.irc$//gi;
  $fname = "$fname\.rc";
  $outfn{$file} = $fname;
  
}

#.........................................................................
#
# Text_UI: Simple text user interface
#
sub Text_UI {  
    my ($file) = @_;
    my $name, $default, $value, $widget, $args;

  AGAIN:

    print "\n\n    "; for (my $i=0; $i<=9+length($RCtitle{$file}); $i++) { printf '-'}; printf "\n"; 
    print     "      Editing $RCtitle{$file}\n";
    print "   "; for (my $i=0; $i<=9+length($RCtitle{$file}); $i++) { printf '-'}; print "\n\n"; 

for ( my $i=0; $i < $nWI{$file}; $i++ ) {

    $name    = $UI[$i]{'name'}{$file};
    $default = $UI[$i]{'value'}{$file};
    $widget  = $UI[$i]{'widget'}{$file};
    $args    = $UI[$i]{'args'}{$file};

    $value = $default;

###    for ( $widget ) {
    $_ = $widget;

SWITCH: {

    /^\&Label/      && do {
                             print "\n$args\n";
                             last SWITCH;
                       };

    /^\&Str/        && do {
                             $value = $default;
                             print "  $args [$default] ";
                             chomp($ans = <STDIN> ); 
                             $ans and $value = $ans;
                             print "";  # cosmetic
                             last SWITCH;
                       };

    /^\&Dir/        && do {
                             $default =~ s/[ ]*//g; 
                             $value = $default;
                             print "  $args [$default] ";
                             chomp($ans = <STDIN> ); 
                             $ans and $value = $ans;
                             print "";  # cosmetic
                              last SWITCH;
                      };

    /^\&File/        && do {
                             $default =~ s/[ ]*//g; 
                             $value = $default;
                             print "  $args [$default] ";
                             chomp($ans = <STDIN> ); 
                             $ans and $value = $ans;
                             print "";  # cosmetic
                             last SWITCH;
                      };

    /\&YesNo/      && do {
                             $default =~ s/[ ]*//g; 
                             $value = $default;
                             print "  $args [$default] ";
                             chomp($ans = <STDIN> ); 
                             $ans and $value = $ans;
                             if ( lc $value eq "y"   ||
                                  lc $value eq "n"   ||
                                  lc $value eq "yes" ||
                                  lc $value eq "no"  ) {
                                  print "";  # cosmetic
                                  last SWITCH;
                             } else {
                               print STDERR "$0: enter either Y or N -------\n";
                               goto SWITCH;
                             }

                       };
		       
    /^\&CheckBox/      && do {
                             $default =~ s/[ ]*//g; 
                             $value = $default;
                             print "  $args [$default] ";
                             chomp($ans = <STDIN> ); 
                             $ans and $value = $ans;
                             if ( lc $value eq "1"   ||
                                  lc $value eq "0"   ||
                                  lc $value eq "off" ||
                                  lc $value eq "on"  ) {
                                  print "";  # cosmetic
                                  last SWITCH;
                             } else {
                               print STDERR "$0: enter either Y or N ++++++++\n";
                               goto SWITCH;
                             }

                       };
		       

    } # SWITCH

### }

    $UI[$i]{'value'}{$file} = $value;  # updated resource file

  } # for each UI request

    print "\n";

#   Update resources
#   ----------------
    Update_RC($file);
    Check_RC($file);

#   Write updated resources to file
#   -------------------------------
    if ($nError{$file}>0) {
      print "--> Found $nError{$file} errors in <$file>\n";
      for ( $i=0; $i < $nError{$file}; $i++ ) {
        my $err = $i+1; 
        print "    $err) " . $Error[$i]{$file} . "\n"; 
      }
      goto AGAIN;
      } else {
        Write_RC($file, $outfn);  
      }

}

#.........................................................................
#
# No_UI:  No user interaction
#

sub No_UI {
  my $file = shift;

#   Update resources
#   ----------------
    Update_RC($file);
    Check_RC($file);

#   Write updated resources to file
#   -------------------------------
    Write_RC($file, $outfn);  
    print "--> Found $nError{$file} errors in <$file>\n" if $nError{$file}; 
    for ( $i=0; $i < $nError{$file}; $i++ ) {
      my $err = $i+1; 
      print "    $err) " . $Error[$i]{$file} . "\n"; 
    }

  }

#.........................................................................
#
# Tk_UI:  Perl TK based user interface
#

sub Tk_UI {  

$main = MainWindow -> new;
my $name, $default, $value, $widget, $args;

Tk_MainGUI($RCfilename[0]);
Tk_CreateWidget($RCfilename[0]);

MainLoop;
exit;

}

sub Tk_CreateWidget { 
    my ($file) = @_;
    print "createwidget: file = $file \n" if ($debug);
    print "\n\n Editing ",$RCtitle{$file},"\n" if ($nWI{$file} > 0  && $debug);

    Tk_TextEntry(-1,"OUTPUT FILE NAME", $outfn{$file}, $file);

for ( my $i=0; $i < $nWI{$file}; $i++ ) {

    $name    = $UI[$i]{'name'  }{$file};
    $default = $UI[$i]{'value' }{$file};
    $widget  = $UI[$i]{'widget'}{$file};
    $args    = $UI[$i]{'args'  }{$file};
    $value = $default;


    for ( $widget ) {

SWITCH: {

    /^\&Label/        && do {
                             Tk_Label($args);
                             print "TK:$args\n" if ($debug);
                             last SWITCH;
                      };

    /^\&Str/          && do {
                             $value = $default;
			     Tk_TextEntry($i,$args, $default, $file);
                             print "TK:$args [$default]\n" if ($debug);
                             #chomp($ans = <STDIN> ); 
                             #$ans and $value = $ans;
                             #print "";  # cosmetic
                             last SWITCH;
                      };

    /^\&Dir/          && do {
                             $default =~ s/[ ]*//g; 
                             $value = $default;
                             print "TK:$args [$default]\n" if ($debug);
                             Tk_Dir($i,$args,$default, $file);
			     last SWITCH;
                      };

    /^\&File/         && do {
                             $default =~ s/[ ]*//g; 
                             $value = $default;
                             print "TK:$args [$default]\n" if ($debug);
			     Tk_File($i,$args,$default, $file);
                             last SWITCH;
                      };

    /^\&YesNo/        && do {
                             $default =~ s/[ ]*//g; 
                             $value = $default;
			     Tk_YesNo($i, $args, $default, $file);
                             last SWITCH;
                      };

    /^\&CheckBox/     && do {
                             $default =~ s/[ ]*//g;
			     Tk_CheckBox($i, $args, $file);
                             last SWITCH;
                      };
    }} # SWITCH
    # updated resource file
  } # for each UI request
  
  Tk_Buttons($file);
###  my $y = ($nWI{$file}) * $elementfontsize * 2.5 + 140;
  my $y = ($nUI{$file}) * $elementfontsize * 2.5 + 160;
  print "No. of UI: $nUI{$file}\n" if ($debug);
  my $x = 555;
  my $geometry = $x . "x" . $y;
  print "geometry = $geometry" if ($debug);
  $main->geometry($geometry);
} 


#.........................................................................
#
# Update_RC: update resources in memory
#

sub Update_RC {  
my ($file) = @_;
my $j, $name, $default, $value, $widget, $args;

    for ( my $i=0; $i < $nWI{$file}; $i++ ) {

    $j       = $UI[$i]{'index' }{$file};
    $name    = $UI[$i]{'name'  }{$file};
    $value   = $UI[$i]{'value' }{$file};
    $widget  = $UI[$i]{'widget'}{$file};
    $args    = $UI[$i]{'args'  }{$file};

#   handle derived parameters
#   -------------------------
$_ = $widget;
SWITCH: {

    /^\&Eval/      && do {
                             my $value_ = eval $args;
			     if ( $value_ ) {
				 $name =~ s/^\#//;
				 $value = $value_;
			     } else {
				 $name = '#' . $name;
                             }
                             last SWITCH;
                       };

    }

#   Make resources environment variables
#   ------------------------------------
    if ( $name =~ /:$/ ) {
      my $name_ = $name; $name_ =~ s/:$//;
      $ENV{$name_} = $value;
      Env::import($name_);
    }

    $Line[$j]{$file} = $name . " " . $value . "   # " . $widget . " " . $args;

### JSY
###    print $Line[$j]{$file}, "\n"; 
### JSY
    }

}

#.........................................................................
#
# Check_RC: check RC for errors
#

sub Check_RC {  
    my ($file) = @_;

    my $value, $widget, $args;
    my $k = -1;

    for ( my $i=0; $i < $nWI{$file}; $i++ ) {

	$value   = $UI[$i]{'value' }{$file};
	$value   = $UI[$i]{'name'  }{$file} unless $value;
	$widget  = $UI[$i]{'widget'}{$file};
	$args    = $UI[$i]{'args'  }{$file};

#       handle Assert directives
#       ------------------------
	$_ = $widget;
SWITCH: {

    /^\&Assert/      && do {
                             my $result = eval "1 if ($args)";
			     unless ( $result ) {
				 $k++;
                                 $Error[$k]{$file} = eval '"' . $value . '"';
                             }
                             last SWITCH;
                       };

       }

#      Make resources environment variables
#      ------------------------------------
       if ( $name =~ /:$/ ) {
	   my $name_ = $name; $name_ =~ s/:$//;
	   $ENV{$name_} = $value;
	   Env::import($name_);
       }

    } # for

    $nError{$file} = $k + 1;

}

#.........................................................................
#
# Write_RC: write out updated resources
#

sub Write_RC {  
  my ($file, $fname) = @_;
  
  $fname = $outfn{$file};
  
# Open
# ----
  if ( $fname eq "-" ) {
       open(FILE,">&STDOUT");
  } else {
       open(FILE,">$fname") or die "cannot open $fname";
       print STDERR "$0: writing RC file $fname\n"  if ( $opt_v );
  }

# Write
# -----
  if ( $opt_e ) { 
      for ( my $i=0; $i < $nLine{$file}; $i++ ) { 
          $evalline = $Line[$i]{$file};
	  eval "\$eLine = \"$evalline\"" ;   # expand environmnet
	  printf FILE "%s\n", "$eLine";  }  
  } else {
    for ( my $i=0; $i < $nLine{$file}; $i++ ) { 
    	printf FILE "%s\n", $Line[$i]{$file};  
    }
  }

# Close
# -----
  close(FILE);
  print "wrote to <$fname>\n";
}

# sub Tk_DirBrowse

sub Tk_DirBrowse {
	my ($i,$text, $path, $file) = @_;
	#my ($direntry);

   if ($path =~ /^\$/gi) { 	
      $evalline = $path;
      eval "\$path = \"$evalline\"" ;   # expand environmnet
   } 

	$ok = 0; # flag: "1" means OK, "-1" means cancelled
	$newtoplevel -> destroy() if (Exists $newtoplevel); 
       	$newtoplevel = MainWindow -> new(-relief=>'ridge',
        	-borderwidth=> 5, -background=>$filebrowseframe); 
	$newtoplevel -> Label ( -background => $filelabelcolor, 
		-text => $text, -font=> "Arial 16 bold",
	)-> pack(-side => 'top', -padx => 6, -pady=>1 );
        $dirtree = $newtoplevel -> Scrolled('DirTree',
                 -background=> $filebrowsecolor,
                 -scrollbars => 'osoe'  , -width => 44, -height => 10,
                 -selectmode => 'browse', -exportselection => 1,
                 -browsecmd  => sub {
                     chomp($path); 
                     #print "dirtree: var $var , value $path\n";
                     #$UI[$i]{'value'}{$file} = $path; 
                     $path = shift;
                     $path =~ s/\/\//\//g;
                     #print "path = $path \n";
	             $direntry{$i}-> delete(0, 'end');
                     $direntry{$i}-> insert(0, "$path");

                  },
                 # With this version of -command a double-click will
                 # select the directory
                 #-command   => sub { $ok = 1 },

                 # With this version of -command a double-click will
                 # open a directory. Selection is only possible with
                 # the Ok button.
                 #-command   => sub { $d->opencmd($_[0]) },
                 -command    => sub { $newtoplevel -> destroy(); },
          )->pack(-side => 'top', -fill => "both", -expand => "both");
       # Set the initial directory
       if (-d $path) { $dirtree -> chdir($path); 
                       $dirtree -> update(); 
       } else { 
            $newtoplevel -> Label(-text => "$path does not exist"); 
            #print "path $path does not exist\n"; 
            $dirtree -> chdir( dirname($path) ) if (Exists $dirtree);
       }
       $newtoplevel -> Button(-text => "Apply", 
              -command => sub { 
		     $path = $direntry{$i} -> get(); 
#      	             $path = shift;
                     $path =~ s/\/\//\//g;
		     chomp($path); 
                     #print "dirtree: var $var , value $path\n";
                     #$UI[$i]{'value'}{$file} = $path; 
                     

                     print "path = $path \n";
                     $UI[$i]{'value'}{$file} = $path; 
                     $newtoplevel -> destroy(); 
	      }
        )-> pack (-side=>'top', -padx=>6, -pady=>1);
       $newtoplevel -> update();
       #$newtoplevel->geometry("310x300+$xpos+$ypos");
       #$newtoplevel->positionfrom("program");
       $newtoplevel->title("RED DIR Browser GUI");

  return 0;
}

sub Tk_Dir{ 
   my ($i,$question,$default, $file) = @_;

   my $dirframe = $rightwidget -> Frame(-background=>$dirframecolor,-height=>35);

   $width = 20 if (! defined $width);
   my $leftframe = $dirframe -> Frame(-background=>$dirframecolor);
   my $rightframe = $dirframe -> Frame(-background=>$dirframecolor);

   $leftframe -> Label (-background => $dirlabelcolor, -text => $question,-font=>$elementfont,
         )-> pack(-side => 'left',-padx => 4, -pady => 1);
   $rightframe -> Button(-text => "Browse", 
       	-command => sub { &Tk_DirBrowse($i,$question,$default,$file); } 
	 )-> pack(-side => 'left',-padx=>3);
   $direntry{$i}  = $rightframe -> Entry(-background=> $direntrycolor,-width=> $textentrywidth )-> pack(-side => 'right');
   $direntry{$i}  -> insert(0,"$default");
   $leftframe -> pack(-side => 'left', -anchor=> 'w', -padx => 2, -pady => 1);
   $rightframe-> pack(-side => 'right',-anchor=> 'e', -padx => 3, -pady => 1);
   $dirframe -> pack(-side => 'top',  -padx => 2, -pady => 1, -fill=>'both');

# Set the initial directory
#   if (! -e $path)  { ("$path does not exist"); }#
#
   $direntry{$i} -> bind('<KeyPress>', sub { 
	print "direntry: $checkbox " if ($debug); 
	$path = $direntry{$i} -> get(); 
	chomp($path);  print "dirtree: var $var , value $path\n" if ($debug);
	$UI[$i]{'value'}{$file} = $path; 
       	}
   );
   return 0;
}
sub Tk_Dir2{ 
   my ($i,$question,$default, $file) = @_;
   my $dirframe = $rightwidget -> Frame(-background=>$dirframecolor, -height=>30);
   my ($direntry);
   $width = 20 if (! defined $width);
   my $leftframe = $dirframe -> Frame(-background=>$dirframecolor);
   my $rightframe = $dirframe -> Frame(-background=>$dirframecolor);
   $leftframe -> Label (-background => $dirlabelcolor, -font=>$elementfont,
                        -text => $question
         )-> pack(-side => 'left',-padx => 4, -pady => 0);
	 
   
   $rightframe -> Button(-text => "Browse",
       	-command => sub { 
	       $path = $rightframe -> getOpenFile(
 			-defaultextension => "",
		     # -filetypes        =>
		     # [ ['All Files',        '*',             ],
		     # 	['Perl Scripts',     '.pl'            ],
  	       	#	['Text Files',       ['.txt', '.text']],
		#       	['C Source Files',   '.c',      'TEXT'],
		#       	['GIF Files',        '.gif',          ],
		#       	['GIF Files',        '',        'GIFF'],
		#      ],
		      -initialdir       => $default,
		     # -initialfile      => "$default",
		      -title            => "$question",
		      );
		chomp($path);
		$UI[$i]{'value'}{$file} = $path;
		$direntry-> delete(0, 'end');
                $direntry-> insert(0, "$path");
	} 
	 )-> pack(-side => 'left',-padx=>3);
   $direntry  = $rightframe -> Entry(
   				-background=> $fileentrycolor,
   				-width=> $textentrywidth 
			)-> pack(-side => 'right');
   $direntry  -> insert(0,"$default");
   $leftframe -> pack(-side => 'left', -anchor=> 'w', -padx => 2, -pady => 0);
   $rightframe-> pack(-side => 'right',-anchor=> 'e', -padx => 3, -pady => 0);
   $dirframe -> pack(-side => 'top',  -padx => 2, -pady => 0, -fill=>'both');

   # Set the initial directory
#   if (! -e $path)  { ("$path does not exist"); }#

   $direntry -> bind('<KeyPress>', sub { 
	print "direntry: $checkbox " if ($debug); 
	$path = $direntry -> get(); 
	chomp($path);  print "dirtree: var $var , value $path\n" if ($debug);
	$UI[$i]{'value'}{$file} = $path; 
       	}
   );
   return 0;
}

sub Tk_File{ 
   my ($i,$question,$default, $file) = @_;
   my $fileframe = $rightwidget -> Frame(-background=>$fileframecolor, -height=>30);
   my ($direntry);
   $width = 20 if (! defined $width);
   my $leftframe = $fileframe -> Frame(-background=>$fileframecolor);
   my $rightframe = $fileframe -> Frame(-background=>$fileframecolor);
   $leftframe -> Label (-background => $filelabelcolor, -font=>$elementfont,
                        -text => $question
         )-> pack(-side => 'left',-padx => 4, -pady => 0);

   $rightframe -> Button(-text => "Browse",
       	-command => sub { 
	       $path = $rightframe -> getOpenFile(
 			-defaultextension => "",
		      -filetypes        =>
		      [ ['All Files',        '*',             ],
		      	['Perl Scripts',     '.pl'            ],
  	       		['Text Files',       ['.txt', '.text']],
		       	['C Source Files',   '.c',      'TEXT'],
		       	['GIF Files',        '.gif',          ],
		       	['GIF Files',        '',        'GIFF'],
		      ],
		      -initialdir       => dirname($default),
		      -initialfile      => "$default",
		      -title            => "$question",
		      );
		      
		chomp($path);
		$UI[$i]{'value'}{$file} = $path;
		$direntry-> delete(0, 'end');
                $direntry-> insert(0, "$path");
	} 
	 )-> pack(-side => 'left',-padx=>3);
   $direntry  = $rightframe -> Entry(
   				-background=> $fileentrycolor,
   				-width=> $textentrywidth 
			)-> pack(-side => 'right');
   $direntry  -> insert(0,"$default");
   $leftframe -> pack(-side => 'left', -anchor=> 'w', -padx => 2, -pady => 0);
   $rightframe-> pack(-side => 'right',-anchor=> 'e', -padx => 3, -pady => 0);
   $fileframe -> pack(-side => 'top',  -padx => 2, -pady => 0, -fill=>'both');

   # Set the initial directory
#   if (! -e $path)  { ("$path does not exist"); }#

   $direntry -> bind('<KeyPress>', sub { 
	print "direntry: $checkbox " if ($debug); 
	$path = $direntry -> get(); 
	chomp($path);  
	print "dirtree: var $var , value $path\n" if ($debug);
	$UI[$i]{'value'}{$file} = $path; 
       	}
   );
   return 0;
}

sub Tk_Label { 
   my ($LABEL) = @_; 
   print "doLABEL : $LABEL \n"  if ($debug);
   my ($labelframe) = $rightwidget -> Frame(-height=>30,
   				);
   $labelframe ->  Label ( 
      -background => $titlecolor,
      -font=>"Helvetica 14 italic",
      -text => $LABEL, 
   )-> pack(-side => 'top', -fill=>'both',-expand=>1,-pady => 0, -fill=>'both',-expand=>1);
   $labelframe -> pack(-side=>'top',-padx=>3, -pady=>0,-fill=>'both',-expand=>1);
   return 0;
}

sub Tk_TextEntry {
   my ($i,$question,$default, $file) = @_;
   my ($inputentry, $textentry, $temp,$pad); 
   if ($i> -1) { $pad = 0;} else { $pad=3;}
   print "doTEXTENTRY   i, question, default = ($i,$question,$default)\n" if ($debug);   

   $textentry = $rightwidget -> Frame(-background=>$textframecolor);
   $textentry -> Label(	-background => $textqcolor,
   			-text => $question, -font=>$elementfont,
		)-> pack(-side => 'left',-anchor=> 'w', -pady=>0,-padx=>1);

   $inputentry = $textentry -> Entry( -width=> $textentrywidth, 
                                      -background=> $textentrycolor,
                   ) -> pack(-side => 'right');
   $inputentry    -> insert(0, "$default");
   $textentry	-> pack(-side => 'top', -anchor=>'e',
   			-padx => 3, -pady => $pad,-fill=>'x');

   $inputentry -> bind('<KeyPress>', 
          sub { 
          	$temp = $inputentry -> get();
          	chomp($temp); 
		if ($i> -1) { 
	  	$UI[$i]{'value'}{$file} = $temp;
          	print "i = $i, \$temp= ",$temp, "\n" if ($debug);
		} else { 
			$outfn{$file} = $temp;
		} 
	  }
   );
   
   return 0;  
}

sub Tk_YesNo { 
   my ($i,$question,$default, $file) = @_;
   my ($var,$radioframe,$radio,$rttl);
   print "Tk_YesNo i, question,default = ($i,$question,$default)\n" if ($debug);   
   chomp($default); $var = $default;

   $radioframe = $rightwidget -> Frame(	-background=>$radioframecolor,
   					-height=>20,
				);
   $rttl = $radioframe -> Frame;
   $rttl -> Label(-background => $radioqcolor,
   		-text=>$question, 
   		-font=>$elementfont )-> pack(-side => 'left',-anchor=>'w');
   $rttl -> pack(-side => 'left', 
   		-anchor=>'w', 
		-padx => 1, 
		-pady => 0,
#		-expand=>1
		);	
   $radio = $radioframe -> Frame; 
   $radio -> Radiobutton(-text       => "Yes", -width=>3,
		-background => $radiobuttoncolor, -value      => "yes", 
                -variable   => \$var, -relief=>'raised',
  	        -command => sub { 
	           $UI[$i]{'value'}{$file} = $var;
                   print "i = $i, name = ", $UI[$i]{'name'}{$file}, ", \$var = ",$var, "\n" if ($debug);
		   return;
		},
		)-> pack(-side => 'right');
   $radio -> Radiobutton(-text       => "No", -width=>3,
     		-background => $radiobuttoncolor, -value      => "no", 
                -variable   => \$var,-relief=>'raised',
 	        -command => sub { 
		    $UI[$i]{'value'}{$file}= $var;
  	            print "i = $i,name = ", $UI[$i]{'name'}{$file}, ", \$var = ",$var, "\n" if ($debug);
		    return;
		},
		)-> pack(-side => 'right');
   $radio -> pack(-side => 'right', -padx => 1, -pady => 0);
   $radioframe -> pack(-side => 'top', 
   			-padx => 3, 
			-pady => 0, 
			-fill=>'x', 
   			#-expand=>'x'
			);
  return 0;
}


sub Tk_CheckBox { 

   my ($i,$default, $file) = @_;
   my ($var,$checkframe,$check,$rttl,$blank);
   print "Tk_CheckBox i, default, var = ($i,$args,$var)\n" if ($debug);   

   chomp($default); 
   if ($default =~ /^on/i) { $var = 1;}
   else { $var = 0;}

### ($labeltext = $UI[$i]{'name'}{$file}) =~ s/\#//gi;;
   $labeltext = $default;
   $labeltext =~ s/^on//gi;
   $labeltext =~ s/^off//gi;
   $labeltext =~ s/^[ ]*//;

   Tk_markCB($i, $var, $file);
   $UI[$i]{'name'}{$file} =~ s/\://gi;
   $UI[$i]{'name'}{$file} =~ s/\?//;
 
   $checkframe = $rightwidget -> Frame(-background=>$checkframecolor,
   				-height=>30, );

   $check = $checkframe -> Frame(-height=>30, -background=>$checkframecolor); 
   $check -> Checkbutton( -width=>1,
		-background => $checkbuttoncolor, 
		-variable   => \$var, -wraplength => 2, -relief=>'raised',
  		-command => sub { 
			Tk_markCB($i, $var, $file);  return;
		},)-> pack();
   $check -> pack(-side => 'left', -padx => 10, -pady => 0);


   $rttl = $checkframe -> Frame(-height=>30,-background=>$checkframecolor,);
   $rttl -> Label(-background => $checkbackground, 
   		-text=>$labeltext, -font=> $elementfont )
         -> pack();
   $rttl -> pack(-side => 'left', -anchor=>'w', -padx => 1, -pady => 0 );	
   
   $checkframe -> pack(	-side => 'top',
   			-anchor=>'w',
			-padx => 3, 
			-pady => 0, 
#			-expand=>'x',
			-fill=>'x'
		);
#   $checkframe -> update;
}

sub Tk_markCB { 
   my ($i, $var, $file) = @_;
   $UI[$i]{'name'}{$file} =~ s/^\ //gi;
   if ($var == 1) {  
   	$UI[$i]{'name'}{$file} =~ s/\#//gi; 
	$UI[$i]{'args'}{$file} = "on $labeltext";
   } else { 
   	$UI[$i]{'name'}{$file} = "#" . $UI[$i]{'name'}{$file} if ($UI[$i]{'name'}{$file} !~ /\#/); 
	$UI[$i]{'args'}{$file} = "off $labeltext";
   }
 
   return ;
} 

sub Tk_HelpAboutRed { 
   $newtoplevel -> destroy() if (Exists $newtoplevel); 
	
   $newtoplevel = MainWindow -> new(-relief=>'ridge',
        				-borderwidth=> 5, 
					-background=>$filebrowseframe); 
   $newtoplevel -> Label ( -background => $filelabelcolor, 
				-text => "About RED", 
				-font=> "Arial 20 bold",
		)-> pack(-fill=>'x',
			-side => 'top', 
			-padx => 6, 
			-pady=>1 
			);
    my (@ARR) = ("About RED", "Resource File Editor",
    		"Global Modeling Assimilation Office", "Created 3/30/2005 "," ",
		
		"[SAVE] - saves current configurations to *.rc file",
		"[NEXT] - goes to next RC file if available",
		"[BACK] - goes back to the previous file if available",
		"[FINISH] - saves all info in memory to *.rc files",
		"This tool holds all config info in memory hash-table", 
		"and saves to rc file at the end or when [save] is pressed",
		);
   foreach my $a (@ARR) { 
     $newtoplevel -> Label(-text=> $a, -background=>$filebrowseframe
      		)->pack(-expand=>1,
			-anchor=>'w',
      			-side=>'top');
   } 
   $newtoplevel -> Button(-text => "Close", 
              -command => sub { $newtoplevel -> destroy(); }
        	)-> pack (-side=>'top', -padx=>6, -pady=>1);

   $newtoplevel->title("RED Help - About RED");
}

sub Tk_HelpAbout { 
    my ($file) = @_;
   $newtoplevel -> destroy() if (Exists $newtoplevel); 
   $newtoplevel = MainWindow -> new(-relief=>'ridge',
        				-borderwidth=> 5, 
					-background=>$filebrowseframe); 
   $newtoplevel -> Label ( -background => $filelabelcolor, 
				-text => "About ". $RCtitle{$file}, 
				-font=> "Arial 20 bold",
		)-> pack(-fill=>'x',
			-side => 'top', 
			-padx => 6, 
			-pady=>1 
			);
	for ( $i=0; $i < $nHelp{$file}; $i++ ) { 
     	 $newtoplevel -> Label(	-text => $Help[$i]{$file}, 
				-background => $filebrowseframe
      		)->pack(	-expand=>1,
				-anchor=>'w',
      				-side=>'top'
			);
   	} 
   $newtoplevel -> Button(	-text => "Close", 
              			-command => sub { 
				$newtoplevel -> destroy(); 
				}
        	)-> pack (-side=>'top', -padx=>6, -pady=>1);

   $newtoplevel->title("RED Help - About " . $RCtitle{$file});
   
} 

sub Tk_HelpRevision { 
   my ($file) = @_;
   $newtoplevel -> destroy() if (Exists $newtoplevel); 
	
   $newtoplevel = MainWindow -> new(	-relief=>'ridge',
        				-borderwidth=> 5, 
					-background=>$filebrowseframe
				); 
   $newtoplevel -> Label ( 	-background => $filelabelcolor, 
				-text => "Revision History for " . $RCtitle{$file}, 
				-font=> "Arial 20 bold",
		)-> pack(	-expand=>1,
				-side => 'top', 
				-padx => 6, 
				-pady=>1 
			);
   for ( $i=0; $i < $nHist{$file}; $i++ ) { 
#   foreach my $a (@Hist{$file}) { 
      $newtoplevel -> Label(	-text=> $Hist[$i]{$file}, 
      			 	-background=>$filebrowseframe
      		)->pack(
			-anchor=>'w',
      			-side=>'top');
   } 
   $newtoplevel -> Button(-text => "Close", 
              	-command => sub { $newtoplevel -> destroy(); }
        	)-> pack (-side=>'top', -padx=>6, -pady=>1);
   $newtoplevel->title("RED Help - Revision History");
} 

sub Tk_FileMenu { 
  my ($file) = @_;  
  if (! $file) { $file = $RCfilename[0]};
  $filemenuwidget = $filemenuframe -> Frame(-height=>40);
  $filemenu = $filemenuwidget -> Menubutton('-text' => 'File')-> pack(-side=> 'left');
  $filemenu -> command('-label' => 'Save As', 
  			-command => sub { 
	       $outfn = $filemenu -> getSaveFile(
		      -filetypes        =>
		      [ ['Resource Files','*.rc',],
		      	['All Files', '.*' ],
		      ],
		      );
 		});
  $filemenu -> command('-label' => 'Reload', '-command' => sub {Tk_Reload($file);});
  $filemenu -> separator();
  $filemenu -> command(-label => 'Exit', -command=> sub { exit; } );
  
  $editmenu = $filemenuwidget -> Menubutton(-text => 'Edit')-> pack(-side=> 'left');
  $editmenu -> command(-label => 'vi', 
  		-command=> sub {system("vi $file");     Tk_Reload($file);});	 
  $editmenu -> command(-label => 'nedit', 
  		-command=> sub {system("nedit $file");  Tk_Reload($file);});	
  $editmenu -> command(-label => 'emacs', 
  		-command=> sub {system("emacs $file");  Tk_Reload($file);});   
  $editmenu -> command(-label => 'xemacs', 
  		-command=> sub {system("xemacs $file"); Tk_Reload($file);});	
		
  $helpmenu = $filemenuwidget -> Menubutton(-text => 'Help')-> pack(-side=> 'left');
  $helpmenu -> command(-label => 'About RED', 
  		-command=> sub { Tk_HelpAboutRed($file);} );

  $helpmenu -> command(-label => "About $file", 
  		-command=> sub { Tk_HelpAbout($file);} );

  $helpmenu -> command(-label => 'Revision History', 
  		-command=> sub { Tk_HelpRevision($file);} );	

  $filemenuwidget ->pack();

} 

sub Tk_Reload { 
   my ($file) = @_;
     print "Reload for $file" if ($debug);
     $buttonwidget	-> destroy() if ( Exists $buttonwidget	);
     $filemenuwidget 	-> destroy() if ( Exists $filemenuwidget);
     $rightwidget 	-> destroy() if ( Exists $rightwidget	);
     $leftwidget	-> destroy() if ( Exists $leftwidget	);
     Parse_RCfile($file);
     Tk_FileMenu($file);
     Tk_RightWidget($file);
     Tk_LeftWidget($file);
     Tk_CreateWidget($file);
     return 0;
}

sub Tk_LeftWidget { 
  my ($currentfile) = @_;
  if (! $currentfile) { $currentfile = $RCfilename[0]};
  
  $leftwidget = $leftframe -> Frame(-background => '#FFAAAA');
  
  $redmapl = $leftwidget -> Photo(-format=>"gif", -data => $logo);
#  $redmapl = $leftwidget -> Photo(-format=>"gif", -file=>"red_nasalogo.gif");
  $label = $leftwidget -> Label(-background => '#FFDDDD', 
  			-text => 'NASA LOGO!',
      			-image => $redmapl, 
		 )->pack(-side => 'top',-fill=>'both');
  #} 
  my %label;
  
  foreach my $file (@RCfilename) { 
    
    if ($currentfile eq $file) { $labelcolor = "#EEEE00"; } 
    else { $labelcolor = "#EEFFEE"; } 
      
    $label{$file} = $leftwidget -> Label (  -text => $RCname{$file}, -background=> $labelcolor,
		 )->pack(-side=> 'top' , -anchor=>'w', -padx=> 5 , -pady=> 2);

    $label{$file} -> bind ('<Button>', sub { 
			     $buttonwidget	-> destroy() if ( Exists $buttonwidget	);
			     $filemenuwidget 	-> destroy() if ( Exists $filemenuwidget);
			     $rightwidget 	-> destroy() if ( Exists $rightwidget	);
			     $leftwidget	-> destroy() if ( Exists $leftwidget	);
			     #Parse_RCfile($file);
			     Tk_FileMenu($file);
			     Tk_RightWidget($file);
			     Tk_LeftWidget($file);
			     Tk_CreateWidget($file);
			     
    			}
    );

  } 
  $leftwidget 	-> pack (	-side=>'left', -fill => 'y', 
  				-anchor=>'w', -padx => 1, -pady => 1); 
}

sub Tk_RightWidget { 
  my ($file) = @_;
  if (! $file) { $file = $RCfilename[0]};
  
  $rightwidget = $rightframe 	-> Frame(#-relief => 'sunken', 
  		-borderwidth=>0,
		-background => "#FFDDDD",
		);

  $rightwidget -> Label(-text=> $RCtitle{$file}, -background=>"#FFE0E0",
  			-font=>"Arial 29",
 		)->pack(-side=>'top',
			-fill=>'x',-pady=>12,
			#-expand=>1
			);

  $rightwidget  		-> pack(-anchor => 'nw',
  					-side   => 'top',
  					-fill   => 'both',  
#					-expand => 1,
  					-padx 	=> 1, 
 					-pady 	=> 0 	);

#  $leftframe -> update;  

}

sub Tk_MainGUI {
  my ($file) = @_;

  print "create_mainGUI\n" if ($debug);

  $main -> title("RED - Resource File EDitor");
  $filemenuframe = $main -> Frame(-height=>35);  

  Tk_FileMenu($file);

  $filemenuframe -> pack(-side=>'top',-anchor=>'w');
  $submain   = $main 	-> Frame(-relief => 'ridge', 
  		-borderwidth=>2, -background => '#FFAAAA', );
  $leftframe = $submain -> Frame(-relief => 'ridge',
  		-background => '#FFDDDD',-width=>100);		
		
  Tk_LeftWidget($file);
  
  $leftframe 	-> pack (	-side=>'left', -fill => 'y', 
  				-anchor=>'w', -padx => 1, -pady => 1); 
  $rightframe = $submain -> Frame(-relief => 'ridge', -width=>800,
  		-borderwidth=>1, 
		-background => '#FFAAAA',);
  
  Tk_RightWidget($file);  

  $rightframe 	-> pack(-anchor => 'nw',
  			-side   => 'top',
  			-fill   => 'both',  
			-expand => 1,
			-padx 	=> 1, 
			-pady 	=> 0 	
			);
					
  $submain 	-> pack(-anchor => 'nw',
  			-side   => 'top',
  			-fill 	=> 'both',  
			-expand => 1,	
  			-padx 	=> 1, 
			-pady 	=> 1,
						
			);

 $buttonframe =$main-> Frame(
 			-background=> $buttonframecolor, 
			-relief => 'groove',
			-height => 40
		);
 $buttonframe -> pack(	
 			-side =>'bottom',
 			-anchor=>'s',
 			-padx => 2, 
			-pady => 3,
			-fill =>'both',
			-expand =>1
		);
 

}

sub Tk_Buttons { 
  my ($file) = @_;
  my ($backfile,$nextfile, $text, $b,$c);


# Calculate location of current $file
# ----------------------------------
foreach my $a (@RCfilename) { 
	$b++; last if ($a eq $file); 
} 


# Next or Finish? 
# ---------------

if ( scalar(@RCfilename) == 1 ) { 
      $text = "Finish";
} elsif ( $file eq $RCfilename[$#RCfilename] ) { 
      $text = "Finish";
} else { 
      $nextfile = $RCfilename[$b];
      $text = "Next";
}


#     Figure out previous file for BACK
#     --------------------------------

$c = $b - 2; 
if ($c < 0) { 
     $backfile = "";
} else { 
     $backfile = $RCfilename[$c];
}
 
$buttonwidget = $buttonframe-> Frame(-height=>35);
$buttonwidget -> Button (-text => "Cancel", -background=> $buttoncolor,
			-command => sub {   exit;	}
		)-> pack(-side => 'left', -padx => 5, -pady => 2, -fill => 'both');
					
$buttonwidget -> Button ( -text => "Save", -background=> $buttoncolor,
			-command => sub { 
			   Update_RC($file);
                           Check_RC($file); # check for errors

                           # Write updated resources to file
			   # -------------------------------
                           unless ( $nError{$file} ) {
			       Write_RC($file,$outfn);  
			       print "wrote to $outfn\n" if ($debug);
                           } else {
                               Tk_ErrorDialog($file);
			       print "cannot write to $outfn due to errors\n" 
                                      if ($debug);
                           }
			}

		)-> pack(-side => 'left', -padx => 5, -pady => 2,-fill   => 'both');

if ($backfile) { 
     $buttonwidget -> Button ( -text => "Back", -background=> $buttoncolor,
                      -command => sub { 
		           print "file = $file\n " if ($debug);
	        	   Update_RC($file);
 		 	   print "updated --------------- \n" if ($debug);
                           Check_RC($file); # check for errors
                           unless ( $nError{$file} ) {
			     $buttonwidget	-> destroy() if ( Exists $buttonwidget	);
			     $filemenuwidget 	-> destroy() if ( Exists $filemenuwidget);
			     $rightwidget 	-> destroy() if ( Exists $rightwidget	);
			     $leftwidget	-> destroy() if ( Exists $leftwidget	);
			     #Parse_RCfile($file);
			     Tk_FileMenu($backfile);
			     Tk_RightWidget($backfile);
			     Tk_LeftWidget($backfile);
			     Tk_CreateWidget($backfile);
                           } else {
                             Tk_ErrorDialog($file);
                           }
		      }
     ) -> pack(-side => 'left', -padx => 5, -pady => 2 ,-fill   => 'both',   );
} 


$buttonwidget -> Button ( -text => $text, -background=> $buttoncolor,

                      -command => sub { 
		           print "file = $file\n " if ($debug);
			   #chomp($currentfile);
		           
	        	   Update_RC($file);
 		 	   print "updated --------------- \n" if ($debug);

		      	   if ($text eq "Finish" ) { 
			      my $nErr = 0;
			      foreach my $rcfile (keys %nWI) { 
				print "rcfile = $rcfile --------------- \n" if ($debug);
				print " --------------- \n" if ($debug);
				
				# Write updated resources to file
			        # -------------------------------
                                Check_RC($rcfile);
				unless ( $nError{$rcfile} ) {  
				    Write_RC($rcfile, $outfn);
                                } else {
                                    $nErr =+ $nError{$rcfile};
                                    Tk_ErrorDialog($rcfile);
                                }
              
				print " --------------- \n" if ($debug);			        
			      } 
			      exit unless ( $nErr );

		           } else { # [NEXT]

                             Check_RC($file);
                             unless ( $nError{$file} ) {  
                               $buttonwidget	-> destroy() if ( Exists $buttonwidget	);
                               $filemenuwidget 	-> destroy() if ( Exists $filemenuwidget);
                               $rightwidget 	-> destroy() if ( Exists $rightwidget	);
                               $leftwidget	-> destroy() if ( Exists $leftwidget	);
                               #Parse_RCfile($file);
                               Tk_FileMenu($nextfile);
                               Tk_RightWidget($nextfile);
                               Tk_LeftWidget($nextfile);
                               Tk_CreateWidget($nextfile);
                             } else {
                               Tk_ErrorDialog($file);
                             }
		      }
       }
       ) -> pack(-side => 'left', -padx => 5, -pady => 2 ,-fill   => 'both',   );
$buttonwidget -> pack(-side=>'bottom',-anchor=>'s');
#$buttonwidget ->update();
}

#.........................................................................

sub Tk_ErrorDialog { 
   my ($file) = @_;

   return unless $nError{$file}; 

###  $newtoplevel -> destroy() if (Exists $newtoplevel); 
 ### $newtoplevel -> destroy() if ($newtoplevel); 
	
   $newtoplevel = MainWindow -> new(	-relief=>'ridge',
        				-borderwidth=> 5, 
					-background=>$filebrowseframe
				); 

   $newtoplevel -> Label ( 	-background => $filelabelcolor, 
				-text => "Errors found in " . $RCname{$file}, 
				-font=> "Arial 20 bold",
		)-> pack(	-expand=>1,
				-side => 'top', 
				-padx => 6, 
				-pady=>1 
			);
   for ( $i=0; $i < $nError{$file}; $i++ ) {
      my $err = $i+1; 
      $newtoplevel -> Label(	-text=> "   $err) " . $Error[$i]{$file}, 
      			 	-background=>$filebrowseframe
      		)->pack(
			-anchor=>'w',
      			-side=>'top');
   } 

   $newtoplevel -> Label ( 	-background => $filelabelcolor, 
				-text => 'Cannot save "' . $outfn{$file} . 
                                  '" unless you correct these errors.', 
				-font=> "Arial 14 bold",
		)-> pack(	-expand=>1,
				-side => 'top', 
				-padx => 6, 
				-pady=>1 
			);

   $newtoplevel -> Button(-text => "Close", 
              	-command => sub { $newtoplevel -> destroy(); }
        	)-> pack (-side=>'top', -padx=>6, -pady=>1);
   $newtoplevel->title("RED Error");
} 

#...................................................................

sub trimit {
    my $str = shift;
    $str =~ s/,.*$//g;
    $str =~ s/^\s*//g;
    $str =~ s/\s*$//g;
    $str =~ s/^\'//;
    $str =~ s/\'$//;
    return $str;
}

#.........................................................................

sub usage {

   print <<"EOF";

NAME
     red_ma.pl - a simple resource file editor
          
SYNOPSIS

     red_ma.pl [OPTIONS]  resource_filename(s)
          
DESCRIPTION

     Reads one or more ESMF Config style resource files, generating a
     text or graphical user interface for those resources marked with
     specific directives. These GUI directives can be used to
     costumize the interface.

OPTIONS
     -d             do not interact with user, just take defaults
     -D             debug mode
     -e             expand environment variables on output
     -o filename    Specifies name of output file name, default
                    is stdout
     -t             text user interface (default is GUI)
     -v             verbose mode

AUTHORS
     Arlindo da Silva, NASA/GSFC.
     Joon Yoon, SAIC

EOF

  exit(1)

 }
 
sub redmapl_mime { 

#
# See http://www.dailycoding.com/Utils/Converter/ImageToBase64.aspx
# To convert a GIF image to Base 64.
#

$logo = qq~
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
~;

}
1;
