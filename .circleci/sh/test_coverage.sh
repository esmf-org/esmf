#!/usr/bin/env bash
############################### Extract test_esmf ########################################

###################################################################################
# This script finds all the fortran public subroutines and finds if any unit tests
# and system calls the routine. It prints out the resuts and creates the ESMF_Functions 
#file. It also updates the metrics files on the web.
####################################################################################
echo "Get the fortran and C routines"
echo "" > ESMF_Functions
echo "" >> ESMF_Functions
#Remove MAPL directory until we start running MAPL tests.
rm -rf src/addon/MAPL*/
find src -name "ESMC*h" -type f -exec grep  -A4 "\/\/BOP" {} > c_functions \;
find src -name "*90" -type f -exec grep  -A4 "\!BOP" {} > fortran_functions \;
find src -name "NUOPC*90" -type f -exec grep  -A4 "\!BOP" {} >> fortran_functions \;

# Handle C
grep  -v "\/\/BOPI" c_functions > temp
grep -A4 "\/\/BOP" temp > functions
grep "ROUTINE"  functions > new_functions
echo "Finished getting the C routines."
awk '{print $3}' new_functions | sort -u > c_routines
C_Count=`cat c_routines | wc -l`
test_c_funcs=`sort  -u c_routines`
mv c_routines c_funcs
sed G c_funcs > new_routines
mv new_routines c_routines

# Handle fortran
grep  -v "\!BOPI" fortran_functions > temp
grep -A4 "\!BOP" temp > functions
grep "ROUTINE"  functions > new_functions
echo "Finished getting the fortran routines."
awk '{print $3}' new_functions | sort -u > fortran_routines



# Remove Demo code from list
sed '/ESMF_Test/d' fortran_routines | sed '/Flow_Final/d' | sed '/FlowInit/d' | sed '/Flow_Init/d' | sed '/FlowPrint/d' | sed '/FlowRho/d' | sed '/FlowRhoI/d' | sed '/FlowRhoVel/d'  | sed '/FlowSolve/d'  | sed '/FlowSolver_register/d'  | sed '/FlowState/d' | sed '/FlowVel/d' > temp1
mv temp1 fortran_routines
fortran_funcs=`sort -u fortran_routines`
fortranCount=`cat fortran_routines | wc -l`
sed G fortran_routines > new_routines 
mv new_routines fortran_routines
# Make the common file header.
echo "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'><html><head> " > file_header
echo "<meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1'> " > file_header
echo "<meta name='Author' content='Himanshu Pillai'>" >> file_header

# Compose the ESMF_Methods.html file
cat file_header > $LOGDIR/ESMF_Methods.html
echo "<meta name='GENERATOR' content='Mozilla/4.79 [en] (X11; U; Linux 2.4.18-3smp i686) [Netscape]'><title>ESMF Methods</title></head>" >> $LOGDIR/ESMF_Methods.html
echo "<body text='#000000' bgcolor='#ffffff' link='#993300' vlink='#666600' alink='#666600'>" >> $LOGDIR/ESMF_Methods.html
echo "<font face='arial, helvetica, sans serif'><font color='#006699'><font size='+2'><b> <br>" >> $LOGDIR/ESMF_Methods.html
echo "Public ESMF Methods </b> </font></font></font>" >> $LOGDIR/ESMF_Methods.html
echo "<pre style='margin-left: 40px;'><font face='arial,' helvetica='' sans='' serif='' style='font-family: sans-serif;'><font size='-1'><br></font></font><font face='arial,' helvetica='' sans='' serif='' size='-1' style='font-family: sans-serif;'>" >> $LOGDIR/ESMF_Methods.html
echo "`date`" >> $LOGDIR/ESMF_Methods.html
echo "<p>" >> $LOGDIR/ESMF_Methods.html
echo "The following is the list of Fortran public subroutines and functions that have been implemented:" >> $LOGDIR/ESMF_Methods.html
echo "" >> $LOGDIR/ESMF_Methods.html
echo "" >> $LOGDIR/ESMF_Methods.html
cat fortran_routines >> $LOGDIR/ESMF_Methods.html
#rm -f fortran_routines

# Compose the ESMC_Methods.html file
cat file_header > $LOGDIR/ESMC_Methods.html
echo "<meta name='GENERATOR' content='Mozilla/4.79 [en] (X11; U; Linux 2.4.18-3smp i686) [Netscape]'><title>ESMC Methods</title></head>" >> $LOGDIR/ESMC_Methods.html
echo "<body text='#000000' bgcolor='#ffffff' link='#993300' vlink='#666600' alink='#666600'>" >> $LOGDIR/ESMC_Methods.html
echo "<font face='arial, helvetica, sans serif'><font color='#006699'><font size='+2'><b> <br>" >> $LOGDIR/ESMC_Methods.html
echo "Public ESMC Methods </b> </font></font></font>" >> $LOGDIR/ESMC_Methods.html
echo "<pre style='margin-left: 40px;'><font face='arial,' helvetica='' sans='' serif='' style='font-family: sans-serif;'><font size='-1'><br></font></font><font face='arial,' helvetica='' sans='' serif='' size='-1' style='font-family: sans-serif;'>" >> $LOGDIR/ESMC_Methods.html
echo "`date`" >> $LOGDIR/ESMC_Methods.html
echo "<p>" >> $LOGDIR/ESMC_Methods.html
echo "The following is the list of C public subroutines and functions that have been implemented:" >> $LOGDIR/ESMC_Methods.html
echo "" >> $LOGDIR/ESMC_Methods.html
echo "" >> $LOGDIR/ESMC_Methods.html
cat c_routines >> $LOGDIR/ESMC_Methods.html
#rm -f c_routines


#Start the ESMF_Methods_Not_Tested.html file
cat file_header > $LOGDIR/ESMF_Methods_Not_Tested.html
echo "<meta name='GENERATOR' content='Mozilla/4.79 [en] (X11; U; Linux 2.4.18-3smp i686) [Netscape]'><title>ESMF Methods Not Tested</title></head>" >> $LOGDIR/ESMF_Methods_Not_Tested.html
echo "<body text='#000000' bgcolor='#ffffff' link='#993300' vlink='#666600' alink='#666600'>" >> $LOGDIR/ESMF_Methods_Not_Tested.html
echo "<font face='arial, helvetica, sans serif'><font color='#006699'><font size='+2'><b> <br>" >> $LOGDIR/ESMF_Methods_Not_Tested.html
echo "Public ESMF Methods Not Tested </b> </font></font></font>" >> $LOGDIR/ESMF_Methods_Not_Tested.html
echo "<pre style='margin-left: 40px;'><font face='arial,' helvetica='' sans='' serif='' style='font-family: sans-serif;'><font size='-1'><br></font></font><font face='arial,' helvetica='' sans='' serif='' size='-1' style='font-family: sans-serif;'>" >> $LOGDIR/ESMF_Methods_Not_Tested.html
echo "`date`" >> $LOGDIR/ESMF_Methods_Not_Tested.html
echo "<p>" >> $LOGDIR/ESMF_Methods_Not_Tested.html
echo "The following is the list of ESMF Fortran public subroutines and functions that have not been tested:" >> $LOGDIR/ESMF_Methods_Not_Tested.html
echo "" >> $LOGDIR/ESMF_Methods_Not_Tested.html
echo "" >> $LOGDIR/ESMF_Methods_Not_Tested.html

#Start the ESMC_Methods_Not_Tested.html file
cat file_header > $LOGDIR/ESMC_Methods_Not_Tested.html
echo "<meta name='GENERATOR' content='Mozilla/4.79 [en] (X11; U; Linux 2.4.18-3smp i686) [Netscape]'><title>ESMC Methods Not Tested</title></head>" >> $LOGDIR/ESMC_Methods_Not_Tested.html
echo "<body text='#000000' bgcolor='#ffffff' link='#993300' vlink='#666600' alink='#666600'>" >> $LOGDIR/ESMC_Methods_Not_Tested.html
echo "<font face='arial, helvetica, sans serif'><font color='#006699'><font size='+2'><b> <br>" >> $LOGDIR/ESMC_Methods_Not_Tested.html
echo "Public ESMC Methods Not Tested </b> </font></font></font>" >> $LOGDIR/ESMC_Methods_Not_Tested.html
echo "<pre style='margin-left: 40px;'><font face='arial,' helvetica='' sans='' serif='' style='font-family: sans-serif;'><font size='-1'><br></font></font><font face='arial,' helvetica='' sans='' serif='' size='-1' style='font-family: sans-serif;'>" >> $LOGDIR/ESMC_Methods_Not_Tested.html
echo "`date`" >> $LOGDIR/ESMC_Methods_Not_Tested.html
echo "<p>" >> $LOGDIR/ESMC_Methods_Not_Tested.html
echo "The following is the list of ESMF C public subroutines and functions that have not been tested:" >> $LOGDIR/ESMC_Methods_Not_Tested.html
echo "" >> $LOGDIR/ESMC_Methods_Not_Tested.html
echo "" >> $LOGDIR/ESMC_Methods_Not_Tested.html

#Start the ESMF_Methods_Tested.html file
cat file_header > $LOGDIR/ESMF_Methods_Tested.html
echo "<meta name='GENERATOR' content='Mozilla/4.79 [en] (X11; U; Linux 2.4.18-3smp i686) [Netscape]'><title>ESMF Methods Tested</title></head>" >> $LOGDIR/ESMF_Methods_Tested.html
echo "<body text='#000000' bgcolor='#ffffff' link='#993300' vlink='#666600' alink='#666600'>" >> $LOGDIR/ESMF_Methods_Tested.html
echo "<font face='arial, helvetica, sans serif'><font color='#006699'><font size='+2'><b> <br>" >> $LOGDIR/ESMF_Methods_Tested.html
echo "Public ESMF Methods Tested </b> </font></font></font>" >> $LOGDIR/ESMF_Methods_Tested.html
echo "<pre style='margin-left: 40px;'><font face='arial,' helvetica='' sans='' serif='' style='font-family: sans-serif;'><font size='-1'><br></font></font><font face='arial,' helvetica='' sans='' serif='' size='-1' style='font-family: sans-serif;'>" >> $LOGDIR/ESMF_Methods_Tested.html
echo "`date`" >> $LOGDIR/ESMF_Methods_Tested.html
echo "<p>" >> $LOGDIR/ESMF_Methods_Tested.html
echo "The following is the list of ESMF public subroutines and functions that have been partially or fully tested:" >> $LOGDIR/ESMF_Methods_Tested.html
echo "" >> $LOGDIR/ESMF_Methods_Tested.html
echo "" >> $LOGDIR/ESMF_Methods_Tested.html

#Start the ESMC_Methods_Tested.html file
cat file_header > $LOGDIR/ESMC_Methods_Tested.html
echo "<meta name='GENERATOR' content='Mozilla/4.79 [en] (X11; U; Linux 2.4.18-3smp i686) [Netscape]'><title>ESMC Methods Tested</title></head>" >> $LOGDIR/ESMC_Methods_Tested.html
echo "<body text='#000000' bgcolor='#ffffff' link='#993300' vlink='#666600' alink='#666600'>" >> $LOGDIR/ESMC_Methods_Tested.html
echo "<font face='arial, helvetica, sans serif'><font color='#006699'><font size='+2'><b> <br>" >> $LOGDIR/ESMC_Methods_Tested.html
echo "Public ESMC Methods Tested </b> </font></font></font>" >> $LOGDIR/ESMC_Methods_Tested.html
echo "<pre style='margin-left: 40px;'><font face='arial,' helvetica='' sans='' serif='' style='font-family: sans-serif;'><font size='-1'><br></font>
</font><font face='arial,' helvetica='' sans='' serif='' size='-1' style='font-family: sans-serif;'>" >> $LOGDIR/ESMC_Methods_Tested.html
echo "`date`" >> $LOGDIR/ESMC_Methods_Tested.html
echo "<p>" >> $LOGDIR/ESMC_Methods_Tested.html
echo "The following is the list of ESMF C public subroutines and functions that have been partially or fully tested:" >> $LOGDIR/ESMC_Methods_Tested.html
echo "" >> $LOGDIR/ESMC_Methods_Tested.html
echo "" >> $LOGDIR/ESMC_Methods_Tested.html
 
#Start the ESMF_Methods_Tests.html file
cat file_header > $LOGDIR/ESMF_Methods_Tests.html
echo "<meta name='GENERATOR' content='Mozilla/4.79 [en] (X11; U; Linux 2.4.18-3smp i686) [Netscape]'><title>ESMF Methods Tests</title></head>" >> $LOGDIR/ESMF_Methods_Tests.html
echo "<body text='#000000' bgcolor='#ffffff' link='#993300' vlink='#666600' alink='#666600'>" >> $LOGDIR/ESMF_Methods_Tests.html
echo "<font face='arial, helvetica, sans serif'><font color='#006699'><font size='+2'><b> <br>" >> $LOGDIR/ESMF_Methods_Tests.html
echo "Public ESMF Methods With Tests </b> </font></font></font>" >> $LOGDIR/ESMF_Methods_Tests.html
echo "<pre style='margin-left: 40px;'><font face='arial,' helvetica='' sans='' serif='' style='font-family: sans-serif;'><font size='-1'><br></font></font><font face='arial,' helvetica='' sans='' serif='' size='-1' style='font-family: sans-serif;'>" >> $LOGDIR/ESMF_Methods_Tests.html
echo "`date`" >> $LOGDIR/ESMF_Methods_Tests.html
echo "<p>" >> $LOGDIR/ESMF_Methods_Tests.html
echo "The following is the list of the implemented ESMF public subroutines and functions. Below each" >> $LOGDIR/ESMF_Methods_Tests.html
echo "is the path(s) to the test that calls the subroutine or function. Those without test" >> $LOGDIR/ESMF_Methods_Tests.html
echo "paths have not been tested." >> $LOGDIR/ESMF_Methods_Tests.html
echo "" >> $LOGDIR/ESMF_Methods_Tests.html
echo "" >> $LOGDIR/ESMF_Methods_Tests.html

#Start the ESMC_Methods_Tests.html file
cat file_header > $LOGDIR/ESMC_Methods_Tests.html
echo "<meta name='GENERATOR' content='Mozilla/4.79 [en] (X11; U; Linux 2.4.18-3smp i686) [Netscape]'><title>ESMC Methods Tests</title></head>" >> $LOGDIR/ESMC_Methods_Tests.html
echo "<body text='#000000' bgcolor='#ffffff' link='#993300' vlink='#666600' alink='#666600'>" >> $LOGDIR/ESMC_Methods_Tests.html
echo "<font face='arial, helvetica, sans serif'><font color='#006699'><font size='+2'><b> <br>" >> $LOGDIR/ESMC_Methods_Tests.html
echo "Public ESMC Methods With Tests </b> </font></font></font>" >> $LOGDIR/ESMC_Methods_Tests.html
echo "<pre style='margin-left: 40px;'><font face='arial,' helvetica='' sans='' serif='' style='font-family: sans-serif;'><font size='-1'><br></font></font><font face='arial,' helvetica='' sans='' serif='' size='-1' style='font-family: sans-serif;'>" >> $LOGDIR/ESMC_Methods_Tests.html
echo "`date`" >> $LOGDIR/ESMC_Methods_Tests.html
echo "<p>" >> $LOGDIR/ESMC_Methods_Tests.html
echo "The following is the list of the implemented ESMF C public subroutines and functions. Below each" >> $LOGDIR/ESMC_Methods_Tests.html
echo "is the path(s) to the test that calls the subroutine or function. Those without test" >> $LOGDIR/ESMC_Methods_Tests.html
echo "paths have not been tested." >> $LOGDIR/ESMC_Methods_Tests.html
echo "" >> $LOGDIR/ESMC_Methods_Tests.html
echo "" >> $LOGDIR/ESMC_Methods_Tests.html


#Start the ESMF_Testing_Metrics.html file
cat file_header > $LOGDIR/ESMF_Testing_Metrics.html
echo "<meta name='GENERATOR' content='Mozilla/4.79 [en] (X11; U; Linux 2.4.18-3smp i686) [Netscape]'><title>ESMF_Testing_Metrics</title></head>" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "<body text='#000000' bgcolor='#ffffff' link='#993300' vlink='#666600' alink='#666600'>" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "<font face='arial, helvetica, sans serif'><font color='#006699'><font size='+2'><b><br>" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "ESMF Public Methods Test Summary </b> </font></font></font>" >> $LOGDIR/ESMF_Testing_Metrics.html
echo " <blockquote>" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "<font face='arial, helvetica, sans serif'><font size='-1'>" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "</font></font><pre>" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "<font face='arial, helvetica, sans serif'><font size='-1'><span style='font-family: sans-serif;'>" >> $LOGDIR/ESMF_Testing_Metrics.html
count=0
unit_test=0

cat fortran_funcs
cat test_c_funcs
for i in $fortran_funcs;do
        echo "$i" >> $LOGDIR/ESMF_Methods_Tests.html
        file_size=`cat $LOGDIR/ESMF_Methods_Tests.html | wc -l`
        #find src/system_tests  -name "*.F90" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/ESMF_Methods_Tests.html \;
        #find src -name "*UTest.F90" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/ESMF_Methods_Tests.html \;
        #find src -name "*EX.F90" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/ESMF_Methods_Tests.html \;
        ##find src -name "*Ex.F90" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/ESMF_Methods_Tests.html \;
	# remove all commented out calls
        #grep -v "!" $LOGDIR/ESMF_Methods_Tests.html >> $LOGDIR/ESMF_Methods_Tests.html
	echo $i > tmp_file
	# Need special treatment of ESMF_TimeIntervalOperator(*)
	# because of the "*".
        if grep ESMF_TimeIntervalOperator tmp_file
        then
                echo "ESMF_TimeIntervalOperator(*)" > new_file
		#Comparing files instead of greping
                diff tmp_file new_file
                if [ "$?" = "0" ]
                then
                        echo "Files matched"
			find src -name "*STest.F90" -type f -exec  grep  -H "ESMF_TimeIntervalOperator(\*)" {} >  $LOGDIR/Methods_Tests \;
        		find src/system_tests  -name "*.F90" -type f -exec  grep  -H "ESMF_TimeIntervalOperator(\*)" {} >>  $LOGDIR/Methods_Tests \;
        		find src -name "*UTest.F90" -type f -exec  grep  -H "ESMF_TimeIntervalOperator(\*)" {} >>  $LOGDIR/Methods_Tests \;
        		find src -name "*EX.F90" -type f -exec  grep  -H "ESMF_TimeIntervalOperator(\*)" {} >>  $LOGDIR/Methods_Tests \;
        		find src -name "*Ex.F90" -type f -exec  grep  -H "ESMF_TimeIntervalOperator(\*)" {} >>  $LOGDIR/Methods_Tests \;
                fi
	else
		find src -name "*STest.F90" -type f -exec  grep  -H "$i *(" {} >  $LOGDIR/Methods_Tests \;
        	find src/system_tests  -name "*.F90" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/Methods_Tests \;
        	find src -name "*UTest.F90" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/Methods_Tests \;
        	find src -name "*EX.F90" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/Methods_Tests \;
        	find src -name "*Ex.F90" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/Methods_Tests \;
        	find src/addon/NUOPC/tests -name "*.F90" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/Methods_Tests \;
        	#find src/addon/MAPL/MAPL* -name "*.*90" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/Methods_Tests \;
        fi
	# get all commented out lines and get those with Operator in them
        grep ": *!" $LOGDIR/Methods_Tests > Commented_lines
        if grep Operator tmp_file
        then
		grep "Operator" Commented_lines > Operator_lines
         fi
        if grep Function tmp_file
        then
		grep "(MOD)" Commented_lines >> Operator_lines
        fi
        if grep Assignment tmp_file
        then
                grep "Assignment" Commented_lines > Operator_lines
         fi

        # remove all commented out calls (! after the ":"
        grep -v ": *!" $LOGDIR/Methods_Tests >> $LOGDIR/ESMF_Methods_Tests.html
	# Add operator lines if it exists
	if [ -e Operator_lines ]
	then
	cat Operator_lines >> $LOGDIR/ESMF_Methods_Tests.html
	fi
        # remove Operator_lines
        rm -f Operator_lines
        new_file_size=`cat $LOGDIR/ESMF_Methods_Tests.html | wc -l`
        new_file_size=`expr $new_file_size + 0`
	if [ $new_file_size -ne  $file_size ]
        then
		unit_test=`expr $unit_test + 1`
		echo "" >> $LOGDIR/ESMF_Methods_Tested.html 
        	echo "$i" >> $LOGDIR/ESMF_Methods_Tested.html 
	else
		echo "" >> $LOGDIR/ESMF_Methods_Not_Tested.html
        	echo "$i" >> $LOGDIR/ESMF_Methods_Not_Tested.html
        fi
	echo "" >> $LOGDIR/ESMF_Methods_Tests.html
	echo "" >> $LOGDIR/ESMF_Methods_Tests.html
	echo -n \.
done
echo " finished accummulating info"
echo ""
echo "" >> $LOGDIR/ESMF_Testing_Metrics.html
fortranCount=`expr $fortranCount + 0`
percent_tested=`expr $unit_test '*' 100`
percent_tested=`expr $percent_tested '/' $fortranCount`
echo "As of `date +%c`" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "$fortranCount ESMF Fortran methods implemented." >> $LOGDIR/ESMF_Testing_Metrics.html
echo "" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "$unit_test fully or partially tested." >> $LOGDIR/ESMF_Testing_Metrics.html
echo "" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "$percent_tested% fully or partially tested." >> $LOGDIR/ESMF_Testing_Metrics.html

echo " finished caluclating percentage "

count=0
unit_test=0

cat c_funcs
for i in $test_c_funcs;do
	echo "$i"
        echo "$i" >> $LOGDIR/ESMC_Methods_Tests.html
        file_size=`cat $LOGDIR/ESMC_Methods_Tests.html | wc -l`
        # remove all commented out calls
        #grep -v "//" $LOGDIR/ESMC_Methods_Tests.html >> $LOGDIR/ESMC_Methods_Tests.html
        find src/system_tests -name "*STest.C" -type f -exec  grep  -H "$i *(" {} >  $LOGDIR/Methods_Tests \;
        find src/system_tests  -name "*.C" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/Methods_Tests \;
        find src -name "*UTest.C" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/Methods_Tests \;
        find src -name "*EX.C" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/Methods_Tests \;
        find src -name "*Ex.C" -type f -exec  grep  -H "$i *(" {} >>  $LOGDIR/Methods_Tests \;
        # get all commented out lines and get those with Operator in them
        #grep ": *\/\/" $LOGDIR/Methods_Tests > Commented_lines
        #echo $i > tmp_file
        #if grep Operator tmp_file
        #then
                #grep "Operator" Commented_lines > Operator_lines
         #fi
        #if grep Function tmp_file
        #then
                grep "(MOD)" Commented_lines >> Operator_lines
         #fi
        # remove all commented out calls (// after the ":"
        grep -v ": *\/\/" $LOGDIR/Methods_Tests >> $LOGDIR/ESMC_Methods_Tests.html
        # Add operator lines if it exists
        #if [ -e Operator_lines ]
        #then
        #cat Operator_lines >> $LOGDIR/ESMC_Methods_Tests.html
        #fi
        # remove Operator_lines
        rm -f Operator_lines
        new_file_size=`cat $LOGDIR/ESMC_Methods_Tests.html | wc -l`
        new_file_size=`expr $new_file_size + 0`
        if [ $new_file_size -ne  $file_size ]
        then
                unit_test=`expr $unit_test + 1`
                echo "" >> $LOGDIR/ESMC_Methods_Tested.html
                echo "$i" >> $LOGDIR/ESMC_Methods_Tested.html
        else
                echo "" >> $LOGDIR/ESMC_Methods_Not_Tested.html
                echo "$i" >> $LOGDIR/ESMC_Methods_Not_Tested.html
        fi
        echo "" >> $LOGDIR/ESMC_Methods_Tests.html
        echo "" >> $LOGDIR/ESMC_Methods_Tests.html
        echo -n \.
done

echo " finished accummulating info"
echo ""
echo "" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "" >> $LOGDIR/ESMF_Testing_Metrics.html
C_Count=`expr $C_Count + 0`
percent_tested=`expr $unit_test '*' 100`
percent_tested=`expr $percent_tested '/' $C_Count`
echo "" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "$C_Count ESMC C methods implemented." >> $LOGDIR/ESMF_Testing_Metrics.html
echo "" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "$unit_test fully or partially tested." >> $LOGDIR/ESMF_Testing_Metrics.html
echo "" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "$percent_tested% fully or partially tested." >> $LOGDIR/ESMF_Testing_Metrics.html

echo " finished caluclating percentage "


echo "</span><br></span> </font></font></pre></body></html>" >> $LOGDIR/ESMF_Methods.html
echo "</span><br></ font></font></pre></blockquote></body></html>" >> $LOGDIR/ESMF_Testing_Metrics.html
echo "</span><br></span> </font></font></pre></body></html>" >> $LOGDIR/ESMF_Methods_Not_Tested.html
echo "</span><br></span> </font></font></pre></body></html>" >> $LOGDIR/ESMF_Methods_Tested.html
echo "</span><br></span> </font></font></pre></body></html>" >> $LOGDIR/ESMF_Methods_Tests.html

echo "</span><br></span> </font></font></pre></body></html>" >> $LOGDIR/ESMC_Methods.html
echo "</span><br></span> </font></font></pre></body></html>" >> $LOGDIR/ESMC_Methods_Not_Tested.html
echo "</span><br></span> </font></font></pre></body></html>" >> $LOGDIR/ESMC_Methods_Tested.html
echo "</span><br></span> </font></font></pre></body></html>" >> $LOGDIR/ESMC_Methods_Tests.html

