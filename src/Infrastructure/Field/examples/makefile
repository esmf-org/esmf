# $Id: makefile,v 1.25.2.7 2008/05/01 21:46:32 feiliu Exp $ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Field/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_FieldCreateEx \
                 $(ESMF_EXDIR)/ESMF_FieldEx
#                 $(ESMF_EXDIR)/ESMF_FieldGetEx
#                 $(ESMF_EXDIR)/ESMF_FieldSetEx
#                 $(ESMF_EXDIR)/ESMF_FieldFromUserEx \
#                 $(ESMF_EXDIR)/ESMF_FieldGlobalEx \
#                 $(ESMF_EXDIR)/ESMF_FieldWriteEx

EXAMPLES_RUN = run_ESMF_FieldCreateEx \
               run_ESMF_FieldEx
#               run_ESMF_FieldGetEx
#                   run_ESMF_FieldSetEx
#                   run_ESMF_FieldFromUserEx \
#                   run_ESMF_FieldGlobalEx \
#                   run_ESMF_FieldWriteEx 

EXAMPLES_RUN_UNI = run_ESMF_FieldCreateEx_uni \
                   run_ESMF_FieldEx_uni
#                   run_ESMF_FieldGetEx_uni
#                   run_ESMF_FieldSetEx_uni
#                   run_ESMF_FieldFromUserEx_uni \
#                   run_ESMF_FieldGlobalEx_uni \
#                   run_ESMF_FieldWriteEx_uni

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = foo.nc $(EXAMPLES_BUILD) 0 1 2 3
CLOBBERDIRS =

#
# ESMF_FieldEx
#
run_ESMF_FieldEx:
	$(MAKE) EXNAME=Field NP=4 exfrun

run_ESMF_FieldEx_uni:
	echo "PASS: ESMF_FieldEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldEx.stdout

#
# ESMF_FieldCreateEx
#
run_ESMF_FieldCreateEx:
	$(MAKE) EXNAME=FieldCreate NP=4 exfrun

run_ESMF_FieldCreateEx_uni:
	echo "PASS: ESMF_FieldCreateEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldCreateEx.stdout

#
# ESMF_FieldGetEx
#
run_ESMF_FieldGetEx:
	$(MAKE) EXNAME=FieldGet NP=4 exfrun
run_ESMF_FieldGetEx_uni:
	echo "PASS: ESMF_FieldGetEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldGetEx.stdout

#
# ESMF_FieldSetEx
#
run_ESMF_FieldSetEx:
	$(MAKE) EXNAME=FieldSet NP=4 exfrun
run_ESMF_FieldSetEx_uni:
	echo "PASS: ESMF_FieldSetEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldSetEx.stdout

#
# ESMF_FieldFromUserEx
#
EXAMPLE_FieldFromUser_OBJS = UserCodeMod.o
ESMF_FieldFromUserEx.o	: UserCodeMod.o

run_ESMF_FieldFromUserEx:
	$(MAKE) EXNAME=FieldFromUser NP=6 exfrun

run_ESMF_FieldFromUserEx_uni:
	echo "PASS: ESMF_FieldFromUserEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldFromUserEx.stdout

#
# ESMF_FieldGlobalEx
#
run_ESMF_FieldGlobalEx:
	$(MAKE) EXNAME=FieldGlobal NP=6 exfrun

run_ESMF_FieldGlobalEx_uni:
	echo "PASS: ESMF_FieldGlobalEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldGlobalEx.stdout

#
# ESMF_FieldWriteEx
#
 run_ESMF_FieldWriteEx:
	$(MAKE) EXNAME=FieldWrite NP=4 exfrun

 run_ESMF_FieldWriteEx_uni:
	$(MAKE) EXNAME=FieldWrite NP=1 exfrun
