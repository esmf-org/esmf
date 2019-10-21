# $Id$

ALL: build_here 

SOURCEC	  = binary.cpp emitterutils.cpp nodeevents.cpp scanscalar.cpp \
            convert.cpp exceptions.cpp null.cpp scantag.cpp directives.cpp \
            exp.cpp ostream_wrapper.cpp scantoken.cpp emit.cpp memory.cpp \
            parse.cpp simplekey.cpp emitfromevents.cpp nodebuilder.cpp \
            parser.cpp singledocparser.cpp emitter.cpp node.cpp regex_yaml.cpp \
            stream.cpp emitterstate.cpp node_data.cpp scanner.cpp tag.cpp
SOURCEF	  =  
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/prologue/yaml-cpp/src

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 

