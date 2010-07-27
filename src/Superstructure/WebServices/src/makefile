# $Id: makefile,v 1.1 2010/07/27 05:52:46 theurich Exp $

ALL: build_here 

AUTOGEN   = 

SOURCEC	  = ESMCI_WebServNetEsmfClient.C ESMCI_WebServCAMOutputFile.C ESMCI_WebServNetEsmfServer.C ESMCI_WebServClientInfo.C ESMCI_WebServPassThruSvr.C ESMCI_WebServClientSocket.C ESMCI_WebServRegistrarClient.C ESMCI_WebServComponentSvr.C ESMCI_WebServServerSocket.C ESMCI_WebServCompSvrClient.C ESMCI_WebServSocketUtils.C ESMCI_WebServInterface.C ESMCI_WebServLowLevelSocket.C
SOURCEF	  = ESMF_WebServComponentAPI.F90
SOURCEH	  = 
STOREH    =
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/WebServices/src

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) ) ESMF_LogFile
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 

