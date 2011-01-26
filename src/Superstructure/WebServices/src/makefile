# $Id: makefile,v 1.3 2011/01/26 04:53:28 ksaint Exp $

ALL: build_here 

AUTOGEN   = 

SOURCEC	  = ESMCI_WebServNetEsmfClient.C ESMCI_WebServCAMOutputFile.C ESMCI_WebServNetEsmfServer.C ESMCI_WebServClientInfo.C ESMCI_WebServPassThruSvr.C ESMCI_WebServClientSocket.C ESMCI_WebServRegistrarClient.C ESMCI_WebServComponentSvr.C ESMCI_WebServServerSocket.C ESMCI_WebServCompSvrClient.C ESMCI_WebServSocketUtils.C ESMCI_WebServLowLevelSocket.C ESMCI_WebServ_F.C
SOURCEF	  = ESMF_WebServ.F90 ESMF_WebServComponent_C.F90
SOURCEH	  = ESMCI_WebServ.h
STOREH    = ESMCI_WebServLowLevelSocket.h ESMCI_WebServServerSocket.h ESMCI_WebServClientSocket.h ESMCI_WebServNetEsmf.h ESMCI_WebServClientInfo.h ESMCI_WebServCAMOutputFile.h ESMCI_WebServPassThruSvr.h
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

