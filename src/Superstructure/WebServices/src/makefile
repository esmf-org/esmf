# $Id: makefile,v 1.5 2011/08/04 21:09:19 ksaint Exp $

ALL: build_here 

AUTOGEN   = 

SOURCEC	  = ESMCI_WebServNetEsmfClient.C \
             ESMCI_WebServCAMOutputFile.C \
             ESMCI_WebServNetEsmfServer.C \
             ESMCI_WebServClientInfo.C \
             ESMCI_WebServCompSvrInfo.C \
             ESMCI_WebServPassThruSvr.C \
             ESMCI_WebServClientSocket.C \
             ESMCI_WebServServerSocket.C \
             ESMCI_WebServRegistrarClient.C \
             ESMCI_WebServComponentSvr.C \
             ESMCI_WebServCompSvrClient.C \
             ESMCI_WebServSecureServerSocket.C \
             ESMCI_WebServSecureClientSocket.C \
             ESMCI_WebServSocketUtils.C \
             ESMCI_WebServSecureUtils.C \
             ESMCI_WebServLowLevelSocket.C \
             ESMCI_WebServSecureSocket.C \
             ESMCI_WebServCompSvrMgr.C \
             ESMCI_WebServGRAMClient.C \
             ESMCI_WebServForkClient.C \
             ESMCI_WebServProcCtrl.C \
             ESMCI_WebServProcCtrlClient.C \
             ESMCI_WebServ_F.C
SOURCEF	  = ESMF_WebServ.F90 ESMF_WebServComponent_C.F90
SOURCEH	  = ESMCI_WebServ.h
STOREH    = ESMCI_WebServLowLevelSocket.h \
STOREH    = ESMCI_WebServSecureSocket.h \
            ESMCI_WebServServerSocket.h \
            ESMCI_WebServClientSocket.h \
            ESMCI_WebServNetEsmf.h \
            ESMCI_WebServClientInfo.h \
            ESMCI_WebServCAMOutputFile.h \
            ESMCI_WebServCompSvrClient.h \
            ESMCI_WebServSecureCompSvrClient.h \
            ESMCI_WebServProcCtrlClient.h \
            ESMCI_WebServPassThruSvr.h
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

