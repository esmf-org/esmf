# $Id$

ALL: build_here

SOURCEC  =
SOURCEF  = \
  dbdsqr.f  dgeqrf.f    dlalsa.f  dlarfb.f  dlascl.f  dlaset.f  dlassq.f  dormrz.f \
  dgebd2.f  disnan.f    dlalsd.f  dlarf.f   dlasd4.f  dlasq1.f  dlasv2.f  dtzrzf.f \
  dgebrd.f  dlabad.f    dlamch.f  dlarfg.f  dlasd5.f  dlasq2.f  dlatrz.f  ieeeck.f \
  dgelq2.f  dlabrd.f    dlamrg.f  dlarft.f  dlasd6.f  dlasq3.f  dorm2r.f  iladlc.f \
  dgelqf.f  dlacpy.f    dlange.f  dlartg.f  dlasd7.f  dlasq4.f  dormbr.f  iladlr.f \
  dgelsd.f  dlaed6.f    dlanst.f  dlarzb.f  dlasd8.f  dlasq5.f  dorml2.f  ilaenv.f \
  dgelsy.f  dlaic1.f    dlapy2.f  dlarz.f   dlasda.f  dlasq6.f  dormlq.f  iparmq.f \
  dgeqp3.f  dlaisnan.f  dlaqp2.f  dlarzt.f  dlasdq.f  dlasr.f   dormqr.f  lsame.f \
  dgeqr2.f  dlals0.f    dlaqps.f  dlas2.f   dlasdt.f  dlasrt.f  dormr3.f  xerbla.f  

SOURCEH  =

# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/src/Lapack

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 



