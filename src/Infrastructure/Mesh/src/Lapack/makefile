# $Id$

ALL: build_here

SOURCEC  =
SOURCEF  = \
  dbdsqr.F90  dgeqrf.F90    dlalsa.F90  dlarfb.F90  dlascl.F90  dlaset.F90  dlassq.F90  dormrz.F90 \
  dgebd2.F90  disnan.F90    dlalsd.F90  dlarf.F90   dlasd4.F90  dlasq1.F90  dlasv2.F90  dtzrzf.F90 \
  dgebrd.F90  dlabad.F90    dlamch.F90  dlarfg.F90  dlasd5.F90  dlasq2.F90  dlatrz.F90  ieeeck.F90 \
  dgelq2.F90  dlabrd.F90    dlamrg.F90  dlarft.F90  dlasd6.F90  dlasq3.F90  dorm2r.F90  iladlc.F90 \
  dgelqf.F90  dlacpy.F90    dlange.F90  dlartg.F90  dlasd7.F90  dlasq4.F90  dormbr.F90  iladlr.F90 \
  dgelsd.F90  dlaed6.F90    dlanst.F90  dlarzb.F90  dlasd8.F90  dlasq5.F90  dorml2.F90  ilaenv.F90 \
  dgelsy.F90  dlaic1.F90    dlapy2.F90  dlarz.F90   dlasda.F90  dlasq6.F90  dormlq.F90  iparmq.F90 \
  dgeqp3.F90  dlaisnan.F90  dlaqp2.F90  dlarzt.F90  dlasdq.F90  dlasr.F90   dormqr.F90  lsame.F90 \
  dgeqr2.F90  dlals0.F90    dlaqps.F90  dlas2.F90   dlasdt.F90  dlasrt.F90  dormr3.F90  xerbla.F90  

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



