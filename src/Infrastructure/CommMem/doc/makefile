# $Id: makefile,v 1.3 2003/03/13 22:56:09 cdeluca Exp $

All:
	do_chprotex ESMC_Comm ../include/ESMC_Comm.h
	do_ccprotex ESMC_Comm ../src/ESMC_Comm.C
	do_chprotex ESMC_DE ../include/ESMC_DE.h
	do_ccprotex ESMC_DE ../src/ESMC_DE.C
	do_chprotex ESMC_DELayout ../include/ESMC_DELayout.h
	do_ccprotex ESMC_DELayout ../src/ESMC_DELayout.C
	do_chprotex ESMC_Machine ../include/ESMC_Machine.h
	do_ccprotex ESMC_Machine ../src/ESMC_Machine.C
	do_chprotex ESMC_PE ../include/ESMC_PE.h
	do_ccprotex ESMC_PE ../src/ESMC_PE.C
	do_chprotex ESMC_PEList ../include/ESMC_PEList.h
	do_ccprotex ESMC_PEList ../src/ESMC_PEList.C
	do_latex CommMem des
	do_l2h CommMem des
	dvipdf CommMem_desdoc.dvi
