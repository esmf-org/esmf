/*
 *
 */

#ifndef _NetEsmf_h_
#define _NetEsmf_h_


#define NET_ESMF_PORT	27001

#define NET_ESMF_EXIT			10
#define NET_ESMF_NEW	   		15
#define NET_ESMF_INIT			20
#define NET_ESMF_RUN				21
#define NET_ESMF_FINAL			22
#define NET_ESMF_STATE			23
#define NET_ESMF_FILES			24
#define NET_ESMF_INIT_DONE		25
#define NET_ESMF_RUN_DONE		26
#define NET_ESMF_FINAL_DONE	27
#define NET_ESMF_DATA			28
#define NET_ESMF_PING			30
#define NET_ESMF_END				40
#define NET_ESMF_UNKN			50

#define NET_ESMF_STAT_IDLE				0
#define NET_ESMF_STAT_READY			1
#define NET_ESMF_STAT_BUSY				2
#define NET_ESMF_STAT_INITIALIZING	3
#define NET_ESMF_STAT_RUNNING			4
#define NET_ESMF_STAT_FINALIZING		5
#define NET_ESMF_STAT_INIT_DONE		6
#define NET_ESMF_STAT_RUN_DONE		7
#define NET_ESMF_STAT_FINAL_DONE		8
#define NET_ESMF_STAT_DONE				9
#define NET_ESMF_STAT_ERROR			99


#endif
