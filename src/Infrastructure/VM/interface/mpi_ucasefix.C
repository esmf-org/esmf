/*
 * the normal MPI libs are mixed case calls.  when we compile our code on
 * some platforms we have to force all external symbols to lower case to make
 * the C++/F90 linking work (without having to supply 3 different entry points
 * for each C routine called from fortran - all lower case, all upper case,
 * mixed case).  but this affects not only our calls to C, but also any other
 * external lib calls - including direct calls to MPI.  we do not make such
 * calls from our F90 code, but users might, and if they use our makefiles 
 * unchanged it will include our flags for case mangling.  so these files
 * (lcase and ucase) may help link without having to go into altering the
 * default makefile flags.
 */

#ifndef _MPI_UCASEFIX_H
#define _MPI_UCASEFIX_H

#include <mpi.h>


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#if 0  // these are already fully all ucase */
int MPI_DUP_FN (MPI_Comm a, int b, void *c, void *d, void *e, int *f) { 
				    return MPI_DUP_FN(a,b,c,d,e,f); }
int MPI_COMM_DUP_FN (MPI_Comm a, int b, void *c, void *d, void *e, int *f) { 
				    return MPI_COMM_DUP_FN(a,b,c,d,e,f); }
int MPI_TYPE_DUP_FN (MPI_Datatype a, int b, void *c, void *d, void *e, int *f) { 
				    return MPI_TYPE_DUP_FN(a,b,c,d,e,f); }
int MPI_WIN_DUP_FN (MPI_Win a, int b, void *c, void *d, void *e, int *f) { 
				    return MPI_WIN_DUP_FN(a,b,c,d,e,f); }
#endif

int MPI_ABORT (MPI_Comm a, int b) { 
                                    return MPI_Abort(a,b); }
int MPI_ACCUMULATE (void *a, int b, MPI_Datatype c, int d, MPI_Aint e, 
		    int f, MPI_Datatype g, MPI_Op h, MPI_Win i) {
                                   return MPI_Accumulate(a,b,c,d,e,f,g,h,i); }
int MPI_ADDRESS (void * a, MPI_Aint *b) { 
				    return MPI_Address(a,b); }
int MPI_ALLGATHER (void *a, int b, MPI_Datatype c, void *d, int e, 
		   MPI_Datatype f, MPI_Comm g) { 
                                    return MPI_Allgather(a,b,c,d,e,f,g); }
int MPI_ALLGATHERV (void *a, int b, MPI_Datatype c, void *d, int *e, 
		    int *f, MPI_Datatype g, MPI_Comm h) { 
				    return MPI_Allgatherv(a,b,c,d,e,f,g,h); }
int MPI_ALLOC_MEM (MPI_Aint size, MPI_Info info, void *baseptr) { 
				    return MPI_Alloc_mem(size,info,baseptr); }
int MPI_ALLREDUCE (void *a, void *b, int c, MPI_Datatype d, MPI_Op e, 
                  MPI_Comm f) { 
				    return MPI_Allreduce(a,b,c,d,e,f); }
int MPI_ALLTOALL (void *a, int b, MPI_Datatype c, void *d, int e, 
		  MPI_Datatype f, MPI_Comm g) { 
				    return MPI_Alltoall(a,b,c,d,e,f,g); }
int MPI_ALLTOALLV (void *a, int *b, int *c, MPI_Datatype d, void *e, 
		   int *f, int *g, MPI_Datatype h, MPI_Comm i) { 
				    return MPI_Alltoallv(a,b,c,d,e,f,g,h,i); }
int MPI_ALLTOALLW (void *a, int *b, int *c, MPI_Datatype *d, void *e,
		   int *f, int *g, MPI_Datatype *h, MPI_Comm i) { 
				    return MPI_Alltoallw(a,b,c,d,e,f,g,h,i); }
int MPI_ATTR_DELETE (MPI_Comm a, int b) { 
				    return MPI_Attr_delete(a,b); }
int MPI_ATTR_GET (MPI_Comm a, int b, void *c, int *d) { 
				    return MPI_Attr_get(a,b,c,d); }
int MPI_ATTR_PUT (MPI_Comm a, int b, void *c) { 
				    return MPI_Attr_put(a,b,c); }
int MPI_BARRIER (MPI_Comm a) { 
				    return MPI_Barrier(a); }
int MPI_BCAST (void *a, int b, MPI_Datatype c, int d, MPI_Comm e) { 
				    return MPI_Bcast(a,b,c,d,e); }
int MPI_BSEND (void *a, int b, MPI_Datatype c, int d, int e, MPI_Comm f) { 
				    return MPI_Bsend(a,b,c,d,e,f); }
int MPI_BSEND_INIT (void *a, int b, MPI_Datatype c, int d, int e, 
		    MPI_Comm f, MPI_Request *g) { 
				    return MPI_Bsend_init(a,b,c,d,e,f,g); }
int MPI_BUFFER_ATTACH (void *a, int b) { 
				    return MPI_Buffer_attach(a,b); }
int MPI_BUFFER_DETACH (void *a, int *b) { 
				    return MPI_Buffer_detach(a,b); }
int MPI_CANCEL (MPI_Request *a) { 
				    return MPI_Cancel(a); }
int MPI_CART_COORDS (MPI_Comm a, int b, int c, int *d) { 
				    return MPI_Cart_coords(a,b,c,d); }
int MPI_CART_CREATE (MPI_Comm a, int b, int *c, int *d, int e, MPI_Comm *f) { 
				    return MPI_Cart_create(a,b,c,d,e,f); }
int MPI_CARTDIM_GET (MPI_Comm a, int *b) { 
				    return MPI_Cartdim_get(a,b); }
int MPI_CART_GET (MPI_Comm a, int b, int *c, int *d, int *e) { 
				    return MPI_Cart_get(a,b,c,d,e); }
int MPI_CART_MAP (MPI_Comm a, int b, int *c, int *d, int *e) { 
				    return MPI_Cart_map(a,b,c,d,e); }
int MPI_CART_RANK (MPI_Comm a, int *b, int *c) { 
				    return MPI_Cart_rank(a,b,c); }
int MPI_CART_SHIFT (MPI_Comm a, int b, int c, int *d, int *e) { 
				    return MPI_Cart_shift(a,b,c,d,e); }
int MPI_CART_SUB (MPI_Comm a, int *b, MPI_Comm *c) { 
				return MPI_Cart_sub(a,b,c); }
int MPI_CLOSE_PORT (char *a) { 
				return MPI_Close_port(a); }
MPI_Fint MPI_COMM_C2F (MPI_Comm a) { 
				return MPI_Comm_c2f(a); }
int MPI_COMM_ACCEPT (char *a, MPI_Info b, int c, MPI_Comm d, MPI_Comm *e) { 
				return MPI_Comm_accept(a,b,c,d,e); }
int MPI_COMM_COMPARE (MPI_Comm a, MPI_Comm b, int *c) { 
				return MPI_Comm_compare(a,b,c); }
int MPI_COMM_CONNECT (char *a, MPI_Info b, int c, MPI_Comm d, MPI_Comm *e) { 
				return MPI_Comm_connect(a,b,c,d,e); }
int MPI_COMM_CREATE (MPI_Comm a, MPI_Group b, MPI_Comm *c) { 
				return MPI_Comm_create(a,b,c); }
int MPI_COMM_CREATE_ERRHANDLER (MPI_Comm_errhandler_fn *a, MPI_Errhandler *b) { 
				return MPI_Comm_create_errhandler(a,b); }
int MPI_COMM_CREATE_KEYVAL (MPI_Comm_copy_attr_function *a, 
			    MPI_Comm_delete_attr_function *b, int *c, void *d) { 
				return MPI_Comm_create_keyval(a,b,c,d); }
int MPI_COMM_DELETE_ATTR (MPI_Comm a, int b) { 
				return MPI_Comm_delete_attr(a,b); }
int MPI_COMM_DISCONNECT (MPI_Comm *a) { 
				return MPI_Comm_disconnect(a); }
int MPI_COMM_DUP (MPI_Comm a, MPI_Comm *b) { 
				return MPI_Comm_dup(a,b); }
MPI_Comm MPI_COMM_F2C (MPI_Fint a) { 
				return MPI_Comm_f2c(a); }
int MPI_COMM_FREE (MPI_Comm *a) { 
				return MPI_Comm_free(a); }
int MPI_COMM_FREE_KEYVAL (int *a) { 
				return MPI_Comm_free_keyval(a); }
int MPI_COMM_GET_ATTR (MPI_Comm a, int b, void *c, int *d) { 
				return MPI_Comm_get_attr(a,b,c,d); }
int MPI_COMM_GET_ERRHANDLER (MPI_Comm a, MPI_Errhandler *b) { 
				return MPI_Comm_get_errhandler(a,b); }
int MPI_COMM_GET_NAME (MPI_Comm a, char *b, int *c) { 
				return MPI_Comm_get_name(a,b,c); }
int MPI_COMM_GET_PARENT (MPI_Comm *a) { 
				return MPI_Comm_get_parent(a); }
int MPI_COMM_GROUP (MPI_Comm a, MPI_Group *b) { 
				return MPI_Comm_group(a,b); }
int MPI_COMM_JOIN (int a, MPI_Comm *b) { 
				return MPI_Comm_join(a,b); }
int MPI_COMM_RANK (MPI_Comm a, int *b) { 
				return MPI_Comm_rank(a,b); }
int MPI_COMM_REMOTE_GROUP (MPI_Comm a, MPI_Group *b) { 
				return MPI_Comm_remote_group(a,b); }
int MPI_COMM_REMOTE_SIZE (MPI_Comm a, int *b) { 
				return MPI_Comm_remote_size(a,b); }
int MPI_COMM_SET_ATTR (MPI_Comm a, int b, void *c) { 
				return MPI_Comm_set_attr(a,b,c); }
int MPI_COMM_SET_ERRHANDLER (MPI_Comm a, MPI_Errhandler b) { 
				return MPI_Comm_set_errhandler(a,b); }
int MPI_COMM_SET_NAME (MPI_Comm a, char *b) { 
				return MPI_Comm_set_name(a,b); }
int MPI_COMM_SIZE (MPI_Comm a, int *b) { 
				return MPI_Comm_size(a,b); }
int MPI_COMM_SPAWN (char *a, char **b, int c, MPI_Info d, int e, 
		    MPI_Comm f, MPI_Comm *g, int *h) { 
				return MPI_Comm_spawn(a,b,c,d,e,f,g,h); }
int MPI_COMM_SPAWN_MULTIPLE (int a, char **b, char ***c, int *d, MPI_Info *e, 
                             int f, MPI_Comm g, MPI_Comm *h, int *i) { 
			return MPI_Comm_spawn_multiple(a,b,c,d,e,f,g,h,i); }
int MPI_COMM_SPLIT (MPI_Comm a, int b, int c, MPI_Comm *d) { 
				return MPI_Comm_split(a,b,c,d); }
int MPI_COMM_TEST_INTER (MPI_Comm a, int *b) { 
				return MPI_Comm_test_inter(a,b); }
int MPI_DIMS_CREATE (int a, int b, int *c) { 
				return MPI_Dims_create(a,b,c); }
MPI_Fint MPI_ERRHANDLER_C2F (MPI_Errhandler err) { 
				return MPI_Errhandler_c2f(err); }
int MPI_ERRHANDLER_CREATE (MPI_Handler_function *a, MPI_Errhandler *b) { 
				return MPI_Errhandler_create(a,b); }
MPI_Errhandler MPI_ERRHANDLER_F2C (MPI_Fint f_handle) { 
				return MPI_Errhandler_f2c(f_handle); }
int MPI_ERRHANDLER_FREE (MPI_Errhandler *a) { 
				return MPI_Errhandler_free(a); }
int MPI_ERRHANDLER_GET (MPI_Comm a, MPI_Errhandler *b) { 
				return MPI_Errhandler_get(a,b); }
int MPI_ERRHANDLER_SET (MPI_Comm a, MPI_Errhandler b) { 
				return MPI_Errhandler_set(a,b); }
int MPI_ERROR_CLASS (int a, int *b) { 
				return MPI_Error_class(a,b); }
int MPI_ERROR_STRING (int a, char *b, int *c) { 
					return MPI_Error_string(a,b,c); }
int MPI_EXSCAN (void *a, void *b, int c, MPI_Datatype d, MPI_Op e, MPI_Comm f) { 
					return MPI_Exscan(a,b,c,d,e,f); }
int MPI_FINALIZE (void) { 
					return MPI_Finalize(); }
int MPI_FINALIZED (int *flag) { 
					return MPI_Finalized(flag); }
int MPI_FREE_MEM (void *base) { 
					return MPI_Free_mem(base); }
int MPI_GATHER (void *a, int b, MPI_Datatype c, void *d, int e, 
	        MPI_Datatype f, int g, MPI_Comm h) { 
					return MPI_Gather(a,b,c,d,e,f,g,h); }
int MPI_GATHERV (void *a, int b, MPI_Datatype c, void *d, int *e, 
		        int *f, MPI_Datatype g, int h, MPI_Comm i) { 
					return MPI_Gatherv(a,b,c,d,e,f,g,h,i); }
int MPI_GET_ADDRESS (void *a, MPI_Aint *b) { 
					return MPI_Get_address(a,b); }
int MPI_GET (void *a, int b, MPI_Datatype c, int d, MPI_Aint e, int f, 
	     MPI_Datatype g, MPI_Win h) { 
					return MPI_Get(a,b,c,d,e,f,g,h); }
int MPI_GET_COUNT (MPI_Status *a, MPI_Datatype b, int *c) { 
					return MPI_Get_count(a,b,c); }
int MPI_GET_ELEMENTS (MPI_Status *a, MPI_Datatype b, int *c) { 
					return MPI_Get_elements(a,b,c); }
int MPI_GET_PROCESSOR_NAME (char *a, int *b) { 
					return MPI_Get_processor_name(a,b); }
int MPI_GET_VERSION (int *a, int *b) { return MPI_Get_version(a,b); }
int MPI_GRAPH_CREATE (MPI_Comm a, int b, int *c, int *d, int e, MPI_Comm *f) { 
					return MPI_Graph_create(a,b,c,d,e,f); }
int MPI_GRAPHDIMS_GET (MPI_Comm a, int *b, int *c) { 
					return MPI_Graphdims_get(a,b,c); }
int MPI_GRAPH_GET (MPI_Comm a, int b, int c, int *d, int *e) { 
					return MPI_Graph_get(a,b,c,d,e); }
int MPI_GRAPH_MAP (MPI_Comm a, int b, int *c, int *d, int *e) { 
					return MPI_Graph_map(a,b,c,d,e); }
int MPI_GRAPH_NEIGHBORS (MPI_Comm a, int b, int c, int *d) { 
					return MPI_Graph_neighbors(a,b,c,d); }
int MPI_GRAPH_NEIGHBORS_COUNT (MPI_Comm a, int b, int *c) { 
					return MPI_Graph_neighbors_count(a,b,c); }
MPI_Fint MPI_GROUP_C2F (MPI_Group a) { return MPI_Group_c2f(a); }
int MPI_GROUP_COMPARE (MPI_Group a, MPI_Group b, int *c) { 
					return MPI_Group_compare(a,b,c); }
int MPI_GROUP_DIFFERENCE (MPI_Group a, MPI_Group b, MPI_Group *c) { 
					return MPI_Group_difference(a,b,c); }
int MPI_GROUP_EXCL (MPI_Group a, int b, int *c, MPI_Group *d) { 
					return MPI_Group_excl(a,b,c,d); }
MPI_Group MPI_GROUP_F2C (MPI_Fint a) { return MPI_Group_f2c(a); }
int MPI_GROUP_FREE (MPI_Group *a) { return MPI_Group_free(a); }
int MPI_GROUP_INCL (MPI_Group a, int b, int *c, MPI_Group *d) { 
					return MPI_Group_incl(a,b,c,d); }
int MPI_GROUP_INTERSECTION (MPI_Group a, MPI_Group b, MPI_Group *c) { 
					return MPI_Group_intersection(a,b,c); }
int MPI_GROUP_RANGE_EXCL (MPI_Group a, int b, int c[][3], MPI_Group *d) { 
					return MPI_Group_range_excl(a,b,c,d); }
int MPI_GROUP_RANGE_INCL (MPI_Group a, int b, int c[][3], MPI_Group *d) { 
					return MPI_Group_range_incl(a,b,c,d); }
int MPI_GROUP_RANK (MPI_Group a, int *b) { return MPI_Group_rank(a,b); }
int MPI_GROUP_SIZE (MPI_Group a, int *b) { return MPI_Group_size(a,b); }
int MPI_GROUP_TRANSLATE_RANKS (MPI_Group a, int b, int *c, MPI_Group d, int *e) { 
				return MPI_Group_translate_ranks(a,b,c,d,e); }
int MPI_GROUP_UNION (MPI_Group a, MPI_Group b, MPI_Group *c) { 
					return MPI_Group_union(a,b,c); }
int MPI_IBSEND (void *a, int b, MPI_Datatype c, int d, int e, 
                MPI_Comm f, MPI_Request *g) { 
					return MPI_Ibsend(a,b,c,d,e,f,g); }
MPI_Fint MPI_INFO_C2F (MPI_Info a) { return MPI_Info_c2f(a); }
int MPI_INFO_CREATE (MPI_Info *a) { return MPI_Info_create(a); }
int MPI_INFO_DELETE (MPI_Info a, char *b) { return MPI_Info_delete(a,b); }
int MPI_INFO_DUP (MPI_Info a, MPI_Info *b) { return MPI_Info_dup(a,b); }
MPI_Info MPI_INFO_F2C (MPI_Fint a) { return MPI_Info_f2c(a); }
int MPI_INFO_FREE (MPI_Info *a) { return MPI_Info_free(a); }
int MPI_INFO_GET (MPI_Info a, char *b, int c, char *d, int *e) { 
					return MPI_Info_get(a,b,c,d,e); }
int MPI_INFO_GET_NKEYS (MPI_Info a, int *b) { 
					return MPI_Info_get_nkeys(a,b); }
int MPI_INFO_GET_NTHKEY (MPI_Info a, int b, char *c) { 
					return MPI_Info_get_nthkey(a,b,c); }
int MPI_INFO_GET_VALUELEN (MPI_Info a, char *b, int *c, int *d) { 
					return MPI_Info_get_valuelen(a,b,c,d); }
int MPI_INFO_SET (MPI_Info a, char *b, char *c) { 
					return MPI_Info_set(a,b,c); }
int MPI_INIT (int *a, char ***b) { return MPI_Init(a, b); }
int MPI_INITIALIZED (int *a) { return MPI_Initialized(a); }
int MPI_INIT_THREAD (int *a, char ***b, int c, int *d) { 
					return MPI_Init_thread(a,b,c,d); }
int MPI_INTERCOMM_CREATE (MPI_Comm a, int b, MPI_Comm c, int d, int e, 
                          MPI_Comm *f) { 
					return MPI_Intercomm_create(a,b,c,d,e,f); }
int MPI_INTERCOMM_MERGE (MPI_Comm a, int b, MPI_Comm *c) { 
					return MPI_Intercomm_merge(a,b,c); }
int MPI_IPROBE (int a, int b, MPI_Comm c, int *d, MPI_Status *e) { 
					return MPI_Iprobe(a,b,c,d,e); }
int MPI_IRECV (void *a, int b, MPI_Datatype c, int d, int e, 
               MPI_Comm f, MPI_Request *g) { 
					return MPI_Irecv(a,b,c,d,e,f,g); }
int MPI_IRSEND (void *a, int b, MPI_Datatype c, int d, int e, 
                MPI_Comm f, MPI_Request *g) { 
					return MPI_Irsend(a,b,c,d,e,f,g); }
int MPI_ISEND (void *a, int b, MPI_Datatype c, int d, int e, 
               MPI_Comm f, MPI_Request *g) { 
					return MPI_Isend(a,b,c,d,e,f,g); }
int MPI_ISSEND (void *a, int b, MPI_Datatype c, int d, int e, 
                MPI_Comm f, MPI_Request *g) { 
					return MPI_Issend(a,b,c,d,e,f,g); }
int MPI_IS_THREAD_MAIN (int *a) { 
					return MPI_Is_thread_main(a); }
int MPI_KEYVAL_CREATE (MPI_Copy_function *a, MPI_Delete_function *b, 
                       int *c, void *d) { 
					return MPI_Keyval_create(a,b,c,d); }
int MPI_KEYVAL_FREE (int *a) { 
					return MPI_Keyval_free(a); }
int MPI_LOOKUP_NAME (char *a, MPI_Info b, char *c) { 
					return MPI_Lookup_name(a,b,c); }
MPI_Fint MPI_OP_C2F (MPI_Op op) { 
					return MPI_Op_c2f(op); } 
int MPI_OP_CREATE (MPI_User_function *a, int b, MPI_Op *c) { 
					return MPI_Op_create(a,b,c); }
int MPI_OPEN_PORT (MPI_Info a, char *b) { 
					return MPI_Open_port(a,b); }
MPI_Op MPI_OP_F2C (MPI_Fint f_handle) { 
					return MPI_Op_f2c(f_handle); }
int MPI_OP_FREE (MPI_Op *a) { 
					return MPI_Op_free(a); }
int MPI_PACK (void *a, int b, MPI_Datatype c, void *d, int e, int *f, 
              MPI_Comm g) { 
					return MPI_Pack(a,b,c,d,e,f,g); }
int MPI_PACK_SIZE (int a, MPI_Datatype b, MPI_Comm c, int *d) { 
					return MPI_Pack_size(a,b,c,d); }
extern int MPI_PCONTROL (int level, ...);
int MPI_PCONTROL (int level, ...) { return MPI_Pcontrol(level); }
int MPI_PROBE (int a, int b, MPI_Comm c, MPI_Status *d) { 
					return MPI_Probe(a,b,c,d); }
int MPI_PUBLISH_NAME (char *a, MPI_Info b, char *c) { 
					return MPI_Publish_name(a,b,c); }
int MPI_PUT (void *a, int b, MPI_Datatype c, int d, MPI_Aint e, int f, 
	     MPI_Datatype g, MPI_Win h) { 
					return MPI_Put(a,b,c,d,e,f,g,h); }
int MPI_QUERY_THREAD (int *a) { 
					return MPI_Query_thread(a); }
int MPI_RECV (void *a, int b, MPI_Datatype c, int d, int e, MPI_Comm f, 
	      MPI_Status *g) { 
					return MPI_Recv(a,b,c,d,e,f,g); }
int MPI_RECV_INIT (void *a, int b, MPI_Datatype c, int d, int e, 
		   MPI_Comm f, MPI_Request *g) { 
					return MPI_Recv_init(a,b,c,d,e,f,g); }
int MPI_REDUCE (void *a, void *b, int c, MPI_Datatype d, MPI_Op e, int f, 
	        MPI_Comm g) { 
					return MPI_Reduce(a,b,c,d,e,f,g); }
int MPI_REDUCE_SCATTER (void *a, void *b, int *c, MPI_Datatype d, 
		        MPI_Op e, MPI_Comm f) { 
					return MPI_Reduce_scatter(a,b,c,d,e,f); }
MPI_Fint MPI_REQUEST_C2F (MPI_Request a) { return MPI_Request_c2f(a); }
MPI_Request MPI_REQUEST_F2C (MPI_Fint a) { return MPI_Request_f2c(a); }
int MPI_REQUEST_FREE (MPI_Request *a) { return MPI_Request_free(a); }
int MPI_RSEND (void *a, int b, MPI_Datatype c, int d, int e, MPI_Comm f) { 
					return MPI_Rsend(a,b,c,d,e,f); }
int MPI_RSEND_INIT (void *a, int b, MPI_Datatype c, int d, int e, 
		    MPI_Comm f, MPI_Request *g) { 
					return MPI_Rsend_init(a,b,c,d,e,f,g); }
int MPI_SCAN (void *a, void *b, int c, MPI_Datatype d, MPI_Op e, MPI_Comm f) { 
					return MPI_Scan(a,b,c,d,e,f); }
int MPI_SCATTER (void *a, int b, MPI_Datatype c, void *d, int e, 
	         MPI_Datatype f, int g, MPI_Comm h) { 
					return MPI_Scatter(a,b,c,d,e,f,g,h); }
int MPI_SCATTERV (void *a, int *b, int *c, MPI_Datatype d, 
		  void *e, int f, MPI_Datatype g, int h, MPI_Comm i) { 
					return MPI_Scatterv(a,b,c,d,e,f,g,h,i); }
int MPI_SEND (void *a, int b, MPI_Datatype c, int d, int e, MPI_Comm f) { 
					return MPI_Send(a,b,c,d,e,f); }
int MPI_SEND_INIT (void *a, int b, MPI_Datatype c, int d, int e, 
		   MPI_Comm f, MPI_Request *g) { 
					return MPI_Send_init(a,b,c,d,e,f,g); }
int MPI_SENDRECV (void *a, int b, MPI_Datatype c, int d, int e, void *f, int g, 
		  MPI_Datatype h, int i, int j, MPI_Comm k, MPI_Status *l) { 
				return MPI_Sendrecv(a,b,c,d,e,f,g,h,i,j,k,l); }
int MPI_SENDRECV_REPLACE (void *a, int b, MPI_Datatype c, int d, int e, int f,
			  int g, MPI_Comm h, MPI_Status *i) { 
				return MPI_Sendrecv_replace(a,b,c,d,e,f,g,h,i); }
int MPI_SSEND (void *a, int b, MPI_Datatype c, int d, int e, MPI_Comm f) { 
					return MPI_Ssend(a,b,c,d,e,f); }
int MPI_SSEND_INIT (void *a, int b, MPI_Datatype c, int d, int e, 
		    MPI_Comm f, MPI_Request *g) { 
					return MPI_Ssend_init(a,b,c,d,e,f,g); }
int MPI_STARTALL (int a, MPI_Request *b) { return MPI_Startall(a,b); }
int MPI_START (MPI_Request *a) { return MPI_Start(a); }
int MPI_STATUS_C2F (MPI_Status *a, MPI_Fint *b) { return MPI_Status_c2f(a,b); }
int MPI_STATUS_F2C (MPI_Fint *a, MPI_Status *b) { return MPI_Status_f2c(a,b); }
int MPI_TESTALL (int a, MPI_Request *b, int *c, MPI_Status *d) { 
					return MPI_Testall(a,b,c,d); }
int MPI_TESTANY (int a, MPI_Request *b, int *c, int *d, MPI_Status *e) { 
					return MPI_Testany(a,b,c,d,e); }
int MPI_TEST (MPI_Request *a, int *b, MPI_Status *c) { 
					return MPI_Test(a,b,c); }
int MPI_TEST_CANCELLED (MPI_Status *a, int *b) { 
					return MPI_Test_cancelled(a,b); }
int MPI_TESTSOME (int a, MPI_Request *b, int *c, int *d, MPI_Status *e) { 
					return MPI_Testsome(a,b,c,d,e); }
int MPI_TOPO_TEST (MPI_Comm a, int *b) { return MPI_Topo_test(a,b); }
MPI_Fint MPI_TYPE_C2F (MPI_Datatype a) { return MPI_Type_c2f(a); }
int MPI_TYPE_COMMIT (MPI_Datatype *a) { return MPI_Type_commit(a); }
int MPI_TYPE_CONTIGUOUS (int a, MPI_Datatype b, MPI_Datatype *c) { 
					return MPI_Type_contiguous(a,b,c); }
int MPI_TYPE_CREATE_DARRAY (int a, int b, int c, int *d, int *e, int *f,
			    int *g, int h, MPI_Datatype i, MPI_Datatype *j) { 
			return MPI_Type_create_darray(a,b,c,d,e,f,g,h,i,j); }
int MPI_TYPE_CREATE_HINDEXED (int a, int *b, MPI_Aint *c, 
			      MPI_Datatype d, MPI_Datatype *e) { 
				return MPI_Type_create_hindexed(a,b,c,d,e); }
int MPI_TYPE_CREATE_HVECTOR (int a, int b, MPI_Aint c, MPI_Datatype d, 
			     MPI_Datatype *e) { 
				return MPI_Type_create_hvector(a,b,c,d,e); }
int MPI_TYPE_CREATE_KEYVAL (MPI_Type_copy_attr_function *a, 
			    MPI_Type_delete_attr_function *b, 
			    int *c, void *d) { 
				return MPI_Type_create_keyval(a,b,c,d); }
int MPI_TYPE_CREATE_RESIZED (MPI_Datatype a, MPI_Aint b, MPI_Aint c, 
			     MPI_Datatype *d) { 
				return MPI_Type_create_resized(a,b,c,d); }
int MPI_TYPE_CREATE_STRUCT (int a, int *b, MPI_Aint *c, 
			    MPI_Datatype *d, MPI_Datatype *e) { 
				return MPI_Type_create_struct(a,b,c,d,e); }
int MPI_TYPE_CREATE_SUBARRAY (int a, int *b, int *c, int *d, int e, 
			      MPI_Datatype f, MPI_Datatype *g) { 
				return MPI_Type_create_subarray(a,b,c,d,e,f,g); }
int MPI_TYPE_DELETE_ATTR (MPI_Datatype a, int b) { 
					return MPI_Type_delete_attr(a,b); }
int MPI_TYPE_DUP (MPI_Datatype a, MPI_Datatype *b) { 
					return MPI_Type_dup(a,b); }
int MPI_TYPE_EXTENT (MPI_Datatype a, MPI_Aint *b) { 
					return MPI_Type_extent(a,b); }
MPI_Datatype MPI_TYPE_F2C (MPI_Fint a) { return MPI_Type_f2c(a); }
int MPI_TYPE_FREE (MPI_Datatype *a) { return MPI_Type_free(a); }
int MPI_TYPE_FREE_KEYVAL (int *a) { return MPI_Type_free_keyval(a); }
int MPI_TYPE_GET_ATTR (MPI_Datatype a, int b, void *c, int *d) { 
					return MPI_Type_get_attr(a,b,c,d); }
int MPI_TYPE_GET_CONTENTS (MPI_Datatype a, int b, int c, int d, int *e, 
			   MPI_Aint *f, MPI_Datatype *g) { 
				return MPI_Type_get_contents(a,b,c,d,e,f,g); }
int MPI_TYPE_GET_ENVELOPE (MPI_Datatype a, int *b, int *c, int *d, int *e) { 
					return MPI_Type_get_envelope(a,b,c,d,e); }
int MPI_TYPE_GET_EXTENT (MPI_Datatype a, MPI_Aint *b, MPI_Aint *c) { 
					return MPI_Type_get_extent(a,b,c); }
int MPI_TYPE_GET_NAME (MPI_Datatype a, char *b, int *c) { 
					return MPI_Type_get_name(a,b,c); }
int MPI_TYPE_GET_TRUE_EXTENT (MPI_Datatype a, MPI_Aint *b, MPI_Aint *c) { 
					return MPI_Type_get_true_extent(a,b,c); }
int MPI_TYPE_HINDEXED (int a, int *b, MPI_Aint *c, 
                       MPI_Datatype d, MPI_Datatype *e) { 
					return MPI_Type_hindexed(a,b,c,d,e); }
int MPI_TYPE_HVECTOR (int a, int b, MPI_Aint c, MPI_Datatype d, 
		      MPI_Datatype *e) { 
					return MPI_Type_hvector(a,b,c,d,e); }
int MPI_TYPE_INDEXED (int a, int *b, int *c, MPI_Datatype d, 
		      MPI_Datatype *e) { 
					return MPI_Type_indexed(a,b,c,d,e); }
int MPI_TYPE_LB (MPI_Datatype a, MPI_Aint *b) { return MPI_Type_lb(a,b); }
int MPI_TYPE_SET_ATTR (MPI_Datatype a, int b, void *c) { 
					return MPI_Type_set_attr(a,b,c); }
int MPI_TYPE_SET_NAME (MPI_Datatype a, char *b) { 
					return MPI_Type_set_name(a,b); }
int MPI_TYPE_SIZE (MPI_Datatype a, int *b) { 
					return MPI_Type_size(a,b); }
int MPI_TYPE_STRUCT (int a, int *b, MPI_Aint *c, MPI_Datatype *d, 
		     MPI_Datatype *e) { 
					return MPI_Type_struct(a,b,c,d,e); }
int MPI_TYPE_UB (MPI_Datatype a, MPI_Aint *b) { 
					return MPI_Type_ub(a,b); }
int MPI_TYPE_VECTOR (int a, int b, int c, MPI_Datatype d, MPI_Datatype *e) { 
					return MPI_Type_vector(a,b,c,d,e); }
int MPI_UNPACK (void *a, int b, int *c, void *d, int e, MPI_Datatype f, 
	        MPI_Comm g) { 
					return MPI_Unpack(a,b,c,d,e,f,g); }
int MPI_UNPUBLISH_NAME (char *a, MPI_Info b, char *c) { 
					return MPI_Unpublish_name(a,b,c); }
int MPI_WAITALL (int a, MPI_Request *b, MPI_Status *c) { 
					return MPI_Waitall(a,b,c); }
int MPI_WAITANY (int a, MPI_Request *b, int *c, MPI_Status *d) { 
					return MPI_Waitany(a,b,c,d); }
int MPI_WAIT (MPI_Request *a, MPI_Status *b) { 
					return MPI_Wait(a,b); }
int MPI_WAITSOME (int a, MPI_Request *b, int *c, int *d, MPI_Status *e) { 
					return MPI_Waitsome(a,b,c,d,e); }

MPI_Fint MPI_WIN_C2F (MPI_Win a) { return MPI_Win_c2f(a); }
int MPI_WIN_CALL_ERRHANDLER (MPI_Win a, int b) { 
					return MPI_Win_call_errhandler(a,b); }
int MPI_WIN_COMPLETE (MPI_Win a) { return MPI_Win_complete(a); }
int MPI_WIN_CREATE (void *a, MPI_Aint b, int c, MPI_Info d, MPI_Comm e, 
		    MPI_Win *f) { return MPI_Win_create(a,b,c,d,e,f); }
int MPI_WIN_CREATE_ERRHANDLER (MPI_Win_errhandler_fn *a, 
			       MPI_Errhandler *b) { 
					return MPI_Win_create_errhandler(a,b); }
int MPI_WIN_CREATE_KEYVAL (MPI_Win_copy_attr_function *a, 
			   MPI_Win_delete_attr_function *b, int *c, void *d) { 
					return MPI_Win_create_keyval(a,b,c,d); }
int MPI_WIN_DELETE_ATTR (MPI_Win a, int b) { return MPI_Win_delete_attr(a,b); }
MPI_Win MPI_WIN_F2C (MPI_Fint a) { return MPI_Win_f2c(a); }
int MPI_WIN_FENCE (int a, MPI_Win b) { return MPI_Win_fence(a,b); }
int MPI_WIN_FREE (MPI_Win *a) { return MPI_Win_free(a); }
int MPI_WIN_FREE_KEYVAL (int *a) { return MPI_Win_free_keyval(a); }
int MPI_WIN_GET_ATTR (MPI_Win a, int b, void *c, int *d) { 
					return MPI_Win_get_attr(a,b,c,d); }
int MPI_WIN_GET_ERRHANDLER (MPI_Win a, MPI_Errhandler *b) { 
					return MPI_Win_get_errhandler(a,b); }
int MPI_WIN_GET_GROUP (MPI_Win a, MPI_Group *b) { return MPI_Win_get_group(a,b); }
int MPI_WIN_GET_NAME (MPI_Win a, char *b, int *c) { 
					return MPI_Win_get_name(a,b,c); }
int MPI_WIN_LOCK (int a, int b, int c, MPI_Win d) { return MPI_Win_lock(a,b,c,d); }
int MPI_WIN_POST (MPI_Group a, int b, MPI_Win c) { return MPI_Win_post(a,b,c); }
int MPI_WIN_SET_ATTR (MPI_Win a, int b, void *c) { 
					return MPI_Win_set_attr(a,b,c); }
int MPI_WIN_SET_ERRHANDLER (MPI_Win a, MPI_Errhandler b) { 
					return MPI_Win_set_errhandler(a,b); }
int MPI_WIN_SET_NAME (MPI_Win a, char *b) { return MPI_Win_set_name(a,b); }
int MPI_WIN_START (MPI_Group a, int b, MPI_Win c) { return MPI_Win_start(a,b,c); }
int MPI_WIN_TEST (MPI_Win a, int *b) { return MPI_Win_test(a,b); }
int MPI_WIN_UNLOCK (int a, MPI_Win b) { return MPI_Win_unlock(a,b); }
int MPI_WIN_WAIT (MPI_Win a) { return MPI_Win_wait(a); }

double MPI_WTICK (void) { return MPI_Wtick(); }
double MPI_WTIME (void) { return MPI_Wtime(); }

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif	/* _MPI_UCASEFIX_H */
