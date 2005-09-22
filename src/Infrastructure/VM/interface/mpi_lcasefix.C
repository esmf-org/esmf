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

#ifndef _MPI_LCASEFIX_H
#define _MPI_LCASEFIX_H

#include <mpi.h>


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

int mpi_dup_fn (MPI_Comm a, int b, void *c, void *d, void *e, int *f) { 
				    return MPI_DUP_FN(a,b,c,d,e,f); }
int mpi_comm_dup_fn (MPI_Comm a, int b, void *c, void *d, void *e, int *f) { 
				    return MPI_COMM_DUP_FN(a,b,c,d,e,f); }
int mpi_type_dup_fn (MPI_Datatype a, int b, void *c, void *d, void *e, int *f) { 
				    return MPI_TYPE_DUP_FN(a,b,c,d,e,f); }
int mpi_win_dup_fn (MPI_Win a, int b, void *c, void *d, void *e, int *f) { 
				    return MPI_WIN_DUP_FN(a,b,c,d,e,f); }

int mpi_abort (MPI_Comm a, int b) { 
                                    return MPI_Abort(a,b); }
int mpi_accumulate (void *a, int b, MPI_Datatype c, int d, MPI_Aint e, 
		    int f, MPI_Datatype g, MPI_Op h, MPI_Win i) {
                                   return MPI_Accumulate(a,b,c,d,e,f,g,h,i); }
int mpi_address (void * a, MPI_Aint *b) { 
				    return MPI_Address(a,b); }
int mpi_allgather (void *a, int b, MPI_Datatype c, void *d, int e, 
		   MPI_Datatype f, MPI_Comm g) { 
                                    return MPI_Allgather(a,b,c,d,e,f,g); }
int mpi_allgatherv (void *a, int b, MPI_Datatype c, void *d, int *e, 
		    int *f, MPI_Datatype g, MPI_Comm h) { 
				    return MPI_Allgatherv(a,b,c,d,e,f,g,h); }
int mpi_alloc_mem (MPI_Aint size, MPI_Info info, void *baseptr) { 
				    return MPI_Alloc_mem(size,info,baseptr); }
int mpi_allreduce (void *a, void *b, int c, MPI_Datatype d, MPI_Op e, 
                  MPI_Comm f) { 
				    return MPI_Allreduce(a,b,c,d,e,f); }
int mpi_alltoall (void *a, int b, MPI_Datatype c, void *d, int e, 
		  MPI_Datatype f, MPI_Comm g) { 
				    return MPI_Alltoall(a,b,c,d,e,f,g); }
int mpi_alltoallv (void *a, int *b, int *c, MPI_Datatype d, void *e, 
		   int *f, int *g, MPI_Datatype h, MPI_Comm i) { 
				    return MPI_Alltoallv(a,b,c,d,e,f,g,h,i); }
int mpi_alltoallw (void *a, int *b, int *c, MPI_Datatype *d, void *e,
		   int *f, int *g, MPI_Datatype *h, MPI_Comm i) { 
				    return MPI_Alltoallw(a,b,c,d,e,f,g,h,i); }
int mpi_attr_delete (MPI_Comm a, int b) { 
				    return MPI_Attr_delete(a,b); }
int mpi_attr_get (MPI_Comm a, int b, void *c, int *d) { 
				    return MPI_Attr_get(a,b,c,d); }
int mpi_attr_put (MPI_Comm a, int b, void *c) { 
				    return MPI_Attr_put(a,b,c); }
int mpi_barrier (MPI_Comm a) { 
				    return MPI_Barrier(a); }
int mpi_bcast (void *a, int b, MPI_Datatype c, int d, MPI_Comm e) { 
				    return MPI_Bcast(a,b,c,d,e); }
int mpi_bsend (void *a, int b, MPI_Datatype c, int d, int e, MPI_Comm f) { 
				    return MPI_Bsend(a,b,c,d,e,f); }
int mpi_bsend_init (void *a, int b, MPI_Datatype c, int d, int e, 
		    MPI_Comm f, MPI_Request *g) { 
				    return MPI_Bsend_init(a,b,c,d,e,f,g); }
int mpi_buffer_attach (void *a, int b) { 
				    return MPI_Buffer_attach(a,b); }
int mpi_buffer_detach (void *a, int *b) { 
				    return MPI_Buffer_detach(a,b); }
int mpi_cancel (MPI_Request *a) { 
				    return MPI_Cancel(a); }
int mpi_cart_coords (MPI_Comm a, int b, int c, int *d) { 
				    return MPI_Cart_coords(a,b,c,d); }
int mpi_cart_create (MPI_Comm a, int b, int *c, int *d, int e, MPI_Comm *f) { 
				    return MPI_Cart_create(a,b,c,d,e,f); }
int mpi_cartdim_get (MPI_Comm a, int *b) { 
				    return MPI_Cartdim_get(a,b); }
int mpi_cart_get (MPI_Comm a, int b, int *c, int *d, int *e) { 
				    return MPI_Cart_get(a,b,c,d,e); }
int mpi_cart_map (MPI_Comm a, int b, int *c, int *d, int *e) { 
				    return MPI_Cart_map(a,b,c,d,e); }
int mpi_cart_rank (MPI_Comm a, int *b, int *c) { 
				    return MPI_Cart_rank(a,b,c); }
int mpi_cart_shift (MPI_Comm a, int b, int c, int *d, int *e) { 
				    return MPI_Cart_shift(a,b,c,d,e); }
int mpi_cart_sub (MPI_Comm a, int *b, MPI_Comm *c) { 
				return MPI_Cart_sub(a,b,c); }
int mpi_close_port (char *a) { 
				return MPI_Close_port(a); }
MPI_Fint mpi_comm_c2f (MPI_Comm a) { 
				return MPI_Comm_c2f(a); }
int mpi_comm_accept (char *a, MPI_Info b, int c, MPI_Comm d, MPI_Comm *e) { 
				return MPI_Comm_accept(a,b,c,d,e); }
int mpi_comm_compare (MPI_Comm a, MPI_Comm b, int *c) { 
				return MPI_Comm_compare(a,b,c); }
int mpi_comm_connect (char *a, MPI_Info b, int c, MPI_Comm d, MPI_Comm *e) { 
				return MPI_Comm_connect(a,b,c,d,e); }
int mpi_comm_create (MPI_Comm a, MPI_Group b, MPI_Comm *c) { 
				return MPI_Comm_create(a,b,c); }
int mpi_comm_create_errhandler (MPI_Comm_errhandler_fn *a, MPI_Errhandler *b) { 
				return MPI_Comm_create_errhandler(a,b); }
int mpi_comm_create_keyval (MPI_Comm_copy_attr_function *a, 
			    MPI_Comm_delete_attr_function *b, int *c, void *d) { 
				return MPI_Comm_create_keyval(a,b,c,d); }
int mpi_comm_delete_attr (MPI_Comm a, int b) { 
				return MPI_Comm_delete_attr(a,b); }
int mpi_comm_disconnect (MPI_Comm *a) { 
				return MPI_Comm_disconnect(a); }
int mpi_comm_dup (MPI_Comm a, MPI_Comm *b) { 
				return MPI_Comm_dup(a,b); }
MPI_Comm mpi_comm_f2c (MPI_Fint a) { 
				return MPI_Comm_f2c(a); }
int mpi_comm_free (MPI_Comm *a) { 
				return MPI_Comm_free(a); }
int mpi_comm_free_keyval (int *a) { 
				return MPI_Comm_free_keyval(a); }
int mpi_comm_get_attr (MPI_Comm a, int b, void *c, int *d) { 
				return MPI_Comm_get_attr(a,b,c,d); }
int mpi_comm_get_errhandler (MPI_Comm a, MPI_Errhandler *b) { 
				return MPI_Comm_get_errhandler(a,b); }
int mpi_comm_get_name (MPI_Comm a, char *b, int *c) { 
				return MPI_Comm_get_name(a,b,c); }
int mpi_comm_get_parent (MPI_Comm *a) { 
				return MPI_Comm_get_parent(a); }
int mpi_comm_group (MPI_Comm a, MPI_Group *b) { 
				return MPI_Comm_group(a,b); }
int mpi_comm_join (int a, MPI_Comm *b) { 
				return MPI_Comm_join(a,b); }
int mpi_comm_rank (MPI_Comm a, int *b) { 
				return MPI_Comm_rank(a,b); }
int mpi_comm_remote_group (MPI_Comm a, MPI_Group *b) { 
				return MPI_Comm_remote_group(a,b); }
int mpi_comm_remote_size (MPI_Comm a, int *b) { 
				return MPI_Comm_remote_size(a,b); }
int mpi_comm_set_attr (MPI_Comm a, int b, void *c) { 
				return MPI_Comm_set_attr(a,b,c); }
int mpi_comm_set_errhandler (MPI_Comm a, MPI_Errhandler b) { 
				return MPI_Comm_set_errhandler(a,b); }
int mpi_comm_set_name (MPI_Comm a, char *b) { 
				return MPI_Comm_set_name(a,b); }
int mpi_comm_size (MPI_Comm a, int *b) { 
				return MPI_Comm_size(a,b); }
int mpi_comm_spawn (char *a, char **b, int c, MPI_Info d, int e, 
		    MPI_Comm f, MPI_Comm *g, int *h) { 
				return MPI_Comm_spawn(a,b,c,d,e,f,g,h); }
int mpi_comm_spawn_multiple (int a, char **b, char ***c, int *d, MPI_Info *e, 
                             int f, MPI_Comm g, MPI_Comm *h, int *i) { 
			return MPI_Comm_spawn_multiple(a,b,c,d,e,f,g,h,i); }
int mpi_comm_split (MPI_Comm a, int b, int c, MPI_Comm *d) { 
				return MPI_Comm_split(a,b,c,d); }
int mpi_comm_test_inter (MPI_Comm a, int *b) { 
				return MPI_Comm_test_inter(a,b); }
int mpi_dims_create (int a, int b, int *c) { 
				return MPI_Dims_create(a,b,c); }
MPI_Fint mpi_errhandler_c2f (MPI_Errhandler err) { 
				return MPI_Errhandler_c2f(err); }
int mpi_errhandler_create (MPI_Handler_function *a, MPI_Errhandler *b) { 
				return MPI_Errhandler_create(a,b); }
MPI_Errhandler mpi_errhandler_f2c (MPI_Fint f_handle) { 
				return MPI_Errhandler_f2c(f_handle); }
int mpi_errhandler_free (MPI_Errhandler *a) { 
				return MPI_Errhandler_free(a); }
int mpi_errhandler_get (MPI_Comm a, MPI_Errhandler *b) { 
				return MPI_Errhandler_get(a,b); }
int mpi_errhandler_set (MPI_Comm a, MPI_Errhandler b) { 
				return MPI_Errhandler_set(a,b); }
int mpi_error_class (int a, int *b) { 
				return MPI_Error_class(a,b); }
int mpi_error_string (int a, char *b, int *c) { 
					return MPI_Error_string(a,b,c); }
int mpi_exscan (void *a, void *b, int c, MPI_Datatype d, MPI_Op e, MPI_Comm f) { 
					return MPI_Exscan(a,b,c,d,e,f); }
int mpi_finalize (void) { 
					return MPI_Finalize(); }
int mpi_finalized (int *flag) { 
					return MPI_Finalized(flag); }
int mpi_free_mem (void *base) { 
					return MPI_Free_mem(base); }
int mpi_gather (void *a, int b, MPI_Datatype c, void *d, int e, 
	        MPI_Datatype f, int g, MPI_Comm h) { 
					return MPI_Gather(a,b,c,d,e,f,g,h); }
int mpi_gatherv (void *a, int b, MPI_Datatype c, void *d, int *e, 
		        int *f, MPI_Datatype g, int h, MPI_Comm i) { 
					return MPI_Gatherv(a,b,c,d,e,f,g,h,i); }
int mpi_get_address (void *a, MPI_Aint *b) { 
					return MPI_Get_address(a,b); }
int mpi_get (void *a, int b, MPI_Datatype c, int d, MPI_Aint e, int f, 
	     MPI_Datatype g, MPI_Win h) { 
					return MPI_Get(a,b,c,d,e,f,g,h); }
int mpi_get_count (MPI_Status *a, MPI_Datatype b, int *c) { 
					return MPI_Get_count(a,b,c); }
int mpi_get_elements (MPI_Status *a, MPI_Datatype b, int *c) { 
					return MPI_Get_elements(a,b,c); }
int mpi_get_processor_name (char *a, int *b) { 
					return MPI_Get_processor_name(a,b); }
int mpi_get_version (int *a, int *b) { return MPI_Get_version(a,b); }
int mpi_graph_create (MPI_Comm a, int b, int *c, int *d, int e, MPI_Comm *f) { 
					return MPI_Graph_create(a,b,c,d,e,f); }
int mpi_graphdims_get (MPI_Comm a, int *b, int *c) { 
					return MPI_Graphdims_get(a,b,c); }
int mpi_graph_get (MPI_Comm a, int b, int c, int *d, int *e) { 
					return MPI_Graph_get(a,b,c,d,e); }
int mpi_graph_map (MPI_Comm a, int b, int *c, int *d, int *e) { 
					return MPI_Graph_map(a,b,c,d,e); }
int mpi_graph_neighbors (MPI_Comm a, int b, int c, int *d) { 
					return MPI_Graph_neighbors(a,b,c,d); }
int mpi_graph_neighbors_count (MPI_Comm a, int b, int *c) { 
					return MPI_Graph_neighbors_count(a,b,c); }
MPI_Fint mpi_group_c2f (MPI_Group a) { return MPI_Group_c2f(a); }
int mpi_group_compare (MPI_Group a, MPI_Group b, int *c) { 
					return MPI_Group_compare(a,b,c); }
int mpi_group_difference (MPI_Group a, MPI_Group b, MPI_Group *c) { 
					return MPI_Group_difference(a,b,c); }
int mpi_group_excl (MPI_Group a, int b, int *c, MPI_Group *d) { 
					return MPI_Group_excl(a,b,c,d); }
MPI_Group mpi_group_f2c (MPI_Fint a) { return MPI_Group_f2c(a); }
int mpi_group_free (MPI_Group *a) { return MPI_Group_free(a); }
int mpi_group_incl (MPI_Group a, int b, int *c, MPI_Group *d) { 
					return MPI_Group_incl(a,b,c,d); }
int mpi_group_intersection (MPI_Group a, MPI_Group b, MPI_Group *c) { 
					return MPI_Group_intersection(a,b,c); }
int mpi_group_range_excl (MPI_Group a, int b, int c[][3], MPI_Group *d) { 
					return MPI_Group_range_excl(a,b,c,d); }
int mpi_group_range_incl (MPI_Group a, int b, int c[][3], MPI_Group *d) { 
					return MPI_Group_range_incl(a,b,c,d); }
int mpi_group_rank (MPI_Group a, int *b) { return MPI_Group_rank(a,b); }
int mpi_group_size (MPI_Group a, int *b) { return MPI_Group_size(a,b); }
int mpi_group_translate_ranks (MPI_Group a, int b, int *c, MPI_Group d, int *e) { 
				return MPI_Group_translate_ranks(a,b,c,d,e); }
int mpi_group_union (MPI_Group a, MPI_Group b, MPI_Group *c) { 
					return MPI_Group_union(a,b,c); }
int mpi_ibsend (void *a, int b, MPI_Datatype c, int d, int e, 
                MPI_Comm f, MPI_Request *g) { 
					return MPI_Ibsend(a,b,c,d,e,f,g); }
MPI_Fint mpi_info_c2f (MPI_Info a) { return MPI_Info_c2f(a); }
int mpi_info_create (MPI_Info *a) { return MPI_Info_create(a); }
int mpi_info_delete (MPI_Info a, char *b) { return MPI_Info_delete(a,b); }
int mpi_info_dup (MPI_Info a, MPI_Info *b) { return MPI_Info_dup(a,b); }
MPI_Info mpi_info_f2c (MPI_Fint a) { return MPI_Info_f2c(a); }
int mpi_info_free (MPI_Info *a) { return MPI_Info_free(a); }
int mpi_info_get (MPI_Info a, char *b, int c, char *d, int *e) { 
					return MPI_Info_get(a,b,c,d,e); }
int mpi_info_get_nkeys (MPI_Info a, int *b) { 
					return MPI_Info_get_nkeys(a,b); }
int mpi_info_get_nthkey (MPI_Info a, int b, char *c) { 
					return MPI_Info_get_nthkey(a,b,c); }
int mpi_info_get_valuelen (MPI_Info a, char *b, int *c, int *d) { 
					return MPI_Info_get_valuelen(a,b,c,d); }
int mpi_info_set (MPI_Info a, char *b, char *c) { 
					return MPI_Info_set(a,b,c); }
int mpi_init (int *a, char ***b) { return MPI_Init(a, b); }
int mpi_initialized (int *a) { return MPI_Initialized(a); }
int mpi_init_thread (int *a, char ***b, int c, int *d) { 
					return MPI_Init_thread(a,b,c,d); }
int mpi_intercomm_create (MPI_Comm a, int b, MPI_Comm c, int d, int e, 
                          MPI_Comm *f) { 
					return MPI_Intercomm_create(a,b,c,d,e,f); }
int mpi_intercomm_merge (MPI_Comm a, int b, MPI_Comm *c) { 
					return MPI_Intercomm_merge(a,b,c); }
int mpi_iprobe (int a, int b, MPI_Comm c, int *d, MPI_Status *e) { 
					return MPI_Iprobe(a,b,c,d,e); }
int mpi_irecv (void *a, int b, MPI_Datatype c, int d, int e, 
               MPI_Comm f, MPI_Request *g) { 
					return MPI_Irecv(a,b,c,d,e,f,g); }
int mpi_irsend (void *a, int b, MPI_Datatype c, int d, int e, 
                MPI_Comm f, MPI_Request *g) { 
					return MPI_Irsend(a,b,c,d,e,f,g); }
int mpi_isend (void *a, int b, MPI_Datatype c, int d, int e, 
               MPI_Comm f, MPI_Request *g) { 
					return MPI_Isend(a,b,c,d,e,f,g); }
int mpi_issend (void *a, int b, MPI_Datatype c, int d, int e, 
                MPI_Comm f, MPI_Request *g) { 
					return MPI_Issend(a,b,c,d,e,f,g); }
int mpi_is_thread_main (int *a) { 
					return MPI_Is_thread_main(a); }
int mpi_keyval_create (MPI_Copy_function *a, MPI_Delete_function *b, 
                       int *c, void *d) { 
					return MPI_Keyval_create(a,b,c,d); }
int mpi_keyval_free (int *a) { 
					return MPI_Keyval_free(a); }
int mpi_lookup_name (char *a, MPI_Info b, char *c) { 
					return MPI_Lookup_name(a,b,c); }
MPI_Fint mpi_op_c2f (MPI_Op op) { 
					return MPI_Op_c2f(op); } 
int mpi_op_create (MPI_User_function *a, int b, MPI_Op *c) { 
					return MPI_Op_create(a,b,c); }
int mpi_open_port (MPI_Info a, char *b) { 
					return MPI_Open_port(a,b); }
MPI_Op mpi_op_f2c (MPI_Fint f_handle) { 
					return MPI_Op_f2c(f_handle); }
int mpi_op_free (MPI_Op *a) { 
					return MPI_Op_free(a); }
int mpi_pack (void *a, int b, MPI_Datatype c, void *d, int e, int *f, 
              MPI_Comm g) { 
					return MPI_Pack(a,b,c,d,e,f,g); }
int mpi_pack_size (int a, MPI_Datatype b, MPI_Comm c, int *d) { 
					return MPI_Pack_size(a,b,c,d); }
extern int mpi_pcontrol (int level, ...);
int mpi_pcontrol (int level, ...) { return MPI_Pcontrol(level); }
int mpi_probe (int a, int b, MPI_Comm c, MPI_Status *d) { 
					return MPI_Probe(a,b,c,d); }
int mpi_publish_name (char *a, MPI_Info b, char *c) { 
					return MPI_Publish_name(a,b,c); }
int mpi_put (void *a, int b, MPI_Datatype c, int d, MPI_Aint e, int f, 
	     MPI_Datatype g, MPI_Win h) { 
					return MPI_Put(a,b,c,d,e,f,g,h); }
int mpi_query_thread (int *a) { 
					return MPI_Query_thread(a); }
int mpi_recv (void *a, int b, MPI_Datatype c, int d, int e, MPI_Comm f, 
	      MPI_Status *g) { 
					return MPI_Recv(a,b,c,d,e,f,g); }
int mpi_recv_init (void *a, int b, MPI_Datatype c, int d, int e, 
		   MPI_Comm f, MPI_Request *g) { 
					return MPI_Recv_init(a,b,c,d,e,f,g); }
int mpi_reduce (void *a, void *b, int c, MPI_Datatype d, MPI_Op e, int f, 
	        MPI_Comm g) { 
					return MPI_Reduce(a,b,c,d,e,f,g); }
int mpi_reduce_scatter (void *a, void *b, int *c, MPI_Datatype d, 
		        MPI_Op e, MPI_Comm f) { 
					return MPI_Reduce_scatter(a,b,c,d,e,f); }
MPI_Fint mpi_request_c2f (MPI_Request a) { return MPI_Request_c2f(a); }
MPI_Request mpi_request_f2c (MPI_Fint a) { return MPI_Request_f2c(a); }
int mpi_request_free (MPI_Request *a) { return MPI_Request_free(a); }
int mpi_rsend (void *a, int b, MPI_Datatype c, int d, int e, MPI_Comm f) { 
					return MPI_Rsend(a,b,c,d,e,f); }
int mpi_rsend_init (void *a, int b, MPI_Datatype c, int d, int e, 
		    MPI_Comm f, MPI_Request *g) { 
					return MPI_Rsend_init(a,b,c,d,e,f,g); }
int mpi_scan (void *a, void *b, int c, MPI_Datatype d, MPI_Op e, MPI_Comm f) { 
					return MPI_Scan(a,b,c,d,e,f); }
int mpi_scatter (void *a, int b, MPI_Datatype c, void *d, int e, 
	         MPI_Datatype f, int g, MPI_Comm h) { 
					return MPI_Scatter(a,b,c,d,e,f,g,h); }
int mpi_scatterv (void *a, int *b, int *c, MPI_Datatype d, 
		  void *e, int f, MPI_Datatype g, int h, MPI_Comm i) { 
					return MPI_Scatterv(a,b,c,d,e,f,g,h,i); }
int mpi_send (void *a, int b, MPI_Datatype c, int d, int e, MPI_Comm f) { 
					return MPI_Send(a,b,c,d,e,f); }
int mpi_send_init (void *a, int b, MPI_Datatype c, int d, int e, 
		   MPI_Comm f, MPI_Request *g) { 
					return MPI_Send_init(a,b,c,d,e,f,g); }
int mpi_sendrecv (void *a, int b, MPI_Datatype c, int d, int e, void *f, int g, 
		  MPI_Datatype h, int i, int j, MPI_Comm k, MPI_Status *l) { 
				return MPI_Sendrecv(a,b,c,d,e,f,g,h,i,j,k,l); }
int mpi_sendrecv_replace (void *a, int b, MPI_Datatype c, int d, int e, int f,
			  int g, MPI_Comm h, MPI_Status *i) { 
				return MPI_Sendrecv_replace(a,b,c,d,e,f,g,h,i); }
int mpi_ssend (void *a, int b, MPI_Datatype c, int d, int e, MPI_Comm f) { 
					return MPI_Ssend(a,b,c,d,e,f); }
int mpi_ssend_init (void *a, int b, MPI_Datatype c, int d, int e, 
		    MPI_Comm f, MPI_Request *g) { 
					return MPI_Ssend_init(a,b,c,d,e,f,g); }
int mpi_startall (int a, MPI_Request *b) { return MPI_Startall(a,b); }
int mpi_start (MPI_Request *a) { return MPI_Start(a); }
int mpi_status_c2f (MPI_Status *a, MPI_Fint *b) { return MPI_Status_c2f(a,b); }
int mpi_status_f2c (MPI_Fint *a, MPI_Status *b) { return MPI_Status_f2c(a,b); }
int mpi_testall (int a, MPI_Request *b, int *c, MPI_Status *d) { 
					return MPI_Testall(a,b,c,d); }
int mpi_testany (int a, MPI_Request *b, int *c, int *d, MPI_Status *e) { 
					return MPI_Testany(a,b,c,d,e); }
int mpi_test (MPI_Request *a, int *b, MPI_Status *c) { 
					return MPI_Test(a,b,c); }
int mpi_test_cancelled (MPI_Status *a, int *b) { 
					return MPI_Test_cancelled(a,b); }
int mpi_testsome (int a, MPI_Request *b, int *c, int *d, MPI_Status *e) { 
					return MPI_Testsome(a,b,c,d,e); }
int mpi_topo_test (MPI_Comm a, int *b) { return MPI_Topo_test(a,b); }
MPI_Fint mpi_type_c2f (MPI_Datatype a) { return MPI_Type_c2f(a); }
int mpi_type_commit (MPI_Datatype *a) { return MPI_Type_commit(a); }
int mpi_type_contiguous (int a, MPI_Datatype b, MPI_Datatype *c) { 
					return MPI_Type_contiguous(a,b,c); }
int mpi_type_create_darray (int a, int b, int c, int *d, int *e, int *f,
			    int *g, int h, MPI_Datatype i, MPI_Datatype *j) { 
			return MPI_Type_create_darray(a,b,c,d,e,f,g,h,i,j); }
int mpi_type_create_hindexed (int a, int *b, MPI_Aint *c, 
			      MPI_Datatype d, MPI_Datatype *e) { 
				return MPI_Type_create_hindexed(a,b,c,d,e); }
int mpi_type_create_hvector (int a, int b, MPI_Aint c, MPI_Datatype d, 
			     MPI_Datatype *e) { 
				return MPI_Type_create_hvector(a,b,c,d,e); }
int mpi_type_create_keyval (MPI_Type_copy_attr_function *a, 
			    MPI_Type_delete_attr_function *b, 
			    int *c, void *d) { 
				return MPI_Type_create_keyval(a,b,c,d); }
int mpi_type_create_resized (MPI_Datatype a, MPI_Aint b, MPI_Aint c, 
			     MPI_Datatype *d) { 
				return MPI_Type_create_resized(a,b,c,d); }
int mpi_type_create_struct (int a, int *b, MPI_Aint *c, 
			    MPI_Datatype *d, MPI_Datatype *e) { 
				return MPI_Type_create_struct(a,b,c,d,e); }
int mpi_type_create_subarray (int a, int *b, int *c, int *d, int e, 
			      MPI_Datatype f, MPI_Datatype *g) { 
				return MPI_Type_create_subarray(a,b,c,d,e,f,g); }
int mpi_type_delete_attr (MPI_Datatype a, int b) { 
					return MPI_Type_delete_attr(a,b); }
int mpi_type_dup (MPI_Datatype a, MPI_Datatype *b) { 
					return MPI_Type_dup(a,b); }
int mpi_type_extent (MPI_Datatype a, MPI_Aint *b) { 
					return MPI_Type_extent(a,b); }
MPI_Datatype mpi_type_f2c (MPI_Fint a) { return MPI_Type_f2c(a); }
int mpi_type_free (MPI_Datatype *a) { return MPI_Type_free(a); }
int mpi_type_free_keyval (int *a) { return MPI_Type_free_keyval(a); }
int mpi_type_get_attr (MPI_Datatype a, int b, void *c, int *d) { 
					return MPI_Type_get_attr(a,b,c,d); }
int mpi_type_get_contents (MPI_Datatype a, int b, int c, int d, int *e, 
			   MPI_Aint *f, MPI_Datatype *g) { 
				return MPI_Type_get_contents(a,b,c,d,e,f,g); }
int mpi_type_get_envelope (MPI_Datatype a, int *b, int *c, int *d, int *e) { 
					return MPI_Type_get_envelope(a,b,c,d,e); }
int mpi_type_get_extent (MPI_Datatype a, MPI_Aint *b, MPI_Aint *c) { 
					return MPI_Type_get_extent(a,b,c); }
int mpi_type_get_name (MPI_Datatype a, char *b, int *c) { 
					return MPI_Type_get_name(a,b,c); }
int mpi_type_get_true_extent (MPI_Datatype a, MPI_Aint *b, MPI_Aint *c) { 
					return MPI_Type_get_true_extent(a,b,c); }
int mpi_type_hindexed (int a, int *b, MPI_Aint *c, 
                       MPI_Datatype d, MPI_Datatype *e) { 
					return MPI_Type_hindexed(a,b,c,d,e); }
int mpi_type_hvector (int a, int b, MPI_Aint c, MPI_Datatype d, 
		      MPI_Datatype *e) { 
					return MPI_Type_hvector(a,b,c,d,e); }
int mpi_type_indexed (int a, int *b, int *c, MPI_Datatype d, 
		      MPI_Datatype *e) { 
					return MPI_Type_indexed(a,b,c,d,e); }
int mpi_type_lb (MPI_Datatype a, MPI_Aint *b) { return MPI_Type_lb(a,b); }
int mpi_type_set_attr (MPI_Datatype a, int b, void *c) { 
					return MPI_Type_set_attr(a,b,c); }
int mpi_type_set_name (MPI_Datatype a, char *b) { 
					return MPI_Type_set_name(a,b); }
int mpi_type_size (MPI_Datatype a, int *b) { 
					return MPI_Type_size(a,b); }
int mpi_type_struct (int a, int *b, MPI_Aint *c, MPI_Datatype *d, 
		     MPI_Datatype *e) { 
					return MPI_Type_struct(a,b,c,d,e); }
int mpi_type_ub (MPI_Datatype a, MPI_Aint *b) { 
					return MPI_Type_ub(a,b); }
int mpi_type_vector (int a, int b, int c, MPI_Datatype d, MPI_Datatype *e) { 
					return MPI_Type_vector(a,b,c,d,e); }
int mpi_unpack (void *a, int b, int *c, void *d, int e, MPI_Datatype f, 
	        MPI_Comm g) { 
					return MPI_Unpack(a,b,c,d,e,f,g); }
int mpi_unpublish_name (char *a, MPI_Info b, char *c) { 
					return MPI_Unpublish_name(a,b,c); }
int mpi_waitall (int a, MPI_Request *b, MPI_Status *c) { 
					return MPI_Waitall(a,b,c); }
int mpi_waitany (int a, MPI_Request *b, int *c, MPI_Status *d) { 
					return MPI_Waitany(a,b,c,d); }
int mpi_wait (MPI_Request *a, MPI_Status *b) { 
					return MPI_Wait(a,b); }
int mpi_waitsome (int a, MPI_Request *b, int *c, int *d, MPI_Status *e) { 
					return MPI_Waitsome(a,b,c,d,e); }

MPI_Fint mpi_win_c2f (MPI_Win a) { return MPI_Win_c2f(a); }
//int mpi_win_call_errhandler (MPI_Win a, int b) { 
//					return MPI_Win_call_errhandler(a,b); }
int mpi_win_complete (MPI_Win a) { return MPI_Win_complete(a); }
int mpi_win_create (void *a, MPI_Aint b, int c, MPI_Info d, MPI_Comm e, 
		    MPI_Win *f) { return MPI_Win_create(a,b,c,d,e,f); }
int mpi_win_create_errhandler (MPI_Win_errhandler_fn *a, 
			       MPI_Errhandler *b) { 
					return MPI_Win_create_errhandler(a,b); }
int mpi_win_create_keyval (MPI_Win_copy_attr_function *a, 
			   MPI_Win_delete_attr_function *b, int *c, void *d) { 
					return MPI_Win_create_keyval(a,b,c,d); }
int mpi_win_delete_attr (MPI_Win a, int b) { return MPI_Win_delete_attr(a,b); }
MPI_Win mpi_win_f2c (MPI_Fint a) { return MPI_Win_f2c(a); }
int mpi_win_fence (int a, MPI_Win b) { return MPI_Win_fence(a,b); }
int mpi_win_free (MPI_Win *a) { return MPI_Win_free(a); }
int mpi_win_free_keyval (int *a) { return MPI_Win_free_keyval(a); }
int mpi_win_get_attr (MPI_Win a, int b, void *c, int *d) { 
					return MPI_Win_get_attr(a,b,c,d); }
int mpi_win_get_errhandler (MPI_Win a, MPI_Errhandler *b) { 
					return MPI_Win_get_errhandler(a,b); }
int mpi_win_get_group (MPI_Win a, MPI_Group *b) { return MPI_Win_get_group(a,b); }
int mpi_win_get_name (MPI_Win a, char *b, int *c) { 
					return MPI_Win_get_name(a,b,c); }
//int mpi_win_lock (int a, int b, int c, MPI_Win d) { return MPI_Win_lock(a,b,c,d); }
int mpi_win_post (MPI_Group a, int b, MPI_Win c) { return MPI_Win_post(a,b,c); }
int mpi_win_set_attr (MPI_Win a, int b, void *c) { 
					return MPI_Win_set_attr(a,b,c); }
int mpi_win_set_errhandler (MPI_Win a, MPI_Errhandler b) { 
					return MPI_Win_set_errhandler(a,b); }
int mpi_win_set_name (MPI_Win a, char *b) { return MPI_Win_set_name(a,b); }
int mpi_win_start (MPI_Group a, int b, MPI_Win c) { return MPI_Win_start(a,b,c); }
//int mpi_win_test (MPI_Win a, int *b) { return MPI_Win_test(a,b); }
//int mpi_win_unlock (int a, MPI_Win b) { return MPI_Win_unlock(a,b); }
int mpi_win_wait (MPI_Win a) { return MPI_Win_wait(a); }

double mpi_wtick (void) { return MPI_Wtick(); }
double mpi_wtime (void) { return MPI_Wtime(); }

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif	/* _MPI_LCASEFIX_H */
