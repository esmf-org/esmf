/*
 * MGen.cpp
 *
 */

#include "moab/MeshGeneration.hpp"
#include "moab/MergeMesh.hpp"
#include <iostream>
#include <vector>

#ifdef MOAB_HAVE_MPI
#include "moab_mpi.h"
#include "moab/ParallelComm.hpp"
#include "MBParallelConventions.h"
#include "moab/ParallelMergeMesh.hpp"
#endif
#include "moab/ReadUtilIface.hpp"

using std::vector;
using std::endl;
using std::string;

namespace moab {

MeshGeneration::MeshGeneration(Interface *impl, ParallelComm *comm, EntityHandle rset)
    : mb(impl), pc(comm), cset(rset)
  {
    //ErrorCode error;

#ifdef MOAB_HAVE_MPI
    // Get the Parallel Comm instance to prepare all new sets to work in parallel
    // in case the user did not provide any arguments
    if (!comm)
      pc = moab::ParallelComm::get_pcomm(mb, 0);
#endif

  }

MeshGeneration::~MeshGeneration() {
}

ErrorCode MeshGeneration::BrickInstance(MeshGeneration::BrickOpts & opts)
{
  int A = opts.A, B = opts.B, C = opts.C, M = opts.M, N = opts.N, K = opts.K;
  int blockSize = opts.blockSize;
  double xsize = opts.xsize, ysize = opts.ysize, zsize = opts.zsize; // The size of the region
  int GL = opts.GL; // number of ghost layers

  bool newMergeMethod = opts.newMergeMethod;
  bool quadratic =  opts.quadratic;
  bool keep_skins = opts.keep_skins;
  bool tetra =  opts.tetra;
  bool adjEnts = opts.adjEnts;
  bool parmerge = opts.parmerge;

  int rank = 0, size = 1;
  clock_t tt = clock();

  #ifdef MOAB_HAVE_MPI
    rank =pc->rank();
    size = pc->size();
  #endif

  if (M*N*K != size) {
    if (0 == rank)
      std::cout << "M*N*K = " << M*N*K << " != size = " << size << "\n";

    return MB_FAILURE;
  }
  // Determine m, n, k for processor rank
  int m, n, k;
  k = rank / (M*N);
  int leftover = rank % (M*N);
  n = leftover / M;
  m = leftover % M;

  // Used for nodes increments
  int q = (quadratic)? 2 : 1;
  // Used for element increments
  int factor = (tetra)? 6 : 1;

  double dx = xsize / (A*M*blockSize*q); // Distance between 2 nodes in x direction
  double dy = ysize / (B*N*blockSize*q); // Distance between 2 nodes in y direction
  double dz = zsize / (C*K*blockSize*q); // Distance between 2 nodes in z direction

  int NX = (q * M * A * blockSize + 1);
  int NY = (q * N * B * blockSize + 1);
  int nex = M * A * blockSize; // Number of elements in x direction, used for global id on element
  int ney = N * B * blockSize; // Number of elements in y direction ...
  // int NZ = (K * C * blockSize + 1); // Not used
  int blockSize1 = q*blockSize + 1; // Used for vertices
  long num_total_verts = (long) NX * NY * (K * C * blockSize + 1);
  if (0 == rank)
  {
    std::cout << "Generate mesh on " << size << " processors \n";
    std::cout << "Total number of vertices: " << num_total_verts << "\n";
  }
  //int xstride = 1;
  int ystride = blockSize1;

  int zstride = blockSize1 * blockSize1;
  // Generate the block at (a, b, c); it will represent a partition, it will get a partition tag

  ReadUtilIface* iface;
  ErrorCode rval = mb->query_interface(iface);MB_CHK_SET_ERR(rval, "Can't get reader interface");


  Tag global_id_tag;
  rval = mb->tag_get_handle("GLOBAL_ID", 1, MB_TYPE_INTEGER,
                             global_id_tag);MB_CHK_SET_ERR(rval, "Can't get global id tag");

  // set global ids
  Tag new_id_tag;
  if (!parmerge)
  {
    rval = mb->tag_get_handle("HANDLEID", sizeof(long), MB_TYPE_OPAQUE,
      new_id_tag, MB_TAG_CREAT|MB_TAG_DENSE);MB_CHK_SET_ERR(rval, "Can't get handle id tag");
  }
  Tag part_tag;
  int dum_id = -1;
  rval = mb->tag_get_handle("PARALLEL_PARTITION", 1, MB_TYPE_INTEGER,
                             part_tag, MB_TAG_CREAT | MB_TAG_SPARSE, &dum_id);MB_CHK_SET_ERR(rval, "Can't get parallel partition tag");

  Range wsets; // write only part sets
  Range localVerts;
  Range all3dcells;
  for (int a = 0; a < A; a++) {
    for (int b = 0; b < B; b++) {
      for (int c = 0; c < C; c++) {
        // We will generate (q*block + 1)^3 vertices, and block^3 hexas; q is 1 for linear, 2 for quadratic
        // the global id of the vertices will come from m, n, k, a, b, c
        // x will vary from  m*A*q*block + a*q*block to m*A*q*block + (a+1)*q*block etc;
        int num_nodes = blockSize1 * blockSize1 * blockSize1;

        vector<double*> arrays;
        EntityHandle startv;
        rval = iface->get_node_coords(3, num_nodes, 0, startv, arrays);MB_CHK_SET_ERR(rval, "Can't get node coords");

        // Will start with the lower corner:
        int x = m*A*q*blockSize + a*q*blockSize;
        int y = n*B*q*blockSize + b*q*blockSize;
        int z = k*C*q*blockSize + c*q*blockSize;
        int ix = 0;
        vector<int> gids(num_nodes);
        vector<long> lgids(num_nodes);
        Range verts(startv, startv + num_nodes - 1);
        for (int kk = 0; kk < blockSize1; kk++) {
          for (int jj = 0; jj < blockSize1; jj++) {
            for (int ii = 0; ii < blockSize1; ii++) {
              arrays[0][ix] = (x+ii) * dx;
              arrays[1][ix] = (y+jj) * dy;
              arrays[2][ix] = (z+kk) * dz;
              gids[ix] = 1 + (x+ii) + (y+jj) * NX + (z+kk) * (NX*NY);
              if (!parmerge)
                lgids[ix] = 1 + (x+ii) + (y+jj) * NX + (long)(z+kk) * (NX*NY);
              // Set int tags, some nice values?

              ix++;
            }
          }
        }

        rval = mb->tag_set_data(global_id_tag, verts, &gids[0]);MB_CHK_SET_ERR(rval, "Can't set global ids to vertices");
        if (!parmerge)
        {
          rval = mb->tag_set_data(new_id_tag, verts, &lgids[0]);MB_CHK_SET_ERR(rval, "Can't set the new handle id tags");
        }
        localVerts.merge(verts);
        int num_hexas = blockSize * blockSize * blockSize;
        int num_el = num_hexas * factor;

        EntityHandle starte; // Connectivity
        EntityHandle* conn;
        int num_v_per_elem = 8;
        if (quadratic) {
          num_v_per_elem = 27;
          rval = iface->get_element_connect(num_el, 27, MBHEX, 0, starte, conn);MB_CHK_SET_ERR(rval, "Can't get element connectivity");
        }
        else if (tetra) {
          num_v_per_elem = 4;
          rval = iface->get_element_connect(num_el, 4, MBTET, 0, starte, conn);MB_CHK_SET_ERR(rval, "Can't get element connectivity");
        }
        else {
          rval = iface->get_element_connect(num_el, 8, MBHEX, 0, starte, conn);MB_CHK_SET_ERR(rval, "Can't get element connectivity");
        }

        Range cells(starte, starte + num_el - 1); // Should be elements
        // Fill cells
        ix = 0;
        // Identify the elements at the lower corner, for their global ids
        int xe = m*A*blockSize + a*blockSize;
        int ye = n*B*blockSize + b*blockSize;
        int ze = k*C*blockSize + c*blockSize;
        gids.resize(num_el);
        lgids.resize(num_el);
        int ie = 0; // Index now in the elements, for global ids
        for (int kk = 0; kk < blockSize; kk++) {
          for (int jj = 0; jj < blockSize; jj++) {
            for (int ii = 0; ii < blockSize; ii++) {
              EntityHandle corner = startv + q * ii + q * jj * ystride + q * kk * zstride;
              // These could overflow for large numbers
              gids[ie] = 1 + ((xe+ii) + (ye+jj) * nex + (ze+kk) * (nex*ney))*factor ; // 6 more for tetra
              lgids[ie] = 1 + ((xe+ii) + (ye+jj) * nex + (long)(ze+kk) * (nex*ney))*factor ; // 6 more for tetra
              //EntityHandle eh = starte + ie;

              ie++;
              if (quadratic) {
  //                    4   ----- 19   -----  7
  //                .   |                 .   |
  //            16         25         18      |
  //         .          |          .          |
  //      5   ----- 17   -----  6             |
  //      |            12       | 23         15
  //      |                     |             |
  //      |     20      |  26   |     22      |
  //      |                     |             |
  //     13         21  |      14             |
  //      |             0   ----- 11   -----  3
  //      |         .           |         .
  //      |      8         24   |     10
  //      |  .                  |  .
  //      1   -----  9   -----  2
  //
                conn[ix] =    corner;
                conn[ix+1] =  corner + 2;
                conn[ix+2] =  corner + 2 + 2 * ystride;
                conn[ix+3] =  corner +     2 * ystride;
                conn[ix+4] =  corner                   + 2 * zstride;
                conn[ix+5] =  corner + 2               + 2 * zstride;
                conn[ix+6] =  corner + 2 + 2 * ystride + 2 * zstride;
                conn[ix+7] =  corner +     2 * ystride + 2 * zstride;
                conn[ix+8] =  corner + 1;                                           // 0-1
                conn[ix+9] =  corner + 2 +     ystride;                             // 1-2
                conn[ix+10] = corner + 1 + 2 * ystride;                             // 2-3
                conn[ix+11] = corner +         ystride;                             // 3-0
                conn[ix+12] = corner +                       zstride;               // 0-4
                conn[ix+13] = corner + 2 +                   zstride;               // 1-5
                conn[ix+14] = corner + 2 + 2 * ystride +     zstride;               // 2-6
                conn[ix+15] = corner +     2 * ystride +     zstride;               // 3-7
                conn[ix+16] = corner + 1 +               2 * zstride;               // 4-5
                conn[ix+17] = corner + 2 +     ystride + 2 * zstride;               // 5-6
                conn[ix+18] = corner + 1 + 2 * ystride + 2 * zstride;               // 6-7
                conn[ix+19] = corner +         ystride + 2 * zstride;               // 4-7
                conn[ix+20] = corner + 1 +                   zstride;               // 0154
                conn[ix+21] = corner + 2 +     ystride +     zstride;               // 1265
                conn[ix+22] = corner + 1 + 2 * ystride +     zstride;               // 2376
                conn[ix+23] = corner +         ystride +     zstride;               // 0374
                conn[ix+24] = corner + 1 +     ystride;                             // 0123
                conn[ix+25] = corner + 1 +     ystride + 2 * zstride;               // 4567
                conn[ix+26] = corner + 1 +     ystride +     zstride;               // center
                ix += 27;
              }
              else if (tetra) {
                //        E      H
                //     F     G
                //
                //        A     D
                //     B     C
                EntityHandle AA = corner;
                EntityHandle BB = corner + 1;
                EntityHandle CC = corner + 1 + ystride;
                EntityHandle D =  corner +     ystride;
                EntityHandle E =  corner +               zstride;
                EntityHandle F =  corner + 1 +           zstride;
                EntityHandle G =  corner + 1 + ystride + zstride;
                EntityHandle H =  corner +     ystride + zstride;

                // tet EDHG
                conn[ix]    = E;
                conn[ix+1]  = D;
                conn[ix+2]  = H;
                conn[ix+3]  = G;

                // tet ABCF
                conn[ix+4]  = AA;
                conn[ix+5]  = BB;
                conn[ix+6]  = CC;
                conn[ix+7]  = F;

                // tet ADEF
                conn[ix+8]  = AA;
                conn[ix+9]  = D;
                conn[ix+10] = E;
                conn[ix+11] = F;

                // tet CGDF
                conn[ix+12] = CC;
                conn[ix+13] = G;
                conn[ix+14] = D;
                conn[ix+15] = F;

                // tet ACDF
                conn[ix+16] = AA;
                conn[ix+17] = CC;
                conn[ix+18] = D;
                conn[ix+19] = F;

                // tet DGEF
                conn[ix+20] = D;
                conn[ix+21] = G;
                conn[ix+22] = E;
                conn[ix+23] = F;
                ix += 24;
                for (int ff = 0; ff < factor - 1; ff++) {
                  gids[ie] = gids[ie-1] + 1; // 6 more for tetra

                  //eh = starte + ie;

                  ie++;
                }
              }
              else { // Linear hex
                conn[ix] =   corner;
                conn[ix+1] = corner + 1;
                conn[ix+2] = corner + 1 + ystride;
                conn[ix+3] = corner +     ystride;
                conn[ix+4] = corner +               zstride;
                conn[ix+5] = corner + 1 +           zstride;
                conn[ix+6] = corner + 1 + ystride + zstride;
                conn[ix+7] = corner +     ystride + zstride;
                ix += 8;
              }
            }
          }
        }

        EntityHandle part_set;
        rval = mb->create_meshset(MESHSET_SET, part_set);MB_CHK_SET_ERR(rval, "Can't create mesh set");
        rval = mb->add_entities(part_set, cells);MB_CHK_SET_ERR(rval, "Can't add entities to set");
        all3dcells.merge(cells);
        // update adjacencies now, because some elements are new;
        rval = iface->update_adjacencies(starte, num_el, num_v_per_elem, conn);MB_CHK_SET_ERR(rval, "Can't update adjacencies");
        // If needed, add all edges and faces
        if (adjEnts) {
          // Generate all adj entities dimension 1 and 2 (edges and faces/ tri or qua)
          Range edges, faces;
          rval = mb->get_adjacencies(cells, 1, true, edges,
                                     Interface::UNION);MB_CHK_SET_ERR(rval, "Can't get edges");
          rval = mb->get_adjacencies(cells, 2, true, faces,
                                     Interface::UNION);MB_CHK_SET_ERR(rval, "Can't get faces");
          //rval = mb->add_entities(part_set, edges);MB_CHK_SET_ERR(rval, "Can't add edges to partition set");
          //rval = mb->add_entities(part_set, faces);MB_CHK_SET_ERR(rval, "Can't add faces to partition set");
        }

        rval = mb->tag_set_data(global_id_tag, cells, &gids[0]);MB_CHK_SET_ERR(rval, "Can't set global ids to elements");
        if (!parmerge){
          rval = mb->tag_set_data(new_id_tag, cells, &lgids[0]);MB_CHK_SET_ERR(rval, "Can't set new ids to elements");
        }
        int part_num = a + m*A + (b + n*B)*(M*A) + (c + k*C)*(M*A * N*B);
        rval = mb->tag_set_data(part_tag, &part_set, 1, &part_num);MB_CHK_SET_ERR(rval, "Can't set part tag on set");
        wsets.insert(part_set);
      }
    }
  }

  mb->add_entities(cset, all3dcells);
  rval = mb->add_entities(cset, wsets); MB_CHK_SET_ERR(rval, "Can't add entity sets");
#ifdef MOAB_HAVE_MPI
   pc->partition_sets()=wsets;
#endif

  /*
  // Before merge locally
  rval = mb->write_file("test0.h5m", 0, ";;PARALLEL=WRITE_PART");MB_CHK_SET_ERR(rval, "Can't write in parallel, before merging");
  */
  // After the mesh is generated on each proc, merge the vertices
  MergeMesh mm(mb);

  //rval = mb->get_entities_by_dimension(0, 3, all3dcells);MB_CHK_SET_ERR(rval, "Can't get all 3d cells elements");

  if (0 == rank) {
    std::cout << "generate local mesh: "
         << (clock() - tt) / (double)CLOCKS_PER_SEC << " seconds" << endl;
    tt = clock();
    std::cout << "number of elements on rank 0: " << all3dcells.size() << endl;
    std::cout << "Total number of elements " << all3dcells.size()*size << endl;
    std::cout << "Element type: " << ( tetra ? "MBTET" : "MBHEX") << " order:" <<
          (quadratic? "quadratic" : "linear" ) << endl;
  }

  if (A*B*C != 1) { // Merge needed
    if (newMergeMethod) {
      rval = mm.merge_using_integer_tag(localVerts, global_id_tag);MB_CHK_SET_ERR(rval, "Can't merge");
    }
    else {
      rval = mm.merge_entities(all3dcells, 0.0001);MB_CHK_SET_ERR(rval, "Can't merge");
    }

    if (0 == rank) {
       std::cout << "merge locally: "
            << (clock() - tt) / (double)CLOCKS_PER_SEC << " seconds" << endl;
       tt = clock();
    }
  }
  // if adjEnts, add now to each set
  if (adjEnts)
  {
    for (Range::iterator wsit =wsets.begin(); wsit!=wsets.end(); ++wsit)
    {
      EntityHandle ws=*wsit;// write set
      Range cells,edges, faces;
      rval = mb->get_entities_by_dimension(ws, 3, cells);MB_CHK_SET_ERR(rval, "Can't get cells");
      rval = mb->get_adjacencies(cells, 1, false, edges,
                               Interface::UNION);MB_CHK_SET_ERR(rval, "Can't get edges");
      rval = mb->get_adjacencies(cells, 2, false, faces,
                               Interface::UNION);MB_CHK_SET_ERR(rval, "Can't get faces");
      rval = mb->add_entities(ws, edges);MB_CHK_SET_ERR(rval, "Can't add edges to partition set");
      rval = mb->add_entities(ws, faces);MB_CHK_SET_ERR(rval, "Can't add faces to partition set");
    }
  }
#ifdef MOAB_HAVE_MPI
  if (size > 1) {

    //rval = mb->create_meshset(MESHSET_SET, mesh_set);MB_CHK_SET_ERR(rval, "Can't create new set");


    if (parmerge)
    {
      ParallelMergeMesh pm(pc, 0.00001);
      rval = pm.merge();MB_CHK_SET_ERR(rval, "Can't resolve shared ents");
      if (0 == rank) {
         std::cout << "parallel mesh merge: "
              << (clock() - tt) / (double)CLOCKS_PER_SEC << " seconds" << endl;
         tt = clock();
      }
    }
    else
    {
      rval = pc->resolve_shared_ents(cset, -1, -1, &new_id_tag);MB_CHK_SET_ERR(rval, "Can't resolve shared ents");

      if (0 == rank) {
         std::cout << "resolve shared entities: "
              << (clock() - tt) / (double)CLOCKS_PER_SEC << " seconds" << endl;
         tt = clock();
      }
    }
    if (!keep_skins) { // Default is to delete the 1- and 2-dimensional entities
      // Delete all quads and edges
      Range toDelete;
      rval = mb->get_entities_by_dimension(cset, 1, toDelete);MB_CHK_SET_ERR(rval, "Can't get edges");

      rval = mb->get_entities_by_dimension(cset, 2, toDelete);MB_CHK_SET_ERR(rval, "Can't get faces");

      rval = pc->delete_entities(toDelete);MB_CHK_SET_ERR(rval, "Can't delete entities");
      rval = mb->remove_entities(cset, toDelete); MB_CHK_SET_ERR(rval, "Can't remove entities from base set");
      if (0 == rank) {

        std::cout << "delete edges and faces \n";
        toDelete.print(std::cout);

        std::cout << (clock() - tt) / (double)CLOCKS_PER_SEC << " seconds" << endl;
        tt = clock();
      }
    }
    // do some ghosting if required
    if (GL>0)
    {
      rval = pc->exchange_ghost_cells(3, // int ghost_dim
                                         0, // int bridge_dim
                                         GL, // int num_layers
                                         0, // int addl_ents
                                         true);MB_CHK_ERR(rval); // bool store_remote_handles
      if (0 == rank) {
         std::cout << "exchange  " << GL << " ghost layer(s) :"
              << (clock() - tt) / (double)CLOCKS_PER_SEC << " seconds" << endl;
         tt = clock();
      }
    }
  }
#endif

  if (!parmerge)
  {
    rval = mb->tag_delete(new_id_tag); MB_CHK_SET_ERR(rval, "Can't delete new ID tag");
  }

  return MB_SUCCESS;
}

} /* namespace moab */
