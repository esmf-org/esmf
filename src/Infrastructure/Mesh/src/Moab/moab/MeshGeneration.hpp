/*
 * MeshGeneration.hpp
 *  class for generating simple meshes online
 */

#ifndef MESHGENERATION_HPP_
#define MESHGENERATION_HPP_
#include "moab/Core.hpp"

#include "moab/CartVect.hpp"

namespace moab {

// Forward Declarations
class ParallelComm;

class MeshGeneration {

public:
  struct BrickOpts
  {
    // follow the options in examples/advanced/GenLargeMesh.cpp
    CartVect ui, uj, uk; // such that ui * uj = uk; uj*uk=ui, etc
    double xsize, ysize, zsize ;  // extents of the brick
    int A, B, C ; // number of blocks per processor
    int M, N, K; // number of processors in each direction
    int blockSize;  // each small block size
    int GL ; // number of ghost layers
    bool newMergeMethod ; // = false;
    bool quadratic ; // = false;
    bool keep_skins ; // = false;
    bool tetra ; // = false;
    bool adjEnts ; // = false;
    bool parmerge ; // = false;
  };

  MeshGeneration(Interface *mbi, ParallelComm * pcomm=0, EntityHandle rset =0);
  virtual ~MeshGeneration();

  ErrorCode BrickInstance(BrickOpts & opts);

private:
  Interface * mb;
  ParallelComm * pc;
  EntityHandle cset;
};

} /* namespace moab */
#endif /* MESHGENERATION_HPP_ */
