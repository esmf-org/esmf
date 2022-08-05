/*
 * Intx2MeshInPlane.hpp
 *
 *  Created on: Oct 24, 2012
 *      Author: iulian
 */

#ifndef INTX2MESHINPLANE_HPP_
#define INTX2MESHINPLANE_HPP_

#include "Intx2Mesh.hpp"
namespace moab
{

class Intx2MeshInPlane : public moab::Intx2Mesh
{
  public:
    Intx2MeshInPlane( Interface* mbimpl );

    virtual ~Intx2MeshInPlane();

    double setup_tgt_cell( EntityHandle tgt, int& nsTgt );

    ErrorCode computeIntersectionBetweenTgtAndSrc( EntityHandle tgt, EntityHandle src, double* P, int& nP, double& area,
                                                   int markb[MAXEDGES], int markr[MAXEDGES], int& nsSrc, int& nsTgt,
                                                   bool check_boxes_first = false );

    ErrorCode findNodes( EntityHandle tgt, int nsTgt, EntityHandle src, int nsSrc, double* iP, int nP );
};

}  // end namespace moab
#endif /* INTX2MESHINPLANE_HPP_ */
