/* *****************************************************************
    MESQUITE -- The Mesh Quality Improvement Toolkit

    Copyright 2004 Sandia Corporation and Argonne National
    Laboratory.  Under the terms of Contract DE-AC04-94AL85000
    with Sandia Corporation, the U.S. Government retains certain
    rights in this software.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    (lgpl.txt) along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    diachin2@llnl.gov, djmelan@sandia.gov, mbrewer@sandia.gov,
    pknupp@sandia.gov, tleurent@mcs.anl.gov, tmunson@mcs.anl.gov,
    kraftche@cae.wisc.edu

  ***************************************************************** */
#include "XYPlanarDomain.hpp"
#include "MsqError.hpp"
#include "MsqVertex.hpp"
#include "DomainUtil.hpp"

#include <algorithm>

MBMesquite::XYPlanarDomain::~XYPlanarDomain() {}

void MBMesquite::XYPlanarDomain::snap_to( MBMesquite::Mesh::VertexHandle /*entity_handle*/, Vector3D& coordinate ) const
{
    coordinate[2] = 0.0;
}

void MBMesquite::XYPlanarDomain::vertex_normal_at( MBMesquite::Mesh::VertexHandle /*entity_handle*/,
                                                   MBMesquite::Vector3D& coordinate ) const
{
    coordinate = Vector3D( 0.0, 0.0, 1.0 );
}

void MBMesquite::XYPlanarDomain::element_normal_at( MBMesquite::Mesh::ElementHandle /*entity_handle*/,
                                                    MBMesquite::Vector3D& coordinate ) const
{
    coordinate = Vector3D( 0.0, 0.0, 1.0 );
}

void MBMesquite::XYPlanarDomain::vertex_normal_at( const MBMesquite::Mesh::VertexHandle*, Vector3D coords[],
                                                   unsigned count, MBMesquite::MsqError& ) const
{
    for( unsigned i = 0; i < count; ++i )
        coords[i] = Vector3D( 0.0, 0.0, 1.0 );
}

void MBMesquite::XYPlanarDomain::closest_point( MBMesquite::Mesh::VertexHandle, const MBMesquite::Vector3D& position,
                                                MBMesquite::Vector3D& closest, MBMesquite::Vector3D& /*normal*/,
                                                MBMesquite::MsqError& ) const
{
    closest = Vector3D( position[0], position[1], 0.0 );
}

void MBMesquite::XYPlanarDomain::domain_DoF( const Mesh::VertexHandle*, unsigned short* dof_array, size_t num_vertices,
                                             MsqError& ) const
{
    std::fill( dof_array, dof_array + num_vertices, 2 );
}
