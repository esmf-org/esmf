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
#include "PlanarDomain.hpp"
#include "MsqError.hpp"
#include "MsqVertex.hpp"
#include "DomainUtil.hpp"

#include <algorithm>

MBMesquite::PlanarDomain::~PlanarDomain() {}

void MBMesquite::PlanarDomain::set_plane( const MBMesquite::Vector3D& normal, const MBMesquite::Vector3D& point )
{
    mNormal = normal;
    mNormal.normalize();
    mCoeff = -( mNormal % point );
}

void MBMesquite::PlanarDomain::snap_to( MBMesquite::Mesh::VertexHandle /*entity_handle*/, Vector3D& coordinate ) const
{
    coordinate -= mNormal * ( mNormal % coordinate + mCoeff );
}

void MBMesquite::PlanarDomain::vertex_normal_at( MBMesquite::Mesh::VertexHandle /*entity_handle*/,
                                                 MBMesquite::Vector3D& coordinate ) const
{
    coordinate = mNormal;
}

void MBMesquite::PlanarDomain::element_normal_at( MBMesquite::Mesh::ElementHandle /*entity_handle*/,
                                                  MBMesquite::Vector3D& coordinate ) const
{
    coordinate = mNormal;
}

void MBMesquite::PlanarDomain::vertex_normal_at( const MBMesquite::Mesh::VertexHandle*, Vector3D coords[],
                                                 unsigned count, MBMesquite::MsqError& ) const
{
    for( unsigned i = 0; i < count; ++i )
        coords[i] = mNormal;
}

void MBMesquite::PlanarDomain::closest_point( MBMesquite::Mesh::VertexHandle, const MBMesquite::Vector3D& position,
                                              MBMesquite::Vector3D& closest, MBMesquite::Vector3D& normal,
                                              MBMesquite::MsqError& ) const
{
    normal  = mNormal;
    closest = position - mNormal * ( mNormal % position + mCoeff );
}

void MBMesquite::PlanarDomain::domain_DoF( const Mesh::VertexHandle*, unsigned short* dof_array, size_t num_vertices,
                                           MsqError& ) const
{
    std::fill( dof_array, dof_array + num_vertices, 2 );
}

void MBMesquite::PlanarDomain::flip()
{
    mNormal = -mNormal;
    mCoeff  = -mCoeff;
}

void MBMesquite::PlanarDomain::fit_vertices( Mesh* mesh, MsqError& err, double epsilon )
{
    // Our goal here is to consider only the boundary (fixed) vertices
    // when calculating the plane.  If for some reason the user wants
    // to snap a not-quite-planar mesh to a plane during optimization,
    // if possible we want to start with the plane containing the fixed
    // vertices, as those won't get snapped.  If no vertices are fixed,
    // then treat them all as fixed for the purpose calculating the plane
    // (consider them all.)

    std::vector< Mesh::VertexHandle > verts, fixed;
    mesh->get_all_vertices( verts, err );MSQ_ERRRTN( err );
    DomainUtil::get_fixed_vertices( mesh, arrptr( verts ), verts.size(), fixed, err );MSQ_ERRRTN( err );

    bool do_all_verts = true;
    if( fixed.size() > 2 )
    {
        do_all_verts = false;
        fit_vertices( mesh, arrptr( fixed ), fixed.size(), err, epsilon );

        // if we failed with only the fixed vertices, try again with all of the
        // vertices
        if( err )
        {
            err.clear();
            do_all_verts = true;
        }
    }

    if( do_all_verts )
    {
        fit_vertices( mesh, arrptr( verts ), verts.size(), err, epsilon );MSQ_ERRRTN( err );
    }

    // now count inverted elements
    size_t inverted_count = 0, total_count = 0;
    std::vector< Mesh::ElementHandle > elems;
    std::vector< size_t > junk;
    std::vector< MsqVertex > coords;
    mesh->get_all_elements( elems, err );MSQ_ERRRTN( err );
    for( size_t i = 0; i < elems.size(); ++i )
    {

        EntityTopology type;
        mesh->elements_get_topologies( &elems[i], &type, 1, err );
        if( TopologyInfo::dimension( type ) != 2 ) continue;

        verts.clear();
        mesh->elements_get_attached_vertices( arrptr( elems ), 1, verts, junk, err );MSQ_ERRRTN( err );
        if( verts.size() < 3 ) continue;

        coords.resize( verts.size() );
        mesh->vertices_get_coordinates( arrptr( verts ), arrptr( coords ), 3, err );MSQ_ERRRTN( err );
        Vector3D n = ( coords[1] - coords[0] ) * ( coords[2] - coords[0] );
        ++total_count;
        if( n % mNormal < 0.0 ) ++inverted_count;
    }

    // if most elements are inverted, flip normal
    if( 2 * inverted_count > total_count ) this->flip();
}

void MBMesquite::PlanarDomain::fit_vertices( MBMesquite::Mesh* mesh, const MBMesquite::Mesh::VertexHandle* verts,
                                             size_t num_verts, MBMesquite::MsqError& err, double epsilon )
{
    std::vector< MsqVertex > coords( num_verts );
    mesh->vertices_get_coordinates( verts, arrptr( coords ), num_verts, err );MSQ_ERRRTN( err );

    if( epsilon <= 0.0 ) epsilon = DomainUtil::default_tolerance( arrptr( coords ), num_verts );

    Vector3D pts[3];
    if( !DomainUtil::non_colinear_vertices( arrptr( coords ), num_verts, pts, epsilon ) )
    {
        MSQ_SETERR( err )( "All vertices are colinear", MsqError::INVALID_MESH );
        return;
    }

    this->set_plane( ( pts[1] - pts[0] ) * ( pts[2] - pts[0] ), pts[0] );
}
