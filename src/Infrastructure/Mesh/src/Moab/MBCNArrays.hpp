/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#ifndef MBCN_ARRAYS_HPP
#define MBCN_ARRAYS_HPP


namespace moab {

const CN::ConnMap CN::mConnectivityMap[MBMAXTYPE][3] =
{
    // vertex-edge
  {{ 0, 0 , {0}, 
    {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
     MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} },
     // vertex-face
   { 0, 0 , {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                  MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} },
     // vertex-region
   { 0, 0 , {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                  MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} }},

    // edge-edge
  {{ 1, 1, {2}, {MBEDGE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0,1}} },
     // edge-face
   { 1, 0, {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} },
     // edge-region
   { 1, 0, {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} }},

    // tri-edge
  {{ 2, 3, {2,2,2}, {MBEDGE, MBEDGE, MBEDGE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                     MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, { {0,1}, {1,2}, {2,0} } },
     // tri-face
   { 2, 1, {3}, {MBTRI, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0,1,2}} },
     // tri-region
   { 2, 0, {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} }},
   
    // quad-edge
  {{ 2, 4, {2,2,2,2}, {MBEDGE,MBEDGE,MBEDGE,MBEDGE, MBMAXTYPE, MBMAXTYPE, 
                       MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, { {0,1}, {1,2}, {2,3}, {3,0} } },
     // quad-face
   { 2, 1, {4}, {MBQUAD, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0,1,2,3}} },
     // quad-region
   { 2, 0, {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} }},
   
    // polygon-edge
  {{ 2, 0, {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} },
     // polygon-face
   { 2, 1, {0}, {MBPOLYGON, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} },
     // polygon-region
   { 2, 0, {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} }},
   
    // tet-edge
  {{ 3, 6, {2,2,2,2,2,2}, {MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE, 
                           MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0,1}, {1,2}, {2,0}, {0,3}, {1,3}, {2,3}} },
     // tet-face
   { 3, 4, {3,3,3,3}, {MBTRI,MBTRI,MBTRI,MBTRI, MBMAXTYPE, MBMAXTYPE, 
                       MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, { {0,1,3}, {1,2,3}, {0,3,2}, {0,2,1} } },
     // tet-tet
   { 3, 1, {4}, {MBTET, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0,1,2,3}} }},

    // pyramid-edge
  {{ 3, 8, {2,2,2,2,2,2,2,2}, {MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE,
                               MBEDGE,MBEDGE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     {{0,1}, {1,2}, {2,3}, {3,0}, {0,4}, {1,4}, {2,4}, {3,4} } },
     // pyramid-face
   { 3, 5, {3,3,3,3,4}, {MBTRI,MBTRI,MBTRI,MBTRI, MBQUAD, MBMAXTYPE,
                         MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     { {0,1,4},  {1,2,4}, {2,3,4}, {3,0,4}, {0,3,2,1} } },
     // pyramid-pyramid
   { 3, 1, {5}, {MBPYRAMID, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0,1,2,3,4}} }},

    // wedge-edge
  {{ 3, 9, {2,2,2,2,2,2,2,2,2}, {MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE,
                                 MBEDGE,MBEDGE,MBEDGE,MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     { {0,1},{1,2},{2,0},{0,3},{1,4},{2,5},{3,4},{4,5},{5,3} } }, 
     // wedge-face
   { 3, 5 , {4,4,4,3,3}, {MBQUAD,MBQUAD,MBQUAD,MBTRI,MBTRI, MBMAXTYPE, 
                          MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     { {0,1,4,3}, {1,2,5,4}, {0,3,5,2}, {0,2,1}, {3,4,5} } }, 
     // wedge-wedge
   { 3, 1, {6}, {MBPRISM, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     {{0,1,2,3,4,5}} }},

    // knife-edge
  {{ 3, 10, {2,2,2,2,2,2,2,2,2,2}, {MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE,
                                    MBEDGE,MBEDGE,MBEDGE,MBEDGE, MBMAXTYPE, MBMAXTYPE}, 
     { {0,1},{1,2},{2,3},{3,0},{0,4},{1,5},{2,6},{3,5},{4,5},{5,6} } },
     // knife-face
   { 3, 5, {4,4,4,4,4}, {MBQUAD,MBQUAD,MBQUAD,MBQUAD,MBQUAD, MBMAXTYPE, 
                         MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     { {0,1,5,4}, {1,2,6,5}, {2,3,5,6}, {3,0,4,5}, {0,3,2,1} } },
     // knife-knife
   { 3, 1, {7}, {MBKNIFE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     {{0,1,2,3,4,5,6}} }},

    // hex-edge
  {{ 3, 12, {2,2,2,2,2,2,2,2,2,2,2,2}, {MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE,
                                        MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE,MBEDGE}, 
     {{0,1},{1,2},{2,3},{3,0},{0,4},{1,5},{2,6},{3,7},{4,5},{5,6},{6,7},{7,4}}},
     // hex-face
   { 3, 6, {4,4,4,4,4,4}, {MBQUAD,MBQUAD,MBQUAD,MBQUAD,MBQUAD,MBQUAD, 
                           MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     { {0,1,5,4}, {1,2,6,5}, {2,3,7,6}, {3,0,4,7}, {0,3,2,1}, {4,5,6,7} } },
     // hex-hex
   { 3, 1, {8}, {MBHEX, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     {{0,1,2,3,4,5,6,7}} }},

    // polyhedron-edge
  {{ 3, 0, {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     {{0}}},
     // polyhedron-face
   { 3, 0, {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE,
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     { {0}} },
     // polyhedron-polyhedron
   { 3, 1, {0}, {MBPOLYHEDRON, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                 MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, 
     {{0}} }},

    // meshset-edge
  {{ 4, 0 , {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                  MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} },  // not handled yet?
     // meshset-face
   { 4, 0 , {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                  MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} },  // not handled yet?
     // meshset-region
   { 4, 0 , {0}, {MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, 
                  MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE, MBMAXTYPE}, {{0}} }}  // not handled yet?

    // maxtype
};

const CN::UpConnMap CN::mUpConnMap[MBMAXTYPE][4][4] =
{
  { // type MBVERTEX
    {{{1}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 0
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 1
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 2
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 3
  },

  { // type MBEDGE
    { // source dim 0
      { // target dim 0
        {1, 1}, // 1 connected vertices for all vertices
        {{1}, {0}} }, // end target dimension 0
      { // target dimension 1
        {1, 1}, // 1 connected edges for all vertices
        {{0}, {0}} }, // end target dimension 1
      { // target dimension 2
        {0}, // 0 connected faces for all vertices
        {{0}} }, // end target dimension 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    }, // end source dim 0

    { // source dim 1
      { // target dim 0
        {2}, // 2 vertices for all edges
        {{0,1}} }, // end target dim 0
      { // target dim 1
        {0}, // 0 edges for all edges
        {{0}} }, // end target dim 1
      { // target dim 2
        {0}, // 2 faces for all edges
        {{0}} }, // end target dim 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    }, // end source dim 1

    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 2
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 3
  }, // end type MBEDGE

  { // type MBTRI
    { // source dim 0
      { // target dim 0
        {2, 2, 2}, // 2 connected vertices for all vertices
        {{1, 2}, {0, 2}, {0, 1}} }, // end target dimension 0
      { // target dimension 1
        {2, 2, 2}, // 2 connected edges for all vertices
        {{0, 2}, {0, 1}, {1, 2}} }, // end target dimension 1
      { // target dimension 2
        {1, 1, 1}, // 1 connected faces for all vertices
        {{0}, {0}, {0}} }, // end target dimension 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    }, // end source dim 0

    { // source dim 1
      { // target dim 0
        {2, 2, 2}, // 2 vertices for all edges
        {{0,1},{1,2},{0,2}} }, // end target dim 0
      { // target dim 1
        {2, 2, 2}, // 2 edges for all edges
        {{1, 2}, {0, 2}, {0, 1}} }, // end target dim 1
      { // target dim 2
        {1, 1, 1}, // 1 faces for all edges
        {{0}, {0}, {0}} }, // end target dim 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    }, // end source dim 1

    { // source dim 2
      { // target dim 0
        {3}, // 3 vertices for all faces
        {{0, 1, 2}} }, // end target dim 0
      { // target dim 1
        {3}, // 3 edges for all faces
        {{0, 1, 2}} }, // end target dim 1
      { // target dim 2
        {0}, // 0 faces for all faces
        {{0}} }, // end target dim 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    }, // end source dim 2
    
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }} // source dim 3
  }, // end type MBTRI

  { // type MBQUAD
    { // source dim 0
      { // target dim 0
        {2, 2, 2, 2}, // 2 connected vertices for all vertices
        {{1, 3}, {0, 2}, {2, 3}, {0, 2}} }, // end target dimension 0
      { // target dimension 1
        {2, 2, 2, 2}, // 2 connected edges for all vertices
        {{0, 3}, {0, 1}, {1, 2}, {2, 3}} }, // end target dimension 1
      { // target dimension 2
        {1, 1, 1, 1}, // 1 connected faces for all vertices
        {{0}, {0}, {0}, {0}} }, // end target dimension 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    }, // end source dim 0

    { // source dim 1
      { // target dim 0
        {2, 2, 2, 2}, // 2 vertices for all edges
        {{0,1},{1,2},{2,3},{0,3}} }, // end target dim 0
      { // target dim 1
        {2, 2, 2, 2}, // 2 edges for all edges
        {{1, 3}, {0, 2}, {1, 3}, {0, 2}} }, // end target dim 1
      { // target dim 2
        {1, 1, 1, 1}, // 1 faces for all edges
        {{0}, {0}, {0}, {0}} }, // end target dim 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    }, // end source dim 1

    { // source dim 2
      { // target dim 0
        {4}, // 4 vertices for all faces
        {{0, 1, 2, 3}} }, // end target dim 0
      { // target dim 1
        {4}, // 4 edges for all faces
        {{0, 1, 2, 3}} }, // end target dim 1
      { // target dim 2
        {0}, // 4 faces for all faces
        {{0}} }, // end target dim 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    }, // end source dim 2
    
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }} // source dim 3
  }, // end type MBQUAD

  { // type MBPOLYGON
    { // source dim 0
      { // target dim 0
        {0},
        {{0}} },
      { // target dimension 1
        {0},
        {{0}} },
      { // target dimension 2
        {0},
        {{0}} },
      { // target dimension 3
        {0},
        {{0}} }
    }, // end source dim 0

    { // source dim 1
      { // target dim 0
        {0},
        {{0}} },
      { // target dimension 1
        {0},
        {{0}} },
      { // target dimension 2
        {0},
        {{0}} },
      { // target dimension 3
        {0},
        {{0}} }
    }, // end source dim 1

    { // source dim 2
      { // target dim 0
        {0},
        {{0}} },
      { // target dimension 1
        {0},
        {{0}} },
      { // target dimension 2
        {0},
        {{0}} },
      { // target dimension 3
        {0},
        {{0}} }
    }, // end source dim 2

    { // source dim 3
      { // target dim 0
        {0},
        {{0}} },
      { // target dimension 1
        {0},
        {{0}} },
      { // target dimension 2
        {0},
        {{0}} },
      { // target dimension 3
        {0},
        {{0}} }
    } // end source dim 3
  }, // end type MBPOLYGON

  { // type MBTET
    { // source dim 0
      { // target dim 0
        {3, 3, 3, 3}, // 3 connected vertices for all vertices
        {{1, 2, 3}, {0, 2, 3}, {0, 1, 3}, {0, 1, 2}} }, // end target dimension 0
      { // target dimension 1
        {3, 3, 3, 3}, // 3 connected edges for all vertices
        {{0, 2, 3}, {0, 1, 4}, {1, 2, 5}, {3, 4, 5}} }, // end target dimension 1
      { // target dimension 2
        {3, 3, 3, 3}, // 3 connected faces for all vertices
        {{0, 2, 3}, {0, 1, 3}, {1, 2, 3}, {0, 1, 2}} }, // end target dimension 2
      { // target dimension 3
        {1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 0

    { // source dim 1
      { // target dim 0
        {2, 2, 2, 2, 2, 2}, // 2 vertices for all edges
        {{0,1},{1,2},{0,2},{0,3},{1,3},{2,3}} }, // end target dim 0
      { // target dim 1
        {4, 4, 4, 4, 4, 4}, // 4 edges for all edges
        {{1,2,3,4}, {0,2,4,5}, {0,1,3,5}, {0,2,4,5}, {0,1,3,5}, {1,2,3,4}} }, // end target dim 1
      { // target dim 2
        {2, 2, 2, 2, 2, 2}, // 2 faces for all edges
        {{0,3}, {1,3}, {2,3}, {0,2}, {0,1}, {1,2}} }, // end target dim 2
      { // target dimension 3
        {1, 1, 1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 1

    { // source dim 2
      { // target dim 0
        {3, 3, 3, 3}, // 3 vertices for all faces
        {{0,1,3}, {1,2,3}, {0,3,2}, {0,2,1}} }, // end target dim 0
      { // target dim 1
        {3, 3, 3, 3}, // 3 edges for all faces
        {{0,4,3}, {1,5,4}, {3,5,2}, {2,1,0}} }, // end target dim 1
      { // target dim 2
        {3, 3, 3, 3}, // 3 faces for all faces
        {{3,1,2}, {3,2,0}, {0,1,3}, {2,1,0}} }, // end target dim 2
      { // target dimension 3
        {1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 2
    
    { // source dim 3
      { // target dim 0
        {4}, // 4 vertices for all elements
        {{0,1,2,3}} }, // end target dim 0
      { // target dim 1
        {6}, // 6 edges for all elements
        {{0,1,2,3,4,5}} }, // end target dim 1
      { // target dim 2
        {4}, // 4 faces for all elements
        {{0,1,2,3}} }, // end target dim 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    } // end source dim 3
  }, // end type MBTET

  { // type MBPYRAMID
    { // source dim 0
      { // target dim 0
        {3, 3, 3, 3, 4}, // 3 connected vertices for most vertices
        {{1,3,4}, {0,2,4}, {1,3,4}, {0,2,4}, {0,1,2,3}} }, // end target dimension 0
      { // target dimension 1
        {3, 3, 3, 3, 4}, // 3 connected edges for most vertices
        {{0,3,4}, {0,1,5}, {1,2,6}, {2,3,7}, {4,5,6,7}} }, // end target dimension 1
      { // target dimension 2
        {3, 3, 3, 3, 4}, // 3 connected faces for most vertices
        {{0,3,4}, {0,1,4}, {1,2,4}, {2,3,4}, {0,1,2,3}} }, // end target dimension 2
      { // target dimension 3
        {1, 1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 0

    { // source dim 1
      { // target dim 0
        {2, 2, 2, 2, 2, 2, 2, 2}, // 2 vertices for all edges
        {{0,1},{1,2},{2,3},{0,3},{0,4},{1,4},{2,4},{3,4}} }, // end target dim 0
      { // target dim 1
        {4, 4, 4, 4, 5, 5, 5, 5}, // 4 edges for some edges, 5 for others
        {{1,3,4,5}, {0,2,5,6}, {1,3,6,7}, {0,2,4,7}, {0,3,5,6,7}, {0,1,4,6,7}, {1,2,4,5,7}, {2,3,4,5,6}} }, // end target dim 1
      { // target dim 2
        {2, 2, 2, 2, 2, 2, 2, 2}, // 2 faces for all edges
        {{0,4}, {1,4}, {2,4}, {3,4}, {0,3}, {0,1}, {1,2}, {2,3}} }, // end target dim 2
      { // target dimension 3
        {1, 1, 1, 1, 1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 1

    { // source dim 2
      { // target dim 0
        {3, 3, 3, 3, 4}, // 3 vertices for most edges
        {{0,1,4}, {1,2,4}, {2,3,4}, {0,3,4}, {0,1,2,3}} }, // end target dim 0
      { // target dim 1
        {3, 3, 3, 3, 4}, // 3 edges for most faces
        {{0,5,4}, {1,6,5}, {6,2,7}, {3,4,7}, {0,3,2,1}} }, // end target dim 1
      { // target dim 2
        {3, 3, 3, 3, 4}, // 4 faces for most faces
        {{4,1,3}, {4,2,0}, {4,3,1}, {4,0,2}, {0,1,2,3}} }, // end target dim 2
      { // target dimension 3
        {1, 1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 2
    
    { // source dim 3
      { // target dim 0
        {5}, // 5 vertices for all elements
        {{0,1,2,3,4}} }, // end target dim 0
      { // target dim 1
        {8}, // 8 edges for all elements
        {{0,1,2,3,4,5,6,7}} }, // end target dim 1
      { // target dim 2
        {5}, // 5 faces for all elements
        {{0,1,2,3,4}} }, // end target dim 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    } // end source dim 3
  }, // end type MBPYRAMID

  { // type MBPRISM
    { // source dim 0
      { // target dim 0
        {3, 3, 3, 3, 3, 3}, // 3 connected vertices for all vertices
        {{1,2,3}, {0,2,4}, {0,1,5}, {0,4,5}, {1,3,5}, {2,3,4}} }, // end target dimension 0
      { // target dimension 1
        {3, 3, 3, 3, 3, 3}, // 3 connected edges for all vertices
        {{0,2,3}, {0,1,4}, {1,2,5}, {3,6,8}, {4,6,7}, {5,7,8}} }, // end target dimension 1
      { // target dimension 2
        {3, 3, 3, 3, 3, 3}, // 3 connected faces for all vertices
        {{0,2,3}, {0,1,3}, {1,2,3}, {0,2,4}, {0,1,4}, {1,2,4}} }, // end target dimension 2
      { // target dimension 3
        {1, 1, 1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 0

    { // source dim 1
      { // target dim 0
        {2, 2, 2, 2, 2, 2, 2, 2, 2}, // 2 vertices for all edges
        {{0,1},{1,2},{0,2},{0,3},{1,4},{2,5},{3,4},{4,5},{3,5}} }, // end target dim 0
      { // target dim 1
        {4, 4, 4, 4, 4, 4, 4, 4, 4}, // 4 edges for all edges
        {{1,2,3,4}, {0,2,4,5}, {0,1,3,5}, {0,2,6,8}, {0,1,6,7}, 
         {1,2,7,8}, {3,4,7,8}, {4,5,7,8}, {3,5,6,7}} }, // end target dim 1
      { // target dim 2
        {2, 2, 2, 2, 2, 2, 2, 2, 2}, // 2 faces for all edges
        {{0,3}, {1,3}, {2,3}, {0,2}, {0,1}, {1,2}, {0,4}, {1,4}, {2,4}} }, // end target dim 2
      { // target dimension 3
        {1, 1, 1, 1, 1, 1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 1

    { // source dim 2
      { // target dim 0
        {4, 4, 4, 3, 3}, // 4 vertices for some faces, 3 for others
        {{0,1,3,4}, {1,2,4,5}, {0,2,3,5}, {0,1,2}, {3,4,5}} }, // end target dim 0
      { // target dim 1
        {4, 4, 4, 3, 3}, // 4 edges for some faces, 3 for others
        {{0,4,6,3}, {1,5,7,4}, {2,3,8,5}, {0,2,1}, {6,7,8}} }, // end target dim 1
      { // target dim 2
        {4, 4, 4, 3, 3}, // 4 faces for some faces, 3 for others
        {{3,1,4,2}, {3,2,4,0}, {3,0,4,1}, {0,2,1}, {0,1,2}} }, // end target dim 2
      { // target dimension 3
        {1, 1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 2
    
    { // source dim 3
      { // target dim 0
        {6}, // 6 vertices for all elements
        {{0,1,2,3,4,5}} }, // end target dim 0
      { // target dim 1
        {9}, // 9 edges for all elements
        {{0,1,2,3,4,5,6,7,8}} }, // end target dim 1
      { // target dim 2
        {5}, // 5 faces for all elements
        {{0,1,2,3,4}} }, // end target dim 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    } // end source dim 3
  }, // end type MBPRISM

  { // type MBKNIFE
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 0
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 1
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 2
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }} // source dim 3
  }, // end type MBKNIFE

  { // type MBHEX
    { // source dim 0
      { // target dim 0
        {3, 3, 3, 3, 3, 3, 3, 3}, // 3 connected vertices for all vertices
        {{1,3,4}, {0,2,5}, {1,3,6}, {0,2,7}, {0,5,7}, {1,4,6}, {2,5,7}, {3,4,6}} }, // end target dimension 0
      { // target dimension 1
        {3, 3, 3, 3, 3, 3, 3, 3}, // 3 connected edges for all vertices
        {{0, 3, 4}, {0, 1, 5}, {1, 2, 6}, {2, 3, 7}, 
         {4, 8, 11}, {5, 8, 9}, {6, 9, 10}, {7, 10, 11}} }, // end target dimension 1
      { // target dimension 2
        {3, 3, 3, 3, 3, 3, 3, 3}, // 3 connected faces for all vertices
        {{0, 3, 4}, {0, 1, 4}, {1, 2, 4}, {2, 3, 4}, 
         {0, 3, 5}, {0, 1, 5}, {1, 2, 5}, {2, 3, 5}} }, // end target dimension 2
      { // target dimension 3
        {1, 1, 1, 1, 1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 0

    { // source dim 1
      { // target dim 0
        {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, // 2 vertices for all edges
        {{0,1},{1,2},{2,3},{0,3},{0,4},{1,5},{2,6},{3,7},{4,5},{5,6},{6,7},{4,7}} }, // end target dim 0
      { // target dim 1
        {4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, // 4 edges for all edges
        {{1,3,4,5}, {0,2,5,6}, {1,3,6,7}, {0,2,4,7}, {0,3,8,11}, {0,1,8,9}, 
         {1,2,9,10}, {2,3,10,11}, {4,5,9,11}, {5,6,8,10}, {6,7,9,11}, {4,7,8,10}} }, // end target dim 1
      { // target dim 2
        {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, // 2 faces for all edges
        {{0,4}, {1,4}, {2,4}, {3,4}, {0,3}, {0,1}, 
         {1,2}, {2,3}, {0,5}, {1,5}, {2,5}, {3,5}} }, // end target dim 2
      { // target dimension 3
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 1

    { // source dim 2
      { // target dim 0
        {4, 4, 4, 4, 4, 4}, // 4 vertices for all faces
        {{0,1,4,5}, {1,2,5,6}, {2,3,6,7}, {0,3,4,7}, {0,1,2,3}, {4,5,6,7}} }, // end target dim 0
      { // target dim 1
        {4, 4, 4, 4, 4, 4}, // 4 edges for all faces
        {{0,5,8,4}, {1,6,9,5}, {2,7,10,6}, {3,4,11,7}, {0,3,2,1}, {8,9,10,11}} }, // end target dim 1
      { // target dim 2
        {4, 4, 4, 4, 4, 4}, // 4 faces for all faces
        {{4,1,5,3}, {4,2,5,0}, {4,3,5,1}, {4,0,5,2}, {0,3,2,1}, {0,1,2,3}} }, // end target dim 2
      { // target dimension 3
        {1, 1, 1, 1, 1, 1}, // 0 connected elements for all vertices
        {{0}, {0}, {0}, {0}, {0}, {0}} } // end target dimension 3
    }, // end source dim 2
    
    { // source dim 3
      { // target dim 0
        {8}, // 8 vertices for all elements
        {{0,1,2,3,4,5,6,7}} }, // end target dim 0
      { // target dim 1
        {12}, // 12 edges for all elements
        {{0,1,2,3,4,5,6,7,8,9,10,11}} }, // end target dim 1
      { // target dim 2
        {6}, // 6 faces for all elements
        {{0,1,2,3,4,5}} }, // end target dim 2
      { // target dimension 3
        {0}, // 0 connected elements for all vertices
        {{0}} } // end target dimension 3
    } // end source dim 3
  }, // end type MBHEX

  { // type MBPOLYHEDRON
    { // source dim 0
      { // target dim 0
        {0},
        {{0}} },
      { // target dimension 1
        {0},
        {{0}} },
      { // target dimension 2
        {0},
        {{0}} },
      { // target dimension 3
        {0},
        {{0}} }
    }, // end source dim 0

    { // source dim 1
      { // target dim 0
        {0},
        {{0}} },
      { // target dimension 1
        {0},
        {{0}} },
      { // target dimension 2
        {0},
        {{0}} },
      { // target dimension 3
        {0},
        {{0}} }
    }, // end source dim 1

    { // source dim 2
      { // target dim 0
        {0},
        {{0}} },
      { // target dimension 1
        {0},
        {{0}} },
      { // target dimension 2
        {0},
        {{0}} },
      { // target dimension 3
        {0},
        {{0}} }
    }, // end source dim 2

    { // source dim 3
      { // target dim 0
        {0},
        {{0}} },
      { // target dimension 1
        {0},
        {{0}} },
      { // target dimension 2
        {0},
        {{0}} },
      { // target dimension 3
        {0},
        {{0}} }
    } // end source dim 3
  }, // end type MBPOLYHEDRON

  { // type MBENTITYSET
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 0
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 1
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }}, // source dim 2
    {{{0}, {{0}} }, {{0}, {{0}} }, {{0}, {{0}} }} // source dim 3
  } // end type MBENTITYSET
};
const unsigned char E = CN::MID_EDGE_BIT;
const unsigned char F = CN::MID_FACE_BIT;
const unsigned char R = CN::MID_REGION_BIT;
const unsigned char CN::midNodesPerType[MBMAXTYPE][MAX_NODES_PER_ELEMENT+1] = {
// vertex
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
// edge
  { 0, 0, 0, E, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
// tri
  { 0, 0, 0, 0, F, 0, E, E|F, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
// quad
  { 0, 0, 0, 0, 0, F, 0, 0, E, E|F, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
// polygon
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
// tet 1, 2, 3, 4, 5, 6, 7, 8, 9,  10, 11, 12,13, 14,  15
  { 0, 0, 0, 0, 0, R, 0, 0, F, F|R, E, E|R, 0, 0, E|F, E|F|R, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
// pyramid   3, 4, 5, 6, 7, 8, 9,10, 11, 12,13, 14, 15,16,17, 18,  19
  { 0, 0, 0, 0, 0, 0, R, 0, 0, 0, F, F|R, 0, E, E|R, 0, 0, 0, E|F, E|F|R, 0, 0, 0, 0, 0, 0, 0, 0 },
// prism  2, 3, 4, 5, 6, 7, 8, 9,10,11, 12, 13,14,15, 16, 17,18,19, 20,  21
  { 0, 0, 0, 0, 0, 0, 0, R, 0, 0, 0, F, F|R, 0, 0, E, E|R, 0, 0, 0, E|F, E|F|R, 0, 0, 0, 0, 0, 0 },
// knife  2, 3, 4, 5, 6, 7, 8, 9,10,11, 12,13, 14,15,16,17, 18, 19,20,21, 22,  23
  { 0, 0, 0, 0, 0, 0, 0, 0, R, 0, 0, 0, F, F|R, 0, 0, 0, E, E|R, 0, 0, 0, E|F, E|F|R, 0, 0, 0, 0 },
// hex 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,11,12,13,14,15, 16,17,18,19,20, 21, 22,23,24,25, 26,  27
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, R, 0, 0, 0, 0, F, F|R, 0, 0, 0, 0, E, E|R, 0, 0, 0, 0, E|F, E|F|R },
// polyhedron
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
// set
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
  };
  
  
} // namespace moab

#endif
