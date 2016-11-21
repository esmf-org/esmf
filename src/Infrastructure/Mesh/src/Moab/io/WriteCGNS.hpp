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


#ifndef WRITE_CGNS_HPP
#define WRITE_CGNS_HPP

#include "moab/Forward.hpp"
#include "moab/WriterIface.hpp"
#include <stdio.h>

//Junior
#include "cgnslib.h"
#include "moab/Range.hpp"

//Junior
#if CGNS_VERSION < 3100
# define cgsize_t int
#else
# if CG_BUILD_SCOPE
#  error enumeration scoping needs to be off
# endif
#endif



namespace moab {

class WriteUtilIface;

/**
 * \brief Export CGNS files.
 * \author Carlos Junqueira Junior
 */

class WriteCGNS : public WriterIface
{
 
public:
  
    //! factory method
  static WriterIface* factory( Interface* );

   //! Constructor
  WriteCGNS(Interface *impl);

   //! Destructor
  virtual ~WriteCGNS();

  // A structure to store Set information.
  class SetStruct {
  public:
    std::string TagName;      // Tag name
    cgsize_t IdSet ;      // Id of the Set
    cgsize_t NbEdges;       // Number of Edges in the Set
    cgsize_t NbFaces;       // Number of Faces in the Set
    cgsize_t NbCells;       // Number of Cells in the Set    
    // vector with the number of entities in the Sets
    // 0-MBEDGE | 1-MBTRI | 2-MBQUAD | 3-MBTET | 4-MBPYRAMID | 5-MBPRISM  | 6-MBHEX
    std::vector<cgsize_t> NbEntities; 
    ElementType_t CGNSType;

    SetStruct(): IdSet(-1), NbEdges(0), NbFaces(0), NbCells(0) {};
    ~SetStruct() {};
  };
  
    //! writes out a file
  ErrorCode write_file(const char *file_name,
                         const bool overwrite,
                         const FileOptions& opts,
                         const EntityHandle *output_list,
                         const int num_sets,
                         const std::vector<std::string>& qa_list,
                         const Tag* tag_list = NULL,
                         int num_tags = 0,
                         int export_dimension = 3);  

  // Get and count vertex entities
  ErrorCode get_vertex_entities ( cgsize_t &VrtSize,
				   std::vector< moab::EntityHandle > &Nodes );

  // Get and count edge entities
  ErrorCode get_edge_entities ( cgsize_t &EdgeSize,
				  std::vector< moab::EntityHandle > &Edges );

  // Get and count face entities
  ErrorCode get_face_entities ( cgsize_t &FaceSize,
				  std::vector< moab::EntityHandle > &Faces );

  // Get and count cell entities
  ErrorCode get_cell_entities ( cgsize_t &CellSize,
				  std::vector< moab::EntityHandle > &Cells );

  // Write coordinates in the cgns file
  ErrorCode write_coord_cgns ( std::vector< moab::EntityHandle > &nodes );

  // Set Tag values on entities
  ErrorCode set_tag_values ( std::vector<moab::Tag> &TagHandles,
			     std::vector< moab::EntityHandle > &Edges,
			     std::vector< moab::EntityHandle > &Faces,
			     std::vector< moab::EntityHandle > &Cells,
			     std::vector<WriteCGNS::SetStruct> &Sets );

  // Get Entities in the set
  ErrorCode get_set_entities ( int i, std::vector<moab::Tag> &TagHandles,
			       std::vector<WriteCGNS::SetStruct> &Sets );

  // Get the CGNSType
  ErrorCode get_cgns_type ( int i, std::vector<WriteCGNS::SetStruct> &Sets );

  // Get the connectivity table
  ErrorCode get_conn_table ( std::vector< moab::EntityHandle > &Elements,
			     std::vector < int > &Begin,
			     std::vector < int > &End,
			     std::vector< moab::Tag > &TagHandles,
			     std::vector< WriteCGNS::SetStruct > &Sets,
			     std::vector < std::vector<cgsize_t> > &ConnTable );

  // Read the Moab type and return CGNS type
  int moab_cgns_conv ( const EntityHandle handle );

private:
                                       
    // interface instance
  Interface *mbImpl;
  WriteUtilIface* mWriteIface;
  
  // File var
  const char *fileName;
  int IndexFile;

  // Base var
  const char *BaseName;
  int IndexBase;
  
  // Zone var
  const char *ZoneName;
  int IndexZone;

  // Section var
  int IndexSection;
  
  // Coordinates var
  int IndexCoord[3];
 
  // Mesh dimension  
  int celldim;
  int physdim;
  cgsize_t isize[3];
  
  // Entities of mesh
  std::vector< moab::EntityHandle > Nodes;
  std::vector< moab::EntityHandle > Edges;
  std::vector< moab::EntityHandle > Faces;
  std::vector< moab::EntityHandle > Cells;

  // Number of entities in the mesh  
  cgsize_t VrtSize ;
  cgsize_t EdgeSize;
  cgsize_t FaceSize;
  cgsize_t CellSize;
  
};

} // namespace moab

#endif
