#include "WriteCGNS.hpp"
#include "moab/CN.hpp"
#include "MBTagConventions.hpp"
#include "MBParallelConventions.h"
#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/WriteUtilIface.hpp"
#include "moab/FileOptions.hpp"
#include "GmshUtil.hpp"

#include <fstream>
#include <map>
#include <set>

#include <iostream>

namespace moab {

WriterIface *WriteCGNS::factory( Interface* iface )
  { return new WriteCGNS( iface ); }

WriteCGNS::WriteCGNS(Interface *impl)
    : mbImpl(impl), fileName(NULL), IndexFile(0), BaseName(NULL), IndexBase(0),
      ZoneName(NULL), IndexZone(0), IndexSection(0), celldim(0), physdim(0),
      VrtSize(0), EdgeSize(0), FaceSize(0), CellSize(0)
{
  impl->query_interface(mWriteIface);
  IndexCoord[0] = IndexCoord[1] = IndexCoord[2] = 0;
  isize[0] = isize[1] = isize[2] = 0;
}

WriteCGNS::~WriteCGNS()
{
  mbImpl->release_interface(mWriteIface);
}

//! writes out a file
ErrorCode WriteCGNS::write_file(const char *file_name,
                                  const bool overwrite,
                                  const FileOptions& /*options*/,
                                  const EntityHandle */*output_list*/,
                                  const int /*num_sets*/,
                                  const std::vector<std::string>&,
                                  const Tag*,
                                  int,
                                  int )
{
  ErrorCode rval;

  if (!overwrite){
    rval = mWriteIface->check_doesnt_exist( file_name );
    if (MB_SUCCESS != rval)
      return rval;
  }
  std::cout << "THE CGNS CONVERSION ONLY WORKS FOR ENTITIES CITED BELOW:";
  std::cout << "\n   -MBVERTEX\n   -MBEDGE\n   -MBTRI\n   -MBQUAD";
  std::cout << "\n   -MBTET\n   -MBPYRAMID\n   -MBHEX\n";

  // Get entities to write
  // Get and count vertex entities
  rval = get_vertex_entities ( VrtSize, Nodes );
  if (rval != MB_SUCCESS){
    return rval;
  }
  // Get and count edge entities
  rval = get_edge_entities( EdgeSize, Edges );
  if (rval != MB_SUCCESS){
    return rval;
  }
  // Get and count face entities
  rval = get_face_entities( FaceSize, Faces );
  if (rval != MB_SUCCESS){
    return rval;
  }
  // Get and count cell entities
  rval = get_cell_entities( CellSize, Cells );
  if (rval != MB_SUCCESS){
    return rval;
  }
  std::cout << "\nThe Number of Vertex is " << VrtSize << ".\n";
  std::cout << "The Number of Edges is " << EdgeSize << ".\n";
  std::cout << "The Number of Faces is " << FaceSize << ".\n";
  std::cout << "The Number of Cells is " << CellSize << ".\n\n";

  // save filename to member variable so we don't need to pass as an argument
  // to called functions
  fileName = file_name;
  std::cout << fileName << " file is a " << physdim << "-D mesh.\n";

  // Open file
  IndexFile = 0;

  // open the cgns file
  // filename:      (input) Name of the CGNS file, including path name if necessary. There is no limit on the 
  //                length of this character variable. 
  // CG_MODE_WRITE: (input) Mode used for opening the file. The modes currently supported are CG_MODE_READ, 
  //                CG_MODE_WRITE, and CG_MODE_MODIFY.
  // filePtr:       (output) CGNS file index number.
  if ( cg_open(fileName, CG_MODE_WRITE, &IndexFile) ){
    std::cout << "Error opening file\n";
    cg_error_exit();
  }
  // Give a base name
  BaseName = "Cgns Base";
  if ( cg_base_write(IndexFile,BaseName,celldim,physdim,&IndexBase) ){
    std::cout << "Error creating CGNS base";
  }
  // Give a zone name
  ZoneName = "Cgns Zone";
  // isize array contains the total vertex size, cell size, and boundary 
  // vertex size for the zone
  // Note that for unstructured zones, the index dimension is always 1
  isize[0] = VrtSize;      // isize[0] contains the total vertex size
  isize[1] = CellSize;     // isize[1] contains the total cell size
  isize[2] = 0;            // isize[2] = 0 for unsorted elements 
  // Create zone */
  // ZoneType_t: Unstructured
  if ( cg_zone_write(IndexFile,IndexBase,ZoneName,isize,Unstructured,&IndexZone) ){
    std::cout << "Error creating CGNS zone\n";
    cg_error_exit();
  }  
  // Write the vertex coordinates
  rval = write_coord_cgns( Nodes );
  if (rval != MB_SUCCESS){
     return rval;
   }

  // Create a vector to hold the Tags
  std::vector<moab::Tag> TagHandles;
  // Get Tags
  rval = mbImpl->tag_get_tags( TagHandles );
  if (rval != MB_SUCCESS){
     return rval;
  }
  // Get the number of Tags in the mesh
  int NbTags = TagHandles.size();
  std:: cout << "\nThe mesh has " << NbTags << " Tags.\n";

  // Create a vector of size NbTags
  // Sets have informations about the entity set
  std::vector<SetStruct> Sets;
  Sets.reserve(NbTags);
  // Fill Sets with all information needed
  rval = set_tag_values( TagHandles, Edges, Faces, Cells, Sets );
  if (rval != MB_SUCCESS){
    std::cout << "Problem to set tag values\n";
    return rval;
  }

  // Create a matrix to hold connectivity
  std::vector < std::vector<cgsize_t> > ConnTable;
  ConnTable.resize(NbTags);

  std::vector< int > Begin ( NbTags , 0 );
  std::vector< int > End ( NbTags , 0 );

  // Take the connectivity of higher dimension entities
  cgsize_t BeginSetsIndex = 1;
  cgsize_t EndSetsIndex;
  switch ( physdim ){
    case 1:
      rval = get_conn_table( Edges, Begin, End, TagHandles, Sets, ConnTable );
      if (rval != MB_SUCCESS){
        std::cout << "Problem to fill the connectivity table for 1-D entities\n";
        return rval;
      }
      for (int i = 0; i < NbTags; ++i) {
        if ( Sets[i].IdSet != -1 ){
          const char * SectionName = Sets[i].TagName.c_str();
          EndSetsIndex = BeginSetsIndex + Sets[i].NbEdges - 1;
          // Write the section in CGNS file
          if ( cg_section_write( IndexFile, IndexBase, IndexBase, SectionName, Sets[i].CGNSType,
                                 BeginSetsIndex, EndSetsIndex, 0, &ConnTable[i][0],&IndexSection) ){
            std::cout << "Issue on writing connectivity - 3-D\n"; 
            cg_error_exit();
          }
          BeginSetsIndex = EndSetsIndex+1;
        }
      }
      break;
    case 2:
      rval = get_conn_table( Edges, Begin, End, TagHandles, Sets, ConnTable );
      if (rval != MB_SUCCESS){
        std::cout << "Problem to fill the connectivity table for 1-D entities\n";
        return rval;
      }
      rval = get_conn_table( Faces, Begin, End, TagHandles, Sets, ConnTable );
      if (rval != MB_SUCCESS){
        std::cout << "Problem to fill the connectivity table for 2-D entities\n";
        return rval;
      }
      for (int i = 0; i < NbTags; ++i) {
        if ( Sets[i].IdSet != -1 ){
            const char * SectionName = Sets[i].TagName.c_str();
            EndSetsIndex = BeginSetsIndex + Sets[i].NbEdges + Sets[i].NbFaces - 1;
            // Write the section in CGNS file
            if ( cg_section_write( IndexFile, IndexBase, IndexBase, SectionName, Sets[i].CGNSType, 
                                   BeginSetsIndex, EndSetsIndex, 0, &ConnTable[i][0],&IndexSection) ){
            std::cout << "Issue on writing connectivity -- 2-D\n"; 
            cg_error_exit();
          }
            BeginSetsIndex = EndSetsIndex+1;
        }
      }
      break;
    case 3:
      rval = get_conn_table( Faces, Begin, End, TagHandles, Sets, ConnTable );
      if (rval != MB_SUCCESS){
        std::cout << "Problem to fill the connectivity table for 2-D entities\n";
        return rval;
      }
      rval = get_conn_table( Cells, Begin, End, TagHandles, Sets, ConnTable );
      if (rval != MB_SUCCESS){
        std::cout << "Problem to fill the connectivity table for 3-D entities\n";
        return rval;
      }
      for (int i = 0; i < NbTags; ++i) {
        if ( Sets[i].IdSet != -1 ){
          const char * SectionName = Sets[i].TagName.c_str();
          EndSetsIndex = BeginSetsIndex + Sets[i].NbFaces + Sets[i].NbCells - 1;
          std::cout << "BeginSetsIndex = " << BeginSetsIndex << "\tEndSetsIndex = " << EndSetsIndex << "\n";
          // Write the section in CGNS file
          if ( cg_section_write( IndexFile, IndexBase, IndexBase, SectionName, Sets[i].CGNSType,
                                 BeginSetsIndex, EndSetsIndex, 0, &ConnTable[i][0],&IndexSection) ){
            std::cout << "Issue on writing connectivity -- 3-D\n"; 
            cg_error_exit();
          }
          BeginSetsIndex = EndSetsIndex+1;
        }
      }
      break;
    default:
      std::cout << "Issue on Physical dimension\n"; 
      return MB_FAILURE;
  }

  // Close the CGNS mesh file
  if ( cg_close(IndexFile) ){
    std::cout << "Error closing file\n";
    cg_error_exit();
  }
  // done
  return MB_SUCCESS;
}

// Get and count vertex entities
ErrorCode WriteCGNS::get_vertex_entities ( cgsize_t &VrtSize_, std::vector< moab::EntityHandle > &Nodes_)
{
   ErrorCode rval;
   // Get vertex entities
   // The first input is 0 because one queries the entire mesh.
   // Retrieves all entities of dimension = 0 in "Nodes"
   rval = mbImpl->get_entities_by_dimension( 0, 0, Nodes_, false );
   if ( Nodes.size() > 0 ){
     celldim=0;
     physdim=0;
     // get the amout of vertex
     VrtSize_ =  Nodes_.size();
   }
   else { std::cout << "The mesh has not node points.\n"; }
   // done
   return rval;
}

// Get and count edge entities
ErrorCode WriteCGNS::get_edge_entities(cgsize_t &EdgeSize_, std::vector< moab::EntityHandle > &Edges_)
{
  ErrorCode rval;
  // The first input is 0 because one queries the entire mesh.
  // Get all entities of dimension = 1 in Edges
  rval = mbImpl->get_entities_by_dimension( 0, 1, Edges_, false );
  if ( Edges_.size() > 0 ){
    celldim=1;
    physdim=1;
    // get the amout of edges
    EdgeSize_ = Edges_.size();
  }
  // done
  return rval;
}

// Get and count face entities
ErrorCode WriteCGNS::get_face_entities(cgsize_t &FaceSize_, std::vector< moab::EntityHandle > &Faces_)
{
  ErrorCode rval;
  // The first input is 0 because one queries the entire mesh.
  // Get all entities of dimension = 2 in Faces
  rval = mbImpl->get_entities_by_dimension( 0, 2, Faces_, false );
  if ( Faces_.size() ){
    celldim=2;
    physdim=2;
    // get the amout of faces
    FaceSize_ = Faces_.size();
  }
  // done
  return rval;
}

// Get and count cell entities
ErrorCode WriteCGNS::get_cell_entities(cgsize_t &CellSize_, std::vector< moab::EntityHandle > &Cells_)
{
  ErrorCode rval;
  // The first input is 0 because one queries the entire mesh.
  // Get all entities of dimension = 3 in Cell
  rval = mbImpl->get_entities_by_dimension( 0, 3, Cells_, false );
  if ( Cells_.size() ){
    celldim=3;
    physdim=3;
    // get the amout of volumes
    CellSize_ = Cells_.size();
  }
  // done
  return rval;
}

ErrorCode WriteCGNS::write_coord_cgns(std::vector< moab::EntityHandle > &Nodes_)
{
  ErrorCode rval;
  
  const int num_entities = (int)Nodes_.size();
  
  // Moab works with one vector for the threee coordinates
  std::vector<double> Coords ( 3*num_entities );
  std::vector<double>::iterator c = Coords.begin();

  // CGNS uses one vector for each coordinate
  std::vector<double> CoordX;
  std::vector<double> CoordY;
  std::vector<double> CoordZ;

  // Summ the values of all coordinates to be sure if it is not zero  
  double SumX=0;
  double SumY=0;
  double SumZ=0;
 
  // Get the moab coordinates - Coords is the output
  rval = mbImpl->get_coords( &Nodes_[0], num_entities, &Coords[0] );
  if (MB_SUCCESS != rval){
    std::cout << "Error getting coordinates from nodes.\n";
    return rval;
  }

  // Reserve the size of nodes
  CoordX.reserve( Nodes_.size() );
  CoordY.reserve( Nodes_.size() );
  CoordZ.reserve( Nodes_.size() );

  for (std::vector<moab::EntityHandle>::iterator i=Nodes_.begin(); i != Nodes_.end(); ++i){
    CoordX.push_back(*c);  // Put the X coordinate in CoordX vector
    SumX += abs(*c);       // Sum all X coordinates
    ++c;                   // Move to Y coordinate
    CoordY.push_back(*c);  // Put the Y coordinate in CoordY vector
    SumY += abs(*c);       // Sum all Y coordinates
    ++c;                   // Move to Z coordinate
    CoordZ.push_back(*c);  // Put the Z coordinate in CoordZ vector
    SumZ += abs(*c);       // Sum all Z coordinates
    ++c;                   // Move to X coordinate
  }       

  // If X coordinate is not empty then write CoordX (user must use SIDS-standard names here)
  if ( SumX != 0 ){
    if ( cg_coord_write(IndexFile,IndexBase,IndexZone,RealDouble,"CoordinateX",&CoordX[0],&IndexCoord[0]) ){
        std::cout << " Error writing X coordinates.\n";
        cg_error_exit();
    }
  }
  // If Y coordinate is not empty then write CoordY (user must use SIDS-standard names here)
  if ( SumY != 0 ){
    if ( cg_coord_write(IndexFile,IndexBase,IndexZone,RealDouble,"CoordinateY",&CoordY[0],&IndexCoord[1]) ){
        std::cout << " Error writing Y coordinates.\n";
        cg_error_exit();
    }
  }
  // If Z coordinate is not empty then write CoordZ (user must use SIDS-standard names here)
  if ( SumZ != 0 ){
    if ( cg_coord_write(IndexFile,IndexBase,IndexZone,RealDouble,"CoordinateZ",&CoordZ[0],&IndexCoord[2]) ){
        std::cout << " Error writing Z coordinates.\n";
        cg_error_exit();
    }
  }

  // Clear vectors
  Coords.clear();
  CoordX.clear();
  CoordY.clear();
  CoordZ.clear();
  
  // done
  return MB_SUCCESS;  
}

ErrorCode WriteCGNS::set_tag_values( std::vector< Tag >& TagHandles,
				     std::vector< moab::EntityHandle > &Edges_,
				     std::vector< moab::EntityHandle > &Faces_,
				     std::vector< moab::EntityHandle > &Cells_,
				     std::vector< WriteCGNS::SetStruct >& Sets )
{
  ErrorCode rval;

  // Get the number of Tags in the mesh
  int NbTags = TagHandles.size();

  // Loop over all Tags
  for (int i = 0; i < NbTags; ++i) {

    // Allocate another position in the vector of SetStruct using a default constructor
    Sets.push_back( SetStruct() );

    // Get the Tag name
    rval = mbImpl->tag_get_name( TagHandles[i], Sets[i].TagName );
    if (rval != MB_SUCCESS){
      std::cout << "Problem to get Tag Name\n";
      return rval;
    }
    std::cout << "Tag name= " << Sets[i].TagName << "\n";

    // Count all entities by type and put in Sets[i].NbEntities vector
    rval = get_set_entities( i, TagHandles, Sets);
    if (rval != MB_SUCCESS ){
     std::cout << "Problem to get Set entities\n";
     return rval;
    }

    // Get the CGNSTYpe of the Set
    rval = get_cgns_type ( i, Sets );
    if (rval != MB_SUCCESS ){
     std::cout << "Problem to get CGNSType\n";
     return rval;
    }
    std::cout << "\tSets[" << i << "].CGNSType= " << Sets[i].CGNSType << "\n";

    //int Number;

    // Set a data index for Edges and TagHandles[i]
    if ( Sets[i].CGNSType == BAR_2 || Sets[i].CGNSType == TRI_3 || Sets[i].CGNSType == QUAD_4 ||
         Sets[i].CGNSType == TETRA_4 || Sets[i].CGNSType == PYRA_5 || Sets[i].CGNSType == PENTA_6 ||
         Sets[i].CGNSType == HEXA_8 || Sets[i].CGNSType == MIXED ){

      if ( Sets[i].NbEdges > 0 && physdim < 3 ){
        // Set a data index for Edges and TagHandles[i]
        const std::vector< int > tag_values ( Edges_.size(), i );
        rval = mbImpl-> tag_set_data ( TagHandles[i], &Edges_[0], Edges_.size(), &tag_values[0] );
        if (rval != MB_SUCCESS ){
          std::cout << "Problem to set data for 1-D entities\n";
          return rval;
        }
      }
      if ( Sets[i].NbFaces > 0 && physdim > 1 ){
        // Set a data index for Faces and TagHandles[i]
        const std::vector< int > tag_values ( Faces.size(), i );
        rval = mbImpl-> tag_set_data ( TagHandles[i], &Faces_[0], Faces_.size(), &tag_values[0] );
        if (rval != MB_SUCCESS ){
         std::cout << "Problem to set data for 2-D entities\n";
         return rval;
        }
      }
      if ( Sets[i].NbCells > 0 && physdim > 2 ){
        // Set a data index for Cells and TagHandles[i]
        const std::vector< int > tag_values ( Cells.size(), i );
        rval = mbImpl-> tag_set_data ( TagHandles[i], &Cells_[0], Cells_.size(), &tag_values[0] );
        if (rval != MB_SUCCESS ){
         std::cout << "Problem to set data for 3-D entities\n";
         return rval;
        }
      }
      // IdSet gets the Set Index indicating that it is not empty
      Sets[i].IdSet = i;
    }
    std::cout << "\tSets[" << i << "].IdSet = " << Sets[i].IdSet << "\n\n";
  }
  return MB_SUCCESS;
}

// Get Entities in the set
ErrorCode WriteCGNS::get_set_entities(int i, std::vector< Tag >& TagHandles, 
				      std::vector< WriteCGNS::SetStruct >& Sets)
{
  ErrorCode rval;
  
  // Get the number of MBEDGE entities
  // NbEntities[0] holds the number of MBEDGE in the "Sets"
  int Number=0;
  rval = mbImpl->get_number_entities_by_type_and_tag( 0, MBEDGE, &TagHandles[i], 0, 1, Number );
  if (rval != MB_SUCCESS ){
    std::cout << "Problem to get the number of entities by type and tag\n";
    return rval;
  }
  Sets[i].NbEntities.push_back(Number);  // MBEDGE == Sets[i].NbEntities[0]
  Sets[i].NbEdges += Number;
  std::cout << "\tNumber of MBEDGE = " << Number << "\n";

  // Get the number of MBTRI entities
  // NbEntities[1] holds the number of MBTRI in the "Sets"
  Number=0;
  rval = mbImpl->get_number_entities_by_type_and_tag( 0, MBTRI, &TagHandles[i], 0, 1, Number );
  if (rval != MB_SUCCESS ){
    std::cout << "Problem to get the number of entities by type and tag\n";
    return rval;
  }
  Sets[i].NbEntities.push_back(Number);  // MBTRI == Sets[i].NbEntities[1]
  Sets[i].NbFaces += Number;
  std::cout << "\tNumber of MBTRI = " << Number << "\n";

  // Get the number of MBQUAD entities
  // NbEntities[2] holds the number of MBQUAD in the "Sets"
  Number=0;
  rval = mbImpl->get_number_entities_by_type_and_tag( 0, MBQUAD, &TagHandles[i], 0, 1, Number );
  if (rval != MB_SUCCESS ){
    std::cout << "Problem to get the number of entities by type and tag\n";
    return rval;
  }
  Sets[i].NbEntities.push_back(Number);  // MBQUAD == Sets[i].NbEntities[2]
  Sets[i].NbFaces += Number;
  std::cout << "\tNumber of MBQUAD = " << Number << "\n";

  // Get the number of MBTET entities
  // NbEntities[3] holds the number of MBTET in the "Sets"
  Number=0;
  rval = mbImpl->get_number_entities_by_type_and_tag( 0, MBTET, &TagHandles[i], 0, 1, Number );
  if (rval != MB_SUCCESS ){
    std::cout << "Problem to get the number of entities by type and tag\n";
    return rval;
  }
  Sets[i].NbEntities.push_back(Number);  // MBTET == Sets[i].NbEntities[3]
  Sets[i].NbCells += Number;
  std::cout << "\tNumber of MBTET = " << Number << "\n";

  // Get the number of MBPYRAMID entities
  // NbEntities[4] holds the number of MBPYRAMID in the "Sets"
  Number=0;
  rval = mbImpl->get_number_entities_by_type_and_tag( 0, MBPYRAMID, &TagHandles[i], 0, 1, Number );
  if (rval != MB_SUCCESS ){
    std::cout << "Problem to get the number of entities by type and tag\n";
    return rval;
  }
  Sets[i].NbEntities.push_back(Number);  // MBPYRAMID == Sets[i].NbEntities[4]
  Sets[i].NbCells += Number;
  std::cout << "\tNumber of MBPYRAMID = " << Number << "\n";

  // Get the number of MBPRISM entities - MBPRISM == PENTA_6
  // NbEntities[5] holds the number of MBPRISM in the "Sets"
  Number=0;
  rval = mbImpl->get_number_entities_by_type_and_tag( 0, MBPRISM, &TagHandles[i], 0, 1, Number );
  if (rval != MB_SUCCESS ){
    std::cout << "Problem to get the number of entities by type and tag\n";
    return rval;
  }
  Sets[i].NbEntities.push_back(Number);  // MBPRISM == Sets[i].NbEntities[5]
  Sets[i].NbCells += Number;
  std::cout << "\tNumber of MBPRISM = " << Number << "\n";

  // Get the number of MBHEX entities
  // NbEntities[6] holds the number of MBHEX in the "Sets"
  Number=0;
  rval = mbImpl->get_number_entities_by_type_and_tag( 0, MBHEX, &TagHandles[i], 0, 1, Number );
  if (rval != MB_SUCCESS ){
    std::cout << "Problem to get the number of entities by type and tag\n";
    return rval;
  }
  Sets[i].NbEntities.push_back(Number);  // MBHEX == Sets[i].NbEntities[6]
  Sets[i].NbCells += Number;
  std::cout << "\tNumber of MBHEX = " << Number << "\n";
  
  std::cout << "\tTotal number of Edges = " << Sets[i].NbEdges << "\n";
  std::cout << "\tTotal number of Faces = " << Sets[i].NbFaces << "\n";
  std::cout << "\tTotal number of Cells = " << Sets[i].NbCells << "\n";
  
  return MB_SUCCESS;
}

// Get CGNSType
ErrorCode WriteCGNS::get_cgns_type ( int i, std::vector<WriteCGNS::SetStruct> &Sets )
{
  std::vector<int> Test;
  int Sum=0;

  // NbEntities is a vector which has the number of entities of each type
  // 0-MBEDGE | 1-MBTRI | 2-MBQUAD | 3-MBTET | 4-MBPYRAMID | 5-MBPRISM | 6-MBHEX
  // if NbEntities[i]>0 then Test[i]=1
  // else then Test[i]=0
  for ( int j=0; j< (int)Sets[i].NbEntities.size(); ++j){
    if ( Sets[i].NbEntities[j] > 0 ){ 
      Test.push_back(1);
      Sum++;
    }
    else { Test.push_back(0); }
  }

  // Test the Sum
  // if Sum > 1 then the Set is MIXED
  // if Sum = 0 then the Set is Homogeneous
  // else then the Set is empty
  if ( Sum>1 ){ Sets[i].CGNSType = MIXED; }
  else if ( Sum==1 ){ 
    int j=0;
    std::cout << "Homogeneous Type\n";
    while (j < (int)Sets[i].NbEntities.size() && Sets[i].NbEntities[j] != 1){ ++j; }
    switch ( j ){
      case 0 :
        Sets[i].CGNSType = BAR_2;
        break;
      case 1 :
        Sets[i].CGNSType = TRI_3;
        break;
      case 2 :
        Sets[i].CGNSType = QUAD_4;
        break;
      case 3 :
        Sets[i].CGNSType = TETRA_4;
        break;
      case 4 :
        Sets[i].CGNSType = PYRA_5;
        break;
      case 5 :
        Sets[i].CGNSType = PENTA_6;
        break;
      case 6 :
        Sets[i].CGNSType = HEXA_8;
        break;
      default :
        std::cout << "It was not possible to identify the CGNSType\n";
        return MB_FAILURE;
    }
  }
  else { Sets[i].CGNSType = ElementTypeNull; } // NOT SURE IF THAT'S THE RIGHT WAY....... 

  // Clear the test vector
  Test.clear();

  return MB_SUCCESS;  
  
}

// Fill the connectivity table
ErrorCode WriteCGNS::get_conn_table( std::vector< moab::EntityHandle > &Elements,
				     std::vector< int > &Begin,
				     std::vector< int > &End,
				     std::vector<moab::Tag> &TagHandles,
				     std::vector<WriteCGNS::SetStruct> &Sets,
				     std::vector < std::vector<cgsize_t> > &ConnTable )
{
  ErrorCode rval;
  
//   int Begin = 0; // GOT TO WORK ON THIS
//   int End;

  // Get the number of Tags in the mesh
  int NbTags = TagHandles.size();

  // Test all Elements, get their ids and connectivity 
  // to fill ConnTable
  for (std::vector<moab::EntityHandle>::iterator i=Elements.begin(); i != Elements.end(); ++i){
    int id;
    // Test all Tags
    for ( int j = 0; j < NbTags; ++j ){
      // Test if the Tag has data
      if ( Sets[j].IdSet != -1 ){
        // Try to get data from entity
        rval = mbImpl->tag_get_data( TagHandles[j], &*i, 1, &id );
        if (MB_SUCCESS != rval){
          return rval;
        }
        // If successful id==j
        if ( id == j ){
          // Get the entity type of the EntityHandle wich points to Cells
          int num_vtx;                 // Number of MeshVertices in array connectivity.
          const EntityHandle* conn;    // Array in which connectivity of entity_handle is returned.
          // Gets a pointer to constant connectivity data of entity_handle
          rval = mbImpl->get_connectivity( *i, conn, num_vtx );
          if (MB_SUCCESS != rval){
            return rval;
          }
          // If the Set is MIXED type
          // push CGNS ENUM type of the entity
          // before the connectivity
          if ( Sets[j].CGNSType == MIXED ){
            ConnTable[j].push_back( moab_cgns_conv(*i) );   // moab_cgns_conv return an int which 
                                                            // represents the CGNS type
            Begin[j]++;
          }
          End[j] = Begin[j] + num_vtx;
          // Push conn in ConnTable in which "j" is the Set Index
          for (int k=Begin[j]; k<End[j]; ++k){
            ConnTable[j].push_back( (cgsize_t)conn[k-Begin[j]] );
          }
          Begin[j] =  End[j];
        }
      }
    }
  }
  return MB_SUCCESS;
}

// Read the Moab type and return CGNS type
int WriteCGNS::moab_cgns_conv(const EntityHandle handle)
{
  EntityType MoabType = mbImpl->type_from_handle( handle );
  switch ( MoabType ){
    case MBEDGE :         /**< Mesh Edge */
      return BAR_2;
    case MBTRI :          /**< Triangular element (including shells) */
      return TRI_3;
    case MBQUAD :         /**< Quadrilateral element (including shells) */
      return QUAD_4;
    case MBTET :          /**< Tetrahedral element */
      return TETRA_4;
    case MBPYRAMID :      /**< Pyramid element */
      return PYRA_5;
    case MBPRISM :        /**< Wedge element */
      return PENTA_6;
    case MBHEX :          /**< Hexahedral element */
      return HEXA_8;
    default :
      std::cout << "It was not possible to identify the CGNSType\n";
      return 0;
  }
}

} //namespace moab
