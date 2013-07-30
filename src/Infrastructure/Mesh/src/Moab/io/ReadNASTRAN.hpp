/**                                                       
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,  
 * storing and accessing finite element mesh data.   
 *                                         
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract        
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government    
 * retains certain rights in this software.      
 *                    
 * This library is free software; you can redistribute it and/or     
 * modify it under the terms of the GNU Lesser General Public    
 * License as published by the Free Software Foundation; either       
 * version 2.1 of the License, or (at your option) any later version.     
 *                     
 */                 
                         
//-------------------------------------------------------------------------    
// Filename      : ReadNASTRAN.hpp                        
//                                
// Purpose       : NASTRAN file reader
//                                             
// Creator       : Brandon Smith             
//                                   
// Date          : 08/2009                
//                                                  
//-------------------------------------------------------------------------     
                                    
#ifndef READNASTRAN_HPP                     
#define READNASTRAN_HPP              
                                     
#ifndef IS_BUILDING_MB                   
  #error "ReadNASTRAN.hpp isn't supposed to be included into an application"
#endif   

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

#include "moab/Interface.hpp"
#include "moab/ReaderIface.hpp"
#include "FileTokenizer.hpp"
#include "moab/RangeMap.hpp"

namespace moab {

class ReadUtilIface;

class ReadNASTRAN : public ReaderIface
{

public:
  // factory method
  static ReaderIface* factory( Interface* );
  
  ErrorCode load_file( const char* file_name,
                       const EntityHandle* file_set,
                       const FileOptions& opts,
                       const SubsetList* subset_list = 0,
                       const Tag* file_id_tag = 0 );
  // constructor
  ReadNASTRAN(Interface* impl = NULL);

  // destructor
  virtual ~ReadNASTRAN();

  ErrorCode read_tag_values( const char* file_name,
                             const char* tag_name,
                             const FileOptions& opts,
                             std::vector<int>& tag_values_out,
                             const SubsetList* subset_list = 0 );

protected:
  
private:  
  // read mesh interface
  ReadUtilIface* readMeshIface;
  
  // MOAB Interface
  Interface* MBI;
  
  RangeMap<int, EntityHandle> nodeIdMap, elemIdMap;

  enum line_format { SMALL_FIELD,                     
                     LARGE_FIELD,                 
                     FREE_FIELD }; 

  ErrorCode determine_line_format( const std::string line, 
                                     line_format &format );
  
  ErrorCode tokenize_line( const std::string line, 
                             const line_format format,
                             std::vector<std::string> &tokens );  

  ErrorCode determine_entity_type( const std::string token, EntityType &type); 

  ErrorCode get_real( const std::string, double &real );

  ErrorCode read_node(const std::vector<std::string> tokens, 
                        const bool           debug, 
                        double*              coord_arrays[3], 
                        int                  &node_id);

  ErrorCode read_element(const std::vector<std::string> tokens, 
                           std::vector<Range>           &materials,
                           const EntityType             element_type,
                           const bool                     debug );

  ErrorCode create_materials( const std::vector<Range> &materials );

  ErrorCode assign_ids( const Tag* file_id_tag );
};

} // namespace moab

#endif
