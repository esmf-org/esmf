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
// Filename      : ReadCGM.hpp
//
// Purpose       : .sat, .step and .brep file reader
//
// Special Notes : Lots of code taken from cgm2moab implementation
//
// Creator       : Jane Hu
//
// Date          : 3/09
//
//-------------------------------------------------------------------------

#ifndef READCGM_HPP
#define READCGM_HPP

#ifndef CGM
#error "ReadCGM.hpp isn't supposed to be included without building CGM"
#endif

#include <string>
#include "moab/ReaderIface.hpp"

namespace moab {

class ReadUtilIface;
class GeomTopoTool;

class ReadCGM : public ReaderIface
{

public:

  static ReaderIface* factory( Interface* );

  void tokenize( const std::string& str,
                 std::vector<std::string>& tokens,
                 const char* delimiters );

    //! load a CGM file
    //  Supported FileOptions:
    //  * FACET_NORMAL_TOLERANCE=<int> (default: 5)
    //  * FACET_DISTANCE_TOLERANCE=<real> (default: 0.001)
    //  * MAX_FACET_EDGE_LENGTH=<real> (default: 0.0)
    //  * CGM_ATTRIBS=<yes|no>         (default: no)
  ErrorCode load_file( const char* file_name,
                       const EntityHandle* file_set,
                       const FileOptions& opts,
                       const SubsetList* subset_list = 0,
                       const Tag* file_id_tag = 0 );

  ErrorCode read_tag_values( const char* file_name,
                             const char* tag_name,
                             const FileOptions& opts,
                             std::vector<int>& tag_values_out,
                             const SubsetList* subset_list = 0 );

   //! Constructor
   ReadCGM(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadCGM();

private:

  ReadUtilIface* readUtilIface;

  GeomTopoTool* myGeomTool;

  const char* get_geom_file_type( const char* filename );
  const char* get_geom_fptr_type( FILE* file );

  int is_cubit_file( FILE* file );
  int is_step_file( FILE* file );
  int is_iges_file( FILE* file );
  int is_acis_txt_file( FILE* file );
  int is_acis_bin_file( FILE* file );
  int is_occ_brep_file( FILE* file );


  //------------member variables ------------//

    //! interface instance
  Interface* mdbImpl;

  Tag geom_tag, id_tag, name_tag, category_tag, faceting_tol_tag, 
        geometry_resabs_tag;
};

} // namespace moab

#endif
