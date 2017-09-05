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

//----------------------------------------------------------------------
// Filename : ReadMCNP5.hpp    
// Purpose  : Read a meshtal file created by MCNP5 into MOAB
// Creator  : Brandon Smith
// Date     : 07/2009
//----------------------------------------------------------------------

/**
 * Data structure of MCNP5 data created by this reader:
 *
 * each file_meshset contains
 *   DATA_AND_TIME_TAG
 *   TITLE_TAG
 *   NPS_TAG
 *   each tally_meshset contains
 *     TALLY_NUMBER_TAG
 *     TALLY_COMMENT_TAG
 *     TALLY_PARTICLE_TAG
 *     TALLY_COORD_SYS_TAG
 *     each mesh element contains
 *       TALLY_TAG
 *       ERROR_TAG
 */

#include "moab/Interface.hpp"
#include "moab/ReaderIface.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

namespace moab {

class ReadUtilIface;

class ReadMCNP5 : public ReaderIface
{

public:
  // factory method
  static ReaderIface* factory( Interface* );
  
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

  // constructor
  ReadMCNP5(Interface* impl = NULL);

  // destructor
  virtual ~ReadMCNP5();
  
protected:
  
private:
  // constants
  static const double PI;
  static const double C2PI;
  static const double CPI;

  enum coordinate_system { NO_SYSTEM,
                           CARTESIAN,
                           CYLINDRICAL,
                           SPHERICAL };
  enum particle { NEUTRON,
                  PHOTON,
                  ELECTRON };
  
  // read mesh interface
  ReadUtilIface* readMeshIface;
  
  // MOAB Interface
  Interface* MBI;
  
  const Tag* fileIDTag;
  int nodeId, elemId;

  // reads the meshtal file
  ErrorCode load_one_file( const char           *fname,
                             const EntityHandle *input_meshset,
                             const FileOptions    &options,
                             const bool           average );
  
  ErrorCode create_tags( Tag &date_and_time_tag,  
                           Tag &title_tag,
                           Tag &nps_tag, 
                           Tag &tally_number_tag,   
                           Tag &tally_comment_tag,
                           Tag &tally_particle_tag, 
                           Tag &tally_coord_sys_tag,
                           Tag &tally_tag,          
                           Tag &error_tag );

  ErrorCode read_file_header( std::fstream      &file,
                                bool              debug,
                                char              date_and_time[100], 
                                char              title[100], 
                                unsigned long int &nps );

  ErrorCode set_header_tags( EntityHandle             output_meshset, 
                                        char              date_and_time[100],
                                        char              title[100],
                                        unsigned long int nps,
                                        Tag             data_and_time_tag,
                                        Tag             title_tag,
                                        Tag             nps_tag );

  ErrorCode read_tally_header( std::fstream &file,
                                 bool         debug,
                                 unsigned int &tally_number,
                                 char         tally_comment[100],
                                 particle     &tally_particle );

  ErrorCode get_tally_particle( std::string a,
                                  bool        debug,
                                  particle    &tally_particle );

  ErrorCode read_mesh_planes( std::fstream         &file, 
                                bool                 debug, 
                                std::vector<double>  planes[3], 
                                coordinate_system    &coord_sys);

  ErrorCode get_mesh_plane( std::istringstream  &ss, 
                              bool                debug, 
                              std::vector<double> &plane);

  ErrorCode read_element_values_and_errors( std::fstream        &file,
                                              bool                debug,
                                              std::vector<double> planes[3],
                                              unsigned int        n_chopped_x0_planes,
                                              unsigned int        n_chopped_x2_planes,
                                              particle            tally_particle,
                                              double              values[],
                                              double              errors[] );

  ErrorCode set_tally_tags( EntityHandle    tally_meshset,
                              unsigned int      tally_number,
                              char              tally_comment[100],
                              particle          tally_particle,
                              coordinate_system tally_coord_sys,
                              Tag             tally_number_tag, 
                              Tag             tally_comment_tag,
                              Tag             tally_particle_tag,
                              Tag             tally_coord_sys_tag );

  ErrorCode create_vertices( std::vector<double> planes[3],
                               bool                debug,
                               EntityHandle      &start_vert,
                               coordinate_system   coord_sys,
                               EntityHandle      tally_meshset );
 
  ErrorCode create_elements( bool                debug, 
                               std::vector<double> planes[3],
                               unsigned int        n_chopped_x0_planes,
                               unsigned int        n_chopped_x2_planes,
                               EntityHandle      start_vert,
                               double              values[],
                               double              errors[],
                               Tag               tally_tag,
                               Tag               error_tag,
                               EntityHandle      tally_meshset,
                               coordinate_system   tally_coord_sys );

  ErrorCode average_with_existing_tally( bool              debug,
                                           unsigned long int &new_nps,
                                           unsigned long int nps,
                                           unsigned int      tally_number,
                                           Tag             tally_number_tag,
                                           Tag             nps_tag,
                                           Tag             tally_tag,
                                           Tag             error_tag,
                                           double            values[],
                                           double            errors[],
                                           unsigned int      n_elements );
  
  ErrorCode transform_point_to_cartesian(double *in, 
                                           double *out, 
                                           coordinate_system coord_sys);

  ErrorCode average_tally_values(const unsigned long int nps0, 
                                   const unsigned long int nps1,
                                   double                  *values0,
                                   const double            *values1,
                                   double                  *errors0,
                                   const double            *errors1,
                                   const unsigned long int n_values);
};

} // namespace moab
