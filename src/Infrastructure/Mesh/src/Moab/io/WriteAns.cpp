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


#ifdef WIN32
#ifdef _DEBUG
// turn off warnings that say they debugging identifier has been truncated
// this warning comes up when using some STL containers
#pragma warning(disable : 4786)
#endif
#endif


#include "WriteAns.hpp"

#include <utility>
#include <algorithm>
#include <time.h>
#include <string>
#include <vector>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <iomanip>

#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "assert.h"
#include "Internals.hpp"
#include "ExoIIUtil.hpp"
#include "MBTagConventions.hpp"

#define INS_ID(stringvar, prefix, id) \
          sprintf(stringvar, prefix, id)

namespace moab {

WriterIface* WriteAns::factory( Interface* iface )
  { return new WriteAns( iface ); }

WriteAns::WriteAns(Interface *impl) 
    : mbImpl(impl), mCurrentMeshHandle(0), mGlobalIdTag(0), mMatSetIdTag(0)
{
  assert(impl != NULL);

  //impl->query_interface( mWriteIface );

    // initialize in case tag_get_handle fails below
  //! get and cache predefined tag handles
  const int negone = -1;
  impl->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mMaterialSetTag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);
  
  impl->tag_get_handle(DIRICHLET_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mDirichletSetTag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);
  
  impl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mNeumannSetTag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);
}

WriteAns::~WriteAns() 
{
  //mbImpl->release_interface(mWriteIface);

}

ErrorCode WriteAns::write_file(const char *file_name, 
                                      const bool /* overwrite (commented out to remove warning) */,
                                      const FileOptions&,
                                      const EntityHandle *ent_handles,
                                      const int num_sets,
                                      const std::vector<std::string>&, 
                                      const Tag* ,
                                      int ,
                                      int )
{
  assert(0 != mMaterialSetTag &&
         0 != mNeumannSetTag &&
         0 != mDirichletSetTag);
  
  ErrorCode result;

  //set SOLID45 element type to #60000, hope nobody has already...
  const char *ETSolid45 = "60045";
  const char *ETSolid92 = "60042";
  const char *ETSolid95 = "60095";

  //set Material id # to be used as default for all elements
  //will need to be subsequently reassigned inside ANSYS
  //Can, although have not, declare similar defaults for other attributes
  const char *MATDefault = "1";

  //create file streams for writing
  std::ofstream node_file;
  std::ofstream elem_file;
  std::ofstream ans_file;
 
  //get base filename from filename.ans 
  std::string temp_string;
  std::string base_string;
  base_string.assign(file_name);
  base_string.replace(base_string.find_last_of(".ans")-3,4,"");
  
  //open node file for writing
  temp_string=base_string+".node";
  node_file.open(temp_string.c_str());
  node_file.setf(std::ios::scientific, std::ios::floatfield);
  node_file.precision(13);

  //open elem file for writing
  temp_string=base_string+".elem";
  elem_file.open(temp_string.c_str());

  //open ans file for writing
  ans_file.open(file_name);
  ans_file << "/prep7" << std::endl;

  //gather single output set
  EntityHandle output_set = 0;
  if(ent_handles && num_sets > 0)
    {
      for(int i=0;i<num_sets;i++)
	{
// from template, maybe can be removed
	  result=mbImpl->unite_meshset(output_set,ent_handles[i]);
	  if(result!=MB_SUCCESS)return result;
        }
    }
  
  //search for all nodes
  Range node_range;
  result=mbImpl->get_entities_by_type(output_set, MBVERTEX, node_range, true);
  if(result !=MB_SUCCESS) return result;
  
  
  // Commented out until Seg Fault taken care of in gather_nodes...
  //get any missing nodes which are needed for elements
  //Range all_ent_range,missing_range;
  //result=mbImpl->get_entities_by_handle(output_set,all_ent_range,true);
  //if(result !=MB_SUCCESS) return result;
  //result=mWriteIface->gather_nodes_from_elements(all_ent_range,0,missing_range);
  //node_range.merge(missing_range);
  

  // write the nodes 
  double coord[3];
  for(Range::iterator it = node_range.begin(); it != node_range.end(); ++it)
    {
      EntityHandle node_handle = *it;
     
      result = mbImpl->get_coords(&node_handle,1, coord);
      if(result !=MB_SUCCESS) return result;
      
      node_file.width(8);
      node_file << mbImpl->id_from_handle(node_handle);
      node_file.width(20);
      node_file << coord[0];
      node_file.width(20);
      node_file << coord[1];
      node_file.width(20);
      node_file << coord[2] << std::endl; 
    }
  
  //update header to load nodes
  ans_file << "nread," << base_string << ",node" << std::endl;

  //search for all node sets (Dirichlet Sets)
  Range node_mesh_sets;
  int ns_id;
  result = mbImpl->get_entities_by_type_and_tag(0,MBENTITYSET,&mDirichletSetTag,NULL,1,node_mesh_sets);
  if(result !=MB_SUCCESS)return result;

  for(Range::iterator ns_it = node_mesh_sets.begin(); ns_it!=node_mesh_sets.end(); ++ns_it)
    {
      result = mbImpl->tag_get_data(mDirichletSetTag, &(*ns_it),1,&ns_id);
      if(result !=MB_SUCCESS)return result;
      std::vector<EntityHandle> node_vector;
      result = mbImpl->get_entities_by_handle(*ns_it,node_vector, true);
      if(result !=MB_SUCCESS)return result;
      //for every nodeset found, cycle through nodes in set:
      for(std::vector<EntityHandle>::iterator node_it = node_vector.begin(); node_it!=node_vector.end(); ++node_it)
	{
	  int ns_node_id = mbImpl->id_from_handle(*node_it);
	  if(node_it==node_vector.begin())
	    {
	      //select first node in new list
	      ans_file << "nsel,s,node,,"<< std::setw(8) << ns_node_id << std::endl;
	    }else{
	      //append node to list
	      ans_file << "nsel,a,node,,"<< std::setw(8) << ns_node_id << std::endl;
	    }
	  
	}
      //create NS(#) nodeset
      ans_file << "cm,NS" << ns_id << ",node" << std::endl;
    }
  
  // ANSYS Element format:
  // I, J, K, L, M, N, O, P,etc... MAT, TYPE, REAL, SECNUM, ESYS, IEL
  // I-P are nodes of element
  // MAT = material number
  // TYPE = Element type number
  // REAL = Real constant set number
  // SECNUM = section attribute number
  // ESYS = coordinate system for nodes
  // IEL = element # (unique?)
  // For all nodes past 8, write on second line

  //Write all MBTET elements
  Range tet_range;
  result = mbImpl->get_entities_by_type(output_set, MBTET, tet_range, true);
  if(result !=MB_SUCCESS) return result;
  for(Range::iterator elem_it=tet_range.begin();elem_it!=tet_range.end();++elem_it)
    {
      EntityHandle elem_handle = *elem_it;
      int elem_id = mbImpl->id_from_handle(elem_handle);
      std::vector<EntityHandle> conn;
      result = mbImpl->get_connectivity(&elem_handle, 1, conn, false);
      if(result !=MB_SUCCESS) return result;
      //make sure 4 or 10 node tet
      if(conn.size()!=4 && conn.size()!=10)
	{
	  std::cout << "Support not added for element type. \n";
	  return MB_FAILURE;
	}      
      //write information for 4 node tet
      if(conn.size()==4){
	elem_file << std::setw(8) << conn[0] << std::setw(8) << conn[1];
	elem_file << std::setw(8) << conn[2] << std::setw(8) << conn[2];
	elem_file << std::setw(8) << conn[3] << std::setw(8) << conn[3];
	elem_file << std::setw(8) << conn[3] << std::setw(8) << conn[3];

	elem_file << std::setw(8) << MATDefault << std::setw(8) << ETSolid45;
    	elem_file << std::setw(8) << "1" << std::setw(8) << "1";
	elem_file << std::setw(8) << "0" << std::setw(8) << elem_id;
	elem_file << std::endl; 
      }
     
     //write information for 10 node tet
      if(conn.size()==10){
      	elem_file << std::setw(8) << conn[0] << std::setw(8) << conn[1];
	elem_file << std::setw(8) << conn[2] << std::setw(8) << conn[3];
	elem_file << std::setw(8) << conn[4] << std::setw(8) << conn[5];
	elem_file << std::setw(8) << conn[6] << std::setw(8) << conn[7];
      
	elem_file << std::setw(8) << MATDefault << std::setw(8) << ETSolid92;
    	elem_file << std::setw(8) << "1" << std::setw(8) << "1";
	elem_file << std::setw(8) << "0" << std::setw(8) << elem_id;
	elem_file << std::endl; 

	elem_file << std::setw(8) << conn[8] << std::setw(8) << conn[9];
	elem_file << std::endl;
      }
   
    }

  //Write all MBHEX elements
  Range hex_range;
  result = mbImpl->get_entities_by_type(output_set, MBHEX, hex_range, true);
  if(result !=MB_SUCCESS) return result;
  for(Range::iterator elem_it=hex_range.begin();elem_it!=hex_range.end();++elem_it)
    {
      EntityHandle elem_handle = *elem_it;
      int elem_id = mbImpl->id_from_handle(elem_handle);
      std::vector<EntityHandle> conn;
      result = mbImpl->get_connectivity(&elem_handle, 1, conn,false);
      if(result !=MB_SUCCESS) return result;
      //make sure supported hex type
      if(conn.size()!=8 && conn.size()!=20)
	{
	  std::cout << "Support not added for element type. \n";
	  return MB_FAILURE;
	}

      //write information for 8 node hex
      if(conn.size()==8){
	elem_file << std::setw(8) << conn[0] << std::setw(8) << conn[1];
	elem_file << std::setw(8) << conn[2] << std::setw(8) << conn[3];
	elem_file << std::setw(8) << conn[4] << std::setw(8) << conn[5];
	elem_file << std::setw(8) << conn[6] << std::setw(8) << conn[7];
      
	elem_file << std::setw(8) << MATDefault << std::setw(8) << ETSolid45;
	elem_file << std::setw(8) << "1" << std::setw(8) << "1";
	elem_file << std::setw(8) << "0" << std::setw(8) << elem_id;
	elem_file << std::endl;    
      }

      //write information for 20 node hex
      if(conn.size()==20){

	elem_file << std::setw(8) << conn[4] << std::setw(8) << conn[5];
	elem_file << std::setw(8) << conn[1] << std::setw(8) << conn[0];
	elem_file << std::setw(8) << conn[7] << std::setw(8) << conn[6];
	elem_file << std::setw(8) << conn[2] << std::setw(8) << conn[3];
     
	elem_file << std::setw(8) << MATDefault << std::setw(8) << ETSolid95;
	elem_file << std::setw(8) << "1" << std::setw(8) << "1";
	elem_file << std::setw(8) << "0" << std::setw(8) << elem_id;
	elem_file << std::endl;    

	elem_file << std::setw(8) << conn[16] << std::setw(8) << conn[13];
	elem_file << std::setw(8) << conn[8] << std::setw(8) << conn[12];
	elem_file << std::setw(8) << conn[18] << std::setw(8) << conn[14];
	elem_file << std::setw(8) << conn[10] << std::setw(8) << conn[15];
	elem_file << std::setw(8) << conn[19] << std::setw(8) << conn[17];
	elem_file << std::setw(8) << conn[9] << std::setw(8) << conn[11];
	elem_file << std::endl;
      }
      
    }
  //Write all MBPRISM elements
  Range prism_range;
  result = mbImpl->get_entities_by_type(output_set, MBPRISM, prism_range, true);
  if(result !=MB_SUCCESS) return result;
  for(Range::iterator elem_it=prism_range.begin();elem_it!=prism_range.end();++elem_it)
    {
      EntityHandle elem_handle = *elem_it;
      int elem_id = mbImpl->id_from_handle(elem_handle);
      std::vector<EntityHandle> conn;
      result = mbImpl->get_connectivity(&elem_handle, 1, conn,false);
      if(result !=MB_SUCCESS) return result;
      //make sure supported prism type
      if(conn.size()!=6)
	{
	  std::cout << "Support not added for element type. \n";
	  return MB_FAILURE;
	}

      //write information for 6 node prism
      if(conn.size()==6){
	elem_file << std::setw(8) << conn[0] << std::setw(8) << conn[3];
	elem_file << std::setw(8) << conn[4] << std::setw(8) << conn[4];
	elem_file << std::setw(8) << conn[1] << std::setw(8) << conn[2];
	elem_file << std::setw(8) << conn[5] << std::setw(8) << conn[5];
      
	elem_file << std::setw(8) << MATDefault << std::setw(8) << ETSolid45;
	elem_file << std::setw(8) << "1" << std::setw(8) << "1";
	elem_file << std::setw(8) << "0" << std::setw(8) << elem_id;
	elem_file << std::endl;    
      }
      
    }
  
  //create element types (for now writes all, even if not used)
  ans_file << "et," << ETSolid45 << ",SOLID45" << std::endl;
  ans_file << "et," << ETSolid92 << ",SOLID92" << std::endl;
  ans_file << "et," << ETSolid95 << ",SOLID95" << std::endl;

  // xxx pyramids, other elements later...

  //write header to load elements
  ans_file << "eread," << base_string << ",elem" << std::endl;

  //search for all side sets (Neumann)
  Range side_mesh_sets;
  int ss_id;
  result = mbImpl->get_entities_by_type_and_tag(0,MBENTITYSET,&mNeumannSetTag,NULL,1,side_mesh_sets);
  if(result !=MB_SUCCESS)return result;
  //cycle through all sets found
  for(Range::iterator ss_it = side_mesh_sets.begin(); ss_it!=side_mesh_sets.end(); ++ss_it)
    {
      result = mbImpl->tag_get_data(mNeumannSetTag, &(*ss_it),1,&ss_id);
      if(result !=MB_SUCCESS)return result;
      std::vector<EntityHandle> elem_vector;
      result = mbImpl->get_entities_by_handle(*ss_it,elem_vector, true);
      if(result !=MB_SUCCESS)return result;
      
      //cycle through elements in current side set
      for(std::vector<EntityHandle>::iterator elem_it = elem_vector.begin(); elem_it!=elem_vector.end();++elem_it)
	{
	  EntityHandle elem_handle = *elem_it;
	  	  
	  //instead of selecting current element in set, select its nodes...
	  std::vector<EntityHandle> conn;
	  result = mbImpl->get_connectivity(&elem_handle, 1, conn);
	  if(result !=MB_SUCCESS)return result;
	  if(elem_it==elem_vector.begin())
	  {
	    ans_file << "nsel,s,node,," <<std::setw(8)<< conn[0] << std::endl;
	    for(unsigned int i=1; i<conn.size(); i++)
	      {
		ans_file << "nsel,a,node,," << std::setw(8) << conn[i] << std::endl;
	      }
	  }else{
	    for(unsigned int i=0; i<conn.size(); i++)
	      {
		ans_file << "nsel,a,node,," << std::setw(8) << conn[i] << std::endl;
	      }
	  }
	  
	}
      //create SS(#) node set
      ans_file << "cm,SS" << ss_id << ",node" << std::endl;
    }
  
  //Gather all element blocks
  Range matset;
  int mat_id;
  result = mbImpl->get_entities_by_type_and_tag(0, MBENTITYSET, &mMaterialSetTag, NULL, 1, matset);
  if(result !=MB_SUCCESS)return result;
  //cycle through all elem blocks
  for(Range::iterator mat_it = matset.begin(); mat_it!=matset.end(); ++mat_it)
    {
      EntityHandle matset_handle = *mat_it;
      result = mbImpl->tag_get_data(mMaterialSetTag,&matset_handle, 1,&mat_id);
      if(result !=MB_SUCCESS)return result;
      std::vector<EntityHandle> mat_vector;
      result = mbImpl->get_entities_by_handle(*mat_it,mat_vector, true);
      if(result !=MB_SUCCESS)return result;
      //cycle through elements in current mat set
      for(std::vector<EntityHandle>::iterator elem_it = mat_vector.begin(); elem_it!=mat_vector.end(); ++elem_it)
	{
	  EntityHandle elem_handle = *elem_it;
	  int elem_id = mbImpl->id_from_handle(elem_handle);
	  if(elem_it==mat_vector.begin())
	    {
	      ans_file << "esel,s,elem,," << std::setw(8) << elem_id << std::endl;
	    }else{
	      ans_file << "esel,a,elem,," << std::setw(8) << elem_id << std::endl;
	    }
	}
      //for each matset, write block command
      ans_file << "cm,EB" << mat_id << ",elem" << std::endl;
    }

  //close all file streams
  node_file.close();
  elem_file.close();
  ans_file.close();

  return MB_SUCCESS;
}

} // namespace moab
