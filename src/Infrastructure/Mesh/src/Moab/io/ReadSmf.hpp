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

#ifndef READ_SMF_HPP
#define READ_SMF_HPP

#define SMF_MAXLINE 4096

#include "moab/Forward.hpp"
#include "moab/ReaderIface.hpp"

#include "SMF_State.hpp"
#include <iosfwd>
#include <fstream>

namespace moab {

class ReadUtilIface;
class AffineXform;

/**\brief Read SMF (Simple Model Format) files.
 *
 * File format is documented at:
 * http://people.sc.fsu.edu/~burkardt/data/smf/smf.txt
 */
class ReadSmf : public ReaderIface
{
   
public:

  static ReaderIface* factory( Interface* );

    //! load a file
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
  ReadSmf(Interface* impl = NULL);

   //! Destructor
  virtual ~ReadSmf();


  typedef ErrorCode (ReadSmf::*read_cmd)( std::vector<std::string> & argv);
  struct cmd_entry { const char* name; read_cmd cmd; };
  void init();

protected:


    ErrorCode annotation(char *cmd,  std::vector<std::string> & argv);
    void bad_annotation(const char* cmd);

    ErrorCode vertex(std::vector<std::string> &);
    ErrorCode v_normal( std::vector<std::string> &);
    ErrorCode v_color(std::vector<std::string> &);
    ErrorCode f_color(std::vector<std::string> &);
    ErrorCode face(std::vector<std::string> &);

    ErrorCode begin(std::vector<std::string> &);
    ErrorCode end(std::vector<std::string> &);
    ErrorCode set(std::vector<std::string> &);
    ErrorCode inc(std::vector<std::string> &);
    ErrorCode dec(std::vector<std::string> &);

    ErrorCode trans(std::vector<std::string> &);
    ErrorCode scale(std::vector<std::string> &);
    ErrorCode rot(std::vector<std::string> &);
    ErrorCode mmult(std::vector<std::string> &);
    ErrorCode mload(std::vector<std::string> &);

    ErrorCode parse_line(char *line);
    
    ErrorCode parse_doubles( int count,
                             const std::vector<std::string> & argv,
                             double results[] );
    ErrorCode parse_mat( const std::vector<std::string> & argv,
                         AffineXform& mat_out );
    ErrorCode check_length( int count, const std::vector<std::string> & argv );
    
private:

  ReadUtilIface* readMeshIface;

  //------------member variables ------------//


     //! interface instance
    //! interface instance
  Interface* mdbImpl;

    //! Meshset Handle for the mesh that is currently being read
  EntityHandle mCurrentMeshHandle;

    //! A field which, if present and having a single integer for storage, should be used to partition the mesh by range. Defaults to MATERIAL_SET_TAG_NAME
  std::string mPartitionTagName;
 
  // these are from SMF_reader from qslim/gfx/SMF/smf.h  
  static cmd_entry read_cmds[];
  char line[SMF_MAXLINE];
  std::vector<SMF_State> state;
  SMF_ivars ivar;
  int _numNodes;
  int _numFaces;
  std::vector<double> _coords; // 3*numNodes; we might not know the number of nodes
  std::vector<int> _connec; // 3*num of elements; we might not know them;
  int _numNodesInFile;
  int _numElementsInFile;
  size_t lineNo;
  size_t commandNo;
  int versionMajor, versionMinor;
   
};

} // namespace moab

#endif




