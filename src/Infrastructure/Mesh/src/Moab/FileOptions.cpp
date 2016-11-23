/*
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

/**\file FileOptions.cpp
 *\author Jason Kraftcheck (kraftche@cae.wisc.edu)
 *\date 2007-08-21
 */

#include "FileOptions.hpp"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>

namespace moab {

const char DEFAULT_SEPARATOR = ';';

static inline bool strempty( const char* s ) { return !*s; }

FileOptions::FileOptions( const char* str )
  : mData(0)
{
    // if option string is null, just return
  if (!str)
    return;
  
    // check if alternate separator is specified
  char separator[2] = { DEFAULT_SEPARATOR, '\0' };
  if (*str == DEFAULT_SEPARATOR) {
    ++str;
    if (strempty(str))
      return;
    separator[0] = *str;
    ++str;
  }
  
    // don't bother allocating copy of input string if
    // input string is empty.
  if (!strempty(str))
  {
       // tokenize at separator character
    mData = strdup( str );
    for (char* i = strtok( mData, separator ); i; i = strtok( 0, separator )) 
      if (!strempty(i)) // skip empty strings
        mOptions.push_back( i );
  }
  
  mSeen.resize( mOptions.size(), false );
}

FileOptions::FileOptions( const FileOptions& copy ) :
  mData(0), mOptions( copy.mOptions.size() )
{
  if (!copy.mOptions.empty()) {
    const char* last = copy.mOptions.back();
    const char* endptr = last + strlen(last) + 1;
    size_t len = endptr - copy.mData;
    mData = (char*)malloc( len );
    memcpy( mData, copy.mData, len );
    for (size_t i = 0; i < mOptions.size(); ++i)
      mOptions[i] = mData + (copy.mOptions[i] - copy.mData);
  }
  mSeen = copy.mSeen;
}

FileOptions& FileOptions::operator=( const FileOptions& copy )
{
  free( mData );
  mData = 0;
  mOptions.resize( copy.mOptions.size() );

  if (!copy.mOptions.empty()) {
    const char* last = copy.mOptions.back();
    const char* endptr = last + strlen(last) + 1;
    size_t len = endptr - copy.mData;
    mData = (char*)malloc( len );
    memcpy( mData, copy.mData, len );
    for (size_t i = 0; i < mOptions.size(); ++i)
      mOptions[i] = mData + (copy.mOptions[i] - copy.mData);
  }

  mSeen = copy.mSeen;
  return *this;
}

FileOptions::~FileOptions()
{
  free( mData );
}

ErrorCode FileOptions::get_null_option( const char* name ) const
{
  const char* s;
  ErrorCode rval = get_option( name, s );
  if (MB_SUCCESS != rval)
    return rval;
  return strempty(s) ? MB_SUCCESS : MB_TYPE_OUT_OF_RANGE;
}

ErrorCode FileOptions::get_int_option( const char* name, int& value ) const
{
  const char* s;
  ErrorCode rval = get_option( name, s );
  if (MB_SUCCESS != rval)
    return rval;
  
    // empty string
  if (strempty(s))
    return MB_TYPE_OUT_OF_RANGE;
  
    // parse value
  char* endptr;
  long int pval = strtol( s, &endptr, 0 );
  if (!strempty(endptr)) // syntax error
    return MB_TYPE_OUT_OF_RANGE;
  
    // check for overflow (parsing long int, returning int)
  value = pval;
  if (pval != (long int)value)
    return MB_TYPE_OUT_OF_RANGE;
  
  return MB_SUCCESS;
}

ErrorCode FileOptions::get_int_option( const char* name, 
                                       int default_val,
                                       int& value ) const
{
  const char* s;
  ErrorCode rval = get_option( name, s );
  if (MB_SUCCESS != rval)
    return rval;
  
    // empty string
  if (strempty(s)) {
    value = default_val;
    return MB_SUCCESS;
  }
  
    // parse value
  char* endptr;
  long int pval = strtol( s, &endptr, 0 );
  if (!strempty(endptr)) // syntax error
    return MB_TYPE_OUT_OF_RANGE;
  
    // check for overflow (parsing long int, returning int)
  value = pval;
  if (pval != (long int)value)
    return MB_TYPE_OUT_OF_RANGE;
  
  return MB_SUCCESS;
}

ErrorCode FileOptions::get_ints_option( const char* name, 
                                          std::vector<int>& values) const
{
  const char* s;
  ErrorCode rval = get_option( name, s );
  if (MB_SUCCESS != rval)
    return rval;
  
    // empty string
  if (strempty(s))
    return MB_TYPE_OUT_OF_RANGE;
  
    // parse values
  while (!strempty(s)) {
    char* endptr;
    long int sval = strtol( s, &endptr, 0 );

#define EATSPACE(a) while ((*a == ' ' ||          \
                            *a == ',') && !strempty(a)) a++;
    EATSPACE(endptr);
    long int eval = sval;
    if (*endptr == '-') {
      endptr++;
      s = endptr;
      eval = strtol(s, &endptr, 0);
      EATSPACE(endptr);
    }
  
      // check for overflow (parsing long int, returning int)
    int value = sval;
    if (sval != (long int)value)
      return MB_TYPE_OUT_OF_RANGE;
    value = eval;
    if (eval != (long int)value)
      return MB_TYPE_OUT_OF_RANGE;
  
    for (int i = sval; i <= eval; i++)
      values.push_back(i);

    s = endptr;
  }
  
  return MB_SUCCESS;
}

ErrorCode FileOptions::get_reals_option( const char* name, 
                                          std::vector<double>& values) const
{
  const char* s;
  ErrorCode rval = get_option( name, s );
  if (MB_SUCCESS != rval)
    return rval;
  
    // empty string
  if (strempty(s))
    return MB_TYPE_OUT_OF_RANGE;
  
    // parse values
  while (!strempty(s)) {
    char* endptr;
    double sval = strtod( s, &endptr);

    EATSPACE(endptr);
    values.push_back(sval);

    s = endptr;
  }
  
  return MB_SUCCESS;
}

ErrorCode FileOptions::get_real_option ( const char* name, double& value ) const
{
  const char* s;
  ErrorCode rval = get_option( name, s );
  if (MB_SUCCESS != rval)
    return rval;
  
    // empty string
  if (strempty(s))
    return MB_TYPE_OUT_OF_RANGE;
  
    // parse value
  char* endptr;
  value = strtod( s, &endptr );
  if (!strempty(endptr)) // syntax error
    return MB_TYPE_OUT_OF_RANGE;
  
  return MB_SUCCESS;
}

ErrorCode FileOptions::get_strs_option( const char* name, 
                                        std::vector<std::string>& values) const
{
  const char* s;
  ErrorCode rval = get_option( name, s );
  if (MB_SUCCESS != rval)
    return rval;
  
    // empty string
  if (strempty(s))
    return MB_TYPE_OUT_OF_RANGE;
  
    // parse values
  char separator[3] = { ' ', ',', '\0' };
  char *tmp_str = strdup(s);
  for (char* i = strtok( tmp_str, separator ); i; i = strtok( 0, separator )) 
    if (!strempty(i)) // skip empty strings
      values.push_back( std::string(i));
  free(tmp_str);
  
  return MB_SUCCESS;
}

ErrorCode FileOptions::get_str_option( const char* name, std::string& value ) const
{
  const char* s;
  ErrorCode rval = get_option( name, s );
  if (MB_SUCCESS != rval)
    return rval;
  if (strempty(s))
    return MB_TYPE_OUT_OF_RANGE;
  value = s;
  return MB_SUCCESS;
}

ErrorCode FileOptions::get_option( const char* name, std::string& value ) const
{
  const char* s;
  ErrorCode rval = get_option( name, s );
  if (MB_SUCCESS != rval)
    return rval;
  
  value = s;
  return MB_SUCCESS;
}  

ErrorCode FileOptions::get_option( const char* name, const char*& value ) const
{
  std::vector<const char*>::const_iterator i;
  for (i = mOptions.begin(); i != mOptions.end(); ++i) {
    const char* opt = *i;
    if (compare( name, opt )) {
      value = opt + strlen(name);
        // if compare returned true, next char after option
        // name must be either the null char or an equals symbol.
      if (*value == '=') 
        ++value;
      
      mSeen[i - mOptions.begin()] = true;
      return MB_SUCCESS;
    }
  }
  
  return MB_ENTITY_NOT_FOUND;
}

ErrorCode FileOptions::match_option( const char* name, 
                                       const char* value ) const
{
  int idx;
  const char* array[] = { value, NULL };
  return match_option( name, array, idx );
}

ErrorCode FileOptions::match_option( const char* name, 
                                       const char* const* values, 
                                       int& index ) const
{
  const char* optval;
  ErrorCode rval = get_option( name, optval );
  if (MB_SUCCESS != rval)
    return rval;
  
  for (index = 0; values[index]; ++index)
    if (compare( optval, values[index] ))
      return MB_SUCCESS;
  
  index = -1;
  return MB_FAILURE;
}

ErrorCode FileOptions::get_toggle_option( const char* name,
                                          bool default_value,
                                          bool& value ) const
{
  static const char* values[] = {
    "true",  "yes", "1", "on",
    "false", "no",  "0", "off",
    0 };
  const int num_true = 4;
  
  int index;
  ErrorCode result = match_option( name, values, index );
  if (result == MB_SUCCESS) {
    value = index < num_true;
  }
  else if (result == MB_ENTITY_NOT_FOUND) {
    value = default_value;
    result = MB_SUCCESS;
  }
  else {
    result = MB_TYPE_OUT_OF_RANGE;
  }
  
  return result;
}


bool FileOptions::compare( const char* name, const char* option )
{
  while (!strempty(name) && toupper(*name) == toupper(*option)) {
    ++name;
    ++option;
  }
   // match if name matched option for length of name,
   // and option either matched entirely or matches up to
   // and equals sign.
  return strempty(name) && (strempty(option) || *option == '=');
}

void FileOptions::get_options( std::vector<std::string>& list ) const
{
  list.clear();
  list.resize( mOptions.size() );
  std::copy( mOptions.begin(), mOptions.end(), list.begin() );
}

bool FileOptions::all_seen() const
{
  return std::find( mSeen.begin(), mSeen.end(), false ) == mSeen.end();
}

void FileOptions::mark_all_seen() const
{
  mSeen.clear();
  mSeen.resize( mOptions.size(), true );
}

ErrorCode FileOptions::get_unseen_option( std::string& name ) const
{
  std::vector<bool>::iterator i = std::find( mSeen.begin(), mSeen.end(), false );
  if (i == mSeen.end()) {
    name.clear();
    return MB_ENTITY_NOT_FOUND;
  }
  
  const char* opt = mOptions[i - mSeen.begin()];
  const char* end = strchr( opt, '=' );
  name = end ? std::string(opt, end-opt) : std::string(opt);
  return MB_SUCCESS;
}
  
} // namespace moab

#ifdef TEST

using namespace moab;

#include <iostream>

#define CHECK(A) \
  if (MB_SUCCESS != (A)) { \
    std::cerr << "Failure at line " << __LINE__ << ": error code " << (A) << std::endl; \
    return 1; \
  }

#define EQUAL(A,B) \
  if (A != B) { \
    std::cerr << "Failure at line " << __LINE__ << ": expected " << (B) << " but got " << (A) << std::endl; \
    return 2; \
  }

int main()
{
  FileOptions tool( "INT1=1;NUL1;STR1=ABC;DBL1=1.0;dbl2=2.0;DBL3=3.0;INT2=2;nul2;NUL3;INT3=3;str2=once upon a time;str3==fubar=;;INTS=1-3,5,6;DBLS=1.0,2.0, 3.0;STRS=var1, var2_var2;STRS2=" );

  std::string s;
  int i;
  double d;
  ErrorCode rval;
  
    // test basic get_option method without deleting entry
  rval = tool.get_option( "STR1", s );
  CHECK(rval);
  EQUAL( s, "ABC" );
  
    // test basic get_option method again, this time deleting the entry
  rval = tool.get_option( "STR1", s );
  CHECK(rval);
  EQUAL( s, "ABC" );
  
    // test basig get_option method with a null option
  rval = tool.get_option( "NUL2", s );
  CHECK( rval );
  EQUAL( s.empty(), true );

  
    // test null option
  rval = tool.get_null_option( "nul1" );
  CHECK( rval );
  
    // try null option method on non-null value
  rval = tool.get_null_option( "INT1" ) ;
  EQUAL( rval, MB_TYPE_OUT_OF_RANGE) ;
  

    // test integer option
  rval = tool.get_int_option( "int1", i );
  CHECK( rval );
  EQUAL( i, 1 );
  
  rval = tool.get_int_option( "int2", i );
  CHECK( rval );
  EQUAL( i, 2 );
  
    // test integer option on non-integer value
  rval = tool.get_int_option( "dbl2", i );
  EQUAL( rval, MB_TYPE_OUT_OF_RANGE );
  
    // test integer option on null value
  rval = tool.get_int_option( "NUL3", i);
  EQUAL( rval, MB_TYPE_OUT_OF_RANGE );
  
    // test double option
  rval = tool.get_real_option( "dbl1", d );
  CHECK( rval );
  EQUAL( d, 1.0 );
  
  rval = tool.get_real_option( "dbl2", d );
  CHECK( rval );
  EQUAL( d, 2.0 );
  
  rval = tool.get_real_option( "int3", d );
  CHECK( rval );
  EQUAL( d, 3.0 );
  
    // test real option on non-real value
  rval = tool.get_real_option( "str2", d );
  EQUAL( rval, MB_TYPE_OUT_OF_RANGE );
  
  
    // test real option on null value
  rval = tool.get_real_option( "NUL3", d );
  EQUAL( rval, MB_TYPE_OUT_OF_RANGE );
  
    // test get a simple string option
  rval = tool.get_str_option( "DBL3", s );
  CHECK( rval );
  EQUAL( s, "3.0" );
  
    // test get a string with spaces
  rval = tool.get_str_option("STR2", s );
  CHECK( rval );
  EQUAL( s, "once upon a time" );
  
    // try to get a string value for a null option
  rval = tool.get_str_option( "nul3", s );
  EQUAL( rval, MB_TYPE_OUT_OF_RANGE );
  
    // We haven't looked at all of the options yet
  EQUAL( false, tool.all_seen() );
  rval = tool.get_unseen_option( s );
  CHECK( rval );
  EQUAL( s, "str3" );
  
    // test options using generic get_option method
    
  rval = tool.get_option( "NUL3", s );
  CHECK( rval );
  EQUAL( s.empty(), true );
  
  rval = tool.get_option( "STR3", s );
  CHECK( rval );
  EQUAL( s, "=fubar=" );
  
    // test size of options string
  unsigned l = tool.size();
  EQUAL( l, 16u );
  
    // test ints option
  std::vector<int> ivals;
  rval = tool.get_ints_option("INTS", ivals);
  CHECK( rval );
  EQUAL(5, ivals.size());
  EQUAL(1, ivals[0]);
  EQUAL(2, ivals[1]);
  EQUAL(3, ivals[2]);
  EQUAL(5, ivals[3]);
  EQUAL(6, ivals[4]);

    // test dbls option
  std::vector<double> vals;
  rval = tool.get_reals_option("DBLS", vals);
  CHECK( rval );
  EQUAL(3, vals.size());
  EQUAL(1.0, vals[0]);
  EQUAL(2.0, vals[1]);
  EQUAL(3.0, vals[2]);
  
    // test strs option
  std::vector<std::string> svals;
  rval = tool.get_strs_option("STRS", svals);
  CHECK( rval );
  EQUAL(2, svals.size());
  EQUAL("var1", svals[0]);
  EQUAL("var2_var2", svals[1]);
  
  svals.clear();
  rval = tool.get_strs_option("STRS2", svals);
  EQUAL( MB_TYPE_OUT_OF_RANGE, rval );
  
    // We requested every option
  EQUAL( true, tool.all_seen() );
  rval = tool.get_unseen_option( s );
  EQUAL( MB_ENTITY_NOT_FOUND, rval );
  
    // test alternate separator
  
  FileOptions tool2( ";+OPT1=ABC+OPT2=" );
  l = tool2.size();
  EQUAL( l, 2 );

    // We haven't looked at all of the options yet
  EQUAL( false, tool2.all_seen() );
  rval = tool2.get_unseen_option( s );
  CHECK( rval );
  EQUAL( s, "OPT1" );
   
  rval = tool2.get_option( "opt1", s );
  CHECK( rval );
  EQUAL( s, "ABC" );
  
  rval = tool2.get_option( "opt2", s );
  CHECK( rval );
  bool e = s.empty();
  EQUAL( e, true );
  
  l = tool2.size();
  EQUAL( l, 2 );
  
    // We requested every option
  EQUAL( true, tool2.all_seen() );
  rval = tool2.get_unseen_option( s );
  EQUAL( MB_ENTITY_NOT_FOUND, rval );
  
    
    // test empty options string
    
  FileOptions tool3( ";;;;" );
  e = tool3.empty();
  EQUAL( e, true );
  l = tool3.size();
  EQUAL( l, 0 );
  EQUAL( true, tool3.all_seen() );
  
  FileOptions tool4(NULL);
  e = tool4.empty();
  EQUAL( e, true );
  l = tool4.size();
  EQUAL( l, 0 );
  EQUAL( true, tool4.all_seen() );
  
  FileOptions tool5(";+");
  e = tool5.empty();
  EQUAL( e, true );
  l = tool5.size();
  EQUAL( l, 0 );
  EQUAL( true, tool5.all_seen() );
  
    // test copy constructor
  
  FileOptions tool6( tool2 );
  
  rval = tool6.get_option( "opt1", s );
  CHECK( rval );
  EQUAL( s, "ABC" );
  
  rval = tool6.get_option( "opt2", s );
  CHECK( rval );
  e = s.empty();
  EQUAL( e, true );
  
  l = tool6.size();
  EQUAL( l, 2 );
  
  FileOptions tool7( tool5 );
  e = tool7.empty();
  EQUAL( e, true );
  l = tool7.size();
  EQUAL( l, 0 );
  
    // test assignment operator
  
  FileOptions tool8( tool2 );
  tool8 = tool;
  EQUAL( tool8.size(), tool.size() );
    
  return 0;
}

#endif
