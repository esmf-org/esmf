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

#include "FileTokenizer.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/ErrorHandler.hpp"

#include <cstring>
#include <cctype>
#include <string>
#include <cstdlib>

namespace moab {

using namespace std;

FileTokenizer::FileTokenizer(FILE* file_ptr, ReadUtilIface* )
  : filePtr(file_ptr),
    nextToken(buffer),
    bufferEnd(buffer),
    lineNumber(1),
    lastChar('\0')
  {}

FileTokenizer::~FileTokenizer()
{
  fclose(filePtr);
}

bool FileTokenizer::eof() const
{
  return nextToken == bufferEnd && feof(filePtr);
}

const char* FileTokenizer::get_string()
{
  // If the whitespace character marking the end of the
  // last token was a newline, increment the line count.
  if (lastChar == '\n')
    ++lineNumber;

  // Loop until either found the start of a token to return or have
  // reached the end of the file.
  for (;;) {
    // If the buffer is empty, read more.
    if (nextToken == bufferEnd) {
      size_t count = fread(buffer, 1, sizeof(buffer) - 1, filePtr);
      if (0 == count) {
        if (feof(filePtr))
          return NULL;
        else
          MB_SET_ERR_RET_VAL("I/O Error", NULL);
      }

      nextToken = buffer;
      bufferEnd = buffer + count;
    }

    // If the current character is not a space, we've found a token.
    if (!isspace(*nextToken))
      break;

    // If the current space character is a newline,
    // increment the line number count.
    if (*nextToken == '\n')
      ++lineNumber;
    ++nextToken;
  }

  // Store the start of the token in "result" and
  // advance "nextToken" to one past the end of the
  // token.
  char* result = nextToken;
  while (nextToken != bufferEnd && !isspace(static_cast<unsigned char>(*nextToken)))
    ++nextToken;

  // If we have reached the end of the buffer without finding
  // a whitespace character terminating the token, we need to
  // read more from the file.  Only try once.  If the token is
  // too large to fit in the buffer, give up.
  if (nextToken == bufferEnd) {
    // Shift the (possibly) partial token to the start of the buffer.
    size_t remaining = bufferEnd - result;
    memmove(buffer, result, remaining);
    result = buffer;
    nextToken =  result + remaining;

    // Fill the remainder of the buffer after the token.
    size_t count = fread(nextToken, 1, sizeof(buffer) - remaining - 1, filePtr);
    if (0 == count && !feof(filePtr))
      MB_SET_ERR_RET_VAL("I/O Error", NULL);
    bufferEnd = nextToken + count;

    // Continue to advance nextToken until we find the space
    // terminating the token.
    while (nextToken != bufferEnd && !isspace(*nextToken))
      ++nextToken;

    if (nextToken == bufferEnd) { // EOF
      *bufferEnd = '\0';
      ++bufferEnd;
    }
  }

  // Save terminating whitespace character (or NULL char if EOF).
  lastChar = *nextToken;
  // Put null in buffer to mark end of current token.
  *nextToken = '\0';
  // Advance nextToken to the next character to search next time.
  ++nextToken;

  return result;
}

bool FileTokenizer::get_double_internal(double& result)
{
  // Get a token
  const char *token_end, *token = get_string();
  if (!token)
    return false;

  // Check for hex value -- on some platforms (e.g. Linux), strtod
  // will accept hex values, on others (e.g. Sun) it will not.  Force
  // failure on hex numbers for consistency.
  if (token[0] && token[1] && token[0] == '0' && toupper(token[1]) == 'X')
    MB_SET_ERR_RET_VAL("Syntax error at line " << line_number() << ": expected number, got \"" << token << "\"", false);

  // Parse token as double
  result = strtod(token, (char**)&token_end);

  // If the one past the last char read by strtod is
  // not the NULL character terminating the string,
  // then parse failed.
  if (*token_end)
    MB_SET_ERR_RET_VAL("Syntax error at line " << line_number() << ": expected number, got \"" << token << "\"", false);

  return true;
}

bool FileTokenizer::get_float_internal(float& result)
{
  double d;
  if (!get_double_internal(d))
    return false;

  result = (float)d;

  return true;
}

bool FileTokenizer::get_long_int_internal(long& result)
{
  // Get a token
  const char *token_end, *token = get_string();
  if (!token)
    return false;

  // Parse token as long
  result = strtol(token, (char**)&token_end, 0);

  // If the one past the last char read by strtol is
  // not the NULL character terminating the string,
  // then parse failed.
  if (*token_end)
    MB_SET_ERR_RET_VAL("Syntax error at line " << line_number() << ": expected number, got \"" << token << "\"", false);

  return true;
}

bool FileTokenizer::get_byte_internal(unsigned char& result)
{
  long i;
  if (!get_long_int_internal(i))
    return false;

  result = (unsigned char)i;
  if (i != (long)result)
    MB_SET_ERR_RET_VAL("Numeric overflow at line " << line_number(), false);

  return true;
}

bool FileTokenizer::get_short_int_internal(short& result)
{
  long i;
  if (!get_long_int_internal(i))
    return false;

  result = (short)i;
  if (i != (long)result)
    MB_SET_ERR_RET_VAL("Numeric overflow at line " << line_number(), false);

  return true;
}

bool FileTokenizer::get_integer_internal(int& result)
{
  long i;
  if (!get_long_int_internal(i))
    return false;

  result = (int)i;
  if (i != (long)result)
    MB_SET_ERR_RET_VAL("Numeric overflow at line " << line_number(), false);

  return true;
}

bool FileTokenizer::get_boolean_internal(bool& result)
{
  // Get a token
  const char *token = get_string();
  if (!token)
    return false;

  if (token[1] || (token[0] != '0' && token[0] != '1'))
    MB_SET_ERR_RET_VAL("Syntax error at line " << line_number() << ": expected 0 or 1, got \"" << token << "\"", false);

  result = token[0] == '1';

  return true;
}

bool FileTokenizer::get_floats(size_t count, float* array)
{
  for (size_t i = 0; i < count; ++i) {
    if (!get_float_internal(*array))
      return false;
    ++array;
  }

  return true;
}

bool FileTokenizer::get_doubles(size_t count, double* array)
{
  for (size_t i = 0; i < count; ++i) {
    if (!get_double_internal(*array))
      return false;
    ++array;
  }

  return true;
}

bool FileTokenizer::get_bytes(size_t count, unsigned char* array)
{
  for (size_t i = 0; i < count; ++i) {
    if (!get_byte_internal(*array))
      return false;
    ++array;
  }

  return true;
}

bool FileTokenizer::get_short_ints(size_t count, short* array)
{
  for (size_t i = 0; i < count; ++i) {
    if (!get_short_int_internal(*array))
      return false;
    ++array;
  }

  return true;
}

bool FileTokenizer::get_integers(size_t count, int* array)
{
  for (size_t i = 0; i < count; ++i) {
    if (!get_integer_internal(*array))
      return false;
    ++array;
  }

  return true;
}

bool FileTokenizer::get_long_ints(size_t count, long* array)
{
  for (size_t i = 0; i < count; ++i) {
    if (!get_long_int_internal(*array))
      return false;
    ++array;
  }

  return true;
}

bool FileTokenizer::get_booleans(size_t count, bool* array)
{
  for (size_t i = 0; i < count; ++i) {
    if (!get_boolean_internal(*array))
      return false;
    ++array;
  }

  return true;
}

void FileTokenizer::unget_token()
{
  if (nextToken - buffer < 2)
    return;

  --nextToken;
  *nextToken = lastChar;
  --nextToken;
  while (nextToken > buffer && *nextToken)
    --nextToken;

  if (!*nextToken)
    ++nextToken;

  lastChar = '\0';
}

bool FileTokenizer::match_token(const char* str, bool print_error)
{
  // Get a token
  const char *token = get_string();
  if (!token)
    return false;

  // Check if it matches
  if (0 == strcmp(token, str))
    return true;

  // Construct error message
  if (print_error)
    MB_SET_ERR_CONT("Syntax error at line " << line_number() << ": expected \"" << str << "\", got \"" << token << "\"");

  return false;
}

int FileTokenizer::match_token(const char* const* list, bool print_error)
{
  // Get a token
  const char *token = get_string();
  if (!token)
    return 0;

  // Check if it matches any input string
  const char* const* ptr;
  for (ptr = list; *ptr; ++ptr) {
    if (0 == strcmp(token, *ptr))
      return ptr - list + 1;
  }

  if (!print_error)
    return 0;

  // No match, constuct error message
  std::string message("Parsing error at line ");
  char lineno[16];
  sprintf(lineno, "%d", line_number());
  message += lineno;
  message += ": expected one of {";
  for (ptr = list; *ptr; ++ptr) {
    message += " ";
    message += *ptr;
  }
  message += " } got \"";
  message += token;
  message += "\"";
  MB_SET_ERR_CONT(message.c_str());

  return 0;
}

bool FileTokenizer::get_newline(bool report_error)
{
  if (lastChar == '\n') {
    lastChar = ' ';
    ++lineNumber;
    return true;
  }

  // Loop until either we a) find a newline, b) find a non-whitespace
  // character or c) reach the end of the file.
  for (;;) {
    // If the buffer is empty, read more.
    if (nextToken == bufferEnd) {
      size_t count = fread(buffer, 1, sizeof(buffer), filePtr);
      if (0 == count) {
        if (eof())
          MB_SET_ERR_RET_VAL("File truncated at line " << line_number(), false);
        else
          MB_SET_ERR_RET_VAL("I/O Error", false);
      }

      nextToken = buffer;
      bufferEnd = buffer + count;
    }

    // If the current character is not a space, the we've failed.
    if (!isspace(*nextToken))
      if (report_error)
        MB_SET_ERR_RET_VAL("Expected newline at line " << line_number(), false);

    // If the current space character is a newline,
    // increment the line number count.
    if (*nextToken == '\n') {
      ++lineNumber;
      ++nextToken;
      lastChar = ' ';
      return true;
    }
    ++nextToken;
  }

  return false;
}

bool FileTokenizer::get_binary(size_t size, void* mem)
{
  // If data in buffer
  if (nextToken != bufferEnd) {
    // If requested size is less than buffer contents,
    // just pass back part of the buffer
    if (bufferEnd - nextToken <= (int)size) {
      memcpy(mem, nextToken, size);
      nextToken += size;
      return true;
    }

    // Copy buffer contents into memory and clear buffer
    memcpy(mem, nextToken, bufferEnd - nextToken);
    size -= bufferEnd - nextToken;
    mem = reinterpret_cast<char*>(mem) + (bufferEnd - nextToken);
    nextToken = bufferEnd;
  }

  // Read any additional data from file
  return size == fread(mem, 1, size, filePtr);
}

} // namespace moab
