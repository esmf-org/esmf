#include <iostream>
#include <sstream>
#include <iomanip>
#include <cstdlib>
#include <list>
#include <limits>
#include <set>

#include <assert.h>
#include <string.h>

#include "moab/MOABConfig.h"
#include "moab/ProgOptions.hpp"
#ifdef MOAB_HAVE_MPI
# include "moab_mpi.h"
#endif


enum OptType {
  FLAG = 0,
  INT, 
  REAL, 
  STRING, 
  INT_VECT
};


template <typename T> inline
static OptType get_opt_type();


template<> OptType get_opt_type<void             >(){ return FLAG;     }
template<> OptType get_opt_type<int              >(){ return INT;      }
template<> OptType get_opt_type<double           >(){ return REAL;     }
template<> OptType get_opt_type<std::string      >(){ return STRING;   }
template<> OptType get_opt_type<std::vector<int> >(){ return INT_VECT; }

class ProgOpt{

  std::string shortname, longname;
  std::vector< std::string > args;
  OptType type;
  void* storage;
  int flags;
  ProgOpt* cancel_opt;

  const char* get_argstring() const { 
    switch( type ){
    case INT:
      return "int";
    case INT_VECT:
      return "ints";
    case REAL:
      return "val";
    case FLAG:
      return "";
    default:
      return "arg";
    }
  }

public:
  ProgOpt( const std::string& longname_p, const std::string& shortname_p, int flags_p, OptType t = FLAG ):
    shortname( shortname_p ), longname( longname_p ), type(t), 
    storage(NULL), flags(flags_p), cancel_opt(NULL)
  {}
 
  friend class ProgOptions;
};

ProgOptions::ProgOptions( const std::string& helpstring, const std::string& briefhelp ) :
    expect_optional_args(false), optional_args_position(0), max_optional_args(0)
{
  brief_help = briefhelp;
  if (!helpstring.empty())
    main_help.push_back( helpstring );
 addOpt<void>( "help,h", "Show full help text", help_flag );
}

ProgOptions::~ProgOptions(){
  for( std::vector<help_line>::iterator i = option_help_strings.begin();
       i != option_help_strings.end(); ++ i )
  {
    if( (*i).first ){ delete (*i).first; }
  }

  for( std::vector<help_line>::iterator i = arg_help_strings.begin();
       i != arg_help_strings.end(); ++ i )
  {
    delete (*i).first;
  }
}


void ProgOptions::get_namestrings( const std::string& namestring, 
                                   std::string* longname, std::string* shortname )
{
  *shortname = "";
  *longname = namestring;

  size_t idx = namestring.find_first_of(',');
  if( idx != namestring.npos ){
    *longname = namestring.substr(0, idx);
    *shortname = namestring.substr( idx+1, namestring.npos );
  }

  
}

void ProgOptions::setVersion( const std::string& version_string, bool addFlag ){
  progversion = version_string;
  if( addFlag ){
    addOpt<void>( "version", "Print version number and exit", version_flag );
  }
}



template < typename T >
void ProgOptions::addOpt( const std::string& namestring, const std::string& helpstring, 
			  T* value, int flags ){

  std::string shortname, longname;
  get_namestrings( namestring, &longname, &shortname );
  
  if (flags & int_flag) { // short name is implicit for this flag
    if (!shortname.empty()) 
      error( "Requested short name with int_flag option" );
    if (get_opt_type<T>() != INT)
      error( "Requested int_flag for non-integer option" );
    if (!number_option_name.empty()) 
      error( "Requested int_flag for multiple options" );
    number_option_name = longname;
  }

  ProgOpt* opt = new ProgOpt( longname, shortname, flags, get_opt_type<T>() );
  if( value ) opt->storage = value;


  if(longname.length())  long_names[longname] = opt;
  if(shortname.length()) short_names[shortname] = opt;

  help_line help = std::make_pair( opt, helpstring );
  option_help_strings.push_back( help );

  if( flags & add_cancel_opt ){
    std::string flag = "no-" + (longname.length() ? longname : shortname );
    ProgOpt* cancel_opt = new ProgOpt( flag, "", flags ^ ProgOptions::store_false, FLAG );
    if (value) cancel_opt->storage = value;
    
    cancel_opt->cancel_opt = opt;
    long_names[flag] = cancel_opt;
    std::string clear_helpstring = "Clear previous " + flag.substr(3,flag.npos) + " flag";
    help = std::make_pair( cancel_opt, clear_helpstring );
    option_help_strings.push_back( help );
  }
}


template < typename T >
void ProgOptions::addRequiredArg( const std::string& helpname, 
                                  const std::string& helpstring, 
                                  T* value,
                                  int flags ){
  
  OptType type = get_opt_type<T>();

  ProgOpt* opt = new ProgOpt( helpname, "", flags,  type );
  if( value ) opt->storage = value;
  help_line help = std::make_pair( opt, helpstring );
  arg_help_strings.push_back( help );
  required_args[helpname] = opt;
}

template < typename T >
void ProgOptions::addOptionalArgs( unsigned max_count, 
                                   const std::string& helpname, 
                                   const std::string& helpstring,
                                   int flags )
{
    // If there was a previous one, we need to remove it
    // because there can be only one.  If we didn't remove
    // the old one then it would be treated as a required arg.
  if (expect_optional_args) {
    std::map<std::string, ProgOpt*>::iterator iter;
    iter = required_args.find( arg_help_strings[optional_args_position].second );
    assert(iter != required_args.end());
    delete iter->second;
    required_args.erase(iter);
    arg_help_strings.erase( arg_help_strings.begin() + optional_args_position );
  }

  expect_optional_args = true;
  optional_args_position = arg_help_strings.size();
  max_optional_args = max_count;
  addRequiredArg<T>( helpname, helpstring, 0, flags );
}


void ProgOptions::addOptionHelpHeading( const std::string& s ){
  option_help_strings.push_back( std::make_pair( (ProgOpt*)NULL, s) );
}

void ProgOptions::printVersion( std::ostream& out ){
  out << progversion << std::endl;
}

void ProgOptions::printHelp( std::ostream& out ){
  
  /* Print introductory help text */
  if (!brief_help.empty())
    out << brief_help << std::endl;
  for( std::vector<std::string>::iterator i = main_help.begin(); i!= main_help.end(); ++i ){
    if( (*i).length() ){
      out << std::endl << *i << std::endl;
    }
  }

  printUsage( out );

  // max number of characters to pad argument/option names with
  // options with long names may exceed this, but will appear out of alignment in help text
  const int max_padding = 20;

  /* List required arguments, with help text */
  if( arg_help_strings.size() > 0 ){
    
    int max_arg_namelen = 0;
    
    for( std::vector<help_line>::iterator i = arg_help_strings.begin();
         i != arg_help_strings.end(); ++i )
      {
        max_arg_namelen = std::max( max_arg_namelen, (int)((*i).first->longname.length()) );
      }
    
    max_arg_namelen = std::min( max_arg_namelen+3, max_padding );

    out << "Arguments: " << std::endl;
    
    for( std::vector<help_line>::iterator i = arg_help_strings.begin();
         i != arg_help_strings.end(); ++i )
      {
        ProgOpt* option = (*i).first;
        std::string& info = (*i).second;

        std::stringstream s;
        s << "  " << option->longname;
        out << std::setw(max_arg_namelen) << std::left << s.str();
        out << ": " << info << std::endl;
        
      }
  }
    
  /* List options, with help text */
  out << "Options: " << std::endl;
  int max_option_prefix_len = 0;

  for( std::vector<help_line>::iterator i = option_help_strings.begin();
       i != option_help_strings.end(); ++ i )
  {
    ProgOpt* option = (*i).first;
    std::string& info = (*i).second;

    if( option ){

      if( max_option_prefix_len == 0 ){
        // iterate ahead in the option list to determine whitespace padding
        // stop if (*j).first is NULL, which indicates a help header message 
        for( std::vector<help_line>::iterator j = i; j!=option_help_strings.end() && (*j).first; ++j ){
          int len = get_option_usage_prefix( *((*j).first) ).length();
          max_option_prefix_len = std::max (max_option_prefix_len, len);
        }
      }
      max_option_prefix_len = std::min( max_option_prefix_len, max_padding );
      std::string option_prefix = get_option_usage_prefix( *option );

      out << std::setw(max_option_prefix_len) << std::left <<  option_prefix; 
      out << ": ";
    }
    else{ 
      // no option: this is a help header.  Reset max name length.
      max_option_prefix_len = 0;
    }
    out << info << std::endl;
  }
}

std::string ProgOptions::get_option_usage_prefix( const  ProgOpt& option ){
  bool has_shortname = option.shortname.length() > 0;
  bool has_longname  = option.longname.length() > 0;  
  std::string argstr = option.get_argstring();

  std::stringstream s;
  s << "  ";
  if( has_shortname ){
    
    s << "-" << option.shortname;
    if( has_longname ){ s << " "; }
    
  }
  else if ( option.flags & int_flag ) {
    
    s << "-<n>";
    if( has_longname ){ s << " "; }
    
  }
  if( has_longname ){
    
    if( has_shortname ) s << "[";
    s << "--" << option.longname; 
    if( has_shortname ) s << "]"; 
    
  }
  
  if( argstr.length() ) s << " <" << argstr << ">";
  return s.str();
}

void ProgOptions::printUsage( std::ostream& out ){

  out << "Usage: " << progname << " --help | [options] ";

  for (size_t i = 0 ; i < arg_help_strings.size(); ++i)
  {
    if (!expect_optional_args || i != optional_args_position) 
      out << '<' << arg_help_strings[i].first->longname << "> "; 
    else if (0 == max_optional_args || max_optional_args > 3)
      out << "[<" << arg_help_strings[i].first->longname << "> ...] ";
    else if (1 == max_optional_args)
      out << "[" << arg_help_strings[i].first->longname << "] ";
    else for (unsigned j = 0; j < max_optional_args; ++j)
      out << "[" << arg_help_strings[i].first->longname << (j+1) << "] ";
  }
    
  out << std::endl;

}


ProgOpt* ProgOptions::lookup( const std::map<std::string, ProgOpt* >& table, const std::string& arg ){
  std::map<std::string, ProgOpt*>::const_iterator it = table.find( arg );
  if (it != table.end())
    return it->second;
  else if (&table == &short_names && 
           arg.size() == 1 && 
           isdigit(arg[0]) &&
           !number_option_name.empty() &&
           (it = long_names.find(number_option_name)) != long_names.end())
    return it->second;
  else
    return 0;
}

ProgOpt* ProgOptions::lookup_option( const std::string& namestring ){
  std::string longname, shortname;
  get_namestrings( namestring, &longname, &shortname );
  
  ProgOpt* opt = lookup( long_names, longname );
  if( !opt ) opt = lookup( short_names, shortname );
  
  if( !opt ){
    error( "Invalid option: " + namestring );
  }
  
  return opt;
}

void ProgOptions::error( const std::string& err ){
  std::cerr << "Error: " << err << "\n"<< std::endl;;
  printUsage( std::cerr );
  std::cerr << std::endl;
  if (getenv("MOAB_PROG_OPT_ABORT"))
    abort();
  std::exit( EXIT_FAILURE );
}

// Copied from convert.cpp
// Parse list of integer ranges
// e.g. 1,2,5-10,12
static
bool parse_int_list( const char* string, std::vector<int>& results )
{
  bool okay = true;
  char* mystr = strdup( string );
  for (const char* ptr = strtok(mystr, ", \t"); ptr; ptr = strtok(0,", \t"))
  {
    char* endptr;
    long val = strtol( ptr, &endptr, 0 );
    if (endptr == ptr) {
      std::cerr << "Not an integer: \"" << ptr << '"' << std::endl;
      okay = false;
      break;
    }
    
    long val2 = val;
    if (*endptr == '-') {
      const char* sptr = endptr+1;
      val2 = strtol( sptr, &endptr, 0 );
      if (endptr == sptr) {
        std::cerr << "Not an integer: \"" << sptr << '"' << std::endl;
        okay = false;
        break;
      }
      if (val2 < val) {
        std::cerr << "Invalid id range: \"" << ptr << '"' << std::endl;
        okay = false;
        break;
      }
    }
    
    if (*endptr) {
      okay = false;
      break;
    }
    
    for (; val <= val2; ++val)
      results.push_back( (int)val );

  }
  
  free( mystr );
  return okay;    
}


// Copied from convert.cpp
// Replace '%' with MPI rank iff compiled with MPI
static
std::string do_rank_subst( const std::string& s )
{
#ifndef MOAB_HAVE_MPI
  return s;
#else
  int rank, size;
  if (MPI_SUCCESS != MPI_Comm_rank( MPI_COMM_WORLD, &rank )
   || MPI_SUCCESS != MPI_Comm_size( MPI_COMM_WORLD, &size ))
    return s;
  int width = 1;
  while (size > 10) {
    size /= 10;
    width++;
  }

  size_t j = s.find( '%' );
  if (j == std::string::npos) 
    return s;
  
  std::ostringstream st;
  st << std::setfill('0');
  st << s.substr( 0, j );
  st << rank;
  
  size_t i;
  while ((i = s.find( '%', j+1)) != std::string::npos) {
    st << s.substr( j, i - j );
    st << std::setw(width) << rank;
    j = i;
  }
  st << s.substr( j+1 );
  return st.str();
#endif
}

/**
 * Check the input to a given option for correctness, converting it to its expected type (e.g. int)
 * and storing the result to target, if target is non-NULL.
 * @param option Used only in error messages to state which option could not be successfully converted
 * @param arg_idx If non-NULL, evaluate the (*arg_idx)'th item in opt's args list
 */
bool ProgOptions::evaluate( const ProgOpt& opt, void* target, const std::string& option, unsigned* arg_idx ){

  unsigned idx = arg_idx ? *arg_idx : opt.args.size()-1;

  switch( opt.type ){
  case FLAG:
    error("Cannot evaluate a flag");
    break;
  case INT:
    {
      int temp;
      int* i = target ? reinterpret_cast<int*>(target) : &temp;
      if( opt.args.size() < 1 ){
	error( "Missing argument to " + option + " option");
      }
      const char* arg = opt.args.at(idx).c_str();
      char* p;
      *i = std::strtol( arg, &p, 0 );
      if( *p != '\0' ){ error("Bad integer argument '" + opt.args.at(idx) + "' to " + option + " option."); }
      return true;
    }
  case REAL:
    {
      double temp;
      double* i = target ? reinterpret_cast<double*>(target) : &temp;
      if( opt.args.size() < 1 ){
	error( "Missing argument to " + option + " option");
      }
      const char* arg = opt.args.at(idx).c_str();
      char* p;
      *i = std::strtod( arg, &p );
      if( *p != '\0' ){ error("Bad real argument '" + opt.args.at(idx) + "' to " + option + " option."); }
      return true;
    
    }
  
  case STRING:
    {
      std::string temp;
      std::string* i = target ? reinterpret_cast<std::string*>(target) : &temp;
      if( opt.args.size() < 1 ){
	error( "Missing argument to " + option + " option");
      }
      if (opt.flags & rank_subst)
        *i = do_rank_subst( opt.args.at(idx) );
      else
        *i = opt.args.at(idx);
      return true;
    }
  
  case INT_VECT:
    {
      std::vector<int> temp;
      std::vector<int>* i = target ? reinterpret_cast<std::vector<int>*>(target) : &temp;
      if(!parse_int_list( opt.args.at(idx).c_str(), *i ))
        error( "Bad integer list '" + opt.args.at(idx) + "' to " + option + " option.");
      return true;
    }

  }

  return false;
}

template <typename T>
bool ProgOptions::getOpt( const std::string& namestring, T* t ){
 
  ProgOpt* opt = lookup_option( namestring );

  if( get_opt_type<T>() != opt->type ){
    error( "Option '" + namestring + "' looked up with incompatible type" );
  }
  
  // This call to evaluate is inefficient, because opt was already evaluated when it was parsed.
  if( opt->args.size() ){
    if (t)
      evaluate( *opt, t, "" );
    return true;
  }
  else return false;

}

template <typename T>
void ProgOptions::getOptAllArgs( const std::string& namestring, std::vector<T>& values ){
  ProgOpt* opt = lookup_option( namestring );

    // special case: if user asks for list of int, but argument
    // was INT_VECT, concatenate all lists
  if (get_opt_type<T>() == INT && opt->type == INT_VECT) {
    for (unsigned i = 0; i < opt->args.size(); ++i)
      evaluate( *opt, &values, "", &i );
    return;
  }
  
  if( get_opt_type<T>() != opt->type ){
    error( "Option '" + namestring + "' looked up with incompatible type" );
  }
  
  values.resize( opt->args.size() );

  // These calls to evaluate are inefficient, because the arguments were evaluated when they were parsed
  for( unsigned i = 0; i < opt->args.size(); ++i ){
    evaluate( *opt, &(values[i]), "", &i );
  }

}

int ProgOptions::numOptSet( const std::string& namestring ){
  std::string longname, shortname;
  get_namestrings( namestring, &longname, &shortname );
  
  ProgOpt* opt = lookup( long_names, longname );
  if( !opt ) opt = lookup( short_names, shortname );

  if( !opt ){
    error( "Could not look up option: " + namestring );
  }
  
  return opt->args.size();

}

template <typename T>
T ProgOptions::getReqArg( const std::string& namestring ){
  
  ProgOpt* opt = lookup( required_args, namestring );
  
  if( !opt ){
    error( "Could not look up required arg: " + namestring );
  }
  
  // if parseProgramOptions succeeded, we can assume each required arg has a value,
  // so calling evaluate is valid
  T value;
  evaluate( *opt, &value, "" );
  return value; 

}

template <typename T>
void ProgOptions::getArgs( const std::string& namestring, 
                           std::vector<T>& values )
{
  ProgOpt* opt = lookup( required_args, namestring );
  
  if( !opt ){
    error( "Could not look up required arg: " + namestring );
  }
  
  
  if( get_opt_type<T>() != opt->type ){
    error( "Option '" + namestring + "' looked up with incompatible type" );
  }
  
  values.resize( opt->args.size() );

  // These calls to evaluate are inefficient, because the arguments were evaluated when they were parsed
  for( unsigned i = 0; i < opt->args.size(); ++i ){
    evaluate( *opt, &(values[i]), "", &i );
  }

}
  

// Process parsed option.
// Returns true if value is still expected
// Should never return true if optional value is passed
// \param arg Used for error messages only
bool ProgOptions::process_option( ProgOpt* opt, std::string arg, const char* value )
{
  if( !opt ){
    if (arg == "--manpage") {
      write_man_page(std::cout);
      exit(0);
    }
    
    error ("Unknown option: " + arg );
  }

  if( opt->flags & help_flag ){
    printHelp( std::cout );
    exit( EXIT_SUCCESS );
  }

  if( opt->flags & version_flag ){
    printVersion( std::cout );
    exit( EXIT_SUCCESS );
  }
  
  if (opt->type != FLAG) {
    if (!value)
      return true;
    
    opt->args.push_back( value );
    evaluate( *opt, opt->storage, arg );
  }
  else {
    if (value) {
      error( "Unexpected value for flag: " + arg );
    }

    // do flag operations
    if( opt->cancel_opt ){ opt->cancel_opt->args.clear(); }
    if( opt->storage ){
      *static_cast<bool*>(opt->storage) = ( opt->flags & store_false ) ? false : true;            
    }
    opt->args.push_back(""); 
  }
  
  return false;
}


void ProgOptions::parseCommandLine( int argc, char* argv[] ){
  const char* name = strrchr(argv[0],'/');
  if (name)
    this->progname = ++name;
  else
    this->progname = argv[0];

  std::vector<const char*> args;
  std::list<ProgOpt*> expected_vals;
  bool no_more_flags = false;                    

    // Loop over all command line arguments
  for( int i = 1; i < argc; ++i ) {
    std::string arg(argv[i]);
    if (arg.empty())
      continue;
    
    if (!expected_vals.empty()) {
      ProgOpt* opt = expected_vals.front();
      expected_vals.pop_front();
      assert(opt->type != FLAG);
      opt->args.push_back( arg );
      evaluate( *opt, opt->storage, arg );
    }
    else if (!no_more_flags && arg[0] == '-') {
      if (arg.length() > 2 && arg[1] == '-') { // long opt
        size_t eq = arg.find_first_of('=');
        if (eq != std::string::npos) {
          ProgOpt* opt = lookup( long_names, arg.substr( 2, eq-2 ) );
          process_option( opt, arg, arg.substr( eq+1 ).c_str() );
        }
        else {
          ProgOpt* opt = lookup( long_names, arg.substr( 2 ) );
          if (process_option( opt, arg )) 
            expected_vals.push_back( opt );
        }
      }
      else if (arg == "--") { // --
        no_more_flags = true;
      }
      else for (size_t f = 1; f < arg.length(); ++f) { // for each short opt
        ProgOpt* opt = lookup( short_names, std::string(1,arg[f]) );
        if (opt && (opt->flags & int_flag) ) {
          const char val[] = { arg[f], 0 };
          process_option( opt, std::string(1,arg[f]), val );
        }
        else if (process_option( opt, std::string(1,arg[f]) ))
          expected_vals.push_back( opt );
      }
    }
    else{ 
      /* arguments */
      args.push_back(argv[i]);
    }
  }/* End loop over inputs */

    // Print error if any missing values
  if (!expected_vals.empty()) {
    error( "Missing value for option: -" + 
           expected_vals.front()->shortname + ",--" + 
           expected_vals.front()->longname );
  }

    // Process non-option arguments
  std::vector<help_line>::iterator arg_help_pos = arg_help_strings.begin();
  std::vector<const char*>::iterator arg_val_pos = args.begin();
  std::vector<help_line>::iterator opt_args_pos = arg_help_strings.end();
  size_t min_required_args = required_args.size();
  size_t max_required_args = required_args.size();
  if (expect_optional_args) {
    min_required_args--;
    if (max_optional_args)
      max_required_args += max_optional_args;
    else
      max_required_args = std::numeric_limits<int>::max();
    opt_args_pos = arg_help_pos + optional_args_position;
  }
  // check valid number of non-flag arguments
  if (args.size() < min_required_args) {
    size_t missing_pos = args.size();
    if (expect_optional_args && missing_pos >= optional_args_position)
      ++missing_pos;
    
    const std::string& missed_arg = arg_help_strings[missing_pos].first->longname; 
    error("Did not find required positional argument: " + missed_arg );
  }
  else if (args.size() > max_required_args) {
    error( "Unexpected argument: " + std::string(args[max_required_args]) );  
  }
  
  // proccess arguments up to the first optional argument
  // (or all arguments if no optional args)
  while (arg_help_pos != opt_args_pos) {
    ProgOpt* opt = arg_help_pos->first;
    ++arg_help_pos;
    opt->args.push_back( *arg_val_pos );
    evaluate( *opt, opt->storage, *arg_val_pos );
    ++arg_val_pos;
  }
  // process any optional args
  if (arg_help_pos != arg_help_strings.end()) {
    assert( arg_help_pos == opt_args_pos );
    size_t num_opt_args = args.size() + 1 - required_args.size();
    ProgOpt* opt = arg_help_pos->first;
    ++arg_help_pos;
    while (num_opt_args--) {
      opt->args.push_back( *arg_val_pos );
      evaluate( *opt, opt->storage, *arg_val_pos );
      ++arg_val_pos;
    }
  }
  // process any remaining args
  while (arg_help_pos != arg_help_strings.end()) {
    assert(arg_val_pos != args.end());
    ProgOpt* opt = arg_help_pos->first;
    ++arg_help_pos;
    opt->args.push_back( *arg_val_pos );
    evaluate( *opt, opt->storage, *arg_val_pos );
    ++arg_val_pos;
  }
  assert(arg_val_pos == args.end());
}

void ProgOptions::write_man_page( std::ostream& s )
{
  // a leading '.' is a control character.  strip it if present.
  std::string lprogname;
  if (progname.empty() || progname[0] != '.')
    lprogname = progname;
  else {
    lprogname = progname.substr(1);
  }

  // Manpage controls:
  // .TH title
  // .SH section
  // .SS subsection
  // .P  paragraph
  // .HP hanging paragraph
  // .B  bold
  // .I  italic
  // .B  bold
  // .I  italic
  // .RS begin indent
  // .RE end indent
  // .RB alternating roman and blold
  // .BR alternating bold and roman

  std::vector<help_line>::iterator it;
  std::set<ProgOpt*> skip_list;

  // start man page
  s << std::endl << ".TH " << lprogname << " 1" << std::endl;

  // write NAME section
  s << std::endl << ".SH NAME" << std::endl
    << ".P " << std::endl
    << lprogname << " \\- ";
  if (brief_help.empty() && !main_help.empty())
    s << main_help.front();
  else
    s << brief_help;
  s << std::endl << std::endl;

  // write SYNOPSIS section
  s << std::endl << ".SH SYNOPSIS" << std::endl
    << ".HP" << std::endl 
    << ".B \"" << lprogname << '"' << std::endl;
  for (it = option_help_strings.begin(); it != option_help_strings.end(); ++it) {
    if (!it->first || 
        skip_list.find( it->first ) != skip_list.end() ||
        it->first->longname == "help")
      continue;
    
    if (it->first->type == FLAG) {
      char c = '[';
      s << ".RB";
      if (!it->first->shortname.empty()) {
        s << ' ' << c << " \"-" << it->first->shortname << '"';
        c = '|';
      }
      if (!it->first->longname.empty()) {
        s << ' ' << c << " \"--" << it->first->longname << '"';
      }
      if (it->first->cancel_opt) {
        skip_list.insert( it->first->cancel_opt );
        if (!it->first->cancel_opt->shortname.empty())
          s << " | \"-" << it->first->cancel_opt->shortname << '"';
        if (!it->first->cancel_opt->longname.empty()) 
          s << " | \"--" << it->first->cancel_opt->longname << '"';
      }
      s << " ]" << std::endl;
    }
    else if (it->first->flags & int_flag) {
      s << ".RB [ - <n>| \"--" << it->first->longname
        << "\" \"=" << it->first->get_argstring() << "]\"" << std::endl;
    }
    else {
      s << ".RB [ ";
      if (!it->first->shortname.empty()) 
        s << "\"-" << it->first->shortname << "\" \"\\ " << it->first->get_argstring();
      if (!it->first->shortname.empty() && !it->first->longname.empty())
        s << "|\" ";
      if (!it->first->longname.empty())
        s << "\"--" << it->first->longname
          << "\" \"=" << it->first->get_argstring();
      s << "]\"" << std::endl;
    }
  }
  for (it = arg_help_strings.begin(); it != arg_help_strings.end(); ++it) {
    if (!it->first)
      continue;
    
    if (!expect_optional_args || (unsigned)(it - arg_help_strings.begin()) != optional_args_position)
      s << it->first->longname << ' ';
    else if (1 == max_optional_args)
      s << '[' << it->first->longname << "] ";
    else 
      s << '[' << it->first->longname << " ...] ";
  }
  s << std::endl;
  s << ".HP" << std::endl 
    << ".B \"" << lprogname << " -h|--help\"" << std::endl;
  
    // write DESCRIPTION section
  s << std::endl << ".SH DESCRIPTION" << std::endl;
  if (main_help.empty())
    s << brief_help << std::endl;
  for (size_t i = 0; i < main_help.size(); ++i) {
    const std::string::size_type n = main_help[i].size();
    std::string::size_type j = 0, k;
    s << std::endl << ".P" << std::endl;
    while (j != n) {
      if (main_help[i][j] == '\n') {
        s << std::endl << ".P" << std::endl;
        ++j;
        continue;
      }
      k = main_help[i].find( "\n", j );
      if (k == std::string::npos)
        k = n;
      if (main_help[i][j] == '.')
        s << '\\';
      s << main_help[i].substr( j, k - j );
      j = k;
    }
  }
  
    // write OPTIONS section
  s << std::endl << ".SH OPTIONS" << std::endl;
  for (it = arg_help_strings.begin(); it != arg_help_strings.end(); ++it) {
    if (it->first) 
      s << ".IP \"" << it->first->longname << '"' << std::endl << it->second << std::endl;
    else
      s << ".SS " << it->first->longname << std::endl;
  }
  for (it = option_help_strings.begin(); it != option_help_strings.end(); ++it) {
    if (!it->first) {
      s << ".SS " << it->second << std::endl;
      continue;
    }
    
    s << ".IP \"";
    if (it->first->longname.empty())
      s << "-" << it->first->shortname;
    else if (it->first->shortname.empty())
      s << "--" << it->first->longname;
    else
      s << "-" << it->first->shortname << ", --" << it->first->longname;
    s << '"' << std::endl << it->second << std::endl;
  }
  s << std::endl;
}


/* Ensure g++ instantiates the template types we expect to use */

#define DECLARE_OPTION_TYPE(T)                                 \
  template void ProgOptions::addOpt<T>( const std::string&, const std::string&, T*, int ); \
  template bool ProgOptions::getOpt<T>( const std::string&, T* ); 

#define DECLARE_VALUED_OPTION_TYPE(T)                          \
  DECLARE_OPTION_TYPE(T)                                       \
  template void ProgOptions::getOptAllArgs<T> (const std::string&, std::vector<T>& ); \
  template void ProgOptions::addRequiredArg<T>( const std::string&, const std::string&, T*, int ); \
  template void ProgOptions::addOptionalArgs<T>( unsigned, const std::string&, const std::string&, int ); \
  template T ProgOptions::getReqArg<T>( const std::string& ); \
  template void ProgOptions::getArgs<T>( const std::string&, std::vector<T>& );
 
DECLARE_OPTION_TYPE(void)
DECLARE_VALUED_OPTION_TYPE(int)
DECLARE_VALUED_OPTION_TYPE(double)
DECLARE_VALUED_OPTION_TYPE(std::string)
DECLARE_VALUED_OPTION_TYPE(std::vector<int>)
