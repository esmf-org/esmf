#ifndef VERTEX_SEQUENCE_HPP
#define VERTEX_SEQUENCE_HPP

#include "EntitySequence.hpp"
#include "SequenceData.hpp"

namespace moab {

class VertexSequence : public EntitySequence
{
public:

  VertexSequence( EntityHandle start,
                  EntityID count,
                  SequenceData* dat )
    : EntitySequence( start, count, dat )
    {}
  
  VertexSequence( EntityHandle start,
                  EntityID count,
                  EntityID data_size )
    : EntitySequence( start, count, new SequenceData( 3, start, start+data_size-1 ) )
    {
      data()->create_sequence_data( X, sizeof(double) );
      data()->create_sequence_data( Y, sizeof(double) );
      data()->create_sequence_data( Z, sizeof(double) );
    } 
  
  virtual ~VertexSequence();
  
  inline ErrorCode get_coordinates( EntityHandle handle,
                                      double& x,
                                      double& y,
                                      double& z ) const;

  inline ErrorCode get_coordinates( EntityHandle handle,
                                      double coords[3] ) const;

  inline ErrorCode get_coordinates_ref( EntityHandle handle,
                                          const double*& x,
                                          const double*& y,
                                          const double*& z ) const;

  inline ErrorCode set_coordinates( EntityHandle entity,
                                      double x, 
                                      double y,
                                      double z );

  inline ErrorCode set_coordinates( EntityHandle entity,
                                      const double xyz[3] );

  inline ErrorCode get_coordinate_arrays( double*& x, 
                                            double*& y, 
                                            double*& z );

  inline ErrorCode get_coordinate_arrays( const double*& x,
                                            const double*& y,
                                            const double*& z ) const;
 
  EntitySequence* split( EntityHandle here );
  
  SequenceData* create_data_subset( EntityHandle start, EntityHandle end ) const;
  
  ErrorCode push_front( EntityID count );
  ErrorCode push_back( EntityID count );
  
  void get_const_memory_use( unsigned long& bytes_per_entity,
                             unsigned long& size_of_sequence ) const;
                             
private:

  enum Coord{ X = 0, Y = 1, Z = 2 };

  inline double* array( Coord coord )
  { 
    return reinterpret_cast<double*>(data()->get_sequence_data( coord ));
  }

  inline const double* array( Coord coord ) const
  { 
    return reinterpret_cast<const double*>(data()->get_sequence_data( coord ));
  }
  
  inline double* x_array() { return array(X); }
  inline double* y_array() { return array(Y); }
  inline double* z_array() { return array(Z); }
  
  inline const double* x_array() const { return array(X); }
  inline const double* y_array() const { return array(Y); }
  inline const double* z_array() const { return array(Z); }
  
  VertexSequence( VertexSequence& split_from, EntityHandle here )
    : EntitySequence( split_from, here )
    {}
};

  
ErrorCode VertexSequence::get_coordinates( EntityHandle handle,
                                             double& x,
                                             double& y,
                                             double& z ) const
{
  EntityID offset = handle - data()->start_handle();
  x = x_array()[offset];
  y = y_array()[offset];
  z = z_array()[offset];
  return MB_SUCCESS;
}

ErrorCode VertexSequence::get_coordinates( EntityHandle handle,
                                             double coords[3] ) const
{
  EntityID offset = handle - data()->start_handle();
  coords[X] = x_array()[offset];
  coords[Y] = y_array()[offset];
  coords[Z] = z_array()[offset];
  return MB_SUCCESS;
}
  

ErrorCode VertexSequence::get_coordinates_ref( EntityHandle handle,
                                                 const double*& x,
                                                 const double*& y,
                                                 const double*& z ) const
{
  EntityID offset = handle - data()->start_handle();
  x = x_array()+offset;
  y = y_array()+offset;
  z = z_array()+offset;
  return MB_SUCCESS;
}

ErrorCode VertexSequence::set_coordinates( EntityHandle entity,
                                             double x, 
                                             double y,
                                             double z )
{
  EntityID offset = entity - data()->start_handle();
  x_array()[offset] = x;
  y_array()[offset] = y;
  z_array()[offset] = z;
  return MB_SUCCESS;
}

ErrorCode VertexSequence::set_coordinates( EntityHandle entity,
                                             const double* xyz )
{
  EntityID offset = entity - data()->start_handle();
  x_array()[offset] = xyz[0];
  y_array()[offset] = xyz[1];
  z_array()[offset] = xyz[2];
  return MB_SUCCESS;
}

ErrorCode VertexSequence::get_coordinate_arrays( double*& x, 
                                                   double*& y, 
                                                   double*& z )
{
  EntityID offset = start_handle() - data()->start_handle();
  x = x_array()+offset;
  y = y_array()+offset;
  z = z_array()+offset;
  return MB_SUCCESS;
}
  
ErrorCode VertexSequence::get_coordinate_arrays( const double*& x,
                                                   const double*& y,
                                                   const double*& z ) const
{
  return get_coordinates_ref( start_handle(), x, y, z ); 
}

} // namespace moab

#endif
