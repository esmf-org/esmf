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

#ifndef MB_CART_VECT_HPP
#define MB_CART_VECT_HPP

#include <cmath>
#include <iosfwd>

namespace moab {


/**
 * \brief Cartesian Vector
 * \author Jason Kraftcheck
 * \date July, 2006
 */
class CartVect
{
  private:
    double d[3];

  public:

    inline CartVect()
      { }
      /**Initialze all three values to same scalar (typically zero)*/
    explicit inline CartVect( double v )
      { d[0] = d[1] = d[2] = v; }
    inline CartVect( double i, double j, double k )
      { d[0] = i; d[1] = j; d[2] = k; }
      /**Initialze from array*/
    explicit inline CartVect( const double a[3] )
      { d[0] = a[0]; d[1] = a[1]; d[2] = a[2]; }
    inline CartVect& operator=( const double v[3] )
      { d[0]= v[0]; d[1] = v[1]; d[2] = v[2]; return *this; }

    inline double& operator[]( unsigned i )
      { return d[i]; }
    inline double operator[]( unsigned i ) const
      { return d[i]; }

    inline CartVect& operator+=( const CartVect& v )
      { d[0] += v.d[0]; d[1] += v.d[1]; d[2] += v.d[2]; return *this; }
    inline CartVect& operator-=( const CartVect& v )
      { d[0] -= v.d[0]; d[1] -= v.d[1]; d[2] -= v.d[2]; return *this; }
      /** Assign cross product to this */
    inline CartVect& operator*=( const CartVect& v );

    inline CartVect& operator+=( double s )
      { d[0] += s; d[1] += s; d[2] += s; return *this; }
    inline CartVect& operator-=( double s )
      { d[0] -= s; d[1] -= s; d[2] -= s; return *this; }
    inline CartVect& operator*=( double s )
      { d[0] *= s; d[1] *= s; d[2] *= s; return *this; }
    inline CartVect& operator/=( double s )
      { d[0] /= s; d[1] /= s; d[2] /= s; return *this; }

    inline double length() const; //!< vector length

    inline double length_squared() const;

    inline void normalize(); //!< make unit length

    inline void flip(); //!< flip direction

      /** per-element scalar multiply (this[0] *= v[0], this[1] *= v[1], ...) */
    inline void scale( const CartVect& v )
      { d[0] *= v.d[0]; d[1] *= v.d[1]; d[2] *= v.d[2]; }

      // get pointer to array of three doubles
    inline double* array()
      { return d; }
    inline const double* array() const
      { return d; }

      /** initialize array from this */
    inline void get( double v[3] ) const
      { v[0] = d[0]; v[1] = d[1]; v[2] = d[2]; }
};

inline CartVect operator+( const CartVect& u, const CartVect& v )
  { return CartVect( u[0] + v[0], u[1] + v[1], u[2] + v[2] ); }

inline CartVect operator-( const CartVect& u, const CartVect& v )
  { return CartVect( u[0] - v[0], u[1] - v[1], u[2] - v[2] ); }

/** cross product */
inline CartVect operator*( const CartVect& u, const CartVect& v )
{
  return CartVect( u[1] * v[2] - u[2] * v[1],
                     u[2] * v[0] - u[0] * v[2],
                     u[0] * v[1] - u[1] * v[0] );
}

//! Dot product
inline double operator%( const CartVect& u, const CartVect& v )
  { return u[0] * v[0] + u[1] * v[1] + u[2] * v[2]; }

inline CartVect& CartVect::operator*=( const CartVect& v )
  { return *this = *this * v; }

inline double CartVect::length() const
  { return std::sqrt( *this % *this ); }

inline double CartVect::length_squared() const
  { return d[0]*d[0] + d[1]*d[1] + d[2]*d[2]; }

inline void CartVect::normalize()
  { *this /= length(); }

inline void CartVect::flip()
  { d[0] = -d[0]; d[1] = -d[1]; d[2] = -d[2]; }

//! Interior angle between two vectors
inline double angle( const CartVect& u, const CartVect& v )
  { return std::acos( (u % v) / std::sqrt((u % u) * (v % v)) ); }

inline CartVect operator-( const CartVect& v )
  { return CartVect( -v[0], -v[1], -v[2] ); }
inline CartVect operator+( const CartVect& v, double s )
  { return CartVect( v[0] + s, v[1] + s, v[2] + s ); }
inline CartVect operator-( const CartVect& v, double s )
  { return CartVect( v[0] - s, v[1] - s, v[2] - s ); }
inline CartVect operator*( const CartVect& v, double s )
  { return CartVect( v[0] * s, v[1] * s, v[2] * s ); }
inline CartVect operator/( const CartVect& v, double s )
  { return CartVect( v[0] / s, v[1] / s, v[2] / s ); }
inline CartVect operator+( double s, const CartVect& v )
  { return CartVect( v[0] + s, v[1] + s, v[2] + s ); }
inline CartVect operator-( double s, const CartVect& v )
  { return CartVect( v[0] - s, v[1] - s, v[2] - s ); }
inline CartVect operator*( double s, const CartVect& v )
  { return CartVect( v[0] * s, v[1] * s, v[2] * s ); }

//! Get unit vector in same direction
inline CartVect unit( const CartVect& v )
{
  const double len = v.length();
  return CartVect( v[0] / len, v[1] / len, v[2] / len );
}


std::ostream& operator<<( std::ostream& s, const CartVect& v );

} // namespace moab

#endif
