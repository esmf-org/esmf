

#include "SMF_State.hpp"
#include <cstring>
#include <cstdlib>

//static inline int streq(const char *a,const char *b) { return std::strcmp(a,b)==0; }

namespace moab {

SMF_State::SMF_State(const SMF_ivars& ivar, SMF_State *next)
{
    first_vertex = ivar.next_vertex;
    if( next )
    {
	vertex_correction = next->vertex_correction;
	xform = next->xform;
    }
    else
    {
	vertex_correction = 0;
        AffineXform identity;
	xform = identity;
    }

}

void SMF_State::vertex(double v[3])
{
    xform.xform_point(v);
}

void SMF_State::normal(double nrm[3])
{
    xform.xform_vector(nrm);
}

void SMF_State::face( int * verts, const SMF_ivars& ivar)
{
    for(int i=0; i<3; i++)
    {
	if( verts[i] < 0 )
	    verts[i] += ivar.next_vertex;
	else
	    verts[i] += vertex_correction + (first_vertex - 1);
    }
}

void SMF_State::set_vertex_correction( int i )
  { vertex_correction = i; }

void SMF_State::mmult(const AffineXform &M)
{
    // initially, we tried this:
    // xform.accumulate(M);
    // maybe we should do M.accumulate(xform) 
    AffineXform tmp=M;
    tmp.accumulate(xform);
    xform = tmp;
}

void SMF_State::mload(const AffineXform& M)
{
    xform = M;
}

} // namespace moab
