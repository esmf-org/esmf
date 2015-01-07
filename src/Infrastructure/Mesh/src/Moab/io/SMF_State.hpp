#ifndef GFXSMF_STATE_INCLUDED // -*- C++ -*-
#define GFXSMF_STATE_INCLUDED

#include "AffineXform.hpp"
#include <string>
#include <vector>

namespace moab {

struct SMF_ivars
{
    int next_vertex;
    int next_face;
};

class SMF_State
{
private:
    //
    // Standard state variables
    int first_vertex;
    int vertex_correction;
    AffineXform xform;

public:
    SMF_State(const SMF_ivars& ivar,SMF_State *next = 0);

    void set_vertex_correction(int i);
    void inc(const char *var, int delta=1);
    void dec(const char *var, int delta=1);

    void mmult(const AffineXform&);
    void mload(const AffineXform&);

    void vertex(double v[3]);
    void normal(double n[3]);
    void face(int *, const SMF_ivars& ivar);
};

} // namespace moab

// GFXSMF_STATE_INCLUDED
#endif
