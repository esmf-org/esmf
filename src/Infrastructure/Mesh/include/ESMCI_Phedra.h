
#ifndef ESMCI_PHEDRA
#define ESMCI_PHEDRA

#include "ESMCI_MathUtil.h"


namespace ESMCI {

 extern  bool phedra_debug;

// Structure for holding a polyhedra the consists of a list of polygon faces
// Currently this class assumes that the polygon faces are planar
// Yeah, this class is redundent in how it stores the point information
// because they are stored in multiple faces, but I only expect a few of these to 
// be around at any one time (basically while one tet. is being clipped against another), and
// doing things this way seems more time efficient
// If memory is an issue in the future, optimize for that
class Phedra;

class PhedraFace {
  friend class Phedra;
  
 public:
  //  const static int max_num_pnts=8;
  const static int max_num_pnts=20;
  
 private:
  int num_pnts;
  double pnts[3*max_num_pnts];
  double normal[3]; // put this in a separate list?

 public:  
  PhedraFace(); // Empty
  void set_tri(double *p1, double *p2, double *p3); // set as triangle

  void clip_against_plane(double *p1, double *p2, double *p3);
};


class Phedra {
  // This structure stores the faces of the polyhedra such that they are counter-clockwise looking  
  // from outside the polyhedra through the face to the center of the polyhedra. The normals
  // stored in the structure are outward normals from the surface of the polyhedra. 

  // CONSIDER GETTING RID OF PhedraFace AND JUST PUTTING EVERYTHING IN LISTS HERE
 public:
  //  const static int max_num_faces=8;
  const static int max_num_faces=30;

  int num_faces;
  PhedraFace faces[max_num_faces];

 public:
  Phedra(double *p1, double *p2, double *p3, double *p4);

  Phedra(int num_tri, double *tri);

  void intersect(Phedra &p);

  //  void clip_against_tet(double *p1, double *p2, double *p3, double *p4);

  void clip_against_plane(double *pln_pnt, double *pln_normal);

  void write_to_vtk(const char *filename);
  void write_faces_to_vtk(char *name);

  double calc_volume();

  int get_num_faces() {return num_faces;}

  void calc_min(double *min);

  void calc_max(double *min);

  bool is_empty() {return (num_faces==0);}

  bool is_degenerate() {return (num_faces<4);}
};


struct PntAndAngle {

  PntAndAngle() {
    pnt[0]=0.0;
    pnt[1]=0.0;
    pnt[2]=0.0;
    
    angle=0.0;
  }

  PntAndAngle &operator=(const PntAndAngle &rhs) {
    pnt[0]=rhs.pnt[0];
    pnt[1]=rhs.pnt[1];
    pnt[2]=rhs.pnt[2];
    
    angle=rhs.angle;
    
    return *this;
  }
    
    
    bool operator<(const PntAndAngle &rhs) const {
      return angle < rhs.angle;
    }
  
  void set(double *_pnt, double _angle) {
    pnt[0]=_pnt[0];
    pnt[1]=_pnt[1];
    pnt[2]=_pnt[2];

    angle=_angle;
  }

  void getPnt(double *_pnt) {
    _pnt[0]=pnt[0];
    _pnt[1]=pnt[1];
    _pnt[2]=pnt[2];
  }

  double getAngle() {
    return angle;
  }


private:
  double pnt[3];
  double angle;
}; 


void intersect_convex_2D_3D_cart_poly_w_plane(int num_p, double *p,
                                              double *pln_pnt, double *pln_normal, int *tmp_ioo,
                                              int *num_out, double *out, int *num_ipnt, double *ipnt);

void make_polygon_3Dcart(int num_pnts, double *pnts, double *normal, PntAndAngle *tmp, double *out);

bool intersect_plane_with_line(const double *pln_pnt, const double *pln_normal, const double *l1, const double *l2, double *t);

// Didn't put this in class for now, so that it's easy to remove if we want to move this class. 
Phedra create_phedra_from_elem(const MeshObj *elem, MEField<> *cfield);

}

#endif



