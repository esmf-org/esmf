// $Id: ESMCI_MathUtil.C,v 1.3 2010/09/17 03:13:32 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_ConserveInterp.h>
#include <Mesh/include/ESMCI_Exception.h>
#include <Mesh/include/ESMCI_MeshOBjConn.h>
#include <Mesh/include/ESMCI_MeshUtils.h>
#include <Mesh/include/ESMCI_MEValues.h>
#include <Mesh/include/ESMCI_Polynomial.h>
#include <Mesh/include/ESMCI_MeshField.h>
#include <Mesh/include/ESMCI_MeshTypes.h>
#include <Mesh/include/ESMCI_Ftn.h>
#include <Mesh/include/ESMCI_ParEnv.h>

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>



//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_MathUtil.C,v 1.3 2010/09/17 03:13:32 oehmke Exp $";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

          
namespace ESMCI {

  ///////// File for random math routines that I didn't know where to put ///////////


//// These should eventually be moved elsewhere (perhaps into ESMCI_ShapeFunc.C??) 
void mult(double m[], double v[], double out_v[]) {

  out_v[0]=m[0]*v[0]+m[1]*v[1]+m[2]*v[2];
  out_v[1]=m[3]*v[0]+m[4]*v[1]+m[5]*v[2];
  out_v[2]=m[6]*v[0]+m[7]*v[1]+m[8]*v[2];

}

// returns true if matrix is inverted, false otherwise
bool invert_matrix_3x3(double m[], double m_inv[]) {

  const double det =  m[0] * (m[4]*m[8] - m[5]*m[7])
                     -m[1] * (m[3]*m[8] - m[5]*m[6])
                     +m[2] * (m[3]*m[7] - m[4]*m[6]);
  
  // If det == 0.0 we can't invert
  if (det == 0.0) return false;

  const double deti = 1.0/det;

  m_inv[0] = (m[4]*m[8] - m[5]*m[7]) * deti;
  m_inv[1] = (m[2]*m[7] - m[1]*m[8]) * deti;
  m_inv[2] = (m[1]*m[5] - m[2]*m[4]) * deti;

  m_inv[3] = (m[5]*m[6] - m[3]*m[8]) * deti;
  m_inv[4] = (m[0]*m[8] - m[2]*m[6]) * deti;
  m_inv[5] = (m[2]*m[3] - m[0]*m[5]) * deti;

  m_inv[6] = (m[3]*m[7] - m[4]*m[6]) * deti;
  m_inv[7] = (m[1]*m[6] - m[0]*m[7]) * deti;
  m_inv[8] = (m[0]*m[4] - m[1]*m[3]) * deti;

  return true;
}


// Intersects between the quad q (entries in counterclockwise order)                                       
// and the line determined by the endpoints l1 and l2                       
// returns true if the two intersect and the output variables are valid                                
// outputs p containing the coordinates in the quad and t the coordinate in the line   
// of the intersection.                                                           
// NOTE: the intersection doesn't have to be inside the quad or line for this to return true    
bool intersect_quad_with_line(const double *q, const double *l1, const double *l2, double *p,
			      double *t) {

  double A[3], B[3], C[3], D[3], E[3], F[3];
  double J[3*3], inv_J[3*3];
  double X[3], delta_X[3];

  const double *q0=q;
  const double *q1=q+3;
  const double *q2=q+6;
  const double *q3=q+9;


  // Set some convient variables
  A[0]=q0[0]-q1[0]+q2[0]-q3[0];
  A[1]=q0[1]-q1[1]+q2[1]-q3[1];
  A[2]=q0[2]-q1[2]+q2[2]-q3[2];

  B[0]=q1[0]-q0[0];
  B[1]=q1[1]-q0[1];
  B[2]=q1[2]-q0[2];

  C[0]=q3[0]-q0[0];
  C[1]=q3[1]-q0[1];
  C[2]=q3[2]-q0[2];

  D[0]=l1[0]-l2[0];
  D[1]=l1[1]-l2[1];
  D[2]=l1[2]-l2[2];

  E[0]=q0[0]-l1[0];
  E[1]=q0[1]-l1[1];
  E[2]=q0[2]-l1[2];

  // Initialize answer
  X[0]=0.0;
  X[1]=0.0;
  X[2]=0.0;

  // Do multiple iterations, exiting inside loop if solution is good enough
  for (int i=0; i<100; i++) {

    // Calculate Value of function at X
    F[0]=X[0]*X[1]*A[0]+X[0]*B[0]+X[1]*C[0]+X[2]*D[0]+E[0];
    F[1]=X[0]*X[1]*A[1]+X[0]*B[1]+X[1]*C[1]+X[2]*D[1]+E[1];
    F[2]=X[0]*X[1]*A[2]+X[0]*B[2]+X[1]*C[2]+X[2]*D[2]+E[2];

    // If we're close enough to 0.0 then exit           
    if (F[0]*F[0]+F[1]*F[1]+F[2]*F[2] < 1.0E-20) break;


    // Construct Jacobian
    J[0]=A[0]*X[1]+B[0]; J[1]=A[0]*X[0]+C[0]; J[2]=D[0];
    J[3]=A[1]*X[1]+B[1]; J[4]=A[1]*X[0]+C[1]; J[5]=D[1];
    J[6]=A[2]*X[1]+B[2]; J[7]=A[2]*X[0]+C[2]; J[8]=D[2];

    // Invert Jacobian
    if (!invert_matrix_3x3(J,inv_J)) return false;
    
    // Calculate change in X
    mult(inv_J, F, delta_X);
    
    // Move to next approximation of X
    X[0] = X[0] - delta_X[0];
    X[1] = X[1] - delta_X[1];
    X[2] = X[2] - delta_X[2];
  }

  // Get answer out
  p[0]=X[0];
  p[1]=X[1];
  *t=X[2];

  return true;
}

//// Intersect a line and a tri 
// Intersects between the tri t (entries in counterclockwise order)
// and the line determined by the endpoints l1 and l2 (t=0.0 at l1 and t=1.0 at l2)
// returns true if the two intersect and the output variables are valid
// outputs p containing the coordinates in the tri and t the coordinate in the line
// of the intersection.
// NOTE: the intersection doesn't have to be inside the tri or line for this to return true
bool intersect_tri_with_line(const double *tri, const double *l1, const double *l2, double *p,
			     double *t) {

  double M[3*3], inv_M[3*3];
  double V[3];
  double X[3];

  const double *tri0=tri;
  const double *tri1=tri+3;
  const double *tri2=tri+6;

  // To do intersection just solve the set of linear equations for both
  // Setup M
  M[0]=l1[0]-l2[0]; M[1]=tri1[0]-tri0[0]; M[2]=tri2[0]-tri0[0];
  M[3]=l1[1]-l2[1]; M[4]=tri1[1]-tri0[1]; M[5]=tri2[1]-tri0[1];
  M[6]=l1[2]-l2[2]; M[7]=tri1[2]-tri0[2]; M[8]=tri2[2]-tri0[2];


  // Invert M
  if (!invert_matrix_3x3(M,inv_M)) return false;

  // Set variable holding vector
  V[0]=l1[0]-tri0[0];
  V[1]=l1[1]-tri0[1];
  V[2]=l1[2]-tri0[2];

  // Calculate solution
  mult(inv_M, V, X);

  // Get answer out
  *t=X[0];
  p[0]=X[1];
  p[1]=X[2];

  return true;
}


  // Num is the number of vertices in the polygon
  // coords is of size 2*num
  double area_of_flat_2D_polygon(int num, double *coords) {

    double area=0;
    for(int i=0; i<num; i++) {
      int ip1=(i+1)%num; // i plus 1 mod the size to wrap at end

      area += (coords[2*i]*coords[2*ip1+1]-coords[2*ip1]*coords[2*i+1]);
    }
    
    return 0.5*area;

  }



//    w
//    | \
//    |   \
//    |     \
//    u------v
//    ^
//    | --- angle being calculated
//      
  double angle(double *u, double *v, double *w) {
  
  double cosa=u[0]*v[0]+u[1]*v[1]+u[2]*v[2];

  double cosb=u[0]*w[0]+u[1]*w[1]+u[2]*w[2];

  double cosc=v[0]*w[0]+v[1]*w[1]+v[2]*w[2];

  //  printf("a=%f b=%f c=%f \n",cosa,cosb,cosc);

  double sina=sqrt(1.0-cosa*cosa);

  double sinb=sqrt(1.0-cosb*cosb);

#if 0
    if (debug) {
        printf("u=[%f %f %f] ",u[0],u[1],u[2]);
        printf("v=[%f %f %f] ",v[0],v[1],v[2]);
        printf("w=[%f %f %f] ",w[0],w[1],w[2]);
	//        printf("sina=%f sinb=%f ",sina,sinb);
	printf("sina*sinb=%f ",sina*sinb);
	printf("cosc-(cosa*cosb)=%f ",cosc-(cosa*cosb));
	printf("cosc-(cosa*cosb)/sina*sinb=%30.27f ",(cosc-(cosa*cosb))/(sina*sinb));
        printf("angle=%f \n",acos((cosc-(cosa*cosb))/(sina*sinb)));


    }
#endif

    // Calculate the cosine of the angle we're calculating
    // using spherical trigonometry formula
   double cos_angle=(cosc-(cosa*cosb))/(sina*sinb);

   // If we've gone a little out of bounds due to round off, shift back
   if (cos_angle > 1.0) cos_angle=1.0;
   if (cos_angle < -1.0) cos_angle=-1.0;

   // return angle
   return acos(cos_angle);

}


// Compute the great circle area of a polygon on a sphere
double great_circle_area(int n, double *pnts) {

  // sum angles around polygon
  double sum=0.0;
  for (int i=0; i<n; i++) {
    // points that make up a side of polygon
    double *pnt0=pnts+3*i;
    double *pnt1=pnts+3*((i+1)%n);
    double *pnt2=pnts+3*((i+2)%n);

    // compute angle for pnt1
    sum += angle(pnt1, pnt2, pnt0);
  }

  // return area
  return sum-(((double)(n-2))*((double)M_PI));
}


  // Not really a math routine, but useful as a starting point for math routines
  void get_elem_coords(const MeshObj *elem, MEField<>  *cfield, int sdim, int max_num_nodes, int *num_nodes, double *coords) {

      // Get number of nodes in element
      const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*elem);

      // make sure that we're not bigger than max size
      if (topo->num_nodes > max_num_nodes) {
	Throw() << "Element exceeds maximum poly size";
      }

      // Get coords of element
      int k=0;
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){
	const MeshObj &node = *(elem->Relations[s].obj);
	double *c = cfield->data(node);
        for (int i=0; i<sdim; i++) {
	  coords[k]=c[i];
          k++;
	}
      }

      // Get number of nodes
      *num_nodes=topo->num_nodes;
  }



void remove_0len_edges3D(int *num_p, double *p) {

#define PNTS_EQUAL(p1,p2) ((std::abs(p1[0]-p2[0]) < 1E-15) &&	\
                           (std::abs(p1[1]-p2[1]) < 1E-15) &&	\
                           (std::abs(p1[2]-p2[2]) < 1E-15))
    
  // Get old value of num_p
  int old_num_p=*num_p;

  // See if there are any equal points
  int j=-1;
  double *last_pnt=p+3*(old_num_p-1);
  for (int i=0; i<old_num_p; i++) {
    double *pnti=p+3*i;
    
    if (PNTS_EQUAL(pnti,last_pnt)) {
      j=i;
      break;
    }

    // advance last point
    last_pnt=pnti;
  } 

  // We found an equal point so start trimming them out
  if (j>-1) {
    for (int i=j; i<old_num_p; i++) {
      double *pnti=p+3*i;
      if (!PNTS_EQUAL(pnti,last_pnt)) {
	double *pntj=p+3*j;

	// copy the non-equal point to j
	pntj[0]=pnti[0];
	pntj[1]=pnti[1];
	pntj[2]=pnti[2];

	// move j
	j++;

	// reset the last pointer to the last non-repeating value
	last_pnt=pntj;
      }
    }

    // reset num_p to the new number of points
    *num_p=j;
  } else {
    // Leave num_p as it is
  }

#undef PNTS_EQUAL
}


} // namespace
