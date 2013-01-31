// $Id: ESMCI_MathUtil.C,v 1.23 2012/11/15 20:50:43 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2013, University Corporation for Atmospheric Research, 
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
#include <Mesh/include/ESMCI_MathUtil.h>

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>



//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_MathUtil.C,v 1.23 2012/11/15 20:50:43 oehmke Exp $";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

          
namespace ESMCI {

  bool mathutil_debug=false;

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



#if 0

  ///////////// This way of calculating great circle area doesn't work very well for small polygons ///////////


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


  double sina=sqrt(1.0-cosa*cosa);

  double sinb=sqrt(1.0-cosb*cosb);


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
#endif



  double tri_area(const double * const u, const double * const v, const double * const w) {
#define CROSS_PRODUCT3D(out,a,b) out[0]=a[1]*b[2]-a[2]*b[1]; out[1]=a[2]*b[0]-a[0]*b[2]; out[2]=a[0]*b[1]-a[1]*b[0];
#define NORM(a) sqrt(a[0]*a[0]+a[1]*a[1]+a[2]*a[2])

  double tmp_vec[3];

  CROSS_PRODUCT3D(tmp_vec,u,v);
  double sina=NORM(tmp_vec);
  double a=asin(sina);

  CROSS_PRODUCT3D(tmp_vec,u,w);
  double sinb=NORM(tmp_vec);
  double b=asin(sinb);

  CROSS_PRODUCT3D(tmp_vec,w,v);
  double sinc=NORM(tmp_vec);
  double c=asin(sinc);

  double s=0.5*(a+b+c);

  double t = tan ( s / 2.0 ) * tan ( ( s - a ) / 2.0 ) * 
             tan ( ( s - b ) / 2.0 ) * tan ( ( s - c ) / 2.0 );

  //  if (mathutil_debug) {
  //  printf("a=%30.27f b=%30.27f c=%30.27f a+b+c=%30.27f s=%30.27f t=%40.37f \n",a,b,c,a+b+c,s,t);
  //}

  double area= std::abs ( 4.0 * atan ( sqrt (std::abs ( t ) ) ) );

  return area;

#undef CROSS_PRODUCT3D
#undef NORM
}


// Compute the great circle area of a polygon on a sphere
double great_circle_area(int n, double *pnts) {

  // sum areas around polygon
  double sum=0.0;
  double *pnt0=pnts;
  for (int i=1; i<n-1; i++) {
    // points that make up a side of polygon
    double *pnt1=pnts+3*i;
    double *pnt2=pnts+3*(i+1);

    // compute angle for pnt1
    sum += tri_area(pnt0, pnt1, pnt2);
  }

  // return area
  return sum;
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



  // Get coords, but flip so always counter clockwise
  // Also gets rid of degenerate edges
  // This version only works for elements of parametric_dimension = 2 and spatial_dim=2
  void get_elem_coords_2D_ccw(const MeshObj *elem, MEField<>  *cfield, int max_num_nodes,double *tmp_coords, 
                              int *num_nodes, double *coords) {
    int num_tmp_nodes;

    // Get element coords
    get_elem_coords(elem, cfield, 2, max_num_nodes, &num_tmp_nodes, tmp_coords);

    // Remove degenerate edges
    remove_0len_edges2D(&num_tmp_nodes, tmp_coords);

    // Check if degenerate
    // (if there's less than 3 no notion of CCW or CW)
    if (num_tmp_nodes <3) {
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }

      *num_nodes=num_tmp_nodes;
      return;
    }
    
    // Get elem rotation
    bool left_turn;
    bool right_turn;
    rot_2D_2D_cart(num_tmp_nodes, tmp_coords, &left_turn, &right_turn);

    // Copy to output array swapping if necessary
    if (left_turn) {
      // Don't Swap
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }
    } else {
      // Swap
      int j=0; int k=2*(num_tmp_nodes-1); 
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[k];
        coords[j+1]=tmp_coords[k+1];
        j+=2; k-=2;
      }
    }

   
  // Output num nodes
  *num_nodes=num_tmp_nodes;
}



  // Get coords, but flip so always counter clockwise
  // Also gets rid of degenerate edges
  // This version only works for elements of parametric_dimension = 2 and spatial_dim=2
  void get_elem_coords_3D_ccw(const MeshObj *elem, MEField<>  *cfield, int max_num_nodes,double *tmp_coords, 
                              int *num_nodes, double *coords) {
    int num_tmp_nodes;

    // Get element coords
    get_elem_coords(elem, cfield, 3, max_num_nodes, &num_tmp_nodes, tmp_coords);

    // Remove degenerate edges
    remove_0len_edges3D(&num_tmp_nodes, tmp_coords);

    // Check if degenerate
    // (if there's less than 3 no notion of CCW or CW)
    if (num_tmp_nodes <3) {
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }
      *num_nodes=num_tmp_nodes;

      return;
    }
    
    // Get elem rotation
    bool left_turn;
    bool right_turn;
    rot_2D_3D_sph(num_tmp_nodes, tmp_coords, &left_turn, &right_turn);

    // Copy to output array swapping if necessary
    if (left_turn) {
      // Don't Swap
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }
    } else {
      // Swap
      int j=0; int k=3*(num_tmp_nodes-1); 
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[k];
        coords[j+1]=tmp_coords[k+1];
        coords[j+2]=tmp_coords[k+2];
        j+=3; k-=3;
      }
    }

   
  // Output num nodes
  *num_nodes=num_tmp_nodes;
}





  // Not really a math routine, but useful as a starting point for math routines
  void get_elem_coords_and_ids(const MeshObj *elem, MEField<>  *cfield, int sdim, int max_num_nodes, int *num_nodes, double *coords, int *ids) {

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
        ids[s]=node.get_id();
      }

      // Get number of nodes
      *num_nodes=topo->num_nodes;
  }




void remove_0len_edges3D(int *num_p, double *p) {

#define EQUAL_TOL 1E-15
#define PNTS_EQUAL(p1,p2) ((std::abs(p1[0]-p2[0]) < EQUAL_TOL) &&	\
                           (std::abs(p1[1]-p2[1]) < EQUAL_TOL) &&	\
                           (std::abs(p1[2]-p2[2]) < EQUAL_TOL))
    
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

#undef EQUAL_TOL
#undef PNTS_EQUAL
}



void remove_0len_edges2D(int *num_p, double *p) {

#define EQUAL_TOL 1E-15
#define PNTS_EQUAL(p1,p2) ((std::abs(p1[0]-p2[0]) < EQUAL_TOL) &&	\
                           (std::abs(p1[1]-p2[1]) < EQUAL_TOL))
    
  // Get old value of num_p
  int old_num_p=*num_p;

  // See if there are any equal points
  int j=-1;
  double *last_pnt=p+2*(old_num_p-1);
  for (int i=0; i<old_num_p; i++) {
    double *pnti=p+2*i;
    
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
      double *pnti=p+2*i;
      if (!PNTS_EQUAL(pnti,last_pnt)) {
	double *pntj=p+2*j;

	// copy the non-equal point to j
	pntj[0]=pnti[0];
	pntj[1]=pnti[1];

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

#undef EQUAL_TOL
#undef PNTS_EQUAL
}



  // Is this a smashed quad?
  // (i.e. a quad with opposite corners equal)
bool is_smashed_quad2D(int num_p, double *p) {

#define EQUAL_TOL 1E-15
#define PNTS_EQUAL(p1,p2) ((std::abs((p1)[0]-(p2)[0]) < EQUAL_TOL) &&	\
                           (std::abs((p1)[1]-(p2)[1]) < EQUAL_TOL))
      

  // If not a quad, then leave
  if (num_p != 4) return false;

  // See if 1st point is equal to 3rd point
  if (PNTS_EQUAL(p,p+4)) return true;

  // See if 2nd point is equal to 4th point
  if (PNTS_EQUAL(p+2,p+6)) return true;

  // not a smashed quad
  return false;

#undef EQUAL_TOL
#undef PNTS_EQUAL
}


  // Is this a smashed quad?
  // (i.e. a quad with opposite corners equal)
bool is_smashed_quad3D(int num_p, double *p) {

#define EQUAL_TOL 1E-15
#define PNTS_EQUAL(p1,p2) ((std::abs((p1)[0]-(p2)[0]) < EQUAL_TOL) &&	\
                           (std::abs((p1)[1]-(p2)[1]) < EQUAL_TOL) &&	\
                           (std::abs((p1)[2]-(p2)[2]) < EQUAL_TOL))
      

  // If not a quad, then leave
  if (num_p != 4) return false;

  // See if 1st point is equal to 3rd point
  if (PNTS_EQUAL(p,p+6)) return true;

  // See if 2nd point is equal to 4th point
  if (PNTS_EQUAL(p+3,p+9)) return true;

  // not a smashed quad
  return false;

#undef EQUAL_TOL
#undef PNTS_EQUAL
}





  void write_3D_poly_woid_to_vtk(const char *filename, int num_p, double *p) {
    std::ofstream myfile;
#define MAX_W3PTV_STR_LEN 1000
    char new_filename[MAX_W3PTV_STR_LEN];
    
    if (strlen(filename)+4 > MAX_W3PTV_STR_LEN) {
       printf("ERROR: filename too long!!!\n");
       return;
    }

    sprintf(new_filename,"%s.vtk",filename);

    myfile.open (new_filename);

    myfile << "# vtk DataFile Version 3.0\n";
    myfile << "This file generated by ESMF\n";
    myfile << "ASCII\n";
    myfile << "DATASET UNSTRUCTURED_GRID\n";
    myfile << "POINTS "<<num_p<<" double\n";
    for (int i=0; i<num_p ; i++){
      myfile << p[3*i]<<" "<< " "<<p[3*i+1] <<" "<< p[3*i+2]<<"\n";
    }
    myfile << "CELLS 1 "<<num_p+1<<"\n";
    myfile << num_p <<" ";
    for (int i=0; i<num_p ; i++){
      myfile << i <<" ";
    }
    myfile << "\n";
    myfile << "CELL_TYPES 1 \n";
    myfile << "7 \n";
    myfile << "POINT_DATA "<<num_p<<"\n";
    myfile << "SCALARS POINT_INDEX double 1 \n";
    myfile << "LOOKUP_TABLE default \n";
    for (int i=0; i<num_p ; i++){
      myfile << i <<" ";
    }
    myfile << "\n";
    myfile.close();
#undef MAX_W3PTV_STR_LEN 
  }

  void write_3D_poly_to_vtk(const char *filename, int id, int num_p, double *p) {
#define MAX_W3PTVID_STR_LEN 1000
    char new_filename[MAX_W3PTVID_STR_LEN];
    
    if (((double)strlen(filename))+log10((double)id)+1.0 > ((double)MAX_W3PTVID_STR_LEN)) {
       printf("ERROR: filename too long!!!\n");
       return;
    }

    sprintf(new_filename,"%s%d",filename,id);

    write_3D_poly_woid_to_vtk(new_filename,num_p,p);

#undef MAX_W3PTVID_STR_LEN 
  }



void rot_2D_2D_cart(int num_p, double *p, bool *left_turn, bool *right_turn) {

  // Define Cross product                                                                                                                    
#define CROSS_PRODUCT2D(out,a,b) out=a[0]*b[1]-a[1]*b[0];
#define TOL 1.0E-17

  // init flags                                                            
  *left_turn=false;
  *right_turn=false;

  // Loop through polygon                    
  for (int i=0; i<num_p; i++) {
    double *pntip0=p+2*i;
    double *pntip1=p+2*((i+1)%num_p);
    double *pntip2=p+2*((i+2)%num_p);

    // vector from pntip1 to pnti0                                                                                             
    double v10[2];
    v10[0]=pntip0[0]-pntip1[0];
    v10[1]=pntip0[1]-pntip1[1];


    // vector from pntip1 to pnti2                                                                            
    double v12[2];
    v12[0]=pntip2[0]-pntip1[0];
    v12[1]=pntip2[1]-pntip1[1];

    // Calc cross product                                                             
    double cross;
    CROSS_PRODUCT2D(cross,v12,v10);

    // Interpret direction
    if (cross > TOL) {
      *left_turn=true;
    } else if (cross < -TOL) {
      *right_turn=true;
    }
  }

  // If no turns default to left
  if (!(*right_turn) && !(*left_turn)) *left_turn=true;


#undef CROSS_PRODUCT2D
#undef TOL
}





void rot_2D_3D_sph(int num_p, double *p, bool *left_turn, bool *right_turn) {

  // Define Cross product                                                                                                                   
#define CROSS_PRODUCT3D(out,a,b) out[0]=a[1]*b[2]-a[2]*b[1]; out[1]=a[2]*b[0]-a[0]*b[2]; out[2]=a[0]*b[1]-a[1]*b[0];
#define DOT_PRODUCT3D(a,b) a[0]*b[0]+a[1]*b[1]+a[2]*b[2];
#define TOL 1.0E-17

  // init flags                                                            
  *left_turn=false;
  *right_turn=false;

  // Loop through polygon                    
  for (int i=0; i<num_p; i++) {
    double *pntip0=p+3*i;
    double *pntip1=p+3*((i+1)%num_p);
    double *pntip2=p+3*((i+2)%num_p);

    // vector from pntip1 to pnti0                                                                                             
    double v10[3];
    v10[0]=pntip0[0]-pntip1[0];
    v10[1]=pntip0[1]-pntip1[1];
    v10[2]=pntip0[2]-pntip1[2];


    // vector from pntip1 to pnti2                                                                            
    double v12[3];
    v12[0]=pntip2[0]-pntip1[0];
    v12[1]=pntip2[1]-pntip1[1];
    v12[2]=pntip2[2]-pntip1[2];

    // Calc cross product                                                             
    double cross[3];
    CROSS_PRODUCT3D(cross,v12,v10);

    // dot cross product with vector from center of sphere
    // to middle point (pntip1)
    double dir=DOT_PRODUCT3D(cross,pntip1);

    // Interpret direction
    if (dir > TOL) {
      *left_turn=true;
    } else if (dir < -TOL) {
      *right_turn=true;
    }
  }

  // If no turns default to left
  if (!(*right_turn) && !(*left_turn)) *left_turn=true;

#undef CROSS_PRODUCT3D
#undef TOL

}


  // Detect if a point (pnt) is in a polygon (p)
  // here the points are 2D and the polygon is counter-clockwise
  // returns true if the point is in the polygon (including on the edge), and
  // false otherwise
template <class GEOM>
  bool is_pnt_in_poly(int num_p, double *p, double *pnt) {
  
  // Loop through polygon                    
  for (int i=0; i<num_p; i++) {
    double *pntip0=GEOM::getPntAt(p,i);
    double *pntip1=GEOM::getPntAt(p,(i+1)%num_p);

    // vector from pntip0 to pnti1              
    double v01[GEOM::pnt_size];
    GEOM::sub(v01,pntip1,pntip0);

    // vector from pntip0 to pnt              
    double v0p[GEOM::pnt_size];
    GEOM::sub(v0p,pnt,pntip0);

    // Calc cross product                                                             
    double cross;
    cross=GEOM::turn(v01,v0p,pntip0);

    //    printf("%d pnt0=%f %f turn=%f\n",i,pntip0[0],pntip0[1],cross);

    if (cross < 0.0) {
      return false;
    }
  }

  return true;
}

// Create instances for supported geometries, otherwise would have to put template
// in include file
template bool is_pnt_in_poly<GEOM_CART2D>(int num_p, double *p, double *pnt);
template bool is_pnt_in_poly<GEOM_SPH2D3D>(int num_p, double *p, double *pnt);

// See if any other points in the polygon are in the triangle formed by ind[0], ind[1], ind[2]
template<class GEOM>
bool is_ear(int num_p, double *p, int *ind) {

  // Form triangle
  double tri[3*GEOM::pnt_size];

  GEOM::copy(GEOM::getPntAt(tri,0),GEOM::getPntAt(p,ind[0]));
  GEOM::copy(GEOM::getPntAt(tri,1),GEOM::getPntAt(p,ind[1]));
  GEOM::copy(GEOM::getPntAt(tri,2),GEOM::getPntAt(p,ind[2]));

  // Check other points in polygon
  for (int i=0; i<num_p; i++) {
    if (i==ind[0]) continue;
    if (i==ind[1]) continue;
    if (i==ind[2]) continue;
    if (is_pnt_in_poly<GEOM>(3, tri, GEOM::getPntAt(p,i))) return false;
  }

  return true;
}

// Create instances for supported geometries, otherwise would have to put template
// in include file
template bool is_ear<GEOM_CART2D>(int num_p, double *p, int *ind);
template bool is_ear<GEOM_SPH2D3D>(int num_p, double *p, int *ind);


// Triangulate a 2D polygon using the ear clip method.
// This method works on both concave and convex polygons.
// As usual in ESMF Mesh this assumes the polygon is counter-clockwise.
// Output is in tri_ind, which are the 0-based indices of the triangles 
// making up the triangulization. tri_ind should be of size 3*(num_p-2)
// td should be the same size as p, ti should be of size num_p.
template <class GEOM>
int triangulate_poly(int num_p, double *p, double *td, int *ti, int *tri_ind) {

  // Error check
  if (num_p < 3) {
    return ESMCI_TP_DEGENERATE_POLY;
  }

  // Handle degenerate case
  if (num_p == 3) {
    tri_ind[0]=0;
    tri_ind[1]=1;
    tri_ind[2]=2;
    return ESMCI_TP_SUCCESS;
  }

  // Copy polygon to temporary array
  memcpy((double *)td, (double *)p, GEOM::pnt_size*num_p*sizeof(double));
  int num_t=num_p;
  
  // Fill index array
  for(int i=0; i<num_p; i++) {
    ti[i]=i;
  }


  // Loop until we've broken everything up
  int pos_tri_ind=0;
  while (true) {

    // Handle triangle case
    if (num_t == 3) {
      tri_ind[pos_tri_ind]=ti[0];
      tri_ind[pos_tri_ind+1]=ti[1];
      tri_ind[pos_tri_ind+2]=ti[2];
      return ESMCI_TP_SUCCESS;
    }

    // Loop through polygon                    
    int max_clip_ind[3];
    double max_clip_dot=-std::numeric_limits<double>::max();
    bool found_clip=false;
    for (int i=0; i<num_t; i++) {
      // indices which make up triangle to potentially clip
      int clip_ind[3];
      clip_ind[0]=i;
      clip_ind[1]=(i+1)%num_t;
      clip_ind[2]=(i+2)%num_t;

      // Points which make up triangle
      double *pntip0=GEOM::getPntAt(td,clip_ind[0]);
      double *pntip1=GEOM::getPntAt(td,clip_ind[1]);
      double *pntip2=GEOM::getPntAt(td,clip_ind[2]);

      // vector from pntip1 to pnti0                 
      double v10[GEOM::pnt_size];
      GEOM::sub(v10,pntip0,pntip1);

      // vector from pntip1 to pnti2
      double v12[GEOM::pnt_size];
      GEOM::sub(v12,pntip2,pntip1);

      // Calc cross product                                                             
      double cross;
      cross=GEOM::turn(v12,v10,pntip1);

      // Find the maximum left turn to clip
      // to give good triangles
      if (cross > 0.0) {
        double dot;
        dot=GEOM::dot(v12,v10);
        //printf("%d dot=%f max=%f \n",i,dot,max_clip_dot);
        if (dot > max_clip_dot) {
          bool is_ear_b=is_ear<GEOM>(num_t, td, clip_ind);
          //printf("%d  is_ear_b=%d \n",i,is_ear_b);
          if (is_ear_b) {
          //          if (is_ear<GEOM>(num_t, td, clip_ind)) {
            max_clip_dot=dot;
            max_clip_ind[0]=clip_ind[0];
            max_clip_ind[1]=clip_ind[1];
            max_clip_ind[2]=clip_ind[2];
            found_clip=true;
          }
        }
      }
    }
    
    // Clip
    if (found_clip) {
      // Add clipped triangle to list
      tri_ind[pos_tri_ind]=ti[max_clip_ind[0]];
      tri_ind[pos_tri_ind+1]=ti[max_clip_ind[1]];
      tri_ind[pos_tri_ind+2]=ti[max_clip_ind[2]];
      pos_tri_ind +=3;
      
      // remove triangle from polygon and collapse arrays
      for (int j=max_clip_ind[1]+1; j<num_t; j++) {
        GEOM::copy(GEOM::getPntAt(td,j-1),GEOM::getPntAt(td,j));
        ti[j-1]=ti[j];
      }

      // shrink size by 1
      num_t--;

      // Loop back to beginning
      continue;
    } else {
      return ESMCI_TP_CLOCKWISE_POLY;
    }        
  } // Loop back to top before triangle detection

}

// Create instances for supported geometries, otherwise would have to put template
// in include file
template int triangulate_poly<GEOM_CART2D>(int num_p, double *p, double *td, int *ti, int *tri_ind);
template int triangulate_poly<GEOM_SPH2D3D>(int num_p, double *p, double *td, int *ti, int *tri_ind);


// calculates spherical coords in radians
// lon -> (-pi to +pi)
// lat -> (0   to +pi)
void convert_cart_to_sph(double x, double y, double z,
                         double *lon, double *lat, double *r) {

  // calc radius of sphere 
  *r=sqrt(x*x+y*y+z*z);

  // calc lon
  *lon=atan2(y,x);

  // calc lat
  *lat=acos(z/(*r));
}


// calculates spherical coords in degs
// lon -> (-180 to 180)
// lat -> (-90  to 90)
void convert_cart_to_sph_deg(double x, double y, double z,
                             double *lon, double *lat, double *r) {

  const double RAD2DEG=57.295779513082325;
  double lon_r,lat_r;

  convert_cart_to_sph(x, y, z,
                      &lon_r, &lat_r, r);

  *lon=lon_r*RAD2DEG;
  *lat=90.0-(lat_r*RAD2DEG);
}



} // namespace
