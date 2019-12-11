#include "moab/DiscreteGeometry/HiReconstruction.hpp"
#include "moab/DiscreteGeometry/DGMSolver.hpp"
#include "moab/HalfFacetRep.hpp"

#ifdef MOAB_HAVE_MPI
#include "moab/ParallelComm.hpp"
#endif
#include "MBTagConventions.hpp"

#define HIREC_USE_AHF

#include <math.h>
#include <deque>
#include <iostream>

namespace moab
{
	HiReconstruction::HiReconstruction(Core *impl, ParallelComm *comm, EntityHandle meshIn, int minpnts, bool recwhole)
	 : mbImpl(impl),pcomm(comm),_mesh2rec(meshIn),_MINPNTS(minpnts){
	 	assert(NULL!=impl);
	 	ErrorCode error;
	 	_MINEPS = 1e-12;
	 	_dim = 0;
	 	_hasfittings = false;
	 	_hasderiv = false;
	 #ifdef MOAB_HAVE_MPI
	 	if(!pcomm){
	 		pcomm = moab::ParallelComm::get_pcomm(mbImpl,0);
	 	}
	 #endif

	 	error = initialize(recwhole);
	 	if(MB_SUCCESS!=error){
	 		std::cout << "Error initializing HiReconstruction\n" << std::endl;
	 		exit(1);
	 	}
	 }

	HiReconstruction::~HiReconstruction(){
	#ifdef MOAB_HAVE_AHF
	 	ahf = NULL;
	#else
	 	delete ahf;
	#endif
	}

	ErrorCode HiReconstruction::initialize(bool recwhole){
		ErrorCode error;

	#ifdef HIREC_USE_AHF
		std::cout << "HIREC_USE_AHF: Initializing" << std::endl;
		ahf = new HalfFacetRep(mbImpl,pcomm,_mesh2rec, false);
		if(!ahf){
			return MB_MEMORY_ALLOCATION_FAILED;
		}
		error = ahf->initialize(); MB_CHK_ERR(error);
	#else
		ahf = NULL;
	#endif

		//error = ahf->get_entity_ranges(_inverts,_inedges,_infaces,_incells); MB_CHK_ERR(error);
		error = mbImpl->get_entities_by_dimension(_mesh2rec,0,_inverts); MB_CHK_ERR(error);
		error = mbImpl->get_entities_by_dimension(_mesh2rec,1,_inedges); MB_CHK_ERR(error);
		error = mbImpl->get_entities_by_dimension(_mesh2rec,2,_infaces); MB_CHK_ERR(error);
		error = mbImpl->get_entities_by_dimension(_mesh2rec,3,_incells); MB_CHK_ERR(error);
		if(_inedges.size()&&_infaces.empty()&&_incells.empty()){
			_dim = 1; _MAXPNTS = 13;
		}else if(_infaces.size()&&_incells.empty()){
			_dim = 2; _MAXPNTS = 128;
		}else{
			MB_SET_ERR(MB_FAILURE,"Encountered a non-manifold mesh or a mesh with volume elements");
		}


		//get locally hosted vertices by filtering pstatus
	#ifdef MOAB_HAVE_MPI
		if(pcomm){
			error = pcomm->filter_pstatus(_inverts,PSTATUS_GHOST,PSTATUS_NOT,-1,&_verts2rec); MB_CHK_ERR(error);
		}else{
			_verts2rec = _inverts;
		}
	#else
		_verts2rec = _inverts;
	#endif
		_nv2rec = _verts2rec.size();

		if(recwhole){
			//compute normals(surface) or tangent vector(curve) for all locally hosted vertices
			if(2==_dim){
				compute_average_vertex_normals_surf();
			}else if(1==_dim){
				compute_average_vertex_tangents_curve();
			}else{
				MB_SET_ERR(MB_FAILURE,"Unknow space dimension");
			}
			_hasderiv = true;
		}
		return error;
	}

	/***************************************************
	 *  User Interface for Reconstruction of Geometry  *
	 ***************************************************/

	ErrorCode HiReconstruction::reconstruct3D_surf_geom(int degree, bool interp, bool safeguard, bool reset){
		assert(2==_dim);
		if(_hasfittings&&!reset){
			//This object has precomputed fitting results and user don't want to reset
			return MB_SUCCESS;
		}else{
			_initfittings = _hasfittings = false;
		}
		//initialize for geometric information
		initialize_surf_geom(degree);
		ErrorCode error;
		double *coeffs,*coords;
		int *degree_out;
		int ncoeffs = (degree+2)*(degree+1)/2;

		//DBG
		int dcount = 0;

		for(Range::iterator ivert=_verts2rec.begin();ivert!=_verts2rec.end();++ivert){
			int index = _verts2rec.index(*ivert);
			//for debug
			/*if(index==70){
				EntityHandle vid = *ivert;
				double vertcoords[3];
				error = mbImpl->get_coords(&vid,1,vertcoords);
			}*/

			size_t istr = _vertID2coeffID[index];
			coords = &(_local_coords[9*index]);
			coeffs = &(_local_fit_coeffs[istr]);
			degree_out = &(_degrees_out[index]);
			_interps[index] = interp;
			error  = polyfit3d_walf_surf_vertex(*ivert,interp,degree,_MINPNTS,safeguard,9,coords,degree_out,ncoeffs,coeffs);MB_CHK_ERR(error);

			//DBG
			if (degree_out[0] < degree)
			  dcount += 1;

		}

		//DBG
		//std::cout<<"Total #points ="<<_verts2rec.size()<<", #degraded points = "<<dcount<<std::endl;

		_geom = HISURFACE;
		_hasfittings = true;
		return error;
	}

	ErrorCode HiReconstruction::reconstruct3D_surf_geom(size_t npts, int* degrees, bool* interps, bool safeguard, bool reset){
		assert(_dim==2);
		if(npts!=_nv2rec){
			MB_SET_ERR(MB_FAILURE,"Input number of degrees doesn't match number of vertices");
		}
		if(_hasfittings&&!reset){
			return MB_SUCCESS;
		}else{
			_initfittings = _hasfittings = false;
		}
		ErrorCode error;
		//initialization for fitting
		initialize_surf_geom(npts,degrees);
		double *coeffs,*coords;
		int *degree_out;
		size_t i=0;
		for(Range::iterator ivert=_verts2rec.begin();ivert!=_verts2rec.end();++ivert,++i){
			int index = _verts2rec.index(*ivert); assert(-1!=index);
			size_t istr = _vertID2coeffID[index];
			coords = &(_local_coords[9*index]);
			coeffs = &(_local_fit_coeffs[istr]);
			degree_out = &(_degrees_out[index]);
			_interps[index] = interps[i];
			int ncoeffs = (degrees[i]+2)*(degrees[i]+1)/2;
			error = polyfit3d_walf_surf_vertex(*ivert,interps[i],degrees[i],_MINPNTS,safeguard,9,coords,degree_out,ncoeffs,coeffs);
			MB_CHK_ERR(error);
		}
		_geom = HISURFACE;
		_hasfittings = true;
		return error;
	}

	ErrorCode HiReconstruction::reconstruct3D_curve_geom(int degree, bool interp, bool safeguard, bool reset){
		assert(_dim==1);
		if(_hasfittings&&!reset){
			return MB_SUCCESS;
		}else{
			_initfittings = _hasfittings = false;
		}
		initialize_3Dcurve_geom(degree);
		ErrorCode error;
		double *coords=0,*coeffs;
		int *degree_out;
		int ncoeffs = 3*(degree+1);
		for(Range::iterator ivert=_verts2rec.begin();ivert!=_verts2rec.end();++ivert){
			int index = _verts2rec.index(*ivert); assert(index!=-1);
			size_t istr = _vertID2coeffID[index];
			coeffs = &(_local_fit_coeffs[istr]);
			degree_out = &(_degrees_out[index]);
			_interps[index] = interp;
			error = polyfit3d_walf_curve_vertex(*ivert,interp,degree,_MINPNTS,safeguard,0,coords,degree_out,ncoeffs,coeffs); MB_CHK_ERR(error);
		}
		_geom = HI3DCURVE;
		_hasfittings = true;
		return error;
	}

	ErrorCode HiReconstruction::reconstruct3D_curve_geom(size_t npts, int* degrees, bool* interps, bool safeguard, bool reset){
		assert(_dim==1);
		ErrorCode error;
		if(npts!=_nv2rec){
			MB_SET_ERR(MB_FAILURE,"Input number of degrees doesn't match the number of vertices");
		}
		if(_hasfittings&&!reset){
			return MB_SUCCESS;
		}else{
			_initfittings = _hasfittings = false;
		}
		//initialize
		initialize_3Dcurve_geom(npts,degrees);
		double *coords=0,*coeffs;
		int *degree_out;
		size_t i=0;
		for(Range::iterator ivert=_verts2rec.begin();ivert!=_verts2rec.end();++ivert,++i){
			int index = _verts2rec.index(*ivert);
			size_t istr = _vertID2coeffID[index];
			coeffs = &(_local_fit_coeffs[istr]);
			degree_out = &(_degrees_out[index]);
			_interps[index] = interps[i];
			int ncoeffs = 3*(degrees[i]+1);
			error = polyfit3d_walf_curve_vertex(*ivert,interps[i],degrees[i],_MINPNTS,safeguard,0,coords,degree_out,ncoeffs,coeffs);
			MB_CHK_ERR(error);
		}
		_geom = HI3DCURVE;
		_hasfittings = true;
		return error;
	}

	ErrorCode HiReconstruction::polyfit3d_walf_surf_vertex(const EntityHandle vid, const bool interp, int degree, int minpnts, const bool safeguard, const int ncoords, double* coords, int* degree_out, const int ncoeffs, double* coeffs){
		assert(_dim==2);
		ErrorCode error;
		int ring = estimate_num_rings(degree,interp);
		//std::cout<<"ring = "<<ring<<std::endl;
		//get n-ring neighbors
		Range ngbvs;
		error = obtain_nring_ngbvs(vid,ring,minpnts,ngbvs); MB_CHK_ERR(error);
		//for debug
		/*if(_verts2rec.index(vid)==70){
			for(Range::iterator ingb=ngbvs.begin();ingb!=ngbvs.end();++ingb) std::cerr << _verts2rec.index(*ingb) << " ";
			std::cout << std::endl;
		}*/

		//get coordinates;
		size_t nverts = ngbvs.size(); assert(nverts);
		double *ngbcoords = new double[nverts*3];
		error = mbImpl->get_coords(ngbvs,ngbcoords); MB_CHK_ERR(error);
		//get normals
		double *ngbnrms = new double[nverts*3];
		error = get_normals_surf(ngbvs,ngbnrms); MB_CHK_ERR(error);
		//switch vid to first one
		int index = ngbvs.index(vid); assert(index!=-1);
		std::swap(ngbcoords[0],ngbcoords[3*index]);std::swap(ngbcoords[1],ngbcoords[3*index+1]);std::swap(ngbcoords[2],ngbcoords[3*index+2]);
		std::swap(ngbnrms[0],ngbnrms[3*index]);std::swap(ngbnrms[1],ngbnrms[3*index+1]);std::swap(ngbnrms[2],ngbnrms[3*index+2]);
		//local WLS fitting
		int degree_pnt,degree_qr;
		polyfit3d_surf_get_coeff(nverts,ngbcoords,ngbnrms,degree,interp,safeguard,ncoords,coords,ncoeffs,coeffs,degree_out,&degree_pnt,&degree_qr);
		delete [] ngbcoords; delete [] ngbnrms;
		return error;
	}

	ErrorCode HiReconstruction::polyfit3d_walf_curve_vertex(const EntityHandle vid, const bool interp, int degree, int minpnts, const bool safeguard, const int ncoords, double* coords, int* degree_out, const int ncoeffs, double* coeffs){
		ErrorCode error;
		int ring = estimate_num_rings(degree,interp);
		//get n-ring neighbors
		Range ngbvs;
		error = obtain_nring_ngbvs(vid,ring,minpnts,ngbvs); MB_CHK_ERR(error);
		//get coordinates
		size_t nverts = ngbvs.size(); assert(nverts);
		double *ngbcoords = new double[nverts*3];
		error = mbImpl->get_coords(ngbvs,ngbcoords); MB_CHK_ERR(error);
		//get tangent vectors
		double *ngbtangs = new double[nverts*3];
		error = get_tangents_curve(ngbvs,ngbtangs); MB_CHK_ERR(error);
		//switch vid to first one
		int index = ngbvs.index(vid); assert(index!=-1);
		std::swap(ngbcoords[0],ngbcoords[3*index]);std::swap(ngbcoords[1],ngbcoords[3*index+1]);std::swap(ngbcoords[2],ngbcoords[3*index+2]);
		std::swap(ngbtangs[0],ngbtangs[3*index]);std::swap(ngbtangs[1],ngbtangs[3*index+1]);std::swap(ngbtangs[2],ngbtangs[3*index+2]);
		//local WLS fittings
		polyfit3d_curve_get_coeff(nverts,ngbcoords,ngbtangs,degree,interp,safeguard,ncoords,coords,ncoeffs,coeffs,degree_out);
		delete [] ngbcoords; delete [] ngbtangs;
		return error;
	}

	/**************************************************************
	 *  User Interface for Evaluation via Reconstructed Geometry  *
	 **************************************************************/

	ErrorCode HiReconstruction::hiproj_walf_in_element(EntityHandle elem, const int nvpe, const int npts2fit, const double* naturalcoords2fit, double* newcoords){
		assert(newcoords);
		ErrorCode error;
		//get connectivity table
		std::vector<EntityHandle> elemconn;
		error = mbImpl->get_connectivity(&elem,1,elemconn); MB_CHK_ERR(error);
		if(nvpe!=(int)elemconn.size()){
			MB_SET_ERR(MB_FAILURE,"element connectivity table size doesn't match input size");
		}

		if(!_hasfittings){
			MB_SET_ERR(MB_FAILURE,"There is no existing fitting results");
		}else{
			std::ostringstream convert; convert << elem; std::string ID = convert.str();
			for(int i=0;i<nvpe;++i){
				if(-1==_verts2rec.index(elemconn[i])){
					MB_SET_ERR(MB_FAILURE,"There is no existing fitting results for element "+ID);
				}
			}
		}
		//check correctness of input
		for(int i=0;i<npts2fit;++i){
			if(!check_barycentric_coords(nvpe,naturalcoords2fit+i*nvpe)){
				MB_SET_ERR(MB_FAILURE,"Wrong barycentric coordinates");
			}
		}

		double *elemcoords = new double[nvpe*3];
		error = mbImpl->get_coords(&(elemconn[0]),nvpe,elemcoords); MB_CHK_ERR(error);

		double *coords2fit = new double[3*npts2fit]();
		for(int i=0;i<npts2fit;++i){
			for(int j=0;j<nvpe;++j){
				coords2fit[3*i] += naturalcoords2fit[i*nvpe+j]*elemcoords[3*j];
				coords2fit[3*i+1] += naturalcoords2fit[i*nvpe+j]*elemcoords[3*j+1];
				coords2fit[3*i+2] += naturalcoords2fit[i*nvpe+j]*elemcoords[3*j+2];
			}
		}

		double *hiproj_new = new double[3*npts2fit];
		//initialize output
		for(int i=0;i<npts2fit;++i){
			newcoords[3*i] = newcoords[3*i+1] = newcoords[3*i+2] = 0;
		}
		//for each input vertex, call nvpe fittings and take average
		for(int j=0;j<nvpe;++j){
			error = hiproj_walf_around_vertex(elemconn[j],npts2fit,coords2fit,hiproj_new); MB_CHK_ERR(error);
			for(int i=0;i<npts2fit;++i){
				newcoords[3*i] += naturalcoords2fit[i*nvpe+j]*hiproj_new[3*i];
				newcoords[3*i+1] += naturalcoords2fit[i*nvpe+j]*hiproj_new[3*i+1];
				newcoords[3*i+2] += naturalcoords2fit[i*nvpe+j]*hiproj_new[3*i+2];
			}
		}
		delete [] elemcoords; delete [] coords2fit; delete [] hiproj_new;
		return error;
	}

	ErrorCode HiReconstruction::hiproj_walf_around_vertex(EntityHandle vid, const int npts2fit, const double* coords2fit, double* hiproj_new){
		if(!_hasfittings){
			MB_SET_ERR(MB_FAILURE,"There is no existing fitting results");
		}else if(-1==_verts2rec.index(vid)){
			std::ostringstream convert; convert << vid; std::string VID = convert.str();
			MB_SET_ERR(MB_FAILURE,"There is no existing fitting results for vertex "+VID);
		}
		ErrorCode error;
		//get center of local coordinates system
		double local_origin[3];
		error = mbImpl->get_coords(&vid,1,local_origin); MB_CHK_ERR(error);
		//get local fitting parameters
		int index = _verts2rec.index(vid);
		bool interp = _interps[index];
		int local_deg = _degrees_out[index];
		double *uvw_coords,*local_coeffs;
		if(_geom==HISURFACE){
			uvw_coords = &(_local_coords[9*index]);
			//int ncoeffs = (local_deg+2)*(local_deg+1)>>1;
			size_t istr = _vertID2coeffID[index];
			local_coeffs = &(_local_fit_coeffs[istr]);
			walf3d_surf_vertex_eval(local_origin,uvw_coords,local_deg,local_coeffs,interp,npts2fit,coords2fit,hiproj_new);
		}else if(_geom==HI3DCURVE){
			uvw_coords = &(_local_coords[3*index]);
			size_t istr = _vertID2coeffID[index];
			local_coeffs = &(_local_fit_coeffs[istr]);
			walf3d_curve_vertex_eval(local_origin,uvw_coords,local_deg,local_coeffs,interp,npts2fit,coords2fit,hiproj_new);
		}
		return error;
	}

	void HiReconstruction::walf3d_surf_vertex_eval(const double* local_origin, const double* local_coords, const int local_deg, const double* local_coeffs, const bool interp, const int npts2fit, const double* coords2fit, double* hiproj_new){
		double xaxis[3], yaxis[3], zaxis[3];
		for(int i=0;i<3;++i){
			xaxis[i] = local_coords[i];
			yaxis[i] = local_coords[3+i];
			zaxis[i] = local_coords[6+i];
		}
		//double *basis = new double[(local_deg+2)*(local_deg+1)/2-1];
		std::vector<double> basis((local_deg+2)*(local_deg+1)/2-1);
		for(int i=0;i<npts2fit;++i){
			double local_pos[3];
			for(int j=0;j<3;++j){
				local_pos[j] = coords2fit[3*i+j]-local_origin[j];
			}
			double u,v,height=0;
			u = DGMSolver::vec_innerprod(3,local_pos,xaxis);
			v = DGMSolver::vec_innerprod(3,local_pos,yaxis);
			basis[0] = u;
			basis[1] = v;
			int l=1;
			for(int k=2;k<=local_deg;++k){
				++l;
				basis[l] = u*basis[l-k];
				for(int id=0;id<k;++id){
					++l;
					basis[l] = basis[l-k-1]*v;
				}
			}
			if(!interp){
				height = local_coeffs[0];
			}
			for(int p=0;p<=l;++p){
				height += local_coeffs[p+1]*basis[p];
			}
			hiproj_new[3*i] = local_origin[0]+u*xaxis[0]+v*yaxis[0]+height*zaxis[0];
			hiproj_new[3*i+1] = local_origin[1]+u*xaxis[1]+v*yaxis[1]+height*zaxis[1];
			hiproj_new[3*i+2] = local_origin[2]+u*xaxis[2]+v*yaxis[2]+height*zaxis[2];
		}
		//delete [] basis;
	}

	void HiReconstruction::walf3d_curve_vertex_eval(const double* local_origin, const double* local_coords, const int local_deg, const double* local_coeffs, const bool interp, const int npts2fit, const double* coords2fit, double* hiproj_new){
		assert(local_origin&&local_coords&&local_coeffs);
		int ncoeffspvpd = local_deg+1;
		for(int i=0;i<npts2fit;++i){
			//get the vector from center to current point, project to tangent line
			double vec[3],ans[3]={0,0,0};
			DGMSolver::vec_linear_operation(3,1,coords2fit+3*i,-1,local_origin,vec);
			double u = DGMSolver::vec_innerprod(3,local_coords,vec);
			//evaluate polynomials
			if(!interp){
				ans[0] = local_coeffs[0]; ans[1] = local_coeffs[ncoeffspvpd]; ans[2] = local_coeffs[2*ncoeffspvpd];
			}
			double uk=1;//degree_out and degree different, stored in columnwise contiguously
			for(int j=1;j<ncoeffspvpd;++j){
				uk *= u;
				ans[0] += uk*local_coeffs[j]; ans[1] += uk*local_coeffs[j+ncoeffspvpd]; ans[2] += uk*local_coeffs[j+2*ncoeffspvpd];
			}
			hiproj_new[3*i] = ans[0]+local_origin[0];
			hiproj_new[3*i+1] = ans[1]+local_origin[1];
			hiproj_new[3*i+2] = ans[2]+local_origin[2];
		}
	}

	bool HiReconstruction::get_fittings_data(EntityHandle vid, GEOMTYPE& geomtype, std::vector<double>& coords, int& degree_out, std::vector<double>& coeffs, bool& interp){
		if(!_hasfittings){
			return false;
		}else{
			int index = _verts2rec.index(vid);
			if(-1==index){
				std::cout << "Input vertex is not locally hosted vertex in this mesh set" << std::endl;
				return false;
			}
			geomtype = _geom;
			if(HISURFACE==_geom){
				coords.insert(coords.end(),_local_coords.begin()+9*index,_local_coords.begin()+9*index+9);
				degree_out = _degrees_out[index]; interp = _interps[index];
				int ncoeffs = (degree_out+2)*(degree_out+1)>>1;
				size_t istr = _vertID2coeffID[index];
				coeffs.insert(coeffs.end(),_local_fit_coeffs.begin()+istr,_local_fit_coeffs.begin()+istr+ncoeffs);
			}else if(HI3DCURVE==_geom){
				coords.insert(coords.end(),_local_coords.begin()+3*index,_local_coords.begin()+3*index+3);
				degree_out = _degrees_out[index]; interp = _interps[index];
				int ncoeffs = 3*(degree_out+1);
				size_t istr = _vertID2coeffID[index];
				coeffs.insert(coeffs.end(),_local_fit_coeffs.begin()+istr,_local_fit_coeffs.begin()+istr+ncoeffs);
			}
			return true;
		}
	}

	/****************************************************************
	 *  Basic Internal Routines to initialize and set fitting data  *
	 ****************************************************************/

	 int HiReconstruction::estimate_num_rings(int degree, bool interp){
		return interp?((degree+1)>>1)+((degree+1)&1):((degree+2)>>1)+((degree+2)&1);
	 }

	 ErrorCode HiReconstruction::vertex_get_incident_elements(const EntityHandle& vid, const int elemdim, std::vector<EntityHandle>& adjents){
	 	ErrorCode error;
	 	assert(elemdim==_dim);
	 #ifdef HIREC_USE_AHF
	 	error = ahf->get_up_adjacencies(vid,elemdim,adjents); MB_CHK_ERR(error);
	 #else
	 	error = mbImpl->get_adjacencies(&vid,1,elemdim,false,adjents); MB_CHK_ERR(error);
	 #endif
	 	return error;
	 }

	 ErrorCode HiReconstruction::obtain_nring_ngbvs(const EntityHandle vid, int ring, const int minpnts, Range& ngbvs){
	 	ErrorCode error;
	 	std::deque<EntityHandle> todo;
	 	todo.push_back(vid); ngbvs.insert(vid);
	 	EntityHandle pre,nxt;
	 	for(int i=1;i<=ring;++i){
	 		int count = todo.size();
	 		while(count){
	 			EntityHandle center = todo.front();
	 			todo.pop_front(); --count;
	 			std::vector<EntityHandle> adjents;
	 			error = vertex_get_incident_elements(center,_dim,adjents); MB_CHK_ERR(error);
	 			for(size_t j=0;j<adjents.size();++j){
	 				std::vector<EntityHandle> elemconn;
	 				error = mbImpl->get_connectivity(&adjents[j],1,elemconn); MB_CHK_ERR(error);
	 				int nvpe = elemconn.size();
	 				for(int k=0;k<nvpe;++k){
	 					if(elemconn[k]==center){
	 						pre = k==0?elemconn[nvpe-1]:elemconn[k-1];
	 						nxt = elemconn[(k+1)%nvpe];
	 						if(ngbvs.find(pre)==ngbvs.end()){
	 							ngbvs.insert(pre);
	 							todo.push_back(pre);
	 						}
	 						if(ngbvs.find(nxt)==ngbvs.end()){
	 							ngbvs.insert(nxt);
	 							todo.push_back(nxt);
	 						}
	 						break;
	 					}
	 				}
	 			}
	 		}
			if(_MAXPNTS<=(int)ngbvs.size()){
	 			//obtain enough points
	 			return error;
	 		}
	 		if(!todo.size()){
	 			//current ring cannot introduce any points, return incase deadlock
	 			return error;
	 		}
			if((i==ring)&& (minpnts>(int)ngbvs.size())){
	 			//reach maximum ring but not enough points
	 			++ring;
	 		}
	 	}
	 	return error;
	 }

	 void HiReconstruction::initialize_surf_geom(const int degree){
	 	if(!_hasderiv){
	 		compute_average_vertex_normals_surf();
	 		_hasderiv = true;
	 	}
	 	if(!_initfittings){
	 		int ncoeffspv = (degree+2)*(degree+1)/2;
	 		_degrees_out.assign(_nv2rec,0);
	 		_interps.assign(_nv2rec,false);
	 		_vertID2coeffID.resize(_nv2rec);
	 		_local_fit_coeffs.assign(_nv2rec*ncoeffspv,0);
	 		for(size_t i=0;i<_nv2rec;++i){
	 			_vertID2coeffID[i] = i*ncoeffspv;
	 		}
	 		_initfittings = true;
	 	}
	 }

	 void HiReconstruction::initialize_surf_geom(const size_t npts, const int* degrees){
	 	if(!_hasderiv){
	 		compute_average_vertex_normals_surf();
	 		_hasderiv = true;
	 	}
	 	if(!_initfittings){
	 		assert(_nv2rec==npts);
	 		_degrees_out.assign(_nv2rec,0);
	 		_interps.assign(_nv2rec,false);
	 		_vertID2coeffID.resize(_nv2rec);
	 		size_t index=0;
	 		for(size_t i=0;i<_nv2rec;++i){
	 			_vertID2coeffID[i] = index;
	 			index += (degrees[i]+2)*(degrees[i]+1)/2;
	 		}
	 		_local_fit_coeffs.assign(index,0);
	 		_initfittings = true;
	 	}
	 }

	 void HiReconstruction::initialize_3Dcurve_geom(const int degree){
	 	if(!_hasderiv){
	 		compute_average_vertex_tangents_curve();
	 		_hasderiv = true;
	 	}
	 	if(!_initfittings){
	 		int ncoeffspvpd = degree+1;
	 		_degrees_out.assign(_nv2rec,0);
	 		_interps.assign(_nv2rec,false);
	 		_vertID2coeffID.resize(_nv2rec);
	 		_local_fit_coeffs.assign(_nv2rec*ncoeffspvpd*3,0);
	 		for(size_t i=0;i<_nv2rec;++i){
	 			_vertID2coeffID[i] = i*ncoeffspvpd*3;
	 		}
	 		_initfittings = true;
	 	}
	 }

	 void HiReconstruction::initialize_3Dcurve_geom(const size_t npts, const int* degrees){
	 	if(!_hasderiv){
	 		compute_average_vertex_tangents_curve();
	 		_hasderiv = true;
	 	}
	 	if(!_hasfittings){
	 		assert(_nv2rec==npts);
	 		_degrees_out.assign(_nv2rec,0);
	 		_interps.assign(_nv2rec,false);
	 		_vertID2coeffID.reserve(_nv2rec);
	 		size_t index=0;
	 		for(size_t i=0;i<_nv2rec;++i){
	 			_vertID2coeffID[i] = index;
	 			index += 3*(degrees[i]+1);
	 		}
	 		_local_fit_coeffs.assign(index,0);
	 		_initfittings = true;
	 	}
	 }

	/* ErrorCode HiReconstruction::set_geom_data_surf(const EntityHandle vid, const double* coords, const double degree_out, const double* coeffs, bool interp)
	 {
	   return MB_SUCCESS;
	 }

	 ErrorCode HiReconstruction::set_geom_data_3Dcurve(const EntityHandle vid, const double* coords, const double degree_out, const double* coeffs, bool interp)
	 {
	   return MB_SUCCESS;
	 } */

   /*********************************************************
	* Routines for vertex normal/tangent vector estimation	*
	*********************************************************/
	 ErrorCode HiReconstruction::average_vertex_normal(const EntityHandle vid, double* nrm){
	 	ErrorCode error;
	 	std::vector<EntityHandle> adjfaces;
	 	error = vertex_get_incident_elements(vid,2,adjfaces); MB_CHK_ERR(error);
	 	int npolys = adjfaces.size();
	 	if(!npolys){
	 		MB_SET_ERR(MB_FAILURE,"Vertex has no incident 2D entities");
	 	}else{
	 		double v1[3],v2[3],v3[3],a[3],b[3],c[3];
	 		nrm[0] = nrm[1] = nrm[2] = 0;
	 		for(int i=0;i<npolys;++i){
	 			//get incident "triangles"
	 			std::vector<EntityHandle> elemconn;
	 			error = mbImpl->get_connectivity(&adjfaces[i],1,elemconn); MB_CHK_ERR(error);
	 			EntityHandle pre,nxt;
	 			int nvpe = elemconn.size();
	 			for(int j=0;j<nvpe;++j){
	 				if(vid==elemconn[j]){
	 					pre = j==0?elemconn[nvpe-1]:elemconn[j-1];
	 					nxt = elemconn[(j+1)%nvpe];
	 					break;
	 				}
	 			}
	 			//compute area weighted normals
	 			error = mbImpl->get_coords(&pre,1,a); MB_CHK_ERR(error);
	 			error = mbImpl->get_coords(&vid,1,b); MB_CHK_ERR(error);
	 			error = mbImpl->get_coords(&nxt,1,c); MB_CHK_ERR(error);
	 			DGMSolver::vec_linear_operation(3,1,c,-1,b,v1);
	 			DGMSolver::vec_linear_operation(3,1,a,-1,b,v2);
	 			DGMSolver::vec_crossprod(v1,v2,v3);
	 			DGMSolver::vec_linear_operation(3,1,nrm,1,v3,nrm);
	 		}
#ifndef NDEBUG
	 		assert ( DGMSolver::vec_normalize(3,nrm,nrm) );
#endif
	 	}
	 	return error;
	 }

	 ErrorCode HiReconstruction::compute_average_vertex_normals_surf(){
	 	if(_hasderiv){
	 		return MB_SUCCESS;
	 	}
	 	ErrorCode error;
	 	_local_coords.assign(9*_nv2rec,0);
	 	size_t index=0;
	 	for(Range::iterator ivert=_verts2rec.begin();ivert!=_verts2rec.end();++ivert,++index){
	 		error = average_vertex_normal(*ivert,&(_local_coords[9*index+6])); MB_CHK_ERR(error);
	 	}
	 	return error;
	 }

	 ErrorCode HiReconstruction::get_normals_surf(const Range& vertsh, double* nrms){
	 	ErrorCode error = MB_SUCCESS;
	 	if(_hasderiv){
	 		size_t id=0;
	 		for(Range::iterator ivert=vertsh.begin();ivert!=vertsh.end();++ivert,++id){
	 			int index = _verts2rec.index(*ivert);
	 		#ifdef MOAB_HAVE_MPI
	 			if(-1==index){
	 				//ghost vertex
	 				error = average_vertex_normal(*ivert,nrms+3*id); MB_CHK_ERR(error);
	 			}else{
	 				nrms[3*id] = _local_coords[9*index+6];
	 				nrms[3*id+1] = _local_coords[9*index+7];
	 				nrms[3*id+2] = _local_coords[9*index+8];
	 			}
	 		#else
	 			assert(-1!=index);
	 			nrms[3*id] = _local_coords[9*index+6];
	 			nrms[3*id+1] = _local_coords[9*index+7];
	 			nrms[3*id+2] = _local_coords[9*index+8];
	 		#endif
	 		}
	 	}else{
	 		size_t id=0;
	 		for(Range::iterator ivert=vertsh.begin();ivert!=vertsh.end();++ivert,++id){
	 			error = average_vertex_normal(*ivert,nrms+3*id); MB_CHK_ERR(error);
	 		}
	 	}
	 	return error;
	 }

	 ErrorCode HiReconstruction::average_vertex_tangent(const EntityHandle vid, double* tang){
	 	ErrorCode error;
	 	std::vector<EntityHandle> adjedges;
	 	error = vertex_get_incident_elements(vid,1,adjedges); MB_CHK_ERR(error);
	 	int nedges = adjedges.size();
	 	if(!nedges){
	 		MB_SET_ERR(MB_FAILURE,"Vertex has no incident edges");
	 	}else{
	 		assert(nedges<=2);
	 		tang[0] = tang[1] = tang[2] = 0;
	 		for(int i=0;i<nedges;++i){
	 			std::vector<EntityHandle> edgeconn;
	 			error = mbImpl->get_connectivity(&adjedges[i],1,edgeconn);
	 			double istr[3],iend[3],t[3];
	 			error = mbImpl->get_coords(&(edgeconn[0]),1,istr);
	 			error = mbImpl->get_coords(&(edgeconn[1]),1,iend);
	 			DGMSolver::vec_linear_operation(3,1,iend,-1,istr,t);
	 			DGMSolver::vec_linear_operation(3,1,tang,1,t,tang);
	 		}
#ifndef NDEBUG
	 		assert ( DGMSolver::vec_normalize(3,tang,tang) );
#endif
	 	}
	 	return error;
	 }

	 ErrorCode HiReconstruction::compute_average_vertex_tangents_curve(){
	 	if(_hasderiv){
	 		return MB_SUCCESS;
	 	}
	 	ErrorCode error;
	 	_local_coords.assign(3*_nv2rec,0);
	 	size_t index=0;
	 	for(Range::iterator ivert=_verts2rec.begin();ivert!=_verts2rec.end();++ivert,++index){
	 		error = average_vertex_tangent(*ivert,&(_local_coords[3*index])); MB_CHK_ERR(error);
	 	}
	 	return error;
	 }

	 ErrorCode HiReconstruction::get_tangents_curve(const Range& vertsh, double* tangs){
	 	ErrorCode error=MB_SUCCESS;
	 	if(_hasderiv){
	 		size_t id=0;
	 		for(Range::iterator ivert=vertsh.begin();ivert!=vertsh.end();++ivert,++id){
	 			int index = _verts2rec.index(*ivert);
	 		#ifdef MOAB_HAVE_MPI
	 			if(-1!=index){
	 				tangs[3*id] = _local_coords[3*index];
	 				tangs[3*id+1] = _local_coords[3*index+1];
	 				tangs[3*id+2] = _local_coords[3*index+2];
	 			}else{
	 				error = average_vertex_tangent(*ivert,tangs+3*id); MB_CHK_ERR(error);
	 			}
	 		#else
	 			assert(-1!=index);
	 			tangs[3*id] = _local_coords[3*index];
	 			tangs[3*id+1] = _local_coords[3*index+1];
	 			tangs[3*id+2] = _local_coords[3*index+2];
	 		#endif
	 		}
	 	}else{
	 		size_t id=0;
	 		for(Range::iterator ivert=vertsh.begin();ivert!=vertsh.end();++ivert,++id){
	 			error = average_vertex_tangent(*ivert,tangs+3*id); MB_CHK_ERR(error);
	 		}
	 	}
	 	return error;
	 }

	/************************************************
	*	Internal Routines for local WLS fittings	*
	*************************************************/

	 void HiReconstruction::polyfit3d_surf_get_coeff(const int nverts, const double* ngbcoords, const double* ngbnrms, int degree, const bool interp, const bool safeguard, const int ncoords, double* coords, const int ncoeffs, double* coeffs, int* degree_out, int* degree_pnt, int* degree_qr){
	 	if(nverts<=0){
	 		return;
	 	}

		//std::cout << "npnts in initial stencil = " << nverts << std::endl;
		//std::cout << "centered at (" << ngbcoords[0] << "," << ngbcoords[1] << "," << ngbcoords[2] << ")" << std::endl;

	 	//step 1. copmute local coordinate system
	 	double nrm[3] = {ngbnrms[0],ngbnrms[1],ngbnrms[2]}, tang1[3] = {0,0,0}, tang2[3] = {0,0,0};
	 	if(fabs(nrm[0])>fabs(nrm[1])&&fabs(nrm[0])>fabs(nrm[2])){
	 		tang1[1] = 1.0;
	 	}else{
	 		tang1[0] = 1.0;
	 	}

	 	DGMSolver::vec_projoff(3,tang1,nrm,tang1);
#ifndef NDEBUG
	 	assert (DGMSolver::vec_normalize(3,tang1,tang1));
#endif
	 	DGMSolver::vec_crossprod(nrm,tang1,tang2);
	 	if(9<=ncoords&&coords){
	 		coords[0] = tang1[0]; coords[1] = tang1[1]; coords[2] = tang1[2];
	 		coords[3] = tang2[0]; coords[4] = tang2[1]; coords[5] = tang2[2];
	 		coords[6] = nrm[0]; coords[7] = nrm[1]; coords[8] = nrm[2];
	 	}
	 	if(!ncoeffs||!coeffs){
	 		return;
	 	}else{
	 		assert(ncoeffs>=(degree+2)*(degree+1)/2);
	 	}

	 	//step 2. project onto local coordinates system
	 	int npts2fit = nverts-interp;
	 	if(0==npts2fit){
	 		*degree_out = *degree_pnt = *degree_qr = 0;
	 		for(int i=0;i<ncoeffs;++i){
	 			coeffs[i] = 0;
	 		}
	 		//coeffs[0] = 0;
	 		return;
	 	}
	 	std::vector<double> us(npts2fit*2); //double *us = new double[npts2fit*2];
	 	std::vector<double> bs(npts2fit); //double *bs = new double[npts2fit];
	 	for(int i=interp;i<nverts;++i){
	 		int k = i-interp;
	 		double uu[3];
	 		DGMSolver::vec_linear_operation(3,1,ngbcoords+3*i,-1,ngbcoords,uu);
	 		us[k*2] = DGMSolver::vec_innerprod(3,tang1,uu); us[k*2+1] = DGMSolver::vec_innerprod(3,tang2,uu);
	 		bs[k] = DGMSolver::vec_innerprod(3,nrm,uu);
	 	}

	 	//step 3. compute weights
	 	std::vector<double> ws(npts2fit); //double *ws = new double[npts2fit];
	 	int nzeros = compute_weights(npts2fit,2,&(us[0]),nverts,ngbnrms,degree,_MINEPS,&(ws[0]));

	 	//step 4. adjust according to zero-weights
	 	if(nzeros){
	 		if(nzeros==npts2fit){
	 			*degree_out = *degree_pnt = *degree_qr = 0;
	 			for(int i=0;i<ncoeffs;++i){
	 				coeffs[i] = 0;
	 			}
	 			//coeffs[0] = 0;
	 			return;
	 		}
	 		int index=0;
	 		for(int i=0;i<npts2fit;++i){
	 			if(ws[i]){
	 				if(i>index){
	 					us[index*2] = us[i*2]; us[index*2+1] = us[i*2+1];
	 					bs[index] = bs[i]; ws[index] = ws[i];
	 				}
	 				++index;
	 			}
	 		}
	 		npts2fit -= nzeros; assert(index==npts2fit);
	 		us.resize(npts2fit*2); bs.resize(npts2fit); ws.resize(npts2fit);
	 		/*us = realloc(us,npts2fit*2*sizeof(double));
	 		bs = realloc(bs,npts2fit*sizeof(double));
	 		ws = realloc(ws,npts2fit*sizeof(double));*/
	 	}

		//std::cout<<"npnts after weighting = "<<npts2fit<<std::endl;

	 	//step 5. fitting
	 	eval_vander_bivar_cmf(npts2fit,&(us[0]),1,&(bs[0]),degree,&(ws[0]),interp,safeguard,degree_out,degree_pnt,degree_qr);

	 	//step 6. organize output
	 	int ncoeffs_out = (*degree_out+2)*(*degree_out+1)/2;
	 	assert(ncoeffs_out<=ncoeffs);
	 	coeffs[0] = 0;
	 	for(int j=0;j<ncoeffs_out-interp;++j){
	 		coeffs[j+interp] =  bs[j];
	 	}
	 	//delete [] us; delete [] bs; delete [] ws;
	 }

	 void HiReconstruction::eval_vander_bivar_cmf(const int npts2fit, const double* us, const int ndim, double* bs, int degree, const double* ws, const bool interp, const bool safeguard, int* degree_out, int* degree_pnt, int* degree_qr){
	 	//step 1. adjust the degree according to number of points to fit
	 	int ncols = (((degree+2)*(degree+1))>>1)-interp;
	 	while(1<degree&&npts2fit<ncols){
	 		--degree;
	 		ncols = (((degree+2)*(degree+1))>>1)-interp;
	 	}
	 	*degree_pnt = degree;

	 	//std::cout << "degree_pnt: " << *degree_pnt << std::endl;

	 	//step 2. construct Vandermonde matrix, stored in columnwise
	 	std::vector<double> V;//V(npts2fit*(ncols+interp)); //double *V_init = new double[npts2fit*(ncols+interp)];
	 	DGMSolver::gen_vander_multivar(npts2fit,2,us,degree,V);
	 	//remove the first column of 1s if interpolation
	 	if(interp){
	 		V.erase(V.begin(),V.begin()+npts2fit);
	 	}
	 	/*double* V;
	 	if(interp){
	 		V = new double[npts2fit*ncols];
	 		std::memcpy(V,V_init+npts2fit,ncols*npts2fit*sizeof(double));
	 		delete [] V_init; V_init = 0;
	 	}else{
	 		V = V_init;
	 	}*/

	 	//step 3. Scale rows to assign different weights to different points
	 	for(int i=0;i<npts2fit;++i){
	 		for(int j=0;j<ncols;++j){
	 			V[j*npts2fit+i] *= ws[i];
	 		}
	 		for(int k=0;k<ndim;++k){
	 			bs[k*npts2fit+i] *= ws[i];
	 		}
	 	}

	 	//step 4. scale columns to reduce condition number
	 	std::vector<double> ts(ncols); //double *ts = new double[ncols];
	 	DGMSolver::rescale_matrix(npts2fit,ncols,&(V[0]),&(ts[0]));

	 	//step 5. Perform Householder QR factorization
	 	std::vector<double> D(ncols); //double *D = new double[ncols];
	 	int rank;
	 	DGMSolver::qr_polyfit_safeguarded(npts2fit,ncols,&(V[0]),&(D[0]),&rank);

	 	//step 6. adjust degree of fitting according to rank of Vandermonde matrix
	 	int ncols_sub = ncols;
	 	while(rank<ncols_sub){
	 		--degree;
	 		if(degree==0){
	 			//surface is flat, return 0
	 			*degree_out = *degree_qr = degree;
	 			for(int i=0;i<npts2fit;++i){
	 				for(int k=0;k<ndim;++k){
	 					bs[k*npts2fit+i] = 0;
	 				}
	 			}
	 			return;
	 		}else{
	 			ncols_sub = (((degree+2)*(degree+1))>>1)-interp;
	 		}
	 	}
	 	*degree_qr = degree;

	 	//std::cout << "degree_qr: " << *degree_qr << std::endl;

		/* DBG
		 * std::cout<<"before Qtb"<<std::endl;
		std::cout<<std::endl;
		std::cout<<"bs = "<<std::endl;
		std::cout<<std::endl;
		for (int k=0; k< ndim; k++){
		    for (int j=0; j<npts2fit; ++j){
			std::cout<<"  "<<bs[npts2fit*k+j]<<std::endl;
		      }
		  }
		std::cout<<std::endl;*/

	 	//step 7. compute Q'b
	 	DGMSolver::compute_qtransposeB(npts2fit,ncols_sub,&(V[0]),ndim,bs);

		/* DBG
		 * std::cout<<"after Qtb"<<std::endl;
		std::cout<<"bs = "<<std::endl;
		std::cout<<std::endl;
		for (int k=0; k< ndim; k++){
		    for (int j=0; j<npts2fit; ++j){
			std::cout<<"  "<<bs[npts2fit*k+j]<<std::endl;
		      }
		  }
		std::cout<<std::endl;*/

	 	//step 8. perform backward substitution and scale the solution
	 	//assign diagonals of V
	 	for(int i=0;i<ncols_sub;++i){
	 		V[i*npts2fit+i] = D[i];
	 	}

	 	//backsolve
	 	if(safeguard){
	 		//for debug
	 		//std::cout << "ts size " << ts.size() << std::endl;
			DGMSolver::backsolve_polyfit_safeguarded(2,degree,interp,npts2fit,ncols_sub,&(V[0]),ndim,bs,&(ts[0]),degree_out);
	 	}else{
	 		DGMSolver::backsolve(npts2fit,ncols_sub,&(V[0]),1,bs,&(ts[0]));
	 		*degree_out = degree;
	 	}
	 	/*if(V_init){
	 		delete [] V_init;
	 	}else{
	 		delete [] V;
	 	}*/
	 }

	 void HiReconstruction::polyfit3d_curve_get_coeff(const int nverts, const double* ngbcors, const double* ngbtangs, int degree, const bool interp, const bool safeguard, const int ncoords, double* coords, const int ncoeffs, double* coeffs, int* degree_out){
	 	if(!nverts){
	 		return;
	 	}
	 	//step 1. compute local coordinates system
	 	double tang[3] = {ngbtangs[0],ngbtangs[1],ngbtangs[2]};
	 	if(coords&&ncoords>2){
	 		coords[0] = tang[0]; coords[1] = tang[1]; coords[2] = tang[2];
	 	}
	 	if(!coeffs||!ncoeffs){
	 		return;
	 	}else{
	 		assert(ncoeffs>=3*(degree+1));
	 	}
	 	//step 2. project onto local coordinate system
	 	int npts2fit = nverts-interp;
	 	if(!npts2fit){
	 		*degree_out = 0;
	 		for(int i=0;i<ncoeffs;++i){
	 			coeffs[0] = 0;
	 		}
	 		return;
	 	}
	 	std::vector<double> us(npts2fit); //double *us = new double[npts2fit];
	 	std::vector<double> bs(npts2fit*3); //double *bs = new double[npts2fit*3];
	 	double uu[3];
	 	for(int i=interp;i<nverts;++i){
	 		int k=i-interp;
	 		DGMSolver::vec_linear_operation(3,1,ngbcors+3*i,-1,ngbcors,uu);
	 		us[k] = DGMSolver::vec_innerprod(3,uu,tang);
	 		bs[k] = uu[0]; bs[npts2fit+k] = uu[1]; bs[2*npts2fit+k] = uu[2];
	 	}

	 	//step 3. copmute weights
	 	std::vector<double> ws(npts2fit);
	 	int nzeros = compute_weights(npts2fit,1,&(us[0]),nverts,ngbtangs,degree,_MINEPS,&(ws[0])); assert(nzeros<=npts2fit);

	 	//step 4. adjust according to number of zero-weights
	 	if(nzeros){
	 		if(nzeros==npts2fit){
	 			//singular case
	 			*degree_out = 0;
	 			for(int i=0;i<ncoeffs;++i){
	 				coeffs[i] =0;
	 			}
	 			return;
	 		}
	 		int npts_new = npts2fit-nzeros;
	 		std::vector<double> bs_temp(3*npts_new);
	 		int index=0;
	 		for(int i=0;i<npts2fit;++i){
	 			if(ws[i]){
	 				if(i>index){
	 					us[index] = us[i]; ws[index] = ws[i];
	 				}
	 				bs_temp[index] = bs[i]; bs_temp[index+npts_new] = bs[i+npts2fit]; bs_temp[index+2*npts_new] = bs[i+2*npts2fit];
	 				++index;
	 			}
	 		}
	 		assert(index==npts_new);
	 		npts2fit = npts_new;
	 		us.resize(index); ws.resize(index); bs = bs_temp;
	 		//destroy bs_temp;
	 		std::vector<double>().swap(bs_temp);
	 	}

	 	//step 5. fitting
	 	eval_vander_univar_cmf(npts2fit,&(us[0]),3,&(bs[0]),degree,&(ws[0]),interp,safeguard,degree_out);
	 	//step 6. write results to output pointers
	 	int ncoeffs_out_pvpd = *degree_out+1;
	 	assert(ncoeffs>=3*ncoeffs_out_pvpd);
	 	//write to coeffs consecutively, bs's size is not changed by eval_vander_univar_cmf
	 	coeffs[0] = coeffs[ncoeffs_out_pvpd] = coeffs[2*ncoeffs_out_pvpd] = 0;
	 	for(int i=0;i<ncoeffs_out_pvpd-interp;++i){
	 		coeffs[i+interp] = bs[i];
	 		coeffs[i+interp+ncoeffs_out_pvpd] = bs[i+npts2fit];
	 		coeffs[i+interp+2*ncoeffs_out_pvpd] = bs[i+2*npts2fit];
	 	}
	 }

	 void HiReconstruction::eval_vander_univar_cmf(const int npts2fit, const double* us, const int ndim, double* bs, int degree, const double* ws, const bool interp, const bool safeguard, int* degree_out){
	 	//step 1. determine degree of polynomials to fit according to number of points
	 	int ncols = degree+1-interp;
	 	while(npts2fit<ncols&&degree>=1){
	 		--degree;
	 		ncols = degree+1-interp;
	 	}
	 	if(!degree){
	 		if(interp){
	 			for(int icol=0;icol<ndim;++icol){
	 				bs[icol*npts2fit] = 0;
	 			}
	 		}
	 		for(int irow=1;irow<npts2fit;++irow){
	 			for(int icol=0;icol<ndim;++icol){
	 				bs[icol*npts2fit+irow] = 0;
	 			}
	 		}
	 		*degree_out = 0;
	 		return;
	 	}
	 	//step 2. construct Vandermonde matrix
	 	std::vector<double> V;//V(npts2fit*(ncols+interp));
	 	DGMSolver::gen_vander_multivar(npts2fit,1,us,degree,V);

	 	if(interp){
	 		V.erase(V.begin(),V.begin()+npts2fit);
	 	}

	 	//step 3. scale rows with respect to weights
	 	for(int i=0;i<npts2fit;++i){
	 		for(int j=0;j<ncols;++j){
	 			V[j*npts2fit+i] *= ws[i];
	 		}
	 		for(int k=0;k<ndim;++k){
	 			bs[k*npts2fit+i] *= ws[i];
	 		}
	 	}

	 	//step 4. scale columns to reduce condition number
	 	std::vector<double> ts(ncols);
	 	DGMSolver::rescale_matrix(npts2fit,ncols,&(V[0]),&(ts[0]));

	 	//step 5. perform Householder QR factorization
	 	std::vector<double> D(ncols);
	 	int rank;
	 	DGMSolver::qr_polyfit_safeguarded(npts2fit,ncols,&(V[0]),&(D[0]),&rank);

	 	//step 6. adjust degree of fitting
	 	int ncols_sub = ncols;
	 	while(rank<ncols_sub){
	 		--degree;
	 		if(!degree){
	 			//matrix is singular, consider curve on flat plane passing center and orthogonal to tangent line
	 			*degree_out = 0;
	 			for(int i=0;i<npts2fit;++i){
	 				for(int k=0;k<ndim;++k){
	 					bs[k*npts2fit+i] = 0;
	 				}
	 			}
	 		}
	 		ncols_sub = degree+1-interp;
	 	}

	 	//step 7. compute Q'*bs
	 	DGMSolver::compute_qtransposeB(npts2fit,ncols_sub,&(V[0]),ndim,bs);

	 	//step 8. perform backward substitution and scale solutions
	 	//assign diagonals of V
	 	for(int i=0;i<ncols_sub;++i){
	 		V[i*npts2fit+i] = D[i];
	 	}
	 	//backsolve
	 	if(safeguard){
			DGMSolver::backsolve_polyfit_safeguarded(1,degree,interp,npts2fit,ncols,&(V[0]),ndim,bs,ws,degree_out);
	 	}else{
	 		DGMSolver::backsolve(npts2fit,ncols_sub,&(V[0]),ndim,bs,&(ts[0]));
	 		*degree_out = degree;
	 	}
	 }

	 int HiReconstruction::compute_weights(const int nrows, const int ncols, const double* us, const int nngbs, const double* ngbnrms, const int degree, const double toler, double* ws){
	 	assert(nrows<=_MAXPNTS&&ws);
	 	bool interp=false;
	 	if(nngbs!=nrows){
	 		assert(nngbs==nrows+1);
	 		interp = true;
	 	}
	 	double epsilon = 1e-2;

	 	//First, compute squared distance from each input piont to the center
	 	for(int i=0;i<nrows;++i){
	 		ws[i] = DGMSolver::vec_innerprod(ncols,us+i*ncols,us+i*ncols);
	 	}

	 	//Second, compute a small correction termt o guard against zero
	 	double h=0;
	 	for(int i=0;i<nrows;++i){
	 		h += ws[i];
	 	}
	 	h /= (double) nrows;

	 	//Finally, compute the weights for each vertex
	 	int nzeros = 0;
	 	for(int i=0;i<nrows;++i){
	 		double costheta = DGMSolver::vec_innerprod(3,ngbnrms,ngbnrms+3*(i+interp));
	 		if(costheta>toler){
	 			ws[i] = costheta*pow(ws[i]/h+epsilon,-1*(double) degree/2.0);
	 		}else{
	 			ws[i] = 0;
	 			++nzeros;
	 		}
	 	}
	 	return nzeros;
	 }
	 bool HiReconstruction::check_barycentric_coords(const int nws, const double* naturalcoords){
		double sum=0;
		for(int i=0;i<nws;++i){
			if(naturalcoords[i]<-_MINEPS){
				return false;
			}
			sum += naturalcoords[i];
		}
		if(fabs(1-sum)>_MINEPS){
			return false;
		}else{
			return true;
		}
	}
}//namespace moab
