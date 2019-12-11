///////////////////////////////////////////////////////////////////////////////
///
///	\file    OfflineMap.h
///	\author  Paul Ullrich
///	\version August 14, 2014
///
///	<remarks>
///		Copyright 2000-2014 Paul Ullrich
///
///		This file is distributed as part of the Tempest source code package.
///		Permission is granted to use, copy, modify and distribute this
///		source code and its documentation under the terms of the GNU General
///		Public License.  This software is provided "as is" without express
///		or implied warranty.
///	</remarks>

#ifndef _TEMPESTOFFLINEMAP_H_
#define _TEMPESTOFFLINEMAP_H_

#include "moab/MOABConfig.h"

#include "SparseMatrix.h"
#include "DataVector.h"
#include "DataMatrix.h"
#include "DataMatrix3D.h"
#include "OfflineMap.h"
#include <string>
#include <vector>

#ifdef MOAB_HAVE_EIGEN
#include <Eigen/Sparse>
#endif

#include "moab/Remapping/TempestRemapper.hpp"

///////////////////////////////////////////////////////////////////////////////

#define RECTANGULAR_TRUNCATION
// #define TRIANGULAR_TRUNCATION

///////////////////////////////////////////////////////////////////////////////

// Forward declarations
class Mesh;

///////////////////////////////////////////////////////////////////////////////

namespace moab
{

///	<summary>
///		An offline map between two Meshes.
///	</summary>
class TempestOfflineMap : public OfflineMap {

public:

	///	<summary>
	///		Generate the metadata associated with the offline map.
	///	</summary>
	TempestOfflineMap(moab::TempestRemapper* remapper);

	///	<summary>
	///		Define a virtual destructor.
	///	</summary>
	virtual ~TempestOfflineMap();

public:

    // Input / Output types
    enum DiscretizationType
    {
        DiscretizationType_FV,
        DiscretizationType_CGLL,
        DiscretizationType_DGLL
    };

	///	<summary>
	///		Generate the offline map, given the source and target mesh and discretization details.
	///     This method generates the mapping between the two meshes based on the overlap and stores 
	///     the result in the SparseMatrix.
	///	</summary>
	moab::ErrorCode GenerateOfflineMap( std::string strInputType="fv", std::string strOutputType="fv",
                                        const int nPin=1, const int nPout=1,
                                        bool fBubble=false, int fMonotoneTypeID=0,
                                        bool fVolumetric=false, bool fNoConservation=false, bool fNoCheck=false,
                                        const std::string srcDofTagName="GLOBAL_ID", const std::string tgtDofTagName="GLOBAL_ID",
                                        const std::string strVariables="", 
                                        const std::string strInputData="", const std::string strOutputData="",
                                        const std::string strNColName="", const bool fOutputDouble=false,
                                        const std::string strPreserveVariables="", const bool fPreserveAll=false, const double dFillValueOverride=0.0,
                                        const bool fInputConcave = false, const bool fOutputConcave = false );

	///	<summary>
	///		Generate the metadata associated with the offline map.
	///	</summary>
	// moab::ErrorCode GenerateMetaData();

public:

	///	<summary>
	///		Read the OfflineMap from a NetCDF file.
	///	</summary>
	// virtual void Read(
	// 	const std::string & strSource
	// );

	///	<summary>
	///		Write the TempestOfflineMap to a parallel NetCDF file.
	///	</summary>
	// virtual void Write(
	// 	const std::string & strTarget
	// );

public:
	///	<summary>
	///		Determine if the map is first-order accurate.
	///	</summary>
	virtual bool IsConsistent(
		double dTolerance
	);

	///	<summary>
	///		Determine if the map is conservative.
	///	</summary>
	virtual bool IsConservative(
		double dTolerance
	);

	///	<summary>
	///		Determine if the map is monotone.
	///	</summary>
	virtual bool IsMonotone(
		double dTolerance
	);

	///	<summary>
	///		If we computed the reduction, get the vector representing the source areas for all entities in the mesh
	///	</summary>
	const DataVector<double>& GetGlobalSourceAreas() const;

	///	<summary>
	///		If we computed the reduction, get the vector representing the target areas for all entities in the mesh
	///	</summary>
	const DataVector<double>& GetGlobalTargetAreas() const;

#ifdef MOAB_HAVE_EIGEN
	void InitVectors();
#endif

private:

	///	<summary>
	///		Gather the mapping matrix that was computed in different processors and accumulate the data
	///     on the root so that OfflineMap can be generated in parallel.
	///	</summary>
	moab::ErrorCode GatherAllToRoot();

	///	<summary>
	///		Compute the remapping weights for a FV field defined on the source to a 
	///     FV field defined on the target mesh.
	///	</summary>
	void LinearRemapFVtoFV_Tempest_MOAB( int nOrder );

	///	<summary>
	///		Generate the OfflineMap for linear conserative element-average
	///		spectral element to element average remapping.
	///	</summary>
	void LinearRemapSE0_Tempest_MOAB(
		const DataMatrix3D<int> & dataGLLNodes,
		const DataMatrix3D<double> & dataGLLJacobian);

	///	<summary>
	///		Generate the OfflineMap for cubic conserative element-average
	///		spectral element to element average remapping.
	///	</summary>
	void LinearRemapSE4_Tempest_MOAB(
	    const DataMatrix3D<int> & dataGLLNodes,
	    const DataMatrix3D<double> & dataGLLJacobian,
	    int nMonotoneType,
	    bool fContinuousIn,
	    bool fNoConservation);

	///	<summary>
	///		Generate the OfflineMap for remapping from finite volumes to finite
	///		elements using simple sampling of the FV reconstruction.
	///	</summary>
	void LinearRemapFVtoGLL_Simple_MOAB(
		const DataMatrix3D<int> & dataGLLNodes,
		const DataMatrix3D<double> & dataGLLJacobian,
		const DataVector<double> & dataGLLNodalArea,
		int nOrder,
		int nMonotoneType,
		bool fContinuous,
		bool fNoConservation
	);

	///	<summary>
	///		Generate the OfflineMap for remapping from finite volumes to finite
	///		elements using a new experimental method.
	///	</summary>
	void LinearRemapFVtoGLL_Volumetric_MOAB(
		const DataMatrix3D<int> & dataGLLNodes,
		const DataMatrix3D<double> & dataGLLJacobian,
		const DataVector<double> & dataGLLNodalArea,
		int nOrder,
		int nMonotoneType,
		bool fContinuous,
		bool fNoConservation
	);

	///	<summary>
	///		Generate the OfflineMap for remapping from finite volumes to finite
	///		elements.
	///	</summary>
	void LinearRemapFVtoGLL_MOAB(
		const DataMatrix3D<int> & dataGLLNodes,
		const DataMatrix3D<double> & dataGLLJacobian,
		const DataVector<double> & dataGLLNodalArea,
		int nOrder,
		int nMonotoneType,
		bool fContinuous,
		bool fNoConservation
	);

	///	<summary>
	///		Generate the OfflineMap for remapping from finite elements to finite
	///		elements.
	///	</summary>
	void LinearRemapGLLtoGLL2_MOAB(
		const DataMatrix3D<int> & dataGLLNodesIn,
		const DataMatrix3D<double> & dataGLLJacobianIn,
		const DataMatrix3D<int> & dataGLLNodesOut,
		const DataMatrix3D<double> & dataGLLJacobianOut,
		const DataVector<double> & dataNodalAreaOut,
		int nPin,
		int nPout,
		int nMonotoneType,
		bool fContinuousIn,
		bool fContinuousOut,
		bool fNoConservation
	);

	///	<summary>
	///		Generate the OfflineMap for remapping from finite elements to finite
	///		elements (pointwise interpolation).
	///	</summary>
	void LinearRemapGLLtoGLL2_Pointwise_MOAB(
		const DataMatrix3D<int> & dataGLLNodesIn,
		const DataMatrix3D<double> & dataGLLJacobianIn,
		const DataMatrix3D<int> & dataGLLNodesOut,
		const DataMatrix3D<double> & dataGLLJacobianOut,
		const DataVector<double> & dataNodalAreaOut,
		int nPin,
		int nPout,
		int nMonotoneType,
		bool fContinuousIn,
		bool fContinuousOut
	);

	///	<summary>
	///		Store the tag names associated with global DoF ids for source and target meshes
	///	</summary>
	moab::ErrorCode SetDofMapTags(const std::string srcDofTagName, 
								  const std::string tgtDofTagName);

	///	<summary>
	///		Compute the association between the solution tag global DoF numbering and 
	///		the local matrix numbering so that matvec operations can be performed
	///     consistently.
	///	</summary>
	moab::ErrorCode SetDofMapAssociation(DiscretizationType srcType, bool isSrcContinuous, 
		DataMatrix3D<int>* srcdataGLLNodes, DataMatrix3D<int>* srcdataGLLNodesSrc,
		DiscretizationType destType, bool isDestContinuous, 
		DataMatrix3D<int>* tgtdataGLLNodes);


	///	<summary>
	///		Copy the local matrix from Tempest SparseMatrix representation (ELL)
	///		to the parallel CSR Eigen Matrix for scalable application of matvec
	///     needed for projections.
	///	</summary>
#ifdef MOAB_HAVE_EIGEN
	void CopyTempestSparseMat_Eigen();
#endif

public:
#ifdef MOAB_HAVE_EIGEN

	typedef Eigen::Matrix< double, 1, Eigen::Dynamic > WeightDRowVector;
	typedef Eigen::Matrix< double, Eigen::Dynamic, 1 > WeightDColVector;
	typedef Eigen::SparseVector<double> WeightSVector;
	typedef Eigen::SparseMatrix<double, Eigen::RowMajor> WeightRMatrix;
	typedef Eigen::SparseMatrix<double, Eigen::ColMajor> WeightCMatrix;

	typedef WeightDRowVector WeightRowVector;
	typedef WeightDColVector WeightColVector;
	typedef WeightRMatrix WeightMatrix;

	///	<summary>
	///		Get the raw reference to the Eigen weight matrix representing the projection from source to destination mesh.
	///	</summary>
	WeightMatrix& GetWeightMatrix();

	///	<summary>
	///		Get the row vector that is amenable for application of A*x operation.
	///	</summary>
	WeightRowVector& GetRowVector();

	///	<summary>
	///		Get the column vector that is amenable for application of A^T*x operation.
	///	</summary>
	WeightColVector& GetColVector();

  ///	<summary>
	///		Get the number of total Degrees-Of-Freedom defined on the source mesh.
	///	</summary>
	int GetSourceGlobalNDofs();

	///	<summary>
	///		Get the number of total Degrees-Of-Freedom defined on the destination mesh.
	///	</summary>
	int GetDestinationGlobalNDofs();

	///	<summary>
	///		Get the number of local Degrees-Of-Freedom defined on the source mesh.
	///	</summary>
	int GetSourceLocalNDofs();

	///	<summary>
	///		Get the number of local Degrees-Of-Freedom defined on the destination mesh.
	///	</summary>
	int GetDestinationLocalNDofs();

	///	<summary>
	///		Get the number of Degrees-Of-Freedom per element on the source mesh.
	///	</summary>
	int GetSourceNDofsPerElement();

	///	<summary>
	///		Get the number of Degrees-Of-Freedom per element on the destination mesh.
	///	</summary>
	int GetDestinationNDofsPerElement();

	///	<summary>
	///		Apply the weight matrix onto the source vector provided as input, and return the column vector (solution projection) after the application 
	///     Compute:        \p tgtVals = A * \srcVals, or 
	///     if (transpose)  \p tgtVals = A^T * \srcVals
	///	</summary>
	moab::ErrorCode ApplyWeights (std::vector<double>& srcVals, std::vector<double>& tgtVals, bool transpose=false);

	///	<summary>
	///		Parallel I/O with NetCDF to write out the SCRIP file from multiple processors.
	///	</summary>
	void WriteParallelWeightsToFile(std::string filename);

#endif

public:

	///	<summary>
	///		The fundamental remapping operator object.
	///	</summary>
	moab::TempestRemapper* m_remapper;

	///	<summary>
	///		The SparseMatrix representing this operator.
	///	</summary>
	// SparseMatrix<double> m_mapRemapGlobal;
	OfflineMap* m_weightMapGlobal;

	///	<summary>
	///		The boolean flag representing whether the root process has the updated global view.
	///	</summary>
	bool m_globalMapAvailable;

#ifdef MOAB_HAVE_EIGEN

	WeightMatrix m_weightMatrix;
	WeightRowVector m_rowVector;
	WeightColVector m_colVector;

#endif

	///	<summary>
	///		The DataVector that stores the global (GID-based) areas of the source mesh.
	///	</summary>
	// DataVector<double> m_areasSrcGlobal;
	
	///	<summary>
	///		The DataVector that stores the global (GID-based) areas of the target mesh.
	///	</summary>
	// DataVector<double> m_areasTgtGlobal;

	///	<summary>
	///		The reference to the moab::Core object that contains source/target and overlap sets.
	///	</summary>
	moab::Interface* mbCore;

#ifdef MOAB_HAVE_MPI
	///	<summary>
	///		The reference to the parallel communicator object used by the Core object.
	///	</summary>
	moab::ParallelComm* pcomm;
#endif

	///	<summary>
	///		The original tag data and local to global DoF mapping to associate matrix values to solution
	///	<summary>
	moab::Tag m_dofTagSrc, m_dofTagDest;
	std::vector<unsigned long> row_dofmap, col_dofmap, srccol_dofmap;
	std::vector<unsigned long> row_gdofmap, col_gdofmap, srccol_gdofmap;
	std::vector<unsigned long> row_ldofmap, col_ldofmap, srccol_ldofmap;

	DataMatrix3D<int> dataGLLNodesSrc, dataGLLNodesSrcCov, dataGLLNodesDest;
	DiscretizationType m_srcDiscType, m_destDiscType;
	int m_nTotDofs_Src, m_nTotDofs_SrcCov, m_nTotDofs_Dest;
	int m_nDofsPEl_Src, m_nDofsPEl_Dest;

	Mesh* m_meshInput;
	Mesh* m_meshInputCov;
	Mesh* m_meshOutput;
	Mesh* m_meshOverlap;
	
	bool is_parallel, is_root;
	int rank, size;
};

///////////////////////////////////////////////////////////////////////////////

inline
const DataVector<double>& TempestOfflineMap::GetGlobalSourceAreas() const {
#ifdef MOAB_HAVE_MPI
  if (pcomm->size() > 1) {
        return m_weightMapGlobal->GetSourceAreas();
	}
	else {
		return this->GetSourceAreas();
	}
#else
  return this->GetSourceAreas();
#endif
}

inline
const DataVector<double>& TempestOfflineMap::GetGlobalTargetAreas() const {
#ifdef MOAB_HAVE_MPI
  if (pcomm->size() > 1) {
        return m_weightMapGlobal->GetTargetAreas();
	}
	else {
		return this->GetTargetAreas();
	}
#else
  return this->GetTargetAreas();
#endif
}

}

#endif

