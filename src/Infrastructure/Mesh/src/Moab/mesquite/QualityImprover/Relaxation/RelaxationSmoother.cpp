#include "RelaxationSmoother.hpp"

namespace MBMesquite
{

RelaxationSmoother::~RelaxationSmoother() {}

void RelaxationSmoother::initialize( PatchData& /*pd*/, MsqError& /*err*/ ) {}

void RelaxationSmoother::initialize_mesh_iteration( PatchData& /*pd*/, MsqError& /*err*/ )
{
    //  cout << "- Executing LaplacianSmoother::iteration_complete()\n";
}

void RelaxationSmoother::terminate_mesh_iteration( PatchData& /*pd*/, MsqError& /*err*/ )
{
    //  cout << "- Executing LaplacianSmoother::iteration_complete()\n";
}

void RelaxationSmoother::cleanup()
{
    //  cout << "- Executing LaplacianSmoother::iteration_end()\n";
}

}  // namespace MBMesquite
