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
#include <Mesh/include/ESMCI_WMat.h>
#include <Mesh/include/ESMCI_Attr.h>
#include <Mesh/include/ESMCI_MeshUtils.h>

namespace ESMCI {



/*-----------------------------------------------------------------*/
// WMat 
/*-----------------------------------------------------------------*/
WMat::WMat() 
{
}

WMat::WMat(const WMat &rhs)
{
}

WMat::~WMat() {
}

WMat &WMat::operator=(const WMat &rhs) 
{

  if (this == &rhs) return *this;

  weights = rhs.weights;
  
  return *this;
}

void WMat::InsertRow(const Entry &row, const std::vector<Entry> &cols) {

  
  std::pair<WeightMap::iterator, bool> wi =
    weights.insert(std::make_pair(row, cols));
    
  if (wi.second == false) {
    // Just verify that entries are the same
    std::vector<Entry> &tcol = wi.first->second;
    ThrowRequire(tcol.size() == cols.size());
    for (UInt i = 0; i < cols.size(); i++) {
      ThrowRequire(tcol[i].id == cols[i].id);
      ThrowRequire(tcol[i].idx == cols[i].idx);
      ThrowRequire(std::abs(tcol[i].value-cols[i].value) < 1e-5);
    }
  } else {
    
    // Sort the column entries (invariant used elsewhere).
    std::sort(wi.first->second.begin(), wi.first->second.end());

    // compress storage
    std::vector<Entry>(wi.first->second).swap(wi.first->second);
    
  }
  
}

void WMat::Print(std::ostream &os) {
  
  WeightMap::iterator wi = weights.begin(), we = weights.end();
  
  for (; wi != we; ++wi) {
    os << "Row:" << wi->first;
    std::vector<Entry> &col = wi->second;
    
    double sum = 0.0;
    for (UInt i = 0; i < col.size(); i++) {
      
      os << col[i] << ", ";
      
      sum += col[i].value;
      
    }
    
    os << "SUM=" << sum << std::endl;
  }
  
}

//void WMat::Migrate(CommRel &crel) {
void WMat::Migrate(Mesh &mesh) { 
  Trace __trace("WMat::Migrate(Mesh &mesh)");
  
  // Gather pole constraints
  {
    std::vector<UInt> mesh_dist, iw_dist;
    
    Context c; c.set(Attr::ACTIVE_ID);
    Attr a(MeshObj::NODE, c);
    getMeshGIDS(mesh, a, mesh_dist);
    GetRowGIDS(iw_dist);
    
    Migrator mig(mesh_dist.size(), mesh_dist.size() > 0 ? &mesh_dist[0] : NULL, 0,
        iw_dist.size(), iw_dist.size() > 0 ? &iw_dist[0] : NULL);
    
    mig.Migrate(*this);
    
//#define CHECK_WEIGHT_MIG
#ifdef CHECK_WEIGHT_MIG
// Check something: should have 1 to 1 coresp ids and entries
for (UInt i = 0; i < mesh_dist.size(); i++) {
  Entry ent(mesh_dist[i]);
  WeightMap::iterator wi = weights.lower_bound(ent);
  if (wi == weights.end() || wi->first.id != ent.id)
    Throw() << "Did not find id:" << ent.id << std::endl;
}
// And the other way
std::sort(mesh_dist.begin(), mesh_dist.end());
WeightMap::iterator wi = weights.begin(), we = weights.end();
for (; wi != we; ++wi) {
  std::vector<UInt>::iterator lb =
    std::lower_bound(mesh_dist.begin(), mesh_dist.end(), wi->first.id);
  
  if (lb == mesh_dist.end() || *lb != wi->first.id)
    Throw() << "Weight entry:" << wi->first.id << " not a mesh id!";
}
#endif
  
    
  }
  
  return;
  
}
  
void WMat::clear() {
  
  // Loop weightmap; swap each row out
  WeightMap::iterator wi = begin_row(), we = end_row();
  
  for (; wi != we; ++wi)
    std::vector<Entry>().swap(wi->second);
  
  WeightMap().swap(weights);
  
}  

struct entry_mult {
  entry_mult(double _mval) : mval(_mval) {}
  WMat::Entry operator()(const WMat::Entry &rhs) {
    return WMat::Entry(rhs.id, rhs.idx, rhs.value*mval);
  }
  double mval;
};

void WMat::AssimilateConstraints(const WMat &constraints) {
  Trace __trace("WMat::AssimilateConstraints(const WMat &constraints)");
  
  // Loop the current entries; see if any constraint rows need to be resolved;
  WeightMap::iterator wi = weights.begin(), we = weights.end();
  
  for (; wi != we; ++wi) {
    
    std::vector<Entry> &col = wi->second;
    
    // Loop constraints; find the entries
    WeightMap::const_iterator ci = constraints.weights.begin(), ce = constraints.weights.end();
    
    for (; ci != ce; ++ci) {
      
      const Entry &crow = ci->first;
      
      std::vector<Entry>::iterator lb = 
        std::lower_bound(col.begin(), col.end(), crow);
        
      // If we found an entry, condense;
      if (lb != col.end() && *lb == crow) {
        
        double val = lb->value;
        
        const std::vector<Entry> &ccol = ci->second;
        
        // Delete entry
        col.erase(lb);
        
        // Add replacements to end
        std::transform(ccol.begin(), ccol.end(), std::back_inserter(col), entry_mult(val));
        
        // Now sort
        std::sort(col.begin(), col.end());
        
        // And condense any duplicates
        std::vector<Entry>::iterator condi = col.begin(), condn, cond_del;
        
        while (condi != col.end()) {
          
          condn = condi; condn++;
          
          // Sum in result while entries are duplicate
          while (condn != col.end() && *condn == *condi) {
            
            condi->value += condn->value;
            
            ++condn;
          }
          
          // Move to next entry
          ++condi;

          // Condense the list if condn != condi (there were duplicaes)
          if (condn != condi) {
            cond_del = std::copy(condn, col.end(), condi);
            
            col.erase(cond_del, col.end());
          }
          
        }  // condi; condensation loop
        
      } // Found an entry
      
    } // for ci
    
  } // for wi
  
}



void WMat::AssimilateConstraintsNPnts(const WMat &constraints) {
  Trace __trace("WMat::AssimilateConstraintsNPnts(const WMat &constraints)");
  
  // Loop the current entries; see if any constraint rows need to be resolved;
  WeightMap::iterator wi = weights.begin(), we = weights.end();
  
  for (; wi != we; ++wi) {
    
    const Entry &row = wi->first;

    std::vector<Entry> &col = wi->second;

    // Loop constraints; find the entries
    // Note for the constraints for NPnts the row entry.id is the src element
    // and the row entry.src_id is the dst id. This is because
    // this was the only way to get the migration to work based on src element id

    // Here we find the range of constraint rows which would contain row.src_id 
    //// Lower Bound
    Entry lower(row.src_id);
    WeightMap::const_iterator ci = constraints.weights.lower_bound(lower);

    //// Upper Bound
    Entry upper(row.src_id+1);
    WeightMap::const_iterator ce = constraints.weights.lower_bound(upper);

    // If there are no constraints which match continue to next row 
    if (ci == constraints.weights.end()) continue;

    // Loop over contraints which match
    for (; ci != ce; ++ci) {
     
      const Entry &crow = ci->first;
     
      // Skip if this constraint isn't for this row
      if (crow.id != row.src_id) continue;
 
      //      printf(">>>>>>> r0w=%d \n",row.id);
      // create entry that looks like constraint gid for the search
      // (e.g. the srd_id where the constraint id is stored)
      Entry t(crow.src_id);

      std::vector<Entry>::iterator lb = 
        std::lower_bound(col.begin(), col.end(), t);

      // If we found an entry, condense;
      if (lb != col.end() && *lb == t) {

        double val = lb->value;
        
        const std::vector<Entry> &ccol = ci->second;
        
        // Delete entry
        col.erase(lb);
        
        // Add replacements to end
        std::transform(ccol.begin(), ccol.end(), std::back_inserter(col), entry_mult(val));
        
        // Now sort
        std::sort(col.begin(), col.end());
        
        // And condense any duplicates
        std::vector<Entry>::iterator condi = col.begin(), condn, cond_del;
        
        while (condi != col.end()) {
          
          condn = condi; condn++;
          
          // Sum in result while entries are duplicate
          while (condn != col.end() && *condn == *condi) {
            
            condi->value += condn->value;
            
            ++condn;
          }
          
          // Move to next entry
          ++condi;

          // Condense the list if condn != condi (there were duplicaes)
          if (condn != condi) {
            cond_del = std::copy(condn, col.end(), condi);
            
            col.erase(cond_del, col.end());
          }
          
        }  // condi; condensation loop
        
      } // Found an entry
      
    } // for ci
    
  } // for wi
  
}


void WMat::GatherToCol(WMat &rhs) {
  Trace __trace("WMat::GatherToCol(WMat &rhs)");
  
  // Gather rhs to col dist of this
  {
    std::vector<UInt> distd, dists;
    
    this->GetColGIDS(distd);
    rhs.GetRowGIDS(dists);
    
    Migrator mig(distd.size(), distd.size() > 0 ? &distd[0] : NULL, 0,
        dists.size(), dists.size() > 0 ? &dists[0] : NULL);
    
    mig.Migrate(rhs);
    
  }
}



void WMat::GatherToRowSrc(WMat &rhs) {
  Trace __trace("WMat::GatherToCol(WMat &rhs)");
  
  // Gather rhs to col dist of this
  {
    std::vector<UInt> distd, dists;
    
    this->GetRowSrcGIDS(distd);
    rhs.GetRowGIDS(dists);

    Migrator mig(distd.size(), distd.size() > 0 ? &distd[0] : NULL, 0,
        dists.size(), dists.size() > 0 ? &dists[0] : NULL);
    
    mig.Migrate(rhs);
    
  }
}



void WMat::GetRowGIDS(std::vector<UInt> &gids) {
  Trace __trace("WMat::GetRowGIDS(std::vector<UInt> &gids)");
  
  gids.clear();
  
  WeightMap::iterator ri = weights.begin(), re = weights.end();
  
  for (; ri != re;) {
    
    gids.push_back(ri->first.id);
    
    UInt gid = ri->first.id;
    
    // Don't repeat a row:
    while (ri != re && ri->first.id == gid)
      ++ri;
    
  }
  
}


void WMat::GetRowSrcGIDS(std::vector<UInt> &gids) {
  Trace __trace("WMat::GetRowGIDS(std::vector<UInt> &gids)");
  
  gids.clear();
  
  WeightMap::iterator ri = weights.begin(), re = weights.end();
  
  for (; ri != re;) {
    
    gids.push_back(ri->first.src_id);
    
    UInt gid = ri->first.src_id;
    
    // Don't repeat a row:
    while (ri != re && ri->first.src_id == gid)
      ++ri;
    
  }
  
}



void WMat::GetColGIDS(std::vector<UInt> &gids) {
  Trace __trace("WMat::GetColGIDS(std::vector<UInt> &gids)");
  
  gids.clear();
  
  std::set<UInt> _gids; // use a set for efficieny

  WeightMap::iterator wi = weights.begin(), we = weights.end();
  
  for (; wi != we; ++wi) {
    
    std::vector<Entry> &col = wi->second;
    
    for (UInt i = 0; i < col.size(); i++) {
      
      _gids.insert(col[i].id);
      
    }
    
  }
  
  std::copy(_gids.begin(), _gids.end(), std::back_inserter(gids));
  
}

std::pair<int, int> WMat::count_matrix_entries() const {
  
  int n_s = 0;
  int max_idx = 0;
  
  WMat::WeightMap::const_iterator wi = begin_row(), we = end_row();
  
  for (; wi != we; ++wi) {
    n_s += wi->second.size();
    if (wi->first.idx > max_idx) max_idx = wi->first.idx;
  }
  
  return std::make_pair(n_s, max_idx);;
    
}

// SparsePack/Unpack
SparsePack<WMat::WeightMap::value_type>::SparsePack(SparseMsg::buffer &b, WMat::WeightMap::value_type &t)
{
 // UInt res = 0;
  
  const WMat::Entry &row = t.first;
  
  std::vector<WMat::Entry> &col = t.second;

  // GID
  SparsePack<WMat::Entry::id_type>(b, row.id);
  
  // IDX
 SparsePack<WMat::Entry::idx_type>(b, row.idx);
  
  // src gid
  SparsePack<WMat::Entry::id_type>(b, row.src_id);

  // Number of columns, this row/idx
  SparsePack<UInt>(b, col.size());
      
    for (UInt i = 0; i < col.size(); i++) {
      
    WMat::Entry &cent = col[i];
      
    // col id
    SparsePack<WMat::Entry::id_type>(b, cent.id);
    
    // col idx
    SparsePack<WMat::Entry::idx_type>(b, cent.idx);
    
    // Matrix value
    SparsePack<WMat::Entry::value_type>(b, cent.value);
      
    } // i
}

UInt SparsePack<WMat::WeightMap::value_type>::size(WMat::WeightMap::value_type &t)
{
  
  UInt res = 0;
  
  //const WMat::Entry &ent = t.first;
  
  std::vector<WMat::Entry> &col = t.second;

  // GID
  res += SparsePack<WMat::Entry::id_type>::size();
  
  // IDX
  res += SparsePack<WMat::Entry::idx_type>::size();
  
  // src gid
  res += SparsePack<WMat::Entry::id_type>::size();

  // Number of columns, this row/idx
  res += SparsePack<UInt>::size();
      
    for (UInt i = 0; i < col.size(); i++) {
      
    // col id
    res += SparsePack<WMat::Entry::id_type>::size();
    
    // col idx
    res+= SparsePack<WMat::Entry::idx_type>::size();
    
    // Matrix value
    res += SparsePack<WMat::Entry::value_type>::size();
      
    } // i
    
    return res;
    
}

SparseUnpack<WMat::WeightMap::value_type>::SparseUnpack(SparseMsg::buffer &b, WMat::WeightMap::value_type &t)
{
  
  WMat::Entry &row = const_cast<WMat::Entry&>(t.first);
  
  // GID
  SparseUnpack<WMat::Entry::id_type>(b, row.id);
  
  // Idx
  SparseUnpack<WMat::Entry::idx_type>(b, row.idx);
  
  // Src GID
  SparseUnpack<WMat::Entry::id_type>(b, row.src_id);

  // ncols
  UInt ncols;
  SparseUnpack<UInt>(b, ncols);
  
  std::vector<WMat::Entry> &col = t.second;
  
  col.clear(); col.reserve(ncols);
  
  for (UInt i = 0; i < ncols; i++) {
    
    WMat::Entry cent;
    
    // Col id
    SparseUnpack<WMat::Entry::id_type>(b, cent.id);
    
    // col idx
    SparseUnpack<WMat::Entry::idx_type>(b, cent.idx);
    
    // value
    SparseUnpack<WMat::Entry::value_type>(b, cent.value);
    
    col.push_back(cent);
    
  } // i
  
}

std::ostream &operator <<(std::ostream &os, const WMat::Entry &ent) {
  os << "{id=" << ent.id << ", idx:" << (int) ent.idx << ", " << ent.value << "}";
  return os;
}

} // namespace
