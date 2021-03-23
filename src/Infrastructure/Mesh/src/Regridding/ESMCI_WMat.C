// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Regridding/ESMCI_WMat.h>
#include <Mesh/include/Legacy/ESMCI_Attr.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include "PointList/include/ESMCI_PointList.h"
#include <Mesh/include/ESMCI_MBMesh.h>
#include <Mesh/include/ESMCI_MBMesh_Util.h>

#include <algorithm>
#include <cstdio>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

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

// Insert row and associated columns into matrix complain if row is already there
// and it isn't the same
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




// Insert row and associated columns into matrix complain if column doesn't
// exist then add it, if it exists with a different value then complain
// ASSUMES cols is in sorted order
void WMat::InsertRowMergeSingle(const Entry &row, const Entry &col) {

  // Create vector for col
  std::vector<Entry> col_vec;
  col_vec.resize(1,col);

  std::pair<WeightMap::iterator, bool> wi =
    weights.insert(std::make_pair(row, col_vec));

  if (wi.second == false) {
    // Get old columns associated with original row
    std::vector<Entry> &old_cols = wi.first->second;

    // Get location where col should be
    std::vector<Entry>::iterator lb =
      std::lower_bound(old_cols.begin(), old_cols.end(), col);

      // If we found an entry, see if it's the same
    if (lb != old_cols.end()) {
        // Is this the same?
      if (*lb == col) {
        // If it has a different val then complain, otherwise ignore it
        if (std::abs(lb->value-col.value) < 1e-5) {
          return;
        } else {
          //printf("ERROR dst_id=%d tmp_cols: id=%d idx=%d src_id=%d value=%f old_cols: id=%d idx=%d src_id=%d value=%f \n", row.id,
          //     tmp_cols[i].id,tmp_cols[i].idx,tmp_cols[i].src_id,tmp_cols[i].value,old_cols[j].id,old_cols[j].idx,old_cols[j].src_id,old_cols[j].value);
          Throw() << "Shouldn't have the same matrix entries with different values.";
        }
      }
    }

    // Insert col
    old_cols.insert(lb, col);

  } // else {
    //  Don't need to do anything here, because it's only a single entry
    //  }
}



// Insert row and associated columns into matrix complain if column doesn't
// exist then add it, if it exists with a different value then sum values
// ASSUMES cols is in sorted order
void WMat::InsertRowSumSingle(const Entry &row, const Entry &col) {

  // Create vector for col
  std::vector<Entry> col_vec;
  col_vec.resize(1,col);

  std::pair<WeightMap::iterator, bool> wi =
    weights.insert(std::make_pair(row, col_vec));

  if (wi.second == false) {
    // Get old columns associated with original row
    std::vector<Entry> &old_cols = wi.first->second;

    // Get location where col should be
    std::vector<Entry>::iterator lb =
      std::lower_bound(old_cols.begin(), old_cols.end(), col);

#if 1
      // If we found an entry, see if it's the same
    if (lb != old_cols.end()) {

      // If we found an entry, see if it's the same (considering everything, but value)
      // If it does match, then sum values and leave
      if (*lb == col) {
        (*lb).value += col.value;
        return;
      }
    }
#endif

    // If it didn't match, then insert col
    old_cols.insert(lb, col);
  }
}


#if 0
// Insert row and associated columns into matrix complain if column doesn't
// exist then add it, if it exists with a different value then complain
// ASSUMES cols is in sorted order
void WMat::InsertRowMerge(const Entry &row, const std::vector<Entry> &cols) {

  std::pair<WeightMap::iterator, bool> wi =
    weights.insert(std::make_pair(row, cols));

  if (wi.second == false) {

    // vector to put cols already not in this row
    std::vector<Entry> uniq_cols;
    uniq_cols.reserve(cols.size());

    // Get old columns associated with original row
    std::vector<Entry> &old_cols = wi.first->second;

    // interator to beginning of old_cols
    std::vector<Entry>::iterator beg = old_cols.begin();

    // Only put in entries which don't already exist in old_cols
    for (int i=0; i< cols.size(); i++) {

      // Get location where col[i] should be
      std::vector<Entry>::iterator lb =
        std::lower_bound(beg, old_cols.end(), cols[i]);

      // If we found an entry, see if it's the same
      if (lb != old_cols.end()) {
        // Is this the same?
        if (*lb == cols[i]) {
          // If it has a different val then complain, otherwise ignore it
          if (std::abs(lb->value-cols[i].value) < 1e-5) {
            continue;
          } else {
            //printf("ERROR dst_id=%d tmp_cols: id=%d idx=%d src_id=%d value=%f old_cols: id=%d idx=%d src_id=%d value=%f \n", row.id,
            //     tmp_cols[i].id,tmp_cols[i].idx,tmp_cols[i].src_id,tmp_cols[i].value,old_cols[j].id,old_cols[j].idx,old_cols[j].src_id,old_cols[j].value);
            Throw() << "Shouldn't have the same matrix entries with different values.";
          }
        }
      }

      // If it's not the same then put it into the new list
      uniq_cols.push_back(cols[i]);

      // Since cols is sorted start from the new location
      beg=lb;
    }

    // If nothing to be added then exit
    if (uniq_cols.empty()) return;

    // temp to put merged results into
    std::vector<Entry> tmp_cols;
    tmp_cols.resize(uniq_cols.size()+old_cols.size());

    // Merge cols together
    std::merge(old_cols.begin(),old_cols.end(),
          uniq_cols.begin(),uniq_cols.end(),
          tmp_cols.begin());

    // Put tmp_cols memory into old_cols
   old_cols.swap(tmp_cols);

  } else {
    // Sort the column entries (invariant used elsewhere).
    // DON'T NEED TO SORT COL ASSUMED TO BE SORTED
    // std::sort(wi.first->second.begin(), wi.first->second.end());
    // compress storage
    std::vector<Entry>(wi.first->second).swap(wi.first->second);
  }
}
#else
// Insert row and associated columns into matrix complain if column doesn't
// exist then add it, if it exists with a different value then complain
// ASSUMES cols is in sorted order
void WMat::InsertRowMerge(const Entry &row, const std::vector<Entry> &cols) {

  std::pair<WeightMap::iterator, bool> wi =
    weights.insert(std::make_pair(row, cols));

  if (wi.second == false) {
    // Get old columns associated with original row
    std::vector<Entry> &old_cols = wi.first->second;

    // temp to put merged results into
    std::vector<Entry> tmp_cols;
    tmp_cols.resize(cols.size()+old_cols.size());
    // If nothing to be added then exit
    if (tmp_cols.empty()) return;

    // Merge cols together
    std::merge(old_cols.begin(),old_cols.end(),
          cols.begin(),cols.end(),
          tmp_cols.begin());

    // Resize old_cols to contain merged and unqiued data
    old_cols.resize(tmp_cols.size());
    // Get rid of duplicates and
    // make sure there are no bad duplicates
    // (e.g. same id, but different values)
    // These will be in sequence since the cols
    // are sorted.
    int j=0;
    old_cols[0]=tmp_cols[0];
    for (int i=1; i<tmp_cols.size(); i++) {
      //      if (tmp_cols[i].id != old_cols[j].id) {
      if ((tmp_cols[i].id != old_cols[j].id) ||
          (tmp_cols[i].src_id != old_cols[j].src_id)) {
        j++;
        old_cols[j]=tmp_cols[i];
      } else {
        // If we look like the same entry, but have different values then complain
        // NOTE: equality for entries considers more than just the .id, but doesn't
        //       consider .value
        if ((tmp_cols[i]       == old_cols[j]) &&
            (std::abs(tmp_cols[i].value-old_cols[j].value) > 1e-5))
          //          printf("ERROR dst_id=%d tmp_cols: id=%d idx=%d src_id=%d value=%f old_cols: id=%d idx=%d src_id=%d value=%f \n", row.id,
          //                 tmp_cols[i].id,tmp_cols[i].idx,tmp_cols[i].src_id,tmp_cols[i].value,old_cols[j].id,old_cols[j].idx,old_cols[j].src_id,old_cols[j].value);

            Throw() << "Shouldn't have the same matrix entries with different values.";
      }
    }

    // Resize old_cols to fit just what's needed
    old_cols.resize(j+1);

    // Get rid of extra memory
    std::vector<Entry>(old_cols).swap(old_cols);

  } else {
    // Sort the column entries (invariant used elsewhere).
     std::sort(wi.first->second.begin(), wi.first->second.end());
    // compress storage
    std::vector<Entry>(wi.first->second).swap(wi.first->second);
  }
}
#endif



// Merge disjoint weight matrices. If the same destination point shows up
// in both with different weights complain
void WMat::MergeDisjoint(const WMat &wmat2) {
  Trace __trace("WMat::AssimilateConstraints(const WMat &constraints)");

  // Loop 2 weight matrix find the entries
  WeightMap::const_iterator w2i = wmat2.weights.begin(), w2e = wmat2.weights.end();
  for (; w2i != w2e; ++w2i) {

    // Get row and column info from 2nd matrix
    const Entry &row = w2i->first;
    const std::vector<Entry> &col = w2i->second;

    // Check to see if destination ids match
    // Since these two matrices may be coming from different weight
    // calculation routines they might have different secondary data (e.g. src_id), so
    // check explicitly if dst ids match
    //
    // Lower bound
    Entry lower(row.id);
    WeightMap::const_iterator ci = weights.lower_bound(lower);

    //// Upper Bound
    Entry upper(row.id+1);
    WeightMap::const_iterator ce = weights.lower_bound(upper);

    // If there are no constraints which match continue to next row
    if (ci != weights.end()) {

      // Loop over contraints which match
      // see if any have the same dst id
      for (; ci != ce; ++ci) {
        const Entry &crow = ci->first;

        // Complain if they have the same dst id
        if (crow.id == row.id) {
          Throw() << "The same destination id appears in the source and destination when they are expected to be disjoint";
        }
      }
    }


    //// Add the row and column from wmat2 to the weight matrix
    std::pair<WeightMap::iterator, bool> wi =
      weights.insert(std::make_pair(row, col));

    // Make sure it isn't seeing the same row
    if (wi.second == false) {
        Throw() << "Unexpectedly seeing an equivalent row in disjoint weight matrices.";
    }

  }
}


// Merge wmat2 into exisitng wmat. If the same row exists in both,
// replace rows in the existing matrix with ones from wmat2
void WMat::MergeReplace(const WMat &wmat2) {
  Trace __trace("WMat::ReplaceRows(const WMat &wmat2)");

  // Loop 2 weight matrix find the entries
  WeightMap::const_iterator w2i = wmat2.weights.begin(), w2e = wmat2.weights.end();
  for (; w2i != w2e; ++w2i) {

    // Get row and column info from 2nd matrix
    const Entry &row = w2i->first;
    const std::vector<Entry> &col = w2i->second;

    // Replace entry, or add a new one
    weights[row]=col;

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

// Migrate WMat based on mesh's node ids
//void WMat::Migrate(CommRel &crel) {
void WMat::Migrate(Mesh &mesh) {
  Trace __trace("WMat::Migrate(Mesh &mesh)");

  // Gather pole constraints
  {
    std::vector<UInt> mesh_dist, iw_dist;

    // BOB - Switch to Owned_ID to only migrate weights to
    //       mesh points which are actually owned, not for example
    //       halo points. The extra points used to be removed
    //       in Prune() but it's more efficient to not send them
    //       in the first place. If we start regridding to halos,
    //       then this will have to be changed.
    //
       Context c; c.set(Attr::ACTIVE_ID);
       // Switch back to original to see if that fixes things
    // Context c; c.set(Attr::OWNED_ID);
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


// Migrate WMat based on pointlist's ids
  void WMat::Migrate(PointList &plist) {
    Trace __trace("WMat::Migrate(PointList &plist)");

    {
      std::vector<UInt> plist_dist, iw_dist;

      int num_pts = plist.get_curr_num_pts();

      for (int iii=0; iii<num_pts; iii++) {
        int asb_id=plist.get_id(iii);
        plist_dist.push_back(asb_id);
      }

      GetRowGIDS(iw_dist);

      Migrator mig(plist_dist.size(), plist_dist.size() > 0 ? &plist_dist[0] : NULL, 0,
                   iw_dist.size(), iw_dist.size() > 0 ? &iw_dist[0] : NULL);

      mig.Migrate(*this);


      //#define CHECK_WEIGHT_MIG
#ifdef CHECK_WEIGHT_MIG
      // Check something: should have 1 to 1 coresp ids and entries
      for (UInt i = 0; i < plist_dist.size(); i++) {
        Entry ent(plist_dist[i]);
        WeightMap::iterator wi = weights.lower_bound(ent);
        if (wi == weights.end() || wi->first.id != ent.id) {
          Throw() << "Did not find id:" << ent.id << std::endl;
        }
      }

      // And the other way
      std::sort(plist_dist.begin(), plist_dist.end());
      WeightMap::iterator wi = weights.begin(), we = weights.end();
#endif

    }
    return;
  }

// Migrate WMat based on mesh's element ids
//void WMat::Migrate(CommRel &crel) {
void WMat::MigrateToElem(Mesh &mesh) {
  Trace __trace("WMat::Migrate(Mesh &mesh)");

  // Gather pole constraints
  {
    std::vector<UInt> mesh_dist, iw_dist;

    Context c; c.set(Attr::ACTIVE_ID);
    Attr a(MeshObj::ELEMENT, c);
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

  // take out if MOAB isn't defined
#if defined ESMF_MOAB

// Migrate WMat based on mesh's element ids
//void WMat::Migrate(CommRel &crel) {
void WMat::MigrateToElem(MBMesh &mesh) {
  Trace __trace("WMat::Migrate(Mesh &mesh)");

    std::vector<UInt> mesh_dist, iw_dist;

    MBMesh_get_local_elem_gids(&mesh, mesh_dist);
    GetRowGIDS(iw_dist);

    // Create description of migration pattern
    Migrator mig(mesh_dist.size(), mesh_dist.size() > 0 ? &mesh_dist[0] : NULL, 0,
        iw_dist.size(), iw_dist.size() > 0 ? &iw_dist[0] : NULL);

    // Migrate weigths
    mig.Migrate(*this);
}

#endif // ESMF_MOAB

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

    // src gid
    SparsePack<WMat::Entry::id_type>(b, cent.src_id);

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

    // src gid
    res += SparsePack<WMat::Entry::id_type>::size();
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

    // Src GID
    SparseUnpack<WMat::Entry::id_type>(b, cent.src_id);

    col.push_back(cent);

  } // i

}

std::ostream &operator <<(std::ostream &os, const WMat::Entry &ent) {
  os << "{id=" << ent.id << ", idx:" << (int) ent.idx << ", " << ent.value << "}";
  return os;
}

} // namespace
