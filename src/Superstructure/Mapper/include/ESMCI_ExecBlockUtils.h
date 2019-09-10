#ifndef ESMCI_ExecBlockUtils_H
#define ESMCI_ExecBlockUtils_H

#include <string>
#include <vector>
#include <map>
#include <utility>
#include <algorithm>
#include <iostream>
#include <initializer_list>
#include <cassert>
#include <cmath>
#include "ESMCI_CompInfo.h"

namespace ESMCI{
  namespace MapperUtil{

    /* Time Extent Matrix class used to map the execution of the
     * different component phases on a 2D matrix where the cols
     * represent PET ranges and columns represent the execution
     * time intervals
     *
     * This structure is used to find potential parallel execution
     * blocks (component phases running on the same PETs)
     *
     * This is a sort of GANTT chart-like representation of the
     * execution of the component phases (x axis/cols representing
     * PETs and y axis/rows representing time)
     */    
    template<typename T>
    class TimeExtentMatrixInfo{
      public:
        TimeExtentMatrixInfo(const std::vector<CompInfo<T> > &comp_infos);
        /* Get number of rows/cols of the matrix */
        int get_nrows(void ) const;
        int get_ncols(void ) const;
        /* Get mapping between matrix indices and PET id/time */
        int to_pet_id(int tem_col_idx) const;
        int to_tem_col_idx(int pet_id) const;
        T to_time(int tem_row_idx) const;
        int to_tem_row_idx(T time) const;
      private:
        int nrows_;
        int ncols_;
        std::map<int, int> pet2col_map_;
        std::map<int, int> col2pet_map_;
        std::map<T, int> time2row_map_;
        std::map<int, T> row2time_map_;
    };

    template<typename T>
    TimeExtentMatrixInfo<T>::TimeExtentMatrixInfo(
      const std::vector<CompInfo<T> > &comp_infos)
    {
      std::vector<CompInfo<T> > cinfos(comp_infos);
      std::sort(cinfos.begin(), cinfos.end(), CompInfoCmpBySTimePet<T>());

      std::vector<int> pet_values;
      std::vector<T> time_values;
      for(typename std::vector<CompInfo<T> >::const_iterator citer = cinfos.cbegin();
          citer != cinfos.cend(); ++citer){
        std::pair<int, int> pet_range = citer->get_pet_range();
        std::pair<T, T> time_intvl = citer->get_time_interval();

        pet_values.push_back(pet_range.first);
        pet_values.push_back(pet_range.second);
        time_values.push_back(time_intvl.first);
        time_values.push_back(time_intvl.second);
      }

      std::sort(pet_values.begin(), pet_values.end());
      std::vector<int>::iterator pet_values_end =
        std::unique(pet_values.begin(), pet_values.end());
      ncols_ = static_cast<int>(std::distance(pet_values.begin(), pet_values_end));
      for(int i=0; i<ncols_; i++){
        pet2col_map_.insert(std::make_pair(pet_values[i], i));
        col2pet_map_.insert(std::make_pair(i, pet_values[i]));
      }

      std::sort(time_values.begin(), time_values.end());
      typename std::vector<T>::iterator time_values_end =
        std::unique(time_values.begin(), time_values.end());
      nrows_ = static_cast<int>(std::distance(time_values.begin(), time_values_end));
      for(int i=0; i<nrows_; i++){
        time2row_map_.insert(std::make_pair(time_values[i], i));
        row2time_map_.insert(std::make_pair(i, time_values[i]));
      }
    }

    template<typename T>
    int TimeExtentMatrixInfo<T>::get_nrows(void ) const
    {
      return nrows_;
    }

    template<typename T>
    int TimeExtentMatrixInfo<T>::get_ncols(void ) const
    {
      return ncols_;
    }

    template<typename T>
    int TimeExtentMatrixInfo<T>::to_pet_id(int tem_col_idx) const
    {
      return col2pet_map_.at(tem_col_idx);
    }

    template<typename T>
    int TimeExtentMatrixInfo<T>::to_tem_col_idx(int pet_id) const
    {
      return pet2col_map_.at(pet_id);
    }

    template<typename T>
    T TimeExtentMatrixInfo<T>::to_time(int tem_row_idx) const
    {
      return row2time_map_.at(tem_row_idx);
    }

    template<typename T>
    int TimeExtentMatrixInfo<T>::to_tem_row_idx(T time) const
    {
      return time2row_map_.at(time);
    }

    /* The class representing the time extent matrix information
     * associated with a component phase.
     * This class includes information on "where" the component
     * phase resides in the time extent matrix
     */
    template<typename T>
    class TimeExtentCompInfo{
      public:
        TimeExtentCompInfo(const CompInfo<T> *comp_info, int min_idx, int max_idx);
        void set_time_extent_idx(int min_idx, int max_idx);
        void set_time_extent_min_idx(int min_idx);
        int get_time_extent_min_idx(void ) const;
        void set_time_extent_max_idx(int max_idx);
        int get_time_extent_max_idx(void ) const;
        const CompInfo<T> *get_comp_info(void ) const;
      private:
        const CompInfo<T> *comp_info_;
        int min_idx_;
        int max_idx_;
    };

    template<typename T>
    TimeExtentCompInfo<T>::TimeExtentCompInfo(const CompInfo<T> *comp_info,
      int min_idx, int max_idx):comp_info_(comp_info),min_idx_(min_idx),max_idx_(max_idx)
    {
    }

    template<typename T>
    void TimeExtentCompInfo<T>::set_time_extent_idx(int min_idx, int max_idx)
    {
      min_idx_ = min_idx;
      max_idx_ = max_idx;
    }

    template<typename T>
    void TimeExtentCompInfo<T>::set_time_extent_min_idx(int min_idx)
    {
      min_idx_ = min_idx;
    }

    template<typename T>
    int TimeExtentCompInfo<T>::get_time_extent_min_idx(void ) const
    {
      return min_idx_;
    }

    template<typename T>
    void TimeExtentCompInfo<T>::set_time_extent_max_idx(int max_idx)
    {
      max_idx_ = max_idx;
    }

    template<typename T>
    int TimeExtentCompInfo<T>::get_time_extent_max_idx(void ) const
    {
      return max_idx_;
    }

    template<typename T>
    const CompInfo<T> *TimeExtentCompInfo<T>::get_comp_info(void ) const
    {
      return comp_info_;
    }

    template<typename T>
    void create_time_extent_matrix(
      const std::vector<CompInfo<T> > &comp_infos,
      std::vector<std::vector<TimeExtentCompInfo<T> *> > &tem,
      TimeExtentMatrixInfo<T> &tem_info)
    {
      int nrows = tem_info.get_nrows();
      int ncols = tem_info.get_ncols();

      /* Initialize the time extent matrix */
      std::vector<TimeExtentCompInfo<T> *> tmp_row(ncols, NULL);
      for(int i=0; i<nrows; i++){
        tem.push_back(tmp_row);
      }

      std::vector<const CompInfo<T> *> pcomp_infos(comp_infos.size(), NULL);
      for(int i=0; i<static_cast<int>(comp_infos.size()); i++){
        pcomp_infos[i] = &(comp_infos[i]);
      }

      /* Sort pointers comp infos by Starting time and PET and add
       * to the time extent matrix
       */

      std::sort(pcomp_infos.begin(), pcomp_infos.end(), CompInfoPtrCmpBySTimePet<T>());

      for(int i=0; i<static_cast<int>(pcomp_infos.size()); i++){
        std::pair<int, int> pet_range = pcomp_infos[i]->get_pet_range();
        std::pair<T, T> time_intvl = pcomp_infos[i]->get_time_interval();
        /* Find the row and column to add the comp info */
        int col_start = tem_info.to_tem_col_idx(pet_range.first);
        int col_end = tem_info.to_tem_col_idx(pet_range.second);
        int row_start = tem_info.to_tem_row_idx(time_intvl.first);
        int row_end = tem_info.to_tem_row_idx(time_intvl.second);
        int min_extent = row_end;
        int max_extent = nrows-1;
        tem[row_start][col_end] = new TimeExtentCompInfo<T>(pcomp_infos[i], min_extent, max_extent);
        for(int j=0; j<row_start; j++){
          for(int k=col_start; k<=col_end; k++){
            if(tem[j][k] != NULL){
              int cur_max_extent = (tem[j][k])->get_time_extent_max_idx();
              (tem[j][k])->set_time_extent_max_idx(std::min(row_start, cur_max_extent));
            }
          }
        }
      }
    }

    template<typename T>
    void print_time_extent_matrix(
      const std::vector<std::vector<TimeExtentCompInfo<T> *> > &tem)
    {
      for(typename std::vector<std::vector<TimeExtentCompInfo<T> *> >::const_iterator
            row_iter = tem.cbegin(); row_iter != tem.cend(); ++row_iter){
        std::vector<TimeExtentCompInfo<T> *> row(*row_iter);
        for(typename std::vector<TimeExtentCompInfo<T> *>::const_iterator
          col_iter = row.cbegin(); col_iter != row.cend(); ++col_iter){
          if(*col_iter != NULL){
            std::cout << "(" << (*col_iter)->get_time_extent_min_idx() << ","
                      << (*col_iter)->get_time_extent_max_idx() << "), ";
          }
          else{
            std::cout << "(0,0), ";
          }
        }
        std::cout << "\n";
      }
    }

    template<typename T>
    bool find_parallel_exec_blocks(const std::vector<CompInfo<T> > &comp_infos,
                                std::vector<std::vector<ExecBlock<T> > > &pexec_blocks)
    {
      /* Create the time extent matrix */
      std::vector<std::vector<TimeExtentCompInfo<T> *> > tem;
      TimeExtentMatrixInfo<T> tem_info(comp_infos);
      create_time_extent_matrix(comp_infos, tem, tem_info);

      int tem_nrows = static_cast<int>(tem.size());
      int tem_ncols = 0;
      if(tem_nrows > 0){
        tem_ncols = static_cast<int>(tem[0].size());
        for(typename std::vector<std::vector<TimeExtentCompInfo<T> *> >::const_iterator
            citer = tem.begin(); citer != tem.end(); ++citer){
          assert(static_cast<int>(citer->size()) == tem_ncols);
        }
      }

      // print_time_extent_matrix(tem);

      std::vector<std::vector<int> > partition_cols;
      for(int ir=tem_nrows-1; ir>=0; ir--){
        for(int jr=0; jr<ir; jr++){
          std::vector<int> pcol;
          std::vector<ExecBlock<T> > exec_blocks;
          int first_eblock_col = -1;
          for(int kc=0; kc < tem_ncols; kc++){
            bool has_potential_kc = false;
            int rkc = jr;
            if(tem[jr][kc] != NULL){
              has_potential_kc = true;
              rkc = jr;
            }
            else{
              rkc = jr;
              for(int irkc=jr; irkc<ir; irkc++){
                if(tem[irkc][kc] != NULL){
                  has_potential_kc = true;
                  rkc = irkc;
                  break;
                }
              }
            }
            if(has_potential_kc){
              if( ((tem[rkc][kc])->get_time_extent_min_idx() <= ir) &&
                  ((tem[rkc][kc])->get_time_extent_max_idx() >= ir) ){
                pcol.push_back(kc);
              }
            }
            if(pcol.empty()){
              first_eblock_col = kc;
            }
          }
          if(pcol.size() > 1){
            partition_cols.push_back(pcol);
            int prev_col = first_eblock_col;
            for(int ipcol=0; ipcol < static_cast<int>(pcol.size()); ipcol++){
              /* Find comp infos in range [prev_col+1, pcol[i]] in the row
               * range [jr, ir)
               */
              ExecBlock<T> exec_block;
              for(int ir_cinfo = jr; ir_cinfo < ir; ir_cinfo++){
                for(int ic_cinfo = prev_col+1; ic_cinfo  <= pcol[ipcol]; ic_cinfo++){
                  if(tem[ir_cinfo][ic_cinfo] != NULL){
                    assert((tem[ir_cinfo][ic_cinfo])->get_comp_info());
                    exec_block.add_comp_phase(
                      *((tem[ir_cinfo][ic_cinfo])->get_comp_info()));
                  }
                }
              }
              prev_col = pcol[ipcol];
              exec_blocks.push_back(exec_block);
            }
            pexec_blocks.push_back(exec_blocks);
          }
        }
      }

      //std::cout << "Partition col list : \n";
      for(std::vector<std::vector<int> >::const_iterator citer = partition_cols.cbegin();
          citer != partition_cols.cend(); ++citer){
        std::vector<int> pcol(*citer);
        for(std::vector<int>::const_iterator citer2 = pcol.cbegin();
            citer2 != pcol.cend(); ++citer2){
          //std::cout << *citer2 << ", ";
        }
        //std::cout << "\n";
      }

      return (pexec_blocks.size() > 0);
    }

  } // namespace MapperUtil
} //namespace ESMCI

#endif // ESMCI_ExecBlockUtils_H
