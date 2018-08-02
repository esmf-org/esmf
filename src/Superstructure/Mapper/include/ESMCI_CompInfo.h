#ifndef ESMCI_CompInfo_H
#define ESMCI_CompInfo_H

#include <string>
#include <vector>
#include <utility>
#include <algorithm>
#include <iostream>
#include <initializer_list>
#include <cassert>
#include <cmath>

namespace ESMCI{
  namespace MapperUtil{

    template<typename T>
    class CompInfo{
      public:
        CompInfo( const std::string &comp_name,
                  const std::string &comp_phase_name,
                  const std::pair<int, int> &pet_range,
                  const std::pair<T, T> &time_intvl);
        CompInfo( const std::string &comp_name,
                  const std::string &comp_phase_name,
                  const std::pair<int, int> &pet_range,
                  const std::pair<T, T> &time_intvl,
                  T stime);
        std::string get_comp_name(void ) const;
        std::string get_comp_phase_name(void ) const;
        std::pair<int, int> get_pet_range(void ) const;
        std::pair<T, T> get_time_interval(void ) const;
        T get_stime(void ) const;
      private:
        std::string comp_name_;
        std::string comp_phase_name_;
        std::pair<int, int> pet_range_;
        std::pair<T, T> time_intvl_;
        T stime_;
    };

    // comparators for the comp info class
    template<typename T>
    class CompInfoCmpBySPet{
      public:
        bool operator()(const CompInfo<T> &a, const CompInfo<T> &b) const;
    };

    template<typename T>
    class CompInfoCmpByEPet{
      public:
        bool operator()(const CompInfo<T> &a, const CompInfo<T> &b) const;
    };

    template<typename T>
    class CompInfoCmpBySTime{
      public:
        CompInfoCmpBySTime();
        CompInfoCmpBySTime(double tol);
        bool operator()(const CompInfo<T> &a, const CompInfo<T> &b) const;
      private:
        double tol_;
    };

    template<typename T>
    class CompInfoCmpByETime{
      public:
        CompInfoCmpByETime();
        CompInfoCmpByETime(double tol);
        bool operator()(const CompInfo<T> &a, const CompInfo<T> &b) const;
      private:
        double tol_;
    };

    template<typename T>
    class CompInfoCmpBySTimePet{
      public:
        CompInfoCmpBySTimePet();
        CompInfoCmpBySTimePet(double tol);
        bool operator()(const CompInfo<T> &a, const CompInfo<T> &b) const;
      private:
        double tol_;
    };

    template<typename T>
    class CompInfoPtrCmpBySTimePet{
      public:
        CompInfoPtrCmpBySTimePet();
        CompInfoPtrCmpBySTimePet(double tol);
        bool operator()(const CompInfo<T> *a, const CompInfo<T> *b) const;
      private:
        double tol_;
    };

    template<typename T>
    inline CompInfo<T>::CompInfo(
              const std::string &comp_name,
              const std::string &comp_phase_name,
              const std::pair<int, int> &pet_range,
              const std::pair<T, T> &time_intvl):
                comp_name_(comp_name),
                comp_phase_name_(comp_phase_name),
                pet_range_(pet_range),
                time_intvl_(time_intvl)
    {
      stime_ = static_cast<T>((time_intvl.second - time_intvl.first) * 
                (pet_range.second - pet_range.first + 1));
    }

    template<typename T>
    inline CompInfo<T>::CompInfo(
              const std::string &comp_name,
              const std::string &comp_phase_name,
              const std::pair<int, int> &pet_range,
              const std::pair<T, T> &time_intvl,
              T stime):
                comp_name_(comp_name),
                comp_phase_name_(comp_phase_name),
                pet_range_(pet_range),
                time_intvl_(time_intvl),
                stime_(stime)
    {
    }

    template<typename T>
    std::string CompInfo<T>::get_comp_name(void ) const
    {
      return comp_name_;
    }

    template<typename T>
    std::string CompInfo<T>::get_comp_phase_name(void ) const
    {
      return comp_phase_name_;
    }

    template<typename T>
    std::pair<int, int> CompInfo<T>::get_pet_range(void ) const
    {
      return pet_range_;
    }

    template<typename T>
    std::pair<T, T> CompInfo<T>::get_time_interval(void ) const
    {
      return time_intvl_;
    }

    template<typename T>
    T CompInfo<T>::get_stime(void ) const
    {
      return stime_;
    }

    template<typename T>
    std::ostream& operator<<(std::ostream &ostr, const CompInfo<T> &comp_info)
    {
      std::pair<int, int> pet_range = comp_info.get_pet_range();
      std::pair<T, T> time_intvl = comp_info.get_time_interval();
      ostr << "{";
      ostr << comp_info.get_comp_name().c_str();
      ostr << " : ";
      ostr << comp_info.get_comp_phase_name().c_str();
      ostr << ", pet(" << pet_range.first << "," << pet_range.second << ") ";
      ostr << ", time(" << time_intvl.first << "," << time_intvl.second << ")";
      ostr << "}";
      return ostr;
    }

    template<typename T>
    inline bool CompInfoCmpBySPet<T>::operator()(const CompInfo<T> &a,
                  const CompInfo<T> &b) const
    {
      std::pair<int, int> a_pet_range = a.get_pet_range();
      std::pair<int, int> b_pet_range = b.get_pet_range();
      return (a_pet_range.first < b_pet_range.first);
    }

    template<typename T>
    inline bool CompInfoCmpByEPet<T>::operator()(const CompInfo<T> &a,
                  const CompInfo<T> &b) const
    {
      std::pair<int, int> a_pet_range = a.get_pet_range();
      std::pair<int, int> b_pet_range = b.get_pet_range();
      return (a_pet_range.second < b_pet_range.second);
    }

    template<typename T>
    inline CompInfoCmpBySTime<T>::CompInfoCmpBySTime():tol_(0)
    {
    }

    template<typename T>
    inline CompInfoCmpBySTime<T>::CompInfoCmpBySTime(double tol):tol_(tol)
    {
    }

    template<typename T>
    inline bool CompInfoCmpBySTime<T>::operator()(const CompInfo<T> &a,
                  const CompInfo<T> &b) const
    {
      std::pair<T, T> a_time_intvl = a.get_time_interval();
      std::pair<T, T> b_time_intvl = b.get_time_interval();
      return ((a_time_intvl.first - b_time_intvl.first + tol_) < 0.0);
    }

    template<typename T>
    inline CompInfoCmpByETime<T>::CompInfoCmpByETime():tol_(0)
    {
    }

    template<typename T>
    inline CompInfoCmpByETime<T>::CompInfoCmpByETime(double tol):tol_(tol)
    {
    }

    template<typename T>
    inline bool CompInfoCmpByETime<T>::operator()(const CompInfo<T> &a,
                  const CompInfo<T> &b) const
    {
      std::pair<T, T> a_time_intvl = a.get_time_interval();
      std::pair<T, T> b_time_intvl = b.get_time_interval();
      return ((a_time_intvl.second - b_time_intvl.second + tol_) < 0.0);
    }

    template<typename T>
    inline CompInfoCmpBySTimePet<T>::CompInfoCmpBySTimePet():tol_(0)
    {
    }

    template<typename T>
    inline CompInfoCmpBySTimePet<T>::CompInfoCmpBySTimePet(double tol):tol_(tol)
    {
    }

    template<typename T>
    inline bool CompInfoCmpBySTimePet<T>::operator()(const CompInfo<T> &a,
                  const CompInfo<T> &b) const
    {
      std::pair<int, int> a_pet_range = a.get_pet_range();
      std::pair<int, int> b_pet_range = b.get_pet_range();
      std::pair<T, T> a_time_intvl = a.get_time_interval();
      std::pair<T, T> b_time_intvl = b.get_time_interval();
      if(std::abs(a_time_intvl.first - b_time_intvl.first) <= tol_){
        return (a_pet_range.first < b_pet_range.first);
      }
      else{
        return (a_time_intvl.first < b_time_intvl.first);
      }
    }

    template<typename T>
    inline CompInfoPtrCmpBySTimePet<T>::CompInfoPtrCmpBySTimePet():tol_(0)
    {
    }

    template<typename T>
    inline CompInfoPtrCmpBySTimePet<T>::CompInfoPtrCmpBySTimePet(double tol):tol_(tol)
    {
    }

    template<typename T>
    inline bool CompInfoPtrCmpBySTimePet<T>::operator()(const CompInfo<T> *a,
                  const CompInfo<T> *b) const
    {
      assert(a && b);
      std::pair<int, int> a_pet_range = a->get_pet_range();
      std::pair<int, int> b_pet_range = b->get_pet_range();
      std::pair<T, T> a_time_intvl = a->get_time_interval();
      std::pair<T, T> b_time_intvl = b->get_time_interval();
      if(std::abs(a_time_intvl.first - b_time_intvl.first) <= tol_){
        return (a_pet_range.first < b_pet_range.first);
      }
      else{
        return (a_time_intvl.first < b_time_intvl.first);
      }
    }

  } // namespace MapperUtil
} //namespace ESMCI
#endif //ESMCI_CompInfo_H
