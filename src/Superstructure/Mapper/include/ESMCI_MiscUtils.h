#ifndef ESMCI_MiscUtils_H
#define ESMCI_MiscUtils_H
#include <vector>
#include <cmath>

namespace ESMCI{
  namespace MapperUtil{
    namespace Math{

      /* A util function to calculate the value of a 
       * number, base, to the power of an integral exponent, exp
       *
       * This function can be considered to replace pow* and has
       * been instrumented to be faster that the functions available
       * in the C library on some systems.
       * Currently we do not use this function in the Mapper
       */
      template<typename T>
      inline T ipow(T base, unsigned int exp,
              std::vector<T> &cache, std::vector<bool> &cache_is_valid)
      {
        if(cache_is_valid[exp]){
          return cache[exp];
        }
        else{
          int exp_lhs = exp >> 1;
          int exp_rhs = exp - exp_lhs;

          T val_lhs, val_rhs;
          if(cache_is_valid[exp_lhs]){
            val_lhs = cache[exp_lhs];
          }
          else{
            val_lhs = ipow(base, exp_lhs, cache, cache_is_valid);
            cache[exp_lhs] = val_lhs;
            cache_is_valid[exp_lhs] = val_lhs;
          }

          if(cache_is_valid[exp_rhs]){
            val_rhs = cache[exp_rhs];
          }
          else{
            val_rhs = ipow(base, exp_rhs, cache, cache_is_valid);
            cache[exp_rhs] = val_rhs;
            cache_is_valid[exp_rhs] = val_rhs;
          }

          return val_lhs * val_rhs;
        }
      }

      template<typename T>
      inline T ipow(T base, int exp)
      {
        const int MAX_EXP = 50;

        /* Handle common cases */
        if(exp == 0){
          return 1;
        }
        else if(exp == 1){
          return base;
        }
        else if(exp == 2){
          return base * base;
        }
        else if(exp == 3){
          return base * base * base;
        }
        else if(exp == 4){
          T val = base * base;
          return val * val;
        }
        assert(exp < MAX_EXP);

        std::vector<T> cache(MAX_EXP, static_cast<T>(0));
        std::vector<bool> cache_is_valid(MAX_EXP, false);

        cache[0] = 1;
        cache_is_valid[0] = true;
        cache[1] = base;
        cache_is_valid[1] = true;
        
        if(exp > 0){
          return ipow(base, exp, cache, cache_is_valid);
        }
        else{
          T val = ipow(base, exp, cache, cache_is_valid);
          return (static_cast<T>(1)/val);
        }
      }
  
    } // namespace Math
  } // namespace MapperUtil
} // namespace ESMCI

#endif // ESMCI_MiscUtils_H
