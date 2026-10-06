###-------------------------------------------------------------------------###
### CMakeList.txt for examples using pio
###-------------------------------------------------------------------------###

if (PIO_ENABLE_FORTRAN)
    ADD_SUBDIRECTORY(f03)
    ADD_SUBDIRECTORY(c)
else()
  ADD_SUBDIRECTORY(c)
endif()
