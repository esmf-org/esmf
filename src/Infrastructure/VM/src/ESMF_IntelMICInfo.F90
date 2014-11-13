! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!-------------------------------------------------------------------------

INTEGER FUNCTION fw_get_num_devices
  use mic_lib
  IMPLICIT NONE
  INTEGER :: ndevices, device_num
  
  ndevices = offload_number_of_devices()
!  device_num = offload_get_device_number()

!  PRINT *, "NUM OF DEVICES = ", ndevices, ", DEVICE NUM = ", device_num

  fw_get_num_devices = ndevices

END FUNCTION fw_get_num_devices
