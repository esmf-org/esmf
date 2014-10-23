! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!-------------------------------------------------------------------------

INTEGER FUNCTION fw_get_num_devices
  USE openacc
  IMPLICIT NONE
  
  INTEGER :: num_cpus, num_accs

  !num_cpus = acc_get_num_devices(acc_device_host)

  num_accs = acc_get_num_devices(acc_device_not_host)

  fw_get_num_devices = num_accs

END FUNCTION fw_get_num_devices
