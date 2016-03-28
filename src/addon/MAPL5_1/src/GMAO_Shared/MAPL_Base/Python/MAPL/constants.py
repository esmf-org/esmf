"""
Python version of MAPL Constants.
"""

MAPL_PI     = 3.14159265358979323846
MAPL_GRAV   = 9.80                   # m^2/s
MAPL_RADIUS = 6376.0E3               # m
MAPL_OMEGA  = 2.0*MAPL_PI/86164.0    # 1/s
MAPL_ALHL   = 2.4665E6               # J/kg @15C
MAPL_ALHF   = 3.3370E5               # J/kg
MAPL_ALHS   = MAPL_ALHL+MAPL_ALHF    # J/kg
MAPL_STFBOL = 5.6734E-8              # W/(m^2 K^4)
MAPL_AIRMW  = 28.97                  # kg/Kmole
MAPL_H2OMW  = 18.01                  # kg/Kmole
MAPL_O3MW   = 47.9982                # kg/Kmole
MAPL_RUNIV  = 8314.3                 # J/(Kmole K)
MAPL_KAPPA  = 2.0/7.0                # --
MAPL_RVAP   = MAPL_RUNIV/MAPL_H2OMW  # J/(kg K)
MAPL_RGAS   = MAPL_RUNIV/MAPL_AIRMW  # J/(kg K)
MAPL_CP     = MAPL_RGAS/MAPL_KAPPA   # J/(kg K)
MAPL_P00    = 100000.0               # Pa
MAPL_CAPICE = 2000.                  # J/(K kg)
MAPL_CAPWTR = 4218.                  # J/(K kg)
MAPL_RHOWTR = 1000.                  # kg/m^3
MAPL_NUAIR  = 1.533E-5               # m^2/S (@ 18C)
MAPL_TICE   = 273.16                 # K
MAPL_SRFPRS = 98470                  # Pa
MAPL_KARMAN = 0.40                   # --
MAPL_USMIN  = 1.00                   # m/s
MAPL_VIREPS = MAPL_AIRMW/MAPL_H2OMW-1.0   # --
MAPL_AVOGAD = 6.023E26               # 1/kmol

MAPL_UNDEF  = 1.0e15
