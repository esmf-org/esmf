 /*  +-======-+ 
  Copyright (c) 2003-2007 United States Government as represented by 
  the Admistrator of the National Aeronautics and Space Administration.  
  All Rights Reserved.
  
  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
  
  Government Agency: National Aeronautics and Space Administration
  Government Agency Original Software Designation: GSC-15354-1
  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
  Government Agency Point of Contact for Original Software:  
  			Dale Hithon, SRA Assistant, (301) 286-2691
  
 +-======-+   */
/*

!
! Similar to benchmark_sdf, but write flat binaries with fwrite() instead.
!

*/

#include <stdio.h>
#include "benchmark.h"

int main ( int argc, char **argv ) {

  float lon[IM_], lat[JM_], lev[KM_];
  float ps[IM_][JM_];
  float u[IM_][JM_][KM_];

  int i, j, k, t;
  int im=IM_, jm=JM_, km=KM_, tm=TM_;

  FILE *file;

  file = fopen("benchmark_out.fwrite", "wb" );
  if ( ! file ) {
       printf("benchmark_fwrite: NOT Passed");
       return 1;
  }

  fwrite(lon, sizeof(float), im, file );
  fwrite(lat, sizeof(float), jm, file );
  fwrite(lev, sizeof(float), km, file );
  for ( t=0; t<tm; t++ ) {
    fwrite(ps, sizeof(float), im*jm,    file );
    fwrite(ps, sizeof(float), im*jm,    file );
    fwrite( u, sizeof(float), im*jm*km, file );
    fwrite( u, sizeof(float), im*jm*km, file );
    fwrite( u, sizeof(float), im*jm*km, file );
    fwrite( u, sizeof(float), im*jm*km, file );
  }
  fclose(file);

  printf("benchmark_fwrite: Passed\n");

}
