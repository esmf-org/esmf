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
