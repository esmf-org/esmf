#include "ser_mmul2d.h"
/* c = a * b; a[m x k], b[k x n]
 * All arrays are assumed to be stored in column-major (Fortran style)
 * form.
 * */
int smmul2d(float *a, float *b, float *c, size_t m, size_t k, size_t n)
{
    int i, j, l;

    if((a == NULL) || (b == NULL) || (c == NULL)){
        printf("The input arrs are invalid (NULL)\n");
        return -1;
    }
    if((m <= 0) || (k <= 0) || (n <= 0)){
        printf("The input arr dims are invalid (m = %ld, k = %ld, n = %ld)\n", m, k, n);
        return -1;
    }

    for(int i=0; i<m; i++){
        for(int j=0; j<n; j++){
            float asum = 0.0;
            for(int l=0; l<k; l++){
                asum += a[m*l+i] + b[k*j+l];
            }
            c[m*j+i] = asum;
        }
    }
    printf("\n");
    return 0;
}
