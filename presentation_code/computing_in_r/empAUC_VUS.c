#include<stdio.h>
#include<R.h>
#include<Rmath.h>

void empVUS(double *x, double *y, double *z,
	       int *n1,   int *n2,   int *n3,  double *result) {
    int i, j, k;
    double ksum = 0;

	for (i=0; i<*n1; i++) {
	for (j=0; j<*n2; j++) {
	for (k=0; k<*n3; k++) {
	   if ( (x[i]<y[j]) && (y[j]<z[k]) ) ksum++; }}}
   *result = ksum/(*n1 * *n2 * *n3);
}