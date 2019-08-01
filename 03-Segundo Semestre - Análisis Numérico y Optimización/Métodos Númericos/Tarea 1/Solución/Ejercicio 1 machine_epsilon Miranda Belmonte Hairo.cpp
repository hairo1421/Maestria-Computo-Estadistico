/*Métodos Númericos
**TAREA 1: Machibe epsilon
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**20 de Febrero del 2019*/


#include <stdio.h>
typedef union {
  long long i64;
  double d64;
} dbl_64;

double machine_eps (double value)
{
    dbl_64 s;
    s.d64 = value;
    s.i64++;
    return s.d64 - value;
} // end machine_eps

int main(){
  	double value = 1;
	double	x = machine_eps (value);
	printf("Epsilon: %.50Lf\n", x);

} // end main

/* El resultado del epsilon es: 0.00000000000000022204460492503131000000000000000000 */
