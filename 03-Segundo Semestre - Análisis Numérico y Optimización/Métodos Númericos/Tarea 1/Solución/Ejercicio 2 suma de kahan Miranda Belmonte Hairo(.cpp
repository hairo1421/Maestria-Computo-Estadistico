/*Métodos Númericos
**TAREA 1: suma de kahan
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**20 de Febrero del 2019*/

#include <stdio.h>
#include <stdlib.h>

//PSEUDOCÓDIGO

/*function KahanSum(input)
    var sum = 0.0
    var c = 0.0                 // la variable que compensa los errores 
    for i = 1 to input.length do
        var y = input[i] - c    // c es cero, en la primera iteración.
        var t = sum + y         // la suma puede ser alta o pequeña pero aún así presenta perdidas en decimales
        c = (t - sum) - y       // (t - sum) aquí t se redondero, entonces recobra la parte negativa de y, c ya tiene valor
        sum = t                 //  suma con los decimales redondeado
    next i                      //continua el loop´
    return sum

// ¿QUÉ ES?
/* El algoritmo de la suma de kahan realiza una suma compensatoria reduciendo el error númerico obtenido a la
 hora de realizar una secuencia de sumas de variables del tipo float. Esto se realiza separando una variable
 # que acomule la compensación.
*/ 
float epsilon() {
    float eps = 1.0f;
    while (1.0f + eps != 1.0f) eps /= 2.0f;
    return eps;
} // end epsilon
 
float kahanSum(float *nums, int count) {
    float sum = 0.0f;
    float c = 0.0f;
    float t, y;
    int i;
    for (i = 0; i < count; ++i) {
        y = nums[i] - c;
        t = sum + y;
        c = (t - sum) - y;
        sum = t;
    }
    return sum;
}  // end suma kahan
 
int main() {
    float a = 1.0f;
    float b = epsilon();
    float c = -b;
    float fa[3];
 
    fa[0] = a;
    fa[1] = b;
    fa[2] = c;
 
    printf("Epsilon     = %0.12f\n", b);
    printf("(a + b) + c = %0.12f\n", (a + b) + c);
    printf("Kahan suma  = %0.12f\n", kahanSum(fa, 3));
 
    return 0;
} // end main


