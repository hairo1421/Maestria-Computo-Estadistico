/*Tarea 2
*Version 1
*Ejercicio 1
*Programa que ingrese dos enteros y cambiar sus valores. Imprima valores originales y los
modificados.
*/

#include<stdio.h>

int main (){
	int x,y, aux;
	aux=0;
		printf("Da un valor entero para x:");
		scanf("%i", &x);
		printf("Da un valor entero para y:");
		scanf("%i", &y);
	aux=x;
	x=y;
	y=aux;
		printf("El valor de x es: %i\n", x);
		printf("El valor de y es: %i\n", y);
	return 0;
	
}
