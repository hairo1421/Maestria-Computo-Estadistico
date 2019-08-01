/*Tarea 3
*Version 1
*Ejercicio 6
*Programa que calcula la suma de los dígitos en un entero dado, deberá sumar todos los
dígitos del entero dado e imprimir el numero dado y el resultado.
*/
#include<stdio.h>
#include<math.h>
void sum();


int main (){

	sum();

}

void sum(){
	int n,i,suma;
	    suma=0;
		printf("digite un entero positivo :");
		scanf("%i", &n);
		
		while (n!=0){
			suma+=n%10;
			n=n/10;
			
		}
		printf("La suma de los digitos es: %i", suma);
	}

