/*Tarea 3
*Version 1
*Ejercicio 9
*Encontrar la suma de los primeros 50 Números naturales usando ciclo For.
*/
#include<stdio.h>
void sum();
int main(){
	sum();
}

void sum(){
	int i,suma;
	suma=0;
	for(i=0;i<=50;i++){
		suma+=i;
	}
	printf("la suma de los primeros 50 numeros naturales es: %i",suma);
	
}
