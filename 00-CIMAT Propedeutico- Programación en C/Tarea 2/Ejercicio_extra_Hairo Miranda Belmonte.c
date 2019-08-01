/*Tarea 2
*Version 1
*Ejercicio extra en clase
*Programa que ingrese diferentes tipos de variables
modificados.
*/

#include<stdio.h>

int main (){


char letra;
char nombre[10];	
int entero;
float  decimal;


	printf("Escribe una letra:");
	scanf("%c", &letra);
	printf("Escribe nombre:");
	scanf("%s", &nombre);
	printf("Da un valor entero:");
	scanf("%d", &entero);
	printf("Da un valor entero con decimal:");
	scanf("%f", &decimal);
	

	printf("%c\n", letra);
	printf("%s\n", nombre);
	printf("%d\n", entero);
	printf("%f\n", decimal);


	
	return 0;
	
}
