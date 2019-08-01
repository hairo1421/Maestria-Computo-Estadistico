/*Tarea 3
*Version 1
*Ejercicio 11
*Que imprima todo el código ASCII y su equivalencia en carácter (el código ASCII va de 0 a
255 y representa el número con el que los ordenadores almacenan los caracteres).
*/
#include<stdio.h>
void ASCII();
int main(){
	ASCII();
}

void ASCII(){
	int i;
	for(i=0;i<=255;i++){
		printf("%d-%c\t",i,i);//se ordena por tabulado
	}
}
