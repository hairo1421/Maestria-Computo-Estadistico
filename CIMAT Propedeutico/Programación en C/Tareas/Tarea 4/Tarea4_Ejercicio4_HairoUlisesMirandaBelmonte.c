/*TAREA #3
**EJERCICIO #4
**VERSION 1.0
**HAIRO ULISES MIRANDA BELMONTE
**Desarrolla un programa que verifique si una cadena dada por el usuario desde teclado
(de m ´aximo 500 caracteres) es un palindromo. El resultado deber ´a imprimirse en
pantalla.*/

#include<stdio.h>
#include<string.h>
#define n 500

int main(){
	//el programa se limita a palabras y no frases
	int i,aux,x,j,h;
	char palabra[500];
	printf("redacta tu palabra o mas:");
	scanf( "%[^\n]%*c",palabra);
	printf("La palabra es: %s\n", palabra);
		//longitud de la cadena
		x=strlen(palabra);
	
	
		//evaluando si es palindromo
		for(i=0;i<x;i++){
			if(palabra[i]==palabra[x-i-1]){
				aux=1;
			}else {
				aux=0;
			}
		}
	 		//condicion de que sea o no un palindromo
	 		if (aux==1){
	 			printf("La palabra es: Palindromo");
	 			}else{
	 			printf("La palabra es: No palindromo");
				 }
}
