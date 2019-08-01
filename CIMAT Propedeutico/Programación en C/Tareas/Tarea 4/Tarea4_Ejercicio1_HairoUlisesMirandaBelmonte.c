/*TAREA #3
**EJERCICIO #1
**VERSION 1.0
**HAIRO ULISES MIRANDA BELMONTE
**Realiza un programa que lea dos cadenas de caracteres de m aximo 25 caracteres
cada una, sin considerar el caracter nulo. Implementa las operaciones necesarias para
imprimir los siguientes resultados:*/

#include<stdio.h>
#include<string.h>

int main (){
	int x,y,i,j, bolean,k, bolean2;
	bolean=0;//boleano=->FALSO
	//Definiendo cadenas de caracteres
	char cadena1[25];
	char cadena2[25];
	char cadena3[50];
		
		printf("introduce cadena1:");
		scanf( "%[^\n]%*c",cadena1);
	
		printf("introduce cadena2:");
		scanf( "%[^\n]%*c",cadena2);
		//Guardando valores de caracteres
		x=strlen(cadena1);
		y=strlen(cadena2);
			//a) Longitud de la primera cadena.Imprimiendo la longitud de la primera cadena
			printf("La longitud de la primera casena es:%i\n", x);
			//b) Longitud de la seguda cadena.Imprimiendo la longitud de la segunda cadenae
			printf("La longitud de la segunda cadena es:%i\n", y);
				//Match carácteres en un tercer vector con longitud maxima de 50
				for(i=0;i<x;i++){
					cadena3[i]=cadena1[i];
				}
				
				//c) Concatenacion de la primera con la segunda cadena. (Guardada en una cadena
					// permite hasta 50 caracteres, sin considerar el caracter nulo.)
						//Match segunda cadena carácteres en un tercer vector con longitud maxima de 50
				for(j=0;j<(x+y);j++){
					cadena3[j+x]=cadena2[j];
				}
					
				printf("La cadeda concatenada dice:%s\n", cadena3);
					//d) Booleano que indique si la primer cadena es subcadena de la segunda cadena.
					 if(x<y){
						for(k=0;k<x+y;k++){
							if(cadena1[k]==cadena2[k]){
								bolean=1;
								}else{bolean=0;}
							}
						}	
							printf("La primera cadena de caracteres es subcadena de la segunda:\n");
							switch(bolean){
								case 0:printf("FALSO\n");break;
								case 1:printf("VERDADERO\n");break;
							}
								
					
					//e) Booleano que indique si la segunda cadena es subcadena de la primer cadena.
					if(x>y){
						for(k=0;k<x+y;k++){
							if(cadena1[k]==cadena2[k]){//cortar el codigo
								bolean2=1;
								}else{bolean2=0;}
		
							}
						}
							printf("La segunda cadena de caracteres es subcadena de la primera:\n");
							switch(bolean2){
								case 0:printf("FALSO\n");break;
								case 1:printf("VERDADERO\n");break;
							}
}
