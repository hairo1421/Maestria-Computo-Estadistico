/*TAREA 5
EJERCICIO 1
DIAGONAL DE UNA MATRIZ
HAIRO ULISES MIRANDA BLEMONTE
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
void diagonal();
 int main(){
 	
 diagonal();//genera matriz diagonal con elementos tecleados por usuarios
 
	 
	 
 }
 
 void diagonal(){//función
 		
 	int n,m,i,j,h,k,j1,i1;
 	printf("introduce numero de filas de la matriz:");
	scanf("%i", &n);
	printf("introduce numero de columnas de la matriz:");
	scanf("%i", &m);
	int ma[n][m];
	
	
	printf("matriz con dimensiones %ix%i\n",n,m);//genero la matriz con elementos ceros
	
		for(i1=0;i1<n;i1++){
 		for(j1=0;j1<m;j1++){
		 
 		ma[i1][j1]=0;
 		}	
	 }
	
	
 	for(i=0;i<n;i++){//de la matriz de elementos ceros les cambio la diagonal con lo que introduce el usuario
		 
 			printf("introduce los elementos [%i][%i]:", i+1,i+1);
 			scanf("%i",&ma[i][i]);
 		
	 }


	//imprimiendo matiz
	int o,p;

		for(o=0;o<n;o++){
			for(p=0;p<m;p++){
				
				printf("%i ", ma[o][p]);
			}
			printf("\n");
		}
		
 }
 

 
 
 

