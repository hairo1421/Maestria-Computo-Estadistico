/*TAREA 6
EJERCICIO 2
DIAGONAL DE UNA MATRIZ memoria dinamica
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
 		
 	int i,j,renglones,columnas;
 	printf("introduce numero de renglones:");
 	scanf("%ii", &renglones);
 	printf("introduce numero de columnas:");
 	scanf("%ii", &columnas);
 	
 	// Declaracion y asignacion de memoria
		double ** m =( double **) calloc ( renglones , sizeof ( double *) );
	for ( i=0;i< renglones ;i++)
	m [i ]=( double *) calloc ( columnas , sizeof ( double ));

	
	
 	for(i=0;i<renglones;i++){//de la matriz de elementos ceros les cambio la diagonal con lo que introduce el usuario
		 
 			printf("introduce los elementos [%i][%i]:", i+1,i+1);
 			scanf("%i",&m[i][i]);
 		
	 }


	//imprimiendo matiz

	if(renglones==columnas){
	
		for(i=0;i<renglones;i++){
			for(j=0;j<columnas;j++){
				
				printf("%i ", m[i][j]);
			}
			printf("\n");
		}
	}else{
		printf("no es cuadrada");
	}
		// L i b e r a c i n de memoria asignada
for ( i=0;i< renglones ;i++)
free ( m[i]) ;
free ( m );

		
 }
 

 
 
 

