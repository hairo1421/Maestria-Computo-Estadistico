/*TAREA 5
EJERCICIO 2
GENERA DOS MATRICES I LAS ESCRIBE EN UN TXT. SUMA ESAS MATRICES Y ESCRIBE LA SUMA DE ESAS MATRICES EN OTRO TXT
HAIRO ULISES MIRANDA BLEMONTE
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
void matrix();
 int main(){
 	
 	
 matrix();//funcion que genera todo el ejercicio
 
	 
	 
 }
 
 void matrix(){
 	
 	
 	
 		
 	int n,m,i,j,h,k;
 	FILE * matrices;//abrir un archivo file y escribir en el
 	matrices=fopen("declaracion maticial.txt","w");
 	if ( matrices == NULL ){
	printf (" Error al abrir el archivo .");


	}
	//introducir dimensiones de la primera matriz
 	printf("introduce numero de filas de la matriz:");
	scanf("%i", &n);
	printf("introduce numero de columnas de la matriz:");
	scanf("%i", &m);
	int ma[n][m];
	fprintf(matrices,"matriz con dimensiones %ix%i\n",n,m);
	
 	for(i=0;i<n;i++){//introducir por teclado los elementros de la primera matriz
 		for(j=0;j<m;j++){
		 
 			printf("introduce los elementos [%i][%i]:", i+1,j+1);
 			scanf("%i",&ma[i][j]);
 		}	
	 }
	 
	 	int q,e;//imprime en el txt la primera matriz
		 for(q=0;q<n;q++){
			for(e=0;e<m	;e++){
				
				fprintf(matrices,"%i ", ma[q][e]);
			}
			fprintf(matrices,"\n");
		}
		
		
		
	 //segunda matriz
	 	int n1,m1,i1,j1,h1,k1;
 	printf("introduce numero de filas de la matriz 2:");//declarando dimensiones de la segunda matriz
	scanf("%i", &n1);
	printf("introduce numero de columnas de la matriz 2:");
	scanf("%i", &m1);
	int m2[n1][m1];
	fprintf(matrices,"matriz 2 con dimensiones %ix%i\n",n1,m1);
	
 	for(i1=0;i1<n1;i1++){//introduciendo por teclaso elementos de la segunda matriz
 		for(j1=0;j1<m1;j1++){
		 
 			printf("introduce los elementos de la matriz 2 [%i][%i]:", i1+1,j1+1);
 			scanf("%i",&m2[i1][j1]);
 		}	
	 }
	 
	 int q1,e1;
		 for(q1=0;q1<n;q1++){
			for(e1=0;e1<m;e1++){
				
				fprintf(matrices,"%i ", m2[q1][e1]);//escribiendo segunda matriz en el txt
			}
			fprintf(matrices,"\n");
		}
		
		fclose ( matrices );//cierra file
		
		
			FILE * matrices2;//abre nuevo file que contenga suma matrices
 	matrices2=fopen("resultado de matrices.txt","w");
 	if ( matrices2 == NULL ){
	printf (" Error al abrir el archivo .");


	}
	int w,r,t,sum;
	int mm[n][m];//nueva matriz

	if(n==n1&&m==m1){//sumando matriz sólo si sus dimensiones son iguales
		for(w=0;w<n;w++){
			for(r=0;r<m;r++){
				sum=0;
				
				mm[w][r]=ma[w][r]+m2[w][r]	;
				
			}
		}
		
	} else{
		printf("no se puede sumar\n");
	
	}

	//imprimiendo matiz

	if(n==n1&&m==m1){//imprime la matriz suma en el txt
	int o,p;

		for(o=0;o<n;o++){
			for(p=0;p<m1;p++){
				
				fprintf(matrices,"%i ", mm[o][p]);
			}
			fprintf(matrices2,"\n");
		}
	
}else{
	printf("intenta de nuevo");
	
}
fclose ( matrices2 );//cierra file
		
 }
 

 
 
 

