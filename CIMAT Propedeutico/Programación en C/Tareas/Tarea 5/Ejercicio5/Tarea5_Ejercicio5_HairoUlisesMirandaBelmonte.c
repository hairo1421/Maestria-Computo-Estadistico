/*TAREA 5
EJERCICIO 5
DIAGONAL DE UNA MATRIZ
HAIRO ULISES MIRANDA BLEMONTE
*/
#include <stdio.h>

#include <string.h>
void matrix();
 int main(){
 	//funcion que realiza multiplicacion matricial
 matrix();
 
 
	 
	 
 }
 
 void matrix(){
 		//primera matriz
 	int n,m,i,j,h,k;
 	printf("introduce numero de filas de la matriz:");//introducir por teclado elementos de la primera matriz
	scanf("%i", &n);
	printf("introduce numero de columnas de la matriz:");
	scanf("%i", &m);
	int ma[n][m];
	printf("matriz con dimensiones %ix%i\n",n,m);//manda dimensión de la matriz generada
	
 	for(i=0;i<n;i++){
 		for(j=0;j<m;j++){
		 
 			printf("introduce los elementos [%i][%i]:", i+1,j+1);
 			scanf("%i",&ma[i][j]);//introduciendo elementos de la primera matriz por teclado
 		}	
	 }
	 //segunda matriz
	 	int n1,m1,i1,j1,h1,k1;
 	printf("introduce numero de filas de la matriz 2:");//introducir por teclado elementos de la segunda matriz
	scanf("%i", &n1);
	printf("introduce numero de columnas de la matriz 2:");//introducir por teclado elementos de la segunda matriz
	scanf("%i", &m1);
	int m2[n1][m1];
	printf("matriz 2 con dimensiones %ix%i\n",n1,m1);//manda dimensión de la matriz generada
	
 	for(i1=0;i1<n1;i1++){
 		for(j1=0;j1<m1;j1++){
		 
 			printf("introduce los elementos de la matriz 2 [%i][%i]:", i1+1,j1+1);//introduce elementos matriz dos
 			scanf("%i",&m2[i1][j1]);
 		}	
	 }
	 
	int w,r,t,sum;
	int mm[n][m1];
	
	if(m==n1){//hace la multiplicación matricial si las el numero de columna de matriz 1 es igual al nuemero de fila
				//de la matriz  numero 2
		for(w=0;w<m+1;w++){
			for(r=0;r<m1;r++){
				sum=0;
				for(t=0;t<n1;t++){
					
				sum+=ma[w][t]*m2[t][r];
				mm[w][r]=sum;	
				
				}
			}
		}
		
	} else{
		printf("no se puede multiplicar");//en caso de que no coincida el numero de columna de la primera matriz con el
												//el numero de filas de la segunda,manda un mensaje y no calcula
	}

	//imprimiendo matiz
	int o,p;

		for(o=0;o<n;o++){
			for(p=0;p<m1;p++){
				
				printf("%i ", mm[o][p]);
			}
			printf("\n");
		}
		
 }
 

 
 
 

