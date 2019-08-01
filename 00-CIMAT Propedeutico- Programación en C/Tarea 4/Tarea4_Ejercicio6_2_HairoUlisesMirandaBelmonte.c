/*TAREA #3
**EJERCICIO #4.2
**VERSION 1.0
**HAIRO ULISES MIRANDA BELMONTE
**determinante de matriz 3x3*/

#include<stdio.h>
#include<math.h>
# define n 3
void introduce(int m1[][n]);
void determinante(int m1[][n]);
void print(int m1[][n]);

int main(){
	int m1[n][n];
	//se realiza en funcon para no saturar en operaciones
	introduce(m1);
	printf("\n");
	printf("El valor de la matriz es:\n");
	print(m1);
	printf("\n");
	printf("El valor del determinante de la matriz es:\n");
	determinante(m1);

	//determinante(m1);

}

void introduce(int m1[][n]){

	//introduciendo elementos a la matriz 3x3
	int i,j;
		
		for(i=0;i<3;i++){
			for(j=0;j<3;j++){
				printf("Introduce los elementos [%i] [%i]:", i+1,j+1);
				scanf("%i", &m1[i][j]);
			}
			
		}	
}



void determinante(int m1[][n]){
	//calculando determinante. Clculo es comprobado a mano y con scientific wirk place
	int a,b,c,aux1,aux2,aux3,det;
	
		//fila uno de l matriz
		a=(m1[0][0]);
		b=(m1[0][1]);
		b=((-1)*b);
		c=(m1[0][2]);
	
		//matriz de cofactores por primer elemento de la fila
		aux1=(m1[1][1]*m1[2][2])-(m1[1][2]*m1[2][1]);
		aux1=a*aux1;
		//matriz de cofactores por segun elemento de la fila
		aux2=(m1[1][0]*m1[2][2])-(m1[1][2]*m1[2][0]);
		aux2=b*aux2;
		//matriz de cofactores por tercer elemento de la fila
		aux3=(m1[1][0]*m1[2][1])-(m1[1][1]*m1[2][0]);
		aux3=c*aux3;
		//valor del determinante
		det=aux1+aux2+aux3;
	
		printf("%i", det);
			
	
		
}



void print(int m1[][n]){
	
		//imprimiendo matiz
	int w,k;

	
			for(w=0;w<3;w++){
			for(k=0;k<3;k++){
				
				printf("%i ", m1[w][k]);
			}
			printf("\n");
		}
		
}
