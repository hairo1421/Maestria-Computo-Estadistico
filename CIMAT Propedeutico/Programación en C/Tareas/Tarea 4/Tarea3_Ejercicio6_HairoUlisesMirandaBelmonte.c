/*TAREA #3
**EJERCICIO #4
**VERSION 1.0
**HAIRO ULISES MIRANDA BELMONTE
**determinante de matriz 2x2*/

#include<stdio.h>
#include<math.h>
# define n 2
void introduce(int m1[][n]);
void determinante(int m1[][n]);
void print(int m1[][n]);

int main(){
	int m1[n][n];
	//todo se realiza por fuención para no meter muchas operaciones en int main
		//entra el valor de los elementos
	introduce(m1);
	printf("\n");
		//muestra en pantalla la matriz
	printf("La matriz es:\n");
	print(m1);
	printf("\n");
		//imprime determinante de la matriz
	printf("El valor del determinante de la matriz es:\n");
	determinante(m1);

	//determinante(m1);

}

void introduce(int m1[][n]){

	//introducuendi elementos a la matriz
	int i,j;
		
		for(i=0;i<2;i++){
			for(j=0;j<2;j++){
				printf("Introduce los elementos [%i] [%i]:", i+1,j+1);
				scanf("%i", &m1[i][j]);
			}
			
		}	
}



void determinante(int m1[][n]){
	//calculando determinanre
	int a,b,c,d,det;
		a=m1[0][0];
		b=m1[0][1];
		c=m1[1][0];
		d=m1[1][1];
		//formula de calcular determinante
		det=(a*d)-(b*c);
		printf("%i", det);
			
	
		
}






void print(int m1[][n]){
	
		//Matriz 2x2
	int w,k;

	
			for(w=0;w<2;w++){
			for(k=0;k<2;k++){
				
				printf("%i ", m1[w][k]);
			}
			printf("\n");
		}
		
}
