/*Programación y Análisis de Algoritmos
**TAREA 2: LANZAR DOS DADOS  36000 VECES
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**28 DE AGOSTO DEL 2018*/

#include<stdio.h>
#include <stdlib.h>
#include <time.h>
# define N 36000
 int main(){
 	int i,j, lanz1 ,lanz2;
 	int count=0;
 	int * lanz=(int*)calloc(N, sizeof(int*));
 	int *freq=(int*)calloc(N, sizeof(int*));
 	double *pr=(double*)calloc(N, sizeof(double*));
 	
 	int num[]={2,3,4,5,6,7,8,9,10,11,12};
	srand(time(NULL));
	
 	  for (i=0;i < N; i++ ) {// ciclo n veces
    // Toma un numero aleatorio del 1 al  y lo imprime
    lanz1=1 + ( rand() % 6 );
    lanz2=1 + ( rand() % 6 );
    lanz[i]=lanz1+lanz2;
    if (lanz[i]==7){
   		count++;
	   }
   	//printf("%d \n", lanz[i]);
   	switch (lanz[i]){
   		case 2:freq[0]+=1;break;
   		case 3:freq[1]+=1;break;
   		case 4:freq[2]+=1;break;
   		case 5:freq[3]+=1;break;
   		case 6:freq[4]+=1;break;
   		case 7:freq[5]+=1;break;
   		case 8:freq[6]+=1;break;
   		case 9:freq[7]+=1;break;
   		case 10:freq[8]+=1;break;
   		case 11:freq[9]+=1;break;
   		case 12:freq[10]+=1;break;
	   }
	 
  }
  	for(i=0;i<=10;i++){
  		pr[i]=freq[i];
  		pr[i]/=N;
	  }
	  
  	printf("La frecuencia de 7 es de %i.\n corresponde aproximadamente a un 1/6 de los lanzamientos\n", count); 
   		printf("Numeros   Frecuencia       Probabilidad\n", num[i], freq[i]);
   	for(i=0;i<11;i++){
   		printf("  %d        %d                  %f \n"   , num[i], freq[i], pr[i]);
	   }
	   
	   free(lanz);
	   free(freq);
 }
 

