/*Programación y Análisis de Algoritmos
**TAREA 2: JUEGO CRAPS
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**28 DE AGOSTO DEL 2018*/

#include<stdio.h>
#include <stdlib.h>
#include <time.h>


void simul(int *N, int *w, int *l, int *count);
void craps(int *N, int *w, int *l, int *count);
int ganar(int *N, int *w);
int posib(int *N, int *w, double *pr);

 int main(){
int i,a;
int N=1000;


//reservando espacio de memora
int *w=(int*)calloc(20, sizeof(int*));
int *l=(int*)calloc(20, sizeof(int*));
int *count=(int*)calloc(1, sizeof(int*));
double *pr=(double*)calloc(20, sizeof(double*));
 //simulando los juegos
 simul(&N,w,l,count);
 	
 	printf("\nTABLA DE RESULTADOS (Tiros y juegos ganados)\n");
 		for(i=0;i<20;i++){
			printf("Las veces que se ganaron en el lanzamiento %d fueron: %d\n",i+1, w[i]);
		}//end for
	printf("lanzamientos mayor a 20 para ganar fueron: %d\n", w[20]);
	
	printf("\nTABLA DE RESULTADOS (Tiros y juegos perdidos)\n");
		for(i=0;i<20;i++){
			printf("Las veces que se perdieron en el lanzamiento %d fueron: %d\n",i+1, l[i]);
		}//end for	
	printf("lanzamientos mayor a 20 para perder fueron: %d\n", l[20]);
	printf("\nLa posibilidad de ganar el juego en general es de es de:");
	a=ganar(&N,w);
	printf("%d porciento\n", a);
	//posibilidad de ganar en juegos más largos 
	printf("\nPosibilidad de ganar con juegos mas largos\n");
	posib(&N,w,pr);
	//numero promedio de tiros en un juego
	printf("\nEn promedio se duran %d tiros en un juego de crap\n", count[0]/N	);
	
	
	
	//Liberando espacio de memoria
	free(count);
	free(w);
	free(l);
	free(pr);
//	printf("Los resultados se encuentran en el archivo de texto\n");
	
return 0;	
   	
}//end main

int ganar(int *N, int *w){//calcula la posibilidad de ganar
	int i;
	double mean=0;
		for(i=0;i<20;i++){
			mean+=w[i];
		}//end for
		mean=(mean/ *N)*100;
		
	return mean;
}//end ganar

int posib(int *N, int *w, double *pr){//calcula la posibilidad de ganar
	int i;
		//posibilidad de ganar en juegos más largos
	for(i=0;i<21;i++){
			pr[i]=w[i];
			pr[i]=pr[i]/ *N;
		}//end for
		for(i=0;i<20;i++){
			printf("\nLa posibilidad de ganar en  %d juego es de: %f", i+1, pr[i]);
		}
		
			printf("\nlanzamientos mayor a 20 para ganar fueron: %f\n", pr[20]);

}//end ganar

void simul(int *N, int *w, int *l, int *count){//simulación del crap
 int i;
srand(time(NULL));
	for(i=0;i<*N;i++){
		printf("**Juego %d\n**", i+1);
		craps(N, w,l, count);
	 	printf("\n");  
	}//end for	
}//end simul	   
 
 
void craps(int *N,int *w, int *l, int *count){//Juego crap
	
int i,j, lanz1 ,lanz2, lanz, meta;

 	count[0]++;//contador
    //simulación y suma del lanzamiento de dos dados
    lanz1=1 + ( rand() % 6 );
    lanz2=1 + ( rand() % 6 );
    lanz=lanz1+lanz2;
   		if(lanz==7||lanz==11){
   			printf("Ya ganaste\n");
   			w[0]+=1;
   	    }else if(lanz==2||lanz==3||lanz==12){
	   		printf("Ya perdiste\n");
	   		l[0]+=1;
	    }else{
	   		meta=lanz;
	   		printf("Tu lanzamiento es de %d, por lo tanto tu nueva meta es de:%i\n",meta, meta);
	   			do{
					lanz1=1 + ( rand() % 6 );
    				lanz2=1 + ( rand() % 6 );
   					lanz=lanz1+lanz2;
   					i++;
    				count[0]++;
    				printf("En tu lanzamiento %d te salio el numero %i\n",i, lanz);
				}while(meta!=lanz&&lanz!=7);
	    }//end if anidado
	
	//realizando conteos para pregunta 2.1   
	for(j=1;j<20;j++){
   		if(i==j&&lanz==meta){
   			w[j]++;
	   }//end if
   }//end for 
	if(i>=20&&lanz==meta){
		w[20]++;
	}//end if
  	
  	//realizando conteos para pregunta 2.2 
  	for(j=1;j<20;j++){
  	if(i==j&&lanz==7){
   			l[j]++;
	   }//end if
   }//end for
   	if(i>=20&&lanz==7){
		l[20]++;
	}//end if

	

 }//end craps


