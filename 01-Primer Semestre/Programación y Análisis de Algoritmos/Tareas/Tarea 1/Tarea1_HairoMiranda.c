/*Programación y Análisis de Algoritmos
**TAREA 1: LANZAR UN DADO 6000000 VECES
**HAIRO ULISES MIRANDA BELMONTE
**VERSION 1.0
**19 DE AGOSTO DEL 2018*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

//generando global 
# define n 6000000

// funcion main empieza la ejecucion del programa
int main( void )
{
	//declarando variables
	 int i; int j;int seq=0;
	 int contar1=1;int contar2=1;int contar3=1;int contar4=1;int contar5=1;int contar6=1;
	 int max1=1;int max2=1;int max3=1;int max4=1;int max5=1;int max6=1;
	 
	//asignando espacio de memoria al array
	int * table=( int *) calloc ( n , sizeof ( int *) );
	int * count=( int *) calloc ( 6 , sizeof ( int *) );
   // la semilla aleatoria
   	srand(time(0));
	

  for (i=0;i < n; i++ ) {// ciclo n veces
    // Toma un numero aleatorio del 1 al  y lo imprime
    table[i]=1 + ( rand() % 6 );
    printf("%10d",table [i]);
    if ( i % 5 == 4 ) { //para tener columnas de 5 
       puts( "" );
   	} // end if;
  } // end for	
 
 	printf("\n");
 	 //generando array con frecuencias
	for (i=0;i< n;i++){
		 switch (table[i]){
  			case 1:count[0]+=1;break;
  			case 2:count[1]+=1;break;
			case 3:count[2]+=1;break;
			case 4:count[3]+=1;break;
			case 5:count[4]+=1;break;
			case 6:count[5]+=1;break;
			}// end switch
	}//end for
 
 //  b) contando el numero de veces que se presenta la secuencia "1,2,3,4,5,6" 
  	for(i=0;i<n;i++){
      	if(table[i]==1&&table[i+1]==2&&table[i+2]==3&&table[i+3]==4&&table[i+4]==5&&table[i+5]==6){
      	seq++;
      	}//end if
	}//end for
  	
//	a)	contando el tamaño de la secuencia mas grande para el propio numero
	for(i=0;i<n;i++){
		if(table[i]==1&&table[i]==table[i+1]){
			contar1++;
			max1=contar1>max1 ? contar1:max1;
			} else{
			contar1=1;
		}//end if
	}//end for
 	
 	for(i=0;i<n;i++){
		if(table[i]==2&&table[i]==table[i+1]){
			contar2++;
			max2=contar2>max2 ? contar2:max2;
			}else{
			contar2=1;
		}//end if
	}//end for
 	
 	for(i=1;i<n;i++){
		if(table[i]==3&&table[i]==table[i+1]){
			contar3++;
			max3=contar3>max3 ? contar3:max3;
			}else{
			contar3=1;
		}//end if
	}//end for
	
	for(i=1;i<n;i++){
		if(table[i]==4&&table[i]==table[i+1]){
			contar4++;
			max4=contar4>max4 ? contar4:max4;
			}else{
			contar4=1;
		}//end if
	}//end for
		
	for(i=1;i<n;i++){
		if(table[i]==5&&table[i]==table[i+1]){
			contar5++;
			max5=contar5>max5 ? contar5:max5;
			}else{
			contar5=1;
		}//end if
	}//end for	
	
	for(i=1;i<n;i++){
		if(table[i]==6&&table[i]==table[i+1]){
			contar6++;
			max6=contar6>max6 ? contar6:max6;
			}else{
			contar6=1;
		}//end if
	}//end for
 			
 			//print a la tabla de frecuencias con la longitud de la secuencia mas grande del propio numero
            printf("Lanzamiento       Frecuencia  		Longitud Secuencia	\n");
            printf("uno                 %i  	  				%i			\n",count[0],max1);
            printf("dos                 %i  					%i			\n",count[1],max2);
            printf("tres                %i 						%i			\n",count[2],max3);
 			printf("cuatro              %i  					%i			\n",count[3],max4);
            printf("cinco               %i  					%i			\n",count[4],max5);
            printf("seis                %i  					%i			\n",count[5],max6);
            printf("\n");
     		printf("Numero de veces que la secuencia '1,2,3,4,5,6' aparece es de:%i veces",  seq);
      	
       
		return 0;
	 
}// end main
